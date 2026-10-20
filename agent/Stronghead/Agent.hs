{-# LANGUAGE CPP #-}

-- | Drop-in live telemetry for a Haskell program, read by the Stronghead
-- GUI's Live panel.
--
-- > import Stronghead.Agent
-- >
-- > main :: IO ()
-- > main = withAgent defaultAgentConfig $ do
-- >   ...
--
-- Link with @-threaded@ and run with @+RTS -T@ for RTS statistics (thread
-- lists work without it). The agent listens on 127.0.0.1:4777 by default and
-- streams a frame every 250 ms to each connected viewer. If the program
-- also writes an eventlog (@+RTS -l@), the agent flushes it on the same
-- cadence so Stronghead can follow the file live.
--
-- Protocol v2 (the wire format is described in "Stronghead.Live") adds an
-- info line, so one Connect can find the program's eventlog, heap profile
-- and sources, each thread's capability, and a small fixed set of commands
-- a viewer may send: post a marker, run a major GC, request a heap census,
-- turn heap profiling on or off, flush the eventlog. Commands perturb the
-- program but cannot run code. They are accepted by default only when the
-- agent listens on a loopback address ('agentAcceptCommands').
module Stronghead.Agent
  ( AgentConfig (..)
  , defaultAgentConfig
  , withAgent
  , startAgent
  ) where

import Control.Concurrent
import Control.Exception (SomeException, bracket, catch, finally, mask_, try)
import Control.Monad (forM, forever, unless)
import Data.ByteString qualified as B
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, isAlphaNum, isAscii, isDigit)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Debug.Trace (flushEventLog, traceMarkerIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import GHC.Conc (labelThread)
import GHC.Conc.Sync (ThreadStatus (..), fromThreadId, listThreads, threadLabel, threadStatus)
import GHC.Environment (getFullArgs)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (utf8)
import GHC.Profiling (requestHeapCensus, startHeapProfTimer, stopHeapProfTimer)
import GHC.RTS.Flags (DoHeapProfile (..), DoTrace (..), ProfFlags (..), TraceFlags (..), getProfFlags, getTraceFlags)
import GHC.Stats
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getExecutablePath, getProgName)
import System.Mem (performMajorGC)
import System.Posix.Internals (c_getpid)
#if defined(mingw32_HOST_OS)
import Foreign.C.String (CWString, peekCWString)
import Foreign.C.Types (CInt (..))
#else
import Foreign.C.String (CString)
import Foreign.C.Types (CSize (..))
import GHC.IO.Encoding (getFileSystemEncoding)
#endif

data AgentConfig = AgentConfig
  { agentHost :: !String
  , agentPort :: !Int
  , agentIntervalMs :: !Int
  , agentFlushEventlog :: !Bool
  , agentAcceptCommands :: !(Maybe Bool)
    -- ^ Whether viewers may send commands (marks, a major GC, a census,
    -- heap profiling on and off, an eventlog flush). 'Nothing', the
    -- default: only when the agent is bound to a loopback address, so a
    -- program listening on the network can be watched but not poked.
  }

defaultAgentConfig :: AgentConfig
defaultAgentConfig = AgentConfig "127.0.0.1" 4777 250 True Nothing

-- | Run an action with the agent serving in the background.
--
-- The server thread spends its life in a blocking @accept@, which on
-- Windows is an uninterruptible foreign call: a synchronous 'killThread'
-- on it would block forever. The kills are therefore sent asynchronously;
-- the threads go away when the program exits if not before.
withAgent :: AgentConfig -> IO a -> IO a
withAgent cfg act = bracket (startAgent cfg) (mapM_ (forkIO . killThread)) (const act)

-- | Start the server (and the eventlog flusher); returns their threads.
startAgent :: AgentConfig -> IO [ThreadId]
startAgent cfg = do
  server <- forkIO (serve cfg `catch` \(_ :: SomeException) -> pure ())
  labelThread server "stronghead-agent"
  flusher <-
    if agentFlushEventlog cfg
      then do
        t <- forkIO . forever $ do
          threadDelay (agentIntervalMs cfg * 1000)
          flushEventLog
        labelThread t "stronghead-eventlog-flush"
        pure [t]
      else pure []
  pure (server : flusher)

serve :: AgentConfig -> IO ()
serve cfg = do
  addr : _ <-
    getAddrInfo
      (Just defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream})
      (Just (agentHost cfg))
      (Just (show (agentPort cfg)))
  let accepting = fromMaybe (isLoopback (addrAddress addr)) (agentAcceptCommands cfg)
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 4
    forever $ do
      (conn, _) <- accept sock
      mask_ $ do
        t <- forkIO (client cfg accepting conn `catch` (\(_ :: SomeException) -> pure ()) `finally` close conn)
        labelThread t "stronghead-agent-client"

isLoopback :: SockAddr -> Bool
isLoopback = \case
  SockAddrInet _ h -> let (a, _, _, _) = hostAddressToTuple h in a == 127
  SockAddrInet6 _ _ h _ -> hostAddress6ToTuple h == (0, 0, 0, 0, 0, 0, 0, 1)
  SockAddrUnix _ -> True

client :: AgentConfig -> Bool -> Socket -> IO ()
client cfg accepting conn = do
  -- Frames and command replies share the socket; a line is never split.
  lock <- newMVar ()
  let send s = withMVar lock (\_ -> sendAll conn (utf8Bytes s))
  prog <- getProgName
  pid <- c_getpid
  caps <- getNumCapabilities
  send (unwords ["H", filter (/= ' ') prog, show pid, show caps] <> "\n")
  send . (<> "\n") =<< infoLine accepting
  -- The reader ends by itself when the viewer goes away (or the socket is
  -- closed under it); a v1 viewer never writes, so it just waits.
  reader <- forkIO (commands accepting send conn `catch` \(_ :: SomeException) -> pure ())
  labelThread reader "stronghead-agent-commands"
  enabled <- getRTSStatsEnabled
  forever $ do
    statsLine <-
      if enabled
        then renderStats <$> getRTSStats
        else pure "S rts_stats=0"
    threads <- listThreads
    rows <- fmap concat . forM threads $ \t -> do
      st <- threadStatus t
      lbl <- threadLabel t
      (cap, _) <- threadCapability t
      let tid = show (fromThreadId t)
      pure $ case st of
        ThreadFinished -> []
        ThreadDied -> []
        _ -> [unwords ["T", tid, showStatus st, fromMaybe "" lbl], unwords ["C", tid, show cap]]
    caps' <- getNumCapabilities
    send (intercalate "\n" ((statsLine <> " capabilities=" <> show caps') : rows ++ ["E", ""]))
    threadDelay (agentIntervalMs cfg * 1000)

-- | What one Connect needs to find the rest of the program: where it runs,
-- how it was started, and what the RTS is writing.
infoLine :: Bool -> IO String
infoLine accepting = do
  cwd <- either (\(_ :: SomeException) -> "") id <$> try currentDirectory
  exe <- either (\(_ :: SomeException) -> "") id <$> try getExecutablePath
  args <- getFullArgs
  prof <- getProfFlags
  tr <- getTraceFlags
  let eventlog = case tracing tr of
        TraceEventLog -> True
        _ -> False
  pure $
    unwords
      [ "I"
      , "version=2"
      , "cwd=" <> percentEncode cwd
      , "exe=" <> percentEncode exe
      , "args=" <> intercalate "," (map percentEncode args)
      , "heap-profile=" <> heapMode (doHeapProfile prof)
      , "heap-interval-ms=" <> show (heapProfileInterval prof `div` 1000000)
      , "eventlog=" <> flag eventlog
      , "commands=" <> flag accepting
      ]
  where
    flag b = if b then "1" else "0"

-- | The heap profile's breakdown as the RTS flag that asks for it.
heapMode :: DoHeapProfile -> String
heapMode = \case
  NoHeapProfiling -> "none"
  HeapByCCS -> "hc"
  HeapByMod -> "hm"
  HeapByDescr -> "hd"
  HeapByType -> "hy"
  HeapByRetainer -> "hr"
  HeapByLDV -> "hb"
  HeapByClosureType -> "hT"
  HeapByInfoTable -> "hi"
  HeapByEra -> "he"

-- | Read command lines and answer each with an @A@ line.
commands :: Bool -> (String -> IO ()) -> Socket -> IO ()
commands accepting send conn = loop B.empty
  where
    loop buf = do
      chunk <- recv conn 4096
      unless (B.null chunk) $ do
        let parts = BC.split '\n' (buf <> chunk)
            rest = last parts
        mapM_ (\l -> run =<< utf8String (BC.filter (/= '\r') l)) (init parts)
        -- A line longer than any command is noise; drop it.
        loop (if B.length rest > 65536 then B.empty else rest)
    run l = case break (== ' ') (dropWhile (== ' ') l) of
      (cid, more) | not (null cid), all isDigit cid -> do
        let (cmd, rest) = break (== ' ') (dropWhile (== ' ') more)
            reply s = send (unwords ["A", cid, s] <> "\n")
        if not accepting
          then reply "error commands are turned off in this program (agentAcceptCommands)"
          else do
            r <- try (perform cmd (drop 1 rest))
            case r of
              Left (e :: SomeException) -> reply ("error " <> filter (/= '\n') (show e))
              Right (Left why) -> reply ("error " <> why)
              Right (Right ()) -> reply . ("ok" <>) =<< stampNow
      _ -> pure ()
    -- Every command but a flush also posts a marker, so it lands on the
    -- timeline of a viewer following the eventlog.
    perform cmd rest = case (cmd, words rest) of
      ("mark", _) -> do
        let text = reverse (dropWhile (== ' ') (reverse (dropWhile (== ' ') rest)))
        traceMarkerIO (if null text then "mark" else text)
        flushEventLog
        pure (Right ())
      ("gc", []) -> marked "stronghead: major GC" performMajorGC
      ("census", []) -> marked "stronghead: heap census" requestHeapCensus
      ("heap-profiling", ["on"]) -> marked "stronghead: heap profiling on" startHeapProfTimer
      ("heap-profiling", ["off"]) -> marked "stronghead: heap profiling off" stopHeapProfTimer
      ("flush", []) -> Right () <$ flushEventLog
      _ -> pure (Left ("unknown command " <> cmd))
    marked text act = do
      traceMarkerIO text
      _ <- act
      flushEventLog
      pure (Right ())

-- | @ t=<ns>@, the program time now as eventlog timestamps count it, when
-- the program runs with @+RTS -T@; nothing otherwise.
stampNow :: IO String
stampNow = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      s <- getRTSStats
      pure (" t=" <> show (toInteger (elapsed_ns s) + toInteger (init_elapsed_ns s)))
    else pure ""

showStatus :: ThreadStatus -> String
showStatus = \case
  ThreadRunning -> "running"
  ThreadFinished -> "finished"
  ThreadDied -> "died"
  ThreadBlocked r -> filter (/= ' ') (show r)

renderStats :: RTSStats -> String
renderStats s =
  unwords $
    "S rts_stats=1"
      : [ k <> "=" <> show v
        | (k, v) <-
            [ ("gcs", fromIntegral (gcs s))
            , ("major_gcs", fromIntegral (major_gcs s))
            , ("allocated_bytes", toInteger (allocated_bytes s))
            , ("max_live_bytes", toInteger (max_live_bytes s))
            , ("max_mem_in_use_bytes", toInteger (max_mem_in_use_bytes s))
            , ("copied_bytes", toInteger (copied_bytes s))
            , ("mutator_cpu_ns", toInteger (mutator_cpu_ns s))
            , ("mutator_elapsed_ns", toInteger (mutator_elapsed_ns s))
            , ("gc_cpu_ns", toInteger (gc_cpu_ns s))
            , ("gc_elapsed_ns", toInteger (gc_elapsed_ns s))
            , ("cpu_ns", toInteger (cpu_ns s))
            , ("elapsed_ns", toInteger (elapsed_ns s))
            , ("init_elapsed_ns", toInteger (init_elapsed_ns s))
            , ("gc_gen", fromIntegral (gcdetails_gen g))
            , ("gc_live_bytes", toInteger (gcdetails_live_bytes g))
            , ("gc_large_objects_bytes", toInteger (gcdetails_large_objects_bytes g))
            , ("gc_mem_in_use_bytes", toInteger (gcdetails_mem_in_use_bytes g))
            , ("gc_copied_bytes", toInteger (gcdetails_copied_bytes g))
            , ("gc_elapsed_ns", toInteger (gcdetails_elapsed_ns g))
            , ("gc_threads", fromIntegral (gcdetails_threads g))
            ]
        ]
  where
    g = gc s

-- ---------------------------------------------------------------------------
-- Text on the wire

utf8Bytes :: String -> B.ByteString
utf8Bytes = BL.toStrict . toLazyByteString . stringUtf8

utf8String :: B.ByteString -> IO String
utf8String bs = B.useAsCStringLen bs (GHC.peekCStringLen utf8)

-- | Percent-encode the UTF-8 bytes of everything but ASCII letters, digits
-- and @-_./:\\@, so a field holds no space, comma, @=@ or @%@.
percentEncode :: String -> String
percentEncode = concatMap byte . B.unpack . utf8Bytes
  where
    byte :: Word8 -> String
    byte w
      | isAscii c && isAlphaNum c || c `elem` ("-_./:\\" :: String) = [c]
      | otherwise = '%' : hexDigit (w `div` 16) : [hexDigit (w `mod` 16)]
      where
        c = chr (fromIntegral w)
    hexDigit d = "0123456789ABCDEF" !! fromIntegral d

-- | The working directory, from the C library: @directory@ would be a
-- dependency the agent promises not to have.
currentDirectory :: IO FilePath
#if defined(mingw32_HOST_OS)
currentDirectory = allocaBytes (2 * 32768) $ \buf -> do
  p <- c_wgetcwd buf 32768
  if p == nullPtr then pure "" else peekCWString buf

foreign import ccall unsafe "_wgetcwd" c_wgetcwd :: CWString -> CInt -> IO CWString
#else
currentDirectory = allocaBytes 4096 $ \buf -> do
  p <- c_getcwd buf 4096
  enc <- getFileSystemEncoding
  if p == nullPtr then pure "" else GHC.peekCString enc buf

foreign import ccall unsafe "getcwd" c_getcwd :: CString -> CSize -> IO CString
#endif
