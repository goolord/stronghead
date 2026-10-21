{-# LANGUAGE CPP #-}

-- | Embedded telemetry server for the Stronghead Live panel.
--
-- > import Stronghead.Agent
-- >
-- > main :: IO ()
-- > main = withAgent defaultAgentConfig $ do
-- >   ...
--
-- Build with @-threaded@ and run with @+RTS -T@ to enable RTS statistics.
-- By default the agent listens on @127.0.0.1:4777@ and streams a frame every
-- 250 ms. When the program writes an eventlog (@+RTS -l@), the agent also
-- flushes it at the same interval.
--
-- Protocol v2 (see "Stronghead.Live") sends process metadata on connect,
-- reports per-thread capabilities, and accepts commands (emit a marker, run
-- a major GC, request a heap census, toggle heap profiling, flush the
-- eventlog). Commands are enabled by default only on loopback addresses
-- ('agentAcceptCommands').
module Stronghead.Agent
  ( AgentConfig (..)
  , defaultAgentConfig
  , withAgent
  , startAgent
  ) where

import Control.Concurrent
import Control.Exception (SomeException, bracket, catch, finally, mask_, try)
import Control.Monad (forM, forever, unless)
import Data.Char (chr, intToDigit, isAlphaNum, isAscii, isDigit, toUpper)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.List (dropWhileEnd, intercalate, unsnoc)
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
import System.Environment (getExecutablePath, getProgName)
import System.Mem (performMajorGC)
import System.Posix.Internals (c_getpid)

import Data.ByteString qualified as B
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
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
    -- ^ Whether to accept client commands. Defaults to 'Nothing' (enabled
    -- only on loopback addresses).
  }

defaultAgentConfig :: AgentConfig
defaultAgentConfig = AgentConfig "127.0.0.1" 4777 250 True Nothing

-- | Run an action with the agent server running in the background.
--
-- On Windows, a blocking @accept@ is an uninterruptible foreign call, so
-- synchronous 'killThread' would block; cleanup kills the threads
-- asynchronously instead.
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
  addrs <-
    getAddrInfo
      (Just defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream})
      (Just (agentHost cfg))
      (Just (show (agentPort cfg)))
  for_ (listToMaybe addrs) \addr -> do
    let accepting = fromMaybe (isLoopback (addrAddress addr)) (agentAcceptCommands cfg)
    bracket (openSocket addr) close \sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 4
      forever do
        (conn, _) <- accept sock
        mask_ do
          t <- forkIO (client cfg accepting conn `catch` (\(_ :: SomeException) -> pure ()) `finally` close conn)
          labelThread t "stronghead-agent-client"
  where
    listToMaybe = \case x : _ -> Just x; [] -> Nothing

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
  cwd <- fromRight "" <$> try @SomeException currentDirectory
  exe <- fromRight "" <$> try @SomeException getExecutablePath
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
      unless (B.null chunk) $
        for_ (unsnoc (BC.split '\n' (buf <> chunk))) \(lines', rest) -> do
          mapM_ (\l -> run =<< utf8String (BC.filter (/= '\r') l)) lines'
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
        let text = dropWhileEnd (== ' ') (dropWhile (== ' ') rest)
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
      | otherwise = ['%', hexDigit (w `div` 16), hexDigit (w `mod` 16)]
      where
        c = chr (fromIntegral w)
    hexDigit = toUpper . intToDigit . fromIntegral

-- | Get the current working directory via libc without depending on @directory@.
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
