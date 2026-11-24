{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Embedded telemetry server and lightweight GHC 9.10+ instrumentation for
-- Stronghead.
--
-- > import Stronghead.Agent
-- >
-- > main :: IO ()
-- > main = withAgent defaultAgentConfig $ do
-- >   withSpan "startup" initApp
-- >   ...
--
-- Build with @-threaded@ (and @-finfo-table-map@ for decoded stack frames and
-- exception backtraces) and run with @+RTS -T@ for RTS statistics.
--
-- Protocol v3 is backward-compatible with v1 and v2 clients: normal frames
-- emit only @H@, @I@, @S@, @T@, @C@, and @E@. Opt-in v3 commands unlock
-- cross-platform @GHC.Stack.CloneStack@ live thread stacks and sampling,
-- era bumping, @ghc-heap@ value probes (via @stronghead:probe@), and remote
-- eventlog chunk relay over TCP.
module Stronghead.Agent
  ( -- * Configuration & lifecycle
    AgentConfig (..)
  , defaultAgentConfig
  , withAgent
  , startAgent
  , startAgentListening
  , agentBeaconsDir
    -- * Zero-overhead spans & allocation tripwires
  , withSpan
  , withAllocGuard
    -- * Exception backtrace tap
  , enableIPEBacktraces
  , withExceptionTap
    -- * Era profiling
  , getEra
  , setEra
  , bumpEra
  , withEra
    -- * Pluggable heap value probes (implemented by @stronghead:probe@)
  , ProbeNode (..)
  , registerProbe
  , unregisterProbe
  , listProbes
  ) where

import Control.Concurrent
import Control.Exception
  ( SomeException (..)
  , bracket
  , catch
  , displayException
  , finally
  , mask_
  , throwIO
  , try
  )
import Control.Exception.Backtrace
  ( BacktraceMechanism (..)
  , setBacktraceMechanismState
  )
import Control.Monad (forM, forever, unless, void, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, intToDigit, isAlphaNum, isAscii, isDigit, toUpper)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Typeable (Proxy (..), typeRep)
import Data.Word (Word64, Word8)
import Debug.Trace (flushEventLog, traceMarkerIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import GHC.Conc (labelThread)
import GHC.Conc.Sync
  ( ThreadStatus (..)
  , fromThreadId
  , getAllocationCounter
  , listThreads
  , threadLabel
  , threadStatus
  )
import GHC.Environment (getFullArgs)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (utf8)
import GHC.Profiling (requestHeapCensus, startHeapProfTimer, stopHeapProfTimer)
import GHC.RTS.Flags (DoHeapProfile (..), DoTrace (..), ProfFlags (..), TraceFlags (..), getProfFlags, getTraceFlags)
import GHC.Stack.CloneStack (StackEntry (..), cloneMyStack, cloneThreadStack, decode)
import GHC.Stats
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getExecutablePath, getProgName, lookupEnv)
import System.FilePath (isAbsolute, (<.>), (</>))
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hClose, hSeek, openBinaryFile)
import System.IO.Unsafe (unsafePerformIO)
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

-- ---------------------------------------------------------------------------
-- Configuration

data AgentConfig = AgentConfig
  { agentHost :: !String
  , agentPort :: !Int
    -- ^ TCP port to listen on; @0@ asks the OS for an ephemeral port.
  , agentIntervalMs :: !Int
  , agentFlushEventlog :: !Bool
  , agentAcceptCommands :: !(Maybe Bool)
    -- ^ Whether to accept client commands. Defaults to 'Nothing' (enabled
    -- only on loopback addresses).
  , agentDiscoverable :: !Bool
    -- ^ Write a local discovery beacon under 'agentBeaconsDir' while
    -- running so Stronghead's Live panel can list this process automatically.
  }

defaultAgentConfig :: AgentConfig
defaultAgentConfig = AgentConfig "127.0.0.1" 4777 250 True Nothing True

-- ---------------------------------------------------------------------------
-- Process-wide state (probes, spans, live alert listeners)

-- | One closure node returned by a registered in-process probe (for example
-- from @stronghead:probe@ using @ghc-heap@).
data ProbeNode = ProbeNode
  { pnId :: !Int
  , pnParent :: !Int
    -- ^ @-1@ for the root closure.
  , pnType :: !String
    -- ^ Closure type (@THUNK_2_0@, @CONSTR@, @ARR_WORDS@, @MUT_VAR_CLEAN@...).
  , pnName :: !String
    -- ^ Constructor name, IPE label, or closure description.
  , pnSource :: !String
    -- ^ Source span from @GHC.InfoProv.whereFrom@, or empty.
  , pnBytes :: !Int
    -- ^ Estimated shallow size in bytes.
  , pnIsThunk :: !Bool
  , pnDetail :: !String
    -- ^ Extra info (e.g., payload preview or free-var count).
  }
  deriving stock (Eq, Show)

{-# NOINLINE probeRegistry #-}
probeRegistry :: IORef (Map String (IO [ProbeNode]))
probeRegistry = unsafePerformIO (newIORef M.empty)

-- | Register a named live value probe callback.
registerProbe :: String -> IO [ProbeNode] -> IO ()
registerProbe name action =
  atomicModifyIORef' probeRegistry (\m -> (M.insert name action m, ()))

-- | Remove a previously registered probe.
unregisterProbe :: String -> IO ()
unregisterProbe name =
  atomicModifyIORef' probeRegistry (\m -> (M.delete name m, ()))

-- | Names of all currently registered probes.
listProbes :: IO [String]
listProbes = M.keys <$> readIORef probeRegistry

{-# NOINLINE spanCounter #-}
spanCounter :: IORef Word64
spanCounter = unsafePerformIO (newIORef 1)

{-# NOINLINE spanStacks #-}
spanStacks :: IORef (Map ThreadId [Word64])
spanStacks = unsafePerformIO (newIORef M.empty)

{-# NOINLINE alertSubscribers #-}
alertSubscribers :: IORef (Map Int (String -> IO ()))
alertSubscribers = unsafePerformIO (newIORef M.empty)

{-# NOINLINE alertSubCounter #-}
alertSubCounter :: IORef Int
alertSubCounter = unsafePerformIO (newIORef 1)

subscribeAlerts :: (String -> IO ()) -> IO Int
subscribeAlerts send = do
  sid <- atomicModifyIORef' alertSubCounter (\n -> (n + 1, n))
  atomicModifyIORef' alertSubscribers (\m -> (M.insert sid send m, ()))
  pure sid

unsubscribeAlerts :: Int -> IO ()
unsubscribeAlerts sid =
  atomicModifyIORef' alertSubscribers (\m -> (M.delete sid m, ()))

broadcastAlert :: String -> String -> String -> IO ()
broadcastAlert kind title detail = do
  subs <- M.elems <$> readIORef alertSubscribers
  unless (null subs) do
    t <- currentProgramNs
    tid <- fromThreadId <$> myThreadId
    let line =
          unwords
            [ "X"
            , show t
            , show tid
            , percentEncode kind
            , percentEncode title
            , percentEncode detail
            ]
            <> "\n"
    for_ subs (\send -> void (try @SomeException (send line)))

-- ---------------------------------------------------------------------------
-- Zero-overhead spans, allocation tripwires, exceptions & eras

-- | Bracket an action with a structured eventlog span recording exact
-- per-thread allocation bytes via 'GHC.Conc.Sync.getAllocationCounter'
-- (zero @-prof@ or @-ticky@ overhead).
withSpan :: String -> IO a -> IO a
withSpan name action = do
  tid <- myThreadId
  sid <- atomicModifyIORef' spanCounter (\n -> (n + 1, n))
  parent <- atomicModifyIORef' spanStacks $ \m ->
    let stack = M.findWithDefault [] tid m
        p = fromMaybe 0 (listToMaybe stack)
     in (M.insert tid (sid : stack) m, p)
  allocBefore <- getAllocationCounter
  traceMarkerIO (unwords ["sh:span:begin", show sid, show parent, percentEncode name])
  action `finally` do
    allocAfter <- getAllocationCounter
    let allocated = max 0 (allocBefore - allocAfter)
    traceMarkerIO (unwords ["sh:span:end", show sid, show allocated, percentEncode name])
    atomicModifyIORef' spanStacks $ \m ->
      let m' = case M.lookup tid m of
            Just (_ : rest@(_ : _)) -> M.insert tid rest m
            _ -> M.delete tid m
       in (m', ())

-- | Guard an action with a non-aborting allocation tripwire (in bytes). When
-- the action allocates more than @budgetBytes@ on the current thread,
-- Stronghead captures a @CloneStack@ backtrace, posts an eventlog marker,
-- and broadcasts a live alert to connected viewers.
withAllocGuard :: String -> Int64 -> IO a -> IO a
withAllocGuard name budgetBytes action = do
  before <- getAllocationCounter
  action `finally` do
    after <- getAllocationCounter
    let allocated = max 0 (before - after)
    when (allocated > budgetBytes) do
      frames <- fromRight [] <$> try @SomeException (cloneMyStack >>= decode)
      let top = intercalate ";" (map encodeStackEntry (take 16 frames))
      traceMarkerIO (unwords ["sh:alloc-spike", percentEncode name, show allocated, show budgetBytes, top])
      broadcastAlert "alloc-spike" (name <> " allocated " <> show allocated <> " B (budget " <> show budgetBytes <> " B)") top

-- | Enable GHC 9.10+ IPE backtraces on thrown exceptions.
enableIPEBacktraces :: IO ()
enableIPEBacktraces = setBacktraceMechanismState IPEBacktrace True

-- | Observe exceptions escaping an action (without swallowing them), posting
-- their type and decoded @CloneStack@ frames as an eventlog marker and live
-- alert.
withExceptionTap :: IO a -> IO a
withExceptionTap action =
  action `catch` \(e :: SomeException) -> do
    frames <- fromRight [] <$> try @SomeException (cloneMyStack >>= decode)
    let tyName = exceptionTypeName e
        msg = oneLine (displayException e)
        bt = intercalate ";" (map encodeStackEntry (take 16 frames))
        detail = oneLine (if null bt then msg else msg <> " | " <> bt)
    traceMarkerIO (unwords ["sh:exc", percentEncode tyName, percentEncode detail])
    broadcastAlert "exception" tyName detail
    throwIO e

exceptionTypeName :: SomeException -> String
exceptionTypeName (SomeException (_ :: exc)) = show (typeRep (Proxy @exc))

-- | Era profiling helpers backed by the GHC RTS (@rts/prof/Heap.h@) when
-- era profiling (@+RTS -he@) is active, with a pure process-wide counter
-- mirror otherwise.
foreign import ccall unsafe "getUserEra" c_getUserEra :: IO Word
foreign import ccall unsafe "setUserEra" c_setUserEra :: Word -> IO ()
foreign import ccall unsafe "incrementUserEra" c_incrementUserEra :: Word -> IO Word

{-# NOINLINE softEra #-}
softEra :: IORef Word
softEra = unsafePerformIO (newIORef 0)

eraProfilingActive :: IO Bool
eraProfilingActive = do
  pf <- getProfFlags
  pure case doHeapProfile pf of
    HeapByEra -> True
    _ -> False

getEra :: IO Word
getEra = do
  active <- eraProfilingActive
  if active then c_getUserEra else readIORef softEra

setEra :: Word -> IO ()
setEra w = do
  writeIORef softEra w
  active <- eraProfilingActive
  when active (c_setUserEra w)

-- | Advance the user era by 1 and return the new era number.
bumpEra :: IO Word
bumpEra = do
  active <- eraProfilingActive
  if active
    then do
      w <- c_incrementUserEra 1
      writeIORef softEra w
      pure w
    else atomicModifyIORef' softEra (\w -> let w' = w + 1 in (w', w'))

-- | Run an action under a specific user era, restoring the previous era
-- afterward.
withEra :: Word -> IO a -> IO a
withEra era action = bracket (getEra <* setEra era) setEra (const action)

-- ---------------------------------------------------------------------------
-- Discovery beacons

-- | Directory where local Stronghead agents publish discovery beacons.
agentBeaconsDir :: IO FilePath
agentBeaconsDir = do
#if defined(mingw32_HOST_OS)
  base <- fromMaybe "." <$> lookupEnv "LOCALAPPDATA"
  pure (base </> "stronghead" </> "agents")
#else
  xdg <- lookupEnv "XDG_STATE_HOME"
  case xdg of
    Just d | not (null d) -> pure (d </> "stronghead" </> "agents")
    _ -> do
      home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
      pure (home </> ".stronghead" </> "agents")
#endif

writeBeacon :: String -> Int -> IO (Maybe FilePath)
writeBeacon host port =
  fromRight Nothing <$> try @SomeException do
    dir <- agentBeaconsDir
    createDirectoryIfMissing True dir
    pid <- c_getpid
    prog <- getProgName
    cwd <- fromRight "" <$> try @SomeException currentDirectory
    let path = dir </> (show pid <> "-" <> show port <.> "agent")
        body =
          unlines
            [ "version=3"
            , "pid=" <> show pid
            , "program=" <> filter (/= '\n') prog
            , "host=" <> host
            , "port=" <> show port
            , "cwd=" <> filter (/= '\n') cwd
            ]
    writeFile path body
    pure (Just path)

removeBeacon :: Maybe FilePath -> IO ()
removeBeacon = mapM_ (\p -> void (try @SomeException (removeFile p)))

-- ---------------------------------------------------------------------------
-- Lifecycle

-- | Run an action with the agent server running in the background.
--
-- On Windows, a blocking @accept@ is an uninterruptible foreign call, so
-- synchronous 'killThread' would block; cleanup kills the threads
-- asynchronously instead.
withAgent :: AgentConfig -> IO a -> IO a
withAgent cfg act =
  bracket
    (startAgentInternal cfg)
    (\(_, mBeacon, ts) -> removeBeacon mBeacon >> mapM_ (forkIO . killThread) ts)
    (\_ -> act)

-- | Start the server (and the eventlog flusher); returns their threads.
startAgent :: AgentConfig -> IO [ThreadId]
startAgent cfg = (\(_, _, ts) -> ts) <$> startAgentInternal cfg

-- | Like 'startAgent', also returning the actual bound TCP port (useful when
-- 'agentPort' is @0@).
startAgentListening :: AgentConfig -> IO (Int, [ThreadId])
startAgentListening cfg = (\(p, _, ts) -> (p, ts)) <$> startAgentInternal cfg

startAgentInternal :: AgentConfig -> IO (Int, Maybe FilePath, [ThreadId])
startAgentInternal cfg = do
  portVar <- newEmptyMVar
  server <- forkIO (serve cfg portVar `catch` \(_ :: SomeException) -> void (tryPutMVar portVar (agentPort cfg)))
  labelThread server "stronghead-agent"
  boundPort <- takeMVar portVar
  mBeacon <-
    if agentDiscoverable cfg && boundPort > 0
      then writeBeacon (agentHost cfg) boundPort
      else pure Nothing
  flusher <-
    if agentFlushEventlog cfg
      then do
        t <- forkIO . forever $ do
          threadDelay (agentIntervalMs cfg * 1000)
          flushEventLog
        labelThread t "stronghead-eventlog-flush"
        pure [t]
      else pure []
  pure (boundPort, mBeacon, server : flusher)

serve :: AgentConfig -> MVar Int -> IO ()
serve cfg portVar = do
  addrs <-
    getAddrInfo
      (Just defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream})
      (Just (agentHost cfg))
      (Just (show (agentPort cfg)))
  case listToMaybe addrs of
    Nothing -> void (tryPutMVar portVar (agentPort cfg))
    Just addr -> do
      let accepting = fromMaybe (isLoopback (addrAddress addr)) (agentAcceptCommands cfg)
      bracket (openSocket addr) close \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        actualPort <- fromIntegral <$> socketPort sock
        listen sock 4
        void (tryPutMVar portVar actualPort)
        forever do
          (conn, _) <- accept sock
          mask_ do
            t <- forkIO (client cfg actualPort accepting conn `catch` (\(_ :: SomeException) -> pure ()) `finally` close conn)
            labelThread t "stronghead-agent-client"

isLoopback :: SockAddr -> Bool
isLoopback = \case
  SockAddrInet _ h -> let (a, _, _, _) = hostAddressToTuple h in a == 127
  SockAddrInet6 _ _ h _ -> hostAddress6ToTuple h == (0, 0, 0, 0, 0, 0, 0, 1)
  SockAddrUnix _ -> True

-- ---------------------------------------------------------------------------
-- Per-client session

data ClientSubtasks = ClientSubtasks
  { csSampler :: !(IORef (Maybe ThreadId))
  , csRelay :: !(IORef (Maybe ThreadId))
  }

client :: AgentConfig -> Int -> Bool -> Socket -> IO ()
client cfg actualPort accepting conn = do
  -- Frames and command replies share the socket; a line is never split.
  lock <- newMVar ()
  let send s = withMVar lock (\_ -> sendAll conn (utf8Bytes s))
  subtasks <- ClientSubtasks <$> newIORef Nothing <*> newIORef Nothing
  subId <- subscribeAlerts send
  let cleanup = do
        unsubscribeAlerts subId
        stopRef (csSampler subtasks)
        stopRef (csRelay subtasks)
  (`finally` cleanup) do
    prog <- getProgName
    pid <- c_getpid
    caps <- getNumCapabilities
    send (unwords ["H", filter (/= ' ') prog, show pid, show caps] <> "\n")
    send . (<> "\n") =<< infoLine actualPort accepting
    -- The reader ends by itself when the viewer goes away (or the socket is
    -- closed under it); a v1 viewer never writes, so it just waits.
    reader <- forkIO (commands accepting subtasks send conn `catch` \(_ :: SomeException) -> pure ())
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

stopRef :: IORef (Maybe ThreadId) -> IO ()
stopRef ref = do
  old <- atomicModifyIORef' ref (\x -> (Nothing, x))
  for_ old (void . forkIO . killThread)

-- | What one Connect needs to find the rest of the program: where it runs,
-- how it was started, and what the RTS is writing.
infoLine :: Int -> Bool -> IO String
infoLine actualPort accepting = do
  cwd <- fromRight "" <$> try @SomeException currentDirectory
  exe <- fromRight "" <$> try @SomeException getExecutablePath
  args <- getFullArgs
  prof <- getProfFlags
  tr <- getTraceFlags
  probes <- listProbes
  let eventlog = case tracing tr of
        TraceEventLog -> True
        _ -> False
  pure $
    unwords
      [ "I"
      , "version=3"
      , "port=" <> show actualPort
      , "cwd=" <> percentEncode cwd
      , "exe=" <> percentEncode exe
      , "args=" <> intercalate "," (map percentEncode args)
      , "heap-profile=" <> heapMode (doHeapProfile prof)
      , "heap-interval-ms=" <> show (heapProfileInterval prof `div` 1000000)
      , "eventlog=" <> flag eventlog
      , "commands=" <> flag accepting
      , "stacks=1"
      , "probes=" <> flag (not (null probes))
      , "relay=" <> flag eventlog
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
commands :: Bool -> ClientSubtasks -> (String -> IO ()) -> Socket -> IO ()
commands accepting subtasks send conn = loop B.empty
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
              Left (e :: SomeException) -> reply ("error " <> oneLine (show e))
              Right (Left why) -> reply ("error " <> why)
              Right (Right ()) -> reply . ("ok" <>) =<< stampNow
      _ -> pure ()
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
      ("era-bump", []) -> do
        e <- bumpEra
        traceMarkerIO ("stronghead: era " <> show e)
        flushEventLog
        pure (Right ())
      ("stacks", []) -> do
        emitLiveStacks send
        pure (Right ())
      ("sample-stacks", ["off"]) -> do
        stopRef (csSampler subtasks)
        pure (Right ())
      ("sample-stacks", ["on"]) -> startStackSampler subtasks 25 send
      ("sample-stacks", [msStr, "on"])
        | all isDigit msStr, not (null msStr) ->
            startStackSampler subtasks (max 5 (read msStr)) send
      ("sample-stacks", ["on", msStr])
        | all isDigit msStr, not (null msStr) ->
            startStackSampler subtasks (max 5 (read msStr)) send
      ("probes", []) -> do
        emitProbes Nothing send
        pure (Right ())
      ("probe", _) -> do
        let name = dropWhileEnd (== ' ') (dropWhile (== ' ') rest)
        emitProbes (if null name then Nothing else Just name) send
        pure (Right ())
      ("relay-eventlog", ["on"]) -> startEventlogRelay subtasks send
      ("relay-eventlog", ["off"]) -> do
        stopRef (csRelay subtasks)
        pure (Right ())
      _ -> pure (Left ("unknown command " <> cmd))
    marked text act = do
      traceMarkerIO text
      _ <- act
      flushEventLog
      pure (Right ())

-- ---------------------------------------------------------------------------
-- Live CloneStack snapshots & background statistical sampling

emitLiveStacks :: (String -> IO ()) -> IO ()
emitLiveStacks send = do
  threads <- listThreads
  rows <- fmap concat . forM threads $ \t -> do
    st <- threadStatus t
    lbl <- threadLabel t
    if isAgentThread lbl || st == ThreadFinished || st == ThreadDied
      then pure []
      else do
        eFrames <- try @SomeException (cloneThreadStack t >>= decode)
        case eFrames of
          Left _ -> pure []
          Right frames ->
            let encoded = intercalate ";" (map encodeStackEntry (take 64 frames))
             in pure [unwords ["K", show (fromThreadId t), showStatus st, encoded]]
  unless (null rows) (send (unlines rows))

startStackSampler :: ClientSubtasks -> Int -> (String -> IO ()) -> IO (Either String ())
startStackSampler subtasks intervalMs send = do
  stopRef (csSampler subtasks)
  accRef <- newIORef (M.empty :: Map (String, String) Int)
  t <- forkIO $ do
    let loop tick = do
          threads <- listThreads
          for_ threads $ \th -> do
            st <- threadStatus th
            lbl <- threadLabel th
            unless (isAgentThread lbl || st == ThreadFinished || st == ThreadDied) do
              eFrames <- try @SomeException (cloneThreadStack th >>= decode)
              case eFrames of
                Right fs@(_ : _) -> do
                  let key = (showStatus st, intercalate ";" (map encodeStackEntry (take 32 fs)))
                  atomicModifyIORef' accRef (\m -> (M.insertWith (+) key 1 m, ()))
                _ -> pure ()
          when (tick >= 10) $ do
            batch <- atomicModifyIORef' accRef (\m -> (M.empty, M.toList m))
            unless (null batch) $
              send $
                unlines
                  [ unwords ["F", stGrp, show cnt, framesStr]
                  | ((stGrp, framesStr), cnt) <- batch
                  ]
          threadDelay (intervalMs * 1000)
          loop (if tick >= 10 then (0 :: Int) else tick + 1)
    loop 0 `catch` \(_ :: SomeException) -> pure ()
  labelThread t "stronghead-agent-sampler"
  writeIORef (csSampler subtasks) (Just t)
  pure (Right ())

isAgentThread :: Maybe String -> Bool
isAgentThread = maybe False ("stronghead-" `isPrefixOf`)

encodeStackEntry :: StackEntry -> String
encodeStackEntry StackEntry {..} =
  intercalate
    "|"
    [ percentEncode functionName
    , percentEncode moduleName
    , percentEncode srcLoc
    , percentEncode (show closureType)
    ]

-- ---------------------------------------------------------------------------
-- Live value probes

emitProbes :: Maybe String -> (String -> IO ()) -> IO ()
emitProbes mName send = do
  reg <- readIORef probeRegistry
  let selected = case mName of
        Nothing -> M.toList reg
        Just n -> maybe [] (\act -> [(n, act)]) (M.lookup n reg)
  rows <- fmap concat . forM selected $ \(name, act) -> do
    eNodes <- try @SomeException act
    case eNodes of
      Left err ->
        pure [unwords ["P", percentEncode name, "0", "-1", "ERROR", percentEncode (oneLine (show err)), "", "0", "0", ""]]
      Right nodes ->
        pure
          [ unwords
              [ "P"
              , percentEncode name
              , show (pnId n)
              , show (pnParent n)
              , percentEncode (pnType n)
              , percentEncode (pnName n)
              , percentEncode (pnSource n)
              , show (pnBytes n)
              , if pnIsThunk n then "1" else "0"
              , percentEncode (pnDetail n)
              ]
          | n <- nodes
          ]
  unless (null rows) (send (unlines rows))

-- ---------------------------------------------------------------------------
-- Remote eventlog relay over TCP

startEventlogRelay :: ClientSubtasks -> (String -> IO ()) -> IO (Either String ())
startEventlogRelay subtasks send = do
  mPath <- ownEventlogPath
  case mPath of
    Nothing -> pure (Left "program is not writing an eventlog (+RTS -l)")
    Just path -> do
      stopRef (csRelay subtasks)
      t <- forkIO $ do
        let loop !off = do
              exists <- doesFileExist path
              off' <-
                if not exists
                  then pure off
                  else do
                    flushEventLog
                    r <- try @SomeException $ bracket (openBinaryFile path ReadMode) hClose \h -> do
                      hSeek h AbsoluteSeek off
                      bs <- B.hGet h 32768
                      if B.null bs
                        then pure off
                        else do
                          send (unwords ["L", show off, encodeBase64 bs] <> "\n")
                          pure (off + fromIntegral (B.length bs))
                    pure (fromRight off r)
              threadDelay 200000
              loop off'
        loop 0 `catch` \(_ :: SomeException) -> pure ()
      labelThread t "stronghead-agent-relay"
      writeIORef (csRelay subtasks) (Just t)
      pure (Right ())

ownEventlogPath :: IO (Maybe FilePath)
ownEventlogPath = do
  tr <- getTraceFlags
  case tracing tr of
    TraceEventLog -> do
      cwd <- fromRight "" <$> try @SomeException currentDirectory
      prog <- getProgName
      args <- getFullArgs
      let rtsArgs = takeWhile (/= "--RTS") (drop 1 (dropWhile (/= "+RTS") args))
          findOpt pfx = listToMaybe (reverse [drop (length pfx) a | a <- rtsArgs, pfx `isPrefixOf` a, length a > length pfx])
          base = fromMaybe (stripExe prog) (findOpt "-po")
          raw = fromMaybe (base <.> "eventlog") (findOpt "-ol")
          full = if isAbsolute raw || null cwd then raw else cwd </> raw
      pure (Just full)
    _ -> pure Nothing
  where
    stripExe s =
      let rev = reverse s
       in if "exe." `isPrefixOf` rev then reverse (drop 4 rev) else s

-- ---------------------------------------------------------------------------
-- Timestamps & stats

currentProgramNs :: IO Word64
currentProgramNs = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      s <- getRTSStats
      pure (fromIntegral (toInteger (elapsed_ns s) + toInteger (init_elapsed_ns s)))
    else pure 0

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
-- Text & binary encoding on the wire

oneLine :: String -> String
oneLine = map (\c -> if c == '\n' || c == '\r' then ' ' else c)

utf8Bytes :: String -> B.ByteString
utf8Bytes = BL.toStrict . toLazyByteString . stringUtf8

utf8String :: B.ByteString -> IO String
utf8String bs = B.useAsCStringLen bs (GHC.peekCStringLen utf8)

-- | Percent-encode the UTF-8 bytes of everything but ASCII letters, digits
-- and @-_./:\\@, so a field holds no space, comma, @=@, @;@, @|@ or @%@.
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

b64Alphabet :: B.ByteString
b64Alphabet = BC.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encodeBase64 :: B.ByteString -> String
encodeBase64 bs = go 0
  where
    len = B.length bs
    at i = fromIntegral (B.index bs i) :: Int
    ch i = BC.index b64Alphabet i
    go i
      | i >= len = []
      | i + 2 < len =
          let b0 = at i; b1 = at (i + 1); b2 = at (i + 2)
              n = (b0 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b2
           in ch (n `shiftR` 18)
                : ch ((n `shiftR` 12) .&. 63)
                : ch ((n `shiftR` 6) .&. 63)
                : ch (n .&. 63)
                : go (i + 3)
      | i + 1 < len =
          let b0 = at i; b1 = at (i + 1)
              n = (b0 `shiftL` 10) .|. (b1 `shiftL` 2)
           in [ch (n `shiftR` 12), ch ((n `shiftR` 6) .&. 63), ch (n .&. 63), '=']
      | otherwise =
          let b0 = at i
           in [ch (b0 `shiftR` 2), ch ((b0 .&. 3) `shiftL` 4), '=', '=']

-- | Get the current working directory via libc without needing path encoding workarounds.
currentDirectory :: IO FilePath
#if defined(mingw32_HOST_OS)
currentDirectory = allocaBytes (2 * 32768) $ \buf -> do
  p <- c_wgetcwd buf 32768
  if p == nullPtr then pure "" else peekCWString buf

foreign import ccall unsafe "_wgetcwd" c_wgetcwd :: CWString -> CInt -> IO CWString
#else
currentDirectory = allocaBytes 4096 $ \buf -> do
  p <- c_getcwd buf 4096
  if p == nullPtr then pure "" else GHC.peekCString enc buf
  where
    enc = unsafePerformIO getFileSystemEncoding

foreign import ccall unsafe "getcwd" c_getcwd :: CString -> CSize -> IO CString
#endif
