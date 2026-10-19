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
module Stronghead.Agent
  ( AgentConfig (..)
  , defaultAgentConfig
  , withAgent
  , startAgent
  ) where

import Control.Concurrent
import Control.Exception (SomeException, bracket, catch, finally, mask_)
import Control.Monad (forM, forever)
import Data.ByteString.Char8 qualified as BC
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Debug.Trace (flushEventLog)
import GHC.Conc (labelThread)
import GHC.Conc.Sync (ThreadStatus (..), fromThreadId, listThreads, threadLabel, threadStatus)
import GHC.Stats
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.Environment (getProgName)
import System.Posix.Internals (c_getpid)

data AgentConfig = AgentConfig
  { agentHost :: !String
  , agentPort :: !Int
  , agentIntervalMs :: !Int
  , agentFlushEventlog :: !Bool
  }

defaultAgentConfig :: AgentConfig
defaultAgentConfig = AgentConfig "127.0.0.1" 4777 250 True

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
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 4
    forever $ do
      (conn, _) <- accept sock
      mask_ $ do
        t <- forkIO (client cfg conn `catch` (\(_ :: SomeException) -> pure ()) `finally` close conn)
        labelThread t "stronghead-agent-client"

client :: AgentConfig -> Socket -> IO ()
client cfg conn = do
  prog <- getProgName
  pid <- c_getpid
  caps <- getNumCapabilities
  sendAll conn (BC.pack (unwords ["H", filter (/= ' ') prog, show pid, show caps] <> "\n"))
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
      pure $ case st of
        ThreadFinished -> []
        ThreadDied -> []
        _ -> [unwords ["T", show (fromThreadId t), showStatus st, fromMaybe "" lbl]]
    caps' <- getNumCapabilities
    let frame = intercalate "\n" ((statsLine <> " capabilities=" <> show caps') : rows ++ ["E", ""])
    sendAll conn (BC.pack frame)
    threadDelay (agentIntervalMs cfg * 1000)

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
