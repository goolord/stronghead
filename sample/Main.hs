{-# LANGUAGE CPP #-}

-- | A small program with the problems Stronghead is for:
--
-- * worker threads contending on one MVar (red blocked spans on the
--   timeline),
-- * a space leak: a lazy accumulator in an IORef that is modified but never
--   forced, so a chain of thunks grows until the end,
-- * a cache that only ever grows,
-- * labelled threads, traceMarker milestones, and the live agent.
--
-- Run it with the eventlog and a closure-type heap profile:
--
-- > cabal run stronghead-sample -- 10 +RTS -l -hT -i0.1 -RTS
--
-- then open stronghead-sample.eventlog (or .hp) in Stronghead, or connect
-- the Live panel to 127.0.0.1:4777 while it runs.
--
-- Built with @-f ghc-debug@ (not on Windows), it also runs ghc-debug-stub,
-- on the socket it prints, or on TCP when @GHC_DEBUG_TCP=host:port@ is set,
-- for the heap panel's live graph.
module Main (main) where

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Monad
import Data.IORef
import Data.Map.Strict qualified as M
import Debug.Trace (traceEventIO, traceMarkerIO)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (labelThread)
import Stronghead.Agent
import System.Environment (getArgs)
#ifdef GHC_DEBUG_STUB
import GHC.Debug.Stub (withGhcDebug, withGhcDebugTCP)
import System.Environment (lookupEnv)
#endif
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = withDebugStub $ withAgent defaultAgentConfig $ do
  args <- getArgs
  let seconds = case args of
        s : _ | [(n, "")] <- reads s -> n
        _ -> 8 :: Int
  hSetBuffering stdout LineBuffering
  me <- myThreadId
  labelThread me "main"
  traceMarkerIO "start"
  start <- getMonotonicTime
  let deadline = start + fromIntegral seconds
      -- Every background loop runs until the deadline and then reports in;
      -- nothing is killed, so the program always shuts down cleanly. The
      -- body gets the iteration number: work that does not depend on it
      -- would be floated out to a CAF, leaving a non-allocating spin loop
      -- that never reaches a safe point and stalls every GC sync.
      untilDeadline body = do
        let loop !n = do
              body n
              now <- getMonotonicTime
              when (now < deadline) (loop (n + 1))
        loop (0 :: Int)

  shared <- newMVar (0 :: Int)
  leak <- newIORef (0 :: Integer)
  cache <- newIORef M.empty
  finished <- newEmptyMVar
  let spawn name body = do
        t <- forkIO (untilDeadline body >> putMVar finished ())
        labelThread t name

  -- Contended workers: each does its pure work while holding the shared
  -- MVar, so the others queue up behind it.
  forM_ [1 .. 6 :: Int] $ \i -> spawn ("worker-" <> show i) $ \iter -> do
    modifyMVar_ shared $ \n -> do
      x <- evaluate (length (show [1 .. 2000 + i * 100 + iter `mod` 7]))
      pure (n + x `mod` 7)
    yield

  -- The leak: modifyIORef (not modifyIORef') builds (+) thunks forever.
  spawn "leaky-accumulator" $ \_ -> do
    forM_ [1 .. 2000 :: Integer] $ \k -> modifyIORef leak (+ k)
    threadDelay 1000

  -- A cache nobody evicts from.
  counter <- newIORef (0 :: Int)
  spawn "cache-hoarder" $ \_ -> do
    k <- atomicModifyIORef' counter (\c -> (c + 1, c))
    modifyIORef' cache (M.insert k (replicate 20 k))
    when (k `mod` 200 == 0) (threadDelay 1000)

  -- A pure, allocation-heavy CPU burner: lots of short-lived garbage.
  spawn "map-builder" $ \iter ->
    void (evaluate (M.size (M.fromList [(i, i * iter) | i <- [1 .. 20000 :: Int]])))

  forM_ [1 .. seconds] $ \s -> do
    threadDelay 1000000
    traceMarkerIO ("second " <> show s)
    traceEventIO ("tick " <> show s)

  replicateM_ 9 (takeMVar finished)
  traceMarkerIO "all threads done"
  total <- readIORef leak
  size <- M.size <$> readIORef cache
  putStrLn ("leaked sum: " <> show total <> ", cache entries: " <> show size)

withDebugStub :: IO a -> IO a
#ifdef GHC_DEBUG_STUB
withDebugStub body =
  lookupEnv "GHC_DEBUG_TCP" >>= \case
    Just hp | (host, ':' : port) <- break (== ':') hp, [(n, "")] <- reads port -> withGhcDebugTCP host n body
    _ -> withGhcDebug body
#else
withDebugStub = id
#endif
