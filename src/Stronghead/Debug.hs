{-# LANGUAGE CPP #-}

-- | Live closure graphs from ghc-debug: connect to a program running
-- ghc-debug-stub, pause it, walk everything reachable from its GC roots,
-- resume it, and analyse the graph ("Stronghead.Debug.Graph").
--
-- The backend is behind the @ghc-debug@ cabal flag (off by default). Without
-- it this module still exports the model and analyses, and 'takeSnapshot'
-- reports that support was not built in.
module Stronghead.Debug
  ( module Stronghead.Debug.Graph
  , ghcDebugEnabled
  , takeSnapshot
  , pauseTarget
  , listSockets
  ) where

import Stronghead.Debug.Graph
#ifdef GHC_DEBUG
import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Bifunctor (first)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Stronghead.Debug.Client qualified as Client
#endif

-- | Whether this build can talk to ghc-debug.
ghcDebugEnabled :: Bool

-- | Pause the target, walk its closure graph, resume it, and index the
-- result. Runs for as long as the walk takes: call it off the UI thread.
takeSnapshot :: SnapshotOptions -> DebugTarget -> IO (Either String HeapSnapshot)

-- | Pause a running target and hold it paused; the action returned
-- resumes it. Blocks while connecting: call it off the UI thread.
pauseTarget :: DebugTarget -> IO (Either String (IO ()))

-- | Sockets in ghc-debug's default socket directory, where
-- 'GHC.Debug.Stub.withGhcDebug' creates them.
listSockets :: IO [FilePath]

#ifdef GHC_DEBUG
ghcDebugEnabled = True

takeSnapshot opts target =
  first (displayException @SomeException) <$> try do
    (roots, nodes, truncated) <- Client.walkTarget opts target
    now <- getZonedTime
    let stamp = T.pack (formatTime defaultTimeLocale "%H:%M:%S" now)
    -- Every field is strict, so this runs the analyses here rather than in
    -- the first frame that looks at them.
    evaluate (mkSnapshot (targetText target <> " at " <> stamp) truncated roots nodes)

pauseTarget target =
  first (displayException @SomeException) <$> try (Client.holdPaused target)

listSockets = Client.listSockets
#else
ghcDebugEnabled = False

takeSnapshot _ _ =
  pure (Left "This build of Stronghead has no ghc-debug support. Rebuild with: cabal build -f ghc-debug")

pauseTarget _ =
  pure (Left "This build of Stronghead has no ghc-debug support. Rebuild with: cabal build -f ghc-debug")

listSockets = pure []
#endif
