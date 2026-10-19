-- | The window: a header with the panel tabs and a global Open, file
-- drag-and-drop, and the per-frame loaders that keep background work going
-- whichever panel is showing.
module UI.Shell
  ( App
  , Options (..)
  , defaultOptions
  , newApp
  , appView
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)
import Stronghead.Dump (kindForPath)
import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Eventlog (LoadProgress (..))
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName)
import UI.Core
import UI.Debugger
import UI.Heap
import UI.Live
import UI.Timeline

data Panel = PTimeline | PHeap | PCore | PDebugger | PLive
  deriving (Eq, Show, Enum, Bounded)

panelTitle :: Panel -> T.Text
panelTitle = \case
  PTimeline -> "Timeline"
  PHeap -> "Heap & leaks"
  PCore -> "Core / STG"
  PDebugger -> "Debugger"
  PLive -> "Live"

tabNamed :: T.Text -> Panel
tabNamed name = case [p | p <- [minBound .. maxBound], T.toLower (T.takeWhile (/= ' ') (panelTitle p)) == name] of
  p : _ -> p
  [] -> PTimeline

data App = App
  { appTab :: !(Var Panel)
  , appTimeline :: !TimelineState
  , appHeap :: !HeapState
  , appCore :: !CoreState
  , appDebug :: !DebugState
  , appLive :: !LiveState
  , appOpen :: !(Var (Maybe FileDialogId))
  , appPending :: !(Var [FilePath])
    -- ^ Paths from the command line, opened on the first frame.
  , appFollow :: !(Var (Maybe FilePath))
  , appStartDebug :: !(Var Bool)
  , appMessage :: !(Var T.Text)
  , appStartTab :: !(Maybe Panel)
    -- ^ A panel asked for on the command line wins over the files' panels.
  }

-- | What the command line asked for.
data Options = Options
  { optFiles :: ![FilePath]
  , optTab :: !(Maybe T.Text)
  , optConnect :: !(Maybe (T.Text, T.Text))
    -- ^ Host and port of an agent to connect the Live panel to.
  , optFollow :: !(Maybe FilePath)
    -- ^ An eventlog to follow as it is written.
  , optDebug :: !(Maybe FilePath)
    -- ^ A module to start a GHCi session on.
  }

defaultOptions :: Options
defaultOptions = Options [] Nothing Nothing Nothing Nothing

newApp :: Options -> IO App
newApp Options {..} = do
  let firstPanel
        | Just _ <- optDebug = PDebugger
        | Just _ <- optConnect = PLive
        | otherwise = PTimeline
  appTab <- newVar (maybe firstPanel tabNamed optTab)
  appTimeline <- newTimelineState
  appHeap <- newHeapState
  appCore <- newCoreState
  appDebug <- newDebugState
  appStartDebug <- newVar False
  for_ optDebug $ \path -> do
    dir <- makeAbsolute (takeDirectory path)
    writeVarIO (dsDir appDebug) (T.pack dir)
    writeVarIO (dsFile appDebug) (T.pack (takeFileName path))
    writeVarIO appStartDebug True
  appLive <- newLiveState
  for_ optConnect $ \(host, port) -> do
    writeVarIO (lvHost appLive) host
    writeVarIO (lvPort appLive) port
    writeVarIO (lvWant appLive) True
  appOpen <- newVar Nothing
  appPending <- newVar optFiles
  appFollow <- newVar optFollow
  appMessage <- newVar ""
  let appStartTab = tabNamed <$> optTab
  pure App {..}

-- | Route a path to the panel that reads it.
openAny :: App -> FilePath -> NanoUI ()
openAny app p = do
  isDir <- liftIO (doesDirectoryExist p)
  if
    | isDir -> openDumpDir (appCore app) p >> writeVar (appTab app) PCore
    | ".eventlog" `isSuffixOf` p -> openEventlog (appTimeline app) p False >> writeVar (appTab app) PTimeline
    | ".hp" `isSuffixOf` p -> openHp (appHeap app) p >> writeVar (appTab app) PHeap
    | ".prof" `isSuffixOf` p -> openProf (appHeap app) p >> writeVar (appTab app) PHeap
    | Just _ <- kindForPath p -> openCodeFile (appCore app) p >> writeVar (appTab app) PCore
    | otherwise -> writeVar (appMessage app) ("Don't know how to open " <> T.pack p)

appView :: App -> NanoUI ()
appView app = do
  pending <- readVar (appPending app)
  unless (null pending) $ do
    writeVar (appPending app) []
    mapM_ (openAny app) pending
    for_ (appStartTab app) (writeVar (appTab app))
  follow <- readVar (appFollow app)
  for_ follow $ \p -> do
    writeVar (appFollow app) Nothing
    openEventlog (appTimeline app) p True
  startDebug <- readVar (appStartDebug app)
  when startDebug $ do
    writeVar (appStartDebug app) False
    wake <- askWake
    liftIO (startSession (appDebug app) wake)
  inp <- askInput
  forM_ (inputDrops inp) $ \d ->
    when (dropEventType d == DropFile) (openAny app (T.unpack (dropEventData d)))
  useFileDialog (appOpen app) (mapM_ (openAny app))

  -- Loaders run every frame so background work survives tab switches.
  timeline <- timelineLoad (appTimeline app)
  heap <- heapLoad (appHeap app)
  core <- coreLoad (appCore app)
  debugSrc <- debugLoad (appDebug app)
  live <- liveLoad (appLive app)
  let logProfile = timeline >>= lpTimeline >>= tlHeapProfile

  columnWith (tight . grow . gap 0) $ do
    rowWith (gap 10 . alignMid . fillW . padXY 12 8) $ do
      labelWith (fontSizeScale 1.25 . fontSemiBold) "Stronghead"
      labelWith (fontMuted . fontSizeScale 0.9) "GHC runtime debugger & profiler"
      whenM (button "Open…") $
        writeVar (appOpen app) . Just
          =<< askOpenFileDialog
            defaultFileDialogOptions
              { dialogAllowMany = True
              , dialogFilters =
                  [ FileFilter "Stronghead inputs" "eventlog;hp;prof;dump-simpl;dump-stg-final;dump-stg;hs"
                  , FileFilter "All files" "*"
                  ]
              }
      msg <- readVar (appMessage app)
      scope $ unless (T.null msg) $ do
        labelWith (fontTone Warning) msg
        whenM (buttonWith tight "×") (writeVar (appMessage app) "")
      flex
      labelWith (fontMuted . fontSizeScale 0.85) "Drop .eventlog / .hp / .prof / dump files anywhere"
    -- Header only: 'tabs' would put the body in a column that does not
    -- stretch vertically, and every panel wants the full height.
    boundedTabBar panelTitle (appTab app)
    active <- readVar (appTab app)
    columnWith (tight . grow) . withKey (fromEnum active) $ case active of
      PTimeline -> timelineView (appTimeline app) timeline
      PHeap -> heapView (appHeap app) heap logProfile
      PCore -> coreView (appCore app) core
      PDebugger -> debugView (appDebug app) debugSrc
      PLive -> liveView (appLive app) live $ \p -> do
        openEventlog (appTimeline app) p True
        writeVar (appTab app) PTimeline
