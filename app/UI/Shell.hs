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
import Data.Maybe (isNothing)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)
import Stronghead.Dump (kindForPath)
import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Eventlog (LoadProgress (..))
import Stronghead.Format (fmtBytes, tshow)
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName)
import UI.Common
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
  PCore -> "Core & STG"
  PDebugger -> "Debugger"
  PLive -> "Live"

-- | How to drive the panel, for the status bar.
panelHint :: Panel -> T.Text
panelHint = \case
  PTimeline -> "Wheel to zoom, drag to pan, double-click to fit"
  PHeap -> "Drag across the chart to move the census cursor"
  PCore -> "Hover a name to find it in every pane, click it to jump to its binding"
  PDebugger -> "Click the gutter to set or clear a breakpoint"
  PLive -> "Samples arrive four times a second"

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

  th <- uiTheme
  let i = ink th
  active <- readVar (appTab app)
  columnWith (tight . grow . gap 0) $ do
    styled (panelStyle (background (inkGround i) . borderWidth 0 . cornerRadius 0)) $
      panelWith (asRow . fillW . fixedH 46 . padLeft 20 . padRight 8 . padTop 0 . padBottom 0 . gap 18) $ do
        labelWith (alignMid . fontSemiBold . fontSizeScale 1.08) "Stronghead"
        themed th $ navBar (appTab app)
        flex
        themed th $ document app active
        themed th $ whenM (toolButton "Open…") $
          writeVar (appOpen app) . Just
            =<< askOpenFileDialog
              defaultFileDialogOptions
                { dialogAllowMany = True
                , dialogFilters =
                    [ FileFilter "Stronghead inputs" "eventlog;hp;prof;dump-simpl;dump-stg-final;dump-stg;hs"
                    , FileFilter "All files" "*"
                    ]
                }
    columnWith (tight . grow . gap 0) . withKey (fromEnum active) $ case active of
      PTimeline -> timelineView (appTimeline app) timeline
      PHeap -> heapView (appHeap app) heap logProfile
      PCore -> coreView (appCore app) core
      PDebugger -> debugView (appDebug app) debugSrc
      PLive -> liveView (appLive app) live $ \p -> do
        openEventlog (appTimeline app) p True
        writeVar (appTab app) PTimeline
    statusBar app active timeline live

-- | The file the active panel is showing, in the title bar.
document :: App -> Panel -> NanoUI ()
document app active = scope $ case active of
  PTimeline -> pathLabel =<< readVar (tsPath (appTimeline app))
  PHeap -> do
    source <- readVar (hsSource (appHeap app))
    case source of
      FromHpFile -> pathLabel =<< readVar (hsHpPath (appHeap app))
      FromEventlog -> pathLabel =<< readVar (tsPath (appTimeline app))
      FromGhcDebug -> labelWith (alignMid . fontMuted . fontSizeScale 0.92) "ghc-debug live graph"
  PCore -> do
    (s, c, g) <- readVar (csPaths (appCore app))
    pathLabel (c `orElse` g `orElse` s)
  PDebugger -> pathLabel =<< readVar (dsSourcePath (appDebug app))
  PLive -> do
    host <- readVar (lvHost (appLive app))
    port <- readVar (lvPort (appLive app))
    labelWith (alignMid . fontMono . fontSizeScale 0.92) (host <> ":" <> port)
  where
    orElse a b = maybe b Just a

-- | Background work from every panel on the left, how to drive the active
-- panel on the right.
statusBar :: App -> Panel -> Maybe LoadProgress -> Maybe LiveData -> NanoUI ()
statusBar app active timeline live = do
  th <- uiTheme
  let i = ink th
      item = labelWith (alignMid . fontSizeScale 0.85)
  busy <- readVar (dsBusy (appDebug app))
  ghci <- readVar (dsRunning (appDebug app))
  msg <- readVar (appMessage app)
  styled (panelStyle (background (inkGround i) . borderTop 1 (inkRule i) . cornerRadius 0)) $
    panelWith (asRow . fillW . fixedH 28 . padXY 14 0 . gap 14) $ themed th $ do
      scope $ for_ timeline $ \p -> do
        unless (lpDone p) $ do
          let frac = if lpTotal p > 0 then fromIntegral (lpBytes p) / fromIntegral (lpTotal p) else 0 :: Double
          progressBarWith (fixedW 120 . alignMid) 6 (realToFrac frac)
          item ("Reading eventlog, " <> fmtBytes (fromIntegral (lpBytes p)))
        when (lpFollowing p && lpDone p) $ item "Following the eventlog as it is written"
        for_ (lpError p) $ \e -> labelWith (alignMid . fontSizeScale 0.85 . fontTone Danger) ("Eventlog decoding stopped: " <> T.take 100 e)
      scope $ for_ live $ \d -> do
        for_ (ldHello d) $ \(p, pid, _) -> when (isNothing (ldClosed d)) $ item ("Live: " <> p <> ", pid " <> tshow pid)
        for_ (ldClosed d) $ \why -> labelWith (alignMid . fontSizeScale 0.85 . fontTone Danger) ("Agent disconnected: " <> T.take 80 why)
      scope $ when (ghci || busy) $ item (if busy then "GHCi is working…" else "GHCi session running")
      scope $ unless (T.null msg) $ do
        labelWith (alignMid . fontSizeScale 0.85 . fontTone Warning) msg
        whenM (buttonWith (alignMid . padXY 6 0 . tight) "Dismiss") (writeVar (appMessage app) "")
      flex
      labelWith (alignMid . fontMuted . fontSizeScale 0.85) (panelHint active)

-- | The panels, as folder tabs along the bottom of the title bar.
navBar :: Var Panel -> NanoUI ()
navBar v =
  rowWith (alignBottom . tight) $
    boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabContained} panelTitle v
