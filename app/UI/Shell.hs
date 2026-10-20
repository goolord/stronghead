-- | The window: a header with the panel tabs, history, the palette, a
-- global Open and Settings, file drag-and-drop, and the per-frame loaders
-- that keep background work going whichever panel is showing. The panels'
-- lenses are built here each frame from what the loaders returned, so every
-- panel can link to every other ("UI.Link").
--
-- Every panel's workbench can hold any pane ("UI.Workspace"): the header's
-- Panes button adds panes and applies arrangements, and each pane's menu
-- moves it or pops it out into a floating window. The shell draws a pane
-- from what its panel's loader returned this frame, whichever workbench it
-- is in, and draws its own two panes, the inspector ("UI.Inspector") and
-- the pinboard ("UI.Pinboard").
--
-- The header's Sources button lists every run's artifacts and connections
-- ("UI.Sources"); the status bar is the connection centre, where each live
-- source opens its panel or its menu. Connect in Live attaches: what it
-- finds is opened here ('applyAttach').
--
-- The shell also keeps the investigation's aids: what is loaded, for the
-- inspector's next steps ('Have'); the watchers on the live streams
-- ("UI.Watchers"); and the baseline run with its diffs ("UI.Baseline").
module UI.Shell
  ( App
  , Options (..)
  , defaultOptions
  , newApp
  , appView
  , saveAppSettings
  , saveLastSession
  ) where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Char (intToDigit)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf, nub)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Sequence qualified as Seq
import Data.Word (Word64)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, askSaveFileDialog, askSdlEnv, defaultFileDialogOptions, setSdlUiScale, useFileDialog, windowZoom)
import NanoUI.Shortcut qualified as K
import Stronghead.Attach (AttachKind (..), AttachStep (..))
import Stronghead.Debug (DebugTarget (..), ghcDebugEnabled)
import Stronghead.Diff (Baseline (..), BaselinePaths (..), noBaselinePaths)
import Stronghead.Dump (ModuleArtifacts (..), kindForPath)
import Stronghead.Eventlog.Model (Timeline (..), summarize)
import Stronghead.Eventlog (LoadProgress (..))
import Stronghead.Format (fmtBytes, fmtNs, tshow)
import Stronghead.HeapProfile (HeapProfile (..))
import Stronghead.Live (AgentInfo (..), Frame (..))
import Stronghead.Next (Have (..))
import Stronghead.Prof (ProfFile (..))
import Stronghead.Ref (Ref (..), RunKey, parseRefTarget)
import Stronghead.Run (RunInfo (..), emptyRunInfo, mergeRunInfo, runFromArgs, runFromHello, runFromProfHeader, runFromSocket, runKey)
import Stronghead.Session (Session (..), decodeSession, encodeSession, sessionExtension)
import Control.Exception (IOException, try)
import Data.Text.IO qualified as T
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, makeAbsolute, renameFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import UI.Baseline
import UI.Clock (confirmMoments)
import UI.Code (colouringName)
import UI.Common
import UI.Core
import UI.Debugger
import UI.Heap
import UI.Inspector
import UI.Link
import UI.Live
import UI.Palette
import UI.Pinboard
import UI.Settings
import UI.Sources
import UI.Timeline
import UI.Watchers
import UI.Workspace

-- | How to drive the panel, for the status bar.
panelHint :: Panel -> T.Text
panelHint = \case
  PTimeline -> "Wheel to zoom, drag to pan, click or drag the ruler for the cursor, Shift+drag for a range, [ ] step collections"
  PHeap -> "Drag the chart to move the cursor, wheel to zoom, drag the time axis to pan, Shift+drag for a range, [ ] step censuses"
  PCore -> "Hover a name to find it in every pane, click it to jump to its binding; click a bar or a note to go to what it measures"
  PDebugger -> "Click the gutter to set or clear a breakpoint"
  PLive -> "Click or drag a chart to move the cursor, Shift+drag for a range; samples arrive four times a second"

tabNamed :: T.Text -> Panel
tabNamed = fromMaybe PTimeline . panelNamed

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
  , appSettingsOpen :: !(Var Bool)
  , appTheme :: !(Var ThemeChoice)
  , appScale :: !(Var UiScale)
  , appRestoreWindow :: !(Var Bool)
  , appStartPanel :: !(Var (Maybe Panel))
    -- ^ 'Nothing': the panel in use when the program last quit.
  , appPlaceWindow :: !(Var (Maybe SavedWindow))
    -- ^ Geometry to put the window back to on the first frame.
  , appWindowSeen :: !(IORef (Maybe SavedWindow))
    -- ^ The window's latest restored geometry, saved on quit.
  , appSaved :: !(IORef Settings)
    -- ^ What the settings file holds, so it is written only on a change.
  , appBus :: !Bus
  , appPalette :: !CommandPalette
  , appGoto :: !(Var (Maybe Ref))
    -- ^ From the command line: opened once a panel can show it.
  , appMark :: !(Var (Maybe T.Text))
    -- ^ From the command line: a mark to send once the agent takes
    -- commands.
  , appAttachRoots :: !(Var [FilePath])
    -- ^ Where the connected program runs, searched for sources.
  , appSourcesOpen :: !(Var Bool)
  , appRunLinks :: !(Var (M.Map RunKey RunInfo))
    -- ^ Runs joined by hand in the data sources popover: a source that
    -- named the key is treated as from the run.
  , appInspector :: !Inspector
  , appPopped :: !(Var [Popped])
    -- ^ Panes in floating windows.
  , appPresets :: !(Var [Preset])
    -- ^ The user's arrangements, saved with sessions.
  , appPanesOpen :: !(Var Bool)
  , appPresetName :: !(Var T.Text)
  , appExport :: !(Var (Maybe FileDialogId))
    -- ^ Where to save the pinboard's report.
  , appSessionSave :: !(Var (Maybe FileDialogId))
  , appSessionOpen :: !(Var (Maybe FileDialogId))
  , appSessionPath :: !(Var (Maybe FilePath))
    -- ^ The session file last saved or opened.
  , appPendingSession :: !(Var (Maybe Session))
    -- ^ The last session, reopened on the first frame.
  , appPendingModule :: !(Var (Maybe T.Text))
    -- ^ A session's module, picked once its dump folder is searched.
  , appReopenLast :: !(Var Bool)
  , appWatchers :: !Watchers
  , appBaseline :: !BaselineState
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
  , optGoto :: !(Maybe T.Text)
    -- ^ An entity to open, by its reference.
  , optMark :: !(Maybe T.Text)
    -- ^ A mark to send the connected program, once it can take one.
  }

defaultOptions :: Options
defaultOptions = Options [] Nothing Nothing Nothing Nothing Nothing Nothing

-- | The app, starting from saved settings; the command line wins over them.
newApp :: Settings -> Options -> IO App
newApp settings Options {..} = do
  let startPanel = cfgStartPanel settings >>= panelNamed
      firstPanel
        | Just _ <- optDebug = PDebugger
        | Just _ <- optConnect = PLive
        | otherwise = fromMaybe (tabNamed (cfgLastPanel settings)) startPanel
  appTab <- newVar (maybe firstPanel tabNamed optTab)
  appTimeline <- newTimelineState
  appHeap <- newHeapState
  appCore <- newCoreState
  writeVarIO (csColouring appCore) (cfgColouring settings)
  writeVarIO (csHideUniques appCore) (cfgHideUniques settings)
  writeVarIO (csHideTypeable appCore) (cfgHideTypeable settings)
  writeVarIO (csProjectRoot appCore) (cfgProjectRoot settings)
  writeVarIO (csEditor appCore) (cfgEditorCommand settings)
  appDebug <- newDebugState
  writeVarIO (dsCommand appDebug) (cfgGhciCommand settings)
  appStartDebug <- newVar False
  for_ optDebug $ \path -> do
    dir <- makeAbsolute (takeDirectory path)
    writeVarIO (dsDir appDebug) (T.pack dir)
    writeVarIO (dsFile appDebug) (T.pack (takeFileName path))
    writeVarIO appStartDebug True
  appLive <- newLiveState
  writeVarIO (lvHost appLive) (cfgAgentHost settings)
  writeVarIO (lvPort appLive) (cfgAgentPort settings)
  for_ optConnect $ \(host, port) -> do
    writeVarIO (lvHost appLive) host
    writeVarIO (lvPort appLive) port
    writeVarIO (lvWant appLive) True
  appOpen <- newVar Nothing
  appPending <- newVar optFiles
  appFollow <- newVar optFollow
  appMessage <- newVar ""
  let appStartTab = tabNamed <$> optTab
  appSettingsOpen <- newVar False
  appTheme <- newVar (cfgTheme settings)
  appScale <- newVar (cfgScale settings)
  appRestoreWindow <- newVar (cfgRestoreWindow settings)
  appStartPanel <- newVar startPanel
  appPlaceWindow <- newVar (if cfgRestoreWindow settings then cfgWindow settings else Nothing)
  appWindowSeen <- newIORef (cfgWindow settings)
  appSaved <- newIORef settings
  appBus <- newBus
  appPalette <- newPalette
  appGoto <- newVar (optGoto >>= parseRefTarget)
  appMark <- newVar optMark
  appAttachRoots <- newVar []
  appSourcesOpen <- newVar False
  appRunLinks <- newVar M.empty
  appInspector <- newInspector
  appPopped <- newVar []
  appPresets <- newVar userPresets
  appPanesOpen <- newVar False
  appPresetName <- newVar ""
  appExport <- newVar Nothing
  appSessionSave <- newVar Nothing
  appSessionOpen <- newVar Nothing
  appSessionPath <- newVar Nothing
  appPendingModule <- newVar Nothing
  appReopenLast <- newVar (cfgReopenSession settings)
  appWatchers <- newWatchers
  appBaseline <- newBaselineState
  -- The last session comes back only when the command line asks for
  -- nothing of its own.
  let asked = not (null optFiles) || isJust optConnect || isJust optFollow || isJust optDebug
  lastSession <-
    if cfgReopenSession settings && not asked
      then do
        path <- lastSessionPath
        exists <- doesFileExist path
        if exists then either (const Nothing) Just <$> readSession path else pure Nothing
      else pure Nothing
  appPendingSession <- newVar lastSession
  pure App {..}

-- | The settings as they stand: the dialog's, the panels' defaults as last
-- set, and the window's geometry.
currentSettings :: App -> IO Settings
currentSettings app = do
  cfgTheme <- readVarIO (appTheme app)
  cfgScale <- readVarIO (appScale app)
  cfgRestoreWindow <- readVarIO (appRestoreWindow app)
  cfgWindow <- readIORef (appWindowSeen app)
  cfgStartPanel <- fmap panelKey <$> readVarIO (appStartPanel app)
  cfgLastPanel <- panelKey <$> readVarIO (appTab app)
  cfgAgentHost <- readVarIO (lvHost (appLive app))
  cfgAgentPort <- readVarIO (lvPort (appLive app))
  cfgGhciCommand <- readVarIO (dsCommand (appDebug app))
  cfgColouring <- readVarIO (csColouring (appCore app))
  cfgHideUniques <- readVarIO (csHideUniques (appCore app))
  cfgHideTypeable <- readVarIO (csHideTypeable (appCore app))
  cfgProjectRoot <- readVarIO (csProjectRoot (appCore app))
  cfgEditorCommand <- readVarIO (csEditor (appCore app))
  cfgReopenSession <- readVarIO (appReopenLast app)
  pure Settings {..}

-- | Save the settings with the window's final geometry; call once the
-- window has closed.
saveAppSettings :: App -> IO (Either T.Text ())
saveAppSettings app = saveSettings =<< currentSettings app

-- | Save whenever a setting changes. The window's geometry changes on every
-- frame of a drag, so it alone waits for 'saveAppSettings' on quit.
persistSettings :: App -> NanoUI ()
persistSettings app = do
  now <- liftIO (currentSettings app)
  saved <- liftIO (readIORef (appSaved app))
  when (now {cfgWindow = Nothing} /= saved {cfgWindow = Nothing}) $ do
    liftIO (writeIORef (appSaved app) now)
    liftIO (saveSettings now) >>= either (writeVar (appMessage app)) pure

-- | Note the window's restored geometry, and put it back on the first frame.
trackWindow :: App -> NanoUI ()
trackWindow app = do
  place <- readVar (appPlaceWindow app)
  for_ place $ \sw -> do
    writeVar (appPlaceWindow app) Nothing
    -- Opening at a zoom other than 1 centres the window, so move it after.
    for_ (swPosition sw) (uncurry moveWindowUi)
    when (swMaximized sw) maximizeWindowUi
  w <- askWindow
  let Size ww wh = winSize w
  liftIO $ unless (winMinimized w || winFullscreen w || ww <= 0 || wh <= 0) $
    modifyIORef' (appWindowSeen app) $ \prev ->
      if winMaximized w
        then Just (maybe (SavedWindow (winSize w) (winPosition w) True) (\p -> p {swMaximized = True}) prev)
        else Just (SavedWindow (winSize w) (winPosition w) False)

-- | The zoom the window is running at, for showing and stepping from
-- 'ScaleAuto'.
currentZoom :: NanoUI Float
currentZoom = askSdlEnv >>= maybe (pure 1) (liftIO . windowZoom)

-- | Route a path to the panel that reads it.
openAny :: App -> FilePath -> NanoUI ()
openAny app p = do
  isDir <- liftIO (doesDirectoryExist p)
  if
    | isDir -> openDumpDir (appCore app) p >> writeVar (appTab app) PCore
    | ".eventlog" `isSuffixOf` p -> openEventlog (appTimeline app) p False >> writeVar (appTab app) PTimeline
    | ".hp" `isSuffixOf` p -> openHp (appHeap app) p >> writeVar (appTab app) PHeap
    | ".prof" `isSuffixOf` p -> openProf (appHeap app) p >> writeVar (appTab app) PHeap
    | sessionExtension `isSuffixOf` p -> openSession app p
    | Just _ <- kindForPath p -> openCodeFile (appCore app) p >> writeVar (appTab app) PCore
    | otherwise -> writeVar (appMessage app) ("Don't know how to open " <> T.pack p)

appView :: App -> NanoUI ()
appView app = do
  -- Settings first, so the whole frame is drawn in the chosen theme.
  choice <- readVar (appTheme app)
  setUiTheme . strongheadTheme . choiceAppearance choice =<< systemAppearance
  setSdlUiScale . uiScaleFactor =<< readVar (appScale app)
  trackWindow app
  settingsShortcuts app
  reopen <- readVar (appPendingSession app)
  for_ reopen $ \s -> writeVar (appPendingSession app) Nothing >> applySession app s
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
  useFileDialog (appSessionSave app) $ \case
    p : _ -> saveSessionAs app p
    [] -> pure ()
  useFileDialog (appSessionOpen app) $ \case
    p : _ -> openSession app p
    [] -> pure ()

  -- Loaders run every frame so background work survives tab switches.
  timeline0 <- timelineLoad (appTimeline app)
  heap0 <- heapLoad (appHeap app)
  debugDir <- T.unpack <$> readVar (dsDir (appDebug app))
  attachRoots <- readVar (appAttachRoots app)
  core <- coreLoad (appCore app) (debugDir : attachRoots)
  debugSrc <- debugLoad (appDebug app)
  -- A session's module, once the dump folder has been searched.
  pendingModule <- readVar (appPendingModule app)
  for_ pendingModule $ \m -> for_ (clArtifacts core) $ \arts -> do
    writeVar (appPendingModule app) Nothing
    for_ (lookup m (zip (map maModule arts) [0 ..])) (writeVar (csModule (appCore app)))
  live0 <- liveLoad (appLive app)
  -- Runs joined by hand are joined here, for every panel at once.
  links <- readVar (appRunLinks app)
  let timeline = fmap (linkTimeline links) timeline0
      heap = linkHeap links heap0
      live = fmap (linkLive links) live0
      logTl = timeline >>= lpTimeline
      logProfile = logTl >>= tlHeapProfile
      bus = appBus app
  timelineSync bus (appTimeline app) timeline
  heapSync bus (appHeap app) heap logTl (maybe False lpFollowing timeline)
  -- One Connect wires the program: its eventlog, heap profile, ghc-debug
  -- socket, sources and dumps.
  attach <- liveAttach (appLive app) live0
  for_ attach (applyAttach app)
  confirmMoments bus logTl
  hooks <- debugHooks app heap
  liveTick bus (appLive app) live logTl hooks
  remote <- liveRemote bus (appLive app) live logTl hooks
  moments <- readVar (busMoments bus)
  pendingMark <- readVar (appMark app)
  -- Sent once the charts have two seconds of frames, so it lands on them.
  for_ pendingMark $ \txt -> when (isNothing (commandsWhy live) && maybe False ((>= 8) . Seq.length . ldFrames) live) $ do
    writeVar (appMark app) Nothing
    sendMark bus (appLive app) live txt

  -- The baseline, and the bindings of the dump folder on show.
  dumpRoot <- readVar (csRoot (appCore app))
  baseline <- baselineLoad (appBaseline app) dumpRoot
  heapShown <- heapOnShow (appHeap app) heap logProfile
  snapshots <- snapshotCount (appHeap app)
  let isRight' = \case
        Just (Right _) -> True
        _ -> False
      have =
        Have
          { haveTimeline = isJust logTl
          , haveHeapCensus = maybe False (not . VU.null . hpTimes) heapShown
          , haveHeapSource = maybe False ((== "info table") . hpBreakdown) heapShown
          , haveCostCentres = isRight' (hlProf heap >>= taskResult)
          , haveSamples = maybe False (not . V.null . tlProfSamples) logTl
          , haveTicky = maybe False (not . V.null . tlTicky) logTl
          , haveDumps = any isRight' [clCore core, clStg core]
          , haveAgent = isNothing (commandsWhy live)
          , haveLiveGraph = either (const False) (const True) (dhSnapshot hooks)
          , haveSnapshots = snapshots
          }
      taskResult = \case
        TaskDone r -> Just r
        TaskRunning r -> r
        _ -> Nothing
  -- What marking the baseline now would keep.
  logPath <- readVar (tsPath (appTimeline app))
  heapSource <- readVar (hsSource (appHeap app))
  hpPath <- readVar (hsHpPath (appHeap app))
  profPath <- readVar (hsProfPath (appHeap app))
  costTotalsNow <- readVar (hsCostTotals (appHeap app))
  let costsNow = case (profPath, costTotalsNow) of
        (Just p, Just (p', cts)) | p == p' -> Just cts
        _ -> Nothing
      basePaths =
        BaselinePaths
          { blEventlog = if isJust logTl then logPath else Nothing
          , blHeapProfile = if heapSource == FromHpFile && isJust heapShown then hpPath else Nothing
          , blCostCentres = if isJust costsNow then profPath else Nothing
          , blDumps = dumpRoot
          }
      markNow
        | basePaths == noBaselinePaths = Left "nothing to keep yet: open an eventlog, a heap or cost-centre profile, or a dump folder"
        | otherwise = Right (markBaseline (appBaseline app) (Baseline basePaths (summarize <$> logTl) heapShown costsNow (boCurBindings baseline)))
  -- Watchers act through the agent and the ghc-debug target, as Live does.
  let remoteRun lbl = case [raRun a | a <- remote, raLabel a == lbl] of
        r : _ -> r
        [] -> Left "not available"
      watchHooks =
        WatchHooks
          { whMark = maybe (Right (sendMark bus (appLive app) live)) Left (commandsWhy live)
          , whCensus = remoteRun "Census now"
          , whSnapshot = remoteRun "Snapshot"
          }
      following = maybe False lpFollowing timeline
  watchTick (appWatchers app) watchHooks live logTl following

  active <- readVar (appTab app)
  heapL <- heapLens (appHeap app) heap logProfile
  -- Each lens may ask the ones built before it where an entity is in the
  -- source: Core opens it in an editor, the Debugger breaks on it.
  coreL <- coreLens (appCore app) core (lensSpan heapL) (lensAnnotations heapL)
  debugL <- debugLens (appDebug app) debugSrc (clIndex core) (\r -> maybe (lensSpan coreL r) Just (lensSpan heapL r))
  let env =
        Env
          { envBus = appBus app
          , envLenses = [timelineLens (appBus app) (writeVar (appTab app) PTimeline) (appTimeline app) timeline, heapL, coreL, debugL, liveLens (appLive app) live moments remote]
          , envPanel = active
          , envSwitch = writeVar (appTab app)
          , envHave = have
          , envBaseline = boBaseline baseline
          }
  navShortcuts app env
  inspectorSync env (appInspector app)
  useFileDialog (appExport app) $ \case
    p : _ -> exportPins app env p
    [] -> pure ()
  -- Every provider of panes, for whichever workbench holds them.
  let providers p site = case p of
        PTimeline -> timelinePanes env (appTimeline app) timeline site
        PHeap -> heapPanes env (appHeap app) heap logTl site
        PCore -> corePanes env (appCore app) core site
        PDebugger -> debugPanes env (appDebug app) (clIndex core) debugSrc site
        PLive -> livePanes env (appLive app) live logTl site
      watching = case (live, following) of
        (Just d, _) | Just (p, _, _) <- ldHello d, isNothing (ldClosed d) -> "watching " <> p <> (if following then " and its eventlog" else "")
        (_, True) -> "watching the followed eventlog"
        _ -> "no live stream: connect in Live, or follow an eventlog"
      diffNow = DiffNow heapShown costsNow markNow
      shellPanes pid ctx
        | pid == paneInspector = Just (inspectorPane env (appInspector app) ctx)
        | pid == panePinboard = Just (pinboardPane env (askExport app) ctx)
        | pid == paneWatchers = Just (watchersPane env (appWatchers app) watching ctx)
        | otherwise = diffPanes env (appBaseline app) baseline diffNow pid ctx
      foreignKinds tree = [k | k <- maybe [] treeKinds tree, Just pk <- [paneKind k], maybe False (/= active) (pkPanel pk)]
      bench =
        Bench
          { bnDraw = \wb own -> do
              tree <- readVar (wbTree wb)
              let kinds = maybe [] treeKinds tree
                  others = nub [q | k <- foreignKinds tree, Just pk <- [paneKind k], Just q <- [pkPanel pk]]
              ownDraw <- own kinds
              theirs <- mapM (`providers` kinds) others
              workspace (benchPlaces app) (appPopped app) wb (foldr orPanes shellPanes (ownDraw : theirs))
          , bnMixed = \wb -> not . null . foreignKinds <$> readVar (wbTree wb)
          }
  -- M marks the program's timeline from any panel.
  whenM (shortcut (K.key 'm')) $ case commandsWhy live of
    Nothing -> openMarkPrompt (appLive app)
    Just why -> when (isJust live) (writeVar (appMessage app) ("Mark: " <> why))
  goto <- readVar (appGoto app)
  for_ goto $ \r -> when (any (`lensShows` r) (envLenses env)) $ do
    writeVar (appGoto app) Nothing
    goTo env r

  th <- uiTheme
  let i = ink th
  columnWith (tight . grow . gap 0) $ do
    styled (panelStyle (background (inkGround i) . borderWidth 0 . cornerRadius 0)) $
      panelWith (asRow . fillW . fixedH 46 . padLeft 20 . padRight 8 . padTop 0 . padBottom 0 . gap 18) $ do
        labelWith (alignMid . fontSemiBold . fontSizeScale 1.08) "Stronghead"
        themed th $ history env
        themed th $ navBar (appTab app)
        flex
        -- The file on show, when there is room beside the buttons.
        winW <- windowWidth
        scope $ when (winW >= 1640) (themed th (document app active))
        themed th $ do
          resp <- buttonWith' alignMid "Sources"
          tooltip resp "What each run has, and how to add the rest"
          when (respClicked resp) (modifyVar (appSourcesOpen app) not)
          sources <- sourcesOf app timeline0 heap0 core live0 links
          sourcesPopover env (appSourcesOpen app) (respRect resp) sources (linkRuns app) (unlinkRun app)
        themed th $ do
          resp <- buttonWith' alignMid "Workspace"
          tooltip resp "Add a pane to this panel, use another arrangement, save or open the session"
          when (respClicked resp) (modifyVar (appPanesOpen app) not)
          panesPopover app active (respRect resp)
        themed th $ whenM (toolButton "Go to…") (openPalette (appPalette app))
        themed th $ whenM (toolButton "Settings") (writeVar (appSettingsOpen app) True)
        themed th $ whenM (toolButton "Open…") (askOpen app)
    columnWith (tight . grow . gap 0) . withKey (fromEnum active) $ case active of
      PTimeline -> timelineView env bench (appTimeline app) timeline
      PHeap -> heapView env bench (appHeap app) heap logTl
      PCore -> coreView env bench (appCore app) core
      PDebugger -> debugView env bench (appDebug app) (clIndex core) debugSrc
      PLive -> liveView env bench (appLive app) live logTl remote $ \p -> do
        openEventlog (appTimeline app) p True
        writeVar (appTab app) PTimeline
    statusBar app env active timeline live heap hooks baseline
  settingsModal app
  markPrompt bus (appLive app) live
  paletteView env (appPalette app) (commands app env active markNow)
  poppedWindows app shellPanes providers
  persistSettings app
  publishHover (appBus app)

askOpen :: App -> NanoUI ()
askOpen app =
  writeVar (appOpen app) . Just
    =<< askOpenFileDialog
      defaultFileDialogOptions
        { dialogAllowMany = True
        , dialogFilters =
            [ FileFilter "Stronghead inputs" "eventlog;hp;prof;dump-simpl;dump-stg-final;dump-stg;hs;stronghead"
            , FileFilter "All files" "*"
            ]
        }

-- | Back and forward through the places links went to, each with a tooltip
-- naming where it leads.
history :: Env -> NanoUI ()
history env = rowWith (alignMid . tight . gap 2) $ do
  let bus = envBus env
      step label locs go = do
        resp <- disabledWhen (null locs) (buttonWith' (alignMid . padXY 8 2 . tight) label)
        for_ (take 1 locs) $ \loc -> tooltip resp (describe loc)
        when (respClicked resp) (go env)
      describe (Location p r) = panelTitle p <> maybe "" ((": " <>) . refTitle env) r
  back <- readVar (busBack bus)
  forward <- readVar (busForward bus)
  step "←" back goBack
  step "→" forward goForward

-- | Window-wide navigation: the palette, history, the panels by number,
-- and Esc to drop the focus.
navShortcuts :: App -> Env -> NanoUI ()
navShortcuts app env = do
  let pressed = fmap or . mapM shortcut
      digit n = K.cmdOrCtrl <> K.key (intToDigit n)
  whenM (pressed [K.cmdOrCtrl <> K.key 'k']) (openPalette (appPalette app))
  inp <- askInput
  back <- pressed [K.alt <> K.key KeyLeft]
  forward <- pressed [K.alt <> K.key KeyRight]
  when (back || pressedIn MouseBack inp) (goBack env)
  when (forward || pressedIn MouseForward inp) (goForward env)
  forM_ (zip [1 :: Int ..] [minBound .. maxBound]) $ \(n, p) ->
    whenM (pressed [digit n]) (writeVar (appTab app) p)
  whenM (pressed [K.key KeyEscape]) (setFocus env Nothing)

-- | The palette's own commands, ahead of what the panels have loaded.
commands :: App -> Env -> Panel -> Either T.Text (NanoUI ()) -> [Hit]
commands app env active markNow =
  [Hit "panel" (panelTitle p) ("Ctrl+" <> tshow n) (HitRun (writeVar (appTab app) p)) | (n, p) <- zip [1 :: Int ..] [minBound .. maxBound]]
    ++ [ Hit "command" "Open file…" "" (HitRun (askOpen app))
       , Hit "command" "Settings" "Ctrl+," (HitRun (writeVar (appSettingsOpen app) True))
       , Hit "command" "Back" "Alt+Left" (HitRun (goBack env))
       , Hit "command" "Forward" "Alt+Right" (HitRun (goForward env))
       , Hit "command" "Clear the focus" "Esc" (HitRun (setFocus env Nothing))
       , Hit "command" "Export pins…" "Markdown" (HitRun (askExport app))
       , Hit "command" "Save session…" "" (HitRun (askSaveSession app))
       , Hit "command" "Open session…" "" (HitRun (askOpenSession app))
       , Hit "command" "Mark as baseline" (either id (const "keep this run to compare the next with") markNow) (HitRun (either (writeVar (appMessage app)) id markNow))
       , Hit "command" "Clear the baseline" "" (HitRun (clearBaseline (appBaseline app)))
       ]
    ++ [ Hit "pane" ("Add pane: " <> pkTitle k) (maybe "" panelTitle (pkPanel k)) (HitRun (activeBench app active >>= \wb -> modifyVar (wbTree wb) (Just . addPane (pkId k))))
       | k <- paneKinds
       ]
    ++ [Hit "arrangement" ("Arrange: " <> panelTitle p) "this panel" (HitRun (useArrangement app active (panelPreset p))) | p <- [minBound .. maxBound]]
    ++ [Hit "arrangement" ("Arrange: " <> prName pr) "this panel" (HitRun (useArrangement app active (prTree pr))) | pr <- userPresets]

-- ---------------------------------------------------------------------------
-- Workspaces

-- | Every workbench, with its name in menus.
benchPlaces :: App -> [BenchPlace]
benchPlaces app =
  [ BenchPlace "Timeline" (tsPanes (appTimeline app))
  , BenchPlace "Heap & leaks" (hsPanes (appHeap app))
  , BenchPlace "Heap & leaks, live graph" (hsDbgPanes (appHeap app))
  , BenchPlace "Core & STG" (csPanes (appCore app))
  , BenchPlace "Debugger" (dsPanes (appDebug app))
  , BenchPlace "Live" (lvPanes (appLive app))
  ]

-- | Every workbench, for sessions: the places, and the heap panel's
-- cost-centre tree on its own.
allBenches :: App -> [Workbench]
allBenches app = map bpBench (benchPlaces app) ++ [hsCCPanes (appHeap app)]

-- | The workbench the panel is showing.
activeBench :: App -> Panel -> NanoUI Workbench
activeBench app = \case
  PTimeline -> pure (tsPanes (appTimeline app))
  PHeap -> do
    source <- readVar (hsSource (appHeap app))
    pure (if source == FromGhcDebug then hsDbgPanes (appHeap app) else hsPanes (appHeap app))
  PCore -> pure (csPanes (appCore app))
  PDebugger -> pure (dsPanes (appDebug app))
  PLive -> pure (lvPanes (appLive app))

-- | Put an arrangement in the panel's workbench. Panes in floating windows
-- stay there.
useArrangement :: App -> Panel -> GridNode -> NanoUI ()
useArrangement app p tree = do
  wb <- activeBench app p
  out <- map poKind <$> readVar (appPopped app)
  writeVar (wbTree wb) (foldr (\k t -> t >>= removePane k) (Just tree) out)

-- | The header's Panes popover: add any pane to this panel's workbench,
-- or use another arrangement: a panel's own, or a preset across panels.
panesPopover :: App -> Panel -> Rect -> NanoUI ()
panesPopover app active anchor = do
  wb <- activeBench app active
  tree <- readVar (wbTree wb)
  out <- map poKind <$> readVar (appPopped app)
  presets <- readVar (appPresets app)
  let close = writeVar (appPanesOpen app) False
      groups = [(maybe "Investigation" panelTitle p, [k | k <- paneKinds, pkPanel k == p]) | p <- Nothing : map Just [minBound .. maxBound]]
  void . popupWith (appPanesOpen app) (defaultPopupConfig (AnchorRect anchor)) (fixedW 560 . maxH 680) $
    void . scrollArea fillW $
      columnWith (fillW . padAll 12 . tight . gap 6) $ do
        labelWith (fontMedium . fontSizeScale 1.05) ("Panes in " <> panelTitle active)
        hint "Add a pane beside the others, then drag its header to place it. Its ⋯ menu moves it to another panel or pops it out."
        rowWith (fillW . tight . gap 16) $
          forM_ (zip [0 :: Int ..] (chunk 3 groups)) $ \(ci, col) -> withKey ci $
            columnWith (fillW . tight . gap 2 . alignTop) $
              forM_ col $ \(title, ks) -> withKey title $ do
                labelWith (fontMuted . fontSizeScale 0.85 . padTop 6) title
                forM_ ks $ \k -> withKey (pkKey k) $
                  if
                    | hasPane (pkId k) tree -> menuItemDisabled (pkTitle k <> " (here)")
                    | pkId k `elem` out -> menuItemDisabled (pkTitle k <> " (floating)")
                    | otherwise -> whenM (menuItem (pkTitle k)) $ do
                        modifyVar (wbTree wb) (Just . addPane (pkId k))
                        close
        sectionTitle "Arrangements"
        rowWith (fillW . tight . gap 6 . wrap . lineGap 6) $ do
          forM_ [minBound .. maxBound] $ \p -> withKey (panelKey p) $
            whenM (buttonWith (padXY 8 2 . tight) (panelTitle p)) (useArrangement app active (panelPreset p) >> close)
        rowWith (fillW . tight . gap 6 . wrap . lineGap 6) $
          forM_ presets $ \pr -> withKey (prName pr) $ do
            whenM (buttonWith (padXY 8 2 . tight) (prName pr)) (useArrangement app active (prTree pr) >> close)
        rowWith (fillW . tight . gap 8 . padTop 4) $ do
          textInputConfigured defaultTextInputConfig {ticPlaceholder = "name for this arrangement", ticLayout = (alignMid . fillW) defaultLayout} (appPresetName app)
          name <- T.strip <$> readVar (appPresetName app)
          disabledWhen (T.null name || maybe True (const False) tree) $
            whenM (buttonWith (alignMid . padXY 8 2 . tight) "Save as a preset") $ for_ tree $ \t -> do
              modifyVar (appPresets app) (\ps -> filter ((/= name) . prName) ps ++ [Preset name t])
              writeVar (appPresetName app) ""
        hint "Presets are saved with the session."
        sectionTitle "Session"
        rowWith (fillW . tight . gap 6) $ do
          whenM (buttonWith (padXY 8 2 . tight) "Save session…") (askSaveSession app >> close)
          whenM (buttonWith (padXY 8 2 . tight) "Open session…") (askOpenSession app >> close)
        sessionPath <- readVar (appSessionPath app)
        hint (maybe "A session keeps what is open, the connections, the pins, the cursor, the focus and every arrangement." (\p -> "This session: " <> T.pack p) sessionPath)
  where
    chunk n xs = case splitAt (max 1 ((length xs + n - 1) `div` n)) xs of
      (a, []) -> [a]
      (a, rest) -> a : chunk (n - 1) rest

-- | Panes popped out of a workbench, each in a floating window. Closing
-- the window puts the pane back where it came from.
poppedWindows :: App -> PaneDraw -> (Panel -> [Word64] -> NanoUI PaneDraw) -> NanoUI ()
poppedWindows app shellPanes providers = do
  popped <- readVar (appPopped app)
  forM_ popped $ \po -> withKey (poKind po) $ do
    let k = poKind po
        always = zoomVar (const True) (\_ ps -> ps) (appPopped app)
    draw <- case paneKind k >>= pkPanel of
      Just p -> providers p [k]
      Nothing -> pure shellPanes
    (resp, _) <- window' always (paneTitle k) $
      columnWith (fixedWH 720 460 . tight) $
        maybe (hint "Nothing draws this pane.") id (draw k floatingPane)
    when (respClicked resp) $ do
      modifyVar (appPopped app) (filter ((/= k) . poKind))
      for_ [wb | wb <- allBenches app, wbName wb == poFrom po] $ \wb -> modifyVar (wbTree wb) (Just . addPane k)

askExport :: App -> NanoUI ()
askExport app =
  writeVar (appExport app) . Just
    =<< askSaveFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "Markdown" "md", FileFilter "All files" "*"]}

-- | Write the pinboard's report, replacing the file only once the new one
-- is whole.
exportPins :: App -> Env -> FilePath -> NanoUI ()
exportPins app env path0 = do
  let path = if null (takeExtension path0) then path0 <> ".md" else path0
  pins <- readVar (busPins (appBus app))
  stamp <- liftIO (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" <$> getZonedTime)
  let report = pinsReport env ["Exported from Stronghead on " <> T.pack stamp <> "."] pins
  r <- liftIO . try $ do
    T.writeFile (path <> ".tmp") report
    renameFile (path <> ".tmp") path
  writeVar (appMessage app) $ case r of
    Left (e :: IOException) -> "Could not save the report: " <> T.pack (show e)
    Right () -> "Saved the pins to " <> T.pack (takeFileName path)

-- ---------------------------------------------------------------------------
-- Sessions

-- | Where the session is when the program quits, reopened on start when
-- Settings says so.
lastSessionPath :: IO FilePath
lastSessionPath = (</> ("last" <> sessionExtension)) . takeDirectory <$> settingsPath

-- | Where the investigation stands: what is open, the connections, the
-- pins, the clock, the focus, every workbench and the panel in front.
currentSession :: App -> IO Session
currentSession app = do
  let tl = appTimeline app
      hs = appHeap app
      cs = appCore app
      ds = appDebug app
      lv = appLive app
      bus = appBus app
  panel <- readVarIO (appTab app)
  eventlog <- readVarIO (tsPath tl)
  follow <- readVarIO (tsFollow tl)
  hp <- readVarIO (hsHpPath hs)
  prof <- readVarIO (hsProfPath hs)
  source <- readVarIO (hsSource hs)
  socket <- readVarIO (hsDbgSocket hs)
  root <- readVarIO (csRoot cs)
  fromFolder <- readVarIO (csLoadedModule cs)
  (s, c, g) <- readVarIO (csPaths cs)
  host <- readVarIO (lvHost lv)
  port <- readVarIO (lvPort lv)
  want <- readVarIO (lvWant lv)
  ghciCmd <- readVarIO (dsCommand ds)
  ghciDir <- readVarIO (dsDir ds)
  ghciFile <- readVarIO (dsFile ds)
  ghci <- readVarIO (dsRunning ds)
  pins <- readVarIO (busPins bus)
  cursor <- readVarIO (busCursor bus)
  range <- readVarIO (busRange bus)
  focus <- readVarIO (busFocus bus)
  layouts <- mapM (\wb -> (wbName wb,) . maybe "-" encodeTree <$> readVarIO (wbTree wb)) (allBenches app)
  presets <- readVarIO (appPresets app)
  popped <- readVarIO (appPopped app)
  watchers <- readVarIO (wrRules (appWatchers app))
  basePaths <- readVarIO (bvPaths (appBaseline app))
  pure
    Session
      { sesPanel = panelKey panel
      , sesEventlog = eventlog
      , sesFollow = follow
      , sesHeapProfile = hp
      , sesCostCentres = prof
      , sesHeapSource = case source of
          FromEventlog -> "eventlog"
          FromHpFile -> "hp"
          FromGhcDebug -> "ghc-debug"
      , sesGhcDebug = socket
      , sesDumps = root
      , sesModule = maybe "" maModule fromFolder
      , sesCodeFiles = if isJust fromFolder then [] else [p | Just p <- [s, c, g]]
      , sesAgentHost = host
      , sesAgentPort = port
      , sesAgentConnected = want
      , sesGhciCommand = ghciCmd
      , sesGhciDir = ghciDir
      , sesGhciFile = ghciFile
      , sesGhciRunning = ghci
      , sesPins = pins
      , sesCursor = cursor
      , sesRange = range
      , sesFocus = focus
      , sesLayouts = layouts
      , sesPresets = [(prName p, encodeTree (prTree p)) | p <- presets]
      , sesPopped = [maybe "" pkKey (paneKind (poKind p)) <> " " <> poFrom p | p <- popped]
      , sesWatchers = watchers
      , sesBaseline = basePaths
      }

-- | Write a session, replacing the file only once the new one is whole.
writeSession :: FilePath -> Session -> IO (Either T.Text ())
writeSession path s = do
  r <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    T.writeFile (path <> ".tmp") (encodeSession s)
    renameFile (path <> ".tmp") path
  pure $ case r of
    Left (e :: IOException) -> Left ("Could not save the session: " <> T.pack (show e))
    Right () -> Right ()

readSession :: FilePath -> IO (Either T.Text Session)
readSession path = do
  r <- try (T.readFile path)
  pure $ case r of
    Left (e :: IOException) -> Left ("Could not read the session: " <> T.pack (show e))
    Right txt -> decodeSession txt

-- | Save the session as it stands to where quitting keeps it. Call once the
-- window has closed.
saveLastSession :: App -> IO (Either T.Text ())
saveLastSession app = do
  path <- lastSessionPath
  writeSession path =<< currentSession app

-- | Open what a session had open, reconnect what it had connected, and put
-- back its pins, clock, focus and arrangements. A pane tree that doesn't
-- read is left as it was, and said to be.
applySession :: App -> Session -> NanoUI ()
applySession app Session {..} = do
  let tl = appTimeline app
      hs = appHeap app
      cs = appCore app
      ds = appDebug app
      lv = appLive app
      bus = appBus app
  case sesEventlog of
    Just p -> openEventlog tl p sesFollow
    Nothing -> writeVar (tsPath tl) Nothing
  writeVar (hsHpPath hs) sesHeapProfile
  writeVar (hsProfPath hs) sesCostCentres
  writeVar (hsSource hs) $ case sesHeapSource of
    "hp" -> FromHpFile
    "ghc-debug" -> FromGhcDebug
    _ -> FromEventlog
  writeVar (hsDbgSocket hs) sesGhcDebug
  case sesDumps of
    Just d -> do
      openDumpDir cs d
      writeVar (appPendingModule app) (if T.null sesModule then Nothing else Just sesModule)
    Nothing -> do
      writeVar (csRoot cs) Nothing
      writeVar (csLoadedModule cs) Nothing
      writeVar (csModule cs) (-1)
      writeVar (csPaths cs) (Nothing, Nothing, Nothing)
      mapM_ (openCodeFile cs) sesCodeFiles
  writeVar (lvHost lv) sesAgentHost
  writeVar (lvPort lv) sesAgentPort
  wanted <- readVar (lvWant lv)
  when (sesAgentConnected && not wanted) $ do
    writeVar (lvWant lv) True
    modifyVar (lvAttempt lv) (+ 1)
  when (not sesAgentConnected && wanted) (writeVar (lvWant lv) False)
  writeVar (dsCommand ds) sesGhciCommand
  writeVar (dsDir ds) sesGhciDir
  writeVar (dsFile ds) sesGhciFile
  running <- readVar (dsRunning ds)
  when (sesGhciRunning && not running) (writeVar (appStartDebug app) True)
  writeVar (busPins bus) sesPins
  writeVar (busCursor bus) sesCursor
  writeVar (busRange bus) sesRange
  writeVar (busFocus bus) sesFocus
  bad <- fmap concat . forM (allBenches app) $ \wb -> case lookup (wbName wb) sesLayouts of
    Nothing -> pure []
    Just "-" -> [] <$ writeVar (wbTree wb) Nothing
    Just t -> case decodeTree t of
      Right tree -> [] <$ writeVar (wbTree wb) (Just tree)
      Left e -> pure [wbName wb <> ": " <> e]
  let presets = [Preset n t | (n, src) <- sesPresets, Right t <- [decodeTree src]]
  unless (null sesPresets) (writeVar (appPresets app) presets)
  writeVar (appPopped app) [Popped (pkId k) (T.strip from) | p <- sesPopped, let (key, from) = T.breakOn " " p, Just k <- [paneKindNamed key]]
  for_ (panelNamed sesPanel) (writeVar (appTab app))
  writeVar (wrRules (appWatchers app)) sesWatchers
  if sesBaseline == noBaselinePaths
    then clearBaseline (appBaseline app)
    else writeVar (bvPaths (appBaseline app)) sesBaseline
  unless (null bad) $ writeVar (appMessage app) ("Some arrangements could not be read and were left as they were: " <> T.intercalate "; " bad)

-- | Open a session file.
openSession :: App -> FilePath -> NanoUI ()
openSession app path =
  liftIO (readSession path) >>= \case
    Left e -> writeVar (appMessage app) e
    Right s -> do
      applySession app s
      writeVar (appSessionPath app) (Just path)

askSaveSession :: App -> NanoUI ()
askSaveSession app =
  writeVar (appSessionSave app) . Just
    =<< askSaveFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "Stronghead session" (T.drop 1 (T.pack sessionExtension))]}

askOpenSession :: App -> NanoUI ()
askOpenSession app =
  writeVar (appSessionOpen app) . Just
    =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "Stronghead session" (T.drop 1 (T.pack sessionExtension)), FileFilter "All files" "*"]}

-- | Save the session where the user chose.
saveSessionAs :: App -> FilePath -> NanoUI ()
saveSessionAs app path0 = do
  let path = if null (takeExtension path0) then path0 <> sessionExtension else path0
  r <- liftIO (writeSession path =<< currentSession app)
  case r of
    Left e -> writeVar (appMessage app) e
    Right () -> do
      writeVar (appSessionPath app) (Just path)
      writeVar (appMessage app) ("Saved the session to " <> T.pack (takeFileName path))

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

-- | The connection centre on the left: every live source (the followed
-- eventlog, the agent, the ghc-debug target, GHCi) opens its panel on a
-- click and has a menu on a right-click. On the right, the cursor's time,
-- the focus, and how to drive the active panel.
statusBar :: App -> Env -> Panel -> Maybe LoadProgress -> Maybe LiveData -> HeapLoaded -> DebugHooks -> BaselineLoaded -> NanoUI ()
statusBar app env active timeline live heap hooks baseline = do
  th <- uiTheme
  let i = ink th
      tl = appTimeline app
      lv = appLive app
      hs = appHeap app
      dbg = appDebug app
  busy <- readVar (dsBusy dbg)
  ghci <- readVar (dsRunning dbg)
  msg <- readVar (appMessage app)
  path <- readVar (tsPath tl)
  styled (panelStyle (background (inkGround i) . borderTop 1 (inkRule i) . cornerRadius 0)) $
    panelWith (asRow . fillW . fixedH 28 . padXY 14 0 . gap 14) $ themed th $ do
      scope $ for_ timeline $ \p -> do
        let file = maybe "the eventlog" (T.pack . takeFileName) path
            toTimeline = writeVar (appTab app) PTimeline
            menu = do
              menuHeader file
              if lpFollowing p
                then whenM (menuItem "Stop following") (writeVar (tsFollow tl) False)
                else whenM (menuItem "Follow as it is written") (writeVar (tsFollow tl) True)
              whenM (menuItem "Show in Timeline") toTimeline
        unless (lpDone p) $ do
          let frac = if lpTotal p > 0 then fromIntegral (lpBytes p) / fromIntegral (lpTotal p) else 0 :: Double
          progressBarWith (fixedW 120 . alignMid) 6 (realToFrac frac)
          connection Nothing ("Reading " <> file <> ", " <> fmtBytes (fromIntegral (lpBytes p))) toTimeline menu
        when (lpFollowing p && lpDone p) $ connection Nothing ("Following " <> file) toTimeline menu
        for_ (lpError p) $ \e -> connection (Just Danger) ("Eventlog decoding stopped: " <> T.take 100 e) toTimeline menu
      scope $ for_ live $ \d -> do
        let toLive = writeVar (appTab app) PLive
            reconnect = modifyVar (lvAttempt lv) (+ 1)
        for_ (ldHello d) $ \(p, pid, _) -> when (isNothing (ldClosed d)) $
          connection Nothing ("Live: " <> p <> ", pid " <> tshow pid) toLive $ do
            menuHeader (p <> ", pid " <> tshow pid)
            whenM (menuItem "Disconnect") (writeVar (lvWant lv) False)
            whenM (menuItem "Reconnect") reconnect
            case commandsWhy live of
              Nothing -> whenM (menuItem "Mark…") (openMarkPrompt lv)
              Just why -> menuItemDisabled ("Mark… (" <> why <> ")")
            whenM (menuItem "Show in Live") toLive
        for_ (ldClosed d) $ \why ->
          connection (Just Danger) ("Agent disconnected: " <> T.take 80 why) toLive $ do
            whenM (menuItem "Reconnect") reconnect
            whenM (menuItem "Forget the connection") (writeVar (lvWant lv) False)
      scope $ for_ (dhTarget hooks) $ \t -> do
        let toGraph = writeVar (hsSource hs) FromGhcDebug >> writeVar (appTab app) PHeap
            state
              | snapshotRunning heap = "ghc-debug: taking a snapshot…"
              | otherwise = "ghc-debug: " <> targetName t
        connection Nothing state toGraph $ do
          menuHeader (targetName t)
          case dhSnapshot hooks of
            Right snap | not (dhBusy hooks) -> whenM (menuItem "Take snapshot") (void snap)
            _ -> menuItemDisabled "Take snapshot"
          whenM (menuItem "Forget the target") (writeVar (hsDbgSocket hs) "")
          whenM (menuItem "Show the live graph") toGraph
      scope $ when (ghci || busy) $ do
        let toDebugger = writeVar (appTab app) PDebugger
        wake <- askWake
        connection Nothing (if busy then "GHCi is working…" else "GHCi session running") toDebugger $ do
          whenM (menuItem "Stop the session") (liftIO (stopSession dbg wake))
          whenM (menuItem "Restart the session") (liftIO (stopSession dbg wake >> startSession dbg wake))
          whenM (menuItem "Show in Debugger") toDebugger
      watchStatus env (appWatchers app)
      baselineStatus (appBaseline app) baseline
      scope $ unless (T.null msg) $ do
        labelWith (alignMid . fontSizeScale 0.85 . fontTone Warning) msg
        whenM (buttonWith (alignMid . padXY 6 0 . tight) "Dismiss") (writeVar (appMessage app) "")
      flex
      cursor <- readVar (busCursor (envBus env))
      scope $ for_ cursor $ \(rk, t) -> do
        labelWith (alignMid . fontMuted . fontSizeScale 0.85) "Cursor"
        rowWith (alignMid . tight) $ linkText env (RInstant rk t) (fontSizeScale 0.85) (fmtNs (fromIntegral t))
      focus <- readVar (busFocus (envBus env))
      scope $ for_ focus $ \r -> do
        labelWith (alignMid . fontMuted . fontSizeScale 0.85) "Focus"
        rowWith (alignMid . tight) $ linkText env r (fontSizeScale 0.85) (T.take 60 (refTitle env r))
        whenM (buttonWith (alignMid . padXY 6 0 . tight) "Clear") (setFocus env Nothing)
      labelWith (alignMid . fontMuted . fontSizeScale 0.85) (panelHint active)

-- | A live source in the status bar: a click opens its panel, a
-- right-click its menu.
connection :: Maybe Tone -> T.Text -> NanoUI () -> NanoUI () -> NanoUI ()
connection tone txt open menu = do
  sid <- currentId
  prev <- lastRect sid
  mouse <- uiMousePos
  let under = maybe False (`rectContains` mouse) prev
  (_, area) <- withCursorShape UiCursorPointer $ mouseArea (alignMid . tight) $
    labelWith (alignMid . fontSizeScale 0.85 . maybe id fontTone tone . (if under then fontUnderline else id)) txt
  when (respClicked area) open
  tooltip area "Click to open, right-click for more"
  void (contextMenu area menu)

targetName :: DebugTarget -> T.Text
targetName = \case
  DebugUnix p -> T.pack (takeFileName p)
  DebugTCP h p -> T.pack h <> ":" <> tshow p
  DebugSnapshotFile p -> T.pack (takeFileName p)

-- ---------------------------------------------------------------------------
-- Attaching

-- | Open what Connect found: follow the eventlog, read the heap profile
-- (beside the eventlog's own samples, when there are both), take
-- snapshots from the ghc-debug socket, search the working directory for
-- sources, and open its dumps unless a dump folder is open already.
applyAttach :: App -> [AttachStep] -> NanoUI ()
applyAttach app steps = do
  let found k = listToMaybe [p | s <- steps, asKind s == k, Just p <- [asPath s]]
      tl = appTimeline app
  for_ (found AttachEventlog) $ \p -> do
    cur <- readVar (tsPath tl)
    following <- readVar (tsFollow tl)
    unless (cur == Just p && following) (openEventlog tl p True)
    writeVar (lvEventlog (appLive app)) (T.pack p)
  for_ (found AttachHeapProfile) $ \p ->
    if isJust (found AttachEventlog)
      then writeVar (hsHpPath (appHeap app)) (Just p)
      else openHp (appHeap app) p
  for_ (found AttachGhcDebug) (useDebugSocket (appHeap app))
  for_ (found AttachSources) $ \p -> writeVar (appAttachRoots app) [p]
  for_ (found AttachDumps) $ \p -> do
    root <- readVar (csRoot (appCore app))
    when (isNothing root) (openDumpDir (appCore app) p)

-- | What Live needs to take snapshots and pause through the heap panel.
debugHooks :: App -> HeapLoaded -> NanoUI DebugHooks
debugHooks app heap = do
  target <- debugTarget (appHeap app)
  pure
    DebugHooks
      { dhSnapshot = case target of
          _ | not ghcDebugEnabled -> Left "built without ghc-debug: cabal build -f ghc-debug"
          Nothing -> Left "no ghc-debug target: link ghc-debug-stub into the program (Unix only)"
          Just _ -> Right (either (const 0) id <$> requestSnapshot (appHeap app))
      , dhBusy = snapshotRunning heap
      , dhTarget = target
      }

-- ---------------------------------------------------------------------------
-- Runs

linkRun :: M.Map RunKey RunInfo -> RunInfo -> RunInfo
linkRun links ri = M.findWithDefault ri (runKey ri) links

linkTimeline :: M.Map RunKey RunInfo -> LoadProgress -> LoadProgress
linkTimeline links lp
  | M.null links = lp
  | otherwise = lp {lpTimeline = fmap one (lpTimeline lp)}
  where
    one t =
      let ri = linkRun links (tlRun t)
       in t {tlRun = ri, tlHeapProfile = fmap (\hp -> hp {hpRun = ri}) (tlHeapProfile t)}

linkHeap :: M.Map RunKey RunInfo -> HeapLoaded -> HeapLoaded
linkHeap links hl
  | M.null links = hl
  | otherwise = hl {hlHp = fmap (fmap (fmap (\hp -> hp {hpRun = linkRun links (hpRun hp)}))) (hlHp hl)}

linkLive :: M.Map RunKey RunInfo -> LiveData -> LiveData
linkLive links d
  | M.null links = d
  | otherwise = d {ldHello = fmap one (ldHello d)}
  where
    one (p, pid, caps) =
      let ri = linkRun links (runFromHello p pid)
       in (maybe p id (riProgram ri), maybe pid id (riPid ri), caps)

linkRuns :: App -> [Source] -> RunInfo -> NanoUI ()
linkRuns app members target =
  modifyVar (appRunLinks app) (M.union (M.fromList [(srcOrigin s, target) | s <- members]))

unlinkRun :: App -> RunKey -> NanoUI ()
unlinkRun app rk = modifyVar (appRunLinks app) (M.delete rk)

-- | Every artifact and connection, with the run it names, for the data
-- sources popover. Takes what the loaders returned before runs were
-- joined, so each says what it named itself.
sourcesOf :: App -> Maybe LoadProgress -> HeapLoaded -> CoreLoaded -> Maybe LiveData -> M.Map RunKey RunInfo -> NanoUI [Source]
sourcesOf app timeline heap core live links = do
  logPath <- readVar (tsPath (appTimeline app))
  hpPath <- readVar (hsHpPath (appHeap app))
  profPath <- readVar (hsProfPath (appHeap app))
  dumpRoot <- readVar (csRoot (appCore app))
  ghci <- readVar (dsRunning (appDebug app))
  ghciCmd <- readVar (dsCommand (appDebug app))
  target <- debugTarget (appHeap app)
  host <- readVar (lvHost (appLive app))
  port <- readVar (lvPort (appLive app))
  let source kind detail ri panel ds =
        let key = runKey ri
         in Source kind detail (linkRun links ri) key (M.member key links) panel ds
      bySource hp = if hpBreakdown hp == "info table" then [DHeapCensus, DHeapSource] else [DHeapCensus]
      ok = \case
        Just (TaskDone (Right x)) -> Just x
        Just (TaskRunning (Just (Right x))) -> Just x
        _ -> Nothing
  pure $
    concat
      [ [ source "Eventlog" (maybe "" T.pack logPath) (tlRun t) PTimeline (DTimeline : maybe [] bySource (tlHeapProfile t))
        | Just t <- [timeline >>= lpTimeline]
        ]
      , [source "Heap profile" (T.pack p) (hpRun hp) PHeap (bySource hp) | Just p <- [hpPath], Just hp <- [ok (hlHp heap)]]
      , [source "Cost centres" (T.pack p) (runFromProfHeader (pfHeader pf)) PHeap [DCostCentres] | Just p <- [profPath], Just pf <- [ok (hlProf heap)]]
      , [ source
            "Agent"
            (host <> ":" <> port)
            (mergeRunInfo (runFromHello p pid) (maybe emptyRunInfo (runFromArgs . aiArgs) (ldInfo d)))
            PLive
            (DLive : [DLiveStats | Just f <- [Seq.lookup (Seq.length (ldFrames d) - 1) (ldFrames d)], frProgramExact f])
        | Just d <- [live]
        , isNothing (ldClosed d)
        , Just (p, pid, _) <- [ldHello d]
        ]
      , [ source "ghc-debug" (targetName t) (case t of DebugUnix p -> maybe emptyRunInfo id (runFromSocket (T.pack p)); _ -> emptyRunInfo) PHeap [DLiveGraph]
        | Just t <- [target]
        ]
      , [ source "Dumps" (T.pack r <> maybe "" (\as -> ", " <> tshow (length as) <> " modules") (clArtifacts core)) emptyRunInfo PCore [DDumps]
        | Just r <- [dumpRoot]
        ]
      , [source "GHCi" ghciCmd emptyRunInfo PDebugger [DDebugger] | ghci]
      ]

-- | The panels, as folder tabs along the bottom of the title bar.
navBar :: Var Panel -> NanoUI ()
navBar v =
  rowWith (alignBottom . tight) $
    boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabContained} panelTitle v

-- ---------------------------------------------------------------------------
-- Settings

zoomIn, zoomOut, zoomReset, openSettings :: [K.Shortcut]
zoomIn = [K.cmdOrCtrl <> K.key '=', K.cmdOrCtrl <> K.shift <> K.key '=', K.cmdOrCtrl <> K.key '+']
zoomOut = [K.cmdOrCtrl <> K.key '-']
zoomReset = [K.cmdOrCtrl <> K.key '0']
openSettings = [K.cmdOrCtrl <> K.key ',']

-- | The window-wide chords: open Settings, and step the interface scale.
settingsShortcuts :: App -> NanoUI ()
settingsShortcuts app = do
  let pressed = fmap or . mapM shortcut
  whenM (pressed openSettings) (writeVar (appSettingsOpen app) True)
  bigger <- pressed zoomIn
  smaller <- pressed zoomOut
  reset <- pressed zoomReset
  when (bigger || smaller) $ do
    zoom <- currentZoom
    modifyVar (appScale app) (stepScale (if bigger then 1 else -1) zoom)
  when reset (writeVar (appScale app) ScaleAuto)

settingsModal :: App -> NanoUI ()
settingsModal app = do
  zoom <- currentZoom
  scale <- readVar (appScale app)
  path <- liftIO settingsPath
  let auto = if scale == ScaleAuto then Just zoom else Nothing
      chords = T.intercalate ", " . map (K.shortcutLabel . headOr)
      headOr = \case k : _ -> k; [] -> mempty
      input f v = textInputConfigured defaultTextInputConfig {ticLayout = (alignMid . f) defaultLayout} v
      inputHint placeholder f v = textInputConfigured defaultTextInputConfig {ticPlaceholder = placeholder, ticLayout = (alignMid . f) defaultLayout} v
      core = appCore app
  modalWith (fixedW 620) (appSettingsOpen app) "Settings" $
    columnWith (fillW . tight . gap 8) $ do
      sectionTitle "Appearance"
      settingRow "Theme" $
        boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} themeChoiceName (appTheme app)
      settingRow "Interface scale" $ enumSelectWith (alignMid . fixedW 230) (uiScaleLabel auto) (appScale app)
      settingNote $
        chords [zoomIn, zoomOut] <> " step the scale; " <> chords [zoomReset] <> " follows the display again."
      sectionTitle "Startup"
      settingRow "Open on" $
        selectWith
          (alignMid . fixedW 230)
          ("The panel last used" : map panelTitle [minBound .. maxBound])
          (zoomVar (maybe 0 ((+ 1) . fromEnum)) (\i _ -> if i <= 0 then Nothing else Just (toEnum (i - 1))) (appStartPanel app))
      settingRow "" $ checkboxWith alignMid "Reopen the window where it was, at the size it was" (appRestoreWindow app)
      settingRow "" $ checkboxWith alignMid "Reopen the last session on start" (appReopenLast app)
      lastPath <- liftIO lastSessionPath
      settingNote ("The session is saved on quit to " <> T.pack lastPath <> ". Files on the command line open instead.")
      sectionTitle "Live"
      settingRow "Agent address" $ do
        input (fixedW 170) (lvHost (appLive app))
        toolLabel ":"
        input (fixedW 80) (lvPort (appLive app))
      sectionTitle "Debugger"
      settingRow "GHCi command" $ input (fixedW 260) (dsCommand (appDebug app))
      sectionTitle "Core & STG"
      settingRow "Colouring" $
        boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} colouringName (csColouring core)
      settingRow "" $ do
        checkboxWith alignMid "Hide uniques" (csHideUniques core)
        checkboxWith alignMid "Hide Typeable bindings" (csHideTypeable core)
      settingNote "The Core & STG toolbar changes these too; the last choice is kept."
      sectionTitle "Source"
      settingRow "Project root" $ inputHint "folder whose sources the panels link to" fillW (csProjectRoot core)
      settingNote "Sources are also found beside the dumps, in the project the dump folder is in, and under the debugger's directory."
      settingRow "Editor command" $ inputHint "code -g {file}:{line}:{col}" fillW (csEditor core)
      settingNote "Run by \"Open in editor\" on any source line; {file}, {line} and {col} are filled in."
      columnWith (padTop 12 . fillW . tight . gap 8) $ do
        labelWith (fillW . fontMuted . fontSizeScale 0.85) ("Saved to " <> T.pack path)
        fillRow $ do
          flex
          whenM (toolButton "Reset to defaults") (resetSettings app)
          whenM (styled primary (toolButton "Done")) (writeVar (appSettingsOpen app) False)

-- | Every setting the dialog shows back to its default. The window's
-- remembered geometry is left alone.
resetSettings :: App -> NanoUI ()
resetSettings app = do
  let d = defaultSettings
      core = appCore app
  writeVar (appTheme app) (cfgTheme d)
  writeVar (appScale app) (cfgScale d)
  writeVar (appRestoreWindow app) (cfgRestoreWindow d)
  writeVar (appStartPanel app) (cfgStartPanel d >>= panelNamed)
  writeVar (lvHost (appLive app)) (cfgAgentHost d)
  writeVar (lvPort (appLive app)) (cfgAgentPort d)
  writeVar (dsCommand (appDebug app)) (cfgGhciCommand d)
  writeVar (csColouring core) (cfgColouring d)
  writeVar (csHideUniques core) (cfgHideUniques d)
  writeVar (csHideTypeable core) (cfgHideTypeable d)
  writeVar (csProjectRoot core) (cfgProjectRoot d)
  writeVar (csEditor core) (cfgEditorCommand d)
  writeVar (appReopenLast app) (cfgReopenSession d)

-- | A setting: its name in a column of names, then its controls.
settingRow :: T.Text -> NanoUI () -> NanoUI ()
settingRow name controls =
  rowWith (fillW . tight . gap 12) $ do
    labelWith (alignMid . fixedW 130 . fontSizeScale 0.92) name
    rowWith (alignMid . fillW . tight . gap 8) controls

-- | A line of explanation under a setting, indented to its controls.
settingNote :: T.Text -> NanoUI ()
settingNote t = rowWith (padLeft 142 . fillW . tight) (labelWith (fillW . fontMuted . fontSizeScale 0.85) t)

-- | A dropdown over a bounded enum, with a layout.
enumSelectWith :: (Bounded a, Enum a) => (Layout -> Layout) -> (a -> T.Text) -> Var a -> NanoUI ()
enumSelectWith f name v = selectWith f (map name [minBound .. maxBound]) (zoomVar fromEnum (\i _ -> toEnum i) v)
