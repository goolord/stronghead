-- | The window: a header with the panel tabs, history, the palette, a
-- global Open and Settings, file drag-and-drop, and the per-frame loaders
-- that keep background work going whichever panel is showing. The panels'
-- lenses are built here each frame from what the loaders returned, so every
-- panel can link to every other ("UI.Link").
--
-- The header's Sources button lists every run's artifacts and connections
-- ("UI.Sources"); the status bar is the connection centre, where each live
-- source opens its panel or its menu. Connect in Live attaches: what it
-- finds is opened here ('applyAttach').
module UI.Shell
  ( App
  , Options (..)
  , defaultOptions
  , newApp
  , appView
  , saveAppSettings
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.Char (intToDigit)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, askSdlEnv, defaultFileDialogOptions, setSdlUiScale, useFileDialog, windowZoom)
import NanoUI.Shortcut qualified as K
import Stronghead.Attach (AttachKind (..), AttachStep (..))
import Stronghead.Debug (DebugTarget (..), ghcDebugEnabled)
import Stronghead.Dump (kindForPath)
import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Eventlog (LoadProgress (..))
import Stronghead.Format (fmtBytes, fmtNs, tshow)
import Stronghead.HeapProfile (HeapProfile (..))
import Stronghead.Live (AgentInfo (..), Frame (..))
import Stronghead.Prof (ProfFile (..))
import Stronghead.Ref (Ref (..), RunKey, parseRefTarget)
import Stronghead.Run (RunInfo (..), emptyRunInfo, mergeRunInfo, runFromArgs, runFromHello, runFromProfHeader, runFromSocket, runKey)
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName)
import UI.Clock (confirmMoments)
import UI.Code (colouringName)
import UI.Common
import UI.Core
import UI.Debugger
import UI.Heap
import UI.Link
import UI.Live
import UI.Palette
import UI.Settings
import UI.Sources
import UI.Timeline

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
  timeline0 <- timelineLoad (appTimeline app)
  heap0 <- heapLoad (appHeap app)
  debugDir <- T.unpack <$> readVar (dsDir (appDebug app))
  attachRoots <- readVar (appAttachRoots app)
  core <- coreLoad (appCore app) (debugDir : attachRoots)
  debugSrc <- debugLoad (appDebug app)
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
          }
  navShortcuts app env
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
        themed th $ document app active
        themed th $ do
          resp <- buttonWith' alignMid "Sources"
          tooltip resp "What each run has, and how to add the rest"
          when (respClicked resp) (modifyVar (appSourcesOpen app) not)
          sources <- sourcesOf app timeline0 heap0 core live0 links
          sourcesPopover env (appSourcesOpen app) (respRect resp) sources (linkRuns app) (unlinkRun app)
        themed th $ whenM (toolButton "Go to…") (openPalette (appPalette app))
        themed th $ whenM (toolButton "Settings") (writeVar (appSettingsOpen app) True)
        themed th $ whenM (toolButton "Open…") (askOpen app)
    columnWith (tight . grow . gap 0) . withKey (fromEnum active) $ case active of
      PTimeline -> timelineView env (appTimeline app) timeline
      PHeap -> heapView env (appHeap app) heap logTl
      PCore -> coreView env (appCore app) core
      PDebugger -> debugView env (appDebug app) (clIndex core) debugSrc
      PLive -> liveView env (appLive app) live logTl remote $ \p -> do
        openEventlog (appTimeline app) p True
        writeVar (appTab app) PTimeline
    statusBar app env active timeline live heap hooks
  settingsModal app
  markPrompt bus (appLive app) live
  paletteView env (appPalette app) (commands app env)
  persistSettings app
  publishHover (appBus app)

askOpen :: App -> NanoUI ()
askOpen app =
  writeVar (appOpen app) . Just
    =<< askOpenFileDialog
      defaultFileDialogOptions
        { dialogAllowMany = True
        , dialogFilters =
            [ FileFilter "Stronghead inputs" "eventlog;hp;prof;dump-simpl;dump-stg-final;dump-stg;hs"
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
commands :: App -> Env -> [Hit]
commands app env =
  [Hit "panel" (panelTitle p) ("Ctrl+" <> tshow n) (HitRun (writeVar (appTab app) p)) | (n, p) <- zip [1 :: Int ..] [minBound .. maxBound]]
    ++ [ Hit "command" "Open file…" "" (HitRun (askOpen app))
       , Hit "command" "Settings" "Ctrl+," (HitRun (writeVar (appSettingsOpen app) True))
       , Hit "command" "Back" "Alt+Left" (HitRun (goBack env))
       , Hit "command" "Forward" "Alt+Right" (HitRun (goForward env))
       , Hit "command" "Clear the focus" "Esc" (HitRun (setFocus env Nothing))
       ]

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
statusBar :: App -> Env -> Panel -> Maybe LoadProgress -> Maybe LiveData -> HeapLoaded -> DebugHooks -> NanoUI ()
statusBar app env active timeline live heap hooks = do
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
