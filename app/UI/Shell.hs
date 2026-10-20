-- | The window: a header with the panel tabs, history, the palette, a
-- global Open and Settings, file drag-and-drop, and the per-frame loaders
-- that keep background work going whichever panel is showing. The panels'
-- lenses are built here each frame from what the loaders returned, so every
-- panel can link to every other ("UI.Link").
module UI.Shell
  ( App
  , Options (..)
  , defaultOptions
  , newApp
  , appView
  , saveAppSettings
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (intToDigit)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, askSdlEnv, defaultFileDialogOptions, setSdlUiScale, useFileDialog, windowZoom)
import NanoUI.Shortcut qualified as K
import Stronghead.Dump (kindForPath)
import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Eventlog (LoadProgress (..))
import Stronghead.Format (fmtBytes, tshow)
import Stronghead.Ref (Ref, parseRefTarget)
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName)
import UI.Code (colouringName)
import UI.Common
import UI.Core
import UI.Debugger
import UI.Heap
import UI.Link
import UI.Live
import UI.Palette
import UI.Settings
import UI.Timeline

-- | How to drive the panel, for the status bar.
panelHint :: Panel -> T.Text
panelHint = \case
  PTimeline -> "Wheel to zoom, drag to pan, double-click to fit"
  PHeap -> "Drag across the chart to move the census cursor"
  PCore -> "Hover a name to find it in every pane, click it to jump to its binding; click a bar or a note to go to what it measures"
  PDebugger -> "Click the gutter to set or clear a breakpoint"
  PLive -> "Samples arrive four times a second"

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
  }

defaultOptions :: Options
defaultOptions = Options [] Nothing Nothing Nothing Nothing Nothing

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
  timeline <- timelineLoad (appTimeline app)
  heap <- heapLoad (appHeap app)
  debugDir <- T.unpack <$> readVar (dsDir (appDebug app))
  core <- coreLoad (appCore app) [debugDir]
  debugSrc <- debugLoad (appDebug app)
  live <- liveLoad (appLive app)
  let logProfile = timeline >>= lpTimeline >>= tlHeapProfile

  active <- readVar (appTab app)
  heapL <- heapLens (appHeap app) heap logProfile
  -- Each lens may ask the ones built before it where an entity is in the
  -- source: Core opens it in an editor, the Debugger breaks on it.
  coreL <- coreLens (appCore app) core (lensSpan heapL) (lensAnnotations heapL)
  debugL <- debugLens (appDebug app) debugSrc (clIndex core) (\r -> maybe (lensSpan coreL r) Just (lensSpan heapL r))
  let env =
        Env
          { envBus = appBus app
          , envLenses = [timelineLens (appTimeline app) timeline, heapL, coreL, debugL, liveLens (appLive app) live]
          , envPanel = active
          , envSwitch = writeVar (appTab app)
          }
  navShortcuts app env
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
        themed th $ whenM (toolButton "Go to…") (openPalette (appPalette app))
        themed th $ whenM (toolButton "Settings") (writeVar (appSettingsOpen app) True)
        themed th $ whenM (toolButton "Open…") (askOpen app)
    columnWith (tight . grow . gap 0) . withKey (fromEnum active) $ case active of
      PTimeline -> timelineView env (appTimeline app) timeline
      PHeap -> heapView env (appHeap app) heap logProfile
      PCore -> coreView env (appCore app) core
      PDebugger -> debugView env (appDebug app) (clIndex core) debugSrc
      PLive -> liveView env (appLive app) live $ \p -> do
        openEventlog (appTimeline app) p True
        writeVar (appTab app) PTimeline
    statusBar app env active timeline live
  settingsModal app
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

-- | Background work from every panel on the left, how to drive the active
-- panel on the right.
statusBar :: App -> Env -> Panel -> Maybe LoadProgress -> Maybe LiveData -> NanoUI ()
statusBar app env active timeline live = do
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
      focus <- readVar (busFocus (envBus env))
      scope $ for_ focus $ \r -> do
        labelWith (alignMid . fontMuted . fontSizeScale 0.85) "Focus"
        linkText env r (alignMid . fontSizeScale 0.85) (T.take 60 (refTitle env r))
        whenM (buttonWith (alignMid . padXY 6 0 . tight) "Clear") (setFocus env Nothing)
      labelWith (alignMid . fontMuted . fontSizeScale 0.85) (panelHint active)

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
      settingRow "Project root" $ inputHint "folder whose sources the panels link to" (fillW) (csProjectRoot core)
      settingNote "Sources are also found beside the dumps, in the project the dump folder is in, and under the debugger's directory."
      settingRow "Editor command" $ inputHint "code -g {file}:{line}:{col}" (fillW) (csEditor core)
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
