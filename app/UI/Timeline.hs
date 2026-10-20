-- | The eventlog timeline: capability tracks (with a code track each when
-- a profiled build's cost-centre samples are in the log), the GC pressure
-- strip, the heap strip with its censuses, markers, per-thread rows, the
-- slice inspector and the markers list. A ticky build's counters annotate
-- the code views through the lens.
--
-- Wheel zooms around the pointer, dragging pans, a click selects the slice
-- under the pointer and moves the shared cursor there, and a double click
-- fits the whole log. Dragging on the ruler scrubs the cursor, Shift+drag
-- selects a range, and @[@ and @]@ step the cursor through collections
-- (with Shift, through censuses).
module UI.Timeline
  ( TimelineState (..)
  , newTimelineState
  , timelineLoad
  , timelineView
  , openEventlog
  , timelineLens
  , timelineSync
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)
import NanoUI.Path qualified as P
import Stronghead.Clock (stepTime)
import Stronghead.Dump (nameKey)
import Stronghead.Eventlog
import Stronghead.Eventlog.Model
import Stronghead.Format
import Stronghead.HeapProfile (HeapProfile (..))
import Stronghead.Join (Annotation (..), Lane (..), Shade (..), TickyName (..), parseTickyName, tickyBinder, tickyTopLevel, tickyUnique)
import Stronghead.Ref
import Stronghead.Run (runKey, runKeysMatch)
import Stronghead.Source (renderSrcLoc, renderSrcSpan, srcLocSpan)
import System.FilePath (takeFileName)
import UI.Clock
import UI.Common
import UI.Link

data TrackRef = OnCap !Int | OnThread !Word32
  deriving (Eq, Show)

data Sel
  = SelSpan !TrackRef !Int
  | SelGC !Int
  | SelRange !Word64 !Word64
  deriving (Eq, Show)

data TimelineState = TimelineState
  { tsPath :: !(Var (Maybe FilePath))
  , tsFollow :: !(Var Bool)
  , tsStream :: !(Stream (FilePath, Bool) LoadProgress)
  , tsView :: !(Var (Maybe TimeView))
    -- ^ 'Nothing' fits the whole log (and keeps fitting while following).
  , tsSel :: !(Var (Maybe Sel))
  , tsThreadScroll :: !(Var Float)
  , tsThreadFilter :: !(Var Text)
  , tsPressAt :: !(Var (Maybe V2))
  , tsDialog :: !(Var (Maybe FileDialogId))
  , tsFocusThread :: !(Var (Maybe Word32))
  , tsPanes :: !Workbench
  , tsScrub :: !(Var Bool)
    -- ^ A drag that began on the ruler, moving the cursor.
  , tsMarkerFilter :: !(Var Text)
  , tsMarkerEvents :: !(Var Bool)
    -- ^ List traceEvent messages with the markers.
  , tsWindowSeen :: !(IORef (Maybe (RunKey, TimeView)))
    -- ^ The window last published to the bus.
  , tsCode :: !(Var Bool)
    -- ^ Show a code track under each capability, from a profiled build's
    -- cost-centre samples.
  }

paneCaps, paneThreads, paneInspector, paneMarkers :: Word64
paneCaps = 1
paneThreads = 2
paneInspector = 3
paneMarkers = 4

newTimelineState :: IO TimelineState
newTimelineState = do
  tsPath <- newVar Nothing
  tsFollow <- newVar False
  tsStream <- newStream
  tsView <- newVar Nothing
  tsSel <- newVar Nothing
  tsThreadScroll <- newVar 0
  tsThreadFilter <- newVar ""
  tsPressAt <- newVar Nothing
  tsDialog <- newVar Nothing
  tsFocusThread <- newVar Nothing
  tsPanes <- newVar (Just (Split 10 AxisV 0.74 (Split 11 AxisH 0.5 (Pane paneCaps) (Pane paneThreads)) (Split 12 AxisH 0.64 (Pane paneInspector) (Pane paneMarkers))))
  tsScrub <- newVar False
  tsMarkerFilter <- newVar ""
  tsMarkerEvents <- newVar False
  tsWindowSeen <- newIORef Nothing
  tsCode <- newVar True
  pure TimelineState {..}

openEventlog :: TimelineState -> FilePath -> Bool -> NanoUI ()
openEventlog st path follow = do
  writeVar (tsPath st) (Just path)
  writeVar (tsFollow st) follow
  writeVar (tsView st) Nothing
  writeVar (tsSel st) Nothing

-- | Drive the loader. Called every frame whatever tab is showing, so a
-- load or a follow keeps running in the background.
timelineLoad :: TimelineState -> NanoUI (Maybe LoadProgress)
timelineLoad st = do
  path <- readVar (tsPath st)
  follow <- readVar (tsFollow st)
  scope $ case path of
    Nothing -> pure Nothing
    Just p ->
      Just <$> useStream (tsStream st) (p, follow) (LoadProgress 0 0 Nothing Nothing False follow) (\update -> streamEventlog follow p (update . const))

-- | Publish the visible window for views locked to the Timeline, and take
-- up a window one of them wrote. Called every frame, whatever is showing.
timelineSync :: Bus -> TimelineState -> Maybe LoadProgress -> NanoUI ()
timelineSync bus st mprog = case mprog >>= lpTimeline of
  Nothing -> do
    shared <- readVar (busWindow bus)
    when (isJust shared) (writeVar (busWindow bus) Nothing)
  Just tl -> do
    let rk = runKey (tlRun tl)
        lo = fromIntegral (tlStart tl)
        hi = fromIntegral (tlEnd tl)
    shared <- readVar (busWindow bus)
    seen <- liftIO (readIORef (tsWindowSeen st))
    case shared of
      Just (r, w) | shared /= seen, r == rk -> writeVar (tsView st) (Just (clampView lo hi w))
      _ -> pure ()
    mv <- readVar (tsView st)
    let now = Just (rk, maybe (TimeView lo hi) (clampView lo hi) mv)
    when (now /= shared) (writeVar (busWindow bus) now)
    liftIO (writeIORef (tsWindowSeen st) now)

-- ---------------------------------------------------------------------------
-- Geometry

-- | The label gutter is the same width on every canvas, so the capability
-- tracks and the thread rows share one time axis down the column.
gutterW, plotMargin, rulerH, capGap, stripGap, gcH, memH, threadRowH, codeH :: Float
gutterW = 172
plotMargin = 12
rulerH = 28
capGap = 4
stripGap = 14
gcH = 54
memH = 54
threadRowH = 20
codeH = 15

-- | The height a code track adds under its capability.
codeExtra :: Bool -> Float
codeExtra code = if code then codeH + 2 else 0

-- | Capability tracks share whatever height the pane gives them, within
-- reason.
capHeight :: Float -> Int -> Bool -> Float
capHeight h nCaps code =
  let spare = h - rulerH - 2 * stripGap - gcH - memH - 8
   in max 14 (min 40 (spare / fromIntegral (max 1 nCaps) - capGap - codeExtra code))

plotX, plotW :: Rect -> Float
plotX r = rectX r + gutterW
plotW r = max 10 (rectW r - gutterW - plotMargin)

data Rows = Rows
  { rPlotX :: !Float
  , rPlotW :: !Float
  , rRuler :: !Rect
  , rCaps :: ![Rect]
  , rCode :: ![Rect]
    -- ^ Each capability's code track, when shown.
  , rGC :: !Rect
  , rMem :: !Rect
  }

mainHeight :: Int -> Bool -> Float
mainHeight nCaps code = rulerH + fromIntegral nCaps * (14 + capGap + codeExtra code) + 2 * stripGap + gcH + memH + 8

layoutRows :: Rect -> Int -> Bool -> Rows
layoutRows r@(Rect _ y _ h) nCaps code =
  let px = plotX r
      pw = plotW r
      capH = capHeight h nCaps code
      capY i = y + rulerH + fromIntegral i * (capH + codeExtra code + capGap)
      gcY = capY nCaps + stripGap
   in Rows
        { rPlotX = px
        , rPlotW = pw
        , rRuler = Rect px y pw rulerH
        , rCaps = [Rect px (capY i) pw capH | i <- [0 .. nCaps - 1]]
        , rCode = [Rect px (capY i + capH + 2) pw codeH | code, i <- [0 .. nCaps - 1]]
        , rGC = Rect px gcY pw gcH
        , rMem = Rect px (gcY + gcH + stripGap) pw memH
        }

-- | Whether the code tracks show: there are samples and they are on.
codeShown :: TimelineState -> Timeline -> NanoUI Bool
codeShown st tl = (&& not (V.null (tlProfSamples tl))) <$> readVar (tsCode st)

-- | The code run at a point on a code track.
codeRunAt :: Timeline -> TimeView -> Rows -> V2 -> Maybe (Int, CodeRun)
codeRunAt tl v rows (V2 mx my) = case [i | (i, Rect _ ry _ rh) <- zip [0 ..] (rCode rows), my >= ry, my < ry + rh] of
  c : _ | Just runs <- tlCodeRuns tl V.!? c ->
    let t = max 0 (xToTime v (rPlotX rows) (rPlotW rows) mx)
        tol = 3 * viewWidth v / realToFrac (rPlotW rows)
        i = lowerBoundBy crEnd runs (floorW (t - tol))
     in listToMaybe
          [ (c, r)
          | r <- V.toList (V.take 3 (V.drop i runs))
          , fromIntegral (crStart r) - tol <= t
          , fromIntegral (crEnd r) + tol >= t
          , not (idleCostCentre tl (crCostCentre r))
          ]
  _ -> Nothing

idleCostCentre :: Timeline -> Int -> Bool
idleCostCentre tl i = maybe False isIdleCostCentre (IM.lookup i (tlCostCentres tl))

costCentreRef :: Timeline -> Int -> Maybe Ref
costCentreRef tl i = (\c -> RCostCentre (cciModule c) (cciLabel c)) <$> IM.lookup i (tlCostCentres tl)

-- ---------------------------------------------------------------------------
-- View

timelineView :: Env -> TimelineState -> Maybe LoadProgress -> NanoUI ()
timelineView env st mprog = columnWith (tight . gap 0 . grow) $ do
  useFileDialog (tsDialog st) $ \case
    p : _ -> openEventlog st p False
    [] -> pure ()
  path <- readVar (tsPath st)
  let loaded = mprog >>= lpTimeline
  toolStrip $ do
    whenM (toolButton "Open eventlog…") $
      writeVar (tsDialog st) . Just
        =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "GHC eventlog" "eventlog", FileFilter "All files" "*"]}
    checkboxWith alignMid "Follow as it is written" (tsFollow st)
    flex
    scope $ for_ loaded $ \tl -> do
      mv <- readVar (tsView st)
      let lo = fromIntegral (tlStart tl)
          hi = fromIntegral (tlEnd tl)
          v = maybe (TimeView lo hi) (clampView lo hi) mv
      toolLabel ("Showing " <> fmtNs (tvStart v - lo) <> " to " <> fmtNs (tvEnd v - lo))
      disabledWhen (isNothing mv) $ whenM (toolButton "Fit to log") (writeVar (tsView st) Nothing)
  case loaded of
    Nothing -> scope $ case path of
      Just p -> emptyState ("Reading " <> T.pack (takeFileName p)) ["The timeline appears as soon as the first scheduler events are decoded."] []
      Nothing ->
        emptyState
          "Open an eventlog"
          [ "Open a .eventlog file, drop one on the window, or pass it on the command line. Record one by running a -threaded program with +RTS -l:"
          ]
          ["ghc -threaded -rtsopts -eventlog Main.hs", "./Main +RTS -l -N4 -hT -RTS"]
    Just tl
      | V.null (tlCaps tl) ->
          scope $ emptyState "No scheduler events yet" ["This eventlog has no capability events. Was the program run with +RTS -l?"] []
      | otherwise -> scope (body env st tl)

body :: Env -> TimelineState -> Timeline -> NanoUI ()
body env st tl = do
  th <- uiTheme
  let pal = palette th
      lo = fromIntegral (tlStart tl)
      hi = fromIntegral (tlEnd tl) :: Double
      full = TimeView lo hi
      summary = summarize tl
  mv <- readVar (tsView st)
  let v = maybe full (clampView lo hi) mv
  marks <- marksFor env (runKey (tlRun tl)) (Just tl)
  stepCursor env st tl v
  columnWith (padBottom 8 . tight . fillW) $
    readout
      [ ("duration", fmtSec (smDuration summary))
      , ("mutator", fmtPct (smMutatorFrac summary))
      , ("GC time", fmtPct (smGCFrac summary))
      , ("collections, major", fmtInt (smGCCount summary) <> ", " <> fmtInt (smMajorGCs summary))
      , ("longest pause", fmtNs (smGCPauseMax summary))
      , ("allocated", fmtBytes (smAllocated summary))
      , ("allocation rate", fmtRate (fromIntegral (smAllocated summary) / max 1e-9 (smDuration summary)))
      , ("peak live", fmtBytes (smPeakLive summary))
      , ("threads", fmtInt (smThreads summary))
      , ("events", fmtInt (tlEventCount tl))
      ]
  mm <- monoMetrics 0.85
  sel <- readVar (tsSel st)
  workbench (tsPanes st) $ \pid ctx ->
    if
      | pid == paneCaps ->
          paneWith tight ctx "Capabilities" (capsLegend st pal tl) (mainCanvas env st pal mm tl v sel marks)
      | pid == paneThreads ->
          paneWith tight ctx "Threads" (threadsHeader st pal tl) (threadsSection env st pal mm tl v sel marks)
      | pid == paneMarkers ->
          paneWith tight ctx "Markers" (markersHeader st tl) (markersList env st tl)
      | otherwise -> do
          let title = case sel of
                Nothing -> "Overview"
                Just (SelGC _) -> "Collection"
                Just (SelSpan _ _) -> "Slice"
                Just (SelRange _ _) -> "Range"
          paneWith tight ctx title (inspectorActions env st tl sel marks) $
            paneScroll (grow . tight) $
              columnWith (padAll 12 . gap 6 . fillW) (inspector env st tl v sel)

capsLegend :: TimelineState -> Palette -> Timeline -> NanoUI ()
capsLegend st pal tl = do
  scope $ unless (V.null (tlProfSamples tl)) $
    checkboxWith alignMid ("Code (" <> fmtInt (V.length (tlProfSamples tl)) <> " samples)") (tsCode st)
  flex
  legendDot (pRun pal) "running"
  legendDot (pGC pal) "minor GC"
  legendDot (pGCMajor pal) "major GC"
  legendDot (pBlocked pal) "stopped to block"
  legendDot (pMarker pal) "marker"

inspectorActions :: Env -> TimelineState -> Timeline -> Maybe Sel -> Marks -> NanoUI ()
inspectorActions env st tl sel marks = do
  flex
  scope $ for_ (tmRange marks) $ \(a, b) -> unless (sel == Just (SelRange a b)) $
    whenM (buttonWith (alignMid . padXY 8 2 . tight) "Analyse range") (analyseRange env st tl a b)
  focus <- readVar (tsFocusThread st)
  scope $ when (isJust focus) $
    whenM (buttonWith (alignMid . padXY 8 2 . tight) "Clear highlight") (writeVar (tsFocusThread st) Nothing)
  scope $ when (isJust sel) $
    whenM (buttonWith (alignMid . padXY 8 2 . tight) "Back to overview") (writeVar (tsSel st) Nothing)

-- | Apply wheel zoom, drag pan (unless @pan@ is off: a range or the cursor
-- is being dragged) and double-click fit for a canvas whose plot area spans
-- @[plotX, plotX + plotW]@.
navigate :: TimelineState -> Timeline -> TimeView -> Response -> Float -> Float -> Bool -> Bool -> NanoUI ()
navigate st tl v resp plotX plotW wheelZooms pan = do
  let lo = fromIntegral (tlStart tl)
      hi = fromIntegral (tlEnd tl)
      setView = writeVar (tsView st) . Just . clampView lo hi
  inp <- askInput
  let mouse = inputMousePos inp
      ctrlHeld = modCtrl (inputModifiers inp)
  (wx, wy) <- useWheelDeltaOn resp
  when ((wheelZooms || ctrlHeld) && wy /= 0) $
    setView (zoomAround (0.8 ** realToFrac wy) (xToTime v plotX plotW (v2X mouse)) v)
  when (wx /= 0) $
    setView (TimeView (tvStart v - realToFrac wx * viewWidth v * 0.1) (tvEnd v - realToFrac wx * viewWidth v * 0.1))
  drag <- useDrag2DOn resp
  let dx = v2X (dragDelta drag)
  when (pan && dragActive drag && dx /= 0) $ do
    let dt = realToFrac dx / realToFrac plotW * viewWidth v
    setView (TimeView (tvStart v - dt) (tvEnd v - dt))
  when (respHovered resp && pressedIn MouseLeft inp) $ do
    writeVar (tsPressAt st) (Just mouse)
    when (pan && inputMouseClicks inp >= 2) (writeVar (tsView st) Nothing)

-- | Whether this frame's click is a click and not the end of a pan.
isClick :: TimelineState -> Response -> NanoUI Bool
isClick st resp
  | not (respClicked resp) = pure False
  | otherwise = do
      pressAt <- readVar (tsPressAt st)
      mouse <- uiMousePos
      pure $ case pressAt of
        Just (V2 px py) -> abs (px - v2X mouse) + abs (py - v2Y mouse) < 5
        Nothing -> True

mainCanvas :: Env -> TimelineState -> Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> Marks -> NanoUI ()
mainCanvas env st pal mm tl v sel marks = do
  let nCaps = V.length (tlCaps tl)
      rk = runKey (tlRun tl)
  inp <- askInput
  code <- codeShown st tl
  (isFocus, _) <- isFocusOrHover env
  let mouse = inputMousePos inp
  -- Geometry from last frame's rect: interaction runs before layout.
  sid <- currentId
  prevRect <- fromMaybe (Rect 0 0 0 0) <$> lastRect sid
  let rows0 = layoutRows prevRect nCaps code
      hover = hoverInfo tl v rows0 mouse
      focusCC i = maybe False isFocus (costCentreRef tl i)
  resp <-
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = (fillW . fillH . minH (mainHeight nCaps code)) defaultLayout
        , canvasTrackPointer = True
        , canvasCursor = Just (\_ _ _ -> UiCursorCrosshair)
        }
      (\r -> drawMain pal mm tl v sel marks code focusCC (if rectContains r mouse then hover else Nothing) r)
  let rows = layoutRows (respRect resp) nCaps code
      axis = linearAxis rk v (rPlotX rows) (rPlotW rows)
      Rect _ ry _ rh = rRuler rows
      onRuler = v2Y mouse >= ry && v2Y mouse < ry + rh && v2X mouse >= rPlotX rows
      mods = inputModifiers inp
      timeAt x = round (max 0 (axToNs axis (max (rPlotX rows) (min (rPlotX rows + rPlotW rows) x)))) :: Word64
  brushing <- timeGestures env axis resp ScrubNone
  -- A drag that begins on the ruler scrubs the cursor instead of panning.
  scrub <- readVar (tsScrub st)
  let scrubNow
        | respHovered resp && pressedIn MouseLeft inp = onRuler && not (modShift mods) && not (modCtrl mods)
        | otherwise = scrub && heldIn MouseLeft inp
  when (scrubNow /= scrub) (writeVar (tsScrub st) scrubNow)
  when scrubNow $ setCursor (envBus env) rk (timeAt (v2X mouse))
  navigate st tl v resp (rPlotX rows) (rPlotW rows) True (not brushing && not scrubNow)
  canvasLinks env resp (refAtMain tl v rows)
  clicked <- isClick st resp
  when (clicked && not brushing && not (modCtrl mods)) $ do
    -- A click on a code track focuses its cost centre everywhere.
    case codeRunAt tl v rows mouse of
      Just (_, run) -> setFocus env (costCentreRef tl (crCostCentre run))
      Nothing -> writeVar (tsSel st) (pickMain tl v rows mouse)
    when (v2X mouse >= rPlotX rows) $ setCursor (envBus env) rk (timeAt (v2X mouse))

-- | The entity at a point on the main canvas: the thread running in a
-- capability's slice, a collection, the nearest marker on the ruler, or on
-- the heap strip the census under the pointer or else the instant.
refAtMain :: Timeline -> TimeView -> Rows -> V2 -> Maybe Ref
refAtMain tl v rows m@(V2 mx my)
  | mx < rPlotX rows || mx > rPlotX rows + rPlotW rows = Nothing
  | Just (_, run) <- codeRunAt tl v rows m = costCentreRef tl (crCostCentre run)
  | otherwise = case pickMain tl v rows m of
      Just (SelSpan (OnCap c) j) ->
        let s = trackSpan (tlCaps tl V.! c) j
         in if srKind s == kindGC then RCollection rk <$> pickGC tl tw tol else Just (RThread rk (srThread s))
      Just (SelGC i) -> Just (RCollection rk i)
      _
        | inRow (rRuler rows) ->
            listToMaybe
              [ RMarker rk i
              | (i, mk) <- zip [0 ..] (V.toList (tlMarkers tl))
              , let d = if mkTime mk > tw then mkTime mk - tw else tw - mkTime mk
              , d <= tol * 2
              ]
        | inRow (rMem rows) -> Just (maybe (RInstant rk tw) (RCensus rk) (censusNear tl tw (tol * 2)))
        | otherwise -> Nothing
  where
    rk = runKey (tlRun tl)
    tw = max 0 (round (xToTime v (rPlotX rows) (rPlotW rows) mx)) :: Word64
    tol = round (4 * viewWidth v / realToFrac (rPlotW rows)) :: Word64
    inRow (Rect _ ry _ rh) = my >= ry && my < ry + rh

-- | What a click at the pointer selects on the main canvas.
pickMain :: Timeline -> TimeView -> Rows -> V2 -> Maybe Sel
pickMain tl v rows (V2 mx my) =
  let t = xToTime v (rPlotX rows) (rPlotW rows) mx
      tol = round (4 * viewWidth v / realToFrac (rPlotW rows)) :: Word64
      tw = max 0 (round t) :: Word64
      inRow (Rect _ ry _ rh) = my >= ry && my < ry + rh
   in case [i | (i, r) <- zip [0 ..] (rCaps rows), inRow r] of
        i : _ -> SelSpan (OnCap i) <$> pickSpan (tlCaps tl V.! i) tw tol
        []
          | inRow (rGC rows) -> SelGC <$> pickGC tl tw tol
          | otherwise -> Nothing

-- | The census nearest @t@, within @tol@.
censusNear :: Timeline -> Word64 -> Word64 -> Maybe Int
censusNear tl t tol =
  let cs = censusTimes tl
      d c = if c > t then c - t else t - c
   in case [(d c, j) | (j, c) <- zip [0 ..] (VU.toList cs), d c <= tol] of
        [] -> Nothing
        near -> Just (snd (minimum near))

pickGC :: Timeline -> Word64 -> Word64 -> Maybe Int
pickGC tl t tol =
  let gcs = tlGCs tl
      i = lowerBoundBy gcEnd gcs (if t > tol then t - tol else 0)
   in if i < V.length gcs && gcStart (gcs V.! i) <= t + tol then Just i else Nothing

-- ---------------------------------------------------------------------------
-- Hover text

hoverInfo :: Timeline -> TimeView -> Rows -> V2 -> Maybe (Float, Float, [Text])
hoverInfo tl v rows m@(V2 mx my)
  | mx < rPlotX rows || mx > rPlotX rows + rPlotW rows = Nothing
  | otherwise = Just (mx, my, timeLine : details)
  where
    t = xToTime v (rPlotX rows) (rPlotW rows) mx
    tw = max 0 (round t) :: Word64
    tol = round (4 * viewWidth v / realToFrac (rPlotW rows)) :: Word64
    timeLine = "t = " <> fmtNs (t - fromIntegral (tlStart tl))
    inRow (Rect _ ry _ rh) = my >= ry && my < ry + rh
    details = case codeRunAt tl v rows m of
      Just (c, run) -> codeDetails c run
      Nothing -> case [i | (i, r) <- zip [0 :: Int ..] (rCaps rows), inRow r] of
        i : _ -> capDetails i
        []
          | inRow (rGC rows) -> gcDetails
          | inRow (rMem rows) -> memDetails
          | inRow (rRuler rows) -> markerDetails
          | any inRow (rCode rows) -> ["no sample here"]
          | otherwise -> []
    -- A code run: its top frame, where that is, and the frames below.
    codeDetails c run =
      let stack = maybe [] (VU.toList . psStack) (tlProfSamples tl V.!? crFirst run)
          callers = [costCentreName tl (fromIntegral f) | f <- take 4 (drop 1 stack)]
          n = crSamples run
       in ["capability " <> tshow c <> " ran " <> costCentreName tl (crCostCentre run)]
            ++ [ "at " <> renderSrcLoc (cciSrc cc)
               | Just cc <- [IM.lookup (crCostCentre run) (tlCostCentres tl)]
               , Just _ <- [srcLocSpan (cciSrc cc)]
               ]
            ++ ["called from " <> T.intercalate " ← " callers | not (null callers)]
            ++ [fmtInt n <> (if n == 1 then " sample" else " samples") <> ", one every " <> fmtNs (fromIntegral (tlSamplePeriod tl))]
    capDetails i =
      let tr = tlCaps tl V.! i
          base = "capability " <> tshow i
       in case pickSpan tr tw tol of
            Nothing -> [base <> ": idle"]
            Just j ->
              let s = trackSpan tr j
                  dur = fmtNs (fromIntegral (srEnd s - srStart s))
               in if srKind s == kindGC
                    then [base <> ": garbage collection, " <> dur]
                    else
                      [ base <> ": running " <> threadLabel tl (srThread s)
                      , "for " <> dur <> ", then " <> stopName (srStop s)
                      ]
                        ++ [ tshow n <> " events here: migrations, steals, wakeups" | let n = pointsNear i, n > 0 ]
    pointsNear i =
      let ps = tlCapPoints tl V.! i
       in VU.length (VU.filter (\(pt, _, _) -> pt + tol >= tw && pt <= tw + tol) ps)
    gcDetails = case pickGC tl tw tol of
      Nothing -> ["no collection here"]
      Just i ->
        let g = tlGCs tl V.! i
         in [ "GC #" <> tshow (i + 1) <> genText (gcGen g) <> ", pause " <> fmtNs (fromIntegral (gcEnd g - gcStart g))
            , "copied " <> fmtBytes (gcCopied g) <> ", " <> tshow (gcParThreads g) <> " GC threads"
            ]
    memDetails =
      let win = round (viewWidth v / 50)
       in catMaybes'
            [ ("live " <>) . fmtBytes <$> seriesValueAt (tlLive tl) tw
            , ("heap " <>) . fmtBytes <$> seriesValueAt (tlHeapSize tl) tw
            , ("allocating " <>) . fmtRate <$> seriesRateBetween (tlAlloc tl) (if tw > win then tw - win else 0) (tw + win)
            , censusLine <$> censusNear tl tw (tol * 2)
            ]
    censusLine j =
      "census " <> tshow (j + 1) <> maybe "" (\hp -> ": " <> fmtBytesD (hpTotals hp VU.! j) <> " profiled") (tlHeapProfile tl)
    markerDetails =
      [ (if mkIsMarker mk then "marker: " else "event: ") <> mkText mk
      | mk <- V.toList (tlMarkers tl)
      , let d = if mkTime mk > tw then mkTime mk - tw else tw - mkTime mk
      , d <= tol * 2
      ]

catMaybes' :: [Maybe a] -> [a]
catMaybes' = mapMaybe id

genText :: Int -> Text
genText g
  | g < 0 = ""
  | g == 0 = " (minor, gen 0)"
  | otherwise = " (major, gen " <> tshow g <> ")"

threadLabel :: Timeline -> Word32 -> Text
threadLabel tl tid = maybe ("thread " <> tshow tid) threadName (M.lookup tid (tlThreads tl))

-- ---------------------------------------------------------------------------
-- Drawing

drawMain :: Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> Marks -> Bool -> (Int -> Bool) -> Maybe (Float, Float, [Text]) -> Rect -> CanvasM ()
drawMain pal mm tl v sel marks code focusCC hover r@(Rect x _ _ _) = do
  let rows = layoutRows r (V.length (tlCaps tl)) code
      px = rPlotX rows
      pw = rPlotW rows
      font = mmFont mm
      label' y txt = drawTextClipped font (mmCharW mm) (V2 (x + 14) y) (gutterW - 26) txt (pText pal)
      sub y txt = drawTextClipped font (mmCharW mm) (V2 (x + 14) y) (gutterW - 26) txt (pMuted pal)
      capsBottom = maximum (rectY (rRuler rows) + rulerH : [cy + ch | Rect _ cy _ ch <- rCaps rows ++ rCode rows])
  drawGutter pal r
  drawRuler pal mm tl v (rRuler rows)
  forM_ (zip3 [0 :: Int ..] (rCaps rows) (V.toList (tlCaps tl))) $ \(i, cr@(Rect _ cy _ ch), tr) -> do
    label' (cy + ch / 2) ("Capability " <> tshow i)
    drawTrack pal v cr tr [(kindRun, pRun pal), (kindGC, pGC pal)] True
    drawPoints pal v cr (tlCapPoints tl V.! i)
    case sel of
      Just (SelSpan (OnCap j) k) | j == i -> outlineSpan pal v cr tr k
      _ -> pure ()
  forM_ (zip3 [0 :: Int ..] (rCode rows) (V.toList (tlCodeRuns tl))) $ \(i, cr@(Rect _ cy _ ch), runs) -> do
    sub (cy + ch / 2) ("  code " <> tshow i)
    drawCodeTrack pal mm tl v cr runs focusCC
  -- Markers run down through the capability tracks.
  withClip (Rect px (rectY (rRuler rows)) pw (capsBottom - rectY (rRuler rows))) $
    forM_ (markersIn tl v) $ \mk -> do
      let mx = timeToX v px pw (fromIntegral (mkTime mk))
      if mkIsMarker mk
        then drawStroke (V2 mx (rectY (rRuler rows) + 12)) (V2 mx capsBottom) 1 (alpha 0.55 (pMarker pal))
        else drawStroke (V2 mx (rectY (rRuler rows) + 18)) (V2 mx (rectY (rRuler rows) + rulerH)) 1 (alpha 0.6 (pMuted pal))
  let Rect _ gy _ _ = rGC rows
      Rect _ my' _ _ = rMem rows
  label' (gy + 12) "GC pauses"
  sub (gy + 30) "log scale"
  drawGCStrip pal v (rGC rows) tl sel
  label' (my' + 12) "Heap"
  sub (my' + 30) "size, live"
  drawMemStrip pal v (rMem rows) tl
  -- The shared cursor and range, down the whole plot, with a handle on the
  -- ruler to drag.
  let plot = Rect px (rectY (rRuler rows)) pw (rectY (rMem rows) + rectH (rMem rows) - rectY (rRuler rows))
  drawMarks pal (linearAxis (runKey (tlRun tl)) v px pw) plot marks {tmMarkers = []}
  for_ (tmCursor marks) $ \t -> do
    let cx = timeToX v px pw (fromIntegral t)
        top = rectY (rRuler rows)
    when (cx >= px && cx <= px + pw) $
      drawPath (P.polygon [V2 (cx - 5) top, V2 (cx + 5) top, V2 cx (top + 7)]) (pAccent pal)
  for_ hover $ \(hx, hy, ls) -> do
    drawStroke (V2 hx (rectY r)) (V2 hx (rectY r + rectH r)) 1 (pCursor pal)
    drawTooltip pal mm r hx hy ls

drawRuler :: Palette -> MonoMetrics -> Timeline -> TimeView -> Rect -> CanvasM ()
drawRuler pal mm tl v (Rect px py pw ph) = do
  let step = niceStep (viewWidth v * 110 / realToFrac pw)
      origin = fromIntegral (tlStart tl)
      firstTick = fromIntegral (ceiling ((tvStart v - origin) / step) :: Integer) * step + origin
      ticks = takeWhile (<= tvEnd v) [firstTick, firstTick + step ..]
  drawRect (Rect px (py + ph - 1) pw 1) (pGrid pal)
  forM_ (take 200 ticks) $ \t -> do
    let tx = timeToX v px pw t
        txt = fmtNs (t - origin)
    drawStroke (V2 tx (py + ph - 7)) (V2 tx (py + ph)) 1 (pMuted pal)
    -- A label that would run past the plot is left off rather than cut.
    when (tx + 4 + fromIntegral (T.length txt) * mmCharW mm <= px + pw) $
      drawTextWith (mmFont mm) (V2 (tx + 4) (py + 11)) AlignStart AlignMiddle txt (pMuted pal)
  forM_ (markersIn tl v) $ \mk -> when (mkIsMarker mk) $ do
    let mx = timeToX v px pw (fromIntegral (mkTime mk))
    drawPath (P.polygon [V2 mx (py + 12), V2 (mx + 6) (py + 16), V2 mx (py + 20)]) (pMarker pal)

-- | The label gutter down the left of a canvas, ruled off from the plot.
drawGutter :: Palette -> Rect -> CanvasM ()
drawGutter pal (Rect x y _ h) = do
  drawRect (Rect x y (gutterW - 8) h) (alpha 0.025 (pText pal))
  drawRect (Rect (x + gutterW - 8) y 1 h) (pGrid pal)

markersIn :: Timeline -> TimeView -> [Marker]
markersIn tl v =
  let ms = tlMarkers tl
      i = lowerBoundBy mkTime ms (floorW (tvStart v))
   in takeWhile (\m -> fromIntegral (mkTime m) <= tvEnd v) (V.toList (V.drop i ms))

floorW :: Double -> Word64
floorW d = if d <= 0 then 0 else floor d

-- | Draw a track. When individual spans are wider than about a pixel they
-- are drawn as rectangles; otherwise each pixel column is drawn as a
-- stacked bar of the fraction of its time spent in each kind, computed from
-- the track's prefix sums.
drawTrack :: Palette -> TimeView -> Rect -> Track -> [(SpanKind, Color)] -> Bool -> CanvasM ()
drawTrack pal v r@(Rect x y w h) tr kinds markBlocked = do
  drawRect r (pTrackBg pal)
  let t0 = floorW (tvStart v)
      t1 = ceiling (tvEnd v) :: Word64
      i0 = firstSpanEndingAfter tr t0
      i1 = countStartingBy tr t1
      n = i1 - i0
      colorOf k = lookup k kinds
  withClip r $
    if n <= floor (w / 2)
      then forM_ [i0 .. i1 - 1] $ \i -> do
        let s = trackSpan tr i
            xa = max x (timeToX v x w (fromIntegral (srStart s)))
            xb = min (x + w) (timeToX v x w (fromIntegral (srEnd s)))
        for_ (colorOf (srKind s)) $ \c -> do
          drawRect (Rect xa y (max 1 (xb - xa)) h) c
          when (markBlocked && srKind s == kindRun && stopIsBlocked (srStop s) && xb - xa > 3) $
            drawRect (Rect (xb - 2) y 2 h) (pBlocked pal)
      else do
        let cols = [0, 1 .. w - 1]
            frac px k =
              let ta = xToTime v x w (x + px)
                  tb = xToTime v x w (x + px + 1)
               in realToFrac (timeIn tr (fromIntegral k) (floorW ta) (floorW tb) / max 1 (tb - ta)) :: Float
            quant f = fromIntegral (round (min 1 f * 16) :: Int) / 16
            column px = [quant (frac px k) | (k, _) <- kinds]
            runs = groupRuns [(px, column px) | px <- cols]
        forM_ runs $ \(px0, px1, fs) -> do
          let go _ [] = pure ()
              go base ((f, (_, c)) : rest) = do
                let hh = f * h
                when (hh > 0) $ drawRect (Rect (x + px0) (y + h - base - hh) (px1 - px0 + 1) hh) c
                go (base + hh) rest
          go 0 (zip fs kinds)

-- | A capability's code track: each run of samples with one top frame is
-- a block, in an ink shade of its cost centre (collections in the GC
-- colour, idle time left empty), named where it is wide enough. The cost
-- centre in focus takes the accent.
drawCodeTrack :: Palette -> MonoMetrics -> Timeline -> TimeView -> Rect -> V.Vector CodeRun -> (Int -> Bool) -> CanvasM ()
drawCodeTrack pal mm tl v r@(Rect x y w h) runs focusCC = do
  drawRect r (pTrackBg pal)
  let i0 = lowerBoundBy crEnd runs (floorW (tvStart v))
      inView = takeWhile (\cr -> fromIntegral (crStart cr) <= tvEnd v) (V.toList (V.drop i0 runs))
      shown = [cr | cr <- inView, not (idleCostCentre tl (crCostCentre cr))]
      info i = IM.lookup i (tlCostCentres tl)
      isGC c = cciLabel c == "GC" && cciModule c == "GC"
      -- A stable shade per cost centre, so neighbouring runs part.
      shade i = fromIntegral ((i * 7919) `mod` 101) / 101 :: Float
      colour i
        | focusCC i = alpha 0.8 (pAccent pal)
        | Just c <- info i, isGC c = alpha 0.75 (pGC pal)
        | otherwise = alpha (0.14 + 0.3 * shade i) (pText pal)
  withClip r $
    if length shown <= floor w
      then forM_ shown $ \cr -> do
        let xa = max x (timeToX v x w (fromIntegral (crStart cr)))
            xb = min (x + w) (timeToX v x w (fromIntegral (crEnd cr)))
            bw = max 1 (xb - xa)
        drawRect (Rect xa (y + 1) bw (h - 2)) (colour (crCostCentre cr))
        when (bw > 40) $ for_ (info (crCostCentre cr)) $ \c ->
          drawTextClipped (mmFont mm) (mmCharW mm) (V2 (xa + 3) (y + h / 2)) (bw - 6) (cciLabel c) (pText pal)
      else do
        -- Too many to draw: keep the most sampled run per pixel.
        let buckets =
              M.fromListWith
                (\a b -> if crSamples a >= crSamples b then a else b)
                [(floor (timeToX v x w (fromIntegral (crStart cr)) - x) :: Int, cr) | cr <- shown]
        forM_ (M.toList buckets) $ \(px, cr) ->
          drawRect (Rect (x + fromIntegral px) (y + 1) 1 (h - 2)) (colour (crCostCentre cr))

-- | Merge consecutive columns with identical values.
groupRuns :: Eq b => [(Float, b)] -> [(Float, Float, b)]
groupRuns [] = []
groupRuns ((p, b) : rest) =
  let (same, others) = span ((== b) . snd) rest
      end = if null same then p else fst (last same)
   in (p, end, b) : groupRuns others

outlineSpan :: Palette -> TimeView -> Rect -> Track -> Int -> CanvasM ()
outlineSpan pal v (Rect x y w h) tr i
  | i >= trackLength tr = pure ()
  | otherwise = do
      let s = trackSpan tr i
          xa = max (x - 2) (timeToX v x w (fromIntegral (srStart s)))
          xb = min (x + w + 2) (timeToX v x w (fromIntegral (srEnd s)))
      drawStrokeRoundedRect (Rect (xa - 2) (y - 2) (max 4 (xb - xa + 4)) (h + 4)) 2 2 (pSelection pal)

drawPoints :: Palette -> TimeView -> Rect -> VU.Vector (Word64, Word8', Word32) -> CanvasM ()
drawPoints pal v r@(Rect x y w _) ps = do
  let t0 = floorW (tvStart v)
      inView = VU.filter (\(t, _, _) -> t >= t0 && fromIntegral t <= tvEnd v) ps
  when (VU.length inView <= 600) $ withClip r $
    VU.forM_ inView $ \(t, k, _) -> do
      let px = timeToX v x w (fromIntegral t)
          c
            | k == pointMigrate = pMigrate pal
            | k == pointSteal = pSteal pal
            | otherwise = pWakeup pal
      drawPath (P.polygon [V2 (px - 3) y, V2 (px + 3) y, V2 px (y + 5)]) c

type Word8' = SpanKind

drawGCStrip :: Palette -> TimeView -> Rect -> Timeline -> Maybe Sel -> CanvasM ()
drawGCStrip pal v r@(Rect x y w h) tl sel = do
  drawRect r (pTrackBg pal)
  let gcs = tlGCs tl
      i0 = lowerBoundBy gcEnd gcs (floorW (tvStart v))
      inView = takeWhile (\g -> fromIntegral (gcStart g) <= tvEnd v) (V.toList (V.drop i0 gcs))
      maxPause = maximum (1 : [fromIntegral (gcEnd g - gcStart g) | g <- V.toList gcs]) :: Double
      height g = realToFrac (log (1 + fromIntegral (gcEnd g - gcStart g) / 1000) / log (1 + maxPause / 1000)) * (h - 4)
      colour g = if gcGen g >= 1 then pGCMajor pal else pGC pal
  withClip r $
    if length inView <= floor w
      then forM_ inView $ \g -> do
        let xa = timeToX v x w (fromIntegral (gcStart g))
            xb = timeToX v x w (fromIntegral (gcEnd g))
            hh = max 2 (height g)
        drawRect (Rect xa (y + h - hh) (max 2 (xb - xa)) hh) (colour g)
      else do
        -- Too many to draw: keep the worst pause per pixel.
        let buckets =
              M.fromListWith
                (\a b -> if gcEnd a - gcStart a >= gcEnd b - gcStart b then a else b)
                [(floor (timeToX v x w (fromIntegral (gcStart g)) - x) :: Int, g) | g <- inView]
        forM_ (M.toList buckets) $ \(px, g) -> do
          let hh = max 2 (height g)
          drawRect (Rect (x + fromIntegral px) (y + h - hh) 1 hh) (colour g)
  case sel of
    Just (SelGC i) | i < V.length gcs -> do
      let g = gcs V.! i
          xa = timeToX v x w (fromIntegral (gcStart g))
          xb = timeToX v x w (fromIntegral (gcEnd g))
      drawStrokeRoundedRect (Rect (xa - 3) (y - 1) (max 6 (xb - xa + 6)) (h + 2)) 2 2 (pSelection pal)
    _ -> pure ()

drawMemStrip :: Palette -> TimeView -> Rect -> Timeline -> CanvasM ()
drawMemStrip pal v r@(Rect x y w h) tl = do
  drawRect r (pTrackBg pal)
  let peak = fromIntegral (max 1 (max (VU.foldl' (\m (_, b) -> max m b) 0 (tlHeapSize tl)) (VU.foldl' (\m (_, b) -> max m b) 0 (tlLive tl)))) :: Double
      py b = y + h - 2 - realToFrac (fromIntegral b / peak) * (h - 6)
      pts s =
        let i0 = max 0 (lowerBoundBy fst (V.convert s) (floorW (tvStart v)) - 1)
            vis = takeWhile (\(t, _) -> fromIntegral t <= tvEnd v) (VU.toList (VU.drop i0 s))
            extra = take 1 (VU.toList (VU.drop (i0 + length vis) s))
         in downsample w [(timeToX v x w (fromIntegral t), b) | (t, b) <- vis ++ extra]
      steps ps = concat [[V2 xa (py b), V2 xb (py b)] | ((xa, b), (xb, _)) <- zip ps (drop 1 ps ++ [(x + w, snd (last ps))])]
  withClip r $ do
    case pts (tlHeapSize tl) of
      ps@(_ : _) ->
        let line = steps ps
            V2 fx _ = case line of { p : _ -> p; [] -> V2 x y }
         in drawPath (P.polygon (V2 fx (y + h) : line ++ [V2 (x + w) (y + h)])) (pHeap pal)
      [] -> pure ()
    case pts (tlLive tl) of
      ps@(_ : _ : _) -> drawStrokePath (P.polyline (steps ps)) 1.5 (pLive pal)
      _ -> pure ()
    -- Censuses as ticks along the bottom.
    let cs = censusTimes tl
        c0 = floorW (tvStart v)
        inView = VU.filter (\c -> c >= c0 && fromIntegral c <= tvEnd v) cs
    when (VU.length inView <= floor w) $
      VU.forM_ inView $ \c -> do
        let cx = timeToX v x w (fromIntegral c)
        drawStroke (V2 cx (y + h - 7)) (V2 cx (y + h)) 1.5 (pText pal)

-- | At most two points per pixel: the first and the maximum.
downsample :: Float -> [(Float, Word64)] -> [(Float, Word64)]
downsample w ps
  | length ps <= floor (w * 2) = ps
  | otherwise =
      [ (fromIntegral px, maximum (map snd grp))
      | (px, grp) <- M.toList (M.fromListWith (flip (++)) [(floor x :: Int, [(x, b)]) | (x, b) <- ps])
      ]

drawTooltip :: Palette -> MonoMetrics -> Rect -> Float -> Float -> [Text] -> CanvasM ()
drawTooltip pal mm (Rect rx ry rw rh) hx hy ls = do
  let lw = maximum (1 : map T.length ls)
      tw = fromIntegral lw * mmCharW mm + 16
      th' = fromIntegral (length ls) * (mmLineH mm + 2) + 10
      tx = if hx + 14 + tw > rx + rw then hx - 14 - tw else hx + 14
      ty = max ry (min (ry + rh - th') (hy + 14))
  drawRoundedRect (Rect (tx + 2) (ty + 3) tw th') 6 (alpha 0.25 colorBlack)
  drawRoundedRect (Rect tx ty tw th') 6 (alpha 0.96 (pTrackBg pal `over` colorFrom pal))
  drawStrokeRoundedRect (Rect tx ty tw th') 6 1 (pGrid pal)
  forM_ (zip [0 :: Int ..] ls) $ \(i, l) ->
    drawTextWith (mmFont mm) (V2 (tx + 8) (ty + 5 + (fromIntegral i + 0.5) * (mmLineH mm + 2))) AlignStart AlignMiddle l (if i == 0 then pMuted pal else pText pal)
  where
    over _ c = c
    colorFrom p = if colorLuminance (pText p) > 0.5 then colorRGB 30 32 36 else colorRGB 252 252 250

-- ---------------------------------------------------------------------------
-- Threads

threadsHeader :: TimelineState -> Palette -> Timeline -> NanoUI ()
threadsHeader st pal tl = do
  paneMeta (fmtInt (M.size (tlThreads tl)))
  searchInputConfigured
    defaultSearchInputConfig {sicPlaceholder = "Filter by label or id", sicLayout = (alignMid . fixedW 220 . padXY 0 0) defaultLayout}
    (tsThreadFilter st)
  flex
  legendDot (pRun pal) "running"
  legendDot (pBlocked pal) "blocked"
  legendDot (pRunnable pal) "runnable"

threadsSection :: Env -> TimelineState -> Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> Marks -> NanoUI ()
threadsSection env st pal mm tl v sel marks = do
  do
    filt <- T.toLower <$> readVar (tsThreadFilter st)
    focus <- readVar (tsFocusThread st)
    (isFocus, isHover) <- isFocusOrHover env
    let rk = runKey (tlRun tl)
    let threads =
          [ ti
          | ti <- M.elems (tlThreads tl)
          , T.null filt || filt `T.isInfixOf` T.toLower (threadName ti)
          ]
        nRows = length threads
        contentH = fromIntegral nRows * threadRowH + 8
    scrollY0 <- readVar (tsThreadScroll st)
    mouse <- uiMousePos
    sid <- currentId
    prevRect <- fromMaybe (Rect 0 0 0 0) <$> lastRect sid
    let viewH = rectH prevRect
        maxScroll = max 0 (contentH - viewH)
        scrollY = max 0 (min maxScroll scrollY0)
        rowAt (Rect _ ry _ _) my = floor ((my - ry - 4 + scrollY) / threadRowH) :: Int
        hoverRow = if rectContains prevRect mouse then listToMaybe (drop (rowAt prevRect (v2Y mouse)) threads) else Nothing
        hoverText = do
          ti <- hoverRow
          let t = xToTime v (plotX prevRect) (plotW prevRect) (v2X mouse)
              tw = floorW t
              tol = round (4 * viewWidth v / realToFrac (plotW prevRect))
          if v2X mouse < plotX prevRect
            then Nothing
            else Just . (v2X mouse,v2Y mouse,) $
              ("t = " <> fmtNs (t - fromIntegral (tlStart tl)))
                : threadName ti
                : case pickSpan (tiTrack ti) tw tol of
                  Nothing -> ["not alive here"]
                  Just j ->
                    let s = trackSpan (tiTrack ti) j
                        dur = fmtNs (fromIntegral (srEnd s - srStart s))
                     in if
                          | srKind s == kindRun -> ["running " <> dur <> ", then " <> stopName (srStop s)]
                          | srKind s == kindBlocked -> [stopName (srStop s) <> " for " <> dur] ++ ownerLine tl (srAux s) ++ wakerLine tl ti s
                          | otherwise -> ["runnable for " <> dur <> " (" <> stopName (srStop s) <> ")"]
    resp <-
      canvasConfigured
        defaultCanvasConfig
          { canvasLayout = (fillW . fillH . minH 80) defaultLayout
          , canvasTrackPointer = True
          }
        $ \r@(Rect x y w h) -> do
          let firstRow = max 0 (floor (scrollY / threadRowH))
              visible = take (ceiling (h / threadRowH) + 1) (drop firstRow (zip [firstRow ..] threads))
              px = plotX r
              pw = plotW r
          drawGutter pal r
          withClip r $ forM_ visible $ \(i, ti) -> do
            let ry = y + 4 + fromIntegral i * threadRowH - scrollY
                rowRect = Rect px (ry + 3) pw (threadRowH - 6)
                focused = focus == Just (tiId ti) || isFocus (RThread rk (tiId ti))
            when (even i) $ drawRect (Rect x ry w threadRowH) (alpha 0.022 (pText pal))
            when focused $ drawRect (Rect x ry w threadRowH) (alpha 0.18 (pSelection pal))
            when (not focused && isHover (RThread rk (tiId ti))) $ drawRect (Rect x ry w threadRowH) (alpha 0.06 (pText pal))
            drawTextClipped (mmFont mm) (mmCharW mm) (V2 (x + 14) (ry + threadRowH / 2)) (gutterW - 26) (threadName ti) (if focused then pAccent pal else pText pal)
            drawTrack pal v rowRect (tiTrack ti) [(kindRun, pRun pal), (kindBlocked, pBlocked pal), (kindRunnable, pRunnable pal)] False
            case sel of
              Just (SelSpan (OnThread tid) k) | tid == tiId ti -> outlineSpan pal v rowRect (tiTrack ti) k
              _ -> pure ()
          -- Markers through the rows, the shared cursor and range, and a
          -- scroll indicator.
          withClip (Rect px y pw h) $ forM_ (markersIn tl v) $ \mk -> when (mkIsMarker mk) $ do
            let mx = timeToX v px pw (fromIntegral (mkTime mk))
            drawStroke (V2 mx y) (V2 mx (y + h)) 1 (alpha 0.35 (pMarker pal))
          drawMarks pal (linearAxis rk v px pw) (Rect px y pw h) marks {tmMarkers = []}
          when (contentH > h) $ do
            let barH = max 20 (h * h / contentH)
                barY = y + (h - barH) * (scrollY / max 1 (contentH - h))
            drawRoundedRect (Rect (x + w - 4) barY 3 barH) 2 (pGrid pal)
          for_ hoverText $ \(hx, hy, ls) -> do
            drawStroke (V2 hx y) (V2 hx (y + h)) 1 (pCursor pal)
            drawTooltip pal mm r hx hy ls
    let r = respRect resp
    -- The wheel scrolls rows here; Ctrl+wheel zooms time.
    inp <- askInput
    (_, wy) <- useWheelDeltaOn resp
    unless (modCtrl (inputModifiers inp)) $
      when (wy /= 0) $ writeVar (tsThreadScroll st) (max 0 (min maxScroll (scrollY - wy * threadRowH * 3)))
    brushing <- timeGestures env (linearAxis rk v (plotX r) (plotW r)) resp ScrubNone
    navigate st tl v resp (plotX r) (plotW r) False (not brushing)
    canvasLinks env resp (\(V2 _ my) -> RThread rk . tiId <$> listToMaybe (drop (rowAt r my) threads))
    clicked <- isClick st resp
    when (clicked && not brushing && not (modCtrl (inputModifiers inp)) && v2X mouse >= plotX r) $
      setCursor (envBus env) rk (round (max 0 (xToTime v (plotX r) (plotW r) (v2X mouse))))
    when (clicked && not brushing) $ do
      let row = drop (rowAt r (v2Y mouse)) threads
      case row of
        ti : _ -> do
          let t = xToTime v (plotX r) (plotW r) (v2X mouse)
              tol = round (4 * viewWidth v / realToFrac (plotW r))
          writeVar (tsFocusThread st) (Just (tiId ti))
          writeVar (tsSel st) (SelSpan (OnThread (tiId ti)) <$> pickSpan (tiTrack ti) (floorW t) tol)
        [] -> pure ()

ownerLine :: Timeline -> Word32 -> [Text]
ownerLine tl owner
  | owner == 0 = []
  | otherwise = ["(black hole owned by " <> threadLabel tl owner <> ")"]

-- | Who woke a thread out of a blocked span, for its hover text.
wakerLine :: Timeline -> ThreadInfo -> SpanRec -> [Text]
wakerLine tl ti s = case wokenBy ti s of
  Just (_, w) | w /= 0 -> ["woken by " <> threadLabel tl w]
  Just _ -> ["woken by the runtime"]
  Nothing -> []

legendDot :: Color -> Text -> NanoUI ()
legendDot c txt = rowWith (tight . gap 5 . alignMid) $ do
  _ <- canvas (fixedWH 9 9 . alignMid) (\r -> drawRoundedRect r 2 c)
  labelWith (alignMid . fontMuted . fontSizeScale 0.85) txt

-- ---------------------------------------------------------------------------
-- Inspector

inspector :: Env -> TimelineState -> Timeline -> TimeView -> Maybe Sel -> NanoUI ()
inspector env st tl v sel = case sel of
  Just (SelSpan ref i) -> scope $ spanInspector env st tl ref i
  Just (SelGC i) | i < V.length (tlGCs tl) -> scope $ gcInspector env st tl i (tlGCs tl V.! i)
  Just (SelRange a b) -> scope $ rangeInspector env st tl a b
  _ -> scope $ overview env st tl v

zoomTo :: TimelineState -> Timeline -> Word64 -> Word64 -> NanoUI ()
zoomTo st tl s e = do
  let pad = max 2000 (fromIntegral (e - s) * 1.5)
      lo = fromIntegral (tlStart tl)
      hi = fromIntegral (tlEnd tl)
  writeVar (tsView st) (Just (clampView lo hi (TimeView (fromIntegral s - pad) (fromIntegral e + pad))))

spanInspector :: Env -> TimelineState -> Timeline -> TrackRef -> Int -> NanoUI ()
spanInspector env st tl ref i = do
  let track = case ref of
        OnCap c -> tlCaps tl V.!? c
        OnThread tid -> tiTrack <$> M.lookup tid (tlThreads tl)
  case track of
    Just tr | i < trackLength tr -> do
      let s = trackSpan tr i
          dur = fromIntegral (srEnd s - srStart s) :: Double
          tid = srThread s
          kindName
            | srKind s == kindRun = "Running"
            | srKind s == kindGC = "Garbage collection"
            | srKind s == kindBlocked = "Blocked"
            | otherwise = "Runnable (waiting for a capability)"
      labelWith (fontMedium . fontSizeScale 1.05 . padBottom 4) kindName
      case ref of
        OnCap c -> kv "Capability" (tshow c)
        OnThread _ -> pure ()
      unless (srKind s == kindGC) $ kvLink env "Thread" (RThread rk tid) (threadLabel tl tid)
      kv "Starts at" (fmtNs (fromIntegral (srStart s - tlStart tl)))
      kv "Duration" (fmtNs dur)
      unless (srKind s == kindGC) $ kv (if srKind s == kindRun then "Ended because" else "Reason") (stopName (srStop s))
      scope $ when (srAux s /= 0) $ do
        prose env ["Waiting on a black hole being evaluated by ", refInline (RThread rk (srAux s)) (threadLabel tl (srAux s)), "."]
        whenM (button "Show owner thread") (writeVar (tsFocusThread st) (Just (srAux s)))
      scope $ for_ (M.lookup tid (tlThreads tl) >>= (`wokenBy` s)) $ \(at, w) ->
        if w /= 0
          then kvLink env "Woken by" (RThread rk w) (threadLabel tl w <> ", after " <> fmtNs (fromIntegral (at - srStart s)))
          else kv "Woken by" ("the runtime, after " <> fmtNs (fromIntegral (at - srStart s)))
      scope $ for_ (seriesRateBetween (tlAlloc tl) (srStart s) (srEnd s)) $ \rate ->
        kv "Allocation rate" (fmtRate rate <> " (whole process)")
      rowWith (padTop 4 . tight . gap 6) $ do
        whenM (button "Zoom to slice") (zoomTo st tl (srStart s) (srEnd s))
        scope $ unless (srKind s == kindGC) $ whenM (button "Highlight thread") (writeVar (tsFocusThread st) (Just tid))
      -- What the profiler saw running: on the capability, or wherever the
      -- thread ran.
      scope $ when (hasSamples && (srKind s == kindRun || srKind s == kindGC)) $ do
        separator
        sectionTitle "What ran during this slice?"
        let cp = case ref of
              OnCap c -> sliceProfile tl (Just c) tid (srStart s) (srEnd s)
              OnThread _ -> sliceProfile tl Nothing tid (srStart s) (srEnd s)
        codeRows env tl cp 8
        sampleNote tl cp
      scope $ for_ (if srKind s == kindGC then Nothing else M.lookup tid (tlThreads tl)) $ \ti -> do
        separator
        threadStats env tl ti
      separator
      hint $
        if hasSamples
          then "Samples show the cost centre on top of the stack. Ctrl+click one to open it in the .prof tree (when loaded) or its source in Core & STG."
          else "The eventlog records no stack traces. A profiled build (-prof -fprof-auto) run with +RTS -p -l samples what each capability runs, and this inspector then says what ran during a slice. Otherwise label threads (GHC.Conc.labelThread), drop traceMarkerIO calls, or step them in the Debugger."
    _ -> hint "The selection is no longer in the log."
  where
    rk = runKey (tlRun tl)
    hasSamples = not (V.null (tlProfSamples tl))

-- | Cost centres sampled, as links, with their samples and their share of
-- the samples that were not idle.
codeRows :: Env -> Timeline -> CodeProfile -> Int -> NanoUI ()
codeRows env tl cp n = do
  let counted = max 1 (cpSamples cp - cpIdle cp)
  forM_ (take n (cpTop cp)) $ \(i, k) -> withKey ("cc" <> tshow i) $ for_ (costCentreRef tl i) $ \ref -> do
    _ <- linkRow env ref False id $ do
      columnWith (tight . gap 0 . fillW . alignMid) $ do
        labelWith (fillW . fontMono . fontSizeScale 0.88) (costCentreName tl i)
        scope $ for_ (IM.lookup i (tlCostCentres tl) >>= srcLocSpan . cciSrc) $ \sp ->
          labelWith (fillW . fontMuted . fontSizeScale 0.8) (renderSrcSpan sp)
      numCell 40 id (fmtInt k)
      numCell 48 fontMuted (fmtPct (fromIntegral k / fromIntegral counted))
    pure ()
  scope $ when (cpIdle cp > 0) $
    labelWith (fontMuted . fontSizeScale 0.85) (fmtInt (cpIdle cp) <> " idle samples left out")

-- | Each ticky counter (most allocation first) with its parsed name and
-- the name key of the top-level binding it is nested in.
tickyNames :: Timeline -> [(TickyCounter, TickyName, Text)]
tickyNames tl =
  let named = [(c, tn) | c <- V.toList (tlTicky tl), Just tn <- [parseTickyName (tcName c)]]
      byUnique = M.fromList [(u, tn) | (_, tn) <- named, Just u <- [tickyUnique tn]]
   in [(c, tn, nameKey (tnName (tickyTopLevel (`M.lookup` byUnique) tn))) | (c, tn) <- named]

-- | The binding a counter belongs to.
tickyRef :: (TickyCounter, TickyName, Text) -> Ref
tickyRef (_, tn, key) = RBinding (tnModule tn) key

tickyNote :: Word64 -> Word64 -> Text
tickyNote entries alloc = fmtInt entries <> " entries · " <> fmtBytes alloc <> " alloc"

-- | Ticky counters with entries and allocation, as links to their
-- bindings.
tickyRows :: Env -> [((TickyCounter, TickyName, Text), Word64, Word64)] -> NanoUI ()
tickyRows env rows = do
  rowWith (padXY 8 0 . tight . gap 8 . fillW) $ do
    colHeader fillW "closure"
    colHeader (fixedW 64 . alignEnd) "entries"
    colHeader (fixedW 72 . alignEnd) "alloc"
  forM_ (zip [0 :: Int ..] rows) $ \(k, (x@(_, tn, key), e, al)) -> withKey ("t" <> tshow k) $ do
    _ <- linkRow env (tickyRef x) False id $ do
      columnWith (tight . gap 0 . fillW . alignMid) $ do
        labelWith (fillW . fontMono . fontSizeScale 0.88) (tickyBinder tn)
        labelWith (fillW . fontMuted . fontSizeScale 0.8) ("in " <> tnModule tn <> "." <> key)
      numCell 64 id (fmtInt e)
      numCell 72 id (fmtBytes al)
    pure ()

-- | How far to trust a handful of samples, in words.
sampleNote :: Timeline -> CodeProfile -> NanoUI ()
sampleNote tl cp
  | cpSamples cp == 0 = hint ("No sample fell inside it: each capability was sampled once every " <> period <> "." <> coarse)
  | fewSamples (cpSamples cp) =
      hint ("Only " <> fmtInt (cpSamples cp) <> " samples, one every " <> period <> " on each capability: a hint of what ran, not a measurement." <> coarse)
  | otherwise = pure ()
  where
    period = fmtNs (fromIntegral (tlSamplePeriod tl))
    coarse
      | tlSamplePeriod tl > 5000000 = " The profiler's timer ticked far slower than the 1 ms it asks for, as it does on Windows."
      | otherwise = ""

threadStats :: Env -> Timeline -> ThreadInfo -> NanoUI ()
threadStats env tl ti = do
  sectionTitle (threadName ti)
  let created = fromMaybe (tlStart tl) (tiCreated ti)
      ended = fromMaybe (tlEnd tl) (tiFinished ti)
      life = fromIntegral (max 1 (ended - created)) :: Double
  kv "Created" (maybe "before the log began" (\c -> fmtNs (fromIntegral (c - tlStart tl))) (tiCreated ti))
  kv "Finished" (maybe "still alive at the end" (\c -> fmtNs (fromIntegral (c - tlStart tl))) (tiFinished ti))
  kv "Times scheduled" (fmtInt (tiRuns ti))
  kv "Time running" (fmtNs (fromIntegral (tiRunTime ti)) <> "  (" <> fmtPct (fromIntegral (tiRunTime ti) / life) <> " of its life)")
  kv "Capabilities" (T.intercalate ", " (map tshow (tiCaps ti)))
  let off = sortOn (Down . snd) (M.toList (tiBlocked ti))
  unless (null off) $ do
    sectionTitle "Time off-CPU by reason"
    forM_ off $ \(code, ns) ->
      rowWith (tight . gap 8 . fillW) $ do
        cell (fillW . if stopIsBlocked code then fontTone Danger else fontMuted) (stopName code)
        numCell 84 id (fmtNs (fromIntegral ns))
        numCell 52 fontMuted (fmtPct (fromIntegral ns / life))
  -- Its CPU profile: the samples taken while it ran, wherever it ran.
  scope $ unless (V.null (tlProfSamples tl)) $ do
    let cp = threadProfile tl (tiId ti)
    sectionTitle ("Its CPU profile (" <> fmtInt (cpSamples cp) <> " samples)")
    codeRows env tl cp 6
    sampleNote tl cp

gcInspector :: Env -> TimelineState -> Timeline -> Int -> GCEvent -> NanoUI ()
gcInspector env st tl i g = do
  labelWith (fontMedium . fontSizeScale 1.05 . padBottom 4) ("Garbage collection" <> genText (gcGen g))
  let pause = fromIntegral (gcEnd g - gcStart g)
      rk = runKey (tlRun tl)
  kv "Starts at" (fmtNs (fromIntegral (gcStart g - tlStart tl)))
  kv "Pause" (fmtNs pause)
  kv "Copied" (fmtBytes (gcCopied g))
  kv "GC threads" (tshow (gcParThreads g))
  for_ (seriesValueAt (tlLive tl) (gcEnd g)) $ \b -> kv "Live after" (fmtBytes b)
  scope $ case censusAfterCollection tl i of
    Just j -> kvLink env "Heap after this collection" (RCensus rk j) ("census " <> tshow (j + 1))
    Nothing
      | isJust (tlHeapProfile tl) -> kv "Heap after this collection" "no census at this collection"
      | otherwise -> pure ()
  rowWith (padTop 4 . tight . gap 6) $ do
    whenM (button "Zoom to collection") (zoomTo st tl (gcStart g) (gcEnd g))
    whenM (button "Move the cursor here") (setCursor (envBus env) rk (gcStart g))
  hint $
    if gcGen g >= 1
      then "A major collection copies (or marks) the whole live heap: its pause grows with residency. Frequent major GCs with rising live bytes usually mean a space leak; see the Heap tab."
      else "A minor collection only scans the nursery. Many of them mean a high allocation rate; a larger -A shrinks their number."

overview :: Env -> TimelineState -> Timeline -> TimeView -> NanoUI ()
overview env st tl v = do
  let rk = runKey (tlRun tl)
      bus = envBus env
  hint "Click a slice on a capability, a thread row or the GC strip to inspect it. Shift+drag selects a range to analyse."
  kv "Window" (fmtNs (viewWidth v))
  cursor <- readVar (busCursor bus)
  scope $ case cursor of
    Just (r, t) | runKeysMatch r rk -> rowWith (fillW . tight . gap 8) $ do
      labelWith (alignBaseline . fontMuted) "Cursor"
      flex
      linkText env (RInstant rk t) alignBaseline (fmtNs (fromIntegral t - fromIntegral (tlStart tl)))
    _ -> pure ()
  range <- readVar (busRange bus)
  scope $ case range of
    Just (r, a, b) | runKeysMatch r rk -> do
      rowWith (fillW . tight . gap 8) $ do
        labelWith (alignBaseline . fontMuted) "Range"
        flex
        linkText env (RRange rk a b) alignBaseline (fmtNs (fromIntegral a - fromIntegral (tlStart tl)) <> " to " <> fmtNs (fromIntegral b - fromIntegral (tlStart tl)))
      rowWith (tight . gap 6) $ do
        whenM (button "Analyse range") (analyseRange env st tl a b)
        whenM (button "Clear range") (writeVar (busRange bus) Nothing)
    _ -> pure ()
  sectionTitle "Longest GC pauses"
  let gcs = take 8 (sortOn (\(_, g) -> Down (gcEnd g - gcStart g)) (zip [0 :: Int ..] (V.toList (tlGCs tl))))
  rowWith (padXY 8 0 . tight . gap 8 . fillW) $ do
    colHeader (fixedW 72 . alignEnd) "pause"
    colHeader fillW "collection"
    colHeader (fixedW 84 . alignEnd) "at"
  forM_ gcs $ \(i, g) -> withKey i $ do
    picked <- linkRow env (RCollection rk i) False id $ do
      numCell 72 id (fmtNs (fromIntegral (gcEnd g - gcStart g)))
      cell (fillW . fontMuted) (if gcGen g >= 1 then "major" else "minor")
      numCell 84 fontMuted (fmtNs (fromIntegral (gcStart g - tlStart tl)))
    when picked $ do
      writeVar (tsSel st) (Just (SelGC i))
      zoomTo st tl (gcStart g) (gcEnd g)
  sectionTitle "Most blocked threads"
  let blockedTime ti = sum [ns | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c]
      worst = take 8 (sortOn (Down . blockedTime) (filter ((> 0) . blockedTime) (M.elems (tlThreads tl))))
  forM_ worst $ \ti -> withKey (tiId ti) $ do
    let top = listToMaybe (sortOn (Down . snd) [(c, ns) | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c])
    picked <- linkRow env (RThread rk (tiId ti)) False id $ do
      columnWith (tight . gap 0 . fillW . alignMid) $ do
        labelWith fillW (threadName ti)
        scope $ for_ top $ \(c, _) -> labelWith (fillW . fontMuted . fontSizeScale 0.85) (stopName c)
      numCell 72 id (fmtNs (fromIntegral (blockedTime ti)))
    when picked (writeVar (tsFocusThread st) (Just (tiId ti)))
  sectionTitle "Busiest threads"
  forM_ (take 6 (sortOn (Down . tiRunTime) (M.elems (tlThreads tl)))) $ \ti -> withKey (tiId ti) $ do
    picked <- linkRow env (RThread rk (tiId ti)) False id $ do
      cell fillW (threadName ti)
      numCell 72 id (fmtNs (fromIntegral (tiRunTime ti)))
    when picked (writeVar (tsFocusThread st) (Just (tiId ti)))
  scope $ unless (V.null (tlProfSamples tl)) $ do
    let cp = codeProfile tl (const True) (tlStart tl) (tlEnd tl)
    sectionTitle ("Most sampled code (" <> fmtInt (cpSamples cp) <> " samples)")
    codeRows env tl cp 6
    sampleNote tl cp
  scope $ unless (V.null (tlTicky tl)) $ do
    sectionTitle ("Ticky counters (" <> fmtInt (V.length (tlTicky tl)) <> ")")
    tickyRows env (take 8 [(x, tcEntries c, tcAlloc c) | x@(c, _, _) <- tickyNames tl])
  sectionTitle "Program"
  forM_ (tlInfo tl) $ \(k, val) -> kv k (T.take 300 val)

-- | Show what happened in a range in the inspector, and make it the range
-- every time view shades.
analyseRange :: Env -> TimelineState -> Timeline -> Word64 -> Word64 -> NanoUI ()
analyseRange env st tl a b = do
  setRange (envBus env) (runKey (tlRun tl)) a b
  writeVar (tsSel st) (Just (SelRange a b))

-- | Zoom to a range, with a little room either side.
zoomToRange :: TimelineState -> Timeline -> Word64 -> Word64 -> NanoUI ()
zoomToRange st tl a b = do
  let pad = max 1000 (fromIntegral (b - a) * 0.05)
  writeVar (tsView st) (Just (clampView (fromIntegral (tlStart tl)) (fromIntegral (tlEnd tl)) (TimeView (fromIntegral a - pad) (fromIntegral b + pad))))

rangeInspector :: Env -> TimelineState -> Timeline -> Word64 -> Word64 -> NanoUI ()
rangeInspector env st tl a b = do
  let rk = runKey (tlRun tl)
      rs = rangeStats tl a b
      dur = fromIntegral (b - a) :: Double
      at t = fmtNs (fromIntegral t - fromIntegral (tlStart tl))
      gcs = tlGCs tl
      pause g = gcEnd g - gcStart g
  labelWith (fontMedium . fontSizeScale 1.05 . padBottom 4) "Range"
  kv "From" (at a)
  kv "To" (at b)
  kv "Duration" (fmtNs dur)
  rowWith (padTop 4 . tight . gap 6) $ do
    whenM (button "Zoom to range") (zoomToRange st tl a b)
    whenM (button "Clear range") (writeVar (busRange (envBus env)) Nothing >> writeVar (tsSel st) Nothing)
  sectionTitle "Collections"
  kv "Collections, major" (fmtInt (length (rsCollections rs)) <> ", " <> fmtInt (rsMajor rs))
  kv "Time paused" (fmtNs (fromIntegral (rsPauseTotal rs)) <> "  (" <> fmtPct (fromIntegral (rsPauseTotal rs) / max 1 dur) <> ")")
  scope $ for_ (rsLongest rs >>= \i -> (i,) <$> gcs V.!? i) $ \(i, g) ->
    kvLink env "Longest pause" (RCollection rk i) (fmtNs (fromIntegral (pause g)) <> " at " <> at (gcStart g))
  let worst = take 6 (sortOn (\(_, g) -> Down (pause g)) [(i, g) | i <- rsCollections rs, Just g <- [gcs V.!? i]])
  scope $ unless (length worst < 2) $ forM_ worst $ \(i, g) -> withKey i $ do
    picked <- linkRow env (RCollection rk i) False id $ do
      numCell 72 id (fmtNs (fromIntegral (pause g)))
      cell (fillW . fontMuted) (if gcGen g >= 1 then "major" else "minor")
      numCell 84 fontMuted (at (gcStart g))
    when picked (writeVar (tsSel st) (Just (SelGC i)))
  sectionTitle "Allocation"
  scope $ case rsAllocated rs of
    Just n -> do
      kv "Allocated" (fmtBytes n)
      kv "Rate" (fmtRate (fromIntegral n / max 1e-9 (dur / 1e9)))
    Nothing -> hint "No allocation counter in this range. The RTS posts it at each collection."
  scope $ unless (V.null (tlTicky tl)) $ do
    sectionTitle "Top allocators (ticky)"
    let named = M.fromList [(tcId c, x) | x@(c, _, _) <- tickyNames tl]
        rows = [(x, e, al) | (c, e, al) <- tickyBetween tl a b, Just x <- [M.lookup (tcId c) named]]
    scope $ when (null rows) $ hint "No ticky sample in this range: the RTS posts them at each major collection."
    tickyRows env (take 8 rows)
  scope $ unless (V.null (tlProfSamples tl)) $ do
    sectionTitle "Code (cost-centre samples)"
    let cp = codeProfile tl (const True) a b
    codeRows env tl cp 8
    sampleNote tl cp
    hint "Samples count where the time went, not what was allocated: a ticky build (-ticky, +RTS -lT) counts allocation per closure."
  sectionTitle "Heap"
  let growth name (sa, sb) = for_ ((,) <$> sa <*> sb) $ \(va, vb) ->
        kv name (fmtBytes va <> " → " <> fmtBytes vb <> "  (" <> (if vb >= va then "+" <> fmtBytes (vb - va) else "−" <> fmtBytes (va - vb)) <> ")")
  growth "Live" (rsLive rs)
  growth "Heap size" (rsHeap rs)
  scope $ forM_ (zip [0 :: Int ..] (take 12 (rsCensuses rs))) $ \(k, j) -> withKey k $
    for_ (censusTimes tl VU.!? j) $ \t -> do
      picked <- linkRow env (RCensus rk j) False id $ do
        cell fillW ("census " <> tshow (j + 1))
        numCell 84 fontMuted (at t)
      when picked (setCursor (envBus env) rk t)
  sectionTitle ("Threads that ran (" <> fmtInt (length (rsThreads rs)) <> ")")
  forM_ (take 12 (rsThreads rs)) $ \(tid, ns) -> withKey tid $ do
    picked <- linkRow env (RThread rk tid) False id $ do
      cell fillW (threadLabel tl tid)
      numCell 72 id (fmtNs (fromIntegral ns))
      numCell 52 fontMuted (fmtPct (fromIntegral ns / max 1 dur))
    when picked (writeVar (tsFocusThread st) (Just tid))
  scope $ unless (null (rsMarkers rs)) $ do
    sectionTitle ("Markers (" <> fmtInt (length (rsMarkers rs)) <> ")")
    forM_ (take 40 (rsMarkers rs)) $ \i -> withKey ("m" <> tshow i) $ for_ (tlMarkers tl V.!? i) $ \mk -> do
      picked <- linkRow env (RMarker rk i) False id $ do
        numCell 84 fontMuted (at (mkTime mk))
        cell (fillW . if mkIsMarker mk then id else fontMuted) (mkText mk)
      when picked (setCursor (envBus env) rk (mkTime mk))

-- | @[@ and @]@ move the cursor to the previous or next collection; with
-- Shift, census. The view follows when the cursor leaves it.
stepCursor :: Env -> TimelineState -> Timeline -> TimeView -> NanoUI ()
stepCursor env st tl v = do
  (dir, shifted) <- stepKeys
  when (dir /= 0) $ do
    let bus = envBus env
        rk = runKey (tlRun tl)
    cursor <- readVar (busCursor bus)
    let from = case cursor of
          Just (r, t) | runKeysMatch r rk -> t
          _ -> if dir > 0 then floorW (tvStart v) else ceiling (tvEnd v)
        stops = if shifted then censusTimes tl else VU.fromList (map gcStart (V.toList (tlGCs tl)))
    for_ (stepTime dir stops from) $ \t -> do
      setCursor bus rk t
      unless (fromIntegral t >= tvStart v && fromIntegral t <= tvEnd v) (centreOn st tl t)

-- ---------------------------------------------------------------------------
-- Markers

markersHeader :: TimelineState -> Timeline -> NanoUI ()
markersHeader st tl = do
  paneMeta (fmtInt (V.length (V.filter mkIsMarker (tlMarkers tl))))
  searchInputConfigured
    defaultSearchInputConfig {sicPlaceholder = "Filter", sicLayout = (alignMid . fixedW 110 . padXY 0 0) defaultLayout}
    (tsMarkerFilter st)
  flex
  checkboxWith alignMid "Events" (tsMarkerEvents st)

-- | Every marker in the log, as links; clicking one moves the cursor to it.
markersList :: Env -> TimelineState -> Timeline -> NanoUI ()
markersList env st tl = do
  filt <- T.toLower <$> readVar (tsMarkerFilter st)
  events <- readVar (tsMarkerEvents st)
  let rk = runKey (tlRun tl)
      rows =
        [ (i, mk)
        | (i, mk) <- zip [0 :: Int ..] (V.toList (tlMarkers tl))
        , events || mkIsMarker mk
        , T.null filt || filt `T.isInfixOf` T.toLower (mkText mk)
        ]
  if null rows
    then columnWith (padAll 12 . fillW) $
      hint $
        if V.null (tlMarkers tl)
          then "No markers in this log. Debug.Trace.traceMarkerIO posts one, and it shows on every time axis."
          else "No marker matches."
    else paneScroll (grow . tight) $ columnWith (padXY 4 4 . tight . fillW . gap 0) $
      forM_ (take 2000 rows) $ \(i, mk) -> withKey i $ do
        picked <- linkRow env (RMarker rk i) False id $ do
          numCell 76 fontMuted (fmtNs (fromIntegral (mkTime mk) - fromIntegral (tlStart tl)))
          cell (fillW . if mkIsMarker mk then id else fontMuted) (mkText mk)
        when picked $ do
          setCursor (envBus env) rk (mkTime mk)
          centreOn st tl (mkTime mk)

-- ---------------------------------------------------------------------------
-- Lens

-- | What the timeline can show of the entities other panels name: threads,
-- collections, censuses and markers of its run, and instants and ranges in
-- it. @switch@ brings the Timeline to the front.
timelineLens :: Bus -> NanoUI () -> TimelineState -> Maybe LoadProgress -> Lens
timelineLens bus switch st mprog = case mprog >>= lpTimeline of
  Nothing -> lens PTimeline
  Just tl ->
    let rk = runKey (tlRun tl)
        mine = runKeysMatch rk
        inLog t = t >= tlStart tl && t <= tlEnd tl
        thread r t = if mine r then M.lookup t (tlThreads tl) else Nothing
        gc r i = if mine r then tlGCs tl V.!? i else Nothing
        marker r i = if mine r then tlMarkers tl V.!? i else Nothing
        -- A census index means this log's censuses only when the ref names
        -- this run exactly: a .hp file's censuses count differently.
        census r j = if r == rk then censusTimes tl VU.!? j else Nothing
        offset t = fmtNs (fromIntegral (t - min t (tlStart tl)))
        -- Cost centres the profiler sampled on top of a stack.
        sampled m l = costCentreNamed tl m l >>= \c -> (c,) <$> IM.lookup (cciId c) (tlSampleCounts tl)
        totalSamples = V.length (tlProfSamples tl)
        ticky = tickyNames tl
        tickyOf m k = [x | x@(_, tn, key) <- ticky, key == k, T.null m || tnModule tn == m]
        -- Each counter's entries and allocation, for the STG pane and the
        -- binding list: placed at its binder's STG let ("siteOfTable"),
        -- else joined by the binding's name.
        tickyAnns =
          concat
            [ [ Annotation Nothing table name LaneEntries (fromIntegral (tcEntries c)) ShadeInk "" ref
              , Annotation Nothing table name LaneTicky (fromIntegral (tcAlloc c)) ShadeInk (tickyNote (tcEntries c) (tcAlloc c)) ref
              ]
            | x@(c, tn, key) <- ticky
            , tcEntries c > 0 || tcAlloc c > 0
            , let table = Just (tickyBinder tn <> "_info")
                  name = Just (tnModule tn, key)
                  ref = Just (tickyRef x)
            ]
     in (lens PTimeline)
          { lensShows = \case
              RThread r t -> isJust (thread r t)
              RCollection r i -> isJust (gc r i)
              RMarker r i -> isJust (marker r i)
              RCensus r j -> isJust (census r j)
              RInstant r t -> mine r && inLog t
              RRange r a b -> mine r && inLog a && b > a
              RCostCentre m l -> isJust (sampled m l)
              _ -> False
          , lensReveal = revealIn bus st tl
          , lensTitle = \case
              RThread r t -> threadName <$> thread r t
              RCollection r i -> (\g -> "Collection #" <> tshow (i + 1) <> genText (gcGen g)) <$> gc r i
              RMarker r i -> (\mk -> (if mkIsMarker mk then "Marker " else "Event ") <> T.take 60 (mkText mk)) <$> marker r i
              RInstant r t | mine r -> Just ("t = " <> offset t)
              RRange r a b | mine r -> Just ("Range " <> offset a <> " to " <> offset b)
              RCostCentre m l -> (\(c, _) -> cciModule c <> "." <> cciLabel c) <$> sampled m l
              _ -> Nothing
          , lensRelated = \case
              RCollection r i | isJust (gc r i) -> [("Heap after this collection", RCensus rk j) | Just j <- [censusAfterCollection tl i]]
              RCensus r j | isJust (census r j) -> [("Show collection", RCollection rk i) | Just i <- [collectionBeforeCensus tl j]]
              RCostCentre m l -> [("Go to source", RSource sp) | Just (c, _) <- [sampled m l], Just sp <- [srcLocSpan (cciSrc c)]]
              _ -> []
          , lensActions = \case
              RRange r a b | mine r, inLog a, b > a -> [Action "Analyse range" (Right (writeVar (busRange bus) (Just (rk, a, b)) >> writeVar (tsSel st) (Just (SelRange a b)) >> switch))]
              _ -> []
          , lensPeek = \case
              RThread r t -> threadPeek tl <$> thread r t
              RCollection r i -> gcPeek tl <$> gc r i
              RMarker r i -> (\mk -> fact "at" (offset (mkTime mk))) <$> marker r i
              RCensus r j -> (\t -> fact "taken at" (offset t)) <$> census r j
              RInstant r t | mine r, inLog t -> Just (instantPeek tl t)
              RRange r a b | mine r, inLog a, b > a -> Just (rangePeek tl a b)
              RCostCentre m l -> (\(c, n) -> codePeek tl c n totalSamples) <$> sampled m l
              RBinding m k | xs@(_ : _) <- tickyOf m k -> Just $ do
                fact "ticky" (tickyNote (sum [tcEntries c | (c, _, _) <- xs]) (sum [tcAlloc c | (c, _, _) <- xs]))
                fact "counters" (fmtInt (length xs) <> ", joined by name")
              _ -> Nothing
          , lensHits =
              [ Hit "thread" (threadName ti) ("ran " <> fmtNs (fromIntegral (tiRunTime ti))) (HitRef (RThread rk (tiId ti)))
              | ti <- M.elems (tlThreads tl)
              ]
                ++ [ Hit (if mkIsMarker mk then "marker" else "event") (mkText mk) (fmtNs (fromIntegral (mkTime mk - min (mkTime mk) (tlStart tl)))) (HitRef (RMarker rk i))
                   | (i, mk) <- take 5000 (zip [0 ..] (V.toList (tlMarkers tl)))
                   ]
                ++ [ Hit "sampled code" (cciModule c <> "." <> cciLabel c) (fmtInt n <> " samples") (HitRef (RCostCentre (cciModule c) (cciLabel c)))
                   | (i, n) <- IM.toList (tlSampleCounts tl)
                   , Just c <- [IM.lookup i (tlCostCentres tl)]
                   , not (isIdleCostCentre c)
                   ]
                ++ [ Hit "ticky counter" (tickyBinder tn) (tickyNote (tcEntries c) (tcAlloc c)) (HitRef (tickyRef x))
                   | x@(c, tn, _) <- ticky
                   ]
          , lensAnnotations = tickyAnns
          , lensSpan = \case
              RCostCentre m l -> sampled m l >>= srcLocSpan . cciSrc . fst
              _ -> Nothing
          }

revealIn :: Bus -> TimelineState -> Timeline -> Ref -> NanoUI ()
revealIn bus st tl = \case
  RThread _ t -> do
    writeVar (tsFocusThread st) (Just t)
    writeVar (tsThreadFilter st) ""
    let row = M.findIndex t (tlThreads tl)
    writeVar (tsThreadScroll st) (max 0 (fromIntegral (row - 3) * threadRowH))
  RCollection _ i -> for_ (tlGCs tl V.!? i) $ \g -> do
    writeVar (tsSel st) (Just (SelGC i))
    zoomTo st tl (gcStart g) (gcEnd g)
  RMarker _ i -> for_ (tlMarkers tl V.!? i) $ \mk -> do
    setCursor bus (runKey (tlRun tl)) (mkTime mk)
    centreOn st tl (mkTime mk)
  RCensus _ j -> for_ (censusTimes tl VU.!? j) $ \t -> do
    setCursor bus (runKey (tlRun tl)) t
    centreOn st tl t
  RInstant _ t -> centreOn st tl t
  RRange _ a b -> do
    zoomToRange st tl a b
    writeVar (tsSel st) (Just (SelRange a b))
  -- The focus lights its runs up on the code tracks.
  RCostCentre _ _ -> writeVar (tsCode st) True
  _ -> pure ()

-- | Move the view to put @t@ in the middle, keeping its width (or a
-- twentieth of the log, when the whole log is showing).
centreOn :: TimelineState -> Timeline -> Word64 -> NanoUI ()
centreOn st tl t = do
  mv <- readVar (tsView st)
  let lo = fromIntegral (tlStart tl)
      hi = fromIntegral (tlEnd tl)
      w = maybe ((hi - lo) / 20) viewWidth mv
      c = fromIntegral t
  writeVar (tsView st) (Just (clampView lo hi (TimeView (c - w / 2) (c + w / 2))))

threadPeek :: Timeline -> ThreadInfo -> NanoUI ()
threadPeek tl ti = do
  let created = fromMaybe (tlStart tl) (tiCreated ti)
      ended = fromMaybe (tlEnd tl) (tiFinished ti)
      life = fromIntegral (max 1 (ended - created)) :: Double
      offCpu = [(c, ns) | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c]
      blocked = sum (map snd offCpu)
      top = listToMaybe (sortOn (Down . snd) offCpu)
  fact "ran" (fmtNs (fromIntegral (tiRunTime ti)) <> ", " <> fmtPct (fromIntegral (tiRunTime ti) / life) <> " of its life")
  scope $ when (blocked > 0) $
    fact "blocked" (fmtNs (fromIntegral blocked) <> maybe "" (\(c, _) -> ", mostly " <> stopName c) top)
  fact "capabilities" (T.intercalate ", " (map tshow (tiCaps ti)))

gcPeek :: Timeline -> GCEvent -> NanoUI ()
gcPeek tl g = do
  fact "pause" (fmtNs (fromIntegral (gcEnd g - gcStart g)))
  fact "at" (fmtNs (fromIntegral (gcStart g - min (gcStart g) (tlStart tl))))
  fact "copied" (fmtBytes (gcCopied g))
  for_ (seriesValueAt (tlLive tl) (gcEnd g)) $ \b -> fact "live after" (fmtBytes b)

rangePeek :: Timeline -> Word64 -> Word64 -> NanoUI ()
rangePeek tl a b = do
  let rs = rangeStats tl a b
  fact "lasts" (fmtNs (fromIntegral (b - a)))
  fact "collections" (fmtInt (length (rsCollections rs)) <> ", paused " <> fmtNs (fromIntegral (rsPauseTotal rs)))
  for_ (rsAllocated rs) $ \n -> fact "allocated" (fmtBytes n)
  fact "threads ran" (fmtInt (length (rsThreads rs)))

codePeek :: Timeline -> CostCentreInfo -> Int -> Int -> NanoUI ()
codePeek tl c n total = do
  fact "on top of the stack" (fmtInt n <> " of " <> fmtInt total <> " samples, one every " <> fmtNs (fromIntegral (tlSamplePeriod tl)))
  scope $ for_ (srcLocSpan (cciSrc c)) $ \sp -> fact "source" (renderSrcSpan sp)
  scope $ when (fewSamples n) $ labelWith (fontMuted . fontSizeScale 0.85 . fillW) "Few samples: a hint, not a measurement."

instantPeek :: Timeline -> Word64 -> NanoUI ()
instantPeek tl t = do
  for_ (seriesValueAt (tlLive tl) t) $ \b -> fact "live" (fmtBytes b)
  for_ (seriesValueAt (tlHeapSize tl) t) $ \b -> fact "heap" (fmtBytes b)
