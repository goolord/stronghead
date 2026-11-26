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
  , timelinePanes
  , openEventlog
  , timelineLens
  , timelineSync
  , markersHeader
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, void, when)
import Data.Foldable (for_)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn, unsnoc)
import Data.List.Extra (minimumOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Word (Word32, Word64)
import System.FilePath (takeFileName)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import NanoUI hiding (selectableRow)
import NanoUI.Backend (colorToWord32)
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)
import NanoUI.Path qualified as P

import Stronghead.Clock (stepTime)
import Stronghead.Diff (Baseline (..), fmtChange)
import Stronghead.Dump (nameKey)
import Stronghead.Eventlog
import Stronghead.Eventlog.CriticalPath
import Stronghead.Eventlog.Model
import Stronghead.Format
import Stronghead.HeapProfile (HeapProfile (..))
import Stronghead.Join (Annotation (..), Lane (..), Shade (..), TickyName (..), parseTickyName, tickyBinder, tickyTopLevel, tickyUnique)
import Stronghead.Next (Fact (..))
import Stronghead.Ref
import Stronghead.Run (runKey, runKeysMatch)
import Stronghead.Source (renderSrcLoc, renderSrcSpan, srcLocSpan)
import UI.Clock
import UI.Common
import UI.Flamegraph (latencyHeatmapView)
import UI.Link
import UI.Workspace

data TrackRef = OnCap !Int | OnThread !Word32
  deriving stock (Eq, Show)

data Sel
  = SelSpan !TrackRef !Int
  | SelGC !Int
  | SelRange !Word64 !Word64
  deriving stock (Eq, Show)

data ThreadSort = SortById | SortByActive | SortByBlocked
  deriving stock (Eq, Show, Enum, Bounded)

threadSortName :: ThreadSort -> Text
threadSortName = \case
  SortById -> "ID"
  SortByActive -> "Active"
  SortByBlocked -> "Blocked"

data TimelineState = TimelineState
  { tsPath :: !(Var (Maybe FilePath))
  , tsFollow :: !(Var Bool)
  , tsStream :: !(Stream (FilePath, Bool) LoadProgress)
  , tsView :: !(Var (Maybe TimeView))
    -- ^ 'Nothing' fits the whole log (and keeps fitting while following).
  , tsSel :: !(Var (Maybe Sel))
  , tsThreadScroll :: !(Var Float)
  , tsThreadFilter :: !(Var Text)
  , tsThreadSort :: !(Var ThreadSort)
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
  , tsCached :: !(IORef (Maybe ((Word64, Word64, Int), TimelineCached)))
  , tsCritCache :: !(IORef (Maybe ((Int, Word32, Word64, Word64), CriticalPathResult)))
  }

data TimelineCached = TimelineCached
  { tlcSummary :: !Summary
  , tlcTopGCs :: ![(Int, GCEvent)]
  , tlcWorstBlocked :: ![(ThreadInfo, Word64, Maybe (StopCode, Word64))]
  , tlcBusiest :: ![ThreadInfo]
  , tlcCodeProfile :: ~CodeProfile
  , tlcTicky :: ~[(TickyCounter, TickyName, Text)]
  , tlcGCStarts :: !(VU.Vector Word64)
  , tlcRunnable :: ~RunnableStats
  , tlcGCHeatmap :: ~LatencyHeatmap
  , tlcRunHeatmap :: ~LatencyHeatmap
  , tlcStmContention :: ~StmContentionReport
  , tlcContentionGraph :: ~ContentionGraph
  }

newTimelineState :: IO TimelineState
newTimelineState = do
  tsPath <- newVar Nothing
  tsFollow <- newVar False
  tsStream <- newStream
  tsView <- newVar Nothing
  tsSel <- newVar Nothing
  tsThreadScroll <- newVar 0
  tsThreadFilter <- newVar ""
  tsThreadSort <- newVar SortById
  tsPressAt <- newVar Nothing
  tsDialog <- newVar Nothing
  tsFocusThread <- newVar Nothing
  tsPanes <- newWorkbench "timeline" (panelPreset PTimeline)
  tsScrub <- newVar False
  tsMarkerFilter <- newVar ""
  tsMarkerEvents <- newVar False
  tsWindowSeen <- newIORef Nothing
  tsCode <- newVar True
  tsCached <- newIORef Nothing
  tsCritCache <- newIORef Nothing
  pure TimelineState {..}

cachedTimeline :: TimelineState -> Timeline -> IO TimelineCached
cachedTimeline st tl = do
  let key = (tlStart tl, tlEnd tl, tlEventCount tl)
  cached <- readIORef (tsCached st)
  case cached of
    Just (k, c) | k == key -> pure c
    _ -> do
      let blockedTime ti = sum [ns | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c]
          topBlock ti = listToMaybe (sortOn (Down . snd) [(c, ns) | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c])
          c =
            TimelineCached
              { tlcSummary = summarize tl
              , tlcTopGCs = take 8 (sortOn (\(_, g) -> Down (gcEnd g - gcStart g)) (zip [0 :: Int ..] (V.toList (tlGCs tl))))
              , tlcWorstBlocked =
                  take 8 $
                    sortOn (\(_, bt, _) -> Down bt) $
                      [(ti, bt, topBlock ti) | ti <- M.elems (tlThreads tl), let bt = blockedTime ti, bt > 0]
              , tlcBusiest = take 6 (sortOn (Down . tiRunTime) (M.elems (tlThreads tl)))
              , tlcCodeProfile = codeProfile tl (const True) (tlStart tl) (tlEnd tl)
              , tlcTicky = tickyNames tl
              , tlcGCStarts = VU.fromListN (V.length (tlGCs tl)) [gcStart g | g <- V.toList (tlGCs tl)]
              , tlcRunnable = runnableStats tl
              , tlcGCHeatmap = gcPauseHeatmap tl 36 10
              , tlcRunHeatmap = runnableHeatmap tl 36 10
              , tlcStmContention = analyzeStmContention tl
              , tlcContentionGraph = buildContentionGraph tl
              }
      writeIORef (tsCached st) (Just (key, c))
      pure c

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

timelineView :: Env -> Bench -> TimelineState -> Maybe LoadProgress -> NanoUI ()
timelineView env bench st mprog = columnWith (tight . gap 0 . grow) $ do
  useFileDialog (tsDialog st) . firstPath $ \p -> openEventlog st p False
  path <- readVar (tsPath st)
  let loaded = mprog >>= lpTimeline
  let askOpenLog =
        writeVar (tsDialog st) . Just
          =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "GHC eventlog" "eventlog", FileFilter "All files" "*"]}
  toolStrip $ do
    whenM (toolIconButton iconFolderOpen "Open eventlog…") askOpenLog
    checkbox "Follow as it is written" (tsFollow st)
    flex
    scope $ for_ loaded $ \tl -> do
      mv <- readVar (tsView st)
      let lo = fromIntegral (tlStart tl)
          hi = fromIntegral (tlEnd tl)
          v = maybe (TimeView lo hi) (clampView lo hi) mv
      let at t = if t - lo < 0.5 then "0" else fmtNs (t - lo)
      toolLabel ("Showing " <> at (tvStart v) <> " to " <> at (tvEnd v))
      disabledWhen (isNothing mv) $ whenM (toolIconButton iconMaximize "Fit to log") (writeVar (tsView st) Nothing)
  mixed <- bnMixed bench (tsPanes st)
  let drawn = scope (bnDraw bench (tsPanes st) (timelinePanes env st mprog))
  case loaded of
    _ | mixed && maybe True (V.null . tlCaps) loaded -> drawn
    Nothing -> scope $ case path of
      Just p -> emptyState ("Reading " <> T.pack (takeFileName p)) ["The timeline appears as soon as the first scheduler events are decoded."] []
      Nothing ->
        emptyStateWith
          "Open an eventlog"
          [ "Open a .eventlog file, drag and drop one onto the window, or pass it on the command line. Record one by running a -threaded program with +RTS -l:"
          ]
          ["ghc -threaded -rtsopts -eventlog Main.hs", "./Main +RTS -l -N4 -hT -RTS"]
          (whenM (styled primary (toolIconButton iconFolderOpen "Open eventlog…")) askOpenLog)
    Just tl
      | V.null (tlCaps tl) ->
          scope $ emptyState "No scheduler events yet" ["This eventlog has no capability events. Was the program run with +RTS -l?"] []
      | otherwise -> scope (body env bench st tl mprog)

body :: Env -> Bench -> TimelineState -> Timeline -> Maybe LoadProgress -> NanoUI ()
body env bench st tl mprog = do
  tlc <- liftIO (cachedTimeline st tl)
  let lo = fromIntegral (tlStart tl)
      hi = fromIntegral (tlEnd tl) :: Double
      full = TimeView lo hi
      summary = tlcSummary tlc
  mv <- readVar (tsView st)
  let v = maybe full (clampView lo hi) mv
  stepCursor env st tl (tlcGCStarts tlc) v
  -- Against the baseline run, when one is marked.
  let base = envBaseline env >>= baSummary
      vs f = (\b -> fmtChange (f b) (f summary) <> " vs baseline") <$> base
      rate sm = fromIntegral (smAllocated sm) / max 1e-9 (smDuration sm)
      mutTone
        | smMutatorFrac summary < 0.50 = Just Danger
        | otherwise = Nothing
      gcTone
        | smGCFrac summary >= 0.25 = Just Danger
        | otherwise = Nothing
      pauseTone
        | smGCPauseMax summary >= 100e6 = Just Danger
        | otherwise = Nothing
  columnWith (padBottom 4 . tight . fillW) $
    readoutToned
      [ ReadoutItem "duration" (fmtSec (smDuration summary)) (vs smDuration) Nothing id
      , ReadoutItem "mutator" (fmtPct (smMutatorFrac summary)) (vs smMutatorFrac) mutTone id
      , ReadoutItem "GC time" (fmtPct (smGCFrac summary)) (vs smGCFrac) gcTone id
      , ReadoutItem ("collections, " <> fmtInt (smMajorGCs summary) <> " major") (fmtInt (smGCCount summary)) (vs (fromIntegral . smGCCount)) Nothing (fixedW 144)
      , ReadoutItem "longest pause" (fmtNs (smGCPauseMax summary)) (vs smGCPauseMax) pauseTone id
      , ReadoutItem "allocated" (fmtBytes (smAllocated summary)) (vs (fromIntegral . smAllocated)) Nothing id
      , ReadoutItem "allocation rate" (fmtRate (rate summary)) (vs rate) Nothing id
      , ReadoutItem "peak live" (fmtBytes (smPeakLive summary)) (vs (fromIntegral . smPeakLive)) Nothing id
      , ReadoutItem "threads" (fmtInt (smThreads summary)) Nothing Nothing id
      , ReadoutItem "events" (fmtInt (tlEventCount tl)) Nothing Nothing id
      ]
  bnDraw bench (tsPanes st) (timelinePanes env st mprog)

-- | The Timeline's panes, for whichever workbench holds them: the
-- capability tracks, the thread rows, the overview of the selection and
-- the markers. Without a log each says how to get one.
timelinePanes :: Env -> TimelineState -> Maybe LoadProgress -> [Word64] -> NanoUI PaneDraw
timelinePanes env st mprog _ = case mprog >>= lpTimeline of
  Just tl | not (V.null (tlCaps tl)) -> do
    th <- uiTheme
    let pal = palette th
        lo = fromIntegral (tlStart tl)
        hi = fromIntegral (tlEnd tl) :: Double
    mv <- readVar (tsView st)
    let v = maybe (TimeView lo hi) (clampView lo hi) mv
    marks <- marksFor env (runKey (tlRun tl)) (Just tl)
    mm <- monoMetrics 0.85
    sel <- readVar (tsSel st)
    pure $ \pid ctx ->
      if
        | pid == paneTlCaps ->
            Just (paneWith tight ctx "Capabilities" (capsLegend st pal tl) (mainCanvas env st pal mm tl v sel marks))
        | pid == paneTlThreads ->
            Just (paneWith tight ctx "Threads" (threadsHeader st pal tl) (threadsSection env st pal mm tl v sel marks))
        | pid == paneTlMarkers ->
            Just (paneWith tight ctx "Markers" (markersHeader st tl) (markersList env st tl))
        | pid == paneTlOverview -> Just $ do
            let title = case sel of
                  Nothing -> "Overview"
                  Just (SelGC _) -> "Collection"
                  Just (SelSpan _ _) -> "Slice"
                  Just (SelRange _ _) -> "Range"
            paneWith tight ctx title (inspectorActions env st tl sel marks) $
              paneScroll (grow . tight) $
                columnWith (padLRTB 12 12 10 20 . gap 6 . fillW) (inspector env st tl v sel)
        | otherwise -> Nothing
  _ -> pure $ \pid ctx ->
    if pid `elem` [paneTlCaps, paneTlThreads, paneTlOverview, paneTlMarkers]
      then Just (paneEmpty ctx (paneTitle pid) "No eventlog with scheduler events is open. Open one in the Timeline, or connect to a program run with +RTS -l." [])
      else Nothing

capsLegend :: TimelineState -> Palette -> Timeline -> NanoUI ()
capsLegend st pal tl = do
  scope $ unless (V.null (tlProfSamples tl)) $
    checkbox ("Code (" <> fmtInt (V.length (tlProfSamples tl)) <> " samples)") (tsCode st)
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
    whenM (buttonWith (padXY 8 2 . tight) "Analyse range") (analyseRange env st tl a b)
  focus <- readVar (tsFocusThread st)
  scope $ when (isJust focus) $
    whenM (buttonWith (padXY 8 2 . tight) "Clear highlight") (writeVar (tsFocusThread st) Nothing)
  scope $ when (isJust sel) $
    whenM (buttonWith (padXY 8 2 . tight) "Back to overview") (writeVar (tsSel st) Nothing)

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
    setView (zoomAround (wheelZoom wy) (xToTime v plotX plotW (v2X mouse)) v)
  when (wx /= 0) $ setView (wheelPan wx v)
  drag <- useDrag2DOn resp
  let dx = v2X (dragDelta drag)
  when (pan && dragActive drag && dx /= 0) $ setView (dragPan plotW dx v)
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
        { canvasLayout = fillW . fillH . minH (mainHeight nCaps code)
        , canvasTrackPointer = True
        , canvasCursor = Just (\_ _ _ -> UiCursorCrosshair)
        }
      (\r -> drawMain pal mm tl v sel marks code focusCC (if rectContains r mouse then (\(hx, _, _) -> hx) <$> hover else Nothing) r)
  cursorTooltip resp (if respHovered resp then (\(_, _, ls) -> ls) <$> hover else Nothing)
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
  -- On the ruler, a moment Stronghead stamped (a mark, a snapshot) is an
  -- instant to link to.
  let momentHere (V2 mx my) = if my >= ry && my < ry + rh then momentAt axis marks mx else Nothing
  canvasLinks env resp (\m -> maybe (refAtMain tl v rows m) Just (momentHere m))
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
        near -> Just (snd (minimumOn fst near))

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
       in VU.length (pointsIn (if tw > tol then tw - tol else 0) (tw + tol) ps)
    gcDetails = case pickGC tl tw tol of
      Nothing -> ["no collection here"]
      Just i ->
        let g = tlGCs tl V.! i
         in [ "GC #" <> tshow (i + 1) <> genText (gcGen g) <> ", pause " <> fmtNs (fromIntegral (gcEnd g - gcStart g))
            , "copied " <> fmtBytes (gcCopied g) <> ", " <> tshow (gcParThreads g) <> " GC threads"
            ]
    memDetails =
      let win = round (viewWidth v / 50)
       in catMaybes
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

genText :: Int -> Text
genText g
  | g < 0 = ""
  | g == 0 = " (minor, gen 0)"
  | otherwise = " (major, gen " <> tshow g <> ")"

threadLabel :: Timeline -> Word32 -> Text
threadLabel tl tid = maybe ("thread " <> tshow tid) threadName (M.lookup tid (tlThreads tl))

-- ---------------------------------------------------------------------------
-- Drawing

drawMain :: Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> Marks -> Bool -> (Int -> Bool) -> Maybe Float -> Rect -> CanvasM ()
drawMain pal mm tl v sel marks code focusCC hoverX r@(Rect x _ _ _) = do
  let rows = layoutRows r (V.length (tlCaps tl)) code
      px = rPlotX rows
      pw = rPlotW rows
      font = mmFont mm
      fm = mmMetrics mm
      lineH = mmLineH mm
      label' y txt = drawTextClippedWith fm font EllipsisEnd (Rect (x + 14) (y - lineH / 2) (gutterW - 26) lineH) AlignStart AlignMiddle txt (pText pal)
      sub y txt = drawTextClippedWith fm font EllipsisEnd (Rect (x + 14) (y - lineH / 2) (gutterW - 26) lineH) AlignStart AlignMiddle txt (pMuted pal)
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
  -- Markers run down through the capability tracks (thinned to 2 px so
  -- dense marker streams never turn the chart into a solid wall).
  withClip (Rect px (rectY (rRuler rows)) pw (capsBottom - rectY (rRuler rows))) $ do
    let go _ _ [] = pure ()
        go !lastM !lastE (mk : rest) = do
          let mx = timeToX v px pw (fromIntegral (mkTime mk))
          if mkIsMarker mk
            then
              if mx - lastM >= 2
                then do
                  drawStroke (V2 mx (rectY (rRuler rows) + rectH (rRuler rows))) (V2 mx capsBottom) 1 (alpha 0.55 (pMarker pal))
                  go mx lastE rest
                else go lastM lastE rest
            else
              if mx - lastE >= 2
                then do
                  drawStroke (V2 mx (rectY (rRuler rows) + 18)) (V2 mx (rectY (rRuler rows) + rulerH)) 1 (alpha 0.6 (pMuted pal))
                  go lastM mx rest
                else go lastM lastE rest
    go (px - 10) (px - 10) (markersIn tl v)
  let Rect _ gy _ _ = rGC rows
      Rect _ my' _ _ = rMem rows
      maxPause = V.foldl' (\m g -> max m (fromIntegral (gcEnd g - gcStart g) :: Double)) 0 (tlGCs tl)
      peakHeap = VU.foldl' (\m (_, b) -> max m b) 0 (tlHeapSize tl)
      peakLive = VU.foldl' (\m (_, b) -> max m b) 0 (tlLive tl)
  label' (gy + 12) "GC pauses"
  sub (gy + 30) (if maxPause > 0 then "max " <> fmtNs maxPause else "log scale")
  drawGCStrip pal v (rGC rows) tl sel
  label' (my' + 12) "Heap"
  sub (my' + 30) (if peakLive > 0 then "live " <> fmtBytes peakLive else if peakHeap > 0 then "size " <> fmtBytes peakHeap else "size, live")
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
  for_ hoverX $ \hx ->
    drawStroke (V2 hx (rectY r)) (V2 hx (rectY r + rectH r)) 1 (pCursor pal)

drawRuler :: Palette -> MonoMetrics -> Timeline -> TimeView -> Rect -> CanvasM ()
drawRuler pal mm tl v (Rect px py pw ph) = do
  let step = niceStep (viewWidth v * 110 / realToFrac pw)
      origin = fromIntegral (tlStart tl)
  drawRect (Rect px (py + ph - 1) pw 1) (pGrid pal)
  forM_ (take 200 (axisTicks step (tvStart v - origin) (tvEnd v - origin))) $ \o -> do
    let tx = timeToX v px pw (o + origin)
        txt = fmtTick step o
    drawStroke (V2 tx (py + ph - 7)) (V2 tx (py + ph)) 1 (pMuted pal)
    -- A label that would run past the plot is left off rather than cut.
    when (tx + 4 + fromIntegral (T.length txt) * mmCharW mm <= px + pw) $
      drawTextWith (mmFont mm) (V2 (tx + 4) (py + 11)) AlignStart AlignMiddle txt (pMuted pal)
  let drawRulerMarkers _ [] = pure ()
      drawRulerMarkers !lastX (mk : rest)
        | not (mkIsMarker mk) = drawRulerMarkers lastX rest
        | otherwise = do
            let mx = timeToX v px pw (fromIntegral (mkTime mk))
            if mx - lastX >= 4
              then do
                -- On the baseline, under the tick labels.
                drawPath (P.polygon [V2 (mx - 4) (py + ph - 8), V2 (mx + 4) (py + ph - 8), V2 mx (py + ph)]) (pMarker pal)
                drawRulerMarkers mx rest
              else drawRulerMarkers lastX rest
  drawRulerMarkers (px - 10) (markersIn tl v)

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
        let wInt = max 1 (floor w :: Int)
            boundary !lo !px
              | px > wInt = []
              | otherwise =
                  let t = floorW (xToTime v x w (x + fromIntegral px))
                      c = countStartingByFrom tr lo t
                   in (t, c - 1) : boundary (max lo (c - 1)) (px + 1)
            bounds = boundary i0 0
            quant :: Double -> Float
            quant f = fromIntegral (round (min 1 f * 16) :: Int) / 16
            colFracs (ta, ia) (tb, ib) =
              let dt = max 1 (fromIntegral (tb - min tb ta) :: Double)
               in [quant ((cumAtIndex tr ib tb (fromIntegral k) - cumAtIndex tr ia ta (fromIntegral k)) / dt) | (k, _) <- kinds]
            cols = [(fromIntegral px, colFracs a b) | (px, (a, b)) <- zip [0 :: Int ..] (zip bounds (drop 1 bounds))]
            runs = groupRuns cols
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
          drawTextClippedWith (mmMetrics mm) (mmFont mm) EllipsisMiddle (Rect (xa + 3) y (bw - 6) h) AlignStart AlignMiddle (cciLabel c) (pText pal)
      else do
        -- Too many to draw: keep the most sampled run per pixel.
        let buckets =
              IM.fromListWith
                (\a b -> if crSamples a >= crSamples b then a else b)
                [(floor (timeToX v x w (fromIntegral (crStart cr)) - x) :: Int, cr) | cr <- shown]
        forM_ (IM.toList buckets) $ \(px, cr) ->
          drawRect (Rect (x + fromIntegral px) (y + 1) 1 (h - 2)) (colour (crCostCentre cr))

-- | Merge consecutive columns with identical values.
groupRuns :: Eq b => [(Float, b)] -> [(Float, Float, b)]
groupRuns [] = []
groupRuns ((p0, b0) : rest0) = go p0 p0 b0 rest0
  where
    go !a !b cur [] = [(a, b, cur)]
    go !a !b cur ((p, x) : rest)
      | x == cur = go a p cur rest
      | otherwise = (a, b, cur) : go p p x rest

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
  let inView = pointsIn (floorW (tvStart v)) (floorW (tvEnd v)) ps
  when (VU.length inView <= 600) $ withClip r $
    VU.forM_ inView $ \(t, k, _) -> do
      let px = timeToX v x w (fromIntegral t)
          c
            | k == pointMigrate = pMigrate pal
            | k == pointSteal = pSteal pal
            | otherwise = pWakeup pal
      drawPath (P.polygon [V2 (px - 3) y, V2 (px + 3) y, V2 px (y + 5)]) c

type Word8' = SpanKind

-- | A capability's points at @[lo, hi]@ ns; they are sorted by time.
pointsIn :: Word64 -> Word64 -> VU.Vector (Word64, Word8', Word32) -> VU.Vector (Word64, Word8', Word32)
pointsIn lo hi ps =
  let at = lowerBoundBy (\(t, _, _) -> t) ps
      i0 = at lo
      i1 = if hi == maxBound then VU.length ps else at (hi + 1)
   in VU.slice i0 (max 0 (i1 - i0)) ps

drawGCStrip :: Palette -> TimeView -> Rect -> Timeline -> Maybe Sel -> CanvasM ()
drawGCStrip pal v r@(Rect x y w h) tl sel = do
  drawRect r (pTrackBg pal)
  let gcs = tlGCs tl
      i0 = lowerBoundBy gcEnd gcs (floorW (tvStart v))
      inView = takeWhile (\g -> fromIntegral (gcStart g) <= tvEnd v) (V.toList (V.drop i0 gcs))
      maxPause = V.foldl' (\m g -> max m (fromIntegral (gcEnd g - gcStart g))) 1 gcs :: Double
      logDenom = log (1 + maxPause / 1000)
      height g = realToFrac (log (1 + fromIntegral (gcEnd g - gcStart g) / 1000) / logDenom) * (h - 4)
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
              IM.fromListWith
                (\a b -> if gcEnd a - gcStart a >= gcEnd b - gcStart b then a else b)
                [(floor (timeToX v x w (fromIntegral (gcStart g)) - x) :: Int, g) | g <- inView]
        forM_ (IM.toList buckets) $ \(px, g) -> do
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
      endT = ceiling (tvEnd v) :: Word64
      pts s =
        let i0 = max 0 (lowerBoundBy fst s (floorW (tvStart v)) - 1)
            i1 = min (VU.length s) (lowerBoundBy fst s (endT + 1) + 1)
            slice = VU.slice i0 (max 0 (i1 - i0)) s
         in downsample w [(timeToX v x w (fromIntegral t), b) | (t, b) <- VU.toList slice]
      steps ps = case unsnoc ps of
        Nothing -> []
        Just (_, (_, lastB)) -> concat [[V2 xa (py b), V2 xb (py b)] | ((xa, b), (xb, _)) <- zip ps (drop 1 ps ++ [(x + w, lastB)])]
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
        ci0 = lowerBoundBy id cs c0
        ci1 = lowerBoundBy id cs (endT + 1)
        inView = VU.slice ci0 (max 0 (ci1 - ci0)) cs
    when (VU.length inView <= floor w) $
      VU.forM_ inView $ \c -> do
        let cx = timeToX v x w (fromIntegral c)
        drawStroke (V2 cx (y + h - 7)) (V2 cx (y + h)) 1.5 (pText pal)

-- | At most two points per pixel: the first and the maximum.
downsample :: Float -> [(Float, Word64)] -> [(Float, Word64)]
downsample w ps
  | null (drop (floor (w * 2)) ps) = ps
  | otherwise =
      [ (fromIntegral px, maxB)
      | (px, maxB) <- IM.toList (IM.fromListWith max [(floor x :: Int, b) | (x, b) <- ps])
      ]

-- | Cursor-following rich tooltip overlay for timeline canvases.
cursorTooltip :: HasResponse r => r -> Maybe [Text] -> NanoUI ()
cursorTooltip resp mLines =
  scope $ for_ mLines $ \ls ->
    void . tooltipWidgetConfigured defaultTooltipConfig {tooltipDelay = 0, tooltipGrace = 0, tooltipPlacement = PlacementAtCursor, tooltipGap = 12} resp $
      columnWith (tight . gap 2 . padXY 6 4) $
        forM_ (zip [0 :: Int ..] ls) $ \(i, l) ->
          labelWith (fontMono . fontTabular . fontSizeScale 0.84 . (if i == 0 then fontMuted else id)) l

-- ---------------------------------------------------------------------------
-- Threads

threadsHeader :: TimelineState -> Palette -> Timeline -> NanoUI ()
threadsHeader st pal tl = do
  winW <- windowWidth
  paneMeta (fmtInt (M.size (tlThreads tl)))
  searchInputConfigured
    defaultSearchInputConfig {sicPlaceholder = "Filter by label or id", sicLayout = fixedW (if winW < 1200 then 160 else 200) . padXY 0 0}
    (tsThreadFilter st)
  scope $ when (winW >= 1220) $
    enumTabBarByConfigured defaultTabsConfig {tabsStyle = TabSegmented} threadSortName (tsThreadSort st)
  flex
  legendDot (pRun pal) "running"
  legendDot (pBlocked pal) "blocked"
  legendDot (pRunnable pal) "runnable"

threadsSection :: Env -> TimelineState -> Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> Marks -> NanoUI ()
threadsSection env st pal mm tl v sel marks = do
  do
    filt <- T.toLower <$> readVar (tsThreadFilter st)
    sortBy' <- readVar (tsThreadSort st)
    focus <- readVar (tsFocusThread st)
    (isFocus, isHover) <- isFocusOrHover env
    let rk = runKey (tlRun tl)
        blockedTotal ti = sum [ns | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c]
        filtered =
          [ ti
          | ti <- M.elems (tlThreads tl)
          , T.null filt || filt `T.isInfixOf` T.toLower (threadName ti)
          ]
        threads = case sortBy' of
          SortById -> filtered
          SortByActive -> sortOn (\ti -> (Down (tiRunTime ti), tiId ti)) filtered
          SortByBlocked -> sortOn (\ti -> (Down (blockedTotal ti), tiId ti)) filtered
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
            then
              let bt = blockedTotal ti
               in Just . (v2X mouse,v2Y mouse,) $
                    [ threadName ti
                    , "running " <> fmtNs (fromIntegral (tiRunTime ti)) <> " · blocked " <> fmtNs (fromIntegral bt)
                    , "scheduled " <> fmtInt (tiRuns ti) <> " times"
                    ]
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
    let mFocusTid =
          focus <|> case sel of
            Just (SelSpan (OnThread tid) _) -> Just tid
            _ -> Nothing
    mFocusCrit <- case mFocusTid of
      Nothing -> pure Nothing
      Just ftid -> liftIO $ do
        let cKey = (tlEventCount tl, ftid, floorW (tvStart v), ceiling (tvEnd v))
        prev <- readIORef (tsCritCache st)
        case prev of
          Just (k, res) | k == cKey -> pure (Just res)
          _ -> do
            let res = analyzeCriticalPath tl ftid (floorW (tvStart v)) (ceiling (tvEnd v))
            writeIORef (tsCritCache st) (Just (cKey, res))
            pure (Just res)
    resp <-
      canvasConfigured
        defaultCanvasConfig
          { canvasLayout = fillW . fillH . minH 80
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
            drawTextClippedWith (mmMetrics mm) (mmFont mm) EllipsisMiddle (Rect (x + 14) ry (gutterW - 26) threadRowH) AlignStart AlignMiddle (threadName ti) (if focused then pAccent pal else pText pal)
            drawTrack pal v rowRect (tiTrack ti) [(kindRun, pRun pal), (kindBlocked, pBlocked pal), (kindRunnable, pRunnable pal)] False
            case sel of
              Just (SelSpan (OnThread tid) k) | tid == tiId ti -> outlineSpan pal v rowRect (tiTrack ti) k
              _ -> pure ()
          -- Markers through the rows, causal critical-path arrows, the shared
          -- cursor and range, and a scroll indicator.
          let rowMidY =
                M.fromList
                  [ (tiId ti, y + 4 + fromIntegral idx * threadRowH - scrollY + threadRowH / 2)
                  | (idx, ti) <- zip [0 :: Int ..] threads
                  ]
          withClip (Rect px y pw h) $ do
            forM_ (markersIn tl v) $ \mk -> when (mkIsMarker mk) $ do
              let mx = timeToX v px pw (fromIntegral (mkTime mk))
              drawStroke (V2 mx y) (V2 mx (y + h)) 1 (alpha 0.35 (pMarker pal))
            for_ mFocusCrit $ \cpr ->
              forM_ (take 64 (cpCausalEdges cpr)) $ \ce ->
                case (M.lookup (ceFromThread ce) rowMidY, M.lookup (ceToThread ce) rowMidY) of
                  (Just y1, Just y2)
                    | abs (y2 - y1) >= 2
                    , min y1 y2 <= y + h + 20
                    , max y1 y2 >= y - 20 -> do
                        let cx = timeToX v px pw (fromIntegral (ceTime ce))
                            edgeCol =
                              if "blackhole" `T.isInfixOf` T.toLower (ceReason ce) || "black hole" `T.isInfixOf` T.toLower (ceReason ce)
                                then pBlocked pal
                                else pWakeup pal
                            dir = if y2 > y1 then 1 else -1
                        when (cx >= px && cx <= px + pw) $ do
                          drawStroke (V2 cx y1) (V2 cx y2) 1.5 (alpha 0.85 edgeCol)
                          drawPath (P.polygon [V2 (cx - 3.5) (y2 - dir * 5), V2 (cx + 3.5) (y2 - dir * 5), V2 cx y2]) edgeCol
                  _ -> pure ()
          drawMarks pal (linearAxis rk v px pw) (Rect px y pw h) marks {tmMarkers = []}
          when (contentH > h) $ do
            let barH = max 20 (h * h / contentH)
                barY = y + (h - barH) * (scrollY / max 1 (contentH - h))
            drawRoundedRect (Rect (x + w - 4) barY 3 barH) 2 (pGrid pal)
          for_ hoverText $ \(hx, _, _) ->
            drawStroke (V2 hx y) (V2 hx (y + h)) 1 (pCursor pal)
    cursorTooltip resp (if respHovered resp then (\(_, _, ls) -> ls) <$> hoverText else Nothing)
    let r = respRect resp
    -- The wheel scrolls rows here; Ctrl+wheel zooms time.
    inp <- askInput
    (_, wy) <- useWheelDeltaOn resp
    unless (modCtrl (inputModifiers inp)) $
      when (wy /= 0) $ writeVar (tsThreadScroll st) (max 0 (min maxScroll (scrollY + wy * threadRowH * 3)))
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
legendDot c txt = rowWith (tight . gap 5 . itemsCenter) $ do
  _ <- canvasKeyed (fromIntegral (colorToWord32 c)) (fixedWH 9 9) (\r -> drawRoundedRect r 2 c)
  labelWith (fontMuted . fontSizeScale 0.85) txt

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
      scope $ when (srKind s /= kindGC && srEnd s > srStart s) $ do
        let cpr = analyzeCriticalPath tl tid (srStart s) (srEnd s)
        when (cpBlockerCpuNs cpr > 0 || cpGCNs cpr > 0 || cpRunnableNs cpr > 0) $ do
          separator
          criticalPathCard env st tl cpr
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
      columnWith (tight . gap 0 . fillW) $ do
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
  rowWith (padXY 8 0 . tight . gap 8 . itemsBaseline . fillW) $ do
    colHeader fillW "closure"
    colHeader (fixedW 64 . alignEnd) "entries"
    colHeader (fixedW 72 . alignEnd) "alloc"
  forM_ (zip [0 :: Int ..] rows) $ \(k, (x@(_, tn, key), e, al)) -> withKey ("t" <> tshow k) $ do
    _ <- linkRow env (tickyRef x) False id $ do
      columnWith (tight . gap 0 . fillW) $ do
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
  tlc <- liftIO (cachedTimeline st tl)
  let rk = runKey (tlRun tl)
      bus = envBus env
  kv "Window" (fmtNs (viewWidth v))
  cursor <- readVar (busCursor bus)
  scope $ case cursor of
    Just (r, t) | runKeysMatch r rk -> rowWith (fillW . tight . gap 8 . itemsBaseline) $ do
      labelWith fontMuted "Cursor"
      flex
      linkText env (RInstant rk t) id (fmtNs (fromIntegral t - fromIntegral (tlStart tl)))
    _ -> pure ()
  range <- readVar (busRange bus)
  scope $ case range of
    Just (r, a, b) | runKeysMatch r rk -> do
      rowWith (fillW . tight . gap 8 . itemsBaseline) $ do
        labelWith fontMuted "Range"
        flex
        linkText env (RRange rk a b) id (fmtNs (fromIntegral a - fromIntegral (tlStart tl)) <> " to " <> fmtNs (fromIntegral b - fromIntegral (tlStart tl)))
      rowWith (tight . gap 6 . itemsCenter) $ do
        whenM (button "Analyse range") (analyseRange env st tl a b)
        whenM (button "Clear range") (writeVar (busRange bus) Nothing)
    _ -> pure ()
  sectionTitle "Longest GC pauses"
  winW <- windowWidth
  let gcs = tlcTopGCs tlc
      showKind = winW >= 1220
      pauseColW = if showKind then 92 else 86
      atColW = if showKind then 92 else 82
  tableRows $ do
    rowWith (padXY 8 0 . tight . gap 6 . itemsBaseline . fillW) $ do
      colHeader (fixedW pauseColW . alignEnd) "pause"
      if showKind then colHeader fillW "collection" else flex
      colHeader (fixedW atColW . alignEnd) "at"
    forM_ gcs $ \(i, g) -> withKey i $ do
      let dur = fromIntegral (gcEnd g - gcStart g) :: Double
      picked <- linkRow env (RCollection rk i) False (padXY 8 3) $ do
        numCell pauseColW (if dur >= 100e6 then fontTone Danger else id) (fmtNs dur)
        if showKind then cell (fillW . fontMuted) (if gcGen g >= 1 then "major" else "minor") else flex
        numCell atColW fontMuted (fmtNs (fromIntegral (gcStart g - tlStart tl)))
      when picked $ do
        writeVar (tsSel st) (Just (SelGC i))
        zoomTo st tl (gcStart g) (gcEnd g)
  sectionTitle "Most blocked threads"
  tableRows $ forM_ (tlcWorstBlocked tlc) $ \(ti, bt, top) -> withKey (tiId ti) $ do
    picked <- linkRow env (RThread rk (tiId ti)) False (padXY 8 3) $ do
      columnWith (tight . gap 0 . fillW) $ do
        labelWith fillW (threadName ti)
        scope $ for_ top $ \(c, _) -> labelWith (fillW . fontMuted . fontSizeScale 0.82) (stopName c)
      numCell 94 id (fmtNs (fromIntegral bt))
    when picked (writeVar (tsFocusThread st) (Just (tiId ti)))
  sectionTitle "Busiest threads"
  tableRows $ forM_ (tlcBusiest tlc) $ \ti -> withKey (tiId ti) $ do
    picked <- linkRow env (RThread rk (tiId ti)) False (padXY 8 3) $ do
      cell fillW (threadName ti)
      numCell 94 id (fmtNs (fromIntegral (tiRunTime ti)))
    when picked (writeVar (tsFocusThread st) (Just (tiId ti)))
  scope $ unless (V.null (tlProfSamples tl)) $ do
    let cp = tlcCodeProfile tlc
    sectionTitle ("Most sampled code (" <> fmtInt (cpSamples cp) <> " samples)")
    codeRows env tl cp 6
    sampleNote tl cp
  scope $ unless (V.null (tlTicky tl)) $ do
    sectionTitle ("Ticky counters (" <> fmtInt (V.length (tlTicky tl)) <> ")")
    tickyRows env (take 8 [(x, tcEntries c, tcAlloc c) | x@(c, _, _) <- tlcTicky tlc])
  sectionTitle "Program"
  forM_ (tlInfo tl) $ \(k, val) -> kv k (T.take 300 val)
  hint "Tip: click any slice, thread row or GC pause to inspect; Shift+drag selects a range."
  scope $ unless (V.null (tlGCs tl)) $ do
    separator
    latencyHeatmapView (tlcGCHeatmap tlc)
  let lags = take 6 (sortOn (Down . slSyncWaitNs) [l | l <- V.toList (tlGCSyncLags tl), slSyncWaitNs l > 0])
  scope $ unless (null lags) $ do
    separator
    sectionTitle "STW rendezvous stragglers"
    hint "Delay between the first capability pausing for GC and the last capability stopping. High sync wait points to a straggler thread in a tight non-allocating loop or unsafe FFI call."
    forM_ lags $ \l -> withKey ("lag" <> tshow (slCollection l)) $
      rowWith (tight . gap 8 . fillW . itemsCenter) $ do
        numCell 72 (if slSyncWaitNs l > 5_000_000 then fontTone Danger else id) (fmtNs (fromIntegral (slSyncWaitNs l)))
        cell (fontMuted . fontSizeScale 0.85) ("cap " <> tshow (slFirstCap l) <> " → " <> tshow (slLastCap l))
        flex
        case slStragglerThread l of
          Just tid -> linkText env (RThread rk tid) (fontSizeScale 0.88) (threadLabel tl tid)
          Nothing -> labelWith (fontMuted . fontSizeScale 0.85) "unknown straggler"
  let rns = tlcRunnable tlc
  scope $ when (rnCount rns > 0) $ do
    separator
    sectionTitle "Scheduler runnable latency"
    rowWith (tight . gap 12 . wrap . fillW) $ do
      fact "p50" (fmtNs (fromIntegral (rnP50Ns rns)))
      fact "p95" (fmtNs (fromIntegral (rnP95Ns rns)))
      fact "p99" (fmtNs (fromIntegral (rnP99Ns rns)))
      fact "max" (fmtNs (fromIntegral (rnMaxNs rns)))
      fact "migrations" (fmtInt (rnMigrations rns))
      fact "steals" (fmtInt (rnSteals rns))
    latencyHeatmapView (tlcRunHeatmap tlc)
  let sp = tlSparks tl
  scope $ when (ssCreated sp > 0) $ do
    separator
    sectionTitle "Parallel sparks (par / Strategies)"
    rowWith (tight . gap 12 . wrap . fillW) $ do
      fact "created" (fmtInt (ssCreated sp))
      fact "converted" (fmtInt (ssConverted sp))
      fact "fizzled" (fmtInt (ssFizzled sp))
      fact "dud" (fmtInt (ssDud sp))
      fact "overflowed" (fmtInt (ssOverflowed sp))
      fact "GC'd" (fmtInt (ssGCd sp))
  let stmRep = tlcStmContention tlc
  scope $ when (stmBlockCount stmRep > 0) $ do
    separator
    sectionTitle "STM contention & retry bursts"
    rowWith (tight . gap 12 . wrap . fillW) $ do
      fact "STM blocks" (fmtInt (stmBlockCount stmRep))
      fact "retry CPU" (fmtNs (fromIntegral (stmWastedCpuNs stmRep)))
      fact "herd bursts" (fmtInt (stmBurstCount stmRep))
    forM_ (take 4 (stmHotThreads stmRep)) $ \(tid, cnt, waitNs) -> withKey ("stm" <> tshow tid) $
      rowWith (tight . gap 8 . fillW) $ do
        linkText env (RThread rk tid) (fontSizeScale 0.88) (threadLabel tl tid)
        flex
        labelWith (fontMono . fontMuted . fontSizeScale 0.84) (fmtInt cnt <> " blocks · " <> fmtNs (fromIntegral waitNs))
  let cg = tlcContentionGraph tlc
  scope $ unless (null (cgBottlenecks cg)) $ do
    separator
    sectionTitle "Synchronization contention hubs"
    forM_ (take 5 (cgBottlenecks cg)) $ \(tid, wokenCnt, waitNs) -> withKey ("hub" <> tshow tid) $
      rowWith (tight . gap 8 . fillW) $ do
        linkText env (RThread rk tid) (fontSizeScale 0.88) (threadLabel tl tid)
        flex
        labelWith (fontMono . fontMuted . fontSizeScale 0.82) (fmtInt wokenCnt <> " wakeups · holds " <> fmtNs (fromIntegral waitNs))
  let conc = tlConcGC tl
      nm = tlNonmoving tl
  scope $ when (not (V.null conc) || nmRemSetFlushes nm > 0) $ do
    separator
    sectionTitle "Non-moving concurrent GC (+RTS -xn)"
    let totalConcNs = V.foldl' (\acc p -> acc + (cgpEnd p - cgpStart p)) (0 :: Word64) conc
    rowWith (tight . gap 12 . wrap . fillW) $ do
      fact "concurrent phases" (fmtInt (V.length conc))
      fact "concurrent time" (fmtNs (fromIntegral totalConcNs))
      fact "upd-rem-set flushes" (fmtInt (nmRemSetFlushes nm))
      fact "pruned segs" (fmtInt (nmPrunedSegments nm))
      fact "live blocks" (fmtInt (nmLiveBlocks nm))
  scope $ unless (V.null (tlMemReturns tl)) $ do
    let latestMr = V.last (tlMemReturns tl)
    separator
    sectionTitle "OS memory return (MADV_FREE / MemReturn)"
    rowWith (tight . gap 12 . wrap . fillW) $ do
      fact "current" (tshow (mrCurrent latestMr) <> " MiB")
      fact "needed" (tshow (mrNeeded latestMr) <> " MiB")
      fact "returned" (tshow (mrReturned latestMr) <> " MiB")
  scope $ unless (V.null (tlUserSpans tl)) $ do
    let slowest = take 10 (sortOn (\u -> Down (usEnd u - usStart u)) (V.toList (tlUserSpans tl)))
    separator
    sectionTitle ("Instrumented spans (" <> fmtInt (V.length (tlUserSpans tl)) <> ")")
    forM_ slowest $ \u -> withKey ("us" <> tshow (usId u)) $ do
      picked <- selectableRow False id $ do
        cell (fillW . fontMono . fontSizeScale 0.88) (usName u)
        numCell 72 id (fmtNs (fromIntegral (usEnd u - usStart u)))
        numCell 78 fontMuted (fmtBytes (usAllocBytes u))
      when picked $ do
        writeVar (tsFocusThread st) (Just (usThread u))
        analyseRange env st tl (usStart u) (usEnd u)
        zoomToRange st tl (usStart u) (usEnd u)
  scope $ unless (V.null (tlAlerts tl)) $ do
    separator
    sectionTitle ("Runtime alerts (" <> fmtInt (V.length (tlAlerts tl)) <> ")")
    forM_ (take 10 (V.toList (tlAlerts tl))) $ \a -> withKey ("al" <> tshow (taTime a)) $ do
      picked <- selectableRow False id $ do
        badge (if taKind a == "exc" then Danger else Warning) (taKind a)
        cell (fillW . fontSizeScale 0.88) (taTitle a)
        numCell 76 fontMuted (fmtNs (fromIntegral (taTime a - tlStart tl)))
      when picked $ do
        setCursor (envBus env) rk (taTime a)
        centreOn st tl (taTime a)

criticalPathCard :: Env -> TimelineState -> Timeline -> CriticalPathResult -> NanoUI ()
criticalPathCard env st tl cpr = do
  let rk = runKey (tlRun tl)
      tot = max 1 (fromIntegral (cpEnd cpr - cpStart cpr) :: Double)
      pctOf ns = fmtPct (fromIntegral ns / tot)
  sectionTitle ("Causal critical path (" <> threadLabel tl (cpTargetThread cpr) <> ")")
  rowWith (tight . gap 12 . wrap . fillW) $ do
    fact "own CPU" (fmtNs (fromIntegral (cpOwnCpuNs cpr)) <> " (" <> pctOf (cpOwnCpuNs cpr) <> ")")
    fact "blocker CPU" (fmtNs (fromIntegral (cpBlockerCpuNs cpr)) <> " (" <> pctOf (cpBlockerCpuNs cpr) <> ")")
    fact "STW GC" (fmtNs (fromIntegral (cpGCNs cpr)) <> " (" <> pctOf (cpGCNs cpr) <> ")")
    fact "runnable wait" (fmtNs (fromIntegral (cpRunnableNs cpr)) <> " (" <> pctOf (cpRunnableNs cpr) <> ")")
    fact "external/IO" (fmtNs (fromIntegral (cpExternalNs cpr)) <> " (" <> pctOf (cpExternalNs cpr) <> ")")
  scope $ for_ (cpTopBlocker cpr) $ \(btid, bns, reason) -> do
    kvLink env "Top blocker" (RThread rk btid) (threadLabel tl btid <> " · " <> fmtNs (fromIntegral bns) <> " (" <> reason <> ")")
    whenM (buttonWith (padXY 6 0 . tight) "Focus blocker thread") (writeVar (tsFocusThread st) (Just btid))
  scope $ unless (null (cpCausalEdges cpr)) $
    labelWith (fontMuted . fontSizeScale 0.82) (tshow (length (cpCausalEdges cpr)) <> " causal wakeup / blackhole hops traced")

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
  focusTid <- readVar (tsFocusThread st)
  let cpTid = case focusTid of
        Just tid -> Just tid
        Nothing -> fst <$> listToMaybe (rsThreads rs)
  scope $ for_ cpTid $ \tid -> do
    separator
    criticalPathCard env st tl (analyzeCriticalPath tl tid a b)
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
stepCursor :: Env -> TimelineState -> Timeline -> VU.Vector Word64 -> TimeView -> NanoUI ()
stepCursor env st tl gcStarts v = do
  (dir, shifted) <- stepKeys
  when (dir /= 0) $ do
    let bus = envBus env
        rk = runKey (tlRun tl)
    cursor <- readVar (busCursor bus)
    let from = case cursor of
          Just (r, t) | runKeysMatch r rk -> t
          _ -> if dir > 0 then floorW (tvStart v) else ceiling (tvEnd v)
        stops = if shifted then censusTimes tl else gcStarts
    for_ (stepTime dir stops from) $ \t -> do
      setCursor bus rk t
      unless (fromIntegral t >= tvStart v && fromIntegral t <= tvEnd v) (centreOn st tl t)

-- ---------------------------------------------------------------------------
-- Markers

markersHeader :: TimelineState -> Timeline -> NanoUI ()
markersHeader st tl = do
  winW <- windowWidth
  let nMarkers = V.foldl' (\acc m -> if mkIsMarker m then acc + 1 else acc) (0 :: Int) (tlMarkers tl)
      compact = winW < 1220
  paneMetaWith (fixedW (if compact then 24 else 32)) (fmtInt nMarkers)
  searchInputConfigured
    defaultSearchInputConfig {sicPlaceholder = "Filter", sicLayout = minW 52 . fillW . padXY 0 0}
    (tsMarkerFilter st)
  (_, evArea) <- mouseArea tight $ checkbox (if compact then "All" else "Events") (tsMarkerEvents st)
  when compact (tooltip evArea "Include all runtime events (markers and RTS events)")

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
    else paneVirtualRows (grow . tight) (padXY 4 4) (take 2000 rows) $ \(i, mk) -> withKey i $ do
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
        tickyByKey = M.fromListWith (flip (++)) [(key, [x]) | x@(_, _, key) <- ticky]
        tickyOf m k = [x | x@(_, tn, _) <- M.findWithDefault [] k tickyByKey, T.null m || tnModule tn == m]
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
              RThread r t -> (\ti -> (threadPeek tl ti) {peekExact = Just (runNote "thread id" r rk)}) <$> thread r t
              RCollection r i -> (\g -> (gcPeek tl g) {peekExact = Just (runNote "collection" r rk)}) <$> gc r i
              RMarker r i -> (\mk -> (peekOf [("at", offset (mkTime mk))]) {peekExact = Just (runNote "eventlog marker" r rk)}) <$> marker r i
              RCensus r j -> (\t -> (peekOf [("taken at", offset t)]) {peekExact = Just "exact: this eventlog's census"}) <$> census r j
              RInstant r t | mine r, inLog t -> Just (instantPeek tl t) {peekExact = Just (runNote "program time" r rk)}
              RRange r a b | mine r, inLog a, b > a -> Just (rangePeek tl a b) {peekExact = Just (runNote "program time" r rk)}
              RCostCentre m l -> (\(c, n) -> codePeek tl c n totalSamples) <$> sampled m l
              RBinding m k | xs@(_ : _) <- tickyOf m k ->
                Just
                  (peekOf [("ticky", tickyNote (sum [tcEntries c | (c, _, _) <- xs]) (sum [tcAlloc c | (c, _, _) <- xs])), ("counters", fmtInt (length xs))])
                    { peekExact = Just "by name: the counters' binder names"
                    }
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
          , lensFacts = \case
              RThread r t | Just ti <- thread r t -> threadFacts tl rk ti
              RRange r a b | mine r, inLog a, b > a -> rangeFacts tl rk ticky a b
              RBinding m k | xs@(_ : _) <- tickyOf m k -> [FactTicky (sum [tcEntries c | (c, _, _) <- xs]) (sum [tcAlloc c | (c, _, _) <- xs])]
              _ -> []
          }

-- | What the next steps need about a thread: its longest blocked span,
-- who woke it from it or owned the black hole it waited on, and what it
-- ran most.
threadFacts :: Timeline -> RunKey -> ThreadInfo -> [Fact]
threadFacts tl rk ti =
  concat
    [ [FactBlocked (RRange rk (srStart s) (srEnd s)) (srEnd s - srStart s) (T.replace "blocked on " "" (stopName (srStop s))) | Just s <- [longest]]
    , [FactOwner (RThread rk (srAux s)) | Just s <- [longest], stopName (srStop s) == "blocked on black hole", srAux s /= 0, srAux s /= tiId ti]
    , [FactWokenBy (RThread rk w) | Just s <- [longest], Just (_, w) <- [wokenBy ti s], w /= 0, w /= tiId ti]
    , [FactRan (RCostCentre (cciModule c) (cciLabel c)) n | (i, n) : _ <- [cpTop (threadProfile tl (tiId ti))], Just c <- [IM.lookup i (tlCostCentres tl)]]
    ]
  where
    tr = tiTrack ti
    n = trackLength tr
    go !i !bestI !bestDur
      | i >= n = if bestI < 0 then Nothing else Just (trackSpan tr bestI)
      | trKind tr VU.! i == kindBlocked =
          let dur = trEnd tr VU.! i - trStart tr VU.! i
           in if bestI < 0 || dur > bestDur
                then go (i + 1) i dur
                else go (i + 1) bestI bestDur
      | otherwise = go (i + 1) bestI bestDur
    longest = go 0 (-1) 0

-- | What the next steps need about a range: the share of it spent paused,
-- what was allocated and how live changed, the top allocator by ticky,
-- what ran most, and the census at its end.
rangeFacts :: Timeline -> RunKey -> [(TickyCounter, TickyName, Text)] -> Word64 -> Word64 -> [Fact]
rangeFacts tl rk ticky a b =
  concat
    [ [FactGC (fromIntegral (rsPauseTotal rs) / fromIntegral (b - a)) (length (rsCollections rs))]
    , [FactAllocation n (fst (rsLive rs)) (snd (rsLive rs)) | Just n <- [rsAllocated rs]]
    , [FactTopAllocator r n | (r, n) : _ <- [sortOn (Down . snd) (M.toList allocs)], n > 0]
    , [FactRan (RCostCentre (cciModule c) (cciLabel c)) n | (i, n) : _ <- [cpTop (codeProfile tl (const True) a b)], Just c <- [IM.lookup i (tlCostCentres tl)]]
    , [FactCensusAtEnd (RCensus rk j) | j : _ <- [reverse (rsCensuses rs)]]
    ]
  where
    rs = rangeStats tl a b
    refOf = M.fromList [(tcId c, tickyRef x) | x@(c, _, _) <- ticky]
    allocs = M.fromListWith (+) [(r, al) | (c, _, al) <- tickyBetween tl a b, Just r <- [M.lookup (tcId c) refOf]]

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

threadPeek :: Timeline -> ThreadInfo -> Peek
threadPeek tl ti =
  let created = fromMaybe (tlStart tl) (tiCreated ti)
      ended = fromMaybe (tlEnd tl) (tiFinished ti)
      life = fromIntegral (max 1 (ended - created)) :: Double
      offCpu = [(c, ns) | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c]
      blocked = sum (map snd offCpu)
      top = listToMaybe (sortOn (Down . snd) offCpu)
   in peekOf $
        [("ran", fmtNs (fromIntegral (tiRunTime ti)) <> ", " <> fmtPct (fromIntegral (tiRunTime ti) / life) <> " of its life")]
          ++ [("blocked", fmtNs (fromIntegral blocked) <> maybe "" (\(c, _) -> ", mostly " <> stopName c) top) | blocked > 0]
          ++ [("capabilities", T.intercalate ", " (map tshow (tiCaps ti)))]

gcPeek :: Timeline -> GCEvent -> Peek
gcPeek tl g =
  peekOf $
    [ ("pause", fmtNs (fromIntegral (gcEnd g - gcStart g)))
    , ("at", fmtNs (fromIntegral (gcStart g - min (gcStart g) (tlStart tl))))
    , ("copied", fmtBytes (gcCopied g))
    ]
      ++ [("live after", fmtBytes b) | Just b <- [seriesValueAt (tlLive tl) (gcEnd g)]]

rangePeek :: Timeline -> Word64 -> Word64 -> Peek
rangePeek tl a b =
  let rs = rangeStats tl a b
   in peekOf $
        [ ("lasts", fmtNs (fromIntegral (b - a)))
        , ("collections", fmtInt (length (rsCollections rs)) <> ", paused " <> fmtNs (fromIntegral (rsPauseTotal rs)))
        ]
          ++ [("allocated", fmtBytes n) | Just n <- [rsAllocated rs]]
          ++ [("threads ran", fmtInt (length (rsThreads rs)))]

codePeek :: Timeline -> CostCentreInfo -> Int -> Int -> Peek
codePeek tl c n total =
  Peek
    { peekFacts =
        ("on top of the stack", fmtInt n <> " of " <> fmtInt total <> " samples, one every " <> fmtNs (fromIntegral (tlSamplePeriod tl)))
          : [("source", renderSrcSpan sp) | Just sp <- [srcLocSpan (cciSrc c)]]
    , peekNotes = ["Few samples: a hint, not a measurement." | fewSamples n]
    , peekExact = Just "by name: the eventlog's cost-centre module and label"
    , peekExtra = Nothing
    }

instantPeek :: Timeline -> Word64 -> Peek
instantPeek tl t =
  peekOf $
    [("live", fmtBytes b) | Just b <- [seriesValueAt (tlLive tl) t]]
      ++ [("heap", fmtBytes b) | Just b <- [seriesValueAt (tlHeapSize tl) t]]
