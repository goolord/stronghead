-- | The eventlog timeline: capability tracks, the GC pressure strip, the
-- heap strip, markers, per-thread rows and the slice inspector.
--
-- Wheel zooms around the pointer, dragging pans, a click selects the slice
-- under the pointer, and a double click fits the whole log.
module UI.Timeline
  ( TimelineState (..)
  , newTimelineState
  , timelineLoad
  , timelineView
  , openEventlog
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)
import NanoUI.Path qualified as P
import Stronghead.Eventlog
import Stronghead.Eventlog.Model
import Stronghead.Format
import UI.Common

data TrackRef = OnCap !Int | OnThread !Word32
  deriving (Eq, Show)

data Sel
  = SelSpan !TrackRef !Int
  | SelGC !Int
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
  , tsShowThreads :: !(Var Bool)
  , tsPressAt :: !(Var (Maybe V2))
  , tsDialog :: !(Var (Maybe FileDialogId))
  , tsFocusThread :: !(Var (Maybe Word32))
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
  tsShowThreads <- newVar True
  tsPressAt <- newVar Nothing
  tsDialog <- newVar Nothing
  tsFocusThread <- newVar Nothing
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

-- ---------------------------------------------------------------------------
-- Geometry

gutterW, rulerH, capH, capGap, gcH, memH, threadRowH :: Float
gutterW = 150
rulerH = 26
capH = 24
capGap = 4
gcH = 54
memH = 54
threadRowH = 18

data Rows = Rows
  { rPlotX :: !Float
  , rPlotW :: !Float
  , rRuler :: !Rect
  , rCaps :: ![Rect]
  , rGC :: !Rect
  , rMem :: !Rect
  }

mainHeight :: Int -> Float
mainHeight nCaps = rulerH + fromIntegral nCaps * (capH + capGap) + 10 + gcH + 10 + memH

layoutRows :: Rect -> Int -> Rows
layoutRows (Rect x y w _) nCaps =
  let px = x + gutterW
      pw = max 10 (w - gutterW - 4)
      capY i = y + rulerH + fromIntegral i * (capH + capGap)
      gcY = capY nCaps + 10
   in Rows
        { rPlotX = px
        , rPlotW = pw
        , rRuler = Rect px y pw rulerH
        , rCaps = [Rect px (capY i) pw capH | i <- [0 .. nCaps - 1]]
        , rGC = Rect px gcY pw gcH
        , rMem = Rect px (gcY + gcH + 10) pw memH
        }

-- ---------------------------------------------------------------------------
-- View

timelineView :: TimelineState -> Maybe LoadProgress -> NanoUI ()
timelineView st mprog = columnWith (gap 8 . grow . padAll 10) $ do
  useFileDialog (tsDialog st) $ \case
    p : _ -> openEventlog st p False
    [] -> pure ()
  path <- readVar (tsPath st)
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    whenM (button "Open eventlog…") $
      writeVar (tsDialog st) . Just
        =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "GHC eventlog" "eventlog", FileFilter "All files" "*"]}
    pathLabel path
    checkboxWith alignMid "Follow live" (tsFollow st)
    scope $ for_ mprog $ \p -> do
      unless (lpDone p) $ do
        let frac = if lpTotal p > 0 then fromIntegral (lpBytes p) / fromIntegral (lpTotal p) else 0 :: Double
        progressBarWith (fixedW 160) 8 (realToFrac frac)
        labelWith fontMuted ("reading " <> fmtBytes (fromIntegral (lpBytes p)))
      when (lpFollowing p && lpDone p) $ badge Success "following"
      for_ (lpError p) $ \e -> labelWith (fontTone Danger) ("decode stopped: " <> T.take 120 e)
    flex
    whenM (button "Fit") (writeVar (tsView st) Nothing)
  case mprog >>= lpTimeline of
    Nothing -> scope $ emptyState path
    Just tl
      | V.null (tlCaps tl) -> scope $ hint "This eventlog has no scheduler events yet. Was the program run with +RTS -l?"
      | otherwise -> scope (body st tl)

emptyState :: Maybe FilePath -> NanoUI ()
emptyState path = columnWith (gap 6 . padAll 24) $ case path of
  Just _ -> hint "Loading…"
  Nothing -> do
    sectionTitle "No eventlog loaded"
    hint "Open a .eventlog file, drop one on the window, or pass it on the command line."
    hint "Record one with:  ghc -threaded -rtsopts Main.hs   then   ./Main +RTS -l -N4 -RTS"
    hint "Add -hT (or -hi with -finfo-table-map) to get heap samples for the Heap tab, and use"
    hint "Debug.Trace.traceMarkerIO to drop named markers on the timeline."

body :: TimelineState -> Timeline -> NanoUI ()
body st tl = do
  th <- uiTheme
  let pal = palette th
      lo = fromIntegral (tlStart tl)
      hi = fromIntegral (tlEnd tl) :: Double
      full = TimeView lo hi
      summary = summarize tl
  mv <- readVar (tsView st)
  let v = maybe full (clampView lo hi) mv
  rowWith (tight . gap 8 . wrap . lineGap 6 . fillW) $ do
    statTile "Duration" (fmtSec (smDuration summary))
    statTile "Mutator" (fmtPct (smMutatorFrac summary))
    statTile "GC time" (fmtPct (smGCFrac summary))
    statTile "GCs (major)" (fmtInt (smGCCount summary) <> " (" <> fmtInt (smMajorGCs summary) <> ")")
    statTile "Longest pause" (fmtNs (smGCPauseMax summary))
    statTile "Allocated" (fmtBytes (smAllocated summary))
    statTile "Alloc rate" (fmtRate (fromIntegral (smAllocated summary) / max 1e-9 (smDuration summary)))
    statTile "Peak live" (fmtBytes (smPeakLive summary))
    statTile "Threads" (fmtInt (smThreads summary))
    statTile "Events" (fmtInt (tlEventCount tl))
  mm <- monoMetrics 0.85
  sel <- readVar (tsSel st)
  rowWith (tight . gap 10 . grow) $ do
    columnWith (tight . gap 6 . grow) $ do
      mainCanvas st pal mm tl v sel
      threadsSection st pal mm tl v sel
    panelWith (fixedW 360 . fillH . padAll 10 . gap 6) $
      scrollWith (grow . tight) $
        columnWith (tight . gap 6 . fillW) (inspector st tl v sel)

-- | Apply wheel zoom, drag pan and double-click fit for a canvas whose plot
-- area spans @[plotX, plotX + plotW]@.
navigate :: TimelineState -> Timeline -> TimeView -> Response -> Float -> Float -> Bool -> NanoUI ()
navigate st tl v resp plotX plotW wheelZooms = do
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
  when (dragActive drag && dx /= 0) $ do
    let dt = realToFrac dx / realToFrac plotW * viewWidth v
    setView (TimeView (tvStart v - dt) (tvEnd v - dt))
  when (respHovered resp && pressedIn MouseLeft inp) $ do
    writeVar (tsPressAt st) (Just mouse)
    when (inputMouseClicks inp >= 2) (writeVar (tsView st) Nothing)

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

mainCanvas :: TimelineState -> Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> NanoUI ()
mainCanvas st pal mm tl v sel = do
  let nCaps = V.length (tlCaps tl)
  mouse <- uiMousePos
  -- Geometry from last frame's rect: interaction runs before layout.
  sid <- currentId
  prevRect <- fromMaybe (Rect 0 0 0 0) <$> lastRect sid
  let rows0 = layoutRows prevRect nCaps
      hover = hoverInfo tl v rows0 mouse
  resp <-
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = (fillW . fixedH (mainHeight nCaps)) defaultLayout
        , canvasTrackPointer = True
        , canvasCursor = Just (\_ _ _ -> UiCursorCrosshair)
        }
      (\r -> drawMain pal mm tl v sel (if rectContains r mouse then hover else Nothing) r)
  let rows = layoutRows (respRect resp) nCaps
  navigate st tl v resp (rPlotX rows) (rPlotW rows) True
  clicked <- isClick st resp
  when clicked $ writeVar (tsSel st) (pickMain tl v rows mouse)

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
    details = case [i | (i, r) <- zip [0 :: Int ..] (rCaps rows), inRow r] of
      i : _ -> capDetails i
      []
        | inRow (rGC rows) -> gcDetails
        | inRow (rMem rows) -> memDetails
        | inRow (rRuler rows) -> markerDetails
        | otherwise -> []
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
            ]
    markerDetails =
      [ (if mkIsMarker mk then "marker: " else "event: ") <> mkText mk
      | mk <- V.toList (tlMarkers tl)
      , let d = if mkTime mk > tw then mkTime mk - tw else tw - mkTime mk
      , d <= tol * 2
      ]
    _ = m

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

drawMain :: Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> Maybe (Float, Float, [Text]) -> Rect -> CanvasM ()
drawMain pal mm tl v sel hover r@(Rect x _ _ _) = do
  let rows = layoutRows r (V.length (tlCaps tl))
      px = rPlotX rows
      pw = rPlotW rows
      font = mmFont mm
      label' y txt = drawTextClipped font (mmCharW mm) (V2 (x + 6) y) (gutterW - 12) txt (pText pal)
      capsBottom = case reverse (rCaps rows) of
        Rect _ cy _ ch : _ -> cy + ch
        [] -> rectY (rRuler rows) + rulerH
  drawRuler pal mm tl v (rRuler rows)
  forM_ (zip3 [0 :: Int ..] (rCaps rows) (V.toList (tlCaps tl))) $ \(i, cr@(Rect _ cy _ ch), tr) -> do
    label' (cy + ch / 2) ("Cap " <> tshow i)
    drawTrack pal v cr tr [(kindRun, pRun pal), (kindGC, pGC pal)] True
    drawPoints pal v cr (tlCapPoints tl V.! i)
    case sel of
      Just (SelSpan (OnCap j) k) | j == i -> outlineSpan pal v cr tr k
      _ -> pure ()
  -- Markers run down through the capability tracks.
  withClip (Rect px (rectY (rRuler rows)) pw (capsBottom - rectY (rRuler rows))) $
    forM_ (markersIn tl v) $ \mk -> do
      let mx = timeToX v px pw (fromIntegral (mkTime mk))
      if mkIsMarker mk
        then drawStroke (V2 mx (rectY (rRuler rows) + 12)) (V2 mx capsBottom) 1 (alpha 0.55 (pMarker pal))
        else drawStroke (V2 mx (rectY (rRuler rows) + 18)) (V2 mx (rectY (rRuler rows) + rulerH)) 1 (alpha 0.6 (pMuted pal))
  let Rect _ gy _ gh = rGC rows
      Rect _ my' _ mh = rMem rows
  label' (gy + 12) "GC pauses"
  drawTextWith font (V2 (x + 6) (gy + 30)) AlignStart AlignMiddle "minor / major" (pMuted pal)
  drawGCStrip pal v (rGC rows) tl sel
  label' (my' + 12) "Heap"
  drawTextWith font (V2 (x + 6) (my' + 30)) AlignStart AlignMiddle "size / live" (pMuted pal)
  drawMemStrip pal v (rMem rows) tl
  let _ = (gh, mh)
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
    drawStroke (V2 tx (py + ph - 7)) (V2 tx (py + ph)) 1 (pMuted pal)
    drawTextWith (mmFont mm) (V2 (tx + 3) (py + 8)) AlignStart AlignMiddle (fmtNs (t - origin)) (pMuted pal)
  forM_ (markersIn tl v) $ \mk -> when (mkIsMarker mk) $ do
    let mx = timeToX v px pw (fromIntegral (mkTime mk))
    drawPath (P.polygon [V2 mx (py + 12), V2 (mx + 6) (py + 16), V2 mx (py + 20)]) (pMarker pal)

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

threadsSection :: TimelineState -> Palette -> MonoMetrics -> Timeline -> TimeView -> Maybe Sel -> NanoUI ()
threadsSection st pal mm tl v sel = do
  showThreads <- readVar (tsShowThreads st)
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    sectionTitle "Threads"
    checkboxWith alignMid "Show" (tsShowThreads st)
    searchInput "Filter by label or id" (tsThreadFilter st)
    flex
    legendDot (pRun pal) "running"
    legendDot (pBlocked pal) "blocked"
    legendDot (pRunnable pal) "runnable"
    legendDot (pGC pal) "GC"
    legendDot (pMarker pal) "marker"
  scope $ when showThreads $ do
    filt <- T.toLower <$> readVar (tsThreadFilter st)
    focus <- readVar (tsFocusThread st)
    let threads =
          [ ti
          | ti <- M.elems (tlThreads tl)
          , T.null filt || filt `T.isInfixOf` T.toLower (threadName ti)
          ]
        nRows = length threads
        contentH = fromIntegral nRows * threadRowH
    scrollY0 <- readVar (tsThreadScroll st)
    mouse <- uiMousePos
    sid <- currentId
    prevRect <- fromMaybe (Rect 0 0 0 0) <$> lastRect sid
    let viewH = rectH prevRect
        maxScroll = max 0 (contentH - viewH)
        scrollY = max 0 (min maxScroll scrollY0)
        rowAt (Rect _ ry _ _) my = floor ((my - ry + scrollY) / threadRowH) :: Int
        plotX r = rectX r + gutterW
        plotW r = max 10 (rectW r - gutterW - 4)
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
                          | srKind s == kindBlocked -> [stopName (srStop s) <> " for " <> dur] ++ ownerLine tl (srAux s)
                          | otherwise -> ["runnable for " <> dur <> " (" <> stopName (srStop s) <> ")"]
    resp <-
      canvasConfigured
        defaultCanvasConfig
          { canvasLayout = (fillW . grow . minH 140) defaultLayout
          , canvasTrackPointer = True
          }
        $ \r@(Rect x y w h) -> do
          let firstRow = max 0 (floor (scrollY / threadRowH))
              visible = take (ceiling (h / threadRowH) + 1) (drop firstRow (zip [firstRow ..] threads))
              px = x + gutterW
              pw = max 10 (w - gutterW - 4)
          withClip r $ forM_ visible $ \(i, ti) -> do
            let ry = y + fromIntegral i * threadRowH - scrollY
                rowRect = Rect px (ry + 2) pw (threadRowH - 4)
                focused = focus == Just (tiId ti)
            when focused $ drawRect (Rect x ry w threadRowH) (alpha 0.15 (pSelection pal))
            drawTextClipped (mmFont mm) (mmCharW mm) (V2 (x + 6) (ry + threadRowH / 2)) (gutterW - 12) (threadName ti) (if focused then pAccent pal else pText pal)
            drawTrack pal v rowRect (tiTrack ti) [(kindRun, pRun pal), (kindBlocked, pBlocked pal), (kindRunnable, pRunnable pal)] False
            case sel of
              Just (SelSpan (OnThread tid) k) | tid == tiId ti -> outlineSpan pal v rowRect (tiTrack ti) k
              _ -> pure ()
          -- Markers through the rows, and a scroll indicator.
          withClip (Rect px y pw h) $ forM_ (markersIn tl v) $ \mk -> when (mkIsMarker mk) $ do
            let mx = timeToX v px pw (fromIntegral (mkTime mk))
            drawStroke (V2 mx y) (V2 mx (y + h)) 1 (alpha 0.35 (pMarker pal))
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
    navigate st tl v resp (plotX r) (plotW r) False
    clicked <- isClick st resp
    when clicked $ do
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

legendDot :: Color -> Text -> NanoUI ()
legendDot c txt = rowWith (tight . gap 4 . alignMid) $ do
  _ <- canvas (fixedWH 10 10) (\r -> drawRoundedRect r 2 c)
  labelWith (fontMuted . fontSizeScale 0.9) txt

-- ---------------------------------------------------------------------------
-- Inspector

inspector :: TimelineState -> Timeline -> TimeView -> Maybe Sel -> NanoUI ()
inspector st tl v sel = case sel of
  Just (SelSpan ref i) -> scope $ spanInspector st tl ref i
  Just (SelGC i) | i < V.length (tlGCs tl) -> scope $ gcInspector st tl (tlGCs tl V.! i)
  _ -> scope $ overview st tl v

zoomTo :: TimelineState -> Timeline -> Word64 -> Word64 -> NanoUI ()
zoomTo st tl s e = do
  let pad = max 2000 (fromIntegral (e - s) * 1.5)
      lo = fromIntegral (tlStart tl)
      hi = fromIntegral (tlEnd tl)
  writeVar (tsView st) (Just (clampView lo hi (TimeView (fromIntegral s - pad) (fromIntegral e + pad))))

spanInspector :: TimelineState -> Timeline -> TrackRef -> Int -> NanoUI ()
spanInspector st tl ref i = do
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
      rowWith (tight . gap 8 . alignMid . fillW) $ do
        heading kindName
        flex
        whenM (buttonWith tight "Clear") (writeVar (tsSel st) Nothing)
      case ref of
        OnCap c -> kv "Capability" (tshow c)
        OnThread _ -> pure ()
      unless (srKind s == kindGC) $ kv "Thread" (threadLabel tl tid)
      kv "Starts at" (fmtNs (fromIntegral (srStart s - tlStart tl)))
      kv "Duration" (fmtNs dur)
      unless (srKind s == kindGC) $ kv (if srKind s == kindRun then "Ended because" else "Reason") (stopName (srStop s))
      scope $ when (srAux s /= 0) $ do
        label ("Waiting on a black hole being evaluated by " <> threadLabel tl (srAux s) <> ".")
        whenM (button "Show owner thread") (writeVar (tsFocusThread st) (Just (srAux s)))
      scope $ for_ (seriesRateBetween (tlAlloc tl) (srStart s) (srEnd s)) $ \rate ->
        kv "Allocation rate" (fmtRate rate <> " (whole process)")
      rowWith (tight . gap 6) $ do
        whenM (button "Zoom to slice") (zoomTo st tl (srStart s) (srEnd s))
        scope $ unless (srKind s == kindGC) $ whenM (button "Show thread row") (writeVar (tsFocusThread st) (Just tid))
      separator
      scope $ for_ (if srKind s == kindGC then Nothing else M.lookup tid (tlThreads tl)) (threadStats tl)
      hint "Stack traces are not recorded in the eventlog. To see where a thread is, label it (GHC.Conc.labelThread), drop traceMarkerIO calls, or step it in the Debugger tab."
    _ -> hint "The selection is no longer in the log."

threadStats :: Timeline -> ThreadInfo -> NanoUI ()
threadStats tl ti = do
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
        labelWith (fillW . if stopIsBlocked code then fontTone Danger else fontMuted) (stopName code)
        labelWith fontMono (fmtNs (fromIntegral ns))
        labelWith (fontMuted . fixedW 52) (fmtPct (fromIntegral ns / life))

gcInspector :: TimelineState -> Timeline -> GCEvent -> NanoUI ()
gcInspector st tl g = do
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    heading ("Garbage collection" <> genText (gcGen g))
    flex
    whenM (buttonWith tight "Clear") (writeVar (tsSel st) Nothing)
  let pause = fromIntegral (gcEnd g - gcStart g)
  kv "Starts at" (fmtNs (fromIntegral (gcStart g - tlStart tl)))
  kv "Pause" (fmtNs pause)
  kv "Copied" (fmtBytes (gcCopied g))
  kv "GC threads" (tshow (gcParThreads g))
  for_ (seriesValueAt (tlLive tl) (gcEnd g)) $ \b -> kv "Live after" (fmtBytes b)
  whenM (button "Zoom to collection") (zoomTo st tl (gcStart g) (gcEnd g))
  hint $
    if gcGen g >= 1
      then "A major collection copies (or marks) the whole live heap: its pause grows with residency. Frequent major GCs with rising live bytes usually mean a space leak; see the Heap tab."
      else "A minor collection only scans the nursery. Many of them mean a high allocation rate; a larger -A shrinks their number."

overview :: TimelineState -> Timeline -> TimeView -> NanoUI ()
overview st tl v = do
  heading "Inspector"
  hint "Click a slice on a capability, a thread row or the GC strip to inspect it. Wheel zooms, drag pans, double-click fits."
  kv "Window" (fmtNs (viewWidth v))
  separator
  sectionTitle "Longest GC pauses"
  let gcs = take 8 (sortOn (\(_, g) -> Down (gcEnd g - gcStart g)) (zip [0 :: Int ..] (V.toList (tlGCs tl))))
  forM_ gcs $ \(i, g) -> withKey i $ do
    picked <- selectableRow False id $ do
      labelWith (fillW . fontMono) (fmtNs (fromIntegral (gcEnd g - gcStart g)))
      labelWith fontMuted (if gcGen g >= 1 then "major" else "minor")
      labelWith (fontMuted . fixedW 80) ("@ " <> fmtNs (fromIntegral (gcStart g - tlStart tl)))
    when picked $ do
      writeVar (tsSel st) (Just (SelGC i))
      zoomTo st tl (gcStart g) (gcEnd g)
  separator
  sectionTitle "Most blocked threads"
  let blockedTime ti = sum [ns | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c]
      worst = take 8 (sortOn (Down . blockedTime) (filter ((> 0) . blockedTime) (M.elems (tlThreads tl))))
  forM_ worst $ \ti -> withKey (tiId ti) $ do
    let top = listToMaybe (sortOn (Down . snd) [(c, ns) | (c, ns) <- M.toList (tiBlocked ti), stopIsBlocked c])
    picked <- selectableRow False id $ do
      labelWith fillW (threadName ti)
      labelWith fontMono (fmtNs (fromIntegral (blockedTime ti)))
    scope $ for_ top $ \(c, _) -> labelWith (fontMuted . fontSizeScale 0.85 . padLeft 12) (stopName c)
    when picked (writeVar (tsFocusThread st) (Just (tiId ti)))
  separator
  sectionTitle "Busiest threads"
  forM_ (take 6 (sortOn (Down . tiRunTime) (M.elems (tlThreads tl)))) $ \ti -> withKey (tiId ti) $ do
    picked <- selectableRow False id $ do
      labelWith fillW (threadName ti)
      labelWith fontMono (fmtNs (fromIntegral (tiRunTime ti)))
    when picked (writeVar (tsFocusThread st) (Just (tiId ti)))
  separator
  sectionTitle "Program"
  forM_ (tlInfo tl) $ \(k, val) -> kv k (T.take 300 val)
  focus <- readVar (tsFocusThread st)
  scope $ when (isJust focus) $ whenM (button "Clear thread highlight") (writeVar (tsFocusThread st) Nothing)
