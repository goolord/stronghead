-- | Interactive flamegraphs (On-CPU, Off-CPU, Cost-Centre, and Differential),
-- 2D HDR latency heatmaps, and Era cohort retention heatmaps rendered on
-- @nano-ui@ canvases and exportable headlessly via @nano-ui-svg@.
module UI.Flamegraph
  ( flamegraphView
  , diffFlamegraphView
  , latencyHeatmapView
  , eraCohortHeatmapView
  , drawFlameTree
  , drawDiffFlameTree
  , drawLatencyHeatmap
  ) where

import Control.Monad (forM_, when)
import Data.Char (ord)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import NanoUI

import Stronghead.Eventlog.CriticalPath (LatencyHeatmap (..))
import Stronghead.Flamegraph
import Stronghead.Format (fmtBytesD, fmtFixed, fmtInt, fmtNs, fmtPct, fmtSec, readDecimal, tshow)
import Stronghead.HeapProfile (Band (..), HeapProfile (..), hpOffsets)
import UI.Common

-- ---------------------------------------------------------------------------
-- Interactive Flamegraph / Icicle View

-- | Draw an interactive icicle/flamegraph for 'FlameTree'. Clicking any bar
-- zooms into that frame (stored in @zoomVar@); clicking Reset zooms back out.
flamegraphView :: FlameTree -> Var (Maybe Text) -> NanoUI ()
flamegraphView ft zoomVar = do
  th <- uiTheme
  mm <- monoMetrics 0.82
  zoomed <- readVar zoomVar
  let pal = palette th
      viewTree = maybe ft (\target -> fromMaybe ft (zoomInto target ft)) zoomed
  columnWith (tight . fillW . fillH . gap 6) $ do
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      labelWith (fontMedium . fontSizeScale 0.92) (ftTitle viewTree)
      labelWith (fontMuted . fontSizeScale 0.85)
        (fmtFixed 0 (ftTotal viewTree) <> " " <> ftUnit viewTree)
      flex
      scope $ case zoomed of
        Just z -> do
          labelWith (fontMono . fontMuted . fontSizeScale 0.82) ("zoom: " <> T.take 36 z)
          whenM (buttonWith (padXY 8 2 . tight) "Reset zoom") (writeVar zoomVar Nothing)
        Nothing -> pure ()
    if null (ftRoots viewTree) || ftTotal viewTree <= 0
      then hint "No stack samples available for this view."
      else do
        (_, mClicked) <-
          canvasRegions
            (fillW . grow . minH 180)
            (\r -> flameRegions r viewTree)
            $ \r mHover -> do
              drawFlameTree pal mm r viewTree
              case mHover of
                Just (_, Rect hx hy hw hh) ->
                  drawStrokeRoundedRect (Rect (hx + 0.5) (hy + 0.5) (max 1 (hw - 2)) (max 1 (hh - 2))) 2 1.5 (pText pal)
                Nothing -> pure ()
        case mClicked of
          Just n -> writeVar zoomVar (Just (fnName n))
          Nothing -> pure ()

-- | Draw a differential flamegraph comparing baseline vs current: red frames
-- regressed, blue frames improved, neutral frames stayed flat.
diffFlamegraphView :: DiffFlameTree -> NanoUI ()
diffFlamegraphView dft = do
  th <- uiTheme
  mm <- monoMetrics 0.82
  let pal = palette th
  columnWith (tight . fillW . fillH . gap 6) $ do
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      labelWith (fontMedium . fontSizeScale 0.92) (dftTitle dft)
      flex
      badge Danger "regressed"
      badge Success "improved"
    if null (dftRoots dft)
      then hint "No differential flamegraph frames available."
      else do
        _ <-
          canvasRegions
            (fillW . grow . minH 180)
            (\r -> diffFlameRegions r dft)
            $ \r mHover -> do
              drawDiffFlameTree pal mm r dft
              case mHover of
                Just (_, Rect hx hy hw hh) ->
                  drawStrokeRoundedRect (Rect (hx + 0.5) (hy + 0.5) (max 1 (hw - 2)) (max 1 (hh - 2))) 2 1.5 (pText pal)
                Nothing -> pure ()
        pure ()

flameRegions :: Rect -> FlameTree -> [(FlameNode, Rect, Maybe Text, UiCursorKind)]
flameRegions r ft =
  [ (node, box, Just tip, UiCursorPointer)
  | (box, node) <- layoutFlame r ft
  , let pct = if ftTotal ft > 0 then fnTotal node / ftTotal ft else 0
        tip =
          fnName node
            <> "\ntotal: "
            <> fmtFixed 1 (fnTotal node)
            <> " ("
            <> fmtPct pct
            <> "), self: "
            <> fmtFixed 1 (fnSelf node)
            <> "\nClick to zoom"
  ]

diffFlameRegions :: Rect -> DiffFlameTree -> [(DiffFlameNode, Rect, Maybe Text, UiCursorKind)]
diffFlameRegions r dft =
  [ (node, box, Just tip, UiCursorDefault)
  | (box, node) <- layoutDiffFlame r dft
  , let tip =
          dfnName node
            <> "\nbase: "
            <> fmtFixed 1 (dfnBaseTotal node)
            <> " → cur: "
            <> fmtFixed 1 (dfnCurTotal node)
            <> " (delta "
            <> (if dfnDelta node >= 0 then "+" else "")
            <> fmtFixed 1 (dfnDelta node)
            <> ")"
  ]

-- | Interactive canvas over typed hit regions @[(a, Rect, Maybe Text, UiCursorKind)]@
-- (last matching region wins), handling hover highlight, press-and-release click
-- matching on the same region, pointer cursor, and tooltip.
canvasRegions ::
  (Layout -> Layout) ->
  (Rect -> [(a, Rect, Maybe Text, UiCursorKind)]) ->
  (Rect -> Maybe (a, Rect) -> CanvasM ()) ->
  NanoUI (Response, Maybe a)
canvasRegions layoutMod mkRegions draw = do
  inp <- askInput
  let mouse = inputMousePos inp
  pressedIdxVar <- useVar (-1 :: Int)
  sid <- currentId
  prev <- fromMaybe (Rect 0 0 0 0) <$> lastRect sid
  let hitIn rs pt =
        foldl'
          (\acc entry@(_, (_, box, _, _)) -> if rectContains box pt then Just entry else acc)
          Nothing
          rs
      prevHit = if rectContains prev mouse then hitIn (zip [0 :: Int ..] (mkRegions prev)) mouse else Nothing
      mHover = fmap (\(_, (k, box, _, _)) -> (k, box)) prevHit
      cursorAt _ r pos =
        case hitIn (zip [0 :: Int ..] (mkRegions r)) pos of
          Just (_, (_, _, _, cur)) -> cur
          Nothing -> UiCursorDefault
  resp <-
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = layoutMod defaultLayout
        , canvasTrackPointer = True
        , canvasCursor = Just cursorAt
        }
      (\r -> draw r mHover)
  let curHit = if respHovered resp then hitIn (zip [0 :: Int ..] (mkRegions (respRect resp))) mouse else Nothing
  when (respHovered resp && pressedIn MouseLeft inp) $
    writeVar pressedIdxVar (maybe (-1) fst curHit)
  pressedIdx <- readVar pressedIdxVar
  clicked <-
    if releasedIn MouseLeft inp
      then do
        writeVar pressedIdxVar (-1)
        pure $ case curHit of
          Just (idx, (k, _, _, _)) | idx == pressedIdx && idx >= 0 -> Just k
          _ -> Nothing
      else pure Nothing
  case curHit of
    Just (_, (_, _, Just tip, _)) -> tooltip resp tip
    _ -> pure ()
  pure (resp, clicked)

-- | Render a 'FlameTree' onto a @nano-ui@ canvas (usable live or in SVG export).
drawFlameTree :: Palette -> MonoMetrics -> Rect -> FlameTree -> CanvasM ()
drawFlameTree pal mm r ft = withClip r $ do
  drawRect r (pTrackBg pal)
  forM_ (layoutFlame r ft) $ \(Rect bx by bw bh, node) -> do
    let col = flameColor (fnName node)
    drawRoundedRect (Rect bx by (max 1 (bw - 1)) (max 1 (bh - 1))) 2 col
    when (bw >= 36) $
      drawTextClippedWith (mmMetrics mm) (mmFont mm) EllipsisMiddle (Rect (bx + 4) by (bw - 8) bh) AlignStart AlignMiddle (fnName node) (colorRGB 20 22 26)

drawDiffFlameTree :: Palette -> MonoMetrics -> Rect -> DiffFlameTree -> CanvasM ()
drawDiffFlameTree pal mm r dft = withClip r $ do
  drawRect r (pTrackBg pal)
  forM_ (layoutDiffFlame r dft) $ \(Rect bx by bw bh, node) -> do
    let denom = max 1 (max (dfnBaseTotal node) (dfnCurTotal node))
        ratio = max (-1) (min 1 (dfnDelta node / denom))
        col
          | ratio > 0.05 = blendColor (colorRGB 215 215 220) (colorRGB 235 87 87) (realToFrac ratio)
          | ratio < -0.05 = blendColor (colorRGB 215 215 220) (colorRGB 72 156 235) (realToFrac (negate ratio))
          | otherwise = colorRGB 190 194 202
    drawRoundedRect (Rect bx by (max 1 (bw - 1)) (max 1 (bh - 1))) 2 col
    when (bw >= 36) $
      drawTextClippedWith (mmMetrics mm) (mmFont mm) EllipsisMiddle (Rect (bx + 4) by (bw - 8) bh) AlignStart AlignMiddle (dfnName node) (colorRGB 20 22 26)

layoutFlame :: Rect -> FlameTree -> [(Rect, FlameNode)]
layoutFlame (Rect rx ry rw rh) ft =
  let tot = max 1e-9 (ftTotal ft)
      rowH = 20 :: Float
      maxDepth = max (1 :: Int) (floor (rh / rowH))
      go depth x0 availW nodes
        | depth >= maxDepth || availW < 1.5 = []
        | otherwise = snd (foldl' step (x0, []) nodes)
        where
          step (x, acc) n =
            let w = realToFrac (fnTotal n / tot) * rw
                box = Rect x (ry + fromIntegral depth * rowH) w rowH
                kids = if w >= 2 then go (depth + 1) x w (fnChildren n) else []
             in (x + w, if w >= 1 then acc ++ ((box, n) : kids) else acc)
   in go 0 rx rw (ftRoots ft)

layoutDiffFlame :: Rect -> DiffFlameTree -> [(Rect, DiffFlameNode)]
layoutDiffFlame (Rect rx ry rw rh) dft =
  let tot = max 1e-9 (max (dftBaseTotal dft) (dftCurTotal dft))
      rowH = 20 :: Float
      maxDepth = max (1 :: Int) (floor (rh / rowH))
      go depth x0 availW nodes
        | depth >= maxDepth || availW < 1.5 = []
        | otherwise = snd (foldl' step (x0, []) nodes)
        where
          step (x, acc) n =
            let weight = max (dfnBaseTotal n) (dfnCurTotal n)
                w = realToFrac (weight / tot) * rw
                box = Rect x (ry + fromIntegral depth * rowH) w rowH
                kids = if w >= 2 then go (depth + 1) x w (dfnChildren n) else []
             in (x + w, if w >= 1 then acc ++ ((box, n) : kids) else acc)
   in go 0 rx rw (dftRoots dft)

zoomInto :: Text -> FlameTree -> Maybe FlameTree
zoomInto target ft = case findNode (ftRoots ft) of
  Nothing -> Nothing
  Just n -> Just ft {ftTotal = fnTotal n, ftRoots = [n]}
  where
    findNode [] = Nothing
    findNode (n : ns)
      | fnName n == target = Just n
      | otherwise = case findNode (fnChildren n) of
          Just found -> Just found
          Nothing -> findNode ns

flameColor :: Text -> Color
flameColor name =
  let h = T.foldl' (\acc c -> (acc * 33 + ord c) `mod` 997) (17 :: Int) name
      r = 210 + (h `mod` 40)
      g = 115 + ((h `div` 7) `mod` 95)
      b = 65 + ((h `div` 19) `mod` 60)
   in colorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

blendColor :: Color -> Color -> Float -> Color
blendColor a b t0 =
  let t = max 0 (min 1 t0)
      lerp x y = round (fromIntegral x + (fromIntegral y - fromIntegral x) * t)
   in colorRGB (lerp (colorR a) (colorR b)) (lerp (colorG a) (colorG b)) (lerp (colorB a) (colorB b))

-- ---------------------------------------------------------------------------
-- 2D HDR Latency Heatmap View

latencyHeatmapView :: LatencyHeatmap -> NanoUI ()
latencyHeatmapView hm = do
  th <- uiTheme
  mm <- monoMetrics 0.8
  inp <- askInput
  let pal = palette th
      mouse = inputMousePos inp
  columnWith (tight . fillW . gap 4) $ do
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      labelWith (fontMedium . fontSizeScale 0.9) (hmTitle hm)
      labelWith (fontMuted . fontSizeScale 0.82)
        (fmtInt (hmSampleCount hm) <> " samples · " <> fmtNs (hmMinNs hm) <> " .. " <> fmtNs (hmMaxNs hm))
      flex
    if hmSampleCount hm == 0
      then hint "No samples in this latency heatmap."
      else do
        resp <-
          canvasConfigured
            defaultCanvasConfig
              { canvasLayout = (fillW . fixedH 132) defaultLayout
              , canvasTrackPointer = True
              }
            (\r -> drawLatencyHeatmap pal mm r hm)
        let Rect rx ry rw rh = respRect resp
            padL = 68 :: Float
            plotX = rx + padL
            plotW = max 1 (rw - padL - 6)
            plotH = max 1 (rh - 8)
            tb = max 1 (hmTimeBins hm)
            vb = max 1 (hmValueBins hm)
            V2 mx my = mouse
        when (respHovered resp && mx >= plotX && mx <= plotX + plotW && my >= ry + 4 && my <= ry + 4 + plotH) $ do
          let tIdx = max 0 (min (tb - 1) (floor ((mx - plotX) / (plotW / fromIntegral tb))))
              vIdx = max 0 (min (vb - 1) (vb - 1 - floor ((my - (ry + 4)) / (plotH / fromIntegral vb))))
              cnt = hmCounts hm VU.! (tIdx * vb + vIdx)
              loNs = hmMinNs hm + (hmMaxNs hm - hmMinNs hm) * (fromIntegral vIdx / fromIntegral vb)
              hiNs = hmMinNs hm + (hmMaxNs hm - hmMinNs hm) * (fromIntegral (vIdx + 1) / fromIntegral vb)
          tooltip resp ("Bucket " <> fmtNs loNs <> " – " <> fmtNs hiNs <> "\n" <> fmtInt cnt <> (if cnt == 1 then " sample" else " samples"))

drawLatencyHeatmap :: Palette -> MonoMetrics -> Rect -> LatencyHeatmap -> CanvasM ()
drawLatencyHeatmap pal mm r@(Rect rx ry rw rh) hm = withClip r $ do
  drawRect r (pTrackBg pal)
  let padL = 68 :: Float
      plotX = rx + padL
      plotW = max 1 (rw - padL - 6)
      plotH = max 1 (rh - 8)
      tb = max 1 (hmTimeBins hm)
      vb = max 1 (hmValueBins hm)
      cw = plotW / fromIntegral tb
      ch = plotH / fromIntegral vb
      maxC = max 1 (hmMaxCount hm)
  forM_ [0 .. tb - 1] $ \tIdx ->
    forM_ [0 .. vb - 1] $ \vIdx -> do
      let cnt = hmCounts hm VU.! (tIdx * vb + vIdx)
      when (cnt > 0) $ do
        let intensity = log (1 + fromIntegral cnt) / log (1 + fromIntegral maxC) :: Double
            col = blendColor (alpha 0.35 (pAccent pal)) (colorRGB 245 110 55) (realToFrac intensity)
            x = plotX + fromIntegral tIdx * cw
            y = ry + 4 + fromIntegral (vb - 1 - vIdx) * ch
        drawRect (Rect x y (max 1 (cw - 0.5)) (max 1 (ch - 0.5))) col
  drawTextWith (mmFont mm) (V2 (rx + 4) (ry + 10)) AlignStart AlignMiddle (fmtNs (hmMaxNs hm)) (pMuted pal)
  drawTextWith (mmFont mm) (V2 (rx + 4) (ry + rh - 10)) AlignStart AlignMiddle (fmtNs (hmMinNs hm)) (pMuted pal)

-- ---------------------------------------------------------------------------
-- Era Cohort Retention Heatmap (+RTS -he)

-- | Render a 2D Cohort Retention Matrix for an era heap profile (@+RTS -he@):
-- X-axis = census time, Y-axis = allocation Era. Healthy short-lived data
-- decays along the diagonal; leaking cohorts glow as horizontal streaks.
eraCohortHeatmapView :: HeapProfile -> NanoUI ()
eraCohortHeatmapView hp = do
  th <- uiTheme
  mm <- monoMetrics 0.8
  inp <- askInput
  let pal = palette th
      light = themeAppearance th == AppearanceLight
      lowCol = if light then colorRGB 178 210 245 else colorRGB 38 78 112
      mouse = inputMousePos inp
      eraBands =
        sortOn fst
          [ (eraNum, b)
          | b <- V.toList (hpBands hp)
          , let cleaned = fromMaybe (bandName b) (T.stripPrefix "era " (T.toLower (bandName b)))
          , Just eraNum <- [readDecimal cleaned :: Maybe Int]
          ]
      nTimes = VU.length (hpTimes hp)
      maxVal = maximum (1 : [bandPeak b | (_, b) <- eraBands])
      rows = take 48 eraBands
  columnWith (tight . fillW . fillH . gap 6) $ do
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      labelWith (fontMedium . fontSizeScale 0.92) "Era cohort retention matrix (+RTS -he)"
      labelWith (fontMuted . fontSizeScale 0.84)
        (tshow (length eraBands) <> " eras × " <> tshow nTimes <> " censuses · peak " <> fmtBytesD maxVal)
    if null eraBands || nTimes == 0
      then hint "No era cohorts found. Run with +RTS -l -he -i0.1 and call Stronghead.Agent.bumpEra per request or phase."
      else do
        resp <-
          canvasConfigured
            defaultCanvasConfig
              { canvasLayout = (fillW . grow . minH 180) defaultLayout
              , canvasTrackPointer = True
              }
            $ \r@(Rect rx ry rw rh) -> withClip r $ do
              drawRect r (pTrackBg pal)
              let padL = 56 :: Float
                  px = rx + padL
                  pw = max 1 (rw - padL - 8)
                  ph = max 1 (rh - 8)
                  nRows = max 1 (length rows)
                  cw = pw / fromIntegral (max 1 nTimes)
                  ch = ph / fromIntegral nRows
              forM_ (zip [0 :: Int ..] rows) $ \(ri, (eraNum, band)) -> do
                let y = ry + 4 + fromIntegral ri * ch
                when (ch >= 11 || ri == 0 || ri == nRows - 1) $
                  drawTextWith (mmFont mm) (V2 (rx + 4) (y + ch / 2)) AlignStart AlignMiddle ("era " <> tshow eraNum) (pMuted pal)
                forM_ [0 .. nTimes - 1] $ \ti -> do
                  let v = if ti < VU.length (bandValues band) then bandValues band VU.! ti else 0
                  when (v > 0) $ do
                    let frac = sqrt (v / maxVal)
                        col = blendColor lowCol (colorRGB 245 108 48) (realToFrac frac)
                    drawRect (Rect (px + fromIntegral ti * cw) y (max 1 (cw - 0.5)) (max 1 (ch - 0.5))) col
        let Rect rx ry rw rh = respRect resp
            padL = 56 :: Float
            px = rx + padL
            pw = max 1 (rw - padL - 8)
            ph = max 1 (rh - 8)
            nRows = max 1 (length rows)
            V2 mx my = mouse
        when (respHovered resp && mx >= px && mx <= px + pw && my >= ry + 4 && my <= ry + 4 + ph) $ do
          let ti = max 0 (min (nTimes - 1) (floor ((mx - px) / (pw / fromIntegral (max 1 nTimes)))))
              ri = max 0 (min (nRows - 1) (floor ((my - (ry + 4)) / (ph / fromIntegral nRows))))
          case drop ri rows of
            (eraNum, band) : _ -> do
              let v = if ti < VU.length (bandValues band) then bandValues band VU.! ti else 0
                  tSec = if ti < VU.length (hpOffsets hp) then hpOffsets hp VU.! ti else 0
              tooltip resp ("era " <> tshow eraNum <> " at " <> fmtSec tSec <> "\n" <> fmtBytesD v <> " live")
            [] -> pure ()
