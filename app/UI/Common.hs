-- | Small pieces shared by the panels: colours, the time-axis mapping,
-- selectable list rows, stat tiles, and canvas helpers.
module UI.Common
  ( -- * Colours
    Palette (..)
  , palette
  , bandColor
  , alpha
    -- * Time axis
  , TimeView (..)
  , viewWidth
  , timeToX
  , xToTime
  , zoomAround
  , clampView
  , niceStep
    -- * Widgets
  , selectableRow
  , statTile
  , fact
  , factMono
  , sectionTitle
  , hint
  , badge
  , pathLabel
  , monoMetrics
  , MonoMetrics (..)
  , drawTextClipped
  , drawLineChart
  , ChartSeries (..)
  , fillRow
  , asRow
  , fitText
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Path qualified as P
import System.FilePath (takeFileName)

-- ---------------------------------------------------------------------------
-- Colours

-- | Semantic colours for traces, picked to read on both light and dark
-- themes.
data Palette = Palette
  { pRun :: !Color
  , pGC :: !Color
  , pGCMajor :: !Color
  , pBlocked :: !Color
  , pRunnable :: !Color
  , pTrackBg :: !Color
  , pGrid :: !Color
  , pText :: !Color
  , pMuted :: !Color
  , pAccent :: !Color
  , pCursor :: !Color
  , pMarker :: !Color
  , pSelection :: !Color
  , pMigrate :: !Color
  , pSteal :: !Color
  , pWakeup :: !Color
  , pLive :: !Color
  , pHeap :: !Color
  }

palette :: Theme -> Palette
palette th =
  Palette
    { pRun = colorRGB 76 175 80
    , pGC = colorRGB 240 192 48
    , pGCMajor = colorRGB 236 112 40
    , pBlocked = colorRGB 220 64 64
    , pRunnable = alpha 0.35 (themeMuted th)
    , pTrackBg = alpha 0.06 (styleFg (themePanel th))
    , pGrid = alpha 0.12 (styleFg (themePanel th))
    , pText = styleFg (themePanel th)
    , pMuted = themeMuted th
    , pAccent = themeAccent th
    , pCursor = alpha 0.75 (styleFg (themePanel th))
    , pMarker = themePurple th
    , pSelection = themeAccent th
    , pMigrate = themePurple th
    , pSteal = themeOrange th
    , pWakeup = colorRGB 80 150 230
    , pLive = themeAccent th
    , pHeap = alpha 0.35 (themeMuted th)
    }

-- | A stable categorical colour for a band: by rank for the first twelve,
-- by name hash after that.
bandColor :: Int -> Text -> Color
bandColor i name
  | i < length cats = cats !! i
  | otherwise = cats !! (T.foldl' (\h c -> (h * 31) `xor` ord c) 7 name `mod` length cats)
  where
    cats =
      [ colorRGB 66 133 244
      , colorRGB 234 67 53
      , colorRGB 251 188 4
      , colorRGB 52 168 83
      , colorRGB 171 71 188
      , colorRGB 0 172 193
      , colorRGB 255 112 67
      , colorRGB 158 157 36
      , colorRGB 92 107 192
      , colorRGB 240 98 146
      , colorRGB 0 137 123
      , colorRGB 121 85 72
      ]

-- | 'withAlpha' with the opacity first, for composing.
alpha :: Float -> Color -> Color
alpha a c = withAlpha c a

-- ---------------------------------------------------------------------------
-- Time axis

-- | The visible time window, in nanoseconds.
data TimeView = TimeView {tvStart :: !Double, tvEnd :: !Double}
  deriving (Eq, Show)

viewWidth :: TimeView -> Double
viewWidth v = max 1 (tvEnd v - tvStart v)

-- | Map a time to an x coordinate inside @[x0, x0 + w]@.
timeToX :: TimeView -> Float -> Float -> Double -> Float
timeToX v x0 w t = x0 + realToFrac ((t - tvStart v) / viewWidth v) * w

xToTime :: TimeView -> Float -> Float -> Float -> Double
xToTime v x0 w x = tvStart v + realToFrac ((x - x0) / max 1 w) * viewWidth v

-- | Zoom by @factor@ (< 1 zooms in) keeping time @t@ under the pointer.
zoomAround :: Double -> Double -> TimeView -> TimeView
zoomAround factor t (TimeView a b) = TimeView (t - (t - a) * factor) (t + (b - t) * factor)

-- | Keep a view inside @[lo, hi]@ and no narrower than 1 µs.
clampView :: Double -> Double -> TimeView -> TimeView
clampView lo hi (TimeView a b)
  | hi <= lo = TimeView lo (lo + 1)
  | otherwise =
      let w = min (hi - lo) (max 1000 (b - a))
          a' = max lo (min (hi - w) a)
       in TimeView a' (a' + w)

-- | A 1-2-5 tick step giving roughly @target@ units per tick.
niceStep :: Double -> Double
niceStep target
  | target <= 0 = 1
  | otherwise =
      let e = 10 ** fromIntegral (floor (logBase 10 target) :: Int)
          m = target / e
       in e * (if m < 1.5 then 1 else if m < 3.5 then 2 else if m < 7.5 then 5 else 10)

-- ---------------------------------------------------------------------------
-- Widgets

asRow :: Layout -> Layout
asRow l = l {layoutDirection = Row}

-- | A full-width clickable row with a selection background. The panel is
-- always present (transparent when unselected) so widget ids stay stable.
selectableRow :: Bool -> (Layout -> Layout) -> NanoUI () -> NanoUI Bool
selectableRow selected f body = do
  th <- uiTheme
  let bg = if selected then themeSelection th else colorTransparent
  (_, area) <- mouseArea (fillW . tight) $
    styled (panelStyle (background bg . borderWidth 0 . cornerRadius 4)) $
      panelWith (asRow . fillW . padXY 8 3 . gap 8 . alignMid . f) body
  pure (respClicked area)

statTile :: Text -> Text -> NanoUI ()
statTile name value =
  panelWith (padXY 10 6 . gap 2 . minW 110) $ do
    labelWith (fontMuted . fontSizeScale 0.85) name
    labelWith (fontMedium . fontSizeScale 1.1) value

sectionTitle :: Text -> NanoUI ()
sectionTitle = labelWith (fontMedium . padTop 4)

hint :: Text -> NanoUI ()
hint = labelWith (fontMuted . fontSizeScale 0.9 . fillW)

badge :: Tone -> Text -> NanoUI ()
badge t txt = do
  th <- uiTheme
  let c = toneColor th t
  styled (panelStyle (background (alpha 0.18 c) . borderWidth 0 . cornerRadius 8)) $
    panelWith (padXY 6 1) (labelWith (fontColor c . fontSizeScale 0.8) txt)

pathLabel :: Maybe FilePath -> NanoUI ()
pathLabel = \case
  Nothing -> labelWith fontMuted "no file"
  Just p -> labelWith fontMono (T.pack (takeFileName p))

fillRow :: NanoUI a -> NanoUI a
fillRow = rowWith (tight . gap 8 . alignMid . fillW)

-- | Monospace metrics for canvases that lay out text themselves.
data MonoMetrics = MonoMetrics
  { mmFont :: !TextFont
  , mmCharW :: !Float
  , mmLineH :: !Float
  }

monoMetrics :: Float -> NanoUI MonoMetrics
monoMetrics scale = do
  base <- uiFontSize
  let size = base * scale
  fm <- resolveFontUi size WeightNormal FontStyleNormal FontMono
  w <- lineWidthUi fm "MMMMMMMMMM"
  pure (MonoMetrics defaultTextFont {textFontSize = size, textFontVariant = FontMono} (w / 10) (fmLineHeight fm))

-- | Text cut to a pixel width by character count (exact for monospace,
-- approximate otherwise).
fitText :: Float -> Float -> Text -> Text
fitText charW width t
  | charW <= 0 = t
  | fromIntegral (T.length t) * charW <= width = t
  | width < charW * 2 = ""
  | otherwise = T.take (max 0 (floor (width / charW) - 1)) t <> "…"

drawTextClipped :: TextFont -> Float -> V2 -> Float -> Text -> Color -> CanvasM ()
drawTextClipped font charW (V2 x y) width txt c =
  let t = fitText charW width txt
   in if T.null t then pure () else drawTextWith font (V2 x y) AlignStart AlignMiddle t c

data ChartSeries = ChartSeries
  { csName :: !Text
  , csColor :: !Color
  , csPoints :: ![(Double, Double)]
  , csFill :: !Bool
  }

-- | A minimal line chart over @[x0, x1]@ with a zero-based y axis.
drawLineChart :: Palette -> TextFont -> Rect -> (Double, Double) -> (Double -> Text) -> [ChartSeries] -> CanvasM ()
drawLineChart pal font r@(Rect x y w h) (x0, x1) fmtY series = do
  drawRoundedRect r 6 (pTrackBg pal)
  let ymax = maximum (1e-9 : [v | s <- series, (_, v) <- csPoints s])
      px t = x + 4 + realToFrac ((t - x0) / max 1e-9 (x1 - x0)) * (w - 8)
      py v = y + h - 4 - realToFrac (v / ymax) * (h - 20)
  drawStroke (V2 x (y + h - 4)) (V2 (x + w) (y + h - 4)) 1 (pGrid pal)
  withClip r $ mapM_ (one px py) series
  drawTextWith font (V2 (x + 6) (y + 9)) AlignStart AlignMiddle (fmtY ymax) (pMuted pal)
  where
    one px py s = case csPoints s of
      pts@(_ : _ : _) -> do
        let ps = [V2 (px t) (py v) | (t, v) <- pts]
        if csFill s
          then
            let V2 fx _ = case ps of { p : _ -> p; [] -> V2 x y }
                V2 lx _ = last ps
             in drawPath (P.polygon (V2 fx (y + h - 4) : ps ++ [V2 lx (y + h - 4)])) (alpha 0.22 (csColor s))
          else pure ()
        drawStrokePathWith (P.stroke 1.5) {P.strokeJoin = P.RoundJoin} (P.polyline ps) (P.Solid (csColor s))
      _ -> pure ()

-- | A compact "key value" pair for wrapping rows ('kv' spreads its value to
-- the far edge, which only suits columns).
fact :: Text -> Text -> NanoUI ()
fact k v = rowWith (tight . gap 4 . alignMid) $ do
  labelWith fontMuted k
  label v

factMono :: Text -> Text -> NanoUI ()
factMono k v = rowWith (tight . gap 4 . alignMid) $ do
  labelWith fontMuted k
  labelWith fontMono v
