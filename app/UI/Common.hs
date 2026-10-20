-- | Small pieces shared by the panels: the theme, the workbench frame
-- (toolbars, panes, readouts, empty states), the time-axis mapping,
-- selectable list rows, and canvas helpers.
module UI.Common
  ( -- * Theme
    strongheadTheme
  , Ink (..)
  , ink
    -- * Colours
  , Palette (..)
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
    -- * Frame
  , toolStrip
  , toolLabel
  , toolButton
  , toolGroup
  , Workbench
  , workbench
  , pane
  , paneWith
  , paneMeta
  , readout
  , emptyState
  , paneScroll
  , paneScrollArea
  , codeBlock
    -- * Widgets
  , cell
  , numCell
  , colHeader
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
  , chartTimeX
  , chartXTime
  , ChartSeries (..)
  , fillRow
  , asRow
  , fitText
  ) where

import Control.Monad (forM_, void, when)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import NanoUI
import NanoUI.Path qualified as P
import System.FilePath (takeFileName)

-- ---------------------------------------------------------------------------
-- Theme

-- | The surfaces the frame is built from: plain neutral greys, so the
-- runtime's own colours (running, GC, blocked) are the only colour on
-- screen besides the accent, which marks selection and focus.
data Ink = Ink
  { inkGround :: !Color
    -- ^ Window, toolbars, the gutters between panes.
  , inkPane :: !Color
    -- ^ Pane surfaces.
  , inkSunken :: !Color
    -- ^ Inputs, code blocks, the inside of tracks.
  , inkRule :: !Color
    -- ^ Hairlines and pane borders.
  , inkText :: !Color
  , inkMuted :: !Color
  , inkAccent :: !Color
  }

ink :: Theme -> Ink
ink th = case themeAppearance th of
  AppearanceLight ->
    Ink
      { inkGround = colorRGB 240 240 240
      , inkPane = colorRGB 251 251 251
      , inkSunken = colorRGB 245 245 245
      , inkRule = colorRGB 220 220 220
      , inkText = colorRGB 30 30 30
      , inkMuted = colorRGB 108 108 108
      , inkAccent = colorRGB 38 110 216
      }
  AppearanceDark ->
    Ink
      { inkGround = colorRGB 24 24 24
      , inkPane = colorRGB 31 31 31
      , inkSunken = colorRGB 20 20 20
      , inkRule = colorRGB 50 50 50
      , inkText = colorRGB 228 228 228
      , inkMuted = colorRGB 150 150 150
      , inkAccent = colorRGB 96 160 250
      }

-- | nano-ui's stock themes, recoloured in Stronghead's ink.
strongheadTheme :: Maybe Appearance -> Theme
strongheadTheme = lightDark (recolour defaultLightTheme) (recolour defaultTheme)
  where
    recolour base =
      let i = ink base
          light = themeAppearance base == AppearanceLight
          surface bg border hover active =
            Style
              { styleBg = bg
              , styleFg = inkText i
              , styleBorder = border
              , styleBorderWidth = 1
              , styleCornerRadius = 6
              , styleHoverBg = hover
              , styleActiveBg = active
              , styleBorderSides = 15
              }
          raised = if light then colorRGB 255 255 255 else colorRGB 44 44 44
          raisedHover = if light then colorRGB 244 244 244 else colorRGB 54 54 54
          raisedActive = if light then colorRGB 232 232 232 else colorRGB 36 36 36
          control = if light then colorRGB 206 206 206 else colorRGB 64 64 64
       in base
            { themeWindow = inkGround i
            , themePanel = surface (inkPane i) (inkRule i) (inkPane i) (inkPane i)
            , themeFloatingWindow = surface (inkPane i) control (inkPane i) (inkPane i)
            , themePopup = surface (inkPane i) control raisedHover raisedActive
            , themeButton = surface raised control raisedHover raisedActive
            , themeInput = surface (inkSunken i) control (inkSunken i) (inkSunken i)
            , themeSeparator = inkRule i
            , themeAccent = inkAccent i
            , themeMuted = inkMuted i
            , themeOnAccent = if light then colorRGB 255 255 255 else inkGround i
            , themeSelection = alpha (if light then 0.20 else 0.30) (inkAccent i)
            , themeFocusRing = inkAccent i
            , themeLink = inkAccent i
            }

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
  let i = ink th
      light = themeAppearance th == AppearanceLight
   in Palette
        { pRun = if light then colorRGB 56 158 84 else colorRGB 88 186 108
        , pGC = if light then colorRGB 226 168 24 else colorRGB 240 192 64
        , pGCMajor = colorRGB 236 112 40
        , pBlocked = if light then colorRGB 214 60 66 else colorRGB 230 84 84
        , pRunnable = alpha 0.40 (inkMuted i)
        , pTrackBg = alpha (if light then 0.05 else 0.045) (inkText i)
        , pGrid = alpha 0.12 (inkText i)
        , pText = inkText i
        , pMuted = inkMuted i
        , pAccent = inkAccent i
        , pCursor = alpha 0.7 (inkText i)
        , pMarker = if light then colorRGB 0 150 136 else colorRGB 77 208 190
        , pSelection = inkAccent i
        , pMigrate = inkAccent i
        , pSteal = themeOrange th
        , pWakeup = colorRGB 84 156 236
        , pLive = inkAccent i
        , pHeap = alpha 0.30 (inkMuted i)
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

-- | A label for a row of mixed text: children of a row are aligned one by
-- one, so every label sits on the row's shared baseline whatever its face
-- or size.
cell :: (Layout -> Layout) -> Text -> NanoUI ()
cell f = labelWith (alignBaseline . f)

-- | A number in a fixed-width, right-aligned monospace column.
numCell :: Float -> (Layout -> Layout) -> Text -> NanoUI ()
numCell w f = labelWith (alignBaseline . fixedW w . alignEnd . fontMono . f)

-- | A table column header, sized like the column it heads.
colHeader :: (Layout -> Layout) -> Text -> NanoUI ()
colHeader f = labelWith (alignBaseline . fontMuted . fontSizeScale 0.85 . f)

-- | A full-width clickable row with a selection background. The panel is
-- always present (transparent when unselected) so widget ids stay stable.
selectableRow :: Bool -> (Layout -> Layout) -> NanoUI () -> NanoUI Bool
selectableRow selected f body = do
  th <- uiTheme
  let bg = if selected then themeSelection th else colorTransparent
      hover = if selected then themeSelection th else alpha 0.06 (inkText (ink th))
  (_, area) <- mouseArea (fillW . tight) $
    styled (panelStyle (background bg . hoverBackground hover . borderWidth 0 . cornerRadius 4)) $
      panelWith (asRow . fillW . padXY 8 2 . gap 8 . f) body
  pure (respClicked area)

-- | A value over its name, for a few numbers on their own; a run's vital
-- numbers belong in one 'readout'.
statTile :: Text -> Text -> NanoUI ()
statTile name value =
  columnWith (padRight 14 . tight . gap 0) $ do
    labelWith (fontMono . fontSizeScale 1.12) value
    labelWith (fontMuted . fontSizeScale 0.8) name

-- | A heading inside a pane's body.
sectionTitle :: Text -> NanoUI ()
sectionTitle = labelWith (fontMedium . fontSizeScale 0.92 . padTop 10 . padBottom 2)

hint :: Text -> NanoUI ()
hint = labelWith (fontMuted . fontSizeScale 0.9 . fillW)

badge :: Tone -> Text -> NanoUI ()
badge t txt = do
  th <- uiTheme
  let c = toneColor th t
  styled (panelStyle (background (alpha 0.16 c) . borderWidth 0 . cornerRadius 4)) $
    panelWith (alignMid . padXY 6 1) (labelWith (fontColor c . fontSizeScale 0.8) txt)

pathLabel :: Maybe FilePath -> NanoUI ()
pathLabel = \case
  Nothing -> labelWith (alignMid . fontMuted) "No file open"
  Just p -> labelWith (alignMid . fontMono . fontSizeScale 0.92) (T.pack (takeFileName p))

fillRow :: NanoUI a -> NanoUI a
fillRow = rowWith (tight . gap 8 . alignMid . fillW)

-- ---------------------------------------------------------------------------
-- Frame

-- | A panel's toolbar: one row on the pane surface, so the active folder tab
-- in the title bar opens onto it, ruled off from the panes below. Use
-- 'toolLabel', 'toolButton' and @alignMid@ controls in it so everything
-- shares one centre line.
toolStrip :: NanoUI a -> NanoUI a
toolStrip body = do
  th <- uiTheme
  styled (panelStyle (background (inkPane (ink th)) . borderBottom 1 (inkRule (ink th)) . cornerRadius 0)) $
    panelWith (asRow . fillW . padXY 8 8 . gap 8 . minH 48) (themed th body)

-- | A caption for the control after it.
toolLabel :: Text -> NanoUI ()
toolLabel = labelWith (alignMid . fontMuted . fontSizeScale 0.92)

toolButton :: Text -> NanoUI Bool
toolButton = buttonWith alignMid

-- | Controls that belong together, set off from their neighbours by a rule.
toolGroup :: NanoUI a -> NanoUI a
toolGroup body = do
  vrule 22
  rowWith (tight . gap 6 . alignMid) body

-- | The split tree of a panel's panes, kept in the panel's state so the
-- arrangement survives switching panels.
type Workbench = Var (Maybe GridNode)

-- | Resizable panes filling the rest of the panel. @view@ draws one pane
-- by id; wrap its content in 'pane'.
workbench :: Workbench -> (Word64 -> PaneGridCtx -> NanoUI ()) -> NanoUI ()
workbench tree view = do
  th <- uiTheme
  columnWith (fillW . grow . padXY 8 0 . padTop 8 . padBottom 8) $
    void $
      paneGrid
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgSpacing = 6
          , pgMinSize = 120
          , pgLeeway = 4
          , pgFocusable = False
          , pgTree = ControlledTree tree
          , pgDividerColor = Just (inkGround (ink th))
          , pgViewPane = \pid ctx -> view pid ctx >> pure (PaneView "" False)
          }

-- | A pane: a header with its title and @trailing@ (meta text, actions),
-- and a padded body. Double-click the header to maximize the pane.
pane :: PaneGridCtx -> Text -> NanoUI () -> NanoUI a -> NanoUI a
pane = paneWith (padAll 10 . gap 6)

-- | 'pane' with the body's layout, e.g. @tight@ for a canvas that fills it.
paneWith :: (Layout -> Layout) -> PaneGridCtx -> Text -> NanoUI () -> NanoUI a -> NanoUI a
paneWith bodyLayout ctx title trailing body = do
  th <- uiTheme
  let i = ink th
  styled (panelStyle (background (inkPane i) . borderColor (inkRule i) . borderWidth 1 . cornerRadius 6)) $
    panelWith (tight . gap 0 . fillW . fillH) $ do
      (_, header) <-
        styled (panelStyle (background colorTransparent . borderBottom 1 (inkRule i) . cornerRadius 0)) $
          mouseArea (fillW . tight) $
            panelWith (asRow . fillW . fixedH 34 . padXY 12 0 . gap 8) $ do
              labelWith (alignMid . fontMedium . fontSizeScale 0.92) title
              themed th trailing
      inp <- askInput
      when (respHovered header && pressedIn MouseLeft inp && inputMouseClicks inp >= 2) $
        if pgcMaximized ctx then pgcRestore ctx else pgcMaximize ctx
      themed th $ columnWith (fillW . fillH . bodyLayout) body

-- | Muted text in a pane header, after the title.
paneMeta :: Text -> NanoUI ()
paneMeta = labelWith (alignMid . fontMuted . fontSizeScale 0.85)

-- | The vital numbers of a run as one instrument line: each value over its
-- name, separated by hairlines. Wraps onto more lines when narrow.
readout :: [(Text, Text)] -> NanoUI ()
readout items =
  rowWith (padXY 20 0 . padTop 10 . tight . gap 0 . fillW . wrap . lineGap 8) $
    forM_ (zip [0 :: Int ..] items) $ \(i, (name, value)) -> withKey i $
      rowWith (tight . gap 0) $ do
        scope $ when (i > 0) (vrule 30)
        columnWith (padLeft (if i > 0 then 14 else 0) . padRight 14 . tight . gap 0) $ do
          labelWith (fontMono . fontSizeScale 1.12) value
          labelWith (fontMuted . fontSizeScale 0.8) name

-- | What a pane says before it has anything to show: a title, what to do,
-- and the commands to do it with.
emptyState :: Text -> [Text] -> [Text] -> NanoUI ()
emptyState title body commands =
  columnWith (fillW . fillH . padAll 24) $ do
    flex
    columnWith (tight . gap 10 . fixedW 640 . alignCenter) $ do
      labelWith (fontMedium . fontSizeScale 1.1) title
      forM_ body (labelWith (fontMuted . fillW))
      scope $ when (not (null commands)) (codeBlock commands)
    flex
    flex

-- | Lines of shell or Haskell, in a sunken monospace block.
codeBlock :: [Text] -> NanoUI ()
codeBlock ls = do
  th <- uiTheme
  let i = ink th
  styled (panelStyle (background (inkSunken i) . borderColor (inkRule i) . cornerRadius 6)) $
    panelWith (fillW . padXY 12 10 . gap 2) $
      forM_ ls (labelWith (fontMono . fontSizeScale 0.9))

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

-- | Where @t@ falls across a 'drawLineChart' of @[x0, x1]@ in @r@.
chartTimeX :: Rect -> (Double, Double) -> Double -> Float
chartTimeX (Rect x _ w _) (x0, x1) t = x + 4 + realToFrac ((t - x0) / max 1e-9 (x1 - x0)) * (w - 8)

chartXTime :: Rect -> (Double, Double) -> Float -> Double
chartXTime (Rect x _ w _) (x0, x1) px = x0 + realToFrac ((px - x - 4) / max 1 (w - 8)) * (x1 - x0)

-- | A minimal line chart over @[x0, x1]@ with a zero-based y axis.
drawLineChart :: Palette -> TextFont -> Rect -> (Double, Double) -> (Double -> Text) -> [ChartSeries] -> CanvasM ()
drawLineChart pal font r@(Rect x y w h) (x0, x1) fmtY series = do
  drawRoundedRect r 6 (pTrackBg pal)
  let ymax = maximum (1e-9 : [v | s <- series, (_, v) <- csPoints s])
      px = chartTimeX r (x0, x1)
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
fact k v = rowWith (tight . gap 5 . alignBaseline) $ do
  cell fontMuted k
  cell id v

factMono :: Text -> Text -> NanoUI ()
factMono k v = rowWith (tight . gap 5 . alignBaseline) $ do
  cell fontMuted k
  cell fontMono v

-- | A vertical hairline of the given height, centred in its row.
vrule :: Float -> NanoUI ()
vrule h = do
  th <- uiTheme
  void (canvas (fixedWH 1 h . alignMid) (\r -> drawRect r (inkRule (ink th))))

-- | A scroll area inside a pane, drawn on the pane's surface. nano-ui gives
-- a scroller a sunken well (or, filling both ways, the window colour);
-- inside a pane that reads as a box within the box, so the well is made
-- transparent and the scroller takes a percentage width to stay off the
-- window-colour path. @f@ sizes the area, e.g. @grow . minH 160@.
paneScroll :: (Layout -> Layout) -> NanoUI a -> NanoUI a
paneScroll f body = snd <$> paneScrollArea f body

paneScrollArea :: (Layout -> Layout) -> NanoUI a -> NanoUI (WidgetId, a)
paneScrollArea f body = do
  th <- uiTheme
  -- The scroller is pinned over a column that takes the space it is given:
  -- in flow, a scroller reports its content's height, and that would push
  -- the pane's neighbours aside.
  columnWith (f . tight . fillW . fillH) $
    styled (inputStyle (background colorTransparent . borderWidth 0)) $
      scrollArea (pinAt 0 0 . percent 100 . fillH . tight) (themed th body)
