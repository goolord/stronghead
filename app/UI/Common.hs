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
  , wheelZoom
  , panBy
  , wheelPan
  , dragPan
  , clampView
  , niceStep
  , axisTicks
    -- * Frame
  , toolStrip
  , toolLabel
  , toolButton
  , toolIconButton
  , toolGroup
  , quiet
  , quietButton
  , quietButton'
  , quietIconButton
  , quietIconButton'
  , actionButton
  , actionIconButton
  , iconButtonWith
  , iconButtonWith'
    -- * Icons
  , iconPlay
  , iconStop
  , iconPause
  , iconStep
  , iconStepInto
  , iconStepOver
  , iconStepOut
  , iconStepModule
  , iconContinue
  , iconTrace
  , iconBolt
  , iconBack
  , iconForward
  , iconHistory
  , iconAbandon
  , iconSend
  , iconTrash
  , iconFolderOpen
  , iconFile
  , iconSave
  , iconSearch
  , iconSettings
  , iconLayers
  , iconLayout
  , iconCheck
  , iconLink
  , iconUnlink
  , iconRefresh
  , iconCamera
  , iconBookmark
  , iconCopy
  , iconMaximize
  , iconTarget
  , firstPath
  , Workbench (..)
  , HeldTab (..)
  , newWorkbench
  , PaneCtx (..)
  , floatingPane
  , pane
  , paneWith
  , paneFlashRef
  , lastMouseClickRef
  , hashText
  , recordTabFlash
  , isPaneFlashing
  , paneFlashing
  , wasPaneFlashed
  , clearPaneFlashes
  , paneMeta
  , paneMetaWith
  , paneMenu
  , paneMenuWithVar
  , readout
  , readoutDelta
  , emptyState
  , paneScroll
  , paneScrollArea
  , bareScroll
  , bareScrollArea
  , paneVirtualRows
  , canvasKeyed
  , codeBlock
    -- * Widgets
  , cell
  , numCell
  , colHeader
  , selectableRow
  , tableRows
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

import Control.Monad (forM_, unless, void, when)
import Data.Bits (xor)
import Data.Char (isDigit, ord)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Stronghead.Format (tshow)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Word (Word64)
import GHC.Float (castFloatToWord32)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import NanoUI
import NanoUI.Adornment qualified as A
import NanoUI.Internal.Context (Context (..), CustomDrawingEntry (..), DrawingCacheState (..), intKey)
import NanoUI.Internal.Layout.Arena (arenaCount, getNodeValue, getStyleIdx, getText, getWidgetId, lookupNodeByWidgetId, setStyleIdx)
import NanoUI.Internal.Monad (askContext, askFrameInput)
import NanoUI.Path qualified as P

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

categoricalColors :: V.Vector Color
categoricalColors =
  V.fromList
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

-- | A stable categorical colour for a band: by rank for the first twelve,
-- by name hash after that.
bandColor :: Int -> Text -> Color
bandColor i name
  | i >= 0 && i < V.length categoricalColors = categoricalColors V.! i
  | otherwise = categoricalColors V.! (T.foldl' (\h c -> (h * 31) `xor` ord c) 7 name `mod` V.length categoricalColors)

-- | 'withAlpha' with the opacity first, for composing.
alpha :: Float -> Color -> Color
alpha a c = withAlpha c a

-- ---------------------------------------------------------------------------
-- Time axis

-- | The visible time window, in nanoseconds.
data TimeView = TimeView {tvStart :: !Double, tvEnd :: !Double}
  deriving stock (Eq, Show)

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

-- | The zoom factor for a wheel delta from 'useWheelDeltaOn'. nano-ui
-- reports the wheel the way its scroll areas apply it, +y for a notch
-- towards the user (scrolling down), +x for scrolling right; so a canvas
-- that pans or scrolls adds the delta, and the wheel turned away from the
-- user zooms in.
wheelZoom :: Float -> Double
wheelZoom wy = 1.25 ** realToFrac wy

-- | Slide a view by @dt@ ns.
panBy :: Double -> TimeView -> TimeView
panBy dt (TimeView a b) = TimeView (a + dt) (b + dt)

-- | A horizontal wheel delta from 'useWheelDeltaOn' as a pan: a tenth of
-- the view a notch.
wheelPan :: Float -> TimeView -> TimeView
wheelPan wx v = panBy (realToFrac wx * viewWidth v * 0.1) v

-- | A drag of @dx@ pixels across a plot @w@ wide, as the pan that keeps
-- the time under the pointer.
dragPan :: Float -> Float -> TimeView -> TimeView
dragPan w dx v = panBy (negate (realToFrac dx / realToFrac w * viewWidth v)) v

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

-- | The multiples of @step@ in @[lo, hi]@.
axisTicks :: Double -> Double -> Double -> [Double]
axisTicks step lo hi = takeWhile (<= hi) [first, first + step ..]
  where
    first = fromIntegral (ceiling (lo / step) :: Integer) * step

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

-- | A table's rows, stacked with no gap: each row's own padding is the
-- rhythm, whatever gap the pane body around them uses.
tableRows :: NanoUI a -> NanoUI a
tableRows = columnWith (tight . gap 0 . fillW)

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
    panelWith (alignMid . padXY 10 4) (labelWith (fontColor c . fontSizeScale 0.8) txt)

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

toolIconButton :: Svg -> Text -> NanoUI Bool
toolIconButton = iconButtonWith alignMid

-- | A button with a leading SVG icon and layout modifier.
iconButtonWith :: (Layout -> Layout) -> Svg -> Text -> NanoUI Bool
iconButtonWith f ic label = respClicked <$> iconButtonWith' f ic label

-- | 'iconButtonWith' returning its 'Response'.
iconButtonWith' :: (Layout -> Layout) -> Svg -> Text -> NanoUI Response
iconButtonWith' f ic label = do
  let lay0 = f defaultLayout
  base <- uiFontSize
  let fs = if layoutFontSize lay0 > 0 then layoutFontSize lay0 else base
  fm <- resolveFontUi fs (layoutFontWeight lay0) (layoutFontStyle lay0) (layoutFontVariant lay0)
  spaceW <- lineWidthUi fm " "
  buttonConfigured'
    defaultButtonConfig
      { bcLayout = f (gap spaceW defaultLayout)
      , bcAdornments = A.leading (A.icon ic)
      }
    label

-- | Controls that belong together, set off from their neighbours by a rule.
toolGroup :: NanoUI a -> NanoUI a
toolGroup body = do
  vrule 22
  rowWith (tight . gap 6 . alignMid) body

-- | Chrome that stays out of the way: a button with no surface until the
-- pointer is on it, for the header's commands and the panes' menus.
quiet :: Theme -> Theme
quiet th =
  let t = inkText (ink th)
   in buttonStyle (background colorTransparent . borderWidth 0 . hoverBackground (alpha 0.08 t) . pressBackground (alpha 0.14 t)) th

quietButton :: Text -> NanoUI Bool
quietButton = styled quiet . buttonWith alignMid

quietIconButton :: Svg -> Text -> NanoUI Bool
quietIconButton ic = styled quiet . iconButtonWith alignMid ic

-- | A 'quietButton' with a tooltip, giving its response for a popover to
-- anchor on.
quietButton' :: Text -> Text -> NanoUI Response
quietButton' label tip = do
  resp <- styled quiet (buttonWith' alignMid label)
  tooltip resp tip
  pure resp

quietIconButton' :: Svg -> Text -> Text -> NanoUI Response
quietIconButton' ic label tip = do
  resp <- styled quiet (iconButtonWith' alignMid ic label)
  tooltip resp tip
  pure resp

-- | A button for something that may not be possible now: disabled with
-- the reason as its tooltip when it is 'Left', else with @tip@ if any.
actionButton :: (Layout -> Layout) -> Text -> Maybe Text -> Either Text (NanoUI ()) -> NanoUI ()
actionButton f label tip run = do
  resp <- disabledWhen (isLeft run) (buttonWith' f label)
  for_ (either Just (const tip) run) (tooltip resp)
  when (respClicked resp) (sequence_ run)

-- | 'actionButton' with a leading SVG icon.
actionIconButton :: (Layout -> Layout) -> Svg -> Text -> Maybe Text -> Either Text (NanoUI ()) -> NanoUI ()
actionIconButton f ic label tip run = do
  resp <- disabledWhen (isLeft run) (iconButtonWith' f ic label)
  for_ (either Just (const tip) run) (tooltip resp)
  when (respClicked resp) (sequence_ run)

-- ---------------------------------------------------------------------------
-- Icons

strokeIcon :: Text -> Svg
strokeIcon body =
  case parseSvg (TE.encodeUtf8 ("<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>" <> body <> "</svg>")) of
    Right s -> s
    Left err -> error ("UI.Common.strokeIcon: " <> err)

iconPlay, iconStop, iconPause, iconStep, iconStepInto, iconStepOver, iconStepOut, iconStepModule, iconContinue :: Svg
iconPlay = strokeIcon "<polygon points='6 3 20 12 6 21 6 3'/>"
iconStop = strokeIcon "<rect x='5' y='5' width='14' height='14' rx='2'/>"
iconPause = strokeIcon "<rect x='6' y='4' width='4' height='16' rx='1'/><rect x='14' y='4' width='4' height='16' rx='1'/>"
iconStep = strokeIcon "<polygon points='5 4 15 12 5 20 5 4'/><line x1='19' y1='5' x2='19' y2='19'/>"
iconStepInto = strokeIcon "<path d='M12 3v12'/><polyline points='8 11 12 15 16 11'/><circle cx='12' cy='20' r='1.5'/>"
iconStepOver = strokeIcon "<path d='M5 14a7 7 0 0 1 14 0'/><polyline points='16 11 19 14 16 17'/><circle cx='12' cy='19' r='1.5'/>"
iconStepOut = strokeIcon "<path d='M12 15V3'/><polyline points='8 7 12 3 16 7'/><circle cx='12' cy='20' r='1.5'/>"
iconStepModule = strokeIcon "<path d='M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z'/><polyline points='14 2 14 8 20 8'/><path d='M9 15h6'/><polyline points='13 13 15 15 13 17'/>"
iconContinue = strokeIcon "<polygon points='4 4 12 12 4 20 4 4'/><polygon points='13 4 21 12 13 20 13 4'/>"

iconTrace, iconBolt, iconBack, iconForward, iconHistory, iconAbandon, iconSend, iconTrash :: Svg
iconTrace = strokeIcon "<polyline points='22 12 18 12 15 21 9 3 6 12 2 12'/>"
iconBolt = strokeIcon "<polygon points='13 2 3 14 12 14 11 22 21 10 12 10 13 2'/>"
iconBack = strokeIcon "<line x1='19' y1='12' x2='5' y2='12'/><polyline points='12 19 5 12 12 5'/>"
iconForward = strokeIcon "<line x1='5' y1='12' x2='19' y2='12'/><polyline points='12 5 19 12 12 19'/>"
iconHistory = strokeIcon "<path d='M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8'/><polyline points='3 3 3 8 8 8'/><polyline points='12 7 12 12 15 15'/>"
iconAbandon = strokeIcon "<circle cx='12' cy='12' r='10'/><line x1='15' y1='9' x2='9' y2='15'/><line x1='9' y1='9' x2='15' y2='15'/>"
iconSend = strokeIcon "<polyline points='9 10 4 15 9 20'/><path d='M20 4v7a4 4 0 0 1-4 4H4'/>"
iconTrash = strokeIcon "<polyline points='3 6 5 6 21 6'/><path d='M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2'/>"

iconFolderOpen, iconFile, iconSave, iconSearch, iconSettings, iconLayers, iconLayout, iconCheck :: Svg
iconFolderOpen = strokeIcon "<path d='M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z'/>"
iconFile = strokeIcon "<path d='M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z'/><polyline points='14 2 14 8 20 8'/>"
iconSave = strokeIcon "<path d='M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z'/><polyline points='17 21 17 13 7 13 7 21'/><polyline points='7 3 7 8 15 8'/>"
iconSearch = strokeIcon "<circle cx='11' cy='11' r='8'/><line x1='21' y1='21' x2='16.65' y2='16.65'/>"
iconSettings = strokeIcon "<line x1='4' y1='21' x2='4' y2='14'/><line x1='4' y1='10' x2='4' y2='3'/><line x1='12' y1='21' x2='12' y2='12'/><line x1='12' y1='8' x2='12' y2='3'/><line x1='20' y1='21' x2='20' y2='16'/><line x1='20' y1='12' x2='20' y2='3'/><line x1='1' y1='14' x2='7' y2='14'/><line x1='9' y1='8' x2='15' y2='8'/><line x1='17' y1='16' x2='23' y2='16'/>"
iconLayers = strokeIcon "<polygon points='12 2 2 7 12 12 22 7 12 2'/><polyline points='2 17 12 22 22 17'/><polyline points='2 12 12 17 22 12'/>"
iconLayout = strokeIcon "<rect x='3' y='3' width='18' height='18' rx='2'/><line x1='3' y1='9' x2='21' y2='9'/><line x1='9' y1='21' x2='9' y2='9'/>"
iconCheck = strokeIcon "<polyline points='20 6 9 17 4 12'/>"

iconLink, iconUnlink, iconRefresh, iconCamera, iconBookmark, iconCopy, iconMaximize, iconTarget :: Svg
iconLink = strokeIcon "<path d='M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71'/><path d='M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71'/>"
iconUnlink = strokeIcon "<path d='m18.84 12.25 1.72-1.71a5 5 0 0 0-7.07-7.07l-1.72 1.71'/><path d='m5.17 11.75-1.71 1.71a5 5 0 0 0 7.07 7.07l1.71-1.71'/><line x1='2' y1='2' x2='22' y2='22'/>"
iconRefresh = strokeIcon "<polyline points='23 4 23 10 17 10'/><polyline points='1 20 1 14 7 14'/><path d='M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15'/>"
iconCamera = strokeIcon "<path d='M23 19a2 2 0 0 1-2 2H3a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h4l2-3h6l2 3h4a2 2 0 0 1 2 2z'/><circle cx='12' cy='13' r='4'/>"
iconBookmark = strokeIcon "<path d='M19 21l-7-5-7 5V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2z'/>"
iconCopy = strokeIcon "<rect x='9' y='9' width='13' height='13' rx='2'/><path d='M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1'/>"
iconMaximize = strokeIcon "<polyline points='15 3 21 3 21 9'/><polyline points='9 21 3 21 3 15'/><line x1='21' y1='3' x2='14' y2='10'/><line x1='3' y1='21' x2='10' y2='14'/>"
iconTarget = strokeIcon "<circle cx='12' cy='12' r='10'/><line x1='22' y1='12' x2='18' y2='12'/><line x1='6' y1='12' x2='2' y2='12'/><line x1='12' y1='6' x2='12' y2='2'/><line x1='12' y1='22' x2='12' y2='18'/>"

-- | Act on the first path a file dialog picked, if it picked any: for
-- @useFileDialog@.
firstPath :: (FilePath -> NanoUI ()) -> [FilePath] -> NanoUI ()
firstPath f = mapM_ f . take 1

-- | A tab currently being dragged across a workbench.
data HeldTab = HeldTab
  { htTab :: !Word64
  , htTitle :: !Text
  , htFrom :: !Word64
  , htFromCount :: !Int
  , htAt :: !V2
  , htPhase :: !DragPhase
  , htDrop :: !(Maybe (Word64, Int))
  , htCommit :: !(Maybe (Word64, Int) -> Maybe PaneDropLocation -> NanoUI ())
  }

-- | The split tree of a panel's panes, kept in the panel's state so the
-- arrangement survives switching panels. Pane ids name pane kinds
-- ("UI.Workspace"); split ids are 1000 and up. The name keys it in
-- session files and in the panes' menus.
data Workbench = Workbench
  { wbName :: !Text
  , wbTree :: !(Var (Maybe GridNode))
  , wbTabDrag :: !(IORef (Maybe HeldTab))
  }

newWorkbench :: Text -> GridNode -> IO Workbench
newWorkbench name tree = Workbench name <$> newVar (Just tree) <*> newIORef Nothing

-- | Where a pane is drawn: in a workbench's grid, or on its own in a
-- floating window. 'pcMenu' draws the pane's menu (move, pop out, close)
-- and says whether one of its items was picked.
data PaneCtx = PaneCtx
  { pcGrid :: !(Maybe PaneGridCtx)
  , pcMenu :: !(Maybe (NanoUI Bool))
  , pcId   :: !(Maybe Word64)
  , pcBench :: !(Maybe Workbench)
  }

-- | A pane in a floating window: no grid, no menu.
floatingPane :: PaneCtx
floatingPane = PaneCtx Nothing Nothing Nothing Nothing

-- | Record a flash for a tab or pane by its title and id.
recordTabFlash :: Text -> Word64 -> Double -> IO ()
recordTabFlash title tid now =
  modifyIORef' paneFlashRef (\m -> M.insert (tshow tid) now (M.insert title now m))

-- | Tracks which pane (by key) most recently started flashing and when.
paneFlashRef :: IORef (M.Map Text Double)
paneFlashRef = unsafePerformIO (newIORef M.empty)
{-# NOINLINE paneFlashRef #-}

-- | Timestamp and screen position of the most recent mouse click / press / release.
lastMouseClickRef :: IORef (Double, V2)
lastMouseClickRef = unsafePerformIO (newIORef (-1000.0, V2 (-9999) (-9999)))
{-# NOINLINE lastMouseClickRef #-}

-- | Check whether the pane with the given title is currently flashing its border.
isPaneFlashing :: Text -> Double -> IO Bool
isPaneFlashing title now = do
  m <- readIORef paneFlashRef
  case M.lookup title m of
    Nothing -> pure False
    Just t -> pure (t >= 0 && now >= t && (now - t) < 0.5)

-- | Check whether the pane was triggered to flash since the last clear.
wasPaneFlashed :: Text -> IO Bool
wasPaneFlashed title = do
  m <- readIORef paneFlashRef
  pure (M.member title m)

-- | Clear the record of pane flashes.
clearPaneFlashes :: IO ()
clearPaneFlashes = do
  writeIORef paneFlashRef M.empty
  writeIORef lastMouseClickRef (-1000.0, V2 (-9999) (-9999))

-- | View query: whether the pane with the given title is currently flashing.
paneFlashing :: Text -> NanoUI Bool
paneFlashing title = do
  now <- uiTime
  liftIO (isPaneFlashing title now)

-- | FNV-1a 64-bit hash of text.
hashText :: Text -> Word64
hashText = T.foldl' (\h c -> (h * 1099511628211) `xor` fromIntegral (ord c)) 14695981039346656037

-- | Compute a signature/hash of the nodes rendered in a pane.
computePaneHash :: Context -> Text -> Int -> Int -> IO Int
computePaneHash ctx title start end = do
  let na = ctxNodeArena ctx
  dc <- readIORef (ctxDrawingCache ctx)
  let customs = dcsCustomDrawings dc
  let initHash = fromIntegral (hashText title) :: Int
  let count = max 0 (end - start)
  let initAcc = (initHash `shiftL` 7) `xor` count
  let go !acc !i
        | i >= end = pure acc
        | otherwise = do
            txt <- getText na i
            val <- getNodeValue na i
            stIdx <- getStyleIdx na i
            wid <- getWidgetId na i
            let hTxt = if T.null txt then 0 else fromIntegral (hashText txt) :: Int
            let hVal = fromIntegral (castFloatToWord32 val) :: Int
            let hSt = stIdx
            let hCustom = case IM.lookup (intKey wid) customs of
                  Nothing -> 0
                  Just cde -> cdrContent cde
            let step = (hTxt * 1099511628211) `xor` (hVal * 1469598103934665603) `xor` (hSt * 2166136261) `xor` (hCustom * 16777619)
            let acc' = ((acc `shiftL` 5) .|. (acc `shiftR` 59)) `xor` step
            go acc' (i + 1)
  finalHash <- go initAcc start
  pure (if finalHash == 0 then 1 else finalHash)
-- | A pane: a header with its title and @trailing@ (meta text, actions),
-- and a padded body. Drag the header to move the pane, double-click it to
-- maximize; the header's last button opens the pane's menu.
pane :: PaneCtx -> Text -> NanoUI () -> NanoUI a -> NanoUI a
pane = paneWith (padAll 10 . gap 6)

-- | 'pane' with the body's layout, e.g. @tight@ for a canvas that fills it.
paneWith :: (Layout -> Layout) -> PaneCtx -> Text -> NanoUI () -> NanoUI a -> NanoUI a
paneWith bodyLayout ctx title trailing body = do
  th <- uiTheme
  now <- uiTime
  inp <- askFrameInput

  let isMouseClick =
        inputButtonsPressed inp /= mempty
          || inputButtonsReleased inp /= mempty
  when isMouseClick $
    liftIO (writeIORef lastMouseClickRef (now, inputMousePos inp))

  sid <- currentId
  mLastRect <- lastRect sid
  let paneRect = case pcGrid ctx of
        Just g -> pgcRect g
        Nothing -> fromMaybe (Rect 0 0 0 0) mLastRect

  flashVar <- useVar (-1.0 :: Double)
  prevHashVar <- useVar (0 :: Int)

  flashStart <- readVar flashVar
  let flashDuration = 0.5 :: Double
  let elapsed = now - flashStart
  let flashing = flashStart >= 0 && elapsed >= 0 && elapsed < flashDuration
  let flashFactor =
        if flashing
          then max 0 (min 1 (realToFrac ((1 - elapsed / flashDuration) ** 2)))
          else 0 :: Float

  when flashing $ wakeAfter 0.016

  let i = ink th
      baseBorder = inkRule i
      accent = inkAccent i
      borderColorActive = if flashing then lerpColor baseBorder accent flashFactor else baseBorder
      borderWidthActive = if flashing && flashFactor > 0.05 then 2 else 1

  menuOpen <- useVar False
  let headerRow = asRow . fillW . fixedH 34 . padXY 12 0 . gap 8
      contents = do
        labelWith (alignMid . fontMedium . fontSizeScale 0.92) title
        themed th trailing
        for_ (pcMenu ctx) (themed th . paneMenuWithVar menuOpen)
  styled (panelStyle (background (inkPane i) . borderColor borderColorActive . borderWidth borderWidthActive . cornerRadius 6)) $
    panelWith (tight . gap 0 . fillW . fillH) $ do
      c <- askContext
      let na = ctxNodeArena c
      startCount <- liftIO (arenaCount na)
      styled (panelStyle (background colorTransparent . borderBottom 1 (inkRule i) . cornerRadius 0)) $
        case pcGrid ctx of
          -- The header drags the pane; a press on a control in it does not.
          Just g -> paneDragHandle g headerRow contents
          Nothing -> panelWith headerRow contents
      for_ (pcGrid ctx) $ \g -> do
        let Rect x y w _ = pgcRect g
            inHeader = rectContains (Rect x y w 34) (inputMousePos inp)
        when (inHeader && pressedIn MouseLeft inp && inputMouseClicks inp >= 2) $
          if pgcMaximized g then pgcRestore g else pgcMaximize g
        when (inHeader && pressedIn MouseRight inp) $
          for_ (pcMenu ctx) (const (writeVar menuOpen True))
      res <- themed th $ columnWith (fillW . fillH . bodyLayout) body
      endCount <- liftIO (arenaCount na)
      curHash <- liftIO (computePaneHash c title startCount endCount)
      prevHash <- readVar prevHashVar
      (lastClickTime, lastClickPos) <- liftIO (readIORef lastMouseClickRef)
      let recentClick = lastClickTime >= 0 && (now - lastClickTime) >= 0 && (now - lastClickTime) < 0.40
          clickInThisPane = rectContains paneRect lastClickPos
          attentionDiverted = recentClick && not clickInThisPane
          isScrollingThisPane = inputScroll inp /= V2 0 0 && rectContains paneRect (inputMousePos inp)
          shouldFlash = prevHash /= 0 && curHash /= prevHash && attentionDiverted && not isScrollingThisPane
      let paneKey = maybe title tshow (pcId ctx)
      when shouldFlash $ do
        writeVar flashVar now
        liftIO $ modifyIORef' paneFlashRef (\m -> M.insert paneKey now (M.insert title now m))
      when (curHash /= prevHash) $ do
        writeVar prevHashVar curHash
      pure res

-- | The pane menu's button, a popover of the menu's items that closes once
-- one is picked.
paneMenu :: NanoUI Bool -> NanoUI ()
paneMenu items = do
  open <- useVar False
  paneMenuWithVar open items

-- | 'paneMenu' with an explicit open flag, so right-clicking the pane's
-- header bar can open it too.
paneMenuWithVar :: Var Bool -> NanoUI Bool -> NanoUI ()
paneMenuWithVar open items = do
  th <- uiTheme
  resp <- styled (quiet . buttonStyle (foreground (inkMuted (ink th)))) (buttonWith' (alignMid . padXY 6 0 . tight) "⋯")
  tooltip resp "Move, pop out or close this pane; drag the header to rearrange"
  when (respClicked resp) (modifyVar open not)
  _ <- popupWith open (defaultPopupConfig (AnchorRect (respRect resp))) (minW 200 . padXY 0 4) $
    columnWith (fillW . tight . gap 0) $ do
      picked <- items
      when picked (writeVar open False)
  pure ()

-- | Muted text in a pane header, after the title.
paneMeta :: Text -> NanoUI ()
paneMeta = paneMetaWith id

-- | 'paneMeta' with an additional layout modifier, e.g. 'fixedW'.
paneMetaWith :: (Layout -> Layout) -> Text -> NanoUI ()
paneMetaWith f = labelWith (alignMid . fontMuted . fontSizeScale 0.85 . f)

-- | The vital numbers of a run as one instrument line: each value over its
-- name, separated by hairlines. Wraps onto more lines when narrow.
readout :: [(Text, Text)] -> NanoUI ()
readout items = readoutDelta [(name, value, Nothing) | (name, value) <- items]

-- | 'readout' with, under a value, its change against the baseline run
-- when there is one.
readoutDelta :: [(Text, Text, Maybe Text)] -> NanoUI ()
readoutDelta items =
  rowWith (padTop 14 . padXY 20 0 . tight . gap 0 . fillW . wrap . lineGap 8) $
    forM_ (zip [0 :: Int ..] items) $ \(i, (name, value, delta)) -> withKey i $
      rowWith (tight . gap 0) $ do
        scope $ when (i > 0) (vrule 34)
        columnWith (padLeft (if i > 0 then 14 else 0) . padRight 14 . tight . gap 0) $ do
          let (number, unit) = splitUnit value
          rowWith (tight . gap 3) $ do
            cell (fontMono . fontSizeScale 1.3) number
            scope $ unless (T.null unit) (cell (fontMuted . fontSizeScale 0.9) unit)
          labelWith (fontMuted . fontSizeScale 0.8) name
          scope $ for_ delta $ labelWith (fontMuted . fontMono . fontSizeScale 0.75)

-- | A reading's number and its unit ("5.084 s", "16.5%"), so the unit can
-- be set smaller and muted beside the figures.
splitUnit :: Text -> (Text, Text)
splitUnit t
  | Just n <- T.stripSuffix "%" t = (n, "%")
  | (a, u) <- T.breakOnEnd " " t
  , not (T.null a)
  , not (T.null u)
  , not (T.any isDigit u) =
      (T.dropEnd 1 a, u)
  | otherwise = (t, "")

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
        let raw = [V2 (px t) (py v) | (t, v) <- pts]
            ps = if null (drop (max 2 (floor w :: Int)) raw) then raw else thinPoints raw
        when (csFill s) $
          for_ ((,) <$> (case ps of p : _ -> Just p; [] -> Nothing) <*> (snd <$> unsnoc ps)) \(V2 fx _, V2 lx _) ->
            drawPath (P.polygon (V2 fx (y + h - 4) : ps ++ [V2 lx (y + h - 4)])) (alpha 0.22 (csColor s))
        drawStrokePathWith (P.stroke 1.5) {P.strokeJoin = P.RoundJoin} (P.polyline ps) (P.Solid (csColor s))
      _ -> pure ()
    thinPoints [] = []
    thinPoints [p] = [p]
    thinPoints (firstP : rest) = firstP : go (floor (v2X firstP) :: Int) firstP firstP rest
      where
        go _ !best !lastP [] = [best | best /= firstP] ++ [lastP | lastP /= best && lastP /= firstP]
        go !col !best _ (p@(V2 px' py') : more) =
          let c = floor px' :: Int
           in if c == col
                then go col (if py' < v2Y best then p else best) p more
                else (if best == firstP then id else (best :)) (go c p p more)

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
  box (fixedWH 1 h . alignMid) (inkRule (ink th))

-- | 'canvas' with a content key, so unchanged frames skip rebuilding its ops.
canvasKeyed :: Int -> (Layout -> Layout) -> (Rect -> CanvasM ()) -> NanoUI Response
canvasKeyed k f = canvasConfigured defaultCanvasConfig {canvasLayout = f defaultLayout, canvasContent = k}

-- | A scroll area inside a pane, drawn on the pane's surface. nano-ui gives
-- a scroller a sunken well (or, filling both ways, the window colour);
-- inside a pane that reads as a box within the box, so the well is made
-- transparent and the scroller takes a percentage width to stay off the
-- window-colour path. @f@ sizes the area, e.g. @grow . minH 160@.
-- | A scroll area without an embedded well, border or background: children
-- are clipped to the viewport, and scrollbars paint over them.
bareScrollArea :: (Layout -> Layout) -> NanoUI a -> NanoUI (WidgetId, a)
bareScrollArea f child = do
  (wid, res) <- scrollArea f child
  ctx <- askContext
  liftIO $ do
    mIdx <- lookupNodeByWidgetId (ctxNodeArena ctx) wid
    for_ mIdx $ \idx -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      setStyleIdx (ctxNodeArena ctx) idx (si .|. 32)
  pure (wid, res)

-- | 'bareScrollArea' discarding the container id.
bareScroll :: (Layout -> Layout) -> NanoUI a -> NanoUI a
bareScroll f child = snd <$> bareScrollArea f child

paneScroll :: (Layout -> Layout) -> NanoUI a -> NanoUI a
paneScroll f body = snd <$> paneScrollArea f body

paneScrollArea :: (Layout -> Layout) -> NanoUI a -> NanoUI (WidgetId, a)
paneScrollArea f body = do
  th <- uiTheme
  -- The scroller is pinned over a column that takes the space it is given:
  -- in flow, a scroller reports its content's height, and that would push
  -- the pane's neighbours aside.
  columnWith (f . tight . fillW . fillH) $
    bareScrollArea (pinAt 0 0 . percent 100 . fillH . tight) (themed th body)

-- | A vertically scrolled list of uniform-height rows inside a pane that
-- virtualizes when there are many items, building only the rows near the
-- viewport.
paneVirtualRows :: (Layout -> Layout) -> (Layout -> Layout) -> [a] -> (a -> NanoUI ()) -> NanoUI ()
paneVirtualRows scrollLayout colLayout items drawItem
  | n <= 80 =
      paneScroll scrollLayout $
        columnWith (colLayout . tight . fillW . gap 0) $
          mapM_ drawItem items
  | otherwise = do
      th <- uiTheme
      base <- uiFontSize
      fm <- resolveFontUi base WeightNormal FontStyleNormal FontRegular
      let !rowH = max 18 (fmLineHeight fm + 4)
          !vec = V.fromListN n items
      columnWith (scrollLayout . tight . fillW . fillH) $
        styled (inputStyle (background colorTransparent . borderWidth 0)) $ do
          sid <- currentId
          metrics <- getScrollMetricsUi sid
          let (!firstRow, !lastRow) = case metrics of
                Nothing -> (0, min (n - 1) 59)
                Just m ->
                  let top = v2Y (scrollOffset m)
                      vh = max 200 (rectH (scrollViewport m))
                   in ( max 0 (floor (top / rowH) - 10)
                      , min (n - 1) (ceiling ((top + vh) / rowH) + 10)
                      )
          void . bareScrollArea (pinAt 0 0 . percent 100 . fillH . tight) . themed th $
            columnWith (colLayout . tight . fillW . gap 0) $ do
              when (firstRow > 0) $
                spacer Fit (Fixed (fromIntegral firstRow * rowH))
              forM_ [firstRow .. lastRow] $ \i ->
                drawItem (vec V.! i)
              when (lastRow < n - 1) $
                spacer Fit (Fixed (fromIntegral (n - 1 - lastRow) * rowH))
  where
    n = length items
