{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Headless SVG graph generation using nano-ui-svg. Reuses Stronghead's
-- native UI components (palettes, dark slate theme, heap chart plotting,
-- chips, badges, and before/after proof diff cards).
module UI.Graph
  ( renderHpSvg
  , renderHpDiffSvg
  , renderUiFrameToSvg
  , heapPanelUi
  , heapDiffUi
  ) where

import Control.Monad (forM_, void, when)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Numeric (showFFloat)

import NanoUI
import NanoUI.Backend
  ( FontMetrics (..)
  , damageFullNow
  , emptyInput
  , runFrame
  , withFontMetrics
  , withMonoFontMetrics
  )
import NanoUI.Draw.FrameSvg (FrameSvgConfig (..), defaultFrameSvgConfig, frameToSvg)
import NanoUI.Internal.Context (setDrawExternalText, setDrawSquareGeometry)
import NanoUI.Testing (newPixelContext)

import Stronghead.Format (fmtBytesD, fmtSec, tshow)
import Stronghead.HeapProfile (Band (..), HeapProfile (..), Suspect (..), leakSuspects)
import Stronghead.Run (runKey)

import UI.Clock (Marks (..), linearAxis)
import UI.Common
  ( MonoMetrics (..)
  , Palette (..)
  , TimeView (..)
  , badge
  , bandColor
  , canvasKeyed
  , monoMetrics
  , numCell
  , palette
  , strongheadTheme
  )
import UI.Heap
  ( chartPlot
  , drawChart
  , maxStack
  , stackLayers
  )



-- ---------------------------------------------------------------------------
-- NanoUI Headless Frame Capture -> SVG via nano-ui-svg frameToSvg
-- ---------------------------------------------------------------------------

-- | Realistic monospace font metrics for headless SVG layout.
monoFm :: Float -> FontMetrics
monoFm cell =
  FontMetrics
    { fmLineHeight = cell
    , fmAscent = cell * 0.8
    , fmSnapScale = 1.0
    , fmAdvance = const (cell * 0.60)
    , fmKerning = \_ _ -> 0
    , fmShape = const Nothing
    , fmGlyph = const Nothing
    , fmBackend = Nothing
    }

-- | Realistic proportional font metrics for headless SVG layout (Segoe UI / Inter proportions).
sansFm :: Float -> FontMetrics
sansFm cell =
  FontMetrics
    { fmLineHeight = cell
    , fmAscent = cell * 0.8
    , fmSnapScale = 1.0
    , fmAdvance = \c ->
        case c of
          ' ' -> cell * 0.28
          _ | c >= '0' && c <= '9' -> cell * 0.55
          _ | c >= 'a' && c <= 'z' -> cell * 0.52
          _ | c >= 'A' && c <= 'Z' -> cell * 0.62
          _ -> cell * 0.52
    , fmKerning = \_ _ -> 0
    , fmShape = const Nothing
    , fmGlyph = const Nothing
    , fmBackend = Nothing
    }

-- | Renders a 'NanoUI ()' frame into a standalone SVG document using nano-ui-svg's 'frameToSvg'.
-- The root window rectangle is filled with 'themeWindow theme' (dark slate `#181818`).
renderUiFrameToSvg :: Float -> Float -> Theme -> NanoUI () -> IO Text
renderUiFrameToSvg width height theme ui = do
  ctx0 <- newPixelContext
  let ctx = withMonoFontMetrics (withFontMetrics ctx0 (sansFm 16)) (monoFm 16)
  setTheme ctx theme
  setDrawSquareGeometry ctx True
  setDrawExternalText ctx True
  let baseInp = emptyInput { inputWindowSize = Size width height, inputMousePos = V2 (-10) (-10) }
  -- Warm up frames so layout, stores, and variables settle
  void (runFrame ctx baseInp ui)
  void (runFrame ctx baseInp ui)
  -- Full paint frame
  (_, drawData, _) <- runFrame ctx baseInp (ui >> damageFullNow)
  let cfg = defaultFrameSvgConfig
        { fsPrecision = 1
        , fsFontFamily = "Inter, 'Segoe UI', -apple-system, BlinkMacSystemFont, Roboto, sans-serif"
        }
  svg <- frameToSvg cfg ctx baseInp drawData
  pure (cleanSvgText svg)

-- | Strips 'textLength="..." lengthAdjust="spacingAndGlyphs"' attributes so SVG text
-- elements render with natural typographic proportions. When an anchor-end tick was
-- pre-shifted to a negative x coordinate by 'drawTextBox', or when text is a right-aligned
-- byte amount (ending in " B", " KiB", " MiB", " GiB"), restores 'text-anchor="end"'
-- at the right edge (x + textLength). Also enables tabular numbers at the SVG root.
cleanSvgText :: Text -> Text
cleanSvgText txt =
  let withTabular = if "<svg " `T.isInfixOf` txt && not ("tabular-nums" `T.isInfixOf` txt)
                      then replaceFirst "<svg " "<svg style=\"font-variant-numeric: tabular-nums;\" " txt
                      else txt
   in processTextTags withTabular
 where
  replaceFirst target repl src =
    let (before, match) = T.breakOn target src
     in if T.null match
          then src
          else before <> repl <> T.drop (T.length target) match

  processTextTags src =
    let (before, match) = T.breakOn "<text " src
     in if T.null match
          then before
          else
            let afterOpen = T.drop 6 match
                (tagAttrs, rest) = T.breakOn ">" afterOpen
             in if T.null rest
                  then before <> match
                  else
                    let afterTag = T.drop 1 rest
                        (textContent, afterText) = T.breakOn "</text>" afterTag
                     in if T.null afterText
                          then before <> "<text " <> cleanAttrs textContent tagAttrs <> ">" <> afterTag
                          else before <> "<text " <> cleanAttrs textContent tagAttrs <> ">" <> textContent <> "</text>" <> processTextTags (T.drop 7 afterText)

cleanAttrs :: Text -> Text -> Text
cleanAttrs txt attrs =
  let mX   = parseAttr "x=\"" attrs
      mLen = parseAttr "textLength=\"" attrs
      attrsWithoutLen = removeAttr "textLength" (removeAttr "lengthAdjust" attrs)
      isRightAligned =
        case (mX, mLen) of
          (Just xVal, Just _)
            | Just (x, "") <- readFloat xVal ->
                -- Case 1: Chart Y-axis tick pre-shifted to negative x
                (x < 0) ||
                -- Case 2: Byte amounts in legend or chips (right-aligned numCell)
                (T.isSuffixOf " B" txt || T.isSuffixOf " KiB" txt || T.isSuffixOf " MiB" txt || T.isSuffixOf " GiB" txt)
            | otherwise -> False
          _ -> False
   in if isRightAligned
        then case (mX, mLen) of
          (Just xVal, Just lenVal)
            | Just (x, "") <- readFloat xVal
            , Just (l, "") <- readFloat lenVal ->
                let targetX = T.pack (showFFloat (Just 1) (x + l) "")
                    replacedX = replaceAttr "x=\"" targetX attrsWithoutLen
                 in replacedX <> " text-anchor=\"end\""
          _ -> attrsWithoutLen
        else attrsWithoutLen
 where
  readFloat s = case reads (T.unpack s) of
    [(v, rest)] -> Just (v :: Float, rest)
    _           -> Nothing

parseAttr :: Text -> Text -> Maybe Text
parseAttr key src =
  let (_, rest) = T.breakOn key src
   in if T.null rest
        then Nothing
        else
          let valPart = T.drop (T.length key) rest
           in Just (T.takeWhile (/= '"') valPart)

removeAttr :: Text -> Text -> Text
removeAttr name src =
  let prefix = " " <> name <> "=\""
      (before, rest) = T.breakOn prefix src
   in if T.null rest
        then src
        else
          let afterVal = T.drop 1 (T.dropWhile (/= '"') (T.drop (T.length prefix) rest))
           in removeAttr name (before <> afterVal)

replaceAttr :: Text -> Text -> Text -> Text
replaceAttr key newPrefix src =
  let (before, rest) = T.breakOn key src
   in if T.null rest
        then src
        else
          let afterVal = T.dropWhile (/= '"') (T.drop (T.length key) rest)
           in before <> key <> newPrefix <> afterVal

-- ---------------------------------------------------------------------------
-- Native Stronghead GUI Components Reused for Proof Layouts
-- ---------------------------------------------------------------------------

cardWith :: (Layout -> Layout) -> NanoUI a -> NanoUI a
cardWith f = panelWith f

-- | Renders a single heap profile residency panel.
heapPanelUi :: Palette -> MonoMetrics -> HeapProfile -> Int -> Text -> NanoUI ()
heapPanelUi pal mm hp topN panelTitle = do
  let peak = if VU.null (hpTotals hp) then 0 else VU.maximum (hpTotals hp)
      times = hpTimes hp
      censuses = VU.length times
      suspects = length (leakSuspects hp)
      lo = hpStart hp * 1e9
      dur = if VU.null times then 0 else VU.last times - hpStart hp
      hi = if VU.null times then lo + 1e9 else max (lo + 1e6) (VU.last times * 1e9)
      v = TimeView lo hi
      ls = stackLayers hp S.empty Nothing topN
      ymax = max 1 (maxStack ls)
      rk = runKey (hpRun hp)
      axis r = linearAxis rk v (rectX (chartPlot r)) (rectW (chartPlot r))
      marks = Marks Nothing Nothing [] []

  panelWith (padAll 10 . gap 8 . fillW . fillH) $ do
    -- Title row: Panel title on left, Leak Suspects badge on right
    rowWith (alignMid . gap 8 . fillW) $ do
      labelWith (fontBold . fontSizeScale 1.05) panelTitle
      flex
      if suspects > 0
        then badge Danger (tshow suspects <> " LEAK SUSPECTS")
        else badge Success "0 LEAK SUSPECTS"

    -- Metric chips row: censuses, duration, peak bytes
    rowWith (alignMid . gap 6 . fillW) $ do
      cardWith (padXY 8 3) $
        labelWith (fontMono . fontSizeScale 0.82 . fontBold) (tshow censuses <> " censuses")
      cardWith (padXY 8 3) $
        labelWith (fontMono . fontSizeScale 0.82 . fontMuted) (fmtSec dur <> " profiled")
      cardWith (padXY 8 3) $
        labelWith (fontMono . fontSizeScale 0.82 . fontBold) (fmtBytesD peak <> " peak")

    -- Chart plot and legend side-by-side
    rowWith (fillW . grow . gap 8) $ do
      void $ canvasConfigured
        defaultCanvasConfig { canvasLayout = (fillW . fillH) defaultLayout }
        (\r -> drawChart pal mm hp ls ymax v (-1) (axis r) marks r Nothing)

      columnWith (fixedW 250 . gap 4) $ do
        labelWith (fontSizeScale 0.8 . fontBold . fontMuted) ("BANDS (" <> tshow (length (hpBands hp)) <> " TOTAL)")
        let bands = V.toList (hpBands hp)
            topBands = take topN bands
            leakNames = S.fromList [bandName (hpBands hp V.! susBand s) | s <- leakSuspects hp]
        columnWith (gap 2 . fillW) $ forM_ (zip [0 :: Int ..] topBands) $ \(i, b) -> do
          let name = bandName b
              c = bandColor i name
              bPeak = if VU.null (bandValues b) then 0 else VU.maximum (bandValues b)
              isLeak = S.member name leakNames
              dispName = if T.length name > 15 then T.take 14 name <> "…" else name
          rowWith (alignMid . gap 6 . fillW) $ do
            void $ canvasKeyed (fromIntegral (colorToWord32 c)) (fixedWH 8 8 . alignMid) (\r -> drawRoundedRect r 2 c)
            labelWith (fontMono . fontSizeScale 0.8 . fixedW 114) dispName
            numCell 64 (fontSizeScale 0.78 . fontMuted) (fmtBytesD bPeak)
            rowWith (fixedW 42 . alignEnd . alignMid) $
              when isLeak $
                badge Danger "LEAK"

-- | Renders the Before & After proof diff view.
heapDiffUi :: Palette -> MonoMetrics -> HeapProfile -> HeapProfile -> Int -> NanoUI ()
heapDiffUi pal mm baseHp curHp topN = do
  let basePeak = if VU.null (hpTotals baseHp) then 0 else VU.maximum (hpTotals baseHp)
      curPeak = if VU.null (hpTotals curHp) then 0 else VU.maximum (hpTotals curHp)
      baseCensuses = VU.length (hpTimes baseHp)
      curCensuses = VU.length (hpTimes curHp)
      baseSuspects = length (leakSuspects baseHp)
      curSuspects = length (leakSuspects curHp)
      reduction = if basePeak > 0
        then let pct = (1.0 - (curPeak / basePeak)) * 100.0 :: Double
              in T.pack (showFFloat (Just 1) pct "") <> "%"
        else "N/A"

  columnWith (padAll 12 . gap 10 . fillW . fillH) $ do
    -- KPI header card
    cardWith (padAll 12 . gap 6 . fillW) $ do
      rowWith (alignMid . gap 8) $
        labelWith (fontBold . fontSizeScale 1.15) "Stronghead Memory Leak Verification & Proof of Fix"
      rowWith (alignMid) $
        labelWith (fontSizeScale 0.85 . fontMuted) "Empirical GHC heap residency diff: unrolling intermediate thunk chains vs strict monadic sequencing"
      rowWith (alignMid . gap 12) $ do
        cardWith (padAll 8 . fixedW 200) $ do
          labelWith (fontSizeScale 0.75 . fontBold . fontMuted) "PEAK HEAP RESIDENCY"
          labelWith (fontMono . fontSizeScale 0.95 . fontBold) (fmtBytesD basePeak <> " -> " <> fmtBytesD curPeak)
          labelWith (fontMono . fontSizeScale 0.85 . fontBold) ("-" <> reduction <> " reduction")
        cardWith (padAll 8 . fixedW 200) $ do
          labelWith (fontSizeScale 0.75 . fontBold . fontMuted) "LEAK SUSPECTS"
          labelWith (fontMono . fontSizeScale 0.95 . fontBold) (tshow baseSuspects <> " -> " <> tshow curSuspects)
          labelWith (fontMono . fontSizeScale 0.85 . fontBold) (tshow (baseSuspects - curSuspects) <> " suspects eliminated")
        cardWith (padAll 8 . fixedW 200) $ do
          labelWith (fontSizeScale 0.75 . fontBold . fontMuted) "TOTAL CENSUSES"
          labelWith (fontMono . fontSizeScale 0.95 . fontBold) (tshow baseCensuses <> " -> " <> tshow curCensuses)
          labelWith (fontMono . fontSizeScale 0.85 . fontMuted) (tshow (curCensuses - baseCensuses) <> " diff")

    -- Side-by-side heap panels
    rowWith (fillW . grow . gap 10) $ do
      columnWith (fillW . grow) $
        heapPanelUi pal mm baseHp topN "1. Baseline (Before Fix — Leaking)"
      columnWith (fillW . grow) $
        heapPanelUi pal mm curHp topN "2. Candidate (After Fix — Constant Memory)"

-- | Renders a single heap profile to SVG using Stronghead's native UI and nano-ui-svg.
renderHpSvg :: Maybe String -> Maybe Int -> HeapProfile -> IO Text
renderHpSvg mTheme mTopN hp = do
  let appearance = case mTheme of
        Just "light" -> Just AppearanceLight
        _            -> Just AppearanceDark
      theme = strongheadTheme appearance
      pal = palette theme
      topN = fromMaybe 12 mTopN
  renderUiFrameToSvg 1000 580 theme $ do
    mm <- monoMetrics 0.88
    heapPanelUi pal mm hp topN "Heap Profile Residency"

-- | Renders the Before & After proof diff to SVG using Stronghead's native UI and nano-ui-svg.
renderHpDiffSvg :: Maybe String -> Maybe Int -> HeapProfile -> HeapProfile -> IO Text
renderHpDiffSvg mTheme mTopN baseHp curHp = do
  let appearance = case mTheme of
        Just "light" -> Just AppearanceLight
        _            -> Just AppearanceDark
      theme = strongheadTheme appearance
      pal = palette theme
      topN = fromMaybe 12 mTopN
  renderUiFrameToSvg 1380 700 theme $ do
    mm <- monoMetrics 0.88
    heapDiffUi pal mm baseHp curHp topN
