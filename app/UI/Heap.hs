-- | The heap explorer and space-leak hunter: stacked residency bands over
-- time with a scrub cursor, the breakdown at the cursor, ranked leak
-- suspects (with the thunk sentinel), the retention tree, and the
-- cost-centre tree from a @.prof@ file.
module UI.Heap
  ( HeapState (..)
  , HeapSource (..)
  , newHeapState
  , heapLoad
  , heapView
  , openHp
  , openProf
  , HeapLoaded (..)
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)
import NanoUI.Path qualified as P
import Stronghead.Format
import Stronghead.HeapProfile
import Stronghead.Prof
import UI.Common

data HeapSource = FromEventlog | FromHpFile
  deriving (Eq, Show, Enum, Bounded)

data HeapTab = AtCursor | Leaks | Retention | CostCentres
  deriving (Eq, Show, Enum, Bounded)

tabName :: HeapTab -> Text
tabName = \case
  AtCursor -> "At cursor"
  Leaks -> "Leak suspects"
  Retention -> "Retention tree"
  CostCentres -> "Cost centres (.prof)"

data HeapState = HeapState
  { hsSource :: !(Var HeapSource)
  , hsHpPath :: !(Var (Maybe FilePath))
  , hsProfPath :: !(Var (Maybe FilePath))
  , hsHpTask :: !(Task FilePath (Either String HeapProfile))
  , hsProfTask :: !(Task FilePath (Either String ProfFile))
  , hsHidden :: !(Var (Set Text))
  , hsSolo :: !(Var (Maybe Text))
  , hsCursor :: !(Var (Maybe Double))
  , hsTab :: !(Var HeapTab)
  , hsTopN :: !(Var Float)
  , hsHpDialog :: !(Var (Maybe FileDialogId))
  , hsProfDialog :: !(Var (Maybe FileDialogId))
  , hsByDelta :: !(Var Bool)
  , hsCCFilter :: !(Var Text)
  , hsCCOpen :: !(Var (Set Int))
  , hsRetOpen :: !(Var (Set Text))
  }

newHeapState :: IO HeapState
newHeapState = do
  hsSource <- newVar FromEventlog
  hsHpPath <- newVar Nothing
  hsProfPath <- newVar Nothing
  hsHpTask <- newTask
  hsProfTask <- newTask
  hsHidden <- newVar S.empty
  hsSolo <- newVar Nothing
  hsCursor <- newVar Nothing
  hsTab <- newVar AtCursor
  hsTopN <- newVar 12
  hsHpDialog <- newVar Nothing
  hsProfDialog <- newVar Nothing
  hsByDelta <- newVar False
  hsCCFilter <- newVar ""
  hsCCOpen <- newVar (S.fromList [1])
  hsRetOpen <- newVar S.empty
  pure HeapState {..}

openHp :: HeapState -> FilePath -> NanoUI ()
openHp st p = do
  writeVar (hsHpPath st) (Just p)
  writeVar (hsSource st) FromHpFile
  writeVar (hsCursor st) Nothing
  writeVar (hsHidden st) S.empty
  writeVar (hsSolo st) Nothing

openProf :: HeapState -> FilePath -> NanoUI ()
openProf st p = do
  writeVar (hsProfPath st) (Just p)
  writeVar (hsTab st) CostCentres

data HeapLoaded = HeapLoaded
  { hlHp :: !(Maybe (TaskStatus (Either String HeapProfile)))
  , hlProf :: !(Maybe (TaskStatus (Either String ProfFile)))
  }

-- | Run the file loaders; called every frame.
heapLoad :: HeapState -> NanoUI HeapLoaded
heapLoad st = do
  hp <- readVar (hsHpPath st)
  prof <- readVar (hsProfPath st)
  a <- traverse (\p -> useTaskStatus (hsHpTask st) p (loadHp p)) hp
  b <- traverse (\p -> useTaskStatus (hsProfTask st) p (loadProf p)) prof
  pure (HeapLoaded a b)

done :: Maybe (TaskStatus (Either String a)) -> Maybe (Either String a)
done = \case
  Just (TaskDone r) -> Just r
  Just (TaskRunning (Just r)) -> Just r
  Just (TaskFailed e _) -> Just (Left (show e))
  _ -> Nothing

heapView :: HeapState -> HeapLoaded -> Maybe HeapProfile -> NanoUI ()
heapView st loaded fromLog = columnWith (gap 8 . grow . padAll 10) $ do
  useFileDialog (hsHpDialog st) $ \case
    p : _ -> openHp st p
    [] -> pure ()
  useFileDialog (hsProfDialog st) $ \case
    p : _ -> openProf st p
    [] -> pure ()
  source <- readVar (hsSource st)
  hpPath <- readVar (hsHpPath st)
  let hpResult = done (hlHp loaded)
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    label "Source"
    boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} (\case FromEventlog -> "Eventlog samples"; FromHpFile -> ".hp file") (hsSource st)
    whenM (button "Open .hp…") $
      writeVar (hsHpDialog st) . Just =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "Heap profile" "hp"]}
    whenM (button "Open .prof…") $
      writeVar (hsProfDialog st) . Just =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "Cost-centre profile" "prof"]}
    scope $ when (source == FromHpFile) (pathLabel hpPath)
    flex
  let profile = case source of
        FromEventlog -> maybe (Left "The loaded eventlog has no heap samples. Run the program with +RTS -l -hT (or -hy, -hd, -hc with profiling, -hi with -finfo-table-map).") Right fromLog
        FromHpFile -> case (hpPath, hpResult) of
          (Nothing, _) -> Left "Open a .hp file (from +RTS -hT, -hc, -hy, -hd, -hr ...)."
          (_, Nothing) -> Left "Loading…"
          (_, Just r) -> r
  case profile of
    Left msg -> scope $ do
      hint (T.pack msg)
      -- The cost-centre tree does not need a heap profile.
      scope $ when (isJust (hlProf loaded)) $ costCentreTree st (done (hlProf loaded))
    Right hp
      | VU.null (hpTimes hp) -> scope (hint "The heap profile has no samples.")
      | otherwise -> scope (profileView st loaded hp)

-- ---------------------------------------------------------------------------
-- The stacked chart

data Layer = Layer
  { lyName :: !Text
  , lyColor :: !Color
  , lyValues :: !(VU.Vector Double)
  }

-- | Bands to draw: visible ones, the top N by peak, and the rest summed into
-- one "other" layer.
stackLayers :: HeapProfile -> Set Text -> Maybe Text -> Int -> [Layer]
stackLayers hp hidden solo topN =
  let bands = zip [0 ..] (V.toList (hpBands hp))
      visible = [(i, b) | (i, b) <- bands, maybe (not (S.member (bandName b) hidden)) (== bandName b) solo]
      (top, rest) = splitAt topN visible
      n = VU.length (hpTimes hp)
      other = foldr (VU.zipWith (+) . bandValues . snd) (VU.replicate n 0) rest
   in [Layer (bandName b) (bandColor i (bandName b)) (bandValues b) | (i, b) <- top]
        ++ [Layer ("(" <> tshow (length rest) <> " more)") (colorRGB 140 140 140) other | not (null rest)]

profileView :: HeapState -> HeapLoaded -> HeapProfile -> NanoUI ()
profileView st loaded hp = do
  th <- uiTheme
  let pal = palette th
  hidden <- readVar (hsHidden st)
  solo <- readVar (hsSolo st)
  topN <- round <$> readVar (hsTopN st)
  cursorT <- readVar (hsCursor st)
  mm <- monoMetrics 0.85
  let ls = stackLayers hp hidden solo topN
      times = hpTimes hp
      n = VU.length times
      tEnd = VU.last times
      cursorIx = maybe (n - 1) (sampleIndexAt hp) cursorT
  rowWith (tight . gap 12 . alignMid . fillW) $ do
    labelWith fontMuted ("Breakdown: " <> hpBreakdown hp)
    labelWith fontMuted (tshow n <> " samples over " <> fmtSec tEnd)
    labelWith fontMuted ("peak total " <> fmtBytesD (VU.maximum (hpTotals hp)))
    flex
    label "Bands"
    sliderWith (fixedW 120) 3 30 (hsTopN st)
    scope $ for_ solo $ \s -> whenM (button ("Unsolo " <> T.take 24 s)) (writeVar (hsSolo st) Nothing)
    scope $ unless (S.null hidden) $ whenM (button "Show all") (writeVar (hsHidden st) S.empty)
  rowWith (tight . gap 10 . fillW . fixedH 300) $ do
    mouse <- uiMousePos
    sid <- currentId
    prev <- fromMaybe (Rect 0 0 0 0) <$> lastRect sid
    let geom = chartGeom prev
        hoverT = if rectContains (cgPlot geom) mouse then Just (cgTime geom tEnd (v2X mouse)) else Nothing
        hoverLines = do
          t <- hoverT
          let i = nearestSample times t
              stackAt = scanl (+) 0 [lyValues l VU.! i | l <- ls]
              ymax = maxStack ls
              bytesAtMouse = (1 - (v2Y mouse - rectY (cgPlot geom)) / rectH (cgPlot geom)) * realToFrac ymax
              under = [l | (l, lo', hi') <- zip3 ls stackAt (drop 1 stackAt), realToFrac bytesAtMouse >= lo', realToFrac bytesAtMouse < hi']
          pure $
            ("t = " <> fmtSec (times VU.! i) <> "   total " <> fmtBytesD (hpTotals hp VU.! i))
              : [lyName l <> ": " <> fmtBytesD (lyValues l VU.! i) | l <- take 1 under]
    resp <-
      canvasConfigured
        defaultCanvasConfig {canvasLayout = (fillW . grow) defaultLayout, canvasTrackPointer = True, canvasCursor = Just (\_ _ _ -> UiCursorCrosshair)}
        (\r -> drawChart pal mm hp ls cursorIx r (hoverT >> fmap (v2X mouse,v2Y mouse,) hoverLines))
    drag <- useDrag2DOn resp
    when (dragActive drag) $
      writeVar (hsCursor st) (Just (cgTime (chartGeom (respRect resp)) tEnd (v2X (dragPosition drag))))
    legend st hp hidden solo
  tab <- readVar (hsTab st)
  boundedTabBar tabName (hsTab st)
  scope $ case tab of
    AtCursor -> atCursor st hp cursorIx
    Leaks -> leakView st hp
    Retention -> retentionView st hp cursorIx
    CostCentres -> costCentreTree st (done (hlProf loaded))

data ChartGeom = ChartGeom {cgPlot :: !Rect, cgTime :: Double -> Float -> Double}

chartGeom :: Rect -> ChartGeom
chartGeom (Rect x y w h) =
  let plot = Rect (x + 72) (y + 8) (max 10 (w - 80)) (max 10 (h - 30))
   in ChartGeom plot (\tEnd px -> max 0 (min tEnd (realToFrac ((px - rectX plot) / rectW plot) * tEnd)))

maxStack :: [Layer] -> Double
maxStack ls = case ls of
  [] -> 1
  _ -> max 1 (VU.maximum (foldr1 (VU.zipWith (+)) (map lyValues ls)))

nearestSample :: VU.Vector Double -> Double -> Int
nearestSample ts t =
  snd (minimum [(abs (ts VU.! i - t), i) | i <- [0 .. VU.length ts - 1]])

drawChart :: Palette -> MonoMetrics -> HeapProfile -> [Layer] -> Int -> Rect -> Maybe (Float, Float, [Text]) -> CanvasM ()
drawChart pal mm hp ls cursorIx r hover = do
  let ChartGeom plot@(Rect px py pw ph) _ = chartGeom r
      times = hpTimes hp
      tEnd = max 1e-9 (VU.last times)
      ymax = maxStack ls
      sx t = px + realToFrac (t / tEnd) * pw
      sy b = py + ph - realToFrac (b / ymax) * ph
      font = mmFont mm
  drawRoundedRect r 6 (pTrackBg pal)
  -- Grid and axes.
  let ystep = niceStep (ymax / 4)
  forM_ (takeWhile (<= ymax) [ystep, 2 * ystep ..]) $ \b -> do
    drawStroke (V2 px (sy b)) (V2 (px + pw) (sy b)) 1 (pGrid pal)
    drawTextWith font (V2 (px - 6) (sy b)) AlignEnd AlignMiddle (fmtBytesD b) (pMuted pal)
  let tstep = niceStep (tEnd / 8)
  forM_ (takeWhile (<= tEnd) [0, tstep ..]) $ \t ->
    drawTextWith font (V2 (sx t) (py + ph + 12)) AlignCenter AlignMiddle (fmtSec t) (pMuted pal)
  -- Stacked bands, bottom up.
  let n = VU.length times
      xs = [sx (times VU.! i) | i <- [0 .. n - 1]]
      go _ [] = pure ()
      go base (l : rest) = do
        let top = VU.zipWith (+) base (lyValues l)
            upper = [V2 xx (sy (top VU.! i)) | (i, xx) <- zip [0 ..] xs]
            lower = reverse [V2 xx (sy (base VU.! i)) | (i, xx) <- zip [0 ..] xs]
        when (n >= 2) $ drawPath (P.polygon (upper ++ lower)) (lyColor l)
        when (n == 1) $ drawRect (Rect px (sy (top VU.! 0)) 6 (sy (base VU.! 0) - sy (top VU.! 0))) (lyColor l)
        go top rest
  withClip plot $ go (VU.replicate n 0) ls
  -- Census ticks along the bottom, and the cursor.
  forM_ xs $ \xx -> drawStroke (V2 xx (py + ph)) (V2 xx (py + ph + 3)) 1 (pMuted pal)
  when (cursorIx < n) $ do
    let cx = sx (times VU.! cursorIx)
    drawStroke (V2 cx py) (V2 cx (py + ph)) 2 (pAccent pal)
  for_ hover $ \(hx, hy, lines') -> do
    drawStroke (V2 hx py) (V2 hx (py + ph)) 1 (pCursor pal)
    let tw = fromIntegral (maximum (1 : map T.length lines')) * mmCharW mm + 16
        th' = fromIntegral (length lines') * (mmLineH mm + 2) + 10
        tx = if hx + 14 + tw > rectX r + rectW r then hx - 14 - tw else hx + 14
        ty = min (rectY r + rectH r - th') (hy + 10)
        bg = if colorLuminance (pText pal) > 0.5 then colorRGB 30 32 36 else colorRGB 252 252 250
    drawRoundedRect (Rect tx ty tw th') 6 (alpha 0.96 bg)
    drawStrokeRoundedRect (Rect tx ty tw th') 6 1 (pGrid pal)
    forM_ (zip [0 :: Int ..] lines') $ \(i, l) ->
      drawTextWith font (V2 (tx + 8) (ty + 5 + (fromIntegral i + 0.5) * (mmLineH mm + 2))) AlignStart AlignMiddle l (pText pal)

legend :: HeapState -> HeapProfile -> Set Text -> Maybe Text -> NanoUI ()
legend st hp hidden solo =
  panelWith (fixedW 320 . fillH . padAll 6 . gap 2) $ do
    labelWith (fontMuted . fontSizeScale 0.85) "Click a swatch to hide, a name to solo"
    scrollWith (grow . tight) $ columnWith (tight . fillW . gap 1) $
      forM_ (zip [0 :: Int ..] (V.toList (hpBands hp))) $ \(i, b) -> withKey (bandName b) $ do
        let name = bandName b
            isHidden = S.member name hidden
            c = bandColor i name
        rowWith (tight . gap 6 . alignMid . fillW) $ do
          sw <- canvas (fixedWH 14 14) $ \r ->
            if isHidden then drawStrokeRoundedRect r 3 1.5 c else drawRoundedRect r 3 c
          when (respClicked sw) $ writeVar (hsHidden st) (if isHidden then S.delete name hidden else S.insert name hidden)
          picked <- selectableRow (solo == Just name) id $ do
            labelWith (fillW . fontSizeScale 0.9 . (if isHidden then fontMuted else id)) (T.take 60 name)
            labelWith (fontMono . fontMuted . fontSizeScale 0.85) (fmtBytesD (bandPeak b))
          when picked $ writeVar (hsSolo st) (if solo == Just name then Nothing else Just name)

-- ---------------------------------------------------------------------------
-- Tabs

atCursor :: HeapState -> HeapProfile -> Int -> NanoUI ()
atCursor st hp i = do
  byDelta <- readVar (hsByDelta st)
  let total = hpTotals hp VU.! i
      rows =
        sortOn
          (\(_, b) -> Down (if byDelta then abs (bandDelta b i) else bandValues b VU.! i))
          [(k, b) | (k, b) <- zip [0 :: Int ..] (V.toList (hpBands hp)), bandValues b VU.! i > 0 || bandDelta b i /= 0]
  rowWith (tight . gap 10 . alignMid . fillW) $ do
    labelWith fontMedium ("Census at " <> fmtSec (hpTimes hp VU.! i) <> " — " <> fmtBytesD total)
    flex
    checkboxWith alignMid "Sort by change since previous census" (hsByDelta st)
  rowWith (gap 8 . fillW . padXY 8 0) $ do
    labelWith (fontMuted . fillW) "Closure / band"
    labelWith (fontMuted . fixedW 90) "Kind"
    labelWith (fontMuted . fixedW 100) "Residency"
    labelWith (fontMuted . fixedW 100) "Δ previous"
    labelWith (fontMuted . fixedW 60) "Share"
  scrollWith (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
    forM_ (take 300 rows) $ \(k, b) -> withKey (bandName b) $ do
      let val = bandValues b VU.! i
          d = bandDelta b i
      picked <- selectableRow False id $ do
        _ <- canvas (fixedWH 10 10) (\r -> drawRoundedRect r 2 (bandColor k (bandName b)))
        labelWith (fillW . fontSizeScale 0.92) (bandName b)
        labelWith (fixedW 90 . kindTone (bandKind b)) (kindName (bandKind b))
        labelWith (fixedW 100 . fontMono) (fmtBytesD val)
        labelWith (fixedW 100 . fontMono . (if d > 0 then fontTone Warning else if d < 0 then fontTone Success else fontMuted)) ((if d > 0 then "+" else "") <> fmtBytesD d)
        labelWith (fixedW 60 . fontMuted) (fmtPct (val / max 1 total))
      when picked $ writeVar (hsSolo st) (Just (bandName b))

kindName :: BandKind -> Text
kindName = \case
  KindThunk -> "thunk"
  KindFunction -> "function"
  KindConstructor -> "data"
  KindData -> "primitive"
  KindUnknown -> "—"

kindTone :: BandKind -> Layout -> Layout
kindTone = \case
  KindThunk -> fontTone Warning
  _ -> fontMuted

leakView :: HeapState -> HeapProfile -> NanoUI ()
leakView st hp = do
  let suspects = take 12 (leakSuspects hp)
      thunkBands = [b | b <- V.toList (hpBands hp), bandKind b == KindThunk]
  scrollWith (grow . tight . minH 160) $ columnWith (tight . fillW . gap 8) $ do
    scope $
      if null suspects
        then hint "No band grows steadily across censuses. If memory still climbs, profile by retainer (-hr) or cost centre (-hc), or take censuses more often (-i0.05)."
        else hint "Bands whose residency keeps climbing census after census. Every census follows a major GC, so a band that never shrinks is data the program keeps alive."
    forM_ suspects $ \s -> do
      let b = hpBands hp V.! susBand s
      withKey (bandName b) $ panelWith (padAll 10 . gap 4 . fillW) $ do
        rowWith (tight . gap 8 . alignMid . fillW) $ do
          _ <- canvas (fixedWH 12 12) (\r -> drawRoundedRect r 3 (bandColor (susBand s) (bandName b)))
          labelWith (fontMedium . fillW) (bandName b)
          badge (if susScore s > 0.7 then Danger else Warning) ("leak score " <> fmtFixed 2 (susScore s))
          scope $ when (bandKind b == KindThunk) $ badge Warning "unforced thunks"
          whenM (buttonWith tight "Isolate") (writeVar (hsSolo st) (Just (bandName b)))
        rowWith (tight . gap 16 . wrap . fillW) $ do
          fact "grew" ("×" <> fmtFixed 1 (susGrowth s))
          fact "rate" (fmtRate (susSlope s))
          fact "censuses survived, never shrinking" (tshow (susSurvived s))
          fact "monotone" (fmtPct (susMonotone s))
          fact "peak" (fmtBytesD (bandPeak b))
        _ <- canvas (fillW . fixedH 36) $ \r -> sparkBand r (bandColor (susBand s) (bandName b)) (bandValues b)
        hint (diagnose hp b s)
    scope $ unless (null thunkBands) $ do
      separator
      sectionTitle "Thunk accumulator sentinel"
      hint "Thunks that are still on the heap at a census have survived a major GC without being forced. Thunk bands, and how many consecutive censuses they have only grown:"
      forM_ thunkBands $ \b -> withKey ("thunk:" <> bandName b) $ rowWith (gap 10 . fillW . padXY 8 0) $ do
        let vs = VU.take (analysedSamples hp) (bandValues b)
            steps = VU.zipWith (-) (VU.drop 1 vs) vs
            streak = VU.length (VU.takeWhile (>= 0) (VU.reverse steps))
        labelWith fillW (bandName b)
        labelWith (fontMono . fixedW 100) (fmtBytesD (VU.last vs))
        labelWith ((if streak >= 5 then fontTone Danger else fontMuted) . fixedW 200) (tshow streak <> " censuses growing")

sparkBand :: Rect -> Color -> VU.Vector Double -> CanvasM ()
sparkBand (Rect x y w h) c vs
  | VU.length vs < 2 = pure ()
  | otherwise = do
      let m = max 1 (VU.maximum vs)
          n = VU.length vs
          pts = [V2 (x + w * fromIntegral i / fromIntegral (n - 1)) (y + h - realToFrac (vs VU.! i / m) * (h - 2)) | i <- [0 .. n - 1]]
      drawPath (P.polygon (V2 x (y + h) : pts ++ [V2 (x + w) (y + h)])) (alpha 0.25 c)
      drawStrokePath (P.polyline pts) 1.5 c

-- | A plain-language reading of a suspect.
diagnose :: HeapProfile -> Band -> Suspect -> Text
diagnose hp b s = case bandKind b of
  KindThunk ->
    "Unevaluated thunks that have survived "
      <> tshow (susSurvived s)
      <> " consecutive censuses. Something keeps building suspended computations and nothing forces them: look for a lazy accumulator (modifyIORef instead of modifyIORef', foldl, a lazy State or Writer, Map.insertWith on a lazy map) that feeds this closure."
      <> retainHint
  KindFunction -> "Partial applications or closures accumulating: often a lazily built continuation or a callback list that is never cleared." <> retainHint
  KindData -> "Primitive storage (arrays, mutable references, stacks) growing: deep recursion grows STACK; a never-trimmed buffer grows ARR_WORDS." <> retainHint
  _ ->
    "Live data that keeps growing: a collection or cache that is never trimmed, or values retained by an unevaluated thunk higher up."
      <> retainHint
  where
    retainHint
      | hpBreakdown hp == "retainer set" = " The band name lists the retainers."
      | hpBreakdown hp == "cost centre" = " The band name is the cost-centre stack that allocated it."
      | otherwise = " Re-run with -hr (retainers) or -hc (cost centres) to see who holds it, or -hi with -finfo-table-map for source locations."

retentionView :: HeapState -> HeapProfile -> Int -> NanoUI ()
retentionView st hp i
  | not (looksStructured hp) =
      hint "The retention tree needs a breakdown with structure: cost-centre stacks (+RTS -hc, profiled build) are shown rooted at main or a CAF and read down to the allocation site; retainer sets (-hr) list what keeps each band alive."
  | otherwise = do
      openSet <- readVar (hsRetOpen st)
      let tree = retainTree hp i
          rows = flattenTree openSet "" 0 tree
      hint ("Census at " <> fmtSec (hpTimes hp VU.! i) <> ". Read top-down: who holds (or allocated) the bytes, down to the closures.")
      scrollWith (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
        forM_ rows $ \(path, depth, node, hasKids, isOpen) -> withKey path $ do
          picked <- selectableRow False id $ do
            spacer (Fixed (fromIntegral depth * 16)) Fit
            labelWith (fixedW 14 . fontMuted) (if hasKids then (if isOpen then "▾" else "▸") else " ")
            labelWith fillW (rtName node)
            _ <- canvas (fixedWH 120 8) $ \r@(Rect x y w h) -> do
              drawRoundedRect r 2 (alpha 0.15 (colorRGB 128 128 128))
              drawRoundedRect (Rect x y (w * realToFrac (rtBytes node / max 1 (rtBytes tree))) h) 2 (colorRGB 66 133 244)
            labelWith (fontMono . fixedW 90) (fmtBytesD (rtBytes node))
          when (picked && hasKids) $
            writeVar (hsRetOpen st) (if isOpen then S.delete path openSet else S.insert path openSet)

flattenTree :: Set Text -> Text -> Int -> RetainTree -> [(Text, Int, RetainTree, Bool, Bool)]
flattenTree openSet parent depth node =
  concat
    [ (path, depth, kid, not (null (rtChildren kid)), isOpen)
        : (if isOpen then flattenTree openSet path (depth + 1) kid else [])
    | kid <- take 200 (rtChildren node)
    , let path = parent <> "/" <> rtName kid
          isOpen = depth < 1 || S.member path openSet
    ]

costCentreTree :: HeapState -> Maybe (Either String ProfFile) -> NanoUI ()
costCentreTree st = \case
  Nothing -> hint "Open a .prof file (from a profiled build run with +RTS -p) to browse its cost-centre tree."
  Just (Left e) -> labelWith (fontTone Danger) (T.pack e)
  Just (Right pf) -> do
    forM_ (take 4 (drop 1 (pfHeader pf))) (labelWith (fontMuted . fontSizeScale 0.9) . T.strip)
    filt <- T.toLower <$> readVar (hsCCFilter st)
    openSet <- readVar (hsCCOpen st)
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      searchInput "Filter cost centres" (hsCCFilter st)
      flex
      labelWith (fontMuted . fixedW 70) "entries"
      labelWith (fontMuted . fixedW 60) "%time"
      labelWith (fontMuted . fixedW 60) "%alloc"
      labelWith (fontMuted . fixedW 150) "inherited time / alloc"
    let matches cc = filt `T.isInfixOf` T.toLower (ccName cc <> " " <> ccModule cc)
        rows
          | T.null filt = visibleCC openSet 0 (pfRoots pf)
          | otherwise = [(d, cc, False, False) | (d, cc) <- flattenCC (pfRoots pf), matches cc]
    scrollWith (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
      forM_ (take 2000 rows) $ \(depth, cc, hasKids, isOpen) -> withKey (ccNo cc) $ do
        picked <- selectableRow False id $ do
          spacer (Fixed (fromIntegral (min 30 depth) * 14)) Fit
          labelWith (fixedW 14 . fontMuted) (if hasKids then (if isOpen then "▾" else "▸") else " ")
          labelWith fillW (ccName cc <> "  ")
          labelWith (fontMuted . fontSizeScale 0.85) (ccModule cc)
          labelWith (fontMono . fixedW 70) (fmtInt (ccEntries cc))
          labelWith (fontMono . fixedW 60) (fmtFixed 1 (ccTime cc))
          labelWith (fontMono . fixedW 60) (fmtFixed 1 (ccAlloc cc))
          _ <- canvas (fixedWH 150 10) $ \(Rect x y w h) -> do
            drawRoundedRect (Rect x y w (h / 2 - 1)) 1 (alpha 0.15 (colorRGB 128 128 128))
            drawRoundedRect (Rect x y (w * realToFrac (ccInhTime cc / 100)) (h / 2 - 1)) 1 (colorRGB 76 175 80)
            drawRoundedRect (Rect x (y + h / 2) (w * realToFrac (ccInhAlloc cc / 100)) (h / 2 - 1)) 1 (colorRGB 66 133 244)
          pure ()
        when (picked && hasKids) $
          writeVar (hsCCOpen st) (if isOpen then S.delete (ccNo cc) openSet else S.insert (ccNo cc) openSet)

visibleCC :: Set Int -> Int -> [CostCentre] -> [(Int, CostCentre, Bool, Bool)]
visibleCC openSet depth ccs =
  concat
    [ (depth, cc, not (null (ccChildren cc)), isOpen)
        : (if isOpen then visibleCC openSet (depth + 1) (sortOn (Down . ccInhTime) (ccChildren cc)) else [])
    | cc <- ccs
    , let isOpen = S.member (ccNo cc) openSet || depth < 1
    ]
