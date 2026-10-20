-- | The heap explorer and space-leak hunter: stacked residency bands over
-- time with a scrub cursor, the breakdown at the cursor, ranked leak
-- suspects (with the thunk sentinel), the retention tree, and the
-- cost-centre tree from a @.prof@ file. With the @ghc-debug@ build flag it
-- can also snapshot a running program's closure graph and show exact
-- retainer paths and thunks that survive between snapshots.
module UI.Heap
  ( HeapState (..)
  , HeapSource (..)
  , newHeapState
  , heapLoad
  , heapView
  , openHp
  , openProf
  , HeapLoaded (..)
  , heapLens
  ) where

import Control.Exception (displayException)
import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)
import NanoUI.Path qualified as P
import Stronghead.Debug
import Stronghead.Dump (nameKey)
import Stronghead.Format
import Stronghead.HeapProfile
import Stronghead.Join
import Stronghead.Prof
import Stronghead.Ref
import Stronghead.Run (runFromSocket, runKey, runKeysMatch)
import Stronghead.Source (parseSrcLoc, renderSrcSpan, srcLocSpan)
import System.FilePath (takeFileName)
import UI.Common
import UI.Link

data HeapSource = FromEventlog | FromHpFile | FromGhcDebug
  deriving (Eq, Show, Enum, Bounded)

-- | How to reach the program for a ghc-debug snapshot.
data DbgConn = ConnSocket | ConnTcp | ConnFile
  deriving (Eq, Show, Enum, Bounded)

data DbgTab = DbgSurvivors | DbgGraph | DbgBiggest
  deriving (Eq, Show, Enum, Bounded)

-- | A snapshot to take: a counter (so the same target can be snapshotted
-- again), the target and the options.
type SnapRequest = (Int, DebugTarget, SnapshotOptions)

-- | Snapshots taken, oldest first, stamped with the request that added the
-- last one (-1 once cleared). Compared by stamp: comparing heaps is too slow.
data History = History !Int ![HeapSnapshot]

instance Eq History where
  History a _ == History b _ = a == b

-- | Snapshots kept for the survivor analysis.
keepSnapshots :: Int
keepSnapshots = 8

data HeapTab = AtCursor | Leaks | Retention | CostCentres
  deriving (Eq, Show, Enum, Bounded)

tabName :: HeapTab -> Text
tabName = \case
  AtCursor -> "At cursor"
  Leaks -> "Leak suspects"
  Retention -> "Retention tree"
  CostCentres -> "Cost centres"

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
  , hsDbgConn :: !(Var DbgConn)
  , hsDbgSocket :: !(Var Text)
  , hsDbgHostPort :: !(Var Text)
  , hsDbgFile :: !(Var (Maybe FilePath))
  , hsDbgFileDialog :: !(Var (Maybe FileDialogId))
  , hsDbgSockets :: !(Var [FilePath])
  , hsDbgLimit :: !(Var Float)
    -- ^ Closure limit, in thousands.
  , hsDbgSrt :: !(Var Bool)
  , hsDbgReq :: !(Var (Maybe SnapRequest))
  , hsDbgTask :: !(Task SnapRequest (Either String HeapSnapshot))
  , hsDbgStored :: !(Var Int)
    -- ^ The request whose snapshot is already in the history.
  , hsDbgHistory :: !(Var History)
    -- ^ Oldest first.
  , hsDbgSurvivors :: !(Var [Survivor])
  , hsDbgMarked :: !(Var (M.Map ClosureId Int))
    -- ^ Thunks of the latest snapshot in a surviving group, and for how many
    -- snapshots it has survived.
  , hsDbgTab :: !(Var DbgTab)
  , hsDbgSel :: !(Var (Maybe ClosureId))
  , hsDbgOpen :: !(Var (Set Text))
  , hsDbgSources :: !(Var [SourceGroup])
    -- ^ The latest snapshot's closures by source, for the code views.
  , hsDbgFind :: !(Var (Maybe Band))
    -- ^ A band whose closures the live graph lists.
  , hsCCSel :: !(Var (Maybe Int))
    -- ^ The cost centre a link opened the tree at.
  , hsCostTotals :: !(Var (Maybe (FilePath, [CostTotal])))
    -- ^ The @.prof@'s cost centres summed, once per file.
  , hsPanes :: !Workbench
  , hsDbgPanes :: !Workbench
  , hsCCPanes :: !Workbench
    -- ^ The cost-centre tree on its own, when there is no heap profile.
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
  hsDbgConn <- newVar ConnSocket
  hsDbgSocket <- newVar ""
  hsDbgHostPort <- newVar "127.0.0.1:1235"
  hsDbgFile <- newVar Nothing
  hsDbgFileDialog <- newVar Nothing
  hsDbgSockets <- newVar []
  hsDbgLimit <- newVar (fromIntegral (soMaxClosures defaultSnapshotOptions `div` 1000))
  hsDbgSrt <- newVar (soFollowSrt defaultSnapshotOptions)
  hsDbgReq <- newVar Nothing
  hsDbgTask <- newTask
  hsDbgStored <- newVar 0
  hsDbgHistory <- newVar (History 0 [])
  hsDbgSurvivors <- newVar []
  hsDbgMarked <- newVar M.empty
  hsDbgTab <- newVar DbgSurvivors
  hsDbgSel <- newVar Nothing
  hsDbgOpen <- newVar S.empty
  hsDbgSources <- newVar []
  hsDbgFind <- newVar Nothing
  hsCCSel <- newVar Nothing
  hsCostTotals <- newVar Nothing
  hsPanes <- newVar (Just (Split 10 AxisH 0.46 (Split 11 AxisV 0.75 (Pane paneChart) (Pane paneLegend)) (Pane paneAnalysis)))
  hsDbgPanes <- newVar (Just (Pane 1))
  hsCCPanes <- newVar (Just (Pane 1))
  pure HeapState {..}

paneChart, paneLegend, paneAnalysis :: Word64
paneChart = 1
paneLegend = 2
paneAnalysis = 3

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
  , hlSnap :: !(Maybe (TaskStatus (Either String HeapSnapshot)))
  }

-- | Run the file loaders and the ghc-debug snapshot; called every frame, so
-- a snapshot keeps running when the panel is switched away from.
heapLoad :: HeapState -> NanoUI HeapLoaded
heapLoad st = do
  hp <- readVar (hsHpPath st)
  prof <- readVar (hsProfPath st)
  req <- readVar (hsDbgReq st)
  hlHp <- traverse (\p -> useTaskStatus (hsHpTask st) p (loadHp p)) hp
  hlProf <- traverse (\p -> useTaskStatus (hsProfTask st) p (loadProf p)) prof
  totals <- readVar (hsCostTotals st)
  case (prof, done hlProf) of
    (Just p, Just (Right pf)) | fmap fst totals /= Just p -> writeVar (hsCostTotals st) (Just (p, costTotals (pfRoots pf)))
    _ -> pure ()
  hlSnap <- traverse (\r@(_, target, opts) -> useTaskStatus (hsDbgTask st) r (takeSnapshot opts target)) req
  case (req, hlSnap) of
    (Just (n, _, _), Just (TaskDone (Right snap))) -> do
      stored <- readVar (hsDbgStored st)
      when (stored /= n) $ do
        writeVar (hsDbgStored st) n
        History _ old <- readVar (hsDbgHistory st)
        let history = drop (length old + 1 - keepSnapshots) old ++ [snap]
            survivors = survivingThunks history
        writeVar (hsDbgHistory st) (History n history)
        writeVar (hsDbgSurvivors st) survivors
        writeVar (hsDbgMarked st) (survivorIds snap survivors)
        writeVar (hsDbgSources st) (closuresBySource snap (survivorIds snap survivors))
        writeVar (hsDbgOpen st) S.empty
        writeVar (hsDbgSel st) Nothing
    _ -> pure ()
  pure HeapLoaded {..}

done :: Maybe (TaskStatus (Either String a)) -> Maybe (Either String a)
done = \case
  Just (TaskDone r) -> Just r
  Just (TaskRunning (Just r)) -> Just r
  Just (TaskFailed e _) -> Just (Left (show e))
  _ -> Nothing

heapView :: Env -> HeapState -> HeapLoaded -> Maybe HeapProfile -> NanoUI ()
heapView env st loaded fromLog = columnWith (tight . gap 0 . grow) $ do
  useFileDialog (hsHpDialog st) $ \case
    p : _ -> openHp st p
    [] -> pure ()
  useFileDialog (hsProfDialog st) $ \case
    p : _ -> openProf st p
    [] -> pure ()
  source <- readVar (hsSource st)
  hpPath <- readVar (hsHpPath st)
  let hpResult = done (hlHp loaded)
  toolStrip $ do
    toolLabel "Source"
    rowWith (alignMid . tight) $
      boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} (\case FromEventlog -> "Eventlog samples"; FromHpFile -> ".hp file"; FromGhcDebug -> "Live graph (ghc-debug)") (hsSource st)
    scope $ unless (source == FromGhcDebug) $ toolGroup $ do
      whenM (toolButton "Open heap profile…") $
        writeVar (hsHpDialog st) . Just =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "Heap profile" "hp"]}
      whenM (toolButton "Open cost-centre profile…") $
        writeVar (hsProfDialog st) . Just =<< askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "Cost-centre profile" "prof"]}
    flex
  let profile = case source of
        FromGhcDebug -> Left ("", [], [])
        FromEventlog ->
          maybe
            (Left ("No heap samples in the eventlog", ["Open an eventlog recorded with heap profiling on, or switch the source to a .hp file."], ["./Main +RTS -l -hT -i0.1 -RTS"]))
            Right
            fromLog
        FromHpFile -> case (hpPath, hpResult) of
          (Nothing, _) -> Left ("Open a heap profile", ["Open the .hp file a program writes when run with heap profiling on, or drop one on the window."], ["./Main +RTS -hT -i0.1 -RTS", "./Main +RTS -hc -RTS      (profiled build: by cost centre)"])
          (_, Nothing) -> Left ("Reading the heap profile…", [], [])
          (_, Just (Left e)) -> Left ("Could not read the heap profile", [T.pack e], [])
          (_, Just (Right hp)) -> Right hp
  case profile of
    _ | source == FromGhcDebug -> scope (ghcDebugView env st loaded)
    Left (title, body, commands) -> scope $
      -- The cost-centre tree does not need a heap profile.
      if isJust (hlProf loaded)
        then workbench (hsCCPanes st) $ \_ ctx -> pane ctx "Cost centres" (pure ()) (costCentreTree env st (done (hlProf loaded)))
        else emptyState title body commands
    Right hp
      | VU.null (hpTimes hp) -> scope (emptyState "The heap profile has no samples" ["The program may have exited before the first census. Take censuses more often with -i0.05."] [])
      | otherwise -> scope (profileView env st loaded hp)

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

profileView :: Env -> HeapState -> HeapLoaded -> HeapProfile -> NanoUI ()
profileView env st loaded hp = do
  th <- uiTheme
  let pal = palette th
  hidden <- readVar (hsHidden st)
  solo <- readVar (hsSolo st)
  topN <- round <$> readVar (hsTopN st)
  cursorT <- readVar (hsCursor st)
  mm <- monoMetrics 0.85
  let ls = stackLayers hp hidden solo topN
      times = hpOffsets hp
      n = VU.length times
      tEnd = VU.last times
      -- Until the cursor is moved it sits on the peak census: the last one
      -- is often taken as the program exits, with little left alive.
      peakIx = VU.maxIndex (hpTotals hp)
      cursorIx = maybe peakIx (sampleIndexAt hp . (+ hpStart hp)) cursorT
  columnWith (padBottom 8 . tight . fillW) $
    readout
      [ ("breakdown", hpBreakdown hp)
      , ("censuses", tshow n)
      , ("profiled", fmtSec tEnd)
      , ("peak total", fmtBytesD (VU.maximum (hpTotals hp)))
      , ("cursor at", fmtSec (times VU.! cursorIx))
      , ("live at cursor", fmtBytesD (hpTotals hp VU.! cursorIx))
      ]
  tab <- readVar (hsTab st)
  workbench (hsPanes st) $ \pid ctx ->
    if
      | pid == paneChart -> do
          let actions = do
                paneMeta "Drag across to move the census cursor"
                flex
                scope $ for_ solo $ \s -> whenM (buttonWith (alignMid . padXY 8 2 . tight) ("Unsolo " <> T.take 24 s)) (writeVar (hsSolo st) Nothing)
                scope $ unless (S.null hidden) $ whenM (buttonWith (alignMid . padXY 8 2 . tight) "Show all bands") (writeVar (hsHidden st) S.empty)
                labelWith (alignMid . fontMuted . fontSizeScale 0.85) ("Top " <> tshow topN <> " bands")
                sliderWith (alignMid . fixedW 110) 3 30 (hsTopN st)
          paneWith (padAll 8) ctx "Residency" actions $ do
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
                    ("t = " <> fmtSec (times VU.! i) <> ", total " <> fmtBytesD (hpTotals hp VU.! i))
                      : [lyName l <> ": " <> fmtBytesD (lyValues l VU.! i) | l <- take 1 under]
            resp <-
              canvasConfigured
                defaultCanvasConfig {canvasLayout = (fillW . fillH) defaultLayout, canvasTrackPointer = True, canvasCursor = Just (\_ _ _ -> UiCursorCrosshair)}
                (\r -> drawChart pal mm hp ls cursorIx r (hoverT >> fmap (v2X mouse,v2Y mouse,) hoverLines))
            canvasLinks env resp (chartRefAt hp ls (respRect resp))
            drag <- useDrag2DOn resp
            when (dragActive drag) $
              writeVar (hsCursor st) (Just (cgTime (chartGeom (respRect resp)) tEnd (v2X (dragPosition drag))))
      | pid == paneLegend ->
          paneWith (padAll 6 . gap 2) ctx "Bands" (paneMeta (tshow (V.length (hpBands hp)))) (legend env st hp hidden solo)
      | otherwise -> do
          let actions = do
                flex
                rowWith (alignMid . tight) $
                  boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabContained} tabName (hsTab st)
          pane ctx (analysisTitle tab) actions $ case tab of
            AtCursor -> atCursor env st hp cursorIx
            Leaks -> leakView env st hp
            Retention -> retentionView env st hp cursorIx
            CostCentres -> costCentreTree env st (done (hlProf loaded))

analysisTitle :: HeapTab -> Text
analysisTitle = \case
  AtCursor -> "Census at the cursor"
  Leaks -> "Leak suspects"
  Retention -> "Retention tree"
  CostCentres -> "Cost centres"

data ChartGeom = ChartGeom {cgPlot :: !Rect, cgTime :: Double -> Float -> Double}

chartGeom :: Rect -> ChartGeom
chartGeom (Rect x y w h) =
  let plot = Rect (x + 72) (y + 8) (max 10 (w - 80)) (max 10 (h - 30))
   in ChartGeom plot (\tEnd px -> max 0 (min tEnd (realToFrac ((px - rectX plot) / rectW plot) * tEnd)))

-- | What the pointer is on in the chart: the band under it, or else the
-- census nearest it.
chartRefAt :: HeapProfile -> [Layer] -> Rect -> V2 -> Maybe Ref
chartRefAt hp ls r m@(V2 mx my)
  | not (rectContains plot m) || VU.null times = Nothing
  | otherwise = case under of
      l : _ | V.any ((== lyName l) . bandName) (hpBands hp) -> Just (RBand (lyName l))
      _ -> Just (RCensus (runKey (hpRun hp)) i)
  where
    geom = chartGeom r
    plot = cgPlot geom
    times = hpOffsets hp
    i = nearestSample times (cgTime geom (VU.last times) mx)
    stackAt = scanl (+) 0 [lyValues l VU.! i | l <- ls]
    bytes = realToFrac ((1 - (my - rectY plot) / rectH plot) * realToFrac (maxStack ls)) :: Double
    under = [l | (l, lo, hi) <- zip3 ls stackAt (drop 1 stackAt), bytes >= lo, bytes < hi]

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
      times = hpOffsets hp
      tEnd = max 1e-9 (VU.last times)
      ymax = maxStack ls
      sx t = px + realToFrac (t / tEnd) * pw
      sy b = py + ph - realToFrac (b / ymax) * ph
      font = mmFont mm
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

legend :: Env -> HeapState -> HeapProfile -> Set Text -> Maybe Text -> NanoUI ()
legend env st hp hidden solo = do
    labelWith (padXY 6 2 . fontMuted . fontSizeScale 0.85 . fillW) "Click a swatch to hide a band, a name to show it alone"
    paneScroll (grow . tight) $ columnWith (tight . fillW . gap 1) $
      forM_ (zip [0 :: Int ..] (V.toList (hpBands hp))) $ \(i, b) -> withKey (bandName b) $ do
        let name = bandName b
            isHidden = S.member name hidden
            c = bandColor i name
        rowWith (padLeft 6 . tight . gap 4 . fillW) $ do
          sw <- canvas (fixedWH 12 12 . alignMid) $ \r ->
            if isHidden then drawStrokeRoundedRect r 3 1.5 c else drawRoundedRect r 3 c
          when (respClicked sw) $ writeVar (hsHidden st) (if isHidden then S.delete name hidden else S.insert name hidden)
          picked <- linkRow env (RBand name) (solo == Just name) id $ do
            cell (fillW . fontSizeScale 0.9 . (if isHidden then fontMuted else id)) (T.take 60 (bandTitle b))
            numCell 72 (fontMuted . fontSizeScale 0.85) (fmtBytesD (bandPeak b))
          when picked $ writeVar (hsSolo st) (if solo == Just name then Nothing else Just name)

-- ---------------------------------------------------------------------------
-- Tabs

atCursor :: Env -> HeapState -> HeapProfile -> Int -> NanoUI ()
atCursor env st hp i = do
  byDelta <- readVar (hsByDelta st)
  let total = hpTotals hp VU.! i
      rows =
        sortOn
          (\(_, b) -> Down (if byDelta then abs (bandDelta b i) else bandValues b VU.! i))
          [(k, b) | (k, b) <- zip [0 :: Int ..] (V.toList (hpBands hp)), bandValues b VU.! i > 0 || bandDelta b i /= 0]
  rowWith (tight . gap 10 . fillW) $ do
    labelWith (alignMid . fontMuted) (fmtSec (hpOffset hp i) <> ", " <> fmtBytesD total <> " live")
    flex
    checkboxWith alignMid "Sort by change since the previous census" (hsByDelta st)
  rowWith (padXY 8 0 . tight . gap 8 . fillW) $ do
    colHeader (fixedW 10) ""
    colHeader fillW "closure or band"
    colHeader (fixedW 90) "kind"
    colHeader (fixedW 100 . alignEnd) "residency"
    colHeader (fixedW 100 . alignEnd) "since previous"
    colHeader (fixedW 60 . alignEnd) "share"
  paneScroll (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
    forM_ (take 300 rows) $ \(k, b) -> withKey (bandName b) $ do
      let val = bandValues b VU.! i
          d = bandDelta b i
      picked <- linkRow env (RBand (bandName b)) False id $ do
        _ <- canvas (fixedWH 10 10 . alignMid) (\r -> drawRoundedRect r 2 (bandColor k (bandName b)))
        cell (fillW . fontSizeScale 0.92) (bandTitle b)
        cell (fixedW 90 . kindTone (bandKind b)) (kindName (bandKind b))
        numCell 100 id (fmtBytesD val)
        numCell 100 (if d > 0 then fontTone Warning else if d < 0 then fontTone Success else fontMuted) ((if d > 0 then "+" else "") <> fmtBytesD d)
        numCell 60 fontMuted (fmtPct (val / max 1 total))
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

leakView :: Env -> HeapState -> HeapProfile -> NanoUI ()
leakView env st hp = do
  let suspects = take 12 (leakSuspects hp)
      thunkBands = [b | b <- V.toList (hpBands hp), bandKind b == KindThunk]
  paneScroll (grow . tight . minH 160) $ columnWith (tight . fillW . gap 8) $ do
    scope $
      if null suspects
        then hint "No band grows steadily across censuses. If memory still climbs, profile by retainer (-hr) or cost centre (-hc), or take censuses more often (-i0.05)."
        else hint "Bands whose residency keeps climbing census after census. Every census follows a major GC, so a band that never shrinks is data the program keeps alive."
    forM_ suspects $ \s -> do
      let b = hpBands hp V.! susBand s
      withKey (bandName b) $ panelWith (padAll 10 . gap 4 . fillW) $ do
        rowWith (tight . gap 8 . alignMid . fillW) $ do
          _ <- canvas (fixedWH 12 12) (\r -> drawRoundedRect r 3 (bandColor (susBand s) (bandName b)))
          linkText env (RBand (bandName b)) fontMedium (bandName b)
          flex
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
        scope $ for_ (bandProv b) $ \ip -> provenance env ip
        hint (diagnose hp b s)
    scope $ unless (null thunkBands) $ do
      separator
      sectionTitle "Thunk accumulator sentinel"
      hint "Thunks that are still on the heap at a census have survived a major GC without being forced. Thunk bands, and how many consecutive censuses they have only grown:"
      forM_ thunkBands $ \b -> withKey ("thunk:" <> bandName b) $ rowWith (gap 10 . fillW . padXY 8 0) $ do
        let vs = VU.take (analysedSamples hp) (bandValues b)
            steps = VU.zipWith (-) (VU.drop 1 vs) vs
            streak = VU.length (VU.takeWhile (>= 0) (VU.reverse steps))
        linkText env (RBand (bandName b)) id (bandName b)
        flex
        labelWith (fontMono . fixedW 100) (fmtBytesD (VU.last vs))
        labelWith ((if streak >= 5 then fontTone Danger else fontMuted) . fixedW 200) (tshow streak <> " censuses growing")

-- | Where a band's closures come from, with the site and the binding as
-- links.
provenance :: Env -> InfoProv -> NanoUI ()
provenance env ip =
  prose env $
    ["Allocated "]
      ++ maybe [] (\sp -> ["at ", refInline (RSource sp) (renderSrcSpan sp), " "]) (srcLocSpan (ipSrc ip))
      ++ (if T.null (ipLabel ip) then [] else ["in ", refInline (RBinding (ipModule ip) (nameKey (ipLabel ip))) (ipLabel ip)])
      ++ [" (", inlineText (ipTableName ip), ")."]

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

retentionView :: Env -> HeapState -> HeapProfile -> Int -> NanoUI ()
retentionView env st hp i
  | not (looksStructured hp) =
      hint "The retention tree needs a breakdown with structure: cost-centre stacks (+RTS -hc, profiled build) are shown rooted at main or a CAF and read down to the allocation site; retainer sets (-hr) list what keeps each band alive."
  | otherwise = do
      openSet <- readVar (hsRetOpen st)
      let tree = retainTree hp i
          rows = flattenTree openSet "" 0 tree
      hint ("Census at " <> fmtSec (hpOffset hp i) <> ". Read top-down: who holds (or allocated) the bytes, down to the closures. For exact retainer paths, snapshot the running program with Source: Live graph (ghc-debug).")
      paneScroll (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
        forM_ rows $ \(path, depth, node, hasKids, isOpen) -> withKey path $ do
          let ref = if hpBreakdown hp == "cost centre" then Just (RCostCentre "" (rtName node)) else Nothing
          picked <- maybe (selectableRow False id) (\r -> linkRow env r False id) ref $ do
            spacer (Fixed (fromIntegral depth * 16)) Fit
            cell (fixedW 14 . fontMuted) (if hasKids then (if isOpen then "▾" else "▸") else " ")
            labelWith fillW (rtName node)
            _ <- canvas (fixedWH 120 8) $ \r@(Rect x y w h) -> do
              drawRoundedRect r 2 (alpha 0.15 (colorRGB 128 128 128))
              drawRoundedRect (Rect x y (w * realToFrac (rtBytes node / max 1 (rtBytes tree))) h) 2 (colorRGB 66 133 244)
            cell (fontMono . fixedW 90) (fmtBytesD (rtBytes node))
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

costCentreTree :: Env -> HeapState -> Maybe (Either String ProfFile) -> NanoUI ()
costCentreTree env st = \case
  Nothing -> hint "Open a .prof file (from a profiled build run with +RTS -p) to browse its cost-centre tree."
  Just (Left e) -> labelWith (fontTone Danger) (T.pack e)
  Just (Right pf) -> do
    pal <- palette <$> uiTheme
    sel <- readVar (hsCCSel st)
    forM_ (take 4 (drop 1 (pfHeader pf))) (labelWith (fontMuted . fontSizeScale 0.9) . T.strip)
    filt <- T.toLower <$> readVar (hsCCFilter st)
    openSet <- readVar (hsCCOpen st)
    searchInputConfigured defaultSearchInputConfig {sicPlaceholder = "Filter cost centres", sicLayout = (fixedW 280) defaultLayout} (hsCCFilter st)
    rowWith (padXY 8 0 . tight . gap 8 . fillW) $ do
      colHeader fillW "cost centre"
      colHeader (fixedW 70 . alignEnd) "entries"
      colHeader (fixedW 60 . alignEnd) "time %"
      colHeader (fixedW 60 . alignEnd) "alloc %"
      colHeader (fixedW 150) "inherited time, alloc"
    let matches cc = filt `T.isInfixOf` T.toLower (ccName cc <> " " <> ccModule cc)
        rows
          | T.null filt = visibleCC openSet 0 (pfRoots pf)
          | otherwise = [(d, cc, False, False) | (d, cc) <- flattenCC (pfRoots pf), matches cc]
    paneScroll (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
      forM_ (take 2000 rows) $ \(depth, cc, hasKids, isOpen) -> withKey (ccNo cc) $ do
        picked <- linkRow env (RCostCentre (ccModule cc) (ccName cc)) (sel == Just (ccNo cc)) id $ do
          spacer (Fixed (fromIntegral (min 30 depth) * 14)) Fit
          cell (fixedW 14 . fontMuted) (if hasKids then (if isOpen then "▾" else "▸") else " ")
          cell id (ccName cc)
          cell (fillW . fontMuted . fontSizeScale 0.85) (ccModule cc)
          numCell 70 id (fmtInt (ccEntries cc))
          numCell 60 id (fmtFixed 1 (ccTime cc))
          numCell 60 id (fmtFixed 1 (ccAlloc cc))
          _ <- canvas (fixedWH 150 10 . alignMid) $ \(Rect x y w h) -> do
            drawRoundedRect (Rect x y w (h / 2 - 1)) 1 (alpha 0.12 (pText pal))
            drawRoundedRect (Rect x y (w * realToFrac (ccInhTime cc / 100)) (h / 2 - 1)) 1 (pRun pal)
            drawRoundedRect (Rect x (y + h / 2) (w * realToFrac (ccInhAlloc cc / 100)) (h / 2 - 1)) 1 (pAccent pal)
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

-- ---------------------------------------------------------------------------
-- Live closure graph (ghc-debug)

connName :: DbgConn -> Text
connName = \case
  ConnSocket -> "Unix socket"
  ConnTcp -> "TCP"
  ConnFile -> "Snapshot file"

dbgTabName :: DbgTab -> Text
dbgTabName = \case
  DbgSurvivors -> "Surviving thunks"
  DbgGraph -> "Closure graph"
  DbgBiggest -> "Biggest retainers"

ghcDebugView :: Env -> HeapState -> HeapLoaded -> NanoUI ()
ghcDebugView env st loaded
  | not ghcDebugEnabled =
      emptyState
        "Built without ghc-debug"
        [ "The live graph pauses a running program through ghc-debug, walks every closure reachable from its GC roots, and shows exact retainer paths. Rebuild Stronghead with the flag to turn it on."
        , "The program being debugged links ghc-debug-stub and wraps main in withGhcDebug (Unix socket) or withGhcDebugTCP. The stub does not build on Windows; from Windows, connect over TCP to a program running on Linux, macOS or WSL, or open a snapshot file."
        ]
        ["cabal build -f ghc-debug"]
  | otherwise = do
      useFileDialog (hsDbgFileDialog st) $ \case
        p : _ -> writeVar (hsDbgFile st) (Just p)
        [] -> pure ()
      conn <- readVar (hsDbgConn st)
      socket <- T.strip <$> readVar (hsDbgSocket st)
      hostPort <- readVar (hsDbgHostPort st)
      file <- readVar (hsDbgFile st)
      sockets <- readVar (hsDbgSockets st)
      History _ history <- readVar (hsDbgHistory st)
      limit <- readVar (hsDbgLimit st)
      let target = case conn of
            ConnSocket -> if T.null socket then Nothing else Just (DebugUnix (T.unpack socket))
            ConnTcp -> uncurry DebugTCP <$> parseHostPort hostPort
            ConnFile -> DebugSnapshotFile <$> file
          running = case hlSnap loaded of
            Just (TaskRunning _) -> True
            _ -> False
      toolStrip $ do
        toolLabel "Target"
        rowWith (alignMid . tight) $
          boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} connName (hsDbgConn st)
        scope $ case conn of
          ConnSocket -> do
            textInputConfigured defaultTextInputConfig {ticPlaceholder = "socket path, printed by withGhcDebug", ticLayout = (alignMid . minW 260 . fillW) defaultLayout} (hsDbgSocket st)
            whenM (toolButton "Find sockets") (writeVar (hsDbgSockets st) =<< liftIO listSockets)
          ConnTcp ->
            textInputConfigured defaultTextInputConfig {ticPlaceholder = "host:port, as given to withGhcDebugTCP", ticLayout = (alignMid . minW 200 . fillW) defaultLayout} (hsDbgHostPort st)
          ConnFile -> do
            whenM (toolButton "Open snapshot…") $
              writeVar (hsDbgFileDialog st) . Just =<< askOpenFileDialog defaultFileDialogOptions
            pathLabel file
            flex
      scope $ when (conn == ConnSocket && not (null sockets)) $
        toolStrip $ do
          toolLabel "Sockets found"
          forM_ sockets $ \p -> withKey (T.pack p) $
            whenM (toolButton (T.pack (takeFileName p))) (writeVar (hsDbgSocket st) (T.pack p))
      toolStrip $ do
        disabledWhen (running || isNothing target) $ whenM (styled primary (toolButton "Take snapshot")) $ for_ target $ \t -> do
          srt <- readVar (hsDbgSrt st)
          n <- maybe 1 (\(k, _, _) -> k + 1) <$> readVar (hsDbgReq st)
          writeVar (hsDbgReq st) (Just (n, t, SnapshotOptions {soMaxClosures = round limit * 1000, soFollowSrt = srt}))
        toolGroup $ do
          toolLabel "Closure limit"
          sliderWith (alignMid . fixedW 120) 10 1000 (hsDbgLimit st)
          numCell 52 id (fmtInt (round limit :: Int) <> "k")
        toolGroup $ checkboxWith alignMid "Follow SRTs (CAFs)" (hsDbgSrt st)
        disabledWhen (null history || running) $ whenM (toolButton "Clear snapshots") $ do
          writeVar (hsDbgHistory st) (History (-1) [])
          writeVar (hsDbgSurvivors st) []
          writeVar (hsDbgMarked st) M.empty
          writeVar (hsDbgSel st) Nothing
          writeVar (hsDbgOpen st) S.empty
        flex
        scope $ case hlSnap loaded of
          Just (TaskRunning _) -> labelWith (alignMid . fontTone Warning) "Target paused: walking its heap…"
          Just (TaskDone (Left e)) -> labelWith (alignMid . fontTone Danger) (T.pack e)
          Just (TaskFailed e _) -> labelWith (alignMid . fontTone Danger) (T.pack (displayException e))
          _ -> pure ()
      case reverse history of
        [] ->
          emptyState
            "Take a snapshot"
            [ "A snapshot pauses the program, walks every closure reachable from its GC roots, and resumes it."
            , "Take another after the program has run for a while: thunks still unevaluated in both are what a lazy accumulator leaves behind."
            ]
            []
        latest : _ -> scope $ do
          survivors <- readVar (hsDbgSurvivors st)
          marked <- readVar (hsDbgMarked st)
          sn <- readVar (hsDbgStored st)
          columnWith (padBottom 8 . tight . fillW) $
            readout
              [ ("closures", fmtInt (M.size (snapNodes latest)))
              , ("thunks", fmtInt (M.size (snapThunkKeys latest)))
              , ("bytes", fmtBytes (fromIntegral (snapBytes latest)))
              , ("roots", fmtInt (length (snapRoots latest)))
              , ("snapshots kept", tshow (length history))
              ]
          tab <- readVar (hsDbgTab st)
          workbench (hsDbgPanes st) $ \_ ctx -> do
            let actions = do
                  paneMeta (dbgTabName tab)
                  scope $ when (snapTruncated latest) $ badge Warning "stopped at the closure limit"
                  flex
                  rowWith (alignMid . tight) $
                    boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabContained} dbgTabName (hsDbgTab st)
            pane ctx (snapLabel latest) actions $ case tab of
              DbgSurvivors -> dbgSurvivors env st history latest survivors marked
              DbgGraph -> dbgGraph env st sn latest marked
              DbgBiggest -> dbgBiggest env st sn latest marked

-- | Select a closure and open the tree down to it.
reveal :: HeapState -> HeapSnapshot -> ClosureId -> NanoUI ()
reveal st snap i = do
  let ids = map cnId (retainerPath snap i)
      keys = drop 1 (scanl (\acc c -> acc <> "/" <> fmtClosureId c) "" ids)
  modifyVar (hsDbgOpen st) (S.union (S.fromList (take (length keys - 1) keys)))
  writeVar (hsDbgSel st) (Just i)
  writeVar (hsDbgTab st) DbgGraph

nodeTone :: M.Map ClosureId Int -> ClosureNode -> Layout -> Layout
nodeTone marked n
  | M.member (cnId n) marked = fontTone Danger
  | cnKind n == NThunk = fontTone Warning
  | otherwise = id

heldBar :: HeapSnapshot -> ClosureId -> NanoUI ()
heldBar snap i = do
  let h = M.findWithDefault 0 i (snapHeld snap)
      share = fromIntegral h / max 1 (fromIntegral (snapBytes snap)) :: Double
  th <- uiTheme
  _ <- canvas (fixedWH 90 6 . alignMid) $ \r@(Rect x y w hh) -> do
    drawRoundedRect r 2 (alpha 0.12 (inkText (ink th)))
    drawRoundedRect (Rect x y (w * realToFrac share) hh) 2 (inkAccent (ink th))
  numCell 90 id (fmtBytes (fromIntegral h))

dbgSurvivors :: Env -> HeapState -> [HeapSnapshot] -> HeapSnapshot -> [Survivor] -> M.Map ClosureId Int -> NanoUI ()
dbgSurvivors env st history latest survivors marked =
  paneScroll (grow . tight . minH 160) $ columnWith (tight . fillW . gap 6) $ do
    scope $
      if
        | length history < 2 -> hint "One snapshot so far. Let the program run, then take another: thunk groups present in consecutive snapshots are listed here, growing ones first."
        | null survivors -> hint "No thunk group survived from the previous snapshot. Thunks are grouped by their code and by the shape of the path that retains them, since objects move between snapshots."
        | otherwise -> hint "Thunks still unevaluated after the program ran on between snapshots, grouped by code and retainer path. A growing group that never gets forced is a lazy accumulator."
    forM_ (take 100 survivors) $ \s -> withKey (tshow (svKey s)) $ panelWith (padAll 8 . gap 3 . fillW) $ do
      let k = svKey s
      rowWith (tight . gap 8 . alignMid . fillW) $ do
        linkText env (RThunkGroup k) fontMedium ("thunk " <> tkName k <> maybe "" ("  " <>) (tkSource k))
        flex
        scope $ when (svGrowing s) $ badge Danger "growing"
        badge Warning ("in " <> tshow (svSurvived s) <> " snapshots")
        scope $ for_ (take 1 (svSample s)) $ \i ->
          whenM (buttonWith tight "Show in graph") (reveal st latest i)
      scope $ unless (null (tkPath k)) $
        labelWith (fontMono . fontMuted . fontSizeScale 0.85 . fillW) ("held via " <> T.intercalate " → " (tkPath k))
      rowWith (tight . gap 16 . wrap . fillW) $ do
        fact "count" (T.intercalate " → " (map fmtInt (svCounts s)))
        fact "bytes now" (fmtBytes (fromIntegral (sum (take 1 (reverse (svBytes s))))))
    separator
    sectionTitle "Thunks in the latest snapshot"
    let groups = take 60 (sortOn (Down . tgBytes . snd) (M.toList (snapThunks latest)))
    scope $ when (null groups) $ hint "No unevaluated thunks reachable."
    forM_ groups $ \(k, g) -> withKey ("g:" <> tshow k) $ do
      picked <- linkRow env (RThunkGroup k) False id $ do
        cell (fillW . (if any (`M.member` marked) (tgSample g) then fontTone Danger else fontTone Warning)) (tkName k)
        cell (fontMuted . fontSizeScale 0.85) (fromMaybe "" (tkSource k))
        cell (fontMono . fixedW 80) (fmtInt (tgCount g))
        cell (fontMono . fixedW 90) (fmtBytes (fromIntegral (tgBytes g)))
      when picked $ for_ (take 1 (tgSample g)) (reveal st latest)

-- | Rows of the expanded tree: path key, depth, closure, whether it can be
-- expanded, whether it is, and whether it closes a cycle.
flattenGraph :: HeapSnapshot -> Set Text -> [ClosureId] -> [(Text, Int, ClosureNode, Bool, Bool, Bool)]
flattenGraph snap openSet = go "" 0 S.empty
  where
    nodes = snapNodes snap
    heldOf i = M.findWithDefault 0 i (snapHeld snap)
    go parent depth onPath ids =
      concat
        [ (path, depth, n, expandable, isOpen, cyclic)
            : (if isOpen && expandable then go path (depth + 1) (S.insert i onPath) (kidsOf n) else [])
        | i <- ids
        , Just n <- [M.lookup i nodes]
        , let path = parent <> "/" <> fmtClosureId i
              cyclic = S.member i onPath
              expandable = not cyclic && any (`M.member` nodes) (cnChildren n)
              isOpen = S.member path openSet
        ]
    kidsOf n = take 200 (sortOn (Down . heldOf) (S.toList (S.fromList (filter (`M.member` nodes) (cnChildren n)))))

dbgGraph :: Env -> HeapState -> Int -> HeapSnapshot -> M.Map ClosureId Int -> NanoUI ()
dbgGraph env st sn snap marked = do
  sel <- readVar (hsDbgSel st)
  openSet <- readVar (hsDbgOpen st)
  let selPath = maybe [] (retainerPath snap) sel
      shown = take 300 (snapRootsByHeld snap)
      roots = shown ++ [cnId r | r <- take 1 selPath, cnId r `notElem` shown]
      rows = take 3000 (flattenGraph snap openSet roots)
  rowWith (grow . tight . gap 10 . fillW . minH 220) $ do
    columnWith (grow . fillH . tight . gap 4) $ do
      hint "GC roots (threads, stable pointers), biggest first. Expand to follow pointers. Held: bytes a closure keeps alive, each closure charged to its first retainer on a shortest path. Thunks are amber, surviving thunks red."
      paneScroll (grow . tight) $ columnWith (tight . fillW . gap 0) $
        forM_ rows $ \(path, depth, n, expandable, isOpen, cyclic) -> withKey path $ do
          picked <- linkRow env (RClosure sn (cnId n)) (sel == Just (cnId n)) id $ do
            spacer (Fixed (fromIntegral (min 40 depth) * 14)) Fit
            cell (fixedW 14 . fontMuted) (if cyclic then "↺" else if expandable then (if isOpen then "▾" else "▸") else " ")
            cell (fillW . nodeTone marked n) (cnName n)
            scope $ for_ (M.lookup (cnId n) marked) $ \k -> badge Danger ("×" <> tshow k)
            cell (fontMuted . fontSizeScale 0.85 . fixedW 120) (cnType n)
            cell (fontMono . fixedW 70) (fmtBytes (fromIntegral (cnSize n)))
            heldBar snap (cnId n)
          when picked $ do
            writeVar (hsDbgSel st) (Just (cnId n))
            when expandable $ writeVar (hsDbgOpen st) (if isOpen then S.delete path openSet else S.insert path openSet)
    panelWith (fixedW 440 . fillH . padAll 10 . gap 6) $ dbgDetail env st sn snap marked sel selPath

dbgDetail :: Env -> HeapState -> Int -> HeapSnapshot -> M.Map ClosureId Int -> Maybe ClosureId -> [ClosureNode] -> NanoUI ()
dbgDetail env st sn snap marked sel selPath = case sel >>= (`M.lookup` snapNodes snap) of
  Nothing -> hint "Select a closure to see what keeps it alive."
  Just n -> paneScroll (grow . tight) $ columnWith (tight . fillW . gap 6) $ do
    labelWith (fontMedium . fillW . nodeTone marked n) (nodeTitle n)
    rowWith (tight . gap 12 . wrap . fillW) $ do
      factMono "address" (fmtClosureId (cnId n))
      fact "type" (cnType n)
      fact "size" (fmtBytes (fromIntegral (cnSize n)))
      fact "held" (fmtBytes (fromIntegral (M.findWithDefault 0 (cnId n) (snapHeld snap))))
    scope $ for_ (M.lookup (cnId n) marked) $ \k ->
      badge Danger ("its thunk group has survived " <> tshow k <> " snapshots")
    sectionTitle "Retained by"
    hint (describePath selPath)
    forM_ (zip [0 :: Int ..] selPath) $ \(d, p) -> withKey ("p:" <> fmtClosureId (cnId p)) $ do
      picked <- linkRow env (RClosure sn (cnId p)) (cnId p == cnId n) id $ do
        spacer (Fixed (fromIntegral (min 12 d) * 8)) Fit
        cell (fixedW 14 . fontMuted) (if d == 0 then "●" else "↳")
        cell (fillW . fontSizeScale 0.9 . nodeTone marked p) (nodeTitle p)
        cell (fontMuted . fontSizeScale 0.8) (cnType p)
      when picked $ reveal st snap (cnId p)
    let kids = [k | i <- take 60 (cnChildren n), Just k <- [M.lookup i (snapNodes snap)]]
    scope $ unless (null kids) $ do
      sectionTitle "Points to"
      forM_ (zip [0 :: Int ..] kids) $ \(j, k) -> withKey ("k:" <> tshow j) $ do
        picked <- linkRow env (RClosure sn (cnId k)) False id $ do
          cell (fillW . fontSizeScale 0.9 . nodeTone marked k) (nodeTitle k)
          cell (fontMono . fontSizeScale 0.85) (fmtBytes (fromIntegral (cnSize k)))
        when picked $ reveal st snap (cnId k)

dbgBiggest :: Env -> HeapState -> Int -> HeapSnapshot -> M.Map ClosureId Int -> NanoUI ()
dbgBiggest env st sn snap marked = readVar (hsDbgFind st) >>= \case
  Just b -> dbgMatching env st sn snap marked b
  Nothing -> dbgBiggestAll env st sn snap marked

-- | The closures a heap band stands for ('closureMatchesBand').
dbgMatching :: Env -> HeapState -> Int -> HeapSnapshot -> M.Map ClosureId Int -> Band -> NanoUI ()
dbgMatching env st sn snap marked b = do
  let held i = M.findWithDefault 0 i (snapHeld snap)
      matches = take 300 (sortOn (Down . held . cnId) [n | n <- M.elems (snapNodes snap), closureMatchesBand b n])
  rowWith (tight . gap 8 . fillW) $ do
    labelWith (alignMid . fillW . fontSizeScale 0.92) ("Closures matching " <> bandTitle b <> (if isJust (bandProv b) then ", by source" else ", by closure type or name"))
    whenM (buttonWith (alignMid . padXY 8 2 . tight) "Show the biggest retainers") (writeVar (hsDbgFind st) Nothing)
  scope $ when (null matches) $ hint "None in the latest snapshot. Bands from a heap profile match by closure type (-hT), constructor (-hd) or source (-hi, with -finfo-table-map on both sides)."
  paneScroll (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
    forM_ matches $ \n -> withKey (fmtClosureId (cnId n)) $ do
      picked <- linkRow env (RClosure sn (cnId n)) False id $ do
        cell (fillW . nodeTone marked n) (nodeTitle n)
        cell (fontMuted . fontSizeScale 0.85 . fixedW 120) (cnType n)
        numCell 70 id (fmtBytes (fromIntegral (cnSize n)))
        heldBar snap (cnId n)
      when picked $ reveal st snap (cnId n)

dbgBiggestAll :: Env -> HeapState -> Int -> HeapSnapshot -> M.Map ClosureId Int -> NanoUI ()
dbgBiggestAll env st sn snap marked = do
  hint "Closures holding the most bytes along shortest retainer paths (threads and roots left out). Select one to see its retainer path."
  rowWith (padXY 8 0 . tight . gap 8 . fillW) $ do
    colHeader fillW "closure"
    colHeader (fixedW 120) "type"
    colHeader (fixedW 70 . alignEnd) "size"
    colHeader (fixedW 188) "held"
  paneScroll (grow . tight . minH 160) $ columnWith (tight . fillW . gap 0) $
    forM_ [n | i <- snapBiggest snap, Just n <- [M.lookup i (snapNodes snap)]] $ \n -> withKey (fmtClosureId (cnId n)) $ do
      picked <- linkRow env (RClosure sn (cnId n)) False id $ do
        cell (fillW . nodeTone marked n) (nodeTitle n)
        cell (fontMuted . fontSizeScale 0.85 . fixedW 120) (cnType n)
        numCell 70 id (fmtBytes (fromIntegral (cnSize n)))
        heldBar snap (cnId n)
      when picked $ reveal st snap (cnId n)

-- ---------------------------------------------------------------------------
-- Lens

-- | What the heap panel can show of the entities other panels name: the
-- bands and censuses of the profile on show (the eventlog's when the live
-- graph is), the cost centres of the @.prof@ file, and the closures, thunk
-- groups and thread roots of the latest snapshot.
heapLens :: HeapState -> HeapLoaded -> Maybe HeapProfile -> NanoUI Lens
heapLens st loaded fromLog = do
  source <- readVar (hsSource st)
  History _ snaps <- readVar (hsDbgHistory st)
  sn <- readVar (hsDbgStored st)
  req <- readVar (hsDbgReq st)
  cursorT <- readVar (hsCursor st)
  totals <- readVar (hsCostTotals st)
  sources <- readVar (hsDbgSources st)
  let profile = case source of
        FromHpFile -> case done (hlHp loaded) of
          Just (Right hp) -> Just hp
          _ -> Nothing
        _ -> fromLog
      fromEventlog = source /= FromHpFile
      prof = case done (hlProf loaded) of
        Just (Right pf) -> Just pf
        _ -> Nothing
      latest = case reverse snaps of
        s : _ -> Just s
        [] -> Nothing
      -- A Unix socket is named after the program and its pid; TCP and
      -- snapshot files say nothing about the run.
      snapRun = case req of
        Just (_, DebugUnix p, _) | Just ri <- runFromSocket (T.pack p) -> runKey ri
        _ -> RunKey "?:?"
      band b = profile >>= \hp -> V.find ((== b) . bandName) (hpBands hp)
      census r i = profile >>= \hp -> if runKeysMatch r (runKey (hpRun hp)) && i >= 0 && i < VU.length (hpTimes hp) then Just hp else Nothing
      instantIn r t = profile >>= \hp ->
        let s = fromIntegral t / 1e9
         in if fromEventlog && runKeysMatch r (runKey (hpRun hp)) && not (VU.null (hpTimes hp)) && s >= VU.head (hpTimes hp) && s <= VU.last (hpTimes hp) then Just hp else Nothing
      ccs = maybe [] (map snd . flattenCC . pfRoots) prof
      costCentre m l = listToMaybe [c | c <- ccs, ccName c == l || ccModule c <> "." <> ccName c == l, T.null m || ccModule c == m]
      closure s a = if s == sn then latest >>= M.lookup a . snapNodes else Nothing
      group k = latest >>= M.lookup k . snapThunks
      tso t = latest >>= \snap ->
        listToMaybe [n | i <- snapRoots snap, Just n <- [M.lookup i (snapNodes snap)], cnType n == "TSO", cnName n == "thread " <> tshow t]
      threadOf n = case T.stripPrefix "thread " (cnName n) of
        Just t | cnType n == "TSO", Right (tid, "") <- TR.decimal t -> Just (RThread snapRun tid)
        _ -> Nothing
      sourceOf = (>>= srcLocSpan . parseSrcLoc)
      cursorIx hp = maybe (VU.maxIndex (hpTotals hp)) (sampleIndexAt hp . (+ hpStart hp)) cursorT
      -- What the code views show: each cost centre's share at its span,
      -- each info-table band's bytes at the cursor, each source's closures
      -- in the latest snapshot.
      profAnns =
        concat
          [ [ (annotationAt sp LaneTime (ctTime t) (costNote t) ref) {anName = name}
            , (annotationAt sp LaneAlloc (ctAlloc t) "" ref) {anName = name}
            ]
          | t <- maybe [] snd totals
          , ctTime t > 0 || ctAlloc t > 0
          , Just sp <- [srcLocSpan (ctLoc t)]
          , let ref = Just (RCostCentre (ctModule t) (ctName t))
                name = Just (ctModule t, nameKey (ctName t))
          ]
      heapAnns = case profile of
        Just hp
          | not (VU.null (hpTimes hp)) ->
              [ Annotation (srcLocSpan (ipSrc ip)) (Just (ipTableName ip)) (Just (ipModule ip, nameKey (ipLabel ip))) LaneHeap v tone (fmtBytesD v <> " live") (Just (RBand (bandName b)))
              | let i = cursorIx hp
              , b <- V.toList (hpBands hp)
              , Just ip <- [bandProv b]
              , let v = bandValues b VU.! i
              , v > 0
              , let tone = if bandKind b == KindThunk then ShadeThunk else ShadeInk
              ]
        _ -> []
      liveAnns =
        [ (annotationAt (sgSpan g) LaneLive (fromIntegral (sgBytes g)) txt (Just (RClosure sn (sgSample g)))) {anShade = tone}
        | g <- sources
        , let tone
                | sgSurvivors g > 0 = ShadeSurvivor
                | sgThunks g > 0 = ShadeThunk
                | otherwise = ShadeInk
              txt =
                fmtInt (sgClosures g) <> " closures"
                  <> (if sgThunks g > 0 then ", " <> fmtInt (sgThunks g) <> " thunks" else "")
                  <> (if sgSurvivors g > 0 then " (" <> fmtInt (sgSurvivors g) <> " surviving)" else "")
                  <> ", " <> fmtBytes (fromIntegral (sgBytes g))
        ]
      costNote t =
        T.intercalate " · " $
          [fmtFixed 1 (ctTime t) <> "% time" | ctTime t >= 0.05]
            ++ [fmtFixed 1 (ctAlloc t) <> "% alloc" | ctAlloc t >= 0.05]
            ++ [fmtInt (ctEntries t) <> "×"]
      ccPath b = do
        hp <- profile
        pf <- prof
        if hpBreakdown hp == "cost centre" then profPath (ccsFrames b) (pfRoots pf) else Nothing
      openPath path = do
        awayFromLiveGraph
        writeVar (hsTab st) CostCentres
        writeVar (hsCCFilter st) ""
        modifyVar (hsCCOpen st) (S.union (S.fromList (map ccNo (dropLast path))))
        writeVar (hsCCSel st) (ccNo <$> listToMaybe (reverse path))
      dropLast xs = take (length xs - 1) xs
      bandActions b bd =
        [ Action "Open its stack in the cost-centre tree" $ case ccPath b of
            Just path -> Right (openPath path)
            Nothing
              | isNothing prof -> Left "open the .prof from the same run"
              | otherwise -> Left "the .prof has no such stack"
        | Just hp <- [profile]
        , hpBreakdown hp == "cost centre"
        ]
          ++ [ Action "Find in live graph" $
                 if
                   | not ghcDebugEnabled -> Left "built without ghc-debug"
                   | isNothing latest -> Left "take a snapshot first"
                   | otherwise -> Right (toLiveGraph >> writeVar (hsDbgTab st) DbgBiggest >> writeVar (hsDbgFind st) (Just bd))
             ]
      awayFromLiveGraph = when (source == FromGhcDebug) (writeVar (hsSource st) FromEventlog)
      toLiveGraph = writeVar (hsSource st) FromGhcDebug
  pure
    (lens PHeap)
      { lensShows = \case
          RBand b -> isJust (band b)
          RCensus r i -> isJust (census r i)
          RInstant r t -> isJust (instantIn r t)
          RCostCentre m l -> isJust (costCentre m l)
          RClosure s a -> isJust (closure s a)
          RThunkGroup k -> isJust (group k)
          RThread r t -> runKeysMatch r snapRun && isJust (tso t)
          _ -> False
      , lensReveal = \case
          RBand b -> awayFromLiveGraph >> writeVar (hsSolo st) (Just b)
          RCensus r i -> for_ (census r i) $ \hp -> awayFromLiveGraph >> writeVar (hsCursor st) (Just (hpOffset hp i))
          RInstant r t -> for_ (instantIn r t) $ \hp -> awayFromLiveGraph >> writeVar (hsCursor st) (Just (fromIntegral t / 1e9 - hpStart hp))
          RCostCentre m l -> for_ (costCentre m l) $ \c -> do
            awayFromLiveGraph
            writeVar (hsTab st) CostCentres
            writeVar (hsCCFilter st) (ccName c)
          RClosure s a -> for_ latest $ \snap -> when (isJust (closure s a)) (toLiveGraph >> reveal st snap a)
          RThunkGroup k -> for_ latest $ \snap -> for_ (group k >>= listToMaybe . tgSample) $ \i -> toLiveGraph >> reveal st snap i
          RThread _ t -> for_ latest $ \snap -> for_ (tso t) $ \n -> toLiveGraph >> reveal st snap (cnId n)
          _ -> pure ()
      , lensTitle = \case
          RBand b | Just bd <- band b, isJust (bandProv bd) -> Just (bandTitle bd)
          RCensus r i -> (\hp -> "Census at " <> fmtSec (hpOffset hp i)) <$> census r i
          RCostCentre m l -> (\c -> ccModule c <> "." <> ccName c) <$> costCentre m l
          RClosure s a -> nodeTitle <$> closure s a
          _ -> Nothing
      , lensRelated = \case
          RBand b -> case band b of
            Just bd ->
              [ (lbl, r)
              | Just ip <- [bandProv bd]
              , (lbl, r) <-
                  [("Go to allocation site", RSource sp) | Just sp <- [srcLocSpan (ipSrc ip)]]
                    ++ [("Go to binding " <> ipLabel ip, RBinding (ipModule ip) (nameKey (ipLabel ip))) | not (T.null (ipLabel ip))]
              ]
                ++ [ ("Go to cost centre " <> f, RCostCentre "" f)
                   | Just hp <- [profile]
                   , hpBreakdown hp == "cost centre"
                   , f : _ <- [ccsFrames b]
                   ]
            Nothing -> []
          RCensus r i -> case census r i of
            Just hp | fromEventlog -> [("Show this moment", RInstant r (round (hpTimes hp VU.! i * 1e9)))]
            _ -> []
          RCostCentre m l -> case costCentre m l of
            Just c ->
              [("Go to source", RSource sp) | Just sp <- [srcLocSpan (parseSrcLoc (ccSrc c))]]
                ++ [("Go to binding " <> ccName c, RBinding (ccModule c) (nameKey (ccName c))) | not (T.null (ccModule c))]
            Nothing -> []
          RClosure s a -> case closure s a of
            Just n -> [("Go to source", RSource sp) | Just sp <- [sourceOf (cnSource n)]] ++ [("Go to thread", r) | Just r <- [threadOf n]]
            Nothing -> []
          RThunkGroup k -> [("Go to source", RSource sp) | Just sp <- [sourceOf (tkSource k)]]
          _ -> []
      , lensActions = \case
          RBand b | Just bd <- band b -> bandActions b bd
          _ -> []
      , lensAnnotations = profAnns ++ heapAnns ++ liveAnns
      , lensSpan = \case
          RBand b -> band b >>= bandProv >>= srcLocSpan . ipSrc
          RCostCentre m l -> costCentre m l >>= srcLocSpan . ccLoc
          RClosure s a -> closure s a >>= sourceOf . cnSource
          RThunkGroup k -> sourceOf (tkSource k)
          _ -> Nothing
      , lensPeek = \case
          RBand b -> (\bd -> bandPeek bd (profile >>= \hp -> Just (hp, cursorIx hp))) <$> band b
          RCensus r i -> (\hp -> fact "live" (fmtBytesD (hpTotals hp VU.! i))) <$> census r i
          RCostCentre m l -> ccPeek <$> costCentre m l
          RClosure s a -> (\n -> closurePeek latest n) <$> closure s a
          RThunkGroup k -> (\g -> fact "in the latest snapshot" (fmtInt (tgCount g) <> " thunks, " <> fmtBytes (fromIntegral (tgBytes g)))) <$> group k
          RThread r t | runKeysMatch r snapRun -> (\n -> closurePeek latest n) <$> tso t
          _ -> Nothing
      , lensHits =
          [ Hit "band" (bandName b) (kindName (bandKind b) <> ", peak " <> fmtBytesD (bandPeak b)) (HitRef (RBand (bandName b)))
          | hp <- maybe [] pure profile
          , b <- V.toList (hpBands hp)
          ]
            ++ [ Hit "cost centre" (ccName c) (ccModule c <> ", " <> fmtFixed 1 (ccInhTime c) <> "% time") (HitRef (RCostCentre (ccModule c) (ccName c)))
               | c <- ccs
               ]
            ++ [ Hit "thunks" (tkName k) (fmtInt (tgCount g) <> " in the latest snapshot") (HitRef (RThunkGroup k))
               | snap <- maybe [] pure latest
               , (k, g) <- M.toList (snapThunks snap)
               ]
            ++ [ Hit "closure" (nodeTitle n) (cnType n <> ", holds " <> fmtBytes (fromIntegral (M.findWithDefault 0 (cnId n) (snapHeld snap)))) (HitRef (RClosure sn (cnId n)))
               | snap <- maybe [] pure latest
               , n <- take 100 [n | i <- snapBiggest snap, Just n <- [M.lookup i (snapNodes snap)]]
               ]
      }

bandPeek :: Band -> Maybe (HeapProfile, Int) -> NanoUI ()
bandPeek b at = do
  fact "kind" (kindName (bandKind b))
  fact "peak" (fmtBytesD (bandPeak b))
  scope $ for_ at $ \(hp, i) -> when (i < VU.length (bandValues b)) $
    fact ("at " <> fmtSec (hpOffset hp i)) (fmtBytesD (bandValues b VU.! i))
  scope $ for_ (bandDetail b) $ labelWith (fontMono . fontMuted . fontSizeScale 0.8 . fillW)
  scope $ when (VU.length (bandValues b) >= 2) $ do
    th <- uiTheme
    _ <- canvas (fixedWH 300 28) $ \r -> sparkBand r (inkText (ink th)) (bandValues b)
    pure ()

ccPeek :: CostCentre -> NanoUI ()
ccPeek c = do
  fact "entries" (fmtInt (ccEntries c))
  fact "time, alloc" (fmtFixed 1 (ccTime c) <> "%, " <> fmtFixed 1 (ccAlloc c) <> "%")
  fact "inherited" (fmtFixed 1 (ccInhTime c) <> "%, " <> fmtFixed 1 (ccInhAlloc c) <> "%")
  scope $ unless (T.null (ccSrc c)) $ fact "source" (ccSrc c)

closurePeek :: Maybe HeapSnapshot -> ClosureNode -> NanoUI ()
closurePeek latest n = do
  fact "type" (cnType n)
  fact "size" (fmtBytes (fromIntegral (cnSize n)))
  scope $ for_ latest $ \snap -> do
    fact "holds" (fmtBytes (fromIntegral (M.findWithDefault 0 (cnId n) (snapHeld snap))))
    labelWith (fontMuted . fontSizeScale 0.85 . fillW) (describePath (retainerPath snap (cnId n)))
