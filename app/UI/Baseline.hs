-- | Baseline state and diff panes ("Stronghead.Diff").
module UI.Baseline
  ( BaselineState (..)
  , newBaselineState
  , BaselineLoaded (..)
  , baselineLoad
  , markBaseline
  , clearBaseline
  , DiffNow (..)
  , diffPanes
  , baselineStatus
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless, void, when)
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.IORef
import Data.Maybe (isJust, isNothing)
import System.FilePath (takeFileName)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import NanoUI

import Stronghead.Diff
import Stronghead.Eventlog (loadEventlog)
import Stronghead.Eventlog.Model (Timeline (..), summarize)
import Stronghead.Format (fmtBytesD, fmtFixed, tshow)
import Stronghead.HeapProfile (HeapProfile (..), loadHp)
import Stronghead.Join (CostTotal (..), costTotals)
import Stronghead.Prof (ProfFile (..), loadProf)
import Stronghead.Ref
import Stronghead.Source (renderSrcSpan)
import UI.Common
import UI.Link
import UI.Workspace

data BaselineState = BaselineState
  { bvPaths :: !(Var BaselinePaths)
    -- ^ 'noBaselinePaths' when there is no baseline.
  , bvMarked :: !(IORef (Maybe Baseline))
    -- ^ What was loaded when the baseline was marked, for its paths.
  , bvTask :: !(Task BaselinePaths Baseline)
    -- ^ Loads a baseline from its paths, when a session names it.
  , bvBaseDumps :: !(Task FilePath (FilePath, [BindingSummary]))
  , bvCurDumps :: !(Task FilePath (FilePath, [BindingSummary]))
  , bvAllBands :: !(Var Bool)
    -- ^ The heap diff lists unchanged bands too.
  }

newBaselineState :: IO BaselineState
newBaselineState = do
  bvPaths <- newVar noBaselinePaths
  bvMarked <- newIORef Nothing
  bvTask <- newTask
  bvBaseDumps <- newTask
  bvCurDumps <- newTask
  bvAllBands <- newVar False
  pure BaselineState {..}

-- | What this frame has of the baseline and of the current dump folder.
data BaselineLoaded = BaselineLoaded
  { boBaseline :: !(Maybe Baseline)
    -- ^ With its bindings, once they are read.
  , boLoading :: !Bool
  , boCurBindings :: !(Maybe [BindingSummary])
  }

-- | Load the baseline when a session named it, and the bindings of both
-- dump folders. Called every frame.
baselineLoad :: BaselineState -> Maybe FilePath -> NanoUI BaselineLoaded
baselineLoad st curRoot = do
  paths <- readVar (bvPaths st)
  marked <- liftIO (readIORef (bvMarked st))
  base0 <- scope $
    if paths == noBaselinePaths
      then pure Nothing
      else case marked of
        Just b | baPaths b == paths -> pure (Just b)
        _ -> do
          r <- useTask (bvTask st) paths (loadBaseline paths)
          pure (r >>= \b -> if baPaths b == paths then Just b else Nothing)
  baseBinds <- scope $ case (base0 >>= baBindings, blDumps paths) of
    (Just bs, _) -> pure (Just bs)
    (Nothing, Just d) | isJust base0 -> keyed d <$> useTask (bvBaseDumps st) d (bindingsOf d)
    _ -> pure Nothing
  cur <- scope $ case curRoot of
    Just d -> keyed d <$> useTask (bvCurDumps st) d (bindingsOf d)
    Nothing -> pure Nothing
  pure
    BaselineLoaded
      { boBaseline = (\b -> b {baBindings = baseBinds}) <$> base0
      , boLoading = paths /= noBaselinePaths && isNothing base0
      , boCurBindings = cur
      }
  where
    keyed d = \case
      Just (d', bs) | d' == d -> Just bs
      _ -> Nothing
    bindingsOf d = do
      r <- try @SomeException (loadBindingSummaries d >>= \bs -> length bs `seq` pure bs)
      pure (d, fromRight [] r)

-- | A baseline from its artifacts' paths. What can't be read is left out.
loadBaseline :: BaselinePaths -> IO Baseline
loadBaseline p = do
  tl <- attempt (blEventlog p) (fmap eitherToMaybe . loadEventlog)
  hp <- case blHeapProfile p of
    Just f -> attempt (Just f) (fmap eitherToMaybe . loadHp)
    Nothing -> pure (tl >>= tlHeapProfile)
  prof <- attempt (blCostCentres p) (fmap eitherToMaybe . loadProf)
  binds <- attempt (blDumps p) (fmap Just . loadBindingSummaries)
  pure
    Baseline
      { baPaths = p
      , baSummary = summarize <$> tl
      , baHeap = hp
      , baCosts = costTotals . pfRoots <$> prof
      , baBindings = binds
      }
  where
    attempt :: Maybe FilePath -> (FilePath -> IO (Maybe a)) -> IO (Maybe a)
    attempt mf load = case mf of
      Nothing -> pure Nothing
      Just f -> fromRight Nothing <$> try @SomeException (load f >>= evaluate)

-- | Keep what is loaded now as the baseline.
markBaseline :: BaselineState -> Baseline -> NanoUI ()
markBaseline st b = do
  liftIO (writeIORef (bvMarked st) (Just b))
  writeVar (bvPaths st) (baPaths b)

clearBaseline :: BaselineState -> NanoUI ()
clearBaseline st = do
  liftIO (writeIORef (bvMarked st) Nothing)
  writeVar (bvPaths st) noBaselinePaths

-- | What the current run has to compare, and how to mark it.
data DiffNow = DiffNow
  { dnHeap :: !(Maybe HeapProfile)
  , dnCosts :: !(Maybe [CostTotal])
  , dnMark :: Either Text (NanoUI ())
    -- ^ Mark the current run as the baseline, or why there is nothing to
    -- mark.
  }

-- | A name for the baseline: its eventlog, else its first artifact.
baselineName :: BaselinePaths -> Text
baselineName p = case [f | Just f <- [blEventlog p, blHeapProfile p, blCostCentres p, blDumps p]] of
  f : _ -> T.pack (takeFileName f)
  [] -> "the baseline"

-- | The three diff panes.
diffPanes :: Env -> BaselineState -> BaselineLoaded -> DiffNow -> PaneDraw
diffPanes env st loaded now pid ctx
  | pid == paneDiffHeap = Just (diffPane "Heap diff" (heapDiff env st (boBaseline loaded >>= baHeap) (dnHeap now)))
  | pid == paneDiffCosts = Just (diffPane "Cost-centre diff" (costDiff env (boBaseline loaded >>= baCosts) (dnCosts now)))
  | pid == paneDiffBindings = Just (diffPane "Binding diff" (bindingDiff env (boBaseline loaded >>= baBindings) (boCurBindings loaded)))
  | otherwise = Nothing
  where
    diffPane title body = do
      paths <- readVar (bvPaths st)
      let header = do
            scope $
              if paths == noBaselinePaths
                then paneMeta "no baseline"
                else paneMeta ("against " <> baselineName paths <> if boLoading loaded then " (reading…)" else "")
            flex
            markButton
            scope $ unless (paths == noBaselinePaths) $
              whenM (buttonWith (alignMid . padXY 8 2 . tight) "Clear") (clearBaseline st)
          markButton =
            actionButton
              (alignMid . padXY 8 2 . tight)
              (if paths == noBaselinePaths then "Mark as baseline" else "Mark again")
              (Just "Keep what is loaded now as the baseline; what you open next is compared with it")
              (dnMark now)
      paneWith tight ctx title header $
        paneScroll (grow . tight) $
          columnWith (padAll 10 . fillW . tight . gap 4) $
            if paths == noBaselinePaths
              then hint "Mark a run as the baseline (this pane's button, or Go to… › Mark as baseline), then open the next run's eventlog, .hp, .prof or dump folder: it is compared with the baseline here, and the readouts show the change."
              else body

-- | Bands of the two heap profiles side by side.
heapDiff :: Env -> BaselineState -> Maybe HeapProfile -> Maybe HeapProfile -> NanoUI ()
heapDiff env st mbase mcur = case (mbase, mcur) of
  (Nothing, _) -> hint "The baseline has no heap profile. Mark a run with heap samples (+RTS -l -hT) or a .hp file on show."
  (_, Nothing) -> hint "No heap profile is on show. Open the next run's eventlog or .hp file."
  (Just base, Just cur) -> do
    allBands <- readVar (bvAllBands st)
    let changes = diffHeap base cur
        same = hpTotals base == hpTotals cur && hpTimes base == hpTimes cur
        shown = if allBands then changes else filter ((/= BandSame) . bcStatus) changes
        peak hp = if VU.null (hpTotals hp) then 0 else VU.maximum (hpTotals hp)
    scope $ when same $ hint "The current heap profile is the baseline's: open the next run's eventlog or .hp file to compare."
    rowWith (fillW . tight . gap 16 . wrap) $ do
      fact "peak total" (fmtBytesD (peak base) <> " → " <> fmtBytesD (peak cur) <> "  " <> fmtChange (peak base) (peak cur))
      fact "gone" (tshow (length [() | c <- changes, bcStatus c == BandGone]))
      fact "new" (tshow (length [() | c <- changes, bcStatus c == BandNew]))
      fact "smaller" (tshow (length [() | c <- changes, bcStatus c == BandShrank]))
      fact "larger" (tshow (length [() | c <- changes, bcStatus c == BandGrew]))
      flex
      checkboxWith alignMid "Show unchanged" (bvAllBands st)
    rowWith (padXY 8 0 . tight . gap 8 . fillW) $ do
      colHeader fillW "band"
      colHeader (fixedW 90) "matched"
      colHeader (fixedW 90 . alignEnd) "peak before"
      colHeader (fixedW 90 . alignEnd) "peak now"
      colHeader (fixedW 70 . alignEnd) "change"
      colHeader (fixedW 160 . alignEnd) "growth before → now"
    forM_ (zip [0 :: Int ..] (take 300 shown)) $ \(i, c) -> withKey i $
      rowWith (padXY 8 1 . tight . gap 8 . fillW) $ do
        let name = maybe "" bfName (maybe (bcBase c) Just (bcCur c))
            title = shortBand (maybe name figuresTitle (maybe (bcBase c) Just (bcCur c)))
        columnWith (fillW . tight) $ case (bcCur c, bcBase c >>= bandSite) of
          (Just _, _) -> linkText env (RBand name) (fontSizeScale 0.92) (clip 24 title)
          (Nothing, Just sp) -> rowWith (tight . gap 6) $ do
            labelWith (fontSizeScale 0.92) (clip 18 title)
            linkText env (RSource sp) (fontMuted . fontSizeScale 0.85) (renderSrcSpan sp)
          (Nothing, Nothing) -> labelWith (fontSizeScale 0.92) (clip 24 title)
        cell (fixedW 90 . fontMuted . fontSizeScale 0.85) $ case bcMatch c of
          SameName -> "by name"
          SameSource -> "by source"
          Unmatched -> "—"
        numCell 90 fontMuted (maybe "—" (fmtBytesD . bfPeak) (bcBase c))
        numCell 90 id (maybe "—" (fmtBytesD . bfPeak) (bcCur c))
        numCell 70 (statusTone (bcStatus c)) $ case bcStatus c of
          BandGone -> "gone"
          BandNew -> "new"
          _ -> fmtChange (maybe 0 bfPeak (bcBase c)) (maybe 0 bfPeak (bcCur c))
        numCell 160 fontMuted (maybe "—" (fmtBytesD . bfGrowth) (bcBase c) <> " → " <> maybe "—" (fmtBytesD . bfGrowth) (bcCur c))
  where
    statusTone = \case
      BandGone -> fontTone Success
      BandShrank -> fontTone Success
      BandGrew -> fontTone Warning
      BandNew -> fontTone Warning
      BandSame -> fontMuted


-- | Cost centres of the two @.prof@ files.
costDiff :: Env -> Maybe [CostTotal] -> Maybe [CostTotal] -> NanoUI ()
costDiff env mbase mcur = case (mbase, mcur) of
  (Nothing, _) -> hint "The baseline has no .prof. Mark a run with a cost-centre profile open."
  (_, Nothing) -> hint "No .prof is open. Open the next run's cost-centre profile in Heap & leaks."
  (Just base, Just cur) -> do
    let changes = filter moved (diffCosts base cur)
        moved c = any (> 0.05) [abs (d ctInhTime c), abs (d ctInhAlloc c)] || isNothing (cgBase c) || isNothing (cgCur c)
        d f c = maybe 0 f (cgCur c) - maybe 0 f (cgBase c)
    scope $ when (base == cur) $ hint "The current .prof is the baseline's: open the next run's to compare."
    rowWith (padXY 8 0 . tight . gap 8 . fillW) $ do
      colHeader fillW "cost centre"
      colHeader (fixedW 120 . alignEnd) "time before → now"
      colHeader (fixedW 60 . alignEnd) "Δ time"
      colHeader (fixedW 120 . alignEnd) "alloc before → now"
      colHeader (fixedW 60 . alignEnd) "Δ alloc"
    forM_ (zip [0 :: Int ..] (take 300 changes)) $ \(i, c) -> withKey i $
      rowWith (padXY 8 1 . tight . gap 8 . fillW) $ do
        columnWith (fillW . tight) $ linkText env (RCostCentre (cgModule c) (cgName c)) (fontSizeScale 0.92) (clip 48 (cgModule c <> "." <> cgName c))
        numCell 120 fontMuted (pct (cgBase c) ctInhTime <> " → " <> pct (cgCur c) ctInhTime)
        numCell 60 id (signed (d ctInhTime c))
        numCell 120 fontMuted (pct (cgBase c) ctInhAlloc <> " → " <> pct (cgCur c) ctInhAlloc)
        numCell 60 id (signed (d ctInhAlloc c))
  where
    pct m f = maybe "—" (\t -> fmtFixed 1 (f t) <> "%") m
    signed x
      | x > 0 = "+" <> fmtFixed 1 x
      | x < 0 = "−" <> fmtFixed 1 (negate x)
      | otherwise = "0"

-- | Bindings of the two dump folders whose badges or size changed.
bindingDiff :: Env -> Maybe [BindingSummary] -> Maybe [BindingSummary] -> NanoUI ()
bindingDiff env mbase mcur = case (mbase, mcur) of
  (Nothing, _) -> hint "The baseline has no dump folder, or it is still being read. Mark a run with its dumps open in Core & STG."
  (_, Nothing) -> hint "No dump folder is open, or it is still being read. Open the next build's dumps in Core & STG."
  (Just base, Just cur) -> do
    let changes = diffBindings base cur
    scope $ when (null changes) $ hint "No binding's badges or size changed."
    forM_ (zip [0 :: Int ..] (take 500 changes)) $ \(i, c) -> withKey i $
      rowWith (padXY 8 1 . tight . gap 8 . fillW) $ do
        columnWith (fixedW 260 . tight) $ linkText env (RBinding (bgModule c) (bgKey c)) (fontMono . fontSizeScale 0.88) (T.take 40 (bgModule c <> "." <> bgKey c))
        labelWith (fillW . fontSizeScale 0.88) (T.intercalate " · " (bgChanges c))

-- | The status bar's note of the baseline, with a menu to clear it.
baselineStatus :: BaselineState -> BaselineLoaded -> NanoUI ()
baselineStatus st loaded = do
  paths <- readVar (bvPaths st)
  scope $ unless (paths == noBaselinePaths) $ do
    (_, area) <- mouseArea (alignMid . tight) $
      labelWith (alignMid . fontMuted . fontSizeScale 0.85) ("Baseline: " <> baselineName paths <> if boLoading loaded then " (reading…)" else "")
    tooltip area (T.intercalate "\n" ([T.pack f | Just f <- [blEventlog paths, blHeapProfile paths, blCostCentres paths, blDumps paths]] ++ ["Right-click to clear"]))
    void . contextMenu area $ do
      menuHeader "Baseline"
      whenM (menuItem "Clear the baseline") (clearBaseline st)

-- | Text cut to @n@ characters, with an ellipsis when it was longer.
clip :: Int -> Text -> Text
clip n t = if T.length t > n then T.take (n - 1) t <> "…" else t

-- | A closure-type band named @pkg:Module.Con@ by its constructor and
-- module, which fit the column: @Integer.IS@.
shortBand :: Text -> Text
shortBand t = case T.breakOn ":" t of
  (pkg, rest)
    | not (T.null pkg), not (T.null rest), not (T.any (== ' ') t) ->
        let parts = T.splitOn "." (T.drop 1 rest)
         in T.intercalate "." (drop (length parts - 2) parts)
  _ -> t
