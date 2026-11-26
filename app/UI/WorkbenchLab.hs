-- | Unified Triage feed, Lens Pipe query explorer, Project Target Runner,
-- RTS Pareto scatter lab, and benchmark CSV importer panes.
module UI.WorkbenchLab
  ( WorkbenchLabState (..)
  , newWorkbenchLabState
  , labSyncTargets
  , triagePane
  , triageCardsView
  , queryPane
  , labPane
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.Foldable (for_)
import Data.List (sortOn, (!?))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import NanoUI
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, defaultFileDialogOptions, useFileDialog)

import Stronghead.Diff (Baseline (baHeap))
import Stronghead.Eventlog.Model (Timeline)
import Stronghead.Format (fmtBytes, fmtInt, fmtNs, fmtPct, fmtSec, tshow)
import Stronghead.HeapProfile (HeapProfile)
import Stronghead.Lab
  ( BenchCase (..)
  , BudgetCheck (..)
  , BudgetResult (..)
  , ParetoSummary (..)
  , SweepConfig (..)
  , SweepPoint (..)
  , defaultSweepGrid
  , evaluateBudget
  , formatBudgetMarkdown
  , parseBenchCsv
  , parseBudgetSpec
  , runRtsSweep
  )
import Stronghead.Next (Step (..))
import Stronghead.Query
  ( QueryContext (..)
  , QueryPill (..)
  , QueryResult (..)
  , QueryRow (..)
  , builtinQueries
  , evalQuery
  , formatQueryMarkdown
  , parseQuery
  , queryToPills
  , removePillAt
  , renderQuery
  )
import Stronghead.Ref (Ref (..))
import Stronghead.Runner
  ( CabalTarget (..)
  , CaptureRecipe (..)
  , LaunchSpec (..)
  , allRecipes
  , discoverCabalTargets
  , formatRecipeCommand
  , launchTargetRecipe
  , recipeCaveat
  , recipeSummary
  , recipeTitle
  , targetKindName
  )
import Stronghead.TextFile (readTextFile)
import Stronghead.Triage
  ( Finding (..)
  , Severity (..)
  , categoryName
  , formatTriageMarkdown
  , severityName
  )
import UI.Common
import UI.Link

data WorkbenchLabState = WorkbenchLabState
  { wlQueryText :: !(Var Text)
  , wlTargetsTask :: !(Task FilePath [CabalTarget])
  , wlProjectRoot :: !(Var FilePath)
  , wlSelectedTarget :: !(Var Int)
  , wlRecipe :: !(Var CaptureRecipe)
  , wlTargetArgs :: !(Var Text)
  , wlSweepExe :: !(Var Text)
  , wlSweepArgs :: !(Var Text)
  , wlSweepGrid :: !(Var Text)
  , wlSweepTask :: !(Task SweepConfig (Either Text ParetoSummary))
  , wlSweepTrigger :: !(Var (Maybe SweepConfig))
  , wlBudgetSpec :: !(Var Text)
  , wlBenchCases :: !(Var [BenchCase])
  , wlBenchDialog :: !(Var (Maybe FileDialogId))
  , wlStatus :: !(Var Text)
  }

newWorkbenchLabState :: IO WorkbenchLabState
newWorkbenchLabState = do
  cwd <- getCurrentDirectory
  wlQueryText <- newVar "closures | thunk | sort dom | top 25"
  wlTargetsTask <- newTask
  wlProjectRoot <- newVar cwd
  wlSelectedTarget <- newVar 0
  wlRecipe <- newVar RecipeLeakProvenance
  wlTargetArgs <- newVar ""
  wlSweepExe <- newVar ""
  wlSweepArgs <- newVar ""
  wlSweepGrid <- newVar "-A16m, -A64m, -A128m, -A64m -xn"
  wlSweepTask <- newTask
  wlSweepTrigger <- newVar Nothing
  wlBudgetSpec <- newVar "max-leak-score = 0.55\nmax-gc-cpu-pct = 25\nmax-gc-p99-ms = 20"
  wlBenchCases <- newVar []
  wlBenchDialog <- newVar Nothing
  wlStatus <- newVar ""
  pure WorkbenchLabState {..}

labSyncTargets :: WorkbenchLabState -> Maybe FilePath -> NanoUI [CabalTarget]
labSyncTargets st mRoot = do
  for_ mRoot $ \r -> unless (null r) (writeVar (wlProjectRoot st) r)
  root <- readVar (wlProjectRoot st)
  mTargets <- useTask (wlTargetsTask st) root (discoverCabalTargets root)
  useFileDialog (wlBenchDialog st) $ \paths ->
    for_ (paths !? 0) $ \p -> do
      txt <- liftIO (readTextFile p)
      let parsed = parseBenchCsv txt
      writeVar (wlBenchCases st) parsed
      writeVar (wlStatus st) ("Loaded " <> tshow (length parsed) <> " benchmark row(s) from " <> T.pack p)
  pure (fromMaybe [] mTargets)

-- ---------------------------------------------------------------------------
-- 1. Automated Run-Level Triage Feed

triagePane :: Env -> WorkbenchLabState -> [Finding] -> (Text -> NanoUI ()) -> PaneCtx -> NanoUI ()
triagePane env _st findings onRunQuery ctx = do
  let hdr = do
        paneMeta (fmtInt (length findings) <> " finding" <> (if length findings == 1 then "" else "s"))
        flex
        unless (null findings) $
          copyFeedbackButton (padXY 8 2 . tight) "Copy brief" (formatTriageMarkdown "Current Run" findings)
  paneWith tight ctx "Triage & findings" hdr $
    paneScroll (grow . tight) $
      columnWith (padAll 12 . gap 8 . fillW) $
        if null findings
          then hint "Open an eventlog, heap profile, snapshot, or Core/STG dump directory: Stronghead automatically synthesizes ranked root causes here."
          else triageCardsView env findings onRunQuery

-- | Reusable ranked 'Finding' cards (rendered both in the Triage pane and in
-- the Inspector when no individual entity is focused).
triageCardsView :: Env -> [Finding] -> (Text -> NanoUI ()) -> NanoUI ()
triageCardsView env findings onRunQuery =
  forM_ (zip [0 :: Int ..] findings) $ \(idx, Finding {..}) -> withKey ("fnd" <> tshow idx <> fnId) $
    panelWith (padXY 10 8 . gap 5 . fillW) $ do
      rowWith (fillW . tight . gap 6 . itemsCenter . wrap) $ do
        badge (sevTone fnSeverity) (severityName fnSeverity)
        badge Muted (categoryName fnCategory)
        labelWith (fontMuted . fontSizeScale 0.82) (fmtPct fnConfidence <> " conf")
      labelWith (fontMedium . fontSizeScale 0.96 . fillW) fnTitle
      labelWith (fontSizeScale 0.88 . fillW) fnSummary
      unless (null fnEvidence) $
        rowWith (fillW . tight . gap 12 . wrap) $
          forM_ (take 4 fnEvidence) $ \(k, v) -> withKey k $ fact k v
      hint ("Fix: " <> fnRemediation)
      rowWith (fillW . tight . gap 6 . itemsCenter . wrap) $ do
        for_ fnPrimaryRef $ \r ->
          whenM (iconButtonWith (padXY 8 2 . tight) iconForward "Investigate") (goTo env r)
        for_ fnSuggestedQuery $ \q ->
          whenM (iconButtonWith (padXY 8 2 . tight) iconSearch "Explore query") (onRunQuery q)
        forM_ (take 2 fnSteps) $ \s ->
          case stepRun env s of
            Right act -> whenM (buttonWith (padXY 8 2 . tight) (stepTitle s)) act
            Left _ -> pure ()
  where
    sevTone = \case
      SevCritical -> Danger
      SevHigh -> Warning
      SevMedium -> Accent
      SevInfo -> Muted

-- ---------------------------------------------------------------------------
-- 2. Cross-Artifact Lens Pipe & Pointer-Path Query Explorer

queryPane :: Env -> WorkbenchLabState -> QueryContext -> PaneCtx -> NanoUI ()
queryPane env st qctx ctx = do
  rawQ <- readVar (wlQueryText st)
  let parsed = parseQuery rawQ
      mRes = fmap (evalQuery qctx) (either (const Nothing) Just parsed)
      hdr = do
        for_ mRes $ \res ->
          paneMeta (fmtInt (qresTotalMatched res) <> " matched" <> (if qresTotalBytes res > 0 then " · " <> fmtBytes (qresTotalBytes res) else ""))
        flex
        for_ mRes $ \res -> buttonGroup $ do
          copyFeedbackButton (padXY 8 2 . tight) "Copy MD" (formatQueryMarkdown res)
          copyFeedbackButton (padXY 8 2 . tight) "Copy CI rule" ("forbid-query = " <> qresQueryText res)
  paneWith tight ctx "Lens query" hdr $
    columnWith (fillW . grow . padAll 10 . gap 6) $ do
      -- Preset catalog strip
      rowWith (fillW . tight . gap 5 . itemsCenter . wrap . lineGap 4) $
        forM_ builtinQueries $ \(title, qStr, desc) -> withKey title $ do
          resp <- buttonWith' (padXY 7 2 . tight) title
          tooltip resp (desc <> "\n" <> qStr)
          when (respClicked resp) (writeVar (wlQueryText st) qStr)
      -- Query text bar
      rowWith (fillW . tight . gap 6 . itemsCenter) $ do
        labelWith (fontMono . fontMuted . fontSizeScale 0.88) "query>"
        textInputConfigured
          defaultTextInputConfig
            { ticPlaceholder = "closures | path (MutVar ~> Thunk{3,}) | sort dom | top 25"
            , ticLayout = fillW . fontMono
            }
          (wlQueryText st)
      -- Bidirectional removable stage pills
      case parsed of
        Right q -> do
          let pills = queryToPills q
          unless (null pills) $
            rowWith (fillW . tight . gap 5 . itemsCenter . wrap . lineGap 4) $
              forM_ (zip [0 :: Int ..] pills) $ \(pi', pill) -> withKey ("pill" <> tshow pi') $
                whenM (buttonWith (padXY 6 1 . tight . fontMono . fontSizeScale 0.82) (qpLabel pill <> " ×")) $
                  writeVar (wlQueryText st) (renderQuery (removePillAt pi' q))
        Left err ->
          labelWith (fontTone Danger . fontMono . fontSizeScale 0.82 . fillW) (T.take 160 err)
      -- Results table
      case mRes of
        Nothing -> pure ()
        Just QueryResult {..}
          | null qresRows ->
              hint "0 matches in currently loaded artifacts. Load a heap snapshot/probe for 'closures', a .hp/.eventlog for 'bands'/'threads'/'spans'/'gcs', or Core/STG dumps for 'bindings'."
          | otherwise ->
              paneScroll (grow . tight) $
                columnWith (fillW . tight . gap 2) $
                  forM_ (zip [0 :: Int ..] qresRows) $ \(ri, QueryRow {..}) -> withKey ("qr" <> tshow ri) $
                    case qrRef of
                      Just r -> do
                        _ <- linkRow env r False id $ do
                          badge Muted qrKindBadge
                          labelWith (fontMono . fontSizeScale 0.88 . fillW) (T.take 56 qrTitle)
                          labelWith (fontMono . fontMedium . fontSizeScale 0.86 . alignEnd) qrPrimaryMetric
                        labelWith (padLeft 14 . fontMuted . fontSizeScale 0.8 . fillW) (qrSecondary <> maybe "" (\s -> " · " <> s) qrSource)
                      Nothing ->
                        rowWith (padXY 8 2 . gap 8 . itemsCenter . fillW) $ do
                          badge Muted qrKindBadge
                          labelWith (fontMono . fontSizeScale 0.88 . fillW) qrTitle
                          labelWith (fontMono . fontSizeScale 0.86) qrPrimaryMetric

-- ---------------------------------------------------------------------------
-- 3. Project Target Runner, RTS Pareto Scatter Lab & Benchmark Explorer

labPane
  :: Env
  -> WorkbenchLabState
  -> [CabalTarget]
  -> Maybe Timeline
  -> Maybe HeapProfile
  -> Maybe FilePath
  -> (FilePath -> FilePath -> NanoUI ())
  -- ^ Callback when a target launches: @(eventlogPath, dumpDir)@.
  -> PaneCtx
  -> NanoUI ()
labPane env st targets mTl mHp mDumps onLaunched ctx = do
  statusTxt <- readVar (wlStatus st)
  let hdr = do
        unless (T.null statusTxt) (paneMeta (T.take 64 statusTxt))
        flex
  paneWith tight ctx "Runner & lab" hdr $
    paneScroll (grow . tight) $
      columnWith (padAll 12 . gap 10 . fillW) $ do
        -- Section A: One-Click Project Target Runner
        sectionTitle "Target runner & capture recipes"
        if null targets
          then hint "No .cabal targets found in project root. Set Project Root in Settings or launch Stronghead from your Haskell repository."
          else do
            selIdx0 <- readVar (wlSelectedTarget st)
            let selIdx = max 0 (min (length targets - 1) selIdx0)
            for_ (targets !? selIdx) $ \curTarget -> do
              rowWith (fillW . tight . gap 6 . itemsCenter . wrap . lineGap 4) $
                forM_ (zip [0 :: Int ..] (take 10 targets)) $ \(ti, ct) -> withKey ("tgt" <> tshow ti) $ do
                  let chosen = ti == selIdx
                      lbl = ctName ct <> " (" <> targetKindName (ctKind ct) <> ")"
                  whenM (buttonWith (padXY 8 2 . tight . (if chosen then fontMedium else fontMuted)) lbl) $
                    writeVar (wlSelectedTarget st) ti
              recipe <- readVar (wlRecipe st)
              rowWith (fillW . tight . gap 6 . itemsCenter . wrap . lineGap 4) $
                forM_ allRecipes $ \rc -> withKey (recipeTitle rc) $ do
                  let activeR = rc == recipe
                  resp <- buttonWith' (padXY 8 2 . tight . (if activeR then fontMedium else fontMuted)) (recipeTitle rc)
                  tooltip resp (recipeSummary rc)
                  when (respClicked resp) (writeVar (wlRecipe st) rc)
              hint (recipeSummary recipe)
              for_ (recipeCaveat recipe) $ \cav ->
                labelWith (fontTone Warning . fontSizeScale 0.84 . fillW) cav
              rowWith (fillW . tight . gap 6 . itemsCenter) $ do
                labelWith (fontMuted . fontSizeScale 0.85) "Args:"
                textInputConfigured defaultTextInputConfig {ticPlaceholder = "program arguments before +RTS", ticLayout = fillW} (wlTargetArgs st)
              root <- readVar (wlProjectRoot st)
              argsRaw <- readVar (wlTargetArgs st)
              let evOut = root </> (T.unpack (ctName curTarget) <> ".eventlog")
                  cmdPreview = formatRecipeCommand curTarget recipe (T.words argsRaw) evOut
              rowWith (fillW . tight . gap 6 . itemsCenter . wrap) $ do
                whenM (iconButtonWith (padXY 10 3 . tight) iconPlay "Run & follow live") $ do
                  res <- liftIO (launchTargetRecipe (LaunchSpec root curTarget recipe (T.words argsRaw) evOut))
                  case res of
                    Left err -> writeVar (wlStatus st) err
                    Right (_, bdir) -> do
                      writeVar (wlStatus st) ("Launched " <> ctName curTarget <> " (" <> recipeTitle recipe <> ")")
                      onLaunched evOut bdir
                copyFeedbackButton (padXY 8 3 . tight) "Copy cabal command" cmdPreview
              labelWith (fontMono . fontMuted . fontSizeScale 0.78 . fillW) (T.take 120 cmdPreview)

        separator

        -- Section B: RTS Pareto Frontier Lab
        sectionTitle "RTS flag Pareto tuning lab"
        hint "Sweep nursery sizes, idle GC, and concurrent non-moving GC (+RTS -xn) across a compiled binary to plot the Throughput vs Tail-Latency Pareto frontier."
        rowWith (fillW . tight . gap 6 . itemsCenter) $ do
          textInputConfigured defaultTextInputConfig {ticPlaceholder = "path/to/executable", ticLayout = fillW} (wlSweepExe st)
          textInputConfigured defaultTextInputConfig {ticPlaceholder = "args", ticLayout = fixedW 120} (wlSweepArgs st)
        rowWith (fillW . tight . gap 6 . itemsCenter) $ do
          textInputConfigured defaultTextInputConfig {ticPlaceholder = "-A16m, -A64m, -A64m -xn", ticLayout = fillW . fontMono} (wlSweepGrid st)
          exeTxt <- T.strip <$> readVar (wlSweepExe st)
          sArgs <- readVar (wlSweepArgs st)
          gridTxt <- readVar (wlSweepGrid st)
          root <- readVar (wlProjectRoot st)
          disabledWhen (T.null exeTxt) $
            whenM (iconButtonWith tight iconBolt "Sweep RTS") $ do
              let parsedGrid = filter (not . null) [words (T.unpack (T.strip g)) | g <- T.splitOn "," gridTxt]
                  grid = if null parsedGrid then defaultSweepGrid else parsedGrid
              writeVar (wlSweepTrigger st) (Just (SweepConfig (T.unpack exeTxt) (words (T.unpack sArgs)) grid (Just root)))
        mTrig <- readVar (wlSweepTrigger st)
        mSweepRes <- case mTrig of
          Nothing -> pure Nothing
          Just cfg -> useTask (wlSweepTask st) cfg (runRtsSweep cfg)
        when (isJust mTrig && isNothing mSweepRes) $
          hint "Running RTS parameter sweep across candidate configurations…"
        for_ mSweepRes $ \case
          Left err -> labelWith (fontTone Danger . fontSizeScale 0.85 . fillW) err
          Right ps -> paretoScatterView ps

        separator

        -- Section C: Live .stronghead-spec Budget Gate & Benchmark CSV
        sectionTitle "Performance budget (.stronghead-spec) & benchmarks"
        textInputConfigured defaultTextInputConfig {ticPlaceholder = "max-leak-score = 0.5\nmax-gc-p99-ms = 15", ticLayout = fillW . fontMono} (wlBudgetSpec st)
        specTxt <- readVar (wlBudgetSpec st)
        bRes <- liftIO (evaluateBudget (parseBudgetSpec specTxt) mTl mHp (envBaseline env >>= baHeap) mDumps)
        rowWith (fillW . tight . gap 8 . itemsCenter . wrap) $ do
          badge (if brPassed bRes then Success else Danger) (if brPassed bRes then "BUDGET PASS" else "BUDGET FAIL")
          forM_ (brChecks bRes) $ \ck ->
            labelWith (fontMono . fontSizeScale 0.82 . (if ckPassed ck then fontMuted else fontTone Danger)) (ckName ck <> ": " <> ckActual ck)
          flex
          copyFeedbackButton (padXY 8 2 . tight) "Copy report" (formatBudgetMarkdown bRes)

        rowWith (fillW . tight . gap 8 . itemsCenter) $ do
          whenM (iconButtonWith (padXY 8 2 . tight) iconFolderOpen "Import Criterion / tasty-bench CSV…") $ do
            fid <- askOpenFileDialog defaultFileDialogOptions {dialogFilters = [FileFilter "CSV files" "csv", FileFilter "All files" "*"]}
            writeVar (wlBenchDialog st) (Just fid)
        cases <- readVar (wlBenchCases st)
        unless (null cases) $
          forM_ (zip [0 :: Int ..] (take 25 (sortOn (negate . bcMeanSec) cases))) $ \(bi, bc) -> withKey ("bc" <> tshow bi) $
            void $ linkRow env (RBinding "" (bcBindingKey bc)) False id $ do
              labelWith (fontMono . fontSizeScale 0.86 . fillW) (bcName bc)
              labelWith (fontMono . fontMedium . fontSizeScale 0.86) (fmtNs (bcMeanSec bc * 1e9))
              for_ (bcAllocBytes bc) $ \ab ->
                labelWith (fontMono . fontMuted . fontSizeScale 0.82) (fmtBytes ab)

paretoScatterView :: ParetoSummary -> NanoUI ()
paretoScatterView ParetoSummary {..} = do
  th <- uiTheme
  mm <- monoMetrics 0.82
  let pal = palette th
      pts = psPoints
      durs = [spDurationSec p | p <- pts]
      p99s = [spGCPauseP99Ns p / 1e6 | p <- pts]
      minX = minimum (0 : durs) * 0.9
      maxX = max (minX + 0.01) (maximum (0.01 : durs) * 1.1)
      minY = 0
      maxY = max 1 (maximum (1 : p99s) * 1.15)
  unless (null pts) $ do
    void $
      canvasConfigured defaultCanvasConfig {canvasLayout = fillW . fixedH 150} $ \r@(Rect x y w h) -> do
        drawRoundedRect r 4 (pTrackBg pal)
        let padL = 46
            padR = 16
            padT = 14
            padB = 22
            pw = max 10 (w - padL - padR)
            ph = max 10 (h - padT - padB)
            pxOf d = x + padL + realToFrac ((d - minX) / (maxX - minX)) * pw
            pyOf ms = y + padT + ph - realToFrac ((ms - minY) / (maxY - minY)) * ph
        drawStroke (V2 (x + padL) (y + padT)) (V2 (x + padL) (y + padT + ph)) 1 (pGrid pal)
        drawStroke (V2 (x + padL) (y + padT + ph)) (V2 (x + padL + pw) (y + padT + ph)) 1 (pGrid pal)
        drawTextWith (mmFont mm) (V2 (x + padL + 4) (y + 10)) AlignStart AlignMiddle "p99 GC pause (ms) vs duration (s) — green = Pareto optimal" (pMuted pal)
        forM_ pts $ \p -> do
          let cx = pxOf (spDurationSec p)
              cy = pyOf (spGCPauseP99Ns p / 1e6)
              col = if spParetoOptimal p then pRun pal else pMuted pal
              lbl = T.unwords (spFlags p)
          drawCircle (V2 cx cy) (if spParetoOptimal p then 5 else 3.5) col
          drawTextWith (mmFont mm) (V2 (cx + 7) cy) AlignStart AlignMiddle lbl (if spParetoOptimal p then pText pal else pMuted pal)
    forM_ (sortOn spDurationSec pts) $ \p ->
      rowWith (fillW . tight . gap 8 . itemsCenter . padXY 4 1) $ do
        badge (if spParetoOptimal p then Success else Muted) (if spParetoOptimal p then "FRONTIER" else "dominated")
        labelWith (fontMono . fontSizeScale 0.85 . fillW) ("+RTS " <> T.unwords (spFlags p))
        labelWith (fontMono . fontSizeScale 0.85) (fmtSec (spDurationSec p))
        labelWith (fontMono . fontMuted . fontSizeScale 0.85) ("p99 " <> fmtNs (spGCPauseP99Ns p))
        labelWith (fontMono . fontMuted . fontSizeScale 0.85) (fmtBytes (spPeakHeapBytes p))
