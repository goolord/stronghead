-- | Empirical Experimentation Lab:
--
-- * Automated RTS flag sweep & Pareto frontier analyzer (Throughput vs p99
--   GC pause vs Peak Heap);
-- * Declarative runtime budgets (@.stronghead-spec@) and CI regression gate
--   with Markdown and JUnit XML reporting.
module Stronghead.Lab
  ( -- * RTS Pareto tuning lab
    SweepConfig (..)
  , defaultSweepGrid
  , SweepPoint (..)
  , ParetoSummary (..)
  , computePareto
  , runRtsSweep
  , renderParetoMarkdown
    -- * Declarative runtime budgets & CI gate
  , BudgetSpec (..)
  , emptyBudgetSpec
  , parseBudgetSpec
  , BudgetCheck (..)
  , BudgetResult (..)
  , evaluateBudget
  , formatBudgetMarkdown
  , formatBudgetJUnit
    -- * Statistical multi-trial benchmarking & Criterion / tasty-bench CSV import
  , TrialStats (..)
  , computeTrialStats
  , StatVerdict (..)
  , ComparisonStats (..)
  , compareSamples
  , runStatisticalExperiment
  , BenchCase (..)
  , parseBenchCsv
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, replicateM_)
import Data.Char (isDigit)
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

import Stronghead.Diff (loadBindingSummaries, bsBoxes, bsKey, bsModule)
import Stronghead.Dump (nameKey)
import Stronghead.Eventlog (loadEventlog)
import Stronghead.Eventlog.Model
import Stronghead.Format (fmtBytes, fmtFixed, fmtNs, fmtPct, fmtSec, readDecimal, readDouble, tshow)
import Stronghead.HeapProfile (HeapProfile (..), leakSuspects, susScore)
import Stronghead.Markdown
import Stronghead.Query (QueryContext (..), emptyQueryContext, evalQuery, parseQuery, qresTotalMatched)
import Stronghead.Run (runKey)

-- ---------------------------------------------------------------------------
-- RTS Flag Tuning Lab & Pareto Frontier

data SweepConfig = SweepConfig
  { swExe :: !FilePath
  , swArgs :: ![String]
  , swRtsGrid :: ![[String]]
    -- ^ Candidate RTS flag combinations (without @+RTS@ / @-RTS@).
  , swWorkDir :: !(Maybe FilePath)
  }
  deriving stock (Eq, Show)

-- | A practical default RTS parameter grid sweeping nursery size, non-moving
-- GC, and parallel GC threads.
defaultSweepGrid :: [[String]]
defaultSweepGrid =
  [ ["-A16m"]
  , ["-A64m"]
  , ["-A128m"]
  , ["-A64m", "-I0"]
  , ["-A64m", "-xn"]
  , ["-A128m", "-xn", "-I0"]
  ]

data SweepPoint = SweepPoint
  { spFlags :: ![Text]
  , spDurationSec :: !Double
  , spGCFrac :: !Double
  , spGCPauseP99Ns :: !Double
  , spGCPauseMaxNs :: !Double
  , spPeakLiveBytes :: !Word64
  , spPeakHeapBytes :: !Word64
  , spAllocatedBytes :: !Word64
  , spParetoOptimal :: !Bool
  }
  deriving stock (Eq, Show)

data ParetoSummary = ParetoSummary
  { psPoints :: ![SweepPoint]
  , psFrontier :: ![SweepPoint]
  , psBestThroughput :: !(Maybe SweepPoint)
  , psBestLatency :: !(Maybe SweepPoint)
  , psBestMemory :: !(Maybe SweepPoint)
  }
  deriving stock (Eq, Show)

-- | Mark the non-dominated Pareto frontier over @(duration, p99GCPause,
-- peakHeap)@ (lower is better on all three axes) and select top winners for
-- throughput, tail latency, and memory footprint.
computePareto :: [SweepPoint] -> ParetoSummary
computePareto pts0 =
  let dominated p =
        any
          ( \q ->
              spFlags q /= spFlags p
                && spDurationSec q <= spDurationSec p
                && spGCPauseP99Ns q <= spGCPauseP99Ns p
                && spPeakHeapBytes q <= spPeakHeapBytes p
                && ( spDurationSec q < spDurationSec p
                       || spGCPauseP99Ns q < spGCPauseP99Ns p
                       || spPeakHeapBytes q < spPeakHeapBytes p
                   )
          )
          pts0
      marked = [p {spParetoOptimal = not (dominated p)} | p <- pts0]
      frontier = filter spParetoOptimal marked
      pickBy f xs = case sortOn f xs of
        x : _ -> Just x
        [] -> Nothing
   in ParetoSummary
        { psPoints = marked
        , psFrontier = frontier
        , psBestThroughput = pickBy spDurationSec marked
        , psBestLatency = pickBy (\p -> (spGCPauseP99Ns p, spGCPauseMaxNs p)) marked
        , psBestMemory = pickBy (\p -> (spPeakHeapBytes p, spPeakLiveBytes p)) marked
        }

-- | Execute an RTS sweep against a real compiled Haskell binary, ingesting
-- each run's eventlog and computing the empirical Pareto frontier.
runRtsSweep :: SweepConfig -> IO (Either Text ParetoSummary)
runRtsSweep SweepConfig {..} = do
  tmp <- getTemporaryDirectory
  let sweepDir = tmp </> "stronghead-sweep"
  createDirectoryIfMissing True sweepDir
  results <- forM (zip [0 :: Int ..] swRtsGrid) $ \(idx, rtsFlags) -> do
    let evPath = sweepDir </> ("run-" <> show idx <> ".eventlog")
        fullArgs = swArgs ++ ["+RTS", "-l", "-ol" <> evPath] ++ rtsFlags ++ ["-RTS"]
        cp = (proc swExe fullArgs) {cwd = swWorkDir}
    r <- try @SomeException (readCreateProcessWithExitCode cp "")
    case r of
      Left ex -> pure (Left ("Failed to spawn " <> T.pack swExe <> ": " <> tshow ex))
      Right (ExitFailure code, _, errOut) ->
        pure (Left ("Command exited with code " <> tshow code <> " for " <> T.unwords (map T.pack rtsFlags) <> ": " <> T.pack (take 200 errOut)))
      Right (ExitSuccess, _, _) -> do
        eTl <- loadEventlog evPath
        _ <- try @SomeException (removeFile evPath)
        pure case eTl of
          Left err -> Left ("Failed to parse eventlog: " <> T.pack err)
          Right tl -> Right (pointFromTimeline (map T.pack rtsFlags) tl)
  case sequence results of
    Left err -> pure (Left err)
    Right pts -> pure (Right (computePareto pts))

pointFromTimeline :: [Text] -> Timeline -> SweepPoint
pointFromTimeline flags tl =
  let sm = summarize tl
      pauses = sort [fromIntegral (gcEnd g - gcStart g) :: Double | g <- V.toList (tlGCs tl), gcEnd g >= gcStart g]
      p99 =
        if null pauses
          then 0
          else pauses !! min (length pauses - 1) (max 0 (round (fromIntegral (length pauses - 1) * (0.99 :: Double))))
   in SweepPoint
        { spFlags = flags
        , spDurationSec = smDuration sm
        , spGCFrac = smGCFrac sm
        , spGCPauseP99Ns = p99
        , spGCPauseMaxNs = smGCPauseMax sm
        , spPeakLiveBytes = smPeakLive sm
        , spPeakHeapBytes = smPeakHeap sm
        , spAllocatedBytes = smAllocated sm
        , spParetoOptimal = False
        }

renderParetoMarkdown :: ParetoSummary -> Text
renderParetoMarkdown ps = renderMD do
  h1 "Stronghead RTS Pareto Tuning Lab"
  field "Configurations Evaluated" (tshow (length (psPoints ps)))
  field "Pareto-Optimal Frontier" (tshow (length (psFrontier ps)))
  hr
  tableAligned
    [ (AlignLeft, "RTS Flags")
    , (AlignRight, "Duration")
    , (AlignRight, "GC CPU %")
    , (AlignRight, "GC p99 Pause")
    , (AlignRight, "Peak Heap")
    , (AlignCenter, "Pareto")
    ]
    [ [ code (T.unwords (spFlags p))
      , fmtSec (spDurationSec p)
      , fmtPct (spGCFrac p)
      , fmtNs (spGCPauseP99Ns p)
      , fmtBytes (spPeakHeapBytes p)
      , if spParetoOptimal p then bold "FRONTIER" else "-"
      ]
    | p <- sortOn spDurationSec (psPoints ps)
    ]
  h2 "Recommended Configurations"
  whenJust (psBestThroughput ps) \p ->
    field "Best Throughput" (code ("+RTS " <> T.unwords (spFlags p) <> " -RTS") <> " (" <> fmtSec (spDurationSec p) <> ")")
  whenJust (psBestLatency ps) \p ->
    field "Lowest Tail Latency (p99)" (code ("+RTS " <> T.unwords (spFlags p) <> " -RTS") <> " (" <> fmtNs (spGCPauseP99Ns p) <> ")")
  whenJust (psBestMemory ps) \p ->
    field "Smallest Peak Heap" (code ("+RTS " <> T.unwords (spFlags p) <> " -RTS") <> " (" <> fmtBytes (spPeakHeapBytes p) <> ")")

-- ---------------------------------------------------------------------------
-- Declarative Runtime Budgets & CI Regression Gate (.stronghead-spec)

data BudgetSpec = BudgetSpec
  { bsMaxLeakScore :: !(Maybe Double)
  , bsMaxGCCpuPct :: !(Maybe Double)
  , bsMaxGCP99Ms :: !(Maybe Double)
  , bsMaxMVarBlockMs :: !(Maybe Double)
  , bsMaxSTWSyncMs :: !(Maybe Double)
  , bsMaxLiveBytes :: !(Maybe Word64)
  , bsMaxLiveDeltaPct :: !(Maybe Double)
  , bsForbidBoxing :: ![Text]
    -- ^ Binding names or @Module.binding@ that must have 0 Core boxing sites.
  , bsForbidQueries :: ![Text]
    -- ^ Lens Pipe queries ("Stronghead.Query") that must match 0 entities.
  }
  deriving stock (Eq, Show)

emptyBudgetSpec :: BudgetSpec
emptyBudgetSpec = BudgetSpec Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []

-- | Parse a @.stronghead-spec@ key-value file (lines starting with @#@ are
-- comments).
parseBudgetSpec :: Text -> BudgetSpec
parseBudgetSpec src = foldl' applyLine emptyBudgetSpec (T.lines src)
  where
    applyLine sp raw =
      let cleaned = T.strip (fst (T.breakOn "#" raw))
          (k, rest) = T.breakOn "=" cleaned
          key = T.toLower (T.strip k)
          val = T.strip (T.drop 1 rest)
       in if T.null rest
            then sp
            else case key of
              "max-leak-score" -> sp {bsMaxLeakScore = readDouble val}
              "max-gc-cpu-pct" -> sp {bsMaxGCCpuPct = readDouble val}
              "max-gc-p99-ms" -> sp {bsMaxGCP99Ms = readDouble val}
              "max-mvar-block-ms" -> sp {bsMaxMVarBlockMs = readDouble val}
              "max-stw-sync-ms" -> sp {bsMaxSTWSyncMs = readDouble val}
              "max-live-bytes" -> sp {bsMaxLiveBytes = round <$> readDouble val}
              "max-live-mb" -> sp {bsMaxLiveBytes = (\mb -> round (mb * 1024 * 1024)) <$> readDouble val}
              "max-live-delta-pct" -> sp {bsMaxLiveDeltaPct = readDouble (T.dropWhile (== '+') val)}
              "forbid-boxing" ->
                let items = filter (not . T.null) (map T.strip (T.splitOn "," val))
                 in sp {bsForbidBoxing = bsForbidBoxing sp ++ items}
              "forbid-query" ->
                if T.null val then sp else sp {bsForbidQueries = bsForbidQueries sp ++ [val]}
              _ -> sp

data BudgetCheck = BudgetCheck
  { ckName :: !Text
  , ckPassed :: !Bool
  , ckActual :: !Text
  , ckLimit :: !Text
  }
  deriving stock (Eq, Show)

data BudgetResult = BudgetResult
  { brPassed :: !Bool
  , brChecks :: ![BudgetCheck]
  }
  deriving stock (Eq, Show)

-- | Evaluate a 'BudgetSpec' against optional current/baseline timelines,
-- heap profiles, and Core/STG dump directory.
evaluateBudget
  :: BudgetSpec
  -> Maybe Timeline
  -> Maybe HeapProfile
  -> Maybe HeapProfile
  -> Maybe FilePath
  -> IO BudgetResult
evaluateBudget spec mTl mHp mBaseHp mDumpsDir = do
  let effectiveHp = case mHp of
        Just hp -> Just hp
        Nothing -> mTl >>= tlHeapProfile
      leakChecks = case (bsMaxLeakScore spec, effectiveHp) of
        (Just lim, Just hp) ->
          let topScore = case leakSuspects hp of
                s : _ -> susScore s
                [] -> 0
           in [BudgetCheck "max-leak-score" (topScore <= lim) (fmtFixed 2 topScore) ("<= " <> fmtFixed 2 lim)]
        _ -> []
      tlChecks = case mTl of
        Nothing -> []
        Just tl ->
          let sm = summarize tl
              gcPct = smGCFrac sm * 100
              pauses = sort [fromIntegral (gcEnd g - gcStart g) / 1e6 :: Double | g <- V.toList (tlGCs tl), gcEnd g >= gcStart g]
              p99Ms = if null pauses then 0 else pauses !! min (length pauses - 1) (max 0 (round (fromIntegral (length pauses - 1) * (0.99 :: Double))))
              maxMVarMs =
                maximum $
                  0
                    : [ fromIntegral (M.findWithDefault 0 7 (tiBlocked ti) + M.findWithDefault 0 8 (tiBlocked ti)) / 1e6
                      | ti <- M.elems (tlThreads tl)
                      ]
              maxStwMs =
                maximum $
                  0 : [fromIntegral (slSyncWaitNs l) / 1e6 | l <- V.toList (tlGCSyncLags tl)]
           in concat
                [ [BudgetCheck "max-gc-cpu-pct" (gcPct <= lim) (fmtFixed 1 gcPct <> "%") ("<= " <> fmtFixed 1 lim <> "%") | Just lim <- [bsMaxGCCpuPct spec]]
                , [BudgetCheck "max-gc-p99-ms" (p99Ms <= lim) (fmtFixed 2 p99Ms <> " ms") ("<= " <> fmtFixed 2 lim <> " ms") | Just lim <- [bsMaxGCP99Ms spec]]
                , [BudgetCheck "max-mvar-block-ms" (maxMVarMs <= lim) (fmtFixed 1 maxMVarMs <> " ms") ("<= " <> fmtFixed 1 lim <> " ms") | Just lim <- [bsMaxMVarBlockMs spec]]
                , [BudgetCheck "max-stw-sync-ms" (maxStwMs <= lim) (fmtFixed 2 maxStwMs <> " ms") ("<= " <> fmtFixed 2 lim <> " ms") | Just lim <- [bsMaxSTWSyncMs spec]]
                , [BudgetCheck "max-live-bytes" (smPeakLive sm <= lim) (fmtBytes (smPeakLive sm)) ("<= " <> fmtBytes lim) | Just lim <- [bsMaxLiveBytes spec]]
                ]
      deltaChecks = case (bsMaxLiveDeltaPct spec, mBaseHp, effectiveHp) of
        (Just lim, Just bHp, Just cHp) ->
          let bPeak = if VU.null (hpTotals bHp) then 0 else VU.maximum (hpTotals bHp)
              cPeak = if VU.null (hpTotals cHp) then 0 else VU.maximum (hpTotals cHp)
              deltaPct = if bPeak > 0 then ((cPeak - bPeak) / bPeak) * 100 else 0
           in [BudgetCheck "max-live-delta-pct" (deltaPct <= lim) (fmtFixed 1 deltaPct <> "%") ("<= +" <> fmtFixed 1 lim <> "%")]
        _ -> []
  boxingChecks <- case (bsForbidBoxing spec, mDumpsDir) of
    (targets@(_ : _), Just dir) -> do
      ok <- doesDirectoryExist dir
      if not ok
        then pure []
        else do
          sums <- loadBindingSummaries dir
          pure
            [ let hits =
                    [ s
                    | s <- sums
                    , (bsModule s <> "." <> bsKey s) == t || bsKey s == t
                    , bsBoxes s > 0
                    ]
                  totalBoxes = sum (map bsBoxes hits)
               in BudgetCheck ("forbid-boxing:" <> t) (totalBoxes == 0) (tshow totalBoxes <> " boxing sites") "0"
            | t <- targets
            ]
    _ -> pure []
  let queryCtx =
        emptyQueryContext
          { qcRunKey = maybe (maybe (qcRunKey emptyQueryContext) (runKey . hpRun) effectiveHp) (runKey . tlRun) mTl
          , qcTimeline = mTl
          , qcHeap = effectiveHp
          }
      queryChecks =
        [ case parseQuery qTxt of
            Left err -> BudgetCheck ("forbid-query:" <> qTxt) False ("parse error: " <> T.take 40 err) "0 matches"
            Right q ->
              let n = qresTotalMatched (evalQuery queryCtx q)
               in BudgetCheck ("forbid-query:" <> qTxt) (n == 0) (tshow n <> " matches") "0 matches"
        | qTxt <- bsForbidQueries spec
        ]
      allChecks = leakChecks ++ tlChecks ++ deltaChecks ++ boxingChecks ++ queryChecks
  pure (BudgetResult (all ckPassed allChecks) allChecks)

formatBudgetMarkdown :: BudgetResult -> Text
formatBudgetMarkdown br = renderMD do
  h1 "Stronghead Runtime Budget Check"
  field "Overall Verdict" (if brPassed br then bold "PASS" else bold "FAIL")
  hr
  tableAligned
    [ (AlignLeft, "Assertion")
    , (AlignRight, "Actual")
    , (AlignRight, "Budget")
    , (AlignCenter, "Result")
    ]
    [ [ code (ckName c)
      , ckActual c
      , ckLimit c
      , if ckPassed c then "PASS" else bold "FAIL"
      ]
    | c <- brChecks br
    ]

formatBudgetJUnit :: BudgetResult -> Text
formatBudgetJUnit br =
  let total = length (brChecks br)
      fails = length (filter (not . ckPassed) (brChecks br))
      cases =
        [ if ckPassed c
            then "  <testcase name=\"" <> xmlEsc (ckName c) <> "\" />"
            else
              "  <testcase name=\""
                <> xmlEsc (ckName c)
                <> "\"><failure message=\"actual "
                <> xmlEsc (ckActual c)
                <> " exceeded budget "
                <> xmlEsc (ckLimit c)
                <> "\" /></testcase>"
        | c <- brChecks br
        ]
   in T.unlines $
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<testsuite name=\"stronghead-budget\" tests=\"" <> tshow total <> "\" failures=\"" <> tshow fails <> "\">"
        ]
          ++ cases
          ++ ["</testsuite>"]
  where
    xmlEsc = T.replace "\"" "&quot;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

-- ---------------------------------------------------------------------------
-- Multi-Trial Statistical Benchmarking & Criterion / tasty-bench CSV Import

data TrialStats = TrialStats
  { tsCount :: !Int
  , tsMean :: !Double
  , tsMedian :: !Double
  , tsStdDev :: !Double
  , tsCi95HalfWidth :: !Double
    -- ^ Half-width of two-sided 95% confidence interval (@1.96 * sd / sqrt n@;
    -- uses Student-t critical values for small @n@).
  }
  deriving stock (Eq, Show)

computeTrialStats :: [Double] -> TrialStats
computeTrialStats [] = TrialStats 0 0 0 0 0
computeTrialStats xs =
  let n = length xs
      sorted = sort xs
      mean = sum xs / fromIntegral n
      median
        | odd n = sorted !! (n `div` 2)
        | otherwise = 0.5 * (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2))
      variance
        | n <= 1 = 0
        | otherwise = sum [(x - mean) ^ (2 :: Int) | x <- xs] / fromIntegral (n - 1)
      sd = sqrt (max 0 variance)
      tCrit = case n of
        1 -> 0
        2 -> 12.706
        3 -> 4.303
        4 -> 3.182
        5 -> 2.776
        6 -> 2.571
        7 -> 2.447
        8 -> 2.365
        9 -> 2.306
        10 -> 2.262
        _ -> 1.960
      ci = if n <= 1 then 0 else tCrit * (sd / sqrt (fromIntegral n))
   in TrialStats n mean median sd ci

data StatVerdict
  = StatImproved
  | StatRegressed
  | StatNoise
  deriving stock (Eq, Ord, Show)

data ComparisonStats = ComparisonStats
  { csBaseline :: !TrialStats
  , csCandidate :: !TrialStats
  , csDeltaPct :: !Double
    -- ^ @(candidateMean - baselineMean) / baselineMean@.
  , csWelchT :: !Double
  , csVerdict :: !StatVerdict
  }
  deriving stock (Eq, Show)

-- | Compare baseline vs candidate trial measurements (where lower is better,
-- such as duration, latency, or bytes) using Welch's two-sample t-test at
-- alpha = 0.05 (|t| >= 2.10 for multi-trial runs and |delta| >= 2%).
compareSamples :: [Double] -> [Double] -> ComparisonStats
compareSamples baseXs candXs =
  let bs = computeTrialStats baseXs
      cs = computeTrialStats candXs
      deltaPct = if tsMean bs > 0 then (tsMean cs - tsMean bs) / tsMean bs else 0
      seSq =
        (if tsCount bs > 0 then (tsStdDev bs ^ (2 :: Int)) / fromIntegral (tsCount bs) else 0)
          + (if tsCount cs > 0 then (tsStdDev cs ^ (2 :: Int)) / fromIntegral (tsCount cs) else 0)
      se = sqrt (max 0 seSq)
      welchT
        | se > 1e-12 = (tsMean cs - tsMean bs) / se
        | abs (tsMean cs - tsMean bs) > 1e-12 = if tsMean cs < tsMean bs then -99.0 else 99.0
        | otherwise = 0
      minCrit = if min (tsCount bs) (tsCount cs) <= 3 then 2.78 else 2.10
      verdict
        | tsCount bs < 2 || tsCount cs < 2 =
            if deltaPct <= -0.05 then StatImproved else if deltaPct >= 0.05 then StatRegressed else StatNoise
        | welchT <= negate minCrit && deltaPct <= -0.02 = StatImproved
        | welchT >= minCrit && deltaPct >= 0.02 = StatRegressed
        | otherwise = StatNoise
   in ComparisonStats bs cs deltaPct welchT verdict

-- | Run a controlled multi-trial experiment comparing two executables (or two
-- RTS flag sets on the same executable) with warmup runs, capturing duration,
-- allocations, and peak live residency across @trials@ runs.
runStatisticalExperiment
  :: Int
  -- ^ Warmup runs per variant.
  -> Int
  -- ^ Measured trials per variant (@>= 2@ recommended).
  -> (FilePath, [String], [String])
  -- ^ Baseline @(exe, args, rtsFlags)@.
  -> (FilePath, [String], [String])
  -- ^ Candidate @(exe, args, rtsFlags)@.
  -> Maybe FilePath
  -> IO (Either Text (ComparisonStats, ComparisonStats, ComparisonStats))
  -- ^ @(durationComparison, allocComparison, peakLiveComparison)@.
runStatisticalExperiment warmups trials0 baseSpec candSpec mCwd = do
  let trials = max 1 trials0
  tmp <- getTemporaryDirectory
  let expDir = tmp </> "stronghead-experiment"
  createDirectoryIfMissing True expDir
  let runOne tag (exe, args, rtsFlags) idx record = do
        let evPath = expDir </> (tag <> "-" <> show idx <> ".eventlog")
            fullArgs = args ++ ["+RTS", "-l", "-ol" <> evPath] ++ rtsFlags ++ ["-RTS"]
            cp = (proc exe fullArgs) {cwd = mCwd}
        r <- try @SomeException (readCreateProcessWithExitCode cp "")
        case r of
          Left ex -> pure (Left ("Failed to run " <> T.pack exe <> ": " <> tshow ex))
          Right (ExitFailure code, _, errOut) ->
            pure (Left (T.pack exe <> " exited with code " <> tshow code <> ": " <> T.pack (take 200 errOut)))
          Right (ExitSuccess, _, _) ->
            if not record
              then do
                _ <- try @SomeException (removeFile evPath)
                pure (Right (pointFromTimeline [] emptyTimeline))
              else do
                eTl <- loadEventlog evPath
                _ <- try @SomeException (removeFile evPath)
                pure case eTl of
                  Left err -> Left ("Failed to read eventlog: " <> T.pack err)
                  Right tl -> Right (pointFromTimeline (map T.pack rtsFlags) tl)
  -- Warmup runs
  replicateM_ (max 0 warmups) do
    _ <- runOne "warm-b" baseSpec 0 False
    _ <- runOne "warm-c" candSpec 0 False
    pure ()
  -- Interleaved ABBA/AB trials to cancel thermal/background drift!
  pairs <- forM [1 .. trials] \i -> do
    rb <- runOne "base" baseSpec i True
    rc <- runOne "cand" candSpec i True
    pure ((,) <$> rb <*> rc)
  case sequence pairs of
    Left err -> pure (Left err)
    Right pts -> do
      let bPts = map fst pts
          cPts = map snd pts
          durCmp = compareSamples (map spDurationSec bPts) (map spDurationSec cPts)
          allocCmp = compareSamples (map (fromIntegral . spAllocatedBytes) bPts) (map (fromIntegral . spAllocatedBytes) cPts)
          liveCmp = compareSamples (map (fromIntegral . spPeakLiveBytes) bPts) (map (fromIntegral . spPeakLiveBytes) cPts)
      pure (Right (durCmp, allocCmp, liveCmp))

data BenchCase = BenchCase
  { bcName :: !Text
  , bcBindingKey :: !Text
    -- ^ Normalized last segment for cross-linking to @RBinding@.
  , bcMeanSec :: !Double
  , bcStdDevSec :: !Double
  , bcAllocBytes :: !(Maybe Word64)
  }
  deriving stock (Eq, Show)

-- | Parse Criterion or @tasty-bench@ CSV output (@Name,Mean,MeanLB,MeanUB,Stddev[,Allocated,...]@).
--Note: @tasty-bench@ outputs nanoseconds (integers) when @Mean@ header says @(ps)@ or raw numbers > 100 when @tasty-bench@ format is used, whereas Criterion outputs seconds (floats). We normalize automatically via header inspection.
parseBenchCsv :: Text -> [BenchCase]
parseBenchCsv src =
  case filter (not . T.null . T.strip) (T.lines src) of
    [] -> []
    hdrLine : dataLines ->
      let hdrs = map (T.toLower . unq . T.strip) (splitCsv hdrLine)
          unitMult
            | any ("(ps)" `T.isInfixOf`) hdrs = 1e-12
            | any ("(ns)" `T.isInfixOf`) hdrs = 1e-9
            | any ("allocated" `T.isInfixOf`) hdrs && not (any ("meanlb" `T.isInfixOf`) hdrs) = 1e-9 -- tasty-bench --csv: Name,Mean (ps)/ns,2*Stdev,Allocated,Copied,Peak
            | otherwise = 1.0
          allocIdx = [i | (i, h) <- zip [0 :: Int ..] hdrs, "alloc" `T.isInfixOf` h]
          sdIdx = case [i | (i, h) <- zip [0 :: Int ..] hdrs, "std" `T.isInfixOf` h || "stdev" `T.isInfixOf` h] of
            i : _ -> i
            [] -> 4
       in mapMaybe (parseRow unitMult sdIdx allocIdx) dataLines
  where
    unq = T.dropAround (\c -> c == '"' || c == ' ')
    parseRow mult sdCol allocCols line =
      let cols = map (unq . T.strip) (splitCsv line)
       in case cols of
            nm : meanTxt : _
              | not (T.null nm)
              , Just rawMean <- readDouble meanTxt ->
                  let sdVal = if sdCol < length cols then maybe 0 (* mult) (readDouble (cols !! sdCol)) else 0
                      mAlloc = case allocCols of
                        ai : _ | ai < length cols -> readDecimal (T.filter isDigit (cols !! ai))
                        _ -> Nothing
                      lastSeg = snd (T.breakOnEnd "/" nm)
                      bKey = nameKey (if T.null lastSeg then nm else lastSeg)
                   in Just
                        BenchCase
                          { bcName = nm
                          , bcBindingKey = bKey
                          , bcMeanSec = if any ("(ps)" `T.isInfixOf`) [T.toLower nm] then rawMean * 1e-12 else if mult == 1e-9 && rawMean > 1000 then rawMean * 1e-12 else rawMean * mult
                          , bcStdDevSec = sdVal
                          , bcAllocBytes = mAlloc
                          }
            _ -> Nothing
    splitCsv :: Text -> [Text]
    splitCsv = go "" []
      where
        go cur acc t = case T.uncons t of
          Nothing -> reverse (cur : acc)
          Just (',', rest) | even (countQuotes cur) -> go "" (cur : acc) rest
          Just (c, rest) -> go (T.snoc cur c) acc rest
        countQuotes = T.length . T.filter (== '"')
