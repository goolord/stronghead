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
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Stronghead.Diff (loadBindingSummaries, bsBoxes, bsKey, bsModule)
import Stronghead.Eventlog (loadEventlog)
import Stronghead.Eventlog.Model
import Stronghead.Format (fmtBytes, fmtFixed, fmtNs, fmtPct, fmtSec, readDouble, tshow)
import Stronghead.HeapProfile (HeapProfile (..), leakSuspects, susScore)
import Stronghead.Markdown

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
    r <- try @SomeException (readProcessWithExitCode swExe fullArgs "")
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
  }
  deriving stock (Eq, Show)

emptyBudgetSpec :: BudgetSpec
emptyBudgetSpec = BudgetSpec Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

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
  let allChecks = leakChecks ++ tlChecks ++ deltaChecks ++ boxingChecks
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
