-- | Run-level root-cause synthesis ("Triage Engine").
--
-- Cross-correlates loaded eventlogs, heap profiles, live/probe closure
-- snapshots, Core/STG optimization smells, static STG closure captures, and
-- runtime alerts into a single confidence-ranked list of actionable 'Finding's.
module Stronghead.Triage
  ( Severity (..)
  , severityName
  , FindingCategory (..)
  , categoryName
  , Finding (..)
  , TriageInput (..)
  , emptyTriageInput
  , triageRun
  , formatTriageMarkdown
  , formatTriageJson
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Numeric (showFFloat)

import Stronghead.Debug.Anatomy (HeapSkeleton (..), SpineFinding (..), buildHeapSkeleton, computeDominators)
import Stronghead.Debug.Graph (HeapSnapshot (..), NodeKind (..), Survivor (..), fmtClosureId, survivingThunks)
import Stronghead.Dump (CodeFile, nameKey)
import Stronghead.Dump.Forensics
  ( CoreSmell (..)
  , StgClosureCapture (..)
  , StgUpdateFlag (..)
  , bandStaticCaptures
  , smellKindTitle
  )
import Stronghead.Eventlog.CriticalPath (CriticalPathResult (..), analyzeCriticalPath)
import Stronghead.Eventlog.Model
  ( BiographyBreakdown (..)
  , GCSyncLag (..)
  , RunnableStats (..)
  , Summary (..)
  , ThreadInfo (..)
  , Timeline (..)
  , TraceAlert (..)
  , biographyBreakdown
  , runnableStats
  , stopName
  , summarize
  , threadName
  )
import Stronghead.Format (fmtBytes, fmtBytesD, fmtFixed, fmtInt, fmtNs, fmtPct, tshow)
import Stronghead.HeapProfile (Band (..), BandKind (..), HeapProfile (..), InfoProv (..), Suspect (..), leakSuspects)
import Stronghead.Join (bandTitle)
import Stronghead.Markdown qualified as MD
import Stronghead.Next (Step (..), StepDo (..))
import Stronghead.Ref (Ref (..), RunKey (..), refTarget)
import Stronghead.Source (renderSrcLoc, srcLocSpan)

data Severity
  = SevCritical
  | SevHigh
  | SevMedium
  | SevInfo
  deriving stock (Eq, Ord, Show, Enum, Bounded)

severityName :: Severity -> Text
severityName = \case
  SevCritical -> "CRITICAL"
  SevHigh -> "HIGH"
  SevMedium -> "MEDIUM"
  SevInfo -> "INFO"

data FindingCategory
  = CatMemory
  | CatGC
  | CatConcurrency
  | CatCompiler
  | CatExceptions
  deriving stock (Eq, Ord, Show, Enum, Bounded)

categoryName :: FindingCategory -> Text
categoryName = \case
  CatMemory -> "Memory & Laziness"
  CatGC -> "Garbage Collection"
  CatConcurrency -> "Concurrency & Latency"
  CatCompiler -> "Compiler IR (Core/STG)"
  CatExceptions -> "Exceptions & Guards"

data Finding = Finding
  { fnId :: !Text
  , fnSeverity :: !Severity
  , fnCategory :: !FindingCategory
  , fnConfidence :: !Double
    -- ^ @0.0 .. 1.0@.
  , fnTitle :: !Text
  , fnSummary :: !Text
  , fnEvidence :: ![(Text, Text)]
  , fnRemediation :: !Text
  , fnPrimaryRef :: !(Maybe Ref)
  , fnSuggestedQuery :: !(Maybe Text)
  , fnSteps :: ![Step]
  }
  deriving stock (Eq, Show)

data TriageInput = TriageInput
  { triRunKey :: !RunKey
  , triTimeline :: !(Maybe Timeline)
  , triHeap :: !(Maybe HeapProfile)
  , triSnapshots :: ![HeapSnapshot]
  , triCoreSmells :: ![CoreSmell]
  , triStgFiles :: ![CodeFile]
  }

emptyTriageInput :: TriageInput
emptyTriageInput =
  TriageInput
    { triRunKey = RunKey "?:?"
    , triTimeline = Nothing
    , triHeap = Nothing
    , triSnapshots = []
    , triCoreSmells = []
    , triStgFiles = []
    }

-- | Synthesize and rank top root-cause findings across all loaded artifacts.
triageRun :: TriageInput -> [Finding]
triageRun inp@TriageInput {..} =
  take 12 $
    sortOn (\f -> (fnSeverity f, Down (fnConfidence f))) $
      concat
        [ alertFindings triRunKey triTimeline
        , leakFindings inp effectiveHp
        , snapshotFindings triSnapshots
        , gcFindings triRunKey triTimeline
        , concurrencyFindings triRunKey triTimeline
        , compilerFindings triCoreSmells
        ]
  where
    effectiveHp = case triHeap of
      Just hp -> Just hp
      Nothing -> triTimeline >>= tlHeapProfile

-- ---------------------------------------------------------------------------
-- 1. Exceptions & Allocation Guards

alertFindings :: RunKey -> Maybe Timeline -> [Finding]
alertFindings rk = \case
  Nothing -> []
  Just tl ->
    let alerts = V.toList (tlAlerts tl)
        excs = [a | a <- alerts, taKind a == "exception"]
        spikes = [a | a <- alerts, taKind a == "alloc-spike"]
     in catMaybes
          [ case excs of
              a : _ ->
                Just
                  Finding
                    { fnId = "runtime-exception"
                    , fnSeverity = SevCritical
                    , fnCategory = CatExceptions
                    , fnConfidence = 0.98
                    , fnTitle = "Uncaught exception: " <> taTitle a
                    , fnSummary = fmtInt (length excs) <> " exception(s) captured via withExceptionTap (" <> T.take 120 (taDetail a) <> ")."
                    , fnEvidence = [("First timestamp", fmtNs (fromIntegral (taTime a))), ("Occurrences", fmtInt (length excs))]
                    , fnRemediation = "Inspect the decoded CloneStack frames on the alert and set a breakpoint at the throwing binding in GHCi."
                    , fnPrimaryRef = Just (RInstant rk (taTime a))
                    , fnSuggestedQuery = Nothing
                    , fnSteps = [Step "Jump to exception timestamp" (taTitle a) (StepGo (RInstant rk (taTime a))) True]
                    }
              [] -> Nothing
          , case spikes of
              a : _ ->
                Just
                  Finding
                    { fnId = "alloc-guard-spike"
                    , fnSeverity = SevMedium
                    , fnCategory = CatExceptions
                    , fnConfidence = 0.90
                    , fnTitle = "Allocation tripwire exceeded: " <> taTitle a
                    , fnSummary = fmtInt (length spikes) <> " withAllocGuard budget violation(s) recorded."
                    , fnEvidence = [("Timestamp", fmtNs (fromIntegral (taTime a))), ("Backtrace", T.take 100 (taDetail a))]
                    , fnRemediation = "Inspect the allocating binding in Core & STG to eliminate intermediate list/tuple allocations."
                    , fnPrimaryRef = Just (RInstant rk (taTime a))
                    , fnSuggestedQuery = Just "spans | sort alloc_bytes | top 20"
                    , fnSteps = [Step "Jump to allocation spike" (taTitle a) (StepGo (RInstant rk (taTime a))) True]
                    }
              [] -> Nothing
          ]

-- ---------------------------------------------------------------------------
-- 2. Heap Leak Suspects, Static STG Captures & LDV Biography

leakFindings :: TriageInput -> Maybe HeapProfile -> [Finding]
leakFindings TriageInput {..} = \case
  Nothing -> []
  Just hp ->
    let suspects = [s | s <- take 3 (leakSuspects hp), susScore s >= 0.45]
        susFindings = map (mkLeakFinding hp) suspects
        bioFinding = case biographyBreakdown hp of
          Just bio
            | bioDominant bio `elem` ["lag", "drag", "void"] ->
                let dom = bioDominant bio
                    domBytes = case dom of
                      "lag" -> bioLagBytes bio
                      "drag" -> bioDragBytes bio
                      "void" -> bioVoidBytes bio
                      _ -> 0
                 in [ Finding
                        { fnId = "ldv-biography-" <> dom
                        , fnSeverity = if domBytes > 10 * 1024 * 1024 then SevHigh else SevMedium
                        , fnCategory = CatMemory
                        , fnConfidence = 0.85
                        , fnTitle = "LDV heap biography dominated by '" <> dom <> "' (" <> fmtBytesD domBytes <> " mean)"
                        , fnSummary = case dom of
                            "lag" -> "Closures sit unevaluated long before their first demand."
                            "drag" -> "Closures remain reachable long after their last actual read."
                            _ -> "Void closures are allocated on the heap and never entered before dying."
                        , fnEvidence =
                            [ ("Lag", fmtBytesD (bioLagBytes bio))
                            , ("Drag", fmtBytesD (bioDragBytes bio))
                            , ("Void", fmtBytesD (bioVoidBytes bio))
                            , ("Active Use", fmtBytesD (bioUseBytes bio))
                            ]
                        , fnRemediation = case dom of
                            "lag" -> "Evaluate values strictly at construction ($!, BangPatterns) or push allocation closer to use."
                            "drag" -> "Sever outer record/container references once fields are consumed."
                            _ -> "Make unused record/tuple fields strict or avoid constructing dead wrappers."
                        , fnPrimaryRef = Nothing
                        , fnSuggestedQuery = Just "bands | sort peak | top 15"
                        , fnSteps = []
                        }
                    ]
          _ -> []
     in susFindings ++ bioFinding
  where
    mkLeakFinding hp s =
      let b = hpBands hp V.! susBand s
          bRef = RBand (bandName b)
          isThunk = bandKind b == KindThunk
          sev
            | susScore s >= 0.75 = SevCritical
            | susScore s >= 0.60 = SevHigh
            | otherwise = SevMedium
          caps = bandStaticCaptures triStgFiles b
          capSummary = case caps of
            c : _ | not (null (scCaptured c)) ->
              let flg = if scFlag c == StgUpdateable then "\\u updateable" else "\\s"
               in Just ("[" <> T.intercalate ", " (take 5 (scCaptured c)) <> "] (" <> flg <> " in " <> scBinder c <> ")")
            _ -> Nothing
          provEv = case bandProv b of
            Just ip ->
              [ ("Source", renderSrcLoc (ipSrc ip))
              , ("Binding", ipModule ip <> "." <> ipLabel ip)
              ]
            Nothing -> []
          capEv = maybe [] (\cs -> [("Static STG Captures", cs)]) capSummary
          primaryTarget = case bandProv b >>= srcLocSpan . ipSrc of
            Just sp -> RSource sp
            Nothing -> bRef
          steps =
            [Step "Inspect leaking band" (bandTitle b) (StepGo bRef) True]
              ++ [ Step "Jump to allocation site" (renderSrcLoc (ipSrc ip)) (StepGo (RSource sp)) True
                 | Just ip <- [bandProv b]
                 , Just sp <- [srcLocSpan (ipSrc ip)]
                 ]
              ++ [ Step "Inspect Core & STG binding" (ipModule ip <> "." <> ipLabel ip) (StepGo (RBinding (ipModule ip) (nameKey (ipLabel ip)))) False
                 | Just ip <- [bandProv b]
                 , not (T.null (ipLabel ip))
                 ]
       in Finding
            { fnId = "space-leak-" <> bandName b
            , fnSeverity = sev
            , fnCategory = CatMemory
            , fnConfidence = min 0.99 (susScore s)
            , fnTitle =
                (if isThunk then "Lazy thunk space leak: " else "Monotone heap growth: ")
                  <> bandTitle b
            , fnSummary =
                "Peak residency " <> fmtBytesD (bandPeak b)
                  <> " (" <> fmtFixed 1 (susGrowth s) <> "x growth, "
                  <> fmtPct (susMonotone s) <> " monotone censuses)."
                  <> maybe "" (\cs -> " Captures STG free vars " <> cs <> ".") capSummary
            , fnEvidence =
                [ ("Peak Residency", fmtBytesD (bandPeak b))
                , ("Leak Score", fmtFixed 2 (susScore s))
                , ("Growth", fmtFixed 1 (susGrowth s) <> "x")
                ]
                  ++ provEv
                  ++ capEv
            , fnRemediation =
                if isThunk
                  then "Force accumulator updates strictly (e.g. modifyIORef' / foldl' / BangPatterns) so updateable thunks collapse immediately."
                  else "Bound container growth (evict stale keys from Map/Cache) or check what GC root retains this band."
            , fnPrimaryRef = Just primaryTarget
            , fnSuggestedQuery = Just "bands | suspect | sort score"
            , fnSteps = steps
            }

-- ---------------------------------------------------------------------------
-- 3. Snapshot Spine & Survivor Findings (ghc-debug & ghc-heap Probes)

snapshotFindings :: [HeapSnapshot] -> [Finding]
snapshotFindings [] = []
snapshotFindings snaps =
  case reverse snaps of
    [] -> []
    latest : _ ->
      let snapIdx = length snaps - 1
          dt = computeDominators latest
          skel = buildHeapSkeleton latest dt
          survivors = survivingThunks snaps
          spineF = case [sp | sp <- hsSpines skel, spDepth sp >= 4] of
            sp : _ ->
              [ Finding
                  { fnId = "heap-spine-" <> spName sp
                  , fnSeverity = if spKind sp == NThunk && spDepth sp >= 10 then SevCritical else SevHigh
                  , fnCategory = CatMemory
                  , fnConfidence = 0.92
                  , fnTitle =
                      (if spKind sp == NThunk then "Unforced thunk chain: " else "Deep linked spine: ")
                        <> spName sp <> " (depth " <> fmtInt (spDepth sp) <> ")"
                  , fnSummary =
                      "Exclusively retains " <> fmtBytes (fromIntegral (spRetainedBytes sp))
                        <> maybe "" (\a -> " anchored at " <> a) (spAnchor sp) <> "."
                  , fnEvidence =
                      [ ("Chain Depth", fmtInt (spDepth sp))
                      , ("Dominated Bytes", fmtBytes (fromIntegral (spRetainedBytes sp)))
                      , ("Head Closure", fmtClosureId (spHead sp))
                      ]
                        ++ maybe [] (\a -> [("Anchor", a)]) (spAnchor sp)
                  , fnRemediation = "Run counterfactual what-if severing on the anchor pointer or evaluate updates strictly to prevent linear chain build-up."
                  , fnPrimaryRef = Just (RClosure snapIdx (spHead sp))
                  , fnSuggestedQuery = Just "closures | path (MutVar ~> Thunk{3,}) | sort dom"
                  , fnSteps = [Step "Inspect spine head closure" (fmtClosureId (spHead sp)) (StepGo (RClosure snapIdx (spHead sp))) True]
                  }
              ]
            [] -> []
          survF = case [sv | sv <- survivors, svGrowing sv] of
            sv : _ ->
              [ Finding
                  { fnId = "surviving-thunks"
                  , fnSeverity = SevHigh
                  , fnCategory = CatMemory
                  , fnConfidence = 0.90
                  , fnTitle = "Growing surviving thunk cohort across " <> fmtInt (svSurvived sv) <> " snapshots"
                  , fnSummary = "Thunk group survived " <> fmtInt (svSurvived sv) <> " consecutive snapshots without shrinking."
                  , fnEvidence = [("Snapshots Survived", fmtInt (svSurvived sv)), ("Counts", T.intercalate " -> " (map tshow (svCounts sv)))]
                  , fnRemediation = "Inspect the retainer chain keeping these thunks alive across GC cycles."
                  , fnPrimaryRef = Just (RThunkGroup (svKey sv))
                  , fnSuggestedQuery = Just "closures | surviving | sort dom"
                  , fnSteps = [Step "Open surviving thunk group" "inspect retainers" (StepGo (RThunkGroup (svKey sv))) True]
                  }
              ]
            [] -> []
       in spineF ++ survF

-- ---------------------------------------------------------------------------
-- 4. GC Pressure & STW Sync Straggler Findings

gcFindings :: RunKey -> Maybe Timeline -> [Finding]
gcFindings rk = \case
  Nothing -> []
  Just tl ->
    let sm = summarize tl
        survivalFrac :: Double
        survivalFrac =
          if smAllocated sm > 0
            then fromIntegral (smPeakLive sm) / fromIntegral (smAllocated sm)
            else 0
        gcHigh
          | smGCFrac sm >= 0.25 =
              let nurseryBound = survivalFrac < 0.05
               in [ Finding
                      { fnId = "gc-cpu-overhead"
                      , fnSeverity = if smGCFrac sm >= 0.45 then SevHigh else SevMedium
                      , fnCategory = CatGC
                      , fnConfidence = 0.88
                      , fnTitle =
                          if nurseryBound
                            then "GC nursery thrashing (" <> fmtPct (smGCFrac sm) <> " of CPU in GC)"
                            else "High GC collection overhead (" <> fmtPct (smGCFrac sm) <> " of CPU)"
                      , fnSummary =
                          if nurseryBound
                            then "Allocated " <> fmtBytes (smAllocated sm) <> " with only " <> fmtBytes (smPeakLive sm) <> " peak live (<5% survival): short-lived allocations are overflowing a small nursery."
                            else "Substantial live heap (" <> fmtBytes (smPeakLive sm) <> ") is repeatedly traversed during stop-the-world collections."
                      , fnEvidence =
                          [ ("GC CPU Share", fmtPct (smGCFrac sm))
                          , ("Collections", fmtInt (smGCCount sm) <> " (" <> fmtInt (smMajorGCs sm) <> " major)")
                          , ("Max GC Pause", fmtNs (smGCPauseMax sm))
                          , ("Allocated vs Peak Live", fmtBytes (smAllocated sm) <> " vs " <> fmtBytes (smPeakLive sm))
                          ]
                      , fnRemediation =
                          if nurseryBound
                            then "Increase nursery size with +RTS -A64m (or -A128m) and eliminate hot Core boxing sites."
                            else "Enable GHC's concurrent non-moving collector (+RTS -xn -A64m) for low tail-latency pauses, or run an RTS Pareto sweep."
                      , fnPrimaryRef = Nothing
                      , fnSuggestedQuery = Just "gcs | sort pause | top 20"
                      , fnSteps = [Step "Try +RTS -A64m -RTS" "reduces minor collection frequency" (StepTry "+RTS -A64m -RTS") True]
                      }
                  ]
          | otherwise = []
        worstSync = listToMaybe (sortOn (Down . slSyncWaitNs) [l | l <- V.toList (tlGCSyncLags tl), slSyncWaitNs l >= 2000000])
        syncFinding = case worstSync of
          Nothing -> []
          Just l ->
            [ Finding
                { fnId = "stw-sync-straggler"
                , fnSeverity = if slSyncWaitNs l >= 10000000 then SevHigh else SevMedium
                , fnCategory = CatGC
                , fnConfidence = 0.86
                , fnTitle = "Stop-the-world GC rendezvous delayed " <> fmtNs (fromIntegral (slSyncWaitNs l))
                , fnSummary =
                    "Capability " <> tshow (slLastCap l)
                      <> maybe "" (\tid -> " (running thread " <> tshow tid <> ")") (slStragglerThread l)
                      <> " stalled all other capabilities waiting to start GC #" <> tshow (slCollection l + 1) <> "."
                , fnEvidence =
                    [ ("Sync Stall", fmtNs (fromIntegral (slSyncWaitNs l)))
                    , ("Slowest Capability", tshow (slLastCap l))
                    , ("Straggler Thread", maybe "unknown" tshow (slStragglerThread l))
                    ]
                , fnRemediation = "Check for long-running 'unsafe' FFI calls (mark 'safe' if >100us) or non-allocating tight loops compiled with -fomit-yields."
                , fnPrimaryRef = Just (maybe (RCollection rk (slCollection l)) (RThread rk) (slStragglerThread l))
                , fnSuggestedQuery = Just "gcs | straggler | sort sync_lag"
                , fnSteps =
                    [ Step "Inspect straggler thread" ("delayed GC sync by " <> fmtNs (fromIntegral (slSyncWaitNs l))) (StepGo (RThread rk tid)) True
                    | Just tid <- [slStragglerThread l]
                    ]
                }
            ]
     in gcHigh ++ syncFinding

-- ---------------------------------------------------------------------------
-- 5. Concurrency Contention vs Post-Wakeup Starvation

concurrencyFindings :: RunKey -> Maybe Timeline -> [Finding]
concurrencyFindings rk = \case
  Nothing -> []
  Just tl ->
    let threads = M.elems (tlThreads tl)
        contended =
          sortOn
            (Down . (\(_, bNs, _) -> bNs))
            [ (ti, lockBlocked, primaryCode)
            | ti <- threads
            , let lbl = fromMaybe "" (tiLabel ti)
            , not ("stronghead-" `T.isPrefixOf` lbl)
            , lbl /= "IOManagerThread"
            , let lockBlocked =
                    sum
                      [ ns
                      | (code, ns) <- M.toList (tiBlocked ti)
                      , code `elem` [7, 8, 9, 13] -- MVar, MVarRead, BlackHole, STM
                      ]
                  life = tiRunTime ti + sum (M.elems (tiBlocked ti))
                  primaryCode = fst (fromMaybe (7, 0) (listToMaybe (sortOn (Down . snd) [(c, n) | (c, n) <- M.toList (tiBlocked ti), c `elem` [7, 8, 9, 13]])))
            , life >= 10000000 -- >= 10ms lifespan
            , lockBlocked * 10 >= life * 3 -- >= 30% lock-blocked
            ]
        lockFinding = case contended of
          (worstTi, bNs, code) : rest ->
            let cp = analyzeCriticalPath tl (tiId worstTi) (tlStart tl) (tlEnd tl)
                topB = cpTopBlocker cp
                cohortCnt = 1 + length rest
                mech = stopName code
             in [ Finding
                    { fnId = "lock-contention"
                    , fnSeverity = if cohortCnt >= 3 || bNs >= 500000000 then SevHigh else SevMedium
                    , fnCategory = CatConcurrency
                    , fnConfidence = 0.89
                    , fnTitle = fmtInt cohortCnt <> " thread(s) serialized on " <> mech
                    , fnSummary =
                        threadName worstTi <> " spent " <> fmtNs (fromIntegral bNs) <> " " <> mech
                          <> maybe "" (\(btid, bDur, _) -> "; critical path points to blocker thread " <> tshow btid <> " (" <> fmtNs (fromIntegral bDur) <> ").") topB
                    , fnEvidence =
                        [ ("Worst Contended Thread", threadName worstTi)
                        , ("Blocked Duration", fmtNs (fromIntegral bNs))
                        , ("Contended Cohort", fmtInt cohortCnt <> " threads")
                        ]
                          ++ maybe [] (\(btid, bDur, why) -> [("Primary Blocker", "thread " <> tshow btid <> " (" <> fmtNs (fromIntegral bDur) <> ", " <> why <> ")")]) topB
                    , fnRemediation =
                        if code == 9
                          then "Multiple threads are blocking on a shared unevaluated thunk (BLACKHOLE): evaluate shared data ahead of time or partition work."
                          else "Move pure evaluation outside modifyMVar/atomically critical sections ($! before taking the lock), or shard state across stripes."
                    , fnPrimaryRef = Just (RThread rk (tiId worstTi))
                    , fnSuggestedQuery = Just "threads | blocked >= 10ms | sort blocked"
                    , fnSteps =
                        [Step "Inspect contended thread" (threadName worstTi) (StepGo (RThread rk (tiId worstTi))) True]
                          ++ [Step "Jump to primary blocker" ("thread " <> tshow btid) (StepGo (RThread rk btid)) True | Just (btid, _, _) <- [topB]]
                    }
                ]
          [] -> []
        rns = runnableStats tl
        starveFinding
          | rnP99Ns rns >= 10000000 = -- >= 10ms p99 runnable wait
              [ Finding
                  { fnId = "runnable-starvation"
                  , fnSeverity = SevMedium
                  , fnCategory = CatConcurrency
                  , fnConfidence = 0.82
                  , fnTitle = "Capability run-queue starvation (p99 wait " <> fmtNs (fromIntegral (rnP99Ns rns)) <> ")"
                  , fnSummary = "Woken threads waited up to " <> fmtNs (fromIntegral (rnMaxNs rns)) <> " in capability run queues before getting CPU time."
                  , fnEvidence =
                      [ ("Runnable p50 / p95 / p99", fmtNs (fromIntegral (rnP50Ns rns)) <> " / " <> fmtNs (fromIntegral (rnP95Ns rns)) <> " / " <> fmtNs (fromIntegral (rnP99Ns rns)))
                      , ("Migrations / Steals", fmtInt (rnMigrations rns) <> " / " <> fmtInt (rnSteals rns))
                      ]
                  , fnRemediation = "Match +RTS -N to physical CPU cores, avoid pinning capabilities with long unsafe FFI calls, or bound worker pool concurrency."
                  , fnPrimaryRef = Nothing
                  , fnSuggestedQuery = Just "threads | runnable >= 5ms | sort runnable"
                  , fnSteps = []
                  }
              ]
          | otherwise = []
     in lockFinding ++ starveFinding

-- ---------------------------------------------------------------------------
-- 6. Compiler Optimization Smells

compilerFindings :: [CoreSmell] -> [Finding]
compilerFindings smells =
  [ Finding
      { fnId = "core-smell-" <> csModule sm <> "." <> csBinding sm
      , fnSeverity = if csWeight sm >= 20 then SevHigh else SevMedium
      , fnCategory = CatCompiler
      , fnConfidence = 0.80
      , fnTitle = smellKindTitle (csKind sm) <> " in " <> csModule sm <> "." <> csBinding sm
      , fnSummary = csSummary sm
      , fnEvidence =
          [ ("Binding", csModule sm <> "." <> csBinding sm)
          , ("Dump Line", tshow (csLine sm + 1))
          , ("Hotness Weight", fmtFixed 1 (csWeight sm))
          ]
      , fnRemediation = csRemediation sm
      , fnPrimaryRef = Just (RBinding (csModule sm) (nameKey (csBinding sm)))
      , fnSuggestedQuery = Just "bindings | boxes > 0 | sort time"
      , fnSteps = [Step "Open binding in Core & STG" (csModule sm <> "." <> csBinding sm) (StepGo (RBinding (csModule sm) (nameKey (csBinding sm)))) True]
      }
  | sm <- take 3 smells
  ]

-- ---------------------------------------------------------------------------
-- Markdown & JSON Formatters

formatTriageMarkdown :: Text -> [Finding] -> Text
formatTriageMarkdown label findings = MD.renderMD do
  MD.h1 ("Stronghead Automated Triage Report: " <> label)
  MD.field "Findings Identified" (fmtInt (length findings))
  MD.hr
  if null findings
    then MD.para "No critical runtime, memory, concurrency, or Core optimization pathologies detected."
    else
      mapM_ renderOne (zip [1 :: Int ..] findings)
  where
    renderOne (idx, Finding {..}) = do
      MD.h2 ("#" <> tshow idx <> " [" <> severityName fnSeverity <> "] " <> fnTitle)
      MD.field "Category" (categoryName fnCategory <> " (confidence " <> fmtPct fnConfidence <> ")")
      MD.para fnSummary
      mapM_ (uncurry MD.subfield) fnEvidence
      MD.field "Recommended Fix" fnRemediation
      case fnPrimaryRef of
        Just r -> MD.fieldCode "Focus Ref" (refTarget r)
        Nothing -> pure ()
      case fnSuggestedQuery of
        Just q -> MD.fieldCode "Explore Query" q
        Nothing -> pure ()

formatTriageJson :: [Finding] -> Text
formatTriageJson findings =
  "{\"findings\":[" <> T.intercalate "," (map oneJson findings) <> "]}"
  where
    oneJson Finding {..} =
      T.concat
        [ "{\"id\":\"" <> esc fnId <> "\""
        , ",\"severity\":\"" <> severityName fnSeverity <> "\""
        , ",\"category\":\"" <> esc (categoryName fnCategory) <> "\""
        , ",\"confidence\":" <> T.pack (showFFloat (Just 2) fnConfidence "")
        , ",\"title\":\"" <> esc fnTitle <> "\""
        , ",\"summary\":\"" <> esc fnSummary <> "\""
        , ",\"remediation\":\"" <> esc fnRemediation <> "\""
        , ",\"ref\":" <> maybe "null" (\r -> "\"" <> esc (refTarget r) <> "\"") fnPrimaryRef
        , ",\"query\":" <> maybe "null" (\q -> "\"" <> esc q <> "\"") fnSuggestedQuery
        , "}"
        ]
    esc =
      T.replace "\"" "\\\""
        . T.replace "\n" "\\n"
        . T.replace "\r" ""
        . T.replace "\\" "\\\\"
