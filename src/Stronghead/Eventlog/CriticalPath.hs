-- | Causal critical-path attribution across Haskell threads, MVars,
-- BlackHoles, STW GC pauses, and runnable queues, plus 2D latency heatmaps.
--
-- Given any thread and time window @[start, end]@ (such as a slow @withSpan@
-- request or a long thread interval), 'analyzeCriticalPath' walks backward
-- from @end@ to @start@: whenever the active thread was blocked on a
-- blackhole or woken by another Haskell thread, causality jumps across the
-- wakeup/owner edge onto the blocking thread so the user sees what *actually*
-- held up the critical path.
module Stronghead.Eventlog.CriticalPath
  ( CriticalKind (..)
  , CriticalSegment (..)
  , CausalEdge (..)
  , CriticalPathResult (..)
  , analyzeCriticalPath
  , analyzeSpanCriticalPath
    -- * 2D latency heatmaps
  , LatencyHeatmap (..)
  , gcPauseHeatmap
  , runnableHeatmap
  , spanLatencyHeatmap
    -- * STM retry & thread contention topology
  , StmContentionReport (..)
  , analyzeStmContention
  , ContentionEdgeKind (..)
  , ThreadContentionEdge (..)
  , ContentionGraph (..)
  , buildContentionGraph
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)

import Stronghead.Eventlog.Model

data CriticalKind
  = CritRunning
    -- ^ Thread actively executing Haskell code on a capability.
  | CritBlockerRunning !Word32 !Text
    -- ^ Another thread (on the causal chain) running while we waited for it.
  | CritGC
    -- ^ Stop-the-world garbage collection pause overlapping the critical path.
  | CritRunnable
    -- ^ Ready to run, waiting in a capability's run queue (scheduler contention).
  | CritExternal !StopCode
    -- ^ Blocked on an external event (FFI CCall, IO read/write, delay, or unknown waker).
  deriving stock (Eq, Show)

data CriticalSegment = CriticalSegment
  { csStart :: !Word64
  , csEnd :: !Word64
  , csThread :: !Word32
  , csKind :: !CriticalKind
  , csDetail :: !Text
  }
  deriving stock (Eq, Show)

-- | A directed causal wakeup or blackhole-ownership link between threads,
-- suitable for drawing curved arrows on the Timeline.
data CausalEdge = CausalEdge
  { ceTime :: !Word64
  , ceFromThread :: !Word32
    -- ^ The blocking / waking thread.
  , ceToThread :: !Word32
    -- ^ The thread that was waiting.
  , ceReason :: !Text
  }
  deriving stock (Eq, Show)

data CriticalPathResult = CriticalPathResult
  { cpStart :: !Word64
  , cpEnd :: !Word64
  , cpTargetThread :: !Word32
  , cpSegments :: ![CriticalSegment]
  , cpCausalEdges :: ![CausalEdge]
  , cpOwnCpuNs :: !Word64
  , cpBlockerCpuNs :: !Word64
  , cpGCNs :: !Word64
  , cpRunnableNs :: !Word64
  , cpExternalNs :: !Word64
  , cpTopBlocker :: !(Maybe (Word32, Word64, Text))
    -- ^ @(blockerTid, totalNsAttributedToBlocker, primaryReason)@.
  }
  deriving stock (Eq, Show)

-- | Maximum span evaluations per 'analyzeCriticalPath' call so pathological
-- multi-thread MVar contention rings over millions of events stay sub-millisecond.
maxCritSteps :: Int
maxCritSteps = 2500

-- | Trace the causal critical path of @targetTid@ across @[a, b]@.
analyzeCriticalPath :: Timeline -> Word32 -> Word64 -> Word64 -> CriticalPathResult
analyzeCriticalPath tl targetTid a b
  | b <= a =
      CriticalPathResult a b targetTid [] [] 0 0 0 0 0 Nothing
  | otherwise =
      let (_, segs, edges) = traceInterval tl maxCritSteps 0 S.empty targetTid targetTid a b
          orderedSegs = sortOn csStart segs
          orderedEdges = sortOn ceTime edges
          ownCpu = sum [dur s | s <- orderedSegs, csKind s == CritRunning]
          blockerCpu = sum [dur s | s <- orderedSegs, CritBlockerRunning {} <- [csKind s]]
          gcNs = sum [dur s | s <- orderedSegs, csKind s == CritGC]
          runnableNs = sum [dur s | s <- orderedSegs, csKind s == CritRunnable]
          extNs = sum [dur s | s <- orderedSegs, CritExternal {} <- [csKind s]]
          blockerTotals =
            M.fromListWith
              (\(d1, r1) (d2, _) -> (d1 + d2, r1))
              [ (tid, (dur s, reason))
              | s <- orderedSegs
              , let mBlocker = case csKind s of
                      CritBlockerRunning tid r -> Just (tid, r)
                      CritExternal code | csThread s /= targetTid -> Just (csThread s, stopName code)
                      _ -> Nothing
              , Just (tid, reason) <- [mBlocker]
              ]
          topBlocker =
            listToMaybe
              [ (tid, ns, reason)
              | (tid, (ns, reason)) <- sortOn (Down . fst . snd) (M.toList blockerTotals)
              ]
       in CriticalPathResult
            { cpStart = a
            , cpEnd = b
            , cpTargetThread = targetTid
            , cpSegments = orderedSegs
            , cpCausalEdges = orderedEdges
            , cpOwnCpuNs = ownCpu
            , cpBlockerCpuNs = blockerCpu
            , cpGCNs = gcNs
            , cpRunnableNs = runnableNs
            , cpExternalNs = extNs
            , cpTopBlocker = topBlocker
            }
  where
    dur s = csEnd s - min (csEnd s) (csStart s)

-- | Trace the causal critical path of a recorded @withSpan@ waterfall span.
analyzeSpanCriticalPath :: Timeline -> UserSpan -> CriticalPathResult
analyzeSpanCriticalPath tl sp = analyzeCriticalPath tl (usThread sp) (usStart sp) (usEnd sp)

traceInterval
  :: Timeline
  -> Int
  -- ^ Remaining span evaluation budget.
  -> Int
  -> S.Set (Word32, Word64, Word64)
  -> Word32
  -- ^ Original root thread being diagnosed.
  -> Word32
  -- ^ Thread currently on the critical path.
  -> Word64
  -> Word64
  -> (Int, [CriticalSegment], [CausalEdge])
traceInterval tl !budget !depth seen rootTid tid a b
  | b <= a || budget <= 0 || depth > 5 || S.member (tid, a, b) seen = (budget, [], [])
  | otherwise = case M.lookup tid (tlThreads tl) of
      Nothing -> (budget - 1, [CriticalSegment a b tid (CritExternal 0) "thread not in timeline"], [])
      Just ti ->
        let tr = tiTrack ti
            i0 = firstSpanEndingAfter tr a
            n = trackLength tr
            -- Stride smoothly when zoomed out across tens of thousands of spans on root thread.
            spanCount = max 1 (firstSpanEndingAfter tr b - i0)
            stride = if depth == 0 && spanCount > 1200 then max 1 (spanCount `div` 1200) else 1
            seen' = S.insert (tid, a, b) seen
            go !bud !i accSegs accEdges
              | bud <= 0 || i >= n || trStart tr VU.! i >= b = (bud, accSegs, accEdges)
              | otherwise =
                  let sp = trackSpan tr i
                      s0 = max a (srStart sp)
                      s1 = min b (srEnd sp)
                   in if s1 <= s0
                        then go bud (i + stride) accSegs accEdges
                        else
                          let (bud', newSegs, newEdges) = classifySpan tl (bud - 1) depth seen' rootTid ti sp s0 s1
                           in go bud' (i + stride) (newSegs ++ accSegs) (newEdges ++ accEdges)
         in go budget i0 [] []

classifySpan
  :: Timeline
  -> Int
  -> Int
  -> S.Set (Word32, Word64, Word64)
  -> Word32
  -> ThreadInfo
  -> SpanRec
  -> Word64
  -> Word64
  -> (Int, [CriticalSegment], [CausalEdge])
classifySpan tl !budget !depth seen rootTid ti sp s0 s1
  | srKind sp == kindRun =
      let k =
            if tiId ti == rootTid
              then CritRunning
              else CritBlockerRunning (tiId ti) ("running on " <> threadName ti)
       in (budget, [CriticalSegment s0 s1 (tiId ti) k (threadName ti)], [])
  | srKind sp == kindRunnable =
      let segs = splitByGC tl (tiId ti) s0 s1 CritRunnable "waiting in runnable queue"
       in (budget, segs, [])
  | srKind sp == kindBlocked =
      let code = srStop sp
          reason = stopName code
          -- Prefer blackhole owner when known, else thread that posted wakeup.
          mBlocker
            | budget <= 0 = Nothing
            | code == 9 && srAux sp /= 0 && srAux sp /= tiId ti =
                Just (srAux sp, s1, "blackhole owned by " <> nameOf (srAux sp))
            | Just (wt, waker) <- wokenBy ti sp
            , waker /= 0
            , waker /= tiId ti
            , code `elem` [4, 7, 8, 9, 13, 16] =
                Just (waker, wt, reason <> " -> woken by " <> nameOf waker)
            | otherwise = Nothing
       in case mBlocker of
            Just (blockerTid, edgeT, desc) ->
              let edge = CausalEdge edgeT blockerTid (tiId ti) desc
                  (bud', subSegs, subEdges) = traceInterval tl budget (depth + 1) seen rootTid blockerTid s0 s1
               in if null subSegs
                    then (bud', [CriticalSegment s0 s1 (tiId ti) (CritExternal code) desc], [edge])
                    else (bud', subSegs, edge : subEdges)
            Nothing ->
              let segs = splitByGC tl (tiId ti) s0 s1 (CritExternal code) reason
               in (budget, segs, [])
  | otherwise = (budget, [], [])
  where
    nameOf t = maybe ("thread " <> T.pack (show t)) threadName (M.lookup t (tlThreads tl))

-- | Split an off-CPU interval around any overlapping stop-the-world GC pauses
-- so GC time is accurately attributed rather than misbilled as runnable wait.
splitByGC :: Timeline -> Word32 -> Word64 -> Word64 -> CriticalKind -> Text -> [CriticalSegment]
splitByGC tl tid a b fallbackKind fallbackDetail = go a i0
  where
    gcs = tlGCs tl
    nGcs = V.length gcs
    i0 = lowerBoundBy gcEnd gcs a
    go !cur !j
      | cur >= b = []
      | j >= nGcs = [CriticalSegment cur b tid fallbackKind fallbackDetail]
      | otherwise =
          let g = gcs V.! j
           in if gcStart g >= b
                then [CriticalSegment cur b tid fallbackKind fallbackDetail]
                else
                  let gs = max cur (gcStart g)
                      ge = min b (gcEnd g)
                      before = [CriticalSegment cur gs tid fallbackKind fallbackDetail | gs > cur]
                      during = [CriticalSegment gs ge tid CritGC ("GC gen " <> T.pack (show (gcGen g))) | ge > gs]
                   in before ++ during ++ go (max cur ge) (j + 1)

-- ---------------------------------------------------------------------------
-- 2D Latency Heatmaps (time bucket x log-spaced duration bucket)

data LatencyHeatmap = LatencyHeatmap
  { hmTitle :: !Text
  , hmStartNs :: !Word64
  , hmEndNs :: !Word64
  , hmTimeBins :: !Int
  , hmValueBins :: !Int
  , hmMinNs :: !Double
  , hmMaxNs :: !Double
  , hmCounts :: !(VU.Vector Int)
    -- ^ Row-major: @tBin * hmValueBins + vBin@.
  , hmMaxCount :: !Int
  , hmSampleCount :: !Int
  }
  deriving stock (Eq, Show)

buildHeatmap :: Text -> Word64 -> Word64 -> Int -> Int -> [(Word64, Double)] -> LatencyHeatmap
buildHeatmap title startNs endNs tBins0 vBins0 pts =
  LatencyHeatmap title startNs endNs tBins vBins loVal hiVal vec maxC (length pts)
  where
    tBins = max 1 tBins0
    vBins = max 1 vBins0
    spanNs = max 1 (endNs - min endNs startNs)
    vals = [max 1 d | (_, d) <- pts]
    loVal = if null vals then 1 else minimum vals
    hiVal = if null vals then 1000 else max (loVal * 2) (maximum vals)
    logLo = log loVal
    logSpan = max 1e-6 (log hiVal - logLo)
    bucketOf (t, d) =
      let tb = min (tBins - 1) (max 0 (floor (fromIntegral (t - min t startNs) / fromIntegral spanNs * fromIntegral tBins :: Double)))
          vb = min (vBins - 1) (max 0 (floor ((log (max 1 d) - logLo) / logSpan * fromIntegral vBins)))
       in tb * vBins + vb
    countsMap = M.fromListWith (+) [(bucketOf p, 1 :: Int) | p <- pts]
    vec = VU.generate (tBins * vBins) (\idx -> M.findWithDefault 0 idx countsMap)
    maxC = if VU.null vec then 0 else VU.maximum vec

-- | 2D time-vs-duration heatmap of stop-the-world GC pauses.
gcPauseHeatmap :: Timeline -> Int -> Int -> LatencyHeatmap
gcPauseHeatmap tl tBins vBins =
  buildHeatmap
    "GC pause latency"
    (tlStart tl)
    (tlEnd tl)
    tBins
    vBins
    [(gcStart g, fromIntegral (gcEnd g - gcStart g)) | g <- V.toList (tlGCs tl), gcEnd g > gcStart g]

-- | 2D time-vs-duration heatmap of thread runnable-queue delays.
runnableHeatmap :: Timeline -> Int -> Int -> LatencyHeatmap
runnableHeatmap tl tBins vBins =
  buildHeatmap
    "Runnable queue delay"
    (tlStart tl)
    (tlEnd tl)
    tBins
    vBins
    [ (trStart tr VU.! i, fromIntegral (trEnd tr VU.! i - trStart tr VU.! i))
    | ti <- M.elems (tlThreads tl)
    , let tr = tiTrack ti
          n = trackLength tr
          stride = max 1 (n `div` 2500)
    , i <- [0, stride .. n - 1]
    , trKind tr VU.! i == kindRunnable
    , trEnd tr VU.! i > trStart tr VU.! i
    ]

-- | 2D time-vs-duration heatmap of recorded @withSpan@ request intervals.
spanLatencyHeatmap :: Timeline -> Int -> Int -> LatencyHeatmap
spanLatencyHeatmap tl tBins vBins =
  buildHeatmap
    "Span latency"
    (tlStart tl)
    (tlEnd tl)
    tBins
    vBins
    [(usStart s, fromIntegral (usEnd s - usStart s)) | s <- V.toList (tlUserSpans tl), usEnd s > usStart s]

-- ---------------------------------------------------------------------------
-- STM Retry Amplification & Thread Contention Topology

data StmContentionReport = StmContentionReport
  { stmBlockCount :: !Int
  , stmTotalWaitNs :: !Word64
  , stmWastedCpuNs :: !Word64
    -- ^ Optimistic CPU burned in short run slices between consecutive @BlockedOnSTM@ stops.
  , stmBurstCount :: !Int
    -- ^ Thundering-herd wakeup bursts (>= 2 STM threads woken within 200 us).
  , stmMaxCohortFanout :: !Int
    -- ^ Largest simultaneous STM wakeup cohort size.
  , stmHotThreads :: ![(Word32, Int, Word64)]
    -- ^ @(tid, stmBlocks, stmWaitNs)@.
  }
  deriving stock (Eq, Show)

-- | Analyze @BlockedOnSTM@ retry behavior, wakeup fanout bursts ("thundering
-- herd"), and optimistic wasted CPU between consecutive STM blocks.
analyzeStmContention :: Timeline -> StmContentionReport
analyzeStmContention tl =
  StmContentionReport
    { stmBlockCount = sum [cnt | (_, cnt, _) <- perThread]
    , stmTotalWaitNs = sum [w | (_, _, w) <- perThread]
    , stmWastedCpuNs = sum wastedSlices
    , stmBurstCount = length bursts
    , stmMaxCohortFanout = maximum (0 : bursts)
    , stmHotThreads = take 12 (sortOn (\(_, _, w) -> Down w) perThread)
    }
  where
    threads = M.elems (tlThreads tl)
    perThread =
      [ (tiId ti, length stmSpans * stride, sum [srEnd s - srStart s | s <- stmSpans] * fromIntegral stride)
      | ti <- threads
      , let tr = tiTrack ti
            n = trackLength tr
            stride = max 1 (n `div` 2500)
            stmSpans =
              [ trackSpan tr i
              | i <- [0, stride .. n - 1]
              , trKind tr VU.! i == kindBlocked
              , trStop tr VU.! i == 13
              ]
      , not (null stmSpans)
      ]
    -- Optimistic retry CPU: a run span immediately preceded by BlockedOnSTM AND
    -- ending again in BlockedOnSTM (< 5ms) likely aborted/retried!
    wastedSlices =
      [ (trEnd tr VU.! i - trStart tr VU.! i) * fromIntegral stride
      | ti <- threads
      , let tr = tiTrack ti
            n = trackLength tr
            stride = max 1 (n `div` 2500)
      , i <- [1, 1 + stride .. n - 1]
      , trKind tr VU.! i == kindRun
      , trStop tr VU.! i == 13
      , trStop tr VU.! (i - 1) == 13
      , trEnd tr VU.! i > trStart tr VU.! i
      , trEnd tr VU.! i - trStart tr VU.! i <= 5000000
      ]
    -- Collect wakeup timestamps ending BlockedOnSTM spans across all threads
    stmWakeTimes =
      sortOn
        id
        [ wt
        | ti <- threads
        , let tr = tiTrack ti
              n = trackLength tr
              stride = max 1 (n `div` 2500)
        , i <- [0, stride .. n - 1]
        , trKind tr VU.! i == kindBlocked && trStop tr VU.! i == 13
        , let sp = trackSpan tr i
        , Just (wt, _) <- [wokenBy ti sp]
        ]
    clusterBursts [] = []
    clusterBursts (t0 : rest) =
      let (same, later) = span (\t -> t - t0 <= 200000) rest -- 200 us burst window
          sz = 1 + length same
       in (if sz >= 2 then [sz] else []) ++ clusterBursts later
    bursts = clusterBursts stmWakeTimes

data ContentionEdgeKind
  = EdgeMVarWakeup
  | EdgeStmWakeup
  | EdgeBlackHole
  | EdgeOtherWakeup
  deriving stock (Eq, Ord, Show)

data ThreadContentionEdge = ThreadContentionEdge
  { tceFrom :: !Word32
    -- ^ Thread that held the lock / blackhole / posted the wakeup.
  , tceTo :: !Word32
    -- ^ Thread that was blocked waiting.
  , tceKind :: !ContentionEdgeKind
  , tceCount :: !Int
  , tceWaitNs :: !Word64
  }
  deriving stock (Eq, Show)

data ContentionGraph = ContentionGraph
  { cgEdges :: ![ThreadContentionEdge]
  , cgBottlenecks :: ![(Word32, Int, Word64)]
    -- ^ Hub blocker threads ranked by total downstream wait time @(tid, wokenCount, totalWaitNs)@.
  }
  deriving stock (Eq, Show)

data EdgeAcc = EdgeAcc !Int !Word64

addEdgeAcc :: EdgeAcc -> EdgeAcc -> EdgeAcc
addEdgeAcc (EdgeAcc c1 w1) (EdgeAcc c2 w2) = EdgeAcc (c1 + c2) (w1 + w2)

-- | Reconstruct the directed thread-to-thread contention & wakeup topology
-- across MVars, STM TVars, and shared BlackHole thunks.
buildContentionGraph :: Timeline -> ContentionGraph
buildContentionGraph tl =
  ContentionGraph
    { cgEdges = sortedEdges
    , cgBottlenecks = take 15 (sortOn (\(_, _, w) -> Down w) [(tid, cnt, w) | (tid, EdgeAcc cnt w) <- M.toList hubs])
    }
  where
    stepThread accMap ti =
      let tr = tiTrack ti
          n = trackLength tr
          stride = max 1 (n `div` 2500)
          wScale = fromIntegral stride :: Word64
          loop !i !mp
            | i >= n = mp
            | trKind tr VU.! i /= kindBlocked = loop (i + stride) mp
            | otherwise =
                let sp = trackSpan tr i
                    mSource
                      | srStop sp == 9 && srAux sp /= 0 && srAux sp /= tiId ti =
                          Just (srAux sp, EdgeBlackHole)
                      | Just (_, waker) <- wokenBy ti sp
                      , waker /= 0 && waker /= tiId ti =
                          let ek = case srStop sp of
                                7 -> EdgeMVarWakeup
                                8 -> EdgeMVarWakeup
                                9 -> EdgeBlackHole
                                13 -> EdgeStmWakeup
                                _ -> EdgeOtherWakeup
                           in Just (waker, ek)
                      | otherwise = Nothing
                 in case mSource of
                      Nothing -> loop (i + stride) mp
                      Just (srcTid, edgeKind) ->
                        let dur = (srEnd sp - min (srEnd sp) (srStart sp)) * wScale
                            mp' = M.insertWith addEdgeAcc (srcTid, tiId ti, edgeKind) (EdgeAcc stride dur) mp
                         in loop (i + stride) mp'
       in loop 0 accMap
    grouped = foldl' stepThread M.empty (M.elems (tlThreads tl))
    sortedEdges =
      take 100 $
        sortOn
          (Down . tceWaitNs)
          [ ThreadContentionEdge s d k cnt w
          | ((s, d, k), EdgeAcc cnt w) <- M.toList grouped
          ]
    hubs =
      foldl'
        (\mp e -> M.insertWith addEdgeAcc (tceFrom e) (EdgeAcc (tceCount e) (tceWaitNs e)) mp)
        M.empty
        sortedEdges
