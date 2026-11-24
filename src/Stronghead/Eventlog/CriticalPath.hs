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

-- | Trace the causal critical path of @targetTid@ across @[a, b]@.
analyzeCriticalPath :: Timeline -> Word32 -> Word64 -> Word64 -> CriticalPathResult
analyzeCriticalPath tl targetTid a b
  | b <= a =
      CriticalPathResult a b targetTid [] [] 0 0 0 0 0 Nothing
  | otherwise =
      let (segs, edges) = traceInterval tl 0 S.empty targetTid targetTid a b
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
  -> S.Set (Word32, Word64, Word64)
  -> Word32
  -- ^ Original root thread being diagnosed.
  -> Word32
  -- ^ Thread currently on the critical path.
  -> Word64
  -> Word64
  -> ([CriticalSegment], [CausalEdge])
traceInterval tl depth seen rootTid tid a b
  | b <= a || depth > 8 || S.member (tid, a, b) seen = ([], [])
  | otherwise = case M.lookup tid (tlThreads tl) of
      Nothing -> ([CriticalSegment a b tid (CritExternal 0) "thread not in timeline"], [])
      Just ti ->
        let tr = tiTrack ti
            i0 = firstSpanEndingAfter tr a
            n = trackLength tr
            seen' = S.insert (tid, a, b) seen
            go i accSegs accEdges
              | i >= n || trStart tr VU.! i >= b = (accSegs, accEdges)
              | otherwise =
                  let sp = trackSpan tr i
                      s0 = max a (srStart sp)
                      s1 = min b (srEnd sp)
                   in if s1 <= s0
                        then go (i + 1) accSegs accEdges
                        else
                          let (newSegs, newEdges) = classifySpan tl depth seen' rootTid ti sp s0 s1
                           in go (i + 1) (newSegs ++ accSegs) (newEdges ++ accEdges)
         in go i0 [] []

classifySpan
  :: Timeline
  -> Int
  -> S.Set (Word32, Word64, Word64)
  -> Word32
  -> ThreadInfo
  -> SpanRec
  -> Word64
  -> Word64
  -> ([CriticalSegment], [CausalEdge])
classifySpan tl depth seen rootTid ti sp s0 s1
  | srKind sp == kindRun =
      let k =
            if tiId ti == rootTid
              then CritRunning
              else CritBlockerRunning (tiId ti) ("running on " <> threadName ti)
       in ([CriticalSegment s0 s1 (tiId ti) k (threadName ti)], [])
  | srKind sp == kindRunnable =
      let segs = splitByGC tl (tiId ti) s0 s1 CritRunnable "waiting in runnable queue"
       in (segs, [])
  | srKind sp == kindBlocked =
      let code = srStop sp
          reason = stopName code
          -- Prefer blackhole owner when known, else thread that posted wakeup.
          mBlocker
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
                  (subSegs, subEdges) = traceInterval tl (depth + 1) seen rootTid blockerTid s0 s1
               in if null subSegs
                    then ([CriticalSegment s0 s1 (tiId ti) (CritExternal code) desc], [edge])
                    else (subSegs, edge : subEdges)
            Nothing ->
              let segs = splitByGC tl (tiId ti) s0 s1 (CritExternal code) reason
               in (segs, [])
  | otherwise = ([], [])
  where
    nameOf t = maybe ("thread " <> T.pack (show t)) threadName (M.lookup t (tlThreads tl))

-- | Split an off-CPU interval around any overlapping stop-the-world GC pauses
-- so GC time is accurately attributed rather than misbilled as runnable wait.
splitByGC :: Timeline -> Word32 -> Word64 -> Word64 -> CriticalKind -> Text -> [CriticalSegment]
splitByGC tl tid a b fallbackKind fallbackDetail = go a overlapping
  where
    gcs = tlGCs tl
    i0 = lowerBoundBy gcEnd gcs a
    overlapping =
      takeWhile (\g -> gcStart g < b) (V.toList (V.drop i0 gcs))
    go cur []
      | cur < b = [CriticalSegment cur b tid fallbackKind fallbackDetail]
      | otherwise = []
    go cur (g : rest) =
      let gs = max cur (gcStart g)
          ge = min b (gcEnd g)
          before = [CriticalSegment cur gs tid fallbackKind fallbackDetail | gs > cur]
          during = [CriticalSegment gs ge tid CritGC ("GC gen " <> T.pack (show (gcGen g))) | ge > gs]
       in before ++ during ++ go (max cur ge) rest

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
    , i <- [0 .. trackLength tr - 1]
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
