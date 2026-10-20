-- | The timeline model built from an eventlog, laid out for fast drawing.
--
-- Every track (a capability, or a Haskell thread) is a run of
-- non-overlapping spans sorted by start time, stored column-wise in unboxed
-- vectors. Alongside the spans each track keeps prefix sums of time spent
-- per span kind, so "how much of pixel column [a, b) was GC?" is two binary
-- searches regardless of how many spans fall inside it. That is what keeps
-- zoomed-out views of multi-gigabyte logs at frame rate.
module Stronghead.Eventlog.Model
  ( -- * Spans
    SpanKind
  , kindRun
  , kindGC
  , kindBlocked
  , kindRunnable
  , nKinds
  , StopCode
  , stopCode
  , stopName
  , stopIsBlocked
  , Track (..)
  , emptyTrack
  , mkTrack
  , trackLength
  , trackSpan
  , SpanRec (..)
  , timeIn
  , spanIndexAt
  , countStartingBy
  , pickSpan
  , lowerBoundBy
  , firstSpanEndingAfter
    -- * Timeline
  , Timeline (..)
  , emptyTimeline
  , CostCentreInfo (..)
  , ThreadInfo (..)
  , GCEvent (..)
  , Marker (..)
  , PointKind
  , pointMigrate
  , pointSteal
  , pointWakeup
  , pointName
  , Series
  , seriesValueAt
  , seriesRateBetween
  , Summary (..)
  , summarize
  , threadName
    -- * Joins
  , wokenBy
  , censusTimes
  , censusAfterCollection
  , collectionBeforeCensus
  , RangeStats (..)
  , rangeStats
    -- * Code samples
  , ProfSample (..)
  , CodeRun (..)
  , mkCodeRuns
  , samplePeriod
  , CodeProfile (..)
  , codeProfile
  , sliceProfile
  , threadProfile
  , costCentreName
  , costCentreNamed
  , isIdleCostCentre
  , fewSamples
    -- * Ticky
  , TickyCounter (..)
  , tickyBetween
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64, Word8)
import GHC.RTS.Events (ThreadStopStatus (..))
import Stronghead.Format (tshow)
import Stronghead.HeapProfile (HeapProfile (..))
import Stronghead.Run (RunInfo, emptyRunInfo)
import Stronghead.Source (SrcLoc)

-- ---------------------------------------------------------------------------
-- Spans

type SpanKind = Word8

kindRun, kindGC, kindBlocked, kindRunnable :: SpanKind
kindRun = 0
kindGC = 1
kindBlocked = 2
kindRunnable = 3

nKinds :: Int
nKinds = 4

-- | Why a thread stopped, compressed to a byte.
type StopCode = Word8

--
-- ghc-events decodes the raw status with a table from GHC 7.8, but modern
-- RTSs post @why_blocked + 6@, whose values have since been renumbered
-- (rts/include/rts/Constants.h). The constructors from raw 16 upward are
-- therefore reinterpreted: what ghc-events calls BlockedOnMsgThrowTo (16)
-- is a blocking C call, ThreadMigrating (17) an interruptible one, and
-- BlockedOnMsgGlobalise (18) is really throwTo.
stopCode :: ThreadStopStatus -> StopCode
stopCode = \case
  NoStatus -> 0
  HeapOverflow -> 1
  StackOverflow -> 2
  ThreadYielding -> 3
  ThreadBlocked -> 4
  ThreadFinished -> 5
  ForeignCall -> 6
  BlockedOnMVar -> 7
  BlockedOnMVarRead -> 8
  BlockedOnBlackHole -> 9
  BlockedOnRead -> 10
  BlockedOnWrite -> 11
  BlockedOnDelay -> 12
  BlockedOnSTM -> 13
  BlockedOnDoProc -> 14
  BlockedOnCCall -> 15
  BlockedOnCCall_NoUnblockExc -> 15
  BlockedOnMsgThrowTo -> 15
  ThreadMigrating -> 15
  BlockedOnMsgGlobalise -> 16
  BlockedOnBlackHoleOwnedBy _ -> 9

stopName :: StopCode -> Text
stopName = \case
  0 -> "still running"
  1 -> "heap overflow (needs GC)"
  2 -> "stack overflow"
  3 -> "yielded"
  4 -> "blocked"
  5 -> "finished"
  6 -> "foreign call"
  7 -> "blocked on MVar"
  8 -> "blocked on MVar read"
  9 -> "blocked on black hole"
  10 -> "blocked on read"
  11 -> "blocked on write"
  12 -> "blocked on delay"
  13 -> "blocked on STM"
  14 -> "blocked on process"
  15 -> "blocked in C call"
  16 -> "blocked on throwTo"
  17 -> "migrating"
  c -> "status " <> tshow c

-- | Whether the thread was waiting on something after this stop, as opposed
-- to being runnable (yield, heap check, migration).
stopIsBlocked :: StopCode -> Bool
stopIsBlocked c = c == 4 || c == 6 || (c >= 7 && c <= 16)

-- | Spans of one track, column-wise. @trCum@ holds, per kind, the running
-- total of that kind's duration through span @i@ inclusive.
data Track = Track
  { trStart :: !(VU.Vector Word64)
  , trEnd :: !(VU.Vector Word64)
  , trKind :: !(VU.Vector Word8)
  , trThread :: !(VU.Vector Word32)
    -- ^ The thread running (for run spans) or the track's thread.
  , trStop :: !(VU.Vector Word8)
    -- ^ How the span ended.
  , trAux :: !(VU.Vector Word32)
    -- ^ Extra data: for a black-hole block, the owning thread (0 if unknown).
  , trCum :: !(V.Vector (VU.Vector Double))
  }
  deriving (Show)

data SpanRec = SpanRec
  { srStart :: !Word64
  , srEnd :: !Word64
  , srKind :: !SpanKind
  , srThread :: !Word32
  , srStop :: !StopCode
  , srAux :: !Word32
  }
  deriving (Eq, Show)

emptyTrack :: Track
emptyTrack = mkTrack []

-- | Build a track from spans sorted by start time.
mkTrack :: [SpanRec] -> Track
mkTrack spans =
  Track
    { trStart = VU.fromListN n (map srStart spans)
    , trEnd = ends
    , trKind = kinds
    , trThread = VU.fromListN n (map srThread spans)
    , trStop = VU.fromListN n (map srStop spans)
    , trAux = VU.fromListN n (map srAux spans)
    , trCum = V.generate nKinds cum
    }
  where
    n = length spans
    starts = VU.fromListN n (map srStart spans)
    ends = VU.fromListN n (map srEnd spans)
    kinds = VU.fromListN n (map srKind spans)
    cum k =
      VU.postscanl'
        (+)
        0
        (VU.zipWith3 (\s e kd -> if fromIntegral kd == k then fromIntegral (e - s) else 0) starts ends kinds)

trackLength :: Track -> Int
trackLength = VU.length . trStart

trackSpan :: Track -> Int -> SpanRec
trackSpan t i =
  SpanRec (trStart t VU.! i) (trEnd t VU.! i) (trKind t VU.! i) (trThread t VU.! i) (trStop t VU.! i) (trAux t VU.! i)

-- | The number of spans starting at or before @t@ (so the index of the last
-- such span plus one).
countStartingBy :: Track -> Word64 -> Int
countStartingBy tr t = go 0 (trackLength tr)
  where
    starts = trStart tr
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
           in if starts VU.! mid <= t then go (mid + 1) hi else go lo mid

-- | Total time of kind @k@ within @[0, t]@.
cumAt :: Track -> Int -> Word64 -> Double
cumAt tr k t =
  let i = countStartingBy tr t - 1
      cum = trCum tr V.! k
   in if i < 0
        then 0
        else
          let before = if i == 0 then 0 else cum VU.! (i - 1)
              s = trStart tr VU.! i
              e = trEnd tr VU.! i
              partial
                | fromIntegral (trKind tr VU.! i) /= k = 0
                | otherwise = fromIntegral (min t e - s)
           in before + partial

-- | Time spent in kind @k@ within @[a, b]@, in nanoseconds.
timeIn :: Track -> Int -> Word64 -> Word64 -> Double
timeIn tr k a b
  | b <= a || trackLength tr == 0 = 0
  | otherwise = cumAt tr k b - cumAt tr k a

-- | The span covering time @t@, if any.
spanIndexAt :: Track -> Word64 -> Maybe Int
spanIndexAt tr t =
  let i = countStartingBy tr t - 1
   in if i >= 0 && trEnd tr VU.! i >= t then Just i else Nothing

-- | The span at @t@, or failing that the nearest one within @tol@ either
-- side: when zoomed out, spans are narrower than the pointer.
pickSpan :: Track -> Word64 -> Word64 -> Maybe Int
pickSpan tr t tol = case spanIndexAt tr t of
  Just i -> Just i
  Nothing ->
    let lo = if t > tol then t - tol else 0
        i = countStartingBy tr lo
        cands = [j | j <- [max 0 (i - 1) .. min (trackLength tr - 1) (i + 8)], trEnd tr VU.! j >= lo, trStart tr VU.! j <= t + tol]
        dist j = let s = trStart tr VU.! j; e = trEnd tr VU.! j in if t < s then s - t else if t > e then t - e else 0
     in case cands of
          [] -> Nothing
          _ -> Just (snd (minimum [(dist j, j) | j <- cands]))

-- | First index whose key is >= @x@ in a vector sorted by that key.
lowerBoundBy :: (a -> Word64) -> V.Vector a -> Word64 -> Int
lowerBoundBy key vec x = go 0 (V.length vec)
  where
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
           in if key (vec V.! mid) < x then go (mid + 1) hi else go lo mid

-- | Index of the first span that ends at or after @t@: where drawing a view
-- starting at @t@ begins.
firstSpanEndingAfter :: Track -> Word64 -> Int
firstSpanEndingAfter tr t = max 0 (countStartingBy tr t - 1)

-- ---------------------------------------------------------------------------
-- Timeline

data ThreadInfo = ThreadInfo
  { tiId :: !Word32
  , tiLabel :: !(Maybe Text)
  , tiCreated :: !(Maybe Word64)
  , tiFinished :: !(Maybe Word64)
  , tiRuns :: !Int
  , tiRunTime :: !Word64
  , tiBlocked :: !(Map StopCode Word64)
    -- ^ Time spent off-CPU after each kind of stop.
  , tiTrack :: !Track
    -- ^ Run, blocked and runnable spans of this thread.
  , tiCaps :: ![Int]
    -- ^ Capabilities it ran on.
  , tiWakeups :: !(VU.Vector (Word64, Word32))
    -- ^ When it was woken, and by which thread: the one running on the
    -- capability that posted the wakeup (0 when none was).
  }
  deriving (Show)

threadName :: ThreadInfo -> Text
threadName ti = maybe ("thread " <> tshow (tiId ti)) (\l -> l <> " (" <> tshow (tiId ti) <> ")") (tiLabel ti)

data GCEvent = GCEvent
  { gcStart :: !Word64
  , gcEnd :: !Word64
  , gcCap :: !Int
  , gcGen :: !Int
    -- ^ The oldest generation collected; -1 when the log did not say.
  , gcCopied :: !Word64
  , gcParThreads :: !Int
  }
  deriving (Eq, Show)

data Marker = Marker
  { mkTime :: !Word64
  , mkText :: !Text
  , mkCap :: !(Maybe Int)
  , mkIsMarker :: !Bool
    -- ^ 'traceMarker' as opposed to 'traceEvent'.
  }
  deriving (Eq, Show)

type PointKind = Word8

pointMigrate, pointSteal, pointWakeup :: PointKind
pointMigrate = 0
pointSteal = 1
pointWakeup = 2

pointName :: PointKind -> Text
pointName = \case
  0 -> "thread migrated away"
  1 -> "spark stolen"
  2 -> "woke a thread on another capability"
  _ -> "event"

-- | @(time, value)@ samples sorted by time.
type Series = VU.Vector (Word64, Word64)

seriesValueAt :: Series -> Word64 -> Maybe Word64
seriesValueAt s t
  | VU.null s = Nothing
  | otherwise =
      let go lo hi
            | lo >= hi = lo
            | otherwise =
                let mid = (lo + hi + 1) `div` 2
                 in if fst (s VU.! mid) <= t then go mid hi else go lo (mid - 1)
          i = go 0 (VU.length s - 1)
       in if fst (s VU.! i) <= t then Just (snd (s VU.! i)) else Nothing

-- | Average rate of a cumulative counter between two times, per second.
seriesRateBetween :: Series -> Word64 -> Word64 -> Maybe Double
seriesRateBetween s a b = do
  va <- seriesValueAt s a
  vb <- seriesValueAt s b
  if b > a && vb >= va
    then Just (fromIntegral (vb - va) / (fromIntegral (b - a) / 1e9))
    else Nothing

data Timeline = Timeline
  { tlStart :: !Word64
  , tlEnd :: !Word64
  , tlCaps :: !(V.Vector Track)
  , tlCapPoints :: !(V.Vector (VU.Vector (Word64, Word8, Word32)))
    -- ^ Per capability: migrations, steals and cross-cap wakeups.
  , tlThreads :: !(Map Word32 ThreadInfo)
  , tlGCs :: !(V.Vector GCEvent)
  , tlAlloc :: !Series
    -- ^ Cumulative bytes allocated.
  , tlLive :: !Series
  , tlHeapSize :: !Series
  , tlMarkers :: !(V.Vector Marker)
  , tlHeapProfile :: !(Maybe HeapProfile)
  , tlInfo :: ![(Text, Text)]
  , tlRun :: !RunInfo
  , tlCostCentres :: !(IntMap CostCentreInfo)
    -- ^ By id, from a profiled program's @HeapProfCostCentre@ events.
  , tlCostCentresByLabel :: !(Map Text [Int])
    -- ^ Cost-centre ids by label, for finding one another panel names.
  , tlProfSamples :: !(V.Vector ProfSample)
    -- ^ A profiled program's cost-centre samples (@+RTS -p -l@), every
    -- capability's, sorted by time.
  , tlSamplePeriod :: !Word64
    -- ^ How far apart one capability's samples are, in ns, as the log
    -- shows it (not as requested: on Windows the timer is coarser); 0 with
    -- no samples.
  , tlCodeRuns :: !(V.Vector (V.Vector CodeRun))
    -- ^ Per capability, its samples merged into runs of one top frame.
  , tlSampleCounts :: !(IntMap Int)
    -- ^ Samples per top-frame cost centre, over the whole log.
  , tlTicky :: !(V.Vector TickyCounter)
    -- ^ A ticky build's counters (@-ticky@, @+RTS -lT@), most allocation
    -- first.
  , tlEventCount :: !Int
  }
  deriving (Show)

emptyTimeline :: Timeline
emptyTimeline = Timeline 0 0 V.empty V.empty M.empty V.empty VU.empty VU.empty VU.empty V.empty Nothing [] emptyRunInfo IM.empty M.empty V.empty 0 V.empty IM.empty V.empty 0

-- | A cost centre as the eventlog defines it.
data CostCentreInfo = CostCentreInfo
  { cciId :: !Int
  , cciLabel :: !Text
  , cciModule :: !Text
  , cciSrc :: !SrcLoc
  , cciCaf :: !Bool
  }
  deriving (Eq, Show)

data Summary = Summary
  { smDuration :: !Double
    -- ^ Seconds.
  , smGCCount :: !Int
  , smMajorGCs :: !Int
  , smGCPauseTotal :: !Double
    -- ^ Nanoseconds.
  , smGCPauseMax :: !Double
  , smMutatorFrac :: !Double
    -- ^ Fraction of capability time spent running Haskell code.
  , smGCFrac :: !Double
  , smAllocated :: !Word64
  , smPeakLive :: !Word64
  , smPeakHeap :: !Word64
  , smThreads :: !Int
  }
  deriving (Show)

summarize :: Timeline -> Summary
summarize tl =
  Summary
    { smDuration = fromIntegral span' / 1e9
    , smGCCount = V.length (tlGCs tl)
    , smMajorGCs = V.length (V.filter ((>= 1) . gcGen) (tlGCs tl))
    , smGCPauseTotal = sum pauses
    , smGCPauseMax = foldl' max 0 pauses
    , smMutatorFrac = frac kindRun
    , smGCFrac = frac kindGC
    , smAllocated = if VU.null (tlAlloc tl) then 0 else snd (VU.last (tlAlloc tl))
    , smPeakLive = VU.foldl' (\m (_, v) -> max m v) 0 (tlLive tl)
    , smPeakHeap = VU.foldl' (\m (_, v) -> max m v) 0 (tlHeapSize tl)
    , smThreads = M.size (tlThreads tl)
    }
  where
    span' = tlEnd tl - tlStart tl
    pauses = [fromIntegral (gcEnd g - gcStart g) | g <- V.toList (tlGCs tl)] :: [Double]
    capTime = fromIntegral span' * fromIntegral (max 1 (V.length (tlCaps tl))) :: Double
    frac k
      | capTime <= 0 = 0
      | otherwise = sum [timeIn c (fromIntegral k) (tlStart tl) (tlEnd tl) | c <- V.toList (tlCaps tl)] / capTime

-- ---------------------------------------------------------------------------
-- Joins

-- | Who woke a thread out of a blocked span, and when: the first wakeup
-- posted for it while it was blocked. The waker is 0 when no thread was
-- running on the capability that posted it.
wokenBy :: ThreadInfo -> SpanRec -> Maybe (Word64, Word32)
wokenBy ti s
  | srKind s /= kindBlocked = Nothing
  | otherwise = VU.find (\(t, _) -> t >= srStart s && t <= srEnd s) (tiWakeups ti)

-- | When each of the log's heap censuses was taken, in program time.
censusTimes :: Timeline -> VU.Vector Word64
censusTimes tl = maybe VU.empty (VU.map (\s -> round (s * 1e9)) . hpTimes) (tlHeapProfile tl)

-- | The census that shows the heap after a collection: the first one taken
-- after it began and before the next began. The RTS takes a census during a
-- major collection, so that is where one is found.
censusAfterCollection :: Timeline -> Int -> Maybe Int
censusAfterCollection tl i = do
  g <- tlGCs tl V.!? i
  let next = maybe maxBound gcStart (tlGCs tl V.!? (i + 1))
      cs = censusTimes tl
  j <- VU.findIndex (>= gcStart g) cs
  if cs VU.! j < next then Just j else Nothing

-- | The collection a census followed: the last one to begin at or before
-- it.
collectionBeforeCensus :: Timeline -> Int -> Maybe Int
collectionBeforeCensus tl j = do
  t <- censusTimes tl VU.!? j
  let i = lowerBoundBy gcStart (tlGCs tl) (t + 1) - 1
  if i >= 0 then Just i else Nothing

-- | What happened between two moments.
data RangeStats = RangeStats
  { rsCollections :: ![Int]
    -- ^ Collections overlapping the range, by index.
  , rsMajor :: !Int
  , rsPauseTotal :: !Word64
    -- ^ Nanoseconds, of the parts inside the range.
  , rsLongest :: !(Maybe Int)
  , rsAllocated :: !(Maybe Word64)
  , rsThreads :: ![(Word32, Word64)]
    -- ^ Threads that ran, with their running time in the range, busiest
    -- first.
  , rsLive :: !(Maybe Word64, Maybe Word64)
    -- ^ Live bytes at the start and the end.
  , rsHeap :: !(Maybe Word64, Maybe Word64)
  , rsCensuses :: ![Int]
  , rsMarkers :: ![Int]
  }
  deriving (Eq, Show)

rangeStats :: Timeline -> Word64 -> Word64 -> RangeStats
rangeStats tl a b =
  RangeStats
    { rsCollections = map fst gcs
    , rsMajor = length [() | (_, g) <- gcs, gcGen g >= 1]
    , rsPauseTotal = sum [min b (gcEnd g) - max a (gcStart g) | (_, g) <- gcs]
    , rsLongest = fst <$> foldr (\x m -> Just (maybe x (\y -> if pause x >= pause y then x else y) m)) Nothing gcs
    , rsAllocated = do
        va <- seriesValueAt (tlAlloc tl) a
        vb <- seriesValueAt (tlAlloc tl) b
        if vb >= va then Just (vb - va) else Nothing
    , rsThreads =
        sortOn (Down . snd) [(tiId ti, ns) | ti <- M.elems (tlThreads tl), let ns = round (timeIn (tiTrack ti) (fromIntegral kindRun) a b), ns > 0]
    , rsLive = (seriesValueAt (tlLive tl) a, seriesValueAt (tlLive tl) b)
    , rsHeap = (seriesValueAt (tlHeapSize tl) a, seriesValueAt (tlHeapSize tl) b)
    , rsCensuses = [j | (j, t) <- zip [0 ..] (VU.toList (censusTimes tl)), t >= a, t <= b]
    , rsMarkers = map fst (takeWhile ((<= b) . mkTime . snd) (zip [lo ..] (V.toList (V.drop lo (tlMarkers tl)))))
    }
  where
    i0 = lowerBoundBy gcEnd (tlGCs tl) a
    gcs = takeWhile ((<= b) . gcStart . snd) (zip [i0 ..] (V.toList (V.drop i0 (tlGCs tl))))
    pause (_, g) = gcEnd g - gcStart g
    lo = lowerBoundBy mkTime (tlMarkers tl) a

-- ---------------------------------------------------------------------------
-- Code samples

-- | One cost-centre sample: what a capability was running at a profiler
-- tick, from a profiled build run with @+RTS -p -l@.
data ProfSample = ProfSample
  { psTime :: !Word64
  , psCap :: !Int
  , psThread :: !Word32
    -- ^ The thread running on the capability at the time (0 when none
    -- was): the sample is part of that thread's CPU profile.
  , psStack :: !(VU.Vector Word32)
    -- ^ Cost-centre ids, innermost (the top frame) first.
  }
  deriving (Eq, Show)

-- | The sample's top frame.
psTop :: ProfSample -> Maybe Int
psTop s = fromIntegral <$> psStack s VU.!? 0

-- | Consecutive samples of one capability with the same top frame, as the
-- code track draws them. A sample stands for the period before it.
data CodeRun = CodeRun
  { crStart :: !Word64
  , crEnd :: !Word64
  , crCostCentre :: !Int
  , crSamples :: !Int
  , crFirst :: !Int
    -- ^ Index of its first sample in 'tlProfSamples', for the stack.
  }
  deriving (Eq, Show)

-- | The median gap between one capability's consecutive samples, from
-- samples sorted by time. The RTS samples every capability at each tick.
samplePeriod :: [ProfSample] -> Word64
samplePeriod samples =
  let byCap = IM.fromListWith (++) [(psCap s, [psTime s]) | s <- reverse samples]
      gaps = sortOn id [b - a | ts <- IM.elems byCap, (a, b) <- zip ts (drop 1 ts), b > a]
   in case gaps of
        [] -> 0
        _ -> gaps !! (length gaps `div` 2)

-- | Merge each capability's samples into runs of one top frame. A gap of
-- more than a period and a half between samples ends a run; samples
-- without a frame (at the stack's root) end one too.
mkCodeRuns :: Word64 -> Int -> V.Vector ProfSample -> V.Vector (V.Vector CodeRun)
mkCodeRuns period nCaps samples = V.generate nCaps (\c -> V.fromList (go Nothing (IM.findWithDefault [] c byCap)))
  where
    byCap = IM.fromListWith (++) [(psCap s, [(i, s)]) | (i, s) <- reverse (zip [0 ..] (V.toList samples))]
    slack = period + period `div` 2
    go acc [] = maybe [] pure acc
    go acc ((i, s) : rest) = case psTop s of
      Nothing -> maybe id (:) acc (go Nothing rest)
      Just cc -> case acc of
        Just r
          | crCostCentre r == cc && psTime s <= crEnd r + slack ->
              go (Just r {crEnd = psTime s, crSamples = crSamples r + 1}) rest
        _ ->
          let from = max (psTime s - min (psTime s) period) (maybe 0 crEnd acc)
           in maybe id (:) acc (go (Just (CodeRun from (psTime s) cc 1 i)) rest)

-- | The top frames of some samples, most sampled first.
data CodeProfile = CodeProfile
  { cpSamples :: !Int
    -- ^ Samples taken, idle ones included.
  , cpIdle :: !Int
    -- ^ Samples of an idle capability (the @IDLE@ cost centre), which
    -- 'cpTop' leaves out.
  , cpTop :: ![(Int, Int)]
    -- ^ Top-frame cost centre and its samples.
  }
  deriving (Eq, Show)

-- | The top frames of the samples taken between two moments that @keep@
-- keeps.
codeProfile :: Timeline -> (ProfSample -> Bool) -> Word64 -> Word64 -> CodeProfile
codeProfile tl keep a b =
  CodeProfile (length ss) (length (filter idle ss)) (sortOn (Down . snd) (IM.toList counts))
  where
    everything = tlProfSamples tl
    ss = filter keep (takeWhile ((<= b) . psTime) (V.toList (V.drop (lowerBoundBy psTime everything a) everything)))
    idle s = maybe False isIdleCostCentre (psTop s >>= (`IM.lookup` tlCostCentres tl))
    counts = IM.fromListWith (+) [(i, 1 :: Int) | s <- ss, not (idle s), Just i <- [psTop s]]

-- | What ran during a slice: the samples on its capability (when it is a
-- capability's slice) taken while its thread (when it has one) was
-- running.
sliceProfile :: Timeline -> Maybe Int -> Word32 -> Word64 -> Word64 -> CodeProfile
sliceProfile tl cap tid = codeProfile tl (\s -> maybe True (== psCap s) cap && (tid == 0 || psThread s == tid))

-- | A thread's CPU profile: the samples taken while it was running.
threadProfile :: Timeline -> Word32 -> CodeProfile
threadProfile tl tid = codeProfile tl ((== tid) . psThread) (tlStart tl) (tlEnd tl)

-- | @Module.label@, from the log's cost-centre table.
costCentreName :: Timeline -> Int -> Text
costCentreName tl i = maybe ("cost centre " <> tshow i) (\c -> cciModule c <> "." <> cciLabel c) (IM.lookup i (tlCostCentres tl))

-- | A cost centre by module (empty for any) and label.
costCentreNamed :: Timeline -> Text -> Text -> Maybe CostCentreInfo
costCentreNamed tl m l =
  listToMaybe
    [ c
    | i <- M.findWithDefault [] l (tlCostCentresByLabel tl)
    , Just c <- [IM.lookup i (tlCostCentres tl)]
    , T.null m || cciModule c == m
    ]

-- | The cost centre an idle capability's samples are charged to.
isIdleCostCentre :: CostCentreInfo -> Bool
isIdleCostCentre c = cciLabel c == "IDLE" && cciModule c == "IDLE"

-- | Too few samples to say much. The profiler samples each capability once
-- a tick, and on Windows its tick is far coarser than asked for (V4).
fewSamples :: Int -> Bool
fewSamples n = n < 30

-- ---------------------------------------------------------------------------
-- Ticky

-- | One ticky counter, from a ticky build (@-ticky@) run with @+RTS -lT@:
-- a closure's entries and allocation, summed over the log's samples (the
-- RTS resets the counters after each one).
data TickyCounter = TickyCounter
  { tcId :: !Word64
  , tcName :: !Text
    -- ^ As the RTS gives it: @main2_sat_s7qL{v} (Main) (fun) in s7qM@
    -- ("Stronghead.Join.parseTickyName").
  , tcArity :: !Int
  , tcKinds :: !Text
    -- ^ Argument kinds, one letter each.
  , tcInfo :: !Word64
    -- ^ Its info table's address.
  , tcEntries :: !Word64
  , tcAlloc :: !Word64
    -- ^ Bytes its code allocated.
  , tcAllocd :: !Word64
    -- ^ Bytes of its own closures allocated (with @-ticky-allocd@).
  , tcSeries :: !(VU.Vector (Word64, Word64, Word64))
    -- ^ Each sample: when it was taken, and the entries and allocation
    -- since the one before.
  }
  deriving (Eq, Show)

-- | The counters with entries or allocation between two moments, and how
-- much, most allocation first. A sample covers the time since the one
-- before it, so a range is as fine as the sampling (each major GC with
-- @-lT@).
tickyBetween :: Timeline -> Word64 -> Word64 -> [(TickyCounter, Word64, Word64)]
tickyBetween tl a b =
  sortOn
    (\(_, _, al) -> Down al)
    [ (c, sum (map fst in'), sum (map snd in'))
    | c <- V.toList (tlTicky tl)
    , let in' = [(e, al) | (t, e, al) <- VU.toList (tcSeries c), t > a, t <= b]
    , any (\(e, al) -> e > 0 || al > 0) in'
    ]
