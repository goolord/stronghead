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
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..))
import Data.Text (Text)
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
  , tlEventCount :: !Int
  }
  deriving (Show)

emptyTimeline :: Timeline
emptyTimeline = Timeline 0 0 V.empty V.empty M.empty V.empty VU.empty VU.empty VU.empty V.empty Nothing [] emptyRunInfo IM.empty 0

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
