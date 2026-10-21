-- | Moving between clocks and along them: the nearest sample to a moment,
-- the next or previous one, and the map from an artifact's own clock to
-- program time.
--
-- Program time is nanoseconds since the RTS started, as eventlog
-- timestamps count it ("Stronghead.Run"). Eventlog heap samples and Live
-- frames are already on it. A @.hp@ file is not: its seconds are most
-- likely mutator CPU time (spike V3), so its censuses are put on program
-- time by pairing them with the eventlog's censuses of the same run, whose
-- totals match exactly, and only approximately when there is no eventlog.
module Stronghead.Clock
  ( -- * Samples
    nearestIndex
  , stepTime
  , partitionPoint
    -- * Clocks
  , ClockMap (..)
  , secondsClock
  , pairedClock
  , toProgramNs
  , fromProgramNs
  , pairByTotals
  ) where

import Data.Map.Strict qualified as M
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)

-- | The index of the sample nearest @t@ in an ascending vector; 0 when it
-- is empty.
nearestIndex :: VU.Vector Double -> Double -> Int
nearestIndex ts t
  | VU.null ts = 0
  | otherwise =
      let i = partitionPoint (\j -> ts VU.! j >= t) 0 (VU.length ts)
          dist j = abs (ts VU.! j - t)
       in case [j | j <- [i - 1, i], j >= 0, j < VU.length ts] of
            [a, b] -> if dist b < dist a then b else a
            [a] -> a
            _ -> 0

-- | The first index in @[lo, hi)@ at which a predicate holds, for one that
-- is false and then true along the range; @hi@ when it never holds.
partitionPoint :: (Int -> Bool) -> Int -> Int -> Int
partitionPoint p = go
  where
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
           in if p mid then go lo mid else go (mid + 1) hi

-- | The next moment after @t@ (a positive direction) or the last before it
-- (a negative one), from an ascending vector.
stepTime :: Int -> VU.Vector Word64 -> Word64 -> Maybe Word64
stepTime dir ts t
  | dir >= 0 = VU.find (> t) ts
  | otherwise = snd <$> VU.unsnoc (VU.takeWhile (< t) ts)

-- | An artifact's clock, in seconds, against program time.
data ClockMap = ClockMap
  { cmPoints :: !(VU.Vector (Double, Double))
    -- ^ Moments both clocks saw, as @(seconds, program ns)@, ascending in
    -- both. Empty: the seconds are program time already.
  , cmExact :: !Bool
    -- ^ Whether the map is exact at its points (and the seconds are program
    -- time between them), or a guess.
  }
  deriving stock (Eq, Show)

-- | Seconds that are program time ('True'), or that stand in for it
-- ('False', a @.hp@ file on its own).
secondsClock :: Bool -> ClockMap
secondsClock = ClockMap VU.empty

-- | A clock through moments both clocks saw. Exact at each of them and
-- interpolated between, so exact as long as there are any.
pairedClock :: [(Double, Double)] -> ClockMap
pairedClock ps = ClockMap (VU.fromList ps) (not (null ps))

toProgramNs :: ClockMap -> Double -> Double
toProgramNs (ClockMap ps _) s = case VU.length ps of
  0 -> s * 1e9
  1 -> let (s0, p0) = ps VU.! 0 in p0 + (s - s0) * 1e9
  n ->
    let i = max 1 (min (n - 1) (VU.length (VU.takeWhile ((<= s) . fst) ps)))
        (s0, p0) = ps VU.! (i - 1)
        (s1, p1) = ps VU.! i
     in if s1 <= s0 then p0 else p0 + (s - s0) * (p1 - p0) / (s1 - s0)

fromProgramNs :: ClockMap -> Double -> Double
fromProgramNs (ClockMap ps _) p = case VU.length ps of
  0 -> p / 1e9
  1 -> let (s0, p0) = ps VU.! 0 in s0 + (p - p0) / 1e9
  n ->
    let i = max 1 (min (n - 1) (VU.length (VU.takeWhile ((<= p) . snd) ps)))
        (s0, p0) = ps VU.! (i - 1)
        (s1, p1) = ps VU.! i
     in if p1 <= p0 then s0 else s0 + (p - p0) * (s1 - s0) / (p1 - p0)

-- | Pair two runs of censuses by equal totals, in order: @(i, j)@ for the
-- @i@th of the first and the @j@th of the second. Totals are whole bytes,
-- so equal means equal. Empty censuses pair with nothing, nor do censuses
-- only one side took (a @.hp@ file's census at exit).
pairByTotals :: VU.Vector Double -> VU.Vector Double -> [(Int, Int)]
pairByTotals as bs = go 0 0
  where
    -- Where each total occurs in the second run, in order.
    at = M.fromListWith (flip (++)) [(b, [k]) | (k, b) <- zip [0 ..] (VU.toList bs), b > 0]
    go i j
      | i >= VU.length as = []
      | otherwise = case dropWhile (< j) (M.findWithDefault [] (as VU.! i) at) of
          k : _ -> (i, k) : go (i + 1) (k + 1)
          [] -> go (i + 1) j
