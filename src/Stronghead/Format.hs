-- | Human-readable rendering of the units every panel shows: nanosecond
-- timestamps, byte counts and rates.
module Stronghead.Format
  ( fmtNs
  , fmtTick
  , fmtSec
  , fmtBytes
  , fmtBytesD
  , fmtRate
  , fmtPct
  , fmtInt
  , fmtFixed
  , tshow
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Numeric (showFFloat)

tshow :: Show a => a -> Text
tshow = T.pack . show

fmtFixed :: Int -> Double -> Text
fmtFixed n x = T.pack (showFFloat (Just n) x "")

-- | A duration in nanoseconds, scaled to the largest unit that keeps it >= 1.
fmtNs :: Double -> Text
fmtNs ns = fmtFixed decimals (ns / scale) <> " " <> unit
  where
    (scale, unit, decimals) = nsUnit ns

-- | The unit 'fmtNs' shows a duration in: its size in ns, its name, and
-- how many decimals it takes.
nsUnit :: Double -> (Double, Text, Int)
nsUnit ns
  | a >= 1e9 = (1e9, "s", 3)
  | a >= 1e6 = (1e6, "ms", 2)
  | a >= 1e3 = (1e3, "µs", 1)
  | otherwise = (1, "ns", 0)
  where
    a = abs ns

-- | A tick on a time axis whose ticks are @step@ ns apart: in the unit
-- 'fmtNs' picks for the tick, with only the decimals the step needs, so a
-- ruler reads 0, 500 ms, 1 s, 1.5 s rather than 1.000 s.
fmtTick :: Double -> Double -> Text
fmtTick step ns
  | abs ns < 0.5 = "0"
  | otherwise = trim (fmtFixed decimals (ns / scale)) <> " " <> unit
  where
    (scale, unit, _) = nsUnit ns
    decimals = max 0 (ceiling (negate (logBase 10 (max 1e-9 step / scale)) - 1e-9))
    trim s
      | T.elem '.' s = T.dropWhileEnd (== '.') (T.dropWhileEnd (== '0') s)
      | otherwise = s

fmtSec :: Double -> Text
fmtSec s
  | s == 0 = "0 s"
  | otherwise = fmtNs (s * 1e9)

fmtBytes :: Word64 -> Text
fmtBytes = fmtBytesD . fromIntegral

fmtBytesD :: Double -> Text
fmtBytesD b
  | a >= 1024 ^ (3 :: Int) = fmtFixed 2 (b / 1024 ^ (3 :: Int)) <> " GiB"
  | a >= 1024 ^ (2 :: Int) = fmtFixed 1 (b / 1024 ^ (2 :: Int)) <> " MiB"
  | a >= 1024 = fmtFixed 1 (b / 1024) <> " KiB"
  | otherwise = fmtFixed 0 b <> " B"
  where
    a = abs b

-- | Bytes per second.
fmtRate :: Double -> Text
fmtRate r = fmtBytesD r <> "/s"

fmtPct :: Double -> Text
fmtPct x = fmtFixed 1 (x * 100) <> "%"

-- | An integer with thousands separators.
fmtInt :: Integral a => a -> Text
fmtInt n
  | n < 0 = "-" <> fmtInt (negate n)
  | otherwise = T.intercalate "," (reverse (map T.reverse (T.chunksOf 3 (T.reverse (tshow (toInteger n))))))
