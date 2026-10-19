-- | Human-readable rendering of the units every panel shows: nanosecond
-- timestamps, byte counts and rates.
module Stronghead.Format
  ( fmtNs
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
fmtNs ns
  | a >= 1e9 = fmtFixed 3 (ns / 1e9) <> " s"
  | a >= 1e6 = fmtFixed 2 (ns / 1e6) <> " ms"
  | a >= 1e3 = fmtFixed 1 (ns / 1e3) <> " µs"
  | otherwise = fmtFixed 0 ns <> " ns"
  where
    a = abs ns

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
