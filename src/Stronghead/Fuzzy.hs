-- | Fuzzy matching for the command palette. A query matches a candidate
-- when its characters appear in it in order, ignoring case and the query's
-- spaces, so @wk3@ finds @worker-3@. Matches score higher for runs of
-- consecutive characters, for starting at a word boundary, for starting
-- early, and for short candidates; a candidate that starts with the query
-- or equals it scores higher still.
module Stronghead.Fuzzy
  ( fuzzyScore
  , fuzzyRank
  ) where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | 'Nothing' when the query does not match. An empty query matches
-- everything with score 0.
fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query cand
  | T.null q = Just 0
  | otherwise = bonus <$> walk (T.unpack q) (zip3 [0 :: Int ..] (T.unpack lc) bounds) Nothing 0
  where
    q = T.toLower (T.filter (not . isSpace) query)
    lc = T.toLower cand
    orig = T.unpack cand
    -- A word starts after anything that is not a letter or digit, and at
    -- a capital following a lower-case letter (camelCase).
    bounds = zipWith (\p c -> not (isAlphaNum p) || (isLower p && isUpper c)) (' ' : orig) orig
    walk [] _ _ acc = Just acc
    walk _ [] _ _ = Nothing
    walk qs@(x : xs) ((i, c, atWord) : rest) prev acc
      | c == x =
          let run = if prev == Just (i - 1) then 5 else 0
              word = if atWord then 8 else 0
              gapCost = maybe (min 10 i) (\p -> min 3 (i - p - 1)) prev
           in walk xs rest (Just i) (acc + 1 + run + word - gapCost)
      | otherwise = walk qs rest prev acc
    bonus s =
      s
        + (if q `T.isPrefixOf` lc then 15 else 0)
        + (if q == lc then 25 else 0)
        - T.length cand `div` 16

-- | The candidates that match, best first; ties keep their order.
fuzzyRank :: Text -> (a -> Text) -> [a] -> [(Int, a)]
fuzzyRank query key xs =
  map snd . sortOn fst $
    [((Down s, i), (s, x)) | (i, x) <- zip [0 :: Int ..] xs, Just s <- [fuzzyScore query (key x)]]
