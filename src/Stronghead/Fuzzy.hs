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

import Data.Char (isAlphaNum, isLower, isSpace, isUpper, toLower)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | 'Nothing' when the query does not match. An empty query matches
-- everything with score 0.
fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query =
  let q = T.toLower (T.filter (not . isSpace) query)
   in fuzzyScorePrepared q (T.unpack q)

fuzzyScorePrepared :: Text -> String -> Text -> Maybe Int
fuzzyScorePrepared q qChars cand
  | null qChars = Just 0
  | otherwise = bonus <$> walk qChars 0 ' ' cand Nothing 0
  where
    walk [] _ _ _ _ !acc = Just acc
    walk (x : xs) !i !prevC rest !prev !acc = case T.uncons rest of
      Nothing -> Nothing
      Just (origC, more) ->
        let c = toLower origC
            i' = i + 1
         in if c == x
              then
                let atWord = not (isAlphaNum prevC) || (isLower prevC && isUpper origC)
                    run = if prev == Just (i - 1) then 5 else 0
                    word = if atWord then 8 else 0
                    gapCost = maybe (min 10 i) (\p -> min 3 (i - p - 1)) prev
                 in walk xs i' origC more (Just i) (acc + 1 + run + word - gapCost)
              else walk (x : xs) i' origC more prev acc
    bonus s =
      let lc = T.toLower cand
       in s
            + (if q `T.isPrefixOf` lc then 15 else 0)
            + (if q == lc then 25 else 0)
            - T.length cand `div` 16

-- | The candidates that match, best first; ties keep their order.
fuzzyRank :: Text -> (a -> Text) -> [a] -> [(Int, a)]
fuzzyRank query key xs
  | T.null q = map (0,) xs
  | otherwise =
      map snd . sortOn fst $
        [((Down s, i), (s, x)) | (i, x) <- zip [0 :: Int ..] xs, Just s <- [fuzzyScorePrepared q qChars (key x)]]
  where
    q = T.toLower (T.filter (not . isSpace) query)
    qChars = T.unpack q
