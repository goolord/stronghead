-- | Cost-centre profiles (@.prof@, from @+RTS -p@): the call tree with
-- individual and inherited time and allocation.
module Stronghead.Prof
  ( CostCentre (..)
  , ProfFile (..)
  , parseProf
  , loadProf
  , flattenCC
  ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import Data.Text qualified as T

import Stronghead.Format (readDecimal, readDouble)
import Stronghead.Source (SrcLoc, parseSrcLoc)
import Stronghead.TextFile (readTextFile)

data CostCentre = CostCentre
  { ccName :: !Text
  , ccModule :: !Text
  , ccSrc :: !Text
  , ccLoc :: !SrcLoc
    -- ^ 'ccSrc' parsed.
  , ccNo :: !Int
  , ccEntries :: !Integer
  , ccTime :: !Double
    -- ^ Individual %time.
  , ccAlloc :: !Double
  , ccInhTime :: !Double
    -- ^ Inherited %time (this node and everything under it).
  , ccInhAlloc :: !Double
  , ccChildren :: ![CostCentre]
  }
  deriving stock (Eq, Show)

data ProfFile = ProfFile
  { pfHeader :: ![Text]
    -- ^ The summary lines before the tables (program, total time, alloc).
  , pfRoots :: ![CostCentre]
  }
  deriving stock (Eq, Show)

loadProf :: FilePath -> IO (Either String ProfFile)
loadProf path = parseProf <$> readTextFile path

-- | Parse a @.prof@ file's call tree: the table whose header has a @no.@
-- column. Nesting is given by indentation.
parseProf :: Text -> Either String ProfFile
parseProf src =
  case break isTreeHeader ls of
    (_, []) -> Left "not a .prof file: no cost-centre tree header"
    (before, hdr : rest) ->
      let hasSrc = "SRC" `elem` T.words hdr
          rows = [r | l <- rest, not (T.all isSpace l), Just r <- [row hasSrc l]]
       in Right
            ProfFile
              { pfHeader = map T.copy (take 12 (filter (not . T.null . T.strip) before))
              , pfRoots = build rows
              }
  where
    ls = map (T.dropWhileEnd (== '\r')) (T.lines src)
    isTreeHeader l = "COST CENTRE" `T.isPrefixOf` T.stripStart l && "no." `elem` T.words l

-- | One table row: its indentation and the parsed node (without children).
row :: Bool -> Text -> Maybe (Int, CostCentre)
row hasSrc l =
  let depth = T.length (T.takeWhile (== ' ') l)
      ws = T.words l
      (name, modu, srcLoc, nums)
        | hasSrc, n : m : s : more <- ws, "<" `T.isPrefixOf` s, not (">" `T.isSuffixOf` s) =
            -- A placeholder with spaces: "<no location info>".
            let (inside, after) = break (">" `T.isSuffixOf`) more
             in (n, m, T.unwords (s : inside ++ take 1 after), drop 1 after)
        | hasSrc, n : m : s : more <- ws = (n, m, s, more)
        | n : m : more <- ws = (n, m, "", more)
        | otherwise = ("", "", "", [])
   in case nums of
        no : entries : it : ia : hT : hA : _ ->
          Just
            ( depth
            , let src = T.copy srcLoc -- copies, not slices that keep the whole file
               in CostCentre (T.copy name) (T.copy modu) src (parseSrcLoc src) (readI no) (readI entries) (readD it) (readD ia) (readD hT) (readD hA) []
            )
        _ -> Nothing
  where
    readI :: Integral a => Text -> a
    readI = fromMaybe 0 . readDecimal
    readD = fromMaybe 0 . readDouble

-- | Turn an indentation-annotated preorder listing into a forest.
build :: [(Int, CostCentre)] -> [CostCentre]
build = fst . go (-1)
  where
    go _ [] = ([], [])
    go parent rows@((d, cc) : rest)
      | d <= parent = ([], rows)
      | otherwise =
          let (kids, rest') = go d rest
              (sibs, rest'') = go parent rest'
           in (cc {ccChildren = kids} : sibs, rest'')

-- | Preorder flattening with depth, for list views.
flattenCC :: [CostCentre] -> [(Int, CostCentre)]
flattenCC = concatMap (go 0)
  where
    go d cc = (d, cc) : concatMap (go (d + 1)) (ccChildren cc)
