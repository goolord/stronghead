-- | Cost-centre profiles (@.prof@, from @+RTS -p@): the call tree with
-- individual and inherited time and allocation.
module Stronghead.Prof
  ( CostCentre (..)
  , ProfFile (..)
  , parseProf
  , loadProf
  , flattenCC
  ) where

import Data.ByteString qualified as B
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Read qualified as TR
import Stronghead.Source (SrcLoc, parseSrcLoc)

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
  deriving (Eq, Show)

data ProfFile = ProfFile
  { pfHeader :: ![Text]
    -- ^ The summary lines before the tables (program, total time, alloc).
  , pfRoots :: ![CostCentre]
  }
  deriving (Eq, Show)

loadProf :: FilePath -> IO (Either String ProfFile)
loadProf path = parseProf . TE.decodeUtf8With TE.lenientDecode <$> B.readFile path

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
              { pfHeader = take 12 (filter (not . T.null . T.strip) before)
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
        | hasSrc, n : m : s : more <- ws = (n, m, s, more)
        | n : m : more <- ws = (n, m, "", more)
        | otherwise = ("", "", "", [])
   in case nums of
        no : entries : it : ia : hT : hA : _ ->
          Just
            ( depth
            , CostCentre name modu srcLoc (parseSrcLoc srcLoc) (readI no) (readI entries) (readD it) (readD ia) (readD hT) (readD hA) []
            )
        _ -> Nothing
  where
    readI :: Integral a => Text -> a
    readI t = either (const 0) fst (TR.decimal t)
    readD t = either (const 0) fst (TR.double t)

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
