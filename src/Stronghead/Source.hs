-- | Source locations and where to find their files.
--
-- GHC prints a source location in a handful of forms, and every artifact
-- Stronghead reads uses one of them: GHCi's stops and breakpoints, IPE info
-- tables, cost centres in @.prof@ files and the eventlog, and ghc-debug's
-- closure info. 'parseSrcLoc' reads them all.
--
-- The paths in those locations are relative to wherever the compiler ran,
-- which is rarely where Stronghead is looking. A 'SourceIndex' knows the
-- Haskell files under a list of roots and resolves a reported path to one
-- of them.
module Stronghead.Source
  ( -- * Spans
    SrcSpan (..)
  , parseSrcSpan
  , renderSrcSpan
  , SrcLoc (..)
  , parseSrcLoc
  , renderSrcLoc
  , srcLocSpan
    -- * Resolving paths
  , SourceIndex (..)
  , mkSourceIndex
  , buildSourceIndex
  , Resolution (..)
  , Resolved (..)
  , resolveSource
  , resolveModule
  , resolvesTo
  , pathComponents
  , shortFileName
  , findProjectRoot
  , sourceRoots
    -- * Editors
  , editorCommand
  ) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, guard)
import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight)
import Data.Foldable (asum)
import Data.List (isPrefixOf, sortOn, unsnoc)
import Data.List.Extra (wordsBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void (Void)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (isAbsolute, normalise, takeDirectory, takeDrive, takeExtension, (</>))

import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (Parsec, chunk, eof, optional, parseMaybe)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

import Stronghead.Format (readDecimal, tshow)

-- ---------------------------------------------------------------------------
-- Spans

-- | A span of source text. Lines and columns are 1-based, as GHC prints
-- them; a point location has the same start and end.
data SrcSpan = SrcSpan
  { ssFile :: !Text
  , ssLine :: !Int
  , ssCol :: !Int
  , ssEndLine :: !Int
  , ssEndCol :: !Int
  }
  deriving stock (Eq, Ord, Show)

-- | @file:12:3-20@, @file:12:3@, or @file:(12,3)-(14,5)@. The file may
-- itself contain colons (@C:\\src\\Main.hs:7:5@).
parseSrcSpan :: Text -> Maybe SrcSpan
parseSrcSpan t0 =
  let t = T.strip t0
   in case T.breakOnEnd ":(" t of
        (pre, rest)
          | not (T.null pre) -> do
              let file = T.dropEnd 2 pre
              guard (not (T.null file))
              (l1, c1, l2, c2) <- parseMaybe pMultiSpan rest
              pure (SrcSpan file l1 c1 l2 c2)
        _ -> simple t
  where
    pMultiSpan :: Parsec Void Text (Int, Int, Int, Int)
    pMultiSpan =
      (,,,)
        <$> (L.decimal <* char ',')
        <*> (L.decimal <* chunk ")-(")
        <*> (L.decimal <* char ',')
        <*> (L.decimal <* char ')' <* eof)
    pCols :: Parsec Void Text (Int, Int)
    pCols = do
      c1 <- L.decimal
      c2 <- fromMaybe c1 <$> optional (char '-' *> L.decimal)
      eof
      pure (c1, c2)
    simple t = case reverse (T.splitOn ":" t) of
      colPart : linePart : fileParts@(_ : _) -> do
        let file = T.intercalate ":" (reverse fileParts)
        guard (not (T.null file))
        l <- readDecimal linePart
        (c1, c2) <- parseMaybe pCols colPart
        pure (SrcSpan file l c1 l c2)
      _ -> Nothing

-- | The form GHC would print.
renderSrcSpan :: SrcSpan -> Text
renderSrcSpan (SrcSpan f l c el ec)
  | l == el && c == ec = f <> ":" <> tshow l <> ":" <> tshow c
  | l == el = f <> ":" <> tshow l <> ":" <> tshow c <> "-" <> tshow ec
  | otherwise = f <> ":(" <> tshow l <> "," <> tshow c <> ")-(" <> tshow el <> "," <> tshow ec <> ")"

-- | A location as GHC reports it: a span, or one of the placeholders it
-- prints for code that has none.
data SrcLoc
  = SrcAt !SrcSpan
  | SrcBuiltIn
    -- ^ @<built-in>@: the RTS's own cost centres (@MAIN@, @SYSTEM@, ...).
  | SrcEntireModule
    -- ^ @<entire-module>@: a module's CAF cost centre.
  | SrcNoLocation
    -- ^ @<no location info>@, or nothing at all.
  | SrcUnhelpful !Text
    -- ^ Any other text, kept as printed (@<wired into compiler>@, ...).
  deriving stock (Eq, Ord, Show)

-- | Total: text that is not a span is kept as 'SrcUnhelpful'. IPE events
-- join a missing file and span as a bare @:@, which is no location.
parseSrcLoc :: Text -> SrcLoc
parseSrcLoc t0 = case T.strip t0 of
  "" -> SrcNoLocation
  ":" -> SrcNoLocation
  "<built-in>" -> SrcBuiltIn
  "<entire-module>" -> SrcEntireModule
  "<no location info>" -> SrcNoLocation
  t -> maybe (SrcUnhelpful t) SrcAt (parseSrcSpan t)

renderSrcLoc :: SrcLoc -> Text
renderSrcLoc = \case
  SrcAt s -> renderSrcSpan s
  SrcBuiltIn -> "<built-in>"
  SrcEntireModule -> "<entire-module>"
  SrcNoLocation -> "<no location info>"
  SrcUnhelpful t -> t

srcLocSpan :: SrcLoc -> Maybe SrcSpan
srcLocSpan = \case
  SrcAt s -> Just s
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- The source index

-- | The Haskell files under some roots, indexed by their path components
-- read from the end, so that a reported path can be matched by its longest
-- tail.
data SourceIndex = SourceIndex
  { siRoots :: ![FilePath]
  , siFiles :: !(Set FilePath)
  , siByName :: !(Map String [FilePath])
    -- ^ Files by their last path component.
  }
  deriving stock (Show)

-- | An index over the given files (as found under the roots).
mkSourceIndex :: [FilePath] -> [FilePath] -> SourceIndex
mkSourceIndex roots files =
  SourceIndex
    { siRoots = roots
    , siFiles = unique
    , siByName = M.fromListWith (++) [(name, [f]) | f <- S.toList unique, Just (_, name) <- [unsnoc (pathComponents f)]]
    }
  where
    -- Overlapping roots find a file twice; once is not a tie.
    unique = S.fromList (map normalise files)

-- | Walk the roots for Haskell sources, skipping build output and hidden
-- directories. Stops after 20000 files so a root set to a home directory
-- does not hang the caller.
buildSourceIndex :: [FilePath] -> IO SourceIndex
buildSourceIndex roots = mkSourceIndex roots <$> go 20000 (outermost roots)
  where
    go :: Int -> [FilePath] -> IO [FilePath]
    go _ [] = pure []
    go budget (d : rest)
      | budget <= 0 = pure []
      | otherwise = do
          entries <- fromRight [] <$> try @IOException (listDirectory d)
          classified <- traverse (\e -> (e,) <$> doesDirectoryExist (d </> e)) entries
          let here = [d </> e | (e, False) <- classified, takeExtension e `elem` sourceExtensions]
              dirs = [d </> e | (e, True) <- classified, not (skipDir e)]
          (here ++) <$> go (budget - length here) (rest ++ dirs)
    skipDir e = "." `isPrefixOf` e || "dist" `isPrefixOf` e || e `elem` ["node_modules", "_build"]
    -- A root inside another is walked with it.
    outermost rs =
      let ns = nubOrd (map normalise rs)
       in [r | r <- ns, not (any (\o -> o /= r && pathComponents o `isPrefixOf` pathComponents r && sameDrive o r) ns)]
    sameDrive a b = takeDrive a == takeDrive b || not (isAbsolute a && isAbsolute b)

sourceExtensions :: [String]
sourceExtensions = [".hs", ".lhs", ".hsc", ".hs-boot", ".chs"]

-- | How a path was found, from most to least certain.
data Resolution
  = ByAbsolute
  | ByRoot
  | BySuffix !Int
    -- ^ The number of trailing path components that matched.
  | ByModule
  deriving stock (Eq, Ord, Show)

data Resolved = Resolved
  { rsPath :: !FilePath
  , rsHow :: !Resolution
  }
  deriving stock (Eq, Show)

-- | Find a reported path among the indexed files: as an absolute path, then
-- relative to each root, then by its longest matching tail of path
-- components. The module name, when given, is the last resort
-- (@Data.Foo@ finds @.../Data/Foo.hs@).
resolveSource :: SourceIndex -> Maybe Text -> Text -> Maybe Resolved
resolveSource ix modu reported =
  asum
    [ absolute
    , listToMaybe [Resolved f ByRoot | r <- siRoots ix, let f = normalise (r </> path), S.member f (siFiles ix)]
    , bySuffix
    , modu >>= resolveModule ix
    ]
  where
    path = T.unpack (T.strip reported)
    comps = pathComponents path
    absolute
      | isAbsolute path, S.member (normalise path) (siFiles ix) = Just (Resolved (normalise path) ByAbsolute)
      | otherwise = Nothing
    bySuffix = do
      (_, name) <- unsnoc comps
      candidates <- M.lookup name (siByName ix)
      best candidates comps BySuffix

-- | The file holding a module, by its dotted name.
resolveModule :: SourceIndex -> Text -> Maybe Resolved
resolveModule ix modu = do
  let parts = T.splitOn "." (T.strip modu)
  guard (not (any T.null parts))
  (prefix, modBase) <- unsnoc (map T.unpack parts)
  asum
    [ do
        let file = modBase <> ext
        candidates <- M.lookup file (siByName ix)
        best candidates (prefix ++ [file]) (const ByModule)
    | ext <- sourceExtensions
    ]

-- | The candidate sharing the longest tail with @comps@. A tie for the
-- longest is ambiguous (two @Main.hs@ files, say) and resolves to nothing:
-- the caller offers to locate the file instead of guessing. Only matches
-- that cover every component of a module path count for 'ByModule'.
best :: [FilePath] -> [String] -> (Int -> Resolution) -> Maybe Resolved
best candidates comps how =
  case sortOn (negate . fst) scored of
    (n, f) : rest
      | n >= minMatch, all ((< n) . fst) rest -> Just (Resolved f (how n))
    _ -> Nothing
  where
    scored = [(commonTail comps (pathComponents f), f) | f <- candidates]
    minMatch = case how 0 of
      ByModule -> length comps
      _ -> 1
    commonTail a b = length (takeWhile id (zipWith (==) (reverse a) (reverse b)))

-- | Whether a reported path is this file. When the index does not hold the
-- file (it is still being built, or the file is outside its roots), the
-- file alone is the index: a reported path matching its tail is it.
resolvesTo :: SourceIndex -> FilePath -> Text -> Bool
resolvesTo ix path reported =
  let file = normalise path
      ix' = if S.member file (siFiles ix) then ix else mkSourceIndex [] [file]
   in fmap rsPath (resolveSource ix' Nothing reported) == Just file

-- | A path's components, splitting on both separators so a Windows path
-- reported by one tool matches a POSIX one from another. Drops @.@ and the
-- drive.
pathComponents :: FilePath -> [String]
pathComponents p =
  [c | c <- wordsBy isSep (stripDrive p), c /= "."]
  where
    isSep c = c == '/' || c == '\\'
    stripDrive = \case
      d : ':' : more | not (isSep d), not (isSpace d) -> more
      s -> s

-- | The last path component of a POSIX or Windows path.
shortFileName :: Text -> Text
shortFileName = T.takeWhileEnd (\c -> c /= '/' && c /= '\\')

-- | The nearest directory at or above a path that holds a @.cabal@ file or
-- a @cabal.project@: where a build of it ran. Gives up after eight levels.
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot start = do
  isDir <- doesDirectoryExist start
  go (8 :: Int) (normalise (if isDir then start else takeDirectory start))
  where
    go 0 _ = pure Nothing
    go n d = do
      entries <- fromRight [] <$> try @IOException (listDirectory d)
      if any (\e -> takeExtension e == ".cabal" || e `elem` ["cabal.project", "stack.yaml"]) entries
        then pure (Just d)
        else
          let up = takeDirectory d
           in if up == d then pure Nothing else go (n - 1) up

-- | The roots to index for a set of starting points (the project root
-- setting, the dump folder, the debugger's directory): each one, and the
-- project it is in when there is one.
sourceRoots :: [FilePath] -> IO [FilePath]
sourceRoots starts = do
  existing <- filterM doesDirectoryExist [normalise s | s <- starts, not (null s)]
  projects <- traverse findProjectRoot existing
  pure (nubOrd (existing ++ catMaybes projects))

-- ---------------------------------------------------------------------------
-- Editors

-- | Fill an editor command template: @{file}@, @{line}@ and @{col}@ are
-- replaced, the file quoted when it holds a space
-- (@code -g {file}:{line}:{col}@).
editorCommand :: Text -> FilePath -> Int -> Int -> Text
editorCommand template file line col =
  T.replace "{file}" quoted . T.replace "{line}" (tshow line) . T.replace "{col}" (tshow (max 1 col)) $ template
  where
    quoted = if any isSpace file then "\"" <> T.pack file <> "\"" else T.pack file
