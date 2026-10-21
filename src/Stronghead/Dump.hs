-- | GHC intermediate-representation dumps (@-ddump-simpl@,
-- @-ddump-stg-final@, with @-ddump-to-file@) and Haskell source, parsed
-- into what the pipeline viewer needs: per-line tokens for highlighting and
-- hover, top-level bindings with their line ranges, and the optimisation
-- facts worth flagging on each binding (inline pragmas, worker/wrapper,
-- boxing, allocation sites).
module Stronghead.Dump
  ( CodeKind (..)
  , CodeFile (..)
  , Token (..)
  , TokKind (..)
  , Binding (..)
  , Notes (..)
  , parseCodeFile
  , loadCodeFile
  , tokenizeLine
  , stripUnique
  , stripQualifier
  , nameKey
  , lineTokens
  , ModuleArtifacts (..)
  , discoverArtifacts
  , dumpModuleName
  , sourceModuleName
  , dumpSourcePath
  , kindForPath
  , isTypeableBinding
  , occurrences
  ) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM)
import Data.ByteString qualified as B
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (isSuffixOf, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import System.Directory (doesDirectoryExist, listDirectory)
import Stronghead.Source (SrcSpan (..), parseSrcLoc, srcLocSpan)
import Stronghead.TextFile (readTextFile)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (</>))

data CodeKind = KindSource | KindCore | KindStg
  deriving (Eq, Ord, Show, Enum, Bounded)

data TokKind
  = TkIdent
  | TkCon
    -- ^ Capitalised identifier (data constructor or type).
  | TkKeyword
  | TkOperator
  | TkNumber
  | TkString
  | TkComment
  | TkPragma
    -- ^ Binder annotations like @[GblId, Arity=1]@ and @{-# ... #-}@.
  | TkSpace
  deriving (Eq, Show)

data Token = Token
  { tkKind :: !TokKind
  , tkText :: !Text
  , tkCol :: !Int
  }
  deriving (Eq, Show)

-- | Facts about one binding.
data Notes = Notes
  { nInline :: !(Maybe Text)
  , nArity :: !(Maybe Int)
  , nStrictness :: !(Maybe Text)
  , nCpr :: !(Maybe Text)
  , nSize :: !(Maybe Text)
  , nRecursive :: !Bool
  , nUnboxedOps :: !Int
    -- ^ Tokens that mention unboxed types, literals or primops (@#@).
  , nBoxing :: ![Int]
    -- ^ Lines applying a boxing constructor (@I#@, @D#@, ...).
  , nAllocs :: ![Int]
    -- ^ Lines with a @let@: in STG every one allocates a heap closure.
  , nJoins :: !Int
  }
  deriving (Eq, Show)

data Binding = Binding
  { bdName :: !Text
    -- ^ As written in the file.
  , bdKey :: !Text
    -- ^ Module qualifier and unique suffix stripped: what the panes match on.
  , bdFirst :: !Int
  , bdLast :: !Int
  , bdNotes :: !Notes
  }
  deriving (Eq, Show)

data CodeFile = CodeFile
  { cfPath :: !FilePath
  , cfKind :: !CodeKind
  , cfModule :: !Text
  , cfLines :: !(V.Vector Text)
  , cfTokens :: !(V.Vector [Token])
    -- ^ Lazily tokenised, one entry per line.
  , cfBindings :: !(V.Vector Binding)
  , cfByKey :: !(M.Map Text [Int])
    -- ^ Binding indices for each key.
  }

loadCodeFile :: CodeKind -> FilePath -> IO (Either String CodeFile)
loadCodeFile kind path = do
  r <- try (readTextFile path)
  pure $ case r of
    Left (e :: IOException) -> Left (show e)
    Right txt -> Right (parseCodeFile kind path txt)

kindForPath :: FilePath -> Maybe CodeKind
kindForPath p
  | "dump-simpl" `isSuffixOf` p = Just KindCore
  | "dump-stg-final" `isSuffixOf` p || "dump-stg" `isSuffixOf` p = Just KindStg
  | takeExtension p `elem` [".hs", ".lhs", ".hs-boot"] = Just KindSource
  | otherwise = Nothing

parseCodeFile :: CodeKind -> FilePath -> Text -> CodeFile
parseCodeFile kind path src =
  CodeFile
    { cfPath = path
    , cfKind = kind
    , cfModule = modName
    , cfLines = lines'
    , cfTokens = tokens
    , cfBindings = V.fromList bindings
    , cfByKey = M.fromListWith (flip (++)) [(bdKey b, [i]) | (i, b) <- zip [0 ..] bindings]
    }
  where
    lines' = V.fromList (map (T.replace "\t" "        " . T.dropWhileEnd (== '\r')) (T.lines src))
    tokens = V.fromList (tokenizeLines kind (V.toList lines'))
    fallbackName = if kind == KindSource then T.pack (takeBaseName path) else dumpModuleName path
    modName = fromMaybe fallbackName $ listToMaybe
      [ T.takeWhile (\c -> not (isSpace c) && c /= '(') (T.strip rest)
      | l <- take 400 (V.toList lines')
      , Just rest <- [T.stripPrefix "module " l]
      ]
    bindings = case kind of
      KindSource -> sourceBindings lines'
      _ -> dumpBindings kind lines' tokens

-- ---------------------------------------------------------------------------
-- Tokenising

-- | Tokenise lines, carrying block-comment state across them.
tokenizeLines :: CodeKind -> [Text] -> [[Token]]
tokenizeLines kind = go False
  where
    go _ [] = []
    go inC (l : ls) =
      let (toks, inC') = tokenizeLine kind inC l
       in toks : go inC' ls

lineTokens :: CodeFile -> Int -> [Token]
lineTokens cf i = fromMaybe [] (cfTokens cf V.!? i)

keywords :: CodeKind -> [Text]
keywords = \case
  KindSource ->
    [ "module", "import", "qualified", "as", "hiding", "where", "data", "newtype", "type", "class"
    , "instance", "deriving", "do", "if", "then", "else", "case", "of", "let", "in", "forall"
    , "family", "pattern", "mdo", "rec", "infixl", "infixr", "infix", "default", "foreign"
    ]
  KindCore -> ["case", "of", "let", "letrec", "in", "join", "joinrec", "jump", "forall", "Rec", "end", "\\"]
  KindStg -> ["case", "of", "let", "letrec", "in", "join", "joinrec", "jump", "forall", "Rec", "end", "\\r", "\\u", "\\s"]

symChar :: Char -> Bool
symChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

identStart :: Char -> Bool
identStart c = isAlpha c || c == '_'

identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_' || c == '\'' || c == '#'

-- | Tokenise one line. The flag says whether a @{- -}@ comment is open.
tokenizeLine :: CodeKind -> Bool -> Text -> ([Token], Bool)
tokenizeLine kind inComment0 line = go inComment0 0 line []
  where
    kws = keywords kind
    go inC col t acc
      | T.null t = (reverse acc, inC)
      | inC =
          let (body, rest) = T.breakOn "-}" t
              closed = not (T.null rest)
              txt = body <> (if closed then "-}" else "")
           in go (not closed) (col + T.length txt) (T.drop 2 rest) (Token TkComment txt col : acc)
      | otherwise =
          let c = T.head t
              emit k n = go inC (col + n) (T.drop n t) (Token k (T.take n t) col : acc)
           in if
                | "{-#" `T.isPrefixOf` t ->
                    let (body, rest) = T.breakOn "#-}" t
                        n = T.length body + min 3 (T.length rest)
                     in emit TkPragma n
                | "{-" `T.isPrefixOf` t -> go True col t acc
                | "--" `T.isPrefixOf` t, isLineComment t -> go inC (col + T.length t) "" (Token TkComment t col : acc)
                | isSpace c -> emit TkSpace (T.length (T.takeWhile isSpace t))
                | c == '"' -> emit TkString (stringLen t)
                | c == '\'' && T.length t >= 3 && T.index t 2 == '\'' -> emit TkString (3 + hashes (T.drop 3 t))
                | isDigit c ->
                    let n = T.length (T.takeWhile (\x -> isAlphaNum x || x == '.' || x == 'e') t)
                     in emit TkNumber (n + hashes (T.drop n t))
                | c == '[' && kind /= KindSource && col == 0 -> emit TkPragma (T.length t)
                | c == '[' && kind /= KindSource && annotationStart t -> emit TkPragma (bracketLen t)
                | c == '\\' && kind == KindStg && T.length t >= 2 && T.index t 1 `elem` ("rus" :: String) -> emit TkKeyword 2
                | c == '$' && T.length t >= 2 && identStart (T.index t 1) -> emit TkIdent (qualIdentLen t)
                | identStart c ->
                    let n = qualIdentLen t
                        w = T.take n t
                        k
                          | w `elem` kws = TkKeyword
                          | isUpper (T.head (lastSegment w)) = TkCon
                          | otherwise = TkIdent
                     in emit k n
                | symChar c ->
                    let n = T.length (T.takeWhile symChar t)
                        w = T.take n t
                     in emit (if w `elem` kws then TkKeyword else TkOperator) n
                | otherwise -> emit TkOperator 1
    isLineComment t = case T.uncons (T.dropWhile (== '-') t) of
      Nothing -> True
      Just (x, _) -> not (symChar x)
    hashes = T.length . T.takeWhile (== '#')
    annotationStart t = any (`T.isPrefixOf` t) ["[GblId", "[LclId", "[LclIdX", "[InlPrag", "[Occ=", "[Dmd=", "[OS="]

-- | Length of a (possibly module-qualified) identifier at the start of the
-- text, including GHC's @$w@-style prefixes and @#@ suffixes.
qualIdentLen :: Text -> Int
qualIdentLen t = go 0 t
  where
    go n s =
      let s1 = if "$" `T.isPrefixOf` s then T.drop 1 s else s
          dollar = T.length s - T.length s1
          seg = T.takeWhile identChar s1
          n' = n + dollar + T.length seg
          rest = T.drop (T.length seg) s1
       in case T.uncons rest of
            Just ('.', more)
              | not (T.null seg)
              , isUpper (T.head seg)
              , Just (x, _) <- T.uncons more
              , identStart x || x == '$' ->
                  go (n' + 1) more
            _ -> max 1 n'

lastSegment :: Text -> Text
lastSegment w = case T.splitOn "." (stripQualifier w) of
  [] -> w
  xs -> let l = last xs in if T.null l then w else T.dropWhile (== '$') l

stringLen :: Text -> Int
stringLen t = go 1 (T.drop 1 t)
  where
    go n s = case T.uncons s of
      Nothing -> n
      Just ('\\', r) -> go (n + 2) (T.drop 1 r)
      Just ('"', r) -> n + 1 + T.length (T.takeWhile (== '#') r)
      Just (_, r) -> go (n + 1) r

bracketLen :: Text -> Int
bracketLen t = go 0 (0 :: Int) (T.unpack t)
  where
    go n _ [] = n
    go n d (x : xs)
      | x == '[' = go (n + 1) (d + 1) xs
      | x == ']' = if d <= 1 then n + 1 else go (n + 1) (d - 1) xs
      | otherwise = go (n + 1) d xs

-- ---------------------------------------------------------------------------
-- Names

-- | Drop a GHC unique suffix: @x_a1Bc@ to @x@, @sat_s1x2@ to @sat@. A suffix
-- counts as a unique when it is short and contains a digit or an internal
-- capital, so ordinary snake_case names survive.
stripUnique :: Text -> Text
stripUnique w =
  let (pre, suf) = T.breakOnEnd "_" w
      base = T.dropEnd 1 pre
      hashes = T.takeWhileEnd (== '#') suf
      core = T.dropEnd (T.length hashes) suf
   in if
        | T.null pre || T.null base -> w
        | T.length core < 2 || T.length core > 7 -> w
        | not (isAlpha (T.head core)) -> w
        | not (T.all isAlphaNum core) -> w
        | T.any isDigit core || T.any isUpper (T.tail core) -> base <> hashes
        | otherwise -> w

-- | @GHC.Base.map@ to @map@, @Main.$wgo@ to @$wgo@.
stripQualifier :: Text -> Text
stripQualifier w = go w
  where
    go s =
      let seg = T.takeWhile identChar s
       in case T.uncons (T.drop (T.length seg) s) of
            Just ('.', rest)
              | not (T.null seg)
              , isUpper (T.head seg)
              , not (T.null rest) ->
                  go rest
            _ -> s

-- | What an identifier is matched on across source, Core and STG: no
-- module, no unique, and no worker/specialisation prefix, so hovering
-- @go@ in the source lights up @Main.$wgo@ and @go_s1x2@.
nameKey :: Text -> Text
nameKey w =
  let s = stripUnique (stripQualifier w)
      unprefixed = foldr (\p acc -> fromMaybe acc (T.stripPrefix p acc)) s ["$w", "$s", "$l", "$j"]
   in if T.null unprefixed then s else unprefixed

isTypeableBinding :: Text -> Bool
isTypeableBinding n = any (`T.isInfixOf` n) ["$trModule", "$tc", "$krep", "$tr"]

-- | Lines of the file mentioning a key (as an identifier token).
occurrences :: CodeFile -> Text -> [Int]
occurrences cf key =
  [ i
  | i <- [0 .. V.length (cfLines cf) - 1]
  , key `T.isInfixOf` (cfLines cf V.! i)
  , any (\tk -> tkKind tk == TkIdent && nameKey (tkText tk) == key) (lineTokens cf i)
  ]

-- ---------------------------------------------------------------------------
-- Bindings

-- | Top-level bindings in a dump: blank-line separated blocks whose first
-- real line starts with the binder.
dumpBindings :: CodeKind -> V.Vector Text -> V.Vector [Token] -> [Binding]
dumpBindings kind ls toks = go False (blocks 0)
  where
    n = V.length ls
    blank i = T.all isSpace (ls V.! i)
    -- (first, last) line ranges of non-blank runs
    blocks i
      | i >= n = []
      | blank i = blocks (i + 1)
      | otherwise =
          let j = until (\k -> k >= n || blank k) (+ 1) i
           in (i, j - 1) : blocks j
    go _ [] = []
    go inRec ((a, b) : rest) =
      let opens = any (\i -> T.strip (ls V.! i) == "Rec {") [a .. b]
          closes = any (\i -> "end Rec }" `T.isPrefixOf` T.strip (ls V.! i)) [a .. b]
          recNow = inRec || opens
          inRec' = (inRec || opens) && not closes
       in case binderIn a b of
            Just (i, name) -> mkBinding recNow name i a b : go inRec' rest
            Nothing -> go inRec' rest
    binderIn a b =
      listToMaybe
        [ (i, tkText t)
        | i <- [a .. b]
        , let l = ls V.! i
        , not (T.null l)
        , not (isSpace (T.head l))
        , not ("--" `T.isPrefixOf` l || "Rec {" `T.isPrefixOf` l || "end Rec" `T.isPrefixOf` l || "[" `T.isPrefixOf` l || "=" `T.isPrefixOf` l || "Result size" `T.isPrefixOf` l)
        , t : _ <- [filter ((/= TkSpace) . tkKind) (toks V.! i)]
        , tkKind t `elem` [TkIdent, TkCon]
        ]
    mkBinding recNow name i a b =
      let firstLine = a
          body = [a .. b]
          header = T.unwords [ls V.! k | k <- body, k <= headerEnd]
          headerEnd = fromMaybe b (listToMaybe [k | k <- body, "=" `T.isInfixOf` (ls V.! k), k > i || hasEq (ls V.! k)])
          hasEq l = " = " `T.isInfixOf` l || " =" `T.isSuffixOf` l
          allToks = concat [toks V.! k | k <- body]
          sizeLine = listToMaybe [T.strip r | k <- body, Just r <- [T.stripPrefix "-- RHS size:" (ls V.! k)]]
          letLines = [k | k <- body, any (\t -> tkKind t == TkKeyword && tkText t `elem` ["let", "letrec"]) (toks V.! k)]
          boxLines =
            [ k
            | k <- body
            , any (\t -> tkKind t == TkCon && lastSegment (tkText t) `elem` ["I#", "D#", "F#", "W#", "C#", "W8#", "W64#", "I64#"]) (toks V.! k)
            ]
       in Binding
            { bdName = name
            , bdKey = nameKey name
            , bdFirst = firstLine
            , bdLast = b
            , bdNotes =
                Notes
                  { nInline = annotation "InlPrag=" header
                  , nArity = annotation "Arity=" header >>= \x -> either (const Nothing) (Just . fst) (TR.decimal x)
                  , nStrictness = annotation "Str=" header
                  , nCpr = annotation "Cpr=" header
                  , nSize = sizeLine
                  , nRecursive = recNow
                  , nUnboxedOps = length [() | t <- allToks, tkKind t `elem` [TkIdent, TkCon, TkOperator, TkNumber], "#" `T.isSuffixOf` tkText t]
                  , nBoxing = boxLines
                  , nAllocs = if kind == KindStg || kind == KindCore then letLines else []
                  , nJoins = length [() | t <- allToks, tkKind t == TkKeyword, tkText t `elem` ["join", "joinrec"]]
                  }
            }

-- | The value of @key=@ in a binder annotation, up to the next top-level
-- comma or closing bracket.
annotation :: Text -> Text -> Maybe Text
annotation key header = case T.breakOn key header of
  (_, rest) | T.null rest -> Nothing
  (_, rest) ->
    let v = T.drop (T.length key) rest
        go _ [] acc = reverse acc
        go d (c : cs) acc
          | (c == ',' || c == ']') && d == (0 :: Int) = reverse acc
          | c `elem` ("[({<" :: String) = go (d + 1) cs (c : acc)
          | c `elem` ("])}>" :: String) = go (d - 1) cs (c : acc)
          | otherwise = go d cs (c : acc)
     in Just (T.strip (T.pack (go 0 (T.unpack v) [])))

-- | Top-level definitions in Haskell source: lines at column 0 that start
-- with a lower-case identifier (or an operator section in parentheses).
sourceBindings :: V.Vector Text -> [Binding]
sourceBindings ls = merge (mapMaybe def [0 .. n - 1])
  where
    n = V.length ls
    def i =
      let l = ls V.! i
       in case T.uncons l of
            Just (c, _)
              | isLower c || c == '_' ->
                  let w = T.takeWhile identChar l
                   in if w `elem` ["import", "module", "where", "data", "type", "newtype", "class", "instance", "deriving", "infixl", "infixr", "infix", "foreign", "default", "pattern"]
                        then Nothing
                        else Just (i, w)
              | c == '(' ->
                  let w = T.takeWhile (/= ')') (T.drop 1 l)
                   in if not (T.null w) && T.all symChar w then Just (i, w) else Nothing
            _ -> Nothing
    -- Consecutive equations and the signature of one function are one binding.
    merge ((i, w) : rest) =
      let (same, others) = span ((== w) . snd) rest
          lastLine = case others of
            (j, _) : _ -> j - 1
            [] -> n - 1
          endLine = trimEnd (max i (maybe i fst (lastMaybe same))) lastLine
       in Binding w w i endLine emptyNotes : merge others
    merge [] = []
    trimEnd lo hi = until (\k -> k <= lo || not (T.all isSpace (ls V.! k))) (subtract 1) hi
    lastMaybe xs = if null xs then Nothing else Just (last xs)

emptyNotes :: Notes
emptyNotes = Notes Nothing Nothing Nothing Nothing Nothing False 0 [] [] 0

-- ---------------------------------------------------------------------------
-- Discovery

-- | The artifacts found for one module.
data ModuleArtifacts = ModuleArtifacts
  { maModule :: !Text
  , maSource :: !(Maybe FilePath)
  , maCore :: !(Maybe FilePath)
  , maStg :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | Walk a directory (skipping VCS and package-db noise, to a bounded depth)
-- and pair each module's source with its Core and STG dumps. Dumps written
-- with @-ddump-to-file@ are named after the module (@Foo.Bar.dump-simpl@ or
-- @Foo/Bar.dump-simpl@ under @-dumpdir@).
discoverArtifacts :: FilePath -> IO [ModuleArtifacts]
discoverArtifacts root = do
  files <- walk (10 :: Int) root
  let dumps k = [(dumpModuleName f, f) | f <- files, kindForPath f == Just k]
      cores = M.fromList (dumps KindCore)
      stgs = M.fromList (dumps KindStg)
  sources <- fmap catMaybes . forM [f | f <- files, kindForPath f == Just KindSource] $ \f -> do
    m <- sourceModuleName f
    pure ((,f) <$> m)
  let srcMap = M.fromList sources
      -- A hierarchical -dumpdir names Foo/Bar.dump-simpl just "Bar".
      -- GHC names a Main module's dumps after its file (Loop.dump-simpl).
      byFile = M.fromList [(T.pack (takeBaseName f), f) | (_, f) <- sources]
      findSrc m = case M.lookup m srcMap of
        Just f -> Just f
        Nothing -> case M.lookup m byFile of
          Just f -> Just f
          Nothing -> listToMaybe [f | (sm, f) <- M.toList srcMap, ("." <> m) `T.isSuffixOf` sm]
      mods = M.keys (M.unions [() <$ cores, () <$ stgs])
  pure [ModuleArtifacts m (findSrc m) (M.lookup m cores) (M.lookup m stgs) | m <- mods]
  where
    walk 0 _ = pure []
    walk d dir = do
      r <- try (listDirectory dir)
      case r of
        Left (_ :: IOException) -> pure []
        Right entries -> do
          let paths = [dir </> e | e <- sortOn id entries, e `notElem` [".git", "node_modules"]]
          dirs <- filterM doesDirectoryExist paths
          let files = [p | p <- paths, p `notElem` dirs]
          deeper <- concat <$> mapM (walk (d - 1)) dirs
          pure (files ++ deeper)

-- | The module a dump is of, from its file name: @Foo.Bar.dump-simpl@ is
-- @Foo.Bar@. A build in another way puts the way in the name
-- (@Main.thr.dump-stg-final@, @Main.p_dyn.dump-simpl@), which is dropped.
dumpModuleName :: FilePath -> Text
dumpModuleName f =
  let b = takeFileName f
      stem = fromMaybe b (listToMaybe (mapMaybe (\ext -> stripSuffix ext b) [".dump-simpl", ".dump-stg-final", ".dump-stg"]))
      parts = T.splitOn "." (T.pack stem)
   in case parts of
        _ : _ : _ | isWay (last parts) -> T.intercalate "." (init parts)
        _ -> T.pack stem
  where
    stripSuffix suf str
      | suf `isSuffixOf` str = Just (take (length str - length suf) str)
      | otherwise = Nothing
    isWay w = not (T.null w) && all (`elem` ["thr", "p", "dyn", "debug", "l"]) (T.splitOn "_" w)

-- | The module a Haskell source file declares, or its base name for a
-- @Main@ module without a header. 'Nothing' when it can't be read.
sourceModuleName :: FilePath -> IO (Maybe Text)
sourceModuleName f = do
  r <- try (B.readFile f)
  pure $ case r of
    Left (_ :: IOException) -> Nothing
    Right bs ->
      let ls = T.lines (TE.decodeUtf8With TE.lenientDecode (B.take 20000 bs))
       in case [T.takeWhile (\c -> not (isSpace c) && c /= '(') (T.strip rest) | l <- ls, Just rest <- [T.stripPrefix "module " l]] of
            m : _ -> Just m
            [] -> Just (T.pack (takeBaseName f))

-- | The source file a dump was compiled from, as the compiler reported it
-- in the first @src<file:...>@ note (STG built with @-finfo-table-map@ has
-- them). For dumps whose source is not beside them.
dumpSourcePath :: CodeFile -> Maybe Text
dumpSourcePath cf =
  listToMaybe
    [ ssFile sp
    | l <- V.toList (cfLines cf)
    , let (_, rest) = T.breakOn "src<" l
    , not (T.null rest)
    , Just sp <- [srcLocSpan (parseSrcLoc (T.takeWhile (/= '>') (T.drop 4 rest)))]
    ]
