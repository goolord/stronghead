-- | Compiler-to-runtime forensics over GHC Core (@-ddump-simpl@), STG
-- (@-ddump-stg-final@), and optional simplifier diagnostic dumps
-- (@-ddump-inlinings@, @-ddump-rule-firings@, @-ddump-spec@):
--
-- * Static STG closure-capture parsing (@[fv1, fv2] \\u []@) for offline,
--   cross-platform retainer slicing (no @ghc-debug@ required);
-- * Runtime-weighted optimization smell detection (missed specialization /
--   dictionary passing, broken fusion, boxed recursive loops, updateable
--   loop thunks, generic PAP calls);
-- * Alpha-normalized semantic Core & STG line diffs;
-- * Simplifier log archaeology.
module Stronghead.Dump.Forensics
  ( -- * Static STG closure captures & retainer slicing
    StgUpdateFlag (..)
  , StgClosureCapture (..)
  , stgClosureCaptures
  , staticRetainersOf
    -- * Runtime-weighted optimization smells
  , SmellKind (..)
  , smellKindTitle
  , CoreSmell (..)
  , detectCoreSmells
    -- * Alpha-normalized semantic Core & STG diffs
  , DiffLine (..)
  , SemanticBindingDiff (..)
  , alphaNormalize
  , semanticBindingDiff
    -- * Simplifier archaeology
  , OptLogKind (..)
  , OptLogEntry (..)
  , parseOptLog
  , discoverOptLogs
  ) where

import Control.Exception (IOException, try)
import Control.Monad.Extra (concatMapM, partitionM)
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isSuffixOf, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (mapAccumL)
import Data.Vector qualified as V
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import Stronghead.Dump
import Stronghead.Format (tshow)
import Stronghead.TextFile (readTextFile)

-- ---------------------------------------------------------------------------
-- Static STG closure-capture parsing & retainer slicing

data StgUpdateFlag
  = StgUpdateable
    -- ^ @\\u@: updateable thunk (retains captured free variables until forced).
  | StgSingleEntry
    -- ^ @\\s@: single-entry thunk (entered at most once).
  | StgReentrant
    -- ^ @\\r@: re-entrant function closure (PAP/FUN capturing free variables).
  | StgUnknown
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data StgClosureCapture = StgClosureCapture
  { scBinder :: !Text
    -- ^ STG binder name (e.g., @sat_s7qA@).
  , scEnclosing :: !Text
    -- ^ Top-level binding key it sits inside.
  , scLine :: !Int
    -- ^ 0-based line number in the STG dump.
  , scFlag :: !StgUpdateFlag
  , scCaptured :: ![Text]
    -- ^ Free variables captured in the closure environment (@[fv1, fv2]@).
  , scArgs :: ![Text]
    -- ^ Lambda arguments after the update flag (@[a1, a2]@).
  }
  deriving stock (Eq, Show)

-- | Extract every closure header with a free-variable capture list and
-- updateability flag (@[fvs] \\u|\\s|\\r [args]@) from an STG dump.
stgClosureCaptures :: CodeFile -> [StgClosureCapture]
stgClosureCaptures cf
  | cfKind cf /= KindStg = []
  | otherwise =
      concatMap parseBinding (V.toList (cfBindings cf))
  where
    ls = cfLines cf
    parseBinding b =
      go Nothing [bdFirst b .. min (V.length ls - 1) (bdLast b)]
      where
        go _ [] = []
        go prevBinder (i : rest) =
          let l = ls V.! i
              curBinder = detectBinder l
              effectiveBinder = fromMaybe (fromMaybe (bdName b) prevBinder) curBinder
           in case parseStgLambda l of
                Just (fvs, flag, args) ->
                  StgClosureCapture effectiveBinder (bdKey b) i flag fvs args : go (Just effectiveBinder) rest
                Nothing ->
                  go (if isJustText curBinder then curBinder else prevBinder) rest
    isJustText (Just _) = True
    isJustText Nothing = False

-- | Detect an STG local/top-level binder on a line like
-- @sat_s7qA [Occ=Once1] :: Int@ or @sat_s7qA = [x, y] \\u [] ...@.
detectBinder :: Text -> Maybe Text
detectBinder line =
  let s = T.stripStart line
      firstWord = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '$' || c == '\'' || c == '.' || c == '#') s
      rest = T.stripStart (T.drop (T.length firstWord) s)
   in if T.null firstWord
        || firstWord `elem` ["let", "letrec", "case", "of", "in", "join", "joinrec", "jump", "Rec", "end", "default", "module"]
        || "--" `T.isPrefixOf` s
        || "[" `T.isPrefixOf` s
        then Nothing
        else
          if "::" `T.isPrefixOf` rest
            || "=" `T.isPrefixOf` rest
            || "[Occ=" `T.isPrefixOf` rest
            || "[LclId" `T.isPrefixOf` rest
            || "[GblId" `T.isPrefixOf` rest
            then Just firstWord
            else Nothing

-- | Parse @[fv1 fv2] \\u [a1 a2]@ (or comma-separated free vars) on an STG line.
parseStgLambda :: Text -> Maybe ([Text], StgUpdateFlag, [Text])
parseStgLambda line = do
  (before, flag, after) <- findFlag line
  fvs <- lastBracketList before
  let args = fromMaybe [] (firstBracketList after)
  pure (fvs, flag, args)
  where
    findFlag t =
      listToMaybe
        [ (pre, flg, T.drop 2 suf)
        | (tok, flg) <- [("\\u", StgUpdateable), ("\\s", StgSingleEntry), ("\\r", StgReentrant)]
        , let (pre, suf) = T.breakOn tok t
        , not (T.null suf)
        , let nextCh = T.uncons (T.drop 2 suf)
        , case nextCh of
            Nothing -> True
            Just (c, _) -> isSpace c || c == '['
        ]
    lastBracketList t =
      let trimmed = T.stripEnd t
       in if "]" `T.isSuffixOf` trimmed
            then
              let withoutClose = T.dropEnd 1 trimmed
                  (pre, inside) = T.breakOnEnd "[" withoutClose
               in if T.null pre then Nothing else Just (parseVars inside)
            else Nothing
    firstBracketList t =
      let trimmed = T.stripStart t
       in if "[" `T.isPrefixOf` trimmed
            then
              let inside = T.takeWhile (/= ']') (T.drop 1 trimmed)
               in Just (parseVars inside)
            else Nothing
    parseVars inner =
      filter (not . T.null) $
        map (T.dropAround (\c -> c == ',' || isSpace c)) (T.words (T.replace "," " " inner))

-- | Find all STG closures capturing a variable matching @varQuery@ (by exact
-- binder name or normalized 'nameKey'). Updateable thunks come first.
staticRetainersOf :: CodeFile -> Text -> [StgClosureCapture]
staticRetainersOf cf varQuery =
  sortOn (\c -> (flagRank (scFlag c), scLine c))
    [ c
    | c <- stgClosureCaptures cf
    , any matches (scCaptured c)
    ]
  where
    qKey = nameKey varQuery
    matches v = v == varQuery || nameKey v == qKey
    flagRank = \case
      StgUpdateable -> (0 :: Int)
      StgSingleEntry -> 1
      StgReentrant -> 2
      StgUnknown -> 3

-- ---------------------------------------------------------------------------
-- Runtime-weighted optimization smell detector ("Perf Core-Lint")

data SmellKind
  = SmellDictPassing
    -- ^ Hot polymorphic function taking typeclass dictionaries (@$dNum@, @$dMonad@...).
  | SmellBrokenFusion
    -- ^ Unfused list/stream combinators (@build@, @foldr@, @augment@, @Stream@) surviving in Core.
  | SmellBoxedLoop
    -- ^ Recursive binding without worker/wrapper split that boxes primitives (@I#@, @D#@...).
  | SmellUpdateableLoopThunk
    -- ^ Updateable thunk (@\\u@) capturing free variables inside a recursive STG binding.
  | SmellPapCalls
    -- ^ Generic unknown-arity calls (@stg_ap_@) in STG.
  deriving stock (Eq, Ord, Show, Enum, Bounded)

smellKindTitle :: SmellKind -> Text
smellKindTitle = \case
  SmellDictPassing -> "Missed specialization (dictionary passing)"
  SmellBrokenFusion -> "Broken list/stream fusion"
  SmellBoxedLoop -> "Boxed recursive loop"
  SmellUpdateableLoopThunk -> "Updateable thunk in recursive loop"
  SmellPapCalls -> "Generic PAP / unknown call"

data CoreSmell = CoreSmell
  { csModule :: !Text
  , csBinding :: !Text
  , csLine :: !Int
  , csKind :: !SmellKind
  , csWeight :: !Double
    -- ^ Runtime hotness weight (e.g., %time + %alloc or ticky bytes; 1.0 when unweighted).
  , csSummary :: !Text
  , csRemediation :: !Text
  }
  deriving stock (Eq, Show)

-- | Detect optimization smells across optional Core and STG dumps, weighted
-- and ranked by a runtime hotness map keyed by normalized binding 'nameKey'.
detectCoreSmells :: Maybe CodeFile -> Maybe CodeFile -> Map Text Double -> [CoreSmell]
detectCoreSmells mCore mStg weights =
  sortOn (\s -> (Down (csWeight s), csKind s, csLine s)) (coreSmells ++ stgSmells)
  where
    weightOf k =
      if M.null weights
        then 1.0
        else M.findWithDefault 0 k weights
    include k = M.null weights || M.findWithDefault 0 k weights > 0

    workerKeys =
      nubOrd
        [ bdKey b
        | Just cf <- [mCore, mStg]
        , b <- V.toList (cfBindings cf)
        , "$w" `T.isPrefixOf` stripQualifier (bdName b)
        ]

    coreSmells = case mCore of
      Nothing -> []
      Just cf ->
        concat
          [ checkCoreBinding cf b
          | b <- V.toList (cfBindings cf)
          , not (isTypeableBinding (bdName b))
          , include (bdKey b)
          ]

    checkCoreBinding cf b =
      let w = weightOf (bdKey b)
          toks = concat [lineTokens cf i | i <- [bdFirst b .. min (V.length (cfLines cf) - 1) (bdLast b)]]
          identTexts = [tkText t | t <- toks, tkKind t == TkIdent]
          dicts = nubOrd [d | d <- identTexts, isDictArg (stripQualifier d)]
          fusionHits = nubOrd [f | f <- identTexts, stripUnique (stripQualifier f) `elem` ["build", "augment", "foldr", "unstream", "stream"]]
          notes = bdNotes b
          hasWorker = bdKey b `elem` workerKeys
       in concat
            [ [ CoreSmell
                  (cfModule cf)
                  (bdName b)
                  (bdFirst b)
                  SmellDictPassing
                  w
                  ("Takes runtime typeclass dictionaries: " <> T.intercalate ", " (take 4 dicts))
                  "Add {-# INLINABLE #-} or {-# SPECIALIZE #-} pragmas (or -fspecialise-aggressively) so GHC eliminates dictionary overhead."
              | not (null dicts)
              ]
            , [ CoreSmell
                  (cfModule cf)
                  (bdName b)
                  (bdFirst b)
                  SmellBrokenFusion
                  w
                  ("Unfused combinators remain after Core simplifier: " <> T.intercalate ", " fusionHits)
                  "Ensure producers/consumers inline before Phase 1, or avoid sharing intermediate lists across multiple consumers."
              | not (null fusionHits) && (nRecursive notes || not (null (nAllocs notes)))
              ]
            , [ CoreSmell
                  (cfModule cf)
                  (bdName b)
                  (fromMaybe (bdFirst b) (listToMaybe (nBoxing notes)))
                  SmellBoxedLoop
                  w
                  ("Recursive binding without $w worker-wrapper split has " <> tshow (length (nBoxing notes)) <> " boxing sites")
                  "Add BangPatterns (!) to loop accumulators or {-# UNPACK #-} on data fields so GHC unboxes the loop."
              | nRecursive notes && not hasWorker && not (null (nBoxing notes))
              ]
            ]

    stgSmells = case mStg of
      Nothing -> []
      Just cf ->
        let captures = stgClosureCaptures cf
            recKeys = [bdKey b | b <- V.toList (cfBindings cf), nRecursive (bdNotes b)]
            thunkSmells =
              [ CoreSmell
                  (cfModule cf)
                  (scEnclosing c)
                  (scLine c)
                  SmellUpdateableLoopThunk
                  (weightOf (scEnclosing c))
                  ("Updateable thunk " <> scBinder c <> " (\\u) inside recursive binding captures [" <> T.intercalate ", " (scCaptured c) <> "]")
                  "Force the bound expression with BangPatterns or seq to avoid allocating updateable thunks that retain captured variables."
              | c <- captures
              , scFlag c == StgUpdateable
              , not (null (scCaptured c))
              , scEnclosing c `elem` recKeys
              , include (scEnclosing c)
              ]
            papSmells =
              [ CoreSmell
                  (cfModule cf)
                  (bdName b)
                  hitLine
                  SmellPapCalls
                  (weightOf (bdKey b))
                  ("Contains generic unknown-arity application (stg_ap_*) in STG")
                  "Eta-expand higher-order arguments or mark the call site INLINE so GHC emits direct known calls."
              | b <- V.toList (cfBindings cf)
              , include (bdKey b)
              , Just hitLine <-
                  [ listToMaybe
                      [ i
                      | i <- [bdFirst b .. min (V.length (cfLines cf) - 1) (bdLast b)]
                      , "stg_ap_" `T.isInfixOf` (cfLines cf V.! i)
                      ]
                  ]
              ]
         in thunkSmells ++ papSmells

isDictArg :: Text -> Bool
isDictArg t =
  "$d" `T.isPrefixOf` t
    && T.length t > 2
    && maybe False (isUpper . fst) (T.uncons (T.drop 2 t))

-- ---------------------------------------------------------------------------
-- Alpha-normalized semantic Core & STG diffs

data DiffLine
  = DiffSame !Text
  | DiffDel !Text
  | DiffAdd !Text
  deriving stock (Eq, Show)

data SemanticBindingDiff = SemanticBindingDiff
  { sbdModule :: !Text
  , sbdKey :: !Text
  , sbdKind :: !CodeKind
  , sbdLines :: ![DiffLine]
  , sbdAdded :: !Int
  , sbdRemoved :: !Int
  }
  deriving stock (Eq, Show)

-- | Deterministically alpha-rename GHC unique suffixes (@sat_s7qA@ ->
-- @sat_v1@) in order of first appearance so rebuilds with different uniques
-- produce clean, minimal diffs.
alphaNormalize :: [Text] -> [Text]
alphaNormalize ls = snd (mapAccumL normLine (M.empty, 1 :: Int) ls)
  where
    normLine st line =
      let (toks, _) = tokenizeLine KindCore False line
          (st', parts) = mapAccumL normTok st toks
       in (st', T.concat parts)
    normTok st@(env, nxt) tk
      | tkKind tk == TkIdent || tkKind tk == TkCon =
          let raw = tkText tk
              stripped = stripUnique raw
           in if stripped == raw
                then (st, raw)
                else case M.lookup raw env of
                  Just alias -> (st, alias)
                  Nothing ->
                    let alias = stripped <> "_v" <> tshow nxt
                     in ((M.insert raw alias env, nxt + 1), alias)
      | otherwise = (st, tkText tk)

-- | Compute a line-level semantic diff for one binding key across baseline
-- and current dumps after alpha-normalizing GHC uniques.
semanticBindingDiff :: CodeFile -> CodeFile -> Text -> Maybe SemanticBindingDiff
semanticBindingDiff baseCf curCf key =
  case (sliceOf baseCf, sliceOf curCf) of
    ([], []) -> Nothing
    (bLines, cLines) ->
      let nb = alphaNormalize bLines
          nc = alphaNormalize cLines
          dlines = lcsDiff nb nc
          added = length [() | DiffAdd _ <- dlines]
          removed = length [() | DiffDel _ <- dlines]
       in Just (SemanticBindingDiff (cfModule curCf) key (cfKind curCf) dlines added removed)
  where
    sliceOf cf =
      concat
        [ [cfLines cf V.! l | l <- [bdFirst b .. min (V.length (cfLines cf) - 1) (bdLast b)]]
        | idx <- M.findWithDefault [] key (cfByKey cf)
        , let b = cfBindings cf V.! idx
        ]

-- | Simple O(m*n) LCS line diff bounded for binding-sized Core/STG snippets.
lcsDiff :: [Text] -> [Text] -> [DiffLine]
lcsDiff xs0 ys0
  | length xs0 * length ys0 > 250000 = map DiffDel xs0 ++ map DiffAdd ys0
  | otherwise = go 0 0
  where
    xv = V.fromList xs0
    yv = V.fromList ys0
    m = V.length xv
    n = V.length yv
    -- Dynamic programming table stored lazily in a vector of vectors.
    dp :: V.Vector (V.Vector Int)
    dp = V.generate (m + 1) \i ->
      V.generate (n + 1) \j ->
        if i == m || j == n
          then 0
          else
            if xv V.! i == yv V.! j
              then 1 + (dp V.! (i + 1) V.! (j + 1))
              else max (dp V.! (i + 1) V.! j) (dp V.! i V.! (j + 1))
    go i j
      | i >= m = [DiffAdd (yv V.! k) | k <- [j .. n - 1]]
      | j >= n = [DiffDel (xv V.! k) | k <- [i .. m - 1]]
      | xv V.! i == yv V.! j = DiffSame (xv V.! i) : go (i + 1) (j + 1)
      | dp V.! (i + 1) V.! j >= dp V.! i V.! (j + 1) = DiffDel (xv V.! i) : go (i + 1) j
      | otherwise = DiffAdd (yv V.! j) : go i (j + 1)

-- ---------------------------------------------------------------------------
-- Optional simplifier archaeology (-ddump-inlinings, -ddump-rule-firings, -ddump-spec)

data OptLogKind
  = OptRuleFired
  | OptInlined
  | OptNotInlined
  | OptSpecialised
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data OptLogEntry = OptLogEntry
  { olKind :: !OptLogKind
  , olBinding :: !Text
  , olDetail :: !Text
  , olLine :: !Int
  }
  deriving stock (Eq, Show)

-- | Parse GHC @-ddump-rule-firings@, @-ddump-inlinings@, and @-ddump-spec@
-- diagnostic output into structured entries.
parseOptLog :: Text -> [OptLogEntry]
parseOptLog src = mapMaybe parseLine' (zip [0 ..] (T.lines src))
  where
    parseLine' (i, raw) =
      let l = T.strip raw
       in if
            | Just rest <- T.stripPrefix "Rule fired: " l ->
                Just (OptLogEntry OptRuleFired (firstToken rest) (T.strip rest) i)
            | Just rest <- T.stripPrefix "Considering inlining: " l ->
                let b = T.dropAround (`elem` ("'`" :: String)) (firstToken rest)
                 in Just (OptLogEntry OptInlined b (T.strip rest) i)
            | Just rest <- T.stripPrefix "Inlining done: " l ->
                Just (OptLogEntry OptInlined (firstToken rest) (T.strip rest) i)
            | "Not inlining" `T.isPrefixOf` l || "Cannot inline" `T.isPrefixOf` l ->
                Just (OptLogEntry OptNotInlined (lastWord l) l i)
            | Just rest <- T.stripPrefix "SPEC " l ->
                Just (OptLogEntry OptSpecialised (firstToken rest) (T.strip rest) i)
            | otherwise -> Nothing
    firstToken t = T.takeWhile (\c -> not (isSpace c) && c /= '(' && c /= ',') (T.stripStart t)
    lastWord t = fromMaybe "" (listToMaybe (reverse (T.words t)))

-- | Discover optional GHC optimizer log files under a directory.
discoverOptLogs :: FilePath -> IO [(FilePath, [OptLogEntry])]
discoverOptLogs root = do
  files <- walk (8 :: Int) root
  let optFiles = [f | f <- files, any (`isSuffixOf` f) [".dump-rule-firings", ".dump-rule-rewrites", ".dump-inlinings", ".dump-spec"]]
  concatMapM
    ( \f -> do
        r <- try @IOException (readTextFile f)
        pure case r of
          Left _ -> []
          Right txt -> [(f, parseOptLog txt)]
    )
    optFiles
  where
    walk 0 _ = pure []
    walk d dir = do
      r <- try @IOException (listDirectory dir)
      case r of
        Left _ -> pure []
        Right entries -> do
          let paths = [dir </> e | e <- sort entries, case e of '.' : _ -> False; _ -> e /= "node_modules"]
          (dirs, files) <- partitionM doesDirectoryExist paths
          (files ++) <$> concatMapM (walk (d - 1)) dirs
