-- | Cross-artifact algebraic query & topological pointer-path engine ("Lens Pipes").
--
-- Queries filter, match pointer-path regular expressions, group, sort, and rank
-- entities across Stronghead's joined runtime & compiler domains:
--
-- > closures | path (MutVar ~> Thunk{3,}) | sort dom | top 20
-- > closures | path ((Bin | Tip) ~> Thunk) | dom >= 4KB
-- > bindings | boxes > 0 | not ww | recursive | sort time
-- > bands    | suspect | kind == thunk | sort score
-- > threads  | runnable >= 5ms | sort runnable
-- > spans    | duration >= 10ms | sort alloc
module Stronghead.Query
  ( -- * Query AST
    QueryDomain (..)
  , domainName
  , parseDomain
  , CmpOp (..)
  , PathHop (..)
  , NodeAtom (..)
  , PathSeg (..)
  , PathStep (..)
  , PathExpr (..)
  , Pred (..)
  , PipeStage (..)
  , Query (..)
    -- * Parsing, rendering & bidirectional UI pills
  , parseQuery
  , renderQuery
  , QueryPill (..)
  , queryToPills
  , removePillAt
  , builtinQueries
    -- * Execution context & results
  , QueryBinding (..)
  , buildQueryBindings
  , QueryContext (..)
  , emptyQueryContext
  , QueryRow (..)
  , QueryResult (..)
  , evalQuery
  , evalPathExpr
  , formatQueryMarkdown
  , formatQueryJson
  ) where

import Control.Applicative (many, optional, (<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void (Void)
import Data.Word (Word32, Word64)
import Numeric (showFFloat)
import Text.Megaparsec (Parsec, between, choice, eof, errorBundlePretty, notFollowedBy, parse, satisfy, sepBy1, takeWhile1P, try)
import Text.Megaparsec.Char (char, hspace, string')

import Stronghead.Debug.Anatomy (DominatorTree (..), HeapSkeleton (..), SpineFinding (..), buildHeapSkeleton, computeDominators, domRetained)
import Stronghead.Debug.Graph
  ( ClosureId
  , ClosureNode (..)
  , HeapSnapshot (..)
  , NodeKind (..)
  , fmtClosureId
  , isCAF
  , nodeTitle
  )
import Stronghead.Dump (Binding (..), CodeFile (..), Notes (..), isTypeableBinding, stripQualifier)
import Stronghead.Eventlog.Model
  ( GCEvent (..)
  , GCSyncLag (..)
  , ThreadInfo (..)
  , Timeline (..)
  , UserSpan (..)
  , kindRunnable
  , stopName
  , threadName
  , trackLength
  , trEnd
  , trKind
  , trStart
  )
import Stronghead.Format (fmtBytes, fmtBytesD, fmtFixed, fmtInt, fmtNs, readDecimal, readDouble, tshow)
import Data.Vector.Unboxed qualified as VU
import Stronghead.HeapProfile (Band (..), BandKind (..), HeapProfile (..), InfoProv (..), Suspect (..), leakSuspects)
import Stronghead.Join (CostTotal (..), bandTitle)
import Stronghead.Markdown qualified as MD
import Stronghead.Ref (Ref (..), RunKey (..), refTarget)
import Stronghead.Source (renderSrcLoc)

-- ---------------------------------------------------------------------------
-- Query AST

data QueryDomain
  = DomClosures
  | DomBands
  | DomBindings
  | DomThreads
  | DomSpans
  | DomGCs
  deriving stock (Eq, Ord, Show, Enum, Bounded)

domainName :: QueryDomain -> Text
domainName = \case
  DomClosures -> "closures"
  DomBands -> "bands"
  DomBindings -> "bindings"
  DomThreads -> "threads"
  DomSpans -> "spans"
  DomGCs -> "gcs"

parseDomain :: Text -> Maybe QueryDomain
parseDomain raw = case T.toLower (T.strip raw) of
  "closures" -> Just DomClosures
  "closure" -> Just DomClosures
  "heap" -> Just DomClosures
  "bands" -> Just DomBands
  "band" -> Just DomBands
  "bindings" -> Just DomBindings
  "binding" -> Just DomBindings
  "core" -> Just DomBindings
  "threads" -> Just DomThreads
  "thread" -> Just DomThreads
  "spans" -> Just DomSpans
  "span" -> Just DomSpans
  "gcs" -> Just DomGCs
  "gc" -> Just DomGCs
  _ -> Nothing

data CmpOp
  = OpEq
  | OpNe
  | OpGt
  | OpGe
  | OpLt
  | OpLe
  | OpMatch
  deriving stock (Eq, Ord, Show)

data PathHop
  = HopDirect
    -- ^ @~>@: direct child pointer edge.
  | HopTransitive
    -- ^ @~>*@: 1-or-more pointer hops.
  deriving stock (Eq, Ord, Show)

data NodeAtom
  = AtomAny
  | AtomRoot
  | AtomKind !NodeKind
  | AtomText !Text
  deriving stock (Eq, Ord, Show)

data PathSeg = PathSeg
  { psAtoms :: ![NodeAtom]
    -- ^ Disjunction of node matchers (e.g. @(IORef | TVar)@).
  , psMinRep :: !Int
  , psMaxRep :: !(Maybe Int)
    -- ^ @Nothing@ means unbounded (@{3,}@ or @+@).
  , psSlot :: !(Maybe Int)
    -- ^ Optional incoming pointer slot index constraint (@[0]@, @[1]@).
  }
  deriving stock (Eq, Ord, Show)

data PathStep = PathStep !PathHop !PathSeg
  deriving stock (Eq, Ord, Show)

data PathExpr = PathExpr !PathSeg ![PathStep]
  deriving stock (Eq, Ord, Show)

data Pred
  = PTrue
  | PAnd !Pred !Pred
  | POr !Pred !Pred
  | PNot !Pred
  | PNum !Text !CmpOp !Double
  | PStr !Text !CmpOp !Text
  | PFlag !Text
  | PPath !PathExpr
  deriving stock (Eq, Show)

data PipeStage
  = StageWhere !Pred
  | StageGroup !Text
  | StageSort !Text !Bool
    -- ^ Field name and @True@ for descending (largest first).
  | StageLimit !Int
  deriving stock (Eq, Show)

data Query = Query
  { qDomain :: !QueryDomain
  , qStages :: ![PipeStage]
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Built-in diagnostic query catalog

builtinQueries :: [(Text, Text, Text)]
builtinQueries =
  [ ( "Accumulator thunk spines"
    , "closures | path (MutVar ~> Thunk{3,}) | sort dom | top 25"
    , "Unforced thunk chains anchored inside an IORef, TVar, or MVar"
    )
  , ( "Spine-strict, value-lazy containers"
    , "closures | path ((Bin | Tip) ~> Thunk) | sort dom | top 25"
    , "Evaluated Map/IntMap/Set tree nodes holding unevaluated leaf thunks"
    )
  , ( "Selector & record-capture leaks"
    , "closures | thunk | dom >= 4KB | size <= 64B | sort dom | top 25"
    , "Small thunks exclusively retaining large sub-graphs"
    )
  , ( "Hot boxed recursive loops"
    , "bindings | boxes > 0 | not ww | recursive | sort time | top 25"
    , "Recursive Core bindings that failed worker/wrapper unboxing"
    )
  , ( "Run-queue starved threads"
    , "threads | runnable >= 5ms | sort runnable | top 25"
    , "Threads woken from locks/IO that stalled waiting for a capability"
    )
  , ( "Tail-latency span outliers"
    , "spans | duration >= 10ms | sort duration | top 25"
    , "Slowest instrumented withSpan intervals with allocation counts"
    )
  ]

-- ---------------------------------------------------------------------------
-- Parser & Pretty-Printer

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

sym :: Text -> Parser Text
sym s = lexeme (string' s)

parseQuery :: Text -> Either Text Query
parseQuery src =
  case parse (hspace *> pQuery <* eof) "query" (T.strip src) of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right q -> Right q

pQuery :: Parser Query
pQuery = do
  domWord <- lexeme pIdent
  dom <- case parseDomain domWord of
    Just d -> pure d
    Nothing -> fail ("unknown query domain '" <> T.unpack domWord <> "' (expected: closures, bands, bindings, threads, spans, gcs)")
  stages <- many (sym "|" *> pStage)
  pure (Query dom stages)

pStage :: Parser PipeStage
pStage =
  choice
    [ try do
        _ <- keyword "sort" <|> keyword "order"
        fld <- lexeme pIdent
        dir <- optional (choice [False <$ keyword "asc", True <$ keyword "desc"])
        pure (StageSort (T.toLower fld) (fromMaybe True dir))
    , try do
        _ <- keyword "top" <|> keyword "limit" <|> keyword "take"
        n <- lexeme pInt
        pure (StageLimit (max 1 n))
    , try do
        _ <- keyword "group" <|> keyword "group-by"
        fld <- lexeme pIdent
        pure (StageGroup (T.toLower fld))
    , StageWhere <$> (optional (try (keyword "where")) *> pOr)
    ]

keyword :: Text -> Parser Text
keyword kw = lexeme $ try do
  _ <- string' kw
  notFollowedBy (takeWhile1P Nothing (\c -> isAlphaNum c || c == '_' || c == '-'))
  pure kw

pOr :: Parser Pred
pOr = foldl1 POr <$> sepBy1 pAnd (try (sym "||") <|> keyword "or")

pAnd :: Parser Pred
pAnd = foldl1 PAnd <$> sepBy1 pNot (try (sym "&&") <|> keyword "and")

pNot :: Parser Pred
pNot =
  choice
    [ PNot <$> ((try (sym "!" <* notFollowedBy (char '=')) <|> keyword "not") *> pNot)
    , pAtomPred
    ]

pAtomPred :: Parser Pred
pAtomPred =
  choice
    [ try (keyword "path" *> (PPath <$> pPathWrapped))
    , try (between (sym "(") (sym ")") pOr)
    , try pComparison
    , PFlag . T.toLower <$> lexeme pIdent
    ]
  where
    pPathWrapped = between (sym "(") (sym ")") pPathExpr <|> pPathExpr

pComparison :: Parser Pred
pComparison = do
  lhs <- T.toLower <$> lexeme pIdent
  op <-
    choice
      [ OpGe <$ sym ">="
      , OpLe <$ sym "<="
      , OpNe <$ (sym "!=" <|> sym "/=")
      , OpEq <$ (sym "==" <|> sym "=")
      , OpGt <$ sym ">"
      , OpLt <$ sym "<"
      , OpMatch <$ (sym "~" <|> keyword "contains" <|> keyword "matches")
      ]
  rhsNumOrStr lhs op

rhsNumOrStr :: Text -> CmpOp -> Parser Pred
rhsNumOrStr lhs op =
  choice
    [ PStr lhs op <$> pQuoted
    , try do
        val <- pNumberWithUnit
        pure (PNum lhs op val)
    , PStr lhs op <$> lexeme pValueWord
    ]

pNumberWithUnit :: Parser Double
pNumberWithUnit = lexeme $ try do
  numTxt <- takeWhile1P Nothing (\c -> isDigit c || c == '.')
  base <- case readDouble numTxt of
    Just d -> pure d
    Nothing -> fail "invalid number"
  unit <- optional (takeWhile1P Nothing (\c -> isAlpha c || c == '%'))
  let mult = case fmap T.toLower unit of
        Nothing -> 1
        Just "%" -> 1
        Just "b" -> 1
        Just "kb" -> 1024
        Just "kib" -> 1024
        Just "mb" -> 1024 * 1024
        Just "mib" -> 1024 * 1024
        Just "gb" -> 1024 * 1024 * 1024
        Just "gib" -> 1024 * 1024 * 1024
        Just "ns" -> 1
        Just "us" -> 1e3
        Just "ms" -> 1e6
        Just "s" -> 1e9
        Just _ -> 0
  if mult == 0 then fail "unknown unit" else pure (base * mult)

pPathExpr :: Parser PathExpr
pPathExpr = do
  firstSeg <- pPathSeg
  rest <- many pPathStep
  pure (PathExpr firstSeg rest)

pPathStep :: Parser PathStep
pPathStep = do
  hop <-
    choice
      [ try (HopTransitive <$ sym "~>*")
      , HopDirect <$ (sym "~>" <|> sym "->")
      ]
  seg <- pPathSeg
  pure (PathStep hop seg)

pPathSeg :: Parser PathSeg
pPathSeg = do
  mPrefixSlot <- optional (try (char '.' *> lexeme pInt))
  atoms <-
    choice
      [ between (sym "(") (sym ")") (sepBy1 pNodeAtom (sym "|"))
      , pure <$> pNodeAtom
      ]
  mBracketSlot <- optional (try (between (sym "[") (sym "]") (lexeme pInt)))
  (lo, hi) <- pQuantifier
  pure (PathSeg atoms lo hi (mBracketSlot <|> mPrefixSlot))

pNodeAtom :: Parser NodeAtom
pNodeAtom =
  choice
    [ AtomAny <$ sym "*"
    , AtomText <$> pQuoted
    , classifyAtom <$> lexeme pIdent
    ]
  where
    classifyAtom w = case T.toLower w of
      "any" -> AtomAny
      "root" -> AtomRoot
      "thunk" -> AtomKind NThunk
      "thunks" -> AtomKind NThunk
      "fun" -> AtomKind NFun
      "function" -> AtomKind NFun
      "pap" -> AtomKind NPap
      "con" -> AtomKind NConstr
      "constr" -> AtomKind NConstr
      "ind" -> AtomKind NIndirection
      "array" -> AtomKind NArray
      "bytes" -> AtomKind NArray
      "mutvar" -> AtomKind NMutable
      "mutable" -> AtomKind NMutable
      "ioref" -> AtomKind NMutable
      "tvar" -> AtomKind NMutable
      "mvar" -> AtomKind NMutable
      "thread" -> AtomKind NThread
      "tso" -> AtomKind NThread
      "weak" -> AtomKind NWeak
      _ -> AtomText w

pQuantifier :: Parser (Int, Maybe Int)
pQuantifier =
  choice
    [ (1, Nothing) <$ sym "+"
    , try $ between (sym "{") (sym "}") do
        lo <- lexeme pInt
        hasComma <- optional (sym ",")
        case hasComma of
          Nothing -> pure (max 1 lo, Just (max 1 lo))
          Just _ -> do
            hi <- optional (lexeme pInt)
            pure (max 1 lo, fmap (max lo) hi)
    , pure (1, Just 1)
    ]

pQuoted :: Parser Text
pQuoted = lexeme do
  _ <- char '"'
  body <- many (satisfy (\c -> c /= '"' && c /= '\n' && c /= '\r'))
  _ <- char '"'
  pure (T.pack body)

pInt :: Parser Int
pInt = do
  ds <- takeWhile1P Nothing isDigit
  case readDecimal ds of
    Just n -> pure n
    Nothing -> fail "expected integer"

pIdent :: Parser Text
pIdent = takeWhile1P Nothing (\c -> isAlphaNum c || c `elem` ("_-.#" :: String))

pValueWord :: Parser Text
pValueWord = takeWhile1P Nothing (\c -> not (isSpace c) && c `notElem` ("|()[]{}" :: String))

-- | Render a 'Query' back into canonical Lens Pipe syntax.
renderQuery :: Query -> Text
renderQuery Query {..} =
  T.intercalate " | " (domainName qDomain : map renderStage qStages)

renderStage :: PipeStage -> Text
renderStage = \case
  StageWhere p -> renderPred p
  StageGroup g -> "group " <> g
  StageSort f desc -> "sort " <> f <> (if desc then "" else " asc")
  StageLimit n -> "top " <> tshow n

renderPred :: Pred -> Text
renderPred = \case
  PTrue -> "true"
  PAnd a b -> renderPred a <> " && " <> renderPred b
  POr a b -> "(" <> renderPred a <> " || " <> renderPred b <> ")"
  PNot a -> "not " <> renderPred a
  PNum k op v -> k <> " " <> renderOp op <> " " <> fmtCompactNum k v
  PStr k op s -> k <> " " <> renderOp op <> " " <> quoteIfNeeded s
  PFlag f -> f
  PPath pe -> "path (" <> renderPath pe <> ")"
  where
    renderOp = \case
      OpEq -> "=="
      OpNe -> "!="
      OpGt -> ">"
      OpGe -> ">="
      OpLt -> "<"
      OpLe -> "<="
      OpMatch -> "~"
    quoteIfNeeded t
      | T.any isSpace t || T.null t = "\"" <> t <> "\""
      | otherwise = t
    fmtCompactNum k v
      | k `elem` ["dom", "retained", "bytes", "size", "peak", "live", "alloc_bytes", "waste"] && v >= 1048576 =
          T.pack (showFFloat (Just 1) (v / 1048576) "MB")
      | k `elem` ["dom", "retained", "bytes", "size", "peak", "live", "alloc_bytes", "waste"] && v >= 1024 =
          T.pack (showFFloat (Just 0) (v / 1024) "KB")
      | k `elem` ["blocked", "runnable", "duration", "pause", "sync_lag", "run"] && v >= 1e6 =
          T.pack (showFFloat (Just 1) (v / 1e6) "ms")
      | otherwise = T.pack (showFFloat Nothing v "")

renderPath :: PathExpr -> Text
renderPath (PathExpr s0 steps) =
  renderSeg s0 <> T.concat [" " <> (if h == HopTransitive then "~>*" else "~>") <> " " <> renderSeg s | PathStep h s <- steps]
  where
    renderSeg PathSeg {..} =
      let atomTxt = case psAtoms of
            [a] -> renderAtom a
            as -> "(" <> T.intercalate " | " (map renderAtom as) <> ")"
          slotTxt = maybe "" (\sl -> "[" <> tshow sl <> "]") psSlot
          repTxt = case (psMinRep, psMaxRep) of
            (1, Just 1) -> ""
            (1, Nothing) -> "+"
            (lo, Nothing) -> "{" <> tshow lo <> ",}"
            (lo, Just hi) | lo == hi -> "{" <> tshow lo <> "}"
            (lo, Just hi) -> "{" <> tshow lo <> "," <> tshow hi <> "}"
       in atomTxt <> slotTxt <> repTxt
    renderAtom = \case
      AtomAny -> "*"
      AtomRoot -> "root"
      AtomKind NThunk -> "Thunk"
      AtomKind NFun -> "Fun"
      AtomKind NPap -> "PAP"
      AtomKind NConstr -> "Con"
      AtomKind NIndirection -> "Ind"
      AtomKind NArray -> "Array"
      AtomKind NMutable -> "MutVar"
      AtomKind NThread -> "Thread"
      AtomKind NWeak -> "Weak"
      AtomKind NOther -> "Other"
      AtomText t -> t

-- | Bidirectional visual pills representing each stage in a 'Query'.
data QueryPill = QueryPill
  { qpLabel :: !Text
  , qpStageText :: !Text
  }
  deriving stock (Eq, Show)

queryToPills :: Query -> [QueryPill]
queryToPills q =
  [ let txt = renderStage st in QueryPill txt txt
  | st <- qStages q
  ]

removePillAt :: Int -> Query -> Query
removePillAt idx q =
  q {qStages = [s | (i, s) <- zip [0 ..] (qStages q), i /= idx]}

-- ---------------------------------------------------------------------------
-- Execution Context & Evaluation

data QueryBinding = QueryBinding
  { qbModule :: !Text
  , qbKey :: !Text
  , qbWorker :: !Bool
  , qbRecursive :: !Bool
  , qbBoxes :: !Int
  , qbLets :: !Int
  , qbUnboxedOps :: !Int
  , qbTimePct :: !Double
  , qbAllocPct :: !Double
  , qbEntries :: !Word64
  , qbTickyBytes :: !Word64
  , qbHeapBytes :: !Double
  , qbSource :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | Join Core and STG files with optional @.prof@ 'CostTotal's into queryable
-- binding records.
buildQueryBindings :: [(Text, Maybe CodeFile, Maybe CodeFile)] -> [CostTotal] -> [QueryBinding]
buildQueryBindings mods costs =
  concatMap buildMod mods
  where
    costMap :: Map (Text, Text) (Double, Double, Word64)
    costMap =
      M.fromListWith
        (\(t1, a1, e1) (t2, a2, e2) -> (t1 + t2, a1 + a2, e1 + e2))
        [ ((ctModule c, ctName c), (ctInhTime c, ctInhAlloc c, fromIntegral (max 0 (ctEntries c))))
        | c <- costs
        ]
    buildMod (modName, mCore, mStg) =
      let keys = nubOrd [bdKey b | Just cf <- [mCore, mStg], b <- V.toList (cfBindings cf)]
          findB mcf k = case mcf of
            Nothing -> []
            Just cf -> [cfBindings cf V.! i | i <- M.findWithDefault [] k (cfByKey cf)]
       in [ let cs = findB mCore k
                ss = findB mStg k
                allB = cs ++ ss
                (tPct, aPct, ents) = M.findWithDefault (0, 0, 0) (modName, k) costMap
             in QueryBinding
                  { qbModule = modName
                  , qbKey = k
                  , qbWorker = any (("$w" `T.isPrefixOf`) . stripQualifier . bdName) allB
                  , qbRecursive = any (nRecursive . bdNotes) allB
                  , qbBoxes = sum (map (length . nBoxing . bdNotes) cs)
                  , qbLets = sum (map (length . nAllocs . bdNotes) ss)
                  , qbUnboxedOps = sum (map (nUnboxedOps . bdNotes) cs)
                  , qbTimePct = tPct
                  , qbAllocPct = aPct
                  , qbEntries = ents
                  , qbTickyBytes = 0
                  , qbHeapBytes = 0
                  , qbSource = fmap (T.pack . cfPath) mCore <|> fmap (T.pack . cfPath) mStg
                  }
          | k <- keys
          , not (T.null k)
          , not (isTypeableBinding k)
          ]

data QueryContext = QueryContext
  { qcRunKey :: !RunKey
  , qcTimeline :: !(Maybe Timeline)
  , qcHeap :: !(Maybe HeapProfile)
  , qcSnapshot :: !(Maybe HeapSnapshot)
  , qcSnapshotIndex :: !Int
  , qcDominators :: !(Maybe DominatorTree)
  , qcSurvivors :: !(Set ClosureId)
  , qcBindings :: ![QueryBinding]
  }

emptyQueryContext :: QueryContext
emptyQueryContext =
  QueryContext
    { qcRunKey = RunKey "?:?"
    , qcTimeline = Nothing
    , qcHeap = Nothing
    , qcSnapshot = Nothing
    , qcSnapshotIndex = 0
    , qcDominators = Nothing
    , qcSurvivors = S.empty
    , qcBindings = []
    }

data QueryRow = QueryRow
  { qrRef :: !(Maybe Ref)
  , qrTitle :: !Text
  , qrKindBadge :: !Text
  , qrPrimaryMetric :: !Text
  , qrPrimaryValue :: !Double
  , qrSecondary :: !Text
  , qrSource :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

data QueryResult = QueryResult
  { qresDomain :: !QueryDomain
  , qresQueryText :: !Text
  , qresTotalMatched :: !Int
  , qresTotalBytes :: !Word64
  , qresRows :: ![QueryRow]
  }
  deriving stock (Eq, Show)

-- | Internal candidate row carrying rich fields for filtering, grouping, and sorting.
data Cand = Cand
  { cdRow :: !QueryRow
  , cdNums :: !(Map Text Double)
  , cdStrs :: !(Map Text Text)
  , cdFlags :: !(Set Text)
  , cdCid :: !(Maybe ClosureId)
  , cdBytes :: !Word64
  }

evalQuery :: QueryContext -> Query -> QueryResult
evalQuery ctx q@Query {..} =
  let baseCands = candidatesFor ctx qDomain
      pathSets = precomputePaths ctx qStages
      filtered = foldl' (applyFilterStage pathSets) baseCands [p | StageWhere p <- qStages]
      grouped = case [g | StageGroup g <- qStages] of
        g : _ -> groupCandidates g filtered
        [] -> filtered
      sorted = case reverse [s | s@(StageSort _ _) <- qStages] of
        StageSort fld desc : _ -> sortCandidates fld desc grouped
        _ -> sortOn (Down . qrPrimaryValue . cdRow) grouped
      totalMatched = length sorted
      totalBytes = sum (map cdBytes sorted)
      limitN = case reverse [n | StageLimit n <- qStages] of
        n : _ -> n
        [] -> 50
      finalRows = map cdRow (take limitN sorted)
   in QueryResult
        { qresDomain = qDomain
        , qresQueryText = renderQuery q
        , qresTotalMatched = totalMatched
        , qresTotalBytes = totalBytes
        , qresRows = finalRows
        }

precomputePaths :: QueryContext -> [PipeStage] -> Map Text (Set ClosureId)
precomputePaths ctx stages =
  case qcSnapshot ctx of
    Nothing -> M.empty
    Just snap ->
      let collect = \case
            PAnd a b -> collect a ++ collect b
            POr a b -> collect a ++ collect b
            PNot a -> collect a
            PPath pe -> [pe]
            _ -> []
          exprs = concat [collect p | StageWhere p <- stages]
       in M.fromList [(renderPath pe, evalPathExpr snap pe) | pe <- exprs]

applyFilterStage :: Map Text (Set ClosureId) -> [Cand] -> Pred -> [Cand]
applyFilterStage pathSets cands pr = filter (matchPred pathSets pr) cands

matchPred :: Map Text (Set ClosureId) -> Pred -> Cand -> Bool
matchPred pathSets pr c = case pr of
  PTrue -> True
  PAnd a b -> matchPred pathSets a c && matchPred pathSets b c
  POr a b -> matchPred pathSets a c || matchPred pathSets b c
  PNot a -> not (matchPred pathSets a c)
  PFlag f -> S.member f (cdFlags c)
  PPath pe ->
    case cdCid c of
      Nothing -> False
      Just cid -> S.member cid (M.findWithDefault S.empty (renderPath pe) pathSets)
  PNum k op rhs ->
    let lhs = M.findWithDefault 0 (canonNumKey k) (cdNums c)
     in cmpNum op lhs rhs
  PStr k op rhs ->
    case M.lookup (canonNumKey k) (cdNums c) of
      Just numVal | Just rhsNum <- readDouble rhs, op `elem` [OpGt, OpGe, OpLt, OpLe] ->
        cmpNum op numVal rhsNum
      _ ->
        let lhs = M.findWithDefault "" (canonStrKey k) (cdStrs c)
         in cmpStr op lhs rhs
  where
    cmpNum OpEq a b = abs (a - b) < 1e-6
    cmpNum OpNe a b = abs (a - b) >= 1e-6
    cmpNum OpGt a b = a > b
    cmpNum OpGe a b = a >= b
    cmpNum OpLt a b = a < b
    cmpNum OpLe a b = a <= b
    cmpNum OpMatch a b = abs (a - b) < 1e-6

    cmpStr OpEq a b = T.toLower a == T.toLower b
    cmpStr OpNe a b = T.toLower a /= T.toLower b
    cmpStr OpMatch a b = T.toLower b `T.isInfixOf` T.toLower a
    cmpStr OpGt a b = T.toLower a > T.toLower b
    cmpStr OpGe a b = T.toLower a >= T.toLower b
    cmpStr OpLt a b = T.toLower a < T.toLower b
    cmpStr OpLe a b = T.toLower a <= T.toLower b

canonNumKey :: Text -> Text
canonNumKey = \case
  "retained" -> "dom"
  "dom-bytes" -> "dom"
  "dom_bytes" -> "dom"
  "bytes" -> "size"
  "shallow" -> "size"
  "alloc_pct" -> "alloc"
  "time_pct" -> "time"
  "leak_score" -> "score"
  "sync-lag" -> "sync_lag"
  other -> other

canonStrKey :: Text -> Text
canonStrKey = \case
  "mod" -> "module"
  "src" -> "source"
  "loc" -> "source"
  "label" -> "name"
  other -> other

groupCandidates :: Text -> [Cand] -> [Cand]
groupCandidates rawField cands =
  let fld = canonStrKey rawField
      byGroup =
        M.fromListWith
          (\(cnt1, b1, p1, sample) (cnt2, b2, p2, _) -> (cnt1 + cnt2, b1 + b2, p1 + p2, sample))
          [ ( let k = M.findWithDefault "(none)" fld (cdStrs c) in if T.null k then "(none)" else k
            , (1 :: Int, cdBytes c, qrPrimaryValue (cdRow c), c)
            )
          | c <- cands
          ]
   in [ let row0 = cdRow sample
            newRow =
              row0
                { qrTitle = grpKey
                , qrKindBadge = fmtInt cnt <> " items"
                , qrPrimaryMetric = if totB > 0 then fmtBytes totB else fmtFixed 1 totPrimary
                , qrPrimaryValue = if totB > 0 then fromIntegral totB else totPrimary
                , qrSecondary = "grouped by " <> fld
                }
         in sample
              { cdRow = newRow
              , cdNums = M.insert "count" (fromIntegral cnt) (M.insert "size" (fromIntegral totB) (M.insert "dom" totPrimary (cdNums sample)))
              , cdBytes = totB
              }
      | (grpKey, (cnt, totB, totPrimary, sample)) <- M.toList byGroup
      ]

sortCandidates :: Text -> Bool -> [Cand] -> [Cand]
sortCandidates rawField desc cands =
  let fld = canonNumKey rawField
      valOf c = M.findWithDefault (qrPrimaryValue (cdRow c)) fld (cdNums c)
   in if desc
        then sortOn (Down . valOf) cands
        else sortOn valOf cands

-- ---------------------------------------------------------------------------
-- Topological Pointer-Path Regex Evaluator

-- | Evaluate a pointer-path regular expression over a 'HeapSnapshot', returning
-- the set of target 'ClosureId's matched by the final path segment.
evalPathExpr :: HeapSnapshot -> PathExpr -> Set ClosureId
evalPathExpr snap (PathExpr seg0 steps) =
  case steps of
    [] -> expandFinalSeg snap rootSet Nothing (M.keysSet nodes) seg0
    _ ->
      let initSeeds =
            S.fromList
              [ cid
              | cid <- M.keys nodes
              , matchesAtomDisj snap rootSet Nothing seg0 cid
              ]
          initEnds = advanceRepetition snap rootSet initSeeds seg0
          (beforeLast, lastStep) = splitLast initEnds steps
          PathStep lastHop lastSeg = lastStep
          entryCandidates = stepHop snap lastHop lastSeg beforeLast
       in expandFinalSeg snap rootSet (psSlot lastSeg) entryCandidates lastSeg
  where
    nodes = snapNodes snap
    rootSet = S.fromList (snapRoots snap)

    splitLast cur = \case
      [lastSt] -> (cur, lastSt)
      PathStep hop seg : rest ->
        let seeds = stepHop snap hop seg cur
            ends = advanceRepetition snap rootSet seeds seg
         in splitLast ends rest
      [] -> (cur, PathStep HopDirect seg0)

-- | Given seeds that already match 1 instance of @seg@, advance @(psMinRep - 1)@
-- to @(psMaxRep - 1)@ additional consecutive instances of @seg@.
advanceRepetition :: HeapSnapshot -> Set ClosureId -> Set ClosureId -> PathSeg -> Set ClosureId
advanceRepetition snap rootSet seeds0 PathSeg {..}
  | S.null seeds0 = S.empty
  | psMinRep <= 1 && psMaxRep == Just 1 = seeds0
  | otherwise =
      let nodes = snapNodes snap
          stepMatching curSet =
            S.fromList
              [ k
              | u <- S.toList curSet
              , Just un <- [M.lookup u nodes]
              , k <- cnChildren un
              , matchesAtoms snap rootSet psAtoms k
              ]
          -- Advance mandatory (psMinRep - 1) steps
          atMin = iterate stepMatching seeds0 !! max 0 (psMinRep - 1)
       in case psMaxRep of
            Just hi ->
              let extra = max 0 (hi - max 1 psMinRep)
               in S.unions (take (extra + 1) (iterate stepMatching atMin))
            Nothing ->
              -- Fixed-point BFS through matching children
              let closure seen Empty = seen
                  closure seen (x :<| q) =
                    let kids =
                          [ k
                          | Just xn <- [M.lookup x nodes]
                          , k <- cnChildren xn
                          , not (S.member k seen)
                          , matchesAtoms snap rootSet psAtoms k
                          ]
                        seen' = foldl' (flip S.insert) seen kids
                     in closure seen' (foldl' (:|>) q kids)
               in closure atMin (Seq.fromList (S.toList atMin))

-- | Expand the final segment of a path expression. When @psMinRep > 1@ (e.g.
-- @MutVar ~> Thunk{3,}@), includes every entry head in @candidates@ that starts
-- a valid chain of length @>= psMinRep@ (plus the chain's members) so the spine
-- head carrying full dominated bytes ranks at the top!
expandFinalSeg :: HeapSnapshot -> Set ClosureId -> Maybe Int -> Set ClosureId -> PathSeg -> Set ClosureId
expandFinalSeg snap rootSet _mSlot candidates PathSeg {..} =
  let validStarts =
        S.filter (\cid -> matchesAtoms snap rootSet psAtoms cid) candidates
   in if psMinRep <= 1 && psMaxRep == Just 1
        then validStarts
        else
          let nodes = snapNodes snap
              -- Measure longest matching chain starting at u (bounded by maxRep or cycle)
              chainFrom u =
                let go seen cur !depth pathAcc =
                      let kids =
                            [ k
                            | Just cn <- [M.lookup cur nodes]
                            , k <- cnChildren cn
                            , not (S.member k seen)
                            , matchesAtoms snap rootSet psAtoms k
                            ]
                          canStop = depth >= psMinRep
                          hitCap = maybe False (depth >=) psMaxRep
                       in if hitCap || null kids
                            then if canStop then pathAcc else S.empty
                            else
                              let childChains = [go (S.insert k seen) k (depth + 1) (S.insert k pathAcc) | k <- take 4 kids]
                                  nonEmpty = filter (not . S.null) childChains
                               in if not (null nonEmpty)
                                    then S.unions nonEmpty
                                    else if canStop then pathAcc else S.empty
                 in go (S.singleton u) u 1 (S.singleton u)
           in S.unions [chainFrom u | u <- S.toList validStarts]

stepHop :: HeapSnapshot -> PathHop -> PathSeg -> Set ClosureId -> Set ClosureId
stepHop snap hop PathSeg {..} frontier
  | S.null frontier = S.empty
  | otherwise =
      let nodes = snapNodes snap
          slotMap = snapEdgeSlots snap
          slotOk p c = case psSlot of
            Nothing -> True
            Just want -> M.lookup (p, c) slotMap == Just want
          directKids u =
            [ k
            | Just un <- [M.lookup u nodes]
            , (idx, k) <- zip [0 ..] (cnChildren un)
            , case psSlot of
                Nothing -> True
                Just want -> M.findWithDefault idx (u, k) slotMap == want
            , slotOk u k || isNothingSlotMap slotMap
            ]
          isNothingSlotMap m = M.null m
       in case hop of
            HopDirect ->
              S.fromList [k | u <- S.toList frontier, k <- directKids u]
            HopTransitive ->
              let bfsT seen Empty = seen
                  bfsT seen (x :<| q) =
                    let ks = [k | Just xn <- [M.lookup x nodes], k <- cnChildren xn, not (S.member k seen)]
                        seen' = foldl' (flip S.insert) seen ks
                     in bfsT seen' (foldl' (:|>) q ks)
                  firstStep = [k | u <- S.toList frontier, k <- directKids u]
               in bfsT (S.fromList firstStep) (Seq.fromList firstStep)

matchesAtomDisj :: HeapSnapshot -> Set ClosureId -> Maybe Int -> PathSeg -> ClosureId -> Bool
matchesAtomDisj snap rootSet _ seg = matchesAtoms snap rootSet (psAtoms seg)

matchesAtoms :: HeapSnapshot -> Set ClosureId -> [NodeAtom] -> ClosureId -> Bool
matchesAtoms snap rootSet atoms cid =
  case M.lookup cid (snapNodes snap) of
    Nothing -> False
    Just nd -> any (matchOne nd) atoms
  where
    matchOne nd = \case
      AtomAny -> True
      AtomRoot -> S.member cid rootSet
      AtomKind k -> cnKind nd == k
      AtomText txt ->
        let q = T.toLower txt
         in q `T.isInfixOf` T.toLower (cnType nd)
              || q `T.isInfixOf` T.toLower (cnName nd)
              || maybe False (T.isInfixOf q . T.toLower) (cnSource nd)

-- ---------------------------------------------------------------------------
-- Domain Candidate Builders

candidatesFor :: QueryContext -> QueryDomain -> [Cand]
candidatesFor ctx = \case
  DomClosures -> closureCandidates ctx
  DomBands -> bandCandidates ctx
  DomBindings -> bindingCandidates ctx
  DomThreads -> threadCandidates ctx
  DomSpans -> spanCandidates ctx
  DomGCs -> gcCandidates ctx

closureCandidates :: QueryContext -> [Cand]
closureCandidates QueryContext {..} = case qcSnapshot of
  Nothing -> []
  Just snap ->
    let dt = fromMaybe (computeDominators snap) qcDominators
        skelSpineMap = M.fromList [(spHead sp, spDepth sp) | sp <- hsSpines (buildHeapSkeleton snap dt)]
     in [ let cid = cnId n
              domB = domRetained dt cid
              heldB = M.findWithDefault (cnSize n) cid (snapHeld snap)
              surv = S.member cid qcSurvivors
              caf = isCAF n
              kName = kindTag (cnKind n)
              flags =
                S.fromList $
                  [kName]
                    ++ ["thunk" | cnKind n == NThunk]
                    ++ ["caf" | caf]
                    ++ ["surviving" | surv]
                    ++ ["root" | cid `elem` snapRoots snap]
              nums =
                M.fromList
                  [ ("dom", fromIntegral domB)
                  , ("held", fromIntegral heldB)
                  , ("size", fromIntegral (cnSize n))
                  , ("survived", if surv then 2 else 0)
                  , ("spine", fromIntegral (M.findWithDefault 1 cid skelSpineMap))
                  ]
              strs =
                M.fromList
                  [ ("kind", kName)
                  , ("type", cnType n)
                  , ("name", cnName n)
                  , ("source", fromMaybe "" (cnSource n))
                  ]
              row =
                QueryRow
                  { qrRef = Just (RClosure qcSnapshotIndex cid)
                  , qrTitle = nodeTitle n
                  , qrKindBadge = cnType n
                  , qrPrimaryMetric = fmtBytes (fromIntegral domB) <> " dom"
                  , qrPrimaryValue = fromIntegral domB
                  , qrSecondary = fmtClosureId cid <> " · own " <> fmtBytes (fromIntegral (cnSize n)) <> (if surv then " · surviving" else "")
                  , qrSource = cnSource n
                  }
           in Cand row nums strs flags (Just cid) (fromIntegral (cnSize n))
        | n <- M.elems (snapNodes snap)
        ]
  where
    kindTag = \case
      NThunk -> "thunk"
      NFun -> "fun"
      NPap -> "pap"
      NConstr -> "constr"
      NIndirection -> "ind"
      NArray -> "array"
      NMutable -> "mutable"
      NThread -> "thread"
      NWeak -> "weak"
      NOther -> "other"

bandCandidates :: QueryContext -> [Cand]
bandCandidates QueryContext {..} = case qcHeap of
  Nothing -> []
  Just hp ->
    let susMap = M.fromList [(susBand s, s) | s <- leakSuspects hp]
     in [ let mSus = M.lookup idx susMap
              score = maybe 0 susScore mSus
              growth = maybe 1 susGrowth mSus
              mono = maybe 0 susMonotone mSus
              kStr = case bandKind b of
                KindThunk -> "thunk"
                KindFunction -> "fun"
                KindConstructor -> "constr"
                KindData -> "data"
                KindUnknown -> "unknown"
              srcTxt = fmap (renderSrcLoc . ipSrc) (bandProv b)
              modTxt = maybe "" ipModule (bandProv b)
              flags =
                S.fromList $
                  [kStr]
                    ++ ["thunk" | bandKind b == KindThunk]
                    ++ ["suspect" | isJust mSus]
                    ++ ["growing" | growth > 1.2]
              nums =
                M.fromList
                  [ ("peak", bandPeak b)
                  , ("size", bandPeak b)
                  , ("dom", bandPeak b)
                  , ("score", score)
                  , ("growth", growth)
                  , ("monotone", mono * 100)
                  ]
              strs =
                M.fromList
                  [ ("kind", kStr)
                  , ("name", bandTitle b)
                  , ("type", bandName b)
                  , ("module", modTxt)
                  , ("source", fromMaybe "" srcTxt)
                  ]
              row =
                QueryRow
                  { qrRef = Just (RBand (bandName b))
                  , qrTitle = bandTitle b
                  , qrKindBadge = kStr
                  , qrPrimaryMetric = fmtBytesD (bandPeak b)
                  , qrPrimaryValue = bandPeak b
                  , qrSecondary = maybe ("band " <> bandName b) (\s -> "leak score " <> fmtFixed 2 (susScore s) <> " · " <> fmtFixed 1 (susGrowth s) <> "x") mSus
                  , qrSource = srcTxt
                  }
           in Cand row nums strs flags Nothing (round (max 0 (bandPeak b)))
        | (idx, b) <- zip [0 ..] (V.toList (hpBands hp))
        ]

bindingCandidates :: QueryContext -> [Cand]
bindingCandidates QueryContext {..} =
  [ let flags =
          S.fromList $
            ["ww" | qbWorker qb]
              ++ ["boxed" | qbBoxes qb > 0]
              ++ ["recursive" | qbRecursive qb]
              ++ ["allocating" | qbLets qb > 0]
        nums =
          M.fromList
            [ ("time", qbTimePct qb)
            , ("alloc", qbAllocPct qb)
            , ("boxes", fromIntegral (qbBoxes qb))
            , ("lets", fromIntegral (qbLets qb))
            , ("unboxed", fromIntegral (qbUnboxedOps qb))
            , ("entries", fromIntegral (qbEntries qb))
            , ("ticky", fromIntegral (qbTickyBytes qb))
            , ("size", qbHeapBytes qb)
            ]
        strs =
          M.fromList
            [ ("module", qbModule qb)
            , ("name", qbKey qb)
            , ("source", fromMaybe "" (qbSource qb))
            ]
        primaryVal = if qbTimePct qb > 0 || qbAllocPct qb > 0 then qbTimePct qb + qbAllocPct qb else fromIntegral (qbBoxes qb * 10 + qbLets qb)
        primaryTxt
          | qbTimePct qb > 0 || qbAllocPct qb > 0 = fmtFixed 1 (qbTimePct qb) <> "% time · " <> fmtFixed 1 (qbAllocPct qb) <> "% alloc"
          | otherwise = tshow (qbBoxes qb) <> " box · " <> tshow (qbLets qb) <> " let"
        badge = if qbWorker qb then "W/W" else if qbBoxes qb > 0 then "boxed" else "clean"
        row =
          QueryRow
            { qrRef = Just (RBinding (qbModule qb) (qbKey qb))
            , qrTitle = qbModule qb <> "." <> qbKey qb
            , qrKindBadge = badge
            , qrPrimaryMetric = primaryTxt
            , qrPrimaryValue = primaryVal
            , qrSecondary = tshow (qbBoxes qb) <> " boxes · " <> tshow (qbLets qb) <> " STG lets" <> (if qbRecursive qb then " · rec" else "")
            , qrSource = qbSource qb
            }
     in Cand row nums strs flags Nothing (qbTickyBytes qb)
  | qb <- qcBindings
  ]

threadCandidates :: QueryContext -> [Cand]
threadCandidates QueryContext {..} = case qcTimeline of
  Nothing -> []
  Just tl ->
    [ let tr = tiTrack ti
          runnableNs :: Word64
          runnableNs =
            sum
              [ trEnd tr VU.! i - trStart tr VU.! i
              | i <- [0 .. trackLength tr - 1]
              , trKind tr VU.! i == kindRunnable
              , trEnd tr VU.! i > trStart tr VU.! i
              ]
          blockedNs = sum (M.elems (tiBlocked ti))
          topStop = case sortOn (Down . snd) (M.toList (tiBlocked ti)) of
            (code, _) : _ -> stopName code
            [] -> "none"
          flags =
            S.fromList $
              ["blocked" | blockedNs > 0]
                ++ ["starved" | runnableNs > 1000000]
          nums =
            M.fromList
              [ ("run", fromIntegral (tiRunTime ti))
              , ("blocked", fromIntegral blockedNs)
              , ("runnable", fromIntegral runnableNs)
              , ("wakeups", fromIntegral (VU.length (tiWakeups ti)))
              ]
          strs =
            M.fromList
              [ ("name", threadName ti)
              , ("stop", topStop)
              , ("id", tshow (tiId ti))
              ]
          row =
            QueryRow
              { qrRef = Just (RThread qcRunKey (tiId ti))
              , qrTitle = threadName ti
              , qrKindBadge = topStop
              , qrPrimaryMetric = fmtNs (fromIntegral blockedNs) <> " blocked"
              , qrPrimaryValue = fromIntegral (blockedNs + runnableNs)
              , qrSecondary = "run " <> fmtNs (fromIntegral (tiRunTime ti)) <> " · runnable wait " <> fmtNs (fromIntegral runnableNs)
              , qrSource = Nothing
              }
       in Cand row nums strs flags Nothing 0
    | ti <- M.elems (tlThreads tl)
    ]

spanCandidates :: QueryContext -> [Cand]
spanCandidates QueryContext {..} = case qcTimeline of
  Nothing -> []
  Just tl ->
    [ let dur = usEnd u - min (usEnd u) (usStart u)
          nums =
            M.fromList
              [ ("duration", fromIntegral dur)
              , ("size", fromIntegral (usAllocBytes u))
              , ("alloc_bytes", fromIntegral (usAllocBytes u))
              , ("thread", fromIntegral (usThread u))
              ]
          strs =
            M.fromList
              [ ("name", usName u)
              , ("thread", tshow (usThread u))
              ]
          row =
            QueryRow
              { qrRef = Just (RRange qcRunKey (usStart u) (usEnd u))
              , qrTitle = usName u
              , qrKindBadge = "thread " <> tshow (usThread u)
              , qrPrimaryMetric = fmtNs (fromIntegral dur)
              , qrPrimaryValue = fromIntegral dur
              , qrSecondary = "allocated " <> fmtBytes (usAllocBytes u)
              , qrSource = Nothing
              }
       in Cand row nums strs S.empty Nothing (usAllocBytes u)
    | u <- V.toList (tlUserSpans tl)
    ]

gcCandidates :: QueryContext -> [Cand]
gcCandidates QueryContext {..} = case qcTimeline of
  Nothing -> []
  Just tl ->
    let lagMap :: Map Int GCSyncLag
        lagMap = M.fromList [(slCollection l, l) | l <- V.toList (tlGCSyncLags tl)]
     in [ let pause = gcEnd g - min (gcEnd g) (gcStart g)
              syncLag = maybe 0 slSyncWaitNs (M.lookup idx lagMap)
              straggler = M.lookup idx lagMap >>= slStragglerThread
              flags = S.fromList (["major" | gcGen g >= 1] ++ ["straggler" | syncLag > 0])
              nums =
                M.fromList
                  [ ("pause", fromIntegral pause)
                  , ("sync_lag", fromIntegral syncLag)
                  , ("copied", fromIntegral (gcCopied g))
                  , ("gen", fromIntegral (gcGen g))
                  ]
              strs =
                M.fromList
                  [ ("kind", if gcGen g >= 1 then "major" else "minor")
                  , ("straggler", maybe "" tshow straggler)
                  ]
              row =
                QueryRow
                  { qrRef = Just (RCollection qcRunKey idx)
                  , qrTitle = "Collection #" <> tshow (idx + 1) <> " (gen " <> tshow (gcGen g) <> ")"
                  , qrKindBadge = if gcGen g >= 1 then "major" else "minor"
                  , qrPrimaryMetric = fmtNs (fromIntegral pause)
                  , qrPrimaryValue = fromIntegral pause
                  , qrSecondary = "copied " <> fmtBytes (gcCopied g) <> (if syncLag > 0 then " · STW sync wait " <> fmtNs (fromIntegral syncLag) else "")
                  , qrSource = fmap (\ (t :: Word32) -> "straggler thread " <> tshow t) straggler
                  }
           in Cand row nums strs flags Nothing (gcCopied g)
        | (idx, g) <- zip [0 ..] (V.toList (tlGCs tl))
        ]

-- ---------------------------------------------------------------------------
-- Markdown & JSON Formatters (for MCP & Export)

formatQueryMarkdown :: QueryResult -> Text
formatQueryMarkdown QueryResult {..} = MD.renderMD do
  MD.h1 ("Stronghead Query Results (" <> domainName qresDomain <> ")")
  MD.fieldCode "Query" qresQueryText
  MD.field "Total Matched" (fmtInt qresTotalMatched)
  if qresTotalBytes > 0
    then MD.field "Total Bytes" (fmtBytes qresTotalBytes)
    else pure ()
  MD.hr
  if null qresRows
    then MD.para "No entities matched the query."
    else
      MD.tableAligned
        [ (MD.AlignLeft, "Entity")
        , (MD.AlignLeft, "Kind")
        , (MD.AlignRight, "Primary Metric")
        , (MD.AlignLeft, "Detail")
        , (MD.AlignLeft, "Ref")
        ]
        [ [ MD.code (qrTitle r)
          , qrKindBadge r
          , qrPrimaryMetric r
          , qrSecondary r <> maybe "" (\s -> " (" <> s <> ")") (qrSource r)
          , maybe "-" (MD.code . refTarget) (qrRef r)
          ]
        | r <- qresRows
        ]

formatQueryJson :: QueryResult -> Text
formatQueryJson QueryResult {..} =
  T.concat
    [ "{\"domain\":\"" <> jsonEsc (domainName qresDomain) <> "\""
    , ",\"query\":\"" <> jsonEsc qresQueryText <> "\""
    , ",\"total_matched\":" <> tshow qresTotalMatched
    , ",\"total_bytes\":" <> tshow qresTotalBytes
    , ",\"rows\":["
    , T.intercalate "," (map rowJson qresRows)
    , "]}"
    ]
  where
    rowJson r =
      T.concat
        [ "{\"title\":\"" <> jsonEsc (qrTitle r) <> "\""
        , ",\"badge\":\"" <> jsonEsc (qrKindBadge r) <> "\""
        , ",\"metric\":\"" <> jsonEsc (qrPrimaryMetric r) <> "\""
        , ",\"value\":" <> T.pack (showFFloat (Just 4) (qrPrimaryValue r) "")
        , ",\"detail\":\"" <> jsonEsc (qrSecondary r) <> "\""
        , ",\"source\":" <> maybe "null" (\s -> "\"" <> jsonEsc s <> "\"") (qrSource r)
        , ",\"ref\":" <> maybe "null" (\rf -> "\"" <> jsonEsc (refTarget rf) <> "\"") (qrRef r)
        , "}"
        ]
    jsonEsc =
      T.replace "\"" "\\\""
        . T.replace "\n" "\\n"
        . T.replace "\r" ""
        . T.replace "\\" "\\\\"
