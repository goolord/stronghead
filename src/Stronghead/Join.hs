-- | The joins between artifacts: what makes a line of source, a binding, a
-- cost centre, a heap band and a closure the same thing in every panel.
--
-- Everything here is pure. Code-keyed joins hold across runs of one build:
-- cost centres and info tables are matched to source by their spans, and
-- info tables and ticky counters to STG binders by name. Each function says
-- how exact its join is.
--
-- The 'Annotation' is what a panel says about a span of source for the
-- annotated code view: a heat bar in one of its lanes, a gutter mark or a
-- trailing note, with the entity it came from.
module Stronghead.Join
  ( -- * Spans
    sameFile
  , spanWithin
  , spanOverlaps
    -- * Annotations
  , Lane (..)
  , laneName
  , Mark (..)
  , Shade (..)
  , Annotation (..)
  , annotationAt
  , lineValues
  , lineOwners
    -- * Cost centres
  , CostTotal (..)
  , costTotals
  , ccsFrames
  , profPath
    -- * Info tables and STG
  , TableName (..)
  , parseTableName
  , StgSites (..)
  , stgSites
  , siteOfTable
    -- * Ticky counters and STG
  , TickyName (..)
  , parseTickyName
  , tickyBinder
  , tickyUnique
  , tickyTopLevel
    -- * Heap bands and closures
  , bandTitle
  , closureMatchesBand
  , bandInfoTable
  , BandMatch (..)
  , bandMatchName
  , matchBand
  , SourceGroup (..)
  , closuresBySource
  ) where

import Data.Char (isDigit, isUpper)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (groupBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Word (Word64)
import Stronghead.Debug.Graph (ClosureId, ClosureNode (..), HeapSnapshot (..), NodeKind (..))
import Stronghead.Dump (CodeFile (..), TokKind (..), Token (..), lineTokens)
import Stronghead.HeapProfile (Band (..), BandKind (..), InfoProv (..))
import Stronghead.Prof (CostCentre (..), flattenCC)
import Stronghead.Ref (Ref)
import Stronghead.Source (SrcLoc (..), SrcSpan (..), parseSrcLoc, pathComponents, srcLocSpan)
import System.FilePath (takeFileName)

-- ---------------------------------------------------------------------------
-- Spans

-- | Whether two reported paths can name the same file: one's components are
-- a tail of the other's (@sample\\Main.hs@ and @/home/me/sample/Main.hs@).
-- Paths from one build agree exactly; this is for paths from two tools.
sameFile :: Text -> Text -> Bool
sameFile a b =
  let ca = reverse (pathComponents (T.unpack a))
      cb = reverse (pathComponents (T.unpack b))
   in not (null ca || null cb) && and (zipWith (==) ca cb)

-- | The first span lies inside the second (files not compared).
spanWithin :: SrcSpan -> SrcSpan -> Bool
spanWithin inner outer =
  (ssLine inner, ssCol inner) >= (ssLine outer, ssCol outer)
    && (ssEndLine inner, ssEndCol inner) <= (ssEndLine outer, ssEndCol outer)

-- | The spans share a character (files not compared).
spanOverlaps :: SrcSpan -> SrcSpan -> Bool
spanOverlaps a b =
  (ssLine a, ssCol a) <= (ssEndLine b, ssEndCol b)
    && (ssLine b, ssCol b) <= (ssEndLine a, ssEndCol a)

-- ---------------------------------------------------------------------------
-- Annotations

-- | A column of heat bars beside the code, one per kind of measurement.
data Lane
  = LaneTime
    -- ^ Share of time, from a @.prof@ file.
  | LaneAlloc
    -- ^ Share of allocation, from a @.prof@ file.
  | LaneHeap
    -- ^ Bytes live at the heap cursor, from an info-table profile (@-hi@).
  | LaneLive
    -- ^ Bytes in the latest ghc-debug snapshot.
  | LaneEntries
    -- ^ Entries, from a ticky build's counters.
  | LaneTicky
    -- ^ Bytes allocated, from a ticky build's counters.
  | LaneNote
    -- ^ No bar: only the trailing note (a binding's badges).
  | LaneMark !Mark
    -- ^ No bar: a gutter mark.
  deriving (Eq, Ord, Show)

laneName :: Lane -> Text
laneName = \case
  LaneTime -> "time"
  LaneAlloc -> "alloc"
  LaneHeap -> "heap"
  LaneLive -> "live"
  LaneEntries -> "entries"
  LaneTicky -> "ticky alloc"
  LaneNote -> "note"
  LaneMark _ -> "mark"

-- | Ordered by precedence: a breakpoint shows over the stop, the stop over
-- a candidate line.
data Mark = CandidateMark | StopMark | BreakMark
  deriving (Eq, Ord, Show)

-- | How a bar is coloured. Bars are ink; only thunks and survivors get a
-- signal colour.
data Shade = ShadeInk | ShadeThunk | ShadeSurvivor
  deriving (Eq, Ord, Show)

data Annotation = Annotation
  { anSpan :: !(Maybe SrcSpan)
    -- ^ Where in the source, as the artifact reported it.
  , anTable :: !(Maybe Text)
    -- ^ The info table it came from, for the STG join ('siteOfTable').
  , anName :: !(Maybe (Text, Text))
    -- ^ Module and name key, for joins by name when there is no span.
  , anLane :: !Lane
  , anValue :: !Double
    -- ^ What the bar measures, in the lane's unit.
  , anShade :: !Shade
  , anText :: !Text
    -- ^ The trailing note: "12.3% time".
  , anRef :: !(Maybe Ref)
    -- ^ The entity it came from, which clicking it reveals.
  }
  deriving (Eq, Show)

-- | An annotation at a span.
annotationAt :: SrcSpan -> Lane -> Double -> Text -> Maybe Ref -> Annotation
annotationAt sp lane v txt ref = Annotation (Just sp) Nothing Nothing lane v ShadeInk txt ref

-- | Values on line ranges (first and last line, inclusive) attributed to
-- lines: each line gets the values of the smallest ranges covering it, so a
-- cost centre's lines show its own cost and a nested one's lines show
-- theirs. Ranges of the same size on the same line add up.
lineValues :: [(Int, Int, Double)] -> IntMap Double
lineValues = IM.map sum . lineOwners

-- | 'lineValues', keeping what owns each line rather than adding it up.
lineOwners :: [(Int, Int, a)] -> IntMap [a]
lineOwners ranges = snd (foldl' step (IS.empty, IM.empty) groups)
  where
    size (a, b, _) = b - a
    groups = groupBy (\x y -> size x == size y) (sortOn size [(a, max a b, v) | (a, b, v) <- ranges])
    step (claimed, acc) grp =
      let own = [(l, [v]) | (a, b, v) <- grp, l <- [a .. b], not (IS.member l claimed)]
       in ( IS.union claimed (IS.fromList (map fst own))
          , IM.unionWith (++) acc (IM.fromListWith (flip (++)) own)
          )

-- ---------------------------------------------------------------------------
-- Cost centres

-- | One cost centre's costs summed over every stack it appears in.
data CostTotal = CostTotal
  { ctModule :: !Text
  , ctName :: !Text
  , ctLoc :: !SrcLoc
  , ctEntries :: !Integer
  , ctTime :: !Double
    -- ^ Individual %time, summed over its stacks.
  , ctAlloc :: !Double
  , ctInhTime :: !Double
    -- ^ Inherited %time of its outermost occurrences, so recursion does not
    -- count twice.
  , ctInhAlloc :: !Double
  }
  deriving (Eq, Show)

-- | Every cost centre of a @.prof@ tree, by module, name and source, with
-- its costs summed. Most expensive first.
costTotals :: [CostCentre] -> [CostTotal]
costTotals roots =
  sortOn (Down . ctTime) . M.elems $
    M.intersectionWith (\t (it, ia) -> t {ctInhTime = it, ctInhAlloc = ia}) individual inherited
  where
    key c = (ccModule c, ccName c, ccSrc c)
    individual = M.fromListWith merge [(key c, CostTotal (ccModule c) (ccName c) (ccLoc c) (ccEntries c) (ccTime c) (ccAlloc c) 0 0) | (_, c) <- flattenCC roots]
    inherited = M.fromListWith (\(a, b) (c, d) -> (a + c, b + d)) [(key c, (ccInhTime c, ccInhAlloc c)) | c <- concatMap (outermost []) roots]
    -- Inherited costs already include every nested occurrence of the same
    -- cost centre, so only the outermost of each nest counts.
    outermost seen c = [c | key c `notElem` seen] ++ concatMap (outermost (key c : seen)) (ccChildren c)
    merge a b =
      a
        { ctEntries = ctEntries a + ctEntries b
        , ctTime = ctTime a + ctTime b
        , ctAlloc = ctAlloc a + ctAlloc b
        }

-- | A cost-centre band's frames, innermost first: @(12)leaf/go/MAIN@ is
-- @leaf@, @go@, @MAIN@.
ccsFrames :: Text -> [Text]
ccsFrames name =
  let stripped = case T.breakOn ")" name of
        (pre, rest) | "(" `T.isPrefixOf` pre, not (T.null rest) -> T.drop 1 rest
        _ -> name
   in filter (not . T.null) (T.splitOn "/" stripped)

-- | The path through a @.prof@ tree a cost-centre stack names, root first:
-- an exact match on names, frame by frame. A heap profile truncates long
-- stacks, so a stack that runs out early still matches its prefix.
profPath :: [Text] -> [CostCentre] -> Maybe [CostCentre]
profPath framesInnermostFirst roots = case reverse framesInnermostFirst of
  [] -> Nothing
  fs -> go fs roots
  where
    go [] _ = Just []
    go (f : rest) level = listToMaybe [c : path | c <- level, ccName c == f, Just path <- [go rest (ccChildren c)]]

-- ---------------------------------------------------------------------------
-- Info tables and STG

-- | What an info table's symbol says about it.
data TableName
  = TableOfBinder !Text
    -- ^ @main2_sat_s7qA_info@: the STG binder @main2_sat_s7qA@ (V1).
  | TableOfCon !Text !Text !Int
    -- ^ @I#_Main_3_con_info@: constructor @I#@, module @Main@, site 3, from
    -- @-fdistinct-constructor-tables@.
  deriving (Eq, Show)

parseTableName :: Text -> Maybe TableName
parseTableName t
  | Just rest <- T.stripSuffix "_con_info" t =
      case reverse (T.splitOn "_" rest) of
        n : modu : con@(_ : _)
          | not (T.null n), T.all isDigit n, not (T.null modu) ->
              Just (TableOfCon (T.intercalate "_" (reverse con)) modu (read (T.unpack n)))
        _ -> Nothing
  | Just binder <- T.stripSuffix "_info" t, not (T.null binder) = Just (TableOfBinder binder)
  | otherwise = Nothing

-- | Where an STG dump defines closures: every binder by the line it is
-- bound on, and every numbered constructor application
-- (@3 GHC.Internal.Types.I#! [x]@ under @-fdistinct-constructor-tables@)
-- by constructor and number.
data StgSites = StgSites
  { sgModule :: !Text
  , sgBinders :: !(Map Text Int)
    -- ^ 0-based line of each binder's binding.
  , sgCons :: !(Map (Text, Int) Int)
  , sgNotes :: ![(SrcSpan, Int)]
    -- ^ @src<file:line:col>@ notes, and the line each is on.
  }
  deriving (Eq, Show)

-- | Scan an STG dump once. A binder's line is the first line it starts:
-- STG binds before it uses, in @let@s and in @case@ alternatives alike.
stgSites :: CodeFile -> StgSites
stgSites cf =
  StgSites
    { sgModule = cfModule cf
    , sgBinders = M.fromListWith min [(tkText t, i) | (i, t : _) <- lineToks, tkKind t == TkIdent]
    , sgCons = M.fromListWith min [((lastSeg (tkText c), read (T.unpack (tkText n))), i) | (i, ts) <- lineToks, (n, c) <- numberedCons ts]
    , sgNotes = [(sp, i) | i <- [0 .. V.length (cfLines cf) - 1], sp <- notes (cfLines cf V.! i)]
    }
  where
    lineToks = [(i, filter ((/= TkSpace) . tkKind) (lineTokens cf i)) | i <- [0 .. V.length (cfLines cf) - 1]]
    numberedCons ts = [(n, c) | (n, c) <- zip ts (drop 1 ts), tkKind n == TkNumber, T.all isDigit (tkText n), tkKind c == TkCon]
    lastSeg w = case T.breakOnEnd "." w of
      (_, s) | not (T.null s), isUpper (T.head s) || T.head s == '(' -> s
      _ -> w
    notes l = case T.breakOn "src<" l of
      (_, rest) | T.null rest -> []
      (_, rest) ->
        let body = T.takeWhile (/= '>') (T.drop 4 rest)
         in maybe [] pure (srcLocSpan (parseSrcLoc body)) ++ notes (T.drop (4 + T.length body) rest)

-- | The STG line an info table was allocated from: its binder (exact) or
-- its numbered constructor site (exact, in the table's own module), else
-- the @src@ note at the table's span (by span).
siteOfTable :: StgSites -> Text -> Maybe SrcSpan -> Maybe Int
siteOfTable sites table msp = case parseTableName table of
  Just (TableOfBinder b) | Just l <- M.lookup b (sgBinders sites) -> Just l
  Just (TableOfCon con modu n) | modu == sgModule sites, Just l <- M.lookup (con, n) (sgCons sites) -> Just l
  _ -> do
    sp <- msp
    listToMaybe [l | (s, l) <- sgNotes sites, sameFile (ssFile s) (ssFile sp), s {ssFile = ""} == sp {ssFile = ""}]

-- ---------------------------------------------------------------------------
-- Ticky counters and STG

-- | What a ticky counter's name says (V5). The RTS names each counter
-- after its closure's binder, as the STG dump prints it once the unique
-- is put back:
--
-- * @main2_sat_s7qL{v} (Main) (fun) in s7qM@: the closure @main2_sat_s7qL@
--   of @Main@, a function, defined inside the closure with unique @s7qM@;
-- * @go3{v r9BH} (Stronghead.Agent) (fun)@: the top-level @go3_r9BH@;
-- * @Main.main2{v r7mA} (fun,se)@: the exported @Main.main2@.
data TickyName = TickyName
  { tnName :: !Text
    -- ^ As written before the braces.
  , tnUnique :: !(Maybe Text)
    -- ^ The unique in the braces, when the name does not carry its own.
  , tnModule :: !Text
  , tnFlags :: ![Text]
    -- ^ @fun@, @thk@, @se@ (single entry) and the like.
  , tnParent :: !(Maybe Text)
    -- ^ The unique of the closure it is defined in.
  }
  deriving (Eq, Show)

parseTickyName :: Text -> Maybe TickyName
parseTickyName raw = do
  let (name, open) = T.breakOn "{" (T.strip raw)
      (braces, close) = T.breakOn "}" (T.drop 1 open)
  if T.null name || T.null close then Nothing else Just ()
  unique <- case T.words braces of
    ["v"] -> Just Nothing
    ["v", u] -> Just (Just u)
    _ -> Nothing
  let (groups, rest) = parenGroups (T.drop 1 close)
      (mods, flagGroups) = span (maybe False (isUpper . fst) . T.uncons) groups
      modName = case mods of
        m : _ -> m
        [] -> maybe "" fst (qualifierOf name)
  pure
    TickyName
      { tnName = name
      , tnUnique = unique
      , tnModule = modName
      , tnFlags = concatMap (map T.strip . T.splitOn ",") flagGroups
      , tnParent = T.strip <$> T.stripPrefix "in " rest
      }
  where
    parenGroups t = case T.stripPrefix "(" (T.stripStart t) of
      Just r ->
        let (g, r') = T.breakOn ")" r
            (gs, rest) = parenGroups (T.drop 1 r')
         in (g : gs, rest)
      Nothing -> ([], T.strip t)

-- | A qualified name's module and the rest: @Main.main2@ is @Main@ and
-- @main2@. The top-level @:Main.main@ GHC adds counts too.
qualifierOf :: Text -> Maybe (Text, Text)
qualifierOf n =
  let bare = T.dropWhile (== ':') n
      (pre, base) = T.breakOnEnd "." bare
   in case T.uncons pre of
        Just (c, _) | isUpper c, not (T.null base) -> Just (T.dropEnd 1 pre, base)
        _ -> Nothing

-- | The STG binder a counter belongs to: the name with its unique put
-- back, unless it is qualified (exported) or carries the unique already.
tickyBinder :: TickyName -> Text
tickyBinder tn = case tnUnique tn of
  Just u | Nothing <- qualifierOf (tnName tn) -> tnName tn <> "_" <> u
  _ -> tnName tn

-- | The counter's own unique, which other counters name as their parent:
-- the braces' one, or else the binder's suffix (@main2_sat_s7qL@ is
-- @s7qL@).
tickyUnique :: TickyName -> Maybe Text
tickyUnique tn = case tnUnique tn of
  Just u -> Just u
  Nothing -> case T.breakOnEnd "_" (tnName tn) of
    (pre, u) | not (T.null pre), not (T.null u) -> Just u
    _ -> Nothing

-- | The top-level closure a counter is nested in, following parents
-- (looked up by unique) as far as they go.
tickyTopLevel :: (Text -> Maybe TickyName) -> TickyName -> TickyName
tickyTopLevel lookupUnique = go (64 :: Int)
  where
    go 0 tn = tn
    go n tn = maybe tn (go (n - 1)) (tnParent tn >>= lookupUnique)

-- ---------------------------------------------------------------------------
-- Heap bands and closures

-- | A band's name for display. An info-table band (@-hi@) is named by an
-- address; its provenance reads better: @thunk in main2 (Main.hs:82)@.
bandTitle :: Band -> Text
bandTitle b = case bandProv b of
  Nothing -> bandName b
  Just ip ->
    let what = case parseTableName (ipTableName ip) of
          Just (TableOfCon con _ _) -> con
          _ -> case ipKind ip of
            KindThunk -> "thunk"
            KindFunction -> "function"
            KindConstructor -> "data"
            KindData -> "primitive"
            KindUnknown -> "closure"
        inLabel = if T.null (ipLabel ip) then "" else " in " <> ipLabel ip
        at = case srcLocSpan (ipSrc ip) of
          Just sp -> " (" <> T.pack (takeFileName (map slash (T.unpack (ssFile sp)))) <> ":" <> T.pack (show (ssLine sp)) <> ")"
          Nothing -> if T.null (ipModule ip) then "" else " (" <> ipModule ip <> ")"
     in what <> inLabel <> at
  where
    slash c = if c == '\\' then '/' else c

-- | Whether a closure from a ghc-debug snapshot belongs to a heap band:
-- by source span for an info-table band (exact for its code, though not
-- its info table), else by closure type (@-hT@) or by constructor or
-- description name (@-hd@).
closureMatchesBand :: Band -> ClosureNode -> Bool
closureMatchesBand b n = case bandProv b of
  Just ip -> case (srcLocSpan (ipSrc ip), cnSource n >>= srcLocSpan . parseSrcLoc) of
    (Just a, Just c) -> sameFile (ssFile a) (ssFile c) && a {ssFile = ""} == c {ssFile = ""}
    _ -> not (T.null (ipLabel ip)) && cnName n == ipLabel ip
  Nothing -> cnType n == bandName b || cnName n == bandName b || stripped == cnName n
  where
    -- -hd names a constructor with its module: "Main.Leaf" or "Leaf".
    stripped = snd (T.breakOnEnd "." (bandName b))

-- | An info-table band's table: @-hi@ names its bands by the address.
bandInfoTable :: Band -> Maybe Word64
bandInfoTable b = case T.stripPrefix "0x" (bandName b) of
  Just h | Right (n, "") <- TR.hexadecimal h -> Just n
  _ -> Nothing

-- | How a band was matched to closures.
data BandMatch = ByInfoTable | BySource | ByName
  deriving (Eq, Show)

bandMatchName :: BandMatch -> Text
bandMatchName = \case
  ByInfoTable -> "exactly, by info table"
  BySource -> "by source"
  ByName -> "by closure type or name"

-- | The closures a band stands for. Exactly, by info-table address, when
-- the snapshot carries its closures' info pointers (D4) and the band's
-- table is among them: addresses hold within one run, so a band from
-- another run finds none and falls back. Otherwise 'closureMatchesBand'.
matchBand :: Band -> [ClosureNode] -> (BandMatch, [ClosureNode])
matchBand b ns = case bandInfoTable b of
  Just a | exact@(_ : _) <- [n | n <- ns, cnInfo n == Just a] -> (ByInfoTable, exact)
  _ -> (if isJust (bandProv b) then BySource else ByName, filter (closureMatchesBand b) ns)

-- | The closures of a snapshot that share a source location.
data SourceGroup = SourceGroup
  { sgSpan :: !SrcSpan
  , sgClosures :: !Int
  , sgThunks :: !Int
  , sgBytes :: !Int
  , sgSurvivors :: !Int
    -- ^ Thunks in a group that survived from earlier snapshots.
  , sgSample :: !ClosureId
    -- ^ The biggest closure, to reveal.
  }
  deriving (Eq, Show)

-- | Group a snapshot's closures by their source (built with
-- @-finfo-table-map@), given the surviving thunks. Biggest first.
closuresBySource :: HeapSnapshot -> Map ClosureId Int -> [SourceGroup]
closuresBySource snap marked =
  sortOn (Down . sgBytes) . M.elems $
    M.fromListWith merge
      [ (sp, SourceGroup sp 1 (fromEnum thunk) (cnSize n) (fromEnum (M.member (cnId n) marked)) (cnId n))
      | n <- M.elems (snapNodes snap)
      , Just sp <- [cnSource n >>= srcLocSpan . parseSrcLoc]
      , let thunk = cnKind n == NThunk
      ]
  where
    sizeOf i = maybe 0 cnSize (M.lookup i (snapNodes snap))
    merge a b =
      SourceGroup
        { sgSpan = sgSpan a
        , sgClosures = sgClosures a + sgClosures b
        , sgThunks = sgThunks a + sgThunks b
        , sgBytes = sgBytes a + sgBytes b
        , sgSurvivors = sgSurvivors a + sgSurvivors b
        , sgSample = if sizeOf (sgSample a) >= sizeOf (sgSample b) then sgSample a else sgSample b
        }
