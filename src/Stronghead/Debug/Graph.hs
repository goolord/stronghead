-- | The live closure graph a ghc-debug snapshot yields, and the analyses run
-- over it: shortest retainer paths from the GC roots, the CAF or root each
-- closure hangs from, the bytes each closure holds, thunks that survive
-- snapshot after snapshot, and each thread's stack of frames.
--
-- Nothing here talks to ghc-debug, so it builds (and is tested) whether or
-- not the @ghc-debug@ flag is on; "Stronghead.Debug" fills it in.
module Stronghead.Debug.Graph
  ( -- * Targets
    DebugTarget (..)
  , targetText
  , parseHostPort
  , SnapshotOptions (..)
  , defaultSnapshotOptions
    -- * The closure graph
  , ClosureId
  , fmtClosureId
  , NodeKind (..)
  , kindOfClosureType
  , ClosureNode (..)
  , StackFrame (..)
  , frameTitle
  , threadStack
  , threadOf
  , RootKind (..)
  , HeapSnapshot (..)
  , mkSnapshot
  , isCAF
    -- * Retainer paths
  , retainerPath
  , anchorOf
  , describePath
  , nodeTitle
    -- * Thunks across snapshots
  , ThunkKey (..)
  , ThunkGroup (..)
  , Survivor (..)
  , survivingThunks
  , survivorIds
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Word (Word16, Word32, Word64)
import Numeric (showHex)

-- | A process to snapshot: one running ghc-debug-stub, listening on a Unix
-- socket ('GHC.Debug.Stub.withGhcDebug') or TCP ('withGhcDebugTCP'), or a
-- snapshot file saved earlier with ghc-debug's @makeSnapshot@.
data DebugTarget
  = DebugUnix FilePath
  | DebugTCP String Word16
  | DebugSnapshotFile FilePath
  deriving (Eq, Show)

targetText :: DebugTarget -> Text
targetText = \case
  DebugUnix p -> T.pack p
  DebugTCP h p -> T.pack h <> ":" <> T.pack (show p)
  DebugSnapshotFile p -> T.pack p

-- | @host:port@, @[v6]:port@ or a bare @port@ (meaning 127.0.0.1).
parseHostPort :: Text -> Maybe (String, Word16)
parseHostPort raw =
  case T.breakOnEnd ":" (T.strip raw) of
    ("", p) -> ("127.0.0.1",) <$> port p
    (h, p) ->
      let host = T.dropEnd 1 h
          host' = if "[" `T.isPrefixOf` host && "]" `T.isSuffixOf` host then T.drop 1 (T.dropEnd 1 host) else host
       in if T.null host' then Nothing else (T.unpack host',) <$> port p
  where
    port p = case TR.decimal p of
      Right (n, "") | n > 0 && n < (65536 :: Integer) -> Just (fromIntegral n)
      _ -> Nothing

data SnapshotOptions = SnapshotOptions
  { soMaxClosures :: Int
    -- ^ Stop the traversal after this many closures.
  , soFollowSrt :: Bool
    -- ^ Follow static reference tables: functions and stack frames keep the
    -- CAFs their code refers to alive this way.
  }
  deriving (Eq, Show)

defaultSnapshotOptions :: SnapshotOptions
defaultSnapshotOptions = SnapshotOptions {soMaxClosures = 200000, soFollowSrt = True}

-- | A closure's untagged address at the moment of the snapshot. Addresses
-- are only stable within one snapshot: the copying collector moves objects
-- between snapshots.
type ClosureId = Word64

fmtClosureId :: ClosureId -> Text
fmtClosureId i = T.pack ("0x" <> showHex i "")

data NodeKind
  = NThunk
    -- ^ THUNK*, AP, AP_STACK and selector thunks: suspended computations.
  | NFun
  | NPap
  | NConstr
  | NIndirection
    -- ^ IND, IND_STATIC (an evaluated CAF) and BLACKHOLE.
  | NArray
  | NMutable
    -- ^ MUT_VAR, MVAR, TVAR.
  | NThread
    -- ^ TSO and STACK.
  | NWeak
  | NOther
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Classify a closure type as the RTS names it (@THUNK_2_0@, @IND_STATIC@).
kindOfClosureType :: Text -> NodeKind
kindOfClosureType t
  | "THUNK" `T.isPrefixOf` t || t `elem` ["AP", "AP_STACK"] = NThunk
  | "FUN" `T.isPrefixOf` t = NFun
  | t == "PAP" = NPap
  | "CONSTR" `T.isPrefixOf` t = NConstr
  | t `elem` ["IND", "IND_STATIC", "BLACKHOLE"] = NIndirection
  | t == "ARR_WORDS" || "MUT_ARR" `T.isPrefixOf` t || "SMALL_MUT_ARR" `T.isPrefixOf` t = NArray
  | "MUT_VAR" `T.isPrefixOf` t || "MVAR" `T.isPrefixOf` t || t == "TVAR" = NMutable
  | t `elem` ["TSO", "STACK"] = NThread
  | t == "WEAK" = NWeak
  | otherwise = NOther

data ClosureNode = ClosureNode
  { cnId :: ClosureId
  , cnType :: Text
    -- ^ RTS closure type, e.g. @THUNK_1_0@.
  , cnKind :: NodeKind
  , cnName :: Text
    -- ^ Constructor name, or the info table's source name when the target
    -- was built with @-finfo-table-map@, else the closure type.
  , cnSource :: Maybe Text
    -- ^ Source span from the info-table map.
  , cnSize :: Int
    -- ^ Bytes, header included.
  , cnChildren :: [ClosureId]
    -- ^ Pointer fields, SRT entries and stack-frame pointers, in order.
  , cnInfo :: Maybe Word64
    -- ^ Its info table's address, for the exact join to an info-table
    -- heap band (@-hi@) of the same run.
  , cnFrames :: [StackFrame]
    -- ^ For a STACK, its frames, the top (most recent) first.
  }
  deriving (Eq, Show)

-- | One frame of a thread's stack: the code it returns to.
data StackFrame = StackFrame
  { sfType :: Text
    -- ^ The frame's closure type: @RET_SMALL@, @UPDATE_FRAME@,
    -- @CATCH_FRAME@, @UNDERFLOW_FRAME@, @STOP_FRAME@ and so on.
  , sfName :: Text
    -- ^ The return point's name from the info-table map, if built with
    -- @-finfo-table-map@.
  , sfSource :: Maybe Text
    -- ^ Its source span, from the same map.
  , sfInfo :: Word64
  , sfPointers :: [ClosureId]
    -- ^ The closures the frame keeps alive.
  }
  deriving (Eq, Show)

-- | What a frame is doing, in words.
frameTitle :: StackFrame -> Text
frameTitle f = case sfType f of
  "UPDATE_FRAME" -> "evaluating a thunk" <> named
  "CATCH_FRAME" -> "inside catch" <> named
  "CATCH_RETRY_FRAME" -> "inside orElse" <> named
  "CATCH_STM_FRAME" -> "inside catchSTM" <> named
  "ATOMICALLY_FRAME" -> "inside atomically" <> named
  "UNDERFLOW_FRAME" -> "continued in an older stack chunk"
  "STOP_FRAME" -> "bottom of the stack"
  _
    | T.null (sfName f) -> "return frame (" <> sfType f <> ")"
    | otherwise -> "returns to " <> sfName f
  where
    named = if T.null (sfName f) then "" else " (" <> sfName f <> ")"

-- | A thread's stack at the snapshot, top frame first: the frames of the
-- STACK its TSO points to, continued through underflow frames into older
-- chunks. Empty when the closure is not a TSO or its stack was not
-- reached.
threadStack :: HeapSnapshot -> ClosureId -> [StackFrame]
threadStack snap tso = case M.lookup tso nodes of
  Just n | cnType n == "TSO" -> go S.empty (stackIn (cnChildren n))
  _ -> []
  where
    nodes = snapNodes snap
    stackIn ids = listToMaybe [i | i <- ids, Just c <- [M.lookup i nodes], cnType c == "STACK"]
    go _ Nothing = []
    go seen (Just s)
      | S.member s seen = []
      | otherwise = case M.lookup s nodes of
          Nothing -> []
          Just st ->
            let fs = cnFrames st
                older = stackIn [p | f <- fs, sfType f == "UNDERFLOW_FRAME", p <- sfPointers f]
             in fs ++ go (S.insert s seen) older

-- | The thread a TSO is: ghc-debug names it @thread N@.
threadOf :: ClosureNode -> Maybe Word32
threadOf n = case T.stripPrefix "thread " (cnName n) of
  Just t | cnType n == "TSO", Right (tid, "") <- TR.decimal t -> Just tid
  _ -> Nothing

data RootKind
  = RootThread
  | RootOther
    -- ^ Stable pointers, stable names and the other miscellaneous roots.
  deriving (Eq, Ord, Show)

-- | A CAF: a top-level thunk, evaluated (@IND_STATIC@) or not
-- (@THUNK_STATIC@).
isCAF :: ClosureNode -> Bool
isCAF n = cnType n `elem` ["IND_STATIC", "THUNK_STATIC"]

data HeapSnapshot = HeapSnapshot
  { snapLabel :: Text
    -- ^ Where it came from (socket, host:port or file) and when.
  , snapRoots :: [ClosureId]
  , snapNodes :: Map ClosureId ClosureNode
  , snapTruncated :: Bool
    -- ^ The traversal stopped at its closure limit; some closures reachable
    -- from the roots are missing.
  , snapParent :: Map ClosureId ClosureId
    -- ^ Breadth-first retainer: the closure through which each closure was
    -- first reached from the roots. Following it gives a shortest path.
  , snapHeld :: Map ClosureId Int
    -- ^ Bytes in each closure's subtree of the breadth-first tree: what it
    -- holds once every closure is charged to its first retainer.
  , snapThunkKeys :: Map ClosureId ThunkKey
    -- ^ The key of every reachable thunk.
  , snapThunks :: Map ThunkKey ThunkGroup
  , snapBytes :: Int
  , snapBiggest :: [ClosureId]
    -- ^ The closures holding the most bytes, roots and threads excluded.
  , snapRootsByHeld :: [ClosureId]
  }

-- | Index the closures and run the per-snapshot analyses. Children missing
-- from the map (past the traversal limit) are ignored.
mkSnapshot :: Text -> Bool -> [ClosureId] -> [ClosureNode] -> HeapSnapshot
mkSnapshot snapLabel snapTruncated roots nodes =
  let snapNodes = M.fromList [(cnId n, n) | n <- nodes]
      snapRoots = uniq (filter (`M.member` snapNodes) roots)
      (snapParent, order) = bfs snapNodes snapRoots
      snapHeld = held snapNodes snapParent order
      snapThunkKeys = thunkKeys snapNodes snapParent order
      snapThunks = thunkGroups snapNodes snapThunkKeys
      snapBytes = sum (map cnSize nodes)
      byHeld = sortOn (Down . heldOf)
      heldOf i = M.findWithDefault 0 i snapHeld
      rootSet = S.fromList snapRoots
      snapBiggest =
        forceList . take 200 . byHeld $
          [cnId n | n <- M.elems snapNodes, cnKind n /= NThread, not (S.member (cnId n) rootSet), M.member (cnId n) snapParent]
      snapRootsByHeld = forceList (byHeld snapRoots)
   in HeapSnapshot {..}
  where
    uniq = go S.empty
      where
        go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs
    forceList xs = foldr seq () xs `seq` xs

-- | Breadth-first from the roots: each closure's first retainer, and the
-- visit order.
bfs :: Map ClosureId ClosureNode -> [ClosureId] -> (Map ClosureId ClosureId, [ClosureId])
bfs nodes roots = go (Seq.fromList roots) (S.fromList roots) M.empty []
  where
    go Empty _ parent acc = (parent, reverse acc)
    go (x :<| queue) seen parent acc =
      let kids = [k | k <- maybe [] cnChildren (M.lookup x nodes), M.member k nodes, not (S.member k seen)]
          kids' = S.toList (S.fromList kids)
          seen' = foldl' (flip S.insert) seen kids'
          parent' = foldl' (\m k -> M.insert k x m) parent kids'
       in go (foldl' (:|>) queue (dedupe kids)) seen' parent' (x : acc)
    dedupe = go' S.empty
      where
        go' _ [] = []
        go' s (k : ks) = if S.member k s then go' s ks else k : go' (S.insert k s) ks

-- | Subtree sizes in the breadth-first tree, children before parents.
held :: Map ClosureId ClosureNode -> Map ClosureId ClosureId -> [ClosureId] -> Map ClosureId Int
held nodes parent order = foldl' step own (reverse order)
  where
    own = M.map cnSize nodes
    step m x = case M.lookup x parent of
      Just p -> M.insertWith (+) p (M.findWithDefault 0 x m) m
      Nothing -> m

-- | The shortest retainer path to a closure, root first, the closure last.
-- Empty when the closure was not reached.
retainerPath :: HeapSnapshot -> ClosureId -> [ClosureNode]
retainerPath snap = go S.empty []
  where
    go seen acc i
      | S.member i seen = acc
      | otherwise = case M.lookup i (snapNodes snap) of
          Nothing -> acc
          Just n -> case M.lookup i (snapParent snap) of
            Just p -> go (S.insert i seen) (n : acc) p
            Nothing
              | i `elem` snapRoots snap -> n : acc
              | otherwise -> []

-- | What a path hangs from: the CAF nearest the closure, or the root.
anchorOf :: [ClosureNode] -> Maybe ClosureNode
anchorOf path = case filter isCAF (reverse (dropLast path)) of
  c : _ -> Just c
  [] -> case path of
    r : _ : _ -> Just r
    _ -> Nothing
  where
    dropLast = reverse . drop 1 . reverse

-- | A short name: kind and name, with the source span when known.
nodeTitle :: ClosureNode -> Text
nodeTitle n =
  kindWord <> cnName n <> maybe "" (\s -> " (" <> s <> ")") (cnSource n)
  where
    kindWord
      | isCAF n = "CAF "
      | otherwise = case cnKind n of
          NThunk -> "thunk "
          NFun -> "function "
          NPap -> "partial application "
          NThread -> ""
          _ -> ""

-- | "closure X held by thunk Y, rooted at CAF Z": the closure, its nearest
-- holder (preferring the nearest thunk, which is usually the culprit), and
-- what the path hangs from.
describePath :: [ClosureNode] -> Text
describePath = \case
  [] -> "Not reachable from the roots in this snapshot."
  [r] -> nodeTitle r <> " is a GC root."
  path -> case reverse path of
    [] -> ""
    target : holders ->
      let holder = case (filter (\n -> cnKind n == NThunk && not (isCAF n)) holders, holders) of
            (t : _, _) -> t
            ([], h : _) -> h
            ([], []) -> target
          rooted = case anchorOf path of
            Just a
              | cnId a /= cnId holder -> ", rooted at " <> (if isCAF a then "" else "root ") <> nodeTitle a
            _ -> ""
       in nodeTitle target <> " held by " <> nodeTitle holder <> rooted <> " (" <> T.pack (show (length holders)) <> " steps from the roots)"

-- ---------------------------------------------------------------------------
-- Thunks across snapshots

-- | What makes two thunks in different snapshots "the same": their code
-- (name and source span) and the shape of the path that retains them,
-- starting at the anchor, with runs of the same closure collapsed so a
-- growing chain keeps one key. Addresses cannot be used: objects move.
data ThunkKey = ThunkKey
  { tkName :: Text
  , tkSource :: Maybe Text
  , tkPath :: [Text]
  }
  deriving (Eq, Ord, Show)

data ThunkGroup = ThunkGroup
  { tgCount :: Int
  , tgBytes :: Int
  , tgSample :: [ClosureId]
    -- ^ A few members, to browse to.
  }
  deriving (Eq, Show)

-- | Where each reachable closure's path stands, for keying the thunks below
-- it: the anchor (nearest CAF, else the root) and the last few distinct
-- closures since it, most recent first.
data PathCtx = PathCtx !ClosureId ![Text]

-- | Every reachable thunk's key, computed in one pass down the
-- breadth-first tree (parents come before children in the visit order).
thunkKeys :: Map ClosureId ClosureNode -> Map ClosureId ClosureId -> [ClosureId] -> Map ClosureId ThunkKey
thunkKeys nodes parent order = snd (foldl' step (M.empty, M.empty) order)
  where
    step (ctxs, keys) x = case M.lookup x nodes of
      Nothing -> (ctxs, keys)
      Just n ->
        let ctx = case M.lookup x parent >>= \p -> (,) <$> M.lookup p nodes <*> M.lookup p ctxs of
              Nothing -> PathCtx x []
              Just (p, PathCtx a recent)
                | isCAF p -> PathCtx (cnId p) (push p [])
                | otherwise -> PathCtx a (push p recent)
            PathCtx _ recent' = ctx
            keys'
              | cnKind n == NThunk && not (isCAF n) = M.insert x (ThunkKey (cnName n) (cnSource n) (reverse recent')) keys
              | otherwise = keys
         in (M.insert x ctx ctxs, keys')
    push p recent
      | cnKind p == NThread = recent
      | (l : _) <- recent, l == label p = recent
      | otherwise = take 4 (label p : recent)
    label p = cnName p <> maybe "" ("@" <>) (cnSource p)

thunkGroups :: Map ClosureId ClosureNode -> Map ClosureId ThunkKey -> Map ThunkKey ThunkGroup
thunkGroups nodes keys =
  M.fromListWith
    merge
    [(k, ThunkGroup 1 (maybe 0 cnSize (M.lookup i nodes)) [i]) | (i, k) <- M.toList keys]
  where
    merge (ThunkGroup a b xs) (ThunkGroup c d ys) = ThunkGroup (a + c) (b + d) (take 8 (xs ++ ys))

data Survivor = Survivor
  { svKey :: ThunkKey
  , svCounts :: [Int]
    -- ^ Thunks with this key in each snapshot, oldest first.
  , svBytes :: [Int]
  , svSurvived :: Int
    -- ^ Consecutive snapshots, ending with the latest, that had the key.
  , svGrowing :: Bool
    -- ^ The count never fell across those snapshots and ended higher.
  , svSample :: [ClosureId]
    -- ^ Members in the latest snapshot.
  }
  deriving (Eq, Show)

-- | Thunk groups of the latest snapshot that were also present in the one
-- before (and possibly earlier), worst first. Snapshots are given oldest
-- first. Thunks still unevaluated after the program ran on between two
-- pauses are what a lazy accumulator leaves behind.
survivingThunks :: [HeapSnapshot] -> [Survivor]
survivingThunks snaps = case reverse snaps of
  [] -> []
  latest : _ ->
    sortOn
      (\s -> (Down (svGrowing s), Down (svSurvived s), Down (take 1 (reverse (svBytes s)))))
      (mapMaybe survivor (M.toList (snapThunks latest)))
  where
    survivor (k, g) =
      let groups = map (M.lookup k . snapThunks) snaps
          counts = map (maybe 0 tgCount) groups
          bytes = map (maybe 0 tgBytes) groups
          run = takeWhile (> 0) (reverse counts)
          streak = length run
          growing = streak >= 2 && and (zipWith (>=) run (drop 1 run)) && take 1 run > take 1 (reverse run)
       in if streak >= 2
            then Just (Survivor k counts bytes streak growing (tgSample g))
            else Nothing

-- | Every thunk in the latest snapshot that belongs to a surviving group,
-- with how many snapshots its group has survived.
survivorIds :: HeapSnapshot -> [Survivor] -> Map ClosureId Int
survivorIds latest survivors =
  let streaks = M.fromList [(svKey s, svSurvived s) | s <- survivors]
   in M.mapMaybe (`M.lookup` streaks) (snapThunkKeys latest)
