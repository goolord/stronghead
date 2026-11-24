-- | Topological heap anatomy over a 'HeapSnapshot':
--
-- * Exact dominator tree and exclusively retained byte attribution
--   (Cooper-Harvey-Kennedy iterative dominators over a virtual super-root);
-- * Quotient heap / automatic spine folding (\"Heap Skeletons\") that
--   distill huge closure graphs into compact anatomical motifs;
-- * Counterfactual \"What-If\" reference severing (simulate breaking a
--   closure, edge, info-edge class, or CAF and measure exact bytes freed);
-- * Bottom-up Merkle structural deduplication audit (find redundant identical
--   sub-DAGs);
-- * Composable closure sieves and cross-snapshot structural diffs.
module Stronghead.Debug.Anatomy
  ( -- * Exact dominator tree
    DominatorTree (..)
  , computeDominators
  , domRetained
    -- * Quotient heap / spine folding
  , SkeletonNode (..)
  , SpineFinding (..)
  , HeapSkeleton (..)
  , buildHeapSkeleton
    -- * Counterfactual what-if severing
  , SeverTarget (..)
  , SeverResult (..)
  , simulateSever
    -- * Structural deduplication audit (Merkle hashing)
  , DuplicateGroup (..)
  , auditDuplicates
    -- * Composable closure sieve
  , ClosureSieve (..)
  , emptySieve
  , applySieve
    -- * Cross-snapshot structural diff
  , SkeletonDelta (..)
  , diffSnapshots
  ) where

import Control.Applicative ((<|>))
import Data.Bits (xor)
import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)

import Stronghead.Debug.Graph

-- ---------------------------------------------------------------------------
-- Exact dominator tree (Cooper-Harvey-Kennedy)

data DominatorTree = DominatorTree
  { dtIdom :: !(Map ClosureId ClosureId)
    -- ^ Immediate dominator of each reachable non-root closure.
  , dtChildren :: !(Map ClosureId [ClosureId])
    -- ^ Direct dominator-tree children of each closure.
  , dtRetainedBytes :: !(Map ClosureId Int)
    -- ^ Exact exclusively retained bytes: the exact memory freed if this
    -- closure becomes unreachable.
  , dtTopDominators :: ![(ClosureId, Int)]
    -- ^ Non-thread closures ranked by exact retained bytes (largest first).
  }
  deriving stock (Eq, Show)

domRetained :: DominatorTree -> ClosureId -> Int
domRetained dt cid = M.findWithDefault 0 cid (dtRetainedBytes dt)

-- | Compute the exact dominator tree of a 'HeapSnapshot' over a virtual
-- super-root (index @0@) connected to every GC root in 'snapRoots'.
computeDominators :: HeapSnapshot -> DominatorTree
computeDominators snap =
  let nodes = snapNodes snap
      roots = [r | r <- snapRoots snap, M.member r nodes]
      -- Iterative DFS postorder from virtual root -> roots.
      -- dfsPostorder conses finished nodes onto the head, so it is already RPO.
      rpoList = dfsPostorder nodes roots
      -- Index 0 is the virtual super-root; indices 1..n are reachable closures in RPO.
      n = length rpoList
      idByIdx = IM.fromList (zip [1 ..] rpoList)
      idxById = M.fromList (zip rpoList [1 ..])
      -- Build predecessor lists in RPO index space.
      rootEdges = [(M.findWithDefault 0 r idxById, 0 :: Int) | r <- roots, M.member r idxById]
      graphEdges =
        [ (cIdx, pIdx)
        | (pId, pIdx) <- M.toList idxById
        , Just pNode <- [M.lookup pId nodes]
        , cId <- nubOrd (cnChildren pNode)
        , cId /= pId
        , Just cIdx <- [M.lookup cId idxById]
        ]
      preds :: IntMap [Int]
      preds = IM.fromListWith (++) [(dst, [src]) | (dst, src) <- rootEdges ++ graphEdges]
      -- Iterative fixed-point for immediate dominators (0 = super-root, -1 = undefined).
      idom0 :: VU.Vector Int
      idom0 = VU.generate (n + 1) (\i -> if i == 0 then 0 else -1)
      idomVec = fixpoint n preds idom0
      -- Map back to ClosureId space.
      dtIdom =
        M.fromList
          [ (cId, pId)
          | i <- [1 .. n]
          , let d = idomVec VU.! i
          , d > 0
          , Just cId <- [IM.lookup i idByIdx]
          , Just pId <- [IM.lookup d idByIdx]
          ]
      dtChildren =
        M.fromListWith (++) [(p, [c]) | (c, p) <- M.toList dtIdom]
      -- Bottom-up retained byte accumulation (reverse RPO guarantees children before parents).
      ownSize i = maybe 0 cnSize (IM.lookup i idByIdx >>= (`M.lookup` nodes))
      retIdx =
        foldl'
          ( \m i ->
              let sz = IM.findWithDefault (ownSize i) i m
                  d = idomVec VU.! i
               in if d > 0
                    then IM.insertWith (+) d sz (IM.insert i sz m)
                    else IM.insert i sz m
          )
          (IM.fromList [(i, ownSize i) | i <- [1 .. n]])
          (reverse [1 .. n])
      dtRetainedBytes =
        M.fromList
          [ (cId, IM.findWithDefault 0 i retIdx)
          | (i, cId) <- IM.toList idByIdx
          ]
      rootSet = S.fromList roots
      dtTopDominators =
        take 200 $
          sortOn (Down . snd)
            [ (cid, bytes)
            | (cid, bytes) <- M.toList dtRetainedBytes
            , not (S.member cid rootSet)
            , Just nd <- [M.lookup cid nodes]
            , cnKind nd /= NThread
            ]
   in DominatorTree {..}

-- | Iterative DFS postorder over reachable closures (avoids stack overflow on
-- million-node thunk spines!).
dfsPostorder :: Map ClosureId ClosureNode -> [ClosureId] -> [ClosureId]
dfsPostorder nodes roots = go (map Left (reverse roots)) S.empty []
  where
    go [] _ post = post
    go (Right cid : stk) seen post = go stk seen (cid : post)
    go (Left cid : stk) seen post
      | S.member cid seen = go stk seen post
      | otherwise = case M.lookup cid nodes of
          Nothing -> go stk seen post
          Just nd ->
            let seen' = S.insert cid seen
                kids = [Left k | k <- reverse (cnChildren nd), M.member k nodes, not (S.member k seen')]
             in go (kids ++ Right cid : stk) seen' post

fixpoint :: Int -> IntMap [Int] -> VU.Vector Int -> VU.Vector Int
fixpoint n preds = loop
  where
    loop idom =
      let (changed, idom') = foldl' step (False, idom) [1 .. n]
       in if changed then loop idom' else idom'
    step (chg, idom) b =
      let ps = [p | p <- IM.findWithDefault [] b preds, idom VU.! p /= -1]
       in case ps of
            [] -> (chg, idom)
            p0 : rest ->
              let newD = foldl' (intersect idom) p0 rest
               in if newD /= idom VU.! b
                    then (True, idom VU.// [(b, newD)])
                    else (chg, idom)
    intersect idom a0 b0 = go a0 b0
      where
        go a b
          | a == b = a
          | a > b = go (idom VU.! a) b
          | otherwise = go a (idom VU.! b)

-- ---------------------------------------------------------------------------
-- Quotient heap / automatic spine folding ("Heap Skeletons")

data SkeletonNode = SkeletonNode
  { skId :: !Int
  , skLabel :: !Text
  , skType :: !Text
  , skKind :: !NodeKind
  , skSource :: !(Maybe Text)
  , skCount :: !Int
  , skMaxSpine :: !Int
    -- ^ Longest linear chain of consecutive identical-signature closures.
  , skOwnBytes :: !Int
  , skRetainedBytes :: !Int
    -- ^ Max dominator retained bytes among instances of this structural motif.
  , skSample :: ![ClosureId]
  , skChildren :: ![Int]
  }
  deriving stock (Eq, Show)

data SpineFinding = SpineFinding
  { spType :: !Text
  , spName :: !Text
  , spSource :: !(Maybe Text)
  , spKind :: !NodeKind
  , spDepth :: !Int
  , spRetainedBytes :: !Int
  , spHead :: !ClosureId
  , spAnchor :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

data HeapSkeleton = HeapSkeleton
  { hsNodes :: ![SkeletonNode]
  , hsRoots :: ![Int]
  , hsSpines :: ![SpineFinding]
    -- ^ Longest/heaviest folded chains (e.g. thunk spines, giant lists).
  }
  deriving stock (Eq, Show)

-- | Collapse a 'HeapSnapshot' into a compact structural quotient graph and
-- detect long homogeneous chains (thunk spines, linked lists, etc.).
buildHeapSkeleton :: HeapSnapshot -> DominatorTree -> HeapSkeleton
buildHeapSkeleton snap dt =
  let nodes = snapNodes snap
      sigOf n = (cnType n, cnName n, cnSource n)
      -- Compute chain depth for every node whose parent in the dominator tree
      -- (or BFS tree) has the same signature.
      parentOf cid = M.lookup cid (dtIdom dt) <|> M.lookup cid (snapParent snap)
      -- Traverse in BFS order (parents before children) via snapParent/dtIdom.
      -- Measure homogeneous chain lengths downward from chain heads.
      sameSigParent cid = case (M.lookup cid nodes, parentOf cid >>= (`M.lookup` nodes)) of
        (Just n, Just p) -> sigOf n == sigOf p
        _ -> False
      chainHeads =
        [ n
        | n <- M.elems nodes
        , M.member (cnId n) (dtRetainedBytes dt)
        , not (sameSigParent (cnId n))
        ]
      measureSpine headNode =
        let targetSig = sigOf headNode
            go seen cur !depth =
              let sameKids =
                    [ k
                    | k <- cnChildren cur
                    , not (S.member k seen)
                    , Just kn <- [M.lookup k nodes]
                    , sigOf kn == targetSig
                    ]
               in case sameKids of
                    [] -> depth
                    k : rest ->
                      -- Follow heaviest same-signature child iteratively (cycle-safe)
                      let bestK = foldl' (\a b -> if domRetained dt a >= domRetained dt b then a else b) k rest
                       in case M.lookup bestK nodes of
                            Nothing -> depth
                            Just kn -> go (S.insert bestK seen) kn (depth + 1)
         in go (S.singleton (cnId headNode)) headNode 1
      spineBySig =
        M.fromListWith
          (\(d1, r1, h1) (d2, r2, h2) -> if (d1, r1) >= (d2, r2) then (d1, r1, h1) else (d2, r2, h2))
          [ (sigOf n, (d, domRetained dt (cnId n), cnId n))
          | n <- chainHeads
          , let d = measureSpine n
          , d >= 2
          ]
      spines =
        take 25 $
          sortOn (\s -> (Down (spKind s == NThunk), Down (spRetainedBytes s), Down (spDepth s)))
            [ SpineFinding ty nm src (cnKind nd) d ret hd (fmap nodeTitle (anchorOf (retainerPath snap hd)))
            | ((ty, nm, src), (d, ret, hd)) <- M.toList spineBySig
            , Just nd <- [M.lookup hd nodes]
            ]
      -- Quotient graph: group all reachable closures by (cnType, cnName, cnSource).
      grouped =
        M.fromListWith
          (\(c1, b1, r1, s1) (c2, b2, r2, s2) -> (c1 + c2, b1 + b2, max r1 r2, take 4 (s1 ++ s2)))
          [ (sigOf n, (1 :: Int, cnSize n, domRetained dt (cnId n), [cnId n]))
          | n <- M.elems nodes
          , M.member (cnId n) (dtRetainedBytes dt)
          ]
      orderedSigs = sortOn (\(_, (_, _, r, _)) -> Down r) (M.toList grouped)
      sigIdMap = M.fromList (zip (map fst orderedSigs) [0 :: Int ..])
      -- Aggregate quotient edges between distinct signatures (ignoring self-loops,
      -- which are already captured by skMaxSpine).
      quotEdges =
        M.fromListWith
          S.union
          [ (srcId, S.singleton dstId)
          | n <- M.elems nodes
          , Just srcId <- [M.lookup (sigOf n) sigIdMap]
          , k <- cnChildren n
          , Just kn <- [M.lookup k nodes]
          , Just dstId <- [M.lookup (sigOf kn) sigIdMap]
          , srcId /= dstId
          ]
      skNodes =
        [ SkeletonNode
            { skId = idx
            , skLabel = nm
            , skType = ty
            , skKind = kindOfClosureType ty
            , skSource = src
            , skCount = cnt
            , skMaxSpine = maybe 1 (\(d, _, _) -> d) (M.lookup sig spineBySig)
            , skOwnBytes = ownB
            , skRetainedBytes = retB
            , skSample = samples
            , skChildren = S.toList (M.findWithDefault S.empty idx quotEdges)
            }
        | (idx, (sig@(ty, nm, src), (cnt, ownB, retB, samples))) <- zip [0 ..] orderedSigs
        ]
      skRoots =
        nubOrd
          [ sid
          | r <- snapRoots snap
          , Just rn <- [M.lookup r nodes]
          , Just sid <- [M.lookup (sigOf rn) sigIdMap]
          ]
   in HeapSkeleton skNodes skRoots spines

-- ---------------------------------------------------------------------------
-- Counterfactual "What-If" Reference Severing

data SeverTarget
  = SeverClosure !ClosureId
    -- ^ Virtually remove one closure (e.g., force this thunk or clear this CAF).
  | SeverEdge !ClosureId !ClosureId
    -- ^ Virtually cut one pointer from @parent@ to @child@.
  | SeverInfoEdge !Text !Text
    -- ^ Virtually cut EVERY pointer from closures matching @parentName@ to
    -- closures matching @childName@ (e.g., making a record field strict).
  | SeverType !Text
    -- ^ Virtually remove every closure matching this type or name.
  deriving stock (Eq, Show)

data SeverResult = SeverResult
  { srTarget :: !SeverTarget
  , srBeforeBytes :: !Int
  , srAfterBytes :: !Int
  , srFreedBytes :: !Int
  , srFreedClosures :: !Int
  , srFreedFraction :: !Double
  , srTopFreed :: ![(Text, Int, Int)]
    -- ^ @(closureName, freedCount, freedBytes)@, largest first.
  }
  deriving stock (Eq, Show)

-- | Simulate severing a closure, edge, or edge class on a 'HeapSnapshot' and
-- compute the exact set of closures and bytes that become unreachable.
simulateSever :: HeapSnapshot -> SeverTarget -> SeverResult
simulateSever snap target =
  let nodes = snapNodes snap
      reachableBefore = M.keysSet (snapParent snap) `S.union` S.fromList [r | r <- snapRoots snap, M.member r nodes]
      beforeBytes = sum [maybe 0 cnSize (M.lookup cid nodes) | cid <- S.toList reachableBefore]
      dropNode n = case target of
        SeverClosure c -> cnId n == c
        SeverType t -> cnType n == t || cnName n == t
        _ -> False
      dropEdge p c = case target of
        SeverEdge a b -> cnId p == a && cnId c == b
        SeverInfoEdge pn cn ->
          (cnName p == pn || cnType p == pn) && (cnName c == cn || cnType c == cn)
        _ -> False
      startRoots = [r | r <- snapRoots snap, Just n <- [M.lookup r nodes], not (dropNode n)]
      reachableAfter = bfsReach nodes dropNode dropEdge startRoots
      freedIds = S.difference reachableBefore reachableAfter
      freedNodes = mapMaybe (`M.lookup` nodes) (S.toList freedIds)
      freedBytes = sum (map cnSize freedNodes)
      afterBytes = max 0 (beforeBytes - freedBytes)
      frac = if beforeBytes > 0 then fromIntegral freedBytes / fromIntegral beforeBytes else 0
      byName =
        M.fromListWith
          (\(c1, b1) (c2, b2) -> (c1 + c2, b1 + b2))
          [(cnName n <> " [" <> cnType n <> "]", (1 :: Int, cnSize n)) | n <- freedNodes]
      topFreed =
        take 10 $
          sortOn (\(_, _, b) -> Down b) [(nm, cnt, b) | (nm, (cnt, b)) <- M.toList byName]
   in SeverResult
        { srTarget = target
        , srBeforeBytes = beforeBytes
        , srAfterBytes = afterBytes
        , srFreedBytes = freedBytes
        , srFreedClosures = S.size freedIds
        , srFreedFraction = frac
        , srTopFreed = topFreed
        }

bfsReach
  :: Map ClosureId ClosureNode
  -> (ClosureNode -> Bool)
  -> (ClosureNode -> ClosureNode -> Bool)
  -> [ClosureId]
  -> Set ClosureId
bfsReach nodes dropNode dropEdge roots = go (Seq.fromList roots) (S.fromList roots)
  where
    go Empty seen = seen
    go (x :<| q) seen = case M.lookup x nodes of
      Nothing -> go q seen
      Just p ->
        let kids =
              [ k
              | k <- cnChildren p
              , not (S.member k seen)
              , Just kn <- [M.lookup k nodes]
              , not (dropNode kn)
              , not (dropEdge p kn)
              ]
            seen' = foldl' (flip S.insert) seen kids
         in go (foldl' (:|>) q kids) seen'

-- ---------------------------------------------------------------------------
-- Bottom-Up Merkle Structural Deduplication Audit

data DuplicateGroup = DuplicateGroup
  { dgHash :: !Word64
  , dgType :: !Text
  , dgName :: !Text
  , dgSource :: !(Maybe Text)
  , dgCount :: !Int
  , dgUnitBytes :: !Int
  , dgWasteBytes :: !Int
    -- ^ @(dgCount - 1) * dgUnitBytes@: bytes reclaimable via sharing/interning.
  , dgSample :: ![ClosureId]
  }
  deriving stock (Eq, Show)

-- | Compute bounded bottom-up FNV-1a Merkle hashes over immutable closures
-- (@CONSTR@, @ARR_WORDS@, @THUNK@) to surface duplicated data structures that
-- could be interned or shared.
auditDuplicates :: HeapSnapshot -> [DuplicateGroup]
auditDuplicates snap =
  let nodes = snapNodes snap
      eligible n = cnKind n `elem` [NConstr, NArray, NThunk] && cnSize n > 0
      -- Memoize bounded-depth structural hashes bottom-up.
      hashes = foldl' (\m n -> fst (hashNode nodes (6 :: Int) S.empty m (cnId n))) M.empty (M.elems nodes)
      grouped =
        M.fromListWith
          (\(n1, c1, s1) (_, c2, s2) -> (n1, c1 + c2, take 4 (s1 ++ s2)))
          [ (h, (n, 1 :: Int, [cnId n]))
          | n <- M.elems nodes
          , eligible n
          , Just h <- [M.lookup (cnId n) hashes]
          ]
   in take 30 $
        sortOn (Down . dgWasteBytes)
          [ DuplicateGroup
              { dgHash = h
              , dgType = cnType n
              , dgName = cnName n
              , dgSource = cnSource n
              , dgCount = cnt
              , dgUnitBytes = cnSize n
              , dgWasteBytes = (cnt - 1) * cnSize n
              , dgSample = samples
              }
          | (h, (n, cnt, samples)) <- M.toList grouped
          , cnt >= 2
          ]

hashNode
  :: Map ClosureId ClosureNode
  -> Int
  -> Set ClosureId
  -> Map ClosureId Word64
  -> ClosureId
  -> (Map ClosureId Word64, Word64)
hashNode nodes depth visiting memo cid
  | Just h <- M.lookup cid memo = (memo, h)
  | depth <= 0 || S.member cid visiting = (memo, 0x9e3779b97f4a7c15)
  | otherwise = case M.lookup cid nodes of
      Nothing -> (memo, 0)
      Just n ->
        let visiting' = S.insert cid visiting
            (memo', childHashes) =
              foldl'
                ( \(m, hs) k ->
                    let (m', kh) = hashNode nodes (depth - 1) visiting' m k
                     in (m', kh : hs)
                )
                (memo, [])
                (take 8 (cnChildren n))
            baseH =
              fnvText (cnType n)
                `fnvMix` fnvText (cnName n)
                `fnvMix` maybe 0 fnvText (cnSource n)
                `fnvMix` fromIntegral (cnSize n)
            finalH = foldl' fnvMix baseH (reverse childHashes)
         in (M.insert cid finalH memo', finalH)

fnvText :: Text -> Word64
fnvText = T.foldl' (\h c -> (h `xor` fromIntegral (ord c)) * 1099511628211) 14695981039346656037

fnvMix :: Word64 -> Word64 -> Word64
fnvMix a b = (a `xor` b) * 1099511628211

-- ---------------------------------------------------------------------------
-- Composable Closure Sieve

data ClosureSieve = ClosureSieve
  { svKinds :: ![NodeKind]
    -- ^ Empty means all kinds.
  , svQuery :: !Text
    -- ^ Case-insensitive substring match against type, name, or source.
  , svMinDomBytes :: !Int
  , svOnlyCAFs :: !Bool
  , svOnlySurviving :: !Bool
  }
  deriving stock (Eq, Show)

emptySieve :: ClosureSieve
emptySieve = ClosureSieve [] "" 0 False False

-- | Filter a snapshot's closures through a composable sieve, returning matches
-- ranked by exact dominator retained bytes.
applySieve :: HeapSnapshot -> DominatorTree -> Set ClosureId -> ClosureSieve -> [(ClosureNode, Int)]
applySieve snap dt survivors ClosureSieve {..} =
  sortOn (Down . snd)
    [ (n, ret)
    | n <- M.elems (snapNodes snap)
    , let ret = domRetained dt (cnId n)
    , null svKinds || cnKind n `elem` svKinds
    , ret >= svMinDomBytes
    , not svOnlyCAFs || isCAF n
    , not svOnlySurviving || S.member (cnId n) survivors
    , T.null q
        || q `T.isInfixOf` T.toLower (cnType n)
        || q `T.isInfixOf` T.toLower (cnName n)
        || maybe False (T.isInfixOf q . T.toLower) (cnSource n)
    ]
  where
    q = T.toLower (T.strip svQuery)

-- ---------------------------------------------------------------------------
-- Cross-Snapshot Structural Diff

data SkeletonDelta = SkeletonDelta
  { sdType :: !Text
  , sdName :: !Text
  , sdSource :: !(Maybe Text)
  , sdKind :: !NodeKind
  , sdBaseCount :: !Int
  , sdCurCount :: !Int
  , sdBaseBytes :: !Int
  , sdCurBytes :: !Int
  , sdBaseDomBytes :: !Int
  , sdCurDomBytes :: !Int
  , sdCurMaxSpine :: !Int
  }
  deriving stock (Eq, Show)

-- | Compare two snapshots structurally by @(type, name, source)@, sorted by
-- largest increase in retained/shallow bytes first.
diffSnapshots :: HeapSnapshot -> HeapSnapshot -> [SkeletonDelta]
diffSnapshots base cur =
  let bSkel = buildHeapSkeleton base (computeDominators base)
      cSkel = buildHeapSkeleton cur (computeDominators cur)
      toMap sk = M.fromList [((skType n, skLabel n, skSource n), n) | n <- hsNodes sk]
      bm = toMap bSkel
      cm = toMap cSkel
      allKeys = M.keys (M.union bm cm)
   in sortOn (\d -> Down (sdCurBytes d - sdBaseBytes d, sdCurDomBytes d - sdBaseDomBytes d))
        [ SkeletonDelta
            { sdType = ty
            , sdName = nm
            , sdSource = src
            , sdKind = maybe (maybe (kindOfClosureType ty) skKind mb) skKind mc
            , sdBaseCount = maybe 0 skCount mb
            , sdCurCount = maybe 0 skCount mc
            , sdBaseBytes = maybe 0 skOwnBytes mb
            , sdCurBytes = maybe 0 skOwnBytes mc
            , sdBaseDomBytes = maybe 0 skRetainedBytes mb
            , sdCurDomBytes = maybe 0 skRetainedBytes mc
            , sdCurMaxSpine = maybe 0 skMaxSpine mc
            }
        | k@(ty, nm, src) <- allKeys
        , let mb = M.lookup k bm
              mc = M.lookup k cm
        , maybe 0 skCount mb /= maybe 0 skCount mc || maybe 0 skOwnBytes mb /= maybe 0 skOwnBytes mc
        ]
