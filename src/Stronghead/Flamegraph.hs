-- | Unified flamegraph / differential flamegraph models and open-format
-- exporters (folded stacks, Speedscope JSON, Perfetto / Chrome Trace JSON).
--
-- Drives interactive flamegraph views and headless SVG exports from any of
-- Stronghead's stack sources: eventlog cost-centre samples (@+RTS -p -l@),
-- @.prof@ trees, or cross-platform live @CloneStack@ folded samples (both
-- On-CPU and Off-CPU).
module Stronghead.Flamegraph
  ( -- * Flame tree
    FlameNode (..)
  , FlameTree (..)
  , fromFoldedStacks
  , fromCostCentreSamples
  , fromProfFile
  , fromLiveFolded
    -- * Differential flamegraph
  , DiffFlameNode (..)
  , DiffFlameTree (..)
  , diffFlameTrees
    -- * Open-format exporters
  , exportFolded
  , exportSpeedscopeJson
  , exportChromeTraceJson
  ) where

import Data.Char (ord)
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Numeric (showHex)

import Stronghead.Eventlog.Model
import Stronghead.Format (tshow)
import Stronghead.Live (LiveFrameEntry (..))
import Stronghead.Prof (CostCentre (..), ProfFile (..))

-- ---------------------------------------------------------------------------
-- Flame trees

data FlameNode = FlameNode
  { fnName :: !Text
  , fnSelf :: !Double
  , fnTotal :: !Double
  , fnChildren :: ![FlameNode]
  }
  deriving stock (Eq, Show)

data FlameTree = FlameTree
  { ftTitle :: !Text
  , ftUnit :: !Text
    -- ^ e.g. @"samples"@, @"percent"@, @"bytes"@.
  , ftTotal :: !Double
  , ftRoots :: ![FlameNode]
  }
  deriving stock (Eq, Show)

-- | Intermediate trie used to merge arbitrary folded stacks root-first.
data Trie = Trie !Double !(Map Text Trie)

emptyTrie :: Trie
emptyTrie = Trie 0 M.empty

insertStack :: [Text] -> Double -> Trie -> Trie
insertStack [] w (Trie s cs) = Trie (s + w) cs
insertStack (f : fs) w (Trie s cs) =
  Trie s (M.alter (Just . insertStack fs w . fromMaybe emptyTrie) f cs)

trieToNodes :: Trie -> [FlameNode]
trieToNodes (Trie _ cs) =
  sortOn (Down . fnTotal) [build k t | (k, t) <- M.toList cs]
  where
    build name (Trie self sub) =
      let kids = trieToNodes (Trie 0 sub)
          tot = self + sum (map fnTotal kids)
       in FlameNode name self tot kids

-- | Build a 'FlameTree' from root-first folded stacks @(frames, weight)@.
fromFoldedStacks :: Text -> Text -> [([Text], Double)] -> FlameTree
fromFoldedStacks title unit stacks =
  let trie = foldl' (\acc (fs, w) -> if w > 0 && not (null fs) then insertStack fs w acc else acc) emptyTrie stacks
      roots = trieToNodes trie
      total = sum (map fnTotal roots)
   in FlameTree title unit total roots

-- | Build a flamegraph from eventlog cost-centre samples (@+RTS -p -l@)
-- within @[startNs, endNs]@.
fromCostCentreSamples :: Timeline -> Word64 -> Word64 -> FlameTree
fromCostCentreSamples tl a b =
  fromFoldedStacks "Cost-centre samples" "samples" stacks
  where
    ccs = tlCostCentres tl
    samples = tlProfSamples tl
    i0 = lowerBoundBy psTime samples a
    i1 = if b == maxBound then V.length samples else lowerBoundBy psTime samples (b + 1)
    inWin = V.slice i0 (max 0 (i1 - i0)) samples
    labelOf cid = case IM.lookup (fromIntegral cid) ccs of
      Just cc | isIdleCostCentre cc -> Nothing
      Just cc -> Just (cciModule cc <> "." <> cciLabel cc)
      Nothing -> Just ("cc" <> tshow cid)
    stacks =
      [ (frames, 1.0)
      | s <- V.toList inWin
      , -- Eventlog stacks are innermost-first; reverse to root-first.
        let frames = [lbl | cid <- reverse (VU.toList (psStack s)), Just lbl <- [labelOf cid]]
      , not (null frames)
      ]

-- | Build a flamegraph from a @.prof@ cost-centre call tree (by %time).
fromProfFile :: ProfFile -> FlameTree
fromProfFile pf =
  let roots = map convert (pfRoots pf)
      tot = sum (map fnTotal roots)
      hdr = case pfHeader pf of
        h : _ -> " (" <> T.strip h <> ")"
        [] -> ""
   in FlameTree ("Cost centres" <> hdr) "percent" tot roots
  where
    convert cc =
      let kids = sortOn (Down . fnTotal) (map convert (ccChildren cc))
          nm = if T.null (ccModule cc) then ccName cc else ccModule cc <> "." <> ccName cc
          tot = max (ccInhTime cc) (ccTime cc + sum (map fnTotal kids))
       in FlameNode nm (ccTime cc) tot kids

-- | Build an On-CPU or Off-CPU flamegraph from Agent v3 @CloneStack@ folded
-- samples. Pass @Just "running"@ for On-CPU, @Just "BlockedOnMVar"@ for
-- contention-specific Off-CPU, or @Nothing@ for all states combined.
fromLiveFolded :: Maybe Text -> Map (Text, [LiveFrameEntry]) Int -> FlameTree
fromLiveFolded mStatusFilter folded =
  fromFoldedStacks title "samples" stacks
  where
    title = case mStatusFilter of
      Nothing -> "Live CloneStack (all threads)"
      Just s -> "Live CloneStack (" <> s <> ")"
    stacks =
      [ (frames, fromIntegral cnt)
      | ((st, entries), cnt) <- M.toList folded
      , cnt > 0
      , maybe True (\f -> f `T.isInfixOf` st) mStatusFilter
      , let frames = [fmtEntry e | e <- reverse entries]
      , not (null frames)
      ]
    fmtEntry e =
      let modPfx = if T.null (lfeModule e) then "" else lfeModule e <> "."
          fn = if T.null (lfeFunction e) then lfeClosureType e else lfeFunction e
       in modPfx <> fn

-- ---------------------------------------------------------------------------
-- Differential flamegraph (current vs baseline)

data DiffFlameNode = DiffFlameNode
  { dfnName :: !Text
  , dfnBaseTotal :: !Double
  , dfnCurTotal :: !Double
  , dfnDelta :: !Double
    -- ^ Positive = regression (red), negative = improvement (blue).
  , dfnChildren :: ![DiffFlameNode]
  }
  deriving stock (Eq, Show)

data DiffFlameTree = DiffFlameTree
  { dftTitle :: !Text
  , dftBaseTotal :: !Double
  , dftCurTotal :: !Double
  , dftRoots :: ![DiffFlameNode]
  }
  deriving stock (Eq, Show)

-- | Compare a baseline and current 'FlameTree' structurally frame-by-frame.
diffFlameTrees :: FlameTree -> FlameTree -> DiffFlameTree
diffFlameTrees base cur =
  DiffFlameTree
    (ftTitle cur <> " vs baseline")
    (ftTotal base)
    (ftTotal cur)
    (mergeLevels (ftRoots base) (ftRoots cur))
  where
    mergeLevels bs cs =
      let bMap = M.fromList [(fnName n, n) | n <- bs]
          cMap = M.fromList [(fnName n, n) | n <- cs]
          keys = M.keys (M.union bMap cMap)
       in sortOn (Down . \d -> max (dfnCurTotal d) (dfnBaseTotal d))
            [ let mb = M.lookup k bMap
                  mc = M.lookup k cMap
                  bt = maybe 0 fnTotal mb
                  ct = maybe 0 fnTotal mc
                  kids = mergeLevels (maybe [] fnChildren mb) (maybe [] fnChildren mc)
               in DiffFlameNode k bt ct (ct - bt) kids
            | k <- keys
            ]

-- ---------------------------------------------------------------------------
-- Open-format exporters

-- | Export a 'FlameTree' as @flamegraph.pl@ folded stacks (@a;b;c 42@).
exportFolded :: FlameTree -> Text
exportFolded ft = T.unlines (concatMap (go []) (ftRoots ft))
  where
    go prefix n =
      let path = prefix ++ [fnName n]
          ownLine = [T.intercalate ";" path <> " " <> tshow (round (fnSelf n) :: Integer) | fnSelf n > 0]
       in ownLine ++ concatMap (go path) (fnChildren n)

-- | Export a 'FlameTree' in Speedscope's @0.0.1@ JSON file format.
exportSpeedscopeJson :: Text -> FlameTree -> Text
exportSpeedscopeJson profileName ft =
  let (frameIdxMap, samples) = collectSamples ft
      framesOrdered = map fst (sortOn snd (M.toList frameIdxMap))
      framesJson = T.intercalate "," ["{\"name\":" <> jsonStr f <> "}" | f <- framesOrdered]
      stacksJson = T.intercalate "," ["[" <> T.intercalate "," (map tshow st) <> "]" | (st, _) <- samples]
      weightsJson = T.intercalate "," [tshow w | (_, w) <- samples]
      endVal = sum (map snd samples)
   in T.concat
        [ "{\"$schema\":\"https://www.speedscope.app/file-format-schema.json\","
        , "\"version\":\"0.0.1\","
        , "\"shared\":{\"frames\":["
        , framesJson
        , "]},"
        , "\"profiles\":[{\"type\":\"sampled\",\"name\":"
        , jsonStr profileName
        , ",\"unit\":\"none\",\"startValue\":0,\"endValue\":"
        , tshow endVal
        , ",\"samples\":["
        , stacksJson
        , "],\"weights\":["
        , weightsJson
        , "]}]}"
        ]
  where
    collectSamples tree =
      let raw = concatMap (flatten []) (ftRoots tree)
          idxMap = M.fromList (zip (nubNames [f | (fs, _) <- raw, f <- fs]) [0 :: Int ..])
          indexed = [([M.findWithDefault 0 f idxMap | f <- fs], w) | (fs, w) <- raw, w > 0]
       in (idxMap, indexed)
    flatten prefix n =
      let path = prefix ++ [fnName n]
          own = [(path, fnSelf n) | fnSelf n > 0]
       in own ++ concatMap (flatten path) (fnChildren n)
    nubNames = M.keys . M.fromList . map (,())

-- | Export a Stronghead 'Timeline' to Perfetto / Chrome Trace JSON
-- (@chrome://tracing@ / ui.perfetto.dev), including capability run spans,
-- GC pauses, @withSpan@ request waterfalls, and user markers.
exportChromeTraceJson :: Timeline -> Text
exportChromeTraceJson tl =
  "{\"traceEvents\":[" <> T.intercalate ",\n" events <> "]}\n"
  where
    t0 = tlStart tl
    us ns = fromIntegral (ns - min ns t0) / (1000.0 :: Double)
    capEvents =
      [ T.concat
          [ "{\"name\":"
          , jsonStr nm
          , ",\"cat\":"
          , jsonStr cat
          , ",\"ph\":\"X\",\"ts\":"
          , tshow (us s)
          , ",\"dur\":"
          , tshow (fromIntegral (e - s) / (1000.0 :: Double))
          , ",\"pid\":1,\"tid\":"
          , tshow c
          , "}"
          ]
      | (c, tr) <- zip [0 :: Int ..] (V.toList (tlCaps tl))
      , i <- [0 .. min 20000 (trackLength tr - 1)]
      , let s = trStart tr VU.! i
            e = trEnd tr VU.! i
            k = trKind tr VU.! i
            tid = trThread tr VU.! i
            (cat, nm) =
              if k == kindGC
                then ("gc", "GC")
                else ("mutator", maybe ("thread " <> tshow tid) threadName (M.lookup tid (tlThreads tl)))
      , e > s
      ]
    spanEvents =
      [ T.concat
          [ "{\"name\":"
          , jsonStr (usName sp)
          , ",\"cat\":\"span\",\"ph\":\"X\",\"ts\":"
          , tshow (us (usStart sp))
          , ",\"dur\":"
          , tshow (fromIntegral (usEnd sp - usStart sp) / (1000.0 :: Double))
          , ",\"pid\":2,\"tid\":"
          , tshow (usThread sp)
          , ",\"args\":{\"alloc_bytes\":"
          , tshow (usAllocBytes sp)
          , "}}"
          ]
      | sp <- V.toList (tlUserSpans tl)
      ]
    markerEvents =
      [ T.concat
          [ "{\"name\":"
          , jsonStr (mkText m)
          , ",\"cat\":\"marker\",\"ph\":\"i\",\"s\":\"g\",\"ts\":"
          , tshow (us (mkTime m))
          , ",\"pid\":1,\"tid\":"
          , tshow (fromMaybe 0 (mkCap m))
          , "}"
          ]
      | m <- take 5000 (V.toList (tlMarkers tl))
      ]
    events = capEvents ++ spanEvents ++ markerEvents

jsonStr :: Text -> Text
jsonStr t = "\"" <> T.concatMap esc t <> "\""
  where
    esc = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c
        | ord c < 0x20 ->
            let h = T.pack (showHex (ord c) "")
             in "\\u" <> T.replicate (4 - T.length h) "0" <> h
        | otherwise -> T.singleton c
