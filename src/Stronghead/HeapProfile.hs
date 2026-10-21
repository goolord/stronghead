-- | Heap profiles: the band-per-sample residency matrix behind the heap
-- explorer, whether it came from a @.hp@ file or from heap-sample events in
-- an eventlog; plus the analyses run over it (leak suspects, thunk
-- classification, and the cost-centre / retainer tree).
module Stronghead.HeapProfile
  ( HeapProfile (..)
  , Band (..)
  , BandKind (..)
  , InfoProv (..)
  , infoProvDetail
  , fromSamples
  , hpOffset
  , hpOffsets
  , parseHp
  , loadHp
  , classifyBand
  , closureTypeKind
    -- * Analysis
  , Suspect (..)
  , leakSuspects
  , analysedSamples
  , bandDelta
  , RetainTree (..)
  , retainTree
  , looksStructured
  ) where

import Control.Applicative ((<|>))
import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn, unsnoc)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

import Stronghead.Format (readDecimal, readDouble)
import Stronghead.Run (RunInfo, emptyRunInfo, runFromHp)
import Stronghead.Source (SrcLoc, renderSrcLoc)
import Stronghead.TextFile (readTextFile)

-- | What a band's closures are, as far as the breakdown lets us tell.
data BandKind
  = KindThunk
  | KindFunction
  | KindConstructor
  | KindData
    -- ^ Arrays, byte arrays, MUT_VARs and other primitive storage.
  | KindUnknown
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Band = Band
  { bandName :: !Text
  , bandValues :: !(VU.Vector Double)
    -- ^ Residency in bytes at each sample.
  , bandPeak :: !Double
  , bandKind :: !BandKind
  , bandProv :: !(Maybe InfoProv)
    -- ^ Where the band's closures were defined, when an info-table
    -- breakdown (@-hi@) names them.
  , bandDetail :: !(Maybe Text)
    -- ^ 'bandProv' as one line of text.
  }
  deriving stock (Eq, Show)

-- | An info table's provenance, from the eventlog's IPE events (built with
-- @-finfo-table-map@).
data InfoProv = InfoProv
  { ipTableName :: !Text
    -- ^ The info table's symbol: an STG binder plus @_info@
    -- (@main2_sat_s7qA_info@), or a constructor allocation site
    -- (@I#_Main_3_con_info@) with @-fdistinct-constructor-tables@.
  , ipClosureType :: !Int
    -- ^ The closure type number (rts/include/rts/storage/ClosureTypes.h).
  , ipKind :: !BandKind
  , ipType :: !Text
  , ipLabel :: !Text
    -- ^ The binding the closure was allocated in.
  , ipModule :: !Text
  , ipSrc :: !SrcLoc
  }
  deriving stock (Eq, Show)

-- | @table :: type  (label, Module file:line:col)@.
infoProvDetail :: InfoProv -> Text
infoProvDetail ip =
  ipTableName ip <> " :: " <> ipType ip <> "  (" <> ipLabel ip <> ", " <> ipModule ip <> " " <> renderSrcLoc (ipSrc ip) <> ")"

data HeapProfile = HeapProfile
  { hpJob :: !Text
  , hpBreakdown :: !Text
  , hpTimes :: !(VU.Vector Double)
    -- ^ Sample times in seconds. From an eventlog, program time (since the
    -- RTS started). A @.hp@ file's own times are not elapsed time (most
    -- likely mutator CPU time) and only approximate it.
  , hpStart :: !Double
    -- ^ The program time displays count from: the eventlog's first event,
    -- or 0 for a @.hp@ file.
  , hpBands :: !(V.Vector Band)
    -- ^ Sorted by peak residency, largest first.
  , hpTotals :: !(VU.Vector Double)
  , hpRun :: !RunInfo
    -- ^ From a @.hp@ file's @JOB@ and @DATE@, or the eventlog's run.
  }
  deriving stock (Eq, Show)

-- | Sample @i@'s time from the start of the run, as displays show it.
hpOffset :: HeapProfile -> Int -> Double
hpOffset hp i = hpTimes hp VU.! i - hpStart hp

hpOffsets :: HeapProfile -> VU.Vector Double
hpOffsets hp = VU.map (subtract (hpStart hp)) (hpTimes hp)

-- | Build a profile from samples of @(time, [(band, bytes)])@, with times
-- counted from @start@ in displays. Bands missing from a sample are zero
-- there. @prov@ supplies provenance (from IPE events) for band names it
-- knows, which also fixes their kind.
fromSamples
  :: Text
  -> Text
  -> Double
  -> (Text -> Maybe InfoProv)
  -> [(Double, [(Text, Double)])]
  -> HeapProfile
fromSamples job breakdown start prov samples0 =
  HeapProfile
    { hpJob = job
    , hpBreakdown = breakdown
    , hpTimes = VU.fromList (map fst samples)
    , hpStart = start
    , hpBands = V.fromList (sortOn (Down . bandPeak) bands)
    , hpTotals = VU.fromList [sum (map snd s) | (_, s) <- samples]
    , hpRun = emptyRunInfo
    }
  where
    samples = sortOn fst samples0
    n = length samples
    names = nubOrd [b | (_, s) <- samples, (b, _) <- s]
    columns :: M.Map Text (M.Map Int Double)
    columns =
      M.fromListWith
        (M.unionWith (+))
        [(b, M.singleton i v) | (i, (_, s)) <- zip [0 ..] samples, (b, v) <- s]
    bands =
      [ Band
          { bandName = T.copy name -- not a slice that keeps the whole file
          , bandValues = vals
          , bandPeak = if VU.null vals then 0 else VU.maximum vals
          , bandKind = maybe (classifyBand name) ipKind p
          , bandProv = p
          , bandDetail = infoProvDetail <$> p
          }
      | name <- names
      , let col = M.findWithDefault M.empty name columns
            vals = VU.generate n (\i -> M.findWithDefault 0 i col)
            p = prov name
      ]

-- | Guess a band's kind from its name. Exact for closure-type (@-hT@)
-- breakdowns; a heuristic otherwise.
classifyBand :: Text -> BandKind
classifyBand name
  | any (`T.isPrefixOf` name) ["THUNK", "AP", "BLACKHOLE", "IND"] = KindThunk
  | any (`T.isPrefixOf` name) ["FUN", "PAP"] = KindFunction
  | any (`T.isPrefixOf` name) ["CONSTR"] = KindConstructor
  | any (`T.isPrefixOf` name) ["ARR_WORDS", "MUT_ARR", "SMALL_MUT_ARR", "MUT_VAR", "MVAR", "TVAR", "WEAK", "STACK", "TSO"] = KindData
  | "(" `T.isPrefixOf` name && ")" `T.isSuffixOf` name = KindConstructor -- (:) (,)
  -- GHC 9.10+ closure-type profiles name constructors pkg:Module.Con.
  | (pkg, rest) <- T.breakOn ":" name
  , not (T.null pkg)
  , not (T.null rest)
  , not (T.any (== ' ') name)
  , con <- T.takeWhileEnd (/= '.') (T.drop 1 rest)
  , maybe False (\(c, _) -> c >= 'A' && c <= 'Z') (T.uncons con) || con == ":" || T.null con =
      KindConstructor
  | T.any (== '#') name && T.length name < 6 = KindConstructor -- I# D#
  | Just (c, _) <- T.uncons name, c >= 'A', c <= 'Z', T.all (\x -> x /= ' ' && x /= '/') name = KindConstructor
  | otherwise = KindUnknown

-- | Map a GHC closure type number (rts/include/rts/storage/ClosureTypes.h)
-- to a band kind.
closureTypeKind :: Int -> BandKind
closureTypeKind t
  | t >= 1 && t <= 7 = KindConstructor
  | t >= 8 && t <= 14 = KindFunction
  | t >= 15 && t <= 22 = KindThunk
  | t == 24 || t == 26 = KindThunk -- AP, AP_STACK
  | t == 25 = KindFunction -- PAP
  | otherwise = KindData

-- ---------------------------------------------------------------------------
-- .hp files

loadHp :: FilePath -> IO (Either String HeapProfile)
loadHp path = parseHp <$> readTextFile path

data HpAcc = HpAcc
  { accJob :: !Text
  , accDate :: !Text
  , accCur :: !(Maybe (Double, [(Text, Double)]))
  , accDone :: ![(Double, [(Text, Double)])]
  }

-- | Parse the text of a @.hp@ heap profile.
parseHp :: Text -> Either String HeapProfile
parseHp src
  | not (any ("BEGIN_SAMPLE" `T.isPrefixOf`) ls) = Left "not a heap profile: no BEGIN_SAMPLE lines"
  | otherwise =
      let acc = foldl' step (HpAcc "" "" Nothing []) ls
          names = [n | (_, s) <- accDone acc, (n, _) <- s]
          breakdown = guessBreakdown (accJob acc) names
       in Right (fromSamples (accJob acc) breakdown 0 (const Nothing) (reverse (accDone acc))) {hpRun = runFromHp (accJob acc) (accDate acc)}
  where
    ls = map (T.dropWhileEnd (== '\r')) (T.lines src)
    step acc line
      | Just rest <- T.stripPrefix "JOB " line = acc {accJob = unquote rest}
      | Just rest <- T.stripPrefix "DATE " line = acc {accDate = unquote rest}
      | Just rest <- T.stripPrefix "BEGIN_SAMPLE " line = acc {accCur = Just (readD rest, [])}
      | "END_SAMPLE" `T.isPrefixOf` line = case accCur acc of
          Just (t, bs) -> acc {accCur = Nothing, accDone = (t, bs) : accDone acc}
          Nothing -> acc
      | Just (t, bs) <- accCur acc
      , Just band <- bandLine line =
          acc {accCur = Just (t, band : bs)}
      | otherwise = acc
    bandLine line =
      let (name, val) = T.breakOnEnd "\t" line
       in if T.null name
            then case unsnoc (T.words line) of
              Just (initWs@(_ : _), lastW) -> Just (T.unwords initWs, readD lastW)
              _ -> Nothing
            else Just (T.dropEnd 1 name, readD val)
    unquote = T.copy . T.dropAround (== '"') . T.strip

-- | The breakdown from the RTS flags in the JOB line or, when the program
-- set its flags another way (-with-rtsopts, GHCRTS), from the band names.
guessBreakdown :: Text -> [Text] -> Text
guessBreakdown job names
  | has "-hT" || closureTypes = "closure type"
  | has "-hy" = "type"
  | has "-hd" = "closure description"
  | has "-hr" = "retainer set"
  | has "-hm" = "module"
  | has "-hb" = "biography"
  | has "-hi" = "info table"
  | has "-he" = "era"
  | otherwise = "cost centre"
  where
    has f = f `T.isInfixOf` job
    closureTypes = any (`elem` names) ["ARR_WORDS", "STACK", "TSO", "BLACKHOLE", "MUT_ARR_PTRS_CLEAN"] || any ("THUNK_" `T.isPrefixOf`) names

readD :: Text -> Double
readD t =
  let s = T.strip t
   in fromMaybe 0 (readDouble s <|> (fromIntegral @Integer <$> readDecimal s))

-- ---------------------------------------------------------------------------
-- Analyses

-- | Residency change of a band from the previous sample to sample @i@.
bandDelta :: Band -> Int -> Double
bandDelta b i
  | i <= 0 || i >= VU.length (bandValues b) = 0
  | otherwise = bandValues b VU.! i - bandValues b VU.! (i - 1)

-- | A band whose residency keeps climbing: what a space leak looks like
-- from outside.
data Suspect = Suspect
  { susBand :: !Int
  , susScore :: !Double
    -- ^ 0..1, higher is more leak-like.
  , susGrowth :: !Double
    -- ^ Last over first non-zero residency.
  , susMonotone :: !Double
    -- ^ Fraction of sample-to-sample steps that did not shrink.
  , susSlope :: !Double
    -- ^ Least-squares growth in bytes per second.
  , susSurvived :: !Int
    -- ^ Consecutive trailing samples it has been non-shrinking: each one is
    -- a census taken after a major GC it survived.
  }
  deriving stock (Eq, Show)

-- | How many samples the analyses look at: all of them, except a final
-- census where the heap has collapsed to almost nothing, which is the RTS
-- profiling the heap as the program exits rather than anything the program
-- did.
analysedSamples :: HeapProfile -> Int
analysedSamples hp =
  let tot = hpTotals hp
      n = VU.length tot
      peak = if n == 0 then 0 else VU.maximum tot
   in case VU.unsnoc tot of
        Just (_, lastTot) | n >= 3 && lastTot < 0.05 * peak -> n - 1
        _ -> n

-- | Rank bands by how leak-like their residency curve is. A band qualifies
-- when it grows over the run, rarely shrinks, and ends at least 1.5x where
-- it started. Thunk bands get a boost: a thunk that keeps surviving
-- censuses is never being forced.
leakSuspects :: HeapProfile -> [Suspect]
leakSuspects hp =
  sortOn (Down . susScore) (mapMaybe judge (zip [0 ..] (V.toList (hpBands hp))))
  where
    n = analysedSamples hp
    ts = VU.take n (hpTimes hp)
    judge (i, b)
      | VU.length vs < 3 = Nothing
      | growth < 1.5 || slope <= 0 || mono < 0.6 = Nothing
      | bandPeak b < 0.01 * maxTotal = Nothing
      | otherwise = Just (Suspect i score growth mono slope trailing)
      where
        vs = VU.take n (bandValues b)
        firstV = maybe 0 fst (VU.uncons (VU.dropWhile (<= 0) vs))
        lastV = maybe 0 snd (VU.unsnoc vs)
        growth = if firstV <= 0 then 0 else lastV / firstV
        steps = VU.zipWith (-) (VU.drop 1 vs) vs
        mono = fromIntegral (VU.length (VU.filter (>= 0) steps)) / fromIntegral (max 1 (VU.length steps))
        slope = regressionSlope ts vs
        trailing = VU.length (VU.takeWhile (>= 0) (VU.reverse steps))
        thunkBoost = if bandKind b == KindThunk then 0.15 else 0
        score =
          min 1 $
            0.45 * mono
              + 0.25 * min 1 (logBase 10 growth / 2)
              + 0.15 * min 1 (bandPeak b / max 1 maxTotal)
              + thunkBoost
    maxTotal = if VU.null (hpTotals hp) then 0 else VU.maximum (hpTotals hp)

regressionSlope :: VU.Vector Double -> VU.Vector Double -> Double
regressionSlope xs ys
  | n < 2 || den == 0 = 0
  | otherwise = num / den
  where
    n = fromIntegral (min (VU.length xs) (VU.length ys)) :: Double
    mx = VU.sum xs / n
    my = VU.sum ys / n
    num = VU.sum (VU.zipWith (\x y -> (x - mx) * (y - my)) xs ys)
    den = VU.sum (VU.map (\x -> (x - mx) ^ (2 :: Int)) xs)

-- | A tree of who holds the heap: cost-centre stacks (@-hc@) are split at
-- @/@ and rooted at their outermost frame, so a path reads from the
-- top-level CAF or @main@ down to the allocation site. Retainer sets
-- (@-hr@, labelled @(id)a,b@) attribute their bytes to each retainer.
data RetainTree = RetainTree
  { rtName :: !Text
  , rtBytes :: !Double
  , rtChildren :: ![RetainTree]
  }
  deriving stock (Eq, Show)

-- | Whether the breakdown's band names carry structure 'retainTree' can use.
looksStructured :: HeapProfile -> Bool
looksStructured hp =
  hpBreakdown hp == "retainer set" || V.any (T.any (== '/') . bandName) (hpBands hp)

retainTree :: HeapProfile -> Int -> RetainTree
retainTree hp i = toTree "heap" (M.unionsWith (<>) [nodePath path v | (path, v) <- paths])
  where
    paths =
      [ (path, v)
      | b <- V.toList (hpBands hp)
      , let v = if i < VU.length (bandValues b) then bandValues b VU.! i else 0
      , v > 0
      , path <- bandPaths (hpBreakdown hp) (bandName b)
      ]
    nodePath [] _ = M.empty
    nodePath (p : ps) v = M.singleton p (Node v (nodePath ps v))
    toTree name m =
      RetainTree name (sum [w | Node w _ <- M.elems m]) $
        sortOn (Down . rtBytes) [(toTree k kids) {rtBytes = w} | (k, Node w kids) <- M.toList m]

data Node = Node !Double !(M.Map Text Node)

instance Semigroup Node where
  Node w1 k1 <> Node w2 k2 = Node (w1 + w2) (M.unionWith (<>) k1 k2)

-- | The root-to-leaf paths a band name stands for. Heap-profile labels
-- carry a @(n)@ id prefix, which is dropped.
bandPaths :: Text -> Text -> [[Text]]
bandPaths breakdown name
  | breakdown == "retainer set" =
      [["retained by " <> m] | m <- T.splitOn "," stripped, not (T.null m)]
  | T.any (== '/') stripped = [reverse (filter (not . T.null) (T.splitOn "/" stripped))]
  | otherwise = [[stripped]]
  where
    stripped
      | "(" `T.isPrefixOf` name, (_, rest) <- T.breakOn ")" name, not (T.null rest) = T.drop 1 rest
      | otherwise = name
