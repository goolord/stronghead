-- | Baseline comparison and diffs for heap profiles, @.prof@ cost centres,
-- and Core/STG bindings.
module Stronghead.Diff
  ( -- * Baselines
    BaselinePaths (..)
  , noBaselinePaths
  , Baseline (..)
    -- * Heap profiles
  , BandFigures (..)
  , BandMatch (..)
  , BandStatus (..)
  , BandChange (..)
  , bandFigures
  , diffHeap
  , changeFor
  , bandSite
  , figuresTitle
    -- * Cost centres
  , CostChange (..)
  , diffCosts
    -- * Bindings
  , BindingSummary (..)
  , summariseModule
  , loadBindingSummaries
  , BindingChange (..)
  , diffBindings
    -- * Readouts
  , relChange
  , fmtChange
  ) where

import Control.Monad.Extra (concatMapM)
import Data.Containers.ListUtils (nubOrd)
import Data.Either.Extra (eitherToMaybe)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

import Stronghead.Dump
import Stronghead.Eventlog.Model (Summary)
import Stronghead.Format (fmtFixed, tshow)
import Stronghead.HeapProfile
import Stronghead.Join (CostTotal (..), bandTitle)
import Stronghead.Source (SrcSpan, renderSrcLoc, srcLocSpan)

-- ---------------------------------------------------------------------------
-- Baselines

-- | Where a baseline's artifacts are, for sessions.
data BaselinePaths = BaselinePaths
  { blEventlog :: !(Maybe FilePath)
  , blHeapProfile :: !(Maybe FilePath)
    -- ^ A @.hp@ file, when that was the heap profile on show.
  , blCostCentres :: !(Maybe FilePath)
  , blDumps :: !(Maybe FilePath)
  }
  deriving stock (Eq, Ord, Show)

noBaselinePaths :: BaselinePaths
noBaselinePaths = BaselinePaths Nothing Nothing Nothing Nothing

-- | A baseline as loaded.
data Baseline = Baseline
  { baPaths :: !BaselinePaths
  , baSummary :: !(Maybe Summary)
  , baHeap :: !(Maybe HeapProfile)
  , baCosts :: !(Maybe [CostTotal])
  , baBindings :: !(Maybe [BindingSummary])
  }

-- ---------------------------------------------------------------------------
-- Heap profiles

-- | A band's numbers in one profile.
data BandFigures = BandFigures
  { bfName :: !Text
  , bfPeak :: !Double
  , bfEnd :: !Double
    -- ^ At the last census the analyses count ('analysedSamples').
  , bfGrowth :: !Double
    -- ^ From the first census to that one, in bytes.
  , bfScore :: !(Maybe Double)
    -- ^ Its leak score, when it is a suspect.
  , bfKind :: !BandKind
  , bfProv :: !(Maybe InfoProv)
  }
  deriving stock (Eq, Show)

data BandMatch = SameName | SameSource | Unmatched
  deriving stock (Eq, Show)

data BandStatus = BandGone | BandNew | BandShrank | BandGrew | BandSame
  deriving stock (Eq, Show)

-- | One band across the two profiles.
data BandChange = BandChange
  { bcBase :: !(Maybe BandFigures)
  , bcCur :: !(Maybe BandFigures)
  , bcMatch :: !BandMatch
  , bcStatus :: !BandStatus
  }
  deriving stock (Eq, Show)

-- | Every band's figures, in the profile's order (largest peak first).
bandFigures :: HeapProfile -> [BandFigures]
bandFigures hp =
  [ BandFigures
      { bfName = bandName b
      , bfPeak = bandPeak b
      , bfEnd = if n > 0 then vs VU.! (n - 1) else 0
      , bfGrowth = if n > 0 then vs VU.! (n - 1) - vs VU.! 0 else 0
      , bfScore = M.lookup i scores
      , bfKind = bandKind b
      , bfProv = bandProv b
      }
  | (i, b) <- zip [0 ..] (V.toList (hpBands hp))
  , let vs = bandValues b
        n = min (analysedSamples hp) (VU.length vs)
  ]
  where
    scores = M.fromList [(susBand s, susScore s) | s <- leakSuspects hp]

-- | Where a band's closures come from, when an info-table breakdown says:
-- the binding, module, source and kind of closure, which survive a
-- rebuild where the table's address does not.
provKey :: BandFigures -> Maybe (Text, Text, Text, BandKind, Text)
provKey bf = (\ip -> (ipModule ip, ipLabel ip, renderSrcLoc (ipSrc ip), ipKind ip, ipType ip)) <$> bfProv bf

-- | Match the bands of a baseline and the current profile: by name, then
-- by provenance. Biggest change in peak first.
diffHeap :: HeapProfile -> HeapProfile -> [BandChange]
diffHeap base cur =
  sortOn (Down . magnitude) (matched ++ gone)
  where
    bs = bandFigures base
    cs = bandFigures cur
    byName = M.fromList [(bfName b, b) | b <- bs]
    byProv = M.fromListWith (\_ old -> old) [(k, b) | b <- bs, Just k <- [provKey b]]
    pick c = case M.lookup (bfName c) byName of
      Just b -> Just (b, SameName)
      Nothing -> (,SameSource) <$> (provKey c >>= (`M.lookup` byProv))
    pairs = [(c, pick c) | c <- cs]
    -- A baseline band matched twice (two current bands with one source)
    -- counts for the first only.
    claimed = S.fromList [bfName b | (_, Just (b, _)) <- pairs]
    matched =
      reverse . snd $
        foldl'
          ( \(used, acc) (c, m) -> case m of
              Just (b, how) | bfName b `S.notMember` used -> (S.insert (bfName b) used, BandChange (Just b) (Just c) how (status (Just b) (Just c)) : acc)
              _ -> (used, BandChange Nothing (Just c) Unmatched BandNew : acc)
          )
          (S.empty, [])
          pairs
    gone = [BandChange (Just b) Nothing Unmatched BandGone | b <- bs, bfName b `S.notMember` claimed]
    magnitude ch = abs (maybe 0 bfPeak (bcCur ch) - maybe 0 bfPeak (bcBase ch))

-- | Gone when it holds less than a hundredth of what it held (the sample's
-- leak, fixed, keeps 64 bytes of its 40 MB); smaller or larger past a
-- fifth either way.
status :: Maybe BandFigures -> Maybe BandFigures -> BandStatus
status mb mc = case (mb, mc) of
  (Just b, Just c)
    | bfPeak c < 0.01 * bfPeak b -> BandGone
    | bfPeak c < 0.8 * bfPeak b -> BandShrank
    | bfPeak c > 1.25 * bfPeak b -> BandGrew
    | otherwise -> BandSame
  (Just _, Nothing) -> BandGone
  (Nothing, _) -> BandNew

-- | The change for one band of the current profile, by its name.
changeFor :: [BandChange] -> Text -> Maybe BandChange
changeFor chs name = listToMaybe [ch | ch <- chs, fmap bfName (bcCur ch) == Just name]

-- ---------------------------------------------------------------------------
-- Cost centres

-- | One cost centre across the two @.prof@ files.
data CostChange = CostChange
  { cgModule :: !Text
  , cgName :: !Text
  , cgBase :: !(Maybe CostTotal)
  , cgCur :: !(Maybe CostTotal)
  }
  deriving stock (Eq, Show)

-- | Cost centres matched by module and label; biggest change in time plus
-- allocation (inherited) first.
diffCosts :: [CostTotal] -> [CostTotal] -> [CostChange]
diffCosts base cur =
  sortOn (Down . magnitude) . M.elems $
    M.unionWith
      (\a b -> a {cgCur = cgCur b})
      (M.fromList [(key c, CostChange (ctModule c) (ctName c) (Just c) Nothing) | c <- base])
      (M.fromList [(key c, CostChange (ctModule c) (ctName c) Nothing (Just c)) | c <- cur])
  where
    key c = (ctModule c, ctName c)
    magnitude ch =
      abs (maybe 0 ctInhTime (cgCur ch) - maybe 0 ctInhTime (cgBase ch))
        + abs (maybe 0 ctInhAlloc (cgCur ch) - maybe 0 ctInhAlloc (cgBase ch))

-- ---------------------------------------------------------------------------
-- Bindings

-- | What the optimiser left of one binding.
data BindingSummary = BindingSummary
  { bsModule :: !Text
  , bsKey :: !Text
  , bsWorker :: !Bool
    -- ^ A @$w@ worker exists for it.
  , bsBoxes :: !Int
    -- ^ Lines in its Core that apply a boxing constructor.
  , bsLets :: !Int
    -- ^ @let@s in its STG.
  , bsSize :: !(Maybe Int)
    -- ^ Terms in its Core, from @-- RHS size@, summed over its pieces.
  }
  deriving stock (Eq, Show)

-- | A module's bindings from its Core and STG dumps, keyed as Core & STG
-- keys them ('nameKey'), so a worker and its wrapper are one binding.
summariseModule :: Text -> Maybe CodeFile -> Maybe CodeFile -> [BindingSummary]
summariseModule m core stg =
  [ BindingSummary
      { bsModule = m
      , bsKey = k
      , bsWorker = any (("$w" `T.isPrefixOf`) . stripQualifier . bdName) (cs ++ ss)
      , bsBoxes = sum (map (length . nBoxing . bdNotes) cs)
      , bsLets = sum (map (length . nAllocs . bdNotes) ss)
      , bsSize = case mapMaybe (\b -> nSize (bdNotes b) >>= terms) cs of
          [] -> Nothing
          xs -> Just (sum xs)
      }
  | k <- keys
  , not (T.null k)
  , not (isTypeableBinding k)
  , let cs = find' core k
        ss = find' stg k
  ]
  where
    keys = nubOrd [bdKey b | Just cf <- [core, stg], b <- V.toList (cfBindings cf)]
    find' mcf k = case mcf of
      Nothing -> []
      Just cf -> [cfBindings cf V.! i | i <- M.findWithDefault [] k (cfByKey cf)]
    -- "{terms: 12, types: 3, ...}"
    terms s = case T.breakOn "terms:" s of
      (_, rest) | not (T.null rest) -> fst <$> eitherToMaybe (TR.decimal (T.stripStart (T.drop 6 rest)))
      _ -> Nothing

-- | Every module's bindings in a dump folder.
loadBindingSummaries :: FilePath -> IO [BindingSummary]
loadBindingSummaries root = do
  arts <- discoverArtifacts root
  concatMapM
    ( \a -> do
        core <- traverse (loadCodeFile KindCore) (maCore a)
        stg <- traverse (loadCodeFile KindStg) (maStg a)
        pure (summariseModule (maModule a) (core >>= eitherToMaybe) (stg >>= eitherToMaybe))
    )
    arts

-- | One binding across the two folders, with what changed in words.
data BindingChange = BindingChange
  { bgModule :: !Text
  , bgKey :: !Text
  , bgBase :: !(Maybe BindingSummary)
  , bgCur :: !(Maybe BindingSummary)
  , bgChanges :: ![Text]
  }
  deriving stock (Eq, Show)

-- | The bindings whose badges or size changed, or that appeared or went,
-- by module and key.
diffBindings :: [BindingSummary] -> [BindingSummary] -> [BindingChange]
diffBindings base cur =
  [ BindingChange m k b c changes
  | ((m, k), (b, c)) <- M.toList both
  , let changes = describe b c
  , not (null changes)
  ]
  where
    key s = (bsModule s, bsKey s)
    both =
      M.unionWith
        (\(b, _) (_, c) -> (b, c))
        (M.fromList [(key s, (Just s, Nothing)) | s <- base])
        (M.fromList [(key s, (Nothing, Just s)) | s <- cur])
    describe mb mc = case (mb, mc) of
      (Just _, Nothing) -> ["gone: inlined or removed"]
      (Nothing, Just _) -> ["new"]
      (Just b, Just c) ->
        concat
          [ [if bsWorker c then "worker/wrapper now" else "worker/wrapper gone" | bsWorker b /= bsWorker c]
          , [count "boxing sites" (bsBoxes b) (bsBoxes c) | bsBoxes b /= bsBoxes c]
          , [count "lets" (bsLets b) (bsLets c) | bsLets b /= bsLets c]
          , ["size " <> size (bsSize b) <> " → " <> size (bsSize c) <> " terms" | bsSize b /= bsSize c, not (isNothing (bsSize b) && isNothing (bsSize c))]
          ]
      (Nothing, Nothing) -> []
    count what a b = what <> " " <> tshow a <> " → " <> tshow b
    size = maybe "?" tshow

-- ---------------------------------------------------------------------------
-- Readouts

-- | The change from a baseline value to the current one, as a fraction;
-- 'Nothing' when the baseline is zero.
relChange :: Double -> Double -> Maybe Double
relChange base cur
  | base == 0 = Nothing
  | otherwise = Just ((cur - base) / abs base)

-- | @+12%@, @−80%@, @±0%@, against the baseline.
fmtChange :: Double -> Double -> Text
fmtChange base cur = case relChange base cur of
  Nothing
    | cur == 0 -> "±0%"
    | otherwise -> "new"
  Just r
    | abs r < 0.0005 -> "±0%"
    | r > 0 -> "+" <> pct r
    | otherwise -> "−" <> pct (negate r)
  where
    pct r = (if r >= 0.1 then fmtFixed 0 else fmtFixed 1) (r * 100) <> "%"

-- | Where a band's closures are allocated, for a link to a band that is
-- gone.
bandSite :: BandFigures -> Maybe SrcSpan
bandSite bf = bfProv bf >>= srcLocSpan . ipSrc

-- | A band's name for people: from its provenance, as 'bandTitle' writes
-- it, else its name.
figuresTitle :: BandFigures -> Text
figuresTitle bf = bandTitle (Band (bfName bf) VU.empty (bfPeak bf) (bfKind bf) (bfProv bf) Nothing)
