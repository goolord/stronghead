-- | Next steps: given what the panels know about the entity in focus and
-- what data is loaded, what to look at or do next, each with a one-line
-- reason.
--
-- The engine follows the four investigations of the integration plan:
--
-- * __Leak.__ A suspect band, then where its closures are allocated (the
--   site and the binding), then two snapshots, then the group of thunks
--   that survives them and what holds it, then a breakpoint where they are
--   created.
-- * __Contention.__ A thread's longest blocked span, then the thread that
--   woke it or the black hole's owner, then what that thread ran (from
--   cost-centre samples), then the binding.
-- * __Hot path.__ A cost centre, then its binding in Core & STG (the
--   badges), then the STG lets and ticky counters, then the heap those
--   lets hold.
-- * __GC pressure.__ A range, then Analyse range, then the top allocators,
--   then the census at its end: when little of what was allocated
--   survives, a bigger nursery rather than a leak.
--
-- The panels say what they know as 'Fact's; the engine never looks at the
-- data itself. Each step is a link to an entity or one of the panels'
-- own actions, found by its label, so following a step takes the same
-- path as a click. Where the data a step needs is missing, the step says
-- what to load and with which flags ('datumRecipe'), as the data sources
-- popover does.
module Stronghead.Next
  ( -- * Data sources
    Datum (..)
  , datumTitle
  , datumAdds
  , datumRecipe
    -- * What is known
  , Have (..)
  , haveNothing
  , Fact (..)
    -- * Steps
  , Step (..)
  , StepDo (..)
  , nextSteps
  , strongStep
  ) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Stronghead.Dump (nameKey)
import Stronghead.Format (fmtBytes, fmtBytesD, fmtFixed, fmtInt, fmtNs, fmtPct, tshow)
import Stronghead.Ref (Ref (..))
import Stronghead.Source (SrcSpan (..), renderSrcSpan)

-- ---------------------------------------------------------------------------
-- Data sources

-- | The kinds of data a run can have (the plan's "what each kind of data
-- adds").
data Datum
  = DTimeline
  | DHeapCensus
  | DHeapSource
  | DCostCentres
  | DSamples
  | DTicky
  | DDumps
  | DLive
  | DLiveStats
  | DLiveGraph
  | DDebugger
  deriving (Eq, Show, Enum, Bounded)

datumTitle :: Datum -> Text
datumTitle = \case
  DTimeline -> "Timeline"
  DHeapCensus -> "Heap census on the clock"
  DHeapSource -> "Heap by source location"
  DCostCentres -> "Cost centres"
  DSamples -> "Cost-centre samples"
  DTicky -> "Ticky counters"
  DDumps -> "Core & STG"
  DLive -> "Live"
  DLiveStats -> "RTS statistics"
  DLiveGraph -> "Live graph"
  DDebugger -> "Debugger"

datumAdds :: Datum -> Text
datumAdds = \case
  DTimeline -> "threads, collections and markers; the program clock"
  DHeapCensus -> "censuses on the timeline, the census ↔ collection links"
  DHeapSource -> "bands that link to source lines and STG lets, the heap gutter"
  DCostCentres -> "time and allocation per line and binding"
  DSamples -> "what each thread ran, on the timeline"
  DTicky -> "entries and allocation per binding and STG let"
  DDumps -> "binding notes, exact links from info tables to STG binders"
  DLive -> "attach, marks and remote actions, threads now"
  DLiveStats -> "allocation, GC and residency charts on program time"
  DLiveGraph -> "retainer paths, surviving thunks, thread stacks"
  DDebugger -> "Break here, values"

-- | What to build with and what to run with to get it.
datumRecipe :: Datum -> (Maybe Text, Maybe Text)
datumRecipe = \case
  DTimeline -> (Just "-threaded -rtsopts", Just "+RTS -l -RTS")
  DHeapCensus -> (Nothing, Just "+RTS -l -hT -i0.1 -RTS")
  DHeapSource -> (Just "-finfo-table-map -fdistinct-constructor-tables", Just "+RTS -l -hi -i0.1 -RTS")
  DCostCentres -> (Just "-prof -fprof-auto", Just "+RTS -p -RTS")
  DSamples -> (Just "-prof -fprof-auto", Just "+RTS -p -l -RTS")
  DTicky -> (Just "-ticky", Just "+RTS -lT -RTS")
  DDumps -> (Just "-ddump-simpl -ddump-stg-final -ddump-to-file", Nothing)
  DLive -> (Just "build-depends: stronghead:agent; main = withAgent defaultAgentConfig ...", Nothing)
  DLiveStats -> (Nothing, Just "+RTS -T -RTS")
  DLiveGraph -> (Just "build-depends: ghc-debug-stub; main = withGhcDebug ... (Unix only)", Nothing)
  DDebugger -> (Nothing, Just "ghci, or cabal repl")

-- ---------------------------------------------------------------------------
-- What is known

-- | What is loaded or connected now.
data Have = Have
  { haveTimeline :: !Bool
  , haveHeapCensus :: !Bool
    -- ^ A heap profile is on show.
  , haveHeapSource :: !Bool
    -- ^ ... and it is broken down by info table (@-hi@).
  , haveCostCentres :: !Bool
    -- ^ A @.prof@ file.
  , haveSamples :: !Bool
    -- ^ The eventlog has cost-centre samples.
  , haveTicky :: !Bool
  , haveDumps :: !Bool
  , haveAgent :: !Bool
    -- ^ The agent is connected and takes commands.
  , haveLiveGraph :: !Bool
    -- ^ A ghc-debug target can be snapshotted.
  , haveSnapshots :: !Int
  }
  deriving (Eq, Show)

haveNothing :: Have
haveNothing = Have False False False False False False False False False 0

-- | One thing a panel knows about an entity, in the terms the engine
-- reasons in. Refs inside are what a step links to.
data Fact
  = FactLeak !Double !Int
    -- ^ A leak suspect: its score (0 to 1) and the censuses it has
    -- survived without shrinking.
  | FactThunks
    -- ^ Its closures are unevaluated thunks.
  | FactAllocated !SrcSpan
    -- ^ Where its closures are allocated.
  | FactInBinding !Text !Text
    -- ^ The binding that allocates it: module and name key.
  | FactSurviving !Ref !Int
    -- ^ A group of thunks that survives between snapshots and matches it,
    -- and for how many snapshots.
  | FactHeldBy !Ref
    -- ^ What retains it in the latest snapshot: a thread or a closure.
  | FactBlocked !Ref !Word64 !Text
    -- ^ A thread's longest blocked span (a range), its length in ns, and
    -- what it was blocked on.
  | FactWokenBy !Ref
    -- ^ The thread that woke it from that span.
  | FactOwner !Ref
    -- ^ The thread evaluating the black hole it blocked on.
  | FactRan !Ref !Int
    -- ^ The cost centre most often on top of its stack, and how many
    -- samples.
  | FactCost !Double !Double
    -- ^ Its inherited share of time and of allocation from the @.prof@,
    -- in percent.
  | FactBadges !Bool !Int !Int
    -- ^ Worker/wrapper fired; boxing sites in Core; lets in STG.
  | FactTicky !Word64 !Word64
    -- ^ Entries, and bytes allocated.
  | FactSiteHeap !Ref !Double
    -- ^ A band its lets allocate, and the bytes it holds at the cursor.
  | FactGC !Double !Int
    -- ^ For a range: the share of it spent paused for collections (0 to
    -- 1), and how many collections.
  | FactAllocation !Word64 !(Maybe Word64) !(Maybe Word64)
    -- ^ For a range: bytes allocated in it, and live bytes at its start
    -- and end.
  | FactTopAllocator !Ref !Word64
    -- ^ For a range: the binding that allocated most in it (ticky), and
    -- how much.
  | FactCensusAtEnd !Ref
    -- ^ For a range: the last census inside it.
  | FactTopSuspect !Ref
    -- ^ The leading leak suspect of the profile on show.
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Steps

-- | A suggestion: what to do, why, and how.
data Step = Step
  { stepTitle :: !Text
  , stepReason :: !Text
    -- ^ One line.
  , stepDo :: !StepDo
  , stepStrong :: !Bool
    -- ^ Worth showing in a peek: the evidence points this way.
  }
  deriving (Eq, Show)

data StepDo
  = StepGo !Ref
    -- ^ Go to an entity.
  | StepAct !Ref !Text
    -- ^ Run the action, offered for that entity by any panel (or as a
    -- palette command), whose label contains this text, ignoring case.
  | StepLoad !Datum
    -- ^ The data the step needs isn't loaded: how to get it.
  | StepTry !Text
    -- ^ A flag to try on the next run.
  deriving (Eq, Show)

-- | The first strong step, for a peek.
strongStep :: [Step] -> Maybe Step
strongStep = find stepStrong

-- | What to do next about an entity, in the order an investigation goes.
nextSteps :: Have -> Ref -> [Fact] -> [Step]
nextSteps have ref facts = case ref of
  RBand _ -> leakSteps have ref facts
  RThunkGroup _ -> survivorSteps ref facts
  RClosure _ _ -> survivorSteps ref facts
  RThread _ _ -> contentionSteps have facts
  RCostCentre m l -> hotSteps have m l facts
  RBinding m k -> bindingSteps have ref m k facts
  RRange {} -> pressureSteps have ref facts
  RSource _ -> [Step "Break here" "stop GHCi on this line and look at the values in scope" (StepAct ref "break here") False]
  _ -> []

-- | A suspect band: where it comes from, what survives, what holds it,
-- and where to stop.
leakSteps :: Have -> Ref -> [Fact] -> [Step]
leakSteps have ref facts =
  provenance ++ (if suspect then snapshots ++ held ++ breakStep else [])
  where
    leak = listToMaybe [(s, n) | FactLeak s n <- facts]
    suspect = maybe False (const True) leak
    strong = maybe False ((>= 0.6) . fst) leak
    thunks = FactThunks `elem` facts
    site = listToMaybe [sp | FactAllocated sp <- facts]
    binding = listToMaybe [(m, k) | FactInBinding m k <- facts]
    surviving = listToMaybe [(g, n) | FactSurviving g n <- facts]
    holder = listToMaybe [r | FactHeldBy r <- facts]
    what = if thunks then "these thunks are" else "these closures are"
    growing = case leak of
      Just (_, n) | n > 0 -> ", growing for " <> tshow n <> " censuses"
      _ -> ""
    provenance =
      [ Step "Go to where they are allocated" (what <> " allocated at " <> shortSpan sp <> growing) (StepGo (RSource sp)) strong
      | Just sp <- [site]
      ]
        ++ [ Step "Open the binding in Core & STG" ("its STG lets show what " <> k <> " allocates; its badges show boxing") (StepGo (RBinding m k)) (strong && maybe True (const False) site)
           | Just (m, k) <- [binding]
           ]
        ++ [ Step "Profile by info table" "an -hi profile names the source line and binding that allocate each band" (StepLoad DHeapSource) strong
           | maybe True (const False) site
           , maybe True (const False) binding
           , not (haveHeapSource have)
           ]
    snapshots = case surviving of
      Just (g, n) -> [Step "Open the surviving group" ("its thunks have survived " <> tshow n <> " snapshots: nothing forces them") (StepGo g) True]
      Nothing
        | haveLiveGraph have ->
            [Step "Snapshot twice, 2 s apart" "thunks that survive both snapshots are the ones nothing forces, and the live graph shows what holds them" (StepAct ref "snapshot twice") (not (maybe False (const True) site) && strong)]
        | otherwise -> [Step "Snapshot the running program" "two ghc-debug snapshots show which thunks survive and the path that holds them" (StepLoad DLiveGraph) False]
    held = [Step "Go to what holds them" "the end of the retainer path: the root that keeps them alive" (StepGo r) True | Just r <- [holder]]
    breakStep =
      [ Step "Break where allocated" ("stop GHCi at " <> shortSpan sp <> " and look at the values as they are built") (StepAct ref "break where allocated") False
      | Just sp <- [site]
      ]

-- | A group of surviving thunks, or a closure in a snapshot.
survivorSteps :: Ref -> [Fact] -> [Step]
survivorSteps ref facts =
  [Step "Go to what holds it" "the end of the retainer path: the root that keeps it alive" (StepGo r) True | r <- take 1 [r | FactHeldBy r <- facts]]
    ++ [ Step "Go to where it is created" ("its code is at " <> shortSpan sp) (StepGo (RSource sp)) False
       | sp <- take 1 [sp | FactAllocated sp <- facts]
       ]
    ++ [ Step "Break where created" "stop GHCi where these closures are built and look at the values" (StepAct ref "break where created") False
       | not (null [() | FactAllocated _ <- facts])
       ]

-- | A thread: what it waited for, who let it go, what that one ran.
contentionSteps :: Have -> [Fact] -> [Step]
contentionSteps have facts = blockedSteps ++ ranSteps ++ bindingStep
  where
    blocked = listToMaybe [(r, d, why) | FactBlocked r d why <- facts]
    long = maybe False (\(_, d, _) -> d >= 10000000) blocked
    owner = listToMaybe [r | FactOwner r <- facts]
    waker = listToMaybe [r | FactWokenBy r <- facts]
    ran = listToMaybe [(r, n) | FactRan r n <- facts]
    blockedSteps = case blocked of
      Nothing -> []
      Just (r, d, why) ->
        Step "Show its longest blocked span" (fmtNs (fromIntegral d) <> " blocked on " <> why) (StepGo r) (long && maybe True (const False) owner && maybe True (const False) waker)
          : [Step "Go to the black hole's owner" "it was evaluating the thunk this thread waited for" (StepGo o) long | Just o <- [owner]]
          ++ [Step "Go to the thread that woke it" "it posted the wakeup that ended the span: it held what this thread waited for" (StepGo w) (long && maybe True (const False) owner) | Just w <- [waker]]
    ranSteps = case ran of
      Just (cc, n) -> [Step "See what it ran" ("on top of its stack in " <> fmtInt n <> " cost-centre samples") (StepGo cc) (maybe True (const False) blocked)]
      Nothing
        | not (haveSamples have) -> [Step "Sample what it runs" "a profiled build run with -p -l records the code each thread ran" (StepLoad DSamples) False]
        | otherwise -> []
    bindingStep = case ran of
      Just (RCostCentre m l, _)
        | not (T.null m) ->
            if haveDumps have
              then [Step "Open the binding in Core & STG" ("where " <> l <> " is compiled: its badges and lets") (StepGo (RBinding m (nameKey l))) False]
              else [Step "Dump Core and STG" ("the dumps show what the optimiser made of " <> l) (StepLoad DDumps) False]
      _ -> []

-- | A cost centre: its binding, as the optimiser left it.
hotSteps :: Have -> Text -> Text -> [Fact] -> [Step]
hotSteps have m l facts = costStep ++ bindingStep
  where
    cost = listToMaybe [(t, a) | FactCost t a <- facts]
    hot = maybe False (\(t, a) -> t >= 10 || a >= 10) cost
    share = case cost of
      Just (t, a) -> fmtFixed 1 t <> "% of the time and " <> fmtFixed 1 a <> "% of the allocation under it; "
      Nothing -> ""
    costStep =
      [ Step "Profile time and allocation" "a .prof gives each cost centre its share of time and allocation" (StepLoad DCostCentres) False
      | maybe True (const False) cost
      , not (haveCostCentres have)
      ]
    bindingStep
      | T.null m = []
      | haveDumps have = [Step "Open the binding in Core & STG" (share <> "its badges show whether it was unboxed (W/W), what it boxes and what its lets allocate") (StepGo (RBinding m (nameKey l))) hot]
      | otherwise = [Step "Dump Core and STG" (share <> "the dumps show what the optimiser made of it") (StepLoad DDumps) hot]

-- | A binding: its STG lets and counters, and the heap they hold.
bindingSteps :: Have -> Ref -> Text -> Text -> [Fact] -> [Step]
bindingSteps have ref _ _ facts = case badges of
  Nothing
    | haveDumps have -> heapSteps
    | otherwise -> Step "Dump Core and STG" "the dumps show worker/wrapper, boxing and the closures STG allocates" (StepLoad DDumps) False : heapSteps
  Just (ww, boxes, lets) ->
    [ Step "Read its STG lets" (letsReason ww boxes lets) (StepGo ref) (lets > 0 && (heapHeld > 0 || maybe False ((> 0) . snd) ticky))
    | lets > 0 || boxes > 0
    ]
      ++ [ Step "Count its entries and allocation" "a ticky build counts how often each let runs and what it allocates" (StepLoad DTicky) False
         | lets > 0
         , not (haveTicky have)
         ]
      ++ heapSteps
      ++ [Step "Break on entry" "stop GHCi when it is called and look at its arguments" (StepAct ref "break on entry") False]
  where
    badges = listToMaybe [(w, b, l) | FactBadges w b l <- facts]
    ticky = listToMaybe [(e, a) | FactTicky e a <- facts]
    sites = [(b, v) | FactSiteHeap b v <- facts]
    heapHeld = sum (map snd sites)
    letsReason ww boxes lets =
      T.intercalate "; " $
        [tshow lets <> " lets allocate closures" | lets > 0]
          ++ [tshow boxes <> " boxing sites" | boxes > 0]
          ++ ["no worker/wrapper split, so arguments stay boxed" | boxes > 0, not ww]
          ++ ["ticky: " <> fmtInt e <> " entries, " <> fmtBytes a | Just (e, a) <- [ticky]]
    heapSteps = case sites of
      (b, v) : _ ->
        [Step "Go to the heap its lets hold" (fmtBytesD v <> " live at the cursor in closures it allocates") (StepGo b) (v > 0)]
      []
        | haveHeapCensus have, not (haveHeapSource have) -> [Step "Profile by info table" "an -hi profile ties each let to the heap its closures hold" (StepLoad DHeapSource) False]
        | not (haveHeapCensus have) -> [Step "Profile the heap by info table" "an -hi profile ties each let to the heap its closures hold" (StepLoad DHeapSource) False]
        | otherwise -> []

-- | A range: how much was collection, who allocated, and what survived.
pressureSteps :: Have -> Ref -> [Fact] -> [Step]
pressureSteps have ref facts =
  analyse ++ allocators ++ census ++ survival
  where
    gc = listToMaybe [(s, n) | FactGC s n <- facts]
    heavy = maybe False ((>= 0.15) . fst) gc
    alloc = listToMaybe [(a, l0, l1) | FactAllocation a l0 l1 <- facts]
    top = listToMaybe [(r, b) | FactTopAllocator r b <- facts]
    ran = listToMaybe [(r, n) | FactRan r n <- facts]
    atEnd = listToMaybe [c | FactCensusAtEnd c <- facts]
    suspect = listToMaybe [b | FactTopSuspect b <- facts]
    analyse =
      [ Step "Analyse range" (fmtPct s <> " of it paused for " <> fmtInt n <> " collections") (StepAct ref "analyse range") heavy
      | Just (s, n) <- [gc]
      ]
    allocators = case (top, ran) of
      (Just (r, b), _) -> [Step "Go to the top allocator" (fmtBytes b <> " allocated in the range, by its ticky counters") (StepGo r) heavy]
      (Nothing, Just (r, n)) -> [Step "See what ran" ("on top of the stack in " <> fmtInt n <> " samples: where the time went, not the allocation") (StepGo r) False]
      (Nothing, Nothing)
        | not (haveTicky have) -> [Step "Count allocation per binding" "a ticky build gives each binding's allocation inside the range" (StepLoad DTicky) False]
        | otherwise -> []
    census = case atEnd of
      Just c -> [Step "Look at the census at its end" "what was still alive after the range's last major collection" (StepGo c) False]
      Nothing
        | not (haveHeapCensus have) -> [Step "Take heap censuses" "a census at the range's end shows what of the allocation survived" (StepLoad DHeapCensus) False]
        | otherwise -> []
    survival = case alloc of
      Just (a, Just l0, Just l1) | a > 0 ->
        let grew = fromIntegral l1 - fromIntegral l0 :: Double
            frac = grew / fromIntegral a
         in if frac < 0.05
              then
                [ Step
                    "Try a bigger nursery"
                    (fmtBytes a <> " allocated, live grew by " <> fmtBytesD (max 0 grew) <> ": most of it dies young, so the time goes on collecting it, and a bigger nursery collects less often")
                    (StepTry "+RTS -A64m -RTS")
                    heavy
                ]
              else
                [ Step "Look at what survives" ("live grew by " <> fmtBytesD grew <> " of the " <> fmtBytes a <> " allocated: something keeps it") (maybe (StepLoad DHeapCensus) StepGo (maybe atEnd Just suspect)) (frac >= 0.2)
                ]
      _ -> []

-- | @Main.hs:82@.
shortSpan :: SrcSpan -> Text
shortSpan sp =
  let file = T.takeWhileEnd (\c -> c /= '/' && c /= '\\') (ssFile sp)
   in if T.null file then renderSrcSpan sp else file <> ":" <> tshow (ssLine sp)
