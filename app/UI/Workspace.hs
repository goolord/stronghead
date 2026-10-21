-- | Mixed workspaces: every pane of every panel, registered by kind, and
-- the workbenches that arrange them.
--
-- A workbench is a nano-ui pane grid whose pane ids are pane kinds: the
-- id of a pane is 'pkId' of what it shows, so a tree says by itself what
-- is in it, and a kind appears at most once in a workbench. Split ids are
-- 1000 and up. Any kind can be added to any panel's workbench; a pane is
-- moved within one by dragging its header ('paneDragHandle') and to
-- another panel through its menu, or popped out into a floating window.
--
-- A pane draws from its panel's state and from what the loaders returned
-- this frame, never from its panel being on screen: each panel offers a
-- 'PaneDraw' built from both, and whichever workbench holds the pane calls
-- it. A popped-out kind is drawn only in its window; a workbench that
-- also holds it shows where it went instead, so no pane is drawn twice.
--
-- The five panels' arrangements are presets of this registry, next to the
-- user's presets ("Leak hunt", "Latency" and any saved ones). Trees are
-- written as text for sessions: @(v 0.74 (h 0.5 tl.caps tl.threads)
-- inspector)@, where @v@ puts its two sides next to each other and @h@
-- stacks them.
module UI.Workspace
  ( -- * Pane kinds
    PaneKind (..)
  , paneKinds
  , paneKind
  , paneKindNamed
  , paneTitle
  , paneTlCaps
  , paneTlThreads
  , paneTlOverview
  , paneTlMarkers
  , paneHeapChart
  , paneHeapBands
  , paneHeapAnalysis
  , paneHeapGraph
  , paneHeapCosts
  , paneCoreBindings
  , paneCoreNotes
  , paneCoreSource
  , paneCoreCore
  , paneCoreStg
  , paneDbgSource
  , paneDbgLocals
  , paneDbgConsole
  , paneDbgBreaks
  , paneLiveCharts
  , paneLiveThreads
  , paneInspector
  , panePinboard
  , paneWatchers
  , paneDiffHeap
  , paneDiffCosts
  , paneDiffBindings
  , timelineTimePanes
    -- * Trees
  , treeKinds
  , addPane
  , removePane
  , hasPane
  , encodeTree
  , decodeTree
    -- * Presets
  , Preset (..)
  , panelPreset
  , heapGraphPreset
  , heapCostsPreset
  , userPresets
    -- * Drawing
  , PaneDraw
  , Panes
  , orPanes
  , Bench (..)
  , BenchPlace (..)
  , Popped (..)
  , workspace
  , paneEmpty
  ) where

import Control.Monad (void, when)
import Data.Char (isSpace)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Word (Word64)
import NanoUI
import Numeric (showFFloat)
import UI.Common
import UI.Link (Panel (..))

-- ---------------------------------------------------------------------------
-- Pane kinds

-- | A kind of pane: what it shows, the panel whose state it draws from
-- ('Nothing' for the shell's own panes), and whether it is a time view
-- that takes the Timeline's window when placed with it.
data PaneKind = PaneKind
  { pkId :: !Word64
  , pkKey :: !Text
    -- ^ Its name in session files.
  , pkTitle :: !Text
    -- ^ Its name in menus.
  , pkPanel :: !(Maybe Panel)
  , pkTime :: !Bool
  }
  deriving (Eq, Show)

paneTlCaps, paneTlThreads, paneTlOverview, paneTlMarkers :: Word64
paneTlCaps = 101
paneTlThreads = 102
paneTlOverview = 103
paneTlMarkers = 104

paneHeapChart, paneHeapBands, paneHeapAnalysis, paneHeapGraph, paneHeapCosts :: Word64
paneHeapChart = 201
paneHeapBands = 202
paneHeapAnalysis = 203
paneHeapGraph = 204
paneHeapCosts = 205

paneCoreBindings, paneCoreNotes, paneCoreSource, paneCoreCore, paneCoreStg :: Word64
paneCoreBindings = 301
paneCoreNotes = 302
paneCoreSource = 303
paneCoreCore = 304
paneCoreStg = 305

paneDbgSource, paneDbgLocals, paneDbgConsole, paneDbgBreaks :: Word64
paneDbgSource = 401
paneDbgLocals = 402
paneDbgConsole = 403
paneDbgBreaks = 404

paneLiveCharts, paneLiveThreads :: Word64
paneLiveCharts = 501
paneLiveThreads = 502

paneInspector, panePinboard, paneWatchers, paneDiffHeap, paneDiffCosts, paneDiffBindings :: Word64
paneInspector = 901
panePinboard = 902
paneWatchers = 903
paneDiffHeap = 904
paneDiffCosts = 905
paneDiffBindings = 906

paneKinds :: [PaneKind]
paneKinds =
  [ PaneKind paneTlCaps "tl.caps" "Capabilities" (Just PTimeline) True
  , PaneKind paneTlThreads "tl.threads" "Threads" (Just PTimeline) True
  , PaneKind paneTlOverview "tl.overview" "Timeline overview" (Just PTimeline) False
  , PaneKind paneTlMarkers "tl.markers" "Markers" (Just PTimeline) False
  , PaneKind paneHeapChart "heap.chart" "Heap residency" (Just PHeap) True
  , PaneKind paneHeapBands "heap.bands" "Heap bands" (Just PHeap) False
  , PaneKind paneHeapAnalysis "heap.analysis" "Heap analysis" (Just PHeap) False
  , PaneKind paneHeapGraph "heap.graph" "Live graph" (Just PHeap) False
  , PaneKind paneHeapCosts "heap.costs" "Cost centres" (Just PHeap) False
  , PaneKind paneCoreBindings "core.bindings" "Bindings" (Just PCore) False
  , PaneKind paneCoreNotes "core.notes" "Optimiser notes" (Just PCore) False
  , PaneKind paneCoreSource "core.source" "Source" (Just PCore) False
  , PaneKind paneCoreCore "core.core" "Core" (Just PCore) False
  , PaneKind paneCoreStg "core.stg" "STG" (Just PCore) False
  , PaneKind paneDbgSource "debugger.source" "Debugger source" (Just PDebugger) False
  , PaneKind paneDbgLocals "debugger.locals" "Local bindings" (Just PDebugger) False
  , PaneKind paneDbgConsole "debugger.console" "GHCi console" (Just PDebugger) False
  , PaneKind paneDbgBreaks "debugger.breaks" "Breakpoints" (Just PDebugger) False
  , PaneKind paneLiveCharts "live.charts" "Live charts" (Just PLive) True
  , PaneKind paneLiveThreads "live.threads" "Live threads" (Just PLive) False
  , PaneKind paneInspector "inspector" "Inspector" Nothing False
  , PaneKind panePinboard "pinboard" "Pinboard" Nothing False
  , PaneKind paneWatchers "watchers" "Watchers" Nothing False
  , PaneKind paneDiffHeap "diff.heap" "Heap diff" Nothing False
  , PaneKind paneDiffCosts "diff.costs" "Cost-centre diff" Nothing False
  , PaneKind paneDiffBindings "diff.bindings" "Binding diff" Nothing False
  ]

paneKind :: Word64 -> Maybe PaneKind
paneKind i = find ((== i) . pkId) paneKinds

paneKindNamed :: Text -> Maybe PaneKind
paneKindNamed k = find ((== k) . pkKey) paneKinds

paneTitle :: Word64 -> Text
paneTitle i = maybe ("pane " <> T.pack (show i)) pkTitle (paneKind i)

-- | The Timeline's panes that draw its window: a time pane placed with one
-- of them takes the same window.
timelineTimePanes :: [Word64]
timelineTimePanes = [paneTlCaps, paneTlThreads]

-- ---------------------------------------------------------------------------
-- Trees

-- | The pane kinds in a tree, depth-first.
treeKinds :: GridNode -> [Word64]
treeKinds = \case
  Pane i -> [i]
  Split _ _ _ a b -> treeKinds a ++ treeKinds b

hasPane :: Word64 -> Maybe GridNode -> Bool
hasPane k = maybe False (elem k . treeKinds)

splitIds :: GridNode -> [Word64]
splitIds = \case
  Pane _ -> []
  Split i _ _ a b -> i : splitIds a ++ splitIds b

-- | Add a pane beside the whole arrangement, on the right, taking a third
-- of it; a kind already there stays where it is.
addPane :: Word64 -> Maybe GridNode -> GridNode
addPane k = \case
  Nothing -> Pane k
  Just t
    | k `elem` treeKinds t -> t
    | otherwise -> Split (maximum (999 : splitIds t) + 1) AxisV 0.68 t (Pane k)

-- | Take a pane out; its neighbour takes its place. 'Nothing' once the last
-- pane has gone.
removePane :: Word64 -> GridNode -> Maybe GridNode
removePane k = \case
  Pane i -> if i == k then Nothing else Just (Pane i)
  Split i ax r a b -> case (removePane k a, removePane k b) of
    (Just a', Just b') -> Just (Split i ax r a' b')
    (Nothing, b') -> b'
    (a', Nothing) -> a'

encodeTree :: GridNode -> Text
encodeTree = \case
  Pane i -> maybe ("pane." <> T.pack (show i)) pkKey (paneKind i)
  Split _ ax r a b ->
    "(" <> (if ax == AxisV then "v" else "h") <> " " <> T.pack (showFFloat (Just 3) r "") <> " " <> encodeTree a <> " " <> encodeTree b <> ")"

-- | Read a tree back, giving its splits fresh ids. Unknown pane names, a
-- pane named twice and ratios outside @(0, 1)@ are refused, and the result
-- passes nano-ui's 'validateGridTree'.
decodeTree :: Text -> Either Text GridNode
decodeTree src = do
  (tree, rest, _) <- node (tokens src) 1000
  case rest of
    [] -> pure ()
    t : _ -> Left ("unexpected " <> t)
  let ks = treeKinds tree
  when (length ks /= length (dedupe ks)) (Left "a pane is named twice")
  either (Left . T.pack) (const (Right tree)) (validateGridTree tree)
  where
    tokens t = case T.uncons (T.dropWhile isSpace t) of
      Nothing -> []
      Just (c, rest)
        | c == '(' || c == ')' -> T.singleton c : tokens rest
        | otherwise ->
            let (w, rest') = T.break (\x -> isSpace x || x == '(' || x == ')') (T.cons c rest)
             in w : tokens rest'
    node ts next = case ts of
      "(" : ax : r : rest -> do
        axis <- case ax of
          "v" -> Right AxisV
          "h" -> Right AxisH
          _ -> Left ("unknown split " <> ax)
        ratio <- case TR.double r of
          Right (x, "") | x > 0 && x < 1 -> Right x
          _ -> Left ("bad ratio " <> r)
        (a, rest1, next1) <- node rest (next + 1)
        (b, rest2, next2) <- node rest1 next1
        case rest2 of
          ")" : rest3 -> Right (Split next axis (realToFrac ratio) a b, rest3, next2)
          _ -> Left "missing )"
      w : rest | w /= "(" && w /= ")" -> case paneKindNamed w of
        Just k -> Right (Pane (pkId k), rest, next)
        Nothing -> Left ("unknown pane " <> w)
      _ -> Left "unexpected end"
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ---------------------------------------------------------------------------
-- Presets

-- | A named arrangement.
data Preset = Preset
  { prName :: !Text
  , prTree :: !GridNode
  }
  deriving (Eq, Show)

-- | Each panel's arrangement as it opens. The inspector sits with the
-- heap's analysis and under Core & STG's bindings.
panelPreset :: Panel -> GridNode
panelPreset = \case
  PTimeline -> Split 1000 AxisV 0.74 (Split 1001 AxisH 0.5 (Pane paneTlCaps) (Pane paneTlThreads)) (Split 1002 AxisH 0.64 (Pane paneTlOverview) (Pane paneTlMarkers))
  PHeap -> Split 1000 AxisH 0.46 (Split 1001 AxisV 0.75 (Pane paneHeapChart) (Pane paneHeapBands)) (Split 1002 AxisV 0.64 (Pane paneHeapAnalysis) (Pane paneInspector))
  PCore ->
    Split 1000 AxisV 0.22 (Split 1001 AxisH 0.58 (Pane paneCoreBindings) (Pane paneInspector)) $
      Split 1002 AxisH 0.2 (Pane paneCoreNotes) $
        Split 1003 AxisV 0.44 (Pane paneCoreSource) (Split 1004 AxisV 0.45 (Pane paneCoreCore) (Pane paneCoreStg))
  PDebugger -> Split 1000 AxisH 0.7 (Split 1001 AxisV 0.68 (Pane paneDbgSource) (Split 1002 AxisH 0.62 (Pane paneDbgLocals) (Pane paneDbgBreaks))) (Pane paneDbgConsole)
  PLive -> Split 1000 AxisH 0.58 (Pane paneLiveCharts) (Pane paneLiveThreads)

-- | The heap panel's live graph, with the inspector beside it.
heapGraphPreset :: GridNode
heapGraphPreset = Split 1000 AxisV 0.7 (Pane paneHeapGraph) (Pane paneInspector)

-- | The cost-centre tree on its own, when there is no heap profile.
heapCostsPreset :: GridNode
heapCostsPreset = Pane paneHeapCosts

-- | Arrangements across panels, offered beside the panels' own and saved
-- with sessions along with any the user adds.
userPresets :: [Preset]
userPresets =
  [ Preset "Leak hunt" $
      Split 1000 AxisH 0.5 (Split 1001 AxisV 0.72 (Pane paneHeapChart) (Pane paneHeapBands)) $
        Split 1002 AxisV 0.34 (Pane paneHeapGraph) (Split 1003 AxisV 0.56 (Pane paneCoreSource) (Pane paneInspector))
  , Preset "Latency" $
      Split 1000 AxisV 0.72 (Split 1001 AxisH 0.55 (Pane paneTlCaps) (Pane paneTlThreads)) (Pane paneInspector)
  , Preset "Before and after" $
      Split 1000 AxisH 0.42 (Split 1001 AxisV 0.6 (Pane paneHeapChart) (Pane paneHeapBands)) $
        Split 1002 AxisV 0.5 (Pane paneDiffHeap) (Split 1003 AxisH 0.5 (Pane paneDiffCosts) (Pane paneDiffBindings))
  ]

-- ---------------------------------------------------------------------------
-- Drawing

-- | One provider's panes for this frame: 'Nothing' for a kind it doesn't
-- draw.
type PaneDraw = Word64 -> PaneCtx -> Maybe (NanoUI ())

-- | A provider's panes, given every pane in the workbench they are drawn
-- in (time panes look for the Timeline's).
type Panes = [Word64] -> NanoUI PaneDraw

-- | Try the first provider, then the second.
orPanes :: PaneDraw -> PaneDraw -> PaneDraw
orPanes a b pid ctx = maybe (b pid ctx) Just (a pid ctx)

-- | A workbench a pane can be moved to, with its name in menus.
data BenchPlace = BenchPlace
  { bpTitle :: !Text
  , bpBench :: !Workbench
  }

-- | A pane in a floating window, and the workbench it came from.
data Popped = Popped
  { poKind :: !Word64
  , poFrom :: !Text
  }
  deriving (Eq, Show)

-- | What a panel needs to draw a workbench of mixed panes, given by the
-- shell each frame.
data Bench = Bench
  { bnDraw :: Workbench -> Panes -> NanoUI ()
    -- ^ Draw a workbench: the panel's own panes from its 'PaneDraw', given
    -- every pane in the workbench, the rest from their panels and the
    -- shell.
  , bnMixed :: Workbench -> NanoUI Bool
    -- ^ Whether it holds another panel's panes, so it shows even when its
    -- own panel has nothing loaded.
  }

-- | Draw a workbench: a pane grid whose panes come from @draw@, each with
-- a menu to move it to another workbench, pop it out or close it.
workspace :: [BenchPlace] -> Var [Popped] -> Workbench -> PaneDraw -> NanoUI ()
workspace places popped bench draw = do
  th <- uiTheme
  out <- readVar popped
  columnWith (fillW . grow . padTop 8 . padBottom 8 . padXY 8 0) $
    void $
      paneGrid
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          -- The gutter between panes (the spacing and the grab margin on
          -- each side) is the 8 px the grid keeps from the window's edge.
          , pgSpacing = 2
          , pgMinSize = 120
          , pgLeeway = 3
          , pgFocusable = False
          , pgTree = ControlledTree tree
          , pgDividerColor = Just (inkGround (ink th))
          , pgViewPane = \pid g -> do
              let ctx = PaneCtx (Just g) (Just (menu pid))
              if any ((== pid) . poKind) out
                then paneWith (padAll 12 . gap 8) ctx (paneTitle pid) (pure ()) $ do
                  hint "In a floating window."
                  whenM (buttonWith tight "Bring it back here") $ modifyVar popped (filter ((/= pid) . poKind))
                else maybe (paneEmpty ctx (paneTitle pid) "Nothing draws this pane." []) id (draw pid ctx)
              pure (PaneView (paneTitle pid) False)
          }
  where
    tree = wbTree bench
    here = wbName bench
    menu pid = do
      menuHeader (paneTitle pid)
      moved <- fmap or . mapM (moveTo pid) $ [p | p <- places, wbName (bpBench p) /= here]
      menuSeparator
      out <- menuItem "Pop out"
      when out $ do
        modifyVar tree (>>= removePane pid)
        modifyVar popped ((++ [Popped pid here]) . filter ((/= pid) . poKind))
      closed <- menuItem "Close pane"
      when closed (modifyVar tree (>>= removePane pid))
      pure (moved || out || closed)
    moveTo pid p = do
      there <- readVar (wbTree (bpBench p))
      if hasPane pid there
        then menuItemDisabled ("Move to " <> bpTitle p <> " (already there)") >> pure False
        else do
          go <- menuItem ("Move to " <> bpTitle p)
          when go $ do
            modifyVar tree (>>= removePane pid)
            writeVar (wbTree (bpBench p)) (Just (addPane pid there))
          pure go

-- | What a pane shows when its panel has nothing loaded for it: what it
-- would show, and how to get it.
paneEmpty :: PaneCtx -> Text -> Text -> [Text] -> NanoUI ()
paneEmpty ctx title why commands =
  pane ctx title (pure ()) $ do
    hint why
    scope $ when (not (null commands)) (codeBlock commands)

