-- | Pane registry, layout trees, presets, and pop-out windows.
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
  , renamePane
  , splitAtDrop
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
import Control.Monad.State.Strict (StateT, evalStateT, get, modify)
import Data.Bifunctor (bimap, first)
import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Data.Void (Void)
import Data.Word (Word64)
import Numeric (showFFloat)

import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import Text.Megaparsec (Parsec, between, choice, eof, errorBundlePretty, parse, takeWhile1P)
import Text.Megaparsec.Char (char, hspace)

import Stronghead.Format (readDouble, tshow)
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
  deriving stock (Eq, Show)

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
paneTitle i = maybe ("pane " <> tshow i) pkTitle (paneKind i)

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

-- | Replace a pane id in place within a tree.
renamePane :: Word64 -> Word64 -> GridNode -> GridNode
renamePane old new = \case
  Pane i -> Pane (if i == old then new else i)
  Split i ax r a b -> Split i ax r (renamePane old new a) (renamePane old new b)

-- | Split a tree at a drop location to insert a pane with a fresh split id.
splitAtDrop :: PaneDropLocation -> Word64 -> GridNode -> GridNode
splitAtDrop loc newPid tree0 =
  let tree = fromMaybe tree0 (removePane newPid tree0)
      sid = maximum (999 : splitIds tree) + 1
      mkSplit ax True a b = Split sid ax 0.5 a b
      mkSplit ax False a b = Split sid ax 0.5 b a
      edgeAxis PaneLeft = (AxisV, True)
      edgeAxis PaneRight = (AxisV, False)
      edgeAxis PaneAbove = (AxisH, True)
      edgeAxis PaneBelow = (AxisH, False)
   in case loc of
        OutsideGrid edge ->
          let (ax, onFirst) = edgeAxis edge
           in mkSplit ax onFirst (Pane newPid) tree
        BesidePane targetPid edge ->
          let (ax, onFirst) = edgeAxis edge
              go (Pane i)
                | i == targetPid = mkSplit ax onFirst (Pane newPid) (Pane i)
                | otherwise = Pane i
              go (Split i sAx r a b) = Split i sAx r (go a) (go b)
           in go tree

encodeTree :: GridNode -> Text
encodeTree = \case
  Pane i -> maybe ("pane." <> tshow i) pkKey (paneKind i)
  Split _ ax r a b ->
    "(" <> (if ax == AxisV then "v" else "h") <> " " <> T.pack (showFFloat (Just 3) r "") <> " " <> encodeTree a <> " " <> encodeTree b <> ")"

-- | Read a tree back, giving its splits fresh ids. Unknown pane names, a
-- pane named twice and ratios outside @(0, 1)@ are refused, and the result
-- passes nano-ui's 'validateGridTree'.
decodeTree :: Text -> Either Text GridNode
decodeTree src = do
  tree <- first (T.pack . errorBundlePretty) (parse (evalStateT (hspace *> pNode <* eof) 1000) "" src)
  let ks = treeKinds tree
  when (length ks /= length (nubOrd ks)) (Left "a pane is named twice")
  bimap T.pack (const tree) (validateGridTree tree)
  where
    lexeme :: StateT Word64 (Parsec Void Text) a -> StateT Word64 (Parsec Void Text) a
    lexeme p = p <* hspace
    wordChar c = not (isSpace c) && c /= '(' && c /= ')'
    pNode :: StateT Word64 (Parsec Void Text) GridNode
    pNode =
      choice
        [ between (lexeme (char '(')) (lexeme (char ')')) do
            splitId <- get <* modify (+ 1)
            ax <- lexeme (takeWhile1P Nothing wordChar)
            axis <- case ax of
              "v" -> pure AxisV
              "h" -> pure AxisH
              _ -> fail ("unknown split " <> T.unpack ax)
            rTxt <- lexeme (takeWhile1P Nothing wordChar)
            ratio <- case readDouble rTxt of
              Just x | x > 0 && x < 1 -> pure (realToFrac x)
              _ -> fail ("bad ratio " <> T.unpack rTxt)
            Split splitId axis ratio <$> pNode <*> pNode
        , do
            w <- lexeme (takeWhile1P Nothing wordChar)
            case paneKindNamed w of
              Just k -> pure (Pane (pkId k))
              Nothing -> fail ("unknown pane " <> T.unpack w)
        ]

-- ---------------------------------------------------------------------------
-- Presets

-- | A named arrangement.
data Preset = Preset
  { prName :: !Text
  , prTree :: !GridNode
  }
  deriving stock (Eq, Show)

-- | Each panel's arrangement as it opens. The inspector sits with the
-- heap's analysis and under Core & STG's bindings.
panelPreset :: Panel -> GridNode
panelPreset = \case
  PTimeline -> Split 1000 AxisV 0.74 (Split 1001 AxisH 0.5 (Pane paneTlCaps) (Pane paneTlThreads)) (Split 1002 AxisH 0.64 (Pane paneTlOverview) (Pane paneTlMarkers))
  PHeap -> Split 1000 AxisH 0.46 (Split 1001 AxisV 0.75 (Pane paneHeapChart) (Pane paneHeapBands)) (Split 1002 AxisV 0.64 (Pane paneHeapAnalysis) (Pane paneInspector))
  PCore ->
    Split 1000 AxisV 0.22 (Split 1001 AxisH 0.58 (Pane paneCoreBindings) (Pane paneInspector)) $
      Split 1002 AxisH 0.26 (Pane paneCoreNotes) (Pane paneCoreSource)
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
  deriving stock (Eq, Show)

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
  liftIO $ modifyIORef' (wbTabDrag bench) (fmap (\h -> h {htDrop = Nothing}))
  columnWith (fillW . grow . padTop 8 . padBottom 8 . padXY 8 0) $ do
    gridResp <-
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
              let ctx = PaneCtx (Just g) (Just (menu pid)) (Just pid) (Just bench)
              if any ((== pid) . poKind) out
                then paneWith (padAll 12 . gap 8) ctx (paneTitle pid) (pure ()) $ do
                  hint "In a floating window."
                  whenM (buttonWith tight "Bring it back here") $ modifyVar popped (filter ((/= pid) . poKind))
                else maybe (paneEmpty ctx (paneTitle pid) "Nothing draws this pane." []) id (draw pid ctx)
              pure (PaneView (paneTitle pid) False)
          }
    held <- liftIO (readIORef (wbTabDrag bench))
    for_ held $ \h -> do
      let offStrip = isNothing (htDrop h)
          gridDrop = case pgrDropTarget gridResp of
            Just t | offStrip, pgdPane t /= htFrom h || htFromCount h > 1 -> Just t
            _ -> Nothing
      scope $ when (htPhase h == Dragging && offStrip) $
        tabGhost (htTitle h) (htAt h) (pgdRect <$> gridDrop)
      when (htPhase h == DragReleased) $ do
        liftIO (writeIORef (wbTabDrag bench) Nothing)
        htCommit h (htDrop h) (pgdLocation <$> gridDrop)
  where
    tree = wbTree bench
    here = wbName bench
    tabGhost title (V2 mx my) mTarget = do
      th <- uiTheme
      baseSize <- uiFontSize
      fm <- resolveFontUi baseSize WeightNormal FontStyleNormal FontRegular
      tw <- lineWidthUi fm title
      let th' = fmLineHeight fm
          ghost = Rect (mx + 10) (my - th' * 0.5) (tw + 24) (th' + 12)
          Rect gx gy gw gh = ghost
          Rect tx ty tw' th'' = fromMaybe (Rect 0 0 0 0) mTarget
          accent = themeAccent th
          panel = themePanel th
      void $
        canvasKeyed (contentKey [gx, gy, gw, gh, tx, ty, tw', th'']) (pinAt 0 0 . grow . pointer PointerPass) $ \_ -> do
          for_ mTarget $ \r -> do
            drawRect r (withAlpha accent 0.15)
            drawStrokeRoundedRect (rectInflate (-1) r) 0 1.5 (withAlpha accent 0.8)
          drawRoundedRect ghost 4 (withAlpha (styleBg panel) 0.92)
          drawStrokeRoundedRect ghost 4 1 accent
          drawText (V2 (gx + 12) (gy + gh * 0.5)) AlignStart AlignMiddle title (styleFg panel)
    menu pid = do
      labelWith (padXY 16 6 . fontMedium . fontSizeScale 0.92 . fillW) (paneTitle pid)
      moved <- or <$> traverse (moveTo pid) [p | p <- places, wbName (bpBench p) /= here]
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

