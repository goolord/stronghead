-- | Headless UI tests for cross-panel links, shared clock, code annotations,
-- remote actions, inspector, pinboard, watchers, diffs, and workspace panes.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, unless, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.IORef
import Data.List ((!?), sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import NanoUI
import NanoUI.Backend (FontMetrics (fmAdvance))
import NanoUI.Widgets.PaneGrid (GridAxis (..), GridNode (..), validateGridTree)
import NanoUI.Testing
import NanoUI.Testing.Assert (evalUi, withInput)
import NanoUI.Testing.Harness
import Stronghead.Dump (CodeKind (..), parseCodeFile, tokenizeLine)
import Stronghead.Eventlog.CriticalPath (gcPauseHeatmap)
import Stronghead.Eventlog.Model (GCEvent (..), Marker (..), Timeline (..), emptyTimeline)
import Stronghead.Flamegraph (diffFlameTrees, fromFoldedStacks)
import Stronghead.Format (tshow)
import Stronghead.Ghci (GhciBinding (..), StopInfo (..), ThunkTransition (..), Value (..), ValueXRay (..))
import Stronghead.Join (Annotation (..), Lane (..), Shade (..), annotationAt)
import Stronghead.Live
import Stronghead.Next (Fact (..), Have (..), Step (..), StepDo (..), haveNothing, strongStep)
import Stronghead.Ref
import Stronghead.Run (RunInfo (..), emptyRunInfo, runFromHello, runKey)
import Stronghead.Session (Pin (..))
import Stronghead.Source (SrcSpan (..), mkSourceIndex)
import Stronghead.Watch
import Stronghead.Diff (Baseline (..), BaselinePaths (..), noBaselinePaths)
import Stronghead.HeapProfile (fromSamples)
import Stronghead.Query (QueryContext (..), emptyQueryContext)
import Stronghead.Runner (CabalTarget (..), TargetKind (..))
import Stronghead.Triage (Finding (..), FindingCategory (..), Severity (..))
import System.Exit (exitFailure)
import UI.Baseline
import UI.Clock
import UI.Code
import UI.Common (Ink (..), PaneCtx (..), TimeView (..), Workbench (..), clearPaneFlashes, floatingPane, ink, isPaneFlashing, monoMetrics, newWorkbench, notePaneState, pane, paneFlashing, paneWith, quiet, readoutDeltaWith, strongheadTheme, wasPaneFlashed)
import UI.Core (CoreLoaded (..), CoreState (..), corePanes, newCoreState)
import UI.Flamegraph (diffFlamegraphView, eraCohortHeatmapView, flamegraphView, latencyHeatmapView)
import UI.Graph (renderFlamegraphSvg, renderHpDiffSvg, renderHpSvg)
import UI.Heap (atCursor, leakView, newHeapState)
import UI.Debugger (DebugState (..), debugPanes, newDebugState, valueTree)
import UI.Inspector
import UI.Link
import UI.Live
import UI.Palette
import UI.Pinboard
import UI.Settings (Settings (..), defaultSettings)
import UI.Shell (Options (..), appView, defaultOptions, newApp)
import UI.Sources
import UI.Timeline (markersHeader, newTimelineState)
import UI.Watchers
import UI.WorkbenchLab
import UI.Workspace

-- | The world's lenses, for scenes that draw something other than 'view'.
worldEnv :: World -> Panel -> Env
worldEnv w tab =
  let record t = liftIO (modifyIORef' (wLog w) (t :))
      timeline =
        (lens PTimeline)
          { lensShows = (== thread)
          , lensReveal = \r -> record ("timeline " <> refTarget r)
          , lensTitle = \r -> if r == thread then Just "worker-7" else Nothing
          , lensRelated = \r -> [("Go to binding go", binding) | r == thread]
          , lensPeek = \r -> if r == thread then Just (peekOf [("ran", "41%")]) {peekExact = Just "exact: thread id in the same run", peekNotes = ["mostly blocked on an MVar"]} else Nothing
          }
      core = (lens PCore) {lensShows = (== binding), lensReveal = \_ -> record "core go"}
      debugger =
        (lens PDebugger)
          { lensActions = \r ->
              if r == thread then [Action "Do thing" (Right (record "acted")), Action "Disabled thing" (Left "no data")] else []
          }
   in Env (wBus w) [timeline, core, debugger] tab (writeVar (wTab w)) haveNothing Nothing

data World = World
  { wTab :: !(Var Panel)
  , wBus :: !Bus
  , wLog :: !(IORef [Text])
  , wBack :: !(IORef Bool)
  , wPalette :: !CommandPalette
  }

thread :: Ref
thread = RThread (RunKey "prog:1") 7

binding :: Ref
binding = RBinding "M" "go"

view :: World -> NanoUI Bool
view w = do
  tab <- readVar (wTab w)
  let record t = liftIO (modifyIORef' (wLog w) (t :))
      timeline =
        (lens PTimeline)
          { lensShows = (== thread)
          , lensReveal = \r -> record ("timeline " <> refTarget r)
          , lensTitle = \r -> if r == thread then Just "worker-7" else Nothing
          , lensRelated = \r -> [("Go to binding go", binding) | r == thread]
          , lensPeek = \r -> if r == thread then Just (peekOf [("ran", "41%")]) {peekExact = Just "exact: thread id in the same run"} else Nothing
          , lensHits = [Hit "thread" "worker-7" "ran 41%" (HitRef thread)]
          }
      live = (lens PLive) {lensShows = (== thread), lensReveal = \_ -> record "live"}
      core = (lens PCore) {lensShows = (== binding), lensReveal = \_ -> record "core go"}
      debugger =
        (lens PDebugger)
          { lensActions = \r ->
              if r == thread then [Action "Do thing" (Right (record "acted")), Action "Disabled thing" (Left "no data")] else []
          }
      env = Env (wBus w) [timeline, live, core, debugger] tab (writeVar (wTab w)) haveNothing Nothing
  back <- liftIO (atomicModifyIORef' (wBack w) (False,))
  when back (goBack env)
  clicked <- column $ do
    c <- linkRow env thread False id (void (label "worker-7 row"))
    -- What a caller does with a plain click happens in the pass that saw
    -- it; a later pass of the same frame sees no click.
    when c (record "clicked")
    linkText env binding id "go link"
    pure c
  paletteView env (wPalette w) [Hit "command" "Do nothing" "" (HitRun (record "nothing"))]
  publishHover (wBus w)
  pure clicked

-- | A time view of one second of run @prog:1@, 400 pixels wide, that
-- scrubs, with a cursor of its own that snaps to "censuses" every 100 ms.
data Clock = Clock
  { cBus :: !Bus
  , cTether :: !(Tether Int)
  , cCensus :: !(Var (Maybe Int))
  , cSteps :: !(IORef [Int])
  , cGo :: !(IORef (Maybe Ref))
  , cRect :: !(IORef Rect)
  }

run :: RunKey
run = RunKey "prog:1"

clockView :: Clock -> NanoUI ()
clockView c = do
  let timeline = (lens PTimeline) {lensShows = \case RInstant {} -> True; RRange {} -> True; _ -> False}
      env = Env (cBus c) [timeline] PHeap (\_ -> pure ()) haveNothing Nothing
  go <- liftIO (atomicModifyIORef' (cGo c) (Nothing,))
  for_ go (goTo env)
  resp <- canvasConfigured defaultCanvasConfig {canvasLayout = fixedWH 400 100, canvasTrackPointer = True} (\_ -> pure ())
  let r = respRect resp
  liftIO (writeIORef (cRect c) r)
  _ <- timeGestures env (linearAxis run (TimeView 0 1e9) (rectX r) (rectW r)) resp ScrubDrag
  tether (cBus c) (cTether c) run (cCensus c) (\t -> Just (round (fromIntegral t / 1e8 :: Double))) (\i -> Just (fromIntegral i * 100000000))
  (dir, _) <- stepKeys
  when (dir /= 0) (liftIO (modifyIORef' (cSteps c) (dir :)))

band :: Ref
band = RBand "0x4fe0"

-- | A three-line file whose second line holds a thunk band's bytes. The
-- point of its heat bar is written out for the test to click.
codeScene :: World -> CodeViewState -> IORef V2 -> NanoUI ()
codeScene w cv point = do
  tab <- readVar (wTab w)
  mm <- monoMetrics 0.88
  let record t = liftIO (modifyIORef' (wLog w) (t :))
      heap = (lens PHeap) {lensShows = (== band), lensReveal = \r -> record ("heap " <> refTarget r), lensTitle = \r -> if r == band then Just "thunk in go" else Nothing}
      env = Env (wBus w) [heap] tab (writeVar (wTab w)) haveNothing Nothing
      src = ["module M where", "go = 1 + 2"] ++ ["line " <> tshow i | i <- [3 .. 60 :: Int]]
      ann = (annotationAt (SrcSpan "M.hs" 2 6 2 10) LaneHeap 2048 "2 KiB live" (Just band)) {anShade = ShadeThunk}
      cfg =
        withNotes
          (placedNotes [(1, 1, ann)])
          defaultCodeConfig
            { ccLineCount = length src
            , ccTokens = \l -> fst (tokenizeLine KindSource False (fromMaybe "" (src !? l)))
            }
  ev <- annotatedCode env cv mm cfg (const Nothing)
  liftIO (writeIORef point (lanePoint mm cfg (respRect (ceResp ev)) 1 0))
  publishHover (wBus w)

-- | A value GHCi printed at a stop, @p = P (_t2::Int) (2 : (_t3::[Int]))@,
-- as the Debugger's tree. Forcing records which thunk, and how deep.
valueScene :: DebugState -> IORef [(Text, Bool)] -> NanoUI ()
valueScene st forced = do
  toggled <- readVar (dsToggled st)
  valueTree st toggled False (\n deep -> liftIO (modifyIORef' forced ((n, deep) :))) "p" $
    VCon "P" [VThunk "_t2" (Just "Int"), VList [VAtom "2"] (Just (VThunk "_t3" (Just "[Int]")))]

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "  ok   " else "  FAIL ") <> name)
        unless ok (modifyIORef' failures (+ 1))
  ctx <- newContext
  w <- World <$> newVar PHeap <*> newBus <*> newIORef [] <*> newIORef False <*> newPalette
  let inp0 = withInput 800 600
      frame inp = evalUi ctx inp (view w)
      click inp pos = do
        let (press, release) = clickPair inp pos
        _ <- frame press
        r <- frame release
        _ <- frame inp0
        pure r
      spans = collectTextSpans ctx
      overlay = collectOverlayTextSpans ctx inp0
      at name = maybe (fail ("no span " <> T.unpack name)) pure . findExact name
      logged = readIORef (wLog w)
      tab = readVarIO (wTab w)
      ctrl inp = inp {inputModifiers = (inputModifiers inp) {modCtrl = True}}
  _ <- warmup2 ctx inp0 (view w)
  row <- at "worker-7 row" =<< spans

  putStrLn "plain click"
  _ <- click inp0 row
  check "reports the click, revealing nothing" . (== ["clicked"]) =<< logged
  check "focuses the entity" . (== Just thread) =<< readVarIO (busFocus (wBus w))
  check "stays in the panel" . (== PHeap) =<< tab
  writeIORef (wLog w) []

  putStrLn "ctrl+click"
  _ <- click (ctrl inp0) row
  check "is not a plain click, and reveals in the home panel" . (== ["timeline thread/prog:1/7"]) =<< logged
  check "switches to it" . (== PTimeline) =<< tab
  check "remembers where it was" . (== [Location PHeap (Just thread)]) =<< readVarIO (busBack (wBus w))

  putStrLn "back"
  writeIORef (wBack w) True
  _ <- frame inp0
  _ <- frame inp0
  check "returns to the panel" . (== PHeap) =<< tab
  check "can go forward again" . (== [Location PTimeline (Just thread)]) =<< readVarIO (busForward (wBus w))

  putStrLn "right-click"
  let (rDown, rUp) = rightClickPair inp0 row
  mapM_ frame [rDown, rUp, inp0, inp0]
  menu <- overlay
  let has t = hasText t menu
  check "titles the menu" (has "worker-7")
  check "offers every panel that shows it" (has "Show in Timeline" && has "Show in Live" && not (has "Show in Core"))
  check "offers what goes with it" (has "Go to binding go")
  check "lists actions, with reasons when disabled" (has "Do thing" && has "Disabled thing (no data)")
  check "can copy the reference" (has "Copy reference")
  check "and pin it" (has "Pin")
  writeIORef (wLog w) []
  item <- at "Go to binding go" menu
  _ <- click inp0 item
  check "a related item goes there" . (== ["core go"]) =<< logged
  check "in its panel" . (== PCore) =<< tab
  closed <- overlay
  check "and closes the menu" (not (hasText "Copy reference" closed))

  putStrLn "text links"
  writeVarIO (wTab w) PHeap
  writeIORef (wLog w) []
  _ <- frame inp0
  link <- at "go link" =<< spans
  _ <- click inp0 link
  check "a plain click goes there" . (== ["core go"]) =<< logged
  check "switching panel" . (== PCore) =<< tab

  putStrLn "hover and peek"
  let hover = inp0 {inputMousePos = row}
  _ <- frame hover
  _ <- frame hover
  check "every panel sees what the pointer is on" . (== Just thread) =<< readVarIO (busHover (wBus w))
  threadDelay 900000
  _ <- frame hover
  _ <- frame hover
  tip <- overlay
  check "the peek shows each panel's facts" (hasText "41%" tip && hasText "Timeline" tip && hasText "exact" tip)
  check "and how to act on it" (hasText "Ctrl+click to open" tip)
  _ <- frame inp0
  _ <- frame inp0
  check "the pointer leaving clears the hover" . (== Nothing) =<< readVarIO (busHover (wBus w))

  putStrLn "palette"
  writeVarIO (wTab w) PHeap
  writeIORef (wLog w) []
  _ <- evalUi ctx inp0 (openPalette (wPalette w) >> view w)
  _ <- frame inp0
  writeVarIO (plQuery (wPalette w)) "wk7"
  _ <- frame inp0
  _ <- frame inp0
  found <- overlay
  check "finds a thread by a fuzzy query" (hasText "worker-7" found && not (hasText "Do nothing" found))
  _ <- frame (keyInp KeyEnter inp0)
  _ <- frame inp0
  check "Enter goes to it" . (== ["timeline thread/prog:1/7"]) =<< logged
  check "in its panel" . (== PTimeline) =<< tab
  check "and closes the palette" . not =<< readVarIO (plOpen (wPalette w))

  putStrLn "the shared clock"
  cctx <- newContext
  c <- Clock <$> newBus <*> newTether <*> newVar Nothing <*> newIORef [] <*> newIORef Nothing <*> newIORef (Rect 0 0 0 0)
  let cframe inp = evalUi cctx inp (clockView c)
      shifted inp = inp {inputModifiers = (inputModifiers inp) {modShift = True}}
      cursor = readVarIO (busCursor (cBus c))
      range = readVarIO (busRange (cBus c))
  warmup cctx inp0 (clockView c)
  Rect x y _ h <- readIORef (cRect c)
  let at px = V2 (x + px) (y + h / 2)
      (p1, r1) = clickPair inp0 (at 130)
  mapM_ cframe [p1, r1, inp0]
  -- Pointer positions are floats: a time is good to well under a pixel.
  let near a b = abs (fromIntegral a - fromIntegral b :: Double) < 1e5
  check "a click moves the cursor to its time" . maybe False (\(r, t) -> r == run && near t (325000000 :: Int)) =<< cursor
  check "a snapping view takes it up, snapped" . (== Just 3) =<< readVarIO (cCensus c)
  writeVarIO (cCensus c) (Just 7)
  _ <- cframe inp0
  check "moving the view's own cursor moves the shared one" . (== Just (run, 700000000)) =<< cursor
  writeVarIO (busCursor (cBus c)) (Just (RunKey "other:2", 5))
  _ <- cframe inp0
  check "a cursor from another run is left alone" . (== Just 7) =<< readVarIO (cCensus c)
  let press = shifted (pressAt inp0 (at 40))
      hold = holdAt press (at 200)
  mapM_ cframe [press, hold, releaseAt hold, inp0]
  check "Shift+drag selects a range" . maybe False (\(r, a, b) -> r == run && near a (100000000 :: Int) && near b (500000000 :: Int)) =<< range
  check "without moving the cursor" . (== Just (RunKey "other:2", 5)) =<< cursor
  let (sp, sr) = clickPair (shifted inp0) (at 300)
  mapM_ cframe [sp, sr, inp0]
  check "Shift+click clears it" . (== Nothing) =<< range
  writeIORef (cGo c) (Just (RInstant run 42))
  _ <- cframe inp0
  check "going to an instant moves the cursor there" . (== Just (run, 42)) =<< cursor
  writeIORef (cGo c) (Just (RRange run 10 20))
  _ <- cframe inp0
  check "going to a range selects it" . (== Just (run, 10, 20)) =<< range
  _ <- cframe (keyInp (KeyChar ']') inp0)
  _ <- cframe (keyInp (KeyChar '[') inp0)
  check "[ and ] step" . (== [-1, 1]) =<< readIORef (cSteps c)

  putStrLn "annotated code"
  codeCtx <- newContext
  cv <- newCodeViewState
  point <- newIORef (V2 0 0)
  writeVarIO (wTab w) PCore
  writeIORef (wLog w) []
  let codeFrame inp = evalUi codeCtx inp (codeScene w cv point)
      codeClick inp pos = do
        let (press, release) = clickPair inp pos
        mapM_ codeFrame [press, release, inp0]
  _ <- warmup2 codeCtx inp0 (codeScene w cv point)
  bar <- readIORef point
  _ <- codeFrame inp0 {inputMousePos = bar}
  _ <- codeFrame inp0 {inputMousePos = bar}
  check "the pointer on a heat bar is on its entity" . (== Just band) =<< readVarIO (busHover (wBus w))
  let V2 bx by = bar
      codePos = V2 (bx + 50) by
      ctrl inp = inp {inputModifiers = (inputModifiers inp) {modCtrl = True}}
  _ <- codeFrame inp0 {inputMousePos = codePos}
  check "cursor over code is text without control" . (== UiCursorText) =<< uiCursorKind codeCtx inp0 {inputMousePos = codePos}
  _ <- codeFrame (ctrl inp0) {inputMousePos = codePos}
  check "cursor over code is pointer when holding control" . (== UiCursorPointer) =<< uiCursorKind codeCtx (ctrl inp0) {inputMousePos = codePos}
  codeClick inp0 bar
  check "clicking a heat bar goes to its entity" . (== ["heap band/0x4fe0"]) =<< logged
  check "in its panel" . (== PHeap) =<< tab
  writeVarIO (wTab w) PCore
  let (bDown, bUp) = rightClickPair inp0 bar
  mapM_ codeFrame [bDown, bUp, inp0, inp0]
  barMenu <- collectOverlayTextSpans codeCtx inp0
  check "right-clicking a heat bar opens its entity's menu" (hasText "thunk in go" barMenu && hasText "Show in Heap & leaks" barMenu)
  mapM_ codeFrame [keyInp KeyEscape inp0, inp0]
  let scrollInp = inp0 {inputMousePos = codePos, inputScroll = V2 0 2}
  mapM_ codeFrame [scrollInp, inp0 {inputMousePos = codePos}]
  cvScrolled <- readVarIO (cvScroll cv)
  check "scrolling code with scroll wheel updates vertical scroll" (cvScrolled > 0)

  putStrLn "ghci values"
  valCtx <- newContext
  dst <- newDebugState
  forced <- newIORef []
  let valFrame inp = evalUi valCtx inp (valueScene dst forced)
      valClick pos = do
        let (press, release) = clickPair inp0 pos
        mapM_ valFrame [press, release, inp0]
      valSpans = collectTextSpans valCtx
      textAt name = maybe (fail ("no span " <> T.unpack name)) pure . findExact name
  _ <- warmup2 valCtx inp0 (valueScene dst forced)
  tree <- valSpans
  check "a value opens as a tree, its thunks named and typed" $
    all (`hasText` tree) ["P", "_t2", ":: Int", "list of 1, then unevaluated", "_t3", "rest"]
  evaluate' <- textAt "Evaluate" tree
  valClick evaluate'
  force' <- textAt "Force" =<< valSpans
  valClick force'
  check "a thunk is forced in place, to WHNF or all the way" . (== [("_t3", True), ("_t3", False)]) =<< readIORef forced
  con <- textAt "P" =<< valSpans
  valClick con
  closed <- valSpans
  check "a node closes to the whole value" (hasText "P _t2 (2 : _t3)" closed && not (hasText "rest" closed))

  putStrLn "remote actions"
  rctx <- newContext
  rbus <- newBus
  lv <- newLiveState
  box <- newOutbox
  let info = AgentInfo 2 "/w" "/w/prog" ["prog"] (Just "hT") 100 True True
      frame0 = Frame 0 1000000000 True (M.fromList [("rts_stats", 1)]) []
      d0 = emptyLiveData {ldHello = Just ("prog", 1, 4), ldInfo = Just info, ldOutbox = Just box, ldFrames = Seq.fromList [frame0]}
      hooks = DebugHooks (Left "built without ghc-debug") False Nothing
      rk = RunKey "prog:1"
      renv = Env rbus [] PLive (\_ -> pure ()) haveNothing Nothing
      rframe d = evalUi rctx inp0 (liveTick rbus lv (Just d) Nothing hooks >> liveRemote rbus lv (Just d) Nothing hooks)
      named t = filter ((== t) . raLabel)
      enabled a = either (const False) (const True) (raRun a)
      moments = readVarIO (busMoments rbus)
      -- The harness may run a frame's view more than once; an action runs
      -- in the first pass, as a click's does.
      once act = do
        first <- newIORef True
        evalUi rctx inp0 (liftIO (atomicModifyIORef' first (False,)) >>= \go -> when go act)
  actions <- rframe d0
  check "agent actions are on" (all enabled (concatMap (`named` actions) ["Mark…", "Major GC", "Census now", "Stop heap profiling"]))
  check "ghc-debug ones say why not" (all (either ("ghc-debug" `T.isInfixOf`) (const False) . raRun) (named "Snapshot" actions))
  for_ (named "Major GC" actions) $ \a -> either (const (pure ())) once (raRun a)
  sent <- readOutbox box
  check "an action sends its command" (sent == "1 gc")
  placed <- moments
  check "and lands on the clock at once, estimated" (map (\m -> (moRun m, moKind m, moExact m, moLabel m)) placed == [(rk, MomentAction, False, "stronghead: major GC")])
  _ <- rframe d0 {ldReplies = M.fromList [(1, ReplyOk (Just 5000000000))]}
  check "the reply stamps it in program time" . (== [(5000000000, True)]) . map (\m -> (moNs m, moExact m)) =<< moments
  for_ (named "Census now" actions) $ \a -> either (const (pure ())) once (raRun a)
  _ <- readOutbox box
  _ <- rframe d0 {ldReplies = M.fromList [(1, ReplyOk (Just 5000000000)), (2, ReplyError "no")]}
  check "a refused command leaves the clock" . (== 1) . length =<< moments
  check "and says so" . ("failed: no" `T.isInfixOf`) =<< readVarIO (lvMessage lv)
  oldAgent <- rframe d0 {ldInfo = Nothing}
  check "an old agent: no commands, and why" (and [either ("protocol v1" `T.isInfixOf`) (const False) (raRun a) | a <- named "Major GC" oldAgent])
  marks <- evalUi rctx inp0 (marksFor renv rk Nothing)
  others <- evalUi rctx inp0 (marksFor renv (RunKey "other:2") Nothing)
  check "every time view of the run marks it" (length (tmMoments marks) == 1 && null (tmMoments others))
  let ax = linearAxis rk (TimeView 0 1e10) 0 1000
  check "a moment is an instant to link to" (momentAt ax marks 500 == Just (RInstant rk 5000000000) && momentAt ax marks 520 == Nothing)
  let logged' = emptyTimeline {tlRun = runFromHello "prog" 1, tlMarkers = V.fromList [Marker 5000200000 "stronghead: major GC" Nothing True]}
  evalUi rctx inp0 (confirmMoments rbus (Just logged'))
  check "its marker in the eventlog takes over, exactly" . (== [(5000200000, True)]) . map (\m -> (moNs m, moInLog m)) =<< moments
  pctx <- newContext
  let prompt inp = evalUi pctx inp (markPrompt rbus lv (Just d0))
  once (openMarkPrompt lv)
  warmup pctx inp0 (markPrompt rbus lv (Just d0))
  shown <- collectOverlayTextSpans pctx inp0
  check "M opens a prompt with a default text" (hasText "Mark the program's timeline" shown)
  check "named after the marks sent" . (== "mark 1") =<< readVarIO (lvMarkText lv)
  markButton <- maybe (fail "no Mark button") pure (findExact "Mark" shown)
  let (mp, mr) = clickPair inp0 markButton
  mapM_ prompt [mp, mr, inp0]
  marked <- readOutbox box
  check "Mark sends it" (" mark mark 1" `T.isSuffixOf` marked)
  check "and closes the prompt" . not =<< readVarIO (lvMarkOpen lv)
  check "a mark lands on the clock" . any ((== ("mark 1", MomentMark)) . (\m -> (moLabel m, moKind m))) =<< moments

  putStrLn "data sources"
  sctx <- newContext
  open <- newVar True
  joined <- newIORef ([], Nothing)
  -- Tall enough for the whole popover.
  let sinp = withInput 1400 1400
      logRun = (runFromHello "prog" 1) {riArgs = ["10"]}
      src kind ri ds = Source kind (kind <> " detail") ri (runKey ri) False PTimeline ds
      sources =
        [ src "Eventlog" logRun [DTimeline, DHeapCensus]
        , src "Heap profile" emptyRunInfo {riProgram = Just "prog"} [DHeapCensus]
        , src "Agent" (runFromHello "prog" 1) [DLive, DLiveStats]
        , src "Cost centres" (runFromHello "renamed" 9) [DCostCentres]
        ]
      senv = Env rbus [] PLive (\_ -> pure ()) haveNothing Nothing
      popover inp = evalUi sctx inp (sourcesPopover senv open (Rect 10 10 80 20) sources (\ss ri -> liftIO (writeIORef joined (map srcKind ss, riProgram ri))) (\_ -> pure ()))
  warmup sctx sinp (sourcesPopover senv open (Rect 10 10 80 20) sources (\_ _ -> pure ()) (\_ -> pure ()))
  listed <- collectOverlayTextSpans sctx sinp
  check "a run per program and pid" (hasText "prog, pid 1" listed && hasText "renamed, pid 9" listed)
  check "a match without a pid says so" (hasText "matched by program name" listed)
  check "what is missing, with how to add it" (hasText "Heap by source location" listed && hasText "-finfo-table-map" listed && hasText "Copy" listed)
  sameRun <- maybe (fail "no Same run button") pure (findExact "Same run as prog, pid 1" listed)
  let (sp', sr') = clickPair sinp sameRun
  mapM_ popover [sp', sr', sinp]
  check "an unmatched run can be joined by hand" . (== (["Cost centres"], Just "prog")) =<< readIORef joined

  putStrLn "inspector"
  let findSpan name = maybe (fail ("no span " <> T.unpack name)) pure . findExact name
  ictx <- newContext
  ins <- newInspector
  writeVarIO (busFocus (wBus w)) Nothing
  writeIORef (wLog w) []
  let iinp = withInput 900 900
      iscene = do
        tab <- readVar (wTab w)
        let env = worldEnv w tab
        inspectorSync env ins
        void (columnWith (fixedWH 600 800) (inspectorPane env ins floatingPane))
      iframe inp = evalUi ictx inp iscene
      iclick pos = let (p, r) = clickPair iinp pos in mapM_ iframe [p, r, iinp]
      ispans = collectTextSpans ictx
  warmup2 ictx iinp iscene >>= \_ -> pure ()
  empty' <- ispans
  check "with nothing in focus it says what it is for" (any (\(_, t, _, _, _) -> "Click a thread" `T.isInfixOf` t) empty')
  writeVarIO (busFocus (wBus w)) (Just thread)
  _ <- iframe iinp
  _ <- iframe iinp
  shownI <- ispans
  check "it shows the focus: its title and reference" (hasText "worker-7" shownI && hasText "thread/prog:1/7" shownI)
  check "a section per panel, with how it matched" (hasText "Timeline" shownI && hasText "exact: thread id in the same run" shownI)
  check "the long form of the peek: facts and notes" (hasText "41%" shownI && hasText "mostly blocked on an MVar" shownI)
  check "what goes with it, as links" (hasText "Go to binding go" shownI)
  check "every panel's actions, as buttons" (hasText "Do thing" shownI && hasText "Disabled thing" shownI && hasText "Show in Timeline" shownI)
  check "and a pin button" (hasText "Pin" shownI)
  pinB <- findSpan "Pin" shownI
  iclick pinB
  check "pinning from the inspector" . (== [thread]) . map pinRef =<< readVarIO (busPins (wBus w))
  check "which then offers to unpin" . hasText "Unpin" =<< ispans
  writeVarIO (busFocus (wBus w)) (Just binding)
  _ <- iframe iinp
  _ <- iframe iinp
  check "it follows the focus" . (== Just binding) =<< readVarIO (inShown ins)
  check "remembering what it showed" . (== [thread]) =<< readVarIO (inBack ins)
  backB <- findSpan "←" =<< ispans
  iclick backB
  check "its own back returns to it" . (== Just thread) =<< readVarIO (inShown ins)
  check "and focuses it everywhere" . (== Just thread) =<< readVarIO (busFocus (wBus w))
  check "with forward to come back" . (== [binding]) =<< readVarIO (inForward ins)
  writeVarIO (inFollow ins) False
  writeVarIO (busFocus (wBus w)) (Just binding)
  _ <- iframe iinp
  check "not following, it holds on" . (== Just thread) =<< readVarIO (inShown ins)

  putStrLn "next steps"
  let waker = RThread run 3
      blockedR = RRange run 100 80000100
      nenv tab =
        (worldEnv w tab)
          { envLenses =
              [ (lens PTimeline)
                  { lensShows = (`elem` [thread, waker])
                  , lensReveal = \r -> liftIO (modifyIORef' (wLog w) (("timeline " <> refTarget r) :))
                  , lensTitle = \r -> if r == waker then Just "worker-3" else Nothing
                  , lensFacts = \r -> if r == thread then [FactBlocked blockedR 80000000 "MVar", FactWokenBy waker] else []
                  }
              ]
          , envHave = haveNothing {haveTimeline = True}
          }
  check "the steps come from what the lenses know" (map stepDo (stepsFor (nenv PHeap) thread) == [StepGo blockedR, StepGo waker, StepLoad DSamples])
  check "the waker is the strong step, for the peek" (fmap stepDo (strongStep (stepsFor (nenv PHeap) thread)) == Just (StepGo waker))
  nctx <- newContext
  nins <- newInspector
  writeVarIO (busFocus (wBus w)) (Just thread)
  writeIORef (wLog w) []
  let nscene = do
        tab <- readVar (wTab w)
        let env = nenv tab
        inspectorSync env nins
        void (columnWith (fixedWH 700 900) (inspectorPane env nins floatingPane))
      nframe inp = evalUi nctx inp nscene
      nclick pos = let (p, r) = clickPair iinp pos in mapM_ nframe [p, r, iinp]
  warmup2 nctx iinp nscene >>= \_ -> pure ()
  _ <- nframe iinp
  nspans <- collectTextSpans nctx
  check "the inspector lists the next steps" (hasText "Next steps" nspans && hasText "Go to the thread that woke it" nspans)
  check "each with its reason and the entity it leads to" (hasText "worker-3" nspans && hasText "80.00 ms blocked on MVar" nspans)
  check "missing data says how to get it" (hasText "Cost-centre samples" nspans && hasText "+RTS -p -l -RTS" nspans)
  wakeB <- findSpan "Go to the thread that woke it" nspans
  nclick wakeB
  check "a step's button follows the link" . elem "timeline thread/prog:1/3" =<< logged

  putStrLn "watchers"
  wctx <- newContext
  watchers <- newWatchers
  marks <- newIORef []
  writeVarIO (wrRules watchers) (setRules [Rule (BlockedOver 500) True [RespNotify, RespMark] 0 0.5])
  let wthread b = LiveThread 7 (if b then "BlockedOnMVar" else "running") "worker-7" Nothing
      wframe i = Frame (fromIntegral i * 0.25) (fromIntegral i * 250000000) True M.empty [wthread True]
      wdata = emptyLiveData {ldHello = Just ("prog", 1, 1), ldFrames = Seq.fromList [wframe i | i <- [0 .. 4 :: Int]]}
      whooks = WatchHooks (Right (\t -> liftIO (modifyIORef' marks (t :)))) (Left "no census here") (Left "no ghc-debug target")
      wscene = do
        watchTick watchers whooks (Just wdata) Nothing False
        void (columnWith (fixedWH 700 900) (watchersPane (worldEnv w PLive) watchers "watching prog" floatingPane))
  warmup2 wctx iinp wscene >>= \_ -> pure ()
  _ <- evalUi wctx iinp wscene
  fired <- readVarIO (wrFired watchers)
  check "a rule fires on the agent's frames, about the thread" (map (fiSubject . feFired) fired == [AboutThread 7 "worker-7"])
  check "once, however many frames see it again" (length fired == 1)
  check "its mark goes through the agent" . (== ["watch: worker-7 blocked on MVar for 500.00 ms"]) =<< readIORef marks
  check "and says what it did" (map feDone fired == [["marked"]])
  check "it is news for the status bar" . (== 1) =<< readVarIO (wrUnseen watchers)
  wspans <- collectTextSpans wctx
  check "the pane lists what fired, and the rules" (hasText "worker-7 blocked on MVar" wspans && hasText "A thread blocked longer than 500 ms" wspans && hasText "Live bytes up across 5 major collections" wspans)

  putStrLn "baseline"
  bctx <- newContext
  bstate <- newBaselineState
  let beforeHp = fromSamples "" "closure type" 0 (const Nothing) [(t, [("THUNK_2_0", t * 4e6), ("ARR_WORDS", 1e6)]) | t <- [1 .. 5]]
      afterHp = fromSamples "" "closure type" 0 (const Nothing) [(t, [("THUNK_2_0", 64), ("ARR_WORDS", 1e6)]) | t <- [1 .. 5]]
      bpaths = noBaselinePaths {blEventlog = Just "before.eventlog"}
      bscene = do
        loaded <- baselineLoad bstate Nothing
        let env = (worldEnv w PHeap) {envBaseline = boBaseline loaded}
            now = DiffNow (Just afterHp) Nothing (Right (markBaseline bstate (Baseline bpaths Nothing (Just beforeHp) Nothing Nothing)))
        void (columnWith (fixedWH 900 500) (maybe (pure ()) id (diffPanes env bstate loaded now paneDiffHeap floatingPane)))
  warmup2 bctx iinp bscene >>= \_ -> pure ()
  check "without a baseline the diff says how to make one" . hasText "Mark a run as the baseline" =<< collectTextSpans bctx
  markB <- findSpan "Mark as baseline" =<< collectTextSpans bctx
  let bclick pos = let (p, r) = clickPair iinp pos in mapM_ (\i -> evalUi bctx i bscene) [p, r, iinp, iinp]
  bclick markB
  check "marking keeps the run's paths" . (== bpaths) =<< readVarIO (bvPaths bstate)
  bspans <- collectTextSpans bctx
  check "the diff names the baseline" (hasText "against before.eventlog" bspans)
  check "the leaking band shows as gone" (hasText "THUNK_2_0" bspans && hasText "gone" bspans)
  check "an unchanged band is left out until asked for" (not (hasText "ARR_WORDS" bspans))

  putStrLn "pinboard"
  pctx' <- newContext
  writeVarIO (busPins (wBus w)) [Pin thread "", Pin binding "the hot loop"]
  let pscene = do
        tab <- readVar (wTab w)
        void (columnWith (fixedWH 600 500) (pinboardPane (worldEnv w tab) (pure ()) floatingPane))
      pframe inp = evalUi pctx' inp pscene
      pclick pos = let (p, r) = clickPair iinp pos in mapM_ pframe [p, r, iinp]
  warmup2 pctx' iinp pscene >>= \_ -> pure ()
  board <- collectTextSpans pctx'
  check "pins are listed by title" (hasText "worker-7" board && hasText "M.go" board)
  downs <- pure [r | (r, t, _, _, _) <- board, t == "↓"]
  for_ (take 1 downs) $ \(Rect x y bw bh) -> pclick (V2 (x + bw / 2) (y + bh / 2))
  check "a pin moves down the board" . (== [binding, thread]) . map pinRef =<< readVarIO (busPins (wBus w))
  let report = pinsReport (worldEnv w PHeap) [] [Pin thread "slow", Pin binding "the hot loop"]
  check "the report carries each panel's facts as text" ("ran: 41%" `T.isInfixOf` report && "exact: thread id in the same run" `T.isInfixOf` report)
  check "with titles, references and notes" ("## 1. worker-7" `T.isInfixOf` report && "`thread/prog:1/7`" `T.isInfixOf` report && "> the hot loop" `T.isInfixOf` report)
  goB <- findSpan "M.go" board
  writeIORef (wLog w) []
  writeVarIO (busPins (wBus w)) [Pin binding ""]
  _ <- pframe iinp
  goB' <- findSpan "M.go" =<< collectTextSpans pctx'
  pclick goB'
  check "a click on a pin goes to it" . (== ["core go"]) =<< logged
  let _ = goB

  putStrLn "pane registry"
  check "pane kinds are unique" (length (nubOrd (map pkId paneKinds)) == length paneKinds && length (nubOrd (map pkKey paneKinds)) == length paneKinds)
  let presets = map panelPreset [minBound .. maxBound] ++ [heapGraphPreset, heapCostsPreset] ++ map prTree userPresets
  check "every preset is a valid tree of known panes" (all (\t -> validateGridTree t == Right () && all (maybe False (const True) . paneKind) (treeKinds t)) presets)
  check "presets read back as written" (all (\t -> decodeTree (encodeTree t) == Right t) presets)
  check "a tree's text form" (encodeTree (Split 1000 AxisV 0.5 (Pane paneHeapChart) (Pane paneInspector)) == "(v 0.500 heap.chart inspector)")
  check "Leak hunt has the chart, the source and the inspector" (and [all (`elem` treeKinds (prTree p)) [paneHeapChart, paneCoreSource, paneInspector] | p <- userPresets, prName p == "Leak hunt"])
  check "unknown panes, repeats, bad ratios and junk are refused" $
    all (either (const True) (const False) . decodeTree) ["(v 0.5 heap.chart nope)", "(v 0.5 inspector inspector)", "(h 1.5 inspector pinboard)", "(v 0.5 inspector", "inspector pinboard", ""]
  let one = Pane paneTlCaps
      two = addPane paneInspector (Just one)
  check "adding a pane puts it beside the rest" (treeKinds two == [paneTlCaps, paneInspector] && validateGridTree two == Right ())
  check "adding one already there leaves the tree" (addPane paneTlCaps (Just two) == two)
  check "adding to an empty workbench" (addPane panePinboard Nothing == Pane panePinboard)
  check "new splits get fresh ids" (validateGridTree (addPane panePinboard (Just (addPane paneHeapChart (Just two)))) == Right ())
  check "removing a pane closes the gap" (removePane paneInspector two == Just one && removePane paneTlCaps one == Nothing)

  putStrLn "moving panes"
  mctx <- newContext
  benchA <- newWorkbench "a" (Split 1000 AxisV 0.5 (Pane paneHeapChart) (Pane paneInspector))
  benchB <- newWorkbench "b" (Pane paneTlCaps)
  popped <- newVar []
  let places = [BenchPlace "Bench A" benchA, BenchPlace "Bench B" benchB, BenchPlace "Heap & leaks, live graph" benchB]
      draw pid ctx = Just (pane ctx (paneTitle pid) (pure ()) (void (label "body")))
      mscene = void (columnWith (fixedWH 800 500) (workspace places popped benchA draw))
      mframe inp = evalUi mctx inp mscene
      mclick pos = let (p, r) = clickPair iinp pos in mapM_ mframe [p, r, iinp, iinp]
  warmup2 mctx iinp mscene >>= \_ -> pure ()
  ws <- collectTextSpans mctx
  check "each pane draws with its title" (hasText "Heap residency" ws && hasText "Inspector" ws)
  let menus = [r | (r, t, _, _, _) <- ws, t == "⋯"]
  check "and a menu" (length menus == 2)
  title <- findSpan "Heap residency" ws
  let dbl = do
        let (dp, dr) = clickPair iinp title
        mapM_ mframe [dp {inputMouseClicks = 2}, dr {inputMouseClicks = 2}, iinp, iinp]
  dbl
  maxed <- collectTextSpans mctx
  check "double-clicking a header maximises its pane" (hasText "Heap residency" maxed && not (hasText "Inspector" maxed))
  dbl
  check "and again restores it" . hasText "Inspector" =<< collectTextSpans mctx
  let (rDown, rUp) = rightClickPair iinp title
  mapM_ mframe [rDown, rUp, iinp, iinp]
  openedRight <- collectOverlayTextSpans mctx iinp
  check "right-clicking a header opens its menu" (hasText "Move to Bench B" openedRight && hasText "Move to Heap & leaks, live graph" openedRight)
  mapM_ mframe [keyInp KeyEscape iinp, iinp]
  for_ (take 1 menus) $ \(Rect x y bw bh) -> mclick (V2 (x + bw / 2) (y + bh / 2))
  opened <- collectOverlayTextSpans mctx iinp
  check "the menu moves, pops out and closes" (hasText "Move to Bench B" opened && hasText "Move to Heap & leaks, live graph" opened && hasText "Pop out" opened && hasText "Close pane" opened && not (hasText "Move to Bench A" opened))
  moveB <- findSpan "Move to Bench B" opened
  mclick moveB
  check "a pane moves to another workbench" . (== Just [paneInspector]) . fmap treeKinds =<< readVarIO (wbTree benchA)
  check "beside what is there" . (== Just [paneTlCaps, paneHeapChart]) . fmap treeKinds =<< readVarIO (wbTree benchB)
  ws2 <- collectTextSpans mctx
  for_ (take 1 [r | (r, t, _, _, _) <- ws2, t == "⋯"]) $ \(Rect x y bw bh) -> mclick (V2 (x + bw / 2) (y + bh / 2))
  popB <- findSpan "Pop out" =<< collectOverlayTextSpans mctx iinp
  mclick popB
  check "popping out takes it from the workbench" . (== Nothing) =<< readVarIO (wbTree benchA)
  check "into a floating window, remembering where from" . (== [Popped paneInspector "a"]) =<< readVarIO popped
  writeVarIO (wbTree benchA) (Just (Pane paneInspector))
  _ <- mframe iinp
  _ <- mframe iinp
  check "a workbench that still holds it says where it went" . hasText "In a floating window." =<< collectTextSpans mctx

  putStrLn "flashing pane border"
  fctx <- newContext
  benchFlash <- newWorkbench "f" (Split 1000 AxisV 0.5 (Pane paneInspector) (Pane panePinboard))
  fpopped <- newVar []
  counterVar <- newVar (0 :: Int)
  clearPaneFlashes
  let fplaces = [BenchPlace "Bench F" benchFlash]
      fdraw pid ctx = Just $ pane ctx (paneTitle pid) (pure ()) $ do
        if pid == panePinboard
          then do
            void (label "Pinboard content")
            resp <- buttonWith' (padAll 4) "Increment"
            when (respClicked resp) (modifyVar counterVar (+ 1))
          else do
            c <- readVar counterVar
            notePaneState (tshow c)
            void (label ("Count: " <> tshow c))
            respSelf <- buttonWith' (padAll 4) "SelfIncrement"
            when (respClicked respSelf) (modifyVar counterVar (+ 1))
      fscene = void (columnWith (fixedWH 800 500) (workspace fplaces fpopped benchFlash fdraw))
      fframe inp = evalUi fctx inp fscene
      fclick pos = let (p, r) = clickPair iinp pos in mapM_ fframe [p, r, iinp, iinp]
      fscroll pos = mapM_ fframe [iinp {inputMousePos = pos, inputScroll = V2 0 1}, iinp {inputMousePos = pos}]
  warmup2 fctx iinp fscene >>= \_ -> pure ()
  _ <- fframe iinp
  check "initial layout does not trigger a flash" . not =<< wasPaneFlashed "Inspector"
  check "other pane also does not flash initially" . not =<< wasPaneFlashed "Pinboard"

  clearPaneFlashes
  writeVarIO counterVar 10
  _ <- fframe iinp
  check "non-mouse update does not trigger a flash" . not =<< wasPaneFlashed "Inspector"

  clearPaneFlashes
  fspans <- collectTextSpans fctx
  incB <- findSpan "Increment" fspans
  fclick incB
  check "mouse click changing another panel causes that panel (where attention was diverted from) to flash" =<< wasPaneFlashed "Inspector"
  check "pane where click occurred does not flash" . not =<< wasPaneFlashed "Pinboard"
  now <- evalUi fctx iinp uiTime
  check "pane currently flashing" =<< isPaneFlashing "Inspector" now
  check "paneFlashing query also detects it" =<< evalUi fctx iinp (paneFlashing "Inspector")
  check "flash expires after duration" . not =<< isPaneFlashing "Inspector" (now + 1.0)

  clearPaneFlashes
  _ <- fframe iinp
  spansSelf <- collectTextSpans fctx
  selfB <- findSpan "SelfIncrement" spansSelf
  fclick selfB
  check "mouse click inside pane does not cause that pane to flash" . not =<< wasPaneFlashed "Inspector"

  clearPaneFlashes
  _ <- fframe iinp
  spansScroll <- collectTextSpans fctx
  pinText <- findSpan "Pinboard content" spansScroll
  fscroll pinText
  check "scrolling with scroll wheel does not flash scrolled pane" . not =<< wasPaneFlashed "Pinboard"
  check "scrolling with scroll wheel does not flash other pane" . not =<< wasPaneFlashed "Inspector"

  cVal <- readVarIO counterVar
  clearPaneFlashes
  _ <- fframe iinp
  spansAfter <- collectTextSpans fctx
  let countText = "Count: " <> tshow cVal
  countSpan <- findSpan countText spansAfter
  fclick countSpan
  check "mouse click without value change does not flash" . not =<< wasPaneFlashed "Inspector"

  putStrLn "core tabs, flashing and dragging"
  check "PCore preset groups code panes into Source tab host" $
    treeKinds (panelPreset PCore) == [paneCoreBindings, paneInspector, paneCoreNotes, paneCoreSource]
  coreCtx <- newContext
  cst <- newCoreState
  cpopped <- newVar []
  clearPaneFlashes
  let cbench = csPanes cst
      cplaces = [BenchPlace "Core & STG" cbench]
      srcDump =
        T.unlines
          [ "module Main where"
          , ""
          , "go :: Int -> Int -> Int"
          , "go 0 acc = acc"
          , "go n acc = go (n - 1) (acc + n)"
          , ""
          , "main :: IO ()"
          , "main = print (go 10 0)"
          ]
      coreDump =
        T.unlines
          [ "==================== Tidy Core ===================="
          , "Rec {"
          , "Main.$wgo [InlPrag=[2], Occ=LoopBreaker]"
          , "  :: GHC.Prim.Int# -> GHC.Prim.Int# -> GHC.Prim.Int#"
          , "[GblId, Arity=2, Str=<1L><L>, Unf=OtherCon []]"
          , "Main.$wgo"
          , "  = \\ (ww_s1 :: GHC.Prim.Int#) (ww1_s2 :: GHC.Prim.Int#) ->"
          , "      case ww_s1 of ds_X0 {"
          , "        __DEFAULT -> Main.$wgo (GHC.Prim.-# ds_X0 1#) (GHC.Prim.+# ww1_s2 ds_X0);"
          , "        0# -> ww1_s2"
          , "      }"
          , "end Rec }"
          , ""
          , "Main.go [InlPrag=INLINE[final]] :: Int -> Int -> Int"
          , "[GblId, Arity=2, Str=<1!P(L)><1!P(L)>, Cpr=1]"
          , "Main.go"
          , "  = \\ (n_a1 :: Int) (acc_a2 :: Int) ->"
          , "      case n_a1 of { GHC.Types.I# ww_s1 ->"
          , "      case acc_a2 of { GHC.Types.I# ww1_s2 ->"
          , "      case Main.$wgo ww_s1 ww1_s2 of ww2_s3 { __DEFAULT -> GHC.Types.I# ww2_s3 }"
          , "      }"
          , "      }"
          , ""
          , "Main.main :: IO ()"
          , "[GblId]"
          , "Main.main = System.IO.print @Int GHC.Show.$fShowInt (Main.go 10 0)"
          ]
      stgDump =
        T.unlines
          [ "Main.go [GblId, Arity=2] :: GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Int"
          , "[GblId, Arity=2, Unf=OtherCon []] ="
          , "    \\r [n_s1 acc_s2]"
          , "        let {"
          , "          sat_s3 [Occ=Once1] :: GHC.Types.Int"
          , "          [LclId] ="
          , "              {n_s1, acc_s2} \\u [] Main.go n_s1 acc_s2;"
          , "        } in  sat_s3;"
          , ""
          , "Main.main :: GHC.Types.IO ()"
          , "[GblId] ="
          , "    \\r [void_0E] Main.go 10 0;"
          ]
      cloaded =
        CoreLoaded
          { clArtifacts = Nothing
          , clSource = Just (Right (parseCodeFile KindSource "Main.hs" srcDump))
          , clCore = Just (Right (parseCodeFile KindCore "Main.dump-simpl" coreDump))
          , clStg = Just (Right (parseCodeFile KindStg "Main.dump-stg-final" stgDump))
          , clIndex = mkSourceIndex [] []
          , clSites = Nothing
          }
      cinp = withInput 1300 1000
      coreScene = void $ columnWith (fixedWH 1300 1000) $ do
        need <- maybe [] treeKinds <$> readVar (wbTree cbench)
        draw <- corePanes (worldEnv w PCore) cst cloaded need
        workspace cplaces cpopped cbench draw
      coreFrame inp = evalUi coreCtx inp coreScene
      cclick pos = let (p, r) = clickPair cinp pos in mapM_ coreFrame [p, r, cinp, cinp]
      cdrag fromPos@(V2 fx fy) toPos =
        let p0 = pressAt cinp fromPos
            h1 = holdAt p0 (V2 (fx + 12) fy)
            h2 = holdAt h1 toPos
            r0 = releaseAt h2
         in mapM_ coreFrame [p0, h1, h2, h2, r0, cinp, cinp]
  warmup2 coreCtx cinp coreScene >>= \_ -> pure ()
  _ <- coreFrame cinp
  check "Source, Core, STG are in tabs by default" . (== Just [paneCoreSource, paneCoreCore, paneCoreStg]) . M.lookup paneCoreSource =<< readVarIO (csTabGroups cst)
  cspans0 <- collectTextSpans coreCtx
  check "all three tab headers and active tab file are rendered" $
    hasText "Source" cspans0 && hasText "Core" cspans0 && hasText "STG" cspans0 && hasText "Main.hs" cspans0 && not (hasText "Main.dump-simpl" cspans0)

  clearPaneFlashes
  coreTabPos <- findSpan "Core" cspans0
  cclick coreTabPos
  cspansCore <- collectTextSpans coreCtx
  check "clicking Core tab switches active tab to Core" (hasText "Main.dump-simpl" cspansCore)
  check "switching tabs directly does not trigger a flash" . not =<< wasPaneFlashed "Core"
  srcTabPos <- findSpan "Source" cspansCore
  cclick srcTabPos
  check "clicking Source tab switches back to Source" . hasText "Main.hs" =<< collectTextSpans coreCtx

  clearPaneFlashes
  cspans1 <- collectTextSpans coreCtx
  goBindPos <- findSpan "go" cspans1
  cclick goBindPos
  check "clicking a binding flashes active Source tab" =<< wasPaneFlashed "Source"
  check "clicking a binding flashes background Core tab" =<< wasPaneFlashed "Core"
  check "clicking a binding flashes background STG tab" =<< wasPaneFlashed "STG"
  check "clicking a binding flashes Optimiser notes pane" =<< wasPaneFlashed "go"
  check "Bindings pane where click occurred does not flash" . not =<< wasPaneFlashed "Bindings"

  clearPaneFlashes
  _ <- coreFrame cinp
  cspans2 <- collectTextSpans coreCtx
  let boxSpans = [V2 (x + bw / 2) (y + bh / 2) | (Rect x y bw bh, t, _, _, _) <- cspans2, "box @" `T.isPrefixOf` t]
  for_ (take 1 boxSpans) cclick
  check "clicking a boxing note in Optimiser notes flashes background Core tab" =<< wasPaneFlashed "Core"
  check "clicking a boxing note does not flash unchanged Source tab" . not =<< wasPaneFlashed "Source"

  clearPaneFlashes
  _ <- coreFrame cinp
  cspans3 <- collectTextSpans coreCtx
  let allocSpans = [V2 (x + bw / 2) (y + bh / 2) | (Rect x y bw bh, t, _, _, _) <- cspans3, "alloc @" `T.isPrefixOf` t]
  for_ (take 1 allocSpans) cclick
  check "clicking an alloc note in Optimiser notes flashes background STG tab" =<< wasPaneFlashed "STG"
  check "clicking an alloc note does not flash unchanged Source tab" . not =<< wasPaneFlashed "Source"

  -- Dragging a tab within the tab strip reorders the tabs.
  cspans4 <- collectTextSpans coreCtx
  stgPos <- findSpan "STG" cspans4
  V2 sx sy <- findSpan "Source" cspans4
  cdrag stgPos (V2 (sx - 6) sy)
  check "dragging STG tab onto Source tab reorders the tab group" . (== Just [paneCoreStg, paneCoreSource, paneCoreCore]) . M.lookup paneCoreSource =<< readVarIO (csTabGroups cst)

  -- Dragging a tab out to a pane edge splits a new pane in the workbench grid.
  cspans5 <- collectTextSpans coreCtx
  corePos2 <- findSpan "Core" cspans5
  cdrag corePos2 (V2 1250 600)
  check "dragging Core tab out to a pane edge splits it into its own pane" . elem paneCoreCore . maybe [] treeKinds =<< readVarIO (wbTree cbench)
  check "split Core pane has its own tab group" . (== Just [paneCoreCore]) . M.lookup paneCoreCore =<< readVarIO (csTabGroups cst)

  -- Dragging a tab from a split pane back onto the tab strip merges it back.
  cspans6 <- collectTextSpans coreCtx
  let coreHeaders = [V2 (x + bw / 2) (y + bh / 2) | (Rect x y bw bh, t, _, _, _) <- cspans6, t == "Core"]
  srcPos2 <- findSpan "Source" cspans6
  for_ (take 1 coreHeaders) $ \cpos -> cdrag cpos srcPos2
  check "dragging split Core tab back onto Source tab merges it back" . not . elem paneCoreCore . maybe [] treeKinds =<< readVarIO (wbTree cbench)
  check "merged tab group holds all three code tabs again" . (== 3) . maybe 0 length . M.lookup paneCoreSource =<< readVarIO (csTabGroups cst)

  putStrLn "markers header"
  mhCtx <- newContext
  tlState <- newTimelineState
  let mhInp = withInput 600 200
      makeTl (count :: Int) =
        emptyTimeline
          { tlMarkers = V.fromList [Marker (fromIntegral i) ("m" <> tshow i) Nothing True | i <- [1 .. count]]
          }
      renderHeader inp tl = evalUi mhCtx inp $
        paneWith tight floatingPane "Markers" (markersHeader tlState tl) (pure ())

  _ <- renderHeader mhInp (makeTl 0)
  inputNodes0 <- nodesOfType NodeTextInput mhCtx
  _ <- renderHeader mhInp (makeTl 54)
  inputNodes54 <- nodesOfType NodeTextInput mhCtx
  _ <- renderHeader mhInp (makeTl 120)
  inputNodes120 <- nodesOfType NodeTextInput mhCtx

  let inputRect nodes = case nodes of
        [(_, r)] -> pure r
        _ -> fail ("expected 1 text input node, got " <> show (length nodes))
  Rect x0 _ w0 _ <- inputRect inputNodes0
  Rect x54 _ w54 _ <- inputRect inputNodes54
  Rect x120 _ _ _ <- inputRect inputNodes120

  check "markers number changing does not move search box" (x0 == x54 && x54 == x120)
  check "search box width is consistent and fills remaining space" (w0 == w54 && w0 > 200)

  _ <- renderHeader (withInput 800 200) (makeTl 54)
  [(_, Rect _ _ w800 _)] <- nodesOfType NodeTextInput mhCtx
  check "search box fills expanded remaining space" (w800 > w54 + 190)

  putStrLn "timeline readout subtitle"
  roCtx <- newContext
  let roInp = withInput 1280 200
      renderReadout (majorCount :: Int) = evalUi roCtx roInp $
        readoutDeltaWith
          [ ("duration", "52.938 s", Nothing, id)
          , ("collections, " <> tshow majorCount <> " major", "2,950", Nothing, fixedW 144)
          , ("longest pause", "187.61 ms", Nothing, id)
          ]
      findX text spans = case [rectX r | (r, t, _, _, _) <- spans, t == text] of
        (x : _) -> pure x
        [] -> fail ("no span " <> T.unpack text)

  _ <- renderReadout 0
  spans0 <- collectTextSpans roCtx
  xPause0 <- findX "longest pause" spans0

  _ <- renderReadout 221
  spans221 <- collectTextSpans roCtx
  xPause221 <- findX "longest pause" spans221

  _ <- renderReadout 999
  spans999 <- collectTextSpans roCtx
  xPause999 <- findX "longest pause" spans999

  check "collections subtitle changing does not move subsequent readouts" (xPause0 == xPause221 && xPause221 == xPause999)

  putStrLn "leak suspects table layout"
  lsCtx <- newContext
  hstate <- newHeapState
  let baseHp = fromSamples "" "closure type" 0 (const Nothing)
        [(t, [("THUNK_2_0", t * 4e6), ("ghc-internal:GHC.Internal.Bignum.Integer.IS", t * 2e6)]) | t <- [1 .. 5]]
      curHp1 = fromSamples "" "closure type" 0 (const Nothing)
        [(t, [("THUNK_2_0", t * 4.2e6), ("ghc-internal:GHC.Internal.Bignum.Integer.IS", t * 2.1e6)]) | t <- [1 .. 5]]
      renderLeakView curHp = evalUi lsCtx (withInput 1000 600) $ do
        let env = (worldEnv w PHeap) {envBaseline = Just (Baseline bpaths Nothing (Just baseHp) Nothing Nothing)}
        void (columnWith (fixedWH 1000 600) (leakView env hstate curHp))

  let findPrefixR prefix spans = case [rectX r + rectW r | (r, t, _, _, _) <- spans, prefix `T.isPrefixOf` t] of
        (x : _) -> pure x
        [] -> fail ("no span with prefix " <> T.unpack prefix)
      findPrefixX prefix spans = case [rectX r | (r, t, _, _, _) <- spans, prefix `T.isPrefixOf` t] of
        (x : _) -> pure x
        [] -> fail ("no span with prefix " <> T.unpack prefix)

  _ <- renderLeakView curHp1
  spans1 <- collectTextSpans lsCtx
  xStatusHdr1 <- findPrefixX "status" spans1
  rThunkBefore1 <- findPrefixR "19.1" spans1
  rIsBefore1 <- findPrefixR "9.5" spans1
  xThunkNow1 <- findPrefixX "20.0" spans1
  xIsNow1 <- findPrefixX "10.0" spans1

  let curHp3 = fromSamples "" "closure type" 0 (const Nothing)
        [(t, [("THUNK_2_0", t * 90e6), ("ghc-internal:GHC.Internal.Bignum.Integer.IS", t * 50e6)]) | t <- [1 .. 5]]
  _ <- renderLeakView curHp3
  spans3 <- collectTextSpans lsCtx
  xStatusHdr3 <- findPrefixX "status" spans3
  rThunkBefore3 <- findPrefixR "19.1" spans3
  rIsBefore3 <- findPrefixR "9.5" spans3

  check "columns align across rows with different name lengths" (rThunkBefore1 == rIsBefore1 && xThunkNow1 == xIsNow1)
  check "values changing does not move column positions" (xStatusHdr1 == xStatusHdr3 && rThunkBefore1 == rThunkBefore3 && rIsBefore3 == rThunkBefore3)

  -- A vertical bar's lane must not tip the tables into horizontal scrolling:
  -- with more baseline suspects than the table's fixed height shows, the body
  -- scrolls vertically and the vertical bar's lane narrows its viewport. At
  -- this pane width the columns still fit that narrowed viewport, so the
  -- stretch column absorbs the lane and no horizontal scrollbar appears.
  let baseMany = fromSamples "" "closure type" 0 (const Nothing)
        [(t, [(T.pack ("old" <> show k), t * fromIntegral (k + 2) * 1e6) | k <- [1 .. 10 :: Int]]) | t <- [1 .. 5]]
      curMany = fromSamples "" "closure type" 0 (const Nothing)
        [(t, [("THUNK_2_0", t * 4.2e6), ("ghc-internal:GHC.Internal.Bignum.Integer.IS", t * 2.1e6)]) | t <- [1 .. 5]]
      laneW = 470
      renderLane = evalUi lsCtx (withInput laneW 600) $ do
        let env = (worldEnv w PHeap) {envBaseline = Just (Baseline bpaths Nothing (Just baseMany) Nothing Nothing)}
        void (columnWith (fixedWH laneW 600) (leakView env hstate curMany))
  _ <- renderLane
  _ <- renderLane
  laneScrolls <- nodesOfType NodeScrollContainer lsCtx
  laneChecks <- forM laneScrolls $ \(wid, _) -> do
    mMetrics <- getScrollMetricsIO lsCtx wid
    pure $ case mMetrics of
      Just sm | scrollAxes sm == ScrollAxisXY -> v2X (scrollRange sm) == 0
      _ -> True
  check "no horizontal scrollbar beside the vertical bar at 470px width" (and laneChecks)

  -- Squeezed, not scrolled: below the pinned columns' total (stretch 0 +
  -- 100 + 100 + 200 + rules = 403), the tables compress their columns
  -- proportionally instead of growing a horizontal bar. Across a run of
  -- narrow widths, every 2D body stays horizontally still, whether the rows
  -- overflow (a 10-suspect baseline) or not (the two-suspect one).
  squeezeChecks <- forM [340, 360 .. 500] $ \winW -> do
    sqCtx <- newContext
    sqState <- newHeapState
    let sqHp = fromSamples "" "closure type" 0 (const Nothing)
          [(t, [("THUNK_2_0", t * 4.2e6), ("ghc-internal:GHC.Internal.Bignum.Integer.IS", t * 2.1e6)]) | t <- [1 .. 5]]
        sqRender = evalUi sqCtx (withInput winW 600) $ do
          let env = (worldEnv w PHeap) {envBaseline = Just (Baseline bpaths Nothing (Just baseMany) Nothing Nothing)}
          void (columnWith (fixedWH winW 600) (leakView env sqState sqHp))
    _ <- sqRender
    _ <- sqRender
    sqScrolls <- nodesOfType NodeScrollContainer sqCtx
    overs <- forM sqScrolls $ \(wid, _) -> do
      mMetrics <- getScrollMetricsIO sqCtx wid
      pure $ case mMetrics of
        Just sm | scrollAxes sm == ScrollAxisXY -> v2X (scrollRange sm) > 0
        _ -> False
    sqSpans <- collectTextSpans sqCtx
    let hasStretchCol = any (\(Rect _ _ sw _, t, _, _, _) -> sw > 16 && "le" `T.isPrefixOf` t) sqSpans
    pure (not (or overs) && hasStretchCol)
  check "tables squeeze instead of scrolling sideways at narrow widths and keep stretch column visible" (and squeezeChecks)

  -- When all rows fit inside the table's max height (<= 8 rows), no vertical
  -- scrollbar appears, including under 15pt TrueType metrics (lineHeight = 20).
  fitVertChecks <- forM [400, 500, 800] $ \winW -> do
    fvCtx0 <- newContext
    let fvCtx = withFontMetrics fvCtx0 ((ctxFontMetrics fvCtx0) {fmLineHeight = 20, fmAscent = 15})
    fvState <- newHeapState
    let fvRender = evalUi fvCtx (withInput winW 600) $ do
          let env = (worldEnv w PHeap) {envBaseline = Just (Baseline bpaths Nothing (Just baseHp) Nothing Nothing)}
          void (columnWith (fixedWH winW 600) (leakView env fvState curHp1))
    _ <- fvRender
    _ <- fvRender
    fvScrolls <- nodesOfType NodeScrollContainer fvCtx
    vOvers <- forM fvScrolls $ \(wid, _) -> do
      mMetrics <- getScrollMetricsIO fvCtx wid
      pure $ case mMetrics of
        Just sm | scrollAxes sm == ScrollAxisXY -> v2Y (scrollRange sm) > 0
        _ -> False
    pure (not (or vOvers))
  check "leak suspect tables hide vertical scrollbar when all rows fit" (and fitVertChecks)

  -- settle sweep: the squeeze reacts to last frame's rect, so it must reach a
  -- fixed point and stop, at every width; an oscillation (squeeze changes
  -- wrapping, wrapping toggles a bar lane, the lane changes the budget, ...)
  -- would keep re-running frames forever and eat pane drags.
  settleChecks <- forM [340, 342 .. 520] $ \winW -> do
    stCtx <- newContext
    stState <- newHeapState
    let stHp = fromSamples "" "closure type" 0 (const Nothing)
          [(t, [("THUNK_2_0", t * 4.2e6), ("ghc-internal:GHC.Internal.Bignum.Integer.IS", t * 2.1e6)]) | t <- [1 .. 5]]
        stEnv = (worldEnv w PHeap) {envBaseline = Just (Baseline bpaths Nothing (Just baseMany) Nothing Nothing)}
        stRender = evalUi stCtx (withInput winW 600) $
          void (columnWith (fixedWH winW 600) (leakView stEnv stState stHp))
    settled <- do
      let pump :: Int -> Maybe [(Rect, Maybe V2)] -> IO (Maybe Int)
          pump 0 _ = pure Nothing
          pump k prev = do
            _ <- stRender
            scrolls <- nodesOfType NodeScrollContainer stCtx
            state <- forM scrolls $ \(wid, r) -> do
              m <- getScrollMetricsIO stCtx wid
              pure (r, fmap scrollRange m)
            if Just state == prev
              then pure (Just (k :: Int))
              else pump (k - 1) (Just state)
      pump 25 Nothing
    case settled of
      Nothing -> putStrLn ("never settles at " <> show winW) >> pure False
      Just _ -> pure True
  check "leak view settles at every narrow width" (and settleChecks)

  putStrLn "at cursor table layout and vertical centering"
  acCtx <- newContext
  acState <- newHeapState
  let acHp = fromSamples "" "closure type" 0 (const Nothing)
        [ (1, [ ("THUNK_2_0", 19.5e6)
              , ("ghc-internal:GHC.Internal.Bignum.Integer.IS", 9.7e6)
              ])
        ]
      renderAtCursor winW winH ctx state = evalUi ctx (withInput winW winH) $ do
        let env = worldEnv w PHeap
        void (columnWith (fixedWH winW winH) (atCursor env state acHp 0))

  _ <- renderAtCursor 1200 600 acCtx acState
  acSpans <- collectTextSpans acCtx
  let headerSpans = [s | s@(Rect _ y _ _, _, _, _, _) <- acSpans, y > 20, y < 50]
      headerRects = sortOn rectX [r | (r, _, _, _, _) <- headerSpans]
      noHeaderOverlaps = and [rectX r1 + rectW r1 <= rectX r2 + 0.1 | (r1, r2) <- zip headerRects (drop 1 headerRects)]
  check "headers do not collide at 1200px width" (length headerRects >= 5 && noHeaderOverlaps)

  -- Table row 0 cells are vertically centered with each other and with the row canvas
  cNodes <- nodesOfType NodeDrawing acCtx
  let row0CanvasCenter = case [rectY r + rectH r / 2 | (_, r) <- cNodes, rectY r > 50, rectY r < 80] of
        (cy : _) -> cy
        [] -> 0
      row0Spans = [s | s@(Rect _ y _ _, _, _, _, _) <- acSpans, y > 50, y < 80]
      row0Centers = [y + h / 2 | (Rect _ y _ h, _, _, _, _) <- row0Spans]
      cellsVertCentered = row0CanvasCenter > 0 && length row0Centers >= 5 && all (\c -> abs (c - row0CanvasCenter) < 1.0) row0Centers
  check "table row cells are vertically centered with the color canvas" cellsVertCentered

  -- In a 650px pane, column 0 dynamically truncates long names with ellipsis, while short names fit
  medCtx <- newContext
  medState <- newHeapState
  _ <- renderAtCursor 650 600 medCtx medState
  _ <- renderAtCursor 650 600 medCtx medState
  medSpans <- collectTextSpans medCtx
  let medTexts = [t | (_, t, _, _, _) <- medSpans]
  check "long closure name truncates dynamically with ellipsis in medium pane" (any (\t -> "..." `T.isInfixOf` t || "…" `T.isInfixOf` t) medTexts)
  check "short closure name remains untruncated in medium pane" ("THUNK_2_0" `elem` medTexts)

  -- In a 500px pane, all columns fit without horizontal scroll and headers do not overlap
  fit500Ctx <- newContext
  fit500State <- newHeapState
  _ <- renderAtCursor 500 600 fit500Ctx fit500State
  _ <- renderAtCursor 500 600 fit500Ctx fit500State
  fit500Scrolls <- nodesOfType NodeScrollContainer fit500Ctx
  horz500Overflows <- forM fit500Scrolls $ \(wid, _) -> do
    mMetrics <- getScrollMetricsIO fit500Ctx wid
    pure $ case mMetrics of
      Just sm -> v2X (scrollRange sm) > 0
      Nothing -> False
  check "all columns fit without horizontal scroll at 500px width" (not (or horz500Overflows))

  -- Header cells do not collide or overflow across widths
  spans500 <- collectTextSpans fit500Ctx
  let medHeaderSpans = [s | s@(Rect _ y _ _, _, _, _, _) <- spans500, y > 20, y < 50]
      medHeaderRects = sortOn rectX [r | (r, _, _, _, _) <- medHeaderSpans]
      noMedOverlaps = and [rectX r1 + rectW r1 <= rectX r2 + 0.1 | (r1, r2) <- zip medHeaderRects (drop 1 medHeaderRects)]
  check "headers do not overlap at 500px width" (length medHeaderRects >= 5 && noMedOverlaps)

  -- Table height responds to being resized
  resizeCtx1 <- newContext
  resizeState1 <- newHeapState
  let manyRows = [(1, [("ghc-internal:GHC.Internal.Bignum.Integer.IS", 9.7e6)] ++ [("band_" <> tshow k, fromIntegral k * 1e5) | k <- [0 .. 99 :: Int]])]
      manyHp = fromSamples "" "closure type" 0 (const Nothing) manyRows
      renderMany winW winH ctx state = evalUi ctx (withInput winW winH) $ do
        let env = worldEnv w PHeap
        void (columnWith (fixedWH winW winH) (atCursor env state manyHp 0))

  _ <- renderMany 1000 600 resizeCtx1 resizeState1
  _ <- renderMany 1000 600 resizeCtx1 resizeState1
  scrolls1 <- nodesOfType NodeScrollContainer resizeCtx1
  let bodyH1 = case [rectH r | (_, r) <- scrolls1, rectH r > 50] of
        (h : _) -> h
        [] -> 0

  resizeCtx2 <- newContext
  resizeState2 <- newHeapState
  _ <- renderMany 1000 350 resizeCtx2 resizeState2
  scrolls2 <- nodesOfType NodeScrollContainer resizeCtx2
  let bodyH2 = case [rectH r | (_, r) <- scrolls2, rectH r > 50] of
        (h : _) -> h
        [] -> 0

  check "table height responds to being resized" (bodyH2 > 0 && bodyH1 > bodyH2 && abs ((bodyH1 - bodyH2) - 250) < 5)

  -- Test vertical scrollbar does not appear when content fits vertically across widths
  let fiveRows = [(1, [("THUNK_2_0", 19.5e6), ("ghc-internal:GHC.Internal.Bignum.Integer.IS", 9.7e6), ("band1", 1e6), ("band2", 2e6), ("band3", 3e6)])]
      fiveHp = fromSamples "" "closure type" 0 (const Nothing) fiveRows
      renderFive winW winH ctx state = evalUi ctx (withInput winW winH) $ do
        let env = worldEnv w PHeap
        void (columnWith (fixedWH winW winH) (atCursor env state fiveHp 0))

  noScrollChecks <- forM [1200, 800, 600, 500, 400] $ \testW -> do
    tCtx <- newContext
    tState <- newHeapState
    _ <- renderFive testW 400 tCtx tState
    _ <- renderFive testW 400 tCtx tState
    scrolls <- nodesOfType NodeScrollContainer tCtx
    hasAnyOverflow <- forM scrolls $ \(wid, _) -> do
      mMetrics <- getScrollMetricsIO tCtx wid
      pure $ case mMetrics of
        Just sm -> v2X (scrollRange sm) > 0 || v2Y (scrollRange sm) > 0
        Nothing -> False
    pure (not (or hasAnyOverflow))
  check "no scrollbar when table fits both axes across widths (1200 down to 400)" (and noScrollChecks)

  putStrLn "theme styles and SVG graph export"
  let qBtn = themeButton (quiet (strongheadTheme (Just AppearanceDark)))
  check "quiet button keeps transparent resting fill and visible hover/press shades"
    (colorA (styleBg qBtn) == 0 && colorA (styleHoverBg qBtn) > 0 && colorA (styleActiveBg qBtn) > 0)
  hpSvg <- renderHpSvg Nothing Nothing curHp1
  check "renderHpSvg emits SVG with shapes and heap text"
    ("<svg " `T.isPrefixOf` hpSvg && "<path " `T.isInfixOf` hpSvg && "THUNK_2_0" `T.isInfixOf` hpSvg)
  diffSvg <- renderHpDiffSvg Nothing Nothing baseHp curHp1
  check "renderHpDiffSvg emits diff SVG with both panels"
    ("<svg " `T.isPrefixOf` diffSvg && "Baseline" `T.isInfixOf` diffSvg && "Candidate" `T.isInfixOf` diffSvg)

  putStrLn "flamegraph, heatmaps, laziness X-ray, and post-mortem dossier"
  let ftA = fromFoldedStacks "On-CPU" "samples" [(["main", "worker", "parse"], 60), (["main", "flush"], 40)]
      ftB = fromFoldedStacks "On-CPU" "samples" [(["main", "worker", "parse"], 25), (["main", "flush"], 75)]
  fgSvg <- renderFlamegraphSvg Nothing ftA
  check "renderFlamegraphSvg emits SVG with stack frame labels"
    ("<svg " `T.isPrefixOf` fgSvg && "worker" `T.isInfixOf` fgSvg && "flush" `T.isInfixOf` fgSvg)
  fgCtx <- newContext
  fgZoom <- newVar Nothing
  let hmTl = emptyTimeline {tlStart = 0, tlEnd = 10_000_000, tlGCs = V.fromList [GCEvent 1_000_000 1_500_000 0 0 1024 1, GCEvent 5_000_000 8_000_000 0 1 65536 2]}
      eraHp = fromSamples "" "era" 0 (const Nothing) [(1, [("era 1", 4096), ("era 2", 1024)]), (2, [("era 1", 4096), ("era 2", 8192)])]
      vizScene = columnWith (fixedWH 900 700 . tight . gap 6) $ do
        flamegraphView ftA fgZoom
        diffFlamegraphView (diffFlameTrees ftA ftB)
        latencyHeatmapView (gcPauseHeatmap hmTl 16 8)
        eraCohortHeatmapView eraHp
  warmup2 fgCtx (withInput 900 700) vizScene >>= \_ -> pure ()
  fgSpans <- collectTextSpans fgCtx
  check "flamegraph, diff flamegraph, latency heatmap, and era cohort matrix render cleanly"
    (hasText "On-CPU" fgSpans && hasText "regressed" fgSpans && hasText "Era cohort retention matrix" fgSpans)

  xrCtx <- newContext
  xrState <- newDebugState
  writeVarIO (dsStop xrState) (Just (StopInfo "Main.go" (Just (SrcSpan "Main.hs" 10 1 10 20))))
  writeVarIO (dsBindings xrState) [GhciBinding "acc" "Integer" (Just "_t1"), GhciBinding "done" "Int" (Just "42")]
  writeVarIO (dsValues xrState) (M.fromList [("acc", VThunk "_t1" Nothing), ("done", VAtom "42")])
  writeVarIO (dsXRay xrState) (M.fromList [("acc", ValueXRay "acc" (ThunkAging 4) 1 1), ("done", ValueXRay "done" ThunkCollapsed 0 1)])
  writeVarIO (dsHistory xrState) [(-1, "Main.step", SrcSpan "Main.hs" 9 1 9 15)]
  let xrScene = do
        draw <- debugPanes (worldEnv w PDebugger) xrState (mkSourceIndex [] []) Nothing []
        void (columnWith (fixedWH 800 500) (fromMaybe (pure ()) (draw paneDbgLocals floatingPane)))
  warmup2 xrCtx (withInput 800 500) xrScene >>= \_ -> pure ()
  xrSpans <- collectTextSpans xrCtx
  check "debugger renders Laziness X-Ray badges and time-travel scrubber"
    (hasText "unforced ×4" xrSpans && hasText "collapsed" xrSpans && hasText "Time-travel scrubber" xrSpans)

  let dossierMd = pinsDossier (worldEnv w PHeap) ["Generated in test."] [("Heap Proof", "run.heap.svg")] [Pin thread "blocked on MVar"]
  check "pinsDossier embeds proof graphs and pins"
    ("Stronghead Post-Mortem Dossier" `T.isInfixOf` dossierMd && "![Heap Proof](run.heap.svg)" `T.isInfixOf` dossierMd)

  putStrLn "UI/UX polish regressions (right-aligned menus, untruncated headers, responsive header, shortcuts, warm ink)"
  -- Pane menu stays pinned to the far right even with empty trailing content.
  menuCtx <- newContext
  menuBench <- newWorkbench "menu-test" (Pane paneInspector)
  menuPopped <- newVar []
  let menuScene =
        void $ columnWith (fixedWH 700 400) $
          workspace [BenchPlace "M" menuBench] menuPopped menuBench $ \pid ctx ->
            if pid == paneInspector
              then Just (pane ctx "Inspector" (pure ()) (void (label "empty body")))
              else Nothing
  warmup2 menuCtx (withInput 700 400) menuScene >>= \_ -> pure ()
  menuSpans <- collectTextSpans menuCtx
  let menuXs = [rectX r | (r, t, _, _, _) <- menuSpans, t == "⋯"]
  check "pane menu is pinned to the right edge even with empty trailing header" (any (> 620) menuXs)

  -- At cursor table headers remain untruncated under realistic 15pt font metrics.
  let ttfMetrics c = withFontMetrics c ((ctxFontMetrics c) {fmLineHeight = 20, fmAscent = 15, fmAdvance = const 7.5})
  acTtfCtx <- ttfMetrics <$> newContext
  acTtfState <- newHeapState
  _ <- renderAtCursor 1200 600 acTtfCtx acTtfState
  _ <- renderAtCursor 1200 600 acTtfCtx acTtfState
  acTtfSpans <- collectTextSpans acTtfCtx
  ac500Ctx <- ttfMetrics <$> newContext
  ac500State <- newHeapState
  _ <- renderAtCursor 500 600 ac500Ctx ac500State
  _ <- renderAtCursor 500 600 ac500Ctx ac500State
  ac500Spans <- collectTextSpans ac500Ctx
  check "at cursor headers (residency, delta previous, share) are untruncated at 1200px"
    (hasText "residency" acTtfSpans && hasText "Δ previous" acTtfSpans && hasText "share" acTtfSpans)
  check "at cursor headers remain untruncated at 500px"
    (hasText "residency" ac500Spans && hasText "Δ previous" ac500Spans && hasText "share" ac500Spans)

  -- Top shell header fits without clipping at 960px, 1100px, and 1500px under 15pt font metrics.
  appRes <- newApp (defaultSettings {cfgRestoreWindow = False, cfgReopenSession = False}) (defaultOptions {optPersist = False})
  for_ [960 :: Float, 1100, 1500] $ \winW -> do
    shCtx <- ttfMetrics <$> newContext
    let shInp = withInput winW 640
    warmup2 shCtx shInp (appView appRes) >>= \_ -> pure ()
    shSpans <- collectTextSpans shCtx
    let hdrOverflow = [t | (Rect x y bw _, t, _, _, _) <- shSpans, y < 44, x + bw > winW + 0.5]
    check ("top header fits cleanly at " <> show (round winW :: Int) <> "px width") (null hdrOverflow)

  scCtx <- newContext
  let scInp = withInput 1400 900
  warmup2 scCtx scInp (appView appRes) >>= \_ -> pure ()
  _ <- evalUi scCtx (keyInp (KeyF 1) scInp) (appView appRes)
  _ <- evalUi scCtx scInp (appView appRes)
  scOverlay <- collectOverlayTextSpans scCtx scInp
  check "F1 opens Keyboard shortcuts modal with keycaps"
    (hasText "Keyboard shortcuts" scOverlay && hasText "Ctrl+K" scOverlay && hasText "F5" scOverlay && hasText "Shift+F11" scOverlay)
  _ <- evalUi scCtx (keyInp KeyEscape scInp) (appView appRes)
  _ <- evalUi scCtx scInp (appView appRes)
  scClosed <- collectOverlayTextSpans scCtx scInp
  check "Esc closes Keyboard shortcuts modal" (not (hasText "Keyboard shortcuts" scClosed))

  -- Warm ink check: no cold blue tint on ground or pane surfaces in either theme.
  let warmOk mode =
        let i = ink (strongheadTheme (Just mode))
            warm c = colorR c >= colorB c
         in warm (inkGround i) && warm (inkPane i) && warm (inkRule i)
  check "both Dark and Light palettes preserve Stronghead's warm neutral tone"
    (warmOk AppearanceDark && warmOk AppearanceLight)

  -- Heap diff header buttons stay inside a 480px split pane without pushing Clear off-edge.
  diffCtx <- ttfMetrics <$> newContext
  diffSt <- newBaselineState
  diffBus <- newBus
  let diffEnv = Env diffBus [] PHeap (const (pure ())) haveNothing Nothing
      diffHp = fromSamples "prog +RTS -hT" "closure type" 0 (const Nothing) [(1.0, [("THUNK_2_0", 1000000)])]
      diffPaths = noBaselinePaths {blEventlog = Just "C:\\Users\\zach\\Dev\\stronghead\\stronghead-sample.eventlog"}
      diffBase = Baseline diffPaths Nothing (Just diffHp) Nothing Nothing
      diffLoaded = BaselineLoaded (Just diffBase) False Nothing
      diffNow = DiffNow (Just diffHp) Nothing (Right (pure ()))
      diffUi = do
        setTheme (strongheadTheme (Just AppearanceDark))
        writeVar (bvPaths diffSt) diffPaths
        columnWith (fillW . fillH . tight) $
          for_ (diffPanes diffEnv diffSt diffLoaded diffNow paneDiffHeap (PaneCtx Nothing (Just (menuItem "Close")) (Just paneDiffHeap) Nothing)) id
  warmup2 diffCtx (withInput 480 400) diffUi >>= \_ -> pure ()
  diffSpans <- collectTextSpans diffCtx
  let diffOverflow = [t | (Rect x y bw _, t, _, _, _) <- diffSpans, y < 40, x + bw > 480.5]
  check "Heap diff header buttons fit cleanly inside 480px split pane"
    (hasText "Mark again" diffSpans && hasText "Clear" diffSpans && null diffOverflow)
  check "Heap diff pane renders visual total residency comparison chart"
    (hasText "Total live residency over elapsed run time" diffSpans)

  putStrLn "Unified workbench: Inspector triage feed, Lens query pills, and Runner & lab pane"
  wlSt <- newWorkbenchLabState
  triCtx <- newContext
  queriedRef <- newIORef ("" :: Text)
  let sampleFinding =
        Finding
          { fnId = "leak-1"
          , fnSeverity = SevCritical
          , fnCategory = CatMemory
          , fnConfidence = 0.94
          , fnTitle = "Unforced accumulator thunk in Main.step"
          , fnSummary = "THUNK_2_0 grew 18x monotonically across censuses."
          , fnEvidence = [("peak", "18.4 MiB"), ("score", "0.94")]
          , fnRemediation = "Force accumulator strictly with $! or BangPatterns."
          , fnPrimaryRef = Just binding
          , fnSuggestedQuery = Just "bands | thunk | top 10"
          , fnSteps = []
          }
  triIns <- newInspector
  let triInp = withInput 900 600
      triScene =
        void $ columnWith (fixedWH 900 600) $
          inspectorPaneWithTriage (worldEnv w PHeap) triIns [sampleFinding] (\q -> liftIO (writeIORef queriedRef q)) floatingPane
  warmup2 triCtx triInp triScene >>= \_ -> pure ()
  triSpans <- collectTextSpans triCtx
  check "Unfocused Inspector renders ranked Triage findings feed"
    (hasText "Start here: Top findings (1)" triSpans && hasText "Unforced accumulator thunk in Main.step" triSpans)
  explorePos <- findSpan "Explore query" triSpans
  let (epPress, epRel) = clickPair triInp explorePos
  mapM_ (\i -> evalUi triCtx i triScene) [epPress, epRel, triInp]
  check "Clicking 'Explore query' on a Triage finding invokes the query runner" . (== "bands | thunk | top 10") =<< readIORef queriedRef

  qCtxUi <- newContext
  writeVarIO (wlQueryText wlSt) "bands | thunk | top 10"
  let qHp = fromSamples "prog" "closure type" 0 (const Nothing) [(1.0, [("THUNK_2_0", 2048)]), (2.0, [("THUNK_2_0", 16384)])]
      qData = emptyQueryContext {qcHeap = Just qHp}
      qInp = withInput 900 600
      qScene = void $ columnWith (fixedWH 900 600) $ queryPane (worldEnv w PHeap) wlSt qData floatingPane
  warmup2 qCtxUi qInp qScene >>= \_ -> pure ()
  qSpans <- collectTextSpans qCtxUi
  check "Lens query pane renders preset chips, removable stage pills, and matched rows"
    (hasText "Lens query" qSpans && hasText "thunk ×" qSpans && hasText "THUNK_2_0" qSpans)
  pillPos <- findSpan "thunk ×" qSpans
  let (ppPress, ppRel) = clickPair qInp pillPos
  mapM_ (\i -> evalUi qCtxUi i qScene) [ppPress, ppRel, qInp]
  qAfter <- readVarIO (wlQueryText wlSt)
  check "Clicking a stage pill removes that filter stage from query text" (not ("thunk" `T.isInfixOf` qAfter) && "bands" `T.isPrefixOf` qAfter)

  labCtx <- newContext
  let sampleTargets = [CabalTarget "stronghead" "stronghead-sample" ExeTarget "stronghead.cabal"]
      labScene =
        void $ columnWith (fixedWH 950 750) $
          labPane (worldEnv w PTimeline) wlSt sampleTargets Nothing (Just qHp) Nothing (\_ _ -> pure ()) floatingPane
  warmup2 labCtx (withInput 950 750) labScene >>= \_ -> pure ()
  labSpans <- collectTextSpans labCtx
  check "Runner & lab pane renders target recipes, Pareto sweep controls, and live budget badge"
    (hasText "Target runner & capture recipes" labSpans && hasText "Space-Leak Provenance (-hi)" labSpans && hasText "RTS flag Pareto tuning lab" labSpans)

  putStrLn "nano-ui ergonomics: ItemsAlign centering, buttonGroup, and useListNav"
  let midY lbl spans = [ry + rh / 2 | (Rect _ ry _ rh, t, _, _, _) <- spans, t == lbl]
  case (midY "CRITICAL" triSpans, midY "94.0% conf" triSpans) of
    (y1 : _, y2 : _) -> check "itemsCenter vertically centers badge and label in Triage card" (abs (y1 - y2) <= 1.5)
    _ -> check "itemsCenter vertically centers badge and label in Triage card" False
  case (midY "Mark again" diffSpans, midY "Clear" diffSpans) of
    (y1 : _, y2 : _) -> check "buttonGroup aligns connected header buttons on identical midline" (abs (y1 - y2) <= 1.0)
    _ -> check "buttonGroup aligns connected header buttons on identical midline" False

  navCtx <- newContext
  navCst <- newCoreState
  navPopped <- newVar []
  let navBench = csPanes navCst
      navScene = void $ columnWith (fixedWH 1300 1000) $ do
        need <- maybe [] treeKinds <$> readVar (wbTree navBench)
        draw <- corePanes (worldEnv w PCore) navCst cloaded need
        workspace [BenchPlace "Core & STG" navBench] navPopped navBench draw
      navFrame i = evalUi navCtx i navScene
  warmup2 navCtx cinp navScene >>= \_ -> pure ()
  navSpans0 <- collectTextSpans navCtx
  navGoPos <- findSpan "go" navSpans0
  let (npPress, npRel) = clickPair cinp navGoPos
  mapM_ navFrame [npPress, npRel, cinp]
  check "clicking binding selects 'go' and focuses Bindings scroller" . (== Just "go") =<< readVarIO (csSelected navCst)
  _ <- navFrame (keyInp KeyDown cinp)
  _ <- navFrame cinp
  check "useListNav KeyDown advances focused Bindings list from 'go' to 'main'" . (== Just "main") =<< readVarIO (csSelected navCst)

  n <- readIORef failures
  if n == 0 then putStrLn "all passed" else putStrLn (show n <> " failed") >> exitFailure
