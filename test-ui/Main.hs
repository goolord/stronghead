-- | The links between panels ("UI.Link"), driven headlessly through
-- nano-ui's test harness: clicks, Ctrl+clicks, right-click menus, peeks and
-- history, against stub lenses that record what they were asked to do;
-- the shared clock ("UI.Clock"): the cursor and range a time view sets on
-- the bus, and a snapping cursor kept in step with them; and the
-- annotated code view ("UI.Code"), whose heat bars are links; and the
-- Live panel's remote actions ("UI.Live"): a command lands on the clock at
-- once, the agent's reply stamps it, the eventlog's marker takes over;
-- the entity inspector ("UI.Inspector") and the pinboard ("UI.Pinboard");
-- and the pane registry ("UI.Workspace"): trees, presets, and moving a
-- pane through its menu.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Data.Foldable (for_)
import Data.IORef
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (evalUi, withInput)
import NanoUI.Testing.Harness
import Stronghead.Dump (CodeKind (..), tokenizeLine)
import Stronghead.Ghci (Value (..))
import Stronghead.Eventlog.Model (Marker (..), Timeline (..), emptyTimeline)
import Stronghead.Join (Annotation (..), Lane (..), Shade (..), annotationAt)
import Stronghead.Live
import Stronghead.Next (Fact (..), Have (..), Step (..), StepDo (..), haveNothing, strongStep)
import Stronghead.Ref
import Stronghead.Run (RunInfo (..), emptyRunInfo, runFromHello, runKey)
import Stronghead.Session (Pin (..))
import Stronghead.Source (SrcSpan (..))
import System.Exit (exitFailure)
import UI.Clock
import UI.Code
import UI.Common (TimeView (..), Workbench (..), floatingPane, monoMetrics, newWorkbench, pane)
import UI.Debugger (DebugState (..), newDebugState, valueTree)
import UI.Inspector
import UI.Link
import UI.Live
import UI.Palette
import UI.Pinboard
import UI.Sources
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
  resp <- canvasConfigured defaultCanvasConfig {canvasLayout = fixedWH 400 100 defaultLayout, canvasTrackPointer = True} (\_ -> pure ())
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
      src = ["module M where", "go = 1 + 2", ""]
      ann = (annotationAt (SrcSpan "M.hs" 2 6 2 10) LaneHeap 2048 "2 KiB live" (Just band)) {anShade = ShadeThunk}
      cfg =
        withNotes
          (placedNotes [(1, 1, ann)])
          defaultCodeConfig
            { ccLineCount = length src
            , ccTokens = \l -> fst (tokenizeLine KindSource False (src !! l))
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
  codeClick inp0 bar
  check "clicking a heat bar goes to its entity" . (== ["heap band/0x4fe0"]) =<< logged
  check "in its panel" . (== PHeap) =<< tab
  writeVarIO (wTab w) PCore
  let (bDown, bUp) = rightClickPair inp0 bar
  mapM_ codeFrame [bDown, bUp, inp0, inp0]
  barMenu <- collectOverlayTextSpans codeCtx inp0
  check "right-clicking a heat bar opens its entity's menu" (hasText "thunk in go" barMenu && hasText "Show in Heap & leaks" barMenu)

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
      d0 = LiveData (Just ("prog", 1, 4)) (Just info) (Just box) (Seq.fromList [frame0]) M.empty Nothing
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
  check "pane kinds are unique" (length (nub (map pkId paneKinds)) == length paneKinds && length (nub (map pkKey paneKinds)) == length paneKinds)
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
  let places = [BenchPlace "Bench A" benchA, BenchPlace "Bench B" benchB]
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
  for_ (take 1 menus) $ \(Rect x y bw bh) -> mclick (V2 (x + bw / 2) (y + bh / 2))
  opened <- collectOverlayTextSpans mctx iinp
  check "the menu moves, pops out and closes" (hasText "Move to Bench B" opened && hasText "Pop out" opened && hasText "Close pane" opened && not (hasText "Move to Bench A" opened))
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

  n <- readIORef failures
  if n == 0 then putStrLn "all passed" else putStrLn (show n <> " failed") >> exitFailure
