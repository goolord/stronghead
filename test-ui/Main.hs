-- | The links between panels ("UI.Link"), driven headlessly through
-- nano-ui's test harness: clicks, Ctrl+clicks, right-click menus, peeks and
-- history, against stub lenses that record what they were asked to do; and
-- the shared clock ("UI.Clock"): the cursor and range a time view sets on
-- the bus, and a snapping cursor kept in step with them.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Data.Foldable (for_)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (evalUi, withInput)
import NanoUI.Testing.Harness
import Stronghead.Ref
import System.Exit (exitFailure)
import UI.Clock
import UI.Common (TimeView (..))
import UI.Link
import UI.Palette

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
          , lensPeek = \r -> if r == thread then Just (void (label "ran 41%")) else Nothing
          , lensHits = [Hit "thread" "worker-7" "ran 41%" (HitRef thread)]
          }
      live = (lens PLive) {lensShows = (== thread), lensReveal = \_ -> record "live"}
      core = (lens PCore) {lensShows = (== binding), lensReveal = \_ -> record "core go"}
      debugger =
        (lens PDebugger)
          { lensActions = \r ->
              if r == thread then [Action "Do thing" (Right (record "acted")), Action "Disabled thing" (Left "no data")] else []
          }
      env = Env (wBus w) [timeline, live, core, debugger] tab (writeVar (wTab w))
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
      env = Env (cBus c) [timeline] PHeap (\_ -> pure ())
  go <- liftIO (atomicModifyIORef' (cGo c) (Nothing,))
  for_ go (goTo env)
  resp <- canvasConfigured defaultCanvasConfig {canvasLayout = fixedWH 400 100 defaultLayout, canvasTrackPointer = True} (\_ -> pure ())
  let r = respRect resp
  liftIO (writeIORef (cRect c) r)
  _ <- timeGestures env (linearAxis run (TimeView 0 1e9) (rectX r) (rectW r)) resp ScrubDrag
  tether (cBus c) (cTether c) run (cCensus c) (\t -> Just (round (fromIntegral t / 1e8 :: Double))) (\i -> Just (fromIntegral i * 100000000))
  (dir, _) <- stepKeys
  when (dir /= 0) (liftIO (modifyIORef' (cSteps c) (dir :)))

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
  check "the peek shows each panel's facts" (hasText "ran 41%" tip && hasText "Timeline" tip)
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

  n <- readIORef failures
  if n == 0 then putStrLn "all passed" else putStrLn (show n <> " failed") >> exitFailure
