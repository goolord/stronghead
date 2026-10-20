-- | The links between panels ("UI.Link"), driven headlessly through
-- nano-ui's test harness: clicks, Ctrl+clicks, right-click menus, peeks and
-- history, against stub lenses that record what they were asked to do.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void, when)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (evalUi, withInput)
import NanoUI.Testing.Harness
import Stronghead.Ref
import System.Exit (exitFailure)
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

  n <- readIORef failures
  if n == 0 then putStrLn "all passed" else putStrLn (show n <> " failed") >> exitFailure
