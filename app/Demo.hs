-- | The demo video: a scripted tour of the real UI, recorded headlessly.
--
-- 'demoMain' spawns the real @stronghead-sample@ (agent, eventlog, heap
-- profile and all), runs the real application view on a hidden window, and
-- drives it with eased pointer moves, clicks, drags and typing, saving every
-- frame as a BMP with the pointer drawn in. Frame durations come from the
-- real clock, so the recording plays at true speed: the program is followed
-- live, for real. @DIR@ also gets @frames.txt@, an ffmpeg concat list.
--
-- Run from the repository root:
--
-- > cabal run -v0 stronghead -- --demo scratch/demo-frames
-- > ffmpeg -y -f concat -safe 0 -i scratch/demo-frames/frames.txt \
-- >   -vsync vfr -pix_fmt yuv420p -c:v libx264 -crf 18 stronghead-demo.mp4
module Demo (demoMain) where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( SomeException (..)
  , Exception
  , catch
  , displayException
  , finally
  , fromException
  , handle
  , throwIO
  )
import Control.Monad (forM_, replicateM_, unless, void, when)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Clock (getMonotonicTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , removeFile
  )
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode, exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (</>))
import System.IO
  ( IOMode (..)
  , hClose
  , hPutBuf
  , hPutStrLn
  , openFile
  , stderr
  , withBinaryFile
  )
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , createProcess
  , proc
  , terminateProcess
  )
import Text.Printf (printf)

import NanoUI
import NanoUI.Backend (applyMouseButton, emptyInput)
import NanoUI.Backend.Sdl
  ( NanoUIFont (..)
  , SdlEnv
  , SdlOptions (..)
  , defaultSdlOptions
  , saveScreenshot
  , sdlDrawFrame
  , syncDisplay
  , withSdl
  )
import NanoUI.Testing (Context, collectOverlayTextSpans, collectTextSpans, newPixelContext)
import NanoUI.Testing.Harness (DemoSpan, findExact, findRightmost, hasText, keyInp, spanCenter)

import UI.Common (strongheadTheme)
import UI.Settings (loadSettings)
import UI.Shell (App, appView, defaultOptions, newApp, optFiles, optPersist, optTab)

-- | Frames per second of the recording.
fps :: Double
fps = 30

-- | Every scripted dwell is scaled by this: the tour reads briskly on
-- screen. 1.15 is about a minute and a quarter.
tempo :: Double
tempo = 1.15

winW, winH :: Float
winW = 1500
winH = 960

-- | How the sample program is run: long enough that it never finishes during
-- the tour, streaming everything Stronghead can follow. Two capabilities and
-- a large nursery keep the eventlog (and the leak) growing at a sane rate.
-- 'STRONGHEAD_DEMO_SAMPLE_ARGS' overrides the arguments, space-split.
sampleArgs :: IO [String]
sampleArgs =
  maybe defaultArgs words <$> lookupEnv "STRONGHEAD_DEMO_SAMPLE_ARGS"
  where
    defaultArgs = ["360", "+RTS", "-l", "-hT", "-i0.1", "-T", "-N2", "-A64m", "-RTS"]

-- | A recording session: the paced frame loop and the pointer it draws.
data Rec = Rec
  { recDraw :: Input -> IO ()
  , recCtx :: Context
  , recEnv :: SdlEnv
  , recBase :: Input
  , recDir :: FilePath
  , recStart :: IORef Double
  , recFrame :: IORef Int
  , recLast :: IORef Double
  , recMouse :: IORef V2
  , recDown :: IORef Bool
  , recLog :: IORef [(V2, Bool, Double)]
  -- ^ Newest first: pointer, button held, time since the start.
  }

demoMain :: FilePath -> IO ()
demoMain dir = handle report $ do
  repo <- getCurrentDirectory
  sample <- findSample
  dumps <- demoDumps repo
  createDirectoryIfMissing True dir
  putStrLn ("demo: frames -> " <> dir)
  putStrLn ("demo: sample   " <> sample)
  putStrLn ("demo: dumps    " <> maybe "none" id dumps)
  settings <- loadSettings
  app <-
    newApp
      settings
      defaultOptions
        { optTab = Just "live"
        , optFiles = maybe [] pure dumps
        , optPersist = False
        }
  withSampleLogged dir $ \ph -> do
    record app repo dir
    terminateProcess ph
  -- Leave the tree as it was.
  removeFileQuiet (repo </> "stronghead-sample.eventlog")
  removeFileQuiet (repo </> "stronghead-sample.prof")
  exitSuccess

-- | Say why a recording stopped; exits pass through.
report :: SomeException -> IO ()
report e = case fromException e of
  Just (code :: ExitCode) -> throwIO code
  Nothing -> hPutStrLn stderr ("demo: FAILED: " <> displayException e) >> exitFailure

-- | The sample executable: next to us in the build tree, or where the
-- environment says.
findSample :: IO FilePath
findSample = do
  exe <- getExecutablePath
  let build = takeDirectory (takeDirectory exe) -- .../x/stronghead/build
      dist = takeDirectory (takeDirectory build) -- .../x
      cands0 =
        [ dist </> "stronghead-sample" </> "build" </> "stronghead-sample" </> "stronghead-sample"
        , takeDirectory exe </> "stronghead-sample"
        ]
      cands = cands0 >>= \p -> [p, p <> ".exe"]
  env <- lookupEnv "STRONGHEAD_DEMO_SAMPLE"
  let cands' = maybe cands (: cands) env
  ok <- mapM (\p -> doesFileExist p >>= \b -> pure (if b then [p] else [])) cands'
  case concat ok of
    p : _ -> pure p
    [] -> fail ("demo: no stronghead-sample next to " <> exe)

-- | Core and STG dumps recorded for the sample, if they were built.
demoDumps :: FilePath -> IO (Maybe FilePath)
demoDumps repo = do
  let dir = repo </> "scratch" </> "demo-assets"
  ok <- doesDirectoryExist dir
  if ok then pure (Just dir) else pure Nothing

-- | Run the sample with its output kept for debugging, on a clean eventlog.
-- It is always terminated on the way out, leak or no leak.
withSampleLogged :: FilePath -> (ProcessHandle -> IO a) -> IO a
withSampleLogged dir k = do
  repo <- getCurrentDirectory
  sample <- findSample
  args <- sampleArgs
  removeFileQuiet (repo </> "stronghead-sample.eventlog")
  h <- openFile (dir </> "sample.log") WriteMode
  (_, _, _, ph) <-
    createProcess
      (proc sample args)
        { cwd = Just repo
        }
  (k ph `finally` terminateProcess ph) `finally` hClose h

removeFileQuiet :: FilePath -> IO ()
removeFileQuiet p = removeFile p `catch` \(_ :: SomeException) -> pure ()

-- Frames ----------------------------------------------------------------------

-- | Draw and save one frame, paced to 'fps'. Durations come from the real
-- clock, so a slow frame stays on screen as long as it took to draw.
frame :: Rec -> (Input -> Input) -> IO ()
frame r f = do
  n <- readIORef (recFrame r)
  start <- readIORef (recStart r)
  now0 <- getMonotonicTime
  let due = start + fromIntegral n / fps
  when (now0 < due) $ threadDelay (round ((due - now0) * 1e6))
  t <- getMonotonicTime
  prev <- readIORef (recLast r)
  pos <- readIORef (recMouse r)
  down <- readIORef (recDown r)
  let held = buttonsFromList [MouseLeft]
      inp =
        f (recBase r)
          { inputMousePos = pos
          , inputButtonsHeld = if down then held else inputButtonsHeld (recBase r)
          , inputDeltaTime = realToFrac (max 1e-3 (t - prev))
          }
  recDraw r inp
  ok <- saveScreenshot (recEnv r) (recDir r </> frameName n)
  unless ok $ fail "demo: could not read back the frame"
  writeIORef (recLast r) t
  writeIORef (recFrame r) (n + 1)
  modifyIORef' (recLog r) ((pos, down, t) :)
  when (n `mod` 300 == 0) $ putStrLn ("demo: frame " <> show n)

frames :: Double -> Int
frames secs = max 1 (round (secs * fps * tempo))

wait :: Rec -> Double -> IO ()
wait r secs = replicateM_ (frames secs) (frame r id)

-- | Draw frames without saving them until @needle@ appears on screen, then
-- resume recording where the clock is now: the video skips the wait.
await :: Rec -> Text -> Double -> IO ()
await r needle secs = do
  t0 <- getMonotonicTime
  let go = do
        found <- hasText needle <$> spans r
        now <- getMonotonicTime
        if found
          then resync r
          else
            if now - t0 > secs
              then dumpVisible r >> fail ("demo: awaited " <> show needle)
              else do
                poll r
                threadDelay 40000
                go
  go

-- | Recorded frames until @needle@ appears (or the budget runs out).
awaitRec :: Rec -> Text -> Double -> IO ()
awaitRec r needle secs = go (frames secs)
  where
    go 0 = dumpVisible r >> fail ("demo: awaited (recorded) " <> show needle)
    go left = do
      frame r id
      found <- hasText needle <$> spans r
      unless found (go (left - 1))

-- | One unpaced, unsaved frame, so streams and tasks move while we wait.
poll :: Rec -> IO ()
poll r = do
  pos <- readIORef (recMouse r)
  now <- getMonotonicTime
  recDraw r (recBase r) {inputMousePos = pos, inputDeltaTime = 0.04}
  writeIORef (recLast r) now

-- | Put the pacing clock back in step with the wall clock.
resync :: Rec -> IO ()
resync r = do
  now <- getMonotonicTime
  n <- readIORef (recFrame r)
  start <- readIORef (recStart r)
  let due = start + fromIntegral n / fps
  writeIORef (recStart r) (start + (now + 1 / fps) - due)
  writeIORef (recLast r) now

frameName :: Int -> FilePath
frameName = printf "f%05d.bmp"

-- Gestures ---------------------------------------------------------------------

-- | Glide the pointer to @to@ over @secs@, easing in and out.
moveTo :: Rec -> V2 -> Double -> IO ()
moveTo r to secs = do
  from <- readIORef (recMouse r)
  let n = frames secs
  forM_ [1 .. n] $ \i -> do
    let s = ease (fromIntegral i / fromIntegral n)
    writeIORef (recMouse r) (lerpV from to s)
    frame r id

ease :: Float -> Float
ease s = if s < 0.5 then 4 * s * s * s else 1 - (-2 * s + 2) ** 3 / 2

lerpV :: V2 -> V2 -> Float -> V2
lerpV (V2 ax ay) (V2 bx by) s = V2 (ax + (bx - ax) * s) (ay + (by - ay) * s)

press, release :: Rec -> IO ()
press r = do
  writeIORef (recDown r) True
  frame r (\i -> i {inputButtonsPressed = buttonsFromList [MouseLeft]})
release r = do
  writeIORef (recDown r) False
  frame r (\i -> i {inputButtonsReleased = buttonsFromList [MouseLeft]})

click :: Rec -> IO ()
click r = press r >> frame r id >> release r >> wait r 0.2

rightClick :: Rec -> IO ()
rightClick r = do
  frame r (applyMouseButton MouseRight True)
  frame r (applyMouseButton MouseRight False)
  wait r 0.25

-- | Move to @pos@ and click there.
clickAt :: Rec -> V2 -> IO ()
clickAt r pos = moveTo r pos 0.35 >> wait r 0.1 >> click r

-- | Press where the pointer is with @mods@ held, drag to @to@ over @secs@,
-- release. The modifier is held from the press on.
dragTo :: Rec -> (Input -> Input) -> V2 -> Double -> IO ()
dragTo r mods to secs = do
  writeIORef (recDown r) True
  frame r (mods . (\i -> i {inputButtonsPressed = buttonsFromList [MouseLeft]}))
  replicateM_ 2 (frame r mods)
  from <- readIORef (recMouse r)
  let n = frames secs
  forM_ [1 .. n] $ \i -> do
    let s = ease (fromIntegral i / fromIntegral n)
    writeIORef (recMouse r) (lerpV from to s)
    frame r mods
  replicateM_ 2 (frame r mods)
  writeIORef (recDown r) False
  frame r (mods . (\i -> i {inputButtonsReleased = buttonsFromList [MouseLeft]}))
  wait r 0.2

shift :: Input -> Input
shift i = i {inputModifiers = (inputModifiers i) {modShift = True}}

typeText :: Rec -> Text -> IO ()
typeText r txt = forM_ (T.unpack txt) $ \c -> do
  frame r (\i -> i {inputChars = T.singleton c})
  frame r id

key :: Rec -> Key -> IO ()
key r k = frame r (keyInp k) >> frame r id

-- | Double-click: press and release carrying two clicks.
dblClick :: Rec -> IO ()
dblClick r = do
  writeIORef (recDown r) True
  frame r (\i -> i {inputButtonsPressed = buttonsFromList [MouseLeft], inputMouseClicks = 2})
  writeIORef (recDown r) False
  frame r (\i -> i {inputButtonsReleased = buttonsFromList [MouseLeft], inputMouseClicks = 2})
  wait r 0.3

dblClickAt :: Rec -> V2 -> IO ()
dblClickAt r pos = moveTo r pos 0.35 >> wait r 0.1 >> dblClick r

wheel :: Rec -> Float -> IO ()
wheel r notches = do
  replicateM_ (round (abs notches)) $
    frame r (\i -> i {inputScroll = V2 0 (signum notches)}) >> frame r id
  wait r 0.4

-- Finding widgets ---------------------------------------------------------------

spans :: Rec -> IO [DemoSpan]
spans r = do
  base <- collectTextSpans (recCtx r)
  over <- collectOverlayTextSpans (recCtx r) (recBase r)
  pure (base ++ over)

-- | The centre of the lowest span starting with @needle@ (menus sit below).
findPrefix :: Rec -> Text -> IO V2
findPrefix r needle = do
  ss <- spans r
  case sortOn (\(r', _, _, _, _) -> negate (rectY' r')) [s | s@(_, txt, _, _, _) <- ss, needle `T.isPrefixOf` txt] of
    ((r', _, _, _, _) : _) -> pure (spanCenter r')
    [] -> dumpVisible r >> fail ("demo: no span starting " <> show needle)
  where
    rectY' (Rect _ y _ _) = y

exact, rightmost :: Rec -> Text -> IO V2
exact r = findVia r findExact
rightmost r = findVia r findRightmost

findVia :: Rec -> (Text -> [DemoSpan] -> Maybe V2) -> Text -> IO V2
findVia r finder needle = do
  ss <- spans r
  case finder needle ss of
    Just p -> pure p
    Nothing -> dumpVisible r >> fail ("demo: no span " <> show needle)

off :: V2 -> Float -> Float -> V2
off (V2 x y) dx dy = V2 (x + dx) (y + dy)

dumpVisible :: Rec -> IO ()
dumpVisible r = do
  ss <- spans r
  hPutStrLn stderr ("demo: visible text: " <> show [txt | (_, txt, _, _, _) <- take 100 ss])

-- | Try a beat of the tour; a miss skips it rather than losing the recording.
softly :: String -> IO () -> IO ()
softly what act = act `catch` \(e :: SomeException) -> case fromException e of
  Just (code :: ExitCode) -> throwIO code
  Nothing -> hPutStrLn stderr ("demo: skipped " <> what <> ": " ++ take 200 (displayException e))

-- The recording -----------------------------------------------------------------

record :: App -> FilePath -> FilePath -> IO ()
record app repo dir = do
  ctx0 <- newPixelContext >>= \c -> withTheme c (strongheadTheme (Just AppearanceDark))
  let opts =
        defaultSdlOptions
          { sdlWindowSettings =
              defaultWindowSettings
                { wsMode = Hidden
                , wsSize = Size winW winH
                , wsResizable = False
                }
          , sdlAppFontSize = 15
          , sdlAppMonoFont =
              FontSearch ["JetBrains Mono", "Cascadia Mono", "Source Code Pro", "Consolas", "DejaVu Sans Mono", "monospace"]
          }
      view = appView app
  withSdl opts ctx0 $ \ctx env -> do
    let base0 = emptyInput {inputWindowSize = Size winW winH, inputMousePos = V2 (-10) (-10)}
    (ctx', base) <- syncDisplay ctx env base0
    -- Warm up: the first frames lay everything out and load glyphs.
    replicateM_ 3 (void (sdlDrawFrame ctx' view env base True))
    t0 <- getMonotonicTime
    r <-
      Rec (\inp -> void (sdlDrawFrame ctx' view env inp False)) ctx' env base dir
        <$> newIORef t0
        <*> newIORef 0
        <*> newIORef t0
        <*> newIORef (V2 (winW * 0.72) (winH * 0.55))
        <*> newIORef False
        <*> newIORef []
    tour r repo `catch` \DemoStop -> pure ()
    entries <- reverse <$> readIORef (recLog r)
    -- The video plays at a constant fps: each saved frame lasts 1/fps, so
    -- slow frames do not stretch the tour.
    let durs = replicate (length entries) (1 / fps)
    writeFile (dir </> "frames.txt") $
      concat [printf "file '%s'\nduration %.4f\n" (frameName i) d | (i, d) <- zip [0 :: Int ..] durs]
        ++ printf "file '%s'\n" (frameName (length entries - 1))
    let total = sum durs
    printf "demo: %d frames, %.1f s; drawing the pointer\n" (length entries) total
    forM_ (zip3 [0 :: Int ..] entries durs) $ \(i, (pos, down, _), _) ->
      drawPointer (dir </> frameName i) pos down

-- | The tour cut short on purpose ('STRONGHEAD_DEMO_LIVE_ONLY').
data DemoStop = DemoStop
  deriving stock (Show)

instance Exception DemoStop

-- The tour ----------------------------------------------------------------------

tour :: Rec -> FilePath -> IO ()
tour r repo = do
  step "the program, live"
  wait r 0.9

  -- Connect: the agent says hello, and Connect finds everything else.
  clickAt r =<< exact r "Connect"
  await r "capabilities" 20
  await r "Connect found" 10
  wait r 1.4

  -- Remote commands: a mark, a collection.
  clickAt r =<< exact r "Mark…"
  await r "Mark the program's timeline" 3
  typeText r "watch the leak"
  key r KeyEnter
  wait r 0.5
  clickAt r =<< exact r "Major GC"
  wait r 0.9

  -- Every thread, live from the agent: filter one out, peek at it, pin it.
  softly "thread filter and peek" $ do
    lf <- findPrefix r "Filter by label or id"
    clickAt r lf
    typeText r "work"
    wait r 0.5
    w3 <- exact r "worker-3"
    moveTo r w3 0.3
    wait r 0.9
    pinThread r

  -- A cut-short run: just the live phase (a reproducer for the follower).
  liveOnly <- lookupEnv "STRONGHEAD_DEMO_LIVE_ONLY"
  when (liveOnly == Just "1") $ do
    wait r 8
    throwIO DemoStop

  -- The timeline, followed as it is written.
  step "timeline"
  clickAt r =<< exact r "Timeline"
  wait r 1.0
  caps <- exact r "Capabilities"
  moveTo r (off caps 420 210) 0.4
  wheel r (-4)
  wait r 0.6
  wheel r 6
  wait r 0.3
  softly "range" $ do
    moveTo r (off caps 260 200) 0.3
    dragTo r shift (off caps 700 200) 0.7
    wait r 0.4
    clickAt r =<< exact r "Analyse range"
    wait r 1.1
    clickAt r =<< exact r "Back to overview"
    wait r 0.3

  -- A pane can take the whole workbench: double-click its header.
  softly "maximise the threads pane" $ do
    tlTitle <- exact r "Threads"
    dblClickAt r tlTitle
    wait r 0.9
    tlTitle2 <- exact r "Threads"
    dblClickAt r tlTitle2
    wait r 0.3

  -- The heap, live: the bands the program is leaking.
  step "heap"
  clickAt r =<< exact r "Heap & leaks"
  wait r 0.9
  clickAt r =<< exact r "Leak suspects"
  wait r 0.9
  softly "solo the thunks" $ do
    m <- rightmost r "THUNK_2_0"
    moveTo r m 0.3
    click r
    wait r 0.6
    clickAt r =<< exact r "Unsolo THUNK_2_0"
    wait r 0.4

  -- A baseline, then the diff against it.
  softly "baseline" $ do
    clickAt r =<< exact r "Go to…"
    await r "Go to anything" 3
    typeText r "baseline"
    wait r 0.3
    clickAt r =<< exact r "Mark as baseline"
    wait r 0.4
    clickAt r =<< exact r "Go to…"
    typeText r "heap diff"
    clickAt r =<< exact r "Add pane: Heap diff"
    wait r 1.1

  -- Core and STG, against the run's own numbers. The dumps cover both the
  -- agent library and the sample; switch to the sample's module.
  step "core"
  clickAt r =<< exact r "Core & STG"
  wait r 0.8
  softly "switch to Main" $ do
    clickAt r =<< exact r "Agent"
    clickAt r =<< exact r "Main"
    wait r 0.4
  softly "open a binding" $ do
    m <- findPrefix r "main"
    clickAt r m
    wait r 0.7
  -- The code tabs flash on update; switch through Core and STG to inspect them.
  softly "code tabs" $ do
    clickAt r =<< exact r "Core"
    wait r 0.7
    clickAt r =<< exact r "STG"
    wait r 0.7
    clickAt r =<< exact r "Source"
    wait r 0.5
  -- The inspector follows the focus: jump to its source, then set a
  -- breakpoint from here, on the binding itself.
  softly "go to its source" $ do
    g <- exact r "Go to"
    clickAt r g
    wait r 0.6
  softly "break on entry" $ do
    b <- findPrefix r "Break on entry"
    clickAt r b
    wait r 0.4
  softly "boxing colours" $ do
    clickAt r =<< exact r "Boxing"
    wait r 0.6

  -- The GHCi debugger, on the same program.
  step "debugger"
  clickAt r =<< exact r "Debugger"
  wait r 0.5
  ghciField <- findPrefix r "ghci"
  clickAt r ghciField
  key r KeyEnd
  replicateM_ 8 (key r KeyBackspace)
  typeText r "cabal repl stronghead:stronghead-sample"
  wdField <- findPrefix r "working directory"
  clickAt r wdField
  key r KeyEnd
  replicateM_ 8 (key r KeyBackspace)
  typeText r (T.pack repo)
  loadField <- findPrefix r "src/Main.hs"
  clickAt r loadField
  key r KeyEnd
  replicateM_ 8 (key r KeyBackspace)
  typeText r "sample/Main.hs"
  clickAt r =<< exact r "Start session"
  await r "GHCi session running" 150
  wait r 0.4
  -- The expression is already 'main'; Step into runs it and stops at once.
  softly "step into main" $ do
    clickAt r =<< exact r "Step into"
    awaitRec r "Stopped in" 45
    wait r 0.6
  softly "force a thunk" $ do
    f <- exact r "Force"
    clickAt r f
    wait r 0.7
  softly "step" $ do
    clickAt r =<< exact r "Step over"
    wait r 0.6
  softly "abandon" $ do
    clickAt r =<< exact r "Abandon"
    wait r 0.4

  -- Back to the program: watchers on the live stream, the pinboard.
  step "watchers"
  clickAt r =<< exact r "Live"
  wait r 0.5
  softly "add watchers" $ do
    clickAt r =<< exact r "Go to…"
    typeText r "watchers"
    clickAt r =<< exact r "Add pane: Watchers"
    wait r 0.6
  softly "enable a watcher" $ do
    rule <- findPrefix r "A heap band growing"
    clickAt r rule
    awaitRec r "Watchers:" 12
    wait r 0.6
  softly "pinboard" $ do
    clickAt r =<< exact r "Go to…"
    typeText r "pinboard"
    clickAt r =<< exact r "Add pane: Pinboard"
    wait r 0.5
    note <- findPrefix r "Note"
    clickAt r note
    typeText r "the MVar queue"
    wait r 0.4

  -- Arrangements, and out on the leak hunt.
  step "workspace"
  softly "leak hunt" $ do
    clickAt r =<< exact r "Workspace"
    wait r 0.7
    clickAt r =<< exact r "Leak hunt"
    wait r 0.4
  clickAt r =<< exact r "Heap & leaks"
  wait r 1.2
  wait r 1.5
  where
    step name = putStrLn ("demo: " <> name)

-- | Right-click the filtered thread and pin it.
pinThread :: Rec -> IO ()
pinThread r = do
  w3 <- exact r "worker-3"
  moveTo r w3 0.3
  rightClick r
  clickAt r =<< exact r "Pin"
  wait r 0.4

-- The pointer -------------------------------------------------------------------

-- | The classic arrow: B outline, W fill.
arrow :: [String]
arrow =
  [ "B"
  , "BB"
  , "BWB"
  , "BWWB"
  , "BWWWB"
  , "BWWWWB"
  , "BWWWWWB"
  , "BWWWWWWB"
  , "BWWWWWWWB"
  , "BWWWWWWWWB"
  , "BWWWWWWWWWB"
  , "BWWWWWWBBBBB"
  , "BWWWBWWB"
  , "BWWBBWWB"
  , "BWB  BWWB"
  , "BB   BWWB"
  , "B     BWWB"
  , "      BWWB"
  , "       BB"
  ]

-- | Draw the pointer into a saved BMP (uncompressed, 24 or 32 bits a pixel).
drawPointer :: FilePath -> V2 -> Bool -> IO ()
drawPointer path (V2 px py) down = do
  bs <- BS.readFile path
  let u32 o = foldr (\k acc -> acc `shiftL` 8 .|. fromIntegral (BS.index bs (o + k))) 0 [0 .. 3] :: Int
      offset = u32 10
      width = u32 18
      height0 = u32 22
      height = if height0 >= 0x80000000 then 0x100000000 - height0 else height0
      bottomUp = height0 < 0x80000000
      bytesPer = u32 28 `mod` 0x10000 `div` 8
      stride = (width * bytesPer + 3) `div` 4 * 4
      len = BS.length bs
  BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
    allocaBytes len $ \buf -> do
      copyBytes buf (castPtr src) len
      let put :: Int -> Int -> Word8 -> IO ()
          put x y v =
            when (x >= 0 && y >= 0 && x < width && y < height) $ do
              let row = if bottomUp then height - 1 - y else y
                  base = offset + row * stride + x * bytesPer
              forM_ [0 .. 2] $ \c -> pokeByteOff (buf :: Ptr Word8) (base + c) v
          x0 = round px :: Int
          y0 = round py :: Int
          ring x y =
            let d = sqrt (fromIntegral ((x - x0) ^ (2 :: Int) + (y - y0) ^ (2 :: Int))) :: Float
             in down && d >= 11 && d <= 14
      forM_ (zip [0 ..] arrow) $ \(dy, line) ->
        forM_ (zip [0 ..] line) $ \(dx, ch) -> case ch of
          'B' -> put (x0 + dx) (y0 + dy) 0
          'W' -> put (x0 + dx) (y0 + dy) 255
          _ -> pure ()
      -- A held button leaves a ring around the tip.
      when down $
        forM_ [x0 - 14 .. x0 + 14] $ \x ->
          forM_ [y0 - 14 .. y0 + 14] $ \y ->
            when (ring x y) $ put x y 96
      withBinaryFile path WriteMode $ \hdl -> hPutBuf hdl (buf `plusPtr` 0) len
