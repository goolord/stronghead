-- | Stronghead: a live debugger and profiler for GHC programs.
--
-- > stronghead [--tab NAME] [--connect HOST:PORT] [--follow FILE.eventlog]
-- >            [--debug MODULE.hs] [--goto REF] [--mark TEXT] [FILE|DIR ...]
--
-- Files are routed by extension: .eventlog to the timeline, .hp and .prof
-- to the heap explorer, .dump-simpl / .dump-stg-final / .hs to the Core
-- viewer; a directory is searched for dumps.
--
-- @--tab NAME@ (timeline, heap, core, debugger, live) picks the starting
-- panel, @--connect@ connects the Live panel to a program's agent, and
-- @--follow@ tails an eventlog the program is still writing, and @--debug@
-- starts a GHCi session on a module. @--goto@ opens an entity by its
-- reference, as "Copy reference" gives it (@source/sample%2FMain.hs/82/1/82/1@),
-- once a panel has loaded it. @--mark@ sends the connected program a mark
-- once its agent has streamed a couple of seconds (for scripts and
-- screenshots). Preferences (theme, interface scale,
-- window geometry, the panels' defaults) are loaded from and saved to the
-- settings file ("UI.Settings"). For development, @STRONGHEAD_CAPTURE=out.bmp@ saves a screenshot
-- once the window has been up for @STRONGHEAD_CAPTURE_DELAY@ seconds
-- (default 3) and exits.
module Main (main) where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), askSdlEnv, defaultSdlOptions, runSdlApp, saveScreenshot)
import System.Environment (getArgs, lookupEnv)
import System.IO (stderr)
import Text.Read (readMaybe)
import UI.Common (strongheadTheme)
import UI.Settings
import UI.Shell

main :: IO ()
main = do
  args <- getArgs
  settings <- loadSettings
  app <- newApp settings (parseArgs args defaultOptions)
  capture <- lookupEnv "STRONGHEAD_CAPTURE"
  delay <- maybe 3 (\s -> maybe 3 id (readMaybe s)) <$> lookupEnv "STRONGHEAD_CAPTURE_DELAY"
  started <- newVar (Nothing :: Maybe Double)
  let window = if cfgRestoreWindow settings then cfgWindow settings else Nothing
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings
            { wsTitle = "Stronghead"
            , wsSize = maybe (Size 1500 960) swSize window
            , wsPosition = maybe WindowPositionDefault (uncurry WindowPositionAt) (window >>= swPosition)
            , wsMinSize = Just (Size 960 640)
            }
      , sdlAppThemeFor = Just (strongheadTheme . choiceAppearance (cfgTheme settings))
      , sdlAppUiScale = uiScaleFactor (cfgScale settings)
      , sdlAppFontSize = 15
      , sdlAppMonoFont = FontSearch ["JetBrains Mono", "Cascadia Mono", "Source Code Pro", "Consolas", "DejaVu Sans Mono", "monospace"]
      }
    (appView app >> maybe (pure ()) (captureAfter started delay) capture)
  -- A screenshot run leaves the user's settings as they were.
  when (isNothing capture) $
    saveAppSettings app >>= either (T.hPutStrLn stderr) pure

parseArgs :: [String] -> Options -> Options
parseArgs args o = case args of
  "--tab" : name : rest -> parseArgs rest o {optTab = Just (T.toLower (T.pack name))}
  "--connect" : addr : rest ->
    let (host, port) = T.breakOnEnd ":" (T.pack addr)
     in parseArgs rest o {optConnect = Just (if T.null host then ("127.0.0.1", port) else (T.dropEnd 1 host, port))}
  "--follow" : f : rest -> parseArgs rest o {optFollow = Just f}
  "--debug" : f : rest -> parseArgs rest o {optDebug = Just f}
  "--goto" : r : rest -> parseArgs rest o {optGoto = Just (T.pack r)}
  "--mark" : m : rest -> parseArgs rest o {optMark = Just (T.pack m)}
  f : rest -> parseArgs rest o {optFiles = optFiles o ++ [f]}
  [] -> o

-- | Save a screenshot once the window has been up for a while, then quit.
captureAfter :: Var (Maybe Double) -> Double -> FilePath -> NanoUI ()
captureAfter started delay path = do
  now <- uiTime
  t0 <- readVar started
  case t0 of
    Nothing -> writeVar started (Just now)
    Just s -> do
      wakeAfter 0.1
      when (now - s >= delay) $
        askSdlEnv >>= \case
          Just env -> liftIO (saveScreenshot env path) >> quitUi
          Nothing -> quitUi
