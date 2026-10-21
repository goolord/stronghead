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
import Data.Maybe (fromMaybe, isNothing)
import System.Environment (lookupEnv)
import System.IO (stderr)
import Text.Read (readMaybe)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), askSdlEnv, defaultSdlOptions, runSdlApp, saveScreenshot)
import Options.Applicative qualified as Opt

import UI.Common (strongheadTheme)
import UI.Settings
import UI.Shell

main :: IO ()
main = do
  capture <- lookupEnv "STRONGHEAD_CAPTURE"
  opts <- Opt.execParser (optionsInfo (isNothing capture))
  settings <- loadSettings
  app <- newApp settings opts
  delay <- maybe 3 (fromMaybe 3 . readMaybe) <$> lookupEnv "STRONGHEAD_CAPTURE_DELAY"
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
  -- A screenshot run leaves the user's settings and last session as they
  -- were.
  when (isNothing capture) do
    saveAppSettings app >>= either (T.hPutStrLn stderr) pure
    saveLastSession app >>= either (T.hPutStrLn stderr) pure

optionsInfo :: Bool -> Opt.ParserInfo Options
optionsInfo persist =
  Opt.info
    (optionsParser persist Opt.<**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc "A live debugger and profiler GUI for GHC programs")

optionsParser :: Bool -> Opt.Parser Options
optionsParser persist =
  Options
    <$> Opt.many (Opt.strArgument (Opt.metavar "FILE|DIR..."))
    <*> Opt.optional (T.toLower <$> Opt.strOption (Opt.long "tab" <> Opt.metavar "NAME" <> Opt.help "Starting panel (timeline, heap, core, debugger, live)"))
    <*> Opt.optional (parseConnect <$> Opt.strOption (Opt.long "connect" <> Opt.metavar "HOST:PORT" <> Opt.help "Connect Live panel to an agent"))
    <*> Opt.optional (Opt.strOption (Opt.long "follow" <> Opt.metavar "FILE.eventlog" <> Opt.help "Follow an eventlog as it is written"))
    <*> Opt.optional (Opt.strOption (Opt.long "debug" <> Opt.metavar "MODULE.hs" <> Opt.help "Start a GHCi session on a module"))
    <*> Opt.optional (Opt.strOption (Opt.long "goto" <> Opt.metavar "REF" <> Opt.help "Open an entity by its reference"))
    <*> Opt.optional (Opt.strOption (Opt.long "mark" <> Opt.metavar "TEXT" <> Opt.help "Send a mark to the connected program"))
    <*> pure persist
  where
    parseConnect :: Text -> (Text, Text)
    parseConnect addr =
      let (host, port) = T.breakOnEnd ":" addr
       in if T.null host then ("127.0.0.1", port) else (T.dropEnd 1 host, port)

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
