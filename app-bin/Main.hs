-- | Stronghead entry point and CLI parser.
--
-- > stronghead [--tab NAME] [--connect HOST:PORT] [--follow FILE.eventlog]
-- >            [--debug MODULE.hs] [--goto REF] [--mark TEXT] [FILE|DIR ...]
module Main (main) where

import System.Environment (getArgs)
import System.IO (stderr)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), defaultSdlOptions, runSdlApp)
import Options.Applicative qualified as Opt

import Demo (demoMain)
import UI.Common (strongheadTheme)
import UI.Settings
import UI.Shell

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- The demo video: a scripted tour recorded against the real sample.
    "--demo" : dir : _ -> demoMain dir
    _ -> mainWindowed

mainWindowed :: IO ()
mainWindowed = do
  opts <- Opt.execParser (optionsInfo True)
  settings <- loadSettings
  let settings' = case optTheme opts of
        Just "light"  -> settings { cfgTheme = ThemeLight }
        Just "dark"   -> settings { cfgTheme = ThemeDark }
        Just "system" -> settings { cfgTheme = ThemeSystem }
        _             -> settings
  app <- newApp settings' opts
  let window = if cfgRestoreWindow settings' then cfgWindow settings' else Nothing
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings
            { wsTitle = "Stronghead"
            , wsSize = maybe (Size 1500 960) swSize window
            , wsPosition = maybe WindowPositionDefault (uncurry WindowPositionAt) (window >>= swPosition)
            , wsMinSize = Just (Size 960 640)
            }
      , sdlAppThemeFor = Just (strongheadTheme . choiceAppearance (cfgTheme settings'))
      , sdlAppUiScale = uiScaleFactor (cfgScale settings')
      , sdlAppFontSize = 15
      , sdlAppMonoFont = FontSearch ["JetBrains Mono", "Cascadia Mono", "Source Code Pro", "Consolas", "DejaVu Sans Mono", "monospace"]
      }
    (appView app)
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
    <*> Opt.optional (Opt.strOption (Opt.long "baseline" <> Opt.metavar "FILE" <> Opt.help "Baseline profile or eventlog to diff against"))
    <*> Opt.optional (Opt.strOption (Opt.long "theme" <> Opt.metavar "THEME" <> Opt.help "UI theme (light, dark, system)"))
    <*> Opt.optional (Opt.strOption (Opt.long "preset" <> Opt.metavar "PRESET" <> Opt.help "Window layout arrangement preset"))
    <*> pure persist
  where
    parseConnect :: Text -> (Text, Text)
    parseConnect addr =
      let (host, port) = T.breakOnEnd ":" addr
       in if T.null host then ("127.0.0.1", port) else (T.dropEnd 1 host, port)
