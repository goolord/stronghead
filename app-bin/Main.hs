-- | Stronghead entry point and CLI parser.
--
-- GUI mode:
--
-- > stronghead [--tab NAME] [--connect HOST:PORT] [--follow FILE.eventlog]
-- >            [--debug MODULE.hs] [--goto REF] [--mark TEXT] [FILE|DIR ...]
--
-- Headless CLI modes (no SDL window opened):
--
-- > stronghead check <spec.stronghead-spec> [--eventlog F] [--hp F] [--baseline F] [--dumps DIR] [--junit OUT.xml] [--svg OUT.svg]
-- > stronghead sweep --exe PATH [--grid "-A16m,-A64m,-A64m -xn"] [-- ARGS...]
-- > stronghead export --format speedscope|chrome-trace|folded|flamegraph-svg --out PATH INPUT
module Main (main) where

import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), defaultSdlOptions, runSdlApp)
import Options.Applicative qualified as Opt
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (IOMode (WriteMode), hSetEncoding, stderr, stdout, utf8, withFile)

import Demo (demoMain)
import Stronghead.Eventlog (loadEventlog)
import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Flamegraph
  ( exportChromeTraceJson
  , exportFolded
  , exportSpeedscopeJson
  , fromCostCentreSamples
  , fromProfFile
  )
import Stronghead.HeapProfile (HeapProfile, loadHp)
import Stronghead.Lab
  ( BudgetResult (..)
  , SweepConfig (..)
  , defaultSweepGrid
  , evaluateBudget
  , formatBudgetJUnit
  , formatBudgetMarkdown
  , parseBudgetSpec
  , renderParetoMarkdown
  , runRtsSweep
  )
import Stronghead.Prof (loadProf)
import UI.Common (strongheadTheme)
import UI.Graph (renderFlamegraphSvg, renderHpDiffSvg, renderHpSvg)
import UI.Settings
import UI.Shell

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    -- The demo video: a scripted tour recorded against the real sample.
    "--demo" : dir : _ -> demoMain dir
    "check" : rest -> runCheckCli rest
    "sweep" : rest -> runSweepCli rest
    "export" : rest -> runExportCli rest
    _ -> mainWindowed

-- ---------------------------------------------------------------------------
-- Headless CLI: check / sweep / export

data CheckOpts = CheckOpts
  { ckSpec :: !FilePath
  , ckEventlog :: !(Maybe FilePath)
  , ckHp :: !(Maybe FilePath)
  , ckBaseline :: !(Maybe FilePath)
  , ckDumps :: !(Maybe FilePath)
  , ckJunit :: !(Maybe FilePath)
  , ckSvg :: !(Maybe FilePath)
  }

runCheckCli :: [String] -> IO ()
runCheckCli rest = do
  let p =
        Opt.info
          ( ( CheckOpts
                <$> Opt.strArgument (Opt.metavar "SPEC" <> Opt.help "Path to .stronghead-spec file")
                <*> Opt.optional (Opt.strOption (Opt.long "eventlog" <> Opt.metavar "FILE" <> Opt.help "Eventlog (.eventlog) to check"))
                <*> Opt.optional (Opt.strOption (Opt.long "hp" <> Opt.metavar "FILE" <> Opt.help "Heap profile (.hp) to check"))
                <*> Opt.optional (Opt.strOption (Opt.long "baseline" <> Opt.metavar "FILE" <> Opt.help "Baseline .hp or .eventlog for delta checks"))
                <*> Opt.optional (Opt.strOption (Opt.long "dumps" <> Opt.metavar "DIR" <> Opt.help "GHC Core/STG dump directory"))
                <*> Opt.optional (Opt.strOption (Opt.long "junit" <> Opt.metavar "OUT.xml" <> Opt.help "Write JUnit XML report"))
                <*> Opt.optional (Opt.strOption (Opt.long "svg" <> Opt.metavar "OUT.svg" <> Opt.help "Write SVG proof graph"))
            )
              Opt.<**> Opt.helper
          )
          (Opt.fullDesc <> Opt.progDesc "Evaluate a declarative .stronghead-spec runtime budget (headless CI gate)")
  opts <- Opt.handleParseResult (Opt.execParserPure Opt.defaultPrefs p rest)
  exists <- doesFileExist (ckSpec opts)
  specTxt <- if exists then T.readFile (ckSpec opts) else pure (T.pack (ckSpec opts))
  let spec = parseBudgetSpec specTxt
  mTl <- traverse loadEventlogOrDie (ckEventlog opts)
  mHp <- traverse loadHpOrEventlog (ckHp opts)
  mBaseHp <- traverse loadHpOrEventlog (ckBaseline opts)
  res <- evaluateBudget spec mTl mHp mBaseHp (ckDumps opts)
  T.putStrLn (formatBudgetMarkdown res)
  case ckJunit opts of
    Just jPath -> writeUtf8 jPath (formatBudgetJUnit res)
    Nothing -> pure ()
  case (ckSvg opts, mBaseHp, maybe (mTl >>= tlHeapProfile) Just mHp) of
    (Just sPath, Just bHp, Just cHp) -> do
      svg <- renderHpDiffSvg Nothing (Just 12) bHp cHp
      writeUtf8 sPath svg
    (Just sPath, Nothing, Just cHp) -> do
      svg <- renderHpSvg Nothing (Just 12) cHp
      writeUtf8 sPath svg
    _ -> pure ()
  if brPassed res then pure () else exitFailure

data SweepOpts = SweepOpts
  { swpExe :: !FilePath
  , swpGrid :: !(Maybe Text)
  , swpProgArgs :: ![String]
  }

runSweepCli :: [String] -> IO ()
runSweepCli rest = do
  let p =
        Opt.info
          ( ( SweepOpts
                <$> Opt.strOption (Opt.long "exe" <> Opt.metavar "PATH" <> Opt.help "Compiled Haskell binary to sweep")
                <*> Opt.optional (Opt.strOption (Opt.long "grid" <> Opt.metavar "FLAGS,..." <> Opt.help "Comma-separated RTS flag combinations (e.g. '-A16m,-A64m,-A64m -xn')"))
                <*> Opt.many (Opt.strArgument (Opt.metavar "ARGS..." <> Opt.help "Arguments passed to the executable (place after --)"))
            )
              Opt.<**> Opt.helper
          )
          (Opt.fullDesc <> Opt.progDesc "Sweep GHC +RTS flags and compute the empirical Pareto frontier")
  opts <- Opt.handleParseResult (Opt.execParserPure Opt.defaultPrefs p rest)
  let grid = case swpGrid opts of
        Nothing -> defaultSweepGrid
        Just g ->
          let parsed = filter (not . null) (map (words . T.unpack . T.strip) (T.splitOn "," g))
           in if null parsed then defaultSweepGrid else parsed
  res <- runRtsSweep (SweepConfig (swpExe opts) (swpProgArgs opts) grid Nothing)
  case res of
    Left err -> T.hPutStrLn stderr err >> exitFailure
    Right ps -> T.putStrLn (renderParetoMarkdown ps)

data ExportOpts = ExportOpts
  { exFormat :: !Text
  , exOut :: !FilePath
  , exInput :: !FilePath
  }

runExportCli :: [String] -> IO ()
runExportCli rest = do
  let p =
        Opt.info
          ( ( ExportOpts
                <$> Opt.strOption (Opt.long "format" <> Opt.metavar "FMT" <> Opt.help "Export format: speedscope, chrome-trace, folded, flamegraph-svg")
                <*> Opt.strOption (Opt.long "out" <> Opt.metavar "PATH" <> Opt.help "Output file path")
                <*> Opt.strArgument (Opt.metavar "INPUT" <> Opt.help "Input .eventlog or .prof file")
            )
              Opt.<**> Opt.helper
          )
          (Opt.fullDesc <> Opt.progDesc "Headless export of Stronghead profiling artifacts to open formats and SVGs")
  opts <- Opt.handleParseResult (Opt.execParserPure Opt.defaultPrefs p rest)
  let fmt = T.toLower (T.strip (exFormat opts))
      inp = exInput opts
  case fmt of
    "chrome-trace" -> do
      tl <- loadEventlogOrDie inp
      writeUtf8 (exOut opts) (exportChromeTraceJson tl)
    _ -> do
      ft <-
        if ".prof" `isSuffixOf` inp
          then do
            ePf <- loadProf inp
            case ePf of
              Left err -> T.hPutStrLn stderr ("Failed to read .prof: " <> T.pack err) >> exitFailure
              Right pf -> pure (fromProfFile pf)
          else do
            tl <- loadEventlogOrDie inp
            pure (fromCostCentreSamples tl 0 maxBound)
      outTxt <- case fmt of
        "speedscope" -> pure (exportSpeedscopeJson (T.pack (takeFileName inp)) ft)
        "folded" -> pure (exportFolded ft)
        "flamegraph-svg" -> renderFlamegraphSvg Nothing ft
        other -> T.hPutStrLn stderr ("Unknown format: " <> other <> " (expected speedscope, chrome-trace, folded, or flamegraph-svg)") >> exitFailure
      writeUtf8 (exOut opts) outTxt

loadEventlogOrDie :: FilePath -> IO Timeline
loadEventlogOrDie path =
  loadEventlog path >>= \case
    Left err -> T.hPutStrLn stderr ("Failed to load eventlog " <> T.pack path <> ": " <> T.pack err) >> exitFailure
    Right tl -> pure tl

loadHpOrEventlog :: FilePath -> IO HeapProfile
loadHpOrEventlog path
  | ".eventlog" `isSuffixOf` path = do
      tl <- loadEventlogOrDie path
      case tlHeapProfile tl of
        Just hp -> pure hp
        Nothing -> T.hPutStrLn stderr ("Eventlog contains no heap profile samples: " <> T.pack path) >> exitFailure
  | otherwise =
      loadHp path >>= \case
        Left err -> T.hPutStrLn stderr ("Failed to load heap profile " <> T.pack path <> ": " <> T.pack err) >> exitFailure
        Right hp -> pure hp

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 path content = do
  let dir = takeDirectory path
  if null dir then pure () else createDirectoryIfMissing True dir
  withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    T.hPutStr h content

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
