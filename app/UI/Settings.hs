-- | Preferences that outlive a run: the interface (theme, scale), the
-- window's geometry, and the defaults the panels start from. They are kept
-- as @key = value@ lines in the user's config directory
-- (@%APPDATA%\\stronghead\\settings.conf@ on Windows,
-- @~\/.config\/stronghead\/settings.conf@ elsewhere). Unknown keys and
-- unreadable values fall back to the defaults, so an old or hand-edited file
-- never stops the program starting.
module UI.Settings
  ( Settings (..)
  , defaultSettings
  , SavedWindow (..)
  , ThemeChoice (..)
  , themeChoiceName
  , choiceAppearance
  , UiScale (..)
  , uiScaleFactor
  , uiScaleLabel
  , stepScale
  , settingsPath
  , loadSettings
  , saveSettings
  ) where

import Control.Exception (IOException, try)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NanoUI (Appearance (..), Size (..))
import Stronghead.Format (tshow)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory, renameFile)
import System.FilePath (takeDirectory, (</>))
import Text.Read (readMaybe)
import UI.Code (Colouring (..))

data Settings = Settings
  { cfgTheme :: !ThemeChoice
  , cfgScale :: !UiScale
  , cfgRestoreWindow :: !Bool
    -- ^ Reopen the window where it was, at the size it was.
  , cfgWindow :: !(Maybe SavedWindow)
  , cfgStartPanel :: !(Maybe Text)
    -- ^ The panel to open on, by its command-line name; 'Nothing' for the
    -- one in use when the program last quit.
  , cfgLastPanel :: !Text
  , cfgAgentHost :: !Text
  , cfgAgentPort :: !Text
  , cfgGhciCommand :: !Text
  , cfgColouring :: !Colouring
  , cfgHideUniques :: !Bool
  , cfgHideTypeable :: !Bool
  , cfgProjectRoot :: !Text
    -- ^ Where to look for sources; empty for only the dump folder's project.
  , cfgEditorCommand :: !Text
    -- ^ A template with @{file}@, @{line}@ and @{col}@; empty for none.
  }
  deriving (Eq, Show)

-- | The window's restored (not maximized) geometry, in layout units.
data SavedWindow = SavedWindow
  { swSize :: !Size
  , swPosition :: !(Maybe (Int, Int))
  , swMaximized :: !Bool
  }
  deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { cfgTheme = ThemeSystem
    , cfgScale = ScaleAuto
    , cfgRestoreWindow = True
    , cfgWindow = Nothing
    , cfgStartPanel = Nothing
    , cfgLastPanel = "timeline"
    , cfgAgentHost = "127.0.0.1"
    , cfgAgentPort = "4777"
    , cfgGhciCommand = "ghci"
    , cfgColouring = BoxingColours
    , cfgHideUniques = True
    , cfgHideTypeable = True
    , cfgProjectRoot = ""
    , cfgEditorCommand = ""
    }

-- ---------------------------------------------------------------------------
-- Choices

data ThemeChoice = ThemeSystem | ThemeLight | ThemeDark
  deriving (Eq, Show, Enum, Bounded)

themeChoiceName :: ThemeChoice -> Text
themeChoiceName = \case
  ThemeSystem -> "System"
  ThemeLight -> "Light"
  ThemeDark -> "Dark"

-- | The appearance to theme for, given the desktop's.
choiceAppearance :: ThemeChoice -> Maybe Appearance -> Maybe Appearance
choiceAppearance c system = case c of
  ThemeSystem -> system
  ThemeLight -> Just AppearanceLight
  ThemeDark -> Just AppearanceDark

-- | Zoom of the whole interface. 'ScaleAuto' follows the display's content
-- scale (on Windows, the desktop's scaling setting).
data UiScale
  = ScaleAuto
  | Scale75
  | Scale90
  | Scale100
  | Scale110
  | Scale125
  | Scale150
  | Scale175
  | Scale200
  | Scale250
  | Scale300
  deriving (Eq, Ord, Show, Enum, Bounded)

scalePercent :: UiScale -> Maybe Int
scalePercent = \case
  ScaleAuto -> Nothing
  Scale75 -> Just 75
  Scale90 -> Just 90
  Scale100 -> Just 100
  Scale110 -> Just 110
  Scale125 -> Just 125
  Scale150 -> Just 150
  Scale175 -> Just 175
  Scale200 -> Just 200
  Scale250 -> Just 250
  Scale300 -> Just 300

-- | The value for @setSdlUiScale@: zero follows the display.
uiScaleFactor :: UiScale -> Float
uiScaleFactor = maybe 0 (\p -> fromIntegral p / 100) . scalePercent

-- | A scale's name; @auto@ is the scale the display gives, when known.
uiScaleLabel :: Maybe Float -> UiScale -> Text
uiScaleLabel auto s = case scalePercent s of
  Just p -> tshow p <> "%"
  Nothing -> "Follow the display" <> maybe "" (\a -> " (" <> tshow (round (a * 100) :: Int) <> "%)") auto

-- | One step up or down the fixed scales, from @current@ (the scale in
-- effect, for stepping off 'ScaleAuto').
stepScale :: Int -> Float -> UiScale -> UiScale
stepScale dir current s =
  let fixed = [Scale75 .. maxBound]
      here = case s of
        ScaleAuto -> nearest
        _ -> s
      nearest = snd (minimum [(abs (uiScaleFactor f - current), f) | f <- fixed])
      i = length (takeWhile (/= here) fixed)
   in fixed !! max 0 (min (length fixed - 1) (i + dir))

-- ---------------------------------------------------------------------------
-- The file

settingsPath :: IO FilePath
settingsPath = (</> "settings.conf") <$> getXdgDirectory XdgConfig "stronghead"

-- | The saved settings, or the defaults when there are none.
loadSettings :: IO Settings
loadSettings = do
  path <- settingsPath
  exists <- doesFileExist path
  if not exists
    then pure defaultSettings
    else either (const defaultSettings) decode <$> (try (T.readFile path) :: IO (Either IOException Text))

-- | Write the settings, replacing the file only once the new one is whole.
saveSettings :: Settings -> IO (Either Text ())
saveSettings s = do
  path <- settingsPath
  r <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    let tmp = path <> ".tmp"
    T.writeFile tmp (encode s)
    renameFile tmp path
  pure $ case r of
    Left (e :: IOException) -> Left ("Could not save settings: " <> T.pack (show e))
    Right () -> Right ()

encode :: Settings -> Text
encode Settings {..} =
  T.unlines $
    "# Stronghead settings. Edited from the Settings dialog; unknown keys are ignored."
      : [k <> " = " <> v | (k, v) <- pairs]
  where
    pairs =
      [ ("theme", case cfgTheme of ThemeSystem -> "system"; ThemeLight -> "light"; ThemeDark -> "dark")
      , ("ui-scale", maybe "auto" tshow (scalePercent cfgScale))
      , ("restore-window", bool cfgRestoreWindow)
      ]
        ++ maybe [] windowPairs cfgWindow
        ++ [ ("start-panel", fromMaybe "last" cfgStartPanel)
           , ("last-panel", cfgLastPanel)
           , ("agent-host", cfgAgentHost)
           , ("agent-port", cfgAgentPort)
           , ("ghci-command", cfgGhciCommand)
           , ("code-colouring", case cfgColouring of BoxingColours -> "boxing"; SyntaxColours -> "syntax")
           , ("hide-uniques", bool cfgHideUniques)
           , ("hide-typeable", bool cfgHideTypeable)
           , ("project-root", cfgProjectRoot)
           , ("editor-command", cfgEditorCommand)
           ]
    windowPairs SavedWindow {..} =
      let Size w h = swSize
       in [ ("window-size", tshow (round w :: Int) <> "x" <> tshow (round h :: Int))
          , ("window-maximized", bool swMaximized)
          ]
            ++ maybe [] (\(x, y) -> [("window-position", tshow x <> "," <> tshow y)]) swPosition
    bool b = if b then "true" else "false"

decode :: Text -> Settings
decode file =
  Settings
    { cfgTheme = field "theme" (cfgTheme d) $ \case
        "system" -> Just ThemeSystem
        "light" -> Just ThemeLight
        "dark" -> Just ThemeDark
        _ -> Nothing
    , cfgScale = field "ui-scale" (cfgScale d) $ \v ->
        if v == "auto"
          then Just ScaleAuto
          else readInt v >>= \p -> lookup (Just p) [(scalePercent s, s) | s <- [minBound .. maxBound]]
    , cfgRestoreWindow = field "restore-window" (cfgRestoreWindow d) readBool
    , cfgWindow = do
        (w, h) <- M.lookup "window-size" kvs >>= pair 'x'
        pure
          SavedWindow
            { swSize = Size (fromIntegral (max 320 w)) (fromIntegral (max 240 h))
            , swPosition = M.lookup "window-position" kvs >>= pair ','
            , swMaximized = field "window-maximized" False readBool
            }
    , cfgStartPanel = field "start-panel" (cfgStartPanel d) $ \v -> Just (if v == "last" then Nothing else Just v)
    , cfgLastPanel = field "last-panel" (cfgLastPanel d) Just
    , cfgAgentHost = field "agent-host" (cfgAgentHost d) Just
    , cfgAgentPort = field "agent-port" (cfgAgentPort d) Just
    , cfgGhciCommand = field "ghci-command" (cfgGhciCommand d) nonEmpty
    , cfgColouring = field "code-colouring" (cfgColouring d) $ \case
        "boxing" -> Just BoxingColours
        "syntax" -> Just SyntaxColours
        _ -> Nothing
    , cfgHideUniques = field "hide-uniques" (cfgHideUniques d) readBool
    , cfgHideTypeable = field "hide-typeable" (cfgHideTypeable d) readBool
    , cfgProjectRoot = field "project-root" (cfgProjectRoot d) Just
    , cfgEditorCommand = field "editor-command" (cfgEditorCommand d) Just
    }
  where
    d = defaultSettings
    kvs = M.fromList (mapMaybe line (T.lines file))
    line l = case T.breakOn "=" (T.strip l) of
      (k, v)
        | T.null k || "#" `T.isPrefixOf` k || T.null v -> Nothing
        | otherwise -> Just (T.strip k, T.strip (T.drop 1 v))
    field :: Text -> a -> (Text -> Maybe a) -> a
    field k def parse = fromMaybe def (M.lookup k kvs >>= parse)
    readBool = \case
      "true" -> Just True
      "false" -> Just False
      _ -> Nothing
    readInt :: Text -> Maybe Int
    readInt = readMaybe . T.unpack . T.strip
    nonEmpty v = if T.null v then Nothing else Just v
    pair c v = case T.splitOn (T.singleton c) v of
      [a, b] -> (,) <$> readInt a <*> readInt b
      _ -> Nothing
