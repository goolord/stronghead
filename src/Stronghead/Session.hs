-- | What a session keeps, and the two texts it is written as: the session
-- file and the pinboard's report.
--
-- A session is what was open and where the investigation stood: the
-- artifacts and dump folders, the connections, the pins with their notes,
-- the shared cursor and range, the focus, each panel's arrangement of panes
-- and the panel in front. It is kept as @key = value@ lines, like the
-- settings file. Lists are numbered keys (@pin.1@, @pin.1.note@). Values are
-- percent-encoded where they would otherwise be lost: a @%@, a line break,
-- and spaces at either end. Unknown keys and unreadable values are skipped,
-- so an older or hand-edited file still opens as far as it can be read.
--
-- Pane arrangements are opaque text here: the GUI writes and validates
-- them, so this module stays free of it.
module Stronghead.Session
  ( -- * Pins
    Pin (..)
  , pinToggle
  , pinMove
    -- * Sessions
  , Session (..)
  , emptySession
  , encodeSession
  , decodeSession
  , sessionExtension
    -- * Reports
  , PinReport (..)
  , ReportSection (..)
  , pinsMarkdown
    -- * Values
  , escapeValue
  , unescapeValue
  , parseKeyValues
  ) where

import Data.Char (chr, digitToInt, isHexDigit, isSpace, ord)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Word (Word64)
import Numeric (showHex)
import Stronghead.Ref (Ref (..), RunKey, parseRefTarget, refTarget)

-- ---------------------------------------------------------------------------
-- Pins

-- | An entity kept on the pinboard, with what the user wrote about it.
data Pin = Pin
  { pinRef :: !Ref
  , pinNote :: !Text
  }
  deriving (Eq, Show)

-- | Pin an entity at the end of the board, or take it off if it is there.
pinToggle :: Ref -> [Pin] -> [Pin]
pinToggle r ps
  | any ((== r) . pinRef) ps = filter ((/= r) . pinRef) ps
  | otherwise = ps ++ [Pin r ""]

-- | Move the pin at an index up (negative) or down (positive) the board,
-- stopping at either end.
pinMove :: Int -> Int -> [Pin] -> [Pin]
pinMove i by ps
  | i < 0 || i >= n || j == i = ps
  | otherwise =
      let p = ps !! i
          rest = take i ps ++ drop (i + 1) ps
       in take j rest ++ [p] ++ drop j rest
  where
    n = length ps
    j = max 0 (min (n - 1) (i + by))

-- ---------------------------------------------------------------------------
-- Sessions

data Session = Session
  { sesPanel :: !Text
    -- ^ The panel in front, by its command-line name.
  , sesEventlog :: !(Maybe FilePath)
  , sesFollow :: !Bool
    -- ^ Follow the eventlog as it is written.
  , sesHeapProfile :: !(Maybe FilePath)
  , sesCostCentres :: !(Maybe FilePath)
  , sesHeapSource :: !Text
    -- ^ What the heap panel charts: @eventlog@, @hp@ or @ghc-debug@.
  , sesGhcDebug :: !Text
    -- ^ The ghc-debug socket, empty for none.
  , sesDumps :: !(Maybe FilePath)
    -- ^ The dump folder Core & STG reads.
  , sesCodeFiles :: ![FilePath]
    -- ^ Files opened in Core & STG on their own: source, Core, STG.
  , sesAgentHost :: !Text
  , sesAgentPort :: !Text
  , sesAgentConnected :: !Bool
  , sesGhciCommand :: !Text
  , sesGhciDir :: !Text
  , sesGhciFile :: !Text
  , sesGhciRunning :: !Bool
  , sesPins :: ![Pin]
  , sesCursor :: !(Maybe (RunKey, Word64))
  , sesRange :: !(Maybe (RunKey, Word64, Word64))
  , sesFocus :: !(Maybe Ref)
  , sesLayouts :: ![(Text, Text)]
    -- ^ Each workbench's pane tree, by workbench name.
  , sesPresets :: ![(Text, Text)]
    -- ^ The user's named arrangements.
  , sesPopped :: ![Text]
    -- ^ Panes popped out into floating windows, by pane name.
  }
  deriving (Eq, Show)

emptySession :: Session
emptySession =
  Session
    { sesPanel = "timeline"
    , sesEventlog = Nothing
    , sesFollow = False
    , sesHeapProfile = Nothing
    , sesCostCentres = Nothing
    , sesHeapSource = "eventlog"
    , sesGhcDebug = ""
    , sesDumps = Nothing
    , sesCodeFiles = []
    , sesAgentHost = "127.0.0.1"
    , sesAgentPort = "4777"
    , sesAgentConnected = False
    , sesGhciCommand = "ghci"
    , sesGhciDir = ""
    , sesGhciFile = ""
    , sesGhciRunning = False
    , sesPins = []
    , sesCursor = Nothing
    , sesRange = Nothing
    , sesFocus = Nothing
    , sesLayouts = []
    , sesPresets = []
    , sesPopped = []
    }

-- | The extension session files are recognised by, on the command line and
-- when dropped on the window.
sessionExtension :: String
sessionExtension = ".stronghead"

encodeSession :: Session -> Text
encodeSession Session {..} =
  T.unlines $
    [ "# Stronghead session. Open it with Open session…, from the command line, or by dropping it on the window."
    , "version = 1"
    ]
      ++ [k <> " = " <> escapeValue v | (k, v) <- pairs]
  where
    pairs =
      [("panel", sesPanel)]
        ++ opt "eventlog" sesEventlog
        ++ [("follow", bool sesFollow) | sesFollow]
        ++ opt "heap-profile" sesHeapProfile
        ++ opt "cost-centres" sesCostCentres
        ++ [("heap-source", sesHeapSource)]
        ++ [("ghc-debug", sesGhcDebug) | not (T.null sesGhcDebug)]
        ++ opt "dumps" sesDumps
        ++ numbered "code-file" (map T.pack sesCodeFiles)
        ++ [ ("agent-host", sesAgentHost)
           , ("agent-port", sesAgentPort)
           , ("agent-connected", bool sesAgentConnected)
           , ("ghci-command", sesGhciCommand)
           , ("ghci-dir", sesGhciDir)
           , ("ghci-file", sesGhciFile)
           , ("ghci-running", bool sesGhciRunning)
           ]
        ++ concat [[(key i, refTarget (pinRef p))] ++ [(key i <> ".note", pinNote p) | not (T.null (pinNote p))] | (i, p) <- zip [1 :: Int ..] sesPins, let key n = "pin." <> tshow' n]
        ++ [("cursor", refTarget (RInstant r t)) | Just (r, t) <- [sesCursor]]
        ++ [("range", refTarget (RRange r a b)) | Just (r, a, b) <- [sesRange]]
        ++ [("focus", refTarget r) | Just r <- [sesFocus]]
        ++ [("layout." <> name, tree) | (name, tree) <- sesLayouts]
        ++ concat [[("preset." <> tshow' i, name), ("preset." <> tshow' i <> ".layout", tree)] | (i, (name, tree)) <- zip [1 :: Int ..] sesPresets]
        ++ numbered "popped" sesPopped
    opt k = maybe [] (\p -> [(k, T.pack p)])
    numbered k vs = [(k <> "." <> tshow' i, v) | (i, v) <- zip [1 :: Int ..] vs]
    bool b = if b then "true" else "false"

-- | The session a file describes. 'Left' only when it isn't a session file
-- at all; anything else unreadable falls back to the empty session's value.
decodeSession :: Text -> Either Text Session
decodeSession file
  | M.notMember "version" kvs = Left "not a Stronghead session: no version line"
  | otherwise =
      Right
        Session
          { sesPanel = text "panel" (sesPanel d)
          , sesEventlog = path "eventlog"
          , sesFollow = flag "follow" False
          , sesHeapProfile = path "heap-profile"
          , sesCostCentres = path "cost-centres"
          , sesHeapSource = text "heap-source" (sesHeapSource d)
          , sesGhcDebug = text "ghc-debug" ""
          , sesDumps = path "dumps"
          , sesCodeFiles = map T.unpack (numbered "code-file")
          , sesAgentHost = text "agent-host" (sesAgentHost d)
          , sesAgentPort = text "agent-port" (sesAgentPort d)
          , sesAgentConnected = flag "agent-connected" False
          , sesGhciCommand = text "ghci-command" (sesGhciCommand d)
          , sesGhciDir = text "ghci-dir" ""
          , sesGhciFile = text "ghci-file" ""
          , sesGhciRunning = flag "ghci-running" False
          , sesPins =
              [ Pin r (text ("pin." <> i <> ".note") "")
              | (i, v) <- indexed "pin"
              , Just r <- [parseRefTarget v]
              ]
          , sesCursor = case M.lookup "cursor" kvs >>= parseRefTarget of
              Just (RInstant r t) -> Just (r, t)
              _ -> Nothing
          , sesRange = case M.lookup "range" kvs >>= parseRefTarget of
              Just (RRange r a b) -> Just (r, a, b)
              _ -> Nothing
          , sesFocus = M.lookup "focus" kvs >>= parseRefTarget
          , sesLayouts = [(name, v) | (k, v) <- M.toList kvs, Just name <- [T.stripPrefix "layout." k], not (T.null name)]
          , sesPresets = [(name, tree) | (i, name) <- indexed "preset", Just tree <- [M.lookup ("preset." <> i <> ".layout") kvs], not (T.null name)]
          , sesPopped = numbered "popped"
          }
  where
    d = emptySession
    kvs = M.fromList (parseKeyValues file)
    text k def = fromMaybe def (M.lookup k kvs)
    path k = T.unpack <$> M.lookup k kvs
    flag k def = case M.lookup k kvs of
      Just "true" -> True
      Just "false" -> False
      _ -> def
    -- @prefix.N@, in order of N, with N as written.
    indexed prefix =
      map snd . sortOn fst $
        [ (n, (i, v))
        | (k, v) <- M.toList kvs
        , Just i <- [T.stripPrefix (prefix <> ".") k]
        , Right (n, "") <- [TR.decimal i :: Either String (Int, Text)]
        ]
    numbered prefix = map snd (indexed prefix)

-- | @key = value@ lines, values unescaped. Comments, blank lines and lines
-- without a key are skipped; a later key wins.
parseKeyValues :: Text -> [(Text, Text)]
parseKeyValues = mapMaybe line . T.lines
  where
    line l = case T.breakOn "=" (T.strip l) of
      (k, v)
        | T.null k || "#" `T.isPrefixOf` k || T.null v -> Nothing
        | otherwise -> Just (T.strip k, unescapeValue (T.strip (T.drop 1 v)))

-- | Percent-encode what a @key = value@ line would lose: @%@, line breaks
-- and spaces at either end. Paths and most text stay readable.
escapeValue :: Text -> Text
escapeValue v =
  let (lead, rest) = T.span isSpace v
      body = T.dropWhileEnd isSpace rest
      trail = T.drop (T.length body) rest
   in T.concatMap enc lead <> T.concatMap inner body <> T.concatMap enc trail
  where
    inner c = if c == '%' || c == '\n' || c == '\r' then enc c else T.singleton c
    enc c
      | ord c < 256 = T.pack ('%' : pad (showHex (ord c) ""))
      | otherwise = T.singleton c
    pad s = replicate (2 - length s) '0' ++ s

unescapeValue :: Text -> Text
unescapeValue = T.pack . go . T.unpack
  where
    go = \case
      '%' : a : b : rest | isHexDigit a, isHexDigit b -> chr (digitToInt a * 16 + digitToInt b) : go rest
      c : rest -> c : go rest
      [] -> []

tshow' :: Int -> Text
tshow' = T.pack . show

-- ---------------------------------------------------------------------------
-- Reports

-- | One panel's facts about a pinned entity, as text.
data ReportSection = ReportSection
  { rsPanel :: !Text
  , rsExact :: !(Maybe Text)
    -- ^ How the panel matched it.
  , rsFacts :: ![(Text, Text)]
  , rsNotes :: ![Text]
  }
  deriving (Eq, Show)

-- | A pin as the report shows it.
data PinReport = PinReport
  { prTitle :: !Text
  , prTarget :: !Text
    -- ^ Its reference, as "Copy reference" gives it.
  , prSections :: ![ReportSection]
  , prNote :: !Text
  }
  deriving (Eq, Show)

-- | The pinboard as Markdown: a heading per pin, its reference as code
-- (paste it into Go to… or @--goto@ to open it again), each panel's facts
-- and the note as a quote. @intro@ lines go under the title.
pinsMarkdown :: Text -> [Text] -> [PinReport] -> Text
pinsMarkdown title intro pins =
  T.unlines $
    ["# " <> md title, ""]
      ++ concatMap (\l -> [l, ""]) intro
      ++ (if null pins then ["No pins.", ""] else concat (zipWith pin [1 :: Int ..] pins))
  where
    pin i p =
      ["## " <> tshow' i <> ". " <> md (prTitle p), "", "`" <> prTarget p <> "`", ""]
        ++ concatMap section (prSections p)
        ++ (if any (not . emptySection) (prSections p) then [""] else [])
        ++ ( if T.null (T.strip (prNote p))
              then []
              else map (\l -> if T.null l then ">" else "> " <> l) (T.lines (prNote p)) ++ [""]
           )
    emptySection s = null (rsFacts s) && null (rsNotes s)
    section s
      | emptySection s = []
      | otherwise =
          ("- **" <> md (rsPanel s) <> "**" <> maybe "" (\e -> " (" <> md e <> ")") (rsExact s))
            : ["  - " <> (if T.null k then "" else md k <> ": ") <> md v | (k, v) <- rsFacts s]
            ++ ["  - " <> md n | n <- rsNotes s]
    md = T.concatMap (\c -> if c `elem` ("\\`*_[]<>#|" :: String) then T.pack ['\\', c] else T.singleton c)

