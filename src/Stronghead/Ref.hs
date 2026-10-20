-- | References to the things every panel shows: a thread, a collection, a
-- line of source, a cost centre. A 'Ref' is how one panel names an entity
-- to another.
--
-- Time-keyed refs carry their run, since a thread id or an instant means
-- nothing outside it. Code-keyed refs don't: a binding is the same binding
-- in every run of a build.
--
-- 'refTarget' gives each ref a text form, @kind/field/field@, with each
-- field percent-encoded so it can hold any text. It is the target of
-- hyperlinks, the reference stored in session files, and what "Copy
-- reference" puts on the clipboard; 'parseRefTarget' reads it back.
module Stronghead.Ref
  ( Ref (..)
  , RunKey (..)
  , refTarget
  , parseRefTarget
  ) where

import Data.ByteString qualified as B
import Data.Char (chr, digitToInt, isAlphaNum, isAscii, isHexDigit, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Word (Word32, Word64)
import Numeric (showHex)
import Stronghead.Debug.Graph (ClosureId, ThunkKey (..))
import Stronghead.Run (RunKey (..))
import Stronghead.Source (SrcSpan (..))

data Ref
  = RThread !RunKey !Word32
  | RInstant !RunKey !Word64
    -- ^ Program time, ns.
  | RRange !RunKey !Word64 !Word64
  | RCollection !RunKey !Int
    -- ^ Index into that run's collections.
  | RCensus !RunKey !Int
  | RMarker !RunKey !Int
  | RSource !SrcSpan
    -- ^ Resolved through the source index.
  | RBinding !Text !Text
    -- ^ Module, name key.
  | RCostCentre !Text !Text
    -- ^ Module, label.
  | RBand !Text
  | RInfoTable !Word64
  | RClosure !Int !ClosureId
    -- ^ Snapshot number, address.
  | RThunkGroup !ThunkKey
  | RBreakpoint !FilePath !Int
  | RModule !Text
  deriving (Eq, Ord, Show)

-- | @thread/stronghead-sample:4242/12@.
refTarget :: Ref -> Text
refTarget ref = T.intercalate "/" $ case ref of
  RThread r t -> ["thread", run r, num t]
  RInstant r t -> ["instant", run r, num t]
  RRange r a b -> ["range", run r, num a, num b]
  RCollection r i -> ["collection", run r, num i]
  RCensus r i -> ["census", run r, num i]
  RMarker r i -> ["marker", run r, num i]
  RSource (SrcSpan f l c el ec) -> ["source", escape f, num l, num c, num el, num ec]
  RBinding m k -> ["binding", escape m, escape k]
  RCostCentre m l -> ["cc", escape m, escape l]
  RBand b -> ["band", escape b]
  RInfoTable a -> ["info", hex a]
  RClosure sn a -> ["closure", num sn, hex a]
  -- "~" never comes out of 'escape', so it can stand for a missing source.
  RThunkGroup (ThunkKey name src path) -> ["thunks", escape name, maybe "~" escape src] ++ map escape path
  RBreakpoint f l -> ["break", escape (T.pack f), num l]
  RModule m -> ["module", escape m]
  where
    run (RunKey k) = escape k
    num :: (Show a) => a -> Text
    num = T.pack . show
    hex a = T.pack ("0x" <> showHex a "")

parseRefTarget :: Text -> Maybe Ref
parseRefTarget t = case T.splitOn "/" (T.strip t) of
  "thunks" : name : src : path -> do
    n <- unescape name
    sr <- if src == "~" then pure Nothing else Just <$> unescape src
    p <- mapM unescape path
    pure (RThunkGroup (ThunkKey n sr p))
  kind : raw -> do
    fs <- mapM unescape raw
    case (kind, fs) of
      ("thread", [r, i]) -> RThread (RunKey r) <$> num i
      ("instant", [r, i]) -> RInstant (RunKey r) <$> num i
      ("range", [r, a, b]) -> RRange (RunKey r) <$> num a <*> num b
      ("collection", [r, i]) -> RCollection (RunKey r) <$> num i
      ("census", [r, i]) -> RCensus (RunKey r) <$> num i
      ("marker", [r, i]) -> RMarker (RunKey r) <$> num i
      ("source", [f, l, c, el, ec]) -> fmap RSource (SrcSpan f <$> num l <*> num c <*> num el <*> num ec)
      ("binding", [m, k]) -> Just (RBinding m k)
      ("cc", [m, l]) -> Just (RCostCentre m l)
      ("band", [b]) -> Just (RBand b)
      ("info", [a]) -> RInfoTable <$> hex a
      ("closure", [sn, a]) -> RClosure <$> num sn <*> hex a
      ("break", [f, l]) -> RBreakpoint (T.unpack f) <$> num l
      ("module", [m]) -> Just (RModule m)
      _ -> Nothing
  [] -> Nothing
  where
    num :: (Integral a) => Text -> Maybe a
    num s = case TR.decimal s of
      Right (n, "") -> Just n
      _ -> Nothing
    hex s = case T.stripPrefix "0x" s of
      Just h | Right (n, "") <- TR.hexadecimal h -> Just n
      _ -> Nothing

-- | Percent-encode the UTF-8 bytes of everything but ASCII letters, digits
-- and @-_.:@, so a field never holds the separator, whitespace or @~@.
escape :: Text -> Text
escape = T.pack . concatMap byte . B.unpack . TE.encodeUtf8
  where
    byte w
      | isAscii c && isAlphaNum c || c `elem` ("-_.:" :: String) = [c]
      | otherwise = '%' : hexDigit (w `div` 16) : [hexDigit (w `mod` 16)]
      where
        c = chr (fromIntegral w)
    hexDigit d = "0123456789ABCDEF" !! fromIntegral d

unescape :: Text -> Maybe Text
unescape s = do
  bytes <- go (T.unpack s)
  either (const Nothing) Just (TE.decodeUtf8' (B.pack bytes))
  where
    go = \case
      [] -> Just []
      '%' : a : b : rest
        | isHexDigit a, isHexDigit b -> (fromIntegral (digitToInt a * 16 + digitToInt b) :) <$> go rest
        | otherwise -> Nothing
      c : rest
        | c /= '%', isAscii c -> (fromIntegral (ord c) :) <$> go rest
        | otherwise -> Nothing
