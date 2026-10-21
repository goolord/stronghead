-- | Entity references shared across panels, with slash-delimited
-- percent-encoded serialization ('refTarget', 'parseRefTarget') used for
-- hyperlinks, session files, and clipboard references.
--
-- Time-keyed references include a 'RunKey'; code-keyed references do not.
module Stronghead.Ref
  ( Ref (..)
  , RunKey (..)
  , refTarget
  , parseRefTarget
  ) where

import Control.Monad (guard, (<=<))
import Data.ByteString qualified as B
import Data.Char (digitToInt, isAlphaNum, isAscii, isHexDigit, ord)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64)

import Stronghead.Debug.Graph (ClosureId, ThunkKey (..))
import Stronghead.Format (fmtHex, percentEncodeWhere, readDecimal, readHex, tshow)
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
  deriving stock (Eq, Ord, Show)

-- | @thread/stronghead-sample:4242/12@.
refTarget :: Ref -> Text
refTarget ref = T.intercalate "/" $ case ref of
  RThread r t -> ["thread", run r, tshow t]
  RInstant r t -> ["instant", run r, tshow t]
  RRange r a b -> ["range", run r, tshow a, tshow b]
  RCollection r i -> ["collection", run r, tshow i]
  RCensus r i -> ["census", run r, tshow i]
  RMarker r i -> ["marker", run r, tshow i]
  RSource (SrcSpan f l c el ec) -> ["source", escape f, tshow l, tshow c, tshow el, tshow ec]
  RBinding m k -> ["binding", escape m, escape k]
  RCostCentre m l -> ["cc", escape m, escape l]
  RBand b -> ["band", escape b]
  RInfoTable a -> ["info", fmtHex a]
  RClosure sn a -> ["closure", tshow sn, fmtHex a]
  -- "~" never comes out of 'escape', so it can stand for a missing source.
  RThunkGroup (ThunkKey name src path) -> ["thunks", escape name, maybe "~" escape src] ++ map escape path
  RBreakpoint f l -> ["break", escape (T.pack f), tshow l]
  RModule m -> ["module", escape m]
  where
    run (RunKey k) = escape k

parseRefTarget :: Text -> Maybe Ref
parseRefTarget t = case T.splitOn "/" (T.strip t) of
  "thunks" : name : src : path -> do
    n <- unescape name
    sr <- if src == "~" then pure Nothing else Just <$> unescape src
    p <- traverse unescape path
    pure (RThunkGroup (ThunkKey n sr p))
  kind : raw -> do
    fs <- traverse unescape raw
    case (kind, fs) of
      ("thread", [r, i]) -> RThread (RunKey r) <$> readDecimal i
      ("instant", [r, i]) -> RInstant (RunKey r) <$> readDecimal i
      ("range", [r, a, b]) -> RRange (RunKey r) <$> readDecimal a <*> readDecimal b
      ("collection", [r, i]) -> RCollection (RunKey r) <$> readDecimal i
      ("census", [r, i]) -> RCensus (RunKey r) <$> readDecimal i
      ("marker", [r, i]) -> RMarker (RunKey r) <$> readDecimal i
      ("source", [f, l, c, el, ec]) -> fmap RSource (SrcSpan f <$> readDecimal l <*> readDecimal c <*> readDecimal el <*> readDecimal ec)
      ("binding", [m, k]) -> Just (RBinding m k)
      ("cc", [m, l]) -> Just (RCostCentre m l)
      ("band", [b]) -> Just (RBand b)
      ("info", [a]) -> RInfoTable <$> readHex a
      ("closure", [sn, a]) -> RClosure <$> readDecimal sn <*> readHex a
      ("break", [f, l]) -> RBreakpoint (T.unpack f) <$> readDecimal l
      ("module", [m]) -> Just (RModule m)
      _ -> Nothing
  [] -> Nothing

-- | Percent-encode the UTF-8 bytes of everything but ASCII letters, digits
-- and @-_.:@, so a field never holds the separator, whitespace or @~@.
escape :: Text -> Text
escape = percentEncodeWhere (\c -> isAscii c && isAlphaNum c || c `elem` ("-_.:" :: String))

unescape :: Text -> Maybe Text
unescape = eitherToMaybe . TE.decodeUtf8' . B.pack <=< go
  where
    go s = case T.uncons s of
      Nothing -> Just []
      Just ('%', r1) -> do
        (a, r2) <- T.uncons r1
        (b, rest) <- T.uncons r2
        guard (isHexDigit a && isHexDigit b)
        (fromIntegral (digitToInt a * 16 + digitToInt b) :) <$> go rest
      Just (c, rest) -> do
        guard (isAscii c)
        (fromIntegral (ord c) :) <$> go rest
