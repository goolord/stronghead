-- | Client side of the live-telemetry protocol spoken by "Stronghead.Agent".
--
-- The agent sends plain text lines over TCP:
--
-- > H <program> <pid> <capabilities>       once, on connect
-- > S <key>=<value> <key>=<value> ...      RTS statistics
-- > T <thread-id> <status> <label...>      one per live Haskell thread
-- > E                                      end of a frame
--
-- Values are integers; times are nanoseconds and sizes bytes, as in
-- 'GHC.Stats.RTSStats'.
module Stronghead.Live
  ( Frame (..)
  , LiveThread (..)
  , LiveEvent (..)
  , connectLive
  , frameClock
  , parseLine
  , Line (..)
  , stat
  ) where

import Control.Exception (SomeException, bracket, try)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Read qualified as TR
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import Network.Socket
import Network.Socket.ByteString (recv)

data LiveThread = LiveThread
  { ltId :: !Int
  , ltStatus :: !Text
  , ltLabel :: !Text
  }
  deriving (Eq, Show)

data Frame = Frame
  { frTime :: !Double
    -- ^ Seconds on our clock when the frame arrived.
  , frProgramNs :: !Word64
    -- ^ Program time, as eventlog timestamps count it (see 'frameClock').
  , frProgramExact :: !Bool
    -- ^ Whether 'frProgramNs' came from the program's statistics rather
    -- than our clock.
  , frStats :: !(Map Text Double)
  , frThreads :: ![LiveThread]
  }
  deriving (Eq, Show)

stat :: Text -> Frame -> Double
stat k f = M.findWithDefault 0 k (frStats f)

data LiveEvent
  = LiveHello !Text !Int !Int
    -- ^ Program name, pid, capabilities.
  | LiveFrame !Frame
  | LiveClosed !Text
  deriving (Eq, Show)

data Line
  = LHello !Text !Int !Int
  | LStats !(Map Text Double)
  | LThread !LiveThread
  | LEnd
  | LUnknown
  deriving (Eq, Show)

parseLine :: Text -> Line
parseLine l = case T.words l of
  ["H", prog, pid, caps] -> LHello prog (int pid) (int caps)
  "S" : kvs -> LStats (M.fromList [(k, dbl (T.drop 1 v)) | kv <- kvs, let (k, v) = T.breakOn "=" kv, not (T.null v)])
  "T" : tid : status : lbl -> LThread (LiveThread (int tid) status (T.unwords lbl))
  ["E"] -> LEnd
  _ -> LUnknown
  where
    int t = either (const 0) fst (TR.signed TR.decimal t)
    dbl t = either (const 0) fst (TR.signed TR.double t)

-- | Connect and deliver events until the connection drops. Blocks; run it
-- on a background thread.
connectLive :: String -> Int -> (LiveEvent -> IO ()) -> IO ()
connectLive host port emit = do
  r <- try $ do
    addrs <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just (show port))
    case addrs of
      [] -> fail ("cannot resolve " <> host)
      addr : _ ->
        bracket (openSocket addr) close $ \sock -> do
          connect sock (addrAddress addr)
          connected <- getMonotonicTime
          let loop buf stats threads = do
                chunk <- recv sock 65536
                if B.null chunk
                  then pure ()
                  else do
                    let (ls, rest) = splitLines (buf <> chunk)
                    (stats', threads') <- handle ls stats threads
                    loop rest stats' threads'
              handle [] s ts = pure (s, ts)
              handle (l : more) s ts = case parseLine (TE.decodeUtf8With TE.lenientDecode l) of
                LHello p pid caps -> emit (LiveHello p pid caps) >> handle more s ts
                LStats m -> handle more m ts
                LThread t -> handle more s (t : ts)
                LEnd -> do
                  now <- getMonotonicTime
                  let (prog, exact) = frameClock (now - connected) s
                  emit (LiveFrame (Frame now prog exact s (reverse ts)))
                  handle more M.empty []
                LUnknown -> handle more s ts
          loop B.empty M.empty []
  emit . LiveClosed $ case r of
    Left (e :: SomeException) -> T.pack (show e)
    Right () -> "connection closed by the program"

-- | A frame's program time from its statistics, or, without them (no
-- @+RTS -T@), the seconds since the connection opened, which is only a
-- guess.
--
-- @elapsed_ns@ counts from the end of RTS initialisation and eventlog
-- timestamps from its start, so the agent also sends @init_elapsed_ns@ and
-- the two are added. An older agent doesn't, and its times are early by the
-- RTS's start-up time, about a millisecond.
frameClock :: Double -> Map Text Double -> (Word64, Bool)
frameClock sinceConnect stats = case M.lookup "elapsed_ns" stats of
  Just ns | M.findWithDefault 0 "rts_stats" stats > 0 -> (round (ns + M.findWithDefault 0 "init_elapsed_ns" stats), True)
  _ -> (round (max 0 sinceConnect * 1e9), False)

splitLines :: B.ByteString -> ([B.ByteString], B.ByteString)
splitLines bs =
  let parts = BC.split '\n' bs
   in (map (BC.filter (/= '\r')) (init parts), last parts)
