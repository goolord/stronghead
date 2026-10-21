-- | Client side of the live-telemetry protocol spoken by "Stronghead.Agent".
--
-- The agent sends plain text lines over TCP:
--
-- > H <program> <pid> <capabilities>       once, on connect
-- > I version=2 cwd=<%> exe=<%> args=<%,%,...> heap-profile=<mode|none>
-- >   heap-interval-ms=<n> eventlog=<0|1> commands=<0|1>
-- >                                        once, after H (v2)
-- > S <key>=<value> <key>=<value> ...      RTS statistics
-- > T <thread-id> <status> <label...>      one per live Haskell thread
-- > C <thread-id> <capability>             after its thread's T line (v2)
-- > E                                      end of a frame
-- > A <id> ok [t=<ns>]                     a command's reply (v2)
-- > A <id> error <text>
--
-- Values are integers; times are nanoseconds and sizes bytes, as in
-- 'GHC.Stats.RTSStats'. In the info line, @<%>@ is a percent-encoded field
-- (UTF-8 bytes; everything but ASCII letters, digits and @-_./:\\@ is
-- encoded), and @args@ is the full command line ('GHC.Environment.getFullArgs',
-- program first, RTS options included), each argument encoded and joined
-- by commas. @heap-profile@ names the RTS flag of the breakdown (@hT@,
-- @hi@, @hc@ ...) and @commands@ says whether the agent takes commands.
--
-- A v2 client writes commands, one per line, each with an id it chooses:
--
-- > <id> mark <text>                       traceMarkerIO
-- > <id> gc                                performMajorGC
-- > <id> census                            requestHeapCensus
-- > <id> heap-profiling on|off             startHeapProfTimer / stopHeapProfTimer
-- > <id> flush                             flushEventLog
--
-- Each is answered by an @A@ line. Every command but @flush@ also posts an
-- eventlog marker (the text, or @stronghead: major GC@ and so on), and the
-- reply's @t@ is the program time just after it ran, when the program runs
-- with @+RTS -T@.
--
-- The versions mix both ways: a v1 client ignores the lines it doesn't
-- know, and a v1 agent sends no @I@ line and never reads the socket, which
-- is how a v2 client tells it apart (see 'ldInfo' in the GUI).
module Stronghead.Live
  ( Frame (..)
  , LiveThread (..)
  , LiveEvent (..)
  , AgentInfo (..)
  , Command (..)
  , Reply (..)
  , Outbox
  , newOutbox
  , readOutbox
  , connectLive
  , sendCommand
  , renderCommand
  , commandMarker
  , frameClock
  , frameGaps
  , parseLine
  , Line (..)
  , stat
  , percentEncode
  , percentDecode
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever)
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (digitToInt, isAlphaNum, isAscii, isHexDigit)
import Data.List (sort, unsnoc, (!?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Stronghead.Format (percentEncodeWhere, readDecimal, readSignedDecimal, readSignedDouble, tshow)

data LiveThread = LiveThread
  { ltId :: !Int
  , ltStatus :: !Text
  , ltLabel :: !Text
  , ltCap :: !(Maybe Int)
    -- ^ The capability it last ran on, from a v2 agent's @C@ line.
  }
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

stat :: Text -> Frame -> Double
stat k f = M.findWithDefault 0 k (frStats f)

-- | What a v2 agent says about where and how the program runs.
data AgentInfo = AgentInfo
  { aiVersion :: !Int
  , aiCwd :: !Text
  , aiExe :: !Text
  , aiArgs :: ![Text]
    -- ^ The full command line, program first, RTS options included.
  , aiHeapProfile :: !(Maybe Text)
    -- ^ The breakdown's RTS flag (@hT@, @hi@ ...); 'Nothing' without one.
  , aiHeapIntervalMs :: !Int
  , aiEventlog :: !Bool
  , aiCommands :: !Bool
    -- ^ Whether the agent takes commands.
  }
  deriving stock (Eq, Show)

-- | What a client may ask of the program.
data Command
  = CmdMark !Text
  | CmdGC
  | CmdCensus
  | CmdHeapProfiling !Bool
  | CmdFlush
  deriving stock (Eq, Show)

data Reply
  = ReplyOk !(Maybe Word64)
    -- ^ With the program time just after the command ran, under @+RTS -T@.
  | ReplyError !Text
  deriving stock (Eq, Show)

-- | Where to put commands for one connection's sender.
newtype Outbox = Outbox (TQueue Text)
  deriving newtype (Eq)

instance Show Outbox where
  show _ = "<outbox>"

newOutbox :: IO Outbox
newOutbox = Outbox <$> newTQueueIO

-- | The next command line queued, waiting for one.
readOutbox :: Outbox -> IO Text
readOutbox (Outbox q) = atomically (readTQueue q)

data LiveEvent
  = LiveHello !Text !Int !Int
    -- ^ Program name, pid, capabilities.
  | LiveInfo !AgentInfo
  | LiveConnected !Outbox
    -- ^ Commands written here go to the program.
  | LiveFrame !Frame
  | LiveReply !Int !Reply
  | LiveClosed !Text
  deriving stock (Eq, Show)

data Line
  = LHello !Text !Int !Int
  | LInfo !AgentInfo
  | LStats !(Map Text Double)
  | LThread !LiveThread
  | LCap !Int !Int
  | LReply !Int !Reply
  | LEnd
  | LUnknown
  deriving stock (Eq, Show)

parseLine :: Text -> Line
parseLine l = case T.words l of
  ["H", prog, pid, caps] -> LHello prog (int pid) (int caps)
  "I" : kvs -> LInfo (parseInfo (pairs kvs))
  "S" : kvs -> LStats (M.fromList [(k, dbl v) | (k, v) <- pairs kvs])
  "T" : tid : status : lbl -> LThread (LiveThread (int tid) status (T.unwords lbl) Nothing)
  ["C", tid, cap] -> LCap (int tid) (int cap)
  "A" : rid : "ok" : rest -> LReply (int rid) (ReplyOk (lookup "t" (pairs rest) >>= readDecimal))
  "A" : rid : "error" : rest -> LReply (int rid) (ReplyError (T.unwords rest))
  ["E"] -> LEnd
  _ -> LUnknown
  where
    int = fromMaybe 0 . readSignedDecimal
    dbl = fromMaybe 0 . readSignedDouble
    pairs kvs = [(k, T.drop 1 v) | kv <- kvs, let (k, v) = T.breakOn "=" kv, not (T.null v)]

parseInfo :: [(Text, Text)] -> AgentInfo
parseInfo kvs =
  AgentInfo
    { aiVersion = maybe 0 int (lookup "version" kvs)
    , aiCwd = field "cwd"
    , aiExe = field "exe"
    , aiArgs = case lookup "args" kvs of
        Just a | not (T.null a) -> map percentDecode (T.splitOn "," a)
        _ -> []
    , aiHeapProfile = case lookup "heap-profile" kvs of
        Just m | m /= "none", not (T.null m) -> Just m
        _ -> Nothing
    , aiHeapIntervalMs = maybe 0 int (lookup "heap-interval-ms" kvs)
    , aiEventlog = lookup "eventlog" kvs == Just "1"
    , aiCommands = lookup "commands" kvs == Just "1"
    }
  where
    field k = maybe "" percentDecode (lookup k kvs)
    int = fromMaybe 0 . readDecimal

-- | A command line for the agent.
renderCommand :: Int -> Command -> Text
renderCommand i c =
  tshow i <> " " <> case c of
    -- One line: a newline in the text would end the command.
    CmdMark t -> "mark " <> oneLine t
    CmdGC -> "gc"
    CmdCensus -> "census"
    CmdHeapProfiling on -> "heap-profiling " <> if on then "on" else "off"
    CmdFlush -> "flush"

-- | The eventlog marker the agent posts for a command (it trims a mark's
-- spaces); 'Nothing' for a flush, which posts none.
commandMarker :: Command -> Maybe Text
commandMarker = \case
  CmdMark t -> let m = T.dropAround (== ' ') (oneLine t) in Just (if T.null m then "mark" else m)
  CmdGC -> Just "stronghead: major GC"
  CmdCensus -> Just "stronghead: heap census"
  CmdHeapProfiling on -> Just ("stronghead: heap profiling " <> if on then "on" else "off")
  CmdFlush -> Nothing

oneLine :: Text -> Text
oneLine = T.map (\ch -> if ch == '\n' || ch == '\r' then ' ' else ch)

-- | Queue a command; the connection's sender writes it.
sendCommand :: Outbox -> Int -> Command -> IO ()
sendCommand (Outbox q) i c = atomically (writeTQueue q (renderCommand i c))

-- | Connect and deliver events until the connection drops. Blocks; run it
-- on a background thread.
connectLive :: String -> Int -> (LiveEvent -> IO ()) -> IO ()
connectLive host port emit = do
  r <- try @SomeException do
    addrs <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just (show port))
    case addrs of
      [] -> fail ("cannot resolve " <> host)
      addr : _ ->
        bracket (openSocket addr) close \sock -> do
          connect sock (addrAddress addr)
          connected <- getMonotonicTime
          box <- newOutbox
          let sender = forever (readOutbox box >>= \l -> sendAll sock (TE.encodeUtf8 (l <> "\n")))
          withAsync sender \_ -> do
            emit (LiveConnected box)
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
                  LInfo i -> emit (LiveInfo i) >> handle more s ts
                  LStats m -> handle more m ts
                  LThread t -> handle more s (t : ts)
                  LCap tid cap -> handle more s (withCap tid cap ts)
                  LReply i rep -> emit (LiveReply i rep) >> handle more s ts
                  LEnd -> do
                    now <- getMonotonicTime
                    let (prog, exact) = frameClock (now - connected) s
                    emit (LiveFrame (Frame now prog exact s (reverse ts)))
                    handle more M.empty []
                  LUnknown -> handle more s ts
            loop B.empty M.empty []
  emit (LiveClosed (either tshow (const "connection closed by the program") r))
  where
    -- The C line follows its thread's T line, so it is almost always the
    -- newest.
    withCap tid cap = \case
      t : more | ltId t == tid -> t {ltCap = Just cap} : more
      t : more -> t : withCap tid cap more
      [] -> []

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

-- | Stretches with no frames: the program (or the agent) was stopped, by
-- a ghc-debug pause or snapshot, a long collection, or a debugger. Program
-- time, from the last frame before to the first after. A gap is three
-- times the usual spacing and at least a second and a half, so a program
-- whose collections slow every frame down isn't all gaps.
frameGaps :: [Frame] -> [(Word64, Word64)]
frameGaps xs =
  let steps = zip xs (drop 1 xs)
      spacing = sort [frTime b - frTime a | (a, b) <- steps]
      usual = fromMaybe 0.25 (spacing !? (length spacing `div` 2))
      limit = max 1.5 (3 * usual)
   in [(frProgramNs a, frProgramNs b) | (a, b) <- steps, frTime b - frTime a > limit, frProgramNs b > frProgramNs a]

splitLines :: B.ByteString -> ([B.ByteString], B.ByteString)
splitLines bs =
  maybe ([], B.empty) (first (map (BC.filter (/= '\r')))) (unsnoc (BC.split '\n' bs))

-- | Percent-encode the UTF-8 bytes of everything but ASCII letters, digits
-- and @-_./:\\@, as the agent does.
percentEncode :: Text -> Text
percentEncode = percentEncodeWhere (\c -> isAscii c && isAlphaNum c || c `elem` ("-_./:\\" :: String))

-- | Undo 'percentEncode'. A malformed escape is kept as it is, and bytes
-- that aren't UTF-8 are replaced.
percentDecode :: Text -> Text
percentDecode = TE.decodeUtf8With TE.lenientDecode . B.pack . go
  where
    go s = case T.uncons s of
      Nothing -> []
      Just ('%', r1)
        | Just (a, r2) <- T.uncons r1
        , Just (b, rest) <- T.uncons r2
        , isHexDigit a && isHexDigit b ->
            fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
      Just (c, rest) -> B.unpack (TE.encodeUtf8 (T.singleton c)) ++ go rest
