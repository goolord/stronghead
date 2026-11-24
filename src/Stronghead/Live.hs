{-# LANGUAGE CPP #-}

-- | Client for the "Stronghead.Agent" TCP wire protocol (v1, v2, and v3)
-- plus local agent discovery.
--
-- Agent to client:
--
-- > H <program> <pid> <capabilities>
-- > I version=3 port=<n> cwd=<%> exe=<%> args=<%,%,...> heap-profile=<mode|none>
-- >   heap-interval-ms=<n> eventlog=<0|1> commands=<0|1> stacks=<0|1> probes=<0|1> relay=<0|1>
-- > S <key>=<value> ...
-- > T <thread-id> <status> <label...>
-- > C <thread-id> <capability>
-- > E
-- > A <id> ok [t=<ns>]
-- > A <id> error <text>
-- > K <tid> <status> <fn|mod|src|ctype;...>            (v3: CloneStack snapshot)
-- > F <status> <count> <fn|mod|src|ctype;...>          (v3: folded stack sample)
-- > P <probe> <id> <parent> <type> <name> <src> <bytes> <isThunk> <detail>
-- > X <t_ns> <tid> <kind> <title> <detail>             (v3: live alert/exception)
-- > L <offset> <base64-bytes>                          (v3: eventlog relay chunk)
--
-- Client to agent:
--
-- > <id> mark <text>                       traceMarkerIO
-- > <id> gc                                performMajorGC
-- > <id> census                            requestHeapCensus
-- > <id> heap-profiling on|off             startHeapProfTimer / stopHeapProfTimer
-- > <id> flush                             flushEventLog
-- > <id> era-bump                          incrementUserEra 1
-- > <id> stacks                            CloneStack snapshot of all live threads
-- > <id> sample-stacks on [ms] | off       background CloneStack sampler
-- > <id> probes | probe <name>             evaluate registered ghc-heap probes
-- > <id> relay-eventlog on|off             stream raw eventlog chunks over TCP
--
-- Protocol v3 is backward-compatible with v1 and v2 in both directions.
module Stronghead.Live
  ( Frame (..)
  , LiveThread (..)
  , LiveFrameEntry (..)
  , LiveThreadStack (..)
  , LiveFoldedSample (..)
  , LiveProbeNode (..)
  , LiveAlert (..)
  , LiveEvent (..)
  , AgentInfo (..)
  , AgentBeacon (..)
  , beaconsDir
  , discoverAgents
  , parseBeacon
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
  , decodeBase64
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forever)
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (digitToInt, isAlphaNum, isAscii, isHexDigit, ord)
import Data.Either (fromRight)
import Data.List (sort, sortOn, unsnoc, (!?))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTime)
import System.Directory (doesDirectoryExist, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Stronghead.Format (percentEncodeWhere, readDecimal, readSignedDecimal, readSignedDouble, tshow)
import Stronghead.TextFile (readTextFile)

data LiveThread = LiveThread
  { ltId :: !Int
  , ltStatus :: !Text
  , ltLabel :: !Text
  , ltCap :: !(Maybe Int)
    -- ^ The capability it last ran on, from a v2+ agent's @C@ line.
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

-- | One decoded Haskell STG stack frame from @GHC.Stack.CloneStack@.
data LiveFrameEntry = LiveFrameEntry
  { lfeFunction :: !Text
  , lfeModule :: !Text
  , lfeSrcLoc :: !Text
  , lfeClosureType :: !Text
  }
  deriving stock (Eq, Ord, Show)

-- | A live thread's decoded stack snapshot (@K@ line).
data LiveThreadStack = LiveThreadStack
  { ltsTid :: !Int
  , ltsStatus :: !Text
  , ltsFrames :: ![LiveFrameEntry]
  }
  deriving stock (Eq, Show)

-- | A folded statistical stack sample batch (@F@ line).
data LiveFoldedSample = LiveFoldedSample
  { lfsStatus :: !Text
  , lfsCount :: !Int
  , lfsFrames :: ![LiveFrameEntry]
  }
  deriving stock (Eq, Show)

-- | One closure node from a cross-platform @ghc-heap@ value probe (@P@ line).
data LiveProbeNode = LiveProbeNode
  { lpnProbe :: !Text
  , lpnId :: !Int
  , lpnParent :: !Int
  , lpnType :: !Text
  , lpnName :: !Text
  , lpnSource :: !Text
  , lpnBytes :: !Int
  , lpnIsThunk :: !Bool
  , lpnDetail :: !Text
  }
  deriving stock (Eq, Show)

-- | A live runtime alert such as an allocation spike or exception (@X@ line).
data LiveAlert = LiveAlert
  { laTimeNs :: !Word64
  , laTid :: !Int
  , laKind :: !Text
  , laTitle :: !Text
  , laDetail :: !Text
  }
  deriving stock (Eq, Show)

-- | What a v2/v3 agent says about where and how the program runs.
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

-- | A locally discovered running Stronghead agent beacon.
data AgentBeacon = AgentBeacon
  { abPid :: !Int
  , abProgram :: !Text
  , abHost :: !Text
  , abPort :: !Int
  , abCwd :: !Text
  , abPath :: !FilePath
  }
  deriving stock (Eq, Show)

-- | Directory where local agents publish discovery beacons.
beaconsDir :: IO FilePath
beaconsDir = do
#if defined(mingw32_HOST_OS)
  base <- fromMaybe "." <$> lookupEnv "LOCALAPPDATA"
  pure (base </> "stronghead" </> "agents")
#else
  xdg <- lookupEnv "XDG_STATE_HOME"
  case xdg of
    Just d | not (null d) -> pure (d </> "stronghead" </> "agents")
    _ -> do
      home <- fromMaybe "/tmp" <$> lookupEnv "HOME"
      pure (home </> ".stronghead" </> "agents")
#endif

parseBeacon :: FilePath -> Text -> Maybe AgentBeacon
parseBeacon path src = do
  let kvs = M.fromList [(T.strip k, T.strip (T.drop 1 v)) | l <- T.lines src, let (k, v) = T.breakOn "=" l, not (T.null v)]
  pid <- M.lookup "pid" kvs >>= readDecimal
  port <- M.lookup "port" kvs >>= readDecimal
  let prog = M.findWithDefault "unknown" "program" kvs
      host = M.findWithDefault "127.0.0.1" "host" kvs
      cwd = M.findWithDefault "" "cwd" kvs
  pure (AgentBeacon pid prog host port cwd path)

-- | Discover live local Stronghead agents, pruning stale beacons whose TCP
-- port is no longer listening.
discoverAgents :: IO [AgentBeacon]
discoverAgents = do
  dir <- beaconsDir
  ok <- doesDirectoryExist dir
  if not ok
    then pure []
    else do
      files <- fromRight [] <$> try @SomeException (listDirectory dir)
      let beaconFiles = [dir </> f | f <- files, takeExtension f == ".agent"]
      found <- forM beaconFiles \fp -> do
        eTxt <- try @SomeException (readTextFile fp)
        case eTxt of
          Left _ -> pure Nothing
          Right txt -> case parseBeacon fp txt of
            Nothing -> pure Nothing
            Just b -> do
              alive <- portReachable (T.unpack (abHost b)) (abPort b)
              if alive
                then pure (Just b)
                else do
                  _ <- try @SomeException (removeFile fp)
                  pure Nothing
      pure (sortOn (\b -> (abProgram b, abPid b)) (catMaybes found))

portReachable :: String -> Int -> IO Bool
portReachable host port =
  fromRight False <$> try @SomeException do
    addrs <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just (show port))
    case addrs of
      [] -> pure False
      addr : _ -> bracket (openSocket addr) close \sock -> do
        connect sock (addrAddress addr)
        pure True

-- | What a client may ask of the program.
data Command
  = CmdMark !Text
  | CmdGC
  | CmdCensus
  | CmdHeapProfiling !Bool
  | CmdFlush
  | CmdEraBump
  | CmdStacks
  | CmdSampleStacks !(Maybe Int)
    -- ^ @Just ms@ starts CloneStack sampling every @ms@ milliseconds; @Nothing@ stops it.
  | CmdProbes !(Maybe Text)
    -- ^ @Nothing@ queries all registered probes; @Just name@ queries one probe.
  | CmdRelayEventlog !Bool
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
  | LiveStack !LiveThreadStack
  | LiveFolded !LiveFoldedSample
  | LiveProbe !LiveProbeNode
  | LiveAlertEv !LiveAlert
  | LiveEventlogChunk !Word64 !B.ByteString
  | LiveClosed !Text
  deriving stock (Eq, Show)

data Line
  = LHello !Text !Int !Int
  | LInfo !AgentInfo
  | LStats !(Map Text Double)
  | LThread !LiveThread
  | LCap !Int !Int
  | LReply !Int !Reply
  | LStack !LiveThreadStack
  | LFolded !LiveFoldedSample
  | LProbe !LiveProbeNode
  | LAlert !LiveAlert
  | LEventlog !Word64 !B.ByteString
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
  ["K", tid, status] -> LStack (LiveThreadStack (int tid) status [])
  ["K", tid, status, frames] -> LStack (LiveThreadStack (int tid) status (parseFrames frames))
  ["F", status, cnt, frames] -> LFolded (LiveFoldedSample status (int cnt) (parseFrames frames))
  ["P", probe, nid, par, ty, nm, src, bytes, isThk, det] ->
    LProbe
      ( LiveProbeNode
          (percentDecode probe)
          (int nid)
          (int par)
          (percentDecode ty)
          (percentDecode nm)
          (percentDecode src)
          (int bytes)
          (isThk == "1")
          (percentDecode det)
      )
  ["P", probe, nid, par, ty, nm, src, bytes, isThk] ->
    LProbe
      ( LiveProbeNode
          (percentDecode probe)
          (int nid)
          (int par)
          (percentDecode ty)
          (percentDecode nm)
          (percentDecode src)
          (int bytes)
          (isThk == "1")
          ""
      )
  ["X", tns, tid, kind, title, det] ->
    LAlert (LiveAlert (fromMaybe 0 (readDecimal tns)) (int tid) (percentDecode kind) (percentDecode title) (percentDecode det))
  ["X", tns, tid, kind, title] ->
    LAlert (LiveAlert (fromMaybe 0 (readDecimal tns)) (int tid) (percentDecode kind) (percentDecode title) "")
  ["L", off, b64] -> LEventlog (fromMaybe 0 (readDecimal off)) (decodeBase64 b64)
  ["E"] -> LEnd
  _ -> LUnknown
  where
    int = fromMaybe 0 . readSignedDecimal
    dbl = fromMaybe 0 . readSignedDouble
    pairs kvs = [(k, T.drop 1 v) | kv <- kvs, let (k, v) = T.breakOn "=" kv, not (T.null v)]

parseFrames :: Text -> [LiveFrameEntry]
parseFrames raw
  | T.null raw = []
  | otherwise =
      [ case T.splitOn "|" part of
          [fn, md, sr, ct] -> LiveFrameEntry (percentDecode fn) (percentDecode md) (percentDecode sr) (percentDecode ct)
          [fn, md, sr] -> LiveFrameEntry (percentDecode fn) (percentDecode md) (percentDecode sr) ""
          _ -> LiveFrameEntry (percentDecode part) "" "" ""
      | part <- T.splitOn ";" raw
      , not (T.null part)
      ]

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
    CmdEraBump -> "era-bump"
    CmdStacks -> "stacks"
    CmdSampleStacks Nothing -> "sample-stacks off"
    CmdSampleStacks (Just ms) -> "sample-stacks on " <> tshow (max 5 ms)
    CmdProbes Nothing -> "probes"
    CmdProbes (Just name) -> "probe " <> oneLine name
    CmdRelayEventlog on -> "relay-eventlog " <> if on then "on" else "off"

-- | The eventlog marker the agent posts for a command (it trims a mark's
-- spaces); 'Nothing' for commands that post no marker.
commandMarker :: Command -> Maybe Text
commandMarker = \case
  CmdMark t -> let m = T.dropAround (== ' ') (oneLine t) in Just (if T.null m then "mark" else m)
  CmdGC -> Just "stronghead: major GC"
  CmdCensus -> Just "stronghead: heap census"
  CmdHeapProfiling on -> Just ("stronghead: heap profiling " <> if on then "on" else "off")
  CmdFlush -> Nothing
  CmdEraBump -> Just "stronghead: era"
  CmdStacks -> Nothing
  CmdSampleStacks _ -> Nothing
  CmdProbes _ -> Nothing
  CmdRelayEventlog _ -> Nothing

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
                  LStack st -> emit (LiveStack st) >> handle more s ts
                  LFolded f -> emit (LiveFolded f) >> handle more s ts
                  LProbe p -> emit (LiveProbe p) >> handle more s ts
                  LAlert a -> emit (LiveAlertEv a) >> handle more s ts
                  LEventlog off bs -> emit (LiveEventlogChunk off bs) >> handle more s ts
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
frameClock :: Double -> Map Text Double -> (Word64, Bool)
frameClock sinceConnect stats = case M.lookup "elapsed_ns" stats of
  Just ns | M.findWithDefault 0 "rts_stats" stats > 0 -> (round (ns + M.findWithDefault 0 "init_elapsed_ns" stats), True)
  _ -> (round (max 0 sinceConnect * 1e9), False)

-- | Stretches with no frames: the program (or the agent) was stopped, by
-- a ghc-debug pause or snapshot, a long collection, or a debugger.
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

-- | Decode standard Base64 emitted by the agent's @L@ eventlog relay lines.
decodeBase64 :: Text -> B.ByteString
decodeBase64 = B.pack . go . filter (/= -2) . map val . T.unpack
  where
    val c
      | c >= 'A' && c <= 'Z' = ord c - ord 'A'
      | c >= 'a' && c <= 'z' = ord c - ord 'a' + 26
      | c >= '0' && c <= '9' = ord c - ord '0' + 52
      | c == '+' = 62
      | c == '/' = 63
      | c == '=' = -1
      | otherwise = -2
    go :: [Int] -> [Word8]
    go (a : b : c : d : rest)
      | a >= 0 && b >= 0 && c >= 0 && d >= 0 =
          let n = (a `shiftL` 18) .|. (b `shiftL` 12) .|. (c `shiftL` 6) .|. d
           in fromIntegral ((n `shiftR` 16) .&. 255)
                : fromIntegral ((n `shiftR` 8) .&. 255)
                : fromIntegral (n .&. 255)
                : go rest
      | a >= 0 && b >= 0 && c >= 0 && d == -1 =
          let n = (a `shiftL` 12) .|. (b `shiftL` 6) .|. c
           in [fromIntegral ((n `shiftR` 10) .&. 255), fromIntegral ((n `shiftR` 2) .&. 255)]
      | a >= 0 && b >= 0 && c == -1 =
          [fromIntegral (((a `shiftL` 2) .|. (b `shiftR` 4)) .&. 255)]
      | otherwise = []
    go _ = []
