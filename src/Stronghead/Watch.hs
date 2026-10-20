-- | Watchers: rules on a running program's streams that act when
-- something goes wrong while nobody is looking.
--
-- The streams are the agent's frames and a followed eventlog, turned into
-- 'Observation's ('frameObservations', 'timelineObservations'). Four rules
-- are built in, each off until it is switched on:
--
-- * live bytes up across N consecutive major collections;
-- * a thread blocked longer than X ms;
-- * GC taking more than P% of the CPU for T seconds;
-- * a heap band growing for N censuses.
--
-- A rule that fires asks for its 'Response's: a mark on the program's
-- timeline (through the agent), a census, a ghc-debug snapshot, and a
-- notification. Each rule has a cooldown, the program time that must pass
-- before it fires again, and a hysteresis, how far its measure must fall
-- back before it counts as having cleared (see 'Trigger'). Evaluation is a
-- pure fold over observations ('watchStep'), so it is tested on synthetic
-- streams.
module Stronghead.Watch
  ( -- * Rules
    Trigger (..)
  , Response (..)
  , Rule (..)
  , defaultRules
  , triggerName
  , describeTrigger
  , responseName
  , setRules
  , encodeRule
  , decodeRule
    -- * Streams
  , Observation (..)
  , ThreadNow (..)
  , obsTime
  , frameObservations
  , timelineObservations
    -- * Evaluation
  , Subject (..)
  , Fired (..)
  , WatchState
  , emptyWatchState
  , watchStep
  , watchRun
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Stronghead.Eventlog.Model
import Stronghead.Format (fmtBytesD, fmtFixed, fmtNs, tshow)
import Stronghead.HeapProfile (Band (..), HeapProfile (..))
import Stronghead.Live (Frame (..), LiveThread (..), stat)

-- ---------------------------------------------------------------------------
-- Rules

-- | What a rule watches for. The hysteresis @h@ of its rule reads:
--
-- * 'LiveUp': a collection whose live bytes fell by more than @h@ of the
--   last breaks the run (and re-arms the rule); a smaller dip is noise and
--   neither breaks nor extends it.
-- * 'BlockedOver': a thread must run for @h@ of the limit before its next
--   blocked stretch counts as a new one.
-- * 'GCOver': the rule re-arms once the share falls below @(1 - h)@ of the
--   limit.
-- * 'BandGrowing': as 'LiveUp', for each band's bytes census to census.
data Trigger
  = LiveUp !Int
    -- ^ Consecutive major collections.
  | BlockedOver !Double
    -- ^ Milliseconds. Threads in a foreign call and the agent's own
    -- threads are left out: they wait on something outside the program.
  | GCOver !Double !Double
    -- ^ Percent of CPU time, for at least this many seconds.
  | BandGrowing !Int
    -- ^ Consecutive censuses.
  deriving (Eq, Show)

data Response = RespMark | RespCensus | RespSnapshot | RespNotify
  deriving (Eq, Ord, Show, Enum, Bounded)

data Rule = Rule
  { ruTrigger :: !Trigger
  , ruOn :: !Bool
  , ruResponses :: ![Response]
  , ruCooldown :: !Double
    -- ^ Seconds of program time before it fires again.
  , ruHysteresis :: !Double
    -- ^ A fraction, 0 to 1.
  }
  deriving (Eq, Show)

-- | The built-in rules, all off.
defaultRules :: [Rule]
defaultRules =
  [ Rule (LiveUp 5) False [RespNotify, RespMark] 30 0.02
  , Rule (BlockedOver 500) False [RespNotify] 10 0.5
  , Rule (GCOver 50 5) False [RespNotify, RespMark] 30 0.2
  , Rule (BandGrowing 5) False [RespNotify] 30 0.02
  ]

-- | Its name in session files.
triggerName :: Trigger -> Text
triggerName = \case
  LiveUp _ -> "live-up"
  BlockedOver _ -> "blocked"
  GCOver _ _ -> "gc-share"
  BandGrowing _ -> "band-growing"

describeTrigger :: Trigger -> Text
describeTrigger = \case
  LiveUp n -> "Live bytes up across " <> tshow n <> " major collections in a row"
  BlockedOver ms -> "A thread blocked longer than " <> fmtFixed 0 ms <> " ms"
  GCOver p s -> "GC above " <> fmtFixed 0 p <> "% of CPU for " <> fmtFixed 0 s <> " s"
  BandGrowing n -> "A heap band growing for " <> tshow n <> " censuses in a row"

responseName :: Response -> Text
responseName = \case
  RespMark -> "mark"
  RespCensus -> "census"
  RespSnapshot -> "snapshot"
  RespNotify -> "notify"

-- | The built-in rules with any saved ones in place of their kind's.
setRules :: [Rule] -> [Rule]
setRules saved = [maybe d id (lookupKind d) | d <- defaultRules]
  where
    lookupKind d = case [r | r <- saved, triggerName (ruTrigger r) == triggerName (ruTrigger d)] of
      r : _ -> Just r
      [] -> Nothing

-- | @live-up n=5 on=true responses=notify,mark cooldown=30 hysteresis=0.02@.
encodeRule :: Rule -> Text
encodeRule r =
  T.unwords $
    [triggerName (ruTrigger r)]
      ++ params (ruTrigger r)
      ++ [ "on=" <> (if ruOn r then "true" else "false")
         , "responses=" <> T.intercalate "," (map responseName (ruResponses r))
         , "cooldown=" <> num (ruCooldown r)
         , "hysteresis=" <> num (ruHysteresis r)
         ]
  where
    params = \case
      LiveUp n -> ["n=" <> tshow n]
      BlockedOver ms -> ["ms=" <> num ms]
      GCOver p s -> ["percent=" <> num p, "seconds=" <> num s]
      BandGrowing n -> ["n=" <> tshow n]
    num x = let t = fmtFixed 3 x in T.dropWhileEnd (== '.') (T.dropWhileEnd (== '0') t)

-- | Read a rule back; a missing or unreadable parameter takes the built-in
-- rule's value.
decodeRule :: Text -> Maybe Rule
decodeRule src = case T.words src of
  name : rest -> do
    d <- case [r | r <- defaultRules, triggerName (ruTrigger r) == name] of
      r : _ -> Just r
      [] -> Nothing
    let kv = M.fromList [(k, T.drop 1 v) | w <- rest, let (k, v) = T.breakOn "=" w, not (T.null v)]
        dbl k def = maybe def id (M.lookup k kv >>= readD)
        int k def = maybe def id (M.lookup k kv >>= readI)
        trigger = case ruTrigger d of
          LiveUp n -> LiveUp (max 1 (int "n" n))
          BlockedOver ms -> BlockedOver (max 1 (dbl "ms" ms))
          GCOver p s -> GCOver (clamp 1 100 (dbl "percent" p)) (max 0 (dbl "seconds" s))
          BandGrowing n -> BandGrowing (max 1 (int "n" n))
        responses = case M.lookup "responses" kv of
          Just t -> [x | w <- T.splitOn "," t, x <- [minBound .. maxBound], responseName x == w]
          Nothing -> ruResponses d
    pure
      Rule
        { ruTrigger = trigger
        , ruOn = M.lookup "on" kv == Just "true"
        , ruResponses = responses
        , ruCooldown = max 0 (dbl "cooldown" (ruCooldown d))
        , ruHysteresis = clamp 0 1 (dbl "hysteresis" (ruHysteresis d))
        }
  [] -> Nothing
  where
    readD t = case TR.double t of
      Right (x, "") -> Just x
      _ -> Nothing
    readI t = case TR.decimal t of
      Right (x, "") -> Just x
      _ -> Nothing
    clamp lo hi = max lo . min hi

-- ---------------------------------------------------------------------------
-- Streams

-- | A thread as a frame saw it.
data ThreadNow = ThreadNow
  { tnId :: !Int
  , tnLabel :: !Text
  , tnBlocked :: !(Maybe Text)
    -- ^ What it is blocked on; 'Nothing' when it isn't.
  }
  deriving (Eq, Show)

-- | Something seen on a stream, at a program time.
data Observation
  = ObsMajorGC !Word64 !Double
    -- ^ A major collection ended, leaving this many live bytes.
  | ObsThreads !Word64 ![ThreadNow]
    -- ^ Every thread, as an agent frame saw them.
  | ObsBlockedSpan !Word64 !Word64 !ThreadNow
    -- ^ A blocked span that has ended (from an eventlog): start, end, and
    -- the thread with what it was blocked on.
  | ObsGCShare !Word64 !Double !Double
    -- ^ The end of an interval, its length in seconds, and the share of
    -- CPU (0 to 1) collections took in it.
  | ObsCensus !Word64 ![(Text, Double)]
    -- ^ A heap census: each band's bytes.
  deriving (Eq, Show)

obsTime :: Observation -> Word64
obsTime = \case
  ObsMajorGC t _ -> t
  ObsThreads t _ -> t
  ObsBlockedSpan _ t _ -> t
  ObsGCShare t _ _ -> t
  ObsCensus t _ -> t

-- | What a frame adds to the one before it: the threads, the GC share of
-- the CPU time between them, and a major collection when the last
-- collection between them was one.
frameObservations :: Maybe Frame -> Frame -> [Observation]
frameObservations prev f =
  ObsThreads t [ThreadNow (ltId lt) (ltLabel lt) (blockedOn (ltStatus lt)) | lt <- frThreads f]
    : case prev of
      Nothing -> []
      Just p ->
        let d k = stat k f - stat k p
            secs = fromIntegral (frProgramNs f - min (frProgramNs f) (frProgramNs p)) / 1e9
         in [ObsGCShare t secs (d "gc_cpu_ns" / d "cpu_ns") | d "cpu_ns" > 0, secs > 0]
              ++ [ObsMajorGC t (stat "gc_live_bytes" f) | d "major_gcs" > 0, stat "gc_gen" f >= 1]
  where
    t = frProgramNs f
    blockedOn s
      | s `elem` ["running", "finished", "died"] = Nothing
      | otherwise = Just (fromMaybe s (T.stripPrefix "BlockedOn" s))

-- | What an eventlog adds after @since@, up to @upto@ (both program time):
-- the major collections that ended, the blocked spans that ended, the
-- censuses, and the GC share of each whole second (aligned to the log's
-- start) in between.
timelineObservations :: Timeline -> Word64 -> Word64 -> [Observation]
timelineObservations tl since upto =
  sortOn obsTime (gcs ++ spans ++ censuses ++ shares)
  where
    inside t = t > since && t <= upto
    gcs =
      [ ObsMajorGC (gcEnd g) (maybe 0 fromIntegral (seriesValueAt (tlLive tl) (gcEnd g)))
      | g <- V.toList (tlGCs tl)
      , gcGen g >= 1
      , inside (gcEnd g)
      ]
    spans =
      [ ObsBlockedSpan (srStart s) (srEnd s) (ThreadNow (fromIntegral (tiId ti)) (fromMaybe "" (tiLabel ti)) (Just (stopName (srStop s))))
      | ti <- M.elems (tlThreads tl)
      , let tr = tiTrack ti
      , i <- [firstSpanEndingAfter tr since .. trackLength tr - 1]
      , let s = trackSpan tr i
      , srKind s == kindBlocked
      , inside (srEnd s)
      ]
    censuses = case tlHeapProfile tl of
      Just hp ->
        [ ObsCensus t [(bandName b, bandValues b VU.! i) | b <- V.toList (hpBands hp), i < VU.length (bandValues b)]
        | i <- [0 .. VU.length (hpTimes hp) - 1]
        , let t = round (hpTimes hp VU.! i * 1e9)
        , inside t
        ]
      Nothing -> []
    second = 1000000000 :: Word64
    caps = max 1 (V.length (tlCaps tl))
    -- Whole seconds from the log's start, ending after @since@ and by
    -- @upto@.
    firstEnd = max 1 ((since - min since (tlStart tl)) `div` second + 1)
    shares =
      [ ObsGCShare b 1 (min 1 (sum [timeIn c (fromIntegral kindGC) a b | c <- V.toList (tlCaps tl)] / (1e9 * fromIntegral caps)))
      | b <- takeWhile (<= min upto (tlEnd tl)) [tlStart tl + k * second | k <- [firstEnd ..]]
      , let a = b - second
      , inside b
      ]

-- ---------------------------------------------------------------------------
-- Evaluation

-- | What a firing is about, for its link.
data Subject
  = AboutRun
    -- ^ The program as a whole: the link is the instant.
  | AboutThread !Int !Text
    -- ^ Thread id and label.
  | AboutBand !Text
  deriving (Eq, Show)

-- | A rule that fired.
data Fired = Fired
  { fiRule :: !Int
    -- ^ Its index in the rules.
  , fiAt :: !Word64
    -- ^ Program time.
  , fiSubject :: !Subject
  , fiText :: !Text
  , fiResponses :: ![Response]
  }
  deriving (Eq, Show)

-- | What the rules remember between observations.
data WatchState = WatchState
  { wsLastFired :: !(Map Int Word64)
    -- ^ When each rule last fired.
  , wsDisarmed :: !(Set (Int, Text))
    -- ^ Rules, and what they fired about, that wait to clear before they
    -- fire again.
  , wsLive :: !(Maybe Double)
    -- ^ Live bytes after the last major collection that counted.
  , wsLiveRun :: !Int
  , wsBlocked :: !(Map Int Word64)
    -- ^ Since when each thread has been seen blocked, by frames.
  , wsRunning :: !(Map Int Word64)
    -- ^ Since when each thread has been seen not blocked, by frames.
  , wsSpanEnd :: !(Map Int Word64)
    -- ^ When each thread's last blocked span ended, from the eventlog.
  , wsGCSince :: !(Maybe Word64)
    -- ^ Since when the GC share has been over the limit.
  , wsBands :: !(Map Text (Double, Int))
    -- ^ Each band's bytes at the last census that counted, and its run of
    -- growing censuses.
  }
  deriving (Eq, Show)

emptyWatchState :: WatchState
emptyWatchState = WatchState M.empty S.empty Nothing 0 M.empty M.empty M.empty Nothing M.empty

-- | Fold observations through the rules, oldest first.
watchRun :: [Rule] -> [Observation] -> WatchState -> (WatchState, [Fired])
watchRun rules obs st0 = go st0 [] obs
  where
    go st acc = \case
      [] -> (st, reverse acc)
      o : os -> let (st', fs) = watchStep rules o st in go st' (reverse fs ++ acc) os

-- | One observation through every rule. A rule that is off still keeps
-- track, so it fires on what it sees from the moment it is switched on.
watchStep :: [Rule] -> Observation -> WatchState -> (WatchState, [Fired])
watchStep rules o st0 = foldl' one (st0, []) (zip [0 ..] rules)
  where
    one (st, acc) (i, r) = let (st', fs) = applyRule i r o st in (st', acc ++ fs)

applyRule :: Int -> Rule -> Observation -> WatchState -> (WatchState, [Fired])
applyRule i r o st = case (ruTrigger r, o) of
  (LiveUp n, ObsMajorGC t v) ->
    let (live, run, cleared) = case wsLive st of
          Nothing -> (v, 0, False)
          Just prev
            | v > prev -> (v, wsLiveRun st + 1, False)
            | v < prev * (1 - h) -> (v, 0, True)
            | otherwise -> (prev, wsLiveRun st, False)
        st1 = (if cleared then rearm "" else id) st {wsLive = Just live, wsLiveRun = run}
     in fireIf (run >= n) t "" AboutRun ("Live bytes rose across " <> tshow run <> " major collections in a row, to " <> fmtBytesD v) st1
  (BlockedOver ms, ObsThreads t threads) ->
    let limit = round (ms * 1e6) :: Word64
        settle = round (ms * 1e6 * h) :: Word64
        present = S.fromList (map tnId threads)
        st1 =
          st
            { wsBlocked = M.restrictKeys (wsBlocked st) present
            , wsRunning = M.restrictKeys (wsRunning st) present
            , wsDisarmed = S.filter (\(j, k) -> j /= i || maybe True (`S.member` present) (readKey k)) (wsDisarmed st)
            }
        track s th = case tnBlocked th of
          Just _ -> s {wsBlocked = M.insertWith (\_ old -> old) (tnId th) t (wsBlocked s), wsRunning = M.delete (tnId th) (wsRunning s)}
          Nothing ->
            let since = M.findWithDefault t (tnId th) (wsRunning s)
                s' = s {wsRunning = M.insert (tnId th) since (wsRunning s), wsBlocked = M.delete (tnId th) (wsBlocked s)}
             in if t - min t since >= settle then rearm (tshow (tnId th)) s' else s'
        st2 = foldl' track st1 threads
        -- The longest blocked first, so a cooldown keeps the worst.
        candidates =
          sortOn fst [(since, th) | th <- threads, contends th, let since = M.findWithDefault t (tnId th) (wsBlocked st2), t - min t since >= limit]
        fire (s, acc) (since, th) =
          let (s', fs) = fireIf True t (tshow (tnId th)) (AboutThread (tnId th) (tnLabel th)) (threadName' th <> " blocked on " <> fromMaybe "?" (tnBlocked th) <> " for " <> fmtNs (fromIntegral (t - since))) s
           in (s', acc ++ fs)
     in foldl' fire (st2, []) candidates
  (BlockedOver ms, ObsBlockedSpan a b th) ->
    let limit = round (ms * 1e6) :: Word64
        settle = round (ms * 1e6 * h) :: Word64
        key = tshow (tnId th)
        gapOk = maybe True (\e -> a >= e && a - e >= settle) (M.lookup (tnId th) (wsSpanEnd st))
        st1 = (if gapOk then rearm key else id) st {wsSpanEnd = M.insert (tnId th) b (wsSpanEnd st)}
     in fireIf (contends th && b - min b a >= limit) b key (AboutThread (tnId th) (tnLabel th)) (threadName' th <> " was blocked on " <> fromMaybe "?" (tnBlocked th) <> " for " <> fmtNs (fromIntegral (b - min b a))) st1
  (GCOver p secs, ObsGCShare t len share)
    | share * 100 > p ->
        let since = fromMaybe (t - min t (round (len * 1e9))) (wsGCSince st)
            st1 = st {wsGCSince = Just since}
            lasted = fromIntegral (t - min t since) / 1e9 :: Double
         in fireIf (lasted >= secs) t "" AboutRun ("GC took " <> fmtFixed 0 (share * 100) <> "% of the CPU for " <> fmtFixed 1 lasted <> " s") st1
    | share * 100 < p * (1 - h) -> (rearm "" st {wsGCSince = Nothing}, [])
    | otherwise -> (st {wsGCSince = Nothing}, [])
  (BandGrowing n, ObsCensus t bands) ->
    let total = sum (map snd bands)
        track (s, ups) (name, v) =
          let (pv, run) = M.findWithDefault (0, 0) name (wsBands s)
              (v', run', cleared)
                | pv <= 0 = (v, 0, False)
                | v > pv = (v, run + 1, False)
                | v < pv * (1 - h) = (v, 0, True)
                | otherwise = (pv, run, False)
              s' = (if cleared then rearm name else id) s {wsBands = M.insert name (v', run') (wsBands s)}
           in (s', if run' >= n && v >= 0.01 * total then (name, v, run') : ups else ups)
        (st1, growing) = foldl' track (st, []) bands
        fire (s, acc) (name, v, run) =
          let (s', fs) = fireIf True t name (AboutBand name) (name <> " grew for " <> tshow run <> " censuses in a row, to " <> fmtBytesD v) s
           in (s', acc ++ fs)
     in foldl' fire (st1, []) (sortOn (\(_, v, _) -> negate v) growing)
  _ -> (st, [])
  where
    h = ruHysteresis r
    rearm k s = s {wsDisarmed = S.delete (i, k) (wsDisarmed s)}
    cooled t s = case M.lookup i (wsLastFired s) of
      Just lastT | t >= lastT -> fromIntegral (t - lastT) >= ruCooldown r * 1e9
      _ -> True
    fireIf cond t k subj txt s
      | cond && ruOn r && not (S.member (i, k) (wsDisarmed s)) && cooled t s =
          ( s {wsLastFired = M.insert i t (wsLastFired s), wsDisarmed = S.insert (i, k) (wsDisarmed s)}
          , [Fired i t subj txt (ruResponses r)]
          )
      | otherwise = (s, [])
    readKey k = case TR.decimal k of
      Right (x, "") -> Just (x :: Int)
      _ -> Nothing
    threadName' th = if T.null (tnLabel th) then "thread " <> tshow (tnId th) else tnLabel th
    -- A thread in a foreign call waits on the outside world, not on the
    -- program, and the agent's own threads (labelled @stronghead-@) wait
    -- on Stronghead.
    contends th = case tnBlocked th of
      Just why -> not (T.isInfixOf "oreign" why) && not ("stronghead-" `T.isPrefixOf` tnLabel th)
      Nothing -> False
