-- | Eventlog ingestion.
--
-- Events are folded one at a time into a 'Builder' as they come off
-- @ghc-events@' incremental decoder, so the same code reads a finished file
-- (reporting progress as it goes) and follows a log that a running program
-- is still writing (@+RTS -l -ol<file>@, flushed by the agent or by
-- 'Debug.Trace.flushEventLog').
--
-- An eventlog is a sequence of per-capability blocks, so events are only
-- ordered within a capability. Everything the builder tracks per capability
-- (run and GC spans) is safe to fold in file order; everything global (GC
-- statistics, heap counters, heap samples, per-thread history, a profiled
-- build's cost-centre samples, ticky counters) is collected and sorted when
-- a snapshot is taken.
module Stronghead.Eventlog
  ( Builder
  , emptyBuilder
  , feedEvent
  , snapshot
  , LoadProgress (..)
  , streamEventlog
  , loadEventlog
  , forceTimeline
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, evaluate, try)
import Control.Monad (when)
import Data.Bits (testBit)
import Data.Foldable (for_)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Word (Word32, Word64, Word8)
import GHC.Clock (getMonotonicTime)
import System.IO

import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import GHC.RTS.Events
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)

import Stronghead.Eventlog.Model
import Stronghead.Format (readHex, tshow)
import Stronghead.HeapProfile (HeapProfile (..), InfoProv (..), closureTypeKind, fromSamples)
import Stronghead.Run
import Stronghead.Source (parseSrcLoc)

-- ---------------------------------------------------------------------------
-- Builder state

data Open
  = NoOpen
  | OpenRun !Word64 !Word32
  | OpenGC !Word64

data CapB = CapB
  { cbOpen :: !Open
  , cbSpans :: ![SpanRec]
    -- ^ Newest first.
  , cbPoints :: ![(Word64, Word8, Word32)]
  }

emptyCap :: CapB
emptyCap = CapB NoOpen [] []

data RunRec = RunRec
  { rrStart :: !Word64
  , rrEnd :: !Word64
  , rrCap :: !Int
  , rrStop :: !StopCode
  , rrAux :: !Word32
  }

data ThreadB = ThreadB
  { tbLabel :: !(Maybe Text)
  , tbCreated :: !(Maybe Word64)
  , tbFinished :: !(Maybe Word64)
  , tbRuns :: ![RunRec]
  , tbWakeups :: ![(Word64, Word32)]
    -- ^ Newest first.
  }

emptyThread :: ThreadB
emptyThread = ThreadB Nothing Nothing Nothing [] []

data GCStat = GCStat !Word64 !Int !Word64 !Int

data HeapB = HeapB
  { hbBreakdown :: !(Maybe Text)
  , hbCostCentres :: !(IntMap CostCentreInfo)
  , hbCur :: !(Maybe (Word64, [(Text, Double)]))
  , hbDone :: ![(Word64, [(Text, Double)])]
  , hbIPE :: !(M.Map Word64 InfoProv)
  }

data Builder = Builder
  { bCaps :: !(IntMap CapB)
  , bThreads :: !(IntMap ThreadB)
  , bGCStats :: ![GCStat]
  , bAlloc :: ![(Word64, Word64)]
  , bLive :: ![(Word64, Word64)]
  , bSize :: ![(Word64, Word64)]
  , bMarkers :: ![Marker]
  , bHeap :: !HeapB
  , bInfo :: ![(Text, Text)]
  , bRun :: !RunInfo
  , bSamples :: ![(Word64, Int, VU.Vector Word32)]
    -- ^ Cost-centre samples: time, capability, stack. Newest first.
  , bTickyDefs :: !(M.Map Word64 (Text, Int, Text, Word64))
    -- ^ Ticky counters by id: name, arity, argument kinds, info table.
  , bTickySums :: !(M.Map Word64 [(Word64, Word64, Word64, Word64)])
    -- ^ Each counter's samples: time, entries, allocation, allocated.
    -- Newest first.
  , bCount :: !Int
  , bMin :: !Word64
  , bMax :: !Word64
  }

emptyBuilder :: Builder
emptyBuilder =
  Builder IM.empty IM.empty [] [] [] [] [] (HeapB Nothing IM.empty Nothing [] M.empty) [] emptyRunInfo [] M.empty M.empty 0 maxBound 0

-- ---------------------------------------------------------------------------
-- Folding events

feedEvent :: Builder -> Event -> Builder
feedEvent b0 ev =
  let t = evTime ev
      b = b0 {bCount = bCount b0 + 1, bMin = min (bMin b0) t, bMax = max (bMax b0) t}
   in case evCap ev of
        Just c -> capEvent b c t (evSpec ev)
        Nothing -> globalEvent b t (evSpec ev)

capEvent :: Builder -> Int -> Word64 -> EventInfo -> Builder
capEvent b c t = \case
  RunThread tid ->
    onCap (\cb -> (closeOpen t 0 0 cb) {cbOpen = OpenRun t tid}) b
  StopThread tid st ->
    let code = stopCode st
        aux = case st of
          BlockedOnBlackHoleOwnedBy owner -> owner
          _ -> 0
        cb = IM.findWithDefault emptyCap c (bCaps b)
     in case cbOpen cb of
          OpenRun s tid'
            | tid' == tid ->
                let b' = onCap (const cb {cbOpen = NoOpen, cbSpans = SpanRec s t kindRun tid code aux : cbSpans cb}) b
                 in onThread tid (\tb -> tb {tbRuns = RunRec s t c code aux : tbRuns tb, tbFinished = if code == 5 then Just t else tbFinished tb}) b'
          _ -> onThread tid (\tb -> tb {tbFinished = if code == 5 then Just t else tbFinished tb}) b
  StartGC -> onCap (\cb -> (closeOpen t 1 0 cb) {cbOpen = OpenGC t}) b
  EndGC -> onCap (closeOpen t 0 0) b
  CreateThread tid -> onThread tid (\tb -> tb {tbCreated = Just t}) b
  ThreadLabel tid lbl -> onThread tid (\tb -> tb {tbLabel = Just lbl}) b
  MigrateThread tid _ -> addPoint pointMigrate tid
  SparkSteal victim -> addPoint pointSteal (fromIntegral victim)
  -- Every wakeup names its waker for the woken thread; one that crosses
  -- capabilities is also a point on the posting capability's track.
  WakeupThread tid other ->
    let waker = case cbOpen (IM.findWithDefault emptyCap c (bCaps b)) of
          OpenRun _ w -> w
          _ -> 0
        b' = onThread tid (\tb -> tb {tbWakeups = (t, waker) : tbWakeups tb}) b
     in if other /= c then addPointTo b' pointWakeup tid else b'
  UserMarker name -> b {bMarkers = Marker t name (Just c) True : bMarkers b}
  UserMessage m -> b {bMarkers = Marker t m (Just c) False : bMarkers b}
  other -> globalEvent b t other
  where
    onCap f bb = bb {bCaps = IM.alter (Just . f . fromMaybe emptyCap) c (bCaps bb)}
    addPoint = addPointTo b
    addPointTo bb k x = onCap (\cb -> cb {cbPoints = (t, k, x) : cbPoints cb}) bb

-- | End whatever span is open on a capability at time @t@; a run span cut
-- short this way gets @stop@ as its end reason.
closeOpen :: Word64 -> StopCode -> Word32 -> CapB -> CapB
closeOpen t stop aux cb = case cbOpen cb of
  NoOpen -> cb
  OpenRun s tid -> cb {cbOpen = NoOpen, cbSpans = SpanRec s t kindRun tid stop aux : cbSpans cb}
  OpenGC s -> cb {cbOpen = NoOpen, cbSpans = SpanRec s t kindGC 0 0 0 : cbSpans cb}

onThread :: Word32 -> (ThreadB -> ThreadB) -> Builder -> Builder
onThread tid f b = b {bThreads = IM.alter (Just . f . fromMaybe emptyThread) (fromIntegral tid) (bThreads b)}

globalEvent :: Builder -> Word64 -> EventInfo -> Builder
globalEvent b t = \case
  GCStatsGHC {gen, copied, parNThreads} -> b {bGCStats = GCStat t gen copied parNThreads : bGCStats b}
  HeapAllocated {allocBytes} -> b {bAlloc = (t, allocBytes) : bAlloc b}
  HeapLive {liveBytes} -> b {bLive = (t, liveBytes) : bLive b}
  HeapSize {sizeBytes} -> b {bSize = (t, sizeBytes) : bSize b}
  CreateThread tid -> onThread tid (\tb -> tb {tbCreated = Just t}) b
  ThreadLabel tid lbl -> onThread tid (\tb -> tb {tbLabel = Just lbl}) b
  UserMarker name -> b {bMarkers = Marker t name Nothing True : bMarkers b}
  UserMessage m -> b {bMarkers = Marker t m Nothing False : bMarkers b}
  RtsIdentifier _ ident -> run (\r -> r {riRts = Just ident}) (info "RTS" ident)
  ProgramArgs _ as -> run (mergeRunInfo (runFromArgs as)) (info "Arguments" (T.unwords as))
  OsProcessPid _ p -> run (\r -> r {riPid = Just (fromIntegral p)}) (info "PID" (tshow p))
  -- Posted once at startup: the moment's Unix time, at program time t.
  WallClockTime {sec, nsec} ->
    run (\r -> r {riWallClock = Just (fromMaybe (WallAnchor t (sec * 1000000000 + fromIntegral nsec)) (riWallClock r))}) b
  Startup n -> info "Capabilities" (tshow n)
  HeapProfBegin {heapProfBreakdown} -> heap (\h -> h {hbBreakdown = Just (breakdownName heapProfBreakdown)})
  HeapProfCostCentre {heapProfCostCentreId, heapProfLabel, heapProfModule, heapProfSrcLoc, heapProfFlags} ->
    let i = fromIntegral heapProfCostCentreId
        cc = CostCentreInfo i heapProfLabel heapProfModule (parseSrcLoc heapProfSrcLoc) (let HeapProfFlags w = heapProfFlags in testBit w 0)
     in heap (\h -> h {hbCostCentres = IM.insert i cc (hbCostCentres h)})
  InfoTableProv {itInfo, itTableName, itClosureDesc, itTyDesc, itLabel, itModule, itSrcLoc} ->
    let prov = InfoProv itTableName itClosureDesc (closureTypeKind itClosureDesc) itTyDesc itLabel itModule (parseSrcLoc itSrcLoc)
     in heap (\h -> h {hbIPE = M.insert itInfo prov (hbIPE h)})
  -- Every capability is sampled at each profiler tick; the event names
  -- the capability rather than coming from its buffer.
  ProfSampleCostCentre {profCap, profCcsStack} -> b {bSamples = (t, fromIntegral profCap, profCcsStack) : bSamples b}
  TickyCounterDef {tickyCtrDefId, tickyCtrDefArity, tickyCtrDefKinds, tickyCtrDefName, tickyCtrInfoTbl} ->
    b {bTickyDefs = M.insert tickyCtrDefId (tickyCtrDefName, fromIntegral tickyCtrDefArity, tickyCtrDefKinds, tickyCtrInfoTbl) (bTickyDefs b)}
  TickyCounterSample {tickyCtrSampleId, tickyCtrSampleEntryCount, tickyCtrSampleAllocs, tickyCtrSampleAllocd} ->
    b {bTickySums = M.insertWith (++) tickyCtrSampleId [(t, tickyCtrSampleEntryCount, tickyCtrSampleAllocs, tickyCtrSampleAllocd)] (bTickySums b)}
  HeapProfSampleBegin {} -> heap (beginSample t)
  HeapBioProfSampleBegin {} -> heap (beginSample t)
  HeapProfSampleEnd {} -> heap endSample
  HeapProfSampleString {heapProfResidency, heapProfLabel} -> heap (addBand heapProfLabel heapProfResidency)
  HeapProfSampleCostCentre {heapProfResidency, heapProfStack} -> heap $ \h ->
    let names = [maybe ("cc" <> tshow i) (\cc -> cciModule cc <> "." <> cciLabel cc) (IM.lookup (fromIntegral i) (hbCostCentres h)) | i <- VU.toList heapProfStack]
     in addBand (T.intercalate "/" names) heapProfResidency h
  _ -> b
  where
    info k v = b {bInfo = (k, v) : filter ((/= k) . fst) (bInfo b)}
    run f bb = bb {bRun = f (bRun bb)}
    heap f = b {bHeap = f (bHeap b)}
    beginSample time h = (endSample h) {hbCur = Just (time, [])}
    endSample h = case hbCur h of
      Just s@(_, _ : _) -> h {hbCur = Nothing, hbDone = s : hbDone h}
      _ -> h {hbCur = Nothing}
    addBand name bytes h = case hbCur h of
      Just (time, bs) -> h {hbCur = Just (time, (name, fromIntegral bytes) : bs)}
      Nothing -> h

breakdownName :: HeapProfBreakdown -> Text
breakdownName = \case
  HeapProfBreakdownCostCentre -> "cost centre"
  HeapProfBreakdownModule -> "module"
  HeapProfBreakdownClosureDescr -> "closure description"
  HeapProfBreakdownTypeDescr -> "type"
  HeapProfBreakdownRetainer -> "retainer set"
  HeapProfBreakdownBiography -> "biography"
  HeapProfBreakdownClosureType -> "closure type"
  HeapProfBreakdownInfoTable -> "info table"
  HeapProfBreakdownEra -> "era"

-- ---------------------------------------------------------------------------
-- Snapshots

-- | Freeze the builder into a drawable 'Timeline'. Spans still open are
-- closed at the latest timestamp seen. Cost is linear in the events folded
-- so far.
snapshot :: Builder -> Timeline
snapshot b
  | bCount b == 0 = emptyTimeline
  | otherwise =
      Timeline
        { tlStart = start
        , tlEnd = end
        , tlCaps = tracks
        , tlCapPoints = V.fromList [VU.fromList (sortOn (\(x, _, _) -> x) (cbPoints cb)) | cb <- caps]
        , tlThreads = M.fromList [(fromIntegral tid, threadInfo end tid tb) | (tid, tb) <- IM.toList threads]
        , tlGCs = V.fromList (gcClusters (sortOn fst gcSpans) (sortOn (\(GCStat x _ _ _) -> x) (bGCStats b)))
        , tlAlloc = series (bAlloc b)
        , tlLive = series (bLive b)
        , tlHeapSize = series (bSize b)
        , tlMarkers = V.fromList (sortOn mkTime (bMarkers b))
        , tlHeapProfile = heapProfile b start
        , tlInfo = reverse (bInfo b)
        , tlRun = bRun b
        , tlCostCentres = costCentres
        , tlCostCentresByLabel = M.fromListWith (++) [(cciLabel c, [cciId c]) | c <- IM.elems costCentres]
        , tlProfSamples = samples
        , tlSamplePeriod = period
        , tlCodeRuns = mkCodeRuns period nCaps samples
        , tlSampleCounts = IM.fromListWith (+) [(fromIntegral i, 1) | s <- V.toList samples, Just i <- [psStack s VU.!? 0]]
        , tlTicky = V.fromList (sortOn (Down . tcAlloc) ticky)
        , tlEventCount = bCount b
        }
  where
    start = bMin b
    end = bMax b
    nCaps = if IM.null (bCaps b) then 0 else fst (IM.findMax (bCaps b)) + 1
    caps = [IM.findWithDefault emptyCap i (bCaps b) | i <- [0 .. nCaps - 1]]
    tracks = V.fromList [mkTrack (sortOn srStart (reverse (cbSpans (closeOpen end 0 0 cb)))) | cb <- caps]
    costCentres = hbCostCentres (bHeap b)
    -- A sample belongs to the thread running on its capability then.
    samples =
      V.fromList
        [ ProfSample t c (runningOn c t) st
        | (t, c, st) <- sortOn (\(x, _, _) -> x) (reverse (bSamples b))
        ]
    runningOn c t = case tracks V.!? c of
      Just tr | Just i <- spanIndexAt tr t, trKind tr VU.! i == kindRun -> trThread tr VU.! i
      _ -> 0
    period = samplePeriod (V.toList samples)
    ticky =
      [ TickyCounter i name arity kinds info (sum [e | (_, e, _, _) <- ss]) (sum [a | (_, _, a, _) <- ss]) (sum [d | (_, _, _, d) <- ss]) (VU.fromList [(t, e, a) | (t, e, a, _) <- ss])
      | (i, (name, arity, kinds, info)) <- M.toList (bTickyDefs b)
      , let ss = reverse (M.findWithDefault [] i (bTickySums b))
      ]
    gcSpans = [(srStart s, srEnd s) | cb <- caps, s <- cbSpans (closeOpen end 0 0 cb), srKind s == kindGC]
    series = VU.fromList . sortOn fst
    -- Runs still open at the snapshot appear in the capability tracks but
    -- not in the thread's history; add them so a busy thread does not look
    -- idle while following a live log.
    threads =
      foldl'
        (\m (ci, cb) -> case cbOpen cb of
            OpenRun s tid -> IM.adjust (\tb -> tb {tbRuns = RunRec s end ci 0 0 : tbRuns tb}) (fromIntegral tid) m
            _ -> m)
        (bThreads b)
        (zip [0 ..] caps)

threadInfo :: Word64 -> Int -> ThreadB -> ThreadInfo
threadInfo end tid tb =
  ThreadInfo
    { tiId = fromIntegral tid
    , tiLabel = tbLabel tb
    , tiCreated = tbCreated tb
    , tiFinished = tbFinished tb
    , tiRuns = length runs
    , tiRunTime = sum [rrEnd r - rrStart r | r <- runs]
    , tiBlocked = M.fromListWith (+) [(srStop g, srEnd g - srStart g) | g <- gaps]
    , tiTrack = mkTrack (sortOn srStart (gaps ++ [SpanRec (rrStart r) (rrEnd r) kindRun (fromIntegral tid) (rrStop r) (rrAux r) | r <- runs]))
    , tiCaps = IM.keys (IM.fromList [(rrCap r, ()) | r <- runs])
    , tiWakeups = VU.fromList (sortOn fst (tbWakeups tb))
    }
  where
    runs = sortOn rrStart (tbRuns tb)
    gaps = concat (zipWith gap runs (map Just (drop 1 runs) ++ [Nothing]))
    gap r next =
      let s = rrEnd r
          e = maybe (if rrStop r == 5 then s else end) rrStart next
          k = if stopIsBlocked (rrStop r) then kindBlocked else kindRunnable
       in [SpanRec s e k (fromIntegral tid) (rrStop r) (rrAux r) | e > s]

-- | Merge overlapping per-capability GC spans into one event per
-- collection, and give each the generation from the GC statistics event
-- the RTS posts as that collection ends.
gcClusters :: [(Word64, Word64)] -> [GCStat] -> [GCEvent]
gcClusters spans stats = go (merge spans) stats
  where
    merge ((s, e) : (s', e') : rest)
      | s' <= e = merge ((s, max e e') : rest)
    merge (x : rest) = x : merge rest
    merge [] = []
    go [] _ = []
    go ((s, e) : rest) st =
      let nextStart = case rest of
            (s', _) : _ -> s'
            [] -> maxBound
          (mine, later) = span (\(GCStat t _ _ _) -> t < nextStart) (dropWhile (\(GCStat t _ _ _) -> t < s) st)
          gen = foldl' (\g (GCStat _ g' _ _) -> max g g') (-1) mine
          cop = sum [c | GCStat _ _ c _ <- mine]
          par = foldl' (\p (GCStat _ _ _ n) -> max p n) 0 mine
       in GCEvent s e 0 gen cop par : go rest later

-- | Sample times are program time, in seconds; displays count from the
-- first event, as the timeline's do.
heapProfile :: Builder -> Word64 -> Maybe HeapProfile
heapProfile b start = case hbDone h of
  [] -> Nothing
  samples ->
    Just $
      ( fromSamples
          (fromMaybe "" (lookup "Arguments" (bInfo b)))
          (fromMaybe "unknown" (hbBreakdown h))
          (fromIntegral start / 1e9)
          ipe
          [(fromIntegral t / 1e9, bands) | (t, bands) <- samples]
      )
        {hpRun = bRun b}
  where
    h = bHeap b
    ipe label = readHex label >>= (`M.lookup` hbIPE h)

-- ---------------------------------------------------------------------------
-- Streaming

data LoadProgress = LoadProgress
  { lpBytes :: !Integer
  , lpTotal :: !Integer
  , lpTimeline :: !(Maybe Timeline)
  , lpError :: !(Maybe Text)
  , lpDone :: !Bool
    -- ^ The initial read reached the end of the file.
  , lpFollowing :: !Bool
  }

-- | Read an eventlog, calling @report@ with progress and timeline snapshots.
-- Snapshots are taken at doubling byte offsets while reading (so a large
-- log fills in progressively), at the end, and, when @follow@ is set, about
-- once a second whenever the file has grown. With @follow@ this never
-- returns; run it on a thread that can be cancelled.
streamEventlog :: Bool -> FilePath -> (LoadProgress -> IO ()) -> IO ()
streamEventlog follow path report = do
  opened <- try @IOException (openBinaryFile path ReadMode)
  case opened of
    Left e -> report (LoadProgress 0 0 Nothing (Just (tshow e)) True False)
    Right h -> do
      lastSnap <- newIORef (0 :: Double, 0 :: Integer)
      let progress bytes total b done = do
            tl <- forceTimeline (snapshot b)
            pure (LoadProgress bytes total (Just tl) Nothing done follow)
          finish bytes b err = do
            p <- progress bytes bytes b True
            report p {lpError = err}
            hClose h
          loop dec !b !bytes !nextSnap !reachedEnd = case dec of
            Produce ev dec' -> loop dec' (feedEvent b ev) bytes nextSnap reachedEnd
            Done _ -> finish bytes b Nothing
            Error _ err -> finish bytes b (Just (T.pack err))
            Consume k -> do
              chunk <- B.hGetSome h (1024 * 1024)
              if not (B.null chunk)
                then do
                  let bytes' = bytes + fromIntegral (B.length chunk)
                  -- Progressive snapshots only during the first read.
                  nextSnap' <-
                    if bytes' >= nextSnap && not reachedEnd
                      then do
                        total <- max bytes' <$> hFileSize h
                        report =<< progress bytes' total b False
                        pure (nextSnap * 2)
                      else pure nextSnap
                  loop (k chunk) b bytes' nextSnap' reachedEnd
                else
                  if
                    | not follow -> finish bytes b Nothing
                    | not reachedEnd -> do
                        report =<< progress bytes bytes b True
                        now <- getMonotonicTime
                        writeIORef lastSnap (now, bytes)
                        threadDelay 250000
                        loop dec b bytes nextSnap True
                    | otherwise -> do
                        now <- getMonotonicTime
                        (prevT, prevBytes) <- readIORef lastSnap
                        when (bytes /= prevBytes && now - prevT >= 1) do
                          report =<< progress bytes bytes b True
                          writeIORef lastSnap (now, bytes)
                        threadDelay 250000
                        loop dec b bytes nextSnap True
      loop decodeEventLog emptyBuilder 0 (4 * 1024 * 1024) False

-- | Evaluate everything a snapshot holds, so the thread that took it pays
-- for building it rather than the UI thread that draws it.
forceTimeline :: Timeline -> IO Timeline
forceTimeline tl = do
  _ <- evaluate tl
  V.mapM_ evaluate (tlCaps tl)
  V.mapM_ evaluate (tlCapPoints tl)
  V.mapM_ evaluate (tlGCs tl)
  V.mapM_ evaluate (tlMarkers tl)
  V.mapM_ evaluate (tlCodeRuns tl)
  _ <- evaluate (IM.size (tlSampleCounts tl))
  mapM_ evaluate (tlThreads tl)
  for_ (tlHeapProfile tl) \hp -> evaluate hp >> V.mapM_ evaluate (hpBands hp)
  pure tl

-- | Read a whole eventlog.
loadEventlog :: FilePath -> IO (Either String Timeline)
loadEventlog path = do
  result <- newIORef (Left "empty eventlog")
  streamEventlog False path $ \p ->
    case (lpError p, lpTimeline p) of
      (Just e, _) | lpDone p -> writeIORef result (Left (T.unpack e))
      (_, Just tl) | lpDone p -> writeIORef result (Right tl)
      _ -> pure ()
  readIORef result
