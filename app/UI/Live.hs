-- | Live RTS telemetry panel, process auto-attach, and remote commands.
module UI.Live
  ( LiveState (..)
  , newLiveState
  , liveLoad
  , liveTick
  , liveAttach
  , liveView
  , livePanes
  , LiveData (..)
  , emptyLiveData
  , latestFrame
  , liveRun
  , liveLens
    -- * Acting on the program
  , DebugHooks (..)
  , RemoteAction (..)
  , liveRemote
  , markPrompt
  , openMarkPrompt
  , sendMark
  , commandsWhy
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_, toList)
import Data.IORef
import Data.List (sortOn, unsnoc)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import System.FilePath (takeFileName)

import Data.Text (Text)
import Data.Text qualified as T
import NanoUI

import Stronghead.Attach (AttachStep (..), attachKindName, probeAttach)
import Stronghead.Debug (DebugTarget, ghcDebugEnabled, listSockets, pauseTarget)
import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Flamegraph (fromLiveFolded)
import Stronghead.Format
import Stronghead.Live
import Stronghead.Ref
import Stronghead.Run (RunInfo (..), runFromHello, runKey, runKeysMatch, wallToProgram)
import Stronghead.Source (parseSrcLoc, srcLocSpan)
import UI.Clock
import UI.Common
import UI.Flamegraph (flamegraphView)
import UI.Link
import UI.Workspace

data LiveSubTab = LiveThreadsList | LiveStacksView | LiveFlameView | LiveProbesView
  deriving stock (Eq, Show, Enum, Bounded)

liveSubTabName :: LiveSubTab -> Text
liveSubTabName = \case
  LiveThreadsList -> "Threads"
  LiveStacksView -> "Stacks"
  LiveFlameView -> "Flamegraph"
  LiveProbesView -> "Probes"

data LiveState = LiveState
  { lvHost :: !(Var Text)
  , lvPort :: !(Var Text)
  , lvWant :: !(Var Bool)
  , lvAttempt :: !(Var Int)
  , lvStream :: !(Stream (String, Int, Int) LiveData)
  , lvEventlog :: !(Var Text)
  , lvThreadFilter :: !(Var Text)
  , lvFocus :: !(Var (Maybe Int))
    -- ^ A thread another panel asked to show.
  , lvPanes :: !Workbench
  , lvNextId :: !(Var Int)
    -- ^ For command ids and moments.
  , lvSent :: !(Var (M.Map Int Sent))
    -- ^ Commands waiting for their reply.
  , lvMessage :: !(Var Text)
    -- ^ What became of the last action, when it went wrong.
  , lvMarks :: !(Var Int)
    -- ^ Marks sent, for the next default text.
  , lvMarkOpen :: !(Var Bool)
  , lvMarkText :: !(Var Text)
  , lvMarkFocus :: !(Var Bool)
    -- ^ Put the prompt's field in focus on its next frame.
  , lvHeapOn :: !(Var (Maybe Bool))
    -- ^ Whether heap profiling is on, as far as we know.
  , lvAttachTask :: !(Task (Int, Int) [AttachStep])
  , lvAttached :: !(Var (Maybe ((Int, Int), [AttachStep])))
    -- ^ What the last attach found, by connection attempt and pid.
  , lvSnapshots :: !(Var Int)
    -- ^ Snapshots asked for, to name their markers.
  , lvSecond :: !(Var (Maybe Double))
    -- ^ When "Snapshot twice" takes its second snapshot ('uiTime').
  , lvPause :: !(Var PauseState)
  , lvResume :: !(IORef (Maybe (IO ())))
    -- ^ Resumes the program a ghc-debug pause holds.
  , lvBeaconsTask :: !(Task Int [AgentBeacon])
  , lvBeaconsTick :: !(Var Int)
  , lvSamplingStacks :: !(Var Bool)
  , lvSubTab :: !(Var LiveSubTab)
  , lvFlameZoom :: !(Var (Maybe Text))
  , lvFlameStatus :: !(Var (Maybe Text))
  , lvCachedFrames :: !(IORef (Maybe ((Int, Maybe Double, Maybe Double), [Rates], [(Word64, Word64)], M.Map Int [Maybe Text], Double)))
  }

-- | A command on its way: what it is called, and how it lands on the
-- clock.
data Sent = Sent
  { snLabel :: !Text
  , snMarker :: !Text
  , snKind :: !MomentKind
  , snRun :: !RunKey
  , snCommand :: !Command
  }
  deriving stock (Eq, Show)

data PauseState = Running | Pausing | Paused | Resuming | PauseFailed !Text
  deriving stock (Eq, Show)

data LiveData = LiveData
  { ldHello :: !(Maybe (Text, Int, Int))
  , ldInfo :: !(Maybe AgentInfo)
    -- ^ A v2/v3 agent's info line. A frame without one first means a v1
    -- agent.
  , ldOutbox :: !(Maybe Outbox)
  , ldFrames :: !(Seq Frame)
  , ldReplies :: !(M.Map Int Reply)
  , ldStacks :: !(M.Map Int LiveThreadStack)
  , ldFolded :: !(M.Map (Text, [LiveFrameEntry]) Int)
  , ldProbes :: !(M.Map Text [LiveProbeNode])
  , ldAlerts :: !(Seq LiveAlert)
  , ldClosed :: !(Maybe Text)
  }

emptyLiveData :: LiveData
emptyLiveData = LiveData Nothing Nothing Nothing Seq.empty M.empty M.empty M.empty M.empty Seq.empty Nothing

latestFrame :: LiveData -> Maybe Frame
latestFrame d = case ldFrames d of
  _ :|> f -> Just f
  _ -> Nothing

newLiveState :: IO LiveState
newLiveState = do
  lvHost <- newVar "127.0.0.1"
  lvPort <- newVar "4777"
  lvWant <- newVar False
  lvAttempt <- newVar 0
  lvStream <- newStream
  lvEventlog <- newVar ""
  lvThreadFilter <- newVar ""
  lvFocus <- newVar Nothing
  lvPanes <- newWorkbench "live" (panelPreset PLive)
  lvNextId <- newVar 1
  lvSent <- newVar M.empty
  lvMessage <- newVar ""
  lvMarks <- newVar 0
  lvMarkOpen <- newVar False
  lvMarkText <- newVar ""
  lvMarkFocus <- newVar False
  lvHeapOn <- newVar Nothing
  lvAttachTask <- newTask
  lvAttached <- newVar Nothing
  lvSnapshots <- newVar 0
  lvSecond <- newVar Nothing
  lvPause <- newVar Running
  lvResume <- newIORef Nothing
  lvBeaconsTask <- newTask
  lvBeaconsTick <- newVar 0
  lvSamplingStacks <- newVar False
  lvSubTab <- newVar LiveThreadsList
  lvFlameZoom <- newVar Nothing
  lvFlameStatus <- newVar Nothing
  lvCachedFrames <- newIORef Nothing
  pure LiveState {..}

keepFrames :: Int
keepFrames = 1200

-- | Keep the connection running; called every frame.
liveLoad :: LiveState -> NanoUI (Maybe LiveData)
liveLoad st = do
  want <- readVar (lvWant st)
  host <- readVar (lvHost st)
  port <- readVar (lvPort st)
  attempt <- readVar (lvAttempt st)
  let portN = fromMaybe 4777 (readDecimal port)
  scope $
    if want
      then
        Just
          <$> useStream (lvStream st) (T.unpack host, portN, attempt) emptyLiveData (\update ->
            connectLive (T.unpack host) portN $ \case
              LiveHello p pid caps -> update (\d -> d {ldHello = Just (p, pid, caps), ldClosed = Nothing})
              LiveInfo i -> update (\d -> d {ldInfo = Just i})
              LiveConnected box -> update (\d -> d {ldOutbox = Just box})
              LiveFrame f -> update (\d -> d {ldFrames = Seq.drop (Seq.length (ldFrames d) + 1 - keepFrames) (ldFrames d |> f)})
              LiveReply i r -> update (\d -> d {ldReplies = M.insert i r (ldReplies d)})
              LiveStack ts -> update (\d -> d {ldStacks = M.insert (ltsTid ts) ts (ldStacks d)})
              LiveFolded fs -> update (\d -> d {ldFolded = M.insertWith (+) (lfsStatus fs, lfsFrames fs) (lfsCount fs) (ldFolded d)})
              LiveProbe pn -> update (\d -> d {ldProbes = M.insertWith (\new old -> if lpnId pn == 0 then new else old ++ new) (lpnProbe pn) [pn] (ldProbes d)})
              LiveAlertEv al -> update (\d -> d {ldAlerts = Seq.drop (Seq.length (ldAlerts d) + 1 - 200) (ldAlerts d |> al)})
              LiveEventlogChunk _ _ -> pure ()
              LiveClosed why -> update (\d -> d {ldClosed = Just why, ldOutbox = Nothing}))
      else pure Nothing

-- | Once per connection, when a v2 agent's info line arrives, look for the
-- program's files. Returns what was found on the frame it is known, for
-- the shell to open; 'lvAttached' keeps it for the panel to show.
liveAttach :: LiveState -> Maybe LiveData -> NanoUI (Maybe [AttachStep])
liveAttach st mdata = do
  attempt <- readVar (lvAttempt st)
  scope $ case mdata of
    Just d | Just (p, pid, _) <- ldHello d, Just info <- ldInfo d -> do
      let key = (attempt, pid)
      found <- useTask (lvAttachTask st) key (probeAttach p pid info ghcDebugEnabled listSockets)
      applied <- readVar (lvAttached st)
      case found of
        Just steps | fmap fst applied /= Just key -> do
          writeVar (lvAttached st) (Just (key, steps))
          heapOn <- readVar (lvHeapOn st)
          when (isNothing heapOn) (writeVar (lvHeapOn st) (Just (isJust (aiHeapProfile info))))
          pure (Just steps)
        _ -> pure Nothing
    _ -> pure Nothing

-- | Take up the agent's replies (each stamps its moment in program time)
-- and take the second of "Snapshot twice" when it is due. Called every
-- frame, whatever is showing.
liveTick :: Bus -> LiveState -> Maybe LiveData -> Maybe Timeline -> DebugHooks -> NanoUI ()
liveTick bus st mdata logTl hooks = do
  sent <- readVar (lvSent st)
  for_ mdata $ \d -> do
    let answered = M.intersectionWith (,) sent (ldReplies d)
    unless (M.null answered) $ do
      writeVar (lvSent st) (sent `M.difference` answered)
      forM_ (M.toList answered) $ \(i, (sn, rep)) -> case rep of
        ReplyOk (Just t) -> placeMoment bus (Moment i (snRun sn) t (snMarker sn) (snKind sn) True False)
        ReplyOk Nothing -> pure ()
        ReplyError why -> do
          modifyVar (busMoments bus) (filter ((/= i) . moId))
          writeVar (lvMessage st) (snLabel sn <> " failed: " <> why)
          case snCommand sn of
            CmdHeapProfiling on -> writeVar (lvHeapOn st) (Just (not on))
            _ -> pure ()
  second <- readVar (lvSecond st)
  for_ second $ \at -> do
    now <- uiTime
    if now >= at && not (dhBusy hooks)
      then do
        writeVar (lvSecond st) Nothing
        snapshotNow bus st mdata logTl hooks
      else wakeAfter (max 0.05 (at - now))

-- | Add a moment, or restamp the one with its id.
placeMoment :: Bus -> Moment -> NanoUI ()
placeMoment bus mo = modifyVar (busMoments bus) $ \ms ->
  if any ((== moId mo) . moId) ms
    then [if moId m == moId mo then mo {moInLog = moInLog m} else m | m <- ms]
    else take 500 (mo : ms)

-- | Per-second rates between consecutive frames.
data Rates = Rates
  { rTime :: !Double
    -- ^ Program time, in seconds.
  , rAlloc :: !Double
  , rGCs :: !Double
  , rGCPauseMs :: !Double
  , rLive :: !Double
  , rMem :: !Double
  , rProductivity :: !Double
  , rThreads :: !Double
  }

rates :: Seq Frame -> [Rates]
rates fs =
  let xs = toList fs
   in zipWith step xs (drop 1 xs)
  where
    step a b =
      let dt = max 1e-6 (frTime b - frTime a)
          d k = stat k b - stat k a
          newGC = d "gcs" > 0
       in Rates
            { rTime = fromIntegral (frProgramNs b) / 1e9
            , rAlloc = d "allocated_bytes" / dt
            , rGCs = d "gcs" / dt
            , rGCPauseMs = if newGC then stat "gc_elapsed_ns" b / 1e6 else 0
            , rLive = stat "gc_live_bytes" b
            , rMem = stat "gc_mem_in_use_bytes" b
            , rProductivity = let e = d "elapsed_ns" in if e > 0 then d "mutator_elapsed_ns" / e else 0
            , rThreads = fromIntegral (length (frThreads b))
            }

liveView :: Env -> Bench -> LiveState -> Maybe LiveData -> Maybe Timeline -> [RemoteAction] -> (FilePath -> NanoUI ()) -> NanoUI ()
liveView env bench st mdata logTl remote followEventlog = columnWith (tight . gap 0 . grow) $ do
  want <- readVar (lvWant st)
  th <- uiTheme
  let i = ink th
      mDotTone = case mdata of
        Just d | isJust (ldClosed d) -> Just Danger
        Just d | not (Seq.null (ldFrames d)) -> Just Success
        _ | want -> Just Warning
        _ -> Nothing
  toolStripWrap $ do
    scope $ for_ mDotTone statusDot
    toolLabel "Agent"
    textInputConfigured defaultTextInputConfig {ticLayout = (alignMid . fixedW 140) defaultLayout} (lvHost st)
    textInputConfigured defaultTextInputConfig {ticLayout = (alignMid . fixedW 68) defaultLayout} (lvPort st)
    if want
      then whenM (toolIconButton iconUnlink "Disconnect") (writeVar (lvWant st) False)
      else whenM (styled primary (toolIconButton iconLink "Connect")) (connect st)
    scope $ for_ mdata $ \d -> do
      for_ (ldHello d) $ \(p, pid, caps) -> when (isNothing (ldClosed d)) $
        toolLabel (p <> ", pid " <> tshow pid <> ", " <> tshow caps <> " capabilities")
      for_ (ldClosed d) $ \_ -> whenM (toolIconButton iconRefresh "Reconnect") (modifyVar (lvAttempt st) (+ 1))
    flex
    toolGroup $ do
      toolLabel "Eventlog"
      textInputConfigured defaultTextInputConfig {ticPlaceholder = "the program's .eventlog", ticLayout = (alignMid . minW 180 . fixedW 220) defaultLayout} (lvEventlog st)
      path <- readVar (lvEventlog st)
      disabledWhen (T.null (T.strip path)) $
        whenM (toolIconButton iconTrace "Follow in Timeline") (followEventlog (T.unpack (T.strip path)))
  scope $ for_ mdata $ \d -> when (isNothing (ldClosed d) && not (Seq.null (ldFrames d))) $
    actionStrip st remote
  scope $ for_ mdata $ \d -> when (isNothing (ldClosed d)) (attachReport st d)
  mixed <- bnMixed bench (lvPanes st)
  case mdata of
    Just d | not (Seq.null (ldFrames d)) -> scope (dashboard env bench st d logTl)
    _ | mixed -> scope (bnDraw bench (lvPanes st) (livePanes env st mdata logTl))
    _ -> do
      tick <- readVar (lvBeaconsTick st)
      mBeacons <- useTask (lvBeaconsTask st) tick discoverAgents
      let cardStyle = panelStyle (background (inkPane i) . borderWidth 1 . borderColor (inkRule i) . cornerRadius 6)
          emptyActions = do
            whenM (styled primary (toolIconButton iconLink "Connect")) (connect st)
            whenM (toolIconButton iconRefresh "Rescan processes") (modifyVar (lvBeaconsTick st) (+ 1))
      scope $
        columnWith (grow . tight . gap 12) $ do
          case mBeacons of
            Just bs@(_ : _) ->
              columnWith (padAll 16 . padBottom 0 . tight . gap 6 . fillW) $ do
                rowWith (tight . gap 8 . alignMid . fillW) $ do
                  sectionTitle ("Discovered local Haskell processes (" <> tshow (length bs) <> ")")
                  flex
                  whenM (toolIconButton iconRefresh "Rescan") (modifyVar (lvBeaconsTick st) (+ 1))
                forM_ bs $ \b -> withKey (abPath b) $
                  styled cardStyle $
                    panelWith (padXY 12 8 . fillW) $ themed th $
                      rowWith (tight . gap 12 . alignMid . fillW) $ do
                        statusDot Success
                        labelWith fontMedium (abProgram b)
                        labelWith (fontMono . fontMuted) ("pid " <> tshow (abPid b))
                        labelWith fontMono (abHost b <> ":" <> tshow (abPort b))
                        labelWith (fillW . fontMuted . fontSizeScale 0.88) (abCwd b)
                        whenM (styled primary (buttonWith tight "Connect")) $ do
                          writeVar (lvHost st) (abHost b)
                          writeVar (lvPort st) (tshow (abPort b))
                          connect st
            _ -> pure ()
          emptyStateWith
            "Connect to a running program"
            [ "Link the program with the stronghead:agent library and wrap main. Build it -threaded, run it with +RTS -T, then press Connect."
            , "Connect finds the rest of the program by itself: run it with +RTS -l and the Timeline follows its eventlog as it is written, with -hT the heap panel follows its heap profile, and its working directory becomes a place to look for sources and dumps."
            ]
            ["import Stronghead.Agent", " ", "main = withAgent defaultAgentConfig $ do", "  ...", " ", "./prog +RTS -l -hT -i0.1 -T -RTS"]
            emptyActions

connect :: LiveState -> NanoUI ()
connect st = do
  writeVar (lvWant st) True
  modifyVar (lvAttempt st) (+ 1)
  writeVar (lvSent st) M.empty
  writeVar (lvHeapOn st) Nothing

-- | The actions, as a toolbar under the connection's, with what went wrong
-- last.
actionStrip :: LiveState -> [RemoteAction] -> NanoUI ()
actionStrip st remote = do
  th <- uiTheme
  toolStripWrap $ do
    toolLabel "Program"
    forM_ (zip [0 :: Int ..] (groupsOf remote)) $ \(gi, grp) -> withKey gi $ do
      scope $ when (gi > 0) (box (fixedWH 1 20 . alignMid) (inkRule (ink th)))
      forM_ grp $ \a -> withKey (raLabel a) $
        actionIconButton alignMid (remoteIcon (raLabel a)) (raLabel a) (Just (maybe (raLabel a) (\k -> raLabel a <> " (" <> k <> ")") (raKey a))) (raRun a)
    flex
    msg <- readVar (lvMessage st)
    scope $ unless (T.null msg) $ do
      labelWith (alignMid . fontSizeScale 0.88 . fontTone Warning) (T.take 120 msg)
      whenM (buttonWith (alignMid . padXY 6 0 . tight) "Dismiss") (writeVar (lvMessage st) "")
  where
    groupsOf as = filter (not . null) [[a | a <- as, raGroup a == g] | g <- [GroupAgent, GroupDebug]]
    remoteIcon = \case
      "Mark…" -> iconBookmark
      "Major GC" -> iconRefresh
      "Census now" -> iconLayers
      "Stop heap profiling" -> iconStop
      "Start heap profiling" -> iconPlay
      "Pause" -> iconPause
      "Resume" -> iconPlay
      _ -> iconCamera

-- | What Connect found, or how to upgrade an old agent.
attachReport :: LiveState -> LiveData -> NanoUI ()
attachReport st d = do
  th <- uiTheme
  attached <- readVar (lvAttached st)
  let oldAgent = isNothing (ldInfo d) && not (Seq.null (ldFrames d))
  if
    | oldAgent ->
        columnWith (padTop 8 . padXY 8 0 . tight . fillW) $
          callout (themeWarning th) $
            columnWith (fillW . tight . gap 4) $ do
              labelWith (fontMedium . fillW) "This program's agent is an old one (protocol v1)"
              labelWith (fillW . fontSizeScale 0.92) "It streams statistics and threads, but doesn't say where the program runs or take commands, so Connect can't follow its eventlog or send marks. Rebuild the program against the current stronghead:agent; nothing else changes."
              codeBlock ["cabal build <your-program>      (picks up the new stronghead:agent)"]
    | Just (_, steps) <- attached ->
        columnWith (padTop 8 . padBottom 2 . padXY 20 0 . tight . fillW) $
          rowWith (fillW . tight . gap 18 . wrap . lineGap 4) $ do
            labelWith (fontMuted . fontSizeScale 0.85) "Connect found"
            forM_ steps $ \s -> withKey (attachKindName (asKind s)) $ do
              (_, area) <- mouseArea tight $ rowWith (tight . gap 6 . alignBaseline) $ do
                labelWith (fontMedium . fontSizeScale 0.85) (attachKindName (asKind s))
                case asPath s of
                  Just p -> labelWith (fontMono . fontSizeScale 0.82) (T.pack (takeFileName p))
                  Nothing -> labelWith (fontMuted . fontSizeScale 0.85) "skipped"
              tooltip area (asNote s)
    | otherwise -> pure ()

dashboard :: Env -> Bench -> LiveState -> LiveData -> Maybe Timeline -> NanoUI ()
dashboard env bench st d logTl = do
  th <- uiTheme
  let frames = ldFrames d
      lastF = Seq.index frames (Seq.length frames - 1)
      latest = case frames of
        _ :|> a :|> b -> listToMaybe (rates (Seq.fromList [a, b]))
        _ -> Nothing
      statsOn = stat "rts_stats" lastF > 0
  scope $ when (not statsOn) $
    columnWith (padBottom 8 . padXY 8 0 . tight . fillW) $
      callout (themeWarning th) (label "The program was not run with +RTS -T, so only the thread list is available.")
  let lastPauseNs = stat "gc_elapsed_ns" lastF
      gcCpuShare = stat "gc_cpu_ns" lastF / max 1 (stat "cpu_ns" lastF)
      prodTone p
        | p < 0.70 = Just Danger
        | p < 0.85 = Just Warning
        | p >= 0.90 = Just Success
        | otherwise = Nothing
      pauseTone
        | lastPauseNs > 100e6 = Just Danger
        | lastPauseNs > 20e6 = Just Warning
        | otherwise = Nothing
      gcTone
        | gcCpuShare > 0.25 = Just Danger
        | gcCpuShare > 0.10 = Just Warning
        | otherwise = Nothing
  columnWith (padBottom 8 . tight . fillW) $
    readoutToned $
      concat
        [ [ readoutItem "allocation rate" (fmtRate (rAlloc r))
          , readoutItem "live after last GC" (fmtBytesD (rLive r))
          , readoutItem "memory in use" (fmtBytesD (rMem r))
          , readoutItem "collections / s" (fmtFixed 1 (rGCs r))
          , (readoutItem "productivity" (fmtPct (rProductivity r))) {roTone = prodTone (rProductivity r)}
          ]
        | Just r <- [latest]
        ]
        ++ [ (readoutItem "last pause" (fmtNs lastPauseNs)) {roTone = pauseTone}
           , readoutItem "max live" (fmtBytesD (stat "max_live_bytes" lastF))
           , readoutItem "major GCs" (fmtInt (round (stat "major_gcs" lastF) :: Int))
           , (readoutItem "GC CPU" (fmtPct gcCpuShare)) {roTone = gcTone}
           , readoutItem "threads" (tshow (length (frThreads lastF)))
           ]
  bnDraw bench (lvPanes st) (livePanes env st (Just d) logTl)

cachedLiveFrames :: LiveState -> Seq Frame -> IO ([Rates], [(Word64, Word64)], M.Map Int [Maybe Text], Double)
cachedLiveFrames st frames = do
  let n = Seq.length frames
      key = (n, frTime <$> Seq.lookup 0 frames, frTime <$> Seq.lookup (n - 1) frames)
  cached <- readIORef (lvCachedFrames st)
  case cached of
    Just (k, rs, holes, hist, sp) | k == key -> pure (rs, holes, hist, sp)
    _ -> do
      let rs = rates frames
          holes = frameGaps (toList frames)
          hist = statusHistory frames
          sp = historySpan frames
      writeIORef (lvCachedFrames st) (Just (key, rs, holes, hist, sp))
      pure (rs, holes, hist, sp)

-- | Live's panes, for whichever workbench holds them: the runtime charts
-- and the thread list. The charts show the last five minutes, or, placed
-- with the Timeline's time panes and on the program clock, the Timeline's
-- window (@site@ is every pane in the same workbench).
livePanes :: Env -> LiveState -> Maybe LiveData -> Maybe Timeline -> [Word64] -> NanoUI PaneDraw
livePanes env st mdata logTl site = case mdata of
  Just d | not (Seq.null (ldFrames d)) -> do
    th <- uiTheme
    window <- readVar (busWindow (envBus env))
    let pal = palette th
        frames = ldFrames d
        lastF = Seq.index frames (Seq.length frames - 1)
    (rs, holes, history, span'') <- liftIO (cachedLiveFrames st frames)
    let series f = [(rTime r, f r) | r <- rs]
        recent = case (rs, unsnoc rs) of
          (r0 : _, Just (_, rN)) -> (rTime r0, max (rTime r0 + 1e-3) (rTime rN))
          _ -> (0, 1)
        -- Without +RTS -T a frame's time is when it arrived, which links to
        -- nothing.
        onClock = frProgramExact lastF
        rk = liveRun d
        placed = any (`elem` site) timelineTimePanes
        shared = case window of
          Just (r, w) | placed, onClock, runKeysMatch r rk -> Just (tvStart w / 1e9, max (tvStart w / 1e9 + 1e-3) (tvEnd w / 1e9))
          _ -> Nothing
        span' = fromMaybe recent shared
    mm <- monoMetrics 0.8
    marks <- if onClock then marksFor env rk logTl else pure (Marks Nothing Nothing [] [])
    let axisOf r = Axis rk (\ns -> chartTimeX r span' (ns / 1e9)) (\x -> chartXTime r span' x * 1e9)
        inner (Rect x y w h) = Rect (x + 4) (y + 2) (max 1 (w - 8)) (max 1 (h - 6))
        -- A gap is "paused" when Stronghead paused the program in it.
        gapLabel (a, b) =
          let ours = any (\mo -> moKind mo `elem` [MomentPause, MomentSnapshot] && moNs mo + 500000000 >= a && moNs mo <= b) (tmMoments marks)
           in (if ours then "paused " else "no samples ") <> fmtSec (fromIntegral (b - a) / 1e9)
        drawGaps ax r@(Rect _ y _ h) = withClip r $ forM_ holes $ \g@(a, b) -> do
          let xa = axToX ax (fromIntegral a)
              xb = axToX ax (fromIntegral b)
          when (xb - xa >= 2) $ do
            drawRect (Rect xa y (xb - xa) h) (alpha 0.06 (pText pal))
            drawTextWith (mmFont mm) (V2 (xa + 4) (y + 22)) AlignStart AlignMiddle (gapLabel g) (pMuted pal)
        nearestPt target pts = case pts of
          [] -> Nothing
          p0 : rest -> Just (foldl' (\best p -> if abs (fst p - target) < abs (fst best - target) then p else best) p0 rest)
        chart title color f fmtY fill' =
          columnWith (tight . gap 4 . fillW . fillH) $ do
            let pts = series f
                ymax = maximum (1e-9 : map snd pts)
            inp <- askInput
            labelWith (fontMuted . fontSizeScale 0.88) title
            resp <-
              canvasConfigured
                defaultCanvasConfig
                  { canvasLayout = (fillW . fillH . minH 60) defaultLayout
                  , canvasTrackPointer = True
                  , canvasCursor = if onClock then Just (\_ _ _ -> UiCursorCrosshair) else Nothing
                  }
                $ \r@(Rect _ y _ h) -> do
                  drawLineChart pal (mmFont mm) r span' fmtY [ChartSeries title color pts fill']
                  when onClock (drawGaps (axisOf r) (inner r))
                  drawMarks pal (axisOf r) (inner r) marks
                  let mouse = inputMousePos inp
                  when (rectContains r mouse) $
                    for_ (nearestPt (chartXTime r span' (v2X mouse)) pts) $ \(ptT, ptV) -> do
                      let cx = chartTimeX r span' ptT
                          cy = y + h - 4 - realToFrac (ptV / ymax) * (h - 20)
                      withClip r $ do
                        drawStroke (V2 cx (y + 2)) (V2 cx (y + h - 4)) 1 (alpha 0.45 (pText pal))
                        drawCircle (V2 cx cy) 3.5 color
                        drawStrokeCircle (V2 cx cy) 3.5 1.2 (inkPane (ink th))
            when (respHovered resp) $ do
              let tHover = chartXTime (respRect resp) span' (v2X (inputMousePos inp))
              for_ (nearestPt tHover pts) $ \(ptT, ptV) ->
                tooltip resp (title <> ": " <> fmtY ptV <> " at " <> fmtSec ptT)
            when onClock $ do
              void (timeGestures env (axisOf (respRect resp)) resp ScrubDrag)
              canvasLinks env resp (momentAt (axisOf (respRect resp)) marks . v2X)
        counts = M.toList (M.fromListWith (+) [(statusGroup (ltStatus t), 1 :: Int) | t <- frThreads lastF])
        meta
          | isJust shared = "the Timeline's window, program time"
          | otherwise = "last " <> fmtSec (snd span' - fst span') <> if onClock then ", program time" else ", times approximate: run with +RTS -T"
    pure $ \pid ctx ->
      if
        | pid == paneLiveCharts -> Just $
            pane ctx "Runtime" (paneMeta meta) $ do
              rowWith (tight . gap 16 . fillW . fillH) $ do
                chart "Allocation rate" (pRun pal) rAlloc fmtRate True
                chart "Live bytes after GC" (pLive pal) rLive fmtBytesD True
              rowWith (padTop 6 . tight . gap 16 . fillW . fillH) $ do
                chart "GC pause at each collection" (pGCMajor pal) rGCPauseMs (\v -> fmtFixed 1 v <> " ms") False
                chart "Threads" (pWakeup pal) rThreads (fmtFixed 0) False
        | pid == paneLiveThreads -> Just $
            pane ctx "Threads" (threadsHeader st counts) $ do
              scope $ unless (Seq.null (ldAlerts d)) $ do
                let recentAlerts = take 4 (reverse (toList (ldAlerts d)))
                callout (themeWarning th) $
                  columnWith (tight . fillW . gap 3) $ do
                    labelWith (fontMedium . fontSizeScale 0.88) ("Live runtime alerts (" <> tshow (Seq.length (ldAlerts d)) <> ")")
                    forM_ recentAlerts $ \al ->
                      rowWith (tight . gap 8 . fillW . alignMid) $ do
                        badge (if laKind al == "exc" then Danger else Warning) (laKind al)
                        labelWith (fontMono . fontSizeScale 0.84) ("tid " <> tshow (laTid al))
                        labelWith (fontMedium . fontSizeScale 0.86) (laTitle al)
                        labelWith (fillW . fontMono . fontMuted . fontSizeScale 0.8) (T.take 120 (laDetail al))
              subTab <- readVar (lvSubTab st)
              case subTab of
                LiveThreadsList -> do
                  filt <- T.toLower <$> readVar (lvThreadFilter st)
                  focus <- readVar (lvFocus st)
                  let threads = sortOn ltId [t | t <- frThreads lastF, T.null filt || filt `T.isInfixOf` T.toLower (ltLabel t <> " " <> tshow (ltId t))]
                      palKey = colorToWord32 (pText pal)
                  rowWith (padXY 8 0 . tight . gap 12 . fillW) $ do
                    colHeader (fixedW 56 . alignEnd) "id"
                    colHeader fillW "label"
                    colHeader (fixedW 36 . alignEnd) "cap"
                    colHeader (fixedW historyW) ("last " <> fmtSec span'')
                    colHeader (fixedW 220) "status"
                  paneVirtualRows (grow . tight) id threads $ \t -> withKey (ltId t) $ void $ linkRow env (RThread rk (fromIntegral (ltId t))) (focus == Just (ltId t)) (padXY 8 3 . gap 12) $ do
                    let hist = M.findWithDefault [] (ltId t) history
                    numCell 56 fontMuted (tshow (ltId t))
                    cell (fillW . if T.null (ltLabel t) then fontMuted else id) (if T.null (ltLabel t) then "unlabelled" else ltLabel t)
                    numCell 36 fontMuted (maybe "" tshow (ltCap t))
                    _ <- canvasKeyed (contentKeyOf [keyPart palKey, keyPart hist]) (fixedWH historyW 12 . alignMid) (\r -> drawHistory pal r hist)
                    cell (fixedW 220 . fontTone (groupTone (statusGroup (ltStatus t)))) (ltStatus t)
                LiveStacksView -> liveStacksView env st rk (ldStacks d)
                LiveFlameView -> liveFlameView st (ldFolded d)
                LiveProbesView -> liveProbesView env (ldProbes d)
        | otherwise -> Nothing
  _ -> pure $ \pid ctx ->
    if pid `elem` [paneLiveCharts, paneLiveThreads]
      then Just (paneEmpty ctx (paneTitle pid) "Not connected to a program. Connect to its agent in Live." [])
      else Nothing

historyW :: Float
historyW = 160

-- | How many frames a thread's history shows: half a minute at the
-- agent's default cadence.
historyFrames :: Int
historyFrames = 120

-- | Each thread's status group in each of the last frames, oldest first:
-- 'Nothing' where it didn't exist.
statusHistory :: Seq Frame -> M.Map Int [Maybe Text]
statusHistory frames =
  let recent = toList (Seq.drop (Seq.length frames - historyFrames) frames)
      perFrame = [M.fromList [(ltId t, statusGroup (ltStatus t)) | t <- frThreads f] | f <- recent]
      ids = M.keys (M.unions perFrame)
   in M.fromList [(i, [M.lookup i m | m <- perFrame]) | i <- ids]

-- | One thread's status group across the last frames, oldest first.
threadStatusHistory :: Int -> Seq Frame -> [Maybe Text]
threadStatusHistory tid frames =
  let recent = toList (Seq.drop (Seq.length frames - historyFrames) frames)
   in [listToMaybe [statusGroup (ltStatus t) | t <- frThreads f, ltId t == tid] | f <- recent]

historySpan :: Seq Frame -> Double
historySpan frames =
  let recent = Seq.drop (Seq.length frames - historyFrames) frames
   in case (Seq.lookup 0 recent, Seq.lookup (Seq.length recent - 1) recent) of
        (Just a, Just b) -> frTime b - frTime a
        _ -> 0

-- | A strip of the thread's status in each frame, in the Timeline's
-- colours: running, blocked on an MVar, STM or a black hole, and anything
-- else muted. Consecutive frames with the same status group are merged
-- into one rectangle.
drawHistory :: Palette -> Rect -> [Maybe Text] -> CanvasM ()
drawHistory pal r@(Rect x y w h) hist = do
  drawRect r (pTrackBg pal)
  let n = max 1 (length hist)
      cw = w / fromIntegral (max n historyFrames)
      x0 = x + w - fromIntegral n * cw
      go _ [] = pure ()
      go !i (Nothing : rest) = go (i + 1) rest
      go !i (Just grp : rest) = do
        let (same, others) = span (== Just grp) rest
            len = 1 + length same
        drawRect (Rect (x0 + fromIntegral i * cw) (y + 1) (max 1 (fromIntegral len * cw)) (h - 2)) (groupColour pal grp)
        go (i + len) others
  go 0 hist

groupColour :: Palette -> Text -> Color
groupColour pal = \case
  "running" -> pRun pal
  "black hole" -> pBlocked pal
  "MVar" -> pBlocked pal
  "STM" -> pBlocked pal
  _ -> pRunnable pal

threadsHeader :: LiveState -> [(Text, Int)] -> NanoUI ()
threadsHeader st counts = do
  rowWith (alignMid . tight) $
    boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabContained} liveSubTabName (lvSubTab st)
  subTab <- readVar (lvSubTab st)
  scope $ when (subTab == LiveThreadsList) $
    searchInputConfigured
      defaultSearchInputConfig {sicPlaceholder = "Filter by label or id", sicLayout = (alignMid . fixedW 180) defaultLayout}
      (lvThreadFilter st)
  flex
  forM_ counts $ \(g, n) -> withKey g $ badge (groupTone g) (g <> " " <> tshow n)

liveStacksView :: Env -> LiveState -> RunKey -> M.Map Int LiveThreadStack -> NanoUI ()
liveStacksView env st rk stacks = do
  focus <- readVar (lvFocus st)
  if M.null stacks
    then hint "No live stacks captured yet. Press Capture stacks in the Program toolbar (works on Windows/macOS/Linux via GHC.Stack.CloneStack; compile with -finfo-table-map for full source frames)."
    else paneScroll (grow . tight) $ columnWith (tight . fillW . gap 8) $ do
      let ordered = case focus >>= (`M.lookup` stacks) of
            Just fts -> fts : [s | s <- M.elems stacks, ltsTid s /= ltsTid fts]
            Nothing -> M.elems stacks
      forM_ (take 32 ordered) $ \ts -> withKey ("stk" <> tshow (ltsTid ts)) $
        panelWith (padAll 8 . gap 3 . fillW) $ do
          rowWith (tight . gap 8 . alignMid . fillW) $ do
            linkText env (RThread rk (fromIntegral (ltsTid ts))) fontMedium ("Thread " <> tshow (ltsTid ts))
            badge (groupTone (statusGroup (ltsStatus ts))) (ltsStatus ts)
            flex
            labelWith (fontMuted . fontSizeScale 0.82) (tshow (length (ltsFrames ts)) <> " frames")
          if null (ltsFrames ts)
            then hint "No decoded frames (compile target with -finfo-table-map for Haskell source locations)."
            else forM_ (zip [0 :: Int ..] (take 24 (ltsFrames ts))) $ \(fi, f) -> withKey ("f" <> tshow fi) $
              rowWith (tight . gap 8 . fillW . alignMid) $ do
                numCell 24 fontMuted (tshow fi)
                cell (fontMono . fontSizeScale 0.86) (if T.null (lfeFunction f) then lfeClosureType f else lfeFunction f)
                cell (fontMono . fontMuted . fontSizeScale 0.82) (lfeModule f)
                flex
                case srcLocSpan (parseSrcLoc (lfeSrcLoc f)) of
                  Just sp -> linkText env (RSource sp) (fontMono . fontMuted . fontSizeScale 0.8) (lfeSrcLoc f)
                  Nothing -> labelWith (fontMono . fontMuted . fontSizeScale 0.8) (lfeSrcLoc f)

liveFlameView :: LiveState -> M.Map (Text, [LiveFrameEntry]) Int -> NanoUI ()
liveFlameView st folded = do
  statusFilt <- readVar (lvFlameStatus st)
  let filtBtn lbl val =
        whenM (styled (if statusFilt == val then primary else id) (buttonWith (padXY 8 2 . tight) lbl)) $
          writeVar (lvFlameStatus st) val
  rowWith (tight . gap 6 . alignMid . fillW . wrap) $ do
    labelWith (fontMuted . fontSizeScale 0.85) "Filter"
    filtBtn "All" Nothing
    filtBtn "On-CPU (running)" (Just "running")
    filtBtn "Off-CPU (MVar)" (Just "BlockedOnMVar")
    filtBtn "Off-CPU (BlackHole)" (Just "BlockedOnBlackHole")
    flex
    labelWith (fontMuted . fontSizeScale 0.82) (tshow (sum (M.elems folded)) <> " total samples")
  flamegraphView (fromLiveFolded statusFilt folded) (lvFlameZoom st)

liveProbesView :: Env -> M.Map Text [LiveProbeNode] -> NanoUI ()
liveProbesView env probes =
  if M.null probes
    then hint "No ghc-heap probes captured yet. Register values with Stronghead.Probe.probeRef / probeTVar and press Read probes in the Program toolbar."
    else paneScroll (grow . tight) $ columnWith (tight . fillW . gap 8) $
      forM_ (M.toList probes) $ \(pName, nodes) -> withKey ("pr" <> pName) $
        panelWith (padAll 8 . gap 3 . fillW) $ do
          let thunkCnt = length (filter lpnIsThunk nodes)
              totBytes = sum (map lpnBytes nodes)
          rowWith (tight . gap 8 . alignMid . fillW) $ do
            labelWith (fontMedium . fontMono) pName
            badge (if thunkCnt > 0 then Warning else Success) (tshow thunkCnt <> " thunks")
            flex
            labelWith (fontMono . fontMuted . fontSizeScale 0.84) (tshow (length nodes) <> " closures · " <> fmtBytes (fromIntegral totBytes))
          forM_ (take 80 nodes) $ \n -> withKey ("pn" <> tshow (lpnId n)) $
            rowWith (tight . gap 8 . fillW . alignMid) $ do
              let depth = max 0 (min 12 (if lpnParent n < 0 then 0 else 1 + min 11 (lpnParent n)))
              spacer (Fixed (fromIntegral depth * 10)) Fit
              cell (fontMono . fontSizeScale 0.86 . if lpnIsThunk n then fontTone Warning else id)
                (if T.null (lpnName n) then lpnType n else lpnName n)
              badge (if lpnIsThunk n then Warning else Muted) (lpnType n)
              scope $ unless (T.null (lpnDetail n)) $
                labelWith (fontMono . fontMuted . fontSizeScale 0.8) (T.take 48 (lpnDetail n))
              flex
              case srcLocSpan (parseSrcLoc (lpnSource n)) of
                Just sp -> linkText env (RSource sp) (fontMono . fontMuted . fontSizeScale 0.8) (lpnSource n)
                Nothing -> labelWith (fontMono . fontMuted . fontSizeScale 0.8) (lpnSource n)
              numCell 64 fontMuted (fmtBytes (fromIntegral (lpnBytes n)))

statusGroup :: Text -> Text
statusGroup s
  | s == "running" = "running"
  | "BlockedOnMVar" `T.isInfixOf` s = "MVar"
  | "BlockedOnSTM" `T.isInfixOf` s = "STM"
  | "BlockedOnBlackHole" `T.isInfixOf` s = "black hole"
  | "BlockedOnForeignCall" `T.isInfixOf` s = "foreign call"
  | otherwise = "blocked"

groupTone :: Text -> Tone
groupTone = \case
  "running" -> Success
  "black hole" -> Danger
  "MVar" -> Warning
  "STM" -> Warning
  _ -> Muted

-- | The run the agent describes.
liveRun :: LiveData -> RunKey
liveRun d = maybe (RunKey "?:?") (\(p, pid, _) -> runKey (runFromHello p pid)) (ldHello d)

-- ---------------------------------------------------------------------------
-- Acting on the program

-- | What the Live panel needs from the heap panel, which takes ghc-debug
-- snapshots.
data DebugHooks = DebugHooks
  { dhSnapshot :: Either Text (NanoUI Int)
    -- ^ Take a snapshot of the target, giving its number, or why not.
  , dhBusy :: !Bool
    -- ^ A snapshot is being taken.
  , dhTarget :: !(Maybe DebugTarget)
  }

data ActionGroup = GroupAgent | GroupDebug
  deriving stock (Eq, Show)

-- | Something Stronghead can do to the program, with why it can't now.
data RemoteAction = RemoteAction
  { raLabel :: !Text
  , raKey :: !(Maybe Text)
  , raGroup :: !ActionGroup
  , raRun :: Either Text (NanoUI ())
  }

-- | Why commands can't go to the program now, if they can't.
commandsWhy :: Maybe LiveData -> Maybe Text
commandsWhy = \case
  Nothing -> Just "connect to the program in Live first"
  Just d
    | isJust (ldClosed d) -> Just "the agent disconnected"
    | Just i <- ldInfo d, not (aiCommands i) -> Just "the agent doesn't take commands: it listens on a non-loopback address (set agentAcceptCommands)"
    | isNothing (ldInfo d) && not (Seq.null (ldFrames d)) -> Just "the agent is protocol v1: rebuild the program against the current stronghead:agent"
    | isNothing (ldInfo d) || isNothing (ldOutbox d) -> Just "waiting for the agent"
    | otherwise -> Nothing

-- | Every action, enabled or with its reason.
liveRemote :: Bus -> LiveState -> Maybe LiveData -> Maybe Timeline -> DebugHooks -> NanoUI [RemoteAction]
liveRemote bus st mdata logTl hooks = do
  heapOn <- readVar (lvHeapOn st)
  sampling <- readVar (lvSamplingStacks st)
  pause <- readVar (lvPause st)
  second <- readVar (lvSecond st)
  let cmdWhy = commandsWhy mdata
      info = mdata >>= ldInfo
      agent act = maybe (Right act) Left cmdWhy
      v3Why = case info of
        Just i | aiVersion i < 3 -> Just "requires stronghead:agent protocol v3"
        _ -> cmdWhy
      v3 act = maybe (Right act) Left v3Why
      heapWhy = case info of
        Just i | isNothing (aiHeapProfile i) -> Just "the program takes no heap profile: run it with +RTS -hT -i0.1"
        _ -> Nothing
      withHeap act = maybe (agent act) Left heapWhy
      send' = sendTo bus st mdata
      snapshot = case dhSnapshot hooks of
        Left why -> Left why
        Right _ | dhBusy hooks -> Left "a snapshot is being taken"
        Right _ | pause /= Running -> Left "the program is paused"
        Right _ -> Right (snapshotNow bus st mdata logTl hooks)
      pauseAction = case (dhTarget hooks, pause) of
        _ | not ghcDebugEnabled -> RemoteAction "Pause" Nothing GroupDebug (Left "built without ghc-debug: cabal build -f ghc-debug")
        (Nothing, _) -> RemoteAction "Pause" Nothing GroupDebug (Left "no ghc-debug target: link ghc-debug-stub into the program (Unix only)")
        (Just t, Running) -> RemoteAction "Pause" Nothing GroupDebug (if dhBusy hooks then Left "a snapshot is being taken" else Right (pauseProgram bus st mdata logTl t))
        (Just t, PauseFailed _) -> RemoteAction "Pause" Nothing GroupDebug (Right (pauseProgram bus st mdata logTl t))
        (_, Paused) -> RemoteAction "Resume" Nothing GroupDebug (Right (resumeProgram bus st mdata logTl))
        (_, Pausing) -> RemoteAction "Pause" Nothing GroupDebug (Left "pausing…")
        (_, Resuming) -> RemoteAction "Resume" Nothing GroupDebug (Left "resuming…")
  pure
    [ RemoteAction "Mark…" (Just "M") GroupAgent (agent (openMarkPrompt st))
    , RemoteAction "Major GC" Nothing GroupAgent (agent (send' "Major GC" CmdGC MomentAction))
    , RemoteAction "Census now" Nothing GroupAgent (withHeap (send' "Census" CmdCensus MomentAction))
    , let on = heapOn /= Just False
       in RemoteAction
            (if on then "Stop heap profiling" else "Start heap profiling")
            Nothing
            GroupAgent
            (withHeap (writeVar (lvHeapOn st) (Just (not on)) >> send' "Heap profiling" (CmdHeapProfiling (not on)) MomentAction))
    , RemoteAction "Capture stacks" Nothing GroupAgent (v3 (writeVar (lvSubTab st) LiveStacksView >> send' "Capture stacks" CmdStacks MomentAction))
    , RemoteAction
        (if sampling then "Stop stack sampler" else "Sample stacks")
        Nothing
        GroupAgent
        (v3 (unless sampling (writeVar (lvSubTab st) LiveFlameView) >> writeVar (lvSamplingStacks st) (not sampling) >> send' "Stack sampler" (CmdSampleStacks (if sampling then Nothing else Just 25)) MomentAction))
    , RemoteAction "Read probes" Nothing GroupAgent (v3 (writeVar (lvSubTab st) LiveProbesView >> send' "Read probes" (CmdProbes Nothing) MomentAction))
    , RemoteAction "Bump era" Nothing GroupAgent (v3 (send' "Bump era" CmdEraBump MomentAction))
    , RemoteAction "Snapshot" Nothing GroupDebug snapshot
    , RemoteAction
        "Census + snapshot"
        Nothing
        GroupDebug
        (case (snapshot, withHeap (pure ())) of
          (Left why, _) -> Left why
          (_, Left why) -> Left why
          (Right snap, Right _) -> Right (send' "Census" CmdCensus MomentAction >> snap))
    , RemoteAction
        "Snapshot twice, 2 s apart"
        Nothing
        GroupDebug
        (case snapshot of
          Left why -> Left why
          Right _ | isJust second -> Left "the second snapshot is still to come"
          Right snap -> Right (snap >> uiTime >>= \now -> writeVar (lvSecond st) (Just (now + 2))))
    , pauseAction
    ]

-- | Send a command, and put it on the clock at once, at the program time
-- the latest frame suggests; the reply restamps it exactly.
sendTo :: Bus -> LiveState -> Maybe LiveData -> Text -> Command -> MomentKind -> NanoUI ()
sendTo bus st mdata lbl cmd kind = for_ mdata $ \d -> for_ (ldOutbox d) $ \box -> do
  i <- readVar (lvNextId st)
  writeVar (lvNextId st) (i + 1)
  let rk = liveRun d
  for_ (commandMarker cmd) $ \marker -> do
    modifyVar (lvSent st) (M.insert i (Sent lbl marker kind rk cmd))
    est <- liftIO (estimateNow d)
    for_ est $ \t -> placeMoment bus (Moment i rk t marker kind False False)
  liftIO (sendCommand box i cmd)

-- | The program time now, from the latest frame and how long ago it came.
estimateNow :: LiveData -> IO (Maybe Word64)
estimateNow d = case latestFrame d of
  Nothing -> pure Nothing
  Just f -> do
    now <- getMonotonicTime
    pure (Just (frProgramNs f + round (max 0 (now - frTime f) * 1e9)))

-- | Stamp a moment of our own on the run's clock: through the agent when
-- it takes commands (a marker, whose reply gives the program's own time),
-- else from the followed eventlog's wall-clock anchor, else from the
-- latest frame.
stamp :: Bus -> LiveState -> Maybe LiveData -> Maybe Timeline -> Text -> MomentKind -> NanoUI ()
stamp bus st mdata logTl marker kind
  | isNothing (commandsWhy mdata) = sendTo bus st mdata marker (CmdMark marker) kind
  | otherwise = do
      i <- readVar (lvNextId st)
      writeVar (lvNextId st) (i + 1)
      fromLog <- case logTl of
        Just tl | Just anchor <- riWallClock (tlRun tl) -> do
          now <- liftIO getPOSIXTime
          pure ((runKey (tlRun tl),) <$> wallToProgram anchor (round (realToFrac now * 1e9 :: Double)))
        _ -> pure Nothing
      fromFrames <- case mdata of
        Just d -> fmap (liveRun d,) <$> liftIO (estimateNow d)
        Nothing -> pure Nothing
      case maybe fromFrames Just fromLog of
        Just (rk, t) -> placeMoment bus (Moment i rk t marker kind False False)
        Nothing -> writeVar (lvMessage st) (marker <> " isn't on the clock: connect the agent or follow the eventlog")

snapshotNow :: Bus -> LiveState -> Maybe LiveData -> Maybe Timeline -> DebugHooks -> NanoUI ()
snapshotNow bus st mdata logTl hooks = case dhSnapshot hooks of
  Left why -> writeVar (lvMessage st) ("Snapshot: " <> why)
  Right take' -> do
    -- The marker goes first: the snapshot pauses the agent with the rest
    -- of the program.
    n <- (+ 1) <$> readVar (lvSnapshots st)
    writeVar (lvSnapshots st) n
    stamp bus st mdata logTl ("stronghead: snapshot " <> tshow n) MomentSnapshot
    void take'

pauseProgram :: Bus -> LiveState -> Maybe LiveData -> Maybe Timeline -> DebugTarget -> NanoUI ()
pauseProgram bus st mdata logTl target = do
  stamp bus st mdata logTl "stronghead: paused" MomentPause
  writeVar (lvPause st) Pausing
  wake <- askWake
  liftIO . void . forkIO $ do
    r <- pauseTarget target
    case r of
      Left e -> writeVarIO (lvPause st) (PauseFailed (T.pack e))
      Right resume -> writeIORef (lvResume st) (Just resume) >> writeVarIO (lvPause st) Paused
    wake

resumeProgram :: Bus -> LiveState -> Maybe LiveData -> Maybe Timeline -> NanoUI ()
resumeProgram bus st mdata logTl = do
  writeVar (lvPause st) Resuming
  wake <- askWake
  resume <- liftIO (atomicModifyIORef' (lvResume st) (Nothing,))
  liftIO . void . forkIO $ do
    sequence_ resume
    writeVarIO (lvPause st) Running
    wake
  -- Stamped once the program runs again, so the agent can answer.
  stamp bus st mdata logTl "stronghead: resumed" MomentPause

-- | Open the mark prompt with a default text.
openMarkPrompt :: LiveState -> NanoUI ()
openMarkPrompt st = do
  n <- readVar (lvMarks st)
  writeVar (lvMarkText st) ("mark " <> tshow (n + 1))
  writeVar (lvMarkOpen st) True
  writeVar (lvMarkFocus st) True

-- | The prompt for a mark's text; Enter sends it. Drawn every frame, so M
-- works from any panel.
markPrompt :: Bus -> LiveState -> Maybe LiveData -> NanoUI ()
markPrompt bus st mdata =
  modalWith (fixedW 440) (lvMarkOpen st) "Mark the program's timeline" $
    columnWith (fillW . tight . gap 10) $ do
      labelWith (fillW . fontMuted . fontSizeScale 0.9) "The agent posts it as an eventlog marker, so it shows on every time view of the run."
      resp <- textInputConfigured' defaultTextInputConfig {ticLayout = fillW defaultLayout} (lvMarkText st)
      focus <- readVar (lvMarkFocus st)
      when focus $ do
        writeVar (lvMarkFocus st) False
        requestFocus (respId resp)
      rowWith (fillW . tight . gap 8) $ do
        flex
        whenM (toolButton "Cancel") (writeVar (lvMarkOpen st) False)
        ok <- styled primary (toolButton "Mark")
        when (ok || respSubmitted resp) $ do
          txt <- T.strip <$> readVar (lvMarkText st)
          writeVar (lvMarkOpen st) False
          sendMark bus st mdata txt

-- | Mark the program's timeline with this text.
sendMark :: Bus -> LiveState -> Maybe LiveData -> Text -> NanoUI ()
sendMark bus st mdata txt = do
  modifyVar (lvMarks st) (+ 1)
  sendTo bus st mdata "Mark" (CmdMark (if T.null txt then "mark" else txt)) MomentMark

-- ---------------------------------------------------------------------------
-- Lens

-- | What Live can show of the entities other panels name: the threads the
-- program has now, and moments of its run, when it is the run they come
-- from. Its palette entries are the actions.
liveLens :: LiveState -> Maybe LiveData -> [Moment] -> [RemoteAction] -> Lens
liveLens st mdata moments remote = case mdata of
  Just d
    | Just lastF <- latestFrame d ->
        let rk = liveRun d
            thread r t = if runKeysMatch r rk then listToMaybe [lt | lt <- frThreads lastF, ltId lt == fromIntegral t] else Nothing
            moment r t = listToMaybe [mo | mo <- moments, runKeysMatch r (moRun mo), runKeysMatch r rk, moNs mo == t]
            onClock = frProgramExact lastF
            inFrames t = case Seq.lookup 0 (ldFrames d) of
              Just f0 -> onClock && t >= frProgramNs f0 && t <= frProgramNs lastF + 1000000000
              Nothing -> False
         in (lens PLive)
              { lensShows = \case
                  RThread r t -> isJust (thread r t)
                  RInstant r t -> runKeysMatch r rk && inFrames t
                  RRange r a b -> runKeysMatch r rk && inFrames a && b > a
                  _ -> False
              , lensReveal = \case
                  RThread _ t -> do
                    writeVar (lvThreadFilter st) ""
                    writeVar (lvFocus st) (Just (fromIntegral t))
                  _ -> pure ()
              , lensTitle = \case
                  RThread r t -> (\lt -> if T.null (ltLabel lt) then "thread " <> tshow (ltId lt) else ltLabel lt <> " (" <> tshow (ltId lt) <> ")") <$> thread r t
                  _ -> Nothing
              , lensPeek = \case
                  RThread r t -> (\lt -> (threadPeek lt (threadStatusHistory (ltId lt) (ldFrames d)) (historySpan (ldFrames d))) {peekExact = Just (runNote "thread id" r rk)}) <$> thread r t
                  RInstant r t -> momentPeek <$> moment r t
                  _ -> Nothing
              , lensHits =
                  [ Hit "live thread" (if T.null (ltLabel lt) then "thread " <> tshow (ltId lt) else ltLabel lt) (ltStatus lt) (HitRef (RThread rk (fromIntegral (ltId lt))))
                  | lt <- frThreads lastF
                  ]
                    ++ actionHits st remote
                    ++ [ Hit (momentKindName (moKind mo)) (moLabel mo) (fmtNs (fromIntegral (moNs mo))) (HitRef (RInstant (moRun mo) (moNs mo)))
                       | mo <- take 200 moments
                       ]
              }
  _ -> (lens PLive) {lensHits = actionHits st remote}

actionHits :: LiveState -> [RemoteAction] -> [Hit]
actionHits st remote =
  [ Hit "program" (raLabel a) (either id (const (fromMaybe "" (raKey a))) (raRun a)) (HitRun (either (writeVar (lvMessage st)) id (raRun a)))
  | a <- remote
  ]

momentKindName :: MomentKind -> Text
momentKindName = \case
  MomentMark -> "mark"
  MomentAction -> "action"
  MomentSnapshot -> "snapshot"
  MomentPause -> "pause"

momentPeek :: Moment -> Peek
momentPeek mo =
  (peekOf [(momentKindName (moKind mo), moLabel mo)])
    { peekExact =
        Just $
          if
            | moInLog mo -> "exact: stamped by the eventlog's marker"
            | moExact mo -> "exact: stamped by the program, through the agent"
            | otherwise -> "approximate: estimated from the latest frame"
    }

threadPeek :: LiveThread -> [Maybe Text] -> Double -> Peek
threadPeek lt hist secs =
  let seen = catMaybes hist
      share g = fromIntegral (length (filter (== g) seen)) / fromIntegral (max 1 (length seen)) :: Double
      groups = nubOrd seen
   in Peek
        { peekFacts =
            [("now", ltStatus lt)]
              ++ [("capability", tshow c) | Just c <- [ltCap lt]]
              ++ [("last " <> fmtSec secs, T.intercalate ", " [g <> " " <> fmtPct (share g) | g <- sortOn (negate . share) groups]) | not (null seen)]
        , peekNotes = []
        , peekExact = Nothing
        , peekExtra =
            if null hist
              then Nothing
              else Just $ do
                th <- uiTheme
                let pal = palette th
                void (canvasKeyed (contentKeyOf [keyPart (colorToWord32 (pText pal)), keyPart hist]) (fixedWH 300 12) (\r -> drawHistory pal r hist))
        }
