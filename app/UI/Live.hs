-- | A "top" for a running Haskell program: connects to the agent in
-- "Stronghead.Agent" and charts allocation, GC and residency as they
-- happen, with the live thread list.
--
-- The charts run on program time, so they are time views like the
-- Timeline's ("UI.Clock"): they show the shared cursor, range and the
-- followed eventlog's markers, a click or drag moves the cursor, and
-- Shift+drag selects a range there too.
--
-- Connect also attaches: from a v2 agent's info line it finds the
-- program's eventlog, heap profile, ghc-debug socket, sources and dumps
-- ('liveAttach'), for the shell to open. And it acts on the program
-- ('liveRemote'): marks, a major GC, a census and heap profiling on and off
-- go through the agent; snapshots and pausing go through ghc-debug. Each
-- lands on every time axis as a moment on the bus, stamped in program
-- time by the agent's reply, until the followed eventlog's own marker
-- takes over.
module UI.Live
  ( LiveState (..)
  , newLiveState
  , liveLoad
  , liveTick
  , liveAttach
  , liveView
  , LiveData (..)
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
import Data.Foldable (for_, toList)
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import NanoUI
import Stronghead.Attach (AttachStep (..), attachKindName, probeAttach)
import Stronghead.Debug (DebugTarget, ghcDebugEnabled, listSockets, pauseTarget)
import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Format
import Stronghead.Live
import Stronghead.Ref
import Stronghead.Run (RunInfo (..), runFromHello, runKey, runKeysMatch, wallToProgram)
import System.FilePath (takeFileName)
import UI.Clock
import UI.Common
import UI.Link

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
  deriving (Eq, Show)

data PauseState = Running | Pausing | Paused | Resuming | PauseFailed !Text
  deriving (Eq, Show)

data LiveData = LiveData
  { ldHello :: !(Maybe (Text, Int, Int))
  , ldInfo :: !(Maybe AgentInfo)
    -- ^ A v2 agent's info line. A frame without one first means a v1
    -- agent.
  , ldOutbox :: !(Maybe Outbox)
  , ldFrames :: !(Seq Frame)
  , ldReplies :: !(M.Map Int Reply)
  , ldClosed :: !(Maybe Text)
  }

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
  lvPanes <- newVar (Just (Split 10 AxisH 0.58 (Pane paneCharts) (Pane 2)))
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
  let portN = either (const 4777) fst (TR.decimal port)
  scope $
    if want
      then
        Just
          <$> useStream (lvStream st) (T.unpack host, portN, attempt) (LiveData Nothing Nothing Nothing Seq.empty M.empty Nothing) (\update ->
            connectLive (T.unpack host) portN $ \case
              LiveHello p pid caps -> update (\d -> d {ldHello = Just (p, pid, caps), ldClosed = Nothing})
              LiveInfo i -> update (\d -> d {ldInfo = Just i})
              LiveConnected box -> update (\d -> d {ldOutbox = Just box})
              LiveFrame f -> update (\d -> d {ldFrames = Seq.drop (Seq.length (ldFrames d) + 1 - keepFrames) (ldFrames d |> f)})
              LiveReply i r -> update (\d -> d {ldReplies = M.insert i r (ldReplies d)})
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

liveView :: Env -> LiveState -> Maybe LiveData -> Maybe Timeline -> [RemoteAction] -> (FilePath -> NanoUI ()) -> NanoUI ()
liveView env st mdata logTl remote followEventlog = columnWith (tight . gap 0 . grow) $ do
  want <- readVar (lvWant st)
  toolStrip $ do
    toolLabel "Agent"
    textInputConfigured defaultTextInputConfig {ticLayout = (alignMid . fixedW 150) defaultLayout} (lvHost st)
    textInputConfigured defaultTextInputConfig {ticLayout = (alignMid . fixedW 72) defaultLayout} (lvPort st)
    if want
      then whenM (toolButton "Disconnect") (writeVar (lvWant st) False)
      else whenM (styled primary (toolButton "Connect")) (connect st)
    scope $ for_ mdata $ \d -> do
      for_ (ldHello d) $ \(p, pid, caps) -> when (isNothing (ldClosed d)) $
        toolLabel (p <> ", pid " <> tshow pid <> ", " <> tshow caps <> " capabilities")
      for_ (ldClosed d) $ \_ -> whenM (toolButton "Reconnect") (modifyVar (lvAttempt st) (+ 1))
    flex
    toolGroup $ do
      toolLabel "Eventlog"
      textInputConfigured defaultTextInputConfig {ticPlaceholder = "the program's .eventlog", ticLayout = (alignMid . fixedW 240) defaultLayout} (lvEventlog st)
      path <- readVar (lvEventlog st)
      disabledWhen (T.null (T.strip path)) $
        whenM (toolButton "Follow in Timeline") (followEventlog (T.unpack (T.strip path)))
  scope $ for_ mdata $ \d -> when (isNothing (ldClosed d) && not (Seq.null (ldFrames d))) $
    actionStrip st remote
  scope $ for_ mdata $ \d -> when (isNothing (ldClosed d)) (attachReport st d)
  case mdata of
    Just d | not (Seq.null (ldFrames d)) -> scope (dashboard env st d logTl)
    _ ->
      scope $
        emptyState
          "Connect to a running program"
          [ "Link the program with the stronghead:agent library and wrap main. Build it -threaded, run it with +RTS -T, then press Connect."
          , "Connect finds the rest of the program by itself: run it with +RTS -l and the Timeline follows its eventlog as it is written, with -hT the heap panel follows its heap profile, and its working directory becomes a place to look for sources and dumps."
          ]
          ["import Stronghead.Agent", " ", "main = withAgent defaultAgentConfig $ do", "  ...", " ", "./prog +RTS -l -hT -i0.1 -T -RTS"]

connect :: LiveState -> NanoUI ()
connect st = do
  writeVar (lvWant st) True
  modifyVar (lvAttempt st) (+ 1)
  writeVar (lvSent st) M.empty
  writeVar (lvHeapOn st) Nothing

-- | The actions, as a toolbar under the connection's, with what went wrong
-- last.
actionStrip :: LiveState -> [RemoteAction] -> NanoUI ()
actionStrip st remote = toolStrip $ do
  toolLabel "Program"
  forM_ (zip [0 :: Int ..] (groupsOf remote)) $ \(gi, grp) -> withKey gi $ do
    scope $ when (gi > 0) (toolGroup (pure ()))
    forM_ grp $ \a -> withKey (raLabel a) $ do
      resp <- disabledWhen (isLeftE (raRun a)) (buttonWith' alignMid (raLabel a))
      tooltip resp (either id (const (maybe (raLabel a) (\k -> raLabel a <> " (" <> k <> ")") (raKey a))) (raRun a))
      when (respClicked resp) (either (const (pure ())) id (raRun a))
  flex
  msg <- readVar (lvMessage st)
  scope $ unless (T.null msg) $ do
    labelWith (alignMid . fontSizeScale 0.88 . fontTone Warning) (T.take 120 msg)
    whenM (buttonWith (alignMid . padXY 6 0 . tight) "Dismiss") (writeVar (lvMessage st) "")
  where
    groupsOf as = filter (not . null) [[a | a <- as, raGroup a == g] | g <- [GroupAgent, GroupDebug]]
    isLeftE = either (const True) (const False)

-- | What Connect found, or how to upgrade an old agent.
attachReport :: LiveState -> LiveData -> NanoUI ()
attachReport st d = do
  th <- uiTheme
  attached <- readVar (lvAttached st)
  let oldAgent = isNothing (ldInfo d) && not (Seq.null (ldFrames d))
  if
    | oldAgent ->
        columnWith (padXY 8 0 . padTop 8 . tight . fillW) $
          callout (themeWarning th) $
            columnWith (fillW . tight . gap 4) $ do
              labelWith (fontMedium . fillW) "This program's agent is an old one (protocol v1)"
              labelWith (fillW . fontSizeScale 0.92) "It streams statistics and threads, but doesn't say where the program runs or take commands, so Connect can't follow its eventlog or send marks. Rebuild the program against the current stronghead:agent; nothing else changes."
              codeBlock ["cabal build <your-program>      (picks up the new stronghead:agent)"]
    | Just (_, steps) <- attached ->
        columnWith (padXY 20 0 . padTop 8 . padBottom 2 . tight . fillW) $
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

dashboard :: Env -> LiveState -> LiveData -> Maybe Timeline -> NanoUI ()
dashboard env st d logTl = do
  th <- uiTheme
  let pal = palette th
      frames = ldFrames d
      rs = rates frames
      lastF = Seq.index frames (Seq.length frames - 1)
      latest = if null rs then Nothing else Just (last rs)
      series f = [(rTime r, f r) | r <- rs]
      span' = case (rs, reverse rs) of
        (r0 : _, rN : _) -> (rTime r0, max (rTime r0 + 1e-3) (rTime rN))
        _ -> (0, 1)
      statsOn = stat "rts_stats" lastF > 0
      -- Without +RTS -T a frame's time is when it arrived, which links to
      -- nothing.
      onClock = frProgramExact lastF
      rk = liveRun d
      holes = frameGaps (toList frames)
  scope $ when (not statsOn) $
    columnWith (padXY 8 0 . padBottom 8 . tight . fillW) $
      callout (themeWarning th) (label "The program was not run with +RTS -T, so only the thread list is available.")
  columnWith (padBottom 8 . tight . fillW) $
    readout $
      concat
        [ [ ("allocation rate", fmtRate (rAlloc r))
          , ("live after last GC", fmtBytesD (rLive r))
          , ("memory in use", fmtBytesD (rMem r))
          , ("collections per second", fmtFixed 1 (rGCs r))
          , ("productivity", fmtPct (rProductivity r))
          ]
        | Just r <- [latest]
        ]
        ++ [ ("last pause", fmtNs (stat "gc_elapsed_ns" lastF))
           , ("max live", fmtBytesD (stat "max_live_bytes" lastF))
           , ("major GCs", fmtInt (round (stat "major_gcs" lastF) :: Int))
           , ("GC CPU", fmtPct (stat "gc_cpu_ns" lastF / max 1 (stat "cpu_ns" lastF)))
           , ("threads", tshow (length (frThreads lastF)))
           ]
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
      chart title color f fmtY fill' =
        columnWith (tight . gap 4 . fillW . fillH) $ do
          labelWith (fontMuted . fontSizeScale 0.88) title
          resp <-
            canvasConfigured
              defaultCanvasConfig
                { canvasLayout = (fillW . fillH . minH 60) defaultLayout
                , canvasTrackPointer = True
                , canvasCursor = if onClock then Just (\_ _ _ -> UiCursorCrosshair) else Nothing
                }
              $ \r -> do
                drawLineChart pal (mmFont mm) r span' fmtY [ChartSeries title color (series f) fill']
                when onClock (drawGaps (axisOf r) (inner r))
                drawMarks pal (axisOf r) (inner r) marks
          when onClock $ do
            void (timeGestures env (axisOf (respRect resp)) resp ScrubDrag)
            canvasLinks env resp (momentAt (axisOf (respRect resp)) marks . v2X)
      counts = M.toList (M.fromListWith (+) [(statusGroup (ltStatus t), 1 :: Int) | t <- frThreads lastF])
  workbench (lvPanes st) $ \pid ctx ->
    if pid == paneCharts
      then pane ctx "Runtime" (paneMeta ("last " <> fmtSec (snd span' - fst span') <> if onClock then ", program time" else ", times approximate: run with +RTS -T")) $ do
        rowWith (tight . gap 16 . fillW . fillH) $ do
          chart "Allocation rate" (pRun pal) rAlloc fmtRate True
          chart "Live bytes after GC" (pLive pal) rLive fmtBytesD True
        rowWith (padTop 6 . tight . gap 16 . fillW . fillH) $ do
          chart "GC pause at each collection" (pGCMajor pal) rGCPauseMs (\v -> fmtFixed 1 v <> " ms") False
          chart "Threads" (pWakeup pal) rThreads (fmtFixed 0) False
      else pane ctx "Threads" (threadsHeader st counts) $ do
        filt <- T.toLower <$> readVar (lvThreadFilter st)
        focus <- readVar (lvFocus st)
        let threads = sortOn ltId [t | t <- frThreads lastF, T.null filt || filt `T.isInfixOf` T.toLower (ltLabel t <> " " <> tshow (ltId t))]
            history = statusHistory frames
            span'' = historySpan frames
        rowWith (padXY 8 0 . tight . gap 12 . fillW) $ do
          colHeader (fixedW 56 . alignEnd) "id"
          colHeader fillW "label"
          colHeader (fixedW 36 . alignEnd) "cap"
          colHeader (fixedW historyW) ("last " <> fmtSec span'')
          colHeader (fixedW 220) "status"
        paneScroll (grow . tight) $ columnWith (tight . fillW . gap 0) $
          forM_ threads $ \t -> withKey (ltId t) $ void $ linkRow env (RThread rk (fromIntegral (ltId t))) (focus == Just (ltId t)) (padXY 8 3 . gap 12) $ do
            numCell 56 fontMuted (tshow (ltId t))
            cell (fillW . if T.null (ltLabel t) then fontMuted else id) (if T.null (ltLabel t) then "unlabelled" else ltLabel t)
            numCell 36 fontMuted (maybe "" tshow (ltCap t))
            _ <- canvas (fixedWH historyW 12 . alignMid) (\r -> drawHistory pal r (M.findWithDefault [] (ltId t) history))
            cell (fixedW 220 . fontTone (groupTone (statusGroup (ltStatus t)))) (ltStatus t)

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

historySpan :: Seq Frame -> Double
historySpan frames =
  let recent = Seq.drop (Seq.length frames - historyFrames) frames
   in case (Seq.lookup 0 recent, Seq.lookup (Seq.length recent - 1) recent) of
        (Just a, Just b) -> frTime b - frTime a
        _ -> 0

-- | A strip of the thread's status in each frame, in the Timeline's
-- colours: running, blocked on an MVar, STM or a black hole, and anything
-- else muted.
drawHistory :: Palette -> Rect -> [Maybe Text] -> CanvasM ()
drawHistory pal r@(Rect x y w h) hist = do
  drawRect r (pTrackBg pal)
  let n = max 1 (length hist)
      cw = w / fromIntegral (max n historyFrames)
      x0 = x + w - fromIntegral n * cw
  forM_ (zip [0 :: Int ..] hist) $ \(i, g) -> for_ g $ \grp ->
    drawRect (Rect (x0 + fromIntegral i * cw) (y + 1) (max 1 cw) (h - 2)) (groupColour pal grp)

groupColour :: Palette -> Text -> Color
groupColour pal = \case
  "running" -> pRun pal
  "black hole" -> pBlocked pal
  "MVar" -> pBlocked pal
  "STM" -> pBlocked pal
  _ -> pRunnable pal

threadsHeader :: LiveState -> [(Text, Int)] -> NanoUI ()
threadsHeader st counts = do
  searchInputConfigured
    defaultSearchInputConfig {sicPlaceholder = "Filter by label or id", sicLayout = (alignMid . fixedW 220) defaultLayout}
    (lvThreadFilter st)
  flex
  forM_ counts $ \(g, n) -> withKey g $ badge (groupTone g) (g <> " " <> tshow n)

paneCharts :: Word64
paneCharts = 1

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
  deriving (Eq, Show)

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
  pause <- readVar (lvPause st)
  second <- readVar (lvSecond st)
  let cmdWhy = commandsWhy mdata
      info = mdata >>= ldInfo
      agent act = maybe (Right act) Left cmdWhy
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
estimateNow d = case Seq.lookup (Seq.length (ldFrames d) - 1) (ldFrames d) of
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
    | Just lastF <- Seq.lookup (Seq.length (ldFrames d) - 1) (ldFrames d) ->
        let rk = liveRun d
            thread r t = if runKeysMatch r rk then listToMaybe [lt | lt <- frThreads lastF, ltId lt == fromIntegral t] else Nothing
            history = statusHistory (ldFrames d)
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
                  RThread r t -> (\lt -> (threadPeek lt (M.findWithDefault [] (ltId lt) history) (historySpan (ldFrames d))) {peekExact = Just (runNote "thread id" r rk)}) <$> thread r t
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
  [ Hit "program" (raLabel a) (either id (const (maybe "" id (raKey a))) (raRun a)) (HitRun (either (writeVar (lvMessage st)) id (raRun a)))
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
      groups = M.keys (M.fromList [(g, ()) | g <- seen])
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
                void (canvas (fixedWH 300 12) (\r -> drawHistory (palette th) r hist))
        }
