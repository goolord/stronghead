-- | A "top" for a running Haskell program: connects to the agent in
-- "Stronghead.Agent" and charts allocation, GC and residency as they
-- happen, with the live thread list.
--
-- The charts run on program time, so they are time views like the
-- Timeline's ("UI.Clock"): they show the shared cursor, range and the
-- followed eventlog's markers, a click or drag moves the cursor, and
-- Shift+drag selects a range there too.
module UI.Live
  ( LiveState (..)
  , newLiveState
  , liveLoad
  , liveView
  , LiveData (..)
  , liveLens
  ) where

import Control.Monad (forM_, void, when)
import Data.Foldable (for_, toList)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Word (Word64)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import NanoUI
import Stronghead.Eventlog.Model (Timeline)
import Stronghead.Format
import Stronghead.Live
import Stronghead.Ref
import Stronghead.Run (runFromHello, runKey, runKeysMatch)
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
  }

data LiveData = LiveData
  { ldHello :: !(Maybe (Text, Int, Int))
  , ldFrames :: !(Seq Frame)
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
          <$> useStream (lvStream st) (T.unpack host, portN, attempt) (LiveData Nothing Seq.empty Nothing) (\update ->
            connectLive (T.unpack host) portN $ \case
              LiveHello p pid caps -> update (\d -> d {ldHello = Just (p, pid, caps), ldClosed = Nothing})
              LiveFrame f -> update (\d -> d {ldFrames = Seq.drop (Seq.length (ldFrames d) + 1 - keepFrames) (ldFrames d |> f)})
              LiveClosed why -> update (\d -> d {ldClosed = Just why}))
      else pure Nothing

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

liveView :: Env -> LiveState -> Maybe LiveData -> Maybe Timeline -> (FilePath -> NanoUI ()) -> NanoUI ()
liveView env st mdata logTl followEventlog = columnWith (tight . gap 0 . grow) $ do
  want <- readVar (lvWant st)
  toolStrip $ do
    toolLabel "Agent"
    textInputConfigured defaultTextInputConfig {ticLayout = (alignMid . fixedW 150) defaultLayout} (lvHost st)
    textInputConfigured defaultTextInputConfig {ticLayout = (alignMid . fixedW 72) defaultLayout} (lvPort st)
    if want
      then whenM (toolButton "Disconnect") (writeVar (lvWant st) False)
      else whenM (styled primary (toolButton "Connect")) (writeVar (lvWant st) True >> modifyVar (lvAttempt st) (+ 1))
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
  case mdata of
    Just d | not (Seq.null (ldFrames d)) -> scope (dashboard env st d logTl)
    _ ->
      scope $
        emptyState
          "Connect to a running program"
          [ "Link the program with the stronghead:agent library and wrap main. Build it -threaded, run it with +RTS -T, then press Connect."
          , "Add +RTS -l -ol<file>.eventlog and use Follow in Timeline: the agent flushes the eventlog every 250 ms, so the timeline fills in as the program runs."
          ]
          ["import Stronghead.Agent", " ", "main = withAgent defaultAgentConfig $ do", "  ..."]

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
  marks <- if onClock then marksFor env rk logTl else pure (Marks Nothing Nothing [])
  let axisOf r = Axis rk (\ns -> chartTimeX r span' (ns / 1e9)) (\x -> chartXTime r span' x * 1e9)
      inner (Rect x y w h) = Rect (x + 4) (y + 2) (max 1 (w - 8)) (max 1 (h - 6))
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
                drawMarks pal (axisOf r) (inner r) marks
          when onClock $ void (timeGestures env (axisOf (respRect resp)) resp ScrubDrag)
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
        let rk = liveRun d
        let threads = sortOn ltId [t | t <- frThreads lastF, T.null filt || filt `T.isInfixOf` T.toLower (ltLabel t <> " " <> tshow (ltId t))]
        rowWith (padXY 8 0 . tight . gap 12 . fillW) $ do
          colHeader (fixedW 56 . alignEnd) "id"
          colHeader fillW "label"
          colHeader (fixedW 240) "status"
        paneScroll (grow . tight) $ columnWith (tight . fillW . gap 0) $
          forM_ threads $ \t -> withKey (ltId t) $ void $ linkRow env (RThread rk (fromIntegral (ltId t))) (focus == Just (ltId t)) (padXY 8 3 . gap 12) $ do
            numCell 56 fontMuted (tshow (ltId t))
            cell (fillW . if T.null (ltLabel t) then fontMuted else id) (if T.null (ltLabel t) then "unlabelled" else ltLabel t)
            cell (fixedW 240 . fontTone (groupTone (statusGroup (ltStatus t)))) (ltStatus t)

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
-- Lens

-- | What Live can show of the entities other panels name: the threads the
-- program has now, when it is the run they come from.
liveLens :: LiveState -> Maybe LiveData -> Lens
liveLens st mdata = case mdata of
  Just d
    | Just lastF <- Seq.lookup (Seq.length (ldFrames d) - 1) (ldFrames d) ->
        let rk = liveRun d
            thread r t = if runKeysMatch r rk then listToMaybe [lt | lt <- frThreads lastF, ltId lt == fromIntegral t] else Nothing
         in (lens PLive)
              { lensShows = \case
                  RThread r t -> isJust (thread r t)
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
                  RThread r t -> (\lt -> fact "now" (ltStatus lt)) <$> thread r t
                  _ -> Nothing
              , lensHits =
                  [ Hit "live thread" (if T.null (ltLabel lt) then "thread " <> tshow (ltId lt) else ltLabel lt) (ltStatus lt) (HitRef (RThread rk (fromIntegral (ltId lt))))
                  | lt <- frThreads lastF
                  ]
              }
  _ -> lens PLive
