-- | A "top" for a running Haskell program: connects to the agent in
-- "Stronghead.Agent" and charts allocation, GC and residency as they
-- happen, with the live thread list.
module UI.Live
  ( LiveState (..)
  , newLiveState
  , liveLoad
  , liveView
  , LiveData (..)
  ) where

import Control.Monad (forM_, when)
import Data.Foldable (for_, toList)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import NanoUI
import Stronghead.Format
import Stronghead.Live
import UI.Common

data LiveState = LiveState
  { lvHost :: !(Var Text)
  , lvPort :: !(Var Text)
  , lvWant :: !(Var Bool)
  , lvAttempt :: !(Var Int)
  , lvStream :: !(Stream (String, Int, Int) LiveData)
  , lvEventlog :: !(Var Text)
  , lvThreadFilter :: !(Var Text)
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
            { rTime = frTime b
            , rAlloc = d "allocated_bytes" / dt
            , rGCs = d "gcs" / dt
            , rGCPauseMs = if newGC then stat "gc_elapsed_ns" b / 1e6 else 0
            , rLive = stat "gc_live_bytes" b
            , rMem = stat "gc_mem_in_use_bytes" b
            , rProductivity = let e = d "elapsed_ns" in if e > 0 then d "mutator_elapsed_ns" / e else 0
            , rThreads = fromIntegral (length (frThreads b))
            }

liveView :: LiveState -> Maybe LiveData -> (FilePath -> NanoUI ()) -> NanoUI ()
liveView st mdata followEventlog = columnWith (gap 8 . grow . padAll 10) $ do
  want <- readVar (lvWant st)
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    label "Agent"
    textInputConfigured defaultTextInputConfig {ticLayout = fixedW 140 defaultLayout} (lvHost st)
    textInputConfigured defaultTextInputConfig {ticLayout = fixedW 70 defaultLayout} (lvPort st)
    if want
      then whenM (button "Disconnect") (writeVar (lvWant st) False)
      else whenM (styled primary (button "Connect")) (writeVar (lvWant st) True >> modifyVar (lvAttempt st) (+ 1))
    scope $ for_ mdata $ \d -> do
      for_ (ldHello d) $ \(p, pid, caps) -> badge Success (p <> "  pid " <> tshow pid <> "  " <> tshow caps <> " caps")
      for_ (ldClosed d) $ \why -> do
        labelWith (fontTone Danger) (T.take 120 why)
        whenM (button "Retry") (modifyVar (lvAttempt st) (+ 1))
    flex
    label "Eventlog"
    textInputConfigured defaultTextInputConfig {ticPlaceholder = "path of the program's .eventlog", ticLayout = minW 220 defaultLayout} (lvEventlog st)
    path <- readVar (lvEventlog st)
    whenM (button "Follow in Timeline") (when (not (T.null (T.strip path))) (followEventlog (T.unpack (T.strip path))))
  case mdata of
    Just d | not (Seq.null (ldFrames d)) -> scope (dashboard st d)
    _ -> scope $ columnWith (gap 6 . padAll 24) $ do
      sectionTitle "Live RTS dashboard"
      hint "Link the target program with the stronghead:agent library and wrap main:"
      labelWith fontMono "  import Stronghead.Agent"
      labelWith fontMono "  main = withAgent defaultAgentConfig $ do ..."
      hint "Build it with -threaded and run it with +RTS -T (for RTS statistics). Then press Connect."
      hint "Run it with +RTS -l -ol<file>.eventlog as well and use Follow in Timeline: the agent flushes the eventlog every 250 ms so the timeline fills in as the program runs."

dashboard :: LiveState -> LiveData -> NanoUI ()
dashboard st d = do
  th <- uiTheme
  let pal = palette th
      frames = ldFrames d
      rs = rates frames
      lastF = Seq.index frames (Seq.length frames - 1)
      latest = if null rs then Nothing else Just (last rs)
      t0 = maybe 0 frTime (Seq.lookup 0 frames)
      series f = [(rTime r - t0, f r) | r <- rs]
      span' = case (rs, reverse rs) of
        (r0 : _, rN : _) -> (rTime r0 - t0, rTime rN - t0)
        _ -> (0, 1)
      statsOn = stat "rts_stats" lastF > 0
  scope $ when (not statsOn) $
    callout (themeWarning th) (label "The program was not run with +RTS -T, so only the thread list is available.")
  rowWith (tight . gap 8 . wrap . lineGap 6 . fillW) $ do
    for_ latest $ \r -> do
      statTile "Allocation" (fmtRate (rAlloc r))
      statTile "Live (last GC)" (fmtBytesD (rLive r))
      statTile "Memory in use" (fmtBytesD (rMem r))
      statTile "GCs / s" (fmtFixed 1 (rGCs r))
      statTile "Productivity" (fmtPct (rProductivity r))
    statTile "Last pause" (fmtNs (stat "gc_elapsed_ns" lastF))
    statTile "Max live" (fmtBytesD (stat "max_live_bytes" lastF))
    statTile "Major GCs" (fmtInt (round (stat "major_gcs" lastF) :: Int))
    statTile "GC CPU" (fmtPct (stat "gc_cpu_ns" lastF / max 1 (stat "cpu_ns" lastF)))
    statTile "Threads" (tshow (length (frThreads lastF)))
  mm <- monoMetrics 0.8
  let chart title color f fmtY fill' =
        columnWith (tight . gap 2 . fillW) $ do
          labelWith (fontMuted . fontSizeScale 0.9) title
          _ <- canvas (fillW . fixedH 110) $ \r ->
            drawLineChart pal (mmFont mm) r span' fmtY [ChartSeries title color (series f) fill']
          pure ()
  rowWith (tight . gap 8 . fillW) $ do
    chart "Allocation rate" (pRun pal) rAlloc fmtRate True
    chart "Live bytes after GC" (pLive pal) rLive fmtBytesD True
  rowWith (tight . gap 8 . fillW) $ do
    chart "GC pause (ms, at each collection)" (pGCMajor pal) rGCPauseMs (\v -> fmtFixed 1 v <> " ms") False
    chart "Threads" (pWakeup pal) rThreads (fmtFixed 0) False
  -- Thread table.
  filt <- T.toLower <$> readVar (lvThreadFilter st)
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    sectionTitle "Threads"
    searchInput "Filter" (lvThreadFilter st)
    flex
    let counts = M.toList (M.fromListWith (+) [(statusGroup (ltStatus t), 1 :: Int) | t <- frThreads lastF])
    forM_ counts $ \(g, n) -> badge (groupTone g) (g <> " " <> tshow n)
  let threads = sortOn ltId [t | t <- frThreads lastF, T.null filt || filt `T.isInfixOf` T.toLower (ltLabel t <> " " <> tshow (ltId t))]
  scrollWith (grow . tight . minH 120) $ columnWith (tight . fillW . gap 0) $
    forM_ threads $ \t -> withKey (ltId t) $ rowWith (gap 10 . fillW . padXY 8 2) $ do
      labelWith (fontMono . fixedW 70) (tshow (ltId t))
      labelWith fillW (if T.null (ltLabel t) then "(unlabelled)" else ltLabel t)
      labelWith (fixedW 220 . fontTone (groupTone (statusGroup (ltStatus t)))) (ltStatus t)
  let _ = isJust (ldHello d) || fromMaybe False Nothing
  pure ()

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
