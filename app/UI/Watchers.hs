-- | Watchers ("Stronghead.Watch") on the program Stronghead is attached
-- to: the rules, evaluated every frame on what the agent's frames and the
-- followed eventlog added since the last; what they did when they fired;
-- and where that shows: the Watchers pane, a badge in the status bar and a
-- callout over it.
--
-- A rule's responses go through the same paths as the Live panel's
-- actions: a mark through the agent, a census through the agent, a
-- snapshot through the heap panel's ghc-debug target. When one can't run,
-- the fired event says why.
module UI.Watchers
  ( Watchers (..)
  , newWatchers
  , WatchHooks (..)
  , watchTick
  , watchersPane
  , watchStatus
  , FiredEvent (..)
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_, toList)
import Data.IORef
import Data.List (insertBy, (!?))
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..), comparing)
import Data.Sequence qualified as Seq
import Data.Word (Word64)

import Data.Text (Text)
import Data.Text qualified as T
import NanoUI

import Stronghead.Eventlog.Model (Timeline (..))
import Stronghead.Format (fmtFixed, fmtNs, tshow)
import Stronghead.Live (Frame (..))
import Stronghead.Ref
import Stronghead.Run (runKey, runKeysMatch)
import Stronghead.Watch
import UI.Common
import UI.Link
import UI.Live (LiveData (..), liveRun)

data Watchers = Watchers
  { wrRules :: !(Var [Rule])
  , wrState :: !(IORef WatchState)
  , wrRun :: !(IORef (Maybe RunKey))
    -- ^ The run the state follows; a new run starts afresh.
  , wrFrameSeen :: !(IORef Word64)
    -- ^ Program time of the newest frame evaluated.
  , wrLogSeen :: !(IORef Word64)
    -- ^ Program time up to which the followed eventlog was evaluated.
  , wrLastFrame :: !(IORef (Maybe Frame))
  , wrFired :: !(Var [FiredEvent])
    -- ^ Newest first.
  , wrUnseen :: !(Var Int)
    -- ^ Fired since the list was last opened, for the badge.
  , wrToast :: !(Var (Maybe (FiredEvent, Double)))
    -- ^ The callout on show, and until when ('uiTime').
  , wrListOpen :: !(Var Bool)
  }

-- | A rule that fired, in which run, and what became of its responses.
data FiredEvent = FiredEvent
  { feFired :: !Fired
  , feRun :: !RunKey
  , feTitle :: !Text
    -- ^ The rule, as it read when it fired.
  , feDone :: ![Text]
  }
  deriving stock (Eq, Show)

newWatchers :: IO Watchers
newWatchers = do
  wrRules <- newVar defaultRules
  wrState <- newIORef emptyWatchState
  wrRun <- newIORef Nothing
  wrFrameSeen <- newIORef 0
  wrLogSeen <- newIORef 0
  wrLastFrame <- newIORef Nothing
  wrFired <- newVar []
  wrUnseen <- newVar 0
  wrToast <- newVar Nothing
  wrListOpen <- newVar False
  pure Watchers {..}

-- | How the responses reach the program, or why they can't now.
data WatchHooks = WatchHooks
  { whMark :: Either Text (Text -> NanoUI ())
  , whCensus :: Either Text (NanoUI ())
  , whSnapshot :: Either Text (NanoUI ())
  }

-- | Feed the rules what the streams added since the last frame, and act
-- on what fires. The agent's frames give the threads and the GC share;
-- the followed eventlog gives major collections, censuses, and, with no
-- agent, blocked spans and the GC share too. Called every frame.
watchTick :: Watchers -> WatchHooks -> Maybe LiveData -> Maybe Timeline -> Bool -> NanoUI ()
watchTick w hooks live logTl following = do
  rules <- readVar (wrRules w)
  let frames = case live of
        Just d | Nothing <- ldClosed d, not (Seq.null (ldFrames d)) -> Just (liveRun d, ldFrames d)
        _ -> Nothing
      followed = if following then logTl else Nothing
      run = case (frames, followed) of
        (Just (rk, _), _) -> Just rk
        (Nothing, Just tl) -> Just (runKey (tlRun tl))
        _ -> Nothing
  prevRun <- liftIO (readIORef (wrRun w))
  when (run /= prevRun) $ liftIO $ do
    writeIORef (wrRun w) run
    writeIORef (wrState w) emptyWatchState
    writeIORef (wrFrameSeen w) 0
    writeIORef (wrLogSeen w) 0
    writeIORef (wrLastFrame w) Nothing
  for_ run $ \rk -> do
    -- Frames newer than the last one evaluated.
    fromFrames <- case frames of
      Just (_, fs) -> liftIO $ do
        seen <- readIORef (wrFrameSeen w)
        prev <- readIORef (wrLastFrame w)
        -- Frames arrive in program order, so the new ones are a suffix.
        let new = toList (Seq.takeWhileR ((> seen) . frProgramNs) fs)
            obs = concat (zipWith frameObservations (prev : map Just new) new)
        case reverse new of
          f : _ -> writeIORef (wrFrameSeen w) (frProgramNs f) >> writeIORef (wrLastFrame w) (Just f)
          [] -> pure ()
        -- The eventlog's collections are exact; frames see the last one only.
        pure [o | o <- obs, not (isMajor o && isJust followed)]
      Nothing -> pure []
    fromLog <- case followed of
      Just tl | runKeysMatch rk (runKey (tlRun tl)) -> liftIO $ do
        seen <- readIORef (wrLogSeen w)
        let upto = tlEnd tl
        writeIORef (wrLogSeen w) upto
        let obs = if upto > seen then timelineObservations tl seen upto else []
        pure [o | o <- obs, not (isJust frames && (isSpan o || isShare o))]
      _ -> pure []
    let obs = mergeByTime fromFrames fromLog
    unless (null obs) $ do
      st <- liftIO (readIORef (wrState w))
      let (st', fired) = watchRun rules obs st
      liftIO (writeIORef (wrState w) st')
      let newest = maximum (map obsTime obs)
      forM_ fired $ \f -> respond w hooks rules rk newest f
  -- The callout goes after a while.
  toast <- readVar (wrToast w)
  for_ toast $ \(_, until') -> do
    now <- uiTime
    if now >= until' then writeVar (wrToast w) Nothing else wakeAfter (until' - now)
  where
    isMajor = \case ObsMajorGC {} -> True; _ -> False
    isSpan = \case ObsBlockedSpan {} -> True; _ -> False
    isShare = \case ObsGCShare {} -> True; _ -> False

mergeByTime :: [Observation] -> [Observation] -> [Observation]
mergeByTime xs [] = xs
mergeByTime [] ys = ys
mergeByTime (x : xs) (y : ys)
  | obsTime x <= obsTime y = x : mergeByTime xs (y : ys)
  | otherwise = y : mergeByTime (x : xs) ys

-- | Carry out what a firing asks for. A firing from more than five
-- seconds before the newest observation (an eventlog read from its
-- start) is only listed: marking or snapshotting now would be about
-- something else.
respond :: Watchers -> WatchHooks -> [Rule] -> RunKey -> Word64 -> Fired -> NanoUI ()
respond w hooks rules rk newest f = do
  let stale = newest > fiAt f + 5000000000
      title = case drop (fiRule f) rules of
        r : _ -> describeTrigger (ruTrigger r)
        [] -> "A watcher"
      attempt what = \case
        Left why -> pure [what <> ": " <> why]
        Right act -> [what] <$ act
  done <-
    if stale
      then pure ["from the eventlog's history: listed only"]
      else fmap concat . sequence $
        [ attempt "marked" (fmap ($ ("watch: " <> T.take 60 (fiText f))) (whMark hooks)) | RespMark `elem` fiResponses f ]
          ++ [attempt "census taken" (whCensus hooks) | RespCensus `elem` fiResponses f]
          ++ [attempt "snapshot taken" (whSnapshot hooks) | RespSnapshot `elem` fiResponses f]
  let ev = FiredEvent f rk title done
  -- Newest first by program time: the eventlog lags the frames.
  modifyVar (wrFired w) (take 200 . insertBy (comparing (Down . fiAt . feFired)) ev)
  when (RespNotify `elem` fiResponses f && not stale) $ do
    modifyVar (wrUnseen w) (+ 1)
    now <- uiTime
    writeVar (wrToast w) (Just (ev, now + 8))

-- | What a fired event is about, as a link.
subjectRef :: FiredEvent -> Ref
subjectRef ev = case fiSubject (feFired ev) of
  AboutThread t _ -> RThread (feRun ev) (fromIntegral t)
  AboutBand b -> RBand b
  AboutRun -> RInstant (feRun ev) (fiAt (feFired ev))

-- | The fired events, newest first: when (a link to the instant), what
-- happened, what it is about (a link), and what was done.
firedList :: Env -> [FiredEvent] -> NanoUI ()
firedList env evs =
  if null evs
    then hint "Nothing has fired yet."
    else forM_ (zip [0 :: Int ..] evs) $ \(i, ev) -> withKey i $
      columnWith (fillW . gap 2 . padTop 4 . tight) $ do
        let f = feFired ev
        rowWith (fillW . tight . gap 8) $ do
          linkText env (RInstant (feRun ev) (fiAt f)) (alignBaseline . fontMono . fontSizeScale 0.85) (fmtNs (fromIntegral (fiAt f)))
          labelWith (alignBaseline . fillW . fontSizeScale 0.9) (fiText f)
        rowWith (fillW . gap 8 . padLeft 12 . tight) $ do
          labelWith (alignBaseline . fontMuted . fontSizeScale 0.8) (feTitle ev)
          case fiSubject f of
            AboutRun -> pure ()
            _ -> linkText env (subjectRef ev) (alignBaseline . fontSizeScale 0.85) (T.take 50 (refTitle env (subjectRef ev)))
          flex
          labelWith (alignBaseline . fontMuted . fontSizeScale 0.8) (T.intercalate "; " (feDone ev))

-- | The Watchers pane: what has fired, newest first; then each built-in
-- rule with its switch, its limits, its responses, its cooldown and
-- hysteresis.
watchersPane :: Env -> Watchers -> Text -> PaneCtx -> NanoUI ()
watchersPane env w watching ctx = do
  rules <- readVar (wrRules w)
  evs <- readVar (wrFired w)
  let header = do
        paneMeta watching
        flex
        disabledWhen (null evs) $ whenM (buttonWith (alignMid . padXY 8 2 . tight) "Clear fired") (writeVar (wrFired w) [] >> writeVar (wrUnseen w) 0)
  paneWith tight ctx "Watchers" header $
    paneScroll (grow . tight) $
      columnWith (padAll 10 . fillW . tight . gap 8) $ do
        rowWith (fillW . tight . gap 8) $ do
          labelWith (fontMedium . fontSizeScale 0.92) "Fired"
          paneMeta (tshow (length evs) <> if length evs > 30 then ", the newest 30 here" else "")
        firedList env (take 30 evs)
        separator
        labelWith (fontMedium . fontSizeScale 0.92) "Rules"
        hint "On the agent's frames and the followed eventlog. Each is off until switched on, and is saved with the session."
        forM_ (zip [0 :: Int ..] rules) $ \(i, r) -> withKey (triggerName (ruTrigger r)) $
          ruleEditor (ruleVar i r) r
  where
    ruleVar i def = zoomVar (\rs -> fromMaybe def (rs !? i)) (\r rs -> take i rs ++ [r] ++ drop (i + 1) rs) (wrRules w)

-- | One rule's controls.
ruleEditor :: Var Rule -> Rule -> NanoUI ()
ruleEditor v r =
  panelWith (padAll 8 . gap 4 . fillW) $ do
    rowWith (fillW . tight . gap 8) $ do
      checkboxWith alignMid (describeTrigger (ruTrigger r)) (zoomVar ruOn (\b x -> x {ruOn = b}) v)
    rowWith (fillW . tight . gap 12 . wrap . lineGap 6) $ do
      case ruTrigger r of
        LiveUp n -> param "collections" (fromIntegral n) 2 20 (\x t -> case t of LiveUp _ -> LiveUp (round x); o -> o) (tshow n)
        BlockedOver ms -> param "ms" (realToFrac ms) 50 5000 (\x t -> case t of BlockedOver _ -> BlockedOver (fromIntegral (round (x / 10) :: Int) * 10); o -> o) (fmtFixed 0 ms <> " ms")
        GCOver p s -> do
          param "percent" (realToFrac p) 5 95 (\x t -> case t of GCOver _ s' -> GCOver (fromIntegral (round x :: Int)) s'; o -> o) (fmtFixed 0 p <> "%")
          param "seconds" (realToFrac s) 0 60 (\x t -> case t of GCOver p' _ -> GCOver p' (fromIntegral (round x :: Int)); o -> o) (fmtFixed 0 s <> " s")
        BandGrowing n -> param "censuses" (fromIntegral n) 2 30 (\x t -> case t of BandGrowing _ -> BandGrowing (round x); o -> o) (tshow n)
      knob "cooldown" (realToFrac (ruCooldown r)) 0 300 (\x x' -> x' {ruCooldown = fromIntegral (round x :: Int)}) (fmtFixed 0 (ruCooldown r) <> " s")
      knob "hysteresis" (realToFrac (ruHysteresis r)) 0 1 (\x x' -> x' {ruHysteresis = fromIntegral (round (x * 100) :: Int) / 100}) (fmtFixed 0 (ruHysteresis r * 100) <> "%")
    rowWith (fillW . tight . gap 10) $ do
      labelWith (alignMid . fontMuted . fontSizeScale 0.85) "then"
      forM_ [minBound .. maxBound] $ \resp -> withKey (responseName resp) $
        checkboxWith alignMid (responseName resp) (zoomVar (elem resp . ruResponses) (\b x -> x {ruResponses = nubOrd (if b then resp : ruResponses x else filter (/= resp) (ruResponses x))}) v)
  where
    param name cur lo hi set shown = knob name cur lo hi (\x x' -> x' {ruTrigger = set x (ruTrigger x')}) shown
    knob :: Text -> Float -> Float -> Float -> (Float -> Rule -> Rule) -> Text -> NanoUI ()
    knob name cur lo hi set shown =
      rowWith (tight . gap 6) $ do
        labelWith (alignMid . fontMuted . fontSizeScale 0.85) name
        sliderWith (alignMid . fixedW 110) lo hi (zoomVar (const cur) set v)
        labelWith (alignMid . fontMono . fontSizeScale 0.85 . fixedW 60) shown

-- | The status bar's part: a badge when something has fired, opening the
-- list, and the callout over it while it is new.
watchStatus :: Env -> Watchers -> NanoUI ()
watchStatus env w = do
  evs <- readVar (wrFired w)
  unseen <- readVar (wrUnseen w)
  toast <- readVar (wrToast w)
  scope $ unless (null evs) $ do
    sid <- currentId
    prev <- lastRect sid
    mouse <- uiMousePos
    let under = maybe False (`rectContains` mouse) prev
        txt = if unseen > 0 then "Watchers: " <> tshow unseen <> " new" else "Watchers: " <> tshow (length evs) <> " fired"
    (_, area) <- withCursorShape UiCursorPointer $ mouseArea (alignMid . tight) $
      labelWith (alignMid . fontSizeScale 0.85 . (if unseen > 0 then fontTone Warning else fontMuted) . (if under then fontUnderline else id)) txt
    tooltip area "Click for what the watchers saw"
    when (respClicked area) $ do
      modifyVar (wrListOpen w) not
      writeVar (wrUnseen w) 0
      writeVar (wrToast w) Nothing
    let anchor = AnchorRect (respRect area)
    popupWith (wrListOpen w) (defaultPopupConfig anchor) {cfgPlacement = PlacementAbove} (fixedW 560 . maxH 480) $
      void . scrollArea fillW $
        columnWith (fillW . padAll 12 . tight . gap 4) $ do
          labelWith (fontMedium . fontSizeScale 1.05) "What the watchers saw"
          firedList env evs
    for_ toast $ \(ev, _) -> do
      th <- uiTheme
      shown <- useVar True
      writeVar shown True
      popupWith shown (defaultPopupConfig anchor) {cfgPlacement = PlacementAbove, cfgDismissable = False} (fixedW 420) $
        calloutWith (toneColor th Warning) (fillW . padAll 10 . gap 4) $ do
          rowWith (fillW . tight . gap 8) $ do
            labelWith (alignMid . fontMedium . fontSizeScale 0.92 . fillW) (feTitle ev)
            whenM (buttonWith (alignMid . padXY 6 0 . tight) "Dismiss") (writeVar (wrToast w) Nothing)
          labelWith (fillW . fontSizeScale 0.9) (fiText (feFired ev))
          rowWith (fillW . tight . gap 8) $ do
            linkText env (subjectRef ev) (fontSizeScale 0.85) (T.take 60 (refTitle env (subjectRef ev)))
            flex
            labelWith (fontMuted . fontSizeScale 0.8) (T.intercalate "; " (feDone ev))
