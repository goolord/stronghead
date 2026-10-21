-- | One clock for every time view. The Timeline, the heap chart and Live's
-- charts each put program time on an 'Axis', draw the bus's cursor, range
-- and the run's markers on it ('drawMarks'), and share the gestures that
-- move them ('timeGestures'): Shift+drag selects a range, and a view that
-- scrubs moves the cursor with a plain drag. A view whose own cursor snaps
-- to samples keeps it in step with the shared one through a 'Tether'.
--
-- Besides the eventlog's markers, every time view draws the bus's moments:
-- the marks and actions Stronghead sent the agent, until their markers turn
-- up in the followed eventlog ('confirmMoments'), and the snapshots it took.
--
-- Time links only join views of matching runs ('runKeysMatch').
module UI.Clock
  ( -- * Axes
    Axis (..)
  , linearAxis
    -- * The bus's moments
  , setCursor
  , setRange
  , Marks (..)
  , marksFor
  , drawMarks
  , momentAt
  , confirmMoments
    -- * Gestures
  , Scrub (..)
  , timeGestures
  , stepKeys
    -- * Snapping cursors
  , Tether
  , newTether
  , tether
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.IORef
import Data.Vector qualified as V
import Data.Word (Word64)
import NanoUI
import NanoUI.Shortcut qualified as K
import Data.List (sortOn)
import Stronghead.Clock (partitionPoint)
import Stronghead.Eventlog.Model (Marker (..), Timeline (..))
import Stronghead.Ref
import Stronghead.Run (runKey, runKeysMatch)
import UI.Common
import UI.Link

-- ---------------------------------------------------------------------------
-- Axes

-- | Where program time falls across a canvas.
data Axis = Axis
  { axRun :: !RunKey
  , axToX :: Double -> Float
    -- ^ Program ns to x.
  , axToNs :: Float -> Double
  }

-- | An axis showing a window of program time across @[x, x + w]@.
linearAxis :: RunKey -> TimeView -> Float -> Float -> Axis
linearAxis rk v x w = Axis rk (timeToX v x w) (xToTime v x w)

-- ---------------------------------------------------------------------------
-- The bus's moments

setCursor :: Bus -> RunKey -> Word64 -> NanoUI ()
setCursor bus rk t = do
  cur <- readVar (busCursor bus)
  when (cur /= Just (rk, t)) (writeVar (busCursor bus) (Just (rk, t)))

setRange :: Bus -> RunKey -> Word64 -> Word64 -> NanoUI ()
setRange bus rk a b = do
  let new = Just (rk, min a b, max a b)
  cur <- readVar (busRange bus)
  when (cur /= new) (writeVar (busRange bus) new)

-- | What a time view of a run marks: the cursor and range when they belong
-- to the run, the eventlog's markers when the log is the run's, and the
-- run's moments.
data Marks = Marks
  { tmCursor :: !(Maybe Word64)
  , tmRange :: !(Maybe (Word64, Word64))
  , tmMarkers :: ![Word64]
  , tmMoments :: ![Moment]
  }

marksFor :: Env -> RunKey -> Maybe Timeline -> NanoUI Marks
marksFor env rk mlog = do
  let bus = envBus env
  cursor <- readVar (busCursor bus)
  range <- readVar (busRange bus)
  moments <- readVar (busMoments bus)
  pure
    Marks
      { tmCursor = case cursor of
          Just (r, t) | runKeysMatch r rk -> Just t
          _ -> Nothing
      , tmRange = case range of
          Just (r, a, b) | runKeysMatch r rk -> Just (a, b)
          _ -> Nothing
      , tmMarkers = case mlog of
          Just tl | runKeysMatch (runKey (tlRun tl)) rk -> [mkTime mk | mk <- V.toList (tlMarkers tl), mkIsMarker mk]
          _ -> []
      , tmMoments = [m | m <- moments, runKeysMatch (moRun m) rk]
      }

-- | The range as a wash, markers as hairlines and the cursor as a line in
-- the accent, from the top of @r@ to its bottom.
drawMarks :: Palette -> Axis -> Rect -> Marks -> CanvasM ()
drawMarks pal ax r@(Rect x y w h) m = withClip r $ do
  let xOf t = axToX ax (fromIntegral t)
      visible px = px >= x - 1 && px <= x + w + 1
  for_ (tmRange m) $ \(a, b) -> do
    let xa = max x (xOf a)
        xb = min (x + w) (xOf b)
    when (xb > xa) $ do
      drawRect (Rect xa y (xb - xa) h) (alpha 0.10 (pSelection pal))
      drawRect (Rect xa y 1 h) (alpha 0.45 (pSelection pal))
      drawRect (Rect (xb - 1) y 1 h) (alpha 0.45 (pSelection pal))
  forM_ (tmMarkers m) $ \t -> do
    let px = xOf t
    when (visible px) $ drawStroke (V2 px y) (V2 px (y + h)) 1 (alpha 0.55 (pMarker pal))
  -- Stronghead's own moments carry a square cap. A mark's line is dashed
  -- while its time is an estimate, and left to the eventlog's marker once
  -- that is there. A snapshot or a pause is a line in ink.
  forM_ (tmMoments m) $ \mo -> do
    let px = xOf (moNs mo)
        cap c = drawRect (Rect (px - 3) y 6 6) c
    when (visible px) $ case moKind mo of
      k | k `elem` [MomentSnapshot, MomentPause] -> do
        let c = alpha (if k == MomentSnapshot then 0.75 else 0.5) (pText pal)
        drawStroke (V2 px y) (V2 px (y + h)) 1 c
        cap c
      _
        | moInLog mo -> cap (pMarker pal)
        | moExact mo -> do
            drawStroke (V2 px y) (V2 px (y + h)) 1 (alpha 0.75 (pMarker pal))
            cap (pMarker pal)
        | otherwise -> do
            forM_ (takeWhile (< y + h) [y, y + 6 ..]) $ \dy ->
              drawStroke (V2 px dy) (V2 px (min (y + h) (dy + 3))) 1 (alpha 0.75 (pMarker pal))
            cap (alpha 0.6 (pMarker pal))
  for_ (tmCursor m) $ \t -> do
    let px = xOf t
    when (visible px) $ drawStroke (V2 px y) (V2 px (y + h)) 1.5 (pAccent pal)

-- | The moment nearest @x@ on an axis, within a few pixels, as an instant.
momentAt :: Axis -> Marks -> Float -> Maybe Ref
momentAt ax m x =
  case sortOn fst [(d, mo) | mo <- tmMoments m, let d = abs (axToX ax (fromIntegral (moNs mo)) - x), d <= 4] of
    (_, mo) : _ -> Just (RInstant (moRun mo) (moNs mo))
    [] -> Nothing

-- | Note the marks whose markers have turned up in the followed eventlog,
-- taking up the marker's time, which is exact. Called once a frame.
confirmMoments :: Bus -> Maybe Timeline -> NanoUI ()
confirmMoments bus = \case
  Nothing -> pure ()
  Just tl -> do
    moments <- readVar (busMoments bus)
    let rk = runKey (tlRun tl)
        ms = tlMarkers tl
        -- Markers are in time order; look only near the moment.
        near mo =
          let lo = if moNs mo > window then moNs mo - window else 0
              start = lowerBound (\mk -> mkTime mk >= lo) ms
           in [ mk
              | mk <- V.toList (V.takeWhile (\mk -> mkTime mk <= moNs mo + window) (V.drop start ms))
              , mkIsMarker mk
              , mkText mk == moLabel mo
              ]
        confirm mo
          | moInLog mo || not (runKeysMatch (moRun mo) rk) = mo
          | mk : _ <- near mo = mo {moNs = mkTime mk, moExact = True, moInLog = True}
          | otherwise = mo
        moments' = map confirm moments
    unless (moments' == moments) (writeVar (busMoments bus) moments')
  where
    window = 5000000000

-- | The first index whose element satisfies a monotone predicate.
lowerBound :: (a -> Bool) -> V.Vector a -> Int
lowerBound p v = partitionPoint (p . (v V.!)) 0 (V.length v)

-- ---------------------------------------------------------------------------
-- Gestures

-- | What a plain drag does on a time view: move the cursor, or what the
-- view does with it (pan, say).
data Scrub = ScrubDrag | ScrubNone
  deriving (Eq, Show)

-- | The gestures every time view shares: Shift+drag selects a range and
-- Shift+click clears it; with 'ScrubDrag', a plain press or drag moves the
-- cursor. Returns whether a range is being drawn (or was finished this
-- frame), so the view can leave its own drag and click alone.
timeGestures :: Env -> Axis -> Response -> Scrub -> NanoUI Bool
timeGestures env ax resp scrub = do
  inp <- askInput
  let bus = envBus env
      mouse = inputMousePos inp
      mods = inputModifiers inp
      r = respRect resp
      at px = round (max 0 (axToNs ax (max (rectX r) (min (rectX r + rectW r) px)))) :: Word64
  when (respHovered resp && pressedIn MouseLeft inp && modShift mods) $
    liftIO (writeIORef (busBrush bus) (Just mouse))
  brush <- liftIO (readIORef (busBrush bus))
  drag <- useDrag2DOn resp
  case brush of
    Just from | rectContains r from -> do
      let moved = abs (v2X mouse - v2X from) >= 3
      if heldIn MouseLeft inp
        then when moved (setRange bus (axRun ax) (at (v2X from)) (at (v2X mouse)))
        else do
          liftIO (writeIORef (busBrush bus) Nothing)
          when (not moved) (writeVar (busRange bus) Nothing)
      pure True
    _ -> do
      when (scrub == ScrubDrag && dragActive drag && not (modCtrl mods) && not (modShift mods)) $
        setCursor bus (axRun ax) (at (v2X (dragPosition drag)))
      pure False

-- | @[@ and @]@ this frame: -1, 1 or 0, and whether Shift was held.
stepKeys :: NanoUI (Int, Bool)
stepKeys = do
  let pressed = fmap or . mapM shortcut
  back <- pressed [K.key '[']
  fwd <- pressed [K.key ']']
  backS <- pressed [K.shift <> K.key '[']
  fwdS <- pressed [K.shift <> K.key ']']
  pure $
    if
      | back -> (-1, False)
      | fwd -> (1, False)
      | backS -> (-1, True)
      | fwdS -> (1, True)
      | otherwise -> (0, False)

-- ---------------------------------------------------------------------------
-- Snapping cursors

-- | What a 'tether' saw last frame: the shared cursor and the view's own.
newtype Tether a = Tether (IORef (Maybe (RunKey, Word64), Maybe a))

newTether :: IO (Tether a)
newTether = Tether <$> newIORef (Nothing, Nothing)

-- | Keep a view's own cursor in step with the shared one, once a frame.
-- When the shared cursor moves, the view takes it up through @adopt@ (which
-- snaps it, or says it is out of the view's reach). When the view's own
-- moves (a drag, a reveal, a step), it is published through @publish@.
tether :: (Eq a) => Bus -> Tether a -> RunKey -> Var (Maybe a) -> (Word64 -> Maybe a) -> (a -> Maybe Word64) -> NanoUI ()
tether bus (Tether seen) rk own adopt publish = do
  shared <- readVar (busCursor bus)
  mine <- readVar own
  (seenShared, seenOwn) <- liftIO (readIORef seen)
  if
    | shared /= seenShared -> do
        let new = case shared of
              Just (r, t) | runKeysMatch r rk -> adopt t
              _ -> Nothing
        for_ new $ \a -> when (mine /= Just a) (writeVar own (Just a))
        liftIO (writeIORef seen (shared, maybe mine Just new))
    | mine /= seenOwn -> do
        let pub = (rk,) <$> (mine >>= publish)
        for_ pub $ \c -> when (shared /= Just c) (writeVar (busCursor bus) (Just c))
        liftIO (writeIORef seen (maybe shared Just pub, mine))
    | otherwise -> pure ()
