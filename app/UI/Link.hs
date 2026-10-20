-- | The links between panels.
--
-- Every panel reads and writes one 'Bus': the entity in focus, the one
-- under the pointer, the navigation history, and the moment and range in
-- program time that every time view of a run marks. Every panel also offers a
-- 'Lens': what it can show, what it knows and what it can do for an entity
-- named by a 'Ref'. The widgets here make any entity on screen a link:
-- resting the pointer on it peeks at what every panel knows, right-clicking
-- it lists every panel's actions, Ctrl+click goes to it, and Alt+Left comes
-- back.
--
-- Lenses are rebuilt every frame from what the loaders returned, so they
-- always see current data and never need invalidating. Their fields are
-- lazy: a peek, a menu or the palette forces only what it shows.
module UI.Link
  ( -- * Panels
    Panel (..)
  , panelTitle
  , panelKey
  , panelNamed
    -- * The bus
  , Bus (..)
  , newBus
  , Location (..)
  , publishHover
    -- * Lenses
  , Lens (..)
  , lens
  , Action (..)
  , Hit (..)
  , HitTarget (..)
  , Env (..)
  , refTitle
  , describeRef
    -- * Navigation
  , goTo
  , showIn
  , goBack
  , goForward
  , setFocus
  , isFocusOrHover
    -- * Widgets
  , linked
  , linkRow
  , linkText
  , kvLink
  , canvasLinks
  , refMenu
  , refInline
  , prose
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.Foldable (for_)
import Data.IORef
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import NanoUI
import Stronghead.Debug.Graph (ThunkKey (..), fmtClosureId)
import Stronghead.Format (fmtNs, tshow)
import Stronghead.Ref
import Stronghead.Source (renderSrcSpan)
import System.FilePath (takeFileName)
import UI.Common

-- ---------------------------------------------------------------------------
-- Panels

data Panel = PTimeline | PHeap | PCore | PDebugger | PLive
  deriving (Eq, Show, Enum, Bounded)

panelTitle :: Panel -> Text
panelTitle = \case
  PTimeline -> "Timeline"
  PHeap -> "Heap & leaks"
  PCore -> "Core & STG"
  PDebugger -> "Debugger"
  PLive -> "Live"

-- | The panel's name on the command line and in the settings file.
panelKey :: Panel -> Text
panelKey = T.toLower . T.takeWhile (/= ' ') . panelTitle

panelNamed :: Text -> Maybe Panel
panelNamed name = lookup name [(panelKey p, p) | p <- [minBound .. maxBound]]

-- ---------------------------------------------------------------------------
-- The bus

data Bus = Bus
  { busFocus :: !(Var (Maybe Ref))
  , busHover :: !(Var (Maybe Ref))
    -- ^ The entity under the pointer last frame, wherever it was drawn.
  , busHoverNext :: !(IORef (Maybe Ref))
    -- ^ This frame's, written by whichever link the pointer is on and
    -- published by 'publishHover'.
  , busBack :: !(Var [Location])
    -- ^ Most recent first.
  , busForward :: !(Var [Location])
  , busMenu :: !(IORef (Maybe Ref))
    -- ^ What the open canvas context menu is about.
  , busCursor :: !(Var (Maybe (RunKey, Word64)))
    -- ^ The moment every time view of that run marks, in program time.
  , busRange :: !(Var (Maybe (RunKey, Word64, Word64)))
    -- ^ The range they shade.
  , busWindow :: !(Var (Maybe (RunKey, TimeView)))
    -- ^ The Timeline's visible window, for views locked to it. The Timeline
    -- publishes it every frame and takes up a change another view writes.
  , busBrush :: !(IORef (Maybe V2))
    -- ^ Where a Shift+drag on a time view began.
  }

-- | A place to come back to: a panel, and what was in focus there.
data Location = Location
  { locPanel :: !Panel
  , locRef :: !(Maybe Ref)
  }
  deriving (Eq, Show)

newBus :: IO Bus
newBus = do
  busFocus <- newVar Nothing
  busHover <- newVar Nothing
  busHoverNext <- newIORef Nothing
  busBack <- newVar []
  busForward <- newVar []
  busMenu <- newIORef Nothing
  busCursor <- newVar Nothing
  busRange <- newVar Nothing
  busWindow <- newVar Nothing
  busBrush <- newIORef Nothing
  pure Bus {..}

-- | Make this frame's hovered entity the one every panel highlights. Call
-- once, at the end of the frame.
publishHover :: Bus -> NanoUI ()
publishHover bus = do
  next <- liftIO (atomicModifyIORef' (busHoverNext bus) (Nothing,))
  cur <- readVar (busHover bus)
  when (next /= cur) (writeVar (busHover bus) next)

-- ---------------------------------------------------------------------------
-- Lenses

-- | One panel's view of the entities the others name.
data Lens = Lens
  { lensPanel :: !Panel
  , lensShows :: Ref -> Bool
    -- ^ Whether the panel can show it, with what it has loaded now.
  , lensReveal :: Ref -> NanoUI ()
    -- ^ Select it, scroll to it, zoom to it: whatever showing it takes. The
    -- panel need not be on screen; it catches up when next drawn.
  , lensTitle :: Ref -> Maybe Text
  , lensRelated :: Ref -> [(Text, Ref)]
    -- ^ Other entities this panel knows go with it: a band's allocation
    -- site, a closure's thread. The text is a menu item ("Go to ...").
  , lensActions :: Ref -> [Action]
  , lensPeek :: Ref -> Maybe (NanoUI ())
    -- ^ This panel's facts about it, for the peek.
  , lensHits :: [Hit]
    -- ^ What the palette can find in this panel.
  }

-- | A lens that shows nothing, to override field by field.
lens :: Panel -> Lens
lens p = Lens p (const False) (const (pure ())) (const Nothing) (const []) (const []) (const Nothing) []

data Action = Action
  { actLabel :: !Text
  , actRun :: !(Either Text (NanoUI ()))
    -- ^ 'Left' says why it can't run now.
  }

-- | Something the palette can find.
data Hit = Hit
  { hitKind :: !Text
  , hitLabel :: !Text
  , hitDetail :: !Text
  , hitTarget :: HitTarget
  }

data HitTarget = HitRef !Ref | HitRun (NanoUI ())

-- | What a view needs to draw links: the bus, this frame's lenses, the
-- panel on screen, and how to switch panels.
data Env = Env
  { envBus :: !Bus
  , envLenses :: [Lens]
  , envPanel :: !Panel
  , envSwitch :: Panel -> NanoUI ()
  }

-- | The best name any panel has for it.
refTitle :: Env -> Ref -> Text
refTitle env ref = maybe (describeRef ref) id (listToMaybe (mapMaybe (`lensTitle` ref) (envLenses env)))

-- | A name for a ref from the ref alone.
describeRef :: Ref -> Text
describeRef = \case
  RThread _ t -> "thread " <> tshow t
  RInstant _ t -> "t = " <> fmtNs (fromIntegral t)
  RRange _ a b -> fmtNs (fromIntegral a) <> " to " <> fmtNs (fromIntegral b)
  RCollection _ i -> "collection #" <> tshow (i + 1)
  RCensus _ i -> "census " <> tshow (i + 1)
  RMarker _ i -> "marker " <> tshow (i + 1)
  RSource s -> renderSrcSpan s
  RBinding m k -> qualified m k
  RCostCentre m l -> qualified m l
  RBand b -> b
  RInfoTable a -> fmtClosureId a
  RClosure _ a -> "closure " <> fmtClosureId a
  RThunkGroup k -> "thunk " <> tkName k <> maybe "" ("  " <>) (tkSource k)
  RBreakpoint f l -> "breakpoint at " <> T.pack (takeFileName f) <> ":" <> tshow l
  RModule m -> m
  where
    qualified m x = if T.null m then x else m <> "." <> x

-- ---------------------------------------------------------------------------
-- Navigation

-- | The panels to try, in order, when going to an entity.
homeOrder :: Ref -> [Panel]
homeOrder = \case
  RThread {} -> [PTimeline, PLive, PHeap]
  RInstant {} -> [PTimeline, PHeap, PLive]
  RRange {} -> [PTimeline, PHeap, PLive]
  RCollection {} -> [PTimeline]
  RMarker {} -> [PTimeline]
  RCensus {} -> [PHeap, PTimeline]
  RBand {} -> [PHeap]
  RInfoTable {} -> [PHeap]
  RClosure {} -> [PHeap]
  RThunkGroup {} -> [PHeap]
  RCostCentre {} -> [PHeap, PCore]
  RSource {} -> [PCore, PDebugger]
  RBinding {} -> [PCore, PDebugger]
  RModule {} -> [PCore]
  RBreakpoint {} -> [PDebugger]

-- | Go to an entity in its home panel, remembering where we were.
goTo :: Env -> Ref -> NanoUI ()
goTo env ref =
  for_ (listToMaybe [l | p <- homeOrder ref, l <- envLenses env, lensPanel l == p, lensShows l ref]) $ \l ->
    showIn env l ref

-- | Show an entity in one panel, remembering where we were.
showIn :: Env -> Lens -> Ref -> NanoUI ()
showIn env l ref = do
  let bus = envBus env
  here <- Location (envPanel env) <$> readVar (busFocus bus)
  let there = Location (lensPanel l) (Just ref)
  unless (here == there) $ modifyVar (busBack bus) (take 100 . (here :) . dropWhile (== here))
  writeVar (busForward bus) []
  visit env there

goBack :: Env -> NanoUI ()
goBack env = step env busBack busForward

goForward :: Env -> NanoUI ()
goForward env = step env busForward busBack

step :: Env -> (Bus -> Var [Location]) -> (Bus -> Var [Location]) -> NanoUI ()
step env from to = do
  let bus = envBus env
  locs <- readVar (from bus)
  case locs of
    [] -> pure ()
    loc : rest -> do
      here <- Location (envPanel env) <$> readVar (busFocus bus)
      writeVar (from bus) rest
      modifyVar (to bus) (take 100 . (here :))
      visit env loc

-- | Going to an instant moves the cursor there, and going to a range
-- selects it, in every time view of the run.
visit :: Env -> Location -> NanoUI ()
visit env (Location p mref) = do
  let bus = envBus env
  writeVar (busFocus bus) mref
  case mref of
    Just (RInstant r t) -> writeVar (busCursor bus) (Just (r, t))
    Just (RRange r a b) -> writeVar (busRange bus) (Just (r, a, b))
    _ -> pure ()
  for_ mref $ \r ->
    for_ (listToMaybe [l | l <- envLenses env, lensPanel l == p, lensShows l r]) $ \l -> lensReveal l r
  envSwitch env p

setFocus :: Env -> Maybe Ref -> NanoUI ()
setFocus env r = do
  cur <- readVar (busFocus (envBus env))
  when (cur /= r) (writeVar (busFocus (envBus env)) r)

-- | Whether a ref is the focus, and whether the pointer is on it anywhere,
-- for panels that highlight it themselves (canvases).
isFocusOrHover :: Env -> NanoUI (Ref -> Bool, Ref -> Bool)
isFocusOrHover env = do
  focus <- readVar (busFocus (envBus env))
  hover <- readVar (busHover (envBus env))
  pure ((\r -> focus == Just r), (\r -> hover == Just r))

-- ---------------------------------------------------------------------------
-- Widgets

-- | Make a widget stand for an entity. Resting the pointer on it peeks;
-- right-click opens its actions; Ctrl+click goes to its home panel. Returns
-- whether it was clicked without Ctrl, which also focuses it, so the caller
-- can do what a click did before.
linked :: (HasResponse r) => Env -> Ref -> r -> NanoUI Bool
linked env ref = linkedWith False env ref

-- | 'linked'; with @navigate@, a plain click goes to it too.
linkedWith :: (HasResponse r) => Bool -> Env -> Ref -> r -> NanoUI Bool
linkedWith navigate env ref r = do
  let resp = toResponse r
      bus = envBus env
  when (respHovered resp) (liftIO (writeIORef (busHoverNext bus) (Just ref)))
  _ <- tooltipWidgetConfigured peekConfig resp (peek env ref)
  _ <- contextMenu resp (refMenu env ref)
  ctrl <- modCtrl . inputModifiers <$> askInput
  let clicked = respClicked resp
  when clicked $ if ctrl || navigate then goTo env ref else setFocus env (Just ref)
  pure (clicked && not ctrl)

peekConfig :: TooltipConfig
peekConfig = defaultTooltipConfig {tooltipDelay = 0.7}

-- | A full-width list row for an entity, like 'selectableRow'. It is shaded
-- when selected in its panel or in focus anywhere, and washed faintly when
-- the pointer is on the same entity in another panel.
linkRow :: Env -> Ref -> Bool -> (Layout -> Layout) -> NanoUI () -> NanoUI Bool
linkRow env ref selected f body = do
  th <- uiTheme
  (isFocus, isHover) <- isFocusOrHover env
  let wash = alpha 0.06 (inkText (ink th))
      lit = selected || isFocus ref
      bg
        | lit = themeSelection th
        | isHover ref = wash
        | otherwise = colorTransparent
  (_, area) <- mouseArea (fillW . tight) $
    styled (panelStyle (background bg . hoverBackground (if lit then themeSelection th else wash) . borderWidth 0 . cornerRadius 4)) $
      panelWith (asRow . fillW . padXY 8 2 . gap 8 . f) body
  linked env ref area

-- | A name that is a link, like a hyperlink: underlined under the pointer,
-- and a plain click goes to it.
linkText :: Env -> Ref -> (Layout -> Layout) -> Text -> NanoUI ()
linkText env ref f txt = do
  sid <- currentId
  prev <- lastRect sid
  mouse <- uiMousePos
  let under = maybe False (`rectContains` mouse) prev
  (_, area) <- withCursorShape UiCursorPointer $ mouseArea tight (labelWith (f . (if under then fontUnderline else id)) txt)
  void (linkedWith True env ref area)

-- | 'kv' whose value is a link.
kvLink :: Env -> Text -> Ref -> Text -> NanoUI ()
kvLink env k ref v = rowWith (fillW . tight . gap 8) $ do
  labelWith (alignBaseline . fontMuted) k
  flex
  linkText env ref alignBaseline v

-- | Links for a canvas that draws many entities: @refAt@ names the one at
-- a point. Right-click opens its actions and Ctrl+click goes to it. The
-- canvas keeps its own hover text and click handling.
canvasLinks :: Env -> Response -> (V2 -> Maybe Ref) -> NanoUI ()
canvasLinks env resp refAt = do
  mouse <- uiMousePos
  let bus = envBus env
      here = if respHovered resp then refAt mouse else Nothing
  for_ here (liftIO . writeIORef (busHoverNext bus) . Just)
  when (respRightClicked resp) (liftIO (writeIORef (busMenu bus) here))
  _ <- contextMenu resp $
    liftIO (readIORef (busMenu bus)) >>= \case
      Just r -> refMenu env r
      Nothing -> menuItemDisabled "Nothing to link to here"
  ctrl <- modCtrl . inputModifiers <$> askInput
  when (respClicked resp && ctrl) (for_ here (goTo env))

-- | Every panel's actions for an entity: show it in each panel that can,
-- go to what goes with it, and the panels' own actions.
refMenu :: Env -> Ref -> NanoUI ()
refMenu env ref = do
  let ls = envLenses env
      shownBy r = any (`lensShows` r) ls
      related = nub [(lbl, r) | l <- ls, (lbl, r) <- lensRelated l ref, shownBy r]
  menuHeader (T.take 60 (refTitle env ref))
  forM_ [l | l <- ls, lensShows l ref] $ \l ->
    whenM (menuItem ("Show in " <> panelTitle (lensPanel l))) (showIn env l ref)
  forM_ related $ \(lbl, r) -> whenM (menuItem lbl) (goTo env r)
  forM_ (concatMap (`lensActions` ref) ls) $ \a -> case actRun a of
    Right run -> whenM (menuItem (actLabel a)) run
    Left why -> menuItemDisabled (actLabel a <> " (" <> why <> ")")
  menuSeparator
  whenM (menuItem "Focus everywhere") (setFocus env (Just ref))
  whenM (menuItem "Copy reference") (void (setClipboard (refTarget ref)))

-- | What every panel knows about an entity, for its tooltip.
peek :: Env -> Ref -> NanoUI ()
peek env ref =
  columnWith (padXY 4 2 . gap 4 . maxW 460 . tight) $ do
    labelWith (fontMedium . fillW) (refTitle env ref)
    let ls = envLenses env
    forM_ [(lensPanel l, body) | l <- ls, Just body <- [lensPeek l ref]] $ \(p, body) -> withKey (fromEnum p) $ do
      labelWith (fontMuted . fontSizeScale 0.8 . padTop 4) (panelTitle p)
      columnWith (fillW . tight . gap 2) body
    let related = nub [(lbl, r) | l <- ls, (lbl, r) <- lensRelated l ref]
    scope $ unless (null related) $
      columnWith (padTop 4 . fillW . tight . gap 2) $
        forM_ (zip [0 :: Int ..] related) $ \(i, (lbl, r)) -> withKey i $ fact lbl (refTitle env r)
    labelWith (fontMuted . fontSizeScale 0.8 . padTop 4) "Ctrl+click to open, right-click for actions"

-- | A piece of prose that links to an entity ('prose').
refInline :: Ref -> Text -> Inline
refInline ref = hyperlink (refTarget ref)

-- | A paragraph whose links go to the entities they name.
prose :: Env -> [Inline] -> NanoUI ()
prose env inlines = do
  target <- richTextWith fillW inlines
  for_ (target >>= parseRefTarget) (goTo env)
