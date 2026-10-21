-- | Entity inspector pane showing suggested steps, per-panel facts, related
-- links, and actions for the focused item.
module UI.Inspector
  ( Inspector (..)
  , newInspector
  , inspectorSync
  , inspect
  , inspectorPane
  , inspectorBody
  , nextStepsSection
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Text qualified as T
import NanoUI
import Stronghead.Format (tshow)
import Stronghead.Next (Step (..), StepDo (..), datumRecipe, datumTitle)
import Stronghead.Ref (Ref (..), refTarget)
import Stronghead.Source (renderSrcSpan)
import UI.Common
import UI.Link

data Inspector = Inspector
  { inShown :: !(Var (Maybe Ref))
  , inBack :: !(Var [Ref])
    -- ^ Most recent first.
  , inForward :: !(Var [Ref])
  , inFollow :: !(Var Bool)
    -- ^ Show the focus whenever it changes.
  , inSeen :: !(Var (Maybe Ref))
    -- ^ The focus last taken up.
  }

newInspector :: IO Inspector
newInspector = do
  inShown <- newVar Nothing
  inBack <- newVar []
  inForward <- newVar []
  inFollow <- newVar True
  inSeen <- newVar Nothing
  pure Inspector {..}

-- | Take up a new focus. Call every frame, whether or not the inspector is
-- on screen, so it shows the latest focus when it comes back.
inspectorSync :: Env -> Inspector -> NanoUI ()
inspectorSync env ins = do
  focus <- readVar (busFocus (envBus env))
  seen <- readVar (inSeen ins)
  when (focus /= seen) $ do
    writeVar (inSeen ins) focus
    follow <- readVar (inFollow ins)
    when follow (for_ focus (inspect ins))

-- | Show an entity, remembering what was shown.
inspect :: Inspector -> Ref -> NanoUI ()
inspect ins r = do
  cur <- readVar (inShown ins)
  unless (cur == Just r) $ do
    for_ cur $ \c -> modifyVar (inBack ins) (take 50 . (c :) . filter (/= c))
    writeVar (inForward ins) []
    writeVar (inShown ins) (Just r)

-- | Step back or forward through what the inspector has shown. The step
-- focuses the entity too, so every panel lights it up.
stepInspector :: Env -> Inspector -> (Inspector -> Var [Ref]) -> (Inspector -> Var [Ref]) -> NanoUI ()
stepInspector env ins from to = do
  refs <- readVar (from ins)
  case refs of
    [] -> pure ()
    r : rest -> do
      cur <- readVar (inShown ins)
      writeVar (from ins) rest
      for_ cur $ \c -> modifyVar (to ins) (take 50 . (c :))
      writeVar (inShown ins) (Just r)
      writeVar (inSeen ins) (Just r)
      setFocus env (Just r)

inspectorPane :: Env -> Inspector -> PaneCtx -> NanoUI ()
inspectorPane env ins ctx = do
  shown <- readVar (inShown ins)
  back <- readVar (inBack ins)
  forward <- readVar (inForward ins)
  let step label refs go = do
        resp <- disabledWhen (null refs) (buttonWith' (alignMid . padXY 8 2 . tight) label)
        for_ (take 1 refs) $ \r -> tooltip resp (refTitle env r)
        when (respClicked resp) go
      header = do
        step "←" back (stepInspector env ins inBack inForward)
        step "→" forward (stepInspector env ins inForward inBack)
        flex
        checkboxWith alignMid "Follow" (inFollow ins)
  paneWith tight ctx "Inspector" header $
    paneScroll (grow . tight) $
      columnWith (padAll 12 . gap 6 . fillW) $ case shown of
        Nothing -> hint "Click a thread, a band, a binding or a line of source anywhere: everything Stronghead knows about it shows here."
        Just r -> inspectorBody env r

-- | Everything known about an entity: its name and reference, where it is
-- in the source, and a section per panel.
inspectorBody :: Env -> Ref -> NanoUI ()
inspectorBody env r = do
  let bus = envBus env
      ls = envLenses env
      shownBy x = any (`lensShows` x) ls
  labelWith (fontMedium . fontSizeScale 1.05 . fillW) (refTitle env r)
  labelWith (fontMono . fontMuted . fontSizeScale 0.8 . fillW) (refTarget r)
  rowWith (fillW . tight . gap 6 . wrap . lineGap 6) $ do
    pinned <- isPinned bus r
    whenM (buttonWith (padXY 8 2 . tight) (if pinned then "Unpin" else "Pin")) (togglePin bus r)
    focus <- readVar (busFocus bus)
    scope $ unless (focus == Just r) $ whenM (buttonWith (padXY 8 2 . tight) "Focus everywhere") (setFocus env (Just r))
    disabledWhen (not (shownBy r)) $ whenM (buttonWith (padXY 8 2 . tight) "Go to") (goTo env r)
    whenM (buttonWith (padXY 8 2 . tight) "Copy reference") (void (setClipboard (refTarget r)))
  scope $ case (r, refSpan env r) of
    (RSource _, _) -> pure ()
    (_, Just sp) -> kvLink env "source" (RSource sp) (renderSrcSpan sp)
    _ -> pure ()
  scope $ nextStepsSection env (stepsFor env r)
  let sections =
        [ (l, peek, related, actions)
        | l <- ls
        , let peek = lensPeek l r
              related = nubOrd [(lbl, x) | (lbl, x) <- lensRelated l r, shownBy x]
              actions = lensActions l r
        , lensShows l r || not (null related) || not (null actions) || isJust peek
        ]
  scope $
    if null sections
      then hint "No panel has this loaded now. Open the artifact it comes from, or connect to the program."
      else forM_ sections $ \(l, peek, related, actions) -> withKey (fromEnum (lensPanel l)) $
        columnWith (fillW . gap 4 . padTop 6 . tight) $ do
          labelWith (fontMedium . fontSizeScale 0.92) (panelTitle (lensPanel l))
          for_ (peek >>= peekExact) $ labelWith (fontMuted . fontSizeScale 0.82 . fillW)
          for_ peek $ columnWith (fillW . tight . gap 2) . drawPeek False
          forM_ (zip [0 :: Int ..] related) $ \(i, (lbl, x)) -> withKey i $
            rowWith (fillW . tight . gap 8) $ do
              linkText env x alignBaseline (T.take 60 lbl)
              labelWith (alignBaseline . fontMuted . fontSizeScale 0.85 . fillW) (T.take 60 (refTitle env x))
          rowWith (fillW . tight . gap 6 . wrap . lineGap 6) $ do
            scope $ when (lensShows l r) $
              whenM (buttonWith (padXY 8 2 . tight) ("Show in " <> panelTitle (lensPanel l))) (showIn env l r)
            forM_ (zip [0 :: Int ..] actions) $ \(i, a) -> withKey i $
              actionButton (padXY 8 2 . tight) (actLabel a) Nothing (actRun a)

-- | What to do next, in order: each step a button that follows it (its
-- reason under it, or why it can't run as its tooltip), the entity it
-- leads to as a link, and for missing data the lines that add it.
nextStepsSection :: Env -> [Step] -> NanoUI ()
nextStepsSection env steps = unless (null steps) $
  columnWith (fillW . gap 4 . padTop 6 . tight) $ do
    labelWith (fontMedium . fontSizeScale 0.92) "Next steps"
    forM_ (zip [1 :: Int ..] steps) $ \(i, s) -> withKey i $
      columnWith (fillW . tight . gap 2) $ do
        rowWith (fillW . tight . gap 8) $ do
          labelWith (alignMid . fontMuted . fontSizeScale 0.85 . fixedW 14) (tshow i)
          let run = stepRun env s
          scope $ case stepDo s of
            -- Missing data has nothing to click: what to add is below it.
            StepLoad _ -> labelWith (alignMid . fontMedium . fontSizeScale 0.9) (stepTitle s)
            _ -> actionButton (alignMid . padXY 8 2 . tight) (stepTitle s) Nothing run
          scope $ case stepDo s of
            StepGo x -> linkText env x (alignMid . fontSizeScale 0.9) (T.take 60 (refTitle env x))
            StepTry flag -> do
              labelWith (alignMid . fontMono . fontSizeScale 0.85) flag
              whenM (buttonWith (alignMid . padXY 6 0 . tight) "Copy") (void (setClipboard flag))
            _ -> pure ()
        labelWith (padLeft 22 . fillW . fontMuted . fontSizeScale 0.85) (stepReason s)
        scope $ case stepDo s of
          StepLoad d -> do
            let (build, runWith) = datumRecipe d
            labelWith (padLeft 22 . fillW . fontSizeScale 0.85) (datumTitle d)
            forM_ (zip [0 :: Int ..] ([("build", b) | Just b <- [build]] ++ [("run", x) | Just x <- [runWith]])) $ \(j, (what, line)) -> withKey j $
              rowWith (padLeft 22 . fillW . tight . gap 6) $ do
                labelWith (alignMid . fixedW 34 . fontMuted . fontSizeScale 0.8) what
                labelWith (alignMid . fillW . fontMono . fontSizeScale 0.8) line
                whenM (buttonWith (alignMid . padXY 6 0 . tight) "Copy") (void (setClipboard line))
          _ -> pure ()
