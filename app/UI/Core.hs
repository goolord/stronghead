-- | Source, Core and STG side by side for one module. Bindings are listed
-- by name with what the optimiser did to them; selecting one lines up all
-- three panes, and hovering any name highlights it everywhere.
module UI.Core
  ( CoreState (..)
  , newCoreState
  , coreLoad
  , coreView
  , openCodeFile
  , openDumpDir
  , CoreLoaded (..)
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import NanoUI hiding (Binding)
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, askOpenFolderDialog, defaultFileDialogOptions, useFileDialog)
import Stronghead.Dump
import Stronghead.Format (tshow)
import System.FilePath (takeFileName)
import UI.Code
import UI.Common

data CoreState = CoreState
  { csRoot :: !(Var (Maybe FilePath))
  , csDirDialog :: !(Var (Maybe FileDialogId))
  , csFileDialog :: !(Var (Maybe FileDialogId))
  , csDiscover :: !(Task FilePath [ModuleArtifacts])
  , csModule :: !(Var Int)
  , csLoadedModule :: !(Var (Maybe ModuleArtifacts))
    -- ^ The module whose files are in 'csPaths', when picked from a folder.
  , csPaths :: !(Var (Maybe FilePath, Maybe FilePath, Maybe FilePath))
    -- ^ Source, Core, STG.
  , csTasks :: !(Task FilePath (Either String CodeFile), Task FilePath (Either String CodeFile), Task FilePath (Either String CodeFile))
  , csHideUniques :: !(Var Bool)
  , csHideTypeable :: !(Var Bool)
  , csFilter :: !(Var Text)
  , csSelected :: !(Var (Maybe Text))
  , csHot :: !(Var (Maybe Text))
  , csViews :: !(CodeViewState, CodeViewState, CodeViewState)
  , csShowSource :: !(Var Bool)
  }

newCoreState :: IO CoreState
newCoreState = do
  csRoot <- newVar Nothing
  csDirDialog <- newVar Nothing
  csFileDialog <- newVar Nothing
  csDiscover <- newTask
  csModule <- newVar 0
  csLoadedModule <- newVar Nothing
  csPaths <- newVar (Nothing, Nothing, Nothing)
  srcTask <- newTask
  coreTask <- newTask
  stgTask <- newTask
  let csTasks = (srcTask, coreTask, stgTask)
  csHideUniques <- newVar True
  csHideTypeable <- newVar True
  csFilter <- newVar ""
  csSelected <- newVar Nothing
  csHot <- newVar Nothing
  srcView <- newCodeViewState
  coreView' <- newCodeViewState
  stgView <- newCodeViewState
  let csViews = (srcView, coreView', stgView)
  csShowSource <- newVar True
  pure CoreState {..}

-- | Put a file in the pane its kind belongs to.
openCodeFile :: CoreState -> FilePath -> NanoUI ()
openCodeFile st p = for_ (kindForPath p) $ \k -> do
  modifyVar (csPaths st) $ \(s, c, g) -> case k of
    KindSource -> (Just p, c, g)
    KindCore -> (s, Just p, g)
    KindStg -> (s, c, Just p)
  writeVar (csSelected st) Nothing

openDumpDir :: CoreState -> FilePath -> NanoUI ()
openDumpDir st dir = do
  writeVar (csRoot st) (Just dir)
  writeVar (csModule st) 0
  writeVar (csLoadedModule st) Nothing

data CoreLoaded = CoreLoaded
  { clArtifacts :: !(Maybe [ModuleArtifacts])
  , clSource :: !(Maybe (Either String CodeFile))
  , clCore :: !(Maybe (Either String CodeFile))
  , clStg :: !(Maybe (Either String CodeFile))
  }

coreLoad :: CoreState -> NanoUI CoreLoaded
coreLoad st = do
  root <- readVar (csRoot st)
  arts <- traverse (\r -> useTask (csDiscover st) r (discoverArtifacts r)) root
  (s, c, g) <- readVar (csPaths st)
  let (ts, tc, tg) = csTasks st
      load task kind = fmap (maybe Nothing id) . traverse (\p -> useTask task p (loadCodeFile kind p))
      clArtifacts = maybe Nothing id arts
  clSource <- load ts KindSource s
  clCore <- load tc KindCore c
  clStg <- load tg KindStg g
  pure CoreLoaded {..}

-- | One entry in the binding list: a name key and what each file has for it.
data Entry = Entry
  { enKey :: !Text
  , enCore :: ![Binding]
  , enStg :: ![Binding]
  , enSrc :: ![Binding]
  }

entries :: Bool -> Maybe CodeFile -> Maybe CodeFile -> Maybe CodeFile -> [Entry]
entries hideTypeable src core stg =
  [ Entry k (find' core k) (find' stg k) (find' src k)
  | k <- nub (keys core ++ keys stg ++ keys src)
  , not (hideTypeable && (isTypeableBinding k || T.null k))
  ]
  where
    keys = maybe [] (\cf -> map bdKey (V.toList (cfBindings cf)))
    find' mcf k = case mcf of
      Nothing -> []
      Just cf -> [cfBindings cf V.! i | i <- M.findWithDefault [] k (cfByKey cf)]

hasWorker :: Entry -> Bool
hasWorker e = any (("$w" `T.isPrefixOf`) . stripQualifier . bdName) (enCore e ++ enStg e)

coreView :: CoreState -> CoreLoaded -> NanoUI ()
coreView st loaded = columnWith (gap 8 . grow . padAll 10) $ do
  useFileDialog (csDirDialog st) $ \case
    p : _ -> openDumpDir st p
    [] -> pure ()
  useFileDialog (csFileDialog st) (mapM_ (openCodeFile st))
  let arts = fromMaybe [] (clArtifacts loaded)
  rowWith (tight . gap 8 . alignMid . fillW) $ do
    whenM (button "Open folder…") $
      writeVar (csDirDialog st) . Just =<< askOpenFolderDialog defaultFileDialogOptions
    whenM (button "Open files…") $
      writeVar (csFileDialog st) . Just
        =<< askOpenFileDialog
          defaultFileDialogOptions
            { dialogAllowMany = True
            , dialogFilters = [FileFilter "Dumps and source" "dump-simpl;dump-stg-final;dump-stg;hs", FileFilter "All files" "*"]
            }
    scope $ unless (null arts) $ do
      label "Module"
      selectWith (minW 220) (map maModule arts) (csModule st)
      i <- readVar (csModule st)
      current <- readVar (csLoadedModule st)
      -- Load whichever module the picker shows (the first one after a
      -- folder is opened).
      for_ (listToMaybe (drop (max 0 i) arts)) $ \a ->
        when (current /= Just a) $ do
          writeVar (csLoadedModule st) (Just a)
          writeVar (csPaths st) (maSource a, maCore a, maStg a)
          writeVar (csSelected st) Nothing
    flex
    checkboxWith alignMid "Hide uniques" (csHideUniques st)
    checkboxWith alignMid "Hide Typeable" (csHideTypeable st)
    checkboxWith alignMid "Source pane" (csShowSource st)
  root <- readVar (csRoot st)
  scope $ when (isJust root && isNothing (clArtifacts loaded)) $ hint "Searching for dumps…"
  scope $ when (isJust root && clArtifacts loaded == Just []) $
    hint "No .dump-simpl or .dump-stg-final files found under that folder."
  let ok = \case
        Just (Right cf) -> Just cf
        _ -> Nothing
      src = ok (clSource loaded)
      core = ok (clCore loaded)
      stg = ok (clStg loaded)
      errors = [T.pack e | Just (Left e) <- [clSource loaded, clCore loaded, clStg loaded]]
  forM_ errors (labelWith (fontTone Danger))
  if isNothing core && isNothing stg && isNothing src
    then scope intro
    else scope (panes st src core stg)

intro :: NanoUI ()
intro = columnWith (gap 6 . padAll 24) $ do
  sectionTitle "Core & STG pipeline viewer"
  hint "Compile with dumps written to files, then open the folder (the dumps are found and paired with their sources):"
  labelWith fontMono "  ghc -O2 -ddump-simpl -ddump-stg-final -ddump-to-file -dsuppress-module-prefixes Main.hs"
  labelWith fontMono "  cabal build --ghc-options=\"-ddump-simpl -ddump-stg-final -ddump-to-file\""
  hint "Cabal puts the dumps next to the object files under dist-newstyle/. Add -dsuppress-uniques for shorter names, or tick Hide uniques here."

panes :: CoreState -> Maybe CodeFile -> Maybe CodeFile -> Maybe CodeFile -> NanoUI ()
panes st src core stg = do
  hideU <- readVar (csHideUniques st)
  hideT <- readVar (csHideTypeable st)
  filt <- T.toLower <$> readVar (csFilter st)
  selected <- readVar (csSelected st)
  hot <- readVar (csHot st)
  showSrc <- readVar (csShowSource st)
  mm <- monoMetrics 0.88
  th <- uiTheme
  let pal = palette th
      es = entries hideT src core stg
      shown = [e | e <- es, T.null filt || filt `T.isInfixOf` T.toLower (enKey e)]
      selEntry = selected >>= \k -> listToMaybe [e | e <- es, enKey e == k]
      (vSrc, vCore, vStg) = csViews st
      select k = do
        writeVar (csSelected st) (Just k)
        for_ (listToMaybe [e | e <- es, enKey e == k]) $ \e -> do
          for_ (listToMaybe (enSrc e)) (scrollToLine vSrc . bdFirst)
          for_ (listToMaybe (enCore e)) (scrollToLine vCore . bdFirst)
          for_ (listToMaybe (enStg e)) (scrollToLine vStg . bdFirst)
  rowWith (tight . gap 10 . grow) $ do
    -- Binding list.
    panelWith (fixedW 270 . fillH . padAll 6 . gap 4) $ do
      searchInput "Filter bindings" (csFilter st)
      labelWith (fontMuted . fontSizeScale 0.85) (tshow (length shown) <> " bindings")
      scrollWith (grow . tight) $ columnWith (tight . fillW . gap 0) $
        forM_ shown $ \e -> withKey (enKey e) $ do
          picked <- selectableRow (selected == Just (enKey e)) id $ do
            labelWith (fillW . fontMono . fontSizeScale 0.9) (T.take 34 (enKey e))
            entryBadges e (isJust core)
          when picked (select (enKey e))
    columnWith (tight . gap 6 . grow) $ do
      scope $ for_ selEntry $ \e -> notesBar st e core stg
      rowWith (tight . gap 8 . grow) $ do
        let pane title cv mcf kind = columnWith (tight . gap 4 . grow) $ do
              rowWith (tight . gap 6 . alignMid . fillW) $ do
                labelWith fontMedium title
                labelWith (fontMuted . fontSizeScale 0.85) (maybe "not loaded" (T.pack . takeFileName . cfPath) mcf)
              let ranges = case (selEntry, mcf) of
                    (Just e, Just _) ->
                      [ (bdFirst b, bdLast b, alpha 0.10 (pAccent pal))
                      | b <- case kind of
                          KindSource -> enSrc e
                          KindCore -> enCore e
                          KindStg -> enStg e
                      ]
                    _ -> []
                  allocMarks = case (selEntry, kind) of
                    (Just e, KindStg) -> concatMap (nAllocs . bdNotes) (enStg e)
                    _ -> []
                  boxMarks = case (selEntry, kind) of
                    (Just e, KindCore) -> concatMap (nBoxing . bdNotes) (enCore e)
                    _ -> []
              codeView
                cv
                mm
                defaultCodeConfig
                  { ccLineCount = maybe 0 (V.length . cfLines) mcf
                  , ccTokens = \l -> maybe [] (`lineTokens` l) mcf
                  , ccHideUniques = hideU && kind /= KindSource
                  , ccHot = hot
                  , ccRanges = ranges
                  , ccGutter = \l ->
                      if
                        | l `elem` allocMarks -> Just (MarkDot (pBlocked pal))
                        | l `elem` boxMarks -> Just (MarkDot (pGCMajor pal))
                        | otherwise -> Nothing
                  , ccEmpty = "Open a " <> title <> " file"
                  }
        evSrc <-
          if showSrc
            then scope (Just <$> pane "Source" vSrc src KindSource)
            else scope (pure Nothing)
        evCore <- pane "Core" vCore core KindCore
        evStg <- pane "STG" vStg stg KindStg
        let events = maybe [] pure evSrc ++ [evCore, evStg]
            hoverNow = listToMaybe (mapMaybe ceHoverKey events)
        when (hoverNow /= hot) (writeVar (csHot st) hoverNow)
        -- Clicking a name jumps to its binding, if it has one.
        for_ (listToMaybe (mapMaybe ceClickedKey events)) $ \k ->
          when (any ((== k) . enKey) es) (select k)

entryBadges :: Entry -> Bool -> NanoUI ()
entryBadges e haveCore = do
  let notes = map bdNotes (enCore e)
      inl = listToMaybe (mapMaybe nInline notes)
      boxes = sum (map (length . nBoxing) notes)
      allocs = sum (map (length . nAllocs . bdNotes) (enStg e))
  scope $ when (hasWorker e) $ badge Success "W/W"
  scope $ for_ inl $ \p -> when ("INLINE" `T.isInfixOf` p || "NOINLINE" `T.isInfixOf` p) $ badge Accent (if "NOINLINE" `T.isInfixOf` p then "NOINLINE" else "INLINE")
  scope $ when (boxes > 0) $ badge Warning ("box " <> tshow boxes)
  scope $ when (allocs > 0) $ badge Danger ("let " <> tshow allocs)
  scope $ when (haveCore && null (enCore e) && not (null (enSrc e))) $ badge Muted "gone"

notesBar :: CoreState -> Entry -> Maybe CodeFile -> Maybe CodeFile -> NanoUI ()
notesBar st e core stg = panelWith (padXY 10 6 . gap 4 . fillW) $ do
  let (_, vCore, vStg) = csViews st
  rowWith (tight . gap 12 . wrap . fillW . alignMid) $ do
    labelWith (fontMedium . fontMono) (enKey e)
    scope $ forM_ (enCore e) $ \b -> withKey (bdName b) $ rowWith (tight . gap 8 . alignMid) $ do
      labelWith (fontMono . fontMuted) (stripQualifier (bdName b))
      let n = bdNotes b
      for_ (nArity n) $ \a -> fact "arity" (tshow a)
      for_ (nStrictness n) $ \s -> factMono "Str" (T.take 40 s)
      for_ (nInline n) $ \p -> factMono "InlPrag" p
      for_ (nSize n) $ \s -> labelWith (fontMuted . fontSizeScale 0.85) (T.take 60 s)
      when (nRecursive n) $ badge Muted "recursive"
      when (nJoins n > 0) $ badge Success (tshow (nJoins n) <> " join points")
  let verdicts =
        concat
          [ [ "Not in the optimised Core: inlined into its callers (or dropped as dead code)." | isJust core, null (enCore e), not (null (enSrc e)) ]
          , [ "Worker/wrapper fired: the wrapper unboxes the arguments and calls $w" <> enKey e <> "." | hasWorker e ]
          , [ "No worker/wrapper split: arguments stay boxed across calls." | isJust core, not (hasWorker e), not (null (enCore e)), any ((> 0) . fromMaybe 0 . nArity . bdNotes) (enCore e), unboxed == 0 ]
          , [ "Re-boxes values (" <> tshow boxes <> " sites with I#, D#, ...): each one allocates a box." | boxes > 0 ]
          , [ tshow allocs <> " let-bound closures in STG: each is a heap allocation when that code runs." | allocs > 0 ]
          , [ "Works on unboxed values (" <> tshow unboxed <> " mentions of #-types and primops)." | unboxed > 0 ]
          , [ "Still referenced on " <> tshow refs <> " other lines of Core." | Just r <- [refsIn core], let refs = r, refs > 0 ]
          ]
      boxes = sum (map (length . nBoxing . bdNotes) (enCore e))
      allocs = sum (map (length . nAllocs . bdNotes) (enStg e))
      unboxed = sum (map (nUnboxedOps . bdNotes) (enCore e))
      refsIn = fmap $ \cf ->
        let own = concat [[bdFirst b .. bdLast b] | b <- enCore e]
         in length (filter (`notElem` own) (occurrences cf (enKey e)))
  forM_ verdicts (labelWith (fontSizeScale 0.92))
  rowWith (tight . gap 6 . wrap . fillW) $ do
    scope $ forM_ (sortOn id (concatMap (nBoxing . bdNotes) (enCore e))) $ \l -> withKey ("box" <> tshow l) $
      whenM (buttonWith tight ("box @" <> tshow (l + 1))) (scrollToLine vCore l)
    scope $ forM_ (sortOn id (concatMap (nAllocs . bdNotes) (enStg e))) $ \l -> withKey ("let" <> tshow l) $
      whenM (buttonWith tight ("alloc @" <> tshow (l + 1))) (scrollToLine vStg l)
  let _ = stg
  pure ()
