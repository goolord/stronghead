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
  , coreLens
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Data.Vector qualified as V
import NanoUI hiding (Binding)
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, askOpenFolderDialog, defaultFileDialogOptions, useFileDialog)
import Stronghead.Dump
import Stronghead.Format (tshow)
import Stronghead.Ref
import Stronghead.Source (Resolved (..), SrcSpan (..), mkSourceIndex, resolveSource)
import System.FilePath (takeFileName)
import UI.Code
import UI.Common
import UI.Link

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
  , csColouring :: !(Var Colouring)
  , csHovers :: !(Var (Maybe Text), Var (Maybe Text), Var (Maybe Text))
    -- ^ The name under the pointer in each code pane: source, Core, STG.
  , csHideTypeable :: !(Var Bool)
  , csFilter :: !(Var Text)
  , csSelected :: !(Var (Maybe Text))
  , csHot :: !(Var (Maybe Text))
  , csViews :: !(CodeViewState, CodeViewState, CodeViewState)
  , csPanes :: !Workbench
  , csPending :: !(Var (Maybe Ref))
    -- ^ A binding or line another panel asked to show, applied once its
    -- module is loaded.
  }

paneBindings, paneNotes, paneSource, paneCore, paneStg :: Word64
paneBindings = 1
paneNotes = 2
paneSource = 3
paneCore = 4
paneStg = 5

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
  csColouring <- newVar BoxingColours
  hs <- newVar Nothing
  hc <- newVar Nothing
  hg <- newVar Nothing
  let csHovers = (hs, hc, hg)
  csHideTypeable <- newVar True
  csFilter <- newVar ""
  csSelected <- newVar Nothing
  csHot <- newVar Nothing
  srcView <- newCodeViewState
  coreView' <- newCodeViewState
  stgView <- newCodeViewState
  let csViews = (srcView, coreView', stgView)
  csPending <- newVar Nothing
  csPanes <-
    newVar . Just $
      Split 10 AxisV 0.17 (Pane paneBindings) $
        Split 11 AxisH 0.2 (Pane paneNotes) $
          Split 12 AxisV 0.3 (Pane paneSource) (Split 13 AxisV 0.5 (Pane paneCore) (Pane paneStg))
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

coreView :: Env -> CoreState -> CoreLoaded -> NanoUI ()
coreView env st loaded = columnWith (tight . gap 0 . grow) $ do
  useFileDialog (csDirDialog st) $ \case
    p : _ -> openDumpDir st p
    [] -> pure ()
  useFileDialog (csFileDialog st) (mapM_ (openCodeFile st))
  let arts = fromMaybe [] (clArtifacts loaded)
  toolStrip $ do
    whenM (toolButton "Open dump folder…") $
      writeVar (csDirDialog st) . Just =<< askOpenFolderDialog defaultFileDialogOptions
    whenM (toolButton "Open files…") $
      writeVar (csFileDialog st) . Just
        =<< askOpenFileDialog
          defaultFileDialogOptions
            { dialogAllowMany = True
            , dialogFilters = [FileFilter "Dumps and source" "dump-simpl;dump-stg-final;dump-stg;hs", FileFilter "All files" "*"]
            }
    scope $ unless (null arts) $ toolGroup $ do
      toolLabel "Module"
      selectWith (alignMid . minW 220) (map maModule arts) (csModule st)
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
    toolGroup $ do
      toolLabel "Colour"
      rowWith (alignMid . tight) $
        boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} colouringName (csColouring st)
    toolGroup $ checkboxWith alignMid "Hide uniques" (csHideUniques st)
    checkboxWith alignMid "Hide Typeable bindings" (csHideTypeable st)
  root <- readVar (csRoot st)
  let ok = \case
        Just (Right cf) -> Just cf
        _ -> Nothing
      src = ok (clSource loaded)
      core = ok (clCore loaded)
      stg = ok (clStg loaded)
      errors = [T.pack e | Just (Left e) <- [clSource loaded, clCore loaded, clStg loaded]]
  scope $ unless (null errors) $
    columnWith (padXY 20 0 . padBottom 8 . tight . gap 2 . fillW) $ forM_ errors (labelWith (fontTone Danger))
  if
    | isJust root && isNothing (clArtifacts loaded) -> scope $ emptyState "Searching for dumps…" [] []
    | isJust root && clArtifacts loaded == Just [] && isNothing core && isNothing stg ->
        scope $ emptyState "No dumps in that folder" ["No .dump-simpl or .dump-stg-final files were found under it. Build with -ddump-to-file, then open the folder again."] dumpCommands
    | isNothing core && isNothing stg && isNothing src -> scope intro
    | otherwise -> scope (panes env st src core stg)

dumpCommands :: [Text]
dumpCommands =
  [ "ghc -O2 -ddump-simpl -ddump-stg-final -ddump-to-file Main.hs"
  , "cabal build --ghc-options=\"-ddump-simpl -ddump-stg-final -ddump-to-file\""
  ]

intro :: NanoUI ()
intro =
  emptyState
    "Open a folder of Core and STG dumps"
    [ "Compile with the dumps written to files, then open the folder. Each module's source, Core and STG are found and paired."
    , "Cabal puts the dumps next to the object files under dist-newstyle/."
    ]
    dumpCommands

panes :: Env -> CoreState -> Maybe CodeFile -> Maybe CodeFile -> Maybe CodeFile -> NanoUI ()
panes env st src core stg = do
  hideU <- readVar (csHideUniques st)
  hideT <- readVar (csHideTypeable st)
  colouring <- readVar (csColouring st)
  filt <- T.toLower <$> readVar (csFilter st)
  selected <- readVar (csSelected st)
  hot <- readVar (csHot st)
  mm <- monoMetrics 0.88
  th <- uiTheme
  let pal = palette th
      es = entries hideT src core stg
      shown = [e | e <- es, T.null filt || filt `T.isInfixOf` T.toLower (enKey e)]
      selEntry = selected >>= \k -> listToMaybe [e | e <- es, enKey e == k]
      (vSrc, vCore, vStg) = csViews st
      modName = moduleOf src core stg
      select k = do
        writeVar (csSelected st) (Just k)
        for_ (listToMaybe [e | e <- es, enKey e == k]) $ \e -> do
          for_ (listToMaybe (enSrc e)) (scrollToLine vSrc . bdFirst)
          for_ (listToMaybe (enCore e)) (scrollToLine vCore . bdFirst)
          for_ (listToMaybe (enStg e)) (scrollToLine vStg . bdFirst)
  -- Another panel's request, once the module it names is the one loaded.
  pending <- readVar (csPending st)
  for_ pending $ \case
    RBinding m k | T.null m || m == modName -> do
      writeVar (csPending st) Nothing
      when (any ((== k) . enKey) es) (select k)
    RSource sp | Just cf <- src, inFile cf sp -> do
      writeVar (csPending st) Nothing
      for_ (listToMaybe [enKey e | e <- es, b <- enSrc e, bdFirst b <= ssLine sp - 1, bdLast b >= ssLine sp - 1]) select
      scrollToLine vSrc (ssLine sp - 1)
    _ -> pure ()
  focus <- readVar (busFocus (envBus env))
  let focusLines = case (focus, src) of
        (Just (RSource sp), Just cf) | inFile cf sp -> [(ssLine sp - 1, ssEndLine sp - 1, alpha 0.16 (pAccent pal))]
        _ -> []
      codeRef kind mcf ev = case ceHoverKey ev of
        Just k | any ((== k) . enKey) es -> Just (RBinding modName k)
        _ -> case (kind, mcf, ceHoverLine ev) of
          (KindSource, Just cf, Just l) -> Just (RSource (SrcSpan (T.pack (cfPath cf)) (l + 1) 1 (l + 1) 1))
          _ -> Nothing
      codePane ctx title cv mcf kind = paneWith tight ctx title (paneMeta (maybe "not loaded" (T.pack . takeFileName . cfPath) mcf)) $ do
        let ranges = case (selEntry, mcf) of
              (Just e, Just _) ->
                [ (bdFirst b, bdLast b, alpha 0.10 (pAccent pal))
                | b <- case kind of
                    KindSource -> enSrc e
                    KindCore -> enCore e
                    KindStg -> enStg e
                ]
              _ -> []
              ++ (if kind == KindSource then focusLines else [])
            allocMarks = case (selEntry, kind) of
              (Just e, KindStg) -> concatMap (nAllocs . bdNotes) (enStg e)
              _ -> []
            boxMarks = case (selEntry, kind) of
              (Just e, KindCore) -> concatMap (nBoxing . bdNotes) (enCore e)
              _ -> []
        ev <-
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
              , ccColouring = colouring
              }
        -- Hovering a name lights it up in every pane; clicking one jumps to
        -- its binding, if it has one.
        let (hs, hc, hg) = csHovers st
        writeVar (case kind of KindSource -> hs; KindCore -> hc; KindStg -> hg) (ceHoverKey ev)
        canvasLinks env (ceResp ev) (const (codeRef kind mcf ev))
        for_ (ceClickedKey ev) $ \k -> when (any ((== k) . enKey) es) (select k)
  workbench (csPanes st) $ \pid ctx ->
    if
      | pid == paneBindings ->
          pane ctx "Bindings" (paneMeta (tshow (length shown))) $ do
            searchInputConfigured defaultSearchInputConfig {sicPlaceholder = "Filter bindings", sicLayout = fillW defaultLayout} (csFilter st)
            paneScroll (grow . tight) $ columnWith (tight . fillW . gap 0) $
              forM_ shown $ \e -> withKey (enKey e) $ do
                picked <- linkRow env (RBinding modName (enKey e)) (selected == Just (enKey e)) id $ do
                  cell (fillW . fontMono . fontSizeScale 0.9) (T.take 34 (enKey e))
                  entryBadges e (isJust core)
                when picked (select (enKey e))
      | pid == paneNotes ->
          pane ctx (maybe "What the optimiser did" enKey selEntry) (pure ()) $
            paneScroll (grow . tight) $ columnWith (tight . gap 6 . fillW) $
              case selEntry of
                Nothing -> hint "Select a binding to see what the optimiser did to it: worker/wrapper, inlining, boxing, and the closures STG allocates."
                Just e -> notesBar st e core stg
      | pid == paneSource -> codePane ctx "Source" vSrc src KindSource
      | pid == paneCore -> codePane ctx "Core" vCore core KindCore
      | otherwise -> codePane ctx "STG" vStg stg KindStg
  let (hs, hc, hg) = csHovers st
  hoverNow <- listToMaybe . concatMap (maybe [] pure) <$> mapM readVar [hs, hc, hg]
  when (hoverNow /= hot) (writeVar (csHot st) hoverNow)

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

-- | The module the loaded files are of.
moduleOf :: Maybe CodeFile -> Maybe CodeFile -> Maybe CodeFile -> Text
moduleOf src core stg = maybe "" cfModule (listToMaybe (concatMap (maybe [] pure) [core, stg, src]))

-- | Whether a span is in this file, as far as its path says.
inFile :: CodeFile -> SrcSpan -> Bool
inFile cf sp = case resolveSource (mkSourceIndex [] [cfPath cf]) Nothing (ssFile sp) of
  Just _ -> True
  Nothing -> False

-- ---------------------------------------------------------------------------
-- Lens

-- | What the Core panel can show of the entities other panels name: the
-- bindings and lines of the loaded module, and any module or source file of
-- the dump folder, which it loads first.
coreLens :: CoreState -> CoreLoaded -> NanoUI Lens
coreLens st loaded = do
  let ok = \case
        Just (Right cf) -> Just cf
        _ -> Nothing
      src = ok (clSource loaded)
      core = ok (clCore loaded)
      stg = ok (clStg loaded)
      arts = fromMaybe [] (clArtifacts loaded)
      es = entries False src core stg
      modName = moduleOf src core stg
      sources = maybe [] (pure . cfPath) src ++ [f | a <- arts, Just f <- [maSource a]]
      ix = mkSourceIndex [] sources
      fileOf sp = rsPath <$> resolveSource ix Nothing (ssFile sp)
      here m = T.null m || m == modName
      entry m k = if here m then listToMaybe [e | e <- es, enKey e == k] else Nothing
      artIndex p = listToMaybe [i | (i, a) <- zip [0 ..] arts, p a]
      loadedFile = cfPath <$> src
      entryAt l = listToMaybe [e | e <- es, b <- enSrc e, bdFirst b <= l, bdLast b >= l]
      switchTo p = for_ (artIndex p) (writeVar (csModule st))
      binding m k = "binding " <> (if T.null m then k else m <> "." <> k)
  pure
    (lens PCore)
      { lensShows = \case
          RBinding m k -> isJust (entry m k) || isJust (artIndex ((== m) . maModule))
          RModule m -> m == modName || isJust (artIndex ((== m) . maModule))
          RSource sp -> isJust (fileOf sp)
          _ -> False
      , lensReveal = \r -> case r of
          RBinding m _ -> do
            unless (here m) $ switchTo ((== m) . maModule)
            writeVar (csPending st) (Just r)
          RModule m -> unless (m == modName) $ switchTo ((== m) . maModule)
          RSource sp -> for_ (fileOf sp) $ \f -> do
            unless (loadedFile == Just f) $ switchTo ((== Just f) . maSource)
            writeVar (csPending st) (Just r)
          _ -> pure ()
      , lensTitle = \case
          RBinding m k | isJust (entry m k) -> Just (binding modName k)
          _ -> Nothing
      , lensRelated = \case
          RBinding m k -> case (entry m k, src) of
            (Just e, Just cf) ->
              [ ("Go to source", RSource (SrcSpan (T.pack (cfPath cf)) (bdFirst b + 1) 1 (bdLast b + 1) 1))
              | b <- take 1 (enSrc e)
              ]
            _ -> []
          RSource sp | fmap Just (fileOf sp) == Just loadedFile -> [("Go to binding " <> enKey e, RBinding modName (enKey e)) | Just e <- [entryAt (ssLine sp - 1)]]
          _ -> []
      , lensPeek = \case
          RBinding m k -> entryPeek (isJust core) <$> entry m k
          RSource sp | fmap Just (fileOf sp) == Just loadedFile -> entryPeek (isJust core) <$> entryAt (ssLine sp - 1)
          _ -> Nothing
      , lensHits =
          [Hit "binding" (enKey e) modName (HitRef (RBinding modName (enKey e))) | e <- es, not (isTypeableBinding (enKey e)), not (T.null (enKey e))]
            ++ [Hit "module" (maModule a) (maybe "" (T.pack . takeFileName) (maSource a)) (HitRef (RModule (maModule a))) | a <- arts]
      }

-- | A binding's badges and what the optimiser recorded about it.
entryPeek :: Bool -> Entry -> NanoUI ()
entryPeek haveCore e = do
  rowWith (tight . gap 6 . wrap . fillW) $ do
    labelWith (fontMono . alignMid) (enKey e)
    entryBadges e haveCore
  scope $ forM_ (take 3 (enCore e)) $ \b -> withKey (bdName b) $ do
    let n = bdNotes b
    rowWith (tight . gap 12 . wrap . fillW) $ do
      for_ (nArity n) $ \a -> fact "arity" (tshow a)
      for_ (nStrictness n) $ \s -> factMono "Str" (T.take 40 s)
  scope $ for_ (listToMaybe (enSrc e)) $ \b ->
    labelWith (fontMuted . fontSizeScale 0.85) ("source lines " <> tshow (bdFirst b + 1) <> "–" <> tshow (bdLast b + 1))
