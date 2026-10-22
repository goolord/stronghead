-- | Side-by-side Source, Core, and STG viewer with binding summaries and
-- runtime metric annotations.
module UI.Core
  ( CoreState (..)
  , newCoreState
  , coreLoad
  , coreView
  , corePanes
  , openCodeFile
  , openDumpDir
  , CoreLoaded (..)
  , coreLens
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, join, unless, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (asum, for_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Word (Word64)
import System.Directory (makeAbsolute)
import System.FilePath (isAbsolute, normalise, takeFileName)
import System.Process (spawnCommand)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import NanoUI hiding (Binding)
import NanoUI.Backend.Sdl (FileDialogId, FileDialogOptions (..), FileFilter (..), askOpenFileDialog, askOpenFolderDialog, defaultFileDialogOptions, useFileDialog)

import Stronghead.Dump
import Stronghead.Format (fmtBytesD, fmtFixed, tshow)
import Stronghead.Join
import Stronghead.Next (Fact (..))
import Stronghead.Ref
import Stronghead.Source (Resolved (..), SourceIndex (..), SrcSpan (..), buildSourceIndex, editorCommand, mkSourceIndex, resolveModule, resolveSource, resolvesTo, shortFileName, sourceRoots)
import UI.Code
import UI.Common
import UI.Link
import UI.Workspace

data CoreState = CoreState
  { csRoot :: !(Var (Maybe FilePath))
  , csDirDialog :: !(Var (Maybe FileDialogId))
  , csFileDialog :: !(Var (Maybe FileDialogId))
  , csDiscover :: !(Task FilePath [ModuleArtifacts])
  , csModule :: !(Var Int)
    -- ^ The module picked from the folder; -1 while a file from elsewhere
    -- is open.
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
  , csProjectRoot :: !(Var Text)
    -- ^ Where to look for sources, from Settings.
  , csEditor :: !(Var Text)
    -- ^ The command that opens a file at a line, from Settings.
  , csIndex :: !(Task [FilePath] SourceIndex)
  , csSites :: !(Var (Maybe (FilePath, Int, StgSites)))
    -- ^ The loaded STG dump's closure sites, by path and length.
  , csSort :: !(Var BindingSort)
  , csHotBoxed :: !(Var Bool)
  , csMessage :: !(Var Text)
  , csEntriesCache :: !(IORef (Maybe ((Maybe FilePath, Maybe FilePath, Maybe FilePath), [Entry], M.Map Text Entry)))
  , csDerivedCache :: !(IORef (Maybe (CoreDerivedKey, CoreDerived)))
  , csTabGroups :: !(Var (M.Map Word64 [Word64]))
    -- ^ For each code pane in the workbench, the ordered list of code tabs
    -- ('paneCoreSource', 'paneCoreCore', 'paneCoreStg') it holds.
  , csActiveTabs :: !(Var (M.Map Word64 Word64))
    -- ^ For each code pane in the workbench, the tab currently in front.
  , csTabGestures :: !(IORef (M.Map Word64 (DragHandle Word64)))
  , csTabHashes :: !(IORef (M.Map Word64 Int))
  , csTabJumpRev :: !(IORef (M.Map Word64 Int))
  }

type CoreDerivedKey = (Bool, Maybe FilePath, Maybe FilePath, Maybe FilePath, Int, [FilePath], [Annotation])

data CoreDerived = CoreDerived
  { cdEntries :: ![Entry]
  , cdByKey :: !(M.Map Text Entry)
  , cdSrcNotes :: !FileNotes
  , cdStgPlaced :: ![(Int, Int, Annotation)]
  , cdStgNotes :: !FileNotes
  , cdRuntime :: !(M.Map Text Runtime)
  , cdLanes :: ![Lane]
  , cdHeapTotal :: !Double
  , cdTickyTotal :: !Double
  }

-- | The order of the binding list.
data BindingSort = InFileOrder | ByTime | ByAlloc | ByHeap | ByEntries | ByTicky
  deriving stock (Eq, Show)

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
  csPanes <- newWorkbench "core" (panelPreset PCore)
  csProjectRoot <- newVar ""
  csEditor <- newVar ""
  csIndex <- newTask
  csSites <- newVar Nothing
  csSort <- newVar InFileOrder
  csHotBoxed <- newVar False
  csMessage <- newVar ""
  csEntriesCache <- newIORef Nothing
  csDerivedCache <- newIORef Nothing
  csTabGroups <- newVar (M.singleton paneCoreSource [paneCoreSource, paneCoreCore, paneCoreStg])
  csActiveTabs <- newVar (M.singleton paneCoreSource paneCoreSource)
  csTabGestures <- newIORef M.empty
  csTabHashes <- newIORef M.empty
  csTabJumpRev <- newIORef M.empty
  pure CoreState {..}

codeTabIds :: [Word64]
codeTabIds = [paneCoreSource, paneCoreCore, paneCoreStg]

paneForKind :: CodeKind -> Word64
paneForKind = \case
  KindSource -> paneCoreSource
  KindCore -> paneCoreCore
  KindStg -> paneCoreStg

-- | Bring a code tab to the front in whichever pane holds it.
activateCodeTab :: CoreState -> Word64 -> NanoUI ()
activateCodeTab st tid = do
  groups <- readVar (csTabGroups st)
  let hosts = nubOrd (paneCoreSource : [h | (h, ts) <- M.toList groups, tid `elem` ts])
  for_ hosts $ \h ->
    modifyVar (csActiveTabs st) (M.insert h tid)

-- | Reconcile the stored tab groups with the code panes currently in the
-- workbench tree.
reconcileTabGroups ::
  [Word64] ->
  M.Map Word64 [Word64] ->
  M.Map Word64 Word64 ->
  (M.Map Word64 [Word64], M.Map Word64 Word64)
reconcileTabGroups hostPanes groups0 actives0
  | null hostPanes = (groups0, actives0)
  | otherwise =
      let claim (seen, acc) h =
            let raw = M.findWithDefault [h] h groups0
                valid =
                  [ t
                  | t <- raw
                  , t `elem` codeTabIds
                  , not (t `elem` seen)
                  , t == h || not (t `elem` hostPanes)
                  ]
                tabsH = if null valid then [h | not (h `elem` seen)] else valid
                seen' = seen ++ tabsH
             in (seen', (h, tabsH) : acc)
          (claimed, pairsRev) = foldl' claim ([], []) hostPanes
          pairs0 = reverse pairsRev
          unclaimed = [t | t <- codeTabIds, not (t `elem` claimed)]
          groups1 = case pairs0 of
            [] -> M.empty
            (h0, ts0) : rest -> M.fromList ((h0, ts0 ++ unclaimed) : rest)
          actives1 =
            M.fromList
              [ (h, act')
              | (h, ts) <- M.toList groups1
              , let cur = M.findWithDefault h h actives0
                    act' = if cur `elem` ts then cur else fromMaybe h (listToMaybe ts)
              ]
       in (groups1, actives1)

paneTabGesture :: CoreState -> Word64 -> NanoUI (DragHandle Word64)
paneTabGesture st pid = do
  known <- liftIO (M.lookup pid <$> readIORef (csTabGestures st))
  case known of
    Just g -> pure g
    Nothing -> liftIO $ do
      g <- newDrag
      atomicModifyIORef' (csTabGestures st) (\m -> (M.insert pid g m, g))

commitCodeTabDrop ::
  Workbench ->
  CoreState ->
  Word64 ->
  Word64 ->
  Maybe (Word64, Int) ->
  Maybe PaneDropLocation ->
  NanoUI ()
commitCodeTabDrop bench st draggedTab fromPid mStripDrop mGridDrop = do
  mTree <- readVar (wbTree bench)
  let hostPanes = [p | p <- maybe [] treeKinds mTree, p `elem` codeTabIds]
  groups0 <- readVar (csTabGroups st)
  actives0 <- readVar (csActiveTabs st)
  let (groups, actives) = reconcileTabGroups hostPanes groups0 actives0
      fromTabs = M.findWithDefault [fromPid] fromPid groups
      remFrom = filter (/= draggedTab) fromTabs
  case mStripDrop of
    Just (targetPid, idx)
      | targetPid == fromPid -> do
          let (before, after) = splitAt (max 0 (min (length remFrom) idx)) remFrom
              newTabs = before ++ [draggedTab] ++ after
          writeVar (csTabGroups st) (M.insert fromPid newTabs groups)
          writeVar (csActiveTabs st) (M.insert fromPid draggedTab actives)
      | otherwise -> do
          let targetTabs0 = filter (/= draggedTab) (M.findWithDefault [targetPid] targetPid groups)
              (before, after) = splitAt (max 0 (min (length targetTabs0) idx)) targetTabs0
              newTargetTabs = before ++ [draggedTab] ++ after
          case remFrom of
            [] -> do
              let groups' = M.insert targetPid newTargetTabs (M.delete fromPid groups)
                  actives' = M.insert targetPid draggedTab (M.delete fromPid actives)
              writeVar (csTabGroups st) groups'
              writeVar (csActiveTabs st) actives'
              modifyVar (wbTree bench) (>>= removePane fromPid)
            firstRem : _
              | fromPid == draggedTab -> do
                  let groups' =
                        M.insert targetPid newTargetTabs
                          . M.insert firstRem remFrom
                          $ M.delete fromPid groups
                      oldAct = M.findWithDefault firstRem fromPid actives
                      newFromAct = if oldAct `elem` remFrom then oldAct else firstRem
                      actives' =
                        M.insert targetPid draggedTab
                          . M.insert firstRem newFromAct
                          $ M.delete fromPid actives
                  writeVar (csTabGroups st) groups'
                  writeVar (csActiveTabs st) actives'
                  modifyVar (wbTree bench) (fmap (renamePane fromPid firstRem))
              | otherwise -> do
                  let oldAct = M.findWithDefault firstRem fromPid actives
                      newFromAct = if oldAct `elem` remFrom then oldAct else firstRem
                      groups' = M.insert targetPid newTargetTabs (M.insert fromPid remFrom groups)
                      actives' = M.insert targetPid draggedTab (M.insert fromPid newFromAct actives)
                  writeVar (csTabGroups st) groups'
                  writeVar (csActiveTabs st) actives'
    Nothing -> for_ mGridDrop $ \loc ->
      case remFrom of
        [] ->
          modifyVar (wbTree bench) (fmap (splitAtDrop loc draggedTab))
        firstRem : _
          | fromPid == draggedTab -> do
              let loc' = case loc of
                    BesidePane p e | p == fromPid -> BesidePane firstRem e
                    other -> other
                  oldAct = M.findWithDefault firstRem fromPid actives
                  newFromAct = if oldAct `elem` remFrom then oldAct else firstRem
                  groups' =
                    M.insert draggedTab [draggedTab]
                      . M.insert firstRem remFrom
                      $ M.delete fromPid groups
                  actives' =
                    M.insert draggedTab draggedTab
                      . M.insert firstRem newFromAct
                      $ M.delete fromPid actives
              writeVar (csTabGroups st) groups'
              writeVar (csActiveTabs st) actives'
              modifyVar (wbTree bench) (fmap (splitAtDrop loc' draggedTab . renamePane fromPid firstRem))
          | otherwise -> do
              let oldAct = M.findWithDefault firstRem fromPid actives
                  newFromAct = if oldAct `elem` remFrom then oldAct else firstRem
                  groups' = M.insert draggedTab [draggedTab] (M.insert fromPid remFrom groups)
                  actives' = M.insert draggedTab draggedTab (M.insert fromPid newFromAct actives)
              writeVar (csTabGroups st) groups'
              writeVar (csActiveTabs st) actives'
              modifyVar (wbTree bench) (fmap (splitAtDrop loc draggedTab))

-- | Put a file in the pane its kind belongs to.
openCodeFile :: CoreState -> FilePath -> NanoUI ()
openCodeFile st p = for_ (kindForPath p) $ \k -> do
  modifyVar (csPaths st) $ \(s, c, g) -> case k of
    KindSource -> (Just p, c, g)
    KindCore -> (s, Just p, g)
    KindStg -> (s, c, Just p)
  writeVar (csSelected st) Nothing
  activateCodeTab st (paneForKind k)

-- | Open a source file on its own, away from the dump folder's modules.
openSourceAlone :: CoreState -> FilePath -> NanoUI ()
openSourceAlone st p = do
  writeVar (csModule st) (-1)
  writeVar (csLoadedModule st) Nothing
  writeVar (csPaths st) (Just p, Nothing, Nothing)
  writeVar (csSelected st) Nothing
  activateCodeTab st paneCoreSource

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
  , clIndex :: !SourceIndex
    -- ^ The Haskell files under the project root, the dump folder's
    -- project and the extra roots; empty until the first walk finishes.
  , clSites :: !(Maybe StgSites)
  }

-- | Load the dumps and sources, and index the sources under the project
-- root setting, the dump folder (and the project it is in) and @extra@
-- (the debugger's directory).
coreLoad :: CoreState -> [FilePath] -> NanoUI CoreLoaded
coreLoad st extra = do
  root <- readVar (csRoot st)
  arts <- traverse (\r -> useTask (csDiscover st) r (discoverArtifacts r)) root
  projectRoot <- T.unpack . T.strip <$> readVar (csProjectRoot st)
  let starts = nubOrd (filter (not . null) (projectRoot : maybe [] pure root ++ extra))
  ix <- useTask (csIndex st) starts (buildSourceIndex =<< traverse makeAbsolute =<< sourceRoots starts)
  (s, c, g) <- readVar (csPaths st)
  let (ts, tc, tg) = csTasks st
      load task kind = fmap join . traverse (\p -> useTask task p (loadCodeFile kind p))
      clArtifacts = join arts
      clIndex = fromMaybe (mkSourceIndex [] []) ix
  clSource <- load ts KindSource s
  clCore <- load tc KindCore c
  clStg <- load tg KindStg g
  -- A module whose source is not beside its dumps: the index finds it, by
  -- the path the STG's source notes give or else by module name.
  loadedModule <- readVar (csLoadedModule st)
  for_ loadedModule $ \a -> when (isNothing s) $ do
    let stgFile = clStg >>= eitherToMaybe
        found =
          rsPath <$> ((stgFile >>= dumpSourcePath >>= resolveSource clIndex (Just (maModule a))) <|> resolveModule clIndex (maModule a))
    for_ found $ \f -> modifyVar (csPaths st) (\(_, c', g') -> (Just f, c', g'))
  -- The STG dump's closure sites, found once per dump.
  memo <- readVar (csSites st)
  clSites <- case clStg of
    Just (Right cf) -> case memo of
      Just (p, n, sites) | p == cfPath cf, n == V.length (cfLines cf) -> pure (Just sites)
      _ -> do
        let sites = stgSites cf
        writeVar (csSites st) (Just (cfPath cf, V.length (cfLines cf), sites))
        pure (Just sites)
    _ -> pure Nothing
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
  | k <- nubOrd (keys core ++ keys stg ++ keys src)
  , not (hideTypeable && (isTypeableBinding k || T.null k))
  ]
  where
    keys = maybe [] (\cf -> map bdKey (V.toList (cfBindings cf)))
    find' mcf k = case mcf of
      Nothing -> []
      Just cf -> [cfBindings cf V.! i | i <- M.findWithDefault [] k (cfByKey cf)]

hasWorker :: Entry -> Bool
hasWorker e = any (("$w" `T.isPrefixOf`) . stripQualifier . bdName) (enCore e ++ enStg e)

-- | What the runtime recorded for a binding.
data Runtime = Runtime
  { rtTime :: !Double
    -- ^ % of time, from the @.prof@.
  , rtAlloc :: !Double
  , rtHeap :: !Double
    -- ^ Bytes live at the heap cursor.
  , rtEntries :: !Double
    -- ^ Entries into it and the closures it allocates, from ticky.
  , rtTicky :: !Double
    -- ^ Bytes they allocated, from ticky.
  }

noRuntime :: Runtime
noRuntime = Runtime 0 0 0 0 0

-- | Bytes in a narrow column: 44.6M, 968B (binary units).
fmtBytesShort :: Double -> Text
fmtBytesShort b
  | b >= 1024 ^ (3 :: Int) = fmtFixed 1 (b / 1024 ^ (3 :: Int)) <> "G"
  | b >= 1024 ^ (2 :: Int) = fmtFixed 1 (b / 1024 ^ (2 :: Int)) <> "M"
  | b >= 1024 = fmtFixed 1 (b / 1024) <> "K"
  | otherwise = tshow (round b :: Int) <> "B"

-- | A count in a narrow column: 4.2M, 51.6k, 7.
fmtCount :: Double -> Text
fmtCount n
  | n >= 1e9 = fmtFixed 1 (n / 1e9) <> "G"
  | n >= 1e6 = fmtFixed 1 (n / 1e6) <> "M"
  | n >= 1e4 = fmtFixed 1 (n / 1e3) <> "k"
  | otherwise = tshow (round n :: Int)

-- | Annotations indexed for joining them to bindings: those in the source
-- file by the line their span starts on, the rest by the name they give.
data Joinable = Joinable
  { jnByLine :: !(IM.IntMap [Annotation])
  , jnByName :: !(M.Map (Text, Text) [Annotation])
  }

joinable :: Maybe (SrcSpan -> Bool) -> [Annotation] -> Joinable
joinable inSource anns =
  Joinable
    { jnByLine = IM.fromListWith (++) [(ssLine sp, [a]) | Just here <- [inSource], a <- anns, Just sp <- [anSpan a], here sp]
    , jnByName = M.fromListWith (++) [(n, [a]) | a <- anns, Just n <- [anName a]]
    }

-- | A binding's costs: every annotation whose span starts in its source
-- lines (a cost centre's span overlapping the binding, nested ones
-- included), or, with no source binding, every one naming it.
runtimeOf :: Text -> Bool -> Joinable -> Entry -> Runtime
runtimeOf modName haveSource jn e =
  Runtime (total LaneTime) (total LaneAlloc) (total LaneHeap) 0 0
  where
    mine = case listToMaybe (enSrc e) of
      Just b | haveSource ->
        let (_, from) = IM.split (bdFirst b) (jnByLine jn)
            (inside, _) = IM.split (bdLast b + 2) from
         in concat (IM.elems inside)
      _ -> M.findWithDefault [] (modName, enKey e) (jnByName jn)
    total lane = sum [anValue a | a <- mine, anLane a == lane]

-- | What its badges say, for the source pane's note on its first line.
badgeText :: Entry -> Bool -> Text
badgeText e haveCore =
  T.intercalate " · " $
    ["W/W" | hasWorker e]
      ++ [p | Just pr <- [listToMaybe (mapMaybe (nInline . bdNotes) (enCore e))], p <- ["NOINLINE" | "NOINLINE" `T.isInfixOf` pr] ++ ["INLINE" | not ("NOINLINE" `T.isInfixOf` pr), "INLINE" `T.isInfixOf` pr]]
      ++ ["box " <> tshow boxes | boxes > 0]
      ++ ["let " <> tshow allocs | allocs > 0]
      ++ ["gone" | haveCore, null (enCore e), not (null (enSrc e))]
  where
    boxes = sum (map (length . nBoxing . bdNotes) (enCore e))
    allocs = sum (map (length . nAllocs . bdNotes) (enStg e))

coreView :: Env -> Bench -> CoreState -> CoreLoaded -> NanoUI ()
coreView env bench st loaded = columnWith (tight . gap 0 . grow) $ do
  useFileDialog (csDirDialog st) (firstPath (openDumpDir st))
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
      -- folder is opened), unless a file from elsewhere is open.
      for_ (if i < 0 then Nothing else listToMaybe (drop i arts)) $ \a ->
        when (current /= Just a) $ do
          writeVar (csLoadedModule st) (Just a)
          writeVar (csPaths st) (maSource a, maCore a, maStg a)
          writeVar (csSelected st) Nothing
    flex
    msg <- readVar (csMessage st)
    scope $ unless (T.null msg) $ do
      labelWith (alignMid . fontSizeScale 0.85 . fontTone Warning) (T.take 100 msg)
      whenM (buttonWith (alignMid . padXY 6 0 . tight) "Dismiss") (writeVar (csMessage st) "")
    toolGroup $ do
      toolLabel "Colour"
      rowWith (alignMid . tight) $
        boundedTabBarConfigured defaultTabsConfig {tabsStyle = TabSegmented} colouringName (csColouring st)
    toolGroup $ checkboxWith alignMid "Hide uniques" (csHideUniques st)
    checkboxWith alignMid "Hide Typeable bindings" (csHideTypeable st)
  root <- readVar (csRoot st)
  let src = clSource loaded >>= eitherToMaybe
      core = clCore loaded >>= eitherToMaybe
      stg = clStg loaded >>= eitherToMaybe
      errors = [T.pack e | Just (Left e) <- [clSource loaded, clCore loaded, clStg loaded]]
  scope $ unless (null errors) $
    columnWith (padBottom 8 . padXY 20 0 . tight . gap 2 . fillW) $ forM_ errors (labelWith (fontTone Danger))
  mixed <- bnMixed bench (csPanes st)
  if
    | mixed -> scope (bnDraw bench (csPanes st) (corePanes env st loaded))
    | isJust root && isNothing (clArtifacts loaded) -> scope $ emptyState "Searching for dumps…" [] []
    | isJust root && clArtifacts loaded == Just [] && isNothing core && isNothing stg && isNothing src ->
        scope $ emptyState "No dumps in that folder" ["No .dump-simpl or .dump-stg-final files were found under it. Build with -ddump-to-file, then open the folder again."] dumpCommands
    | isNothing core && isNothing stg && isNothing src -> scope intro
    | otherwise -> scope (bnDraw bench (csPanes st) (corePanes env st loaded))

dumpCommands :: [Text]
dumpCommands =
  [ "ghc -O2 -ddump-simpl -ddump-stg-final -ddump-to-file Main.hs"
  , "cabal build --ghc-options=\"-ddump-simpl -ddump-stg-final -ddump-to-file\""
  ]

intro :: NanoUI ()
intro =
  emptyState
    "Open a folder of Core and STG dumps, or a source file"
    [ "Compile with the dumps written to files, then open the folder. Each module's source, Core and STG are found and paired."
    , "Cabal puts the dumps next to the object files under dist-newstyle/. Sources are found beside the dumps, in the project the folder is in, and under the project root set in Settings."
    , "Any source line another panel links to opens here too, annotated with what the runtime saw of it."
    ]
    dumpCommands

-- | Cached full entry list and key map for the currently loaded files.
cachedEntries :: CoreState -> Maybe CodeFile -> Maybe CodeFile -> Maybe CodeFile -> IO ([Entry], M.Map Text Entry)
cachedEntries st src core stg = do
  let key = (cfPath <$> src, cfPath <$> core, cfPath <$> stg)
  cached <- readIORef (csEntriesCache st)
  case cached of
    Just (k, es, byKey) | k == key -> pure (es, byKey)
    _ -> do
      let es = entries False src core stg
          byKey = M.fromList [(enKey e, e) | e <- es]
      writeIORef (csEntriesCache st) (Just (key, es, byKey))
      pure (es, byKey)

-- | Core & STG's panes, for whichever workbench holds them: the binding
-- list, the optimiser's notes, and the source, Core and STG of the module
-- open. Without files each code pane says what to open.
corePanes :: Env -> CoreState -> CoreLoaded -> [Word64] -> NanoUI PaneDraw
corePanes env st loaded sitePanes = do
  let src = clSource loaded >>= eitherToMaybe
      core = clCore loaded >>= eitherToMaybe
      stg = clStg loaded >>= eitherToMaybe
      hostPanes = [p | p <- sitePanes, p `elem` codeTabIds]
  groups0 <- readVar (csTabGroups st)
  actives0 <- readVar (csActiveTabs st)
  let (groups, actives) = reconcileTabGroups hostPanes groups0 actives0
  hideU <- readVar (csHideUniques st)
  hideT <- readVar (csHideTypeable st)
  colouring <- readVar (csColouring st)
  filt <- T.toLower <$> readVar (csFilter st)
  selected <- readVar (csSelected st)
  hot <- readVar (csHot st)
  sortBy' <- readVar (csSort st)
  hotBoxed <- readVar (csHotBoxed st)
  pending <- readVar (csPending st)
  focus <- readVar (busFocus (envBus env))
  mm <- monoMetrics 0.88
  th <- uiTheme
  let pal = palette th
      ix = clIndex loaded
      anns = concatMap lensAnnotations (envLenses env)
      modName = moduleOf src core stg
      tickyLane a = anLane a == LaneEntries || anLane a == LaneTicky
      derivKey = (hideT, cfPath <$> src, cfPath <$> core, cfPath <$> stg, S.size (siFiles ix), siRoots ix, anns)
  CoreDerived
    { cdEntries = es
    , cdByKey = byKey
    , cdSrcNotes = srcNotes
    , cdStgPlaced = stgPlaced
    , cdStgNotes = stgNotes
    , cdRuntime = runtime
    , cdLanes = lanes
    , cdHeapTotal = heapTotal
    , cdTickyTotal = tickyTotal
    } <- liftIO $ do
    cached <- readIORef (csDerivedCache st)
    case cached of
      Just (k, d) | k == derivKey -> pure d
      _ -> do
        (allEs, allByKey) <- cachedEntries st src core stg
        let es'
              | hideT = [e | e <- allEs, not (isTypeableBinding (enKey e) || T.null (enKey e))]
              | otherwise = allEs
            byKey'
              | hideT = M.fromList [(enKey e, e) | e <- es']
              | otherwise = allByKey
            srcNotes' = maybe noNotes (\cf -> notesForFile ix (cfPath cf) anns) src
            reported = M.fromList [(ssFile sp, ()) | a <- anns, Just sp <- [anSpan a]]
            inSource = (\cf -> let here = M.mapWithKey (\f _ -> resolvesTo ix (cfPath cf) f) reported in \sp -> M.findWithDefault False (ssFile sp) here) <$> src
            jn = joinable inSource anns
            tickyHere = [a | a <- anns, tickyLane a, fmap fst (anName a) == Just modName]
            stgOwners = IM.fromList [(bdFirst b, (bdLast b, enKey e)) | e <- es', b <- enStg e]
            stgOwner l = case IM.lookupLE l stgOwners of
              Just (_, (lastLine, k)) | l <= lastLine -> Just k
              _ -> Nothing
            tickyByEntry = M.fromListWith (++) $ case clSites loaded of
              Just sites -> [(k, [a]) | a <- tickyHere, Just t <- [anTable a], Just l <- [siteOfTable sites t Nothing], Just k <- [stgOwner l]]
              Nothing -> [(k, [a]) | a <- tickyHere, Just (_, k) <- [anName a]]
            withTicky e r =
              let as = M.findWithDefault [] (enKey e) tickyByEntry
               in r {rtEntries = sum [anValue a | a <- as, anLane a == LaneEntries], rtTicky = sum [anValue a | a <- as, anLane a == LaneTicky]}
            runtime' = M.fromList [(enKey e, withTicky e (runtimeOf modName (isJust src) jn e)) | e <- es']
            lanes' = [l | l <- [LaneTime, LaneAlloc, LaneHeap], any ((== l) . anLane) anns] ++ [l | not (null tickyHere), l <- [LaneEntries, LaneTicky]]
            heapTotal' = sum [anValue a | a <- anns, anLane a == LaneHeap]
            tickyTotal' = sum [anValue a | a <- tickyHere, anLane a == LaneTicky]
            stgPlaced' = case clSites loaded of
              Just sites ->
                [ (l, l, a)
                | a <- anns
                , anLane a == LaneHeap || tickyLane a && fmap fst (anName a) == Just (sgModule sites)
                , Just t <- [anTable a]
                , Just l <- [siteOfTable sites t (anSpan a)]
                ]
              Nothing -> []
            stgNotes' = placedNotes stgPlaced'
            d = CoreDerived es' byKey' srcNotes' stgPlaced' stgNotes' runtime' lanes' heapTotal' tickyTotal'
        writeIORef (csDerivedCache st) (Just (derivKey, d))
        pure d
  let rt e = M.findWithDefault noRuntime (enKey e) runtime
      boxedOrAlloc e = any (not . null . nBoxing . bdNotes) (enCore e) || any (not . null . nAllocs . bdNotes) (enStg e)
      isHot e =
        let r = rt e
         in rtTime r >= 1 || rtAlloc r >= 1 || rtHeap r >= 0.01 * heapTotal && rtHeap r > 0 || rtTicky r >= 0.01 * tickyTotal && rtTicky r > 0
      sorted = case sortBy' of
        InFileOrder -> id
        ByTime -> sortOn (Down . rtTime . rt)
        ByAlloc -> sortOn (Down . rtAlloc . rt)
        ByHeap -> sortOn (Down . rtHeap . rt)
        ByEntries -> sortOn (Down . rtEntries . rt)
        ByTicky -> sortOn (Down . rtTicky . rt)
      shown =
        sorted
          [ e
          | e <- es
          , T.null filt || filt `T.isInfixOf` T.toLower (enKey e)
          , not hotBoxed || null lanes || (isHot e && boxedOrAlloc e)
          ]
      selEntry = selected >>= (`M.lookup` byKey)
      (vSrc, vCore, vStg) = csViews st
      select k = do
        curSel <- readVar (csSelected st)
        writeVar (csSelected st) (Just k)
        for_ (M.lookup k byKey) $ \e -> do
          for_ (listToMaybe (enSrc e)) $ \b -> do
            scrollToLine vSrc (bdFirst b)
            when (curSel /= Just k) $
              liftIO $ modifyIORef' (csTabJumpRev st) (M.insertWith (+) paneCoreSource 1)
          for_ (listToMaybe (enCore e)) $ \b -> do
            scrollToLine vCore (bdFirst b)
            when (curSel /= Just k) $
              liftIO $ modifyIORef' (csTabJumpRev st) (M.insertWith (+) paneCoreCore 1)
          for_ (listToMaybe (enStg e)) $ \b -> do
            scrollToLine vStg (bdFirst b)
            when (curSel /= Just k) $
              liftIO $ modifyIORef' (csTabJumpRev st) (M.insertWith (+) paneCoreStg 1)
      -- Notes stay in view on the line in focus, and on the STG sites
      -- compiled from it.
      focusSpan = case focus of
        Just (RSource sp) -> Just sp
        _ -> Nothing
      fromFocus a = anRef a == focus && isJust focus || case (focusSpan, anSpan a) of
        (Just f, Just sp) -> ssLine sp >= ssLine f && ssLine sp <= ssEndLine f && sameFile (ssFile sp) (ssFile f)
        _ -> False
      pinnedStg = IS.fromList [l | (l, _, a) <- stgPlaced, fromFocus a]
      pinnedSrc l = case (focusSpan, src) of
        (Just f, Just cf) -> l >= ssLine f - 1 && l <= ssEndLine f - 1 && resolvesTo ix (cfPath cf) (ssFile f)
        _ -> False
      pendingStgLine = case (pending, clSites loaded) of
        (Just r@(RBand _), Just sites) -> listToMaybe [l | a <- anns, anRef a == Just r, Just t <- [anTable a], Just l <- [siteOfTable sites t (anSpan a)]]
        _ -> Nothing
  -- Another panel's request, once the module it names is the one loaded.
  for_ pending $ \case
    RBinding m k | T.null m || m == modName -> do
      writeVar (csPending st) Nothing
      when (M.member k byKey) (select k)
    RSource sp | Just cf <- src, resolvesTo ix (cfPath cf) (ssFile sp) -> do
      writeVar (csPending st) Nothing
      activateCodeTab st paneCoreSource
      for_ (listToMaybe [enKey e | e <- es, b <- enSrc e, bdFirst b <= ssLine sp - 1, bdLast b >= ssLine sp - 1]) select
      scrollToLine vSrc (ssLine sp - 1)
      -- The STG compiled from the line, by its source notes.
      for_ (clSites loaded >>= \sites -> listToMaybe [l | (n, l) <- sgNotes sites, ssLine n == ssLine sp, sameFile (ssFile n) (ssFile sp) || resolvesTo ix (cfPath cf) (ssFile n)]) $ \l ->
        let indent = maybe 0 (\g -> maybe 0 (T.length . T.takeWhile (== ' ')) (cfLines g V.!? l)) stg
         in scrollToLineAt vStg (max 0 (l - 3)) (indent - 6)
    RBand _ | Just l <- pendingStgLine -> do
      writeVar (csPending st) Nothing
      activateCodeTab st paneCoreStg
      scrollToLine vStg l
      liftIO $ modifyIORef' (csTabJumpRev st) (M.insertWith (+) paneCoreStg 1)
    _ -> pure ()
  let focusLines = case (focus, src) of
        (Just (RSource sp), Just cf) | resolvesTo ix (cfPath cf) (ssFile sp) -> [(ssLine sp - 1, ssEndLine sp - 1, alpha 0.16 (pAccent pal))]
        _ -> []
      codeRef kind mcf ev = case ceHoverKey ev of
        Just k | M.member k byKey -> Just (RBinding modName k)
        _ -> case (kind, mcf, ceHoverLine ev) of
          (KindSource, Just cf, Just l) -> Just (RSource (SrcSpan (T.pack (cfPath cf)) (l + 1) 1 (l + 1) 1))
          _ -> Nothing
      tabInfo tid
        | tid == paneCoreCore = ("Core", vCore, core, KindCore, noNotes)
        | tid == paneCoreStg = ("STG", vStg, stg, KindStg, stgNotes)
        | otherwise = ("Source", vSrc, src, KindSource, srcNotes)
      computeTabHash tid = do
        let (tTitle, cv, mcf, kind, notes) = tabInfo tid
        scV <- readVar (cvScroll cv)
        scH <- readVar (cvHScroll cv)
        selNow <- readVar (csSelected st)
        let selEntryNow = selNow >>= (`M.lookup` byKey)
        jumpRev <- liftIO (M.findWithDefault 0 tid <$> readIORef (csTabJumpRev st))
        let ranges =
              ( case (selEntryNow, mcf) of
                  (Just e, Just _) ->
                    [ (bdFirst b, bdLast b)
                    | b <- case kind of
                        KindSource -> enSrc e
                        KindCore -> enCore e
                        KindStg -> enStg e
                    ]
                  _ -> []
              )
                ++ [(a, b) | kind == KindSource, (a, b, _) <- focusLines]
            marks = case (selEntryNow, mcf, kind) of
              (Just e, Just _, KindStg) -> concatMap (nAllocs . bdNotes) (enStg e)
              (Just e, Just _, KindCore) -> concatMap (nBoxing . bdNotes) (enCore e)
              _ -> []
            pinnedList = case (mcf, kind) of
              (Just _, KindStg) -> IS.toList pinnedStg
              (Just _, KindSource) -> [a | (a, _, _) <- focusLines]
              _ -> []
            pathTxt = maybe "" (T.pack . cfPath) mcf
            lineCnt = maybe 0 (V.length . cfLines) mcf
            hideUFlag = if isJust mcf && kind /= KindSource && hideU then (1 :: Int) else 0
            colFlag = if isJust mcf then fromEnum colouring else 0
            notesKey =
              ( laneLegend notes
              , IM.keys (fnBars notes)
              , IM.keys (fnHints notes)
              , IM.keys (fnMarks notes)
              )
            sigTxt =
              T.intercalate
                "|"
                [ tTitle
                , pathTxt
                , tshow lineCnt
                , tshow (round (scV * 100) :: Int)
                , tshow (round (scH * 100) :: Int)
                , tshow ranges
                , tshow marks
                , tshow pinnedList
                , tshow hideUFlag
                , tshow colFlag
                , tshow notesKey
                , tshow jumpRev
                ]
            h = fromIntegral (hashText sigTxt) :: Int
        pure (if h == 0 then 1 else h)
      tabbedCodePane ctx pid = do
        now <- uiTime
        inp <- askInput
        let isMouseClick =
              inputButtonsPressed inp /= mempty
                || inputButtonsReleased inp /= mempty
        when isMouseClick $
          liftIO (writeIORef lastMouseClickRef (now, inputMousePos inp))
        sid <- currentId
        mLastRect <- lastRect sid
        let paneRect = case pcGrid ctx of
              Just g -> pgcRect g
              Nothing -> fromMaybe (Rect 0 0 0 0) mLastRect
            groupTabs = case pcBench ctx of
              Nothing -> [pid]
              Just _ -> M.findWithDefault [pid] pid groups
            activeTab = case pcBench ctx of
              Nothing -> pid
              Just _ -> M.findWithDefault pid pid actives
            (activeTitle, activeCv, activeMcf, activeKind, activeNotes) = tabInfo activeTab
            flashDuration = 0.5 :: Double
            factorFor t0 =
              let el = now - t0
               in if t0 >= 0 && el >= 0 && el < flashDuration
                    then max 0 (min 1 (realToFrac ((1 - el / flashDuration) ** 2)))
                    else 0 :: Float
        flashMap0 <- liftIO (readIORef paneFlashRef)
        let paneFlashKey = "pane:" <> tshow pid
            paneFlashFactor0 = factorFor (fromMaybe (-1.0) (M.lookup paneFlashKey flashMap0))
            i = ink th
            baseBorder = inkRule i
            accent = inkAccent i
            borderColorActive = if paneFlashFactor0 > 0 then lerpColor baseBorder accent paneFlashFactor0 else baseBorder
            borderWidthActive = if paneFlashFactor0 > 0.05 then 2 else 1
        menuOpen <- useVar False
        let metaContent = rowWith (alignMid . tight . gap 8 . padRight 6) $ do
              paneMeta (maybe (if activeKind == KindSource then "not found: set the project root in Settings" else "not loaded") (T.pack . takeFileName . cfPath) activeMcf)
              scope $ unless (T.null (laneLegend activeNotes)) $ paneMeta (laneLegend activeNotes)
              for_ (pcMenu ctx) (themed th . paneMenuWithVar menuOpen)
            trailingMeta = case pcGrid ctx of
              Just g -> paneDragHandle g tight (themed th metaContent)
              Nothing -> themed th metaContent
            tabItems = [tab tid (paneTitle tid) () | tid <- groupTabs]
            renderBar =
              tabBarConfigured'
                (TabsConfig TabContained TabTop trailingMeta)
                (constVar activeTab)
                tabItems
        styled (panelStyle (background (inkPane i) . borderColor borderColorActive . borderWidth borderWidthActive . cornerRadius 6)) $
          panelWith (tight . gap 0 . fillW . fillH) $ do
            resp <- renderBar
            when (tabActive resp /= activeTab) $
              modifyVar (csActiveTabs st) (M.insert pid (tabActive resp))
            for_ (pcGrid ctx) $ \g -> do
              let Rect x y w _ = pgcRect g
                  inHeader = rectContains (Rect x y w 34) (inputMousePos inp)
                  onTab = any (\(_, r) -> rectContains (respRect r) (inputMousePos inp)) (tabHeaders resp)
              when (inHeader && not onTab && pressedIn MouseLeft inp && inputMouseClicks inp >= 2) $
                if pgcMaximized g then pgcRestore g else pgcMaximize g
              when (inHeader && pressedIn MouseRight inp) $
                for_ (pcMenu ctx) (const (writeVar menuOpen True))
            for_ (pcBench ctx) $ \bench -> do
              gesture <- paneTabGesture st pid
              drag <- useDrag gesture (tabHeaders resp)
              for_ drag $ \d -> liftIO $
                if dragPhase d == DragCancelled
                  then writeIORef (wbTabDrag bench) Nothing
                  else do
                    prev <- readIORef (wbTabDrag bench)
                    let tId = dragPayload d
                    writeIORef (wbTabDrag bench) $
                      Just
                        HeldTab
                          { htTab = tId
                          , htTitle = paneTitle tId
                          , htFrom = pid
                          , htFromCount = length groupTabs
                          , htAt = dragAt d
                          , htPhase = dragPhase d
                          , htDrop = prev >>= htDrop
                          , htCommit = commitCodeTabDrop bench st tId pid
                          }
              held <- liftIO (readIORef (wbTabDrag bench))
              for_ held $ \h -> do
                let key = htTab h
                    others = [respRect r | (k, r) <- tabHeaders resp, k /= key]
                    beforeCount = length (takeWhile (/= activeTab) groupTabs)
                    landing = case insertionIndex DragAxisX (tabStripRect resp) others (inputMousePos inp) of
                      Just idx -> Just idx
                      Nothing
                        | htFrom h /= pid
                        , rectContains paneRect (inputMousePos inp) ->
                            Just (1 + beforeCount)
                        | otherwise -> Nothing
                for_ landing $ \idx -> do
                  liftIO $ modifyIORef' (wbTabDrag bench) (fmap (\h' -> h' {htDrop = Just (pid, idx)}))
                  let boundary = case splitAt idx others of
                        (_, r : _) -> rectX r - 1
                        (_, []) -> maybe (rectX (tabStripRect resp) + 2) (\r -> rectX r + rectW r + 1) (listToMaybe (reverse others))
                      Rect _ sy _ sh = tabStripRect resp
                  when (boundary > 0 && sh > 0) $
                    scope $ void $
                      canvasKeyed (contentKey [boundary, sy, sh]) (pinAt 0 0 . grow . pointer PointerPass) $ \_ ->
                        drawRect (Rect (boundary - 1) sy 2 sh) accent
            selNow <- readVar (csSelected st)
            let selEntryNow = selNow >>= (`M.lookup` byKey)
            themed th $ columnWith (fillW . fillH . tight) $ withKey activeTab $ do
              let ranges =
                    ( case (selEntryNow, activeMcf) of
                        (Just e, Just _) ->
                          [ (bdFirst b, bdLast b, alpha 0.10 (pAccent pal))
                          | b <- case activeKind of
                              KindSource -> enSrc e
                              KindCore -> enCore e
                              KindStg -> enStg e
                          ]
                        _ -> []
                    )
                      ++ (if activeKind == KindSource then focusLines else [])
                  allocMarks = case (selEntryNow, activeKind) of
                    (Just e, KindStg) -> IS.fromList (concatMap (nAllocs . bdNotes) (enStg e))
                    _ -> IS.empty
                  boxMarks = case (selEntryNow, activeKind) of
                    (Just e, KindCore) -> IS.fromList (concatMap (nBoxing . bdNotes) (enCore e))
                    _ -> IS.empty
                  tokFn = if hideU && activeKind /= KindSource then lineTokensClean else lineTokens
              ev <-
                annotatedCode
                  env
                  activeCv
                  mm
                  ( withNotes
                      activeNotes
                      defaultCodeConfig
                        { ccLineCount = maybe 0 (V.length . cfLines) activeMcf
                        , ccTokens = \l -> maybe [] (`tokFn` l) activeMcf
                        , ccHideUniques = False
                        , ccHot = hot
                        , ccRanges = ranges
                        , ccGutter = \l ->
                            if
                              | IS.member l allocMarks -> Just (MarkDot (pBlocked pal))
                              | IS.member l boxMarks -> Just (MarkDot (pGCMajor pal))
                              | otherwise -> Nothing
                        , ccEmpty = "Open a " <> activeTitle <> " file"
                        , ccColouring = colouring
                        , ccPinned = case activeKind of
                            KindSource -> pinnedSrc
                            KindStg -> (`IS.member` pinnedStg)
                            KindCore -> const False
                        }
                  )
                  (codeRef activeKind activeMcf)
              -- Hovering a name lights it up in every pane; clicking one jumps to
              -- its binding, if it has one.
              let (hs, hc, hg) = csHovers st
              writeVar (case activeKind of KindSource -> hs; KindCore -> hc; KindStg -> hg) (ceHoverKey ev)
              for_ (ceClickedKey ev) $ \k -> when (M.member k byKey) (select k)
            (lastClickTime, lastClickPos) <- liftIO (readIORef lastMouseClickRef)
            let recentClick = lastClickTime >= 0 && (now - lastClickTime) >= 0 && (now - lastClickTime) < 0.40
                clickInThisPane = rectContains paneRect lastClickPos
                isScrollingThisPane = inputScroll inp /= V2 0 0 && rectContains paneRect (inputMousePos inp)
            forM_ groupTabs $ \tid -> do
              curHash <- computeTabHash tid
              prevHash <- liftIO (M.findWithDefault 0 tid <$> readIORef (csTabHashes st))
              let tabHdrRect = maybe (Rect 0 0 0 0) respRect (lookup tid (tabHeaders resp))
                  clickOnThisTabHdr = rectContains tabHdrRect lastClickPos
                  attentionDivertedForTab
                    | tid == activeTab = recentClick && not clickInThisPane && not isScrollingThisPane
                    | otherwise = recentClick && not clickOnThisTabHdr
              when (prevHash /= 0 && curHash /= prevHash && attentionDivertedForTab) $ do
                let (tTitle, _, _, _, _) = tabInfo tid
                liftIO $ do
                  recordTabFlash tTitle tid now
                  when (tid == activeTab && not clickInThisPane) $
                    recordTabFlash paneFlashKey pid now
              when (curHash /= prevHash) $
                liftIO $ modifyIORef' (csTabHashes st) (M.insert tid curHash)
            flashMap <- liftIO (readIORef paneFlashRef)
            let tabFlashFactor tid =
                  let (tTitle, _, _, _, _) = tabInfo tid
                      t0 = fromMaybe (-1.0) (M.lookup tTitle flashMap <|> M.lookup (tshow tid) flashMap)
                   in factorFor t0
                paneFlashFactor = factorFor (fromMaybe (-1.0) (M.lookup paneFlashKey flashMap))
                anyFlashing = paneFlashFactor > 0 || any (\tid -> tabFlashFactor tid > 0) groupTabs
            when anyFlashing $ wakeAfter 0.016
            forM_ (tabHeaders resp) $ \(tid, hdrResp) -> do
              let ff = tabFlashFactor tid
                  hr0 = respRect hdrResp
              scope $ when (ff > 0.05 && rectW hr0 > 0 && rectH hr0 > 0) $ do
                let hr = rectInflate (-1) hr0
                    fillCol = alpha (0.24 * ff) accent
                    strokeCol = lerpColor baseBorder accent ff
                void $
                  canvasKeyed (contentKey [fromIntegral tid, rectX hr, rectY hr, rectW hr, rectH hr, ff]) (pinAt 0 0 . grow . pointer PointerPass) $ \_ -> do
                    drawRoundedRect hr 4 fillCol
                    drawStrokeRoundedRect hr 4 2 strokeCol
  -- The name under the pointer in any code pane last frame lights it up
  -- in all of them.
  let (hs0, hc0, hg0) = csHovers st
  hoverNow <- asum <$> traverse readVar [hs0, hc0, hg0]
  when (hoverNow /= hot) (writeVar (csHot st) hoverNow)
  pure $ \pid ctx ->
    if
      | pid == paneCoreBindings -> Just $
          pane ctx "Bindings" (paneMeta (tshow (length shown))) $ do
            searchInputConfigured defaultSearchInputConfig {sicPlaceholder = "Filter bindings", sicLayout = fillW defaultLayout} (csFilter st)
            scope $ unless (null lanes) $ do
              checkboxWith fillW "Hot and boxed only" (csHotBoxed st)
              rowWith (padXY 8 0 . tight . gap 6 . fillW) $ do
                sortHeader st fillW "binding" InFileOrder
                forM_ lanes $ \l -> withKey (laneName l) $ case l of
                  LaneTime -> sortHeader st (fixedW 38 . alignEnd) "time" ByTime
                  LaneAlloc -> sortHeader st (fixedW 38 . alignEnd) "alloc" ByAlloc
                  LaneEntries -> sortHeader st (fixedW 44 . alignEnd) "entries" ByEntries
                  LaneTicky -> sortHeader st (fixedW 46 . alignEnd) "ticky" ByTicky
                  _ -> sortHeader st (fixedW 52 . alignEnd) "heap" ByHeap
            paneScroll (grow . tight) $ columnWith (tight . fillW . gap 0) $
              forM_ shown $ \e -> withKey (enKey e) $ do
                let r = rt e
                picked <- linkRow env (RBinding modName (enKey e)) (selected == Just (enKey e)) id $ do
                  -- With several columns, the badges go under the name so
                  -- the numbers keep their place.
                  if length lanes >= 3 || LaneTicky `elem` lanes
                    then columnWith (fillW . minW 72 . tight . gap 2 . alignMid) $ do
                      labelWith (fillW . fontMono . fontSizeScale 0.9) (T.take 24 (enKey e))
                      rowWith (tight . gap 4 . wrap . fillW) (entryBadges e (isJust core))
                    else do
                      cell (fillW . minW 72 . fontMono . fontSizeScale 0.9) (T.take (if null lanes then 34 else 24) (enKey e))
                      entryBadges e (isJust core)
                  forM_ lanes $ \l -> withKey (laneName l) $ case l of
                    LaneTime -> numCell 38 (costTone (rtTime r)) (pct (rtTime r))
                    LaneAlloc -> numCell 38 (costTone (rtAlloc r)) (pct (rtAlloc r))
                    LaneEntries -> numCell 44 (if rtEntries r > 0 then id else fontMuted) (if rtEntries r > 0 then fmtCount (rtEntries r) else "·")
                    LaneTicky -> numCell 46 (if rtTicky r > 0 then id else fontMuted) (if rtTicky r > 0 then fmtBytesShort (rtTicky r) else "·")
                    _ -> numCell 52 (if rtHeap r > 0 then id else fontMuted) (if rtHeap r > 0 then fmtBytesD (rtHeap r) else "·")
                when picked (select (enKey e))
      | pid == paneCoreNotes -> Just $
          pane ctx (maybe "What the optimiser did" enKey selEntry) (pure ()) $
            paneScroll (grow . tight) $ columnWith (tight . gap 6 . fillW) $
              case selEntry of
                Nothing -> hint "Select a binding to see what the optimiser did to it: worker/wrapper, inlining, boxing, and the closures STG allocates."
                Just e -> notesBar st e core (rt e) (not (null lanes))
      | pid `elem` codeTabIds -> Just (tabbedCodePane ctx pid)
      | otherwise -> Nothing
  where
    pct v = if v > 0 then fmtFixed 1 v else "·"
    costTone v = if v >= 1 then id else fontMuted

-- | A column header that sorts the binding list.
sortHeader :: CoreState -> (Layout -> Layout) -> Text -> BindingSort -> NanoUI ()
sortHeader st f name order = do
  current <- readVar (csSort st)
  let txt = if current == order && order /= InFileOrder then name <> " ▾" else name
  (_, area) <- withCursorShape UiCursorPointer $ mouseArea (f . tight) (labelWith (fontMuted . fontSizeScale 0.85 . (if current == order then fontMedium else id)) txt)
  when (respClicked (toResponse area)) (writeVar (csSort st) order)

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

notesBar :: CoreState -> Entry -> Maybe CodeFile -> Runtime -> Bool -> NanoUI ()
notesBar st e core r haveRuntime = panelWith (padXY 10 6 . gap 4 . fillW) $ do
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
  scope $ when haveRuntime $ rowWith (tight . gap 16 . wrap . fillW) $ do
    fact "time" (fmtFixed 1 (rtTime r) <> "%")
    fact "allocation" (fmtFixed 1 (rtAlloc r) <> "%")
    fact "heap at the cursor" (fmtBytesD (rtHeap r))
    scope $ when (rtEntries r > 0 || rtTicky r > 0) $ do
      fact "ticky entries" (fmtCount (rtEntries r))
      fact "ticky allocation" (fmtBytesD (rtTicky r))
  let verdicts =
        concat
          [ [ "Not in the optimised Core: inlined into its callers (or dropped as dead code)." | isJust core, null (enCore e), not (null (enSrc e)) ]
          , [ "Worker/wrapper fired: the wrapper unboxes the arguments and calls $w" <> enKey e <> "." | hasWorker e ]
          , [ "No worker/wrapper split: arguments stay boxed across calls." | isJust core, not (hasWorker e), not (null (enCore e)), any ((> 0) . fromMaybe 0 . nArity . bdNotes) (enCore e), unboxed == 0 ]
          , [ "Re-boxes values (" <> tshow boxes <> " sites with I#, D#, ...): each one allocates a box." | boxes > 0 ]
          , [ tshow allocs <> " let-bound closures in STG: each is a heap allocation when that code runs." | allocs > 0 ]
          , [ "Works on unboxed values (" <> tshow unboxed <> " mentions of #-types and primops)." | unboxed > 0 ]
          , [ "Still referenced on " <> tshow refs <> " other lines of Core." | Just refs <- [refsIn core], refs > 0 ]
          , [ "Hot and boxed: it costs " <> fmtFixed 1 (max (rtTime r) (rtAlloc r)) <> "% and allocates boxes or closures, which is where unboxing pays." | haveRuntime, max (rtTime r) (rtAlloc r) >= 1, boxes + allocs > 0 ]
          ]
      boxes = sum (map (length . nBoxing . bdNotes) (enCore e))
      allocs = sum (map (length . nAllocs . bdNotes) (enStg e))
      unboxed = sum (map (nUnboxedOps . bdNotes) (enCore e))
      refsIn = fmap $ \cf ->
        let inOwn l = any (\b -> l >= bdFirst b && l <= bdLast b) (enCore e)
         in length (filter (not . inOwn) (occurrences cf (enKey e)))
  forM_ verdicts (labelWith (fontSizeScale 0.92))
  rowWith (tight . gap 6 . wrap . fillW) $ do
    scope $ forM_ (sort (concatMap (nBoxing . bdNotes) (enCore e))) $ \l -> withKey ("box" <> tshow l) $
      whenM (buttonWith tight ("box @" <> tshow (l + 1))) $ do
        scrollToLine vCore l
        liftIO $ modifyIORef' (csTabJumpRev st) (M.insertWith (+) paneCoreCore 1)
    scope $ forM_ (sort (concatMap (nAllocs . bdNotes) (enStg e))) $ \l -> withKey ("let" <> tshow l) $
      whenM (buttonWith tight ("alloc @" <> tshow (l + 1))) $ do
        scrollToLine vStg l
        liftIO $ modifyIORef' (csTabJumpRev st) (M.insertWith (+) paneCoreStg 1)

-- | The module the loaded files are of.
moduleOf :: Maybe CodeFile -> Maybe CodeFile -> Maybe CodeFile -> Text
moduleOf src core stg = maybe "" cfModule (asum [core, stg, src])

-- | Open a file in the editor Settings names, at a line.
openInEditor :: CoreState -> Text -> FilePath -> Int -> Int -> NanoUI ()
openInEditor st template file line col = do
  wake <- askWake
  liftIO . void . forkIO $ do
    r <- try (void (spawnCommand (T.unpack (editorCommand template file line col))))
    case r of
      Left (e :: SomeException) -> writeVarIO (csMessage st) ("Could not start the editor: " <> T.pack (show e)) >> wake
      Right () -> pure ()

-- ---------------------------------------------------------------------------
-- Lens

-- | What the Core panel can show of the entities other panels name: the
-- bindings and lines of the loaded module, any module or source file the
-- dump folder or the source index has (loading it first), and the STG
-- sites of heap bands. It opens any source span in the editor, and says
-- where a binding's source is.
--
-- @spanOf@ is where the other panels put an entity in the source; @anns@
-- are their annotations, for the bands whose STG sites it knows.
coreLens :: CoreState -> CoreLoaded -> (Ref -> Maybe SrcSpan) -> [Annotation] -> NanoUI Lens
coreLens st loaded spanOf anns = do
  editor <- T.strip <$> readVar (csEditor st)
  let src = clSource loaded >>= eitherToMaybe
      core = clCore loaded >>= eitherToMaybe
      stg = clStg loaded >>= eitherToMaybe
      arts = fromMaybe [] (clArtifacts loaded)
      ix = clIndex loaded
  (es, byKey) <- liftIO (cachedEntries st src core stg)
  let modName = moduleOf src core stg
      -- The files the panel can open: the index's, the dump folder's
      -- sources, and the one loaded.
      sources = maybe [] (pure . cfPath) src ++ [f | a <- arts, Just f <- [maSource a]]
      local = mkSourceIndex [] sources
      fileOf sp = rsPath <$> (resolveSource local Nothing (ssFile sp) <|> resolveSource ix Nothing (ssFile sp))
      moduleFile m = rsPath <$> resolveModule ix m
      here m = T.null m || m == modName
      entry m k = if here m then M.lookup k byKey else Nothing
      artIndex p = listToMaybe [i | (i, a) <- zip [0 ..] arts, p a]
      loadedFile = normalise . cfPath <$> src
      entryAt l = listToMaybe [e | e <- es, b <- enSrc e, bdFirst b <= l, bdLast b >= l]
      binding m k = "binding " <> (if T.null m then k else m <> "." <> k)
      -- Open a file: the loaded one, a module of the dump folder, or else
      -- on its own.
      openFile f = unless (loadedFile == Just (normalise f)) $
        case artIndex ((== Just (normalise f)) . fmap normalise . maSource) of
          Just i -> writeVar (csModule st) i
          Nothing -> openSourceAlone st f
      bindingSpan e = case (src, listToMaybe (enSrc e)) of
        (Just cf, Just b) -> Just (SrcSpan (T.pack (cfPath cf)) (bdFirst b + 1) 1 (bdLast b + 1) 1)
        _ -> Nothing
      stgLine r = case clSites loaded of
        Just sites -> listToMaybe [l | a <- anns, anRef a == Just r, Just t <- [anTable a], Just l <- [siteOfTable sites t (anSpan a)]]
        Nothing -> Nothing
      spanOfRef r = case r of
        RSource sp -> Just sp
        RBinding m k | Just e <- entry m k -> bindingSpan e
        _ -> spanOf r
      -- The editor: a resolved file, or an absolute path as it is.
      editAction label sp =
        let file = case fileOf sp of
              Just f -> Just f
              Nothing -> if isAbsolute (T.unpack (ssFile sp)) then Just (T.unpack (ssFile sp)) else Nothing
         in Action label $ case file of
              _ | T.null editor -> Left "set an editor command in Settings"
              Nothing -> Left ("can't find " <> shortFileName (ssFile sp) <> "; set the project root in Settings")
              Just f -> Right (openInEditor st editor f (ssLine sp) (ssCol sp))
      badgeNotes =
        [ Annotation (Just sp) Nothing (Just (modName, enKey e)) LaneNote 0 ShadeInk txt (Just (RBinding modName (enKey e)))
        | e <- es
        , let txt = badgeText e (isJust core)
        , not (T.null txt)
        , Just sp <- [bindingSpan e]
        ]
  pure
    (lens PCore)
      { lensShows = \case
          RBinding m k -> isJust (entry m k) || isJust (artIndex ((== m) . maModule)) || isJust (moduleFile m)
          RModule m -> m == modName || isJust (artIndex ((== m) . maModule)) || isJust (moduleFile m)
          RSource sp -> isJust (fileOf sp)
          r@(RBand _) -> isJust (stgLine r)
          _ -> False
      , lensReveal = \r -> case r of
          RBinding m _ -> do
            unless (here m) $ case artIndex ((== m) . maModule) of
              Just i -> writeVar (csModule st) i
              Nothing -> for_ (moduleFile m) openFile
            writeVar (csPending st) (Just r)
          RModule m -> unless (m == modName) $ case artIndex ((== m) . maModule) of
            Just i -> writeVar (csModule st) i
            Nothing -> for_ (moduleFile m) openFile
          RSource sp -> for_ (fileOf sp) $ \f -> do
            openFile f
            writeVar (csPending st) (Just r)
          RBand _ -> writeVar (csPending st) (Just r)
          _ -> pure ()
      , lensTitle = \case
          RBinding m k | isJust (entry m k) -> Just (binding modName k)
          _ -> Nothing
      , lensRelated = \case
          RBinding m k -> case entry m k of
            Just e -> [("Go to source", RSource sp) | Just sp <- [bindingSpan e]]
            _ -> []
          RSource sp | fmap Just (fileOf sp) == Just loadedFile -> [("Go to binding " <> enKey e, RBinding modName (enKey e)) | Just e <- [entryAt (ssLine sp - 1)]]
          _ -> []
      , lensActions = \r -> case spanOfRef r of
          Just sp -> [editAction (case r of RSource _ -> "Open in editor"; _ -> "Open its source in editor") sp]
          Nothing -> []
      , lensPeek = \case
          RBinding m k -> (\e -> (entryPeek (isJust core) e) {peekExact = Just "by name: module and binding name"}) <$> entry m k
          RSource sp | fmap Just (fileOf sp) == Just loadedFile -> (\e -> (entryPeek (isJust core) e) {peekExact = Just "exact: the line, once its path resolves"}) <$> entryAt (ssLine sp - 1)
          r@(RBand _) | Just l <- stgLine r -> Just (peekOf [("STG site", "line " <> tshow (l + 1) <> " of " <> maybe "the STG dump" (T.pack . takeFileName . cfPath) stg)]) {peekExact = Just "exact: info-table name to STG binder"}
          _ -> Nothing
      , lensHits =
          [Hit "binding" (enKey e) modName (HitRef (RBinding modName (enKey e))) | e <- es, not (isTypeableBinding (enKey e)), not (T.null (enKey e))]
            ++ [Hit "module" (maModule a) (maybe "" (T.pack . takeFileName) (maSource a)) (HitRef (RModule (maModule a))) | a <- arts]
            ++ [ Hit "file" (T.pack (takeFileName f)) (T.pack f) (HitRef (RSource (SrcSpan (T.pack f) 1 1 1 1)))
               | f <- take 2000 (S.toList (siFiles ix))
               ]
      , lensAnnotations = badgeNotes
      , lensSpan = \case
          RBinding m k -> entry m k >>= bindingSpan
          _ -> Nothing
      , lensFacts = \case
          RBinding m k | Just e <- entry m k ->
            FactBadges (hasWorker e) (sum (map (length . nBoxing . bdNotes) (enCore e))) (sum (map (length . nAllocs . bdNotes) (enStg e)))
              -- The heap its lets hold: the bands whose info tables name
              -- it, biggest first.
              : take 1
                [ FactSiteHeap r v
                | (r, v) <- sortOn (Down . snd) (M.toList (M.fromListWith (+) [(r, anValue a) | a <- anns, anLane a == LaneHeap, anName a `elem` [Just (modName, k), Just (m, k)], Just r <- [anRef a]]))
                , v > 0
                ]
          _ -> []
      }

-- | A binding's badges and what the optimiser recorded about it.
entryPeek :: Bool -> Entry -> Peek
entryPeek haveCore e =
  Peek
    { peekFacts =
        ("binding", enKey e)
          : concat
            [ [("arity", tshow a) | Just a <- [nArity n]] ++ [("Str", T.take 40 s) | Just s <- [nStrictness n]]
            | b <- take 3 (enCore e)
            , let n = bdNotes b
            ]
          ++ [("source lines", tshow (bdFirst b + 1) <> "–" <> tshow (bdLast b + 1)) | Just b <- [listToMaybe (enSrc e)]]
    , peekNotes = []
    , peekExact = Nothing
    , peekExtra = Just (rowWith (tight . gap 6 . wrap . fillW) (entryBadges e haveCore))
    }
