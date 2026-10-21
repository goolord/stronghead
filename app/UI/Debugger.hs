-- | A visual front end for GHCi's debugger: start a session, click the
-- gutter to set breakpoints, step through evaluation, and inspect local
-- bindings, telling evaluated values from unevaluated thunks and forcing
-- them on demand.
--
-- Breakpoints can be set from anywhere: any line, binding, cost centre,
-- band or closure another panel puts in the source offers "Break here",
-- starting a session on its module when none is running. The source pane
-- carries the other panels' annotations, the stop links to its binding in
-- Core & STG, and @:history@ steps are links to their spans.
module UI.Debugger
  ( DebugState (..)
  , startSession
  , stopSession
  , newDebugState
  , debugLoad
  , debugView
  , debugPanes
  , debugLens
  , valueTree
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Extra (concatMapM)
import Data.Char (isDigit)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (for_)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as S
import Data.Word (Word64)
import System.Directory (makeAbsolute)
import System.FilePath (isAbsolute, makeRelative, normalise, takeDirectory, takeFileName, (</>))

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import NanoUI

import Stronghead.Dump (CodeFile (..), CodeKind (..), lineTokens, loadCodeFile, nameKey)
import Stronghead.Format (tshow)
import Stronghead.Ghci
import Stronghead.Join (Annotation (..), Lane (..), Mark (..), Shade (..))
import Stronghead.Ref
import Stronghead.Source (Resolved (..), SourceIndex (..), SrcSpan (..), findProjectRoot, mkSourceIndex, renderSrcSpan, resolveModule, resolveSource, shortFileName)
import UI.Code
import UI.Common
import UI.Link
import UI.Workspace

data DebugState = DebugState
  { dsCommand :: !(Var Text)
  , dsDir :: !(Var Text)
  , dsFile :: !(Var Text)
  , dsExpr :: !(Var Text)
  , dsSession :: !(IORef (Maybe Ghci))
  , dsRunning :: !(Var Bool)
  , dsBusy :: !(Var Bool)
  , dsConsole :: !(Var [Text])
    -- ^ Newest last.
  , dsInput :: !(Var Text)
  , dsStop :: !(Var (Maybe StopInfo))
  , dsBindings :: !(Var [GhciBinding])
  , dsValues :: !(Var (M.Map Text Value))
    -- ^ Each binding's value as @:print@ shows it, its thunks named.
  , dsToggled :: !(Var (S.Set Text))
    -- ^ Value-tree nodes opened or closed by hand, by path.
  , dsBreaks :: !(Var (M.Map (FilePath, Int) Int))
    -- ^ Breakpoint numbers by (file, 1-based line).
  , dsSourcePath :: !(Var (Maybe FilePath))
  , dsSource :: !(Task FilePath (Either String CodeFile))
  , dsCode :: !CodeViewState
  , dsSeenLines :: !(Var Int)
  , dsPanes :: !Workbench
  , dsCandidatesCache :: !(IORef (Maybe (FilePath, Int, [Int], IS.IntSet)))
  , dsNotesCache :: !(IORef (Maybe ((FilePath, Int, [FilePath], [Annotation]), FileNotes)))
  }

newDebugState :: IO DebugState
newDebugState = do
  dsCommand <- newVar "ghci"
  dsDir <- newVar ""
  dsFile <- newVar ""
  dsExpr <- newVar "main"
  dsSession <- newIORef Nothing
  dsRunning <- newVar False
  dsBusy <- newVar False
  dsConsole <- newVar []
  dsInput <- newVar ""
  dsStop <- newVar Nothing
  dsBindings <- newVar []
  dsValues <- newVar M.empty
  dsToggled <- newVar S.empty
  dsBreaks <- newVar M.empty
  dsSourcePath <- newVar Nothing
  dsSource <- newTask
  dsCode <- newCodeViewState
  dsSeenLines <- newVar 0
  dsPanes <- newWorkbench "debugger" (panelPreset PDebugger)
  dsCandidatesCache <- newIORef Nothing
  dsNotesCache <- newIORef Nothing
  pure DebugState {..}

cachedCandidates :: DebugState -> Maybe CodeFile -> IO ([Int], IS.IntSet)
cachedCandidates _ Nothing = pure ([], IS.empty)
cachedCandidates st (Just cf) = do
  let p = cfPath cf
      n = V.length (cfLines cf)
  cached <- readIORef (dsCandidatesCache st)
  case cached of
    Just (p', n', ls, set) | p' == p, n' == n -> pure (ls, set)
    _ -> do
      let ls = candidateLines (V.toList (cfLines cf))
          set = IS.fromList ls
      writeIORef (dsCandidatesCache st) (Just (p, n, ls, set))
      pure (ls, set)

debugLoad :: DebugState -> NanoUI (Maybe (Either String CodeFile))
debugLoad st = do
  p <- readVar (dsSourcePath st)
  r <- traverse (\path -> useTask (dsSource st) path (loadCodeFile KindSource path)) p
  pure (fromMaybe Nothing r)

-- ---------------------------------------------------------------------------
-- Talking to GHCi (on background threads)

appendConsole :: DebugState -> [Text] -> IO ()
appendConsole st ls = modifyVarIO (dsConsole st) (\old -> let new = old ++ ls in drop (length new - 2000) new)

resolve :: Text -> Text -> FilePath
resolve dir f
  | isAbsolute (T.unpack f) || T.null dir = T.unpack f
  | otherwise = T.unpack dir </> T.unpack f

-- | Commands that resume evaluation: after them the stop location and the
-- bindings are refreshed.
resumes :: Text -> Bool
resumes c = any (`T.isPrefixOf` T.strip c) [":step", ":steplocal", ":stepmodule", ":stepout", ":continue", ":trace", ":back", ":forward", ":abandon", ":main"] || not (":" `T.isPrefixOf` T.strip c)

runCommands :: DebugState -> IO () -> [Text] -> IO ()
runCommands st wake cmds = void . forkIO $ do
  writeVarIO (dsBusy st) True >> wake
  readIORef (dsSession st) >>= \case
    Nothing -> appendConsole st ["<no GHCi session: press Start>"]
    Just g -> runIn st g cmds
  writeVarIO (dsBusy st) False >> wake

-- | Run commands in a session, keeping the stop, the bindings, the
-- breakpoints and the loaded file up to date.
runIn :: DebugState -> Ghci -> [Text] -> IO ()
runIn st g cmds = do
  forM_ cmds $ \c -> do
    out <- ghciCommand g c
    appendConsole st (("λ> " <> c) : T.lines out)
    dir <- readVarIO (dsDir st)
    for_ (parseBreakpointSet out) $ \(n, sp) -> for_ sp $ \s ->
      modifyVarIO (dsBreaks st) (M.insert (resolve dir (ssFile s), ssLine s) n)
    for_ (T.stripPrefix ":load " (T.strip c)) $ \f -> do
      writeVarIO (dsSourcePath st) (Just (resolve dir (T.strip f)))
      writeVarIO (dsBreaks st) M.empty
    when (resumes c) $ do
      let stop = parseStop out
      writeVarIO (dsStop st) stop
      for_ (stop >>= stSpan) $ \s -> writeVarIO (dsSourcePath st) (Just (resolve dir (ssFile s)))
      if isJust stop
        then refreshLocals st g
        else writeVarIO (dsBindings st) [] >> writeVarIO (dsValues st) M.empty
  alive <- ghciAlive g
  writeVarIO (dsRunning st) alive

-- | Read the local bindings, and @:print@ each, which names its thunks
-- (@_t1@, @_t2@) so they can be forced. GHCi's own @_t@ bindings are left
-- out: they show inside the values.
refreshLocals :: DebugState -> Ghci -> IO ()
refreshLocals st g = do
  b <- ghciCommand g ":show bindings"
  let bs = [x | x <- parseBindings b, not (isThunkBinding (gbName x))]
  writeVarIO (dsBindings st) bs
  vals <-
    concatMapM
      ( \x -> do
          out <- ghciCommand g (":print " <> gbName x)
          pure [(gbName x, v) | (n, v) <- parsePrint out, n == gbName x]
      )
      (take 24 bs)
  writeVarIO (dsValues st) (M.fromList vals)

isThunkBinding :: Text -> Bool
isThunkBinding n = case T.stripPrefix "_t" n of
  Just d -> not (T.null d) && T.all isDigit d
  Nothing -> False

-- | Force a binding or a named thunk in place, to weak head normal form
-- (@seq@) or all the way (@:force@), then print the values again. GHCi
-- stays stopped where it was, unless forcing runs into a breakpoint.
forceValue :: DebugState -> IO () -> Text -> Bool -> IO ()
forceValue st wake name deep = void . forkIO $ do
  writeVarIO (dsBusy st) True >> wake
  readIORef (dsSession st) >>= \case
    Nothing -> appendConsole st ["<no GHCi session: press Start>"]
    Just g -> do
      let cmd = if deep then ":force " <> name else name <> " `seq` ()"
      out <- ghciCommand g cmd
      appendConsole st (("λ> " <> cmd) : T.lines out)
      for_ (parseStop out) (writeVarIO (dsStop st) . Just)
      refreshLocals st g
  writeVarIO (dsBusy st) False >> wake

startSession :: DebugState -> IO () -> IO ()
startSession st wake = startSessionThen st wake []

-- | Start a session with the command and directory set, load the file set,
-- then run @followUps@.
startSessionThen :: DebugState -> IO () -> [Text] -> IO ()
startSessionThen st wake followUps = void . forkIO $ do
  writeVarIO (dsBusy st) True >> wake
  cmd <- readVarIO (dsCommand st)
  dir <- readVarIO (dsDir st)
  file <- readVarIO (dsFile st)
  appendConsole st ["$ " <> cmd]
  r <- startGhci (T.unpack dir) (T.unpack cmd)
  case r of
    Left e -> appendConsole st ["failed to start: " <> e]
    Right (g, out) -> do
      writeIORef (dsSession st) (Just g)
      appendConsole st (T.lines out)
      writeVarIO (dsRunning st) True
      writeVarIO (dsBreaks st) M.empty
      runIn st g ([":load " <> T.strip file | not (T.null (T.strip file))] ++ followUps)
  writeVarIO (dsBusy st) False >> wake

-- | Start a session on a file, then run @followUps@. A @cabal@ or @stack@
-- command runs in the file's project; plain GHCi in the file's directory.
startOn :: DebugState -> IO () -> FilePath -> [Text] -> IO ()
startOn st wake file0 followUps = void . forkIO $ do
  file <- makeAbsolute file0
  cmd <- T.strip <$> readVarIO (dsCommand st)
  dir <-
    if any (`T.isPrefixOf` cmd) ["cabal", "stack"]
      then fromMaybe (takeDirectory file) <$> findProjectRoot file
      else pure (takeDirectory file)
  writeVarIO (dsDir st) (T.pack dir)
  writeVarIO (dsFile st) (T.pack (makeRelative dir file))
  startSessionThen st wake followUps

stopSession :: DebugState -> IO () -> IO ()
stopSession st wake = do
  readIORef (dsSession st) >>= mapM_ stopGhci
  writeIORef (dsSession st) Nothing
  writeVarIO (dsRunning st) False
  writeVarIO (dsStop st) Nothing
  writeVarIO (dsBindings st) []
  appendConsole st ["<session stopped>"]
  wake

-- ---------------------------------------------------------------------------
-- View

debugView :: Env -> Bench -> DebugState -> SourceIndex -> Maybe (Either String CodeFile) -> NanoUI ()
debugView env bench st ix source = columnWith (tight . gap 0 . grow) $ do
  wake <- askWake
  running <- readVar (dsRunning st)
  busy <- readVar (dsBusy st)
  stop <- readVar (dsStop st)
  let send cmds = liftIO (runCommands st wake cmds)
      input placeholder f v = textInputConfigured defaultTextInputConfig {ticPlaceholder = placeholder, ticLayout = (alignMid . f) defaultLayout} v
  toolStrip $ disabledWhen running $ do
    toolLabel "Run"
    input "ghci  or  cabal repl" (fixedW 160) (dsCommand st)
    toolLabel "in"
    input "working directory" (minW 240 . fillW) (dsDir st)
    toolLabel "and load"
    input "src/Main.hs" (fixedW 200) (dsFile st)
  toolStrip $ do
    scope $
      if running
        then whenM (toolButton "Stop session") (liftIO (stopSession st wake))
        else disabledWhen busy $ whenM (styled primary (toolButton "Start session")) (liftIO (startSession st wake))
    scope $ when busy (rowWith (alignMid . tight) spinner)
    disabledWhen (not running || busy) $ toolGroup $ do
      input "expression, e.g. main" (fixedW 170) (dsExpr st)
      expr <- T.strip <$> readVar (dsExpr st)
      whenM (toolButton "Step into") (send [":step " <> expr])
      whenM (toolButton "Trace") (send [":trace " <> expr])
      whenM (toolButton "Run") (send [expr])
    -- Continue and Abandon take their colour only when they can act.
    let stepping = running && not busy && isJust stop
        whenStepping f = if stepping then f else id
    disabledWhen (not stepping) $ do
      toolGroup $ do
        whenM (toolButton "Step") (send [":step"])
        whenM (toolButton "Step over") (send [":steplocal"])
        whenM (toolButton "Step out") (send [":stepout"])
        whenM (toolButton "Step module") (send [":stepmodule"])
        whenM (styled (whenStepping success) (toolButton "Continue")) (send [":continue"])
      toolGroup $ do
        whenM (toolButton "Back") (send [":back"])
        whenM (toolButton "Forward") (send [":forward"])
        whenM (toolButton "History") (send [":history"])
      flex
      whenM (styled (whenStepping destructive) (toolButton "Abandon")) (send [":abandon"])
  bnDraw bench (dsPanes st) (debugPanes env st ix source)

-- | The Debugger's panes, for whichever workbench holds them: the source
-- with the stop and breakpoints, the local bindings, the breakpoints and
-- the GHCi console.
debugPanes :: Env -> DebugState -> SourceIndex -> Maybe (Either String CodeFile) -> [Word64] -> NanoUI PaneDraw
debugPanes env st ix source _ = do
  wake <- askWake
  stop <- readVar (dsStop st)
  let send cmds = liftIO (runCommands st wake cmds)
  pure $ \pid ctx ->
    if
      | pid == paneDbgSource -> Just (sourcePane env st ix ctx source stop send)
      | pid == paneDbgLocals -> Just (pane ctx "Local bindings" (pure ()) (bindingsPane st stop))
      | pid == paneDbgBreaks -> Just (breaksPane env st ctx send)
      | pid == paneDbgConsole -> Just (consolePane env st ctx send)
      | otherwise -> Nothing

-- | A stop's binding: @Main.go@ is @go@ in @Main@.
stopBinding :: Text -> Ref
stopBinding name =
  let (m, k) = T.breakOnEnd "." name
   in if T.null m || T.null k then RBinding "" (nameKey name) else RBinding (T.dropEnd 1 m) (nameKey k)

sourcePane :: Env -> DebugState -> SourceIndex -> PaneCtx -> Maybe (Either String CodeFile) -> Maybe StopInfo -> ([Text] -> NanoUI ()) -> NanoUI ()
sourcePane env st ix ctx source stop send = do
  dir <- readVar (dsDir st)
  path <- readVar (dsSourcePath st)
  breaks <- readVar (dsBreaks st)
  mm <- monoMetrics 0.9
  th <- uiTheme
  let pal = palette th
      cf = source >>= eitherToMaybe
      here = stop >>= stSpan
      fileBreaks = IM.fromList [(l, n) | ((f, l), n) <- M.toList breaks, Just f == path]
      -- What the other panels say about this file; the debugger's own
      -- marks are drawn from its state.
      others = concat [lensAnnotations l | l <- envLenses env, lensPanel l /= PDebugger]
  (_, candidates) <- liftIO (cachedCandidates st cf)
  notes <- case path of
    Nothing -> pure noNotes
    Just p -> liftIO $ do
      let key = (p, S.size (siFiles ix), siRoots ix, others)
      cached <- readIORef (dsNotesCache st)
      case cached of
        Just (k, fn) | k == key -> pure fn
        _ -> do
          let fn = notesForFile ix p others
          writeIORef (dsNotesCache st) (Just (key, fn))
          pure fn
  let header = do
        paneMeta (maybe "no module loaded" (T.pack . takeFileName) path)
        scope $ unless (T.null (laneLegend notes)) $ paneMeta (laneLegend notes)
        flex
        scope $ for_ stop $ \s -> do
          labelWith (alignMid . fontSizeScale 0.85 . fontColor (pGC pal)) "Stopped in"
          linkText env (stopBinding (stName s)) (alignMid . fontSizeScale 0.85 . fontColor (pGC pal)) (stName s)
          for_ (stSpan s) $ \sp ->
            linkText env (RSource sp {ssFile = T.pack (resolve dir (ssFile sp))}) (alignMid . fontMono . fontMuted . fontSizeScale 0.85) (shortFileName (ssFile sp) <> ":" <> tshow (ssLine sp) <> ":" <> tshow (ssCol sp))
  ev <- paneWith tight ctx "Source" header $ do
    scope $ case source of
      Just (Left e) -> columnWith (padXY 12 6) $ labelWith (fontTone Danger) (T.pack e)
      _ -> pure ()
    let lineRef ev' = case (ceHoverKey ev', cf, path) of
          (Just k, Just c, _) | M.member k (cfByKey c) -> Just (RBinding (cfModule c) k)
          (_, _, Just p) | Just l <- ceHoverLine ev' -> Just (RSource (SrcSpan (T.pack p) (l + 1) 1 (l + 1) 1))
          _ -> Nothing
    annotatedCode
      env
      (dsCode st)
      mm
      ( withNotes
          notes
          defaultCodeConfig
            { ccLineCount = maybe 0 (V.length . cfLines) cf
            , ccTokens = \l -> maybe [] (`lineTokens` l) cf
            , ccSpans =
                [ (ssLine s - 1, ssCol s - 1, ssEndLine s - 1, ssEndCol s, alpha 0.35 (pGC pal))
                | Just s <- [here]
                ]
            , ccGutter = \l ->
                if
                  | IM.member (l + 1) fileBreaks -> Just MarkBreakpoint
                  | fmap ssLine here == Just (l + 1) -> Just MarkCurrent
                  | IS.member (l + 1) candidates -> Just MarkCandidate
                  | otherwise -> Nothing
            , ccEmpty = "Start a session and load a module to see its source."
            }
      )
      lineRef
  -- Follow the stop location.
  let stopLine = maybe 0 ssLine here
  seen <- readVar (dsSeenLines st)
  when (stopLine > 0 && stopLine /= seen) $ do
    writeVar (dsSeenLines st) stopLine
    scrollToLine (dsCode st) (stopLine - 1)
  for_ (ceGutterClick ev) $ \l0 -> for_ cf $ \c -> do
    let line = l0 + 1
    case IM.lookup line fileBreaks of
      Just n -> deleteBreak st send (path, line) n
      Nothing -> send [":break " <> cfModule c <> " " <> tshow line]

deleteBreak :: DebugState -> ([Text] -> NanoUI ()) -> (Maybe FilePath, Int) -> Int -> NanoUI ()
deleteBreak st send (path, line) n = do
  send [":delete " <> tshow n]
  for_ path $ \p -> modifyVar (dsBreaks st) (M.delete (p, line))

-- | Every breakpoint, across files, as links.
breaksPane :: Env -> DebugState -> PaneCtx -> ([Text] -> NanoUI ()) -> NanoUI ()
breaksPane env st ctx send = do
  breaks <- readVar (dsBreaks st)
  running <- readVar (dsRunning st)
  pane ctx "Breakpoints" (paneMeta (tshow (M.size breaks))) $ do
    scope $ when (M.null breaks) $ hint "No breakpoints. Click the gutter, or right-click any line, binding, cost centre or closure in another panel: Break here."
    paneScroll (grow . tight) $ columnWith (tight . fillW . gap 0) $
      forM_ (M.toList breaks) $ \((f, l), n) -> withKey (T.pack f <> ":" <> tshow l) $
        rowWith (tight . fillW . gap 4) $ do
          _ <- linkRow env (RBreakpoint f l) False fillW $ do
            cell (fixedW 26 . fontMuted . fontSizeScale 0.85) ("#" <> tshow n)
            cell (fillW . fontMono . fontSizeScale 0.9) (T.pack (takeFileName f) <> ":" <> tshow l)
          disabledWhen (not running) $ whenM (buttonWith (alignMid . padXY 6 0 . tight) "Delete") (deleteBreak st send (Just f, l) n)

-- | The local bindings at the stop, each value an expandable tree.
bindingsPane :: DebugState -> Maybe StopInfo -> NanoUI ()
bindingsPane st stop = do
  bindings <- readVar (dsBindings st)
  values <- readVar (dsValues st)
  toggled <- readVar (dsToggled st)
  busy <- readVar (dsBusy st)
  wake <- askWake
  let force name deep = liftIO (forceValue st wake name deep)
  scope $ case (stop, bindings) of
    (Nothing, _) -> hint "Not stopped at a breakpoint. Set one in the gutter, then use Step into or Run."
    (Just _, []) -> hint "No local bindings at this location."
    _ -> pure ()
  paneScroll (grow . tight) $ columnWith (tight . fillW . gap 4) $
    forM_ bindings $ \b -> withKey (gbName b) $ panelWith (padXY 8 5 . gap 2 . fillW) $ do
      let value = maybe (parseValue <$> gbValue b) Just (M.lookup (gbName b) values)
          name = gbName b
      rowWith (tight . gap 6 . fillW) $ do
        cell (fontMono . fontMedium . alignMid) name
        cell (fontMono . fontMuted . fillW . fontSizeScale 0.9 . alignMid) (":: " <> gbType b)
        scope $ case value of
          Nothing -> badge Warning "thunk"
          Just (VThunk _ _) -> badge Warning "thunk"
          Just v | valueHasThunks v -> badge Warning "partly evaluated"
          Just _ -> badge Success "evaluated"
        disabledWhen busy $ do
          whenM (buttonWith (alignMid . padXY 6 0 . tight) "WHNF") (force name False)
          whenM (buttonWith (alignMid . padXY 6 0 . tight) "Force all") (force name True)
      scope $ for_ value $ \v -> valueTree st toggled busy force name v
  scope $ unless (null bindings) $
    hint "Thunks are amber. Evaluate takes one to weak head normal form (seq), Force evaluates it all the way (:force); the values are printed again afterwards."

-- | A value as an expandable tree, styled like the closure graph: thunks
-- amber, each one GHCi named (@_t3@) with buttons that force it in place.
valueTree :: DebugState -> S.Set Text -> Bool -> (Text -> Bool -> NanoUI ()) -> Text -> Value -> NanoUI ()
valueTree st toggled busy force root v0 = columnWith (tight . fillW . gap 0) $
  forM_ (take 400 (flattenValue toggled root v0)) $ \(path, depth, label', v, open, expandable) -> withKey path $
    rowWith (tight . fillW . gap 4) $ do
      clicked <- selectableRow False fillW $ do
        spacer (Fixed (fromIntegral (min 30 depth) * 14)) Fit
        cell (fixedW 14 . fontMuted) (if expandable then (if open then "▾" else "▸") else " ")
        scope $ unless (T.null label') $ cell (fontMono . fontMuted . fontSizeScale 0.9) label'
        cell (fillW . fontMono . fontSizeScale 0.9 . (case v of VThunk _ _ -> fontTone Warning; _ -> id)) (T.take 160 (nodeText open v))
        scope $ case v of
          VThunk _ (Just ty) -> cell (fontMono . fontMuted . fontSizeScale 0.85) (":: " <> T.take 60 ty)
          _ -> pure ()
      when (clicked && expandable) $ modifyVar (dsToggled st) (\s -> if S.member path s then S.delete path s else S.insert path s)
      scope $ case v of
        VThunk n _ | isThunkBinding n -> disabledWhen busy $ do
          whenM (buttonWith (alignMid . padXY 6 0 . tight) "Evaluate") (force n False)
          whenM (buttonWith (alignMid . padXY 6 0 . tight) "Force") (force n True)
        _ -> pure ()

-- | Rows of a value tree: path, depth, field label, value, whether it is
-- open and whether it can be. The first two levels start open.
flattenValue :: S.Set Text -> Text -> Value -> [(Text, Int, Text, Value, Bool, Bool)]
flattenValue toggled root = go root 0 ""
  where
    go path d label' v =
      let kids = children v
          expandable = not (null kids)
          open = expandable && (d < 2) /= S.member path toggled
       in (path, d, label', v, open, expandable)
            : (if open then concat [go (path <> "/" <> tshow i) (d + 1) l k | (i, (l, k)) <- zip [0 :: Int ..] kids] else [])
    children = \case
      VCon _ fs -> [("", f) | f <- fs]
      VRecord _ fs -> [(f <> " =", x) | (f, x) <- fs]
      VList xs t -> [("", x) | x <- xs] ++ [("rest", r) | Just r <- [t]]
      VTuple xs -> [("", x) | x <- xs]
      _ -> []

-- | What a tree node says: an open node names its shape, a closed one shows
-- the whole value.
nodeText :: Bool -> Value -> Text
nodeText open v = case v of
  VThunk n _ -> if n == "_" then "unevaluated" else n
  VCon c _ | open -> c
  VRecord c _ | open -> c
  VList xs t | open -> "list of " <> tshow (length xs) <> maybe "" (const ", then unevaluated") t
  VTuple _ | open -> "tuple"
  _ -> renderValue v

consolePane :: Env -> DebugState -> PaneCtx -> ([Text] -> NanoUI ()) -> NanoUI ()
consolePane env st ctx send = do
  th <- uiTheme
  dir <- readVar (dsDir st)
  let i = ink th
      clear = do
        flex
        whenM (buttonWith (alignMid . padXY 8 2 . tight) "Clear") (writeVar (dsConsole st) [])
      at sp = RSource sp {ssFile = T.pack (resolve dir (ssFile sp))}
      mono = fontMono . fontSizeScale 0.85
  paneWith tight ctx "GHCi console" clear $ do
    ls <- readVar (dsConsole st)
    sid <- currentId
    (_, ()) <- paneScrollArea (grow . tight) $ columnWith (padXY 12 8 . tight . fillW . gap 0) $
      forM_ (zip [0 :: Int ..] (drop (length ls - 400) ls)) $ \(n, l) -> withKey n $
        -- History steps and stops are links to where they are.
        case (historyLine l, parseStop l) of
          (Just (k, name, sp), _) -> rowWith (fillW . tight . gap 8) $ do
            labelWith (mono . fontMuted) (tshow k)
            labelWith mono name
            linkText env (at sp) (mono . fontMuted) (renderSrcSpan sp)
          (_, Just s) | Just sp <- stSpan s -> rowWith (fillW . tight . gap 8) $ do
            labelWith mono "Stopped in"
            linkText env (stopBinding (stName s)) mono (stName s)
            linkText env (at sp) (mono . fontMuted) (renderSrcSpan sp)
          _ -> labelWith (fillW . mono . (if "λ>" `T.isPrefixOf` l then fontTone Accent else id)) (if T.null l then " " else l)
    -- Keep the newest output in view.
    seen <- useVar (0 :: Int)
    n <- readVar seen
    when (n /= length ls) $ do
      writeVar seen (length ls)
      scrollToUi sid (V2 0 (1 / 0)) ScrollInstant
    running <- readVar (dsRunning st)
    styled (panelStyle (background (inkSunken i) . borderTop 1 (inkRule i) . cornerRadius 0)) $
      panelWith (asRow . fillW . padXY 12 6 . gap 8) $ themed th $ do
        labelWith (alignMid . fontMono . fontTone Accent) "λ>"
        resp <- textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Any GHCi command, e.g. :show breaks, :history, :sprint xs", ticLayout = (alignMid . fillW) defaultLayout} (dsInput st)
        go <- disabledWhen (not running) (toolButton "Send")
        when (respSubmitted resp || go) $ do
          cmd <- readVar (dsInput st)
          unless (T.null (T.strip cmd)) $ do
            send [cmd]
            writeVar (dsInput st) ""

-- ---------------------------------------------------------------------------
-- Lens

-- | What the debugger can show of the entities other panels name: lines of
-- the module GHCi has loaded, and its breakpoints. It can break on any line
-- the source index finds, and on any entity another panel puts in the
-- source (@spanOf@): a cost centre, a band, a closure, a surviving thunk.
-- It breaks on bindings by name. Without a session it offers to start one
-- on the right module, with the GHCi command from Settings.
debugLens :: DebugState -> Maybe (Either String CodeFile) -> SourceIndex -> (Ref -> Maybe SrcSpan) -> NanoUI Lens
debugLens st source ix spanOf = do
  path <- readVar (dsSourcePath st)
  running <- readVar (dsRunning st)
  busy <- readVar (dsBusy st)
  stop <- readVar (dsStop st)
  breaks <- readVar (dsBreaks st)
  dir <- readVar (dsDir st)
  wake <- askWake
  let cf = source >>= eitherToMaybe
  (candLines, _) <- if running then liftIO (cachedCandidates st cf) else pure ([], IS.empty)
  let inLoaded sp = maybe False (\p -> isJust (resolveSource (mkSourceIndex [] [p]) Nothing (ssFile sp))) path
      send cmds = liftIO (runCommands st wake cmds)
      startThen file cmds = liftIO (startOn st wake file cmds)
      fileOf sp = case resolveSource ix Nothing (ssFile sp) of
        Just r -> Just (rsPath r)
        Nothing -> if isAbsolute (T.unpack (ssFile sp)) then Just (normalise (T.unpack (ssFile sp))) else Nothing
      name f = T.pack (takeFileName f)
      -- Break on a line: in the loaded module, after loading another, or in
      -- a new session.
      breakAt verb sp
        | busy = Action verb (Left "GHCi is busy")
        | running, inLoaded sp, Just c <- cf = Action (verb <> " (GHCi)") (Right (send [":break " <> cfModule c <> " " <> tshow (ssLine sp)]))
        | otherwise = case fileOf sp of
            Nothing -> Action verb (Left ("can't find " <> shortFileName (ssFile sp) <> "; set the project root in Settings"))
            Just f
              | running -> Action ("Load " <> name f <> " in GHCi and " <> T.toLower verb) (Right (send [":load " <> T.pack (relativeTo f), ":break " <> tshow (ssLine sp)]))
              | otherwise -> Action ("Start GHCi on " <> name f <> " and " <> T.toLower verb) (Right (startThen f [":break " <> tshow (ssLine sp)]))
      relativeTo f = if T.null dir then f else makeRelative (T.unpack dir) f
      qualified m k = if T.null m then k else m <> "." <> k
      breakOn m k
        | busy = Action label (Left "GHCi is busy")
        | running = Action (label <> " (GHCi)") (Right (send [":break " <> qualified m k]))
        | otherwise = case resolveModule ix m of
            Just r -> Action ("Start GHCi on " <> name (rsPath r) <> " and break on entry to " <> k) (Right (startThen (rsPath r) [":break " <> qualified m k]))
            Nothing -> Action label (Left (if T.null m then "start a GHCi session first" else "can't find module " <> m <> "; set the project root in Settings"))
        where
          label = "Break on entry to " <> k
      stopHere sp = [s | s <- maybe [] pure (stop >>= stSpan), inLoaded sp, ssLine s == ssLine sp]
      breakHere sp = [n | Just p <- [path], Just n <- [M.lookup (p, ssLine sp) breaks], inLoaded sp]
      stopped = isJust stop
      command label cmd = Hit "debugger" label "GHCi" (HitRun (send [cmd]))
      marks =
        [ Annotation (Just (SrcSpan (T.pack f) l 1 l 1)) Nothing Nothing (LaneMark BreakMark) 0 ShadeInk "" (Just (RBreakpoint f l))
        | (f, l) <- M.keys breaks
        ]
          ++ [ Annotation (Just s') Nothing Nothing (LaneMark StopMark) 0 ShadeInk ("stopped in " <> stName st') (Just (stopBinding (stName st')))
             | Just st' <- [stop]
             , Just s <- [stSpan st']
             , let s' = s {ssFile = T.pack (resolve dir (ssFile s))}
             ]
          ++ [ Annotation (Just (SrcSpan (T.pack p) l 1 l 1)) Nothing Nothing (LaneMark CandidateMark) 0 ShadeInk "" Nothing
             | running
             , Just p <- [path]
             , l <- candLines
             ]
  pure
    (lens PDebugger)
      { lensShows = \case
          RSource sp -> inLoaded sp
          RBreakpoint f l -> M.member (f, l) breaks
          _ -> False
      , lensReveal = \case
          RSource sp -> scrollToLine (dsCode st) (ssLine sp - 1)
          RBreakpoint f l -> do
            unless (path == Just f) $ writeVar (dsSourcePath st) (Just f)
            scrollToLine (dsCode st) (l - 1)
          _ -> pure ()
      , lensTitle = \case
          RBreakpoint f l -> (\n -> "Breakpoint " <> tshow n <> " at " <> T.pack (takeFileName f) <> ":" <> tshow l) <$> M.lookup (f, l) breaks
          _ -> Nothing
      , lensRelated = \case
          RBreakpoint f l -> [("Go to source", RSource (SrcSpan (T.pack f) l 1 l 1))]
          _ -> []
      , lensActions = \case
          RSource sp -> [breakAt "Break here" sp]
          RBinding m k -> [breakOn m k]
          RCostCentre m l -> maybe [] (pure . breakAt "Break here") (spanOf (RCostCentre m l)) ++ [breakOn m l | not (T.null m), not (T.any (`elem` ['\\', '.']) l)]
          r@(RThunkGroup _) -> maybe [] (pure . breakAt "Break where created") (spanOf r)
          r@(RClosure _ _) -> maybe [] (pure . breakAt "Break where created") (spanOf r)
          r@(RBand _) -> maybe [] (pure . breakAt "Break where allocated") (spanOf r)
          RBreakpoint f l
            | Just n <- M.lookup (f, l) breaks ->
                [Action "Delete breakpoint" (if running && not busy then Right (deleteBreak st send (Just f, l) n) else Left "GHCi is not running")]
          _ -> []
      , lensPeek = \case
          RSource sp
            | not (null (stopHere sp) && null (breakHere sp)) ->
                Just
                  (peekOf ([("stopped here", renderSrcSpan s {ssFile = T.pack (takeFileName (resolve dir (ssFile s)))}) | s <- stopHere sp] ++ [("breakpoint", tshow n) | n <- breakHere sp]))
                    { peekExact = Just "exact: file and line, as GHCi reports them"
                    }
          _ -> Nothing
      , lensHits =
          if not running
            then [Hit "debugger" "Start GHCi session" "Debugger" (HitRun (liftIO (startSession st wake))) | not busy]
            else
              [command "Step" ":step" | stopped]
                ++ [command "Step over" ":steplocal" | stopped]
                ++ [command "Step out" ":stepout" | stopped]
                ++ [command "Continue" ":continue" | stopped]
                ++ [command "Show breakpoints" ":show breaks"]
                ++ [command "History" ":history" | stopped]
      , lensAnnotations = marks
      , lensSpan = \case
          RBreakpoint f l -> Just (SrcSpan (T.pack f) l 1 l 1)
          _ -> Nothing
      }
