-- | A visual front end for GHCi's debugger: start a session, click the
-- gutter to set breakpoints, step through evaluation, and inspect local
-- bindings, telling evaluated values from unevaluated thunks and forcing
-- them on demand.
module UI.Debugger
  ( DebugState (..)
  , startSession
  , newDebugState
  , debugLoad
  , debugView
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import Data.Foldable (for_)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import NanoUI
import Stronghead.Dump (CodeFile (..), CodeKind (..), lineTokens, loadCodeFile)
import Stronghead.Format (tshow)
import Stronghead.Ghci
import System.FilePath (isAbsolute, takeFileName, (</>))
import UI.Code
import UI.Common

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
  , dsForced :: !(Var (M.Map Text Text))
  , dsBreaks :: !(Var (M.Map (FilePath, Int) Int))
    -- ^ Breakpoint numbers by (file, 1-based line).
  , dsSourcePath :: !(Var (Maybe FilePath))
  , dsSource :: !(Task FilePath (Either String CodeFile))
  , dsCode :: !CodeViewState
  , dsSeenLines :: !(Var Int)
  , dsPanes :: !Workbench
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
  dsForced <- newVar M.empty
  dsBreaks <- newVar M.empty
  dsSourcePath <- newVar Nothing
  dsSource <- newTask
  dsCode <- newCodeViewState
  dsSeenLines <- newVar 0
  dsPanes <- newVar (Just (Split 10 AxisH 0.7 (Split 11 AxisV 0.68 (Pane 1) (Pane 2)) (Pane 3)))
  pure DebugState {..}

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
    Just g -> do
      forM_ cmds $ \c -> do
        out <- ghciCommand g c
        appendConsole st (("λ> " <> c) : T.lines out)
        dir <- readVarIO (dsDir st)
        for_ (parseBreakpointSet out) $ \(n, sp) -> for_ sp $ \s ->
          modifyVarIO (dsBreaks st) (M.insert (resolve dir (ssFile s), ssLine s) n)
        when (resumes c) $ do
          let stop = parseStop out
          writeVarIO (dsStop st) stop
          writeVarIO (dsForced st) M.empty
          for_ (stop >>= stSpan) $ \s -> writeVarIO (dsSourcePath st) (Just (resolve dir (ssFile s)))
          b <- if isJust stop then ghciCommand g ":show bindings" else pure ""
          writeVarIO (dsBindings st) (parseBindings b)
      alive <- ghciAlive g
      writeVarIO (dsRunning st) alive
  writeVarIO (dsBusy st) False >> wake

startSession :: DebugState -> IO () -> IO ()
startSession st wake = void . forkIO $ do
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
      unless (T.null (T.strip file)) $ do
        loadOut <- ghciCommand g (":load " <> T.strip file)
        appendConsole st (("λ> :load " <> T.strip file) : T.lines loadOut)
        writeVarIO (dsSourcePath st) (Just (resolve dir (T.strip file)))
      writeVarIO (dsBreaks st) M.empty
  writeVarIO (dsBusy st) False >> wake

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

debugView :: DebugState -> Maybe (Either String CodeFile) -> NanoUI ()
debugView st source = columnWith (tight . gap 0 . grow) $ do
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
    disabledWhen (not running || busy || isNothing' stop) $ do
      toolGroup $ do
        whenM (toolButton "Step") (send [":step"])
        whenM (toolButton "Step over") (send [":steplocal"])
        whenM (toolButton "Step out") (send [":stepout"])
        whenM (toolButton "Step module") (send [":stepmodule"])
        whenM (styled success (toolButton "Continue")) (send [":continue"])
      toolGroup $ do
        whenM (toolButton "Back") (send [":back"])
        whenM (toolButton "Forward") (send [":forward"])
        whenM (toolButton "History") (send [":history"])
      flex
      whenM (styled destructive (toolButton "Abandon")) (send [":abandon"])
  workbench (dsPanes st) $ \pid ctx ->
    if
      | pid == 1 -> sourcePane st ctx source stop send
      | pid == 2 -> pane ctx "Local bindings" (pure ()) (bindingsPane st stop send)
      | otherwise -> consolePane st ctx send

isNothing' :: Maybe a -> Bool
isNothing' = maybe True (const False)

sourcePane :: DebugState -> PaneGridCtx -> Maybe (Either String CodeFile) -> Maybe StopInfo -> ([Text] -> NanoUI ()) -> NanoUI ()
sourcePane st ctx source stop send = do
  path <- readVar (dsSourcePath st)
  breaks <- readVar (dsBreaks st)
  mm <- monoMetrics 0.9
  th <- uiTheme
  let pal = palette th
      cf = case source of
        Just (Right c) -> Just c
        _ -> Nothing
      here = stop >>= stSpan
      fileBreaks = M.fromList [(l, n) | ((f, l), n) <- M.toList breaks, Just f == path]
      candidates = maybe mempty (\c -> M.fromList [(l, ()) | l <- candidateLines (V.toList (cfLines c))]) cf
  let header = do
        paneMeta (maybe "no module loaded" (T.pack . takeFileName) path)
        flex
        scope $ for_ stop $ \s -> do
          labelWith (alignMid . fontSizeScale 0.85 . fontColor (pGC pal)) ("Stopped in " <> stName s)
          for_ (stSpan s) $ \sp -> labelWith (alignMid . fontMono . fontMuted . fontSizeScale 0.85) (T.pack (takeFileName (T.unpack (ssFile sp))) <> ":" <> tshow (ssLine sp) <> ":" <> tshow (ssCol sp))
  ev <- paneWith tight ctx "Source" header $ do
    scope $ case source of
      Just (Left e) -> columnWith (padXY 12 6) $ labelWith (fontTone Danger) (T.pack e)
      _ -> pure ()
    codeView
      (dsCode st)
      mm
      defaultCodeConfig
        { ccLineCount = maybe 0 (V.length . cfLines) cf
        , ccTokens = \l -> maybe [] (`lineTokens` l) cf
        , ccSpans =
            [ (ssLine s - 1, ssCol s - 1, ssEndLine s - 1, ssEndCol s, alpha 0.35 (pGC pal))
            | Just s <- [here]
            ]
        , ccGutter = \l ->
            if
              | M.member (l + 1) fileBreaks -> Just MarkBreakpoint
              | fmap ssLine here == Just (l + 1) -> Just MarkCurrent
              | M.member (l + 1) candidates -> Just MarkCandidate
              | otherwise -> Nothing
        , ccEmpty = "Start a session and load a module to see its source."
        }

  -- Follow the stop location.
  let stopLine = maybe 0 ssLine here
  seen <- readVar (dsSeenLines st)
  when (stopLine > 0 && stopLine /= seen) $ do
    writeVar (dsSeenLines st) stopLine
    scrollToLine (dsCode st) (stopLine - 1)
  for_ (ceGutterClick ev) $ \l0 -> for_ cf $ \c -> do
    let line = l0 + 1
    case M.lookup line fileBreaks of
      Just n -> do
        send [":delete " <> tshow n]
        for_ path $ \p -> modifyVar (dsBreaks st) (M.delete (p, line))
      Nothing -> send [":break " <> cfModule c <> " " <> tshow line]

bindingsPane :: DebugState -> Maybe StopInfo -> ([Text] -> NanoUI ()) -> NanoUI ()
bindingsPane st stop send = do
  bindings <- readVar (dsBindings st)
  forced <- readVar (dsForced st)
  scope $ case (stop, bindings) of
    (Nothing, _) -> hint "Not stopped at a breakpoint. Set one in the gutter, then use Step into or Run."
    (Just _, []) -> hint "No local bindings at this location."
    _ -> pure ()
  paneScroll (grow . tight) $ columnWith (tight . fillW . gap 4) $
    forM_ bindings $ \b -> withKey (gbName b) $ panelWith (padXY 8 5 . gap 3 . fillW) $ do
      let thunk = valueIsThunk (gbValue b)
      rowWith (tight . gap 6 . fillW) $ do
        cell (fontMono . fontMedium) (gbName b)
        cell (fontMono . fontMuted . fillW . fontSizeScale 0.9) (":: " <> gbType b)
        scope $ if thunk then badge Muted "_thunk" else badge Success "evaluated"
      scope $ for_ (gbValue b) $ \v ->
        unless (v == "_") $ labelWith (fontMono . fillW . (if thunk then fontMuted else id)) (T.take 400 v)
      scope $ for_ (M.lookup (gbName b) forced) $ \v -> labelWith (fontMono . fontTone Success . fillW) (T.take 400 v)
      rowWith (tight . gap 6) $ do
        let name = gbName b
        whenM (buttonWith tight "Force WHNF") $ do
          send [name <> " `seq` ()", ":print " <> name]
          captureForce name (":print " <> name)
        whenM (buttonWith tight "Force NF") $ do
          send [":force " <> name]
          captureForce name (":force " <> name)
        whenM (buttonWith tight ":print") (send [":print " <> name])
  where
    -- The forced value is read back from the console once the command ran.
    captureForce name cmd = do
      wake <- askWake
      liftIO . void . forkIO $ do
        readIORef (dsSession st) >>= \case
          Nothing -> pure ()
          Just g -> do
            out <- ghciCommand g cmd
            modifyVarIO (dsForced st) (M.insert name (T.strip (last ("" : T.lines out))))
            b <- ghciCommand g ":show bindings"
            writeVarIO (dsBindings st) (parseBindings b)
            wake

consolePane :: DebugState -> PaneGridCtx -> ([Text] -> NanoUI ()) -> NanoUI ()
consolePane st ctx send = do
  th <- uiTheme
  let i = ink th
      clear = do
        flex
        whenM (buttonWith (alignMid . padXY 8 2 . tight) "Clear") (writeVar (dsConsole st) [])
  paneWith tight ctx "GHCi console" clear $ do
    ls <- readVar (dsConsole st)
    sid <- currentId
    (_, ()) <- paneScrollArea (grow . tight) $ columnWith (padXY 12 8 . tight . fillW . gap 0) $
      forM_ (zip [0 :: Int ..] (drop (length ls - 400) ls)) $ \(n, l) -> withKey n $
        labelWith (fillW . fontMono . fontSizeScale 0.85 . (if "λ>" `T.isPrefixOf` l then fontTone Accent else id)) (if T.null l then " " else l)
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
