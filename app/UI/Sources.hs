-- | Data sources popover grouping loaded artifacts and connections by run.
module UI.Sources
  ( Source (..)
  , Datum (..)
  , datumTitle
  , sourcesPopover
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.List ((!?))
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)

import Data.Text (Text)
import Data.Text qualified as T
import NanoUI

import Stronghead.Format (tshow)
import Stronghead.Next (Datum (..), datumAdds, datumRecipe, datumTitle)
import Stronghead.Run
import UI.Common
import UI.Link

-- | An artifact or a connection.
data Source = Source
  { srcKind :: !Text
    -- ^ "Eventlog", "Agent" ...
  , srcDetail :: !Text
    -- ^ Its path or address.
  , srcRun :: !RunInfo
    -- ^ The run it is from, as Stronghead treats it; 'emptyRunInfo' for
    -- sources joined by code only (dumps, GHCi).
  , srcOrigin :: !RunKey
    -- ^ The run it named itself.
  , srcLinked :: !Bool
    -- ^ Joined to its run by hand.
  , srcPanel :: !Panel
  , srcData :: ![Datum]
    -- ^ What it brings.
  }

-- | The popover, anchored under its button in the header. @link ss ri@
-- treats the sources @ss@ as from run @ri@; @unlink@ undoes that for the
-- run a source named itself.
sourcesPopover :: Env -> Var Bool -> Rect -> [Source] -> ([Source] -> RunInfo -> NanoUI ()) -> (RunKey -> NanoUI ()) -> NanoUI ()
sourcesPopover env open anchor sources link unlink =
  popupWith open (defaultPopupConfig (AnchorRect anchor)) (fixedW 720 . maxH 640) $
    void . scrollArea fillW $
      columnWith (fillW . padAll 12 . tight . gap 10) $ do
        labelWith (fontMedium . fontSizeScale 1.05) "Data sources"
        let timed = [s | s <- sources, srcRun s /= emptyRunInfo]
            codeOnly = [s | s <- sources, srcRun s == emptyRunInfo]
            grouped = groupRuns (map srcRun timed)
            runs = [mapMaybe (\(i, _) -> timed !? i) g | g <- grouped]
            matches = [map snd g | g <- grouped]
            have = nubOrd (concatMap srcData sources)
        scope $ when (null runs) $
          labelWith (fillW . fontMuted) "No run yet: open an eventlog, a heap or cost-centre profile, or connect to a running program in Live."
        forM_ (zip3 [0 :: Int ..] runs matches) $ \(ri, members, ms) -> withKey ri $ do
          let info = foldMap srcRun members
              mine = nubOrd (concatMap srcData members)
              probable = ProbablySameRun `elem` ms
          columnWith (fillW . gap 4 . padTop 4 . tight) $ do
            rowWith (fillW . tight . gap 8) $ do
              labelWith (alignMid . fontMedium) (runTitle info)
              scope $ when probable $ badge Muted "matched by program name: no pid on one side, times approximate"
              flex
              scope $ when (ri > 0) $ for_ (listToMaybe runs) $ \firstRun ->
                let target = foldMap srcRun firstRun
                 in whenM (buttonWith (alignMid . padXY 8 2 . tight) ("Same run as " <> runTitle target)) (link members target)
            forM_ (zip [0 :: Int ..] members) $ \(si, s) -> withKey si (sourceRow env s unlink)
            missing (filter (`notElem` mine) [DTimeline, DHeapCensus, DHeapSource, DCostCentres, DLive, DLiveStats, DLiveGraph]) (riProgram info)
        scope $ unless (null codeOnly) $
          columnWith (fillW . gap 4 . padTop 4 . tight) $ do
            labelWith (alignMid . fontMedium) "Not tied to a run (joined by code)"
            forM_ (zip [0 :: Int ..] codeOnly) $ \(si, s) -> withKey si (sourceRow env s unlink)
        let missingCode = filter (`notElem` have) [DDumps, DDebugger]
        scope $ unless (null missingCode) $ missing missingCode Nothing
        columnWith (fillW . gap 4 . padTop 6 . tight) $ do
          labelWith (fontMedium . fontSizeScale 0.92) "Everything at once"
          let prog = fromMaybe "<your-program>" (listToMaybe [p | s <- timed, Just p <- [riProgram (srcRun s)]])
          recipe "build" ("cabal build " <> prog <> " --ghc-options=\"-finfo-table-map -fdistinct-constructor-tables -ddump-simpl -ddump-stg-final -ddump-to-file\"")
          recipe "run" (prog <> " +RTS -l -hi -i0.1 -T -RTS")

runTitle :: RunInfo -> Text
runTitle ri = fromMaybe "an unknown program" (riProgram ri) <> maybe "" (\p -> ", pid " <> tshow p) (riPid ri)

sourceRow :: Env -> Source -> (RunKey -> NanoUI ()) -> NanoUI ()
sourceRow env s unlink =
  rowWith (fillW . gap 8 . padLeft 12 . tight) $ do
    (_, area) <- withCursorShape UiCursorPointer $ mouseArea tight $ labelWith (alignBaseline . fixedW 130) (srcKind s)
    when (respClicked area) (envSwitch env (srcPanel s))
    tooltip area ("Open " <> panelTitle (srcPanel s))
    columnWith (fillW . tight . gap 0) $ do
      labelWith (fillW . fontMono . fontSizeScale 0.85) (T.take 90 (srcDetail s))
      labelWith (fillW . fontMuted . fontSizeScale 0.85) (T.intercalate "; " (map datumAdds (srcData s)))
    scope $ when (srcLinked s) $ whenM (buttonWith (alignBaseline . padXY 6 0 . tight) "Unlink") (unlink (srcOrigin s))

-- | What a run lacks, each with what it would add and how to get it.
missing :: [Datum] -> Maybe Text -> NanoUI ()
missing ds prog = scope $ unless (null ds) $
  columnWith (fillW . gap 6 . padLeft 12 . padTop 2 . tight) $ do
    labelWith (fontMuted . fontSizeScale 0.85) "Missing"
    forM_ ds $ \d -> withKey (fromEnum d) $
      columnWith (fillW . tight . gap 2) $ do
        labelWith (fillW . fontSizeScale 0.88) (datumTitle d <> ": " <> datumAdds d)
        let (build, run) = datumRecipe d
        scope $ for_ build $ \b -> recipe "build" b
        scope $ for_ run $ \r -> recipe "run" (maybe "" (<> " ") (if isNothing build then prog else Nothing) <> r)

-- | A line to copy, after what it is for.
recipe :: Text -> Text -> NanoUI ()
recipe what t =
  rowWith (fillW . gap 6 . padLeft 12 . tight) $ do
    labelWith (alignMid . fixedW 40 . fontMuted . fontSizeScale 0.82) what
    columnWith (alignMid . fillW . tight) $ labelWith (fillW . fontMono . fontSizeScale 0.82) t
    whenM (buttonWith (alignMid . padXY 6 0 . tight) "Copy") (void (setClipboard t))
