-- | The pinboard: entities pinned from any menu or the inspector, each with
-- a note, in the user's order. A click on a pin goes to it. The board
-- exports as a Markdown report: each pin's title, its reference, every
-- panel's facts about it as text, and the note.
--
-- Pins live on the bus, so every panel's menus can add them, and are saved
-- with the session.
module UI.Pinboard
  ( pinboardPane
  , pinsReport
  , pinsDossier
  , pinReport
  ) where

import Control.Monad (forM_, when)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import Stronghead.Format (tshow)
import Stronghead.Ref (Ref (..), refTarget)
import Stronghead.Session (Pin (..), PinReport (..), ReportSection (..), pinMove, pinsMarkdown)
import Stronghead.Source (renderSrcSpan)
import UI.Common
import UI.Link

-- | The board. @export@ saves the report where the user chooses.
pinboardPane :: Env -> NanoUI () -> PaneCtx -> NanoUI ()
pinboardPane env export ctx = do
  let bus = envBus env
  pins <- readVar (busPins bus)
  let header = do
        paneMeta (tshow (length pins))
        flex
        disabledWhen (null pins) $ buttonGroup $ do
          copyFeedbackButton (padXY 8 2 . tight) "Copy report" (pinsReport env [] pins)
          whenM (iconButtonWith (padXY 8 2 . tight) iconSave "Export…") export
  paneWith tight ctx "Pinboard" header $
    if null pins
      then columnWith (padAll 12 . fillW) $ hint "Pin a thread, a band, a binding or a line from its right-click menu or the inspector. Pins keep a note each, and are saved with the session."
      else paneScroll (grow . tight) $ columnWith (padAll 8 . tight . gap 8 . fillW) $
        forM_ (zip [0 :: Int ..] pins) $ \(i, p) -> withKey (refTarget (pinRef p)) $
          columnWith (fillW . tight . gap 4) $ do
            rowWith (fillW . itemsCenter . tight . gap 6) $ do
              linkText env (pinRef p) fontMedium (T.take 70 (refTitle env (pinRef p)))
              flex
              let small label tip enabled act = do
                    resp <- disabledWhen (not enabled) (buttonWith' (padXY 6 0 . tight) label)
                    tooltip resp tip
                    when (respClicked resp) act
              buttonGroup $ do
                small "↑" "Move up" (i > 0) (modifyVar (busPins bus) (pinMove i (-1)))
                small "↓" "Move down" (i < length pins - 1) (modifyVar (busPins bus) (pinMove i 1))
                small "×" "Unpin" True (modifyVar (busPins bus) (filter ((/= pinRef p) . pinRef)))
            textInputConfigured
              defaultTextInputConfig {ticPlaceholder = "Note", ticLayout = fillW}
              (zoomVar (noteOf (pinRef p)) (setNote (pinRef p)) (busPins bus))
  where
    noteOf r = maybe "" pinNote . find ((== r) . pinRef)
    setNote r note = map (\q -> if pinRef q == r then q {pinNote = note} else q)

-- | One pin as the report has it: every panel's facts as text.
pinReport :: Env -> Pin -> PinReport
pinReport env p =
  PinReport
    { prTitle = refTitle env r
    , prTarget = refTarget r
    , prSections =
        [ReportSection "Source" Nothing [("at", renderSrcSpan sp)] [] | not (isSource r), Just sp <- [refSpan env r]]
          ++ [ ReportSection (panelTitle (lensPanel l)) (peekExact pk) (peekFacts pk) (peekNotes pk)
             | l <- envLenses env
             , Just pk <- [lensPeek l r]
             ]
    , prNote = pinNote p
    }
  where
    r = pinRef p
    isSource = \case
      RSource _ -> True
      _ -> False

-- | The board as Markdown, with @intro@ lines under the title.
pinsReport :: Env -> [Text] -> [Pin] -> Text
pinsReport env intro pins = pinsMarkdown "Stronghead pins" intro (map (pinReport env) pins)

-- | A Post-Mortem Dossier: the pinboard Markdown report enriched with embedded
-- companion SVG proof graphs (heap residency, before/after diff, flamegraph).
pinsDossier :: Env -> [Text] -> [(Text, Text)] -> [Pin] -> Text
pinsDossier env intro images pins =
  let base = pinsMarkdown "Stronghead Post-Mortem Dossier" intro (map (pinReport env) pins)
      imgBlock =
        if null images
          then ""
          else
            "\n## Proof Graphs\n\n"
              <> T.unlines ["### " <> caption <> "\n\n![" <> caption <> "](" <> relPath <> ")\n" | (caption, relPath) <- images]
   in base <> imgBlock

