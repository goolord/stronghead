-- | The command palette (Ctrl+K): one search over everything every panel
-- has loaded (threads, bindings, cost centres, bands, closures, markers)
-- and the commands that act on them. Enter goes to the highlighted hit,
-- the arrow keys move the highlight, Esc closes.
module UI.Palette
  ( CommandPalette (..)
  , newPalette
  , openPalette
  , paletteView
  ) where

import Control.Monad (forM_, when)
import Data.Foldable (for_)
import Data.Text qualified as T
import NanoUI
import Stronghead.Fuzzy (fuzzyRank)
import UI.Common
import UI.Link

data CommandPalette = CommandPalette
  { plOpen :: !(Var Bool)
  , plQuery :: !(Var T.Text)
  , plIndex :: !(Var Int)
  , plFresh :: !(Var Bool)
    -- ^ Opened this frame: focus the search field.
  }

newPalette :: IO CommandPalette
newPalette = CommandPalette <$> newVar False <*> newVar "" <*> newVar 0 <*> newVar False

openPalette :: CommandPalette -> NanoUI ()
openPalette pl = do
  writeVar (plQuery pl) ""
  writeVar (plIndex pl) 0
  writeVar (plFresh pl) True
  writeVar (plOpen pl) True

-- | Results shown at once; the query narrows the rest.
shown :: Int
shown = 60

-- | @commands@ come first when the query is empty, then every lens's hits.
paletteView :: Env -> CommandPalette -> [Hit] -> NanoUI ()
paletteView env pl commands =
  modalWith (fixedW 720) (plOpen pl) "Go to anything" $
    columnWith (fillW . tight . gap 8) $ do
      resp <-
        textInputConfigured'
          defaultTextInputConfig
            { ticPlaceholder = "Threads, bindings, cost centres, bands, closures, markers, commands…"
            , ticLayout = fillW defaultLayout
            }
          (plQuery pl)
      fresh <- readVar (plFresh pl)
      when fresh $ do
        writeVar (plFresh pl) False
        requestFocus (respId resp)
      query <- readVar (plQuery pl)
      let hits = commands ++ concatMap lensHits (envLenses env)
          ranked = map snd (take shown (fuzzyRank query (\h -> hitLabel h <> " " <> hitKind h) hits))
          n = length ranked
      idx0 <- readVar (plIndex pl)
      up <- keyPressed KeyUp
      down <- keyPressed KeyDown
      let idx
            | n == 0 = 0
            | down = (idx0 + 1) `mod` n
            | up = (idx0 - 1) `mod` n
            | otherwise = min idx0 (n - 1)
      when (idx /= idx0) (writeVar (plIndex pl) idx)
      enter <- keyPressed KeyEnter
      let run h = do
            writeVar (plOpen pl) False
            case hitTarget h of
              HitRef r -> goTo env r
              HitRun act -> act
      when ((respSubmitted resp || enter) && n > 0) $
        for_ (take 1 (drop idx ranked)) run
      scope $
        if n == 0
          then hint ("Nothing matches “" <> query <> "”.")
          else paneScroll (fixedH 420 . tight) $ columnWith (tight . fillW . gap 0) $
            forM_ (zip [0 :: Int ..] ranked) $ \(i, h) -> withKey i $ do
              picked <- selectableRow (i == idx) id $ do
                cell (fixedW 110 . fontMuted . fontSizeScale 0.85) (hitKind h)
                cell (fontSizeScale 0.95) (T.take 70 (hitLabel h))
                cell (fillW . fontMuted . fontSizeScale 0.85) (T.take 70 (hitDetail h))
              when picked (run h)
      labelWith (fontMuted . fontSizeScale 0.8) "Enter to go, ↑ ↓ to choose, Esc to close"
