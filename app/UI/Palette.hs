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

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.Text qualified as T
import NanoUI
import NanoUI.Adornment qualified as A
import Stronghead.Format (tshow)
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
            , ticLayout = (fillW . gap 6) defaultLayout
            , ticAdornments = A.leading (A.icon iconSearch)
            }
          (plQuery pl)
      fresh <- readVar (plFresh pl)
      when fresh $ do
        writeVar (plFresh pl) False
        requestFocus (respId resp)
      query <- readVar (plQuery pl)
      let hits = commands ++ concatMap lensHits (envLenses env)
          allRanked = fuzzyRank query (\h -> hitLabel h <> " " <> hitKind h) hits
          totalMatches = length allRanked
          ranked = map snd (take shown allRanked)
          n = length ranked
      idx0 <- readVar (plIndex pl)
      up <- keyPressed KeyUp
      down <- keyPressed KeyDown
      let idx
            | n == 0 = 0
            | down = (idx0 + 1) `mod` n
            | up = (idx0 - 1) `mod` n
            | otherwise = min idx0 (n - 1)
          idxMoved = idx /= idx0
      when idxMoved (writeVar (plIndex pl) idx)
      enter <- keyPressed KeyEnter
      let run h = do
            writeVar (plOpen pl) False
            case hitTarget h of
              HitRef r -> goTo env r
              HitRun act -> act
      when ((respSubmitted resp || enter) && n > 0) $
        for_ (take 1 (drop idx ranked)) run
      rowWith (fillW . tight . gap 8 . itemsCenter) $ do
        labelWith (fontMuted . fontSizeScale 0.8) $
          if T.null (T.strip query)
            then tshow totalMatches <> " available"
            else tshow totalMatches <> (if totalMatches == 1 then " match" else " matches") <> (if totalMatches > shown then " (showing top " <> tshow shown <> ")" else "")
        flex
        scope $ unless (T.null query) $
          whenM (styled quiet (buttonWith (padXY 6 1 . tight . fontSizeScale 0.8) "Clear")) (writeVar (plQuery pl) "")
      scope $
        if n == 0
          then columnWith (padAll 20 . fillW) $
            hint ("Nothing matches “" <> query <> "”.")
          else do
            (scrollWid, ()) <- paneScrollArea (fixedH 380 . tight) $ columnWith (tight . fillW . gap 0 . padRight 6) $
              forM_ (zip [0 :: Int ..] ranked) $ \(i, h) -> withKey i $ do
                picked <- selectableRow (i == idx) (padXY 8 4) $ do
                  cell (fixedW 92 . fontMuted . fontSizeScale 0.82) (hitKind h)
                  cell (fontSizeScale 0.94 . (if i == idx then fontMedium else id)) (T.take 64 (hitLabel h))
                  flex
                  let det = hitDetail h
                  scope $ unless (T.null det) $
                    cell (fontMono . fontMuted . fontSizeScale 0.82) (T.take 56 det)
                when picked (run h)
            when (idxMoved && (up || down)) $ do
              let rowH = 28 :: Float
              scrollRectIntoViewUi scrollWid (Rect 0 (fromIntegral idx * rowH) 1 rowH) ScrollNearest ScrollInstant
      labelWith (fontMuted . fontSizeScale 0.8 . padTop 2) "Enter to open · ↑↓ to choose · Esc to close"
