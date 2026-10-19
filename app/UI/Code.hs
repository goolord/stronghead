-- | A read-only code pane drawn on a canvas: only visible lines are built,
-- tokens are coloured by kind, every occurrence of the hovered name is
-- highlighted (across panes, via the caller), and a gutter carries line
-- numbers and markers.
module UI.Code
  ( CodeViewState (..)
  , newCodeViewState
  , CodeConfig (..)
  , defaultCodeConfig
  , GutterMark (..)
  , CodeEvent (..)
  , codeView
  , scrollToLine
  ) where

import Control.Monad (forM_, when)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import Stronghead.Dump (TokKind (..), Token (..), nameKey, stripUnique)
import UI.Common

data CodeViewState = CodeViewState
  { cvScroll :: !(Var Float)
    -- ^ First visible line.
  , cvHScroll :: !(Var Float)
    -- ^ First visible column.
  }

newCodeViewState :: IO CodeViewState
newCodeViewState = do
  cvScroll <- newVar 0
  cvHScroll <- newVar 0
  pure CodeViewState {..}

scrollToLine :: CodeViewState -> Int -> NanoUI ()
scrollToLine cv l = do
  writeVar (cvScroll cv) (fromIntegral (max 0 (l - 3)))
  writeVar (cvHScroll cv) 0

data GutterMark
  = MarkBreakpoint
  | MarkCandidate
  | MarkCurrent
  | MarkDot !Color
  deriving (Eq)

data CodeConfig = CodeConfig
  { ccLineCount :: !Int
  , ccTokens :: Int -> [Token]
  , ccHideUniques :: !Bool
  , ccHot :: !(Maybe Text)
    -- ^ Name key whose occurrences are highlighted.
  , ccRanges :: ![(Int, Int, Color)]
    -- ^ Line ranges (0-based, inclusive) with a background.
  , ccSpans :: ![(Int, Int, Int, Int, Color)]
    -- ^ Exact spans: line, column, end line, end column (0-based, end
    -- column exclusive).
  , ccGutter :: Int -> Maybe GutterMark
  , ccLayout :: Layout -> Layout
  , ccEmpty :: !Text
  }

defaultCodeConfig :: CodeConfig
defaultCodeConfig = CodeConfig 0 (const []) False Nothing [] [] (const Nothing) (fillW . grow) ""

data CodeEvent = CodeEvent
  { ceHoverKey :: !(Maybe Text)
  , ceClickedKey :: !(Maybe Text)
  , ceGutterClick :: !(Maybe Int)
  , ceClickedLine :: !(Maybe Int)
  }

-- | Tokens as displayed: unique suffixes optionally stripped, with columns
-- recomputed so the rest of the line closes up.
displayTokens :: Bool -> [Token] -> [Token]
displayTokens False ts = ts
displayTokens True ts = go 0 ts
  where
    go _ [] = []
    go col (t : rest) =
      let txt = if tkKind t `elem` [TkIdent, TkCon] then stripUnique (tkText t) else tkText t
       in t {tkText = txt, tkCol = col} : go (col + T.length txt) rest

isNameTok :: Token -> Bool
isNameTok t = tkKind t `elem` [TkIdent, TkCon]

codeView :: CodeViewState -> MonoMetrics -> CodeConfig -> NanoUI CodeEvent
codeView cv mm cfg = do
  th <- uiTheme
  let pal = palette th
      lh = mmLineH mm + 3
      cw = mmCharW mm
      n = ccLineCount cfg
      gutterW = fromIntegral (length (show (max 1 n))) * cw + 22
  scroll0 <- readVar (cvScroll cv)
  hscroll0 <- readVar (cvHScroll cv)
  mouse <- uiMousePos
  sid <- currentId
  prev <- fromMaybe (Rect 0 0 0 0) <$> lastRect sid
  let visibleLines = max 1 (floor (rectH prev / lh)) :: Int
      maxScroll = fromIntegral (max 0 (n - visibleLines + 2))
      scroll = max 0 (min maxScroll scroll0)
      hscroll = max 0 hscroll0
      first = floor scroll :: Int
      lineAt (Rect _ y _ _) my = first + floor ((my - y - 4) / lh)
      colAt (Rect x _ _ _) mx = floor ((mx - x - gutterW - 6) / cw + hscroll) :: Int
      hovered
        | rectContains prev mouse && v2X mouse > rectX prev + gutterW =
            let l = lineAt prev (v2Y mouse)
                c = colAt prev (v2X mouse)
             in if l >= 0 && l < n
                  then (l,) <$> listToMaybe [t | t <- displayTokens (ccHideUniques cfg) (ccTokens cfg l), isNameTok t, c >= tkCol t, c < tkCol t + T.length (tkText t)]
                  else Nothing
        | otherwise = Nothing
      hoverKey = nameKey . tkText . snd <$> hovered
      hot = maybe (ccHot cfg) Just hoverKey
  resp <-
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = ccLayout cfg defaultLayout
        , canvasTrackPointer = True
        , canvasCursor = Just (\_ (Rect x _ _ _) (V2 px _) -> if px > x + gutterW then UiCursorText else UiCursorDefault)
        }
      $ \r@(Rect x y w h) -> do
        drawRoundedRect r 4 (pTrackBg pal)
        let count = ceiling (h / lh) + 1
            lines' = [l | l <- [first .. first + count], l < n]
            codeX = x + gutterW + 6
            colX c = codeX + (fromIntegral c - hscroll) * cw
            lineY l = y + 4 + fromIntegral (l - first) * lh
        drawRect (Rect x y gutterW h) (alpha 0.04 (pText pal))
        withClip r $ do
          -- Backgrounds first.
          forM_ (ccRanges cfg) $ \(a, b, c) ->
            when (b >= first && a <= first + count) $ do
              let ya = lineY (max a first)
                  yb = lineY (min b (first + count) + 1)
              drawRect (Rect (x + gutterW) ya (w - gutterW) (yb - ya)) c
          forM_ (ccSpans cfg) $ \(l0, c0, l1, c1, c) ->
            forM_ [max l0 first .. min l1 (first + count)] $ \l -> do
              let ca = if l == l0 then c0 else 0
                  cb = if l == l1 then c1 else 400
              drawRect (Rect (max codeX (colX ca)) (lineY l) (max cw (colX cb - max codeX (colX ca))) lh) c
          forM_ lines' $ \l -> do
            let ly = lineY l
                mid = ly + lh / 2
            drawTextWith (mmFont mm) (V2 (x + gutterW - 8) mid) AlignEnd AlignMiddle (T.pack (show (l + 1))) (pMuted pal)
            for_ (ccGutter cfg l) $ \case
              MarkBreakpoint -> drawCircle (V2 (x + 8) mid) 5 (pBlocked pal)
              MarkCandidate -> drawCircle (V2 (x + 8) mid) 2 (alpha 0.5 (pMuted pal))
              MarkCurrent -> drawRoundedRect (Rect (x + 2) (mid - 5) 12 10) 3 (pGC pal)
              MarkDot c -> drawCircle (V2 (x + 8) mid) 4 c
            withClip (Rect (x + gutterW) y (w - gutterW) h) $
              forM_ (displayTokens (ccHideUniques cfg) (ccTokens cfg l)) $ \t -> do
                let tx = colX (tkCol t)
                    tw = fromIntegral (T.length (tkText t)) * cw
                when (tx + tw >= codeX && tx <= x + w && tkKind t /= TkSpace) $ do
                  let isHot = isNameTok t && Just (nameKey (tkText t)) == hot
                  when isHot $ drawRoundedRect (Rect (tx - 1) (ly + 1) (tw + 2) (lh - 2)) 3 (alpha 0.28 (pAccent pal))
                  drawTextWith (mmFont mm) (V2 tx mid) AlignStart AlignMiddle (tkText t) (tokenColor th pal t)
                  when (fmap snd hovered == Just t && fmap fst hovered == Just l) $
                    drawRect (Rect tx (ly + lh - 2) tw 1) (pAccent pal)
          when (n == 0) $
            drawTextWith (mmFont mm) (V2 (x + gutterW + 10) (y + 16)) AlignStart AlignMiddle (ccEmpty cfg) (pMuted pal)
          when (n > count) $ do
            let barH = max 16 (h * fromIntegral count / fromIntegral n)
                barY = y + (h - barH) * (scroll / max 1 maxScroll)
            drawRoundedRect (Rect (x + w - 5) barY 4 barH) 2 (pGrid pal)
  inp <- askInput
  (wx, wy) <- useWheelDeltaOn resp
  let shiftHeld = modShift (inputModifiers inp)
  when (wy /= 0 && not shiftHeld) $ writeVar (cvScroll cv) (max 0 (min maxScroll (scroll - wy * 3)))
  when (wy /= 0 && shiftHeld) $ writeVar (cvHScroll cv) (max 0 (hscroll - wy * 6))
  when (wx /= 0) $ writeVar (cvHScroll cv) (max 0 (hscroll - wx * 6))
  let r = respRect resp
      inGutter = v2X mouse <= rectX r + gutterW
      clickedLine = if respClicked resp then Just (lineAt r (v2Y mouse)) else Nothing
  pure
    CodeEvent
      { ceHoverKey = hoverKey
      , ceClickedKey = if respClicked resp then hoverKey else Nothing
      , ceGutterClick = if inGutter then clickedLine else Nothing
      , ceClickedLine = clickedLine
      }

tokenColor :: Theme -> Palette -> Token -> Color
tokenColor th pal t = case tkKind t of
  TkKeyword -> themePurple th
  TkCon
    | T.last (tkText t) == '#' && isBoxCon (tkText t) -> themeOrange th
    | otherwise -> themeYellow th
  TkIdent
    | T.last (tkText t) == '#' -> themeGreen th
    | otherwise -> pText pal
  TkOperator
    | "#" `T.isSuffixOf` tkText t -> themeGreen th
    | otherwise -> pMuted pal
  TkNumber -> themeGreen th
  TkString -> themeGreen th
  TkComment -> alpha 0.7 (pMuted pal)
  TkPragma -> alpha 0.8 (themeAccent th)
  TkSpace -> pText pal
  where
    isBoxCon c = any (`T.isSuffixOf` c) ["I#", "D#", "F#", "W#", "C#", "W8#", "W16#", "W32#", "W64#", "I8#", "I16#", "I32#", "I64#"]
