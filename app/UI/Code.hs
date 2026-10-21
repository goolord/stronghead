-- | A read-only code pane drawn on a canvas: only visible lines are built,
-- tokens are coloured by kind, every occurrence of the hovered name is
-- highlighted (across panes, via the caller), and a gutter carries line
-- numbers and markers.
--
-- Annotated, it also shows what the runtime saw of each line: a lane of
-- heat bars per measurement between the gutter and the code (time,
-- allocation, heap, live closures), gutter marks, and muted notes after the
-- line's end. Every bar and note stands for an entity: clicking it goes
-- there, and right-clicking a line, a name or a note lists every panel's
-- actions ('annotatedCode').
module UI.Code
  ( CodeViewState (..)
  , newCodeViewState
  , CodeConfig (..)
  , defaultCodeConfig
  , GutterMark (..)
  , LineNote (..)
  , FileNotes (..)
  , noNotes
  , fileNotes
  , notesForFile
  , placedNotes
  , withNotes
  , laneLegend
  , Colouring (..)
  , colouringName
  , CodeEvent (..)
  , codeView
  , annotatedCode
  , lanePoint
  , scrollToLine
  , scrollToLineAt
  ) where

import Control.Monad (forM_, guard, when)
import Data.Foldable (for_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (maximumBy, sortOn, unsnoc, (!?))
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as S

import Data.Text (Text)
import Data.Text qualified as T
import NanoUI

import Stronghead.Dump (TokKind (..), Token (..), nameKey, stripUnique)
import Stronghead.Format (tshow)
import Stronghead.Join (Annotation (..), Lane (..), Mark (..), Shade (..), laneName, lineOwners)
import Stronghead.Ref (Ref)
import Stronghead.Source (SourceIndex, SrcSpan (..), resolvesTo)
import UI.Common
import UI.Link (Env, canvasLinks, goTo)

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

-- | Scroll a line into view at a column: for code indented far to the
-- right, like STG.
scrollToLineAt :: CodeViewState -> Int -> Int -> NanoUI ()
scrollToLineAt cv l col = do
  writeVar (cvScroll cv) (fromIntegral (max 0 (l - 3)))
  writeVar (cvHScroll cv) (fromIntegral (max 0 col))

data GutterMark
  = MarkBreakpoint
  | MarkCandidate
  | MarkCurrent
  | MarkDot !Color
  deriving stock (Eq)

-- | One heat bar: a line's share of what its lane measures.
data LineNote = LineNote
  { lnLane :: !Lane
  , lnWeight :: !Float
    -- ^ 0 to 1, of the file's largest in this lane.
  , lnHead :: !Bool
    -- ^ The first line of the span it came from; the rest are drawn fainter.
  , lnShade :: !Shade
  , lnRef :: !(Maybe Ref)
  }

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
  , ccLanes :: ![Lane]
    -- ^ The heat lanes to draw, left to right.
  , ccBars :: Int -> [LineNote]
  , ccHints :: Int -> [(Text, Maybe Ref)]
    -- ^ Notes written after the line's end, each standing for its entity.
  , ccPinned :: Int -> Bool
    -- ^ Lines whose notes stay in view, over the code if they must (the
    -- line in focus). The line under the pointer always does.
  , ccLayout :: Layout -> Layout
  , ccEmpty :: !Text
  , ccColouring :: !Colouring
  }

defaultCodeConfig :: CodeConfig
defaultCodeConfig =
  CodeConfig
    { ccLineCount = 0
    , ccTokens = const []
    , ccHideUniques = False
    , ccHot = Nothing
    , ccRanges = []
    , ccSpans = []
    , ccGutter = const Nothing
    , ccLanes = []
    , ccBars = const []
    , ccHints = const []
    , ccPinned = const False
    , ccLayout = fillW . grow
    , ccEmpty = ""
    , ccColouring = SyntaxColours
    }

data CodeEvent = CodeEvent
  { ceHoverKey :: !(Maybe Text)
  , ceClickedKey :: !(Maybe Text)
  , ceGutterClick :: !(Maybe Int)
  , ceClickedLine :: !(Maybe Int)
  , ceHoverLine :: !(Maybe Int)
    -- ^ The line under the pointer, 0-based.
  , ceNoteRef :: !(Maybe Ref)
    -- ^ The entity of the bar or note under the pointer.
  , ceResp :: !Response
  }

-- ---------------------------------------------------------------------------
-- Annotations

-- | What every panel says about one file, by 0-based line.
data FileNotes = FileNotes
  { fnLanes :: ![Lane]
  , fnBars :: !(IntMap [LineNote])
  , fnHints :: !(IntMap [(Text, Maybe Ref)])
  , fnMarks :: !(IntMap Mark)
  }

noNotes :: FileNotes
noNotes = FileNotes [] IM.empty IM.empty IM.empty

-- | The annotations whose spans are in this file (as @here@ decides), on
-- the lines their spans cover.
fileNotes :: (SrcSpan -> Bool) -> [Annotation] -> FileNotes
fileNotes here anns =
  placedNotes [(ssLine sp - 1, ssEndLine sp - 1, a) | a <- anns, Just sp <- [anSpan a], here sp]

-- | The annotations on a file, finding the files they name through the
-- source index. Each reported path is resolved once.
notesForFile :: SourceIndex -> FilePath -> [Annotation] -> FileNotes
notesForFile ix path anns =
  let reported = S.fromList [ssFile sp | a <- anns, Just sp <- [anSpan a]]
      here = M.fromSet (resolvesTo ix path) reported
   in fileNotes (\sp -> M.findWithDefault False (ssFile sp) here) anns

-- | Annotations already placed on line ranges (0-based, inclusive). In each
-- lane a line shows the smallest spans covering it, so a nested cost
-- centre's lines show its own cost. Bars are scaled to the file's largest.
placedNotes :: [(Int, Int, Annotation)] -> FileNotes
placedNotes placed = FileNotes {..}
  where
    fnLanes = [l | l <- [LaneTime, LaneAlloc, LaneHeap, LaneLive, LaneEntries, LaneTicky], any (\(_, _, a) -> anLane a == l && anValue a > 0) placed]
    fnBars = IM.unionsWith (++) (map laneBars fnLanes)
    laneBars l =
      let owners = lineOwners [(a0, b, (a0, a)) | (a0, b, a) <- placed, anLane a == l, anValue a > 0]
          value = sum . map (anValue . snd)
          top = max 1e-12 (maximum (0 : map value (IM.elems owners)))
       in IM.mapWithKey
            ( \line os ->
                [ LineNote
                    { lnLane = l
                    , lnWeight = realToFrac (min 1 (value os / top))
                    , lnHead = any ((== line) . fst) os
                    , lnShade = maximum (map (anShade . snd) os)
                    , lnRef = anRef (snd (maximumBy (comparing (anValue . snd)) os))
                    }
                ]
            )
            owners
    fnHints =
      IM.map (take 3) . IM.fromListWith (flip (++)) $
        [(a0, [(anText a, anRef a)]) | (a0, _, a) <- sortOn (\(_, _, a) -> (hintOrder (anLane a), Down (anValue a))) placed, not (T.null (anText a))]
    fnMarks = IM.fromListWith max [(a0, m) | (a0, _, a) <- placed, LaneMark m <- [anLane a]]
    -- A binding's badges lead; the measurements follow.
    hintOrder = \case
      LaneNote -> 0 :: Int
      LaneMark _ -> 1
      _ -> 2

-- | Draw the notes in a view. The view's own gutter marks win.
withNotes :: FileNotes -> CodeConfig -> CodeConfig
withNotes fn cfg =
  cfg
    { ccLanes = fnLanes fn
    , ccBars = \l -> IM.findWithDefault [] l (fnBars fn)
    , ccHints = \l -> IM.findWithDefault [] l (fnHints fn)
    , ccGutter = \l -> case ccGutter cfg l of
        Just m -> Just m
        Nothing -> markGutter <$> IM.lookup l (fnMarks fn)
    }
  where
    markGutter = \case
      BreakMark -> MarkBreakpoint
      CandidateMark -> MarkCandidate
      StopMark -> MarkCurrent

-- | The lanes shown, for a pane's header: "lanes: time · alloc · heap".
laneLegend :: FileNotes -> Text
laneLegend fn = case fnLanes fn of
  [] -> ""
  ls -> "lanes: " <> T.intercalate " · " (map laneName ls)

-- ---------------------------------------------------------------------------
-- The view

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

laneW :: Float
laneW = 26

-- | Where things go across a code view: the gutter, then the lanes, then
-- the code.
data Geom = Geom
  { gGutterW :: !Float
  , gCodeLeft :: !Float
    -- ^ From the view's left edge to the code's.
  , gLineH :: !Float
  }

geom :: MonoMetrics -> CodeConfig -> Geom
geom mm cfg =
  let gutterW = fromIntegral (length (show (max 1 (ccLineCount cfg)))) * mmCharW mm + 22
      lanesW = if null (ccLanes cfg) then 0 else fromIntegral (length (ccLanes cfg)) * laneW + 4
   in Geom gutterW (gutterW + lanesW) (mmLineH mm + 3)

-- | The middle of a lane's bar on a line, in a view at @r@ scrolled to the
-- top: for tests that click one.
lanePoint :: MonoMetrics -> CodeConfig -> Rect -> Int -> Int -> V2
lanePoint mm cfg (Rect x y _ _) line lane =
  let g = geom mm cfg
   in V2 (x + gGutterW g + (fromIntegral lane + 0.5) * laneW) (y + 4 + (fromIntegral line + 0.5) * gLineH g)

-- | Where each note on a line starts and ends, in columns: after the code,
-- three columns apart, or pinned to the visible right edge (column
-- @right@) when the line runs past it. Says whether they were pinned.
hintColumns :: Maybe Int -> [Token] -> [(Text, Maybe Ref)] -> (Bool, [(Int, Int, (Text, Maybe Ref))])
hintColumns mright toks hints =
  let end = maybe 0 (\(_, t) -> tkCol t + T.length (tkText t)) (unsnoc toks)
      total = sum (map (T.length . fst) hints) + 3 * (length hints - 1)
      natural = end + 3
      right = fromMaybe maxBound mright
      pinned = not (null hints) && natural + total > right
      go _ [] = []
      go c (h@(txt, _) : rest) = (c, c + T.length txt, h) : go (c + T.length txt + 3) rest
   in (pinned, go (if pinned then max 0 (right - total) else natural) hints)

codeView :: CodeViewState -> MonoMetrics -> CodeConfig -> NanoUI CodeEvent
codeView cv mm cfg = do
  th <- uiTheme
  let pal = palette th
      g = geom mm cfg
      gutterW = gGutterW g
      codeLeft = gCodeLeft g
      lh = gLineH g
      cw = mmCharW mm
      n = ccLineCount cfg
      toksOf l = displayTokens (ccHideUniques cfg) (ccTokens cfg l)
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
      colAt (Rect x _ _ _) mx = floor ((mx - x - codeLeft - 6) / cw + hscroll) :: Int
      -- The last column in view, for notes pinned to the right edge.
      rightCol (Rect _ _ w _) l
        | ccPinned cfg l || Just l == hoverLine = Just (floor ((w - codeLeft - 16) / cw + hscroll))
        | otherwise = Nothing
      inside = rectContains prev mouse
      hoverLine =
        let l = lineAt prev (v2Y mouse) in if inside && l >= 0 && l < n then Just l else Nothing
      inCode = inside && v2X mouse > rectX prev + codeLeft
      hovered = do
        l <- hoverLine
        if inCode
          then (l,) <$> listToMaybe [t | let c = colAt prev (v2X mouse), t <- toksOf l, isNameTok t, c >= tkCol t, c < tkCol t + T.length (tkText t)]
          else Nothing
      -- The note under the pointer, and the bar.
      hoverHint = do
        l <- hoverLine
        if inCode
          then (l,) <$> listToMaybe [h | let c = colAt prev (v2X mouse), h@(a, b, _) <- snd (hintColumns (rightCol prev l) (toksOf l) (ccHints cfg l)), c >= a, c < b]
          else Nothing
      hoverBar = do
        l <- hoverLine
        let dx = v2X mouse - rectX prev - gutterW
            i = floor (dx / laneW)
        guard (inside && dx >= 0 && v2X mouse < rectX prev + codeLeft)
        lane <- ccLanes cfg !? i
        listToMaybe [b | b <- ccBars cfg l, lnLane b == lane]
      noteRef = case hoverHint of
        Just (_, (_, _, (_, r))) -> r
        Nothing -> hoverBar >>= lnRef
      hoverKey = if isJust noteRef then Nothing else nameKey . tkText . snd <$> hovered
      hot = maybe (ccHot cfg) Just hoverKey
  resp <-
    canvasConfigured
      defaultCanvasConfig
        { canvasLayout = ccLayout cfg defaultLayout
        , canvasTrackPointer = True
        , canvasCursor = Just $ \_ (Rect x _ _ _) (V2 px _) ->
            if
              | px > x + codeLeft -> if isJust noteRef then UiCursorPointer else UiCursorText
              | px > x + gutterW -> UiCursorPointer
              | otherwise -> UiCursorDefault
        }
      $ \r@(Rect x y w h) -> do
        let count = ceiling (h / lh) + 1
            lines' = [l | l <- [first .. first + count], l < n]
            codeX = x + codeLeft + 6
            colX c = codeX + (fromIntegral c - hscroll) * cw
            lineY l = y + 4 + fromIntegral (l - first) * lh
            codeArea = Rect (x + codeLeft) y (w - codeLeft) h
        drawRect (Rect x y gutterW h) (alpha 0.025 (pText pal)) >> drawRect (Rect (x + gutterW) y 1 h) (pGrid pal)
        when (codeLeft > gutterW) $ drawRect (Rect (x + codeLeft - 1) y 1 h) (pGrid pal)
        withClip r $ do
          -- Backgrounds first.
          forM_ (ccRanges cfg) $ \(a, b, c) ->
            when (b >= first && a <= first + count) $ do
              let ya = lineY (max a first)
                  yb = lineY (min b (first + count) + 1)
              drawRect (Rect (x + codeLeft) ya (w - codeLeft) (yb - ya)) c
          forM_ (ccSpans cfg) $ \(l0, c0, l1, c1, c) ->
            forM_ [max l0 first .. min l1 (first + count)] $ \l -> do
              let ca = if l == l0 then c0 else 0
                  cb = if l == l1 then c1 else 400
              drawRect (Rect (max codeX (colX ca)) (lineY l) (max cw (colX cb - max codeX (colX ca))) lh) c
          forM_ lines' $ \l -> do
            let ly = lineY l
                mid = ly + lh / 2
                toks = toksOf l
            drawTextWith (mmFont mm) (V2 (x + gutterW - 8) mid) AlignEnd AlignMiddle (tshow (l + 1)) (pMuted pal)
            for_ (ccGutter cfg l) $ \case
              MarkBreakpoint -> drawCircle (V2 (x + 8) mid) 5 (pBlocked pal)
              MarkCandidate -> drawCircle (V2 (x + 8) mid) 2 (alpha 0.5 (pMuted pal))
              MarkCurrent -> drawRoundedRect (Rect (x + 2) (mid - 5) 12 10) 3 (pGC pal)
              MarkDot c -> drawCircle (V2 (x + 8) mid) 4 c
            -- Heat bars: ink, except thunks and survivors.
            forM_ (zip [0 :: Int ..] (ccLanes cfg)) $ \(i, lane) ->
              forM_ [b | b <- ccBars cfg l, lnLane b == lane] $ \b -> do
                let lx = x + gutterW + 3 + fromIntegral i * laneW
                    bw = max 2 ((laneW - 6) * lnWeight b)
                    base = case lnShade b of
                      ShadeInk -> alpha 0.5 (pText pal)
                      ShadeThunk -> themeOrange th
                      ShadeSurvivor -> pBlocked pal
                    c = if lnHead b then base else alpha 0.45 base
                    lit = fmap lnRef hoverBar == Just (lnRef b) && isJust (lnRef b)
                drawRoundedRect (Rect lx (ly + 4) (laneW - 6) (lh - 8)) 2 (alpha 0.05 (pText pal))
                drawRoundedRect (Rect lx (ly + 4) bw (lh - 8)) 2 (if lit then pAccent pal else c)
            withClip codeArea $ do
              forM_ toks $ \t -> do
                let tx = colX (tkCol t)
                    tw = fromIntegral (T.length (tkText t)) * cw
                when (tx + tw >= codeX && tx <= x + w && tkKind t /= TkSpace) $ do
                  let isHot = isNameTok t && Just (nameKey (tkText t)) == hot
                  when isHot $ drawRoundedRect (Rect (tx - 1) (ly + 1) (tw + 2) (lh - 2)) 3 (alpha 0.28 (pAccent pal))
                  drawTextWith (mmFont mm) (V2 tx mid) AlignStart AlignMiddle (tkText t) (tokenColor (ccColouring cfg) th pal t)
                  when (fmap snd hovered == Just t && fmap fst hovered == Just l) $
                    drawRect (Rect tx (ly + lh - 2) tw 1) (pAccent pal)
              -- Notes after the code, muted; the one under the pointer is
              -- underlined like a link.
              let (pinned, cols) = hintColumns (rightCol r l) toks (ccHints cfg l)
              case cols of
                (a, _, _) : _ | pinned -> drawRect (Rect (colX a - cw) ly (x + w - colX a + cw) lh) (inkPane (ink th))
                _ -> pure ()
              forM_ cols $ \(a, b, (txt, _)) -> do
                let tx = colX a
                    under = fmap fst hoverHint == Just l && fmap (\(_, (a', _, _)) -> a') hoverHint == Just a
                drawTextWith (mmFont mm) (V2 tx mid) AlignStart AlignMiddle txt (alpha 0.8 (pMuted pal))
                when under $ drawRect (Rect tx (ly + lh - 2) (fromIntegral (b - a) * cw) 1) (pMuted pal)
          when (n == 0) $
            drawTextWith (mmFont mm) (V2 (x + codeLeft + 10) (y + 16)) AlignStart AlignMiddle (ccEmpty cfg) (pMuted pal)
          when (n > count) $ do
            let barH = max 16 (h * fromIntegral count / fromIntegral n)
                barY = y + (h - barH) * (scroll / max 1 maxScroll)
            drawRoundedRect (Rect (x + w - 5) barY 4 barH) 2 (pGrid pal)
  inp <- askInput
  (wx, wy) <- useWheelDeltaOn resp
  let shiftHeld = modShift (inputModifiers inp)
  when (wy /= 0 && not shiftHeld) $ writeVar (cvScroll cv) (max 0 (min maxScroll (scroll + wy * 3)))
  when (wy /= 0 && shiftHeld) $ writeVar (cvHScroll cv) (max 0 (hscroll + wy * 6))
  when (wx /= 0) $ writeVar (cvHScroll cv) (max 0 (hscroll + wx * 6))
  let r = respRect resp
      inGutter = v2X mouse <= rectX r + gutterW
      clickedLine = if respClicked resp then Just (lineAt r (v2Y mouse)) else Nothing
  pure
    CodeEvent
      { ceHoverKey = hoverKey
      , ceClickedKey = if respClicked resp then hoverKey else Nothing
      , ceGutterClick = if inGutter then clickedLine else Nothing
      , ceClickedLine = clickedLine
      , ceHoverLine = hoverLine
      , ceNoteRef = noteRef
      , ceResp = resp
      }

-- | A code view whose bars and notes are links: a click goes to the entity
-- behind one, and right-click and Ctrl+click work on it, or else on what
-- @refOf@ names under the pointer (a binding, a line).
annotatedCode :: Env -> CodeViewState -> MonoMetrics -> CodeConfig -> (CodeEvent -> Maybe Ref) -> NanoUI CodeEvent
annotatedCode env cv mm cfg refOf = do
  ev <- codeView cv mm cfg
  canvasLinks env (ceResp ev) (const (maybe (refOf ev) Just (ceNoteRef ev)))
  ctrl <- modCtrl . inputModifiers <$> askInput
  when (respClicked (ceResp ev) && not ctrl) $ for_ (ceNoteRef ev) (goTo env)
  pure ev

-- ---------------------------------------------------------------------------
-- Colours

-- | What the code panes colour.
data Colouring
  = SyntaxColours
    -- ^ Ordinary syntax highlighting, kept quiet.
  | BoxingColours
    -- ^ Only what the optimiser did: boxing sites in orange, unboxed values
    -- and primops in green, everything else plain.
  deriving stock (Eq, Show, Enum, Bounded)

colouringName :: Colouring -> Text
colouringName = \case
  SyntaxColours -> "Syntax"
  BoxingColours -> "Boxing"

tokenColor :: Colouring -> Theme -> Palette -> Token -> Color
tokenColor colouring th pal t = case colouring of
  SyntaxColours -> case tkKind t of
    TkKeyword -> themeAccent th
    TkCon -> themeYellow th
    TkIdent -> pText pal
    TkOperator -> pMuted pal
    TkNumber -> themeOrange th
    TkString -> themeGreen th
    TkComment -> alpha 0.55 (pMuted pal)
    TkPragma -> pMuted pal
    TkSpace -> pText pal
  BoxingColours
    | boxing -> themeOrange th
    | unboxed -> themeGreen th
    | otherwise -> case tkKind t of
        TkKeyword -> pMuted pal
        TkOperator -> pMuted pal
        TkString -> pMuted pal
        TkComment -> alpha 0.55 (pMuted pal)
        TkPragma -> pMuted pal
        _ -> pText pal
  where
    hashed = "#" `T.isSuffixOf` tkText t
    boxing = tkKind t == TkCon && hashed && isBoxCon (tkText t)
    unboxed = hashed && tkKind t `elem` [TkCon, TkIdent, TkOperator, TkNumber]
    isBoxCon c = any (`T.isSuffixOf` c) ["I#", "D#", "F#", "W#", "C#", "W8#", "W16#", "W32#", "W64#", "I8#", "I16#", "I32#", "I64#"]
