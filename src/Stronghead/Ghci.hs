-- | Driving a GHCi (or @cabal repl@) subprocess for the breakpoint stepper.
--
-- GHCi's prompt is replaced with a sentinel line, so each command's output
-- is everything printed before the next sentinel. stdout and stderr share
-- one pipe so compiler errors interleave with the output that caused them.
-- The rest of the module parses the bits of GHCi's output the GUI acts on:
-- "Stopped in" locations, breakpoint numbers, @:show bindings@, and the
-- values @:print@ shows, as trees whose unevaluated parts are named @_t@
-- thunks that can be forced.
module Stronghead.Ghci
  ( Ghci
  , startGhci
  , ghciCommand
  , stopGhci
  , ghciAlive
    -- * Output parsing
  , StopInfo (..)
  , parseStop
  , parseBreakpointSet
  , parseHistory
  , historyLine
  , GhciBinding (..)
  , parseBindings
  , valueIsThunk
    -- * Values
  , Value (..)
  , parseValue
  , parsePrint
  , renderValue
  , valueThunks
  , valueHasThunks
  , candidateLines
  , haskellString
  ) where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import Data.IORef
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import Stronghead.Source (SrcSpan, parseSrcSpan)
import System.IO
import System.Process

data Ghci = Ghci
  { ghIn :: !Handle
  , ghProc :: !ProcessHandle
  , ghLines :: !(Chan (Maybe Text))
  , ghLock :: !(MVar ())
  , ghAlive :: !(IORef Bool)
  }

marker :: Text
marker = "<<stronghead-ghci>>"

-- | Start GHCi with a shell command in a working directory. Returns the
-- session and everything it printed while loading.
startGhci :: FilePath -> String -> IO (Either Text (Ghci, Text))
startGhci cwd' cmd = do
  r <- try $ do
    (readEnd, writeEnd) <- createPipe
    (Just hin, _, _, ph) <-
      createProcess
        (shell cmd)
          { cwd = if null cwd' then Nothing else Just cwd'
          , std_in = CreatePipe
          , std_out = UseHandle writeEnd
          , std_err = UseHandle writeEnd
          }
    forM_ [hin, readEnd] $ \h -> do
      hSetEncoding h utf8
      hSetBuffering h LineBuffering
    ghLines <- newChan
    ghAlive <- newIORef True
    void . forkIO $ do
      let loop = do
            next <- try (TIO.hGetLine readEnd)
            case next of
              Left (_ :: SomeException) -> writeIORef ghAlive False >> writeChan ghLines Nothing
              Right l -> writeChan ghLines (Just (T.dropWhileEnd (== '\r') l)) >> loop
      loop
    ghLock <- newMVar ()
    let ghIn = hin
        ghProc = ph
        g = Ghci {..}
    -- The default prompt has no newline, so it prefixes the first sentinel.
    sendLine g (":set prompt " <> haskellString (marker <> "\n"))
    startup <- collect g
    _ <- ghciCommand g (":set prompt-cont " <> haskellString "")
    pure (g, startup)
  pure $ case r of
    Left (e :: SomeException) -> Left (T.pack (show e))
    Right ok -> Right ok

sendLine :: Ghci -> Text -> IO ()
sendLine g l = TIO.hPutStrLn (ghIn g) l >> hFlush (ghIn g)

-- | Lines up to the next sentinel.
collect :: Ghci -> IO Text
collect g = go []
  where
    go acc =
      readChan (ghLines g) >>= \case
        Nothing -> pure (T.unlines (reverse ("<ghci exited>" : acc)))
        Just l
          | marker `T.isSuffixOf` l ->
              let before = T.strip (T.dropEnd (T.length marker) l)
                  -- While stopped, GHCi puts the location before even a
                  -- custom prompt: "[Main.hs:7:23-37] ".
                  isContext = "[" `T.isPrefixOf` before && "]" `T.isSuffixOf` before
                  extra = if T.null before || isContext || "ghci>" `T.isSuffixOf` before || ">" `T.isSuffixOf` before then [] else [before]
               in pure (T.intercalate "\n" (reverse (extra ++ acc)))
          | otherwise -> go (l : acc)

-- | Run one command and return its output. Commands are serialised.
ghciCommand :: Ghci -> Text -> IO Text
ghciCommand g cmd = withMVar (ghLock g) $ \_ -> do
  alive <- readIORef (ghAlive g)
  if not alive
    then pure "<ghci is not running>"
    else do
      r <- try (sendLine g cmd)
      case r of
        Left (e :: SomeException) -> pure (T.pack (show e))
        Right () -> collect g

ghciAlive :: Ghci -> IO Bool
ghciAlive = readIORef . ghAlive

stopGhci :: Ghci -> IO ()
stopGhci g = do
  _ <- try @SomeException (sendLine g ":quit")
  _ <- try @SomeException (hClose (ghIn g))
  _ <- forkIO $ do
    threadDelay 500000
    void (try @SomeException (terminateProcess (ghProc g)))
  pure ()

-- | Render text as a Haskell string literal.
haskellString :: Text -> Text
haskellString = T.pack . show . T.unpack

-- ---------------------------------------------------------------------------
-- Parsing

data StopInfo = StopInfo
  { stName :: !Text
  , stSpan :: !(Maybe SrcSpan)
  }
  deriving (Eq, Show)

-- | The last "Stopped in NAME, LOC" in some output.
parseStop :: Text -> Maybe StopInfo
parseStop out =
  listToMaybe . reverse $
    [ StopInfo (T.strip name) (parseSrcSpan (T.drop 2 loc))
    | l <- T.lines out
    , Just rest <- [T.stripPrefix "Stopped in " (T.strip l)]
    , let (name, loc) = T.breakOn ", " rest
    ]
      ++ [ StopInfo "<exception>" Nothing
         | l <- T.lines out
         , "Stopped at <exception thrown>" `T.isInfixOf` l
         ]

-- | One step of @:history@: @-1  : go (Main.hs:5:8-21)@ is step -1, in
-- @go@, at that span.
parseHistory :: Text -> [(Int, Text, SrcSpan)]
parseHistory out = mapMaybe historyLine (T.lines out)

historyLine :: Text -> Maybe (Int, Text, SrcSpan)
historyLine l = do
  rest <- T.stripPrefix "-" (T.strip l)
  (n, after) <- either (const Nothing) Just (TR.decimal rest)
  body <- T.stripPrefix ":" (T.stripStart after)
  let (name, loc) = T.breakOn " (" (T.strip body)
  sp <- parseSrcSpan =<< T.stripSuffix ")" (T.drop 2 loc)
  pure (negate n, T.strip name, sp)

-- | "Breakpoint 3 activated at file:12:3-20" lines.
parseBreakpointSet :: Text -> [(Int, Maybe SrcSpan)]
parseBreakpointSet out =
  [ (n, parseSrcSpan loc)
  | l <- T.lines out
  , Just rest <- [T.stripPrefix "Breakpoint " (T.strip l)]
  , Right (n, after) <- [TR.decimal rest]
  , let loc = snd (T.breakOnEnd " at " after)
  ]

data GhciBinding = GhciBinding
  { gbName :: !Text
  , gbType :: !Text
  , gbValue :: !(Maybe Text)
    -- ^ 'Nothing' when GHCi printed no value at all.
  }
  deriving (Eq, Show)

-- | Parse @name :: type = value@ lines (from @:show bindings@ or the
-- bindings GHCi lists when it stops). Indented lines continue the previous
-- binding.
parseBindings :: Text -> [GhciBinding]
parseBindings out = mapMaybe one (joinContinuations (T.lines out))
  where
    joinContinuations (a : b : rest)
      | not (T.null b) && isSpace (T.head b) = joinContinuations ((a <> " " <> T.strip b) : rest)
      | otherwise = a : joinContinuations (b : rest)
    joinContinuations xs = xs
    one l =
      let (name, rest) = T.breakOn " :: " l
       in if T.null rest || T.any isSpace (T.strip name) || T.null (T.strip name) || "Stopped" `T.isPrefixOf` name
            then Nothing
            else
              let afterTy = T.drop 4 rest
                  (ty, val) = T.breakOn " = " afterTy
               in Just
                    GhciBinding
                      { gbName = T.strip name
                      , gbType = T.strip ty
                      , gbValue = if T.null val then Nothing else Just (T.strip (T.drop 3 val))
                      }

-- | Whether GHCi showed the value as (partly) unevaluated.
valueIsThunk :: Maybe Text -> Bool
valueIsThunk = \case
  Nothing -> True
  Just v -> v == "_" || "_t" `T.isPrefixOf` v || v == "<unevaluated>"

-- ---------------------------------------------------------------------------
-- Values

-- | A value as GHCi prints it (@:print@, @:sprint@, @:show bindings@), with
-- what is still unevaluated marked.
data Value
  = VThunk !Text !(Maybe Text)
    -- ^ An unevaluated part and its type: @(_t3::Int)@ from @:print@, which
    -- binds @_t3@ so it can be forced, or a bare @_@ (named @_@), which
    -- can't be.
  | VCon !Text ![Value]
    -- ^ A constructor and its fields, prefix or infix (@1 :| [2]@).
  | VRecord !Text ![(Text, Value)]
  | VList ![Value] !(Maybe Value)
    -- ^ Elements, and the rest when it is not @[]@: @1 : 2 : (_t1::[Int])@
    -- is two elements and a thunk.
  | VTuple ![Value]
  | VAtom !Text
    -- ^ A literal, a variable, @<function>@, or text that didn't parse.
  deriving (Eq, Show)

data VTok = VOpen !Char | VClose !Char | VComma | VWord !Text | VOp !Text
  deriving (Eq, Show)

-- | Tokens with their start and end offsets, for slicing types out.
lexValue :: Text -> [(Int, Int, VTok)]
lexValue t0 = go 0 (T.unpack t0)
  where
    go _ [] = []
    go i s@(c : cs)
      | isSpace c = go (i + 1) cs
      | c `elem` ("([{" :: String) = (i, i + 1, VOpen c) : go (i + 1) cs
      | c `elem` (")]}" :: String) = (i, i + 1, VClose c) : go (i + 1) cs
      | c == ',' = (i, i + 1, VComma) : go (i + 1) cs
      | c == '"' || c == '\'' = emit (c : quoted c cs)
      -- "<function>", "<unevaluated>" and the like are one atom.
      | c == '<', (w, '>' : _) <- span isAlpha cs, not (null w) = emit ('<' : w ++ ">")
      | isAlphaNum c || c == '_' = emit (word s)
      | isOpChar c = (i, i + length op, VOp (T.pack op)) : go (i + length op) (drop (length op) s)
      | otherwise = emit [c]
      where
        op = takeWhile isOpChar s
        emit tok = (i, i + length tok, VWord (T.pack tok)) : go (i + length tok) (drop (length tok) s)
    quoted q = loop
      where
        loop ('\\' : x : r) = '\\' : x : loop r
        loop (x : r)
          | x == q = [x]
          | otherwise = x : loop r
        loop [] = []
    -- Names (qualified ones too), and numbers such as 1.0e-2.
    word s =
      let w = takeWhile (\x -> isAlphaNum x || x `elem` ("_'#." :: String)) s
          w' = reverse (dropWhile (== '.') (reverse w))
          rest = drop (length w') s
       in case (w', rest) of
            (d : _, '-' : e : _) | isDigit d, "e" `isSuffixOfS` w', isDigit e -> w' ++ "-" ++ takeWhile isDigit (drop 1 rest)
            _ -> w'
    isSuffixOfS suf x = reverse suf == take (length suf) (reverse x)
    isOpChar x = x `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

-- | Parse a value. Whatever doesn't parse is kept whole as a 'VAtom'.
parseValue :: Text -> Value
parseValue t = case pExpr t (lexValue t) of
  Just (v, []) -> v
  _ -> VAtom (T.strip t)

type VToks = [(Int, Int, VTok)]

pExpr :: Text -> VToks -> Maybe (Value, VToks)
pExpr src ts = do
  (l, rest) <- pApp src ts
  case rest of
    (_, _, VOp op) : rest'
      | op `notElem` ["::", "="] -> do
          (r, rest'') <- pExpr src rest'
          pure (infixOf op l r, rest'')
    _ -> pure (l, rest)
  where
    infixOf ":" l r = case r of
      VList xs t -> VList (l : xs) t
      _ -> VList [l] (Just r)
    infixOf op l r = VCon op [l, r]

pApp :: Text -> VToks -> Maybe (Value, VToks)
pApp src ts = do
  (h, rest) <- pAtom src ts
  let (args, rest') = many' rest
  pure $ case (h, args) of
    (_, []) -> (h, rest')
    (VAtom c, _) -> (VCon c args, rest')
    _ -> (VCon (renderValue h) args, rest')
  where
    many' r = case pAtom src r of
      Just (v, r') -> let (vs, r'') = many' r' in (v : vs, r'')
      Nothing -> ([], r)

pAtom :: Text -> VToks -> Maybe (Value, VToks)
pAtom src = \case
  (_, _, VOpen '(') : (_, _, VClose ')') : rest -> Just (VAtom "()", rest)
  (_, _, VOpen '(') : (_, _, VOp op) : (_, _, VClose ')') : rest -> Just (VAtom ("(" <> op <> ")"), rest)
  (_, _, VOpen '(') : rest -> do
    (v, rest1) <- pExpr src rest
    case rest1 of
      (_, _, VClose ')') : r -> Just (v, r)
      (_, e, VOp "::") : r -> do
        (s, r') <- closing (0 :: Int) r
        let ty = T.strip (T.take (s - e) (T.drop e src))
        Just (case v of VThunk n _ -> VThunk n (Just ty); _ -> v, r')
      (_, _, VComma) : r -> do
        (vs, r') <- commaList ')' r
        Just (VTuple (v : vs), r')
      _ -> Nothing
  (_, _, VOpen '[') : (_, _, VClose ']') : rest -> Just (VList [] Nothing, rest)
  (_, _, VOpen '[') : rest -> do
    (vs, r) <- commaList ']' rest
    Just (VList vs Nothing, r)
  (_, _, VWord w) : rest
    | w == "_" || isThunkName w -> Just (VThunk w Nothing, rest)
    | isConName w, (_, _, VOpen '{') : r <- rest -> do
        (fs, r') <- fields r
        Just (VRecord w fs, r')
    | otherwise -> Just (VAtom w, rest)
  (_, _, VOp "-") : (_, _, VWord n) : rest | Just (d, _) <- T.uncons n, isDigit d -> Just (VAtom ("-" <> n), rest)
  _ -> Nothing
  where
    commaList close ts = do
      (v, r) <- pExpr src ts
      case r of
        (_, _, VComma) : r' -> do
          (vs, r'') <- commaList close r'
          Just (v : vs, r'')
        (_, _, VClose c) : r' | c == close -> Just ([v], r')
        _ -> Nothing
    fields = \case
      (_, _, VClose '}') : r -> Just ([], r)
      (_, _, VWord f) : (_, _, VOp "=") : r -> do
        (v, r') <- pExpr src r
        case r' of
          (_, _, VComma) : r'' -> do
            (fs, r''') <- fields r''
            Just ((f, v) : fs, r''')
          (_, _, VClose '}') : r'' -> Just ([(f, v)], r'')
          _ -> Nothing
      _ -> Nothing
    -- The offset of the bracket closing a type annotation, and what follows.
    closing d = \case
      (s, _, VClose ')') : r | d == 0 -> Just (s, r)
      (_, _, VOpen _) : r -> closing (d + 1) r
      (_, _, VClose _) : r -> closing (d - 1) r
      _ : r -> closing d r
      [] -> Nothing

isThunkName :: Text -> Bool
isThunkName w = case T.stripPrefix "_t" w of
  Just n -> not (T.null n) && T.all isDigit n
  Nothing -> False

isConName :: Text -> Bool
isConName w = case T.uncons (snd (T.breakOnEnd "." w)) of
  Just (c, _) -> isUpper c
  Nothing -> False

-- | @name = value@ lines, as @:print@ and @:force@ write them. Indented
-- lines continue the one before.
parsePrint :: Text -> [(Text, Value)]
parsePrint out =
  [ (T.strip name, parseValue (T.drop 3 rest))
  | l <- joinLines (T.lines out)
  , let (name, rest) = T.breakOn " = " l
  , not (T.null rest)
  , not (T.null (T.strip name))
  , not (T.any isSpace (T.strip name))
  ]
  where
    joinLines (a : b : rest)
      | not (T.null b) && isSpace (T.head b) = joinLines ((a <> " " <> T.strip b) : rest)
      | otherwise = a : joinLines (b : rest)
    joinLines xs = xs

-- | A value in Haskell syntax, thunks by name.
renderValue :: Value -> Text
renderValue = \case
  VThunk n _ -> n
  VCon c [] -> c
  VCon c [l, r] | isOperator c -> wrap l <> " " <> c <> " " <> wrap r
  VCon c fs -> c <> " " <> T.unwords (map wrap fs)
  VRecord c fs -> c <> " {" <> T.intercalate ", " [f <> " = " <> renderValue v | (f, v) <- fs] <> "}"
  VList xs Nothing -> "[" <> T.intercalate ", " (map renderValue xs) <> "]"
  VList xs (Just t) -> T.intercalate " : " (map wrap (xs ++ [t]))
  VTuple xs -> "(" <> T.intercalate ", " (map renderValue xs) <> ")"
  VAtom a -> a
  where
    isOperator c = maybe False (\(h, _) -> not (isAlphaNum h) && h /= '(' && h /= '_') (T.uncons c)
    wrap v =
      let r = renderValue v
       in case v of
            VCon _ (_ : _) -> "(" <> r <> ")"
            VList _ (Just _) -> "(" <> r <> ")"
            VAtom a | "-" `T.isPrefixOf` a -> "(" <> r <> ")"
            _ -> r

-- | The named thunks in a value, which GHCi can force.
valueThunks :: Value -> [Text]
valueThunks = \case
  VThunk n _ -> [n | isThunkName n]
  VCon _ fs -> concatMap valueThunks fs
  VRecord _ fs -> concatMap (valueThunks . snd) fs
  VList xs t -> concatMap valueThunks (xs ++ maybe [] pure t)
  VTuple xs -> concatMap valueThunks xs
  VAtom _ -> []

-- | Whether any part of a value is unevaluated, named or not.
valueHasThunks :: Value -> Bool
valueHasThunks = \case
  VThunk _ _ -> True
  VCon _ fs -> any valueHasThunks fs
  VRecord _ fs -> any (valueHasThunks . snd) fs
  VList xs t -> any valueHasThunks (xs ++ maybe [] pure t)
  VTuple xs -> any valueHasThunks xs
  VAtom _ -> False

-- | Source lines that plausibly hold a breakpoint site: expressions, not
-- blank lines, comments, imports, pragmas or bare type signatures.
candidateLines :: [Text] -> [Int]
candidateLines ls = go False (zip [1 ..] ls)
  where
    go _ [] = []
    go inComment ((n, l) : rest)
      | inComment = go (not ("-}" `T.isInfixOf` l)) rest
      | "{-" `T.isPrefixOf` s && not ("-}" `T.isInfixOf` s) = go True rest
      | T.null s
          || "--" `T.isPrefixOf` s
          || "{-" `T.isPrefixOf` s
          || any (`T.isPrefixOf` s) ["import ", "module ", "data ", "newtype ", "type ", "class ", "instance ", "deriving ", "where", "infix"]
          || isSignature s =
          go False rest
      | otherwise = n : go False rest
      where
        s = T.strip l
    isSignature s =
      let (lhs, rhs) = T.breakOn " :: " s
       in not (T.null rhs) && not ("=" `T.isInfixOf` lhs) && T.all (\c -> not (isSpace c) || c == ' ') lhs && not (T.any isDigit (T.take 1 lhs)) && not (" = " `T.isInfixOf` rhs)
