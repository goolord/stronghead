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

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, forkIO, newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TQueue, TVar, atomically, newTQueueIO, newTVarIO, readTQueue, readTVarIO, writeTQueue, writeTVar)
import Control.Exception (SomeException)
import Control.Exception qualified as E
import Control.Monad (forM_, guard, void)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Void (Void)
import System.IO
import System.Process

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import Text.Megaparsec (Parsec, anySingle, between, choice, chunk, eof, many, optional, parse, satisfy, sepBy, sepBy1, takeWhile1P, try)
import Text.Megaparsec.Char (char, hspace)

import Stronghead.Format (tshow)
import Stronghead.Source (SrcSpan, parseSrcSpan)

data Ghci = Ghci
  { ghIn :: !Handle
  , ghProc :: !ProcessHandle
  , ghLines :: !(TQueue (Maybe Text))
  , ghLock :: !(MVar ())
  , ghAlive :: !(TVar Bool)
  }

marker :: Text
marker = "<<stronghead-ghci>>"

-- | Start GHCi with a shell command in a working directory. Returns the
-- session and everything it printed while loading.
startGhci :: FilePath -> String -> IO (Either Text (Ghci, Text))
startGhci cwd' cmd =
  first (tshow @SomeException) <$> E.try do
    (readEnd, writeEnd) <- createPipe
    (Just hin, _, _, ph) <-
      createProcess
        (shell cmd)
          { cwd = if null cwd' then Nothing else Just cwd'
          , std_in = CreatePipe
          , std_out = UseHandle writeEnd
          , std_err = UseHandle writeEnd
          }
    forM_ [hin, readEnd] \h -> do
      hSetEncoding h utf8
      hSetBuffering h LineBuffering
    ghLines <- newTQueueIO
    ghAlive <- newTVarIO True
    void $ forkIO do
      let loop = do
            next <- E.try @SomeException (TIO.hGetLine readEnd)
            case next of
              Left _ -> atomically (writeTVar ghAlive False >> writeTQueue ghLines Nothing)
              Right l -> atomically (writeTQueue ghLines (Just (T.dropWhileEnd (== '\r') l))) >> loop
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

sendLine :: Ghci -> Text -> IO ()
sendLine g l = TIO.hPutStrLn (ghIn g) l >> hFlush (ghIn g)

-- | Lines up to the next sentinel.
collect :: Ghci -> IO Text
collect g = go []
  where
    go acc =
      atomically (readTQueue (ghLines g)) >>= \case
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
ghciCommand g cmd = withMVar (ghLock g) \_ -> do
  alive <- readTVarIO (ghAlive g)
  if not alive
    then pure "<ghci is not running>"
    else do
      r <- E.try @SomeException (sendLine g cmd)
      either (pure . tshow) (const (collect g)) r

ghciAlive :: Ghci -> IO Bool
ghciAlive = readTVarIO . ghAlive

stopGhci :: Ghci -> IO ()
stopGhci g = do
  _ <- E.try @SomeException (sendLine g ":quit")
  _ <- E.try @SomeException (hClose (ghIn g))
  void $ async do
    threadDelay 500000
    void (E.try @SomeException (terminateProcess (ghProc g)))

-- | Render text as a Haskell string literal.
haskellString :: Text -> Text
haskellString = tshow . T.unpack

-- ---------------------------------------------------------------------------
-- Parsing

data StopInfo = StopInfo
  { stName :: !Text
  , stSpan :: !(Maybe SrcSpan)
  }
  deriving stock (Eq, Show)

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
parseHistory = mapMaybe historyLine . T.lines

historyLine :: Text -> Maybe (Int, Text, SrcSpan)
historyLine l = do
  rest <- T.stripPrefix "-" (T.strip l)
  (n, after) <- eitherToMaybe (TR.decimal rest)
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
  deriving stock (Eq, Show)

-- | Join indented continuation lines onto the line they continue.
joinIndented :: [Text] -> [Text]
joinIndented (a : b : rest)
  | Just (c, _) <- T.uncons b, isSpace c = joinIndented ((a <> " " <> T.strip b) : rest)
  | otherwise = a : joinIndented (b : rest)
joinIndented xs = xs

-- | Parse @name :: type = value@ lines (from @:show bindings@ or the
-- bindings GHCi lists when it stops). Indented lines continue the previous
-- binding.
parseBindings :: Text -> [GhciBinding]
parseBindings = mapMaybe one . joinIndented . T.lines
  where
    one l =
      let (name, rest) = T.breakOn " :: " l
          name' = T.strip name
       in if T.null rest || T.any isSpace name' || T.null name' || "Stopped" `T.isPrefixOf` name
            then Nothing
            else
              let afterTy = T.drop 4 rest
                  (ty, val) = T.breakOn " = " afterTy
               in Just
                    GhciBinding
                      { gbName = name'
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
  deriving stock (Eq, Show)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

sym :: Text -> Parser Text
sym = lexeme . chunk

isOpChar :: Char -> Bool
isOpChar x = x `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

-- | Parse a value. Whatever doesn't parse is kept whole as a 'VAtom'.
parseValue :: Text -> Value
parseValue t = fromRight (VAtom (T.strip t)) (parse (hspace *> pExpr <* eof) "" t)

pExpr :: Parser Value
pExpr = do
  l <- pApp
  optional (try pInfixOp) >>= \case
    Nothing -> pure l
    Just op -> infixOf op l <$> pExpr
  where
    infixOf ":" l (VList xs r) = VList (l : xs) r
    infixOf ":" l r = VList [l] (Just r)
    infixOf op l r = VCon op [l, r]
    pInfixOp = do
      op <- lexeme (takeWhile1P Nothing isOpChar)
      guard (op `notElem` ["::", "="])
      pure op

pApp :: Parser Value
pApp = do
  h <- pAtom
  args <- many pAtom
  pure case (h, args) of
    (_, []) -> h
    (VAtom c, _) -> VCon c args
    _ -> VCon (renderValue h) args

pAtom :: Parser Value
pAtom =
  choice
    [ try (VAtom "()" <$ (sym "(" *> sym ")"))
    , try ((\op -> VAtom ("(" <> op <> ")")) <$> between (sym "(") (sym ")") (lexeme (takeWhile1P Nothing isOpChar)))
    , pParens
    , VList <$> between (sym "[") (sym "]") (pExpr `sepBy` sym ",") <*> pure Nothing
    , pQuoted '"'
    , pQuoted '\''
    , pAngleAtom
    , try pNegNumber
    , pWordAtom
    ]

pParens :: Parser Value
pParens = do
  _ <- sym "("
  v <- pExpr
  choice
    [ v <$ sym ")"
    , do
        _ <- sym "::"
        ty <- T.strip <$> pBalancedType
        _ <- sym ")"
        pure case v of
          VThunk n _ -> VThunk n (Just ty)
          _ -> v
    , do
        _ <- sym ","
        vs <- pExpr `sepBy1` sym ","
        _ <- sym ")"
        pure (VTuple (v : vs))
    ]

pBalancedType :: Parser Text
pBalancedType = T.concat <$> many piece
  where
    piece =
      choice
        [ (\inner -> "(" <> inner <> ")") <$> between (char '(') (char ')') pBalancedType
        , (\inner -> "[" <> inner <> "]") <$> between (char '[') (char ']') pBalancedType
        , (\inner -> "{" <> inner <> "}") <$> between (char '{') (char '}') pBalancedType
        , takeWhile1P Nothing (`notElem` ("()[]{}" :: String))
        ]

pQuoted :: Char -> Parser Value
pQuoted q = lexeme $ fmap (VAtom . T.pack) do
  _ <- char q
  body <- many (try ((\a b -> [a, b]) <$> char '\\' <*> anySingle) <|> ((: []) <$> satisfy (/= q)))
  _ <- char q
  pure (q : concat body ++ [q])

-- | "<function>", "<unevaluated>" and the like are one atom.
pAngleAtom :: Parser Value
pAngleAtom = try $ lexeme do
  _ <- char '<'
  w <- takeWhile1P Nothing isAlpha
  _ <- char '>'
  pure (VAtom ("<" <> w <> ">"))

pNegNumber :: Parser Value
pNegNumber = do
  _ <- sym "-"
  w <- pWord
  guard (maybe False (isDigit . fst) (T.uncons w))
  pure (VAtom ("-" <> w))

-- | Names (qualified ones too), and numbers such as @1.0e-2@.
pWord :: Parser Text
pWord = lexeme do
  firstSeg <- seg
  moreSegs <- many (try (T.cons <$> char '.' <*> seg))
  let base = T.concat (firstSeg : moreSegs)
  expSuffix <-
    if maybe False (isDigit . fst) (T.uncons base) && ("e" `T.isSuffixOf` base || "E" `T.isSuffixOf` base)
      then optional (try (T.cons <$> char '-' <*> takeWhile1P Nothing isDigit))
      else pure Nothing
  pure ( maybe base (base <>) expSuffix )
  where
    seg = takeWhile1P Nothing (\x -> isAlphaNum x || x `elem` ("_'#" :: String))

pWordAtom :: Parser Value
pWordAtom = do
  w <- pWord
  if
    | w == "_" || isThunkName w -> pure (VThunk w Nothing)
    | isConName w ->
        optional (between (sym "{") (sym "}") (pField `sepBy` sym ",")) >>= \case
          Just fs -> pure (VRecord w fs)
          Nothing -> pure (VAtom w)
    | otherwise -> pure (VAtom w)
  where
    pField = (,) <$> pWord <* sym "=" <*> pExpr

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
  [ (name', parseValue (T.drop 3 rest))
  | l <- joinIndented (T.lines out)
  , let (name, rest) = T.breakOn " = " l
        name' = T.strip name
  , not (T.null rest)
  , not (T.null name')
  , not (T.any isSpace name')
  ]

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
