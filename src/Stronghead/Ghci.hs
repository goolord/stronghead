-- | Driving a GHCi (or @cabal repl@) subprocess for the breakpoint stepper.
--
-- GHCi's prompt is replaced with a sentinel line, so each command's output
-- is everything printed before the next sentinel. stdout and stderr share
-- one pipe so compiler errors interleave with the output that caused them.
-- The rest of the module parses the bits of GHCi's output the GUI acts on:
-- "Stopped in" locations, breakpoint numbers and @:show bindings@.
module Stronghead.Ghci
  ( Ghci
  , startGhci
  , ghciCommand
  , stopGhci
  , ghciAlive
    -- * Output parsing
  , SrcSpan (..)
  , parseSrcSpan
  , StopInfo (..)
  , parseStop
  , parseBreakpointSet
  , GhciBinding (..)
  , parseBindings
  , valueIsThunk
  , candidateLines
  , haskellString
  ) where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.Char (isDigit, isSpace)
import Data.IORef
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
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

data SrcSpan = SrcSpan
  { ssFile :: !Text
  , ssLine :: !Int
  , ssCol :: !Int
  , ssEndLine :: !Int
  , ssEndCol :: !Int
  }
  deriving (Eq, Show)

-- | @file:12:3-20@, @file:12:3@, or @file:(12,3)-(14,5)@.
parseSrcSpan :: Text -> Maybe SrcSpan
parseSrcSpan t0 =
  let t = T.strip t0
   in case T.breakOnEnd ":(" t of
        (pre, rest)
          | not (T.null pre) -> do
              let file = T.dropEnd 2 pre
              (l1, r1) <- num rest
              (c1, r2) <- num (T.drop 1 r1)
              let r3 = T.drop 3 r2 -- ")-("
              (l2, r4) <- num r3
              (c2, _) <- num (T.drop 1 r4)
              pure (SrcSpan file l1 c1 l2 c2)
        _ -> simple t
  where
    num s = either (const Nothing) Just (TR.decimal s)
    simple t =
      -- Split from the right: the file may contain ':' (C:\...).
      let parts = T.splitOn ":" t
       in case reverse parts of
            colPart : linePart : fileParts@(_ : _) -> do
              (l, _) <- num linePart
              (c1, rest) <- num colPart
              let c2 = case T.stripPrefix "-" rest of
                    Just more -> maybe c1 fst (num more)
                    Nothing -> c1
              pure (SrcSpan (T.intercalate ":" (reverse fileParts)) l c1 l c2)
            _ -> Nothing

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
