-- | Which run of which program an artifact came from, and its clock.
--
-- Each source knows part of the story: the eventlog has the pid, the full
-- command line, the RTS version and a wall-clock anchor; the agent's hello
-- has the program and pid; a ghc-debug socket is named @<pid>-<prog>@; a
-- @.hp@ file has its command line and start date; a @.prof@ file has both
-- in its header. 'matchRuns' decides whether two of them describe the same
-- run, which is what lets time-keyed links cross between them.
--
-- Program time is nanoseconds since the RTS started, as eventlog
-- timestamps count it.
module Stronghead.Run
  ( RunInfo (..)
  , emptyRunInfo
  , mergeRunInfo
  , WallAnchor (..)
  , runStartWall
  , programToWall
  , wallToProgram
  , RunKey (..)
  , runKey
  , runKeysMatch
    -- * From each source
  , programName
  , splitRtsArgs
  , runFromArgs
  , runFromHello
  , runFromSocket
  , runFromHp
  , runFromProfHeader
    -- * Matching
  , RunMatch (..)
  , matchRuns
  , groupRuns
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)

import Data.Text (Text)
import Data.Text qualified as T

import Stronghead.Format (readDecimal, tshow)
import Stronghead.Source (shortFileName)

data RunInfo = RunInfo
  { riProgram :: !(Maybe Text)
    -- ^ Base name, without directory or @.exe@.
  , riPid :: !(Maybe Int)
  , riArgs :: ![Text]
    -- ^ The program's own arguments.
  , riRtsFlags :: ![Text]
    -- ^ RTS options given on the command line (@+RTS ... -RTS@).
  , riRts :: !(Maybe Text)
    -- ^ The RTS identifier, e.g. @GHC-9.14.1 rts_thr@.
  , riWallClock :: !(Maybe WallAnchor)
  , riStarted :: !(Maybe Text)
    -- ^ The start date a @.hp@ or @.prof@ file prints: local time, to the
    -- minute (@Mon Oct 19 21:49 2026@).
  }
  deriving stock (Eq, Show)

instance Semigroup RunInfo where
  a <> b =
    RunInfo
      { riProgram = riProgram a <|> riProgram b
      , riPid = riPid a <|> riPid b
      , riArgs = if null (riArgs a) then riArgs b else riArgs a
      , riRtsFlags = if null (riRtsFlags a) then riRtsFlags b else riRtsFlags a
      , riRts = riRts a <|> riRts b
      , riWallClock = riWallClock a <|> riWallClock b
      , riStarted = riStarted a <|> riStarted b
      }

instance Monoid RunInfo where
  mempty = RunInfo Nothing Nothing [] [] Nothing Nothing Nothing

emptyRunInfo :: RunInfo
emptyRunInfo = mempty

-- | Combine what two sources know about one run, preferring the first.
mergeRunInfo :: RunInfo -> RunInfo -> RunInfo
mergeRunInfo = (<>)

-- | A moment both clocks saw: the eventlog's @WallClockTime@ event, posted
-- at program time 'waProgramNs' with the Unix time 'waUnixNs'.
data WallAnchor = WallAnchor
  { waProgramNs :: !Word64
  , waUnixNs :: !Word64
  }
  deriving stock (Eq, Show)

-- | Unix time, in nanoseconds, at program time zero.
runStartWall :: RunInfo -> Maybe Word64
runStartWall ri = (\(WallAnchor p u) -> u - min u p) <$> riWallClock ri

programToWall :: WallAnchor -> Word64 -> Word64
programToWall (WallAnchor p u) t = u - min u p + t

-- | 'Nothing' for a Unix time before the program started.
wallToProgram :: WallAnchor -> Word64 -> Maybe Word64
wallToProgram (WallAnchor p u) w
  | w + p >= u = Just (w + p - u)
  | otherwise = Nothing

-- | How time-keyed references name a run: @program:pid@, with @?@ for
-- whichever part is unknown.
newtype RunKey = RunKey Text
  deriving stock (Eq, Ord, Show)

runKey :: RunInfo -> RunKey
runKey ri = RunKey (fromMaybe "?" (riProgram ri) <> ":" <> maybe "?" tshow (riPid ri))

-- | Whether time-keyed references from two runs may be joined: the same
-- key, the same program where one side does not know the pid, or a side
-- that knows nothing at all (a ghc-debug target reached over TCP, say).
-- The last two are guesses, and whoever shows the join should say so.
runKeysMatch :: RunKey -> RunKey -> Bool
runKeysMatch a b
  | a == b = True
  | unknown a || unknown b = True
  | otherwise =
      let (pa, ia) = parts a
          (pb, ib) = parts b
       in pa /= "?" && pa == pb && (ia == "?" || ib == "?")
  where
    parts (RunKey k) = first (T.dropEnd 1) (T.breakOnEnd ":" k)
    unknown k = parts k == ("?", "?")

-- ---------------------------------------------------------------------------
-- Sources

-- | A program name as the different sources print it, reduced to one form:
-- @C:\\bin\\prog.exe@, @./prog@ and @prog@ are all @prog@.
programName :: Text -> Maybe Text
programName t =
  let base = shortFileName (T.strip t)
      stripped = if ".exe" `T.isSuffixOf` T.toLower base then T.dropEnd 4 base else base
   in if T.null stripped then Nothing else Just stripped

-- | Separate a command line's RTS options from the program's arguments.
-- @+RTS@ starts RTS options, @-RTS@ ends them, and after @--RTS@ everything
-- belongs to the program.
splitRtsArgs :: [Text] -> ([Text], [Text])
splitRtsArgs = go False
  where
    go _ [] = ([], [])
    go _ ("--RTS" : rest) = (rest, [])
    go _ ("+RTS" : rest) = go True rest
    go _ ("-RTS" : rest) = go False rest
    go inRts (a : rest) =
      (if inRts then second (a :) else first (a :)) (go inRts rest)

-- | From a full command line, program first: the eventlog's
-- @ProgramArgs@, or a @.hp@ file's @JOB@.
runFromArgs :: [Text] -> RunInfo
runFromArgs = \case
  [] -> emptyRunInfo
  prog : rest ->
    let (args, rts) = splitRtsArgs rest
     in emptyRunInfo {riProgram = programName prog, riArgs = args, riRtsFlags = rts}

-- | From the agent's hello.
runFromHello :: Text -> Int -> RunInfo
runFromHello prog pid = emptyRunInfo {riProgram = programName prog, riPid = Just pid}

-- | From a ghc-debug socket path, named @<pid>-<prog>@.
runFromSocket :: Text -> Maybe RunInfo
runFromSocket path = do
  let name = shortFileName path
      (pidTxt, rest) = T.span isDigit name
  prog <- T.stripPrefix "-" rest
  pid <- readDecimal pidTxt
  pure emptyRunInfo {riProgram = programName prog, riPid = Just pid}

-- | From a @.hp@ file's @JOB@ and @DATE@ values (without their quotes).
runFromHp :: Text -> Text -> RunInfo
runFromHp job date =
  let d = T.strip date
   in (runFromArgs (T.words job)) {riStarted = if T.null d then Nothing else Just d}

-- | From a @.prof@ file's header lines (blank lines removed): the date
-- before "Time and Allocation Profiling Report", then the command line.
runFromProfHeader :: [Text] -> RunInfo
runFromProfHeader ls = case break ("Profiling Report" `T.isInfixOf`) ls of
  (_, title : rest) ->
    let date = T.strip (fst (T.breakOn "Time and Allocation" title))
        cmd = case rest of
          l : _ | not ("total " `T.isPrefixOf` T.strip l) -> runFromArgs (T.words l)
          _ -> emptyRunInfo
     in cmd {riStarted = if T.null date then Nothing else Just date}
  _ -> emptyRunInfo

-- ---------------------------------------------------------------------------
-- Matching

data RunMatch
  = SameRun
    -- ^ Same program and pid: time links are exact.
  | ProbablySameRun
    -- ^ Same program, but one side has no pid: time links are on, marked
    -- approximate, and the match can be undone.
  | DifferentRuns
    -- ^ Joined only by code.
  deriving stock (Eq, Ord, Show)

-- | Two sources that both print a start date (a @.hp@ and a @.prof@) and
-- disagree are different runs: one run stamps both with the same minute.
matchRuns :: RunInfo -> RunInfo -> RunMatch
matchRuns a b = case (riProgram a, riProgram b) of
  (Just p, Just q)
    | p /= q -> DifferentRuns
    | otherwise -> case (riPid a, riPid b) of
        (Just x, Just y) -> if x == y then SameRun else DifferentRuns
        _
          | Just da <- riStarted a, Just db <- riStarted b, da /= db -> DifferentRuns
          | otherwise -> ProbablySameRun
  _ -> DifferentRuns

-- | Put sources into runs: each source joins the first run whose first
-- source it matches, else starts its own. Each is given by its index,
-- with how it matched the run's first ('SameRun' for that one itself).
groupRuns :: [RunInfo] -> [[(Int, RunMatch)]]
groupRuns ris = map snd (foldl add [] (zip [0 ..] ris))
  where
    -- Each group keeps its first source's run, and its members in order.
    add groups (i, ri) = case break (\(first, _) -> matchRuns first ri /= DifferentRuns) groups of
      (before, (first, members) : after) -> before ++ [(first, members ++ [(i, matchRuns first ri)])] ++ after
      _ -> groups ++ [(ri, [(i, SameRun)])]
