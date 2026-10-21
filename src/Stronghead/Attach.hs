-- | One Connect wires a running program: from a v2 agent's info line, work
-- out where the program's eventlog, heap profile, ghc-debug socket, sources
-- and dumps are, and check which of them are there.
--
-- The paths follow the RTS's own rules. The eventlog is @-ol<file>@ when
-- given, else @<stem>.eventlog@ for @-po<stem>@, else @<program>.eventlog@,
-- in the program's working directory; the heap profile is @<stem>.hp@ or
-- @<program>.hp@ the same way. A ghc-debug socket is named
-- @<pid>-<program>@.
module Stronghead.Attach
  ( AttachKind (..)
  , attachKindName
  , AttachStep (..)
  , rtsOption
  , eventlogPathFor
  , hpPathFor
  , socketFor
  , probeAttach
  ) where

import Control.Exception (SomeException, try)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (isAbsolute, (<.>), (</>))

import Data.Text (Text)
import Data.Text qualified as T

import Stronghead.Dump (discoverArtifacts)
import Stronghead.Format (tshow)
import Stronghead.Live (AgentInfo (..))
import Stronghead.Run (RunInfo (..), programName, runFromSocket, splitRtsArgs)

data AttachKind = AttachEventlog | AttachHeapProfile | AttachGhcDebug | AttachSources | AttachDumps
  deriving stock (Eq, Ord, Show, Enum, Bounded)

attachKindName :: AttachKind -> Text
attachKindName = \case
  AttachEventlog -> "Eventlog"
  AttachHeapProfile -> "Heap profile"
  AttachGhcDebug -> "ghc-debug"
  AttachSources -> "Sources"
  AttachDumps -> "Core & STG dumps"

-- | One thing Connect looked for: where, whether it was there, and what
-- that means, in words.
data AttachStep = AttachStep
  { asKind :: !AttachKind
  , asPath :: !(Maybe FilePath)
    -- ^ Set only when it was found.
  , asNote :: !Text
  }
  deriving stock (Eq, Show)

-- | The value of an RTS option given as @-<flag><value>@ on the command
-- line (the last one wins, as in the RTS).
rtsOption :: Text -> [Text] -> Maybe Text
rtsOption flag args =
  let (_, rts) = splitRtsArgs (drop 1 args)
   in listToMaybe (reverse [v | a <- rts, Just v <- [T.stripPrefix flag a], not (T.null v)])

-- | Where the program writes its eventlog, if it writes one.
eventlogPathFor :: Text -> AgentInfo -> Maybe FilePath
eventlogPathFor prog info
  | not (aiEventlog info) = Nothing
  | Just f <- rtsOption "-ol" (aiArgs info) = Just (inCwd info (T.unpack f))
  | otherwise = Just (inCwd info (stem prog info <.> "eventlog"))

-- | Where the program writes its heap profile, if it takes one.
hpPathFor :: Text -> AgentInfo -> Maybe FilePath
hpPathFor prog info = inCwd info (stem prog info <.> "hp") <$ aiHeapProfile info

stem :: Text -> AgentInfo -> FilePath
stem prog info = T.unpack (fromMaybe prog (rtsOption "-po" (aiArgs info)))

inCwd :: AgentInfo -> FilePath -> FilePath
inCwd info f
  | isAbsolute f || T.null (aiCwd info) = f
  | otherwise = T.unpack (aiCwd info) </> f

-- | The ghc-debug socket of a program and pid, among those listed.
socketFor :: Text -> Int -> [FilePath] -> Maybe FilePath
socketFor prog pid socks =
  listToMaybe
    [ s
    | s <- socks
    , Just ri <- [runFromSocket (T.pack s)]
    , riPid ri == Just pid
    , maybe True (== prog) (riProgram ri)
    ]

-- | Look for each piece of the program: @prog@ and @pid@ from the hello,
-- the info line, the ghc-debug sockets there are (empty when Stronghead was
-- built without ghc-debug, which @debugBuilt@ says), and the steps found.
probeAttach :: Text -> Int -> AgentInfo -> Bool -> IO [FilePath] -> IO [AttachStep]
probeAttach hello pid info debugBuilt sockets = do
  -- The RTS names its files after the program without @.exe@.
  let prog = fromMaybe hello (programName hello)
      cwd = T.unpack (aiCwd info)
      file kind found missing = \case
        Nothing -> pure (AttachStep kind Nothing missing)
        Just p -> do
          there <- doesFileExist p
          pure $
            if there
              then AttachStep kind (Just p) (found p)
              else AttachStep kind Nothing ("Skipped: " <> T.pack p <> " isn't there" <> remoteNote)
      remoteNote = " (is the program on another machine?)"
  cwdThere <- if null cwd then pure False else doesDirectoryExist cwd
  eventlog <-
    file
      AttachEventlog
      (\p -> "Following " <> T.pack p <> " in the Timeline")
      "Skipped: the program isn't writing an eventlog. Run it with +RTS -l"
      (eventlogPathFor prog info)
  hp <-
    file
      AttachHeapProfile
      (\p -> "Following " <> T.pack p <> " in Heap & leaks")
      "Skipped: the program takes no heap profile. Run it with +RTS -hT -i0.1 (or -hi for source locations)"
      (hpPathFor prog info)
  debug <-
    if not debugBuilt
      then pure (AttachStep AttachGhcDebug Nothing "Skipped: this build of Stronghead has no ghc-debug support (cabal build -f ghc-debug)")
      else do
        socks <- fromRight [] <$> try @SomeException sockets
        pure case socketFor prog pid socks of
          Just s -> AttachStep AttachGhcDebug (Just s) ("Snapshots go to " <> T.pack s)
          Nothing -> AttachStep AttachGhcDebug Nothing ("Skipped: no ghc-debug socket for pid " <> tshow pid <> ". Link ghc-debug-stub and wrap main in withGhcDebug (Unix only)")
  let sources
        | null cwd = AttachStep AttachSources Nothing "Skipped: the agent didn't say where the program runs"
        | cwdThere = AttachStep AttachSources (Just cwd) ("Sources are looked up under " <> T.pack cwd)
        | otherwise = AttachStep AttachSources Nothing ("Skipped: " <> T.pack cwd <> " isn't on this machine")
  dumps <-
    if not cwdThere
      then pure (AttachStep AttachDumps Nothing "Skipped: no working directory to search")
      else do
        arts <- fromRight [] <$> try @SomeException (discoverArtifacts cwd)
        pure $
          if null arts
            then AttachStep AttachDumps Nothing ("Skipped: no dumps under " <> T.pack cwd <> ". Build with -ddump-simpl -ddump-stg-final -ddump-to-file")
            else AttachStep AttachDumps (Just cwd) (tshow (length arts) <> " modules' dumps under " <> T.pack cwd)
  pure [eventlog, hp, debug, sources, dumps]
