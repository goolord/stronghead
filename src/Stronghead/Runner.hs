-- | Project Target Discovery & Goal-Oriented Capture Recipes ("Target Runner").
--
-- Scans @.cabal@ files in a workspace to discover @executable@, @benchmark@,
-- and @test-suite@ targets, and builds isolated profiling/diagnostic runs with
-- goal-oriented recipes instead of manual GHC/RTS flag juggling.
module Stronghead.Runner
  ( TargetKind (..)
  , targetKindName
  , CabalTarget (..)
  , discoverCabalTargets
  , parseCabalTargets
    -- * Goal-oriented capture recipes
  , CaptureRecipe (..)
  , allRecipes
  , recipeTitle
  , recipeSummary
  , recipeCaveat
  , recipeBuildDir
  , recipeGhcFlags
  , recipeRtsFlags
  , formatRecipeCommand
  , LaunchSpec (..)
  , launchTargetRecipe
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import System.Process (CreateProcess (..), ProcessHandle, createProcess, proc)

import Stronghead.TextFile (readTextFile)

data TargetKind
  = ExeTarget
  | BenchTarget
  | TestTarget
  deriving stock (Eq, Ord, Show, Enum, Bounded)

targetKindName :: TargetKind -> Text
targetKindName = \case
  ExeTarget -> "executable"
  BenchTarget -> "benchmark"
  TestTarget -> "test-suite"

data CabalTarget = CabalTarget
  { ctPackage :: !Text
  , ctName :: !Text
  , ctKind :: !TargetKind
  , ctCabalFile :: !FilePath
  }
  deriving stock (Eq, Show)

-- | Discover Cabal targets across @.cabal@ files in @root@ (and one child
-- directory level for multi-package @cabal.project@ workspaces, walking up
-- parent directories when @root@ is a build or dump subdirectory).
discoverCabalTargets :: FilePath -> IO [CabalTarget]
discoverCabalTargets root0 = go (4 :: Int) root0
  where
    go stepsLeft root = do
      ok <- doesDirectoryExist root
      if not ok
        then pure []
        else do
          topEntries <- fromRight [] <$> try @SomeException (listDirectory root)
          let topCabals = [root </> e | e <- topEntries, ".cabal" `isSuffixOf` e]
          subDirs <-
            sequence
              [ do
                  let p = root </> e
                  isD <- doesDirectoryExist p
                  pure (if isD && not (hiddenOrBuild e) then [p] else [])
              | e <- topEntries
              ]
          subCabals <-
            fmap concat $
              sequence
                [ do
                    es <- fromRight [] <$> try @SomeException (listDirectory d)
                    pure [d </> f | f <- es, ".cabal" `isSuffixOf` f]
                | d <- concat subDirs
                ]
          let allCabals = topCabals ++ subCabals
          if null allCabals && stepsLeft > 0 && takeDirectory root /= root
            then go (stepsLeft - 1) (takeDirectory root)
            else do
              parsed <-
                sequence
                  [ do
                      eTxt <- try @SomeException (readTextFile cf)
                      pure case eTxt of
                        Left _ -> []
                        Right txt -> parseCabalTargets cf txt
                  | cf <- allCabals
                  ]
              pure (sortOn (\t -> (ctKind t, ctPackage t, ctName t)) (concat parsed))
    hiddenOrBuild ('.' : _) = True
    hiddenOrBuild n = "dist-" `T.isPrefixOf` T.pack n || n `elem` ["node_modules", "scratch"]

-- | Extract @executable@, @benchmark@, and @test-suite@ stanzas from a @.cabal@ file.
parseCabalTargets :: FilePath -> Text -> [CabalTarget]
parseCabalTargets cabalPath src =
  mapMaybe parseStanza ls
  where
    ls = T.lines src
    defaultPkg = T.pack (dropExtension (takeFileName cabalPath))
    pkgName =
      case [ T.strip (T.drop 1 rest)
           | l <- ls
           , not ("--" `T.isPrefixOf` T.stripStart l)
           , Just (c, _) <- [T.uncons l]
           , not (isSpace c)
           , let (k, rest) = T.breakOn ":" l
           , T.toLower (T.strip k) == "name"
           , not (T.null rest)
           ] of
        p : _ | not (T.null p) -> p
        _ -> defaultPkg
    parseStanza l
      | Just (c, _) <- T.uncons l
      , isSpace c =
          Nothing
      | otherwise =
          let cleaned = T.strip (fst (T.breakOn "--" l))
              ws = T.words cleaned
           in case ws of
                [kw, rawName] ->
                  let nm = T.dropAround (\ch -> ch == '"' || ch == '\'') rawName
                   in case T.toLower kw of
                        "executable" -> Just (CabalTarget pkgName nm ExeTarget cabalPath)
                        "benchmark" -> Just (CabalTarget pkgName nm BenchTarget cabalPath)
                        "test-suite" -> Just (CabalTarget pkgName nm TestTarget cabalPath)
                        _ -> Nothing
                _ -> Nothing

-- ---------------------------------------------------------------------------
-- Goal-Oriented Capture Recipes

data CaptureRecipe
  = RecipeQuickHealth
  | RecipeLeakProvenance
  | RecipeIrAudit
  | RecipeCallStacks
  deriving stock (Eq, Ord, Show, Enum, Bounded)

allRecipes :: [CaptureRecipe]
allRecipes = [minBound .. maxBound]

recipeTitle :: CaptureRecipe -> Text
recipeTitle = \case
  RecipeQuickHealth -> "Quick Health & Concurrency"
  RecipeLeakProvenance -> "Space-Leak Provenance (-hi)"
  RecipeIrAudit -> "Hot-Loop, Core/STG/Cmm & Ticky"
  RecipeCallStacks -> "Cost-Centre Call Stacks (-prof)"

recipeSummary :: CaptureRecipe -> Text
recipeSummary = \case
  RecipeQuickHealth -> "Low-overhead eventlog, thread scheduling, GC pauses, and IPE exception stacks."
  RecipeLeakProvenance -> "Maps every heap band and closure directly to its exact Haskell source line and STG binder."
  RecipeIrAudit -> "Dumps Core, STG, Cmm, and inlining logs alongside per-closure ticky entry & allocation counts."
  RecipeCallStacks -> "Full dynamic cost-centre call trees (.prof) and timeline CPU stack samples."

recipeCaveat :: CaptureRecipe -> Maybe Text
recipeCaveat = \case
  RecipeCallStacks ->
    Just "Note: -prof -fprof-auto inserts SCC ticks that can inhibit Core inlining and stream fusion; verify hot loops with the IR & Ticky recipe."
  _ -> Nothing

recipeBuildDir :: CaptureRecipe -> FilePath
recipeBuildDir = \case
  RecipeQuickHealth -> "dist-stronghead/health"
  RecipeLeakProvenance -> "dist-stronghead/ipe-heap"
  RecipeIrAudit -> "dist-stronghead/ir-ticky"
  RecipeCallStacks -> "dist-stronghead/prof"

recipeGhcFlags :: CaptureRecipe -> [Text]
recipeGhcFlags = \case
  RecipeQuickHealth ->
    ["-threaded", "-rtsopts", "-finfo-table-map"]
  RecipeLeakProvenance ->
    ["-threaded", "-rtsopts", "-finfo-table-map", "-fdistinct-constructor-tables"]
  RecipeIrAudit ->
    ["-O2", "-threaded", "-rtsopts", "-ticky", "-finfo-table-map", "-ddump-simpl", "-ddump-stg-final", "-ddump-cmm", "-ddump-inlinings", "-ddump-rule-firings", "-ddump-to-file"]
  RecipeCallStacks ->
    ["-threaded", "-rtsopts", "-prof", "-fprof-auto"]

recipeRtsFlags :: CaptureRecipe -> FilePath -> [Text]
recipeRtsFlags recipe evPath =
  let base = ["-l", "-ol" <> T.pack evPath, "-T"]
   in case recipe of
        RecipeQuickHealth -> base
        RecipeLeakProvenance -> base ++ ["-hi", "-i0.05"]
        RecipeIrAudit -> ["-lT", "-ol" <> T.pack evPath, "-T"]
        RecipeCallStacks -> base ++ ["-p", "-hc", "-i0.05"]

-- | Render the exact Cabal CLI invocation for transparency & copy-to-clipboard.
formatRecipeCommand :: CabalTarget -> CaptureRecipe -> [Text] -> FilePath -> Text
formatRecipeCommand ct recipe progArgs evPath =
  let compPrefix = case ctKind ct of
        ExeTarget -> "exe:"
        BenchTarget -> "bench:"
        TestTarget -> "test:"
      targetSpec = ctPackage ct <> ":" <> compPrefix <> ctName ct
      ghcOpt = "--ghc-options=\"" <> T.unwords (recipeGhcFlags recipe) <> "\""
      bdir = "--builddir=" <> T.pack (recipeBuildDir recipe)
      rts = ["+RTS"] ++ recipeRtsFlags recipe evPath ++ ["-RTS"]
   in T.unwords (["cabal", "run", bdir, ghcOpt, targetSpec, "--"] ++ progArgs ++ rts)

data LaunchSpec = LaunchSpec
  { lsWorkDir :: !FilePath
  , lsTarget :: !CabalTarget
  , lsRecipe :: !CaptureRecipe
  , lsArgs :: ![Text]
  , lsEventlogOut :: !FilePath
  }
  deriving stock (Eq, Show)

-- | Spawn @cabal run@ in the background with isolated @--builddir@ and recipe
-- compiler/RTS flags, returning the 'ProcessHandle' and expected dump directory.
launchTargetRecipe :: LaunchSpec -> IO (Either Text (ProcessHandle, FilePath))
launchTargetRecipe LaunchSpec {..} = do
  let compPrefix = case ctKind lsTarget of
        ExeTarget -> "exe:"
        BenchTarget -> "bench:"
        TestTarget -> "test:"
      targetSpec = T.unpack (ctPackage lsTarget <> ":" <> compPrefix <> ctName lsTarget)
      bdir = recipeBuildDir lsRecipe
      ghcOpts = "--ghc-options=" <> T.unpack (T.unwords (recipeGhcFlags lsRecipe))
      rtsArgs = ["+RTS"] ++ map T.unpack (recipeRtsFlags lsRecipe lsEventlogOut) ++ ["-RTS"]
      cabalArgs = ["run", "--builddir=" <> bdir, ghcOpts, targetSpec, "--"] ++ map T.unpack lsArgs ++ rtsArgs
      cp = (proc "cabal" cabalArgs) {cwd = Just lsWorkDir}
  r <- try @SomeException (createProcess cp)
  pure case r of
    Left ex -> Left ("Failed to launch cabal target: " <> T.pack (show ex))
    Right (_, _, _, ph) -> Right (ph, lsWorkDir </> bdir)
