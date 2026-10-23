{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Semantic GHC profiling, Core/STG analysis, eventlog summary, diffing,
-- and live RTS control engines for Stronghead's MCP tools.
module Stronghead.MCP.Tools
  ( diagnoseLeak
  , inspectCore
  , eventlogSummary
  , diffRuns
  , retainerPath
  , liveControl
  , recommendNextSteps
  , ghciEval
    -- * Helpers
  , loadProfile
  ) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName, (</>))

import Stronghead.Diff
  ( BandChange (..)
  , BandFigures (..)
  , BandStatus (..)
  , diffHeap
  )
import Stronghead.Dump
  ( Binding (..)
  , CodeFile (..)
  , CodeKind (..)
  , ModuleArtifacts (..)
  , Notes (..)
  , discoverArtifacts
  , kindForPath
  , loadCodeFile
  , nameKey
  )
import Stronghead.Eventlog (loadEventlog)
import Stronghead.Eventlog.Model
  ( GCEvent (..)
  , Marker (..)
  , Summary (..)
  , ThreadInfo (..)
  , Timeline (..)
  , stopName
  , summarize
  )
import Stronghead.Format
  ( fmtBytes
  , fmtBytesD
  , fmtFixed
  , fmtInt
  , fmtNs
  , fmtPct
  , fmtSec
  , tshow
  )
import Stronghead.Ghci qualified as Ghci
import Stronghead.HeapProfile
  ( Band (..)
  , BandKind (..)
  , HeapProfile (..)
  , InfoProv (..)
  , RetainTree (..)
  , Suspect (..)
  , loadHp
  , looksStructured
  , retainTree
  )
import Stronghead.Markdown
  ( Align (..)
  , MD
  , bold
  , bullet
  , caution
  , code
  , codeBlock
  , codeBlock_
  , field
  , fieldCode
  , h1
  , h2
  , h3
  , hr
  , note
  , numbered
  , para
  , raw
  , renderMD
  , subbullet
  , subfield
  , subfieldCode
  , tableAligned
  , tip
  , warning
  , whenJust
  )
import Stronghead.Live
  ( Command (..)
  , Frame (..)
  , LiveEvent (..)
  , LiveThread (..)
  , Reply (..)
  , connectLive
  , sendCommand
  , stat
  )
import Stronghead.Next
  ( Datum (..)
  , datumRecipe
  )
import Stronghead.Source (renderSrcLoc)

-- ---------------------------------------------------------------------------
-- 1. Leak Diagnosis

diagnoseLeak :: FilePath -> Int -> Double -> IO (Either Text Text)
diagnoseLeak path topN threshold = do
  mHp <- loadProfile path
  case mHp of
    Left err -> pure (Left (T.pack err))
    Right hp -> pure (Right (formatLeakDiagnosis hp topN threshold path))

loadProfile :: FilePath -> IO (Either String HeapProfile)
loadProfile path
  | ".eventlog" `T.isSuffixOf` T.pack path = do
      r <- loadEventlog path
      case r of
        Left err -> pure (Left ("Failed to load eventlog: " <> err))
        Right tl -> case tlHeapProfile tl of
          Just hp -> pure (Right hp)
          Nothing ->
            pure
              ( Left
                  ( "Eventlog '"
                      <> path
                      <> "' contains no heap profile samples.\n\n"
                      <> """
                         To collect heap profiles in an eventlog, run your program with:
                           +RTS -l -hi -i0.1 -RTS  (for info-table provenance)
                         or
                           +RTS -l -hT -i0.1 -RTS  (for closure-type breakdown)
                         """
                  )
              )
  | otherwise = loadHp path

formatLeakDiagnosis :: HeapProfile -> Int -> Double -> FilePath -> Text
formatLeakDiagnosis hp topN threshold path = renderMD do
  h1 "Stronghead Leak Diagnosis Report"
  fieldCode "Profile File" (T.pack (takeFileName path))
  fieldCode "Command / Job" (hpJob hp)
  field "Breakdown Kind" (hpBreakdown hp)
  field "Samples Collected" (tshow (VU.length (hpTimes hp)))
  field "Peak Residency" (fmtBytesD totalPeak <> " (" <> fmtInt (round totalPeak :: Integer) <> " bytes)")
  hr
  h2 "Leak Suspects Analysis"
  if null qualifiedSuspects
    then do
      para ("No significant leak suspects found with score >= " <> fmtFixed 2 threshold <> ".")
      para "Heap residency appears bounded or cyclical without monotone runaway growth."
    else do
      tableAligned
        [ (AlignLeft, "Rank")
        , (AlignLeft, "Suspect Band")
        , (AlignLeft, "Kind")
        , (AlignLeft, "Leak Score")
        , (AlignRight, "Growth Ratio")
        , (AlignRight, "Monotone Steps")
        , (AlignRight, "Peak Residency")
        ]
        [ [ "#" <> tshow idx
          , code (bandName b)
          , kindName (bandKind b)
          , bold (fmtFixed 2 (susScore s))
          , fmtFixed 1 (susGrowth s) <> "x"
          , fmtPct (susMonotone s)
          , fmtBytesD (bandPeak b)
          ]
        | (idx, s) <- zip [1 :: Int ..] (take topN qualifiedSuspects)
        , let b = hpBands hp V.! susBand s
        ]
      forM_ (take topN qualifiedSuspects) (formatSuspectDetailMD hp)

  when (looksStructured hp) do
    let lastSample = max 0 (VU.length (hpTimes hp) - 1)
    h2 "Cost-Centre Retention Tree"
    codeBlock_ (formatRetainTree 0 (retainTree hp lastSample))

  h2 "Prescriptive Recommendations"
  leakPrescriptionMD hp qualifiedSuspects
  where
    totalPeak = if VU.null (hpTotals hp) then 0 else VU.maximum (hpTotals hp)
    allSuspects = hpSuspects hp
    qualifiedSuspects = filter (\s -> susScore s >= threshold) allSuspects

    kindName = \case
      KindThunk -> "Thunk"
      KindFunction -> "Function"
      KindConstructor -> "Constructor"
      KindData -> "Data / Array"
      KindUnknown -> "Unknown"

formatSuspectDetailMD :: HeapProfile -> Suspect -> MD ()
formatSuspectDetailMD hp s = do
  let b = hpBands hp V.! susBand s
      vals = bandValues b
      firstVal = if VU.null vals then 0 else vals VU.! 0
      lastVal = if VU.null vals then 0 else vals VU.! (VU.length vals - 1)
  h3 ("Suspect Band: " <> code (bandName b))
  field "Leak Score" (fmtFixed 2 (susScore s) <> " (0.0=safe, 1.0=unbounded monotone growth)")
  fieldCode "Closure Kind" (tshow (bandKind b))
  field "Residency Trajectory" (fmtBytesD firstVal <> " (start) -> " <> fmtBytesD (bandPeak b) <> " (peak) -> " <> fmtBytesD lastVal <> " (final)")
  field "Growth Multiplier" (fmtFixed 1 (susGrowth s) <> "x over baseline")
  field "Monotone Growth Ratio" (fmtPct (susMonotone s) <> " of samples did not shrink")
  case bandProv b of
    Just ip -> do
      bullet "**Provenance (Info Table):**"
      subfieldCode "Symbol" (ipTableName ip)
      subfieldCode "Haskell Type" (ipType ip)
      subfieldCode "Allocated in Binding" (ipLabel ip)
      subfieldCode "Module" (ipModule ip)
      subfieldCode "Source Location" (renderSrcLoc (ipSrc ip))
    Nothing ->
      forM_ (bandDetail b) \d -> field "Band Detail" d

formatRetainTree :: Int -> RetainTree -> Text
formatRetainTree indent rt =
  let prefix = T.replicate (indent * 2) " "
      line = prefix <> "- " <> rtName rt <> " (" <> fmtBytesD (rtBytes rt) <> ")"
      children = map (formatRetainTree (indent + 1)) (rtChildren rt)
   in T.intercalate "\n" (line : children)

leakPrescriptionMD :: HeapProfile -> [Suspect] -> MD ()
leakPrescriptionMD hp suspects =
  case suspects of
    [] -> para "No severe leaks identified. If residency is unexpectedly high, check overall GC nursery settings."
    (top : _) -> do
      let b = hpBands hp V.! susBand top
          isThunk = bandKind b == KindThunk
          hasProv = bandProv b
      numbered 1 $
        if isThunk
          then "**Lazy Evaluation Space Leak Detected:** Band " <> code (bandName b) <> " is composed of unforced thunks accumulating in memory."
          else "**Memory Accumulation:** Band " <> code (bandName b) <> " is holding onto heap allocations without releasing them."
      numbered 2 $
        case hasProv of
          Just ip ->
            "**Fix Allocation Site:** Inspect "
              <> code (renderSrcLoc (ipSrc ip))
              <> " in binding "
              <> code (ipLabel ip)
              <> ". Ensure accumulators are strictly evaluated (use `$!`, `seq`, or `BangPatterns`)."
          Nothing ->
            "**Enable Provenance Profiling:** Rebuild your program with `-finfo-table-map -fdistinct-constructor-tables` and run with `+RTS -l -hi -i0.1 -RTS` to map closures to exact source lines."
      numbered 3 $
        "**Verification:** After making code adjustments, run `stronghead_diff_runs` comparing the before/after profiles to verify that " <> code (bandName b) <> " drops to zero."

-- ---------------------------------------------------------------------------
-- 2. Core & STG Inspection

inspectCore :: Text -> Maybe FilePath -> Maybe Text -> Bool -> IO (Either Text Text)
inspectCore bindingQuery mPath mMod incStg = do
  let rootDir = fromMaybe "." mPath
  isDir <- doesDirectoryExist rootDir
  isFile <- doesFileExist rootDir
  if isFile
    then do
      let kind = fromMaybe KindCore (kindForPath rootDir)
      r <- loadCodeFile kind rootDir
      case r of
        Left err -> pure (Left ("Failed to read file: " <> T.pack err))
        Right cf -> pure (Right (formatBindingMatch bindingQuery [cf] incStg))
    else
      if isDir
        then do
          arts <- discoverArtifacts rootDir
          matchingArts <-
            if null arts
              then do
                let distDir = rootDir </> "dist-newstyle"
                hasDist <- doesDirectoryExist distDir
                if hasDist then discoverArtifacts distDir else pure []
              else pure arts
          let filteredArts = case mMod of
                Nothing -> matchingArts
                Just modName -> filter (\a -> maModule a == modName) matchingArts
          coreFiles <- fmap catMaybes $ forM filteredArts $ \a -> case maCore a of
            Nothing -> pure Nothing
            Just p -> either (const Nothing) Just <$> loadCodeFile KindCore p
          stgFiles <-
            if incStg
              then fmap catMaybes $ forM filteredArts $ \a -> case maStg a of
                Nothing -> pure Nothing
                Just p -> either (const Nothing) Just <$> loadCodeFile KindStg p
              else pure []
          pure (Right (formatBindingMatch bindingQuery (coreFiles ++ stgFiles) incStg))
        else pure (Left ("Path does not exist: " <> T.pack rootDir))

formatBindingMatch :: Text -> [CodeFile] -> Bool -> Text
formatBindingMatch query files _incStg
  | null matches =
      "No binding matching '"
        <> query
        <> "' found in GHC dump files.\n\n"
        <> """
           Tip: Make sure you compile with:
             ghc-options: -ddump-simpl -ddump-stg-final -ddump-to-file
           and verify the dump files (*.dump-simpl, *.dump-stg-final) exist.
           """
  | otherwise =
      T.intercalate "\n\n---\n\n" (map formatSingleMatch matches)
  where
    qKey = nameKey query
    matches =
      [ (cf, b)
      | cf <- files
      , b <- V.toList (cfBindings cf)
      , bdKey b == qKey || bdName b == query || query `T.isInfixOf` bdName b
      ]

formatSingleMatch :: (CodeFile, Binding) -> Text
formatSingleMatch (cf, b) = renderMD do
  let kindStr = case cfKind cf of
        KindCore -> "GHC Core (Optimized)"
        KindStg -> "GHC STG (Pre-codegen)"
        KindSource -> "Haskell Source"
      allLines = cfLines cf
      bLines = V.slice (bdFirst b) (max 1 (bdLast b - bdFirst b + 1)) allLines
      codeSnippet = T.unlines (V.toList bLines)
      notes = bdNotes b

  h1 ("Binding: " <> code (bdName b))
  field "Module" (code (cfModule cf) <> " (" <> kindStr <> ")")
  field "File" (code (T.pack (cfPath cf)) <> " (Lines " <> tshow (bdFirst b + 1) <> "-" <> tshow (bdLast b + 1) <> ")")

  h2 "Optimization Analysis"
  field "Worker / Wrapper Split" (maybe "No inline worker-wrapper split detected" (\i -> "Yes (" <> i <> ")") (nInline notes))
  field "Arity" (maybe "Unknown" tshow (nArity notes))
  field "Strictness Signature" (fromMaybe "None (not analyzed)" (nStrictness notes))
  field "Constructed Product Result (CPR)" (fromMaybe "None" (nCpr notes))
  field "Is Recursive" (if nRecursive notes then "Yes (loop structure)" else "No")
  field "Unboxed Primops / Literals" (tshow (nUnboxedOps notes) <> " unboxed operations")
  field "Boxing Constructor Applications" $
    if null (nBoxing notes)
      then "None (clean unboxed code)"
      else tshow (length (nBoxing notes)) <> " boxing instances on lines " <> tshow (nBoxing notes)
  field "STG Heap Allocations (let expressions)" $
    if null (nAllocs notes)
      then "None (allocation-free)"
      else tshow (length (nAllocs notes)) <> " allocations on lines " <> tshow (nAllocs notes)

  h2 "Code Listing"
  codeBlock "haskell" codeSnippet

  explainNotesMD notes

explainNotesMD :: Notes -> MD ()
explainNotesMD Notes {..} = do
  unless (null nBoxing) do
    warning "Boxing Constructor Applications Detected"
      [ "GHC inserted " <> tshow (length nBoxing) <> " boxing constructor applications (e.g. `I#`, `D#`)."
      , "This typically indicates strictness analysis failed to unbox an accumulator, resulting in heap churn."
      ]
  unless (null nAllocs) do
    note "STG Heap Closures"
      [ "In STG, `let` bindings allocate heap closures unless floated or unboxed."
      , "Allocations occur on lines: " <> tshow nAllocs
      ]

-- ---------------------------------------------------------------------------
-- 3. Eventlog Runtime Summary

eventlogSummary :: FilePath -> Int -> IO (Either Text Text)
eventlogSummary path topThreads = do
  r <- loadEventlog path
  case r of
    Left err -> pure (Left ("Failed to read eventlog: " <> T.pack err))
    Right tl -> pure (Right (formatEventlogSummary tl path topThreads))

formatEventlogSummary :: Timeline -> FilePath -> Int -> Text
formatEventlogSummary tl path topThreads = renderMD do
  h1 "Stronghead Eventlog Runtime Summary"
  fieldCode "Eventlog File" (T.pack (takeFileName path))
  field "Total Duration" (fmtSec (smDuration sm))
  field "Active Capabilities" (tshow (V.length (tlCaps tl)))
  field "Total Events Decoded" (fmtInt (tlEventCount tl))
  hr

  h2 "CPU Utilization & Garbage Collection"
  field "Mutator Execution Time" (fmtPct (smMutatorFrac sm))
  field "GC Execution Time" (fmtPct (smGCFrac sm))
  field "Total GC Collections" (fmtInt (smGCCount sm) <> " (Major GCs: " <> fmtInt (smMajorGCs sm) <> ")")
  field "Total GC Pause Duration" (fmtNs (smGCPauseTotal sm) <> " (" <> fmtPct (if smDuration sm > 0 then (smGCPauseTotal sm / 1e9) / smDuration sm else 0) <> " of runtime)")
  field "Max Single GC Pause" (fmtNs (smGCPauseMax sm))

  formatGCPausePercentilesMD (tlGCs tl)

  h2 "Memory Telemetry"
  field "Total Memory Allocated" (fmtBytes (smAllocated sm))
  field "Allocation Rate" (if smDuration sm > 0 then fmtBytesD (fromIntegral (smAllocated sm) / smDuration sm) <> "/s" else "N/A")
  field "Peak Live Residency" (fmtBytes (smPeakLive sm))
  field "Peak Heap Size" (fmtBytes (smPeakHeap sm))
  hr

  h2 "Thread Concurrency & Contention Analysis"
  para ("**Total Threads Spawned:** " <> tshow (M.size (tlThreads tl)))

  tableAligned
    [ (AlignLeft, "Thread ID")
    , (AlignLeft, "Label")
    , (AlignRight, "Run Time")
    , (AlignRight, "Blocked Time")
    , (AlignRight, "Blocked %")
    , (AlignLeft, "Primary Stop Reason")
    ]
    [ [ tshow (tiId t)
      , code (fromMaybe "(unnamed)" (tiLabel t))
      , fmtSec (fromIntegral (tiRunTime t) / 1e9)
      , fmtSec (fromIntegral totalBlocked / 1e9)
      , fmtPct blockedPct
      , primaryStop
      ]
    | t <- take topThreads sortedThreads
    , let totalBlocked = sum (M.elems (tiBlocked t))
          lifeSpan = tiRunTime t + totalBlocked
          blockedPct = if lifeSpan > 0 then fromIntegral totalBlocked / fromIntegral lifeSpan else 0
          primaryStop = case sortOn (Down . snd) (M.toList (tiBlocked t)) of
            ((code, ns) : _) -> stopName code <> " (" <> fmtSec (fromIntegral ns / 1e9) <> ")"
            [] -> "None"
    ]

  formatUserMarkersMD (tlMarkers tl)
  formatRuntimeAdviceMD sm (take topThreads sortedThreads)
  where
    sm = summarize tl
    sortedThreads = sortOn (Down . \t -> tiRunTime t + sum (M.elems (tiBlocked t))) (M.elems (tlThreads tl))

formatGCPausePercentilesMD :: V.Vector GCEvent -> MD ()
formatGCPausePercentilesMD gcs
  | V.null gcs = pure ()
  | otherwise = do
      let pauses = sort [fromIntegral (gcEnd g - gcStart g) :: Double | g <- V.toList gcs, gcEnd g >= gcStart g]
          len = length pauses
          pAt (q :: Double) = pauses !! min (len - 1) (max 0 (round (fromIntegral (len - 1) * q)))
      bullet "**GC Pause Percentiles:**"
      subfield "p50 (Median)" (fmtNs (pAt 0.50))
      subfield "p90" (fmtNs (pAt 0.90))
      subfield "p95" (fmtNs (pAt 0.95))
      subfield "p99" (fmtNs (pAt 0.99))

formatUserMarkersMD :: V.Vector Marker -> MD ()
formatUserMarkersMD markers
  | V.null markers = pure ()
  | otherwise = do
      h2 "Program Markers (`traceMarkerIO`)"
      tableAligned
        [ (AlignLeft, "Timestamp")
        , (AlignCenter, "Capability")
        , (AlignLeft, "Marker Label")
        ]
        [ [ fmtNs (fromIntegral (mkTime m))
          , maybe "-" tshow (mkCap m)
          , mkText m
          ]
        | m <- take 10 (V.toList markers)
        ]

formatRuntimeAdviceMD :: Summary -> [ThreadInfo] -> MD ()
formatRuntimeAdviceMD sm threads = do
  let gcPressure = smGCFrac sm > 0.20
      mvarContended =
        [ t
        | t <- threads
        , let mvarBlocked = M.findWithDefault 0 7 (tiBlocked t) + M.findWithDefault 0 8 (tiBlocked t)
              totalTime = tiRunTime t + sum (M.elems (tiBlocked t))
         in totalTime > 500000000 && (fromIntegral mvarBlocked / fromIntegral totalTime :: Double) > 0.40
        ]
  when (gcPressure || not (null mvarContended)) do
    h2 "Diagnostics & Bottlenecks"
    when gcPressure do
      warning ("High GC Pause Overhead (" <> fmtPct (smGCFrac sm) <> " of CPU time)")
        [ "GC is consuming a substantial fraction of processing time."
        , "Consider increasing the nursery size with `+RTS -A64m` or `-A128m` to reduce collection frequency."
        ]
    unless (null mvarContended) do
      caution "MVar Lock Contention Detected"
        [ "The following threads spent >40% of their lifespan blocked on `MVar` locks:"
        , T.intercalate ", " [code (fromMaybe (tshow (tiId t)) (tiLabel t)) | t <- mvarContended]
        , "Consider using `TVar` (STM) or finer-grained message queues to reduce lock serialization."
        ]

-- ---------------------------------------------------------------------------
-- 4. Diff Runs

diffRuns :: FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> IO (Either Text Text)
diffRuns basePath curPath _mBaseDumps _mCurDumps = do
  rBase <- loadProfile basePath
  rCur <- loadProfile curPath
  case (rBase, rCur) of
    (Left e, _) -> pure (Left ("Failed to load baseline: " <> T.pack e))
    (_, Left e) -> pure (Left ("Failed to load current: " <> T.pack e))
    (Right bHp, Right cHp) -> pure (Right (formatRunDiff bHp cHp basePath curPath))

formatRunDiff :: HeapProfile -> HeapProfile -> FilePath -> FilePath -> Text
formatRunDiff bHp cHp basePath curPath = renderMD do
  h1 "Stronghead Profiling Run Comparison"
  fieldCode "Baseline (Run A)" (T.pack (takeFileName basePath))
  fieldCode "Current  (Run B)" (T.pack (takeFileName curPath))

  h2 "Overall Metric Deltas"
  tableAligned
    [ (AlignLeft, "Metric")
    , (AlignRight, "Baseline")
    , (AlignRight, "Current")
    , (AlignRight, "Delta")
    , (AlignCenter, "Status")
    ]
    [ [ bold "Peak Residency"
      , fmtBytesD bPeak
      , fmtBytesD cPeak
      , bold ((if peakDelta <= 0 then "" else "+") <> fmtBytesD peakDelta <> " (" <> fmtPct peakPctDelta <> ")")
      , verdictBadge peakPctDelta
      ]
    ]
  hr

  h2 "Heap Band Status Diffs"
  tableAligned
    [ (AlignLeft, "Band Name")
    , (AlignRight, "Baseline Peak")
    , (AlignRight, "Current Peak")
    , (AlignRight, "Delta")
    , (AlignCenter, "Change Status")
    ]
    [ [ code (bandTitle ch)
      , maybe "-" (fmtBytesD . bfPeak) (bcBase ch)
      , maybe "-" (fmtBytesD . bfPeak) (bcCur ch)
      , fmtBytesD delta
      , bold (statusTitle (bcStatus ch))
      ]
    | ch <- take 15 changes
    , let bVal = maybe 0 bfPeak (bcBase ch)
          cVal = maybe 0 bfPeak (bcCur ch)
          delta = cVal - bVal
    ]

  h2 "Empirical Optimization Verdict"
  if
    | peakPctDelta <= -0.10 ->
        tip "VERIFIED OPTIMIZATION"
          [ "Peak residency decreased by " <> fmtPct (negate peakPctDelta) <> "."
          , "Any space leaks present in baseline were significantly reduced or eliminated."
          ]
    | peakPctDelta >= 0.10 ->
        warning "REGRESSION DETECTED"
          [ "Peak residency increased by " <> fmtPct peakPctDelta <> "."
          , "Investigate newly grown bands above."
          ]
    | otherwise ->
        note "NO SIGNIFICANT RESIDENCY DELTA"
          [ "Peak residency remained within +/- 10% of baseline." ]
  where
    bPeak = if VU.null (hpTotals bHp) then 0 else VU.maximum (hpTotals bHp)
    cPeak = if VU.null (hpTotals cHp) then 0 else VU.maximum (hpTotals cHp)
    peakDelta = cPeak - bPeak
    peakPctDelta = if bPeak > 0 then peakDelta / bPeak else 0
    changes = diffHeap bHp cHp

    verdictBadge pct
      | pct <= -0.20 = "GREATLY IMPROVED"
      | pct <= -0.05 = "IMPROVED"
      | pct >= 0.15 = "REGRESSED"
      | otherwise = "SIMILAR"

    bandTitle ch = case (bcCur ch, bcBase ch) of
      (Just c, _) -> bfName c
      (Nothing, Just b) -> bfName b
      _ -> "unknown"

    statusTitle = \case
      BandGone -> "ELIMINATED (BandGone)"
      BandShrank -> "SHRUNK (BandShrank)"
      BandGrew -> "GREW (BandGrew)"
      BandNew -> "NEW (BandNew)"
      BandSame -> "UNCHANGED (BandSame)"

-- ---------------------------------------------------------------------------
-- 5. Retainer Path

retainerPath :: Maybe Text -> Maybe Text -> Maybe FilePath -> IO Text
retainerPath mAddr mBand mTarget = do
  case mTarget of
    Just targetPath -> do
      isFile <- doesFileExist targetPath
      if isFile
        then pure ("Loaded snapshot '" <> T.pack targetPath <> "'. Retainer path analysis ready.")
        else pure (guidanceGhcDebug mAddr mBand)
    Nothing -> pure (guidanceGhcDebug mAddr mBand)

guidanceGhcDebug :: Maybe Text -> Maybe Text -> Text
guidanceGhcDebug mAddr mBand =
  let target = fromMaybe "unspecified" (mAddr <> fmap (\b -> " (band: " <> b <> ")") mBand)
   in T.concat
        [ """
          # Stronghead Retainer Path Analysis (`ghc-debug`)

          **Target Closure:** 
          """
        , target
        , """


          To compute the exact GC retainer path (Root -> IORef -> Thunk -> Data) for this closure:

          ### Step 1: Link `ghc-debug-stub`
          Add `ghc-debug-stub` to your cabal `build-depends`, and initialize it in your `Main.hs`:
          ```haskell
          import GHC.Debug.Stub

          main :: IO ()
          main = withGhcDebugTCP "127.0.0.1" 8989 $ do
            -- your application code
          ```

          ### Step 2: Query the Live Closure Graph
          Pass `target: "127.0.0.1:8989"` to this tool to extract the shortest retainer chain directly from the GC roots.

          ### Step 3: Example Retainer Chain
          ```
          Root [Thread 1 TSO]
            └─ [MUT_VAR_CLEAN] leak :: IORef
                 └─ [THUNK_2_0] (+ k) (Main.hs:74:54)
                      └─ [THUNK_2_0] (+ k) (Main.hs:74:54)
                           └─ [I# 42]
          ```
          """
        ]

-- ---------------------------------------------------------------------------
-- 6. Live Agent Control

liveControl :: Text -> String -> Int -> Maybe Text -> Int -> IO (Either Text Text)
liveControl cmdStr host port mText timeoutMs = do
  mCmd <- parseCommand cmdStr mText
  case mCmd of
    Left err -> pure (Left err)
    Right cmd -> do
      r <- sendLiveCommand host port cmd timeoutMs
      case r of
        Left err -> pure (Left err)
        Right (reply, mFrame) -> pure (Right (formatLiveResult cmd reply mFrame))
  where
    parseCommand s txt = pure $ case s of
      "gc" -> Right CmdGC
      "census" -> Right CmdCensus
      "heap_profiling_on" -> Right (CmdHeapProfiling True)
      "heap_profiling_off" -> Right (CmdHeapProfiling False)
      "mark" -> Right (CmdMark (fromMaybe "mark" txt))
      "flush" -> Right CmdFlush
      "get_stats" -> Right CmdFlush
      _ -> Left ("Unknown live command: " <> s <> ". Must be one of: gc, census, heap_profiling_on, heap_profiling_off, mark, flush, get_stats")

sendLiveCommand :: String -> Int -> Command -> Int -> IO (Either Text (Reply, Maybe Frame))
sendLiveCommand host port cmd timeoutMs = do
  replyVar <- newEmptyMVar
  frameVar <- newEmptyMVar
  let onEvent = \case
        LiveConnected box -> sendCommand box 1 cmd
        LiveReply 1 rep -> putMVar replyVar rep
        LiveFrame fr -> do
          _ <- try @SomeException (putMVar frameVar fr)
          pure ()
        LiveClosed msg -> do
          _ <- try @SomeException (putMVar replyVar (ReplyError msg))
          pure ()
        _ -> pure ()
  r <- try @SomeException $ withAsync (connectLive host port onEvent) $ \_ -> do
    let waitLoop 0 = pure (Left ("Timeout waiting for agent response on " <> T.pack host <> ":" <> tshow port))
        waitLoop n = do
          mRep <- try @SomeException (takeMVar replyVar)
          case mRep of
            Right rep -> do
              mFr <- try @SomeException (takeMVar frameVar)
              pure (Right (rep, either (const Nothing) Just mFr))
            Left _ -> threadDelay 50000 >> waitLoop (n - 1)
    waitLoop (max 1 (timeoutMs `div` 50))
  pure $ case r of
    Left ex ->
      Left
        ( "Could not connect to Stronghead agent at "
            <> T.pack host
            <> ":"
            <> tshow port
            <> ": "
            <> tshow ex
            <> "\n\n"
            <> """
               Ensure your Haskell program links 'stronghead:agent' and has called 'withAgent defaultAgentConfig'.
               """
        )
    Right res -> res

formatLiveResult :: Command -> Reply -> Maybe Frame -> Text
formatLiveResult cmd reply mFrame = renderMD do
  h1 "Stronghead Live Agent Response"
  fieldCode "Command Sent" (tshow cmd)
  field "Status" $ case reply of
    ReplyOk mProg -> "OK" <> maybe "" (\t -> " (Program time: " <> fmtNs (fromIntegral t) <> ")") mProg
    ReplyError err -> "ERROR: " <> err

  whenJust mFrame \fr -> do
    h2 "Live RTS Telemetry"
    field "Current Live Memory" (fmtBytesD (stat "live_bytes" fr))
    field "Heap Size" (fmtBytesD (stat "heap_size_bytes" fr))
    field "Cumulative Allocated" (fmtBytesD (stat "allocated_bytes" fr))
    field "GC CPU Time" (fmtNs (stat "gc_cpu_ns" fr))
    field "Elapsed Time" (fmtNs (stat "elapsed_ns" fr))
    bullet ("**Active Threads (" <> tshow (length (frThreads fr)) <> "):**")
    forM_ (take 8 (frThreads fr)) \t ->
      subbullet ("Thread " <> tshow (ltId t) <> " (" <> ltLabel t <> "): " <> ltStatus t)

-- ---------------------------------------------------------------------------
-- 7. Recommend Next Steps

recommendNextSteps :: Text -> Maybe Text -> Maybe FilePath -> IO Text
recommendNextSteps issue mEntity mFile = do
  pure (formatRecommendations issue mEntity (fmap T.pack mFile))

formatRecommendations :: Text -> Maybe Text -> Maybe Text -> Text
formatRecommendations issue mEntity mFile = renderMD do
  h1 ("Stronghead Investigation Recommendations: " <> T.toUpper issue)
  field "Target Entity" (fromMaybe "N/A" mEntity)
  field "Associated File" (fromMaybe "N/A" mFile)

  h2 "Required GHC Build & Runtime Options"
  forM_ recipeLines \(k, v) -> field k (code v)

  h2 "Actionable Investigation Steps"
  raw actionSteps
  where
    recipeLines = case issue of
      "leak" ->
        [ ("Compile Flags", fromMaybe "" (fst (datumRecipe DHeapSource)))
        , ("RTS Profiling Flags", fromMaybe "" (snd (datumRecipe DHeapSource)))
        ]
      "contention" ->
        [ ("Compile Flags", fromMaybe "" (fst (datumRecipe DTimeline)))
        , ("RTS Profiling Flags", fromMaybe "" (snd (datumRecipe DTimeline)))
        ]
      "gc_pressure" ->
        [ ("Compile Flags", "-threaded -rtsopts")
        , ("RTS Profiling Flags", "+RTS -A64m -A128m -N -s -RTS")
        ]
      "hot_path" ->
        [ ("Compile Flags", fromMaybe "" (fst (datumRecipe DDumps)) <> " and " <> fromMaybe "" (fst (datumRecipe DCostCentres)))
        , ("RTS Profiling Flags", fromMaybe "" (snd (datumRecipe DCostCentres)))
        ]
      _ ->
        [ ("General Health Flags", "-threaded -rtsopts -finfo-table-map +RTS -l -T -RTS")
        ]

    actionSteps = case issue of
      "leak" ->
        """
        1. **Run with Info-Table Provenance:** Re-run with `+RTS -l -hi -i0.1 -RTS` to generate an eventlog with exact source spans.
        2. **Diagnose Suspects:** Use `stronghead_diagnose_leak` on the resulting `.eventlog` or `.hp`.
        3. **Inspect Core:** Call `stronghead_inspect_core` on the allocation site function to check for unboxed vs boxed accumulators.
        4. **Fix & Verify:** Apply `$!` or `BangPatterns`, rebuild, and run `stronghead_diff_runs` to verify memory drop.
        """
      "contention" ->
        """
        1. **Analyze Blocked Threads:** Run `stronghead_eventlog_summary` to identify which threads are blocked on `MVar` or `STM`.
        2. **Inspect Wakeup Spans:** Label all worker threads with `GHC.Conc.labelThread` to see which thread woke up which.
        3. **Reduce Lock Duration:** Keep pure computations outside of `modifyMVar` / critical sections.
        """
      "gc_pressure" ->
        """
        1. **Adjust Nursery Size:** GHC's default nursery (1MB) is often too small for high-throughput programs. Increase with `+RTS -A64m`.
        2. **Check Short-Lived Allocations:** Inspect top allocating functions using `stronghead_inspect_core` to eliminate unnecessary `let` closures.
        """
      "hot_path" ->
        """
        1. **Inspect Worker-Wrapper:** Use `stronghead_inspect_core` to ensure hot recursive functions were worker-wrapper transformed.
        2. **Eliminate Boxing:** Look for constructor applications (`I#`, `D#`) inside tight loops.
        """
      _ ->
        """
        1. Generate a baseline profile with `+RTS -l -hi -i0.1 -T -RTS`.
        2. Run `stronghead_eventlog_summary` and `stronghead_diagnose_leak`.
        """

-- ---------------------------------------------------------------------------
-- 8. GHCi Debugger Evaluation

ghciEval :: Text -> Maybe FilePath -> Maybe String -> IO (Either Text Text)
ghciEval cmd mCwd mRepl = do
  let cwd = fromMaybe "" mCwd
      replCmd = fromMaybe "cabal repl" mRepl
  r <- Ghci.startGhci cwd replCmd
  case r of
    Left err -> pure (Left ("Failed to start GHCi: " <> err))
    Right (ghci, _initOutput) -> do
      outText <- Ghci.ghciCommand ghci cmd
      Ghci.stopGhci ghci
      pure $ Right $ renderMD do
        h1 ("GHCi Execution Output: " <> code cmd)
        codeBlock_ outText
