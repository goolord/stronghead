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
  , formatSnapshotRetainers
  , liveControl
  , recommendNextSteps
  , ghciEval
  , renderGraph
    -- * MCP v2 diagnostic engines
  , heapAnatomy
  , formatHeapAnatomy
  , criticalPathReport
  , formatCriticalPath
  , coreSmellsReport
  , formatCoreSmells
  , liveStacksReport
  , rtsSweepReport
  , assertBudgetReport
    -- * MCP v3 stateful & synthesis engines
  , triageRunReport
  , queryReport
  , strictnessAuditReport
  , experimentReport
  , debugSessionTool
  , guiCopilotTool
    -- * Helpers & Cache
  , loadProfile
  , cachedLoadEventlog
  , cachedLoadHp
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, makeAbsolute)
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))
import System.IO (IOMode (WriteMode), hSetEncoding, utf8, withFile)
import System.IO.Unsafe (unsafePerformIO)

import UI.Graph
  ( renderHpDiffSvg
  , renderHpSvg
  )
import UI.Shell (sendCopilotCommand)

import Stronghead.Debug
  ( ClosureNode (..)
  , DebugTarget (..)
  , HeapSnapshot (..)
  , defaultSnapshotOptions
  , describePath
  , fmtClosureId
  , ghcDebugEnabled
  , nodeTitle
  , parseHostPort
  , takeSnapshot
  )
import Stronghead.Debug qualified as Dbg
import Stronghead.Debug.Anatomy
  ( ContainerHealth (..)
  , DominatorTree (..)
  , DuplicateGroup (..)
  , HeapSkeleton (..)
  , SeverResult (..)
  , SeverTarget (..)
  , SpineFinding (..)
  , StrictnessAdvice (..)
  , auditContainers
  , auditDuplicates
  , auditStrictnessOpportunities
  , buildHeapSkeleton
  , computeDominators
  , simulateSever
  , snapshotFromProbes
  )
import Stronghead.Diff
  ( BandChange (..)
  , BandFigures (..)
  , BandStatus (..)
  , BindingChange (..)
  , CostChange (..)
  , diffBindings
  , diffCosts
  , diffHeap
  , fmtChange
  , loadBindingSummaries
  )
import Stronghead.Join (CostTotal (..), costTotals)
import Stronghead.Prof (ProfFile (..), loadProf)
import Stronghead.Dump
  ( Binding (..)
  , CodeFile (..)
  , CodeKind (..)
  , FullModuleArtifacts (..)
  , ModuleArtifacts (..)
  , Notes (..)
  , discoverArtifacts
  , discoverFullArtifacts
  , kindForPath
  , loadCodeFile
  , nameKey
  )
import Stronghead.Dump.Forensics qualified as Forensics
import Stronghead.Eventlog (loadEventlog)
import Stronghead.Eventlog.CriticalPath
  ( CriticalPathResult (..)
  , analyzeCriticalPath
  , analyzeSpanCriticalPath
  )
import Stronghead.Eventlog.Model
  ( GCEvent (..)
  , GCSyncLag (..)
  , Marker (..)
  , MemReturnPoint (..)
  , NonmovingStats (..)
  , RunnableStats (..)
  , SparkStats (..)
  , Summary (..)
  , ThreadInfo (..)
  , Timeline (..)
  , TraceAlert (..)
  , UserSpan (..)
  , runnableStats
  , stopName
  , summarize
  , threadName
  )
import Stronghead.Lab
  ( BenchCase (..)
  , ComparisonStats (..)
  , StatVerdict (..)
  , SweepConfig (..)
  , TrialStats (..)
  , defaultSweepGrid
  , evaluateBudget
  , formatBudgetMarkdown
  , parseBenchCsv
  , parseBudgetSpec
  , renderParetoMarkdown
  , runRtsSweep
  , runStatisticalExperiment
  )
import Stronghead.Format
  ( fmtBytes
  , fmtBytesD
  , fmtFixed
  , fmtInt
  , fmtNs
  , fmtPct
  , fmtSec
  , readHex
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
  , leakSuspects
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
  ( AgentBeacon (..)
  , Command (..)
  , Frame (..)
  , LiveEvent (..)
  , LiveFrameEntry (..)
  , LiveProbeNode (..)
  , LiveThread (..)
  , LiveThreadStack (..)
  , Reply (..)
  , connectLive
  , discoverAgents
  , sendCommand
  , stat
  )
import Stronghead.Next
  ( Datum (..)
  , datumRecipe
  )
import Stronghead.Query
  ( QueryContext (..)
  , buildQueryBindings
  , emptyQueryContext
  , evalQuery
  , formatQueryJson
  , formatQueryMarkdown
  , parseQuery
  )
import Stronghead.Ref (RunKey (..))
import Stronghead.Run (runKey)
import Stronghead.Source (renderSrcLoc, renderSrcSpan)
import Stronghead.Triage
  ( TriageInput (..)
  , emptyTriageInput
  , formatTriageJson
  , formatTriageMarkdown
  , triageRun
  )

-- ---------------------------------------------------------------------------
-- 1. Leak Diagnosis

diagnoseLeak :: FilePath -> Int -> Double -> IO (Either Text Text)
diagnoseLeak path topN threshold = do
  mHp <- loadProfile path
  case mHp of
    Left err -> pure (Left (T.pack err))
    Right hp -> pure (Right (formatLeakDiagnosis hp topN threshold path))

type FileStamp = (FilePath, String, Integer)

eventlogCacheRef :: IORef (M.Map FilePath (FileStamp, Timeline))
eventlogCacheRef = unsafePerformIO (newIORef M.empty)
{-# NOINLINE eventlogCacheRef #-}

hpCacheRef :: IORef (M.Map FilePath (FileStamp, HeapProfile))
hpCacheRef = unsafePerformIO (newIORef M.empty)
{-# NOINLINE hpCacheRef #-}

profCacheRef :: IORef (M.Map FilePath (FileStamp, ProfFile))
profCacheRef = unsafePerformIO (newIORef M.empty)
{-# NOINLINE profCacheRef #-}

fileStampOf :: FilePath -> IO (Maybe FileStamp)
fileStampOf p = do
  r <- try @SomeException do
    ap <- makeAbsolute p
    mt <- getModificationTime ap
    sz <- getFileSize ap
    pure (ap, show mt, sz)
  pure (either (const Nothing) Just r)

cachedLoadEventlog :: FilePath -> IO (Either String Timeline)
cachedLoadEventlog p = do
  mSt <- fileStampOf p
  case mSt of
    Nothing -> loadEventlog p
    Just st@(ap, _, _) -> do
      cache <- readIORef eventlogCacheRef
      case M.lookup ap cache of
        Just (st0, tl) | st0 == st -> pure (Right tl)
        _ -> do
          res <- loadEventlog ap
          case res of
            Right tl -> atomicModifyIORef' eventlogCacheRef (\m -> (M.insert ap (st, tl) m, ())) >> pure (Right tl)
            Left err -> pure (Left err)

cachedLoadHp :: FilePath -> IO (Either String HeapProfile)
cachedLoadHp p = do
  mSt <- fileStampOf p
  case mSt of
    Nothing -> loadHp p
    Just st@(ap, _, _) -> do
      cache <- readIORef hpCacheRef
      case M.lookup ap cache of
        Just (st0, hp) | st0 == st -> pure (Right hp)
        _ -> do
          res <- loadHp ap
          case res of
            Right hp -> atomicModifyIORef' hpCacheRef (\m -> (M.insert ap (st, hp) m, ())) >> pure (Right hp)
            Left err -> pure (Left err)

cachedLoadProf :: FilePath -> IO (Either String ProfFile)
cachedLoadProf p = do
  mSt <- fileStampOf p
  case mSt of
    Nothing -> loadProf p
    Just st@(ap, _, _) -> do
      cache <- readIORef profCacheRef
      case M.lookup ap cache of
        Just (st0, pf) | st0 == st -> pure (Right pf)
        _ -> do
          res <- loadProf ap
          case res of
            Right pf -> atomicModifyIORef' profCacheRef (\m -> (M.insert ap (st, pf) m, ())) >> pure (Right pf)
            Left err -> pure (Left err)

loadProfile :: FilePath -> IO (Either String HeapProfile)
loadProfile path
  | ".eventlog" `T.isSuffixOf` T.pack path = do
      r <- cachedLoadEventlog path
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
  | otherwise = cachedLoadHp path

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
        KindCmm -> "GHC Cmm (Low-level RTS IR)"
        KindAsm -> "Native Assembly / LLVM"
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
diffRuns basePath curPath mBaseDumps mCurDumps = do
  let isEventlog p = ".eventlog" `T.isSuffixOf` T.pack p
      isProf p = ".prof" `T.isSuffixOf` T.pack p
  mBindingDiff <- case (mBaseDumps, mCurDumps) of
    (Just bd, Just cd) -> do
      bOk <- doesDirectoryExist bd
      cOk <- doesDirectoryExist cd
      if bOk && cOk
        then do
          bs <- loadBindingSummaries bd
          cs <- loadBindingSummaries cd
          pure (Just (diffBindings bs cs))
        else pure Nothing
    _ -> pure Nothing
  if isProf basePath && isProf curPath
    then do
      rBase <- loadProf basePath
      rCur <- loadProf curPath
      case (rBase, rCur) of
        (Left e, _) -> pure (Left ("Failed to load baseline .prof: " <> T.pack e))
        (_, Left e) -> pure (Left ("Failed to load current .prof: " <> T.pack e))
        (Right bp, Right cp) ->
          pure (Right (formatMultiRunDiff Nothing Nothing (Just (diffCosts (costTotals (pfRoots bp)) (costTotals (pfRoots cp)))) mBindingDiff basePath curPath))
    else
      if isEventlog basePath && isEventlog curPath
        then do
          rBase <- loadEventlog basePath
          rCur <- loadEventlog curPath
          case (rBase, rCur) of
            (Left e, _) -> pure (Left ("Failed to load baseline eventlog: " <> T.pack e))
            (_, Left e) -> pure (Left ("Failed to load current eventlog: " <> T.pack e))
            (Right bTl, Right cTl) ->
              let mHpPair = (,) <$> tlHeapProfile bTl <*> tlHeapProfile cTl
               in pure (Right (formatMultiRunDiff (Just (bTl, cTl)) mHpPair Nothing mBindingDiff basePath curPath))
        else do
          rBase <- loadProfile basePath
          rCur <- loadProfile curPath
          case (rBase, rCur) of
            (Left e, _) -> pure (Left ("Failed to load baseline: " <> T.pack e))
            (_, Left e) -> pure (Left ("Failed to load current: " <> T.pack e))
            (Right bHp, Right cHp) ->
              pure (Right (formatMultiRunDiff Nothing (Just (bHp, cHp)) Nothing mBindingDiff basePath curPath))

formatMultiRunDiff
  :: Maybe (Timeline, Timeline)
  -> Maybe (HeapProfile, HeapProfile)
  -> Maybe [CostChange]
  -> Maybe [BindingChange]
  -> FilePath
  -> FilePath
  -> Text
formatMultiRunDiff mTimelines mHeaps mCosts mBindings basePath curPath = renderMD do
  h1 "Stronghead Profiling Run Comparison"
  fieldCode "Baseline (Run A)" (T.pack (takeFileName basePath))
  fieldCode "Current  (Run B)" (T.pack (takeFileName curPath))

  forM_ mTimelines \(bTl, cTl) -> do
    let bSm = summarize bTl
        cSm = summarize cTl
        bP99 = gcPausePercentile 0.99 (tlGCs bTl)
        cP99 = gcPausePercentile 0.99 (tlGCs cTl)
    h2 "Eventlog Runtime Deltas"
    tableAligned
      [ (AlignLeft, "Metric")
      , (AlignRight, "Baseline")
      , (AlignRight, "Current")
      , (AlignRight, "Change")
      ]
      [ ["Duration", fmtSec (smDuration bSm), fmtSec (smDuration cSm), fmtChange (smDuration bSm) (smDuration cSm)]
      , ["GC CPU Share", fmtPct (smGCFrac bSm), fmtPct (smGCFrac cSm), fmtChange (smGCFrac bSm) (smGCFrac cSm)]
      , ["GC Collections", fmtInt (smGCCount bSm) <> " (" <> fmtInt (smMajorGCs bSm) <> " major)", fmtInt (smGCCount cSm) <> " (" <> fmtInt (smMajorGCs cSm) <> " major)", fmtChange (fromIntegral (smGCCount bSm)) (fromIntegral (smGCCount cSm))]
      , ["GC Pause p99", fmtNs bP99, fmtNs cP99, fmtChange bP99 cP99]
      , ["Max GC Pause", fmtNs (smGCPauseMax bSm), fmtNs (smGCPauseMax cSm), fmtChange (smGCPauseMax bSm) (smGCPauseMax cSm)]
      , ["Total Allocated", fmtBytes (smAllocated bSm), fmtBytes (smAllocated cSm), fmtChange (fromIntegral (smAllocated bSm)) (fromIntegral (smAllocated cSm))]
      , ["Peak Live Residency", fmtBytes (smPeakLive bSm), fmtBytes (smPeakLive cSm), fmtChange (fromIntegral (smPeakLive bSm)) (fromIntegral (smPeakLive cSm))]
      ]
    hr

  forM_ mHeaps \(bHp, cHp) -> do
    let bPeak = if VU.null (hpTotals bHp) then 0 else VU.maximum (hpTotals bHp)
        cPeak = if VU.null (hpTotals cHp) then 0 else VU.maximum (hpTotals cHp)
        peakDelta = cPeak - bPeak
        peakPctDelta = if bPeak > 0 then peakDelta / bPeak else 0
        changes = diffHeap bHp cHp
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

  forM_ mCosts \changes -> do
    h2 "Cost-Centre Deltas (.prof)"
    if null changes
      then para "No cost-centre changes detected."
      else
        tableAligned
          [ (AlignLeft, "Cost Centre")
          , (AlignRight, "Base Time %")
          , (AlignRight, "Cur Time %")
          , (AlignRight, "Base Alloc %")
          , (AlignRight, "Cur Alloc %")
          ]
          [ [ code (cgModule ch <> "." <> cgName ch)
            , maybe "-" (fmtFixed 1 . ctInhTime) (cgBase ch)
            , maybe "-" (fmtFixed 1 . ctInhTime) (cgCur ch)
            , maybe "-" (fmtFixed 1 . ctInhAlloc) (cgBase ch)
            , maybe "-" (fmtFixed 1 . ctInhAlloc) (cgCur ch)
            ]
          | ch <- take 20 changes
          ]

  forM_ mBindings \changes -> do
    h2 "Core & STG Binding Diffs"
    if null changes
      then para "No Core/STG optimization changes detected between dump directories."
      else
        tableAligned
          [ (AlignLeft, "Module")
          , (AlignLeft, "Binding")
          , (AlignLeft, "Changes")
          ]
          [ [ code (bgModule ch)
            , code (bgKey ch)
            , T.intercalate "; " (bgChanges ch)
            ]
          | ch <- take 25 changes
          ]
  where
    gcPausePercentile :: Double -> V.Vector GCEvent -> Double
    gcPausePercentile q gcs
      | V.null gcs = 0
      | otherwise =
          let pauses = sort [fromIntegral (gcEnd g - gcStart g) :: Double | g <- V.toList gcs, gcEnd g >= gcStart g]
              len = length pauses
           in if len == 0 then 0 else pauses !! min (len - 1) (max 0 (round (fromIntegral (len - 1) * q)))

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
    Nothing -> pure (guidanceGhcDebug mAddr mBand Nothing)
    Just targetStr
      | not ghcDebugEnabled ->
          pure (guidanceGhcDebug mAddr mBand (Just "This build of Stronghead has no `ghc-debug` support enabled. Rebuild with `cabal build -f ghc-debug`."))
      | otherwise -> do
          isFile <- doesFileExist targetStr
          let dbgTarget
                | isFile = DebugSnapshotFile targetStr
                | Just (h, p) <- parseHostPort (T.pack targetStr) = DebugTCP h p
                | otherwise = DebugUnix targetStr
          res <- takeSnapshot defaultSnapshotOptions dbgTarget
          case res of
            Left err -> pure (guidanceGhcDebug mAddr mBand (Just ("Snapshot failed for `" <> T.pack targetStr <> "`: " <> T.pack err)))
            Right snap -> pure (formatSnapshotRetainers mAddr mBand snap)

-- | Pure retainer-path formatter over a 'HeapSnapshot', selecting closures by
-- hex address, band/type/name substring, or top held closures when unspecified.
formatSnapshotRetainers :: Maybe Text -> Maybe Text -> HeapSnapshot -> Text
formatSnapshotRetainers mAddr mBand snap = renderMD do
  h1 "Stronghead Retainer Path Analysis"
  fieldCode "Snapshot" (snapLabel snap)
  field "Reachable Closures" (fmtInt (M.size (snapNodes snap)))
  field "Total Snapshot Bytes" (fmtBytes (fromIntegral (snapBytes snap)))
  when (snapTruncated snap) do
    warning "Snapshot Truncated" ["Closure traversal reached the configured closure cap; some deep retainers may be omitted."]
  hr
  case targets of
    [] -> do
      para "No matching closures found in snapshot."
      whenJust mAddr \a -> fieldCode "Requested Address" a
      whenJust mBand \b -> fieldCode "Requested Band" b
    matches -> do
      h2 ("Retainer Chains (" <> tshow (length matches) <> " matched)")
      forM_ matches \cid -> do
        let path = Dbg.retainerPath snap cid
            heldBytes = M.findWithDefault 0 cid (snapHeld snap)
            mNode = M.lookup cid (snapNodes snap)
            title = maybe (fmtClosureId cid) nodeTitle mNode
        h3 (title <> " (" <> code (fmtClosureId cid) <> ")")
        field "Subtree Held" (fmtBytes (fromIntegral heldBytes))
        field "Summary" (describePath path)
        codeBlock_ (renderChain path)
  where
    targets = case mAddr >>= readHex of
      Just cid | M.member cid (snapNodes snap) -> [cid]
      _ -> case mBand of
        Just b
          | not (T.null (T.strip b)) ->
              let q = T.toLower (T.strip b)
                  matched =
                    [ cnId n
                    | n <- M.elems (snapNodes snap)
                    , q `T.isInfixOf` T.toLower (cnName n)
                        || q `T.isInfixOf` T.toLower (cnType n)
                        || maybe False (T.isInfixOf q . T.toLower) (cnSource n)
                    ]
               in take 5 (sortOn (\i -> Down (M.findWithDefault 0 i (snapHeld snap))) matched)
        _ -> take 5 (snapBiggest snap)

    renderChain [] = "(not reachable from GC roots)"
    renderChain nodes =
      T.unlines
        [ T.replicate (idx * 2) " "
            <> (if idx == 0 then "Root " else "└─ ")
            <> "[" <> cnType n <> "] "
            <> cnName n
            <> maybe "" (\s -> " (" <> s <> ")") (cnSource n)
            <> "  (" <> fmtClosureId (cnId n) <> ", " <> fmtBytes (fromIntegral (cnSize n)) <> ")"
        | (idx, n) <- zip [0 :: Int ..] nodes
        ]

guidanceGhcDebug :: Maybe Text -> Maybe Text -> Maybe Text -> Text
guidanceGhcDebug mAddr mBand mNote =
  let target = fromMaybe "unspecified" (mAddr <> fmap (\b -> " (band: " <> b <> ")") mBand)
      noteBlock = maybe "" (\n -> "\n> **Note:** " <> n <> "\n") mNote
   in T.concat
        [ """
          # Stronghead Retainer Path Analysis (`ghc-debug`)

          **Target Closure:** 
          """
        , target
        , noteBlock
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
      "era_bump" -> Right CmdEraBump
      _ -> Left ("Unknown live command: " <> s <> ". Must be one of: gc, census, heap_profiling_on, heap_profiling_off, mark, flush, get_stats, era_bump")

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

-- ---------------------------------------------------------------------------
-- 9. Bespoke SVG Heap Profile Graph Generator

-- | Produce publication-quality bespoke SVG heap profile graphs matching classic hp2ps
-- PostScript styling with modern Stronghead formatting, typography, and empirical
-- before/after diff layouts for issue reports and proof of fixes.
renderGraph
  :: FilePath             -- ^ Path to .hp file
  -> Maybe FilePath       -- ^ Optional baseline .hp file
  -> Maybe FilePath       -- ^ Optional output SVG path
  -> Maybe String         -- ^ Theme ("classic", "dark", "light")
  -> Maybe Int            -- ^ Top N bands
  -> IO (Either Text Text)
renderGraph hpPath mBaseline mOutPath mTheme mTopN = do
  exists <- doesFileExist hpPath
  if not exists
    then pure (Left $ "Heap profile not found: " <> T.pack hpPath)
    else do
      mBaseValid <- case mBaseline of
        Just bp -> do
          be <- doesFileExist bp
          if be
            then fmap (Right . Just) (makeAbsolute bp)
            else pure (Left $ "Baseline heap profile not found: " <> T.pack bp)
        Nothing -> pure (Right Nothing)
      case mBaseValid of
        Left err -> pure (Left err)
        Right validBaseline -> do
          absHp <- makeAbsolute hpPath
          eHp <- loadProfile absHp
          case eHp of
            Left err -> pure (Left $ "Failed to load heap profile: " <> T.pack err)
            Right curHp -> do
              let writeUtf8File fp txt = withFile fp WriteMode \h -> do
                    hSetEncoding h utf8
                    TIO.hPutStr h txt

              case validBaseline of
                Nothing -> do
                  let outSvg = fromMaybe (takeDirectory absHp </> takeBaseName absHp <> ".svg") mOutPath
                  svg <- renderHpSvg mTheme mTopN curHp
                  createDirectoryIfMissing True (takeDirectory outSvg)
                  writeUtf8File outSvg svg

                  let peak = if VU.null (hpTotals curHp) then 0 else VU.maximum (hpTotals curHp)
                      censuses = VU.length (hpTimes curHp)
                      suspects = length (leakSuspects curHp)
                      metrics = "\n**Heap Profile Summary:**\n\n"
                                <> "* **Peak Residency:** " <> fmtBytesD peak <> "\n"
                                <> "* **Total Censuses:** " <> tshow censuses <> "\n"
                                <> "* **Identified Leak Suspects:** " <> tshow suspects <> "\n"

                  pure $ Right $
                    "### Standalone SVG Heap Profile Graph\n\n"
                    <> "Generated bespoke vector heap profile graph using nano-ui-svg:\n\n"
                    <> "![" <> T.pack (takeFileName outSvg) <> "](" <> T.pack outSvg <> ")\n"
                    <> metrics

                Just absBase -> do
                  eBase <- loadProfile absBase
                  case eBase of
                    Left err -> pure (Left $ "Failed to load baseline heap profile: " <> T.pack err)
                    Right baseHp -> do
                      let outSvg = fromMaybe (takeDirectory absHp </> takeBaseName absHp <> "_proof_diff.svg") mOutPath
                      diffSvg <- renderHpDiffSvg mTheme mTopN baseHp curHp
                      createDirectoryIfMissing True (takeDirectory outSvg)
                      writeUtf8File outSvg diffSvg

                      let basePeak = if VU.null (hpTotals baseHp) then 0 else VU.maximum (hpTotals baseHp)
                          curPeak = if VU.null (hpTotals curHp) then 0 else VU.maximum (hpTotals curHp)
                          baseCensuses = VU.length (hpTimes baseHp)
                          curCensuses = VU.length (hpTimes curHp)
                          baseSuspects = length (leakSuspects baseHp)
                          curSuspects = length (leakSuspects curHp)
                          reduction = if basePeak > 0
                            then let pct = (1.0 - (curPeak / basePeak)) * 100.0 :: Double
                                  in T.pack (showFFloat (Just 1) pct "") <> "%"
                            else "N/A"

                      let diffTable =
                            "\n### Empirical Verification & Metrics Comparison\n\n"
                            <> "| Metric | Baseline (Before Fix) | Current (After Fix) | Improvement |\n"
                            <> "| :--- | :--- | :--- | :--- |\n"
                            <> "| **Peak Heap Residency** | " <> fmtBytesD basePeak
                            <> " | " <> fmtBytesD curPeak
                            <> " | **-" <> reduction <> "** reduction |\n"
                            <> "| **Total Censuses** | " <> tshow baseCensuses
                            <> " | " <> tshow curCensuses
                            <> " | " <> (let d = curCensuses - baseCensuses in if d >= 0 then "+" <> tshow d else tshow d) <> " |\n"
                            <> "| **Identified Leak Suspects** | " <> tshow baseSuspects
                            <> " | " <> tshow curSuspects
                            <> " | " <> (if curSuspects < baseSuspects then "Fixed (" <> tshow (baseSuspects - curSuspects) <> " suspects eliminated)" else "0") <> " |\n"

                      pure $ Right $
                        "## Stronghead Proof of Fix (Bespoke SVG Graph)\n\n"
                        <> "Generated bespoke publication-quality SVG heap graph for issue reporting:\n\n"
                        <> "![" <> T.pack (takeFileName outSvg) <> "](" <> T.pack outSvg <> ")\n\n"
                        <> diffTable

-- ---------------------------------------------------------------------------
-- 10. Topological Heap Anatomy & Counterfactual What-If Severing

heapAnatomy :: String -> Maybe Text -> Maybe Text -> Int -> IO (Either Text Text)
heapAnatomy rawTarget mSevClos mSevType topN = do
  if not ghcDebugEnabled
    then pure (Left "Stronghead was built without ghc-debug support (-f ghc-debug).")
    else do
      isFile <- doesFileExist rawTarget
      let dbgTarget
            | isFile = DebugSnapshotFile rawTarget
            | Just (h, p) <- parseHostPort (T.pack rawTarget) = DebugTCP h p
            | otherwise = DebugUnix rawTarget
      res <- takeSnapshot defaultSnapshotOptions dbgTarget
      pure case res of
        Left err -> Left ("Failed to take/load heap snapshot: " <> T.pack err)
        Right snap -> Right (formatHeapAnatomy snap mSevClos mSevType topN)

formatHeapAnatomy :: HeapSnapshot -> Maybe Text -> Maybe Text -> Int -> Text
formatHeapAnatomy snap mSevClos mSevType topN =
  let dt = computeDominators snap
      skel = buildHeapSkeleton snap dt
      dups = auditDuplicates snap
      mSever = case (mSevClos >>= readHex, mSevType) of
        (Just cid, _) -> Just (simulateSever snap (SeverClosure cid))
        (Nothing, Just ty) | not (T.null (T.strip ty)) -> Just (simulateSever snap (SeverType (T.strip ty)))
        _ -> Nothing
   in renderMD do
        h1 ("Stronghead Topological Heap Anatomy: " <> snapLabel snap)
        field "Reachable Closures" (fmtInt (M.size (snapNodes snap)))
        field "Total Bytes" (fmtBytes (fromIntegral (snapBytes snap)))
        field "GC Roots" (fmtInt (length (snapRoots snap)))
        whenJust mSever \sr -> do
          h2 "Counterfactual What-If Severing Result"
          field "Before" (fmtBytes (fromIntegral (srBeforeBytes sr)))
          field "After" (fmtBytes (fromIntegral (srAfterBytes sr)))
          field "Freed" (bold (fmtBytes (fromIntegral (srFreedBytes sr))) <> " (" <> fmtPct (srFreedFraction sr) <> ", " <> fmtInt (srFreedClosures sr) <> " closures)")
          unless (null (srTopFreed sr)) $
            tableAligned
              [(AlignLeft, "Freed Closure"), (AlignRight, "Count"), (AlignRight, "Bytes")]
              [[code nm, fmtInt cnt, fmtBytes (fromIntegral b)] | (nm, cnt, b) <- take topN (srTopFreed sr)]
        h2 ("Folded Quotient Spines (" <> tshow (length (hsSpines skel)) <> ")")
        if null (hsSpines skel)
          then para "No homogeneous thunk or list spines detected."
          else
            tableAligned
              [(AlignLeft, "Spine"), (AlignLeft, "Type"), (AlignRight, "Depth"), (AlignRight, "Retained"), (AlignLeft, "Anchor")]
              [ [ code (spName sp)
                , code (spType sp)
                , tshow (spDepth sp)
                , fmtBytes (fromIntegral (spRetainedBytes sp))
                , fromMaybe "-" (spAnchor sp)
                ]
              | sp <- take topN (hsSpines skel)
              ]
        h2 ("Top Exact Dominators (Top " <> tshow topN <> ")")
        tableAligned
          [(AlignLeft, "Address"), (AlignLeft, "Closure"), (AlignLeft, "Type"), (AlignRight, "Exclusively Retained")]
          [ [ code (fmtClosureId cid)
            , maybe (fmtClosureId cid) nodeTitle (M.lookup cid (snapNodes snap))
            , maybe "-" cnType (M.lookup cid (snapNodes snap))
            , fmtBytes (fromIntegral retB)
            ]
          | (cid, retB) <- take topN (dtTopDominators dt)
          ]
        h2 ("Merkle Structural Duplicate Waste (" <> tshow (length dups) <> " groups)")
        if null dups
          then para "No duplicated identical immutable sub-DAGs detected."
          else
            tableAligned
              [(AlignLeft, "Closure"), (AlignLeft, "Type"), (AlignRight, "Copies"), (AlignRight, "Reclaimable Waste")]
              [ [ code (dgName dg)
                , code (dgType dg)
                , fmtInt (dgCount dg)
                , fmtBytes (fromIntegral (dgWasteBytes dg))
                ]
              | dg <- take topN dups
              ]

-- ---------------------------------------------------------------------------
-- 11. Causal Critical Path & Extended Eventlog Diagnostics

criticalPathReport :: FilePath -> Maybe Word32 -> Maybe Text -> IO (Either Text Text)
criticalPathReport evPath mTid mSpanName = do
  eTl <- loadEventlog evPath
  pure case eTl of
    Left err -> Left ("Failed to load eventlog: " <> T.pack err)
    Right tl -> Right (formatCriticalPath evPath tl mTid mSpanName)

formatCriticalPath :: FilePath -> Timeline -> Maybe Word32 -> Maybe Text -> Text
formatCriticalPath evPath tl mTid mSpanName =
  let rns = runnableStats tl
      sp = tlSparks tl
      conc = tlConcGC tl
      nm = tlNonmoving tl
      lags = take 8 (sortOn (Down . slSyncWaitNs) [l | l <- V.toList (tlGCSyncLags tl), slSyncWaitNs l > 0])
      mTargetSpan = case mSpanName of
        Just q ->
          let ql = T.toLower (T.strip q)
              matched = sortOn (\u -> Down (usEnd u - usStart u)) [u | u <- V.toList (tlUserSpans tl), ql `T.isInfixOf` T.toLower (usName u)]
           in listToMaybe matched
        Nothing ->
          listToMaybe (sortOn (\u -> Down (usEnd u - usStart u)) (V.toList (tlUserSpans tl)))
      mCpr = case (mTid, mTargetSpan) of
        (Just tid, _) -> Just ("Thread " <> tshow tid, analyzeCriticalPath tl tid (tlStart tl) (tlEnd tl))
        (Nothing, Just u) -> Just ("Span " <> usName u, analyzeSpanCriticalPath tl u)
        (Nothing, Nothing) ->
          case sortOn (Down . tiRunTime) (M.elems (tlThreads tl)) of
            ti : _ -> Just (threadName ti, analyzeCriticalPath tl (tiId ti) (tlStart tl) (tlEnd tl))
            [] -> Nothing
   in renderMD do
        h1 ("Stronghead Causal Critical Path & Runtime Diagnostics: " <> T.pack (takeFileName evPath))
        whenJust mCpr \(lbl, cpr) -> do
          let tot = max 1 (fromIntegral (cpEnd cpr - cpStart cpr) :: Double)
              pctOf ns = fmtPct (fromIntegral ns / tot)
          h2 ("Critical Path Attribution: " <> lbl)
          field "Window" (fmtNs (fromIntegral (cpEnd cpr - cpStart cpr)))
          field "Own CPU" (fmtNs (fromIntegral (cpOwnCpuNs cpr)) <> " (" <> pctOf (cpOwnCpuNs cpr) <> ")")
          field "Blocker CPU" (fmtNs (fromIntegral (cpBlockerCpuNs cpr)) <> " (" <> pctOf (cpBlockerCpuNs cpr) <> ")")
          field "STW GC" (fmtNs (fromIntegral (cpGCNs cpr)) <> " (" <> pctOf (cpGCNs cpr) <> ")")
          field "Runnable Queue Wait" (fmtNs (fromIntegral (cpRunnableNs cpr)) <> " (" <> pctOf (cpRunnableNs cpr) <> ")")
          field "External / IO Wait" (fmtNs (fromIntegral (cpExternalNs cpr)) <> " (" <> pctOf (cpExternalNs cpr) <> ")")
          field "Causal Wakeup/Blackhole Hops" (tshow (length (cpCausalEdges cpr)))
          whenJust (cpTopBlocker cpr) \(btid, bns, reason) ->
            field "Top Blocker Thread" (bold ("Thread " <> tshow btid) <> " (" <> fmtNs (fromIntegral bns) <> " via " <> reason <> ")")
        unless (null lags) $ do
          h2 "STW Rendezvous Stragglers"
          tableAligned
            [(AlignRight, "GC #"), (AlignRight, "Sync Wait"), (AlignLeft, "Caps"), (AlignLeft, "Straggler Thread")]
            [ [ tshow (slCollection l)
              , fmtNs (fromIntegral (slSyncWaitNs l))
              , tshow (slFirstCap l) <> " -> " <> tshow (slLastCap l)
              , maybe "unknown" (\t -> "Thread " <> tshow t) (slStragglerThread l)
              ]
            | l <- lags
            ]
        when (rnCount rns > 0) $ do
          h2 "Scheduler Runnable Latency"
          field "p50 / p95 / p99 / max" (fmtNs (fromIntegral (rnP50Ns rns)) <> " / " <> fmtNs (fromIntegral (rnP95Ns rns)) <> " / " <> fmtNs (fromIntegral (rnP99Ns rns)) <> " / " <> fmtNs (fromIntegral (rnMaxNs rns)))
          field "Migrations / Steals" (fmtInt (rnMigrations rns) <> " / " <> fmtInt (rnSteals rns))
        when (ssCreated sp > 0) $ do
          h2 "Parallel Sparks"
          field "Created / Converted / Fizzled / Dud / Overflow / GC'd"
            (T.intercalate " / " (map fmtInt [ssCreated sp, ssConverted sp, ssFizzled sp, ssDud sp, ssOverflowed sp, ssGCd sp]))
        when (not (V.null conc) || nmRemSetFlushes nm > 0) $ do
          h2 "Non-Moving Concurrent GC (+RTS -xn)"
          field "Concurrent Phases" (fmtInt (V.length conc))
          field "Upd-Rem-Set Flushes" (fmtInt (nmRemSetFlushes nm))
          field "Pruned Segments / Live Blocks" (fmtInt (nmPrunedSegments nm) <> " / " <> fmtInt (nmLiveBlocks nm))
        unless (V.null (tlMemReturns tl)) $ do
          let mr = V.last (tlMemReturns tl)
          h2 "OS Memory Return (MemReturn)"
          field "Current / Needed / Returned (MiB)" (tshow (mrCurrent mr) <> " / " <> tshow (mrNeeded mr) <> " / " <> tshow (mrReturned mr))
        unless (V.null (tlUserSpans tl)) $ do
          h2 ("User Spans (" <> fmtInt (V.length (tlUserSpans tl)) <> ")")
          tableAligned
            [(AlignLeft, "Span"), (AlignRight, "Thread"), (AlignRight, "Duration"), (AlignRight, "Allocated")]
            [ [code (usName u), tshow (usThread u), fmtNs (fromIntegral (usEnd u - usStart u)), fmtBytes (usAllocBytes u)]
            | u <- take 12 (sortOn (\u -> Down (usEnd u - usStart u)) (V.toList (tlUserSpans tl)))
            ]
        unless (V.null (tlAlerts tl)) $ do
          h2 ("Runtime Alerts (" <> fmtInt (V.length (tlAlerts tl)) <> ")")
          forM_ (take 10 (V.toList (tlAlerts tl))) \a ->
            bullet (bold ("[" <> taKind a <> "] ") <> taTitle a <> " — " <> taDetail a)

-- ---------------------------------------------------------------------------
-- 12. Compiler-to-Runtime Forensics (Core Smells, STG Captures, Semantic Diff)

coreSmellsReport :: FilePath -> Maybe FilePath -> Maybe FilePath -> Maybe Text -> Int -> IO (Either Text Text)
coreSmellsReport dumpPath mProfPath mBaseDumps mBinding topN = do
  isDir <- doesDirectoryExist dumpPath
  isFile <- doesFileExist dumpPath
  if not isDir && not isFile
    then pure (Left ("Dump path not found: " <> T.pack dumpPath))
    else do
      weights <- case mProfPath of
        Nothing -> pure M.empty
        Just pp -> do
          ePf <- loadProf pp
          pure case ePf of
            Left _ -> M.empty
            Right pf ->
              M.fromListWith (+)
                [ (nameKey (ctName ct), ctTime ct + ctAlloc ct)
                | ct <- costTotals (pfRoots pf)
                ]
      arts <-
        if isDir
          then discoverArtifacts dumpPath
          else case kindForPath dumpPath of
            Just KindCore -> pure [ModuleArtifacts (T.pack (takeBaseName dumpPath)) Nothing (Just dumpPath) Nothing]
            Just KindStg -> pure [ModuleArtifacts (T.pack (takeBaseName dumpPath)) Nothing Nothing (Just dumpPath)]
            _ -> pure []
      loadedMods <- forM arts \ma -> do
        mCore <- maybe (pure Nothing) (\p -> either (const Nothing) Just <$> loadCodeFile KindCore p) (maCore ma)
        mStg <- maybe (pure Nothing) (\p -> either (const Nothing) Just <$> loadCodeFile KindStg p) (maStg ma)
        pure (maModule ma, mCore, mStg)
      let allSmells = concatMap (\(_, mc, ms) -> Forensics.detectCoreSmells mc ms weights) loadedMods
          allCaptures = concatMap (\(_, _, ms) -> maybe [] Forensics.stgClosureCaptures ms) loadedMods
          qBind = fmap (nameKey . T.strip) mBinding
          filteredSmells = maybe allSmells (\b -> filter (\s -> nameKey (Forensics.csBinding s) == b) allSmells) qBind
          filteredCaptures = maybe allCaptures (\b -> filter (\c -> Forensics.scEnclosing c == b) allCaptures) qBind
      semDiffs <- case (mBaseDumps, qBind) of
        (Just baseDir, Just bKey) -> do
          baseOk <- doesDirectoryExist baseDir
          if not baseOk
            then pure []
            else do
              bArts <- discoverArtifacts baseDir
              concat <$> forM bArts \ba ->
                case (maCore ba, listToMaybe [mc | (m, Just mc, _) <- loadedMods, m == maModule ba]) of
                  (Just bp, Just curCf) -> do
                    eBase <- loadCodeFile KindCore bp
                    pure case eBase of
                      Right baseCf -> maybe [] pure (Forensics.semanticBindingDiff baseCf curCf bKey)
                      Left _ -> []
                  _ -> pure []
        _ -> pure []
      optLogs <- if isDir then Forensics.discoverOptLogs dumpPath else pure []
      pure (Right (formatCoreSmells dumpPath filteredSmells filteredCaptures semDiffs (concatMap snd optLogs) topN))

formatCoreSmells
  :: FilePath
  -> [Forensics.CoreSmell]
  -> [Forensics.StgClosureCapture]
  -> [Forensics.SemanticBindingDiff]
  -> [Forensics.OptLogEntry]
  -> Int
  -> Text
formatCoreSmells dumpPath smells captures semDiffs optLogs topN = renderMD do
  h1 ("Stronghead Compiler-to-Runtime Forensics: " <> T.pack (takeFileName dumpPath))
  field "Optimization Smells Detected" (tshow (length smells))
  field "STG Closures with Free-Variable Captures" (tshow (length (filter (not . null . Forensics.scCaptured) captures)))
  unless (null optLogs) $
    field "Simplifier Log Entries" (tshow (length optLogs))
  h2 ("Top Optimization Smells (Top " <> tshow topN <> ")")
  if null smells
    then para "No Core/STG optimization smells detected."
    else forM_ (take topN smells) \sm -> do
      h3 (Forensics.smellKindTitle (Forensics.csKind sm) <> " in " <> code (Forensics.csModule sm <> "." <> Forensics.csBinding sm))
      field "Line" (tshow (Forensics.csLine sm + 1))
      field "Weight" (fmtFixed 2 (Forensics.csWeight sm))
      para (Forensics.csSummary sm)
      field "Remediation" (Forensics.csRemediation sm)
  let nonEmptyCaps = filter (not . null . Forensics.scCaptured) captures
  unless (null nonEmptyCaps) $ do
    h2 ("Static STG Closure Captures (Top " <> tshow topN <> ")")
    tableAligned
      [(AlignLeft, "Enclosing"), (AlignLeft, "Binder"), (AlignLeft, "Flag"), (AlignLeft, "Captured FVs")]
      [ [ code (Forensics.scEnclosing c)
        , code (Forensics.scBinder c)
        , code case Forensics.scFlag c of
            Forensics.StgUpdateable -> "\\u"
            Forensics.StgSingleEntry -> "\\s"
            Forensics.StgReentrant -> "\\r"
            Forensics.StgUnknown -> "?"
        , code (T.intercalate ", " (Forensics.scCaptured c))
        ]
      | c <- take topN nonEmptyCaps
      ]
  forM_ semDiffs \sd -> do
    h2 ("Alpha-Normalized Semantic Core Diff: " <> code (Forensics.sbdModule sd <> "." <> Forensics.sbdKey sd))
    field "Added / Removed Lines"
      (tshow (Forensics.sbdAdded sd) <> " / " <> tshow (Forensics.sbdRemoved sd))
    let changed =
          [ case dl of
              Forensics.DiffAdd t -> "+ " <> t
              Forensics.DiffDel t -> "- " <> t
              Forensics.DiffSame t -> "  " <> t
          | dl <- Forensics.sbdLines sd
          , case dl of Forensics.DiffSame _ -> False; _ -> True
          ]
    unless (null changed) $ codeBlock "diff" (T.unlines (take 60 changed))

-- ---------------------------------------------------------------------------
-- 13. Live Cross-Platform CloneStack & ghc-heap Probes (Agent v3)

liveStacksReport :: Maybe String -> Maybe Int -> Bool -> Int -> IO (Either Text Text)
liveStacksReport mHost mPort includeProbes timeoutMs = do
  targetPair <- case (mHost, mPort) of
    (Nothing, Nothing) -> do
      beacons <- discoverAgents
      pure case beacons of
        b : _ -> Right (T.unpack (abHost b), abPort b, Just (abProgram b, abPid b))
        [] -> Right ("127.0.0.1", 4777, Nothing)
    _ -> pure (Right (fromMaybe "127.0.0.1" mHost, fromMaybe 4777 mPort, Nothing))
  case targetPair of
    Left err -> pure (Left err)
    Right (host, port, mDiscovered) -> do
      stacksRef <- newIORef ([] :: [LiveThreadStack])
      probesRef <- newIORef ([] :: [LiveProbeNode])
      doneVar <- newEmptyMVar
      let wantReplyId = if includeProbes then (2 :: Int) else 1
          onEvent = \case
            LiveConnected box -> do
              sendCommand box 1 CmdStacks
              when includeProbes (sendCommand box 2 (CmdProbes Nothing))
            LiveStack ts -> modifyIORef' stacksRef (ts :)
            LiveProbe pn -> modifyIORef' probesRef (pn :)
            LiveReply rid _ | rid == wantReplyId -> void (try @SomeException (putMVar doneVar (Right ())))
            LiveClosed msg -> void (try @SomeException (putMVar doneVar (Left msg)))
            _ -> pure ()
          void m = m >> pure ()
      res <- withAsync (connectLive host port onEvent) \_ ->
        withAsync (threadDelay (timeoutMs * 1000) >> void (try @SomeException (putMVar doneVar (Right ())))) \_ ->
          takeMVar doneVar
      stacks <- reverse <$> readIORef stacksRef
      probes <- reverse <$> readIORef probesRef
      case res of
        Left err | null stacks && null probes ->
          pure (Left ("Could not reach Stronghead.Agent v3 at " <> T.pack host <> ":" <> tshow port <> " (" <> err <> ")."))
        _ ->
          pure $ Right $ renderMD do
            h1 ("Stronghead Live Stacks & Probes (" <> T.pack host <> ":" <> tshow port <> ")")
            whenJust mDiscovered \(prog, pid) ->
              field "Auto-Discovered Process" (prog <> " (pid " <> tshow pid <> ")")
            h2 ("Live Thread Stacks (" <> tshow (length stacks) <> " threads)")
            if null stacks
              then para "No thread stacks returned (ensure target links stronghead:agent v3 and compiles with -finfo-table-map)."
              else forM_ (take 24 stacks) \ts -> do
                h3 ("Thread " <> tshow (ltsTid ts) <> " (" <> ltsStatus ts <> ")")
                if null (ltsFrames ts)
                  then para "No decoded frames."
                  else codeBlock_ $ T.unlines
                    [ tshow idx <> ": "
                        <> (if T.null (lfeFunction f) then lfeClosureType f else lfeFunction f)
                        <> (if T.null (lfeModule f) then "" else " [" <> lfeModule f <> "]")
                        <> (if T.null (lfeSrcLoc f) then "" else " @ " <> lfeSrcLoc f)
                    | (idx, f) <- zip [0 :: Int ..] (take 25 (ltsFrames ts))
                    ]
            when includeProbes $ do
              h2 ("In-Process ghc-heap Probes (" <> tshow (length probes) <> " closure nodes)")
              if null probes
                then para "No registered probes. Use Stronghead.Probe.probeRef / probeTVar to inspect live values without ghc-debug-stub."
                else
                  tableAligned
                    [(AlignLeft, "Probe"), (AlignLeft, "Name"), (AlignLeft, "Type"), (AlignCenter, "Thunk?"), (AlignRight, "Bytes"), (AlignLeft, "Source")]
                    [ [ code (lpnProbe pn)
                      , code (if T.null (lpnName pn) then lpnType pn else lpnName pn)
                      , code (lpnType pn)
                      , if lpnIsThunk pn then bold "THUNK" else "-"
                      , fmtBytes (fromIntegral (lpnBytes pn))
                      , lpnSource pn
                      ]
                    | pn <- take 50 probes
                    ]

-- ---------------------------------------------------------------------------
-- 14. Automated RTS Pareto Sweep

rtsSweepReport :: FilePath -> Maybe Text -> Maybe Text -> IO (Either Text Text)
rtsSweepReport exePath mArgs mGrid = do
  let progArgs = maybe [] (words . T.unpack) mArgs
      grid = case mGrid of
        Nothing -> defaultSweepGrid
        Just g
          | T.null (T.strip g) -> defaultSweepGrid
          | otherwise ->
              let parsed = filter (not . null) (map (words . T.unpack . T.strip) (T.splitOn "," g))
               in if null parsed then defaultSweepGrid else parsed
  res <- runRtsSweep (SweepConfig exePath progArgs grid Nothing)
  pure (fmap renderParetoMarkdown res)

-- ---------------------------------------------------------------------------
-- 15. Declarative Performance Budget Gate (.stronghead-spec)

assertBudgetReport
  :: Text
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> IO (Either Text Text)
assertBudgetReport specInput mEvPath mHpPath mBasePath mDumpPath = do
  isFile <- doesFileExist (T.unpack specInput)
  specTxt <- if isFile then TIO.readFile (T.unpack specInput) else pure specInput
  let spec = parseBudgetSpec specTxt
  mTl <- case mEvPath of
    Nothing -> pure Nothing
    Just p -> either (const Nothing) Just <$> cachedLoadEventlog p
  mHp <- case mHpPath of
    Nothing -> pure Nothing
    Just p -> either (const Nothing) Just <$> loadProfile p
  mBaseHp <- case mBasePath of
    Nothing -> pure Nothing
    Just p -> either (const Nothing) Just <$> loadProfile p
  res <- evaluateBudget spec mTl mHp mBaseHp mDumpPath
  pure (Right (formatBudgetMarkdown res))

-- ---------------------------------------------------------------------------
-- 16. MCP v3 Shared Snapshot / Dump / Probe Loaders

fetchLiveProbesSnapshot :: Int -> IO (Maybe HeapSnapshot)
fetchLiveProbesSnapshot timeoutMs = do
  beacons <- discoverAgents
  let (host, port) = case beacons of
        b : _ -> (T.unpack (abHost b), abPort b)
        [] -> ("127.0.0.1", 4777)
  probesRef <- newIORef ([] :: [LiveProbeNode])
  doneVar <- newEmptyMVar
  let putOnce v = try @SomeException (putMVar doneVar v) >> pure ()
      onEvent = \case
        LiveConnected box -> sendCommand box 1 (CmdProbes Nothing)
        LiveProbe pn -> modifyIORef' probesRef (pn :)
        LiveReply 1 _ -> putOnce ()
        LiveClosed _ -> putOnce ()
        _ -> pure ()
  _ <- withAsync (connectLive host port onEvent) \_ ->
    withAsync (threadDelay (timeoutMs * 1000) >> putOnce ()) \_ ->
      takeMVar doneVar
  probes <- reverse <$> readIORef probesRef
  pure (if null probes then Nothing else Just (snapshotFromProbes "live-probes" probes))

resolveOptionalSnapshot :: Maybe FilePath -> Bool -> IO (Maybe HeapSnapshot)
resolveOptionalSnapshot mTarget includeLiveProbes = do
  mFromTarget <- case mTarget of
    Nothing -> pure Nothing
    Just rawTarget
      | not ghcDebugEnabled -> pure Nothing
      | otherwise -> do
          isFile <- doesFileExist rawTarget
          let dbgTarget
                | isFile = DebugSnapshotFile rawTarget
                | Just (h, p) <- parseHostPort (T.pack rawTarget) = DebugTCP h p
                | otherwise = DebugUnix rawTarget
          either (const Nothing) Just <$> takeSnapshot defaultSnapshotOptions dbgTarget
  case mFromTarget of
    Just snap -> pure (Just snap)
    Nothing
      | includeLiveProbes -> fetchLiveProbesSnapshot 1500
      | otherwise -> pure Nothing

loadDumpPairs :: Maybe FilePath -> IO [(Text, Maybe CodeFile, Maybe CodeFile)]
loadDumpPairs Nothing = pure []
loadDumpPairs (Just root) = do
  isFile <- doesFileExist root
  isDir <- doesDirectoryExist root
  if isFile
    then do
      let k = fromMaybe KindCore (kindForPath root)
      eCf <- loadCodeFile k root
      pure case eCf of
        Right cf ->
          [ ( cfModule cf
            , if k == KindCore then Just cf else Nothing
            , if k == KindStg then Just cf else Nothing
            )
          ]
        Left _ -> []
    else if isDir
      then do
        arts0 <- discoverArtifacts root
        arts <-
          if null arts0
            then do
              let dist = root </> "dist-newstyle"
              ex <- doesDirectoryExist dist
              if ex then discoverArtifacts dist else pure []
            else pure arts0
        forM (take 40 arts) \a -> do
          mc <- traverse (fmap (either (const Nothing) Just) . loadCodeFile KindCore) (maCore a)
          ms <- traverse (fmap (either (const Nothing) Just) . loadCodeFile KindStg) (maStg a)
          pure (maModule a, mc >>= id, ms >>= id)
      else pure []

-- ---------------------------------------------------------------------------
-- 17. One-Call Run-Level Triage Synthesis (stronghead_triage_run)

triageRunReport
  :: Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe Text
  -> IO (Either Text Text)
triageRunReport mEvPath mHpPath mTarget mDumpPath mProfPath mFmt = do
  mTl <- traverse (fmap (either (const Nothing) Just) . cachedLoadEventlog) mEvPath
  mHp0 <- traverse (fmap (either (const Nothing) Just) . loadProfile) mHpPath
  let tl = mTl >>= id
      hp = (mHp0 >>= id) <|> (tl >>= tlHeapProfile)
  mSnap <- resolveOptionalSnapshot mTarget False
  dumpPairs <- loadDumpPairs mDumpPath
  mProf <- traverse (fmap (either (const Nothing) Just) . cachedLoadProf) mProfPath
  let costs = maybe [] (costTotals . pfRoots) (mProf >>= id)
      weightMap =
        M.fromListWith
          max
          [(nameKey (ctName c), max (ctInhTime c) (ctInhAlloc c)) | c <- costs]
      smells =
        concat
          [ Forensics.detectCoreSmells mc ms weightMap
          | (_, mc, ms) <- dumpPairs
          ]
      stgFiles = [s | (_, _, Just s) <- dumpPairs]
      rk =
        fromMaybe (RunKey "?:?") $
          (runKey . tlRun <$> tl) <|> (runKey . hpRun <$> hp)
      findings =
        triageRun
          emptyTriageInput
            { triRunKey = rk
            , triTimeline = tl
            , triHeap = hp
            , triSnapshots = maybe [] pure mSnap
            , triCoreSmells = smells
            , triStgFiles = stgFiles
            }
      labelTxt =
        fromMaybe "Run" $
          (T.pack . takeFileName <$> mEvPath)
            <|> (T.pack . takeFileName <$> mHpPath)
            <|> (T.pack . takeFileName <$> mDumpPath)
      wantsJson = fmap (T.toLower . T.strip) mFmt == Just "json"
  pure . Right $
    if wantsJson
      then formatTriageJson findings
      else formatTriageMarkdown labelTxt findings

-- ---------------------------------------------------------------------------
-- 18. Cross-Artifact Lens Pipe & Pointer-Path Query Engine (stronghead_query)

queryReport
  :: Text
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Bool
  -> Maybe Text
  -> IO (Either Text Text)
queryReport rawQuery mEvPath mHpPath mTarget mDumpPath mProfPath includeLive mFmt =
  case parseQuery rawQuery of
    Left err -> pure (Left ("Invalid Lens Pipe query:\n" <> err))
    Right q -> do
      mTl <- traverse (fmap (either (const Nothing) Just) . cachedLoadEventlog) mEvPath
      mHp0 <- traverse (fmap (either (const Nothing) Just) . loadProfile) mHpPath
      let tl = mTl >>= id
          hp = (mHp0 >>= id) <|> (tl >>= tlHeapProfile)
      mSnap <- resolveOptionalSnapshot mTarget includeLive
      dumpPairs <- loadDumpPairs mDumpPath
      mProf <- traverse (fmap (either (const Nothing) Just) . cachedLoadProf) mProfPath
      let costs = maybe [] (costTotals . pfRoots) (mProf >>= id)
          qBinds = buildQueryBindings dumpPairs costs
          rk =
            fromMaybe (RunKey "?:?") $
              (runKey . tlRun <$> tl) <|> (runKey . hpRun <$> hp)
          qctx =
            emptyQueryContext
              { qcRunKey = rk
              , qcTimeline = tl
              , qcHeap = hp
              , qcSnapshot = mSnap
              , qcSnapshotIndex = 0
              , qcDominators = computeDominators <$> mSnap
              , qcSurvivors = S.empty
              , qcBindings = qBinds
              }
          res = evalQuery qctx q
          wantsJson = fmap (T.toLower . T.strip) mFmt == Just "json"
      pure . Right $
        if wantsJson
          then formatQueryJson res
          else formatQueryMarkdown res

-- ---------------------------------------------------------------------------
-- 19. Automated Strictness, Container & IR Audit (stronghead_strictness_audit)

strictnessAuditReport
  :: Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> Bool
  -> Int
  -> IO (Either Text Text)
strictnessAuditReport mTarget mDumpPath mHpPath includeLive topN = do
  mSnap <- resolveOptionalSnapshot mTarget includeLive
  mHp0 <- traverse (fmap (either (const Nothing) Just) . loadProfile) mHpPath
  let mHp = mHp0 >>= id
  dumpPairs <- loadDumpPairs mDumpPath
  fullLow <- case mDumpPath of
    Nothing -> pure []
    Just root -> do
      isDir <- doesDirectoryExist root
      if isDir then discoverFullArtifacts root else pure []
  lowReports <- forM (take 10 fullLow) \f -> do
    mcmm <- traverse (fmap (either (const Nothing) Just) . loadCodeFile KindCmm) (fmaCmm f)
    masm <- traverse (fmap (either (const Nothing) Just) . loadCodeFile KindAsm) (fmaAsm f)
    let rep = Forensics.analyzeLowLevelIr (mcmm >>= id) (masm >>= id)
    pure (fmaModule f, rep)
  let stgFiles = [s | (_, _, Just s) <- dumpPairs]
      allSmells = concat [Forensics.detectCoreSmells mc ms M.empty | (_, mc, ms) <- dumpPairs]
      misses = Forensics.explainOptimizationMisses allSmells []
      bandCaps = case mHp of
        Nothing -> []
        Just hp ->
          [ (bandName b, caps)
          | b <- V.toList (hpBands hp)
          , let caps = Forensics.bandStaticCaptures stgFiles b
          , not (null caps)
          ]
  pure . Right $ renderMD do
    h1 "Stronghead Strictness, Container & Low-Level IR Audit"
    case mSnap of
      Nothing ->
        para "No heap snapshot or live ghc-heap probes supplied; skipping runtime constructor-slot counterfactuals (pass `target` or `include_live_probes = true` to include)."
      Just snap -> do
        let dt = computeDominators snap
            strictOps = take topN (auditStrictnessOpportunities snap dt)
            containers = take topN (auditContainers snap dt)
        h2 ("Constructor Slot & Mutable Cell Strictness Opportunities (" <> tshow (length strictOps) <> ")")
        if null strictOps
          then para "No lazy constructor slots retaining unevaluated thunks detected."
          else
            tableAligned
              [(AlignLeft, "Parent"), (AlignCenter, "Slot"), (AlignLeft, "Thunk"), (AlignRight, "Instances"), (AlignRight, "Freed Bytes"), (AlignLeft, "Recommended Fix")]
              [ [ code (saParent sa)
                , maybe "-" (("#" <>) . tshow) (saSlot sa)
                , code (saChildThunk sa)
                , fmtInt (saThunkInstances sa)
                , bold (fmtBytes (fromIntegral (saFreedBytes sa)))
                , saPragmaHint sa
                ]
              | sa <- strictOps
              ]
        h2 ("Container Spine vs. Value-Laziness Health (" <> tshow (length containers) <> ")")
        if null containers
          then para "No standard Haskell container spines detected in snapshot."
          else
            tableAligned
              [(AlignLeft, "Container"), (AlignRight, "Spine Nodes"), (AlignRight, "Lazy Thunk Values"), (AlignRight, "Thunk Bytes"), (AlignLeft, "Diagnosis")]
              [ [ code (chContainerKind ch)
                , fmtInt (chSpineNodes ch)
                , fmtInt (chThunkValues ch)
                , fmtBytes (fromIntegral (chThunkValueBytes ch))
                , chDiagnosis ch
                ]
              | ch <- containers
              ]
    unless (null bandCaps) do
      h2 "Heap Profile Bands Joined to Static STG Closure Captures"
      forM_ (take topN bandCaps) \(bName, caps) -> do
        h3 ("Band " <> code bName)
        forM_ (take 4 caps) \c ->
          bullet (code (Forensics.scBinder c) <> " in " <> code (Forensics.scEnclosing c) <> " captures `[" <> T.intercalate ", " (Forensics.scCaptured c) <> "]`")
    unless (null misses) do
      h2 ("Simplifier & Worker/Wrapper Explanations (" <> tshow (length misses) <> ")")
      forM_ (take topN misses) \ex ->
        bullet (bold (exBinding ex) <> " (" <> Forensics.smellKindTitle (Forensics.seSmell ex) <> "): " <> Forensics.seReason ex)
    let nonEmptyLow = [(m, r) | (m, r) <- lowReports, Forensics.carHeapChecks r + Forensics.carSlowCalls r + Forensics.carSpills r > 0]
    unless (null nonEmptyLow) do
      h2 "Cmm & Native Assembly Forensics"
      tableAligned
        [(AlignLeft, "Module"), (AlignRight, "Heap Checks"), (AlignRight, "Stack Checks"), (AlignRight, "Spills"), (AlignRight, "Slow Calls"), (AlignRight, "SIMD Ops")]
        [ [ code m
          , fmtInt (Forensics.carHeapChecks r)
          , fmtInt (Forensics.carStackChecks r)
          , fmtInt (Forensics.carSpills r)
          , fmtInt (Forensics.carSlowCalls r)
          , fmtInt (Forensics.carSimdOps r)
          ]
        | (m, r) <- nonEmptyLow
        ]
  where
    exBinding = Forensics.seBinding

-- ---------------------------------------------------------------------------
-- 20. Multi-Trial Statistical A/B Experiment & CSV Comparison (stronghead_experiment)

experimentReport
  :: Maybe Text
  -> Maybe Text
  -> Int
  -> Int
  -> Maybe FilePath
  -> Maybe FilePath
  -> Maybe FilePath
  -> IO (Either Text Text)
experimentReport mBaseCmd mCandCmd trials warmups mBaseCsv mCandCsv mCwd =
  case (mBaseCmd, mCandCmd, mBaseCsv, mCandCsv) of
    (Just bCmd, Just cCmd, _, _) -> do
      let parseCmd raw =
            let ws = words (T.unpack (T.strip raw))
                (pre, rtsRest) = break (== "+RTS") ws
                rtsFlags = takeWhile (/= "-RTS") (drop 1 rtsRest)
             in case pre of
                  exe : args -> Just (exe, args, rtsFlags)
                  [] -> Nothing
      case (parseCmd bCmd, parseCmd cCmd) of
        (Just bSpec, Just cSpec) -> do
          res <- runStatisticalExperiment warmups trials bSpec cSpec mCwd
          pure $ flip fmap res \(durCmp, allocCmp, liveCmp) -> renderMD do
            h1 "Stronghead Statistical A/B Experiment (Welch's t-Test)"
            fieldCode "Baseline" bCmd
            fieldCode "Candidate" cCmd
            field "Protocol" (tshow warmups <> " warmup(s) + " <> tshow (max 1 trials) <> " interleaved trial(s) per variant")
            tableAligned
              [(AlignLeft, "Metric"), (AlignRight, "Baseline (Mean ± 95% CI)"), (AlignRight, "Candidate (Mean ± 95% CI)"), (AlignRight, "Delta"), (AlignRight, "Welch t"), (AlignCenter, "Verdict")]
              [ fmtCmpRow "Duration" fmtSec durCmp
              , fmtCmpRow "Total Allocated" (fmtBytes . round) allocCmp
              , fmtCmpRow "Peak Live Residency" (fmtBytes . round) liveCmp
              ]
        _ -> pure (Left "Both `baseline_cmd` and `candidate_cmd` must start with a valid executable path.")
    (_, _, Just bCsvPath, Just cCsvPath) -> do
      bTxt <- TIO.readFile bCsvPath
      cTxt <- TIO.readFile cCsvPath
      let bCases = M.fromList [(bcName bc, bc) | bc <- parseBenchCsv bTxt]
          cCases = parseBenchCsv cTxt
      pure . Right $ renderMD do
        h1 "Stronghead Criterion / tasty-bench CSV Comparison"
        fieldCode "Baseline CSV" (T.pack bCsvPath)
        fieldCode "Candidate CSV" (T.pack cCsvPath)
        tableAligned
          [(AlignLeft, "Benchmark"), (AlignRight, "Baseline Mean"), (AlignRight, "Candidate Mean"), (AlignRight, "Delta"), (AlignRight, "Cand Allocated")]
          [ let mb = M.lookup (bcName c) bCases
                dStr = maybe "new" (\b -> fmtChange (bcMeanSec b) (bcMeanSec c)) mb
             in [ code (bcName c)
                , maybe "-" (fmtNs . (* 1e9) . bcMeanSec) mb
                , fmtNs (bcMeanSec c * 1e9)
                , bold dStr
                , maybe "-" fmtBytes (bcAllocBytes c)
                ]
          | c <- take 50 cCases
          ]
    _ ->
      pure (Left "Provide either (`baseline_cmd` and `candidate_cmd`) for a live multi-trial Welch's t-test run, or (`baseline_csv` and `candidate_csv`) to compare Criterion/tasty-bench CSV files.")
  where
    fmtCmpRow label fmtFn ComparisonStats {..} =
      [ bold label
      , fmtFn (tsMean csBaseline) <> " ± " <> fmtFn (tsCi95HalfWidth csBaseline)
      , fmtFn (tsMean csCandidate) <> " ± " <> fmtFn (tsCi95HalfWidth csCandidate)
      , (if csDeltaPct >= 0 then "+" else "") <> fmtPct csDeltaPct
      , fmtFixed 2 csWelchT
      , case csVerdict of
          StatImproved -> bold "IMPROVED"
          StatRegressed -> bold "REGRESSED"
          StatNoise -> "NOISE (within variance)"
      ]

-- ---------------------------------------------------------------------------
-- 21. Stateful Multi-Turn GHCi Debug Session (stronghead_debug_session)

data GhciSessionEntry = GhciSessionEntry
  { gseGhci :: !Ghci.Ghci
  , gsePrevValues :: !(M.Map Text Ghci.Value)
  , gseThunkAges :: !(M.Map Text Int)
  }

ghciSessionsRef :: MVar (M.Map Text GhciSessionEntry)
ghciSessionsRef = unsafePerformIO (newMVar M.empty)
{-# NOINLINE ghciSessionsRef #-}

debugSessionTool
  :: Text
  -> Text
  -> Maybe Text
  -> Maybe FilePath
  -> Maybe String
  -> Int
  -> IO (Either Text Text)
debugSessionTool rawAction sid mArg mCwd mRepl timeoutMs = do
  let act = T.toLower (T.strip rawAction)
  case act of
    "start" -> do
      let replCmd = fromMaybe "cabal repl" mRepl
          workDir = fromMaybe "." mCwd
      started <- Ghci.startGhci workDir replCmd
      case started of
        Left err -> pure (Left ("Failed to start GHCi session '" <> sid <> "': " <> err))
        Right (g, banner) -> do
          _ <- Ghci.ghciCommandTimeout g timeoutMs ":set +s"
          modifyMVar ghciSessionsRef \m -> do
            for_ (M.lookup sid m) (Ghci.stopGhci . gseGhci)
            pure (M.insert sid (GhciSessionEntry g M.empty M.empty) m, ())
          pure . Right $ renderMD do
            h1 ("GHCi Debug Session Started: " <> code sid)
            fieldCode "Command" (T.pack replCmd)
            fieldCode "Working Directory" (T.pack workDir)
            unless (T.null (T.strip banner)) $ codeBlock_ (T.take 2000 (T.strip banner))
    "stop" -> do
      mOld <- modifyMVar ghciSessionsRef \m -> pure (M.delete sid m, M.lookup sid m)
      case mOld of
        Nothing -> pure (Right ("Session '" <> sid <> "' was not running."))
        Just entry -> do
          Ghci.stopGhci (gseGhci entry)
          pure (Right ("Stopped GHCi session '" <> sid <> "'."))
    "list" -> do
      m <- readMVar ghciSessionsRef
      rows <- forM (M.toList m) \(k, entry) -> do
        alive <- Ghci.ghciAlive (gseGhci entry)
        pure [code k, if alive then "running" else "exited", tshow (M.size (gsePrevValues entry)) <> " tracked bindings"]
      pure . Right $ renderMD do
        h1 "Active GHCi Debug Sessions"
        if null rows
          then para "No active sessions. Call `stronghead_debug_session` with `action = 'start'`."
          else tableAligned [(AlignLeft, "Session ID"), (AlignCenter, "Status"), (AlignRight, "State")] rows
    _ -> do
      m <- readMVar ghciSessionsRef
      case M.lookup sid m of
        Nothing ->
          pure (Left ("No active GHCi session '" <> sid <> "'. Call `stronghead_debug_session` with `action = 'start'` first."))
        Just entry -> runInSession sid entry act mArg timeoutMs
  where
    for_ Nothing _ = pure ()
    for_ (Just x) f = f x

runInSession :: Text -> GhciSessionEntry -> Text -> Maybe Text -> Int -> IO (Either Text Text)
runInSession sid entry act mArg timeoutMs = do
  let g = gseGhci entry
      argTxt = T.strip (fromMaybe "" mArg)
  case act of
    "interrupt" -> do
      Ghci.interruptGhci g
      pure (Right ("Sent process-group interrupt to GHCi session '" <> sid <> "'."))
    "break" -> do
      when (T.null argTxt) $ pure ()
      eOut <- Ghci.ghciCommandTimeout g timeoutMs (":break " <> argTxt)
      pure $ flip fmap eOut \out ->
        let (clean, _) = Ghci.parseGhciStats out
            bps = Ghci.parseBreakpointSet clean
         in renderMD do
              h2 ("Breakpoint (" <> code sid <> ")")
              forM_ bps \(bpNo, mSp) ->
                field ("Breakpoint #" <> tshow bpNo) (maybe "activated" renderSrcSpan mSp)
              codeBlock_ (T.strip clean)
    "bindings" -> inspectSessionBindings sid entry "" Nothing timeoutMs
    "step" -> do
      let cmd = if T.null argTxt then ":step" else ":step " <> argTxt
      stepAndInspect sid entry cmd timeoutMs
    "steplocal" -> stepAndInspect sid entry ":steplocal" timeoutMs
    "continue" -> stepAndInspect sid entry ":continue" timeoutMs
    "force"
      | T.null argTxt -> pure (Left "`action = 'force'` requires `arg` naming a binding or `_tN` thunk.")
      | otherwise -> do
          beforeRaw <- either id fst . fmap Ghci.parseGhciStats <$> Ghci.ghciCommandTimeout g timeoutMs (":print " <> argTxt)
          eForce <- Ghci.ghciCommandTimeout g timeoutMs (":force " <> argTxt)
          case eForce of
            Left err -> pure (Left err)
            Right forceRaw -> do
              let (forceClean, mStats) = Ghci.parseGhciStats forceRaw
              pure . Right $ renderMD do
                h2 ("Forced Thunk / Binding: " <> code argTxt)
                whenJust mStats \st ->
                  field "Evaluation Cost (:set +s)" (fmtSec (Ghci.ssSeconds st) <> " · " <> fmtBytes (Ghci.ssAllocBytes st) <> " allocated")
                field "Before (:print)" (code (T.take 300 (T.strip beforeRaw)))
                h3 "After (:force)"
                codeBlock_ (T.take 2000 (T.strip forceClean))
    "eval" -> do
      eOut <- Ghci.ghciCommandTimeout g timeoutMs argTxt
      pure $ flip fmap eOut \rawOut ->
        let (clean, mStats) = Ghci.parseGhciStats rawOut
         in renderMD do
              h2 ("GHCi Eval (" <> code sid <> ")")
              whenJust mStats \st ->
                field "Step Timing & Allocation" (fmtSec (Ghci.ssSeconds st) <> " · " <> fmtBytes (Ghci.ssAllocBytes st))
              codeBlock_ (T.take 3000 (T.strip clean))
    _ ->
      pure (Left ("Unknown debug_session action '" <> act <> "'. Expected: start, break, continue, step, steplocal, force, bindings, eval, interrupt, stop, list."))

stepAndInspect :: Text -> GhciSessionEntry -> Text -> Int -> IO (Either Text Text)
stepAndInspect sid entry cmd timeoutMs = do
  eOut <- Ghci.ghciCommandTimeout (gseGhci entry) timeoutMs cmd
  case eOut of
    Left err -> pure (Left err)
    Right rawOut -> do
      let (clean, mStats) = Ghci.parseGhciStats rawOut
      inspectSessionBindings sid entry clean mStats timeoutMs

inspectSessionBindings :: Text -> GhciSessionEntry -> Text -> Maybe Ghci.StepStats -> Int -> IO (Either Text Text)
inspectSessionBindings sid entry stepOut mStats timeoutMs = do
  eBinds <- Ghci.ghciCommandTimeout (gseGhci entry) timeoutMs ":show bindings"
  let bindRaw = either (const "") (fst . Ghci.parseGhciStats) eBinds
      parsedBinds = Ghci.parseBindings bindRaw
      curVals =
        M.fromList
          [ (Ghci.gbName b, Ghci.parseValue (fromMaybe "_" (Ghci.gbValue b)))
          | b <- parsedBinds
          ]
      (newAges, xrays) = Ghci.diffValueStep (gsePrevValues entry) curVals (gseThunkAges entry)
      mStop = Ghci.parseStop stepOut
  modifyMVar ghciSessionsRef \m ->
    pure (M.insert sid (entry {gsePrevValues = curVals, gseThunkAges = newAges}) m, ())
  pure . Right $ renderMD do
    h2 ("Debugger State (" <> code sid <> ")")
    whenJust mStop \st ->
      field "Stopped In" (code (Ghci.stName st) <> maybe "" (\sp -> " @ " <> code (renderSrcSpan sp)) (Ghci.stSpan st))
    whenJust mStats \st ->
      field "Step Allocation & Time" (fmtSec (Ghci.ssSeconds st) <> " · " <> fmtBytes (Ghci.ssAllocBytes st))
    unless (T.null (T.strip stepOut)) $ codeBlock_ (T.take 1200 (T.strip stepOut))
    unless (null parsedBinds) do
      h3 "Bindings & Laziness X-Ray"
      tableAligned
        [(AlignLeft, "Binding"), (AlignLeft, "Type"), (AlignCenter, "X-Ray"), (AlignRight, "Thunks"), (AlignLeft, "Value")]
        [ let xr = M.lookup (Ghci.gbName b) xrays
              xTag = case fmap Ghci.vxTransition xr of
                Just Ghci.ThunkCollapsed -> bold "COLLAPSED"
                Just Ghci.ThunkSpawned -> "SPAWNED"
                Just (Ghci.ThunkAging k) -> bold ("AGING ×" <> tshow k)
                _ -> "WHNF/NF"
              tCnt = maybe 0 Ghci.vxThunkCount xr
           in [ code (Ghci.gbName b)
              , code (T.take 40 (Ghci.gbType b))
              , xTag
              , tshow tCnt
              , code (T.take 80 (fromMaybe "_" (Ghci.gbValue b)))
              ]
        | b <- take 40 parsedBinds
        ]

-- ---------------------------------------------------------------------------
-- 22. GUI Co-Pilot Sync Bridge (stronghead_gui_copilot)

guiCopilotTool :: Text -> Maybe Text -> IO (Either Text Text)
guiCopilotTool rawAction mVal = do
  let act = T.toLower (T.strip rawAction)
      val = T.strip (fromMaybe "" mVal)
  if act `notElem` ["focus", "query", "open", "triage"]
    then pure (Left ("Unknown gui_copilot action '" <> act <> "'. Expected: focus, query, open, triage."))
    else do
      let line = if T.null val then act else act <> " " <> val
      res <- sendCopilotCommand line
      pure case res of
        Left err -> Left err
        Right cmdFile ->
          Right ("Dispatched GUI co-pilot command `" <> line <> "` via `" <> T.pack cmdFile <> "`.")
