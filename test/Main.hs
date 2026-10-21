-- | Parser and analysis tests. No framework: each check prints a line and
-- the run fails if any check failed.
module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, unless, void)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Either (fromRight, isLeft)
import Data.Foldable (for_)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.List ((!?))
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import GHC.RTS.Events (EventInfo (..), HeapProfBreakdown (..), HeapProfFlags (..), ThreadStopStatus (..))
import GHC.RTS.Events qualified as E
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as NB
import Stronghead.Agent (AgentConfig (..), defaultAgentConfig, startAgent)
import Stronghead.Attach
import Stronghead.Clock
import Stronghead.Debug
import Stronghead.Diff
import Stronghead.Dump
import Stronghead.Eventlog (emptyBuilder, feedEvent, snapshot)
import Stronghead.Eventlog.Model
import Stronghead.Format (fmtTick)
import Stronghead.Fuzzy
import Stronghead.Ghci
import Stronghead.HeapProfile
import Stronghead.Join
import Stronghead.Live
import Stronghead.Next
import Stronghead.Prof
import Stronghead.Ref
import Stronghead.Run
import Stronghead.Session
import Stronghead.TextFile (readTextFile, writeTextFile)
import Stronghead.Source
import Stronghead.Watch
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath (pathSeparator, (</>))

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "  ok   " else "  FAIL ") <> name)
        unless ok (modifyIORef' failures (+ 1))

  putStrLn "axis ticks"
  check "the origin is 0" (fmtTick 5e8 0 == "0")
  check "whole seconds drop their decimals" (fmtTick 1e9 2e9 == "2 s")
  check "half-second steps keep one decimal" (map (fmtTick 5e8) [5e8, 1e9, 1.5e9] == ["500 ms", "1 s", "1.5 s"])
  check "fine steps keep what they need" (fmtTick 2e7 1.02e9 == "1.02 s" && fmtTick 5e2 2.5e3 == "2.5 µs")

  putStrLn "heap profiles"
  case parseHp hpText of
    Left e -> check ("parseHp: " <> e) False
    Right hp -> do
      check "three samples" (VU.length (hpTimes hp) == 3)
      check "bands sorted by peak" (map bandName (V.toList (hpBands hp)) == ["THUNK_2_0", "(:)", "ARR_WORDS"])
      check "breakdown from JOB flags" (hpBreakdown hp == "closure type")
      check "run from JOB and DATE" (hpRun hp == (runFromArgs ["prog", "+RTS", "-hT", "-i0.1"]) {riStarted = Just "Mon Oct 19 21:49 2026"})
      check ".hp times count from zero" (hpStart hp == 0 && hpOffset hp 0 == 0.1)
      check "missing band is zero" (bandValues (hpBands hp V.! 2) VU.! 0 == 0)
      check "thunk classified" (bandKind (hpBands hp V.! 0) == KindThunk)
      case leakSuspects hp of
        s : _ -> check "growing thunk band is the top suspect" (susBand s == 0 && susSurvived s == 2)
        [] -> check "a leak suspect is found" False
  case parseHp ccsHp of
    Left e -> check ("parseHp ccs: " <> e) False
    Right hp -> do
      let tree = retainTree hp 0
      check "cost-centre stacks are structured" (looksStructured hp)
      check "tree rooted at the outermost frame" (map rtName (rtChildren tree) == ["MAIN"])
      check "bytes add up through the tree" (rtBytes tree == 300)

  putStrLn "cost-centre profiles"
  case parseProf profText of
    Left e -> check ("parseProf: " <> e) False
    Right pf -> do
      let flat = flattenCC (pfRoots pf)
      check "five cost centres" (length flat == 5)
      check "nesting from indentation" (map fst flat == [0, 1, 2, 1, 1])
      check "a source with spaces keeps the columns after it" (fmap (ccTime . snd) (flat !? 4) == Just 0.2 && fmap (ccNo . snd) (flat !? 4) == Just 5)
      check "inherited time parsed" (fmap (ccInhTime . snd) (flat !? 2) == Just 60.5)
      check "cost-centre spans parsed" $
        map (ccLoc . snd) flat
          == [SrcBuiltIn, SrcAt (SrcSpan "Main.hs" 9 1 9 20), SrcAt (SrcSpan "Main.hs" 5 1 5 20), SrcEntireModule, SrcNoLocation]
      check "run from the header" $
        runFromProfHeader (pfHeader pf) == (runFromArgs ["prog", "+RTS", "-p", "-RTS"]) {riStarted = Just "Fri Oct 16 21:49 2026"}

  putStrLn "dumps"
  check "strip unique" (stripUnique "x_a1Bc" == "x" && stripUnique "sat_s2xz" == "sat")
  check "keep snake_case" (stripUnique "foo_bar" == "foo_bar")
  check "strip qualifier" (stripQualifier "GHC.Base.map" == "map" && stripQualifier "Main.$wgo" == "$wgo")
  check "name key drops worker prefix" (nameKey "Main.$wgo_s1x2" == "go")
  let core = parseCodeFile KindCore "Main.dump-simpl" coreText
      names = map bdName (V.toList (cfBindings core))
  check "core bindings found" (names == ["Main.$wgo", "Main.go", "Main.main"])
  let worker = cfBindings core V.! 0
      wrapper = cfBindings core V.! 1
  check "worker arity" (nArity (bdNotes worker) == Just 2)
  check "worker is recursive" (nRecursive (bdNotes worker))
  check "worker uses unboxed ops" (nUnboxedOps (bdNotes worker) > 0)
  check "wrapper inline pragma" (nInline (bdNotes wrapper) == Just "INLINE[final]")
  check "wrapper boxes" (not (null (nBoxing (bdNotes wrapper))))
  check "keys group worker and wrapper" (M.lookup "go" (cfByKey core) == Just [0, 1])
  let (toks, _) = tokenizeLine KindCore False "  case ds_d1 of { I# x_a1 -> x_a1 +# 1# }"
  check "tokenizer finds keyword, con and primop" $
    [tkText t | t <- toks, tkKind t == TkKeyword] == ["case", "of"]
      && any (\t -> tkKind t == TkCon && tkText t == "I#") toks
      && any (\t -> tkText t == "+#") toks
  let src = parseCodeFile KindSource "Main.hs" "module Main where\n\ngo :: Int -> Int\ngo 0 = 0\ngo n = go (n - 1)\n\nmain = print (go 3)\n"
  check "source bindings merge signature and equations" (map bdKey (V.toList (cfBindings src)) == ["go", "main"])

  putStrLn "source spans"
  check "simple span" (parseSrcSpan "src/Main.hs:12:3-20" == Just (SrcSpan "src/Main.hs" 12 3 12 20))
  check "multi-line span" (parseSrcSpan "Main.hs:(4,1)-(6,9)" == Just (SrcSpan "Main.hs" 4 1 6 9))
  check "point span" (parseSrcSpan "Main.hs:7:5" == Just (SrcSpan "Main.hs" 7 5 7 5))
  check "windows path span" (parseSrcSpan "C:\\x\\Main.hs:7:5-9" == Just (SrcSpan "C:\\x\\Main.hs" 7 5 7 9))
  check "windows path multi-line span" (parseSrcSpan "C:\\x\\Main.hs:(4,1)-(6,9)" == Just (SrcSpan "C:\\x\\Main.hs" 4 1 6 9))
  check "not spans" (all ((== Nothing) . parseSrcSpan) ["Main.hs", "Main.hs:7", "Main.hs:7:5x", ":7:5", "Main.hs:(4,1)-(6,9"])
  check "placeholders" $
    map parseSrcLoc ["<built-in>", "<entire-module>", "<no location info>", "", ":", "<wired into compiler>"]
      == [SrcBuiltIn, SrcEntireModule, SrcNoLocation, SrcNoLocation, SrcNoLocation, SrcUnhelpful "<wired into compiler>"]
  check "render round trip" $
    all
      (\t -> renderSrcLoc (parseSrcLoc t) == t)
      ["a/B.hs:1:2", "a/B.hs:1:2-9", "C:\\a\\B.hs:(1,2)-(3,4)", "<built-in>", "<entire-module>", "<no location info>"]

  putStrLn "source index"
  let root = if pathSeparator == '\\' then "C:\\proj" else "/proj"
      file = foldl (</>) root
      runHs = file ["src", "Stronghead", "Run.hs"]
      ix =
        mkSourceIndex
          [root]
          [runHs, file ["app", "Main.hs"], file ["sample", "Main.hs"], file ["src", "Data", "Foo.hs"]]
  check "absolute path" (resolveSource ix Nothing (T.pack runHs) == Just (Resolved runHs ByAbsolute))
  check "root-relative path" (resolveSource ix Nothing "src/Stronghead/Run.hs" == Just (Resolved runHs ByRoot))
  check "suffix of another machine's path" $
    resolveSource ix Nothing "/home/ci/build/src/Stronghead/Run.hs" == Just (Resolved runHs (BySuffix 3))
      && resolveSource ix Nothing "D:\\a\\src\\Stronghead\\Run.hs" == Just (Resolved runHs (BySuffix 3))
  check "longest suffix wins" (resolveSource ix Nothing "x/sample/Main.hs" == Just (Resolved (file ["sample", "Main.hs"]) (BySuffix 2)))
  check "a tie is ambiguous" (resolveSource ix Nothing "Main.hs" == Nothing)
  check "module name as a last resort" (resolveSource ix (Just "Data.Foo") "elsewhere/Gone.hs" == Just (Resolved (file ["src", "Data", "Foo.hs"]) ByModule))
  check "module must match its whole path" (resolveModule ix "Other.Foo" == Nothing)
  check "unknown file" (resolveSource ix Nothing "Nope.hs" == Nothing)

  putStrLn "ghci"
  check "stop parsed" (fmap stName (parseStop "Stopped in Main.go, Main.hs:5:8-21\n_result :: Int = _") == Just "Main.go")
  check "breakpoint set" (map fst (parseBreakpointSet "Breakpoint 3 activated at Main.hs:5:8-21") == [3])
  let bs = parseBindings "_result :: Int = _\nn :: Int = 3\nxs :: [Int] = 1 : _"
  check "bindings parsed" (map gbName bs == ["_result", "n", "xs"])
  check "thunk detection" (map (valueIsThunk . gbValue) bs == [True, False, False])
  check "string literal" (haskellString "a\"b\n" == "\"a\\\"b\\n\"")

  putStrLn "live protocol"
  check "hello" (parseLine "H prog 42 4" == LHello "prog" 42 4)
  check "stats" (parseLine "S gcs=3 allocated_bytes=1024" == LStats (M.fromList [("gcs", 3), ("allocated_bytes", 1024)]))
  check "thread" (parseLine "T 7 BlockedOnMVar worker 1" == LThread (LiveThread 7 "BlockedOnMVar" "worker 1" Nothing))
  check "program clock from elapsed_ns" (frameClock 9 (M.fromList [("rts_stats", 1), ("elapsed_ns", 1.5e9)]) == (1500000000, True))
  check "init time added" (frameClock 9 (M.fromList [("rts_stats", 1), ("elapsed_ns", 1.5e9), ("init_elapsed_ns", 1.3e6)]) == (1501300000, True))
  check "arrival time without +RTS -T" (frameClock 2.5 (M.fromList [("rts_stats", 0)]) == (2500000000, False))
  -- v2 lines, which a v1 client reads as LUnknown and skips.
  let infoText = "I version=2 cwd=C:\\work%20dir exe=C:\\bin\\prog.exe args=prog,%2BRTS,-l,-oltrace%2C1.eventlog,-RTS,a%20b heap-profile=hT heap-interval-ms=100 eventlog=1 commands=1"
      info = AgentInfo 2 "C:\\work dir" "C:\\bin\\prog.exe" ["prog", "+RTS", "-l", "-oltrace,1.eventlog", "-RTS", "a b"] (Just "hT") 100 True True
  check "info line" (parseLine infoText == LInfo info)
  check "info line without a heap profile" (fmap aiHeapProfile (case parseLine "I version=2 heap-profile=none eventlog=0" of LInfo i -> Just i; _ -> Nothing) == Just Nothing)
  check "capability line" (parseLine "C 7 2" == LCap 7 2)
  check "reply with a stamp" (parseLine "A 3 ok t=1500000000" == LReply 3 (ReplyOk (Just 1500000000)))
  check "reply without one" (parseLine "A 4 ok" == LReply 4 (ReplyOk Nothing))
  check "error reply" (parseLine "A 5 error unknown command x" == LReply 5 (ReplyError "unknown command x"))
  check "v1 lines unchanged" (parseLine "T 9 running" == LThread (LiveThread 9 "running" "" Nothing) && parseLine "E" == LEnd && parseLine "X whatever" == LUnknown)
  check "percent encoding round trip" (all (\t -> percentDecode (percentEncode t) == t) ["C:\\a b,c=d%e", "/home/zoë/x", "", "plain-path_1.0"])
  check "percent encoding keeps separators out" (not (T.any (`elem` (" ,=" :: String)) (percentEncode "a b,c=d")))
  check "commands rendered" $
    map (renderCommand 7) [CmdMark "two\nlines", CmdGC, CmdCensus, CmdHeapProfiling False, CmdFlush]
      == ["7 mark two lines", "7 gc", "7 census", "7 heap-profiling off", "7 flush"]
  check "the markers commands post" $
    map commandMarker [CmdMark "  deploy ", CmdMark "", CmdGC, CmdHeapProfiling True, CmdFlush]
      == [Just "deploy", Just "mark", Just "stronghead: major GC", Just "stronghead: heap profiling on", Nothing]
  let frame t p = Frame t p True M.empty []
  check "a pause leaves a gap" (frameGaps [frame (0.25 * fromIntegral i) (250000000 * i) | i <- [0 .. 9]] == [] && frameGaps ([frame (0.25 * fromIntegral i) (250000000 * i) | i <- [0 .. 9]] ++ [frame 6 6000000000]) == [(2250000000, 6000000000)])

  putStrLn "attach"
  let v2 = info {aiCwd = "/work", aiArgs = ["prog", "+RTS", "-l", "-RTS"]}
  check "eventlog named after the program" (eventlogPathFor "prog" v2 == Just ("/work" </> "prog.eventlog"))
  check "eventlog from -ol" (eventlogPathFor "prog" v2 {aiArgs = ["prog", "+RTS", "-olout/trace.eventlog", "-RTS"]} == Just ("/work" </> "out/trace.eventlog"))
  check "an absolute -ol stays" (eventlogPathFor "prog" v2 {aiArgs = ["prog", "+RTS", "-ol/tmp/t.eventlog", "-RTS"]} == Just "/tmp/t.eventlog")
  check "-ol after --RTS is the program's" (rtsOption "-ol" ["prog", "--RTS", "-olx"] == Nothing)
  check "stem from -po" (eventlogPathFor "prog" v2 {aiArgs = ["prog", "+RTS", "-poall", "-RTS"]} == Just ("/work" </> "all.eventlog") && hpPathFor "prog" v2 {aiArgs = ["prog", "+RTS", "-poall", "-RTS"]} == Just ("/work" </> "all.hp"))
  check "no eventlog without -l" (eventlogPathFor "prog" v2 {aiEventlog = False} == Nothing)
  check "heap profile when profiling" (hpPathFor "prog" v2 == Just ("/work" </> "prog.hp") && hpPathFor "prog" v2 {aiHeapProfile = Nothing} == Nothing)
  check "socket by pid" (socketFor "prog" 42 ["/tmp/ghc-debug/41-prog", "/tmp/ghc-debug/42-other", "/tmp/ghc-debug/42-prog"] == Just "/tmp/ghc-debug/42-prog")
  tmp <- getTemporaryDirectory
  let dir = tmp </> "stronghead-attach-test"
  createDirectoryIfMissing True dir
  writeFile (dir </> "prog.eventlog") ""
  steps <- probeAttach "prog.exe" 42 v2 {aiCwd = T.pack dir} True (pure [dir </> "42-prog"])
  check "probe finds the eventlog" (map asPath steps == [Just (dir </> "prog.eventlog"), Nothing, Just (dir </> "42-prog"), Just dir, Nothing])
  check "probe says what it skipped" (maybe False (("isn't there" `T.isInfixOf`) . asNote) (steps !? 1) && maybe False (("no dumps" `T.isInfixOf`) . asNote) (steps !? 4))
  away <- probeAttach "prog" 42 v2 {aiCwd = "/no/such/place"} False (pure [])
  check "a directory elsewhere is skipped" (all (isNothing . asPath) away && maybe False (("ghc-debug support" `T.isInfixOf`) . asNote) (away !? 2))
  removeDirectoryRecursive dir

  putStrLn "agent"
  agentChecks check

  putStrLn "eventlog provenance"
  let tl = snapshot (foldl feedEvent emptyBuilder [E.Event t e Nothing | (t, e) <- eventlogFixture])
      prov = InfoProv "sat_s3Pe_info" 15 KindThunk "Int" "main" "Main" (SrcAt (SrcSpan "sample/Main.hs" 82 15 82 40))
  case tlHeapProfile tl of
    Nothing -> check "heap samples found" False
    Just hp -> do
      check "IPE record kept" (fmap bandProv (hpBands hp V.!? 0) == Just (Just prov))
      check "band kind from the IPE record" (fmap bandKind (hpBands hp V.!? 0) == Just KindThunk)
      check "band detail derived from the record" $
        fmap bandDetail (hpBands hp V.!? 0) == Just (Just "sat_s3Pe_info :: Int  (main, Main sample/Main.hs:82:15-40)")
      check "heap samples in program time" (VU.toList (hpTimes hp) == [3.0] && hpStart hp == 1.0)
      check "displayed from the first event" (hpOffset hp 0 == 2.0)
      check "heap profile knows its run" (riPid (hpRun hp) == Just 4242)
  check "cost-centre table with spans" $
    IM.lookup 7 (tlCostCentres tl) == Just (CostCentreInfo 7 "go" "Main" (SrcAt (SrcSpan "sample/Main.hs" 10 1 12 20)) True)
  check "run from the eventlog" $
    tlRun tl
      == emptyRunInfo
        { riProgram = Just "stronghead-sample"
        , riPid = Just 4242
        , riArgs = ["arg"]
        , riRtsFlags = ["-l", "-hi"]
        , riRts = Just "GHC-9.14.1 rts_thr"
        , riWallClock = Just (WallAnchor 1500000000 1760000000000000123)
        }
  check "start wall time" (runStartWall (tlRun tl) == Just 1759999998500000123)
  check "wall clock conversion" $
    case riWallClock (tlRun tl) of
      Just a -> wallToProgram a (programToWall a 7) == Just 7 && wallToProgram a 0 == Nothing
      Nothing -> False

  putStrLn "wakeups"
  let sched = snapshot (foldl feedEvent emptyBuilder [E.Event t e c | (t, c, e) <- schedFixture])
      blockedSpans tid =
        [ s
        | Just ti <- [M.lookup tid (tlThreads sched)]
        , s <- map (trackSpan (tiTrack ti)) [0 .. trackLength (tiTrack ti) - 1]
        , srKind s == kindBlocked
        ]
      woken tid = [M.lookup tid (tlThreads sched) >>= (`wokenBy` s) | s <- blockedSpans tid]
  check "every wakeup is recorded" (fmap (VU.length . tiWakeups) (M.lookup 2 (tlThreads sched)) == Just 2)
  check "the waker runs on the posting capability" (woken 2 == [Just (150, 1), Just (180, 3)])
  check "a wakeup on the same capability names its waker too" (woken 2 !? 1 == Just (Just (180, 3)))
  check "only cross-capability wakeups are track points" $
    map (VU.toList . VU.map (\(t, _, _) -> t)) (V.toList (tlCapPoints sched)) == [[150], []]
  check "a running span has no waker" $
    case M.lookup 2 (tlThreads sched) of
      Just ti -> wokenBy ti (trackSpan (tiTrack ti) 0) == Nothing
      Nothing -> False

  putStrLn "time"
  let rs = rangeStats sched 100 255
  check "range: collections and pauses" (rsCollections rs == [0] && rsPauseTotal rs == 35 && rsLongest rs == Just 0 && rsMajor rs == 0)
  check "range: allocation from the counter" (rsAllocated rs == Just 5000)
  check "range: threads busiest first" (rsThreads rs == [(1, 110), (2, 30), (3, 10)])
  check "range: heap growth" (rsLive rs == (Just 100, Just 300))
  check "range: censuses and markers" (rsCensuses rs == [0] && rsMarkers rs == [0])
  check "range: a pause cut by its edge" (rsPauseTotal (rangeStats sched 100 230) == 10)
  check "census after a collection" (censusAfterCollection sched 0 == Just 0)
  check "collection before a census" (collectionBeforeCensus sched 0 == Just 0)
  check "census times in program time" (censusTimes sched == VU.fromList [240])
  check "nearest sample" (map (nearestIndex (VU.fromList [1, 2, 4])) [-5, 1.4, 2.9, 3.1, 10] == [0, 0, 1, 2, 2])
  check "nearest of nothing" (nearestIndex VU.empty 3 == 0)
  check "step forward and back" $
    let ts = VU.fromList [10, 20, 30]
     in map (\(d, t) -> stepTime d ts t) [(1, 20), (-1, 20), (1, 30), (-1, 10), (1, 0)] == [Just 30, Just 10, Nothing, Nothing, Just 10]
  check "pair censuses by total" (pairByTotals (VU.fromList [0, 100, 250, 400, 90]) (VU.fromList [100, 250, 400]) == [(1, 0), (2, 1), (3, 2)])
  check "a census on one side only is skipped" (pairByTotals (VU.fromList [100, 120, 250]) (VU.fromList [100, 250]) == [(0, 0), (2, 1)])
  let paired = pairedClock [(0.1, 1.5e8), (0.2, 3e8)]
      near a b = abs (a - b) < 1e-3
  check "seconds clock" (toProgramNs (secondsClock False) 1.5 == 1.5e9 && fromProgramNs (secondsClock True) 2e9 == 2)
  check "paired clock is exact at its points" (toProgramNs paired 0.1 == 1.5e8 && toProgramNs paired 0.2 == 3e8)
  check "paired clock interpolates and extrapolates" (near (toProgramNs paired 0.15) 2.25e8 && near (toProgramNs paired 0.3) 4.5e8)
  check "paired clock inverts" (all (\x -> near (fromProgramNs paired (toProgramNs paired x)) x) [0, 0.12, 0.2, 0.5])
  check "exactness" (cmExact paired && not (cmExact (secondsClock False)) && not (cmExact (pairedClock [])))

  putStrLn "runs"
  let fromLog = (runFromArgs ["C:\\bin\\stronghead-sample.exe", "+RTS", "-l", "-RTS"]) {riPid = Just 4242}
      hello = runFromHello "stronghead-sample.exe" 4242
      hpRunInfo = runFromHp "stronghead-sample +RTS -hT" "Mon Oct 19 21:49 2026"
  -- A non-profiled build writes just the program to JOB.
  check "bare JOB" (riProgram (runFromHp "stronghead-sample.exe" "") == Just "stronghead-sample")
  check "program names agree" (riProgram fromLog == Just "stronghead-sample" && riProgram hello == Just "stronghead-sample")
  check "rts options split out" (splitRtsArgs ["a", "+RTS", "-N4", "-RTS", "b", "+RTS", "-l", "--RTS", "+RTS"] == (["a", "b", "+RTS"], ["-N4", "-l"]))
  check "socket name" (runFromSocket "/tmp/ghc-debug/4242-stronghead-sample" == Just (runFromHello "stronghead-sample" 4242))
  check "not a socket name" (runFromSocket "/tmp/ghc-debug/socket" == Nothing)
  check "same pid and program" (matchRuns fromLog hello == SameRun)
  check "no pid on one side" (matchRuns fromLog hpRunInfo == ProbablySameRun && matchRuns hpRunInfo hello == ProbablySameRun)
  check "different pid" (matchRuns fromLog hello {riPid = Just 1} == DifferentRuns)
  check "different program" (matchRuns fromLog (runFromHello "other" 4242) == DifferentRuns)
  check "different start dates" (matchRuns hpRunInfo hpRunInfo {riStarted = Just "Tue Oct 20 09:00 2026"} == DifferentRuns)
  check "unknown program" (matchRuns emptyRunInfo {riPid = Just 4242} hello == DifferentRuns)
  check "merge prefers the first" (mergeRunInfo hpRunInfo hello == hpRunInfo {riPid = Just 4242})
  check "runs grouped: same, probable, different" $
    groupRuns [fromLog, hpRunInfo, runFromHello "other" 1, hello]
      == [[(0, SameRun), (1, ProbablySameRun), (3, SameRun)], [(2, SameRun)]]
  check "a source naming no program is a run of its own" (groupRuns [emptyRunInfo, emptyRunInfo] == [[(0, SameRun)], [(1, SameRun)]])
  check "run key" (runKey fromLog == RunKey "stronghead-sample:4242" && runKey hpRunInfo == RunKey "stronghead-sample:?")
  check "run keys match" $
    runKeysMatch (runKey fromLog) (runKey hello)
      && runKeysMatch (runKey fromLog) (runKey hpRunInfo)
      && runKeysMatch (RunKey "?:?") (runKey hello)
      && not (runKeysMatch (runKey fromLog) (RunKey "stronghead-sample:1"))
      && not (runKeysMatch (runKey fromLog) (RunKey "other:4242"))
      && not (runKeysMatch (RunKey "?:4242") (RunKey "other:?"))

  putStrLn "fuzzy matching"
  check "subsequence across punctuation" (isJust (fuzzyScore "wk3" "worker-3 (14)") && isJust (fuzzyScore "worker 3" "worker-3"))
  check "no match" (fuzzyScore "xyz" "worker-3" == Nothing && fuzzyScore "ba" "ab" == Nothing)
  check "case ignored" (isJust (fuzzyScore "GO" "Main.go"))
  check "empty query matches all" (fuzzyScore "" "anything" == Just 0)
  check "exact beats prefix beats middle" $
    take 2 (map snd (fuzzyRank "go" id ["algorithm", "goal", "go", "lego"])) == ["go", "goal"]
  check "word starts beat scattered letters" $
    map snd (fuzzyRank "mb" id ["number", "map-builder"]) == ["map-builder", "number"]

  putStrLn "refs"
  let rk = RunKey "stronghead-sample:4242"
      refs =
        [ RThread rk 12
        , RInstant rk 1500000000
        , RRange rk 10 20
        , RCollection rk 3
        , RCensus rk 4
        , RMarker rk 0
        , RSource (SrcSpan "C:\\x y\\Main.hs" 4 1 6 9)
        , RBinding "Main" "go"
        , RBinding "Data.Op" "</>"
        , RCostCentre "Main" "main/loop 100%"
        , RBand "THUNK_2_0"
        , RBand "naïve ~ λ"
        , RInfoTable 0x4a2f10
        , RClosure 2 0xdeadbeef
        , RThunkGroup (ThunkKey "plus" Nothing ["main", "counter"])
        , RThunkGroup (ThunkKey "plus" (Just "Main.hs:82:15-40") [])
        , RThunkGroup (ThunkKey "~" (Just "") [""])
        , RBreakpoint "src/Main.hs" 82
        , RModule "Stronghead.Ref"
        ]
  check "ref text form" (refTarget (RThread rk 12) == "thread/stronghead-sample:4242/12")
  check "ref round trip" (all (\r -> parseRefTarget (refTarget r) == Just r) refs)
  check "refs never contain spaces" (not (any (T.any (== ' ') . refTarget) refs))
  check "bad refs rejected" (not (any (isJust . parseRefTarget) ["", "thread/x", "thread/x/abc", "nope/1", "band/%zz", "info/12"]))

  putStrLn "timeline model"
  let tr = mkTrack [SpanRec 0 10 kindRun 1 3 0, SpanRec 10 20 kindGC 0 0 0, SpanRec 30 50 kindRun 2 7 0]
  check "time in run over everything" (timeIn tr 0 0 100 == 30)
  check "time in gc partial window" (timeIn tr 1 15 40 == 5)
  check "span at time" (spanIndexAt tr 35 == Just 2 && spanIndexAt tr 25 == Nothing)
  check "pick nearest span" (pickSpan tr 27 6 == Just 2)

  putStrLn "ghc-debug graph"
  check "host:port" (parseHostPort "localhost:1235" == Just ("localhost", 1235))
  check "bare port" (parseHostPort " 1235 " == Just ("127.0.0.1", 1235))
  check "ipv6 host" (parseHostPort "[::1]:9" == Just ("::1", 9))
  check "bad port" (parseHostPort "host:abc" == Nothing && parseHostPort "host:70000" == Nothing)
  check "closure kinds" (map kindOfClosureType ["THUNK_2_0", "AP", "IND_STATIC", "CONSTR_1_0", "MUT_VAR_DIRTY", "TSO"] == [NThunk, NThunk, NIndirection, NConstr, NMutable, NThread])
  let snapA = leakSnapshot 2
      snapB = leakSnapshot 5
      path = map cnId (retainerPath snapA 8)
  check "shortest retainer path" (path == [1, 2, 3, 4, 5, 6, 8])
  check "unreachable closure has no path" (null (retainerPath snapA 99))
  check "describe names holder thunk and CAF" $
    let d = describePath (retainerPath snapA 8)
     in "held by thunk plus" `T.isInfixOf` d && "rooted at CAF counter" `T.isInfixOf` d
  check "held bytes cover the subtree" (M.lookup 5 (snapHeld snapA) == Just (4 * 16))
  check "held never double counts shared children" (M.lookup 1 (snapHeld snapA) == Just (snapBytes snapA - 16))
  check "thunks keyed by code and path" (M.size (snapThunks snapB) == 2 && sum (map tgCount (M.elems (snapThunks snapB))) == 5)
  case survivingThunks [snapA, snapB] of
    s : rest -> do
      check "growing chain is the top survivor" (svGrowing s && svCounts s == [1, 4] && svSurvived s == 2)
      check "chain head survives without growing" (map svGrowing rest == [False])
      check "survivor ids cover the chain" (M.size (survivorIds snapB (s : rest)) == 5)
    [] -> check "survivors found" False
  check "one snapshot has no survivors" (null (survivingThunks [snapB]))

  putStrLn "joins"
  check "same file across tools" (sameFile "sample\\Main.hs" "C:\\x\\sample\\Main.hs" && sameFile "/p/sample/Main.hs" "sample/Main.hs")
  check "different directories are different files" (not (sameFile "app/Main.hs" "sample/Main.hs"))
  check "span within" (spanWithin (SrcSpan "" 82 42 82 63) (SrcSpan "" 81 37 83 20) && not (spanWithin (SrcSpan "" 81 1 83 20) (SrcSpan "" 81 37 83 20)))
  check "spans overlap" (spanOverlaps (SrcSpan "" 5 1 6 9) (SrcSpan "" 6 3 8 1) && not (spanOverlaps (SrcSpan "" 5 1 5 9) (SrcSpan "" 5 10 5 12)))
  check "the smallest range owns a line" (lineValues [(80, 82, 5.2), (81, 81, 4.9)] == IM.fromList [(80, 5.2), (81, 4.9), (82, 5.2)])
  check "equal ranges add up" (lineValues [(1, 1, 1), (1, 1, 2), (0, 3, 9)] == IM.fromList [(0, 9), (1, 3), (2, 9), (3, 9)])
  case parseProf profText of
    Left e -> check ("parseProf for joins: " <> e) False
    Right pf -> do
      check "a cost-centre stack finds its .prof path" (fmap (map ccName) (profPath ["go", "main", "MAIN"] (pfRoots pf)) == Just ["MAIN", "main", "go"])
      check "a truncated stack matches its prefix" (fmap (map ccName) (profPath ["main", "MAIN"] (pfRoots pf)) == Just ["MAIN", "main"])
      check "a stack the tree lacks does not" (profPath ["nope", "MAIN"] (pfRoots pf) == Nothing && profPath [] (pfRoots pf) == Nothing)
      case costTotals (pfRoots pf) of
        t : _ -> check "cost totals, dearest first" (ctName t == "go" && ctTime t == 60 && ctEntries t == 1000 && ctLoc t == SrcAt (SrcSpan "Main.hs" 5 1 5 20))
        [] -> check "cost totals found" False
  let cc name time inh kids = CostCentre name "Main" "Main.hs:5:1-20" (parseSrcLoc "Main.hs:5:1-20") 0 10 time time inh inh kids
      recursive = costTotals [cc "go" 10 50 [cc "go" 20 40 [cc "go" 20 20 []]]]
  check "recursion sums individual costs, inherited once" (map (\t -> (ctTime t, ctInhTime t, ctEntries t)) recursive == [(50, 50, 30)])
  check "frames of a cost-centre band" (ccsFrames "(12)leaf/go/MAIN" == ["leaf", "go", "MAIN"])
  check "binder table names" (parseTableName "main2_sat_s7qA_info" == Just (TableOfBinder "main2_sat_s7qA"))
  check "constructor table names" (parseTableName "I#_Main_3_con_info" == Just (TableOfCon "I#" "Main" 3) && parseTableName "Bin_Data.Map_8_con_info" == Just (TableOfCon "Bin" "Data.Map" 8))
  check "other symbols are not tables" (parseTableName "main2" == Nothing && parseTableName "_info" == Nothing)
  let stg = parseCodeFile KindStg "dumps/Main.thr.dump-stg-final" stgText
      sites = stgSites stg
      lineOf needle = V.findIndex (needle `T.isInfixOf`) (cfLines stg)
      thunkSpan = SrcSpan "sample/Main.hs" 82 59 82 63
  check "a way-suffixed dump is of its module" (cfModule stg == "Main")
  check "an IPE binder is its STG let" (siteOfTable sites "main2_sat_s7qA_info" Nothing == lineOf "main2_sat_s7qA [Occ")
  check "a constructor table is its numbered site" (siteOfTable sites "I#_Main_3_con_info" Nothing == lineOf "3 GHC.Internal.Types.I#!")
  check "a table without a site falls back to its span" (siteOfTable sites "I#_Main_9_con_info" (Just thunkSpan) == lineOf "src<sample")
  check "another module's constructor is not here" (siteOfTable sites "I#_Other_3_con_info" Nothing == Nothing)
  check "a dump names its source" (dumpSourcePath stg == Just "sample\\Main.hs")
  let prov table kind = InfoProv table 15 kind "Integer" "main2" "Main" (SrcAt (SrcSpan "sample\\Main.hs" 82 59 82 63))
      band name p = Band name (VU.fromList [10, 20]) 20 KindThunk p Nothing
      hiBand = band "0x4fe0" (Just (prov "main2_sat_s7qA_info" KindThunk))
  check "an info-table band is titled by its provenance" (bandTitle hiBand == "thunk in main2 (Main.hs:82)")
  check "a constructor band by its constructor" (bandTitle (band "0x5000" (Just (prov "I#_Main_3_con_info" KindConstructor))) == "I# in main2 (Main.hs:82)")
  check "other bands by name" (bandTitle (band "THUNK_2_0" Nothing) == "THUNK_2_0")
  let srcNode i ty src size = ClosureNode i ty (kindOfClosureType ty) "x" src size [] Nothing []
      thunkAt = srcNode 10 "THUNK_2_0" (Just "sample/Main.hs:82:59-63") 32
  check "a closure matches an info-table band by span" (closureMatchesBand hiBand thunkAt && not (closureMatchesBand hiBand (srcNode 11 "THUNK_2_0" (Just "sample/Main.hs:9:1-3") 32)))
  check "and a closure-type band by type" (closureMatchesBand (band "THUNK_2_0" Nothing) thunkAt && not (closureMatchesBand (band "ARR_WORDS" Nothing) thunkAt))
  let srcSnap =
        mkSnapshot "src" False [1] $
          [ ClosureNode 1 "TSO" NThread "thread 1" Nothing 16 [10, 11, 12] Nothing []
          , thunkAt
          , srcNode 11 "THUNK_2_0" (Just "sample/Main.hs:82:59-63") 48
          , srcNode 12 "CONSTR_1_0" (Just "sample/Main.hs:93:1-4") 16
          ]
  case closuresBySource srcSnap (M.fromList [(11, 2)]) of
    [g, h] -> do
      check "closures grouped by source, biggest first" (sgSpan g == SrcSpan "sample/Main.hs" 82 59 82 63 && sgClosures g == 2 && sgThunks g == 2 && sgBytes g == 80 && sgClosures h == 1)
      check "survivors counted, the biggest kept to reveal" (sgSurvivors g == 1 && sgSample g == 11 && sgSurvivors h == 0)
    gs -> check ("two source groups, not " <> show (length gs)) False
  check "way suffixes dropped from dump names" (map dumpModuleName ["d/Main.thr.dump-stg-final", "Data.Map.dump-simpl", "Foo.p_dyn.dump-simpl", "Main.dump-stg"] == ["Main", "Data.Map", "Foo", "Main"])
  check "editor command template" (editorCommand "code -g {file}:{line}:{col}" "C:\\My Files\\M.hs" 3 0 == "code -g \"C:\\My Files\\M.hs\":3:1")
  check "overlapping roots are not a tie" (fmap rsHow (resolveSource (mkSourceIndex [] ["/p/sample/Main.hs", "/p/sample/Main.hs"]) Nothing "sample/Main.hs") == Just (BySuffix 2))
  check "history steps are spans" $
    parseHistory "-1  : go (Main.hs:5:8-21)\n-2  : main (Main.hs:(10,1)-(12,3))\n<end of history>"
      == [(-1, "go", SrcSpan "Main.hs" 5 8 5 21), (-2, "main", SrcSpan "Main.hs" 10 1 12 3)]

  putStrLn "code samples"
  let prof = snapshot (foldl feedEvent emptyBuilder [E.Event t e c | (t, c, e) <- profFixture])
      samples = V.toList (tlProfSamples prof)
  check "every sample is kept, in time order" (map psTime samples == [110, 110, 120, 120, 130, 130, 150, 170])
  check "a sample belongs to the thread running on its capability" $
    [(psCap s, psThread s) | s <- samples] == [(0, 1), (1, 2), (0, 1), (1, 2), (0, 1), (1, 2), (0, 0), (0, 1)]
  check "the period is the usual gap" (tlSamplePeriod prof == 10)
  check "samples merge into runs of one top frame" $
    [(crStart r, crEnd r, crCostCentre r, crSamples r) | r <- V.toList (tlCodeRuns prof V.! 0)]
      == [(100, 120, 2, 2), (120, 130, 3, 1), (140, 150, 1, 1), (160, 170, 2, 1)]
  check "what ran during a slice" (sliceProfile prof (Just 0) 1 100 140 == CodeProfile 3 0 [(2, 2), (3, 1)])
  check "a thread's CPU profile" (threadProfile prof 1 == CodeProfile 4 0 [(2, 3), (3, 1)])
  check "idle samples are counted, not listed" (codeProfile prof (const True) 100 200 == CodeProfile 8 2 [(2, 3), (3, 3)])
  check "top-frame counts for the whole log" (tlSampleCounts prof == IM.fromList [(1, 2), (2, 3), (3, 3)])
  check "few samples are few" (fewSamples (length samples) && not (fewSamples 500))
  check "cost centres by name" (fmap cciId (costCentreNamed prof "" "leak") == Just 2 && costCentreNamed prof "Other" "leak" == Nothing && costCentreName prof 3 == "Main.show")
  check "no samples, no runs" (V.all V.null (tlCodeRuns sched) && tlSamplePeriod sched == 0)

  putStrLn "ticky"
  let ticky = snapshot (foldl feedEvent emptyBuilder [E.Event t e Nothing | (t, e) <- tickyFixture])
  check "counters summed over samples, most allocation first" $
    [(tcId c, tcEntries c, tcAlloc c, VU.length (tcSeries c)) | c <- V.toList (tlTicky ticky)] == [(3, 1, 828, 2), (1, 1, 496, 1), (2, 0, 0, 0)]
  check "ticky in a range" ([(tcId c, e, a) | (c, e, a) <- tickyBetween ticky 20 30] == [(3, 0, 700), (1, 1, 496)])
  check "a local closure's counter" $
    parseTickyName "main2_sat_s7qL{v} (Main) (fun) in s7qM" == Just (TickyName "main2_sat_s7qL" Nothing "Main" ["fun"] (Just "s7qM"))
  let go3 = parseTickyName "go3{v r9BH} (Stronghead.Agent) (fun)"
      exported = parseTickyName "Main.main2{v r7mA} (fun,se)"
  check "a top-level binder gets its unique back" (fmap tickyBinder go3 == Just "go3_r9BH" && fmap tnModule go3 == Just "Stronghead.Agent")
  check "an exported binder is qualified" (fmap tickyBinder exported == Just "Main.main2" && fmap tnModule exported == Just "Main" && fmap tnFlags exported == Just ["fun", "se"])
  check "the root main counts as Main's" (fmap tnModule (parseTickyName ":Main.main{v 48_1D} (fun)") == Just "Main")
  check "not a counter name" (parseTickyName "main2" == Nothing && parseTickyName "x{y} (M)" == Nothing)
  let names = [tn | c <- V.toList (tlTicky ticky), Just tn <- [parseTickyName (tcName c)]]
      byUnique u = listToMaybe [tn | tn <- names, tickyUnique tn == Just u]
  check "uniques, given or carried" (map tickyUnique names == [Just "s7qL", Just "r7mA", Just "s7qM"])
  check "a nested counter's top-level binding" (map (nameKey . tnName . tickyTopLevel byUnique) names == ["main2", "main2", "main2"])
  check "a counter is its STG let" (siteOfTable sites (maybe "" tickyBinder (parseTickyName "main2_sat_s7qA{v} (Main) (fun) in s7qu") <> "_info") Nothing == lineOf "main2_sat_s7qA [Occ")
  check "an exported counter is its top-level binding" (siteOfTable sites (maybe "" tickyBinder exported <> "_info") Nothing == Just 0)

  putStrLn "thread stacks"
  let frame ty name src ptrs = StackFrame ty name src 0 ptrs
      bare i ty = ClosureNode i ty (kindOfClosureType ty) ty Nothing 16 [] Nothing []
      stackSnap =
        mkSnapshot "stacks" False [1, 30] $
          [ (bare 1 "TSO") {cnName = "thread 7", cnChildren = [2]}
          , (bare 2 "STACK") {cnChildren = [20, 3], cnFrames = [frame "RET_SMALL" "go" (Just "Main.hs:5:3-9") [20], frame "UPDATE_FRAME" "" Nothing [20], frame "UNDERFLOW_FRAME" "" Nothing [3]]}
          , (bare 3 "STACK") {cnFrames = [frame "CATCH_FRAME" "" Nothing [], frame "STOP_FRAME" "" Nothing []]}
          , bare 20 "THUNK_1_0"
          , (bare 30 "TSO") {cnName = "thread 8", cnChildren = [31]}
          , (bare 31 "STACK") {cnFrames = [frame "UNDERFLOW_FRAME" "" Nothing [31]]}
          ]
  check "a thread's stack runs on through older chunks" (map sfType (threadStack stackSnap 1) == ["RET_SMALL", "UPDATE_FRAME", "UNDERFLOW_FRAME", "CATCH_FRAME", "STOP_FRAME"])
  check "a stack that loops ends" (length (threadStack stackSnap 30) == 1)
  check "only a TSO has a stack" (null (threadStack stackSnap 20) && null (threadStack stackSnap 99))
  check "frames in words" (map frameTitle (take 2 (threadStack stackSnap 1)) == ["returns to go", "evaluating a thunk"])
  check "a TSO's thread" (fmap threadOf (M.lookup 1 (snapNodes stackSnap)) == Just (Just 7) && fmap threadOf (M.lookup 2 (snapNodes stackSnap)) == Just Nothing)

  putStrLn "info-table pointers"
  let withInfo n a = n {cnInfo = Just a}
      exactNode = withInfo (srcNode 40 "THUNK_2_0" (Just "sample/Main.hs:9:1-3") 32) 0x4fe0
      spanNode = withInfo thunkAt 0x9999
  check "a band's table is its address" (bandInfoTable hiBand == Just 0x4fe0 && bandInfoTable (band "THUNK_2_0" Nothing) == Nothing)
  check "pointers present: matched exactly" (matchBand hiBand [exactNode, spanNode] == (ByInfoTable, [exactNode]))
  check "no closure has the table: by source, as before" (matchBand hiBand [spanNode] == (BySource, [spanNode]))
  check "no pointers at all: as before" (matchBand hiBand [thunkAt] == (BySource, [thunkAt]) && fst (matchBand (band "THUNK_2_0" Nothing) [thunkAt]) == ByName)

  putStrLn "ghci values"
  check "a named thunk and its type" (parseValue "(_t1::[Int])" == VThunk "_t1" (Just "[Int]"))
  check "a partial list" (parseValue "1 : 2 : (_t2::[Integer])" == VList [VAtom "1", VAtom "2"] (Just (VThunk "_t2" (Just "[Integer]"))))
  check "a whole list" (parseValue "[Just 1,Nothing]" == VList [VCon "Just" [VAtom "1"], VAtom "Nothing"] Nothing)
  check "a constructor with a thunk" (parseValue "Just (_t3::Int)" == VCon "Just" [VThunk "_t3" (Just "Int")])
  check "a type with brackets inside" (parseValue "(_t4::Map.Map Int (Maybe [Int]))" == VThunk "_t4" (Just "Map.Map Int (Maybe [Int])"))
  check "tuples, chars and strings" (parseValue "(1,'a',\"b, c\")" == VTuple [VAtom "1", VAtom "'a'", VAtom "\"b, c\""])
  check "records and holes" (parseValue "P {px = 1.0e-2, py = _}" == VRecord "P" [("px", VAtom "1.0e-2"), ("py", VThunk "_" Nothing)])
  check "negative numbers" (parseValue "Just (-3)" == VCon "Just" [VAtom "-3"])
  check "infix constructors" (parseValue "1 :| [2]" == VCon ":|" [VAtom "1", VList [VAtom "2"] Nothing])
  check "what doesn't parse is kept" (parseValue "(((" == VAtom "(((" && parseValue "<function>" == VAtom "<function>")
  check "render round trip" (all (\t -> renderValue (parseValue t) == t) ["Just (-3)", "1 : 2 : _t2", "(1, 'a')", "[Just 1, Nothing]", "P {px = 1, py = _}"])
  check ":print lines" (parsePrint "xs = 1 : (_t1::[Int])\nys = []\n" == [("xs", VList [VAtom "1"] (Just (VThunk "_t1" (Just "[Int]")))), ("ys", VList [] Nothing)])
  check "named thunks to force" (valueThunks (parseValue "(Just (_t1::Int), _, (_t2::Int))") == ["_t1", "_t2"] && valueHasThunks (parseValue "Just _") && not (valueHasThunks (parseValue "Just 1")))

  putStrLn "sessions"
  let sesRun = RunKey "stronghead-sample:4242"
      session =
        emptySession
          { sesPanel = "heap"
          , sesEventlog = Just "C:\\scratch dir\\stronghead-sample.eventlog"
          , sesFollow = True
          , sesHeapProfile = Just "scratch/stronghead-sample.hp"
          , sesCostCentres = Nothing
          , sesHeapSource = "hp"
          , sesGhcDebug = "/tmp/ghc-debug/4242-stronghead-sample"
          , sesDumps = Just "dist-newstyle/build/x/dumps"
          , sesModule = "Main"
          , sesCodeFiles = ["sample/Main.hs"]
          , sesAgentHost = "127.0.0.1"
          , sesAgentPort = "4777"
          , sesAgentConnected = True
          , sesGhciCommand = "cabal repl stronghead-sample"
          , sesGhciDir = "C:\\Users\\zach\\Dev\\stronghead"
          , sesGhciFile = "sample/Main.hs"
          , sesGhciRunning = False
          , sesPins =
              [ Pin (RThread sesRun 12) "the leaky accumulator\nholds 40 MB = too much"
              , Pin (RBand "THUNK_2_0") ""
              , Pin (RSource (SrcSpan "sample/Main.hs" 82 1 82 40)) "  100% of it, from here  "
              ]
          , sesCursor = Just (sesRun, 1500000000)
          , sesRange = Just (sesRun, 1000, 2000)
          , sesFocus = Just (RBinding "Main" "leak")
          , sesLayouts = [("heap", "(h 0.500 heap.chart inspector)"), ("timeline", "tl.caps")]
          , sesPresets = [("Leak hunt", "(v 0.500 heap.chart core.source)"), ("Mine = mine", "pinboard")]
          , sesPopped = ["pinboard"]
          }
  check "a session round trips" (decodeSession (encodeSession session) == Right session)
  check "the empty session round trips" (decodeSession (encodeSession emptySession) == Right emptySession)
  check "notes keep their line breaks and spaces" $
    T.isInfixOf "pin.1.note = the leaky accumulator%0aholds 40 MB = too much" (encodeSession session)
      && T.isInfixOf "pin.3.note = %20%20100% of it, from here%20%20" (encodeSession session)
  check "paths and references stay readable" $
    T.isInfixOf "eventlog = C:\\scratch dir\\stronghead-sample.eventlog" (encodeSession session)
      && T.isInfixOf "pin.3 = source/sample%2FMain.hs/82/1/82/40" (encodeSession session)
  check "a reference written by hand reads as it is" $
    fmap (map pinRef . sesPins) (decodeSession "version = 1\npin.1 = source/sample%2FMain.hs/82/1/82/1\n") == Right [RSource (SrcSpan "sample/Main.hs" 82 1 82 1)]
  check "a file without a version is not a session" (isLeft (decodeSession "panel = heap\n"))
  check "unknown keys and bad values are skipped" $
    decodeSession "version = 1\nwhatever = 3\npin.1 = nope/1\npin.2 = band/x\nfollow = maybe\ncursor = band/y\n"
      == Right emptySession {sesPins = [Pin (RBand "x") ""]}
  check "pins keep their numbered order" $
    fmap (map pinRef . sesPins) (decodeSession "version = 1\npin.10 = band/c\npin.2 = band/b\npin.1 = band/a\n") == Right [RBand "a", RBand "b", RBand "c"]
  check "values escape and unescape" (all (\v -> unescapeValue (escapeValue v) == v) ["", " a ", "100%", "a\nb\r\n", "  ", "naïve λ", "%41", "%25", "%2", "a%", "%%0a", "%0A x", "\t%20\t", "%2\n"])

  putStrLn "pins"
  let pa = RBand "a"
      pb = RBand "b"
      pc = RBand "c"
      board = [Pin pa "", Pin pb "note", Pin pc ""]
  check "pinning adds at the end, once" (map pinRef (pinToggle pc [Pin pa ""]) == [pa, pc])
  check "pinning a pinned entity unpins it" (pinToggle pb board == [Pin pa "", Pin pc ""])
  check "a pin moves up and down" (map pinRef (pinMove 2 (-1) board) == [pa, pc, pb] && map pinRef (pinMove 0 1 board) == [pb, pa, pc])
  check "and stops at either end" (pinMove 0 (-1) board == board && pinMove 2 1 board == board && pinMove 5 1 board == board)
  let report =
        pinsMarkdown
          "Stronghead pins"
          ["From stronghead-sample, pid 4242."]
          [ PinReport "leaky-accumulator (12)" "thread/stronghead-sample:4242/12" [ReportSection "Timeline" (Just "exact: thread id in the same run") [("ran", "1.2 s, 41% of its life")] [], ReportSection "Live" Nothing [] []] "holds the thunks\nsee line 82"
          , PinReport "THUNK_2_0 *x*" "band/THUNK_2_0" [ReportSection "Heap & leaks" Nothing [("peak", "40 MiB"), ("", "more")] ["retained by an IORef"]] ""
          ]
  check "the report has a heading per pin, its reference as code" $
    all (`T.isInfixOf` report) ["# Stronghead pins", "## 1. leaky-accumulator (12)", "`thread/stronghead-sample:4242/12`", "## 2. THUNK\\_2\\_0 \\*x\\*", "`band/THUNK_2_0`"]
  check "each panel's facts, with how it matched" $
    all (`T.isInfixOf` report) ["- **Timeline** (exact: thread id in the same run)", "  - ran: 1.2 s, 41% of its life", "  - peak: 40 MiB", "  - more", "  - retained by an IORef"]
  check "a panel with nothing to say is left out" (not ("**Live**" `T.isInfixOf` report))
  check "the note as a quote" ("> holds the thunks\n> see line 82" `T.isInfixOf` report)
  check "no pins says so" ("No pins." `T.isInfixOf` pinsMarkdown "Pins" [] [])

  putStrLn "next steps"
  let leakRun = RunKey "stronghead-sample:4242"
      line82 = SrcSpan "sample\\Main.hs" 82 5 82 40
      heapHave = haveNothing {haveTimeline = True, haveHeapCensus = True, haveHeapSource = True}
      leakFacts = [FactLeak 0.82 6, FactThunks, FactAllocated line82, FactInBinding "Main" "main2"]
      leak0 = nextSteps heapHave (RBand "0x1") leakFacts
      doing = map stepDo
  check "leak: the allocation site first, and strongly" $
    case leak0 of
      s : _ -> stepDo s == StepGo (RSource line82) && stepStrong s && "Main.hs:82" `T.isInfixOf` stepReason s
      [] -> False
  check "leak: then the binding, then how to snapshot, then break where allocated" $
    doing leak0 == [StepGo (RSource line82), StepGo (RBinding "Main" "main2"), StepLoad DLiveGraph, StepAct (RBand "0x1") "break where allocated"]
  check "leak: with a ghc-debug target, snapshot twice is an action" $
    StepAct (RBand "0x1") "snapshot twice" `elem` doing (nextSteps heapHave {haveLiveGraph = True} (RBand "0x1") leakFacts)
  let group = RThunkGroup (ThunkKey "sat_s7qA" (Just "sample\\Main.hs:82:5-40") ["IORef"])
      holder = RThread leakRun 12
      leak1 = nextSteps heapHave {haveLiveGraph = True, haveSnapshots = 2} (RBand "0x1") (leakFacts ++ [FactSurviving group 2, FactHeldBy holder])
  check "leak: after two snapshots, the surviving group and what holds it" $
    doing leak1 == [StepGo (RSource line82), StepGo (RBinding "Main" "main2"), StepGo group, StepGo holder, StepAct (RBand "0x1") "break where allocated"]
  check "leak: without -hi the first step says how to get provenance" $
    map stepDo (take 1 (nextSteps haveNothing {haveHeapCensus = True} (RBand "THUNK_2_0") [FactLeak 0.9 8, FactThunks])) == [StepLoad DHeapSource]
  check "a band that isn't a suspect gets only its provenance, weakly" $
    let ss = nextSteps heapHave (RBand "0x2") [FactAllocated line82]
     in doing ss == [StepGo (RSource line82)] && not (any stepStrong ss)
  check "a surviving group: what holds it, then where to break" $
    doing (nextSteps heapHave group [FactThunks, FactAllocated line82, FactHeldBy holder])
      == [StepGo holder, StepGo (RSource line82), StepAct group "break where created"]
  let blockedSpan = RRange leakRun 1000000000 1080000000
      worker1 = RThread leakRun 4
      showCC = RCostCentre "Main" "worker"
      contHave = haveNothing {haveTimeline = True, haveSamples = True, haveDumps = True}
      cont = nextSteps contHave (RThread leakRun 7) [FactBlocked blockedSpan 80000000 "MVar", FactWokenBy worker1, FactRan showCC 40]
  check "contention: the blocked span, the waker, what it ran, the binding" $
    doing cont == [StepGo blockedSpan, StepGo worker1, StepGo showCC, StepGo (RBinding "Main" "worker")]
  check "contention: the waker is the strong step" (fmap stepDo (strongStep cont) == Just (StepGo worker1))
  check "contention: a black hole's owner comes before the waker, and leads" $
    let ss = nextSteps contHave (RThread leakRun 7) [FactBlocked blockedSpan 80000000 "black hole", FactOwner worker1, FactWokenBy (RThread leakRun 5)]
     in take 3 (doing ss) == [StepGo blockedSpan, StepGo worker1, StepGo (RThread leakRun 5)] && fmap stepDo (strongStep ss) == Just (StepGo worker1)
  check "contention: without samples, how to sample" $
    StepLoad DSamples `elem` doing (nextSteps haveNothing {haveTimeline = True} (RThread leakRun 7) [FactBlocked blockedSpan 80000000 "MVar"])
  check "contention: a short block is no strong suggestion" $
    strongStep (nextSteps contHave (RThread leakRun 7) [FactBlocked blockedSpan 2000 "MVar", FactWokenBy worker1]) == Nothing
  let hotHave = haveNothing {haveCostCentres = True, haveDumps = True}
  check "hot path: a hot cost centre leads to its binding" $
    case nextSteps hotHave (RCostCentre "Main" "go") [FactCost 35 20] of
      [s] -> stepDo s == StepGo (RBinding "Main" "go") && stepStrong s && "35.0% of the time" `T.isInfixOf` stepReason s
      _ -> False
  check "hot path: without dumps, how to dump" $
    doing (nextSteps haveNothing {haveCostCentres = True} (RCostCentre "Main" "go") [FactCost 35 20]) == [StepLoad DDumps]
  check "hot path: without a .prof, how to profile first" $
    take 1 (doing (nextSteps haveNothing {haveDumps = True} (RCostCentre "Main" "go") [])) == [StepLoad DCostCentres]
  let goBinding = RBinding "Main" "go"
      bindingSs = nextSteps heapHave {haveDumps = True} goBinding [FactBadges False 3 2, FactSiteHeap (RBand "0x2") 1048576]
  check "hot path: the binding's lets, ticky, the heap they hold, break on entry" $
    doing bindingSs == [StepGo goBinding, StepLoad DTicky, StepGo (RBand "0x2"), StepAct goBinding "break on entry"]
  check "hot path: the lets' reason names the boxing" $
    case bindingSs of
      s : _ -> all (`T.isInfixOf` stepReason s) ["2 lets", "3 boxing sites", "no worker/wrapper split"]
      [] -> False
  check "hot path: with ticky the counters join the reason, and no ticky step" $
    let ss = nextSteps heapHave {haveDumps = True, haveTicky = True} goBinding [FactBadges True 0 2, FactTicky 5000 1048576]
     in StepLoad DTicky `notElem` doing ss && any (("5,000 entries" `T.isInfixOf`) . stepReason) ss
  let range = RRange leakRun 2000000000 12000000000
      census = RCensus leakRun 40
      pressure = nextSteps haveNothing {haveTimeline = True, haveHeapCensus = True} range [FactGC 0.4 120, FactAllocation 10000000000 (Just 50000000) (Just 52000000), FactCensusAtEnd census]
  check "GC pressure: analyse, allocators, the census, a bigger nursery" $
    doing pressure == [StepAct range "analyse range", StepLoad DTicky, StepGo census, StepTry "+RTS -A64m -RTS"]
  check "GC pressure: analyse and the nursery are strong" (map stepStrong pressure == [True, False, False, True])
  check "GC pressure: when much survives, the leak suspect" $
    let suspect = RBand "THUNK_2_0"
        ss = nextSteps haveNothing {haveTimeline = True, haveHeapCensus = True, haveTicky = True} range [FactGC 0.1 10, FactAllocation 100000000 (Just 10000000) (Just 60000000), FactTopAllocator goBinding 80000000, FactTopSuspect suspect]
     in doing ss == [StepAct range "analyse range", StepGo goBinding, StepGo suspect] && not (any stepStrong (take 1 ss))
  check "a line of source offers to break there" (doing (nextSteps haveNothing (RSource line82) []) == [StepAct (RSource line82) "break here"])
  check "recipes are shared with the data sources" (datumRecipe DTicky == (Just "-ticky", Just "+RTS -lT -RTS") && datumTitle DSamples == "Cost-centre samples")

  putStrLn "watchers"
  let secs x = round (x * 1e9 :: Double) :: Word64
      on rule = rule {ruOn = True, ruCooldown = 0}
      firedAt rules obs = [fiAt f | f <- snd (watchRun rules obs emptyWatchState)]
      gcs = [ObsMajorGC (secs t) (v * 1e6) | (t, v) <- zip [1 ..] [10, 11, 12, 13, 12.9, 14, 15, 5, 6, 7, 8]]
      liveUp = Rule (LiveUp 3) True [RespNotify] 0 0.05
  check "the built-in rules are all off" (length defaultRules == 4 && not (any ruOn defaultRules))
  check "a rule that is off never fires" (null (firedAt [liveUp {ruOn = False}] gcs))
  check "live up across 3 collections fires once, until a real drop re-arms it" (firedAt [liveUp] gcs == [secs 4, secs 11])
  check "a small dip neither breaks nor extends the run" $
    firedAt [liveUp {ruTrigger = LiveUp 5}] (take 7 gcs) == [secs 7]
  check "the cooldown holds the second firing back" (firedAt [liveUp {ruCooldown = 30}] gcs == [secs 4])
  let frame t blocked = ObsThreads (secs t) [ThreadNow 7 "worker-7" (if blocked then Just "MVar" else Nothing), ThreadNow 1 "main" Nothing]
      frames = [frame (fromIntegral i * 0.25) b | (i, b) <- zip [0 :: Int ..] ([True, True, True, True, True, False] ++ [True, True, True] ++ [False, False, False, False, False, False, False] ++ [True, True, True, True])]
      blockedRule = on (Rule (BlockedOver 500) False [RespNotify] 10 0.5)
      blockedFired = snd (watchRun [blockedRule] frames emptyWatchState)
  check "a thread blocked past the limit fires, once per stretch" (map fiAt blockedFired == [secs 0.5, secs 4.5])
  check "a short run between blocks doesn't count as a new stretch" (length blockedFired == 2)
  check "it names the thread" (map fiSubject blockedFired == [AboutThread 7 "worker-7", AboutThread 7 "worker-7"])
  check "threads in a foreign call, and the agent's own, are left out" $
    null (firedAt [blockedRule] [ObsThreads (secs t) [ThreadNow 2 "IOManagerThread" (Just "ForeignCall"), ThreadNow 4 "stronghead-agent" (Just "MVar")] | t <- [0, 0.5, 1]])
  check "blocked spans from an eventlog fire when long enough" $
    firedAt [blockedRule] [ObsBlockedSpan 0 (secs 0.1) (ThreadNow 3 "" (Just "MVar")), ObsBlockedSpan (secs 1) (secs 1.6) (ThreadNow 3 "" (Just "MVar"))] == [secs 1.6]
  let shares = [ObsGCShare (secs t) 1 s | (t, s) <- zip [1 ..] [0.6, 0.7, 0.65, 0.45, 0.6, 0.6, 0.6, 0.3, 0.6, 0.6, 0.6]]
      gcRule = on (Rule (GCOver 50 2) False [RespNotify] 0 0.2)
  check "GC over the limit for long enough fires" (take 1 (firedAt [gcRule] shares) == [secs 2])
  check "and re-arms only once it falls below the hysteresis" (firedAt [gcRule] shares == [secs 2, secs 10])
  let census t vs = ObsCensus (secs t) vs
      censuses = [census t [("THUNK", v * 1e6), ("ARR_WORDS", 5e6), ("tiny", t * 10)] | (t, v) <- zip [1 ..] [1, 2, 3, 4, 5]]
      bandFired = snd (watchRun [on (Rule (BandGrowing 3) False [RespNotify, RespMark] 0 0.02)] censuses emptyWatchState)
  check "a band growing for 3 censuses fires, about that band" (map fiSubject bandFired == [AboutBand "THUNK"] && map fiAt bandFired == [secs 4])
  check "a fired rule carries its responses" (map fiResponses bandFired == [[RespNotify, RespMark]])
  let ruleText = encodeRule (on (Rule (GCOver 40 5) False [RespNotify, RespSnapshot] 15 0.25))
  check "a rule round trips through its text" (decodeRule ruleText == Just (on (Rule (GCOver 40 5) False [RespNotify, RespSnapshot] 15 0.25)))
  check "every built-in rule round trips" (all (\r -> decodeRule (encodeRule r) == Just r) defaultRules)
  check "an unknown rule is refused; missing values take the defaults" $
    decodeRule "nonsense n=3" == Nothing && fmap ruTrigger (decodeRule "live-up on=true") == Just (LiveUp 5)
  check "saved rules replace the built-in ones by kind, in their order" $
    map (triggerName . ruTrigger) (setRules [on (Rule (BandGrowing 9) False [] 0 0)]) == map (triggerName . ruTrigger) defaultRules
      && fmap ruTrigger (setRules [on (Rule (BandGrowing 9) False [] 0 0)] !? 3) == Just (BandGrowing 9)
  let stats m = M.fromList m
      f0 = Frame 0 1000000000 True (stats [("cpu_ns", 1e9), ("gc_cpu_ns", 2e8), ("major_gcs", 3), ("gc_gen", 0), ("gc_live_bytes", 1e6)]) [LiveThread 1 "running" "main" Nothing]
      f1 = Frame 0.25 1250000000 True (stats [("cpu_ns", 2e9), ("gc_cpu_ns", 8e8), ("major_gcs", 4), ("gc_gen", 1), ("gc_live_bytes", 2e6)]) [LiveThread 1 "BlockedOnMVar" "main" Nothing]
      fobs = frameObservations (Just f0) f1
  check "frames give the threads, blocked or not" $
    [ts | ObsThreads _ ts <- fobs] == [[ThreadNow 1 "main" (Just "MVar")]] && [ts | ObsThreads _ ts <- frameObservations Nothing f0] == [[ThreadNow 1 "main" Nothing]]
  check "frames give the GC share of the CPU between them" ([s | ObsGCShare _ _ s <- fobs] == [0.6])
  check "frames give a major collection when the last one was major" ([v | ObsMajorGC _ v <- fobs] == [2e6] && null [() | ObsMajorGC {} <- frameObservations (Just f1) f1 {frProgramNs = 1500000000}])
  let lobs = timelineObservations sched 0 maxBound
  check "an eventlog gives its blocked spans and censuses" $
    length [() | ObsBlockedSpan _ _ th <- lobs, tnId th == 2, tnBlocked th == Just "blocked on MVar"] == 2 && length [() | ObsCensus 240 _ <- lobs] == 1
  check "and only what came after the last look" (length (timelineObservations sched 200 maxBound) < length lobs && null (timelineObservations sched 260 maxBound))

  putStrLn "diffs"
  let ipe name label = InfoProv (name <> "_info") 15 KindThunk "Integer" label "Main" (SrcAt (SrcSpan "sample\\Main.hs" 82 5 82 40))
      provBase = \case
        "0x1" -> Just (ipe "main2_sat_s7qA" "main2")
        _ -> Nothing
      provCur = \case
        "0x9" -> Just (ipe "main2_sat_s8bB" "main2")
        _ -> Nothing
      before = fromSamples "" "info table" 0 provBase [(t, [("0x1", t * 1e6), ("ARR_WORDS", 4e6), ("THUNK_2_0", t * 2e6), ("IS", t * 5e5)]) | t <- [1 .. 6]]
      after = fromSamples "" "info table" 0 provCur [(t, [("0x9", 2e6), ("ARR_WORDS", 4e6), ("MUT_VAR_CLEAN", 2e6), ("IS", 32)]) | t <- [1 .. 6]]
      changes = diffHeap before after
      statusOf name = [bcStatus c | c <- changes, fmap bfName (bcBase c) == Just name || fmap bfName (bcCur c) == Just name]
  check "a band that went away is gone" (statusOf "THUNK_2_0" == [BandGone])
  check "and so is one left with a trace of what it held" (statusOf "IS" == [BandGone])
  check "a band on the same source, under a new address, is matched by source" $
    [(bcMatch c, bcStatus c) | c <- changes, fmap bfName (bcCur c) == Just "0x9"] == [(SameSource, BandShrank)]
  check "an unchanged band is the same, by name" ([(bcMatch c, bcStatus c) | c <- changes, fmap bfName (bcCur c) == Just "ARR_WORDS"] == [(SameName, BandSame)])
  check "a band only in the current run is new" (statusOf "MUT_VAR_CLEAN" == [BandNew])
  check "biggest change in peak first" ((listToMaybe changes >>= bcBase >>= Just . bfName) == Just "THUNK_2_0")
  check "the baseline's suspects keep their leak scores" $
    all (isJust . bfScore) [bf | c <- changes, Just bf <- [bcBase c], bfName bf `elem` ["0x1", "THUNK_2_0"]]
  check "growth is end minus start" ([bfGrowth bf | c <- changes, Just bf <- [bcBase c], bfName bf == "THUNK_2_0"] == [10e6])
  check "a gone band still knows its site" (isNothing (changeFor changes "THUNK_2_0") && fmap (fmap ssLine . (>>= bandSite) . bcBase) (changeFor changes "0x9") == Just (Just 82))
  let ct m n t a = CostTotal m n SrcNoLocation 1 t a t a
      costs = diffCosts [ct "Main" "go" 60 50, ct "Main" "same" 5 5, ct "Main" "gone" 3 1] [ct "Main" "go" 10 5, ct "Main" "same" 5 5, ct "Main" "new" 2 2]
  check "cost centres matched by module and name, biggest change first" $
    map cgName costs == ["go", "gone", "new", "same"] && maybe False (isNothing . cgCur) (costs !? 1) && maybe False (isNothing . cgBase) (costs !? 2)
  let bsum k w b l sz = BindingSummary "Main" k w b l sz
      bindings = diffBindings [bsum "go" False 2 3 (Just 40), bsum "same" False 0 1 (Just 5), bsum "gone" False 0 0 Nothing] [bsum "go" True 0 3 (Just 12), bsum "same" False 0 1 (Just 5), bsum "fresh" False 0 0 Nothing]
  check "bindings whose badges changed, in words" $
    [bgChanges c | c <- bindings, bgKey c == "go"] == [["worker/wrapper now", "boxing sites 2 → 0", "size 40 → 12 terms"]]
  check "unchanged bindings are left out; new and gone ones say so" $
    map bgKey bindings == ["fresh", "go", "gone"] && [bgChanges c | c <- bindings, bgKey c /= "go"] == [["new"], ["gone: inlined or removed"]]
  let summaries = summariseModule "Main" (Just core) (Just (parseCodeFile KindStg "Main.dump-stg-final" stgText))
  check "a module's bindings from its dumps, worker and wrapper as one" $
    [(bsWorker s, bsBoxes s > 0) | s <- summaries, bsKey s == "go"] == [(True, True)] && "main" `elem` map bsKey summaries
  check "changes against the baseline, in readouts" $
    fmtChange 100 18 == "−82%" && fmtChange 100 112 == "+12%" && fmtChange 0 0 == "±0%" && fmtChange 0 5 == "new" && fmtChange 100 100.04 == "±0%" && relChange 0 1 == Nothing

  putStrLn "text files"
  do
    tmpDir <- getTemporaryDirectory
    let path = tmpDir <> "/stronghead-utf8-test.stronghead"
        txt = "focus = binding/Main/go\ntitle = worker\8230 2.5 \181s\n"
    writeTextFile path txt
    back <- readTextFile path
    check "non-ASCII text round-trips as UTF-8, whatever the code page" (back == txt)

  putStrLn "sessions: watchers and baseline"
  let watched =
        emptySession
          { sesWatchers = setRules [on (Rule (BlockedOver 250) False [RespNotify, RespMark] 5 0.3)]
          , sesBaseline = noBaselinePaths {blEventlog = Just "scratch\\before.eventlog", blCostCentres = Just "scratch/before.prof", blDumps = Just "scratch/dumps before"}
          }
  check "watchers and the baseline round trip" (decodeSession (encodeSession watched) == Right watched)
  check "a session names the baseline's artifacts" ("baseline.eventlog = scratch\\before.eventlog" `T.isInfixOf` encodeSession watched)
  check "an older session gets the built-in rules and no baseline" $
    fmap (\s -> (sesWatchers s, sesBaseline s)) (decodeSession "version = 1\n") == Right (defaultRules, noBaselinePaths)
  check "a hand-written rule replaces its kind only" $
    fmap (map ruOn . sesWatchers) (decodeSession "version = 1\nwatch.1 = band-growing n=4 on=true\n") == Right [False, False, False, True]

  n <- readIORef failures
  if n == 0 then putStrLn "all passed" else putStrLn (show n <> " failed") >> exitFailure

-- | Scheduling on two capabilities. Thread 2 blocks twice: thread 1 wakes
-- it from capability 0, then thread 3 from its own capability 1. One
-- collection, one census during it, and a marker.
schedFixture :: [(Word64, Maybe Int, EventInfo)]
schedFixture =
  [ (50, Nothing, HeapAllocated 0 0)
  , (100, Just 0, RunThread 1)
  , (100, Nothing, HeapLive 0 100)
  , (110, Just 1, RunThread 2)
  , (120, Just 1, StopThread 2 BlockedOnMVar)
  , (150, Just 0, WakeupThread 2 1)
  , (155, Nothing, UserMarker "mid")
  , (160, Just 1, RunThread 2)
  , (170, Just 1, StopThread 2 BlockedOnMVar)
  , (175, Just 1, RunThread 3)
  , (180, Just 1, WakeupThread 2 1)
  , (185, Just 1, StopThread 3 ThreadFinished)
  , (190, Just 1, RunThread 2)
  , (200, Just 1, StopThread 2 ThreadFinished)
  , (210, Just 0, StopThread 1 ThreadFinished)
  , (220, Just 0, StartGC)
  , (230, Nothing, HeapAllocated 0 5000)
  , (240, Nothing, HeapProfSampleBegin 0)
  , (240, Nothing, HeapProfSampleString 0 300 "THUNK")
  , (240, Nothing, HeapProfSampleEnd 0)
  , (250, Nothing, HeapLive 0 300)
  , (260, Just 0, EndGC)
  ]

-- | Cost-centre samples on two capabilities, each sampled every 10 ns.
-- Thread 1 runs on capability 0 from 100 to 140 and again from 160;
-- thread 2 on capability 1 from 100 to 140. Cost centre 1 is the idle one.
profFixture :: [(Word64, Maybe Int, EventInfo)]
profFixture =
  [ (0, Nothing, HeapProfCostCentre 1 "IDLE" "IDLE" "<built-in>" (HeapProfFlags 0))
  , (0, Nothing, HeapProfCostCentre 2 "leak" "Main" "sample/Main.hs:82:1-20" (HeapProfFlags 0))
  , (0, Nothing, HeapProfCostCentre 3 "show" "Main" "sample/Main.hs:76:5-30" (HeapProfFlags 0))
  , (0, Nothing, HeapProfCostCentre 4 "main" "Main" "sample/Main.hs:40:1-4" (HeapProfFlags 0))
  , (100, Just 0, RunThread 1)
  , (100, Just 1, RunThread 2)
  , (110, Nothing, sample 0 1 [2, 4])
  , (110, Nothing, sample 1 1 [1])
  , (120, Nothing, sample 0 2 [2, 4])
  , (120, Nothing, sample 1 2 [3, 4])
  , (130, Nothing, sample 0 3 [3, 4])
  , (130, Nothing, sample 1 3 [3, 4])
  , (140, Just 0, StopThread 1 ThreadYielding)
  , (140, Just 1, StopThread 2 ThreadFinished)
  , (150, Nothing, sample 0 5 [1])
  , (160, Just 0, RunThread 1)
  , (170, Nothing, sample 0 7 [2])
  , (180, Just 0, StopThread 1 ThreadFinished)
  ]
  where
    sample cap tick stack = E.ProfSampleCostCentre cap tick (fromIntegral (length stack)) (VU.fromList stack)

-- | Three ticky counters: @main2@ and two closures nested in it. The RTS
-- resets the counters after each sample.
tickyFixture :: [(Word64, EventInfo)]
tickyFixture =
  [ (10, E.TickyCounterDef 1 1 "L" "Main.main2{v r7mA} (fun,se)" 0x100 Nothing)
  , (10, E.TickyCounterDef 2 0 "" "main2_sat_s7qM{v} (Main) (fun) in r7mA" 0x200 Nothing)
  , (10, E.TickyCounterDef 3 0 "" "main2_sat_s7qL{v} (Main) (fun) in s7qM" 0x300 Nothing)
  , (20, E.TickyBeginSample)
  , (20, E.TickyCounterSample 3 1 128 0)
  , (30, E.TickyBeginSample)
  , (30, E.TickyCounterSample 3 0 700 0)
  , (30, E.TickyCounterSample 1 1 496 0)
  ]

-- | A slice of a @-hi@ eventlog: the run's identity, one cost centre, one
-- info table and one census. The first event is at 1 s.
eventlogFixture :: [(Word64, EventInfo)]
eventlogFixture =
  [ (1000000000, RtsIdentifier 0 "GHC-9.14.1 rts_thr")
  , (1000000000, ProgramArgs 0 ["C:\\s\\stronghead-sample.exe", "arg", "+RTS", "-l", "-hi", "-RTS"])
  , (1000000000, OsProcessPid 0 4242)
  , (1500000000, WallClockTime 0 1760000000 123)
  , (1600000000, HeapProfBegin 0 100000000 HeapProfBreakdownInfoTable "" "" "" "" "" "" "")
  , (1600000000, HeapProfCostCentre 7 "go" "Main" "sample/Main.hs:(10,1)-(12,20)" (HeapProfFlags 1))
  , (1700000000, InfoTableProv 0x1000 "sat_s3Pe_info" 15 "Int" "main" "Main" "sample/Main.hs:82:15-40")
  , (3000000000, HeapProfSampleBegin 0)
  , (3000000000, HeapProfSampleString 0 1000 "0x1000")
  , (3000000000, HeapProfSampleEnd 0)
  ]

hpText :: Text
hpText =
  T.unlines
    [ "JOB \"prog +RTS -hT -i0.1\""
    , "DATE \"Mon Oct 19 21:49 2026\""
    , "SAMPLE_UNIT \"seconds\""
    , "VALUE_UNIT \"bytes\""
    , "BEGIN_SAMPLE 0.10"
    , "THUNK_2_0\t1000"
    , "(:)\t500"
    , "END_SAMPLE 0.10"
    , "BEGIN_SAMPLE 0.20"
    , "THUNK_2_0\t5000"
    , "(:)\t400"
    , "ARR_WORDS\t100"
    , "END_SAMPLE 0.20"
    , "BEGIN_SAMPLE 0.30"
    , "THUNK_2_0\t20000"
    , "(:)\t600"
    , "ARR_WORDS\t100"
    , "END_SAMPLE 0.30"
    ]

ccsHp :: Text
ccsHp =
  T.unlines
    [ "JOB \"prog +RTS -hc\""
    , "BEGIN_SAMPLE 0.1"
    , "(12)leaf/go/MAIN\t200"
    , "(13)go/MAIN\t100"
    , "END_SAMPLE 0.1"
    ]

profText :: Text
profText =
  T.unlines
    [ "\tFri Oct 16 21:49 2026 Time and Allocation Profiling Report  (Final)"
    , ""
    , "\t   prog +RTS -p -RTS"
    , ""
    , "\ttotal time  =        1.00 secs   (1000 ticks @ 1000 us, 1 processor)"
    , ""
    , "COST CENTRE MODULE SRC %time %alloc"
    , ""
    , "go          Main   Main.hs:5:1-20  60.0   70.0"
    , ""
    , "                                                         individual      inherited"
    , "COST CENTRE  MODULE    SRC               no.   entries  %time %alloc   %time %alloc"
    , ""
    , "MAIN         MAIN      <built-in>        1          0    0.0    0.0   100.0  100.0"
    , " main        Main      Main.hs:9:1-20    2          1    1.0    1.0    80.0   90.0"
    , "  go         Main      Main.hs:5:1-20    3       1000   60.0   70.0    60.5   70.0"
    , " CAF         GHC.IO    <entire-module>   4          0    0.5    0.1     0.5    0.1"
    , " CAF         GHC.Conc  <no location info> 5         0    0.2    0.0     0.2    0.0"
    ]

coreText :: Text
coreText =
  T.unlines
    [ ""
    , "==================== Tidy Core ===================="
    , "Result size of Tidy Core"
    , "  = {terms: 40, types: 12, coercions: 0, joins: 0/0}"
    , ""
    , "Rec {"
    , "-- RHS size: {terms: 14, types: 3, coercions: 0, joins: 0/0}"
    , "Main.$wgo [InlPrag=[2], Occ=LoopBreaker]"
    , "  :: GHC.Prim.Int# -> GHC.Prim.Int# -> GHC.Prim.Int#"
    , "[GblId, Arity=2, Str=<1L><L>, Unf=OtherCon []]"
    , "Main.$wgo"
    , "  = \\ (ww_s1 :: GHC.Prim.Int#) (ww1_s2 :: GHC.Prim.Int#) ->"
    , "      case ww_s1 of ds_X0 {"
    , "        __DEFAULT -> Main.$wgo (GHC.Prim.-# ds_X0 1#) (GHC.Prim.+# ww1_s2 ds_X0);"
    , "        0# -> ww1_s2"
    , "      }"
    , "end Rec }"
    , ""
    , "-- RHS size: {terms: 15, types: 5, coercions: 0, joins: 0/0}"
    , "Main.go [InlPrag=INLINE[final]] :: Int -> Int -> Int"
    , "[GblId, Arity=2, Str=<1!P(L)><1!P(L)>, Cpr=1]"
    , "Main.go"
    , "  = \\ (n_a1 :: Int) (acc_a2 :: Int) ->"
    , "      case n_a1 of { GHC.Types.I# ww_s1 ->"
    , "      case acc_a2 of { GHC.Types.I# ww1_s2 ->"
    , "      case Main.$wgo ww_s1 ww1_s2 of ww2_s3 { __DEFAULT -> GHC.Types.I# ww2_s3 }"
    , "      }"
    , "      }"
    , ""
    , "-- RHS size: {terms: 6, types: 2, coercions: 0, joins: 0/0}"
    , "Main.main :: IO ()"
    , "[GblId]"
    , "Main.main = System.IO.print @Int GHC.Show.$fShowInt (Main.go 10 0)"
    ]

-- | A thread whose stack refers to a CAF holding an IORef with a chain of
-- @n@ (+) thunks, the first of which also points at the boxed integer
-- directly (so the graph is a DAG), plus one unreachable closure. Thunk
-- addresses differ per call, as they would between real snapshots.
leakSnapshot :: Int -> HeapSnapshot
leakSnapshot n =
  mkSnapshot "test" False [1] $
    [ node 1 "TSO" "thread 1" [2]
    , node 2 "STACK" "STACK" [3]
    , node 3 "FUN_STATIC" "main" [4]
    , node 4 "IND_STATIC" "counter" [5]
    , node 5 "MUT_VAR_DIRTY" "IORef" [thunk 0]
    , node 8 "CONSTR_1_0" "IS" []
    , node 99 "CONSTR_0_1" "I#" []
    ]
      ++ [node (thunk i) "THUNK_2_0" "plus" ([thunk (i + 1) | i + 1 < n] ++ [8 | i == 0]) | i <- [0 .. n - 1]]
  where
    base = if n == 2 then 6 else 1000
    thunk i = if n == 2 && i == 1 then 7 else base + fromIntegral i
    node i ty name kids = ClosureNode i ty (kindOfClosureType ty) name Nothing 16 kids Nothing []

-- | A slice of an STG dump from an investigation build: a thunk bound by a
-- @let@, a numbered constructor site, and the source notes
-- @-finfo-table-map@ adds.
stgText :: Text
stgText =
  T.unlines
    [ "Main.main2 [GblId, Arity=1] :: GHC.Internal.Bignum.Integer.Integer -> GHC.Internal.Types.Int"
    , "[GblId, Arity=1, Unf=OtherCon []] ="
    , "    \\r [x1_s7qu]"
    , "        let {"
    , "          main2_sat_s7qA [Occ=Once1]"
    , "            :: GHC.Internal.Bignum.Integer.Integer"
    , "          [LclId] ="
    , "              {x1_s7qu} \\u []"
    , "                  src<sample\\Main.hs:82:59-63>"
    , "                  GHC.Internal.Bignum.Integer.integerAdd x1_s7qu x1_s7qu;"
    , "        } in"
    , "        let {"
    , "          main2_sat_s7qB [Occ=Once1] :: GHC.Internal.Types.Int"
    , "          [LclId] ="
    , "              3 GHC.Internal.Types.I#! [1#];"
    , "        } in  main2_sat_s7qB;"
    ]

-- ---------------------------------------------------------------------------
-- The agent and the client, in this process

-- | A v2 agent and client together: the info line, capabilities, commands
-- and their replies, and commands turned off. Then each side against the
-- other version: a v1 agent (no info line, never reads) still streams
-- frames to this client, and a v2 agent's lines leave everything a v1
-- client reads as it was.
agentChecks :: (String -> Bool -> IO ()) -> IO ()
agentChecks check = do
  let cfg port = defaultAgentConfig {agentPort = port, agentIntervalMs = 50, agentFlushEventlog = False}
  agent <- startAgent (cfg 47771)
  (events, client) <- listen 47771
  box <- waitFor events (\case LiveConnected b -> Just b; _ -> Nothing)
  hello <- waitFor events (\case LiveHello _ pid _ -> Just pid; _ -> Nothing)
  info <- waitFor events (\case LiveInfo i -> Just i; _ -> Nothing)
  frame <- waitFor events (\case LiveFrame f -> Just f; _ -> Nothing)
  check "v2 agent: hello" (maybe False (> 0) hello)
  check "v2 agent: info line" (fmap aiVersion info == Just 2 && fmap aiCommands info == Just True && maybe False (not . T.null . aiCwd) info && maybe False (not . null . aiArgs) info)
  check "v2 agent: no heap profile or eventlog in this test" (fmap aiHeapProfile info == Just Nothing && fmap aiEventlog info == Just False)
  check "v2 agent: threads carry capabilities" (maybe False (any (isJust . ltCap) . frThreads) frame)
  for_ box $ \b -> do
    sendCommand b 1 (CmdMark "test mark")
    sendCommand b 2 CmdGC
    sendCommand b 3 (CmdHeapProfiling False)
    sendCommand b 4 CmdFlush
  replies <- mapM (\i -> waitFor events (\case LiveReply j r | j == i -> Just r; _ -> Nothing)) [1, 2, 3, 4]
  -- Without +RTS -T the agent can't say when.
  check "v2 agent: commands answered, unstamped without +RTS -T" (all (== Just (ReplyOk Nothing)) replies)
  stop client
  -- Commands turned off.
  closed <- startAgent (cfg 47772) {agentAcceptCommands = Just False}
  (events2, client2) <- listen 47772
  box2 <- waitFor events2 (\case LiveConnected b -> Just b; _ -> Nothing)
  info2 <- waitFor events2 (\case LiveInfo i -> Just i; _ -> Nothing)
  for_ box2 $ \b -> sendCommand b 9 CmdGC
  refused <- waitFor events2 (\case LiveReply 9 r -> Just r; _ -> Nothing)
  check "commands turned off: the info line says so" (fmap aiCommands info2 == Just False)
  check "commands turned off: refused" (case refused of Just (ReplyError e) -> "turned off" `T.isInfixOf` e; _ -> False)
  stop client2
  -- A v1 client reading a v2 agent: the lines it knows are as they were,
  -- and the rest start with a letter it skips.
  raw <- rawLines 47771
  let kinds = map (takeWhile (/= ' ')) raw
  check "v1 client: hello first, as before" (case raw of h : _ -> length (words h) == 4 && take 2 h == "H "; [] -> False)
  check "v1 client: only lines it knows or skips" (all (`elem` ["H", "I", "S", "T", "C", "E"]) kinds && "T" `elem` kinds && "E" `elem` kinds)
  check "v1 client: thread lines unchanged" (and [length (words l) >= 3 | l <- raw, take 2 l == "T "])
  -- A v2 client reading a v1 agent, which never reads what it is sent.
  v1 <- forkIO (fakeV1Agent 47773)
  threadDelay 100000
  (events3, client3) <- listen 47773
  box3 <- waitFor events3 (\case LiveConnected b -> Just b; _ -> Nothing)
  for_ box3 $ \b -> sendCommand b 1 CmdGC
  frames <- waitFor events3 (\case LiveFrame f -> Just f; _ -> Nothing)
  threadDelay 300000
  seen <- readIORef events3
  check "v1 agent: frames still arrive" (isJust frames && not (null [() | LiveHello "prog" 7 1 <- seen]))
  check "v1 agent: no info line, so no replies" (null [() | LiveInfo _ <- seen] && null [() | LiveReply _ _ <- seen])
  stop client3
  -- A synchronous kill would wait on a blocking accept (Windows).
  mapM_ (forkIO . killThread) (v1 : agent ++ closed)
  where
    listen port = do
      events <- newIORef []
      t <- forkIO (connectLive "127.0.0.1" port (\e -> atomicModifyIORef' events (\es -> (es ++ [e], ()))))
      pure (events, t)
    stop t = void (forkIO (killThread t))

-- | The first event that matches, waiting up to five seconds for it.
waitFor :: IORef [LiveEvent] -> (LiveEvent -> Maybe a) -> IO (Maybe a)
waitFor events pick = go (100 :: Int)
  where
    go 0 = pure Nothing
    go n = do
      es <- readIORef events
      case mapMaybe pick es of
        x : _ -> pure (Just x)
        [] -> threadDelay 50000 >> go (n - 1)

-- | A few frames' worth of an agent's lines, read as a v1 client would.
rawLines :: Int -> IO [String]
rawLines port =
  fromRight [] <$> try @SomeException do
    addrs <- N.getAddrInfo (Just N.defaultHints {N.addrSocketType = N.Stream}) (Just "127.0.0.1") (Just (show port))
    case addrs of
      [] -> pure []
      addr : _ ->
        bracket (N.openSocket addr) N.close $ \sock -> do
          N.connect sock (N.addrAddress addr)
          let go acc n
                | n <= (0 :: Int) = pure acc
                | otherwise = do
                    chunk <- NB.recv sock 65536
                    if B.null chunk then pure acc else go (acc <> chunk) (n - BC.count '\n' chunk)
          bytes <- go B.empty 60
          pure [l | l <- lines (filter (/= '\r') (BC.unpack bytes)), not (null l)]

-- | An agent from before protocol v2: hello and frames, and it never reads
-- the socket.
fakeV1Agent :: Int -> IO ()
fakeV1Agent port = do
  addrs <- N.getAddrInfo (Just N.defaultHints {N.addrFlags = [N.AI_PASSIVE], N.addrSocketType = N.Stream}) (Just "127.0.0.1") (Just (show port))
  for_ (listToMaybe addrs) $ \addr ->
    bracket (N.openSocket addr) N.close $ \sock -> do
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.addrAddress addr)
      N.listen sock 1
      (conn, _) <- N.accept sock
      NB.sendAll conn "H prog 7 1\n"
      forever $ do
        NB.sendAll conn "S rts_stats=0\nT 1 running main\nE\n"
        threadDelay 50000
