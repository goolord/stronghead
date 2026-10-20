-- | Parser and analysis tests. No framework: each check prints a line and
-- the run fails if any check failed.
module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import GHC.RTS.Events (EventInfo (..), HeapProfBreakdown (..), HeapProfFlags (..))
import GHC.RTS.Events qualified as E
import Stronghead.Debug
import Stronghead.Dump
import Stronghead.Eventlog (emptyBuilder, feedEvent, snapshot)
import Stronghead.Eventlog.Model
import Stronghead.Ghci
import Stronghead.HeapProfile
import Stronghead.Live
import Stronghead.Prof
import Stronghead.Ref
import Stronghead.Run
import Stronghead.Source
import System.Exit (exitFailure)
import System.FilePath (pathSeparator, (</>))

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "  ok   " else "  FAIL ") <> name)
        unless ok (modifyIORef' failures (+ 1))

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
      check "four cost centres" (length flat == 4)
      check "nesting from indentation" (map fst flat == [0, 1, 2, 1])
      check "inherited time parsed" (ccInhTime (snd (flat !! 2)) == 60.5)
      check "cost-centre spans parsed" $
        map (ccLoc . snd) flat
          == [SrcBuiltIn, SrcAt (SrcSpan "Main.hs" 9 1 9 20), SrcAt (SrcSpan "Main.hs" 5 1 5 20), SrcEntireModule]
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
  check "thread" (parseLine "T 7 BlockedOnMVar worker 1" == LThread (LiveThread 7 "BlockedOnMVar" "worker 1"))
  check "program clock from elapsed_ns" (frameClock 9 (M.fromList [("rts_stats", 1), ("elapsed_ns", 1.5e9)]) == (1500000000, True))
  check "init time added" (frameClock 9 (M.fromList [("rts_stats", 1), ("elapsed_ns", 1.5e9), ("init_elapsed_ns", 1.3e6)]) == (1501300000, True))
  check "arrival time without +RTS -T" (frameClock 2.5 (M.fromList [("rts_stats", 0)]) == (2500000000, False))

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
  check "run key" (runKey fromLog == RunKey "stronghead-sample:4242" && runKey hpRunInfo == RunKey "stronghead-sample:?")

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

  n <- readIORef failures
  if n == 0 then putStrLn "all passed" else putStrLn (show n <> " failed") >> exitFailure

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
    node i ty name kids = ClosureNode i ty (kindOfClosureType ty) name Nothing 16 kids
