{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Model Context Protocol (MCP) tool types for Stronghead, using the 'mcp-server' library.
module Stronghead.MCP.Types
  ( StrongheadTool (..)
  , toolDescriptions
  ) where

import Data.Text (Text)

-- | High-level data type defining all Stronghead MCP tools.
--
-- Constructor names are automatically converted to snake_case by 'MCP.Server.Derive'
-- (e.g. 'StrongheadDiagnoseLeak' becomes 'stronghead_diagnose_leak').
data StrongheadTool
  = StrongheadDiagnoseLeak
      { path :: !Text
      , top_n :: !(Maybe Int)
      , threshold :: !(Maybe Double)
      }
  | StrongheadInspectCore
      { binding :: !Text
      , dump_path :: !(Maybe Text)
      , module_name :: !(Maybe Text)
      , include_stg :: !(Maybe Bool)
      }
  | StrongheadEventlogSummary
      { path :: !Text
      , top_threads :: !(Maybe Int)
      }
  | StrongheadDiffRuns
      { baseline :: !Text
      , current :: !Text
      , baseline_dumps :: !(Maybe Text)
      , current_dumps :: !(Maybe Text)
      }
  | StrongheadRetainerPath
      { closure_address :: !(Maybe Text)
      , band_name :: !(Maybe Text)
      , target :: !(Maybe Text)
      }
  | StrongheadLiveControl
      { command :: !Text
      , host :: !(Maybe Text)
      , port :: !(Maybe Int)
      , text :: !(Maybe Text)
      , timeout_ms :: !(Maybe Int)
      }
  | StrongheadRecommendNextSteps
      { issue_type :: !Text
      , entity :: !(Maybe Text)
      , file_path :: !(Maybe Text)
      }
  | StrongheadGhciEval
      { command :: !Text
      , cwd :: !(Maybe Text)
      , repl_cmd :: !(Maybe Text)
      }
  | StrongheadRenderGraph
      { path :: !Text
      , baseline_path :: !(Maybe Text)
      , output_path :: !(Maybe Text)
      , theme :: !(Maybe Text)
      , top_n :: !(Maybe Int)
      }
  | StrongheadHeapAnatomy
      { snapshot_target :: !Text
      , sever_closure :: !(Maybe Text)
      , sever_type :: !(Maybe Text)
      , top_n :: !(Maybe Int)
      }
  | StrongheadCriticalPath
      { path :: !Text
      , thread_id :: !(Maybe Int)
      , span_name :: !(Maybe Text)
      }
  | StrongheadCoreSmells
      { dumps_dir :: !Text
      , prof_path :: !(Maybe Text)
      , baseline_dumps :: !(Maybe Text)
      , filter_binding :: !(Maybe Text)
      , top_n :: !(Maybe Int)
      }
  | StrongheadLiveStacks
      { host :: !(Maybe Text)
      , port :: !(Maybe Int)
      , include_probes :: !(Maybe Bool)
      , timeout_ms :: !(Maybe Int)
      }
  | StrongheadRtsSweep
      { exe :: !Text
      , args :: !(Maybe Text)
      , grid :: !(Maybe Text)
      }
  | StrongheadAssertBudget
      { spec :: !Text
      , eventlog_path :: !(Maybe Text)
      , hp_path :: !(Maybe Text)
      , baseline_path :: !(Maybe Text)
      , dump_path :: !(Maybe Text)
      }
  deriving stock (Show, Eq)

-- | Descriptions for tools and arguments used by 'deriveToolHandlerWithDescription'.
toolDescriptions :: [(String, String)]
toolDescriptions =
  [ ( "StrongheadDiagnoseLeak"
    , """
      Analyzes a GHC heap profile (.hp file) or eventlog to pinpoint space leaks,
      calculate leak suspect scores, growth rates, monotone steps, band kinds
      (e.g. KindThunk), info-table provenance (STG symbols, source locations),
      and cost-centre retention trees.
      """
    )
  , ( "StrongheadInspectCore"
    , """
      Inspects GHC Core and STG intermediate representations for a Haskell binding
      or function, detailing worker-wrapper splits, strictness properties, CPR,
      unboxed operations, boxing constructor allocations (e.g. I#), STG let
      allocations, and ticky counters.
      """
    )
  , ( "StrongheadEventlogSummary"
    , """
      Analyzes a GHC .eventlog file to summarize runtime performance: GC pause time
      percentiles (p50, p90, p95, p99, max), mutator vs GC CPU time, capability
      utilization, total memory allocation and peak live residency, thread lifetimes,
      and thread blocking contention breakdown (MVar, STM, Delay, Blackhole).
      """
    )
  , ( "StrongheadDiffRuns"
    , """
      Compares two profiling runs (Run A baseline vs Run B current) to empirically
      verify performance optimizations, tracking deltas in peak heap residency,
      total allocations, GC pause percentiles, and heap band status (BandGone,
      BandShrank, BandGrew, BandNew).
      """
    )
  , ( "StrongheadRetainerPath"
    , """
      Finds the shortest retainer path from GC roots to a target closure or heap
      band using a live ghc-debug connection or saved heap snapshot, revealing the
      exact retention chain keeping memory alive.
      """
    )
  , ( "StrongheadLiveControl"
    , """
      Sends control commands to a running Haskell process linking Stronghead.Agent
      over TCP: trigger major GC, request heap census, toggle heap profiling timer
      on/off, insert eventlog marker, or query live RTS stats.
      """
    )
  , ( "StrongheadRecommendNextSteps"
    , """
      Generates prescriptive, expert GHC compiler flags, RTS profiling options,
      and debugging actions tailored to a specific issue (space leak, thread
      contention, GC pressure, or hot path).
      """
    )
  , ( "StrongheadGhciEval"
    , """
      Executes a command in a GHCi session using Stronghead's debugger driver:
      inspects thunk trees via :print, shows variable bindings, and identifies
      unevaluated thunks (_t0, _t1) that can be forced.
      """
    )
  , ( "StrongheadRenderGraph"
    , """
      Produces publication-quality bespoke SVG heap profile graphs matching classic hp2ps
      PostScript styling with modern Stronghead formatting, typography, and empirical
      before/after diff layouts for issue reports and proof of fixes.
      """
    )
  , ( "StrongheadHeapAnatomy"
    , """
      Runs topological heap anatomy on a ghc-debug snapshot or live ghc-debug target:
      exact dominator retained bytes, folded quotient thunk/list spines, Merkle structural
      duplicate waste, and optional counterfactual 'what-if' severing simulation.
      """
    )
  , ( "StrongheadCriticalPath"
    , """
      Traces the causal critical path across Haskell threads, MVars, BlackHoles, STW GC
      pauses, and runnable queues in an .eventlog, plus STW rendezvous straggler blame,
      runnable latency percentiles, parallel sparks, non-moving GC, and OS MemReturn.
      """
    )
  , ( "StrongheadCoreSmells"
    , """
      Runs compiler-to-runtime forensics across GHC Core and STG dumps: detects
      runtime-weighted optimization smells (dictionary passing, broken fusion, boxed loops,
      updateable loop thunks, PAP calls), extracts static STG closure free-variable captures,
      and optionally performs alpha-normalized semantic Core diffs vs a baseline dump dir.
      """
    )
  , ( "StrongheadLiveStacks"
    , """
      Connects to a running Haskell program linking Stronghead.Agent v3 (auto-discovering
      local agent beacons when host/port are omitted) and retrieves live cross-platform
      GHC.Stack.CloneStack thread backtraces and registered ghc-heap value probes.
      """
    )
  , ( "StrongheadRtsSweep"
    , """
      Runs an automated RTS parameter sweep across a compiled Haskell binary (+RTS flags),
      ingests each run's eventlog, and computes the empirical Pareto frontier across
      throughput (duration), p99 GC pause latency, and peak heap footprint.
      """
    )
  , ( "StrongheadAssertBudget"
    , """
      Evaluates a declarative .stronghead-spec performance budget (file path or inline
      key=value rules) against eventlog, heap profile, baseline, and Core dump artifacts
      for automated CI regression gating.
      """
    )
  , ("path", "Path to the profiling artifact (.hp or .eventlog)")
  , ("dump_path", "Path to GHC dump directory or dump file (.dump-simpl, .dump-stg-final)")
  , ("top_n", "Maximum number of top suspects to return (default: 5)")
  , ("threshold", "Minimum leak score (0.0 to 1.0) to qualify as a suspect (default: 0.1)")
  , ("binding", "Function or binding name to inspect (e.g. 'sum', 'worker', 'go')")
  , ("module_name", "Optional module name to narrow the search (e.g. 'Main')")
  , ("include_stg", "Whether to include STG intermediate representation (default: true)")
  , ("top_threads", "Maximum number of threads to detail in contention breakdown (default: 8)")
  , ("baseline", "Path to baseline run (.hp or .eventlog)")
  , ("baseline_path", "Optional path to baseline .hp file to diff against")
  , ("current", "Path to current run (.hp or .eventlog)")
  , ("baseline_dumps", "Optional dump directory for baseline run")
  , ("current_dumps", "Optional dump directory for current run")
  , ("closure_address", "Hex closure address (e.g. '0x00000000045f8a00' or '45f8a00')")
  , ("band_name", "Name of heap band to search for in snapshot")
  , ("target", "Snapshot file path or host:port for live ghc-debug session")
  , ("command", "Command to execute")
  , ("host", "Agent host (default: '127.0.0.1')")
  , ("port", "Agent port (default: 4777)")
  , ("text", "Marker text (required if command is 'mark')")
  , ("timeout_ms", "Timeout in milliseconds (default: 3000)")
  , ("issue_type", "Type of issue: 'leak', 'contention', 'gc_pressure', 'hot_path', or 'general'")
  , ("entity", "Name of target band, thread id, or function")
  , ("file_path", "Path to source file, eventlog, or heap profile")
  , ("cwd", "Working directory for GHCi")
  , ("repl_cmd", "Command to start GHCi (default: 'cabal repl')")
  , ("output_path", "Optional file path where the generated SVG graph will be saved (.svg)")
  , ("theme", "Graph theme: 'classic' (hp2ps PostScript look), 'dark' (Stronghead dark mode), or 'light' (modern light)")
  , ("sever_closure", "Optional hex closure address to simulate severing in counterfactual what-if analysis")
  , ("sever_type", "Optional closure type or name to simulate severing globally in counterfactual what-if analysis")
  , ("snapshot_target", "Snapshot file path, host:port, or Unix socket path for ghc-debug heap anatomy")
  , ("thread_id", "Optional Haskell thread ID for causal critical-path attribution")
  , ("span_name", "Optional withSpan marker name to trace critical path for its slowest occurrence")
  , ("dumps_dir", "Path to GHC dump directory or single .dump-simpl / .dump-stg-final file")
  , ("filter_binding", "Optional binding name to filter smells, captures, and semantic Core diffs")
  , ("prof_path", "Optional .prof file path to weight Core/STG optimization smells by runtime hotness")
  , ("include_probes", "Whether to also query registered ghc-heap value probes (default: true)")
  , ("exe", "Path to the compiled Haskell executable to sweep")
  , ("args", "Optional whitespace-separated program arguments passed before +RTS")
  , ("grid", "Optional comma-separated RTS flag combinations (e.g. '-A16m,-A64m,-A64m -xn')")
  , ("spec", "Path to a .stronghead-spec file or inline key=value budget rules")
  , ("eventlog_path", "Optional .eventlog path to check against the runtime budget")
  , ("hp_path", "Optional .hp heap profile path to check against the runtime budget")
  ]
