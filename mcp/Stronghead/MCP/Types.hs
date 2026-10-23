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
  , ("path", "Path to the profiling artifact (.hp or .eventlog)")
  , ("dump_path", "Path to GHC dump directory or dump file (.dump-simpl, .dump-stg-final)")
  , ("top_n", "Maximum number of top suspects to return (default: 5)")
  , ("threshold", "Minimum leak score (0.0 to 1.0) to qualify as a suspect (default: 0.1)")
  , ("binding", "Function or binding name to inspect (e.g. 'sum', 'worker', 'go')")
  , ("module_name", "Optional module name to narrow the search (e.g. 'Main')")
  , ("include_stg", "Whether to include STG intermediate representation (default: true)")
  , ("top_threads", "Maximum number of threads to detail in contention breakdown (default: 8)")
  , ("baseline", "Path to baseline run (.hp or .eventlog)")
  , ("current", "Path to current run (.hp or .eventlog)")
  , ("baseline_dumps", "Optional dump directory for baseline run")
  , ("current_dumps", "Optional dump directory for current run")
  , ("closure_address", "Hex closure address (e.g. '0x00000000045f8a00' or '45f8a00')")
  , ("band_name", "Name of heap band to search for in snapshot")
  , ("target", "Snapshot file path or host:port for live ghc-debug session")
  , ("command", "Command to execute")
  , ("host", "Agent host (default: '127.0.0.1')")
  , ("port", "Agent port (default: 9100)")
  , ("text", "Marker text (required if command is 'mark')")
  , ("timeout_ms", "Timeout in milliseconds (default: 3000)")
  , ("issue_type", "Type of issue: 'leak', 'contention', 'gc_pressure', 'hot_path', or 'general'")
  , ("entity", "Name of target band, thread id, or function")
  , ("file_path", "Path to source file, eventlog, or heap profile")
  , ("cwd", "Working directory for GHCi")
  , ("repl_cmd", "Command to start GHCi (default: 'cabal repl')")
  ]
