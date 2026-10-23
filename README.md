# Stronghead

A profiler and debugger GUI for GHC programs, built on
[nano-ui](https://github.com/goolord/nano-ui). It brings eventlogs, heap and
cost-centre profiles, Core and STG dumps, GHCi debugging, and live runtime
telemetry into one window.

https://github.com/user-attachments/assets/e032a2e9-78a7-4ab8-89d2-284be45da7c6

## Features

- Eventlog timeline with per-capability tracks, GC pauses, heap size, thread
  states and blocking reasons, `traceMarkerIO` markers, profiled cost-centre
  samples (`+RTS -p -l`), and ticky counters (`+RTS -lT`), with support for
  tailing logs as they are written
- Heap profiler for `.hp` files and eventlog heap samples (`-hT`, `-hi`, `-hc`,
  `-hr`), with residency charts, leak and surviving-thunk detection, retention
  trees, and `.prof` call trees
- Side-by-side Haskell source, Core (`-ddump-simpl`), and STG
  (`-ddump-stg-final`) viewer with inline time, allocation, and live-heap
  annotations
- GHCi debugger frontend with source breakpoints, stepping, execution history,
  and expandable `:print` value trees where individual `_t` thunks can be
  evaluated or forced in place
- Live RTS monitor and process control via the `stronghead:agent` library, plus
  optional live closure graph snapshots through
  [ghc-debug](https://gitlab.haskell.org/ghc/ghc-debug) (`-f ghc-debug`)
- Cross-panel navigation, shared time cursor and range selection, custom pane
  layouts, stream watchers, and baseline run diffs

## Usage

Building requires GHC 9.10 or newer, Cabal, `pkg-config`, SDL3, and SDL3_ttf.

```sh
cabal run stronghead -- [FILE|DIR ...]
```

Files are opened by extension (`.eventlog`, `.hp`, `.prof`, `.dump-simpl`,
`.dump-stg-final`, `.hs`, `.stronghead`), or dragged onto the window. Passing a
directory loads all Core and STG dumps inside it.

```sh
cabal run stronghead-sample -- 10 +RTS -l -hT -i0.1 -T -RTS
cabal run stronghead -- stronghead-sample.eventlog
cabal run stronghead -- --follow stronghead-sample.eventlog
cabal run stronghead -- --connect 127.0.0.1:4777
cabal run stronghead -- --debug sample/Main.hs
```

### Live agent

To stream live RTS stats and thread states from a running program, depend on
`stronghead:agent`, compile with `-threaded`, and run with `+RTS -T`:

```haskell
import Stronghead.Agent

main :: IO ()
main = withAgent defaultAgentConfig $ do
  ...
```

### Live closure graphs (`ghc-debug`)

Build Stronghead with `cabal run -f ghc-debug stronghead` and wrap the target
program with `withGhcDebug` from `ghc-debug-stub` (compiled with
`-threaded -finfo-table-map -fdistinct-constructor-tables`):

```haskell
import GHC.Debug.Stub (withGhcDebug)

main :: IO ()
main = withGhcDebug $ do
  ...
```

### Model Context Protocol (MCP) Server for AI Assistants

Stronghead includes a native [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) server built on the [`mcp-server`](https://hackage.haskell.org/package/mcp-server) library. It exposes Stronghead's profiling and optimization analysis engines directly to AI coding assistants (Claude Desktop, Cursor, Zed, Antigravity, Claude Code).

Instead of forcing AI agents to guess where to insert `BangPatterns` or speculative `seq`, Stronghead MCP gives LLMs direct semantic observability into GHC Core/STG optimization decisions, memory residency, and runtime bottlenecks.

#### Running the MCP Server

```sh
# Start stdio MCP server (standard JSON-RPC over stdin/stdout)
cabal run stronghead-mcp

# Or inspect available tools
cabal run stronghead-mcp -- --tools
```

#### Client Configuration (e.g., `claude_desktop_config.json` or Antigravity `mcp_config.json`)

```json
{
  "mcpServers": {
    "stronghead": {
      "command": "cabal",
      "args": ["run", "stronghead-mcp", "--"]
    }
  }
}
```

#### Available MCP Tools

| Tool | Purpose | Key Inputs |
|:---|:---|:---|
| `stronghead_diagnose_leak` | Pinpoint space leaks, calculate suspect scores (0-1), growth slopes, and cost-centre retention trees | `path` (`.hp` or `.eventlog`), `top_n`, `threshold` |
| `stronghead_inspect_core` | Inspect optimized Core & STG for worker-wrapper, strictness signatures, constructor boxing, and STG `let` allocations | `binding`, `dump_path`, `module_name`, `include_stg` |
| `stronghead_eventlog_summary` | Aggregate GC pause percentiles (p50, p90, p95, p99, max), mutator/GC time, and thread contention breakdown (MVar, STM) | `path` (`.eventlog`), `top_threads` |
| `stronghead_diff_runs` | Empirically compare baseline vs current profiling runs to verify memory and latency improvements | `baseline`, `current`, `baseline_dumps`, `current_dumps` |
| `stronghead_retainer_path` | Trace shortest retainer path from GC roots via `ghc-debug` (Root $\to$ `IORef` $\to$ `Thunk` $\to$ `Data`) | `closure_address`, `band_name`, `target` |
| `stronghead_live_control` | Remotely trigger major GC, take heap censuses, toggle profiling, or query live RTS stats via `Stronghead.Agent` | `command`, `host`, `port`, `text` |
| `stronghead_recommend_next_steps` | Generate prescriptive GHC compiler & RTS flags for leaks, contention, or GC pressure | `issue_type`, `entity`, `file_path` |
| `stronghead_ghci_eval` | Inspect thunk trees and unevaluated `_t` closures via GHCi `:print` | `command`, `cwd`, `repl_cmd` |

## License

MIT
