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

### MCP server for AI assistants

Stronghead includes a Model Context Protocol (MCP) server that exposes its heap
profiler, Core/STG dump analyzer, and eventlog diagnostics to AI coding assistants
(Claude Desktop, Cursor, Antigravity).

Add to your MCP client configuration (e.g. `claude_desktop_config.json`):

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

To test over stdio or inspect available tools from the command line:

```sh
cabal run stronghead-mcp -- --tools
```

## License

MIT
