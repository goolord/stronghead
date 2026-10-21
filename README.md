# Stronghead

A GUI profiler and debugger for GHC programs, built on [nano-ui](https://github.com/goolord/nano-ui). It combines an eventlog timeline, heap profiler, Core/STG viewer, GHCi debugger, and live RTS monitor.

## Usage

```sh
cabal run stronghead -- [--tab NAME] [--connect HOST:PORT] [--follow FILE.eventlog]
                        [--debug MODULE.hs] [--goto REF] [--mark TEXT] [FILE|DIR ...]
```

Files are routed by extension (`.eventlog`, `.hp`, `.prof`, `.dump-simpl`, `.dump-stg-final`, `.hs`, `.stronghead`). Passing a directory searches it for Core and STG dumps. Files can also be opened from the header bar or dragged onto the window.

- `--tab NAME`: open on `timeline`, `heap`, `core`, `debugger`, or `live`.
- `--connect HOST:PORT`: connect to a running program's `Stronghead.Agent` socket.
- `--follow FILE.eventlog`: tail an eventlog as it is written.
- `--debug MODULE.hs`: start a GHCi session on a module.
- `--goto REF`: jump to an entity reference (for example `source/sample%2FMain.hs/82/1/82/1`).
- `--mark TEXT`: send an eventlog marker to a connected program after connecting.

Settings (`Ctrl+,`) are saved to `%APPDATA%\stronghead\settings.conf` on Windows and `~/.config/stronghead/settings.conf` elsewhere. Command-line flags override saved settings.

## Panels

### Timeline

Reads `.eventlog` files incrementally with `ghc-events`, supporting both completed logs and files still being written.

- **Capabilities and threads**: per-capability tracks (running, GC, blocked ticks, migrations, spark steals, wakeups) and per-thread rows with blocking reasons (MVar, STM, black hole owner, foreign call, `throwTo`).
- **GC and heap strips**: pause durations (minor vs. major), heap size, live bytes, and heap census ticks.
- **Markers**: `traceMarkerIO` and `traceEventIO` events, searchable in the **Markers** pane.
- **Cost-centre samples**: for profiled builds (`-prof -fprof-auto`, `+RTS -p -l`), draws a sampled call-stack track under each capability and shows per-slice and per-thread CPU profiles.
- **Ticky counters**: for ticky builds (`-ticky`, `+RTS -lT`), shows closure entry counts and allocations across the overview, selected time ranges, and Core/STG views.

Scroll to zoom, drag to pan, double-click to fit, drag the ruler to move the cursor, and `Shift+drag` to select a time range. `[` and `]` step between collections (`Shift+[` and `Shift+]` step between censuses).

### Heap

Displays heap profiles from eventlogs (`+RTS -l -hT`, `-hi`, `-hc`, `-hr`) or `.hp` files, and cost-centre trees from `.prof` files.

- **Residency chart**: stacked bands over time with a census cursor.
- **At cursor**: band sizes and deltas at the selected census.
- **Leak suspects**: bands ranked by sustained growth, plus thunk bands that survive consecutive major GCs without shrinking.
- **Retention and cost centres**: `-hc` and `-hr` retention trees, and `.prof` call trees with individual and inherited time and allocation.
- **Info-table profiles (`-hi`)**: bands are labeled with their source location and closure description when compiled with `-finfo-table-map`.

#### Live closure graph (`ghc-debug`)

Build with `-f ghc-debug` to inspect live heap snapshots from a target linked with `ghc-debug-stub`:

```haskell
import GHC.Debug.Stub (withGhcDebug)

main :: IO ()
main = withGhcDebug $ do
  ...
```

Build the target with `-threaded -finfo-table-map -fdistinct-constructor-tables`, then run Stronghead with:

```sh
cabal run -f ghc-debug stronghead
```

- **Closure graph**: browse reachable closures from GC roots with shallow and retained sizes, shortest retainer paths, and thread stack frames.
- **Surviving thunks**: compare consecutive snapshots to find unevaluated thunks that persist across GCs.
- **Biggest retainers**: closures retaining the most memory.

`ghc-debug-client` builds on Linux, macOS, and Windows, but `ghc-debug-stub` (linked into the target program) requires Unix sockets and only builds on Linux and macOS. On Windows, connect over TCP (`withGhcDebugTCP`) to a target running in WSL or on a remote machine, or open a saved snapshot file.

### Core & STG

Pairs Haskell source files with `-ddump-simpl` and `-ddump-stg-final` dumps:

```sh
cabal build --ghc-options="-ddump-simpl -ddump-stg-final -ddump-to-file"
```

- **Bindings list**: shows top-level bindings with badges for `INLINE`/`NOINLINE`, worker/wrapper splits, boxing sites, STG `let` allocations, and bindings eliminated by inlining. When `.prof`, `-hi`, or ticky data is loaded, bindings can be filtered and sorted by time, allocation, live heap, or entry count.
- **Synchronized panes**: selecting a binding aligns Source, Core, and STG. Hovering an identifier highlights matching names across all three panes (ignoring GHC unique suffixes and `$w` prefixes).
- **Inline metrics**: source lines and STG `let`/boxing sites display heat bars and byte/time annotations from loaded `.prof`, `-hi`, ticky, and `ghc-debug` data.

### Debugger

A graphical interface for `ghci` / `cabal repl`.

- Set and toggle breakpoints in the source gutter or from any source link in other panels.
- Step into, step over (`:steplocal`), step out (`:stepout`), continue, trace (`:trace`), and inspect `:history`.
- **Local bindings**: parses `:print` output into expandable trees. Unevaluated `_t` thunks are highlighted with their types and can be evaluated to WHNF (`seq`) or fully forced (`:force`) in place.
- Includes a console for running raw GHCi commands.

### Live

Streams runtime statistics and thread states from a running program linked with `stronghead:agent` (which depends only on `base`, `bytestring`, and `network`):

```haskell
import Stronghead.Agent

main :: IO ()
main = withAgent defaultAgentConfig $ do
  ...
```

Build the target with `-threaded` and run with `+RTS -T`. By default the agent listens on `127.0.0.1:4777` and streams frames every 250 ms.

Connecting to the agent also locates and opens the target's eventlog, `.hp` file, `ghc-debug` socket, and Core/STG dumps if present in its working directory. From the Live toolbar or command palette, you can send markers (`M`), trigger a major GC, request a heap census, toggle heap profiling, or take `ghc-debug` snapshots. Commands are only accepted on loopback addresses unless enabled in `AgentConfig`.

## Navigation and Workspace

- **Cross-panel links**: threads, collections, censuses, heap bands, closures, cost centres, bindings, and source lines are linked across panels. Hover to preview details from other panels, right-click for panel actions, and `Ctrl+click` to jump to the item's primary panel (`Alt+Left` / `Alt+Right` to go back and forward).
- **Command palette (`Ctrl+K`)**: search loaded threads, bindings, cost centres, bands, closures, markers, and commands. `Ctrl+1` through `Ctrl+5` switch panels.
- **Shared clock**: Timeline, Heap, and Live charts from the same run share a time cursor and `Shift+drag` range selection.
- **Workspaces and panes**: open the **Workspace** menu in the header to add, rearrange, or pop out panes across panels, or switch between layout presets (**Leak hunt**, **Latency**, **Before and after**). Additional panes include:
  - **Inspector**: shows all cross-panel data, links, actions, and suggested next steps for the focused item.
  - **Pinboard**: bookmark items with notes and export them as a Markdown report.
  - **Watchers**: rules that monitor live streams (heap growth across major GCs, long thread blocks, high GC CPU share, growing heap bands) and trigger markers, censuses, snapshots, or notifications.
  - **Baselines and diffs**: mark a loaded run as a baseline to compare metrics and view heap, cost-centre, and Core/STG binding diffs against a new run.
- **Sessions (`.stronghead`)**: save and restore open files, connections, pins, watcher rules, baseline paths, and pane layouts from the **Workspace** menu.

## Sample Program

`stronghead-sample` is a test executable with MVar contention, a lazy `IORef` accumulator leak, and an unbounded map:

```sh
cabal run stronghead-sample -- 10 +RTS -l -hT -i0.1 -T -RTS
cabal run stronghead -- stronghead-sample.eventlog
```

To attach while it is running:

```sh
cabal run stronghead -- --connect 127.0.0.1:4777
```

## Building

Requires GHC 9.10+ (tested with 9.14), `cabal`, and `SDL3` + `SDL3_ttf` (via `pkg-config`). `nano-ui` is fetched from GitHub via `cabal.project`.

```sh
cabal build
cabal test
```

## Project Structure

| Path | Description |
| --- | --- |
| `src/Stronghead/Eventlog*.hs` | Incremental eventlog decoder and timeline data structures |
| `src/Stronghead/HeapProfile.hs` | `.hp` parser, band classification, leak scoring, retention trees |
| `src/Stronghead/Prof.hs` | `.prof` parser |
| `src/Stronghead/Dump.hs` | Core, STG, and source tokenizer and binding extractor |
| `src/Stronghead/Ghci.hs` | GHCi subprocess driver and output parsers |
| `src/Stronghead/Source.hs` | Source spans and file path resolution |
| `src/Stronghead/Join.hs` | Cross-artifact matching (spans, cost centres, info tables, ticky, STG) |
| `src/Stronghead/Live.hs` | Client for the `Stronghead.Agent` wire protocol |
| `src/Stronghead/Attach.hs` | Discovery of a connected process's eventlog, profiles, socket, and dumps |
| `src/Stronghead/Debug*.hs` | `ghc-debug` client, closure graph model, retainer paths, survivor analysis |
| `src/Stronghead/Session.hs` | `.stronghead` session serialization and Markdown report formatting |
| `src/Stronghead/Next.hs` | Rule-based investigation suggestions |
| `src/Stronghead/Watch.hs` | Stream watcher evaluation |
| `src/Stronghead/Diff.hs` | Heap, cost-centre, and binding diffing |
| `agent/Stronghead/Agent.hs` | Embedded telemetry agent library (`stronghead:agent`) |
| `app/UI/*.hs` | `nano-ui` views and panels |
| `sample/Main.hs` | Sample executable (`stronghead-sample`) |

## Limitations

- Without `-f ghc-debug`, retention analysis uses `-hc`, `-hr`, or `-hi` heap profiles. With `ghc-debug`, retained sizes are computed along a breadth-first shortest-path tree rather than a dominator tree, and taking a snapshot pauses the target process up to the closure limit (default 200k, max 1M). Weak pointers are not traversed.
- `ghc-debug-stub` does not build on Windows because it depends on Unix domain sockets.
- Eventlogs only include call stacks in profiled builds (`+RTS -p -l`), and on Windows the RTS profiling timer ticks roughly every 15 ms rather than 1 ms.
- `ghc-events` does not support GHC thread-stop status code 21 (WinIO completion) and fails to decode eventlogs containing it.
