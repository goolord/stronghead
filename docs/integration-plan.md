# Cross-Panel Architecture

This document describes how Stronghead links data across its five panels (Timeline, Heap, Core & STG, Debugger, and Live).

## Entity References (`Stronghead.Ref`)

All cross-panel links use a shared `Ref` sum type. Time-keyed references include a `RunKey` (`program:pid`) because thread IDs and timestamps only apply to a single execution. Code-keyed references omit `RunKey` so bindings, cost centres, and source spans match across runs of the same build.

```haskell
newtype RunKey = RunKey Text

data Ref
  = RThread !RunKey !Word32
  | RInstant !RunKey !Word64          -- program time (ns)
  | RRange !RunKey !Word64 !Word64
  | RCollection !RunKey !Int
  | RCensus !RunKey !Int
  | RMarker !RunKey !Int
  | RSource !SrcSpan
  | RBinding !Text !Text              -- module, normalized name
  | RCostCentre !Text !Text           -- module, label
  | RBand !Text
  | RInfoTable !Word64
  | RClosure !Int !ClosureId          -- snapshot index, closure address
  | RThunkGroup !ThunkKey
  | RBreakpoint !FilePath !Int
  | RModule !Text
```

`refTarget` and `parseRefTarget` serialize and parse `Ref` values as slash-separated, percent-encoded strings (for example `thread/stronghead-sample:4242/12`). This format is used for rich-text hyperlinks, `.stronghead` session files, and clipboard references.

### Cross-Panel Joins (`Stronghead.Join`)

| Entity | Timeline | Heap Profile | Live Graph (`ghc-debug`) | Core & STG | Debugger | Live | Join Key |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Thread | Thread rows, spans, black-hole owner | — | TSO roots (`thread N`) | — | — | Thread list | `RunKey` + thread ID |
| Instant / Range | Ruler, spans | Census times | Snapshot time | — | — | Frames | Program time (ns) |
| Collection | GC strip | Post-GC census | — | — | — | Pause chart | Program time (ns) |
| Marker | Ruler, tracks | Chart axis | — | — | — | Chart axis | Program time (ns) |
| Source span | Sampled cost centres | `-hi` IPE span, `-hc` span | Closure source info | Binding ranges | Stop span, breakpoints | — | Resolved file path + line/column |
| Binding | — | `-hd` names, IPE label | Closure label | Top-level bindings | Stop binding, `:break` | — | Module + normalized name |
| Cost centre | Sampled cost centres | `-hc` stacks | — | Span overlap or name | — | — | Module + label, or `.prof` stack path |
| Info table | — | `-hi` band address | Closure info pointer | STG binder (`<name>_info`) | — | — | Address within a run; symbol name within a build |
| Closure type | — | `-hT`, `-hd` bands | Closure type / constructor | Boxing sites (`I#`, `D#`) | `:print` values | — | Closure type or constructor name |
| Thunk group | — | Thunk bands | Surviving thunk groups | STG `let` sites | `_t` bindings | — | Source location + retainer path shape (`ThunkKey`) |

### Build and RTS Flags by Feature

| Feature | Build Flags | RTS / Run Flags |
| --- | --- | --- |
| Eventlog timeline | `-threaded` | `+RTS -l` |
| Heap profile in eventlog | — | `+RTS -l -hT -i0.1` |
| Heap profile by source location | `-finfo-table-map -fdistinct-constructor-tables` | `+RTS -l -hi -i0.1` |
| Cost-centre profiling | `-prof -fprof-auto` | `+RTS -p` (add `-l` for timeline samples) |
| Core & STG dumps | `-ddump-simpl -ddump-stg-final -ddump-to-file` | — |
| Ticky-ticky counters | `-ticky` | `+RTS -lT` |
| Live telemetry agent | Link `stronghead:agent` | `+RTS -T` |
| Live closure graph | Link `ghc-debug-stub` (Unix only) | — |

## Runs and Clock Alignment (`Stronghead.Run`, `Stronghead.Clock`)

`RunInfo` records the program name, PID, command-line arguments, RTS flags, and start wall time extracted from eventlogs, `.hp` files, `.prof` headers, `ghc-debug` socket names, and the live agent.

- **Exact run match**: same program name and PID.
- **Probable run match**: same program name when one source has no PID (such as `.hp` or `.prof` files).
- **Separate runs**: different PIDs or program names; only code-keyed references link across them unless manually linked in the **Sources** popover.

Program time is measured in nanoseconds since RTS initialization:

- **Eventlog events and heap samples**: native eventlog timestamps.
- **Live agent frames**: `elapsed_ns + init_elapsed_ns` from `GHC.Stats.RTSStats` (falling back to arrival time if run without `+RTS -T`).
- **`.hp` files**: `.hp` timestamps record mutator CPU time rather than elapsed time. When an eventlog from the same run is loaded, `.hp` censuses are matched to eventlog censuses by total byte count; otherwise `.hp` times are marked approximate.
- **`ghc-debug` snapshots**: stamped using the live agent's program time or converted from wall time via the eventlog's `WallClockTime` anchor.

## Bus and Lenses (`UI.Link`)

The shell maintains a shared `Bus` holding the focused `Ref`, hovered `Ref`, shared time cursor, selected time range, navigation history, pinned items, and temporary timeline markers.

Each panel exports a `Lens` constructed each frame from the current loaded state:

- `lensReveal`: selects and scrolls/zooms to a `Ref` in that panel.
- `lensActions`: returns context-menu actions for a `Ref`.
- `lensPeek`: returns summary fields and match accuracy (`exact`, `by name`, `approximate`) for hover previews, the Inspector pane, and Pinboard exports.
- `lensSearch`: returns fuzzy-search candidates for the command palette (`Ctrl+K`).
- `lensAnnotations`: supplies per-line heat bars, gutter markers, and inline notes to `UI.Code`.
- `lensFacts`: supplies structured observations about the focused `Ref` to the rule-based suggestion engine (`Stronghead.Next`).

## Agent Wire Protocol v2 (`Stronghead.Agent`, `Stronghead.Live`)

The agent streams newline-delimited ASCII records over TCP (default `127.0.0.1:4777`) and accepts commands on the same socket. v2 remains backward-compatible with v1 clients and agents.

### Agent to Client

```text
H <program> <pid> <capabilities>
I version=2 cwd=<%-encoded> exe=<%-encoded> args=<%-encoded-csv> heap-profile=<mode|none> heap-interval-ms=<n> eventlog=<0|1> commands=<0|1>
S <key>=<value> ...
T <tid> <status> <label...>
C <tid> <capability>
E
A <id> ok [t=<ns>]
A <id> error <text>
```

### Client to Agent

```text
<id> mark <text>                -- traceMarkerIO
<id> gc                         -- performMajorGC
<id> census                     -- requestHeapCensus
<id> heap-profiling on|off      -- startHeapProfTimer / stopHeapProfTimer
<id> flush                      -- flushEventLog
```

All commands except `flush` also emit an eventlog marker. By default, commands are only enabled when the agent is bound to a loopback address (`agentAcceptCommands`).

## Known Limitations

- **Scrolling list views to a revealed row**: `nano-ui`'s `scrollIntoView` requires an internal `Context` not exposed by the public API. Revealing an item in a list or tree clears or sets the text filter instead of scrolling the list container directly.
- **Canvas hover previews**: timeline tracks, charts, and code canvases use their own lightweight hover tooltips rather than full multi-panel `Peek` popups; full cross-panel details are shown in the context menu and the Inspector pane.
- **Pop-out window sizing**: `nano-ui` floating windows use a fixed `720 x 460` layout region.
- **Windows `ghc-debug-stub`**: `ghc-debug-stub` requires Unix sockets and cannot be linked into native Windows executables; live closure snapshots require a Linux, macOS, or WSL target.
