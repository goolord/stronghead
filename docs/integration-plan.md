# Cross-Panel Architecture

Stronghead links data across its five panels (Timeline, Heap, Core & STG,
Debugger, and Live) through shared entity references, run matching, a common
program clock, and per-panel lenses.

## Entity References (`Stronghead.Ref`)

Cross-panel links use the `Ref` sum type. Time-keyed references include a
`RunKey` (`program:pid`) because thread IDs and timestamps only apply to a
single run. Code-keyed references omit `RunKey` so bindings, cost centres, and
source spans match across runs of the same build.

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

`refTarget` and `parseRefTarget` convert `Ref` values to and from
slash-separated, percent-encoded strings (for example
`thread/stronghead-sample:4242/12`) for hyperlinks, `.stronghead` session
files, and clipboard references.

## Runs and Clock Alignment (`Stronghead.Run`, `Stronghead.Clock`)

`RunInfo` records the program name, PID, command-line arguments, RTS flags, and
start wall time extracted from eventlogs, `.hp` files, `.prof` headers,
`ghc-debug` socket names, and the live agent. Artifacts with the same program
name and PID match exactly; artifacts with the same program name where one side
has no PID (such as `.hp` or `.prof` files) are treated as a probable match.

Program time is measured in nanoseconds since RTS initialization:

- Eventlog events and heap samples use native eventlog timestamps.
- Live agent frames use `elapsed_ns + init_elapsed_ns` from
  `GHC.Stats.RTSStats`, falling back to arrival time when run without
  `+RTS -T`.
- `.hp` timestamps record mutator CPU time rather than elapsed time. When an
  eventlog from the same run is loaded, `.hp` censuses are paired with eventlog
  censuses by total byte count; otherwise `.hp` times are treated as
  approximate.
- `ghc-debug` snapshots are stamped using the live agent's program time or
  converted from wall time via the eventlog's `WallClockTime` anchor.

## Bus and Lenses (`UI.Link`)

`Bus` holds the focused `Ref`, hovered `Ref`, shared time cursor, selected time
range, navigation history, pinned items, and temporary timeline markers. Each
panel builds a `Lens` each frame from its loaded state to handle revealing a
`Ref`, building context-menu actions and hover previews, answering command
palette searches, and providing per-line code annotations to `UI.Code`.

## Agent Wire Protocol v2 (`Stronghead.Agent`, `Stronghead.Live`)

The agent streams newline-delimited ASCII records over TCP (default
`127.0.0.1:4777`) and accepts commands on the same socket. v2 is
backward-compatible with v1 clients and agents.

Agent to client:

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

Client to agent:

```text
<id> mark <text>                -- traceMarkerIO
<id> gc                         -- performMajorGC
<id> census                     -- requestHeapCensus
<id> heap-profiling on|off      -- startHeapProfTimer / stopHeapProfTimer
<id> flush                      -- flushEventLog
```

All commands except `flush` also emit an eventlog marker. By default, commands
are only enabled when the agent is bound to a loopback address
(`agentAcceptCommands`).
