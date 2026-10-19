# Stronghead

A live debugger and profiler GUI for GHC programs, built on
[nano-ui](https://github.com/goolord/nano-ui). It goes after the opaque parts
of running Haskell: what the capabilities and threads are doing, where GC time
goes, which thunks are piling up, what the optimiser did to a function, and
what is and isn't evaluated at a breakpoint.

```sh
cabal run stronghead -- [--tab NAME] [--connect HOST:PORT] [--follow FILE.eventlog]
                        [--debug MODULE.hs] [FILE|DIR ...]
```

Files are routed by extension (`.eventlog`, `.hp`, `.prof`, `.dump-simpl`,
`.dump-stg-final`, `.hs`); a directory is searched for Core/STG dumps. Files can
also be opened from the header or dropped on the window.

## Panels

### Timeline (eventlog)

A ThreadScope replacement. The eventlog is read with `ghc-events`' incremental
decoder, so the same code loads a finished multi-gigabyte log (progressively,
with a progress bar) and **follows a log the program is still writing**.

- One track per capability: green while running Haskell code, yellow in GC, a
  red tick where a thread stopped because it blocked. Markers show thread
  migrations, spark steals and cross-capability wakeups.
- A GC pressure strip (pause per collection, log-scaled; minor vs. major) and a
  heap strip (heap size and live bytes).
- One row per Haskell thread: running, **blocked** (red, with the reason: MVar,
  STM, black hole and its owner, foreign call, throwTo ...) and runnable.
- `traceMarkerIO` markers run through every track; `traceEventIO` messages are
  ticks on the ruler.
- Click any slice for the inspector: thread, label, duration, why it stopped,
  the process allocation rate during the slice, the thread's whole history
  (time running, time off-CPU by reason, capabilities). With nothing selected
  the inspector lists the longest GC pauses and the most-blocked threads.

Wheel zooms around the pointer, drag pans, double-click fits. Each track keeps
prefix sums of time per span kind, so a zoomed-out pixel column is two binary
searches however many spans it covers.

### Heap & leaks

Heap profiles from either the eventlog's heap samples (`+RTS -l -hT`, `-hi`,
...) or a `.hp` file, plus `.prof` cost-centre profiles.

- Stacked residency bands over time, with a legend to hide or solo bands and a
  scrub cursor.
- **At cursor**: every band at a census, sortable by size or by change since
  the previous census.
- **Leak suspects**: bands ranked by how leak-like their curve is (steady
  growth, never shrinking, growth factor, rate), each with a plain-language
  diagnosis.
- **Thunk accumulator sentinel**: thunk bands and how many consecutive censuses
  (each one after a major GC) they have survived without shrinking: thunks
  that are never forced.
- **Retention tree**: for `-hc` profiles, cost-centre stacks rooted at `MAIN`
  or a CAF and read down to the allocation site; for `-hr`, what retains each
  band.
- **Cost centres**: the `.prof` call tree with individual and inherited
  time/allocation.
- **Live graph (ghc-debug)**: exact retainer paths from the running program's
  closure graph, below.

#### Live closure graph (ghc-debug)

Optional, behind the `ghc-debug` cabal flag (off by default). A heap profile
says what *kind* of closure keeps growing; [ghc-debug](https://gitlab.haskell.org/ghc/ghc-debug)
says which closure holds it. With **Source: Live graph (ghc-debug)**, **Take
snapshot** connects to a program running ghc-debug-stub, pauses it, walks
every closure reachable from its GC roots (threads and stable pointers, and
through static reference tables the CAFs), and resumes it.

- **Closure graph**: the roots, biggest first, expanded pointer by pointer.
  Each closure shows its type, its size and the bytes it *holds* (each closure
  charged to its first retainer on a shortest path). Selecting one shows its
  shortest retainer path, summed up as e.g. "IS held by thunk plus, rooted at
  CAF counter", and what it points to. Thunks are amber.
- **Surviving thunks**: take snapshots a few seconds apart. Thunks still
  unevaluated in consecutive snapshots are listed, growing groups first, and
  shown red in the graph. The collector moves objects, so thunks are matched
  across snapshots by their code (name and source span) and by the shape of
  the path retaining them from the nearest CAF, with repeats collapsed so a
  growing chain keeps one identity.
- **Biggest retainers**: the closures holding the most bytes.

Set up the program being debugged (build it `-threaded`; add
`-finfo-table-map -fdistinct-constructor-tables` so closures carry source
names and spans):

```haskell
import GHC.Debug.Stub (withGhcDebug) -- from ghc-debug-stub ^>=0.8

main :: IO ()
main = withGhcDebug $ do
  ...
```

`withGhcDebug` listens on a Unix socket under ghc-debug's data directory
(`~/.local/share/ghc-debug/debuggee/sockets/<pid>-<prog>`, or
`$GHC_DEBUG_SOCKET`) and prints its path; **Find sockets** lists them.
`withGhcDebugTCP "0.0.0.0" 1235` listens on TCP instead. Then build Stronghead
with the flag:

```sh
cabal run -f ghc-debug stronghead
```

(or put `flags: +ghc-debug` under `package stronghead` in
`cabal.project.local`). With the flag, `stronghead-sample` runs the stub too
(on TCP when `GHC_DEBUG_TCP=host:port` is set), so on Linux or macOS
`cabal run -f ghc-debug stronghead-sample -- 60` gives the live graph a leak
to find.

**Platforms.** ghc-debug 0.8 supports GHC 9.14. The client side
(ghc-debug-client, -common, -convention) builds on Linux, macOS and Windows,
so Stronghead builds with the flag everywhere. ghc-debug-stub, which the
*target* links, is Unix-only: its C++ needs `sys/socket.h` and `sys/un.h` and
fails to build on Windows (`stronghead-sample` leaves it out there). From
Windows, run the target on Linux, macOS or WSL with `withGhcDebugTCP` and
connect over **TCP** (WSL2 forwards `localhost`), or open a **snapshot file**
saved with ghc-debug's `GHC.Debug.Snapshot.makeSnapshot`.

### Core / STG

Point it at a folder of `-ddump-to-file` dumps (cabal puts them under
`dist-newstyle/`) and it pairs each module's source, Core and STG.

- Bindings are listed by name with what happened to them: worker/wrapper
  (`W/W`), `INLINE`/`NOINLINE`, boxing sites, STG `let` allocations, and
  `gone` when a source binding no longer exists in Core (inlined away).
- Selecting a binding lines up all three panes and explains it: arity,
  strictness signature, inline pragma, size, "Re-boxes values ...",
  "N let-bound closures in STG ...", with buttons that jump to each boxing
  and allocation site.
- Hovering any name highlights every occurrence in all three panes (uniques,
  module qualifiers and `$w` prefixes are normalised away, so `go` in the
  source lights up `Main.$wgo` and `go_s1x2`); clicking a name jumps to its
  binding. **Hide uniques** strips `_a1Bc`-style suffixes from the display.

```sh
ghc -O2 -ddump-simpl -ddump-stg-final -ddump-to-file Main.hs
cabal build --ghc-options="-ddump-simpl -ddump-stg-final -ddump-to-file"
```

### Debugger (GHCi)

A visual front end to GHCi's debugger (`ghci`, `cabal repl`, or any command
that starts one).

- Source view with candidate breakpoint lines; click the gutter to set or
  delete a breakpoint.
- Step into, step over (`:steplocal`), step out (`:stepout`), step module,
  continue, trace, history, back/forward, abandon. The current span is
  highlighted and followed.
- **Local bindings** at each stop, marked as evaluated or `_thunk`, with
  **Force WHNF**, **Force NF** and `:print` per binding.
- A console showing every command and its output, plus a prompt for raw GHCi
  commands.

`--debug path/to/Main.hs` starts a session on that module straight away.

### Live

A "top" for a running Haskell service. Link the target with the
`stronghead:agent` library (it depends only on `base`, `bytestring` and
`network`):

```haskell
import Stronghead.Agent

main :: IO ()
main = withAgent defaultAgentConfig $ do
  ...
```

Build it `-threaded` and run it with `+RTS -T`. The agent listens on
`127.0.0.1:4777` and streams RTS statistics and the live thread list (labels
and blocking status) four times a second. The panel charts allocation rate,
live bytes, GC pauses and thread count. If the program also writes an eventlog
(`+RTS -l -ol<file>`), the agent flushes it on the same cadence, and
**Follow in Timeline** (or `--follow`) tails it live.

## Try it

`stronghead-sample` misbehaves on purpose: six workers contend on one MVar, an
`IORef` is updated lazily (a classic thunk leak), a cache grows without bound,
and the threads are labelled.

```sh
cabal run stronghead-sample -- 10 +RTS -l -hT -i0.1 -RTS
cabal run stronghead -- stronghead-sample.eventlog
```

While it runs, `cabal run stronghead -- --connect 127.0.0.1:4777` shows it
live.

## Building

GHC 9.10 or newer (developed with 9.14), cabal, and SDL3 plus SDL3_ttf
(found through `pkg-config`) for nano-ui's SDL backend. nano-ui is not on
Hackage. `cabal.project` pulls it from GitHub, pinned to a commit; to move to
the current HEAD, update the `tag` there from

```sh
git ls-remote https://github.com/goolord/nano-ui HEAD
```

`cabal test` runs the parser and analysis tests (the ghc-debug graph analyses
included, with or without the flag). `-f ghc-debug` adds the ghc-debug
backend; see [Live closure graph](#live-closure-graph-ghc-debug).

## Layout

| Path | What it is |
| --- | --- |
| `src/Stronghead/Eventlog*.hs` | Incremental eventlog fold, timeline model and its level-of-detail queries |
| `src/Stronghead/HeapProfile.hs` | `.hp` parsing, band classification, leak suspects, retention tree |
| `src/Stronghead/Prof.hs` | `.prof` cost-centre tree |
| `src/Stronghead/Dump.hs` | Core/STG/source tokenising, bindings, optimisation notes, dump discovery |
| `src/Stronghead/Ghci.hs` | GHCi subprocess driver and output parsers |
| `src/Stronghead/Live.hs` | Client for the agent's protocol |
| `src/Stronghead/Debug.hs` | ghc-debug snapshots (the backend behind the `ghc-debug` flag) |
| `src/Stronghead/Debug/Graph.hs` | Closure graph model: retainer paths, held bytes, thunk survival across snapshots |
| `src/Stronghead/Debug/Client.hs` | ghc-debug-client traversal (built only with the flag) |
| `agent/Stronghead/Agent.hs` | The drop-in agent |
| `app/UI/*.hs` | The nano-ui panels |
| `sample/Main.hs` | The deliberately leaky sample program |

## Limits

- Without the `ghc-debug` flag, retention data comes from heap profiles
  (`-hc`, `-hr`, `-hi`). With it, "held" bytes follow the breadth-first
  shortest-path tree, not dominators: a closure reachable two ways counts once,
  under the retainer found first. A snapshot pauses the target for the whole
  walk (one socket round trip per closure) and stops at the closure limit
  (200k by default, up to 1M). Weak pointers are not followed.
- ghc-debug-stub does not build on Windows, so a Windows target can't be
  snapshotted; see [Platforms](#live-closure-graph-ghc-debug).
- The eventlog carries no stack traces, so the thread inspector shows labels,
  timing and blocking reasons, not stacks.
- ghc-events' thread-stop table predates GHC's renumbering of blocking
  reasons, so Stronghead reinterprets codes 16 and up (C calls, `throwTo`).
  GHC's raw code 21 (WinIO completion) still makes ghc-events fail to decode.
