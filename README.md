# Stronghead

A live debugger and profiler GUI for GHC programs, built on
[nano-ui](https://github.com/goolord/nano-ui). It goes after the opaque parts
of running Haskell: what the capabilities and threads are doing, where GC time
goes, which thunks are piling up, what the optimiser did to a function, and
what is and isn't evaluated at a breakpoint.

```sh
cabal run stronghead -- [--tab NAME] [--connect HOST:PORT] [--follow FILE.eventlog]
                        [--debug MODULE.hs] [--goto REF] [--mark TEXT] [FILE|DIR ...]
```

Files are routed by extension (`.eventlog`, `.hp`, `.prof`, `.dump-simpl`,
`.dump-stg-final`, `.hs`, and `.stronghead` for a [session](#sessions)); a
directory is searched for Core/STG dumps. Files can also be opened from the
header or dropped on the window. `--goto` opens an
entity by the reference **Copy reference** gives
(`source/sample%2FMain.hs/82/1/82/1` is line 82 of the sample), once a panel
has loaded it. `--mark` sends a connected program a mark once its agent
has streamed a couple of seconds, for scripts and screenshots.

**Settings** (the header button, or Ctrl+,) holds the theme (system, light,
dark), the interface scale (follow the display's DPI scaling, or 75–300%;
Ctrl+= / Ctrl+- step it, Ctrl+0 goes back to following the display), which
panel to open on, the defaults for the Live agent address, the GHCi
command and the Core view, the **project root** sources are looked up under,
and the **editor command** that "Open in editor" runs
(`code -g {file}:{line}:{col}`, say). They are saved as they change, with the window's
size and position on quit, to `settings.conf` in the user config directory
(`%APPDATA%\stronghead` on Windows, `~/.config/stronghead` elsewhere). The
command line wins over them. **Reopen the last session on start** (off by
default) brings back the session saved on quit; see [Sessions](#sessions).

## Links between panels

Threads, collections, markers, heap bands, censuses, cost centres, closures,
thunk groups, bindings and source lines are links wherever they appear, and
the same entity in every panel.

- **Resting the pointer** on one peeks at what every panel knows about it.
  Rows for the same entity in other panels are washed faintly.
- **Right-click** lists every panel that can show it ("Show in Timeline"),
  what goes with it ("Go to allocation site", "Go to thread"), and the
  panels' actions ("Break here (GHCi)"). Actions that can't run yet say why.
- **Ctrl+click** goes to it in its home panel. **Alt+←** / **Alt+→**, the
  mouse's back and forward buttons, or the arrows in the header retrace your
  steps.
- **Ctrl+K** searches everything loaded: threads, bindings, cost centres,
  bands, closures, markers, and commands.
- **Ctrl+1** to **Ctrl+5** switch panels. A click focuses an entity, and the
  status bar shows the focus; **Esc** clears it.
- Each panel's facts in a peek say how that panel matched the entity:
  **exact** (the same thread id in the same run, an info table through the
  IPE map), **by name** (a binding by module and name, a `-hT` band by its
  closure type) or **approximate** (a `.hp` file's clock).

Time-keyed links (a thread, a census, an instant) join artifacts from the
same run, matched by program name and pid. Code-keyed links (a binding, a
source line) join across runs.

### One clock

The Timeline, the heap chart and Live's charts share one cursor and one
range in program time, for every view of the same run.

- **Moving the cursor anywhere moves it everywhere.** Click the Timeline or
  drag its ruler, drag the heap chart, or click or drag a Live chart. The
  heap chart snaps it to the nearest census. **[** and **]** step it through
  collections on the Timeline (with Shift, censuses) and censuses on the
  heap chart.
- **Shift+drag** on any of them selects a range, and every other view of the
  run shades it; Shift+click clears it. **Analyse range** in the Timeline's
  inspector, or on a range's right-click menu, lists the collections and
  pauses, allocation, the threads that ran, heap growth, censuses and
  markers inside it, all as links.
- `traceMarkerIO` markers show on every time axis of the run, and so do
  the marks, actions and snapshots Stronghead sends a connected program
  ([Live](#live)).
- A `.hp` file keeps its own clock, which is not elapsed time. When an
  eventlog of the same run is loaded, its censuses are paired with the
  eventlog's by their totals and put on program time; otherwise the heap
  chart says its times are approximate.

## Inspector

The **Inspector** pane shows everything Stronghead knows about the focus:
the long form of a peek. Its name and reference, where it is in the source,
and then a section for each panel that knows it: how that panel matched it,
its facts, what goes with it (as links) and the panel's actions (as
buttons, disabled with the reason when they can't run). **Pin**, **Go to**
and **Copy reference** sit under the name.

It follows the focus as it changes. Its **←** and **→** step through what it
has shown, apart from the window's history, and focus the entity again; with
**Follow** off it holds on to what it shows. It sits beside the heap's
analysis and under Core & STG's bindings by default, and can be added to any
panel.

## Pinboard

**Pin** is on every entity's right-click menu and in the inspector. The
**Pinboard** pane lists the pins by name, each with a note; the arrows
reorder them, **×** unpins, and a click on a pin's name goes to it.

**Copy report** puts the board on the clipboard as Markdown, and
**Export…** saves it: a heading per pin, its reference as code (paste it
into Go to… or `--goto` to open it again), every panel's facts about it as
text, with how each matched, and the note as a quote. Pins are saved with
the session.

## Sessions

A session is where an investigation stands: the eventlog (and whether it
is followed), the heap and cost-centre profiles and what the heap panel
charts, the ghc-debug socket, the dump folder and module or the files open
in Core & STG, the agent's address and whether it is connected, the GHCi
command, directory and file and whether a session runs, the pins and their
notes, the shared cursor and range, the focus, every panel's arrangement of
panes, the presets, the panes popped out, and the panel in front.

**Save session…** and **Open session…** are in the header's **Workspace**
popover and in Go to… (Ctrl+K). A `.stronghead` file also opens from the
command line or by dropping it on the window. Stronghead saves the session
on quit to `last.stronghead` beside `settings.conf`, and reopens it on
start when Settings says to (unless the command line opens something).

The file is `key = value` lines like `settings.conf`, written whole or not
at all. Lists are numbered (`pin.1`, `pin.1.note`); arrangements are trees
of pane names (`layout.heap = (h 0.5 heap.chart inspector)`, where `v` puts
two panes side by side and `h` stacks them). Only line breaks, spaces at
either end and a `%` that would read as an escape are escaped, so paths
and references stay readable and the file can be written by hand.
Unknown keys are skipped, and an arrangement that doesn't read is left as
it was, with a message.

## Workspaces

Every pane of every panel is registered by kind, and any panel can hold any
of them: the Timeline's capabilities, threads, overview and markers; the
heap chart, bands, analysis, live graph and cost centres; Core & STG's
bindings, notes, source, Core and STG; the Debugger's source, locals,
console and breakpoints; Live's charts and threads; and the inspector and
the pinboard.

- **Workspace** in the header adds a pane to the panel in front, beside the
  others. Drag a pane by its header to place it: the middle of another pane
  swaps them, an edge splits it. Double-click a header to maximise.
- Each pane's **⋯** menu moves it to another panel, pops it out into a
  floating window (closing the window puts it back), or closes it. A pane
  is drawn once: a panel that still holds a popped-out pane says where it
  went.
- A pane draws from its panel's loaded data whichever panel it is in, so a
  heap chart under the Timeline, or Core's source pane beside the leak
  suspects, is live.
- Time panes placed with the Timeline's capabilities or threads take the
  Timeline's window: the heap chart (as **Lock to Timeline** does) and
  Live's charts, when they are on the program clock.
- **Arrangements** in the popover put back any panel's own panes, or a
  preset across panels: **Leak hunt** (heap chart, bands, live graph,
  source and inspector) and **Latency** (capabilities with their code
  tracks, threads and inspector). **Save as a preset** names the current
  arrangement; presets are saved with the session.

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
  ticks on the ruler. The **Markers** pane lists them, with a filter, as
  links that move the cursor.
- Ticks on the heap strip mark the heap censuses. A collection's inspector
  links to the heap after it, and a census offers the collection it
  followed.
- Click any slice for the inspector: thread, label, duration, why it stopped,
  the process allocation rate during the slice, the thread's whole history
  (time running, time off-CPU by reason, capabilities). A blocked span says
  **who woke it**: the thread running on the capability that posted the
  wakeup. With nothing selected the inspector lists the longest GC pauses
  and the most-blocked threads, the cursor and the selected range.
- **Code on the timeline.** A profiled build (`-prof -fprof-auto`) run with
  `+RTS -p -l` posts a cost-centre sample for every capability at each
  profiler tick. Each capability gets a **code track** of the sampled top
  frames, merged into runs and shaded ink per cost centre (collections in
  the GC colour, idle time left empty); hover names the frame, its source
  and its callers, and a click focuses the cost centre everywhere. A sample
  belongs to the thread running on its capability, so the slice inspector
  answers **What ran during this slice?**, a thread's inspector shows its
  **CPU profile**, and **Analyse range** lists the top cost centres. Every
  list says plainly when there are too few samples to trust: on Windows the
  profiler's timer ticks about every 15 ms, not the 1 ms it asks for.
- **Ticky.** A ticky build (`-ticky`) run with `+RTS -lT` writes each
  closure's entries and allocation. The overview lists the counters,
  **Analyse range** the top allocators in the range, and Core & STG puts
  them on bindings and `let`s.

Wheel zooms around the pointer, drag pans, double-click fits. A click also
moves the shared cursor; drag the ruler to scrub it, and Shift+drag to select
a range. Each track keeps prefix sums of time per span kind, so a zoomed-out
pixel column is two binary searches however many spans it covers.

### Heap & leaks

Heap profiles from either the eventlog's heap samples (`+RTS -l -hT`, `-hi`,
...) or a `.hp` file, plus `.prof` cost-centre profiles.

- Stacked residency bands over time, with a legend to hide or solo bands and a
  census cursor, which is the shared cursor snapped to a census. Drag the
  chart to move it, wheel to zoom, drag the time axis below the chart to pan
  (double-click it to fit), and Shift+drag to select a range. **Lock to
  Timeline** shows the Timeline's window and pans and zooms it. While an
  eventlog is followed, the cursor stays on the newest census until you move
  it. A `.hp` file is read again as it grows.
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
- Bands from an info-table profile (`-hi`) are named by their provenance
  ("thunk in main (Main.hs:82)") rather than an address. A `-hc` band's menu
  opens its stack in the `.prof` tree; any band's **Find in live graph**
  lists the snapshot's closures with its closure type, name or source, and
  an `-hi` band's closures exactly, by info-table address, when the
  snapshot is of the same run.
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
  CAF counter", and what it points to. Thunks are amber. A thread (its TSO)
  shows its **stack at the snapshot**, frame by frame, each frame a link to
  the source of the code it returns to (with `-finfo-table-map`).
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
`dist-newstyle/`) and it pairs each module's source, Core and STG. Dumps
from a threaded or profiled build (`Main.thr.dump-stg-final`) are paired too.
Sources are found beside the dumps, in the project the dump folder is in (up
to its `.cabal` file), under the **project root** setting and the
debugger's directory; any source file found this way opens here, with or
without dumps, when another panel links to it.

The source pane shows what the runtime saw of each line, in lanes of ink
bars between the line numbers and the code:

| Lane | From | Bar |
| --- | --- | --- |
| time, alloc | a `.prof` (`-prof -fprof-auto`, `+RTS -p`), by each cost centre's span | share of time and allocation |
| heap | an info-table profile (`-finfo-table-map`, `+RTS -hi`), by each band's span | bytes live at the heap cursor; thunks in orange |
| live | a ghc-debug snapshot, by each closure's source | bytes in the snapshot; thunks orange, survivors red |

A line takes the costs of the smallest spans covering it, so a nested cost
centre's lines show its own. Muted notes after the code give the numbers and
a binding's badges, and the gutter shows breakpoints and the debugger's
stop. Every bar and note is a link: a click goes to the cost centre, band or
closure, a right-click lists what every panel can do with it. The line in
focus keeps its notes in view, as does the line under the pointer.

- Bindings are listed by name with what happened to them: worker/wrapper
  (`W/W`), `INLINE`/`NOINLINE`, boxing sites, STG `let` allocations, and
  `gone` when a source binding no longer exists in Core (inlined away).
  With runtime data loaded the list gains **time**, **alloc** and **heap**
  columns (cost centres and bands whose spans start in the binding's source,
  or that name it), sorted by clicking a header; **Hot and boxed only**
  keeps the bindings that cost at least 1% and box or allocate.
- With a ticky eventlog loaded (`-ticky`, `+RTS -lT`) the list also gains
  **entries** and **ticky** (bytes allocated) columns. Each counter is named
  after its closure's STG binder (`main2_sat_s7qL{v} (Main) (fun) in s7qM`),
  so it joins exactly to its `let` in the STG dump and through it to the
  binding; without the dump it joins by the binding's name.
- Selecting a binding lines up all three panes and explains it: arity,
  strictness signature, inline pragma, size, "Re-boxes values ...",
  "N let-bound closures in STG ...", with buttons that jump to each boxing
  and allocation site.
- Hovering any name highlights every occurrence in all three panes (uniques,
  module qualifiers and `$w` prefixes are normalised away, so `go` in the
  source lights up `Main.$wgo` and `go_s1x2`); clicking a name jumps to its
  binding. **Hide uniques** strips `_a1Bc`-style suffixes from the display.
- The STG pane marks each `let` and boxing site with the bytes its closures
  hold at the heap cursor. An info table's name is its STG binder plus
  `_info` (`main2_sat_s7qA_info`), and with `-fdistinct-constructor-tables`
  a constructor table (`I#_Main_3_con_info`) names its numbered site
  (`3 GHC.Internal.Types.I#! [x]`); both are exact joins within one build.
  With ticky, each `let` also shows its closure's entries and allocation.
  Revealing a source line scrolls the STG pane to the code compiled from it.
- Right-click any line, name or note: **Open in editor** runs the editor
  command from Settings at that line, and **Break here** sets a breakpoint.

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
- **Local bindings** at each stop, each `:print`ed and shown as an
  expandable tree, styled like the closure graph: constructors, records,
  lists and tuples open level by level, and unevaluated parts are amber
  `_t` thunks with their types. **Evaluate** takes a thunk to weak head
  normal form (`seq`) and **Force** all the way (`:force`), in place; the
  values are printed again and GHCi stays stopped where it was.
- A console showing every command and its output, plus a prompt for raw GHCi
  commands. `:history` steps and stops are links to their source.
- **Breakpoints** lists every breakpoint, in any file, as links.
- The stop names its binding, a link to it in Core & STG.
- **Break here from anywhere**: right-click a line in Core & STG, a cost
  centre, a band, a closure or a surviving thunk. With a session it breaks
  on the line (`:break Module line`), loading its module first if needed;
  without one it starts the GHCi command from Settings on the file the
  source index finds (a `cabal` or `stack` command in its project, plain
  GHCi in its directory). Bindings break by name (`:break Module.name`).
- The source pane carries the same lanes and notes as Core & STG's.

`--debug path/to/Main.hs` starts a session on that module straight away.

### Live

A "top" for a running Haskell service, and the way to attach to one. Link
the target with the `stronghead:agent` library (it depends only on `base`,
`bytestring` and `network`):

```haskell
import Stronghead.Agent

main :: IO ()
main = withAgent defaultAgentConfig $ do
  ...
```

Build it `-threaded` and run it with `+RTS -T`. The agent listens on
`127.0.0.1:4777` and streams RTS statistics and the live thread list (labels,
blocking status and capability) four times a second. The panel charts
allocation rate, live bytes, GC pauses and thread count on program time, and
each thread row has a strip of its status over the last half minute in the
Timeline's colours.

**One Connect wires the program.** The agent says where the program runs and
how it was started, and Connect follows from there; the strip under the
toolbar says what it found, and why it skipped the rest:

- the eventlog (`+RTS -l`, at `-ol<file>` or `<program>.eventlog` in its
  working directory), followed live in the Timeline; the agent flushes it
  every 250 ms;
- the heap profile (`-hT`, `-hi` ...), read as it grows, beside the
  eventlog's own heap samples;
- the ghc-debug socket of the same pid, for snapshots (with `-f ghc-debug`,
  on a Unix target);
- the working directory, searched for sources and for Core and STG dumps.

So `stronghead-sample 60 +RTS -l -hT -i0.1 -T -RTS` and a Connect give the
Timeline, the heap chart and Live all on one clock.

**Acting on the program.** The **Program** toolbar, the palette (Ctrl+K) and
the status bar's menu send the agent:

- **Mark…** (**M** from any panel): an eventlog marker with your text;
- **Major GC**, **Census now**, and **Start/Stop heap profiling**;

and, through ghc-debug, **Snapshot**, **Census + snapshot**, **Snapshot
twice, 2 s apart** and **Pause** / **Resume**. Each lands on every time axis
of the run at once, as a mark with a square cap, dashed until the agent's
reply stamps it in program time, and then drawn by the eventlog's own marker
once it arrives. Snapshots are stamped the same way (through the agent, or
from the eventlog's wall-clock anchor), in ink. Hovering a mark or a
snapshot on a chart or the Timeline's ruler links to that instant. A
stretch with no frames, while a snapshot or a pause holds the program, is
shaded and labelled on Live's charts.

Commands perturb the program but can't run code. The agent takes them only
when it listens on a loopback address, unless `agentAcceptCommands` says
otherwise. An agent built before protocol v2 still connects and streams;
the panel says to rebuild against the current `stronghead:agent` to attach
and act.

The wire format (protocol v2, compatible with v1 both ways) is in
`Stronghead.Live`'s header.

## Data sources and connections

**Sources** in the header lists each run Stronghead has something from (an
eventlog, a heap or cost-centre profile, the agent, a ghc-debug target), by
program and pid, with what each adds, what is missing, and the build and run
flags that add it, ready to copy. Runs that should match but don't (a
renamed binary, say) can be joined by hand there, and unlinked again.

The **status bar** is the connection centre: the followed eventlog, the
agent, the ghc-debug target and the GHCi session each open their panel on a
click and have a menu on a right-click (stop following, disconnect,
reconnect, take a snapshot, stop or restart the session). It also shows the
shared cursor's time, which links to that instant.

## Try it

`stronghead-sample` misbehaves on purpose: six workers contend on one MVar, an
`IORef` is updated lazily (a classic thunk leak), a cache grows without bound,
and the threads are labelled.

```sh
cabal run stronghead-sample -- 10 +RTS -l -hT -i0.1 -RTS
cabal run stronghead -- stronghead-sample.eventlog
```

While it runs, `cabal run stronghead -- --connect 127.0.0.1:4777` (or
Connect in Live) attaches: the Timeline follows its eventlog, the heap panel
its heap profile, and **M** marks all of them. Run it with `-T` for the
charts: `+RTS -l -hT -i0.1 -T -RTS`.

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
| `src/Stronghead/Source.hs` | Source spans, the source index, the editor command |
| `src/Stronghead/Join.hs` | Joins between artifacts: spans, cost centres, info tables, ticky counters and STG binders, `-hc` stacks and `.prof` paths, bands and closures |
| `src/Stronghead/Live.hs` | Client for the agent's protocol (v2: info line, capabilities, commands) |
| `src/Stronghead/Attach.hs` | Where a connected program's eventlog, heap profile, socket and dumps are |
| `src/Stronghead/Debug.hs` | ghc-debug snapshots (the backend behind the `ghc-debug` flag) |
| `src/Stronghead/Debug/Graph.hs` | Closure graph model: retainer paths, held bytes, thunk survival across snapshots, thread stacks |
| `src/Stronghead/Debug/Client.hs` | ghc-debug-client traversal (built only with the flag) |
| `src/Stronghead/Session.hs` | Pins, the session file, the pinboard's Markdown report |
| `agent/Stronghead/Agent.hs` | The drop-in agent |
| `app/UI/*.hs` | The nano-ui panels; `UI.Workspace` is the pane registry, `UI.Inspector` and `UI.Pinboard` the shell's own panes |
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
- The eventlog carries no stack traces. A profiled build's cost-centre
  samples say what each capability ran, coarsely (one sample per tick per
  capability), and a ghc-debug snapshot gives each thread's stack; otherwise
  the thread inspector shows labels, timing and blocking reasons.
- ghc-events' thread-stop table predates GHC's renumbering of blocking
  reasons, so Stronghead reinterprets codes 16 and up (C calls, `throwTo`).
  GHC's raw code 21 (WinIO completion) still makes ghc-events fail to decode.
