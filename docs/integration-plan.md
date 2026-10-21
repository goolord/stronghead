# Integration plan: one instrument

Draft of 2026-10-20, written against `f76f071`.

**Status.** All eight phases, P0 to P7, are implemented: every item of the
[catalogue](#feature-catalogue) has shipped in some form, and the spikes are
answered (V6 aside, which needs Linux or WSL). What each phase left out, and
why, is in the "Deferred from P*n*" entries under [Progress](#progress); the
largest gaps are the ghc-debug features never run against a real target on
this Windows machine, peeks on canvases, and scrolling revealed list rows
into view.

Stronghead today is five good viewers in one window. This plan makes them one
instrument aimed at one program. Every noun on screen is the same object in
every panel: a thread, a collection, a census, a heap band, a closure, a
binding, a cost centre, a line of source. There is one clock, one selection
and one history. Every number links to the code that produced it, and every
line of code shows what the runtime recorded about it.

## Contents

1. [What is connected today](#what-is-connected-today)
2. [Principles](#principles)
3. [What it feels like](#what-it-feels-like)
4. [The join map](#the-join-map)
5. [Architecture](#architecture)
6. [Interaction and visual rules](#interaction-and-visual-rules)
7. [Feature catalogue](#feature-catalogue)
8. [Roadmap](#roadmap)
9. [Verification spikes](#verification-spikes)
10. [Testing](#testing)
11. [Non-goals](#non-goals)
12. [Decisions for you](#decisions-for-you)

## What is connected today

- **Live → Timeline.** *Follow in Timeline* hands the eventlog path over and
  switches tab (`app/UI/Live.hs:131`, `app/UI/Shell.hs:284`).
- **Timeline → Heap.** The eventlog's heap samples feed the heap panel
  (`app/UI/Shell.hs:256`), but the census cursor and the timeline share no
  notion of time.
- **Inside one panel.** Core's cross-pane name highlighting
  (`app/UI/Core.hs:276`), the black-hole owner button
  (`app/UI/Timeline.hs:776`), Heap's `reveal` along a retainer path
  (`app/UI/Heap.hs:754`).
- **Everything else is prose.** "see the Heap tab" (`app/UI/Timeline.hs:821`),
  "step it in the Debugger" (`app/UI/Timeline.hs:786`), "snapshot the running
  program with Source: Live graph" (`app/UI/Heap.hs:566`).

What stands in the way:

- **Each panel owns its data and its selection** (`tsSel`, `tsFocusThread`,
  `hsSolo`, `hsDbgSel`, `csSelected`, ...). Nothing names an entity in a form
  another panel understands.
- **Four clocks.** The timeline uses nanoseconds. The heap profile uses
  seconds since the *first event* (`src/Stronghead/Eventlog.hs:312`). Live
  stamps frames with Stronghead's own clock on arrival
  (`src/Stronghead/Live.hs:105`). ghc-debug stamps snapshots with wall-clock
  `HH:MM:SS` (`src/Stronghead/Debug.hs:43`).
- **Provenance is thrown away or flattened.** IPE records become one display
  string (`src/Stronghead/Eventlog.hs:183`). The eventlog's cost-centre source
  locations are dropped (`src/Stronghead/Eventlog.hs:181`). `.prof`'s `SRC`
  column is parsed and never shown (`src/Stronghead/Prof.hs:22`). `SrcSpan`
  lives in the GHCi driver (`src/Stronghead/Ghci.hs:141`).

## Principles

1. **One program, one clock, one selection.** Artifacts from the same run
   share a timeline. Selecting something anywhere shows it everywhere it
   appears.
2. **Every noun is a link** with the same gestures in every panel: hover to
   peek, right-click for actions, Ctrl+click to go to it, Alt+← to come back.
3. **Every number has a source; every source line has numbers.** Source code
   is the hub the other panels lead to and annotate.
4. **Say how a link was made.** A link is exact (same info table, same thread
   id in the same run), by name, or approximate (a `.hp` clock). Peeks say
   which, in words.
5. **Missing data is a next step.** An action that needs data that isn't
   loaded stays visible. It is disabled with the reason and the flag that adds
   the data, in the style of today's empty states.
6. **Colour stays data.** Links look like text. The accent is still only for
   selection and focus, and the chrome stays neutral grey.
7. **New gestures are additive.** Today's clicks keep doing what they do.

## What it feels like

Four walkthroughs on `stronghead-sample`. Bracketed IDs refer to the
[catalogue](#feature-catalogue).

**The leak.** Run an investigation build of the sample (dumps,
`-finfo-table-map`, `+RTS -l -hi -i0.1 -T`) and press **Connect** in Live.
The agent's hello names the program's directory and pid. Stronghead starts
following `stronghead-sample.eventlog`, points the source index at the project
and finds the dumps; on Linux or WSL it also finds the ghc-debug socket
`<pid>-stronghead-sample` [A2]. Live's live-bytes chart climbs, and the
`second 3` marker crosses it, the timeline and the heap chart at the same
moment [C1, C3]. The top leak suspect is a thunk band. Its peek shows the
curve, `sample/Main.hs:82` and the STG `let` that allocates it [S4].
Right-click → **Snapshot twice, two seconds apart** [A4]. Surviving thunks
lists the same group, growing, held through an `IORef` from the stack of
thread `leaky-accumulator`. The thread name is a link: Ctrl+click it and the
Timeline highlights its row, mostly blocked on delay, with both snapshot
instants marked [L3, C3]. Right-click line 82, whose gutter shows the bytes
it holds, → **Break here** [S5]. The Debugger starts `cabal repl`, stops on
the line and shows `leak` as a chain of `_t` thunks [D5]. Swap in
`modifyIORef'`, rerun, and compare with the baseline: the band is gone [G3].

**The stall.** `worker-3` has a long red span on the Timeline, blocked on an
MVar. Its peek says who woke it, `worker-1`: the thread running on the
capability that posted the wakeup [C8]. Ctrl+click `worker-1`; it ran for the
whole span. In a profiled run the slice inspector answers *doing what*, from
cost-centre samples taken on that capability during the slice [D1]: the
`show` and `length` work inside `modifyMVar_` (`sample/Main.hs:76`).
Ctrl+click opens the enclosing binding in Core & STG, with the line's share of
time in the gutter [S2]. The fix, doing the work before taking the lock, is
one screen away.

**GC pressure.** Live shows GC taking a large share of CPU. Shift-drag the
last ten seconds of its allocation chart, and the Timeline, following the
same run, selects the same range [C2, C6]. **Analyse range** lists the
collections, pauses and allocation rate inside it. With cost-centre samples it
also lists the top allocators, led by `map-builder`'s `M.fromList`
(`sample/Main.hs:94`). The census at the end of the range shows that little
of it survives, which points at nursery size (`-A`) rather than a leak [G1].

**The optimiser.** Open a dump folder and a `.prof` from the same build. The
bindings list gains time and allocation columns [S3]; sort by allocation. A
hot binding with `box` and `let` badges is the target. Its STG pane marks each
`let` with the heap its closures hold at the cursor census [S4], and the
source pane shows the lines they came from [S2]. Right-click → **Break on
entry** sets a GHCi `:break` by name [S5].

## The join map

The entities that cross panels, where each one already appears, and how
they join. Time-keyed joins only hold within one run. Code-keyed joins hold
across runs of the same build.

| Entity | Timeline | Heap profile | Live graph (ghc-debug) | Core & STG | Debugger | Live | Joined by |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Thread | rows, spans, black-hole owner | — | TSO roots, `thread N` | — | — | thread list | thread id + run: **exact** |
| Instant, range | ruler, spans | census times | snapshot instant | — | — | frames | program clock: **exact** from the eventlog and `elapsed_ns` (V2); `.hp` only by pairing censuses with an eventlog (V3) |
| Collection | GC strip | census after each major GC | — | — | — | pause chart | time: **exact** |
| Marker | ruler, tracks | *new* | *new* | — | — | *new* | time: **exact** |
| Source span | *D1 samples* | `-hi` IPE span, `-hc` cost-centre span | closure info position | source binding ranges | stop span, breakpoints | — | normalised file and lines: **exact** once the path resolves |
| Binding | — | `-hd` names, IPE label | info label | bindings (`nameKey`) | stop name, `:break name` | — | module + name key: **by name** |
| Cost centre | *D1 samples* | `-hc` stack frames | — | via span or name | — | — | module + label, or `.prof` path: **exact** within a build |
| Info table | — | `-hi` band address | *D4* closure info pointer | STG binder = table name (V1) | — | — | address within a run; table name within a build: **exact** |
| Closure type, constructor | — | `-hT`, `-hd` bands | `cnType`, `cnName` | `I#`, `D#` sites | `:print` values | — | name: **exact** for closure types |
| Thunk group | — | thunk bands | surviving groups | STG `let` sites | `_t` bindings | — | code + retainer shape (`ThunkKey`) |

What each kind of data adds:

| Data | Build with | Run with | Adds |
| --- | --- | --- | --- |
| Timeline | `-threaded` | `+RTS -l` | thread, GC and marker links; the clock |
| Heap census on the clock | — | `+RTS -l -hT -i0.1` | census ↔ timeline |
| Heap by source location | `-finfo-table-map -fdistinct-constructor-tables` | `+RTS -l -hi -i0.1` | band → source, band → STG `let`, heap gutter |
| Cost centres | `-prof -fprof-auto` | `+RTS -p` (with `-l` for D1, V4) | time and allocation per line and binding |
| Core & STG | `-ddump-simpl -ddump-stg-final -ddump-to-file`, same build | — | binding notes, exact table-name joins |
| Ticky | `-ticky` | `+RTS -lT` (V5) | entries and allocation per binding |
| Live | link `stronghead:agent` | `+RTS -T` | attach, remote actions, `elapsed_ns` clock |
| Live graph | link `ghc-debug-stub` (Unix) | — | retainers, survivors, thread stacks (D3) |
| Debugger | — | `ghci` / `cabal repl` | break here, values |

## Architecture

```
                 loaders, every frame               lenses, rebuilt every frame
 eventlog  ──┐                                      ┌─ Timeline ─┐
 .hp, .prof ─┤                                      ├─ Heap      │
 dumps     ──┼─► artifacts ─► indices (useTask) ────┼─ Code      ├─► links · peeks · menus · palette
 GHCi      ──┤                                      ├─ Debugger  │          ▲          │
 agent     ──┘                                      └─ Live ─────┘          │          ▼
                                                                   Bus: focus · hover · cursor · range · history
```

### Refs

`src/Stronghead/Ref.hs` holds the vocabulary every panel speaks. Time-keyed
refs carry their run; code-keyed refs don't.

```haskell
newtype RunKey = RunKey Text          -- "program:pid", from Stronghead.Run

data Ref
  = RThread !RunKey !Word32
  | RInstant !RunKey !Word64          -- program time, ns
  | RRange !RunKey !Word64 !Word64
  | RCollection !RunKey !Int          -- index into that run's tlGCs
  | RCensus !RunKey !Int
  | RMarker !RunKey !Int
  | RSource !SrcSpan                  -- resolved through the source index
  | RBinding !Text !Text              -- module, nameKey
  | RCostCentre !Text !Text           -- module, label
  | RBand !Text
  | RInfoTable !Word64
  | RClosure !Int !ClosureId          -- snapshot number, address
  | RThunkGroup !ThunkKey
  | RBreakpoint !FilePath !Int
  | RModule !Text
  deriving (Eq, Ord, Show)

refTarget :: Ref -> Text              -- "thread/stronghead-sample:4242/12"
parseRefTarget :: Text -> Maybe Ref   -- hyperlink targets, sessions, clipboard
```

The text form does three jobs. It is the target of `richText` hyperlinks
(L4), the reference stored in session and pin files (W2, W3), and what
**Copy reference** puts on the clipboard.

### Runs and the program clock

`src/Stronghead/Run.hs` defines `RunInfo`: program, pid, arguments, RTS flags
and start wall time. Each source contributes what it knows:

- the eventlog: `OsProcessPid`, `ProgramArgs`, `RtsIdentifier`, `WallClockTime`;
- the agent: its hello, plus the v2 info line (A1);
- ghc-debug: the socket name, `<pid>-<prog>`;
- a `.hp` file: its `JOB` and `DATE` lines;
- a `.prof` file: its header.

Matching rules:

- Same pid and program: the same run, and time links are on.
- Same program, no pid on one side: probably the same run. Time links are on,
  marked approximate, and one click undoes the match.
- Anything else: separate runs, joined only by code.

Program time is nanoseconds since RTS start, stored absolute. Displays keep
showing offsets from the run's start, as they do now.

| Source | Program time |
| --- | --- |
| eventlog | event timestamps, as they are |
| eventlog heap samples | the event time; stop subtracting the first event |
| `.hp` | not convertible (V3): its seconds are not elapsed time. Pair each census with the eventlog census of equal total bytes when both come from one run; otherwise approximate |
| Live frames | `elapsed_ns + init_elapsed_ns` (V2), falling back to arrival time aligned at connect |
| ghc-debug snapshots | request time through the eventlog's `WallClockTime`, or the agent (A4) |

### Lenses and the bus

Each panel exports a **lens**. The shell builds the list every frame from
what the loaders returned, so lenses always see current data and never need
invalidating.

```haskell
data Lens = Lens
  { lensPanel :: !Panel
  , lensReveal :: Ref -> NanoUI Bool                 -- select, scroll, zoom to it; False if it can't
  , lensActions :: Ref -> NanoUI [Action]            -- "Show in Timeline", "Break here", ...
  , lensPeek :: Ref -> NanoUI (Maybe (NanoUI ()))    -- this lens's section of a peek
  , lensSearch :: Text -> NanoUI [Hit]               -- for the palette
  }

data Action = Action
  { actLabel :: !Text
  , actNote :: !(Maybe Text)   -- "matched by name", or why it is disabled
  , actEnabled :: !Bool
  , actRun :: NanoUI ()
  }

data Bus = Bus
  { busFocus :: !(Var (Maybe Ref))
  , busHover :: !(Var (Maybe Ref))   -- written this frame, read next, like csHovers today
  , busCursor :: !(Var (Maybe (RunKey, Word64)))
  , busRange :: !(Var (Maybe (RunKey, Word64, Word64)))
  , busBack :: !(Var [Location])
  , busForward :: !(Var [Location])
  }
```

Navigation means: pick the ref's home lens, push the current location, write
the target panel's state through `lensReveal`, and switch `appTab`. The
target applies scrolling and zooming when it next draws, the way
`scrollToLine`, `zoomTo` and Heap's `reveal` already work.

Focus is mirrored, not imposed. Lenses read `busFocus` while drawing to
highlight it (a thread row, a band, a source line) and leave their own
selection alone unless the user navigates.

### Links, peeks and menus

`app/UI/Link.hs` provides the widgets every panel uses:

- `link env ref label` for text and `linkRow` for list rows. Both keep each
  panel's existing click behaviour, add Ctrl+click navigation, open a peek on
  hover (`tooltipWidget`) and open actions on right-click (`contextMenu`).
- Canvas views (timeline tracks, the heap chart, code panes) expose
  `refAt :: V2 -> Maybe Ref`, built from the hit-testing they already do
  (`pickMain`, `hoverInfo`, the code view's token lookup). They are wrapped in
  `contextMenuArea`, whose menu is told where it opened.
- Peeks and actions are computed only for the one ref being hovered or
  right-clicked, never for every link drawn.
- A context menu groups actions by panel. It always offers **Copy
  reference**, **Pin** and **Focus everywhere**.

### The source index and annotated code

`src/Stronghead/Source.hs` takes `SrcSpan` out of the GHCi driver and parses
every form GHC prints: `f:l:c-c`, `f:(l,c)-(l,c)`, `f:l:c`, `<built-in>`,
`<entire-module>` and `<no location info>`. Its `SourceIndex` resolves the
paths that IPE, `.prof`, ghc-debug and GHCi report against a list of roots:
the project root setting, the agent's cwd, sources found next to the dumps,
and the debugger's directory. It tries an absolute path first, then each
root, then the longest matching path suffix, then the module name. A path it
can't resolve keeps a **Locate…** action.

`UI.Code` grows from one gutter mark per line into lanes:

- a heat bar per line;
- gutter marks;
- a muted trailing hint;
- right-click on lines and tokens.

Each lens provides annotations for a file. The same annotated view backs
Core & STG's source pane and the Debugger's.

### Where the code goes

| Module | Holds |
| --- | --- |
| `src/Stronghead/Source.hs` | spans, span parsing, `SourceIndex` |
| `src/Stronghead/Run.hs` | `RunInfo`, run matching, clock conversion |
| `src/Stronghead/Ref.hs` | `Ref` and its text form |
| `src/Stronghead/Join.hs` | pure joins: cost centre ↔ span ↔ binding, info table ↔ STG binder, `-hc` stack ↔ `.prof` path, wakeup attribution, TSO ↔ thread |
| `src/Stronghead/Fuzzy.hs` | the palette's matcher |
| `app/UI/Link.hs` | `Bus`, `Lens`, `Action`, link widgets, peeks, menus, history |
| `app/UI/Palette.hs` | Ctrl+K |
| `app/UI/Sources.hs` | the data sources popover (A6) |
| `app/UI/Inspector.hs` | the entity inspector (W1) |
| `app/UI/Pinboard.hs` | the pinboard and its report (W2) |
| `app/UI/Workspace.hs` | the pane registry, workbench trees and presets (W4) |
| `src/Stronghead/Session.hs` | pins, the session file, the report's Markdown (W2, W3) |
| `agent/Stronghead/Agent.hs` | protocol v2, still on `base`, `bytestring` and `network` only |

The `src/` modules stay free of the GUI and are tested headlessly, as the
cabal file promises for the library.

### Frame-loop rules

- Indices are built in `useTask`, keyed on artifact identity (path and size,
  snapshot stamp, the number of IPE records seen), never per frame. Lookups
  are logarithmic. The timeline's prefix sums set the standard.
- Views start no threads. Background work stays in `useTask` and
  `useStream`, as now.
- Navigation writes the target's state and lets it apply on its next draw.
  No panel draws another panel's view.

## Interaction and visual rules

| Gesture | Effect |
| --- | --- |
| Hover, resting | Peek: what every lens knows about it |
| Click | What it does today in that panel (select, zoom, expand); also sets the global focus |
| Ctrl+click, Ctrl+Enter | Go to its home panel, pushing history |
| Right-click, Shift+F10 | Every lens's actions, grouped, with reasons when disabled |
| Alt+←, Alt+→, mouse back and forward | History (nano-ui has `MouseBack` and `MouseForward`) |
| Ctrl+K | Find anything |
| Ctrl+1 … Ctrl+5 | Switch panel |
| Esc | Clear the focus |
| A link in prose | Navigates on a plain click, like a hyperlink |

Home panels, where Ctrl+click goes:

| Ref | Home panel |
| --- | --- |
| thread | Timeline, or Live when there is no eventlog for that run |
| instant, range, collection, marker | Timeline |
| census, band, info table | Heap |
| closure, thunk group | Heap › Live graph |
| cost centre | Heap › Cost centres |
| source span, binding, module | Core & STG |
| breakpoint | Debugger |

Visual rules (see the colour memory: neutral chrome, colour only for signal):

- **Links look like text:** underlined with a pointer cursor on hover, no
  chips and no new hue. The focused entity gets the selection accent wherever
  it is drawn; nothing else does.
- **Cross-highlighting on hover is neutral,** a faint ink wash.
- **Signal colours stay consistent everywhere.** Running, GC, blocked and
  thunk colours mean the same thing in every panel. A band is the same colour
  in the chart, the legend, a peek and the pinboard. Live's thread statuses
  move from generic tones (`app/UI/Live.hs:227`) onto the timeline's
  `Palette` colours.
- **Source heat lanes are ink bars.** Colour appears only for thunks and
  survivors, so a gutter never turns into a rainbow.
- **Exactness is written in the peek, not coloured.**

## Feature catalogue

IDs are stable so later sessions can say "implement S4". Each item gives its
phase.

### F: Foundation (P0)

- **F1 Source spans and the source index.** `Stronghead.Source` as above;
  GHCi, Prof and Eventlog use it.
- **F2 Keep provenance.**
  - an `InfoProv` record (table name, closure kind, type, label, module,
    span) instead of the detail string;
  - the eventlog's cost-centre table with `heapProfSrcLoc`;
  - `ccSrc` parsed into a span.
- **F3 One program clock.** Absolute times for eventlog heap samples;
  `elapsed_ns + init_elapsed_ns` on Live frames (V2); `WallClockTime`
  captured so wall-clock stamps can be converted.
- **F4 Runs.** `RunInfo` and the matching rules.
- **F5 Refs.** `Ref`, its text form, and a round-trip test.

### L: Links (P1)

- **L1 Bus.** Created in `newApp` and passed to every view.
- **L2 Lenses.** One per panel, rebuilt each frame in `appView` after the
  loaders.
- **L3 Link widgets.** Text, row and canvas variants with the gestures above.
- **L4 Prose links.** Hints and diagnoses (`diagnose`, `describePath`, the
  inspector's hints) rendered with `richText` and `hyperlink`, with targets
  from `refTarget`.
- **L5 History.** Back and forward in the header, a recent-locations menu,
  Alt+arrows and the mouse buttons.
- **L6 Cross-highlight.** The hovered or focused ref highlighted in every
  visible panel that shows it.
- **L7 Shortcuts.** Ctrl+1–5 for panels and Esc to clear the focus. The
  bindings show in tooltips.
- **L8 Command palette.** Ctrl+K searches threads, bindings, cost centres,
  bands, closures, markers, modules, files and commands such as "Take
  snapshot", "Step over" and "Follow eventlog". It uses fuzzy matching and
  groups results by kind. Enter runs the primary action; → opens the action
  list.
- **L9 Wire every panel.** Make every noun in every panel a ref:
  - Timeline: slices, threads, the black-hole owner, GC rows, markers, the
    overview lists;
  - Heap: bands, census rows, suspects, retention nodes, cost centres,
    closures, retainer paths, survivors;
  - Core & STG: bindings and tokens;
  - Debugger: the stop location, breakpoints, history;
  - Live: thread rows.

### C: Clock (P2)

- **C1 Global cursor.** One cursor across the Timeline, the heap chart (which
  snaps to the nearest census) and the Live charts in the same run. `[` and
  `]` step it through censuses or collections.
- **C2 Range brush.** Shift+drag on any time view. **Analyse range** lists
  collections, pauses, allocation, active threads, heap growth and markers,
  all as links.
- **C3 Markers everywhere.** Eventlog markers, agent marks and snapshot
  instants appear on every time axis. A markers pane lists them with a
  filter.
- **C4 Census ↔ collection.** Census ticks on the Timeline's heap strip. The
  GC inspector gains "Heap after this collection", and a census gains "Show
  collection".
- **C5 Heap chart navigation.** The heap chart gets a `TimeView` with wheel
  zoom and drag pan, and a **Lock to Timeline** toggle.
- **C6 Live on program time.** Brushing a Live chart selects that range in
  the followed eventlog.
- **C7 Follow the latest.** The heap cursor tracks the newest census while
  following an eventlog. A `.hp` file is re-read as it grows.
- **C8 Who woke whom.** Record every `WakeupThread`, not only
  cross-capability ones (`src/Stronghead/Eventlog.hs:146`). The waker is the
  thread running on the posting capability at that moment; it shows in the
  blocked span's inspector and peek.

### S: Source hub and code joins (P3)

- **S1 Annotated code view.** Lanes, trailing hints and line and token menus
  in `UI.Code`.
- **S2 Annotation providers.**
  - `.prof`: time, allocation and entries per line, from `ccSrc`;
  - heap: bytes at the cursor census per line, from IPE spans (`-hi`);
  - Live graph: closures and thunks per line from `cnSource`, survivors in
    red;
  - Core: badges on the binding's first line;
  - Debugger: breakpoints, candidates and the stop.
- **S3 Runtime columns on bindings.** Time, allocation, heap and (with D2)
  ticky columns, joined by cost-centre span overlap with the source binding
  and by name. Sortable, with a "hot and boxed" filter.
- **S4 STG `let` ↔ heap.** An IPE table name such as `main2_sat_s7qA_info`
  names the STG binder `main2_sat_s7qA` from the same build (V1). Each `let`
  shows the bytes its closures hold. With `-fdistinct-constructor-tables`,
  each boxing site (`I#_Main_3_con_info`) shows its constructor band, joined
  by label and span since constructor tables have no binder.
- **S5 Break here.** From any span, binding, cost centre, closure source or
  survivor: `:break <module> <line>` or `:break <name>`. If no session is
  running, one starts, with the module resolved through the source index.
- **S6 Debugger ↔ Core.** The stop location opens its binding in Core & STG.
  A breakpoints pane lists breakpoints across files. `:history` entries
  become span links.
- **S7 Open in editor.** A command template in Settings, for example
  `code -g {file}:{line}:{col}`, offered on every span.
- **S8 Source without dumps.** The code panel opens any file the index
  resolves, with its annotations. Consider renaming the tab "Code".
- **S9 Heap ↔ code joins.**
  - A `-hc` band's stack (`leaf/go/MAIN`) opens the matching path in the
    `.prof` tree, an exact match.
  - A band's **Find in live graph** lists closures by closure type, name or
    source, exactly by info table once D4 lands.
  - Survivors get **Break where created**.

### A: Attach and control (P4)

- **A1 Agent protocol v2.** Backward compatible in both directions (below).
- **A2 One-step attach.** Connect also:
  - follows the eventlog (`<cwd>/<prog>.eventlog`, or `-ol` from the args);
  - follows the `.hp`;
  - finds the ghc-debug socket by pid;
  - sets the project root to the program's cwd;
  - searches it for dumps.
- **A3 Remote actions.** **Mark** (M), **Major GC**, **Census now** and
  **Heap profiling on/off** go through the agent. **Snapshot** and
  **Pause/resume** go through ghc-debug. All are in the Live toolbar and the
  palette, and each lands as a marker on every time axis.
- **A4 Snapshot stamping.** Snapshots are stamped in program time. **Census +
  snapshot** takes both at one instant; **Snapshot twice** spaces two by a
  delay. Live labels the gap a pause leaves in its charts.
- **A5 Threads across sources.** A Live thread row, a Timeline thread and a
  ghc-debug TSO are one ref in the same run. Each Live row gets a status
  history sparkline from the frame buffer.
- **A6 Data sources popover.** Lists each run's artifacts and connections,
  what each one adds, and what is missing, with copyable build and run
  recipes. Unmatched runs can be linked by hand here.
- **A7 Status bar as connection centre.** Every live source is clickable and
  has a menu. The bar also shows the cursor time and the focus.

```
agent → client   (v1 lines unchanged; v1 clients ignore the new ones)
  H <program> <pid> <capabilities>
  I version=2 cwd=<%-encoded> exe=<%-encoded> args=<%-encoded> heap-profile=<mode|none> heap-interval-ms=<n> eventlog=<0|1>
  S ...   T <tid> <status> <label...>   E
  C <tid> <capability>            after a thread's T line
  A <id> ok | A <id> error <text>

client → agent   (v1 agents never read the socket)
  <id> mark <text>                traceMarkerIO
  <id> gc                         performMajorGC
  <id> census                     requestHeapCensus
  <id> heap-profiling on|off      startHeapProfTimer / stopHeapProfTimer
  <id> flush                      flushEventLog
```

All of these are in `base` as shipped with GHC 9.14 (checked against
`GHC.Profiling`, `GHC.RTS.Flags`, `System.Mem` and `GHC.Environment`).
Commands perturb the program but can't run code. A new
`agentAcceptCommands` setting defaults to on only when the agent is bound to
loopback.

### W: Workspace (P5)

- **W1 Entity inspector.** A pane showing everything known about the focus,
  one section per lens, each with its exactness note. It is the long form of
  a peek.
- **W2 Pinboard.** Pin refs, add notes, export a Markdown report whose links
  are ref targets.
- **W3 Sessions.** Save and restore artifacts, connections, pins, cursor and
  layout in `key = value` form like `settings.conf`, with optional reopen on
  start.
- **W4 Mixed workspaces.** A pane registry lets any pane live in any
  workbench, using nano-ui's `paneDragHandle` and `commitPaneDrop`.
  - The five panels become presets, next to user presets such as "Leak hunt"
    and "Latency".
  - Time panes stacked in one workbench share an axis.
  - Any pane can pop out into a floating `window`.

### D: Deeper evidence (P6)

- **D1 Code on the timeline.** `ProfSampleCostCentre` events from a profiled
  build give a code track per capability. Samples inside a thread's run spans
  on that capability are that thread's CPU profile, and "What ran during this
  slice?" goes in the span inspector. This answers the README's "no stack
  traces" limit for profiled builds.
- **D2 Ticky.** `TickyCounterDef` and `TickyCounterSample` give entries and
  allocation per binding, joined to Core and STG by exact name.
- **D3 Thread stacks.** Decode ghc-debug STACK frames with each frame's
  source, giving a "Stack at snapshot" for every thread. This answers the
  "no stack traces" limit for ghc-debug targets.
- **D4 Info-table pointers on closures.** Keep the info pointer on
  `ClosureNode` for an exact join from `-hi` bands to closures.
- **D5 Structured GHCi values.** Parse `:print` output into trees with `_t`
  thunks you can force in place, styled like the closure graph.

### G: Investigations (P7)

- **G1 Next steps.** Given the focus and the loaded data, the inspector
  proposes the next link. Leak: provenance, then snapshot, then retainer, then
  break. Contention: owner, then what it ran. Hot path: binding, then STG,
  then allocation. Each flow reads the heuristics it already has, such as the
  leak score and the boxing notes.
- **G2 Watchers.** Rules on live streams, such as "live bytes up across N
  major GCs" or "a thread blocked longer than X", that mark, take a census,
  snapshot or notify.
- **G3 Baselines and diffs.** Mark a run as the baseline, see deltas in
  every readout, and diff heap profiles, `.prof` trees and two dump folders.

### What each panel gains

| Panel | Items |
| --- | --- |
| Timeline | L9, C1–C4, C8, A5, D1, D3 |
| Heap & leaks | L9, C1, C3–C5, C7, S2, S4, S9, A4, D4 |
| Core & STG | L9, S1–S4, S7, S8, D2 |
| Debugger | L9, S1, S5, S6, D5 |
| Live | L9, C1, C3, C6, A1–A5 |
| Shell | L1–L8, A6, A7, W1–W4 |

## Roadmap

Sizes are relative: S is a day or two, M under a week, L more.

| Phase | Goal | Items | Size | Needs | Can run alongside |
| --- | --- | --- | --- | --- | --- |
| **P0** Foundation | Artifacts keep the identities that links need | F1–F5, spikes V1–V3 | M | — | F1–F4 are independent |
| **P1** Links | Right-click anything, peek anything, go anywhere, come back | L1–L9 | L | P0 | — |
| **P2** One clock | Time is shared by every time view | C1–C8 | M | P1 | P3 |
| **P3** Source hub | Every line shows what the runtime saw; break from anywhere | S1–S9 | L | P1, V1 for S4 | P2 |
| **P4** Attach | One Connect wires a running program; Stronghead can act on it | A1–A7 | M | P0; UI after P1, P2 | A1 from P0 onwards |
| **P5** Workspace | Inspect, pin, save, mix panes | W1–W4 | L | P1; W4's shared axes need P2 | — |
| **P6** Deeper evidence | New data that makes links exact | D1–D5 | L | D1 needs P2 and P3; D3 and D4 need Linux or WSL | items are independent |
| **P7** Investigations | Stronghead suggests the next step | G1–G3 | open | P2–P4 | — |

Ship P1 in two halves. **P1a** is the fabric (L1–L3, L5) with the Timeline
and Heap wired. **P1b** wires Core & STG, the Debugger and Live, and adds
prose links, cross-highlighting and the palette (L4, L6–L9).

Done when:

- **P0.** Tests cover every span form, path resolution, provenance records,
  clock fields, run matching and the ref round trip. Spikes V1–V3 are
  answered, and their findings are written into this file.
- **P1.** In all five panels, right-clicking any thread, collection, census,
  band, closure, binding, cost centre or span offers "Show in …" for every
  panel that can show it. Ctrl+click goes there and Alt+← comes back. A peek
  shows at least the home panel's facts. Ctrl+K finds threads, bindings, cost
  centres, bands and markers by name.
- **P2.** On the sample's `-l -hT` eventlog, the heap cursor and the timeline
  cursor move together. Markers show on the Timeline, Heap and Live. A brush
  on one time view selects the range in the others. A collection links to its
  census.
- **P3.** With dumps, a `.prof` and an `-hi` eventlog from one build of the
  sample, the source pane shows time, allocation and heap per line, and line
  82 carries the leak's bytes. STG `let` sites show the heap they hold.
  **Break here** stops GHCi on the line.
- **P4.** One Connect to the running sample wires the eventlog, the project
  root and, on Linux or WSL, ghc-debug. **Mark** shows on every time view
  within a second. An old agent still connects and shows how to upgrade.
- **P5–P7.** Each item ships with a fixture or a walkthrough step that shows
  it working.

Where to start: P0, beginning with spikes V1–V3, because they decide S4 and
how far C1 can trust each clock. Then P1a.

### Progress

- **P0** landed in `8005143`. The spike findings are below.
- **P1** is in place, P1a and P1b together:
  - `UI.Link` holds the bus, lenses, link widgets, peeks, menus and history.
  - `UI.Palette` and `Stronghead.Fuzzy` make up the palette.
  - Each panel exports a lens: `timelineLens`, `heapLens`, `coreLens`,
    `debugLens` and `liveLens`.
  - `stronghead-ui-test` drives clicks, Ctrl+clicks, menus, peeks, history
    and the palette headlessly through nano-ui's harness.
  - Two pieces of later phases arrived early, because the lenses made them
    one line each:
    - **Break here** and **Break on entry** (S5) work for the module GHCi has
      loaded.
    - A census, or an instant on the Timeline's heap strip, moves the heap
      cursor (a first step of C1).
- **Deferred from P1:**
  - **Scrolling revealed list rows into view.** nano-ui's `scrollIntoView`
    takes a `Context`, which the public API doesn't hand out. Revealing
    clears the filter, or sets it to the item (cost centres), and the
    timeline's thread rows scroll exactly. An upstream `scrollIntoViewUi`
    would close the gap.
  - **Pin.** It belongs with the pinboard (W2).
  - **Opening a menu from the keyboard** (Shift+F10). `contextMenu` only
    opens on a right-click.
  - **The recent-locations menu.** Back and forward carry tooltips naming
    where they lead instead.
  - **Peeks on canvases.** Canvases keep their own hover text, and get
    right-click and Ctrl+click.
- **P2** is in place:
  - The bus carries the cursor, the range and the Timeline's visible window
    (`busCursor`, `busRange`, `busWindow`), all in program time and keyed by
    run. `UI.Clock` holds what every time view shares: an `Axis`, drawing
    the cursor, range and markers (`drawMarks`), Shift+drag and scrubbing
    (`timeGestures`), `[` and `]` (`stepKeys`), and the `Tether` that keeps
    a snapping cursor (the heap chart's census) in step with the shared one.
  - `Stronghead.Clock` holds the time helpers: the nearest sample, stepping,
    and `ClockMap`, which puts a `.hp` file on program time by pairing its
    censuses with the eventlog's by total (V3), or says it can't.
  - The model gained every `WakeupThread` with its waker (`tiWakeups`,
    `wokenBy`), the census ↔ collection joins and `rangeStats`.
  - Going to an `RInstant` moves the cursor, and going to an `RRange` selects
    it.
  - The heap chart's gestures: drag moves the cursor (as before), the wheel
    zooms, a drag on the time axis pans and a double-click there fits,
    Shift+drag selects a range. **Lock to Timeline** writes its pans and
    zooms to the Timeline's window, which the Timeline takes up on its next
    frame.
- **Deferred from P2:**
  - **Agent marks and snapshot instants on the time axes** (C3). Only
    eventlog markers are drawn; the other two need A3 and A4.
  - **Stepping on Live's charts.** `[` and `]` step through collections and
    censuses on the Timeline and the heap chart. Live keeps only five
    minutes of frames, and its collections are counts between frames, not
    events.
  - **Locking Live's charts to the Timeline.** They always show their last
    five minutes; the cursor, range and markers already join them to it.
  - **Census refs across artifacts.** An `RCensus` names a census of one
    artifact (an eventlog's or a `.hp` file's), so refs are matched by exact
    run key. A `.hp` file paired with an eventlog moves the shared cursor
    exactly, but its census refs are not translated to the eventlog's, so
    "Show collection" is offered only for the eventlog's censuses.
  - **The cursor in the status bar** belongs with A7.
  - **Top allocators in Analyse range** need cost-centre samples (D1).
- **Seen while wiring P2:**
  - A wakeup posted while its capability runs no thread (during GC, or by
    the RTS itself) reads "woken by the runtime".
  - Peeks on canvases are still deferred (from P1), so a blocked span's
    waker shows in its hover text and the inspector rather than a peek.
- **Seen while wiring, for P3:**
  - `-hi` bands are named by address. Their `InfoProv` label and span would
    read better in the legend and the census table. *(Done in P3.)*
  - A dump folder without sources beside it shows "Source not loaded" until
    the source index (S8) can find the sources elsewhere. *(Done in P3.)*
- **P3** is in place, S1–S9:
  - `Stronghead.Join` holds the pure joins, each tested in `test/Main.hs`:
    span containment and overlap, `.prof` totals per cost centre
    (recursion counted once), per-line attribution (the smallest span
    covering a line owns it), IPE table names to STG binders and to
    numbered constructor sites, `-hc` stacks to `.prof` paths, bands to
    closures, and closures grouped by source.
  - Lenses gained `lensAnnotations` (heat bars, gutter marks and notes, each
    with its entity) and `lensSpan` (where an entity is in the source).
    The Heap lens annotates from the `.prof`, the `-hi` census at the
    cursor and the latest snapshot; Core adds binding badges; the Debugger
    adds breakpoints, candidates and the stop.
  - `UI.Code` draws the lanes, marks and notes, and `annotatedCode` makes
    every bar and note a link. Core & STG's source pane and the Debugger's
    use it; the STG pane carries the heap of each `let` and boxing site.
  - Core & STG builds a `SourceIndex` in a `useTask` from the project root
    setting, the dump folder and its project, and the debugger's
    directory. A module without its source beside the dumps finds it
    through the STG's `src<...>` notes, then by module name.
  - Break here works from any ref a lens gives a span, and starts GHCi on
    the resolved file when no session runs. Checked end to end with `ghci`
    on a standalone file (`:break 4`, then `main` stops in `Main.fib`).
  - Settings gained the project root and the editor command.
  - `--goto REF` opens an entity on start; the screenshot check uses it.
  - Found on the way: `.prof` rows whose `SRC` is `<no location info>`
    shifted every number column. `parseProf` now reads placeholders with
    spaces whole.
  - The STG pane follows a revealed source line through its `src<...>`
    notes, scrolled to the code's indentation.
- **Deferred from P3:**
  - **Renaming the tab "Code".** The panel's key, `core`, is its
    `--tab` name and its settings value, and the title derives the key.
    Renaming means splitting the two; the content is still the Core/STG
    pairing, so the title stays "Core & STG" for now.
  - **Ticky columns on bindings.** They need D2.
  - **Scrolling to the opened `.prof` row.** A `-hc` band opens and selects
    its path, but the tree does not scroll to it (the P1 `scrollIntoView`
    gap).
  - **Exact band ↔ closure joins.** **Find in live graph** matches by
    closure type, constructor name or source span; by info table it needs
    D4.
  - **Peeks on bars and notes.** They have clicks and menus; hover peeks on
    canvases are still deferred from P1.
  - **Locate… for unresolved paths.** Actions that can't find a file say to
    set the project root instead.
  - **Breakpoint toggling in Core & STG's gutter.** Its menu has Break here;
    the gutter click stays the Debugger's.
  - **Runtime lanes in the Core pane.** Core dumps carry no source notes
    without `-g`, so only the source and STG panes are annotated.
  - **Break here on the sample itself.** It imports `stronghead:agent`, so
    plain `ghci` can't load it; set the GHCi command to
    `cabal repl stronghead-sample`. Not checked end to end.
  - **A real `-hc` profile.** V4 found no non-empty `-hc` census on
    Windows, so the `.prof` path join is checked against fixtures only.
- **P6** is in place, D1–D5, with the ghc-debug parts (D3, D4) built and
  tested against the model but not run end to end:
  - **D1.** The eventlog model keeps every `ProfSampleCostCentre`
    (`tlProfSamples`), each with the thread running on its capability at
    the time, so a thread's CPU profile is its samples. The samples merge
    into per-capability runs of one top frame (`tlCodeRuns`), and
    `tlSamplePeriod` is the gap the log shows between one capability's
    samples. `codeProfile`, `sliceProfile` and `threadProfile` count top
    frames, leaving the `IDLE` cost centre out. The Timeline draws a code
    track under each capability (toggle **Code**), the span inspector
    answers **What ran during this slice?**, the thread section shows its
    CPU profile, and **Analyse range** and the overview list the top cost
    centres, all as `RCostCentre` links with their spans. Each list says
    when there are fewer than 30 samples, and names the timer's real
    period when it is coarser than 5 ms. The Timeline lens shows, peeks,
    finds and puts in the source any sampled cost centre, so Ctrl+click
    reaches it without a `.prof` (its home order is now Heap, Core,
    Timeline).
  - On the sample's profiled run (`+RTS -p -l`, 4 s, `-N4`): 1,084
    samples, one every 15.5 ms per capability (the 1 ms tick asked for runs
    at the Windows timer's 1/64 s), 58% of them in `GC`, 12% idle.
  - **D2.** Ticky counters are summed per counter, keeping each sample
    (`tcSeries`): the RTS resets the counters after each sample, so the
    samples are deltas. `Stronghead.Join.parseTickyName` reads the counter
    names: `tickyBinder` puts the unique back (`go1{v s7vg}` is
    `go1_s7vg`, `Main.main2{v r7mA}` stays qualified), and `tickyTopLevel`
    follows the `in s7qM` parents to the top-level binding. A counter
    joins its STG `let` through `siteOfTable`, as S4's IPE tables do. In
    the sample's ticky build, 35 of its 36 counters name a binder in their
    module's STG dump; the one that doesn't is GHC's `:Main.main`. Core &
    STG's bindings list gains **entries** and **ticky** columns (sortable,
    counted in "hot"), each `let` in the STG pane shows its counter, the
    notes bar its totals, the Timeline's overview the counters, **Analyse
    range** the top allocators from the samples in the range (`tickyBetween`),
    and the palette every counter.
  - **D3.** The client decodes each STACK's frames (`dereferenceStack`),
    with each frame's info-table source (`getSourceInfo`), into
    `cnFrames`. `threadStack` reads a TSO's stack top first, following
    `UNDERFLOW_FRAME`s into older chunks and stopping on a loop. A TSO's
    detail pane in the live graph shows **Stack at snapshot**, frames as
    source links, and its peek (so a thread's peek in every panel) its top
    frames.
  - **D4.** `ClosureNode` keeps its info pointer (`cnInfo`). `matchBand`
    matches an `-hi` band to closures by address when any closure carries
    it, and falls back to source, type or name otherwise; **Find in live
    graph** says which it used.
  - **D5.** `Stronghead.Ghci` parses values (`parseValue`, `parsePrint`)
    into trees: constructors (infix ones too), records, lists with an
    unevaluated tail, tuples, literals, and `_t` thunks with their types.
    At each stop the Debugger `:print`s every local binding and shows the
    value as an expandable tree, thunks amber; **Evaluate** (`seq`) and
    **Force** (`:force`) work on any named thunk, then reprint. Forcing no
    longer runs through the resume path, so GHCi's stop is kept (the old
    Force WHNF cleared it). Checked against `ghci` 9.14 on a small module:
    `p = (_t1::P)` becomes `P (_t2::Int) (_t3::[Int])` after `_t1 \`seq\`
    ()`, and `P 5 [2,4,6,8,10]` after `:force p`.
  - `stronghead-ui-test` drives the value tree: its thunks show named and
    typed, Evaluate and Force reach the right thunk, and a node closes to
    the whole value.
- **Deferred from P6:**
  - **D3 and D4 end to end.** No ghc-debug target runs on this Windows
    machine (the stub needs Unix sockets, and there is no WSL distro), so
    the frame decoding and the info pointers are compiled
    (`cabal build -f ghc-debug`) but never exercised against a real
    debuggee (V6 is still open). Frame titles, underflow chains and the
    exact band match are tested on hand-built snapshots only.
  - **Stacks in the Timeline's thread inspector.** A thread's stack shows
    in the TSO's detail pane and in its peek, which the Timeline's thread
    rows already open; the inspector itself doesn't read the Heap's
    snapshots.
  - **Top allocators from cost-centre samples** (the P2 deferral).
    Samples measure where the time went, not what was allocated; the
    eventlog has no per-cost-centre allocation. Analyse range lists the
    top cost centres by samples, and the top allocators from ticky when a
    ticky log is loaded.
  - **Ticky counters as their own refs.** Counters link to the binding
    they are nested in (`RBinding`); there is no ref for an STG `let`, so
    Ctrl+click on a counter opens the binding, not the `let`.
  - **Ticky ↔ IPE by info table.** Counters carry their info table's
    address, which joins `-hi` bands of the same run; a ticky build with
    `-finfo-table-map` wasn't tried.
  - **Debugger screenshots.** The value tree is checked headlessly
    (`stronghead-ui-test`) and the parser against real `ghci` output; a
    stopped session can't be started from the command line, so there is
    no capture of it.
  - **Code-track peeks.** As with every canvas (P1), the code track has
    hover text, right-click and Ctrl+click, not a peek.
- **P4** is in place, A1–A7:
  - **Protocol v2** (A1). After its hello the agent sends an `I` line:
    version, working directory, executable, the full command line
    (`getFullArgs`, so `+RTS` options included), the heap profile's
    breakdown and interval (`getProfFlags`), whether an eventlog is being
    written (`getTraceFlags`) and whether it takes commands. Fields are
    percent-encoded; the arguments are encoded one by one and joined by
    commas. Each `T` line is followed by `C <tid> <capability>`.
  - Commands carry an id and are answered by `A <id> ok [t=<ns>]` or `A <id>
    error <text>`. `t` is `elapsed_ns + init_elapsed_ns` just after the
    command ran, under `+RTS -T`. Every command but `flush` also posts an
    eventlog marker (the mark's text, or `stronghead: major GC` and so on),
    so it reaches a followed Timeline by itself. `agentAcceptCommands ::
    Maybe Bool` defaults to on only for a loopback address. The agent reads
    its working directory through `getcwd` / `_wgetcwd`, to stay on `base`,
    `bytestring` and `network`.
  - Tests cover the v2 lines and v1 lines still parsing, and run an agent in
    the test process: its info line, capabilities, commands and their
    replies, commands turned off, what a v1 client reads from it (only lines
    it knows or skips), and a v2 client against a fake v1 agent that never
    reads.
  - **Attach** (A2). `Stronghead.Attach` works out the eventlog (`-ol`,
    else `<stem>.eventlog` for `-po`, else `<program>.eventlog`, in the
    working directory), the `.hp`, the ghc-debug socket by pid and the dumps
    under the working directory, and checks each. Live shows what Connect
    found in one line, with the reason for each skip in its tooltip; the
    shell opens the rest (`applyAttach`). A v1 agent gets a callout saying
    to rebuild.
  - **Actions** (A3). `liveRemote` gives one list, each action with why it
    can't run, for the Live toolbar, the palette (the Live lens's hits) and
    the status bar's menu: **Mark…** (M from any panel, a prompt with a
    default text), **Major GC**, **Census now**, **Start/Stop heap
    profiling**; and through the heap panel's ghc-debug target **Snapshot**,
    **Census + snapshot**, **Snapshot twice, 2 s apart** and **Pause** /
    **Resume** (`Stronghead.Debug.pauseTarget` holds a connection paused).
  - Each action becomes a `Moment` on the bus (`busMoments`) as it is sent,
    at the program time the latest frame suggests. `drawMarks` draws moments
    on every time axis with a square cap: dashed while estimated, solid once
    the reply stamps it, and only the cap once the followed eventlog's own
    marker has arrived (`confirmMoments` takes up that marker's time). On a
    Live chart or the Timeline's ruler a moment is an `RInstant` link.
  - **Snapshot stamping** (A4). Before a snapshot is requested, the agent
    posts `stronghead: snapshot N` and its reply gives the program time;
    without commands the followed eventlog's `WallClockTime` anchor converts
    the wall clock, and failing that the latest frame does. Live shades and
    labels the stretches with no frames ("paused 3.2 s" when Stronghead
    paused or snapshotted the program then, "no samples" otherwise).
  - **Threads** (A5). A Live row, a Timeline thread and a TSO were one ref
    already; the peek of a thread now shows the Timeline's and Live's facts
    together. Live rows gained the capability from `C` lines and a strip of
    the thread's status in each of the last 120 frames, in the Timeline's
    running and blocked colours.
  - **Data sources** (A6). `UI.Sources` is the header's popover, with runs
    grouped by `Stronghead.Run.groupRuns` over every artifact's and
    connection's `RunInfo`. Each source opens its panel; each run lists what
    is missing with copyable build and run lines. **Same run as …** joins an
    unmatched run by hand: the shell then gives that run's sources the other
    run's `RunInfo` as it hands them to every panel (`linkTimeline`,
    `linkHeap`, `linkLive`), and **Unlink** undoes it.
  - **Status bar** (A7). The followed eventlog, the agent, the ghc-debug
    target and GHCi each open their panel on a click and have a menu (stop
    following, disconnect, reconnect, snapshot, stop or restart the
    session). The shared cursor's time is shown and links to the instant.
  - `--mark TEXT` sends a connected program a mark once it has streamed a
    couple of seconds, for scripts and the screenshot check.
  - Checked by hand on Windows: `stronghead-sample 300 +RTS -l -hT -i0.1 -T`
    run from `scratch/`, then `stronghead --connect 127.0.0.1:4777 --mark
    "p4 mark"`. Connect followed `stronghead-sample.eventlog` and the `.hp`,
    took `scratch` as a source root, and skipped ghc-debug and dumps with
    their reasons. The mark showed on Live's charts, stamped, and in the
    Timeline's markers list and on its ruler.
  - Found on the way: `getProgName` keeps `.exe` on Windows, but the RTS
    names its files without it. Under the sample's late GC load frames come
    a second or more apart, so a gap is measured against the usual spacing.
- **Deferred from P4:**
  - **Overwriting the project root.** Connect adds the program's working
    directory to the source roots instead of writing the Project root
    setting, which is saved and would outlive the connection.
  - **Anything that needs a running ghc-debug-stub.** The socket found by
    pid, snapshots from Live, Census + snapshot, Snapshot twice and
    Pause/Resume build with `-f ghc-debug` and are tested through
    `socketFor`, `probeAttach` and the actions' disabled states, but none
    was run against a target: the stub needs Linux or WSL.
  - **Program time on the snapshot itself.** The moment carries it; the
    heap panel's snapshot label still reads the wall clock, and changing
    `HeapSnapshot` belongs with the Phase 6 work on `Stronghead.Debug`.
  - **Undoing a probable match in one click.** Only runs joined by hand can
    be unlinked; a same-program, no-pid match stays on.
  - **Moments across restarts.** They live on the bus; sessions (W3) would
    keep them.
  - **Path mapping for a remote program.** A working directory that isn't
    on this machine is skipped, and said to be.
  - **Statuses between frames.** A thread's strip samples its status four
    times a second; the Timeline has the exact spans.
- **P5** is in place, W1–W4:
  - **Peeks are data.** `lensPeek` returns a `Peek`: facts and notes as
    text, an exactness note ("exact: thread id in the same run", "by name:
    module and binding name", "approximate: a .hp file keeps its own
    clock"), and an optional drawing (a sparkline, badges). The hover peek
    names each panel's kind of match beside its title; the inspector and
    the report write it out. `runNote` words a time-keyed match: the same
    run key is exact, a match `runKeysMatch` lets through is "probable".
  - **Inspector** (W1). `UI.Inspector` shows the focus: its title and
    reference, **Pin**, **Focus everywhere**, **Go to** and **Copy
    reference**, the source span any lens gives it, then a section per lens
    that shows, peeks, relates or acts on it: the exactness note, the facts
    and notes, related refs as links, **Show in …** and the lens's actions
    as buttons (disabled ones with their reason as a tooltip). It takes up
    the focus every frame (`inspectorSync`), and keeps its own back and
    forward, which refocus the entity; **Follow** off holds it. It is in the
    Heap layout beside the analysis and in Core & STG's under the bindings,
    and any panel can add it.
  - **Pinboard** (W2). Pins live on the bus (`busPins`); every ref menu has
    **Pin** / **Unpin**, and so does the inspector. The pane lists them by
    `refTitle`, with a note each (a text input over the pin), up, down and
    unpin, and a click on the title goes to it. **Copy report** and
    **Export…** (a save dialog; `.md` is added when there is no extension)
    give the Markdown from `Stronghead.Session.pinsMarkdown`: a heading per
    pin, its `refTarget` as code, the source span, each lens's facts and
    notes as text with its exactness note, and the note as a quote.
  - **Sessions** (W3). `Stronghead.Session` holds `Session` and its
    `key = value` codec: the eventlog and following, the `.hp` and `.prof`,
    the heap source, the ghc-debug socket, the dump folder and module or
    the files opened alone, the agent's address and whether it is
    connected, the GHCi command, directory, file and whether it runs, the
    pins with notes, the cursor, the range, the focus, every workbench's
    tree, the user's presets, the popped-out panes and the panel in front.
    Lists are numbered keys; values escape only what a line would lose
    (line breaks, spaces at the ends, and a `%` that would read as one of
    the five escapes), so references (`source/sample%2FMain.hs/82/…`) and
    paths are written as they are. Files are written to a `.tmp` and
    renamed. **Save session…** and **Open session…** are in the header's
    Workspace popover and the palette; `.stronghead` opens from the command
    line and by drag and drop (`openAny`). Settings gained **Reopen the last
    session on start** (`reopen-last-session`, off); the session is saved to
    `last.stronghead` beside `settings.conf` on quit, except in a screenshot
    run, and reopened only when the command line opens nothing.
  - **Mixed workspaces** (W4). `UI.Workspace` registers 22 pane kinds by id
    and session name, and a workbench's pane ids are those kind ids, so a
    tree says what is in it and a kind is in a workbench at most once;
    split ids are 1000 and up. `Workbench` is now a named tree. Each panel
    exports its panes (`timelinePanes`, `heapPanes`, `corePanes`,
    `debugPanes`, `livePanes`): a function from what the frame's loaders
    returned and the panel's state to a `PaneDraw`, with an in-pane empty
    state when its data isn't loaded. The panel views draw their toolbars
    and readouts and hand their workbench to the shell's `Bench`, which
    draws the grid with the panel's own panes, any other panel's from the
    same loader output, and the inspector and pinboard. No pane draws from
    its panel being on screen.
    - **Add** in the header's **Workspace** popover puts any kind beside the
      arrangement (`addPane`); the pane header is a `paneDragHandle`, so a
      drag swaps or splits within the workbench; each pane's **⋯** menu has
      **Move to …** (any other panel's workbench), **Pop out** and **Close
      pane**. Double-clicking a header still maximises.
    - The five panels' layouts are `panelPreset`s; **Leak hunt** (heap
      chart, bands, live graph, source, inspector) and **Latency**
      (capabilities with their code tracks, threads, inspector) are the
      seeded `userPresets`, and **Save as a preset** adds the current
      arrangement. Presets are saved with sessions.
    - A time pane placed with `tl.caps` or `tl.threads` takes the
      Timeline's window: the heap chart through the Lock to Timeline path
      (`chartWindow`, reading and writing `busWindow`), and Live's charts
      by drawing the window's span when the run matches and frames are on
      the program clock.
    - **Pop out** moves a pane into a nano-ui `window`; closing the window
      puts it back in the workbench it came from. A workbench that still
      holds a popped-out kind draws a placeholder with **Bring it back
      here**, so a pane is never drawn twice.
    - Trees are written as `(v 0.740 (h 0.500 tl.caps tl.threads)
      inspector)`; `decodeTree` refuses unknown names, repeats, ratios
      outside (0, 1) and junk, and checks `validateGridTree`.
  - Tests: `stronghead-test` round-trips a full session and the empty one,
    escapes and unescapes values, keeps hand-written references and
    numbered pin order, skips bad keys, and covers pinning, moving pins and
    the report text. `stronghead-ui-test` drives the inspector (content,
    pinning, following, its own back and forward, holding), the pinboard
    (titles, reorder, click to go, report text from the lenses' peeks), the
    registry (unique kinds, valid presets, tree text round trips, refused
    trees, add and remove) and a workbench's pane menu (move to another
    workbench, pop out, the placeholder).
  - Checked by hand with screenshots on the sample's `-l -hT` eventlog:
    the Heap layout with the inspector; a hand-written session opening the
    **Leak hunt** arrangement (heap chart, bands, live graph, source and the
    inspector on `THUNK_2_0`); the same session with a pinboard of two pins
    with notes; a Timeline workbench with the heap chart under the
    capabilities on the Timeline's axis and the pinboard popped out;
    Core & STG with the inspector under the bindings; and, connected to the
    sample running with `+RTS -l -hT -i0.1 -T` through a session with
    `agent-connected = true`, Live's charts under the capabilities drawing
    the followed Timeline's window, its markers lined up.
  - Found on the way: with a sixth header button the title bar overflowed
    at the default window width, so the active panel's file shows only when
    the window is at least 1640 wide.
- **Deferred from P5:**
  - **`commitPaneDrop`.** Panes are added from the Workspace popover, beside
    the arrangement, and placed by dragging their header, which nano-ui
    commits itself. Dropping a new pane straight onto a spot would need a
    drag source that lives on after the popover closes and feeds
    `pgrDropTarget`; that wasn't attempted.
  - **Dragging a pane between panels.** Each workbench is its own pane grid
    and only one panel is on screen, so there is nothing to drop onto;
    **Move to …** does it.
  - **Pop-out size.** nano-ui's `window` takes no layout, and a pane needs a
    bounded box, so a popped-out pane is a fixed 720 × 460: resizing the
    window scrolls it rather than growing it. Sessions keep which panes are
    out, not where their windows were.
  - **Keys follow the panel, not the pane.** A panel's keys (`[` and `]`
    stepping collections on the Timeline, censuses on the heap chart) belong
    to the panel in front, so a Timeline pane in the heap panel steps
    censuses.
  - **Big empty states.** A panel with nothing loaded still shows its own
    empty state unless its workbench holds another panel's panes (the
    inspector and pinboard don't count), so the default Heap layout's
    inspector appears once a profile is loaded.
  - **Moments and run links in sessions.** The marks and snapshots sent to
    a program, and runs joined by hand, belong to a run that is over once
    the program exits; the followed eventlog keeps the marks themselves.
  - **The Workspace popover headlessly.** It lives in `UI.Shell`, which
    needs nano-ui-sdl's dialogs, so the UI tests drive the registry, the
    pane menu and the panes, not the popover. Screenshot runs can't click,
    so the menus are checked by the tests only.
  - **Notes** are one-line inputs; a long note scrolls.
- **P7** is in place, G1–G3:
  - **Next steps** (G1). `Stronghead.Next` is a pure engine: from the facts
    the lenses know about the focus and what is loaded (`Have`), it proposes
    steps, each a title, a one-line reason, what to do and whether it is
    strong. Lenses gained `lensFacts`, in the engine's terms:
    - Heap: a band's leak score and censuses survived, thunks, allocation
      site and binding (from `-hi`), the surviving group whose thunks match
      it (`matchBand`) and what holds it (the retainer path's anchor, a
      thread when it is a TSO); a cost centre's inherited share from the
      `.prof`; a range's top suspect.
    - Timeline: a thread's longest blocked span (as a range), its waker
      (`wokenBy`), the black hole's owner, its most sampled cost centre; a
      range's paused share, allocation, live bytes at either end, top ticky
      allocator, most sampled cost centre and last census; a binding's
      ticky totals.
    - Core: a binding's badges (W/W, boxing sites, STG lets) and the band
      its lets hold most of at the cursor (from the heap lens's
      annotations).
  - The flows follow the plan: leak (site, binding, snapshot twice or the
    surviving group, its holder, break where allocated), contention (the
    blocked span, the owner or the waker, what it ran, its binding), hot
    path (binding; lets with ticky; the heap they hold; break on entry)
    and GC pressure (Analyse range, top allocator, census at the end, and
    `+RTS -A64m` when live grew by under 5% of what was allocated). A
    step is `StepGo` (a ref, followed by `goTo`), `StepAct` (the first
    panel action, or palette command, whose label contains the text),
    `StepLoad` (a `Datum`: the kinds of data and their recipes moved from
    `UI.Sources` into `Stronghead.Next`, which the popover now imports) or
    `StepTry` (a flag, copied). So no step has a code path of its own.
  - The inspector lists the steps under the entity's name as buttons, the
    target as a link, the reason under each, and a missing datum's build
    and run lines with **Copy**. A peek shows the first strong step in a
    line: a suspect scoring 0.6 or more, a block of 10 ms or more with a
    known waker or owner, a cost centre with 10% of time or allocation, a
    range paused 15% of the time.
  - **Watchers** (G2). `Stronghead.Watch` holds the four built-in rules
    (off by default), their text form for sessions, the observations the
    streams give (`frameObservations` from consecutive agent frames: the
    threads, the GC share of CPU between them, a major collection when the
    last collection was one; `timelineObservations` from a followed
    eventlog after a given time: major collections with live bytes after
    them, ended blocked spans, censuses, and the GC share of each whole
    second) and `watchStep`, a pure fold. Each rule has a cooldown (program
    time) and a hysteresis whose meaning depends on the rule (a dip in live
    bytes or a band smaller than `h` neither breaks nor extends a run; a
    thread must run `h` of the limit before its next block counts; the GC
    share must fall below `(1 - h)` of the limit). A rule disarms per
    subject (a thread, a band) until it clears.
  - `UI.Watchers` feeds them every frame with what is new since the last:
    frames give threads and the GC share, the followed eventlog major
    collections and censuses (and blocked spans and the GC share when no
    agent is connected). Responses go through Live's paths: **mark** by
    `sendMark`, **census** and **snapshot** by Live's remote actions, each
    fired event saying what was done or why not. **Notify** counts on a
    status-bar badge, whose click opens the list, and shows a callout over
    it for eight seconds. The **Watchers** pane (`watchers`, a shell pane)
    lists what fired, each with its instant and its thread or band as
    links, then each rule's switch, limits, responses, cooldown and
    hysteresis. Rules are saved in sessions as `watch.N = live-up n=5
    on=true responses=notify,mark cooldown=30 hysteresis=0.02`.
  - **Baselines** (G3). `Stronghead.Diff` holds `BaselinePaths` and
    `Baseline` (the summary, heap profile, cost totals and binding
    summaries), `diffHeap` (by name, then by provenance key: module, label,
    span, kind and type, since `-hi` addresses change between builds; gone
    below a hundredth of the old peak, shrank or grew past a fifth),
    `diffCosts` (by module and label), `summariseModule` and
    `loadBindingSummaries` (worker, boxing sites, lets, Core size per
    `nameKey`) with `diffBindings`, and `fmtChange` for the readouts.
  - `UI.Baseline`: **Mark as baseline** (palette, every diff pane) keeps
    what is loaded at that moment, in memory and by its paths; a session
    keeps the paths (`baseline.eventlog`, `baseline.heap-profile`,
    `baseline.cost-centres`, `baseline.dumps`) and loads them again in a
    `useTask`. The **Heap diff**, **Cost-centre diff** and **Binding diff**
    panes (`diff.heap`, `diff.costs`, `diff.bindings`) list changes as
    links; a gone band links to its allocation site when it had one. The
    Timeline's readout shows each figure's change, the heap's its peak and
    suspects, and the leak suspects a line each for the baseline's band
    and a **Since the baseline** list of the baseline's suspects. The
    status bar names the baseline, with a menu to clear it. A **Before and
    after** preset puts the heap chart beside the three diffs.
  - Tests: `stronghead-test` covers each flow's steps, their order and
    strength, the missing-data steps; every rule on synthetic streams
    (firing, staying off, cooldown, hysteresis re-arming, per-thread
    stretches, foreign calls left out, eventlog spans, the GC share's
    duration), the frame and eventlog observations, the rules' text
    round trip; the heap diff (gone, by source, the same, new, order,
    scores, growth), the cost and binding diffs, the readout format; and
    sessions with watchers and a baseline. `stronghead-ui-test` drives the
    inspector's steps (listed, reasons, recipes, a button that follows its
    link), a watcher firing on frames through `watchTick` (once, its mark
    sent through the hook, the badge counting it, the pane listing it) and
    the heap diff (marking, naming the baseline, the leaking band gone).
  - Checked by hand with screenshots, on the sample:
    - an `-hi` investigation build's eventlog and dumps, focus on the top
      suspect (the `main` thunk at `Main.hs:82`): the inspector's next
      steps are its allocation site, `Main.main` in Core & STG, how to
      snapshot (ghc-debug, missing on Windows) and Break where allocated;
      on a `-hT` run the first step is to profile by info table;
    - a worker thread of a `-hT` run: its 82 ms block on an MVar, the
      thread that woke it, and how to sample what it ran; a 7 s range:
      Analyse range (68% paused), ticky, the census at its end, and
      `-A64m` (5.6 GiB allocated, live up 53 MiB);
    - the sample running 90 s with `+RTS -l -hT -i0.1 -T`, connected
      through a session that switches on three watchers: within 20 s the
      pane listed a worker blocked on an MVar, `THUNK_2_0` growing for 5
      censuses and live bytes up across 3 major collections (marked through
      the agent), with the badge and the callout;
    - the sample before and after the `modifyIORef'` fix (built into its
      own builddir from a temporary edit, then reverted), the first as
      the baseline: the heap diff shows `THUNK_2_0` gone (38.1 MiB to
      64 B) and the `Integer` `IS` band with it, the readout's peak total
      −75%.
  - Found on the way: the fixed sample still keeps 64 bytes of
    `THUNK_2_0`, so "gone" is a hundredth of the old peak, not zero. The
    blocked-thread rule first fired on the IO manager and the agent's
    threads, which sit in foreign calls; they are left out. Frames carry
    only the last collection's details, so with an eventlog followed the
    major collections come from it. `useTask` hands back the previous
    key's result while a new key loads, so the baseline's and the dump
    folders' loads carry their key and are checked.
- **Deferred from P7:**
  - **What the waker ran during the span.** The contention flow's "what it
    ran" is the thread's whole CPU profile: a step on the waker doesn't
    know which blocked span the investigation came from. The span
    inspector's **What ran during this slice?** (P6) has the slice's.
  - **Top allocators from samples** (the P6 deferral stands): only ticky
    counts allocation; samples give "what ran", labelled as time.
  - **Snapshot twice and the survivor steps end to end.** They need a
    ghc-debug target, which this machine can't run (V6); the steps are
    tested on facts, and **Snapshot twice** resolves to Live's palette
    command.
  - **Steps for collections, markers, instants and modules.** The engine
    has no flow for them.
  - **User-defined watcher rules.** The four built-in rules, one of each,
    are configurable; there are no new kinds, per-thread filters (beyond
    leaving out foreign calls and the agent) or rules on ticky or samples.
  - **Watching history.** Rules run while Stronghead runs; when a followed
    eventlog is first read from its start, firings more than five seconds
    older than the newest observation are listed without acting. The fired
    list belongs to the run and is not saved with the session.
  - **A baseline overwritten in place.** Marking keeps the loaded data for
    the rest of the session, but a session reopens the baseline from its
    paths: an eventlog written again to the same file reopens as the new
    run. Write the second run elsewhere (`-ol`, `-po`).
  - **Deltas everywhere.** The Timeline's and the heap's readouts and the
    leak suspects show them; Live's readout, the cost-centre tree and Core
    & STG's runtime columns don't (the diff panes have the numbers).
  - **Bindings with one key.** The binding diff matches by module and
    `nameKey`, so a worker and its wrapper, or two local bindings that
    strip to one name, are one row.
  - **Peeks on canvases** (from P1) still have no peek, so neither do their
    strong steps; list rows and links show them.

## Verification spikes

Run in P0. Each records what to try and the fallback if the assumption fails.

| # | Assumption | How to check | If it fails |
| --- | --- | --- | --- |
| V1 | IPE table names name STG binders from the same build (`sat_s3Pe_info` ↔ `let sat_s3Pe`) | Build the sample with `-finfo-table-map -fdistinct-constructor-tables -ddump-stg-final -ddump-to-file`, run `+RTS -l -hi`, compare `itTableName`s with the STG binders | Join through the IPE label and span to the source binding: "allocated in `main`" rather than the exact `let` |
| V2 | Live's `elapsed_ns` and eventlog timestamps share a clock | Have the agent post a marker and report `elapsed_ns` at the same moment; compare | Derive a per-run offset from an agent-posted marker (A3) |
| V3 | `.hp` census times are elapsed time, not mutator time | Compare a `.hp` with the eventlog heap samples from the same run | Mark `.hp` time as approximate and prefer eventlog heap samples |
| V4 | A profiled build run with `-p -l` writes `ProfSampleCostCentre` that decodes on 9.14 and Windows | Run a profiled sample; count the events and their stack depths | D1 falls back to `.prof` totals only |
| V5 | `-ticky` with `+RTS -lT` writes decodable ticky events | Run a ticky build | D2 reads ticky's text output instead |
| V6 | ghc-debug-client 0.8 decodes stack frames with source info | Linux or WSL target, decode a STACK | D3 shows the stack's pointers without frames |

### Findings

Run on 2026-10-20, GHC 9.14.1 on Windows 11, against the P0 branch. Every
artifact is under `scratch/` (ignored): `scratch/ipe` (V1, V3),
`scratch/v2`, `scratch/prof` (V4) and `scratch/ticky` (V5). The checker is
`scratch/tools/Spike.hs`. Each variant is built into its own `--builddir`,
so the normal build is left alone:

```
cabal build stronghead-sample --builddir scratch/build-ipe --ghc-options="-finfo-table-map -fdistinct-constructor-tables -ddump-stg-final -ddump-simpl -ddump-to-file -dumpdir scratch/ipe/dumps"
stronghead-sample 4 +RTS -l -hi -i0.1 -olsample-hi.eventlog -RTS
```

**V1 holds for thunks and functions.**

- The run wrote 628 IPE records and sampled 69 bands.
- All 8 `Main` thunk and function bands have a table name that is an STG
  binder plus `_info`. For example, `main2_sat_s7qA_info` is the binder
  `main2_sat_s7qA` in `Main.thr.dump-stg-final`.
- The binder carries its enclosing top-level name (`main2_`), not the bare
  `sat_s3Pe` the plan assumed. `Stronghead.Dump.stripUnique` reduces it to
  `main2_sat`, so S4 must match the full binder.
- The other 11 `Main` bands are constructor tables (`I#_Main_3_con_info`,
  `Bin_Main_8_con_info`). They name an allocation site, not a binder. They
  still have a label and a span, so they need the fallback join.
- Only 24 of the 69 bands have an IPE record. Code from libraries built
  without `-finfo-table-map` (base, containers) has none. Those 24 bands hold
  67% of peak residency.
- 286 of the 628 records have no location. The RTS writes a missing file and
  span as a bare `:`, which `parseSrcLoc` reads as no location.
- Paths are as GHC saw them (`sample\Main.hs` on Windows), and multi-line
  spans appear (`sample\Main.hs:(93,25)-(94,84)`).
- Dumps from this build are named `Main.thr.dump-stg-final`, with the build
  way in the name. `Stronghead.Dump.discoverArtifacts` will read that as a
  module called `Main.thr`.

**V2 holds with a fixed offset, and needs no marker.**

- The test program posted `traceMarkerIO (show elapsed_ns)` 200 times over
  3 s, under GC load, with `-N1` and with `-N2`.
- Event time minus `elapsed_ns` was 1.284–1.377 ms (median 1.297 ms) and did
  not drift. `init_elapsed_ns` was 1.281 ms in that run.
- The two clocks start at different moments. `elapsed_ns` counts from the end
  of RTS initialisation; eventlog timestamps count from its start.
- P0 adds them together. The agent now also sends `init_elapsed_ns`, and
  `Stronghead.Live.frameClock` uses it.
- The remaining ~15 µs is the time between reading the statistics and posting
  the marker. Frames from an older agent are about 1 ms early.
- The clock doesn't need A3's per-run marker offset.

**V3 fails: `.hp` times are not elapsed time.**

- One `-hi` run produced both files. The eventlog has 7 censuses. The `.hp`
  has 9: an empty one at 0 and a final one at exit.
- Paired by total bytes, which match exactly, `.hp` time runs at 0.62–0.69 of
  program time. It doesn't match elapsed time minus GC time either.
- Its values are multiples of 1/64 s, the Windows CPU-time tick. So they are
  most likely mutator CPU time, whose rate depends on parallelism and GC load.
- Fallback adopted: `.hp` time is approximate, and eventlog heap samples are
  preferred.
- A better join than the fallback: when both files come from one run, pair
  censuses by equal totals.
- A non-profiled build writes only the program name to `JOB`
  (`"stronghead-sample.exe"`). Neither the breakdown nor the RTS flags can be
  read from it. A profiled build writes the whole command line.

**V4 holds, with a Windows caveat.**

- The profiled build (`--enable-profiling --ghc-options=-fprof-auto`) took
  about 2 minutes. It ran with `+RTS -p -l -hc -i0.1`.
- 796 `ProfSampleCostCentre` events decode, with stack depths from 1 to 20
  (548 of them depth 1). There are 29,755 `HeapProfCostCentre` definitions.
- The profiler's clock ran slowly. The `.prof` reports 0.06 s and 249 ticks
  for a 3 s run on 4 capabilities, and `-hc -i0.1` took no non-empty census
  in 3 s. D1 will see few samples on Windows.
- The `.prof` header and the `.hp` `DATE` gave the same start minute.
- The `.prof` command line includes the `-with-rtsopts` flags (`-N4 -T`),
  which the eventlog's `ProgramArgs` lacks.

**V5 holds.**

- A `-ticky` build ran with `+RTS -lT`. It wrote 36 `TickyCounterDef` (17 in
  `Main`), 200 `TickyCounterSample` and 14 `TickyBeginSample`.
- Only counters that were entered are defined.
- Counter names carry the STG binder
  (`main2_sat_s7qL{v} (Main) (fun) in s7qM`) and an info table address. D2
  can join ticky to STG binders and IPE records the same way as S4.

**V6 was not run.** It needs Linux or WSL.

What this changes in the plan:

- The clock table above now uses `elapsed_ns + init_elapsed_ns` for Live,
  and pairs `.hp` censuses through an eventlog.
- S4 matches full binder names, without stripping uniques, and joins
  constructor tables by label and span.
- C1 can treat eventlog and Live times as one clock without A3. It should
  pair `.hp` censuses by total with an eventlog from the same run, and show
  times from a `.hp` alone as approximate.
- `RunInfo` reads the program from a `.hp` `JOB`, but not the RTS flags,
  which a non-profiled build leaves out.
- Core & STG's dump discovery should strip a way suffix (`.thr`, `.p`,
  `.dyn`) from the module name. This is a P3 prerequisite and is not done in
  P0. *(Done in P3: `dumpModuleName`.)*
- D1 should expect sparse profiler samples on Windows.

## Testing

- **Library tests** in `test/Main.hs`'s style, with inline fixtures and
  `check`:
  - span forms, including `<built-in>`, `<entire-module>`,
    `<no location info>`, multi-line spans and Windows paths;
  - path resolution: absolute, root-relative, suffix and module name;
  - IPE records and cost-centre spans;
  - run matching;
  - the ref round trip;
  - agent v2 lines, with v1 lines still parsing;
  - fuzzy ranking;
  - wakeup attribution;
  - `-hc` stack ↔ `.prof` path;
  - table-name normalisation.
- **An investigation-build recipe**, a `cabal.project.local` snippet and a
  run line that produce every artifact from `stronghead-sample`, for checking
  the walkthroughs by hand. The artifacts stay out of git (`scratch/` is
  ignored).
- **UI flows** with nano-ui's `NanoUI.Testing` harness: a link click switches
  panel and selects the target, Alt+← returns, and a palette query finds a
  thread. Screenshots for review come from `STRONGHEAD_CAPTURE`.

## Non-goals

- **Editing source.** S7 opens your editor.
- **A general debugger protocol** (DAP) or non-GHC targets.
- **Running arbitrary code through the agent.** Its commands stay a small
  fixed set.
- **A second OS window.** Pop-outs are nano-ui floating windows.

## Decisions for you

1. **Click semantics.** I recommend the additive scheme above: today's clicks
   stay and Ctrl+click navigates. The alternative is a plain click navigating
   from any link, which is simpler to learn but yanks you between tabs.
2. **Agent control by default.** I recommend on for loopback and off
   otherwise.
3. *(Done in P5.)* **Mixed workspaces (W4)** are the largest refactor, because panes would
   stop owning their panel's state. I recommend deciding after P1–P3. Peeks,
   the inspector and the cross-panel strips may make it unnecessary.
4. **nano-ui upstream.** The link widget, the palette list and the annotated
   code view are generic. I recommend building them in Stronghead first and
   moving them into nano-ui once they settle.
