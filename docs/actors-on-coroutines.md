# Actors on coroutines — design note

*Status: decided 2026-10-06, not built.  This note is what the gate tests for
the work get derived from.  Supersedes the memory model of
`docs/hosted-actor-runtime.md` ("a GC region per actor slot"); the API and the
scheduler core it describes stay.*

## The decision in one paragraph

An **actor** stays the unit of isolation, failure and messaging, with the API
it has (`actors-spawn`, `actors-send`, `actors-receive`, `actors-receive-if`,
timeouts, `actors-link`).  What changes is what backs it.  Today an actor is
backed by its **own GC region** and a stack that **migrates** across native
threads.  Under this design an actor is a **coroutine on one native thread,
sharing that thread's heap**.  The unit of *memory* becomes the native thread;
the unit of *isolation* stays the actor; the two are joined by one rule that is
already the system's rule: **a value crosses a thread boundary only by copy**.

This is the apartment model (COM's single-threaded apartments, one Lua state
per thread), not Erlang's heap-per-process.  Rejected alongside it: one shared
heap with thread-local allocation buffers and a stop-the-world or G1-style
collector.  That is the right collector for a conventional Common Lisp; it
loses per-domain GC pauses and leaves isolation to discipline, which is the
wrong trade for an actor operating system.

## Why

Every defect in the 2026-09 threading campaign had one shape: a **pointer from
a shared structure into some actor's or thread's private region**, which
nothing enforced and which the next collection of that region turned into
silent corruption — the intern table, `CL:INTERN`'s symtab, special bindings,
`TERM-ENCODE`'s symbol payloads, the lock arena, the actor stacks, the
parked- and running-actor STW scans.  The fixes were plumbing around the rule
(an immortal arena, a heap hop in `%RT-ENTER`, per-thread dynamic-binding
storage, a shared-store guard).  Per-actor regions multiply the number of
places the rule can be broken by the number of actors.

Per-**thread** regions already exist (the hosted thread regions, their
stop-the-world for region 0, the per-thread window and dynbind storage) and the
shared-store guard already turns the one remaining cross-boundary store into a
refusal.  Making actors coroutines *inside* a thread's heap deletes the actor
half of the problem and keeps the half that works.

It also unblocks **128 native threads**: with per-actor regions the thread count
was tied to heap geometry (a 16 MB semispace pair per slot, carved from region
0); with per-thread regions it is one carve per thread and the actor count is
free.

## The model

| | today | this design |
|---|---|---|
| heap | one GC region per **actor** slot, carved from region 0 | one GC region per **native thread** (the existing thread regions, lifted to 128) |
| stack | per actor, migrates between threads | per actor, **pinned** to its thread's coroutine pool |
| window (handler frames, dynbind, MV buffer, STW words) | per actor | per actor — unchanged, it is state of the *computation*, not of the heap |
| GC roots for a thread's collection | its own stack + a global table of parked actors + the running actor's stack and region | **every coroutine stack on this thread** (a per-thread list) + the thread's window set; nothing cross-thread |
| who collects an actor's garbage | the actor's own region's collector, from whichever thread runs it | the owning thread, stopping only that thread's coroutines |
| message within a thread | copy | copy (see "Same-thread sends") |
| message across threads | copy | copy |
| shared-store guard range | the actor's region pair | the **thread's** region pair — the guard as built, unchanged |
| migration | free (the region travels) | **by deep copy**, rare, later |
| preemption | none hosted (YIELD is a NOP) | watchdog + reductions at safe points |

### What goes away

From `net/hosted-actor-runtime.lisp` and `translate-x64`:

- the per-actor region pool (`%ar-carve`, control block `+0x200 + id*64`,
  `%ar-region-from/to`, `%ar-clear-region-bits`);
- `actor-region-resume` / `%gc-region-switch` in the actor switch, and the
  `+0x68` region slot's use by actors (bare-metal `net/actors.lisp` keeps it);
- the parked-actor scan table (`+0x1400 + id*48`, `%ar-mark-parked`,
  `EMIT-STW-SCAN-PARKED-ACTORS`) and the running-actor substitution in
  `EMIT-X64-STW-EXTRA-ROOTS` (`MODUS_STW_SCANMODE`) — replaced by the per-thread
  coroutine list below;
- the shared-store guard's per-*actor* region words in the actor's window
  (`+0x5040/48/50` are seeded from the **thread's** region instead).

### What stays

`net/actors.lisp`'s scheduler core (mailboxes, `SAVE-CONTEXT`/`RESTORE-CONTEXT`
switches only through the scheduler, `TERM-ENCODE`/`DECODE`), the per-actor
window and its FS switch, `%with-computation-state`, selective receive and
deadlines, links, the 2 MB guarded actor stacks, `%AR-SCHED-RUN`'s futex idle
protocol, the stop-the-world handshake for region 0, the runtime lock and the
lock arena for interned names.

## Scheduling

**One run queue per native thread.**  Today's single global queue
(`actor-dequeue` under `sched-lock-addr`) becomes N queues, one per scheduler
thread, each protected by its own lock, so a thread dispatching its own
coroutines contends with nobody.  Cross-thread traffic touches another thread's
queue in exactly one case: a *send* to an actor that lives on another thread
enqueues it there (under that thread's lock) and wakes that thread's futex.
This is the only cross-thread write in the scheduler and it writes a fixnum.

**Placement at spawn.**  `actors-spawn` picks a thread (least-loaded by queue
length and live-actor count, overridable with a `:thread` keyword for the
cases that want affinity — a net-domain actor on the thread that polls its
sockets) and the actor lives there until it ends.  Spawning from an actor
defaults to the spawner's thread, so a parent/child pair that chats shares a
heap and never copies across threads.

**A coroutine switch is a stack switch inside one heap.**  `SAVE-CONTEXT` +
FS to the next actor's window + `RESTORE-CONTEXT`.  No region park, no region
enter: R12/R14 are the thread's and stay the thread's.  The "ordering argument"
that made the region hop fragile (an allocation between the save and the
restore) ceases to exist.

**Blocking.**  `actors-receive` with an empty mailbox hands the thread back to
`%AR-SCHED-RUN` as today.  A coroutine that makes a blocking syscall blocks
every coroutine on its thread; that is the one new cost of the model, and it is
addressed by routing I/O through the thread's poll loop (the socket server's
`poll(2)` loop already has this shape): a socket read from an actor becomes
"register interest, hand the thread back, be woken by the scheduler's poll".
File I/O on a local filesystem stays synchronous (it does not block in
practice).  Until the poll integration lands, the documented rule is: an actor
that blocks in a syscall should be the only actor on its thread (`:thread`
placement).

## Preemption: the watchdog and reductions

A hot actor must not hold its thread forever, and the runtime **cannot be
interrupted at an arbitrary instruction**: compiled and interpreted code hold
raw object words in registers between allocations, which is exactly why
stop-the-world parks only at allocation sites.  So preemption is a *request*
honoured at a safe point, never an asynchronous switch.

- **Safe points already exist.**  The compiler emits `YIELD` at every loop
  back-edge (`compile-loop`'s single `(emit-ir :yield)`), a NOP on hosted
  Linux today, and a `:gc-check` at every allocation.
- **`YIELD` becomes `test [preempt word]; jnz slow`** — one compare per loop
  iteration against this thread's preempt word (per-CPU block, so GS-relative:
  `GS:[+preempt-off]`), the same shape the STW flag test has.  The slow path is
  `actors-yield`: the actor goes to the back of its thread's run queue.
- **The gc-check is the other safe point**: the STW protocol already clamps the
  allocation limit so the next allocation parks; the watchdog can clamp the
  same word, so a straight-line allocating hot path without loops also yields.
- **Reductions are the local half** (BEAM's model): a per-actor budget
  decremented at back-edges; at zero the actor yields on its own.  With it, the
  watchdog only has to catch actors that burn time without looping or
  allocating, which are short by nature.
- **The watchdog is one thread with a timer.**  Each scheduler thread stamps
  "running since" (a per-CPU word) at dispatch.  The watchdog wakes every
  quantum (10 ms default; per-actor override for the net-domain), and for any
  thread whose stamp is older than the quantum sets that thread's preempt word
  and clamps its allocation limit.  Two per-thread words, no signal, no lock.
- **What it cannot catch:** an actor parked in a syscall.  That is blocking
  I/O, not a hot routine; the poll loop is its answer.

## The collector

Per-thread Cheney collection as today, stop-the-world only for region 0, with
one change to the **root set**: a thread's roots are the stacks of **all its
coroutines**, not just the one on the CPU.

- Each thread keeps a **coroutine list**: for every actor pinned to it, the
  actor's stack bounds and, when parked, the SP its `SAVE-CONTEXT` recorded.
  The running one is scanned from the live SP (the collector entry) to its
  stack top; each parked one from its parked SP to its top.  This is the
  parked-window scan the collector already has (`+GC-OFF-SAVED-SP+`,
  size-capped), applied N times from a per-thread table instead of once from
  the region block.  Exactly the roots that exist, no global table, no
  cross-thread reads.
- Each coroutine's **window** is scanned as a thread window is today (MV
  extras, dynbind values, handler-frame RBX slots): the per-thread window scan
  becomes a loop over the thread's coroutine windows.
- Regions outside the thread are never scanned — a pointer into another
  thread's heap is a shared-store-guard violation at the store, so none can
  exist.
- The allocation limit clamp used by STW and the watchdog is the same word,
  so "park at next allocation" serves both.

**Stop-the-world for region 0** is unchanged: the fixed tables live in region
0, main is its mutator, and a collection of region 0 still stops every thread
at an allocation and scans every thread's roots — now via each thread's
coroutine list rather than the actor-scan tables.

## Isolation, stated

1. **Within a thread:** no data race is possible, because exactly one
   coroutine runs and it yields only at `actors-receive`, `actors-yield`, a
   watchdog-honoured safe point, or a blocking call.  Coroutines on one thread
   may *see* each other's objects only through messages (copied) or through
   CL globals — the same two channels as today.
2. **Across threads:** an object is reachable from another thread only by copy
   (`TERM-ENCODE`, the lock arena for interned names) or through a CL global,
   and a store of a thread-local pointer into shared memory is refused by the
   shared-store guard before it happens.
3. **Failure:** an actor's unhandled error ends that actor (`%ar-actor-entry`'s
   handler), never its thread or its siblings; a stack overflow faults in the
   overflowing actor (the guard page), never in a neighbour.
4. **GC pauses:** an actor's garbage is collected by its thread, pausing that
   thread's coroutines and nothing else.  Region 0 is the exception, as today.

### Same-thread sends

A send between two actors on one thread *could* pass the object by pointer:
one heap, no race.  It will not, in this design.  The copy keeps the semantics
of a message identical regardless of where the receiver lives (an actor
rescheduled onto another thread by a later migration feature must not change
the meaning of a send), keeps `EQ` across actors meaningless by construction,
and keeps the mutation-after-send hazard impossible.  Same-thread copy is a
measurable cost and a later optimisation with a clear test (the receiver sees a
copy: `(eq sent received)` is NIL), not a semantic choice to make now.

## Migration, later

An actor is its mailbox, its stack and the objects reachable from them.  All of
that is copyable: messages already serialise, and a stack can be moved by
copying the reachable graph into the target thread's heap and relocating the
stack's pointers (a conservative stack makes this harder than it sounds; a
first version migrates only *parked* actors whose stacks hold no raw object
words, i.e. those parked in `actors-receive`).  It is deliberately out of
scope: placement at spawn is enough for the workloads that exist, and BEAM
itself migrates by copying heaps.

## Prerequisites: 128 thread regions

Per-thread regions are capped at 16 today by a handful of literal sixteens
and one address collision.  Lifting them is the first work item on every path:

1. **The per-CPU active-region cell moves into the per-CPU block.**  It is a
   16-word fixed table at `0x10000F08..F88` today, and on hosted x64 its
   entries 3..11 **collide** with the GC statistics words (`0x10000F20..F50`)
   and the core cursor (`0x10000F60`) — a worker with CPU id 3..11 has its
   active-region word under the collector's own statistics.  (aarch64 moved its
   statistics to `0x1000FF00` for this reason; x64 did not.)  As a slot in the
   per-CPU block (`GS:[+region-off]`) it is one load instead of two, has no
   count, and `EMIT-LOAD-GC-REGION`, `A64-LOAD-GC-REGION`, `%GC-REGION-CELL`
   and the STW scans' `+gc-region-addr+ + 8*cpu` reads all simplify.  Mode off
   keeps reading `0x10000F08`; mode on copies that word into CPU 0's slot.
2. **A thread-region arena** in the heap mmap after the guard band, with the
   object-start and cons-kind bitmaps sized to cover it, `MAP_NORESERVE` so
   its virtual size costs nothing until touched: 128 × 2 × 16 MB = 4 GB
   virtual.  Regions must stay inside bitmap coverage (`scan_word` rejects a
   root whose granule has no start bit).  Region 0 is untouched, so every
   single-threaded image and every ANSI gate shard behaves exactly as today.
   Where the arena is absent (macOS, iOS, bare metal) `%HA-FIT-REGIONS` keeps
   carving from region 0 and the thread count is what that affords.
3. **The thread page laid out for N**: records (`+0x100 + 0x80*slot` is a
   4 KB table today), per-CPU blocks, dynbind stacks, window blocks, all
   × 128; `+stw-max-slots+` and the record loops become N.

## Acceptance

Each of these is a red test before its code lands.

- **Preemption:** two actors on one thread, one in an infinite loop with no
  allocation, the other counting messages: the counter advances; the hot actor
  is yielded by the watchdog within 2 quanta (measured with the real clock);
  the same with a hot *allocating* non-looping body (gc-check path); the
  reduction budget alone yields a looping actor without the watchdog.
- **Isolation by construction:** N actors on one thread each mutating its own
  structure across 10 000 switches: every structure intact (no interleaving
  possible); the shared-store guard still refuses an actor storing its object
  into a global, with the error naming the actor.
- **Collector roots:** 8 actors on one thread, each holding a 2000-cons chain
  live in its frame, parked in `actors-receive` while the thread collects 20
  times: every chain walks; a 9th actor running during the collections keeps
  its chain too (the running-coroutine scan); the thread's own stack and window
  still scanned (`test/hosted-dynbind.lisp`'s bitmask on an actor).
- **Cross-thread:** actors on 8 threads in a ring passing a 1000-element
  message 10 000 hops: every receiver sees a copy (`EQ` NIL), checksums match;
  `%GC-COUNT-FOREIGN-REFS` between every pair of thread regions is 0 with a
  positive control.
- **Scale:** 128 threads × 8 actors each, all allocating and collecting; region
  0 collected ≥ 1 time under STW with all 1024 coroutine stacks scanned;
  every per-thread count moves independently.
- **Blocking placement:** an actor blocked in `read(2)` on a pipe does not
  delay a sibling on a *different* thread; the documented rule for same-thread
  siblings is tested as a *known* stall until the poll integration lands (the
  test asserts the stall so the integration has a red test to turn green).
- **Nothing else moves:** the hosted threading bar, the 64-shard ANSI gate
  (NET ≥ 0 after 3× recheck) and the upstream sweep (0 lost) on every step.

## Order of work

1. Per-CPU active-region cell (fixes the live collision; gated on its own).
2. Thread-region arena + thread page × 128 + STW slot count; `hosted-many-
   threads` / `hosted-many-regions` extended to 64 and 128.
3. Per-thread run queues, placement, pinned actors; delete per-actor regions
   and the actor-scan tables; per-thread coroutine list in the collector's root
   scan.  `hosted-actor-runtime.lisp`'s 35 checks must stay green throughout,
   with the ring test's "5 actors over 3 threads" now meaning 5 actors placed
   across 3 threads.
4. Watchdog thread + `YIELD` as a preempt test + reductions.
5. I/O through the poll loop (turns the blocking-placement stall test green).
6. Migration by copy — not scheduled.

## Risks, named

- **The YIELD compare is on every loop back-edge of every hosted program**,
  threads or not.  It must be gated the way `%RT-ENTER` and `%DYNBIND` are: one
  load of a word that is zero in a single-threaded process.  Measure the
  interpreter's inner loop before and after; the budget is "within noise".
- **Window FS switches per coroutine remain**, so the per-actor window cost
  (32 KB block, prepared at spawn) stays.  Fine at hundreds of actors; a
  thousand coroutines per thread would want windows pooled.  Not a 2026
  problem.
- **A conservative stack scan over N coroutine stacks** is N times the stack
  work per collection.  Stacks are 2 MB virtual but parked depth is small;
  the size cap on parked windows bounds the worst case.  The scale test
  measures it.
- **Pinning without migration** means a bad placement is permanent for that
  actor's life.  The placement heuristic and the `:thread` override are what
  make this acceptable; the migration item exists for when they are not.
