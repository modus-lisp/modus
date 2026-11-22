# Coroutines on hosted x86-64

Status: stage 1 implemented (net/hosted-coroutines.lisp, test/hosted-coroutines.lisp).

## Why

cortez runs TI Explorer's stack groups: their processes, their scheduler,
their listener.  A stack group is a coroutine.  Only one runs at a time, and
resuming one suspends the resumer.  On SBCL cortez gives each stack group an
OS thread and hands control between them with semaphores, with every thread
sharing one heap.  Modus threads share no state, so the first `SETQ` of a
global from a stack group's thread is refused ("a thread stored one of its
own objects into shared memory").

The faithful shape is the machine's own: one OS thread, several stacks, and
an explicit transfer between them.

## What already exists

- `SAVE-CONTEXT` / `RESTORE-CONTEXT` (translate-x64 `+op-save-ctx+`): a
  setjmp/longjmp over RSP, RBX and RBP plus a continuation RIP.
  `%HA-CTX-SELFTEST` (net/hosted-actors.lisp) drives coroutine round trips
  with it.  Locals live in frame slots, so everything held across a switch is
  on the stack RSP restores.
- The **per-thread window** (compiler.lisp, THE PER-THREAD WINDOW).  It is
  selected by a segment base and holds the MV buffer, the armed handler-case
  frame and handler stack, and the nargs slot.  Its block also holds the
  deep-binding stack for dynamic bindings (prelude.lisp, PER-THREAD DYNAMIC
  BINDINGS).
- **Stop-the-world** (docs/macos-hosting.md, "Stop the world").  A region-0
  collection waits for every other live thread record to be PARKED or SAFE.
  It then roots each such thread's stack from its published SP to its stack
  top, plus its window's MV buffer and bindings.

## The design

A coroutine is the **hosted actor runtime's computation without a region of
its own**, on the main OS thread, switched by an explicit transfer.  The actor
runtime already gives a computation its own stack and window and already
registers a parked one with the collector (the parked-actor scan table,
`EMIT-STW-SCAN-PARKED-ACTORS`); a coroutine uses exactly that, with its window's
region words 0 so it allocates in region 0 and the shared-store guard has
nothing to refuse.  The two cannot run in one process: they share the one
parked-computation table, and `COROUTINES-START` refuses when actors hold it.

- **Create** (`COROUTINE-CREATE fn`): a 32 MB stack with a guard page below
  it, a clean window with its own binding stack and no region, a save area
  whose first entry is `%CO-ENTRY`, and a parked scan entry.
- **Transfer** (`COROUTINE-TRANSFER id value`), in order:
  1. `SAVE-CONTEXT` into the current computation's save area, and register it
     as parked at the SP that recorded.
  2. Set the main thread record's stack top (`+0x78`) and region 0's
     stack_base to the target's stack top, so the collector bounds the live
     stack correctly.
  3. Switch the FS base to the target's window, and the record's window word
     (`+0x38`) with it.  Base 0 is the main computation, exactly as today.
  4. Mark the target running and `RESTORE-CONTEXT` into it, VALUE handed over
     in `*CO-VALUE*`.
- **Exit:** a coroutine whose function returns transfers back with the
  value.  Its record and stack are freed when it is unreachable or killed
  explicitly.  Freeing is not needed for cortez's first use.

Consequences:

- Dynamic bindings, the handler stack, `CATCH`/`THROW`, unwind frames and
  multiple values are per coroutine, because they live in the window.
- A region-0 collection taken in any coroutine roots every parked one,
  through the same code that roots parked threads today.
- The shared-store guard does not fire: a coroutine has no region pair, so
  nothing it allocates is "its own region's".
- The main context still shallow-binds into global cells, because it has no
  window block.  So a program should run its coroutine-visible work inside
  coroutines.  cortez's initial process will be one.

## Risks to measure first

1. **Stack top for the running record.**  The collector's live window must
   end at the running coroutine's stack top, not the process stack base.
2. **Faults on a coroutine stack.**  Signals use the alternate stack, so a
   fault report or a recovery into a `HANDLER-CASE` must land on the right
   stack.  A guard page catches overflow.
3. **Arming.**  The thread table and windows exist only once the threaded
   runtime is armed (`#x10000DB8`).  A coroutine program arms it; an image
   that never makes a coroutine is unchanged.
4. **The JIT.**  Native pages, bridge thunks and the hot-function JIT keep
   global state.  It is main-thread state, and a coroutine is still the main
   OS thread, but `%MVM-ON-MAIN-THREAD-P` must answer true in a coroutine.

## Stages, each gated

1. Primitive plus self-test (`test/hosted-coroutines.lisp`):
   - N transfers between three coroutines;
   - consing across transfers, with forced collections from inside each
     coroutine and objects held only on parked stacks surviving;
   - a dynamic binding visible only to the coroutine that made it;
   - `HANDLER-CASE`, `THROW` and an error inside a coroutine, none of which
     cross into another coroutine;
   - a coroutine that returns.
2. cortez's stack groups on modus as coroutines instead of threads, then
   stack-group-gate.
3. TI's scheduler on modus: scheduler-gate, then the relaunch gates and the
   boot to their Listener.
