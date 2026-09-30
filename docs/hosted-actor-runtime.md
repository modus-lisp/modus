# Hosted actor runtime — plan and gap analysis

Status: in progress (2026-09-29).  Goal: the actor model of `docs/actors.md`
(per-actor heaps, messages by copy, fault isolation) as a usable runtime on
hosted x86-64, scheduled M:N over the hardened native threads, so that
programs (operandi first) can use actors instead of shared-memory threads.

## What exists

- `net/actors.lisp`: the scheduler core — actor table (64 × 128 B), run queue,
  mailbox pool, `send`/`receive`/`yield`, context switch by `save-context` /
  `restore-context` holding the scheduler lock, per-actor regions
  (`actor-region-hop`), staging buffers + `term-encode`/`term-decode`.
- `net/hosted-sync.lisp`: a real per-thread scheduler (`%sched-run`: futex idle
  wait, no-lost-wakeup protocol), the safe blocked hand-back
  (`ap-scheduler-blocked` restores into the thread's scheduler context before
  the lock drops), `%sched-park`, `%sched-stop`, `wake-idle-ap`.
- Selftests only (`%ha-*-selftest`, `%br-selftest`): every use is hand-wired
  (carve, bringup, per-CPU, regions) and runs its scheduler on a thread made by
  `%ha-spawn-t2`, not on the hardened native threads (`%make-native-thread`).
  There is no public API.

## Gaps (each gets a red test before its fix)

1. **No runtime/API.**  No start/spawn-closure/exit; `actor-spawn` takes a raw
   function address whose entry must never return; slots are never reclaimed
   (64 spawns per process, ever).
2. **STW does not see actors.**  A region-0 collection scans stopped THREADS
   (stack, live region, handler RBX, dynbind).  An actor parked on its own
   band stack, or its own region, holding a region-0 object is not scanned —
   stale after the flip.
3. **The shared-store guard does not see actors.**  Its window words hold the
   THREAD's region pair; an actor allocates in its own region, so an actor's
   store of its own object into shared memory is not refused.
4. **Execution context is per thread, not per actor.**  Handler frames and
   depth, the armed frame, the dynamic-binding stack, the MV buffer and the
   per-computation runtime specials bound in `%thr-trampoline` live in the
   thread's window.  Two actors interleaving on one thread across a blocking
   `receive` inside `handler-case`/`let` of a special corrupt each other.
5. **Messages are not sound.**  `term-encode` falls back to encoding an unknown
   object's raw pointer as a fixnum (floats, structs, CLOS instances, hash
   tables…); staging is 16 KB per recipient (`send` returns 0 beyond).
6. **Runtime services in actors** (intern, FORMAT, EVAL, JIT) were declared
   off-limits by the selftests; to re-measure under the runtime lock.
7. **No supervision**: links carry one id, an unhandled error takes the
   thread's handler path, no monitors, no watchdog.

## Order

1. Runtime skeleton on native threads (start, spawn closure, exit + slot
   reuse, blocking receive from actors and from main) — needed to test
   anything else.  Gaps 2, 3, 4 are fixed as part of making the skeleton
   sound, each with its own red test, before anything builds on it.
2. Messages (gap 5), reusing the thread layer's deep-copy semantics.
3. Runtime services (gap 6), supervision (gap 7), capacities.
4. operandi on actors behind a portability layer (SBCL keeps bt threads).
