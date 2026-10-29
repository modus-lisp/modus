# Generational GC — design and staging

Status: **stages 1-3 built on the AArch64 native collector and ported to the hosted x86-64
collector** (`gc-tenure`, test/gc-tenure.lisp, test/gc-major-threads.lisp); i386 / riscv / arm32
not ported.  This document came out of a measurement on the Pi Zero
board image (2026-10-29), so the board is the first target and the reasons are the board's.

## Why

`(gc-report)` and `(room)` (net/bare-perf.lisp, mvm/gc.lisp) on a running board, during a remote
desktop drag:

- region 0 has a 50 MB semispace (56 MB less the 4 MB lock arena and the 2 MB cstr/io-buf hole
  at 0x0FE00000) and **~40 MB of it survives every collection**: the restored core's data (a
  528 KB JavaScript bundle, both TTF fonts, package and symbol tables, ~570K conses);
- so every collection copies ~40 MB to free ~10 MB: ~450 ms a pause under QEMU, 11-13% of the
  board's time, and it comes back every 0.7-3 s depending on the allocation rate;
- the mutator allocates ~3-15 MB/s.  Cutting that (buffer reuse, done) stretches the interval;
  it cannot shorten the pause, which is all old data being moved for nothing.

That is the generational hypothesis measured directly: what survived one collection survives
all of them.  A copying collector should stop copying it.

## What exists today (facts the design rests on)

Citations are to this tree.

- **One Cheney collector per back end**, natively emitted.  aarch64:
  `emit-aarch64-native-gc-trampoline` (mvm/translate-aarch64.lisp).  Roots: the stack window, the
  fixed global words (0x10000080/88/148/170/FA0/FB0/FD0), MV extras, the dynamic-binding stack,
  and under stop-the-world every other thread's window plus the carved regions and the lock
  arena, which are *walked object by object and scanned, not copied* -- the one existing
  "scan but don't copy" mechanism (`emit-aarch64-stw-extra-roots`, `emit-aarch64-object-walk`).
- **A pointer is moved only if it is in `[from_start, from_start+space_size)`** (`scan_word`);
  anything else is left alone.  A candidate must also have its OBJECT-START bit set (bitmap at
  0x10000E18, 1 bit / 16 bytes from `page_base`), and the CONS-KIND bitmap (0x10000E40) must
  agree with its tag.  After a collection both bitmaps are cleared over the old from-space only.
- **A restored core is contiguous at the start of region 0's first semispace**:
  `%restore-image` puts the saved `[from, free)` back at `from` (lib/save-image.lisp), which
  `%gc-force-to-space-0` arranged before the save.
- **There is no write barrier.**  `:write-barrier` (#x8A) exists as an opcode; the compiler
  emits it after `setcar`/`setcdr` only, and every translator compiles it to nothing.  The heap
  stores a barrier would have to cover are `setcar`, `setcdr`, `obj-set`, `aset`, `bulk-move`,
  `bulk-fill` (and `:store` / `atomic-xchg` where they write a Lisp pointer into the heap): about
  six emission sites per translator.  The **shared-store guard** already intercepts exactly those
  sites on aarch64 and x64 and is the template.
- Other collectors that would follow: x64 `emit-gc-trampoline`, i386, riscv, arm32, and the Lisp
  `%gc-collect` (aarch64 shim path).  `lib/save-image.lisp` assumes `[from, free)` is the whole
  heap.
- Earlier plans (PLAN.md "Stage 2: Generational", docs/64-bit-allocator-design.md) sketch a
  nursery + old generation + card table at one byte per 512 bytes.  Nothing was built.

## The stages

Each stage is useful on its own and is measured before the next starts.

### Stage 1 — an old space that is scanned, never copied (no barrier)

**Tenure** turns a contiguous run of live data into the OLD space.  At boot, right after the
core is restored and initialised, region 0's `[from_start, free)` is exactly that run.

- The old space is `[old_lo, old_hi)`.  Region 0's two semispaces are re-carved from what is
  left of the 112 MB heap, so they are equal and lie wholly outside it.  Today: 40 MB old leaves
  2 x ~33 MB young, against 10 MB free now.
- `scan_word` needs no change: an old pointer is outside from-space and is already left alone.
  Old objects keep their bitmap bits.  The post-collection clear covers only from-space, and the
  old space is never from-space.
- **Correctness without a barrier:** every minor collection walks the WHOLE old space as roots
  (`emit-aarch64-object-walk` over `[old_lo, old_hi)`, the walk STW already does for carved
  regions).  An old object that points at a young one is found because everything old is looked
  at.
- **Cost:** reading 40 MB instead of copying it.  The expected pause is a fraction of today's,
  and collections come ~3x less often.  Both are measured on the board with `(gc-report)` before
  and after.
- **No major collection yet.**  Old garbage is not reclaimed, which is right for a boot image
  (SBCL's pseudo-static space is the same idea) and is why tenure is EXPLICIT: `(gc-tenure)` once
  after boot, not automatic promotion.
- **save-image** must refuse, LOUDLY, when an old space exists, until stage 3 teaches it the
  two-range layout.  A silent wrong core is the failure mode to rule out.
- **Words:** the old bounds need a home in the low block that every collector can read.  Check
  `origin/main` before claiming one (they have collided before).
- **First on aarch64**: the board, and the hosted aarch64 CLI on the Pi 5 as the fast test bed,
  which shares the native collector.  Other back ends keep working because they never tenure.

**Acceptance:**
- A hosted test builds a large live set, tenures it, then churns through many collections with
  old→young pointers created after tenure (a `setf` into an old vector, a `push` onto an old
  list).  It walks every structure intact, and `%gc-count-foreign-refs` shows nothing dangling.
- The **negative control** in the same binary skips the old-space walk and must fail.
- On the board, a drag run reports `(gc-report)` pause % before and after.

**Built (2026-10-29).**  `(gc-tenure)` (mvm/gc.lisp) posts a request and forces a collection;
`emit-aarch64-tenure` takes it at the swap: the survivors `[to_start, free)` become the old space
and region 0 continues on two halves of the semispace it just reclaimed, size field included.
Every later region-0 collection runs `emit-aarch64-old-space-walk` before the Cheney walk.
Words: `#x10000CC0/CC8` old bounds, `#x10000CE0/CE8/CF0` the request (free on HEAD and
origin/main).  `%gc-old-audit` walks the old space typed and counts pointer slots into the
vacated semispace.  A conservative word count is NOT the oracle: it also sees raw leaf payload.
On the Pi 5 hosted CLI: 8 MB tenured, 125,111 objects parse end to end, 80 forced collections
with old->young stores into a vector, a list tail, a growing hash table and globals, every
structure intact, 0 dangling slots, and one collection copies 0.75 MB, not 8 MB.  The negative
control (`%gc-old-space-forget`, same binary) fails every run: one memory fault, three hangs.
The first cut forgot the region's SIZE field.  The next collection's from_end then reached over
the old space and copied it, which the audit showed at once.

**On the board (card #8, QEMU raspi3b, one browser drag of 25 s each, `gc-tenure` called live
from the REPL):** 42.7 MB tenured (581,022 objects, parsed end to end, 0 dangling before and
after).  Region 0 went from 50 MB to 2 x 25 MB young.

| | collections in the drag | mean pause | paused | copied per collection |
|---|---|---|---|---|
| before tenure | 5 | 242 ms | 4.9% | 40.8 MB |
| after tenure | 2 | 129 ms | 1.0% | 3.7 MB |

The frame rate did not move (~2.7 updates/s both times), so GC was not what limits a drag; the
event loop's ~0.75 s turn is.  The remaining ~125 ms pause is the walk of 42 MB of old space,
not copying.  That is what stage 2's card table removes.

### Stage 2 — a card table, so a minor collection scans only what changed

- One byte per 512 bytes of old space (the existing design doc's figure; 40 MB = 80 KB of
  table).
- The barrier goes after each heap store listed above.  If the stored-into object is in
  `[old_lo, old_hi)`, it marks its card.  That is a compare, a shift and a byte store; the
  shared-store guard's sites are the hook points.  The runtime JIT uses the same translator, so
  JIT code gets the barrier for free.
- A minor collection scans dirty cards only, then cleans them.  The full-walk path of stage 1
  stays in the tree behind a flag, as the reference to bisect against: the
  `%HA-SET-COLLECT-SERIALIZED` pattern.
- **Raw stores:** `(setf mem-ref)`, `%gc-write64` and `%core-copy-words` write words the barrier
  cannot see.  Each one that can put a Lisp pointer into the old space either calls the barrier
  explicitly or is documented as never doing so.  An audit (`%gc-count-foreign-refs`-style:
  old→young pointers on CLEAN cards must be zero) is the acceptance test, with a positive
  control.

**Built (2026-10-29).**  The table is one `(unsigned-byte 8)` vector made by `gc-tenure` just
before the tenure collection, so it is tenured with everything else.  It holds a card byte per
512 bytes of the old space's semispace (0 = dirty; zeroed means all dirty, so the first card
collection is a full walk), then a u32 CROSSING entry per card: the object covering the card's
first byte, which is where a card scan starts.  Four raw words at `#x10000CE0`: cards (0 = off),
area, ncards, crossing.  The tenure request moved to `#x10000C88/C90/C98`.

- **Barrier:** every `setcar`/`setcdr`/`obj-set`/`aset`, and word `bulk-move`/`bulk-fill` (range
  form), ends in `ADR x9; B entry`.  There is one image-wide subroutine with a per-register entry,
  so a site costs 2 instructions, and the JIT gets its own per unit.  `obj-set` with offset 0..255
  marks the one or two cards the slot can fall in.  Image cost: +4.0 MB on the 50 MB aarch64 CLI
  (8%).  A per-function copy of the subroutine had been +1.8 MB, and 3-instruction sites +1.8 MB more.
- **Collector:** `emit-aarch64-old-card-scan` skips clean cards 8 at a time.  For a dirty card it
  starts at its crossing object and scans only the pointer slots inside the card.  The card is
  cleaned unless a slot now points into to-space.  It uses x0-x4/x8, which the collector saves
  and scan_word/copy_object never touch.
- **Oracle:** `%gc-card-audit`: every old pointer slot that points into young must sit on a dirty
  card, at any moment.  Its positive control is a raw `(setf mem-ref)` of a young cons into an
  old car, which the audit catches (1), then the same store through `setf car` (0).
- **Pi 5 hosted CLI, test/gc-tenure.lisp 11/11:** a minor collection takes 1.20 ms with cards
  against 3.58 ms walking an 8 MB old space.  Controls in the same binary: `:barrier` (barrier off,
  card scan on) fails 3/3 and `:walk` fails 3/3.
- Raw stores (`(setf mem-ref)`, `%gc-write64`) are not covered.  The audit is how to find one
  that matters.

### Stage 3 — promotion and a major collection

- Objects surviving N minor collections are copied into the old space (bump at `old_hi`), not
  back into young.  This needs an age (a survivor count per young semispace, or a two-step
  nursery).
- A major collection reclaims old garbage: mark-compact the old space in place, or copy old into
  a fresh old area when memory allows.  It is rare, and its pause is allowed to be long.
- save-image learns the two-range layout, so a core saved after tenure restores with its old
  space intact.

**Built (2026-10-29), aarch64.**  Promotion is by SURVIVOR MARK rather than a per-object age:
at the end of every region-0 collection the to-space frontier is recorded, and the next
collection promotes any from-space object below that mark (it has now survived twice) straight
into the old area instead of copying it back into young.  One word of state, no header bits.

- **Card vector meta.**  The card vector now starts with 128 bytes of raw words: capacity, area
  size, survivor mark, major state, promotion failures, majors, bytes promoted, promote-on, and
  reserve (0 = 4 MB).  Cards start at meta+128 and the crossing table at cards+capacity.  The
  vector's tagged pointer lives at `#x10000D88`, a fixed root every collector scans.
- **Registers for the whole collection:** x3 = survivor mark, x8 = old bump pointer, x2 = area
  end.  The card scan's own state moved into a 64-byte frame to free them.  `copy_object` was
  rewritten to compute the size FIRST, then choose the destination (old area or to-space), then
  run one 16-byte copy loop.
- **Cheney with two cursors** (`emit-aarch64-cheney`): x0 over to-space and x1 over the newly
  promoted part of the old area, until both are caught up.  Promoted objects are scanned like any
  copied object, and their crossing entries are filled as they land (`emit-aarch64-crossing-fill`).
- **Promotion failure** (old area full) falls back to copying into young for the rest of the
  collection and counts a failure; nothing is lost.
- **The major collection** is a state machine in the card meta, run across two ordinary
  collections so it reuses the copying collector instead of a second algorithm:
  - phase 1 (state 7, or 10 for untenure) promotes EVERYTHING young into the old area;
  - phase 2 (state 2 / 5) copies the whole old area into the young semispace, which collects old
    garbage, and then re-tenures it (2) or leaves it young (5 = untenure).  Phase 2 does not start
    unless old bytes <= 2 x half, otherwise state returns to 0 and a failure is counted.
  - a major is requested automatically when room < max(reserve, 2 x survivors).
  Lisp: `gc-major`, `gc-untenure`, `gc-gen-reserve`, `gc-gen-report`.
- **save-image** calls `gc-untenure` first, so a saved core has the plain single-range layout
  and restores on any image.  Measured: a core saved after tenure + a major restores with its
  data intact.
- **Two bugs found on the way.**  The cons check in the new `copy_object` used `tag & 7`, and an
  object pointer's tag 9 & 7 = 1 = cons: it compares `tag & 15` now.  The promotion setup
  clobbered x17, the region base the root-window code reads after it; it is restored, also on
  the phase-2 re-entry.
- **Pi 5 hosted CLI:** test/gc-tenure.lisp 19/19 (promotion, an explicit major 20.5 -> 17.4 MB in
  15 ms, automatic majors under a huge reserve, untenure).  GC regression tests at baseline.
- **The 22-library ladder (Pi 5), five arms, identical per library** (104 probes ok; the same 8
  failures as base, all from before this work): base, tenured, tenured with a huge reserve,
  `(gc-tenure :young 32MB)`, and the same plus a huge reserve.  **The first two tenured arms proved
  less than they look:** the hosted young space is hundreds of MB, so a library load collects
  about 4 times and promoted 0 bytes.  `:young` is the stress knob that makes it real: alexandria
  then promotes 5.6 MB, and with the reserve it runs a major.  Both audits read 0 afterwards.

### On the board: tenure at boot (kiln boot/zero.lisp, cards #9-#11)

`kz-gen-gc-start` tenures once the threads are up (`GC=plain` on the card's command line skips
it).  Serial says `KZ:gc generational, old 32142 KB, young 2 x 25600 KB`.

| QEMU test board, per 25 s drag | collections | paused | share |
|---|---|---|---|
| plain collector (card #8 before tenure) | 5 | 242 ms | 4.9% |
| stage 1, tenure only (card #8) | 2 | 129 ms | 1.0% |
| **stages 1-3 from boot (card #10)** | **1-2** | **5-12 ms** | **0.0%** |

Card #12 (all fixes below): 9 majors in one session (4 live, 5 automatic, about one per
session start), ~255 ms each, 0 failures.  A keyword held across each live major stayed EQ, and
every audit read 0.  The old space settles at ~44 MB of its 50 MB area.  They are now the longest pause (~285 ms, about one old plain collection), and they
are rare.  The drag frame rate did not move (2.1-3.0 updates/s), because the event loop sets
it, not GC.  The typed audits read 0 after drags and after majors.

**Cards #9 and #11 found three real bugs and one false alarm:**

- **The old space ran into the I/O staging pages.**  The board keeps `*CSTR-SCRATCH*`
  (`0x0FE00000`) and `*IO-BUF-ADDR*` (`0x0FF00000`) at the top of the upper semispace, where the
  mutator's 8 MB limit guard always kept allocation away.  Promotion fills the old area up to a
  bound instead, and `old_hi` reached `0x0FE6B260`.  Fix: a CEILING word (meta +72, the lowest
  staging page inside region 0's span, 0 hosted).  It stops promotion, a major's phase-2
  destination and `%gc-old-room`.  The test plants one: `old_hi` stops exactly there, and the
  control promotes past it.
- **The lock arena was carved after tenure.**  Starting the threads shrinks region 0, which left
  a carved gap inside the young pair, and a major's phase 2 copies straight across the pair.  It
  happened to be refused for size there.  Now `%gc-region-shrink` of region 0 errors once an old
  space exists, and the board tenures after `kz-usb-start`.
- **(card #11) A major with live threads lost data.**  After a live `gc-major`, the keyword
  `:US` in the calling form came back as a closure, and the board faulted at NIL-9.  A threaded
  STW collection also EVACUATES THE LOCK ARENA.  Phase 1 promoted only what lay below from_end,
  and the arena (at the top of a semispace) can lie above it, so its objects stayed young.
  Phase 2 then copied the old area over the whole young semispace.  The hosted tests had no
  threads, so they could never see it.  `test/gc-major-threads.lisp` (2 interning threads, 40
  majors) lost data 3 of 3 times, with clean controls (minors with threads, majors without, 0 of
  6).  Phase 1's mark is now all ones, and the test runs 5 of 5 clean.
- **"Dangling" slots (36-139 per audit) were the AUDIT's own fault.**  Its `%gc-old-leaf-p` was
  `(member st '(...))`, which conses the list on every call: about 48 bytes per object, so every
  walk triggered a collection halfway through and then judged the rest of the heap against
  pre-flip bounds.  The audits now allocate nothing and re-run until no collection overlaps.

### Stage 4 — the rest of the tree

- Carved thread regions and the lock arena.  Those are scanned, not copied, under STW today, so
  they already behave like old spaces.
- The other back ends (x64 first, hosted, where the thread tests live), then i386 / riscv /
  arm32.
- `CLAUDE.md`'s GC sections ("there is no write barrier") are updated as each lands.

**x64 built (2026-10-29), the hosted `./modus`.**  Same words, meta layout and state machine
as AArch64, so `mvm/gc.lisp` is shared unchanged; only `translate-x64.lisp` ("GENERATIONAL GC")
is new.  `*x64-gen-gc*` turns it on (build-generic-cli, and the JIT co-init in
build-cli-common, which must agree); `MODUS_X64_GEN=0` builds the control.

- **Barrier:** each unit (the image, each JIT page) gets a `card_mark` subroutine (RAX = slot,
  preserves every register) and a range form for word `bulk-move`/`bulk-fill`.  A site is
  `push rax; lea rax,[slot]; call; pop rax`, before the store.  Cost: +7.4 MB native (+14%);
  1.8x on a loop that does nothing but store, cards on or off (the call is the cost, not the
  mark); +1-6% user CPU loading cl-ppcre / babel / md5.  Inlining the "cards off" test at the
  site would make an untenured image nearly free, at ~11 bytes a site.
- **Collector:** R12 = the old area's bump, R14 = the survivor mark (all ones in a major's
  phase 1), meta+80 = the promotion bound (area end, clipped by the ceiling).  `copy_object`
  sizes first, then picks R12 or R13.  The old space is a root through a typed dirty-card scan
  (crossing table, cons-kind bitmap), or a flat walk without cards.  Cheney runs two cursors
  (to-space to R13, promotions to R12; the second at [rsp]).  The bitmap clears move BEFORE the
  swap, then promo-finish, tenure and the major step, which set the to_start/RBX/R13 the swap
  installs or jump back to the root scan for phase 2.  The card vector's root `#x10000D88` is
  a global root.
- **Results (hosted x64):** test/gc-tenure.lisp 21/21, a dirty-card minor 0.26-0.28 ms against
  5.4 ms walking the whole old space; `:walk` control SIGSEGV, `:barrier` control aborts.
  test/gc-major-threads.lisp 5/5 (40 majors each, two interning threads).  18 GC/thread
  regression tests: same results as the control build.  22-library ladder: control, untenured,
  `:young` 32 MB (359 collections) and `:young` + a huge reserve all 20/22 clean, 104 probes,
  the same 8 failures.  The control build's code is base's modulo relocation: the one function
  that changed size is the translate-state constructor (two slots).
- **Not done:** tenure with `:cards nil` cannot be untenured (gc-untenure is a major, which
  needs the card meta), on both back ends.

## Risks named up front

- **Conservative roots** stay conservative.  A false root into the old space pins nothing and
  costs nothing, because the old space never moves.  A false root into young behaves exactly as
  today.
- **The walk relies on the old space parsing as objects** (headers and conses, distinguished by
  the CONS-KIND bitmap), exactly as STW's region walk does.  Any raw, non-object data inside
  `[from, free)` at tenure time would break it.  Stage 1's first test is a full parse of a
  tenured board heap.
- **A layout change is a board change.**  Re-carving region 0 moves `from_start`, which the core
  restore checks (`%core-die "heap base differs"`).  Tenure happens AFTER restore, so the saved
  geometry is unchanged.  This ordering is load-bearing.
