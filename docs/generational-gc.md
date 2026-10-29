# Generational GC — design and staging

Status: **stage 1 built on the AArch64 native collector** (`gc-tenure`, test/gc-tenure.lisp); stages 2-4 are design.  This document came out of a measurement on the Pi Zero
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

### Stage 3 — promotion and a major collection

- Objects surviving N minor collections are copied into the old space (bump at `old_hi`), not
  back into young.  This needs an age (a survivor count per young semispace, or a two-step
  nursery).
- A major collection reclaims old garbage: mark-compact the old space in place, or copy old into
  a fresh old area when memory allows.  It is rare, and its pause is allowed to be long.
- save-image learns the two-range layout, so a core saved after tenure restores with its old
  space intact.

### Stage 4 — the rest of the tree

- Carved thread regions and the lock arena.  Those are scanned, not copied, under STW today, so
  they already behave like old spaces.
- The other back ends (x64 first, hosted, where the thread tests live), then i386 / riscv /
  arm32.
- `CLAUDE.md`'s GC sections ("there is no write barrier") are updated as each lands.

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
