# Mostly-Copying (Bartlett) GC — design blueprint

GOAL: replace the contiguous two-space Cheney copier with a page-based
mostly-copying collector so that AMBIGUOUS (conservative stack) roots PIN
their object's page (object stays in place, never forwarded), while PRECISE
references (globals, object fields) copy normally. Eliminates the
false-positive-forwards-garbage corruption class entirely.

SUCCESS CRITERION (binary): ANSI pass count back to baseline (~16.5k via the
reliable per-file method) AND the ASDF gauntlet corruption-free and robust to
perturbation. Until both, not done.

## Heap layout (Linux x64 target = where ANSI/gauntlet run)
Today: mmap 896MB+16MB guard at heap_base; semispaces [0x200,MID) and
[MID,2*MID), MID=0x1C000000; R12=alloc, R14=MID, metadata @abs 0x10000040.

New regions (carve from an EXTENDED mmap; keep the heap data region the same
size so object addresses don't shift more than necessary):
- PAGE_SIZE = 4 KiB (0x10000). Heap data region = the existing 896MB → 14336 pages.
- PAGE DESCRIPTOR ARRAY: 1 byte/page (state: 0=free, 1=live, 2=live+pinned-this-GC).
  14336 bytes. Lives at a fixed offset in an extra mmap'd metadata region.
- FREE LIST: a stack of free page indices (u32 each), + a free-count. ~57KB.
- OBJECT-START BITMAP: 1 bit / 16-byte granule over the 896MB data region =
  896MB/16/8 = 7 MiB. Fixed offset in the metadata region.
- New metadata words @ 0x10000060+: page_base, page_count, freelist_ptr,
  freelist_count, bitmap_base, descriptor_base, alloc_page_idx.

## Allocator (preserve the bump fast-path)
- R12 bumps within the CURRENT page; R14 = current page END.
- alloc sites UNCHANGED except: after writing the header/cons, SET the
  object-start bit for the R12 granule (a few insns; or fold into a shared
  alloc prologue).
- gc-check (R12>=R14) slow path becomes "refill": pop a free page →
  R12=page_start, R14=page_end, mark descriptor live; if free-list empty → GC,
  then refill. (This is the ONLY alloc-path control-flow change.)

## Collector (page-based mostly-copying)
Phase 0: reset all live descriptors to free-candidate; clear pinned marks;
  reset the to-side bookkeeping.
Phase 1 — PIN: scan stack conservatively. For each ambiguous word that (a) lands
  in the heap data region and (b) hits a MARKED OBJECT START in the bitmap:
  mark that object's PAGE pinned (descriptor=2), and enqueue the object as a
  gray root. Do NOT forward it, do NOT rewrite the stack word.
Phase 2 — COPY/SCAN (Cheney-style worklist):
  - Precise roots (globals, symtab, keyword table, MV extras) are forwarded
    normally into fresh COPY pages (popped from the free list, marked live).
  - Scan gray objects (copied objects + pinned objects). For each reference
    field pointing into the heap data region: if the target's PAGE is pinned →
    keep the address (no forward); else forward (copy into a copy page) and
    update the field.
  - copy_object now ALWAYS validates via the bitmap before treating a word as a
    pointer (kills the residual). Object-start bit is SET for each copied object
    in its new page; pinned objects keep their existing bit.
Phase 3 — RECLAIM: any page still marked free-candidate (not live, not pinned,
  not a copy page) → push back to the free list; clear its bitmap bits.
  Pinned + copy pages are the new live set. No semispace flip — it's a page pool.

## Build order (each builds; only the whole thing passes the gate)
1. Metadata + extended mmap + descriptor/freelist/bitmap init (boot-linux-x64,
   boot-x64). No behavior change yet (GC still old). Gate: ANSI identical.
2. Object-start bitmap set at every alloc site. Still write-only. Gate: identical.
3. Page-refill allocator slow path (free-list); GC still old contiguous Cheney
   but over the page pool (treat live pages as the from-space set). Gate: identical.
4. Mostly-copying collector (phases above). Gate: GREEN + gauntlet robust.

## Notes / hazards
- Bare-metal x64 (boot-x64) needs the same metadata; lower priority (ANSI/gauntlet
  are Linux). Keep it building; full bare-metal validation later.
- The bitmap must be maintained for COPY pages during GC (set bit on copy).
- Pinned objects can themselves reference copied objects and vice-versa — the
  worklist handles both; the pinned-page check on each field is the crux.
- Large objects spanning >1 page: a pinned large object pins ALL its pages.
  alloc-array of a huge array must mark every page it covers.

---

# Stages 3-4 detailed design (page-pinning, the durable FFI/IO + Bartlett goal)

STATUS: stages 1 (metadata) + 2 (bitmap) banked.  Stage "2.5" = the
conservative-root VALIDATION collector (ace1544 + point-c 810a975): the
contiguous Cheney copier hardened by the object-start bitmap gate.  That
ELIMINATED the corruption class and is the default-on production collector.
Stages 3-4 below add true PAGE PINNING (objects whose page is pinned never
move) so FFI/IO can hand a stable heap address to DMA / foreign code, and so
ambiguous conservative roots PIN (strictly safer than the current rejection).
All of stage 3-4 lives behind `*mcgc-pinning-enabled*` (default NIL); canonical
stays on the validation collector until pinning passes the gate.

## Why a contiguous copier cannot pin (the forcing function)
Cheney evacuates the ENTIRE from-space; it cannot leave one object behind.
Pinning therefore requires PAGE granularity: a page can be excluded from
evacuation.  This is Bartlett "mostly-copying": per-page SPACE ids, not a
from/to address split.

## Page model
- Heap data region stays ONE contiguous mmap (so addresses don't shift).
- 4 KiB logical page grid; descriptor byte per page:
    bit0-1 space: 0=free, else generation parity (current vs prior alloc space)
    bit2   pinned-this-GC (transient; recomputed every GC from conservative roots)
    (persistent FFI pins live in a SEPARATE per-page u16 pin-count array, NOT
     the descriptor, because they must survive the per-GC descriptor reset.)
- alloc_space_parity flips each GC (1↔2).  "Live in new space" = descriptor
  space == current alloc_space_parity OR pinned OR pin-count>0.

## Allocator — REFINED: per-run GUARD shrinks the per-site change (PREFERRED)
The naive "size-aware pre-check at EVERY alloc site" is invasive and bug-prone.
Better factoring, exploiting that the current post-write gc-check + a guard
already works:
- Refill sets R14 = run_end - GUARD (GUARD = 64 KiB = 16 pages).  Any object
  whose size <= GUARD keeps the CURRENT "write, advance R12, post-write
  gc-check (R12>=R14 → refill)" protocol UNCHANGED — an overshoot of <= GUARD
  lands in the last 16 pages of the SAME run (still free pages of this run,
  never the next pinned/foreign page).  cons, float, and small constant-size
  :alloc-obj (the vast majority) need ZERO per-site change.
- Only objects that can EXCEED GUARD need an explicit pre-check + a contiguous
  run: array / string (runtime count) and any :alloc-obj with constant size >
  GUARD (rare).  Those compute size → ensure R12+size <= real run_end → else
  refill_big (find/split a run >= size contiguous bytes; GC+retry; else OOM).
- Net: the pre-check touches only array/string (+ a guarded constant check on
  big alloc-obj).  The hot path stays byte-for-byte the legacy fast path; only
  R14's value changes (guarded).  GUARD must be >= the largest no-check object;
  enforce by routing > GUARD through the big path.
This is the implementation plan for stage 4's allocator.

## Allocator — SIZE-AWARE pre-check (original framing; superseded by GUARD above)
The contiguous bump does "write object, THEN gc-check" — safe only because a
16 MiB guard absorbs overshoot.  With pages, overshoot would scribble into the
next page (possibly PINNED or another generation) → corruption.  So flip to
"ensure-room(size) BEFORE write":
- R12 = bump ptr; R14 = end of current free RUN (= first byte of the next
  non-current-alloc-space page, or heap end).  A run is >=1 consecutive pages
  all in the alloc space, no pin interruptions.
- Each alloc site emits, before writing: `LEA tmp,[R12+size]; CMP tmp,R14;
  JA refill_size`.  size is a compile-time constant for cons/obj/float (fold
  the LEA disp) and a runtime value for array/string (ADD tmp, computed_size).
- `refill(size)`: pop a free RUN of >= size bytes from the run-free-list (see
  below); set R12=run_start, R14=run_end; mark the run's pages alloc-space.
  If no run fits → GC; retry; if still none → true OOM.
- This is the ONLY control-flow change to the fast path; the common case adds
  one LEA+CMP+JA (not-taken) per alloc.  gc-check opcode keeps working for
  back-compat but the pinning allocator drives off the per-site pre-check.

## Free list = list of RUNS, not single pages
A single-page free-list can't serve a >4 KiB object without a contiguous-run
scan.  Keep a free-list of (start_page, n_pages) RUNS.  At boot: one run
covering the whole data region.  Refill splits a run (take head, push back the
remainder).  GC REBUILDS the run-free-list from the descriptor array in one
linear pass (coalescing consecutive free pages) — O(page_count), once per GC,
cheap vs the copy work.

## Collector (phases) — driven only when *mcgc-pinning-enabled*
P0  flip alloc_space_parity; do NOT touch descriptors yet.
P1  PIN: conservative stack scan.  For each word landing in the data region,
    mark its page's transient-pinned bit AND promote that page's descriptor to
    the NEW alloc space (so it's retained in place).  No bitmap lookup needed
    to pin — pinning a page on a garbage word only over-retains one page for
    one GC (safe).  Also: every page with pin-count>0 (persistent FFI) is
    promoted + treated as a gray root source.
P2  COPY/SCAN worklist (Cheney-style over a scan queue of gray objects):
    - precise roots (globals, symtab, keyword table, MV extras) forward into
      fresh COPY pages (popped from the run-free-list, marked new-space).
    - scan each gray object's fields: if target page is pinned/persistent →
      keep address (no forward); else forward (copy to a copy page) + update.
    - pinned objects are gray roots scanned IN PLACE.  Set object-start bit for
      copied survivors in their new page.
P3  RECLAIM: descriptor pass — any page NOT in the new alloc space and pin-
    count==0 → free; clear its bitmap bits.  Rebuild run-free-list (coalesce).
    No semispace flip.  R12/R14 = refill from the rebuilt free-list.

## Explicit pin API (FFI/IO)
- `%pin-object(obj)`: for each page the object covers, pin-count++ ; returns the
  (now stable) raw address.  `%unpin-object(obj)`: pin-count-- per page.
- `with-pinned-objects` macro = unwind-protected pin/unpin.  Pins survive GCs
  (pin-count array isn't reset).  GC P1/P2 honor pin-count>0.
- Large object: pin/unpin every page in [obj_start, obj_start+size).

## Build/verify order (each behind the flag; gate = SUB-SHARDED ANSI parity +
## gauntlet determinism; stage 4 also a PIN STRESS test)
3a. Run-free-list + boot init (whole-region run).  No allocator change yet
    (validation collector still runs).  Gate: ANSI identical (dead code).
3b. Size-aware pre-check allocator + refill from run-free-list, BUT collector
    still the validation Cheney over the contiguous region treated as one run
    (no pinning yet).  This is the landmine; verify to PARITY before P-pinning.
4.  Page-based mostly-copying collector (phases P0-P3) + explicit pin API.
    Gate: GREEN ANSI + gauntlet robust + a pin-stress probe (allocate, pin,
    force N GCs, assert address stable + contents intact + unpin reclaims).
