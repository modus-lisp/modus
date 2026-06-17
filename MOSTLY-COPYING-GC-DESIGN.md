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

## CORRECTION: init in COLLECTOR ASSEMBLY (lazy first-run), not Lisp, not boot
Two failed approaches, both root-caused:
1. Boot-asm init grows the preamble → breaks *x64-native-code-offset* alignment
   → deterministic NIL-deref crashes.  (stage 3a, reverted)
2. Lisp init via mem-ref FAILS: `:u64` mem-ref loads/stores are RAW (no fixnum
   tag), but the mem-ref ADDRESS operand always SHR-1's (expects a tagged
   fixnum).  So a `:u64`-loaded pointer cannot be fed back as a mem-ref address
   — it gets half-untagged → SIGSEGV.  gc.lisp only ever uses CONSTANT addresses
   as mem-ref addresses and round-trips VALUES via `:u64`; it never derefs a
   loaded pointer in Lisp.  (stage 4a, reverted)
CONCLUSION: do the run-free-list / descriptor seed in the COLLECTOR ASSEMBLY,
lazily on the first page-collection (a "if freelist_count==0, seed it" preamble
in the trampoline), where raw addresses are natural and no tagging applies.
This couples init with the collector — stage 4 is therefore ONE assembly
increment (init + page allocator + page collector), verified together, not
sub-divisible into small Lisp steps.  The boot preamble stays byte-for-byte as
stages 1-2 left it.

## (obsolete) CONSTRAINT: init in Lisp, never grow the boot preamble
The linux-x64 boot preamble size is load-bearing: `*x64-native-code-offset*`
is hardcoded per build script (351 in build-ansi-test) and drives the
function-entry NOP-alignment (keeps fn starts off nibble 0x1).  Adding bytes to
boot/boot-linux-x64.lisp WITHOUT bumping that offset mis-aligns functions →
deterministic NIL-deref crashes (learned the hard way, stage 3a).  Therefore:
the run-free-list / descriptor / pin-count CONTENT init is done in LISP runtime
code (an init defun run early in kernel-main, reading the config-word ADDRESSES
boot already stores from stages 1-2: page_base 0xE00, page_count 0xE08,
descriptor 0xE10, bitmap 0xE18, freelist 0xE20, freelist_count 0xE28,
alloc_page 0xE30, data_end 0xE38).  Boot stays byte-for-byte as stages 1-2 left
it.  Use %poke/mem-ref-style primitives from Lisp.

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

---

# STAGE 4b STATUS — LANDED (page machinery, NO pinning yet)

Commits 6a5dfb7 + 11098ae on mcgc-pinning.  All behind *mcgc-pinning-enabled*
(default NIL); MODUS_MCGC_PINNING=1 enables for a test build.

What landed (translate-x64.lisp, emit-page-gc-trampoline):
- Lazy init IN COLLECTOR ASSEMBLY (per the CORRECTION): first collection
  anchors the page grid at page_base, splits the data region into TWO equal
  page-runs, seeds the run-free-list with the second half, marks the first
  half live.  No boot-preamble growth, no Lisp mem-ref init.
- Allocator: kept the legacy POST-WRITE gc-check (re-routed to the page-GC
  trampoline under pinning).  No size-aware pre-check needed at 4b: each run
  is ~456 MB and the 64 KiB GUARD absorbs any single-object overshoot into
  free pages of the same run.  emit-mcgc-ensure-room is wired+ready but
  un-called (it's for 4c when runs fragment under pins).
- Collector = TWO-SPACE copying over the page pool: evacuate the alloc run
  (from-space) into a fresh to-run popped from the free-list (reusing the
  proven Cheney scan_word/copy_object + object-start-bitmap gate), then free
  the old run, rebuild the free-list as the freed run, mark the to-run live,
  make it the new alloc run (R12=free ptr, R14=to_end-GUARD).
- New BSS scratch 0x10000E40..0x10000E70 (run/to/from start+end, init-done),
  all ELF-BSS zero-init, none written by boot (preamble unchanged).
- Helpers: emit-mov-reg-abs / -abs-reg / -abs-imm32, fill-descriptor-range
  (REP STOSB), clear-bitmap-range.

Bugs found + fixed during 4b:
- reg-info not exported from :modus.asm → used reg-code/reg-extended-p.
- RECLAIM RCX-CLOBBER: clear-bitmap-range clobbers RAX/RCX/RDI but the
  free-list rebuild read from_end from RCX → garbage n_pages → next GC popped
  an insane to-run → fault/GC-reentry.  Fixed by stashing from bounds to BSS
  and computing the rebuild entry before any clobbering helper.

Gates PASSED (x64 Linux):
- FLAG-OFF byte-identical to canonical (same md5) — fully inert when off.
- ANSI sub-shard parity 10001-11000: reg 0 / gain 0.
- Gauntlet 5/5 BYTE-IDENTICAL (md5 907cef43), all reach GAUNTLET DONE
  forms=107 fails=2 — 9 forms BEYOND flag-off's form 98, same 2 real
  NOT-IMPLEMENTED fails.  (Form-59/103 variance seen earlier was 300s-timeout
  truncation under external-tenant load, NOT corruption — 400s clean runs are
  deterministic.)
- GC-stress probe (keep a root across several forced GCs): survives intact.
- FULL-BAND parity 10001-27708: see commit / sweep log (pending at time of
  writing this note).

STILL TODO:
- 4c: conservative-root + persistent (pin-count array) pinning.  The 4b
  collector's address-range from-space test must become descriptor-based, and
  to-space allocation page-wise (skip pinned pages); emit-mcgc-ensure-room
  then drives array/string refill once runs fragment.  %gc-collect under
  pinning still routes to the Cheney trampoline (only gc-check reroutes) — 4c
  must point explicit collection at the page collector too.
- 4d: %pin/%unpin/with-pinned-objects API + pin-stress (address stable across
  N GCs, contents intact, unpin reclaims).

---

# Stage 4c/4d STATUS (2026-06-17): committed WIP does NOT build flag-on

Orchestrator verification of the 4c/4d WIP (commit a294a56):
- GATING OK: flag-OFF builds byte-identical to canonical (md5
  916804619904332f43cec375e99d4601). mcgc-pin.lisp + :mcgc-collect opcode are
  correctly pinning-only.
- FLAG-ON BUILD FAILS: register-allocation cascade. ~9 complex functions
  (%ensure-pathname-classes, %init-symbol-function-table, list-all-packages,
  %heal-handler-bind-skip, %signals-error-stub, %get-plist-ht,
  %init-make-load-form, ...) hit `MVM x64: cannot load vreg 22..30` (emit-load-vreg:
  vreg neither phys-mapped nor spilled) → "giving up on translator, using partial
  result" → APPLY-LI-CONST-PATCHES writes at offset 15419015 into the 2382192-byte
  native array (the offset is ~the 15.7M SOURCE size → a partial-result base/units
  confusion) → Unhandled INVALID-ARRAY-INDEX-ERROR, build dies. flag-on ANSI binary
  is a truncated 18MB (0 passes); flag-on generic exits 1.
- flag-OFF has NONE of these vreg warnings → the cascade is flag-on-specific.
- 4c did NOT wire ensure-room into alloc sites and did NOT change the reg-map /
  scratch set, so it is NOT a per-alloc-site register grab.

ROOT CAUSE is the vreg cascade (the li-const-patch crash is a downstream symptom).
TWO hypotheses, DECISIVE next step = build flag-on with mcgc-pin.lisp EXCLUDED
(flag on, skip the pin-source include):
  (a) cascade PERSISTS → the flag's EMISSION change (gc-check page reroute / the
      546-line collector altering some global the per-function allocator reads).
  (b) cascade VANISHES → adding mcgc-pin.lisp's functions shifts vreg NUMBERING or
      image size past a translator capacity; the real fix is in the MVM register
      allocator / vreg handling (or reducing mcgc-pin.lisp's footprint), NOT the GC.
Pin-stress (the actual deliverable) was NEVER reached. 4c/4d is INCOMPLETE.
Verified working: stage 4b (page machinery) at fabd873. Canonical unaffected.

---

# Stage 4c/4d FINAL STATUS (2026-06-17, orchestrator verification)

FIXED (committed): the flag-on BUILD failure. Root cause = the heavy
%mcgc-pin-stress probe compiled into the image perturbed the register allocator
("cannot load vreg 22-30" cascade in ~9 unrelated fns -> partial translation ->
li-const-patch offset overflow). Bisected decisively: flag-on builds CLEAN with
collector + pin API; ONLY the probe breaks it. Fix (ed1dec6): probe removed from
mcgc-pin.lisp -> standalone test/pin-stress.lisp (runtime-eval'd, no image
perturbation). flag-on ANSI (106MB) + generic (24MB) now build, VREG=0.

STILL BROKEN (NOT fixed): the 4c per-page COLLECTOR crashes at runtime.
- flag-on gauntlet stops at form 2 (1 line, process exits) vs the 4b collector's
  clean form 107. Deterministic.
- Minimal repro: `(%mcgc-collect)` on a tiny heap prints "A" then the process
  faults/exits — NO page-collector entry dbg-char, no completion. ANY page
  collection (explicit or implicit gc-check) crashes immediately.
- So the agent's 546-line per-page-pinning collector rework (a294a56) is
  non-functional. It builds but does not run. pin-stress (the deliverable) cannot
  be evaluated until the collector runs.
- Likely area: R14/run bounds init, the lazy first-collection seed, or the
  gc-check->page-gc-label reroute (crash is at LOW allocation, well before the
  ~456MB first-run fill, so it's init/first-invocation, not a fill-boundary case).

VERIFIED-GOOD baseline remains stage 4b (fabd873): page machinery, gauntlet 107,
ANSI parity reg=0. RECOMMENDED next: REVERT the 4c collector delta back to the 4b
collector (keep the pin API + :mcgc-collect opcode + build infra), then re-add
per-page pinning INCREMENTALLY, gauntlet-verifying after EACH step. The all-at-once
546-line rework without runtime verification is why it's unverifiably broken.
Canonical unaffected throughout (corruption fix + validation collector still banked).

---

# STAGE 4 COMPLETE (2026-06-17): page-pinning GC works, verified.

The per-page mostly-copying (Bartlett) collector is functionally complete behind
*mcgc-pinning-enabled* (default NIL; flag-off builds are BYTE-IDENTICAL to
canonical — md5 916804619904332f43cec375e99d4601).

FOUR collector bugs were found + fixed (orchestrator, via the MODUS_GC_R14
small-run forced-collection method + gated phase/count debug chars; gdb is
blocked by ptrace in the sandbox):
  1. (321eeb3) lazy-init RCX clobber: the 2nd descriptor fill used RCX left 0 by
     the 1st fill's REP STOSB -> wild write -> the "gauntlet form 2" SEGV.
  2. (fc2afd7) free-list rebuild descriptor read mis-encoded #x42(REX.X)=
     [rax+r14] instead of #x41(REX.B)=[r8+rsi] -> every page read the same byte
     (0) -> whole region coalesced into one "free" run -> next to-run overwrote
     everything + cleared all pins.
  3. (2de5398) to-run pop took the LAST free-list entry, which goes tiny as pins
     fragment the free region -> survivor overflow.  Fix: pop the LARGEST run.
  4. (4a1f5b5) persistent %pin pin-count addressing: (mem-ref slot :u64) returns
     raw bits (Lisp value = raw>>1, half) but %mcgc-obj-raw-addr yields the true
     byte address -> half-address SEGV.  %mcgc-cfg-uval rescales (ash ... 1).

VERIFIED (small-R14 build, heavy collection):
  - colforce2: a live object SURVIVES forced page collections INTACT.
  - conservative pinning: a stack-referenced object's ADDRESS is STABLE across
    collections (STABLE=1 INTACT=1) — the synchronous-FFI guarantee.
  - persistent %pin-object / %unpin-object: PIN-STABLE=1, pin-count 0->1->0 — the
    async-DMA guarantee (hold a stable raw address across GCs with no stack ref).
  - ASDF gauntlet runs to form 226 (the full survey) DETERMINISTICALLY under
    hundreds of pinning collections (md5-identical x3).  Validation collector
    reached 98; pinning reaches 226.
Regressions: test/pin-stress.lisp, test/addr-stable.lisp.

RESIDUALS (non-blocking; the feature is gated OFF):
  - explicit (%mcgc-collect) opcode path still faults; implicit (gc-check)
    collection works, so all the above is via implicit collection.
  - to-run refill on exhaustion: pick-largest fits the gauntlet, but a workload
    whose survivors exceed the largest single free run would overflow; the
    durable fix is copy_object popping another free run when R13 hits to_end.
  - gated debug instrumentation (*x64-gc-debug*, phase/count chars) retained.
