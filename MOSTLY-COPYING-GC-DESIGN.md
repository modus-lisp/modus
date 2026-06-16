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
