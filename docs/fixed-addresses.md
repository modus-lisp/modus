# Fixed runtime addresses: the registry and CHECK H

The runtime keeps its process state in hand-allocated **literal addresses**:
argc at `#x10000200`, the hosted scheduler lock at `#x10000FC0`, the
stop-the-world flag at `#x10000FC8`, AArch64's JIT constant-vector root at
`#x10000FD0`, the handler-frame extension at window `+0x5600`, i386's at
`#x1001FF00`, the thread table's futex words at `+0xC0/+0xC8`, ...  Until this
registry nothing checked them, and "clear of X" in a docstring was a claim
nobody verified.  Three collisions bit in one campaign:

1. `SMP-INIT` zeroed `FC0/FC8/FD0` after `FC8` had become the STW flag and `FD0`
   the AArch64 constvec root (4f12b7e8): every actor bring-up nulled compiled
   QUOTE loads on AArch64.
2. The threads merge silently collided `EB8` (eval lock owner) and `F18` (a live
   per-region table cell).
3. Same shape, other class: a 29-bit name-hash collision made a gensym special
   (d2541ac).

`mvm/fixed-addresses.lisp` is the single registry, and CHECK H fails the build
when two features claim the same bytes.  It is host-only (loaded by
`lib/load-mvm.lisp` after `mvm/build-checks.lisp`) and never baked: a build with
it is byte-identical to one without it.

## Claiming a new word

1. **Look at the map first**: `MODUS_FIXED_ADDR_CHECK=report` prints every
   map's sorted table on any build (or load `lib/load-mvm.lisp` and call
   `(modus.mvm::fixed-address-report)`).  Pick a gap in *every* map your word
   lives in.
2. **Register it** in `mvm/fixed-addresses.lisp`, next to its neighbours:

   ```lisp
   (define-fixed-word :my-feature-flag #x10000F60 8
     :images (:x64-hosted :a64-hosted)          ; maps or groups, see below
     :space :global                             ; :global | :window | :block
     :owner "net/my-feature.lisp"
     :constants ("MODUS.MVM.X64::+MY-FEATURE-FLAG-ADDR+")  ; checked if bound
     :doc "What it holds, and who writes it.")
   ```

3. **Build.**  The check runs when `lib/load-mvm.lisp` loads (seconds, before
   anything compiles) and again inside `BUILD-IMAGE`.  A collision fails with
   both owners named:

   ```
   *** check-fixed-addresses: the fixed-address registry is inconsistent ***
   TWO FEATURES CLAIM THE SAME BYTES (mvm/fixed-addresses.lisp):
     - in a64-hosted (absolute absolute view):
         STW-FLAG [#x10000FC8, #x10000FD0) global absolute in WORD — mvm/translate-x64.lisp, ...
         MY-FEATURE-FLAG [#x10000FC8, #x10000FD0) global absolute in WORD — net/my-feature.lisp
   ```

4. **Do not grow the allowlist.**  If you write the literal in code without
   registering it, CHECK H4 (the ratchet) fails the build with the file and the
   address.  `mvm/fixed-address-allowlist.lisp` is a debt register that may
   only shrink.

## The model

**Maps.**  An entry is a byte range claimed in a set of *memory maps* — one per
image class with its own layout: `:x64-hosted :x64-bare :a64-hosted :a64-bare
:rv64-hosted :rv32-hosted :rv-bare :ppc-hosted :ppc-bare :i386-hosted
:i386-bare :arm32-hosted :arm32-bare :68k-hosted :68k-bare :web`.  `:images`
takes maps or groups:

| group | maps |
|---|---|
| `:low-block` (default) | every map whose convention block is RAM at `#x10000000`: all but `:rv-bare`, `:arm32-bare`, `:ppc-bare` |
| `:all` | every map |
| `:hosted` / `:bare` | by kind |
| `:threads` | `:x64-hosted :a64-hosted` (native threads, per-thread window) |
| `:x64`, `:a64`, `:x64+a64`, `:hosted-64`, `:rv-hosted`, `:hosted-32` | as named |

Bare RISC-V (NS16550 UART at `#x10000000`), bare ARM32 (versatilepb/virt
device space) and bare PowerPC relocate their convention slots
(`*RV-GLOBALS-BASE*`, `*ARM32-GLOBALS-BASE*`, `*PPC-GLOBALS-BASE*`), and the
registry records what *is* at `#x10000000` there as a `:region` — so a word
claimed `:all` by mistake collides with device space.  The bare ARM32 **CL**
image (`*ARM32-CL-BARE*`) keeps the hosted layout and counts as `:arm32-hosted`.

**Space.**  `:global` is a process-wide address.  `:window` is a per-thread
window slot (`mvm/compiler.lisp` THE PER-THREAD WINDOW): the main thread's
copy is at the address, a worker's at its block + (address − `#x10000000`).
`:block` is an offset into a *worker's* block that the main thread never
addresses (the dynamic-binding cache at `blk+0xC50`).  Overlaps are checked in
two views per map: absolute (`:global` + `:window`) and block (`:window` +
`:block`).  Entries with `:base` other than `:absolute` (`:thr-table`) are
offsets into that structure, checked among themselves.

**Kind.**  `:word`, `:range` (an array, a frame stack, a table), `:region` (a
heap, an arena, a scratch page, device space — not a word, but it owns the
bytes) and `:container` (a mapping such as i386's demand-zeroed BSS: anything
may lie inside it, nothing may straddle its edge).

**Nesting and sharing.**  `:within PARENT` lets a field sit inside its record
(the MCGC config block's 22 words).  `:shares (OTHER) :reason "..."` declares a
*deliberate* overlap; every one is printed on every build as a near-miss, and
the current list is below.  It is not an escape hatch for a collision.

## The checks

* **H1 overlap** — per map, per view, no two entries intersect unless nested,
  contained or shared.
* **H2 constants** — every bound `:constants` symbol equals its entry's address
  (the registry and the translator cannot drift apart).  A `DEFPARAMETER` that a
  target installer re-points (`+MV-COUNT-ADDR+`, `*I386-HSTACK-BASE-ADDR*`, ...)
  is deliberately not listed.
* **H3 window extent** — every `:window`/`:block` word of a threaded map fits
  in that map's worker block (x86-64 `0x8000`, AArch64 `0x11000`), or a
  worker's copy lands in the next worker's block (`%THR-TLS-BLOCK`'s comment
  records that bug).
* **H4 ratchet** — every `#x10000001..#x1002FFFF` literal in `mvm/ net/ lib/
  boot/` code (comments stripped; strings kept, since build scripts carry
  image source in them), and every window/thread-table offset literal of the
  shapes in `*FA-OFFSET-BASES*` (`(+ (%tls-self-base) #x50A0)`, `(+ b #x5610)`
  inside `%TLS-PREPARE-BLOCK`, `(+ (%thr-table) #xC0)`), must lie inside a
  registered word or range, or be the first or one-past-last byte of any entry.
  Literals in the middle of a heap or arena do not count as registered.

Knobs: `MODUS_FIXED_ADDR_CHECK=0` (off), `=warn` (report, never fail),
`=report` (also print every map); `MODUS_FIXED_ADDR_ALLOWLIST_WRITE=1`
regenerates the allowlist from the tree — to shrink it.

**What it cannot see.**  Addresses assembled inside instruction encodings
(`MOVZ x18,#0x0CD8`, hand-written byte vectors such as boot-x64's IDT stub),
addresses computed at run time, and vessel's `kernel/` sources (a separate
repository; its words — `+RPI-CL-DTB-PTR-SLOT+` — are registered by hand).

## Sanctioned sharing (the near-misses, as of the registry's introduction)

| entries | map | why it is tolerated |
|---|---|---|
| `gc-scratch-historic` ↔ `mv-values` | low-block | `0x10000100..11F` is MV extras 13..16.  Only the Lisp collector (AArch64 shim path / no native arm) uses the historic block; no 14th+ value is read across an allocation there. |
| `fault-diag` ↔ `tls-self`, `dynb-next`, `dynb-depth` | x64, bare | `C30` is both the self slot and the fault RIP slot; the x64 stub skips the RIP store whenever threads can run. |
| `a64-gc-stats`, `a64-jit-bump`, `core-cursor-slot` ↔ `gc-region-table` | a64-bare | CPU 3..11's cells of the per-CPU active-region table; one CPU runs Lisp there.  The threaded layout moves them to `0x1000FF00`. |
| `68k-core-header` ↔ `dirent-buf-page` | 68k-hosted | Header written only while saving a core. |
| `x64-bare-rescue-rip` ↔ `clos-debug-budget` | x64-bare | A rescued fault leaves the RIP in cl-clos's EQ-COLL print budget (diagnostic output only). |
| `68k-profile-counters` ↔ `argv-arena` | 68k-hosted | `MODUS_68K_PROFILE` builds only: the counters sit in the staged-argv arena's tail. |
| `gv-cache-root`, `gv-cache-guard`, `static-literal-root` ↔ `rv-bare-uart`, `arm32-bare-device-space` | rv-bare, arm32-bare | The compiler bakes these roots as absolute `#x10000FAx/FB0` for every target; on bare RISC-V/ARM32 that is device space.  Unverified on hardware; the bare ladder passes. |

Stale claims found while populating the registry (comments, not code):
translate-x64 says "NEXT FREE MCGC CONFIG SLOT IS 0x10000EB0" and its pinning
note "reserves through 0x10000EC0" — `EB0/EB8/EC0` are now the eval lock
(`mvm/mvm-eval.lisp`).  The registry is the authority; those comments predate it.
