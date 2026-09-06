# save-and-die: heap snapshots for Modus

Written 2026-09-06.  Status: **landed for the hosted Linux/AArch64 CLI**
(a6085d2) **and for the bare-metal Pi image, validated under QEMU raspi3b**
(fa9fa1e).  The remaining loose ends are the real board (the RAM-core
round-trip) and x64 stub parity, at the bottom.

## Why

SBCL bakes Quicklisp into its core with `save-lisp-and-die`.  Modus had no
heap snapshot, so every boot re-compiled the quicklisp client from source
through `mvm-eval` — on the Pi Zero 2 W that reload, not the hardware, is the
hour (see the SBCL reference numbers: the same `ql:quickload sha1` is 144 ms
on an A76 and 362 ms under QEMU TCG).  The `MODUS_BAKE_QL` attempt to compile
the client natively into the image hit the build-vs-runtime divergence thirteen
times in thirteen rounds; a snapshot sidesteps that whole class because it
carries *runtime state*, whatever produced it.

Measured, hosted aarch64 under `qemu-aarch64-static` (TCG):

| step | time |
|---|---|
| fresh boot to `--eval` | 15.9 s |
| **restored** boot to `--eval` | **0.24 s** |
| load quicklisp client + `(quicklisp:setup)` from source | 5 m 14 s, once |
| from the 46 MB core: restore + `(ql:quickload "sha1")` + `sha1-hex "abc"` | **4 s** (digest correct) |

## The model

After a full Cheney collection every live object sits in ONE contiguous range
`[from_start, alloc-ptr)` of the first semispace.  The only pointers into that
range from outside it are the collector's fixed roots in the metadata window,
plus the object-start and cons-kind bitmap bits that validate conservative
roots.  So a snapshot is: a header, the metadata window, the live range, and
the JIT arena (which holds the bitmaps too).  Restore is the same reads in the
same order into the same addresses.

"Same addresses" is the whole design.  Two fixed mappings make the core
relocation-free, so nothing is rewritten on the way in — and EQ hash tables
keyed on addresses, JIT'd code with absolute targets, and every interior
pointer stay valid as they are:

- **Heap** at `+linux-aarch64-fixed-heap-base+` = `0x2000000000`, asked for
  with `MAP_FIXED_NOREPLACE`.  If the kernel returns anything else the stub
  falls back to the historical hint mmap, and a later `--core` refuses with
  `heap base differs` instead of guessing.
- **JIT arena**: one 512 MB `PROT_RWX | MAP_NORESERVE` mapping at
  `0x3000000000`.  The `%mmap-exec-page` trap (`#x0531`) bump-allocates from
  it — the shape bare metal already had — with the bump word at `0x10000F58`.
  Bump word 0 (no arena) is the old `mmap(NULL)` path.  Because the GC bitmaps
  are reserved through the same trap, they are the arena's first 16 MB and
  travel with it; JIT pages follow.

`lib/save-image.lisp` is the shared code; the arch slots are the stub
(`boot/boot-linux-aarch64.lisp`), the trap arm (`translate-aarch64.lisp`), and
`%core-open-path-at` (aarch64 needs `openat`).

## Core file layout

All header words are written with `(setf (mem-ref … :u64))` and read back with
`mem-ref`, so they round-trip through the same `value<<1` convention; the
window, heap and arena bytes go through `read(2)`/`write(2)` and never pass
through a Lisp word (a tagged pointer would lose its low bit).

| offset | word |
|---|---|
| 0 | magic `20260905` |
| 8 | `from_start` (must equal this process's) |
| 16 | `to_start` |
| 24 | `space_size` (must equal) |
| 32 | `free` — the alloc pointer at save |
| 40 | window length (4096) |
| 48 | bitmap slice length (0 when an arena is present) |
| 56 | bitmap slice byte offset |
| 64 | arena base (0 = none) |
| 72 | arena bump |
| 128 | metadata window `0x10000000..0x10001000` |
| +4096 | heap `[from_start, free)` |
| … | bitmap slices (arena-less saves only) |
| … | arena `[base, bump)` |

## What is restored, and what deliberately is not

In place, from the window: the collector's fixed roots — globals alist
`0x80`, symbol table `0x88`, keyword table `0x148`, package-by-hash `0x170`,
the JIT constant-vector root `0xF10` — and `gc_count` at `0x60`.  Everything
else in the window is **per-process** and is read to a staging page instead:
`stack_base` / `saved_rsp`, the native handler-frame triple at `0x180..`,
`nargs`, the argv copies at `0x200..`, the bitmap config words at `0xE00..`,
the GC stats.  After the reads, `set-alloc-ptr` to the saved `free`, the bump
word is set (raw: `(ash bump -1)`), and `%jit-icache-flush` runs over the
arena.  `%core-post-restore` reinstalls signal handlers — `sigaction` is per
process.

Not carried, by design: the stack (restore happens on a fresh one, before any
boot init), open file descriptors other than 0/1/2, sockets, and anything the
kernel maps per process.  A saving run must therefore be quiescent: call
`save-and-die` from a toplevel `--eval`, not from inside a handler or a
`with-open-file`.

## Usage

    modus --eval '(load "quicklisp/setup.lisp")' --eval '(save-and-die "ql.core")'
    modus --core ql.core --eval '(ql:quickload "sha1")' …

`--core` must be `argv[1]`: `kernel-main` tests it before `init-symbol-table`
(the snapshot carries every table boot would build) and reads the path through
the raw `argv[2]` pointer the stub stored at heap-base+32 — the restore path
touches no global, no string and no stream, because none exists yet.  Anywhere
else on the command line `--core` reports itself and exits rather than run
un-restored.  `%save-image` returns the live heap byte count and does not
exit; `save-and-die` does.

The rig: `/home/claude/cabfs/core/t1.sh` (trivial save/restore) and `t2.sh`
(the quicklisp client, from the Genera-aware copy under `boardstage/qlfiles`
— the host's `~/quicklisp/quicklisp/impl.lisp` has no Genera block).

## Things that will bite

- **A core is bound to the image that wrote it.**  Function addresses in the
  heap are absolute into the image; only the magic and heap geometry are
  checked today.  Restoring a core into a different build is undefined.  TODO:
  bake a build hash into both and refuse on mismatch.
- **`mem-ref :u64` halves what it loads and doubles what it stores.**  Raw
  words (the bump pointer, the argv pointers) are read as `(* 2 …)` and written
  as `(ash v -1)`; both are even by construction.
- **Anything allocated by the restore code before the heap read is gone
  after it.**  Every value the restore holds is a fixnum in its own frame, and
  the header scratch sits below `from_start` (heap-base+256), which is never
  live.
- **The stub order matters:** the arena block clobbers `x0` and must come
  after `MOV x22, x0` captures the heap base.  Getting that wrong pointed the
  heap registers at the arena and died on a NULL `car` — `gdb-multiarch` on
  `qemu-aarch64-static -g` read it off x22 in one shot.
- **The RAM copy MUST be bit-exact (bare metal).**  `mem-ref :u64` type-puns
  the tagged word: a fixnum loads>>1 / stores<<1, a pointer keeps its low tag
  bits.  A `:u64` word-copy therefore loses the low bit of every tagged
  pointer and corrupts the whole heap.  Copy two `:u32` halves instead (the
  load's `<<1` and store's `>>1` cancel, exact, and no `>=2^62` word is ever
  materialised so it never conses); use `%gc-read64`/`%gc-write64` for the raw
  cursor word.  The header round-trips regardless because it is written and
  read through the same `mem-ref` on both sides.
- **Publish the alloc pointer before ANYTHING conses (bare metal).**
  `%core-close` prints `CORE-END=` via `print-dec`, which allocates; if that
  runs before `(set-alloc-ptr free)`, the alloc pointer is still the fresh
  boot's (heap base) and the string lands on the just-restored globals table,
  so the next global read faults in `%gv-cell`.  Symptom that pinned it:
  offline the core's bucket vector was intact, but the LIVE restored heap had
  fresh objects sitting exactly where the vector should be.  On bare metal a
  fault is a silent spin, not a SIGSEGV — attach `gdb-multiarch` AFTER the
  `!!FAULT` line prints and dump memory from the spinning guest.

## Found on the way (open)

1. **Compiler bug, every arch, JIT on or off:** a `labels`/`flet` local
   function whose `&optional`/`&key` *default* references an enclosing lexical
   returns NIL:

       (defun r3 (v) (labels ((f (&optional (i v)) i)) (f)))   ; => NIL, want 7

   A bare `lambda` with the same lambda list is fine.  Quicklisp's
   `next-line-pos` and `process-header` (http.lisp) are exactly this shape.
2. **JIT-off only, hosted aarch64:** loading `http.lisp` forms 67 and 69
   (those two defuns) signals `SIMPLE-ERROR NIL` at *definition* time under
   the interpreter; JIT-on loads them.  Hand-written reductions pass, so it
   needs the real `acase` forms — `/home/claude/cabfs/core/probe-http.lisp`
   walks the file and names the failing form.  Bare metal runs the
   interpreter for everything the JIT declines, so this one matters for the
   board.
3. **x64 `./modus` has no fixed heap yet.**  The shared code builds and
   `--core` refuses honestly (`heap base differs`); the stub needs the same
   two mappings and the x64 `#x0531` arm the same bump fallback.  Small.
4. The core path is read from the live `argv[2]`, so it has no 63-byte limit,
   but `--core` must be first.

## The board

**★ VALIDATED ON REAL SILICON (Pi Zero 2 W, 2026-09-06):** produced a core
under QEMU from the exact chainload board binary (trampoline at 0x80000 jumps
to the image at 0x300000; the board build talks to the mini-UART, which
qemu-system-aarch64 `-M raspi3b` maps to its SECOND `-serial`), then netbooted
kernel+core to the board (U-Boot `tftpboot 0x18000000 ql.core; tftpboot
0x300000 kernel; go 0x300000`, scripts/netboot-core.py).  The board printed
`MODUS-CL` then `CORE-RESTORED` (restore before boot init -- no E2SMOKE, no
reload) and evaluated the restored CLOS surface: `(dist2 *p*)`=>25,
`(px *p*)`=>3, and dispatch on a NEW instance of the restored class
`(dist2 (make-instance 'pt :x 6 :y 8))`=>100.

**Validated under QEMU `-M raspi3b` (fa9fa1e):** a core carrying a defvar, a
defun, a defclass with accessors, a defmethod, a macro and an instance
restores in ~2 s (vs ~30 s normal boot) and every form evaluates correctly at
the REPL, including method dispatch on a NEW instance of the restored class
(5^2+12^2 = 169).  Two bare-metal bugs were fixed getting there — see
"Things that will bite".  What remains is the RAM-core round-trip on real
silicon.

The bare-metal Pi image already has both fixed regions the model needs —
that is why this design was chosen over relocation:

| region | address |
|---|---|
| image (chainload) | `0x300000`, ~64 MB |
| stack top (down) | `0x08000000` |
| heap | `0x09000000 .. 0x10000000` (two 56 MB semispaces) |
| metadata window | `0x10000000 .. 0x10001000` |
| JIT bump word / region | `0x13FFFFF0` / `0x14000000 .. 0x18000000` |
| **free** | `0x18000000 .. 0x1C000000` (DRAM is 448 MiB) |

Plan, in order:

1. **Deliver the core with the kernel** — DONE in the image.  No filesystem
   exists at restore time, so the core is a second TFTP file: `tftpboot
   0x18000000 ql.core` before `tftpboot 0x300000 modus.img; go 0x300000`.  The
   board build reads from that address instead of `read(2)`; the arch slots
   (`%core-open-in`/`%core-read-all`/`%core-copy-words`, `%core-requested-p`
   on the magic at `0x18000000`) are in build-cl-repl-common.lisp.

   **THE OPEN PROBLEM: getting the core OFF the board as a file to TFTP back.**
   `save-and-die` writes the core to `0x18000000` in RAM and halts; there is
   no gdbstub on real silicon to dump it (that is how QEMU validation cheats).
   Candidate: after `%save-image` writes `[0x18000000, end)`, DON'T halt —
   stream that RAM range to modus-pi over the board's own TCP/HTTP stack (a
   POST, or a raw send), and modus-pi saves it as `ql.core`.  Then netboot
   again with it.  A "dump a RAM range over the network" form is the one new
   piece; the send path is the same one `net-install-and-call` fetches over.
2. **Save on the board's geometry.**  A core must match the image that
   restores it, so the saving run is the *board image* itself: boot, load the
   client from the tarball path (the one command that already works on
   hardware), `save-and-die` to serial or to the cabinet.  Serial at the mini
   UART's rate is too slow for ~10 MB; the cabinet on the SD card, or the
   USB/CDC network, is the channel.  Alternative if that proves slow: run the
   board image under QEMU `raspi3b` (same geometry, same image) and save there
   — QEMU can write the file; the core is then bit-identical to what the board
   would produce, minus hardware state we do not carry anyway.
3. **Hardware re-init after restore.**  The saved heap holds driver state
   (DWC2, CDC-ECM, the cabinet's device layer) from the saving process.  The
   restore path must re-run the hardware bring-up that `kernel-main` does after
   the snapshot point, or the snapshot point must be *before* it.  Simplest:
   snapshot before net-up, restore, then run net-up as usual.
4. **Cache maintenance** is already in the model: `%jit-icache-flush` over
   the arena after the copy, and the DRAM the core lands in was written by
   U-Boot with the MMU on (see the `go` cache-inheritance note).
5. Then the goal: `ql:quickload` on the Zero 2 W in seconds, from a core, with
   the client's compile paid once.

Order of work: (2) via QEMU `raspi3b` first — it validates the bare-metal
restore path with a gdbstub on hand — then (1) on the board.
