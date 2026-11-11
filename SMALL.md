# Small images

How small modus can be on small machines, what makes it big, and the switches
that make it smaller.  Measured on the 68LC040 target (hosted Linux/m68k,
soft float) under `qemu-m68k-static -cpu m68040`; the same switches apply to
the other hosted CLIs unless noted.

## The yardstick

Macintosh Common Lisp 2.0 (1992): a full Common Lisp with a native compiler,
CLOS and an IDE, in **4 MB of RAM and 6 MB of disk** on a 68020+ Mac.  Coral /
Macintosh Allegro CL 1.x ran on a 1 MB Mac Plus (a 68000), slowly.

The goal: a true hosted REPL -- reader, compiler, printer, the whole language --
on the LC040 inside MCL 2.0's budget.  Below that, images that only run a
fixed program (a "thunk"): the floor for microcontrollers.

## Where it stands

| image | file | resident | heap floor |
|---|---|---|---|
| default 68k CLI (REPL) | 30.4 MB | -- | 7.5 MB |
| `MODUS_SMALL_IMAGE` + `MODUS_NARROW_STRINGS` (REPL) | 12.5 MB | 12.0 MB | 5.75 MB |
| `+ MODUS_NO_COMPILER` (hello world, no REPL) | 12.7 MB | 7.3 MB | 4 MB |
| `+ MODUS_LEAN_BOOT` + `MODUS_HOT_ORDER` (hello world) | 3.0 MB | 0.40 MB | 1.25 MB |

The `MODUS_NO_COMPILER` row predates the translator density work; the others
are current.

Resident is the guest's own pages at the REPL prompt (after a `defun` and a
call) or at exit.  `xz -9` takes the REPL image to 1.5 MB and the hello-world
image to 0.3 MB: most of the size is how code is encoded, not what it says.

## Switches

All opt-in; unset, every image is built as before.  68k soft float:
`MODUS_68K_SOFT_FLOAT=1 sbcl --dynamic-space-size 12288 --script mvm/build-68k-cli.lisp`.

| switch | what it does |
|---|---|
| `MODUS_SMALL_IMAGE=1` | boot tables built from packed text (symbol names, function table, opcode table), global init batched through one guarded loop, no `ansi-test-aux`; on 68k turns on branch relaxation |
| `MODUS_NARROW_STRINGS=1` | one byte a character for strings the reader and the name tables make, and (68k) for pooled literals (the collector sizes `#x13` since `238b68e2`) |
| `MODUS_68K_A5_GLOBALS=1` / `0` | A5 holds the globals base, so nargs / closure env / MV count are `(A5)` / `d16(A5)`; unset follows `MODUS_SMALL_IMAGE` |
| `MODUS_68K_RELAX_BRANCHES=1` / `0` | each 68k branch as short as its distance allows (two translation passes); unset follows `MODUS_SMALL_IMAGE` |
| `MODUS_NO_SOURCE_BLOB=1` | no embedded source (disk only on a hosted system) |
| `MODUS_NO_COMPILER=1` | no compiler: the compiler-side files filtered to what the rest of the image reaches (`mvm/build-no-compiler.lisp`), entry points stubbed to signal; the toplevel runs a `%IMAGE-MAIN` baked in with `MODUS_EXTRA_SOURCE=FILE` |
| `MODUS_LEAN_BOOT=1` | with `MODUS_NO_COMPILER`: boot builds only what the thunk needs -- no packages (lazy interning), no name tables, no keyword seeding, globals initialised only if kept code reads them -- the image is shaken to what `KERNEL-MAIN` reaches (`%IR-DCE`, exact, from the IR), and quoted symbols are static objects in the constant pool.  Strictly boot-and-run: integer printing and `handler-case` of `error` do not work |
| `MODUS_HOT_ORDER=FILE` | lay out first the functions boot runs (`scripts/hot-order.py` makes the file from a `MODUS_SYMMAP` and a `qemu -d in_asm` log) |

## Measuring

- **Size by function:** `MODUS_SYMMAP=FILE` writes address, size and name of
  every function.
- **Size by MVM opcode:** `MODUS_68K_OP_STATS=1` prints native bytes per opcode,
  and LI by destination and immediate class with the commonest immediates.
- **Why something is kept:** `MODUS_IR_DCE_WHY=NAME,...` (lean builds) prints
  the call chain that kept each name.
- **Resident memory:** qemu-user maps the guest 1:1, so the guest's pages are
  the host process's pages below `#x41000000`.  Read `/proc/PID/pagemap` per
  region -- image (the ELF's file bytes), heap (`#x10000000`), kind map
  (`#x1F000000`), stack (`#x40000000`).  Count the BSS after the file
  separately: qemu-user clears part of it in place, a real kernel does not.
- **What boot executes:** `qemu-m68k-static -d in_asm -D LOG` logs every
  translated block; map the addresses through the symbol map.
- **Correctness:** a smoke program (floats, bignums, ratios, `format`,
  conditions, CLOS, `loop`, `sort`, symbols) must print the same as the
  original image, also at a tight heap (collections throughout) and on a
  `MODUS_68K_GC_POISON=1` build (a stale pointer faults).  "No change" for
  other images is proven with `MODUS_DUMP_FULL_SOURCE` -- image binaries are
  not reproducible build to build.

## What costs what (the REPL image)

- **Code encoding.** The 68k translator expands each MVM operation into a fixed
  native sequence; the same type checks, GC checks, argument setup and global
  reads repeat at every site.  Done so far: the object tag folded into slot
  displacements, short stores, relaxed branches, compact `car`/`cdr`/subtag
  sequences (`0d1c4bc3`, `5860ef14`): default image 38.2 -> 31.0 MB.
  Under `MODUS_SMALL_IMAGE`: out-of-line cons / subtag / array-length /
  allocation sequences, a quoted symbol as one call, and a global read or
  write as one call (`*GV-CACHE-MODE*` `:CALL`, no inline cache probe), a
  quoted symbol's site passing only its slot index (the hashes in a table),
  the calling-convention slots through A5, and shifted-byte immediates as
  MOVEQ + LSL: the REPL image's native code 13.7 -> 10.0 MB; the smoke
  program runs ~20% slower.
- **The compiler**, ~5.3 MB of native code, ~2.7 MB touched in a short session.
- **Boot data built at boot.**  Packages, symbols, the name and function
  tables: most of the heap floor, held twice over by the copying collector.
  Some of it was written as code that runs once (`dfa4db90`, `4a321be3`).
- **String literals** are built a character at a time, ~22 bytes of 68k a
  character.

## Tried, and why it failed

- **Pooled string literals** (`*NARROW-LITERALS-P*`, a load and a call to
  `%FRESH-STRING` instead of inline construction).  Narrow pooled copies broke
  the reader (kernel code writes characters a narrow copy cannot hold); wide
  pooled copies ran away at boot.  The literal becomes a CALL in the middle of
  an expression, and the compiler treats literals as call-free leaves: saving
  the caller-saved temps and V0-V3 around it was not enough.  The x64
  `MODUS_NARROW_STRINGS` build emits the same bare call.  A literal should
  stay call-free: an inline "copy pooled string" operation per translator.
- **Batching the function table's entries** through a 100-argument `&rest`
  call: correct, and bigger (215 -> 395 KB) -- a `&rest` argument costs more
  than the three-argument call it replaced.
- **A symbols-first constant pool** (lean images): no measurable effect.
- **A tethered REPL** (compile on a host, replay FASL records on the target):
  feasible, set aside -- the goal is a self-contained REPL.

## Plan

1. **Shared sequences in the 68k translator.**  The most repeated native
   sequences out of line, one copy each: type checks, the GC check, argument
   setup, the global-read cache path.  Native speed stays; each pattern is
   measured on its own.
2. **Boot from a saved core with a static region.**  Build the boot data at
   build time and keep it where the collector does not copy it, as MCL booted
   from a saved heap.  Lowers the heap floor and the boot time.
3. Measure against 4 MB / 6 MB.  If native cannot get there: the **bytecode
   image** (the web REPL's design -- the image stays MVM bytecode, a small
   native interpreter runs it; slower, several times smaller).
4. Smaller: call-free pooled literals; the remaining generated tables (hash
   dispatch names, runtime macros); the FASL cache as an option.

The Mac Plus (68000) would also need a 68000 code-generation mode: the
translator uses 68020+ instructions (long branches and displacements).
