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
| `MODUS_SMALL_IMAGE` + `MODUS_NARROW_STRINGS` (REPL) | 11.9 MB | 8.2 MB | 5.75 MB |
| `+ MODUS_68K_THREADED` + `MODUS_68K_HOT` + `MODUS_HOT_ORDER` (REPL) | 5.5 MB | 5.6 MB | 5.75 MB |
| `+ MODUS_68K_CORE` (REPL booting from its saved core, 1 MB young heap) | 7.8 MB | dirty 0.4 MB; a command touches 1.0 MB code + 1.3 MB heap | 1 MB |
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
| `MODUS_68K_GC_STATIC=1` / `0` | after boot, what boot left live becomes a static heap the collector walks and never copies, and the collector hands evacuated pages back (`madvise`); unset follows `MODUS_SMALL_IMAGE` |
| `MODUS_68K_THREADED=1` | cold functions become THREADED CODE: a byte stream of tokens (one per MVM instruction, a compare and its branch fused) run by ~2,800 shared native handlers; A5 is the token pointer (so not the globals base) |
| `MODUS_68K_HOT=FILE` | with `MODUS_68K_THREADED`: the functions named in FILE (one a line) stay native |
| `MODUS_68K_PROFILE=1` | per-function execution counters at `#x10010000` (entries + loop heads), for choosing the hot list |
| `MODUS_68K_CORE=1` | with `MODUS_68K_GC_STATIC`: the build boots the image once (`MODUS_68K_SAVE_CORE`), saves the low block and the static heap, and links them in as `PT_LOAD`s (`scripts/68k-core-link.py`); the image then starts from that heap: no boot init, 0.1 s to the prompt |
| `MODUS_68K_RELAX_BRANCHES=1` / `0` | each 68k branch as short as its distance allows (two translation passes); unset follows `MODUS_SMALL_IMAGE` |
| `MODUS_NO_SOURCE_BLOB=1` | no embedded source (disk only on a hosted system) |
| `MODUS_NO_COMPILER=1` | no compiler: the compiler-side files filtered to what the rest of the image reaches (`mvm/build-no-compiler.lisp`), entry points stubbed to signal; the toplevel runs a `%IMAGE-MAIN` baked in with `MODUS_EXTRA_SOURCE=FILE` |
| `MODUS_LEAN_BOOT=1` | with `MODUS_NO_COMPILER`: boot builds only what the thunk needs -- no packages (lazy interning), no name tables, no keyword seeding, globals initialised only if kept code reads them -- the image is shaken to what `KERNEL-MAIN` reaches (`%IR-DCE`, exact, from the IR), and quoted symbols are static objects in the constant pool.  Strictly boot-and-run: integer printing and `handler-case` of `error` do not work |
| `MODUS_HOT_ORDER=FILE` | lay out first the functions boot runs (`scripts/hot-order.py` makes the file from a `MODUS_SYMMAP` and a `qemu -d in_asm` log); any build, not only `MODUS_LEAN_BOOT`.  `mvm/68k-hotorder-repl.txt` is boot + a first REPL command |

## Measuring

- **Size by function:** `MODUS_SYMMAP=FILE` writes address, size and name of
  every function.
- **A hot list:** build with `MODUS_68K_PROFILE=1 MODUS_SYMMAP=FILE`, run the
  workload under gdb to `%CLI-EXIT`, `dump binary memory` `#x10010000` ..
  `#x10018000`; counter i is the i-th function in address order.  On the
  smoke program + a REPL session the top 100 functions are 96% of the counts
  (424 KB native), the top 400 99.9% (1.7 MB); 4,100 of 6,226 never ran.
- **Size by MVM opcode:** `MODUS_68K_OP_STATS=1` prints native bytes per opcode,
  and LI by destination and immediate class with the commonest immediates.
- **Why something is kept:** `MODUS_IR_DCE_WHY=NAME,...` (lean builds) prints
  the call chain that kept each name.
- **What a REPL needs, Coral-fashion:** dirty memory plus the clean pages one
  more command touches (`/proc/PID/clear_refs`, then `Referenced:` in smaps)
  -- clean image pages can be dropped and re-read, as Mac code segments were.
- **Resident memory, carefully:** measure after the REPL has ANSWERED (poll
  its output), with `MODUS_FASL=0` (a FASL cache hit runs a different path
  and touches other pages), and read the pages of YOUR process (`$!`), never
  a `pgrep` of the image's path -- a concurrent test run of the same image
  answers instead.  Each of these produced wrong numbers here first.
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
  Under `MODUS_SMALL_IMAGE`: out-of-line cons / car / cdr / subtag /
  array-length / allocation sequences, a quoted symbol as one call, and a global read or
  write as one call (`*GV-CACHE-MODE*` `:CALL`, no inline cache probe), a
  quoted symbol's site passing only its slot index (the hashes in a table),
  the calling-convention slots through A5, shifted-byte immediates as
  MOVEQ + LSL, and the 30-bit arithmetic range guard as one ASL + BVS: the
  REPL image's native code 13.7 -> 9.4 MB; the smoke program runs ~20%
  slower.
- **The compiler**, ~5.3 MB of native code, ~2.7 MB touched in a short session.
- **Boot data built at boot.**  Packages, symbols, the name and function
  tables: most of the heap floor, held twice over by the copying collector.
  Some of it was written as code that runs once (`dfa4db90`, `4a321be3`).
  Measured at the REPL prompt (68k small + narrow image, a heap dump walked
  by the collector's kind map): ~1.4 MB in the live semispace, of which
  0.8 MB is 13,200 WIDE strings (4 bytes a character) -- mostly symbol and
  package names, 7,900 of them in one hash-table storage vector that could
  not be tied to a live global (it may be a stale generation) -- 0.27 MB
  conses, 0.23 MB arrays.  `*SYM-NAME-TABLE*` (11,940 names) is narrow, as
  are symbol names; the symbol-function table has 240 wide keys.  `COPY-SEQ`
  and `SUBSEQ` of a narrow string return a WIDE string (CL says the result
  keeps the element type), and package names are wide.  The heap floor is
  between 5 and 6 MB.
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
2. **Boot from a saved core** (`MODUS_68K_CORE`): done.  The low block and the
   static heap are file-backed `PT_LOAD`s, clean until written; the stub maps
   only the young semispaces (`--dynamic-space-size` is their total, min 512
   KB), and KERNEL-MAIN's core hook skips boot init.  Dirty memory at the
   prompt ~0.3 MB (was 2.4); boot 0.1 s (was 12); the smoke program 0.4 s.
   Every collection walked the whole 2 MB static heap, so all of it was in a
   command's working set: now a CARD BARRIER marks the static card holding an
   object's start after each SETCAR / SETCDR / OBJ-SET / ASET (two
   instructions and a shared helper; threaded code pays it per handler), and
   a collection scans only marked cards, re-marking those that still refer
   to the young space.  A command's heap working set 2.4 -> 1.3 MB, most of
   it the young space itself.
   The core image still carries boot's init code and tables: disk, not RAM.
   **A static boot heap.**  First half done (`MODUS_68K_GC_STATIC`): after
   boot the collector freezes what it copied -- 1.56 MB of objects, 0.51 MB of
   conses -- walks it typed at every collection, and releases evacuated
   pages (and their kind bytes), so the heap at the prompt is 2.3-2.5 MB
   resident, not 5.4, and the kind map 0.12 MB, not 0.37.  The heap
   FLOOR is unchanged (5-6 MB), because boot's final live set is what sets it;
   lowering it means freezing during boot, or building the boot data at build
   time (a saved core, as MCL booted from a saved heap).
3. **Threaded cold code** (`MODUS_68K_THREADED`): first cut done.  MVM
   bytecode itself is only ~15% denser than 68k (8.0 vs 9.4 MB), so the cold
   tier is a new encoding: 1-byte tokens for the 255 commonest shapes,
   operands sized by value.  All 5,990 eligible functions threaded: 7.75 MB
   image, ~14x slower.  With the profile's 300 hottest kept native (functions
   with SETJMP / LONGJMP thread too), boot-time shim sources baked without
   comments (they are evaluated, not read by people), pooled literals really
   narrow (the switch had been set in the wrong package), and boot's
   functions laid out first: 5.5 MB image, 5.6 MB resident at the prompt
   (image pages 3.1, heap 2.3 -- the static boot data), smoke time on a par
   with the native image.
   Next: superinstructions (call sequences, push/pop runs) and a smaller
   handler table; the boot-time cost of `%GV-CACHE-FLUSH` (16,384 slots
   cleared per global created, ~7,500 times at boot: the profile's hottest
   function).
4. Smaller: call-free pooled literals; the remaining generated tables (hash
   dispatch names, runtime macros); the FASL cache as an option.

The Mac Plus (68000) would also need a 68000 code-generation mode: the
translator uses 68020+ instructions (long branches and displacements).
