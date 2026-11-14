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
| `+ MODUS_68K_CORE` (REPL booting from its saved core, 512 KB young heap) | 7.8 MB | 4.5 MB after a REPL session, 0.44 MB of it dirty | 512 KB |
| `+ MODUS_68K_TAGS3` (3-bit tags, 8-byte granule) | 7.5 MB; core 1.88 MB (was 2.24) | 4.15 MB after a REPL session (was 4.52), 0.34 MB of it dirty (was 0.44) | 512 KB |
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
| `MODUS_68K_TAGS3=1` / `0` | 68k pointer tags in 3 bits (object 7, T `#xDEAD1007`) and an 8-byte heap granule instead of 16: a cons is 8 bytes, objects round to 8, the kind map is one byte per 8 bytes, forwarding is recorded in the kind map.  Unset follows `MODUS_SMALL_IMAGE`; `mvm/build-hosted.lisp` reads it too, for the hosted ladder |
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
- **qemu-user cannot show a code working set:** code it has translated runs
  from its own cache and never touches the guest's pages again, so
  `Referenced:` on the image counts data (token streams, constants, tables)
  and first translations.  Dirty memory and resident memory are sound.
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

## The 8-byte granule (`MODUS_68K_TAGS3`)

The 16-byte granule cost 426 KB of the 2.1 MB static heap (an 8-byte cons
took 16).  With 3-bit tags the core goes 2,240,544 -> 1,884,192 bytes.  The
tag constants are per-target parameters in `target.lisp` (folded into the
image through the width preamble), and three places had the old values
baked in, each found by the image failing:
- `translate-i386.lisp` DEFCONSTANTed `+tag-cons+` / `+tag-object+`, so every
  translator loaded after it folded 9 (SBCL: `(constantp '+tag-object+)` = T);
- prelude's `%make-base-string` / `%make-string32` / `%make-u32-vector`
  wrote the header at `word - 9`;
- the interpreter's LI arm recognised T by its low byte `#x09`.
Validated: hosted ladder 35/35 + h01 under tags3, `validate.sh` and the poison
build identical to the references, core image included.  x64 and i386 ladder
images byte-identical to before.

## A REPL on a 1.44 MB floppy

The core image above, compressed, is 1.57 MB (xz -9e).  On one 1.44 MB disk
(1,457,664 bytes under FAT12) it is 1,457,039 bytes, self-unpacking:

    # 1. trim (EXPERIMENTAL, post-link): what a core image never runs again
    qemu-m68k-static -d in_asm -D LOG IMAGE ...   # over a representative workload
    grep -oE '^0x[0-9a-f]+' LOG | sort -u > EXEC
    scripts/68k-floppy-trim.py IMAGE SYMMAP EXEC TRIMMED
    # 2. the loader: LZMA1 decoder in Lisp, no runtime (a ladder rung's build)
    MODUS_68K_LINUX_OUT=unlzma sbcl --script mvm/build-hosted.lisp 68k \
        mvm/68k-unlzma.lisp mvm/unlzma-core.lisp
    # 3. pack (--test: the image writes the unpacked bytes to stdout instead)
    scripts/68k-floppy-pack.py unlzma TRIMMED FLOPPY

- **The trim** blanks boot-only code (~300 KB: init, table fillers, the
  x64/aarch64 JIT runtime) and zeroes the packed name tables (256 KB): xz 1.57
  -> 1.456 MB.  The shim sources stay -- they are evaluated at first use.
  It is a post-link hack; the real version is a build-time shake from the
  core-boot path (see the script's header for the two traps it hit).
- **The loader** (`mvm/68k-unlzma.lisp`, the Linux head, + `mvm/unlzma-core.lisp`,
  the decoder; 13.6 KB) is LzmaSpec.cpp in fixnum
  Lisp: the range coder's 32-bit values are 16-bit halves, the arithmetic the
  raw `%FIXNUM-` ops on declared fixnums, so there are no overflow checks and
  nothing to link (generic arithmetic made it 40 KB).  7.46 MB unpacks in
  ~4 s under qemu; on a 68040, minutes.
- **The copier** (`scripts/68k-floppy-copier.s`, 88 bytes) maps each segment
  fresh, copies it in, restores the stack the process started with (the boot
  stub's saved `stack_base`, `#x10000058`) and jumps to the image's entry --
  reached from Lisp by `%JIT-CALL` (trap `#x0532`, now on 68k).
- **The pack** is a raw LZMA1 stream, `lc=2 lp=1 pb=2` (best for 68k code,
  1,443,186 vs xz's 1,456,180), mapped with the whole file from offset 0 so
  the payload needs no page padding (its offset is in `e_ident[12..15]`).

Validated: `validate.sh` and DESCRIBE / APROPOS / DOCUMENTATION / COMPILE
probes identical to the untrimmed image.  The margin is 625 bytes.

## The same, as a Mac boot floppy

    scripts/mac68k-floppy.py MAC-IMAGE DISK        # two 800 KB floppies, DISK-1 and DISK-2
    scripts/mac68k-floppy.py MAC-IMAGE DISK --hd   # or one 1.44 MB floppy

Two 800 KB disks (818,176 + 421,699 bytes; 397 KB free on the second): disk
1's boot blocks enter a 68000 stage 2 (`scripts/mac68k-disk2.s`) that reads
block 0 of every drive for disk 2's label; if no drive has it, it ejects disk
1, writes INSERT MODUS DISK 2 on the screen and polls.  Measured: both disks in
two drives boot to the prompt (15.7 s, JIT); disk 1 alone ejects and shows the
message.  NOT measured: a swap in ONE drive -- headless Basilisk cannot insert
a disk after boot.

A bare-Mac image (`mvm/build-mac68k-cl-repl.lisp`, linked at `#x00100000`)
goes behind the same decoder with a Mac head (`mvm/mac68k-unlzma.lisp`): the
boot blocks read loader + stream to `#x01200000`, the loader unpacks to
`#x00100000`, pushes the 68040's caches (`CPUSHA BC`: the image was written
as data) and jumps.  No trim needed: the small REPL image (5,557,396 bytes,
framebuffer console included) is 1,222,613 bytes of LZMA + a 13 KB loader, a
1.44 MB floppy with 237 KB to spare.  Measured on Basilisk II (LC 475 ROM,
68LC040, 32 MB), prompt at:

| | interpreted | JIT |
|---|---|---|
| uncompressed disk | 85.3 s | 11.5 s |
| compressed, `--disk` | 143.4 s | -- |
| compressed, `--floppy` | 142.8 s | 13.6 s |

Unpacking is the difference: ~58 s interpreted, ~2 s under the JIT.  Debian's
`BasiliskII-jit` crashes before the ROM loads (any disk, System 7.5.3 too): it
is a PIE and its JIT bakes 32-bit absolute addresses of its globals and heap.
`scripts/basilisk-unpie.py` relinks it at `#x10000000`; run it with
`MALLOC_MMAP_MAX_=0` so every `malloc` comes from the (now low) brk heap.

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

## The 68000 (Mac Classic / Plus / SE): `MODUS_68K_CPU=68000`

The translator's 68020 instructions, each replaced (translate-68k, "THE
68000"): long `BRA` / `BSR` -> `JMP` / `JSR` to an absolute address (the same 6
bytes), long `Bcc` -> `B!cc.S` over a `JMP`, `LEA (bd.l,PC)` -> `LEA abs.L`
(absolute native addresses are filled in after layout: `*68K-ABS-PATCHES*`,
cross.lisp `apply-68k-abs-patches`; a boot stub's long branches are 16-bit
instead), `MULS.L` / `MULU.L` (64) / `DIVS.L` -> shared subroutines on D0/D1
(a boot stub gets a private copy), `EXTB.L` -> `EXT.W` + `EXT.L`, the threaded
dispatcher's scaled index -> two adds, and every token-stream operand read a
byte at a time (an operand may be at an odd address, a bus error on a 68000;
the stream branch tests its condition first because a byte read sets the
flags).  Off, every image is unchanged (a ladder image is byte-identical).

Measured:
- the hosted ladder on `qemu-m68k -cpu m68000` (which rejects the 68020
  instructions but does not fault odd addresses): 32 / 35;
- the Mac ladder on MAME's Mac Classic (a 68000 that does fault them, 4 MB):
  32 / 35.  The three misses are the FPU rungs (r18 r30 r31): a ladder image
  has no runtime, so no soft float;
- the small CL REPL image built `MODUS_68K_CPU=68000 MODUS_68K_SOFT_FLOAT=1`
  (5.6 MB) on `qemu-m68k -cpu m68000`: bignums, ratios, `sqrt`, `format ~R`,
  `defun`, hash tables, `handler-case` all answer as on the 68040.

The Mac Classic itself: `MODUS_MAC68K_MACHINE=classic` (boot/boot-mac68k.lisp
`*MAC68K-LAYOUTS*`, measured in MAME when our boot blocks run) puts the image
at 64 KB and a 1 MB heap at 2 MB, under the screen at `#x3FA700`.  The REPL
image does not fit 4 MB yet.  MAME 0.251 (Debian's) runs `macclasc` headless
with the Classic ROM (`A49F9914`) and the ADB modem's `342s0440-b.bin`; it has
no serial port there, so `scripts/mac68k-mame-serial.lua` taps the SCC's
channel A data register (`#xBFFFFF`) for the ladder's oracle:

    MODUS_MAC68K_MACHINE=classic MODUS_68K_CPU=68000 MAC68K_EMULATOR=mame \
      MAC68K_MAME=mame MAC68K_ROMPATH=ROMS scripts/mac68k-ladder.py

### Boot's live heap, measured (`MODUS_68K_GC_PEAK=1`)

The collector records the most bytes any collection kept (`#x10006080`) and
the latest (`#x10006084`).  The small 68k REPL image, booted under qemu:

| heap | boots | collections | peak kept |
|---|---|---|---|
| 64 MB | yes | 2 | 1,559,744 |
| 8 MB | yes | 24 | 1,558,672 |
| 5 MB | yes | 91 | 1,564,512 |
| 4.5 MB and below | heap exhausted | | |

Boot's live data only GROWS, to exactly what the freeze keeps (1.29 MB of
objects, 0.27 MB of conses): a never-copied old space (generational
promotion) would take the boot floor from ~5 MB to that plus a young pair.
But boot ALLOCATES on the order of 50 MB (24 collections of a 4 MB
semispace), so on an 8 MHz 68000 a boot from scratch is likely most of an
hour whatever the collector: a small Mac starts from a saved core, and with
one the static freeze already gives the small floor.  Promotion is for a long
session's growth, not for boot.

## A REPL in 4 MB (the Mac Classic map), booting from a saved core

    MODUS_MAC68K_MACHINE=classic MODUS_68K_PAGED=1 MODUS_68K_CPU=68000 \
      MODUS_SMALL_IMAGE=1 MODUS_NARROW_STRINGS=1 MODUS_68K_THREADED=1 \
      MODUS_68K_HOT=/dev/null ... sbcl --script mvm/build-mac68k-cl-repl.lisp
    scripts/mac68k-core.py IMAGE DISK          # boots it once on a 32 MB Mac

Every function threaded and paged: 804 KB resident (stubs, handlers, the
512 KB constant pool), 3.96 MB of streams on disk.  A Classic cannot boot the
image from scratch (a 5 MB heap, ~50 MB of allocation), so `mac68k-core.py`
boots the SAME binary once on Basilisk II with 32 MB -- the stub's :BOOT- heap,
code cache and stack above 4 MB -- where it freezes and sends its core over
serial; the static heap (1.28 MB of objects, 0.27 MB of conses) is moved to
`#x110000` the way a copying collection would move it (89,488 pointers), and
the disk is the boot blocks, [image, low block end) in one read, then the
streams.  The Classic map (boot-mac68k.lisp `:classic`): image `#x40000`,
static heap `#x110000`, low block `#x2B0000`, two 192 KB young semispaces,
the kind map, a 460 KB CODE CACHE, NIL's page, a 64 KB stack under the screen.

The code cache is a FIFO that never overwrites an ACTIVE stream: one a root
points into -- the caller's A5, -4(A6) of every frame on the A6 chain (every
function is threaded, so that slot is always the caller's token pointer), the
jmpbuf's and every handler frame's A5.  `MODUS_68K_PAGED_CHECK=1` re-walks the
roots after every load and stops with `!LDR-LOST` if one points into an evicted
stream; it found the one bug (a root before the place, in a stream overlapping
it).  The boot blocks first move themselves and the stack 64 KB under the top
of RAM: the ROM's boot stack is mid-RAM, inside the 2.7 MB they read.

Measured on Basilisk II (LC 475 ROM, `--ramsize 4M`, no JIT -- the relinked JIT
binary still dies at startup now and then): the core boots to the prompt in
~23 s; defun, bignums, `format ~R`, `loop` answer, the check silent.

**On a Mac Classic** (MAME's `macclasc`: a 68000 at 7.8 MHz, 4 MB), booting a
SCSI disk: `scripts/mac68k-hd.py VOLUME OUT --chd CHDMAN` puts a driver
descriptor ('ER') in block 0, our read-only driver (`scripts/mac68k-scsi.s`)
from block 1 and the boot volume from block 64.  The ROM loads the driver and
enters it with the SCSI ID in D5; it installs itself (_DrvrInstall, unit 32 +
ID, marked open), adds a locked, non-ejectable drive, and the ROM boots that
drive's boot blocks.  PRIME reads through the SCSI Manager (Get, Select, READ(6)
Cmd, RBlind, Complete).  `_SCSIDispatch` is a Toolbox trap: it may clobber
D0-D2/A0-A1 under Pascal's convention, which first read as a SCSI error of
5696.  `scripts/mac68k-mame-repl.lua` types forms when the prompt shows and taps
the SCC for what the Mac sends.  Measured, in emulated time: the prompt at
288 s from power-on, `(+ 1 2)` => 3, `(defun sq ...)` => SQ, `(sq 12)` => 144,
on the Classic's 1-bit screen.

**Where the time went, and goes** (MAME's Lua samples the 68000's PC; the
translator writes where each shared region starts, `MODUS_68K_REGIONS=FILE`):
first the stream LOADER, 70-80% of a form -- for every root it searched the
6,444-entry stream table for the stream holding it, and eviction searched it
again.  An OWNER MAP (a 16-bit word per 64-byte granule of the cache: the
stream placed there; streams 64-aligned, so a granule has one) makes both one
step.  Then the cache was too small for a DEFUN's active streams (the
interpreter, 176 KB as a stream, and the compiler's biggest pieces): the stack
and scratch moved below the image ([#x10000, #x40000) is free on a Classic),
the young semispaces to 2 x 128 KB, the cache to ~796 KB.  Measured, in a
Classic's own seconds:

| form | before | now | threaded handlers | GC | loader | ROM (SCSI, serial) | barrier |
|---|---|---|---|---|---|---|---|
| `(+ 1 2)` | 49 s | 9 s | 45% | -- | 24% | 22% | 7% |
| `(defun sq ...)` | 178 s | 53 s | 55% | 13% | 10% | 12% | 7% |
| `(sq 12)` | 177 s | 35 s | 46% | 18% | 14% | 13% | 7% |
| `format ~R`, bignums | -- | 69 s | 62% | 6% | 8% | 14% | 8% |

Not GC: interpretation.  Boot to the prompt is 262 s, three quarters of it
threaded code -- the resume's %GC-CARDS-INIT walking the 1.5 MB static heap
object by object, which the core builder could do instead.  A form still reads
~450 streams from disk: its working set is about the cache.

## Toward 1 MB: the cheap cuts

What a core-booted Classic holds, before and after (the static heap counted
object by object in the saved core; the image's sizes from the build):

| | before | now | what went |
|---|---|---|---|
| resident image | 805 KB | 711 KB | the surfaces' source (162 KB) out; literals into the pool (+68 KB) |
| code streams on disk | 3.96 MB | 3.66 MB | literals no longer built a character at a time |
| static objects | 1.28 MB | 1.04 MB | wide strings 157 KB -> 0; the 64 KB cell cache; two 64 KB buffers |
| static conses | 267 KB | 254 KB | |

- **`MODUS_NO_SURFACES`** (on for small Mac builds): no Genera / SB-* / gray
  streams / ASDF source, and `%SURFACE-OFF-P` is T.  A Mac has no environment
  to switch them off in, and no sockets or threads for them to wrap.
- **Narrow literals** (`*NARROW-LITERALS-P*`, as the x64 CLI's
  `MODUS_NARROW_STRINGS`): pooled once, a byte a character, `%FRESH-STRING`'d.
  Not 32-bit (#x15) strings, whose accessors assume the 64-bit object layout
  (`*STRING32-P*` / `%STRING32-STRINGS-P`).
- **`%BASE-SUBSEQ-P`**: `SUBSEQ` of a narrow string is narrow.  Boot cuts every
  name in the symbol, function and global tables out of narrow packed text.
- **`MODUS_SMALL_IMAGE`**: the global-cell cache 1024 slots (597 used, of
  16384), mvm-eval's buffers start at 4 KB (they grow), no opcode descriptions.
- **No wide strings at all** (`*BASE-STRINGS-ONLY*` / `%BASE-STRINGS-ONLY-P`,
  on for the Mac; `MODUS_68K_WIDE_STRINGS=1` keeps them): every fresh string
  is narrow, a byte a character, and a character above 255 cannot be stored in
  one (it signals; nothing truncates).  Mac text is 8-bit.  The core's wide
  strings 157 KB -> 0, and a form's strings take a quarter of the young heap.

**What is left, and where the next cuts are.**  The resident image is 219 KB
of native code (stubs, handlers, the stream table) and ~490 KB of pool, of
which ~270 KB is the BOOT TABLES' packed text (symbol names 152 KB, function
names 76 KB, global names 16 KB, keyword lists 12 KB) plus the runtime macros'
source -- read once, by boot.  A core-booted Mac never reads them: an init-only
section at the end of the image that the core disk leaves off is the next
~300 KB.  In the static heap: 21,000 narrow strings (419 KB, names, many
duplicates), 351 KB of hash-table storage sized for growth (the dict of a
12,232-entry table is a 64 KB index + 135 KB of entries; a freeze-time compact
to the live count is ~60 KB), and 254 KB of conses.

## What a session touches, and swapping the rest

**Measured** (MAME's Lua read and write taps over the whole static heap, a
typical session of eleven forms -- arithmetic, DEFUN and a call, FORMAT ~R,
DOTIMES, MAPCAR, the printer, HANDLER-CASE, an undefined function, INTERN,
DEFSTRUCT -- each 8-byte granule mapped back to its object in the saved core):
the forms touch **170 KB of the 1.29 MB**.  Boot touches every object (the
resume's %GC-CARDS-INIT reads every header).  Cold, by owner (each global's
value, keyed by the compiler's name hash): `*SYM-NAME-TABLE*` 430 KB,
`*KERNEL-FN-NAMES*` 143 KB, the package system 124 KB, the pinned tables
124 KB, `*MACRO-PKG-TABLE*` 82 KB, `*MVM-EVAL-CACHE*` 66 KB,
`*KERNEL-GLOBAL-NAMES*` 33 KB.

A 68000 has no MMU, so there is no page fault to catch: data comes off the
disk where it is ASKED FOR, through a function, as the code streams do.
**Cold names** (`mvm/mac68k-cold.lisp`) are the first: the build writes the
12,247 names, sorted by hash, to 512-byte pages (`IMAGE.cold`, 203 KB);
`mac68k-core.py` puts them on the disk after the streams; the core run drops
`*SYM-NAME-TABLE*`; and `SYMBOL-NAME`, where the table would have answered,
asks `%COLD-SYM-NAME` -- a binary search of the pages' first hashes (in the
image), ONE block read (`SYSCALL3` service 1 on a bare Mac, the code loader's
`_Read`), the name kept.  The freeze also empties `*MVM-EVAL-CACHE*`, runs
`COMPACT-NAMES` (5,000 duplicate strings), and clears the closure-environment
slot, which was holding a 400 KB table live.

| | before | now |
|---|---|---|
| static objects | 1.04 MB | 0.89 MB |
| static conses | 254 KB | 222 KB |
| prompt on MAME's Classic | 254 s | 224 s |

Next, in both directions: the kernel-name sets and the macro table are
consulted only at DEFUN / DEFMACRO / global-definition time -- the next cold
customers, or replaced by a test on what is already resident.

## Shrinking what stays, and a code cache that can evict anything

**The core's own collection** (`mac68k-core.py`): a core boot's only roots are
the low block's words, so the builder keeps what they reach and compacts it --
326 KB the freeze had kept live through a stale stack word went.  **Hash
tables**: the symbol-function table is a compact dict now -- keys and values
packed -- and a cached `(name . function)` pair is made on demand
(`%GETHASH-PAIR`'s side table), not three conses an entry for every name;
pinned bucket vectors are right-sized at the freeze.  **Packages**: a
package's symbols are an EQUAL hash table, not an alist with a derived index.
The saved heap: 1.12 MB -> 657 KB.  (A table that grows is NOT right-sized: a
doubled 2,400-entry dict does not fit the Classic's 128 KB young semispace.)

**Active streams are evictable.**  The code cache used to stop (`!LDR-FULL`)
when the streams on the call chain outgrew it -- three DO-EXTERNAL-SYMBOLS in
one form did.  Now FORCE takes a place anyway and rewrites every exact root
into what it evicts (the caller's A5, -4 of each frame, the jmpbuf's and
handler frames' A5) to a RELOAD RECORD: a token, the stream, the offset.
Returning to one reloads the stream wherever it fits and goes on.  So the
cache's size is now a speed choice, not a ceiling; its floor is the largest
stream (the interpreter, ~176 KB).  `MODUS_MAC68K_CACHE_KB=N` sets it.
Measured on MAME's Classic (68000, SCSI):

| | 796 KB cache | 192 KB cache |
|---|---|---|
| prompt | 224 s | 179 s |
| `(+ 1 2)` | 23 s | 28 s |
| `defmacro` | 207 s | 316 s |

The 4 MB Classic's map now holds ~1.77 MB: image 470 KB, saved heap 657 KB,
low block 128 KB, young heap 256 KB, cache 192 KB, stack and buffers ~70 KB.

## Toward 1 MB: what the saved core and the image hold now

Measured on MAME's 2 MB Classic (`MODUS_MAC68K_MACHINE=classic2m`), same
REPL forms throughout, every answer right:

| | before | now | how |
|---|---|---|---|
| code cache floor | 196 KB (the interpreter's stream) | none (64 KB runs) | **chunked streams**: a cold function over `MODUS_68K_CHUNK_KB` (default 4) is several streams; a cross-chunk branch goes through an XJUMP trampoline, which is the RELOAD token over a record-shaped operand |
| resident image | 607 KB | 443 KB | no NFN table (nothing reads it); CL's runtime macro texts and the boot-only name lists init-only; the constant pool coalesced (4,921 strings, 2,773 distinct) and 8-aligned; no ANSI-test helpers or stage-1/2 probes |
| saved heap | 657 KB | 400 KB | **cold functions**: the function table's image entries (4,574 names) and `*KERNEL-FN-NAMES*` go to disk at the freeze, as hash-sorted pages; a miss in either table reads one block and keeps what it found (`%HT-COLD-MISS`, `mac68k-cold.lisp`) |
| low block | 128 KB | 56 KB | the layout's `:ALLOC-START`; the 72 KB went to the young semispaces (64 -> 100 KB) |
| prompt | 74 s | 55 s | less to read at boot |

`MAC68K_CORE_CENSUS=1 scripts/mac68k-core.py` prints the saved heap by subtag.
What is left in it is live: symbols and their intern tables (~130 KB), the
package tables, the bytecode of the 47 runtime macros boot compiles (56 KB),
dict indexes, conses (60 KB).

### The REPL's code working set is the wall

Profiled with a write tap on the stream table (every load) and a PC sampler:
one `(length (let ((l nil)) (dotimes (i 5) (push (intern (format nil "S~D" i)) l)) l))`
touches **732 streams, 830 KB of threaded code** -- the interpreter (125 KB of
it), `COMPILE-COMPOUND` (96 KB), `EMIT-BYTECODE-FOR-IR` (65 KB), `COMPILE-CALL`,
`TYPEP`, `%FORMAT-IMPL` ... every REPL form is compiled by the in-image compiler
and run by the interpreter.  With a 64 KB cache that form costs ~10,000 stream
loads, and 69% of its time is the loader and the ROM's disk driver:

| chunk size, 64 KB cache | DOTIMES form | DEFUN | DEFMACRO | LET + macro |
|---|---|---|---|---|
| 16 KB | 452 s | 70 s | 253 s | 127 s |
| 4 KB | 239 s | 62 s | 239 s | 89 s |

An LRU simulation over that form's load trace still misses ~6,500 times at
64 KB, ~1,900 at 192 KB and ~1,200 at 256 KB.  So a 1 MB Mac can be made to
*fit* (image + saved heap need to reach ~600 KB, from 843 KB: the 1 MB hot list
`mvm/68k-hot-mac-1m.txt` is -100 KB, stub/table slots ~-50 KB), but with the
cache it can afford every non-trivial form is minutes of paging -- and a real
SCSI disk's seek time would make each of those thousands of reads dearer than
MAME's.  What would change that is a smaller REPL path, not a smaller image.

## A REPL in 1 MB (Mac Plus): `MODUS_MAC68K_MACHINE=plus1m`

MAME `macplus -ramsize 1M`, our boot blocks and SCSI driver, booting from the
saved core: the prompt at 54 s, and `(+ 1 2)`, DEFUN, a call, FORMAT ~R,
DEFMACRO and its use, DOTIMES/PUSH/INTERN all right.  Build with an empty
`MODUS_68K_HOT` (no hot natives).

| range | what |
|---|---|
| `#x10000`-`#x18000` | stack |
| `#x18000`-`#x1C000` | I/O scratch pages |
| `#x1C000`-`#x3E000` | code cache (136 KB) |
| `#x3E000`-`#x40000` | NIL's page and T's |
| `#x40000`-`#x8E000` | resident image (~318 KB) |
| `#x8E000`-`#xD0000` | saved heap (268 KB) |
| `#xD1000`-`#xF5000` | low block (56 KB), young semispaces 2 x 40 KB, 8 KB guard |
| `#xF5000`-`#xF9800` | kind map |
| `#xFA700`- | the screen |

What it took, beyond the earlier rounds: the init-only text LINKED at 4 MB
(`*POOL-INIT-BASE*`) for the core run, where this map's region would sit on it;
CL's runtime macros compiled into the image (-132 KB of saved heap); and the
serial REPL's input buffer growing from 512 characters instead of a fixed
16 KB string -- found by dumping RAM at "heap exhausted" and walking the young
heap through the kind map.  Measured: 20 KB semispaces exhaust before the
prompt, 32 KB at the first DEFUN, 40 KB with the 16 KB buffer at DEFMACRO.

It is slow: seconds for a call, minutes for DEFMACRO or a loop.  Every REPL
form is compiled in the image and run by the bytecode interpreter, and their
working set pages through a 136 KB cache (see "The REPL's code working set is
the wall" above).

### With the threaded JIT: `MODUS_MAC68K_MACHINE=plus1mjit MODUS_68K_JIT=1`

REPL DEFUNs translated to threaded code (docs/68k-threaded-jit.md).  The JIT's
shape index (8 KB) and the handlers it adds (79) do not fit plus1m's resident
range (2.9 KB free), so this map moves the image 16 KB down and takes a code
area from the cache:

| range | what |
|---|---|
| `#x1C000`-`#x36000` | code cache (104 KB) |
| `#x36000`-`#x3A000` | the JIT's code area (16 KB) |
| `#x3A000`-`#x3C000` | NIL's page and T's (no lower: the core run's LC ROM keeps its system heap to `#x39FE0`) |
| `#x3C000`-`#x8E000` | resident image |

A translation pages the compiler and the JIT through the 104 KB cache: with
every DEFUN translated, a DEFUN took 196-315 s.  So here DEFUN does not
translate (the layout's `:jit-on-defun nil`) and `(compile 'f)` does.  MAME
`macplus -ramsize 1M`:

A session, in the order typed (each form's time to the next prompt; the
second column is the same image before the 2026-11-14 round below):

| type | time | before |
|---|---|---|
| `(defun sq (x) (* x x))` (the first form) | 46 s | 62 s |
| `(defun greet (name) (format nil "Hello, ~A!" name))` | 38 s | 53 s |
| `(greet "Mac")` | 10 s | 9 s |
| `(+ 1 2)` | 4 s | 5 s |
| `(defun fib (n) ...)` | 96 s | 111 s |
| `(compile 'fib)` | 62 s | 105 s |
| `(fib 20)` | 19 s | heap exhausted |
| `(defun fact ...)`, `(compile 'fact)`, `(fact 10)` | 74 s, 60 s, 5 s | |
| `(defun tri (n) ...)`, `(tri 20)` interpreted | 72 s, 54 s | |
| `(defmacro twice (x) ...)`, its use in a LET | 347 s, 66 s | |
| `(compile 'tri)`, `(tri 100)` | 49 s, 4 s | |
| `(mapcar #'sq (list 1 2 3))` | 50 s | |

Built with `MODUS_68K_HOT=mvm/68k-hot-mac-plus.txt`: one native function,
`%GV-REF-FILL` (2 KB of the 5.5 KB the resident range has free), which every
special-variable read of a small image calls.

**What a DEFUN was spending, found by sampling the Lisp call stack** (MAME
Lua: PC, the threaded IP, and the frame chain's return addresses mapped to
streams through the cache's owner map; `greet`, 53 s).  None of it is the
Mac's: it is in the shared evaluator and compiler, where a workstation hides
it (x64, 3000 DEFUNs through EVAL: 1360 ms before, 1050 ms after; the ANSI
suite 21680/21681 both sides, the same test failing).

| was | what | now |
|---|---|---|
| 9% | `BOUNDP`: is the symbol's name a kernel global's (a string hash), then is its package a kernel package (STRING= against each) | the package first, by EQ (`%KERNEL-HOME-PKG-P`) |
| 7% | the 94 bootstrap macros copied into a fresh `*MACRO-TABLE*` at every EVAL | the fresh table overlays them (`%MACRO-TABLE-REF`) |
| 5% | every called name looked up in the kernel's function names -- on a Mac a read of the cold functions on disk | only for a symbol of a non-kernel package |
| 6% | `TAIL-FORM-IS-VALUES-P` building a list of 53 strings to MEMBER the operator's name in (x64: 3 KB and 5 us a call) | by hash, as its other arms |
| 3% | the arity check's function name compiled through COMPILE-FORM: a macroexpansion of `(QUOTE NAME)` per function | `COMPILE-QUOTE` |

Also in that round, for the heap: the compile's IR and expansion memo are
dropped before the form runs; `*MVM-EVAL-CACHE*` has a bound (4096 forms; 0
here -- it kept every form evaluated, compiled, for the life of the process);
the JIT's permutation candidates are read from the shape table, not collected
into a hash table that a 40 KB young semispace could not hold (the "heap
exhausted" above).  And the cold names' index is searched where it is (15 s
of the first `(compile 'f)`: 403 PARSE-INTEGERs through the bignum reader).

**What is left of a DEFUN** (`greet`, 38 s): the disk.  The SCSI transfer in
the ROM is ~33% and our loader ~15%; Lisp is a third.  One DEFUN calls 616
distinct functions, 664 KB of threaded code, through the 104 KB cache: 1806
stream loads, 1.7 MB read.  Two functions are a quarter of it --
`EMIT-BYTECODE-FOR-IR` (65 KB, 339 KB read) and `COMPILE-COMPOUND` (95 KB,
114 KB read): each is one long chain of tests, walked from the top for every
instruction or form, so a late arm pages the whole function in.  A simulation
over the recorded references: a clock policy in place of the FIFO would save
~20% of the loads, 24 KB more cache ~15%.  DEFMACRO compiles its expander in
nested EVALs (347 s).

Modules over 1 KB of bytecode (every DEFSTRUCT) stay interpreted.

## Launched from Mac OS: `MODUS_MAC68K_MACHINE=classicos` + `scripts/mac68k-chain.py`

No floppy and no disk of our own: a Mac application, "Modus", whose data fork
IS the boot disk `mac68k-core.py` writes.  Double-clicked (or set as the
startup application), it opens its own data fork, reads the image to its
address, stores the open file's reference number in the image's static block
(`#x10006050`, `+68K-MAC-FILE-REFNUM+`) and jumps.  The image runs on top of
Mac OS as it runs on top of the ROM -- low memory, the system heap, drivers,
interrupt handlers kept -- and its code loader and disk service `_Read` that
file instead of the boot drive, so the File Manager follows the file's
extents and nothing is written to the Mac's disk.  One file to copy
(MacBinary II); restart to get back to the Finder.

- **The map** (`:CLASSIC-OS`): the 4 MB Classic map with nothing under the
  image.  Measured on MAME, System 6.0.8 with no INITs: the system heap
  `#x1400`-`#x24270`, the application zone from `#x2427C`.  The stack and the
  I/O scratch pages move from `#x10000` to under NIL's page.  The application
  refuses, on the screen, if MemTop is short of the map, the system heap
  reaches the image, or its own code or stack is where the image goes.
- **The boot blocks carry a record** for it: `modusCHN`, then DEST ENTRY LEN
  OFF SPOFF SPLEN REFSLOT MEMTOP; REFSLOT is 0 for a map that cannot run under
  Mac OS.  The application draws the splash from SPOFF/SPLEN too.
- **Measured**, MAME `macclasc -ramsize 4M`, System 6.0.8 on our SCSI driver,
  Modus as the startup application: the prompt at 61-73 s, (+ 1 2), DEFUN, a
  call; at 2 MB the refusal on the screen.  The same image booted from its own
  disk: prompt at 60 s, DEFUN 25 s (the 1 MB Plus: 62 s).
- **Our SCSI driver now reads any position and count** (a partial block
  through a bounce buffer).  It rounded every request up to whole blocks --
  modus's own readers never noticed -- and System 6 booted from it crashed in 6
  boots of 8, its reads overrunning their buffers; 6 of 6 after.
