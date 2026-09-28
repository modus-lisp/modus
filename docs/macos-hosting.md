# Hosted macOS (and the road to iOS)

Written 2026-09-26.  Status: **M1 in progress** — the runtime-data region, heap and JIT arena move (option B, verified on aarch64 Linux); the code VA does not yet.

## Goal

A native `modus` CLI on macOS/arm64 — a Mach-O that runs on the Mac, not the
Linux ELF under OrbStack or a bare-metal image under QEMU (which is how modus
"runs on the Mac" today; every modus binary here is ELF).  macOS is also the
stepping stone to iOS: the same Mach-O, x18, address-layout and syscall work,
with iOS then adding "no runtime codegen" on top.

## What exists to build on

Hosted Linux/AArch64 (`mvm/build-aarch64-cli.lisp`, shared
`build-cli-common.lisp`) isolates the OS well:

- `*aarch64-linux-mode*` switches the OS-dependent traps (console write
  0x0300, sys-exit 0x0500) from bare-metal UART to Linux syscalls.
- The runtime issues syscalls by **x86-64 number**; each target remaps.
  `*aarch64-linux-syscall-remap*` (translate-aarch64.lisp) is therefore the
  complete list of syscalls hosted AArch64 makes — 18 of them.
- Native code reaches the runtime's convention block through a **base
  register** (x18 = `#x10000000`): `a64-load-imm64` folds any address in
  `[#x10000000, #x10001000)` into `add xd, x18, #off`.
- **Load-address agnosticism already exists — for the RPi CL image
  (`4a03744`).**  Not position-independent code: the translator still writes
  absolute addresses (function pointers, code bounds, the constant pool,
  veneers, the GC trampoline, absolute intra-module calls).  Instead it copies
  Linux's `head.S`: the image is linked once at a fixed virtual address
  (`:code-vaddr-base`, `0x30000000`, read by `mvm/cross.lisp`), and the boot
  stub's first instruction (`adr x20,#0`) records where it actually loaded,
  maps `[0x30000000, +128MB)` onto that with the MMU, and jumps to the entry
  at its virtual address.  One image boots at both `0x80000` and `0x300000`.
  Only `boot/boot-rpi-cl.lisp` sets the field; without it every other image
  builds byte-identically to before.

## Measured on this Mac (macOS 26.6.2, arm64) — probes in C, not guesses

| probe | result | consequence |
|---|---|---|
| RW `MAP_FIXED` at `0x10000000` | ENOMEM | inside the 4 GB `__PAGEZERO` |
| link with `-pagezero_size 0x4000` | **SIGKILL at launch** | arm64 macOS enforces a 4 GB `__PAGEZERO`; no escape hatch |
| RW `MAP_FIXED` at 8 / 63 / 64 / 128 GB | EPERM | reserved ranges — they vary by OS and hardware |
| RW `MAP_FIXED` at 32 / 60 / 448 GB | OK | usable today, but not something to depend on |
| RWX, no `MAP_JIT` | EPERM | W^X enforced |
| RWX `MAP_JIT` + `MAP_FIXED` | EINVAL | a JIT arena cannot sit at a fixed address |
| RWX `MAP_JIT`, unfixed, `pthread_jit_write_protect_np` toggles | wrote `mov x0,#42; ret`, returned 42 | the JIT can stay on for macOS bringup |
| unhinted `mmap` | `0x102b30000` | the kernel places memory wherever it likes |
| **`mach_vm_remap` own signed code → `0x7000000000`** | **`r-x`, call through the new address returned 42** | **the RPi `head.S` trick has a hosted equivalent** |
| `mach_vm_remap` own code → `0x800000000` | "no space" in that process (a fresh process could map it RW) | the reserved map shifts; reserve early, check, fail clearly |
| `mach_vm_remap` own code → `0x30000000` (the RPi link VA) | invalid address | anything below 4 GB is `__PAGEZERO` |

## The central problem: modus is a fixed-address program

- The hosted CLI is `ET_EXEC` at `0x400000` (entry `0x400078`); x64 is the same.
- The runtime's own Lisp source names fixed addresses:
  `(mem-ref #x10000208 …)` (argv), `#x10000200` (argc), `#x10000180`,
  `#x10000190`, and `#x10010000` (the handler-frame stack) — the last is
  **outside** the 4 KB fold window, so it compiles to an absolute immediate.
- The AArch64 heap base is fixed (`0x2000000000`) for save-and-die, and the
  JIT arena is one `PROT_RWX` mapping holding code with absolute targets.
- arm64 Mach-O executables are always PIE and slid by ASLR.

**We do not need true PIC for macOS.**  The RPi approach carries over: link
the code at a fixed virtual address, and at startup map it there — with
`mach_vm_remap` of the process's own signed pages instead of the MMU, which
the probe above shows keeps them executable.  Absolute addresses in the code
then resolve exactly as they do on the Pi.

What does NOT carry over is the **data** base: `0x10000000` is inside
`__PAGEZERO`, and no mapping of any kind is allowed there.  The convention
block / BSS, and the `#x1000…` literals that name it in runtime Lisp source,
have to move above 4 GB.

### Inventory of the data side (2026-09-26, regex estimates — not an audit)

Sub-4 GB literals in the 41 runtime files the aarch64 CLI bakes in, plus its
build scripts: **534 uses, 146 distinct values, 27 files.**  But a literal in
`[0x10000000, 0x20000000)` is not necessarily an address: A64 opcodes live in
exactly that range (`ADR` = `0x10000000`, `B` = `0x14000000`, LDR-literal =
`0x18000000`), and the translators — which are compiled into the image too —
use them as instruction encodings.  So no rule can fold "every value in the
region".

The fold already had to learn this.  After the Pi bug (a tagged fixnum
`#x08000000` has machine word `#x10000000`; it was folded, and the GC's stack
scan window came out empty), folding moved out of `a64-load-imm64` into
**`a64-load-conv-addr`, which a caller uses to ASSERT it is naming a
convention slot**; everything else is plain `MOVZ`/`MOVK`.  Its docstring says
a missed call site "costs two instructions, never correctness" — true only
while the base IS `0x10000000`.  **Once the base moves, a missed call site is a
correctness bug.**  M1's invariant flips from "folding is an optimisation" to
"every runtime-data address is base-relative".

### Moving the data region: option B, a link-time delta (2026-09-26)

Two ways were on the table: a runtime base register (A), or a link-time
constant (B).  **B was chosen**: arm64 macOS has no spare register (x18 is
Apple's; x19–x28 are all taken — vreg homes, alloc pointer/limit, NIL, closure
env, GC trampoline VA), and the code already needs a fixed VA because it is not
PIC, so fixing the data VA is the same bet.  A remains the route if iOS forces
true PIC.

The region `[0x10000000, 0x40000000)` has a **virtual** address (what the
source says) and a **real** one: real = virtual + `*conv-delta*`, a build
constant (`MODUS_CONV_DELTA`, hex; default 0 = unmoved).  `conv-real` maps one
to the other.  Every kind of reference is rebased at compile or translate time:

- **`mem-ref` operands the compiler can prove** — a constant (literal or
  `defconstant`), or constant + index.  `compile-mem-ref` / `compile-setf`
  compile the rebased form; the access also carries `+width-conv-bit+` and is
  recorded by `MODUS_CONV_AUDIT`.  Gated by `*conv-relative*`.
- **Addresses used as values** — `(%conv-addr K)` in runtime source: returned
  from accessor defuns, passed to `%gc-read64` / `%core-slice` /
  `%gc-forward-slot`, stored in defvars (the file-I/O buffers).  Substituted by
  `%conv-subst` before a top-level form is compiled, so the compiler's
  source-shape analyses see the literal (without that, `%gc-region-0` lost its
  MV-count store).  About 70 sites across gc, save-image, mvm-eval, prelude,
  cl-fileio, mcgc-pin, interp and the hosted net files — including files the
  CLI bakes as SOURCE TEXT and evaluates at boot (`net/cooperative-atomics.lisp`
  and the sb-* shims), which a scan of the compiled file list misses.  The
  pre-pass runs on every form the image evaluates, so it hashes each list head
  once against read-time constants: written with two `name-eq`s it cost 2.7 MB
  of boot allocation, now 0.4 MB (0.8%).
- **Compiler-emitted IR** — the MV buffer, the global-cell cache root, the
  binding gate, the handler-active check: `conv-real` at the `:li`.
- **Translator** — x18 holds the region's REAL base, so the fold window stays
  one `add`; everything outside it (`a64-load-conv-addr`'s other arm,
  `a64-load-imm64-general` sites, the GC trampoline's roots, seven hand-split
  MOVZ/MOVK pairs, the constvec root, the x18 reload after longjmp) goes
  through `conv-real`.
- **Boot + ELF** — when moved, the ELF segment ends at `0x0F000000` and the
  boot stub maps the region at its real base (`MAP_FIXED_NOREPLACE`, exit 97
  on refusal) before its first store.  The argv copy blocks now patch their
  own skip branch, since a high address takes more than two words.
- **Cross-emit** (`--compile`, `--compile-aarch64`) resets both variables:
  the images it writes are ordinary, unmoved Linux ELFs.

**Verified.**  Delta 0 is neutral: the aarch64 and x64 CLIs are per-function
identical to the pre-change build for every runtime function — only the
compiler, translator and boot functions that were edited differ, and every
other difference is a branch, `movabs` or RIP-relative displacement.  With
`MODUS_CONV_DELTA=7000000000` the region lives at `0x7010000000` with **nothing
mapped at the old address**, so a missed site faults rather than passing by
luck; the smoke, stress (17 collections, file I/O, CLOS, restarts, hash
tables) and GC-count probes give identical output to the baseline.

**Differential testing** is the gate for this: every `test/*.lisp` runs under
the baseline image and the moved one, and the outputs are compared.  A
difference is either a missed site or a test that prints an address.  The
first run found the source-baked `%atomics-lock-addr` (via `hosted-atomics`).
Final run: **67 of 72 byte-identical**; the other five differ only in printed
addresses — `hosted-atomics` (the lock word) and the four `region-gc*` tests
(control-block address, address-bearing checksums) — with identical `ok`
counts, and `region-gc`'s one FAIL is on the baseline too.  (Run the tests
with stdin from `/dev/null`: a REPL that inherits a pipe eats the rest of it.)  Cross-emit from the moved image writes
x64 and aarch64 ELFs byte-identical to the baseline's.

**The heap and the JIT arena** are the layout's other two fixed mappings
(save-and-die restores both in place).  They are build-time layout constants
too: `MODUS_HEAP_BASE` / `MODUS_JIT_ARENA_BASE` (hex; defaults `0x2000000000`
/ `0x3000000000`, which macOS may refuse — 128 GB was EPERM in the probe) set
`*hosted-layout*`, read by the boot stub's two `MAP_FIXED_NOREPLACE` calls and,
through `(%layout :jit-arena-base …)`, by save-image.  The runtime otherwise
reads both dynamically (`%gc-from-start`, the bump word).

A macOS-shaped layout, all inside the 448 GB band the probe allowed, runs on
Linux: region `0x7010000000`, heap `0x7040000000`, arena `0x7080000000`
(`MODUS_CONV_DELTA=7000000000 MODUS_HEAP_BASE=7040000000
MODUS_JIT_ARENA_BASE=7080000000`).  Smoke/stress/GC probes match the baseline;
a core saved and restored in a fresh process carries a JIT'd function, a
closure, an `equal` table and a CLOS method, and keeps collecting; a core from
a different layout is refused.  The default layout is per-function neutral.

**The code** is the last fixed mapping: `MODUS_CODE_BASE` (hex, layout key
`:code-base`; default `0x400000`).  Linux maps an `ET_EXEC` at its `p_vaddr`,
so on Linux this is only a link-time change — no remap stub.  Two things had
to follow it:

- **Code addresses above 4 GB.**  Every placeholder the cross-linker patches
  with a code address (function addresses, code bounds, the x28 trampoline VA,
  the handler-helper VAs) was a MOVZ+MOVK pair — 32 bits.  They now come from
  one helper that adds a third MOVK when the code base is high
  (`*a64-code-addr-wide*`), and one patcher that fills however many halfwords
  follow and **fails the build** when an address does not fit — the link-time
  counterpart of leaving the old region unmapped.
- **The BSS tail below the region.**  Restore staged the metadata window at
  `0x0FF00000` and hosted-storage parks `*block-scratch*` at `0x0FC00000`: both
  in the ELF's BSS tail, which exists only because the image is linked at
  `0x400000`.  The moving range now starts at `0x0F000000`
  (`+conv-region-low+`) so they move with the region at their old virtual
  addresses (restore failed with `core: short read` until they did).

A code base above the old region requires the region moved, and above 4 GB
requires the wide placeholders; both are build-time errors, as is code that
would overlap the moved region.  That last check fired on the ANSI gate: its
image is 264 MB (301 MB without x18), and the layout below left the code only
240 MB.  **The recommended layout gives the code 1.2 GB:**

```
MODUS_CODE_BASE=7000000000       code          0x7000000000
MODUS_CONV_DELTA=7040000000      region        0x704F000000 .. 0x7080000000
MODUS_HEAP_BASE=7080000000       heap          0x7080000000 .. 0x70F1000000
MODUS_JIT_ARENA_BASE=7100000000  JIT arena     0x7100000000 .. 0x7120000000
```

The heap is 896 MB without threads and 1808 MB with them (the CLI's
default; see "Native threads on AArch64"), so the arena sits above the larger.
`check-layout-overlaps` refuses a layout whose heap runs into the arena.

All knobs live in `mvm/hosted-layout-env.lisp`, loaded by both the CLI and the
ANSI gate builds (`build-cli-common`, `build-ansi-common`), which apply them
host-side and splice the same values into their JIT co-init.

**ANSI gate: run, 2026-09-27.**  aarch64 hosted runner (`build-aarch64-linux`,
JIT on), upstream ansi-test `ca06bd9` in `get-ansi-corpus.sh`'s layout, run
on Linux/aarch64 under Apple `container`, 12 shards.  The runner needed
`(init-all-globals)` in its `kernel-main` to boot at all (a `main` bug,
reported upstream); both images carry that one line.

| layout (code 0x7000000000, region delta 0x7040000000) | pass | fail | lost |
|---|---|---|---|
| with x18 | 18,855 | 1,357 | 1,321 |
| **without x18** (`MODUS_NO_X18`, x18 poisoned) | 18,854 | 1,358 | 1,321 |

Of 21,533 ids.  One test differs, and it is not an x18 defect: 25396
(`format.f.38`) belongs to a pre-existing, nondeterministic class of about 27
tests in BOTH runs that return garbage objects (`#<?NN>`) where floats should
be (25361 and 25362 fail in both, with different garbage each run; 25392
passed in both full runs and failed under gdb).  With the JIT off, 25396 fails
10/10 on the x18 image too.  Hybrid builds that move only the nargs slot back
to x18 make it pass, which changes code size and timing, not semantics: the
fallback's address is identical, no JIT page faults, and moving its scratch
from x17 to x9 changes nothing.  Shard timings match between the two images.

The gate also found two more missed sites, both fixed: its long-range entry
jump to KERNEL-MAIN patched only 32 bits, and its `kernel-main` set its
scratch buffers from literal addresses in the build script's source string.
The low layout cannot hold this image (273 MB from 0x400000 overruns
0x10000000; the build now warns), which is why both runs link high.

The full macOS-shaped layout runs on Linux — `/proc/self/maps` of a live
image, nothing of modus below 4 GB:

```
7000000000-7003e46000 rwxp  code (the ELF)
700f000000-7040000000 rw-p  runtime-data region (virtual 0x0F000000–0x40000000)
7040000000-7078000000 rw-p  heap
7080000000-70a0000000 rwxp  JIT arena
```

(`MODUS_CODE_BASE=7000000000 MODUS_CONV_DELTA=7000000000
MODUS_HEAP_BASE=7040000000 MODUS_JIT_ARENA_BASE=7080000000`.)  Smoke, stress,
GC, save/restore, `functionp`, and cross-emit (byte-identical x64 and aarch64
ELFs) all match; `test/*.lisp` 67/72 byte-identical, the other five differ
only in printed addresses.  The default layout is per-function neutral.
x18 is still the base register on Linux; Darwin runs without it (see Darwin
differences).
The interpreter honours the move for its simulated MV buffer; it still
masks `+width-conv-bit+`, which is correct because addresses arrive real.

True PIC remains the fallback if iOS forbids the remap or its address-space
limit leaves no room for a fixed VA.

## Darwin differences the port must absorb

- **x18 is zeroed by the kernel — on macOS too, not only iOS.**  Measured on
  this Mac (a C probe parks 0xDEADBEEF in x18 and spins under 1 kHz signals
  and 16 competing threads): 214 of 3000 rounds came back 0.  So a Darwin
  image may not depend on x18 at all.  There is no other register to spare
  (x19–x28 are all taken), so on Darwin the convention base is not a register:
  `*a64-x18-base*` off, and every slot address is loaded as an immediate.

  Measured cost of that (fully moved image, x18 vs none, on Linux): code
  +13% (65.5 → 74.2 MB); `sort` / `intern` / hash tables +3–5%; reader and
  printer within noise; call/MV/special/handler-case/allocation loops
  +1–2.5%.  A cheaper Darwin option exists but is unbuilt: code and region sit
  at fixed VAs within ±4 GB, so `ADRP`+`ADD`/`LDR` reaches a slot in two
  instructions with no register (vs one with x18, four as an immediate).
  Linux and bare metal keep x18: it is free there and fastest.

  The native GC trampoline used x18 as its Cheney slot cursor — preserved
  across calls, and the one use a zeroing kernel would actually break (a
  cursor reset mid-object scans from address 0).  It now uses x23, whose
  value (stack_base) is dead once the root window is scanned, and no longer
  saves or restores x18.  The longjmp path reloads x18 only when it is the
  base.

  **`MODUS_NO_X18=1` runs the Darwin discipline on Linux**: no x18 base (host
  and JIT co-init), and the boot stub poisons x18 with the non-canonical
  `0x0018DEAD0018DEAD`, so anything still depending on it faults here
  instead of intermittently on a Mac.
- **Syscall convention**: `svc #0x80`, number in **x16** (Linux: `svc #0`,
  x8).  Errors set the **carry flag** and return a **positive** errno; Linux
  returns `-errno`.  The trap must `b.cc` past a `neg x0, x0` so the runtime
  keeps seeing `-errno` — otherwise every failed syscall reads as success.
- **Numbers** (from the SDK's `sys/syscall.h`, x64 → Darwin): read 0→3,
  write 1→4, close 3→6, fstat 5→189, lseek 8→199, mmap 9→197, getpid 39→20,
  socket 41→97, connect 42→98, accept 43→30, bind 49→104, listen 50→106,
  exit 60→1, ioctl 16→54.  Different semantics: exit_group 231→exit (1),
  getdents64 217→getdirentries64 (344, different record), clock_gettime
  228→gettimeofday (116) or the commpage.  None: perf_event_open 298 (drop).
- **Constants and structs**: `MAP_ANON` is `0x1000` (Linux `0x20`); `stat`
  layout differs; `sockaddr` carries a leading `sin_len` byte and `AF_INET6`
  is 30; `SOL_SOCKET` is `0xffff`.
- **Linking**: let Apple's `ld` produce the executable — dyld, code signing
  (arm64 kernels kill unsigned code) and later Xcode integration come free.
  Modus emits code and data; a tiny shim provides the entry point.

## Milestones

**M0 — native on macOS: DONE (2026-09-27).**  `host/macos/`: the Linux
aarch64 ELF, built with `MODUS_DARWIN=1`, embedded in a signed Mach-O that
`mach_vm_remap`s it to its link address and calls it through a syscall stub.
Natively on this Mac: `--eval`, `--load`, the smoke/stress/GC probes (200
collections, file I/O, CLOS, conditions, restarts) and save-and-die /
`--core` restore all give the same results as on Linux.  See *Running
natively* below.

*The plan as first written:* **M0 — Darwin trap spike (small).**  A `*aarch64-darwin-mode*` beside the
Linux one: `svc #0x80`, x16, carry→negate, the remap table.  Compile a
trivial MVM program (no runtime) that writes and exits, link it with Apple
`ld`, run it on macOS.  Proves the toolchain path and the syscall ABI in
isolation.

**M1 — move modus above 4 GB, on aarch64 LINUX.**  *Data region, heap
and JIT arena: done (option B above).  Remaining: code VA, x18.*  The platform-neutral part
of the move, done where everything else works and the existing gates can
prove nothing broke.  Linux loads an `ET_EXEC` at its link address, so no
remap is needed there yet:
- inventory every fixed address below 4 GB: the `#x1000…` literals in
  runtime Lisp source, `+a64-conv-base+`, the GC metadata constants, the
  handler-frame stack at `#x10010000`, the `0x400000` load address;
- move the runtime data base above 4 GB and rebuild from one constant;
- link the code at a high VA with `:code-vaddr-base` (e.g. `0x7000000000`);
- move the base register off x18 (Darwin reserves it).
Done when the hosted aarch64 CLI runs under OrbStack at the new addresses and
the ANSI gate / glass witness still pass.

**M2 — native macOS CLI.**  M1's image embedded in a signed Mach-O (code in
`__TEXT`, data in `__DATA`, linked by Apple `ld`), a shim that reserves the
fixed VAs early and `mach_vm_remap`s the code (r-x) and data (rw-) there
before jumping in — the hosted `head.S` — plus M0's trap mode and the
constant/struct differences.  Done when `modus --eval '(+ 1 2)'` and
`modus --load script.lisp` run natively on this Mac.

**M3 — JIT on macOS: DONE (2026-09-27).**  Build the Darwin image WITHOUT
`MODUS_NO_JIT`.  See *The JIT on macOS* below.  (Planned as "unfixed arena,
saved images relocate"; neither turned out to be needed.)

**M4 — sockets natively.**  The glass RFB server serving from a native
macOS modus.

**M5 — iOS.**  AOT the whole image, JIT off, syscalls through libSystem
function pointers handed in by the shim (no raw `svc` on iOS), and a small
Swift host that blits the glass framebuffer to a Metal texture and feeds
touches back.

## Running natively (M0)

```
MODUS_DARWIN=1 MODUS_NO_JIT=1 MODUS_CODE_BASE=7000010000 \
MODUS_CONV_DELTA=7040000000 MODUS_HEAP_BASE=7080000000 \
MODUS_JIT_ARENA_BASE=7100000000 MODUS_CLI_OUT=/tmp/modus-darwin.elf \
  sbcl --dynamic-space-size 16384 --script mvm/build-aarch64-cli.lisp
host/macos/build-macos.sh /tmp/modus-darwin.elf ./modus
./modus --eval '(+ 1 2)'
```

- **Syscalls.**  `MODUS_DARWIN=1` makes `a64-svc` emit, for SVC #0,
  `str x30,[sp,#-16]!; x16 = slot; ldr x16,[x16]; blr x16; ldr x30,[sp],#16`
  (other SVC immediates become BRK).  The slot is one 16 KB page below the
  code base; the shim stores `modus_syscall_stub` there.  The stub
  (`syscall-stub.S`) preserves every register but x0 — SVC semantics — and
  `modus_syscall` (`modus-shim.c`) translates the LINUX aarch64 call the
  image makes: numbers, open/at flags, `struct stat`, mmap flags
  (`MAP_FIXED_NOREPLACE` emulated), clock ids, errno.  Unknown calls return
  `-ENOSYS` and are logged once.  Translating in the host's C, not a second
  trap ABI in the translator, is also the iOS shape (no raw syscalls there).
- **Mappings.**  Only the code is the shim's job (remapped from `__TEXT`,
  16 KB-aligned via `-sectalign`); the boot stub maps the region, heap and
  arena itself, through the translator, exactly as on Linux.  An unfixed RWX
  request (the exec-page primitive with no arena — GC bitmaps) is retried RW.
- **macOS reserves `[0x1000000000, 0x7000000000)`** (64–448 GB, no access;
  probed with `mach_vm_region`), which is why fixed maps at 64/128 GB fail and
  why 448 GB is the floor.  Nothing can sit below `0x7000000000`, so the code
  base is `0x7000010000` and the slot page `0x700000C000`.
- **The JIT-off boot applies the layout too** (`%jit-boot-init` /
  `%aa64-jit-boot-init`): the in-image compiler serves the interpreter, and
  without it the first native run faulted on the MV-count slot's old
  address.  (This also invalidates the earlier moved-layout JIT-OFF gate
  spot-checks; the stock-`main` reproduction of the format-f failures stands.)
- **argv strings are copied to 16-byte-aligned storage**: the restore path
  reads `argv[2]` as `(* 2 (mem-ref … :u64))`, which drops bit 0 — the class
  `455f7780` fixed for getenv; macOS put argv at odd addresses.
- **Fault report.**  The image installs no signal handlers on Darwin yet, so
  the shim prints the PC and registers (as image offsets) on SIGSEGV/BUS/ILL/
  TRAP.  `lldb` cannot attach to the ad-hoc-signed binary without
  `get-task-allow`.

- **Signals.**  `rt_sigaction` / `rt_sigprocmask` / `kill` translate Linux
  signal numbers (SIGBUS 7 -> 10, …), flags and masks.  The image's handler
  (trap `#x0520`) is a stub that ignores its arguments and branches into the
  armed handler-case frame, so Darwin calls it directly: real SIGSEGVs unwind
  into handler-case natively, repeated and nested.
- **Directories.**  `getdents64` is emulated with a `DIR*` per fd.

**The test scripts, native vs a Linux twin** (same layout, no x18, no JIT —
only the OS differs): 68 of 72 byte-identical.  Three `region-gc*` tests
differ only in heap checksums and live-byte counts (the process's own strings:
macOS passes a far larger environment), with identical pass counts;
`rfb-static` needs sockets.

- **Sockets.**  socket/bind/listen/accept/connect/getsockname/sendto/recvfrom/
  set-/getsockopt/shutdown, translating `sockaddr` (Darwin's length byte),
  `SOL_SOCKET` and option numbers, and `SOCK_NONBLOCK`/`SOCK_CLOEXEC`;
  sockets get `SO_NOSIGPIPE` so writes fail with EPIPE as Linux callers
  expect.  `test/run-rfb-static.sh` — modus serving a framebuffer over RFB to
  a real (Python) VNC client — PASSES natively.  Getting there found a hosted
  aarch64 LINUX bug: the syscall remap table lacked getsockname/setsockopt/
  sendto/recvfrom, so they ran as chroot/fchownat/fstatfs/truncate (the
  Linux twin printed `PORT -1`); fixed in its own commit, and the same test
  now passes on Linux too.

**Not yet:** fork/wait.  Native threads: see below.

## The JIT on macOS (M3)

- **The arena stays fixed.**  `MAP_JIT` with `MAP_FIXED` is refused, but
  macOS honours a `MAP_JIT` address *hint* (probed), so the boot stub's arena
  request — any exec `mmap` — becomes `MAP_JIT` at the hint, and
  `MAP_FIXED_NOREPLACE` is still emulated.  JIT pages keep their addresses,
  so save-and-die carries them exactly as on Linux.
- **Write/exec is flipped by faults.**  A thread sees `MAP_JIT` memory
  writable OR executable (`pthread_jit_write_protect_np`).  The image writes
  JIT code with ordinary stores and then calls it, as on Linux; the shim owns
  SIGSEGV/SIGBUS and, for a fault inside a JIT region, flips the mode and
  returns so the instruction retries — a fetch in write mode (pc == fault
  address) flips to exec, a store in exec mode flips to write.  Probed with
  1000 alternating rounds.  Other faults chain to the handler the image
  registered (handler-case recovery), else the fault report.  JIT code that
  writes JIT memory would ping-pong; the shim stops it loudly after 8 flips at
  one pc.  The handler runs `SA_NODEFER` because a chained image handler
  never returns.
- **The GC bitmaps leave the arena on Darwin** (`gc.lisp`
  `%gc-bitmap-map`, `(%layout :darwin 0)`): written on every allocation,
  they would flip the mode twice per allocation.  They are plain RW `mmap`s
  there; everywhere else they still come from the exec-page primitive.
- **`read(2)` into the arena bounces through ordinary memory**: the kernel's
  copy-out fails with EFAULT on `MAP_JIT` pages even in write mode, and no
  fault reaches the handler.  Core restore reads JIT pages that way.

Verified natively: `(jit-eager)` translates 121 runtime functions (37 fail —
the same count as Linux), `(jit-eager '(fib 24))` runs native, 6.7 MB of JIT
code in the arena; a core saved after `jit-eager` restores with its JIT pages
and keeps running them; smoke/stress/GC/fault/RFB all pass on the JIT build.
(Runtime DEFUNs start as interpreter trampolines and the JIT is hot-gated;
`jit-eager` is how to exercise it — the same on Linux.)

## Native threads on AArch64 (Linux and macOS)

Hosted x86-64 has had up to 16 OS threads for a while: a per-thread *window*
(the MV buffer, nargs, handler frames, the dynamic-binding stack) addressed
through the FS segment, and a per-CPU block (CPU id, active GC region) through
GS.  AArch64 has no segments, so the port keeps the same design and changes how
the two bases are reached.

**The per-thread window.**  Every window access adds a per-thread DELTA to its
literal address (translate-aarch64.lisp, THE PER-THREAD WINDOW, AARCH64).  The
delta is 0 on the main thread, so the main thread needs no set-up, exactly as
FS = 0 did.
- Linux: TPIDR_EL0, which the kernel saves per thread and a static image
  starts with 0.
- macOS: TPIDR_EL0 is **not** usable.  The kernel rewrites it with the CPU
  number on context switches: a probe that wrote it and read it back 3M times
  lost the value ~50 times per thread, and read values 0..13 on a 14-core
  machine.  TPIDRRO_EL0 points at the thread's pthread TSD array, so the
  delta lives in a pthread key (300).  The shim reserves the key at start-up
  and the image reads `[TPIDRRO_EL0 + 2400]`: one extra load per window access.
  A probe of 16 threads x 20M reads saw no mismatch.
- The AArch64 window is bigger than x86-64's.  The handler-frame stack is at
  0x10010000, not in the first page, and there are two more words: the
  helper-call LR save at 0x1000FFF0 and the per-CPU pointer at 0x1000FFE8.  A
  worker's block is therefore 0x11000 bytes.
- On macOS the thread page is mapped *below* the relocated region, so deltas
  can be negative.  The register add wraps correctly; the Lisp readers of the
  self slot sign-extend.

**The per-CPU block.**  EL0 cannot touch TPIDR_EL1, so the block's address is
a word in the window (0x1000FFE8) that PERCPU-REF/-SET load through.  One
consequence: a thread cannot have its own per-CPU block without its own
window.  The older two-thread path (`%ha-spawn-t2`) never gave threads a
window, so on AArch64 `%ha-percpu-init-cpu` now installs one first.  That is
also why `hosted-mv-handler-unsync`, a negative control that relies on a
window-less thread colliding, now "comes back clean" on AArch64.

**Spawn and the rest.**
- TRAP #x0540 is clone(220) with x64's flags, in generic-ABI argument order.
  The child branches off in the stub, before any compiled Lisp runs.
- TRAP #x0541 (`%set-thread-delta`) sets the delta.  Runtime source reaches it
  through `(%layout-if :a64-threads ...)`, a compile-time choice made in the
  %CONV-SUBST pre-pass, so x86-64 never sees an AArch64-only trap.
- ATOMIC-XCHG is now LDAXR/STLXR + DMB ISH.  x86-64's XCHG is a full barrier,
  and the spin locks were written against that.
- futex, gettid, nanosleep, sched_yield, fcntl and getuid are remapped.  The
  remap is a sequential cmp/csel chain, so x64 futex (202) must come before
  `(43 . 202)`, or every accept becomes a futex.
- The GC trampoline scans this thread's MV buffer and dynamic-binding stack,
  and reads this CPU's active-region cell.
- The CLI takes x86-64's heap geometry with threads: 896 MB semispaces in an
  1808 MB mapping (layout key `:heap-size`), where it had 128 MB semispaces.
  Thread regions, the actor band and the lock arena are carved from region
  0's semispace, so 128 MB afforded two regions and 432 MB twelve.

**Collisions on the per-CPU region table.**  `0x10000F08 + 8*cpu`, sixteen
cells, overlapped two AArch64-only word sets:
- The JIT constant-vector root at 0x10000F10, which is CPU 1's cell.  The
  first worker's region adoption overwrote the JIT's constants, and
  sb-thread:make-thread's `'SLOT` read back as 0.  The root is now at
  0x10000FD0; save-image slices the new place.
- The GC pause statistics and the JIT arena bump at 0x10000F20..F58, which are
  CPU 3..10's cells.  With threads these move to 0x1000FF00..FF38.

**Two fixes in shared code.**  Both apply to x86-64 as well, and its suite is
unchanged by them.
- `%rt-threads-on` ran JIT-EAGER *after* opening the gate.  Installing ~140
  native functions grows the symbol-function table inside one locked section,
  which outgrows the 64 KB slice and collects the slice (the B-lite landmine).
  It now compiles before the gate opens, in region 0, since the caller may be
  in a private region.
- `%rt-arena-carve` collects once and retries when the frontier is in the way,
  as `%ha-carve-room` does.

**macOS shim.**
- `clone` copies the syscall stub's frame, which now also stores x19..x28,
  into a resume context.  A detached pthread jumps into the image with it:
  PC after the stub call, SP 16 below the new stack so the stub's LR pop
  lands on it, x0 = 0.
- `exit(93)` on such a thread clears the CHILD_CLEARTID word, wakes waiters,
  and calls pthread_exit.
- `futex` maps to os_sync_wait_on_address / os_sync_wake_by_address_*.
- `gettid` returns the pid on the main thread, as on Linux.
- Each thread gets its own sigaltstack.
- `MODUS_SHIM_STRACE=1` traces every translated call.

**Actors (green threads) on AArch64.**  Hosted SAVE-CTX/RESTORE-CTX now
follow translate-x64's contract.
- The save area holds SP, FP (x29), CENV (x27), x19 and the continuation, and
  nothing goes on the stack.  The bare-metal arm pushed a register block below
  the saved SP and popped it immediately on the save path, so every call that
  path made (YIELD's queue work) built its frames over the block.  A resume
  then popped the wreckage: YIELD came back with FP = 0x20.
- The allocation pointer and limit are no longer saved or restored.  They are
  shared by every fiber on the thread and are moved explicitly by a region
  switch; restoring them rolled the allocator back.
- RESTORE-CTX releases the hosted scheduler lock and does not execute
  `MSR DAIFClr`, which is UNDEFINED at EL0.
- The in-image JIT never register-promotes a local in a function containing
  SAVE-CONTEXT.
- The collector concurrency probe (EE0..EF8: inside / overlap witness /
  barrier) is ported to the AArch64 trampoline with exclusive-monitor loops.
- The bitmap alignment check uses AArch64's unit: bits are set a byte at a
  time, so 128 heap bytes, not x86-64's 1024.

**Status, thread suite (24 tests).**  Linux/aarch64, native macOS and x86-64
now give **identical** results: 19 pass on all three.  The 12 threads, 16
regions and region-0 counts misses are gone, because threaded AArch64 now has
x86-64's heap geometry:
- 896 MB semispaces in an 1808 MB mapping, via the layout key `:heap-size`.
- GC bitmaps sized to the heap, at 1/128 of it.  They were a fixed 8 MB, which
  covers exactly 1 GB, and the collector read past them on a bigger heap.
- The alignment control's offset follows each target's bitmap unit.
- The two tests that compared against unrelocated literals use `%conv-addr`.
- Two selftest windows were measured wrong:
  - `%tl-selftest` opened its "no region-0 collection" window before
    `%rt-threads-on`, whose compile-ahead runs before any second thread exists.
  - The n-region selftest now compiles ahead and collects region 0 before it
    spawns, so the spawns cannot collect region 0 while earlier workers are
    live.  That was the residual race, not a test artifact.

The five failing tests fail on all three platforms: sb-thread, region0-frontier,
term-xregion, worker-xregion and thread-lisp-unsync (a control).
mv-handler-unsync is a race control; x86-64 collides in about 2 of 6 runs.

**sb-thread.**  It passes on all three targets now (44 checks).  It had
crashed on all three.  Three shared fixes, then an AArch64 one:
- **Per-thread condition and non-local-exit state.**  THROW, cross-unit
  RETURN-FROM and handler-case dispatch hand the in-flight exit from frame to
  frame in plain specials (`*catch-tag*`, `*catch-value(s)*`, `*catch-active*`,
  `*current-condition*`, and the restart and handler bookkeeping).  Shared, one
  thread's CATCH read another's tag, re-threw an exit that was its own, and a
  worker longjmped through an empty frame to PC 0.  `%thr-trampoline` now binds
  them per thread (with the gate open, where bindings are per-thread; with it
  shut a LET is shallow and would race).  So is the eval-run state
  `mvm-eval-forms` saves and restores with SETQ.  The compiler needs
  `(declare (special ...))` for those LETs, or it binds them lexically.
- **Nothing escapes a thread.**  `%thr-run-body` catches whatever reaches the
  bottom of a thread, reports it ("thread N: ... escaped the thread body") and
  counts it in the thread record at +0x60.  Before, that was a silent jump to
  PC 0 that took the process down.
- **The eval lock.**  The in-image compiler is process-wide state, and
  interpreted closures are compiled on their first call, so workers compiled
  concurrently.  `%mvm-eval-forms-1` holds a recursive lock for the compile
  only and drops it before the code runs, because a run can block.  It is
  inert until threads are on.

**AArch64 now passes it too, and on every target.**  The last failure was
region 0 collecting under a running worker.  The worker's interpreter read its
closure's bytecode, which main had compiled into region 0, as
`#<STALE-FORWARDED>` after main collected region 0.  That is CLAUDE.md's
unmet precondition ("region 0 must not collect while threads run Lisp").
AArch64 broke it only because it filled region 0 so fast.
- `%make-native-thread` calls JIT-EAGER on every spawn.
- JIT-EAGER re-attempted the 19 modules the AArch64 JIT cannot translate,
  every time, at about 7 MB of garbage each: 140 MB per spawn.
- `%jit-eager-all` now remembers a failed module (its bytecode is immutable,
  so it fails again).  A retry costs 175 KB.  Five fresh modules cost 6.6 MB
  on AArch64 against 2.7 MB on x86-64.

The precondition itself still stands on x86-64: a program that allocates
enough in region 0 while threads run can still break it.  On AArch64 the
stop-the-world handshake below removes it.

## Stop the world (AArch64)

A region-0 collection with threads armed stops every other thread first.
`translate-aarch64.lisp`, "STOP-THE-WORLD FOR REGION 0", has the layout; this
is the shape.
- **The handshake.**  The collector takes a stop flag (`0x10000FC8`, its token
  `2*cpu+2`, by LDAXR/STLXR) and waits until every other live thread's record
  says PARKED or SAFE.  It then adds to its roots:
  - each such thread's stack, from its published SP to its stack top;
  - its per-thread window (MV buffer and dynamic bindings);
  - every carved thread region, up to the frontier the thread published;
  - the lock arena.

  Then it collects as usual and clears the flag.
- **A thread parks only at an allocation.**  The single-threaded runtime
  already assumes an object moves only where something allocates, and code
  relies on it.  The MVM interpreter keeps raw object words as fixnums between
  allocations (`REG-GET`), where no root scan can see them.
  - The first cut parked at loop back-edges too.  A worker FUNCALLing main's
    interpreted closure then died with a TYPE-ERROR or a SIGSEGV: the
    interpreter's raw words still named from-space.
  - Now a back-edge only polls.  With the flag held, it clamps the thread's
    allocation limit (x25) to 0, keeping the real one in the region's
    saved-limit field.
  - The next allocation enters the trampoline, which parks, takes the real
    limit back and, when the allocation fits under it, returns without
    collecting.
  - The gc-check leaves the requested end in x16 (the no-size form now puts
    x24 there), which is how the trampoline knows.
- **Safe regions.**  `%GC-SAFE-ENTER` / `%GC-SAFE-LEAVE` bracket the nanosleep
  and futex waits.  A thread blocked there counts as stopped, since its
  published stack is all it holds.  Leaving while a collection runs, it goes
  back to SAFE and waits for the flag to clear.
- **The cost.**  A loop that never allocates and never blocks holds up a
  region-0 collection until it does one or the other.

`test/hosted-stw.lisp` has four workers traverse main's list, vector and
interpreted closure while main collects region 0.  It requires at least three
such collections during the run and no bad traversal.

**A separate bug it tripped:** a closure that escapes a top-level `LET`
(`(setq *fn* (let ((k 7)) (lambda (x) (+ x k))))`) cannot be called once the
form has returned.  This happens on x86-64 and AArch64 alike, single-threaded.
The test makes its closure with a DEFUN instead.

## Open questions

- Does the image ever write into its own loaded code bytes?  (Decides how
  cleanly it splits into `__TEXT` and `__DATA`.)
- Which callee-saved register is free to replace x18?  (x24/x25 already hold
  the allocation pointer/limit — see actors.lisp.)
- How many fixed addresses live in runtime Lisp source (`mem-ref #x1000…`)
  versus the translators?  M1 needs the full inventory.
- Choosing the fixed VAs robustly: the reserved map differs by OS version
  and hardware, and even between processes.  Reserve the ranges first thing
  at startup and fail with a clear message rather than a stray fault.
- iOS: an app's address space is far smaller than macOS's (and extended
  addressing needs an entitlement), and it is unverified whether iOS allows
  `mach_vm_remap` of executable pages.  If either fails, iOS needs true PIC.
- Save-and-die cores embed code pointers from the image that saved them —
  the same staleness `4a03744` hit on the Pi.  A core is only valid for the
  image, and the VAs, that wrote it.
- Native threads and the actor (green-thread) layer run on hosted AArch64,
  Linux and macOS (see "Native threads on AArch64").
