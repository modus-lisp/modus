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
- **Boot + ELF** — when moved, the ELF segment ends at `0x10000000` and the
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

Not moved yet: the code itself (`:code-vaddr-base`).  x18 is still the base register, which Darwin
forbids — on Darwin the fold arm becomes a plain immediate load.
The interpreter honours the move for its simulated MV buffer; it still
masks `+width-conv-bit+`, which is correct because addresses arrive real.

True PIC remains the fallback if iOS forbids the remap or its address-space
limit leaves no room for a fixed VA.

## Darwin differences the port must absorb

- **x18 is reserved** by Apple's arm64 ABI (the OS may clobber it).  The
  convention-block base moves to another callee-saved register.
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

**M0 — Darwin trap spike (small).**  A `*aarch64-darwin-mode*` beside the
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

**M3 — JIT on macOS.**  `MAP_JIT` arena (unfixed) with
`pthread_jit_write_protect_np` around writes; saved images relocate.

**M4 — sockets natively.**  The glass RFB server serving from a native
macOS modus.

**M5 — iOS.**  AOT the whole image, JIT off, syscalls through libSystem
function pointers handed in by the shim (no raw `svc` on iOS), and a small
Swift host that blits the glass framebuffer to a Metal texture and feeds
touches back.

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
- Hosted AArch64 has no native threads yet (hosted x64 does); threads on
  macOS come after M2.
