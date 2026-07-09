# Modus

Modus is a self-hosting bare-metal Lisp operating system. It compiles Lisp to native code via the MVM (Modus Virtual Machine) — a portable virtual ISA with translators for 9 CPU architectures. The system runs SSH servers, handles USB devices, and supports cooperative actor-based concurrency, all on bare metal with no OS underneath.

## Directory Structure

```
lib/            Shared utilities
  load-mvm.lisp        MVM system loading boilerplate
  hash.lisp            Dual-FNV-1a symbol hashing

boot/           Architecture boot sequences
  boot-aarch64.lisp    AArch64 QEMU virt boot
  boot-rpi.lisp        Raspberry Pi (BCM2835/2837) boot
  boot-x64.lisp        x86-64 boot
  boot-riscv.lisp      RISC-V boot
  boot-ppc64.lisp      PowerPC 64 boot
  boot-ppc32.lisp      PowerPC 32 boot
  boot-i386.lisp       i386 boot
  boot-68k.lisp        Motorola 68k boot
  boot-arm32.lisp      ARM32 boot
  boot-uefi-x64.lisp   UEFI x86-64 boot (PE32+ EFI application)

mvm/            MVM compiler, translators, and build scripts
  mvm.lisp             ISA definition (~50 opcodes)
  target.lisp          Target descriptors for all architectures
  compiler.lisp        3-phase compiler: Source → IR → MVM bytecode
  interp.lisp          MVM interpreter (bootstrapping)
  cross.lisp           Universal cross-compilation pipeline
  repl-source.lisp     Embedded REPL source for bare-metal builds
  translate-*.lisp     Native code translators (x64, riscv, aarch64, ppc, i386, 68k, arm32)
  build-*.lisp         Build scripts (see Build Commands below)

net/            Networking, crypto, USB, actor system
  arch-aarch64.lisp    QEMU virt PCI/E1000 adapter + actor addresses
  arch-raspi3b.lisp    RPi adapter (DMA addresses, actor addresses)
  arch-x86.lisp        x86 adapter
  actors.lisp          Cooperative actor system (spawn, yield, send/receive, scheduling)
  actors-net-overrides.lisp   Actor-aware SSH overrides
  isolated-net.lisp    Qubes-like isolation (net-domain owns all hardware)
  e1000.lisp           Intel E1000 NIC driver
  dwc2.lisp            DWC2 USB host controller (RPi 3B QEMU)
  dwc2-device.lisp     DWC2 USB gadget + CDC-ECM (Pi Zero 2 W)
  usb.lisp             USB enumeration + hub support
  cdc-ether.lisp       USB CDC Ethernet
  hid.lisp             USB HID (keyboard, mouse, tablet)
  ip.lisp              ARP/IP/TCP/UDP/DHCP/DNS
  crypto.lisp          SHA-256/512, ChaCha20, Poly1305, X25519, Ed25519
  crypto-32.lisp       32-bit safe field/poly multiply (pair arithmetic)
  crypto-w32.lisp      32-bit SHA-256/512, ChaCha20 (w32 pair arithmetic)
  ssh.lisp             SSH-2 server (key exchange, auth, channels)
  ne2000.lisp          NE2000 ISA NIC driver (i386)
  http.lisp            HTTP/1.0 server
  http-client.lisp     HTTP client (URL parsing, GET, fetch)
  aarch64-overrides.lisp   Line editor, buffer reader, SSH I/O overrides
  32bit-overrides.lisp     30-bit fixnum safety overrides (crypto, SSH)
  arch-i386.lisp           i386 NE2000 adapter, NIC state, allocation
  uefi-console.lisp        GOP framebuffer + PS/2 keyboard for UEFI x64
  uart-bootloader.lisp     UART bootloader for rapid kernel redeploy
  bcm2835-periph.lisp      BCM2835 GPIO, SPI, I2C, PWM

scripts/        Deployment and boot scripts
  boot-pizero2w.sh     Build + boot + network + SSH (USB or SD card)
  build-pizero2w.sh    Build kernel + SD card image
  fuse-pizero2w.sh     Program USB boot OTP fuse on Pi Zero 2 W
  make-sdcard-bootloader.sh  Create SD card with UART bootloader
  make-uefi-usb.sh     Create bootable USB image for UEFI hardware
  run-rpi-periph.sh    Launch RPi peripheral test in QEMU

mvm/cl-*.lisp   Common Lisp runtime implementation (10 modules)
  cl-sequences.lisp    837L  find, search, sort, merge, map, reduce
  cl-streams.lisp      145L  Stream type system (9+ types)
  cl-fileio.lisp     1,036L  File I/O, Linux syscalls, pathnames
  cl-printer.lisp    1,545L  Printer + format (50+ directives)
  cl-reader.lisp     1,383L  Reader, readtable, #-dispatch, backquote
  cl-packages.lisp     909L  Package system (intern, defpackage, etc.)
  cl-conditions.lisp   915L  Condition system (24 types, handler-bind, restarts)
  cl-clos.lisp         429L  CLOS (defclass, defgeneric, defmethod, dispatch)
  cl-eval.lisp       3,575L  Eval/compile/load (eval2), symbol-function table
  cl-types.lisp        519L  typep, coerce, numeric helpers
  ansi-bridge.lisp   1,888L  Test helpers (eqlt, scaffold, stubs)
  gc.lisp                    GC helper functions (Lisp-side)
  eval2.lisp                 Production eval: compile→MVM bytecode→interpret
  interp.lisp                MVM bytecode interpreter (in-image)
  tree-walker.lisp   3,050L  LEGACY %eval-in-env engine — fork builds ONLY

runtime/        Runtime type system
  tags.lisp            Tag/subtag definitions
  packages.lisp        Runtime package definitions
```

## ANSI CL Conformance

**Real numbers — HARD per-file name-stable comparison (x64 Linux, 2026-06-15):**
- HEAD passed: **16,489 / 17,465 = 94.4%** (tip 9c6151f)
- NET vs base b2ae056: **+825** (same 32-shard method both sides; base=15,664 this run)
- MEASUREMENT NOTE: ID-based cross-build diffs are INVALID (corpus grows mid-
  session → IDs shift).  Compare by FILE NAME via each build's own
  tmp/ansi-file-ranges.txt → per-file pass counts (see tmp/perfile.sh).  The
  per-file ABSOLUTE count has ~±200 run-to-run variance (timeout-600 shards);
  the NET (same-run apples-to-apples) is the reliable metric.  The old
  single-process "88.10%/15,386" was an UNDERCOUNT (~500 tests lost to
  hangs/timeouts) — base was really ~91%.
- Remaining real regressions (~25): warn −11 (handler-bind/signal boundary,
  NOT closure cells), make-string-input-stream −3, union/nunion −2, scattered −1s.

**UNCOUNTED batch on top (tip 1a279f6, 2026-06-13):** a large fan-out landed
many cluster-gated fixes on top of b2ae056 — cerror(+1), compiler-ecase-signal
(+2), streams(+2), numeric rational/subtypep(+6), map-into fp-vector(+10),
file-io composite-stream+pathname(+7), packages-intern, reader-position, clos-egf
(+2), **cross-unit RETURN-FROM/BLOCK via runtime CATCH frame (b50f758, ≥40 tests
— handler-bind/restart cluster)**, typep-symbol+rationalize+ACOS-hang (+8).  Each
was cluster-gated +N/−0; return-from validated by probes 9520/9521 PASS + healthy
block/catch/handler-bind ranges; ACOS hang gone.  A reliable FULL headline re-count
is PENDING a quiet box — the single-process full sweep (a) hangs in post-ANSI
custom probes 100000+ so ansi-summary.sh never prints its summary, and (b)
range-mode `./binary 10001 27708` loses ~2000 tests to intra-range stalls.  Best
partial signal: of-run pass rate 90.0% (up from 88.5%).  DO NOT trust a full count
until measured on a fully idle box via ansi-summary.sh that completes.
- Failed: 2,035
- Lost-to-crash: 44
- Crash markers (timing-IMMUNE gate): FILE-WEDGE=30, CHUNK-CRASH=0 (was 4).
  NOTE: judge regressions by CRASH MARKERS + passed, NOT raw lost-to-crash
  (wall-clock noisy).  Passed swing vs the prior 15,397 is the CHUNK-CRASH
  4→0 reshuffle (recovered chunks now RUN and mostly fail, so ran rose
  17,395→17,421 while passed is flat-within-noise).  This rotation's wins are
  CORRECTNESS/structural, not ANSI-count movers: GC alloc-obj + alloc-array
  zero-init (2faad76/8a76e16 — eliminated a whole Cheney-flat-scan heap-
  corruption class) and runtime-DEFUN implicit-BLOCK (062edd4 — return-from
  stops escaping to empty stack).  Payoff is the gauntlet: define-package
  cascade GONE, frontier 158-fail-cascade → clean deterministic stop at
  form 56 (runtime ECASE gap).  Sweep noise ~±10-15 on Passed (wider when
  CHUNK-CRASH count changes).

The historical "17,567/17,568" figure was inflated. Per-chunk forks die
silently mid-thunk on unimplemented forms — and the summary only saw
tests that finished AND passed/failed cleanly. Every test after a fork's
first crash was simply lost.

Now the harness:
- Installs a SIGSEGV/SIGBUS/SIGFPE/SIGILL handler at boot. Faults longjmp
  to the nearest handler-case instead of killing the fork.
- Wraps every `rt-run-test` in `(handler-case ... (t (c) :CRASHED))` so a
  fault during a test eval reports `:CRASHED` rather than dying.
- Wraps each `run-ansi-FOO` body in an outer `(handler-case (progn ...) (t (c) nil))`
  so init-form crashes don't kill the chunk.
- Writes one "+" byte per pass to stdout immediately (no buffering,
  survives any later crash); the summary counts those bytes.

Build: `sbcl --dynamic-space-size 2048 --script mvm/build-ansi-test.lisp`
Run with summary: `./scripts/ansi-summary.sh` (runs binary, prints honest pass/fail/lost counts)

### CL Implementation Status

```
Packages          ✓  make-package, intern, find-symbol, defpackage, 24 functions
Streams           ✓  9 stream types, read/write-char, string/file/broadcast/echo
Reader            ✓  full read, readtable, #-dispatch, backquote, package qualifiers
Printer           ✓  write with *print-* vars, 50+ format directives
Conditions        ~  24 types, handler-case/bind, restart-case, signal/warn/cerror
                     handler-case :no-error landed (commit 334cc83); handler-bind
                     frame inhibition + restart-aware warn/cerror landed via the
                     COND agent fleet.  Restart-case nested invoke + MV
                     propagation landed (b09b978).  Remaining gaps: restart-bind
                     return-from across closure body, with-condition-restarts
                     condition→restart association, assert macro semantics.
CLOS              ~  defclass, defgeneric, defmethod, standard method combination
                     :before/:after/:around, call-next-method, class precedence lists
                     (define-method-combination tests fail)
File I/O          ✓  Linux syscalls, file streams, open/close, pathnames
Eval/Compile/Load ✓  eval2 (compile→MVM bytecode→interpret) is production eval/load
Closures          ✓  Mutable closures via heap-allocated cells
unwind-protect    ✓  setjmp/longjmp, cleanup on both normal and error paths
GC                ✓  Cheney semi-space copying collector
Setf              ~  defsetf (short + CLHS-correct long form), define-setf-expander,
                     get-setf-expansion, define-modify-macro register, but
                     compile-time only (no runtime defsetf in eval).  setf (the
                     type place) and setf (values ...) expand correctly via
                     compile.lisp's SETF macro (commit f03d9f1).
~400+ CL functions implemented
```

### What's Missing for Quicklisp

```
[✓] Runtime compile (source → bytecode → interpret): SHIPPED — production
    `eval`/`load` default to eval2 (compile to MVM bytecode + mvm-interpret)
    as of d3434e6 (WS3 flip, 2026-07-03).  Post-flip full ANSI: 17321/17318
    vs tree-walker 17311 (net positive).  MODUS_NO_EVAL2=1 = rollback.
    Compile-caching (~20x for repeated forms) serves load/asdf patterns.
    bytecode→native at runtime (the JIT) is WS4.
[ ] compile-file → FASL
[ ] Full numeric tower (arbitrary bignums, ratios, full floats, complex)
[ ] Setf machinery (defsetf, define-setf-expander)
[✓] Everything else
```

### Self-hosting (WS3): the tree-walker is RETIRED from production

Production `eval`/`load` are **eval2 UNCONDITIONALLY** (`(eval2 form)` —
compile the form to MVM bytecode via the self-hosted compiler, run it through
`mvm-interpret`).  The `*use-eval2*` flag, `MODUS_NO_EVAL2` lever, boot
marker, probes-on-walker bracket, and `*e2ic-disable*` are all DELETED
(STEP 2 = b1e889a, STEP 4 = 5453732, 2026-07-09).  The tree-walker
(`%eval-in-env` + its ~48-function engine) lives in **mvm/tree-walker.lisp**,
loaded ONLY by the four legacy fork builds (build-aarch64-ansi-test,
build-aarch64-linux-ansi-test, build-with-compiler,
build-x64-modus-ansi-test), each of which also defines the
`(defun eval2 (form) (%eval-in-env form nil))` bridge in its own build
script.  Production images contain ZERO walker code; cl-eval.lisp's
`%call-interp-closure` and ansi-bridge.lisp's `%expand-deftype` are
engine-provider stubs overridden last-defun-wins by eval2.lisp (production)
or tree-walker.lisp (forks) — every build wires exactly one engine.
Unsupported interp-closure shapes / eval2 compile failures now SIGNAL
honest errors (no silent second-evaluator degradation).  History: the flip
landed d3434e6 (drain 387→0); the asdf gauntlet runs 243/243 on
production-eval2.  Landing STEP 2 exposed two structural rules — no
`(eval …)` in kernel-main/boot paths (boot-time eval2 runs before
init-all-globals), and never define a duplicate defun name in shared image
source (by-name resolution ambiguity).
See /home/claude/.claude/plans/hazy-dazzling-deer.md.

## Build Commands

All builds: `sbcl --script <build-script>`

### QEMU AArch64 (virt machine, E1000)
```bash
# SSH server (single-threaded)
sbcl --script mvm/build-aarch64-ssh.lisp
# Actors (cooperative multi-connection SSH)
sbcl --script mvm/build-aarch64-actors.lisp
# Isolated actors (Qubes-like, net-domain owns hardware)
sbcl --script mvm/build-aarch64-isolated.lisp
# REPL only (serial)
sbcl --script mvm/build-aarch64-repl.lisp
```

QEMU launch (actors example):
```bash
qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 \
  -kernel /tmp/modus-aarch64-actors.bin -nographic -semihosting \
  -device 'e1000,netdev=net0,romfile=,rombar=0' \
  -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
```

### QEMU RPi 3B (DWC2 USB host, CDC Ethernet)
```bash
sbcl --script mvm/build-rpi-ssh.lisp      # SSH
sbcl --script mvm/build-rpi-hid.lisp      # USB keyboard REPL
sbcl --script mvm/build-rpi-repl.lisp     # Serial REPL
sbcl --script mvm/build-rpi-periph.lisp   # GPIO/SPI/I2C peripherals
```

### QEMU i386 (32-bit x86, Multiboot)
```bash
sbcl --script mvm/build-i386-repl.lisp    # Serial REPL
sbcl --script mvm/build-i386-ssh.lisp     # SSH (NE2000 ISA NIC)
```

QEMU launch (REPL):
```bash
qemu-system-i386 -kernel /tmp/modus-i386.bin -m 256 \
  -display none -serial stdio -no-reboot
```

QEMU launch (SSH):
```bash
qemu-system-i386 -m 256 -nographic -no-reboot \
  -kernel /tmp/modus-i386-ssh.bin \
  -device ne2k_isa,netdev=net0,iobase=0x300,irq=9 \
  -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
```

### UEFI x86-64 (OVMF, for real hardware)
```bash
sbcl --script mvm/build-uefi-repl.lisp   # REPL (serial + framebuffer + PS/2 keyboard)
```

QEMU launch (requires OVMF + mtools):
```bash
./scripts/run-uefi-repl.sh               # interactive (serial)
./scripts/run-uefi-repl.sh "(+ 1 2)"     # eval expression
```

Bootable USB for real hardware (ThinkPad T420 etc.):
```bash
./scripts/make-uefi-usb.sh               # create /tmp/modus-usb.img
sudo dd if=/tmp/modus-usb.img of=/dev/sdX bs=1M status=progress  # write to USB
```

### Pi Zero 2 W (real hardware, DWC2 USB gadget, CDC-ECM)
```bash
sbcl --script mvm/build-pizero2w-ssh.lisp      # Single-threaded SSH
sbcl --script mvm/build-pizero2w-actors.lisp   # Actor-based SSH
```
Output: `/tmp/piboot/kernel8.img`

Deploy via rpiboot (USB boot): `sudo rpiboot -d /tmp/piboot`
Deploy via UART bootloader: `sudo python3 ~/deploy-kernel.py ~/kernel8.img`
Full workflow: `./scripts/boot-pizero2w.sh`

## Tagged Value System

All values are tagged 64-bit words with fixnum-shift=1:
- **Fixnum** (tag xxx0): value << 1, 63-bit integers
- **Cons** (tag 0001): pointer to car/cdr pair, 16-byte aligned
- **Object** (tag 1001): pointer to header + data
- **Immediate** (tag 0101): characters, nil, booleans
- **Forward** (tag 1111): GC forwarding pointer

Object header: `[subtag:8][unused:7][element-count:49]`
Key subtags: string=#x10, symbol=#x50, keyword=#x53, closure=#x52, array=#x32, hash-table=#x41

Keywords (`:foo`) are subtag #x53 — distinct from #x50 symbols so KEYWORDP
can identify them without a per-symbol package slot.  compile-keyword in
mvm/compiler.lisp emits `(li v0 hash; call %INTERN-KEYWORD)` so all `:foo`
literals at any call site resolve to the same heap object via the keyword
intern table at #x10000148 (init by `init-keyword-table` early in
kernel-main).  SYMBOLP accepts both #x50 and #x53; KEYWORDP only #x53;
SYMBOL-PACKAGE returns the KEYWORD package for #x53 objects.

Reader's `(intern name (find-package "KEYWORD"))` is unified with
compile-keyword: it also routes through %INTERN-KEYWORD and returns the
**same** #x53 object the literal `:foo` in source resolved to.  Round-trip
eq holds: `(eq (read-from-string ":FOO") :foo)` is T.  Regression tests
9701–9706 in `run-clos-diag-tests` lock in the contract.

### mem-ref Semantics (MVM)
- `:u8`, `:u32` loads → result is **tagged** (SHL 1); stores → value is **untagged** (SHR 1)
- `:u64` loads/stores → **raw** bits, no shift
- Address operand is always **untagged** (SHR 1)

### Object Layout
Objects have 8-byte header + 8-byte padding + data. Data starts at raw+16 (not raw+8).
OBJ-REF/OBJ-SET offset formula: `idx*8 + 7` (accounts for tag 9 and 16-byte header).

### String element access (CONFORMANT as of e159986, 2026-06-14)
Strings store char CODES in their slots, but PUBLIC `aref`/`elt`/`row-major-aref`/
`char`/`schar` on a string return a **CHARACTER** (`(aref "abc" 0)` → `#\a`, not 97),
and `(setf (aref s i) ch)` accepts a CHARACTER and stores its code. This is CL-
conformant. Internal code that needs the raw CODE must use `%prim-aref`/`%prim-aset`
(NOT public `aref`). compile-aref wraps string reads in `code-char` (gated by
`%prim-stringp`/`%wrapper-stringp`); compile-aset coerces char→code for string dests.
WARNING: compiled `(code-char X)` is a raw shift-and-tag primop with NO `characterp`
guard — calling it on an already-character double-encodes (garbage). So never
`code-char` a value already read via public `aref`.

### CL Symbol Layout
CL symbols: `(cons *sym-tag* #<array [hash, package, name]>)` where *sym-tag* = 123456789.
`%cl-sym-p` checks `(eql (car x) *sym-tag*)`. Package is array slot 1. Name is slot 2.
See `mvm/cl-packages.lisp` for all accessors.

### CL Package Layout
Packages: `(cons *pkg-tag* #<array-7 [name, nicknames, internal, external, use-list, used-by, shadowing]>)`.

## x86-64 Memory Layout (bare metal)

The kernel image loads at 0x100000 (1MB). Memory regions must not overlap:
- **0x100000**: Kernel image (native code + bytecode + fn table + metadata)
- **0x500000**: Metadata (64 bytes, at image offset 0x400000)
- **0x504000**: Page tables (16KB, identity map for 1GB)
- **0x600000**: Global variable store (alist head pointer, 8 bytes)
- **0x800000**: Stack top (grows downward)
- **0x10000000**: Heap start (R12 alloc pointer)
- **0x1E000000**: Heap limit (R14)

## Linux x64 Memory Layout

ELF loads at 0x400000. Heap via mmap (hint 0x10000000, kernel may place elsewhere).
- **0x400000**: ELF image (code + data)
- **0x400078**: Entry point (boot stub)
- **0x10000040-0x10000068**: GC metadata (BSS, part of ELF LOAD segment)
- **0x10000080**: Global variable alist (BSS)
- **0x10000088**: Symbol intern table (BSS)
- **0x10000090**: MV-count + MV-values (BSS)
- **mmap result**: Heap (R12=alloc ptr, R14=limit)

**Important**: The mmap hint 0x10000000 is NOT honored — Linux typically maps at 0x7fff...
The BSS at 0x10000000 is part of the ELF's LOAD segment (p_memsz >> p_filesz).
GC metadata stores mmap-relative addresses at BSS locations. The GC trampoline reads
these at collection time.

## Garbage Collector

Cheney semi-space copying collector. Two ~469MB semispaces within the mmap'd heap.

- **GC check**: `CMP R12, R14; JB skip; CALL gc_trampoline` after every allocation
- **Roots**: stack scan (RSP to stack_base) + globals alist + symbol table
- **Copy**: cons cells (16 bytes manual) + objects (REP MOVSQ, size from header)
- **Forwarding**: tag 0xF in from-space, Cheney scan in to-space
- **Init**: lazy — metadata computed from heap_base on first trigger

`scan_word` saves/restores RDX (stack_base) around copy_object calls.

### Conservative-root validation collector (x64, landed ace1544 + 810a975)

The Cheney collector is now hardened by an **object-start bitmap** (1 bit /
16-byte granule, MAP_ANON zero-filled, config words at 0x10000E00..; built in
`boot-linux-x64.lisp`, maintained in `translate-x64.lisp`).  Every allocation
site sets its start-bit (cons/`:alloc-obj`/array/string/float/sap — and since
`:alloc-obj` is the universal object allocator, that also covers closures,
bignums, ratios, hash-tables).  `scan_word` REJECTS any root candidate whose
tag-stripped address is not a recorded object start; `copy_object` then runs
only on real starts.

This kills the catastrophic corruption class: a conservative stack word
aliasing a from-space address used to make `copy_object` stamp a forwarding
pointer over MID-OBJECT data (the header-less cons path had no validation).
Provably safe — Modus has no interior pointers and fixnums (low bit 0) can't
alias tags 1/9, so a candidate pointing at a non-start is *guaranteed* a false
positive.  This is NOT pin-in-place: everything still copies; the gate only
filters ambiguous roots.  (Pinning would additionally cover a raw scratch word
exactly aliasing a live object's BASE — a narrow, non-corrupting case; deferred
pending a decision on whether it's worth the page-pool allocator.)

**Point (c)** (810a975): after the semispace swap, `REP STOSB`-clears the
reclaimed from-space's bitmap sub-range (byte-exact; the per-semispace bitmap
boundary at space_size/128 isn't 8-aligned).  Without it the bitmap saturated
and the gate decayed to a no-op over many GCs — still corrupting long runs.
Payoff: the asdf gauntlet went from a form-49 saturation-corruption stop (a
FALSE `UNDEFINED-FUNCTION` on `define-package UIOP/PATHNAME`) to a CLEAN
form-98 completion (the 2 remaining fails are real NOT-IMPLEMENTED gaps like
`with-upgradability`, not GC).  Verified **net +134 ANSI / 0 real regressions**
— the apparent −210 was a 600s shard-timeout truncation in the alloc-heavy
printer/format cluster; sub-sharding that range at 900s recovered reg=0.
**Judge GC ANSI by SUB-SHARDED comm-diff, never a single coarse shard.**
Build knobs: `*mcgc-collector-enabled*` / `*mcgc-bitmap-enabled*` (default
`:follow-gc`, tied to `*x64-gc-enabled*`).  Known cost: the per-alloc BTS
sequence slows alloc-heavy code (optimizable follow-up).

Functions are NOP-aligned to avoid addresses ending in 0x1 (cons tag collision
with closure-aware funcall dispatch). See `*x64-native-code-offset*`.

### Crash triage: it's almost never the GC — default elsewhere

The collector is hardened (fuzz-closed layout-dependence, conservative-root
object-start + cons-kind validation, guard band, zero-init alloc).  When a
new change crashes, do **not** conclude "GC fragility / out of scope" — that
verdict has been wrong every time it's been reached.  Bisect to the real
cause, which is almost always one of:
- **A subtag collision** — a new object subtag reusing one already assigned
  in `runtime/tags.lisp` (e.g. single-float at `#x61` == `+subtag-mvm-module+`:
  the collector scanned the float's raw IEEE bit-slots as mvm-module pointers
  → corrupted a live fn → `RIP=0xDEAD1004`).  **ALWAYS check `runtime/tags.lisp`
  before claiming a free subtag.**  See [[reference_float_subtag_collision]].
- **MVM caller-save clobber** — a live reg-COPY destroyed across an added call
  (see [[reference_mvm_caller_save_bug]]).
- **A missing `:gc-check` before `:alloc-obj`** (see [[reference_make_closure_gc_check]]),
  or a real logic bug in the new code.

Method that works:
1. **Reproduce deterministically.** A crash that reproduces every run is NOT a
   GC race.  NOTE (corrected 2026-07-09): the generic binary does NOT run
   GC-off — R14 sits 16MB below the mmap end (the guard band), so long runs
   DO collect (the gauntlet crosses it).  For a faster repro lower the
   trigger with `MODUS_GC_R14=<small>` (e.g. 262144).
2. **`RIP=0xDEADxxxx` as the program counter** = a corrupted/NIL FUNCTION
   pointer was *called* (control transfer), not a data deref — i.e. heap
   corruption of a live fn/closure, which is exactly what scanning raw
   non-pointer slots (float bits, etc.) as pointers produces.
3. **Bisect the trigger**: calls-only-no-alloc → isolates caller-save; same
   object at a different subtag value → isolates a subtag collision; etc.
   A subagent reporting "GC fragility, out of scope" should be sent back to
   bisect to determinism first.

The image (especially fixpoint-ssh with networking) can grow past 0x400000. The fn table
at the end of the image must not overlap the globals or stack. Build scripts assert this.

## MVM Compiler — Source-Quality Guardrails

- **`check-parses` at build time**: Build scripts (`build-ansi-test.lisp`, `build-fixpoint.lisp`, `build-mvm.lisp`) call `modus.mvm::check-parses` on every first-party source file before reading it. A paren mismatch in `%format-impl` once hid behind the lenient in-build reader for weeks, presenting as a fake "late cond branch miscompilation". `check-parses` fails fast with the specific file and error so this can't recur silently. If you write a new build script, wire it into your `mvm-text` wrapper.
- **`compile-call` warns on list-headed non-lambda fn**: The old fallback silently emitted `CALL-INDIRECT` on whatever the "function expression" evaluated to, which routed every downstream cond clause through T/NIL indirection for the `~( ~)` paren bug. Now any `((test) body)`-shaped fn (other than `(lambda …)` or `(setf NAME)`) prints `;; WARN compile-call: …` to stderr with the source location. The code still emits the indirect call so ANSI tests that deliberately construct bad callables keep compiling.

## MVM Compiler — Active Limitations

1. **Last-defun-wins**: All calls resolve to the LAST defun of a given name. You cannot alias a function before overriding it. Use different names.
2. **Variable-index ASET bug**: When `(aset arr idx val)` with a variable `idx` is a non-last form (`dest=nil`), the value may not load correctly. **Workaround**: `(let ((dummy (aset arr idx val))) body)` forces `dest=frame-slot`.
3. **Symbol identity — per-package (CLHS 11.1.2) via SYMBOLS_PLAN phase 1 (a1327d6)**: `%intern-symbol-pkg` and `intern` both key the global intern table by a composite of NAME-HASH and PKG-HASH.  Within-package `(eq 'foo 'foo)` holds (covers compile-time literal vs runtime READ vs the CL-symbol-wrapper / native-MVM-sym boundary within a single package).  Cross-package `(eq cl-test::x ds4::x)` returns **NIL** — CL-correct, was previously T under the daa0763 unified model.  See SYMBOLS_PLAN.md.
4. **YIELD opcode**: Emitted at end of every `loop` iteration. On AArch64 bare metal, must be SEV+WFE (not just WFE which would stall on Cortex-A53).
5. **cons cells in actor context**: May get corrupted across yield/context-switch boundaries. Inline data construction instead of relying on cons returns when the result crosses scheduling points.
6. **Funcall tag — RESOLVED via OR-3**: Function pointers are explicitly tagged in `mvm-fn-addr` (translate-x64.lisp ~line 2794): `LEA + OR-3`.  Raw fn-code is NOP-padded so the low nibble is 0; OR-3 yields tagged fn pointers with low nibble `0x3` always.  Tags `cons=0x1`, `fn=0x3`, `char=0x5`, `obj=0x9` are mutually disjoint in the low nibble, so the historic "funcall-tag-collision" / "fn-addrs at vaddr ???05" classes are STRUCTURALLY impossible.  When bytecode shifts move function addresses, the OR-3 still produces a clean tag — predicate flips don't happen via tag-nibble accident.  A change that appears to break something via "code movement" has a real semantic cause; bisect to it (see "Layout shift broke an unrelated test" below).
7. **defvar init-thunks not run**: `(defvar *foo* 42)` declares `*foo*` but does NOT set it to 42 at boot — `init-all-globals` is skipped. Defvars default to NIL. Initialize required values explicitly via setq in an init function (e.g. `*pkg-tag*`, `*sym-tag*` set in `%init-packages`). Predicates that compare against an uninitialized tag with `(eql (car x) *tag*)` will return T for *any* cons-with-NIL-car — see `%pkg-p` history.
8. **`compile-ash` inlines `:shl`/`:sar` for small constant counts (≤30) WITHOUT a fixnum tag check — corrupts a BIGNUM value.** `(ash bignum-val small-const-count)` shifts the raw tagged word, which is right for a fixnum (low bit 0) but mangles a bignum pointer (tag 0x9): `(ash (ash 1 200) -1)` :sar's the heap address to garbage, `(ash 2^200 -200)` returns a bogus fixnum. Variable-count and large-constant-count ash already route to runtime `bignum-ash` (correct). The bignum-ash runtime fix (9463e26) makes `(ash 1 N)` produce a CORRECT big bignum, which then SURFACES this latent compiler bug in any code that shifts those bignums by a small constant — `%print-integer-in-base`-adjacent format-runtime paths (format-d/o/x/b chunk-crashes), `gcd.5`/`logcount.7-8` (`(ash 1 200/300)` bounds → big-bignum operands → `(ash x -1)`). **FIX IS A COMPILER CHANGE but every attempt so far BROKE the harness's SIGSEGV-handler longjmp recovery** (a previously-recoverable pre-existing crash in nunion/modules went fatal; the 4x code-size blowup of the source-expansion `(if (fixnump g) (%ash-fixnum-raw g k) (bignum-ash g k))` is the suspected trigger — possibly fn-table/branch-displacement). The individual ash results were CORRECT (probes 9920-9925 passed); only harness recovery regressed. A recovery-safe fix is the open follow-up. Workaround for now: route through `bignum-ash` explicitly when a value may be a bignum.

## Known Bugs

### Mutable Closures — Global Cell Limitation (RESOLVED)

This is FIXED (verified 2026-06-13, probes 9740-9744). `compile-let`/`compile-let*`
now allocate each captured+mutated variable's cell as a **local `let`-binding**
`%CELL-V = (cons init nil)` — fresh per `let` execution — and `compile-lambda`
captures the cons pointer by value into the closure env. There is no global-cell
fallback path left in compiler.lisp. Multiple closures from the same source lambda
get independent accumulators; `(mapcar #'(lambda (x) (push x acc)) items)` and
two-counters-from-one-`make-counter` both work.

(Historical: the old scheme used `setq`-based global cells `%CELL-varname` shared
across closures from one source lambda. Replaced by the let-binding scheme in an
undocumented prior commit; the map-into cluster fails that were blamed on this are
actually `cl-sequences.lisp` fill-pointer / bit-vector-store bugs.)

**The "context-sensitive residual" hypothesis was WRONG (disproven 2026-06-15,
fixed in 3c034b8).**  The warn cluster's 8/19 was NEVER a cell-capture bug.
Driving the real compiler in SBCL on both the standalone form and the 2-sibling
chunk shape shows the handler closure captures `%CELL-WARNED` CORRECTLY in both
(`captured=(%CELL-WARNED)`, `env-has-cell=T` both).  The actual defect was a
RUNTIME handler-bind state leak crossing the harness's per-file forks: an
escaping handler (return-from/throw/muffle — the throw-from-handler probes
9523/9526) skips both `%signal-condition`'s skip/walk-depth restore AND
`%with-handler-bind`'s frame pop; the leaked frame keeps `*handler-bind-stack*`
non-null, which blocks `%heal-handler-bind-skip` (it only rewinds skip on a NULL
stack), so elevated skip silently inhibits the next signal's leading handler
frames.  A custom probe escaping a handler in the PARENT process poisons every
subsequently-forked ANSI file → warn.1 (the FIRST warn test) fails on an
apparently-clean slate.  The runtime-data tell: failures were the LEADING block
(warn.1-11) with later tests passing — the opposite of a cascade, and warn.3
PRINTED instead of muffling (handler never ran).  Fix: `%reset-signal-state` at
each `run-test`/`run-test-mv` boundary (per-test isolation).  Gate: warn 8->18,
conditions band +23, zero regressions, 9xxx probes bit-identical.  The proper
escape-safe pop in `%with-handler-bind` is now LANDED (279f2cc, 2026-06-27):
once 9525 was fixed (7a56022, NLX threads through unwind-protect cleanups) the
pop could be wrapped in `unwind-protect` (lexical-save + setq-restore; do NOT
dynamically rebind the special).  The stack now always drains, so
`%heal-handler-bind-skip` rewinds the leaked skip at the next fresh signal and
REAL programs are fixed too — not just the harness.  Gate +1, 0 condition-
chapter regressions, CHUNK-CRASH 15->0.  `%reset-signal-state`/`%heal` remain
as defense-in-depth.  STILL latent: `%push-restarts` (restart-bind) has the
identical bare-setq leak; the one-line unwind-protect fix is correct but gated
-4 pure layout churn with no corpus gain, so deferred until layout-neutral.

### Vector-literal symbol elements (FIXED)

The earlier docs claimed `#(...)` literals containing symbols caused a SIGSEGV
inside lambda bodies that contained nested-let-with-mutation. That was wrong;
nothing crashed. The actual bug was a silent value-corruption in compile-form's
vector-literal path:

  ((and (vectorp form) (not (stringp form)))
   (compile-form `(let ((arr (make-array ,n)))
                    ,@(loop for i from 0 below n
                            collect `(aset arr ,i ,(aref form i)))
                    arr) env dest))

The `,(aref form i)` pasted SBCL-side element values directly into the
expansion. For symbol elements (e.g. `#(A B A C)`), each element became a
bare token `A`, `B`, etc., which compile-form then resolved as a variable
reference — `(symbol-value 'A)` → NIL. So vector literals containing symbols
silently became vectors of NILs. NSUBSTITUTE/SUBSTITUTE/FIND on such vectors
trivially failed because the elements never matched the search items.

Fix: compile-form delegates literal vectors to `compile-quote`, which emits
proper element loads (`%INTERN-SYMBOL` for symbols, fixnum loads for ints,
etc.) and `:obj-set` into the freshly allocated array. See `mvm/compiler.lisp`
~line 1444. Regression: `ansi-tests.lisp` deftests 3091/3092.

### "Layout shift broke an unrelated test" — it almost never did

This verdict has been wrong every time it's been reached.  Adding/resizing a
function does NOT flip unrelated tests on x64 Linux: the OR-3 fn-tagging +
SIGSEGV handler + NIL=#xDEAD0001 fixes closed that class, and a fuzz sweep
(`MODUS_FUZZ_FUNCALL_NOPS=64`, ~50K perturbation sites) over 8000 tests
produced **zero diff** vs baseline.  When a change appears to break something
unrelated, the cause is a real semantic regression — bisect to it.  Almost
always one of: an auto-extracted runtime macro now shadowing a validating
runtime function (the eval2-CLOS make-instance case), a missing rewrite, a
name collision (last-defun-wins), a subtag collision, or a missing gc-check.

If you still suspect layout, the fuzz knob settles it in one build: set
`MODUS_FUZZ_FUNCALL_NOPS=4`, rebuild, rerun the same range.  Bit-identical
results ⇒ not layout; go find the semantic bug.  (Do this before, not after,
spending hours on a "fragility" theory.)

## Fixpoint Build (`mvm/build-fixpoint.lisp`)

The fixpoint build combines source from multiple architectures into a single multi-arch binary.
It uses an `*override-fns*` dispatch system to select 32-bit vs 64-bit function variants at runtime.

### `*override-fns*` dispatch pitfall

Functions listed in `*override-fns*` get their `defun` names renamed: `c64-*` in text-64 source,
`c32-*` in text-32 source. A dispatch wrapper checks `(mem-ref #x48006D :u8)` at runtime.

**Critical**: Within text-64, multiple `arch-*.lisp` files define the same function with different
addresses (e.g., `edit-line-len` in `arch-i386.lisp`, `arch-x86.lisp`, `arch-aarch64.lisp`).
Since `arch-aarch64.lisp` loads LAST, `c64-edit-line-len` uses AArch64's address (`#x41112800`).
On x64, this address is past the 1GB identity map → page fault → silent hang.

**Rule**: Any function in `*override-fns*` that uses architecture-specific base addresses
MUST be overridden in `*fixpoint-extra-source*` with a dynamic version using `(ssh-ipc-base)`.
Functions already correctly overridden: `write-byte`, `edit-line-len`, `edit-set-line-len`,
`edit-cursor-pos`, `edit-set-cursor-pos`. Check before adding new address-dependent functions
to `*override-fns*`.

### Fixpoint SSH test

```bash
# Build Gen0
sbcl --script mvm/build-fixpoint.lisp

# Run x64→x64 SSH chain
./scripts/run-fixpoint-ssh.sh x64 x64

# Test
echo '(+ 1 2)' | ssh -p 2223 test@localhost   # → = 3
```

## Actor System

Cooperative scheduling on single core (SMP stubs exist, multi-core not yet active).

- **Actor 1**: Primordial (kernel-main → idle yield loop)
- **Actor 2**: Net-domain (owns all hardware: NIC polling, TCP/IP, ARP)
- **Actor 3+**: SSH handler actors (one per connection)

Per-actor heaps: 4MB each (`actor-heap-base + (id-1) << 22`).
Communication: mailbox messages via `send`/`try-receive`. Messages serialized through staging buffers.

Key globals for translator:
- `*aarch64-sched-lock-addr*`: Set to lock address for RESTORE-CONTEXT unlock. nil = no actor support.
- `*aarch64-serial-base/width/tx-poll*`: UART configuration per board.

## Pi Zero 2 W Hardware Notes

- **BCM2710A1** (Cortex-A53, 512MB, same as BCM2837 in RPi 3B)
- **USB**: Single micro-USB OTG, DWC2 in device/gadget mode
- **UART**: Mini UART at 0x3F215040 (not PL011), 32-bit stores, ALT5 on GPIO14/15
- **CDC-ECM**: Static IP 10.0.0.2, host 10.0.0.1, MAC 02:00:00:00:00:01
- **GPIO17**: Connected to RST pad for reset. Reset: `pinctrl set 17 op dl; sleep 0.3; pinctrl set 17 ip pn` (MUST set back to input-no-pull or default pull-down holds Pi in reset)
- **USB boot**: Requires OTP fuse programmed once via `scripts/fuse-pizero2w.sh`
- **Crypto**: Pre-compute Ed25519 host key + X25519 ephemeral at boot (saves ~10s per connection). USB keep-alive polling during crypto prevents NETDEV WATCHDOG timeout.
- **Host NAT**: `boot-pizero2w.sh` sets up iptables MASQUERADE for Pi internet access

## Networking Architecture

Shared source files between QEMU virt (E1000) and RPi (DWC2 CDC-ECM):
- `ip.lisp`, `crypto.lisp`, `ssh.lisp`, `http.lisp`, `http-client.lisp`, `aarch64-overrides.lisp`

Per-platform adapters provide: `e1000-send`, `e1000-receive`, `e1000-state-base`, `ssh-ipc-base`, allocation primitives (`make-array`, `aref`, `aset`), and actor address hooks.

Source load order matters — later files override earlier defuns:
```
arch-* → [actors] → NIC driver → ip → crypto → ssh → http → http-client →
aarch64-overrides → [actors-net-overrides] → [isolated-net]
```

## Testing

```bash
# QEMU SSH test
echo '(+ 1 2)' | ssh -p 2222 test@localhost   # → 3

# Pi Zero 2 W SSH test
ssh -o StrictHostKeyChecking=no -o ConnectTimeout=30 test@10.0.0.2
```

SSH credentials: username `test`, any password accepted (no real auth).

## Development Hosts

### modulator (Pi Zero 2 W — USB gadget for T420)
Connected to the T420 via USB, presenting as composite device: HID keyboard + mass storage + CDC ACM serial.

- **SSH**: `ssh modus@modulator`
- **Type at T420 console**: `ssh modus@modulator 'echo "(expr)" | sudo python3 ~/type.py'`
- **Force reboot T420**: `ssh modus@modulator 'sudo python3 ~/force-reboot.py'` (Ctrl+Alt+Delete — only works if BIOS/OS handles it)
- **Deploy image**: `scp /tmp/modus-i386-diag-ssh.img modus@modulator:/home/modus/modus.img` (T420 boots from this via mass storage gadget)
- **Gadget setup**: `~/setup-gadget.sh` (creates `/dev/hidg0` + mass storage backed by `~/modus.img`)
- **Boot helper**: `~/boot-helper.py` (sends ESC periodically to help T420 boot menu)
- **Note**: `(reboot)` from the Modus REPL may fail — if so, retry or physically power-cycle the T420

### modus-pi (Raspberry Pi — webcam + monitoring)
Has a USB webcam pointed at the T420 screen for remote VGA capture.

- **SSH**: `ssh modus@modus-pi`
- **Capture T420 screen**: `rpi-webcam.sh` (runs on this host, SSHes to modus-pi, captures frame, SCPs back `image.jpg`)
- **View screenshot**: Read `image.jpg` in working directory after capture
