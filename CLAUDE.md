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
  run.sh               ONE entry point for every QEMU run config (`run.sh list`)
  run-repl-eval.sh     shared helper: boot a REPL image, eval one expr, print
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
  cl-eval.lisp       3,575L  Eval/compile/load (mvm-eval), symbol-function table
  cl-types.lisp        519L  typep, coerce, numeric helpers
  ansi-bridge.lisp   1,888L  Test helpers (eqlt, scaffold, stubs)
  gc.lisp                    GC helper functions (Lisp-side)
  mvm-eval.lisp                 Production eval: compile→MVM bytecode→interpret
  interp.lisp                MVM bytecode interpreter (in-image)

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

Build: `sbcl --dynamic-space-size 12288 --script mvm/build-x64-linux.lisp`
(2048 was enough historically; the assembled blob is now ~17.6 MB and 2 GB
dies with an SBCL HEAP-EXHAUSTED-ERROR partway through BUILD-IMAGE.  Always
set `MODUS_ANSI_OUT` too — the default path is outside the worktree and two
concurrent builds overwrite each other.)
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
Eval/Compile/Load ✓  mvm-eval (compile→MVM bytecode→interpret) is production eval/load
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
    `eval`/`load` default to mvm-eval (compile to MVM bytecode + mvm-interpret)
    as of d3434e6 (WS3 flip, 2026-07-03).  Post-flip full ANSI: 17321/17318
    vs tree-walker 17311 (net positive).  MODUS_NO_MVM_EVAL=1 = rollback.
    Compile-caching (~20x for repeated forms) serves load/asdf patterns.
    bytecode→native at runtime (the JIT) is WS4.
[ ] compile-file → FASL
[ ] Full numeric tower (arbitrary bignums, ratios, full floats, complex)
[ ] Setf machinery (defsetf, define-setf-expander)
[✓] Everything else
```

### Self-hosting (WS3): FINISHED — the tree-walker is DELETED

There is ONE evaluator.  Production `eval`/`load` compile the form to MVM
bytecode via the self-hosted compiler and run it through `mvm-interpret`
(mvm-eval).  The tree-walker (`%eval-in-env`) is GONE from the repository —
deleted after every consumer was ported: the x64-Linux gate image, the
generic/gauntlet image, the bare-metal x64 runner (build-x64.lisp), the
Linux/AArch64 runner (build-aarch64-linux.lisp, verified natively on a
Pi 5), and the bare-metal AArch64 runner (build-aarch64.lisp).  The
deletion was census-gated: an instrumented build measured ZERO walker
fallback invocations across the full ANSI corpus and the asdf gauntlet.
An interp-closure shape mvm-eval cannot compile now SIGNALS an honest error
(*e2ic-fallback-count* is the diagnostic; nonzero = new capability gap).
E2SMOKE (the in-image compile→interpret self-test) passes at boot on
x64-Linux, bare x64, aarch64-Linux (native Pi 5), and bare aarch64.
Structural rules learned during the retirement: no `(eval …)` in
kernel-main/boot init paths; never define a duplicate defun name in
shared image source; when a crash marker flips with unrelated edits,
check the build-time SKIP/WARN list for garbage-execution paths (the
:li-func offset-0 class) before theorizing.  Follow-ups live in the task
list (aa64 bignum×GC poison band; aa64 safepoint-deadline port; the
compile-ash root fix = the bare-metal full-sweep blocker; GO-out-of-
unwind-protect-cleanup NLX).

## WS4 — the runtime JIT (bytecode→native at runtime): LANDED + FLIPPED ON

The self-hosted MVM compiler now JITs mvm-eval forms to native x86-64 at runtime
(translate-mvm-to-x64 → mmap exec page → call-relocation → %jit-call), and it is
the **DEFAULT** for the shipping `build-generic-cli` image (`*jit-on*` defaults
T; rollback = `MODUS_NO_JIT=1`).  Flip-clean, validated two ways: the full
64-shard ANSI gate is **reg=0 / gain=+2 / CHUNK-CRASH=0** JIT-on vs JIT-off
(17478 vs 17476), and a JIT-vs-interpret differential harness (422 boundary
forms, `/home/claude/jitdiff`) shows **zero JIT wrong-values**.  Correctness is
preserved by construction: any form the translator can't handle falls back to
mvm-interpret via the `%jit-translate-page` guard, and both backends share the
interp-safe boundary-literal codegen (post #197) so fallbacks are correct too.
Key fixes en route: call-relocation untag (5b866fa), out-of-module GC-trampoline
+ genarith slow-path (a3ea500/88e7519), bignum-safe `emit-u64` (88e7519 — the
`(ldb (byte 32 32) bignum)` high-word extract was broken by limitation #8; now
`(logand …)` + `(ash (floor V 2^31) -1)`).  Next: port the JIT to aarch64; then
back to Quicklisp.

## Build Commands

All builds: `sbcl --script <build-script>`

### Build taxonomy — clean images vs ANSI gate runners

Two kinds of build:

- **Clean images (what ships)** — a plain Modus kernel/REPL/SSH/CLI, no test
  corpus baked in.  These are the shipping artifacts: `build-generic-cli`
  (the `modus` CLI, SBCL-faithful flags + `--load`/`--eval`), `build-x64-repl`,
  `build-x64-ssh`, `build-uefi-repl`, the `build-aarch64-*`/`build-rpi-*`/
  `build-pizero2w-*`/`build-i386-*` images, etc.  (25 of the 29 build scripts.)
- **ANSI gate runners (NOT for shipping)** — bake the transformed ANSI test
  corpus into the kernel image and run it natively, for the conformance gate:
  `build-x64-linux` (the 64-shard Linux gate), `build-x64` (bare-metal QEMU),
  and the `build-aarch64`/`build-aarch64-linux` counterparts.  Only these 4
  compile the `.lsp` corpus in; their host-build logs are noisy with
  test-corpus warnings by design.  Each pair shares its ~4300-line harness via a
  common module — `mvm/build-ansi-common.lisp`, shared by all four; each of the
  4 builds is a thin wrapper that sets `*ansi-target-bare-metal*` and
  `*ansi-target-arch*` (`:x64` / `:aarch64`), loads the common file, then
  appends its own runner-source + build-image tail.  The common file's only
  arch divergence is the WS4 Stage-1 native-translator block it bakes in.  (Both extractions verified
  byte-identical to pre-split.)  Later end-state (gated on the WS4 JIT): the
  suite becomes a `--load`-able script run on a clean image, so nothing is baked.

### QEMU AArch64 (virt machine, E1000)
```bash
# SSH server (single-threaded)
sbcl --script mvm/build.lisp aarch64/bare/qemu/ssh
# Actors (cooperative multi-connection SSH)
sbcl --script mvm/build.lisp aarch64/bare/qemu/actors
# Isolated actors (Qubes-like, net-domain owns hardware)
sbcl --script mvm/build.lisp aarch64/bare/qemu/isolated
# REPL only (serial)
sbcl --script mvm/build.lisp aarch64/bare/qemu/repl
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
sbcl --script mvm/build.lisp aarch64/bare/rpi/repl   # Serial REPL
sbcl --script mvm/build-rpi-periph.lisp   # GPIO/SPI/I2C peripherals
```

### Running i386 (hosted Linux/i386 — the REAL CL image)

`mvm/build-i386-cli.lisp` is the hosted Modus CLI on 32 bits, built from the
**same** shared assembly (`mvm/build-cli-common.lisp`) as x64's `./modus` and
the aarch64 CLI: CL bridge, in-image MVM compiler + `mvm-eval`, RTEST,
tar/`install-tarball`, the hosted socket/storage/HTTP layer, the ASDF and
`:GENERA` surfaces, and the SBCL-faithful toplevel (`--eval`/`--load`/
`--script`/`--quit`, `~/.modusrc`, REPL).

```bash
./scripts/run-i386.sh build              # build the image
./scripts/run-i386.sh eval '(+ 1 2)'     # evaluate one form and exit
./scripts/run-i386.sh repl               # interactive REPL on stdin
./scripts/run-i386.sh exec ARGS...       # arbitrary invocation
./scripts/run-ladder-i386.sh <img> <tag> # the 22-library ladder (the gate)
```

**32-bit ELFs need `qemu-i386-static` — binfmt_misc is NOT registered here, so
running the binary directly does not fail loudly, it silently fails to exec.**
That cost hours once; the run scripts wrap it so it cannot happen again.

Build knobs (env; full list at the top of `mvm/build-i386-cli.lisp`):
`MODUS_I386_OUT` is production; `MODUS_I386_GC=0`, `MODUS_I386_BMP=0`,
`MODUS_I386_VL=<bytes>`, `MODUS_I386_GCSTRESS=<bytes>`,
`MODUS_I386_NO_CHECKED=1` are dev/triage only. `GCSTRESS` makes the collector
recollect at a fixed interval — a copying collector's corruption appears at the
SECOND collection, so this is what makes survival tests cheap.

i386 has a **native Cheney collector** (`i386-emit-gc-trampoline` in
`translate-i386.lisp`), the third arch arm after x64 and aarch64, **on by
default**, with BOTH conservative-root bitmaps (object-start + cons-kind) on;
the boot stub mmaps them because `gc.lisp`'s `%gc-bitmap-init` allocates through
a trap i386 does not implement. It is gated on `*i386-linux-mode*`: bare-metal
i386 has no GC metadata or bitmaps and keeps `int $0x31`.

The arena mmap is `+linux-i386-heap-size+` = two 256 MB semispaces **plus a
16 MB `+linux-i386-gc-guard+`**. That guard is not optional and it is not an
i386 invention — it is x64's `+linux-x64-gc-guard+` (c9c6278), which the i386
port originally omitted. **`:gc-check` tests `VA < VL` BEFORE an allocation
whose size it does not know**, so the alloc that follows a passing check
overshoots VL by up to that object's whole size. In the FIRST semispace the
overshoot lands in the (mapped) second semispace and is harmless (copy_object's
read pointer trails its write pointer by a constant, so a straddling object
still copies correctly); in the SECOND semispace `from_end` sat only
`+linux-i386-heap-alloc-start+` = **512 bytes** below the end of the mapping, so
once a process reached generation 2 any allocation bigger than 512 bytes that
tripped the check ran off the mmap and SIGSEGV'd inside its own initialising
stores. That was ladder defect **B3** — 16 of the 22 libraries. Residual, and
identical on x64: a SINGLE allocation larger than the guard still overruns; the
real cure is a size-aware `:gc-check`, a shared-compiler change.
`boot-linux-aarch64.lisp` still has the un-guarded shape (heap 0x38000000,
midpoint 0x1C000000 → the same 512-byte margin) and is a live suspect there.

**The i386 arch slots** — everything this wrapper is allowed to contain — are
`exit` = syscall 1 (int 0x80 numbering); the i386 file-I/O syscall numbers and
`stat64`/`fstat64` struct offsets; the argv/envp reader, which walks the copy
the boot stub STAGED into the BSS at `0x10009000` rather than the live initial
stack (4-byte slots, and the kernel stack at `0x40800390` is above the 2^30
ceiling a tagged `mem-ref` address can express); and `*cstr-scratch*` /
`*io-buf-addr*`, which must sit in the `0x10004000..0x10009000` BSS window
because i386's heap is at `0x30000000` and every syscall address travels as a
tagged fixnum. There is **no in-image JIT** on i386 (`*JIT-ON*` is forced NIL) —
`translate-i386.lisp` builds the image but has no runtime arm; `mvm-eval` falls
back to `mvm-interpret`, which is correct, just slower.

**RETIRED (2026-08 convergence):** `MODUS_I386_LAYER=1..5` and the ~1300-line
baked probe suite (`run-i386.sh test/gc/bulk/chain/argv/probe N`, including the
SHA-256 GC-survival gate that needed `net/crypto.lisp` baked in). A shipping
image bakes no test corpus ("Build taxonomy" above), and that suite is precisely
what kept i386 on its own build lineage — which is why the library ladder and
alexandria's own test suite had never run on 32 bits at all. Recover from git
history at `ba693fa`; the replacement gate is the ladder.

### QEMU i386 — LEGACY mini-Lisp (32-bit x86, Multiboot)

The builds below run `mvm/repl-source.lisp`, a separate 708-line toy Lisp with
its own reader/printer/evaluator — **not** the CL stack above. Retiring this
second Lisp is its own workstream.

```bash
sbcl --script mvm/build-i386-repl.lisp    # Serial REPL
sbcl --script mvm/build.lisp i386/bare/qemu/ssh   # SSH (NE2000 ISA NIC)
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

### Per-region GC, stage 1: the metadata block is a REGION CONTROL BLOCK

The GC metadata is no longer global.  `mvm/compiler.lisp` owns the layout:
eight `+GC-OFF-*+` OFFSETS (from/to/size/stack_base/count/saved sp+alloc+limit)
into a 64-byte block, `+GC-REGION-0-BASE+` = `0x10000040`, and
`+GC-REGION-ADDR+` = `0x10000F08`, one word holding the ACTIVE block's raw
address.  `+GC-FROM-START-ADDR+` and friends still exist with their old values;
they now spell "region 0's block, field F", so **no boot descriptor changed**.

**ZERO MEANS REGION 0.** Nothing writes `0x10000F08` until something creates a
second region, so BSS zero-fill (Linux) and unwritten-metadata-reads-as-zero
(bare metal, the same assumption `%gc-bitmap-base` already degrades on) both
answer "region 0".  A single-region image behaves exactly as before.

Region-aware readers: `mvm/gc.lisp` (all field access is `(+ (%gc-region) OFF)`)
and translate-x64's `emit-gc-trampoline` (`emit-load-gc-region` → `[reg+OFF]`).
**NOT region-aware, and commented as such**: translate-aarch64's GC shim,
translate-i386's collector, and translate-x64's page/pinning collector — they
read region 0 unconditionally, which is correct until those targets create a
second region.  Porting is the same edit: load the base once, index off it.

API in `gc.lisp`: `%gc-region-init` (write a block anywhere the caller owns —
BSS, a carved guard band, or an actor struct), `%gc-region-enter` (park the
mutator's alloc ptr/limit in the region you leave, load the one you enter —
exactly `actors.lisp`'s x24/x25 context switch), `%gc-region-shrink`,
`%gc-collect-here` (pull the limit to the pointer so the next `:gc-check`
trips — the real collector, not a side door).  `%gc-meta-scale` DERIVES whether
this target stores metadata raw (x64) or SHL'd (aarch64/i386) by asking which
reading brackets the alloc pointer, so there is no per-target knob.

Soundness is `net/actors.lisp`'s: each actor has a private alloc ptr/limit and
messages are term-serialized, so no actor holds a pointer into another's
region.  **Stage 1 does not enforce that** — a pointer stored from region A
into region B dangles when B is collected.

Acceptance: `./modus --script test/region-gc.lisp` — carves two 16 MB regions
off the top of region 0's semispaces, collects ONE, and checks the other two
are bit-for-bit unchanged (23 checks).  Note the ordinary 200k-allocation
stress collects **zero** times on an 896 MB semispace (measured); use
`%gc-collect-here` or `MODUS_GC_R14` if you mean to exercise the collector.

### Per-region GC, stage 2: the ROOT SET is a property of the region

The collector's stack scan is `[root_sp, stack_base)` and **both ends are
fields of the region being collected** (`+GC-OFF-SAVED-SP+` / `+GC-OFF-STACK-
BASE+`).  Collecting region N examines N's roots and nothing else — no global
stack base, no other region's window.

**`root_sp == 0` MEANS "THIS REGION'S ACTOR IS RUNNING"**, the same zero-is-the-
historic-answer trick stage 1 used, so nothing needed initialising anywhere:

| state | root window low end | who writes it |
|---|---|---|
| running | the LIVE SP at collector entry (x64: RBP, below all twelve pushed registers, so the register file is in the window; aarch64: its shim writes the same value into the field) | the collector entry point |
| parked | the SP the context switch recorded on that actor's OWN stack, where the switch also spilled its registers | `%gc-region-park` |

The two writers can never be live at once — a region is either running or
parked.  `translate-x64`'s trampoline branches on `saved_sp = 0` because it has
RBP in hand and no shim; the Lisp `%gc-collect` just reads the field, which is
correct in both cases.  A parked window is size-capped at
`+GC-MAX-PARKED-WINDOW+` (16 MB) since that low end came from memory rather
than a register; over the cap the window collapses to EMPTY rather than walking
unmapped pages.

New API in `gc.lisp`: `%gc-region-park` (rcb sp), `%gc-region-unpark`,
`%gc-region-parked-p`, `%gc-region-switch` (rcb sp) — park the region you leave
*and* its roots, enter and un-park the one you arrive in.  **The SP is an
ARGUMENT** because no MVM primitive yields the stack pointer as a value on every
target (`:save-ctx` is not implemented on x64 at all); the value already exists
where it is needed, since `actors.lisp`'s `yield` has just written the outgoing
actor's SP to its struct at `+0x08`.  Nothing in `net/actors.lisp` owns a region
yet (all actors bump-allocate inside region 0), so nothing calls it outside the
tests — wiring that up is stage 3.

**NOT partitioned, and it is the one root set stage 2 does not split**:
`%gc-scan-globals` / the trampoline's globals block (globals alist, symbol +
keyword + package intern tables, MV extras).  Any region's actor can store its
own object into a global, so every region scans them; it is a no-op for
anything that does not point into the region being collected, hence
conservative-safe, but it is not per-region.

**The soundness assumption, stated because stage 2 rests on it**: per-region
collection is correct only because no actor can hold a pointer into another
actor's region (`net/actors.lisp` term-serializes every message — copied, never
shared).  **The collector does NOT enforce that and there is no write barrier**;
a pointer stored from region A into region B dangles silently when B is
collected.  `%gc-count-foreign-refs (start end other-start other-size)` is the
debug-mode audit — exact, non-allocating, works in either direction — and
`test/region-gc-roots.lisp` runs it both ways over the real heaps after a real
collection, plus once as a positive control on a span known to hold exactly one
such pointer.  The collector never calls it (it is an O(heap) sweep).

Acceptance: `./modus --script test/region-gc-roots.lisp` (32 checks).  Two
identical chains in region 1: A rooted ONLY from the parked window, B rooted
ONLY from the live stack (which belongs to region 0's actor).  Collect region 1
twice while parked — A survives, B does not; live bytes `16*N+16`, and `32*N+16`
if the live stack had been scanned too.  Then the **negative control in the same
binary**: un-park it, and every expectation inverts (chain C on the live stack
survives, the parked slot is never even rewritten).

`./modus --script test/gc-forced-stress.lisp` (4 checks) is the collector
regression: 20 forced collections, 20,000 live structures, checksum
199990000.

Two traps this cost time on, both pre-existing:
- **`%gc-count` reports HALF the count on x64.** The field is stored raw there
  and `(mem-ref addr :u64)` halves — the defect `gc.lisp`'s word-access block
  documents.  It is right where it is *used* (aarch64/i386 store fields SHL'd,
  so the halving cancels); test code must read it with `%gc-meta-read`.
- **`%gc-collect-here` at an interpreted `--script` toplevel breaks the next
  `(format t …)`** (it re-runs its control string, then signals).  Reproduced on
  an unmodified HEAD build, so it is a property of the mvm-eval toplevel, not of
  per-region GC — but it means a scripted stress measures that instead of the
  collector.  Drive forced collections from compiled in-image code.

### Per-region GC, stage 3: a REGION is a property of an ACTOR

Stage 1 made the heap per-region, stage 2 the roots.  Stage 3 makes a region
something an ACTOR OWNS, which is what PLAN.md's "Stage 3: Per-actor SMP" needs.
Three parts, and **none of them changes a single-region image**:

**1. The active-region word stops being ONE word.**  It is now the CPU-0 entry
of a per-CPU array based at the same address: `+GC-REGION-ADDR+ + 8*cpu_id`, so
a uniprocessor image reads and writes exactly the word it always did, at exactly
the address stages 1 and 2 used.  "Which CPU" is the `:CPU-ID` slot at
`+GC-PERCPU-CPU-ID-OFF+` (=16) that **every** boot descriptor's
`percpu-layout-fn` already allocates — no per-CPU block grows, no
`+*-PERCPU-STRIDE+` moves.  Both readers go through one place: `%GC-REGION-CELL`
in `gc.lisp`, `EMIT-LOAD-GC-REGION` in translate-x64.

**IT IS OFF, on both sides, and that is deliberate**: a per-CPU read is GS: on
x64, FS: on i386, TPIDR_EL1 on aarch64, and **on hosted Linux the GS base is 0**,
so an unguarded `GS:[16]` reads absolute address 16 and takes SIGSEGV.  The Lisp
side branches on `+GC-REGION-PERCPU-ADDR+` (`0x10000FF8`), a BSS word that is
zero in every image ever built — nothing writes it; the native side branches on
the host flag `*X64-GC-REGION-PERCPU*`, default NIL.  Measured, not asserted:
with the flag off `EMIT-LOAD-GC-REGION` and the **whole 826-byte x64 GC
trampoline** are byte-identical to a build of the HEAD source loaded over the
new one in the same image.  Turning it on is two deliberate acts and both must
happen together or the collector and the mutator read different cells —
`build-checks.lisp` CHECK G now ratchets both literals in `gc.lisp`.

**2. An actor names its region by POINTER, in its struct at `+0x68`.**
`ZERO MEANS REGION 0`, so an actor that owns no region is the actor of today and
nothing needs initialising — `SPAWN` does not touch the slot.  `+0x68` and not
the other reserved slot `+0x28` because `+0x28` is inside the save area
`:SAVE-CTX` writes through (it gets `cur+0x08` and stores SP/alloc/limit/V4 at
pa+0x00…0x18, the continuation at pa+0x28 = struct **+0x30**, and on aarch64
obj-alloc/obj-limit at pa+0x68/0x70 = struct **+0x70/+0x78**); struct +0x68 is
pa+0x60, which nothing writes.  `net/actors.lisp` gained `ACTOR-REGION-RAW`,
`ACTOR-REGION-HOP` (YIELD's and RECEIVE's save paths, immediately before
`restore-context`) and `ACTOR-REGION-RESUME` (the idle scheduler and
`ACTOR-EXIT`, where there is no outgoing actor to park).  The SP it parks at is
read back out of `+0x08`, where `SAVE-CONTEXT` has just written it.  **The guard
is the compatibility story**: when neither side of a switch names a region, the
hop makes no call and performs no write.

**A net/-only image does not link `mvm/gc.lisp`**, so those four calls land on
`bare-runtime-stubs.lisp`'s `%UNRESOLVED-FN` (→ NIL) there — measured, the
aarch64 bare actors image goes 4760 unresolved calls / 24 functions → 4771 / 28,
the extra 11 being the 4 new names plus 2 `GENERIC-MULTIPLY` and 5
`NUMERIC-EQUAL-P` from the new helpers' own arithmetic.  Harmless while no actor
owns a region, but **giving an actor a region on bare metal means linking
`mvm/gc.lisp` into that image first.**

**3. The aarch64 and i386 collectors are region-aware.**  Both used to read
region 0's absolute field addresses.  Now: aarch64's native collector and its
Lisp-calling shim both resolve the base once (`A64-LOAD-GC-REGION`), i386's
trampoline resolves it into a BSS scratch dword at entry (it has no register to
spare — EBX/ESI/EDI are collector state, EBP is every loop's cursor, scan_word
clobbers EAX/ECX/EDX) and hoists the three read-only fields out of it.  Both
also grew stage 2's **parked-window branch**, which they never had: `saved_sp==0`
= RUNNING (low end is the live SP), non-zero = PARKED and the window is capped
by SIZE, not by an address.  `%gc-collect`'s old absolute floor (`0x07200000`,
one board's stack guard) is gone for the same reason, replaced by the same size
cap translate-x64 has used since stage 2.

**A COLLECTOR ENTRY POINT THAT WRITES `+0x28` MUST PUT BACK WHAT IT FOUND.**
aarch64's shim is the only one in the tree that writes the field at all
(translate-x64 holds RBP, translate-i386 holds ESP, so both only read it), and
it needs BOTH halves or it is wrong:
- it must not write when the field says PARKED — that value is an actor's own
  recorded SP, and **this case is reachable**, because `%gc-collect-here`
  collects the ACTIVE region and the stage-1/2/3 selftests deliberately make a
  region active *while parked* and collect it from there;
- it must restore the original on the way out — nothing else ever clears the
  field (`%gc-collect` reads it into a local and clamps the local), so a
  conditional write with no restore leaves the first collection's live SP behind
  **forever**, and the second collection scans `[SP-of-the-first, stack_base)`.
  Stacks grow down, so a deeper second collection — the normal case, since GC
  triggers from arbitrary allocation sites at arbitrary depths — gets a window
  starting ABOVE its own live frames and misses all of them, the shim's own
  232-byte register-save area included.

All three variants were **measured on a bare-metal shim-path RPi image under
QEMU** (`*aarch64-gc-native-mcgc*` NIL), with `test/region-gc-depth.lisp`'s
probe (two forced collections of one carved region, the second 200 frames
deeper, a 2000-link chain rooted only by the deep frame's local) and the stage-2
parked test:

| shim behaviour | `saved_sp` after GC #1 | live bytes after the DEEP GC #2 | parked chain A forwarded? | parked slot → new / old space |
|---|---|---|---|---|
| conditional write, no restore | `0x07FFE028` stale | **16** ✗ | 1 ✓ | 1 / 0 ✓ |
| unconditional write (the pre-stage-3 shape) | `0x07FFE028` stale | 32016 ✓ | **0** ✗ | **0 / 1** ✗ |
| **write-if-RUNNING + restore (shipped)** | **0** ✓ | **32016** ✓ | 1 ✓ | 1 / 0 ✓ |

Correct answer for live bytes is `16*2000+16 = 32016`; `16` means the whole
chain was never copied.  The unconditional row also prints `%gc-collect`'s
**`S`** marker (stack-scan skipped): clobbering the parked SP with the live one
inverts the window, so it collapses to empty.  Only the third row is correct on
both axes — reverting the guard is *not* an equivalent fix.

`./modus --script test/region-gc-depth.lisp` (7 checks) is that regression, and
it is portable: it passes on x64 and on the aarch64 NATIVE arm by construction
(neither writes the field), so it is a guard there and the real subject on the
shim path.

Acceptance: `./modus --script test/region-gc-actors.lisp` (53 checks).  Three
actor structs in the carved guard band, in `net/actors.lisp`'s own layout; actor
0's region slot left ZERO and required to resolve to region 0's block; actors 1
and 2 owning carved regions and round-robin switched 1,2,1,2 with a FORCED
collection at each stop while parked on their own 512-byte root windows.  Each
region's own count rises, its chain still walks read back through its parked
slot, live bytes are `16*N+16`, the other region and region 0 are bit-for-bit
unchanged, and `%gc-count-foreign-refs` is 0 in every direction with a POSITIVE
CONTROL (each window holds exactly one pointer) so the oracle can answer
non-zero.

**The carve in all three selftests is now ADAPTIVE** (2 MB regions below a 128 MB
semispace, 16 MB above) for one reason: the bare-metal RPi CL image has 56 MB
semispaces and used to skip — and it is the **only** target whose native
collector can be booted here.  With that, stages 1, 2 AND 3 all RUN on the
region-aware **aarch64 native collector** under `qemu-system-aarch64 -M raspi3b`,
including the brand-new parked-window branch (chain A, rooted only from the
parked window, survives; chain B, rooted only from the live stack, does not).
`(%gc-forced-stress 2000 5)` there is `(1 6 2000 1999000 0 5)`.

**`net/cooperative-atomics.lisp` was reconciled, not silently invalidated.**  Its
three checked facts all still hold and for the same reasons — stage 3 added no
yield site, made no scheduler preemptive and installed no signal handler, and no
image runs native threads.  The macros did not become safe either.  The header
now says so, and says the thing that matters: **fact 1 bounds only THIS CPU**, so
per-actor SMP and those macros cannot ship together unchanged.

### Per-region GC, stage 3 ON HOSTED x64: the actors are REAL (7af5212 … 7c25356)

Stage 3 wired `%GC-REGION-SWITCH` into `net/actors.lisp`'s `YIELD` and
`RECEIVE`, and **those call sites had never executed**, because that file is
bare-metal-only and is in no hosted image.  It is now linked into `./modus`, so
on the one target runnable here the mechanism is driven by an actual scheduler
instead of a harness standing in for one.  Four steps, four tests, all on
hosted x86-64:

| test | what it is the first execution of |
|---|---|
| `test/hosted-ctx-switch.lisp` (12) | `+op-save-ctx+` / `+op-restore-ctx+` on x64 |
| `test/hosted-percpu.lisp` (19) | `PERCPU-REF`/`-SET` (`GS:[disp32]`) in a Linux process |
| `test/hosted-actors.lisp` (20) | spawn / yield / send / receive / term serialisation |
| `test/hosted-actor-regions.lisp` (48) | a NON-ZERO actor `+0x68`, i.e. stage 3 for real |

**`translate-x64` DOES implement save-ctx/restore-ctx and always did** — `gc.lisp`
used to claim otherwise; the comment is fixed.  It works unmodified: 200
coroutine round trips, both sides progressing, locals intact.  It saves
RSP/RBX/RBP + a continuation and NOTHING else — not V0-V3/V5-V8, which
aarch64's equivalent pushes — and that is sound only because the compiler keeps
let-bound locals in FRAME SLOTS, on the stack RSP restores.  The test holds a
sentinel fixnum AND a heap cons across every switch to make that a measurement.

**Memory comes from the heap, not from an invented address.**  A hosted process
owns no fixed RAM, so `net/hosted-actors.lisp` derives all twelve address hooks
by `%GC-REGION-SHRINK`ing region 0 by 48 MB — the same carve gc.lisp's selftests
use — and laying the actor table, mailbox pool, staging buffers, per-CPU block
and 64 KB actor stacks out in the freed top of the semispaces.

**Per-CPU storage is `arch_prctl(ARCH_SET_GS)`, syscall 158.**  Not a
plain-memory override: `PERCPU-REF`/`PERCPU-SET` are COMPILER INTRINSICS
(op-name hash dispatch), so no `defun` can shadow them and overriding would mean
editing every percpu call site in the file under test.  The test cross-checks
every slot through `%GC-READ64`, which does not go through GS, so it cannot pass
on a segment base pointing somewhere else.  ***`*X64-GC-REGION-PERCPU*` IS STILL
OFF*** — a GS base is a NECESSARY condition for that flag, not the same
decision.

**Three things the port needed, each an arch fact:**
- **`WRITE-BYTE` HAS TWO INCOMPATIBLE MEANINGS** — a board's ONE-argument console
  byte vs ANSI's `(write-byte byte stream)`.  `net/actors.lisp`'s three progress
  markers now use `WRITE-CHAR-SERIAL` (MVM trap `#x0300`, present on every
  target).  This is the ONLY edit to that file.
- **`SPIN-LOCK`/`SPIN-UNLOCK` MUST BE NO-OPS ON x64**, and not merely because one
  core needs no lock: `net/actors.lisp` hands the RELEASE to `RESTORE-CONTEXT`,
  which aarch64's `+op-restore-ctx+` does (via `*AARCH64-SCHED-LOCK-ADDR*`) and
  **translate-x64's does not** — so a real lock would stay held across every
  switch and the next `SPIN-LOCK` would spin forever.  Open follow-up.
- **`AP-SCHEDULER` is overridden**: the bare-metal one uses trap `#x0400`
  (translate-x64 does not decode it — falls through to a real `INT 0x30`) then
  CLI / STI+HLT.  Hosted, an empty run queue is a DEADLOCK, not an idle CPU, so
  it counts the event and returns; every test asserts the count is 0.

**A NIL-TERMINATED LIST CANNOT BE SENT** by `TERM-SIZE`/`TERM-ENCODE` in a hosted
image.  They open with `(zerop val)`, which spots the end of a list on bare
metal WHERE NIL IS ZERO; hosted, NIL is the immediate `#xDEAD0001`, all three of
`zerop`/`consp`/`numberp` are false, and it falls through to `SOFT-SUBTAG`,
which dereferences it.  Dotted pairs of non-zero fixnums work and exercise the
same cons path.  Fixing the general case changes `net/actors.lisp`'s semantics.

**WHY STEP C (no regions) BEFORE STEP D.**  With every actor in region 0, whose
stack_base is the PROCESS stack base, a collection while an actor runs on a band
stack would scan terabytes of unmapped VA.  Step C therefore ASSERTS region 0
never collects; step D fixes it, because a region's stack_base becomes ITS
ACTOR'S STACK TOP and both the running and the parked window then lie inside one
64 KB stack.  **Step D is not just a heap partition — it is what makes
collecting DURING an actor possible at all.**

**THE ORDERING ARGUMENT, which is a real correctness hazard.**  x64's SAVE-CTX
does not save R12/R14, and that omission is what makes the hop work:
`ACTOR-REGION-HOP` runs after `SAVE-CONTEXT` recorded the outgoing SP and
immediately before `RESTORE-CONTEXT`, so `%GC-REGION-ENTER`'s load of the
arriving region's alloc pointer/limit SURVIVES the restore (which moves only
RSP/RBX/RBP).  Between the two, the CPU is on the OUTGOING actor's stack holding
the ARRIVING region's allocation registers — safe only because nothing in that
window allocates.  On aarch64 the window closes differently (its RESTORE-CTX
reloads x24/x25 from the arriving actor's save area, so struct +0x10/+0x18 and
the region's parked pair must agree there); this is measured on x64 only.

Stage-3-for-real numbers: 2000-cons chain live in each worker's frame across 16
forced collections of its OWN region, 0 walk failures; every message re-checked
and re-sent AFTER a collection of the region it was decoded into; counts
16/16 → 18/17 with region 0 at 0 throughout; the other region bit-for-bit
identical (heap AND control block) across each collection; parked-window live
bytes 32048 = 16*2000 + 3 junk conses; `%GC-COUNT-FOREIGN-REFS` 0 in every
direction with an EXACT synthetic control (one planted pointer → 1) and a REAL
one (each actor's parked window → 2 pointers into its own region).

**KNOWN GAP, x64 parked windows.** `SAVE-CTX` spills RBX (V4) into the actor
STRUCT (+0x20), which is in the band and therefore OUTSIDE the parked window
`[SP, stack_base)`.  A Lisp POINTER left in RBX by a parked actor is restored on
resume but is NOT scanned or forwarded while parked, so it would dangle if its
object moved.  It is not reachable today — `SAVE-CONTEXT` is only ever called
from `YIELD` and `RECEIVE`, whose V4 holds a fixnum — but it is the thing to fix
before an actor can be parked from an arbitrary call site.  aarch64 has the same
shape for x19/x24/x25.

**`net/cooperative-atomics.lisp`'s three facts still hold**, re-checked: no new
yield site (`compile-loop`'s is still the only `(emit-ir :yield)`), no
preemption (hosted safepoint stub still NIL), no new signal handler, and the
hosted scheduler is COOPERATIVE and SINGLE-CORE — its only switch is an explicit
`YIELD`/`RECEIVE`.  Linking a live actor scheduler into the hosted image does
not change that; what would is a second CPU.

The aarch64 CLI is **per-function BIT-IDENTICAL** across this work
(`scripts/fndiff.py`: 4536/4536 identical, native byte delta +0).  Its FILE size
grows 304 bytes, and that is not code: `build-image` embeds the image's own
source text (`embed-source-blob`), and `mvm/gc.lisp`'s corrected comment is 301
chars longer.  **Judge these builds per-function, never by image size.**

### Per-region GC, stage 4: the collector is REENTRANT — the global collection lock is GONE

`%HA-LOCKED-COLLECT-HERE` (0fdb21a) serialized every collection behind one lock
and named three reasons.  All three are answered and the lock is removed; it is
KEPT SWITCHED OFF (`%HA-SET-COLLECT-SERIALIZED`) so a future corruption can be
bisected against the serialized path in the same binary.

**FIRST, WHICH COLLECTOR RUNS WHERE, because it reframes the whole problem.**
The three fixed shared words `0x10000100/0x10000108/0x10000110` belong to
`mvm/gc.lisp`'s **Lisp** `%GC-COLLECT`, which runs on the **aarch64 SHIM path**
and on any target with no native arm.  It is **not** what runs on the target
that has threads: hosted x86-64 — like bare x64 and i386 — collects in a NATIVE
trampoline (`translate-x64`'s `emit-gc-trampoline`) that keeps every
per-collection value in **registers** (R13 free ptr, RBX from_start, RCX
from_end, RDX stack_base, R10 scan ptr), pushed on the collecting thread's own
stack.  It was private per CPU by construction and always was.

| the lock's reason | verdict |
|---|---|
| three fixed shared scratch words | real, but only on the **Lisp** arm.  Now a 32-byte block **per CPU** (`%GC-SCRATCH-CELL`), threaded through the whole collector as an argument `SC` resolved once in `%GC-COLLECT`.  Zero config word = the historic block, so single-threaded images are unchanged. |
| the shared **globals root set** | **not a race between two collectors.**  `%GC-FORWARD-SLOT` rewrites a slot only when its value points into **this** region's from-space, and two regions' from-spaces are disjoint, so two collectors' writes are disjoint.  Full argument (incl. the read side and the separate MUTATOR hazard it does *not* cover) lives in `mvm/gc.lisp`. |
| *(not named by the lock)* the shared **BITMAPS** | **the one live defect.**  See below. |

**THE BITMAP HAZARD THE LOCK WAS SILENTLY COVERING.**  One pair of bitmaps
covers the whole heap at 1 bit / 16-byte granule.  The widest **unLOCKed**
read-modify-write on them is `translate-x64`'s `BTS [base], idx` at 64-bit
operand size, which by the Intel definition touches the **eight-byte unit** at
`base + 8*(idx >> 6)` — eight bitmap bytes = **1024 heap bytes** — and it is on
the allocation fast path.  So two regions whose boundary falls inside such a
unit share a read-modify-write word, and a mutator in one can lose a
collector's survivor bit in the other.  **A lost start bit is not benign**:
`%gc-forward-slot` and `scan_word` both reject a candidate whose granule is not
a recorded start, so the object is silently not forwarded.

Measured on hosted x64 with the invariant's own oracle, **before** the fix:
`old r1-to align mask = 2`, `old r2-to align mask = 2`, `r0 align mask = 2` —
both carved regions' **to-spaces** were 512-aligned, because region 0's
`space_size` was `512 mod 1024` (`+LINUX-X64-HEAP-ALLOC-START+` was `0x200`
against a 1024-aligned midpoint) and the carve derived the to-side by adding it.
Fixed at three layers: the boot constant `0x200 -> 0x400`, `%HA-CARVE` rounding
both bases up to page_base's congruence (with 4 KB of headroom reserved in
`NEW0`), and `%GC-REGION-INIT` **measuring** it into a violation ledger.
**Align against page_base, never against from0** — from0 is whichever semispace
is currently the from-space and it swaps on every collection.

**REMOVING A LOCK IS ONLY TESTED IF THE COLLECTIONS OVERLAP IN TIME**, and
`test/hosted-thread-gc.lisp`'s 39 checks all pass with the lock still in.  So
the trampoline itself counts, with LOCKed instructions, at the true boundaries
of a collection: `lock inc [cur]` / witness if `cur >= 2` at entry, `lock dec`
at the restore label, plus a bounded **in-collection barrier** (`[barrier]` is a
spin budget; a collector alone at entry waits for a second).  Gated `:LINUX`, so
bare-metal x64 emission is byte-identical.

**The negative control is the old lock, in the same binary, on the same
workload** — `test/hosted-thread-gc-concurrent.lisp` (55 checks) runs it twice,
one flag apart:

| | overlap witnesses | barrier meetings | collectors inside at end |
|---|---|---|---|
| concurrent | **48** | **51** | 0 |
| serialized | **0** | **0** | 0 |

A serialized implementation FAILS the concurrent arm.
`test/hosted-thread-gc-stress.lisp` (675 checks) then runs 24 whole two-thread
rounds — 64 messages, a 2000-cons chain live on every actor, a forced collection
per message on both threads: **3120 collections, 1541 witnesses, 1978 barrier
meetings, zero chain-survival failures, zero foreign refs, zero alignment
violations.**

**AND THE HONEST THROUGHPUT ANSWER: on this workload there isn't one.**  The
flag exists so the two paths can be MEASURED, so they were — 12 rounds x 64
messages x 2 threads x 2000-cons chains, barrier off, same binary:

| | wall (3 reps) | overlap witnesses |
|---|---|---|
| concurrent | 3.36 / 3.25 / 3.31 s | 528 / 522 / 517 |
| serialized | 3.21 / 3.45 / 3.25 s | 0 / 0 / 0 |

Indistinguishable, and that is expected rather than disappointing: a forced
collection of a 16 MB region holding ~2000 live conses takes microseconds, so
the run is dominated by message passing and chain walking and the serialized
arm almost never actually blocks.  **Removing the lock bought correctness and
structure, not throughput on this workload** — the throughput case needs
regions large enough that a collection is long compared to the interval between
them.  Do not quote a speedup here; there isn't one to quote.

**Two things the 10x runs found, both mine and not the collector's:**
- **Heap-window uniformity must not be asserted on a hosted target.**
  `%GC-HEAP-WINDOW-UNIFORM-P` measures the precondition of the *Lisp*
  collector's torn-read argument (two 32-bit stores).  Measured over 40 fresh
  processes, region 0 had both semispaces in one 4 GiB window in **26** — the
  ~1.8 GB mmap straddles a boundary about a third of the time under ASLR.  It
  bounds nothing on x64 (the native slot rewrite is one aligned 8-byte store),
  so the test reports it and asserts neither.  The useful conclusion: making the
  **Lisp** collector concurrent on a hosted target needs a 64-bit store, not a
  layout assumption.
- **The message log would have run into the per-CPU block.**  0x800 bytes at
  `band+0x800` = 64 entries, with `PERCPU-DATA-BASE` immediately above; nothing
  bounded the index.  Unreachable at NMSG=48, found while sizing the stress.
  `%HA-LOG-CAP` now bounds both writers.

**EXECUTED, NOT MERELY COMPILED.**  x86-64 cannot run `%GC-COLLECT`, so the Lisp
arm is driven on the one bootable target that can:
`MODUS_RPI_GC_SHIM=1 sbcl --script mvm/build-rpi-cl-repl.lisp` turns the native
aarch64 collector off so every gc-check lands in the shim, which CALLs
`%GC-COLLECT`.  Under `qemu-system-aarch64 -M raspi3b`:
`(%gc-forced-stress 2000 5)` → `(1 6 2000 1999000 0 5)` (the value documented
for that image on its *native* arm) and `(%gc-forced-stress 500 3)` →
`(6 9 500 124750 0 3)`; violations 0.  Default OFF — the shipping image is
byte-identical.  The per-CPU **addressing** is separately exercised on the
threaded target: thread 1 gets scratch entry 0, thread 2 entry 1, +32 bytes.

`build-checks.lisp` CHECK G now fails the build if `mvm/gc.lisp` still spells
`0x10000108` or `0x10000110` as a literal — verified by reintroducing one.

**AND THE ALIGNMENT LEDGER HAS A POSITIVE CONTROL**, because after the fix it
reads zero everywhere, which means its increment path had never executed — the
same "a check that can only answer 0" trap 0fdb21a caught in its own checksum.
`%HA-ALIGN-CONTROL` initialises a throwaway block five times (aligned, then
from / to / size / all-three nudged by exactly the 512 the shipping carve was
off by) and the test asserts masks `0 / 1 / 2 / 4 / 7` and that
`%GC-REGION-INIT` counted **4 of 5**.

### Threads become USABLE: the shared runtime tables, blocking receive, mutex/condvar, SLEEP

Stage 4 gave hosted x86-64 two real OS threads with independent, concurrent
collections.  What it did NOT give them was the ability to run **Lisp**.  Every
threaded selftest's header says the workload touches "arithmetic, raw memory
access and message passing only — no FORMAT, INTERN, EVAL, or symbol/keyword
literal", and that restriction was the **ceiling**, not an accident.  Four
things, in the order they matter:

**1. THE SHARED TABLES ARE LOCKED, AND THE LOCK ALSO CHOOSES THE HEAP.**  The
globals hash table (`0x10000080`), the symbol / keyword / package intern tables
(`0x10000088` / `0x10000148` / `0x10000170`) and `%MACRO-PKG-*` are one set of
shared mutable structures.  A concurrent `PUTHASH` is not merely a lost entry:
two threads that both miss the same `GETHASH` both allocate a symbol and both
store it, so **`(eq 'foo 'foo)` stops holding across threads**.

`%RT-ENTER` / `%RT-LEAVE` (mvm/prelude.lisp) wrap `SYMBOL-VALUE`,
`SET-SYMBOL-VALUE`, `%INTERN-SYMBOL-PKG`, `%INTERN-KEYWORD` and the three
`%MACRO-PKG-*`.  They are a **gate on one BSS word (`0x10000DB8`) plus two no-op
bodies**; nothing writes the word unless a program declares it is running Lisp
on more than one thread, so every other image — bare metal, the four ANSI gate
runners, the aarch64 and i386 CLIs, an ordinary `./modus` — pays one 32-bit load
and a branch.  The hosted x64 image overrides the two bodies
(net/hosted-sync.lisp) with a **recursive** futex mutex (recursive because
interning reaches globals and `SYMBOL-VALUE` takes the same lock; ownership is
the CPU id, not `gettid`, so `%RT-THREADS-ON` refuses unless per-CPU storage is
on).

**A LOCK ALONE IS HALF THE FIX, and the other half is the hazard mvm/gc.lisp
records as out of scope.**  A Cheney collector scans only what it **copies**, so
a table living in region 0 is never walked into by thread B's collector — and a
symbol B allocated in B's own region, whose only reference is that table, is not
forwarded.  It is garbage the instant B collects.  So the locked section makes
**region 0 the active heap** (`%GC-REGION-ENTER` parks the thread's own
allocation pointer/limit and loads region 0's) and puts the thread back on the
way out; under the lock exactly one thread is in region 0, so its single parked
frontier is not a shared pointer two CPUs read.

***PRECONDITION, MEASURED NOT ASSUMED: region 0 must not collect while this is
in use.***  Its root window ends at the **process** stack base, which is not
where thread 2's roots are.  It is ~840 MB after the carve and the interned
universe is small; `test/hosted-thread-lisp.lisp` requires region 0's collection
count **unchanged**, so a workload that outgrew this FAILS rather than corrupts.
Making region 0 collectable under threads needs a stop-the-world handshake with
per-thread root windows — **not done**.

**ORDERING TRAP:** `%RT-LEAVE-LOCKED` must not decrement the depth to zero until
**after** the region is restored.  `%GC-REGION-ENTER` is ordinary compiled Lisp;
anything it touches that the compiler resolved as an implicit global becomes a
`SYMBOL-VALUE` call, which takes this same lock.  At depth 0 that nested acquire
sees itself as the outermost holder, restores the region again and **unlocks** —
and the outer frame then unlocks a mutex the other thread owns.

**2. BLOCKING `RECEIVE`.**  `RECEIVE`'s blocking arm used to release the
scheduler lock and then call the idle loop — but from the instant the lock drops
the actor is claimable by another CPU, and this CPU is still standing on its
stack.  The arm is now `AP-SCHEDULER-BLOCKED`, entered **with the lock held**
(bare-metal behaviour unchanged: it is the two lines `RECEIVE` used to run
inline).  Hosted, it `RESTORE-CONTEXT`s into the thread's own scheduler context,
which moves RSP off the actor's stack and only then releases, because
`+OP-RESTORE-CTX+` zeroes the lock after the stack switch.  `%SCHED-RUN` parks
on a futex; `WAKE-IDLE-AP` — already called by `ACTOR-SPAWN` and
`MAILBOX-ENQUEUE-AND-WAKE`, and previously `0` — is the wake.

**The wake word is a STATE written with XCHG, not a sequence counter.**
Incrementing a sequence is a read-modify-write and this ISA has an unconditional
exchange but no atomic add, so two wakers could write back the same value and a
stale writer could restore exactly the value a not-yet-parked idler is about to
wait on.  Idler `XCHG(wake,0)` **before** looking at the queue; waker
`XCHG(wake,1)` **after** enqueuing.

**3. MUTEX / CONDVAR ON FUTEX(2), WITH NO NEW INSTRUCTION.**  The textbook futex
mutex uses CAS to move 1 → 2 without disturbing 0.  `+OP-ATOMIC-XCHG+` is an
unconditional exchange, so the three-state protocol writes 2 **unconditionally**
and reads what was there; the direction it errs in is a wake nobody needs.  The
condvar avoids an atomic increment by **requiring the associated mutex across
`%COND-SIGNAL`** (as pthreads permits), which makes the sequence counter
ordinary protected code.  **No `LOCK CMPXCHG` was added and none is needed.**
`%MUTEX-LOCK` parks with a 20 ms timeout as a *safety net* — a lost wake would
otherwise be a hang, the one failure mode that tells you nothing —
and `%FUTEX-TIMEOUTS` counts every expiry so "no wake was lost" is asserted.

**4. `SLEEP` WAS `(defun sleep (n) nil)`.**  It is now a restarting
`nanosleep(2)`; restarting is not optional, because this image installs signal
handlers and a non-restarting SLEEP would sometimes do nothing at all.

**MEMORY:** one 8 KB `mmap` (the *thread page*) addressed from **one** BSS word
(`0x10000DA8`, with `0x10000DB0` its init lock) — per-CPU timespecs, per-CPU
scheduler contexts, the scheduler globals, and test scratch.  Not the carved
actor band (SLEEP must work with no actor system) and not a Lisp global (a
global lives in the very table a second thread must not be mutating).

**A REAL BUG FOUND ON THE WAY: `SPIN-LOCK`'s test-and-TEST-and-set inner loop
was a no-op.**  It spun on `(zerop (mem-ref addr :u64))`, and a `:u64` load hands
the machine word back **as a tagged Lisp value** — so a held lock, raw 1, read
that way is Lisp 0 and `zerop` said UNLOCKED every time.  The acquire
degenerated into an unbounded XCHG hammer.  `:u8` fixes it (a `:u8` load is
tagged on the way out, so the value IS the raw byte; big-endian degrades to
today's behaviour).  Measured with a busy-polling actor on one thread and a
sender on the other: ten sends cost **2.0 s** of extra wall time typically and
**260 s** in the worst run; with the read-only wait, **0.1 s**.

**ACCEPTANCE (all 10-of-10 runs):**

| test | checks | headline numbers |
|---|---|---|
| `test/hosted-thread-lisp.lisp` | 29 | 2 threads x 300 iterations of intern / FORMAT / globals / define-and-call / cons; 12 forced collections each; region 0 at **0** collections; 624 lock acquisitions, 24 contended; 0 futex timeouts |
| `test/hosted-thread-lisp-unsync.lisp` | — | **the negative control**: same workload, gate OFF.  10/10 FAIL — 6 SIGSEGV, 4 TYPE-ERROR aborts, one run dumped the symbol-name table as garbage |
| `test/hosted-blocking-receive.lisp` | 29 | idle window: blocking **wall 1000 ms / CPU 0 ms**, polling **wall 1000 ms / CPU 997 ms** |
| `test/hosted-mutex.lisp` | 18 | negative control 338187 of 600000 survived (**261813 lost**); under the mutex exactly 600000; condvar wait wall 300 ms / thread CPU 0 ms |
| `test/hosted-sleep.lisp` | 13 | sleeping 400 ms -> wall 400 / cpu 0; spinning 400 ms -> wall 400 / cpu 398 |

**CORRECTION (2026-08-23, measured): `test/hosted-thread-lisp.lisp` IS NOT
10-OF-10.  IT IS ABOUT 27 OF 30, AND IT WAS ALREADY.**  Run 30 times in a row
rather than 10, it aborts roughly one time in ten with an unhandled
`PROGRAM-ERROR` out of `%TL-SELFTEST` (occasionally a SIGSEGV) — **before it
prints a single section header**, so the failure is inside the two-thread
workload and not in the reporting.  When that happens the process becomes an
**unreapable zombie**: the main thread dies through a path that ends only the
thread, thread 2 is still parked in `futex_wait` and nothing wakes it, so the
leader stays `Z` with one live task and any pipe reading its output never sees
EOF.  A harness that runs this test through a pipe therefore HANGS rather than
reports.

**It is pre-existing, and that is measured, not assumed.**  The same 30 runs
against the binary built from `166fa1b` — the commit before the socket work —
score **27 of 30 with the identical signature**, against **27 of 30** for the
binary after it.  The socket work is not implicated (this test never opens a
socket) and neither is anything else recent; the "all 10-of-10" line above was
written from ten-run samples, which is simply too few to see a 1-in-10 event
reliably.  **Two things to fix, and they are different bugs:** the
`PROGRAM-ERROR` itself, and the teardown that leaves thread 2 parked forever
when the main thread aborts.

**THE TEARDOWN BUG IS FIXED (2026-08-23).  IT WAS `exit` WHERE `exit_group` WAS
MEANT, IN THREE PLACES, AND IT WAS NEVER ABOUT THIS TEST.**  On x86-64 syscall
**60 is `exit`** — it ends only the CALLING THREAD — and **`exit_group` is 231**
(93 vs 94 on the AArch64 generic ABI, 1 vs 252 on i386).  Every comment in the
tree asserted the opposite.  In a single-threaded image the two are
indistinguishable, which is exactly why it stood: `./modus` had no second thread
until this campaign.

The load-bearing one was **not** `sys-exit` but the **SIGSEGV/SIGBUS/SIGFPE/
SIGILL stub** that `translate-x64.lisp` emits for TRAP `#x0520`: its
"no handler-case is active" arm ended with `mov eax,60; syscall`.  So ANY
threaded modus program that faults on ANY thread with nothing armed to catch it
left the group leader an unreapable `Z`, its sibling parked in `futex_wait` with
nobody left to wake it, and every pipe reading its output blocked forever.  The
observable failure was not a crash report but an infinite hang — the one mode
that tells you nothing.  Fixed in the x64 stub (231) and the identical aarch64
stub (94), plus `sys-exit`/`halt` in all three CLI arch slots.

**NOT changed, because it is correct:** the clone child's own epilogue in the
`#x0531` stub (`call rbx; xor edi,edi; mov eax,60; syscall`).  A worker thread
that finishes SHOULD end only itself.

**MEASURED, same binary, same workload.**  Before: failures were `rc=124`
(the harness timeout — a hang).  After: **every** failure is `rc=139`, a clean
immediate process death, and 50 consecutive runs produced **no hang at all**.
`test/run-thread-exit.sh` is the regression, and it asserts only that every run
TERMINATES — deliberately not that every run passes, because the second bug is
still open.

**THE SECOND BUG IS NOT FIXED, AND IT IS THE CONCURRENT FORCED COLLECTION.**
**— FIXED, and it was not the collection: see THE PER-THREAD WINDOW below.  The
standing suspects named at the end of this section were right.  Kept as written
because the bisect that localised it is the reason the fix was findable.**
Localised by varying only `*gcevery*` from the script, which needs no rebuild:

| arm | forced collections | result |
|---|---|---|
| `(%tl-selftest 0 300 100000)` | none — `since` never reaches the threshold | **60 of 60 clean** |
| `(%tl-selftest 0 300 25)` | 12 per thread (the shipping test) | **54 of 60** |
| `(%tl-selftest 0 50 25)` | 2 per thread | 25 of 25 clean |

Remove the collections and the failure disappears entirely.  The fault itself is
a **corrupted control transfer on the MAIN thread**: with a temporary dump wired
into the stub's exit arm (`MODUS_FAULT_DUMP=1`; ptrace is blocked on the dev box
at Yama `ptrace_scope=2`, so there is no gdb and no strace), all four captured
faults show `RSP` on the PROCESS stack and a jump either to address **0** or to
an address **inside that same stack**, with `si_addr = RIP` — a fetch fault on a
return address or handler frame that is no longer valid.  `RAX` reads
`0xffffffffffffff24` in every one, which is tagged `-110` = `-ETIMEDOUT`, i.e.
the thread had just come back from a timed-out futex park.

**Serializing the collections does NOT fix it**, and that is the useful negative:
with `(%ha-set-collect-serialized 1)` — the flag that exists in-tree for exactly
this bisect — the rate is unchanged at **54 of 60**.  So this is not two
collectors overlapping with each other; it is what a collection does with
respect to the OTHER thread's Lisp work.  The standing suspects are the two
pieces of shared BSS named below that are neither tables nor per-thread: the
**multiple-value return buffer at `0x10000090`** and the **handler-frame stack at
`0x10000400`** (one depth counter, one frame array, one armed-frame slot at
`0x10000180`) — a stale or half-written frame restored by a pop is precisely a
jump to a dead stack address.  (The serialized arm additionally reintroduces a
hang of its own, unexplained, on a path that is not the shipping default.)

**WHAT IS STILL SERIALIZED, AND WHY.  — The two pieces of shared BSS named
here are PER-THREAD NOW (see THE PER-THREAD WINDOW below); the whole-iteration
critical section in `test/hosted-thread-lisp.lisp` has NOT been narrowed, so
this paragraph still describes that test.**  `test/hosted-thread-lisp.lisp` holds the
runtime lock across the whole **Lisp-level** work of an iteration, not just the
table calls.  Per-call locking is enough for the **tables**, but FORMAT and the
printer also touch two pieces of shared BSS that are neither tables nor
per-thread: **the multiple-value return buffer at `0x10000090`** (one buffer for
every CPU) and **the handler-frame stack at `0x10000400`** (one depth counter,
one frame array).  Making those per-thread means moving addresses the *compiler*
bakes into every emitted `MULTIPLE-VALUE-BIND` and every function epilogue on
four back-ends — its own campaign, and the next real obstacle.  The consing and
the forced collections **are** genuinely concurrent.

**Runtime EVAL / `compile` on a second thread was NOT attempted.**  "Defining a
function" here means registering a fresh name in the shared symbol-function
table and calling back through it — the table hazard, not the compiler's own
globals.

### THE PER-THREAD WINDOW: the MV buffer and the handler-frame stack stop being one copy per process

**THIS CLOSES BOTH OPEN THREAD DEFECTS ABOVE, AND THEY WERE ONE DEFECT.**  The
"second bug" (a fault during a concurrent forced collection, 54 of 60) and the
residual hang were the same thing: **the handler-frame stack**.  Three pieces of
the low BSS are state of the RUNNING COMPUTATION held one copy per PROCESS:

| slot | what | failure mode under two threads |
|---|---|---|
| `0x10000090` / `0x10000098..0x10000138` | MV count + up to 21 extras | a wrong ANSWER — the producer stores in its epilogue, the consumer reads at the call site |
| `0x10000150` | dynamic nargs | an `&rest` callee reads the other thread's caller's count |
| `0x10000180..0x1019F`, `0x10000400..0xC0F`, `0x10000C10..0xC2F` | armed handler frame, frame stack + depth, longjmp scratch | a wrong JUMP — A arms, B arms over it, A's next unwind restores B's frame and lands **on B's stack** |

That last row is exactly the captured fault signature: `RSP` on the process
stack, a jump either to **0** or to an address **inside that same stack**.

**THE MECHANISM IS A SEGMENT BASE, NOT A NEW ADDRESS.**  The compiler marks a
`LOAD`/`STORE` whose address is provably a window slot — a new **width bit**
(`+WIDTH-TLS-BIT+`; widths 4..7 are widths 0..3 plus "this is a window slot"),
chosen over a new opcode because the address is already in a register by then,
so the whole change is one prefix byte at one instruction rather than a new
opcode number across four translators, an interpreter and the assembler.
`translate-x64` turns the mark into an **FS override**.  The raw-asm sites carry
the same prefix: SETJMP, LONGJMP, `__handler_push`/`__handler_pop`, the
SIGSEGV/SIGBUS/SIGFPE/SIGILL stub, SET/GET-NARGS, and both GC trampolines'
MV-extras root scan.

**WHY FS, AND WHY NOTHING NEEDS INITIALISING.**  GS is already the per-CPU
segment (`PERCPU-REF/-SET`), and the two must not fight.  A fresh Linux process
has **FS base 0**, so `FS:[0x10000180]` *is* `[0x10000180]` — same word, same
address, one byte longer.  A mode word would have had to be written before the
first `MULTIPLE-VALUE-BIND`, and the first `MULTIPLE-VALUE-BIND` happens during
boot.  A worker issues one `arch_prctl(ARCH_SET_FS, block - 0x10000000)` as its
first act.  The self slot at `0x10000C30` holds **the base, not the block**, for
the same reason: the main thread's is BSS zero and correct.

**WHAT IS DELIBERATELY OUT OF THE WINDOW, because a wrong answer is silent:**
`0x10000148` (keyword table), `0x10000158` (intern counter), `0x10000160/68`
(code bounds), `0x10000170` (package table), `0x10000080/88` (globals alist,
symbol table) sit inside the same address range and are process-global — a
per-thread copy of the code bounds reads zero and breaks `FUNCTIONP` on thread
2.  The window is an explicit **SET** of slots, not a range.
`+CLOSURE-ENV-ADDR+` is out too: on x86-64 the closure environment is **R13**.

**EVERY OTHER IMAGE IS UNCHANGED BY CONSTRUCTION.**  Compiler flag off = the bit
is never emitted; back-end flag off = no prefix and widths 4..7 mask to 0..3;
aarch64/i386/interp mask unconditionally.  Only `build-generic-cli` sets both.
The one hand-computed length in the tree — SETJMP's `lea rax,[rip+N]`, which
counts bytes to the end of its own trap block — is **15 not 14** when the window
is on.

**MEASURED, `test/hosted-thread-lisp.lisp`, 100 runs each, same harness
(`test/classify-thread-lisp.sh`):**

| | pass | clean death | ran-but-failed | HANG |
|---|---|---|---|---|
| `c187941` (before) | 91 | 6 | 1 | **2** |
| after | **100** | 0 | 0 | **0** |

**ACCEPTANCE:** `test/hosted-mv-handler.lisp`, 24 of 24 — two threads, 400
iterations each, a forced collection every 25, per iteration a TRUNCATE each
thread reconstructs, a three-value return checked in full, an unwind through
`handler-case`, a NESTED unwind whose inner handler itself unwinds, and a
three-value return ACROSS an armed `handler-case`.  Both barrier timeouts 0.
**And the fix removed must fail:** `test/hosted-mv-handler-unsync.lisp` is the
same binary and workload with MODE 1 (thread 2 skips `%TLS-INSTALL`) — **3 of 3
died rc=139**.

**THAT CONTROL IS PROBABILISTIC, NOT DETERMINISTIC — CORRECTED 2026-08-23.**
The damage is a race between two threads over one window, so a run in which
they never interleave badly comes back CLEAN.  Independently measured over 12
runs: **11 detected the missing fix, 1 survived**.  (A later 12-run batch on
the N-regions binary scored 12 of 12; same coin, different toss.)  A single
clean run of this control is therefore not evidence that the fix is
unnecessary.  Read the RATE over at least a dozen runs, never one result.  The
"3 of 3" above is a sample too small to have shown this and should be read that
way.

**BOUNDARY, stated:** the compiler baked *into* the image keeps `*TLS-WINDOW*`
nil, so a function compiled by the RUNTIME JIT emits absolute window accesses.
Identical on the main thread (base 0), wrong on a worker.  Runtime EVAL on a
second thread was never attempted anyway.

### N threads, and spawn takes a CLOSURE

`%SPAWN-THREAD` took a **raw native entry address**, and
`net/hosted-actors-post.lisp` had one stack, one thread block and one TID word —
**exactly two OS threads**, and a body that had to be a zero-argument top-level
`DEFUN`.  A closure could not be a thread body, so a thread could carry **no
state of its own**.

Now a table of **16** threads, each with its own stack, per-thread window (FS),
per-CPU block (GS), TID word, and a **Lisp closure** as its body.
`%MAKE-NATIVE-THREAD` takes a function and returns a handle;
`%NATIVE-THREAD-ALIVE-P` / `%JOIN-NATIVE-THREAD` read the TID word **the kernel
clears** on exit, not a flag the thread set about itself.

**How a closure reaches a thread that cannot be passed an argument:** the clone
stub enters the child with a bare `call rbx` on a fresh stack.  So the child is
always the same zero-argument **trampoline**, and what varies is a **slot
number** it picks up from the table.  Spawning is serialised by a lock and a
handshake — publish slot, spawn, wait for the child's ack — so "which slot am
I?" has one answer while any child is reading it.  The child acks **before**
running the body: the ack means "I have read my slot".

**Order inside the trampoline is load-bearing:** the per-thread window FIRST
(until it returns, the thread's values and handler frames are still the
spawner's — cloned without `CLONE_SETTLS`), then per-CPU block and CPU id, then
the closure.

The 16 per-CPU blocks and the thread table live in the thread page, grown 8 KB →
336 KB; nothing at a lower offset moved.  The actor band only ever had room for
two per-CPU blocks.

**ACCEPTANCE:** `test/hosted-many-threads.lisp`, 12 of 12 — eight threads, eight
closures over eight different captured values, all eight through a spin-budget
barrier with **0 timeouts**, each summing its OWN K 200000 times and required to
produce exactly `K*200000`, and all eight gettids / CPU ids / window bases
pairwise distinct and none the driver's.

**THE NEXT THING IN THE WAY, and it is not the thread layer:** a thread that
**allocates** needs a **GC region** of its own, and `%HA-CARVE` produces exactly
**two** out of region 0.  So 16 threads get everything except a region, and the
eight-thread body is fixnum arithmetic, raw memory and syscalls.  Carving N
regions is a change to the carve.  Until then a thread-per-client server can
have its threads but not its conses.

### Sixteen regions, one per thread — a thread that can cons

`%HA-CARVE` now produces **one region per thread slot** (`%HA-MAX-REGIONS` = 16),
each with its own semispace pair, its own 64-byte control block at band
`+0xA000 + 64i`, its own root window bounded by **that thread's** stack, and its
own collection count.  `%HA-FIT-REGIONS` decides how many the heap affords —
sixteen at 896 MB semispaces, **never fewer than two**, so an image that only
ever afforded the historic pair carves the historic pair at the historic
addresses and `*HA-R1-FROM*` / `*HA-R2-FROM*` still name them.  `%THR-MAX-THREADS`
is now *defined as* `%HA-MAX-REGIONS`, so a slot without a region cannot exist.

**A thread gets its heap exactly when it can have one.**  The spawner
initialises the slot's region before the clone (it is the side that knows the
stack it just mapped, and the only side that may compute the metadata scale);
the trampoline adopts it *after* stamping its CPU id, because which
active-region cell it writes comes from that stamp.  All of it is gated on
per-CPU active-region storage being ON — with the mode word off, every
`%GC-SET-REGION` writes one shared word.  Mode off ⇒ no region ⇒ this layer
behaves exactly as it did.

**ACCEPTANCE:** `test/hosted-many-regions.lisp`, **106 checks**.  Eight threads,
eight heaps, twelve forced collections each, all eight inside one barrier at
once; each block's own count 0 → 12 (eight counters, not one); worker *i* holds
400+7*i* links and must walk back **its own** answer, so eight different
numbers; with the workers gone the driver collects its own region twice and
every worker's heap **and** control block is bit-for-bit identical across it,
with the checksum asserted **non-zero** first and all eight asserted
**distinct**; `%GC-COUNT-FOREIGN-REFS` over all **56 ordered pairs** = 0 with a
positive control of 1; region 0 collected **0** times; 85 collector entries
while another collector was already inside.

**Two defects the test found, both real.**  (1) The per-thread record was 64
bytes and the heap fields did not fit — a region control block written at
`rec+0x40` landed on the *next* record's STATE word, so eight threads came back
on slots 1,3,5,…,15 and four of the audited regions were never initialised.
Records are 128 bytes now.  (2) Eight identical chains give eight **identical**
checksums: `%GC-SUM-RANGE` folds to 24 bits and the regions are 16 MB = 2^24
apart, so the only thing that differs between two live heaps is exactly what it
masks away.  "Region B is unchanged" would have held if B held C's data.

### The sb-thread / sb-bsd-sockets surface, and two compiler bugs it exposed

`net/sb-thread-shim.lisp` and `net/sb-sys-shim.lisp` are the **SBCL
compatibility surface** glass asks modus for — `SB-THREAD`, `SB-BSD-SOCKETS`,
`SB-POSIX`, `SB-SYS`, `SB-ALIEN`, and `SB-EXT` (which **did not exist** in this
image: measured, `(find-package "SB-EXT")` ⇒ NIL, while glass says
`sb-ext:posix-getenv` 34 times).  Both are **evaluated at boot from a baked
source string**, like `net/genera-compat.lisp` and for one more reason: these
packages exist ON THE HOST AND ARE LOCKED, so merely *reading*
`sb-thread::threadp` host-side is a package-lock violation.  `MODUS_NO_SB=1`
skips both.  The features pushed are `:SB-THREAD` and `:SB-BSD-SOCKETS` and
**not** `:SBCL`.

**ACCEPTANCE:** `test/hosted-sb-thread.lisp` (44 checks) and
`test/hosted-sb-sockets.lisp` (52 checks).

**THE JIT DID NOT KNOW ABOUT THE PER-THREAD WINDOW.**  The runtime co-init set
`*X64-TLS-WINDOW*` (the translator half) and **not** `*TLS-WINDOW*` (the
compiler half, which is what *sets* the width bit).  So every JIT-compiled
`MULTIPLE-VALUE-BIND`, `HANDLER-CASE` and `UNWIND-PROTECT` reached the MAIN
thread's window from whatever thread it ran on.  Measured before the fix: a
JIT-compiled thread body whose whole content was `(handler-case 222 (error (c)
-1))` died with an MVM CALL-IND through a non-callable target, while
MULTIPLE-VALUE-BIND and LOOP in the same position were fine — the shape of a
shared **handler-frame stack**.  One `setq` in `mvm/build-cli-common.lisp`.

**`SYSCALL3` FROM JIT-COMPILED CODE RETURNS THE SYSCALL NUMBER** when a global
read is in the same function.  Measured, no shim involved:

```
(defvar *a* <an address>)
(defun f3 () (syscall3 39 *a* 0 0))            => 39      ; NOT the pid
(defun f4 (x) (syscall3 39 x 0 0))             => 804095  ; correct
(defun f2 () (let ((a *a*)) (syscall3 49 41 a 16))) => 49
(defun f1 (fd a len) (syscall3 49 fd a len))   => -9 (EBADF), correct
```

The syscall IS issued; the RESULT is wrong, and a non-negative wrong result is
indistinguishable from success to a caller checking for `-errno`.  That is how a
`socket-bind` that appeared to succeed left a socket bound to nothing.
**NOT FIXED.**  Routed around: the shim issues no syscalls at all, and
`net/hosted-sockets-post.lisp` gained an AOT `%SBS-*` floor it calls instead.

**`(LISTEN stream)` ON A DRAINED SOCKET ANSWERED T** — fixed, via a
`%FD-INPUT-READY-P` seam whose conservative default lives where `LISTEN` can see
it (`mvm/cl-fileio.lisp`) and whose real zero-millisecond `poll(2)` lives where
`poll` can be called (`net/hosted-sockets-post.lisp`).

**THE CEILING, MEASURED.**  8 threads x 20 000 locked increments through the AOT
primitives is EXACT, five batches running, slots and regions reused correctly.
The same 8 threads at 2000 iterations **through runtime-JIT-compiled code**
fault NON-DETERMINISTICALLY inside the MVM (`LONGJMP with no active
handler-case`, `unknown opcode #x4 at PC N`).  Region 0 collected **zero** times
in every such run, so it is not the documented region-0-from-a-worker-stack
hazard; not thread count (4 threads faults too), not slot reuse, not the futex
primitives.  It is bounded to **calling a runtime-JIT-compiled function from a
worker thread, at a rate**, and it is the ceiling that stands between the shim
and running glass.

### GLASS'S OWN RFB SERVER RUNS ON MODUS AND SHAKES HANDS WITH A REAL VNC CLIENT

Four things, in the order they were found, and one wall.

**THE SHIM WAS PORTABLE-SHAPED IN ITS ARGUMENTS AND SHIM-SHAPED IN ITS
INITARGS.**  `net/sb-sys-shim.lisp`'s socket class named its slots `sock-type` /
`sock-protocol` — correctly, because `:type` is also a DEFCLASS slot option —
and then named its INITARGS after the slots.  An initarg is not the slot's name
and is not ours to choose: `sb-bsd-sockets` spells it `:TYPE`, and glass writes
`(make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)` five
times.  Every one died at the first socket a real program opened.  The slots keep
their names; the initargs are now `:TYPE` and `:PROTOCOL`.

**AND THE DESCRIPTOR IS OPENED AT MAKE-INSTANCE, WHERE SBCL OPENS IT.**  The
first draft opened it lazily and argued nothing in glass depended on the earlier
point.  True of glass, false of the surface: `socket-file-descriptor` on a fresh
socket answered NIL here and a descriptor under SBCL, one accessor call away.
A shim has to be portable-shaped in WHEN, too, wherever that is observable.

**FIND THE GAPS ALL AT ONCE OR PAY A BUILD EACH.**  The shim is baked into the
image, so discovering gaps by running glass costs one rebuild per gap and only
ever finds the gaps on the path the first client takes.
`test/run-glass-shim-audit.sh` asks the WHOLE surface in one run, in the shape
glass writes it (every probe is a transcription of a named call site), with
**SBCL run first and required to score 100%** so a wrong probe is reported as a
harness bug rather than a modus gap.  It went **22 ok / 20 GAPS -> 42 ok / 0
GAPS** across two builds instead of twenty.

**THE FILE-STREAM STAGING PAGE WAS ONE PAGE FOR THE WHOLE PROCESS, AND THAT IS
SILENT DATA CORRUPTION.**  `mvm/cl-fileio.lisp` stages every raw read and write
through `*IO-BUF-ADDR*`: `%FS-WRITE-BYTE` stores the byte there and then issues
`write(2)` from it, so two threads writing to two DIFFERENT descriptors send
each other's bytes.  **Measured: 327 680 bytes** — one RFB raw rectangle at 1280
wide with glass's default banding — **written by one thread and read by another
came back with 73 933 bytes wrong**, while the identical transfer on one thread
was byte-perfect, and nothing signalled.  Fixed by making the page a SEAM
(`%FS-IO-PAGE` in cl-fileio, historic behaviour) overridden per-CPU in
`net/hosted-sync.lisp` by last-defun-wins — the same seam `%SOCK-IO-BUF` already
uses, at +0x2000 in the per-CPU block.  It defers to the old page unless per-CPU
mode is on, so a single-threaded `./modus` maps nothing new.  After it: 48 KB
across two threads in **1 ms, zero bytes wrong**.

**WHAT NOW RUNS.**  `test/run-glass-serve.sh` stands up GLASS:SERVE — glass's
own `src/rfb.lisp`, loaded from the glass tree, on glass's own TCP-LISTEN, with
glass's per-client reader AND sender threads — and points `test/glass-rfb-client.py`
(Python, not modus, generating its own expected image) at it.  **The handshake
completes end to end**: ProtocolVersion, security type None, SecurityResult,
ClientInit/ServerInit with all ten pixel-format fields checked against glass's
values (note `big-endian-flag` **0**, where modus's own minimal server in
`test/rfb-static.lisp` says 1 — two different legal wires, two clients) and the
desktop name.  17 client-side checks pass.

**THE WALL: THE FRAME NEVER ARRIVES.**  With `:WAKE NIL` the sender polls
forever and delivers nothing; with a real WAKE the server process **dies
outright, with no condition and no output**, immediately after the client is
counted in.  Every ingredient passes in isolation, measured this round:

* `RFB-SENDER-LOOP` itself, on a worker, producing a correct **4112-byte** Raw
  update that a peer reads back;
* a worker writing 4112 bytes to the SAME stream the main thread is parked
  reading — 0 wrong;
* timed `CONDITION-WAIT` on a worker, **200/200**;
* `WITH-MUTEX` / `WITH-RECURSIVE-LOCK` acquired on a worker, including one the
  main thread used first, and contended both ways;
* a struct slot written by main AFTER a worker is already polling it — seen;
* `FORMAT` on a worker, to `*error-output*` and to `t`;
* a blocking `READ-BYTE` on main not starving a worker.

What is left is the runtime-JIT concurrency ceiling named in the section above:
glass is LOADED AT RUNTIME, so `rfb-sender-loop` is JIT-compiled, and it is
called from a worker thread at 60 Hz.  `test/run-glass-serve.sh` is the thing
that will notice when that lifts.

**— WRONG ON BOTH COUNTS, AND MEASURED WRONG.  IT WAS TWO DEFECTS AND NEITHER
WAS THE JIT.  See THE FRAME IS DESCRIBED below.**

**TWO INSTRUMENTS THAT LIE, AND COST HOURS BEFORE THEY WERE CAUGHT.**

* **`GET-INTERNAL-REAL-TIME` IS A CALL COUNTER, NOT A CLOCK.**  It returns 1, 2,
  3 on successive calls and is unmoved by a real `(sleep 3)`.  So every "busy
  wait until N ms have passed" written against it returns after N CALLS — which
  made a worker thread look BLOCKED when it had simply not been given a
  microsecond.  `GET-UNIVERSAL-TIME` is correct; `SLEEP` is correct (verified
  against wall time: `(sleep 5)` costs 5 s of real time and no user time).
  glass uses `get-internal-real-time` as a real unit throughout `rfb.lisp`.
  **FIXED (243b265) — it is clock_gettime(CLOCK_MONOTONIC) now; see THE CLOCKS
  ARE CLOCKS below.**
* **`#'NAME` RESOLVES LATE.**  Saving `#'f` and then redefining `f` gives you
  the NEW `f`, so the classic diagnostic wrapper calls itself until the stack
  dies.  Replace outright when instrumenting; do not wrap.

### THE FRAME IS DESCRIBED: the empty update was a LOOP bug, not a thread bug

**THE SERVER WAS SENDING FOUR BYTES THAT MEANT "ZERO RECTANGLES FOLLOW".**  Read
off the wire with a permissive Python client that dumps whatever arrives instead
of asserting: `00 00 00 00` — a FramebufferUpdate header with `n-rects = 0`.  The
client then waited forever for pixels the server had already decided not to
describe, which is what "the frame never arrives" was.

The cause is in **this repository's LOOP**, and it is CLHS 6.1.3: COLLECT,
APPEND and NCONC are ONE accumulation category, and this LOOP gave them one
accumulator EACH, returning whichever was declared first.

    (loop for k in '(1 2 3) if (evenp k) collect k else nconc (list k k))
      => (2)          here (before)        => (1 1 2 3 3)   SBCL

That is glass's `BAND-RECTS` verbatim: rectangles short enough to need no
banding took the COLLECT arm and were described; rectangles TALL enough to need
banding took the NCONC arm and were **dropped**.  A 96-row framebuffer banded at
`*MAX-BAND-ROWS*` = 64 came out as NIL.  Fixed in 6a9a72a by making the three
kinds share one variable AND one representation (all three accumulate reversed —
`REVAPPEND` for APPEND, which copies; `NRECONC` for NCONC, which may destroy —
and ride COLLECT's existing closing NREVERSE).

**AFTER IT**, on the wire, from `test/run-glass-serve.sh`'s own server: two
rectangles, `(0 0 128 64)` and `(0 64 128 32)`, 49180 bytes, pixels matching the
image the Python client generates for itself.  The 96-row screen in that test
was chosen on purpose — a 64-row one would have needed no banding and passed
this whole time.

**AND THE SERVER STOPPED DYING.**  Before: `SIMPLE-ERROR | MVM LONGJMP (TRAP
#x0511) with no active handler-case`, process gone.  After: server side PASS, 8
checks, 0 failed, listener closed, fd probe unchanged.

### AND THE SECOND DEFECT, BISECTED: IT IS NOT THE JIT

The client still does not get the whole frame: the transfer stops at **32785
bytes**, three bytes into the SECOND rectangle's header.
`test/run-glass-send-worker.sh` is that stop with the RFB session removed — one
socket, one worker, one `GLASS:SEND-RECTS`, and a Python peer that computes
49180 itself and checks every pixel.  Four arms differing ONLY by which wrappers
`RFB-SENDER-LOOP` puts around the call:

| arm | wrappers | result |
|---|---|---|
| plain | none | 49180 bytes delivered |
| tx | `(let ((glass::*tx* (list 0))) …)` | 49180 bytes delivered |
| lock | `(glass::with-fb-locked (fb) …)` | 49180 bytes delivered |
| **both** | the two, **nested**, as the sender loop nests them | **32785, then the process dies** |

So the STOP is not the socket, not the encoder (`WRITE-RECT-RAW`'s output for
both bands is byte-perfect written to a FILE from the same worker), not the
thread, not the concurrent reader (measured with the main thread parked in
`READ-BYTE` on that stream and without — no difference), and not either wrapper
alone.

**THE THREE COMPLETING ARMS ARE NOT CLEAN, AND THIS TABLE SAID THEY WERE.**
They deliver every byte, but on SOME MACHINES exactly one pixel arrives as
`0x000000`.  This was first written up as "every pixel correct" on the strength
of a handful of runs on one box — a sample reported as a property, and it sent
two people at the wrong target.  What is actually established:

* index **2591** = (31,20) on one machine, in all three completing arms;
  **2528** = (96,19) on another, in an earlier run.  **The index MOVES between
  environments**, so it is not a fixed structural offset — not an encoder or a
  buffer boundary.
* the value is **ZERO both times**, and zero is what `MAKE-FRAMEBUFFER` fills
  with, so it reads as **one lost STORE**, not one wrong value.
* it does **not** reproduce on the second machine at all: 14 consecutive clean
  runs of the shipping binary, plus 3 more built with `MODUS_GC_R14=33554432`
  so collections fire constantly — and that binary was verified **byte-identical**
  to a fresh build of the same commit, so the tree is reproducible and the
  variation is environmental, not a build difference.
* **`MODUS_GC_R14` IS A BUILD-TIME KNOB, NOT A RUNTIME ONE** (`build-generic-cli.lisp`
  reads it with `posix-getenv` while building).  Setting it on the command line
  of `./modus` does nothing and silently looks like "no collections happened".

`test/glass-send-worker.lisp` now grades the FRAMEBUFFER through glass's own
`FB-GET`, before the port is announced and again after serving, because a
client reporting a bad pixel cannot tell a store lost while PAINTING from a
byte lost on the WIRE.  **`FB-SELFCHECK` is the line to read**: non-zero before
serving = the paint lost it; `bad=0` before with the client still seeing a zero
= the encode or the transport lost it.  The client also counts every bad pixel
now, groups them into runs and prints stream byte offsets, so "one lost store"
and "a corrupted region" stop being indistinguishable.

**AND IT IS NOT THE JIT.**  The whole family reproduces IDENTICALLY under
`MODUS_NO_JIT=1` — three runs each, same failure, same place.  So
`%JIT-ENTRY-FOR`'s per-region `(%gc-count)` re-bake stamp and the unlocked
`*jit-page-cache*` are **not what is in the way**, whatever else may be wrong
with them.  Both were standing hypotheses; both are dead.

**WHAT THE EVIDENCE POINTS AT INSTEAD — the RUNTIME-LOCK REGION HOP.**  Not
proven, and stated as a lead rather than a finding:

* A hot loop reading its source array from a GLOBAL (so every iteration is a
  `SYMBOL-VALUE`, which takes the runtime lock and hops the active region to
  region 0) hands a worker back a `(unsigned-byte 8)` array whose element 0 is
  the CHARACTER `#\V`.  The **identical** loop with the array hoisted into a
  local — no hop — is correct on two successive workers.  Same binary, same run
  length, one difference.
* A run of that shape left a file in the working tree literally named
  `#<?STALE-FORWARDED-191>`: a global whose value was a pathname STRING came
  back, on a worker, as a stale forwarding pointer, and `WITH-OPEN-FILE` created
  a file named after the printed representation of the wreck.
* The mechanism that would explain it is already named in this file: `%RT-ENTER`
  parks the caller's frontier and loads REGION 0's from its control block, but
  the MAIN thread allocates in region 0 **from registers, without the lock**, so
  the block's frontier is stale by however much main has consed since its last
  hop.  "Two threads in region 0 with one parked allocation frontier between
  them" is exactly what `%RT-LEAVE-LOCKED`'s own docstring says the lock exists
  to prevent — and the lock does not cover main's ordinary allocation, because
  region 0 IS main's heap.
* Region 0's collection count is **0** throughout, so this is not the documented
  "region 0 must not collect from a worker stack" hazard.

The obvious shape of a fix is to stop region 0 being two things at once: either
give the main thread a region of its own so region 0 is only ever the
lock-protected shared heap, or give each CPU its own non-collecting slice of
region 0 for locked-section allocation so no two threads ever share a frontier.
Both are carve changes and neither was attempted.

### — AND THAT LEAD IS REFUTED.  DO NOT CHANGE THE CARVE.

It was the most substantial thing anyone had, and it is wrong.  Three
measurements, on a reproducer that fails **8 of 8**:

* **THE HOP ALONE IS HARMLESS.**  A worker taking the runtime lock 200 000
  times — hopping into region 0 and back — while the main thread conses
  continuously **outside** the lock: chain intact, 0 corrupted walks, rc=0.
  If loading a stale parked frontier were the defect, this is the shape that
  would show it.
* **THE POSITIVE CONTROL DOES NOT FIX IT.**  Wrapping the main thread's
  allocation in `%RT-ENTER`/`%RT-LEAVE` by hand — so no thread can be in
  region 0 while main advances it, which is exactly what the proposed carve
  would buy — leaves the failure **6 of 6, unchanged**.  The control was
  verified to be real: `%RT-ENTER` is fbound and moves the lock's acquisition
  counter at `0x10000DE0` by one.
* **THE MAIN THREAD IS NOT INVOLVED AT ALL.**  With `MAIN_ROUNDS=0` — main
  allocates nothing whatever after spawning — the failure reproduces
  identically.  A two-thread heap race that survives one thread doing nothing
  is not a two-thread heap race.

The staleness *precondition* is real but narrow, and was measured rather than
assumed: over a loop with no explicit global read, region 0's parked frontier
moved **19 201 920 bytes** while the loop's own conses account for 3 200 000 —
the evaluator itself hops constantly, so the block is republished far more
often than the mechanism needs.  (`GET-ALLOC-PTR` reads **0** from evaluated
code and cannot be used as the live frontier in a script; the control block's
`+0x30` field is what to read.)

**WHAT IT ACTUALLY IS, narrowed to one operation.**
`test/run-worker-intern.sh` is self-bisecting — four arms, one loop, one
worker thread, differing only in the loop body:

| arm | what it does | result |
|---|---|---|
| cons | conses | clean 3/3 |
| format | `FORMAT NIL` — allocates strings | clean 3/3 |
| intern-same | INTERNs ONE name repeatedly — takes the lock, searches the shared table | clean 3/3 |
| **intern-fresh** | INTERNs a NEW name each time — takes the lock AND **adds** to the table | **clean 0/3**, `MVM LONGJMP (TRAP #x0511) with no active handler-case` |

So it is not consing on a worker, not allocating on a worker, not taking the
runtime lock on a worker, and not searching the shared tables from one.  What
is left is **adding** to them.  The signature is the glass sender's, exactly.

**AND IT IS LAYOUT-SENSITIVE, WHICH IS NOW A PROPERTY OF THIS WHOLE CLASS
RATHER THAN A SURPRISE.**  Whether a given arm dies moves with the LENGTH of
the interned names and with whether the loop body reads a global:
`(format nil "XR-~D" i)` with a literal prefix passes 5/5 where
`(format nil "~a~D" *pfx* i)` with the same prefix in a global fails 4/4.  A
single clean run of any of these means "not reproduced in this shape", never
"correct" — the same lesson the pixel-2591 story taught, and the reason the
runner defaults to three runs per arm and reports a rate.

### THE ROOT CAUSE, MEASURED: a worker's INTERN puts the symbol in the WRONG REGION

`%RT-ENTER` exists so that the shared runtime tables and everything reachable
from them live in **region 0** — that is the whole argument of "A LOCK ALONE IS
HALF THE FIX" above, and it is why the locked section hops the heap at all.

**IT DOES NOT.**  `test/run-worker-xregion.sh` asks where the objects actually
are, with `%GC-WORD-OF` for the addresses and `%GC-COUNT-FOREIGN-REFS` for the
audit, at collection count 0 so nothing has had a chance to dangle yet:

| arm | last object | its name | foreign refs, region 0's live span -> the worker's region |
|---|---|---|---|
| strings (control) | the worker's region | — | **0** |
| **intern** | **the worker's region** | **the worker's region** | **508** (516 on another run) |

So it is not merely the NAME STRING computed before the call, which was the
obvious suspect: **the symbol object itself** is allocated in the worker's
region, and region 0's tables then point at it.  That is the forbidden
direction of the one rule per-region collection rests on, created once per
fresh intern, and nothing enforces it.

**AND IT IS FATAL RATHER THAN UNTIDY, WITH A CONTROL.**  Those pointers are not
on the worker's stack, so the worker's own collector never updates them.  Force
ONE collection of the worker's region after interning and the process takes
**SIGSEGV, 3 of 3**.  The identical shape — same loop, same length, same strings,
same forced collection, differing only in that the results are KEPT rather than
INTERNED — survives **3 of 3**, with every object correctly moving
(`721EE898A769 -> 721F205FF9E9`) and its re-lookup still `EQ`.

That is why `intern-fresh` in `test/run-worker-intern.sh` dies and the other
three arms do not, why it is layout-sensitive (it bites when the worker's region
happens to collect), and why the glass sender dies with the same
`MVM LONGJMP (TRAP #x0511)` signature: glass interns while it serves.

**THE FIX IS A DESIGN QUESTION, NOT A PATCH**, which is why none was attempted:
intern in the owning region and accept that the tables are then per-region; or
COPY the name into region 0 and allocate the symbol there, which means the
locked section must own the allocation rather than merely hop the frontier; or
make the tables' region explicit and give every shared-table write a barrier.
The audit is the acceptance test for whichever is chosen —
`test/run-worker-xregion.sh` goes green when a worker's fresh intern stops
leaving a pointer behind.

**A TRAP THIS COST A CYCLE ON: `SYS-EXIT` FROM INSIDE A NESTED `LET*`/`IF` IN A
`--script` DOES NOT TAKE EFFECT.**  The process ran to the end of the file and
exited **0** while the verdict line above it said FAIL — and every runner reads
the code, not the prose.  Put the exit at TOPLEVEL; the other tests in this tree
already do.

### THE CLOCKS ARE CLOCKS (243b265)

`GET-INTERNAL-REAL-TIME` bound `T` — `(let ((t (handler-case (syscall3 201 …))))`
— so `(integerp t)` asked whether TRUE is an integer, said no, and took the
call-counter branch on every call regardless of the syscall.  And syscall 201 is
`time(2)`, whole SECONDS, reported as if it were the thousandths
`INTERNAL-TIME-UNITS-PER-SECOND` promised; on the AArch64 generic ABI 201 is
`listen(2)`.  `GET-INTERNAL-RUN-TIME` was `(defun get-internal-run-time () 0)`,
which made every `(- after before)` zero.

Both now go through a seam — `%IRT-NS` / `%IRUN-NS`, defined at 0 in
`mvm/ansi-bridge.lisp` ("this target has no clock") and overridden by
last-defun-wins in `net/hosted-sync.lisp`, which is where `clock_gettime(2)` can
be called at all because it owns the **per-CPU** timespec scratch (a shared
timespec is two threads writing one buffer).  Real time is CLOCK_MONOTONIC, run
time is CLOCK_PROCESS_CPUTIME_ID.  The unit is converted in ONE place
(`%NS-TO-ITU`) so the clock and `INTERNAL-TIME-UNITS-PER-SECOND` cannot drift
apart again, and the default is now 1000000 — which is what
`build-x64-linux`, `build-x64`, `build-aarch64`, `build-aarch64-linux`,
`build-x64-cl-repl` and `build-rpi-cl-repl` all already SETQ'd it to.  The CLI
was the odd one out at 1000.

Measured on the built binary: `(sleep 2)` moves it 2000122 units = 2.000122 s;
two back-to-back calls differ by 11 units; run time over a busy loop 10786
units, over `(sleep 1)` 30 units.

**A TRAP FOR THE NEXT INSTRUMENTER: `(%GC-COUNT)` IS NOT A COUNT ON x64.**  The
field is stored raw there and `mem-ref :u64` halves it, so a count of 1 reads
back as a value whose bit pattern is a CONS tag and prints as garbage.  That
looked exactly like heap corruption after a forced collection and cost a
detour.  Read it with `%GC-META-READ`, or `(%gc-read64 (+ (%gc-region) #x20))`.
This is already written down under "Two traps this cost time on"; it is written
down again here because it was still walked into.

**THE HARNESSES, ALL RE-RUNNABLE, ALL SBCL-REFERENCED WHERE THAT MEANS
ANYTHING.**

    test/run-glass-load.sh        :glass loads — 13 of 13 files (its 8 + cram's 5),
                                  file list read out of glass.asd/cram.asd, each
                                  file's witness the LAST thing it defines
                                  (because modus's LOAD swallows a form that dies)
    test/run-glass-shim-audit.sh  the whole SB-* surface — 42 ok / 0 gaps
    test/run-glass-serve.sh       glass's RFB server vs a real Python VNC client
                                  (handshake passes; the frame does not arrive)

### THE CARVE TOOK MEMORY THAT WAS STILL IN USE — and all eight glass files load

`%HA-CARVE` takes the top ~272 MB of region 0's 896 MB semispaces for the actor
band and the sixteen per-thread regions, then **zeroes four ranges inside what
it took**.  It never asked whether anything was living there.

At boot nothing is: every selftest in `net/hosted-actors.lisp` calls
`%HA-PERCPU-INIT` on its first line, so every measurement this layer has ever
taken was taken from a nearly fresh heap.  **A REAL PROGRAM DOES NOT CARVE AT
BOOT.**  The carve is reached from `%HA-BASE` ← `%SYNC-CELL-CTL` ← the FIRST
`sb-thread:make-mutex` a program executes, and a program asks for its first lock
whenever it happens to want one.  glass wants one at
`(defvar *session-clipboard-lock* (%clip-make-lock))` — `src/clipboard.lisp`,
the fourth file — by which point loading cram's five files and glass's first
three has pushed the allocation frontier **past the carve point**.  Measured,
not inferred: with the fix in place that load reports
`*ha-carve-collections*` = **1**, which is the count of times the frontier was
found above `from0 + new0`.

**THE ZEROING WAS NEVER THE BUG, AND AN EARLIER ROUND FIXED THE WRONG HALF.**
The comment at `+0xC000` in that file records the first face of this: a non-zero
word at `%SYNC-CELL-CTL`'s spin lock, left over from region 0's from-space, hung
the first `MAKE-MUTEX` at a full core — and the answer was to zero that range
too.  That cures the hang by *widening the damage*.  All four `%HA-ZERO` ranges
were writing over live objects.

**WHAT IT LOOKED LIKE, so the shape is recognisable next time.**  The keyword
`:NAME` came back with a wrecked header, so `SYMBOLP` said NIL, so the compiler's
terminal arm reported `WARN: cannot compile #<?>, using nil` **on the next form
read** — a literal silently replaced by NIL — and the `TYPE-ERROR` that followed
carried a condition object whose own type-name slot was garbage, so
`%CONDITION-P` rejected it, so `handler-case` matched no clause (**including a
`T` clause**) and it escaped to LOAD's swallow as
`!! UNHANDLED-ESCAPE load-toplevel-form-swallowed: #(#<?111> NIL)`.

**THAT ESCAPE IS NOT A CONDITION-SYSTEM BUG.**  Two independent findings, both
measured.  (1) The signal is raised while the **toplevel form is being
COMPILED**, before the `handler-case` in it has been armed — so there is no
frame to catch it and `(handler-case (eval '…) (t (c) …))` fails identically.
(2) Where a runtime signal *is* involved, the condition's type-name is a wrecked
pointer, so every `(typep *current-condition* 'X)` is NIL by construction.
`handler-case` was doing exactly what it was told.  In the same image
`(handler-case (error "boom") (t (c) :CAUGHT))` and `(handler-case (car 5) …)`
both catch, and `*CATCH-ACTIVE*` is NIL.

**IT IS NOT A MISSED GC ROOT, AND THE MEASUREMENT THAT SETTLES IT IS `%GC-COUNT`
= 0.**  `(%gc-count)` reads **0** at boot, after each of cram's five files, after
each of glass's first three, and immediately before the failing call.  A
forwarding pointer can only be written by `copy_object`, which only runs during
a collection, and no collection had run.  The low-nibble-#xF headers are real
forwarding words — they are the tracks of the **first** collection, which the
shrink itself provokes by moving the limit below the frontier — but they are
downstream of the carve, not the cause of it.  The missed reference is not one
`scan_word` forgot: it is every object above the new boundary, which the shrink
put **outside the region** while the band overwrote it.

**THE FIX: `%HA-CARVE-ROOM`.**  Ask, before committing, whether the live
frontier (`GET-ALLOC-PTR`) is at or below the carve point.  If it is not,
**collect region 0 once** — a Cheney collection compacts the live set to the
bottom of the new from-space, which is exactly the shape that makes the top
spare again, and it is the region's ordinary collector rather than a side door —
then re-read and re-ask.  Still over (a program with >620 MB genuinely live) is
an honest **0**, the same answer a heap too small to carve from already gets: a
refusal is a mutex the caller cannot have, a silent carve is a heap the caller
cannot trust.  `*HA-CARVE-COLLECTIONS*` and `*HA-CARVE-REFUSALS*` are counts, so
"it happened and nobody noticed" is distinguishable from "it never happened".
`%HA-CARVE-NEW0` is split out and used by **both** sides, so the address the
check guards cannot drift from the address the carve uses.  The collection FLIPS
the semispaces, so `%HA-CARVE` re-reads `from0`/`to0`/`size0` after it.

Degrades to the historic behaviour by construction: a `VA` of 0, or one outside
`[from0, from0+size0)`, means this is not a live Cheney region (bare metal, an
uninitialised image) and the check abstains.

**RESULT: ALL EIGHT FILES OF THE `:glass` SYSTEM LOAD ON MODUS** — packages,
record, framebuffer, clipboard, perf, socket, rfb, zrle, over cram's five — from
a plain `./modus --script`, with no early-carve workaround and
`refusals=0`.  It was four before.

**AND THE NEXT WALL, NAMED:** `GLASS:SERVE-ONE` gets as far as opening its
socket and dies in the shim, not in glass —
`SIMPLE-ERROR | make-instance: invalid initarg ~S ARGS=(TYPE)`.  `sb-bsd-sockets`
makes an `INET-SOCKET` with `:TYPE` and `:PROTOCOL`; `net/sb-sys-shim.lisp`'s
class does not take them.  That is a shim gap with a name, which is a different
kind of problem from the one this section is about.

**RESIDUAL, STATED RATHER THAN HIDDEN.**  The forced collection happens while an
interpreted/JIT'd **toplevel form is executing**, and that form then re-runs part
of itself: the eight-file load prints `>> packages … >> clipboard` and then
`>> packages` again, because the `DOLIST` restarted.  Harmless here (the forms
are idempotent `DEFUN`s and the load completes correctly) and pre-existing — it
is the same shape as the documented "`%GC-COLLECT-HERE` at an interpreted
`--script` toplevel breaks the next `(format t …)`" — but the carve is now a
place that *reaches* it, so it is on the critical path and it was not before.
`(%gc-count)` read from an interpreted toplevel after such a collection also
misbehaves.  Both are the same unfixed defect and both are one level below this
one.

### The socket layer becomes usable by a SERVER: unbounded transfers, per-CPU buffers, poll(2)

The campaign is threads -> sockets -> glass.  `net/hosted-sockets.lisp` already
had connect/listen/accept/send/recv, connected UDP and a DNS resolver on
`syscall3`; what it did not have was anything a server could stand on.

**THREE DEFECTS, NONE OF WHICH NEEDED A THREAD TO BE WRONG.**

1. **A transfer was capped at one page, silently.** `SOCKET-SEND` copied LEN
   bytes into `*IO-BUF-ADDR*` — a 4096-byte page — and issued one `write(2)`.
   A LEN above 4096 wrote past the buffer.  Nothing named the limit, so nothing
   could respect it.  `%SOCK-IO-CAP` names it and the transfer loop CHUNKS
   against it, so a transfer is bounded by nothing whatever the buffer's size.
   **Glass's `write-rect-raw` issues ONE `write-sequence` of 327 680 bytes at
   1280 wide with default banding, and 4 096 000 with `*max-band-rows*` NIL.**
2. **A short write lost the tail of the message.** `write(2)` on a socket takes
   fewer bytes than offered whenever the send queue fills — exactly what a large
   framebuffer update does.  The old code returned that short count and every
   caller in the tree ignored it.  The loop now advances by what `write(2)`
   actually took.
3. **`SOCKET-LISTEN` bound `0.0.0.0` by default.**  Now `SOCKET-LISTEN` is
   127.0.0.1 and `SOCKET-LISTEN-ON` takes an address — **a different function
   name, so serving the network appears in a diff and cannot be arrived at by
   leaving an argument off.**  Nothing in the image calls either
   (`grep -rn "socket-listen" net/ mvm/ lib/ boot/` outside the socket files
   returns nothing): loading opens no socket, the CLI opens no socket, there is
   no autostart and no environment variable.

**PER-*CPU* BUFFERS, NOT PER-*CONNECTION* ONES, and the reason is what makes the
fix small.**  The bounce buffer's LIVE RANGE IS ONE SYSCALL — bytes in, one
`read(2)`/`write(2)`, bytes out, nothing retained.  So the only thing that can
overlap it is ANOTHER CPU, never a second connection on the same CPU: there is
no preemption and no signal that re-enters Lisp, so a thread is inside exactly
one socket call at a time.  Partition by what can actually overlap.  A
per-connection buffer would additionally be an allocation per fd with an
ownership question attached.  Same answer `%THR-TS` gives for timespecs and
`%GC-SCRATCH-CELL` for the collector's per-collection state.

`net/hosted-sockets-post.lisp` (x64-hosted only, baked after
`net/hosted-sync.lisp`, overriding the three seam functions by last-defun-wins)
maps ONE 1 MB anonymous page from two BSS words — **0x10000DF0 / 0x10000DF8, the
last two free words below the MCGC config block at 0x10000E00** — holding
per-CPU sockaddr scratch, per-CPU pollfd arrays, the server control block and
16 x 64 KB of staging.  **SIZE IS NOT THE FIX**: the chunking loop would be
equally correct at 64 bytes.  It also ends a hazard that was never about
threads — the old buffer WAS `mvm/cl-fileio.lisp`'s file-I/O page, so reading a
file during a socket transfer corrupted one of them on ONE CPU.

**poll(2), AND THE REASON IS NOT PREFERENCE.**  A thread per connection is NOT
AVAILABLE: `net/hosted-actors-post.lisp` has one stack, one thread block and one
TID word, so the image runs exactly two OS threads and a thread-per-connection
server would serve ONE client.  `select` has the kernel rewrite its fd_sets, so
the set is rebuilt every pass anyway.  `epoll` wins at thousands of fds and
loses here — a kernel object with a lifetime, and an edge-triggered mode whose
failure is "a socket you did not fully drain goes quiet forever".  **`poll` is
syscall 7 with THREE arguments, so it rides the existing `syscall3` trap: no new
opcode, no `syscall6`, no translator change on any back-end.**  Each CPU polls a
DISJOINT fd set, so the loops need no lock between them; a write that fills the
send queue waits on POLLOUT through a RESERVED pollfd entry so it never disturbs
the loop's own set.

**MEASURED** (`test/run-socket-server.sh`, 4 concurrent connections x 1 MiB,
client = PYTHON, 10 of 10):

| | |
|---|---|
| accepted / retired / held at once | 4 / 4 / 4 |
| CPU 0 | 2 097 152 bytes over 88 read events |
| CPU 1 | 2 097 152 bytes over 105 read events |
| max handlers inside at once | **2** |
| overlap witnesses / handoffs | **135 / 135** |
| wrong-CPU services, accept/poll/echo errors | 0 |
| fd probe before / after | 4 / 4 |

**THE WITNESS CAN ANSWER BOTH WAYS** — the trap 0fdb21a caught in its own
checksum.  `MODUS_SK_NCPU=1` on the same workload gives max-inside **1**,
overlap **0**, handoffs **0**, and the test ASSERTS those on that arm (10 of 10).

**THE NEGATIVE CONTROL is `MODUS_SK_SHARED=1`** — every CPU back on the one
process-global page, same binary, same workload.  **10 of 10 FAIL**, all four
connections each time, with CROSS-CONNECTION CONTAMINATION (connection 0
receives connection 1's/2's/3's pattern byte).  **And note what that means: the
server's OWN checks still pass in the control, because the server counts bytes
and not contents.  Only the external client sees it.  That is the argument for
having one that is not modus.**

**A MODUS STREAM ALREADY WORKS OVER A SOCKET FD**, which is the finding that
matters most for glass (it does 100% of its RFB I/O through Lisp streams).
`mvm/cl-fileio.lisp`'s `%MAKE-FILE-STREAM-FULL fd dir element-type` takes a RAW
FD; pointed at a socket it does `write-byte`, `write-sequence` of 8192,
`force-output`, `read-byte`, `read-sequence` and `close` verbatim.

**BUG FOUND, NOT FIXED: `(listen stream)` returns T on a DRAINED socket** while
`poll(2)` on the same fd correctly reports 0 ready.  `LISTEN` cannot be used as
a readiness test.  Small, isolated, and wrong regardless of glass.

**THE GAP MAP IS `docs/rfb-socket-gap-map.md`, and its headline is that SOCKETS
ARE NOT WHAT STANDS BETWEEN MODUS AND GLASS.**  Glass is thread-per-client TIMES
TWO (a reader parked in `read-byte` and a sender parked on a waitqueue, per
connection; `make-thread` appears 99 times), and modus has ONE spare thread
whose spawn takes a raw native entry address rather than a closure — so the
literal shim serves ZERO clients.  The synchronisation half of `sb-thread` is
largely already there (futex mutex + condvar); `sb-alien` is two libc functions
that are both syscalls; `sb-posix` production use is 12 sites all in one file.
Proposed next rung is NOT "port glass": load `glass/fb` — which depends on
nothing and already carries `#-sb-thread` arms — with no sockets and no threads.

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

### i386 allocation zero-init (#259) — DONE, and it fixed nothing (measured)

`*i386-alloc-fill*` (default `:zero`) now fills every word an i386 allocator
does not otherwise write, at 8 sites: cons tail (+8/+12), `:alloc-cons` (all
four words), `:alloc-obj` payload + alignment tail, `:alloc-array`,
`:alloc-u8`, `:sap-new` tail, and the double-float granule's 3 tail words.
`:alloc-string` content stays UNFILLED behind `*i386-fill-strings*`, mirroring
translate-x64's deliberate exclusion.  The gap was real and bigger than x64's:
16-byte alignment is a FOUR-word granule at 32 bits, so half of every cons was
uninitialised, and `copy_object`'s cons arm copies 8 bytes while advancing the
free pointer by 16 — the stale pair rides into to-space at every collection.

**The false-root payoff hypothesis is DISPROVED, with numbers.**  A diagnostic
build (`*i386-gc-instrument*`, default off — scan_word counters + a 32-byte
binary `write(2,…)` per collection) measured **15.7% of every word the
collector sweeps was allocator-uninitialised** (4,106,804 of 26,089,717 at GC
25 of an alexandria load).  Making them inert moved the conservative-root
population by **0.06%**: in-range candidates 3,686,693 → 3,684,417, copies
3,654,082 → 3,652,347 (same tree, same workload, same GC index).  Structural
reason: i386 already gates every candidate on the object-start AND cons-kind
bitmaps plus a size sanity guard, so a surviving false root can only copy a
REAL, correctly-typed object — **retention, never corruption**.  Note the
start-bit gate is nearly a no-op in a cons-dense 32-bit heap (it rejected 512
of 3.69M); the cons-kind gate did 63× more work (32,099 rejects).  Do not
re-derive this: it is hardening, priced at +6.0% image (41,388,856 →
43,865,800) and +0.50% aggregate wall over 18 ladder libraries.

Same-run control (both arms built from one tree, ladders run CONCURRENTLY,
22/22 terminated): base clean 7/22, ok 65, err 39, missing 8 → zero clean
**8/22**, ok 48, err 33, missing 31.  The whole ok/missing swing is ONE
library: `alexandria-ql` exit-139 at `P1.curry`.  That outcome is a coin flip
for a FIXED binary — base segfaulted there standalone and completed in the
ladder, and the pre-change baseline recorded base itself at exit-139 — so
treat ±1 clean and ±20 probes on alexandria as harness noise, not signal.
Real per-lib deltas: `parse-float-ql` 1 ok → 2 ok (now clean), `cl-base64-ql`
0 ok → 1 ok; every other library bit-identical.

The `MVM: unknown opcode` class **SURVIVES** and is NOT a GC/stale-reference
class: named-readtables fails 10 top-level forms with the *same* (opcode, PC)
pair in BOTH arms — deterministic.  That is the "called a non-function" shape
documented at `mvm/compiler.lisp` ~8057 (a fixnum used as a code address).
`named-readtables-ql` (~430s) is a far cheaper repro than `alexandria-ql`
(~19min, exit 139 at `P1.curry`).

### i386 CONVENTION-SLOT ADDRESS DIVERGENCE (#260/#261) — two bugs, both FIXED

Chasing the unknown-opcode turned up a distinct, higher-impact class: a
convention slot that ONE side of a contract addresses differently on i386.
There are three such slots (`nargs`, `cenv`, `mv-count`) and the rule that
separates them is **who reads it**:

- `cenv` — reached from shared CL source only through the `(%get-cenv)`
  PRIMITIVE, so each translator may keep its own slot.  Always correct.
- `nargs` (#260) — genuinely per-target (x64/aarch64 park it at `#x10000150`;
  translate-i386 has no spare physical register and uses globals+0x0C).  Only
  `:set-nargs`/`:get-nargs` touch it — but `%MACRO-EXPANDER-SHIM` /
  `%INTERP-MACRO-SHIM` read the raw literal `#x10000150`, a word i386 never
  writes.  `(= nargs 2)` was never true, so **every**
  `(funcall (macro-function 'X) form env)` — and therefore MACROEXPAND, which
  funcalls MACRO-FUNCTION's result — signalled PROGRAM-ERROR on 32 bits while
  MACROEXPAND-1 (dispatches via `%RAW-MACRO-EXPANDER`) worked.  Fix: use the
  `(%get-nargs)` primitive.
- `mv-count` (#261) — NOT per-target.  `mvm/compiler.lisp` bakes
  `+mv-count-addr+` (`#x10000090`) into every MULTIPLE-VALUE-BIND /
  MULTIPLE-VALUE-LIST / VALUES / NTH-VALUE expansion, and shared CL source
  (prelude, cl-eval, cl-clos, gc) reads the same literal.  translate-i386
  relocated it with its private block, so the ONE writer (`:set-mv-count`)
  pointed at an address with ZERO readers: every function epilogue's
  "I returned exactly one value" reset was DISCARDED while `(values a b c)`
  still stored 3, leaving the count monotonically stale-high.  Fix: bind
  `*mvcount-addr*` to `modus.mvm::+mv-count-addr+`, never relocate it.

30-SECOND REPRODUCERS (they beat a 430 s ladder run — use them first):

```
./modus --eval "(print (multiple-value-list (typep 1 'integer)))"
   (T)    x64/aarch64        (T T)  i386 pre-#261   ; also (concatenate …), (coerce …)
(defmacro mm (x) (list 'quote x))
(funcall (macro-function 'mm) '(mm 1) nil)
   works  x64/aarch64        PROGRAM-ERROR  i386 pre-#260
```

x64/aarch64 CLI images are BYTE-IDENTICAL across #261 (translator-only).
#260 touches `cl-eval.lisp`, so it moves x64 codegen (37,960,175 →
37,960,551) and was gated on the 64-shard ANSI run: CTL 17522 / FIX 17519,
CHUNK-CRASH 0 and FILE-WEDGE 30 on both, and all 3 deltas cleared on a
deterministic single-process recheck (`read-byte` 24167/24168 pass in both;
`times` 14486 is 1-in-5 flaky in BOTH arms) — **net regression 0**.

**MERGE STATUS — #261 YES, #260 NOT YET.**  The i386 ladder separates them
cleanly (attribution build = MV-count fix alone):

- #261 alone is ladder-neutral on the libraries #260 disturbs
  (bordeaux-threads / iterate / puri: identical probe values and EXIT=0), with
  ONE measured exception — `cl-base64` `P1.enc` goes `YWJj` → `(ERR
  TYPE-ERROR)`.  A correct MV count changes what a MULTIPLE-VALUE-BIND sees,
  so this reads as a newly-EXPOSED downstream gap rather than a defect in the
  fix, but it is unchased and should be treated as an open follow-up.
- #260 converts **4 of 22 ladder libraries** (bordeaux-threads, cl-base64,
  iterate, puri) from EXIT=0 to **EXIT=139**, by unlocking a macro path i386
  could never previously execute.  Root cause of that SIGSEGV is NOT FOUND.
  The obvious candidate was disproved by experiment: `%MACRO-EXPANDER-SHIM`
  really did read `(car (%get-cenv))` after `(setq *%mexp-trace* …)`, and on
  i386 cenv is a GLOBAL SLOT (globals+0x10) that any call clobbers — a genuine
  latent hazard, fixed by hoisting both slot reads into a LET* — but the
  crashes persist with the hoist in place (puri merely fails ~2 minutes
  earlier).  Do not merge #260 until that is chased.

RULE the cenv work established anyway: **nargs and cenv are BOTH
call-clobbered convention slots on at least one target.**  Read them into
locals as the first thing a shim body does; never re-read one after a call.
x64/aarch64 hide this because cenv is R13 and comes back intact.

WHEN COMPARING i386 TO x64, REMEMBER x64 DEFAULTS TO **JIT ON** AND i386 IS
PURE-INTERPRET (`build-cli-common.lisp` "NAMED DIVERGENCE").  A bare x64-vs-i386
diff confounds the two.  Rebuild the control with `MODUS_NO_JIT=1` before
concluding "i386-specific" — it is a 4-minute build and it settled the
unknown-opcode question in one run.

### The `MVM: unknown opcode` class is STILL OPEN — what it is NOT

Neither #260 nor #261 fixes it (10 failures before and after; the (opcode, PC)
pair merely MOVES).  Measured facts, so nobody re-derives them:

- i386-only.  x64 passes all 10 forms **with AND without the JIT**.
- The 10 forms are exactly named-readtables' 10 `define-api` forms.  A
  standalone harness (no `install-tarball`) reproduces 2 of them —
  `make-readtable` and `ensure-readtable`, the only two whose DEFUN
  lambda-list has an `&optional` spec **with a supplied-p variable**.
- NOT the macro body: deleting the entire `#-sbcl (progn (check-type …))`
  block from `define-api` still fails.
- NOT the emitted form: `load`ing the x64-produced macroexpansion of
  `make-readtable` VERBATIM on i386 succeeds.
- NOT reproducible by hand: `(progn (declaim (ftype …)) (locally (defun f
  (&optional (x nil xp) &key k) …)))` evaluated the same way passes on i386.
- Ruled out individually: `parse-body`, `parse-ordinary-lambda-list` (both
  byte-identical to x64 on the failing lambda lists), the dotted-destructuring
  LOOP, the FLET parameter shadowing a boxed macro parameter, the inner
  ASSERTs, the docstring, `destructuring-bind`, `constantp`.
- Substituting a plain `(name)` lambda-list into the SAME macro makes it pass.
- The error always comes from `mvm-interpret`'s `otherwise` arm, i.e.
  `op-call-ind` took its `(integerp target)` branch and set PC to a bogus small
  integer.  Observed pairs across builds: 4/956, 196/945, 6/5018, 31/5019,
  7/5018 — the pair moves with unrelated code, which fits "a stale or foreign
  in-module bytecode OFFSET reached operator position", not a corrupted opcode
  stream.  Next step is to instrument that branch with the target value and the
  module identity rather than to keep bisecting source shapes.

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

- **`check-parses` at build time**: Build scripts (`build-x64-linux.lisp`, `build-fixpoint.lisp`, `build-mvm.lisp`) call `modus.mvm::check-parses` on every first-party source file before reading it. A paren mismatch in `%format-impl` once hid behind the lenient in-build reader for weeks, presenting as a fake "late cond branch miscompilation". `check-parses` fails fast with the specific file and error so this can't recur silently. If you write a new build script, wire it into your `mvm-text` wrapper.
- **Never call `CL:GENSYM` in `mvm/compiler.lisp` — use `%MVM-GENSYM`** (task #251).
  The compiler's expanders bake names into the image, so `CL:GENSYM` made the
  emitted binary a function of `CL:*GENSYM-COUNTER*` — i.e. of HOST state, not
  of source.  `expand-cl-loop`'s `(gensym "NAT")` name reaches the image (99 of
  them in `build-generic-cli`), so bumping the host counter by ONE — loading a
  host-only utility that gensyms, adding a `defclass` to a build script,
  printing through a new stream class so PCL computes a dfun — changed **435
  bytes** of the 37 MB binary with no semantic difference.  `%MVM-GENSYM` draws
  from `*mvm-gensym-counter*`, which the compiler owns and `mvm-compile-all`
  resets, so names depend only on the forms compiled and their order.  Two
  points that are easy to get wrong if you touch it: (a) names are
  `PREFIX%<digits>`, and the `%` is load-bearing — this compiler resolves
  variables by NAME HASH, so an uninterned `Z1` **is** the corpus's `Z1`;
  `CL:GENSYM` only escaped that because the host counter was already in the
  tens of thousands.  (b) In-image runtime eval still uses `CL:GENSYM`
  (image state, emits no bytes); a compile-scoped counter would reset per form
  and hand two forms the same name.  `check-no-host-gensym` (CHECK F in
  build-checks.lisp) ratchets this at exactly one sanctioned call.
  **Reproducibility is testable: build, perturb something host-only, rebuild,
  `cmp`.** Verified byte-identical under bare `(gensym)` calls, a
  `defclass`/`defmethod`/PCL-dfun battery, a different build directory, 2000
  fresh interns + hash-table iteration + `*random-state*`, `LC_ALL`/`TZ`/clean
  env, and checks-on vs checks-off.
- **`compile-call` warns on list-headed non-lambda fn**: The old fallback silently emitted `CALL-INDIRECT` on whatever the "function expression" evaluated to, which routed every downstream cond clause through T/NIL indirection for the `~( ~)` paren bug. Now any `((test) body)`-shaped fn (other than `(lambda …)` or `(setf NAME)`) prints `;; WARN compile-call: …` to stderr with the source location. The code still emits the indirect call so ANSI tests that deliberately construct bad callables keep compiling.
- **`check-global-inits` at build time (#243)**: `mvm/build-checks.lisp` (host-only, loaded by `lib/load-mvm.lisp`, never baked) encapsulates `build-image` and audits the assembled blob **per build script** for globals whose initialisation never runs in *that* image — Active Limitation #7 below. Two checks: `ORPHANED-INITFORM` (non-NIL defvar initform in an image whose `kernel-main` never reaches `init-all-globals`, with no reachable assignment) and `ORPHANED-INITIALISER` (an `INIT-*`/`%INIT-*` defun that sets a global but is unreachable from `kernel-main` — the #242 shape, where the initialiser was correct and simply not called by 3 of 10 build scripts). It fails the build and names the variable. Every build prints a one-line summary. Known-unfixed instances live in `*GLOBAL-CHECK-BASELINE*` (printed, never fatal) so new ones still fail; knobs `MODUS_GLOBAL_CHECK=0|warn|force|dump`. Blobs over 8 MB (only the 4 ANSI gate runners) are skipped by default — the re-read costs ~20 min there.

## MVM Compiler — Active Limitations

1. **Last-defun-wins**: All calls resolve to the LAST defun of a given name. You cannot alias a function before overriding it. Use different names.
2. **Variable-index ASET bug**: When `(aset arr idx val)` with a variable `idx` is a non-last form (`dest=nil`), the value may not load correctly. **Workaround**: `(let ((dummy (aset arr idx val))) body)` forces `dest=frame-slot`.
3. **Symbol identity — per-package (CLHS 11.1.2) via SYMBOLS_PLAN phase 1 (a1327d6)**: `%intern-symbol-pkg` and `intern` both key the global intern table by a composite of NAME-HASH and PKG-HASH.  Within-package `(eq 'foo 'foo)` holds (covers compile-time literal vs runtime READ vs the CL-symbol-wrapper / native-MVM-sym boundary within a single package).  Cross-package `(eq cl-test::x ds4::x)` returns **NIL** — CL-correct, was previously T under the daa0763 unified model.  See SYMBOLS_PLAN.md.
4. **YIELD opcode**: Emitted at end of every `loop` iteration. On AArch64 bare metal, must be SEV+WFE (not just WFE which would stall on Cortex-A53).
5. **cons cells in actor context**: May get corrupted across yield/context-switch boundaries. Inline data construction instead of relying on cons returns when the result crosses scheduling points.
6. **Funcall tag — RESOLVED via OR-3**: Function pointers are explicitly tagged in `mvm-fn-addr` (translate-x64.lisp ~line 2794): `LEA + OR-3`.  Raw fn-code is NOP-padded so the low nibble is 0; OR-3 yields tagged fn pointers with low nibble `0x3` always.  Tags `cons=0x1`, `fn=0x3`, `char=0x5`, `obj=0x9` are mutually disjoint in the low nibble, so the historic "funcall-tag-collision" / "fn-addrs at vaddr ???05" classes are STRUCTURALLY impossible.  When bytecode shifts move function addresses, the OR-3 still produces a clean tag — predicate flips don't happen via tag-nibble accident.  A change that appears to break something via "code movement" has a real semantic cause; bisect to it (see "Layout shift broke an unrelated test" below).
7. **defvar init-thunks not run**: `(defvar *foo* 42)` declares `*foo*` but does NOT set it to 42 at boot — `init-all-globals` is skipped.
   **"Present in the image" and "actually invoked" are different questions.** The COMPILER emits `init-all-globals` from whatever source it compiles, so it is in essentially every image — while nothing calls it. i386 hit this in a new place (WS5 layer 5): the symbol map showed `INIT-ALL-GLOBALS` present, yet every defparameter the compiler and `mvm-eval` depend on sat at NIL, so `(eval 42)` SIGSEGV'd with nothing to work with. Calling it is one line with a large blast radius. When a runtime table reads as empty, check whether its initialiser was ever CALLED before concluding it is missing.

   **CORRECTED 2026-08-12 — "defvars default to NIL" was FALSE; they were UNBOUND.** A `(defvar *x* nil)` used to emit NO init thunk at all (compile-defvar skipped a NIL initform), so `*x*` was neither 42 nor NIL — reading it signalled `UNBOUND-VARIABLE`. That hit 167 defvars, `CL:*MODULES*` among them, which broke `PROVIDE`/`REQUIRE` and the trivial-indent ladder rung. Fixed at f8ec62b: a NIL initform now emits a thunk, guarded by the CLHS rule that DEFVAR assigns only if not already bound. That guard is load-bearing — boot steps that run BEFORE `(init-all-globals)` store into globals that also have a `(defvar *x* nil)` elsewhere (e.g. `(setq *macro-table* (make-hash-table))` at build-cli-common.lisp:882 — moved there from build-generic-cli:740 when the two CLI lineages were merged at f98bdef), and an unguarded store re-NILs them and breaks the reader. DEFPARAMETER is unchanged: CLHS makes it assign unconditionally, so a NIL-initform defparameter STILL emits no build-time thunk.

   Initialize required values explicitly via setq in an init function (e.g. `*pkg-tag*`, `*sym-tag*` set in `%init-packages`). Predicates that compare against an uninitialized tag with `(eql (car x) *tag*)` will return T for *any* cons-with-NIL-car — see `%pkg-p` history.
8. **`compile-ash` inlines `:shl`/`:sar` for small constant counts (≤30) WITHOUT a fixnum tag check — corrupts a BIGNUM value.** `(ash bignum-val small-const-count)` shifts the raw tagged word, which is right for a fixnum (low bit 0) but mangles a bignum pointer (tag 0x9): `(ash (ash 1 200) -1)` :sar's the heap address to garbage, `(ash 2^200 -200)` returns a bogus fixnum. Variable-count and large-constant-count ash already route to runtime `bignum-ash` (correct). The bignum-ash runtime fix (9463e26) makes `(ash 1 N)` produce a CORRECT big bignum, which then SURFACES this latent compiler bug in any code that shifts those bignums by a small constant — `%print-integer-in-base`-adjacent format-runtime paths (format-d/o/x/b chunk-crashes), `gcd.5`/`logcount.7-8` (`(ash 1 200/300)` bounds → big-bignum operands → `(ash x -1)`). **FIX IS A COMPILER CHANGE but every attempt so far BROKE the harness's SIGSEGV-handler longjmp recovery** (a previously-recoverable pre-existing crash in nunion/modules went fatal; the 4x code-size blowup of the source-expansion `(if (fixnump g) (%ash-fixnum-raw g k) (bignum-ash g k))` is the suspected trigger — possibly fn-table/branch-displacement). The individual ash results were CORRECT (probes 9920-9925 passed); only harness recovery regressed. A recovery-safe fix is the open follow-up. Workaround for now: route through `bignum-ash` explicitly when a value may be a bignum.

   **Related open bignum bugs surfaced by the WS5 self-host (2026-07); ALL THREE
   still need a root fix — they were WORKED AROUND, not fixed. See
   [[reference_bignum_bug_cluster]] for repro cases.**
   - **8a. `compile-ash` LEFT-shift overflow (the twin of #8).** `(ash <fixnum>
     small-const)` inlines a raw `:shl` with NO overflow promotion, so
     `(ash 4611686018427387903 1)` → **−2** (wraps) instead of the bignum
     9223372036854775806. Broke compile-integer's own 2^62-1 boundary constant in
     the self-compiled product. WS5 workaround: `emit-li-tagged` emits `:li value;
     :shl` (untagged fixnum + runtime raw shift) for the tagged word — but any
     USER `(ash big-fixnum small-const)` still silently wraps.
   - **8b. `bignum-ash` with a VARIABLE count corrupts the count.** A runtime
     `bignum-ash(n, count)` on the left-shift limb path passed a CLOBBERED count
     to `%shl-limbs-mag` (gdb: count became a stack address) → SIGSEGV / runaway
     bignum. Smells like the MVM caller-save hazard [[reference_mvm_caller_save_bug]]
     on `count` across `%any-to-limbs`. WS5 workaround: `emit-rest-prologue` uses
     literal shift counts `(ash X 1)` so it inlines `:shl` instead of calling
     `bignum-ash`. Any code calling `bignum-ash` with a runtime count is exposed.
   - **8c. `:mul-checked` / `:add-checked` overflow promotion fails for NEGATIVE
     operands.** `(* -4611686018427387904 2)` and
     `(+ -4611686018427387904 -4611686018427387904)` both return small positive
     GARBAGE instead of −2^63 (positive overflow promotes fine). A real arithmetic
     correctness bug. CAVEAT: observed only via the self-host binary's mvm-eval so
     far — confirm it reproduces in the ANSI / build-generic path before treating
     it as definitively core (this session mis-attributed several bugs; verify
     first).

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
runtime function (the mvm-eval-CLOS make-instance case), a missing rewrite, a
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

### Socket / server tests — AND THE RULE THEY OBEY

```bash
# THE SERVER, against a client that is NOT modus (Python).  4 concurrent
# connections x 1 MiB, two serving threads, byte-for-byte comparison.
test/run-socket-server.sh ./modus /tmp/modus-socket-test 4 1048576 16384

# THE POSITIVE CONTROL for the concurrency witness: one serving CPU.
# max-inside must be 1, overlap 0, handoffs 0 — and the test asserts it.
MODUS_SK_NCPU=1 test/run-socket-server.sh ./modus /tmp/mst1 4 1048576 16384

# THE NEGATIVE CONTROL: every CPU back on the one shared page.  MUST FAIL.
MODUS_SK_SHARED=1 test/run-socket-server.sh ./modus /tmp/mstc 4 1048576 16384

# MODUS-TO-MODUS: the same server, a modus client, plus one 96 KB transfer in
# a SINGLE socket-send-from against a 64 KB staging buffer.
test/run-socket-modus.sh ./modus /tmp/modus-socket-modus 4 16 8192
```

**EVERY ONE OF THESE BINDS 127.0.0.1 AND AN EPHEMERAL PORT, AND NOTHING IS LEFT
LISTENING.**  The modus side calls `SOCKET-LISTEN-ON` with `(%SOCK-LOOPBACK)`
written out in full and has no other spelling available to it; the port is
`bind(0)` + `getsockname`, so no script picks a number and none can collide with
a desktop (5900-5920 or otherwise).  The server holds a **deadline** it checks
once per poll timeout, so a run whose client never arrives still ends by itself
and still closes its listener; `timeout` bounds the process as a second line of
defence; and each harness **verifies with `ss` that the port is no longer
listening** and fails the run if it is.

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
