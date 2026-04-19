# Modus

Modus is a self-hosting bare-metal Lisp operating system. It compiles Lisp to native code via the MVM (Modus Virtual Machine) — a portable virtual ISA with translators for 9 CPU architectures. The system runs SSH servers, handles USB devices, and supports cooperative actor-based concurrency, all on bare metal with no OS underneath.

## Directory Structure

```
cross/          Vestigial cross-compiler (see cross/README.md)
  packages.lisp        Package definitions (used by MVM)
  x64-asm.lisp         x86-64 assembler (used by MVM)
  cross-compile.lisp   Original Phase 0 cross-compiler (historical)
  build.lisp           Original kernel builder (historical)

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
  cl-eval.lisp       1,353L  Eval/compile/load, symbol-function table
  cl-types.lisp        519L  typep, coerce, numeric helpers
  ansi-bridge.lisp   1,888L  Test helpers (eqlt, scaffold, stubs)
  gc.lisp                    GC helper functions (Lisp-side)

runtime/        Runtime type system
  tags.lisp            Tag/subtag definitions
  packages.lisp        Runtime package definitions
```

## ANSI CL Conformance

**Real numbers (single run, current state):**
- Expected: ~17,700 tests
- Ran: ~660
- Passed: ~353 (53% of those that ran)
- Failed: ~307
- Lost: ~17,048 (fork crashed in a way the SIGSEGV handler couldn't recover)

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
                     (handler-case has gaps — many ANSI tests crash forks)
CLOS              ~  defclass, defgeneric, defmethod, standard method combination
                     :before/:after/:around, call-next-method, class precedence lists
                     (define-method-combination tests fail)
File I/O          ✓  Linux syscalls, file streams, open/close, pathnames
Eval/Compile/Load ✓  Tree-walking interpreter, load from file, macroexpand
Closures          ✓  Mutable closures via heap-allocated cells
unwind-protect    ✓  setjmp/longjmp, cleanup on both normal and error paths
GC                ✓  Cheney semi-space copying collector
Setf              ~  defsetf/define-setf-expander/get-setf-expansion/define-modify-macro
                     register, but compile-time only (no runtime defsetf in eval)
~400+ CL functions implemented
```

### What's Missing for Quicklisp

```
[ ] Runtime compile (source → bytecode → native at runtime)
[ ] compile-file → FASL
[ ] Full numeric tower (arbitrary bignums, ratios, full floats, complex)
[ ] Setf machinery (defsetf, define-setf-expander)
[✓] Everything else
```

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
Key subtags: string=#x10, symbol=#x50, closure=#x52, array=#x32, hash-table=#x41

### mem-ref Semantics (MVM)
- `:u8`, `:u32` loads → result is **tagged** (SHL 1); stores → value is **untagged** (SHR 1)
- `:u64` loads/stores → **raw** bits, no shift
- Address operand is always **untagged** (SHR 1)

### Object Layout
Objects have 8-byte header + 8-byte padding + data. Data starts at raw+16 (not raw+8).
OBJ-REF/OBJ-SET offset formula: `idx*8 + 7` (accounts for tag 9 and 16-byte header).

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

Functions are NOP-aligned to avoid addresses ending in 0x1 (cons tag collision
with closure-aware funcall dispatch). See `*x64-native-code-offset*`.

The image (especially fixpoint-ssh with networking) can grow past 0x400000. The fn table
at the end of the image must not overlap the globals or stack. Build scripts assert this.

## MVM Compiler — Active Limitations

1. **Last-defun-wins**: All calls resolve to the LAST defun of a given name. You cannot alias a function before overriding it. Use different names.
2. **Variable-index ASET bug**: When `(aset arr idx val)` with a variable `idx` is a non-last form (`dest=nil`), the value may not load correctly. **Workaround**: `(let ((dummy (aset arr idx val))) body)` forces `dest=frame-slot`.
3. **Symbol identity**: `%intern-symbol` sometimes creates duplicate objects for the same name-hash. Two symbols with identical hashes may be `eql` but not `eq`. Use `equal` for symbol comparison in critical paths.
4. **YIELD opcode**: Emitted at end of every `loop` iteration. On AArch64 bare metal, must be SEV+WFE (not just WFE which would stall on Cortex-A53).
5. **cons cells in actor context**: May get corrupted across yield/context-switch boundaries. Inline data construction instead of relying on cons returns when the result crosses scheduling points.
6. **Funcall tag collision**: Function addresses ending in `0x1` get misidentified as cons pointers by the closure-aware funcall dispatch. The translator NOP-aligns functions to avoid this. If adding boot code that changes the code offset, update `*x64-native-code-offset*` in the build script.
7. **defvar init-thunks not run**: `(defvar *foo* 42)` declares `*foo*` but does NOT set it to 42 at boot — `init-all-globals` is skipped. Defvars default to NIL. Initialize required values explicitly via setq in an init function (e.g. `*pkg-tag*`, `*sym-tag*` set in `%init-packages`). Predicates that compare against an uninitialized tag with `(eql (car x) *tag*)` will return T for *any* cons-with-NIL-car — see `%pkg-p` history.

## Known Bugs

### Mutable Closures — Global Cell Limitation

The cell-boxing for captured+mutated variables uses global cells (`%CELL-varname`).
Multiple closures from the SAME source function share the same global cell. This breaks
`(let ((closures ...)) (mapcar #'(lambda (x) (push x acc)) items))` patterns where
the lambda is created once but `acc` should be independent per call.

Heap-allocated closure cells (`is-eql-p` pattern) work around this for specific
functions. Full fix: allocate a fresh cons cell per closure creation in compile-lambda.

### `&rest` in defun + funcall-of-let-allocated-lambda → SIGSEGV

Adding `&rest args` to a defun that's later called via `(funcall (lambda () ...))`
where the lambda is bound by `let` makes the binary SIGSEGV at the funcall, often
several tests downstream of the `&rest` defun (so the crash site isn't where the
broken function was defined).

Symptom seen during ANSI work: switching `remove-if` from `(pred seq)` to
`(pred seq &rest args)` (so callers could pass `:test`/`:key`) made `run-cl-loop-tests`
crash at the very next `funcall` on a let-bound lambda. Reverting the `&rest`
restored the binary even though `remove-if` was never called between definition
and the crashing site.

Workaround: keep prelude / heavily-called runtime functions to fixed positional
parameters. If you need keyword tolerance, hand-parse the `&rest` in a wrapper
that itself uses positional params and dispatches.

### Vector literal `#(...)` inside `(let ((x #(...))) ...)` in a lambda body → SIGSEGV

The `#(...)` reader macro compiles to `(let ((arr (make-array N))) (aset arr 0 ...) ... arr)`.
When this expansion lands inside another `let`'s init-form inside a lambda body
(e.g. `(funcall (lambda () (let ((x #(a b a c))) (substitute 'b 'a x))))`), the
nested frame allocation collides with the outer frame and the runtime SIGSEGVs.

Same pattern at top level (no lambda wrap) works fine. Replacing `#(a b a c)`
with `(vector 'a 'b 'a 'c)` would dodge it — but `vector` isn't defined as a
runtime function (only opcode-style `make-array` + `aset` are), so the literal
expansion has no clean alternative.

Affects: `SUBSTITUTE-VECTOR.*`, `NSUBSTITUTE-VECTOR.*`, `SUBSTITUTE-IF-VECTOR.*`,
`NSUBSTITUTE-IF-VECTOR.*` and friends — every test of the shape
`(let ((x #(...))) (some-fn ... x))` that the per-file fork's wrap turns into
a `(funcall (lambda () ...))`. ~250 tests in the ANSI suite fail to this.

Reproducer is fragile: the same form added as a custom deftest passes when the
surrounding function is small but crashes when more deftests are added before
or after it. That points to a register-pressure or code-size threshold in the
function compiler rather than a clean bug in vector-literal expansion.

### Function size in run-cl-loop-tests changes which sub-tests crash

Adding too many `deftest` forms to `run-cl-loop-tests` makes some passing
tests start crashing — even ones that have nothing to do with the new tests
and were passing in a smaller version of the function. Some compile-state
(register pressure? code buffer size? branch displacement limits?) silently
flips at a threshold. Workaround: split big test runners into multiple defuns
called sequentially.

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
