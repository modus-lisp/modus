# Modus: Plan

## Vision

A self-hosting bare-metal Lisp operating system that is a full ANSI Common Lisp 
implementation. Not "passes the tests" — actually IS a Common Lisp. Capable of 
running Quicklisp, compiling itself, and evolving at runtime.

The endgame: a `modus` binary that replaces SBCL. Runs on Linux, macOS, Windows 
as a userspace CL. Runs on bare metal as the OS itself. Same compiler, same 
runtime, same REPL. One language, every layer, any platform.

## Architecture

### MVM: The Portable Core

```
Source → Reader → Compiler → MVM Bytecode → Translator → Native Code
                                  ↑                          ↓
                            (portable,              (arch-specific,
                             verified,               zero-overhead)
                             the fixed point)
```

MVM bytecode is architecture-independent. The fixpoint proof demonstrates this:
SBCL→Gen0(x64)→Gen1(aarch64)→Gen2(x64), SHA256(Gen0)==SHA256(Gen2).

Translators exist for: x64, aarch64, arm32, i386, riscv, ppc, 68k.

### Platform Targets

```
Translators (bytecode → native):     Boot stubs (OS interface):
  translate-x64.lisp       ✓           boot-linux-x64.lisp       ✓
  translate-aarch64.lisp   ✓           boot-linux-aarch64.lisp   —
  translate-arm32.lisp     ✓           boot-macos-arm64.lisp     —
  translate-i386.lisp      ✓           boot-macos-x64.lisp       —
  translate-riscv.lisp     ✓           boot-windows-x64.lisp     —
  translate-ppc.lisp       ✓           boot-x64.lisp (bare)      ✓
  translate-68k.lisp       ✓           boot-aarch64.lisp (bare)  ✓
                                       boot-uefi-x64.lisp        ✓
```

New OS target = one boot stub (~200 lines mapping mmap/read/write/exit).
Cross-compilation = pick a different translator + boot stub. Zero runtime dispatch.

WASM target (tentative — here be dragons):
```
translate-wasm.lisp        —    MVM bytecode → WASM bytecode
boot-wasm.lisp             —    WASI imports for I/O (or JS FFI)
```
MVM registers → WASM locals. Heap → linear memory. CALL → call_indirect.
Main challenge: WASM requires structured control flow (block/loop/br,
no arbitrary goto). MVM branches need relooper-style restructuring.
No register allocation needed (stack machine). i64 gives us 63-bit fixnums.
Unlocks: browser REPL, edge compute, universal distribution, sandboxed
self-modification via runtime module compilation.

### Package-Based Module System

Arch-specific code composes via packages, resolved at build time:

```lisp
;; aarch64-ssh build
(use-package :modus.crypto.native :modus.crypto)  ;; 64-bit fixnum crypto
(use-package :modus.net.e1000     :modus.net)      ;; Intel E1000 NIC
(use-package :modus.arch.aarch64  :modus.arch)     ;; MMIO, GIC, page tables

;; i386-ssh build  
(use-package :modus.crypto.pair   :modus.crypto)   ;; 32-bit pair arithmetic
(use-package :modus.net.ne2000    :modus.net)       ;; NE2000 ISA NIC
(use-package :modus.arch.i386     :modus.arch)      ;; PIC, PIT, port I/O
```

Same source code, different symbol resolution, zero runtime cost. Import at build 
time, not dispatch at call time.

### Image Format

```
[native code]      — translated for this arch, runs immediately at boot
[MVM bytecode]     — portable, for cross-compilation to other targets
[source text]      — for verification, runtime compilation, REPL
[function table]   — maps names → offsets in all three layers
[filesystem]       — in-memory FS, optional sync to block storage
```

Why embed source:
- **Hash verification** — SHA256(source) must match before compiling. 
  Supply chain security at the Lisp level.
- **Lazy compilation** — boot image only has native code for the boot path. 
  `(require :ssh-server)` compiles from source on demand.
- **Self-documenting** — `(describe 'sha256)` shows actual source.
  `(disassemble 'sha256)` shows bytecode AND native.
- **Fixpoint verification** — compile embedded source, compare bytecode 
  against embedded bytecode. If they match, the compiler is correct. 
  If not, tampering detected.

### In-Memory Filesystem

```
/boot/kernel.mvb           — boot-critical bytecode (pre-translated)
/src/modus/crypto.lisp     — source text, hash-verified
/src/modus/ssh.lisp        — compiled on first (require :ssh)
/cache/fasl/crypto.mvb     — cached bytecode after first compile
/quicklisp/...             — fetched via HTTP, stored in-memory
```

`sync-filesystem` writes to block storage (SD card, USB, disk).
On next boot, restored from block device. Hot-reload: edit source, 
`(load "/src/modus/crypto.lisp")`, new native code compiled in-place.

## CL Implementation Roadmap

See PLAN-CL.md for the detailed layer-by-layer plan.

```
Layer 0: Packages          ✓ DONE — 24 functions, 303 tests pass
Layer 1: Streams            IN PROGRESS
Layer 2: Reader             —
Layer 3: Printer            —
Layer 4: Format             —
Layer 5: Conditions         partial (handler-case works)
Layer 6: CLOS               —
Layer 7: File I/O           —
Layer 8: Eval/Compile/Load  —
```

## The `modus` Command

The self-hosted CL implementation as a userspace binary:

```bash
$ modus                          # REPL
$ modus script.lisp              # run script
$ modus --build kernel.lisp      # compile bare-metal image
$ modus --target aarch64 app.lisp  # cross-compile
```

Contains: full CL runtime + compiler + reader + printer + translators.
No SBCL dependency. Bootstraps from the fixpoint.

## Self-Improvement Architecture

A bare-metal Lisp that controls its own compiler, memory, and network stack
is infrastructure for self-improving systems:

- **Code-as-data** — Lisp source is manipulable at runtime
- **Runtime compilation** — `(load)` compiles and installs new code live
- **Fixpoint verification** — compiler correctness is provable 
- **Hash-verified source** — modifications are auditable
- **Actor isolation** — spawn processes with modified code, evaluate,
  keep improvements, discard failures
- **Network-capable** — fetch new code via HTTP/SSH on bare metal
- **No escape surface** — there's no OS underneath. The Lisp IS the machine.

## Hardware Targets (Current)

- QEMU x86-64 (virt, E1000 NIC) — primary development
- QEMU AArch64 (virt, E1000 NIC) — SSH, actors, isolation
- QEMU Raspberry Pi 3B (DWC2 USB host, CDC Ethernet)
- QEMU i386 (NE2000 ISA NIC) — 32-bit, SSH
- QEMU ARM32 (raspi2b, DWC2 USB gadget)
- Pi Zero 2 W (real hardware, USB gadget, CDC-ECM)
- ThinkPad T420 (real hardware, UEFI boot, PS/2 keyboard, GOP framebuffer)
- QEMU RISC-V, PowerPC, 68k (REPL only)

## What Exists Today

- MVM compiler: Source → IR → bytecode → native (9 architectures)
- 733 ANSI test files, ~12,600 tests, 3-8 failures
- Self-hosting fixpoint across x64/aarch64/i386/arm32
- SSH server (Ed25519, ChaCha20, X25519)
- Cooperative actor system with per-actor heaps
- HTTP client (fetch from internet on bare metal)
- USB host + device drivers (DWC2, CDC-ECM, HID)
- Intel E1000 + NE2000 NIC drivers
- GPIO/SPI/I2C/PWM peripheral control
- UEFI boot with GOP framebuffer
- handler-case with setjmp/longjmp
- declare special with shallow binding
- Minimal bignum (2-slot, 124-bit)
- ~250 CL functions implemented
- Package system (Layer 0 complete)
