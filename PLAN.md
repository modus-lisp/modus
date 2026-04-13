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

## Garbage Collection

No GC today — bump allocator, never frees. Any long-running program 
eventually exhausts the heap. This is the #1 blocker for real applications.

### Why it's simple for us

The JVM's GC is ~80K lines of C++ because it solves concurrent compaction 
on terabyte heaps across thousands of threads. We have:

- **Tagged values** — bit 0 tells you fixnum vs pointer. Bits 0-3 give 
  the full type. No OOP maps needed. Just scan memory and the tags tell 
  you what to trace.
- **Cooperative scheduling** — GC at yield points. World is already stopped. 
  No read/write barriers between threads. No safepoints.
- **Per-actor heaps** — each actor has its own memory region. GC is per-actor.
  Actor 1 collecting doesn't pause actor 2. This is the Erlang model.
- **Message copying** — actors communicate through staging buffers (already 
  implemented). The serialization boundary IS the GC boundary. No shared 
  mutable heap.

### Stages

**Stage 1: Cheney copying collector (~200 lines)**
```
from-space: [header | obj1 | obj2 | ... | free →]
to-space:   [empty                              ]

On GC:
  1. Scan roots (stack, globals) — tag bits identify pointers
  2. Copy each live object from from-space to to-space  
  3. Update all pointers (forwarding pointer in old location)
  4. Swap spaces, reset allocation pointer
```
Allocation stays bump-pointer (what we already have). Collection cost is 
proportional to LIVE data. Dead objects (most of them) cost nothing.

**Stage 2: Generational (+300 lines)**
```
nursery (2MB):   [young objects — collected frequently]
tenured (256MB): [survivors — collected rarely]
```
Write barrier: one instruction after set-car/set-cdr/aset. Records 
old→young pointers. Most GC cycles only touch the nursery.

**Stage 3: Per-actor SMP**
```
Core 0: Actor 1 [nursery][tenured]  ← GCs at yield
Core 1: Actor 2 [nursery][tenured]  ← GCs independently
Core 2: Actor 3 [nursery][tenured]  ← never pauses for others
```
Each core runs its own actor loop. Actors migrate via scheduler.
GC is still per-actor. Only shared state is lock-free message queues.
No global pauses, no concurrent compaction, no barriers between actors.

**Validation:** the ANSI test suite (12,600 tests) becomes the GC stress 
test. If it passes with GC enabled, the collector is correct.

## Performance: The 80/20 Stack

Current: naive code generation, every value boxed, every call is a full call.
Target: within 3-5x of SBCL for typical code.

### TCO — Tail Call Elimination (correctness + performance)

Not optional. Without it, idiomatic Lisp blows the stack. `mapcar`, 
`reduce`, `member`, recursive tree walks all depend on tail calls.

Self-tail-call: the compiler already detects tail position (for 
`set-mv-count`). Emit JMP to own entry label instead of CALL+RET. 
Reuses the current frame. Zero stack growth.

Mutual tail call: tear down caller's frame, set up callee's args, 
JMP. Harder but covers A→B→A patterns.

### Inline Small Functions (2-5x speedup)

`(cadr x)` is currently a full CALL with 1120-byte frame setup for 
two instructions of actual work. Inline any function < N IR instructions 
at the call site. The compiler already inlines `car`, `cdr`, `+` as 
builtins — extend to user-defined functions via `(declaim (inline f))` 
or auto-detect.

### Leaf Function Optimization (1.5-2x)

Functions that make no calls don't need `sub rsp, 1120`. The compiler
already tracks this (`form-contains-call-p`). Skip frame setup for
leaf functions — use registers only.

### Fixnum Loop Unboxing (5-10x numeric loops)

`(dotimes (i 1000000) ...)` — keep `i` as raw integer for the whole
loop body. Currently every `(+ i 1)` untags, adds, retags. Detect
`dotimes`/`loop for i from` patterns and unbox the counter.

### Constant Folding (free)

`(+ 3 4)` → `7` at compile time. Extend to all pure functions with
constant arguments.

### Not Needed (yet)

- SSA form — diminishing returns without full type inference
- Graph coloring register allocation — memory is fast, spills are cheap
- Block compilation — optimize across function boundaries

Full type inference is SBCL's killer feature (20 years of work). 
We can get 80% of the practical benefit from the above without it.

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
