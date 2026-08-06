# AArch64: Networking and SSH on QEMU virt

## Overview

Modus runs on AArch64 QEMU `virt` with E1000 networking and an SSH server. The networking stack (~4800 lines) is shared between x86-64 and AArch64 via thin architecture adapters. The AArch64 build uses the MVM compiler pipeline (Source → MVM bytecode → AArch64 native) and runs single-threaded (no actor model, no runtime compiler).

```bash
# Build and run
./scripts/run-aarch64-ssh.sh

# Connect
ssh -p 2222 -o StrictHostKeyChecking=no test@localhost

modus> (+ 1 2)
= 3
modus> (defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))
= FACT
modus> (fact 10)
= 3628800
```

---

## Architecture

### Shared networking code

The networking/crypto/SSH code lives in `net/` and is architecture-independent — pure `mem-ref` MMIO operations and fixnum arithmetic. Each architecture provides a thin adapter (~200 lines) defining PCI access, I/O delays, and memory addresses.

```
net/
  arch-x86.lisp          x86 adapter: I/O port PCI, addresses at 0x05xxxxxx
  arch-aarch64.lisp      AArch64 adapter: ECAM PCI, addresses at 0x41xxxxxx
  e1000.lisp             E1000 NIC driver (~300 lines)
  ip.lisp                ARP, IP, UDP, TCP, DHCP, DNS, ICMP (~1100 lines)
  crypto.lisp            SHA-256, SHA-512, ChaCha20, Poly1305, X25519, Ed25519 (~1900 lines)
  ssh.lisp               SSH server (~1100 lines)
  aarch64-overrides.lisp Single-threaded overrides for AArch64 (~100 lines)
```

### Arch adapter interface

Shared code calls these functions, defined per-architecture:

| Function | x86 | AArch64 |
|----------|-----|---------|
| `pci-config-read` | I/O ports 0xCF8/0xCFC | ECAM MMIO at 0x3f000000 |
| `pci-config-write` | I/O ports 0xCF8/0xCFC | ECAM MMIO at 0x3f000000 |
| `io-delay` | `(io-in-byte #x3F8)` ×5000 | `(mem-ref #x09000000 :u8)` ×5000 |
| `write-byte` | Serial + capture flags | Serial + capture flags |
| `e1000-state-base` | `#x05060000` | `#x41060000` |
| `ssh-conn-base` | `#x05080000` | `#x41080000` |
| `ssh-ipc-base` | `#x300000` | `#x41100000` |

### Build pipeline

Both x86 and AArch64 share the same source files. The build scripts differ only in which adapter is loaded first and which `build-image` target is used:

- **x86**: `build-kernel-mvm` in `build.lisp` reads shared files as form lists via `read-source-forms`, appends to `*runtime-functions*`
- **AArch64**: the `aarch64/bare/qemu/ssh` cell in `mvm/build.lisp` reads shared files as text via `read-file-text`, concatenates with REPL source, compiles with `(build-image :target :aarch64 :source-text combined)`

Source ordering matters: the last `defun` of a given name wins (NFN table). `kernel-main` is defined first (SSH entry point), then net source (function definitions), then REPL source (eval-sexp, etc.), then overrides.

---

## Memory layout

QEMU `virt` machine, 512MB RAM starting at 0x40000000.

```
0x09000000       PL011 UART (TX/RX)
0x3f000000       PCI ECAM configuration space
0x40200000       Kernel image (native code, ~500KB)
0x40400000       Stack (grows down)
0x41000000       E1000 DMA region
  +0x000000       RX descriptors (128 × 16 bytes)
  +0x001000       RX buffers (128 × 2048 bytes)
  +0x041000       TX descriptors (64 × 16 bytes)
  +0x041400       TX buffers (64 × 1536 bytes)
0x41060000       Network state (IP, MAC, gateway, ARP table, etc.)
0x41080000       SSH connection buffers (4 connections × 16KB)
0x41100000       SSH IPC shared state (line editor, eval, capture)
0x42000000       Wired memory (scratch)
0x44000000       Cons space (16MB, bump-allocated)
0x45000000       General heap
```

### SSH IPC memory map (at 0x41100000)

```
+0x14       Capture flags (bit 0 = capture to buffer, bit 1 = suppress serial)
+0x18       Capture buffer write position (u32)
+0x20       Line buffer read position (u32)
+0x24       Line buffer length (u32)
+0x28       Line buffer data (256 bytes)
+0x100      Capture output buffer (4096 bytes)
+0x12800    Line editor: line length (u64)
+0x12808    Line editor: cursor position (u64)
+0x12810    Line editor: escape sequence state (u64)
+0x12A00    Prompt mode flag (u64, 0 = "> ", nonzero = "modus> ")
+0x12A08    Return code (u64, 1 = enter, 2 = ctrl-d)
+0x60000    Persistent globals pointer for eval (u64)
```

---

## Single-threaded SSH

On x86, the SSH server uses the actor model: a network actor receives packets and dispatches to per-connection actors. On AArch64, the system is single-threaded:

- `net-actor-main` polls E1000 in a tight loop (`receive` → `e1000-receive` → process packet)
- SSH handshake, encryption, and channel data are processed synchronously
- The line editor (`handle-edit-byte`) buffers keystrokes until Enter
- Expression evaluation uses a buffer-based reader instead of UART

### Why a buffer-based reader?

`read-char-serial` compiles to TRAP #x0301, which the MVM compiler inlines as a PL011 UART read. This is hardwired — it cannot be overridden by `defun`. But SSH input arrives via network packets, not UART.

Solution: `aarch64-overrides.lisp` overrides `ssh-do-eval-expr` with a buffer-based reader:
1. `handle-edit-byte` accumulates keystrokes into a line buffer at ssh-ipc-base+0x28
2. On Enter, the line buffer becomes the FIFO for the reader
3. `buf-read-sexp` / `buf-read-list` parse s-expressions from the buffer
4. `eval-sexp` (from repl-source.lisp) evaluates the parsed expression
5. `ssh-print-sexp` prints the result via capture-aware `write-byte`
6. The captured output is flushed as an SSH channel data packet

### Capture-aware output

`write-byte` checks flags at ssh-ipc-base+0x14:
- Bit 0 set: also write to capture buffer (for SSH output collection)
- Bit 1 set: suppress UART output (avoid echoing SSH traffic to serial)

During eval, both bits are set. The captured bytes are sent as a single SSH channel data message.

---

## Key bugs fixed

### Alloc pointer alignment

`try-alloc-obj` (soft allocation for `make-array`) computed `total = 8 + padded` where `padded` was 16-aligned, but `8 + 16k` is not 16-aligned. After hundreds of `make-array` calls during SSH/crypto initialization, the cons alloc pointer (x24) drifted to a non-16-byte-aligned address. Then `cons` (which does `x24 + 1`) produced pointers with tag 0x9 (object) instead of 0x1 (cons), causing `consp` to always fail.

**Symptom**: `(+ 1 2)` over SSH showed `= ?` instead of `= 3`.

**Fix**: Round `total` to 16 bytes: `(logand (+ 8 padded 15) (- 0 16))`. Also fixed the `ALLOC-OBJ` opcode in all architecture translators.

### SHLV/SARV variable shifts

The MVM ISA originally only had SHL/SAR with immediate shift counts. Variable-count shifts (used in Ed25519 scalar multiplication and X25519 Montgomery ladder) silently treated register IDs as literal shift amounts. Fixed by adding SHLV (#x2F) and SARV (#x32) opcodes to all 7 translators.

### Timer flag / ephemeral key address collision

`enable-gic-timer` in `net/arch-aarch64.lisp` wrote a timer-enabled flag to `state+0x700`. The ephemeral X25519 public key occupies `state+0x6E4..0x703` (32 bytes). The flag overwrote the last 4 bytes of the key with `01 00 00 00`, causing the SSH client to compute a different shared secret and fail signature verification.

**Symptom**: `ssh_dispatch_run_fatal: incorrect signature` on every SSH connection.

**Fix**: Moved the timer flag from `state+0x700` to `state+0x704`.

**Diagnostic**: `python3 tests/ssh-kex-dump.py localhost 2222` — performs key exchange manually, dumps raw KEXDH_REPLY bytes, and independently verifies the Ed25519 signature. Before the fix, reported the ephemeral key ending in `...01000000` instead of `...09f8a209`.

### Pre-computed host key

Ed25519 key generation takes too long on MVM-compiled AArch64 (~minutes for scalar-basepoint multiply). The SSH build embeds a pre-computed host key (all-zero private key) directly in `kernel-main`, skipping runtime generation.
