# ARM32: Fixpoint + SSH Implementation Plan

## Goal

Add ARM32 (ARMv7, 32-bit A32 mode) as the 4th architecture in the fixpoint cross-compilation chain and SSH server. Extends the proof from 5 to 9 SHA256 equalities across 4 architectures.

## Architecture Decision: ARMv7 virt

Using QEMU `-M virt -cpu cortex-a15` (not versatilepb ARMv5):

- **E1000 PCI** at ECAM 0x10000000 — reuse existing `e1000.lisp`, no new NIC driver
- **ARMv7 instructions** — SDIV/UDIV (no software divide), MOVW/MOVT (2-instruction imm32)
- **Translator ready** — `*arm32-v7*` flag, `install-armv7-translator` exists
- **Same constraints** — 32-bit A32 mode, 30-bit fixnums, identical to i386

versatilepb would require a LAN91C111 driver (~300 lines) for no benefit.

## Phase 1: ARM32 REPL

Validate translator + boot before fixpoint work.

### Files to create

**`mvm/build-arm32-repl.lisp`** — copy from build-i386-repl.lisp:
- Target `:armv7`, install `armv7-translator`
- Output `/tmp/modus-arm32.bin`

**`scripts/run-arm32-repl.sh`** (historical — RETIRED; now `./scripts/run.sh arm32-repl`):
```bash
qemu-system-arm -M virt -cpu cortex-a15 -m 256 -nographic \
  -kernel /tmp/modus-arm32.bin
```

### Verification
- Boot to REPL, compute `(fact 20)` = 2432902008176640000
- Confirm serial I/O works (PL011 at 0x09000000)

## Phase 2: ARM32 in Fixpoint Chain

### 2a. Source and metadata wiring

In `mvm/build-fixpoint.lisp`:
- Add `translate-arm32.lisp` and `boot-arm32.lisp` to `*mvm-source-files*`
- Add arch_id 3 = arm32 to metadata dispatch in `build-image-cross`
- Call `(install-armv7-translator)` in package init section

### 2b. Bare-metal ARM32 translator overrides

The ARM32 translator uses CL idioms unavailable on bare metal. Override with manual equivalents:

| CL idiom | Occurrences | Replacement |
|-----------|-------------|-------------|
| `defstruct arm32-buffer` | 1 | Array + manual accessors |
| `case`/`ecase` | ~8 | `cond` chains |
| `macrolet` | 1 (arm32-translate-insn) | Inline expansion |
| `destructuring-bind` | 1 (arm32-resolve-fixups) | Manual car/cdr |
| `return-from` | 2 (arm32-encode-imm, arm32-load-imm32) | Flag-based exit |
| `push` | ~3 | Manual cons |
| `dolist`/`dotimes` | ~5 | Manual loop |
| `let*` | ~30 | Nested let (usually fine) |
| `first`/`second` | ~3 | car/cadr |

Write `arm32i-*` override functions in `build-fixpoint.lisp`, following the `a64i-*` pattern from i386→AArch64.

### 2c. 30-bit instruction encoding

ARM instructions like `#xE320F002` exceed 2^30. Split every instruction into byte3 + lo24:

```lisp
(defun arm32i-emit (buf b3 lo24)
  (img-emit-byte buf (logand lo24 #xFF))
  (img-emit-byte buf (logand (ash lo24 -8) #xFF))
  (img-emit-byte buf (logand (ash lo24 -16) #xFF))
  (img-emit-byte buf b3))
```

Create parallel `arm32i-*` encoder suite for all instruction types:
- `arm32i-dp-reg`, `arm32i-dp-imm` — data processing
- `arm32i-mem` — load/store
- `arm32i-branch` — B/BL (24-bit signed offset relative to PC+8)
- `arm32i-movw`, `arm32i-movt` — ARMv7 wide immediates

### 2d. Image assembly

**`td-assemble-gen1-arm32`**:
```
[boot preamble (emit-armv7-entry)]
[B kernel-main]
[native code]
[bytecodes]
[fn-table (12 bytes/entry, u32 LE)]
[metadata at offset 0x280000]
```

ARM32 virt memory:
- Load: 0x40000000, metadata VA: 0x40280000
- Extract PA for pmemsave: 0x40000000
- Image size: 0x280000 + 64 = 2621504

### 2e. Cross-compilation dispatch

12 new `build-*-from-*` functions (4 architectures × 3 targets each):

**ARM32 as target** (from x64/aarch64/i386 host):
- `build-arm32-from-x64` — straightforward, 64-bit host has no overflow issues
- `build-arm32-from-aarch64` — same
- `build-arm32-from-i386` — 30-bit host, reuse i386 patterns

**ARM32 as host** (targeting x64/aarch64/i386):
- `build-x64-from-arm32` — same constraints as i386→x64 (LI opcode, decode-u64 overflow)
- `build-aarch64-from-arm32` — same as i386→AArch64 (&key fix already done)
- `build-i386-from-arm32` — 30-bit→30-bit, straightforward
- `build-arm32-from-arm32` — ARM32 self-hosting

Cross-compiling FROM arm32 reuses the i386-safe patterns since both are 30-bit fixnum.

## Phase 3: ARM32 SSH

### Files to create

**`net/arch-arm32.lisp`** (~100 lines) — ARM32 virt adapter:
- PCI ECAM at 0x10000000 (32-bit, not AArch64's 0x4010000000)
- PL011 UART at 0x09000000
- State bases: e1000-state-base 0x40200000, ssh-conn-base 0x40280000, ssh-ipc-base 0x40300000
- `pci-config-read`/`pci-config-write` via ECAM MMIO
- `io-delay` reads UART register (same as AArch64, not x86 port I/O)
- capture-aware `write-byte`

**`mvm/build-arm32-ssh.lisp`** — build script:

Source load order:
```
arch-arm32 → e1000 → ip → crypto → crypto-32 → crypto-w32 → ssh →
http → aarch64-overrides → 32bit-overrides
```

Reuse unchanged:
- `e1000.lisp` — E1000 PCI driver
- `crypto-32.lisp` — field multiply (pair arithmetic, 26-bit halves)
- `crypto-w32.lisp` — SHA-256/ChaCha20 (w32 pairs, 16-bit halves) — arch-neutral
- `32bit-overrides.lisp` — 30-bit buf-read-u32 — also arch-neutral

**`scripts/run-arm32-ssh.sh`**:
```bash
qemu-system-arm -M virt -cpu cortex-a15 -m 256 -nographic \
  -kernel /tmp/modus-arm32-ssh.bin \
  -device 'e1000,netdev=net0,romfile=,rombar=0' \
  -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
```

## Phase 4: Fixpoint Proof

### Extended chain: 13 steps, 9 proofs

| Step | Chain | New? |
|------|-------|------|
| 0 | SBCL → Gen0(x64) | |
| 1 | Gen0(x64) → Gen1(aarch64) | |
| 2 | Gen1(aarch64) → Gen2(x64) | |
| 3 | Gen2(x64) → Gen3(aarch64) | |
| 4 | Gen0(x64) → i386-A | |
| 5 | Gen1(aarch64) → i386-B | |
| 6 | i386-A → i386-C (self-hosting) | |
| 7 | i386-A → x64-D | |
| 8 | i386-A → AArch64-E | |
| 9 | Gen0(x64) → arm32-F | NEW |
| 10 | Gen1(aarch64) → arm32-G | NEW |
| 11 | arm32-F → arm32-H (self-hosting) | NEW |
| 12 | arm32-F → x64-I | NEW |
| 13 | arm32-F → AArch64-J | NEW |

### SHA256 proofs

| # | Equality | Proves |
|---|----------|--------|
| 1 | Gen1 == Gen3 | x64↔AArch64 fixpoint |
| 2 | i386-A == i386-B | i386 translator determinism |
| 3 | i386-A == i386-C | i386 self-hosting |
| 4 | x64-D == Gen2 | i386→x64 cross-compilation |
| 5 | AArch64-E == Gen1 | i386→AArch64 cross-compilation |
| 6 | arm32-F == arm32-G | arm32 translator determinism |
| 7 | arm32-F == arm32-H | arm32 self-hosting |
| 8 | x64-I == Gen2 | arm32→x64 cross-compilation |
| 9 | AArch64-J == Gen1 | arm32→AArch64 cross-compilation |

### Script updates

- `scripts/run-fixpoint-i386.sh` → extend or rename to `run-fixpoint-4arch.sh`
- `scripts/run-fixpoint-ssh.sh` — add `arm32` to valid architectures

## Risks

| Risk | Level | Mitigation |
|------|-------|------------|
| ARM32 override volume (~1200 lines) | HIGH | Follow i386 pattern mechanically |
| 30-bit instruction encoding suite | HIGH | Same byte3/lo24 split as a64i-* |
| ARM virt E1000 PCI probing | MEDIUM | Test in Phase 1; confirm BAR0 address |
| ARM virt boot address | MEDIUM | Verify 0x40000000 matches in Phase 1 |
| Crypto/override reuse | LOW | Proven on i386, pure Lisp |

## Memory Layout (ARM32 virt)

```
0x09000000       PL011 UART
0x10000000       PCI ECAM configuration space
0x40000000       Kernel image (load address)
0x40200000       E1000 state (MAC, IP, ARP, crypto K)
0x40280000       SSH connection buffers
0x40300000       SSH IPC shared state
0x40400000       Stack (grows down)
0x41000000       Cons space (bump-allocated)
0x42000000       General heap
```

## Estimate

- Phase 1: ~1 hour
- Phase 2: ~2-3 days
- Phase 3: ~1 day
- Phase 4: ~few hours

About 1/3 the effort of the i386 work — the hard 32-bit problems are solved.
