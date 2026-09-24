# Modus

A bare-metal Lisp operating system. Boots directly on hardware (or QEMU) into an interactive Lisp REPL with networking, SSH, and cryptography — no underlying OS.

A self-hosting system with a portable virtual machine (MVM) targeting 9 CPU architectures, an Erlang-style actor model, SMP multicore support, and per-actor garbage collection.

## Recent work

Over ~1500 commits (April–July 2026) Modus grew from a bare-metal Lisp with
networking into a largely self-hosting ANSI Common Lisp:

- **ANSI Common Lisp (~94% per-file conformance).** Full condition system
  (handler-case/bind, restarts, `signal`/`warn`/`cerror`), CLOS with standard
  method combination and `call-next-method`, the reader and printer with
  `format` (50+ directives), pathnames and file I/O, a numeric tower (bignums,
  ratios, single/double floats, complex), and ~400 standard functions.
- **One evaluator (WS3).** Production `eval` and `load` compile each form to MVM
  bytecode and run it — the self-hosted MVM compiler runs *inside* the image,
  and the original tree-walking interpreter has been deleted. The vendored ASDF
  driver evaluates end-to-end on this path.
- **Runtime JIT (WS4).** MVM bytecode is translated to native code at runtime
  (executable memory + call relocation) behind the same evaluator seam.
- **Full self-reproduction (WS5).** A Modus binary compiles its own source to a
  native ELF and reproduces itself (modus → modus2 → modus3) with SBCL out of
  the loop.
- **GC hardening.** Conservative-root validation via an object-start bitmap, a
  heap guard band, zero-initialized allocation, and optional Bartlett
  page-pinning — closing a class of heap-corruption crashes.
- **SBCL drop-in compatibility (in progress).** Unmodified pure-Common-Lisp
  libraries load and run from source — e.g. natrium (SHA-256 FIPS-exact) and
  alexandria — with the `bordeaux-threads` dependency tree loading unmodified.
- **New platforms and features.** Bare-metal ThinkPad T420 (E1000 SSH, EHCI USB,
  VGA + PS/2 console), UEFI x86-64, ARM32, byte-packed `(unsigned-byte 8)`
  arrays, and a hosted `modus` CLI with SBCL-faithful flags.

## Feature matrix

### Platforms

|                    | x86-64 QEMU | AArch64 QEMU virt | RPi 3B QEMU | Pi Zero 2 W | i386 QEMU | ARM32 QEMU | ThinkPad T420 |
|--------------------|:-----------:|:-----------------:|:-----------:|:-----------:|:---------:|:----------:|:-------------:|
| Serial REPL        | Y | Y | Y | Y | Y | Y | - |
| VGA + PS/2 REPL    | - | - | - | - | - | - | Y |
| E1000 networking   | Y | Y | - | - | - | - | Y |
| NE2000 networking  | - | - | - | - | Y | - | - |
| USB CDC networking | - | - | Y | Y | - | Y | - |
| SSH server         | Y | Y | Y | Y | Y | Y | Y |
| HTTP server/client | - | Y | - | Y | - | - | - |
| Actors             | Y | Y | - | Y | - | - | - |
| Actor isolation    | - | Y | - | - | - | - | - |
| SMP (multi-core)   | Y | - | - | - | - | - | - |
| Self-hosting       | Y | Y | - | - | Y | - | - |
| USB HID            | - | - | Y | - | - | - | - |
| GPIO/SPI/I2C       | - | - | Y | - | - | - | - |
| GPU framebuffer    | - | - | Y | Y | - | - | Y (UEFI) |
| UART bootloader    | - | - | - | Y | - | - | - |
| EHCI USB (WIP)     | - | - | - | - | - | - | Y |

NIC driver: x86-64 and AArch64 virt use Intel E1000 (PCI). RPi uses DWC2 USB — host mode (CDC Ethernet) on QEMU raspi3b, device/gadget mode (CDC-ECM) on Pi Zero 2 W. i386 uses NE2000 ISA NIC (port I/O). ARM32 uses DWC2 USB gadget (CDC-ECM) on QEMU raspi2b. T420 uses E1000 82579LM with BIOS-preserving init.

### MVM architectures

**Eight of the nine run compiled Lisp in QEMU, verified.** The ninth, AArch64
RPi, shares AArch64's translator and is blocked by the RPi family's build
failure (#209), not by its back end. What none of them has yet is a payload
worth booting into — the two sections below are kept apart deliberately,
because conflating "the code generator works" with "the system works" is how
the claims in this file went wrong before.

#### Back end — measured

Every architecture below boots in QEMU, runs compiled Lisp, and returns the
right answer for a 14-rung ladder: a call, add/multiply, a branch, an argument,
recursion, `(factorial 10)`, `cons`/`car`/`cdr`, a loop with an early return,
three live variables, arrays, byte vectors, strings and SAPs. The answer is
read back out of guest memory over QMP and compared to an expected value —
"the image wrote something" is not a pass. **112/112 as of 5b187e4.**

| Architecture | Bits | Endian | Translator | QEMU target | Opcodes | Ladder |
|-------------|:----:|:------:|:----------:|-------------|:-------:|:------:|
| x86-64      | 64 | little | translate-x64.lisp     | `qemu-system-x86_64`                  | 137/144 | 14/14 |
| AArch64     | 64 | little | translate-aarch64.lisp | `qemu-system-aarch64 -M virt`         | 138/144 | 14/14 |
| i386        | 32 | little | translate-i386.lisp    | `qemu-system-i386`                    | 102/144 | 14/14 |
| ARM32       | 32 | little | translate-arm32.lisp   | `qemu-system-arm -M raspi2b`          |  92/144 | 14/14 |
| RISC-V 64   | 64 | little | translate-riscv.lisp   | `qemu-system-riscv64 -M virt`         |  92/144 | 14/14 |
| RISC-V 32   | 32 | little | translate-riscv.lisp   | hosted only so far (see below)        | (shared) | 14/14 hosted |
| PPC64       | 64 | big    | translate-ppc.lisp     | `qemu-system-ppc64 -M powernv`        |  92/144 | 14/14 |
| PPC32       | 32 | big    | translate-ppc.lisp     | `qemu-system-ppc -M ppce500`          |  92/144 | 14/14 |
| 68k         | 32 | big    | translate-68k.lisp     | `qemu-system-m68k -M virt`            |  92/144 | 14/14 |
| AArch64 RPi | 64 | little | translate-aarch64.lisp | `qemu-system-aarch64 -M raspi3b`      | (shared) | not in the ladder |

The opcode counts are the honest measure of how much of the ISA each back end
covers, but they are a poor proxy for capability and should not be read as a
ranking: AArch64 carries 138 and was, until 5b187e4, missing two opcodes i386
has. The ten i386 implements that the four 92s do not are the seven float ops,
`:fn-addr`, `:li-const` and `:bvs` — each blocked on something specific rather
than merely unwritten (floats need the FPU enabled in each boot stub, which
i386 itself does not do; `:li-const` is only ever emitted by the in-image
compiler; `:bvs` is a design question on RISC-V, which has no condition flags).

#### Payload — one generic image, everything else loaded

A back end that computes correctly is not a system. The direction is **not** a
per-feature image per architecture; it is one generic image per architecture
running the real CL, with everything else — SSH, test runners, tools — arriving
as source it LOADs at runtime. `build-generic` is that shape hosted (boot, LOAD
`argv[1]`, exit); `build-x64-cl-repl` is that shape on bare metal, a multiboot
image whose REPL is the actual CL — reader, `eval` = mvm-eval, printer — over
COM1 via `lib/serial-repl.lisp`. It evaluates `(+ 1 2)` to `3`, and it is the
declared replacement for `build-x64-repl` and `build-x64-console-repl`.

Read the state of the old payloads in that light:

- **The legacy `repl-source.lisp` REPL evaluates a bare symbol and nothing
  else** — `(+ 1 2)` echoes and never returns, on x86-64 and AArch64 alike.
  That is the second Lisp rotting, and #204 deletes it rather than repairing
  it. The broken RPi family builds `*repl-source*` too; the second Lisp and the
  broken cell overlap heavily.
- **The baked SSH payload is unproven everywhere** (`GATE-RESULT-run-cells.md`
  marks all six ssh cells CANNOT PROVE). Under this strategy that matters much
  less than it looks: a baked SSH image is the thing being retired, not the
  thing being fixed. SSH becomes source the generic image loads.

So the remaining work is to bring the CL image up on the other seven
architectures, not to port seven payloads. The back-end work was its
prerequisite — the real CL cannot run on 68k until 68k can allocate a cons,
which it could not until 5b187e4.

#### What is and is not gated

The ladder is a gate you can run:

```bash
scripts/arch-ladder-gate.sh                 # every arch, every rung
scripts/arch-ladder-gate.sh riscv64 68k     # just these
```

**It proves it can fail before it claims anything.** The first thing it does is
run one rung with a deliberately wrong expected value and require that to FAIL;
if the control passes it refuses to run the ladder at all. A gate nobody has
seen fail is not evidence — which is the lesson this table was built out of.
"All 9 architectures compile and produce correct output (factorial 3628800) in
QEMU" stood in this file for months, and not one of those images had a serial
output path to print with.

It is not yet wired into CI, and an architecture whose `qemu-system-*` is not
installed is reported as SKIP rather than silently dropped.

#### Hosted Linux ports — the same rungs in seconds, not ninety minutes

Bare metal needs a boot stub, an FPU enable, a collector bring-up and a UART
driver before an image can say anything. Hosted Linux needs none of it: `mmap`
supplies memory, `write(2)` is the console, and a fault is a signal. So a
hosted port reaches a working image far sooner, and — because it has a console —
the same fourteen rungs can be graded by reading four bytes off stdout instead
of booting a machine and reading guest memory over QMP.

| Port | ELF | Build | Run | Hosted ladder |
|------|-----|-------|-----|:-------------:|
| Linux/RV64  | ELF64-LE, EM_RISCV | `mvm/build-riscv-linux.lisp`   | `qemu-riscv64-static` | 14/14 |
| Linux/RV32  | ELF32-LE, EM_RISCV | `mvm/build-riscv32-linux.lisp` | `qemu-riscv32-static` | 14/14 |
| Linux/ARM32 | ELF32-LE, EM_ARM   | `mvm/build-arm32-linux.lisp`   | `qemu-arm-static`     | not yet run |

```bash
scripts/hosted-ladder.py riscv32            # one port, all rungs
scripts/hosted-ladder.py --all              # every hosted port with a source build
```

It carries the full-system harness's two rules unchanged: the expected value is
mandatory, and one rung runs first with a deliberately wrong expectation and
must FAIL. That is not ceremony — `r13-sap` on RV32 returned a clean, plausible
**0**, because a 4-byte read past a granule boundary landed on zeroed heap. A
harness that accepted "the image printed something" would have called it a pass.

**RV32 is the embedded RISC-V** — GD32VF103, ESP32-C3, CH32V, the SiFive E
cores — and it shares the whole back end with RV64 on the PPC dual-width model:
one translator, `*riscv-64-bit*` set by the installer, everything width-
dependent derived from it. Three things are not merely narrower there, and each
was a bug before it was a comment: `SLLI`'s shift-amount field is **five** bits,
so the RV64 mask-to-24-bits distance of 40 sets bit 25 (funct7) and is a
reserved instruction rather than a shift; `AMOSWAP.D` does not exist, because
funct3 encodes the access width; and `#x80000000` is an ordinary RV32 value that
nonetheless falls outside signed-32, so the 64-bit immediate chain would run and
build a value the register cannot hold.

One hosted trap worth knowing before it costs an afternoon: **`qemu-*-static`
rejects a non-executable file silently, with exit 1 and nothing on stderr** —
indistinguishable from "file not found". The builds write mode 644, so a first
run looks like a broken image. Same shape as the i386 `binfmt_misc` trap
documented in CLAUDE.md.

### Cross-architecture fixpoint

The fixpoint proof verifies that the MVM compiler produces identical output
regardless of which architecture runs it. The chain
(`scripts/run-fixpoint-i386.sh`) runs 8 QEMU steps across **three**
architectures — x64, AArch64 and i386 — verifying SHA256 equalities, and
includes i386 self-hosting (i386 compiles itself) plus cross-compilation among
those three.

Two corrections to what this section used to say. **ARM32 is not in that
chain**: it appears only in `scripts/run-fixpoint-ssh.sh`, which drives real
boards over SSH and does not run here. And there is no "SSH over the fixpoint
chain" — `run-fixpoint-i386.sh` extracts each generation over QMP `pmemsave`,
never over the network.

The chain does not currently complete from a clean checkout: step 0
(`mvm/build-fixpoint.lisp`) heap-exhausts under SBCL's default dynamic space.
`build.lisp` lists the `multi/fixpoint` cell as BROKEN for this reason.

## Building and running

Requires SBCL and QEMU.

### Quick test (any architecture)

All SSH scripts support batch mode — pass an expression as the second argument:

```bash
./scripts/run-x64-ssh.sh 2222 "(+ 1 2)"       # → = 3
./scripts/run-aarch64-ssh.sh 2222 "(+ 1 2)"    # → = 3
./scripts/run-i386-ssh.sh 2222 "(+ 1 2)"       # → = 3
./scripts/run-arm32-ssh.sh 2222 "(+ 1 2)"      # → = 3
```

### x86-64

```bash
./scripts/run.sh x64-repl      # boot to serial REPL
./scripts/run-x64-ssh.sh       # boot with SSH networking
```

### AArch64 (QEMU virt)

```bash
./scripts/run.sh aarch64-repl  # AArch64 REPL on QEMU virt
./scripts/run-aarch64-ssh.sh   # AArch64 with SSH networking (E1000, port 2222)
```

### i386 (QEMU)

```bash
./scripts/run.sh i386-repl     # 32-bit x86 REPL on QEMU
./scripts/run-i386-ssh.sh      # i386 SSH over NE2000 NIC (port 2222)
```

### ARM32 (QEMU raspi2b)

```bash
./scripts/run-arm32-ssh.sh     # ARM32 SSH over DWC2 USB CDC (port 2222)
./scripts/run.sh arm32-repl    # ARM32 serial REPL
```

### Raspberry Pi (QEMU raspi3b)

```bash
./scripts/run-rpi-repl.sh      # AArch64 REPL on emulated raspi3b
./scripts/run-rpi-ssh.sh       # SSH over USB CDC Ethernet (port 2222)
./scripts/run-rpi-hid.sh       # USB keyboard-driven REPL
```

### UEFI x86-64 (real hardware)

```bash
sbcl --script mvm/build-uefi-repl.lisp   # build UEFI EFI application
./scripts/run-uefi-repl.sh               # test in QEMU with OVMF
./scripts/run-uefi-repl.sh "(+ 1 2)"     # eval expression
./scripts/make-uefi-usb.sh               # create bootable USB image
sudo dd if=/tmp/modus-usb.img of=/dev/sdX bs=1M  # write to USB stick
```

### ThinkPad T420 (real hardware, i386)

Boots from USB mass storage via Pi Zero 2W gadget. VGA console + PS/2 keyboard REPL, E1000 82579LM SSH over direct Ethernet to RPi5.

```bash
sbcl --script mvm/build-i386-diag-ssh.lisp
scp /tmp/modus-i386-diag-ssh.img modus@modulator:/home/modus/modus.img
ssh -J modus@modus-pi test@10.0.2.15 "(+ 1 2)"   # → = 3
```

### Raspberry Pi Zero 2 W (real hardware)

The Pi Zero 2 W boots via USB (no SD card needed) using rpiboot, with SSH over USB CDC-ECM Ethernet.

```bash
# One-time: program USB boot OTP fuse (irreversible, requires SD card)
bash scripts/fuse-pizero2w.sh

# Build and deploy
sbcl --script mvm/build-pizero2w-actors.lisp   # actor-based SSH + HTTP
bash scripts/boot-pizero2w.sh                   # full workflow
```

### Self-hosted x86-64

SBCL cross-compiles Gen0 via the MVM pipeline (Source → MVM bytecode → x86-64 native). Gen0 boots in QEMU, runs `(build-image)` to natively compile Gen1 from its own embedded source (~558KB), and Gen1 is extracted via QMP. Gen1 can repeat the process indefinitely.

```bash
./scripts/run-x64-gen1-ssh.sh          # Gen0 → Gen1, boot Gen1 with SSH
```

Connect to any SSH server: `ssh -p 2222 -o StrictHostKeyChecking=no test@localhost`

## Project structure

```
mvm/                   Modus Virtual Machine
  mvm.lisp             ISA definition (~50 opcodes, encoding/decoding)
  compiler.lisp        3-phase compiler (Source → IR → MVM bytecode)
  prelude.lisp         MVM-compilable CL library (hash tables, sort, mapcar)
  translate-*.lisp     Native code translators (9 architectures)
  interp.lisp          MVM interpreter (bootstrapping)
  target.lisp          Architecture descriptors (9 targets)
  cross.lisp           Universal cross-compilation pipeline
  repl-source.lisp     Embedded REPL source for bare-metal builds
  build-*.lisp         Build scripts for all platforms
boot/                  Per-architecture boot sequences (9 targets + UEFI)
net/                   Networking, crypto, SSH, USB, actors
  ip.lisp              ARP, IP, UDP, TCP, DHCP, DNS, ICMP
  crypto.lisp          SHA-256/512, ChaCha20, Poly1305, X25519, Ed25519
  ssh.lisp             SSH-2 server (key exchange, auth, channels)
  e1000.lisp           Intel E1000 NIC driver
  ne2000.lisp          NE2000 ISA NIC driver
  dwc2.lisp            DWC2 USB host controller (RPi 3B QEMU)
  dwc2-device.lisp     DWC2 USB gadget + CDC-ECM (Pi Zero 2 W)
  usb.lisp             USB enumeration + hub support
  actors.lisp          Actor system (spawn, yield, send, receive)
  i386-ehci.lisp       EHCI USB host controller (T420)
  i386-e1000-raw.lisp  E1000 82579LM driver (T420)
  i386-console.lisp    VGA console + PS/2 keyboard (T420)
  http.lisp            HTTP/1.0 server
  http-client.lisp     HTTP client with URL parsing
  hid.lisp             USB HID keyboard/mouse/tablet
  uefi-console.lisp    UEFI GOP framebuffer + PS/2 keyboard
  bcm2835-periph.lisp  GPIO, SPI, I2C, system timer
  uart-bootloader.lisp UART bootloader for rapid kernel redeploy
cross/                 x64 assembler (used by MVM)
runtime/               Tags, subtags, package definitions
scripts/               Build, run, test, and deployment scripts
tests/                 QEMU integration tests + SSH oracle
docs/                  Design documents
lib/                   Shared utilities (MVM loader, hash functions)
crypto/                CL reference crypto implementations
proto/                 CL reference protocol implementations (TLS 1.3, WebSocket, Nostr)
```

## Design

### MVM: the Modus Virtual Machine

MVM is a portable register-based bytecode ISA (~50 opcodes) that decouples the Lisp compiler from target architectures. The compiler is a 3-phase pipeline: Source → IR (virtual register operations) → MVM bytecode. Thin per-architecture translators (~1300-1900 lines each) convert MVM bytecode to native code.

### Tagged objects

4-bit tag scheme with 63-bit fixnums (64-bit architectures) or 30-bit fixnums (32-bit). Large enough for native multiply, which makes 256-bit elliptic curve cryptography practical without bignums. 32-bit targets use pair arithmetic (carry chains) for crypto operations.

### Self-hosting

The kernel carries its own source (~558KB, plain s-expressions with symbol names replaced by integer hashes) and includes a native compiler that rebuilds the entire kernel from it. Each generation copies the source blob into the next, so SBCL is only needed for the initial bootstrap.

### Actors and SMP

Erlang-style actor model with per-actor heaps, so garbage collection never stops the world. Preemption uses reduction counting with LAPIC timer backup. SMP distributes actors across cores with per-CPU run queues and IPI wakeup.

See `docs/` for detailed design documents.

## License

MIT.
