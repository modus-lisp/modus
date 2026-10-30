# Modus

**Website: [modus-lisp.github.io](https://modus-lisp.github.io)**

A self-hosting Common Lisp that boots on bare metal — directly on hardware or in
QEMU, into a Lisp REPL with networking, SSH and cryptography and no OS
underneath — and runs hosted on Linux as an SBCL-compatible `modus` command.
Its compiler targets a portable virtual machine (MVM) with native translators
for nine CPU architectures.

## Start here: kiln

**[kiln](https://github.com/modus-lisp/kiln) is the way in to modus-lisp.** It
fetches the workspace — this repo and the rest of the org — builds it, and runs it:

```sh
git clone https://github.com/modus-lisp/kiln && cd kiln
bin/kiln build           # set this machine up: the workspace, and a core
bin/kiln local --view    # the desktop, in a window here
bin/kiln sync            # every org repo as a live checkout beside kiln, kept current
```

Or in a container: `bin/kiln container` once, then `bin/kiln vnc` for the desktop
and `bin/kiln modus` for this Lisp, hosted — a REPL, or `--script`. And as an
attestable bare-metal image: `bin/kiln image x64-uefi`. kiln pins every repo to an
exact commit in its `repos.lock`.

This repository is Modus itself — the compiler, the runtime and the ports. Work
here directly when you are changing those; [Building and running](#building-and-running)
below covers that.

## What Modus is now

A Common Lisp that compiles itself, runs hosted on Linux under six CPU
architectures and on bare metal on four, and is checked against SBCL byte for
byte.

- **ANSI Common Lisp: the whole upstream suite.** The pinned
  [ansi-test](https://gitlab.common-lisp.net/ansi-test/ansi-test) suite passes
  **21,681 of 21,681** on all six hosted CLIs (x86-64, AArch64, i386, ARM32,
  RV32, RV64), and every merge is gated on the x86-64 run: CLOS with method
  combination, the condition system and restarts, the pretty printer,
  pathnames, `compile-file` with real FASLs, and the full numeric tower, IEEE
  floats included.
- **An SBCL drop-in, as far as libraries can tell.**  `./modus` takes SBCL's
  flags (`--eval`, `--load`, `--script`, `~/.modusrc`), ASDF is built in, and
  `SB-EXT`, `SB-THREAD`, `SB-BSD-SOCKETS`, `SB-POSIX` and `SB-SYS` are there
  for the code that asks for them.  Unmodified Quicklisp installs and loads
  libraries, including on bare metal.
- **Packages like SBCL's.**  Internals live in `MODUS-INT` (SB-INT's role),
  the public API in `MODUS` (SB-EXT's), the runtime in `MODUS.RUNTIME`, and the
  network stack, drivers, crypto, SSH and board adapters in packages of their
  own.  A script's `(defun print-dec ...)` or `(defvar *tar-block-size* ...)`
  makes its own function or variable instead of replacing the kernel's;
  `modus-int::print-dec` reaches the kernel's on purpose.
- **Native code at runtime.**  `eval` and `load` compile to MVM bytecode, and a
  hot-function JIT turns it into native x86-64 or AArch64.  Redefining a
  function reaches every caller already compiled against it, the kernel's own
  included.
- **Threads.**  Hosted x86-64 has native OS threads, each with its own GC
  region, dynamic bindings and handler stack: the [glass](https://github.com/modus-lisp/glass)
  compositor serves VNC clients from its own threads, unmodified.  On bare
  metal the Pi and QEMU virt images run threads on all four cores.
- **Garbage collection.**  A Cheney collector in every CL image and every
  hosted port (hosted 68k and PowerPC size their heap at startup), per-region
  collection for threads, and a generational collector with a card-marking
  write barrier on AArch64 and hosted x86-64.
- **Self-hosting, checked against SBCL.**  The self-hosted compiler rebuilds
  the UEFI x86-64 image byte-for-byte as SBCL builds it, and emits the same
  Linux/AArch64 image from the same source; the acceptance gate runs both
  checks on every merge.  The bootstrap also runs under CCL and ABCL.
- **Attested.**  Modus runs as an AWS Nitro enclave and as an AMD SEV-SNP guest,
  serving SSH whose host key is bound into the attestation document
  (docs/nitro-enclaves.md, docs/vpsbg-snp-status.md).
- **Everywhere else.**  A saved core boots in seconds (`save-and-die`,
  docs/save-and-die.md); `web/mvm.js` runs Modus in a browser; the 68k image is
  the first big-endian full CL, and 68k and i386 build soft-float images for
  FPU-less CPUs.

## Platforms

### Hosted (Linux)

| CLI | Build | ANSI suite |
|-----|-------|:----------:|
| x86-64  | `mvm/build-generic-cli.lisp` (JIT, threads, sockets, SSH) | 21,681 / 21,681 (gated) |
| AArch64 | `mvm/build-aarch64-cli.lisp` (JIT)                         | 21,681 / 21,681 |
| i386    | `mvm/build-i386-cli.lisp` (SSE2, x87 or soft-float)        | 21,681 / 21,681 |
| ARM32   | `mvm/build-arm32-cli.lisp`                                 | 21,681 / 21,681 |
| RV32 / RV64 | `mvm/build-riscv32-cli.lisp`, `mvm/build-riscv64-cli.lisp` | 21,681 / 21,681 |
| 68k     | `mvm/build-68k-cli.lisp` (FPU or soft-float)               | loads alexandria, cl-ppcre |

The non-x86-64 ports run under `qemu-*-static`; `scripts/allarch-ansi.sh` sweeps
them (one to two hours each, so outside the half-hour gate).

### Bare metal

Every bare-metal image runs the same Common Lisp REPL.

|                  | x86-64 (multiboot, UEFI) | AArch64 QEMU virt | Raspberry Pi 3B / Zero 2 W | i386 | ARM32 QEMU virt |
|------------------|:---:|:---:|:---:|:---:|:---:|
| Serial REPL      | Y | Y | Y | Y | Y |
| Screen + keyboard | VGA / GOP + PS/2 | - | HDMI + USB | - | - |
| Network          | E1000, virtio | E1000 | USB CDC-ECM, RTL8152/8153 | NE2000 | - |
| SSH server       | Y | Y | Y | Y | - |
| Threads          | - | 4 cores | 4 cores | - | - |
| Isolated SSH (session per thread) | - | Y | Y | - | - |
| Generational GC  | - | - | Y | - | - |
| Saved cores      | Y | - | Y | - | - |

The Pi Zero 2 W image is the one [kiln](https://github.com/modus-lisp/kiln)
deploys to a board (`kiln zero`): netboot, SSH, a desktop on HDMI.  The UEFI image
boots real PCs from a USB stick and is the one the SEV-SNP guest measures.

### MVM architectures

**Eight of the nine run compiled Lisp in QEMU, verified by the ladder below.**
The ninth, AArch64 RPi, shares AArch64's translator and runs the full CL image
on real boards; it is simply not a ladder target.  The ladder measures the code
generator, not the system: the platform tables above say which images run the
full Common Lisp.

#### Back end — measured

Every architecture below boots in QEMU, runs compiled Lisp, and returns the
right answer for a 14-rung ladder: a call, add/multiply, a branch, an argument,
recursion, `(factorial 10)`, `cons`/`car`/`cdr`, a loop with an early return,
three live variables, arrays, byte vectors, strings and SAPs. The answer is
read back out of guest memory over QMP and compared to an expected value —
"the image wrote something" is not a pass. **120/120** — eight architectures
x fifteen rungs, with the control answering WRONG first.

| Architecture | Bits | Endian | Translator | QEMU target | Opcodes | Ladder |
|-------------|:----:|:------:|:----------:|-------------|:-------:|:------:|
| x86-64      | 64 | little | translate-x64.lisp     | `qemu-system-x86_64`                  | 137/144 | 15/15 |
| AArch64     | 64 | little | translate-aarch64.lisp | `qemu-system-aarch64 -M virt`         | 138/144 | 15/15 |
| i386        | 32 | little | translate-i386.lisp    | `qemu-system-i386`                    | 102/144 | 15/15 |
| ARM32       | 32 | little | translate-arm32.lisp   | `qemu-system-arm -M raspi2b`          |  92/144 | 15/15 |
| RISC-V 64   | 64 | little | translate-riscv.lisp   | `qemu-system-riscv64 -M virt`         |  92/144 | 15/15 |
| RISC-V 32   | 32 | little | translate-riscv.lisp   | hosted only so far (see below)        | (shared) | 15/15 hosted |
| PPC64       | 64 | big    | translate-ppc.lisp     | `qemu-system-ppc64 -M powernv`        |  92/144 | 15/15 |
| PPC32       | 32 | big    | translate-ppc.lisp     | `qemu-system-ppc -M ppce500`          |  92/144 | 15/15 |
| 68k         | 32 | big    | translate-68k.lisp     | `qemu-system-m68k -M virt`            |  92/144 | 15/15 |
| AArch64 RPi | 64 | little | translate-aarch64.lisp | `qemu-system-aarch64 -M raspi3b`      | (shared) | not in the ladder |

The opcode counts are the honest measure of how much of the ISA each back end
covers, but they are a poor proxy for capability and should not be read as a
ranking: AArch64 carries 138 and was, until 5b187e4, missing two opcodes i386
has. The ten i386 implements that the four 92s do not are the seven float ops,
`:fn-addr`, `:li-const` and `:bvs` — each blocked on something specific rather
than merely unwritten (floats need the FPU enabled in each boot stub, which
i386 itself does not do; `:li-const` is only ever emitted by the in-image
compiler; `:bvs` is a design question on RISC-V, which has no condition flags).

#### Payload — one image per platform, everything else loaded

The direction held: one Common Lisp image per platform, and everything else
arriving as source it loads.  x86-64, AArch64, i386 and ARM32 run the full CL
on bare metal, and six architectures run it hosted; SSH, Quicklisp and the
board's desktop are code the image loads or bakes from the same sources.  The
mini-Lisp that the early images ran is retired (docs/mini-lisp-archive.md,
tag `mini-lisp-final`).  RISC-V, PowerPC and 68k bare metal pass the ladder but
have no CL image yet.

#### What is and is not gated

The ladder is a gate you can run:

```bash
scripts/arch-ladder-gate.sh                 # every arch, every rung
scripts/arch-ladder-gate.sh riscv64 68k     # just these
```

**It proves it can fail before it claims anything.** The first thing it does is
run one rung with a deliberately wrong expected value and require the harness to
report that the image *built, ran, and answered wrong* — a distinct exit code, so
a control that "fails" merely because nothing compiled is rejected rather than
believed. (That is not a hypothetical: a broken build once printed "the gate can
fail" above fourteen BUILD-FAILs.) If the control passes, or cannot answer at
all, it refuses to run the ladder. A gate nobody has
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
| Linux/RV64  | ELF64-LE, EM_RISCV | `mvm/build-riscv-linux.lisp`   | `qemu-riscv64-static` | 15/15 |
| Linux/RV32  | ELF32-LE, EM_RISCV | `mvm/build-riscv32-linux.lisp` | `qemu-riscv32-static` | 15/15 |
| Linux/ARM32 | ELF32-LE, EM_ARM   | `mvm/build-arm32-linux.lisp`   | `qemu-arm-static`     | 15/15 |
| Linux/PPC64 | ELF64-**BE**, EM_PPC64 | `mvm/build-ppc64-linux.lisp` | `qemu-ppc64-static`  | 15/15 |
| Linux/PPC32 | ELF32-**BE**, EM_PPC   | `mvm/build-ppc32-linux.lisp` | `qemu-ppc-static`    | 15/15 |
| Linux/m68k  | ELF32-**BE**, EM_68K   | `mvm/build-68k-linux.lisp`   | `qemu-m68k-static`   | 15/15 |

```bash
scripts/hosted-ladder.py riscv32            # one port, all rungs
scripts/hosted-ladder.py --all              # every hosted port with a source build
HOSTED_LADDER_JOBS=12 scripts/hosted-ladder.py 68k
```

Cells run in PARALLEL — each has its own temp directory, output path and emulator
process, so the only thing that was ever sequential about this was the loop.  Six
ports x fifteen rungs finishes in minutes; the full-system gate needs about ninety.

**THE BIG-ENDIAN PORTS NEEDED NO NEW ELF WRITER.**  `mvm/cross.lisp` already had
`wrap-in-elf32-be` and `wrap-in-elf64-be` for the bare-metal images, reached by
the GENERIC arm of the wrapper dispatch — so what these ports needed was the
right descriptor, not new code.  Their syscall ABIs are the new part: PowerPC
signals an error in CR0.SO rather than by negating the return, so the "negative
means errno" test every other port here uses cannot work; and m68k passes
arguments in `d1,d2,d3,d4,d5` and then **`a0`**, the only mixed argument bank in
the tree.

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

### Self-hosting checks

The self-hosted compiler is checked against SBCL on every merge
(`scripts/acceptance-gate.sh`, step 4):

- `test/run-uefi-ddc.sh` — SBCL builds the UEFI x86-64 CL image and dumps the
  source it compiled; `modus-sh` (the self-hosting image) compiles that source
  in-image, twice.  All three images must be byte-identical.
- `test/run-aarch64-ddc.sh` — the same source, compiled to Linux/AArch64 by SBCL
  and by `modus-sh --compile-aarch64`, must be byte-identical.

`scripts/run-fixpoint-hosts.sh` builds `modus-sh` under SBCL, CCL and ABCL and
requires the programs each compiles to be byte-identical: the host Lisp washes
out once Modus is compiling.

## Building and running

Requires SBCL and QEMU.

### The hosted CLI

```bash
sbcl --dynamic-space-size 12288 --script mvm/build-generic-cli.lisp   # -> /tmp/modus
/tmp/modus --eval '(print (expt 2 100))' --quit
/tmp/modus --script my-program.lisp
scripts/ansi-sweep.sh /tmp/modus /path/to/outdir                    # the upstream suite
```

### Quick test (QEMU)

`./scripts/run.sh list` shows every QEMU configuration.  Each builds its image if
any source is newer, boots it and evaluates one expression, at a serial REPL or
over SSH:

```bash
./scripts/run.sh x64-cl-repl "(+ 1 2)"     # x86-64 multiboot, serial REPL
./scripts/run.sh x64-ssh "(+ 1 2)"         # ... with SSH over E1000      → = 3
./scripts/run.sh i386-cl-repl "(+ 1 2)"    # 32-bit x86
./scripts/run.sh i386-ssh "(+ 1 2)"        # ... with SSH over NE2000     → = 3
./scripts/run.sh aarch64-cl-repl "(+ 1 2)" # AArch64, QEMU virt
./scripts/run.sh aarch64-ssh "(+ 1 2)"     # ... with SSH over E1000      → = 3
./scripts/run.sh arm32-cl-repl "(+ 1 2)"   # ARMv7, QEMU virt
./scripts/run.sh rpi-cl-repl "(+ 1 2)"     # Raspberry Pi 3B (QEMU raspi3b)
```

Every one of these is the same Common Lisp — reader, `eval`, printer, CLOS,
conditions.  The old per-image mini-Lisp is gone (docs/mini-lisp-archive.md).

### UEFI x86-64 (real hardware)

```bash
sbcl --dynamic-space-size 12288 --script mvm/build-uefi-cl-repl.lisp
./scripts/run-uefi-repl.sh "(+ 1 2)"     # test in QEMU with OVMF
./scripts/make-uefi-usb.sh               # create bootable USB image
sudo dd if=/tmp/modus-usb.img of=/dev/sdX bs=1M  # write to USB stick
```

### Raspberry Pi boards (real hardware)

`mvm/build-rpi-cl-repl.lisp` builds the board image: U-Boot chainload, a USB
NIC, HDMI + USB-keyboard console, SSH, threads on all four cores.  See CLAUDE.md
"QEMU RPi 3B / real Pi boards" and docs/quickload-on-rpi.md.

Connect to any SSH server: `ssh -p 2222 -o StrictHostKeyChecking=no test@localhost`

## Project structure

```
mvm/                   The compiler, runtime and build scripts
  compiler.lisp        Source -> IR -> MVM bytecode (runs on the host and in the image)
  translate-*.lisp     Native translators: x64, aarch64, i386, arm32, riscv, ppc, 68k
  x64-asm.lisp         x86-64 assembler
  cross.lisp           Image assembly for every target
  prelude.lisp, cl-*.lisp   The Common Lisp runtime (package MODUS.RUNTIME)
  mvm-eval.lisp        eval/load: compile to bytecode, interpret or JIT
  gc.lisp              Collector support shared by the ports
  kernel-packages.lisp The kernel's packages and what each exports
  build-*.lisp         Build scripts; build-checks.lisp the checks every build runs
boot/                  Boot stubs: bare metal per architecture, Linux per architecture
net/                   Networking, drivers, crypto, SSH, threads, boards
  ip.lisp, tcp-conns.lisp   ARP, IP, TCP, UDP, DHCP, DNS
  nic.lisp             The NIC interface the drivers implement
  e1000.lisp, ne2000.lisp, virtio-net.lisp, dwc2.lisp, usb.lisp, cdc-ether.lisp, r8152.lisp
  crypto*.lisp         SHA-256/512, ChaCha20, Poly1305, X25519, Ed25519
  ssh.lisp             SSH-2 server; hosted-ssh.lisp, isolated-cl.lisp its other shapes
  arch-*-cl.lisp       Board adapters (package MODUS.BOARD)
  bare-threads*.lisp   Threads on bare metal;  hosted-*.lisp: threads, sockets on Linux
  pc-console.lisp, hdmi-console.lisp   Screen and keyboard consoles
lib/                   Loader, hashing, save-and-die, ASDF/Quicklisp support
runtime/               Tags and subtags
web/                   The browser build (mvm.js)
test/                  Tests: test/*.lisp run under ./modus; ladders, DDCs, harnesses
scripts/               run.sh, the acceptance gate, build matrix, ladders, deployment
docs/                  Design notes and campaign write-ups
crypto/, proto/        Reference implementations (crypto, TLS 1.3, WebSocket, Nostr)
```

## Design

### MVM: the Modus Virtual Machine

MVM is a portable register-based bytecode ISA that decouples the Lisp compiler from target architectures. The compiler is a 3-phase pipeline: Source → IR (virtual register operations) → MVM bytecode. A translator per architecture (3,400 to 11,600 lines; x86-64 and AArch64 also translate at runtime, for the JIT) converts MVM bytecode to native code.

### Tagged objects

4-bit tag scheme with 63-bit fixnums (64-bit architectures) or 30-bit fixnums (32-bit). Large enough for native multiply, which makes 256-bit elliptic curve cryptography practical without bignums. 32-bit targets use pair arithmetic (carry chains) for crypto operations.

### Self-hosting

The image carries its compiler, so `eval`, `load` and `compile-file` compile
in-image, and `modus-sh --compile` / `--compile-uefi` / `--compile-aarch64`
build whole images with no SBCL in the loop.  SBCL (or CCL, or ABCL) is needed
only for the first bootstrap.

### Concurrency

Hosted x86-64 runs native OS threads with per-thread GC regions; the bare-metal
Pi and QEMU virt images run threads on every core; and a cooperative actor
scheduler (spawn, send, receive, with per-actor heaps and copied messages) runs
on top of either.

See `docs/` for detailed design documents.

## License

MIT.
