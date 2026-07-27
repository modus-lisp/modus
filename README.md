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

All 9 architectures compile and produce correct output (factorial 3628800) in QEMU. x86-64, AArch64, i386, and ARM32 have full runtime support with SSH.

| Architecture | Bits | Endian | Translator | Boot | QEMU target | Status |
|-------------|:----:|:------:|:----------:|:----:|-------------|--------|
| x86-64      | 64 | little | translate-x64.lisp    | boot-x64.lisp    | `qemu-system-x86_64`    | Full (REPL, SSH, actors, self-hosting) |
| AArch64     | 64 | little | translate-aarch64.lisp | boot-aarch64.lisp | `qemu-system-aarch64 -M virt` | Full (REPL, SSH, actors) |
| AArch64 RPi | 64 | little | translate-aarch64.lisp | boot-rpi.lisp    | `qemu-system-aarch64 -M raspi3b` | Full (REPL, SSH, USB HID, real hardware) |
| i386        | 32 | little | translate-i386.lisp   | boot-i386.lisp   | `qemu-system-i386`      | Full (REPL, SSH, self-hosting, real hardware) |
| ARM32       | 32 | little | translate-arm32.lisp  | boot-arm32.lisp  | `qemu-system-arm -M raspi2b` | Full (REPL, SSH) |
| RISC-V 64   | 64 | little | translate-riscv.lisp  | boot-riscv.lisp  | `qemu-system-riscv64`   | Serial output |
| PPC64       | 64 | big    | translate-ppc.lisp    | boot-ppc64.lisp  | `qemu-system-ppc64`     | Serial output |
| PPC32       | 32 | big    | translate-ppc.lisp    | boot-ppc32.lisp  | `qemu-system-ppc`       | Serial output |
| 68k         | 32 | big    | translate-68k.lisp    | boot-68k.lisp    | `qemu-system-m68k -M an5206` | Serial output |

### Cross-architecture fixpoint

The fixpoint proof verifies that the MVM compiler produces identical output regardless of which architecture runs it. The chain (`scripts/run-fixpoint-i386.sh`) runs 8 QEMU steps across x64, AArch64, i386, and ARM32 to verify SHA256 equalities — proving byte-identical native code regardless of host architecture. This includes i386 self-hosting (i386 compiles itself), cross-compilation between all four architectures, and SSH over the fixpoint chain.

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
./scripts/run-x64-repl.sh      # boot to serial REPL
./scripts/run-x64-ssh.sh       # boot with SSH networking
```

### AArch64 (QEMU virt)

```bash
./scripts/run-aarch64-repl.sh  # AArch64 REPL on QEMU virt
./scripts/run-aarch64-ssh.sh   # AArch64 with SSH networking (E1000, port 2222)
```

### i386 (QEMU)

```bash
./scripts/run-i386-repl.sh     # 32-bit x86 REPL on QEMU
./scripts/run-i386-ssh.sh      # i386 SSH over NE2000 NIC (port 2222)
```

### ARM32 (QEMU raspi2b)

```bash
./scripts/run-arm32-ssh.sh     # ARM32 SSH over DWC2 USB CDC (port 2222)
./scripts/run-arm32-repl.sh    # ARM32 serial REPL
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
