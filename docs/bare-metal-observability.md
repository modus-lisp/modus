# Bare-metal observability: why we need a gdbstub equivalent

Written 2026-09-04, after a session where **silence was mistaken for evidence
about six times** and the single most useful measurement took four seconds once
the right tool was pointed at it.

## The problem, stated precisely

On bare metal a Modus image that is **computing**, one that is **blocked
reading the UART**, and one that has **faulted into a silent `b .` spin** are
indistinguishable from outside. All three produce:

- no serial output,
- no response to ping (the image is single-threaded; nothing services the NIC
  while it is busy *or* while it is blocked),
- an SSH session that eventually dies at the host's ~970 s TCP retransmission
  timeout regardless.

So the only signal is *absence*, and absence is compatible with every
hypothesis. That is what made this session expensive: a 93-minute board step
could not be classified without re-running it.

## What settled it, and how fast

QEMU's gdbstub. The virt image was run under KVM with `-gdb tcp::1234`, and one
sample answered the question:

```
mov  x17, #0x20000000     ; PL011 UART base
ldur w18, [x17, #24]      ; UARTFR
=> tbnz w18, #4, <back>   ; RXFE set -> spin
ldurb w0, [x17]           ; read data register
```

Four instructions: the guest was **idle, waiting for a character**, not
computing. Twelve PC samples three seconds apart, all identical, plus a
disassembly. Total cost: under a minute.

The equivalent question on the board is still unanswered after hours.

## Recommended workflow TODAY (no new hardware)

1. **Debug on QEMU virt under KVM on the Pi 5**, not on the board. It is real
   bare metal (no Linux, handler-stack helpers absent at runtime, same
   `0x09000000..0x10000000` heap = 56 MB semispaces, same cabinet path) but
   runs at near-native A76 speed and accepts `-gdb tcp::1234`.
   Rig: `/tmp/mql/ql-kvm10.sh` on modus-pi; add `-gdb tcp::1234` to the qemu
   line. Poisoned DRAM is available too (see below) — the board's memory is
   NOT zeroed and QEMU's is.
2. **Reserve the board for final validation**, not for bisection. A board cycle
   is ~90 min; a KVM cycle is ~5 min.
3. **Every long step must emit per-item serial markers** (the untar's `W<n>` is
   the model). Then silence means STUCK and a marker stream means SLOW, with a
   rate you can extrapolate. A step that prints nothing is unobservable by
   construction — that is a harness bug, not a board bug.

Poisoned-DRAM QEMU (models uninitialised hardware RAM; QEMU zero-fills by
default and that HID a real fault class for the life of the virt image):

```
-object memory-backend-file,id=pmem,size=512M,mem-path=/dev/shm/poison512.bin,share=off
-machine virt,memory-backend=pmem
```

Bisect *which* region must be zeroed by zeroing regions of the poison FILE — no
rebuild, no U-Boot `mw.q`. For virt, VA `0x0xxxxxxx` == file offset.

## The gap: real hardware has no gdbstub

The board (Pi Zero 2 W, BCM2710A1, Cortex-A53, running at **EL2**) has no
equivalent.

**PREFER SERIAL. It is the only channel every target already has** — COM1 on
bare x64, PL011 / mini-UART on aarch64, the i386 images too — so a serial-based
answer is written once and works on every board we bring up, including ones
that do not exist yet. GPIO trace pins and JTAG below are Pi-specific escape
hatches: reach for them when serial cannot answer the question (a wedge with
interrupts off, or a fault before our vectors are installed), not as the
default.

Ordered by generality, not just by cost:

| approach | channel | portable? |
|---|---|---|
| per-item markers | serial | yes — do this always |
| timer-IRQ PC sampler | serial readout | yes (timer/IRQ setup is per-arch) |
| in-image gdbstub | serial | yes — the general answer |
| GPIO trace pins | GPIO | Pi only |
| JTAG | SWD/JTAG pins | Pi only, needs wiring |

### Option A — timer-interrupt PC sampler (recommended first)

A periodic interrupt that records the interrupted PC into a ring buffer and
prints it over serial. This is a *sampling profiler*, and crucially it works
when the image is stuck in a tight loop, which is exactly the case that defeats
every cooperative (polled) hook.

Sketch:

- **Timer**: the ARM generic timer per core — `CNTP_TVAL_EL0` to arm,
  `CNTP_CTL_EL0` bit 0 to enable.
- **Routing**: NOT a GICv2 — a BCM2837 has none. Use the ARM *local
  peripherals* block at `0x40000000`; core 0's timer interrupt control is at
  `0x40000040` (route nCNTPNSIRQ to IRQ).
- **Vector**: `boot-rpi-cl.lisp` already installs exception vectors and sets
  **both** `VBAR_EL1` and `VBAR_EL2` — the image runs at EL2, and setting only
  EL1 once produced a 53-million-exception storm. The IRQ entry must read
  `ELR_EL2`.
- **Handler**: append `ELR_EL2` to a fixed ring buffer in the metadata window,
  re-arm `CNTP_TVAL_EL0`, `ERET`. Allocation-free and reentrancy-free.
- **Readout**: a serial dump of the ring, plus `MODUS_SYMMAP=<path>` at build
  time to turn addresses into function names. NOTE: the map's addresses are
  relative to the DECLARED load-addr, so for a chainload image that RUNS at
  `0x300000` while declared at `0x280000`, subtract `0x80000` before lookup —
  and verify the rebuilt image is byte-identical (`cmp`) to the one that
  actually ran, or the map is meaningless.

Cost: real, but bounded, and it pays for itself the first time a board step
goes quiet. It also generalises to every future bare-metal target.

### Option B — a real gdbstub in the image (the GENERAL answer)

Implement the GDB remote serial protocol over the UART: packet framing,
`g`/`m`/`c`/`s`, breakpoints. This is the one that generalises — the same stub
serves bare x64 (COM1), aarch64 (PL011 / mini-UART) and i386, and it gives
exactly the experience that settled the virt question above, on hardware where
QEMU cannot help.

It is larger (packet parser, register marshalling), and it shares the UART with
the console. Do NOT require a second UART — multiplex instead: a magic escape
sequence on the console switches the port into gdb-stub mode and `D`/detach
switches it back, which is standard practice for embedded stubs and keeps the
one-cable setup we already have on every target.

### Option C' — GPIO trace pins (cheapest of all, do this alongside A)

The Zero 2 W has plenty of spare GPIO. Toggling a pin costs a single store to
the BCM2835 GPIO SET/CLR registers — no allocation, no UART, no interrupt, and
it works while the serial line is busy or owned by `ssh-boot`. Assign a pin per
phase (compile / GC / cabinet write / JIT translate), raise it on entry and drop
it on exit, and the duty cycle IS the profile. Read it with modus-pi's own GPIO
(or any logic analyser). `net/bcm2835-periph.lisp` already has the GPIO
primitives.

This is strictly weaker than a PC sampler — it only shows phases you thought to
instrument — but it is minutes of work and it answers "is it even alive" and
"which phase is it in" without touching the console.

### Option C — JTAG

The Pi Zero 2 W exposes JTAG on GPIO alt functions (GPIO22-27, ALT4: TRST /
RTCK / TDO / TCK / TDI / TMS) — **and we already own the probe**. OpenOCD's
`linuxgpiod` (or legacy `bcm2835gpio`) adapter driver lets **modus-pi drive the
JTAG lines straight from its own GPIO header**, so this needs jumper wires, not
an FT2232 purchase. The two boards are already cabled together for serial and
netboot, so the incremental cost is ~6 wires and an OpenOCD config. This is the only option
that can debug the boot path *before* any of our own code runs, and the only
one that survives a total wedge. It is also the only one requiring hardware we
do not have wired up. Reach for it when a fault happens before the vectors are
installed, or when Option A shows nothing because the core is not executing our
code at all.

## Standing rule

**Never infer state from silence.** If a step can go quiet, give it a marker
before running it, or run it somewhere with a gdbstub. Every wrong conclusion
in the 2026-09-04 session came from reading absence as information.
