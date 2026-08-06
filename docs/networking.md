# Networking, USB, and I/O Architecture

## Overview

Modus implements a full networking stack — from NIC drivers through IP/TCP to SSH and HTTP — running on bare metal across 6 CPU architectures. The design principle is **shared protocol code with thin architecture adapters**: the ~12,000 lines of ip.lisp, crypto.lisp, and ssh.lisp run identically on x64, AArch64, i386, ARM32, RPi 3B, and Pi Zero 2W. Each platform provides a ~200-line adapter defining hardware addresses, PCI access, and I/O primitives.

Three NIC driver classes cover all platforms: Intel E1000 (PCI Gigabit Ethernet), NE2000 (ISA 10Mbps), and DWC2 USB (host and gadget modes with CDC-ECM). All expose the same `e1000-send`/`e1000-receive`/`e1000-rx-buf` API so ip.lisp doesn't know which hardware is underneath.

---

## Platform Matrix

| Platform | Machine | NIC | Driver | Transport | Speed |
|---|---|---|---|---|---|
| x64 | QEMU virt | E1000 | PCI I/O port | Ethernet | 1 Gbps |
| AArch64 | QEMU virt | E1000 | PCI ECAM MMIO | Ethernet | 1 Gbps |
| i386 | QEMU i386 | NE2000 | ISA port I/O | Ethernet | 10 Mbps |
| RPi 3B | QEMU raspi3b | DWC2 host | USB CDC-ECM | USB Full Speed | 12 Mbps |
| ARM32 | QEMU raspi2b | DWC2 host | USB CDC-ECM | USB Full Speed | 12 Mbps |
| Pi Zero 2W | Real hardware | DWC2 gadget | USB CDC-ECM | USB Full Speed | 12 Mbps |

### Build variants

| Build | Script | Features |
|---|---|---|
| AArch64 SSH | `build.lisp aarch64/bare/qemu/ssh` | Single-threaded SSH |
| AArch64 actors | `build.lisp aarch64/bare/qemu/actors` | Multi-connection SSH + HTTP, cooperative actors |
| AArch64 isolated | `build.lisp aarch64/bare/qemu/isolated` | Qubes-like isolation, net-domain owns hardware |
| RPi 3B SSH | `build-rpi-ssh.lisp` | Single-threaded SSH over USB |
| RPi 3B HID | `build-rpi-hid.lisp` | USB keyboard/mouse/tablet (no networking) |
| i386 SSH | `build.lisp i386/bare/qemu/ssh` | Single-threaded SSH, 32-bit crypto |
| ARM32 SSH | `build-arm32-ssh.lisp` | Single-threaded SSH, 32-bit crypto, USB |
| Pi Zero 2W SSH | `build-pizero2w-ssh.lisp` | Single-threaded SSH, USB gadget |
| Pi Zero 2W actors | `build-pizero2w-actors.lisp` | Multi-connection SSH, USB gadget |
| Fixpoint | `build-fixpoint.lisp` | Multi-arch SSH for cross-compilation chain |

---

## NIC Drivers

### E1000 — Intel Gigabit PCI (`net/e1000.lisp`, 264 lines)

Used by AArch64 and x64 builds on QEMU virt. Accesses the NIC through PCI configuration space (BAR0 MMIO) and descriptor rings in host memory.

**Initialization** (`e1000-probe`):
1. Scan PCI bus 0 for device 8086:100E (E1000)
2. Enable bus mastering (PCI command register bit 2)
3. Read BAR0 → MMIO base address
4. Software reset (CTRL.RST), wait for completion
5. Read MAC from EEPROM (3 × 16-bit words)
6. Initialize RX ring: 128 descriptors, 2048-byte buffers
7. Initialize TX ring: 64 descriptors, 1536-byte buffers
8. Enable RX (RCTL) and TX (TCTL), set link up

**Send path**: Write packet to current TX buffer, set descriptor length + EOP + IFCS flags, advance tail pointer.

**Receive path**: Check current RX descriptor DD (descriptor done) bit. If set, return packet length. Advance head pointer.

**PCI access**: Architecture-specific. x64 uses I/O ports 0xCF8/0xCFC. AArch64 uses ECAM MMIO at 0x4010000000.

### NE2000 — ISA 10Mbps (`net/ne2000.lisp`, 315 lines)

Used by i386 builds. The NE2000 is a 16-bit ISA NIC accessed entirely through I/O port reads/writes at a fixed base address (0x300). No PCI, no MMIO, no DMA.

**Key design constraint**: The MVM compiler can only emit I/O instructions with compile-time constant port numbers. The `ne2k-read`/`ne2k-write` functions use an if-chain dispatching on register number to emit the correct port constant.

**NIC-side memory**: 16KB SRAM organized as 256-byte pages. TX uses pages 0x40–0x45 (1536 bytes). RX uses pages 0x46–0xBF (6656 bytes ring buffer).

**Host-side buffers**: TX at 0x780000, RX at 0x781000. Data is transferred between NIC SRAM and host memory via Remote DMA (RDMA) — port I/O word-at-a-time, not bus mastering.

**MAC**: Read from NE2000 PROM via remote DMA at init (`ne2k-dma-read` of PROM address 0x0000). Bytes are duplicated in PROM, so every even offset is read.

### DWC2 Host — USB 2.0 OTG (`net/dwc2.lisp`, 568 lines)

Used by RPi 3B and ARM32 builds. The DesignWare Core USB 2.0 OTG controller (at 0x3F980000 on BCM2836/2837) operates in host mode, controlling an external USB CDC Ethernet device.

**Channel architecture**: 16 host channels, each with HCCHAR (characteristics), HCTSIZ (transfer size), HCDMA (DMA address), and HCINT (interrupt status) registers. Channels are assigned to specific transfer types:
- Channel 0: Control (SETUP/DATA/STATUS)
- Channel 1: Bulk IN (device → host, Ethernet RX)
- Channel 2: Bulk OUT (host → device, Ethernet TX)

**Critical QEMU quirk**: Never halt bulk IN channels. QEMU's DWC2 emulation corrupts internal state when channels are halted (`needs_service` persists, `work_bh` services stale transfers). Use `dwc2-poll-bulk-in` (non-blocking single HCINT check) instead of the blocking `dwc2-poll-channel`. DWC2's `work_bh` auto-retries NAK internally.

**Data toggle**: USB bulk transfers alternate DATA0/DATA1 PIDs. The driver manually tracks toggles in USB state memory (per-endpoint). HCTSIZ PID field must match the expected toggle.

**HCTSIZ race**: In `e1000-receive`, read HCTSIZ BEFORE calling `usb-bulk-receive` — the poll may start a new transfer that overwrites the register value.

### DWC2 Gadget — USB Device Mode (`net/dwc2-device.lisp`, 1215 lines)

Used by Pi Zero 2W builds. Same DWC2 controller but configured as a USB device (gadget). The Pi appears as a USB Ethernet adapter to the host computer.

**Endpoints**: EP0 (control, bidirectional), EP1 IN (bulk, device → host), EP2 OUT (bulk, host → device).

**FIFO mode**: PIO (programmed I/O), not DMA. Data is written to/read from the FIFO register word-by-word. This avoids DMA buffer alignment issues but limits throughput.

**CDC-ECM**: Static IP configuration. Pi = 10.0.0.2, host = 10.0.0.1, MAC = 02:00:00:00:00:01.

**ISR**: A hand-assembled AArch64 ISR in `boot/boot-rpi.lisp` handles DWC2 interrupts. It drains the RX FIFO into a 4-slot ring buffer at `usb-ring-base` (0x01090000), and sets deferred flags for USB reset, enumeration done, and SETUP packets.

### USB Core (`net/usb.lisp`, 416 lines)

USB enumeration and control transfer state machine shared by all DWC2 host builds.

**Enumeration sequence**:
1. Port reset → detect device speed (FS/LS/HS)
2. GET_DESCRIPTOR (device) at address 0 → read max packet size
3. SET_ADDRESS → assign device address 1
4. GET_DESCRIPTOR (device) at address 1 → read VID/PID
5. GET_DESCRIPTOR (config) → parse for bulk IN/OUT endpoints
6. SET_CONFIGURATION → activate config
7. SET_INTERFACE → select CDC data interface (best-effort, QEMU may STALL)

**Hub support**: If VID/PID matches a hub (0409:55AA), enumerate downstream ports recursively. QEMU's raspi3b includes a virtual root hub.

### CDC Ethernet (`net/cdc-ether.lisp`, 241 lines)

Bridges USB bulk transfers to the `e1000-send`/`e1000-receive` API expected by ip.lisp.

**`cdc-ether-init`**: Calls `dwc2-init`, runs USB enumeration, reads MAC address from the CDC Ethernet Networking Functional Descriptor (string index → UTF-16LE hex string), initializes network state.

**QEMU usb-net note**: Config descriptor index 1 must be used (index 0 is RNDIS, which the driver doesn't support).

### USB HID (`net/hid.lisp`, 703 lines)

Boot protocol HID for keyboard, mouse, and absolute-position tablet. Used by `build-rpi-hid.lisp`.

**Protocol**: Boot protocol mode (SET_PROTOCOL request) — fixed 8-byte keyboard reports and 3-byte mouse reports. No HID report descriptor parsing.

**Channels**: 3 dedicated DWC2 interrupt IN channels (3=kbd, 4=mouse, 5=tablet) polled in a round-robin loop.

**Keyboard**: Scancode-to-ASCII lookup tables (normal + shifted) at HID memory base + 0x400. Ring buffer for keypress events.

---

## Protocol Stack

### IP/TCP/UDP (`net/ip.lisp`, ~1300 lines)

Complete IPv4 stack with ARP, ICMP, TCP, UDP, DHCP client, DNS resolver, and IP fragment reassembly. All protocol state lives in a flat memory region at `e1000-state-base()`.

**Key state offsets** (relative to `e1000-state-base`):
```
+0x00  MMIO base (u64)         +0x08  MAC address (6 bytes)
+0x18  Our IP (4 bytes)        +0x1C  Gateway IP (u32)
+0x28  Gateway MAC (6 bytes)   +0x30  TCP state (u32)
+0x34  TCP local port (u16)    +0x36  TCP remote port (u16)
+0x38  TCP remote IP (u32)     +0x3C  TCP local seq (u32)
+0x40  TCP remote ack (u32)    +0x58  DNS server IP (u32)
+0x5C  Subnet mask (u32)       +0x60  DHCP state (u32)
+0xA00 IP fragment reassembly state
```

**TCP**: Multi-connection support. Connection table (`tcp-conn-table`) tracks up to 8 simultaneous connections, each with independent sequence numbers, ports, and state. 3-way handshake (SYN, SYN-ACK, ACK), data transfer with sequence/ack tracking, passive close (FIN from client). Actor builds additionally give each SSH connection its own actor with isolated TCP state.

**IP fragment reassembly**: Inbound fragments are reassembled with out-of-order support (any fragment can arrive first). Tracks `have-first`, `expected-total`, and `received-bytes` to detect completion. Single reassembly buffer at `ip-frag-base()`, up to 8KB payload. Outbound packets always set DF (Don't Fragment).

**DHCP**: Full DORA sequence (Discover → Offer → Request → Ack). Extracts IP, gateway, subnet mask, DNS server.

**DNS**: Simple A-record query over UDP port 53. Single outstanding query.

### Cryptography

#### 64-bit (`net/crypto.lisp`, 2018 lines)

Full implementations of the SSH-2.0 cipher suite:
- **SHA-256** (32-byte digest): Message schedule + 64 compression rounds
- **SHA-512** (64-byte digest): 80 compression rounds with 64-bit words
- **ChaCha20**: 20-round stream cipher (quarter-round × 8 per block + final add)
- **Poly1305**: One-time authenticator (clamp key, accumulate blocks, finalize)
- **X25519**: Elliptic curve Diffie-Hellman on Curve25519 (Montgomery ladder, 255 iterations)
- **Ed25519**: EdDSA signatures (scalar multiplication on twisted Edwards curve)

#### 32-bit (`net/crypto-32.lisp` + `net/crypto-w32.lisp`, 1252 lines)

i386 and ARM32 have 30-bit fixnums (31-bit word, tag bit steals 1 bit). SHA-256/512 and ChaCha20 values are split into `(hi16 . lo16)` cons pairs:
```lisp
;; 0xDEADBEEF represented as:
(cons #xDEAD #xBEEF)
```

Pair arithmetic functions (`w32-add`, `w32-mul`, `w32-shr`, etc.) propagate carries through the pair structure. Triple carry direction is critical: `(cons 0 (cons hi mid))` not `(cons hi (cons mid 0))`.

#### Fast field arithmetic (`net/crypto-fast.lisp` + `net/crypto-32-fast.lisp`)

Optimization layer replacing cons-based field elements with direct `mem-ref :u32` scratch memory access. Scratch memory at `fe-scratch-base()` (architecture-specific, ~256 bytes).

**64-bit** (`crypto-fast.lisp`): Direct memory field element operations, 2–5× speedup for X25519/Ed25519.

**32-bit** (`crypto-32-fast.lisp`, ~990 lines): Complete cons-free field arithmetic and X25519 for 30-bit fixnum targets. Key techniques:
- **3-word accumulators**: Each h[k] uses (hi2, hi, lo) at 12 bytes, with each word kept < 2^26 via immediate carry propagation. Prevents 30-bit fixnum overflow during fe-mul accumulation.
- **Hardware multiply**: `mul26lo`/`mul26hi` MVM opcodes for unsigned 32×32→64 multiply. Used both for field element multiply and for ×19 reduction (avoids IMUL overflow on `19 × 2^26`).
- **Split x25519/fe-invert**: Original `x25519` has 20-binding `let` and `fe-invert` has ~25 sequential forms — both exceed MVM compiler limits on 32-bit. Split into small functions (x25-alloc-ctx, x25-ladder-step, x25-step-mid, x25-step-fin, fe-invert-p1/p2/p3) matching ARM32's proven implementation.
- **tb-merge/fb-merge4 helpers**: Avoid 3+ level nested logior/logand/ash which silently clobbers on i386 (4 GPRs).

#### Performance

| Operation | x64 | AArch64 | ARM32 | i386 |
|---|---|---|---|---|
| SSH connection | <1 s | <1 s | ~2 s | ~1 s |
| Ed25519 sign | ~2 ms | ~10 ms | ~1 s | ~0.5 s |

All platforms pre-compute Ed25519 host key derivatives and X25519 ephemeral key pair at boot. ARM32 and Pi Zero 2W additionally poll USB during crypto to prevent NETDEV WATCHDOG timeout.

### SSH Server (`net/ssh.lisp`, 1262 lines)

Full SSH-2.0 server with key exchange, encryption, and interactive shell.

**Protocol flow**:
1. Version exchange (`SSH-2.0-modus`)
2. Algorithm negotiation (KEX_INIT)
3. Key exchange: ECDH with X25519 + Ed25519 host key signature
4. New keys: derive encryption (ChaCha20) and MAC (Poly1305) keys via SHA-256/512
5. User authentication (accepts any password)
6. Channel open (session)
7. Message loop: receive commands, eval Lisp expressions, send results

**Eval**: `ssh-do-eval-expr` reads a line from the SSH channel, parses it as a Lisp S-expression, evaluates it, and sends the result back. On platforms without a runtime compiler, this uses a compiled function dispatch table.

**Pre-computed keys**: Host Ed25519 key pair is embedded at build time. `pre-compute-host-sign` and `pre-compute-server-eph` run at boot to prepare scalar multiplication results, saving ~10s per SSH connection on slow platforms.

### HTTP (`net/http.lisp` + `net/http-client.lisp`, 504 lines)

**Server**: HTTP/1.0 on port 80. Single request-response per connection. Returns an HTML index page for GET /.

**Client**: URL parsing, DNS resolution, TCP connect, GET request with Host header, response body retrieval. Used for outbound fetches from bare metal (e.g., `(http-fetch "http://example.com/")`).

No HTTPS — plain HTTP only.

---

## I/O Delay (`io-delay`)

### Problem

QEMU's emulated NICs only process packets when the QEMU event loop runs. On bare metal, the guest CPU runs native code at full speed without yielding to QEMU. Without explicit I/O delays, the CPU spins through TCP retransmission loops and ARP resolution without giving QEMU time to deliver packets.

### Solution: interrupt-driven sleep

All architectures now use interrupt-driven sleep in SSH mode instead of busy-waiting:

| Platform | Timer | Sleep mechanism | Wake source |
|---|---|---|---|
| x64 | PIT 8254 (~100 Hz) | STI + HLT + CLI | PIT IRQ + E1000 IRQ 11 via IDT |
| i386 | PIT 8254 (~100 Hz) | STI + HLT + CLI | PIT IRQ + NE2000 IRQ 9 via IDT |
| AArch64 virt | ARM virtual timer (62.5 MHz) | timer-rearm + WFI | GICv2 timer PPI + E1000 SPI |
| RPi 3B | ARM virtual timer | timer-rearm + WFI | Timer PPI + DWC2 USB IRQ |
| ARM32 | ARM virtual timer (CP15) | timer-rearm + WFI | Timer PPI + DWC2 USB IRQ |
| Pi Zero 2W | ARM virtual timer | timer-rearm + WFI | Timer PPI + DWC2 USB IRQ |

### x86 approach (x64, i386)

PIC remapped (IRQ0 → INT 0x20, IRQ8 → INT 0x28), PIT programmed in mode 2 (rate generator) at ~100 Hz. IDT entries for PIT (INT 0x20) and NIC (E1000 INT 0x2B on x64, NE2000 INT 0x29 on i386). PIT ISR (7 bytes, master EOI only):
```asm
push eax        ; save
mov al, 0x20    ; EOI
out 0x20, al    ; send to PIC
pop eax         ; restore
iret            ; return
```

`io-delay` calls `(sti-hlt)` which atomically enables interrupts and halts. The CPU sleeps until the next PIT tick (~10ms), then `(cli)` re-disables interrupts. The MVM `+op-halt+` opcode emits a single HLT instruction (not an infinite loop).

Setup: `(setup-irq)` → TRAP #x0320 → native PIC+PIT+IDT code.

### ARM approach (AArch64, ARM32, RPi)

The ARM generic timer fires after a countdown in CNTV_TVAL (virtual timer value register). On AArch64, accessed via MSR/MRS system registers. On ARM32, accessed via MCR/MRC CP15 coprocessor (c14, c3).

**Polled WFI**: IRQs stay masked (PSTATE.I=1 on AArch64, CPSR I-bit on ARM32). The timer PPI (Private Peripheral Interrupt) wakes WFI even without an ISR running — the pending interrupt signal itself is the wake event. This avoids ISR corruption of long-running crypto computations.

**GICv2** (AArch64 QEMU virt only): GIC distributor at 0x08000000, CPU interface at 0x08010000. Enables timer PPI (INTID 27) at the GIC level. Not needed on RPi or fixpoint — timer PPI wakes WFI directly without GIC configuration.

**Timer rearm**: Before each WFI, `(timer-rearm)` writes 62500 to CNTV_TVAL_EL0 (AArch64) or CNTVTVAL via CP15 (ARM32). At 62.5 MHz, this gives ~1ms sleep. Clears the pending interrupt so the next WFI can sleep again.

Setup: `(setup-irq)` → TRAP #x0320 → timer init code (always on AArch64/ARM32). GIC init conditional on `*aarch64-setup-irq-enable*` (QEMU virt only).

### Fixpoint

The fixpoint multi-arch build dispatches at runtime based on architecture ID stored in the target descriptor:
```lisp
(defun io-delay ()
  (let ((mode (td-read-u32 #x480038)))
    (if (= mode 1)  ; SSH mode
        (let ((arch (td-read-u32 #x480008)))
          (if (= arch 3)  (progn (timer-rearm) (wfi))      ; ARM32
          (if (= arch 1)  (progn (timer-rearm) (wfi))      ; AArch64
                           (progn (sti-hlt) (cli)))))       ; x64/i386
        (dotimes (d 100) (mem-ref 0 :u8)))))  ; cross-compile: busy-wait
```

Cross-compile mode uses a short busy-wait (100 reads) because compilation speed matters more than CPU efficiency during the fixpoint chain.

### Flag-gated activation

Each platform uses a memory flag to switch between spinning and sleeping. The flag is set by an enable function called after all crypto initialization completes (crypto uses `io-delay` internally, so the timer must not interfere with pre-computation):

| Platform | Flag address | Enable function |
|---|---|---|
| x64/i386 | 0x600010 | `(enable-pit-timer)` |
| AArch64 virt | 0x41060704 | `(enable-gic-timer)` |
| RPi 3B / Pi Zero 2W | usb-ring-base + 0x10 | `(enable-rpi-timer)` |
| ARM32 | usb-ring-base + 0x10 | `(enable-arm32-timer)` |

Before the flag is set, `io-delay` falls back to the old spinning behavior (UART reads or serial port I/O).

---

## NIC Interrupt-Driven Receive

In addition to timer-driven io-delay, NIC drivers use hardware interrupts to wake the CPU when packets arrive. This allows HLT/WFI to sleep until actual network activity occurs, rather than relying solely on periodic timer ticks.

### Approach: polled interrupts

Same pattern as io-delay — NIC interrupts are enabled at the hardware level, but IRQs stay masked at the CPU. The pending interrupt signal wakes HLT/WFI; the main loop then polls the NIC registers as before. No ISR runs for NIC events.

On x86, NIC IRQs go through the PIC and require IDT entries with EOI-only ISRs (since HLT only wakes on unmasked interrupts). On AArch64, GIC level-triggered SPIs auto-clear when the hardware deasserts, so the existing ISR's GICC_IAR/GICC_EOIR handles them generically.

### Per-platform details

| Platform | NIC | IRQ/Interrupt | IDT/GIC entry | ISR |
|---|---|---|---|---|
| x64 | E1000 | IRQ 11 (slave PIC) | Vector 0x2B, ISR at 0x4F0810 | Slave+master EOI (10 bytes) |
| i386 | NE2000 | IRQ 9 (slave PIC) | Vector 0x29, ISR at 0x90410 | Slave+master EOI (9 bytes) |
| AArch64 virt | E1000 | PCI INTA → SPI 3 (INTID 35) | GICD_ISENABLER1 bit 3 | Generic GIC ISR in boot-aarch64 |
| RPi 3B / ARM32 | DWC2 host | USB interrupt | WFI wakes on DWC2 HCINT | Existing DWC2 interrupt handler |
| Pi Zero 2W | DWC2 gadget | USB interrupt | WFI wakes on DWC2 device IRQ | Existing DWC2 gadget ISR |

### E1000 interrupt setup

E1000 Interrupt Mask Set (IMS, reg 0xD0) enables RXT0 (bit 7 — RX timer interrupt). The RX timer fires when a packet arrives and the programmable delay expires. Interrupt Cause Read (ICR, reg 0xC0) is read-to-clear: reading it clears all pending causes.

In `e1000-hw-receive`, when no packet is found, ICR is read to clear the pending interrupt cause. This allows the next HLT/WFI to sleep until a new packet arrives rather than immediately waking on a stale cause.

### NE2000 interrupt setup

NE2000 Interrupt Mask Register (IMR, port 0x30F) enables PRX (bit 0 — packet received). The ISR register (port 0x307) is write-1-to-clear: writing 0x01 clears the PRX bit. Done at the start of `ne2k-receive` so HLT can sleep until the next packet.

### DWC2 (RPi 3B, ARM32, Pi Zero 2W)

DWC2 USB interrupts are already configured by the existing ISR in `boot-rpi.lisp` (gadget mode) and `arch-arm32-rpi.lisp` (host mode). The `dwc2-enable-host-irq` function enables HCINT channel interrupts at the DWC2 controller level, and WFI wakes when USB transfer completion fires.

---

## Architecture Adapters

Each platform provides a thin adapter file (~200 lines) in `net/` implementing:

| Function | Purpose |
|---|---|
| `pci-config-read/write` | PCI bus access (stubs on non-PCI platforms) |
| `io-delay` | Sleep/yield to QEMU event loop |
| `e1000-state-base` | Base address for NIC + protocol state |
| `ssh-conn-base` | SSH connection state base |
| `ssh-ipc-base` | SSH IPC / line editor state base |
| `write-byte` | Capture-aware serial output (routes to SSH channel or UART) |
| `make-array`, `aref`, `aset` | Object allocation (32-bit platforms need different headers) |
| `yield` | Actor system yield (WFI or NOP) |

### Adapter files

| Platform | File | PCI | NIC state | Notes |
|---|---|---|---|---|
| x64 | `arch-x86.lisp` | I/O port 0xCF8/CFC | 0x05060000 | Standard x86 PCI |
| AArch64 virt | `arch-aarch64.lisp` | ECAM at 0x4010000000 | 0x41060000 | 48-bit MMIO |
| i386 | `arch-i386.lisp` | Stubs (no PCI) | 0x200000 | All < 8MB (fixnum safe) |
| RPi 3B | `arch-raspi3b.lisp` | Stubs | 0x01060000 | DWC2 at 0x3F980000 |
| ARM32 | `arch-arm32-rpi.lisp` | Stubs | 0x01060000 | 4-byte object headers |
| Pi Zero 2W | `arch-raspi3b.lisp` | Stubs | 0x01060000 | Shares with RPi 3B |

### Override chain

Source load order determines which `defun` wins (last-defun-wins):
```
arch-* → [actors] → NIC driver → ip → crypto → [crypto-32] → [crypto-w32] →
ssh → [http] → [http-client] → aarch64-overrides → [actors-net-overrides] →
[32bit-overrides] → [crypto-fast] → [crypto-32-fast] → [isolated-net]
```

Files in brackets are loaded only for specific builds. Later files override earlier defuns of the same name. This is how 32-bit crypto, actor-aware I/O, and isolation are layered without modifying shared code.

---

## Actor System (`net/actors.lisp`, ~400 lines)

Cooperative scheduling on a single core. Actors communicate via mailbox messages (async send/receive). No preemption — actors yield voluntarily via `(yield)`.

**Actor layout**:
- Actor 1: Primordial (kernel-main → idle yield loop)
- Actor 2: Net-domain (owns NIC hardware: polling, TCP/IP, ARP)
- Actor 3+: SSH handler (one per connection)

**Per-actor heaps**: 4MB each, starting at `actor-heap-base + (id-1) << 22`.

**Isolated mode** (`net/isolated-net.lisp`): Qubes-like architecture where SSH handler actors cannot access hardware directly. All NIC operations are proxied through the net-domain actor via mailbox messages. This prevents a compromised SSH session from accessing raw network hardware.

---

## Memory Layout

### State regions by platform

| Region | x64 | AArch64 virt | i386 | RPi / ARM32 |
|---|---|---|---|---|
| NIC state | 0x05060000 | 0x41060000 | 0x200000 | 0x01060000 |
| SSH connection | 0x05080000 | 0x41080000 | 0x280000 | 0x01080000 |
| USB ring buffer | — | — | — | 0x01090000 |
| SSH IPC | 0x300000 | 0x41100000 | 0x300000 | 0x01100000 |
| USB DMA buffers | — | — | — | 0x01000000 |
| Crypto scratch | — | — | 0x200900 | 0x01060900 |

### NIC state layout (shared, relative to `e1000-state-base`)

```
+0x000  MMIO/DWC2 base     +0x008  MAC address (6 bytes)
+0x010  RX cursor           +0x014  TX cursor
+0x018  Our IP address      +0x01C  Gateway IP
+0x028  Gateway MAC         +0x030  TCP state
+0x034  TCP ports           +0x038  TCP remote IP
+0x03C  TCP sequence nums   +0x058  DNS server IP
+0x060  DHCP state          +0x0A0  ARP cache
+0x600  Ed25519 init flag    +0x680  Pre-computed s (32B)
+0x6A0  Pre-computed prefix  +0x6C4  Eph X25519 priv (32B)
+0x6E4  Eph X25519 pub (32B) +0x704  Timer-enabled flag
+0x710  Host priv key (32B)  +0x730  Host pub key (32B)
```

---

## Testing

### Quick test (any SSH build)

```bash
# Build
sbcl --script mvm/build.lisp aarch64/bare/qemu/ssh

# Run
qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 \
  -kernel /tmp/modus-aarch64-ssh.bin -nographic -semihosting \
  -device 'e1000,netdev=net0,romfile=,rombar=0' \
  -netdev 'user,id=net0,hostfwd=tcp::2222-:22'

# Test
echo '(+ 1 2)' | ssh -p 2222 -o StrictHostKeyChecking=no test@localhost
# → = 3
```

### Platform-specific QEMU commands

**i386**:
```bash
qemu-system-i386 -m 256 -nographic -no-reboot \
  -kernel /tmp/modus-i386-ssh.bin \
  -device ne2k_isa,netdev=net0,iobase=0x300,irq=9 \
  -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
```

**RPi 3B**:
```bash
qemu-system-aarch64 -machine raspi3b -m 1G -display none -serial stdio \
  -kernel /tmp/kernel8-ssh.img \
  -device usb-net,netdev=net0 \
  -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
```

**ARM32**:
```bash
qemu-system-arm -M raspi2b -m 1G -nographic \
  -kernel /tmp/modus-arm32-ssh.bin \
  -device usb-net,netdev=net0 \
  -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
```

**Pi Zero 2W** (real hardware):
```bash
sudo rpiboot -d /tmp/piboot
sudo ip addr add 10.0.0.1/24 dev usb0
sudo ip link set usb0 up
ssh test@10.0.0.2
```

### Fixpoint SSH chain

```bash
sbcl --script mvm/build-fixpoint.lisp
./scripts/run-fixpoint-ssh.sh x64 arm32
echo '(+ 1 2)' | ssh -p 2223 test@localhost  # → = 3 on Gen1
```

---

## Known Limitations

1. **No HTTPS**: Plain HTTP only. SSH provides the encrypted channel.

2. **DWC2 channel halt corruption**: QEMU's DWC2 emulation corrupts state when bulk IN channels are halted. The driver uses non-blocking polling only.

3. **QEMU usb-net RNDIS**: Config descriptor index 0 is RNDIS (unsupported). Must use index 1 for CDC-ECM.

4. **Single reassembly buffer**: IP fragment reassembly supports out-of-order arrival but only one packet at a time (single ident/src-ip session). Up to 8KB payload. Outbound packets always set DF (Don't Fragment).

5. **TCP connection limit**: Connection table supports up to 8 simultaneous TCP connections. Sufficient for SSH + HTTP but not for high-concurrency workloads.
