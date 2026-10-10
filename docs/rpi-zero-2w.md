# Raspberry Pi Zero 2 W: Bare-Metal Lisp over USB

## Overview

Modus runs bare-metal on the Pi Zero 2 W (BCM2710A1, Cortex-A53). The system boots from SD card into a UART bootloader that accepts kernel uploads over serial, eliminating SD card swapping during development. A built-in SSH server over USB CDC-ECM Ethernet provides remote Lisp evaluation.

```
┌─────────────────────────────────────────────────────────┐
│                    Pi Zero 2 W                          │
│                                                         │
│  ┌──────────┐   ┌──────────┐   ┌────────────────────┐  │
│  │ Bootloader│──>│ Kernel   │──>│ SSH Server         │  │
│  │ (UART RX) │   │ (jumped) │   │ (CDC-ECM over USB) │  │
│  └──────────┘   └──────────┘   └────────────────────┘  │
│       ▲                               │                 │
│       │ serial                        │ USB gadget      │
└───────│───────────────────────────────│─────────────────┘
        │                               │
   ┌────┴────┐                     ┌────┴────┐
   │  Pi 5   │                     │  Pi 5   │
   │ (deploy)│                     │ (SSH)   │
   └─────────┘                     └─────────┘
   GPIO14/15                       USB cable
   + GPIO17 reset
```

---

## Quick Start

### First-time setup (one SD card flash)

```bash
# Build the bootloader kernel (includes SSH fallback)
bash scripts/make-sdcard-bootloader.sh

# Flash to micro-SD (one time only!)
sudo dd if=/tmp/pizero2w-sdcard.img of=/dev/sdX bs=4M status=progress
```

### Deploy a kernel (no SD card needed)

```bash
# Build any kernel
sbcl --script mvm/build-rpi-periph.lisp     # peripheral test
sbcl --script mvm/build-pizero2w-ssh.lisp    # SSH server
sbcl --script mvm/build-uart-bootloader.lisp # bootloader itself

# Copy to Pi 5 and deploy over UART
scp /tmp/piboot/kernel8.img modus@<pi5-ip>:~/kernel8.img
ssh modus@<pi5-ip> 'python3 ~/deploy-kernel.py ~/kernel8.img'
```

### SSH into the Pi Zero

```bash
# After deploying the SSH kernel and USB enumeration:
IFACE=$(ip -o link | grep -i 02:00:00:00:00:01 | awk -F'[: ]' '{print $3}')
sudo ip addr add 10.0.0.1/24 dev "$IFACE"
sudo ip link set "$IFACE" up
ssh test@10.0.0.2

modus> (+ 1 2)
= 3
```

---

## Hardware Setup

### Wiring (Pi 5 → Pi Zero 2 W)

```
Pi 5                     Pi Zero 2 W
─────                    ───────────
GPIO14 (TXD) ──────────> GPIO15 (RXD)      UART data
GPIO15 (RXD) <────────── GPIO14 (TXD)      UART data
GND          ──────────> GND               Common ground
GPIO17       ──────────> RUN pin           Reset (active low)
                  USB cable                 Power + CDC-ECM
```

### Pi 5 UART configuration

```bash
# Enable UART0 on Pi 5
sudo dtoverlay uart0-pi5

# Stop getty on UART
sudo systemctl stop serial-getty@ttyAMA10.service

# GPIO17 for reset (must be input with no pull when not resetting)
pinctrl set 17 ip pn
```

---

## Architecture

### Boot Flow

```
                            SD Card Boot
                                 │
         ┌───────────────────────┼───────────────────────┐
         │                       ▼                       │
         │    GPU firmware (bootcode.bin → start.elf)     │
         │                       │                       │
         │              Load kernel8.img                  │
         │               at 0x80000                      │
         │                       │                       │
         │                       ▼                       │
         │    ┌──────────────────────────────────┐       │
         │    │        Boot Preamble             │       │
         │    │  SP = 0x200000                   │       │
         │    │  PL011 init (IBRD=26, FBRD=3)   │       │
         │    │  x24 = cons ptr (0x04000000)     │       │
         │    │  x25 = alloc limit (0x05000000)  │       │
         │    │  x26 = NIL (0)                   │       │
         │    └──────────┬───────────────────────┘       │
         │               │                               │
         │               ▼                               │
         │    ┌──────────────────────────────────┐       │
         │    │        kernel-main               │       │
         │    │                                  │       │
         │    │  1. Init mini UART (115200)      │       │
         │    │  2. Print "BOOT"                 │       │
         │    │  3. Blink LED once               │       │
         │    │  4. Print "RDY"                  │       │
         │    │  5. Wait 5s for magic 0x55       │       │
         │    │         │              │         │       │
         │    │    [received]     [timeout]      │       │
         │    │         │              │         │       │
         │    │         ▼              ▼         │       │
         │    │  Load kernel     Print "SSH"     │       │
         │    │  at 0x300000     Init USB gadget │       │
         │    │  Jump to it      CDC-ECM + SSH   │       │
         │    └──────────────────────────────────┘       │
         └───────────────────────────────────────────────┘
```

### UART Deploy Protocol

```
    Pi 5 (host)                     Pi Zero 2 W (target)
    ───────────                     ──────────────────────

    GPIO17 LOW (300ms)  ─────────>  Reset
    GPIO17 INPUT        ─────────>  Release
                                    ... GPU boots ...
                        <─────────  "BOOT\nRDY\n"
    0x55 (magic)        ─────────>
                        <─────────  0xAA (ACK)
    [size: 4 bytes LE]  ─────────>
    [kernel data...]    ─────────>  Writes to 0x300000
    [checksum: 1 byte]  ─────────>
                        <─────────  "OK\n" or "ER\n"
                                    Jump to 0x300000
```

Transfer speed: 115200 baud (~11.5 KB/s). A 130KB periph kernel takes ~12s, a 530KB SSH kernel takes ~47s.

### USB CDC-ECM Stack

```
┌─────────────────────────────────┐
│          SSH Server             │  ssh.lisp
│   (eval, channel I/O, auth)    │
├─────────────────────────────────┤
│         TCP/IP/ARP              │  ip.lisp
│   (DHCP, checksums, segments)  │
├─────────────────────────────────┤
│      CDC-ECM Ethernet           │  dwc2-device.lisp
│  (USB gadget, raw Ethernet)    │
├─────────────────────────────────┤
│    DWC2 Device Controller       │  dwc2-device.lisp
│  (PIO mode, Full Speed 12Mbps) │
├─────────────────────────────────┤
│       BCM2710A1 Hardware        │
│      USB OTG at 0x3F980000     │
└─────────────────────────────────┘

Static IP: 10.0.0.2  MAC: 02:00:00:00:00:01
Host IP:   10.0.0.1
```

### Crypto Stack

All cryptography runs on the bare-metal Cortex-A53 in pure Lisp:

- **Key exchange**: X25519 (Curve25519 ECDH)
- **Host key**: Ed25519 (pre-computed, embedded in kernel)
- **Encryption**: ChaCha20-Poly1305
- **Hashing**: SHA-256, SHA-512
- **Random**: BCM2835 system timer entropy

---

## Memory Map

```
0x00000000 ┌─────────────────────────────┐
           │                             │
0x00080000 │  Kernel image (SD boot)     │  ~530 KB
           │  (bootloader + SSH)         │
0x00200000 │  Stack (grows down)  ↓      │
           │                             │
0x00300000 │  UART-loaded kernel         │  ~530 KB
           │  (jumped to by bootloader)  │
           │                             │
0x01000000 │  USB DMA region             │
0x01001000 │  RX buffer (2048 bytes)     │
0x01041400 │  TX buffer (1536 bytes)     │
0x01050000 │  DWC2 gadget state          │
0x01050100 │  Mailbox buffer             │
0x01050200 │  Framebuffer state          │
0x01060000 │  Network state (IP/MAC/ARP) │
0x01080000 │  SSH connection buffers     │
0x01100000 │  SSH IPC state              │
0x01200000 │  HID state (keyboard/mouse) │
           │                             │
0x04000000 │  Cons heap                  │  16 MB
0x05000000 │  General heap               │
           │                             │
0x3F000000 │  BCM2837 Peripherals        │
0x3F003000 │    System Timer (1MHz)      │
0x3F104000 │    Hardware RNG             │
0x3F200000 │    GPIO                     │
0x3F215000 │    AUX / Mini UART         │
0x3F980000 │    DWC2 USB Controller     │
           └─────────────────────────────┘
```

---

## BCM2835 Peripherals

All drivers in `net/bcm2835-periph.lisp`. Tested on both QEMU raspi3b and real hardware.

| Peripheral | Address | Status | Notes |
|------------|---------|--------|-------|
| System Timer | 0x3F003004 | Working | 1MHz free-running CLO counter |
| Hardware RNG | 0x3F104000 | Broken | Crashes on real BCM2710A1 (needs clock enable) |
| GPIO / LED | 0x3F200008 | Working | GPIO29 active-low activity LED |
| GPU Framebuffer | Mailbox ch8 | Working | 640x480x32bpp via property tags |
| Mini UART | 0x3F215040 | Working | 115200 baud, 32-bit stores required |
| DWC2 USB | 0x3F980000 | Partial | Host mode works (QEMU); gadget enumeration WIP |

### Hardware Quirks

- **Mini UART requires 32-bit stores** — byte stores (STRB) are silently ignored by BCM2837
- **Mini UART TX poll required** — TX FIFO is only 8 deep; writes without polling are dropped
- **AUX_MU_LSR is at offset 0x14** (0x3F215054), not 0x1C (0x3F21505C which is SCRATCH)
- **PM_RSTC causes QEMU reset** — writing 0x5A000020 to 0x3F10001C reboots QEMU raspi3b
- **Hardware RNG crashes** — accessing 0x3F104000 faults on real BCM2710A1 without clock domain enable
- **GPU framebuffer address** — mask off 0xC0000000 to convert GPU bus address to ARM physical
- **Pixel order** — use BGR mode (0) with `make-color` building 0xFFRRGGBB; little-endian stores blue in LSB
- **MVM YIELD = SEV+WFE** — bare WFE stalls real Cortex-A53; SEV sets event flag so WFE returns immediately

---

## Files

### Source files (`net/`)

| File | Lines | Purpose |
|------|-------|---------|
| `arch-raspi3b.lisp` | ~200 | RPi 3B hardware adapter (addresses, PCI stubs) |
| `dwc2-device.lisp` | ~710 | DWC2 USB gadget + CDC-ECM Ethernet driver |
| `bcm2835-periph.lisp` | ~216 | System timer, LED, framebuffer, RNG |
| `uart-bootloader.lisp` | ~110 | UART serial bootloader protocol |
| `dwc2.lisp` | ~559 | DWC2 USB host controller (QEMU) |
| `usb.lisp` | ~417 | USB enumeration + hub support |
| `cdc-ether.lisp` | ~150 | CDC Ethernet NIC (QEMU host mode) |
| `hid.lisp` | ~400 | USB HID keyboard/mouse/tablet |
| `ip.lisp` | ~2000 | ARP, IP, TCP, DHCP (shared) |
| `crypto.lisp` | ~2500 | SHA, ChaCha20, Poly1305, X25519, Ed25519 (shared) |
| `ssh.lisp` | ~1500 | SSH server (shared) |

### Build scripts (`mvm/`)

| Script | Output | Purpose |
|--------|--------|---------|
| `build-uart-bootloader.lisp` | 528 KB | Permanent SD card bootloader + SSH fallback |
| `build-pizero2w-ssh.lisp` | 545 KB | SSH-only kernel (no bootloader) |
| `build-rpi-periph.lisp` | 134 KB | BCM2835 peripheral diagnostic |
| `build-rpi-ssh.lisp` | ~550 KB | RPi 3B SSH (QEMU, USB host mode) |
| `build-rpi-hid.lisp` | ~200 KB | RPi 3B HID keyboard REPL (QEMU) |

### Deployment scripts

| Script | Location | Purpose |
|--------|----------|---------|
| `deploy-kernel.py` | `scripts/` | UART kernel upload from Pi 5 |
| `make-sdcard-bootloader.sh` | `scripts/` | One-time SD card with bootloader |
| `make-sdcard.sh` | `scripts/` | SD card with SSH-only kernel |

---

## QEMU Testing

Most development uses QEMU raspi3b for testing before deploying to real hardware.

```bash
# Peripheral diagnostic (mini UART on serial1, framebuffer on display)
sbcl --script mvm/build-rpi-periph.lisp
qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \
    -serial null -serial stdio -display gtk

# SSH server (PL011 on serial0, USB networking)
sbcl --script mvm/build-rpi-ssh.lisp
qemu-system-aarch64 -M raspi3b -kernel /tmp/kernel8-ssh.img \
    -display none -serial stdio \
    -device usb-net,netdev=net0 \
    -netdev 'user,id=net0,hostfwd=tcp::2222-:22'
ssh -p 2222 -o StrictHostKeyChecking=no test@localhost

# Bootloader timeout test (should print BOOT, RDY, then fall through to SSH)
sbcl --script mvm/build-uart-bootloader.lisp
qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \
    -serial null -serial stdio -display none
```

**QEMU serial ports**: `-serial null -serial stdio` routes mini UART (serial1) to terminal. PL011 (serial0) goes to `/dev/null`. Swap for PL011 builds.

---

## Current Status

| Feature | QEMU | Real Hardware |
|---------|------|---------------|
| Mini UART (serial output) | Working | Working |
| System timer / delays | Working | Working |
| GPIO LED blink | Working | Working |
| GPU framebuffer (HDMI) | Working | Working |
| UART bootloader | Working | Working |
| USB host mode (CDC-ECM) | Working | N/A (QEMU only) |
| USB gadget mode (CDC-ECM) | N/A | Enumeration WIP |
| SSH over USB | Working (QEMU) | Blocked on USB gadget |
| USB HID keyboard | Working (QEMU) | Not tested |

The UART bootloader + peripheral drivers work end-to-end on real hardware. SSH over USB requires fixing DWC2 gadget device descriptor enumeration (error -71 on Pi 5 host).
