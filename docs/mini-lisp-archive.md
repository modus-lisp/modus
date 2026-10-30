# The mini-Lisp images: archived 2026-10-28

Until this date some bare-metal images did not run Common Lisp.  They ran
`mvm/repl-source.lisp`, a separate 708-line Lisp with its own reader,
printer, evaluator and global alist, built together with the `net/` drivers.
"Modus is CL now" (2026-10-23) meant that second Lisp had to go.  Each image
was either **ported**, meaning its drivers now run on a CL image, or
**archived**, meaning it was deleted from the tree and kept under a git tag.

**Tag:** `mini-lisp-final` is the last commit that still has all of them.
To recover a file:

```bash
git show mini-lisp-final:mvm/repl-source.lisp
git show mini-lisp-final:net/dwc2-device.lisp > /tmp/dwc2-device.lisp
git worktree add ../mini mini-lisp-final     # the whole tree as it was
```

Most of these images were already broken before they were archived.  Every
RPi and Pi Zero mini image failed in `BUILD-IMAGE :TARGET :RPI` with
`#S(A64-BUFFER)` vs `MVM-BUFFER` (`scripts/run.sh` listed them as
KNOWN_BROKEN).  Recovering one therefore means repairing it as well.

## What replaced each image

| mini image | build | now | verified |
|---|---|---|---|
| x64 REPL | `build.lisp x64/bare/qemu/repl` | `x64/bare/qemu/cl-repl` | the gate builds it; `run.sh x64-cl-repl` |
| x64 SSH | `build.lisp x64/bare/qemu/ssh` | `x64/bare/qemu/cl-ssh` (`run.sh x64-ssh`) | QEMU: `(+ 1 2)` and `(lisp-implementation-type)` over SSH |
| i386 REPL | `build-i386-repl.lisp` | `i386/bare/qemu/cl-repl` | 6a6c4dc / 718250d |
| i386 SSH (NE2000) | `build.lisp i386/bare/qemu/ssh` | `i386/bare/qemu/cl-ssh` (`run.sh i386-ssh`) | QEMU: two sessions over the NE2000 |
| arm32 REPL | `build-arm32-repl.lisp` | `build-arm32-cl-repl.lisp` | 80ead3e |
| aarch64 virt REPL / SSH / actors / isolated | `build.lisp aarch64/bare/qemu/*` | `build-aarch64.lisp`, `aarch64/bare/qemu/cl-ssh` | 9c8116f |
| RPi REPL | `build.lisp aarch64/bare/rpi/repl` | `build-rpi-cl-repl.lisp` | the gate builds it |
| RPi SSH (USB host, CDC Ethernet) | `build-rpi-ssh.lisp` | `build-rpi-cl-repl.lisp` with `MODUS_NET_BUILD=1 MODUS_SSH_BUILD=1` | real boards (r8152, CDC); 3b6415e, isolated |
| RPi USB keyboard REPL | `build-rpi-hid.lisp` | the board image's `net/usb-hid-split.lisp` | real board |
| Pi Zero HDMI console | `build-pizero2w-hdmi.lisp` | the board image's `net/hdmi-console.lisp` | real board |
| RPi peripherals (timer, LED, framebuffer, RNG) | `build-rpi-periph.lisp` | **ported:** `net/bcm2835-gpio.lisp` (GPIO, system timer, LED) in every Pi CL image; framebuffer = `net/hdmi-fb.lisp`, RNG = `net/hwrng-bcm2835.lisp` | QEMU raspi3b: `(1 NIL T 2 2 20144)` — LED pin set to output, driven low then high, GPIO14 to ALT5, `delay-us 20000` |

## Archived without a CL replacement

Both of these need the physical board to port, so neither could be verified
here.

| image | what it was | to port it |
|---|---|---|
| Pi Zero 2 W USB-gadget SSH (`build-pizero2w-ssh.lisp`, `build-pizero2w-actors.lisp`) | `net/dwc2-device.lisp`: the DWC2 controller in **device** mode presenting a CDC-ECM NIC to the host (10.0.0.2). The board ran SSH over its one OTG port with no adapter. `scripts/build-pizero2w.sh`, `boot-pizero2w.sh` and `build-turnkey-ssh.sh` built and deployed it. | Recover `dwc2-device.lisp` and splice it into `build-rpi-cl-repl.lisp`'s `:rpi` net arm in place of `dwc2.lisp` + `usb.lisp`. The 32-bit-store and DMA-address lessons in `net/arch-rpi-cl.lisp` apply. Needs a Zero on a USB host. |
| UART bootloader (`build-uart-bootloader.lisp`, `net/uart-bootloader.lisp`, `scripts/deploy-kernel.py`, `make-sdcard-bootloader.sh`) | An SD-card resident kernel that waited ~2 s for magic byte 0x55 on the mini UART and loaded a new kernel from it. Otherwise it fell through to the gadget SSH server. | The board flow now netboots through U-Boot (`MODUS_RPI_CHAINLOAD=1`; docs/quickload-on-rpi.md). A UART loader would be a few hundred bytes of hand-written AArch64, not a Lisp image. |

## Also removed

These had no consumers once the images above were gone:

- **Drivers:** `net/{arch-raspi3b, arch-x86, arch-i386, arch-arm32, arch-arm32-rpi, actors-net-overrides, hid, http, 32bit-overrides, crypto-32, crypto-w32, mvm-ssh-fixes, bare-runtime-stubs, bcm2835-periph, dwc2-device, dwc2-32, uart-bootloader, i386-console, i386-ehci, i386-ehci-full, i386-e1000-raw}.lisp`
- **Scripts:** `scripts/run-{x64-ssh, i386-ssh, rpi-repl, rpi-ssh, rpi-hid, rpi-periph}.sh`
- **Tests:** `test/features/` (all of it referenced deleted cells or the long-gone `modus.build:build-kernel-mvm`) and `tests/test-aarch64-*.lisp` (they concatenated `*repl-source*`)

The CL images have their own adapters: `arch-x86-cl`, `arch-i386-cl`,
`arch-aarch64-cl` and `arch-rpi-cl`.
