# Zero 2 W HVS display overlay — investigation notes (2026-09-12)

Goal: play VP8 on the Pi Zero 2 W's HDMI at 60 fps. The frame is ~350 ms (~3 fps)
= ~222 ms decode + ~128 ms blit. The blit (CPU converting YUV→RGB and upscaling
into the scanout in Lisp) is the wrong tool for 60 fps here; the VideoCore's
Hardware Video Scaler (HVS) does colorspace + scale in hardware. The plan: hand
the HVS the decoder's small YUV planes at a stable address, so the display costs
zero per-frame CPU. This doc records how far that got and exactly where it stops.
Register/display-list reference: `videocore-hvs-overlay.md`. Driver + primitives:
`net/hdmi-hvs.lisp`.

## The ladder (each rung is a real result)

1. **Firmware-owned display (default) → the ARM cannot touch the HVS.** We boot
   the firmware's simple mailbox framebuffer (`fbtall` in `net/hdmi-runtime.lisp`).
   The HVS at ARM `0x3F400000` reads static garbage while the mailbox (`0x3F00B898`
   → clean `0x40000000`) and timer and USB all read fine. A webcam frame of the
   monitor shows our cleared framebuffer, so the HVS is *running* — the VideoCore
   just keeps it to itself. Our reference `librerpi/lk-overlay` reaches the HVS
   only because it runs *on the VideoCore VPU*, not the ARM.

2. **Full KMS (`dtoverlay=vc4-kms-v3d`) → Modus won't boot.** U-Boot netboots and
   the kernel decompresses, then a silent fault before NETUP. Cause: the vc4
   overlay reserves a large **CMA** carveout (default ~256 MB on the 512 MB board)
   that lands on Modus's *fixed* low-memory map — stack `0x08000000`, Cheney heap
   `0x09000000`–`0x10000000`, metadata `0x10000000`, **USB-DMA window `0x11000000`**
   (the r8152 networking depends on it). Linux never trips on this because it reads
   the DTB `reserved-memory` node; Modus uses hardcoded addresses.

3. **KMS + `cma-64` → Modus boots.** `dtoverlay=vc4-kms-v3d,cma-64` shrinks the
   carveout to 64 MB; the firmware places it high (~`0x1C000000`), clear of
   everything Modus uses below ~`0x11200000`. **NETUP.** DTB confirms
   `hvs@7e400000 reg 0x6000` → ARM `0x3F400000` (address was right all along).

4. **Power + clock the display block → PARTIAL HVS access.** In KMS the firmware
   hands the display to the ARM but leaves it unpowered/unclocked (expecting an OS
   driver). Linux's vc4 on VC-IV (Pi 0–3) does *no* clock/power for the HVS — it
   rides the always-on CORE clock and `vc4_hvs_hw_init` just does
   `SCALER_DISPCTRL |= (1<<31)`. We enabled the display power domains
   (SET_DOMAIN_STATE, VIDEO_SCALER=3/HDMI=5/VEC=7) and clocks (SET_CLOCK_STATE,
   CORE=4/DISP=16/PIXEL=9) via the property mailbox (`hvs-display-on`). Result:
   **twice** we read a clean, real `DISPCTRL = 0x9A0F00FF` (ENABLE bit set) — proof
   the ARM *can* reach the live HVS. But it is **non-deterministic**.

## Where it stops: partial bus, needs the VCHIQ handover

The reproducible behavior is **byte-partial**: a `u32` write to an HVS register
latches only its **low 8 bits**, and reads return `0x646F43` in the top 24
(`write 0x42 → read 0x646F4342`, `0x123 → …23`, `0x456 → …56`). That signature —
low byte works, high 24 fixed, with rare clean full reads — is a display
peripheral bus that is only *partway* connected to the ARM.

Linux gets clean, stable access because full KMS completes a **firmware handover
over VCHIQ** (the VideoCore message channel) that fully hands the display block to
the ARM. The property **mailbox** (all `hdmi-fb.lisp`/`hdmi-hvs.lisp` implement)
can power and clock the block but cannot perform that handover. So:

- **Proven:** the ARM can partially reach the HVS on this hardware (byte writes
  latch; occasional full real reads). Feasibility is not in question.
- **Not achieved:** stable 32-bit read/write, which the overlay build needs.
- **The one barrier left:** the VCHIQ handshake, or running the display code on
  the VPU (the `lk-overlay` approach). Both are project-sized, not more poking.

## To resume

Reliable HVS access = implement enough **VCHIQ** to make the firmware complete the
KMS display handover (message-protocol driver against the firmware; see the Linux
`drivers/staging/vc04_services` / `bcm2835-vchiq` and how vc4 + firmware negotiate
display ownership), **or** run a small display component on the VideoCore VPU like
`librerpi/lk-overlay`. Once the HVS reads/writes cleanly, the overlay itself is the
mechanical part already speced in `videocore-hvs-overlay.md`: build a display list
with the firmware FB plane + a scaled YUV plane (format 8, BT.601 CSC
`0x00f00000/0xe73304a8/0x00066604`, PPF upscale, kernel upload), point
`SCALER_DISPLISTx` at it, refresh the decoder's Y/U/V each frame.

## Board rig / reproduce

- SD boot config lives on the Zero's SD (`/dev/sda1` when in modus-pi's reader).
  Current: `config.txt` has `dtoverlay=vc4-kms-v3d,cma-64`. **Working netboot
  backup: `config.txt.uboot-bak`** — restore it (SD → modus-pi, `sudo mount
  /dev/sda1 /mnt/zboot; sudo cp .../config.txt.uboot-bak .../config.txt`) to get
  the plain board back for other work.
- Netboot from modus-pi: `python3 ~/netboot-gz.py --img board-demo6.img.gz
  --send '(ssh-boot)' --send-delay 240`; wait for NETUP; `ssh test@10.0.0.2`.
- HVS probes are RUNTIME-PUSHED (only use `mem-ref` + baked mailbox helpers), no
  rebake: push `net/hdmi-hvs.lisp`'s defuns, then `(hvs-display-on)` `(hvs-dump)`.
- Image build (has HVS reachable, mailbox, USB net): branch `hdmi-on-main`
  (HDMI-display commits cherry-picked onto main) via `cabfs/core/build-board.sh`
  (12 GB SBCL build), gzip → `board-demo6.img.gz`, put in `/srv/tftp` on modus-pi.
