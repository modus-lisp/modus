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

## Where it stops: the HVS is 8-bit-wide to the ARM (CORRECTED 2026-09-13)

**The 2026-09-12 "byte-partial, non-deterministic, needs VCHIQ" conclusion was
WRONG on the evidence.** Re-decoding the probe logs (they print DECIMAL) and
re-probing on the live board settles it precisely:

- Every HVS register reads back **`0x646472` in bits[31:8]** and the **low byte
  of the last value written** in bits[7:0]. Fully **deterministic** (identical
  reads ×3; `hvsstab`/`hvsconfirm`). `DISPCTRL`=`0x646472FF`, write `0xCAFEBABE`
  → `0x646472BE`, write `0x123` → `0x64647223`, etc. The banked
  "occasional clean 0x9A0F00FF" was a misread of this same pattern (low byte `FF`).
- It is **not a floating bus-keeper**: reading V3D (`0xDEADBEEF`) or the system
  timer immediately before an HVS read does NOT drag the HVS top-24 — it stays
  fixed `0x646472`. The HVS block itself drives that constant.
- **Byte-addressing does not help.** `:u8` reads of lanes 1/2/3 return the fixed
  `0x72,0x64,0x64`; `:u8` writes to lanes 1/2/3 are ignored. Only **byte lane 0**
  is a real, writable register.
- It is **HVS-window-specific, not an ARM-bus limit**: the mailbox, the system
  timer, and V3D (`0x3FC00000`, reads full-width `0xDEADBEEF`) all do clean
  32-bit ARM access through the same Device-nGnRnE mapping. So it is **not** our
  MMU/cache/mem-ref codegen — those are proven good at the same instant.

Net: **in the KMS+cma-64 state the ARM can drive only bit[7:0] of any HVS
register; lanes 1–3 are dead.** That kills the byte-wise workaround AND the
"compose a scaled-YUV plane on top of the firmware simple-FB" plan — you cannot
write a 32-bit dlist slot pointer or a CTL0 word through an 8-bit hole.

### The crux mystery (this is the thing to crack next)

Mainline Linux `vc4` on VideoCore IV (Pi 0–3) gets **full 32-bit** HVS access
with **no VCHIQ, no clock enable, no power-domain call** — it just `ioremap`s
`0x3f400000` and writes `SCALER_DISPCTRL` directly (verified against
raspberrypi/linux `vc4_hvs.c`: the `clk_get`/`clk_prepare_enable` path is
`gen >= VC4_GEN_5` / BCM2711 only; VC-IV relies on always-on firmware clocking
and direct MMIO). We access the **same address, same Device attributes, same
power state** and get 8-bit. So the difference is some **software/handover state**
Linux establishes that we don't — NOT a hardware barrier and NOT VCHIQ. Cracking
*why Linux gets 32-bit here* is cheaper than committing to a full driver and
unblocks everything. Power-domain pokes (`hvs-display-on`) did **not** change the
8-bit behavior (`hvspd.log`), so it is not the firmware power/clock interface.

### CRUX CRACKED (2026-09-13): firmware FB ownership gates HVS width

Tested on the live board (10.0.0.2). Issuing the property-mailbox
**`FRAMEBUFFER_RELEASE` (tag `0x00048001`)** flips the HVS from the 8-bit view to
**genuine 32-bit ARM access**: immediately after release the ARM read
`DISPCTRL=0x9A0F00FF` (ENABLE set), `DISPCTRLX0=0x00000000`, `DISPLIST2=0x02340612`
— real register values, not the `0x646472` byte-lane artifact. So the barrier was
never VCHIQ and never a hardware wall: **the firmware owning a console framebuffer
is what pins the HVS to an 8-bit ARM view; release it and the ARM owns the real
32-bit HVS.** This matches mainline vc4 exactly (it releases the firmware FB, then
programs the HVS directly).

**It is transient.** Release-then-only-*read* and the firmware reclaims the idle
display within a second or two (reads revert to `0x646472xx`). The cure is what
vc4 does: **release + immediately DRIVE** — write our dlist and keep an HVS channel
enabled so the display is never idle, so there is nothing for the firmware to
reclaim. Repeated release without a fresh firmware FB is a near-no-op (the FB is a
one-shot; re-owning needs a firmware re-alloc or a fresh boot).

**Consequence — the overlay is the SMALL path, not "path A".** We do NOT need to
own HDMI timing or the pixelvalve (firmware set those up and keeps them). We only:
release the FB, compose a dlist (re-emit the firmware FB plane if we still want it,
+ our scaled YUV plane), point `SCALER_DISPLISTx` at it, enable the channel, and
refresh Y/U/V per frame. That is exactly the mechanical build in
`videocore-hvs-overlay.md`, now unblocked. Reference sequence in `net/hdmi-hvs.lisp`
`rel-fb`-style FRAMEBUFFER_RELEASE + the dlist emit.

### Follow-up probing (2026-09-13, session 2) — timing + a hard operational constraint

Tried to catch a 32-bit window and dump the firmware's live dlist. Learned three
things that shape the build:

1. **The handover is ASYNCHRONOUS.** The `FRAMEBUFFER_RELEASE` mailbox returns
   immediately, but the HVS does not become 32-bit until **~1 second later**, and
   the firmware then **reclaims** it ~1 s after that. So a read in the *same form*
   right after `rel-fb` still sees 8-bit (`0x646472xx`); Run 1 only caught 32-bit
   because its read was in a *separate* ev ~1–2 s later. To catch it you must
   **release, then spin-wait until `DISPCTRL`'s top 24 bits ≠ 0x646472**, then act
   inside the ~1 s window.
2. **Guard every HVS-address dereference.** A dump helper computed a SRAM address
   from a `DISPLACT` slot value and dereferenced it; when that read came back as
   8-bit garbage (`~1.68e9`), the address was ~6.7e9 → `mem-ref` fault → board
   wedged. Bound-check slots (`0 ≤ slot < 4096`) before reading SRAM. With the
   guard the board survives an all-8-bit dump cleanly.
3. **HARD CONSTRAINT: `rel-fb` + lingering-idle destabilizes the RTL8153 network.**
   Every probe that released the FB and then *dawdled* (spin-waiting, multi-read,
   or re-`fbtall`) lost the board's network (ARP FAILED, SSH dead) within seconds,
   needing a full netboot to recover. The firmware's reclaim/reconfigure after an
   idle release appears to disrupt the shared USB/power the RTL8153 rides. **So the
   overlay cannot be built by interactive SSH iteration across the window** — the
   release + drive must be **one atomic in-image routine** that releases, waits for
   the window, writes our dlist, and enables the channel *before* the network/
   firmware disruption matters, and keeps the display driven so nothing reclaims.
   This is the correct architecture regardless (it is exactly what vc4 does).

**Revised build plan:** a single baked `hvs-overlay-on` in `net/hdmi-hvs.lisp`:
release → spin until 32-bit → (read the live dlist ONCE, now that the window is
ours and held) → compose our dlist (FB plane + scaled YUV plane) in unused SRAM →
point `SCALER_DISPLISTx` at it → keep the channel enabled. Because it drives the
HVS the instant the window opens, the firmware never sees an idle display to
reclaim, and there is no SSH round-trip inside the fragile window. Test via a
freshly baked image (or one-shot pushed defun called ONCE), never interactive
multi-step probing.

### Milestone 1 attempt (2026-09-13, session 3) — WE DROVE THE HVS FROM THE ARM

Wrote the atomic release-and-drive routine in `net/hdmi-hvs.lisp`
(`hvs-rel-fb`, `hvs-8bit-p`, `hvs-wait-window`, `hvs-active-channel`, `hvs-fill`,
`hvs-plane`, `hvs-overlay-on`) and ran it on the board. Results:

- **Confirmed on hardware:** a release opens the 32-bit window (`DISPCTRL` reads
  the real `0x9A0F00FF`), and it is **re-triggerable** (not strictly one-shot).
- **WE CHANGED THE HDMI OUTPUT.** After releasing and writing to the HVS at 32-bit,
  the console vanished and the screen showed a **solid color** (webcam-confirmed).
  That is the core capability proven end-to-end: the ARM drives the HVS and it
  reaches the display. (`docs`/scratch webcam grab.)
- **The plane did NOT render yet** — only a solid fill (background) showed, no
  320x180 magenta rect. The unity-plane dlist (`hvs-plane`) needs debugging: the
  CTL0/POS/PTR/pitch layout, the PIXEL_ORDER field, and/or cache coherency of the
  scratch buffer at `0x12000000` (cached DRAM vs the HVS's uncached read).
- **`hvs-wait-window` timing:** 3000 ms sometimes misses — the async handover is
  variable and can take longer; a slower spaced-read sequence caught the window.
  Bump the wait and/or make it more robust.
- **CONFIRMED HARD CONSTRAINT (again):** even a near-atomic drive lost the RTL8153
  network within seconds of the release. The firmware disrupts the shared USB a few
  seconds after release no matter what we do. **Therefore SSH iteration after a
  release is impossible** — the overlay (and its debugging) must run as ONE BAKED
  in-image routine, observed via the webcam, NOT driven step-by-step over SSH.
  Recovery from each attempt is a full netboot (and the RTL8153 sometimes needs a
  physical dongle power-cycle).

**Next:** bake `net/hdmi-hvs.lisp` into the board image (add it to
`build-cl-repl-common.lisp`'s baked sources), have `kernel-main`/`ssh-boot` OR a
one-shot form call `hvs-overlay-on` after boot, netboot once, and read the result
off the webcam. Iterate the plane dlist by rebuilding (reliable) rather than
pushing over SSH. Fix the plane render (coherency: fill via the uncached alias or
add a cache clean; verify CTL0 WORDS/format/pixel-order against a real dumped
firmware plane once we can hold the window with our own dlist).

### Session 3 continued — the window-open is NON-DETERMINISTIC (the real blocker)

Extensive board work with `hvs-drive` (split out of `hvs-overlay-on` so the caller
polls the window instead of an in-routine spin-wait) and a serial-console driver
(the mini-UART is independent of the RTL8153, so it survives a release — the right
way to drive this). Findings:

- **The release → 32-bit-window handover is NOT reliably reproducible.** On the
  boot right after the user's **dongle power-cycle** (recJ), releases opened the
  window repeatedly (`0x9A0F00FF`). On later warm-netboot boots (recN), the window
  **never opened** across many releases — with or without `(ssh-boot)` (USB/net up),
  fresh boot or not. So it is neither USB-init-gated nor a simple one-shot.
- **Leading theory:** a warm U-Boot netboot does NOT cold-reset the VideoCore
  firmware/display block. An earlier **hung `hvs-overlay-on`** (a non-terminating
  `hvs-wait-window` when the timer didn't advance — since hardened with an iteration
  cap) appears to have left the VC in a state where the FB-release handover no longer
  fires, and warm netboots inherit it. recJ worked because the dongle power-cycle
  (closer to a real reset) preceded it. **A full COLD power-cycle of the Zero (not
  just the dongle) is likely required to restore the deterministic-handover state.**
- **`hvs-wait-window` hung the board once** (timer didn't advance → infinite spin →
  no serial echo → wedge). Fixed with a hard iteration cap. Lesson: any on-board
  spin MUST have a non-time bound too.
- **Serial-console parsing gotcha:** the SSH REPL prefixes results with `= `, the
  SERIAL REPL prints the bare value (`12`, `T`, `NIL`) then `> `. A poll parser that
  looked for `= ` silently matched nothing and missed an open window. Parse the bare
  value over serial (`"NIL" in out`).
- **Confirmed twice:** driving the HVS at 32-bit changes the HDMI output (console
  gone → solid fill). We have NOT yet shown a *chosen* color (green) because the
  window has not opened on a boot where the correct drive was queued.

**Where this leaves it.** The ARM-side FB-release handover is real but
non-deterministic on this firmware/rig, which makes interactive milestone work
unreliable. Two ways forward, in order of preference:
1. **Pin the deterministic trigger.** Get a clean state (COLD power-cycle the Zero),
   then on that boot run the baked/serial `rel-fb` + poll(correct) + `hvs-drive` and
   confirm GREEN. If cold-boot reliably opens the window, bake the routine and the
   plane work proceeds. If it is still flaky, investigate the firmware/EDID/HDMI
   state that gates the handover.
2. **VPU-side (`lk-overlay`).** If the ARM handover stays non-deterministic, run the
   display component on the VideoCore VPU where the HVS is natively 32-bit and no
   firmware handover is needed. Bigger, but deterministic.

Also still open (independent of the above): the unity **plane** did not render (only
the background fill) — needs the buffer coherent (no runtime `dc-cvac` primitive
exists; use an uncached buffer region or add a cache-clean) and the dlist
CTL0/format/pixel-order verified against a real dumped firmware plane.

### Session 4 (2026-09-16) — CORRECTION: nothing has rendered; SRAM ≠ registers

Two honest corrections from the user watching the physical monitor:

- **The screen has been DARK the whole time.** The "solid blue/fill" in earlier
  webcam grabs was a camera artifact — "always has been." So the claim that we
  "drove the HVS to a solid color" is WRONG. What actually happens: a release
  BLANKS the firmware framebuffer (screen goes black); we have **never rendered a
  single pixel of our own**. Proven capability so far = FB-release (dark) + a
  transient 32-bit DISPCTRL read + 32-bit dlist-SRAM writes. That's it.
- **The dlist SRAM takes 32-bit writes, but the HVS control registers apparently do
  NOT hold them** — even when DISPCTRL reads 32-bit. Decomposed drive (sd7/sd8): at
  a moment DISPCTRL read `0x9a0c0fff`, a SRAM slot write read back the full
  `0x80000000`, but DISPBKGND (0x44) and DISPLIST (0x20) writes read back
  `0x646472xx` (byte-narrow). If real, this is fatal to the ARM-side path: you can
  build a dlist in SRAM but cannot point a channel at it. CAVEAT: those register
  reads happened ~1.4 s after detection (serial transmission of the drive form),
  so it MIGHT be the window closing mid-form, not a fundamental register-write
  limit. `hvs-regtest` (added to net/hdmi-hvs.lisp) settles it atomically.

- **The window exhausts per boot.** It opens reliably only for the first few
  releases right after a COLD power-cycle (sd6: stable ~10 s). After that,
  releases return no-window (confirmed: `hvs-overlay-on` and 4× `hvs-regtest` all
  `:no-window` later in the same boot). So each definitive test needs a fresh cold
  boot, and the test must be the FIRST release.

**THE ONE DECISIVE TEST:** cold power-cycle the Zero, netboot, push the forms
(pushing defuns does NOT consume the window), then call `(hvs-regtest)` as the
first release. Read `:bg` and `:dl`:
  - 32-bit (e.g. `:bg` = 0x0100FF00-ish, `:dl` = 900) → control-register writes DO
    stick; the ARM-side overlay is viable; remaining work is rendering/coherency.
  - `0x646472xx` → control-register writes never stick in the window → the ARM-side
    FRAMEBUFFER_RELEASE path is a dead end; go VPU-side (lk-overlay) or full vc4.

Serial-drive facts locked in: mini-UART survives a release (RTL8153 dies); the
serial REPL prints BARE values (`42`, `T`, `NIL`) with a `> ` prompt, no `= `
prefix; transmitting a long form costs ~1.4 s (12 ms/char) — which is why detect-
then-drive must be ONE on-board form, never two serial round-trips.

### VERDICT (2026-09-16): the ARM-side FRAMEBUFFER_RELEASE path CANNOT render

After exhaustive board testing with the user watching the physical monitor, the
ARM-side release path is a dead end for actually putting pixels up:

- A release **blanks the display — the screen is DARK, always** (confirmed at the
  monitor; earlier "solid color" grabs were a webcam blue-cast artifact).
- It grants a **transient 32-bit READ of DISPCTRL** (`0x9a0c0fff`/`0x9A0F00FF`) and
  **32-bit dlist-SRAM writes** (a slot reliably holds `0x80000000`).
- But **control-register WRITES never render.** `hvs-rt` and direct probes: writing
  `DISPBKGND` (FILL|green) and `DISPLIST`→(END-only dlist) back-to-back in a
  confirmed-open window leaves the screen DARK. Readbacks always revert to
  `0x646472xx` (a write also appears to close the DISPCTRL read-window: pure reads
  held it 10 s in sd6, the first write reverted it). So we can build a dlist in
  SRAM but cannot commit it (point a channel at it) from the ARM.
- Likely cause: `FRAMEBUFFER_RELEASE` tears down scanout / leaves the channel
  disabled, OR the register-write bus is byte-narrow regardless of the read-window.
  Either way the effect is the same — no ARM-side render. This fits that mainline
  vc4 never uses FRAMEBUFFER_RELEASE to take over; it does a full modeset
  (ioremap + program HVS/PV/HDMI/clocks directly), which the firmware-owned Zero
  does not expose to the ARM the way it does on a KMS-driven Linux.

Also confirmed: the window is non-deterministic and per-boot-limited; the atomic
on-board drivers (`hvs-overlay-on`, `hvs-regtest`) reliably WEDGE the board (a
control-register write inside the routine hangs it / the wait grinds), whereas the
decomposed serial approach (`hvs-rt`, poll-then-fire) does not hang. `wait-window`
now bounds on the BCM system timer (0x3F003004) since get-internal-real-time did
not advance on some boots.

**RECOMMENDATION.** Stop pursuing the ARM-side FRAMEBUFFER_RELEASE overlay. The two
realistic routes to real pixels on the Zero:
1. **VPU-side (librerpi/lk-overlay).** The HVS is natively fully accessible from
   the VideoCore VPU — no firmware handover, no byte-narrow bus. Run a small display
   component there; the ARM feeds it YUV. This is the most promising path and is a
   distinct, sizeable workstream (build/load a VPU program).
2. **Full ARM-side vc4-style modeset.** Reproduce what Linux does — own HVS + PV +
   HDMI + clocks from scratch, without relying on the firmware FB. Largest effort;
   and the byte-narrow-write observation suggests the firmware may not even expose
   the register block writably to the ARM in the firmware-owned boot mode, so this
   may require booting the firmware in a mode that hands the display to the ARM
   (the KMS-equivalent) first.

The 60 fps VP8-on-screen goal is therefore blocked on a VPU-side or full-modeset
effort, not on more mailbox/register poking. The blit-side perf work
(reference_reel_perf_profile) remains independently valid.

### The two real paths (SUPERSEDED — kept for context; the small path above wins)

1. **ARM owns the whole display pipeline** (real vc4-style): boot with the
   firmware relinquishing display, ARM drives HVS + pixelvalve + HDMI. Biggest,
   but it is the "deep VideoCore integration" path and the honest route to 60 fps.
2. **VPU-side display component** (`librerpi/lk-overlay`): the HVS is natively
   32-bit on the VPU. A small VPU program owns the overlay; ARM feeds YUV.

Both are project-sized. Neither is VCHIQ. The immediate next move is the crux
probe, not either big build.

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
