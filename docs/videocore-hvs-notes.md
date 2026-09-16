# Zero 2 W HVS display overlay — investigation notes (2026-09-12)

Goal: play VP8 on the Pi Zero 2 W's HDMI at 60 fps. The frame is ~350 ms (~3 fps)
= ~222 ms decode + ~128 ms blit. The blit (CPU converting YUV→RGB and upscaling
into the scanout in Lisp) is the wrong tool for 60 fps here; the VideoCore's
Hardware Video Scaler (HVS) does colorspace + scale in hardware. The plan: hand
the HVS the decoder's small YUV planes at a stable address, so the display costs
zero per-frame CPU. This doc records how far that got and exactly where it stops.
Register/display-list reference: `videocore-hvs-overlay.md`. Driver + primitives:
`net/hdmi-hvs.lisp`.

### ★★★★ PIXELS ON SCREEN (2026-09-16): write the firmware's LIVE scanout FB

The pragmatic win, cleaner than any HVS-takeover: do NOT send NOTIFY_DISPLAY_DONE.
The firmware then keeps the ENTIRE display pipeline running (HVS + PV + HDMI +
clocks) — `HD_FRAME_COUNT` increments, `PV_V_CONTROL=0x3`. It scans out on **HVS
CHANNEL 1** (not 0): `DISPCTRLX1 (0x50)=0x807804b0` (ENABLE|1920x1200),
`DISPLACT1 (0x34)=0x664`=slot 1636 (that is why driving channel 0 never appeared —
channel 0 is disconnected from the pixelvalve). The active dlist at slot 1636 is a
unity RGBA8888 full-screen plane; its **PTR0 (slot 1636+4) is the live framebuffer
bus address**. Strip the alias (`phys = ptr & 0x3FFFFFFF`) and write 0x00RRGGBB
pixels straight into that DRAM — they appear on the HDMI output IMMEDIATELY. Proven:
a green block drawn at (200,200) showed on the monitor over the U-Boot console.

RECIPE (serial, forms loaded, no notify, no STP, no power pokes needed — the
firmware already has everything on):
  read DISPLACT1 = (mem-ref 0x3F400034 :u32)          ; active dlist slot
  read plane[slot..slot+6] in SRAM at 0x3F402000+slot*4 ; PTR0 = plane[4], pitch=plane[6]
  fb = (PTR0 & 0x3FFFFFFF)                              ; framebuffer phys (pitch 7680, RGBA8888, 1920x1200)
  (hvs-fill (+ fb (* row pitch) (* x 4)) width color)  ; draw
This IS the flat 0x00RRGGBB buffer glass/reel/the whole media stack target — the
display seam is now real.

FULL-SCREEN NUMBERS (same day, BCM system timer, 1920x1200 RGBA = 9.2 MB):
  compiled u32 loop (hvs-fill / baked hdmi-fill-rect)   227 ms  (45 MB/s)  — loop-bound,
       ~100 ns/pixel, NOT memory: the FB (0x1e330000) is in the Normal-WB range, and the
       cached writes leave dirty lines the HVS never sees (red speckle under a green fill;
       %jit-icache-flush cleans to PoU only — the HVS reads at PoC).
  native STP-Q fill + DC CVAC per line (hvs-nfill)      19.4 ms (475 MB/s)
  native LDP/STP-Q copy src->FB + DC CVAC (hvs-ncopy)   52 ms   (176 MB/s), coherent
       edge to edge (striped 9.2 MB source rendered uniformly, zero speckle).
Code: hvs-scanout-fb / hvs-blit-init / hvs-nfill / hvs-ncopy / hvs-frame in
net/hdmi-hvs.lisp (hand-assembled words, exec-page vehicle, X0-X3 + Q0/Q1 only).

★ HARDWARE DOUBLE BUFFER + NON-CACHEABLE BACK BUFFERS — the general fast case
(same day).  Two facts make presenting effectively free:
  * the HVS re-reads its dlist every frame and the live plane's PTR0 word is at an
    8-ALIGNED slot (1636+4 -> 0x3F4039A0), so one 128-bit STP retargets the scanout
    to any coherent buffer at the next vsync: hvs-flip = 1-2 us (orange/stripes/
    orange swaps captured).
  * the raw STP stream is 9.65 ms per 9.2 MB (955 MB/s, the DRAM ceiling; same into
    RAM or the FB) and the DC CVAC clean pass is another 9.7 ms — pure overhead.
    Mapping the back buffer Normal-NON-CACHEABLE removes it: hvs-map-nc rewrites the
    2 MB L2 block descriptors (L1 hardwired at 0x70000, EL2 — MRS TTBR0_EL2 = 0x70000
    confirmed) from AttrIdx0/0x701 to AttrIdx2/0x709 after a DC CIVAC sweep, then
    MAIR_EL2 := 0x4400FF + TLBI ALLE2 + DSB/ISB.  Board stays alive; the NC buffer's
    cyan fill appeared on screen with NO clean, zero speckle.
  NUMBERS (BCM timer):   full 1920x1200        640x360
    NC fill (no clean)      9.75 ms             0.99 ms
    cached -> NC copy      15.5 ms              1.2 ms
    flip                    1 us                1 us
  (DC CVAC on an NC line still costs the full instruction time — 19.4 ms — so never
  use the cleaning routines on NC memory.)  API: (hvs-double-buffer) -> (a b), render
  with hvs-nfill-nc / hvs-ncopy-nc, present with (hvs-flip buf).
  LESSONS: tag every serial read ((list TAG form) + regex) — a lagging echo once
  parsed stale lines as a TTBR and nearly wrote page tables at a garbage address
  (the sanity gate "descriptor == (b<<21)|0x701" is what saved the second run);
  the ')))))' unstick burst makes the NEXT form READ-ERROR, so send a throwaway
  form before probing; MRS Rt is bits[4:0] — D53C2000 is X0, not X1.
★★★★ THE 4:1 CRUX SOLVED + HARDWARE-SCALED PLANE ON SCREEN (same day).
  The byte-narrow HVS writes were the DEVICE WRITE PATH, not the HVS: with the
  HVS's 2 MB block remapped Normal-Non-Cacheable (descriptor 0x405 -> 0x409, same
  MAIR/TLBI as hvs-map-nc) a plain u32 store latches ALL 32 BITS at ANY slot
  (slot 2201 := 0x12345678, read back under Device).  Under Device: u32 -> 1 byte,
  u64 -> 2 bytes, STP -> the low 32 bits of each 64-bit beat (so my "pairs" were
  interleaved: X1 at +0, X2 at +8, odd slots untouched).  READS under NC are
  garbage (four identical words; DISPLACT1 read 0), so: (hvs-window-nc t) ->
  u32 writes (SRAM AND registers, incl. DISPLIST1 at 0x24) -> (hvs-window-nc nil)
  -> verify.  All 28 words (11-word Mitchell-Netravali kernel at slot 2100 + 17-word
  scaled plane at 2000) read back exact; DISPLIST1 := 2000 -> DISPLACT1 = 2000.
  RESULT: a 640x360 RGBA source (red / green band / 80x60 white box / blue) PPF-
  upscaled by the HVS to the full 1920x1200, geometry exact.  Full-screen video is
  now ~1 ms of CPU per frame (render 640x360 into an NC buffer) + a PTR0 flip.
  Plane recipe (vc4_plane_mode_set, non-unity RGB, PPF/PPF): ctl0 = fw ctl0 minus
  UNITY/SIZE/SCL, SIZE=16; pos0 = 0xFF000000|y<<12|x; pos1 = dh<<16|dw; pos2 =
  ALPHA_MODE_FIXED(1<<30)|sh<<16|sw  (alpha mode 0 = per-pixel -> our alpha-0
  pixels would be INVISIBLE); pos3 ctx; ptr0 (0xC0000000|phys); ctx; pitch; LBM 0;
  H-PPF = AGC(1<<30)|((sw<<16)/dw)<<8; V-PPF likewise; ctx; kernel slot x4; END.
  Kernel: coefficients 0,-2,-6,-8,-10,-8,-3,2,18,50,82,119,155,187,213,227 packed
  3 x 9-bit per word (6 words, last = (c15,c15,0)), uploaded as words 0-5 then
  4,3,2,1,0.  Code: hvs-window-nc / hvs-slot-wr32 / hvs-upload-kernel /
  hvs-scaled-plane in net/hdmi-hvs.lisp.  Sources: Linux v6.6 vc4_plane.c /
  vc4_hvs.c / vc4_regs.h.

## Board transport lessons (2026-09-16, the reel-on-HVS attempt — read before
## driving the Zero again; each of these cost a power cycle)
* `pkill -f netboot-gz` inside a `bash -c "... python3 netboot-gz.py ..."` kills
  the launching shell itself — the pattern matches the shell's OWN argv (the
  python invocation text is in it).  Bracketing `[n]etboot` does NOT help there.
  Anchor: `pkill -f "^python3 -u netboot"`, or launch with a plain
  `(setsid nohup python3 -u netboot-gz.py … &)` and never pkill in the same
  command.  Symptom: "netboot never starts", empty logs.
* Two readers on /dev/ttyAMA0 = the other one eats the bytes (pyserial then
  raises "device reports readiness to read but returned no data").  netboot-gz
  holds the port after `go` (send-delay); stop it before reading the boot trace.
  A host-side background task that gets memory-killed mid-write leaves U-Boot
  with a HALF-TYPED `go 0x300` — finish it with `000⏎` (the image is already
  unzipped at 0x300000).
* U-Boot autoboots the STALE SD image ("Hit any key to stop autoboot: 0") if the
  CR spam misses the window; the monitor shows "64921864 bytes read … Starting
  application" and netboot prints nothing.  Just re-run netboot (RUN-pin reset).
* Serial REPL prints BARE values; the SSH exec path prints `= value`.  A regex
  written for one silently fails on the other.  Tag every serial read:
  `(list TAG form)` → `\(TAG (.+?)\)`.  The `)))))` unstick makes the NEXT form
  READ-ERROR — send a throwaway `(+ 0 0)` before probing.
* "Never send input during boot": a second `go` injected while Modus boots
  wedges the reader; nothing recovers it but a reboot.  An idle REPL prints
  nothing on a passive read — empty ≠ dead; send-then-read is the only test.
* `*jit-hot-only* NIL` BEFORE `net-install-and-call` makes the reel install
  eager-JIT everything: 16 MINUTES on the A53 (and the r8152 dies meanwhile).
  Install at the default, flip the flag only for timing loops.
* THE NETWORK: outbound TX from the r8152 works ONLY under ssh-boot (ping,
  fetch, SSH all fine).  With the same driver bound (`usb-netdev-get`=2, MAC
  right, `e1000-send`→1) at the serial REPL, NOTHING reaches the wire (tcpdump
  on modus-pi eth0: zero packets) — ARP never resolves, every fetch `TCP:F`.
  Tried and ruled out: static IP/GW byte order (state+0x1C is LE bytes, the
  `GW=1.0.0.10` print is the printer's quirk), ssh-seed-random, zeroing the
  ssh-ipc/conn regions, priming with 600 receive polls, cdc-ether-init vs
  net-usb-probe (cdc "succeeds" but is the wrong driver; net-usb-probe only
  exists in MODUS_SSH_BUILD=1 images), a second net-usb-probe (→ NOTFOUND: the
  device is already claimed; `(usb-netdev-set 2)` restores the binding).  Still
  unknown what in ssh-boot/net-actor-main enables TX — chase with tcpdump +
  bisecting ssh-boot's body next time.  Build knobs added: MODUS_NET_STATIC=1
  (pipeline uses 10.0.0.2/10.0.0.1 instead of DHCP) and the pipeline adopts via
  net-usb-probe in SSH builds (*net-rpi-nic-init*).
* ssh-boot's single-connection server WEDGES on an interactive (-tt) shell
  channel (ping ok, port 22 times out) — use exec-mode `ssh host "(form)"` per
  form; `net-actor-main` polls between calls so the NIC stays alive.  A long
  synchronous eval (the install) stops all polling: no ping replies until it
  returns.
* Working demo flow: demo9 (NET+SSH build, MODUS_NET_NOAUTO=1) → netboot →
  `(ssh-boot)` over serial → exec-mode forms (rh_run.py / rh_session.sh on
  modus-pi; hvs-all-forms.txt = flatten2.lisp of net/hdmi-hvs.lisp).
  BUT the reel install over SSH is ~16 min on the A53 at DEFAULT hot-only too
  (measured 953 s, then No route to host) — the NIC never survives it.
* THE ROUTE THAT WORKS: the CORE.  Run the exact board kernel under
  `qemu-system-aarch64 -M raspi3b -kernel tramp.bin -device loader,file=
  kernel8.img,addr=0x300000 -device loader,file=reel.tar,addr=0x1A000000
  -device loader,file=small.ivf,addr=0x1B000000 -serial null -serial
  unix:q.sock,server,nowait -s` (tramp.bin = `MOVZ X1,#0x30,LSL#16; BR X1` =
  D2A00601 D61F0020), drive the SECOND serial over the unix socket
  (scratchpad qinstall.py/qsave.py): ramv the tarball from RAM,
  install-tarball-from-bytes, push the 71 HVS/demo forms, ramv the clip into
  *rh-ivf*, `(%save-image "x")`, then gdb-multiarch `dump binary memory` of
  [0x18000000, +size).  Netboot with netboot-core-gz.py (--core: tftp the core
  to 0x18000000 instead of the `mw.q` clear) → `CORE-RESTORED` in ~2 s, REEL +
  forms + clip present at a serial REPL, NO network at all.  Gotchas:
  - under QEMU `CORE-END=` printed the byte COUNT (4103084 for a 7.65 MB
    core); on the board it prints the cursor (0x18000000+size).  Trust neither:
    dump 16 MB and trim at the last non-zero byte (that RAM starts zeroed).
    Header fields are stored <<1 (from 0x09000000, live 7.5 MB).
  - A core saved with the JIT OFF restores every defun as BYTECODE: they run
    in the interpreter, whose bare-metal arm of the exec-page trap ECHOES ITS
    ARGUMENT — `(rh-init)` got buffers at 0x200000 (= the size) and the first
    frame wrote over low memory (board dead).  A top-level `(%mmap-exec-page
    4096)` (JIT'd form) works fine.  Save with `*jit-on* T` (the Pi core
    carries the arena: %core-jit-lossy-p → NIL) so reel + the HVS forms are
    native — also the only way the decode timing means anything.
  - Wait for "Modus CL REPL" before sending ANYTHING to the QEMU serial; a
    reader left mid-form echoes input but never evaluates (parens burst fixes).
  - Host-side `ser.write(b"unzip 0x08000000 …")` bursts DROP CHARACTERS into
    U-Boot ("unzip 0x00", "go 0x300"): pace every byte (4 ms) — patched into
    netboot-gz.py / netboot-core-gz.py.

★★★★ 60 Hz ON SCREEN, MEASURED (same day): two NC 640x360 buffers, per frame
  fill (0.84 ms) -> u32 write of the scaled plane's PTR0 (slot 2005, window NC)
  -> spin on HD FRAME_COUNT (0x3F808068).  300 frames = 5.002 s, 600 = 10.005 s
  (59.97 fps), FRAME_COUNT +1 per frame = zero drops.  Camera video tiled at
  0.5 s spacing shows the colour sweep.  Code: hvs-vsync / hvs-anim.  The display
  side of "VP8 at 60 fps on the Zero 2 W" is DONE: budget per frame ≈ 16.7 ms
  minus 0.84 ms render = ~15.8 ms for the decoder to produce a 640x360 frame into
  the back buffer (YUV->RGB in the HVS is the next optimisation: a YUV420 plane
  drops even the CPU colour conversion). The HVS hardware-scaled overlay (compose our own plane
on channel 1's dlist, or add a scaled YUV plane) remains the path to zero-CPU
scaling, but is no longer on the critical path to "pixels up".

CAMERA: continuous autofocus + auto-exposure made every earlier grab unreadable.
Fixed values that read the console text crisply: focus_automatic_continuous=0
focus_absolute=55, auto_exposure=1 (Manual) exposure_time_absolute=300 gain=48
backlight_compensation=0. (v4l2-ctl -d /dev/video0 -c ...)

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

### Session 5 (2026-09-16) — "what would Linux do?": the REAL handoff, and the last wall

The VERDICT below is REVISED by this session. Asking what Linux actually does
(reading `vc4_drv.c`, not just `vc4_hvs.c`) found the step we had never sent:

- **`RPI_FIRMWARE_NOTIFY_DISPLAY_DONE` (property tag `0x00030066`, zero-length
  payload).** `vc4_drm_bind()` sends it (after `drm_aperture_remove_framebuffers`,
  BEFORE binding the HVS) to tell the firmware the ARM is taking over the display.
  `FRAMEBUFFER_RELEASE` only frees the buffer — the firmware's own VPU display
  service keeps running and re-asserts, which is exactly the "transient window that
  closes on write" we measured. After NOTIFY_DISPLAY_DONE: `DISPCTRL` reads
  `0x9a0c0000` STABLY (low 12 bits — the firmware's irq/status enables — cleared)
  and **no longer reverts on a write**. The firmware is quiescent. (Tag resp word
  read 0/0xAAAAAAAA — don't trust that; the hardware effect is unambiguous.)
- **Then power+clock bring the register file alive.** Earlier negative results for
  SET_DOMAIN_STATE/SET_CLOCK_STATE were taken while the firmware was still fighting.
  Post-notify: VIDEO_SCALER domain (mbox id 3) + CORE clock (4) → every HVS register
  reads a clean 32-bit value (the `0x646472xx` "ddr" filler is GONE; e.g. 0x44 →
  `0x00000000`, 0x20 → `0x00000084`). HDMI domain (5) → the pixelvalve
  (`PV2_CONTROL` `0x70697805` "pix"-filler → real `0x177005`, EN set) and HDMI core
  (`0x0` → `0x600`) come alive too. So the `0x646472`/`"pix"` pattern = an
  UNPOWERED/unclocked block's read signature [byte][3-char block tag], not a bus
  keeper.
- **THE LAST WALL — write width.** With everything quiescent and powered, ARM
  WRITES to any HVS register latch ONLY byte 0: `DISPCTRL` (`0x9a0d00ff` → upper
  byte unchanged), `DISPCTRLX0` (`0x80780438` → `0x38`), `DISPLIST0` (`900`/0x384 →
  `0x84`), `DISPBKGND` (`0x0100ff00` → `0x0`). Uniform, and unchanged by every
  firmware lever (notify; domains 3/5/7/10; clocks 4/9/5; SET_DISPLAY_POWER).
  Byte-wise `:u8` stores to lanes 1–3 don't latch either (`:u8` IS a real STRB —
  verified in translate-aarch64 — though the primitive showed a lossy RMW quirk on
  SRAM). Meanwhile the dlist SRAM at +0x2000 — SAME 2 MB MMU block, SAME attribute,
  SAME instruction — takes full 32-bit writes. V3D never woke (`0xDEADBEEF` even
  with domain 10 + clock 5), so it couldn't serve as a second-block discriminator.
- Reads are full-width, so `DISPCTRL` bits 8–11 reading back `0x0` after a write
  of `0xf` are just read-only status bits — real register behaviour, not narrowness.
- Monitor: pure black with no backlight after the handoff (scanout stops; Linux
  re-enables PV+HDMI). We powered PV/HDMI; they read real values.

**Diagnosis of the wall.** Same store instruction → full word into the SRAM, byte 0
into the register file 0x2000 bytes away. That is the VC-side SLAVE treating the
identical transaction differently — and the two slaves are different kinds: the
dlist SRAM is a plain memory slave, the register file sits behind an APB bridge. A
bridge can legitimately downsize a STRONGLY-ORDERED write (`Device-nGnRnE`, which
boot-rpi-cl.lisp uses: MAIR attr1 = 0x00) while passing a normal device write
(`Device-nGnRE`, MAIR 0x04 — what Linux `ioremap` uses) full-width. That single
attribute is the LAST structural difference between our write and Linux's at the
moment it hits the bus, and it explains every observation (mailbox and SRAM work:
different slaves). It cannot be changed at runtime (needs `msr mair_el2`; no Lisp
primitive) → **NEXT TEST: rebuild with MAIR attr1 = 0x04 (nGnRE) and rerun
notify → power/clock → register write.** If writes go full-width, the ARM-side
overlay is UNBLOCKED (the dlist is already word-writable; PV/HDMI are powered). If
still byte-0, the remaining candidates are EL2-vs-EL1 or a genuine ARM-aperture
limit → VPU-side.

Recipe that now gives a stable, alive HVS (all over serial; forms in
net/hdmi-hvs.lisp + a cloned `hvs-notify-done` = rel-fb with tag 196710):
`(hvs-rel-fb)` → `(hvs-notify-done)` → `(mb2 229424 3 3)` `(mb2 229377 4 1)` →
(optional `(mb2 229424 5 3)` for PV/HDMI) → registers read real 32-bit.

**VALIDATED (session 5, later): the write wall is an exact 4:1 downsizing.** With the
HVS alive, a 64-bit store latches exactly 2 bytes where a 32-bit store latches 1 —
confirmed with PREDICTED values on two registers: `(setf (mem-ref DISPLIST0 :u64)
450)` (`:u64` stores the tagged word, 450<<1 = 0x384) read back exactly `0x384`;
`(setf (mem-ref DISPCTRLX0 :u64) 21845)` (→0xAAAA) read back exactly `0xaaaa`. The
bridge keeps the LOW QUARTER of each write. Consequences: (1) a 128-bit store
(`STP Xt1,Xt2` — `a64-stp-offset` exists in translate-aarch64) should latch a full
32-bit register even under nGnRnE — a fallback primitive if the attribute isn't the
cause; (2) 16-bit writes already suffice for `DISPLIST` (slot < 4096) but NOT for
`DISPCTRLX.ENABLE` (bit 31) or `DISPBKGND.FILL` (bit 24), so a render needs one of
the two 32-bit paths. Wide stores are NEIGHBOR-SAFE: a `DISPLIST1` sentinel (0x55)
survived a 64-bit store to `DISPLIST0` — dropped upper bytes are discarded, not
zeroed. GOTCHA: `:u64` stores to Device memory MUST be 8-byte aligned — a 64-bit
store to 0x24 was a silent alignment fault that wedged the board (0x20/0x40 fine).

**Bring-up sequencing that works reliably** (a notify sent a fixed 1 s after the
release, without waiting, failed once): `(hvs-rel-fb)` → POLL `DISPCTRL` until it
leaves `0x646472xx` (the async handover; poll 0 sometimes, ~1.2 s typical) →
`(hvs-notify-done)` → `(mb2 229424 3 3)` → `(mb2 229377 4 1)` → registers alive.

**Two images now exist:** `board-demo6.img.gz` (nGnRnE, the one all the above was
measured on) and `board-demo7.img.gz` (built 2026-09-16 from the same tree with
boot-rpi-cl.lisp MAIR attr1 = 0x04 / Device-nGnRE; `mvm/build-rpi-cl-repl.lisp`,
MODUS_NET_BUILD=1 MODUS_RPI_MINIUART=1, no SSH — serial-driven; 63,715,600 bytes,
`tmp/piboot/kernel8.img`). The decisive test = the same bring-up + a plain 32-bit
register write on demo7: full 32 bits → the attribute was the cause; still 8 bits →
build the STP primitive.

**nGnRE REFUTED (demo7).** The Device-nGnRE image boots and drives the board
identically, and with the register file reading clean a u32 write of `0x184` to
`DISPLIST0` still read back `0x384` (byte 1 untouched); `DISPCTRLX0 ← 0x80780438`
read `0x38` with visible zero upper bytes; u64 → `0xaaaa` (4:1 intact). The
memory attribute is NOT the cause — the 4:1 downsizing is the bridge's own
behaviour. boot-rpi-cl.lisp reverted to nGnRnE (comment records the experiment).
Note: right after bring-up the non-DISPCTRL registers can transiently read the
`0x646472` filler for a few seconds before going clean — re-read before judging.

**Alignment corollary of the 4:1 model.** Device memory faults on misaligned
accesses (a u64 store to 0x24 silently wedged the board), so a register at a
4-mod-8 offset (0x04, 0x0c, 0x24, 0x2c, 0x34, 0x44 = DISPBKGND, 0x4c, 0x54, 0x64)
can ONLY ever be reached by a 32-bit store → 8 bits. `DISPBKGND.FILL` (bit 24) is
therefore unreachable from the ARM: **background fill is out; render colour with a
PLANE**, whose whole descriptor (CTL0/POS/PTR/pitch) lives in the fully
word-writable dlist SRAM. The two registers a render needs are both 8-aligned:
`DISPLIST0` (0x20; slot < 4096 fits the 16 bits a u64 gives) and `DISPCTRLX0`
(0x40; needs bit 31 → a 128-bit store).

**The 128-bit vehicle, no rebuild:** `(%mmap-exec-page 4096)` → poke raw
AArch64 words with `(setf (mem-ref … :u32) w)` → `(%jit-icache-flush p 4096)` →
`(%jit-call p)`. Hand-assembled routine (args via a scratch block at 0x12000000
= [addr,0,lo,0,hi,0] as u32s): `MOVZ X3,#0x1200,LSL#16` (D2A24003); `LDR X0,[X3]`
(F9400060); `LDR X1,[X3,#8]` (F9400461); `LDR X2,[X3,#16]` (F9400862);
`STP X1,X2,[X0]` (A9000801); `RET` (D65F03C0). Prediction from 4:1: STP at 0x40
latches `DISPCTRLX0` in full (low 32 of the 128-bit write); bytes 4-15 dropped, so
`DISPBKGND` (0x44) untouched. A proper primitive later = a pair-store width in
`a64-str-width` (STP helpers already exist) plus compiler plumbing for two values.
Pixel-buffer coherency for the plane: no dc-cvac primitive, but `%jit-icache-flush`
does `DC CVAU` (clean to PoU) over a range — try PTR0 via the L2-cached VC alias
(0x40000000|phys) + that clean, instead of the uncached 0xC0000000 alias.

### ★★★ BREAKTHROUGH (2026-09-16): a 128-bit STP latches a FULL 32-bit HVS register

The 4:1 extrapolation was right. Via the exec-page vehicle above, the hand-assembled
`STP X1,X2,[X0]` at `DISPCTRLX0` (0x40) took it from `0xaaaa` to **`0x80780438`**
(ENABLE | 1920x1080) — all four bytes — with pre/post markers in the scratch block
proving the routine ran straight through the STP (no bus abort), and `DISPBKGND`
(0x44, the dropped upper quarter) untouched. **The ARM CAN fully program every
8-aligned HVS register.** Channel 0 is enabled and scanning our dlist.

**GOTCHA that cost several attempts: the JIT-exec traps have NO interpreter arm.**
`%jit-icache-flush` / `%jit-call` typed at the REPL are evaluated by the
INTERPRETER (the JIT is hot-form only, `*jit-hot-only*` T) — the compiled MVM op
sequence runs, but the `:trap` does nothing there, so the flush returned its base,
`%jit-call` returned 0, and a `MOVZ X0,#0x1234; RET` callee "didn't run".
`(setq *jit-hot-only* nil)` makes every form JIT-compile (native trap executes;
`*jit-fallback-count*` stayed 0) and it all works. `%mmap-exec-page` works either
way because the JIT machinery itself calls it natively (its bump pointer advances
with every hot form, which is why my consecutive pages weren't 4 KB apart).
(`%mmap-exec-page` result printed as `4096` once = my parser catching the echoed
argument; the real pages are in [0x14000000, 0x18000000).)

**Working recipe for a full 32-bit register write (demo6 OR demo7, serial):**
`(setq *jit-hot-only* nil)`; `p=(%mmap-exec-page 4096)`; poke words
`D2A24003 F9400060 F9400461 F9400862 A9000801 D65F03C0` (MOVZ X3,#0x1200,LSL#16;
LDR X0,[X3]; LDR X1,[X3,#8]; LDR X2,[X3,#16]; STP X1,X2,[X0]; RET) with
`(setf (mem-ref (+ p 4i) :u32) w)`; `(%jit-icache-flush p 4096)`; write the scratch
block at 0x12000000 = [addr,0,lo,0,hi,0] as u32s; `(%jit-call p)`. `lo` lands in
the 8-aligned register at `addr` in full. Proper primitive to follow: a pair-store
width in `a64-str-width` (STP helpers exist) + compiler plumbing (`%setf-mem-ref`).

**The hangs, explained.** Every "mysterious" wedge this session (`hvs-drive`,
`hvs-overlay-on`, the plane attempt) ran a 230 KB `hvs-fill` at 0x12000000 or
0x12100000. That range is NOT free: `build-cl-repl-common.lisp` places
`percpu-data-base` at 0x12000000 and `sched-lock-addr` at 0x12000200 — the
actor/SSH address map (per-CPU data, scheduler lock, actor stacks, crypto
scratch) occupies 0x12000000–0x16000000. The fill stomped the runtime. Rule:
**every buffer and scratch block comes from `(%mmap-exec-page n)`** (the JIT
arena [0x14000000, 0x18000000), guaranteed free, Normal-WB); compute the STP
routine's scratch address into `MOVZ X3,#hi16,LSL#16 ; MOVK X3,#lo16` instead of
hard-coding 0x12000000.

Remaining to first pixels: a PLANE in SRAM (word-writable) → a coherent pixel
buffer (no dc-cvac primitive, but `%jit-icache-flush` = DC CVAU to PoU; point PTR0
through the L2-cached VC alias 0x40000000|phys) → and scanout: the monitor showed
no backlight after the handoff, so PV2 (0x3F807000) / HDMI (0x3F902000) may need
re-enabling — both 8-aligned, so STP-writable the same way.

### Scanout after display-done: what the firmware tears down, and the mode

With the HVS channel enabled and a correct plane in SRAM (descriptor reads back
word-for-word; `DISPLIST0` swapped), `DISPLACT0` still stayed 0 — the HVS only
advances a channel when the downstream pixelvalve pulls, and the whole scanout
chain had been shut off by NOTIFY_DISPLAY_DONE. Inventory (all measured):

- **Pixel clock OFF:** firmware clock 9 (PIXEL) `GET_CLOCK_STATE`=(9 0),
  `GET_CLOCK_RATE`=(9 0). vc4's first act is `clk_set_rate`+enable through the
  firmware clock driver. `SET_CLOCK_RATE` = tag 0x38002 (3 words: clock, rate,
  skip_turbo) → `(mb3 229378 9 RATE 0)` turns it on (state → (9 1)).
- **The mode is 1920x1200, not 1080p.** The firmware's PV timings are intact:
  `PV_HORZA=0x500020` (HBP 80, HSYNC 32), `PV_HORZB=0x300780` (HFP 48, HACTIVE
  1920), `PV_VERTA=0x1a0006` (VBP 26, VSYNC 6), `PV_VERTB=0x304b0` (VFP 3, VACTIVE
  1200) → 2080x1235 total → **154 MHz** at 60 Hz (CVT-RB 1920x1200), not 148.5.
- **Pixelvalve gated:** `PV_V_CONTROL=0x2` (CONTINUOUS, VIDEN clear);
  `PV_CONTROL=0x177005` (EN | clk_select=HDMI | WAIT_HSTART|TRIGGER_UNDERFLOW|
  CLR_AT_START | fifo/format) left configured. vc4 order: PV_CONTROL with
  FIFO_CLR → |= EN → PV_V_CONTROL |= VIDEN. VIDEN/EN/FIFO_CLR are bits 0-1 → the
  byte-0 u32 write reaches them even though PV_V_CONTROL is at a 4-mod-8 offset.
- **Two register blocks** on VC-IV: the HDMI core at 0x7e902000 (ARM 0x3F902000:
  SW_RESET_CONTROL 0x004, FIFO_CTL 0x05c, RAM_PACKET_CONFIG 0x0a0,
  SCHEDULER_CONTROL 0x0c0, TX_PHY_RESET_CTL 0x2c0, TX_PHY_CTL0 0x2c4) and the
  **HD block at 0x7e808000 (ARM 0x3F808000: M_CTL 0x00c, VID_CTL 0x038,
  CSC_CTL 0x040)**. "HDMI+0x38" is the wrong block for VID_CTL.
- **Encoder cleared** (matches vc4_hdmi_encoder_disable): `HD_VID_CTL` had ENABLE
  clear (read 0x100000), `SCHEDULER_CONTROL=0xcb028` (MODE_HDMI bit 0 clear,
  HDMI_ACTIVE bit 1 clear; MANUAL_FORMAT bit 15 + IGNORE_VSYNC_PREDICTS bit 5
  still set), `RAM_PACKET_CONFIG` not enabled. Re-enable: `VID_CTL ←
  ENABLE|UNDERFLOW_ENABLE|FRAME_COUNTER_RESET|CLRRGB|BLANK_INSERT_EN = 0xE0C00000`
  via STP (reads back 0xc0880000 — FRAME_COUNTER_RESET self-clears, bit 22 RO,
  bit 19 status), `SCHEDULER_CONTROL |= MODE_HDMI` (byte-0 write), then wait
  `HDMI_ACTIVE`, then `RAM_PACKET_CONFIG ← 0x10000` (bit 16 → STP).
- **PHY held in reset and range-powered-down:** `TX_PHY_RESET_CTL=0x1ff`,
  `TX_PHY_CTL0=0x8e000000` (bit 25 = RNG_PWRDN set). vc4 phy_init: RESET_CTL ← 0xf,
  udelay, ← 0 (byte-0 writes suffice). phy_rng_enable clears CTL0 bit 25 — BUT
  TX_PHY_CTL0 is at 0x2c4 (4-mod-8): unreachable by u32 (byte 0 only) and by STP
  (it is the dropped hi word of an STP at 0x2c0). Candidate: a NEON `ST1 {V0.4S},
  [X0]` — a 16-byte transaction whose element alignment is 4, legal at 0x2c4, whose
  low quarter is exactly the four bytes of CTL0. Untested; only needed if
  HDMI_ACTIVE refuses to assert with RNG powered down.
- Also in vc4's pre-configure: `SW_RESET_CONTROL ← HDMI|FORMAT_DETECT (0x3) then 0`;
  `HD_M_CTL ← SW_RST (0x4), 0, ENABLE (0x1)`; `FIFO_CTL |= MASTER_SLAVE_N (bit 0)`.
- Webcam note: the "blue screen" was **U-Boot's console framebuffer** (blue
  background, white text, U-Boot logo) persisting into Modus until `rel-fb` blanks
  it — not a camera artifact. Pure black with no backlight glow = no signal.

**The actual scanout blocker: the HSM clock.** `CM_HSMCTL` (0x3F101088) and
`CM_HSMDIV` (0x3F10108c) both read 0 after display-done — the HDMI state-machine
clock is stopped, so `HDMI_ACTIVE` can never assert and `HD_FRAME_COUNT` stays 0
regardless of encoder/PV/PHY state. vc4 programs it directly in CPRMAN
(clk-bcm2835: parent PLLD_PER = 500 MHz, target 163,682,864 Hz → DIVI 3, MASH 1).
There is no firmware mailbox clock id for HSM. Negatives on the way here:
`SET_DISPLAY_POWER` (0x48019) is accepted for display 0 and does nothing (display
2 → invalid); a 16-byte NEON `ST1 {V0.4S}` at the 4-mod-8 `TX_PHY_CTL0` RAN (pre/
post markers) but latched nothing — NEON stores are not a way to reach 4-mod-8
words. CPRMAN writes need the 0x5a password in bits 31:24; for the 4-mod-8
`CM_HSMDIV` the plan is a `STP W1,W2` (a 64-bit transaction legal at 4-byte
alignment; low quarter = 16 bits, enough for DIVI=3|DIVF) with 0x5a in the
unlatched top byte in case the password check is combinational on the data bus.
Note `PHY RNG_PWRDN` (CTL0 bit 25) is cleared by vc4 in post_crtc_powerup, i.e.
AFTER the HDMI_ACTIVE wait, so it is not what gates ACTIVE.

**CORRECTION: CPRMAN is INVISIBLE to the ARM here.** `CM_VPUCTL`, `CM_VPUDIV`,
`CM_PERIACTL`, `A2W_PLLD_CTRL`, `A2W_PLLD_PER` ALL read 0 — impossible on a running
system (the VPU runs the firmware on that clock) — and writes (STP-X full-width,
STP-W, u32 byte-0, with the 0x5a password) leave `CM_HSMCTL` at 0. So "HSM = 0"
was never a measurement: the ARM has no access to the clock manager in this boot
state and cannot program HSM at all. Under a Linux KMS boot the ARM does reach
CPRMAN (clk-bcm2835), so this is another firmware-state difference — unresolved.

**What actually kills scanout is NOTIFY_DISPLAY_DONE**, not the release: after
`rel-fb` alone the webcam still showed backlight (a black frame = signal present);
only after the notify did it go to no-signal. The notify was only ever sent to stop
the firmware "fighting" HVS writes — and the fight was largely the unpowered-block
read signature + the bridge write-width, both since explained. **Next experiment
(fresh boot, NO notify):** `rel-fb` → VIDEO_SCALER domain + CORE clock → check the
HVS reads real → build the plane in SRAM → STP `DISPCTRLX0` enable (the firmware
disables channel 0 on release: it read 0x00000000 right after the first release)
→ u64 `DISPLIST0` → watch `DISPLACT0`/`HD_FRAME_COUNT` and the monitor, and watch
whether the firmware re-powers the block down or re-writes the channel.

**TOOLING LESSON — "echo but no eval" is usually a STUCK READER, not a wedge.**
Several recent "board hangs" (serial echoes the line, prints no value, no `> `)
were the REPL reader sitting inside an unterminated form: input sent while the
kernel was still booting (after the `MODUS-CL` banner but before the prompt) or a
half-delivered form from a killed script leaves an open paren, and every later
line just extends it. Sending a burst of `)))))))` produced
`ERROR: UNDEFINED-FUNCTION ... > ` and the REPL answered again immediately. Rules:
(1) after a boot, wait for the actual `> ` prompt (not the banner) before sending;
(2) before declaring a wedge, send `)))))))\r\n` then probe `(+ 21 21)`; (3) run
long board sequences as a DETACHED runner on modus-pi (`nohup setsid script`) that
logs to a file, and poll the file — a killed ssh loses stdout and half-delivers a
form. Real faults do exist (e.g. a misaligned u64 store), but check the reader
first. Also: the webcam runs continuous autofocus; set
`focus_automatic_continuous=0` and a fixed `focus_absolute` for readable text.

### VERDICT (2026-09-16, REVISED ABOVE — kept for the record): the ARM-side FRAMEBUFFER_RELEASE-ONLY path cannot render

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
- **modus-pi's SD root fills up** (it hit 100% / 2.5 MB free on 2026-09-16). The
  failure signature is subtle: `scp` reports a bare "write remote: Failure" but
  leaves a file of the EXPECTED SIZE that is nonetheless corrupt (`gzip -t` →
  "invalid compressed data"), and `netboot-gz.py` dies at its first `print`
  (`OSError: No space left`) leaving a 0-byte log and no process — which looks like
  a mysterious launch failure. ALWAYS `gzip -t` / md5-verify an uploaded image
  before it goes into `/srv/tftp`, and upload with a `cat` pipe (clean error) rather
  than scp. Freed 65 MB by deleting the RAW `/srv/tftp/board-demo.img` (its exact
  `.gz` twin is what netboot uses); `board-nojit.img` + `board-ql-nojit.core`
  (quickload milestone), `board-hdmi.img`, `reel-demo.mp4` deliberately kept. `/tmp`
  on modus-pi is tmpfs — usable as RAM staging.
- Images: `board-demo6.img.gz` (nGnRnE, all the register-width measurements),
  `board-demo7.img.gz` (Device-nGnRE, MAIR attr1=0x04; serial-only, no SSH;
  built from `mvm/build-rpi-cl-repl.lisp` with MODUS_NET_BUILD=1
  MODUS_RPI_MINIUART=1 MODUS_CL_REPL_OUT=tmp/piboot/kernel8.img; gzip -9; md5
  3c968af84b515d8bd19070281873ff3a). Serial-driven workflow: netboot with
  `--send-delay 6` (no `(ssh-boot)`), kill netboot-gz once "MODUS-CL" shows, then
  drive /dev/ttyAMA0 directly (bare-value REPL output, no `= `).
- HVS probes are RUNTIME-PUSHED (only use `mem-ref` + baked mailbox helpers), no
  rebake: push `net/hdmi-hvs.lisp`'s defuns, then `(hvs-display-on)` `(hvs-dump)`.
- Image build (has HVS reachable, mailbox, USB net): branch `hdmi-on-main`
  (HDMI-display commits cherry-picked onto main) via `cabfs/core/build-board.sh`
  (12 GB SBCL build), gzip → `board-demo6.img.gz`, put in `/srv/tftp` on modus-pi.

★★★★ REEL FRAMES ON THE HVS-SCALED PLANE — THE WHOLE PIPELINE (2026-09-16, late)
  Core route (see "THE ROUTE THAT WORKS" above), native core built with
  `*jit-on* T` + `*jit-hot-only* NIL` under QEMU (9.5 MB: reel + 71 HVS/demo
  forms + small.ivf; 3.4 MB of native code), `netboot-core-gz.py --core
  reel-native2.core --img board-demo9.img.gz` → CORE-RESTORED in ~2 s, then over
  serial (docs/reel-on-zero/rh_core3.py):
    (rh-init)         -> two NC 2 MB buffers in the arena above the restored code
    (rh-first-frame)  -> (320 180 384 192 0): decoded 320x180, Y stride 384,
                         chroma stride 192, planes copied into the NC buffer and a
                         YUV420 3-plane PPF plane switched in — ON SCREEN
                         (docs/reel-on-zero/first-frame-on-hvs.jpg)
    (rh-play nil)     -> (FRAMES 30 TOTAL-MS 14547 DECODE-MS 14538 COPY-MS 7 FPS 2)
    (rh-play t)       -> (FRAMES 30 TOTAL-MS 14858 DECODE-MS 14602 COPY-MS 7 FPS 2)
  DISPLAY COST: 0.23 ms/frame (three plane copies + three PTR writes) — the HVS
  does colour conversion and the 6x upscale; the display side of "VP8 at 60 fps"
  is closed.  DECODE: 485 ms/frame on this core — the entire budget, and 2x
  SLOWER than the 224 ms/frame measured on 2026-09-10 with reel JIT'd on the
  board (board-demo4, 90-frame clip).  *jit-native-count* 311, fallback 1, so it
  is native; suspects: different clip, QEMU-side JIT missing a runtime switch,
  or the core's global-type promises.  NEXT: profile/bisect that 2x, then the
  A53 decode campaign (the 224 ms itself is 13x off 16.7 ms).
  Traps that each cost a boot: a core saved JIT-OFF has bytecode defuns whose
  interpreter-arm %mmap-exec-page ECHOES its argument (buffers at 0x200000 →
  wrote over low memory); a core-native caller NEVER sees an on-board
  redefinition (rebuild the core, don't mix units); `hvs-rd` compiled under QEMU
  faults on `(+ (hvs-base) #x34)` (address left tagged: FAR 0x7e800034) while
  stores at computed addresses are fine — the trailing DISPLACT1 read was
  dropped from rh-yuv-plane; resetting the arena bump after pushing forms
  overlays the code you just compiled; the 9.5 MB core TFTP outruns a 60 s
  wait — poll for "go", never kill the netboot on a timer.
