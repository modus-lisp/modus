# VideoCore IV HVS scaled-YUV overlay (Pi Zero 2 W, BCM2710)

Goal: composite a hardware-scaled YUV video plane (the decoder's small YUV420
planes) onto the native-res HDMI scanout with **zero per-frame CPU pixel work**,
so 60 fps "pixels on screen" is a VideoCore job, not the A53s. The HVS
(Hardware Video Scaler) does colorspace conversion + scaling in hardware; we
hand it a stable buffer address via a display list.

Peripheral base `0x3F000000`. **SCALER_BASE = 0x3F400000** (bus `0x7E400000`).
This whole window is Device-mapped by `boot/boot-rpi-cl.lisp` (identity table,
`0x3F000000-0x3FFFFFFF` Device), so registers are reachable from Lisp with
`(mem-ref addr :u32)` — no boot/MMU change needed.

Sources: `librerpi/lk-overlay` `platform/bcm28xx/hvs/hvs.c`; Linux vc4
`drivers/gpu/drm/vc4/{vc4_regs.h,vc4_plane.c,vc4_hvs.c}`.

## Register map (offsets from 0x3F400000)

| Reg | Off | Purpose |
|---|---|---|
| SCALER_DISPCTRL | 0x00 | global HVS enable/control |
| SCALER_DISPSTAT | 0x04 | global status |
| SCALER_DISPLIST0/1/2 | 0x20/24/28 | per-channel "next dlist" head = **dword slot index** into dlist SRAM |
| SCALER_DISPLSTAT | 0x2c | dlist status |
| SCALER_DISPLACT0/1/2 | 0x30/34/38 | read-only: currently-active dlist head |
| SCALER_DISPCTRLX(0) | 0x40 | channel-0 control (ENABLE=bit31, RESET=bit30). stride 0x10/chan |
| SCALER_DISPBKGNDX(0) | 0x44 | channel-0 background (FILL=bit24) |
| SCALER_DISPSTATX(0) | 0x48 | channel-0 status |
| SCALER_DISPBASEX(0) | 0x4c | channel-0 base |
| **DLIST SRAM** | **0x2000** | display-list SRAM, on-chip, 0x4000 bytes = **4096 dwords**, at 0x3F402000 |

`DISPCTRLX(x)=0x40+0x10x`, `DISPLISTX(x)=0x20+0x04x`, `DISPBKGNDX(x)=0x44+0x10x`.
The value written to DISPLISTX is a **dword slot index into the SRAM**, not a
byte or DRAM address. dlist SRAM is uncached (Device MMIO) → HVS sees writes
immediately, no cache maintenance.

## Display-list plane entry

A plane is a run of dwords; CTL0.SIZE says how many. Plane order in the list =
compositing order (later = on top). List ends at a word with bit31 set.

**CTL0 (word 0):** `END=bit31`, `VALID=bit30`, `SIZE=29:24` (dwords in this
entry), `TILING=21:20`, `HFLIP=16`, `VFLIP=15`, `ORDER=14:13`, `RGBA_EXPAND=12:11`,
`SCL1=10:8`, `SCL0=7:5`, `UNITY=bit4`, `PIXEL_FORMAT=3:0`.

**Pixel format (4-bit):** 0 RGB332, 1 RGBA4444, 2 RGB555, 3 RGBA5551, 4 RGB565,
5 RGB888, 6 RGBA6666, 7 RGBA8888, **8 YUV420 3-plane (Y,U,V)**, 9 YUV420 2-plane
(NV12), 10 YUV422 3-plane, 11 YUV422 2-plane. Firmware FB is usually 4 or 7.

**SCL0/SCL1 scaler mode:** 0 H-PPF V-PPF, 1 H-TPZ V-PPF, 2 H-PPF V-TPZ,
3 H-TPZ V-TPZ, 4 H-PPF V-NONE, 5 H-NONE V-PPF, 6 H-NONE V-TPZ, 7 H-TPZ V-NONE.
**Upscale (320→1080) = PPF both axes → SCL0=SCL1=0, UNITY clear.**

**POS0:** `FIXED_ALPHA=31:24`, `START_Y=23:12`, `START_X=11:0` (dest position).
**POS1 (only when not unity):** `SCL_HEIGHT=27:16`, `SCL_WIDTH=11:0` (dest size).
**POS2:** `ALPHA_MODE=31:30`, `ALPHA_PREMULT=29`, `ALPHA_MIX=28`, `HEIGHT=27:16`,
`WIDTH=11:0` (SOURCE size). **POS3:** scratch, write `0xc0c0c0c0`.

**Pointers/pitches (order):** PTR0,PTR1,PTR2 = **bus addresses** of Y,U,V planes
(`DRAM_addr | 0xC0000000` uncached alias, or flush cache each frame); then one
`0xc0c0c0c0` scratch per plane; then Y pitch (`SRC_PITCH=15:0` bytes); then U,V
pitches.

**CSC (YUV only, after pitches, before scaling) — 3 words. BT.601 limited:**
`0x00f00000, 0xe73304a8, 0x00066604`. (BT.709 ltd: `0x00f00000,0xf27784a8,0x00072e1d`;
JFIF full: `0x00000000,0xea349400,0x00059dc6`.)

**Scaling (only when not unity):** LBM base word (vertical PPF needs a
line-buffer-memory allocation — reserve a region, write its base). Then chroma
channel then luma channel scaling params: `scale=(src_px<<16)/dst_px`,
word=`PPF_AGC(bit30) | (scale<<8 & PPF_SCALE[24:8]) | (phase & PPF_IPHASE[6:0])`
for H then V. Then 4 PPF-kernel-offset words (H0,V0,H1,V1) =
`KOFF & PPF_KERNEL_OFFSET[13:0]`. For 320→1920: scale=(320<<16)/1920≈0x2AAA.

**PPF kernel must be uploaded once** to SRAM before PPF works: Mitchell-Netravali
16 taps `(0,-2,-6,-8,-10,-8,-3,2,18,50,82,119,155,187,213,227)` packed into 11
dwords by `VC4_LINEAR_PHASE_KERNEL` (read exact packing from vc4_hvs.c). Put the
slot index in each PPF word's KERNEL_OFFSET. Linux puts kernels at the top of
the SRAM pool, dlists at the bottom.

Finally patch CTL0.SIZE = total dwords in the entry.

## Committing / coexisting with firmware FB

Firmware wrote ONE plane and pointed DISPLISTX(chan) at it (usually chan 0 —
**read the register, don't assume**). Compose a NEW dlist in unused SRAM (past
firmware; lk-overlay starts at slot 11): re-emit the FB plane (unity RGB at the
mailbox FB address) first, then the scaled YUV plane on top, then `0x80000000`
END. Write the new list's start slot into DISPLISTX(chan) — latches next frame.
`DISPLACTX(chan)` reads what's currently active. **Do NOT** write RESET on the
live channel or touch the HDMI PHY/pixelvalve (firmware owns mode setup).

## Build milestones (de-risked, incremental)

0. **Read-only dump** of the firmware dlist: walk DISPLISTX(0) in SRAM, print
   each plane's CTL0/POS/PTR. Confirms register access, live channel, FB plane
   format+pointer. Zero risk. → `hvs-dump` in net/hdmi-hvs.lisp.
1. **Re-emit** the FB plane into a new dlist + END, repoint DISPLISTX → screen
   unchanged proves we can own the list.
2. **Add a unity (unscaled) plane** on top (solid color, or the 320×180 YUV at
   1:1) → proves plane compose + YUV format + pointers + CSC.
3. **Add scaling** (PPF kernel upload, LBM, scale words) → the full scaled YUV
   overlay. Then per frame: refresh Y/U/V DRAM (cache-flush or uncached alias);
   HVS does the rest. 720p60 target.

## Open items (verify before hardcoding)
- `VC4_LINEAR_PHASE_KERNEL` exact 16-tap→11-dword packing (read vc4_hvs.c).
- Which channel firmware uses (read DISPLISTX/DISPCTRLX).
- LBM base allocation for vertical PPF.
- ORDER field value for our Y/Cb/Cr byte order.
