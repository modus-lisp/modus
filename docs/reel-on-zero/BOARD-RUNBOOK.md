# Pi Zero 2 W board runbook: build → netboot → install reel → measure

The canonical sequence for getting a Modus image onto the bare-metal Zero 2 W
(Cortex-A53) and running the reel VP8 decoder on it. Written 2026-09-17 after
re-deriving it from scratch once too often. Every step below was executed and
its failure mode observed; the gotchas are the ones that actually bit.

## Topology

| thing | value |
|---|---|
| host driving the board | `modus-pi` (Pi 5, Linux; `ssh modus@modus-pi`) |
| host → Zero network | `eth0` on modus-pi = `10.0.0.1/24`; Zero = `10.0.0.2` (USB CDC-ECM, Zero is the gadget) |
| serial to the Zero | `/dev/ttyAMA0` on modus-pi, 115200 |
| board reset | `bash /home/modus/pi5-reset-zero.sh` (GPIO; netboot-gz.py calls it) |
| TFTP root (U-Boot fetches from here) | `/srv/tftp` on modus-pi, **root-owned dir** |
| HTTP for reel + clips | `python3 -m http.server 8099` in `/home/modus`, reached by the Zero as `http://10.0.0.1:8099/` |
| netboot driver | `/home/modus/netboot-gz.py` (retries TFTP over the flaky RTL8153) |
| measurement forms | `/home/modus/demo-forms.txt` (HDMI init, `play-ivf`, `reel-demo-pass`) |

## 1. Build the board image

From the modus tree (branch with your changes):

```
MODUS_NET_BUILD=1 MODUS_SSH_BUILD=1 MODUS_RPI_CHAINLOAD=1 \
  sbcl --dynamic-space-size 8192 --script mvm/build-rpi-cl-repl.lisp
gzip -kf /tmp/piboot/kernel8.img          # -> kernel8.img.gz (~5 MB from a ~62 MB raw)
```

- **Both `MODUS_NET_BUILD=1` and `MODUS_SSH_BUILD=1` are mandatory** for
  anything network. `MODUS_NET_BUILD` gates the whole `*net-source*` block
  (NIC driver, ip.lisp, http-client); the SSH transport (`ssh-boot`,
  `net-install-and-call`, `net-fetch-bytes`) is spliced **inside** that block
  under `MODUS_SSH_BUILD`, so `MODUS_SSH_BUILD=1` alone is silently a no-op —
  the image comes out byte-identical to a no-network build. Without them you
  get a serial-only REPL and `(ssh-boot)` answers `UNDEFINED-FUNCTION`.
  SSH is RPi-only (the actor/SSH address map is Pi DRAM; QEMU builds refuse it).
- **Verify the stack is in before you netboot** — it costs seconds and saves a
  boot cycle: `strings kernel8.img | grep -c ssh-boot` must be non-zero, and
  `cmp` against your previous image must report a difference. The build log
  prints nothing about these flags.
- **`MODUS_NET_NOAUTO=1` is also mandatory for the `(ssh-boot)` path.**
  `MODUS_NET_BUILD` enables a boot-time auto-install pipeline
  (`NET-PIPELINE-START` in the serial log: DHCP, fetch a tarball, drop to the
  REPL). On the Zero it gets a garbage lease (`IP=10.0.2.15 GW=2.2.0.10`),
  logs `TCP:F TCP:F LIB-FETCH-FAIL`, and leaves the NIC's TCP state poisoned.
  `(ssh-boot)` then still reaches `NETUP` and the board answers **ping**, but
  every SSH connection dies: the first attempts close during key exchange
  (`ssh -vv` ends at `expecting SSH2_MSG_KEX_ECDH_REPLY`, then `Connection
  closed by 10.0.0.2`), later ones time out at TCP connect, and the serial
  console prints nothing. Every prior working boot log has
  `NET-PIPELINE-START` count 0. `MODUS_NET_STATIC=1` is the other half of that
  auto-install pipeline; leave both alone for this flow.

So the full, verified flag set is:

```
MODUS_NET_BUILD=1 MODUS_SSH_BUILD=1 MODUS_NET_NOAUTO=1 MODUS_RPI_CHAINLOAD=1
```

Read the serial log after `go`: the sequence must be `E2SMOKE-END` →
`Modus CL REPL` → your `(ssh-boot)` → `NETUP`, with **no** `NET-PIPELINE-START`
in between.
- `MODUS_RPI_CHAINLOAD=1` is the load-address-agnostic chainloader layout the
  netboot `go 0x300000` expects.
- Optional trims: `MODUS_RPI_NO_BLOB=1`, `MODUS_RPI_NO_BRIDGE=1` (smaller image).
- The raw `kernel8.img` is mostly zero-filled BSS; the gzip is what travels.
  ~5 MB gz is normal and boots fine.

## 2. Stage on modus-pi

```
scp kernel8.img.gz modus@modus-pi:/home/modus/board-XXX.img.gz
ssh modus@modus-pi 'sudo -n cp /home/modus/board-XXX.img.gz /srv/tftp/ && \
                    sudo -n chown modus:modus /srv/tftp/board-XXX.img.gz && \
                    ls -la /srv/tftp/board-XXX.img.gz'
```

- **`/srv/tftp` is root-owned. A plain `cp` fails silently** and U-Boot then
  reports `TFTP error: 'File not found' (1)` while `ping 10.0.0.1` is fine —
  that exact combination means "not staged", nothing else. Always `sudo -n`
  and always `ls` the result.
- Check disk first: `df -h /home/modus`. The SD fills up (each hosted CLI
  binary is ~67 MB); a full disk makes `scp` fail with `write remote: Failure`.

Also put in `/home/modus` whatever the Zero will fetch over HTTP: the reel
tarball (see §5) and the clip (`cam.ivf`, `small.ivf`, `vp8-std.ivf`).

## 3. Serve HTTP

```
ssh modus@modus-pi 'cd /home/modus && nohup python3 -m http.server 8099 >http8099.log 2>&1 &'
ssh modus@modus-pi 'pgrep -af "[h]ttp.server 8099"'
```

Nothing starts this for you. `net-install-and-call` / `reel-demo-load` on the
Zero fetch from it.

## 4. Netboot

```
ssh modus@modus-pi 'cd /home/modus && python3 netboot-gz.py \
    --img board-XXX.img.gz --tftp-tries 6 --send-delay 240 --send "(ssh-boot)" \
    > nb.log 2>&1'
```

What `netboot-gz.py` does, in order (so you can read its log):

1. reset the Zero, wait for U-Boot
2. `setenv ipaddr 10.0.0.2`, `setenv serverip 10.0.0.1`, `usb start`
3. `ping 10.0.0.1` until `is alive` (`usb reset` between tries)
4. `setenv tftpblocksize 512`; `tftpboot 0x08000000 <img>` — retried with
   `usb stop/start` on `Rx: failed` — success marker **`Bytes transferred`**
5. `unzip 0x08000000 0x300000` — success marker **`Uncompressed size`**
6. `mw.q 0x18000000 0` (clear the core-save word) then `go 0x300000`
7. wait for **`Modus CL REPL`**, then send each `--send` form and wait
   `--send-delay` seconds (`(ssh-boot)` needs the full 240 s to bring the NIC
   up; its success marker in the log is **`NETUP`**)
8. prints `NETBOOT-DONE`

Then confirm the link from modus-pi: `ping -c1 -W1 10.0.0.2`.

- **The wrapper only summarizes at the end.** While it runs, watch the live
  log: `tr -d '\0' < nb.log | grep -aE 'tftpboot try|Bytes transferred|Uncompressed|Modus CL|NETUP|FAULT|ESR'`.
- **Never type into the serial port while Modus is booting** — input during
  boot wedges the reader.
- `Device NOT ready` / `Request Sense 02 3A 00` from the DWC2 scan is the
  RTL8153 dongle; the retry loop usually clears it, a wedged dongle needs a
  **full power cycle** of the Zero (the physical switch), not a GPIO reset.
- `go` bounce: if a stray `go 0x300000` reaches the Modus REPL it evaluates as
  `UNBOUND-VARIABLE`; harmless, re-send what you meant.
- **`pkill -f netboot` matches the ssh shell that contains that string and
  kills it.** Bracket the pattern: `pkill -f "[n]etboot-gz"`. Same for
  `pgrep`.

## 5. The reel tarball

The board installs a **decoder-only trim** of reel via ASDF
(`install-tarball` untars and `asdf:load-system`s the `.asd` inside). Build it
from the reel checkout:

```
reel/reel.asd            # decoder-only trim (NOT the full repo reel.asd)
reel/src/packages.lisp
reel/src/decode/{tables,bool,transform,intra,loopfilter,inter-tables,inter,
                 inter-neon,loopfilter-neon}.lisp
```

with the trimmed `.asd` listing the decode module `:serial t` in that order.
`inter-neon` and `loopfilter-neon` are the `#+modus` NEON kernels; they
redefine `mc-filter` and the loop-filter edge functions **after** the scalar
ones. For the precompiled decode callers to actually use them the board image
must have **linkage cells** on (`*jit-linkage-cells*` T, set in
`build-cl-repl-common.lisp`'s JIT init since modus 841f8ef); without that the
redefinition is invisible and the decoder silently keeps the scalar kernels
(mc-NEON 0 / mc-SCALAR 13638 — that is how this was discovered).

`tar cf reel-neon.tar reel` from the staging dir; the ASDF system name stays
`:reel`.

## 6. Install reel and measure

Over SSH from modus-pi (password auth, any password):

```
SSH="ssh -n -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
     -o PreferredAuthentications=password -o PubkeyAuthentication=no test@10.0.0.2"
$SSH '(+ 2 3)'                                                    # sanity
$SSH '(setq *jit-hot-only* nil)'                                  # every defun goes native on load
$SSH '(net-install-and-call "http://10.0.0.1:8099/reel-neon.tar")' # minutes; blocks net-actor-main
$SSH '(if (find-package "REEL") 1 0)'
while IFS= read -r f; do [ -n "$f" ] && $SSH "$f"; done < /home/modus/demo-forms.txt
$SSH '(init)'                                                     # HDMI mailbox framebuffer
$SSH '(reel-demo-load "http://10.0.0.1:8099/cam.ivf")'
$SSH '(reel-demo-pass 4)'   # -> (FRAMES n TOTAL-MS t DECODE-MS d FPS f)
```

- **Keep a serial tap running during every SSH phase**
  (`serial-tap.py /home/modus/serial-install.log &`, killed before the serial
  driver takes the port). A bare-metal fault is a silent spin; without the tap
  a crash during `net-install-and-call` and a NIC wedge look identical (SSH
  form never returns, board dark). The tap is the only place `FAULT`/`ESR` or
  a load error will show up. `board-flow.sh` does this.
- **Gate on the first reply.** `(+ 2 3)` must print `= 5` before anything
  else is sent. The `run` helper (and `zero-measure.sh`) discards SSH stderr,
  so a dead SSH server looks like thirty silently-empty forms and a
  `MEASURE DONE` banner with no numbers. If `= 5` is missing: `ssh -vv` from
  modus-pi shows where the handshake stops, and `serial-ssh-diag.py`
  (captures `/dev/ttyAMA0` while opening one SSH connection) shows whether
  the board faults or is simply silent.
- **`DECODE-MS` is the number**: pure decode time, blit and flush excluded.
  Run `(reel-demo-pass 4)` three times; the first includes JIT warm-up.
- SSH replies print as `= value` **followed by `\r\n`**; strip NULs and CRs
  (`tr -d '\0\r'`) before comparing, or `[ "$R" = "= 5" ]` fails on `= 5\r`
  and a working board looks dead. `zero-measure.sh` greps `^= `.
- If the netboot succeeded but a later step failed, the board is still up:
  use `measure-only.sh` (everything after netboot) instead of re-netbooting.
- `net-install-and-call` blocks the net actor while it loads, so the link
  looks dead during the install — that is expected, wait for it.
- **Do NOT set `*jit-hot-only*` NIL before the install.** With it NIL,
  `net-install-and-call` eager-JITs every reel DEFUN as it loads (~16 min on
  the A53) while `net-actor-main` is blocked, so USB polling stops, the host
  side hits its NETDEV watchdog, and the board ends up with no ping, no SSH
  and a dead serial console — a wedge that needs a reset (GPIO via
  `netboot-gz.py`, or a power cycle if U-Boot then shows the RTL8153 as
  `Device NOT ready`). Observed 2026-09-17: three good replies, then silence.
- The order that works, and why (`board-flow.sh` + `serial-measure.py`):
  1. **While the NIC is alive (SSH, default hot-only):** install reel (fast —
     trampolines), push `demo-forms.txt`, `hvs-all-forms.txt`,
     `reel-hvs-forms.txt`, `(init)`, and fetch the clip(s) into memory with
     `reel-demo-load` / `rh-load`. Everything that needs the network happens
     here.
  2. **Then over serial** (nothing needs the network any more):
     `(setq *jit-hot-only* nil)`, `(jit-eager)` — natively compiles every
     registered reel DEFUN (the trampoline registry), the ~16 min the install
     must not spend — then `(reel-demo-pass 4)` ×3 for `DECODE-MS`, then
     `(rh-init)`, `(rh-first-frame)`, `(rh-play nil)` with the webcam
     recording. Tagged forms, long timeouts (see §7).
  Interpreted DEFUNs cost ~5 µs per loop iteration, so a measurement taken
  before `jit-eager` is meaningless.

## 6b. Reading a `!!FAULT` line

`!!FAULT E<esr> L<elr> F<far> S<sp>`. ESR `0x96000005` = data abort from
EL1, translation fault; `0x86…` would be an instruction abort. The kernel is
linked at the canonical VA `0x30000000` (`+rpi-cl-code-vaddr-base+`), so
`ELR − 0x30000000` is the file offset in the raw `kernel8.img`, and

```
aarch64-linux-gnu-objdump -D -b binary -m aarch64 --adjust-vma=0x30000000 \
    --start-address=$((ELR-0x40)) --stop-address=$((ELR+0x20)) kernel8.img
```

shows the faulting instruction. A build with `MODUS_SYMMAP=path` (same source
⇒ byte-identical image, so it applies retroactively) names the function; its
VAs are host-layout (`virtual-addr − native-offset` gives the code base), so
match by native offset, not by the board VA.

**OPEN (2026-09-17): `net-install-and-call` faults on the Zero with this
image, scalar or NEON tarball alike, linkage cells on or off.** Tarball
arrives intact; the `%net-fnv1a` verification loop runs ~60 KB, then faults at
`ldur x17,[x19,#-9]` inside `GENERIC-MULTIPLY` — the header read of a
tag-9 object — with x19 a garbage pointer that differs run to run. The hash
accumulator never leaves fixnum range, so `*` should never reach generic
multiply: `h` was corrupted mid-loop. The identical function is correct on the
hosted aarch64 CLI, interpreted and native. Not the NEON files, not linkage
cells; a bare-metal, timing/data-dependent corruption (GC-timing or the
stale-DRAM class) somewhere in `e490440..0808b85` (jit-eager, SIMD 2b). The
last image known to install over the network predates that series. Until it
is bisected, put reel on the board via the QEMU core route (§8), which does
not run this path.

## 6c. The same image under QEMU raspi3b (core route, debugging)

```
qemu-system-aarch64 -M raspi3b -kernel kernel8.img \
  -serial null -serial unix:q.sock,server,nowait -display none -no-reboot \
  -device loader,file=reel.tar,addr=0x1a000000 -device loader,file=clip.ivf,addr=0x1b000000 \
  -gdb tcp::1234
```

- **The board image talks on the mini-UART, which is QEMU raspi3b's SECOND
  `-serial`.** The first is the PL011. Wire the first to `null`; a driver on
  a socket attached to the first serial sees no banner and every form returns
  empty while the guest sits in the mini-UART RX poll
  (`ldur w18,[x17,#20]; tbz w18,#0,…` with x17 = `0x3F215040`).
- `-serial unix:…,server,nowait` drops output emitted before a client
  connects; start the driver as soon as the socket exists.
- **Boot to the REPL takes minutes under TCG** (the 62 MB image, E2SMOKE,
  init). 25 s in, only the first boot warnings have printed. A driver that
  waits 300 s for `Modus CL REPL`, gives up, and starts sending forms is
  talking to a half-booted guest and gets empty replies for everything —
  indistinguishable from a wrong serial. Wait ≥ 1500 s for the banner and
  send nothing before it (`qcore.py`).
- Silence is never evidence: `gdb-multiarch -batch -ex 'target remote :1234'
  -ex 'info registers pc' -ex 'x/3i $pc'` tells you in seconds whether the
  guest is spinning, faulted, or waiting for input. A stale QEMU from an old
  session can hold :1234 (`ss -ltnp | grep 1234`).
- Tar and clip are read out of RAM with `ramv` (`(ramv #x1a000000 <bytes>)`),
  installed with `install-tarball-from-bytes`; then `jit-eager`,
  `(%save-image "x.core")` → `CORE-END=<end>`, and
  `gdb … dump binary memory x.core 0x18000000 <end>`. Netboot with
  `netboot-core-gz.py --img <image>.gz --core x.core` (core in `/srv/tftp`).
  `qcore.py` is the whole thing (2026-09-17: install ≈ minutes, `jit-eager`
  308 s under TCG, core 12.7 MB).
- `(jit-eager)` returns `(INSTALLED MODULES FAILED)`; a handful of FAILED is
  normal (`%INIT-GENERA-COMPAT` and three ASDF fns never translate).
- `%jit-fn-native-p` keys by the SFT name string: a non-`CL-USER` symbol
  needs its **qualified** name (`"REEL.DECODE::DECODE-FRAME"`); the bare
  name returns NIL even when the function is native.
- The core carries forms and clip but not board state: run `(init)` (HDMI
  mailbox framebuffer) on the board before `reel-demo-pass`.
- **`(jit-eager)` on the board after the core restore is mandatory before
  any timing**, even though it reports only `(3 3 4)`. Same core, same
  clip: `rh-play` without it = 3309 ms/frame; with it = 218 ms/frame
  (`reel-demo-pass` 209). A restored core's decode path is not fully native
  until that call runs.

## 7. Serial-only fallback (no `MODUS_SSH_BUILD`)

The serial REPL prints **bare values** (no `= `). Tag every form so a reply can
be matched: send `(list 4711 FORM)` and regex for `(4711 …)`. A stuck reader
is freed by sending `)))))))))` then a throwaway `(+ 0 0)` (the next form
after the unstick reads as an error). See `rh_core6.py` for a working driver.
Loading reel over serial is impractical (163 KB at 4 ms/byte); use §6.

## Baselines to compare against (A53, cam.ivf 320x180)

| build | per inter frame |
|---|---|
| scalar, on-board JIT, hot-only-NIL load | ~224 ms |
| scalar, QEMU-built native core, jit-eager | ~137 ms |
| scalar phase profile (eager core) | MB loop 98 (IDCT+add 30, tokens 25, MC 18, modes 18), loop filter 40, copy 3 |
| **NEON + linkage cells, QEMU core (`reel-lc.core`, tree at `841f8ef`+), 2026-09-17** | **209 ms** (18774 ms / 90, three passes within 0.5%; `reel-demo-pass 4`, DECODE-MS) |

**CORRECTION (2026-09-17 late):** the 137 ms figure was on `small.ivf`
(`qsave.py` loads 63792 bytes = small.ivf), not cam.ivf. Rebooted with
yesterday's own `board-demo12.img.gz` + `reel-eager.core`, `rh-play` on
cam.ivf today = **217 ms/frame** — identical to the NEON core's 218. So there
is **no timing regression**; there is also no NEON gain on this clip on the
A53. Compare like with like: same clip, same driver, same boot recipe.

**The display regression IS real:** yesterday's image + core show cam.ivf's
frames correctly on the HVS plane today (the clip is a webcam view of a dark
room with a laptop terminal — do not mistake the decoded frame for the
board's console); the new image + core show noise + pink. `hvs-all-forms.txt`
is semantically identical to the baked `net/hdmi-hvs.lisp` (only docstrings /
hex spelling differ), so late binding to stale pushed forms is not it.
Hosted checks on the new tree of JIT'd `mem-ref :u32` stores of words ≥ 2³¹
are exact. `serial-static.py` isolates the baked HVS writers with a
solid-colour scaled plane and no reel at all.

**Trap:** `(%mmap-exec-page N)` evaluated at top level or in an interpreted
defun ECHOES N (interpreter arm). Get pages from a JIT'd defun
(`*jit-hot-only*` NIL before the defun) or a baked one; a probe that got
"page 4096" and then faulted on every `mem-ref` tested nothing.

**Decode on the A53 with that core is CORRECT** (`serial-check.py`): frame-0
`(w h Σy Σu first-8-Y)` on the board = `(320 180 9035032 2267981 (154 131 83
112 125 130 58 92))`, identical to the hosted decode that is MD5-exact vs
libvpx. So the slowness is real work done slowly, and the display fault
below is not a decode fault.

**Display shows garbage with that core** (`rh-first-frame` → HVS plane): a
strip of RGB noise over a pink field with a faint periodic pattern =
uninitialised NC buffer / unwritten chroma, with `jit-eager` or without.
Decode is correct, so the decoded planes are not reaching the NC display
buffers (`rh-copy-planes` → `rh-yuv-plane`), or the plane descriptor points
elsewhere. `serial-probe.py` reads the NC buffer back after the copy.
RETRACTED finding: the first probe read `154 ×8` at the buffer/array START
and called it a broadcast. It is the plane's 32-pixel LEFT BORDER (each row
begins with `+border+` copies of its first pixel); the visible pixels start at
`picture-y-offset`. `serial-probe2.py` confirmed the blit's instruction words
in the exec page equal the literal list, and the identical `mem-ref` loop is
correct on the hosted CLI in every arm. **Compare at the visible offset**
(`ptrs` returned by `rh-copy-planes` = `buf + y-offset`), never at the array
start. `serial-probe3.py` result: NC buffer at the visible pointer =
`(154 131 83 112 125 130 58 92)` = the decoded pixels — **the copy is
correct**; the plane-scale arithmetic `ppf` = 1076537856 (host-identical);
a JIT'd FNV over 4096 bytes = 65470874 (host-identical). So decode, copy and
JIT'd integer arithmetic are all right on the board.

**CORRECTION: the HVS functions are NOT baked into the board image.**
`net/hdmi-hvs.lisp` is not part of `build-rpi-cl-repl`; on a plain image
`(fboundp 'hvs-base)` → 0 and `*jit-on*` is unbound. Every HVS function
exists only as a pushed form (`hvs-all-forms.txt`), JIT-compiled at runtime
and, in a core, `jit-eager`'d under QEMU. So the whole display path is
runtime-compiled code, and the one thing that differs between yesterday's
working core and the new one on that path is linkage cells. (The
`serial-static.py` probe was therefore invalid on both images: `??` on every
form was `UNDEFINED-FUNCTION HVS-BASE`, and my driver's short tail hid the
error — on a miss, print the whole reply.) `serial-dlist.py` reads the
plane's dlist words back under the Device window after `rh-first-frame` to
compare with what `rh-yuv-plane` should have written.
`serial-probe4.py` calls the baked `%net-fnv1a` directly over 4 KB / 64 KB /
the full clip against host truths.

**`serial-dlist.py` result (the first real signature):** reading the plane's
30 dlist words back under the Device window after `rh-first-frame` on the
new core: every *computed* word is right (control `0x5C000008`, position,
`sw/sh`, strides, all `ppf` scale words, kernel slots, `0x80000000`), but
every slot that should hold the **literal `#xC0C0C0C0`** holds garbage or a
neighbouring pointer-like value (6 of 6), and the three `#xC0000000|ptr`
words are wrong. The same `words` defun JIT'd on the hosted aarch64 CLI
(`litrepro.lisp`) yields all 29 words correctly. `rh-yuv-plane` was
re-pushed and JIT-compiled ON THE BOARD, so this is board-side JIT
materialisation of large literals in the new tree, not the core restore.
The constant-vector path (`e9a5aeb`, Aug 30) predates yesterday's working
image, so it is not the change itself. `serial-lit.py` runs the same defun on
the plain image (no reel, no core) — a 5-minute bisect probe.

`serial-lit.py` on the plain new image: **all 29 words correct**, constvec
root (`#x10000F10`) reads 0 ⇒ literals baked. So board-side JIT literal
materialisation is fine on a fresh image; the garbage is specific to the
**core-restored session**. Mechanism under test (`serial-lit-core.py`):
`*aarch64-jit-constvec-p*` is a heap global and comes back T from the QEMU
save, but the constant-vector root is a BSS word that boot re-zeroes — a
state a plain image never has — so a form JIT'd after the restore may emit
constant-vector loads against a root/vector that is not the one its
constants were placed in.

`serial-lit-core.py` result: in the core session a **fresh** `words` defun
is correct before and after `jit-eager`, and the hosted CLI is correct for
the exact `let*` shape of `rh-yuv-plane` (`letstar.lisp`). So every way of
*constructing* the words is right; the corruption is on the **write** path:
`hvs-slot-wr32` / `hvs-window-nc` / `hvs-upload-kernel` are QEMU-compiled,
core-restored functions built from large literals (the MAIR/blit instruction
words `#xD5033F9F …`, `#x2000`). Hypothesis: large literals inside
core-restored pages resolve wrong after the restore (the fresh-on-board pages
are fine), so the MAIR exec page holds garbage, the NC remap never takes, and
descriptor stores latch partially — slots whose old SRAM value already
matched read "right". `serial-lit2.py` calls the restored `hvs-mair-words` /
`hvs-blit-nc-words` and compares with host truth, then a fresh copy.

`serial-lit2.py` result: the core-restored `hvs-mair-words`,
`hvs-blit-nc-words` and `hvs-ppf-word` return the host-true words and are
native. Large-literal materialisation is correct in restored pages too.
**Word construction is now exonerated in every form** (fresh, `let*`, hosted,
restored). What is left is the store itself under the NC window —
`serial-win.py` writes known words to spare dlist slots under
`(hvs-window-nc t)`, reads back under Device, and also tries a plain Device
store, to see whether the remap takes and the stores land whole in a core
session.

`serial-win.py` result: in the core session `(hvs-window-nc t)` populates
`*hvs-attr*`, three known words written with `hvs-slot-wr32` read back whole
under Device (`3233857728 305419896 305419896`), and even a plain
Device-mapped store reads back whole. **Construction, literals, remap and
stores are all correct.** That leaves my *expected* list for the
`serial-dlist.py` readback as the suspect — it assumed a fixed
`picture-y-offset` (12320) while the pointer arithmetic in the readback
implies a different offset for that decode. `serial-diff.py` removes the
assumption: same boot, `rh-yuv-plane` writes, an identical twin returns its
`words`, the dlist is read back, and the two are diffed element by element,
with a photo taken between.

`serial-diff.py` result: written list and readback are identical in every
slot **except the six `#xC0C0C0C0` placeholders**, which read back as the
three pointer words again plus small context values — those slots are
**owned by the HVS** (it writes its per-plane context there after scanout).
RETRACTED: the "literal words wrong" finding of `serial-dlist.py` was a
misread of hardware-written slots. The descriptor is exactly what we intend.

Reading the screen itself: a uniform pink YUV field is what the HVS renders
from **all-0xFF memory** (Y=U=V=255 → magenta); the noise strip on top is a
short run of real bytes before it. So the HVS is dereferencing a physical
region that is not our buffer, while the CPU sees the right bytes at the same
address: the plane pointer is built from the buffer's **VA** (JIT arena,
`0x144xxxxx`) as if VA = PA. Yesterday's 20 MB image held that identity; the
new 62 MB image (net + SSH stack) may map the arena elsewhere.
`serial-white.py` (fill Y/U/V solid white through the same buffer and plane)
and `serial-pt.py` (read the L2 block entry for the buffer's VA and print its
PA) settle it. If PA ≠ VA, the fix is to program the plane with the PA (or
allocate display buffers from an identity-mapped region).

`serial-white.py` result: **the screen went white.** Filling the same NC
buffer's planes with Y=235/U=V=128 through the same blit and the same plane
descriptor displays correctly, so VA = PA for the arena, the descriptor is
right, and the blit reaches DRAM. (The page-table probe is moot and its
numbers are unusable: `mem-ref :u64` of a descriptor prints as a tagged
value.) The fault is between the decoded planes and that buffer:
`rh-copy-planes` → `hvs-ncopy-nc`. Re-reading the garbage frame with this in
hand: a strip of real rows on top, then the 0xFF the buffer already held —
**the copy writes only a prefix of each plane.** The earlier CPU readback
that "proved the copy correct" sampled row 32, inside that prefix.
`rh-ceil64` is correct hosted; `serial-copy.py` pre-fills the buffer with a
known byte, runs `rh-copy-planes`, and reads Y at rows 0…240 and the U/V
starts against the source arrays to locate where the copy stops.

`serial-copy.py` result: the copy is **complete and correct** — Y rows 0, 32,
60, 100, 150, 200, 240 and the U/V starts all equal the source arrays
(`rh-ceil64` right on the board too). So the "prefix" reading was wrong as
well. Through the CPU's view the buffer holds the whole frame; the HVS shows
that buffer when filled white; the HVS shows garbage for the frame. The only
difference left between the two writes is the **cache**: if the block's
non-cacheable remap (`hvs-map-nc`) is not actually in effect, the blit's
stores sit in L2, the CPU reads them back from L2 (looks perfect) and the
HVS reads stale DRAM — the pink 0xFF. Yesterday's buffers sat at
`0x14880000`; today's blocks (`0x144…`, `0x14C…`) also hold JIT pages and
the linkage cells, and a remap of a block that contains executing code may
not take. `serial-cvac.py` runs the frame copy through `hvs-ncopy` (the
cache-cleaning `DC CVAC` variant) on the live session and photographs both;
a correct frame there both confirms the mechanism and is a usable fix
(clean after the copy, or give display buffers their own 2 MB blocks).

`serial-probe4.py` result: the **baked** `%net-fnv1a` returns 65470874 /
1325675142 / 71127839 over 4096 / 65536 / 151295 bytes of the clip —
host-identical, no fault. So the hypothesis above is wrong as stated: the
baked loop is fine on a heap array called from the REPL. The install fault
is specific to its context — the buffer `net-fetch-bytes` returns and a
loop running with the net actor blocked — and the display fault is specific
to the HVS descriptor path. Neither reproduces in isolation; both are
parked. Every isolated probe on this core passes: decode (frame-0 stats),
NC copy (visible offset), JIT'd and baked integer arithmetic, FNV full-length.
The actionable regression is the **timing** (209 ms vs 137 ms), and that
bisects on the hosted aarch64 CLI with `drv-neon.lisp` — no board needed.

**QEMU-compiled core + HVS: re-push `rh-yuv-plane` before `rh-first-frame`.**
Compiled under QEMU, `rh-yuv-plane`'s trailing HVS register read mis-tags the
address and the first frame faults at FAR `0x7e800034` (the HVS *bus*
address, unmapped). `rh_core6.py` / `serial-fast.py` re-define it over serial
without that read first; then `rh-init` → `rh-first-frame` → `rh-play` work.

A76 (modus-pi hosted CLI) for scale: scalar 26 ms → NEON 24 ms on cam.ivf;
scalar 13.3 → NEON 12.0 ms on the libvpx vector vp80-00-comprehensive-006.
