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
- `MODUS_NET_STATIC=1` / `MODUS_NET_NOAUTO=1` are for a boot-time auto-install
  pipeline; the explicit `(ssh-boot)` path here does not need them.
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

- **`DECODE-MS` is the number**: pure decode time, blit and flush excluded.
  Run `(reel-demo-pass 4)` three times; the first includes JIT warm-up.
- SSH replies print as `= value`; `zero-measure.sh` greps `^= `.
- `net-install-and-call` blocks the net actor while it loads, so the link
  looks dead during the install — that is expected, wait for it.
- Set `*jit-hot-only*` NIL **before** the install, or the DEFUNs stay
  interpreter trampolines (~5 µs per interpreted loop iteration).

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

A76 (modus-pi hosted CLI) for scale: scalar 26 ms → NEON 24 ms on cam.ivf;
scalar 13.3 → NEON 12.0 ms on the libvpx vector vp80-00-comprehensive-006.
