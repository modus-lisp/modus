# Quickload on bare-metal Raspberry Pi — status and remaining problem

*Written 2026-08-29. Facts below were read out of the working tree and out of
live hardware on that date; every claim is either cited to a file:line or
labelled as unverified.*

The release gate (user, 2026-08-15) is: **Modus running AS the operating system
on Raspberry Pi hardware, fetching and loading a library itself.** Asked to
choose between bare metal and Linux-on-Pi-5, the answer was bare metal.

This document separates three things that are easy to conflate:

1. what has actually been demonstrated on Pi silicon,
2. what `ql:quickload` needs beyond that,
3. which of those gaps is the real one.

---

## 1. What is proven on bare-metal Pi silicon

These are not QEMU results.

| Capability | Evidence |
|---|---|
| Modus boots and runs as the OS on a real Pi Zero 2 W | task #273 |
| SSH server, remote eval — `ssh test@10.0.0.2 '(+ 2 3)'` → `= 5` | task #276 |
| Real HTTP fetch of a library tarball, then load **and run** it | task #268 — alexandria 22/22, commit `bfca1db` |
| Runtime JIT (bytecode → native aarch64) driving that load | tasks #198, #277 |
| 16 MB image deployed in ~9 s (U-Boot + TFTP netboot rig) | `reference_rpi_netboot_rig` |

The alexandria result is the important one, because it is the whole pipeline in
miniature: **the Pi resolved a host over DNS, opened TCP, issued an HTTP GET,
received a `.tar`, unpacked it into RAM, compiled it, and called into it.**
`net-install-and-call` → `flatten` → `= (1 2 3)`.

So "fetch a library over the network and load it on bare metal" is **done**.
That is worth stating plainly, because it is most of the distance and it is easy
to lose sight of it while enumerating what is left.

---

## 2. What `ql:quickload` requires that this does not cover

The gap is not the network and not the compiler. It is that the demonstrated
path uses `install-tarball` — Modus's own loader — against a pre-staged `.tar`,
whereas `ql:quickload` is a **real program with real dependencies** that expects
to run on a POSIX-ish host.

The bare-metal RPi image (`mvm/build-rpi-cl-repl.lisp`, a thin tail over
`mvm/build-cli-common.lisp`) already has, verified in source:

- `lib/tar.lisp` + `lib/install-tarball.lisp` — bare metal takes this branch via
  `*cli-bare-metal-tarball*` (`build-cli-common.lisp:836-864`)
- the `:GENERA` shim, spliced unconditionally (`build-cli-common.lisp:1231-1255`,
  installed at `:1170`)
- the ASDF-shaped surface, `net/asdf-interface.lisp` (`build-cli-common.lisp:1274-1290`)
- DNS, TCP, HTTP/1.0 GET, and the DWC2/CDC-ECM/RTL8153 USB-ethernet stack
  (`build-rpi-cl-repl.lisp:546-639`)
- the aarch64 JIT, on by default (`build-rpi-cl-repl.lisp:116`)

What it lacks:

### 2a. The genuine Quicklisp client is not in this repo at all

`modus-quicklisp/setup.lisp` is a **107-line shim**: it interns `QL:QUICKLOAD`
and points it at `install-tarball` over a local `.tar`
(`modus-quicklisp/setup.lisp:51-105`). No dist, no HTTP, no `~/quicklisp/`.

The genuine client lives on the development host at `/home/claude/quicklisp/`
(22 source files). Task #278's result — "unmodified quicklisp client quickloads
alexandria in 43.6 s" — was obtained by `cl:load`ing that host checkout into the
**hosted x64** image. The build system deliberately bakes nothing:

> "The QL package + ql:quickload still come ONLY from a runtime (load) of
> modus-quicklisp/setup.lisp — never baked here."
> — `build-cli-common.lisp:849-850`, restated at `build-generic-cli.lisp:13`
> ("NO quicklisp is baked in — exactly like stock SBCL")

That is the right design for a hosted image. It is exactly wrong for a machine
with no disk to load from.

### 2b. There was no filesystem — SOLVED on x64, 2026-08-29

`ql:quickload` is not a function that fetches a tarball. It is a program that
reads and writes a directory tree: `setup.lisp`, `config.lisp`, `dist.lisp`,
`local-projects.lisp` all call `cl:load` / `open` / `probe-file` /
`ensure-directories-exist` against real paths.

On bare metal every one of those is a Linux syscall with no OS behind it
(`build-cli-common.lisp:1021` — "a literal SVC with no OS behind it"), so `OPEN`
and `LOAD` existed as symbols and **faulted when called**. That was named here
as the one load-bearing gap.

**It is now closed on x64** (branch `cabinet-fs`, `a325f25`). `cl-fileio.lisp`
carries one seam — the function-valued global `*CAB-CALL*` — and when it is set
every `%SYS-*` primitive routes to a cabinet filesystem instead of a syscall.
`lib/cabinet-fs.lisp` is the loadable other half. Measured:

```
probe-file /nope        => NIL
(with-open-file … :output) writes, (… :input) reads it back verbatim
(load "/hello.lisp")    => defines CAB-HELLO, which returns :FROM-CABINET
(cabinet-unmount)       => real syscalls again
```

That is CL's `LOAD` pulling source out of an in-RAM filesystem, compiling it and
running it, with no kernel underneath.

Two things measured on the way that are easy to get wrong:

- **The seam has to be baked; a runtime override does not work.** A runtime
  `(defun %sys-stat-exists …)` is visible to interpreted callers but NOT to the
  already-compiled `PROBE-FILE`, which keeps calling the baked one — direct call
  returns 1 while `probe-file` still returns NIL. Compiled call sites bind at
  build time.
- **Cabinet bytes must never travel through `*IO-BUF-ADDR*`.** The stream layer
  already keeps its own Lisp buffer, so `%SYS-READ-INTO-BUF` fills that directly
  on both paths. Routing through the raw buffer would have worked JIT-on and
  silently corrupted JIT-off, because `(setf (mem-ref addr :u8) v)` is a no-op
  on the interpreted path (task #272).

Status and honest limits: **ungated** (the refactor moves `%FS-READ-CHAR`, which
every file read goes through — wants a 64-shard run), **not on main**, and **not
yet run on bare metal at all**, because of 2d.

### 2d. SOLVED (2026-08-29) — aarch64 lost runtime CLOS methods at the first GC

Task **#281**, root-caused and fixed the day after it was written up. The
repro (a runtime `defmethod` dying with "no applicable method" after 200k
allocations, 3/3 deterministic, x64 immune) turned out to be one missing
runtime flag with a long causal chain:

`*aarch64-gc-bitmap-enabled*` was set by the **build scripts only** — host
side. At runtime the defvar read NIL (Limitation 7), and the set-bit emitters
consult it **at emit time**, so every page the *in-image runtime JIT* emitted
allocated conses and objects with **no object-start/cons-kind bitmap bit**.
The aa64 validating collector applies the bitmap gate to *every* word its
`scan_word` visits — Cheney scan slots included — so any reference to a
JIT-allocated object was treated as a conservative false positive and **never
forwarded**. A runtime `defmethod`'s specializer list is JIT-materialized:
after the first collection the method record's specializer slot pointed at a
from-space corpse (forwarding stamp visible as subtag `0x5F`), the
specializer↔CPL `eq` failed, and dispatch was gone for good.

Proof was a chain-walk printing `%val->word` for every link either side of a
GC (`cabfs/aa-chain.lisp`): registry → entry → gf → method list → method all
moved to to-space; the specializer-list slot kept its pre-GC word. Two earlier
"leads" died honestly: the "binding into locals repairs it" observation was an
artifact (probe defuns shift compile-time allocation so the GC trigger never
fires — read the gc counter at `0x10000060`, probe shapes lie), and `ash` of a
symbol SEGVs at runtime on aa64, so pointer-printing must use `%val->word`.

Fixes, both on `cabinet-fs`:
- **de6b02c** — `%jit-boot-init`'s aarch64 arm enables the flag at runtime,
  gated on `%gc-bitmap-init` actually having published a bitmap base.
- **b781961** — the RPi build's identical enable was env-gated behind the
  "exception storm" wedge note; that wedge was symptom 3 of the
  alloc-overshoot bug fixed in bfca1db, so the gate was stale. Default is now
  ON (`MODUS_RPI_JIT_BITMAP=0` = triage opt-out). Build verified.

Verified on the rebuilt hosted CLI: repro 3/3 `:SYNCED`, every dispatch shape
survives gc=3, and — the payoff — **the full pagetree+cabinet stack now loads
and its smoke test passes on aarch64** (format-fs, make-directories, UTF-8
write/read round-trip, readdir) through 16 collections. **Validated on
silicon the same day**: the bitmap-on RPi image (`modus-clos-mini.img`)
netbooted to the Zero 2 W runs the repro to `SYNCED` before GC, through NINE
collections, and after — on the old instance and a fresh one. (One rig trap
re-paid en route: the first netboot image was built without
`MODUS_RPI_MINIUART=1`/`MODUS_RPI_CHAINLOAD=1` and was silent on the board
while green in QEMU — both knobs are now in the MODULATOR notes.)

**Residual, split out as task #282:** some probe *shapes* still hit exactly one
swallowed TYPE-ERROR at a shape-determined point during the load (btree in one
shape, fs.lisp in another), after which the next toplevel form aborts —
deterministic per shape, `MODUS_NO_JIT=1` changes nothing, not the guard band
(#258), x64 clean. A different, smaller bug; it no longer blocks the rung
because a clean shape loads and runs everything.

### 2c. Two smaller, well-understood items

- **The response cap silently truncates.** `%net-resp-cap` is baked at *build*
  time, default 32768, raised only by `MODUS_NET_BUFSZ`
  (`build-rpi-cl-repl.lisp:573-584`). Past the cap `tcp-rx-copy` drops bytes
  **while still reporting the full length**. A dist tarball is far past it. This
  class has already cost one session to a false "HEAD regression" (task #270)
  and is recorded in `reference_net_resp_cap_silent_truncation`. Set
  `MODUS_NET_BUFSZ=400000`; it is not optional.
- **HTTP/1.0 only, no TLS, no chunked encoding, no redirects.** Survivable:
  genuine QL defaults to `http://beta.quicklisp.org/`. Worth knowing before
  something is pointed at an https URL and the failure looks mysterious.

---

## 3. Critical path

Ordered by dependency, not by size.

### Does the RAM disk fit, or is an SD driver needed first?

**It fits, with ~85× margin, and no SD driver is needed for the gate.**

What quicklisp needs on disk, measured against the host install:

| item | size |
|---|---|
| the QL client itself | 296 KB |
| dist index (`releases.txt` 578 KB + `systems.txt` 470 KB) | ~1 MB |
| alexandria tarball | 57 KB |
| **minimal quickload-alexandria** | **~2 MB** |
| whole host tree, dozens of libraries installed | 49 MB |

What the board has (448 MiB confirmed by U-Boot on the hardware, not assumed —
not 512, not 496):

```
0x00300000  image (netboot)        ~60 MB, ends ~0x03F00000
0x08000000  stack top (grows down)
0x09000000  Cheney from-space   ┐
0x0C800000  semispace midpoint  ├─ 112 MB heap = 2 x 56 MB
0x10000000  heap end / metadata ┘
0x11000000  USB DMA window (Device, 2 MB)
0x11200000  ---- Normal-WB, cached, UNUSED ----┐
   ...      SSH builds park actors at 0x12/0x13/0x16000000
0x1C000000  top of physical DRAM               ┘  ~174 MB free (REPL build)
```

`0x11200000-0x3EFFFFFF` is **already mapped Normal-WB** by the boot page tables
(`boot-rpi-cl.lisp:338-343`), so this needs no MMU work. In an SSH build the
actor regions carve it up and roughly 45 MB remains between `ssh-ipc-base` and
`actor-heap-base` — still ample for 2 MB.

**Put the RAM disk OUTSIDE the Lisp heap**, as a raw block device in that
region, rather than as a Lisp array inside it. Three reasons, in order of
weight:

1. **pagetree's interface already *is* a block device** (~5 ops). A raw memory
   range with read-block/write-block is the direct implementation — nothing to
   adapt or wrap.
2. **Inside the heap, the GC copies the whole filesystem on every collection,
   forever.** FS contents are permanently live — they never become garbage — so
   in a Cheney collector every byte is copied at every single collection. It
   also consumes semispace that the compiler needs for its ~1.7 MB/form of
   transient garbage (task #188), in a 56 MB space that already hosts 512 KB JIT
   code arrays and a 400 KB network buffer.
3. **The SD driver later drops in behind the same five operations.** The RAM
   disk is not throwaway scaffolding; it is the same interface with a different
   backing store.

SD/eMMC is required only for a filesystem that **survives a reboot**. The gate
does not ask for that, and putting a BCM SDHOST driver on the critical path
would be an expensive way to learn nothing new.

**1 — ~~Fix #281~~ DONE (2026-08-29, de6b02c + b781961; §2d).** Runtime CLOS
survives collections on aarch64, and cabinet's full stack loads and passes its
smoke test there. What replaced it, much smaller: **#282**, a shape-determined
single swallowed TYPE-ERROR during heavy LOAD on aa64 (JIT-independent, one
probe shape hits it, another loads everything clean). Chase it before trusting
long unattended loads on the board, but it does not block the next steps.

**2 — Gate and merge the filesystem seam** (branch `cabinet-fs`, `a325f25`).
64-shard run, because `%FS-READ-CHAR` is on every file read. The design is
proven end to end on x64; what is unproven is that it is regression-free and
that it works on bare metal.

**3 — Storage on the board.** `(cabinet:format-fs nil)` needs **no block
driver** — `mem-device` is pure CL, an adjustable vector of
`(unsigned-byte 8)` pages (`pagetree/src/device.lisp:29-60`). So the bare-metal
v1 is: bake or fetch cabinet+pagetree source, load it, `cabinet-mount`. Cabinet
already passes **6,823 checks / 0 failures** on Modus, and as of 2026-08-29 all
10 of its and pagetree's files load AND the format-fs/round-trip smoke passes
on aarch64 (hosted, through 16 collections).

**4 — Vendor the genuine client.** With a filesystem, the hosted mechanism
(`cl:load` the real client) works unchanged. It brings its own
`deflate.lisp`/`minitar.lisp`, so chipz is not needed.

**5 — Populate `~/quicklisp/` at boot**, since an in-RAM FS starts empty and
does not survive reset. Baked-and-unpacked is deterministic; fetched is closer
to the real thing.

**6 — Size the network.** `MODUS_NET_BUFSZ=400000`, and confirm fetched
**content**, never the reported count.

**7 — Run it on silicon.** The rig works and the board is healthy (§4).

## 4. Risks

**Almost all measurement is x64.** This is the standing lesson from task #211: a
change that gates clean on the surface it was measured on can break the surface
it wasn't. The 64-shard ANSI gate, the library ladder, and the JIT differential
harness are all x64. The aarch64 arm of most of this is thinner than it looks.

**Bare metal punishes what hosted forgives.** Two already-paid examples:
zeroed DRAM (QEMU zero-fills; silicon does not —
`reference_baremetal_needs_zeroed_dram`), and allocation overshoot past the
semispace end onto the GC config page, which presented as *three* unrelated-looking
bugs before one 8 MB guard fixed all of them (`reference_alloc_overshoot_guard_band`).
Expect the filesystem work to surface a third.

**Every CPU fault on bare-metal RPi is a silent `b .` spin.** There is no
console for a data abort. The method that works is PC-sampling over QEMU's
gdbstub and diffing RAM against the image file
(`reference_baremetal_faults_are_invisible`).

**Hardware, verified 2026-08-29: the rig works and the board is healthy.**

The Pi Zero 2 W does **not** answer on the network and has no SSH — it is
attached to the Pi 5 (`modus-pi`) by **GPIO and UART**, not by USB or IP:
`/dev/ttyAMA0` for console, GPIO 17 → the Zero's RUN pin for reset
(`~/pi5-reset-zero.sh`). Do not conclude anything about it from `ping`,
`ssh modulator`, or `lsusb` — none of those instruments can see it. That
mistake was made once on this date and produced a confident "the board is
powered off" that was simply false.

The working check is `~/netboot-modus.py --img <name> --capture N [--send FORM]`,
which resets, interrupts U-Boot autoboot, TFTPs from `modus-pi:/srv/tftp`, runs
`go 0x300000`, and streams serial. Measured on this date:

```
DRAM:  448 MiB                              <- U-Boot, on the board
63341720 bytes transferred (TFTP)
## Starting application at 0x00300000 ...
BOOT / MODUS-CL / DTB ptr=1
E2SMOKE-START add=3 sqr=25 defcall=49 persist-call=36 persist-fn=45 E2SMOKE-END
Modus CL REPL (bare metal).  EVAL = MVM-EVAL.
> (+ 2 3)                          => 5
> (length (list 1 2 3 4))          => 4
```

So step 5 is **not** blocked on hardware. Two caveats found while confirming it:

- **The saved U-Boot env boots from the SD card, not the network:**
  `bootcmd = usb start; fatload mmc 0:1 0x300000 modus-ssh.img; go 0x300000`,
  and `ipaddr`/`serverip` are unset. A plain reset therefore loads whatever
  `modus-ssh.img` is on the card — currently an image that starts and prints
  **nothing** for 75 s. Netbooting a known image is the way to test; don't read
  the card's default as a platform result.
- A fixed-size `fatload` is deterministic to the millisecond across resets
  (61,792,816 bytes / 2578 ms, twice). That looks like a suspicious coincidence
  if you have assumed it was a network transfer. It is not one.

---

## 5. Honest summary

Updated 2026-08-29, after a session that moved the answer.

**The gap this document was written to name is closed on x64.** CL's `OPEN` and
`LOAD` now work with no operating system underneath them — `LOAD` reads Lisp
source out of an in-RAM cabinet filesystem, compiles it, and runs it. That was
"the one subsystem", and it took one seam in `cl-fileio.lisp` plus a loadable
shim, not a rewrite.

**The blocker that replaced it is now fixed too.** #281 turned out to be one
missing runtime flag: the aarch64 in-image JIT emitted every allocation with no
GC bitmap bit, and the validating collector then refused to forward any
reference to those objects — a runtime `defmethod`'s specializer list being the
first casualty anyone noticed (§2d, commits de6b02c + b781961). With it fixed,
**cabinet loads and runs on aarch64** — the filesystem is no longer x64-only.
The fix is worth more than cabinet: it un-breaks *every* CLOS-using library on
the Pi's architecture, and the same enable is now default-ON in the RPi image.

What has not changed: the parts that could have stayed impossible — booting on
silicon, an RTL8153 driver written from scratch, JIT-compiling on the board,
fetching a real library over real HTTP and running it — remain done, and the
board was re-verified healthy on this date. What is left still has names and
line numbers rather than mysteries.

The honest caveats: the filesystem seam is still **ungated** and has **never
run on bare metal** (the *fix* is silicon-validated — nine collections with
runtime CLOS surviving on the Zero 2 W — but cabinet itself has only run
hosted); and #282 (one shape-determined swallowed TYPE-ERROR during heavy
loads on aa64) is open, characterized, and smaller than anything it replaced.
