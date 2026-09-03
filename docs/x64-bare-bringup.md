# Bare-metal x86-64 bring-up — handoff (2026-09-03)

Goal: a bare-metal x64 QEMU image that runs the same fetch → install → `ql:quickload`
pipeline the aarch64 virt rig runs, so quickload bring-up can be debugged on the
x64 box (TCG only, no KVM here) instead of on the Pi.

## Where it stands

Done and committed on branch `perf-specials-306` (commit `28dc451`, not pushed):

- `mvm/build-x64-cl-repl.lisp` is now a two-line head over
  `mvm/build-cl-repl-common.lisp`, exactly like `mvm/build-aarch64.lisp`.
  Platform selector: `(defvar *cl-repl-platform* :x64)`.
- Every x86 difference is a `:X64` arm at a marked `DIVERGENCE` site in the
  common file. Grep `cl-repl-x64-p` to find them all (kernel prologue, io-scratch
  without the DTB probe, NIC source, pipeline, `boot/boot-x64.lisp` descriptor and
  console, memory-map asserts, build target, output path, QEMU run line).
- `net/arch-x86-cl.lisp` is the QEMU-pc CL network adapter. It defines the same
  function set as `net/arch-aarch64-cl.lisp` (verified with `comm`): port-I/O PCI
  config at 0xCF8/0xCFC, BARs left as SeaBIOS assigned, E1000 DMA region at
  `0x0C000000..0x0C113000`, single-threaded actor stubs.
- JIT is OFF by default on this platform. `MODUS_X64_JIT=1` opts in (untested).

Verified under TCG:

| Step | Result |
|------|--------|
| Build (`MODUS_NET_BUILD=1`) | 38 MB image, memory-map asserts pass |
| Boot | prints `MODUS-CL`, E2SMOKE passes, REPL up, `(+ 1 2)` = 3 |
| NIC | E1000 found at BAR0 FEBC0000, MAC read, RX/TX rings programmed |
| DHCP TX | DISCOVER on the wire; slirp answers with an OFFER 100 µs later (pcap) |
| DHCP RX | OFFER lands in RX descriptor 0, RDH advances to 1 |
| DHCP parse | `(dhcp-client)` from the REPL, with the offer queued, gets 10.0.2.15 |
| Pipeline | **`(run-net-pipeline)` gets `IP=10.0.2.15` / `GW=10.0.2.2`** (fixed 2026-09-03) |

## DHCP: SOLVED (2026-09-03)

The first DISCOVER after a NIC reset draws its reply only after a fixed
post-reset settle inside QEMU's E1000 model. `STATUS.LU` (reg 0x08 bit 1) is
already set immediately after `e1000-init` — measured `LU=0`, `RDH` advances at
~1840 io-paced polls — so waiting for link-up (the old option 1) is a no-op and
does not help. The settle is wall-clock bound (~60 ms), and one `io-delay` is a
pure RAM-read spin whose wall cost varies wildly under TCG, so a single
poll-*count* window (500, or even 2500) catches the first reply only
unreliably. The decisive tell: a **second** DISCOVER once the NIC has cycled
once is answered *immediately* (`RAW=(0 590 17 67)` — offer at poll 0).

Fix — two changes, both device-correct and confirmed reliable across runs (DHCP
lands on attempt 4 of 8, `IP=10.0.2.15`):

1. `run-net-pipeline` (x64 arm, `mvm/build-cl-repl-common.lisp`) **re-runs
   `dhcp-client`** up to 8 times until `state+0x18` holds an address. The
   failing early attempts burn the settle wall-time and each re-sends DISCOVER,
   so a later attempt hits the fast (already-settled) path. This is ordinary
   DHCP retry behaviour.
2. `dhcp-client` (`net/ip.lisp`) offer/ack poll caps widened 500 → 2500 so each
   attempt spans more settle wall-time. Shared with aarch64/rpi but a strict
   no-op there — the loop exits early on the first offer.

Do NOT reintroduce a port-0x80 spin in `io-delay`: 5000 PIO exits per call take
the QEMU big lock and starve the iothread. (A *single* PIO exit per call to
schedule the iothread was tried and works, but is unnecessary once the pipeline
retries — pure-spin + retry is reliable — so `io-delay` stays a pure RAM read.)

## Fetch → install → load → call: WORKING (2026-09-03)

The whole rung-3 ladder passes on QEMU x64. With DHCP up and the fetch pointed at
a live tarball server, `(run-net-pipeline)` completes in ~95 s under TCG:

```
FETCHED bytes=20480  HEAD4=73 68 61 31   ("sha1" — the tarball)
LIB-SYSTEM=sha1                          (install-tarball parsed + installed it)
LIB-EXPR=(sha1:sha1-hex "abc")
LIB-VALUE=A9993E364706816ABA3E25717850C26C9CD0D89D   (correct SHA-1 of "abc")
```

### The port trap that looked like "no server"

The image bakes the fetch URL at build time; the default is
`http://10.0.2.2:8080/sha1.tar` (`10.0.2.2` is slirp's route to the host).  On
this box **port 8080 is held by an unrelated long-running Hunchentoot app that
404s every path** — so the guest got a 9-byte `"not "` body and
`install-tarball` reported `<install-failed>`.  The tarball server was actually
up on **8086** the whole time.  Fix is a build-time repoint, no new server:

```
MODUS_NET_URL=http://10.0.2.2:8086/sha1.tar   # add to the build env
```

(Confirm the port first: `curl -sI http://127.0.0.1:8086/sha1.tar` should be
`200` with `Content-Length: 20480`.  `systems/sha1.tar` in the repo is the same
20480-byte `sha1-20211020-git` tarball if you need to (re)start a server:
`python3 -m http.server 8086 --directory systems`.)

## `ql:quickload :alexandria` over the network: WORKING (2026-09-03)

The image bakes a `QL` package whose `ql:quickload` fetches `<MODUS_QL_BASE><name>.tar`
over HTTP and installs it from the in-memory bytes (`net-fetch-bytes` ->
`install-tarball-from-bytes`).  `run-net-pipeline` calls `ql-net-setup` once
DHCP is up, so `ql:quickload` is nameable at the REPL.  End to end on bare metal:

```
(run-net-pipeline)
(ql:quickload :alexandria)
  ; ql:quickload alexandria <- http://10.0.2.2:8086/alexandria.tar
  FETCHED bytes=276480
  install-tarball: done, system=alexandria
(alexandria:flatten (list 1 (list 2 (list 3))))   ; => (1 2 3)
```

Timing under TCG (JIT off on this platform — pure interpret): fetch 0.47 s,
untar+compile+load ~36.7 s, ~37 s total; the whole cost is compiling alexandria's
~200 forms through the interpreter, the transport is negligible.

### The GC bug this uncovered (fixed)

The alexandria load first corrupted mid-way — symbols came back with empty name
strings, so the install died with read/eval errors, and the failure point moved
with the semispace size (a temporary boot-x64 sweep: 24 MB -> at boot, 64 MB
-> form 37, 112 MB -> form 117), i.e. GC-triggered.  Root cause: the MCGC
**cons-kind bitmap check** addressed its bitmap at `[bitmap_base] +
+mcgc-kindbitmap-delta+`, and that delta (`#xFE4000`) is a LINUX-x64 layout
constant — `boot-x64.lisp` lays the GC metadata out differently, so on bare the
check read a wrong, uninitialised region and falsely rejected valid conservative
roots, dropping live objects across a collection.  A self-host
`null -> *x64-gc-enabled*` routing had silently turned this check on for bare
against `build-x64.lisp:1356`'s stated intent.  Fix (build-cl-repl-common, x64
arm): `(setf *ws5-force-no-kindcheck* t)` — the object-start bitmap alone
validates roots correctly.  Follow-up: make the kind-bitmap delta layout-agnostic
(a config word filled by `boot-x64`) and re-enable the check on bare; and check
whether `build-x64.lisp` (the bare ANSI gate runner) is losing tests to the same
latent bug.

## How to build and run

```
cd /home/claude/modus
MODUS_NET_BUILD=1 MODUS_NET_NOAUTO=1 MODUS_NET_BUFSZ=400000 \
MODUS_CL_REPL_OUT=/home/claude/cabfs/x64bare/modus-x64-cl-repl.bin \
sbcl --dynamic-space-size 12288 --script mvm/build-x64-cl-repl.lisp
```

Build takes about 4 minutes. A driver script with the same knobs is at
`/home/claude/cabfs/x64bare/run.sh`; it writes `build.log` and `result.txt`.

```
qemu-system-x86_64 -m 512 -kernel /home/claude/cabfs/x64bare/modus-x64-cl-repl.bin \
  -display none -serial stdio -no-reboot \
  -device e1000,netdev=net0 -netdev user,id=net0
```

The REPL is on the serial port. It needs about 45 s under TCG before it reads
input, so pipe forms in after a sleep, e.g.
`(sleep 45; printf '(run-net-pipeline)\n'; sleep 60) | timeout 120 qemu-system-x86_64 ...`.
Bytes sent before the banner are lost.

Packet capture: add
`-object filter-dump,id=f1,netdev=net0,file=/home/claude/cabfs/x64bare/net.pcap`.
There is no tcpdump on the box; the small parser used for the runs above is in
the session logs and is a 15-line struct.unpack loop over the pcap.

Useful REPL probes (all worked in the runs above):

```lisp
(run-net-pipeline)                                   ; the whole thing, prints DHCP:D / DHCP:F
(list :rdh (e1000-read-reg #x2810) :rdt (e1000-read-reg #x2818)
      :status (e1000-read-reg 8) :rctl (e1000-read-reg #x100))
(e1000-receive)                                       ; length of the next RX packet, 0 if none
(let ((h0 (e1000-read-reg #x2810)) (n -1))            ; iterations until a reply lands
  (dhcp-discover)
  (dotimes (i 20000)
    (when (and (< n 0) (not (eq (e1000-read-reg #x2810) h0))) (setq n i)))
  n)
(dhcp-client)                                         ; the baked client; works once an offer is queued
```

Note `mem-ref :u64` results print halved at the REPL (raw bits shown as a
fixnum); `:u8`/`:u16`/`:u32` loads are tagged and print correctly.

## After DHCP works

The pipeline continues into the tarball fetch and `install-tarball`, then the
quickload driver. The board-side plan and the known quickload stall (in-image
gunzip/untar before the first file compiles) are in the memory notes
`reference_pi5_quickload_phase_split` and the aarch64 rig script `ql-kvm10.sh`.
Expect the x64 run to be slow under TCG; it is a debugging rig, not a benchmark.

## Things that bit me during the port

- The common file is loaded by three heads. Every `(if *cl-repl-virt-p* A B)`
  I turned into a `cond` needed its paren count re-checked. A paren-depth
  script that reports any column-0 form starting at nonzero depth found the
  three misplaced parens in one pass; SBCL's "unmatched close parenthesis"
  only reports the first.
- The rpi kernel prologue string ends with comment lines inside the string, so
  anchors on "the line after the last form" are wrong. Anchor on the `")` line.
- Acceptance gate before merging to main: `scripts/acceptance-gate.sh <base> <fix> <workdir>`
  with `MODUS_ANSI_OUT` set. Not yet run for this port; the build-common change
  also touches the aarch64 and rpi heads, so rebuild `build-aarch64.lisp` and
  confirm byte-identity or a clean boot before merging.
