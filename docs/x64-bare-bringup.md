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
| Pipeline | **`DHCP:F` — the pipeline's own poll gives up before the reply lands** |

## The open problem

`dhcp-client` (net/ip.lisp) polls 500 times, one `io-delay` per poll. On x64 the
whole window is 53 ms (serial timestamps: `DHCP:D` → `DHCP:F`). The first reply
after `e1000-init` takes about 60 ms to reach the ring, so the window closes first.

Measured with a REPL loop counting interpreted `RDH` reads until the head moves
(`boardstage`-free, all from the serial REPL):

| Condition | Iterations until reply |
|-----------|------------------------|
| right after `(e1000-init)` + discover, run 1 | 3174 |
| same, run 2 | 3177 |
| same, plus a rewrite of RDT after discover | 3391 |
| discover with NO re-init (NIC already settled) | 0 |

So the latency is a fixed post-initialisation delay inside QEMU's E1000 model
(link/autoneg settling after the driver's reset, most likely), not a queue-flush
problem: rewriting RDT does not release it, and once the NIC has settled the
reply is synchronous. The option ROM is not the cause (`romfile=,rombar=0`
fails the same way). The aarch64 rig never hit this because its poll loop runs
slower per iteration and stays open long enough.

Two candidate fixes, pick one:

1. Wait for link-up before the first DHCP DISCOVER. After `e1000-init`, poll
   STATUS (reg 0x08) bit 1 (LU) with a bounded spin, in the pipeline or at the end
   of `e1000-init`. This is device-correct and arch-neutral. Measure LU timing
   first with the probe below; if LU is already set immediately after init, the
   delay is something else and option 2 applies.
2. Widen the poll: make `io-delay` in `net/arch-x86-cl.lisp` spin longer (it is
   5000 RAM reads now, about 100 µs native). Simple, but it also slows every idle
   poll in the TCP receive loops by the same factor.

Do NOT reintroduce a port-0x80 spin in `io-delay`: 5000 PIO exits per call take
the QEMU big lock and starve the iothread. That was the first (wrong) theory and
it is already fixed.

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
