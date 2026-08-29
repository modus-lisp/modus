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

### 2b. There is no filesystem — this is the real remaining problem

`ql:quickload` is not a function that fetches a tarball. It is a program that
reads and writes a directory tree: `setup.lisp`, `config.lisp`, `dist.lisp`,
`local-projects.lisp` all call `cl:load` / `open` / `probe-file` /
`ensure-directories-exist` against real paths, and the dist machinery caches
metadata and fasls under `~/quicklisp/`.

On the bare-metal image:

- `net/hosted-storage.lisp` is **hosted-gated** (`build-cli-common.lisp:883`);
  bare metal gets no storage layer.
- There is **no SD/eMMC/SDHOST block driver**. Searched
  `build-rpi-cl-repl.lisp` and `build-cli-common.lisp` for `mmc|emmc|sdhost|block`
  — zero hits. Stating that explicitly because an empty grep is not evidence on
  its own; the absence is corroborated by `build-cli-common.lisp:871`, which
  names "pagetree/cabinet" as what bare metal *would* need.
- `mvm/cl-fileio.lisp` **is** baked (it sits above the bare-metal cut at
  `build-cli-common.lisp:928`), but every primitive is a raw Linux syscall —
  `%sys-open-rdonly` → `(syscall3 2 …)` (`cl-fileio.lisp:67-99`). On bare metal
  those are "a literal SVC with no OS behind it" (`build-cli-common.lisp:1021-1024`).
  So `OPEN` and `LOAD` exist as symbols and **fault when called**. That is worse
  than absent: it fails late and in the dark.
- `*filesystem*` (`cl-fileio.lisp:682`, "alist of (path . content) for
  bare-metal use") is set to `nil` by the RPi build
  (`build-rpi-cl-repl.lisp:329`). Repo-wide it is written by six build scripts
  and **read by nothing** — a vestigial defvar, not a working in-memory FS.

**This is the gate.** Everything else on the list is hours of plumbing; this is
a subsystem.

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

**1 — Storage. `cabinet`/`pagetree` as the Modus filesystem (task #279).**
This is the intended answer and it is already most of the way there. `pagetree`
is a copy-on-write B+tree over a ~5-operation block device; `cabinet` is a
POSIX-ish filesystem on top of it. Both were written for this — pagetree's
`src/device.lisp` says "a disk driver on modus tomorrow" — and `(cabinet:format-fs
nil)` gives an **in-RAM** filesystem, so the bare-metal v1 needs no block driver
at all.

Status: cabinet's own differential-oracle suite runs **on Modus** at
**6,823 checks / 0 failures**, including a persistence round trip (1500 fuzzed
ops against a real file → unmount → remount → compare against the model).
Getting there found and fixed three Modus bugs (package-local nicknames,
condition identity across a nested-interpret boundary, `COERCE` silently
ignoring `ARRAY`/`SIMPLE-ARRAY`) — all now on main.

Remaining for this step: wire cabinet under the pathname/stream layer so
`OPEN`/`LOAD`/`PROBE-FILE`/`ENSURE-DIRECTORIES-EXIST` route to it instead of to
naked syscalls. Sources are at `/home/claude/modus-lisp/{cabinet,pagetree}`;
they are not yet vendored into this repo.

**2 — Vendor the genuine client.** Once there is a filesystem, the hosted
mechanism (`cl:load` the real client) works unchanged. The client brings its own
`deflate.lisp`/`minitar.lisp`, so chipz is not needed — note that
`install-tarball`'s chipz calls are prefix-stripped to bare `decompress`/`gzip`
(`build-cli-common.lisp:154-164`) and are dangling names on a never-taken path.

**3 — Populate `~/quicklisp/` at boot.** The in-RAM FS starts empty and does not
survive reset, so the client's tree has to be materialised each boot — either
baked as a payload and unpacked into cabinet, or fetched. Decide which; the
former is deterministic and testable, the latter is closer to the real thing.

**4 — Size the network.** `MODUS_NET_BUFSZ=400000`, and confirm the fetched byte
**content**, never the reported count.

**5 — Run it on silicon.** Not QEMU. See the risk below.

---

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

**Hardware, as of 2026-08-29:** `modulator` (Pi Zero 2 W) is **unreachable**
(`no route to host`) — same as on 2026-08-15. `modus-pi` is up (Pi 5, aarch64,
Linux) and is the TFTP head of the netboot rig. So silicon validation currently
runs through modus-pi; the Pi Zero's own reachability needs restoring before
step 5.

---

## 5. Honest summary

The hard, uncertain parts are behind us. Booting on silicon, driving USB
ethernet from scratch, JIT-compiling on the board, fetching a real library over
real HTTP and running it — those were the things that could have stayed
impossible, and they don't have open questions attached to them any more.

What remains is **one subsystem** (a filesystem, already written and already
passing 6,823 of its own checks on Modus) plus **plumbing** (vendor a client,
populate a directory, raise a buffer). The failures left in this project have
names and line numbers rather than mysteries, and that is a different kind of
hard than where it started.
