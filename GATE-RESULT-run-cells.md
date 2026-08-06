# GATE-RESULT: proving `scripts/run.sh`, and retiring what it replaces

Branch `run-cells`, off `main` @ `4a8a1f0`.  Unpushed.

## Headline

**`run-*.sh`: 24 → 19.**  Five retired, each gated on the same target being
demonstrated through `run.sh` first.

`run.sh` itself was **not** working when this started.  Proving it turned up
three real defects, all fixed here:

1. **The ssh readiness test was a lie.**  `run.sh` waited for the forwarded
   port with a bash `/dev/tcp` probe.  QEMU binds a `hostfwd` port the moment
   it starts, so that probe succeeded on the *first* poll — before the guest
   had reached its NIC init — and the smoke test then ran against nothing:

   ```
   run.sh: x64-ssh booting (pid 1166906), ssh on port 2222, log /tmp/modus-x64-ssh.log
   run.sh: port 2222 open — smoke test
   Connection timed out during banner exchange
   ```

   …and `run.sh` exited **0**.  A silent false pass, on every ssh target.
   Fixed: wait for the guest's own log marker (`SSH:` on x64/aarch64/arm32/rpi,
   `DHCP:IP=` on i386), transcribed from the scripts being replaced, with each
   script's own poll budget and settle delay; and **exit non-zero** when the
   smoke test fails.

2. **The build heap was too small for the biggest cell.**  `run.sh` ran
   `sbcl --script mvm/build.lisp <cell>` with SBCL's default dynamic space.
   `x64-cl-repl` heap-exhausts inside `BUILD-IMAGE`:
   `Unhandled SB-KERNEL::HEAP-EXHAUSTED-ERROR … 14: (BUILD-IMAGE :TARGET :X86-64 …)`
   and leaves no image behind.  Fixed: `--dynamic-space-size ${SBCL_DYNAMIC_SPACE:-12288}`
   (the value `mvm/BUILDS.md`'s own audit recipe uses).

3. **`x64-cl-repl` named an image that is never produced.**  The table said
   `/tmp/modus-x64-cl.bin`; `mvm/build-x64-cl-repl.lisp:1135` writes
   `/tmp/modus-x64-cl-repl.bin`.  The row could never have booted.  Fixed.
   (`mvm/build.lisp`'s row for that cell carries the same wrong string; it is
   only printed by `--list` there, so it is left for the build side to correct.)

Two more corrections that are not "run.sh was broken" but were wrong anyway:

4. **The four `rpi-*` rows had drifted from the scripts they claim to
   transcribe** — `rpi-repl` lost `-semihosting` and gained `-m 1G -nographic`;
   `rpi-ssh` was declared `mode=ssh` with an **empty net device**, so it could
   never have forwarded a port; `rpi-hid` lost `-device usb-kbd`;
   `rpi-periph` pointed at `/tmp/kernel8.img` instead of
   `/tmp/piboot/kernel8.img` and lost `-serial null -serial stdio -display gtk`.
   All four rows are now verbatim.  They are still **unprovable** (below), so
   their runners are **not** retired.

5. `run-repl-eval.sh` had no `rpi` arch, so `run.sh rpi-repl "expr"` would have
   died with `Unknown arch: rpi`.  Added, transcribed from `run-rpi-repl.sh`.

## Part 1 — per-target proof

| target | mode | how proven | result | runner retired |
|---|---|---|---|---|
| `x64-repl` | repl | `run.sh x64-repl "x"` → `nil` | **PASS** | yes — `run-x64-repl.sh` |
| `x64-console-repl` | repl | `run.sh x64-console-repl "x"` → `nil` | **PASS** | yes — `run-x64-console-repl.sh` |
| `x64-cl-repl` | repl | `run.sh x64-cl-repl "(+ 1 2)"` → **`3`** | **PASS** | n/a (never had a runner) |
| `aarch64-repl` | repl | `run.sh aarch64-repl "x"` → `nil` | **PASS** | yes — `run-aarch64-repl.sh` |
| `i386-repl` | repl | `run.sh i386-repl "x"` → `nil` | **PASS** | yes — `run-i386-repl.sh` |
| `arm32-repl` | repl | `run.sh arm32-repl "x"` → `nil` | **PASS** | yes — `run-arm32-repl.sh` |
| `x64-ssh` | ssh | boots, stalls in `dhcp-discover`; **the retired-candidate script fails identically** | **CANNOT PROVE** (payload) | no |
| `aarch64-ssh` | ssh | same stall | **CANNOT PROVE** (payload) | no |
| `aarch64-actors` | ssh | boots to `> ` prompt, never announces sshd | **CANNOT PROVE** (payload) | no |
| `aarch64-isolated` | ssh | same | **CANNOT PROVE** (payload) | no |
| `i386-ssh` | ssh | QEMU exits ~10 s after `Booting from ROM..` | **CANNOT PROVE** (payload) | no |
| `arm32-ssh` | ssh | boots, then `Slirp: Failed to send packet, ret: -1` forever | **CANNOT PROVE** (payload) | no |
| `rpi-repl` | repl | build fails: `#S(A64-BUFFER …) is not of type MODUS.MVM::MVM-BUFFER` | **CANNOT PROVE** (build, #209) | no |
| `rpi-ssh` | ssh | same build failure | **CANNOT PROVE** (build, #209) | no |
| `rpi-hid` | repl | same build failure | **CANNOT PROVE** (build, #209) | no |
| `rpi-periph` | repl | same build failure | **CANNOT PROVE** (build, #209) | no |

**6 of 16 proven.**  The other 10 are blocked by the *payload*, not by
`run.sh` — see the evidence below.

### REPL transcripts (all six through `run.sh`, nothing else)

```
=== x64-repl  expr: x                === aarch64-repl  expr: x
rc=0  out=[nil]                      rc=0  out=[nil]
=== x64-repl  expr: (+ 1 2)          === aarch64-repl  expr: (+ 1 2)
rc=0  out=[]                         rc=0  out=[]

=== i386-repl  expr: x               === arm32-repl  expr: x
rc=0  out=[nil]                      rc=0  out=[nil]
=== i386-repl  expr: (+ 1 2)         === arm32-repl  expr: (+ 1 2)
rc=0  out=[]                         rc=0  out=[]

=== x64-console-repl  expr: x        === x64-cl-repl  expr: x
rc=0  out=[nil]                      rc=0  out=[]
=== x64-console-repl  expr: (+ 1 2)  === x64-cl-repl  expr: (+ 1 2)
rc=0  out=[]                         rc=0  out=[3]
```

Reproduced on a second, sequential pass on an otherwise idle box — each of
these forced a **full rebuild through `run.sh`'s own build path** first, so the
build side of every row is exercised too:

```
x64-repl             x       -> [nil]
aarch64-repl         x       -> [nil]
i386-repl            x       -> [nil]
arm32-repl           x       -> [nil]
x64-console-repl     x       -> [nil]
x64-cl-repl          (+ 1 2) -> [3]
```

Reading these:

- Reaching the prompt at all is the pass: `run-repl-eval.sh` aborts with
  `Timeout waiting for prompt` if the REPL banner never appears, and none of
  the six did that.  Every one booted and evaluated.
- `x` → `nil` on the five `repl-source.lisp` images and `(+ 1 2)` → nothing is
  **#204 rot, not a runner defect** — and it is now measured on **four**
  arches, not the two the issue records: i386 and arm32 do it too, which is
  what you would expect from one shared second-Lisp source.
- `x64-cl-repl` is the counter-example that makes the point: the image whose
  REPL is the real CL answers `(+ 1 2)` → `3` through the identical harness.
  (Its `x` → nothing is the global-var READ gap its own header documents.)
  This is also the row that could never have run before fix #2 and #3.

### The ssh targets: it is the payload, and here is the proof

The strongest single piece of evidence is that **the images are byte-identical
to the ones `mvm/BUILDS.md` already certifies**.  Every one was rebuilt in this
worktree through `run.sh`'s own build path, and every md5 matches the table in
`BUILDS.md` / `GATE-RESULT-ssh-cells.md`:

```
a168c3fe76e313cf9c644b7b5af0ac3d  /tmp/modus-aarch64-ssh.bin
782c7414cf3ad555c302500942004a8c  /tmp/modus-x64-ssh.bin
98cc10becd7a7da404ba700d86fd6ce2  /tmp/modus-aarch64-actors.bin
2083abab509dca80f930a36cffee9a5b  /tmp/modus-aarch64-isolated.bin
dcadafd144d94459e6aac9302998c5f9  /tmp/modus-i386-ssh.bin
269b461a764016eea6533c46798ad3e4  /tmp/modus-x64.bin
fd0d40b12e984e064f3713ce03ed21e8  /tmp/modus-aarch64.bin
```

So nothing about *this* branch produced the failure.  Three independent
harnesses then fail the same way:

1. **`run.sh`** with the fixed marker wait — and it now fails *loudly*:

   ```
   $ ./scripts/run.sh x64-ssh
   run.sh: x64-ssh booting (pid 1216207), ssh on port 2222, log /tmp/modus-x64-ssh.log
   run.sh: timed out waiting for 'SSH:' — last 20 log lines:
   SeaBIOS (version 1.16.2-debian-1.16.2-1)
   Booting from ROM..E1000:FEBC0000
   MAC:52:54:00:12:34:56
   E1000:OK
   [1][2][3]EI
   [4][5]
   rc=1
   ```
   Compare that with the `Connection timed out during banner exchange` / exit-0
   transcript at the top: same box, same image, fix working.

2. **The scripts `run.sh` would have replaced**, run exactly as `README.md`
   documents them:

   ```
   $ ./scripts/run-x64-ssh.sh 2431 "(+ 1 2)"      $ ./scripts/run-aarch64-ssh.sh 2431 "(+ 1 2)"
   Booting...                                     Booting...
   Timed out waiting for SSH. Log:                Timed out waiting for SSH server. Log:
   Booting from ROM..E1000:FEBC0000               E1000:10000000
   MAC:52:54:00:12:34:56                          MAC:52:54:00:12:34:56
   E1000:OK                                       E1000:OK
   [1][2][3]EI                                    [1][2][3]EI
   [4][5]                                         [4][5]

   $ ./scripts/run-i386-ssh.sh 2431 "(+ 1 2)"
   Booting...
   QEMU exited unexpectedly. Log:
   SeaBIOS (version 1.16.2-debian-1.16.2-1)
   Booting from ROM..
   ```
   All three `rc=0`, incidentally — `cleanup` ends in `exit 0`, so the `exit 1`
   after it is dead code.  The old scripts had exactly the same
   silent-false-pass shape `run.sh` did; the consolidated one no longer does.

3. **The project's own feature test**, run alone on an idle box:

   ```
   $ bash test/features/aarch64-ssh.sh
   Building AArch64 SSH kernel...
   FAIL: aarch64-ssh — timeout waiting for 'SSH:'
   E1000:10000000
   MAC:52:54:00:12:34:56
   E1000:OK
   [1][2][3]EI
   [4][5]
   rc=1
   ```
   (`test/features/aarch64-e1000.sh` only waits for `E1000:OK`, which *does*
   appear — so the suite has never covered the part that is broken.)

4. **A bare QEMU boot with no port forwarding at all** (`-netdev user,id=net0`),
   150 s, both arches — identical stall.  So `hostfwd` is not implicated.

Where it stalls is precise.  `net/ip.lisp:1222 dhcp-client` prints `DHCP:D`
*immediately after* `(dhcp-discover)` returns.  The log stops at `[5]` — the
marker printed *before* `dhcp-client` is entered — and `DHCP:D` never appears.
So the guest is wedged **inside `dhcp-discover`**, i.e. in the E1000 transmit
path, before any receive polling happens.  `E1000:OK` and the BAR address do
print, so probe/BAR assignment are fine.

Environment: QEMU 7.2.22 (Debian 1:7.2+dfsg-7+deb12u18).

One tempting lead that is **not** the tell: every SSH build prints
`=== N unresolved calls to 40 functions (!! NO %UNRESOLVED-FN STUB — targeting
offset 0, this is garbage execution) ===`.  So do the REPL builds (15
functions) — and those images boot and evaluate.  Don't start there.

The other three ssh targets fail differently, which is worth recording
separately rather than lumping in:

- `i386-ssh` — QEMU **exits** ~10 s in, right after `Booting from ROM..`, with
  `-no-reboot` set: a guest fault, not a hang.
- `arm32-ssh` — the guest *does* transmit, and slirp rejects every frame:
  `qemu-system-arm: Slirp: Failed to send packet, ret: -1`, repeating.
- `aarch64-actors` / `aarch64-isolated` — boot to a bare `\r\n> ` REPL prompt
  and emit nothing else (no `E1000:` lines) for the whole window.

None of these is a launcher problem, so **none of the six ssh runners is
retired**.  That is the honest boundary: `run.sh`'s ssh mode is now *correct*
(right marker, right flags, right exit code) but it is not *proven*, because
there is nothing working to prove it against on this box.

### The rpi targets

All four builds were attempted through `run.sh`'s build path and all four fail
identically inside `BUILD-IMAGE :TARGET :RPI`:

```
Unhandled TYPE-ERROR …
  #S(MODUS.MVM::A64-BUFFER …) is not of type MODUS.MVM::MVM-BUFFER
0: (MODUS.MVM::EMIT-RPI-ENTRY #S(MODUS.MVM::A64-BUFFER :CODE #(…) :POSITION 23))
```

`aarch64/bare/rpi/repl` rc=1, `build-rpi-ssh` rc=1, `build-rpi-hid` rc=1,
`build-rpi-periph` rc=1.  This is #209, unchanged.  Their runners stay.

## Part 2 — retirements

Five scripts, `git rm`'d, referrers repointed:

| retired | replaced by | proven by |
|---|---|---|
| `scripts/run-x64-repl.sh` | `./scripts/run.sh x64-repl` | `x` → `nil` |
| `scripts/run-x64-console-repl.sh` | `./scripts/run.sh x64-console-repl` | `x` → `nil` |
| `scripts/run-aarch64-repl.sh` | `./scripts/run.sh aarch64-repl` | `x` → `nil` |
| `scripts/run-i386-repl.sh` | `./scripts/run.sh i386-repl` | `x` → `nil` |
| `scripts/run-arm32-repl.sh` | `./scripts/run.sh arm32-repl` | `x` → `nil` |

Referrers (`grep -RIn`, which unlike `-r` follows symlinked dirs), all
repointed:

- `README.md` — the four per-arch REPL quick-start blocks.
- `CONTRIBUTING.md` — Quick Start now leads with `./scripts/run.sh list`.
- `mvm/build-x64-console-repl.lisp` — header comment **and** the "Boot with:"
  line the build prints at the end of a successful build.
- `scripts/run-i386.sh` — the header comment distinguishing the CL image from
  the legacy mini-Lisp.
- `docs/arm32-fixpoint-plan.md` — a historical design doc; the filename is
  annotated `(historical — RETIRED; now ./scripts/run.sh arm32-repl)` rather
  than rewritten, because the doc is a record of what was planned.

After the sweep, `grep -RIn` for the five names returns only those two
deliberate historical mentions.

### Nothing was dropped in the merge

Each retired script was diffed against the row that replaces it:

- **QEMU flags** — verbatim, already.
- **Staleness** — `run-x64-repl.sh` and `run-aarch64-repl.sh` compared the
  image against `mvm/build.lisp`; `run-i386-repl.sh` and `run-arm32-repl.sh`
  against their own `mvm/build-*-repl.lisp`; `run-x64-console-repl.sh` against
  a **hand-maintained list of seven sources** (`repl-source.lisp`, both boot
  files, `compiler.lisp`, `translate-x64.lisp`, …).  (The *unconditional*
  rebuilds `run.sh`'s header calls out are in the `-ssh` scripts —
  `run-aarch64-ssh.sh`, `run-arm32-ssh.sh`, `run-i386-ssh.sh` — which are not
  being retired here.)  `run.sh` now rebuilds if any `*.lisp` under
  `mvm/ boot/ net/ lib/ runtime/` is newer than the image — a **superset** of
  all four, so the console-repl script's wider check is not silently lost.
  This is deliberately over-broad (`build-image` bakes the whole source text,
  comments included, so any of those files *can* change an image) and it costs
  real time: editing one unrelated build script rebuilds every target.
  `MODUS_NO_REBUILD=1` opts out; with no image on disk it errors rather than
  handing QEMU a missing kernel.
- **Batch eval** — `run-x64-console-repl.sh` had its own inline copy of the
  boot-and-eval harness that sent `\n`.  The shared `run-repl-eval.sh` sends
  `\r`, which is what the bare-metal line editor actually terminates on, so
  the consolidated path is strictly *more* correct than the copy it replaces.
  That is also how this run could report `nil` for `x64-console-repl` at all.
- **`no-thp-exec`** — the x64/i386 scripts wrapped QEMU, the aarch64/arm32 ones
  did not.  `run.sh` wraps all of them.
- **Stale-QEMU cleanup** — some `pkill`'d first, some raced.  `run.sh` always
  `pkill`s.  (Its pattern, `"$QEMU.*$(basename "$BIN")"`, cannot self-match:
  `run.sh`'s own argv contains neither the qemu binary name nor the image path.
  A probe harness written for this gate *did* pass both as arguments and
  `pkill -9 -f`'d itself out of existence — twice — which is why the note is
  here.)

### Capability added rather than dropped, on the ssh side

The ssh scripts took `[port] [expr]` positionally and, with no expression,
printed a banner and **blocked** until Ctrl-C.  `run.sh` had `PORT=` and a
fixed `(+ 1 2)` smoke test only.  Now:

- `./scripts/run.sh x64-ssh "(* 6 7)"` — run that expression instead.
- `PORT=2223 ./scripts/run.sh x64-ssh` — as before.
- `./scripts/run.sh x64-ssh --serve` — boot, smoke-test, then block with the
  old banner, i.e. the old no-argument behaviour.
- `./scripts/run.sh x64-ssh --keep` — leave QEMU running and return.

## Not done / honest boundary

- **10 of 16 targets are unproven**, and this branch does not fix any of them.
  Six are blocked on the SSH payload (three distinct failure modes, above),
  four on the #209 RPi build error.  Their runners are all still on disk.
- The ssh `dhcp-discover` stall is a real finding but is **not diagnosed**
  here.  What is established: it is not the launcher, not the `hostfwd`, and
  not this branch's builds (byte-identical md5s).  It reproduces on x64 and
  aarch64 with the same signature.
- `run.sh`'s ssh mode is therefore **fixed but untested end-to-end**.  The
  marker wait, the settle delay and the exit code are transcribed and
  reviewed, not demonstrated against a working sshd.
- `mvm/build.lisp` still lists `/tmp/modus-x64-cl.bin` for the
  `x64/bare/qemu/cl-repl` cell.  Only `--list` prints it, so it is cosmetic
  there, but it is the same typo fixed in `run.sh` and should be corrected on
  the build side.

## Final count

`ls scripts/run-*.sh | wc -l`: **24 → 19.**

Still present, and why:

- `run-repl-eval.sh` — shared helper `run.sh` calls; not a launcher.
- `run-uefi-repl.sh`, `run-i386.sh`, `run-fixpoint{,-ssh,-i386}.sh`,
  `run-bench{,-compare}.sh`, `run-ansi-{all,per-file}.sh` — the `SPECIAL`
  entries `run.sh` deliberately redirects to; not plain QEMU boots.
- `run-{x64,aarch64,i386,arm32}-ssh.sh`, `run-aarch64-actors.sh` — target
  unproven (payload).
- `run-rpi-{repl,ssh,hid,periph}.sh` — target unproven (build, #209).
