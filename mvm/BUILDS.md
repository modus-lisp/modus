# Modus build scripts — the matrix

28 scripts (was 36; 8 retired into `mvm/build.lisp`'s matrix). They are
essentially **arch × (hosted | bare) × board**,
plus a payload axis (repl / ssh / actors / CLI / ANSI-gate / self-host).

## Building them — one entry point

```bash
sbcl --script mvm/build.lisp --list             # the build matrix
sbcl --script mvm/build.lisp x64/bare/qemu/repl
sbcl --script mvm/build.lisp build-x64-repl     # legacy name still resolves
```

`mvm/build.lisp` is the single build entry point.  The matrix is
**arch × mode × board × payload** — `x64|aarch64|i386` × `hosted|bare` ×
`qemu|rpi|t420`.  Every cell is reachable today: a cell is either `native`
(built from the table, no legacy script) or `legacy` (delegates to its
`build-*.lisp`, unchanged).  Cells migrate one at a time, each gated on
producing a **byte-identical** image.

Migrated so far (each verified byte-identical to the script it replaces —
the script is then DELETED, so the count above really does go down):

| cell | retired script | md5 legacy == native |
|---|---|---|
| `x64/bare/qemu/repl` | `build-x64-repl` | `269b461a764016eea6533c46798ad3e4` |
| `aarch64/bare/qemu/repl` | `build-aarch64-repl` | `fd0d40b12e984e064f3713ce03ed21e8` |
| `x64/bare/qemu/ssh` | `build-x64-ssh` | `782c7414cf3ad555c302500942004a8c` |
| `aarch64/bare/qemu/ssh` | `build-aarch64-ssh` | `a168c3fe76e313cf9c644b7b5af0ac3d` |
| `aarch64/bare/qemu/actors` | `build-aarch64-actors` | `98cc10becd7a7da404ba700d86fd6ce2` |
| `aarch64/bare/qemu/isolated` | `build-aarch64-isolated` | `2083abab509dca80f930a36cffee9a5b` |
| `i386/bare/qemu/ssh` | `build-i386-ssh` | `dcadafd144d94459e6aac9302998c5f9` |

`aarch64/bare/rpi/repl` is also native and reproduces the known RPi
`A64-BUFFER` build failure *identically* — the migration does not mask it.

The five SSH/actors cells were NOT pure table data — each composes a per-arch
list of `net/` files (order is semantic: last-defun-wins) plus an inline
`kernel-main` in source strings.  `build.lisp` grew a `*COMPOSITES*` table for
exactly that shape (`:net` / `:main` / `:extra` / `:parts` / `:flags`); the
`:parts` order is load-bearing (single-threaded SSH puts kernel-main LAST so
it wins the entry point; the actor builds put it FIRST).  `actors` and
`isolated` share one verbatim kernel-main — they differ only by appending
`isolated-net.lisp`.  Evidence: `GATE-RESULT-ssh-cells.md`.

## Running them — one entry point

```bash
./scripts/run.sh list           # the run matrix
./scripts/run.sh x64-repl       # interactive
./scripts/run.sh aarch64-ssh    # boot, wait for sshd, smoke test
```

`scripts/run.sh` replaces the near-identical `run-<arch>-<payload>.sh`
launchers.  They were copy-paste siblings that had **drifted**, and the drift
was the real cost: `run-aarch64-ssh` rebuilt the kernel on *every* invocation
while `run-x64-ssh` checked staleness; x64/i386 wrapped QEMU in `no-thp-exec`
to dodge THP-compaction hangs while aarch64/arm32 did not.  Every QEMU flag
in `run.sh`'s table is transcribed verbatim from the script it replaces.

**24 → 19 `run-*.sh`.**  Five REPL launchers (`run-{x64,x64-console,aarch64,
i386,arm32}-repl.sh`) are RETIRED — each was demonstrated through `run.sh`
first (boot to prompt, evaluate, right answer).  The six `*-ssh` launchers and
the four `run-rpi-*` ones are **kept**: their targets could not be proven, and
not for launcher reasons — the SSH payload wedges in `dhcp-discover` before
printing `DHCP:D` (the scripts `run.sh` would replace fail *identically*, and
the images are byte-identical to the md5s in this file), and the RPi family
still dies on the `A64-BUFFER` build error.  Evidence: `GATE-RESULT-run-cells.md`.

Proving `run.sh` also turned up three defects in it, now fixed: the ssh
readiness test was a `/dev/tcp` probe, which QEMU satisfies the instant it
binds `hostfwd` — so the smoke test always ran before the guest was up and
still exited 0; the build shelled out to SBCL with the default heap, which
heap-exhausts on `x64-cl-repl`; and the `x64-cl-repl` row named an image
(`/tmp/modus-x64-cl.bin`) that build never writes — it writes
`/tmp/modus-x64-cl-repl.bin`.  **`build.lisp`'s row for that cell still has
the wrong path**; it is only printed by `--list`, but it should be fixed.

**BUILD status is not RUN status.**  Every row below was *built*; that says
nothing about whether the image evaluates.  Two verified examples:
- `scripts/run-repl-eval.sh` sent `\n`; the bare-metal serial line editor
  terminates on `\r`.  It therefore produced NO output and exited 0 — a
  silent pass.  Fixed.
- With `\r`, `build-x64-repl` and `build-aarch64-repl` both evaluate a bare
  symbol (`x` → `nil`) but a call (`(+ 1 2)`) echoes and never returns, on
  **both** arches.  That is legacy `repl-source.lisp` rot and is exactly the
  argument for #204 (retire the second Lisp; put every image on CL/mvm).

Every row below was **built from a clean worktree on 2026-08-02** (all 36,
concurrently). `BUILD` is that result — not a guess from file dates.

---

## Hosted (Linux ELF — runs on the dev box or under qemu-user)

| script | arch | what it is | BUILD |
|---|---|---|---|
| `build-generic-cli` | x64 | **the canonical `./modus` CLI** — SBCL-faithful flags, net stack, JIT on | OK |
| `build-aarch64-cli` | aarch64 | the aarch64 CLI / JIT host | OK |
| `build-i386-cli` | i386 | the 32-bit CLI (WS5 #200) | **OK** (layer 5) |
| `build-x64-linux` | x64 | **ANSI gate runner** (the 64-shard conformance number) | OK |
| `build-aarch64-linux` | aarch64 | ANSI gate runner | OK |
| `build-generic` | x64 | minimal image that LOADs argv[1] | OK |
| `build-modus-selfhost` | x64 | self-hosting image (`--compile`, modus2/modus3) | OK |
| `build-mvm` | x64 | self-hosted compiler over the MVM subset | OK |
| `build-compiler-test` | x64 | compiler test harness | OK |
| `build-bench` | x64 | benchmark image | OK |
| `build-fixpoint` | multi | multi-arch fixpoint (32/64 dispatch) | **BROKEN** |

## Bare metal

### QEMU virt / multiboot — the "does it boot" targets

| script | arch | board | BUILD |
|---|---|---|---|
| `build-x64` | x64 | multiboot QEMU — **bare ANSI gate** | OK |
| `build-x64-cl-repl` | x64 | multiboot QEMU — **bare metal running the REAL CL** (#204); boots, evaluates, one known gap (global var READ — see its header) | OK |
| `x64/bare/qemu/repl` + `x64/bare/qemu/ssh` **(both scripts RETIRED)** / `build-x64-console-repl` | x64 | multiboot QEMU (second Lisp) | OK |
| `build-aarch64` | aarch64 | QEMU virt — **bare ANSI gate** | OK |
| `aarch64/bare/qemu/` `repl` + `ssh` + `actors` + `isolated` **(all four scripts RETIRED)** | aarch64 | QEMU virt (E1000) | OK |
| `build-i386-repl` / `-diag-ssh`, `i386/bare/qemu/ssh` **(script RETIRED)** | i386 | QEMU + T420 | OK |
| `build-arm32-repl` / `-ssh` | arm32 | QEMU | OK |

### Real hardware

| script | arch | board | BUILD |
|---|---|---|---|
| `build-uefi-repl` | x64 | **T420** (PE32+ EFI app, GOP + PS/2) | OK |
| `aarch64/bare/rpi/repl` **(script RETIRED)** / `build-rpi-ssh` / `-hid` / `-periph` | aarch64 | RPi 3B (DWC2 USB) | **BROKEN** |
| `build-pizero2w-ssh` / `-actors` / `-hdmi` | aarch64 | **Pi Zero 2 W** (USB gadget) | **BROKEN** |
| `build-uart-bootloader` | aarch64 | Pi UART bootloader | **BROKEN** |

## Not images — shared libraries loaded by the gate wrappers

| file | note |
|---|---|
| `build-ansi-common` (4646 L) | loaded by all 4 gate wrappers (+ `build-aarch64-cli`); arch chosen by `*ansi-target-arch*` |

This **fails standalone by design** — it is not an entry point. It is the
result of #208 (LANDED): the two ~94%-identical `build-ansi-common-{x64,
aarch64}` files were merged into one, so a shared-harness fix no longer has
to be applied twice with nothing enforcing it.

---

## Audit result: 21 OK, 12 "fail", but only **2 real defects**

The 12 failures are NOT 12 atrophied scripts. By actual cause:

1. **2 × expected** — `build-ansi-common-{x64,aarch64}` are libraries, not
   entry points. Not a defect.
2. **1 × environment — FIXED.** `build-i386-cli` wrote to
   `/home/claude/ws5-gate-out/…`, a path from a worktree that no longer
   exists (3 sites: image, symmap, and a build-time fixture).  Defaults now
   point at `/tmp/modus-i386-cli`; `MODUS_I386_OUT` still overrides.

   The bigger trap was underneath: `*i386-layer*` **defaults to 1**, and
   layer 1 bakes `prelude.lisp` ONLY — no gc/rt (≥2), no CL bridge (≥3), no
   compiler/mvm-eval (≥5).  So the default `build-i386-cli` produced a
   bring-up rung, not the i386 CLI, and reported *2500 unresolved calls with
   no `%UNRESOLVED-FN` stub at all* — correctly, since `cl-sequences.lisp`
   (where the stub lives) simply is not in a layer-1 image.  At **layer 5**
   the image builds clean (41.8 MB), unresolved falls to **55 → the real
   stub**, the EAX/VR invariant is clean, and it RUNS under
   `qemu-i386-static` (probe 5 evaluates: `e1=42 e2=3 e3=42 e4=66 e5=1`).

   `mvm/build.lisp` now sets `MODUS_I386_LAYER=5` for the
   `i386/hosted/-/cli` cell, so the matrix key means the CLI rather than
   rung 1 of its ladder.

   **Remaining real i386 gap: floating point.**  13 distinct translator
   gaps, and 157 of the ~165 sites are float ops —
   `FDIV ×57, FMUL ×31, FADD ×24, FSUB ×22, ITOF ×17, FTOI ×6` — plus 8
   traps and `SAP-NEW`/`SAP-ADDR`.  That is #201 (width-neutral numeric
   tower), not a build defect.
3. **1 × REAL bit-rot** — `build-fixpoint`:
   `Fixpoint content (15399263 bytes) exceeds metadata offset 0x400000`.
   The image has outgrown the fixpoint layout. Genuinely stale.
4. **8 × ONE SHARED BUG** — every RPi / Pi Zero 2 W / UART target dies the same
   way inside `BUILD-IMAGE :TARGET :RPI`:
   `#S(A64-BUFFER …) is not of type MODUS.MVM::MVM-BUFFER`
   — an a64-buffer reaching code that expects an mvm-buffer. **One type
   confusion breaks the entire Raspberry-Pi family**, including the Pi Zero 2 W
   hardware path. That is a regression to fix, not eight scripts to delete.

## What to actually consolidate

- **DONE (#208)**: merged the two `build-ansi-common-*` — ~4,250 duplicated lines,
  and a shared-harness fix had to be applied twice with nothing
  enforcing it.
- **DONE**: `scripts/run.sh` — one entry point for ~14 drifted QEMU launchers.
- **Do**: retire `repl-source.lisp` (#204). Note the whole broken RPi family
  builds `*repl-source*` — the second Lisp and the broken cell overlap heavily.
  Step 1 has landed: `build-x64-cl-repl` is a bare-metal multiboot image whose
  REPL is the real CL (reader + `eval` = mvm-eval + printer) over COM1, with
  `lib/serial-repl.lisp` as the bare-metal counterpart of `lib/cli-toplevel.lisp`.
  It is the replacement target for `build-x64-repl` and `build-x64-console-repl`.
  Also worth knowing: `build-generic-cli` and `build-ansi-common-x64` still
  `(mvm-load "mvm/repl-source.lisp")` but never reference `*repl-source*` — two
  of the 24 "bakes the second Lisp" scripts are vestigial loads, not bakes.
- **Don't**: merge the three CLI builds. Measured overlap of non-comment lines
  is `generic∩aa64 64`, `generic∩i386 193`, `aa64∩i386 72`, **all three 35** —
  they are genuinely different bring-ups, and the shared parts were already
  hoisted to `lib/cli-toplevel.lisp` and `lib/runtime-backquote.lisp`.
- **DONE**: the `*-repl` and `*-ssh` wrappers ARE now table rows in
  `mvm/build.lisp` (8 scripts retired, 36 -> 28).  The earlier "don't delete
  them, they're thin by design" note was wrong about the payoff: thin is
  exactly what makes them table data, and each retirement was gated on a
  byte-identical image, so nothing was risked to get the count down.

## Reproducing this audit

```sh
for f in mvm/build-*.lisp; do
  ( timeout 2400 sbcl --dynamic-space-size 12288 --script "$f" \
      > AUDIT/$(basename $f .lisp).log 2>&1; echo $? > AUDIT/$(basename $f .lisp).rc ) &
  while [ $(jobs -r | wc -l) -ge 12 ]; do sleep 5; done
done; wait
```

TRAP: a **comment-only** edit to any source file an image bakes DOES change
that image's bytes. `build-image` calls `embed-source-blob` (`mvm/cross.lisp`),
which bakes the entire combined source text verbatim, comments included. So
byte-identity gates are exact tests of the source string — which is what makes
them worth so much — and a stray comment tidy-up in `net/*.lisp` will
legitimately break one. Measured: a 10-character comment edit in
`net/arch-x86.lisp` moved `/tmp/modus-x64-ssh.bin` by exactly 8 bytes.

TRAP: every build log contains a benign host-side
`undefined variable: MODUS.MVM::*AARCH64-GC-NATIVE-MCGC*` warning. Grepping for
`error`-ish patterns finds it first and hides the real cause — read the
`Unhandled …` block and the `BUILD-IMAGE` frame instead.
