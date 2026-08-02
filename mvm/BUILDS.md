# Modus build scripts — the matrix

36 scripts, 30,387 lines. They are essentially **arch × (hosted | bare) × board**,
plus a payload axis (repl / ssh / actors / CLI / ANSI-gate / self-host).

Every row below was **built from a clean worktree on 2026-08-02** (all 36,
concurrently). `BUILD` is that result — not a guess from file dates.

---

## Hosted (Linux ELF — runs on the dev box or under qemu-user)

| script | arch | what it is | BUILD |
|---|---|---|---|
| `build-generic-cli` | x64 | **the canonical `./modus` CLI** — SBCL-faithful flags, net stack, JIT on | OK |
| `build-aarch64-cli` | aarch64 | the aarch64 CLI / JIT host | OK |
| `build-i386-cli` | i386 | the 32-bit CLI (WS5 #200) | env |
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
| `build-x64-repl` / `-ssh` / `-console-repl` | x64 | multiboot QEMU (second Lisp) | OK |
| `build-aarch64` | aarch64 | QEMU virt — **bare ANSI gate** | OK |
| `build-aarch64-repl` / `-ssh` / `-actors` / `-isolated` | aarch64 | QEMU virt (E1000) | OK |
| `build-i386-repl` / `-ssh` / `-diag-ssh` | i386 | QEMU + T420 | OK |
| `build-arm32-repl` / `-ssh` | arm32 | QEMU | OK |

### Real hardware

| script | arch | board | BUILD |
|---|---|---|---|
| `build-uefi-repl` | x64 | **T420** (PE32+ EFI app, GOP + PS/2) | OK |
| `build-rpi-repl` / `-ssh` / `-hid` / `-periph` | aarch64 | RPi 3B (DWC2 USB) | **BROKEN** |
| `build-pizero2w-ssh` / `-actors` / `-hdmi` | aarch64 | **Pi Zero 2 W** (USB gadget) | **BROKEN** |
| `build-uart-bootloader` | aarch64 | Pi UART bootloader | **BROKEN** |

## Not images — shared libraries loaded by the gate wrappers

| file | note |
|---|---|
| `build-ansi-common` (4646 L) | loaded by all 4 gate wrappers (+ `build-aarch64-cli`); arch chosen by `*ansi-target-arch*` |

These two **fail standalone by design** — they are not entry points. They are
also ~94% identical (253 diff lines, 7 hunks); merging them is task #208.

---

## Audit result: 21 OK, 12 "fail", but only **2 real defects**

The 12 failures are NOT 12 atrophied scripts. By actual cause:

1. **2 × expected** — `build-ansi-common-{x64,aarch64}` are libraries, not
   entry points. Not a defect.
2. **1 × environment** — `build-i386-cli` wants
   `/home/claude/ws5-gate-out/modus-i386-cli.symmap`, a path from another
   worktree. The build is fine; the default output path is not.
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

- **Do**: merge the two `build-ansi-common-*` (#208) — ~4,250 duplicated lines,
  and a shared-harness fix currently has to be applied twice with nothing
  enforcing it.
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
- **Don't**: delete the ~22-line wrappers (`*-repl`, `*-ssh`). They are thin by
  design — a few defvars plus `build-image` — which is the right shape. The
  bulk is in 13 files; the other 23 are ~4,000 lines total.

## Reproducing this audit

```sh
for f in mvm/build-*.lisp; do
  ( timeout 2400 sbcl --dynamic-space-size 12288 --script "$f" \
      > AUDIT/$(basename $f .lisp).log 2>&1; echo $? > AUDIT/$(basename $f .lisp).rc ) &
  while [ $(jobs -r | wc -l) -ge 12 ]; do sleep 5; done
done; wait
```

TRAP: every build log contains a benign host-side
`undefined variable: MODUS.MVM::*AARCH64-GC-NATIVE-MCGC*` warning. Grepping for
`error`-ish patterns finds it first and hides the real cause — read the
`Unhandled …` block and the `BUILD-IMAGE` frame instead.
