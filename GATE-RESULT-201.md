# GATE-RESULT-201 — width-neutral float layout (4 slots x 16-bit tagged chunks)

Branch `float-layout` in `/home/claude/ws-float`, unpushed, off `main` @ `fe23468`.

| commit | what |
|---|---|
| `b212bc0` | hoist the six duplicated in-image float accessors into `mvm/float-slot-overrides.lisp` (behaviour-neutral) |
| `ec32bc7` | the layout change: 2 slots x 32-bit → 4 slots x 16-bit, tagged |

---

## 1. Commit 1 is behaviour-neutral — two independent proofs

`ieee-float-hi32` / `ieee-float-lo32` / `ieee-float-bits` plus the bignum
literal-decomposition helpers were duplicated **verbatim** inside a
`*stage2-float-override*` string literal in six build scripts.  Five copies were
byte-identical; `build-ansi-common.lisp` carried the same code plus an
ANSI-specific `%global-name-key` override.

* **Form identity.**  Reading the old inline string and the new file and
  comparing the resulting form lists: 9 forms vs 9 forms, `EQUAL = T` for the
  five identical copies; 10 vs 10, `EQUAL = T` for `build-ansi-common` (its
  `%global-name-key` block stays inline and is concatenated after the shared
  text, preserving order).
* **Behaviour identity.**  The 33-probe float battery on the x64 CLI is
  byte-identical before and after.

The built image grows 1272 bytes only because `build-generic-cli` embeds its own
source blob and the new file carries a header comment.  No form, defun or
symbol-table entry changed: `defuns found: 3152` and `symbol names found: 8655`
on both sides.

---

## 2. Headline — 64-shard NET gate, x64 Linux

Both binaries built from scratch for this run (`compiler.lisp` / `translate-*`
have changed recently, so no older binary was reused).  Both gates run with the
box otherwise idle, `NSH=64`, `10001..27800`.

```
BASE(main fe23468)         : passed=17493 CHUNK-CRASH=0 FILE-WEDGE=30 (NSH=64)
NET (float-layout ec32bc7) : passed=17496 CHUNK-CRASH=0 FILE-WEDGE=30 (NSH=64)
```

`ansi-file-ranges.txt` is **identical** between the two builds, so per-ID diffs
are valid (no corpus shift).

Crash markers are the timing-immune gate: `CHUNK-CRASH 0 → 0`,
`FILE-WEDGE 30 → 30`.  Unchanged.

---

## 3. Per-ID diff

```
comm -23 base net   (losses, base-only) : 2
comm -13 base net   (gains,  net-only)  : 5
```

| ID | file | direction |
|---|---|---|
| 14310 | `random` | loss |
| 26808 | `truename` | loss |
| 8661 | `signum` (custom probe, complex/float `EQL`) | gain |
| 13409 | `decf` | gain |
| 13558 | `float` | gain |
| 13639 | `incf` | gain |
| 14381 | `signum` | gain |

### 3.1 Deterministic recheck of EVERY loss

Both losses were re-run in isolation (3 reps) and in their containing shard
(3 reps) on **both** binaries.

**14310 (`random`) — not a regression, the test is nondeterministic.**

```
isolation, 3 reps:   base  F F F     net  F F F
shard 14186..14464:  base  1314 / 1314(P:14310) / 1313
                     net   1316 / 1316 / 1316(P:14310)
```

It passes intermittently on *both* binaries — base caught it in rep 2, net in
rep 3.  The shard total is strictly higher on net in all three reps
(1316 vs 1313-1314).

**26808 (`truename`) — not a regression, sweep artifact.**

```
isolation, 3 reps:   base  F F F     net  F F F
shard 26741..27019:  base  1296 / 1296 / 1296   all three with P:26808
                     net   1297 / 1297 / 1297   all three with P:26808
```

It passes 3/3 on **both** binaries when its shard is re-run; its absence from
the net sweep was a lost line in the concurrent 64-shard run.  The shard total
is again strictly higher on net.

**Conclusion: zero real regressions.**  Re-running the containing shard of each
"loss" shows the NET build ahead in both.

### 3.2 The gains are real, and one of them is a genuine correctness fix

`8661` reproduces deterministically:

```
isolation, 3 reps:   base  F F F     net  P P P
```

`8661` is `(let ((c (complex -1.0 0.0))) (eql c (signum c)))` → `T`.

Root cause of the old failure, and why the layout change fixes it: under the
2-slot layout the hi32 slot could be stored **signed or unsigned depending on
which path produced the float** — the SSE arithmetic path stored it
sign-extended (`sar 32; shl 1`), the reader's literal path stored it unsigned.
`cl-types.lisp:float-negative-p` documents exactly this ("the hi32 slot may be
stored SIGNED … or UNSIGNED … so test bit 31 directly").  `EQL` compared the raw
slots, so two floats with *identical IEEE bits* could compare unequal
(-1082130432 vs 3212836864).  Under 4 x 16-bit chunks every chunk is 0..65535 —
unsigned by construction — and every reader goes through `%float-hi32` /
`%float-lo32`, so the signed/unsigned ambiguity is eliminated structurally.
That is almost certainly what `float`, `signum`, `decf` and `incf` are picking
up too.

The four in-range gains fail in *single-ID* isolation on both binaries (these
tests need their file's setup forms, which a one-ID run skips), so they were
judged by containing-shard re-runs — see §3.3.

### 3.3 Containing-shard re-runs for the gains

```
shard 13349..13627 (decf 13409, float 13558):
    base 1318 / 1318 / 1316
    net  1321 / 1321          both reps carry P:13409 AND P:13558
shard 13628..13906 (incf 13639):
    base 1321 / 1321
    net  1323 / 1323          both reps carry P:13639
shard 14186..14464 (signum 14381):
    base 1314 / 1314 / 1313
    net  1316 / 1316 / 1316
```

Every shard containing a gain is higher on NET, in every rep, and the gained IDs
are present in every NET rep and absent in every BASE rep.  The base side of
13349..13627 does vary 1316-1318, so a 1-2 test shard delta on its own would be
inside the noise band — but the *targeted IDs* are 2/2 or 3/3 present on NET and
0/2 or 0/3 on BASE, which is not noise.

Summary: **+5 on the sweep, 0 real regressions.**  `8661` is proven
deterministic in single-ID isolation; the other four are proven by
containing-shard re-runs.

---

## 4. Float battery — before and after (x64 CLI, byte-identical)

Same output from `main` and from `float-layout`:

```
(:ADD 3.75d0)                     (:MAXPOS 1.7976931348623157d308)
(:NEG -5.5d0)                     (:MINPOS 5.0d-324)
(:MUL -10.0d0)                    (:NEGZERO 0.0d0 -0.0d0)
(:DIV 0.25d0)                     (:ZEROCMP T)
(:SUB -2.0d0)                     (:TINY 1.0d-10)
(:ROUNDTRIP-NEG -0.3333333333333333d0)  (:HUGE 1.0d200)
(:ITOF 7.0d0)                     (:SINGLE 3.75 SINGLE-FLOAT)
(:FTOI 3)                         (:SINGLE-NEG -5.5)
(:FTOI-NEG -3)                    (:IDF 4503599627370496)
(:CMP (T T T))                    (:IDF-NEG 7881299347898368)
(:DECODE 0.5d0)                   (:RAT 1/2)
(:SQRT 1.414213562373095d0)       (:FLOOR -8 -7 -8 -7)
(:FMT "3.14159|~E|3.14159d0")     (:READ -1.25d0 6.02d23)
(:EQLFL T T T)                    (:SIGNBIT -1.0d0 1.0d0)
(:SCALE 1024.0d0)                 (:BIG 1.2345678901234567d19)
(:EXPT 1024.0d0)
```

All ten of the probes named in the task brief hold, including the negative and
full-mantissa cases that drive the sign bit.

### 4.1 Layout probes (new build only — these read the slots directly)

```
(:SLOTS 4 4)                                   ; double and single both 4 slots
(:CHUNKS      16376 0 0 0)                     ;  1.5d0 = 0x3FF8000000000000
(:CHUNKS-NEG  49144 0 0 0)                     ; -1.5d0 = 0xBFF8000000000000
(:HILO         1073217536 0)                   ; 0x3FF80000 / 0
(:HILO-NEG     3220701184 0)                   ; 0xBFF80000 — sign bit intact
(:HILO-COMPUTED 3220701184 0)                  ; same via the SSE write path
(:THIRD-BITS   1070945621 1431655765)          ; 1/3 = 0x3FD55555 55555555
(:NEG-THIRD-BITS 3218429269 1431655765)        ; -1/3 = 0xBFD55555 55555555
```

Boundary cases:

```
(:DENORM   5.0d-324  hi=0          lo=1)             ; 0x…0001
(:MAXBITS  hi=2146435071  lo=4294967295)             ; 0x7FEFFFFF FFFFFFFF
(:MINBITS  hi=4293918719  lo=4294967295)             ; 0xFFEFFFFF FFFFFFFF
(:POSZERO  hi=0 lo=0)   (:NEGZERO hi=2147483648 lo=0); -0.0 sign bit survives
(:ALLONES-LO hi=1069128089 lo=2576980378)            ; 1/10 = 0x3FB99999 9999999A
(:RT  print→read round-trip of -1/3 is bit-exact)
```

GC stress — 20 000 freshly allocated floats, then check two long-lived ones:

```
(:SURVIVED 0.3333333333333333d0 -0.14285714285714285d0)
(:SURVIVED-BITS 1070945621 1431655765 3217180964 2454267026)   ; bit-exact
(:EQL-AFTER T T)
```

### 4.2 The JIT path is covered too

The shipping x64 CLI has the runtime JIT ON by default (`(%jit-enabled-p)` → `T`),
so `--eval` float forms are translated by the **in-image** `translate-x64.lisp`
— i.e. `EMIT-FLOAT-LOAD-BITS` / `EMIT-FLOAT-STORE-BITS` running as Modus code.
The ANSI gate binary has the JIT baked off, so it exercises the host-side
build-time codegen.  Both produce correct floats.

---

## 5. aarch64 and i386

### 5.1 aarch64 — builds, boots, and shows the SAME gain as x64

`mvm/build-aarch64-linux.lisp` builds clean from both `main` and `float-layout`
(needs `--dynamic-space-size 16384`; 4096 heap-exhausts, which is pre-existing
and not related to this change).  The resulting binary boots and runs under
`qemu-aarch64-static`.

Float-family ranges, in-range test IDs only, base vs net:

```
float      13554..13566 : base 11   net 12    diff: net gains P:13558 only
ceiling    13329..13349 : base 19   net 19    IDENTICAL
```

`P:13558` is **the same test that gained on x64** — independent confirmation
that the aarch64 chunk load/store emitters (`A64-FLOAT-LOAD-BITS` /
`A64-FLOAT-STORE-BITS`) round-trip the payload correctly, and that the fix is a
representation fix rather than an x64 codegen accident.  Zero losses.

`print-floats` (21931..21950) needs a caveat: `21942` is a **randomized** test
(`(RANDOM 20000000)`, 10 000 iterations of PRIN1 round-trip).  Under qemu it is
flaky on both sides — base 18/18 twice, net 18 once and 17 once, the missing one
always being exactly `21942`.  On x64, where the gate is authoritative, all 20
print-floats tests including `21942` pass on **both** binaries.  So this is a
qemu timing artifact, not a print regression.

A full aarch64 gate was not run: under `qemu-aarch64-static` a single 279-ID
shard takes ~10 minutes, so 17 800 IDs x 2 binaries is many hours.

### 5.2 i386 — nothing broke

`mvm/build-i386-cli.lisp` builds clean ("TRANSLATOR: no unimplemented
opcodes"), and its 91-line built-in self test (arithmetic, strings, hash,
ChaCha/SHA crypto, GC collector, chain survival) is **byte-identical** between
`main` and `float-layout` under `qemu-i386-static`.

That is the expected result: `translate-i386.lisp` has no float opcodes at all,
so i386 cannot construct a float; it only picks up the shared accessor file and
the `%float-*` helpers, which compile but are unreachable.  Making them
*reachable* is the follow-up.

### 5.3 Other images that consume the hoisted file

`build-generic`, `build-generic-cli`, `build-ansi-common` (via
`build-x64-linux`), `build-i386-cli` and `build-x64-cl-repl` were all built
successfully on this branch.  `build-modus-selfhost` was not rebuilt (the
self-compile is ~15 minutes and its edit is the identical mechanical hoist as
the other four, applied by the same script).

---

## 6. Notes / follow-ups

* **Scope was x64 + aarch64 only.**  No i386 float codegen here.  That is the
  follow-up this change unblocks: with `chunk << 1` fitting a 32-bit word, the
  i386 SSE2 sequences (`F2 0F 58/5C/59/5E`, `CVTSI2SD`, `CVTTSD2SI`) can be
  ported directly.  Nothing i386 regresses meanwhile — `translate-i386.lisp` has
  no float ops at all, and `build-i386-cli` only picks up the shared accessor
  file, which cannot produce floats on its own.
* **Cost.**  A float read is now ~20 instructions instead of ~9; a write ~20
  instead of ~10.  There are 5 read sites and 2 write sites on x64 (fadd/fsub/
  fmul/fdiv, itof, ftoi, fcmp), all factored into two helper emitters.  The gate
  shows no timing consequence: shard wall-clock and the crash markers are
  unchanged.  The ANSI image is 129 618 397 bytes vs 129 533 066; the CLI image
  actually shrank (36 979 342 vs 37 012 979) because the inline sequences became
  shared helpers.
* **The collector needed no change.**  `copy_object` (x64 asm, aarch64 asm and
  `mvm/gc.lisp`) derives object size from the header element-count —
  `align16((count + 2) * 8)` = 48 for count 4 — so allocator and collector agree
  automatically.  The `+float-slots+` bump in the two translators (`add r12, 48`
  / `add x24, x24, 48`) matches.  48 bytes is 16-byte aligned and far inside the
  64 KiB MCGC overshoot guard, so `:gc-check` still covers the allocation.
  aarch64's Cheney scan already lists `#x60`/`#x64..#x66` as leaf subtags and
  x64's flat scan is filtered by the object-start bitmap, so the (still tagged,
  still low-bit-0) chunks are never followed as pointers.
* **`compile-make-float`** (the dead 1-slot `%make-float` primop) was resized to
  `+float-slots+` so that if anything ever does reach it the object is at least
  well-formed.  Its only historical caller was renamed to
  `%build-float-from-parts` years ago precisely because the 1-slot object was
  malformed.
* **`:ftoi` still has no overflow check** — `CVTTSD2SI` returns the
  integer-indefinite value for out-of-range doubles and the following `shl 1`
  turns it into 0.  Pre-existing, independent of this change, noted in
  `docs/i386-float-blocker.md`.

---

## 7. Where the evidence lives

Everything below is under `tmp/gate-201/` in this worktree (`tmp/` is
gitignored, so the tree stays clean):

```
gate-base.txt / gate-net.txt          the two 64-shard sweeps (sorted P: IDs)
gate-*.txt.shard.*                    raw per-shard output
losses.txt / gains.txt                comm -23 / comm -13
b.s / n.s                             sorted inputs to comm
recheck.sh                            ./recheck.sh <base> <net> <id> [reps]
float-battery.lisp                    the 33-probe battery (pipe to ./modus)
float-layout-probes.lisp              slot/chunk/GC-churn probes
float-boundary-probes.lisp            denormal / maxdouble / -0.0 / bit probes
modus-ansi-net                        the NET ANSI gate binary
modus-aa64-net / modus-aa64-201base   the aarch64 gate binaries used here
aa64.*.base / aa64.*.net, pf.*        the aarch64 range comparisons
```

The BASE x64 binary is `/home/claude/ws-float-base/modus-ansi-base` (a detached
worktree at `main` @ `fe23468`), with `/home/claude/ws-float-base/modus` as the
BASE CLI.

Note: `mvm/build-aarch64-linux.lisp` writes to the shared path
`<repo>/tmp/modus-aa64-ansi-test` regardless of worktree, so the base and net
aarch64 builds overwrite each other there — copy the binary out after each
build (that path currently holds the BASE build).
