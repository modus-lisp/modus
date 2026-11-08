# Task #220 — `(mod 1 64)` returns 0 on aarch64

Branch `aa64-mod`, off `main` @ `3a6a7f8`. One file changed:
`mvm/translate-aarch64.lisp` (+45 / −7).

---

## 1. Three-way discrimination

The task asked to split three ways before fixing. The answer is the third
option, and it is narrower than any of them:

| probe | expectation if… | result |
|---|---|---|
| `(truncate 65 64)` → `(1 1)` but `(nth-value 1 …)` → 0 | MV **return** path | **not this** |
| `(truncate 65 64)` → `(1 0)` | division/remainder **computation** | **THIS** |
| only `mod` wrong, `rem` right | `compile-mod` **expansion** | **not this** |

**It is the remainder computation, in the aarch64 translator's `+op-mod+`
codegen.** Not the MV convention, not `compile-mod`, not `%fixnum-truncate2`,
not the shared numeric tower.

**The MV convention on aarch64 is healthy.** Verified directly rather than
assumed — `multiple-value-list`, `multiple-value-bind`, `nth-value`, `values`,
`gethash`'s present-p, and `floor`/`ceiling`/`round`/`truncate` 2-value returns
all behave correctly (§4). The scope worry in the task brief does not
materialise: nothing built on `(nth-value 1 …)` is generically broken.

**One expansion or a broken convention?** Neither, exactly: it is **one
translator op**, `+op-mod+`. An automated scan of every op in
`translate-aarch64.lisp` for the same "clobber a scratch, then re-read a source
that may *be* that scratch" shape produced 9 candidates; 8 are false positives
(mutually-exclusive `cond` branches — `add-checked`, `mul-checked`, `obj-ref`)
or safe read-before-write. `+op-mod+` is the only live one. Two adjacent
observations are recorded in §8 as follow-ups, not fixed here.

---

## 2. Root cause

`+op-mod+` emitted (main):

```
pa = ensure-src(Va, x16)      ; x16 is the SCRATCH
pb = ensure-src(Vb, x17)
pd = phys(Vd) or x16

SDIV x16, pa, pb              ; x16 = quotient
MUL  x16, x16, pb             ; x16 = q*divisor
SUB  pd, pa, x16              ; remainder = dividend - q*divisor
```

`ensure-src` returns the **scratch** `x16` whenever its vreg is spilled
(V9–V15 have no physical register). So at any site whose dividend `Va` is
spilled, **`pa` *is* `x16`** — and the `SUB` becomes

```
SUB pd, x16, x16     →  0
```

A hard zero, for every operand pair, at every such site. `(mod 1 64)` → 0 is
just the smallest visible instance.

Why only the remainder: `+op-div+` does `SDIV x16, pa, pb` / `LSL pd, x16, #1`
— it reads `pa` and writes `x16` in the *same* instruction, so it was always
correct. That is exactly why the quotient looked fine and only the second
value was wrong, and it is why this never looked like a division bug.

**Not reachable through the MVM interpreter or x64** — this is aarch64 native
codegen only.

### Incidence — measured, not estimated

An instrumented build (translator reports every `+op-mod+` where `pa == x16`;
compiler reports the enclosing function) of `aarch64/hosted/-/cli`:

**9 live sites**, all in the `%temps-must-spill-p` variant of
`compile-fixnum-truncate2` (`:MOD Vd=11 Va=9 Vb=8` ×8, `Vd=12 Va=10 Vb=9` ×1
— `Va=V9`/`V10` are spill slots, confirming the mechanism exactly):

| function | consequence |
|---|---|
| `%LEAP-YEAR-P` (3 sites) | **every year is a leap year** |
| `%FMT-INTEGER` | integer/radix formatting |
| `EXACT-DIVIDE` | |
| `%GF-CHECK-KEYS` | CLOS generic-function keyword checking |
| `%CLOS-VALIDATE-INITARGS` | CLOS initarg validation |
| `TRANSLATE-MVM-TO-AARCH64` | **the self-hosted compiler itself** |
| `MVM-COMPILE-ALL` | **the self-hosted compiler itself** |

Confirmed at runtime on the unfixed aarch64 CLI (§4): `%leap-year-p` returned
`T` for 1900, 2000, 2001, 2004 and 2100, and `decode-universal-time` decoded
universal time 3600000000 as **Nov 4 2013** — wrong, and internally
inconsistent (it reported day-of-week 2 = Wednesday; Nov 4 2013 was a Monday).

---

## 3. The fix

```lisp
;; SDIV x9, Va, Vb          (x9 = untagged quotient)
(a64-sdiv buf +a64-x9+ pa pb)
;; MSUB Vd, x9, Vb, Va      (Vd = Va - x9*Vb)
(a64-msub buf pd +a64-x9+ pb pa)
```

Two properties make this correct by construction rather than by case analysis:

1. **`x9` can alias neither `pa` nor `pb`.** It is never in
   `*a64-vreg-to-phys*` and is never an `ensure-src` scratch (those are `x16` /
   `x17`). `+op-mul-checked+` already uses `x9`/`x10`/`x11` the same way, so
   this is an established scratch in this translator, not a new claim on a
   register.
2. **`MSUB` reads all three sources before writing `Rd`**, so it is safe even
   when `pd == pa == x16` — the one combination that has no register left to
   hide behind.

`a64-msub` is new (MADD with `o0=1`). Encoding verified byte-exact against
GNU `as`, not derived from the manual alone:

```
msub x11, x9, x17, x16   → 9b11c12b   (computed 0x9b11c12b ✓)
msub x16, x9, x17, x16   → 9b11c130   (computed 0x9b11c130 ✓)
sdiv x9,  x16, x17       → 9ad10e09   (computed 0x9ad10e09 ✓)
```

Structural check on the built image (`aarch64/bare/qemu/repl`, disassembled):
4 × `msub`, 4 × `sdiv x9` (the `:mod` sites), 4 × `sdiv x16` (the `:div`
sites) — the old `MUL`+`SUB` pair is gone from every `:mod` site.

---

## 4. Probe results — before / after / x64

`cli-base` = aarch64 CLI at `main`. `cli-fixed` = same with the fix.
`cli-x64` = `x64/hosted/-/cli` at this branch. All hosted CLIs, same probes.

### The task's probe list

| probe | aa64 BEFORE | aa64 AFTER | x64 |
|---|---|---|---|
| `(mod 1 64)` | 1 | 1 | 1 |
| `(mod 65 64)` | 1 | 1 | 1 |
| `(mod 7 3)` | 1 | 1 | 1 |
| `(rem 1 64)` | 1 | 1 | 1 |
| `(multiple-value-list (truncate 65 64))` | (1 1) | (1 1) | (1 1) |
| `(multiple-value-list (floor 65 64))` | (1 1) | (1 1) | (1 1) |
| `(nth-value 1 (truncate 65 64))` | 1 | 1 | 1 |
| `(mod 12345 251)` | 46 | 46 | 46 |

**Read this table carefully: the task's own probe list does NOT reproduce the
bug on the hosted CLI at `main`.** It is all-green before the fix. That is not
a contradiction — it is the key to why #220 was hard to pin down:

- In the **hosted CLI**, a REPL form goes through eval2 → MVM **interpreter**.
  `interp.lisp`'s `+op-mod+` is `(rem a b)` in Lisp — correct. The buggy native
  code is never executed for a form you type.
- The bug only bites **natively compiled** code — the library functions baked
  into the image, and every **bare-metal** image, where there is no interpreter
  at all.

I also checked the JIT path (`(setq *cli-jit-on* t)`) and the bare-metal
`aarch64/bare/qemu/repl`: JIT-on is all-green too, and the bare REPL cannot
evaluate *any* call (`(+ 1 2)` echoes and never returns) — pre-existing
`repl-source.lisp` rot documented in `mvm/BUILDS.md`, on **both** arches, so it
is not a usable probe vector.

### The probes that DO reproduce it

Reached by calling the affected native functions directly:

| probe | aa64 BEFORE | aa64 AFTER | x64 | ground truth |
|---|---|---|---|---|
| `(%leap-year-p 1900)` | **T** | NIL | NIL | NIL |
| `(%leap-year-p 2000)` | T | T | T | T |
| `(%leap-year-p 2001)` | **T** | NIL | NIL | NIL |
| `(%leap-year-p 2004)` | T | T | T | T |
| `(%leap-year-p 2100)` | **T** | NIL | NIL | NIL |
| `(exact-divide 6 3)` | 2 | 2 | 2 | 2 |
| `(decode-universal-time 3600000000 0)` | **(0 0 16 4 11 2013 2 NIL 0)** | (0 0 16 29 1 2014 2 NIL 0) | (0 0 16 29 1 2014 2 NIL 0) | 2014-01-29 16:00, Wed |
| `(format nil "~D" 123456789012)` | "123456789012" | "123456789012" | "123456789012" | — |

Ground truth for the date computed independently (Python
`datetime(1900,1,1) + timedelta(seconds=3600000000)` = `2014-01-29 16:00:00`,
weekday Wednesday). **AFTER matches x64 and ground truth exactly; BEFORE
matches neither.**

### MV-convention probes (all on aarch64, all green before the fix)

`(multiple-value-list (truncate 1 64))` → `(0 1)`; `(rem 7 3)` → 1;
`(rem 65 64)` → 1; `(multiple-value-bind (q r) (truncate 1 64) …)` → `(0 1)`;
same for `floor`; `(multiple-value-list (values 10 20))` → `(10 20)`;
`(nth-value 1 (values 10 20))` → 20; `gethash` present-p → `(5 T)`;
`(multiple-value-list (ceiling 65 64))` → `(2 -63)`;
`(multiple-value-list (round 65 64))` → `(1 1)`; `(mod 100 7)` → 2;
`(mod 3 5)` → 3; `(mod -1 64)` → 63.

This is the evidence for "the MV convention is **not** broken".

---

## 5. Regression evidence

Only `mvm/translate-aarch64.lisp` changed, so x64 codegen cannot move — and
that is demonstrated, not asserted:

**`x64/bare/qemu/repl` = `269b461a764016eea6533c46798ad3e4`** — byte-identical
to the `mvm/BUILDS.md` table. The x64 ANSI gate is therefore not applicable
(and could not regress); this hash is the stronger statement.

### aarch64 ANSI runner, range 13300..14600

`mvm/build-aarch64-linux.lisp`, `--dynamic-space-size 16384`. BASE built from
a clean `git archive main` export (verified: `grep -c a64-msub` = 0), NET from
this branch. Identical shard geometry and timeout on both sides
(`NSH=8 TMO=900`); the two sweeps were run **sequentially**, never overlapping.
Matched wall times confirm matched budgets (7m48s / 7m54s).

```
BASE: passed=2104  CHUNK-CRASH=0  FILE-WEDGE=1   (13300..14600 NSH=8 TMO=900)
NET:  passed=2132  CHUNK-CRASH=0  FILE-WEDGE=1   (13300..14600 NSH=8 TMO=900)
```

**Per-ID diff (the metric that counts): +28 gained, 0 LOST.** Crash markers
identical on both sides. There are no losses, so the "recheck every loss 3×
isolated and in its containing shard" step is vacuous — I instead re-ran the
*gains* in isolation (outside the sharded harness), and they reproduce with
still zero losses.

The gains are causally coherent with a `mod` fix rather than scattered noise —
they cluster in exactly the arithmetic files that depend on a remainder, and
`evenp`/`oddp` are literally `(mod x 2)`:

| n | file | IDs |
|---|---|---|
| 10 | custom in-image probe battery (outside corpus ranges) | 5530, 5531, 5603, 5604, 9488, 9920, 9964, 9970–9972 |
| 7 | `number-comparison` | 14009, 14027, 14097–14100, 14122 |
| 4 | `numerator-denominator` | 14135–14138 |
| 2 | `divide` | 13443, 13450 |
| 1 | `evenp` | 13478 |
| 1 | `oddp` | 14145 |
| 1 | `logand` | 13723 |
| 1 | `real` | 14335 |
| 1 | `round` | 14354 |

(File names resolved through the **NET build's own** freshly written
`/tmp/ansi-file-ranges.txt`, not a stale copy.)

### Byte-identity table

`aarch64/bare/qemu/repl` at `main` reproduced its documented hash exactly
(`fd0d40b1…`) before the change, which validates the BASE.

| image | BUILDS.md | BASE (main, measured) | AFTER (this branch) |
|---|---|---|---|
| `x64/bare/qemu/repl` | `269b461a…` | — | **`269b461a…` unchanged** |
| `aarch64/bare/qemu/repl` | `fd0d40b1…` | `fd0d40b1…` ✓ | `32491efbbcd9adbfa7df7099b8fc66de` |
| `aarch64/bare/qemu/ssh` | `a168c3fe…` | `8e4dcdf82a27e60dfbb7135d4cf13fe7` | `6f311c6237aba883b03be3f14a26c1ee` |
| `aarch64/bare/qemu/actors` | `98cc10be…` | not measured | `0c4e246e90400739e498977b44ffa053` |
| `aarch64/bare/qemu/isolated` | `2083abab…` | not measured | `5a05780a0b91cc0c6823d1a52497db38` |

**The aarch64 hashes change by design** — this is a codegen fix, so every
native aarch64 image differs. That is the intended effect, not a regression.

Note, separately: the `aarch64/bare/qemu/ssh` row in `BUILDS.md` was **already
stale at `main`** (table says `a168c3fe…`, a clean build of `main` gives
`8e4dcdf8…`). That drift predates this branch — almost certainly the #219
merge — and is not caused by this change. I have not edited `BUILDS.md`; the
aarch64 rows need a refresh whoever lands this.

---

## 6. `aarch64-ssh` — before / after

**No improvement. The two are indistinguishable.**

| | BEFORE (main) | AFTER (fixed) |
|---|---|---|
| `./scripts/run.sh aarch64-ssh` | exit 255 | exit 255 |
| failure | `Connection timed out during banner exchange` | identical |

Guest-side log is the same on both runs, and reaches the end of bring-up:

```
E1000:10000000 / MAC:52:54:00:12:34:56 / E1000:OK
[1][2][3]EI [4][5]DHCP:D DHCP:R DHCP:IP=10.0.2.15 [6]SSH:22
```

The guest listens on 22 and then never emits a banner. **I am not claiming a
connection between #220 and the SSH failure.** The hypothesis in the task brief
— that broken `mod` would break Ed25519/KEX modular arithmetic — was
reasonable, and it is now tested and **disproved**: the SSH failure survives a
correct `mod` unchanged. It is in the TCP-accept / banner-write path, before
any crypto runs (the banner is sent *before* KEX, so no amount of modular
arithmetic is involved yet). It needs its own investigation.

---

## 7. What this does and does not claim

- **Does**: `+op-mod+` on aarch64 returned a hard 0 at 9 sites in the CLI image
  and in every bare-metal aarch64 image; it is fixed; x64 is provably
  untouched; the aarch64 ANSI range gate is +28 / −0 with identical crash
  markers.
- **Does not**: claim the MV convention was broken (it is not), claim this
  fixes `aarch64-ssh` (it does not), or claim the aarch64 image hashes are
  stable (they change, by design).

---

## 8. Adjacent findings — NOT fixed here

Same "scratch clobbered, then a source that may *be* that scratch is re-read"
class, found by the scan in §1. Both are **unverified at runtime** — I did not
build instrumentation for them, and I am flagging them rather than asserting
they fire. Neither is `mod`, so neither is in scope for #220.

1. **`+op-consp+` / `+op-atom+`.** `AND x16, ps, x17` (mask) then
   `CMP ps, x26` (the NIL pre-check). If `ps == x16` (spilled operand), the
   NIL check compares the *masked* value against `#xDEAD0001` and can never
   match, defeating the special case. Since `NIL`'s low nibble is 1 (cons
   tag), `(consp nil)` could return `T` at such a site. `(consp nil)` and
   `(atom nil)` are correct on the built CLI image, so if this shape exists it
   is not on those paths — but the codegen shape is there.

2. **`+op-obj-set+`, large-offset branch only** (slot index > 31, i.e.
   `idx*8+7 > 255`). `load-imm64 x16, offset` then `ADD x16, pobj, x16` — if
   `pobj == x16` the base is destroyed before use. Narrow (needs both a
   >31-slot object and a spilled object vreg).

Both would take the same shape of fix (compute into `x9`, or read the source
before clobbering). Worth a follow-up task; I did not widen this change to
cover them because they are unmeasured and would put unvalidated codegen
changes in a fix that is otherwise proven end to end.

---

## 9. Reproduction

```bash
# build
sbcl --dynamic-space-size 16384 --script mvm/build.lisp aarch64/hosted/-/cli

# the probe that actually shows it (the task's own list does NOT — see §4)
printf '(print (list :lyp1900 (%%leap-year-p 1900)))\n(print (list :lyp2001 (%%leap-year-p 2001)))\n' \
  | qemu-aarch64-static /home/claude/modus-aa64-cli
#   main  → (:LYP1900 T)   (:LYP2001 T)     <- both wrong
#   fixed → (:LYP1900 NIL) (:LYP2001 NIL)
```

`binfmt_misc` is not registered; `qemu-aarch64-static` must be invoked
explicitly, and the CLI reads forms from **stdin** (a filename argument is not
loaded).
