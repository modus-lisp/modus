# GATE-RESULT-i386-float — i386 floating-point codegen (#200 / #201 follow-up)

Branch `i386-float` in `/home/claude/ws-i386f`, unpushed, off `main` @ `140ae66`.

| commit | what |
|---|---|
| `71c13d6` | SSE2 float codegen: FADD / FSUB / FMUL / FDIV / ITOF / FTOI / FCMP |
| `9c51c77` | JIT exec-page traps `#x0531`–`#x0534` + SAP-NEW / SAP-ADDR |
| `55c4d2b` | `%bignum-to-float`: width-parameterise the limb radix (SHARED FILE — gated) |

---

## 1. Headline — TRANSLATOR GAPS 13 → 1

Layer-5 i386 CLI, built with

```
MODUS_I386_LAYER=5 MODUS_I386_OUT=... MODUS_I386_SYMMAP=... \
sbcl --dynamic-space-size 8192 --script mvm/build-i386-cli.lisp
```

**BEFORE** (`main` @ `140ae66`) — 13 distinct, ~165 sites:

```
  === 55 unresolved calls to 16 functions (→ %%unresolved-fn @ offset 6150367 → nil) ===
  EAX/VR INVARIANT: clean (no opcode writes EAX with a non-VR dest).
  *** TRANSLATOR GAPS (13 distinct) ***
    opcode #xC1  FDIV  x57
    opcode #xC0  FMUL  x31
    opcode #xBE  FADD  x24
    opcode #xBF  FSUB  x22
    opcode #xC2  ITOF  x17
    opcode #xC3  FTOI  x6
    trap #x0531  x4
    trap #x0520  x1
    trap #x0534  x1
    trap #x0532  x1
    trap #x0533  x1
    opcode #xB7  SAP-ADDR  x1
    opcode #xB0  SAP-NEW  x1
```

**AFTER commit 1** (floats only) — 7 distinct.
**AFTER commit 2** (traps + SAP) — 1 distinct:

```
  === 55 unresolved calls to 16 functions (→ %%unresolved-fn @ offset 6150367 → nil) ===
  EAX/VR INVARIANT: clean (no opcode writes EAX with a non-VR dest).
  *** TRANSLATOR GAPS (1 distinct) ***
    trap #x0520  x1
```

The unresolved-calls line is **unchanged** (55 / 16 functions, real stub offset
6150367 — never "NO %UNRESOLVED-FN STUB"), and the EAX/VR invariant stays clean.

`#x0520` (INSTALL-SIGNAL-HANDLERS) is **left deliberately**: it is already on
`*i386-safe-nop-traps*` — pure side effect, no result, no control transfer — so
it is *recorded* as a gap but *emitted* as a correct no-op. A real arm needs the
i386 `rt_sigaction` ABI with an `sa_restorer` trampoline, which is its own piece
of work and buys diagnostics, not correctness. Everything else is closed.

---

## 2. Probes 1–5 — unchanged

`qemu-i386-static <binary> <1..5>`, and the binary's own 91-line self-test with
no arguments. Identical on `main` and on this branch:

```
== summary  pass 90  fail 0  known-gap 0        <- built-in suite, no args

probe 1  from_start=1fffd200 to_start=2fffd000 space_size=0ffffe00
         stack_base=187ffff0 gc_count=00000000 VA=1fffd430 VL=2fffd000
         CENV=00000000 page_base=1fffd200 start_bmp=1f7fd000 cons_bmp=1effd000
probe 2  digest=e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
         collections=0
probe 3  wrong-elements=0   collections=0
probe 4  argc=2  esp=408003e0  argv[0]=<binary>  argv[1]=4  envp[0]=_=/usr/bin/timeout
probe 5  abcdefghimjklnopqrs
         e1=42  e2=3  e3=42  e4=66  e5=1  e6=42  e7=42  e8=1
```

`e1=42 e2=3 e3=42 e4=66 e5=1` holds exactly as on `main`.

---

## 3. The float codegen, and why it is shaped the way it is

x64 reassembles the four 16-bit chunks into ONE 64-bit GPR and `MOVQ`s it into
an XMM. i386 has no 64-bit GPR and **exactly two free registers** — EAX *is* VR,
ESI/EDI/EBX are V0/V1/V4, EBP is VFP — which cannot hold an operand pointer plus
a two-register 64-bit accumulator. Rather than clobber the VR, the double is
assembled **directly in memory**:

```
        sub  esp, 8
   x4:  mov  edx, [ecx + (i+1)*4]     ; slot i, tagged chunk
        shr  edx, 1                   ; untag
        mov  word [esp + (6 - 2i)], dx
        movsd xmm0, [esp]
        add  esp, 8
```

The little-endian stack image of a double puts bits 15..0 at `[ESP+0]` and bits
63..48 at `[ESP+6]`, so each chunk goes straight to its own halfword and **there
is no accumulator at all**. Peak live registers: one pointer + one chunk = 2.
Writing back is the exact inverse (`MOVZX` word / `shl 1` / store slot).

Two properties fall out for free:

* storing only `DX` masks the chunk to 16 bits, so junk above bit 15 of a slot
  cannot corrupt a neighbour — the same guarantee x64 buys with its
  `shl 48 / shr N` pair;
* `MOVZX` zero-extends, so a stored slot can never be negative — the GC-safety
  property (positive, low-bit-0, read as a fixnum by the conservative collector)
  is preserved by construction.

**SSE2, not x87, deliberately.** x87 computes in 80-bit extended precision and
storing back to a double DOUBLE-ROUNDS, which can differ from x64 in the last
bit. Bit-exact agreement with x64 is the requirement here, so SSE2 it is.
Verified available under `qemu-i386-static`. Hosted Linux/i386 already has
CR4.OSFXSR set by the kernel; a bare-metal i386 kernel would have to enable it
first, but bare-metal i386 emits no float opcode at all, so nothing there
changes.

i386 object shape respected: 4-byte header, **no padding word**, slot *i* at
`raw+(i+1)*4`, size `align16((4+1)*4) = 32` — exactly what the i386 collector's
`copy_object` derives from the header count, so allocator and collector agree.
Allocation copies `+op-alloc-obj+`'s VR-preserving shape (absolute `*va-addr*`
bump, `i386-emit-gc-mark-start`, OR in `+tag-object+` last). `:gc-check` stays
compiler.lisp's job; 32 bytes is well inside the overshoot guard.

### Register pressure: no, it did not run out

The brief flagged this as a real risk. It was avoided rather than survived: the
memory-assembly scheme never needs a third register, so **EAX is never touched**
except through `i386-store-vreg`, and the raw SSE byte sequences target ECX
only. `EAX/VR INVARIANT: clean` is the mechanized confirmation.

---

## 4. Float battery — i386 vs x64, side by side

x64 oracle: `sbcl --dynamic-space-size 8192 --script mvm/build.lisp x64/hosted/-/cli`
built from **this same branch**, run as `./modus --noinform --eval '<form>' --quit`.
i386: `qemu-i386-static <binary> 11 '<form>'` (probe 11 evaluates argv[2] through
`%cli-eval-string`; the CLI's own `--eval` path prints nothing on i386, on `main`
as well as here — a separate pre-existing `cli-toplevel` gap, see §7).

Each row prints the value **and its four raw slots** (`%prim-aref x 0..3`), so
the comparison is on IEEE bits, not on printed decimal.

| | case | form | i386 (value + slots 0..3) | x64 |
|---|---|---|---|---|
| OK | `ADD` | `(+ 1.5d0 2.25d0)` | `3.75d0 16398 0 0 0` | *(identical)* |
| OK | `NEG` | `(- 0.0d0 5.5d0)` | `-5.5d0 49174 0 0 0` | *(identical)* |
| OK | `MUL` | `(* -2.5d0 4.0d0)` | `-10.0d0 49188 0 0 0` | *(identical)* |
| OK | `DIV` | `(/ 1.0d0 4.0d0)` | `0.25d0 16336 0 0 0` | *(identical)* |
| OK | `SUB` | `(- 1.0d0 3.0d0)` | `-2.0d0 49152 0 0 0` | *(identical)* |
| OK | `RTNEG` | `(* -1.0d0 (/ 1.0d0 3.0d0))` | `-0.3333333333333333d0 49109 21845 21845 21845` | *(identical)* |
| OK | `ITOF7` | `(float 7 1.0d0)` | `7.0d0 16412 0 0 0` | *(identical)* |
| **X** | `ITOFMAX` | `(float 1073741823 1.0d0)` | `1.90294926d8 16806 44887 7168 0` | `1.073741823d9 16847 65535 65408 0` |
| OK | `ITOFMIN` | `(float -1073741824 1.0d0)` | `-1.073741824d9 49616 0 0 0` | *(identical)* |
| OK | `ITOFBIG` | `(float 1000000000000 1.0d0)` | `1.0d12 17005 6804 41472 0` | *(identical)* |
| OK | `THIRD` | `(/ 1.0d0 3.0d0)` | `0.3333333333333333d0 16341 21845 21845 21845` | *(identical)* |
| OK | `PT3` | `(+ 0.1d0 0.2d0)` | `0.30000000000000004d0 16339 13107 13107 13108` | *(identical)* |
| OK | `NEGZERO` | `(* -1.0d0 0.0d0)` | `-0.0d0 32768 0 0 0` | *(identical)* |
| OK | `INF` | `(/ 1.0d0 0.0d0)` | `#.float-infinity 32752 0 0 0` | *(identical)* |
| OK | `NEGINF` | `(/ -1.0d0 0.0d0)` | `-#.float-infinity 65520 0 0 0` | *(identical)* |
| OK | `LIT300` | `1.0d300` | `1.0d300 32311 58428 34816 30108` | *(identical)* |
| OK | `LIT300N` | `-1.0d300` | `-1.0d300 65079 58428 34816 30108` | *(identical)* |
| OK | `LIT300X2` | `(+ 1.0d300 1.0d300)` | `2.0d300 32327 58428 34816 30108` | *(identical)* |
| OK | `LITMAX` | `1.7976931348623157d308` | `#.float-infinity 32752 0 0 0` | *(identical)* |
| OK | `LITDEN` | `5.0d-324` | `0.0d0 0 0 0 0` | *(identical)* |
| OK | `LITSM` | `1.0d-308` | `0.0d0 0 0 0 0` | *(identical)* |
| OK | `LITE9` | `1.0d9` | `1.0d9 16845 52581 0 0` | *(identical)* |
| OK | `LIT602` | `6.02d23` | `6.02d23 17631 56991 4264 54113` | *(identical)* |
| OK | `E100` | `1.0d0 x10 100 times (FMUL)` | `1.0000000000000006d100 21682 18861 9620 50048` | *(identical)* |
| OK | `E308` | `1.0d0 x10 308 times` | `9.999999999999998d307 32737 52467 34283 51359` | *(identical)* |
| OK | `OVF` | `1.0d0 x10 400 times -> overflow` | `#.float-infinity 32752 0 0 0` | *(identical)* |
| -- | `DENORM` | `1.0d0 /10 320 times -> denormal` | _no output — SIGSEGV, see 7.6_ | `1.0d-320 0 0 0 2024` |
| -- | `UNDF` | `1.0d0 /10 400 times -> underflow` | _no output — SIGSEGV, see 7.6_ | `0.0d0 0 0 0 0` |
| -- | `ACC1K` | `1000 x (+ s 1/3) (FADD)` | _no output — SIGSEGV, see 7.6_ | `333.33333333333184d0 16500 54613 21845 21819` |
| -- | `GC2K` | `2000 float allocs, then read a survivor` | _no output — SIGSEGV, see 7.6_ | `0.14285714285714285d0 16322 18724 37449 9362` |
| OK | `TRUNC1` | `(truncate 3.99d0)` | `3` | *(identical)* |
| OK | `TRUNC2` | `(truncate -3.99d0)` | `-3` | *(identical)* |
| OK | `TRUNC3` | `(truncate +-0.5d0)` | `(0 0)` | *(identical)* |
| **X** | `TRUNC5` | `(truncate (float 1073741823 1.0d0))` | `206414442` | `1073741823` |
| OK | `TRUNC6` | `(truncate 1.0d9)` | `1000000000` | *(identical)* |
| OK | `CMP1` | `(< 1 2) (> -1 -2) (= 1.5 1.5)` | `(T T T)` | *(identical)* |
| OK | `CMP2` | `the false forms of CMP1` | `(NIL NIL NIL)` | *(identical)* |
| OK | `CMP3` | `(< -0.0 0.0) (= -0.0 0.0) (<= 1 1)` | `(NIL T T)` | *(identical)* |
| OK | `CMP4` | `1/3 bracketed by 0.33/0.34` | `(T T)` | *(identical)* |
| OK | `SQRT2` | `(sqrt 2.0d0)` | `1.414213562373095d0` | *(identical)* |
| OK | `SINGLE` | `(+ 1.5 2.25) single` | `3.75` | *(identical)* |
| OK | `SING2` | `(float 7 1.0) single` | `7.0` | *(identical)* |
| OK | `EXPT` | `(expt 2.0d0 10)` | `1024.0d0` | *(identical)* |
| OK | `FCR` | `(floor/ceiling/round 7.5d0)` | `(7 8 8)` | *(identical)* |
| OK | `FCRN` | `(floor/ceiling/round -7.5d0)` | `(-8 -7 -8)` | *(identical)* |
| OK | `RT` | `read-from-string -1.25d0 / 6.02d23` | `(-1.25d0 6.02d23)` | *(identical)* |
| -- | `FMT` | `(format nil "~F|~A" 3.14159d0 3.14159d0)` | _no output — SIGSEGV, see 7.6_ | `"3.14159|3.14159d0"` |

**40 of 47 bit-identical.**

---

## 5. What the base build does with the same forms

On `main`, `(+ 1.5d0 2.25d0)` does not produce a wrong answer — it does not
produce an answer:

```
$ qemu-i386-static base-cli 11 '(print (+ 1.5d0 2.25d0))'
...
L=24 C=40
qemu: uncaught target signal 5 (Trace/breakpoint trap) - core dumped
```

That is the FADD `INT3` placeholder. Every float value in §4 is new capability,
not a changed one.

---

## 6. The one shared-file change, and its gate

`mvm/cl-types.lisp` — `%bignum-to-float` (commit `55c4d2b`).

Found while diffing against x64: every double literal whose mantissa or divisor
exceeds the 31-bit fixnum range read WRONG on i386, **and differently on every
parse** — `1.0d300` came out as infinity, `1.0d9` as 0.

The codegen was correct; `%bignum-to-float` was not. It folds a bignum's limbs
MSB-first in float domain with radix `2^+limb-bits+`, and named that radix as
the square of a **hardcoded 2147483648** — 2^31, correct only on the 62-bit
tower. On the 30-bit i386 tower 2147483648 is itself a BIGNUM, and
`%float-from-int` is `cvtsi2sd` on a FIXNUM, so it converted the bignum's heap
POINTER: the radix became a function of the allocation address. That is the
identical failure this function's own docstring was written to prevent,
reintroduced one level up inside the fix. Same class as the `+half-limb-mask+`
hardcoding already fixed in `target.lisp`, whose comment says it outright:
*"silently wrong on any tower narrower than 62"*.

```lisp
-  (b31  (%float-from-int 2147483648))              ; 2^31
-  (base (%float-mul b31 b31))                      ; 2^62
+  (bhl  (%float-from-int (ash 1 +half-limb-bits+))); 2^(limb-bits/2)
+  (base (%float-mul bhl bhl))                      ; 2^+limb-bits+
```

**x64/aarch64 are unaffected by construction**: `+half-limb-bits+` is 31 on the
62-bit tower, so `(ash 1 +half-limb-bits+)` *is* 2147483648 and `base` *is* 2^62
— the same two values, computed from the width constant instead of typed in. On
i386 it becomes 15 / 2^30, the correct radix there.

Measured effect on i386: `1.0d300` infinity → `1.0d300`; `1.0d9` 0 → `1.0d9`.

### 6.1 64-shard x64 ANSI gate

```
BASE (main 140ae66)          : passed=17301  CHUNK-CRASH=0  FILE-WEDGE=30   (NSH=64)
NET  (i386-float 55c4d2b)    : passed=17301  CHUNK-CRASH=0  FILE-WEDGE=30   (NSH=64)

comm -23 base net  (losses, base-only) : 0
comm -13 base net  (gains,  net-only)  : 0
```

`ansi-file-ranges.txt` is **byte-identical** between the two builds, so the
per-ID diff is valid (no corpus shift).  Both binaries were built from scratch
for this run, both sweeps `NSH=64` over `10001..27800`, same budget both sides.

The per-ID sets are **exactly equal** — not "no net change", literally the same
17301 IDs on both sides.  For a change whose x64 arm is the same two constants
computed instead of typed, that is the expected result and it is what happened.

**Two shards hung and were re-run.**  `13349..13627` and `13628..13906` — the
`decf`/`float`/`incf` cluster — stalled on **both** binaries: the harness forks
per file, `timeout` kills only the parent, and the orphaned forks hold the pipe
open so `n5gate.sh`'s `wait` never returns.  (Not a property of this change; it
is the fork/pipe interaction, and it happened symmetrically.)  Both were killed
and then re-run standalone, **3 reps on each binary**, with a 1800 s budget:

```
rep1  13349..13627 : base 1122 unique   net 1122 unique   losses 0  gains 0
rep2  13349..13627 : base 1122          net 1122          losses 0  gains 0
rep3  13349..13627 : base 1122          net 1122          losses 0  gains 0
rep1  13628..13906 : base  584          net  584          losses 0  gains 0
rep2  13628..13906 : base  584          net  584          losses 0  gains 0
rep3  13628..13906 : base  584          net  584          losses 0  gains 0
```

Identical ID sets in every rep on both sides, and the re-runs recover far more
coverage than the truncated shards contributed (246/65) — so the range that a
`%bignum-to-float` change would most plausibly disturb is the range with the
strongest evidence of no disturbance.

Crash markers, the timing-immune gate: `CHUNK-CRASH 0 → 0`, `FILE-WEDGE 30 → 30`.

**Conclusion: zero regressions, zero gains, x64 untouched.**

---

## 7. Honest disagreements, and where they come from

### 7.1 NOT a float bug: i386 miscompiles integer LITERALS ≥ 2^29 (pre-existing)

The only two battery rows that disagree — `ITOFMAX` = `(float 1073741823 1.0d0)`
and `TRUNC5`, which truncates that same value — are not ITOF. `ITOF` is fine:
`(float 7 1.0d0)`, `(float -1073741824 1.0d0)` and `(float 1000000000000 1.0d0)`
are all bit-exact. What is wrong is the *literal*, and it is a pre-existing i386
bug with no floats in it at all:

```
$ qemu-i386-static base-cli 11 '(print (list 536870911 536870912 1073741823 (+ 536870911 1)))'
(536870911 185348330 185348870 536870912)
        ^ 2^29-1 OK   ^ 2^29 WRONG  ^ 2^30-1 WRONG   ^ computed, OK
```

Reproduced identically on `main`'s binary (above) and on this branch. Note the
last column: the *same magnitude* computed arithmetically is correct — only the
LITERAL is wrong, and successive wrong values differ by ~540, i.e. they are
consecutive heap addresses.

Root cause, and it matches the boundary exactly: an integer literal is emitted
as `:li (* n 2)`. On i386 the tagged word of 2^29 is 2^30 = 1073741824, which is
**greater than `+fixnum-max+` = 2^30-1**, so the in-image compiler's checked `*`
promotes it to a BIGNUM — and `:li`'s `(logand imm #xFFFFFFFF)` on a bignum is
the documented lossy in-image `logand`, yielding the bignum's address. Exactly
the `:mul-checked` / intern-composite-key class already on record. It bites the
**in-image compiler only** (runtime `eval`); build-time literals are computed in
host arithmetic and are fine, which is why the baked image's own 90-test suite
passes.

Filed as a separate follow-up. It is not float work and its fix belongs in
`compile-literal` / the `:li` path, not in `translate-i386.lisp`.

### 7.2 Cases where i386 and x64 AGREE on a value that is itself wrong

`5.0d-324` reads as `0.0d0` and `1.7976931348623157d308` reads as infinity on
**both** arches. These are pre-existing limits of `%build-float-from-parts`
(the decimal exponent loops lose the extremes), not an i386 divergence — i386
matches the oracle, which is the property under test here. Note that the
*computed* denormal `1.0d-320` (320 successive FDIVs) IS bit-exact on both, so
the arithmetic reaches the denormal range correctly; only the literal reader
does not.

### 7.3 `:ftoi` overflow — inherited from x64, worse on i386

`CVTTSD2SI` returns the integer-indefinite value out of range and the following
`shl 1` turns it into 0 — a silent wrong answer rather than a bignum or an
error. x64 has this today; i386 hits it at 2^30 instead of 2^62 because its
fixnums are 31-bit. Documented at the opcode. Fixing it is one change in both
translators and is deliberately not bundled here.

### 7.4 `SAP-ADDR` tags with `shl 1`

Lossy above 2^31 on a 32-bit word, exactly as x64's is above 2^63. Every SAP in
this image points into the 0x1FFF_xxxx heap, so it does not bite; stated at the
opcode rather than silently assumed.

---

### 7.5 `--eval` prints nothing on i386 (pre-existing, not float)

`./modus-i386 --eval '(print 42)' --quit` exits 0 and prints nothing, on `main`
as well as on this branch — the i386 `cli-toplevel` action path is already
broken (the build script carries a whole `probe-cli` section written to debug
exactly that). Every i386 measurement here therefore goes through probe 11,
which feeds argv[2] to `%cli-eval-string` — the same runtime string path
`cli-toplevel` would use, so it is the closest available stand-in. Probe 11
evaluates its form three times (its `R`/`S`/`T` markers); the tables use the
first result and every rep agreed.

### 7.6 The two SIGSEGVs in §4 — what they are, and what they are not

Both reproduce **identically on the floats-only build (`net-cli`, commit 1) and
on the final build**, so neither is caused by the traps/SAP commit or the
`%bignum-to-float` commit. Neither can exist on `main`, which cannot construct a
float at all.

**Crash A — `(format nil "~F" <inexact double>)`.** Bisected by value:

```
~F 0.5d0    OK        ~F 0.1d0      SIGSEGV
~F 0.25d0   OK        ~F 3.1d0      SIGSEGV
~F 1.25d0   OK        ~F 3.14d0     SIGSEGV
~F 2.5d0    OK        ~F 3.14159d0  SIGSEGV
~F 3.5d0    OK        ~F 0.14159d0  SIGSEGV
~F 3.0d0    OK
```

The split is exact: every value that is **exactly representable in binary**
formats fine; every value that is **not** crashes, i.e. the crash is in `~F`'s
digit-generation loop, the only part an inexact value reaches. `(format nil "~A"
3.14159d0)` → `"3.14159d0"` works, so the printer's own float→string path is
fine; `(format nil "~A" 12345)` works on `main`, so `format` itself is fine.
It is `~F`'s scaling arithmetic only.

Classification: the same **31-bit-fixnum width-safety** family as
`%bignum-to-float` in §6 — `%format-f` scales by powers of ten, which overflow a
30-bit tower far sooner than a 62-bit one. It is shared Lisp, not codegen: the
operand bits reaching `~F` are provably correct (`PT3` = `0.1d0 + 0.2d0` is
bit-identical to x64, as is `RT`'s read of `-1.25d0` / `6.02d23`). Fixing it
means auditing `%format-f` for the 30-bit tower and re-running the 64-shard
gate — a separate change, deliberately not bundled here.

**Crash B — cumulative, under sustained float allocation.** The battery's
`DENORM` (320 successive FDIVs) is bit-exact when run alone, on both builds:

```
(:DENORM 1.0d-320 0 0 0 2024)      <- i386, identical to x64
```

It only fails when preceded in the same evaluated form by the `E308` + `OVF`
loops (708 more float ops) *with* their four `%prim-aref` reads and 6-element
result lists. Bisection: `(progn OVF DENORM)`, `(progn E308 DENORM)`,
`(progn OVF OVF)`, `(progn DENORM DENORM)` and `(progn OVF (/ 1.0d0 3.0d0))`
**all pass**; only the fuller form crashes. That profile — passes at N
allocations, fails at N+k, in the interpreter — points at a heap/GC boundary
rather than at any single opcode.

**The obvious suspect — "floats are not GC-safe" — was tested and RULED OUT.**
A dedicated `MODUS_I386_GCSTRESS=65536` image (forces a collection every 64 KiB,
so *every* float allocation is exercised across collections) was built and run.
A float allocated **before** 200 further float allocations, i.e. across many
forced collections, comes back **bit-exact**:

```
$ qemu-i386-static gcs-cli 11 '(let ((k (/ 1.0d0 7.0d0)) (i 0))
    (loop (when (>= i 200) (return nil)) (* 1.5d0 2.5d0) (setq i (+ i 1)))
    (print (list :surv k (%prim-aref k 0) ... )))'
(:SURV 0.14285714285714285d0 16322 18724 37449 9362)
```

which is byte-for-byte the x64 `GC2K` row in §4. Plain arithmetic and slot reads
under the same stress build are bit-exact too:

```
(:ADD 3.75d0 :THIRD 0.3333333333333333d0 :NEG -5.5d0)
(:B 0.3333333333333333d0 16341 21845 21845 21845)
```

So the 4-slot float object is allocated, marked, copied and re-read correctly by
the i386 collector. That is consistent with the structural argument:
`i386-emit-float-alloc` cannot itself be interrupted by a GC — there is no
`:gc-check` and no call between the VA bump and the fourth slot store, so a
float is never observable partially-initialised, which is strictly *safer* than
`+op-alloc-obj+` (whose payload arrives via later `:obj-set` opcodes that *can*
be separated by a `:gc-check`).

**What crash B actually is remains OPEN**, and it is stated as open rather than
guessed at. The GC-stress runs of the full `churn` and `g5` forms did not
finish: a 64 KiB collection trigger under qemu is slow enough that both hit
their 2400 s wall-clock cap and were **killed by `timeout`, not by a signal** —
`g5` had by then printed its `E308` row bit-exactly and had *not* reached a
SIGSEGV. So those two runs are inconclusive on their own; do not read the
"Killed" lines in `tmp/i386f/gcs.log` as a reproduction. Finishing them on a
quiet box with a larger stress limit, then bisecting crash B on allocation
count, is the next step. It is a real SIGSEGV and should not be shelved — but
it is not "the float object is GC-unsafe", which is the hypothesis that was
actually testable and is now falsified.

---

## 8. Where the evidence lives

Under `tmp/i386f/` and `tmp/g/` in this worktree (both gitignored):

```
base-cli, base-build.log        main @ 140ae66, 13 gaps — the BEFORE side
net-cli,  net-build.log         after commit 1 (floats)      — 7 gaps
net2-cli, net2-build.log        after commit 2 (traps+SAP)   — 1 gap
net3-cli, net3-build.log        after commit 3               — 1 gap, the binary used in §4
gcs-cli,  gcs-build.log         MODUS_I386_GCSTRESS=65536 build (§7.6)
gcs.surv200 / gcs.suite / gcs.bits    GC-stress float results
iso2.* / iso3.* / iso4.* / iso5.*     the two crash bisections
verify3.log                     self-test + probes 1-5 on net3
battery3.txt                    the 47-form battery
final.i386.raw / final.x64.raw  the two sides of §4
i386b.raw / x64b.raw            the earlier 25-form bit-level battery
runbat3.sh                      ./runbat3.sh <i386-binary> <tag>
tmp/g/ansi-net                  NET x64 ANSI gate binary
tmp/g/base.out / net.out        the two 64-shard sweeps (sorted P: IDs)
tmp/g/rs.{base,net}.*.{1,2,3}   the 3-rep re-runs of the two hung shards
tmp/g/reshard.sh                the re-run driver
```

BASE x64 ANSI gate binary and its worktree: `/home/claude/ws-i386f-base`
(detached at `main` @ `140ae66`), binary `tmp/g/ansi-base`.

The x64 oracle CLI is `./modus` in this worktree.
