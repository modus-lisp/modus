# Task #221 — sweep of the aarch64 `ensure-src` scratch-clobber class

Branch `aa64-clobber`, off `main` @ `303c6de`. One file changed:
`mvm/translate-aarch64.lisp`. Two commits (`6441f39`, `04d767d`).

---

## 0. Summary

| site | verdict | live trigger constructed? | runtime-confirmed? |
|---|---|---|---|
| `+op-consp+` | **REAL BUG — `(consp nil)` returned T** | yes | **yes** |
| `+op-atom+` | **REAL BUG — `(atom nil)` returned NIL** | yes | **yes** |
| `+op-obj-set+` (slot idx > 31) | **REAL BUG — store to a wild address** | yes | **yes** |
| `+op-atomic-xchg+` | real in codegen, `pd == pa` destroys the address | no — opcode is emitted by no first-party source in these images | no — **fixed unverified** |
| `+op-call-ind+` | real in codegen, `SUB ps,ps,#3` mutates a live vreg (1517 sites) | no — benign under the current allocator | no — **fixed unverified** |
| `+op-percpu-set+` (large offset) | real in codegen, stores the OFFSET instead of the value | no — the arm is provably dead | no — **fixed unverified, byte-neutral** |

The first three are the ones the task asked me to verify; all three reproduce
at runtime and all three are fixed. The other three came out of the sweep.
The remaining **96 of 102** `ensure-src` call sites are safe, each for a stated
reason (§4).

x64 is provably untouched: `x64/bare/qemu/repl` rebuilds to
**`269b461a764016eea6533c46798ad3e4`**, the `mvm/BUILDS.md` hash, from this
branch after the change.  The aarch64 ANSI gate is **17348 = 17348 over the
FULL corpus, +0 / −0 reproducible, crash markers identical** (§6.2).

---

## 1. Method — the instrument, and its validation

A static read is a hypothesis. I built an instrumented translator that logs,
for the whole aarch64 CLI image:

- a **histogram**: every `ensure-src` call that returned the SCRATCH (i.e. the
  vreg was spilled), keyed by `(opcode . scratch-register)`; and
- **per-site records** for the specific shapes under suspicion, each resolved
  to its enclosing function through the module's `mvm-function-info` table.

**The instrument validates against known ground truth.** #220 reported exactly
**9** live `:mod` clobber sites in this image. The histogram independently
reports:

```
mod              x16  n=9
```

Nine, in the same image, from an independent measurement path. That is the
reason to trust the rest of the table.

### Full spill histogram — aarch64 CLI image (`aarch64/hosted/-/cli`)

Every opcode that ever received the scratch back from `ensure-src`:

```
cmp         x17 422 | cmp         x16 251 | test        x17 236
push        x16 206 | mov         x16 131 | and         x17 128
store       x16  95 | test        x16  92 | store       x17  78
obj-set     x17  68 | obj-subtag  x16  56 | cons        x17  29
and         x16  29 | obj-tag     x16  28 | aref        x17  28
u8-ref      x17  28 | or          x17  23 | bnull       x16  12
sub-checked x17  10 | mul-checked x17   9 | mod         x16   9   <-- #220
div         x16   9 | or          x16   2 | shr         x16   2
car         x16   1 | consp       x16   1 | add-checked x17   1
mul-checked x16   1 | sub-checked x16   1 | load        x16   1
mod         x17   1 | div         x17   1
```

`atom` and `atomic-xchg` do not appear: **zero** spilled sources in this image.

### Per-site records

```
CONSP        1 site   : bc=6761783 vd=V10 vs=V9  fn=MVM-COMPILE-ALL
ATOM         0 sites
OBJ-SET >255 8486 sites, ALL with pobj != x16   (0 bad)
ATOMIC-XCHG  0 sites
CALL-IND     1517 sites where ps is a REAL vreg register (x19-x23)
```

**Honest reading of the CONSP result:** the one live site in the shipping CLI
image is

```lisp
(cond ((null result) nil)
      ((and (consp result) (eq (car result) :multi-result)) …)   ; <- the site
      …)
```

in `MVM-COMPILE-ALL` (`mvm/compiler.lisp:17356`). The `(null result)` clause
immediately above it filters NIL, and NIL is the *only* value the bug gets
wrong (§2). So this particular site **cannot manifest**. I am not claiming an
in-the-wild failure in the CLI image; I am claiming the codegen is wrong and
demonstrating it with a constructed trigger.

---

## 2. Why only NIL — the mechanism

`+op-consp+` emitted (main):

```
ps = ensure-src(Vs, x16)      ; x16 is the SCRATCH — ps IS x16 when Vs spills
pd = phys(Vd) or x17

MOVZ x17, #0xF
AND  x16, ps, x17             ; if ps IS x16 -> ps is DESTROYED here
CMP  ps,  x26                 ; ... so this compares the MASKED NIBBLE vs NIL
CSEL x16, XZR, x16, EQ        ; never EQ -> nibble survives
CMP  x16, #1
CSEL pd,  x18(T), x26(NIL), EQ
```

After the `AND`, `ps` names the nibble, so the result reduces to
`(nibble == 1)`. For every value except NIL that is the *correct* answer — the
cons tag *is* 1. NIL is `#xDEAD0001`, whose low nibble is also 1; the whole
point of the `CMP ps, x26` pre-check is to special-case it, and the clobber is
exactly what defeats that pre-check.

Hence the sharp, testable signature: **wrong for NIL only, right for
everything else** — and, because `+op-atom+`'s final CSEL uses `NE` instead of
`EQ`, the failure inverts there. Both confirmed below.

`compile-consp` emits `(:consp dest dest)` — Vd and Vs are the *same* vreg — so
the trigger condition is simply "the destination is a spill slot".

### `+op-obj-set+`

```
ps   = ensure-src(Vs,   x17)
pobj = ensure-src(Vobj, x16)
offset = idx*8 + 7
  offset > 255:
    load-imm64 x16, offset        ; pobj DESTROYED when pobj IS x16
    ADD  x16, pobj, x16           ; = offset + offset
    STUR ps, [x16]                ; store to 2*offset — a wild address
```

Because `ps` already claims x17, x16 was the only scratch left, which is
precisely why `+op-obj-ref+` (whose address temp is x17, and which has no
x17-scratch source) was safe and `+op-obj-set+` was not.

`idx*8+7 > 255` means slot index > 31. Reachable from ordinary Lisp: a quoted
**string literal of ≥ 32 characters**, a quoted **vector literal of ≥ 32
elements**, or `%word-aset` with a constant index > 31 — each compiled where
the destination is a spill slot. `compile-quote`'s string path emits
`(:obj-set dest i temp)` for `i = 0..n-1` against the incoming `dest`.

---

## 3. Runtime confirmation

Forcing the spill is the hard part, and the task said so. `alloc-temp-reg`
hands out V4… in order and V9+ are spill slots, so the operand lands in a spill
slot once ~5 temps are held across the recursion. Driving the **real** compiler
plus the **real** aarch64 translator inside SBCL (seconds per shape, no image
build) located the threshold exactly:

```
arith-nest-5  consp : 0 clobber sites
arith-nest-6  consp : 1 clobber site   (:CONSP :VD 9 :VS 9)   <-- V9 = spilled
arith-nest-7  consp : 1                (:CONSP :VD 10 :VS 10)
strlit-nest-4       : 0
strlit-nest-5       : 8 OBJ-SET-BAD sites (slots 32..39)
```

The resulting probe set, with **controls that must not move**:

```lisp
(defun %aa64-str-tail (s) (char-code (char s 39)))
(defun %aa64-consp-nospill (x) (if (consp x) 1 0))          ; control
(defun %aa64-atom-nospill  (x) (if (atom  x) 1 0))          ; control
(defun %aa64-consp-spill (x) (+ 6 (+ 5 (+ 4 (+ 3 (+ 2 (+ 1 (if (consp x) 1 0))))))))
(defun %aa64-atom-spill  (x) (+ 6 (+ 5 (+ 4 (+ 3 (+ 2 (+ 1 (if (atom  x) 1 0))))))))
(defun %aa64-strlit-spill ()
  (+ 5 (+ 4 (+ 3 (+ 2 (+ 1 (%aa64-str-tail "abcdefghijklmnopqrstuvwxyz0123456789ABCD")))))))
(defun %aa64-strlit-nospill ()                              ; control
  (%aa64-str-tail "abcdefghijklmnopqrstuvwxyz0123456789ABCD"))
```

Base is `1+2+3+4+5+6 = 21` (`+1` if the predicate is true) and
`1+2+3+4+5 = 15` (`+68`, `#\D`, for the string). Compiling this source through
the instrumented translator shows the spill variants hit all three sites and
the `nospill` variants hit **none** — i.e. the controls are real controls, not
decoration.

These were baked into two `aarch64/hosted/-/cli` images differing **only** in
the translator, and run under `qemu-aarch64-static`:

| probe | argument | BEFORE (main) | AFTER (fixed) | correct |
|---|---|---|---|---|
| `%aa64-consp-nospill` | `nil` | 0 | 0 | 0 |
| `%aa64-consp-nospill` | `(cons 1 2)` | 1 | 1 | 1 |
| `%aa64-atom-nospill` | `nil` | 1 | 1 | 1 |
| `%aa64-atom-nospill` | `(cons 1 2)` | 0 | 0 | 0 |
| **`%aa64-consp-spill`** | **`nil`** | **22** ✗ | **21** ✓ | 21 |
| `%aa64-consp-spill` | `(cons 1 2)` | 22 | 22 | 22 |
| `%aa64-consp-spill` | `7` | 21 | 21 | 21 |
| **`%aa64-atom-spill`** | **`nil`** | **21** ✗ | **22** ✓ | 22 |
| `%aa64-atom-spill` | `(cons 1 2)` | 21 | 21 | 21 |
| `%aa64-atom-spill` | `7` | 22 | 22 | 22 |
| `%aa64-strlit-nospill` | — | 68 | 68 | 68 |
| **`%aa64-strlit-spill`** | — | **`ERROR #(PROGRAM-ERROR NIL)`** ✗ | **83** ✓ | 83 |

Every control is identical on both sides, and every non-NIL argument is
identical on both sides — exactly the "wrong for NIL only" signature §2
predicts. `22` for `(consp nil)` is `(consp nil) => T`; `21` for `(atom nil)`
is `(atom nil) => NIL`. The string probe's wild store lands at
`2*(39*8+7) = 638`, an unmapped address; the boot SIGSEGV handler converts it
to the `PROGRAM-ERROR` shown.

The probe source was wired into `mvm/build-aarch64-cli.lisp` for these two
builds only and is **not** part of the branch — `git status` is clean.

`cli-BASE` md5 `89d5c64efa87dd9a55281f6e0eaacfce`,
`cli-FIXED` md5 `d1b55288c3b115f59df06413b6e8f204`.

---

## 4. The sweep — all 102 `ensure-src` call sites

`translate-mvm-insn` has **102** `ensure-src` call sites across **65** opcodes
(enumerated mechanically, not by eye; the `phys`/`phys2` macros defined
alongside `ensure-src` are dead — zero uses). Below, "same-insn" means the
write and the last read are the *same* AArch64 instruction, which is safe: an
instruction reads all its source registers before writing `Rd`.

### 4.1 The six unsafe sites

| opcode | line | scratch | collision condition | consequence | status |
|---|---|---|---|---|---|
| `+op-consp+` | 3329 | `ps`→x16 | Vs spilled ⇒ `ps == x16`, then `AND x16,ps,x17` | `(consp nil)` ⇒ **T** | fixed → x9/x10 |
| `+op-atom+` | 3364 | `ps`→x16 | same | `(atom nil)` ⇒ **NIL** | fixed → x9/x10 |
| `+op-obj-set+` | 3460 | `pobj`→x16 (`ps`→x17) | Vobj spilled **and** slot idx > 31 | store to `2*offset` | fixed → x9 |
| `+op-atomic-xchg+` | 3996 | `pa`→x16 | `pd == pa` (both default x16; or Vd == Vaddr) | `LDXR pd,[pa]` destroys the address | fixed → x9/x10 |
| `+op-call-ind+` | 3751 | `ps`→x16 | Vs **not** spilled ⇒ `ps` is a real vreg reg | `SUB ps,ps,#3` corrupts a live vreg | fixed → x16 |
| `+op-percpu-set+` | 4131 | `ps`→x17 | Vs spilled **and** large offset | stores the OFFSET, not the value | fixed → x9 |

### 4.2 The 96 safe sites, by reason

**(a) Single-instruction three-address form — sources read, `Rd` written, one
instruction.** Safe for every aliasing of `pd`/`pa`/`pb`.
`add` `sub` `adds` `subs` (L2707-2738) · `and` `or` `xor` (L3051-3072) ·
`mul64lo` `mul64hi` (L2916/2928) · `shlv` `sarv` (L3111-3122) ·
`neg` (L3025) · `ldb` (L3132) · `obj-tag` (L3477) ·
`shl` `shr` `sar` (L3081-3101, UBFM/SBFM) · `car` `cdr` (L3275/3285, LDUR
reads the base then writes Rt) · `setcar` `setcdr` (L3311-3319) ·
`load` (L3656) · `store` (L3665) · `io-write` (L4032, writes x16 while `ps`
is x17) · `inc` `dec` (L3035/3043 — dest *is* the source, `ADD pd,pd,#2`).

**(b) Flags-only — no register is written at all.**
`cmp` `test` (L3142-3149) · `bnull` `bnnull` (L3242/3257) · `fcmp` (L4346).

**(c) The scratch write is the same instruction as the last source read.**
`mul` (L2871: `ASR x16,pa,#1`) · `mul26lo` `mul26hi` (L2885/2901: `ASR x16,pa`
then `ASR x17,pb`) · `div` (L2972: `SDIV x16,pa,pb` — **this is exactly why
#220 was remainder-only and the quotient was always right**) ·
`add-checked` `sub-checked` (L2768/2796: `ADDS x16,pa,pb`) ·
`aref` `aset` (L3535/3547: `ADD x16,pobj,#7`) ·
`alloc-array` `alloc-string` (L3578/4203: `ADD x17,pcount,#3`).

**(d) The slow path re-reads the VREG, not the resolved register.** The
overflow arms of `add-checked`/`sub-checked`/`mul-checked` call
`a64-emit-generic-arith-call buf va vb`, which re-materialises both operands
from their spill slots / physical registers via `a64-emit-load-vreg` into
x9/x10. `pa`/`pb` being clobbered on the fast path is therefore irrelevant.
This is the "9 candidates, 8 false positives" family #220 §1 refers to.

**(e) The source is consumed before the next write.**
`mov` (L2661) · `push` (L2676) · `mul-checked` (L2821: `ASR x9,pa,#1` and
`MOV x10,pb` precede `MUL x16,…`) · `acc128` (L2946/2951/2953: each
`ensure-src` result is copied out by the very next instruction) ·
`u8-ref` (L3628) · `u8-set` (L3644 — note this opcode already uses **x9** as a
third `ensure-src` scratch, from `fb38bd3`) · `obj-subtag` (L3500: `ps` is
re-read after two x17 writes, but x17 can never *be* `ps` — see (f)) ·
`array-len` (L3560) · `itof` `ftoi` (L4310/4331) ·
`fadd`/`fsub`/`fmul`/`fdiv` (L4278: `a64-float-load-bits` clobbers only x9/x10
and preserves its address operand) · `set-cenv` (L4237).

**(f) The written register can never be the resolved source, by construction.**
x17 is not in `*a64-vreg-to-phys*`, so a source whose scratch is x16 can never
*be* x17, and vice versa. This is what makes `+op-obj-ref+` (L3434) safe in the
large-offset arm — it stages the offset in **x17** while its only source scratch
is x16 — and it is precisely the property `+op-obj-set+` lacked, because `ps`
had already taken x17 there. Same reason for `obj-subtag`, `array-len`,
`io-write`, `percpu-ref`.

**(g) `cons` (L3297) — safe, but load-bearingly so.** `emit-aarch64-gc-mark-
start` / `-cons` run *between* the `ensure-src` calls and the `STP pa,pb,[x24]`.
They are safe only because `emit-aarch64-gc-set-bit` clobbers **x9..x13 only**
(documented in its docstring and verified in the body). If that helper ever
reached for x16/x17, `+op-cons+` would silently start storing garbage cars and
cdrs. Flagging, not fixing.

**(h) `aset` (L3547) — safe by statement ordering.** The address is fully
computed into x16 *before* the third `ensure-src` loads `ps` into x17. Reorder
those two and it breaks. Flagging, not fixing.

**(i) `save-ctx` (L3888) / `restore-ctx` (L3947) — scratch is x0.**
`restore-ctx` is defensively written (`MOV x16, pa` on the first line, with a
comment saying why). `save-ctx` is safe only because its `MOVZ x0,#0` falls
*after* the last `STR …,[pa,…]`. Flagging, not fixing.

**(j) No `ensure-src` at all** (immediate-only or no operands):
`nop` `break` `trap` `li` `pop` `bvs` `li-const` `br` `beq` `bne` `blt` `bge`
`ble` `bgt` `alloc-obj` `alloc-cons` `gc-check` `mcgc-collect` `write-barrier`
`fence` `call` `tailcall` `ret` `yield` `halt` `cli` `sti` `io-read`
`percpu-ref` `set-nargs` `get-nargs` `set-mv-count` `get-cenv` `fn-addr`.
(`alloc-obj` deserves a note: `pd` may be x16 and the header also goes through
x16, but the header is stored before `ADD pd,x24,#9`.)

### 4.3 How the two passes differed — and why that matters

The first pass looked for "writes **x16** while an x16-scratch source is live"
and found four sites. A second, separate pass looking for "writes **x17** while
an **x17**-scratch source is live" found `+op-percpu-set+`, which the first pass
had walked straight past. Both directions are needed; either alone is
incomplete. Recorded here because the next person to sweep this file should run
both.

---

## 5. The fixes

Pattern, from #220: route the temporary through a register that is not a vreg
and not an `ensure-src` scratch **in that opcode**, or reorder so all sources
are read before any destination is written.

```lisp
;; consp / atom : nibble -> x9, mask -> x10 (was x16 / x17)
(a64-movz    buf +a64-x10+ #xF 0)
(a64-and-reg buf +a64-x9+  ps +a64-x10+)
(a64-cmp-reg buf ps +a64-x26+)
(a64-emit    buf (logior #x9A800000 (ash +a64-x9+ 16) (ash +cc-eq+ 12)
                         (ash 31 5) +a64-x9+))          ; CSEL x9, XZR, x9, EQ
(a64-cmp-imm buf +a64-x9+ 1)

;; obj-set large offset / percpu-set large offset : address temp -> x9
(a64-load-imm64 buf +a64-x9+ offset)
(a64-add-reg    buf +a64-x9+ pobj +a64-x9+ 0 0)
(a64-stur       buf ps +a64-x9+ 0)

;; atomic-xchg : load into x9, status in x10, MOV to pd after the loop
(a64-ldxr    buf +a64-x9+ pa)
(a64-stxr    buf +a64-x10+ ps pa)
;; CBNZ w10, loop
(a64-mov-reg buf pd +a64-x9+)

;; call-ind : strip into x16 rather than in place
(a64-sub-imm buf +a64-x16+ ps 3)
…
(a64-blr     buf +a64-x16+)
```

**Correction to a claim inherited from #220's comment:** x9 is *not* "never an
`ensure-src` scratch" — `+op-u8-set+` uses it as a third scratch (`fb38bd3`).
The property that licenses these fixes is **per-opcode**, because register
state never crosses an MVM instruction boundary. The comments now say that.
None of the six touched opcodes allocates (so none emits
`emit-aarch64-gc-set-bit`, which owns x9..x13), and none of them uses x9/x10
for anything else.

Instruction counts: unchanged for consp, atom, obj-set, percpu-set and
call-ind. `+op-atomic-xchg+` grows by one `MOV`; the `CBNZ` back-offset is
computed before that `MOV` is emitted, so the loop body length is untouched.

### Encoding verification

Every new encoding assembled with GNU `aarch64-linux-gnu-as` and disassembled
with `objdump`, then compared against what the Lisp emitters compute:

```
movz x10, #0xf              d28001ea    ✓
and  x9, x16, x10           8a0a0209    ✓   (Rd=9  Rn=16 Rm=10)
and  x9, x19, x10           8a0a0269    ✓   (unspilled source)
csel x9, xzr, x9, eq        9a8903e9    ✓
cmp  x9, #0x1               f100053f    ✓
movz x9, #0x1234            d2824689    ✓
add  x9, x16, x9            8b090209    ✓
add  x9, x19, x9            8b090269    ✓
stur x17, [x9]              f8000131    ✓
ldxr x9, [x16]              c85f7e09    ✓
stxr w10, x17, [x16]        c80a7e11    ✓
cbnz w10, .-8               35ffffca    ✓
mov  x0, x9                 aa0903e0    ✓
mov  x16, x9                aa0903f0    ✓
sub  x16, x19, #0x3         d1000e70    ✓
sub  x16, x16, #0x3         d1000e10    ✓   (== the old bytes when ps was x16)
blr  x16                    d63f0200    ✓
```

### Structural check on the built image

Disassembling the two probe CLIs shows a clean 1:1 substitution at every
consp/atom site — not a partial edit:

| pattern | `cli-BASE` | `cli-FIXED` |
|---|---|---|
| `csel x16, xzr, x16, eq` | **14002** | **0** |
| `csel x9,  xzr, x9,  eq` | **0** | **14002** |

---

## 6. Regression evidence

### 6.1 x64 — byte-identical, measured

Only `mvm/translate-aarch64.lisp` changed, so x64 codegen cannot move. That is
demonstrated rather than asserted: `x64/bare/qemu/repl` rebuilt from this
branch *after* the change gives

```
269b461a764016eea6533c46798ad3e4   /tmp/modus-x64.bin   (81168 bytes)
```

— exactly the `mvm/BUILDS.md` hash. The x64 ANSI gate is therefore not
applicable, and this hash is the stronger statement.

### 6.2 aarch64 ANSI gate — four targeted windows + the FULL corpus

`mvm/build-aarch64-linux.lisp`, `--dynamic-space-size 16384`, run under
`qemu-aarch64-static`. BASE built from a clean `git archive main` export
(verified unfixed: `csel x16,xzr,x16` still present, no `x9` nibble). NET built
from **this branch's HEAD `04d767d`** — the first NET image was accidentally
built from `6441f39`, one commit short, and was discarded and rebuilt rather
than reported. Both sides use the identical shard geometry and timeout, and
every pair was run **sequentially, never overlapping**. `ansi-file-ranges.txt`
is byte-identical on both sides, so the corpus geometry is the same.

Sharding script (`NSH` shards, `TMO`s each, per-ID `P:` lines sorted -u):

```bash
for ((i=0;i<NSH;i++)); do
  ( timeout "$TMO" qemu-aarch64-static "$BIN" $s $e 2>&1 \
      | grep -aE "^(P:[0-9]+|CHUNK-CRASH|FILE-WEDGE)" > "$OUT".shard.$i ) &
done; wait
```

| window | IDs | what it covers | BASE | NET | CC | FW | **+gain / −loss** |
|---|---|---|---|---|---|---|---|
| W1 | 10001–10600 | `atom` `cons` `consp` `cxr` `assoc` `append` … | 1515 | 1515 | 0 / 0 | 22 / 22 | **+0 / −0** |
| W2 | 13300–14600 | numeric (#220's window, for continuity) | 2132 | 2132 | 0 / 0 | 1 / 1 | **+0 / −0** |
| W3 | 19700–20700 | `array*` `make-array` `simple-array` `adjust-array` | 2030 | 2030 | 0 / 0 | 0 / 0 | **+0 / −0** |
| W4 | 25090–25600 | `subtypep-*` `type-of` `typep` `types-and-class` | 1419 | 1419 | 0 / 0 | 1 / 1 | **+0 / −0** |
| **FULL** | **10001–27800** | **the whole corpus** | **17348** | **17348** | 1 / 1 | 30 / 30 | **+3 / −3** |

`NSH=8 TMO=900` for W1–W4; `NSH=64 TMO=900` for FULL. Wall times W1 17.5s /
17.1s; FULL 8m03s / 9m49s — both far below the 900s per-shard cap, so **no
shard was truncated on either side**, and the diff is 6 scattered IDs in 3
files rather than the contiguous runs that truncation produces.

**Cross-validation of the harness:** W2 BASE = **2132**. #220 measured exactly
**2132** for its NET on the same window with the same geometry, and `main`
contains #220. An independently rebuilt BASE reproducing that number to the
unit says the measurement setup matches the one this branch is being compared
against.

#### The FULL diff is noise — demonstrated on the BASE binary alone

```
gained: 21942 (print-floats)  22153 (print-length)  23820 (format-x)
lost:   21933 21943 21947 (all print-floats)
```

Required deterministic recheck, isolated file ranges, 3 runs per side:

```
print-floats 21911..21960   run1 BASE=1003 NET=1003
                            run2 BASE=1003 NET=1003
                            run3 BASE=1003 NET=1003
print-length 22140..22170   run1 BASE=1169 NET=1169
                            run2 BASE=1169 NET=1170
                            run3 BASE=1169 NET=1170
format-x     23800..23840   run1 BASE=1017 NET=1016
                            run2 BASE=1017 NET=1017
                            run3 BASE=1017 NET=1017
```

**No loss reproduces.** And the per-ID check settles it outright — the same
`21942`/`21943` flip happens on the **BASE** binary between its own runs:

```
run1 BASE[21933 21943 21947]  NET[21933 21942 21947]
run2 BASE[21933 21942 21947]  NET[21933 21942 21947]   <- identical
run3 BASE[21933 21942 21947]  NET[21933 21942 21947]   <- identical
```

Same binary, same range, different answer run to run. The `print-floats` band
is intrinsically non-deterministic here, independent of this change; in runs 2
and 3 BASE and NET agree exactly. So the honest reading of FULL is
**17348 = 17348, +0 / −0 reproducible, crash markers identical**.

No gains either — which is expected and worth stating plainly: §1 measured
**zero** live `atom`, `obj-set`-bad and `atomic-xchg` sites in these images, and
the single live `consp` site is NIL-guarded. A codegen fix for paths the corpus
does not currently exercise should move nothing, and it moved nothing. The
evidence that the bugs are real is §3 (constructed triggers, runtime
before/after), not the gate; the gate's job here is to show the fix costs
nothing, and it does.

---

## 7. Does x64 / i386 have the same class?

**Reported, not fixed**, per the task.

**x64 — the aarch64 mechanism does not exist; a different aliasing route does,
and it is latent.** `translate-x64.lisp` has **no `ensure-src`**. There is no
helper that returns "the vreg's own register, or the scratch if spilled";
sources are always *materialised into a caller-chosen register* via
`emit-load-vreg buf vreg PHYS-DEST`, so the codegen always knows it wrote
there. Two hazards remain, from a different door:

1. `*vreg-to-x64*` maps `VR` (vreg 16) to **RAX**, and RAX *is* `+scratch-reg+`.
   So `(vreg-phys vs)` returns the scratch whenever `vs == VR`.
2. In the `dest-then-op` arms (`emit-alu-rrr`, `+op-mul+`, `+op-mul26*+`, the
   checked-arith fast paths), step 1 overwrites `d` — so `vb == vd` yields
   `va OP va`.

The cleanest concrete instance is `+op-sap-new+` (`translate-x64.lisp:3264`):
`emit-mov-reg-imm buf +scratch-reg+ #x116` writes RAX with the header *before*
the `unless pa` guard reads the address; with `vaddr == VR` the SAP would wrap
address `0x116`. Both emitters hardcode `+vreg-v0+`, so it is unreachable.
Similar latent shapes exist in `+op-cons+`, `+op-setcar+`/`+op-setcdr+`,
`+op-store+`, `+op-aset+`/`+op-aref+`, `+op-u8-ref+`/`+op-u8-set+`,
`+op-atomic-xchg+`, `+op-acc128+`, `emit-alu-rrr`, `+op-shlv+`/`+op-sarv+`.

Reachability was **measured, not assumed**: an instrumented `emit-ir` over
**2,400 real top-level forms** from 17 first-party files (the whole CL runtime,
`compiler.lisp`, and the full net stack including `actors.lisp` — the only user
of `atomic-xchg`) scanned every emitted IR instruction for all thirteen
hazardous operand shapes. **All are zero**, with one near-miss:
`(:cons VR temp VR)` fires 978 times from `compile-quote`'s >4-element
quoted-list loop, but `temp` is always **V4**, which has a physical register, so
the dangerous arm is never taken. It is one allocator-pressure step from live.

Notably the direct x64 analogue of the aarch64 `+op-mod+` bug **does not
exist** — `+op-div+`/`+op-mod+` stack-mediate both operands unconditionally,
with a comment naming this exact hazard. `+op-consp+`/`+op-atom+` are likewise
safe: they `emit-load-vreg buf vs d` (a *copy*) and destroy `d`, never the
source's home. And neither x64 nor i386 has the large-offset hazard at all —
both encode disp32 directly, so nothing needs staging.

**x64 `+op-call-ind+` DOES mutate its source, structurally identically to
aarch64** (`translate-x64.lisp:3161`): `emit-sub-reg-imm buf call-reg 3` is the
destructive two-operand form, and when `vs` maps to a physical register the
`-3` is permanent. The V4 case is the sharp one — RBX is callee-saved and
deliberately excluded from the caller-save loop, so the callee faithfully
restores the *already-decremented* value. It is dead in practice for the same
reason as on aarch64 (all six emitters pass a fresh `alloc-temp-reg` result,
always above the saved range and freed after the join with no intervening
read), but nothing *enforces* that discipline.

**i386 — no.** It *has* the exact `ensure-src` analogue,
`i386-vreg-or-scratch` (`translate-i386.lisp:1377`), which returns `scratch`
for a spilled vreg — but it is **dead code: zero call sites repo-wide**. Every
arm materialises via `i386-load-vreg`. Scratches are ECX/EDX, neither in
`*i386-vreg-map*`. The only aliasing surface is again EAX==VR, and the file
already mechanises a checker for it (`i386-check-eax-write`). i386's
`+op-call-ind+` does `SUB reg, 3` on a *copy* in EAX, so no bug.

Two adjacent x64 findings, flagged as **unconfirmed**: `+op-mul64lo+` is
missing the `(unless (= vd 6) …)` RDX guard its sibling `+op-mul64hi+` has (an
asymmetry that looks like an oversight, though no first-party source calls
either); and `+op-io-read+`/`+op-io-write+` plus the float arms clobber EDX (V6)
and RCX (V5) without saving — whether a live V5/V6 can span those opcodes was
not verified.

**Coverage of the x64/i386 audit.** All 110 `(op= +op-…)` branches in x64's
`translate-instruction` were enumerated by line, with the ~50 multi-operand /
fixed-register ones read in full, plus every shared helper. For i386, 96 of 96
`cond` clauses and 27 of 27 `+op-trap+` sub-arms were enumerated; the specific
i386 line-level findings were **not** independently re-verified by me. Not
checked at all: the x64 GC trampoline / MCGC helper bodies beyond confirming
they are push/pop-bracketed, and the ARM32/PPC/68k/RISC-V translators. No x64
binary was built and no x64 test run as part of that audit — the byte-identity
result in §6.1 is separate and is a real measurement.

---

## 8. What this does and does not claim

- **Does**: `+op-consp+`, `+op-atom+` and `+op-obj-set+` are genuinely wrong on
  aarch64; each is reproduced at runtime with controls; all three are fixed;
  x64 is provably untouched; three further sites in the same class
  (`atomic-xchg`, `call-ind`, `percpu-set`) are fixed as **unverified
  hardening**; the other 96 `ensure-src` sites are safe with a stated reason
  each.
- **Does not**: claim any of the three verified bugs currently fires in a
  shipping image — the one live `consp` site in the CLI is guarded by a
  preceding `(null result)` test, and `atom`/`obj-set`/`atomic-xchg` have zero
  live sites there. The triggers are **constructed**. What is measured in the
  wild is the *codegen*, not a user-visible failure. Nor does it claim the
  three hardening fixes caught anything — I could not construct a live trigger
  for any of them, and say so at every mention.
