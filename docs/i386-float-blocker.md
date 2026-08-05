# i386 floats: a REPRESENTATION wall, not a codegen gap (#201)

Measured 2026-08-05 against `build-i386-cli` at layer 5.

## The remaining i386 gap, quantified

13 distinct translator gaps; 157 of ~165 sites are float:

```
FDIV ×57   FMUL ×31   FADD ×24   FSUB ×22   ITOF ×17   FTOI ×6
trap #x0531 ×4, #x0520/#x0532/#x0533/#x0534 ×1 each
SAP-ADDR ×1   SAP-NEW ×1
```

## Why it is NOT just "port the x64 SSE sequence"

The SSE2 *instructions* port trivially — i386 has the same `ADDSD/SUBSD/
MULSD/DIVSD` (`F2 0F 58/5C/59/5E`), the same `CVTSI2SD`/`CVTTSD2SI`, and no
REX prefix to strip.  `MOVSD xmm, [esp]` (`F2 0F 10 04 24`) replaces x64's
`MOVQ xmm, r64`, which is if anything simpler.

The wall is the **float object layout**.

x64 (`translate-x64.lisp` ~2550): a float object is subtag `#x60` with
**2 slots**, each holding one 32-bit half of the IEEE double, stored
**TAGGED** — `half << 1`:

```
slot 0 = (hi32 sign-extended) << 1
slot 1 = (lo32 zero-extended) << 1
```

The tagging is not decoration.  It is **GC safety**: a tagged half has low
bit 0, so the conservative collector reads the slot as a fixnum and never
treats the float's bit pattern as a pointer.  This is exactly the failure
CLAUDE.md records — scanning raw IEEE bit-slots as pointers corrupted a live
fn and produced `RIP=0xDEAD1004`.

On x64 a slot is a 64-bit word, so `hi32 << 1` fits with room to spare.

**On i386 a slot is a 32-bit word, so `hi32 << 1` overflows and loses bit
31 — the sign bit of the double.**  The x64 representation cannot be ported
as-is.  Any i386 float support requires choosing a representation first.

## The two real options

**(a) 4 slots × 16-bit tagged chunks.**  Each chunk `<< 1` fits a 32-bit
word with low bit 0, so the GC-safety property is preserved verbatim and the
SAME layout works on x64 and aarch64.  This is the width-neutral answer and
the one that matches #201's intent.  Cost: the layout is shared, so it
touches both translators plus every Lisp-side reader of float slots
(printer, reader, type predicates, `coerce`, contagion).

**(b) Raw untagged slots + a subtag-aware GC skip.**  Cheaper to write, but
it re-introduces exactly the class the object-start bitmap was built to
close: a raw IEEE bit pattern that happens to equal a live object start is
indistinguishable from a real pointer to a conservative scanner.  i386 has
the validating collector (#202), which makes this *mostly* safe — but
"mostly" is the wrong property for a collector.  Not recommended.

Recommendation: (a).  It is more work, but it is the only option that leaves
one representation across all three arches, which is the point of #201.

## Separately: a subtag assignment worth checking

`translate-x64.lisp` hardcodes subtag `#x60` for float objects
(`(emit-mov-reg-imm buf 'rcx #x260)` — `(count 2) << 8 | #x60`).

`runtime/tags.lisp` declares:

```lisp
;;; 0x60-0x6F: MVM objects
(defconstant +subtag-mvm-bytecode+ #x60)
(defconstant +subtag-mvm-module+   #x61)
```

There is **no float subtag defined in `runtime/tags.lisp` at all**.  So the
float object's `#x60` collides with `+subtag-mvm-bytecode+` in the declared
table, and `#x61` is the value that already caused one documented
corruption incident (single-float vs `+subtag-mvm-module+`, in CLAUDE.md).

This is a FLAG, not a confirmed live bug: whether it bites depends on
whether anything allocates an object with `+subtag-mvm-bytecode+` and is
then scanned alongside floats.  It should be resolved — by assigning floats
a real, reserved subtag in `runtime/tags.lisp` — before (a) is implemented,
since (a) rewrites that allocation site anyway.

## Non-float gaps

`SAP-NEW`/`SAP-ADDR` (2 sites) and the 5 traps (8 sites) are independent of
the representation question and can be closed separately.

## x64 VERIFIED CORRECT (2026-08-05) — the base for (a) is sound

Reviewed and then measured, on a CLI built from current main.

**Code review.** The read path (`sar 1; shl 32` for hi, `sar 1; shl 32;
shr 32` for lo) exactly inverts the write path (`sar 32; shl 1` for hi,
`shl 32; shr 32; shl 1` for lo), so the 64 IEEE bits round-trip losslessly.
Both slots keep low bit 0, preserving the GC-safety property.  `:gc-check`
IS emitted before every allocating float op (`:fadd/:fsub/:fmul/:fdiv/
:itof`) in compiler.lisp, and correctly omitted for `:ftoi`, which does not
allocate — so the missing-gc-check class does not apply here.

**Measured** (all 10 correct):

```
(+ 1.5d0 2.25d0)            => 3.75d0
(- 0.0d0 5.5d0)             => -5.5d0
(* -2.5d0 4.0d0)            => -10.0d0
(/ 1.0d0 4.0d0)             => 0.25d0
(- 1.0d0 3.0d0)             => -2.0d0
(* -1.0d0 (/ 1.0d0 3.0d0))  => -0.3333333333333333d0
(float 7 1.0d0)             => 7.0d0
(truncate 3.99d0)           => 3
(truncate -3.99d0)          => -3
(< 1.0 2.0) (> -1.0 -2.0) (= 1.5 1.5) => T T T
```

The negative and full-mantissa cases are the important ones: they drive the
`hi32` SIGN BIT through the tagged-halves representation — exactly the bit
that overflows a 32-bit slot on i386.  x64 handles it correctly, which
confirms the wall is width, not logic.

## Subtag question: RESOLVED, no live collision

Retracting the earlier flag.  `compiler.lisp:71` defines
`+subtag-float+ = #x60` with the decision recorded in-source: "#x60 stays
double-float (preserves the boot image and every existing literal)", and
single/short/long were deliberately placed at `#x64..#x66` *because* `#x61`
is `+subtag-mvm-module+` — the documented `RIP=0xDEAD1004` incident.

`+subtag-mvm-bytecode+` (`runtime/tags.lisp:54`) is referenced NOWHERE but
its own definition — nothing allocates or reads it.  So there is no live
collision; `#x60` belongs to floats in practice and by intent.
`runtime/tags.lisp` is simply stale and should gain a `+subtag-float+`
entry (and lose or reserve the unused mvm-bytecode name) as bookkeeping.

## Residual x64 gap (separate from (a))

`:ftoi` has no overflow check: `CVTTSD2SI` returns the integer-indefinite
value `0x8000000000000000` when the double exceeds the target range, and the
subsequent `shl rax, 1` turns that into 0 — a silent wrong answer rather
than a bignum or an error.  Real conformance gap, independent of the
representation change.
