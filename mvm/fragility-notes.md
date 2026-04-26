# Bytecode-layout fragility notes (per-translator porting)

Notes for porting the x64 fragility fixes to other architectures.

## Background

`compile-funcall` (in `mvm/compiler.lisp`) checks tag bits of values to
distinguish closure objects, native MVM symbols, and raw fn-addrs.
The check uses `:obj-tag` (extracts low 4 bits) and compares against
`+tag-object+` (= 9).  When a raw native function address happens to
have low nibble = 9, the dispatch dereferences `[fn-addr - 9]` looking
for a heap-object header.  If those bytes accidentally contain
subtag #x50 (native MVM symbol) or #x52 (closure), the dispatch
routes a raw pointer through a closure code path → SIGSEGV.

A similar collision existed historically for low nibble = 1 (cons
tag).  Earlier translator versions only handled that.

## Which translators need this fix?

The vulnerability requires that *function entry points can land at
addresses with low nibble 1 or 9*.  This depends on instruction
alignment:

| Translator                | Inst align | Low-nibble range | Vulnerable? |
|---------------------------|------------|------------------|-------------|
| `translate-x64.lisp`      | 1 byte     | 0..F             | **Yes — fixed** |
| `translate-i386.lisp`     | 1 byte     | 0..F             | **Yes — TODO** |
| `translate-aarch64.lisp`  | 4 bytes    | 0, 4, 8, C       | No (ISA-protected) |
| `translate-arm32.lisp`    | 4 bytes    | 0, 4, 8, C       | No (ISA-protected) |
| `translate-riscv.lisp`    | 4 bytes    | 0, 4, 8, C       | No (ISA-protected) |
| `translate-ppc.lisp`      | 4 bytes    | 0, 4, 8, C       | No (ISA-protected) |
| `translate-68k.lisp`      | 2 bytes    | 0, 2, 4, …, E    | No (always even) |

So only **x64** and **i386** can hit the bad-nibble case.  x64 is
fixed (commit 40faf94); i386 still needs the same fix.

## The x64 fix

After every function emission, the translator NOPs forward until the
next address has a safe low nibble (not 1, not 9):

```lisp
;; in translate-x64.lisp, post-function loop
(loop
  (let* ((p (code-buffer-position buf))
         (n (logand (+ *x64-native-code-offset* p) #xF)))
    (if (or (= n 1) (= n 9))
        (emit-nop buf)
        (return))))
```

Cost: at most 2 NOP bytes per function (since `:nop` is 1 byte and
incrementing by 1 always changes the low nibble).

### Why not full 16-byte alignment?

Tested.  More NOPs = more layout shift = different tests flip via
the deeper fragility (see Open Questions below).  Net regression.
Aim only at the two confirmed-bad nibbles for now.

## The i386 port

Same pattern.  Need to find the post-function-emission point in
`translate-i386.lisp` and add the same loop, using i386's NOP
instruction (`#x90`, same as x64).

i386 builds don't load `cl-*.lisp` so the binary is much smaller and
the probability of hitting the bad nibble is lower in practice — but
the bug class still exists.  Worth fixing pre-emptively before any
i386 build pulls in more code.

## Other findings worth porting

### `:call` clobbers caller-saved physical registers

When emitting `:call` directly (outside `compile-call`'s caller-save
machinery), V5..V8 (caller-saved on x64 SysV) get wrecked across the
call.  Any outer code with live temps in that range silently
corrupts.

`compile-call` already saves V5..V(4 + save-count - 1) before its
own `:call`.  Custom dispatch sites need the same.  We added it to
`emit-arith-pair` (commit 40faf94).

For other architectures: identify the caller-saved physical registers
in their ABI, audit any direct `:call` emission outside `compile-call`.

### Comparison slow-path caller-save: tried and rejected

`compile-compare-2`'s ratio-aware slow path has the same hazard.
Adding push/pop fixed the semantics but the per-call-site size
growth shifted enough other functions into bad-bit-pattern territory
to net-regress 10 CLOS tests.  Comment in `compile-compare-2`
documents why this remains unfixed.

This is a hint that the fragility is broader than just funcall.

## Open questions / deeper fragility

After the nibble-9 alignment AND functionp fix, the 4-stubborn-tests
group (12257, 12261, 12285, 14253) reduced to 1.  Three of them
were caused by `functionp`'s integerp-heuristic exclusion (see
commit 7203e19) — raw fn-addrs with low bit 0 looked like fixnums
to integerp, so functionp returned NIL for them deterministically.

**Test 14253 still flips** on bytecode shift — likely a different
root cause (uses `(LET ((BOUND (ASH 1 200))) ...)` and bignum
arithmetic).  Probably overflow / loop issues unrelated to the
funcall/typep dispatch family.

Concrete tools to dig deeper:
1. **Layout-flip fuzzer**: build N variants with different NOP padding
   in different places.  Diff which tests flip across variants.
2. **Funcall range-check dispatch**: replace tag-bit dispatch with
   a runtime `[code_base, code_end)` range check.  Eliminates
   bit-pattern dependence entirely.  Requires storing code base/end
   in fixed memory slots at boot.
3. **Per-module namespaces**: stop "last defun wins" so layouts don't
   silently shift on minor reordering.

## Audit checklist for new translators

When adding or auditing a translator:

- [ ] Does the instruction alignment naturally avoid nibble 1 and 9?
      If yes (4-byte aligned), no fix needed.  If no (i386, 68k word),
      copy the post-function-NOP loop.
- [ ] Are there `:call` emission sites outside `compile-call`?  If
      yes, they need caller-save (push/pop the caller's live
      caller-saved-temps before/after the call).
- [ ] What are the caller-saved registers in the ABI?  V5..V8 on
      x64 SysV; different on other architectures.
- [ ] Is `:obj-subtag` reading at the same offset (`-9`) as x64?
      If the offset differs, the alignment requirement may differ
      proportionally.

## Test it

After porting a fix, run the ANSI-test build for that architecture
(e.g., AArch64) and confirm pass count is stable.  Inject a few
extra `:nop`s in `compile-add` (or another hot op) and rerun: tests
should not flip.  If they do, there's still layout-fragility on that
architecture and the fix above is incomplete.
