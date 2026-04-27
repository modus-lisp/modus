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

## Per-call-site code growth as its own fragility class

Several attempted fixes failed not because they were semantically wrong
but because they grew the bytecode at every call site, shifting addresses
elsewhere into bad-bit-pattern territory.  Confirmed cases:

- compile-compare-2 caller-save (commit 39a414d, comment in source):
  push/pop V5..V8 across the slow-path :call.  Correct semantically;
  regressed 10 CLOS tests via per-comparison size growth.
- compile-1+/1- ratio dispatch (commit 41b1434): rerouting through
  (+ x 1) so ratios get incremented properly.  Regressed 2 tests.
- compile-* ratio dispatch (commit 40faf94 contains the working +/-
  variant; * not enabled): ~85 tests over SIGALRM budget because
  multiply hits tight inner loops.
- compile-funcall code-bounds range check (commit 28ceb52): 8 IR ops
  before every funcall; regressed 33 tests.

Hypothesis: each per-call-site addition shifts function offsets by a
small amount, but compounded over the call density of a hot operator
(thousands of call sites in a real binary), the cumulative shift is
enough to flip tests via the still-not-fully-rooted-out underlying
fragility (whatever's left after our two fixed root causes).

What works without per-call-site growth:
- Translator alignment fixes (NOP padding, applied per-function not
  per-call-site).
- Predicate body fixes (functionp, vectorp etc. — applied to the
  defun once, not per call).
- Boot-stub init code (one-time at boot).

What doesn't work yet:
- Anything inserted at every call site of a hot intrinsic.

Likely-needed prerequisite: solve the residual "what gets flipped by
small-uniform shifts" mystery, then per-call-site additions become safe.

## Layout-flip fuzzer findings (2026-04-27)

`scripts/fragility-fuzzer.sh` builds the ANSI-test binary with N
extra `:nop` IR ops injected at every compile-funcall site, for
N ∈ {0..8}, and diffs which tests flip across builds.  Each NOP
is 1 byte, and there are thousands of funcall sites, so each
increment of N shifts the binary by thousands of bytes.

Results:
```
  N=0  passed=9573  failed=9573
  N=1  passed=9559  failed=9587
  N=2  passed=9562  failed=9584
  N=3  passed=9571  failed=9575
  N=4  passed=9562  failed=9584
  N=5  passed=9571  failed=9575
  N=6  passed=9571  failed=9575
  N=7  passed=9573  failed=9573
  N=8  passed=9573  failed=9573

  Stable across all N: 9156 tests
  Flippy: 14 tests
```

**Key observations:**

1. **Non-monotonic.**  N=0 and N=7,8 both score 9573.  The pass count
   doesn't drop and stay dropped — it dips and recovers.  This is the
   signature of an alignment-modulo issue (something needs to be at a
   particular address mod K), not a generic "code grew, something
   broke" issue.

2. **Only 14 tests are flippy.**  9156 are stable across all N — most
   of the suite is layout-insensitive.

3. **All 14 flippy tests are CLOS:**

   ```
   12252  function          (TYPEP #'IDENTITY 'FUNCTION)
   12276  functionp         (FUNCTIONP #'IDENTITY)
   12281  functionp         (FUNCTIONP ...)
   26949  change-class      CHANGE-CLASS-CLASS-04B
   27084  defclass-02       (defclass with metaclass)
   27465  make-instance
   27484  make-load-form
   27509-27512  reinitialize-instance
   27534, 27551, 27561  shared-initialize
   ```

   The 12252/12276/12281 group is `(typep / functionp #'IDENTITY)`
   — they fail only at N=1, suggesting the lookup path encounters
   something that's at a layout-specific bad address only at that
   shift.

   The 26949/27484/27509-27561 group fails at N ∈ {1, 2, 4} but
   passes at N ∈ {0, 3, 5, 6, 7, 8}.  These all involve CLOS
   instance manipulation (slot access, generic dispatch, metaclass
   protocol).

   27084/27465 pass only at N ∈ {0, 7, 8} — most restrictive.
   Both are CLOS metaprotocol tests.

4. **At our default (N=0) we already score 9573.**  We're "lucky"
   on the current layout — all 14 flippy tests happen to pass.
   Any per-call-site code growth (like the ratio-aware `*` we
   tried earlier) shifts addresses and flips these tests.

5. **The mechanism is CLOS-specific.**  CLOS in `mvm/cl-clos.lisp`
   uses a few patterns that could be layout-sensitive:
     - `eq` against marker symbols (`'%clos-instance`,
       `'%generic-function`) stored in slot 0 of objects.
     - `obj-subtag` / `array-length` checks on objects to
       distinguish GFs / methods / instances.
     - generic-function dispatch via assoc on method tables.

   The non-monotonic flip pattern strongly suggests `eq` collisions:
   at certain layouts, two different things have the same bit
   pattern (e.g. an `aref instance 0` lookup happens to return
   bytes that match `'%generic-function`'s value).  Adding NOPs
   shifts addresses and the coincidence comes and goes.

**Next step (if we keep digging):** instrument cl-clos.lisp's
`%clos-instance-p` / `%gf-p` to print the values they're comparing
against, run with N=1 (failing) and N=0 (passing), diff to see
which `eq` returns differently.  That should pinpoint the exact
collision.

**Heisenberg note (commit 9530b1b):** any in-process instrumentation
of the predicates (write-char-serial, print-dec etc.) adds bytes to
the predicate's compiled body, multiplying across thousands of call
sites and shifting the binary far enough to change the failure mode.
The instrumentation literally moves the bug.

**Strace observations (commit 9530b1b):** running the N=1 binary
under `strace -e signal,rt_sigaction` shows the binary takes
*hundreds* of SIGSEGVs during a normal run, all caught by the
sigaction handler stub and longjmp'd back to the nearest
handler-case.  The fault addresses cluster in two ranges:
  - 0xdead0001 / 0xdead1009 / 0xdead3ca2: NIL/T/some-immediate
    being dereffed (expected — guarded car/cdr-on-nil path)
  - 0x7c259b000xxx, 0xffff83da7e...: high-userspace addresses
    that look like mmapped heap regions or stack values being
    treated as pointers.  These appear in chunks per test,
    suggesting the handler-case recovery path itself sometimes
    re-faults a few times before settling.

The test-12252 fault wasn't directly observed (strace timed out
before reaching it), so the precise mechanism for these 14 CLOS
tests is still open.  External observation via gdb-on-binary or
dual-binary disassembly diff is what's left.

## Open questions / deeper fragility

After the nibble-9 alignment AND functionp fix, the 4-stubborn-tests
group (12257, 12261, 12285, 14253) reduced to 1.  Three of them
were caused by `functionp`'s integerp-heuristic exclusion (see
commit 7203e19) — raw fn-addrs with low bit 0 looked like fixnums
to integerp, so functionp returned NIL for them deterministically.

**Test 14253 has a different root cause:** it uses `(1+ R)` and
`(+ R 1)` on a ratio R.  Today's `compile-1+` emits `:inc` (raw
pointer-bump), which gives garbage for ratio operands; `compile-add`
dispatches properly via emit-arith-pair so `(+ R 1)` works.  The
test compares `(1+ R)` to `(+ R 1)` with EQL — they diverge → test
fails.  The fix (route compile-1+ through `(+ x 1)`) regresses 2
other tests via the same per-call-site-growth fragility we've
documented; commit 41b1434 reverts it with a forward-pointing
comment.

## The "process-of-elimination predicate" fragility class

Multiple predicates were implemented as negation chains:

    (defun foo-p (x) (and (not (null x)) (not (integerp x)) ...))

These all have two failure modes:
1. **Wrong for any heap object that survived elimination** —
   closures, bignums, ratios, packages, generic functions etc.
   were classified as "foo-p" by accident.
2. **Layout-fragile for fn-addrs** — fn-addrs always have low
   bit 0 after the nibble-9 alignment fix (their low nibble is
   restricted to {0, 2, 3, 4, 5, 6, 7, 8, A, B, C, D, E, F});
   `(integerp fn-addr)` returns T for the 8 even nibbles, so the
   chain short-circuits.  Before the alignment fix, fn-addrs
   landed on odd nibbles ~36% of the time and were classified
   as "foo-p" by accident.

Audit of this class so far:
  ✓ functionp (commit 7203e19) — replaced with positive-list cond
  ✓ vectorp (commit fd27d1c) — replaced with `(or (arrayp x) (stringp x))`
  ✓ typep 'symbol branch (commit 1acaf80) — replaced with `(symbolp x)`
  ✓ typep 'bit branch (commit 1acaf80) — added integerp guard
  ✓ symbolp — has heuristic but uses obj-subtag check at end (safe)
  ✓ %clos-instance-p, %gf-p, %standard-method-p, %condition-p,
    floatp-impl — all use heuristic as a guard before precise
    obj-subtag check (safe)
  ✓ %pkg-p, streamp — use cons-marker pattern (safe)
  ? trig stubs (sin/cos/tan/etc.) — still heuristic + obj-subtag
    check; benign because all stubs return 0 or a placeholder float.

## Dead-code dispatch entries (commit 34341e0)

Audit of compile-form's intrinsic-dispatch cond (175 unique hashes
across 176 entries) found one duplicate:
MULTIPLE-VALUE-BIND was dispatched at lines 1624 and 1716 — the
first delegated to compile-multiple-value-bind helper, the second
inline expansion was dead code (unreachable due to cond ordering).
Removed the dead clause; replaced with a forward pointer comment.

This is its own fragility class: dead dispatch entries silently
fail to take effect when someone tries to fix the (unreachable)
case, leading them to conclude the compiler is broken in some
other way.

## Hash-collision audit results

Run via `compute-name-hash` over all 1133 user defuns + the 175
intrinsic-dispatch hashes:

  - **No collisions among user defuns** (60-bit dual-FNV-1a is
    collision-resistant at this scale).
  - **20 user-defun ↔ intrinsic collisions** (consp / integerp /
    + / - / * / / / mod / values / values-list / not / null /
    listp / symbolp / stringp / zerop / logand / logior / logxor /
    nth-value / set-car / set-cdr / code-char / ldb / ratiop) —
    all INTENTIONAL.  The user defuns are thin wrappers whose
    body recurses to the same name; compile-form re-dispatches
    on the body call, so the compiled function is just the inline
    IR.  Required so `#'consp` etc. work via funcall.

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
