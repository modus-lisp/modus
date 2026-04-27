# Layout-fragility: the part we can't yet root-cause

## Setting

Modus is a self-hosting Lisp where the MVM compiler emits native x86-64.
The ANSI test suite runs ~17,700 tests through a per-file fork harness.
A "layout-flip fuzzer" (`scripts/fragility-fuzzer.sh`) builds the same
binary 9 times with N ∈ {0..8} extra `:nop` IR ops injected at every
`compile-funcall` site.  Each NOP is one byte and there are thousands
of funcall sites, so each increment of N shifts the binary by a few KB.

Most of the suite is layout-stable.  But ~14 tests flip pass/fail
across N values **non-monotonically** (e.g. N=0 pass, N=1 fail, N=2
pass, N=3 pass, N=4 fail, …).  This is the signature of an
*alignment-modulo* issue, not a generic "code grew, something broke".

## What we have rooted out

### 1. Funcall-dispatch tag-collision (commit 40faf94)

`compile-funcall`'s closure-aware dispatch checks `obj-tag(x) = 9`
to decide whether `x` is a heap closure or a raw fn-addr.  When a fn
entry happened to land at vaddr `…?9` the dispatch dereffed
`[fn-addr - 9]` looking for a closure header and (sometimes) found a
matching subtag → SIGSEGV.  Fix: NOP-align function entries away from
low-nibble 1 (cons-tag) and 9 (object-tag) at translation time.

### 2. functionp / characterp process-of-elimination (commit 21ae4eb)

`functionp` was a process-of-elimination cond:

```
((null x) nil) ((eq x t) nil) ((consp x) nil)
((characterp x) nil)         ; ← the trap
((stringp x) nil) ...
((and (integerp x) (in-code-range x)) t)
((and (integerp x) (< x #x100000)) nil)
(t t)
```

`compile-characterp` checks **low byte** == `+char-tag+` (#x05).
The MVM character encoding is `(code << 8) | char-tag`, so byte 1
carries the low byte of the char-code (NOT a separate `imm-char`
subtype field as some docs suggested).  We can't tighten characterp
to check 16 bits — that rejects every non-`#\Null` real character.

A raw fn-addr at vaddr `…???05` therefore passes characterp.
Compounding: low byte 0x05 has low bit 1, so the existing
`(and (integerp x) (in-code-range x))` arm — gated on integerp,
which requires low bit 0 — never fires for those fn-addrs either.
At certain N values `#'IDENTITY` lands at a `…???05` address and
`(functionp #'identity)` returns NIL.  Tests 12252/12276/12281
flipped exactly at those N values.

Fix: move the `[code_base, code_end)` range check ahead of the
characterp arm and drop the `integerp` gate.  Range check is
layout-independent for fn-addrs regardless of low-bit pattern.

After this fix, 12252/12276/12281 pass at every N we've tried.

## What we can't root-cause: the CLOS family

The remaining 11 layout-fragile tests are all CLOS:

```
26949    CHANGE-CLASS.4.5
27084    CLASS-0206.1
27465    MAKE-INSTANCE.ORDER.1
27484    MAKE-LOAD-FORM.13
27509-12 REINITIALIZE-INSTANCE.{1-4}
27534    SHARED-INITIALIZE.1.2
27551    SHARED-INITIALIZE.4.2
27561    SHARED-INITIALIZE.6.1
```

The simplest is REINITIALIZE-INSTANCE.1:

```
(deftest reinitialize-instance.1
  (let* ((obj (make-instance 'class-01))
         (obj2 (reinitialize-instance obj)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))
```

`class-01` is defined in `defclass-01.lsp`:
`(defclass class-01 () (s1 s2 s3))`.

### Probes that came back negative

We added end-of-kernel-main probes (placed in the LAST defun in source
order so any layout shift is local to kernel-main itself, leaving
earlier function addresses unchanged):

1. **Same-function intern eq**: bind `'%clos-fragility-probe` to `s1`,
   force a million conses to trigger several GCs, intern again to `s2`,
   compare.  Prints `DIAG-INTERN-EQ: Y`.  Intern is consistent across GC.

2. **Cross-function intern eq**: define two helpers
   `(defun %fragprobe-a () '%xfn-probe-sym)` and
   `(defun %fragprobe-b () '%xfn-probe-sym)`, eq their results.
   Prints `DIAG-XFN-EQ: Y`.  ansi-tests.lisp's "cross-function symbol
   eq is known-broken" comment is stale.

3. **Parent class registration**: probe `(%find-clos-class 'class-01)`
   in the parent before forks.  By default it prints
   `DIAG-CLASS-01-FOUND: N` because the harness emits each file's
   `defclass`/`defmethod` calls only into that file's `run-ansi-X`
   function, which runs *inside* the per-file fork (parent's
   `*clos-classes*` stays empty).  We changed the harness to
   pre-run all `defclass-*` files' init forms in the parent (commit
   pending) — probe now prints `DIAG-CLASS-01-FOUND: Y`.

4. **GC stale-table hypothesis**: tried re-reading `#x10000088` after
   `%make-symbol` in `%intern-symbol`.  Correct in theory (would
   close the window where local `table` could be from-space if the
   GC stack scan missed it), but the one extra mem-ref shifts the
   binary enough to net-regress 159 ANSI tests in unrelated
   `AREF.*` / `ARRAY.*` ranges.  Reverted; documented in source.

### What remains mysterious

Even after the parent-init harness change registers class-01 visibly
in the parent before forks, the CLOS family tests **still fail at
some N values and pass at others**.  So the failure is not "class-01
is missing"; the test is reaching `make-instance` with class-01 in
`*clos-classes*` and something downstream is layout-fragile.

The cascade hypothesis (`make-instance` returns NIL → cascade through
NIL slots happens to match `(t (nil nil nil))` expected) was the
prior explanation for *passes*.  But with class-01 actually
registered, `make-instance` returns a real instance, the slots are
unbound (-999 sentinel), and `slot-boundp` should still return NIL
for each — also matching.  Yet the test fails.

What we don't have:
- A reproducible per-test trace of why 27509 returns wrong values
  at N=0 with the functionp fix but right values at N=1 with the
  same fix.
- Any in-process instrumentation of `%make-instance` /
  `%slot-boundp` / `%clos-instance-p` that doesn't itself shift
  layout enough to mask the bug ("Heisenberg").

What would help:
- An external observer (gdb on the running binary) that catches the
  failure path live without modifying the binary.  Hardest part is
  identifying *which* fork (one per test file) is the failing one
  from outside, since they exec the same image.
- A dual-binary disassembly diff: build at N=0 (failing) and N=1
  (passing), find the compiled body of `run-tests-reinitialize-instance`
  in each, and compare the emitted x64 for the test thunk —
  specifically the make-instance / map-slot-boundp* calls — to spot
  what differs.  Tedious but not Heisenberg-prone.

### A note on "non-monotonic" flips

Pure code-grew-something-broke would give monotonic flips: pass
counts decrease with N and stay decreased.  We see N=0 and N=7
both pass, N=1 fails, N=2 passes again — the shape that says
*"the bug fires when something lands at a particular address mod K"*.
We've fixed two such "particular addresses" (low-nibble 1, 9, and
low-byte 0x05 via the functionp fix).  The CLOS family's bad
address is something else.

Likely candidates for "what bit pattern breaks at certain layouts":
- An `eq` comparison whose two sides happen to bit-pattern-coincide
  at certain layouts.  The marker symbols `'%clos-instance` and
  `'%generic-function` live at addresses determined by allocation
  order; layout shift can put them at addresses that some other
  field happens to hold.
- A heap-relative offset that wraps modulo something.
- A function-table lookup hashing a tagged value where one specific
  layout produces a hash collision with a different name.

But none of these are confirmed.  The probes show intern, class
registration, and the basic comparators all work in isolation.
The bug is in the interaction with whatever layout shift puts in
the wrong place.

## Where to start fresh

Progress against the open question came from reframing it:
**stop chasing CLOS, start auditing every "low byte/nibble equals K"
predicate the same way we audited characterp.**

That found two more root causes of the same shape:

### Root cause #3: `:obj-subtag` IR-op tag-unsafe (commit 9a11f24)

The x64 translator emitted a bare `mov d, [src - 9]' with no check
that `src' was actually a tag-9 heap pointer.  Two crashes possible:

  1. src low nibble != 9 (fixnum/cons/immediate/forward) — random
     offset, often unmapped → SIGSEGV.
  2. src IS T (= +t-value+ = #xDEAD1009).  Low nibble 9 looks like
     a heap pointer, but T is an immediate and [T-9] = #xDEAD1000
     is one byte past the 4KB NIL-page mmap → SIGSEGV.

Class (2) was the path that surfaced via rt-equal → rt-floatp(T)
whenever a test returned T but expected something non-T:

    rt-equal:
      (eql a b) → NIL                       ; a=T, b=other
      ((or (consp a) (consp b)) NIL)        ; falls through
      ((and (rt-floatp a) (rt-floatp b)) …) ; calls (rt-floatp T)
        rt-floatp guards: fixnump/consp/null all NIL for T, then
        (= (obj-subtag T) 96) → deref [T-9] → SIGSEGV.

Same shape as characterp/functionp: a process-of-elimination
predicate validates with insufficient guards and a non-matching
immediate slips through to a deref.  cl-types' cos/sin/exp/cosh
and integerp's bignum-check all had identical shape.

Fix: tag-check at the IR-op level, returning subtag 0 on mismatch.

### Root cause #4: `:array-len` IR-op tag-unsafe (commit f7fa46c)

Same shape as obj-subtag.  CLOS predicates do
`(if (= (obj-subtag x) #x32) (if (>= (array-length x) 1) ...))';
the obj-subtag fix makes the first arm safely return NIL for T,
but `%gf-p' / similar reach `array-length' on T → crash.

Mirror fix applied.

### Empirical results

  Baseline           : N=0 9169 unique / N=1 9156 unique
  + functionp fix    : N=0 9159 unique / N=1 9167 unique
  + obj-subtag fix   : N=0 9327 unique / N=1 9317 unique
  + array-len fix    : N=0 9386 unique / N=1 9395 unique
  + functionp mask   : N=0 9395 unique / N=1 9386 unique

Cumulative gain: **+226 unique tests at N=0, +230 at N=1**.

### SIGSEGV handler instrumentation (commit 48c1b36)

To attack the residual, the SIGSEGV handler stub now captures
ucontext state into fixed slots at the moment of fault, before
the longjmp clobbers it.  Six values:

  0x10000C30 — saved RIP   (ucontext+0xA8 = uc_mcontext.gregs[16])
  0x10000C38 — saved RSP   (ucontext+0xA0 = gregs[15])
  0x10000C40 — [saved RSP] (return addr — byte after failing call)
  0x10000C48 — saved RAX   (ucontext+0x90 = gregs[13])
  0x10000C50 — si_addr     (siginfo+16 — the bad memory address)
  0x10000C58 — ucontext ptr itself (RDX, for verification)

Required adding **SA_SIGINFO** to the sigaction flags — without it
the kernel doesn't populate RDX with the ucontext pointer on
handler entry, and our ucontext-relative reads were reading
garbage from wherever RDX happened to point.  (The earlier
"RIP=1053720 across all fails" output was meaningless — it was
some constant that happened to be in RDX-relative memory.)

%record-test-fail prints all five (output values are raw/4 — divide
by 4 to recover real address; low 2 bits lost in the sar).

### Three residual bug classes confirmed

1. **CHANGE-CLASS no-op stub** (ansi-bridge.lisp:1352-54).
   `(defun change-class (instance new-class &rest initargs) instance)`.
   Tests like CLASS-0203.2 (ID 27081) fail because change-class
   returns the instance unchanged — slot values aren't transferred
   to a new layout, and the test's expected `(T NIL NIL NIL)` slot
   pattern (slots set to T by initforms in the new class) shows up
   as `(NIL NIL NIL NIL)` (initforms never ran).  This is
   *unimplemented feature*, not a same-shape bug.

2. **Cross-function intern non-determinism (RULED OUT
   empirically).**  Probe at `%specializer-matches-p` (cl-clos.lisp,
   gated to fire only when two symbols' eq-mismatch coincides
   with a name-hash match) ran a full ANSI suite without firing
   once.  The collaborator's hypothesis that dispatch's
   eq-on-class-name was the call-NIL source is therefore not
   the cause.

3. **The same-shape sixth bug: `:car` / `:cdr` IR-ops are
   tag-unsafe.**  After the eq-collision probe came up empty, the
   captured-state SIGSEGV signatures decoded against the binary's
   disasm.  Three distinct fault patterns dominate:

      Sig 1 (72 hits) at RIP 0x41812C: `mov rax, [rax+7]` = `(cdr X)`.
        RAX = 0 at fault, si_addr = 7 → reading [+7] off NULL.
      Sig 3 (31 hits) at RIP 0x462134: `mov rax, [rax-1]` = `(car X)`.
        RAX = 0, si_addr = 0xFFFFFFFFFFFFFFFE → reading [-1] off NULL.
      Sig 2 (36 hits) at RIP 0x405060 (middle of a movabs in some
        array-init function — the same-shape pattern but on a
        different opcode; needs more investigation).

   All three are deref-without-tag-check, the same family as the
   five already-fixed bugs.  car/cdr translate to `mov d, [src ± 1/7]`
   with no validation that `src` is a real cons (low nibble 1).
   For NIL = 0xDEAD0001 they happen to work because the NIL-page
   mmap absorbs the read; for raw 0 (uninitialized slots, integer
   values mistakenly chased as cons) the read goes to address ±1
   from 0 → unmapped → SIGSEGV.

   The fix is the same shape as obj-subtag/array-len: tag-check at
   IR-op level, return NIL on tag mismatch.  Cost: ~15 bytes per
   `car`/`cdr` site, and these are everywhere — risk of significant
   layout shift.  Held until the layout-stability work makes
   per-call-site changes safe.

### What's left after this finding

The same-shape bug class is fully named.  Closing it needs:

  - **car/cdr IR-op tag-check** (this commit's hypothesis,
    implementation deferred for layout-shift reasons).
  - **Implement CHANGE-CLASS** (separate feature work, not
    fragility).

A fresh fragility-fuzzer N=0..8 sweep after the car/cdr fix would
empirically validate "the family is closed" or surface a seventh.

### What's left

Pin down (1) by reading the captured state from a failing run.  The
collaborator's classification rubric:

  - if `si_addr` falls inside the NIL-page (`0xDEAD0000..0xDEAD1000`),
    it's instruction-fetch; saved RIP equals si_addr, and `[saved-RSP]`
    is just past the failing call in the caller — that byte is what
    to disasm.
  - if `si_addr` is elsewhere, it's a data fault; saved RIP is the
    load/store and si_addr is the bad effective address.

Implementing CHANGE-CLASS properly would close (2) and remove
~5 of the residual flippy tests from the count.  The unification
between (1) and (2) the collaborator suggested doesn't apply
directly — change-class doesn't even reach the slot-by-slot
transfer logic — but the dispatch path's eq-on-symbols hypothesis
is still live for the remaining call-NIL crashes.

## What's still open

The 11-test CLOS family (REINITIALIZE-INSTANCE.{1-4},
SHARED-INITIALIZE.*, CHANGE-CLASS.4.5, MAKE-LOAD-FORM.13,
CLASS-0206.1) is no longer all-or-nothing per N — but has a
**residual layout-fragility** that flips it across N.  At N=0 most
fail; at N=1 most pass.  This is *less bad* than baseline (where
N=1 was a total wipeout) but it's not solved.

The crash signature in fork at N=0 is now:

    SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_ACCERR,
             si_addr=0xdead0001}

i.e. **`call NIL`** — execution jumped to NIL's value `#xDEAD0001`,
which lies inside the NIL-page mmap (RW but not X) so the access
faults on instruction-fetch.

This says: somewhere in the test path, a function pointer is NIL.
Most likely path: `mapcar (applyf #'slot-boundp obj) slots'.
applyf returns NIL only if its etypecase matches no clause.
etypecase rewrites to typecase and typecase has no error-on-no-match
arm, so a non-symbol non-function `fn' gives NIL.

Hypothesis to investigate: at certain layouts, `#'slot-boundp` (or
similar) loads as something that fails both `(typep x 'symbol)` and
`(typep x 'function)`.  But `functionp` uses a code-bounds range
check now, which should be layout-stable for any in-segment fn-addr.

Other concrete things to audit:
  - `:obj-ref' and `:obj-set' for large idx (currently safe for
    small idx because [obj+7+8N] lands in the 4KB NIL page; for
    N >= ~510 it'd fault).
  - `&rest defun + funcall-of-let-allocated-lambda' SIGSEGV
    documented in CLAUDE.md.  reinitialize-instance has &rest.
  - `etypecase' rewrite's no-match path — should signal but
    currently silently returns NIL.  applyf depends on it.
  - Caller-saved registers V5..V8 around `:call-ind' in
    `compile-funcall' / `compile-call' — there were past
    regressions when the slow-path didn't save these.

The pattern that's been productive: **every (obj OP) intrinsic
where OP eventually derefs needs the same tag-check**.  We've done
obj-subtag and array-len.  Remaining IR-ops with deref: car/cdr,
set-car/set-cdr, obj-ref, obj-set, aref, aset.  None of these were
the immediate cause of the call-NIL crash, but they're worth
auditing to close the bug class.
