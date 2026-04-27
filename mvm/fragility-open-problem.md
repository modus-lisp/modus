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

The functionp fix landed (commit 21ae4eb) and is layout-stable.

The harness now (commit pending) pre-runs `defclass-*` init forms
in the parent so cross-file class references are real instead of
relying on the NIL-cascade.  This is structurally correct but
doesn't on its own resolve the CLOS family.

The next concrete step is **dual-binary disassembly diff**: build at
N=0 (where 27509 fails after our fixes) and N=1 (where it passes),
locate the compiled body of `run-tests-reinitialize-instance`, and
compare the bytes around the test thunk for test 27509.  That will
either reveal a register-allocation / branch-displacement difference
that points at the layout-fragile path, or it will show byte-for-byte
identical code in which case the fragility is in shared code reached
via the test (most likely `%make-instance`, `%slot-boundp`, or
`map-slot-boundp*`).

The harness changes for parent-init are in commits to come; the
functionp fix is at 21ae4eb.

