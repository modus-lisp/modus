# ANSI test notes — session log

State as of last session: **8907 passes / 9197 fails / 0 lost**
(was 7048 / 9429 / 1215 at session start). Net
**+1859 passes / −232 fails / −1215 lost** across eleven commits:

- Paren-bug fix in `%format-impl`: +23 passes, -111 lost
- `extended-char.3` fork-crash workaround: +97 passes, -133 lost
  (now subsumed by generic shm fork recovery — workaround removed)
- Package-system unlock (MAKE-PACKAGE/FIND-PACKAGE/EXPORT/etc. no
  longer compile to no-ops): +65 passes, -150 lost
- Shared-memory parent-child fork recovery (`%mmap-shared-page`
  + re-fork loop in fork-file): recovered ~820 previously-lost
  tests.
- **Lost → 0**: fork-file now passes chunk last-id in, bumps
  retry-cap to 256, adds no-progress-cap=4 to bail on init-crashes
  without burning alarm budget, stamps all remaining chunk IDs as
  FAIL when giving up, and — critically — detects "child exited
  cleanly but ran zero tests" (silent thunk no-op from broken
  TYPECASE/PPRINT compilation) and stamps those too. Recovered
  14 chunks (print-array, format-t, typecase, etypecase, ctypecase,
  pprint-tab, pprint-newline, pprint-tabular, print-unreadable-object,
  pprint, prin1, princ, prin1-to-string, princ-to-string — 221 tests)
  plus the partial-loss chunks (loop13/loop6/adjust-array/elt/etc.).
- **cl-symbols crash fix**: 978 cl-symbols.lsp tests were crashing
  because `(find-symbol str 'common-lisp)` passed a *native* MVM
  symbol (subtag 0x50, 1 slot = hash) as the package designator.
  `%cl-sym-p` returned T (only checked subtag), then
  `%pkg-string-designator` called `(aref x 2)` on a 1-slot object →
  garbage from adjacent heap → crash. Fix: `%cl-sym-p` now also
  checks element-count >= 3 (CL syms have 3 slots [hash, package,
  name]); `%resolve-package` recognises native MVM syms via a new
  `%native-mvm-sym-p` helper and resolves them by hashing each
  candidate package's name with the runtime `compute-name-hash`.
  +970 passes, −995 crashes.

The three "lost" IDs reported by range-based analysis (12372, 24909,
24910) are phantom — the build counter advances past IDs that never
get a deftest emitted, so they exist in `/tmp/ansi-file-ranges.txt`
but there's no corresponding test to run.

Later in the session:

- **substitute-if{,-not}, nsubstitute-if-not list path inlined** to
  bypass MVM's lost-capture across the apply+closure chain. +51 passes.

- **funcall on a native MVM symbol now actually dispatches.** Added a
  hash-keyed parallel `*native-sym-function-table*` mirrored from
  `*symbol-function-table*` at boot, plus a `compile-funcall` branch
  that detects subtag #x50 / element-count 1 / matched in the table
  and routes through `%native-sym-resolve` (signals UNDEFINED-FUNCTION
  if no binding). Fixes ANSI's required `(funcall 'sym ...)` path.
  +8 passes (the substitute family that motivated this change is
  blocked behind a different issue — closure capture — not this one).

- **Closure env passed via R13 register, not a global memory slot.**
  Replaced the single `+CLOSURE-ENV-ADDR+` (#x10000140) with a
  reserved x86-64 register (R13 was unused). New IR opcodes
  `:set-cenv` / `:get-cenv` handle the transfer; closure body's
  `(%get-cenv)` snapshot at entry stays the same in shape but no
  longer collides on a global slot when nested closures call each
  other. +1 pass (clean substitution, no regression).

- **Closure auto-capture finally lit up.** Rewrote the free-variable
  walker to use direct `(rec (cdr form) (rec (car form) acc))` style
  instead of the dolist-based `cfv-list` mutual recursion that was
  triggering the regression cascade. Same captures, same closures —
  but the dolist shape silently dropped chunks from the binary at MVM
  compile time. We don't have an SBCL-side explanation; the rec shape
  doesn't trigger it. +290 passes from substitute/find/count/format-
  circumflex/format-brace/nintersection/position/adjoin families
  unlocking now that closure-returning library helpers actually work.

## Closure-mechanism investigation (resolved-but-mysterious)

Wrote a free-variable walker (`%collect-free-vars` family) that
correctly identifies free vars in lambda bodies, including a
special-var exclusion (`*foo*` patterns aren't lexically captured).
At ~845 captures across the build, enabling auto-capture causes a
**−76 pass regression** that is *not* about the env-passing
mechanism (R13-only is +1) and *not* about correctness of the
walker itself. Confirmed with chunk-entry markers: when the walker
is invoked at compile-lambda time, run-ansi-NUMBER-COMPARISON (and
~12 other chunks: assoc, rassoc, labels, destructuring-bind, flet,
equalp, letstar, let, loop2-5, member-if{,-not}, nsublis, sublis,
multiple-value-setq, with-hash-table-iterator) silently exits — its
defun's first form (`(write-string-serial "[E:...]")`) never prints,
even though the *caller* lambda's first form (the analogous
`[C:...]`) does.

Reproducer:

```
(let* ((_walker (%collect-free-vars-list actual-body nil nil nil))
       (captured-vars nil))
  (declare (ignore _walker))
  ...)
```

A walker call with **NIL env** — guaranteed pure traversal, can't
find any captures, returns NIL — still reproduces the regression.
Replacing the walker call with anything else of similar shape
(`(length actual-body)`, a hand-written deep-walk no-op, just
`(%extract-lambda-param-names actual-params)`) does **not** cause
the regression. Only `%collect-free-vars-list` does.

Binary size: 32.66MB without walker, 31.87MB with walker. Smaller =
some ~800KB of compiled code is missing — almost exactly matching
the test wrappers in the regressed chunks. So the chunks aren't
just silent at runtime; they fail to compile cleanly. But there's
no translator error, no SBCL warning specific to those chunks, no
"WARN: giving up" — the build looks normal, the binary just lacks
those defuns.

Open hypotheses, none confirmed:
- SBCL form-traversal mutates something that affects later
  compilation (cons-cell sharing? hash-cons interning? GC pressure?)
- Walker recursion through `cdr` chains tickles a SBCL compiler bug
- Some `let*` in my walker shadows a special variable the MVM
  compiler relies on
- `*compiler-label-counter*` advances during walker-invoked side
  effects we haven't found

Next-session path: 1) bisect the walker — start from a literal
no-op and add operations one at a time until the regression appears;
2) introspect *functions* hash-table size with vs without walker;
3) compare the SBCL macro-expansion of compile-lambda walker-on vs
walker-off for a specific defun. The R13 plumbing and the special-var
exclusion are committed and ready when this lands.

Measured 2026-04-25. Pass rate of tests that ran cleanly:
8572 / (8572 + 1942 real fails) = **81.5%** (up from 79.8%).
Overall pass rate: 8572 / 17692 = **48.5%**.
The remaining 7681 crash-fails are runtime SIGSEGVs; substitute /
count / format-circumflex / numbers / typecase / format directives
are the next big surfaces.

Historical:
(+460 passes and +126 lost since 6588 / ~10000 / 1089 at the start
of the multi-session run; run-to-run variance is ±20 via SIGALRM
timing).

Relevant commits across these sessions:
- Auto-emit (setq) for test-file defvar/defparameter inits
- __handler_pop: preserve RAX so handler-case body result isn't
  clobbered (eliminated all 318 `#<?184>` verdicts)
- format ~{~} / ~^: fixed scanner, factored helpers, and **moved ~{
  and ~^ to the top of the dispatch cond** to avoid an MVM compiler
  bug where late cond branches silently never match.
- Error-detection wave: type/arity checks in list-length, endp,
  butlast, last, make-list, and critical compiler macros (PLUSP,
  MINUSP, ABS, LOGNOT, LDB, FIRST..FIFTH, REST, /=, CADDR, CDDDR,
  CADDDR). The macro fixes alone were worth +32 passes and -108
  lost — the macros silently dropped extra args so (HANDLER-CASE
  (PROGN (PLUSP 0 0) NIL) (ERROR (C) T)) never saw an error to
  catch.
- **Closures and symbols migrated off cons representation** to their
  reserved object subtags (#x52 for closures, #x50 for symbols).
  The old form collided in funcall's consp dispatch and was the
  root cause of `(funcall 'sym …)` crashing through misdispatch.
  +352 passes across the two migrations.

## SOLVED: compile-syscall3 V-reg clobber bug

Root-caused the 16k-lost regression that appeared whenever
compile-car/cdr emitted a runtime consp-check + signal branch.

The bug was in `compile-syscall3`: it staged arg0..arg3 into fixed
VRs V4-V7 and then MOV'd them into V0-V3 right before the trap.
But it didn't bump `*temp-reg-counter*`, so a nested compile-form
for a later arg — like reading a global variable, which goes through
`compile-variable-ref → :call "SYMBOL-VALUE"` — reached
compile-call's caller-save loop, which reads the temp counter to
decide which of V5-V8 to push. Counter = 0, nothing pushed. Callee
clobbered V5, and the pid that compile-syscall3 had just stored
there (for the wait4 call in fork-file) was gone. strace saw
`wait4(1867941892)` — the low bytes of a string pointer that
SYMBOL-VALUE had left in V5.

The bug was LATENT in baseline: register allocation in SYMBOL-VALUE
happened not to clobber V5. Adding a compile-car safe-path (which
touches SYMBOL-VALUE's compiled body too, since it has car/cdr
internally) shifted the allocator and surfaced the bug.

Fix: push each syscall3 arg onto the stack before compile-form'ing
the next, pop into V0-V3 right before the trap. No more fixed-VR
staging, no dependency on the temp counter being accurate across
sub-compile-form calls.

With the fix in place, the full consp-check version is stable —
6984 pass vs 7048 baseline — so we lose 64 passes from legitimate
tests that relied on `(car non-cons)` returning lax garbage. Keeping
null-only for now to preserve those; the consp-check is ready to
enable whenever we want strict ANSI semantics.

Starting point for next session: `git log --oneline` for the
handoff-chain commits — they're self-contained and explain their own
rationale. This doc is the shared context.

## Lost-test hunt: extended-char.3 workaround (+97 passes)

**Current: 7168 passes, 9553 fails, 971 lost.** Baseline was 7071/9517/1104.

### Finding the biggest loss

Used the output post-processor to enumerate T: id gaps. Each first-party
ANSI file's fork records ONE `FAIL <first-id>` sentinel on death; tests
in that file with no `T:<id>` line are lost. The largest gap (115
tests) sat in `characters/character.lsp`, IDs 16499..16622.

### Narrowing to the crasher

Added per-test `B:<id>` markers just before the outer `handler-case`
entry for tests in the suspect range 16505..16520. Output showed:

    B:16509 → FAIL 16509        ; outer handler caught (expected)
    B:16510 → (then nothing)    ; handler entered but didn't return
    FAIL 16499                  ; fork-death sentinel

Then split the marker in two (one before `handler-case`, one inside
the thunk after setjmp):

    B1 → B2 → (nothing)         ; thunk entered, body crashed

So `(extended-char.3.body)` is the culprit. Neither the inner nor
outer handler-case catches it; the fork dies.

### The workaround

In `mvm/build-ansi-test.lisp`, replaced the wrapper for id 16510 with
a bare `(%test-crash-fail 16510)` (no `handler-case`, no `run-test`
call). The file's fork now records the pre-stamped FAIL for 16510 and
continues into tests 16511..16622. 113 downstream tests recovered +
some side-effect passes elsewhere = +97 passes / -133 lost.

### Root cause: still open

`extended-char.3.body` is:

    (loop for i from 0 below (min 65536 char-code-limit)
          for c = (code-char i)
          unless (not (and (typep c 'base-char)
                           (typep c 'extended-char)))
          collect (char-name c))

With `char-code-limit` set to 256 and both type predicates never
simultaneously true (our `typep` correctly returns NIL for
`'extended-char`), the loop should produce `()` without ever
consing anything. Standalone reproducer with stubs doesn't crash.
Standalone with real typep + char-name from a fresh MVM build
crashed — but that was because typep was unresolved and
CALL-INDIRECT'd on poison. In the ANSI build, typep is resolved
and the crash is real but the diagnosis path stalls here.

Hypothesis: GC interaction. The loop creates a long-lived `result`
list accumulator + short-lived `c` chars; if GC triggers mid-loop
from an unrelated allocation and has a bug in the specific state
left by this body, the fork could segfault inside the collector in
a way the SIGSEGV → longjmp handler can't recover from. Confirmation
would need core-dump analysis or GDB on the child fork.

### Attempts to skip other first-test-crashing files (didn't pan out)

Added an `I:<first-id>` diagnostic after init forms so we could tell
init-dying files from first-test-dying files. Categorizer found 10
"init OK, first test dies" candidates (loop7, format-ampersand,
defmethod, array-as-class, set-syntax-from-char, acosh,
update-instance-for-different-class, subtypep-function,
defclass-forward-reference, unbound-slot).

Result: stamping FAIL for the first test of each of those files and
re-measuring, only format-ampersand got any recovery (9 tests ran, 3
passes). The other 9 files recovered nothing — the "first-test dies"
classification was wrong (the `I:<first-id>` actually coincides with
the first test's `T:<first-id>`, so the marker succeeded but the
first test's body still crashed uncatchably). Net effect vs pure
16510-only baseline was about -13 passes, within run-to-run variance.

Skipping expt.18..28 (13567..13577) and typep.19 (25630) was also
tried — they're slow infinite-loops that eventually die to SIGALRM
at 45s each. Saves wall-clock but no passes; skip wasn't worth the
complexity. Reverted.

**Current skip list: just 16510.** +97 passes, -133 lost vs the
7071 baseline that preceded the paren fix.

### Next fork-death gaps (unskipped, for future hunts)

Files where the fork dies during init-or-first-test (no `T:` in
range) and no simple skip helps:

| File                  | Lost  |
|-----------------------|-------|
| format-circumflex     | ~250  |
| syntax                | ~141  |
| nsubstitute-if-not    | ~116  |
| make-array            | ~114  |
| substitute-if-not     | ~114  |
| format-brace          |  ~96  |
| string-comparisons    |  ~95  |
| number-comparison     |  ~94  |
| adjust-array          |  ~87  |
| count-if-not          |  ~80  |

A generic auto-recovery (parent re-forks with `*skip-below*` after a
child dies) would require parent/child shared memory so the parent
can read the last-attempted test id. MVM has `syscall3` but not
`syscall6`; the cleanest route is a pipe-based channel (`syscall 22 =
pipe` takes one arg, fits `syscall3`). Child writes test id after
each `T:` emit; parent reads all buffered data after `wait4` returns
non-zero. Deferred.

## SOLVED: Package functions compiled to no-ops (+65 passes)

`mvm/compiler.lisp` had a special-dispatch at the top of
`compile-compound` that turned every call to `MAKE-PACKAGE`,
`FIND-PACKAGE`, `FIND-SYMBOL`, `EXPORT`, `IMPORT`, `SHADOW`, and
`USE-PACKAGE` into `(compile-nil dest)` — a hard-coded NIL. Next to
them were `PROVIDE` / `REQUIRE` / `PROCLAIM` / `DECLAIM`, which
genuinely have no runtime implementation and should stay as no-ops.

The grouping was wrong: the package functions all have real defuns in
`mvm/cl-packages.lisp`. Falling through to `compile-call` (the default)
makes those real defuns get invoked, at which point the package
system actually functions — `*all-packages*` populates, `find-package
"COMMON-LISP"` returns a package object, etc.

Removed the package functions from the no-op list, kept
`PROVIDE/REQUIRE/PROCLAIM/DECLAIM`. +65 passes concentrated in
files that genuinely exercise the package system: syntax (+16),
find-package (+12), package-name (+10), find-symbol (+9), format-x
(+9), make-package (+5), package-nicknames (+5), etc. 43 tests
regressed (mostly format-d, gentemp, in-package) because some
downstream code was relying on the silent-nil behavior.

### What didn't help: `*pkg-hash-to-name*` for native MVM symbols

ANSI tests pass designators like `'common-lisp` as the package arg
to `find-package`. That quoted symbol compiles to a native MVM symbol
(subtag 0x50 object with just a hash at slot 0 — no stored name).
`%pkg-string-designator` can't recover the string "COMMON-LISP" from
just the hash, so `find-symbol "&OPTIONAL" 'common-lisp` always
returns `(values nil nil)` and cl-symbols.lsp (978 tests) keeps
failing.

Tried: build a hash→name map in `make-package` so native symbols
with matching hashes can resolve. The runtime's `compute-name-hash`
had to match the build-time algorithm (added an equivalent `defun`
in `mvm/prelude.lisp`). Got hashes matching — but the subsequent
`find-symbol` path introduced an uncatchable crash somewhere in
the resolve chain, so reverted.

A cleaner fix would be to extend native MVM symbols to carry their
name string at allocation time (requires layout change to
subtag-0x50 objects and the compile-quote emission path). Deferred.

## SOLVED: "late-cond-branch miscompilation" was a paren bug

Turns out this wasn't a compiler bug at all — it was a mis-counted
paren in `%format-impl`'s `~( ~)` clause.

### How it masqueraded as a compiler bug

The `~( ~)` clause had one missing `)` inside its body (specifically
at the close of `(%print-string-raw converted stream)` — the sequence
of trailing `))` needed to be `)))` to close LET-CONVERTED, LET-RESULT,
AND LET-SUB-S2). The `%format-impl` function was "balanced" overall
because two extra `)` at the very end of the function absorbed the
stray opens. SBCL's `READ` actually errors on this file with "unmatched
close parenthesis" — but the MVM reader is more forgiving and
processed the structure in its miscounted form.

The net effect of the off-by-one: every `cond` clause after `~( ~)` —
`~[`, `~{`, `~}`, `~^`, `~_`, `~I`, `~/`, and the `(t ...)` default —
ended up as body forms of the `~( ~)` clause, not sibling clauses of
the outer cond. The outer cond looked like:

    (cond (...)                ; ~A through ~(
          ((or (= dir 40) (= dir 41))
           (when ...)           ; real ~( body
           ((or (= dir 91) ...)    ; absorbed as a second body form of ~(
            (when ...) ...)
           ((= dir 123) (let ...)) ; absorbed as a third body form
           ...))

So when `compile-if` compiled the `~( ~)` clause, it passed those
"extra body forms" to `compile-progn`. `compile-progn` iterated over
them and called `compile-form` on each, which fell into
`compile-compound`. Each "form" looked like `((= dir 123) (let ...))`
— a list-headed form. That matches the `(null op-name)` branch of
`compile-compound`, which hands off to `compile-call` with the LIST
as the "function" and the body as a single argument. `compile-call`
then goes to its last branch (not a symbol, not a `(setf name)` form)
and emits `:call-indirect` on whatever `(= dir N)` evaluates to.
Since `(= dir N)` evaluates to T or NIL, the runtime called through
T or NIL as if they were code — which failed silently.

That's why the symptom presented as "late clauses never match":
- The test compiled as a normal equality producing T/NIL.
- The body was then indirect-called through that T/NIL.
- CALL-INDIRECT(T, 1) and CALL-INDIRECT(NIL, 1) do nothing observable.
- So `cond` fell through every "clause" after `~( ~)` without running
  any body.

### How we found it

1. `ansi-notes` sentinel bisection showed the cutoff was at cond
   position 19 (the `~( ~)` clause) — replacing that body with `nil`
   fixed every downstream clause. That localized it to `~(`.
2. Dumped `%format-impl`'s IR to `/tmp/format-impl-ir.txt` and saw
   `:CALL-INDIRECT` instructions where `:BNULL` dispatches should have
   been. Each CALL-INDIRECT was preceded by a compile-eq pattern
   (CMP/BEQ + T/NIL materialization), confirming the T/NIL value was
   being CALLED.
3. Only three sites emit `:call-indirect`. Logged `fn` and `args`
   at the `compile-call` fallback for `%format-impl` and saw:

       FALLBACK-INDIRECT fn=(OR (= DIR 91) (= DIR 93)) nargs=1
         args=((WHEN (= DIR 91) ...))
       FALLBACK-INDIRECT fn=(= DIR 123) nargs=1
         args=((LET ((NEW-I-AND-ARGS ...)) ...))
       FALLBACK-INDIRECT fn=(= DIR 125) nargs=1 args=(NIL)
       ...

   Each "fn" was a cond-clause test and each "args" was that clause's
   body — i.e., the cond clauses were being compiled as function
   calls.
4. `sb-debug:print-backtrace` at the fallback showed the caller was
   `COMPILE-PROGN` with a form list containing the remaining cond
   clauses. The enclosing frame was `COMPILE-IF` handling the `~(`
   test — so the cond clauses after `~(` had been absorbed into `~(`'s
   `then` branch.
5. Python paren-balance checker on `mvm/cl-printer.lisp` reported
   `final depth: -1` — confirming one paren was off.
6. Line-by-line depth trace showed the depth at the end of the `~(`
   clause was `1` instead of `0`, with one missing close on the line
   `(%print-string-raw converted stream)))`.

### The fix

`mvm/cl-printer.lisp`:
- Add the missing `)` after `(%print-string-raw converted stream))))`
  so `LET-CONVERTED / LET-RESULT / LET-SUB-S2` all close in the
  intended place.
- Remove the two compensating extra `)` at the very end of
  `%format-impl` (`arg-list))))` → `arg-list))`).

Result: +23 passes (7048 → 7071), −111 lost to test crash. And the
earlier workaround that hoisted `~{` / `~}` / `~^` to the top of the
cond is no longer needed — those clauses now dispatch correctly from
their natural late positions.

### Relation to the other "N-th thing silently vanishes" cases

- **rt-equal regalloc crash** (commit 489c557): genuinely a compiler
  issue and still worth understanding. Different fingerprint.
- **AArch64 ~25-form truncation** (`tests/test-aarch64-progn-limit`):
  also genuinely a translator bug, still to be investigated.
- **run-cl-loop-tests threshold flip**: likely the same family as
  AArch64.

The `%format-impl` cond failure was NOT one of these. It was a
hand-crafted paren bug that happened to present with the same symptom
("dispatch silently skips past some position"), which is why it was
initially misdiagnosed as part of this family. Moral: when the
compiler appears to "silently skip" forms, always verify the source
parses first.

## How the harness works

`mvm/build-ansi-test.lisp` builds `/tmp/modus-ansi-test`. It:

1. Loads all `cl-*.lisp` plus `ansi-bridge.lisp` into a big source
   string (`*bridge-source*`).
2. Loads every `.lsp` from `/tmp/ansi-test/*` into
   `*real-ansi-sources*`, rewriting:
   - `(deftest NAME form result)` → `(fork-file ID (lambda () ...))`
   - Each test wrapped in `(handler-case (run-test ID ...) (t (c)
     (%test-crash-fail ID)))`
   - Multi-arg `apply`, 0-dim `make-array`, quoted vector dimensions
   - Strip `:compile-toplevel`, SBCL-only reader macros, etc.
3. Compiles the concatenated source via the MVM compiler → ELF.

Two dumps useful for debugging:
- `/tmp/real-ansi-gen.lisp` — the full generated Lisp source (~11MB).
- `/tmp/build.log` — redirect the sbcl run here; has `Wrote N bytes`
  at the bottom on success, SBCL backtrace on failure.

## Execution model (runtime)

```
parent process                                     binary entry
  ├── init-forms (setq *foo*) — in main entry
  ├── (run-all-tests) — custom deftests, no fork
  ├── ANSI-TOTAL=<N>
  └── (run-real-ansi-tests)
        └── (fork-file FIRST-ID (lambda () (run-ansi-FOO)))
              ├── clear [#x10000180], [#x10000400]  (handler state)
              ├── alarm(*file-alarm-secs*)          (SIGALRM on timeout)
              ├── (handler-case (funcall thunk)
              │     (t (c) (%record-test-fail first-id)))
              │     └── (run-ansi-FOO) = sequential handler-case per test
              │           └── (handler-case (run-test ID ...) (t (c) ...))
              │                 └── (handler-case (rt-run-test ...) (t (c) ...))
              ├── alarm(0)                          (cancel before exit)
              └── sys_exit(0)
```

Parent waits via `wait4`; if child exits non-zero (signal-killed, or
we manually exit non-zero), parent records one FAIL with FIRST-ID.

## Output tokens

- `T:<id>` — test starting (rt-run-test prologue)
- `P:<id>` — pass
- `FAIL <id>` — per-test handler caught a raised error / misverdict
- `FAIL <id> GOT:... EXP:...` — rt-run-test verdict mismatch with printed GOT/EXP (subject to `*fail-cap*`, now 2000)
- `ANSI-TOTAL=<N>` — expected test count, line emitted just once at end of custom-test phase
- `SLOW <id> <cycles>` — was added during session for rdtsc
  profiling; **reverted because of 100+ pass regression**.

## What "lost" means

`lost = expected − (passed + failed)`. A lost test has *no* P/FAIL
line in the log. Usually because the fork died without recording.
Causes seen in this session:

1. **Nested handler-cases clobber each other's setjmp frame** — fixed
   by per-fork handler stack (commit `aad956e`). `[#x10000400]` is the
   stack depth; `[#x10000408 + 24*N]` are saved (RSP, RBP, IP) frames.
   SETJMP/CLEAR-HANDLER/LONGJMP/SIGSEGV-stub all push/pop this stack.
2. **Hardware faults during test** — SIGSEGV/BUS/FPE/ILL are converted
   to longjmp by the stub so the per-test handler-case catches. SIGALRM
   is *not* hooked (attempted — see below).
3. **Fork-file outer handler silently exited 0** — fixed by having it
   call `%record-test-fail FIRST-ID` + cancel alarm before `sys_exit`
   (commits `eeca1c6`, `111e9c8`).
4. **Infinite loops inside builtins** — DECODE-FLOAT on NIL, SXHASH on
   circular cons, TYPEP 'BIGNUM never firing on overflowing int,
   `(make-array nil ...)`. Fixed commits `b166ab5`, `e89bed3`.

## Pending: what kills the remaining ~1089 lost

79 distinct killer tests surveyed via the TRY-marker diagnostic (see
below). Three rough clusters:

- **~25 "float tower" tests**: `(LOOP FOR TYPE IN '(SHORT-FLOAT SINGLE-FLOAT
  DOUBLE-FLOAT LONG-FLOAT) ...)` threading COERCE/RANDOM/COMPLEX/ACOSH on
  floats. Our (RANDOM boxed-float) does integer mod on a pointer, producing
  garbage whose downstream arithmetic loops. Fixing RANDOM alone doesn't
  help — tests need the whole tower (COERCE + COMPLEX + ACOSH + FLOAT-DIGITS
  + `(TYPEP x (COMPLEX SHORT-FLOAT))`) to reach a clean verdict. Attempts to
  guard/truncate RANDOM either slowed into SIGALRM (truncate → loops run
  their full REPEAT 1000) or regressed passes (type-error → tests that had
  been passing with garbage output now fail differently).

- **~10 bignum tests**: `(LET ((BOUND (ASH 1 200))) ...)` and the
  `(LOOP WHILE (NOT (TYPEP X 'BIGNUM)) DO (SETF X (* X X)))` pattern.
  TYPEP BIGNUM-threshold fix lets the loop exit, but the BIGNUM-valued
  result then participates in downstream arithmetic that silently
  overflows our 63-bit fixnum and crashes the test in a different place.

- **Misc**: MAKE-LIST 'A (type-error inside a catch-type-error that our
  compile doesn't let propagate cleanly), destructuring LOOP `(X Y . Z)`
  over undefined `*number-less-tests*`, bit-array / hash-table fixtures,
  FLET helpers with recursion.

The 79 killer IDs are in `/tmp/killers.txt` when the TRY-marker build
is active; regenerate with the diagnostic below.

## Diagnostics tried this session

### TRY markers (in `build-ansi-test.lisp`, reverted)

Add to the codegen loop emit a `(when *try-markers-on* (write-string-serial
"\nTRY ") (print-dec ID) (write-char-serial 10))` before each per-test
handler-case. Then killer tests are the ones whose last `TRY <id>` in the
fork's stdout has no matching P/FAIL.

Analysis pipeline:

```
awk '/^TRY [0-9]/ {tid=$2+0; t_seen[tid]=1; pending=tid; next}
     /^P:[0-9]/   {tid=substr($0,3)+0; if (tid==pending) pending=0; passed[tid]=1}
     /^FAIL [0-9]/ {tid=$2+0; if (tid==pending) pending=0; failed[tid]=1}
     END {for (t in t_seen) if (!passed[t] && !failed[t]) print t}' \
  /tmp/run.log | sort -n > /tmp/killers.txt
```

**Why reverted**: having the when/write-string-serial/print-dec form in
the codegen, even with `*try-markers-on*=NIL` at runtime, regressed
passes by ~66. Something about the extra code per test (stale spill
slot? cache effect?) pushes files past their SIGALRM budget. Only turn
it on for one-shot diagnostic runs.

### rdtsc per-test profiler (in `build-ansi-test.lisp` + `compiler.lisp`, reverted)

Same idea — wrap run-test with `(let ((t0 (rdtsc))) body (%record-slow-test
id (- (rdtsc) t0)))`. `compile-rdtsc` needed a SHL +fixnum-shift+ after
the trap so subsequent arithmetic works. Found:

- typep.19-fn: 41s (single test!). It's the *last* test in typep's file
  so doesn't cause lost tests there.
- ftruncate/fround/ffloor/fceiling: 5 slow tests each
- format-d/o/x/b/r: 3-4 slow each

**Why reverted**: 119-pass regression from whatever the let/rdtsc adds
to each test site. Not a useful profiler for lost tests anyway since
SIGALRM-killed tests never reach the post-body rdtsc.

### Per-file loss breakdown (runnable as-is)

```
awk '
  FILENAME ~ /run.log$/  {if ($0~/^P:[0-9]/) reported[substr($0,3)+0]=1;
                          else if ($0~/^FAIL [0-9]/) reported[$2+0]=1; next}
  FILENAME ~ /all-expected/ {expected[$1]=1; next}
  {f=$1; first=$2+0; last=$3+0; cnt=$4+0; last_seen=0; total=0
   for (t=first; t<=last; t++) if (expected[t]) {
     if (reported[t]) {if (t>last_seen) last_seen=t} else total++}
   if (total>5) printf "%4d lost (%d)  %-25s last=%d/%d\n", total, cnt, f, last_seen, last}
' /tmp/run.log /tmp/all-expected-ids.txt /tmp/file-ranges.txt | sort -rn
```

`/tmp/file-ranges.txt` and `/tmp/all-expected-ids.txt` are regenerated
by scanning `/tmp/real-ansi-gen.lisp`:

```
awk '/^\(defun run-ansi-/ {match($0,/run-ansi-[^ )]*/); f=substr($0,RSTART+9,RLENGTH-9)}
     /run-test(-mv)? [0-9]+/ {match($0,/run-test(-mv)? [0-9]+/); s=substr($0,RSTART,RLENGTH)
                              n=split(s,p," "); id=p[n]+0
                              if (!ff[f]) ff[f]=id; if (fl[f]<id) fl[f]=id; fc[f]++}
     END {for (f in ff) printf "%s\t%d\t%d\t%d\n", f, ff[f], fl[f], fc[f]}' \
  /tmp/real-ansi-gen.lisp | sort -k2n > /tmp/file-ranges.txt

grep -oE "run-test(-mv)? [0-9]+" /tmp/real-ansi-gen.lisp | awk '{print $NF}' \
  | sort -un > /tmp/all-expected-ids.txt
```

## Gotchas (CLAUDE.md is also worth rereading)

- **defvar doesn't run its init thunk** at runtime. ANY fresh `defvar
  *foo* val` resolves to NIL at runtime. Explicit `(setq *foo* val)`
  in the parent's main entry is required. Session added ~20 such
  initializers to `build-ansi-test.lisp`. Full list: char-code-limit,
  call-arguments-limit, most-*-fixnum, *should-always-be-true*,
  *use-random-byte*, *random-readable*, *hash-table-test-iters*,
  *type-list*, *supertype-table*, *defclass-slot-*, *mapc.6-var*,
  *report-and-ignore-errors-break*, double-float-epsilon,
  single-float-epsilon, short-float-epsilon, long-float-epsilon (+ the
  negative variants), most-positive-short/single/double/long-float (+
  negatives). Attempted to auto-emit a `(setq foo val)` per
  user-defined defvar via emit-sub in the codegen — produced a paren
  mismatch, reverted. Retry if willing to reshape `emit-sub`.

- **Handler-stack depth = 64** (per-fork stack in
  `translate-x64.lisp`). Tests don't approach it in practice but if a
  macro expands into deeply nested handler-cases you'll silently fall
  back to the old single-slot behavior (stack full skips push).

- **RDTSC result is raw 64-bit cycles**, not tagged fixnum. If you
  want to do arithmetic with normal Lisp integers, add `(emit-ir :shl
  dest dest +fixnum-shift+)` after the trap in `compile-rdtsc`. Loses
  top bit but 2^62 cycles at 2 GHz is ~73 years.

- **File alarm default is 45s** (was 30s originally, session moved up).
  Files that naturally take longer (typep.19 ≈ 41s alone!) are on the
  edge. Consider either shortening typep.19 specifically, or bumping
  alarm higher if you can stomach the wall-clock cost of losses.

- **`*fail-cap*` default is 2000** per fork. Files with >2000
  failures (cl-symbols has 1002, bit-* each ~27, so none hit it now)
  would have FAILs silently dropped past that point.

- **FORMAT directives with `~` in comments in build-ansi-test.lisp will
  explode during the format-nil of the per-fork source string.** Don't
  write "~200ms" in source comments; use "200ms" or escape the `~~`.

## Where would I start next session?

Options #1 (#<?184>) and #2 (defvar auto-emit) are DONE. What's left:

1. **"Expect T get NIL" — the ~509-test bucket (down from 536).**
   Pattern is `(handler-case (progn <op-with-bad-arg>) (error (c) t))`
   where the bad arg *should* trigger a type/keyword error but our
   runtime silently accepts it. Remaining sub-patterns:
   - MVM inline `:car` / `:cdr` IR reads garbage without a type
     check. `(CAR 'A)` silently returns garbage instead of signaling.
     Fixing means either emitting type-check+branch in the inline
     translator or replacing inline with a safe defun (the latter
     regressed unrelated tests when tried — needs more care).
   - `(MAPC #'CONS list)` / `(MAPCAR #'CONS list)` etc. — the
     callback is called with 1 arg but needs 2. Our funcall doesn't
     check arity at call time, so the wrong-arity call silently
     reads garbage. Fix needs funcall arity-check, which requires
     storing per-function arity in the callable object.
   - Many defuns (FBOUNDP, ELT on NIL, etc.) don't type-check args.
     Each one is a targeted fix like LIST-LENGTH/ENDP/BUTLAST.

2. **FORMATTER / ~{...~^...~} — the format-circumflex & format-brace
   bucket (~540 tests).** Tests build `(FORMATTER "...")` then
   funcall the formatter. Our cl-printer doesn't compile format
   strings to closures — FORMATTER calls crash. Either implement
   FORMATTER as "wrap the string in a thunk that calls FORMAT" or
   accept those tests as permanently out-of-scope until the printer
   is refactored.

3. **SUBTYPEP multiple-value return.** `(SUBTYPEP* 'NULL 'CONS)`
   returns `(NIL)` instead of `(NIL T)` in the ANSI tests even though
   cl-conditions.lisp's subtypep does `(values nil t)`. About 30 tests
   rely on the T-return for determinism. Worth a 20-minute
   bisect — is MV-COUNT getting reset on the funcall-through-alias
   path? (`subtypep*` is `(defun subtypep* (t1 t2) (subtypep t1 t2))`.)

4. **Numeric tower, but committed to seeing it through.** Don't start
   unless you can land all of: ratios, complex, multi-float aliases,
   and overflow-detecting multiply. Half-done leaves tests in
   "hangs in different place" state.
