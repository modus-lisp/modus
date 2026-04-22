# ANSI test notes — session log

State as of last session: **~7048 passes / ~9429 fails / ~1215 lost**
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

## NEW gotcha: MVM miscompiles late cond branches (IN LARGE FUNCTIONS)

As of 2026-04 the MVM compiler silently stops matching cond branches
beyond an (unknown) threshold clause count in **sufficiently large
functions**. Symptom: for a branch `((= dir <N>) ...)` sitting deep in
a long cond, the body never executes even when dir equals N. A
standalone `(= dir N)` test in arbitrary code still returns T — so
the bug is in cond-dispatch code generation interacting with function
size, not in = on fixnums.

### What tried to isolate it (all pass, none reproduce the bug)

- 30-clause `(= dir N)` cond in a trivial function.
- Same cond with `or`-heavy clauses mirroring %format-impl's shape.
- Same cond inside outer let/loop/if with many locals.
- "Bulked-up" function: full param parser + modifier parser + 25-clause
  cond with substantial bodies.

All of those work correctly. Only `%format-impl` exhibits the bug. It
shares the same shape as the bulked-up reproducer but is larger — the
trigger seems to be cumulative function size / IR instruction count
past some threshold, at which point the register allocator or spill
logic flips.

This is the same "compile-state flip" that CLAUDE.md documents for
run-cl-loop-tests: "Adding too many deftest forms ... makes some
passing tests start crashing — even ones that have nothing to do with
the new tests." Same root cause, different symptom.

### Workaround

Hoist frequently-missed branches to the top of the cond. See the `~{`
/ `~^` handling in `%format-impl` for an example plus a `NOTE:`
comment flagging the reordering. Also consider factoring very large
functions into helpers so each stays under the threshold.

If you add a new cond branch to a long dispatch in a complex function
and it seems to never fire, this is the first thing to check.

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
