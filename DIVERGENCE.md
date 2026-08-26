# DIVERGENCE — Modus's deviations from CLHS

A living list of where Modus's behaviour differs from ANSI CL.  Goal is
to drive each to zero — if we're building a CL runtime, every
divergence is debt.  Items grouped by subsystem; each entry tags
status (`stub` / `partial` / `divergent semantics` / `not implemented`)
and points at the relevant code or test cluster.

When you fix one, move the entry under "Fixed this session" with the
commit hash.  When a fix is verified, retire it from the list entirely
in the next pass.

---

## Currently-pending divergences

### Reader macros

- `#A(...)` — array literal reader is a stub returning NIL.  Blocks
  array-literal tests + any `#0A`/`#nA` form. *(cl-reader.lisp ~line
  1101)*
- `#S(...)` — struct literal reader is a stub.  Affects defstruct
  test edges that use `#S(name :slot val)` literals.
- `#P"..."` — pathname literal reader returns NIL or the bare
  namestring.  FILEIO2 agent introduced structured pathname objects
  (commit `c0694ce`); the reader could now allocate them properly.

### Setf machinery extensions

- `psetf` (16 fails) — parallel-setf semantics; current impl is
  sequential.
- `rotatef` (10 fails) — rotates a chain of places.
- `multiple-value-setq` (9 fails) — partial; arity edges fail.

### Numeric

- **Float precision tiers**: single / double / short / long all
  conflated to IEEE double.  `most-positive-short-float² → overflow`
  fails because the result is well inside double range.  Needs
  format-distinct float boxing (precision tag in the float object).
- **Complex arithmetic** — `complex` literal works; arithmetic
  operations may diverge.  Unprobed.
- **Random** — capped at 30 bits per `reference_apply_arity_8` blocker
  on `compare-rationals.4`.
- Ratio edges (normalisation, repeated promotion under division) —
  mostly work; not exhaustively probed.

### Type system / declarations

- `(declare (type ...))` — fully ignored at runtime, no type
  enforcement at compile-time either.
- `declaim` (partial stub) — mostly no-op except `(declaim (special
  ...))`.
- `proclaim` (13 fails) — partial stub.
- `the` — handled for setf-place (commit `f03d9f1`); elsewhere mostly
  ignored.

### CLOS / MOP

- **MOP not implemented** — `class-of`, `slot-value-using-class`,
  method-combination customisation, `validate-superclass`, etc.
- `define-method-combination` long form — may have gaps.
- `change-class` (32 fails) — partial.
- Slot allocation `:class` vs `:instance` — fixed by CLOS-X agent
  (commit `c2acdf8`).

### Debug / introspection

- `trace` / `untrace` (11 fails each) — stubbed.
- `disassemble` (13 fails) — stub by EVAL-X agent (+12 ANSI from
  stub).
- `describe` — likely stub.
- `break` / `debugger` — not implemented.
- `inspect` — stub by EVAL-X (commit `a449acd`).
- `room` / `time` — stub by EVAL-X (commit `4c332bb`).

### Restart machinery edges

- `assert` macro semantics (9 fails) — stub.  Needs restart-case +
  setf-place loop per CLHS 29.4.1.
- `check-type` (4 fails) — same shape as assert.  Current rewriter
  expands to `(unless typep (error))`; tests want `store-value` restart
  to correct the place.
- `restart-case` options — `:test` `:report` `:interactive` discarded
  by the build-script rewriter at `mvm/build-x64-linux.lisp:1769`.
- `with-condition-restarts` — prior agent attempt regressed; left as
  `(progn body)` stub at `mvm/build-x64-linux.lisp:1825`.
- `restart-bind` return-from across closure body — needs compiler /
  runtime block-tag setjmp.

### Runtime compilation

- `compile-file` → FASL — not implemented (CLAUDE.md Quicklisp gap).
- Runtime `compile` (source → bytecode → native) — same.
- Runtime `defstruct` in eval — compile clause for nested defstruct
  added this session (`1f06efd`); eval interpreter still doesn't know
  about new structs at runtime.

### Streams

- `with-output-to-string` with 2nd arg — rewriter ignores target-string
  arg; needs fill-pointer-aware string sink.  *(build-x64-linux.lisp
  ~line 1356)*
- `read-sequence` / `write-sequence` — partial.
- Binary streams (`:element-type '(unsigned-byte 8)`) — partial.
- `file-position` — partial; FILEIO2 added some support (`c0694ce`).

### Build-time test-helpers (rewriter stubs)

- `handle-non-abort-restart` — stub returns `'fail` instead of
  checking restarts.  *(build-x64-linux.lisp ~line 2389)*
- Various `make-int-list` / aux helpers — partial.

### Argument evaluation order

- **A call's 5th and later argument forms are evaluated FIRST, and among
  themselves RIGHT-TO-LEFT** — the register arguments (1-4) are evaluated
  afterwards, left to right.  CLHS 3.1.2.1.2.3 requires strict left-to-right
  for the whole argument list.  *(divergent semantics; the overflow-argument
  path — see `#x0530` COPY-OVERFLOW-ARGS in the translators.)*

  Measured with a counter bumped once per argument form, `./modus` against
  SBCL — the value each parameter receives, in parameter order:

  | call | SBCL (correct) | modus |
  |---|---|---|
  | `(f4 (b) (b) (b) (b))` | `(1 2 3 4)` | `(1 2 3 4)` |
  | `(f5 (b) (b) (b) (b) (b))` | `(1 2 3 4 5)` | `(2 3 4 5 1)` |
  | `(f6 ...)` | `(1 2 3 4 5 6)` | `(3 4 5 6 2 1)` |
  | `(f7 ...)` | `(1 2 3 4 5 6 7)` | `(4 5 6 7 3 2 1)` |

  Same for `&rest` functions; `LIST` is unaffected because it does not go
  through the general call path.

  **Only argument lists with SIDE EFFECTS are affected** — each form is still
  bound to the right parameter, so pure arguments give correct results and this
  is invisible until an argument mutates something another argument reads.  The
  common way to meet it is a `FORMAT` with three or more directives whose
  arguments are not pure, e.g.
  `(format t "~S ~S" (take-and-clear x) (read x))`, which reports the
  *un-cleared* value.  Found by running glass/fb's drawing primitives under
  modus against SBCL as the oracle (`test/run-glass-fb.sh`).

  **RE-DERIVED INDEPENDENTLY IN THE THREADS/GLASS CAMPAIGN (2026-08), AND THE
  TABLE ABOVE IS CORRECT AS WRITTEN.**  The second measurement reported "three
  bumps evaluate 3,1,2" and looked like a contradiction.  It is not — it is the
  same rule, and the reconciliation is the thing worth writing down:

  * ***`FORMAT`'s `t` AND THE CONTROL STRING OCCUPY ARGUMENT SLOTS 1 AND 2.***
    So `(format t "~s ~s ~s" (b 1) (b 2) (b 3))` has the three side-effecting
    forms in slots **3, 4 and 5** — one of them is in the overflow path — and
    the rule predicts evaluation order `3, 1, 2` exactly, which is what was
    measured.  It is very easy to count the visible arguments and conclude the
    register/overflow boundary sits three places later than it does.
  * **THE TABLE ABOVE MEASURES A DIFFERENT QUANTITY FROM AN EVALUATION-ORDER
    LOG.**  Its columns are *the value each parameter receives*, with a counter
    bumped once per argument form — so the FIRST form evaluated receives 1.
    Eval order `(5 1 2 3 4)` therefore shows up as params `(2 3 4 5 1)`.  Both
    descriptions are of the same run.  When re-measuring, log the order
    directly (push a marker per form and reverse it) as well as the parameter
    values, or the two are easy to mistake for a disagreement.

  Confirmed on the current build with a direct evaluation-order log —
  `f3` -> `(1 2 3)`, `f4` -> `(1 2 3 4)`, `f5` -> `(5 1 2 3 4)`,
  `f6` -> `(6 5 1 2 3 4)`, `f7` -> `(7 6 5 1 2 3 4)`, SBCL `(1 2 … n)`
  throughout — i.e. arguments 5+ first and right-to-left among themselves, then
  1-4 left to right, precisely the rule stated above.  **This bug has now cost
  two separate investigations; read this entry before measuring it a third
  time.**

### Eval-time machinery

- `eval-when` — partial; `(:compile-toplevel :load-toplevel :execute)`
  situations.
- `load-time-value` — not implemented.
- `progv` — partial.
- Macro `&environment` — not parsed in `defmacro`/`macrolet`
  lambda-lists.
- `compiler-macro-function` — registry added by MACRO agent (`75089fa`
  neutral; needs follow-up to actually dispatch compiler macros).

### Print machinery

- `pprint` family — many stubs (`pprint-logical-block`,
  `pprint-indent`, `pprint-exit-if-list-exhausted`, `pprint-pop`, etc.).
- `*print-pprint-dispatch*` — likely stub.
- `print-object` for conditions — `defmethod` blocked by ansi-bridge
  load order (override registered after the wired GF).
- `*print-array*` / `*print-readably*` edge cases.

### Symbol model

- **Phase 1 follow-up** — symbol home-package slot is NIL when it
  should be a package.  Visible as `#:X` instead of expected `X` or
  `CL-TEST::X` in the print-symbol cluster.  Probably a `pkg-by-hash`
  timing issue at boot or a compile-quote pkg-hash mismatch.  See
  `SYMBOLS_PLAN.md` Phase 1.
- Symbol property lists (`get` / `(setf get)`) — likely partial.
- Symbol redefinition edge cases.

### Other

- Hash-table edge cases (`equalp` test, weak keys, rehash).
- `format` directives with `:` / `@` modifier combos — many edge
  cases.
- `format-control` pre-compiled formatters via `formatter` — partial.
- `*random-state*` — fixed-seed; not full CL semantics.
- `tagbody` / `go` — partial.
- `block` / `return-from` — partial.

---

## Fixed this session (chronological)

- `7e79aa2` — bignum-aware `%truncate2-generic` via `%INTEGER-TRUNCATE`
- `5c039a0` — DEFSETF long form follows CLHS 5.1.1.2
- `e33bc69` — cl-reader: shared structure `#N=`/`#N#`, mid-token
  escape, radix ratios
- `32b0ffe` + `7a306d3` — compiler: CL operand-eval semantics, apply
  ladder gap, arity for comparison ops
- `aadd95a` + `1e3b25e` — expt: `(= power 0)` returns float-1; signal
  floating-point-overflow / underflow on IEEE non-finite / zero
- `692cf20` — streams: stream-subtype predicates + typep dispatch
- `e1ee954` — character: signal type-error for non-character input
- `76d0ec9` — load: file-error on missing file, program-error on
  `(load)`, validate kwargs
- `c39420b` — file-IO cluster: echo-stream, close, file-error,
  peek-char, read-byte
- `397a668` — cl-printer: `~^` propagates through `~( / ~) / ~?`, `~A`
  negative MINPAD
- `d9fcb0c` + `4c332bb` + `a449acd` — cl-eval: CLHS arity for
  macroexpand / macro-function, runtime stubs for COMPILE-FILE / TIME
  / ROOM / INSPECT / DISASSEMBLE
- `433c1b3` — vector-push / -extend: displaced-aware + extension hint
  + type-error on missing fp
- `93a43c3` — compiler: cell-rewrite + closure transform for FLET /
  LABELS captures
- `d6296c6` — cl-fileio: structured pathname objects + lazy CLOS class
  registration
- `74b126e` — cl-clos: short-form method combination correctness
- `ab2fedf` — cl-sequences: validate keyword args + resolve fn
  designators in -IF family
- `b09b978` — cl-conditions: restart-case nested invoke + MV
  propagation
- `393374d` — cl-packages: PKG2 agent (defpackage CLHS 11.1.1.1
  checks)
- `7bec439` — cl-reader: READER2 agent (syntax + reader-test +
  with-stdio)
- `c2acdf8` — cl-clos: CLOS-X agent (defgeneric / defclass-01 / shared-
  init / change-class)
- `91702fb` — cl-conditions: CND3 agent (restart-bind / case + warn +
  define-condition)
- `dffe38e` — build/scripts: move binaries + logs + ANSI test source
  to project-local `tmp/`
- `769bf36` — cl-reader: implement `#.` read-time eval
- `f03d9f1` — compiler: `setf (the type place)` + `setf (values ...)`
  macro expansion
- `334cc83` — compiler: handler-case `:no-error` clause per CLHS
  9.1.5.1.1
- `88a89bd` — build/rewriter: with-package-iterator implements actual
  symbol enumeration
- `449d8b0` — compiler: nested DEFMACRO in expression context
  registers via mvm-define-macro
- `9de3ec5` — build/rewriter: ignore-errors returns `(values nil c)`
  per CLHS 9.1.5.3.1
- `4bcd4ef` — CLAUDE.md: update Active Limitation #3 for SYMBOLS_PLAN
  phase 1
- `a1327d6` — SYMBOLS phase 1: per-package distinct intern (CLHS
  11.1.2)
- `0d9c9cc` — compiler: implicit special binding for CLHS-standard
  earmuffs
- `1f06efd` — divergences: nested DEFSTRUCT compile clause + (setf
  NAME) GF name routing
- `c9310eb` — cl-eval: %bind-params handles &KEY / &AUX / &OPTIONAL
  init-forms

Session arc: 14,180 / 81.72% → 14,618+ / 84.3+% verified (most
divergence fixes post-sweep).

---

## CLAUDE.md "Active Limitations" (#1, #2, #4, #5, #7)

Tracked separately in CLAUDE.md.  Generally bigger structural items:

1. **Last-defun-wins** — still active.
2. **Variable-index ASET bug** — still active, has workaround docs.
3. **Symbol identity** — RESOLVED (`a1327d6`).
4. **YIELD opcode (aa64)** — bare-metal aa64 only; x64 not affected.
5. **cons cells in actor context** — still active, has workaround.
6. **Funcall tag** — RESOLVED earlier session.
7. **defvar init-thunks not run** — still active, has workaround.
