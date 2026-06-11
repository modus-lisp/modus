# Vendored ASDF (single-file build)

`asdf.lisp` is ASDF 3.2.1 as shipped by the Quicklisp bootstrap
(copied from ~/quicklisp/asdf.lisp, 2026-06-10).  UIOP is concatenated
in front (the `;;;; ---- file: uiop/...` section markers delimit it).

Purpose: the "ASDF gauntlet" — drive Modus's runtime EVAL/LOAD through
real-world CL until `(asdf:load-system ...)` works.  Run via:

    sbcl --script mvm/build-generic.lisp     # build /tmp/modus
    /tmp/modus vendor/asdf/gauntlet.lisp     # survey run

The gauntlet runner reads asdf.lisp form-by-form, evals each in a
handler-case, and reports per-form failures with section context —
one pass gives the full landscape of what breaks.

Do NOT reformat or hand-edit asdf.lisp; Modus-side fixes belong in the
runtime, and unavoidable shims belong in gauntlet-shims.lisp loaded
before it.

## Vendor patches

Implementation-detection lists in asdf.lisp enumerate every supported CL
and `(error "ASDF is not supported …")` otherwise.  Modus pushes `:modus`
onto `*features*`, so we add `modus` to those lists.  Each edit is the
minimal token insertion (the upstream line is otherwise verbatim):

- line ~810 `#-(or abcl allegro … sbcl scl xcl)` guarding the
  "ASDF is not supported on your implementation" error → added `modus`.

Add future implementation-type lists (uiop/os, uiop/lisp-build, etc.) the
same way as the gauntlet reaches them, and record them here.

## Gauntlet frontier (as of 2026-06-11, after GC copy_object bounds guard 6eed02a/be4e12c)

The gauntlet runner now reaches **form 54** deterministically and stops
with `READ-ERROR after form 54`.  Two non-fatal `FAILFORM`s precede it —
forms 43 and 44 (the uiop/os `with-upgradability` bodies) signal
`SIMPLE-ERROR "%eval-escape"` but are caught, so the march continues to
the hard read-error at 54.  See the two diagnosis sections below.

### Form 54→55 READ-ERROR — GC/heap-corruption during `#.` read-eval (compiler/translator/GC track)

Diagnosed 2026-06-11.  The hard stop is a **`READER-ERROR` signalled
during the read of form 55**, the uiop/filesystem `directory*` defun
whose body contains
`(append keys '#.(or #+allegro … #+sbcl (when (find-symbol* …) …)))`.
Evidence this is heap-corruption / a stale GC root, NOT a reader-logic
gap (so it belongs to the GC/translator track, not reader/eval/packages):

  - Form 55 reads **fine in isolation** (extracted to its own file and
    read after eval'ing forms 1-54 from asdf.lisp in the same process) —
    `head=WITH-UPGRADABILITY`, all 16 sub-defuns parse.
  - From the **live continuous stream** (after eval'ing forms 1-54 from
    the SAME open stream) the next `read` signals a genuine
    `READER-ERROR` (caught by a `(reader-error (c) …)` clause, so it is
    NOT a mislabeled SEGV-fault array).
  - Binding `*read-suppress*` = T immediately before that read — which
    **disables the `#.` read-eval** (the suppress branch in
    `%read-sharpsign`'s `#.` handler returns NIL without `(eval obj)`) —
    makes the live read **succeed**.  So the fault is in executing the
    `#.` eval mid-read, while the reader's partial form is in flight.
  - The exact `#.` body (`(or #+(or clozure digitool) … #+sbcl (when
    (find-symbol* :resolve-symlinks '#:sb-impl nil) …))`) evals to NIL
    **fine standalone**, even from the post-form-54 live heap state, and
    `find-symbol*` is defined and works.  Allocating `#.` forms mid-list
    (`(a b #.(progn (make-array 5000) 99) c d)`) also read correctly in
    isolation.
  - **Deterministic across GC thresholds** (`MODUS_GC_R14` 20MB → 2GB,
    same form-54 stop) — so it is not a "GC fires at the wrong moment"
    timing race; it is a stale-pointer / mis-forwarded-root defect that
    the `#.` eval's allocation surfaces.
  - **Layout-sensitive** (the classic heap-corruption signature): adding
    one `(setq …)` line to `%reader-error` and rebuilding moved the
    frontier — a minimal probe loop then read form 55 OK, while the
    gauntlet runner (different per-form allocation) still stopped at 54.

Confirmed in-domain primitives that are all correct (don't re-chase):
plain `read-char` across many 4096-byte buffer refills reproduces the raw
file byte-for-byte to 80 000 bytes; `unread-char` immediately after read
across buffer boundaries round-trips; multi-char `#\Name` and `#:foo`
uninterned literals (bare, in lists, and under suppress) all read; the
`#+feature` skip and nested `#+(or …)` skips all read.

Hand-off: the next allocation site to audit is the `#.` path —
`%read-sharpsign` (mvm/cl-reader.lisp ~1439) calls `(eval obj)` while the
reader's in-progress list accumulator and the file-stream's heap `buf`
string are live.  If either is not a GC root during that eval (or the
translator's GC root-scan misses the reader's C-stack temporaries), an
allocation inside the evaled form forwards everything else but strands
the reader's partial state → the next deref signals.  This matches the
translate-x64.lisp comment (~line 3530) that scan-semantics changes move
the gauntlet between forms 36/44, and the older define-package GC-fault
section below.  **Off-limits to the reader/eval/packages seat.**

### OPEN — forms 43/44 `%eval-escape` leak from `(featurep …)` (reader/eval track)

Non-fatal (caught; the march continues to form 54) but a real
runtime-EVAL bug in cl-eval.lisp.  Diagnosed 2026-06-11.

Form 43 (uiop/os `with-upgradability`) defines `featurep`, the `os-*-p`
predicates, `detect-os`, the `os-cond` macro, then ends with a top-level
`(detect-os)` call (sub 10).  That call signals `SIMPLE-ERROR
"%eval-escape"`.  Bisected: `detect-os` → `os-unix-p` → **`(featurep
:unix)` itself leaks**, and `(featurep :unix)` takes only the first
`cond` clause `((atom x) (and (member x *features*) t))` — `(member :unix
*features*)` evaluates to NIL fine on its own.

Key signal: `*%eval-escape-stack*` is **length 0** both before and after
the failing `(featurep :unix)` call, yet the error is `(error
"%eval-escape")`.  That is the *re-raise* path in `%eval-block` /
`%eval-loop` (cl-eval.lisp ~1045): `%eval-escape-pop-if` returned
`:%eval-no-escape` (no matching catcher on an empty stack) and the
handler re-signalled.  So some construct INSIDE the real `featurep`
performs a RETURN-FROM / RETURN / GO whose target block/tag is not the
one the runtime-EVAL `cond`/`and` lowering set up — a tag-mismatch in the
`cond`→block/return-from (or `and`→short-circuit) runtime lowering.

Could NOT reproduce with hand-written reconstructions: a plain `(defun fp
(x &optional (*features* *features*)) (cond ((atom x) (and (member x
*features*) t)) …))` with the full 5-clause cond (incl. `assert`,
`some #'fp`, `every #'fp`, `parameter-error`) evals `(fp :unix)` → NIL
cleanly.  The difference is that the REAL `featurep` is defined via
`with-upgradability` → `defun*` → `(progn (fmakunbound 'featurep)
(declaim (notinline featurep)) (defun featurep …))` inside an
`(eval-when (:compile-toplevel :load-toplevel :execute) …)`.  The
`defun*`/`declaim`/`fmakunbound`/`eval-when` shapes each reproduce-clean
in isolation too, so the trigger is the *combination* as eval'd through
the live form-43 walk.  Next step: dump the macroexpansion of the real
loaded `featurep` (it may be wrapped in an extra block whose name the
inner `cond`/`and` return-from doesn't match) and trace which
`%eval-escape-push`/re-raise fires.  In the reader/eval/packages domain.

### RESOLVED — `define-condition` `%eval-escape` at runtime (forms 34/36/40) (2026-06-11)

`%eval-compound` (mvm/cl-eval.lisp) had no DEFINE-CONDITION branch, so
runtime EVAL of the `with-upgradability` bodies that `define-condition`
(form 34 `simple-style-warning`/`style-warn`, form 36
`not-implemented-error`/`parameter-error`, form 40 the
`deprecated-function-*` cluster) fell through to the funcall path and
signalled `SIMPLE-ERROR "%eval-escape"` (define-condition is not a
function).  The compile-time expander is an `mvm-define-macro` SBCL-side
lambda that can't cross into the image, so the runtime macro table only
knew the *name*.

Fix: added a DEFINE-CONDITION branch dispatching to a new
`%runtime-define-condition` helper that mirrors the compiler.lisp
expander — parses slot-specs into `(name (initargs) initform)`
descriptors, collects `:reader`/`:accessor` names, evaluates a
`(:report (lambda …))` to a real interp-closure (so the registry holds a
callable), then calls `%define-condition` and defines the reader/accessor
functions via `(defun NAME (c) (%condition-slot c 'SLOT))`.  Verified in
isolation: simple parents, `:initarg`+`:reader`, and `:report` lambda all
register and dispatch correctly; `make-condition`/`%condition-typep`/
slot readers work on the registered types.

### RESOLVED — form-26 `READ-ERROR` was TWO reader bugs (2026-06-11)

The old "READ-ERROR after form 26" had nothing to do with form 26's
`#+(and clozure windows-target)` feature suppression (that reads fine).
It was the read of the NEXT form, which contained a `#\Space` character
literal, and Modus's reader signalled `SIMPLE-ERROR "unknown character
name"` on EVERY multi-char char name (`#\Space`, `#\Newline`, `#\Tab`,
`#\Return`, …):

  - **`%read-character` multi-char path (mvm/cl-reader.lisp ~1497)** seeded
    the reversed name accumulator as `(list (char-code ch) (char-code
    next))` = `[CH NEXT]`, but every subsequent char is consed onto the
    FRONT, so the final `nreverse` produced `NEXT CH rest…` — `#\Space`
    became name "pSace", which matched nothing → "unknown character name".
    Fix: seed `[NEXT CH]` so `nreverse` yields `CH NEXT rest…` in order.

  - **`%read-uninterned-symbol` (`#:foo`)** did `read-char` + `unread-char`
    then passed the char to `%read-token-from` as its first-char.  But
    `%read-token-from` consumes first-char AND reads the rest from the
    stream — the unread put the char back so it was processed twice,
    doubling the first letter (`#:foo` → `FFOO`, `#:a` → `AA`).  Fix:
    drop the `unread-char`.

Both are pure correctness wins.  ANSI gate (reader fixes only): 15,300
passed / 111 lost (baseline 15,308 / 103 — within sweep variance under
load).

### RESOLVED — runtime DEFTYPE crash (form 28, `%eval-escape`) (2026-06-11)

`%eval-compound` (mvm/cl-eval.lisp) had no DEFTYPE branch, so runtime EVAL
of `(deftype stamp () '(or real boolean))` (form 28, inside an eval-when)
fell through to the funcall path, tried to *call* DEFTYPE as a function,
and signalled `%eval-escape`.  Added a DEFTYPE branch that registers the
expander in a new `*%runtime-deftype-table*` (NAME→(params . body)) and
returns the name (CLHS).  typep/subtypep don't yet consult the table
(cl-types.lisp is another agent's file), so this is correct-but-inert for
type checks — but it lets DEFTYPE-bearing load streams (uiop, asdf, and the
ansi deftype/subtypep test files) proceed instead of crashing.  ANSI gate
effect: **lost-to-crash 111 → 69 (−42)**, passed 15,300 → ~15,287 (−13:
tests that were false-passing via crash-recovery, or now reach an honest
`typep`-against-unknown-type fail).  Net robustness win per the project's
fails-over-lost / correctness-over-regression guidance.

### NEXT BLOCKER — form-54 heap/GC fault (GC track)

### RESOLVED — GC fault during `define-package` reexport (was the #1 blocker)

The fault during form 11's `:use-reexport :common-lisp` (979-symbol loop,
crash at ~symbol 404 with a wild `#<?N>` condition) was a **missing GC
root**.  The Cheney trampoline's root scan (mvm/translate-x64.lisp,
`emit-gc-trampoline`) scanned the globals alist (`0x10000080`) and the
symbol intern table (`0x10000088`) but **not** two other BSS-resident
heap roots:

  - `0x10000148` — the **keyword intern table** (`init-keyword-table` /
    `%intern-keyword`).
  - `0x10000170` — the **package-by-hash table** (`%init-pkg-by-hash` /
    `%intern-symbol-pkg`).

Both are heap hash-tables whose root slot the GC must forward.  `define-
package`'s reexport interns ~979 symbols and many keywords through the
interpreter; that allocation crosses the GC midpoint (~448MB through the
interp) and fires a real collection.  After the copy completed (gc-count
→ 1) the keyword/pkg tables still pointed into the now-dead from-space, so
the next `%intern-keyword` / `keywordp` deref read a stale pointer and
faulted — the wild `#<?N>` "condition" was that corrupted pointer.

Fix: scan both slots in the trampoline root set (two extra
`mov rax, imm; call scan_word` pairs).  Verified with an early-GC debug
build (`MODUS_GC_R14=<bytes>` knob in build-generic.lisp): the README's
minimal `make-string` reproducer survives 50+ collections, keyword `eq`
identity holds across GC, and the gauntlet advances 11 → 26.  ANSI gate:
15,251 → 15,260 passed, 122 → 112 lost-to-crash.

### `&REST` :internal status — FIXED (shared layer)

The first sub-blocker was that `find-symbol* "&REST"` signalled because
`&REST` was :INTERNAL (not :external) in Modus's COMMON-LISP package.
`%export-standard-cl-symbols` exports it at boot, but a later boot-time
read of a `&rest`-bearing form demotes it back to :internal (every OTHER
lambda-list keyword — `&BODY`/`&OPTIONAL`/`&KEY`/`&AUX`/`&WHOLE`/
`&ALLOW-OTHER-KEYS`/`&ENVIRONMENT` — stays :external; only `&REST` flips).
The exact demotion site was not pinned down, but the fix is idempotent:
**`%install-runtime-cl-macros` (mvm/runtime-cl-macros.lisp) now re-runs
`%export-standard-cl-symbols` at its tail.**  It runs at the end of boot in
BOTH build-generic and build-ansi-test, so `&REST` is now :external
everywhere (also a latent ANSI-conformance fix — CLHS requires it).
Verified: `(find-symbol "&REST" 'cl)` => `:external`.

### TRUE NEXT BLOCKER — fault during `define-package` reexport (allocation/GC layer)

With `&REST` fixed, form 11's `:use-reexport :common-lisp` path now runs
further and faults.  The fault is NOT in the package layer — every uiop
helper (`import*`, `export*`, `ensure-imported`, `find-symbol*`) works
correctly over all 979 CL externals in isolation; Modus's own
`intern`+`export` over all 979 into a CL-using package works too.  The
fault appears only **cumulatively**, at ~symbol 404, with `(%gc-count)`
still 0.  The signalled "condition" is a wild 2-element vector whose
slot 0 is a corrupted pointer printed `#<?N>` (N varies run-to-run: 47,
79, 111, 255) — the signature of heap corruption / a stale pointer, i.e.
an allocation/GC bug, NOT a Lisp-level error.

Minimal reproducer (no uiop needed), in `mvm/gc.lisp`/translator terms:

```lisp
;; <generic-binary> this.lisp
(handler-case
    (let ((i 0))
      (loop (when (>= i 30000) (return :ok))
            (make-string 20000)            ; ~460MB total -> crosses GC midpoint
            (setq i (+ i 1))))
  (t (c) (write-string-serial "CAUGHT")))
(write-object (%gc-count))                 ; => 1, then a fault was caught
```

Result: `gc-count` increments to exactly **1** (the Cheney copy completes
and bumps the counter at gc.lisp:327), THEN a fault is signalled and
caught.  So GC's copy finishes but **resumption faults** — a live root
(env / args / `*%eval-escape-stack*` / a large object near a space
boundary) was not forwarded, leaving a stale from-space pointer that
faults on next deref.  Notes:
  - With small allocations (`make-string 1000`) the first several GCs
    survive (probe reached gc=4); with large objects (`make-string 20000`)
    the FIRST GC faults.  Suggests the Cheney copy of large objects (or
    objects landing near the from/to boundary) is the trigger.
  - The `define-package` fault shows `gc-count`=0 at the crash, so it may
    be a SECOND, distinct bug: an `alloc-obj`-without-preceding-`gc-check`
    site (cf. MEMORY `reference_make_closure_gc_check`) that writes past
    R14 and corrupts an object header.  Both live in the GC / translate-x64
    layer (off-limits to the packages agent) and should be handed to the
    compiler/translator track.

This is the wall: until GC survives a collection triggered from inside a
loaded script's call graph, define-package's 979-symbol reexport cannot
complete and will corrupt global state when it faults mid-way.

### Runtime-EVAL interp bug (still open, lower priority)

`(let ((x ..)) (tagbody BODY))` as the **last form inside a simple `loop`**
infinite-loops (the loop restarts instead of continuing).  Worked around in
the do-symbols runtime macros by using `progn` instead of `tagbody` for the
body, but the underlying `%eval` LOOP/LET/TAGBODY interaction in
mvm/cl-eval.lisp should be root-caused (it breaks any runtime `do-symbols`
whose body uses `go`).

### Gauntlet runner improvements

`gauntlet.lisp` now: (1) points at THIS worktree's asdf.lisp, and (2) on
FAILFORM, prints the condition (type-name + format-control if it's a
recognised `%condition-p`, else the raw object) after ` :: ` so the
failure mode is visible without a separate probe.
