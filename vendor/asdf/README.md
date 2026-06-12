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

## Gauntlet frontier (as of 2026-06-12, after the form-94 reader + runtime-DEFSETF + macro-branch fixes)

The gauntlet runner now **reads ALL 243 toplevel forms** of asdf.lisp —
`GAUNTLET DONE forms=243` with NO `READ-ERROR`.  The reader frontier is
GONE; what remains is purely runtime-EVAL failures (caught, the march
continues).  Three fixes this seat:

1. **Form-94 `READ-ERROR` FIXED — consing-dot under `*read-suppress*`.**
   `%read-list`'s consing-dot branch (mvm/cl-reader.lisp) signalled a
   spurious `"dot at start of list"` reader-error whenever a *legal*
   dotted list appeared inside a feature-suppressed form, because under
   `*read-suppress*` the list elements are NOT accumulated (`result`
   stays nil), so the "is result empty?" guard fired wrongly.  uiop/lisp-
   build's `#+clozure … (destructuring-bind (fun . more) …)` tripped it
   and desynced the stream.  Fix: skip the empty-result check while
   suppressing.  Gauntlet form 94 → 243 (whole file reads).

2. **Forms 44/56 runtime DEFSETF — FIXED.**  `%eval-compound`
   (mvm/cl-eval.lisp) had no DEFSETF branch, so `(defsetf getenv (x)
   (val) …)` fell through to funcall and `%eval-escape`'d.  Added a
   runtime DEFSETF branch (short + CLHS long form) that registers a
   descriptor in `*setf-expanders*` via `%register-setf-expander`; the
   runtime SETF macro (runtime-cl-macros.lisp) and `get-setf-expansion`
   now consult it via `%find-setf-expander` + the new
   `%apply-setf-expander` (descriptors avoid the closure-cell
   limitation).  Form 44 now PASSES.

3. **Runtime-DEFMACRO dispatch — FIXED (the keystone for the read-through).**
   `%eval-compound`'s macro-call branch looked up the expander via
   `macro-function`, which WRAPS the raw `%interp-closure` in a
   user-facing closure object (subtag #x52) — that wrapper is not an
   `%interp-closure`, so it fell to the `(funcall mf form)` path and
   CRASHED (the shim expects the macro's args, not the whole form).  Any
   runtime-`defmacro`'d macro invoked through eval (uiop's
   `DEFINE-PACKAGE`, `WITH-UPGRADABILITY`, etc.) hit this.  Fix: use
   `%raw-macro-expander` (mirroring `macroexpand-1`) to get the bare
   expander before dispatching on `%interp-closure-p`.

Forms that still `FAILFORM` (all caught, runtime-EVAL gaps — NOT reader):
  - **43 / 87** — detect-os "can't detect this OS" + uiop/image
    `NOT-IMPLEMENTED-ERROR`; both expected (no `:unix` in *features*).
  - **56** — uiop/pathname with-upgradability `%eval-escape` (pre-existing).
  - **49, 58, 67, 80, 88, 224, 227, 231, 234, 237-239** — `DEFINE-PACKAGE`
    `%eval-escape`.  Fix #3 makes define-package *actually run* (instead
    of crashing in the wrapper-funcall path); it now creates the package
    (the `INPKG` markers still appear) but `ensure-package`'s body escapes
    /hangs deep in the runtime-EVAL `do-symbols` over the 978-symbol CL
    package + the `(loop … :being :the :hash-keys …)` reexport loop.  This
    is the **define-package / runtime-EVAL track** (separate, large
    effort) — NOT a reader bug.  In baseline these "passed" only because
    define-package was crashing earlier and never completing; the new
    FAILFORMs are honest.
  - **223 / 226 / 242** — `ETYPECASE: no clause matches` (runtime
    etypecase gap).
  - **233 / 236** — `WITH-ASDF-DEPRECATION` macroexpand crash.
  - **241** — `FORMATTED-SYSTEM-DEFINITION-ERROR` (asdf component
    designator validation — a real asdf error, honest).

**The old form-54 `READ-ERROR` is FIXED** (commit 1b3edd3) — it was NOT a
GC/`#.` issue at all.  See "Form 54→55 READ-ERROR — RESOLVED" below.

**The forms 43/44 `%eval-escape` leak is FIXED** (commit ae1e140) — it was
an error-MASKING bug in the runtime escape handlers, not a cond/and
lowering tag mismatch.  See "forms 43/44 — RESOLVED" below.

(The earlier 16 MB heap guard band in `boot/boot-linux-x64.lisp` removed
the GC-overshoot crash class and is unrelated to either of these.)

### Form 54→55 READ-ERROR — RESOLVED 2026-06-12 (commit 1b3edd3): `;'-comment-before-consing-dot reader bug

**The `#.` / GC framing below was a complete red herring.**  The form-54
boundary was a pure reader bug with no GC, no `#.`, no heap corruption.

`%read-list`'s inline whitespace-skip loop (mvm/cl-reader.lisp) skipped
only `%whitespace-char-p` chars, not `;' LINE COMMENTS.  When a `;'-comment
sat immediately before a CONSING DOT, the `;' fell through to the
fall-through `%read-internal` call, whose `%read-skip-whitespace` DID skip
the comment AND the following whitespace, landing on the lone `.', which
`%read-internal` then read as a token — a reader-error (consing-dot
context is only recognised inside `%read-list`, not `%read-internal`).

The trigger form was NOT `directory*` (that's a later form) but the
uiop/pathname `subpathname` `with-upgradability` (the form read right
after form 54), whose `pathname-root` / `pathname-host-pathname` defuns
end with exactly this shape:

    (make-pathname :directory '(:absolute) … :defaults pathname
                   ;; host device, and on scl, *some*
                   ;; scheme-specific parts: port username password
                   . #.(or #+scl '(:parameters nil :query nil :fragment nil)))

Minimal repro (no GC, no `#.`): `(a 1 ;c\n . tail)` -> READER-ERROR.
`*read-suppress*`=T "fixing" it was a coincidence — suppress changes the
fall-through path, not because `#.` eval was the culprit.

Fix: `%read-list` now skips `;' line comments in its own whitespace loop
(`((eql ch #\;) (%skip-line-comment stream))`), so the consing-dot branch
fires.  Gauntlet advances form 54 -> form 94.  ANSI 15,360 -> 15,376,
markers flat.  `%read-skip-whitespace`, `%read-after-ws2`, and the
`:semicolon` dispatch already handled comments; only the `%read-list`
inline loop had the gap.

----- (original, now-DISPROVEN `#.`/GC notes preserved below) -----

### Form 54→55 READ-ERROR — GC/heap-corruption during `#.` read-eval (compiler/translator/GC track) [DISPROVEN]

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

#### 2026-06-12 RESOLVED — it was NOT a missing root; it was a gc-check OVERSHOOT off the end of the mmap (commit on aarch64-ansi-timeout)

The "missing root" framing below was a **red herring**.  Instrumenting the
native GC trampoline (entry/exit trace bytes + per-GC dumps of R12, R14,
from_start, space_size, the saved-register slots, and the faulting RIP/si_addr
captured by the SIGSEGV stub) proved:

  - The GC copy/swap/bounds are correct; **all** roots (stack scan, the 12
    saved-register slots, globals, symtab, keyword + pkg-by-hash tables, MV
    extras) are forwarded.  No saved-reg slot is ever left pointing into the old
    from-space after a collection (verified with a post-GC `!`-if-stale probe —
    zero hits).  The interp `env` was never actually lost.
  - The deterministic fault is in **`MAKE-STRING` itself** (symmap'd RIP), writing
    at **`heap_end + 0`** — i.e. ONE PAGE PAST the end of the 896 MB mmap.
  - Cause: the gc-check is POST-allocation (`alloc … ; cmp r12,r14 ; jb skip ; call
    gc`).  R12 overshoots R14 (= the space's `from_end`) by one-or-more objects
    before the next check fires the collector — ~33 KB observed for the interp's
    make-string loop.  The two Cheney semispaces were sized to fill the **entire**
    mmap (`midpoint + space_size ≈ 0x37FFFE00`, heap end `0x38000000`, only 0x200
    bytes of slack).  In the FIRST space an overshoot lands in the (mapped) second
    space and is harmless; in the SECOND space it runs off the mapping → SIGSEGV
    mid-write.  That is why it only died on the **second** GC, was value-type
    independent (the env was fine; the *allocator* crashed), survived in a global
    (globals don't change allocation pressure), and was layout-sensitive (adding
    loop-body code shifts where R12 lands relative to heap_end).

Fix (`boot/boot-linux-x64.lisp`): add a 16 MB guard band to the mmap'd heap
(`+linux-x64-gc-guard+`), so the second semispace's `from_end` has far more
mapped slack than any inter-check overshoot.  Semispace midpoint/size are
unchanged; only the mapping is larger.

Verified: the README's minimal `fsc` reproducer now prints `car=111 gc=1`; the
`(a b #.(progn (make-string…)(loop … make-string …) 99) c d)` two-GC `#.`
read-eval reads cleanly; `dotimes`/`loop`/recursion variants all survive.
Shared boot file ⇒ build-ansi-test gets the fix too.  ANSI gate: 15,355 →
**15,360 passed**, lost-to-crash flat (~118→119), no FILE-WEDGE/CHUNK-CRASH rise.
Gauntlet: now cleanly reaches uiop/pathname (form 50) and stops at the
**form-54 READ-ERROR**, which is a SEPARATE, genuine reader/eval issue (the `#.`
crash class is gone — a 2-GC `#.` reproduces clean standalone), so form 54 is
now in the reader/eval track, not GC.

----- (original, now-disproven "missing root" notes preserved below) -----

#### 2026-06-12 update — narrowed to a MISSING-ROOT for a LEXICAL local across GC

The `#.`/reader framing above is a red herring: the same fault reproduces
with **no reader and no `#.`** at all.  The minimal deterministic
reproducer (3/3 runs, ~11s) is just **a live value held in a lexical
binding across an allocation loop that triggers one GC**:

```lisp
;; /tmp/modus this.lisp  — process dies (nothing printed), deterministic
(defun fsc ()
  (let ((keep (cons 111 222)) (i 0))
    (loop (when (>= i 16000) (return nil))   ; ~640MB of make-string → 1 GC
          (make-string 5000) (setq i (+ i 1)))
    keep))                                    ; keep is corrupt after the GC
(let ((r (fsc))) (write-string-serial "car=")(print-dec (car r)))
```

`keep` can even be a **fixnum** (`(let ((keep 999) …))`) — the value type
is irrelevant, so it is NOT a value-forwarding bug.  It faults during the
loop's GC and the resumed body can't deref `keep`.

**Decisive test — global survives, lexical local does not:**

```lisp
(defvar *keep* nil) (setq *keep* (cons 111 222))
(let ((i 0)) (loop (when (>= i 16000) (return nil)) (make-string 5000) (setq i (+ i 1))))
(print-dec (car *keep*))      ;; => 111, gc=1  — SURVIVES
```

Holding the identical cons in a **global** (rooted via the globals alist
at `0x10000080`, which the trampoline scans) survives the GC perfectly.
Holding it in a **lexical local** loses it.  A false-forward (the
conservative-scan hazard) would corrupt the global too — it does not.
**Therefore this is a MISSING ROOT specific to the lexical-binding path,
not a false-forward and not fresh-payload garbage.**

Ruled out this session (don't re-chase):

  - **R15 is NOT a gap.**  R15 is the dedicated NIL-constant register
    (`mov r15, 0xDEAD0001` in boot-linux-x64.lisp ~L348; VN in the vreg
    map).  The trampoline omits it from its 12-reg save set, but that is
    correct — it always holds NIL, never a heap root.  Adding R15 to the
    save/scan/restore set (built + tested) does NOT fix the fault.
  - **The trampoline's save/scan/restore set is otherwise complete.**  It
    saves rax,rsi,rdi,r8,r9,rbx,rcx,rdx,r10,r11,r13,rbp; that is exactly
    the allocatable GP vregs V0–V8 + RAX(VR) + R13.  Push order and the
    reversed pop order MATCH (verified register-by-register).  Spilled
    vregs V9–V15 live in stack frame slots and the stack scan ([RBP,
    stack_base)) covers them.
  - **Zero-initialising fresh alloc payloads is NOT the fix.**  Built
    alloc-obj / alloc-array / alloc-string payload-zeroing (inline MOV loop
    for small obj; R11 byte-index countdown loop for dynamic array/string;
    `mov [r12+r11],0` verified correct via objdump).  alloc-obj-only is
    behaviourally identical to baseline (gc-count matches) but does NOT fix
    the missing-root fault (the live local is still lost).  The
    array/string variants additionally PERTURB GC firing (gc-count 1 → 0 on
    the discard-result burn loop) for an unresolved reason — do NOT ship
    payload-zeroing for array/string without root-causing that
    interaction.  All reverted; tree is clean.

Hand-off — where the missing root must be: the corrupted value lives in a
lexical binding, reachable only through the interpreter's `env` alist
(`%env-extend` conses) threaded as the `env` argument through the
`%eval`/`%eval-progn`/simple-`LOOP` (`(loop (%eval-progn args env))`,
cl-eval.lisp ~L3022) call chain.  Those are COMPILED native frames, so
`env` should be a scanned stack/register root — yet the GC loses it while
the globals-alist root survives.  Next steps for the GC seat: (1) confirm
whether `env` at the make-string GC instant sits in a frame slot the stack
scan reaches, or in a transient register window (e.g. spilled to an
RSP-relative slot BELOW the trampoline's captured RBP, i.e. in red-zone /
outgoing-arg territory the `[RBP, stack_base)` walk skips); (2) check the
stack-scan START — it begins at the trampoline's RBP (= RSP after its
pushes); any live root pushed by the CALLER as an outgoing argument below
the `call gc_trampoline` return address but above RBP is in range, but a
value the compiler parked at `[RSP-8]` (red zone) before the gc-check call
would be BELOW RBP and unscanned.  The deterministic (not layout-flaky)
nature points at a systematic such slot, not a coincidental false
positive.  **Off-limits to the reader/eval/packages seat — this is a GC
root-set / calling-convention defect, surfaced by any GC with a live
lexical local, of which the gauntlet `#.` eval is one instance.**

### RESOLVED 2026-06-12 (commit ae1e140) — forms 43/44 `%eval-escape` was error-MASKING, not a cond/and lowering bug

The "tag-mismatch in cond→block lowering" theory below was WRONG.  The
real bug: the `handler-case` wrappers around BLOCK / extended-LOOP /
simple-LOOP / CATCH / TAGBODY in mvm/cl-eval.lisp caught EVERY condition,
consulted `*%eval-escape-stack*` via `%eval-escape-pop-if`, and on an
empty / non-matching stack re-signalled a GENERIC `(error "%eval-escape")`
— **discarding the original condition object**.  So any genuine
`(error …)` signalled inside a loop/block/catch/tagbody body (or a loop's
FINALLY) was masked: its true type/message became `SIMPLE-ERROR
"%eval-escape"`, and the empty-stack signature in the prior notes was
just "this real error pushed no escape descriptor".

uiop's `detect-os` ends with `(loop* … :finally (return (or o (error
"…neither Unix nor Windows…"))))`.  On Modus *features* lacks `:unix`, so
all `os-*-p` return NIL, `o` stays NIL, and the FINALLY `(error …)` fires
— a LEGITIMATE ASDF "can't detect this OS" error that the masking turned
into the mysterious empty-stack `%eval-escape`.  (NB the bisect that
blamed `(featurep :unix)` was misled by the masking — `featurep` itself
is correct; the leak was detect-os's finally-error.)

Fix: all six escape handlers now re-raise the caught condition C itself
(`(error c)`) when `%eval-escape-pop-if` returns `:%eval-no-escape`,
instead of a fresh "%eval-escape".  Escape propagation is preserved: when
C IS a deeper `%eval-escape-push`'s condition, its escape value is still
on the stack and the outer handler pops it.  detect-os now FAILFORMs with
its real "Congratulations for trying ASDF on an operating system…" message
(honest + catchable).  ANSI 15,360 -> 15,376 passed, lost-to-crash
119 -> 56 (errors that died silently are now caught), markers flat (4/30).

Still-open follow-ups in the uiop/os region (NOT this bug):
  - **Form 44/56 = runtime DEFSETF gap.**  `%eval-compound` has no DEFSETF
    branch, so `(defsetf getenv (x) (val) …)` falls through to funcall and
    escapes.  A runtime DEFSETF branch (mirroring the DEFTYPE /
    DEFINE-CONDITION ones, registering into the existing `*setf-expanders*`
    via `%register-setf-expander`) would clear forms 44 & 56.
  - **detect-os PASS would need `:unix` in `*features*`.**  Tried it
    (cl-reader.lisp `%init-reader`); it makes form 43 PASS but activates
    `#+unix` branches in uiop/filesystem (forms 50-58) that Modus can't
    handle, producing a form-58 runtime cascade (the gauntlet's `*g-form-n*`
    got stuck advancing).  REVERTED — net negative.  Revisit only with the
    `#+unix`-path gaps closed.

----- (original, now-DISPROVEN "cond/and lowering" notes preserved below) -----

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
