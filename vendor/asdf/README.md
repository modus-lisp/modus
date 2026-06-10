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

## Gauntlet frontier (as of commit acf0850)

Reaches form 26 (uiop/utility section), 1 fail + a downstream read desync.
Both stem from the SAME root: form 11 (`define-package :uiop/common-lisp`)
fails inside its `:use-reexport :common-lisp` path.

- **FAILFORM 11 — reexport `&REST`**: `ensure-package`'s reexport loop
  re-exports every external CL symbol.  `find-symbol* "&REST" pkg` SIGNALS
  because `&REST` is INTERNAL (not external) in Modus's COMMON-LISP package,
  so it isn't inherited by a use-CL package and find-symbol* hits its
  error path.  Root: `%install-runtime-cl-macros` (macro lambda-lists
  contain `&rest`) re-interns `&REST` into CL as internal AFTER
  `%export-standard-cl-symbols` ran, clobbering its :external status.
  Re-running `%export-standard-cl-symbols` late fixes `&REST`'s status —
  BUT then the reexport proceeds further and the symbol-rehoming /
  shadowing-import machinery (rehome-symbol, nuke-symbol, ensure-symbol
  recycle) corrupts reader state -> READ-ERROR after form 11.  So the
  real work is making define-package's full reexport+rehome path correct,
  not just the `&REST` status.  `&BODY`/`&OPTIONAL`/`&KEY`/`&AUX` are
  already :external and reexport fine.

- **READ-ERROR after form 26**: a cascade of the above — uiop/utility
  `:use`s uiop/common-lisp, whose half-built reexport leaves symbol
  resolution inconsistent.  Form 27 (`ensure-function`/`access-at`) reads
  fine in isolation but desyncs in cumulative state.  Likely resolves once
  form 11 succeeds.

### Runtime-EVAL interp bug found (not yet fixed)

`(let ((x ..)) (tagbody BODY))` as the **last form inside a simple `loop`**
infinite-loops (the loop restarts instead of continuing).  Worked around in
the do-symbols runtime macros by using `progn` instead of `tagbody` for the
body, but the underlying `%eval` LOOP/LET/TAGBODY interaction in
mvm/cl-eval.lisp should be root-caused (it breaks any runtime `do-symbols`
whose body uses `go`).
