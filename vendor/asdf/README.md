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
