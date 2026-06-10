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

## Gauntlet frontier (as of 2026-06-10, after GC-root fix)

Reaches form **26** with **fails=0** (define-package for UIOP/PACKAGE,
UIOP/COMMON-LISP and UIOP/UTILITY all complete; INPKG markers print for
forms 2/12/17).  New frontier is a `READ-ERROR after form 26` — a reader
desync that is NOT GC-related (separate track).

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
