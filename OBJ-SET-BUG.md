# OBJ-SET Bug — RESOLVED (2026-04-17)

Not actually an OBJ-SET bug. The "store doesn't persist" symptom was misleading.

## Actual root cause

`defvar` init-thunks are not run at boot in this runtime (`init-all-globals` is skipped — see comment in `cl-reader.lisp`). So `*pkg-tag*` and `*sym-tag*`, declared in `cl-packages.lisp` as

    (defvar *pkg-tag* 987654321)
    (defvar *sym-tag* 123456789)

were both NIL at runtime. Every package and CL symbol therefore had `car=NIL` (since `%make-package-object` and `%make-cl-symbol` cons their data with the tag).

`%cl-sym-p` had a fallback for the uninit-tag case (it discriminates by checking the data array length is 3). `%pkg-p` did not — it just did `(eql (car x) *pkg-tag*)`, which when `*pkg-tag*=NIL` returns T for *any* cons with NIL car, including all CL symbols.

## Why this manifested as GENTEMP.4

`gentemp.4` calls `(gentemp "" (make-symbol "GENTEMP-TEST-PACKAGE"))`. Inside gentemp, `%resolve-package` checks `%pkg-p` first — and on the make-symbol result, `%pkg-p` returned T (false positive). So the symbol was treated as the package, never going through `find-package`. The symbol's slot 1 ended up holding the symbol-treated-as-pkg, not the real package. `(eql (symbol-package sym) pkg)` → NIL.

Tests `gentemp.1`/`.2`/`.3`/`.5`/`.6` happened to work because their package designators were not symbols (they were `*package*`, strings, or characters), so the `%pkg-p` false positive was never triggered.

## Fix

Two lines in `%init-packages` (`mvm/cl-conditions.lisp`):

    (setq *pkg-tag* 987654321)
    (setq *sym-tag* 123456789)

After the fix: 17,568/17,568 ANSI tests pass.

## Lessons

- Predicates that compare `(car x)` against a tag value must guard against the uninit case, OR the tag must be guaranteed non-NIL by an init function.
- The "OBJ-SET register clobber" theory was wrong because OBJ-SET *was* writing the symbol; the bug was that the wrong object was being passed in. Static analysis of the encoding was a red herring.
- All the previously-tried workarounds (cons-based symbol layout, inline aset, etc.) failed because they didn't address `%pkg-p`.
