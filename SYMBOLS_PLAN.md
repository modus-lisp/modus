# SYMBOLS_PLAN — bring CL-correct symbol identity to Modus

## Goal

Replace the **unified-by-name-hash** symbol model with **per-package
distinct symbols** matching CLHS 11.1.

> CLHS: `(intern "X" pkg1)` and `(intern "X" pkg2)` produce *different*
> symbol objects unless one package `:use`s another that exports X.

## Why now

- Modus's purpose is to run unmodified CL.  Trading CL semantics for
  ergonomics is a contradiction.
- The unified model breaks the print-symbol test cluster (DS4-stamped
  home package leaks across all reads of the same name) and any other
  test that distinguishes same-named, package-distinct symbols.
- Fix is structural; agent-level work around the printer / accessibility
  check can't paper over it.

## Current state (pre-refactor)

- `compile-quote` emits `%intern-symbol-pkg(name-hash, pkg-hash)` for
  every `'foo` literal.
- `%intern-symbol-pkg` (mvm/prelude.lisp:1411) consults a single global
  table keyed by **name-hash only**.  First call stamps the home package
  into slot 1 of the symbol; subsequent calls with a different pkg-hash
  return the same symbol unchanged.
- `%pkg-find-sym` (cl-printer.lisp:481) walks the *package*'s symtab via
  `%do-symbols-fn`, but the symbol may have been allocated under a
  different package and only registered in that one — accessibility
  returns NIL even when CLHS would say T.
- Symbol-package = first stamp; never updated.
- `(eq cl-test::x ds4::x)` returns T (unified).

## Target state (post-refactor)

- `intern "X" pkg` creates a fresh symbol in pkg if not already there,
  else returns the existing one.  Symbol's home package = pkg.
- `find-symbol "X" pkg` walks pkg's internal + external, then each
  `:used` pkg's external in order.  Returns first eq match + access-type.
- `(eq cl-test::x ds4::x)` returns NIL.
- `(eq (intern "X" pkg) (intern "X" pkg))` returns T (same package
  identity preserved).
- Printer's accessibility check works: `find-symbol name *package*`
  returns the symbol → no qualifier; returns NIL or different sym →
  emit "pkg::sym".

## Phases

### Phase 1 — Per-package intern path

**Files**: `mvm/prelude.lisp` (`%intern-symbol-pkg`), `mvm/cl-packages.lisp`
(`intern`).

- Replace the "first-stamp-wins" behaviour with a per-package check:
  - If pkg-hash is 0 (uninterned literal), allocate a fresh symbol with
    pkg=nil.  Don't share.
  - If pkg-hash > 0, look up pkg-hash in `*pkg-by-hash*`.  If pkg is
    found, check pkg's internal/external symtabs for the name.  Hit →
    return that symbol.  Miss → allocate a new symbol, set home=pkg, add
    to pkg's internal table, return.
  - Optionally keep a name-hash → symbol global cache **per package**
    (alist or table) to make intern O(1) instead of walking the symtab.
- `intern` (cl-packages.lisp) goes through this path with the supplied
  pkg-or-name.

**Verification**:
- Probe: `(eq (intern "X" "DS4") (intern "X" "CL-TEST"))` → NIL after.
- Probe: `(eq (intern "X" "DS4") (intern "X" "DS4"))` → T.
- Sweep: expect transient regressions in tests that depend on cross-pkg
  eq.

### Phase 2 — find-symbol with use-list walking

**File**: `mvm/cl-packages.lisp` (`find-symbol`, `%pkg-find-sym`).

- Walk pkg's internal + external tables.
- For each pkg in `(%pkg-use-list pkg)`, walk that pkg's external.
- Return (values sym access-type).
- Access-type: `:internal` / `:external` / `:inherited` / NIL.

**Verification**:
- Probe: `(find-symbol "CONS" "CL-USER")` returns the same symbol as
  `(find-symbol "CONS" "CL")`, both with access-type T but kind
  differs (`:external` vs `:inherited`).
- Sweep: find-symbol tests should newly pass.

### Phase 3 — compile-quote read-package routing

**Files**: `mvm/compiler.lisp` (`compile-quote`).

- The current `(emit-ir :call "%INTERN-SYMBOL-PKG" 2)` keeps its
  signature.  The change is in the runtime helper — once Phase 1 is in,
  literal `'foo` resolves via real per-package intern.
- Audit: `compile-quote` should pass the SBCL read-time package's hash.
  Already does.  Verify special handling of uninterned `'#:gensym`
  symbols still works (pkg-hash = 0).

**Verification**:
- Probe: A test file in `(in-package :cl-test)` reads `'foo` → at
  runtime the symbol's home is CL-TEST, not DS4.
- print-symbol cluster: PRIN1.SYMBOL.1/2/3 + PRINT.SYMBOL.PREFIX.5/6/etc.
  start passing.

### Phase 4 — Decontaminate code that assumes unification

**Files**: TBD — needs audit.

- Grep for `(eq sym1 sym2)` patterns across `mvm/*.lisp` where sym1 /
  sym2 might come from different packages.
- Grep for `string=`-on-symbol-names that the unification made
  redundant — those will need to come back for any genuinely
  cross-package compare.
- Update `feedback_eq_works_on_symbols.md` to clarify scope: kernel
  symbols within `modus.mvm` are still trivially `eq`; cross-package
  user-source symbols are not.
- Update / retire `project_unify_symbols.md` — its premise reverses.

**Verification**:
- Full sweep against pre-Phase-1 baseline.  Net ANSI change must be
  non-negative.

## Risks & mitigations

- **Cascade regression on Phase 1 commit**: Many existing tests pass
  because `(eq 'foo 'foo)` works across packages.  If those tests are
  cross-package and now eq returns NIL, they break.
  - *Mitigation*: feature flag (`*per-package-symbols*`) to gate the
    new intern behavior.  Default off, flip after Phase 2 verification.
- **Modus internal code in `modus.mvm` package may assume unification**.
  Kernel symbols are all in one package though, so `eq` within the
  kernel still works — only user-source crosses packages.
- **defpackage processing order at build time**: DS4 declares `:intern X
  Y Z` before CL-TEST is created.  With per-package intern this stops
  stealing X for everyone.  But any test that *needs* DS4 to own X by
  identity already passes — the change should be transparent for those.
- **Performance**: Per-package symtab walking on every intern is slower
  than global name-hash lookup.  If profile hits a wall, add per-package
  hash table.

## Concrete deliverables per phase

| Phase | Commit message prefix | What's in it |
|---|---|---|
| 1 | `cl-packages: per-package distinct intern` | new intern + per-pkg cache, feature flag off |
| 1.5 | `cl-packages: flip per-package-symbols flag on` | enable the new path |
| 2 | `cl-packages: find-symbol walks use-list per CLHS 11.1.2.4` | proper find-symbol |
| 3 | `compiler: compile-quote routes through per-package intern` | tested e2e |
| 4 | `audit: drop unification-dependent eq comparisons` | + memory note updates |

## Out of scope

- Symbol property lists (CLHS 5.2.4) — separate gap; not touched here.
- Package-locks (SBCL extension) — not CL-required.
- shadowing / unintern / rename-package — existing impl stays; we only
  touch the intern / find-symbol pair.

## Tracking

- See commits chained from `aarch64-ansi-timeout` after this plan
  document lands.
- Verification sweeps go in `tmp/sweep-symbols-phase-N.log` per phase.
