# GATE #215 — %UNRESOLVED-FN stub lookup used an integer key in a string-keyed table

## The bug

`emit-bytecode-for-ir`'s `:call` arm resolved an unknown callee to the
`%UNRESOLVED-FN` stub via

```lisp
(gethash (compute-name-hash "%UNRESOLVED-FN") *functions*)
```

`*functions*` is `(make-hash-table :test 'equal)` keyed by the function
**name string** (bare name, plus `"PKG::NAME"` since #211).
`compute-name-hash` returns an **integer**.  The lookup therefore could not
hit under any input, and every unresolved call fell through to the literal
fallback target `0`.

Offset 0 is not a trap and not a halt: `global-offset` is initialized to 0
and incremented per function, so **offset 0 is the first real function's
entry point**.  Each unresolved call jumped into an arbitrary function with
whatever arguments the caller had pushed, and returned its value.  That is
the `:li-func offset-0` garbage-execution class named in CLAUDE.md, live in
the shipping gate image.

Scale: **1202 unresolved calls to 84 functions** in the x64-linux gate image.

## The fix

Use the string key.  Confirmed at build time by the report line:

```
BASE  === 1202 unresolved calls to 84 functions (resolve to %%unresolved-fn → nil) ===
NET   === 1202 unresolved calls to 84 functions (→ %%unresolved-fn @ offset 415295 → nil) ===
```

The report text itself was part of the defect: it *asserted* the safe outcome
without checking it, which is why this survived in plain sight.  It now prints
the resolved offset, or shouts if the stub is ever missing.

Preconditions verified rather than assumed:
- no tree-shaking pass — the only `remhash` fires when a function fails to
  compile, and the build has zero `SKIP bytecode` lines, so the stub exists;
- `%fn-register-info` **always** registers the bare name, so #211's fold did
  not move the key out from under this lookup.

## Gate — 64 shards, [10001,27800]

```
BASE main e041372   passed=17479  CHUNK-CRASH=0  FILE-WEDGE=30
NET  #215           passed=17492  CHUNK-CRASH=0  FILE-WEDGE=30
per-ID: gained=16  lost=3
```

Deterministic recheck of the 3 losses (3 reps each, isolation): all 3 fail on
**both** binaries.  Re-running the two containing shards in context:

- shard 13349..13627 — `13448` is **not** reproducibly lost (the re-run gains
  `13445` instead); it is the documented flaky.  +7 gains reproduce.
- shard 16976..17254 — `16991` and `17003` **are** reproducibly lost.

So: **16 gained, 2 really lost.**

## What the +13 actually means — NOT a conformance gain

All 18 changed tests have the same shape: they call a helper function that
**does not exist in the image**.

- `elt-v.6` → `(elt-v-6-body)`, defined only in `auxiliary/ansi-aux.lsp`,
  which the build skips wholesale (read-time error on
  `#.(coerce … string)`; see `reference_array_string_fill_kwarg`).
- `ceiling.7` → `(ceiling.7-fn)`, **commented out** in
  `auxiliary/ceiling-aux.lsp:53`.

So every one of them was an unresolved call.  Under BASE the garbage jump to
offset 0 returned a consistently non-NIL value; under NET the stub returns
NIL.  The sign of the change is therefore decided purely by the test's
expected value:

| expected | BASE (garbage, non-NIL) | NET (NIL) | delta |
|----------|------------------------|-----------|-------|
| `nil`    | FAIL                   | PASS      | +16   |
| `t`      | PASS (spurious)        | FAIL      | −2    |

**None of these 18 tests exercises the function it names.** The 16 gains are
right-answer-for-the-wrong-reason exactly as much as the 2 losses were
false-green.  The honest read is that the +13 headline is an artifact of
expected-value polarity, and the real conformance work is making
`ansi-aux.lsp` load (known; a partial load previously cost −2862, so the
override pattern is the right approach, not a rescue).

Crash markers are unchanged (CC=0, FW=30), so this did not close a wedge
either.

## Why land it anyway

The value is correctness, not score: 1202 call sites stop jumping into an
arbitrary function with mismatched arguments and start returning NIL, which
is the designed and documented behaviour.  A wild call that happens to
return a plausible value is strictly worse than an honest NIL — it is what
made two tests report green while testing nothing.

## Not addressed

The `:fn-addr` arm has the same `0` fallback (the `:li-func` arm already
emits a `#xFFFFFFF0` NIL sentinel).  Separate change, needs its own gate.
