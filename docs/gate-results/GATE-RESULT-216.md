# GATE #216 — :fn-addr unresolved fallback emitted offset 0, not the NIL sentinel

## The bug (a latent hazard, not an active one)

`emit-bytecode-for-ir`'s `:fn-addr` arm fell back to target `0` for an
unresolved name, while the sibling `:li-func` arm — which emits the *same*
`MVM-FN-ADDR` opcode — has emitted the `#xFFFFFFF0` NIL sentinel since the
CHUNK-CRASH fix.

The failure mode is subtler than #215's, and worse in kind.  Offset 0 is a
VALID bytecode offset (the module's first function) and it IS present in the
translator's offset→label table.  So `#'undefined-name` did not yield a bad
pointer that traps on use — it yielded a **well-formed, correctly-tagged
pointer to the wrong function**.  `functionp` is true, the tag nibble is
clean, and `funcall` invokes it happily.

With the sentinel, native translate loads NIL so `funcall` signals
UNDEFINED-FUNCTION (CLHS-correct), and the in-image interpreter resolves it
to NIL the same way.

## JIT interaction (checked, not assumed)

`#xFFFFFFF0` >= `#x40000000`, so under `*x64-jit-mode*` it takes the
out-of-module reloc branch.  `%jit-reloc-fn-addrs` finds no name in the
rt-table, gets word 0, returns NIL — and `%jit-translate-page-1` then falls
back to interpretation for that module.  Safe by construction: a perf
effect, not a correctness one.  aarch64 already faces the identical sentinel
today via `:li-func`, so this introduces no new situation there.

## Scale: ZERO sites fire today

The arm had no counter, so the frequency was unknown.  Added the same WARN
`:li-func` carries.  Result on the x64-linux gate image:

```
WARN fn-addr : 0
WARN li-func : 25   (pre-existing, unchanged)
```

**No `:fn-addr` site in the gate image is unresolved.**  This fix therefore
changes no emitted behavior in this image.  It is a latent-hazard fix plus a
diagnostic, and it should not be credited with any observable improvement.

## Gate — 64 shards, [10001,27800]

```
BASE main 8dd4318   passed=17492  CHUNK-CRASH=0  FILE-WEDGE=30
NET  #216           passed=17491  CHUNK-CRASH=0  FILE-WEDGE=30
per-ID: lost=2 (14310 random, 23237 format-f)  gained=1 (13448 divide)
```

All three fail on BOTH binaries in isolation (3 reps each).  Re-running the
two containing shards in context:

```
14186..14464   BASE=1283  NET=1283   lost: none   gained: none
23114..23392   BASE=1317  NET=1317   lost: none   gained: none
```

Neither loss reproduces — identical pass sets.  **Zero real diffs**, which is
what `WARN fn-addr = 0` predicts: the only difference between the two images
is baked comment text (layout), and layout shift is fuzz-proven not to flip
tests on x64 Linux.

## Why land it

Correctness insurance with a visible tripwire.  A silent call to the wrong
function is the hardest class of bug to diagnose — it is exactly what made
two `elt` tests report green while testing nothing in #215.  If an
`:fn-addr` site ever does go unresolved, the build now says so.
