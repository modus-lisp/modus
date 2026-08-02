# ANSI gate — WS5 #206/#207 (mvm-eval.lisp + translate-aarch64.lisp)

64-shard NET gate over IDs 10001..27800, both sides built from fresh detached
worktrees off the same tree, same corpus (both builds report "ANSI tests: 17625",
so ID-based comparison is valid here).

    BASE  fa6b9c1   passed=17476  CHUNK-CRASH=0  FILE-WEDGE=30
    NET   21347e4   passed=17475  CHUNK-CRASH=0  FILE-WEDGE=30

Per-ID diff: lost=1 (P:13445), gained=0.

DETERMINISTIC RECHECK of the single delta — P:13445 run in isolation
(`./tmp 13440 13450`), 3 repetitions per binary:

    gate-base   P:13445  P:13445  P:13445
    gate-net    P:13445  P:13445  P:13445

It passes on BOTH. The sweep-level difference is shard noise (600s per-shard
wall-clock truncation), not a regression. Judge by crash markers + deterministic
recheck, never the raw headline — CHUNK-CRASH and FILE-WEDGE are identical.

VERDICT: NET == BASE, zero real regressions. The shared-file changes in
mvm/mvm-eval.lisp are ANSI-neutral.

Commits gated: d139c73, aec8341, 3b9b4a4, 21347e4.
NOT gated by this run (does not affect the x64 ANSI image): the aarch64-only
hunks in mvm/translate-aarch64.lisp and mvm/build-aarch64-cli.lisp — those are
covered by tests/runtime-metric.lisp diffing EMPTY vs SBCL with the aarch64 JIT
ON, plus the x64 neutrality rebuild.
