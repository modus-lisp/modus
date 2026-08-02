# ANSI gate — #205 ANF normalization of over-nested arithmetic (mvm/compiler.lisp)

64-shard NET gate over IDs 10001..27800.  Both sides built from fresh detached
worktrees, same corpus (both builds report "ANSI tests: 17625", so ID-based
comparison is valid), each with its own `MODUS_ANSI_OUT` so the two builds
cannot clobber each other's binary.  The two sweeps were run sequentially —
both images share `/home/claude/modus/tmp/ansi-test/sandbox` at run time.

    BASE  910a300 (main)  passed=17476  CHUNK-CRASH=0  FILE-WEDGE=30
    NET   293ce0a         passed=17475  CHUNK-CRASH=0  FILE-WEDGE=30

BASE reproduces the reference numbers for this base exactly (17476 / 0 / 30).

Per-ID diff: lost=1 (P:14310), gained=0.

DETERMINISTIC RECHECK of the single delta.  P:14310 maps (via each build's own
`ansi-file-ranges.txt`, range 14304..14314 = numbers/random.lsp) to
**random.4**:

    (deftest random.4
      (binomial-distribution-test 10000
                                  #'(lambda () (< (random 1.0s0) 0.5s0)))
      t)

— a statistical test over 10000 samples of a non-reproducible random state.
Run in isolation (`<binary> 14309 14312`), 5 repetitions per side:

    gate-base   FAIL FAIL PASS FAIL PASS   (2/5)
    gate-net    FAIL FAIL PASS PASS PASS   (3/5)

It is flaky on BOTH binaries, and NET passed it MORE often than BASE in this
sample.  The sweep-level difference is that flakiness, not a regression.
(Its neighbour random.5 / 14311 fails on both sides — pre-existing.)

VERDICT: NET == BASE, zero real regressions.  CHUNK-CRASH and FILE-WEDGE are
identical.

Additional non-ANSI verification:

  - `test/regress/nested-arith-loop-sum.lisp` — BASE: `!! UNHANDLED-ESCAPE
    load-toplevel-form-swallowed: SIMPLE-ERROR | MVM compiler: nested
    arithmetic with function calls will miscompile`.  NET: `[B] hdr-bits = 31`.
  - `tests/runtime-metric.lisp` vs SBCL — diff EMPTY, `form-ran-once=1`, both
    with the JIT at its default and with `MODUS_USE_JIT=1`.
  - A 9-shape battery (the #205 shape; `-`, `*`, `logand`, `logior`, `logxor`
    variants; a deeper nest; two evaluation-order probes) — byte-identical to
    SBCL, including `order-1 = 102` (the left operand is still read BEFORE the
    hoisted call) and `log=(A B C)` (left-to-right order across a hoist).
