# GATE-RESULT — cabinet-fs → main (2026-09-01)

`scripts/acceptance-gate.sh main HEAD`, base `29dd302` vs fix `033955a`
(17 commits).  Two gate runners + one clean image (`build-generic-cli`, the
build-taxonomy check), 64-shard sweep on both sides, same method same run.

| | base (main 29dd302) | fix (HEAD 033955a) |
|---|---|---|
| passed | 17,515 | **17,519** |
| CHUNK-CRASH | 0 | **0** |
| FILE-WEDGE | 30 | **30** |

**NET = +4  (lost 1, gained 5).  VERDICT: PASS.**

Both timing-immune crash markers unmoved — the criterion that actually
matters; raw lost-to-crash is wall-clock noisy and was not used.

Per-ID sets read before merging (the script only automates the measurement;
the explanation is on us):

- **lost:** `24718` = **COMPILE.5**, `eval-and-compile/compile.lsp`.
  A literal-IDENTITY test: `(let ((x (copy-seq "abc"))) (funcall (compile nil
  \`(lambda () (eqt ,x ,x)))))` must be T.  This is a **reveal, not a
  regression** — the defect predates the branch and was invisible only because
  `CL:COMPILE` did not compile (it returned an `%interp-closure` marker), so
  the compiled-literal path never ran.  #297 made COMPILE real; the test then
  started exercising code that had never executed.  Measured mechanism:
  compiled literals are COPIES coalesced by CONTENT — two occurrences of one
  object correctly share a copy, but the copy is not the original object.
  Filed as **task #302**, which also records the still-unexplained
  gate-image-vs-CLI divergence over which of COMPILE.4/COMPILE.5 fails.
- **gained:** `11770, 12253, 12276, 26068, 26069`.

No unexplained ID on either side.

## Scope of what this PASS does and does not say

It says the branch does not regress **x64 ANSI conformance**.

It says **nothing** about the cabinet filesystem, the aa64 JIT constant vector,
or `#281`/`#282`/`#283` — 16 of the 17 commits are aa64 and library-loading
work that the x64 ANSI gate is structurally blind to (see
`reference_gate_blindspot_library_loading` and
`reference_ansi_gate_blind_to_gc_lisp`).  That is why NET here is identical to
the +4/−1 measured for the single commit `23688a9..d277611`.  The evidence for
those 16 commits is the library ladder and the bare-metal aa64 / real-silicon
runs recorded when they landed, not this sweep.

## Method notes (each cost a run)

- A single test is `./binary <id> <id+1>` — the END argument is EXCLUSIVE and
  the START snaps to the file boundary, so `<id> <id>` is a NULL range that
  runs the file's earlier tests and never reaches `<id>`.
- Always count `ran=` (`grep '^T:<id>'`) alongside `fail=`.  A bare fail count
  of 0 cannot distinguish "passed" from "never executed".
- The id→name mapping is PRINTED BY THE BUILD
  (`build-ansi-common.lisp:3936`): `grep -a "^ *<id> = " build.log`.  Do not
  infer it from `ansi-file-ranges.txt`, and never from a *different* build's
  ranges file.
- `nohup … &` inside a wrapper reports the WRAPPER's exit status, not the
  job's.  Wait on the process (`until ! pgrep -f …`), not on the wrapper.
