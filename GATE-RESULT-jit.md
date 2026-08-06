# JIT-vs-interpret differential coverage — gate, census, oracle

Branch `jit-diff`, off `main` @ `603e672`.  Unpushed.

**Headline: the runtime JIT produces the same answers as the interpreter.**
64-shard NET gate over IDs 10001..27800, JIT-ON vs JIT-OFF, both sides built
from the same pristine `main` tree with the same 17 625-test corpus:

| | passed | CHUNK-CRASH | FILE-WEDGE |
|---|---|---|---|
| BASE `MODUS_USE_JIT` unset (`%jit-enabled-p` baked NIL) | **17498** | 0 | 30 |
| NET  `MODUS_USE_JIT=1` (`%jit-enabled-p` baked T)       | **17497** | 0 | 30 |

Per-ID diff: **lost = 1, gained = 0**.  The single delta, `P:22140`
(`print-integers`, range 21951..22144), **passes on BOTH binaries** — 3/3 run
isolated (`<bin> 22135 22144`) and 2/2 in its own containing shard 43
(`<bin> 21998 22276`).  It is sweep noise, not a regression.  Crash markers are
identical on both sides.

The gate was then re-run on the tree **with the co-init fix of §5 applied**,
which is the state of this branch:

| | passed | CHUNK-CRASH | FILE-WEDGE |
|---|---|---|---|
| BASE JIT-off, with fix | **17497** | 0 | 30 |
| NET  JIT-on,  with fix | **17497** | 0 | 30 |

Per-ID diff: lost = `P:13448`, gained = `P:14310`.  Both are **randomized
statistical tests** and both flip run-to-run on the JIT-**off** binary, so
neither is a JIT effect:

- `P:14310` = `random.4`, a binomial-distribution test over 10 000 samples of a
  non-reproducible random state (already recorded as flaky in
  `GATE-RESULT-205.md`).  Isolated, 5 reps per binary:
  `f-off 2/5, jitoff 2/5, f-on 5/5, jiton 5/5` — flaky on both sides, and the
  JIT-on binaries passed it *more*.
- `P:13448` = `/.9` in `numbers/divide.lsp`: `(loop for a = (random-fixnum) …
  repeat 1000 …)`, complex division over 1000 random samples.  Passes 3/3
  isolated on **all four** binaries; at a wider range it flips run-to-run on
  the JIT-**off** binary (`13420..13460`, 4 reps: `1 1 0 1`).  Its two
  neighbours `/.10` and `/.11` are randomized the same way, and this is the
  same file an earlier gate blamed for a phantom `P:13445` delta.

Cross-checking the shared-file changes for neutrality — JIT-**off** with the
fix vs JIT-off pristine: lost `P:14310` (the flaky random test), gained none.
**The default (interpret) path is unchanged.**

So: **turning the JIT on costs nothing in conformance.**  This number has been
deferred repeatedly ("confirming JIT-on gate — deferred"); it is now measured.
It is also the first time the JIT path has been run against the corpus at all —
`%jit-enabled-p` is baked NIL in every ANSI gate image, and the two builds that
default it ON (`build-generic-cli`, `build-x64-cl-repl`) ship no test corpus.

---

## 1. What was and was not covered before

Three execution paths:

1. **Build-time native** — `translate-*.lisp` output baked into the image.  The
   ANSI corpus test *bodies* run this way.  Well covered: this is where #220's
   aarch64 `(mod 1 64)` → 0 lived and the aarch64 gate caught it (+28).
2. **Runtime interpret** — `mvm-eval` → `mvm-interpret`.  The default for
   runtime `eval`/`load`; what REPL probes exercise.  Covered by the JIT-off
   gate above (17 498 tests).
3. **Runtime JIT** — `mvm-eval` → translate → exec page → native call.
   **Covered by nothing.**

Path 3 now has two instruments, both in this branch:

- `tests/jit-diff.lisp` — the correctness oracle (§3).
- `tests/jit-census.lisp` + reason counters in `mvm/mvm-eval.lisp` — why a form
  does not reach native (§4).

Both run on the shipping CLI, which has had the runtime JIT on by default since
WS5 #206/#207.

---

## 2. The reframe: interp is not a safety net

The seam's design treats an interpret fallback as harmless — any JIT failure
degrades to `mvm-interpret` and "correctness is never worse than JIT-off".  For
*answers* that is true and the gate above confirms it.  For the architecture it
is not, and the reason is structural:

- **`mvm/interp.lisp` no-ops the YIELD opcode.**  `(#.+op-yield+ nil) ;
  preemption: no-op`.  `:yield` sits at every compiled loop back-edge
  (`mvm/compiler.lisp:8650`) and is the bare-metal preemption safepoint
  (`mvm/translate-x64.lisp:3488`).  Under interpretation that safepoint does
  not exist.
- **`actor-spawn` requires a native code address.**  `net/actors.lisp:275`
  stores the entry function as `(actor-set id #x30 (untag fn))` — a raw
  continuation the scheduler jumps to.  An interpreter trampoline is a *heap
  closure* (tag 9); untagged it is a heap address, and the heap is mapped
  `PROT_READ|PROT_WRITE` with no `PROT_EXEC`.

So cooperative scheduling cannot work under the interpreter, and on a mature
arch the JIT is meant to be the only path.  Every shape that still falls back
is therefore a **blocker**, not a documented condition.

### Does anything run actors under the JIT today?  No.

Exactly two build scripts pull in `net/actors.lisp`: `mvm/build-pizero2w-actors.lisp`
and `mvm/build.lisp` (the consolidated matrix cells).  Neither contains a single
`jit` reference.  The JIT-capable builds — `build-generic-cli`,
`build-x64-cl-repl`, `build-ansi-common`/`build-x64-linux`,
`build-aarch64-cli`/`build-aarch64-linux` — pull in no actor source.  The two
sets are disjoint.

Actors work today **because their code is build-time native (path 1)**, not
because the interpreter or the JIT supports them.  A runtime-defined actor
entry — `(actor-spawn #'my-entry)` where `my-entry` came from `eval`/`load` —
cannot work on any image that exists, and this gate is the first time path 3
has been exercised at all.

---

## 3. The differential oracle — `tests/jit-diff.lisp`

288 forms, each `EVAL`'d **twice in one process off one image**: once with the
seam dynamically inhibited (`*jit-inhibit*` = T → interpret) and once with it
live (→ native).  Compares the full multiple-value list, the value *count*, and
signalled-vs-returned.

Run: `./modus --load tests/jit-diff.lisp --quit`

**The gate is verified, not assumed.**  Identical answers from a run where
nothing was JIT'd would be a vacuous pass, so the harness reads
`*jit-native-count*` around every form and reports how many actually reached
native.  A direct seam probe confirms the inhibit works:

```
SEAM inhibited=0 live=3 (3 forms each)
```

Coverage: arithmetic (incl. bignum/ratio/float, both fixnum-overflow paths),
bit ops, chars, strings, `length` across all sequence types, lists, arrays
(incl. bit-vectors, `(unsigned-byte 8)`, adjustable, 2-D), hash tables,
symbols/packages, every control-flow special form (`tagbody`/`go`,
`catch`/`throw`, `block`/`return-from`, `unwind-protect`), closures and
mutation, `&optional`/`&rest`/`&key`, multiple values, all iteration macros,
conditions and restarts, types, printing/reading, setf places, runtime
`defstruct`/`defclass`/`defmethod`/`:around`, and the #220/#221 regression
shapes specifically (`(mod 1 64)`, `(consp nil)`, `(atom nil)`).

### Result

```
JD-TOTAL=292   JD-DIVERGE=1   JD-NATIVE=257   JD-BOTH-SIGNALLED=1
```

**Zero value divergences across 288 forms.**  The one non-zero line is not a
wrong value — it is the double-execution probe:

```
JIT-DOUBLE-EXEC side-mv ran 2 time(s), expected 1 (value (1 2))
```

### The one real defect the oracle found: MV forms double-execute

A value-only oracle cannot see this, which is why the harness carries explicit
side-effect counters.  Isolated:

```
(defparameter *k* 0)
(progn (setq *k* (+ *k* 1)) (floor 7 2))     ; -> *k* = 2   (expected 1)
(progn (setq *j* (+ *j* 1)) (values 1 2))    ; -> *j* = 2   (expected 1)
(progn (setq *m* (+ *m* 1)) (mk2 3))         ; -> *m* = 1   correct
```

This is documented in `mvm/mvm-eval.lisp` ("this is the ONE remaining path that
re-runs a form whose side effects already happened") but its *reach* was not:
it is not just `(values …)`.  Native stamps a real BSS MV block at
`#x10000090`, the seam cannot translate that into the interpreter's simulated
`*mvm-last-mv*`, so it re-interprets — and **any** form whose last operation
leaves MV-count > 1 qualifies: `floor`, `truncate`, `ceiling`, `round`,
`gethash`, `subtypep`, `read-from-string`, `multiple-value-list`,
`multiple-value-prog1`.  That is a large fraction of ordinary code.

Correctness footnote: a genuine user error still runs exactly once (probed —
a form that prints then `(error "boom")` prints once and propagates), and
calling a runtime-defined function runs exactly once (the #206 native-callee
guard fails the relocation *before* any native code runs).  After correcting a
double-count in the census (the MV sentinel reaches the seam handler with
`*jit-native-ran*` already T), **`R-NATIVE-ESCAPE` is 0**: MV is the only
double-execution path.

---

## 4. Fallback census — `tests/jit-census.lisp`

`*jit-fallback-count*` answered *how often*, never *why*.  Every fallback in the
seam now bumps exactly one reason counter, and under `*jit-census-on*` the
census also records the NAMES behind each blocked relocation and the MESSAGES
behind each translator gap.  Reasons: `R-MV`, `R-TRANSLATE-ERR`,
`R-RELOC-CALL-NONNATIVE`, `R-RELOC-CALL-UNRESOLVED`, `R-RELOC-FNADDR-FAIL`,
`R-MMAP-FAIL`, `R-NATIVE-ESCAPE`, `R-PAGE-NIL`.

Ranked, on the shipping CLI (post-fix, §5):

```
NATIVE=36  FALLBACK=17  TOTAL=53  NATIVE%=67
    23  R-RELOC-CALL-NONNATIVE  (callee is a runtime heap closure, no PROT_EXEC)
     9  R-PAGE-NIL              (page build returned NIL; union of RELOC/MMAP)
     8  R-MV                    (form left MV state native cannot hand back)
     1  R-RELOC-CALL-UNRESOLVED
     0  R-TRANSLATE-ERR  R-RELOC-FNADDR-FAIL  R-MMAP-FAIL  R-NATIVE-ESCAPE
-- blocked out-of-module CALL targets --
    10  JC-N [heap-closure]      6  JC-LIST [heap-closure]
     3  JC-F1 [heap-closure]     2  JC-RUN [heap-closure]  …
```

On the 288-form oracle corpus, 257/288 = **89 % native**.

---

## 5. Bug found and fixed: the JIT co-init left V9..V15 as the fixnum 0

The census found exactly one translator gap — `Unknown register: 0` — and it is
not a translator gap.  It is a one-line init bug in the JIT's own
`%init-x64-translator`, replicated across four build scripts.

`*vreg-to-x64*` is `(make-array 23)`.  V9..V15 spill to the stack and V22 (VPC)
is unmapped, so `VREG-PHYS` must return NIL for them — but `make-array`
zero-inits unwritten slots to the **fixnum 0** (GC safety), not to NIL, and 0 is
**true** in CL.  So `(vreg-phys 9)` yielded 0, `DEST-PHYS-OR-SCRATCH`'s
`(or (vreg-phys v) +scratch-reg+)` picked 0 over the scratch register, and
`reg-info` signalled `Unknown register: 0`.  The flip-safety guard turned that
signal into a clean interpret fallback — which is exactly why it was invisible:
right answers, silently never native.

`mvm/build-modus-selfhost.lisp` has carried this fix *and a comment explaining
it* since the self-host work.  The four JIT co-inits were never brought to
parity.  Fixed in `mvm/build-ansi-common.lisp`, `mvm/build-generic-cli.lisp`,
`mvm/build-x64-cl-repl.lisp`, `mvm/build-mvm.lisp`.

Measured on the CLI, same corpus both sides:

```
                      R-TRANSLATE-ERR   census native%   JD-NATIVE   JD-DIVERGE
before (jit2)                3               60            251/288        0
after  (jit3)                0               67            257/288        0
```

Moved from never-native to native, zero regressions:
`clos-class`, `loop-chain`, `mod`, `mod-neg`, `mod-var`, `mvc`
— i.e. CLOS `defclass`/`defmethod` dispatch, nested `LOOP`, and `(mod a b)`.

`(mod a b)` is worth pausing on: #220 was an aarch64 `:mod` miscompile that
returned a hard 0.  On x64 the same form was not being JIT'd *at all* because
of this init bug, so no amount of JIT-path testing would have exercised the x64
`:mod` codegen.  This is #221's argument concretely: turning the JIT on widens
the blast radius of every translator bug, and until it is on, whole opcode
paths have zero live sites.

---

## 6. What blocks JIT-only

Ranked by measured frequency.  These are blockers, not steady state.

**1. Runtime-defined functions are never native.  (`R-RELOC-CALL-NONNATIVE`, 23)**

This is the structural one and it is not a translator gap.  When `mvm-eval`
evaluates `(defun f …)`, `f` is installed as `%mvm-make-trampoline`
(`mvm/interp.lisp:393`) — a closure that **re-enters `mvm-interpret`**.  So:

- Under the JIT, only a top-level form's own module runs native.  Every function
  it *defines* still has an interpreted body.
- A later top-level form calling `f` needs an out-of-module CALL relocation.  The
  callee is a heap closure (tag 9), the #206 guard refuses to patch a non-tag-3
  address into `movabs rax, imm64; call rax` (correctly — the heap has no
  `PROT_EXEC`), the page build fails, and the whole calling form interprets.

Measured in the oracle: `xform-call`, `xform-nested`, `xform-2calls`,
`xform-in-loop` never reach native, while the in-module `(progn (defun f …)
(f 7))` shapes do.

This is the same wall as threading.  `actor-spawn` untags its `fn` and jumps to
it; an interpreter trampoline untagged is a non-executable heap address.  **JIT-only
and runtime-spawned actors are blocked on the same missing piece: a native
function-installation path** — JIT the defun body into an exec page and install
a tag-3 native pointer, so `symbol-function` of a runtime defun is real code.
Fixing that fixes both.

**2. The multiple-values bridge.  (`R-MV`, 8 — and it double-executes)**

Native writes a real MV block (count at `#x10000090`, extras at `#x10000098+`);
the seam cannot hand that back through the interpreter's simulated
`*mvm-last-mv*`, so it re-interprets the form — running its side effects twice.
Every `floor`/`truncate`/`round`/`gethash`/`subtypep`/`read-from-string`/
`values`/`multiple-value-*` form is affected.  This is the largest *correctness*
liability found and it is a bounded fix: read the BSS MV block and re-emit it
instead of re-running.  It is also the one item here that is a bug today, not
just a performance ceiling.

**3. Translator gaps: currently zero.**  After §5, `R-TRANSLATE-ERR` is 0 on
both the oracle and census workloads.  This does not mean the translator is
complete — it means the current corpus stopped finding gaps.  The census will
name the next one by message.

Not blockers: `R-RELOC-FNADDR-FAIL`, `R-MMAP-FAIL` and `R-NATIVE-ESCAPE` are all
0.  `#'NAME`, `funcall`, `apply` and macro use across top-level forms all reach
native today (`xform-macro`, `xform-fnval`, `xform-funcall`, `xform-apply`,
`xform-var`).

---

## 7. Is the interpreter still complete enough to bring up a new arch?

Under the corrected model the interpreter is a legitimate long-term component —
the bring-up path for an arch with no native translator yet (the
`MODUS_I386_LAYER` 1..5 ladder; #210's cross-arch fixpoint depends on being able
to run the compiler before the target has codegen).  So its bugs are real bugs.

Evidence it is complete today:

- **The JIT-off gate is the measurement: 17 498 / 17 625, CHUNK-CRASH 0,
  FILE-WEDGE 30** — the whole ANSI corpus under `mvm-eval` → `mvm-interpret`,
  with the JIT baked off.
- `tests/runtime-metric.lisp` — the repo's "does loaded code actually RUN"
  metric (define in one top-level form, use from a later one) — **diffs EMPTY
  against SBCL on all 16 checks under both paths**, including `form-ran-once=1`:

```
  JIT vs SBCL:     EMPTY
  INTERP vs SBCL:  EMPTY
```

- The oracle's 288 forms return identical values on both paths, so the
  interpreter is not silently behind on any construct the oracle covers.

Known interpreter gap, relevant to bring-up rather than to CL semantics: YIELD
is a no-op, so an interpreted image has no preemption safepoint and cannot run
cooperative actors.  That is acceptable for bring-up (get the compiler running,
then stand up native codegen) but it means "interpreter-only" is never a
shippable steady state for an arch that needs threading.

---

## 8. Invariants and hygiene

- `x64/bare/qemu/repl` = **`269b461a764016eea6533c46798ad3e4`** — rebuilt from
  this tree, unchanged.  (`mvm/mvm-eval.lisp` is not baked into that image.)
- All four gate sweeps run **sequentially** (the images share
  `/home/claude/modus/tmp/ansi-test/sandbox`), 64 shards, identical budgets,
  6 m 31 s / 6 m 30 s / 6 m 31 s / 6 m 33 s.  All four builds report
  `ANSI tests: 17625` and all four `ansi-file-ranges.txt` are byte-identical,
  so ID-based comparison is valid across every pairing.
- Every delta deterministically rechecked isolated (3–5 reps) **and** in its
  containing shard (2 reps), on every binary — not just the two being compared.
  No shard hit the 600 s cap (shard 12 completes in 277–279 s), so none of
  these deltas is truncation; they are the corpus's own randomized tests.

## 9. Reproduce

```bash
# JIT-on vs JIT-off ANSI gate (build each into its own MODUS_ANSI_OUT *file*)
MODUS_ANSI_OUT=<dir>/bin              sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
MODUS_USE_JIT=1 MODUS_ANSI_OUT=<dir2>/bin sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
NSH=64 /home/claude/n5gate.sh <dir>/bin  gate-off.out JITOFF     # sequentially!
NSH=64 /home/claude/n5gate.sh <dir2>/bin gate-on.out  JITON
comm -23 gate-off.out gate-on.out   # lost
comm -13 gate-off.out gate-on.out   # gained

# oracle + census (shipping CLI, JIT on by default)
sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp
./modus --load tests/jit-diff.lisp   --quit    # JD-DIVERGE must be 0
./modus --load tests/jit-census.lisp --quit    # ranked blockers
```
