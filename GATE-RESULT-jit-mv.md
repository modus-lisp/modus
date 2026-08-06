# JIT multiple values: read the native MV block, stop re-running the form

Branch `jit-mv`, off `main` @ `d30eaaa`.  Unpushed.

**The JIT's last double-execution path is closed.**  A form whose last
operation leaves MV-count > 1 now runs EXACTLY ONCE under the JIT.  The seam
reads the native MV block back out of BSS and publishes it in the shape
`mvm-interpret` produces, instead of signalling a sentinel and asking the
interpreter to re-run a form whose side effects had already happened.

| | before (`main` @ d30eaaa) | after (this branch) |
|---|---|---|
| `tests/jit-diff.lisp` JD-DIVERGE | **21** | **0** |
| MV probes that double-executed | **20 of 21** | **0 of 21** |
| MV probes reaching native (JD-MV-NATIVE) | **1 of 21** | **21 of 21** |
| `*jit-mv-fallback-count*` on that workload | **444** | **0** |
| census `R-MV` | **8** | **0** |
| census native% | 67 | **83** |
| oracle forms reaching native (JD-NATIVE / 288) | 257 | **280** |

**ANSI gate, 64-shard, IDs 10001..27800, corpus 17 625 on all three images:**

| | passed | CHUNK-CRASH | FILE-WEDGE |
|---|---|---|---|
| `main` @ d30eaaa, JIT-off (reference) | 17498 | 0 | 30 |
| this branch, JIT-off | 17497 | 0 | 30 |
| **this branch, JIT-on** | **17498** | **0** | **30** |

Per-ID: **`main`-JIT-off → this-branch-JIT-on is lost = 0, gained = 0 — the
two pass-ID sets are IDENTICAL.**  The only delta anywhere is `P:14310`
(`random.4`), which is flaky on the JIT-off binaries and passes on the JIT-on
ones; it is the randomized binomial test already recorded as flaky in
`GATE-RESULT-jit.md` and `GATE-RESULT-205.md`, with the same signature.

`x64/bare/qemu/repl` rebuilds from this tree to
**`269b461a764016eea6533c46798ad3e4`** — the `mvm/BUILDS.md` hash, unchanged.

---

## 1. The bug

`%mvm-eval-jit-run` ran the form natively, then read the BSS MV-count at
`#x10000090`.  If it was not 1, it set `*jit-infra-fallback*`, bumped
`*jit-mv-fallback-count*` and signalled `"jit-mv-fallback"` — a deliberate
sentinel telling the seam handler to allow the interpret fallback *despite*
`*jit-native-ran*` being T.  The interpreter then re-ran **the whole form**.
The values it returned were correct; every side effect the native run had
already performed happened a second time.

```lisp
(defparameter *k* 0)
(progn (setq *k* (+ *k* 1)) (floor 7 2))   ; -> 3, 1   and *k* = 2
```

This is invisible to a value-only oracle — which is why 288 forms of
JIT-vs-interpret value comparison found nothing and one side-effect counter
found it immediately.

**The reach was much wider than the docstring's "the multiple-values case".**
It was not `(values …)`; it was *any* form whose last operation leaves
MV-count > 1.  Measured: every one of these double-executed on `main`.

```
floor  floor-neg  truncate  ceiling  round
gethash (hit and miss)  subtypep  read-from-string
multiple-value-bind  multiple-value-list  multiple-value-call
multiple-value-prog1  values (0, 2, 3 and 8 values)
MV in tail position of LET and of IF     heap-valued extras
```

The docstrings on `*jit-infra-fallback*`, `*jit-mv-fallback-count*`,
`%mvm-eval-jit-run`, the `R-MV` census entry and the two seam-handler comments
all said "the multiple-values case"; all of them are corrected here.

## 2. The mechanism

`mvm/mvm-eval.lisp`, `%mvm-eval-jit-run`, immediately after `%jit-call`:

```lisp
(let ((mvc (mem-ref #x10000090 :u64))     ; tagged count == the Lisp integer
      (secs nil) (mv-ok nil))
  (if (fixnump mvc) (if (< mvc 0) nil (if (< mvc 22) (setq mv-ok t) nil)) nil)
  (if mv-ok
      (let ((i (- mvc 2)))                ; back-to-front, so each cons prepends
        (loop (when (< i 0) (return nil))
              (setq secs (cons (mem-ref (+ #x10000098 (* i 8)) :u64) secs))
              (setq i (- i 1))))
      nil)
  (setq *mvm-last-mv* (if (eql mvc 1) nil (cons mvc secs)))
  …)
```

Three facts make this work.  Each was checked, not assumed:

**(a) `mem-ref … :u64` hands back the TAGGED value, not a raw integer needing
decode.**  `:u64` is the "raw bits, no shift" mode, and the raw bits of the
count slot are `count << 1` — which *is* the tagged fixnum `count`.  So `mvc`
reads back as an ordinary Lisp integer and each extra reads back as its Lisp
object.  This is exactly what compiled `MULTIPLE-VALUE-LIST` already does
(`compile-multiple-value-list`, `mvm/compiler.lisp`), so the decoding is not a
new convention invented here.

**(b) The consumer is `*mvm-last-mv*` and its shape is `(count . secondaries)`.**
`mvm-interpret` sets
`(setq *mvm-last-mv* (cons %mvc (%mvm-collect-mv-secs state %mvc)))`
(`mvm/interp.lisp`), and *both* seam sites — `%mvm-eval-run-tuple` and
`mvm-eval-forms` — latch `(%mv *mvm-last-mv*)` immediately after the run and
re-emit `(values-list (cons %prim (cdr %mv)))`, with `(car %mv)` = 0 meaning
`(values)`.  Writing the same cons from the native path means **no seam change
at all**: both call sites got the fix for free, and `%prim` (the `%jit-call`
return value) is already the primary value.

**(c) The address is arch-independent — no `*jit-target-arch*` branch is
needed.**  `+mv-count-addr+` = `#x10000090` and `+mv-values-addr+` =
`#x10000098` are compiler constants (`mvm/compiler.lisp`), and
`translate-aarch64.lisp` emits the identical literal addresses that
`translate-x64.lisp` does — aarch64 `op-set-mv-count` at
`(a64-load-imm64 buf +a64-x17+ #x10000090)`, its GC MV-area root scan at
`#x10000098`.  The aarch64 seam therefore takes the same corrected path; it is
neither special-cased nor bypassed.  (x64 is what is gated here; aarch64 is
unchanged in structure, not re-gated.)

### The ordering constraint, which is the whole difficulty

The read-back must be the first thing after `%jit-call` and must not CALL
anything until the last extra is latched:

* every compiled function epilogue emits `op-set-mv-count 1`, so a single
  intervening call resets the count slot and the number of live extras is lost;
* worse, the collector scans **exactly `(count-1)` words from `#x10000098`**
  (`translate-x64.lisp`'s MV-area root scan).  If the count were reset while
  extras were still unread, a GC triggered by the read-back's own cons loop
  would leave those extras un-forwarded — stranded at from-space addresses.
  Same class as the interpreter bug documented at `mvm/interp.lisp:166`.

So the read-back uses only operations that compile INLINE — `mem-ref`,
`fixnump`, `<`, `+`, `-`, `*`, `cons`.  (`fixnump` is a primop,
`compile-fixnump`; `<` is `compile-compare :blt`, whose fast path is a register
compare when both operands are fixnums, which the `fixnump` guard guarantees.)
The BSS count therefore stays authoritative for the whole loop, the collector
keeps scanning the right number of extras, and the back-to-front read re-reads
each slot *after* the previous cons's possible GC, so a forwarded extra is
picked up at its new address.

`jd-mv-stress` in `tests/jit-diff.lisp` exercises precisely this: 400 evals of
`(values (list 1 2 3) (list 4 5) "zz")` — freshly consed HEAP extras — with a
128-byte string allocated each iteration.  `bad=0`.  Fixnum extras would have
survived a stranding bug silently; heap extras would not.

### What still falls back, and why

Only an MV count outside `[0,21]`.  The MV-VALUES area is 20 slots
(`#x10000098` … `#x10000130`; `+closure-env-addr+` starts at `#x10000140`), so
a larger count would mean reading words that are not MV storage and handing the
collector bogus roots.  Such counts are already truncated by `compile-values`'
own 16-slot storage cap on **both** paths, so re-interpreting them yields
nothing better — but the fallback is kept, and still counted, rather than
guessed at.  `*jit-mv-fallback-count*` measures it: **0** on every workload run
here (it read 444 on the same workload before the change).

**The re-execution guard is not weakened.**  `*jit-native-ran*` is untouched.
`*jit-infra-fallback*` is now set in strictly fewer places than before — one
narrow residual instead of every MV form.  The user-condition re-signal branch
and `*jit-resignal-count*` are unchanged.  This removes a reason to re-run; it
does not broaden when re-running is allowed.

## 3. The side-effect oracle

`tests/jit-diff.lisp` gains `jd-mv-once`, which for each MV-producing form
evaluates it **once with the JIT live and once with the seam inhibited**,
resetting a counter before each, and asserts four things:

1. the counter increments exactly once under JIT-on;
2. it also increments exactly once under interpret (so the probe is honest);
3. the full multiple-value list equals the literal CL answer — a fix that
   returned one value where CL requires two would be worse than the doubling;
4. the JIT and the interpret paths agree with each other.

It also records the `*jit-native-count*` delta per probe: an all-clear from a
run where nothing was JIT'd is not a measurement, and before the fix 20 of the
21 MV probes reached native ZERO times.

Twenty-one probes: `values-0/1/2/3`, `floor`, `floor-neg`, `truncate`,
`ceiling`, `round`, `gethash-hit`, `gethash-miss`, `subtypep`,
`read-from-string`, `mv-bind`, `mv-list`, `mv-call`, `mv-prog1`, `mv-in-let`,
`mv-in-if`, `mv-many` (8 values), `mv-heap` (heap-object extras) — plus
`jd-mv-stress` (400 GC-pressured evals).

### Before — `main` @ `d30eaaa`, JIT-on CLI

```
JIT-DOUBLE-EXEC side-mv ran 2 time(s), expected 1 (value (1 2))
JIT-DOUBLE-EXEC mv:values-2         ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:values-3         ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:values-0         ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:floor            ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:floor-neg        ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:truncate         ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:ceiling          ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:round            ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:gethash-hit      ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:gethash-miss     ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:subtypep         ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:read-from-string ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-bind          ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-list          ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-call          ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-prog1         ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-in-let        ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-in-if         ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-many          ran 2 time(s) under JIT, expected 1
JIT-DOUBLE-EXEC mv:mv-heap          ran 2 time(s) under JIT, expected 1
MV-CL-GAP (both paths) mv:mv-list got=((3 1) 1) expected=((3 1))
JD-MV-STRESS n=400 bad=0

JD-TOTAL=314  JD-DIVERGE=21  JD-NATIVE=257  JD-BOTH-SIGNALLED=1
JD-MV-TOTAL=21  JD-MV-NATIVE=1  JD-MV-CL-GAP=1  JD-MV-FALLBACK=444
JD-FAIL
```

Every printed value was already CORRECT — that is the whole point.  Only the
counters were wrong.

### After — this branch

```
MV-CL-GAP (both paths) mv:mv-list got=((3 1) 1) expected=((3 1))
JD-MV-STRESS n=400 bad=0

JD-TOTAL=314  JD-DIVERGE=0  JD-NATIVE=280  JD-BOTH-SIGNALLED=1
JD-MV-TOTAL=21  JD-MV-NATIVE=21  JD-MV-CL-GAP=1  JD-MV-FALLBACK=0
JD-OK
```

`JD-NEVER-NATIVE` shrinks from 27 labels to 8, and the 8 that remain are the
pre-existing blockers unrelated to MV: `ecase-err`, `err-undef`,
`clos-method`, `clos-around`, and the four `xform-*` shapes that are blocked by
`R-RELOC-CALL-NONNATIVE` (a top-level form calling a function defined by an
earlier top-level form — §6.1 of `GATE-RESULT-jit.md`, still open).

### Census (`tests/jit-census.lisp`, same CLI)

```
                 before        after
NATIVE%            67            83
R-MV                8             0
R-TRANSLATE-ERR     0             0
```

`R-RELOC-CALL-NONNATIVE` (23) and `R-PAGE-NIL` (9) are unchanged — the
runtime-defun relocation blocker is untouched by this work and is now the sole
remaining fallback reason of any weight.

## 4. A pre-existing CL gap this surfaced (NOT a JIT defect)

```
MV-CL-GAP (both paths) mv:mv-list got=((3 1) 1) expected=((3 1))
```

`(multiple-value-list (floor 7 2))` returns **two** values in Modus — the list
`(3 1)` plus a stray secondary `1`.  `compile-multiple-value-list` does not
reset the MV count to 1 after reading the inner form's block, so `floor`'s
count of 2 leaks out as `multiple-value-list`'s own.  This is identical on the
JIT and the interpret paths (they are the same compiler), so the oracle counts
it separately (`JD-MV-CL-GAP`) rather than as a divergence — a differential
oracle must not be made un-passable by a defect it does not measure.  Left
unfixed here: it is a compiler change on a shared file and belongs in its own
gated commit.

## 5. The ANSI gate

Three images, all built from the same 17 625-test corpus with **byte-identical
`ansi-file-ranges.txt`** (`90b30b1e8f6f281a7264f2128b6beda3`), so ID-based
comparison is valid across every pairing:

| image | `%jit-enabled-p` | passed | CHUNK-CRASH | FILE-WEDGE |
|---|---|---|---|---|
| `mainoff` — pristine `main` @ d30eaaa | NIL | 17498 | 0 | 30 |
| `off` — this branch | NIL | 17497 | 0 | 30 |
| `on` — this branch | T | **17498** | 0 | 30 |

Per-ID diffs:

```
mainoff -> on   (main JIT-off  ->  branch JIT-on)    lost=0  gained=0
off     -> on   (branch JIT-off -> branch JIT-on)    lost=0  gained=1  (P:14310)
mainoff -> off  (main JIT-off  ->  branch JIT-off)   lost=1  gained=0  (P:14310)
```

**The JIT-on pass-ID set is identical to pristine `main`'s JIT-off set.**  Zero
tests lost in the direction that matters.  Crash markers are identical on all
three (CHUNK-CRASH 0, FILE-WEDGE 30).

### The single delta, deterministically rechecked on every binary

`P:14310` = `random.4` (`ansi-file-ranges.txt`: 14304..14314, file `random`) —
a binomial-distribution test over 10 000 samples of a non-reproducible random
state.

| binary | isolated (14305..14315), 5 reps | containing shard 15 (14186..14464), 3 reps |
|---|---|---|
| `mainoff` (main, JIT-off) | 0/5 | 1/3 |
| `off` (branch, JIT-off) | 2/5 | 1/3 |
| `on` (branch, JIT-on) | **5/5** | **3/3** |

Flaky on the JIT-off binaries — including pristine `main`, which never saw this
change — and stable on JIT-on.  This is the identical signature
`GATE-RESULT-jit.md` recorded for it on pristine `main` before this branch
existed ("f-off 2/5, jitoff 2/5, f-on 5/5, jiton 5/5 — flaky on both sides, and
the JIT-on binaries passed it *more*"), and `GATE-RESULT-205.md` already lists
it as flaky.  It is not a JIT-MV effect in either direction.

A first round of this gate (built before a one-line docstring correction) also
showed `P:21937` (`print-floats`) missing from one JIT-off sweep; it rechecks
5/5 isolated and 3/3 in its containing shard on **all three** binaries, i.e.
pure sweep noise, and it does not appear in the final round at all.

### Sweep hygiene

All sweeps ran **sequentially** (the images share
`/home/claude/modus/tmp/ansi-test/sandbox`), 64 shards, identical budgets:
JIT-off 392 s, JIT-on 391 s, `main` baseline 391 s.  Since all 64 shards run
concurrently, total wall time bounds the slowest shard: **no shard came near
the 600 s cap**, so none of these deltas is truncation.  Each image was built
into its own `MODUS_ANSI_OUT` **file** path with its own dump directory.

## 6. JIT-off invariants

* `x64/bare/qemu/repl` rebuilt from this tree:
  **`269b461a764016eea6533c46798ad3e4`** (81 168 bytes) — the `mvm/BUILDS.md`
  hash, unchanged.  Verified twice, before and after the final source edit.
* The JIT-off ANSI image never executes the changed code at all:
  `%jit-enabled-p` is baked to a NIL constant, `%jit-active-p` is therefore
  constant-false, and `%mvm-eval-jit-run` — the only function whose body
  changed — is never called.  Its JIT-off gate is 17497 / CHUNK-CRASH 0 /
  FILE-WEDGE 30, differing from pristine `main` by exactly the one flaky
  randomized test above.
* The seam itself (`%mvm-eval-run-tuple`, `mvm-eval-forms`) is **unmodified**:
  the fix writes the value both sites already read.
* No compiler, translator, runtime or corpus file is touched.  The diff is
  `mvm/mvm-eval.lisp` (one function body plus docstrings/comments) and
  `tests/jit-diff.lisp` (new probes).

## 7. Reproduce

```bash
# oracle + census on the shipping CLI (JIT on by default)
sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp
./modus --load tests/jit-diff.lisp   --quit   # JD-DIVERGE 0, JD-MV-FALLBACK 0
./modus --load tests/jit-census.lisp --quit   # R-MV 0

# ANSI gate: build each image into its own MODUS_ANSI_OUT *file*
MODUS_ANSI_OUT=/tmp/mvgate/off/bin \
  sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
MODUS_USE_JIT=1 MODUS_ANSI_OUT=/tmp/mvgate/on/bin \
  sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
NSH=64 /home/claude/n5gate.sh /tmp/mvgate/off/bin off.out JITOFF   # sequentially!
NSH=64 /home/claude/n5gate.sh /tmp/mvgate/on/bin  on.out  JITON
comm -23 off.out on.out    # lost
comm -13 off.out on.out    # gained

# JIT-off shipping invariant
sbcl --script mvm/build.lisp x64/bare/qemu/repl && md5sum /tmp/modus-x64.bin
```

## 8. What this leaves

`R-NATIVE-ESCAPE` is 0 and `R-MV` is now 0, so **no JIT path double-executes a
form's side effects any more** except the out-of-range MV residual, which does
not fire.  The remaining fallback weight is entirely
`R-RELOC-CALL-NONNATIVE` (23) — a top-level form calling a function that was
itself defined at runtime, whose callee is an interpreter trampoline (a heap
closure with no `PROT_EXEC`).  That is the structural blocker for JIT-only and
for runtime-spawned actors, and it is unchanged by this work; see
`GATE-RESULT-jit.md` §6.1.
