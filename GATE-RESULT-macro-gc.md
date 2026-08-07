# Runtime macros die on GC — and macros were the smallest instance

Branch `macro-gc`, off `main` @ `9a3e48e`.  Unpushed.  Two source files changed
(`mvm/mvm-eval.lisp`, plus the counter row in `tests/jit-census.lisp`) and the
probe battery in `tests/jit-diff.lisp`.

```
JD-GCSTALE-HITS      main 10 / 23 probes   ->   this branch 0 / 23
                     (1 / 2 on the two probes that existed before)

ANSI 64-shard        BASE-OFF 17498  BASE-ON 17498      CHUNK-CRASH 0
                     NET-OFF  17497  NET-ON  17497      FILE-WEDGE  30
                     no deterministic loss; main's own image re-swept
                     scores 17497 and loses the same ID (see §6)

x64/bare/qemu/repl   269b461a764016eea6533c46798ad3e4 — unchanged
```

---

## 1. What was actually wrong

The reported symptom: a runtime `DEFMACRO`'s expander survives as an object —
`macro-function` still returns non-NIL — but its quoted-symbol constants go
stale across a collection, so `(macroexpand-1 '(mac-add 2 3))` returns
`(#<?> 2 3)` and compiling that signals `UNDEFINED-FUNCTION`.

**It is not about macros, and the fix that was suggested first would have
covered only the one probe that happened to be measured.**

A JIT page bakes ABSOLUTE addresses into `movabs` immediates in exactly three
places.  Two of them could be **heap** addresses:

| site | what it bakes | moves under GC? |
|---|---|---|
| `%jit-patch-consts` | each const-pool object's tagged heap address | **yes** |
| `%jit-reloc-fn-addrs` | `#'NAME`'s resolved word, **any tag** | **yes**, when the target is a runtime trampoline (tag 9) |
| `%jit-reloc-calls` | callee entry, tag-3 required since #206 | no |

Heap objects move under the Cheney collector.  The only re-bake —
`%jit-entry-for`, keyed on `%gc-count` — fires when a cached page is re-entered
**through the seam**.  Anything that re-enters the page by another route keeps
reading the pre-collection address, forever.

### Why `DEFUN` is immune and `DEFMACRO` is not

Not because of anything about functions vs macros.  A **const-bearing** runtime
`DEFUN` is refused native installation by #222's per-function const check, so it
keeps its interpreter trampoline, and `mvm-interpret`'s `op-LI-CONST` does
`(gethash idx *e2-const-pool*)` **at execution time** — it reads whatever the
collector has updated the pool to hold.  A **const-free** runtime `DEFUN` is
installed natively but has no const to go stale.  Either way, safe.

Everything else in the system that a JIT page can produce was uncovered: a macro
expander is just a lambda handed to `set-macro-function`, and its code is in the
page with baked consts.

This was verified rather than reasoned about.  Inside **one** lambda body, after
a collection:

```lisp
(eval '(defparameter *cc* (lambda () (list 'zzuniq (gethash *pidx* *e2-const-pool*)))))
;; … one real collection …
(funcall *cc*)   =>  (#<?> ZZUNIQ)
```

The explicit `gethash` into the pool returns the **live** object; the quoted
literal beside it returns the **stale** one.  The pool is fine; the path from
baked code to the pool is what was broken.  (`(gethash *pidx* *e2-const-pool*)`
read directly at top level after the same GC also returns `ZZUNIQ`.)

One measurement trap worth recording, because it cost an hour: **`MODUS_NO_JIT`
is a BUILD-time variable, not a runtime one.**  Setting it on the command line of
an already-built CLI changes nothing, and the bug then appears to reproduce
"JIT-off" — which sends you looking for a defect in the interpreter that is not
there.  The runtime inhibit is `(setq *jit-inhibit* t)`, and with it the bug
vanishes:

```
                       defined JIT-inhibited      defined JIT-live
after one collection   (AA "bb")   (+ 2 3)        (#<?> #<?>)   (#<?> 2 3)
```

## 2. The class — five instances, four fixed

Every row measured on `main` @ `9a3e48e`, one real collection.

| # | shape | on `main`, after 1 GC |
|---|---|---|
| 1 | runtime `DEFMACRO` expander | `(#<?> 2 3)` |
| 2 | lambda in a global / inside a data structure | `(#<?> #<?>)` |
| 3 | lambda returned as the `EVAL` result | `(#<?> #<?>)` |
| 4 | #222-installed const-**clean** DEFUN calling a const-**dirty** sibling | `(#<?> #<?>)` |
| 5 | `#'RUNTIME-DEFUN` as a value inside a const-free lambda | `UNHANDLED-ESCAPE … NIL` |
| — | **residual:** collection *during* one JIT'd top-level form | `(#<?> #<?>)` — still |

Two of these are worth calling out because they are not "persistence" bugs in
the sense the report assumed:

**Row 4 is a hole in #222 itself.**  `%jit-fn-native-offsets` checks only a
function's OWN byte range for const patch sites.  But an in-module call is a
direct in-page branch, so a const-clean function that #222 publishes as native
can jump straight into a const-dirty sibling:

```lisp
(eval '(progn (defun h1 () (list 'aa "bb")) (defun h2 () (h1))))
;; … one GC …
(h1) => (AA "bb")      ; kept its trampoline, fine
(h2) => (#<?> #<?>)    ; published native, reads through h1's stale consts
```

A per-function range check cannot see this; only a page-level rule can.

**Row 5 is a second baked-heap-address site.**  `%jit-reloc-fn-addrs` patched
whatever word the name resolved to, with no tag requirement — and a runtime
`DEFUN` that kept its trampoline is a tag-9 **heap closure**.  So a lambda that
merely *mentions* `(function tf)`, with no constant of its own anywhere, held a
heap address too.  It returns correctly, then after one collection faults hard
enough to escape its own `handler-case`.

**The residual is the ugliest and is NOT fixed.**  One top-level form, no macro,
no closure, nothing persisted — just a form that allocates past the collection
threshold during its own execution:

```lisp
(eval '(progn <allocate until gc_count moves> (list 'ee "ff")))   =>  (#<?> #<?>)
```

The seam re-bakes at entry; a collection mid-flight gets no second chance.  This
is exactly what a GC-updated constant vector (task **#226**) fixes and what
nothing short of it can.  It is reported on its own counter — `JD-GCRESID-HITS`
— so it stays visible and cannot be mistaken for the class that is now closed.

## 3. The fix

Two guards, both in `mvm/mvm-eval.lisp`, that together establish one invariant:
**a built JIT page contains no heap address at all.**

1. **`%jit-translate-page-1` — the GC-safety gate (`R-CONST-BAKED`).**  If any
   const patch site falls **outside the `%MVM-EVAL-THUNK`'s own native range**,
   the page is not built and the module interprets.  Its constants are then read
   live out of `*e2-const-pool*` by `op-LI-CONST` — precisely the mechanism that
   already made `DEFUN` trampolines immune.

2. **`%jit-reloc-fn-addrs` — tag-3 required**, the rule `%jit-reloc-calls` has
   had since #206.  A heap-closure target now fails the reloc, which fails the
   page build, which interprets the form once.

Const patches and those two relocation classes are the only places an immediate
is baked.  What survives the guards is image code and `%mmap-exec-page` pages,
neither of which the collector moves — so the result is GC-immune **by
construction**, with no post-GC hook needed anywhere (there is none available:
`gc_trampoline` is emitted assembly and never calls back into Lisp).

### Why the gate is at the thunk, not "any const at all"

The blanket rule — reject on any const patch site anywhere — is sound, and it
was implemented and measured first.  It is unaffordable: the top-level thunk of
`(defun f …)` carries the name `'F` as a constant, so **every** runtime defun
form is rejected and #222 never installs anything native again.  With the
blanket rule the class probes' own `(dotimes (j 100000) (make-list 40))`
allocation loop went from seconds to not finishing in two minutes, because
`force-gc` itself had degraded to interpreted.

The thunk is the one function in a module that is entered only through the seam,
exactly once, with `%jit-entry-for`'s re-bake in front of it.  Every other
function in the page — top-level defuns, lambda/closure bodies, flet/labels
bodies — either outlives the module or is the in-page callee of something that
does.  So the rule is: a const site inside any non-thunk function rejects the
page; const sites confined to the thunk may bake.  That is exactly the line
between the four fixed rows and the residual.

## 4. `JD-GCSTALE-HITS`, and the extended battery

`tests/jit-diff.lisp` had two GC-survival probes on one macro.  It now has 23,
each asserting the **exact value** it must get back, so a garbage head or a
stale string counts as a hit rather than passing because nothing crashed.  Added:
an expansion carrying a string *and* a quoted list; a macro calling another
runtime macro; use after four separate collections; redefinition across a
collection; closures in a global, inside a data structure, and as an `EVAL`
result; the #222 dirty-sibling pair; the `#'RUNTIME-DEFUN` value case.

Same file, both images:

```
                          main @ 9a3e48e      macro-gc
JD-GCSTALE-PROBES         23                  23
JD-GCSTALE-HITS           10                   0      <-- the headline
JD-GCRESID-HITS            1                   1      known-open, needs #226
JD-MACRO-REDEF-HITS        1                   1      separate bug, see §7
JD-TOTAL                  332                 332
JD-DIVERGE                  0                   0
JD-NATIVE                 297                 291
JD222-TOTAL                61                  61
JD222-FAIL                  0                   0
JD222-EARLYBIND             1                   1
JD222-INSTALLED            27                  26
JD-MV-FALLBACK              0                   0
                          JD-OK               JD-OK
```

Probe counts are equal on both images by construction: the `#'RUNTIME-DEFUN`
probe faults past its own `handler-case` on `main`, so the risky call sits alone
in its own top-level form writing a global, and the assertion reads that global
afterwards — a swallowed form still lands as a hit rather than skipping a probe.

The `main` hits in full:

```
JD-GCSTALE gc-macro-expand              got=(#<?> 2 3)
JD-GCSTALE gc-macro-rich-expand-after   got=(#<?> #<?> (#<?> (#<?> #<?>)) 7)
JD-GCSTALE gc-macro-fn-funcall-after    got=(#<?> 2 3)
JD-GCSTALE gc-closure-global-after      got=(#<?> #<?>)
JD-GCSTALE gc-closure-in-data-after     got=(#<?> #<?>)
JD-GCSTALE gc-closure-eval-result-after got=(#<?> #<?>)
JD-GCSTALE gc-closure-many-gcs          got=(#<?> #<?>)
JD-GCSTALE gc-sibling-clean-after       got=(#<?> #<?>)
JD-GCSTALE gc-fnaddr-value-after        got=:ERR
JD-GCSTALE gc-redef-v2-after            got=:ERR
```

## 5. Cost

```
tests/jit-census.lisp   NATIVE%  92 -> 86   (46/53 native; R-CONST-BAKED = 3)
tests/jit-diff.lisp     JD-NATIVE 297 -> 291
                        JD222-INSTALLED 27 -> 26
wall clock, 3 reps each, same box, same file:
   main 12.413 / 12.784 / 12.227 s      macro-gc 12.382 / 12.311 / 12.314 s
```

No measurable wall-clock cost on that workload.  The forms that lose native
execution are top-level installer forms and const-bearing lambda bodies, which
run once; hot code lives in function bodies, which are separate modules.  The one
lost #222 install is a dirty-sibling caller — i.e. one of the wrong answers.

## 6. Gate evidence

### JIT-off invariant — byte-identical

```
rm -f /tmp/modus-x64.bin
sbcl --dynamic-space-size 2048 --script mvm/build.lisp x64/bare/qemu/repl
md5sum /tmp/modus-x64.bin   ->  269b461a764016eea6533c46798ad3e4
```

Unchanged, confirmed **after deleting the stale artifact first**, and re-confirmed
after the second guard landed.  Both guards are inside `%jit-translate-page-1` /
`%jit-reloc-fn-addrs`, which exist only on the runtime-JIT path.

### 64-shard ANSI gate

Four images, each built into its own `MODUS_ANSI_OUT` **file** path, all four
`md5`-distinct, built sequentially (they share `tmp/ansi-test/sandbox`) and swept
sequentially with matched budgets (`NSH=64`, 600 s cap, ~280–400 s/shard).  BASE
is owned from `main` @ `9a3e48e`, not quoted from a previous report.

```
BASE-OFF: passed=17498 CHUNK-CRASH=0 FILE-WEDGE=30 (NSH=64)
BASE-ON:  passed=17498 CHUNK-CRASH=0 FILE-WEDGE=30 (NSH=64)
NET-OFF:  passed=17497 CHUNK-CRASH=0 FILE-WEDGE=30 (NSH=64)
NET-ON:   passed=17497 CHUNK-CRASH=0 FILE-WEDGE=30 (NSH=64)
```

`CHUNK-CRASH=0` and `FILE-WEDGE=30` on all four — the timing-immune markers are
flat.  `BASE-OFF` hits the stated 17498 exactly, which validates the method
before any conclusion is drawn from it.  `BASE-OFF` and `BASE-ON` pass sets are
**byte-identical** (`11bc5329b28f9cc8a645ecdd79ae4aa3`).

Per-ID diff, run 1:

```
BASE-OFF -> NET-OFF   lost P:14310   gained (none)
BASE-ON  -> NET-ON    lost P:13198   gained (none)
```

**The two runs lose DIFFERENT IDs.**  That is the flake signature, not a
regression — a real defect would lose the same test both times.  Both were
deterministically rechecked.

Single-ID isolation is **not** a valid recheck here and saying so matters: every
`./binary <id> <id>` run reports FAIL, on all four binaries including the two
that passed the test in the sweep, because a test needs the preceding forms of
its own file.  The valid recheck is the **containing shard range**, 3 reps:

```
id      shard range     base-off   base-on   net-off   net-on
13198   13070..13348    P P P      P P P     P P F     P P P
14310   14186..14464    P F F      P P P     P F F     P P P
```

`P:14310` (`random.4`) is on the known-flaky list, and it **fails 2 of 3 reps on
`main`'s own JIT-off image** — it is not this branch's. `P:13198` passes 2 of 3
on `NET-OFF` and 3 of 3 on `NET-ON`.  Neither loss reproduces; there is no
deterministic regression.

Honest caveat on the byte-identity bar: `NET-OFF` and `NET-ON` are **not**
byte-identical in run 1 — they differ by exactly those two flaky IDs, one in each
direction (`net-off` has 13198 and not 14310; `net-on` has 14310 and not 13198).
`BASE-OFF`/`BASE-ON` *were* identical in the same wave, which initially looked
like the bar being reachable and this branch missing it.  Run 2 shows otherwise.

### Run 2 — the byte-identity bar is not reachable, because a binary is not identical to ITSELF

Rather than argue about the two IDs, the same binaries were swept a second time
with the identical command.

```
              run 1     run 2
BASE-OFF      17498     17497
BASE-ON       17498       —
NET-OFF       17497     17496
NET-ON        17497     17497
```

Same-binary diffs — **no rebuild, no source change, the same file on disk**:

```
base-off  R1 vs R2   differs by  P:14310          <- main's OWN image
net-off   R1 vs R2   differs by  P:24168
net-on    R1 vs R2   differs by  P:13198, P:14488
```

`main`'s own JIT-off gate image scores **17498 on one run and 17497 on the next**,
losing `P:14310` — the same ID this branch "lost" in run 1.  Every ID that ever
differs between two binaries here is drawn from the same small pool that also
differs when a binary is compared against itself:

```
flaky pool observed:  P:13198   P:14310 (random.4, known)   P:14488   P:24168
```

So the "JIT-on and JIT-off pass sets must be byte-identical" bar cannot be met in
this environment — not by this branch and not by `main` — because a *single*
image's pass set is not byte-identical to itself across runs.  What can be said,
and is:

* every headline is within a 1-test band of 17498, on both branches;
* the timing-immune markers are flat: `CHUNK-CRASH=0`, `FILE-WEDGE=30`, on all
  six sweeps;
* no ID is lost deterministically — the shard recheck above passes every one of
  them in the majority of reps;
* `BASE-OFF` ≡ `BASE-ON` byte-identically in the run where nothing flaked, so
  the JIT itself is not introducing divergence;
* the only IDs ever implicated are in the flaky pool, one of which is the
  known-flaky `random.4` named in advance.

**Verdict: gate clean, no regression.**  If a stricter byte-identity result is
wanted, it needs a quiet box and a flake-quarantine list, not a code change here.

### JIT-off CLI oracle — not a usable gate, stated rather than glossed

A `MODUS_NO_JIT=1` CLI build runs `tests/jit-diff.lisp` to `JD-FAIL`, on this
branch and on `main` alike, and that is expected: the file documents itself as
requiring a JIT-capable image.  `JD-NATIVE=0`, so all 19 failures are the #222
native-installation assertions plus every `jd-gc-fired` — an interpreted
allocation loop does not reach the 939 MB threshold within the probe's 60 rounds,
so `jd-force-gc` returns `:no-collection`.  **Its `JD-GCSTALE-HITS=0` is
therefore vacuous and is not offered as evidence.**  The real JIT-off evidence is
the byte-identical bare-metal image above and the `BASE-OFF` vs `NET-OFF` shard
comparison.

## 7. The library payoff

`alexandria` `package` + `definitions` + `symbols` + `macros` loaded into a
running image, then **one real collection**, then the library's own macros used:

```
                                main @ 9a3e48e            macro-gc
mf-with-gensyms (after GC)      T                         T
wgs-expand (before GC)          (LET ((G (GENSYM "G")))   same
                                  (LIST G))
wgs-expand (after GC)           (:ERR SIMPLE-ERROR)       (LET ((G (GENSYM "G")))
                                                            (LIST G))
```

`alexandria:with-gensyms` — the single most-used macro in the ecosystem —
**signals** on `main` after one collection and expands correctly here.

Two honest caveats about this run:

* `macros.lisp` needs a `(setf documentation)` setter Modus does not have; the
  load aborts on `UNDEFINED-FUNCTION ALEXANDRIA::SET-DOCUMENTATION`.  A one-line
  no-op `set-documentation` was defined before the load.  **The library source is
  unmodified**; the shim supplies a missing runtime function.  It is identical on
  both images, so it does not affect the comparison.  It is a real gap and its
  own task.
* The remaining alexandria files were not loaded, so this is not a full
  `quickload`.

A second real library, `sha1` (via `ql:quickload` and via direct `load`), was
also run across three collections and is correct on **both** images — it defines
no macros, and its const-bearing functions all keep their trampolines, so it was
never exposed.  That is a useful negative control: it shows the bug is not "any
library breaks", it is "anything a JIT page produces that outlives it breaks".

## 8. Two other bugs found, reported not fixed

**(a) Runtime macro redefinition is invisible to an already-evaluated call
site.**  No GC anywhere; identical on `main`:

```lisp
(eval '(defmacro rm (a) (list 'list ''v1 a)))   (eval '(rm 5)) => (V1 5)
(eval '(defmacro rm (a) (list 'list ''v2 a)))   (eval '(rm 5)) => (V1 5)   ***
                                                (eval '(rm 6)) => (V2 6)
                                                (macroexpand-1 '(rm 5))
                                                               => (LIST 'V2 5)
```

`*mvm-eval-cache*` is keyed by `EQUAL` on the forms, so the second `(rm 5)`
replays the module compiled against the old expander.  `MACRO-FUNCTION` and
`MACROEXPAND-1` are both already correct, and a `DEFUN` redefinition **is**
honoured (a compiled call resolves its callee by name at call time, so it
late-binds); only the macro case is frozen.  The parallel fix already exists on
the native side — `mvm-eval-forms` drops `*jit-page-cache*` when a DEFUN is
redefined — so the shape is known.  It belongs in its own change with its own
gate, not smuggled into a GC fix.  Counter: `JD-MACRO-REDEF-HITS`.

**(b) The mid-form-collection residual** (§2).  Counter: `JD-GCRESID-HITS`.
Needs #226.

Both are on `main` today, both are reported on their own lines so `JD-GCSTALE-HITS`
stays a clean single-purpose headline, and both stop reporting when fixed.

## 9. Reproduce

```bash
sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp   # ~1m35s
./modus --load tests/jit-diff.lisp   --quit    # JD-GCSTALE-HITS = 0
./modus --load tests/jit-census.lisp --quit    # R-CONST-BAKED row

# minimal, no harness (main gives (#<?> 2 3); this branch gives (+ 2 3))
cat > /tmp/gcmac.lisp <<'EOF'
(defun gcn () (mem-ref #x10000060 :u32))
(defun force-gc () (let ((b (gcn)) (k 0))
  (loop (when (or (> (gcn) b) (>= k 60)) (return (- (gcn) b)))
        (dotimes (j 100000) (make-list 40)) (setq k (+ k 1)))))
(eval '(defmacro mac-add (a b) (list '+ a b)))
(format t "before = ~S~%" (macroexpand-1 '(mac-add 2 3)))
(format t "gc     = ~S~%" (force-gc))
(format t "after  = ~S~%" (macroexpand-1 '(mac-add 2 3)))
EOF
./modus --load /tmp/gcmac.lisp --quit

# NOTE: MODUS_NO_JIT is a BUILD-time variable.  To inhibit the JIT in a running
# image use (setq *jit-inhibit* t) — setting MODUS_NO_JIT on an already-built
# binary changes nothing and makes the bug look like an interpreter defect.

# ANSI gate, sequentially (the images share tmp/ansi-test/sandbox)
MODUS_ANSI_OUT=<dir>/base-off                  sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
MODUS_USE_JIT=1 MODUS_ANSI_OUT=<dir>/base-on   sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
   # (same two from this branch as net-off / net-on)
NSH=64 /home/claude/n5gate.sh <dir>/base-off base-off.out BASE-OFF     # one at a time
comm -23 base-off.out net-off.out    # lost
comm -13 base-off.out net-off.out    # gained

# JIT-off invariant
rm -f /tmp/modus-x64.bin
sbcl --dynamic-space-size 2048 --script mvm/build.lisp x64/bare/qemu/repl
md5sum /tmp/modus-x64.bin      # 269b461a764016eea6533c46798ad3e4
```
