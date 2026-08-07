# Runtime-defined functions install as NATIVE code

Branch `jit-defun`, off `main` @ `65ef6cd`.  Unpushed.  Two source files:
`mvm/mvm-eval.lisp` (+218 lines, the mechanism) and `tests/jit-diff.lisp`
(the oracle extension), plus this document.

**The last JIT fallback reason is now 3 out of 53, down from 23.**

| | before (`main` @ 65ef6cd) | after (this branch) |
|---|---|---|
| census `R-RELOC-CALL-NONNATIVE` | **23** | **3** |
| census `R-PAGE-NIL` | 9 | 4 |
| census `R-RELOC-CALL-UNRESOLVED` | 1 | 1 |
| census `NATIVE` / `FALLBACK` / `TOTAL` | 44 / 9 / 53 | **49 / 4 / 53** |
| census **native%** | **83** | **92** |
| oracle `JD-DIVERGE` | 0 | **0** |
| oracle double-executions | 0 | **0** |
| oracle `JD-NATIVE` | 280 / 314 | **297 / 332** |
| oracle never-native `xform-*` shapes | 4 | **0** |

The three remaining `R-RELOC-CALL-NONNATIVE` hits are named and explained in §5;
they are `JC-RUN` (2) and `JC-REPORT` (1) — both const-bearing census-harness
functions — plus one unrelated `R-RELOC-CALL-UNRESOLVED` (`JC-M`, a CLOS method
name that does not resolve at all).

**ANSI gate, 64-shard, IDs 10001..27800, corpus 17 625 on all three images:**

| | passed | CHUNK-CRASH | FILE-WEDGE |
|---|---|---|---|
| `main` @ 65ef6cd, JIT-off (reference) | 17498 | 0 | 30 |
| this branch, JIT-off | 17498 | 0 | 30 |
| **this branch, JIT-on** | **17498** | **0** | **30** |

Per-ID: **lost = 0, gained = 0 on all three pairings.**  The three pass-ID files
are not merely the same size — they are **byte-identical**
(`md5 11bc5329b28f9cc8a645ecdd79ae4aa3`).  There is no delta to deterministically
recheck: not one ID differs anywhere, including the two tests recorded as flaky
in `GATE-RESULT-jit.md` (`P:14310` = `random.4`, `P:13448` = `/.9`), which
happened not to flip on this run.  This exactly meets the stated bar.

**`x64/bare/qemu/repl` = `269b461a764016eea6533c46798ad3e4`** — the `mvm/BUILDS.md`
hash, unchanged.  Rebuilt from this tree via `sbcl --script mvm/build.lisp
x64/bare/qemu/repl` after deleting the stale `/tmp/modus-x64.bin`; the first
attempt used a script name that does not exist in this tree
(`mvm/build-x64-repl.lisp`) and "confirmed" the hash off a stale artifact, which
is exactly the empty-result-is-not-a-measurement trap.

---

## 1. The mechanism

`mvm-eval` installed every runtime `DEFUN` as `%mvm-make-trampoline`
(`mvm/interp.lisp:393`) — a **heap closure**, tag nibble 9, whose body re-enters
`mvm-interpret`.  A later top-level form calling it needed an out-of-module CALL
relocation; `%jit-reloc-calls` requires `(eql (logand word 15) 3)` and correctly
refused, because the heap is mapped `PROT_READ|PROT_WRITE` with no `PROT_EXEC`.
The relocation failed, the page build failed, and the **entire calling module**
interpreted.

Nothing new has to be compiled.  The module the JIT already translates for the
top-level form **already contains native code for every function that form
defines** — same `translate-mvm-to-x64` output, same exec page, same ABI as a
build-time-native function.  All that was missing was publishing the entry
*address* instead of a trampoline:

* `translate-mvm-to-x64` returns `fn-map` (NAME → native label);
  `label-position` gives the byte offset.
* The exec page from `%mmap-exec-page` is 4096-aligned, and the in-image JIT
  co-init sets `*x64-native-code-offset*` to **0**
  (`build-generic-cli.lisp:284`, `build-ansi-common.lisp:352`), so the
  translator's function-entry alignment loop puts every entry at a **16-byte
  boundary inside the page**.
* Therefore the OR-3 discipline of `translate-x64.lisp` (`mvm-fn-addr` =
  LEA + OR-3, ~line 2794) applies verbatim: `(logior (+ base off) 3)` is a
  well-formed tag-3 native function word, disjoint from cons(1)/char(5)/obj(9).
  The alignment is **checked**, not assumed — a misaligned entry is skipped
  rather than published as a word whose low nibble could collide.
* `%word->val` reinterprets that word as the function VALUE — precisely the
  object `symbol-function` returns for a build-time function.  It is published
  into both tables the trampoline used: `*symbol-function-table*` by name (the
  `%mvm-resolve-runtime-fn` key that `%jit-reloc-calls` consults) and
  `*native-sym-function-table*` by name-hash (`symbol-function` / `funcall`).
  `%jit-reloc-fn-addrs` keeps patching the full TAGGED word, so
  `(eq #'f (symbol-function 'f))` still holds — asserted in the oracle.

**x64 only, by construction.**  The offset alist is element 6 of the jit-entry
returned by `%jit-translate-page-1`; the aarch64 sibling
(`%jit-translate-page-1-aarch64`) still returns its original 5-element entry, so
`(cadr (cddddr je))` is NIL there and `%jit-install-native-fns` is simply never
reached — aarch64 keeps trampolines and its behaviour is unchanged.  Porting it
needs the aarch64 in-module fn-addr relocation (`lrel`) applied to the installed
entry, which x64 gets for free from RIP-relative `LEA`, plus the existing
16-byte-alignment check that path already performs.

Three new functions in `mvm/mvm-eval.lisp`: `%jit-fn-native-offsets` (the
per-function offset/const analysis), `%jit-install-native-fns` (the publish),
`%jit-native-defuns-p` (a defun gate, not a defvar — defvar init-thunks do not
run in-image).  `%jit-translate-page-1` returns the offset alist as element 6 of
its jit-entry; `%mvm-eval-jit-run` takes a `persist-names` argument and installs
**after every relocation and const patch has succeeded** and **before
`%jit-call`**, so the module's own thunk already sees the native definition
exactly as it previously saw the trampoline.  On a page-build failure nothing is
published and the trampolines stand — the interpret fallback is byte-for-byte
unchanged.

## 2. Exec-page lifetime, and why it is safe

The page must outlive the compilation unit, and it does.  It is `mmap`'d memory
**outside the GC heap**: the collector never scans, moves or frees it, and a
tag-3 word on the stack is not a pointer tag, so the conservative root scan
ignores it.  x64 never `munmap`s — the `%jit-free-page` reclamation is gated to
`*jit-target-arch* :aarch64` *and* further gated on the result not being a
function — so no installed function's code can be pulled out from under it.
There is no use-after-free by construction.

The cost is that a **redefinition leaks its predecessor's page** (a few KB), and
it must: a caller that already baked the old address may still be live.  The
leak is bounded by redefinition count, not by call count.  On this branch it is
noted, not fixed.

## 3. The const-pool restriction — why installation is per-FUNCTION

`%jit-patch-consts` bakes each const-pool object's **current tagged heap
address** into a `movabs` immediate.  Those objects move under the Cheney
collector, and the existing re-bake (`%jit-entry-for`, keyed on `%gc-count`)
only fires when a page is re-entered **through the seam**.  A persistently
installed function is entered directly by native code and is never re-visited by
the seam, so nothing would re-bake it and a GC would leave it holding a dangling
from-space address — a hard corruption, not a wrong value.  There is no
Lisp-visible post-GC hook to fix this from inside `mvm-eval`: `gc_trampoline` is
emitted assembly (`translate-x64.lisp`) and never calls back into Lisp.

So a function is installed natively only when **no const patch site falls inside
its own native byte range**.  That is why `%jit-fn-native-offsets` computes
per-function ranges from `fn-map` rather than gating the whole page on
`(null cpatches)` — the thunk of `(defun f (x) (* x 2))` may carry consts that
have nothing to do with `f`.  Range ends are computed by scanning for the
smallest greater start (so the result does not depend on `ft-list` being sorted),
and the last function's end is the whole native length, which is conservatively
*over*-wide: over-wide can only reject a clean function, never admit a dirty one.
A name appearing twice in `ft-list` is skipped outright, because `fn-map` is
keyed by name and would alias the two.

**Measured boundary** (classification probe, shipping CLI, JIT on):

| installs NATIVE | keeps its trampoline |
|---|---|
| integer/float arithmetic, comparisons, `if`/`cond` | any **quoted literal** (`'foo`, `'(1 2)`) |
| `dotimes`/`loop`, self-recursion | any **string literal** (so `format`, `error`, docstring-free or not) |
| `&optional`, `&rest` | `&key` (the keyword names are consts) |
| `cons` / `list` / `make-array` / `make-hash-table` | **reading a special variable** |
| `values` (multiple values) | |
| closures (`(lambda (x) (+ x n))`) | |
| **`setq` of a special** (global WRITE) | |
| **calls to other runtime-defined functions** (the cascade) | |

The last two rows are what makes this useful: a runtime function calling another
runtime function relocates and goes native, so a loaded library natively links
against itself one definition at a time.

The **highest-value next step is the global READ row**, and its cause is exactly
one line.  Under `*mvm-eval-runtime-p*`, `compile-variable-ref`
(`mvm/compiler.lisp:4485`) compiles a special read as
`(%e2-symbol-value-checked <hash> (quote NAME))` — the quoted symbol exists only
to be the `:name` of a possible `UNBOUND-VARIABLE`, but it routes through
`:li-const` / `*e2-const-pool*` and disqualifies the whole function.  Fixing it
properly means one of:

1. an **indirection through a GC-updated constant vector** (the translator emits
   `movabs rax, <stable-cell>; mov rax, [rax]` and the collector treats the cell
   array as a root region) — the general fix, a translator + collector change;
2. **pinning** the const-pool objects (the MCGC Bartlett page-pinning work is
   already harvested and gated off — `%pin`/`%unpin` give stable addresses);
3. narrowly, passing the unbound-variable name as a **hash** and interning it
   only on the error path, which would move global READs into the native column
   without touching the collector.

None of these belongs in this change.

## 4. Redefinition

Two distinct problems, one fixed and one reported.

**Fixed — the cached-module path.**  `*jit-page-cache*` is keyed by bytecode
identity, so a second `eval` of an identical call form reuses a page whose
`movabs` sites were relocated against the *previous* definition.  The first
version of this change had exactly that bug: three successive `(defun jd-redef
…)` and the same `(eval '(jd-redef 5))` text returned the **second**
definition's answer for the third.  The invalidation has to happen in
`mvm-eval-forms`' trampoline install loop, because that is the **only** point at
which the name's previous binding is still visible — by the time
`%jit-install-native-fns` runs, the loop has already overwritten it (the
first attempt put the check there and it was dead code, and the probe caught it).
So: if a persist-name's previous binding was NATIVE (tag 3, whether build-time
or a `#222` install), the whole page cache is dropped.  It has to fire even when
the *new* definition ends up being only a trampoline, since the stale page still
holds the old native address.  A **first** definition never invalidates anything
— no page can have baked an address for a name that did not previously resolve
to native code — so loading a library pays nothing here.

**Reported, not fixed — early binding in an already-installed caller.**  Once
`jd-redef-caller` is itself installed as native code it has *baked* `jd-redef`'s
address, so a later redefinition of `jd-redef` is invisible to it.  The oracle
measures this explicitly and reports it as `JD222-EARLYBIND` (count 1) rather
than silently passing:

```
JD222-EARLYBIND d222-redef-via-2  got=-5  late-binding-would-give=35
```

This is a real divergence from the interpreter, which resolves by name on every
call and therefore late-binds.  It is also exactly the semantics build-time
native Modus code has always had (`CLAUDE.md` active limitation 1,
last-defun-wins: "All calls resolve to the LAST defun of a given name").  The
*direct* call always sees the newest definition (`d222-redef-direct`,
`d222-redef-symfn2` assert it).  A fix would be either the same
call-through-a-cell indirection as §3 option 1, or a per-page reverse index
(page → runtime callee names) that re-runs `%jit-reloc-calls` on the affected
pages when a name is redefined; the latter needs a demotion path for the case
where the new definition is *not* native, and transitively so, which is why it
is not in this change.

## 5. What the remaining 3 fallbacks are

```
NATIVE=49  FALLBACK=4  TOTAL=53  NATIVE%=92
     4  R-PAGE-NIL              (union of the RELOC/MMAP rows, not an independent cause)
     3  R-RELOC-CALL-NONNATIVE
     1  R-RELOC-CALL-UNRESOLVED
     0  R-MV  R-TRANSLATE-ERR  R-RELOC-FNADDR-FAIL  R-MMAP-FAIL  R-NATIVE-ESCAPE
-- blocked out-of-module CALL targets --
     2  JC-RUN    [heap-closure]
     1  JC-REPORT [heap-closure]
     1  JC-M      [unresolved]
```

* `JC-RUN` and `JC-REPORT` are the census harness's own functions.  Both are
  **const-bearing** (`jc-report` is nothing but `format` strings; `jc-run` holds
  a `handler-case` type literal), so they keep their trampolines by the §3 rule.
  They are the const-pool restriction, measured.
* `JC-M` is a CLOS method name that `%mvm-resolve-runtime-fn` cannot find at
  all — `R-RELOC-CALL-UNRESOLVED`, a different and pre-existing gap.

In the oracle, the never-native list lost all four `xform-*` shapes and gained
exactly the two deliberate boundary probes:

```
JD-NEVER-NATIVE:
  ecase-err  err-undef  clos-method  clos-around      <- unchanged, pre-existing
  d222-mutrec-call   <- split mutual recursion, see below
  d222-const-call    <- the const-bearing pair, by design
```

Both additions are deliberate boundary probes, not unexplained fallbacks.  The
one-form mutual-recursion probe (`d222-mutrec-1form-call`) does **not** appear
in the list — it reaches native.

### Forward references and mutual recursion — measured, and not what I first wrote

The first draft of this document and of the test claimed "mutual recursion is
one pass deep, not permanent."  **That was wrong, and the probe disproved it.**
The three cases are distinct:

| shape | result |
|---|---|
| plain forward reference (`a` calls not-yet-defined `b`) | **one pass deep** — `a` keeps its trampoline, `b` installs native, re-evaluating `a` once makes `a` native |
| mutual recursion in **separate** top-level forms | **never native**, at any number of passes |
| mutual recursion in **one** top-level form | **both native** |

Measured directly (`p1`/`p2`/`p3` = three full re-definition passes):

```
p1 M-EVEN  trampoline    p1 M-ODD  trampoline
p2 M-EVEN  trampoline    p2 M-ODD  trampoline
p3 M-EVEN  trampoline    p3 M-ODD  trampoline     <- never resolves
1f S-EVEN  NATIVE        1f S-ODD  NATIVE         <- same form: both native
f1 FWD-A   trampoline    f1 FWD-B  NATIVE
f2 FWD-A   NATIVE                                 <- one pass deep
```

The deadlock is exact: `m-even` can only relocate if `m-odd` is *already*
native, and `m-odd` can only relocate if `m-even` is — neither can go first.
Answers are correct in every row; the interpret fallback is doing its job.  The
limitation is narrower than it looks, because putting both definitions in one
top-level form (a `progn`, what a file compiler or `labels` produces) makes the
calls **in-module**, so no relocation is involved and both install.  All three
rows are now asserted in the oracle (`d222-mutrec-p1/p2-*`,
`d222-mutrec-1form-*`, `d222-fwd-*`).

## 6. The oracle — `tests/jit-diff.lisp`

```
JD-TOTAL=332   JD-DIVERGE=0   JD-NATIVE=297   JD-BOTH-SIGNALLED=1
JD222-TOTAL=46 JD222-FAIL=0   JD222-EARLYBIND=1   JD222-INSTALLED=23
JD-MV-TOTAL=21 JD-MV-NATIVE=21 JD-MV-CL-GAP=1 JD-MV-FALLBACK=0
JD-OK
```

Baseline on `main` for comparison: `JD-TOTAL=314  JD-DIVERGE=0  JD-NATIVE=280`,
with `xform-call`, `xform-nested`, `xform-2calls` and `xform-in-loop` in
`JD-NEVER-NATIVE`.

**Zero value divergences and zero double-executions**, both unchanged from
`main`.  The 314-form baseline is now 332 forms; the 46 new `#222` assertions
cover:

* cross-form call shapes — direct, nested, self-recursive, two calls in one
  form, in a `dotimes`, through `mapcar`/`funcall`/`apply`/`reduce`/`sort`, and
  a mixed in-module-defun-plus-cross-module-call form;
* **identity** — `(eq #'f (symbol-function 'f))`, `functionp`, `funcall` and
  `apply` of `symbol-function`;
* **the tag itself** — a runtime defun reads tag **3**, a build-time function
  (`#'car`) reads 3 as the control, and a deliberately const-bearing runtime
  defun reads **9**, so the boundary of the feature is asserted, not assumed;
* **redefinition** — three successive definitions with the *same* call-form text
  (to force the cached-module path), through `symbol-function`, and via an
  indirect caller (the early-binding case, §4);
* **mutual recursion and forward references** — all three rows of the table in
  §5, including the tag of each side;
* **GC** — define, allocate hard (`(dotimes (i 4000) (make-list 40))`) twice,
  and re-assert value, identity and tag afterwards, plus the same for the
  const-bearing sibling;
* **the native ABI** — 6 positional args, `&optional`, `&rest`, `&key`,
  multiple values out of an installed function, a function that `error`s (must
  propagate, must not double-execute), and closures returned from an installed
  function (two independent adders);
* **double execution** — three `jd-once` probes across the install boundary
  (defun-then-call, plain cross-form call, and a redefinition), each asserting
  the form's side effect ran exactly once.

Two probe traps worth recording, because both produced confident wrong readings
before being tracked down:

1. **`%val->word` is only reliable in native code.**  The interpreter itself
   works in the word domain (`reg-get` = `%val->word`, `reg-set` = `%word->val`,
   `interp.lisp:53`), so an *interpreted* `%val->word` double-converts and the
   low nibble reads back wrong — on the pre-`#222` binary the same expression
   returns **2** for `#'CAR`, which is unambiguously tag 3.  The tag probe
   therefore runs in a top-level form of its own (a bare `setq` whose only
   callees are build-time native) and stashes the result in a global.
2. Putting that expression inline as an argument to `jd-assert` does **not**
   work: `jd-assert` is itself a runtime-defined, const-bearing function that
   keeps its trampoline, so the whole calling form fails relocation and
   interprets — and the probe would have measured the interpreter's
   `%val->word`, not the installed function's tag.  This is the feature
   measuring itself; it took a `#'car` control to notice.

## 7. Actors

`actor-spawn` (`net/actors.lisp:277`) stores the entry function as
`(actor-set id #x30 (untag fn))` — a **raw continuation address the scheduler
jumps to**.  So "can a runtime-defined function be an actor entry" reduces
exactly to: *is `(untag (symbol-function 'f))` directly-jumpable executable
memory?*  Before this change the answer was structurally no — an untagged heap
closure is a heap address in a `PROT_READ|PROT_WRITE` mapping.

That precondition is now met, and it is measured rather than argued.
`%jit-call` is precisely the operation `actor-spawn` needs (untag a raw address
and indirect-call it), so:

```lisp
(defun ap-pure () 4242)                       ; runtime DEFUN, loaded at runtime
(setq *w1* (%val->word (symbol-function 'ap-pure)))
;; tag nibble = 3
(setq *r1* (%jit-call (- *w1* 3)))            ; == actor-spawn's (untag fn) + jump
;; => 4242, repeatably
```

Measured on the shipping CLI: `B1 pure tag = 3`, `B5 pure RAW-JUMP = 4242`,
`B6 pure RAW-JUMP2 = 4242`.  The same probe on a function that keeps its
trampoline reports tag 9 and does not execute.  **This is the first time a
runtime-defined function in Modus has been jumpable as a raw continuation.**

**It is not a running actor, and I did not build one.**  Nothing has ever run
actors under the JIT: exactly two build scripts pull in `net/actors.lisp`
(`build-pizero2w-actors.lisp` and `build.lisp`'s matrix cells) and neither
contains a single `jit` reference, while every JIT-capable build pulls in no
actor source.  The two sets are disjoint, and they are disjoint for a structural
reason, not an oversight: `net/actors.lisp` is written against per-actor heap
bases, a scheduler lock address and a save-area layout supplied by an
architecture adapter (`net/arch-aarch64.lisp`, `net/arch-raspi3b.lisp`,
`net/arch-x86.lisp`), and the JIT-capable images are Linux-hosted with no such
address space.  A real end-to-end test therefore needs a **new bare-metal image**
— realistically an aarch64-virt or bare-x64 build that loads `arch-*` + `actors`
alongside `mvm-eval` with `MODUS_USE_JIT=1`, plus:

* `*jit-target-arch*` set to the image's back-end at boot (already supported);
* the aarch64 JIT's in-module fn-addr relocation path
  (`%jit-translate-page-1-aarch64`'s `lrel` loop) exercised for the installed
  entry, which x64 gets for free from RIP-relative LEA;
* a `+op-yield+` that is a real safepoint — which native code has
  (`translate-x64.lisp:3488`, `compiler.lisp:8650`) and the interpreter does not
  (`interp.lisp:2042`, `(#.+op-yield+ nil) ; preemption: no-op`), and which is
  the whole reason actors need the JIT;
* a scheduler smoke test: spawn an actor whose entry is a runtime `DEFUN`,
  confirm it is enqueued, yields, and is re-scheduled.

Doing that half-way would prove nothing, so it is scoped out and described
instead.  What *is* established is that the one blocking primitive — an
executable, correctly-tagged, raw-jumpable address for a runtime-defined
function — now exists.

## 8. Invariants and hygiene

* **`x64/bare/qemu/repl` = `269b461a764016eea6533c46798ad3e4`** — unchanged.
  Measured properly: the stale `/tmp/modus-x64.bin` was deleted first, and the
  build was run through `mvm/build.lisp x64/bare/qemu/repl` (there is no
  `mvm/build-x64-repl.lisp` in this tree — the first attempt used that name,
  got a `FILE-DOES-NOT-EXIST`, and the `md5sum` that followed reported the
  *stale* file's hash, which happened to be the right answer for the wrong
  reason).
* **JIT-off is untouched by construction.**  Everything in
  `%jit-fn-native-offsets` / `%jit-install-native-fns` is reachable only from
  `%mvm-eval-jit-run`, which the seam only calls under `%jit-active-p`.  The one
  piece of new code on the shared path is the redefinition check in the
  trampoline install loop, and under JIT-off `*jit-page-cache*` is NIL so it is
  a `gethash` + a shift + a `logand` and never clears anything.
* **Corpus identity.**  All three gate images report `ANSI tests: 17625`, and
  their `ansi-file-ranges.txt` is byte-identical to the one `main`'s own gate
  build produced (`md5 90b30b1e8f6f281a7264f2128b6beda3`), so ID-based
  comparison is valid across every pairing.
* **The three sweeps ran strictly sequentially**, 64 shards, identical budgets,
  from one script — the images share
  `/home/claude/modus/tmp/ansi-test/sandbox`, and concurrent sweeps produce
  600 s shard truncation that reads exactly like a huge regression (contiguous
  runs of lost IDs are the tell).  Wall clock per sweep was **346 s / 375 s**
  for the second and third (the first reads 1205 s cumulative only because the
  script's clock starts while it waits for the third image to finish building);
  all well inside the healthy 280–400 s band and nowhere near the 600 s cap.
  All 64 shards produced output on every image, none truncated.
* **No perf regression on the gate.**  The JIT-on sweep is not slower than
  either JIT-off sweep, so neither the per-page `%jit-fn-native-offsets`
  analysis nor the redefinition `clrhash` of `*jit-page-cache*` costs
  measurable time on a 17 625-test corpus.
* **The pristine-`main` reference was built from this worktree**, by checking
  out `main`'s `mvm/mvm-eval.lisp`, building, and restoring — no other worktree
  was touched.  Its assembled source is 17 808 914 characters against this
  branch's 17 822 163, the difference being exactly the added source.
* **Disk.**  The repo carries ~900 MB of committed build binaries and the box
  had 1.2 GB free, so this worktree uses a `git sparse-checkout` that skips
  them (`gate-*`, `modus-ansi-*`, `modus-aa64-*`, `modus-206*`, `modus-fix*`,
  `modus-base`, `modus-p1`).  They are `skip-worktree`, so `git status` is
  clean and the branch content is unaffected; all gate artifacts were written
  to `/tmp/ws222*` (a different filesystem) rather than into `/home`.

## 9. Reproduce

```bash
# oracle + census (shipping CLI, JIT on by default) -- ~1m35s build
sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp
./modus --load tests/jit-census.lisp --quit    # R-RELOC-CALL-NONNATIVE, native%
./modus --load tests/jit-diff.lisp   --quit    # JD-DIVERGE must be 0, JD-OK

# ANSI gate, sequentially (the images share /home/claude/modus/tmp/ansi-test/sandbox)
MODUS_USE_JIT=1 MODUS_ANSI_OUT=<dirA>/bin sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
MODUS_ANSI_OUT=<dirB>/bin                 sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
NSH=64 /home/claude/n5gate.sh <dirB>/bin off.out JITOFF
NSH=64 /home/claude/n5gate.sh <dirA>/bin on.out  JITON
comm -23 off.out on.out    # lost
comm -13 off.out on.out    # gained

# JIT-off invariant
rm -f /tmp/modus-x64.bin
sbcl --dynamic-space-size 2048 --script mvm/build.lisp x64/bare/qemu/repl
md5sum /tmp/modus-x64.bin      # 269b461a764016eea6533c46798ad3e4
```
