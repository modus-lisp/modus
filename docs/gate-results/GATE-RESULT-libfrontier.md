# libfrontier — did today's JIT/GC correctness work move the library-loading frontier?

Branch `libfrontier` off `main` @ `b1654c3`.  Not pushed, not merged.

**Short answer.**  It moved something real but small, and it is not the thing
that was blocking the north star.  The macro-GC fix removes a **JIT-only
regression**: on `main`-before, turning the JIT on made three libraries load
*worse* than the interpreter did, and after the fix JIT-on is back at
interpreter parity.  No library goes from "fails" to "loads" because of it.
The thing actually blocking the ladder is a `SETF` bug that has nothing to do
with the JIT or the GC — and fixing that one line is worth more than the
entire day's GC work, measured on the same ladder.

Two findings that were not on the task list are more important than either:

* **A JIT-off Modus image cannot garbage-collect at all.**  `mvm/interp.lisp`
  treats `op-gc-check` as a no-op, so nothing running under `mvm-interpret`
  ever triggers a collection.  Measured: 2 GB allocated from interpreted code
  → `gc_count` still 0, then the image breaks.  JIT-on collects normally and
  is ~40× faster on the same loop.  This makes `MODUS_NO_JIT=1` unusable for
  any long-running workload, and it makes every JIT-off "survives GC" probe
  vacuous.
* **The task brief's "JIT is OFF by default in every shipping build" is wrong
  for `x64/hosted/-/cli`.**  `mvm/build-generic-cli.lisp` `*jit-on*` ends in
  `(t t)` — JIT ON is the default since WS5 #206.  All four ladder binaries
  were therefore built with the flag set explicitly, both ways.

---

## 1. Method

**Binaries.**  Five `x64/hosted/-/cli` images, all built from scratch:

| tag | commit | JIT |
|---|---|---|
| `pre-jit` | `9a3e48e` (= `b1654c3^2^^`, before the macro fix) | on |
| `pre-nojit` | `9a3e48e` | off (`MODUS_NO_JIT=1`) |
| `new-jit` | `b1654c3` (`main`) | on |
| `new-nojit` | `b1654c3` | off |
| `fix-jit` | `libfrontier` (`main` + the SETF fix below) | on |

**Ladder.**  22 libraries from a real Quicklisp dist
(`/home/claude/quicklisp/dists/quicklisp/software/`), sources **unmodified**.
For each library the host SBCL was asked for the real ASDF `load-op` plan and
its ordered `cl-source-file` list *including its dependency tree*; the driver
then loads exactly those files, in exactly that order, on Modus.  This
deliberately takes ASDF's own answer rather than Modus's `.asd` parser, so the
measurement is of the **library**, not of `install-tarball`.  (The
`install-tarball` / `ql:quickload` path is measured separately in §6.)

Each file is read+eval'd form by form via the image's own `%it-eval-source`, so
a bad form is *reported and skipped* rather than aborting the file — that is
what makes "how far did it get" a number instead of a boolean.  `*package*` is
saved/restored around each file (CL:LOAD is required to do this; the raw path
does not, which is itself a small gap).

After loading, each library gets **use probes** — a value computed by calling
the library's own functions and macros, guarded so one gap reports `:ERR`
instead of killing the run.  Every probe is then run **a second time after a
forced collection** (`P1.` = before GC, `P2.` = after GC).  That second pass is
the whole point: a define-time "loads clean" metric cannot see the macro bug.

One process per library, 8-way parallel, 1500 s cap.  Logs in
`/home/claude/lf/logs/<tag>/<lib>.log`.

**Harness discipline.**  The driver contains **no `DEFMACRO`** — a runtime macro
is exactly what is under test, so the instrument must not depend on one.
`gc_count` is read `:u32` (a `:u64` read of 1 comes back as a CONS).  `lf-say`
builds the whole `K=V` line before writing it, because on a JIT image the
translator prints to stdout *between* two `princ` calls and splits the record.

---

## 2. The measurement that matters: which failures moved

Load-time form errors across the whole ladder, JIT on:

```
pre-jit  (before macro fix)   326
new-jit  (main, macro fix)    289     <- macro fix:  -37
fix-jit  (+ SETF fix)         241     <- SETF fix:   -48
```

and the JIT-off control, which the macro fix cannot touch by construction:

```
pre-nojit  291
new-nojit  291                        <- macro fix:   0, exactly as expected
```

### Failures the macro fix DID move — all three, named

| library | evidence | pre-fix | main |
|---|---|---|---|
| `babel` | 35 load-time `UNDEFINED-FUNCTION` with a **NIL name** (a garbage expansion head) disappear | 185 errors | 150 errors |
| `cl-ppcre` | 2 × `UNDEFINED-FUNCTION NAME="G82219"` — a **gensym** as a function name, i.e. a stale expansion | 6 errors | 4 errors |
| `alexandria` | `(macroexpand-1 '(alexandria:with-gensyms (g) (list g)))` **after one collection** | `(:ERR SIMPLE-ERROR)` | `LET` |

The mechanism is visible in the data and not merely asserted: `babel` and
`cl-ppcre` are the **only two libraries in the ladder whose load triggers a
collection at all** (`LF-GC-1 = 1`; every other library is 0), and they are
exactly the two whose load-error count moved.  `alexandria`'s load does not
collect, so its regression only shows up in the `P2.` (post-GC) probe.

The control settles what kind of movement this is.  On the JIT-off images
`babel` scores **150** and `cl-ppcre` **4** — the same as `main` JIT-on, and
better than pre-fix JIT-on.  So the 37 recovered failures were **caused by
turning the JIT on**, and the fix restores parity with the interpreter.  It
does not take any library past what interpretation already achieved.

### Failures the macro fix did NOT move — everything else

All 19 remaining libraries score **identically** on `pre-jit` and `new-jit`.
This is checked, not eyeballed: for each library the sorted set of `LF-FILE`
lines, `P1.`/`P2.` probe lines and `!!` error texts is compared between the two
runs, and exactly three libraries differ — `alexandria`, `cl-ppcre`, `babel`,
the three named above.  The other 19 are string-equal.  `split-sequence`,
`bordeaux-threads`,
`documentation-utils`, `named-readtables`, `puri`, `cl-annot`, `md5`,
`iterate`, `trivial-gray-streams`, `cl-base64`, `salza2`, `trivial-indent`,
`trivial-features`, `cl-utilities`, `global-vars`, `parse-float`,
`ieee-floats`, `trivial-garbage` — zero difference.

**No library changed status from "fails" to "loads" because of the macro fix.**
Said plainly: today's correctness work was right on its own terms and it closed
a real JIT-only regression, but it did not buy the north star.  The reason is
structural and is now measured rather than guessed: **a library load does not
allocate enough to collect.**  20 of 22 libraries complete their entire load
with `gc_count` still at 0.  The hypothesis in the brief — "libraries are mostly
macros and a real load allocates enough to collect" — is false for the second
half.  Where a load *does* collect (`babel`, `cl-ppcre`), the fix paid off
immediately and exactly as predicted.

---

## 3. Per-library before / after

`files` = source files loaded (ASDF order, deps included).  `load errors` =
top-level forms that signalled, counted `pre-jit / new-jit / fix-jit`.
Probes are `passing/total`, `before GC -> after GC`.

| library | files | load errors pre-fix / macro-fix / +setf-fix | use-probes pre / post GC (macro-fix) | use-probes (+setf-fix) | GCs during load |
|---|---|---|---|---|---|
| `alexandria` | 22 | 1 / 1 / **0** | 15/15 -> 15/15 | 15/15 -> 15/15 | 0 |
| `split-sequence` | 6 | 3 / 3 / **0** | 2/2 -> 2/2 | 2/2 -> 2/2 | 0 |
| `cl-utilities` | 11 | 0 / 0 / **0** | 2/3 -> 2/3 | 2/3 -> 2/3 | 0 |
| `trivial-features` | 1 | 1 / 1 / **1** | 2/2 -> 2/2 | 2/2 -> 2/2 | 0 |
| `global-vars` | 1 | 4 / 4 / **0** | 1/1 -> 1/1 | 1/1 -> 1/1 | 0 |
| `trivial-gray-streams` | 2 | 2 / 2 / **2** | 1/2 -> 1/2 | 1/2 -> 1/2 | 0 |
| `trivial-indent` | 1 | 2 / 2 / **2** | 1/2 -> 1/2 | 1/2 -> 1/2 | 0 |
| `documentation-utils` | 4 | 24 / 24 / **23** | 2/2 -> 2/2 | 2/2 -> 2/2 | 0 |
| `named-readtables` | 11 | 19 / 19 / **19** | 1/2 -> 1/2 | 1/2 -> 1/2 | 0 |
| `bordeaux-threads` | 38 | 42 / 42 / **8** | 3/4 -> 3/4 | 3/4 -> 3/4 | 0 |
| `iterate` | 2 | 1 / 1 / **1** | 1/3 -> 1/3 | 1/3 -> 1/3 | 0 |
| `cl-ppcre` | 17 | 6 / 4 / **2** | 1/3 -> 1/3 | 1/3 -> 1/3 | 1 |
| `md5` | 1 | 1 / 1 / **1** | 0/1 -> 0/1 | 0/1 -> 0/1 | 0 |
| `sha1` | 1 | 0 / 0 / **0** | 1/1 -> 1/1 | 1/1 -> 1/1 | 0 |
| `cl-base64` | 3 | 4 / 4 / **4** | 0/2 -> 0/2 | 1/2 -> 1/2 | 0 |
| `parse-float` | 24 | 1 / 1 / **0** | 1/1 -> 1/1 | 1/1 -> 1/1 | 0 |
| `ieee-floats` | 1 | 0 / 0 / **0** | 2/2 -> 2/2 | 2/2 -> 2/2 | 0 |
| `trivial-garbage` | 1 | 0 / 0 / **0** | 2/2 -> 2/2 | 2/2 -> 2/2 | 0 |
| `babel` | 51 | 185 / 150 / **149** | 1/3 -> 1/3 | 1/3 -> 1/3 | 1 |
| `salza2` | 21 | 3 / 3 / **3** | 1/1 -> 1/1 | 1/1 -> 1/1 | 0 |
| `puri` | 1 | 10 / 10 / **10** | 0/1 -> 0/1 | 0/1 -> 0/1 | 0 |
| `cl-annot` | 33 | 17 / 17 / **16** | 1/1 -> 1/1 | 1/1 -> 1/1 | 0 |

---

## 4. JIT-on vs JIT-off

Beyond the parity story above, the two modes differ in ways that matter more
than the ladder scores.

**JIT-off never collects.**  Staged allocation from interpreted code, same
binary pair, same loop:

```
                 gc_count after N x (make-array 100000)
   N =            200   400   800  1600  3200  6400  12800     wall
   JIT off          0     0     0     0     -     -      -    >100 s to N=1600, then unusable
   JIT on           0     0     0     1     2     5     10      2.8 s to N=12800
```

At N=1600 the JIT-off image has allocated 1.28 GB and collected nothing; RSS
keeps climbing and later reads of the GC counter signal `TYPE-ERROR`.  Three
ladder runs on the JIT-off images exit non-zero for exactly this reason (`sha1`,
`ieee-floats`, `iterate` — all *after* their probes passed).  The cause is one
line: `mvm/interp.lisp:1952`, `(#.+op-gc-check+ nil) ; no-op in interpreter`.
Nothing else triggers a collection from interpreted code.

This has a direct consequence for how JIT work is validated: **any "survives a
collection" probe run on a JIT-off image is vacuous**, because no collection
happens.  `GATE-RESULT-macro-gc.md` already says this about `JD-GCSTALE-HITS`;
the finding here is that it generalises to the whole image, and that JIT-off is
not merely slower but has no memory reclamation at all.

**JIT-on is ~40× faster** on allocation-heavy code (2.8 s vs >100 s above) and
is the shipping default, so it is the mode the frontier should be measured in.

**Correctness, JIT-on vs JIT-off, after the macro fix:** identical on the whole
ladder except the two libraries above, where they now agree.  Before the macro
fix, JIT-on was strictly worse.  There is no case in the ladder where JIT-on is
better or worse than JIT-off on `main`.

---

## 5. The new frontier — ranked, root-caused

241 load-time form errors remain across the ladder on `fix-jit`.  Grouped by
actual cause, not by symptom:

### #1 — `(setf (foo x) v)` ignores `#'(setf foo)` — **~135 errors, 6 libraries**

`compile-setf`'s generic fallback rewrites `(setf (foo a…) v)` into
`(SET-FOO a… v)`.  CLHS requires `(funcall #'(setf foo) v a…)`.  Modus
*does* define the function — but under the other name:

```lisp
(defun (setf gg) (v k) …)
(fboundp '(setf gg))        => T
(funcall #'(setf gg) 7 :a)  => 7        ; works
(fboundp 'set-gg)           => NIL
(setf (gg :a) 7)            => UNDEFINED-FUNCTION SET-GG
```

Hits: `babel` (97 — `SET-GET-ABSTRACT-MAPPING`, and it is what keeps babel at
149 errors), `documentation-utils` (20), `cl-annot` (16), `named-readtables`,
`trivial-indent`, `bordeaux-threads`.  **Shape of the fix:** in the fallback,
emit `(funcall #'(setf ACC) value args…)` when `(setf ACC)` is fbound, keeping
`SET-ACC` as the fallback for `defstruct`/`defclass` accessors.  Deliberately
NOT landed here — the fbound test is a runtime question asked at macroexpansion
time and getting the ordering wrong would silently break every struct setter,
so it wants its own change and its own gate.

### #2 — `ASSERT` is a FUNCTION, not a macro — `cl-base64`, and a landmine

`mvm/cl-sequences.lisp:833` defines `(defun assert (test-form &rest ignored) …)`.
CLHS requires ASSERT to be a **macro**, and its second argument is a list of
*places*, not a form.  As a function, every argument is evaluated:

```lisp
(let ((x 5)) (assert (< x 10) (x) "too big ~S" x))
  => UNDEFINED-FUNCTION X          ; it CALLED (x)
```

That is exactly `cl-base64`'s blocker — `package.lisp`'s `make-decode-table`
contains `(assert (< (length encode-table) 128) (encode-table) …)`, so the
`defun` fails, and `+decode-table+` / `+uri-decode-table+` are then unbound.
Four of `cl-base64`'s four errors are this one cause.

Deliberately not landed despite looking like a one-liner: the function's own
docstring records that a previous attempt to give ASSERT real behaviour was
**net −31 ANSI**, and the ANSI corpus has 1000+ ASSERT call sites whose
arguments currently *do* get evaluated.  Turning it into a macro changes all of
them at once.  It wants its own change and its own gate.

### #3 — a library's own macro mis-expands — `puri` (10 errors, one cause)

Every `puri` error traces to one macro.  `puri` defines its own Allegro-style
`if*`; Modus expands it and **puri's own code signals** `if*: bad keyword ELSE`
on `(if* (and except …) thenret else (setf …))`.  Those failing `defun`s then
make the four `(defparameter *reserved-characters* (reserved-char-vector …))`
forms report `UNDEFINED-FUNCTION PURI::RESERVED-CHAR-VECTOR`, and the file
finally read-errors on `GEN-CHAR-RANGE-LIST`.  So `puri` is not ten gaps, it is
one: `if*`'s `(do ((xx (reverse args) …)))` walk sees a different argument list
than it should.  A `&rest`/argument-passing issue in a runtime-defined macro is
the leading suspect; needs its own bisect.

### #3b — macroexpansion-time helpers — ~15 errors

`EDITOR-HINTS.NAMED-READTABLES::DOCSTRING` (13), `%STANDARD-READTABLE`,
`%CLEAR-READTABLE`, `BT2::%MAKE-LOCK`.  A function defined for
macroexpansion-time use earlier in the same file is not visible when a later
form in that file expands.

### #4 — CLOS `make-instance` initarg validation — 33 errors, `babel` only

`make-instance: invalid initarg LITERAL-CHAR-CODE-LIMIT`.  Plain slot-initarg
inheritance *does* work (`(defclass cb (ca) …)` + `:s1` inherited → OK), so it
is narrower than "inheritance is broken".  Separately confirmed:
**`:default-initargs` is silently ignored** — `(defclass cc () ((s :initarg :s
:initform 0)) (:default-initargs :s 3))` → `(slot-value (make-instance 'cc) 's)`
returns `0`, should be `3`.

### #5 — reader errors from the `#-(or sbcl cmu …)` fallback branch — 14 errors

Modus is in no library's `#+impl` allowlist, so the *portable fallback* branch
is the live one, and that branch typically references a portability library's
package that is not loaded — a package-qualified symbol for a missing package is
a hard `READER-ERROR`, which **truncates the rest of the file**.

| file | truncates after form | reason |
|---|---|---|
| `trivial-features/src/tf-sbcl.lisp` | 1 | `sb-alien:` — ASDF picked SBCL's file; there is no Modus file |
| `md5/md5.lisp` | 43 | `flexi-streams:string-to-octets` in the `#-(or :cmu :sbcl …)` branch |
| `trivial-gray-streams/package.lisp` | 1 | reads the literal token `...` from `#-(or sbcl …) ...` — genuinely invalid CL; **not a Modus bug** |
| `salza2/stream.lisp` | 2 | `trivial-gray-streams:` — cascade from the above |
| `bordeaux-threads/apiv{1,2}/impl-sbcl.lisp` | — | `sb-thread:` |

Reader conditionals themselves are **fine** — `#+`/`#-`, nested `(and (or (not …)))`,
`#.` and package-qualified symbols under `*read-suppress*` all verified correct.
The mitigation is a lenient reader that defers an unknown package rather than
erroring (Modus already does this for `modus.*` qualifiers, commit `67564c5`).

### #6 — missing standard globals — 8 errors, cascading much wider

* **`char-code-limit` is unbound in the shipping CLI.**  This is `cl-ppcre`'s
  actual blocker: its two `UNBOUND-VARIABLE CHAR-CODE-LIMIT` forms mean its
  charset tables never initialise, and every regex then fails with cl-ppcre's own
  `PPCRE-SYNTAX-ERROR` — the library loads 17/17 files and cannot match `"b+"`.
  Deliberately not landed: the value is a real trade-off, not a typo.
  `code-char` accepts at least 1114112 on Modus, so a conforming value is
  1114112 — but the ANSI gate images set it to **256** on purpose
  (`build-x64-linux.lisp:1145`), and CLAUDE.md records the `array-rank-limit`
  incident where a large limit turned a loop bound into a suite-wide timeout.
  Landing this needs a decision on the value plus an explicit `setq` (limitation
  #7: a `defvar` initform in a runtime file reads back NIL).
* **`*modules*` is unbound** — `trivial-indent`'s only blocker.  Zero-risk on its
  own (nothing loops over it) but it is the same shared-file/gate cost.

### #7 — `iterate`'s `#L` reader macro is not honoured — truncates at form 45

`iterate.lisp` installs `#L` via `(set-dispatch-macro-character #\# #\L …)` in an
earlier top-level form, then uses `#L(list …)` at line 731.  The read fails
(with a NIL condition), so 45 of the file's forms load and `iterate:iter` is
unusable.

### #8 — small, single-library

`bordeaux-threads`: `atomic-incf/decf/cas` `OPERATION-NOT-IMPLEMENTED`, and weak
hash-tables reported unsupported (3 + 1 errors).  `babel`: 9 x `PROGRAM-ERROR
NIL` in `enc-unicode`.

---

## 6. The `ql:quickload` path

`install-tarball` / `ql:quickload` parses the real `.asd` and loads the real
sources.  End to end on `fix-jit`, from a staged tarball of the unmodified
Quicklisp checkout:

```lisp
(load "modus-quicklisp/setup.lisp")
(ql:quickload :alexandria)          ; -> install-tarball: done, system=alexandria
(alexandria:flatten '(1 (2 (3))))   ; -> (1 2 3)
(alexandria:with-gensyms (a b) …)   ; -> (T T NIL)
(funcall (alexandria:curry #'+ 10) 5) ; -> 15
```

**Zero load errors on `fix-jit`; one (`SET-DOCUMENTATION`) on `main` and on
`pre-jit`.**  Note honestly: this worked — values and all — on all three
binaries.  `ql:quickload :alexandria` was **not** blocked before today's macro
fix and is not unblocked by it; a quickload-and-use session is too short to
collect.  What the macro fix buys is that the same session stays correct once
something *does* collect.

Two `.asd`-path gaps found:

* `install-tarball` does **not** restore `*package*`, so the form after a
  `quickload` is READ in the library's package.  CL:LOAD is required to bind it.
* `split-sequence.asd` opens with `#.(unless (or #+asdf3.1 (version<= "3.1"
  (asdf-version))) (error "You need ASDF >= 3.1 …"))`.  Without `:asdf3.1` in
  `*features*` the `#+` reads as absent, the `(or)` is NIL, and the read-time
  eval fires the error — so the whole `.asd` fails to read:

  ```
  install-tarball "split-sequence.tar"
    using asd: split-sequence-v2.0.1/split-sequence.asd
    -> SIMPLE-ERROR "You need ASDF >= 3.1 to load this system correctly."
  ```

  `*features*` on the CLI is `(:LINUX :UNIX :COMMON-LISP :CL :ANSI-CL :MODUS)`.
  Every `quickload` of a system with an ASDF-version guard hits this.  The two
  ways out are (a) push `:asdf3.1` — cheap, and a lie, since `install-tarball`
  is an ASDF subset; or (b) have the `.asd` reader tolerate a signalling `#.`
  by skipping that form.  Neither is obviously right, so neither is landed.
  Note this affects only the `.asd` path — `split-sequence` itself loads its
  6 files with **0** errors when driven from the ASDF-computed order.

---

## 7. The cheap fix I landed

**`39b8eee` — SETF fallback: intern `SET-<NAME>` in the accessor's package, not
`*package*`.**  `mvm/compiler.lisp`, one `or` clause.

`(setf (documentation 'f 'function) "d")` signalled `UNDEFINED-FUNCTION
FOO::SET-DOCUMENTATION` inside every library that had done `(in-package :foo)`.
Host-side `MODUS.MVM` always resolves so the build never saw it; in-image there
is no `MODUS.MVM`, so it fell back to `*package*` — the *library's* package —
and `%FN-KEY-ANSI-PKG-P` exempts only `COMMON-LISP` / `COMMON-LISP-USER` /
`KEYWORD` from the function-table package fold, so that name could never reach
the image's function.  It worked at the REPL only because the REPL sits in
CL-USER.

Minimal repro, inside a runtime-made package, before → after:

```
(setf (documentation 'zz 'function) "d")   ERR -> "d"
(setf (char (copy-seq "abc") 0) #\x)       ERR -> "xbc"
(setf (lfs-a s) 5)   ; defstruct              5 -> 5     (unchanged)
(setf (gethash :a h) 3) / (aref v 0) / (car c)          (unchanged, already intercepted)
```

Ladder effect (JIT on): **289 → 241** load errors, **zero libraries regressed**.

| library | errors before → after |
|---|---|
| `alexandria` | 1 → **0**  (22/22 files, fully clean) |
| `split-sequence` | 3 → **0** |
| `global-vars` | 4 → **0** |
| `parse-float` | 1 → **0** |
| `bordeaux-threads` | 42 → **8** |
| `cl-ppcre` | 4 → 2 |
| `documentation-utils` / `babel` / `cl-annot` | −1 each |
| `cl-base64` | `(cl-base64:string-to-base64-string "abc")` `UNDEFINED-FUNCTION` → `"YWJj"` |

`MODUS.MVM` stays first in the `or`, so the host build is untouched and SBCL is
never asked to intern into its locked `CL` package.

### Gate

*(64-shard ANSI sweep in progress — see below.)*

---

## 8. What I would queue next, in order

1. **`(setf (foo x) v)` must dispatch to `#'(setf foo)`** (blocker #1).  ~135 of
   the 241 remaining errors, 6 libraries, plain CLHS conformance.
2. **`ASSERT` as a macro** (blocker #2).  Small, but it touches 1000+ ANSI call
   sites, so it needs the gate and the historical -31 note taken seriously.
3. **`char-code-limit`** with a decided value + an explicit `setq` (limitation
   #7).  This is what unblocks `cl-ppcre` end to end, and `cl-ppcre` is the
   gateway to a large slice of the ecosystem.
4. **Make the interpreter honour `op-gc-check`**, or state and enforce that
   JIT-off is a debugging mode only.  As it stands `MODUS_NO_JIT=1` produces an
   image with no memory reclamation at all.
5. **A lenient reader for unknown packages** (extend the `modus.*` deferral to
   any package).  Turns 5 hard file truncations into skipped forms.
6. `puri`'s `if*` (a runtime-macro argument bug worth bisecting on its own);
   `:default-initargs`; macroexpansion-time helper visibility; `iterate`'s `#L`;
   `*modules*`.

## 9. Reproducing

```
/home/claude/lf/gen.py        generate the 22 drivers (+2 quickload drivers)
/home/claude/lf/run.sh <bin> <tag>    run the ladder, 8-way parallel
/home/claude/lf/table.py      the per-library before/after table
/home/claude/lf/logs/<tag>/   raw per-library logs
/home/claude/lf/diag[1-8].lisp   the minimal repros quoted above
```
