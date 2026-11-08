# Task #237 — Modus presents as `:GENERA`

Branch `genera-feature`, off `main` @ `e4d26a8`.  Not pushed, not merged.

---

## 1. What was implemented

| file | what |
|---|---|
| `net/genera-compat.lisp` | the Genera surface + the `:genera` feature push (LAST) |
| `net/cooperative-atomics.lisp` | `%ATOMIC-CAS/-INCF/-DECF` + the verified cooperative-atomicity argument and SMP landmine warning (`COOPERATIVE-ATOMIC-PRECONDITION`), re-pointed from the earlier `:clasp` proposal to `:genera` |
| `mvm/build-generic-cli.lisp` | bakes both as a source string evaluated at boot; `MODUS_NO_GENERA=1` disables |
| `mvm/runtime-cl-macros.lisp` | **comment only** — the old "`:genera` reexports FUTURE-COMMON-LISP, which Modus lacks" claim is measurably false |

### Packages (`genera-compat.lisp:66-152`)

`SCL`, `SYS`, `SI`, `PROCESS`, `CLI`, `GRAY-STREAMS` — all `:use NIL`, pure
namespaces.  `FUTURE-COMMON-LISP` is installed as a **nickname of
COMMON-LISP** (`%genera-add-cl-nickname`, line ~110): on a real Genera, `LISP`
is CLtL1 and FUTURE-COMMON-LISP is the ANSI package; on Modus `COMMON-LISP`
*is* the ANSI package, so the nickname is the truth, and cl-ppcre's
`(:use #+:genera :future-common-lisp)` means exactly what its `#-:genera :cl`
branch meant.  A separate package that `:USE`s CL would **not** work — `:USE`
is not transitive, so every CL symbol would have to be re-exported by hand.

### Locative representation (`genera-compat.lisp:154-215`)

`SCL:LOCF`'s only consumer anywhere in the corpus is `SYS:STORE-CONDITIONAL`,
so both ends are ours.

**A locative is ONE closure of two arguments — a read/write dispatcher:**

```lisp
(scl:locf (svref v 0))
  => (lambda (op val) (if (eq op :read) (svref v 0) (setf (svref v 0) val)))
read  = (funcall loc :read nil)
write = (funcall loc :write new)
```

Why a closure and not a raw address: Modus's collector **copies** (Cheney
semispace), so a raw interior address handed to Lisp is invalidated by the
next GC, silently; a closure is an ordinary traced heap object.  It also works
for *any* setf-able place (locf's whole point) and needs no new object subtag,
so it cannot collide with `runtime/tags.lisp`.

Why **one dispatcher** and not the obvious `(cons READER WRITER)`: the pair
shape hits a **pre-existing Modus compiler bug**, found by probing, with a
plain `./modus` and no Genera code involved —

```lisp
(defvar *v* (make-array 1))
(defun k () (cons (lambda () (svref *v* 0)) (lambda () 1)))
(funcall (car (k)))     ; => UNHANDLED-ESCAPE, swallowed
```

Trigger: **two closures constructed inside one compiled DEFUN body where at
least one references a global**; funcalling either escapes.  One closure with
a global is fine; two closures over lexicals only is fine; the same
cons-of-two-lambdas written at toplevel is fine.  This mattered because LOCF
expands *inside* the caller's defun — bordeaux's
`ATOMIC-INTEGER-COMPARE-AND-SWAP` — i.e. exactly the wrong place.  The
dispatcher shape allocates one closure and sidesteps it, and is cheaper.

### Return conventions

`PROCESS:ATOMIC-INCF/-DECF` return the **NEW** value (bordeaux uses them
unwrapped, unlike its `#+sbcl`/`#+ecl` branches).  `SYS:STORE-CONDITIONAL`
returns `T`/`NIL`.  Verified behaviourally, not assumed — see §4.

---

## 2. What every other `#+genera` bordeaux site did once the feature was live

| site | effect | verdict |
|---|---|---|
| `apiv2/atomics.lisp:14,28,40` | the three `OPERATION-NOT-IMPLEMENTED` errors disappear; atomics work | **win** |
| `apiv2/api-threads.lisp:27` | `(make-weak-hash-table)` loses `:weakness :key`, so trivial-garbage no longer signals `"Your Lisp does not support weak key hash-tables"` — it just makes a strong table, which is all Modus could ever make | **win** (4th cleared bordeaux error) |
| `apiv2/api-threads.lisp:101` | skips `(*print-pprint-dispatch* (copy-pprint-dispatch nil))` | neutral/win — Modus has no `copy-pprint-dispatch` |
| `apiv2/api-threads.lisp:128,156` | adds `native-thread` to a `with-slots` and a `remove-thread-wrapper` call | neutral (slot exists) |
| `apiv2/timeout-interrupt.lisp:6,10` | **drops** `define-condition interrupt` and `defmacro with-timeout`; file goes 3 forms → 1 | **regression, see §5** |
| `apiv1/bordeaux-threads.lisp:53` | **drops** `defmacro with-timeout`; file 18 forms → 17 | **regression, see §5** |

---

## 3. ANSI gate

**The gate image is BYTE-IDENTICAL to the baseline.**

```
cmp /home/claude/ws-atomics/ansi-net /home/claude/ws-genera/ansi-net  ->  identical
```

The only shared-file change is a comment (the reader strips it), and `:genera`
is installed from the **CLI's** `kernel-main`, not from anything the ANSI image
bakes.  Confirmed independently: the ANSI corpus contains **zero** `genera`
tokens, so no reader conditional in it can move either.

Sweeps (`/home/claude/n5gate.sh`, NSH=64):

| run | binary | passed | CHUNK-CRASH | FILE-WEDGE |
|---|---|---|---|---|
| BASE rep1 (given) | `ws-atomics/ansi-net` | 17495 | 0 | 30 |
| BASE rep2 (mine)  | `ws-atomics/ansi-net` | 17494 | 0 | 30 |
| NET genera        | `ws-genera/ansi-net`  | **17496** | **0** | **30** |

Per-ID, NET vs BASE rep1: **lost 1** (`14310`), **gained 2** (`13448`,
`23242`).  All three are on the known-flaky list.

Control: the **same** baseline binary re-swept against itself moved
**lost 3 / gained 2** (`13202 13240 13448 14310 23242`) — i.e. the NET-vs-BASE
delta is strictly *smaller* than the binary's own run-to-run variance.

Shard recheck of the one loss (`14310` → shard 15, `14186..14464`), 2 reps
each:

```
rep1 BASE: present   rep1 NET: present
rep2 BASE: ABSENT    rep2 NET: present
```

It flips on the **baseline** binary.  Not a regression.

Gate binary left in place at `/home/claude/ws-genera/ansi-net`.

---

## 4. Ladder — full diff by error text

Baseline is a **fresh control run of my own binary with no prelude**
(`logs/g237-base`), which reproduces `logs/dostep` **exactly**: 28 = 28, every
library SAME.  Genera run = `logs/g237-genera`.  The **baked image** run
(`logs/g237-baked`, no prelude, `:genera` from boot) is **bit-identical to the
prelude run** — 21 errors, zero per-library differences.

### **28 → 21 errors.  Four libraries changed, all better.  None worse by count.**

```
bordeaux-threads       8 -> 4   BETTER
salza2                 3 -> 1   BETTER
trivial-gray-streams   2 -> 1   BETTER
trivial-garbage        0 -> 0   CHANGED (correctness fix, see below)
20 others              unchanged, byte-identical error text / file counts / probes
```

**bordeaux-threads 8 → 4** — cleared:

```
- OPERATION-NOT-IMPLEMENTED (ATOMIC-CAS)     apiv2/atomics.lisp
- OPERATION-NOT-IMPLEMENTED (ATOMIC-DECF)    apiv2/atomics.lisp
- OPERATION-NOT-IMPLEMENTED (ATOMIC-INCF)    apiv2/atomics.lisp
- SIMPLE-ERROR "Your Lisp does not support weak ~(~A~) hash-tables." (KEY)
                                             apiv2/api-threads.lisp
```

remaining 4 are the three SBCL-internal `READER-ERROR`s plus
`UNDEFINED-FUNCTION BT2::%MAKE-LOCK` downstream of them — unchanged.
Behaviour verified against bordeaux's own API (base → genera):

```
atomic-integer-incf     UNDEFINED-FUNCTION -> (1 6 6)     new-value semantics, accumulates
atomic-integer-decf     UNDEFINED-FUNCTION -> works
atomic-integer-CAS      UNDEFINED-FUNCTION -> works
1000x incf              UNDEFINED-FUNCTION -> 1000
```

**trivial-gray-streams 2 → 1** — the package/streams `READER-ERROR`s (the
literal `...` token in the unrecognised-implementation `:import-from` branch)
are gone; the package and its class hierarchy now exist:

```
- READER-ERROR  package.lisp        FILE package.lisp   1 -> 3  forms
- READER-ERROR  streams.lisp        FILE streams.lisp   1 -> 21 forms
+ PROGRAM-ERROR streams.lisp        (see §5)
  P1.pkg  0 -> 1     P1.cls  (:ERR READER-ERROR) -> 1
```

**salza2 3 → 1** — same two TGS reader errors plus its own:

```
- READER-ERROR  salza2-2.1/stream.lisp        FILE stream.lisp  2 -> 10 forms
+ PROGRAM-ERROR trivial-gray-streams/streams.lisp   (the same one, inherited)
```

**trivial-garbage 0 → 0 but CHANGED — a silent-wrongness FIX**:

```
P1.wp / P2.wp:
  BASE:   "If @code{weak-pointer} is valid, returns its value. Otherwise,"
  GENERA: 5
```

At baseline `trivial-garbage:weak-pointer-value` had an **empty body** (no
recognised implementation), so it returned its own docstring.  The `#+genera`
branch is a real (if strong-reference) implementation.  `make-weak-pointer`
likewise went from returning NIL to returning a real object, and
`weak-pointer-p` from an empty-bodied defun to the `defstruct` predicate.

**cl-ppcre 0 → 0, bit-identical** — the FUTURE-COMMON-LISP risk did not
materialise.  Same file counts, same probe results (`P1.match` /
`P1.subst` are `PPCRE-SYNTAX-ERROR` on *both* sides — a pre-existing gap).
The `#-:genera (declare (string string))` drops at `lexer.lisp:70`,
`util.lisp:177,185` changed nothing observable.

Full machine diff: `LADDER-DIFF-237.txt`.

---

## 5. What got worse, and what is silently wrong

### 5a. REGRESSION — bordeaux `with-timeout` disappears (apiv1 **and** apiv2)

`#-(or allegro clisp cmu genera sbcl)` and `#-(or sbcl genera)` guard the
*portable fallback*; claiming `:genera` skips it because a real Genera has
`process:with-timeout` natively and Modus does not.  Measured:

```
                       BASE   GENERA
bt2 with-timeout macro    1 ->   0
bt1 with-timeout macro    1 ->   0
```

**Severity: low.**  The baseline macro was not functional either — with no
`:thread-support` feature its whole body is `(error (make-threading-support-error))`
/ `(signal-not-implemented 'with-timeout)`, i.e. it discards the body and
signals.  So neither side runs user code; the difference is a clean
`NOT-IMPLEMENTED` condition versus an undefined macro.  It did not change any
ladder error count.

**The real fix is upstream-shaped and favourable**: bordeaux ships
`apiv1/impl-genera.lisp` and `apiv2/impl-genera.lisp`, selected by
`:if-feature :genera` in its `.asd`.  Under a correct ASDF-driven load with
`:genera` on, Modus would load those instead of `impl-sbcl.lisp` — which is 2
of the 4 *remaining* bordeaux errors.  They need ~26 more `PROCESS:` symbols
(`process-run-function`, `process-wait`, `make-lock`, `with-timeout`,
`atomic-updatef`, …) — and Modus already has a cooperative actor scheduler
(`net/actors.lisp`) shaped almost exactly like Genera's PROCESS package.
That is the obvious follow-on and would make bordeaux a *real* backend, not a
degenerate one.

### 5b. NEW ERROR — `PROGRAM-ERROR` in trivial-gray-streams, and it is a **Modus bug**

The one new error, in TGS `streams.lisp` form 21, is the `#+genera` block

```lisp
(defmethod gray-streams:stream-read-sequence
    ((s fundamental-input-stream) seq &optional start end) …)
```

Not a `&optional`-in-defmethod gap — that works fine in isolation.  Isolated
repro:

```lisp
(defpackage "PA" (:use) (:export "FOO"))
(defpackage "PB" (:use) (:export "FOO"))
(eval '(defgeneric pa:foo (s seq start end)))          ; => OK
(eval '(defmethod pb:foo ((x zc) seq &optional a b) …)) ; => PROGRAM-ERROR
(eq 'pa:foo 'pb:foo)                                    ; => NIL
```

**Modus's CLOS generic-function registry is package-blind.**  `PA:FOO` and
`PB:FOO` are distinct symbols but share one GF, so a congruence check runs
against the wrong lambda list.  TGS legitimately has *two* different
`stream-read-sequence` generic functions — its own portable one
`(stream sequence start end &key)` and the implementation's
`gray-streams:stream-read-sequence (stream seq &optional start end)`.  This is
the same class as the already-documented "shadowed-CL-fn clobber" (fn table
keyed by name-hash, not package); the fix pattern — folding the package hash
into the key for runtime-born packages — is known and applied elsewhere.
Cost today: 3 methods on GFs that are degenerate anyway (§5c).

### 5c. SILENTLY WRONG — `GRAY-STREAMS` is a namespace, not an implementation

This is the one place where `:genera` makes something *load* that does not
*work*, and it must not be oversold.  Probed on the baked binary:

```
define a subclass of trivial-gray-streams:fundamental-character-input-stream  -> OK
define a trivial-gray-streams:stream-read-char method on it                   -> OK
call that method directly                                                     -> #\a
(read-char <that object>)                                                     -> SIMPLE-ERROR
(streamp <that object>)                                                       -> NIL
```

So trivial-gray-streams now loads, but a Gray stream built through it is not a
CL stream and `cl:read-char` will not dispatch to it.  Before this change the
user got a clean total failure (package absent); now they get a package that
exists and a stream that isn't one.  **This is documented in
`net/genera-compat.lisp`'s KNOWN DEGENERACIES section**, and the real fix —
wiring Modus's stream dispatch through the Gray generic functions — is the
second obvious follow-on.

### 5d. SILENTLY WRONG (pre-existing, newly reachable) — `make-atomic-integer :value N` ignores N

```
(atomic-integer-value (make-atomic-integer :value 10))  => 0
```

`make-atomic-integer` does `(setf (atomic-integer-value aint) value)` through
bordeaux's `(defun (setf atomic-integer-value) …)`.  A plain
`(defun (setf foo) …)` + `(setf (foo x) v)` works in isolation, so this is a
narrower setf-function-resolution gap (there is an open `setf-function-registry`
line of work).  Not caused by `:genera` — but `:genera` is what makes the code
reachable, so it is now a live wrong answer rather than dead code.  Everything
that goes through `atomic-integer-incf/-decf/-CAS` after construction is
correct; only the `:value` initarg is dropped.

### 5e. Weak references remain fictional

The `#+genera` weak-pointer path keeps objects alive forever in a strong hash
table, and `SCL:MAKE-HASH-TABLE` accepts and ignores `:gc-protect-values`.
This is not a regression — Modus's non-genera trivial-garbage path has no weak
references either — but a program that *relies* on weakness to bound memory
will now grow instead of failing loudly.  Documented.

---

## 6. Wider corpus (beyond the 22-library ladder)

Reader-conditional `genera` sites in the whole 69-system local Quicklisp
corpus, outside the ladder:

| library | site | effect if it were loaded |
|---|---|---|
| `fiveam` `src/run.lisp:220,225` | `#+genera` uses plain format strings instead of `~@<…~@:>` | **favourable** — Modus's `~<` justify is a known gap |
| `pathname-utils` `toolkit.lisp:489,497` | `:genera` is now in the recognised set | **favourable** — avoids an unsupported-impl branch |
| `usocket` `package.lisp:5` | `(:use #+genera :future-common-lisp)` | **handled** by the nickname |
| `chipz` `stream.lisp:35,46,57,71` | `#+genera 'gray-streams:…` names now resolve; line 71 enables a `defmethod` on `gray-streams:stream-read-sequence` | **hazard** — same package-blind-GF collision as §5b.  `decompress.lisp`'s 6 sites need `#+(and genera gray-streams)` and stay off (we do not push `:gray-streams`). |
| `cl-json` `src/package.lisp:109` | `#+genera #:clos-internals` as a MOP package | **hazard** — would need a `CLOS-INTERNALS` package.  Broken without `:genera` too (no package designator at all), so not a regression. |
| `trivial-features` `src/tf-genera.lisp` | selected by `:if-feature :genera`; pushes `:little-endian` **and `:32-bit`** | **hazard, and a good example of "Genera predates ANSI"** — Genera was a 32-bit machine.  Modus is 64-bit.  Harmless today (the ladder driver hardcodes `tf-sbcl.lisp`) but it is exactly the stale-branch class to watch. |

---

## 7. Recommendation: **GLOBAL**, with two named follow-ons

Advertise `:genera` unconditionally, from boot, in the shipping image.  That
is what this branch does.

Evidence:

* **28 → 21 ladder errors.  Four libraries improved, zero regressed by count.**
* **20 of 24 drivers are bit-identical** — same error text, same per-file form
  counts, same probe values.  `:genera` is not a broad perturbation; it is a
  narrow, targeted one.
* The single biggest *risk* — cl-ppcre switching its `:use` to
  FUTURE-COMMON-LISP — measured as **zero change**, because on Modus the CL
  package genuinely is the ANSI package.
* It **fixed** a silently-wrong function (`weak-pointer-value` returning its
  own docstring).
* The only behavioural regression (§5a) is a macro that was already
  non-functional on both sides, and the path that removes it is the same path
  that would give Modus a *real* bordeaux backend.
* The ANSI gate cannot move: the gate image is byte-identical and the corpus
  has no `genera` token.

Per-library scoping is **not** warranted and would cost more than it saves:
no library takes a bad Genera branch, and scoping would mean the feature is
absent while a library's *dependencies* load — exactly where it matters
(trivial-garbage's weak-hash-table decision is made while loading
*bordeaux-threads*).

Two things must ship alongside it, and both are in the tree:

1. **`MODUS_NO_GENERA=1`** — one env var turns the whole thing off (feature,
   packages, boot cost).  Same reversible-flip discipline as `MODUS_NO_EVAL2`.
2. **The KNOWN DEGENERACIES section** in `net/genera-compat.lisp` — weak
   references are fictional, GC is not requestable, GRAY-STREAMS is a
   namespace not an implementation, and any `#+genera` branch should be
   behaviour-probed rather than trusted.

### Known cost

Boot time **1145 ms → 1499 ms (+354 ms, +31%)**; `MODUS_NO_GENERA=1` returns
it to 1134 ms.  The cost is that the compat surface is a *runtime-evaluated
source string*, because it defines symbols in packages that do not exist
outside a running Modus and `CHECK-PARSES` reads every build source with
SBCL's reader.  The way to remove the cost is to teach the MVM compiler those
packages so the definitions compile into the image — real work, separately
scoped, not this change.

### Follow-ons, in priority order

1. **Package-blind CLOS GF registry** (§5b) — a 3-line repro, a known fix
   pattern, and it unblocks both trivial-gray-streams' Genera methods and
   chipz's.
2. **Wire Modus's stream dispatch through the Gray generic functions** (§5c) —
   turns trivial-gray-streams from "loads" into "works", and is the thing that
   makes salza2/chipz streams real.
3. **`PROCESS:` on top of `net/actors.lisp`** (§5a) — ~26 symbols away from
   loading bordeaux's own `impl-genera.lisp` and having a real threading
   backend instead of a degenerate one.
4. **Two-closures-in-one-defun escape** (§1) — pre-existing, unrelated to
   Genera, minimal repro recorded in `net/genera-compat.lisp`.
5. **`(defun (setf …))` resolution** (§5d) — makes
   `make-atomic-integer :value N` honest.

---

## 8. Reproduction

```bash
# build
sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp     # -> ./modus (:genera on)
MODUS_NO_GENERA=1 ./modus --eval '(print *features*)' --quit           # rollback check

# ladder
/home/claude/lf/run.sh  <binary> <tag>
python3 tools/ladder-diff.py /home/claude/lf/logs/<baseA> /home/claude/lf/logs/<tagB>

# gate
MODUS_ANSI_OUT=$PWD/ansi-net sbcl --dynamic-space-size 4096 --script mvm/build-x64-linux.lisp
/home/claude/n5gate.sh $PWD/ansi-net $PWD/g-gen.out "NET genera"
```

Logs: `logs/g237-base` (control), `logs/g237-genera` (prelude),
`logs/g237-baked` (in-image) under `/home/claude/lf/logs/`.
