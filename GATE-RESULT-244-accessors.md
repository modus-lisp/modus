# Task #244 item 2 — DEFSTRUCT accessors do no type check

Branch `task244-struct-accessor-typecheck`, off `c2896ac`.

## 1. The gap is systematic, and worse than the ticket described

Measured on the reference CLI (`/home/claude/ws-mkinst/modus-fix2`) with
`probes-scratch/survey.lisp`.  Every generated reader, writer and copier is
affected, in every variant — default conc-name, custom `:conc-name`, `:include`
parent and child, BOA constructor:

| call | before | SBCL |
|---|---|---|
| `(zs-a nil)` | SIGSEGV, recovered as SIMPLE-ERROR | TYPE-ERROR |
| `(zs-a 5)` | SIGSEGV, recovered as SIMPLE-ERROR | TYPE-ERROR |
| `(zs-a "hi")` | **returns garbage, no signal** | TYPE-ERROR |
| `(zs-a 'foo)` | **returns FOO, no signal** | TYPE-ERROR |
| `(zs-a (cons 1 2))` | **returns NIL, no signal** | TYPE-ERROR |
| `(zs-a (make-cn …))` | **returns the sibling's slot** | TYPE-ERROR |
| `(derived-dz (make-base …))` | **reads past the object** | TYPE-ERROR |
| `(setf (derived-dz (make-base …)) 9)` | **WRITES past the object** | TYPE-ERROR |
| `(setf (ro-r1 x) 42)` | succeeds — `:read-only` ignored | error, refused |
| `(copy-zs nil)` | SIGSEGV, recovered as SIMPLE-ERROR | TYPE-ERROR |

The silent-garbage rows are worse than the SIGSEGV the ticket reported, and the
out-of-bounds **write** is a heap-corruption vector rather than a wrong answer.

## 2. Design, and why

Guard emitted in the prologue of every reader, writer and copier
(`mvm/compiler.lisp`, the DEFSTRUCT arm of `mvm-compile-toplevel`):

```lisp
(if (= (obj-subtag obj) #x32)                     ; +SUBTAG-ARRAY+
    (if (>= (%prim-array-length obj) MIN-INST-LEN); 2 + NSLOTS
        (%prim-aref obj IDX)
        (%signal-type-error))
    (%signal-type-error))
```

* `obj-subtag` is **tag-safe by construction** — `translate-x64.lisp`'s
  `+op-obj-subtag+` returns 0 for any non-tag-9 word, `T` included — so this one
  compare legally rejects NIL, T, fixnums, characters and conses, plus strings
  (#x31), symbols (#x50), floats, bignums, hash-tables, closures and native
  MDAs (#x34).
* The length test bounds the access **and** rejects a parent instance passed to
  a child's accessor, while an `:include` child (which is longer) still passes
  the parent's accessors, as CLHS requires.

**Rejected: checking the `'%struct-instance` marker or the slot-1 type name.**
Measured, not assumed — in the CLI

```
(eq (aref (make-zs :a 1 :b 2) 0) '%struct-instance)  =>  NIL
```

that literal is not `EQ` across compilation units.  The type predicate gets
away with it only because it is emitted in the *same expansion* as its
constructor (its own comment says so).  An accessor must additionally accept
instances built by `%ALLOC-STRUCT`, by the `#S` reader and by a copier in
another unit, so a marker `EQ` would reject valid structs.  The slot-1 type
name has the identical identity problem, and the `:include` ancestry that would
rescue it lives in the `*struct-types*` registry that the AOT DEFSTRUCT path
deliberately does **not** populate (`(when *mvm-eval-runtime-p* …)`), so a
strict type check would break `:include` accessors in every AOT image —
including the ANSI gate.

**Documented residual:** a same-length sibling struct, or a long enough plain
vector, is still accepted where SBCL signals TYPE-ERROR.  Both are probed
(`acc.read.same-size-sibling`, `acc.read.plain-vector`) so a future fix gets
credit automatically.

`:read-only` is now enforced: writers for a read-only slot signal
PROGRAM-ERROR and store nothing.  Read-only-ness is inherited through
`:include` via a new compile-time `*DEFSTRUCT-EFF-RO*` table, kept parallel to
`*DEFSTRUCT-EFF-SLOTS*` rather than widening its `(name . default)` pairs,
which several sites destructure with plain CAR/CDR.

## 3. Cost — the check is NEGATIVE cost

The old body was `(aref obj IDX)`, i.e. the **public** AREF, which expands to a
cond testing `%MDA-P` (itself five type tests plus an OBJ-SUBTAG), `CONSP` and
`%PRIM-STRINGP` before reaching `%PRIM-AREF` — on every single slot access.
Having proven subtag #x32, the checked body can call `%PRIM-AREF` / `%PRIM-ASET`
directly and skip all of it.

Interleaved A/B (alternating binaries per rep so tenant load hits both equally),
best of 3, `bench2.sh`:

| benchmark | BASE | FIX | ratio |
|---|---|---|---|
| `b-null` — startup only, control | 2346 ms | 2318 ms | 0.988 |
| `b-ctrl` — same loop, trivial calls instead of accessors, control | 9563 ms | 9483 ms | 0.992 |
| `b-read` — 180 000 slot READS | 36903 ms | 32227 ms | **0.873** |
| `b-write` — 180 000 slot WRITES | 38746 ms | 32987 ms | **0.851** |
| `b-native` — compile 600 DEFUNs, exercises the compiler's OWN struct accessors as native code | 8610 ms | 8466 ms | 0.983 |

Both controls land inside 1 %, so the read/write deltas are real: **checked
accessors are 12.7 % faster to read and 14.9 % faster to write** than the
unchecked ones they replace, and the generic-CLI image is **411 KB smaller**.
The native-path proxy moves 1.7 % in the same direction.

So the design question the ticket posed — how much conformance to buy for how
much speed — did not arise: the cheap structural guard is strictly better than
what was there.  No safety mode, no shared checked reader, no per-call
structure-type dispatch.

## 4. The bug this fix tripped over

The first patched CLI made **every** runtime `(defstruct s a b)` die with a
TYPE-ERROR during expansion, while the host-compiled image sources were fine.
Root cause was not the type check: the writer template had been written as

```lisp
,@(or writer-body `((if (= (obj-subtag obj) #x32) … ,min-inst-len …)))
```

— a backquote nested **directly inside a comma** of the enclosing backquote.
SBCL compiles that correctly, so the image built clean, but when the MVM
compiler compiles `compiler.lisp` *into* the image it cannot compile the inner
commas, says so, and **substitutes NIL**:

```
;; WARN: cannot compile ,MIN-INST-LEN, using nil   (x2)
;; WARN: cannot compile ,(+ 2 I), using nil        (x2)
;; WARN: implicit global VAL / OBJ
```

Found by diffing the build's WARN histogram against the baseline build's — a
zero-slot defstruct worked and a one-slot one did not, which localised it to the
per-slot loop but not to which part.  The nested-backquote-inside-
`,@(mapcar (lambda …))` sites elsewhere in the file are safe because the inner
backquote sits in a LAMBDA body, a separate compilation scope.

**This silent degradation is a live hazard worth its own build check** — the
compiler already knows enough to print the warning; nothing fails the build on
it.

## 5. Probe rows

`probes/binding-differential.lisp` section A10 — 44 rows (plus a
`ds.slot.read-only.unchanged` row), both directions: valid access still
returns the right value, invalid access signals a catchable TYPE-ERROR.
Run with `./probes/run-binding-differential.sh <binary>`; the runner's grep
now admits the `acc.` prefix.

Disagreements with the SBCL oracle over those 44 rows:

```
baseline (modus-base) : 30 / 44
fixed    (modus-fix)  :  2 / 44     <- both the documented residual
                                       (acc.read.same-size-sibling,
                                        acc.read.plain-vector)
```

The 87 rows OUTSIDE the accessor section are **byte-identical** between the
two binaries, so nothing else moved.

## 6. ANSI gate

Gate binary built from this branch at `/home/claude/ws-acc/ansi-net`.
Baseline `/home/claude/ws-mkinst/ansi-net2`.  64 shards, `n5gate.sh`.

```
NOISE base rep1 : passed=17503  CHUNK-CRASH=0  FILE-WEDGE=30
NET accessors r1: passed=17504  CHUNK-CRASH=0  FILE-WEDGE=30
NOISE base rep2 : passed=17504  CHUNK-CRASH=0  FILE-WEDGE=30
NET accessors r2: passed=17504  CHUNK-CRASH=0  FILE-WEDGE=30
```

**Own noise floor, measured:** the baseline swept against ITSELF moves
17503 -> 17504, and per-file the only file that differs is `divide`, at exactly
ID **13445 — on the known-flaky list**.  The fix-vs-baseline difference on rep 1
is *the same single flaky ID*, and on the settled reps (base2 vs fix2) the
per-file diff is **0 files, net +0**.  Fix rep1 vs fix rep2 is also 0 files.
So the change is indistinguishable from run-to-run noise: no losses to recheck.

`structures-*` per file, every sweep, both binaries:

```
structures-01 = 12    structures-02 = 13    structures-03 = 36
structure-00  =  0    structures-04 =  0    print-structure = 0
```

exactly the reference counts.  CHUNK-CRASH stayed 0 and FILE-WEDGE 30
throughout.

## 7. Ladder

`/home/claude/lf/run-ql.sh`, run with the **baseline binary alongside as a
control** in the same window:

```
BASE (control) : libs=22 clean=15  probes ok=96 err=16 missing=0  FAILURES=16
FIX            : libs=22 clean=15  probes ok=96 err=16 missing=0  FAILURES=16
```

Identical, and identical per library (same 7 libraries carry the same errors).
Matches the documented baseline of FAILURES 16 / clean 15 of 22.

## 8. Still open

* Same-length sibling struct and long-enough plain vector are accepted where
  SBCL signals TYPE-ERROR.  Closing this needs per-instance type identity that
  survives crossing a compilation unit — either an identity-independent marker
  (the keyword intern table gives exactly that guarantee, per CLAUDE.md, but
  changing the marker touches the predicate, the printer, `%struct-instance-p`,
  cl-clos and eval2) or making the AOT DEFSTRUCT path populate `*struct-types*`
  so an ancestry check is available.  Neither is a small change; both are
  measurable via the two probe rows.
* `;; WARN: cannot compile ,X, using nil` silently produces wrong code and only
  warns.  A `build-checks.lisp` rule for it would have turned a multi-hour
  bisect into a failed build with the variable named.
