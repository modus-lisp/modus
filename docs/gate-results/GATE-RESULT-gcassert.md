# The "survives GC" assertions were vacuous — and one of them was hiding a live bug

Branch `jit-gcassert`, off `main` @ `6b7d20c`.  Unpushed.  **One file changed:
`tests/jit-diff.lisp`.**  No translator, collector, or runtime change; the image
is bit-for-bit `main`'s, so there is nothing for the ANSI gate to say.
`x64/bare/qemu/repl` = `269b461a764016eea6533c46798ad3e4`, confirmed after
deleting `/tmp/modus-x64.bin` first.

## The defect in the test

Every "survives GC" probe in `tests/jit-diff.lisp` forced collection with

```lisp
(eval '(dotimes (i 4000) (make-list 40)))
```

— roughly **2.5 MB** of garbage against a collection threshold at the **939 MB**
heap midpoint (`*linux-x64-r14-offset* #x38000000`, `build-generic-cli.lisp`).
Reading the collector's own counter afterwards gives `gc_count = 0`.  **Not one
collection ever ran.**  Every GC assertion in the file was asserting that a
function still worked after doing nothing.

This matters beyond the test: #222 was merged citing "surviving a forced GC" as
part of its evidence, and the const-pool restriction it introduced rests on the
premise that a baked const address goes stale across a collection.  Neither the
evidence nor the premise had ever been exercised.

`jd-force-gc` now allocates until the counter actually advances and returns how
many collections it saw; `jd-gc-fired` **asserts** that number is non-zero, so a
future heap-size change cannot silently turn these back into no-ops.

Two measurement traps worth recording:

* `gc_count` is a **raw** word at BSS `0x10000060` bumped by `inc qword [abs]`,
  so a `mem-ref :u64` read reinterprets it as a *tagged value* — a count of 1
  reads back as a CONS and printing it aborts the load.  `mem-ref :u32` returns
  its low half properly tagged.  This produced one confidently wrong reading
  before it was tracked down.
* `MODUS_GC_R14=262144`, the documented small-heap knob, **cannot** substitute
  here: at that size the JIT falls back for essentially everything
  (`native%≈0` under heap pressure), so the code path under test stops running
  entirely.  These probes collect at the **shipping** heap settings instead.

## Answer to the question that prompted this: #222 is fine

With collections that actually fire, **every #222 claim holds**:

```
gc-defun-before  31      gc-defun-seam   31      gc-defun-direct 31
gc-defun-id      T       d222-gc-after   31      d222-gc-after2  31
d222-gc-id       T       d222-gc-tag     3       d222-gc-const   "v=7"
```

A runtime `DEFUN` installed as native code survives a real collection by value,
by identity, by tag, through the seam, and by direct `funcall` of the installed
object.  `JD222-FAIL=0`.  **The merge is retroactively validated** — the
mechanism was right even though the test proving it was not.

## But a real GC exposes a live bug on `main`: runtime macros die

```
JD-GCSTALE gc-macro-expand  got=(#<?> 2 3)  correct-would-be=(+ 2 3)
```

A runtime `DEFMACRO` does **not** survive a collection.  `macro-function` still
returns non-NIL, but the expander's own **quoted-symbol constants have gone
stale**: `(macroexpand-1 '(jd-gcmac 2 3))` comes back as `(#<?> 2 3)` — the head
symbol `+` is now a dangling from-space pointer — and compiling that expansion
signals `UNDEFINED-FUNCTION`.

Characterised in isolation (`defmacro mac-add (a b) → (list '+ a b)`):

| | before GC | after 1 real GC |
|---|---|---|
| `macro-function` non-NIL | 1 | 1 |
| `(funcall (macro-function 'mac-add) '(mac-add 2 3) nil)` | `(+ 2 3)` | **`(#<?> 2 3)`** |
| `(macroexpand-1 '(mac-add 2 3))` | `(+ 2 3)` | **`(#<?> 2 3)`** |
| using the macro | `5` | **`UNDEFINED-FUNCTION`** |
| a plain runtime `DEFUN` | `5` | `5` (fine) |
| a macro defined *after* that GC | — | works, then breaks at the *next* GC |

That last row is the signature: it is not "macros are broken", it is **"a macro's
baked const address is valid exactly until the next collection"** — precisely
the hazard #222's const-pool restriction describes.  The restriction covers
installed *functions*.  Nothing has ever covered macro expanders, and an
interpreter trampoline is safe only because `mvm-interpret`'s `op-li-const`
reads `*e2-const-pool*` live rather than through a baked address.

**Severity.**  This breaks every runtime-defined macro in any session that
collects — which is every long-running session, every `--load` of a real
library, and the whole Quicklisp-on-Modus ladder, since libraries are mostly
macros.  It has been invisible only because nothing measured it.

### It is exactly what the constant vector fixes

Running **this same test file** against the `jit-constvec` image (`e932e3e`,
the other branch) instead of `main`'s:

| | `main` @ 6b7d20c | `jit-constvec` @ e932e3e |
|---|---|---|
| `JD-GCSTALE-PROBES` | 2 | 2 |
| **`JD-GCSTALE-HITS`** | **1** | **0** |
| `macroexpand-1` after GC | `(#<?> 2 3)` | `(+ 2 3)` |
| macro use after 2nd GC | `UNDEFINED-FUNCTION` | `42` |

(The `JD222-FAIL d222-tag-const-tramp` that appears on the `jit-constvec` run is
this branch's copy of `main`'s tag-**9** assertion; on that branch const-bearing
functions install natively and read tag **3**.  Expected, explained, and not a
defect.)

So the constant-vector work is not merely a `native%` optimisation — it removes
a class of live heap corruption that today silently destroys every runtime
macro.  That is worth weighing when deciding what to do with the still-unclean
JIT-on gate on that branch.

## Reporting style

`gc-macro-expand` / `gc-macro-use` are counted through `jd-note-gcstale`,
mirroring the file's existing `jd-note-earlybind` pattern: a **known divergence,
reported loudly but not failed**, so `JD-OK` stays a usable gate while the bug
stays visible.  `JD-GCSTALE-HITS` is the headline number — **0 means the const
pool survives collection**.

One harness accommodation, documented rather than hidden: `jd-gc-fired`
re-establishes this file's own `jd-tag-into` macro after each forced collection.
Without it the first `jd-tag-into` after a real GC compiles as a call to an
undefined function and **aborts the entire load** — which is exactly what
happens on `main`, and which would leave the rest of the file unmeasured.  The
bug itself is asserted by the `gc-macro-*` probes; the re-definition only keeps
the remaining ~250 assertions reachable.

## Summary numbers

```
                        main @ 6b7d20c    (before this branch)
JD-TOTAL                332               332
JD-DIVERGE              0                 0
JD-NATIVE               297               297
JD222-TOTAL             55                46
JD222-FAIL              0                 0
JD222-EARLYBIND         1                 1
JD222-INSTALLED         26                23
JD-GCSTALE-PROBES       2                 —
JD-GCSTALE-HITS         1                 —   <- the finding
                        JD-OK             JD-OK
```

## Reproduce

```bash
sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp   # ~1m50s
./modus --load tests/jit-diff.lisp --quit    # JD-GCSTALE-HITS=1 on main

# minimal, no harness:
cat > /tmp/gcmac.lisp <<'EOF'
(defun gcn () (mem-ref #x10000060 :u32))
(defun force-gc () (let ((b (gcn)) (k 0))
  (loop (when (or (> (gcn) b) (>= k 60)) (return (- (gcn) b)))
        (dotimes (j 100000) (make-list 40)) (setq k (+ k 1)))))
(defmacro mac-add (a b) (list '+ a b))
(format t "before=~S~%" (macroexpand-1 '(mac-add 2 3)))
(format t "collections=~S~%" (force-gc))
(format t "after=~S~%"  (macroexpand-1 '(mac-add 2 3)))
EOF
./modus --load /tmp/gcmac.lisp --quit        # after=(#<?> 2 3)
```
