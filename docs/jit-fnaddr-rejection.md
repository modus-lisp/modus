# Reproducing the aarch64 JIT `#'NAME` rejection

How the aarch64 JIT silently demoted whole functions to the interpreter
whenever they held a `#'NAME` value, how to see it happening, and how to
measure it without fooling yourself.  Found chasing reel's VP8 decoder
(2026-09-09); fixed by the nargs-generic late-binding thunk in commit
`f0047a8` (branch `fnaddr-thunk-aa64`).

## The symptom

A library loaded with every `defun` installed native (`*jit-hot-only*` NIL)
still ran at interpreter speed.  reel's `vp8-idct` — a pure fixnum 4×4
inverse DCT — took **~70 ms per call** under QEMU-TCG (≈7 ms on silicon),
where native code takes microseconds.  A 320×180 frame has ~1,080 such
blocks, so IDCT alone cost seconds per frame.  Yet the load reported
`native=166 fallback=45` and 505 defuns "installed native": the *hot*
functions were among the 45.

## The mechanism

The aarch64 page builder, `%jit-translate-page-1-aarch64` (mvm/mvm-eval.lisp),
relocates three kinds of reference after translating a module:

| pass   | reference                                   | on a non-native target        |
|--------|---------------------------------------------|-------------------------------|
| `crel` | out-of-module **call** `(NAME …)`           | bridge thunk (#306) — page OK |
| `lrel` | in-module `#'LOCAL` / closure fn-addr       | patched — page OK             |
| `frel` | out-of-module **`#'NAME` as a value**       | **page REJECTED, unconditionally** |

The `frel` pass did not even check whether NAME was native.  It refused every
`#'NAME` value on purpose: the older code *baked* the address resolved at
page-build time into the MOVZ quad, which froze the pre-CLOS stub for any
name CLOS later (re)defined — the `ql:quickload` failure that got the board
shipped with `MODUS_RPI_JIT=0`.  Refusing was correct (a stale native jump is
worse than a slow interpret) but it meant **any form containing a `#'`
value interpreted in its entirety**.

Two things produce such a value in ordinary code:

- explicit `#'` — `(every #'zerop blk)`, `(apply #'format …)`, `(sort … #'<)`
- the **keyword `make-array` expansion** — `(make-array n :element-type …)`
  references `#'MAKE-ARRAY` as a value.  This is the one that bites: it is in
  `vp8-idct`, `vp8-iwht`, `decode-key-frame`, `decode-macroblocks` …

A rejected page bumps `*jit-fallback-count*` and `*jit-r-page-nil*`, attributes
itself to `*jit-r-aa-frel-pages*`, and bumps `*jit-r-reloc-fnaddr-fail*` once
per reference.  x64 is different: its `%jit-reloc-fn-addrs` bakes a *native*
`#'NAME` (only a heap-closure target fails), so x64 ran reel natively — while
carrying the CLOS-staleness hazard the aarch64 refusal was avoiding.

## Reproduce it (pre-fix binary)

Any aarch64 CLI built before `f0047a8` — e.g. `/home/claude/cabfs/modus-aa64-diag`
under `qemu-aarch64-static`, or the board image.  Turn hot-only off so each
`defun` is translated at definition, then define a form holding a `#'` value
and read the reject counters:

```lisp
(setq *jit-hot-only* nil)
(defun ff () (if (boundp '*jit-r-reloc-fnaddr-fail*) (or *jit-r-reloc-fnaddr-fail* 0) 0))
(defvar *f0* (ff))
(defun f-a (v) (every #'zerop v))                        ; explicit #'
(format t "explicit #': fnaddr-fail +~a~%" (- (ff) *f0*))   ; => +1
(setq *f0* (ff))
(defun f-b () (make-array 4 :element-type '(signed-byte 32)))  ; keyword make-array
(format t "keyword make-array: fnaddr-fail +~a~%" (- (ff) *f0*)) ; => +1
```

Measured on the pre-fix binary: **+1 for each**, in the default package and
in a fresh runtime-born package alike.  (`/home/claude/cabfs/hdmi/min-repro.lisp`.)

### Measure with the right counter

Do **not** judge a single defun by the `*jit-native-count*` delta read from a
following top-level form.  With hot-only off, that probing form is itself
JIT'd and bumps `native-count` before it reads it, so *every* variant reports
"native" — an earlier bisect concluded the transforms translated fine
standalone for exactly this reason, and it was wrong.  Use the counters that
only the reject path increments:

- `*jit-r-reloc-fnaddr-fail*` — +1 per refused `#'NAME` reference
- `*jit-fallback-count*` — +1 per page that failed and interpreted

## See it at library scale (reel)

```lisp
(setq *jit-hot-only* nil)
(dolist (f '("src/packages" "src/decode/tables" "src/decode/bool" "src/decode/transform"
             "src/decode/intra" "src/decode/loopfilter" "src/decode/inter-tables" "src/decode/inter"))
  (load (format nil "/home/claude/reel/~a.lisp" f)))
(list *jit-native-count* *jit-fallback-count* *jit-r-reloc-fnaddr-fail*)
;; pre-fix: (166 45 121)
```

Attribution by reason (all pre-fix):

| counter                          | value | meaning                                  |
|----------------------------------|------:|------------------------------------------|
| `*jit-r-page-nil*`               |    45 | every fallback is a failed page          |
| `*jit-r-aa-frel-pages*`          |    42 | 42 of them: a refused `#'NAME` value     |
| `*jit-r-reloc-fnaddr-fail*`      |   121 | references refused                       |
| `*jit-r-reloc-call-nonnative*`   |     2 | (crel residue — the bridge already ran)  |
| `*jit-r-reloc-call-unresolved*`  |     1 |                                          |
| `*jit-r-const-baked*`, mmap …    |   nil | not involved                             |

### Name the referents (census)

Set `*jit-census-on*` **before** loading; the `frel` reject path then records
`(name . count)` into `*jit-blocked-fnaddrs*` (a bounded 96-entry alist —
plain `write` chokes on it mid-form; print it with `dolist`):

```lisp
(setq *jit-census-on* t)
;; … load reel as above …
(dolist (e *jit-blocked-fnaddrs*) (format t "~a x~a~%" (car e) (cdr e)))
;; MAKE-ARRAY x118      <- keyword make-array expansion
;; ZEROP      x2        <- (every #'zerop blk) in inter.lisp
;; FORMAT     x1        <- (apply #'format …) in %err
```

`*jit-blocked-callees*` is the sibling list for the `crel` pass.

### What retry does not fix

`*jit-retry-on*` re-translates queued failed modules once more callees are
native.  It cannot help here — the rejection is unconditional, so a retry
fails identically: measured `queued=6 succeeded=NIL exhausted=7`, fallbacks
unchanged.

## The fix, and how to confirm it

`f0047a8` adds `%jit-make-fnaddr-thunk-aarch64` and switches the `frel` loop to
thunk-first.  The `#'NAME` value becomes a stable tag-3 word in the exec thunk
page.  Because the arity of a *value* is unknown at build time, the thunk
cannot use the existing nargs-specific bridge (which shifts `x0..x3` to pass
the name index in `x0`); instead it passes the index out-of-band —
`movz/movk x17 ← idx; str w17, [#x10000178]; br %jit-bridge-any` — leaving
every argument register, the caller's stack pushes and the nargs slot
untouched.  `%jit-bridge-any (&rest args)` reads the slot and `apply`s the name
resolved **at call time**: late-bound on every call, so a later `defun` /
CLOS redefinition is followed — the property the old refusal existed to
protect, now satisfied *and* native.

On the fixed binary the same probes read:

```
13-case #' regression: every/mapcar/reduce/funcall/apply/sort, a HOF taking a
user #'fn, and a saved #'f seeing a later (defun f …) => all ok,
native=21 fallback=0 fnaddr-fail=NIL
```

and the reel-scale load (same QEMU-TCG host, so compare ratios, not absolutes):

| measurement                | pre-fix     | with the thunk | ratio  |
|----------------------------|------------:|---------------:|-------:|
| pages fallen back          |          45 |          **1** |        |
| `#'NAME` references refused|         121 |          **0** | 3 thunks built, one per name |
| `vp8-idct` × 2000 calls    | 139,600 ms  |     **453 ms** |  ~308× |
| `vp8-iwht` × 2000 calls    | 114,842 ms  |     **450 ms** |  ~255× |
| keyframe (frame 0) decode  | 397,278 ms  |   **2,270 ms** |  ~175× |
| frame-0 Y-plane checksum   |   7133244   |    **7133244** | bit-exact vs the interpreter |

The one remaining fallback is a non-hot module; the transforms and both
macroblock loops are native.  `/home/claude/cabfs/hdmi/thunk-regress.lisp`,
`fnaddr-validate.lisp`.

On real silicon (a Raspberry Pi 5, Cortex-A76, the linux-aarch64 CLI run
natively — no emulation): `515 defuns native, 1 fallback`; the 320×180 clip
decodes **30 frames in 8.85 s = 3 fps** (keyframe 341 ms, inter frames
≈271 ms).  Before this fix the same run had not finished one keyframe after
several minutes.

A follow-on (`ac59dd4`, branch `mka-inline`) removed the other cost the
census exposed: the keyword `make-array` expansion.  It now compiles to a
direct call, and for element types the runtime stores as a plain word array
anyway (`(signed-byte 32)` etc. — it returns a bare simple-vector with
element-type `T`) to the 0.05 µs inline allocation instead of a 14.4 µs
runtime call.  `vp8-idct` dropped a further 11× (it allocated two such arrays
per block); on the Pi 5 the clip now decodes **30 frames in 6.9 s = 4 fps**,
keyframe **139 ms**, inter frames 213 ms.

What remains between 4 and 60 fps is *uniform per-operation* native code
cost, not any one function: on this build an `aref` costs ~1.8 empty calls
(a runtime wrapper check plus a u8-vs-word subtag dispatch on every access,
even where the source declares `(simple-array (signed-byte 32))`), arithmetic
is tag-checked with overflow promotion, every local is spilled to the frame,
and `bool-bit` — a full call per decoded bit, ~100k per keyframe — costs ~9
empty calls.  A frame is ~1–2M such operations.  Honoring declared array and
fixnum types in the emitters is the first, most contained step; the profile
and plan are in the session memory (`reference_reel_perf_profile`).

## Related

- `docs/calling-convention-design.md` — the tagged-word / tag-3 native function discipline this relies on.
- mvm/mvm-eval.lisp: `%jit-translate-page-1-aarch64` (the three reloc passes), `%jit-reloc-fn-addrs` (x64 counterpart), `%jit-make-bridge-thunk-aarch64` (the nargs-specific call bridge), `%jit-bridge-resolve`.
- The 32-argument cap fixed alongside this (`cbe4e92`) is a separate blocker on the same decoder: see the commit message.
