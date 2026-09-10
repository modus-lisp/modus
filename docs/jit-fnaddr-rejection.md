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

What remained was *uniform per-operation* native code cost, not any one
function.  The first tier of that is now done (`0144cb3`, `cfeed1b`,
`2dd2ff4`, branch `decl-fastpath`): the compiler had no type-declaration
tracking at all, so every `aref` ran a four-way wrapper/string/mda dispatch
even where the source declared `(simple-array (signed-byte 32))`.  Bindings
now carry the declared type (deftype names like reel's `u8vec`/`fxvec`
resolved through the runtime deftype table), and a declared generic array
takes the raw word-slot access while a declared `(unsigned-byte 8)` array
takes the packed-byte primitives directly.  That exposed a runtime bug of
its own — `make-array` wrapped every typed generic array in an MDA header
(`type-of` hides it; test with `%mda-p`), which put all of reel's tables on
the slow branch — fixed alongside.  Declared array access is 2.5–7× faster;
on the Pi 5 the clip now decodes **30 frames in 6.3 s = 5 fps**, keyframe
**114 ms** (from 341 before any of this), inter frames 196 ms.

What remains between 5 and 60 fps: `bool-bit` is a full call per decoded
bit (~9 empty calls, ~100k per keyframe) — inline it; arithmetic on declared
fixnums is still tag-checked with overflow promotion — the binding type slot
now makes a raw-op path a contained change; and every local is spilled to
the frame.  Profile and plan: session memory `reference_reel_perf_profile`.

## The thunk's own cost, and the cache that removed it (2026-09-09, later)

With the decoder native and the per-op tiers in (typed arithmetic, leaf
operands, LET width inference — commit `2d4d824`), a direct per-phase timing
of an inter frame on the Pi 5 (`/home/claude/cabfs/hdmi/pi-phase.lisp`,
`pi-mb.lisp` — copies of `decode-frame` / `decode-macroblocks` with timers,
because a `(setf (symbol-function …))` wrapper is *not seen* by native
callers) attributed **92 % of the frame to the residual-add phase**: 150–267
ms of a 181 ms frame in `decode-macroblocks`, of which `add-inter-residual`
was 1249 of 1364 ms over seven frames.  Motion compensation measured
≤ 20 ms, the loop filter 15 ms, the bordered-plane copy 16 ms, the bool
decoder ~4 ms (0.215 µs/bit).

The phase is `(unless (every #'zerop blk) …)` on every 4×4 block: 24 blocks
per macroblock, 16 element calls each — ~92,000 calls per frame through the
`#'ZEROP` thunk, and each of those took the thunk's slow path: store the
name index, branch to `%jit-bridge-any (&rest args)`, cons the argument list,
`apply`.  The thunk that made the function native had made every element
call cost a few microseconds.

The fix keeps the late binding and adds a cache.  The thunk now begins

```
 0: ldr x16, [pc, #72]     ; cached native target (raw code address)
 4: cbz x16, +8            ; 0 = not cached: slow path
 8: br  x16                ; direct: arguments, nargs slot and LR untouched
12: (the previous sequence: idx → slot, br %jit-bridge-any)
72: cache word
```

`%jit-bridge-any` fills the word when the resolved target is a tag-3 native
function (a heap closure keeps taking the slow path), and every writer of
`*symbol-function-table*` (`defun`, `fmakunbound`, the native installer, the
trampoline path, the CLOS accessor alias) calls
`%jit-fnaddr-thunk-invalidate`, which zeroes the word, so a later
redefinition is followed on the next call.  `thunk-cache.lisp` checks both
properties: 5,760 blocks of `every #'zerop` take 35 ms under QEMU-TCG, and a
saved `#'tf` sees a later `(defun tf …)` and an `fmakunbound` + `defun`.

One trap, worth recording because it surfaces as a bare `#(SIMPLE-ERROR
NIL)` (a recovered fault carries no condition): a Lisp
`(setf (mem-ref slot :u64) v)` stores `v`'s **tagged** form (a fixnum is
`n<<1`), while the thunk's `ldr` reads the raw bits, so storing the address
branched to twice the address.  The cache stores `(ash raw -1)`; the file's
`RAW-ADDR-AUDIT` note has the convention.

Pi 5, same 320×180 clip: **30 frames in 1.63 s = 18 fps**; inter frames
181 → **52 ms**; keyframe 89 ms (it has no zero-block test in its path and is
now the outlier).  x64 is unchanged — it bakes `#'NAME` addresses and never
had the slow path (nor the late binding).

## After the thunk: three more tiers to 30 fps (2026-09-09, late)

With the residual-add phase gone, direct per-phase timing of an inter frame
read: macroblock loop 23 ms, loop filter 15 ms, bordered plane copy 16 ms
(of 52).  Each became its own fix on the Modus side:

- **u8 block paths for `replace` / `fill`** (`dd8e138`).  `%bulk-copy`
  deliberately excludes byte-packed vectors (its word copier would read eight
  elements per slot), so reel's row-by-row plane copy fell to the generic
  element loop.  A byte-wise sibling (`%bulk-copy-u8`, memmove semantics) and
  a byte `fill` took the copy 16 → 5 ms.  (Aside: a top-level probe *loop*
  is interpreted unless the probe sets `*jit-hot-only*` to NIL — every
  iteration then costs ~5 µs, which made `length` and `replace` look 40 µs a
  call.  They are ~0.  Set it in timing probes.)
- **`(declaim (inline f))` honoured at runtime** (`bb1f957`).  The runtime
  `declaim` macro expanded to NIL and a runtime `proclaim` *macro* shadowed
  the real function, so no declaration reached the compiler.  reel declaims
  exactly its hot leaves inline — `bool-bit`, `treed-read`, `clamp255`,
  `c8`, `%adjust`, `%edge-ok`, `%hev` … — and the loop filter called six of
  them per filtered line.  Calls to a declaimed-inline DEFUN with only
  required parameters now expand in place as a LET with the parameters
  renamed to fresh symbols (the body's own declarations land on the LET,
  where the typed fast paths read them).  With it, `abs` on a typed operand
  no longer costs two runtime calls (`%complex-p` of a known-width integer
  is NIL; negating a ≤ 61-bit value is a plain subtract).  Loop filter
  15 → 4 ms.
- **An x64-only fault the inliner exposed.**  Once `bool-bit`'s body sat
  inside an `aref` index, its `loop while … do` — which `expand-cl-loop`
  turns into a `(setq #:nat%N t)` termination flag, i.e. a *global* store —
  reached `%compile-setq-global`, whose `%GV-SET` call saved none of the live
  expression temps.  x64's V5–V8 are caller-saved (aarch64's are x19–x23,
  callee-saved), so the held array register came back as garbage and the
  keyframe decode died in the error signaller.  Bisected with two new
  compiler knobs (`*inline-never*`, `*inline-only-in*`) and a probe-local
  copy of one reel file down to a six-line shape; the call now saves its
  temps exactly as `compile-call` does.

Pi 5, same clip: **30 frames in 0.99 s = 30 fps**; inter frames 31 ms
(macroblock loop 24, loop filter 4, copy 6), keyframe 68 ms.  Frame-0
checksum 7133244 bit-exact on both arches throughout.

## Type promises on globals, inline EVERY/SOME, word copies: 33 fps

The next attribution read: macroblock loop 22 ms (token parsing 7.4, motion
compensation 4.9, residual add 5, mode parsing 3.7), loop filter 4, copy 4 —
no single item left, so three small general fixes:

- `(declaim (type … *global*))` is now honoured for **globals** (reel
  declaims its coefficient tables — `+coeff-bands+`, `+coeff-scan+`, the
  quantiser lookups, the trees and probabilities — but references them as
  globals, so every access in the token loop was still the four-way
  dispatch).  The compiled `(proclaim …)` form is swallowed by
  `compile-form`'s DECLAIM/PROCLAIM no-op clause, so the recorder hangs off
  that clause, not the `proclaim` function; a lexical of the same name is
  never typed by the global's promise.
- `(every #'p v)` / `(some #'p v)` over a declared simple array compile to
  an index loop, with `zerop`/`plusp`/`minusp` as inline compares — the
  per-block test now costs no calls at all.
- `%bulk-copy-u8` moves eight bytes at a time through raw word loads on the
  packed data (byte K of an object with word W lives at W+7+K; nothing in
  the loop allocates), including disjoint ranges of the same object.

Pi 5: **30 frames in 0.90 s = 33 fps**, inter 28 ms, keyframe 65 ms.

And the ANSI gate caught a real bug of the arithmetic tier: `minus.8`
(13982) faulted because `%expr-width` reported `(ash 1 1000)` as 1002 bits
and the trust logic read *any known width* as "this operand is a fixnum" —
`(- (ash 1 1000))` became a tag-less subtract on a bignum pointer.  A
computed width beyond 63 bits now comes back as NIL, which every consumer
already treats as "not provably a fixnum".

One more promise-driven tier followed (`85cd19b`): **typed struct slot
access**.  `defstruct` now records every accessor with its struct, slot
index and `:type`; a call `(acc x)` or `(setf (acc x) v)` whose argument is
*declared* that struct compiles to the slot read/write in place — no call,
no subtag/length check — and the slot's type feeds the array and width
paths.  reel's `pget` is `(aref (pl-data pl) …)` on a declared `plane`, so
every intra-predicted pixel had cost an accessor call plus a generic `aref`;
`bool-bit` carried six `bd-*` calls per decoded bit.  Pi 5: **30 frames in
0.80 s = 37 fps**, inter 25 ms, keyframe 55 ms.

A last instruction-count slice (`2f2411e` — pure typed right operands skip
the push/pop guard, and a guard for an x64 hazard in the earlier leaf
shortcut: a spilled temp or an `aref` is written through rax, which is also
VR) changed nothing on the Pi.  That is the useful negative result: on an
out-of-order core the remaining cost is not the instruction count inside
expression chains.

That conclusion was wrong, and a sampling profile said so.  `perf` on the
Pi 5 (with a symbol map dumped from `*symbol-function-table*` and samples
filtered to those under `DECODE-FRAME`) put only ~45 % of the decode in
reel's kernels.  The rest was runtime library work the phase timers and
op histograms could not see: every global-variable reference probing the
globals table by hash (`%GV-CELL`/`GETHASH`), `expand-cl-loop`'s unbound
termination-flag gensym turning every `loop while` exit into a *global*
write, `FILL`'s generic element loop plus ~200 instructions of keyword
parsing on each 16-element coefficient block, keyword literals re-interned
(a `GETHASH`) on every evaluation, and rank-2 declared `aref`s through
`APPLY`.  Each became a general fix (`8bd9962` and after): a runtime-
compiled global read/write bakes the `(key . value)` pair as a constant
and does `cdr`/`setcdr` in place; the loop flag is LET-bound; `fill` on a
declared array is an inline typed loop and `replace`/`fill` with
`:start`/`:end` keywords on declared arrays compile to bounded loops with
no keyword parsing; keyword literals compile to constants; rank-2 declared
arrays read their data vector row-major.  Pi 5: **30 frames in 0.63 s =
48 fps**, inter 19 ms, keyframe 47 ms.  The measurement recipe lives in
the memory note `reference_pi_perf_profiling_recipe`.

What remains between 48 and 60 fps is, this time measured, mostly reel's
kernels themselves (`add-residual`, the loop-filter edges, `mc-filter`,
`decode-residue`/`get-coeffs`, ~65 %) at ~12 instructions per binary op
with every local in a frame slot — register allocation across a basic
block — plus `decode-residue`'s untyped inner `(aref (aref yc b) i)`.

## Related

- `docs/calling-convention-design.md` — the tagged-word / tag-3 native function discipline this relies on.
- mvm/mvm-eval.lisp: `%jit-translate-page-1-aarch64` (the three reloc passes), `%jit-reloc-fn-addrs` (x64 counterpart), `%jit-make-bridge-thunk-aarch64` (the nargs-specific call bridge), `%jit-bridge-resolve`.
- The 32-argument cap fixed alongside this (`cbe4e92`) is a separate blocker on the same decoder: see the commit message.
