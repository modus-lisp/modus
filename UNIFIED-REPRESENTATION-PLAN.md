# Plan: One Value Representation Everywhere (and it is Common Lisp)

Status: IN PROGRESS (2026-06-20). WS0 decided ((A) raw words). **WS1 essentially
complete**: val↔word boundary (committed), runtime-call bridge, cons/list alignment,
the **GC-safe register file** (validated by a list-build-under-early-GC stress test),
and strings/vectors opcodes — all done. eval2 runs a real CL subset (arithmetic,
if/let/progn, recursion, cons/list structure, strings/vectors, native runtime calls
with fixnum+list args) **GC-safely**. Both boundary follow-ups worked through:
(a) the variable-index bug was an *interpreter* gap (frame-alloc/free treated as
frame-enter, wiping the frame) — fixed, and it unlocks any local-spilling
function; (b) cross-bridge **strings** now work (real make-string objects; native
length/char/string-upcase on eval2-built strings; conformant aref). Remaining:
native-layout VECTORS + the numeric tower (bignum/float/ratio) — needs
make-mvm-object as a distinguishable type and ripples into the numeric opcodes.
Then WS2/WS3/WS4.

## Thesis

There must be **one** data representation shared by the compiler, the MVM
interpreter, an eventual JIT, and native code — and that representation is the
real native **Common Lisp** one (tagged words on the shared heap). There must be
**no second, Modus-specific value format** to translate to/from.

This is the conformance-first principle (`PLAN.md`: "actually IS a Common Lisp",
and `DIVERGENCE.md`: drive every deviation to zero) applied to *data
representation*, not just to functions. Modus should *be* Common Lisp end to
end, including how a value is laid out in memory — fix the wart, don't work
around it.

## Why (the wall this removes)

Driving runtime `eval` through `compile-form → MVM bytecode → mvm-interpret`
(the plan to retire the divergent tree-walker `mvm/cl-eval.lisp`) hit a wall:
the in-image meta-interpreter uses its **own** value format, divergent from
native, so it cannot call native runtime functions without lossy
**marshalling** (copying), which breaks mutation/identity for a large class of
ANSI tests.

The wall is self-inflicted: it exists *only* because the interpreter invented a
second representation. Align the representation and the wall disappears —
interpreter ⇄ JIT ⇄ native interoperate freely, GC "just works" (registers are
real roots holding real pointers), and a whole bug class (double-tagging, the
GC `regs`-reload hack) evaporates.

We are **not** removing native codegen (eval's endgame is a JIT — see Philosophy)
and **not** necessarily removing the interpreter (keep it as an execution
option / bootstrap rung). We are removing the *divergent format*.

## Mission (the root why)

Modus is a **legible** operating system that can be **effectively defended in an
environment where ASI makes opaque systems legible** — a *minimum viable
sovereign computing environment* for people or AI agents. When superintelligence
dissolves the obscurity that today "protects" complex opaque stacks (a system its
owner cannot fully read is a system someone else can read better), the only
defensible system is one that is *already* legible end to end: small enough to
audit in full, with no opaque corner where a vulnerability can hide. Legibility
is therefore a **security property**, and minimalism and sovereignty (no external
toolchain you cannot account for) are how you keep it.

There is no "us and them": defender and adversary are symmetric peers (we will
all be ASI; we will all have adversarial other ASI). That symmetry is *why*
legibility is the defense — obscurity buys nothing against a peer who reads
through it, complexity only favors the attacker, and there is no overseer to
outsource defense to. The one asymmetry you can manufacture in your own favor is
total comprehension and ownership of a minimal substrate: you understand your own
system best because it is small, legible, and yours.

This is why the representation work below is load-bearing, not cleanup: every
non-CL "Modus format" is an un-auditable corner and added attack surface;
collapsing to one readable Common Lisp representation *is* defense. Self-hosting
(no external compiler ⇒ JIT) *is* sovereignty. Minimalism (delete surface — the
tree-walker, divergent formats, eventually the native-codegen duplication) *is*
the security model, not a stylistic preference.

## Philosophy (why this is load-bearing, not cleanup)

Modus is meant to be a **legible** operating system whose running **state can
transfer between architectures** — migrate a live system from x64 to aarch64 and
keep going. The portable, legible form of that state *is* the MVM bytecode plus
the Common Lisp value heap. Native machine code is neither portable nor legible;
a CL heap + bytecode is both. That is the whole reason the representation must be
**one** thing and must be **Common Lisp**: it is the substance that transfers and
that a human (or the system itself) can read.

**JIT is therefore mandatory, not optional.** There is *no external compiler* —
Modus compiles itself — so the only way to get acceptable performance from the
portable state on a given architecture is to JIT it to native at runtime. It is
technically possible to run interpret-only, but performance makes JIT a
requirement of the design, not a someday-nice-to-have. The aligned interpreter
(WS1) is the bootstrap/interim execution path and a permanent fallback; the JIT
(WS3) is the destination.

## Current representations (grounded)

### Native / canonical (the target — keep)
Tagged 64-bit words, fixnum-shift = 1 (see `CLAUDE.md` "Tagged Value System"):
- Fixnum `xxx0`: value << 1. Cons `0001`. Object `1001`. Immediate `0101`
  (chars, nil, t). Forward `1111`.
- Object header `[subtag:8][unused:7][element-count:49]`; data at `raw+16`;
  `OBJ-REF`/`OBJ-SET` offset `idx*8 + 7`.
- NIL = `#xDEAD0001`, T immediate = `#xDEAD1009`.

### MVM interpreter (the divergent format — fix)
`mvm/interp.lisp`:
- `tag-fixnum (n) = (ash n 1)` / `untag-fixnum = (ash n -1)` — stores MVM-fixnum
  `n` as the **Lisp integer 2n** (a *second* shift on top of the native fixnum
  tag → "double tagged"). In-image this is why a returned value needs `(ash r -1)`.
- `make-mvm-object (size subtag)` = a **Modus simple-vector** `[tagword, data…]`
  simulating an MVM heap object — *not* the native object layout above.
- `regs` = a Modus simple-vector of these simulated values.
- `+mvm-nil+`/`+mvm-t+` were `0`/`2` (stale); already realigned this session to
  `#xDEAD0001`/`#xDEAD1009`. That realignment is the *kind* of change this plan
  generalizes to the whole value domain.

### Other non-CL "Modus formats" (candidates to remove — CONFIRM scope)
- CL-symbol wrapper: `(cons *sym-tag* #<array [hash, package, name]>)` with a
  dual system `%cl-sym-p` vs `%native-mvm-sym-p` (`mvm/cl-packages.lisp`). Two
  symbol representations is itself a divergence.
- Strings store char **codes**; `%prim-aref` returns the raw code while public
  `aref` wraps in `code-char` (`CLAUDE.md` "String element access"). The public
  surface is conformant, but the dual prim/public split is a Modus-ism worth
  auditing.
- Any other Modus-only in-memory or on-disk format (bytecode container, fn
  table, metadata blobs) — audit, but these are *compilation artifacts*, not
  *value* formats, so likely fine to keep.

## Target

Every execution mechanism operates on the **native** value layout:
- Interpreter registers hold real CL values / real tagged words / real heap
  pointers — identical to what compiled native code holds.
- A value produced by interpreted code is directly usable by native code and
  vice versa (no marshalling, no copy, identity & mutation preserved).
- One symbol representation, one string story, one of everything.

## Key design decision (resolve before WS1)

**How does the aligned interpreter hold values — "real CL values" or "raw native
words"?** The bytecode's fast-paths examine *tag bits* (e.g. fixnum check =
`OR a b; AND 1; TEST`), so the interpreter must either:

- **(A) Raw native words.** Each register holds the literal tagged word (bits).
  Every opcode works literally as specified (bit-level add, tag examination).
  A raw word *is* the native value's bit pattern, but calling a native function
  needs a **bitcast** primitive ("treat these raw bits as a tagged value") at
  the CALL boundary — a no-op reinterpret, presumably already available to the
  GC / funcall-dispatch machinery. Most faithful to the ISA.
- **(B) Real CL values.** Registers hold real values; tag-check opcodes are
  reinterpreted as *type predicates* (`fixnump`, `consp`, …) instead of bit
  tests. No bitcast at CALL, but every bit-examining opcode needs a typed
  rewrite, and some bit tricks (subtag extraction, raw shifts) get awkward.

Lean: **(A) raw words** — it keeps the interpreter a faithful VM and confines
the native-boundary work to a single bitcast at CALL/return. Confirm with a
spike (WS1.0).

## Workstreams

### WS0 — Decide the representation — DECIDED: (A) raw words (2026-06-20)
Decision and *why* (sharper than the original lean):
1. The interpreter **already** holds raw words — `interp.lisp` `:li` does
   `(setf (svref regs vd) imm)` with the raw immediate, no decode. (A) is the
   path of least change.
2. (B) real-values is **blocked without compiler changes**: the bytecode mixes
   **raw** immediates (a fast-path mask `(:LI 6 1)` = raw 1) with **tagged**
   immediates (an operand `(:LI 16 2)` = tagged 1) in the *same* stream. A
   real-values interpreter can't uniformly decode LI immediates — decoding the
   mask `1` as tagged gives `0` and breaks the fast-path. (B) would force the
   compiler to emit a second, interp-only bytecode dialect.
3. (A) keeps **one** bytecode format shared by the interpreter *and* the JIT —
   exactly the unified-representation thesis. (B) forks the bytecode; the JIT
   wants the tagged/optimized form anyway.
4. The native heap already stores raw words (= the native value representation),
   so structure the interpreter builds via raw `mem-write` of raw words is
   directly readable by native code — **no marshalling, identity/mutation
   preserved**.

The one genuinely new mechanism (→ first WS1 task): the **value↔word boundary**
at native CALLs.
- Fixnums: trivial (`ash` ±1).
- Pointers: a translator-level reinterpret — *raw word ↔ value*. Modus has the
  pieces (addresses are tagged fixnums you SHR to get the raw addr,
  `sap-new`/`sap-addr` move raw-addr↔SAP, `mem-ref` reads/writes the raw heap),
  but not yet a clean `:val->word` / `:word->val` op pair. **Alternative that may
  avoid it entirely:** do the native CALL as a *raw register call* (place raw
  words in arg regs, jump to the function's real address via `call-indirect`,
  read the raw word result) — then values never leave the raw-word domain and no
  bitcast is needed. Spike this in WS1.0.

Spike (deferred into WS1.0 since it needs the boundary mechanism): the
interpreter reads/writes a **real** native heap object via `mem-ref` and native
code sees the same object (identity/mutation) — proving the no-marshalling,
shared-heap property end to end.

### WS1 — Align the in-image interpreter value format to native (model A)
- **WS1.0 (spike) — DONE ✓ (2026-06-20, commit c4bd769).** Settled on **(ii)
  val↔word ops**: `%val->word` = SHL 1, `%word->val` = SHR 1 (a value's native
  word held as an integer is `word<<1`; the reinterpret is one shift, no type
  guard, because the runtime is tag-driven). In-image spike confirmed the
  boundary is cheap, identity-preserving, and **non-copying**:
  `(eq c (%word->val (%val->word c)))` = T for a real cons, it cars/cdrs
  correctly after the round-trip, and a `rplaca` through the reconstructed
  pointer is visible on the original (shared heap = no marshalling). The heavier
  raw-register-call (i) is unnecessary. Byte-identical to canonical.
- Rewrite `interp.lisp` value handling for model (A) — raw words throughout:
  - **Register file must be a raw `u64` buffer, not a Lisp `simple-vector`**
    (the current regs). This is load-bearing for **GC safety**: a pointer held
    as a *Lisp integer* looks like a fixnum to the moving collector, so it is not
    relocated and goes **stale** after a collection. A raw `u64` buffer holds the
    genuine tagged word `addr|tag`, which the conservative kind-bitmap collector
    recognizes as a root and updates on move — the native register-file model.
    Interpreter reads/writes registers via `mem-ref :u64`.
  - `cons`/`alloc-obj` allocate on the **real native heap** (drop the simulated
    `make-mvm-object` Modus-vector — that *is* the non-CL format to remove),
    returning the raw word (addr|tag).
  - `car`/`cdr`/`aref`/`obj-ref`/`obj-set` read/write the real heap via the raw
    word address (`mem-ref`), not a simulated vector.
  - arithmetic/branch/predicate opcodes already operate on raw words (faithful);
    keep. NIL/T already realigned to `#xDEAD0001`/`#xDEAD1009`.
  - NOTE: operating on a value needs **no** reinterpret — the tagging scheme is
    self-consistent under integer/bit/memory ops (tagged add = int add, tag
    check = low-bit mask, pointer = address+`mem-ref`). Reinterpret only arises
    at the native-call boundary (sidestepped by raw-register-calls) and is what
    the GC-scanned raw register file makes safe.
- The **host** interpreter may keep simulating (it is only a build tool); gate
  the in-image path so host self-tests / `demo.lisp` still pass.
- Exit: eval2 of `(+ 1 2)` returns a value `eq` to native `3` (no untag), and
  eval2 of `(f x)` can call a **native** runtime `f` directly. Re-run the Stage
  1–3 probes (arith, comparisons, IF, helper-call, recursion) on the aligned
  format.

### WS2 — Enumerate & remove other non-CL Modus formats (CONFIRM each)
- Symbols: collapse the CL-wrapper + native-MVM-sym dual system to one
  representation. (Cross-reference `SYMBOLS_PLAN.md`.)
- Strings/chars: decide whether the `%prim` code-array vs public-char split is
  worth unifying or is acceptable internal machinery.
- Produce a checklist in `DIVERGENCE.md` (one entry per format, with status).

### WS3 — Eval as JIT (the destination; mandatory per Philosophy)
- **JIT** — in-image `compile-form` (have it, Stage 2) + in-image `translate-x64`
  + executable mmap (W^X) + cache flush. Generated code calls runtime functions
  by **real address**, shares the heap ⇒ perfect CL semantics and acceptable
  performance from portable state.
- The **aligned interpreter** from WS1 is the bootstrap/interim eval *and* a
  permanent fallback (for slow paths, or arches before their JIT backend lands).
  With the format aligned it is a first-class option, not a dead end.
- Either way, no marshalling, because WS1/WS2 made the format uniform.
- Do x64-linux first; the portable state means other arches reuse the same
  bytecode/heap and only need their `translate-*` backend wired to the in-image
  JIT path.

### WS4 — Retire the tree-walker
- Wire `eval`/`load` to the chosen mechanism (JIT or aligned interpreter).
- Run the existing tree-walker as a **differential oracle** over ANSI + the
  ASDF gauntlet; fix every divergence until the new eval ≥ tree-walker.
- Delete `mvm/cl-eval.lisp` (the divergent second CL semantics) and its
  scaffolding.

## Sequencing & dependencies
```
WS0 (decide A/B) ─▶ WS1 (align interpreter) ─┬─▶ WS3 (eval: JIT or aligned interp)
                                             └─▶ WS4 (oracle, delete tree-walker)
WS2 (other formats) runs in parallel; symbols (WS2) gate full conformance but
not the eval milestone.
```
WS1 is the keystone — it both removes the marshalling wall and unblocks a clean
interpreter-as-eval; WS3's JIT is the long-term performance answer.

## Already done (reusable; not wasted)
- **Stage 2 — compiler self-hosted into the image.** `compile-form` /
  `mvm-compile-toplevel` run in-image. Foundation for *both* an aligned
  interpreter and a JIT.
- **Stage 1 — interpreter + ISA in the image**, callable from runtime EVAL.
- **Stage 3 — eval2 runs fns calling fns incl. recursion in-image**
  (`((defun fact (n) …) (fact 5))` = 120) — proves the pipeline; it used the
  *misaligned* format, which WS1 fixes.
- Committed byte-identical fixes en route: `defstruct :conc-name` verbatim,
  cross `#.` reader package, PUSH/POP/DECF→SETF, get-ir-instructions clears
  `*ir-buffer*`. (See git log on `aarch64-ansi-timeout`.)
- Held bundle (uncommitted): `interp.lisp` (opcodes, NIL/T realign, trace) +
  `build-generic.lisp` (compiler/interp/ISA spliced in, opcode-table populate,
  `eval2-forms`). Lands with WS1 once the format is aligned and scaffolding
  stripped.

## Decisions & open questions
- **SETTLED — JIT is the destination** (Philosophy: no external compiler +
  cross-arch state transfer ⇒ JIT is required for performance). The aligned
  interpreter is bootstrap/interim/fallback, not the endgame.
- OPEN — WS0: (A) raw words vs (B) real values for the aligned interpreter —
  confirm the lean toward (A) after the spike.
- OPEN — WS2 scope: which non-CL formats are in scope now vs later? (Symbols are
  the big one and interact with `SYMBOLS_PLAN.md`.)

## Risks
- **Layout shift.** Changing the interpreter's value handling touches a hot path
  and (for the JIT) executable memory — validate with the ANSI sweep, the way GC
  changes are judged (per-file/sub-sharded comm-diff), not a single coarse run.
- **W^X for the JIT** on bare metal (page tables) vs Linux (`mmap PROT_EXEC`) —
  per-arch; do x64-linux first.
- **Scope creep into the symbol rework** — keep WS2 confirmable item-by-item so
  WS1/WS3/WS4 (the eval line) aren't blocked on it.

## Done when
- `eval` and `load` run through the aligned mechanism; the tree-walker
  (`cl-eval.lisp`) is deleted; ANSI + ASDF gauntlet are ≥ their pre-change
  numbers; and there is exactly one value representation — the Common Lisp one —
  across compiler, interpreter, JIT, and native code.
