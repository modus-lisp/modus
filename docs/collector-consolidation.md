# Collector consolidation: one source, N targets

**Status:** design, not implemented. Written 2026-08-19, immediately after the
bare-metal GC chain (#263) was closed.

## The situation

Modus has **four implementations of one algorithm**:

| implementation | where | size | used by |
|---|---|---:|---|
| `emit-gc-trampoline` | `translate-x64.lisp` | 572 lines | x64 (all images) |
| `emit-aarch64-native-gc-trampoline` | `translate-aarch64.lisp` | 420 | aarch64 CLI + Linux |
| `i386-emit-gc-trampoline` | `translate-i386.lisp` | 177 | i386 |
| `%gc-collect` & friends | `gc.lisp` | ~500 | **bare-metal RPi only** |

~1,670 lines, four notations, one Cheney semi-space collector. Three are
hand-written target assembly; the fourth is Lisp interpreted at runtime.

This is the *one* component that never received the architecture Modus applies
everywhere else: **one IR, N translators**.

## Why this is not merely untidy

Each arm exists because the shared thing was broken, and the fork was cheaper
than the fix. `translate-i386.lisp`'s own docstring says so:

> "gc.lisp cannot see a heap pointer at all … that is why this arm is native."

That is **the exact defect fixed on 2026-08-19** (`b65730c`): `%gc-read64`
returned `word/2`, so `%gc-is-pointer` rejected every real pointer. It was
found on i386 in July, worked around by writing a fourth collector, and left
live for the one target still running the shared code — where it went on to
produce, in sequence: unforwarded stack roots, a stale pointer surviving in a
register, a reentrant `%SIGNAL-TYPE-ERROR`, a 121 MB stack overflow across the
kernel's own code, and an undefined-instruction fault into an unported vector.

**Forking to avoid a bug leaves the bug loaded and pointed at whoever didn't
fork.**

The corollary is about visibility. Because x64 collects natively, the 64-shard
ANSI gate never calls `%gc-collect` at all — it is nearly **blind** to
`gc.lisp`. The gate read 17522 before and after that entire bug chain. Four
implementations means four separate things to gate, and in practice only the
ones an arch actually runs get exercised.

## The proposal

Write the collector **once**, in an allocation-free Lisp subset, and compile it
through the normal MVM pipeline so every target gets native code from one
source.

This is what `gc.lisp` was reaching for. Its fatal mistake was not being
portable — it was being **interpreted at runtime** instead of compiled. Both of
its failure modes follow directly from that:

- word access went through `mem-ref`, whose `:u64` read halves the value
  (hence `word/2`, hence nothing was ever forwarded);
- ordinary-looking Lisp arithmetic could allocate — `(ash 1 bit)` with a
  variable count compiles to a **consing** `BIGNUM-ASH`, and one cons inside
  `%gc-collect` re-trips `:gc-check` and re-enters the collector (1835-deep,
  measured).

Compiled, both disappear: word access becomes real loads, and arithmetic
becomes inline instructions.

### The part that makes it safe: a build-time no-allocation verifier

Mark the collector's functions, then **assert at build time** that their
compiled output contains:

- zero allocation sites (`:alloc-obj`, `:alloc-array`, `:alloc-string`, cons),
- zero `:gc-check`s,
- zero calls to any function not itself in the verified no-alloc set.

That single check would have caught, at build time rather than as a hang on a
Pi:

- `(ash 1 bit)` → `BIGNUM-ASH` (the bug this document was written after),
- `print-dec` inside `%gc-collect` (one of my own diagnostic probes),
- `(mem-ref a :u64)` on a word with high bits set, which can build a bignum
  (another of my probes).

Three instances of one class in a single session. The verifier converts "we
must remember not to allocate here" into "it cannot build if you do."

## Why not the alternatives

**Port x64's collector to i386.** Considered and rejected. i386 already has a
working native Cheney and it is the *smallest* of the three (177 lines). Porting
x64's 572 lines would replace a working implementation with a larger translated
one — substitution, not consolidation — and it would be a rewrite anyway:
different register set, and every offset is word-size dependent (`cdr` at +4 not
+8, size `(count+1)*4` not `(count+2)*8`).

**Abandon Cheney for a non-moving collector.** Tempting, because nearly every
catastrophic bug here is a *moving*-collector bug: unforwarded roots, stale
registers, a false root copied using data as a header, plus the forwarding
pointers, object-start bitmap, cons-kind bitmap, copy invariants and pinning
machinery that exist solely to make moving safe under **conservative** roots.
A non-moving collector makes a false root retain garbage instead of corrupting
memory — categorically the better failure mode.

But the cost is not in the collector, it is in the **codegen**: bump-pointer
allocation with an inline `gc-check` is baked into all seven translators, and a
free-list allocator changes every one of them. If this is ever revisited, the
shape to look at is **mark-region / Immix**: non-moving, but allocation stays
bump-pointer *within a block*, so the existing inline alloc sequence survives
nearly intact. That is a separate decision and should be made on its own
evidence — not smuggled in with a consolidation.

*(Note on the record: #102 characterises the pinning collector as
"layout-fragile." Treat that with suspicion. In this project the "layout"
verdict has never survived contact with evidence — the last instance was 253
"layout losses" that were shards hitting the 600s timeout, and a fuzz sweep over
8000 tests produced zero diff. There is always something else wrong.)*

## Sequencing

1. **Migrate bare-metal RPi to the native aarch64 arm.** Set
   `*aarch64-gc-native-mcgc*` in `build-rpi-cl-repl.lisp` and give the
   bare-metal boot preamble the trampoline call. At 54 MB the image fits `BL`'s
   ±128 MB reach, so it likely does *not* need the `x28` materialisation the
   ~200 MB gate image required.
2. **Delete `gc.lisp`'s collector** — `%gc-collect`, `%gc-scan-*`,
   `%gc-copy-object`, `%gc-forward-slot`, the word-access layer. Note
   `%gc-collect` has **zero Lisp callers**; it is reached only by name from the
   trampoline shim.
   **Keep** the metadata/config half: `%gc-init`, `%gc-bitmap-init` and the slot
   accessors are called by the *native* builds (`build-aarch64-linux`,
   `build-aarch64-cli`, `build-aarch64`, `build-rpi-cl-repl`).
3. **Build the no-alloc verifier** and point it at the three native arms'
   Lisp-side helpers first — it is useful before unification and it is what
   makes unification safe.
4. **Unify**: express the collector once, lower per target, delete the three
   hand-written arms.

Steps 1–2 are near-term and independently valuable. Steps 3–4 are the actual
consolidation.

## How to gate any of this

The ANSI gate is nearly blind here (see above), so a green gate means "did not
leak into x64", **not** "the collector is correct". Real evidence comes from a
target that runs the code under test:

- the bare-metal RPi alexandria repro (`MODUS_NET_BUFSZ=400000` is mandatory);
- the phase trace: a healthy collection prints `12345`. Repeats without a `5`
  mean re-entry; `1S2345` means `saved_rsp` failed its window check and the
  stack scan was **skipped**;
- an allocation-free heap/stack verifier spliced in after the scan and before
  the swap — and **validate the verifier against a planted value written by the
  path under test**, never by round-tripping the API being questioned.
