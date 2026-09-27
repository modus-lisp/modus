# Proposal: static literals in image code

Status: phase 1 (static keywords) implemented 2026-09-27 for the CLI and
ANSI builds; phases 2-3 proposed. Scope: every back end; the change is in the
compiler and image assembler, not in any one translator.

## Phase 1 results (measured)

- Enabled by `*static-keywords-p*` (set in `build-cli-common.lisp` and
  `build-x64-linux.lisp`); bare-metal and other images are unchanged.
  Static keywords per image: x64 511, i386 480, aarch64 662, riscv64/32 480.
- Identity holds on x64, i386, aarch64, riscv64 and riscv32 (qemu-user):
  a literal is `eq` to `intern`, `read-from-string`, `find-symbol`, `eval`
  and runtime-compiled `:foo`; boot reports no heap copies.
- Runtime compilation (`eval` of a `defun`, 50x): x64 42 -> 17 ms, i386
  161 -> 68 ms. The compiler is image code that dispatches on keyword IR
  operations constantly, so this is where it pays.
- `make-array` with `&key` arguments (100k calls): x64 1.70 -> 1.57 s (one
  keyword), 2.30 -> 2.00 s (three); i386 8.46 -> 7.57 s (three). **Interning
  turned out to be a minor part of `&key` cost**; the rest is elsewhere in
  keyword-argument handling (below, the "measured cost" figures were the
  motivation, not a measurement of interning alone).

## Problem

Code compiled into the image never loads a keyword, symbol or quoted list as a
constant. It rebuilds the value **every time the expression is evaluated**:

| literal in image code | what runs on every evaluation |
|---|---|
| `:foo` | `li v0, hash(FOO)` + `call %INTERN-KEYWORD` → `%rt-enter` (lock check) → `GETHASH` on the keyword table at `0x10000148` → `%rt-leave` |
| `'foo` | `li v0, hash; li v1, pkg-hash; call %INTERN-SYMBOL-PKG` (same shape) |
| `'(a b c)` | a fresh cons per element, and an intern call for each symbol inside |
| `"text"` | already static: `:li-const` patched to the image string pool |

This happens on every architecture, because it is `compile-keyword` and
`compile-quote` (`mvm/compiler.lisp`) that emit the calls, not a translator.

Code compiled at *runtime* (mvm-eval) already avoids it. It looks each literal
up once at compile time and loads it from the quote pool (`:li-const`). That
change was made for reel's VP8 decoder, where keyword literals cost 8% of a
frame. The image build still takes the old path.

### Measured cost (multi-arch 3608b79)

- `(make-array 3)` from a runtime loop costs 9.9 µs per call on x64. Adding
  `:initial-element 0` makes it 17.0 µs, and three keyword arguments make it
  23.0 µs; i386: 57.6, 69.9 and 84.6 µs. How much of that difference is
  keyword interning versus the extra arguments themselves has not been
  isolated. But profiles of calls from interpreted code show `MAKE-ARRAY` →
  `%INTERN-KEYWORD` → `GETHASH` as a hot chain, and its `&key` parsing interns
  each keyword it compares against.
- In core-dump profiles, `%INTERN-KEYWORD` → `GETHASH` was on the stack in
  28% of x64 samples of an interpreted call loop, and was the top self-time
  item (GETHASH 17.5%, `%RT-ENTER` 10%) of an i386 interpreted loop. There the
  interpreter's own `:lt/:eq/:gt` flag keywords were paying it on every
  compare and branch; 3608b79 changed those to fixnums.
- Scale: the runtime sources (prelude, `cl-*`, ansi-bridge, interp) contain
  about 1,760 keyword literal sites (293 distinct keywords) and about 255
  quoted-list literals.

### Semantics, not just speed

A quoted list in image code is freshly consed on each evaluation, so
`(defun f () '(1 2))` gives `(eq (f) (f))` ⇒ NIL. In CL a literal is one
object, so this should be T. Nothing depends on the fresh copy, because
modifying a literal is undefined behaviour anyway, so this is a divergence to
remove, not a feature to keep.

## Why it is like this

Symbols and keywords are heap objects looked up by name hash, and the
collector copies them. The image had no GC-safe place for code to keep "the
object for FOO", so every use looked it up again. The lock (`%rt-enter`) came
later, with threads, and made each lookup cost a little more. Strings escaped
this only because they are never interned: a string literal can live in the
static pool, where nothing ever needs to find it by name.

## Goals

1. Loading a keyword literal in image code costs one baked immediate: no call,
   no hash, no lock.
2. Loading a symbol or quoted-structure literal costs at most a few loads,
   with no call.
3. `eq` identity with objects produced at runtime by the reader, `intern`,
   `find-symbol` and runtime-compiled code is preserved exactly.
4. It works on every architecture through machinery they already have, with no
   new opcode.

Non-goal: making runtime defuns native. That is the JIT's job (the x64
`*x64-jit-constvec-full-p*` path already took a quoted-literal-bearing defun
from 12.78 s to 1.22 s) and it is independent of this change.

## Design

### Phase 1: static keyword objects

A keyword object is one slot holding its name hash, a fixnum
(`compile-make-keyword-obj`: `alloc-obj 1 +subtag-keyword+`). It contains no
heap pointer. So a keyword can live **outside the GC heap** exactly as a string
literal does, and the collector never has to know about it:
`%gc-forward-slot` only moves from-space pointers, and nothing inside a
keyword points into the heap.

1. **Build time.** `compile-keyword` (image build path only, i.e. when
   `*mvm-eval-runtime-p*` is NIL) records the keyword in a build-wide table and
   emits `:li-const dest <idx>` into the image constant table instead of the
   intern call.
2. **Image assembly.** `assemble-kernel-image` lays out one static keyword
   object per distinct keyword in the constant pool (header with
   `+subtag-keyword+`, then the tagged name hash). The existing per-arch
   `*<arch>-li-const-patches*` pass bakes each site's address, just as it does
   for strings today. All seven back ends have patch lists (i386's lives in
   `modus.mvm.i386` and `cross.lisp` finds it by name at run time).
3. **Boot.** Before the first keyword can be interned at runtime, boot seeds
   the keyword table (`0x10000148`) with every static keyword, keyed by name
   hash. From then on `%intern-keyword` returns the static object for those
   names. The reader, `intern` and runtime-compiled code are therefore `eq` to
   the image's literals by construction (goal 3). Keywords first created at
   runtime still go to the heap, as today.
4. **Threads.** Loading a static address is a read of code, so it needs no
   lock. The table itself keeps its lock for runtime interning.

Expected effect: every keyword literal in image code drops from a call, lock
check and hash lookup to one `movabs`, or its per-arch equivalent. That covers
all of `&key` parsing in the runtime.

### Phase 2: symbols and quoted structure via a literal vector

A symbol has heap-pointer slots (`[hash, package, name]`), and a quoted list is
conses. Neither can be static unless the collector scans it. For these, reuse
the pattern the image already trusts twice: the JIT constant vector (root
`0x10000F00`) and the global-cell cache vector (root `0x10000FA0`).

1. **Build time.** Each distinct symbol literal, and each quoted cons tree
   (which may contain symbols and keywords), gets an index in a build-wide
   *literal vector* plan. The site compiles to the Lisp-level equivalent of
   `(svref (mem-ref LITVEC-ROOT :u64) idx)`: three dependent loads, the same
   sequence the x64 JIT constvec emits. Because this is expressed at the IR
   level through existing memory and object-reference ops, **no translator
   changes**.
2. **Boot.** A boot step allocates the vector, interns each symbol with its
   home package (the same `%intern-symbol-pkg` call sites make today, but once
   per literal), and builds each quoted tree once.
3. **GC.** Add `LITVEC-ROOT` (a free BSS word; choose it with the repo-wide
   `#x10000[EF]..` collision check the constvec comment asks for) to the root
   list of **every** collector: the Lisp one in `%gc-scan-globals` and each
   native trampoline. As the comments there already warn, keep them in step.
   The vector is an ordinary heap object, so Cheney forwards its contents.
4. **Semantics.** A quoted list is now one object, so `(eq (f) (f))` ⇒ T.

### Phase 3 (optional): static symbols

If phase 2's three loads show up in a profile, symbols can follow keywords into
static space. Their package and name slots would then have to be registered as
roots, or those objects made static too. Don't do this until a measurement
asks for it.

## Risks

- **Boot order.** The keyword table must be seeded before the first
  `%intern-keyword` (reader init, `%init-signal-symbols`). Keywords created
  earlier would duplicate a static one and break `eq`. Guard it: after
  seeding, assert that no pre-existing entry has the same name hash as a
  static keyword.
- **Root-set drift.** Every added root must be in every collector. The x64
  trampoline and the Lisp scan have drifted before (see the 0xCA0 note in
  `gc.lisp`). A rung that collects while a literal vector entry is the only
  reference to its object would catch drift on each arch.
- **Byte-identity.** Shipped images change, which is intended. Record the new
  image sizes, and `cmp` any image whose byte-identity is still promised.
- **Image size.** About 300 static keywords at 16 bytes each is about 5 KB.
  The literal vector is one word per distinct literal plus the objects
  themselves, which are allocated at boot today anyway.

## Validation

- Microbenchmarks: `make-array` with 0, 1 and 3 keyword arguments, and an
  interpreted call loop, on x64 and i386, before and after.
- `eq` tests: `(eq :foo (intern "FOO" :keyword))`,
  `(eq :foo (read-from-string ":foo"))`, a runtime-compiled `:foo` against
  an image `:foo`, and `(eq (f) (f))` for a quoted list.
- A GC rung per phase: force a collection between building a literal and
  using it.
- The full gates: x64 ANSI, library ladder on x64 and i386, `md5-substrate`,
  `word-boundary`, and the arch ladder (bare and hosted).

## Alternatives considered

- **Lock-free lookup in `%intern-keyword`.** It removes the `%rt-enter` part
  but keeps the call and the hash lookup, so it is a small win, not a fix.
- **Per-call-site inline caches.** Each site would need a writable, GC-scanned
  cell, which is the literal vector with extra steps.
- **Make every literal a phase-2 vector load**, keywords included. That is
  simpler, with one mechanism, but it costs three loads where phase 1 needs
  one, for the most common literal kind.

## Open questions

1. ~~How does i386 bake constants?~~ Answered: its own patch list, found by name.
2. Is the boot keyword-table seeding cheap enough on bare-metal images, or
   should the table be pre-built into the image as static data?
3. Should phase 2 also take over the runtime quote pool's per-execution
   `GETHASH` in the interpreter's `:li-const` (`*e2-const-pool*` is a hash
   table keyed by index), for example by backing it with the same kind of
   vector?
