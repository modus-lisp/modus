# Function-Pointer Tag Plan

## Problem

Modus's funcall-tag collision class.  Function pointers are stored as
raw native addresses with no tag bits.  The closure-aware funcall
dispatch decides "this is a closure-env cons" vs "this is a bare
function pointer" by testing `(val & 0xF) == 1` — i.e., it reuses
the cons tag.  Any function whose native address happens to end in
`0x1` is misidentified as a cons; the dispatch dereferences code as
if it were a closure pair and jumps into the middle of an unrelated
function.

The current mitigation is `*x64-native-code-offset*` plus NOP
alignment in the translator, which tries to dodge unfortunate offsets.
That dodge is precarious: adding 5KB of native code anywhere (a new
defun, a larger docstring, an extra primitive) can shift a downstream
function across an alignment boundary and re-introduce the collision.
Every "mystery crash that moves when we resize an unrelated function"
in CLAUDE.md is this class.

## Comparison with other implementations

### CCL (3-bit primary `lisptag`, 4-bit `fulltag`)
```
tag 0  fixnum         tag 4  TRA (tagged return address)
tag 1  single-float   tag 5  misc (uvector/array)
tag 2  char/imm       tag 6  symbol
tag 3  cons (+NIL)    tag 7  function
```
8 categories in 3 bits.  `functionp` is `(val & 7) == 7`.  Functions
and conses have distinct primary tags — collision impossible by
construction.  The 4-bit fulltag distinguishes even/odd fixnums and
provides node-header / imm-header variants.

### SBCL x86-64 (4-bit lowtag, 4 pointer tags)
```
xxx0  fixnum (low bit 0)
0001  instance-pointer (CLOS / structure)
0011  list-pointer (cons, NIL)
0101  fun-pointer
0111  other-pointer (symbol, array, string, ... — subtag in header)
1001  immediate-1 (chars, markers)
1011  immediate-2
1101  immediate-3
1111  reserved
```
Properties:
- fixnum tag 0 → `add`/`sub` are tag-preserving.
- All pointer tags odd → `(val & 1)` distinguishes pointer/non-pointer.
- All pointer tags `0xxx`, all immediates `1xxx` → `(val & 8)`
  distinguishes pointer/immediate.
- Four distinct pointer tags for the four hottest dispatch classes;
  everything else goes through `other-pointer` with a subtag in the
  object header.

### Modus (current)
```
xxx0  fixnum
0001  cons
0011  unused
0101  immediate (char, NIL, T)
0111  unused
1001  object (subtag in header byte — symbol/array/string/etc.)
1011  unused
1101  unused
1111  forward (GC)
```
Functions: **untagged raw native addresses** → collide with `0001`
whenever the addr ends in `0x1`.

### Genera
Different beast — Lisp Machine 36/40-bit words with separate tag and
CDR-code fields.  Includes `dtp-fix`, `dtp-list`, `dtp-symbol`,
`dtp-compiled-function`, etc. — distinct tags for distinct types,
just in a wider word.  Not directly applicable to a 64-bit
4-bit-tag impl.

## What's worth copying

The **principle**, not the numbering:

1. **fixnum tag = 0** for fast arithmetic.  Modus has this.
2. **Distinct tags for hot type predicates.**  `consp` and
   `functionp` are checked constantly — making each
   `(val & 0xF) == K` for a constant K is one cycle.  Funneling
   everything through "object with subtag in header" costs a memory
   load every time.

The specific numbering (whether cons is `0001` or `0011`) isn't
load-bearing — what matters is that the hot tags are distinct
constants the compiler can compare against directly.

## Renumbering?  No.

Don't renumber.  `cons=1` means CAR is `mov rax, [rax-1]` and CDR is
`mov rax, [rax+7]`.  Renumbering to `cons=3` (SBCL-style) makes them
`[rax-3]` / `[rax+5]` — hundreds of touch points across every
translator and every primitive for zero correctness gain.

## Recommendation: minimal tag-add

Claim `0011` for function pointers.  Reasons to pick `0011`:
- Adjacent to cons (`0001`), so future code can collapse "heap-allocated
  cons-or-function" with a single mask check `(val & 0xD) == 1`
  if that ever becomes useful.
- Currently unused.
- 0x3 is small enough that `[rax-3]` and `[rax+5]` (the analogous
  car/cdr-style accesses) fit in 8-bit displacements.

### Implementation steps

1. **`LI-FUNC`**: every translator's emit-LI-FUNC needs to emit the
   address with the tag set.  Look for `+op-li-func+` handlers in
   `mvm/translate-{x64,aarch64,arm32,i386,riscv,ppc,68k}.lisp`.
   Where the current code emits a raw address, OR with `3`:
   `movabs rax, addr | 3` on x64, `MOVZ+MOVK` of `addr | 3` on
   AArch64, etc.

2. **`CALL-IND` / funcall dispatch**: every translator's indirect-call
   path needs to strip the tag before calling.  Either:
   - `sub rax, 3; call rax` (cheap, 1 instruction added per call)
   - `call [rax-3]` if the function pointer is stored boxed (need to
     check if anywhere stores tagged-fn in memory then derefs)

3. **Closure-vs-bare-fn discriminator**: the code that tests
   `(val & 0xF) == 1` to detect a closure-env cons must instead
   test `== 3` to detect a function (and continue to test `== 1`
   for cons-as-closure-env separately).  Search for the
   tag-collision-dodge comments in each translator.

4. **`functionp`**: a new MVM op or just `(val & 0xF) == 3`.  Add
   `+subtag-function+` and the compile-functionp routing.

5. **GC scan**: any GC code that walks slots looking for "is this a
   pointer to follow" needs to recognise the new tag.  Function
   pointers are NOT heap allocations — they point into the code
   region, not the GC'd heap.  The scan should *skip* tagged-function
   values, not copy or rewrite them.  Likely live in
   `mvm/translate-x64.lisp`'s `emit-gc-trampoline` and the AArch64
   equivalent.

6. **`#'FOO` ↔ called-via-funcall identity**: today `#'FOO` returns
   a raw address.  After tagging it returns `addr | 3`.  Any code
   that compares function pointers with `eq` (some ANSI tests do)
   still works because both sides get the same tagging.  But any
   code that does `(funcall (mem-ref ADDR :u64))` where ADDR was
   computed from a function offset directly (without going through
   `LI-FUNC`) won't have the tag set and will fault on the
   tag-strip in funcall.  Audit for such sites; they likely live in
   `cl-eval.lisp`'s `*symbol-function-table*` lookup paths.

## Risk

Multi-arch, multi-file change.  Each translator must be updated
together — a half-tagged build with x64 tagged but AArch64 not would
crash immediately.  Recommended order:

1. Land on x64 first (smaller test surface, full ANSI run cycle is
   600s).  Validate ANSI passes don't regress.
2. AArch64 next (already has separate fragility class — this might
   unstick the dominant SIGSEGV signature).
3. i386 / arm32 / RISC-V / PPC / 68k last (less frequently exercised).

The branch-offset asserts (committed earlier as the precursor) will
catch any layout-induced regression at build time instead of as a
mystery crash, so the tag work is well-scaffolded for landing.
