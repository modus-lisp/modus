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

## First-attempt notes (`b9efb17` → reverted in `b7fe791`)

Tried the minimal x64 tag-add per "Implementation steps" above.
Build succeeded, no branch-offset asserts fired (the precursor work
held).  Linux/x64 sharded run delta:

  Before: P=11576 / F=5881 / lost=235  SIGSEGVs=549
  After:  P=11527 / F=5930 / lost=235  SIGSEGVs=780

By unique-ID comparison: +52 newly passing, -50 newly broken = +2
net.  The dominant pre-tag SIGSEGV signature (~125 occurrences from
RUN-ANSI-POP$$LAMBDA66106 indirect calls) WAS eliminated — that's
the funcall-tag-collision class going away.  But a new dominant
signature took its place: ~125 occurrences of RIP/4=933969919, which
decodes to RIP = 0xDEACFFFE = NIL - 3.

That signature is `sub rax, 3; call rax` where rax = NIL — i.e.,
`(funcall NIL)` from somewhere in the CLOS allocate-instance /
change-class path (test ids 26908+).  Pre-tag, the same paths
called NIL directly and SOMEHOW didn't crash — likely the SIGSEGV
landed at NIL (= 0xDEAD0001) which is mapped (one of the address
slots), so the fault was recoverable via handler-case longjmp.
Post-tag at NIL-3 the fault is on a different / unmapped page and
the longjmp recovery doesn't catch as cleanly.

What this means for the next attempt:

1. **The collision IS structural and the dodge IS load-bearing.**
   Removing the dodge AND adding tag works (`*x64-native-code-offset*`
   alignment loosened from "avoid nibble 1/9" to "require nibble 0"
   was correct).

2. **NIL-funcall recovery has architecture-dependent latency.**
   Pre-tag NIL=0xDEAD0001 happens to be in a mapped page; the
   SIGSEGV handler at 0x4F0820 longjmps to handler-case successfully.
   Post-tag NIL-3=0xDEACFFFE is at the END of the page-before-NIL
   (or the start of an unmapped page) — different fault mechanics.
   Need to audit cl-eval.lisp's SIGSEGV stub for "fault address
   near NIL" handling, or have compile-funcall test for NIL/T
   explicitly before calling.  CCL handles this with `fulltag-nil = 11`
   (separate tag for NIL alone) and tests for it in funcall.

3. **Closures may need slot-0 audit.**  The `(emit-ir :obj-ref
   fn-call-reg fn-call-reg 0)` extracts the function from a closure
   and call-indirect strips the tag.  If %make-closure stored a
   tagged value (which it does, since fn-form compiles via LI-FUNC),
   the obj-ref returns tagged, call strips, correct.  Verified
   working on regular closures; CLOS method functions stored in
   `(cons qual (cons specs fn))` cells are NOT obj-refs but cddr
   accesses — those return whatever was stored by %make-method,
   which is the value passed in by defmethod's compile path.  Need
   to confirm that path also produces a tagged value.

4. **Audit `eq` on fn-pointers.**  Any code that compares
   `(eq #'FOO stored-fn)` requires both sides to be tagged
   consistently.  If one side comes from `(symbol-function 'FOO)`
   (table lookup, tagged) and the other from `#'FOO` directly
   (LI-FUNC, tagged), good.  But if any path manufactures a raw
   fn-addr (e.g., via a cross.lisp emit-fn-addr that doesn't go
   through LI-FUNC), the comparison fails.

Plan for next attempt: same minimal tag-add, PLUS explicit
NIL/T-funcall guard in compile-funcall:
```
(emit-ir :cmp fn-call-reg +vreg-vn+)
(emit-ir :beq nil-funcall-label)
;; ... existing dispatch ...
nil-funcall-label:
(emit-ir :call "%SIGNAL-UNDEFINED-FUNCTION" 0)
```
This converts the NIL-funcall fault into a clean condition signal
that handler-case catches the same way as today.  Add the same
guard for the T sentinel.
