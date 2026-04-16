# Plan: Full ANSI Common Lisp Implementation

## Current State (2026-04-16)
- 17,568 ANSI tests, 14 failures (99.92% pass rate)
- ~400 CL functions implemented across 10 runtime modules
- ~10 real CL functions missing (rest are test infrastructure)
- Compiler: MVM bytecode → x64 native, self-hosting, mutable closures, unwind-protect
- Runtime: bare-metal + Linux userspace, tagged 63-bit fixnums + bignums

## What's Done

```
Layer 0: Packages          ✓  24 functions, CL/CL-USER/KEYWORD
Layer 1: Streams           ✓  9 stream types, read/write-char
Layer 2: Reader            ✓  full read, readtable, #-dispatch, backquote
Layer 3: Printer           ✓  write with *print-* vars, 50+ format directives
Layer 4: Format            ✓  ~A ~S ~D ~B ~O ~X ~R ~C ~P ~(~) ~[~] ~{~} ~^ etc.
Layer 5: Conditions        ✓  24 types, handler-bind/case, restarts, unwind-protect
Layer 6: CLOS (minimal)    ✓  struct-based defclass, make-instance, slot-value
Layer 7: File I/O          ✓  Linux syscalls, file streams, pathnames
Layer 8: Eval/Compile/Load ✓  tree-walking interpreter, load from file
```

## What's Left — Road to Quicklisp

### Phase 1: Real CLOS (required for Quicklisp)

ASDF, most libraries, and the condition system all depend on real CLOS.
We have struct-based defclass/make-instance/slot-value. We need:

**defgeneric / defmethod / method dispatch:**
- Generic function = (name lambda-list methods)
- Method = (specializers qualifiers function)
- Dispatch: for each arg, find applicable methods by class precedence
- Single dispatch first (90% of real code), then multi-dispatch
- `call-next-method`, `next-method-p`

**Standard method combination:**
- :before, :after, :around methods
- Standard combination: around wraps (call-next-method → before → primary → after)
- `define-method-combination` (short form covers most cases)

**Class hierarchy:**
- `standard-class`, `standard-object`, `built-in-class`
- Class precedence list computation (topological sort)
- `subtypep` integration with class hierarchy
- `typep` dispatches to class membership

**Slots:**
- `:initarg`, `:initform`, `:accessor`, `:reader`, `:writer`
- `:allocation :class` (shared slots) vs `:instance`
- `initialize-instance`, `shared-initialize`
- `slot-missing`, `slot-unbound` (have basic version)

**Implementation approach:**
Generic functions as hash tables mapping specializer-tuples → method lists.
Dispatch via `class-of` → class precedence list → find most specific method.
No MOP metaclass protocol needed for Quicklisp — just the user-facing API.

### Phase 2: GC (required for anything long-running)

Current: bump allocator, never frees. Any Quicklisp load exhausts the heap.

**Cheney copying collector (~300 lines):**
- Two semispaces: from-space and to-space
- On collection: scan roots (stack, globals), copy live objects, swap spaces
- Tagged values make root scanning trivial — bit 0 tells fixnum vs pointer
- Allocation stays bump-pointer (already have this)

**Generational (+300 lines):**
- Nursery (2-4MB) collected frequently
- Tenured space collected rarely
- Write barrier: one check after set-car/set-cdr/aset

**Per-actor heaps (future):**
- Each actor has own nursery + tenured
- GC is per-actor, no global pause
- Already have per-actor heap regions

### Phase 3: Runtime Compilation (required for ASDF)

Current: `compile` is a stub. `compile-file` doesn't exist.

**Wire MVM compiler into runtime:**
- The compiler already exists (mvm/compiler.lisp)
- Need to make it callable from the runtime `compile` function
- Source → MVM bytecode (the compiler) → x64 native (the translator)
- Install compiled function into the function table

**compile-file:**
- Read source forms from file
- Compile each top-level form
- Write FASL (compiled bytecode) to output file
- `load` on FASL loads bytecodes and translates to native

**ASDF integration:**
- ASDF calls `compile-file` + `load`
- Needs `*features*`, `logical-pathname-translations`
- Needs `require`/`provide` (have stubs)

### Phase 4: Numeric Tower

Current: 63-bit fixnums + 2-slot bignums + stub floats.

- Full bignum arithmetic (arbitrary precision)
- Ratios (num/denom pair, auto-reduce)
- IEEE 754 double floats (have boxing, need full ops)
- Complex numbers
- `rational`, `rationalize`, `float`, coercions

### Phase 5: Setf Machinery

Current: compiler handles `(setf (car x) v)` etc. for known places.

- `defsetf` (short and long form)
- `define-setf-expander`
- `get-setf-expansion`
- User-defined setf places

### Phase 6: Polish

- `trace`, `untrace`, `step`
- `describe`, `inspect`, `documentation`
- `time`, `room`
- Full `loop` (hash-table iteration, `into`, destructuring)
- `multiple-value-setq`, `nth-value`
- `compiler-macroexpand`
- Logical pathnames
- `*features*` properly populated

## File Organization

```
mvm/cl-sequences.lisp    837L  Sequence functions
mvm/cl-streams.lisp      145L  Stream type system
mvm/cl-fileio.lisp     1,036L  File I/O + Linux syscalls
mvm/cl-printer.lisp    1,545L  Printer + format
mvm/cl-reader.lisp     1,383L  Reader + readtable
mvm/cl-packages.lisp     909L  Package system
mvm/cl-conditions.lisp   915L  Condition system
mvm/cl-clos.lisp         429L  CLOS (expand this)
mvm/cl-eval.lisp       1,353L  Eval/compile/load
mvm/cl-types.lisp        519L  Type predicates
mvm/prelude.lisp              Core runtime (hash tables, equal, etc.)
mvm/compiler.lisp              MVM compiler
mvm/translate-x64.lisp         x64 native translator
```

## Known Bugs

1. **ASET on cons-cdr arrays**: `%cl-sym-set-package` stores package into
   symbol data array but value doesn't persist. Causes GENTEMP.4 failure.
   Root cause: unclear — inline `(cdr sym)` + aset segfaults, function-call
   `(%cl-sym-data sym)` + aset silently drops the value.

2. **Symbol identity**: `%intern-symbol` sometimes creates duplicate objects
   for the same name-hash. Two symbols with identical hashes may not be `eq`.
   Mitigated by `equal` comparison in CLOS/package code.

## Quicklisp Readiness Checklist

```
[ ] defgeneric / defmethod with dispatch
[ ] Standard method combination (:before/:after/:around)
[ ] Class precedence lists
[ ] initialize-instance / shared-initialize
[ ] GC (at least Cheney copying)
[ ] compile-file → FASL
[ ] Runtime compile (source → native)
[ ] ASDF loadable
[ ] Full bignum arithmetic
[ ] IEEE 754 float operations
[✓] Packages
[✓] Streams + file I/O
[✓] Reader + printer + format
[✓] Conditions + restarts + unwind-protect
[✓] Eval + load
[✓] Mutable closures
[✓] 99.92% ANSI conformance
```
