# OBJ-SET Register Clobber Bug

## Summary

`(aset array 1 value)` via OBJ-SET opcode silently drops the stored value
when called through a function like `%cl-sym-set-package`. The value reads
back as nil. Only affects non-fixnum values (cons cells, arrays, packages).
Fixnums store correctly.

## Reproducer

```lisp
(defun %cl-sym-set-package (sym pkg)
  (aset (%cl-sym-data sym) 1 pkg))  ;; pkg doesn't persist

;; But this works:
(defun %test-set-slot (arr val)
  (aset arr 1 val)   ;; non-last form
  arr)                ;; val persists
```

## What Works / What Doesn't

| Pattern | Result |
|---------|--------|
| `(aset data 1 42)` | WORKS (fixnum) |
| `(aset data 1 (cons 1 2))` | WORKS (fresh cons) |
| `(aset data 1 (make-array 7))` | WORKS (fresh array) |
| `(aset data 1 (make-package ...))` | WORKS (fresh package) |
| `(aset data 1 *package*)` | FAILS (existing package) |
| `(aset data 1 (find-package "CL-USER"))` | FAILS (existing package) |
| `(aset data 1 pkg)` where pkg is function param | FAILS (when pkg = existing package) |

The distinguishing factor appears to be the **memory address** of the value.
Freshly allocated objects (high heap addresses) store correctly. Objects
allocated during init (low heap addresses, like `*package*`) do not persist.

## Generated Code Analysis

`compile-aset` with constant index emits:
```
arr-reg = alloc-temp-reg()  ;; V4 = RBX
val-reg = alloc-temp-reg()  ;; V5 = RCX
compile-form arr-form → arr-reg   ;; MOV RBX, [frame-slot]
compile-form val-form → val-reg   ;; MOV RCX, [frame-slot]
emit OBJ-SET arr-reg 1 val-reg    ;; MOV [RBX+15], RCX
emit MOV dest val-reg
```

The x64 translator for OBJ-SET with two physical registers (po, ps both non-nil):
```lisp
((and po ps)
 (emit-mov-mem-reg buf po ps offset))
```

Emits: `48 89 4B 0F` = `MOV [RBX+0xF], RCX` (REX.W + MOV r/m64,r64)

The instruction encoding is correct. The offset (15 = slot 1 past header)
is correct. Static analysis of the compiler and translator shows no obvious bug.

## Hypothesis

The store instruction executes but the value doesn't persist, suggesting either:

1. **Write to wrong address**: RBX contains a stale or incorrect array pointer.
   The function call to `%cl-sym-data` returns in RAX, MOVs to RBX. If
   something between the MOV and the OBJ-SET clobbers RBX, the store goes
   to the wrong location.

2. **Memory aliasing**: The array's slot 1 occupies memory that overlaps
   with something else (GC metadata, page table, memory-mapped region).
   Objects at low addresses (init-time allocations) might be in a region
   with different properties than heap allocations.

3. **Tag bit interaction**: OBJ-SET stores the raw 64-bit value. If the
   translator or some memory subsystem strips or mangles tag bits for
   values in certain address ranges, the store could silently become nil.

## What's Needed

1. **gdb session**: Set breakpoint on the `MOV [RBX+0xF], RCX` instruction
   inside `%cl-sym-set-package`. Verify RBX and RCX values. Step past the
   store and verify the memory location changed. Check if it reverts.

2. **Instruction dump**: Use `objdump` to disassemble `%cl-sym-set-package`
   and verify the generated code matches expectations. The function's
   name-hash is 776306660281531559.

3. **Hardware watchpoint**: Set a hardware write watchpoint on the array
   slot to see if anything writes to it after the OBJ-SET.

## Impact

Only failure: GENTEMP.4 (symbol-package comparison after gentemp into
a user package). All other ANSI tests pass (17,567/17,568 = 99.994%).

The bug could affect any code that stores non-fixnum values via `aset`
through a function call, though most paths work due to different register
allocation patterns.

## Workarounds Tried (All Failed)

- **Cons-based symbol layout**: Broke CLOS method combination (12 regressions)
- **Inline aset in gentemp**: Also broke CLOS (12 regressions)
- **Helper function with non-last aset**: Fixed gentemp but broke CLOS
- **`%make-cl-symbol` with &rest pkg arg**: Segfault during init
- **Deep let nesting in gentemp**: Segfault (compiler nesting limit in loops)
