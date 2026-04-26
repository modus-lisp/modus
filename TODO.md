# Modus TODO

## ANSI Test Suite Status

**236 files, ~7200 tests, 5 failures, 5.6s runtime**

### Remaining 5 Failures (hard compiler walls)

| Test | Root Cause | Fix |
|------|-----------|-----|
| MAPCAR.3 | Mutable closure — `(incf n)` inside `#'(lambda ...)` | Closure-over-box: mutable captured vars need heap-allocated box cells. Lambda captures pointer to box; `incf`/`setq` modifies box contents. Needs new IR for box-alloc, box-ref, box-set. |
| REMF.6 | `macrolet` + `expand-in-current-env` | Proper macrolet environment threading. Low priority — niche CL feature. |
| MV-BIND.7 | `declare special` + `symbol-value` | Dynamic binding stack (bind/unbind around let), `symbol-value` checks dynamic bindings before globals. |
| MV-LIST.4 | `(values (values 'a 'b 'c 'd 'e))` — nested values | MV count propagation: inner values sets count=5, outer should reset to 1. Needs careful analysis of count flow through compile-values. |
| INTEGER-LENGTH.1 | `(ash 1 100)` — fixnum overflow | Bignum support. Infrastructure exists (`%make-bignum`, subtag #x30), `bignum-ash` works, but adding bignum functions shifts bytecode layout and causes regressions. Blocked on bytecode stability. |

## Compiler Fragility Issues

Found by deep investigation of register clobbering, stack smashing, and layout sensitivity.

### FIXED

- **Heap/MV-storage overlap**: Heap started at mmap+256 but MV-VALUES extends to mmap+312. First 56 bytes of heap overlapped with MV slots 4-19. **Fixed**: heap starts at mmap+512.
- **`%unresolved-fn` lookup**: String key in integer-keyed hash table. All 4723 unresolved calls jumped to bytecode offset 0. **Fixed**: uses `compute-name-hash`.
- **`decode-instruction` OOB infinite loop**: Returned same position on out-of-bounds → `scan-branch-targets` looped forever. **Fixed**: returns pos+1.
- **`compile-values` register clobber**: Let-based approach corrupted primary value for function call results. **Fixed**: push/pop preserves values on real stack.
- **Nested boolean expressions in loops**: `(or (and ...) (and ...))` inside loop bodies clobbers frame variables. **Workaround**: extract logic into helper functions. Root cause still open.

### OPEN — Bytecode Layout Fragility

**Symptom**: Adding or removing functions causes unrelated tests to break.

**Root cause** (partially understood):
1. **Funcall dispatch reads tag bits of raw fn-addrs.** PARTIALLY FIXED:
   the translator now NOP-aligns function entry points away from low
   nibbles 1 (cons) and 9 (object).  Both nibbles caused
   compile-funcall's closure / native-MVM-sym detection to dereference
   `[fn-addr - 9]` looking for a heap-object header and SIGSEGV when
   the bytes there happened to look like a real subtag.
   - Tried full 16-byte alignment as a stronger fix; regressed via
     more layout shifting elsewhere — fragility runs deeper than just
     the funcall path.
2. **Global flat namespace, "last defun wins."** Adding functions can
   shadow existing ones via name-hash collision.  Not yet addressed.
3. **Bytecode-size shift triggers other layout-sensitive code paths.**
   Around 4 tests (12257 typep-of-lambda, 12261, 12285, 14253) flip
   when bytecode layout shifts even with no semantic change (pure
   `:nop` injection).  Root cause not yet found — may be GC root scan
   reading incorrect frame data, or setjmp frame layout, or some
   address ending up in another bit-pattern collision we haven't
   identified yet.

**Fixes attempted**:
- ✓ NOP-align fn entry away from nibbles 1, 9 (in
  `mvm/translate-x64.lisp`'s post-function alignment pass)
- ✗ Full 16-byte alignment (regressed via bigger layout shift)
- ✗ caller-save in compile-compare-2's slow path (regressed CLOS
  tests via per-comparison size growth)

**Fixes still needed**:
- Funcall dispatch via runtime range check (`if addr in [code_base,
  code_end) → direct call; else → object dispatch`).  Eliminates
  bit-pattern dependence entirely.  Requires storing code base/end
  in fixed memory slots at boot.
- Per-module namespaces (compile each source file separately, link)
- Warn on function name hash collisions
- Make `check-arith-nesting` non-fatal (skip single form, not whole
  function)
- Two-pass bytecode emission stable under function count changes
- Layout-flip fuzzer: build N variants of the binary with different
  NOP padding, run suite, surface tests that flip across variants —
  those are the layout-sensitive ones to fix.

### OPEN — `bignump` Tag Extraction Bug

`compile-bignump` (compiler.lisp) uses `(AND value #x0F)` but `#x0F` loads as tagged `0x1E`. Mask is wrong → `bignump` always returns false. Other predicates (`stringp`, `symbolp`) use `compile-object-subtype-p` with `OBJ-TAG`/`OBJ-SUBTAG` opcodes.

**Fix**: `(compile-object-subtype-p arg env dest +subtag-bignum+)` — one line.

### OPEN — Bytecode Error-Recovery Offset Corruption

In `mvm-compile-all` Phase 3 (~line 5176), if a function fails during bytecode emission, buffer position is restored and function removed, but subsequent functions' offsets in `*functions*` become stale. CALLs to later functions use wrong offsets.

**Fix**: Abort build on emission error, or re-compute all offsets after removal.

### OPEN — `set-mv-count` Hardcoded Address

x64 translator hardcodes `0x10000090` instead of deriving from `+mv-count-addr+`. If constant changes, compiler and translator disagree.

### OPEN — Register Clobber in Deeply Nested Expressions

`(if (pred x) (if flag (aset ...) (progn (aset ...) (setq ...))) ...)` inside a `loop` body silently produces wrong results in large binaries. Works in small standalone binaries. **Workaround**: extract per-iteration logic into helper functions. **Root cause**: likely `alloc-temp-reg`/`free-temp-reg` counter not properly scoping across nested `compile-form` calls.

## Architecture Notes

### Memory Layout (Linux x64, mmap at hint 0x10000000)

```
Offset    Size   Contents
0x000     128    argc/argv storage
0x080     8      Globals alist head pointer
0x088     8      Symbol table (intern hash table)
0x090     8      MV count (tagged fixnum)
0x098     160    MV values (20 slots × 8 bytes)
0x138     200    Reserved
0x200     ...    Heap start (R12 alloc pointer)
...       896MB  Heap (R14 alloc limit)
```

### MV Count Protocol (Genera-style)

Every function that does NOT end with `(values ...)` emits `set-mv-count 1` before its epilogue. Functions ending with `(values ...)` set count=N via `compile-values`. `%mv-to-list` reads count from MV-COUNT address.

`tail-form-is-values-p` walks through progn/let/let*/if to detect values in tail position.

## Next Steps

### High Value
1. **Fix `bignump`** — one-line fix, unblocks bignum support
2. **Fix bytecode error-recovery** — prevents silent corruption
3. **Add ANSI test chapters** — printer, streams, packages, conditions, etc. Infrastructure ready (chunked fork, alarm). 236 → 400+ files.
4. **Push to GitHub** — 75+ unpushed commits

### Medium Value
5. **Mutable closures** — box-cell allocation for captured mutable variables
6. **Dynamic (special) bindings** — dynamic binding stack
7. **Bignum arithmetic** — infrastructure ready, blocked on layout fragility

### Foundational
8. **Per-module compilation** — eliminate "last defun wins" fragility
9. **Register allocation audit** — find root cause of nested-expression clobber
10. **Runtime memory guards** — bounds-check heap, stack, MV storage
