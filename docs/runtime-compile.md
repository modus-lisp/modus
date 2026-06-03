# Runtime compile — status and roadmap

Modus is self-hosting (`mvm/build-mvm.lisp` produces `/tmp/mvm`, a Modus
image whose runtime contains the compiler + x64 translator + ELF
emitter).  That binary has been the proof point of "the compiler runs
in-image" for as long as Modus has existed.

What it doesn't do is run the compiler *alongside* the standard CL
runtime in a single binary — which is what `(compile …)` / `(compile-
file …)` would need.

## Current state

### What works

- **`mvm/build-mvm.lisp`** builds `/tmp/mvm` (≈3 MB).  Reads a `.lisp`
  source from argv, emits an ELF.  Proves the compiler stack works
  inside a Modus image.
- **`mvm/build-with-compiler.lisp`** (new in this commit) concatenates
  the standard CL runtime (`prelude` + `gc` + `cl-*` + `ansi-bridge`)
  with the compiler stack and produces `/tmp/modus-rtc` (≈8.7 MB).
  The SBCL-side build completes cleanly.  This is **Tier 1** — the
  source can co-exist.

### Tier-2 status (clean)

After stripping the build-compiler-test.lisp adapter ("there is no
modus, there is only CL") — keeping only the defstruct stand-ins,
manual `*vreg-to-x64*` / `*condition-codes*` init, and a
`register-mvm-bootstrap-macros` noop — both stacks coexist for boot.

The boot sequence in `rtc-init` now runs through:

  init-symbol-table         ✓
  init-keyword-table        ✓
  %init-packages            ✓  (was segfaulting before the adapter
                                  surgery)
  %init-streams             ✓
  %init-reader              ✓
  %init-condition-types     ✓
  %init-method-combinations ✓
  %init-symbol-function-table ✓
  %init-sft-auto            ✓
  %init-sym-name-auto       ✓
  %init-runtime-macros      ✓
  init-compiler-macro-set   ✓
  %init-signal-handling     ✓
  %init-signal-symbols      ✓
  init-opcode-entries       ✓
  init-condition-codes-manual ✓
  init-vreg-to-x64-manual   ✓
  +explicit setq for *functions* / *label-counter* / *globals* /
  *constants* / *unresolved-calls* / etc. (compiler.lisp's defvars,
  whose init-thunks don't run on bare metal — known limitation #7)

### Tier-3 status (clean for non-quote forms)

After the fix to add compiler-internal globals to the implicit-special
list (`*FUNCTIONS*` / `*FUNCTION-TABLE*` / `*LABEL-COUNTER*` /
`*UNRESOLVED-CALLS*` / `*MACRO-TABLE*` / `*GLOBALS*` / `*CONSTANTS*`
/ `*LOOP-EXIT-LABEL*` / `*BLOCK-LABELS*` / `*TAGBODY-TAGS*` /
`*PENDING-FLET-IR*` / `*CURRENT-SOURCE-LOCATION*` / `*COMPILE-TRACE*`
/ `*FRAME-SLOTS*` / `*CURRENT-FN*` / `*CURRENT-FN-NAME*` /
`*OPCODE-TABLE*` / `*VREG-TO-X64*` / `*CONDITION-CODES*`, in
`compiler.lisp` ~line 184), the in-image compile pipeline runs.

The self-test in `kernel-main` proves these flows compile cleanly
end-to-end inside the runtime image:

  A — handler-case enters cleanly
  B — `(make-hash-table :test 'equal)` returns a real hash-table
  C — `(compute-name-hash "+")` returns a fixnum hash
  D — `(macroexpand-mvm '(+ 1 2))` returns the form unchanged
  E — `(format nil "TOPLEVEL-~D" (make-compiler-label))` produces a
      proper thunk name
  F — `(mvm-compile-function name nil (list 42))` compiles a body of
      literal 42
  G — `(mvm-compile-function name nil '((progn 1)))` compiles a
      progn form
  H — `(mvm-compile-function name nil '((setq x 1)))` compiles an
      implicit-global setq (the compiler's "WARN: implicit global
      setq" diagnostic prints to stdout)

The redefining-thunk-name diagnostic (`NOTE: redefining TOPLEVEL-1`)
also reaches stdout — proving the compiler's internal `*function-
table*` is correctly accumulating state across calls.

### What still faults

`compile-quote` on a symbol or cons literal from a runtime-built
form.  i.e. `(mvm-compile-function name nil '((quote sym)))` or
anything in the body that quotes a symbol or list throws.  The
chain bottoms out somewhere inside `(symbol-package value)` — at
SBCL build time the compile-quote path calls SYMBOL-PACKAGE on the
literal to compute the home-package hash; at runtime the symbol-
package implementation needs to walk a Modus symbol whose package
slot was populated by `%intern-symbol-pkg` at boot.  Probably one
of those slot reads dereferences something unexpected when the
symbol came from a runtime READ rather than SBCL build-time intern.

This blocks compiling anything with literal data — every (defun
foo (x) (list x)), (cons 1 2) with quoted arg, etc.

### Tier-4 — make a compiled function callable

Same plan as before: take the byte vector that `translate-mvm-to-
x64` produces, mprotect a page RWX, store the entry address as a
fn-pointer with the OR-3 low-nibble tag.

We have a clear path now — translate-mvm-to-x64 isn't exercised yet
but the rest is wired.

### Estimated remaining effort

| Tier | Status | Remaining |
|------|--------|-----------|
| 1 — source layers | ✓ done | — |
| 2 — clean co-init | ✓ done | — |
| 3 — `compile` glue | ~60% | resolve compile-quote-of-runtime-
                              symbol fault; ≈half day |
| 4 — exec dispatch | not started | ~1–2 days |
| 5 — FASL persistence | not started | ~1–2 days |

## What you'd need for each tier

### Tier 2 — compiler runtime state coexists with CL runtime

The collision shape is **two `*globals*` tables**.  Both the compiler
and the CL runtime use `defvar` for top-level mutable state, and the
build-mvm adapter overrides `init-globals-table` / `symbol-value` /
`set-symbol-value` to use a fixed memory slot (`#x600000`).  The CL
runtime expects the standard globals alist at `#x10000080`.

Resolution options:

1. **Strip the adapter's globals overrides** (drop `init-globals-
   table`, `symbol-value`, `set-symbol-value` from the adapter source
   string).  CL runtime's versions remain in effect for both stacks.
   This is what `build-with-compiler.lisp`'s commentary describes —
   but the current build still pulls them in via `*adapter-source-raw*`
   because last-defun-wins doesn't help when the CL versions are
   defined *earlier* in the source order.  Move the CL stack BEFORE
   the adapter section, or sed those defuns out of `*adapter-source-
   raw*` before concatenating.

2. **Give the compiler its own namespace** for compile-time globals
   (rename `*functions*` → `%cmp-functions*` etc.) — heavy lift, lots
   of `compiler.lisp` edits.

3. **Run the compiler in a fresh sub-environment** — initialize a
   secondary globals table at compile time, bind the CL globals
   reader to the secondary table during compile.  Probably the
   cleanest long-term answer.

### Tier 3 — `compile` glue drives the in-image pipeline

Once Tier 2 is clean:

```lisp
(defun %rtc-compile-lambda (lambda-form)
  (let ((module (mvm-compile-all
                  (list `(defun %rtc-tmp ,(cadr lambda-form)
                           ,@(cddr lambda-form))))))
    (let ((native (translate-mvm-to-x64
                    (compiled-module-bytecode module)
                    (compiled-module-function-table module))))
      ;; native is (cons byte-vector length)
      ...)))
```

Probably 100–200 lines covering defun / lambda / closure entry points.

### Tier 4 — make the compiled bytes callable

The byte vector lives in the Lisp heap (PROT_READ | PROT_WRITE).
Need PROT_EXEC to call into it.  Options:

- **mprotect the existing page** as RWX.  Single syscall3 (number 10,
  3 args).  Simplest, but we'd mark a heap page executable —
  acceptable on a Lisp OS, suspect on a multi-tenant system.

- **mmap a fresh JIT region** at boot, PROT_READ | PROT_WRITE |
  PROT_EXEC.  Needs syscall6 — currently no syscall6 trap.  Could
  add `%MMAP-EXEC-PAGE` opcode (`#x0505`) following the `%MMAP-
  SHARED-PAGE` (`#x0504`) pattern in `mvm/translate-x64.lisp` ~line
  458.  About 50 lines of asm-emit-bytes calls per arch.

Plus tagged-pointer hygiene: the returned address needs to land in
the FN-pointer slot of `*symbol-function-table*` with low nibble `0x3`
(per the OR-3 fn-pointer tagging from `mvm/translate-x64.lisp`
~line 2794).  Already handled by the existing translator emission;
just need to OR-3 the entry address before storing.

### Tier 5 — `compile-file` + a FASL format

If we want persistence: serialize `(name → native-bytes)` pairs to a
binary blob, plus a relocation table for any references to non-static
symbols.  `load` learns to recognise the blob magic and reverse the
process.

## Estimated effort

| Tier | Status | Estimate to clear |
|------|--------|-------------------|
| 1 — source layers | ✓ done | — |
| 2 — clean init | partial | 0.5–1 day to pick option 1 (sed the overrides), longer for cleaner approaches |
| 3 — `compile` glue | not started | ~1 day |
| 4 — exec dispatch | not started | ~1–2 days (depending on syscall6 vs mprotect choice) |
| 5 — FASL persistence | not started | ~1–2 days for in-memory blob format |

Cumulative: **3–6 days of focused work** to land a binary where
`(funcall (compile nil '(lambda () 42)))` returns 42 at runtime.

## How to use what's here now

```sh
# Standalone compiler (works today)
sbcl --script mvm/build-mvm.lisp    # → /tmp/mvm (~3 MB)

# Compiler + CL runtime layered (builds, doesn't boot cleanly yet)
sbcl --script mvm/build-with-compiler.lisp    # → /tmp/modus-rtc (~8.7 MB)
```

Once Tier 2 is cleared, the build script's kernel-main does a Tier 3
self-test that calls `mvm-compile-all` against a one-form program and
prints `COMP-OK` / `COMP-FAIL`.  The infrastructure is ready; the init
collision is the next thing to chase.
