# GATE-RESULT #210 rung 1 — hosted x64 Modus image emits a runnable AArch64 ELF

Branch `xarch-rung1`, based on `main` @ `e915944`.

**Claim under test.** A *hosted x64* Modus image, with the AArch64 translator
compiled from source baked into that image, emits a Linux/AArch64 ELF that runs.
No SBCL anywhere in the foreign emit.

This replaces `mvm/build-fixpoint.lisp`'s approach (one multi-arch binary with an
`*override-fns*` runtime dispatch, broken since the image outgrew its layout)
with the expected shape: **boot a Modus image → compile the foreign
architecture's translator from embedded source → emit a foreign image.**

---

## RESULT

### PASS — an x64 Modus image emitted a Linux/AArch64 ELF that runs.

```
$ ./modus --compile-aarch64 tests/xarch/rung1.lisp rung1-aa64
aa64-cfg fn-align=120 linux=T stackalign=T vregmap=24 midpoint=469762048
a64-roundtrip n=400 cap0=1048576 endpos=400 endcap=1048576 bytelen=1600 first-bad=-1
a64-roundtrip n=3000 cap0=1048576 endpos=3000 endcap=1048576 bytelen=12000 first-bad=-1
modus: wrote 10473 bytes to rung1-aa64 (linux-aarch64)

$ file rung1-aa64
rung1-aa64: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), statically linked, not stripped

$ qemu-aarch64-static ./rung1-aa64
MODUS XARCH RUNG1 OK
$ echo $?
42
```

`binfmt_misc` is not registered on this host, so `qemu-aarch64-static` is invoked
explicitly. Exit 42 is the *expected* value: the program calls `(sys-exit 21)`
and AArch64 does not untag the status — see "Second finding" below. The stdout
line is the arch-independent success signal; the identical x64 build of the same
source prints the same line and exits 21.

### Bonus: the executed code is a byte-exact fixpoint with the SBCL reference

Compiling the same program SBCL-side (`build-image :target :linux-aarch64`,
same knob settings) and comparing:

```
$ python3 -c "...a[0x78:0x5e0]==b[0x78:0x5e0]..."
boot+kernel-main region (0x78..0x5e0) identical: True
first diff after 0x78: 1529   (0x5e0 = start of INIT-ALL-GLOBALS)
```

The boot stub and `KERNEL-MAIN` — everything rung 1 actually executes — are
**byte-identical** to what SBCL emits. `KERNEL-MAIN` lands at the same address
(`0x400200`) with the same length (992).

The only divergence is `INIT-ALL-GLOBALS`: 4892 bytes in-image vs 3404 SBCL-side,
from the same 22 init thunks (both builds report `init-all-globals: 22 init
thunks`). It is dead code here — the boot stub branches straight to
`KERNEL-MAIN`, which never calls it — so it does not affect the result. Closing
that codegen gap is rung-2 work, and it is the natural next fixpoint target.

---

## Reproduction

Build the image (~13 min):

```
sbcl --dynamic-space-size 16384 --script mvm/build-modus-selfhost.lisp
```

The probe program is `tests/xarch/rung1.lisp`. It is deliberately trivial: no
heap allocation and no runtime library, using only two compiler primitives that
every Linux target implements — `WRITE-CHAR-SERIAL` (trap `#x0300` → `write(2)`
on fd 1) and `SYS-EXIT` (trap `#x0500` → `exit_group(2)`). What it proves is the
*pipeline* (compile → MVM bytecode → native translate → ELF wrap), not the
runtime.

```
$ ./modus --compile-aarch64 tests/xarch/rung1.lisp rung1-aa64   # cross-arch emit
$ qemu-aarch64-static ./rung1-aa64                              # -> MODUS XARCH RUNG1 OK, exit 42
$ ./modus --compile          tests/xarch/rung1.lisp rung1-x64   # same image, x64 emit
$ ./rung1-x64                                                   # -> MODUS XARCH RUNG1 OK, exit 21
```

The `aa64-cfg` and `a64-roundtrip` lines are intentional diagnostics kept in the
emit path: `aa64-cfg` prints the globals whose `defvar`/`defparameter` init
thunks do **not** run at boot (a `NIL` there is the signature of a missed
co-init), and `a64-roundtrip` emits known words below and above the code-array
capacity and verifies they read back (`first-bad=-1` = sound).

---

## Why x64 → aarch64 (and not x64 → i386)

`+FRAME-SLOT-BASE+` is `-96` in `translate-x64.lisp` and `-68` in
`translate-i386.lisp`, and **both are bare-named**, so an i386+x64 co-bake
collides in the flat image namespace. AArch64 prefixes its own
(`+A64-FRAME-SLOT-BASE+`).

A name-by-name comparison of every toplevel `DEFUN`/`DEFMACRO`/`DEFSTRUCT` in
`translate-aarch64.lisp` against `translate-x64.lisp`, `x64-asm.lisp`,
`boot-linux-x64.lisp` and the rest of the baked image found **zero collisions** —
the x64 translator uses `TRANSLATE-INSTRUCTION`/`TRANSLATE-FUNCTION` where the
AArch64 one uses `TRANSLATE-MVM-INSN`/`TRANSLATE-MVM-FUNCTION`. The co-bake
needed no renaming at all.

---

## THE ROOT-CAUSE FINDING: `LET` of a special does not dynamically bind in-image

This is the substantive result of rung 1 and the reason the first two attempts
produced a SIGILL ELF.

`cross.lisp`'s `translate-module-to-native` passes the unified `a64-buffer` to
the translator through a dynamic binding:

```lisp
(let ((translator (target-translate-fn target))
      (modus.mvm::*aarch64-translate-into-buf* into-buf))
  … (funcall translator bytecode table) …)
```

**In-image that binding never reaches the callee.** Demonstrated directly in the
built image:

```
$ ./modus --eval '(progn (setq *aarch64-translate-into-buf* nil)
                         (defun %rd () *aarch64-translate-into-buf*)
                         (print (list :inside (let ((*aarch64-translate-into-buf* 42)) (%rd))
                                      :after *aarch64-translate-into-buf*)))' --quit
(:INSIDE NIL :AFTER NIL)
```

`:INSIDE` should be `42`. The image's compiler treats a `LET` of a special as an
ordinary lexical binding, so anything *called* from the body still sees the
global.

Consequence: `translate-mvm-to-aarch64` reads `NIL`, concludes it is not
appending, allocates a **fresh** buffer (`(or *aarch64-translate-into-buf*
(make-a64-buffer))`, translate-aarch64.lisp:5108), translates the whole module
into it *correctly*, and returns it — while `assemble-kernel-image` goes on to
slice the **unified** buffer, which only ever received the boot preamble. The
emitted ELF got a byte-perfect 94-instruction boot stub, a correct symbol table
with correct function names, and **zero bytes of function code**; execution ran
off the end of the preamble into the constant pool and died with SIGILL.

### The tell that identified it

Not "code went missing" — that is too vague to act on. The decisive observation
was that `KERNEL-MAIN`'s recorded native offset was **8**:

| | translation starts at buffer index | align NOPs emitted | KERNEL-MAIN native offset |
|---|---|---|---|
| unified buffer (correct) | 95 | 3 | **12** |
| fresh buffer (observed) | 0 | 2 | **8** |

With `*aarch64-fn-align-offset*` = 120 the pad loop runs while
`(idx*4 + 120) mod 16 ≠ 0`. From index 0: `120 mod 16 = 8` → pad; `124 mod 16 =
12` → pad; `128 mod 16 = 0` → stop ⇒ offset 8, two NOPs. From index 95: `500,
504, 508` → three pads, stop at 512 ⇒ offset 12. Offset 8 is reachable **only**
from a fresh-buffer translation, which is what named the bug.

(This is the same class CLAUDE.md already warns about for the handler-bind
work — "do NOT dynamically rebind the special". It is now confirmed to apply to
`LET` of a special generally, not just that one site.)

### Fix

Applied to the **baked copy only**, as a source rewrite inside
`mvm/build-modus-selfhost.lisp` — `SETQ` the global instead of `LET`-binding it.
Every call site assigns it unconditionally (`nil` for non-AArch64 targets, which
never read it), so dropping the restore is safe. Repairing `cross.lisp` itself
would be the real fix, but `cross.lisp` is on the x64 ANSI-gate path and this
rung does not need to touch it.

### Known sibling, deferred (rung 2)

`translate-aarch64.lisp:5111` has the identical construct:

```lisp
(let* ((buf (or *aarch64-translate-into-buf* (make-a64-buffer)))
       (translated-start-idx (a64-buffer-position buf))
       (*aarch64-translated-start-idx* translated-start-idx)   ; ← also never binds
       …)
```

so the seven `(or *aarch64-translated-start-idx* 0)` readers compute fn-addr
patch byte positions relative to 0 instead of the true start. **Not exercised by
rung 1** — measured, not assumed:

```
PATCHCOUNTS fn-addr=0 li-const=0 inmodule=0 call-relocs=0
```

Rung 1's program emits zero patches of every kind. A rung-2 payload with real
function calls will hit this and must fix it (same `SETQ` treatment).

---

## Second finding: AArch64 does not untag the `SYS-EXIT` status

`translate-x64.lisp:648` and `translate-i386.lisp:2041` both `SAR` the tagged
fixnum by 1 before the exit syscall. `translate-aarch64.lisp:1907` does **not** —
it passes the tagged word straight to `exit_group(2)`:

```lisp
((and *aarch64-linux-mode* (= code #x0500))
 ;; Linux sys-exit: V0 (x0) = exit status.  exit_group(2) = 94.
 (a64-load-imm64 buf +a64-x8+ 94)
 (a64-emit buf #xD4000001))      ; SVC #0     ← no ASR x0, x0, #1
```

So every Linux/AArch64 Modus image exits with **twice** its intended status:
`(sys-exit 21)` yields 21 on x64/i386 and 42 on aarch64. Reproduced on both the
SBCL-side reference emit and the in-image emit, so it is a translator bug, not an
artifact of this work.

**Deliberately NOT fixed on this branch.** The one-line repair (`a64-asr-imm buf
+a64-x0+ +a64-x0+ 1` before the `MOVZ x8,#94`) changes shared aarch64 behaviour —
including what the aarch64 ANSI gate's exit status means, since that encodes the
failure count — and gating it needs an aarch64 run that is out of rung 1's scope.
Flagged for a decision.

---

## Changes

| File | Change | Shared? |
|---|---|---|
| `mvm/build-modus-selfhost.lisp` | bake `translate-aarch64` + the linux-aarch64 boot descriptor; AArch64 translator co-init; `*target-aarch64*` registration; `--compile-aarch64`; the `cross.lisp` `:into-buf` source rewrite | no (build script) |
| `mvm/translate-aarch64.lisp` | two stale-local-across-GC hardenings (below) | yes, but aarch64-only |
| `tests/xarch/rung1.lisp` | new probe program | new file |

### On the two `translate-aarch64.lisp` edits

`a64-buffer-to-bytes` and `a64-emit`'s array-grow path each bound a local to the
buffer's `code` slot and *then* allocated, which is a stale-local-across-GC
hazard in-image (a copying collector moves the array; the local keeps pointing
into from-space). Both were re-ordered to read the slot after the allocation.

These are **defensive hardening, not the root cause** — they were made while
chasing the bug and did not change the outcome. They are kept because they are
genuine latent hazards of the documented class. Risk is bounded by measurement:

```
$ cmp tmp/bisect-baseline tmp/rung1-aa64-SBCLREF     # pre-fix vs post-fix SBCL emit
$ echo $?
0
```

**Byte-identical SBCL output**, so every existing aarch64 build is unaffected.
No x64 ANSI gate was run because no file on the x64 gate path changed:
`translate-aarch64.lisp` is loaded by `load-mvm.lisp` for all builds but none of
its functions are *called* by an x64 target, and `cross.lisp`/`compiler.lisp`/
`target.lisp` are untouched. The x64 no-regression check below is the direct
evidence.

---

## No x64 regression

The selfhost image's existing `--compile` path must still produce a working x64
binary. Compiled the same program with the unmodified `main` image (BASE) and
with this branch's image:

```
$ ./modus-BASE   --compile tests/xarch/rung1.lisp rung1-x64-BASE     # main @ e915944
modus: wrote 13561 bytes to rung1-x64-BASE
$ ./modus-XARCH3 --compile tests/xarch/rung1.lisp rung1-x64-XARCH3   # this branch
modus: wrote 13561 bytes to rung1-x64-XARCH3

$ cmp rung1-x64-BASE rung1-x64-XARCH3 && echo BYTE-IDENTICAL
BYTE-IDENTICAL

$ sha256sum rung1-x64-BASE rung1-x64-XARCH3
6e8ddfa1dd25e5b0d8253b037463129bd918b958d940a4bc9eb1d611d0297e70  rung1-x64-BASE
6e8ddfa1dd25e5b0d8253b037463129bd918b958d940a4bc9eb1d611d0297e70  rung1-x64-XARCH3

$ ./rung1-x64-XARCH3
MODUS XARCH RUNG1 OK
$ echo $?
21
```

Byte-identical x64 output from a from-scratch BASE build of unmodified `main`.
The image itself also still boots normally (`./modus --version` ->
`Modus 0.1 (hosted CLI; self-hosted MVM, mvm-eval)`).

---

## Caveat on scope

The branch HEAD was rebuilt from scratch and re-verified; the emitted AArch64
ELF is **byte-identical** to the earlier run (10473 bytes), still prints
`MODUS XARCH RUNG1 OK` and exits 42, and the x64 `--compile` output is still
byte-identical to BASE. So the deliverable reproduces from the committed tree.

The five commits are staged by recipe stage for review. Intermediate commits
were **not** individually built (each `build-modus-selfhost` run is ~13 min);
they were checked only for structural soundness (whole-file paren balance and
balance of every generated source string). Treat stages 1-4 as a reviewable
decomposition of the verified endpoint, not as independently gated states.
