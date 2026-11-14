# REPL code as threaded code on the 68000

## Why

On a Mac Classic, a function defined at the REPL is MVM bytecode run by the
bytecode interpreter (`%mvm-interpret-1`), which is itself image code running
as threaded tokens on a 7.8 MHz 68000: measured ~100,000 cycles per bytecode
instruction, `(fib 15)` 19 minutes.  Image functions run as threaded code
directly: one token (one or three bytes) per MVM instruction, dispatched by
`NEXT` to a native handler with its registers baked in.  The same compiler
produced both; only the last step differs.  This puts REPL code through that
last step, at run time.

## The pieces

1. **The translator in the image.**  `translate-68k.lisp`'s threaded tier
   (classify, walk, stream emission) and its per-instruction emitter
   (`m68k-translate-insn`, for handlers) are baked into 68k images built with
   `MODUS_68K_JIT=1`, as the x64 JIT bakes `translate-x64.lisp`.  On a paged
   Mac image they are cold code like any other: on disk until a DEFUN is
   translated.

2. **The image's shapes, as data.**  The build writes every shape key the
   image's own translation numbered (2,954) with its token index into the
   image: a vector of encoded keys, sorted, binary-searched at run time.  A
   runtime instruction whose shape the image has costs nothing new: its token
   is the image's.

3. **New shapes.**  Shape keys bake register operands and most immediates
   (`(OBJ-REF 5 VFP 2)`), so REPL code will sometimes need one the image never
   made.  Its handler is emitted at run time by the same emitter into the
   **runtime code area** and reached through one extra built-in shape,
   **INDIRECT**: the token, then the handler's 32-bit address; the handler is
   `MOVE.L (A5)+,A0` / `JMP (A0)` (byte reads on the 68000).  The dispatch
   table never changes.  Generated handlers are cached by key.

4. **The runtime code area.**  A fixed, non-collected region of the map (the
   Mac's `:classic-os` layout; an mmap on the hosted 68k) holding stubs,
   streams and generated handlers, bump-allocated.  Redefinition leaks the old
   code (it is small; a REPL session makes little).

5. **Linking.**  A runtime function is the unpaged cold-function shape: a stub
   `MOVEA.L A5,A1` / `LEA stream(PC),A5` / `JMP next`, 16-byte aligned (its
   tagged address is the function object), then its stream.  A CALL inside
   the module names the callee's stub; a CALL to a runtime-table name is
   resolved to that function's entry now (an image function's stub address).
   The image exports the few addresses runtime code needs: `NEXT`, the 68000
   multiply/divide helpers, the GC trampoline.

6. **No heap pointers in code.**  The collector moves objects and does not
   scan streams.  LI-CONST (a quoted constant, an index into
   `*E2-CONST-POOL*`) becomes a generated handler that reads the pool at run
   time; nothing else in a runtime module names a heap object.

7. **The seam.**  mvm-eval's JIT seam (`%jit-translate-page` and the
   hot-function promotion that calls it) gets a 68k arm.  On the Mac every
   DEFUN is translated when it is defined (compiling already takes ~20 s;
   translating is a fraction of that); a module that will not translate stays
   interpreted, as on x64.

## Development

Against a hosted, threaded 68000 CLI (`MODUS_68K_THREADED=1 MODUS_68K_CPU=68000
mvm/build-68k-cli.lisp`) under `qemu-m68k-static`: seconds a test, the hosted
ladder and alexandria/cl-ppcre for regressions.  Then the Mac.

## Milestones

1. Bake the translator; translate a module with existing shapes only (fail ->
   interpret); `(defun sq ...)` runs threaded on the hosted 68k.  Shape
   coverage of typical REPL code measured.
2. Generated handlers and INDIRECT; LI-CONST.
3. The seam: eager on DEFUN, forwarding, GC safety checks.
4. The Mac: the runtime code area in `:classic-os`; MAME timings.
5. Gates: hosted 68k ladder and libraries; the x64 acceptance gate for the
   shared files.

## Status (2026-11-13)

Runs on the Mac Classic layout launched from Mac OS (`classicos`, 4 MB), in
MAME's 68000: `(fib 15)` 4 s against ~19 minutes interpreted, `(fib 20)` 17 s.
And on a 1 MB Mac Plus (`plus1mjit`: the image 16 KB lower, a 16 KB code area
from the page cache): `(fib 15)` 6 s, `(fib 20)` 18 s.  There a translation
costs minutes (the compiler and the JIT page through a 104 KB cache), so DEFUN
does not translate and `(compile 'fib)` does (97 s): `%JIT-EAGER-ALL`, the
seam cl-eval's COMPILE already called for the x64 JIT, now gets the name.
Hosted 68k: `t10` (24 cases against the interpreter, constants across a
collection) passes, `(fib 20)` 27.8 s -> 18 ms.  Not done: handlers generated
at run time (INDIRECT) -- a missing shape refuses the module instead; late
binding for redefinitions; translating only hot functions, which would take
the translation cost (up to a minute a DEFUN on a Classic) off DEFUN.  The
shape index is built in the core run and saved with the core.

## Found on the way

- **A RETURN-FROM that did not return.**  In the first `%T68-TRANSLATE`, a
  `(return-from %t68-translate nil)` in the lambda handed to `%T68-WALK`
  returned from the lambda only (interpreted, hosted 68k), so a missing shape
  was skipped instead of refused and `(setf (aref ...))`'s token vanished.
  The same shape in small tests (`/tmp`-style probes: a lambda in a LET*
  initform, inside DOLIST, from a DEFUN) returns correctly; the failing code
  also bound a variable named LABELS.  Not yet reduced; the translator now
  has no non-local exits and no such variable.
- **The Mac's seams were bound in the wrong package.**  The Mac build gives
  the JIT its code area and its "only from a saved core" rule by rewriting
  `jit-68k.lisp`'s text into `*CLI-ARCH-LATE-SOURCE*` before
  `build-cli-common.lisp` loads.  That file reads in CL-USER (its one
  `IN-PACKAGE` is inside a string), and the Mac block bound
  `MODUS.MVM::*CLI-ARCH-LATE-SOURCE*`, a variable nothing read.  The image got
  the hosted defaults: E2SMOKE's DEFUN in the core run was translated, and the
  JIT went to mmap a code area on a Mac.  Basilisk then died in the ROM's
  `_Write` with the File Manager queue's tail at -1, on both 68040 ROMs, and
  only when the build had the JIT code and the shape table, which looked like
  a layout fault.  Serial probes found it: `%T68-ALLOWED-P` answered T where a
  function with its intended body answered NIL, and `MODUS_DDC_DUMP_SOURCE`
  showed the default bodies in the blob.
- **What the hosted 68k could not show.**  Two defects passed every hosted
  test because qemu-user and Basilisk both emulate a 68040 running an unpaged
  or paged image they are not: a CALL operand in a *paged* image's stream is
  an absolute address (its streams move; `M68K-EMIT-FETCH-TARGET`), so the
  JIT's relative operand sent every Mac call into the weeds -- `(sq 7)` ran,
  `(hanoi 10)` hung; and `%T68-LOOKUP` read a packed table entry's index as
  one word, an address error on a real 68000 at an odd entry.  The JIT now
  takes the operand form from CAR's stub (paged or not) and reads bytes.
- **The table indexes what runtime code asks for, not every shape.**  The
  Mac's small image makes fewer shapes than the hosted CLI (2,979 against
  4,532), so `(defun fib ...)` there needed `(:FUSED (48 4 6) 67 2)` and was
  refused; and indexing all of them was 40 KB, which a 1 MB Mac has no room
  for.  `mvm/68k-jit-repl-shapes.txt` is a census (`*T68-KEY-LOG*`) of the 795
  keys the JIT was asked for while translating alexandria and ~35 REPL
  functions on the hosted 68k.  An image indexes those, or an equivalent it
  has, and gets a handler for each it lacks: on the 1 MB image 798 entries in
  8 KB and 79 handlers.  Entry offsets are 2 bytes.
