# REPL code as threaded code on the 68000 (design, in progress)

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
