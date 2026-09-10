# Modus in the browser

A JavaScript interpreter for MVM bytecode that runs the hosted Modus CLI image
in a Web Worker (and under node).  No native code is involved: the image is
compiled to MVM bytecode by the normal build pipeline and stopped before the
translator; `mvm.js` executes that bytecode against a flat linear memory that
reproduces the hosted x86-64 machine (BSS block at `0x10000000`, machine stack,
Cheney semispace heap, x64 object and frame layouts, the x64 trap table).

## Files

| file | what |
|---|---|
| `../mvm/build-web.lisp` | x64 CLI wrapper's arch slots + web-only overrides; dumps `modus.mvmw` |
| `mvm.js` | the interpreter: memory, GC, calls, traps, syscalls, snapshots |
| `host-node.js`, `run-node.js` | node host (real fds) and CLI runner |
| `host-browser.js`, `worker.js`, `index.html`, `serve.js` | browser host (in-memory FS, stdin ring in a SharedArrayBuffer), the worker, the page, a COOP/COEP static server |
| `t/headless.js` | smoke test: drives the page in headless Chrome over the DevTools pipe |
| `modus.mvmw` | generated: bytecode + function table + constant pool (~13 MB) |
| `modus.core.gz` | generated: a core snapshot taken at CLI-TOPLEVEL (~1 MB) |

## Build and run

```sh
# 1. the bytecode module (~3 min; the JIT must be off, it needs native code)
MODUS_NO_JIT=1 sbcl --dynamic-space-size 8192 --script mvm/build-web.lisp

# 2. a core snapshot, so nobody waits for the boot (~8 min once)
node web/run-node.js --trace 1 --save-core web/modus.core.gz --eval '(print 1)' --quit

# 3. run under node
node web/run-node.js --core web/modus.core.gz --eval '(print (+ 1 2))' --quit
node web/run-node.js --core web/modus.core.gz            # REPL on stdin

# 4. run in a browser
node web/serve.js 18080     # then open http://localhost:18080/
node web/t/headless.js 'http://localhost:18080/?eval=(print%20(*%206%207))'
```

The page must be served cross-origin isolated (`serve.js` sets the headers)
because the worker blocks on stdin with `Atomics.wait`.

## What the interpreter does and does not do

- Every vreg lives in the frame (per-frame copies), so callee-saved registers
  and GC roots fall out of the stack layout; the frame keeps translate-x64's
  slot offsets so `&rest` argument copying and frame slots match.
- Words are 64-bit, carried as int32 pairs; `mul`, checked arithmetic, `div`
  and the crypto multiplies fall back to BigInt only when a value leaves the
  53-bit range.
- The collector is a Cheney copy with conservative, bitmap-validated roots
  (stack, BSS block, VR), like the native trampoline.
- A bad dereference (`car` of a fixnum) does what the native SIGSEGV stub
  does: longjmp through the armed `handler-case` with T, which is how it
  becomes a `TYPE-ERROR`.
- Not supported: threads (`%spawn-thread`), the runtime JIT, sockets, port I/O.
  `mvm-eval` falls back to the image's own bytecode interpreter, so
  user-defined functions run doubly interpreted and are slow (fib 20 ≈ 70 s).

## Debugging flags (`run-node.js`)

`--trace 1` (progress + GC log), `--trace 2` (every instruction; with
`--trace-from N --trace-count M --trace-regs` for a window), `--max-steps N`,
`--profile` (call counts + sampled self time per function), `--watch FN`
(print args at each entry), `--debug`.

## Web-only source overrides (`build-web.lisp`)

Same answers, cheaper on an interpreter: `eql` exits early for the non-numeric
cases, and hash tables use 4096 buckets instead of 256.  The i386 image was
tried first and abandoned: its 30-bit fixnums push interning and hashing
through the bignum tower.
