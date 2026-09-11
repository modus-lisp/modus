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

## GitHub Pages

The REPL runs as a static site. GitHub Pages cannot send the COOP/COEP headers
that `SharedArrayBuffer` (and thus the blocking stdin) needs, so
`coi-serviceworker.js` installs a service worker that re-serves same-origin
responses with those headers and reloads once; the second load is
cross-origin isolated. This is verified against a header-less server
(`node web/serve.js --no-coi`), which is exactly the Pages condition.

Deploy (pushes an orphan `gh-pages` branch of just the browser files plus the
gzipped `modus.mvmw.gz` ~1.3 MB and `modus.core.gz` ~0.9 MB):

```sh
web/deploy-pages.sh                 # origin gh-pages
```

Then in the repo: **Settings → Pages → Deploy from a branch → `gh-pages` /
(root)**. The REPL appears at `https://<user>.github.io/<repo>/`, boots from
the core in well under a second, and needs no server.

## Build and run

```sh
# 1. the bytecode module (~3 min; MODUS_NO_JIT keeps the x64 translator out)
MODUS_NO_JIT=1 sbcl --dynamic-space-size 8192 --script mvm/build-web.lisp

# 2. a core snapshot, so nobody waits for the boot (~8 min once)
node web/run-node.js --trace 1 --save-core web/modus.core.gz --eval '(print 1)' --quit

# 3. run under node
node web/run-node.js --core web/modus.core.gz --eval '(print (+ 1 2))' --quit
node web/run-node.js --core web/modus.core.gz            # REPL on stdin

# 4. run in a browser
node web/serve.js 18080     # then open http://localhost:18080/
node web/serve.js 18080 --no-coi   # emulate GitHub Pages (no COOP/COEP; the service worker supplies them)
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
- The image's runtime JIT seam is served by a web arm (`build-web.lisp`,
  appended last so it wins): an eval'd module's bytecode is copied into an
  exec page inside linear memory, `%jit-icache-flush` relocates its call and
  fn-addr operands in place (out-of-module callees through a table the Lisp
  side resolves), quoted constants are read through a GC-updated vector at
  run time, and `%jit-call` runs the page on this interpreter.  So
  user-defined functions run at interpreter speed instead of doubly
  interpreted (fib 20: 71 s → 1.2 s).  Function values are
  `(phys-index << 4) | 3`; the module's own bytecode is relocated the same
  way at load.
- Translation to JS.  A bytecode function called more than a threshold
  (`--compile-threshold`, default 20) is translated into JS: basic blocks
  become `switch` cases, V0–V8 live in JS locals and are spilled to the
  memory frame around calls, traps, collections and delegated
  instructions, V9–V15 and the frame slots stay in memory, so the GC and
  longjmp see exactly what they see for interpreted code.  A driver loop
  runs translated functions without JS recursion (a Lisp call never grows
  the JS stack), large functions are emitted as chunks small enough for
  V8 to optimize, and cold or untranslatable code stays interpreted; the
  two mix freely.  Steady state, node, `web/t/bench.lisp`:

  | | interpreted | translated |
  |---|---|---|
  | fib 27 | 712 ms | 98 ms |
  | tak 18 12 6 | 69 ms | 13 ms |
  | sort 100k fixnums with `#'<` | 1433 ms | 645 ms |
  | format 3000 pairs to a string | 3126 ms | 1118 ms |
  | 10k string keys into an EQUAL table | 7455 ms | 2485 ms |
  | read a 3000-element list from a string | 11035 ms | 3639 ms |

- I/O.  Files: node uses real descriptors; the browser has an in-memory
  filesystem rooted at `/home/web` — drop files on the page (or use the
  picker) and they appear at the next prompt, and every file the program
  writes is offered as a download in the bar.  Network: the image's
  socket layer (`socket`/`connect`/`write`/`read`/`close`, plus a private
  syscall 4242 for name resolution) is answered as one HTTP request per
  connection, so `(http-get "http://host/path")` works: node performs it
  with curl, the browser with `fetch` on the page thread (subject to CORS;
  same-origin and CORS-enabled hosts work, port 443 maps to https).  The
  C-string and I/O scratch buffers are moved into the BSS block
  (`*cli-arch-io-scratch-source*`) because the x64 defaults fall inside the
  interpreter's heap arena.
- Not supported: threads (`%spawn-thread`), listening sockets, port I/O.

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
