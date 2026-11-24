'use strict';
// worker.js — runs the MVM interpreter off the main thread.
//
// Messages in:  { type: 'start', mvmw: ArrayBuffer, core: ArrayBuffer|null,
//                 stdin/http/inbox/query/gui: SharedArrayBuffer,
//                 argv: [...], env: [...],
//                 files: [{ path, bytes }] }
// Messages out: { type: 'stdout', bytes }, { type: 'log', text },
//               { type: 'waiting' }, { type: 'exit', code }, { type: 'fault', text },
//               { type: 'http', url, method, headers, body }, { type: 'file', path, bytes },
//               { type: 'query-result', id, op, bytes }

//               mvmEnv: { wasm: '1'|'0', MVM_*: ... } (optional; from the page URL)
importScripts('host-browser.js', 'warmset.js');
let X = null;

// Does this engine run what wasm32.js emits?  It needs the legacy exception
// handling (try/catch on a tag, plus the WebAssembly.Tag/Exception JS API the
// VM throws a longjmp with) and tail calls (return_call_indirect).  Validate a
// tiny module using exactly those encodings.
function wasmSupported() {
  try {
    if (typeof WebAssembly === 'undefined' || !WebAssembly.Tag || !WebAssembly.Exception) return false;
    return WebAssembly.validate(new Uint8Array([
      0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
      0x01, 0x0a, 0x02, 0x60, 0x00, 0x01, 0x7f, 0x60, 0x02, 0x7f, 0x7f, 0x00,  // types: []->[i32], [i32 i32]->[]
      0x03, 0x03, 0x02, 0x00, 0x00,                                          // two functions of type 0
      0x04, 0x04, 0x01, 0x70, 0x00, 0x01,                                    // funcref table, min 1
      0x0d, 0x03, 0x01, 0x00, 0x01,                                          // tag 0 : type 1
      0x0a, 0x16, 0x02,                                                      // code
      0x0c, 0x00, 0x06, 0x40, 0x01, 0x07, 0x00, 0x1a, 0x1a, 0x0b, 0x41, 0x00, 0x0b, //   try nop catch 0 drop drop end; i32.const 0
      0x07, 0x00, 0x41, 0x00, 0x13, 0x00, 0x00, 0x0b,                        //   i32.const 0; return_call_indirect 0 0
    ]));
  } catch (e) { return false; }
}

// wasm32.js is a CommonJS-style file (node require()s it); evaluate it in a
// function scope of its own so its top-level names stay out of mvm32.js's.
async function loadWasm32() {
  const r = await fetch('wasm32.js');
  if (!r.ok) throw new Error('wasm32.js: HTTP ' + r.status);
  const module = { exports: {} };
  new Function('module', 'exports', (await r.text()) + '\n//# sourceURL=wasm32.js')(module, module.exports);
  return module.exports;
}

self.onmessage = async (ev) => {
  const msg = ev.data;
  if (msg.type !== 'start') return;
  const post = (m, t) => self.postMessage(m, t);
  try {
    const t0 = performance.now();
    // The module header's word size picks the machine (as run-node.js): 4 = the
    // i386-target module on mvm32.js, else mvm.js.  Only one of them is loaded:
    // both are classic scripts declaring the same top-level names.
    const bytes = new Uint8Array(msg.mvmw);
    if (bytes[8] === 4) {
      // mvm32.js reads its MVM_* settings from self.MVM_ENV (process.env under
      // node).  Wasm translation is on by default where the engine supports
      // it; ?wasm=0 turns it off, ?wasm=1 insists (still off if unsupported).
      const env = Object.assign({}, msg.mvmEnv || {});
      const want = env.wasm; delete env.wasm;
      let wasm = want === '0' ? false : wasmSupported();
      if (want === '1' && !wasm) post({ type: 'log', text: '[wasm: this engine lacks exception handling or tail calls; using the JS translator]' });
      // Both read MVM_ENV when they load (wasm32.js's settings are top-level
      // constants), so it is in place before either is evaluated.
      self.MVM_ENV = env;
      self.MVM_LOG = (s) => post({ type: 'log', text: String(s).replace(/\n$/, '') });
      if (wasm) {
        env.MVM_WASM = '1';
        try { self.MVM_WASM32 = await loadWasm32(); }
        catch (e) { wasm = false; delete env.MVM_WASM; post({ type: 'log', text: '[wasm: ' + (e && e.message || e) + '; using the JS translator]' }); }
      }
      importScripts('mvm32.js');
      post({ type: 'log', text: wasm ? '[modus: translating to WebAssembly]' : '[modus: translating to JavaScript]' });
    } else importScripts('mvm.js');
    X = self.MVM_EXPORTS;
    const mod = X.loadModule(bytes);
    const host = new BrowserHost(msg.stdin, post, msg.http, msg.inbox, msg.query, msg.gui);
    for (const f of msg.files || []) host.addFile(f.path, new Uint8Array(f.bytes));
    const vm = new X.MVM(mod, host, { argv: msg.argv, env: msg.env, semispace: msg.semispace, trace: msg.trace | 0 });
    host.vm = vm;   // for the introspection RPC serviced while idle in read
    // pre-translate the small introspection helpers so the first completion is
    // quick (a fresh core has an empty translation cache); the builtins they
    // call translate on their own after a few uses.
    for (const n of ['%WEB-RPC', '%WEB-MATCH', '%WEB-DESCRIBE', '%WEB-PREFIX-P', '%WEB-SUBSTR-P', '%WEB-PUT-STR', '%WEB-PUT-BYTE', '%WEB-CSTR', '%WEB-TRUNC']) vm.forceCompile(n);
    vm.forceCompileMatching(/^%WEB-MATCH\$\$/);
    let resumed = false;
    if (msg.core) {
      vm.restore(X.MVM.decodeCore(new Uint8Array(msg.core)), msg.argv, msg.env);
      resumed = true;
    }
    post({ type: 'log', text: `[modus: ${resumed ? 'core restored' : 'cold boot'} in ${Math.round(performance.now() - t0)}ms]` });
    // Warm the compiler/reader/eval hot set (see warmset.js).  A core restore has
    // an empty JS-translation cache, so the first form typed or file loaded would
    // otherwise JS-translate ~345 functions AND run them interpreted until they
    // cross the compile threshold.  Doing it up front (behind the load spinner)
    // makes the first REPL interaction as fast as later ones.
    if (resumed && self.MVM_WARMSET) {
      const tw = performance.now();
      let n = 0; for (const name of self.MVM_WARMSET) { try { if (vm.forceCompile(name)) n++; } catch (e) {} }
      post({ type: 'log', text: `[warmed ${n}/${self.MVM_WARMSET.length} fns in ${Math.round(performance.now() - tw)}ms]` });
    }
    let code;
    try {
      code = vm.main(resumed);
    } catch (e) {
      host.flush();
      if (e instanceof X.MvmFault) { post({ type: 'fault', text: e.message }); return; }
      throw e;
    }
    host.flush();
    post({ type: 'exit', code, steps: vm.steps });
  } catch (e) {
    post({ type: 'fault', text: String(e && e.stack || e) });
  }
};
