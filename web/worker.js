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

importScripts('mvm.js', 'host-browser.js', 'warmset.js');
const X = self.MVM_EXPORTS;

self.onmessage = (ev) => {
  const msg = ev.data;
  if (msg.type !== 'start') return;
  const post = (m, t) => self.postMessage(m, t);
  try {
    const t0 = performance.now();
    const mod = X.loadModule(new Uint8Array(msg.mvmw));
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
