'use strict';
// worker.js — runs the MVM interpreter off the main thread.
//
// Messages in:  { type: 'start', mvmw: ArrayBuffer, core: ArrayBuffer|null,
//                 stdin: SharedArrayBuffer, http: SharedArrayBuffer, inbox: SharedArrayBuffer,
//                 argv: [...], env: [...],
//                 files: [{ path, bytes }] }
// Messages out: { type: 'stdout', bytes }, { type: 'log', text },
//               { type: 'waiting' }, { type: 'exit', code }, { type: 'fault', text },
//               { type: 'http', url, method, headers, body }, { type: 'file', path, bytes }

importScripts('mvm.js', 'host-browser.js');
const X = self.MVM_EXPORTS;

self.onmessage = (ev) => {
  const msg = ev.data;
  if (msg.type !== 'start') return;
  const post = (m, t) => self.postMessage(m, t);
  try {
    const t0 = performance.now();
    const mod = X.loadModule(new Uint8Array(msg.mvmw));
    const host = new BrowserHost(msg.stdin, post, msg.http, msg.inbox);
    for (const f of msg.files || []) host.addFile(f.path, new Uint8Array(f.bytes));
    const vm = new X.MVM(mod, host, { argv: msg.argv, env: msg.env, semispace: msg.semispace, trace: msg.trace | 0 });
    let resumed = false;
    if (msg.core) {
      vm.restore(X.MVM.decodeCore(new Uint8Array(msg.core)), msg.argv, msg.env);
      resumed = true;
    }
    post({ type: 'log', text: `[modus: ${resumed ? 'core restored' : 'cold boot'} in ${Math.round(performance.now() - t0)}ms]` });
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
