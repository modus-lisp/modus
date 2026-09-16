#!/usr/bin/env node
'use strict';
// run-node.js — run the Modus CLI image under the JS interpreter.
//
//   node web/run-node.js [--mvmw FILE] [--trace N] [--debug] [--semi MB] -- MODUS ARGS...
//   node web/run-node.js --eval '(print (+ 1 2))' --quit
//
// Everything not recognised here is passed to the Modus toplevel as argv.

const fs = require('fs');
const path = require('path');
const { MVM, loadModule, MvmFault } = require('./mvm.js');
const { NodeHost } = require('./host-node.js');

const opts = { mvmw: path.join(__dirname, 'modus.mvmw'), trace: 0, debug: false, semi: 256 };
const argv = ['modus'];
const args = process.argv.slice(2);
for (let i = 0; i < args.length; i++) {
  const a = args[i];
  if (a === '--mvmw') opts.mvmw = args[++i];
  else if (a === '--trace') opts.trace = parseInt(args[++i], 10);
  else if (a === '--debug') opts.debug = true;
  else if (a === '--profile') opts.profile = true;
  else if (a === '--no-compile') opts.compile = false;
  else if (a === '--compile-threshold') opts.compileThreshold = parseInt(args[++i], 10);
  else if (a === '--trace-from') opts.traceFrom = parseInt(args[++i], 10);
  else if (a === '--trace-count') opts.traceCount = parseInt(args[++i], 10);
  else if (a === '--trace-regs') opts.traceRegs = true;
  else if (a === '--watch') opts.watch = args[++i];
  else if (a === '--max-steps') opts.maxSteps = parseInt(args[++i], 10);
  else if (a === '--semi') opts.semi = parseInt(args[++i], 10);
  else if (a === '--save-core') opts.saveCore = args[++i];
  else if (a === '--core') opts.core = args[++i];
  else if (a === '--snapshot-at') opts.snapshotAt = args[++i];
  else if (a === '--') { argv.push(...args.slice(i + 1)); break; }
  else argv.push(a);
}

const mod = loadModule(new Uint8Array(fs.readFileSync(opts.mvmw)));
const env = Object.entries(process.env).map(([k, v]) => `${k}=${v}`);
const host = new NodeHost();
const vm = new MVM(mod, host, { argv, env, trace: opts.trace, debug: opts.debug, maxSteps: opts.maxSteps, compile: opts.compile, compileThreshold: opts.compileThreshold, traceFrom: opts.traceFrom, traceCount: opts.traceCount, traceRegs: opts.traceRegs, profile: opts.profile, semispace: opts.semi << 20 });
if (opts.watch) { const f = vm.byName.get(opts.watch); if (!f) throw new Error('no fn ' + opts.watch); vm.watchAt = f.off; vm.watchLeft = 60; }
const t0 = Date.now();
let code = 0;
let resumed = false;
if (opts.core) {
  const zlib = require('zlib');
  let bytes = new Uint8Array(fs.readFileSync(opts.core));
  if (bytes[0] === 0x1f && bytes[1] === 0x8b) bytes = new Uint8Array(zlib.gunzipSync(bytes));
  vm.restore(MVM.decodeCore(bytes), argv, env);
  resumed = true;
  if (opts.trace) host.log(`[restored ${opts.core} in ${Date.now() - t0}ms]`);
}
if (opts.saveCore) {
  const fn = vm.byName.get(opts.snapshotAt || 'CLI-TOPLEVEL');
  if (!fn) throw new Error('no such function to snapshot at: ' + opts.snapshotAt);
  vm.snapAt = fn.off;
  vm.onSnapshot = () => {
    const core = vm.snapshot();
    const zlib = require('zlib');
    const enc = MVM.encodeCore(core);
    const gz = zlib.gzipSync(enc, { level: 6 });
    fs.writeFileSync(opts.saveCore, gz);
    host.log(`[saved core ${opts.saveCore}: ${enc.length >> 10}K raw, ${gz.length >> 10}K gz, after ${vm.steps} steps, ${Date.now() - t0}ms]`);
    process.exit(0);
  };
}
try {
  code = vm.main(resumed);
} catch (e) {
  if (e instanceof MvmFault) { process.stderr.write('MVM FAULT: ' + e.message + '\n'); code = 70; }
  else { process.stderr.write('MVM internal error at ' + vm.where() + ' (step ' + vm.steps + ')\n  ' + vm.backtrace().join('\n  ') + '\n'); throw e; }
}
if (process.env.DUMP_COMPILED) {
  const off2name = new Map(); for (const f of vm.fnsPhys) off2name.set(f.off, f.name);
  const names = [...vm.compiled.entries()].filter(([, f]) => f).map(([off]) => off2name.get(off)).filter(Boolean).sort();
  require('fs').writeFileSync(process.env.DUMP_COMPILED, JSON.stringify(names, null, 0));
  host.log(`[dumped ${names.length} compiled fn names to ${process.env.DUMP_COMPILED}]`);
}
if (opts.profile) host.log(vm.profileReport(40));
if (vm.dcount.size) host.log('[dynamic delegated: ' + [...vm.dcount.entries()].sort((a, b) => b[1] - a[1]).slice(0, 12).map(([o, n]) => '0x' + o.toString(16) + ':' + n).join(' ') + ']');
if (opts.trace && vm.compileStats.delegated.size) host.log('[delegated ops: ' + [...vm.compileStats.delegated.entries()].sort((a, b) => b[1] - a[1]).slice(0, 12).map(([o, n]) => '0x' + o.toString(16) + ':' + n).join(' ') + ']');
if (opts.trace) host.log(`[compiled ${vm.compileStats.fns} fns / ${vm.compileStats.insns} insns, ${vm.compileStats.failed} failed, ${vm.compileStats.ms | 0} ms]`);
if (opts.trace) host.log(`[exit ${code}: ${vm.steps} steps, ${vm.gcCount} gcs, ${Date.now() - t0}ms]`);
process.exitCode = code;
