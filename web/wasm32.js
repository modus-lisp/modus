'use strict';
// wasm32.js -- MVM bytecode -> WebAssembly, for web/mvm32.js (MVM_WASM=1).
//
// A drop-in for mvm32.js's JS translator (compileFn): the function it returns
// takes (vm, L) and answers with the same protocol -- -1 returned, t*2 "call t
// and resume me at vm.resumeL", t*2+1 "tail-called t" -- and carries RES
// (setjmp landing labels) and ENTRYL (an OSR entry's label), so drive(),
// direct calls, OSR and handler-case re-entry work unchanged.
//
// The machine model is mvm32.js's: the heap, stack and frames live in the
// VM's memory, which is a WebAssembly.Memory shared with the module.  All 16
// V registers are i32 LOCALS; they are written to their frame homes before
// anything that can look at the frame (a call, a collection, a trap, a
// delegated instruction) and read back after.  VR / VA / VL / ESP / EBP / PC
// are imported mutable globals, synced with the VM's properties at every
// JS boundary.  Anything not translated here -- allocation of vectors and
// floats, the FP register file, SAPs, W32 ops, rare traps -- is DELEGATED to
// vm.execInsn through an import, exactly as the JS translator delegates.
//
// Control flow: one block per label, nested, dispatched by a br_table at the
// top of a loop.  A FORWARD jump is a plain `br' out to the target's block;
// only a backward jump sets L and re-enters the dispatch.

let C = null;
// Environment and log: node's process.env / stderr, or in a browser worker the
// MVM_ENV object and MVM_LOG function worker.js sets on its global (no node
// globals are touched there).
const ENV = typeof process !== 'undefined' && process.env ? process.env : (globalThis.MVM_ENV || {});
const LOG = typeof process !== 'undefined' && process.stderr ? (s) => process.stderr.write(s) : (s) => (globalThis.MVM_LOG || console.log)(s);
const ON_EXIT = typeof process !== 'undefined' && process.on ? (f) => process.on('exit', f) : () => {};
const W_OPS = ENV.MVM_WASM_COUNT ? new Float64Array(256) : null;
const W_TR = ENV.MVM_WASM_COUNT ? new Map() : null, W_DC = ENV.MVM_WASM_COUNT ? new Map() : null;
if (W_TR) ON_EXIT(() => {
  LOG('[wasm traps ' + [...W_TR].sort((a, b) => b[1] - a[1]).slice(0, 8).map(([c, n]) => '0x' + c.toString(16) + ':' + n).join(' ') + ']\n');
  LOG('[wasm dcall\n  ' + [...W_DC].sort((a, b) => b[1] - a[1]).slice(0, 10).map(([c, n]) => n + ' ' + c).join('\n  ') + ']\n'); });
if (W_OPS) ON_EXIT(() => { const r = []; for (let i = 0; i < 256; i++) if (W_OPS[i]) r.push([W_OPS[i], i]); r.sort((a, b) => b[0] - a[0]); LOG('[wasm exec ops ' + r.slice(0, 15).map(([n, i]) => '0x' + i.toString(16) + ':' + n).join(' ') + ']\n'); });
function init(c) { C = c; }

// ---- binary encoding --------------------------------------------------------
function uleb(o, v) { v >>>= 0; do { let b = v & 0x7f; v >>>= 7; if (v) b |= 0x80; o.push(b); } while (v); }
function sleb(o, v) {
  v |= 0;
  for (;;) { const b = v & 0x7f; v >>= 7;
    if ((v === 0 && !(b & 0x40)) || (v === -1 && (b & 0x40))) { o.push(b); return; }
    o.push(b | 0x80); }
}
function str(o, s) { uleb(o, s.length); for (let i = 0; i < s.length; i++) o.push(s.charCodeAt(i)); }
function section(o, id, body) { o.push(id); uleb(o, body.length); for (const b of body) o.push(b); }

const I32 = 0x7f, I64 = 0x7e;
// imports, in index order: [name, params, results]
const IMPORTS = [
  ['exec', [I32], [I32]],            // 0 delegate pc -> vm.cmp
  ['gc', [I32], []],                 // 1
  ['checked', [I32, I32, I32], [I32]], // 2 op a b -> result
  ['fault', [I32], []],              // 3 pc (throws)
  ['tail', [I32], []],               // 4 t
  ['trap', [I32, I32, I32], []],     // 5 code next pc
  ['cons', [I32, I32], [I32]],       // 6 car cdr -> cons
  ['callreq', [I32, I32], []],       // 7 t resumeL
  ['dcall', [I32, I32], [I32]],      // 8 t pc -> 0 done / 1 done+gc / 2 cannot
  ['dcallind', [I32, I32], [I32]],   // 9 fnobj pc -> (t<<2)|status
  ['finish', [I32, I32], []],        // 10 t q: a Wasm callee answered a request; finish it in drive()
  ['slotfor', [I32], [I32]],         // 11 t -> table slot of t's Wasm code, or -1
  ['fbytes', [I32], [I32]],          // 12 t -> its frame size
  ['syncin', [], []],                // 13 Wasm globals <- VM (after a longjmp)
  ['ljlabel', [I32, I32, I32], [I32]], // 14 function id, ip, frame -> its label there, or -1
  ['dbgenter', [I32, I32, I32], []],  // 15 debug: a setjmp function entered (id, frame, L)
];
const FNTYPE = 0;   // (i32 L) -> i32, always type 0
const NOFAST = !!ENV.MVM_WASM_NOFAST;
const NOTAIL = !!ENV.MVM_WASM_NOTAIL, NOSJ = !!ENV.MVM_WASM_NOSJ;
const MAXINSN = (ENV.MVM_WASM_MAXINSN && parseInt(ENV.MVM_WASM_MAXINSN, 10)) || 100000;
// Labels per chunk: a function with more becomes an entry dispatcher plus
// one Wasm function per CHL labels (the JS translator's CHUNK, for the same
// reasons: TurboFan's frame and compile time grow with a function's size).
const CHL = (ENV.MVM_WASM_CHUNK && parseInt(ENV.MVM_WASM_CHUNK, 10)) || 256;
const NOINL = new Set((ENV.MVM_WASM_NOINL || '').split(',').filter(Boolean).map((x) => parseInt(x, 16)));
const F_EXEC = 0, F_GC = 1, F_CHECKED = 2, F_FAULT = 3, F_TAIL = 4, F_TRAP = 5, F_CONS = 6,
      F_CALLREQ = 7, F_DCALL = 8, F_DCALLIND = 9, F_FINISH = 10, F_SLOTFOR = 11, F_FBYTES = 12, F_SYNCIN = 13, F_LJLABEL = 14, F_DBGENTER = 15;
// dep: Wasm-to-Wasm call depth (native stack), not mirrored in the VM.
// cmp / ovf: the compare and overflow flags across a jump between chunks.
const FB_BIAS = 4096;          // see LOAD
const DIRTYOFF = !!ENV.MVM_WASM_NODIRTY;
const NOINLSJ = !!ENV.MVM_WASM_NOINLSJ;      // bisect: setjmp / handler-pop through JS   // bisect: spill every register every time
const GLOBALS = ['vr', 'va', 'vl', 'esp', 'ebp', 'pc', 'dep', 'cmp', 'ovf'];
const G_VR = 0, G_VA = 1, G_VL = 2, G_ESP = 3, G_EBP = 4, G_PC = 5, G_DEP = 6, G_CMP = 7, G_OVF = 8;
const WDEPTH = (ENV.MVM_WASM_DEPTH && parseInt(ENV.MVM_WASM_DEPTH, 10)) || 600;
const SYNCED = ['vr', 'va', 'vl', 'esp', 'ebp', 'pc'];

// ---- per-VM state: the shared globals and the import object --------------------
function state(vm) {
  if (vm._w) return vm._w;
  const g = {};
  for (const n of GLOBALS) g[n] = new WebAssembly.Global({ value: 'i32', mutable: true }, 0);
  const syncIn = () => { g.vr.value = vm.vrl; g.va.value = vm.va; g.vl.value = vm.vl; g.esp.value = vm.esp; g.ebp.value = vm.ebp; g.pc.value = vm.pc; };
  // Every compiled Wasm function has a slot in one shared table, so a Wasm
  // caller reaches it with call_indirect and no JS frame in between.
  const table = new WebAssembly.Table({ element: 'anyfunc', initial: 1024 });
  const slots = new Map();
  const resMaps = [];
  const slotOf = (t) => {
    let s = slots.get(t);
    if (s === undefined) { s = slots.size; slots.set(t, s); if (s >= table.length) table.grow(table.length); }
    return s;
  };
  const syncOut = () => { vm.vrl = g.vr.value; vm.va = g.va.value; vm.vl = g.vl.value; vm.esp = g.esp.value; vm.ebp = g.ebp.value; vm.pc = g.pc.value; };
  const direct = (t, c) => {
    vm.push(C.RET_SENTINEL); vm.enter(t);
    const g0 = vm.gcCount;
    if (c.RES) vm.drive(c);
    else { vm.jd++; let q; try { q = c(vm, 0); } finally { vm.jd--; } if (q !== -1) vm.drive(c, 0, q, vm.ld32(vm.ebp)); }
    syncIn();
    return vm.gcCount !== g0 ? 1 : 0;
  };
  const f = {
    exec(pc) { if (W_OPS) W_OPS[vm.m8[pc]]++; syncOut(); vm.execInsn(pc); syncIn(); return vm.cmp; },
    gc(pc) { syncOut(); vm.pc = pc; vm.gc(); syncIn(); },
    checked(op, a, b) { syncOut(); const r = vm.checked(op, a, b); syncIn(); return r; },
    fault(pc) { syncOut(); vm.pc = pc; vm.memFault('car/cdr/rplac of non-cons'); },
    tail(t) { syncOut(); vm.doTailcall(t); syncIn(); },
    trap(code, next, pc) { if (W_TR) W_TR.set(code, (W_TR.get(code) || 0) + 1); syncOut(); vm.pc = pc; vm.trap(code, next); syncIn(); },
    cons(a, d) { syncOut(); const r = vm.allocCons(a, d); syncIn(); return r; },
    callreq(t, resumeL) { syncOut(); vm.resumeL = resumeL; vm.nextF = undefined; vm.push(C.RET_SENTINEL); vm.enter(t); syncIn(); },
    dcall(t, pc) {
      if (W_DC) { const k = vm.where(t) + ' <- ' + vm.where(pc); W_DC.set(k, (W_DC.get(k) || 0) + 1); }
      syncOut(); vm.pc = pc;
      const c = vm.compiledFor(t);
      if (!c || vm.jd >= C.JD_MAX) return 2;
      return direct(t, c);
    },
    slotfor(t) {
      syncOut();
      const c = vm.compiledFor(t);
      syncIn();
      return c && c.wexp ? slotOf(t) : -1;
    },
    fbytes(t) { return C.frameBytes(vm.m8, t); },
    syncin() { syncIn(); },
    dbgenter(id, fb, L) { LOG(`[sj-enter ${vm.where(resMaps[id].keys().next().value)} frame=${((fb + C.VBASE) >>> 0).toString(16)} L=${L}]\n`); },
    ljlabel(id, ip, fb) {
      const l = vm.ebp - C.VBASE === fb ? resMaps[id].get(ip) : undefined;
      if (ENV.MVM_WASM_DEBUG && l === undefined) { const owner = resMaps.findIndex((m) => m.has(ip)); const c = [...vm.compiled].find(([t, f]) => f && f.RES && f.RES[ip] !== undefined); LOG(`[lj-miss owner-wasm-module=${owner} js-owner=${c ? vm.where(c[0]) + (c[1].wexp ? ' (wasm)' : ' (js)') : 'none (interpreted?)'}]\n`); }
      if (ENV.MVM_WASM_DEBUG && l !== undefined) vm.dumpFrame('wasm');
      if (ENV.MVM_WASM_DEBUG && l !== undefined) { let q = ip, out = []; for (let k = 0; k < 14; k++) { const op = vm.m8[q], n = C.INSN_LEN[op]; out.push(Array.from(vm.m8.slice(q, q + n)).map((x) => x.toString(16).padStart(2, '0')).join('')); q += n || 1; } LOG('[lj code ' + out.join(' ') + ']\n'); }
      if (ENV.MVM_WASM_DEBUG) LOG(`[lj ip=${vm.where(ip)} vm.ebp=${(vm.ebp >>> 0).toString(16)} frame=${((fb + C.VBASE) >>> 0).toString(16)} -> ${l}]\n`);
      return l === undefined ? -1 : l;
    },
    finish(t, q) {
      syncOut();
      const c = vm.compiled.get(t);
      vm.jd++; try { vm.drive(c, 0, q, vm.ld32(vm.ebp)); } finally { vm.jd--; }
      syncIn();
    },
    dcallind(x, pc) {
      syncOut(); vm.pc = pc;
      const t = vm.fnAddrToOffset(x);
      const c = vm.compiledFor(t);
      if (!c || vm.jd >= C.JD_MAX) return (t << 2) | 2;
      return (t << 2) | direct(t, c);
    },
  };
  const imports = { e: { m: vm.wmem, t: table, lj: C.LJ_TAG } };
  for (const n of GLOBALS) imports.e['g_' + n] = g[n];
  const counts = {};
  if (ENV.MVM_WASM_COUNT) {
    for (const [n] of IMPORTS) { counts[n] = 0; const h = f[n]; imports.e[n] = function (a, b, c) { counts[n]++; return h(a, b, c); }; }
    ON_EXIT(() => LOG("[wasm imports " + JSON.stringify(counts) + " entries " + vm._w.entries + " stats " + JSON.stringify(vm._w.stats) + "]\n"));
  } else for (const [n] of IMPORTS) imports.e[n] = f[n];
  if (ENV.MVM_WASM_DEBUG) ON_EXIT(() => { if (vm._w.dump) LOG('[RANGE ' + vm._w.dump + ']\n'); });
  vm._w = { g, imports, syncIn, syncOut, table, slotOf, resMaps, stats: { fns: 0, failed: 0, bytes: 0, ms: 0 } };
  return vm._w;
}

// ---- the translator ------------------------------------------------------------
function compile(vm, entry, osr) {
  const m8 = vm.m8;
  if (m8[entry] === 0x72 && m8[entry + 1] === 0x72) return null;   // late-binding thunk
  const t0 = typeof performance !== 'undefined' ? performance.now() : Date.now();
  const W = state(vm);
  const { VBASE, NIL, TV, IX, slotOff, FRAME_SIZE, INSN_LEN, JIT_PHYS, A_WEB_CONSTS, POOL_ADDR,
          A_MVCOUNT, A_CENV, A_NARGS, FN_UNRESOLVED, frameEnterParams, ldbMask } = C;
  const rd32 = (p) => (m8[p] | (m8[p + 1] << 8) | (m8[p + 2] << 16) | (m8[p + 3] << 24));

  // 1. control-flow graph (as compileFn)
  const starts = new Set([entry]);
  // BRANCH targets: a label only a fallthrough (a call's return point, a
  // conditional's not-taken side) or the dispatcher after a full LOAD can
  // reach keeps the register state it was reached with; these reset it.
  const jtargets = new Set();
  const seen = new Set();
  const work = [entry];
  const insns = [];
  let hasSetjmp = false;
  const sjResume = [];
  let resId = -1;
  while (work.length) {
    let pc = work.pop();
    while (!seen.has(pc)) {
      seen.add(pc);
      const op = m8[pc], len = INSN_LEN[op];
      if (len === 0) { W.stats.failed++; if (ENV.MVM_WASM_DEBUG) LOG(`[wasm refuse ${vm.where(entry)}: opcode 0x${op.toString(16)} len 0]\n`); return null; }
      insns.push([pc, op, len]);
      if ((op >= 0x41 && op <= 0x46) || op === 0xC7) {
        const t = pc + len + rd32(pc + 1); starts.add(t); jtargets.add(t); work.push(t); starts.add(pc + len);
      } else if (op === 0x40) { const t = pc + len + rd32(pc + 1); starts.add(t); jtargets.add(t); work.push(t); break; }
      else if (op === 0x47 || op === 0x48) { const t = pc + len + rd32(pc + 2); starts.add(t); jtargets.add(t); work.push(t); starts.add(pc + len); }
      else if (op === 0x82 || op === 0x83) break;
      else if (op === 0x80 || op === 0x81) starts.add(pc + len);
      else if (op === 0x02 && (m8[pc + 1] | (m8[pc + 2] << 8)) === 0x0510) { hasSetjmp = true; starts.add(pc + len); sjResume.push(pc + len); }
      else if (op === 0x90 || op === 0x91 || op === 0xA2 || op === 0x01) { W.stats.failed++; if (ENV.MVM_WASM_DEBUG) LOG(`[wasm refuse ${vm.where(entry)}: opcode 0x${op.toString(16)}]\n`); return null; }
      pc += len;
    }
  }
  insns.sort((a, b) => a[0] - b[0]);
  if (hasSetjmp && NOSJ) return null;
  // TurboFan's frame for a huge function is huge (tens of KB: spill slots
  // across thousands of merge points), and a few nested ones overflow the
  // native stack.  Those stay on the JS translator.
  if (insns.length > MAXINSN) { W.stats.big = (W.stats.big || 0) + 1; return null; }
  const label = new Map(); let N = 0;
  for (const [pc] of insns) if (starts.has(pc)) label.set(pc, N++);
  if (hasSetjmp) { resId = W.resMaps.length; W.resMaps.push(label); }
  const insnIdx = new Map(); for (let i = 0; i < insns.length; i++) insnIdx.set(insns[i][0], i);

  const tCfg = performance.now(); W.stats.cfgMs = (W.stats.cfgMs || 0) + (tCfg - t0);
  // 2. code
  const LP = 0, LR = (v) => 1 + v, LCMP = 17, LOVF = 18, LFB = 19, LT1 = 20, LT2 = 21, LT3 = 22, LDEP = 23;
  const NLOCALS = 23;           // beyond the param
  let o = [];                   // current function body bytes
  let nglob = GLOBALS.length;   // module-defined globals (call-site caches) follow the imported ones
  let cur = -1;                 // label of the code being emitted
  let nest = 0;                 // extra block nesting inside the current code
  const i32c = (v) => { o.push(0x41); sleb(o, v); };
  const lget = (l) => { o.push(0x20); uleb(o, l); };
  const lset = (l) => { o.push(0x21); uleb(o, l); };
  const ltee = (l) => { o.push(0x22); uleb(o, l); };
  const gget = (g) => { o.push(0x23); uleb(o, g); };
  const gset = (g) => { o.push(0x24); uleb(o, g); };
  const call = (f) => { o.push(0x10); uleb(o, f); };
  const ld32 = (off = 0) => { o.push(0x28, 0x02); uleb(o, off); };
  const st32 = (off = 0) => { o.push(0x36, 0x02); uleb(o, off); };
  const op1 = (b) => o.push(b);
  // DIRTY: bit v set = V<v>'s local may differ from its frame home.  A spill
  // (STOREv) stores only dirty registers; LOAD and STOREv clean them; set()
  // dirties.  Every label starts all-dirty (any predecessor), and a block's
  // join takes the union of its arms, so a store skipped on one arm is never
  // taken as done after it.  Measured: spills were 59% of every module.
  let dirty = 0xFFFF;
  const dstack = [];
  const IFv = (t) => { o.push(0x04, t); nest++; dstack.push({ entry: dirty, then: -1 }); };
  const IF = () => IFv(0x40);
  const ELSE = () => { o.push(0x05); const d = dstack[dstack.length - 1]; d.then = dirty; dirty = d.entry; };
  const END = () => { o.push(0x0B); nest--; const d = dstack.pop(); dirty |= d.then >= 0 ? d.then : d.entry; };
  const RET = () => o.push(0x0F);
  // value of vreg v on the stack / stack into vreg v
  const get = (v) => {
    if (v < 16) return lget(LR(v));
    switch (v) { case 16: return gget(G_VR); case 17: return gget(G_VA); case 18: return gget(G_VL);
      case 19: return i32c(NIL); case 20: return gget(G_ESP); case 21: return gget(G_EBP); default: return gget(G_PC); }
  };
  const set = (v) => {
    if (v < 16) { dirty |= 1 << v; return lset(LR(v)); }
    switch (v) { case 16: return gset(G_VR); case 17: return gset(G_VA); case 18: return gset(G_VL);
      case 20: return gset(G_ESP); case 21: return gset(G_EBP); default: return op1(0x1A); }
  };
  // memory at (stack top address) - VBASE
  const adr = (delta = 0) => { i32c(delta - VBASE); op1(0x6A); };
  // LFB is the frame base minus FB_BIAS, so a home's or slot's (negative)
  // offset becomes the load/store's own unsigned immediate: 7 bytes a spill
  // instead of 11 (spills were 59% of every module).
  const LOAD = (from = 0, to = 15) => { for (let v = from; v <= to; v++) { lget(LFB); ld32(IX[v] * 4 + FB_BIAS); lset(LR(v)); dirty &= ~(1 << v); } };
  function ld32_neg(off) { i32c(off); op1(0x6A); ld32(); }
  const STOREv = (from = 0, to = 15, all = DIRTYOFF) => {
    for (let v = from; v <= to; v++) if (all || (dirty & (1 << v))) { lget(LFB); lget(LR(v)); st32(IX[v] * 4 + FB_BIAS); dirty &= ~(1 << v); }
  };
  const fbArg = () => { lget(LFB); i32c(FB_BIAS); op1(0x6A); };   // the true frame base, for an import
  const setPC = (pc) => { i32c(pc); gset(G_PC); };
  const reloadFB = () => { gget(G_EBP); i32c(-VBASE - FB_BIAS); op1(0x6A); lset(LFB); };
  // jump to label of address t: forward = br to its block, backward = dispatch
  const chunked = N > CHL;
  const chunkOf = (j) => (j / CHL) | 0;
  let lo = 0, hi = N - 1;       // the current chunk's labels (all of them unchunked)
  // leave this chunk for label j: registers and flags to memory, ask the entry
  const crossTo = (j) => {
    STOREv(); lget(LCMP); gset(G_CMP); lget(LOVF); gset(G_OVF);
    i32c(-(2 + j)); RET();
  };
  const jumpTo = (t, cond) => {
    const j = label.get(t);
    if (chunked && chunkOf(j) !== chunkOf(cur)) {
      if (cond) IF();
      crossTo(j);
      if (cond) END();
      return;
    }
    if (j > cur) {
      if (cond) { o.push(0x0D); uleb(o, j - cur - 1 + nest); }
      else { o.push(0x0C); uleb(o, j - cur - 1 + nest); }
    } else {
      if (cond) IF();
      i32c(j); lset(LP); o.push(0x0C); uleb(o, hi - cur + nest);
      if (cond) END();
    }
  };
  const cmpFrom = () => {        // stack: a b -> cmp = (a>b)-(a<b)
    lset(LT2); lset(LT1);
    lget(LT1); lget(LT2); op1(0x4A); lget(LT1); lget(LT2); op1(0x48); op1(0x6B); lset(LCMP);
  };
  const testFrom = () => {       // stack: r -> cmp = (r>0)-(r<0)
    ltee(LT1); i32c(0); op1(0x4A); lget(LT1); i32c(0); op1(0x48); op1(0x6B); lset(LCMP);
  };
  // mark LT3's granule in the start bitmap (and the cons bitmap)
  const markOne = (off) => {
    lget(LT3); i32c(vm.pageBase); op1(0x6B); i32c(4); op1(0x76); lset(LT2);
    lget(LT2); i32c(3); op1(0x76); i32c(off); op1(0x6A); ltee(LT1);
    lget(LT1); o.push(0x2D, 0x00, 0x00);
    i32c(1); lget(LT2); i32c(7); op1(0x71); op1(0x74); op1(0x72);
    o.push(0x3A, 0x00, 0x00);
  };
  const markBits = (cons) => { markOne(vm.bmpOff); if (cons) markOne(vm.consBmpOff); };
  // Push "T is callable as Wasm" and return how to get T, its slot and its
  // frame size: constants for a plain function, a per-site cache for a thunk.
  const calleeCache = (t) => {
    const thunk = m8[t] === 0x72 && m8[t + 1] === 0x72;
    let T, S, F;
    if (!thunk) {
      const fbytes = C.frameBytes(m8, t), slot = W.slotOf(t);
      T = () => i32c(t); S = () => i32c(slot); F = () => i32c(fbytes);
      // callee compiled to Wasm?
      i32c(slot); o.push(0x25, 0x00); o.push(0xD1); op1(0x45);
    } else {
      // a late-binding thunk whose +2 is TAILCALL target: call the
      // target, through a per-site cache (target, slot, frame size).
      const gT = nglob++, gS = nglob++, gF = nglob++;
      T = () => gget(gT); S = () => gget(gS); F = () => gget(gF);
      i32c(t + 2); o.push(0x2D, 0x00, 0x00); i32c(0x83); op1(0x46);
      IFv(I32);
        i32c(t + 3); o.push(0x28, 0x00, 0x00); lset(LT2);
        lget(LT2); gget(gT); op1(0x47);
        IF();
          lget(LT2); call(F_SLOTFOR); gset(gS);
          lget(LT2); call(F_FBYTES); gset(gF);
          // cache only a hit: an uncompiled target is asked again
          gget(gS); i32c(0); op1(0x4E); IF(); lget(LT2); gset(gT); ELSE(); i32c(-1); gset(gT); END();
        END();
        gget(gT); lget(LT2); op1(0x46);
      ELSE(); i32c(0); END();
    }
    return { T, S, F };
  };
  const fastCall = (T, S, F) => {
    // fast: callee is Wasm, depth and stack allow -> push the sentinel
    // and the frame here (enter()), call_indirect, reload V4-V8.
    gget(G_DEP); i32c(WDEPTH); op1(0x48); op1(0x71);
    gget(G_ESP); i32c(C.STACK_ADDR + 4096 + 8); F(); op1(0x6A); op1(0x4A); op1(0x71);
    IF();
      // [esp-4] = RET_SENTINEL; [esp-8] = ebp; ebp = esp-8; esp = ebp - fbytes
      gget(G_ESP); i32c(-8); op1(0x6A); lset(LT3);
      lget(LT3); adr(4); i32c(-1); st32();
      lget(LT3); adr(); gget(G_EBP); st32();
      for (const v of [0, 1, 2, 3, 4]) { lget(LT3); adr(IX[v] * 4); lget(LR(v)); st32(); }
      lget(LT3); gset(G_EBP); lget(LT3); F(); op1(0x6B); gset(G_ESP);
      T(); gset(G_PC);
      gget(G_DEP); i32c(1); op1(0x6A); gset(G_DEP);
      i32c(0); S(); o.push(0x11); uleb(o, FNTYPE); o.push(0x00); lset(LT1);
      gget(G_DEP); i32c(1); op1(0x6B); gset(G_DEP);
      lget(LT1); i32c(-1); op1(0x47); IF(); T(); lget(LT1); call(F_FINISH); END();
      LOAD(4, 15);
  };
  // absolute address A as a memory operand base (adr's form, from a constant)
  const absA = (a) => i32c(a - VBASE);
  const st64lo = (a, val) => { absA(a); val(); st32(); absA(a + 4); i32c(0); st32(); };
  const inlineHandlerPop = () => {
    const { A_JMPBUF, A_HDEPTH, A_HSTACK, A_HOVF, JMPBUF_WORDS } = C;
    absA(A_HOVF); ld32(); ltee(LT1);
    IF();                                   // overflowed pushes are popped first
      absA(A_HOVF); lget(LT1); i32c(1); op1(0x6B); st32();
    ELSE();
      absA(A_HDEPTH); ld32(); ltee(LT1); op1(0x45);
      IF();
        for (let i = 0; i < JMPBUF_WORDS; i++) st64lo(A_JMPBUF + 8 * i, () => i32c(0));
      ELSE();
        lget(LT1); i32c(1); op1(0x6B); lset(LT1);
        st64lo(A_HDEPTH, () => lget(LT1));
        lget(LT1); i32c(5); op1(0x74); i32c(A_HSTACK - VBASE); op1(0x6A); lset(LT2);   // frame
        for (let i = 0; i < JMPBUF_WORDS; i++) st64lo(A_JMPBUF + 8 * i, () => { lget(LT2); ld32(8 * i); });
      END();
    END();
  };
  const inlineSetjmp = (resumePc) => {
    const { A_JMPBUF, A_HDEPTH, A_HSTACK, HMAX, A_HOVF, JMPBUF_WORDS } = C;
    absA(A_HDEPTH); ld32(); ltee(LT1); i32c(HMAX); op1(0x4E);           // depth >= HMAX
    IF();
      absA(A_HOVF); absA(A_HOVF); ld32(); i32c(1); op1(0x6A); st32();
    ELSE();
      lget(LT1); i32c(5); op1(0x74); i32c(A_HSTACK - VBASE); op1(0x6A); lset(LT2);   // push the armed frame
      for (let i = 0; i < JMPBUF_WORDS; i++) { lget(LT2); absA(A_JMPBUF + 8 * i); ld32(); st32(8 * i); lget(LT2); i32c(0); st32(8 * i + 4); }
      st64lo(A_HDEPTH, () => { lget(LT1); i32c(1); op1(0x6A); });
      st64lo(A_JMPBUF, () => gget(G_ESP));
      st64lo(A_JMPBUF + 8, () => gget(G_EBP));
      st64lo(A_JMPBUF + 16, () => i32c(resumePc));
      st64lo(A_JMPBUF + 24, () => lget(LR(4)));                       // V4 (the home, which the JS read)
    END();
    i32c(NIL); gset(G_VR);
  };
  const delegate = (pc) => { STOREv(); i32c(pc); call(F_EXEC); lset(LCMP); LOAD(); };

  const bodies = [];
  // the dispatch skeleton of labels lo..hi: loop, a block per label, br_table on LP-lo
  const skeleton = () => {
    const n = hi - lo + 1;
    o.push(0x03, 0x40);                     // loop $top
    for (let i = 0; i < n; i++) o.push(0x02, 0x40);
    lget(LP); if (lo) { i32c(lo); op1(0x6B); }
    o.push(0x0E); uleb(o, n); for (let i = 0; i < n; i++) uleb(o, i); uleb(o, n - 1);
  };
  const startChunk = (k) => {
    o = []; lo = k * CHL; hi = Math.min(N, lo + CHL) - 1;
    // every register from its home (chunk 0 too: homes past V4 are dead at entry)
    gget(G_EBP); i32c(-VBASE - FB_BIAS); op1(0x6A); lset(LFB);
    LOAD(); gget(G_CMP); lset(LCMP); gget(G_OVF); lset(LOVF);
    skeleton();
  };
  const endChunk = (next) => {
    if (next !== undefined) crossTo(next);  // fall through into the next chunk
    o.push(0x0B, 0x00, 0x0B);               // end loop, unreachable, end function
    bodies.push(o);
  };
  if (chunked) startChunk(0);
  else {
  // entry
  gget(G_EBP); i32c(-VBASE - FB_BIAS); op1(0x6A); lset(LFB);
  if (osr) LOAD();
  else { lget(LP); op1(0x45); IF(); LOAD(0, 4); ELSE(); LOAD(); END(); }
  // A function with setjmp landings: loop $re { try { <dispatch> } catch $lj }.
  // The catch takes a longjmp to THIS frame (ebp) at one of its landings: VM
  // state back from JS, the Wasm depth back to this frame's, re-enter there.
  if (hasSetjmp && ENV.MVM_WASM_DEBUG) { i32c(resId); fbArg(); lget(LP); call(F_DBGENTER); }
  if (hasSetjmp) { gget(G_DEP); lset(LDEP); o.push(0x03, 0x40, 0x06, 0x40); }
  skeleton();
  }

  const guardedCons = (i, pc, b2) => {
    const g1 = insns[i - 1], g2 = insns[i - 2], g3 = insns[i - 3];
    return i >= 3 && !label.has(pc) && g1[1] === 0x42 && g1[0] + 5 === pc
      && g2[1] === 0x33 && g2[0] + 6 === g1[0] && rd32(g2[0] + 2) === 2
      && g3[1] === 0x63 && g3[0] + 3 === g2[0] && m8[g3[0] + 1] === m8[g2[0] + 1]
      && m8[g3[0] + 2] === b2 && m8[g3[0] + 1] !== b2;
  };

  for (let i = 0; i < insns.length; i++) {
    const [pc, op, len] = insns[i];
    if (label.has(pc)) {
      const j = label.get(pc);
      if (chunked && j > 0 && chunkOf(j) !== chunkOf(j - 1)) { endChunk(j); startChunk(chunkOf(j)); }
      o.push(0x0B); cur = j; nest = 0;      // end of block b_cur
      if (dstack.length) throw new Error(`wasm32: unbalanced blocks at label ${pc}`);
      if (jtargets.has(pc) || DIRTYOFF) dirty = 0xFFFF;
    }
    const b1 = m8[pc + 1], b2 = m8[pc + 2], b3 = m8[pc + 3], b4 = m8[pc + 4];
    if (NOINL.has(op)) { delegate(pc); continue; }
    switch (op) {
      case 0x00: case 0x72: case 0x8A: case 0x8B: case 0x92: break;
      case 0x02: {
        const c = b1 | (b2 << 8);
        if (c < 0x100 || c >= 0x8000) {
          // FRAME-ENTER: parameters 5.. are copied from the caller's pushed
          // arguments into their frame slots (vm.trap's first arm), inline.
          const n = frameEnterParams(c);
          for (let i = 4; i < n; i++) { lget(LFB); lget(LFB); ld32(8 + 4 * (i - 4) + FB_BIAS); st32(slotOff(i) + FB_BIAS); }
          break;
        }
        if (c >= 0x100 && c < 0x300) break;
        // SETJMP and HANDLER-POP are memory shuffles (vm.setjmp / vm.handlerPop):
        // inline, not a trip through JS -- a million each loading cl-bench.
        if (c === 0x510 && !NOINLSJ) { inlineSetjmp(pc + len); break; }
        if (c === 0x512 && !NOINLSJ) { inlineHandlerPop(); break; }
        STOREv(); setPC(pc); i32c(c); i32c(pc + len); i32c(pc); call(F_TRAP); reloadFB(); LOAD();
        break;
      }
      case 0x10: get(b2); set(b1); break;
      case 0x11: i32c(rd32(pc + 2)); set(b1); break;
      case 0x14: {
        const idx = rd32(pc + 2);
        if (pc >= JIT_PHYS) { i32c(A_WEB_CONSTS - VBASE); ld32(); adr(4 * idx - 5); ld32(); }
        else { const off = vm.mod.addrTable[idx] | 0; i32c(off === 0 ? 0 : POOL_ADDR + off); }
        set(b1); break;
      }
      case 0x12: gget(G_ESP); i32c(4); op1(0x6B); ltee(LT1); gset(G_ESP); lget(LT1); adr(); get(b1); st32(); break;
      case 0x13: gget(G_ESP); ltee(LT1); adr(); ld32(); set(b1); lget(LT1); i32c(4); op1(0x6A); gset(G_ESP); break;
      case 0x20: get(b2); get(b3); op1(0x6A); set(b1); break;
      case 0x21: get(b2); get(b3); op1(0x6B); set(b1); break;
      case 0x22: get(b2); i32c(1); op1(0x75); get(b3); op1(0x6C); set(b1); break;
      case 0x25: i32c(0); get(b2); op1(0x6B); set(b1); break;
      case 0x26: get(b1); i32c(2); op1(0x6A); set(b1); break;
      case 0x27: get(b1); i32c(2); op1(0x6B); set(b1); break;
      case 0x28: get(b2); get(b3); op1(0x71); set(b1); break;
      case 0x29: get(b2); get(b3); op1(0x72); set(b1); break;
      case 0x2A: get(b2); get(b3); op1(0x73); set(b1); break;
      case 0x2B: if (b3 >= 32) i32c(0); else { get(b2); i32c(b3); op1(0x74); } set(b1); break;
      case 0x2C: if (b3 >= 32) i32c(0); else { get(b2); i32c(b3); op1(0x76); } set(b1); break;
      case 0x2D: get(b2); i32c(Math.min(b3, 31)); op1(0x75); set(b1); break;
      case 0x2E: get(b2); i32c(b3 & 31); op1(0x76); i32c(ldbMask(b4)); op1(0x71); set(b1); break;
      case 0x30: get(b1); get(b2); cmpFrom(); break;
      case 0x31: get(b1); get(b2); op1(0x71); testFrom(); break;
      case 0x33: get(b1); i32c(rd32(pc + 2)); cmpFrom(); break;
      case 0x34: get(b1); i32c(rd32(pc + 2)); op1(0x71); testFrom(); break;
      case 0x40: jumpTo(pc + len + rd32(pc + 1), false); break;
      case 0x41: lget(LCMP); op1(0x45); jumpTo(pc + len + rd32(pc + 1), true); break;
      case 0x42: lget(LCMP); jumpTo(pc + len + rd32(pc + 1), true); break;
      case 0x43: lget(LCMP); i32c(0); op1(0x48); jumpTo(pc + len + rd32(pc + 1), true); break;
      case 0x44: lget(LCMP); i32c(0); op1(0x4E); jumpTo(pc + len + rd32(pc + 1), true); break;
      case 0x45: lget(LCMP); i32c(0); op1(0x4C); jumpTo(pc + len + rd32(pc + 1), true); break;
      case 0x46: lget(LCMP); i32c(0); op1(0x4A); jumpTo(pc + len + rd32(pc + 1), true); break;
      case 0x47: get(b1); i32c(NIL); op1(0x46); jumpTo(pc + len + rd32(pc + 2), true); break;
      case 0x48: get(b1); i32c(NIL); op1(0x47); jumpTo(pc + len + rd32(pc + 2), true); break;
      case 0x50: case 0x51: {
        const d = op === 0x50 ? -1 : 3;
        if (!guardedCons(i, pc, b2)) {
          get(b2); i32c(7); op1(0x71); i32c(1); op1(0x47); IF(); setPC(pc); STOREv(); i32c(pc); call(F_FAULT); END();
        }
        get(b2); adr(d); ld32(); set(b1); break;
      }
      case 0x52: case 0x88: {
        // bump VA by 16 and mark the start and cons bits (allocCons); a heap
        // overrun goes to JS, which faults.
        if (op === 0x52) { get(b2); lset(LT1); get(b3); lset(LT2); } else { i32c(0); lset(LT1); i32c(0); lset(LT2); }
        gget(G_VA); ltee(LT3); i32c(16); op1(0x6A); i32c(vm.heapEnd); op1(0x4A);
        IFv(I32);
          lget(LT1); lget(LT2); call(F_CONS);
        ELSE();
          lget(LT3); i32c(16); op1(0x6A); gset(G_VA);
          lget(LT3); adr(); lget(LT1); st32();
          lget(LT3); adr(4); lget(LT2); st32();
          lget(LT3); adr(8); i32c(0); st32();
          lget(LT3); adr(12); i32c(0); st32();
          markBits(true);
          lget(LT3); i32c(1); op1(0x72);
        END();
        set(op === 0x52 ? b1 : b1); break;
      }
      case 0x60: {
        const count = b2 | (b3 << 8), subtag = b4, total = (((count + 1) * 4) + 15) & ~15;
        gget(G_VA); ltee(LT3); i32c(total); op1(0x6A); i32c(vm.heapEnd); op1(0x4A);
        IF(); delegate(pc); ELSE();
          lget(LT3); i32c(total); op1(0x6A); gset(G_VA);
          lget(LT3); adr(4); i32c(0); i32c(total - 4); o.push(0xFC, 0x0B, 0x00);
          lget(LT3); adr(); i32c((count << 8) | subtag); st32();
          markBits(false);
          lget(LT3); i32c(9); op1(0x72); set(b1);
        END();
        break;
      }
      case 0x8C: {
        const n = rd32(pc + 1) >>> 0;
        gget(G_VA); i32c(n); op1(0x6A); gget(G_VL); op1(0x4E);
        IF(); STOREv(); setPC(pc); i32c(pc); call(F_GC); LOAD(); END();
        break;
      }
      case 0xC9: case 0xCA: {
        // u8 ref / set: a = obj + (idx>>1) - 5, bounds-checked like the VM
        if (op === 0xC9) { get(b2); get(b3); } else { get(b1); get(b2); }
        i32c(1); op1(0x75); op1(0x6A); i32c(-5); op1(0x6A); lset(LT1);
        lget(LT1); i32c(VBASE); op1(0x48); lget(LT1); i32c(vm.heapEnd); op1(0x4E); op1(0x72);
        IF(); delegate(pc); ELSE();
          if (op === 0xC9) { lget(LT1); adr(); o.push(0x2D, 0x00, 0x00); i32c(1); op1(0x74); set(b1); }
          else { lget(LT1); adr(); get(b3); i32c(1); op1(0x75); o.push(0x3A, 0x00, 0x00); }
        END();
        break;
      }
      case 0x23: case 0x24: {
        get(b3); i32c(1); op1(0x75); ltee(LT2); op1(0x45);
        IF(); delegate(pc); ELSE();
          get(b2); i32c(1); op1(0x75); lget(LT2); op1(op === 0x23 ? 0x6D : 0x6F); i32c(1); op1(0x74); set(b1);
        END();
        break;
      }
      case 0x53: case 0x54: {
        const d = op === 0x53 ? -1 : 3;
        get(b1); i32c(7); op1(0x71); i32c(1); op1(0x47); IF(); setPC(pc); STOREv(); i32c(pc); call(F_FAULT); END();
        get(b1); adr(d); get(b2); st32(); break;
      }
      case 0x55: case 0x56: {
        // consp: x != NIL && (x & 15) == 1
        get(b2); ltee(LT1); i32c(NIL); op1(0x47); lget(LT1); i32c(15); op1(0x71); i32c(1); op1(0x46); op1(0x71);
        lset(LT1);
        if (op === 0x55) { i32c(TV); i32c(NIL); } else { i32c(NIL); i32c(TV); }
        lget(LT1); op1(0x1B); set(b1); break;
      }
      case 0x61:
        if (b2 === 21) { lget(LFB); ld32(slotOff(b3) + FB_BIAS); }
        else { get(b2); adr(4 * b3 - 5); ld32(); }
        set(b1); break;
      case 0x62:
        if (b1 === 21) { lget(LFB); get(b3); st32(slotOff(b2) + FB_BIAS); }
        else { get(b1); adr(4 * b2 - 5); get(b3); st32(); }
        break;
      case 0x63: get(b2); i32c(15); op1(0x71); i32c(1); op1(0x74); set(b1); break;
      case 0x64: case 0x67: {
        // header-derived: subtag (0x64) or count (0x67) of a tag-9 object, else 0
        get(b2); lset(LT1);
        lget(LT1); i32c(15); op1(0x71); i32c(9); op1(0x46);
        lget(LT1); i32c(TV); op1(0x47); op1(0x71);
        lget(LT1); i32c(-9 - VBASE); op1(0x6A); i32c(vm.heapEnd - VBASE); op1(0x49); op1(0x71);
        IFv(I32);
        lget(LT1); adr(-9); ld32();
        if (op === 0x64) { i32c(255); op1(0x71); } else { i32c(8); op1(0x76); }
        i32c(1); op1(0x74);
        ELSE(); i32c(0); END();
        set(b1); break;
      }
      case 0x65:
        if (b2 === 21) {
          // frame slot k = x>>1: k < 129 ? SLOT_BASE - 4k : -FRAME_SIZE - 4(k-129)
          get(b3); i32c(1); op1(0x75); lset(LT1);
          lget(LFB);
          i32c(slotOff(0)); lget(LT1); i32c(4); op1(0x6C); op1(0x6B);
          i32c(-FRAME_SIZE + 4 * 129); lget(LT1); i32c(4); op1(0x6C); op1(0x6B);
          lget(LT1); i32c(129); op1(0x48); op1(0x1B);
          op1(0x6A); ld32(FB_BIAS);
        } else { get(b2); get(b3); i32c(1); op1(0x74); op1(0x6A); adr(-5); ld32(); }
        set(b1); break;
      case 0x66: get(b1); get(b2); i32c(1); op1(0x74); op1(0x6A); adr(-5); get(b3); st32(); break;
      case 0x70: {
        const w = b3 & 3;
        get(b2); adr();
        if (w === 0) o.push(0x2D, 0x00, 0x00); else if (w === 1) o.push(0x2F, 0x01, 0x00); else ld32();
        set(b1); break;
      }
      case 0x71: {
        const w = b3 & 3;
        get(b1); adr(); get(b2);
        if (w === 0) o.push(0x3A, 0x00, 0x00); else if (w === 1) o.push(0x3B, 0x01, 0x00); else st32();
        break;
      }
      case 0x80: case 0x81: {
        const resumeL = label.get(pc + len);
        STOREv(); setPC(pc);
        if (op === 0x80 && NOFAST) {
          const t = rd32(pc + 1) >>> 0;
          i32c(t); i32c(pc); call(F_DCALL); lset(LT1);
          lget(LT1); i32c(2); op1(0x46);
          IF(); i32c(t); i32c(resumeL); call(F_CALLREQ); i32c(t * 2); RET(); END();
        } else if (op === 0x80) {
          const t = rd32(pc + 1) >>> 0;
          const { T, S, F } = calleeCache(t);
          fastCall(T, S, F);
          ELSE();
          i32c(t); i32c(pc); call(F_DCALL); lset(LT1);
          lget(LT1); i32c(2); op1(0x46);
          IF(); i32c(t); i32c(resumeL); call(F_CALLREQ); i32c(t * 2); RET(); END();
        } else if (!NOFAST) {
          // CALL-IND: cache (function object, slot, frame size) per site.
          const gT = nglob++, gS = nglob++, gF = nglob++;
          get(b1); lset(LT2);
          lget(LT2); gget(gT); op1(0x47);
          IF();
            lget(LT2); i32c(15); op1(0x71); i32c(3); op1(0x46);
            IF();
              lget(LT2); i32c(3); op1(0x6B); i32c(4); op1(0x76); ltee(LT1); call(F_SLOTFOR); gset(gS);
              lget(LT1); call(F_FBYTES); gset(gF);
              gget(gS); i32c(0); op1(0x4E); IF(); lget(LT2); gset(gT); ELSE(); i32c(-1); gset(gT); END();
            ELSE(); i32c(-1); gset(gT); END();
          END();
          gget(gT); lget(LT2); op1(0x46);
          fastCall(() => { lget(LT2); i32c(3); op1(0x6B); i32c(4); op1(0x76); }, () => gget(gS), () => gget(gF));
          ELSE();
          get(b1); i32c(pc); call(F_DCALLIND); ltee(LT2); i32c(3); op1(0x71); lset(LT1);
          lget(LT1); i32c(2); op1(0x46);
          IF(); lget(LT2); i32c(2); op1(0x76); ltee(LT3); i32c(resumeL); call(F_CALLREQ);
               lget(LT3); i32c(1); op1(0x74); RET(); END();
        } else {
          get(b1); i32c(pc); call(F_DCALLIND); ltee(LT2); i32c(3); op1(0x71); lset(LT1);
          lget(LT1); i32c(2); op1(0x46);
          IF(); lget(LT2); i32c(2); op1(0x76); ltee(LT3); i32c(resumeL); call(F_CALLREQ);
               lget(LT3); i32c(1); op1(0x74); RET(); END();
        }
        lget(LT1); IF(); LOAD(4, 15); END();
        if (!NOFAST) END();
        break;
      }
      case 0x82:
        // doRet: esp = ebp; ebp = [esp]; pc = [esp+4]; esp += 8
        gget(G_EBP); ltee(LT1); adr(); ld32(); gset(G_EBP);
        lget(LT1); adr(4); ld32(); gset(G_PC);
        lget(LT1); i32c(8); op1(0x6A); gset(G_ESP);
        i32c(-1); RET(); break;
      case 0x83: {
        // doTailcall in place: the frame base stays (the saved ebp and return
        // slot are re-pushed where they were), esp = ebp - frame size, V0-V4
        // to the new frame's homes; then return_call_indirect if T is Wasm,
        // else hand T to the driver as before.
        const t = rd32(pc + 1) >>> 0;
        const setup = (T, F) => {
          gget(G_EBP); F(); op1(0x6B); gset(G_ESP);
          STOREv(0, 4, true);
          T(); gset(G_PC);
        };
        if (!NOFAST && !NOTAIL) {
          const { T, S, F } = calleeCache(t);
          IF();
            setup(T, F);
            i32c(0); S(); o.push(0x13); uleb(o, FNTYPE); o.push(0x00);
          END();
        }
        setup(() => i32c(t), () => i32c(C.frameBytes(m8, t)));
        i32c(t * 2 + 1); RET(); break;
      }
      case 0x89:
        gget(G_VA); gget(G_VL); op1(0x4F); IF(); STOREv(); setPC(pc); i32c(pc); call(F_GC); LOAD(); END();
        break;
      case 0xA7: { const t = rd32(pc + 2) >>> 0; i32c(t === FN_UNRESOLVED ? NIL : ((t << 4) | 3)); set(b1); break; }
      case 0xAE: case 0xAF: {
        get(b2); lset(LT1); get(b3); lset(LT2);
        lget(LT1); lget(LT2); op1(op === 0xAE ? 0x6A : 0x6B); lset(LT3);
        if (op === 0xAE) { lget(LT1); lget(LT3); op1(0x73); lget(LT2); lget(LT3); op1(0x73); op1(0x71); }
        else { lget(LT1); lget(LT2); op1(0x73); lget(LT1); lget(LT3); op1(0x73); op1(0x71); }
        i32c(0); op1(0x48);
        IF(); STOREv(); setPC(pc); i32c(op); lget(LT1); lget(LT2); call(F_CHECKED); lset(LT3); LOAD(); END();
        lget(LT3); set(b1); break;
      }
      case 0xAD: {
        get(b2); lset(LT1); get(b3); lset(LT2);
        lget(LT1); i32c(1); op1(0x75); lget(LT2); op1(0x6C); lset(LT3);
        // overflow: (i64)(a>>1) * (i64)b != (i64)r
        lget(LT1); i32c(1); op1(0x75); op1(0xAC); lget(LT2); op1(0xAC); op1(0x7E);
        lget(LT3); op1(0xAC); op1(0x52);
        IF(); STOREv(); setPC(pc); i32c(op); lget(LT1); lget(LT2); call(F_CHECKED); lset(LT3); LOAD(); END();
        lget(LT3); set(b1); break;
      }
      case 0xB8: i32c(A_MVCOUNT - VBASE); i32c(b1 << 1); st32(); break;
      case 0xBA: i32c(A_CENV - VBASE); get(b1); st32(); break;
      case 0xBB: i32c(A_CENV - VBASE); ld32(); set(b1); break;
      case 0xBC: i32c(A_NARGS - VBASE); i32c(b1); st32(); break;
      case 0xBD: i32c(A_NARGS - VBASE); ld32(); i32c(1); op1(0x74); set(b1); break;
      case 0xC5: case 0xC6: {
        get(b2); lset(LT1); get(b3); lset(LT2);
        lget(LT1); lget(LT2); op1(op === 0xC5 ? 0x6A : 0x6B); lset(LT3);
        if (op === 0xC5) { lget(LT1); lget(LT3); op1(0x73); lget(LT2); lget(LT3); op1(0x73); op1(0x71); }
        else { lget(LT1); lget(LT2); op1(0x73); lget(LT1); lget(LT3); op1(0x73); op1(0x71); }
        i32c(0); op1(0x48); lset(LOVF);
        lget(LT3); set(b1); break;
      }
      case 0xC7: lget(LOVF); jumpTo(pc + len + rd32(pc + 1), true); break;
      default:
        if ((op >= 0x40 && op <= 0x48) || op === 0x80 || op === 0x81 || op === 0x82 || op === 0x83 || op === 0xC7) { W.stats.failed++; return null; }
        delegate(pc);
        break;
    }
    // a block whose last instruction falls through to a non-label is fine; a
    // transfer leaves the rest of the block unreachable, which Wasm accepts.
  }
  const ljCatch = () => {
    o.push(0x00);               // the dispatch never falls out
    o.push(0x07); uleb(o, 0);   // catch $lj: [esp ip]
    lset(LT2); o.push(0x1A);
    call(F_SYNCIN);
    {
      // this frame, and any label is a landing (as drive()'s RES): ask JS
      i32c(resId); lget(LT2); fbArg(); call(F_LJLABEL); ltee(LT1); i32c(0); op1(0x4E);
      o.push(0x04, 0x40);
        lget(LDEP); gset(G_DEP); if (!chunked) LOAD();
        lget(LT1); lset(LP);
        o.push(0x0C, 0x02);     // br $re: 0 = this if, 1 = the try, 2 = loop $re
      o.push(0x0B);
    }
    o.push(0x09, 0x00);         // rethrow
    o.push(0x0B);               // end try
    o.push(0x0B);               // end loop $re
  };
  let entryBody;
  if (!chunked) {
    o.push(0x0B);               // end of the last label's code... closes the loop
    if (hasSetjmp) ljCatch();
    o.push(0x00);               // unreachable: fell off the end
    o.push(0x0B);               // end of function
    entryBody = o;
  } else {
    endChunk(undefined);
    // the entry: loop { R = chunk[LP / CHL](LP); R < -1 ? LP = -(R+2) : return R }
    const K = bodies.length, F0 = IMPORTS.length + 1;
    o = [];
    gget(G_EBP); i32c(-VBASE - FB_BIAS); op1(0x6A); lset(LFB);
    if (hasSetjmp && ENV.MVM_WASM_DEBUG) { i32c(resId); fbArg(); lget(LP); call(F_DBGENTER); }
    if (hasSetjmp) { gget(G_DEP); lset(LDEP); o.push(0x03, 0x40, 0x06, 0x40); }
    o.push(0x03, 0x40);                     // loop $d
    o.push(0x02, I32);                      // block $chk (result i32)
    for (let k = 0; k < K; k++) o.push(0x02, 0x40);
    lget(LP); i32c(CHL); op1(0x6E);         // i32.div_u
    o.push(0x0E); uleb(o, K); for (let k = 0; k < K; k++) uleb(o, k); uleb(o, K - 1);
    for (let k = 0; k < K; k++) {
      o.push(0x0B);                         // end block k
      lget(LP); call(F0 + k);
      if (k < K - 1) { o.push(0x0C); uleb(o, K - 1 - k); }
    }
    o.push(0x0B);                           // end $chk: R
    ltee(LT1); i32c(-1); op1(0x48);
    o.push(0x04, 0x40); i32c(-2); lget(LT1); op1(0x6B); lset(LP); o.push(0x0C, 0x01); o.push(0x0B);
    lget(LT1); RET();
    o.push(0x0B);                           // end loop $d
    if (hasSetjmp) ljCatch();
    o.push(0x00, 0x0B);
    entryBody = o;
  }
  const allBodies = [entryBody, ...bodies];

  const tCode = performance.now(); W.stats.codeMs = (W.stats.codeMs || 0) + (tCode - tCfg);
  // 3. module
  const mod = [0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00];
  // types: one per distinct import signature, plus the function (i32)->i32
  const sigs = [{ k: [I32].join(',') + '>' + [I32].join(','), p: [I32], r: [I32] }]; const sigKey = (p, r) => p.join(',') + '>' + r.join(',');
  const typeOf = (p, r) => { const k = sigKey(p, r); let i = sigs.findIndex((s) => s.k === k); if (i < 0) { i = sigs.length; sigs.push({ k, p, r }); } return i; };
  const impTypes = IMPORTS.map(([, p, r]) => typeOf(p, r));
  const fnType = FNTYPE;
  const ljType = typeOf([I32, I32], []);
  { const b = []; uleb(b, sigs.length); for (const s of sigs) { b.push(0x60); uleb(b, s.p.length); b.push(...s.p); uleb(b, s.r.length); b.push(...s.r); } section(mod, 1, b); }
  { const b = []; uleb(b, 3 + GLOBALS.length + IMPORTS.length);
    str(b, 'e'); str(b, 'm'); b.push(0x02, 0x01); uleb(b, vm.wmem.buffer.byteLength >>> 16); uleb(b, vm.wmem.buffer.byteLength >>> 16);
    for (const n of GLOBALS) { str(b, 'e'); str(b, 'g_' + n); b.push(0x03, I32, 0x01); }
    str(b, 'e'); str(b, 't'); b.push(0x01, 0x70, 0x00, 0x00);
    str(b, 'e'); str(b, 'lj'); b.push(0x04, 0x00); uleb(b, ljType);
    IMPORTS.forEach(([n], i) => { str(b, 'e'); str(b, n); b.push(0x00); uleb(b, impTypes[i]); });
    section(mod, 2, b); }
  { const b = []; uleb(b, allBodies.length); for (let k = 0; k < allBodies.length; k++) uleb(b, fnType); section(mod, 3, b); }
  if (nglob > GLOBALS.length) {
    const b = []; uleb(b, nglob - GLOBALS.length);
    for (let k = GLOBALS.length; k < nglob; k++) { b.push(I32, 0x01, 0x41); sleb(b, -1); b.push(0x0B); }
    section(mod, 6, b);
  }
  { const b = []; uleb(b, 1); str(b, 'f'); b.push(0x00); uleb(b, IMPORTS.length); section(mod, 7, b); }
  // The code section is sized first and every body copied ONCE, straight
  // into the module's bytes: pushing each byte through three JS arrays was
  // most of the translator's time (7.9 of 13 s on web-mvm-calls).
  const hdrs = allBodies.map((ob) => { const h = []; uleb(h, ob.length + 3); h.push(1); uleb(h, NLOCALS); h.push(I32); return h; });
  let codeLen = 0; { const t = []; uleb(t, allBodies.length); codeLen = t.length; }
  for (let k = 0; k < allBodies.length; k++) codeLen += hdrs[k].length + allBodies[k].length;
  mod.push(10); uleb(mod, codeLen); uleb(mod, allBodies.length);
  const bytes = new Uint8Array(mod.length + codeLen - (() => { const t = []; uleb(t, allBodies.length); return t.length; })());
  bytes.set(mod, 0);
  { let at = mod.length;
    for (let k = 0; k < allBodies.length; k++) { bytes.set(hdrs[k], at); at += hdrs[k].length; bytes.set(allBodies[k], at); at += allBodies[k].length; }
    if (at !== bytes.length) throw new Error(`wasm32: module size ${at} != ${bytes.length}`); }

  let inst;
  const tA = performance.now();
  try { const mm = new WebAssembly.Module(bytes); const tB = performance.now(); W.stats.modMs = (W.stats.modMs || 0) + (tB - tA); inst = new WebAssembly.Instance(mm, W.imports); W.stats.instMs = (W.stats.instMs || 0) + (performance.now() - tB); W.stats.emitMs = (W.stats.emitMs || 0) + (tA - t0); }
  catch (e) { W.stats.failed++; if (ENV.MVM_WASM_DEBUG && W.stats.failed < 6) LOG(`[wasm compile ${vm.where(entry)}: ${e.message}]\n`); return null; }
  if (hasSetjmp) W.stats.sj = (W.stats.sj || 0) + 1;
  const run = inst.exports.f;
  const fn = function (vm, L) {
    if (ENV.MVM_WASM_DEBUG) {
      W.depth = (W.depth || 0) + 1; (W.hist = W.hist || []).push(vm.where(entry) + ' L' + L + ' jd' + vm.jd);
      try { return fn2(vm, L); }
      catch (e) { if (e instanceof RangeError && !W.logged) { W.logged = 1; W.dump = 'wrapper depth ' + W.depth + ' jd ' + vm.jd + ' dep ' + W.g.dep.value + '\n' + W.hist.slice(-12).join('\n'); } throw e; }
      finally { W.depth--; W.hist.pop(); }
    }
    return fn2(vm, L);
  };
  const fn2 = function (vm, L) {
    W.entries = (W.entries || 0) + 1;
    W.syncIn();
    let r;
    const dep0 = W.g.dep.value;
    try { r = run(L); }
    catch (e) {
      W.g.dep.value = dep0;          // a longjmp unwound Wasm frames that did not count down
      if (e instanceof WebAssembly.RuntimeError) {
        W.syncOut();
        // `unreachable' is the translator's own bug, never a Lisp error: a hard
        // fault, so it cannot be caught by a HANDLER-CASE and pass for one (a
        // bad branch depth in the setjmp catch once did exactly that).
        if (/unreachable/.test(e.message)) vm.fault(`wasm: ${e.message} in ${vm.where(entry)}`);
        vm.memFault('wasm: ' + e.message);
      }
      throw e;
    }
    W.syncOut();
    return r;
  };
  if (hasSetjmp) { fn.RES = {}; for (const [pc, l] of label) fn.RES[pc] = l; }
  fn.ENTRYL = label.get(entry);
  fn.wexp = run;
  W.stats.fns++; W.stats.bytes += bytes.length;
  W.stats.ms += (typeof performance !== 'undefined' ? performance.now() : Date.now()) - t0;
  return fn;
}

// compiledFor() kept a Wasm function for T: put it in T's table slot.
// Not a function with setjmp landings: a longjmp re-enters those through drive().
function install(vm, t, fn) { const W = state(vm); W.table.set(W.slotOf(t), fn.wexp); }
module.exports = { init, compile, state, install };
