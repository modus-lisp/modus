'use strict';
// mvm32.js — the JavaScript MVM at 32-bit words.
//
// The same machine as mvm.js (which see for the history of every arm), built
// for a module compiled with the i386 COMPILE TARGET (build-web.lisp,
// MODUS_WEB_WORD=4): one tagged value is one int32, so a register is one JS
// number and a memory word one Int32Array slot -- no (lo, hi) pairs, no carry
// propagation, no 64-bit multiply.  Fixnums are 30 bits (tagged in 31), and
// arithmetic overflows at 32 bits exactly as translate-i386's `jo` does.
//
// Object layout is the i386 one: a 4-byte header (count << 8) | subtag, slot I
// at raw + 4*(I+1) = tagged - 5 + 4*I, byte / lane payloads at tagged - 5, a
// cons a 16-byte granule with car at +0 and cdr at +4.  Everything the
// compiled code does NOT compute addresses for -- frames, the handler stack,
// the collector -- is this file's own business, as in mvm.js.
//
// The HOST ABI is mvm.js's (Linux x86-64 syscall numbers, argv on the initial
// stack in 8-byte slots): the image's file / clock / argv readers use
// promoting :u32 loads at those offsets, which are exact on a 32-bit target.

const NIL = 0xDEAD0001 | 0;
const TV  = 0xDEAD1009 | 0;

// ---- virtual memory layout (as mvm.js) ------------------------------------
const VBASE      = 0x10000000;
const BSS_END    = 0x10020000;
const POOL_ADDR  = 0x10020000;
const CODE_ADDR  = 0x10400000;
const JIT_ADDR   = 0x12400000;
const JIT_END    = 0x13400000;
const STACK_ADDR = 0x13400000;
const STACK_SIZE = 0x00800000;
const ARGV_AREA  = 0x00010000;
const HEAP_ADDR  = 0x13C00000;
// Every address must stay below 2^30: a word reaches Lisp through a fixnum
// (SHL 1 / SAR 1), and the 30-bit tower cannot carry one at or above it.
const ADDR_LIMIT = 0x40000000;
const CODE_PHYS  = CODE_ADDR - VBASE, JIT_PHYS = JIT_ADDR - VBASE;
const A_WEB_CONSTS = 0x10000D40, A_WEB_RELOCS = 0x10000D48, A_WEB_RELOC_STATUS = 0x10000D50;

const INSN_LEN = new Uint8Array(256);
for (const [ops, n] of [[[0x00,0x01,0x72,0x82,0x89,0x8B,0x92,0xA2,0xA3,0xA4],1],[[0x02,0x10,0x25,0x30,0x31,0x50,0x51,0x53,0x54,0x55,0x56,0x63,0x64,0x67,0x68,0xB0,0xB7,0xB9,0xC2,0xC3,0xC4,0xC8],3],
  [[0x11,0x14],10],[[0x12,0x13,0x26,0x27,0x81,0x88,0x8A,0x90,0x91,0xB8,0xBA,0xBB,0xBC,0xBD],2],
  [[0x20,0x21,0x22,0x23,0x24,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2F,0x32,0x52,0x61,0x62,0x65,0x66,0x70,0x71,0x93,0xA5,0xA6,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xBE,0xBF,0xC0,0xC1,0xC5,0xC6,0xC9,0xCA],4],
  [[0x2E,0x60,0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x80,0x83,0xA0,0xA1,0xC7],5],[[0x47,0x48,0xA7,0x33,0x34],6]]) for (const o of ops) INSN_LEN[o] = n;
for (const [ops, n] of [[[0x8D,0xCB,0xD0,0xD1,0xD2,0xDB,0xE1,0xF4],3],
  [[0xCC,0xCD,0xCE,0xCF,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDC,0xDD,0xDE,0xDF,0xE0,0xE4,0xE5,0xE8],4],
  [[0x8C,0xE2,0xE3,0xE6,0xE7,0xE9],5],[[0xEA],6]]) for (const o of ops) INSN_LEN[o] = n;
for (const [ops, n] of [[[0xEB,0xEC,0xF1,0xF9],3],[[0xED,0xEE,0xEF,0xF0,0xF2,0xF3,0xF5,0xF8],4],
  [[0xF6,0xF7],5],[[0xFA,0xFB],10]]) for (const o of ops) INSN_LEN[o] = n;
// W32 (docs/w32.md): raw 32-bit words, emitted for 32-bit targets only.
for (const [ops, n] of [[[0x74,0x7C,0x7D,0x7E],3],[[0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,0x73,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7F,0x84,0x85],4],
  [[0x69],6]]) for (const o of ops) INSN_LEN[o] = n;
const BACK_BRANCH = new Uint8Array(256);
for (const o of [0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0xC7]) BACK_BRANCH[o] = 1;
// Environment: node's process.env, or in a browser worker the MVM_ENV object
// worker.js sets on its global (the URL's ?wasm= / MVM_* parameters).
const ENV = typeof process !== 'undefined' && process.env ? process.env : ((typeof self !== 'undefined' && self.MVM_ENV) || {});
const OSR_THRESHOLD = (ENV.MVM_OSR_THRESHOLD && parseInt(ENV.MVM_OSR_THRESHOLD, 10)) || 500;
const NO_OSR = !!ENV.MVM_NO_OSR;
const NO_EXEC = new Uint8Array(256);
for (const o of [0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,0xE0,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xAA,0xAB,0xAC]) NO_EXEC[o] = 1;
const ALLOC_START_OFF = 0x400;
const GUARD      = 0x01000000;

const A_GC_FROM   = 0x10000040, A_GC_TO = 0x10000048, A_GC_SIZE = 0x10000050,
      A_GC_STACKB = 0x10000058, A_GC_COUNT = 0x10000060;
const A_MVCOUNT   = 0x10000090;
const A_CENV      = 0x10000140;
const A_NARGS     = 0x10000150;
const A_JMPBUF    = 0x10000180;           // 4 words, 8-byte stride kept: esp ebp ip V4
const A_ARGC      = 0x10000200, A_ARGV1 = 0x10000208, A_ARGV2 = 0x10000248;
const A_HDEPTH    = 0x10000400, A_HSTACK = 0x10000408, HMAX = 64, JMPBUF_WORDS = 4;
const A_HOVF      = 0x10000D20;
const A_CLOCK_EPOCH = 0x10000FB8;         // translate-i386's #x0310 first-read seconds

// ---- frames (this file's own layout) ---------------------------------------
// [ebp] saved ebp, [ebp+4] return pc, stack arguments 5.. at ebp+8+4*(i-4).
// Below ebp: the homes of V4 V0 V1 V2, V9..V15, frame slots 0..128, then the
// homes of V3 V5..V8.  A sized FRAME-ENTER naming more slots grows the frame
// below the homes.
const FRAME_SIZE = 584;
function frameEnterParams(c) { return c >= 0x8000 ? c & 0xFF : c; }
function frameBytes(code, target) {
  if (code[target] !== 0x02) return FRAME_SIZE;
  const c = code[target + 1] | (code[target + 2] << 8);
  if (c < 0x8000) return FRAME_SIZE;
  const slots = ((c >> 8) & 0x3F) * 8;
  return slots > 128 ? FRAME_SIZE + 4 * (slots - 128) : FRAME_SIZE;
}
const SLOT_BASE  = -48;
const SLOT_SPLIT = 129;
function slotOff(i) { return i < SLOT_SPLIT ? SLOT_BASE - 4 * i : -FRAME_SIZE - 4 * (i - SLOT_SPLIT); }
const ROFF = new Int32Array(16);
ROFF[4] = -4; ROFF[0] = -8; ROFF[1] = -12; ROFF[2] = -16;
for (let v = 9; v <= 15; v++) ROFF[v] = -20 - 4 * (v - 9);
ROFF[3] = -564; ROFF[5] = -568; ROFF[6] = -572; ROFF[7] = -576; ROFF[8] = -580;
const IX = new Int32Array(16); for (let v = 0; v < 16; v++) IX[v] = ROFF[v] >> 2;
const IX0 = IX[0], IX1 = IX[1], IX2 = IX[2], IX3 = IX[3], IX4 = IX[4];

const RET_SENTINEL = -1;
const SHAPES = !!ENV.MVM_SHAPES;
// Nested drive()s a direct call may stack on the JS stack before a call falls
// back to returning to its driver (deep Lisp recursion must not overflow it).
const JD_MAX = ENV.MVM_JD_MAX ? parseInt(ENV.MVM_JD_MAX, 10) : 400;
const DBG = ENV;
const FN_UNRESOLVED = 0xFFFFFFF0;

class LongJmp { constructor(esp, ip) { this.esp = esp; this.ip = ip; } }
// With Wasm code on the stack a longjmp is a WebAssembly.Exception of this tag
// (Wasm cannot catch a JS object); ljOf() turns either form into a LongJmp.
const LJ_TAG = typeof WebAssembly !== 'undefined' && WebAssembly.Tag ? new WebAssembly.Tag({ parameters: ['i32', 'i32'] }) : null;
function ljOf(e) {
  if (e instanceof LongJmp) return e;
  if (LJ_TAG && e instanceof WebAssembly.Exception && e.is(LJ_TAG)) return new LongJmp(e.getArg(LJ_TAG, 0), e.getArg(LJ_TAG, 1));
  return null;
}
class MvmExit { constructor(code) { this.code = code; } }
class MvmFault extends Error {}

function align16(n) { return (n + 15) & ~15; }
// Value-level variable shifts (the interpreter's contract): count untagged,
// and a count past the word saturates, so compile-ash's shift-back check fails
// and takes the slow path instead of trusting a 5-bit-masked shift.
function shlv(x, n) { return n >= 32 ? 0 : n <= 0 ? x : (x << n); }
function sarv(x, n) { return n >= 32 ? (x >> 31) : n <= 0 ? x : (x >> n); }
// Immediate shifts saturate at >= 32, as translate-i386 emits them (a raw x86
// shift masks the count to 5 bits, and (ash x -32) would be a shift by 0).
function shli(x, n) { return n >= 32 ? 0 : x << n; }
function shri(x, n) { return n >= 32 ? 0 : (x >>> n) | 0; }
function ldbMask(size) { return size >= 32 ? -1 : ((1 << size) - 1) | 0; }

function loadModule(bytes) {
  const dv = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let p = 0;
  const u32 = () => { const v = dv.getUint32(p, true); p += 4; return v; };
  const u16 = () => { const v = dv.getUint16(p, true); p += 2; return v; };
  if (String.fromCharCode(bytes[0], bytes[1], bytes[2], bytes[3]) !== 'MVMW')
    throw new Error('not an MVMW module');
  p = 4;
  const version = u32(), wordSize = u32();
  if (version !== 1 || wordSize !== 4) throw new Error('unsupported MVMW version/word size (mvm32.js needs the 32-bit module)');
  const bcLen = u32();
  const code = bytes.subarray(p, p + bcLen); p += bcLen;
  const nfn = u32();
  const fns = new Array(nfn);
  for (let i = 0; i < nfn; i++) {
    const hash = u32(), params = u32(), off = u32(), len = u32();
    const nl = u16();
    let name = '';
    for (let j = 0; j < nl; j++) name += String.fromCharCode(bytes[p + j]);
    p += nl;
    fns[i] = { hash, params, off, len, name };
  }
  const npool = u32();
  const addrTable = new Int32Array(npool);
  for (let i = 0; i < npool; i++) addrTable[i] = u32();
  const poolLen = u32();
  const pool = bytes.subarray(p, p + poolLen); p += poolLen;
  const byName = new Map();
  for (const f of fns) byName.set(f.name, f);
  const sorted = fns.slice().sort((a, b) => a.off - b.off);
  return { code, fns, byName, sorted, addrTable, pool };
}

// Subtags whose payload holds no pointers: the typed to-space scan skips them.
// translate-i386's collector's list (i386-emit-gcnative-typed-walk), plus the
// wide string #x31, whose slots are character codes.
const LEAF = new Uint8Array(256);
for (const s of [0x10, 0x11, 0x12, 0x13, 0x14, 0x16, 0x31, 0x60, 0x64, 0x65, 0x66]) LEAF[s] = 1;
// The count field is BYTES for a u8 vector (#x11) and a narrow string (#x13, a
// u8 vector with its subtag rewritten), WORDS for everything else.  Sizing a
// narrow string in words copied 4x its size and made the to-space walk step
// over the objects after it unscanned: their pointers stayed in from-space
// (a forwarding word, `#<?F239>', read back as a value), intermittently,
// whenever a collection found a live narrow string.
function objBytes(hdr) {
  const st = hdr & 0xFF, n = hdr >>> 8;
  return (st === 0x11 || st === 0x13) ? align16(4 + n) : align16(4 + 4 * n);
}

class MVM {
  constructor(mod, host, opts = {}) {
    this.mod = mod;
    this.host = host;
    this.trace = opts.trace || 0;
    this.maxSteps = opts.maxSteps || 0;
    this.traceFrom = opts.traceFrom || 0; this.traceCount = opts.traceCount || 0; this.traceRegs = !!opts.traceRegs;
    this.prof = opts.profile ? new Map() : null;
    this.debug = !!opts.debug;
    this.semi = opts.semispace || (256 << 20);
    this.stackTop = STACK_ADDR + STACK_SIZE - ARGV_AREA;
    this.heapBase = HEAP_ADDR;
    this.heapEnd = HEAP_ADDR + 2 * this.semi + GUARD;
    if (this.heapEnd > ADDR_LIMIT) throw new Error(`semispace ${this.semi >> 20} MB puts the heap past 2^30 (the 30-bit word limit)`);
    // The two GC bitmaps (1 bit / 16-byte granule) follow the heap in the same
    // buffer, so translated Wasm code can mark an allocation inline.
    const bmpBytes = (((this.heapEnd - this.heapBase) >> 4) >> 3) + 16;
    this.bmpOff = (this.heapEnd - VBASE + 15) & ~15;
    // and the sixteen float registers, so translated Wasm reads and writes
    // them where the VM's own float ops do
    this.frOff = (this.bmpOff + 2 * bmpBytes + 15) & ~15;
    const size = this.frOff + 128;
    // A WebAssembly.Memory when the engine has one, so translated Wasm code
    // (wasm32.js, MVM_WASM=1) and this VM share the same bytes.
    if (typeof WebAssembly !== 'undefined') {
      const pages = Math.ceil(size / 65536);
      this.wmem = new WebAssembly.Memory({ initial: pages, maximum: pages });
      this.buf = this.wmem.buffer;
    } else this.buf = new ArrayBuffer(size);
    this.m8 = new Uint8Array(this.buf);
    this.m32 = new Int32Array(this.buf);
    this.dv = new DataView(this.buf);
    this.f64 = new Float64Array(1);
    this.f64u16 = new Uint16Array(this.f64.buffer);
    this.f64u32 = new Uint32Array(this.f64.buffer);
    this.fr = new Float64Array(this.buf, this.frOff, 16);
    const granules = (this.heapEnd - this.heapBase) >> 4;
    if (DBG.MVM_OLD_BMP) { this.startBmp = new Uint8Array((granules >> 3) + 1); this.consBmp = new Uint8Array((granules >> 3) + 1); } else {
    this.startBmp = new Uint8Array(this.buf, this.bmpOff, (granules >> 3) + 1);
    this.consBmp = new Uint8Array(this.buf, this.bmpOff + bmpBytes, (granules >> 3) + 1); }
    this.consBmpOff = this.bmpOff + bmpBytes;
    this.pageBase = this.heapBase + ALLOC_START_OFF;
    this.mmapNext = JIT_ADDR;
    this.code = this.m8;
    this.fnsPhys = mod.fns.map((f) => ({ ...f, off: f.off + CODE_PHYS }));
    this.byName = new Map(); for (const f of this.fnsPhys) this.byName.set(f.name, f);
    this.sorted = this.fnsPhys.slice().sort((a, b) => a.off - b.off);
    this.gcCount = 0;
    this.steps = 0;
    this.vrl = 0; this.vrh = 0;               // VR; vrh is always 0 (kept for the hosts)
    this.va = 0; this.vl = 0;
    this.esp = 0; this.ebp = 0; this.pc = 0;
    this.cmp = 0; this.ovf = false;
    this.snapAt = -1;
    this.onSnapshot = null;
    this.compiled = new Map();
    this.resumeL = 0;
    this.jd = 0;                              // nested drive() depth (direct calls)
    this.nextF = undefined;
    this.dcount = new Map();
    this.contF = []; this.contL = []; this.contE = []; this.contDepth = 0;
    this.compileOn = opts.compile !== false;
    this.compileThreshold = opts.compileThreshold || (ENV.MVM_COMPILE_THRESHOLD && parseInt(ENV.MVM_COMPILE_THRESHOLD, 10)) || 20;
    this.callCounts = new Map();
    this.osrCounts = new Map(); this.osrCompiled = new Map();
    this.compileStats = { fns: 0, insns: 0, failed: 0, ms: 0, osr: 0, delegated: new Map() };
    this.socks = new Map();
    this.nextSockFd = 1000;
    this.fakeIps = new Map(); this.ipNames = new Map();
    this.watchAt = -1; this.watchLeft = 0;
    // MVM_SHAPES: entering LISP-IMPLEMENTATION-VERSION zeroes the census
    // (clb-all.lisp calls it just before each timed loop).
    this.shapeResetAt = SHAPES && this.byName.get('LISP-IMPLEMENTATION-VERSION') ? this.byName.get('LISP-IMPLEMENTATION-VERSION').off : -2;
    this.shapeResets = 0;
    if (DBG.MVM_CALLERS) { this.callerOf = new Map(); this.callers = new Map();
      for (const n of DBG.MVM_CALLERS.split(',')) { const f = this.byName.get(n); if (f) this.callerOf.set(f.off, n); } }
    this.genAdd = this.byName.get('GENERIC-ADD');
    this.genSub = this.byName.get('GENERIC-SUBTRACT');
    this.genMul = this.byName.get('GENERIC-MULTIPLY');
    // %INTERN-KEYWORD memo (see mvm.js): a fixnum key is GC-stable, the
    // cached keyword is not, so gc() drops the cache.
    this._ikCache = new Map();
    const ik = this.byName.get('%INTERN-KEYWORD');
    if (ik && this.compileOn) {
      const self = this;
      this.compiled.set(ik.off, function (vm) {
        const a0 = vm.m32[((vm.ebp - VBASE) >> 2) + IX0];
        if ((a0 & 1) === 0) {
          const hit = self._ikCache.get(a0);
          if (hit !== undefined) { vm.vrl = hit; vm.doRet(); return -1; }
          vm.run();
          self._ikCache.set(a0, vm.vrl);
          return -1;
        }
        vm.run(); return -1;
      });
    }
    this.initMemory(opts.argv || ['modus'], opts.env || []);
  }

  // -- raw memory -------------------------------------------------------------
  ld32(a) { return this.m32[(a - VBASE) >> 2]; }
  st32(a, v) { this.m32[(a - VBASE) >> 2] = v; }
  ldlo(a) { return this.m32[(a - VBASE) >> 2]; }          // host-facing name
  ldhi(a) { return 0; }
  // An 8-byte host slot (argv, stat, timespec, the BSS words the image reads
  // with :u64): low word, high word zeroed.
  st64(a, lo, hi = 0) { const i = (a - VBASE) >> 2; this.m32[i] = lo; this.m32[i + 1] = hi; }
  ld16(a) { return this.dv.getUint16(a - VBASE, true); }
  st16(a, v) { this.dv.setUint16(a - VBASE, v, true); }
  st8(a, v) { this.m8[a - VBASE] = v; }
  cstr(a) {
    let s = '', i = a - VBASE;
    while (this.m8[i] !== 0) s += String.fromCharCode(this.m8[i++]);
    return s;
  }
  putBytes(a, bytes) { this.m8.set(bytes, a - VBASE); }
  zero(from, to) { this.m8.fill(0, from - VBASE, to - VBASE); }

  stageArgv(argv, env) {
    const enc = (s) => { const b = []; for (let i = 0; i < s.length; i++) b.push(s.charCodeAt(i) & 0xFF); b.push(0); return b; };
    this.st32(A_ARGC, argv.length);
    this.zero(A_ARGV1, A_ARGV1 + 128);
    if (argv.length > 1) this.putBytes(A_ARGV1, enc(argv[1]).slice(0, 63));
    if (argv.length > 2) this.putBytes(A_ARGV2, enc(argv[2]).slice(0, 63));
    const top = STACK_ADDR + STACK_SIZE;
    this.zero(this.stackTop, top);
    const ptrs = [];
    let sp = this.stackTop + 8 * (argv.length + env.length + 3);
    sp = (sp + 15) & ~15;
    for (const s of [...argv, ...env]) {
      const b = enc(s);
      if (sp + b.length + 2 > top) { ptrs.push(0); continue; }
      this.putBytes(sp, b); ptrs.push(sp); sp += (b.length + 1) & ~1;
    }
    let p = this.stackTop;
    const put = (v) => { this.st64(p, v, 0); p += 8; };
    put(argv.length);
    for (let i = 0; i < argv.length; i++) put(ptrs[i]);
    put(0);
    for (let i = 0; i < env.length; i++) put(ptrs[argv.length + i]);
    put(0);
    this.st64(A_GC_STACKB, this.stackTop, 0);
  }
  installModule() {
    if (POOL_ADDR + this.mod.pool.length > CODE_ADDR) throw new Error('constant pool too large');
    if (CODE_PHYS + this.mod.code.length > JIT_PHYS) throw new Error('module bytecode too large');
    this.putBytes(POOL_ADDR, this.mod.pool);
    this.m8.set(this.mod.code, CODE_PHYS);
    if (!this.relocate(CODE_PHYS, this.mod.code.length, CODE_PHYS, false)) throw new Error('module relocation failed');
  }
  relocate(phys, len, base, synthetic) {
    const m8 = this.m8, end = phys + len;
    let p = phys;
    const tab = synthetic ? this.ld32(A_WEB_RELOCS) : 0;
    while (p < end) {
      const op = m8[p], n = INSN_LEN[op];
      if (n === 0) { this.host.log(`[relocate: unknown opcode 0x${op.toString(16)} at phys 0x${p.toString(16)}]`); return false; }
      if (synthetic && NO_EXEC[op]) return false;
      if (op === 0x80 || op === 0x83 || op === 0xA7) {
        const at = p + (op === 0xA7 ? 2 : 1);
        const imm = (m8[at] | (m8[at + 1] << 8) | (m8[at + 2] << 16) | (m8[at + 3] << 24)) >>> 0;
        let t;
        if (imm === FN_UNRESOLVED) t = imm;
        else if (imm >= 0x40000000) {
          if (!synthetic) return false;
          const w = this.ld32(tab - 5 + 4 * (imm - 0x40000000));
          if ((w & 0xF) !== 3) return false;
          t = (w - 3) >>> 4;
        } else t = base + imm;
        m8[at] = t & 0xFF; m8[at + 1] = (t >>> 8) & 0xFF; m8[at + 2] = (t >>> 16) & 0xFF; m8[at + 3] = (t >>> 24) & 0xFF;
      }
      p += n;
    }
    return true;
  }
  initMemory(argv, env) {
    this.stageArgv(argv, env);
    this.installModule();
    const from = this.heapBase + ALLOC_START_OFF;
    const spaceSize = this.semi - ALLOC_START_OFF;
    this.st64(this.heapBase, argv.length, 0);
    this.va = from;
    this.vl = from + spaceSize;
    this.st64(A_GC_FROM, from, 0);
    this.st64(A_GC_TO, this.heapBase + this.semi, 0);
    this.st64(A_GC_SIZE, spaceSize, 0);
    this.st64(A_GC_COUNT, 0, 0);
    this.esp = this.stackTop - 16;
    this.ebp = this.esp;
    this.st32(A_CENV, NIL);
  }

  // -- registers ----------------------------------------------------------------
  rv(v) {
    if (v < 16) return this.m32[((this.ebp - VBASE) >> 2) + IX[v]];
    switch (v) {
      case 16: return this.vrl;
      case 17: return this.va;
      case 18: return this.vl;
      case 19: return NIL;
      case 20: return this.esp;
      case 21: return this.ebp;
      case 22: return this.pc;
    }
    this.fault('bad vreg ' + v);
  }
  setR(v, x) {
    if (v < 16) { this.m32[((this.ebp - VBASE) >> 2) + IX[v]] = x; return; }
    switch (v) {
      case 16: this.vrl = x; return;
      case 17: this.va = x; return;
      case 18: this.vl = x; return;
      case 19: return;
      case 20: this.esp = x; return;
      case 21: this.ebp = x; return;
      case 22: this.pc = x; return;
    }
    this.fault('bad vreg ' + v);
  }
  // mvm.js-compatible names for host code
  rlo(v) { return this.rv(v); }
  rhi(v) { return 0; }
  setReg(v, lo) { this.setR(v, lo); }
  rd32(p) { const c = this.m8; return c[p] | (c[p + 1] << 8) | (c[p + 2] << 16) | (c[p + 3] << 24); }
  push(x) { this.esp -= 4; this.m32[(this.esp - VBASE) >> 2] = x; }
  pop() { const x = this.m32[(this.esp - VBASE) >> 2]; this.esp += 4; return x; }

  // -- diagnostics ----------------------------------------------------------------
  fnAt(pc) {
    const s = this.sorted;
    let lo = 0, hi = s.length - 1, best = null;
    while (lo <= hi) {
      const mid = (lo + hi) >> 1;
      if (s[mid].off <= pc) { best = s[mid]; lo = mid + 1; } else hi = mid - 1;
    }
    return best;
  }
  where(pc = this.pc) {
    if (pc >= JIT_PHYS) return `jit@0x${(pc + VBASE).toString(16)}`;
    const f = this.fnAt(pc);
    return f ? `${f.name}+${pc - f.off}` : `@${pc}`;
  }
  backtrace(max = 30) {
    const out = [];
    let ebp = this.ebp, pc = this.pc;
    for (let i = 0; i < max && ebp >= STACK_ADDR && ebp < this.stackTop; i++) {
      out.push(this.where(pc));
      pc = this.ld32(ebp + 4);
      if (pc === RET_SENTINEL) break;
      ebp = this.ld32(ebp);
    }
    return out;
  }
  profileReport(n = 30) {
    if (!this.prof) return '';
    const calls = [...this.prof.entries()].filter(([k]) => k >= 0).sort((a, b) => b[1] - a[1]).slice(0, n)
      .map(([off, c]) => `${String(c).padStart(10)} ${this.where(off)}`).join('\n');
    const self = [...this.prof.entries()].filter(([k]) => k < 0).sort((a, b) => b[1] - a[1]).slice(0, n)
      .map(([k, c]) => `${String(c * 256).padStart(12)} ${this.where(-k - 1)}`).join('\n');
    return 'CALLS\n' + calls + '\nSELF STEPS\n' + self;
  }
  regDump() {
    const out = [];
    for (let v = 0; v < 9; v++) out.push(`V${v}=${this.describe(this.rv(v))}`);
    out.push(`VR=${this.describe(this.vrl)}`);
    return out.join(' ');
  }
  lispStr(x, _hi, depth = 0) {
    if (depth > 6) return '…';
    if (x === NIL) return 'NIL'; if (x === TV) return 'T';
    const tag = x & 0xF;
    if ((x & 1) === 0) return String(x >> 1);
    if (tag === 5) return `#\\${String.fromCharCode(x >>> 8)}`;
    if (tag === 3) return `#<fn ${this.where((x - 3) >>> 4)}>`;
    if (x < this.heapBase || x >= this.heapEnd) return `#<bad ${(x >>> 0).toString(16)}>`;
    if (tag === 1) {
      const parts = []; let cur = x, n = 0;
      while (cur !== NIL && (cur & 0xF) === 1 && n++ < 12) {
        parts.push(this.lispStr(this.ld32(cur - 1), 0, depth + 1));
        const nx = this.ld32(cur + 3);
        if (nx !== NIL && (nx & 0xF) !== 1) { parts.push('.', this.lispStr(nx, 0, depth + 1)); break; }
        cur = nx;
      }
      if (n >= 12) parts.push('…');
      return '(' + parts.join(' ') + ')';
    }
    if (tag === 9) {
      const h = this.ld32(x - 9), st = h & 0xFF, n = h >>> 8;
      const slot = (i) => this.ld32(x - 5 + 4 * i);
      if (st === 0x31) { let s = ''; for (let i = 0; i < Math.min(n, 40); i++) s += String.fromCharCode(slot(i) >> 1); return JSON.stringify(s); }
      if (st === 0x50 || st === 0x53) { const nm0 = slot(2); const nm = ((nm0 & 0xF) === 9 && nm0 >= this.heapBase) ? this.lispStr(nm0, 0, depth + 1).replace(/^"|"$/g, '') : ('#' + (slot(0) >> 1)); return (st === 0x53 ? ':' : '') + nm; }
      return `#<obj ${st.toString(16)} n=${n}>`;
    }
    return `#<0x${(x >>> 0).toString(16)}>`;
  }
  describe(x) {
    if (x === NIL) return 'NIL'; if (x === TV) return 'T';
    const tag = x & 0xF;
    if ((x & 1) === 0) return `fix ${x >> 1}`;
    if (tag === 5) return `char ${x >>> 8}`;
    if (tag === 3) return `fn ${this.where((x - 3) >>> 4)}`;
    if (x < VBASE || x >= this.heapEnd) return `bad 0x${(x >>> 0).toString(16)}`;
    if (tag === 1) return `cons@${(x - 1).toString(16)}`;
    if (tag === 9) { const h = this.ld32(x - 9); return `obj@${(x - 9).toString(16)} subtag=0x${(h & 0xFF).toString(16)} n=${h >>> 8}`; }
    return `0x${(x >>> 0).toString(16)}`;
  }
  fault(msg) {
    const bt = this.backtrace(100000);
    const shown = bt.length > 40 ? bt.slice(0, 20).concat([`... ${bt.length - 40} more frames ...`], bt.slice(-20)) : bt;
    const segv = this.segvLog && this.segvLog.length
      ? `\n-- recovered SEGVs, oldest first --\n  ` + this.segvLog.join('\n  ') : '';
    throw new MvmFault(`${msg} at ${this.where()} (step ${this.steps})\n  ` + shown.join('\n  ') + segv);
  }
  heapExhausted(used, live, need = 0) {
    const mb = (x) => (x / 1048576).toFixed(1);
    this.host.log(`modus: heap exhausted (${mb(live)} MB survived a collection of ${mb(used)} MB into a ${mb(this.ld32(A_GC_SIZE))} MB semispace${need ? `, ${need} more bytes wanted` : ''}; raise --semi)`);
    this.fault('heap exhausted');
  }
  memFault(what) {
    if (!this.segvLog) this.segvLog = [];
    this.segvLog.push(`${what} at ${this.where()} < ` + this.backtrace(8).slice(1).join(' < '));
    if (this.segvLog.length > 8) this.segvLog.shift();
    if (this.ld32(A_JMPBUF) !== 0) { if (this.trace) this.host.log(`[segv: ${what} at ${this.where()}]`); this.longjmp(); }
    this.fault(`SIGSEGV: ${what}`);
  }

  // -- allocation -----------------------------------------------------------------
  markStart(raw) { const g = (raw - this.pageBase) >> 4; this.startBmp[g >> 3] |= 1 << (g & 7); }
  markCons(raw) { const g = (raw - this.pageBase) >> 4; this.consBmp[g >> 3] |= 1 << (g & 7); }
  isStart(raw) { const g = (raw - this.pageBase) >> 4; return (this.startBmp[g >> 3] >> (g & 7)) & 1; }
  isCons(raw) { const g = (raw - this.pageBase) >> 4; return (this.consBmp[g >> 3] >> (g & 7)) & 1; }
  bump(total) {
    const base = this.va;
    if (base + total > this.heapEnd) this.fault('heap exhausted');
    this.va = base + total;
    return base;
  }
  allocObj(count, subtag, fill) {
    if (count < 0 || count > 0xFFFFFF) this.memFault(`object of ${count} slots`);
    const total = align16((count + 1) * 4);
    const base = this.bump(total);
    if (fill) this.zero(base + 4, base + total);
    this.m32[(base - VBASE) >> 2] = (count << 8) | subtag;
    this.markStart(base);
    return base | 9;
  }
  allocCons(car, cdr) {
    const base = this.bump(16);
    const i = (base - VBASE) >> 2;
    this.m32[i] = car; this.m32[i + 1] = cdr; this.m32[i + 2] = 0; this.m32[i + 3] = 0;
    this.markStart(base); this.markCons(base);
    return base | 1;
  }
  allocFloat(d, subtag = 0x60) {
    const base = this.bump(32);
    const i = (base - VBASE) >> 2, m32 = this.m32;
    m32[i] = (4 << 8) | subtag;
    this.f64[0] = d;
    for (let k = 0; k < 4; k++) m32[i + 1 + k] = this.f64u16[3 - k] << 1;
    m32[i + 5] = 0; m32[i + 6] = 0; m32[i + 7] = 0;
    this.markStart(base);
    return base | 9;
  }
  floatVal(v) {
    if ((v & 0xF) !== 9 || v < this.heapBase || v >= this.heapEnd) this.memFault('float op on non-object');
    const i = (v - 9 - VBASE) >> 2;
    for (let k = 0; k < 4; k++) this.f64u16[3 - k] = (this.m32[i + 1 + k] >> 1) & 0xFFFF;
    return this.f64[0];
  }
  f32Lane(arr, x) {
    const o = this.rv(arr), a = o - 5 + 4 * (this.rv(x) >> 1);
    if ((o & 0xF) !== 9 || a < this.heapBase || a >= this.heapEnd) this.memFault('f32 lane');
    return a - VBASE;
  }

  // -- garbage collector: Cheney copy, conservative validated roots, TYPED
  // to-space walk (a word-by-word walk would rewrite byte payloads -- bytecode
  // is one -- that happen to spell a from-space pointer). ------------------------
  // NEED: bytes the allocation after this collection takes.  Survivors that
  // do not fit in to-space, or a NEED that still does not fit after
  // collecting, is HEAP EXHAUSTED -- loudly: the semispaces are adjacent, so
  // copying on would write into the from-space still being read (a 12M-cons
  // list came back 3.4M long), and allocating on would run into it.
  gc(need = 0) {
    if (this._ikCache) this._ikCache.clear();
    const m32 = this.m32;
    const fromStart = this.ld32(A_GC_FROM);
    const spaceSize = this.ld32(A_GC_SIZE);
    const fromEnd = fromStart + spaceSize;
    const toStart = this.ld32(A_GC_TO);
    let free = toStart;
    const toEnd = toStart + spaceSize;
    const self = this;
    const t0 = Date.now();
    const used = this.va - fromStart;
    function copy(v) {
      const tag = v & 0xF;
      const raw = v - tag;
      const hi = (raw - VBASE) >> 2;
      const hdr = m32[hi];
      if ((hdr & 0xF) === 0xF) return (hdr & ~0xF) | tag;
      const cons = self.isCons(raw);
      let size;
      if (cons) size = 16;
      else {
        size = objBytes(hdr);
        if (size < 16 || raw + size > fromEnd) return v;
      }
      const dst = free;
      if (dst + size > toEnd) self.heapExhausted(used, dst - toStart);
      self.m8.copyWithin(dst - VBASE, raw - VBASE, raw - VBASE + size);
      free += size;
      self.markStart(dst);
      if (cons) self.markCons(dst);
      m32[hi] = dst | 0xF;
      return dst | tag;
    }
    function scanWord(i) {
      const v = m32[i];
      const tag = v & 0xF;
      if (tag !== 1 && tag !== 9) return;
      const raw = v - tag;
      if (raw < fromStart || raw >= fromEnd) return;
      if (!self.isStart(raw)) return;
      if (self.isCons(raw) !== (tag === 1 ? 1 : 0)) return;
      m32[i] = copy(v);
    }
    for (let a = this.esp; a < this.stackTop; a += 4) scanWord((a - VBASE) >> 2);
    for (let a = VBASE; a < BSS_END; a += 4) scanWord((a - VBASE) >> 2);
    { const t = this.vrl & 0xF;
      if (t === 1 || t === 9) { const keep = m32[0]; m32[0] = this.vrl; scanWord(0); this.vrl = m32[0]; m32[0] = keep; } }
    let a = toStart;
    while (a < free) {
      const i = (a - VBASE) >> 2;
      if (this.isCons(a)) { scanWord(i); scanWord(i + 1); a += 16; continue; }
      const hdr = m32[i], size = objBytes(hdr);
      if (!LEAF[hdr & 0xFF]) { const n = hdr >>> 8; for (let k = 1; k <= n; k++) scanWord(i + k); }
      a += size;
    }
    this.st64(A_GC_FROM, toStart, 0);
    this.st64(A_GC_TO, fromStart, 0);
    this.va = free;
    this.vl = toStart + spaceSize;
    const g0 = (fromStart - this.pageBase) >> 4, g1 = (fromEnd - this.pageBase) >> 4;
    // EXCLUSIVE end: the semispaces are adjacent and granule-aligned to 8, so
    // byte g1>>3 is the first of the OTHER semispace -- the to-space this
    // collection just copied its first survivors into.  Clearing it (as a +1
    // here did) erased their start/cons bits, so a later root pointing at one
    // was not forwarded: TYPE-ERRORs / `#<?F..>' after enough collections.
    this.startBmp.fill(0, g0 >> 3, g1 >> 3);
    this.consBmp.fill(0, g0 >> 3, g1 >> 3);
    this.gcCount++;
    if (this._w) this._w.g.gcn.value = this.gcCount;   // wasm32.js: reload after a call only if this moved
    this.st64(A_GC_COUNT, this.gcCount, 0);
    if (this.va + need >= this.vl) this.heapExhausted(used, free - toStart, need);
    if (this.trace) this.host.log(`[gc #${this.gcCount}: ${used >> 10}K -> ${(free - toStart) >> 10}K, ${Date.now() - t0}ms]`);
  }

  // -- calls ------------------------------------------------------------------------
  enter(target) {
    const m32 = this.m32;
    if (this.callerOf) this.callerPc = this.pc;
    const cb = (this.ebp - VBASE) >> 2;
    const a0 = m32[cb + IX0], a1 = m32[cb + IX1], a2 = m32[cb + IX2], a3 = m32[cb + IX3], a4 = m32[cb + IX4];
    this.push(this.ebp);
    this.ebp = this.esp;
    this.esp -= frameBytes(this.code, target);
    if (this.esp < STACK_ADDR + 4096) { this.esp = this.ebp; this.ebp = this.pop(); this.pop(); this.memFault('stack overflow'); }
    const b = (this.ebp - VBASE) >> 2;
    m32[b + IX0] = a0; m32[b + IX1] = a1; m32[b + IX2] = a2; m32[b + IX3] = a3; m32[b + IX4] = a4;
    this.pc = target;
    if (this.prof) this.prof.set(target, (this.prof.get(target) || 0) + 1);
    if (target === this.watchAt && this.watchLeft-- > 0) this.host.log(`[watch ${this.where(target)} V0=${this.lispStr(a0)} V1=${this.lispStr(a1)} from ${this.backtrace(4).slice(1).join(' < ')}]`);
    if (target === this.snapAt) { this.snapAt = -1; this.onSnapshot(); }
    if (target === this.shapeResetAt && this.SC) { this.SC.fill(0); this.shapeResets++; if (this.callers) this.callers.clear(); }
    // MVM_CALLERS=NAME,...: who calls these (return address -> function), since the last reset.
    if (this.callerOf && this.callerOf.has(target)) {
      const k = this.callerOf.get(target) + ' <- ' + this.where(this.callerPc);
      this.callers.set(k, (this.callers.get(k) || 0) + 1);
    }
  }
  doCall(target, retpc) { this.push(retpc); this.enter(target); }
  doTailcall(target) {
    const m32 = this.m32, b = (this.ebp - VBASE) >> 2;
    const a0 = m32[b + IX0], a1 = m32[b + IX1], a2 = m32[b + IX2], a3 = m32[b + IX3], a4 = m32[b + IX4];
    this.esp = this.ebp;
    const oldEbp = this.pop();
    const ret = this.pop();
    this.push(ret);
    this.push(oldEbp);
    this.ebp = this.esp;
    this.esp -= frameBytes(this.code, target);
    const nb = (this.ebp - VBASE) >> 2;
    m32[nb + IX0] = a0; m32[nb + IX1] = a1; m32[nb + IX2] = a2; m32[nb + IX3] = a3; m32[nb + IX4] = a4;
    this.pc = target;
    if (this.prof) this.prof.set(target, (this.prof.get(target) || 0) + 1);
  }
  doRet() {
    this.esp = this.ebp;
    this.ebp = this.pop();
    this.pc = this.pop();
  }
  fnAddrToOffset(x) {
    if ((x & 0xF) !== 3) this.memFault(`call-ind on non-function 0x${(x >>> 0).toString(16)}`);
    return (x - 3) >>> 4;
  }
  // Run a nested activation to completion; the result is returned (and in VR).
  // ARGS: numbers, or mvm.js-style [lo, hi] pairs.
  callLisp(fn, args) {
    const savedPc = this.pc, svl = this.vrl;
    const m32 = this.m32, b = (this.ebp - VBASE) >> 2;
    const s0 = m32[b + IX0], s1 = m32[b + IX1], s2 = m32[b + IX2], s3 = m32[b + IX3];
    for (let i = 0; i < 4; i++) {
      const a = args[i];
      this.setR(i, i < args.length ? (Array.isArray(a) ? a[0] : a) : NIL);
    }
    this.st32(A_NARGS, args.length);
    this.callFn(fn.off);
    const r = this.vrl;
    m32[b + IX0] = s0; m32[b + IX1] = s1; m32[b + IX2] = s2; m32[b + IX3] = s3;
    this.pc = savedPc; this.vrl = svl;
    return r;
  }

  // -- handler stack (setjmp / longjmp) -------------------------------------------
  handlerPush() {
    const depth = this.ld32(A_HDEPTH);
    if (depth >= HMAX) { this.st32(A_HOVF, this.ld32(A_HOVF) + 1); return 1; }
    const fr = A_HSTACK + depth * 32;
    for (let i = 0; i < JMPBUF_WORDS; i++) this.st64(fr + 8 * i, this.ld32(A_JMPBUF + 8 * i), 0);
    this.st64(A_HDEPTH, depth + 1, 0);
    return 0;
  }
  handlerPop() {
    const ovf = this.ld32(A_HOVF);
    if (ovf !== 0) { this.st32(A_HOVF, ovf - 1); return; }
    const depth = this.ld32(A_HDEPTH);
    if (depth === 0) {
      for (let i = 0; i < JMPBUF_WORDS; i++) this.st64(A_JMPBUF + 8 * i, 0, 0);
      return;
    }
    const fr = A_HSTACK + (depth - 1) * 32;
    this.st64(A_HDEPTH, depth - 1, 0);
    for (let i = 0; i < JMPBUF_WORDS; i++) this.st64(A_JMPBUF + 8 * i, this.ld32(fr + 8 * i), 0);
  }
  setjmp(resumePc) {
    if (!this.handlerPush()) {
      const b = (this.ebp - VBASE) >> 2;
      this.st64(A_JMPBUF, this.esp, 0);
      this.st64(A_JMPBUF + 8, this.ebp, 0);
      this.st64(A_JMPBUF + 16, resumePc, 0);
      this.st64(A_JMPBUF + 24, this.m32[b + IX4], 0);
    }
    this.vrl = NIL;
  }
  longjmp() {
    this.st32(A_HOVF, 0);
    const esp = this.ld32(A_JMPBUF), ebp = this.ld32(A_JMPBUF + 8), ip = this.ld32(A_JMPBUF + 16);
    const v4 = this.ld32(A_JMPBUF + 24);
    if (esp === 0) this.fault('longjmp with no handler armed');
    this.handlerPop();
    this.ebp = ebp; this.esp = esp; this.pc = ip;
    this.m32[((ebp - VBASE) >> 2) + IX4] = v4;
    this.vrl = TV;
    if (WASM) throw new WebAssembly.Exception(LJ_TAG, [esp, ip]);
    throw new LongJmp(esp, ip);
  }

  dumpFrame(who) {
    const b = (this.ebp - VBASE) >> 2, m32 = this.m32, r = [], sl = [];
    for (let v = 0; v < 16; v++) r.push(this.describe(m32[b + IX[v]]));
    for (let k = 0; k < 8; k++) sl.push(this.describe(m32[b + (slotOff(k) >> 2)]));
    this.host.log(`[land ${who} ip=${this.where(this.pc)} VR=${this.describe(this.vrl)} regs ${r.join(',')} slots ${sl.join(',')}]`);
  }
  // -- traps ------------------------------------------------------------------------
  trap(codeNum, nextPc) {
    const h = this.host;
    if (codeNum < 0x100 || codeNum >= 0x8000) {
      const n = frameEnterParams(codeNum);
      for (let i = 4; i < n; i++) this.st32(this.ebp + slotOff(i), this.ld32(this.ebp + 8 + 4 * (i - 4)));
      return;
    }
    if (codeNum < 0x300) return;
    switch (codeNum) {
      case 0x0300: h.writeByte(1, (this.rv(0) >> 1) & 0xFF); return;
      case 0x0301: { const c = h.readByte(0); this.setR(0, (c < 0 ? 0xFF : c) << 1); return; }
      case 0x0302: case 0x0303: case 0x0304: case 0x0320: case 0x0321: return;
      case 0x0310: {                          // translate-i386 hosted: ms since the first read, tagged
        const s = Math.floor(h.now());
        let e = this.ld32(A_CLOCK_EPOCH);
        if (e === 0) { e = (s + 1) << 1; this.st32(A_CLOCK_EPOCH, e); }
        const ms = Math.floor(h.now() * 1000) - ((e >> 1) - 1) * 1000;
        this.vrl = (ms << 1) | 0; return;
      }
      case 0x0311: this.vrl = 1000 << 1; return;
      case 0x0500: throw new MvmExit(this.rv(0) >> 1);
      case 0x0502: { const r = this.syscall(this.arg(0), this.arg(1), this.arg(2), this.arg(3), 0, 0, 0); this.setR(0, (r * 2) | 0); return; }
      case 0x0503: { const r = this.syscall(this.arg(0), this.rv(1), this.rv(2), this.rv(3), 0, 0, 0); this.setR(0, r | 0); return; }
      case 0x0507: case 0x050B: {
        const r = this.syscall(this.arg(0), this.arg(1), this.arg(2), this.arg(3), this.arg(4), this.arg(5), this.arg(6));
        this.setR(0, (r * 2) | 0); return;
      }
      case 0x0504: case 0x0531: { const a = this.mmap(this.arg(0)); this.setR(0, (a * 2) | 0); return; }
      case 0x0510: this.setjmp(nextPc); return;
      case 0x0511: this.longjmp(); return;
      case 0x0512: this.handlerPop(); return;
      case 0x0520: this.setR(0, NIL); return;
      case 0x0530: {
        let n = this.ld32(A_NARGS);
        if (n < 5) return;
        if (n > 121) n = 121;
        for (let i = 4; i < n; i++) this.st32(this.ebp + slotOff(i), this.ld32(this.ebp + 8 + 4 * (i - 4)));
        return;
      }
      case 0x0532: {
        const phys = this.arg(0) - VBASE;
        if (phys < JIT_PHYS || phys >= JIT_END - VBASE) this.fault(`%jit-call outside the exec region: 0x${this.arg(0).toString(16)}`);
        this.callFn(phys);
        return;
      }
      case 0x0533: {
        const base = this.arg(0), len = this.arg(1);
        const ok = this.relocate(base - VBASE, len, base - VBASE, true);
        this.st64(A_WEB_RELOC_STATUS, ok ? 0 : 2, 0);
        return;
      }
      case 0x0534: return;
      case 0x0540: this.fault('threads are not supported here (%spawn-thread)');
      default: this.fault(`unimplemented trap 0x${codeNum.toString(16)}`);
    }
  }
  arg(v) { return this.rv(v) >> 1; }
  mmap(size) {
    const a = this.mmapNext;
    const n = (size + 4095) & ~4095;
    if (a + n > JIT_END) return -12;
    this.mmapNext += n;
    return a;
  }

  // -- sockets as HTTP requests (as mvm.js) ---------------------------------------
  resolveHost(name) {
    if (!this.fakeIps.has(name)) {
      const n = this.fakeIps.size + 1;
      const ip = (10 << 24) | (77 << 16) | (((n >> 8) & 0xFF) << 8) | (n & 0xFF);
      this.fakeIps.set(name, ip >>> 0); this.ipNames.set(ip >>> 0, name);
    }
    return this.fakeIps.get(name);
  }
  sockRead(sk, m8, off, len) {
    if (!sk.resp) {
      const req = new Uint8Array(sk.req.reduce((a, b) => a + b.length, 0));
      let p = 0; for (const b of sk.req) { req.set(b, p); p += b.length; }
      const text = new TextDecoder('latin1').decode(req);
      const m = /^([A-Z]+) (\S+) HTTP\/1\.[01]\r?\n([\s\S]*?)\r?\n\r?\n([\s\S]*)$/.exec(text);
      if (!m) return -104;
      const headers = {};
      for (const line of m[3].split(/\r?\n/)) { const i = line.indexOf(':'); if (i > 0) headers[line.slice(0, i).trim()] = line.slice(i + 1).trim(); }
      const host = sk.host || headers.Host || headers.host || `${sk.ip}`;
      const scheme = sk.port === 443 ? 'https' : 'http';
      const url = `${scheme}://${host}${(sk.port === 80 || sk.port === 443) ? '' : ':' + sk.port}${m[2]}`;
      try { sk.resp = this.host.httpRequest(url, m[1], headers, m[4]); }
      catch (e) { this.host.log(`[http: ${e.message || e}]`); sk.resp = new TextEncoder().encode(`HTTP/1.0 502 Bad Gateway\r\nContent-Length: 0\r\n\r\n`); }
      sk.pos = 0;
    }
    const n = Math.min(len, sk.resp.length - sk.pos);
    m8.set(sk.resp.subarray(sk.pos, sk.pos + n), off);
    sk.pos += n;
    return n;
  }
  syscall(nr, a1, a2, a3, a4, a5, a6) {
    const h = this.host;
    const inMem = (a, n) => a >= VBASE && a + n <= this.heapEnd;
    const sk = (nr === 0 || nr === 1 || nr === 3 || nr === 42) ? this.socks.get(a1) : undefined;
    switch (nr) {
      case 60: case 231: throw new MvmExit(a1);
      case 13: return 0;
      case 0: if (!inMem(a2, a3)) return -14;
              return sk ? this.sockRead(sk, this.m8, a2 - VBASE, a3) : h.read(a1, this.m8, a2 - VBASE, a3);
      case 1: if (!inMem(a2, a3)) return -14;
              if (sk) { sk.req.push(this.m8.slice(a2 - VBASE, a2 - VBASE + a3)); return a3; }
              return h.write(a1, this.m8, a2 - VBASE, a3);
      case 2: return h.open(this.cstr(a1), a2, a3);
      case 3: if (sk) { this.socks.delete(a1); return 0; } return h.close(a1);
      case 41: {
        if (a1 !== 2) return -97;
        const fd = this.nextSockFd++;
        this.socks.set(fd, { type: a2, req: [], resp: null, pos: 0, host: null, ip: 0, port: 0 });
        return fd;
      }
      case 42: {
        if (!sk) return -9;
        const port = (this.m8[a2 - VBASE + 2] << 8) | this.m8[a2 - VBASE + 3];
        const ip = ((this.m8[a2 - VBASE + 4] << 24) | (this.m8[a2 - VBASE + 5] << 16) | (this.m8[a2 - VBASE + 6] << 8) | this.m8[a2 - VBASE + 7]) >>> 0;
        sk.port = port; sk.ip = ip;
        sk.host = this.ipNames.get(ip) || `${ip >>> 24}.${(ip >>> 16) & 255}.${(ip >>> 8) & 255}.${ip & 255}`;
        return 0;
      }
      case 7: return 1;
      case 54: case 55: return 0;
      case 4242: { const name = this.cstr(a1); return name ? this.resolveHost(name) : 0; }
      case 4: case 5: {
        const st = nr === 4 ? h.stat(this.cstr(a1)) : h.fstat(a1);
        if (typeof st === 'number') return st;
        this.st64(a2 + 48, st.size | 0, 0);
        this.st64(a2 + 88, st.mtime | 0, 0);
        return 0;
      }
      case 8: return h.lseek(a1, a2, a3);
      case 9: return this.mmap(a2);
      case 10: case 11: return 0;
      case 21: return h.access(this.cstr(a1), a2);
      case 24: case 158: return 0;
      case 186: return 1;
      case 35: return 0;
      case 39: return h.getpid();
      case 74: case 75: case 77: return 0;
      case 82: return h.rename(this.cstr(a1), this.cstr(a2));
      case 83: return h.mkdir(this.cstr(a1), a2);
      case 87: return h.unlink(this.cstr(a1));
      case 201: return (Date.now() / 1000) | 0;
      case 228: {
        // CLOCK_PROCESS_CPUTIME_ID (2) / CLOCK_THREAD_CPUTIME_ID (3): the
        // process's CPU time where the host has it (node; run it with
        // --single-threaded for a VM-only figure), else the wall clock.
        if ((a1 === 2 || a1 === 3) && typeof process !== 'undefined' && process.cpuUsage) {
          const u = process.cpuUsage(), us = u.user + u.system;
          this.st64(a2, Math.floor(us / 1e6), 0); this.st64(a2 + 8, (us % 1e6) * 1000, 0); return 0;
        }
        const ms = Date.now(); this.st64(a2, (ms / 1000) | 0, 0); this.st64(a2 + 8, ((ms % 1000) * 1e6) | 0, 0); return 0;
      }
      case 217: {
        const r = h.getdents(a1);
        if (typeof r === 'number') return r;
        let p = a2, total = 0, n = 0;
        for (const e of r) {
          const nm = e.name;
          const reclen = (19 + nm.length + 1 + 7) & ~7;
          if (total + reclen > a3) break;
          this.st64(p, e.ino | 0, 0); this.st64(p + 8, 0, 0);
          this.st16(p + 16, reclen); this.st8(p + 18, e.type);
          for (let i = 0; i < nm.length; i++) this.st8(p + 19 + i, nm.charCodeAt(i) & 0xFF);
          this.st8(p + 19 + nm.length, 0);
          p += reclen; total += reclen; n++;
        }
        h.getdentsConsumed(a1, n);
        return total;
      }
      case 5000: if (a2 > 0 && inMem(a1, a2) && h.guiSend) h.guiSend(this.m8, a1 - VBASE, a2); return 0;
      case 5001: return (h.guiPoll && inMem(a1, a2)) ? h.guiPoll(this.m8, a1 - VBASE, a2) : 0;
      case 5002: if (h.guiWait) h.guiWait(a1); return 0;
      default:
        h.log(`[mvm: unsupported syscall ${nr}]`);
        return -38;
    }
  }

  // -- the interpreter --------------------------------------------------------------
  run() {
    const baseEsp = this.esp, jd0 = this.jd;
    for (;;) {
      try { this.loop(); return; }
      catch (e) {
        const lj = ljOf(e);
        if (DBG.MVM_WASM_DEBUG && lj) this.host.log(`[run catch lj.esp=${(lj.esp >>> 0).toString(16)} baseEsp=${(baseEsp >>> 0).toString(16)} ip=${this.where(lj.ip)} -> ${lj.esp <= baseEsp}]`);
        if (lj && lj.esp <= baseEsp) { this.jd = jd0; continue; }
        throw e;
      }
    }
  }
  loop() {
    let pc = this.pc;
    for (;;) {
      if (this.trace) {
        this.steps++;
        if (this.trace > 1 && (!this.traceFrom || (this.steps >= this.traceFrom && this.steps < this.traceFrom + this.traceCount))) this.host.log(`${this.steps} ${this.where(pc)} op=${this.code[pc].toString(16)} ${this.traceRegs ? this.regDump() : ''}`);
        else if (this.maxSteps && this.steps >= this.maxSteps) { this.pc = pc; this.fault('step limit'); }
        else if (this.prof && (this.steps & 255) === 0) { const f = this.fnAt(pc); if (f) this.prof.set(-f.off - 1, (this.prof.get(-f.off - 1) || 0) + 1); }
      }
      const op = this.code[pc];
      const npc = this.execInsn(pc);
      // ON-STACK REPLACEMENT: a hot backward branch translates from the loop
      // head and continues there in JS.  Sound because this interpreter keeps
      // every register in its frame home, which is exactly what a translated
      // function loads when entered at a block label.  Without it a long loop
      // inside ONE call (every benchmark's driver, any script loop) never left
      // the interpreter: a function is translated only after 20 CALLS.
      if (npc < pc && BACK_BRANCH[op] && this.compileOn && !NO_OSR) pc = this.osrAt(npc);
      else pc = npc;
      if (pc === RET_SENTINEL) return;
    }
  }
  osrAt(t) {
    let f = this.osrCompiled.get(t);
    if (f === undefined) {
      const n = (this.osrCounts.get(t) || 0) + 1;
      if (n < OSR_THRESHOLD || this.trace > 1) { this.osrCounts.set(t, n); return t; }
      const t0 = performance.now();
      f = (WASM && this.wmem && WASM.compile(this, t, true)) || this.compileFn(t, true);
      this.compileStats.ms += performance.now() - t0;
      this.osrCompiled.set(t, f);
      if (f) this.compileStats.osr++;
    }
    if (!f) return t;
    this.pc = t;
    this.drive(f, f.ENTRYL);
    return this.pc;
  }
  // checked memory address: a tagged/raw address must land in modelled memory
  ck(a, what) { if (a < VBASE || a >= this.heapEnd) this.memFault(what); return (a - VBASE) >> 2; }

  execInsn(pc) {
    const code = this.code, m32 = this.m32, m8 = this.m8;
    const heapEnd = this.heapEnd;
    const op = code[pc];
    this.pc = pc;
    switch (op) {
      case 0x00: return pc + 1;
      case 0x01: this.fault('break');
      case 0x02: { this.trap(code[pc + 1] | (code[pc + 2] << 8), pc + 3); return pc + 3; }
      case 0x10: this.setR(code[pc + 1], this.rv(code[pc + 2])); return pc + 3;
      case 0x11: this.setR(code[pc + 1], this.rd32(pc + 2)); return pc + 10;
      case 0x12: this.push(this.rv(code[pc + 1])); return pc + 2;
      case 0x13: this.setR(code[pc + 1], this.pop()); return pc + 2;
      case 0x14: {
        const idx = this.rd32(pc + 2);
        if (pc >= JIT_PHYS) {
          const vec = this.ld32(A_WEB_CONSTS);
          if (vec === 0) this.fault('li-const in a page with no constant vector');
          this.setR(code[pc + 1], this.ld32(vec - 5 + 4 * idx));
        } else {
          const off = this.mod.addrTable[idx] | 0;
          this.setR(code[pc + 1], off === 0 ? 0 : (POOL_ADDR + off));
        }
        return pc + 10;
      }
      case 0x20: this.setR(code[pc + 1], (this.rv(code[pc + 2]) + this.rv(code[pc + 3])) | 0); return pc + 4;
      case 0x21: this.setR(code[pc + 1], (this.rv(code[pc + 2]) - this.rv(code[pc + 3])) | 0); return pc + 4;
      case 0x22: this.setR(code[pc + 1], Math.imul(this.rv(code[pc + 2]) >> 1, this.rv(code[pc + 3]))); return pc + 4;
      case 0x23: case 0x24: {
        const x = this.rv(code[pc + 2]) >> 1, y = this.rv(code[pc + 3]) >> 1;
        if (y === 0) this.memFault('division by zero');
        const r = op === 0x23 ? Math.trunc(x / y) : x % y;
        this.setR(code[pc + 1], (r * 2) | 0); return pc + 4;
      }
      case 0x25: this.setR(code[pc + 1], (-this.rv(code[pc + 2])) | 0); return pc + 3;
      case 0x26: { const d = code[pc + 1]; this.setR(d, (this.rv(d) + 2) | 0); return pc + 2; }
      case 0x27: { const d = code[pc + 1]; this.setR(d, (this.rv(d) - 2) | 0); return pc + 2; }
      case 0x28: this.setR(code[pc + 1], this.rv(code[pc + 2]) & this.rv(code[pc + 3])); return pc + 4;
      case 0x29: this.setR(code[pc + 1], this.rv(code[pc + 2]) | this.rv(code[pc + 3])); return pc + 4;
      case 0x2A: this.setR(code[pc + 1], this.rv(code[pc + 2]) ^ this.rv(code[pc + 3])); return pc + 4;
      case 0x2B: this.setR(code[pc + 1], shli(this.rv(code[pc + 2]), code[pc + 3])); return pc + 4;
      case 0x2C: this.setR(code[pc + 1], shri(this.rv(code[pc + 2]), code[pc + 3])); return pc + 4;
      case 0x2D: this.setR(code[pc + 1], this.rv(code[pc + 2]) >> Math.min(code[pc + 3], 31)); return pc + 4;
      case 0x2F: this.setR(code[pc + 1], shlv(this.rv(code[pc + 2]), this.rv(code[pc + 3]) >> 1)); return pc + 4;
      case 0x32: this.setR(code[pc + 1], sarv(this.rv(code[pc + 2]), this.rv(code[pc + 3]) >> 1)); return pc + 4;
      case 0x2E: this.setR(code[pc + 1], (this.rv(code[pc + 2]) >>> (code[pc + 3] & 31)) & ldbMask(code[pc + 4])); return pc + 5;
      case 0x30: { const a = this.rv(code[pc + 1]), b = this.rv(code[pc + 2]); this.cmp = a < b ? -1 : a > b ? 1 : 0; return pc + 3; }
      case 0x31: { const r = this.rv(code[pc + 1]) & this.rv(code[pc + 2]); this.cmp = r === 0 ? 0 : (r < 0 ? -1 : 1); return pc + 3; }
      case 0x33: { const a = this.rv(code[pc + 1]), b = this.rd32(pc + 2); this.cmp = a < b ? -1 : a > b ? 1 : 0; return pc + 6; }
      case 0x34: { const r = this.rv(code[pc + 1]) & this.rd32(pc + 2); this.cmp = r === 0 ? 0 : (r < 0 ? -1 : 1); return pc + 6; }
      case 0x40: return pc + 5 + this.rd32(pc + 1);
      case 0x41: return this.cmp === 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5;
      case 0x42: return this.cmp !== 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5;
      case 0x43: return this.cmp < 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5;
      case 0x44: return this.cmp >= 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5;
      case 0x45: return this.cmp <= 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5;
      case 0x46: return this.cmp > 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5;
      case 0x47: return this.rv(code[pc + 1]) === NIL ? pc + 6 + this.rd32(pc + 2) : pc + 6;
      case 0x48: return this.rv(code[pc + 1]) !== NIL ? pc + 6 + this.rd32(pc + 2) : pc + 6;
      case 0x50: case 0x51: {
        const x = this.rv(code[pc + 2]), a = x + (op === 0x50 ? -1 : 3);
        if ((x & 7) !== 1 || a < VBASE || a >= heapEnd) this.memFault((op === 0x50 ? 'car' : 'cdr') + ` of non-cons 0x${(x >>> 0).toString(16)}`);
        this.setR(code[pc + 1], m32[(a - VBASE) >> 2]); return pc + 3;
      }
      case 0x52: this.setR(code[pc + 1], this.allocCons(this.rv(code[pc + 2]), this.rv(code[pc + 3]))); return pc + 4;
      case 0x53: case 0x54: {
        const x = this.rv(code[pc + 1]), a = x + (op === 0x53 ? -1 : 3);
        if ((x & 7) !== 1 || a < VBASE || a >= heapEnd) this.memFault(`rplac on non-cons 0x${(x >>> 0).toString(16)}`);
        m32[(a - VBASE) >> 2] = this.rv(code[pc + 2]); return pc + 3;
      }
      case 0x55: { const x = this.rv(code[pc + 2]); this.setR(code[pc + 1], (x !== NIL && (x & 0xF) === 1) ? TV : NIL); return pc + 3; }
      case 0x56: { const x = this.rv(code[pc + 2]); this.setR(code[pc + 1], (x !== NIL && (x & 0xF) === 1) ? NIL : TV); return pc + 3; }
      case 0x60: this.setR(code[pc + 1], this.allocObj(code[pc + 2] | (code[pc + 3] << 8), code[pc + 4], true)); return pc + 5;
      case 0x61: {
        const vobj = code[pc + 2], idx = code[pc + 3];
        const a = vobj === 21 ? this.ebp + slotOff(idx) : this.rv(vobj) - 5 + 4 * idx;
        if (a < VBASE || a >= heapEnd) this.memFault(`obj-ref on 0x${(this.rv(vobj) >>> 0).toString(16)}`);
        this.setR(code[pc + 1], m32[(a - VBASE) >> 2]); return pc + 4;
      }
      case 0x62: {
        const vobj = code[pc + 1], idx = code[pc + 2];
        const a = vobj === 21 ? this.ebp + slotOff(idx) : this.rv(vobj) - 5 + 4 * idx;
        if (a < VBASE || a >= heapEnd) this.memFault(`obj-set on 0x${(this.rv(vobj) >>> 0).toString(16)}`);
        m32[(a - VBASE) >> 2] = this.rv(code[pc + 3]); return pc + 4;
      }
      case 0x63: this.setR(code[pc + 1], (this.rv(code[pc + 2]) & 0xF) << 1); return pc + 3;
      case 0x64: {
        const x = this.rv(code[pc + 2]);
        let r = 0;
        if ((x & 0xF) === 9 && x !== TV && x - 9 >= VBASE && x - 9 < heapEnd) r = (m32[(x - 9 - VBASE) >> 2] & 0xFF) << 1;
        this.setR(code[pc + 1], r); return pc + 3;
      }
      case 0x65: {
        const o = code[pc + 2], x = this.rv(code[pc + 3]);
        const a = o === 21 ? this.ebp + slotOff(x >> 1) : this.rv(o) + x * 2 - 5;
        if (a < VBASE || a >= heapEnd) this.memFault('aref');
        this.setR(code[pc + 1], m32[(a - VBASE) >> 2]); return pc + 4;
      }
      case 0x66: {
        const a = this.rv(code[pc + 1]) + this.rv(code[pc + 2]) * 2 - 5;
        if (a < VBASE || a >= heapEnd) this.memFault('aset');
        m32[(a - VBASE) >> 2] = this.rv(code[pc + 3]); return pc + 4;
      }
      case 0x67: {
        const x = this.rv(code[pc + 2]);
        let r = 0;
        if ((x & 0xF) === 9 && x !== TV && x - 9 >= VBASE && x - 9 < heapEnd) r = (m32[(x - 9 - VBASE) >> 2] >>> 8) << 1;
        this.setR(code[pc + 1], r); return pc + 3;
      }
      case 0x68: this.setR(code[pc + 1], this.allocObj(this.rv(code[pc + 2]), 0x32, true)); return pc + 3;
      // ---- W32: raw 32-bit words (interp.lisp is the oracle) ----
      case 0x69: this.setR(code[pc + 1], this.rd32(pc + 2)); return pc + 6;
      case 0x6A: this.setR(code[pc + 1], (this.rv(code[pc + 2]) + this.rv(code[pc + 3])) | 0); return pc + 4;
      case 0x6B: this.setR(code[pc + 1], (this.rv(code[pc + 2]) - this.rv(code[pc + 3])) | 0); return pc + 4;
      case 0x6C: this.setR(code[pc + 1], Math.imul(this.rv(code[pc + 2]), this.rv(code[pc + 3]))); return pc + 4;
      case 0x6D: {
        const a = this.rv(code[pc + 2]) >>> 0, b = this.rv(code[pc + 3]) >>> 0;
        this.setR(code[pc + 1], Number((BigInt(a) * BigInt(b)) >> 32n) | 0); return pc + 4;
      }
      case 0x6E: this.setR(code[pc + 1], this.rv(code[pc + 2]) & this.rv(code[pc + 3])); return pc + 4;
      case 0x6F: this.setR(code[pc + 1], this.rv(code[pc + 2]) | this.rv(code[pc + 3])); return pc + 4;
      case 0x73: this.setR(code[pc + 1], this.rv(code[pc + 2]) ^ this.rv(code[pc + 3])); return pc + 4;
      case 0x74: this.setR(code[pc + 1], ~this.rv(code[pc + 2])); return pc + 3;
      case 0x75: this.setR(code[pc + 1], this.rv(code[pc + 2]) << (code[pc + 3] & 31)); return pc + 4;
      case 0x76: this.setR(code[pc + 1], (this.rv(code[pc + 2]) >>> (code[pc + 3] & 31)) | 0); return pc + 4;
      case 0x77: this.setR(code[pc + 1], this.rv(code[pc + 2]) >> (code[pc + 3] & 31)); return pc + 4;
      case 0x78: { const x = this.rv(code[pc + 2]), n = code[pc + 3] & 31; this.setR(code[pc + 1], n === 0 ? x : (x << n) | (x >>> (32 - n))); return pc + 4; }
      case 0x79: this.setR(code[pc + 1], ((this.rv(code[pc + 2]) >>> 0) < (this.rv(code[pc + 3]) >>> 0)) ? 2 : 0); return pc + 4;
      case 0x7A: this.setR(code[pc + 1], (this.rv(code[pc + 2]) < this.rv(code[pc + 3])) ? 2 : 0); return pc + 4;
      case 0x7B: this.setR(code[pc + 1], (this.rv(code[pc + 2]) === this.rv(code[pc + 3])) ? 2 : 0); return pc + 4;
      case 0x7C: this.setR(code[pc + 1], this.rv(code[pc + 2]) >> 1); return pc + 3;
      case 0x7D: this.setR(code[pc + 1], (this.rv(code[pc + 2]) >>> 16) << 1); return pc + 3;
      case 0x7E: this.setR(code[pc + 1], (this.rv(code[pc + 2]) & 0xFFFF) << 1); return pc + 3;
      case 0x7F: this.setR(code[pc + 1], (((this.rv(code[pc + 2]) >> 1) << 16) | ((this.rv(code[pc + 3]) >> 1) & 0xFFFF)) | 0); return pc + 4;
      case 0x84: { const a = this.rv(code[pc + 2]) + code[pc + 3];
                   if (a < VBASE || a + 4 > heapEnd) this.memFault('w32-load');
                   this.setR(code[pc + 1], (a & 3) === 0 ? m32[(a - VBASE) >> 2] : this.dv.getInt32(a - VBASE, true)); return pc + 4; }
      case 0x85: { const a = this.rv(code[pc + 1]) + code[pc + 3];
                   if (a < VBASE || a + 4 > heapEnd) this.memFault('w32-store');
                   const v = this.rv(code[pc + 2]);
                   if ((a & 3) === 0) m32[(a - VBASE) >> 2] = v; else this.dv.setInt32(a - VBASE, v, true); return pc + 4; }
      case 0x70: {
        const a = this.rv(code[pc + 2]), w = code[pc + 3] & 3;
        if (a < VBASE || a >= heapEnd) this.memFault(`load 0x${(a >>> 0).toString(16)}`);
        let r;
        if (w === 0) r = m8[a - VBASE];
        else if (w === 1) r = this.ld16(a);
        else r = (a & 3) === 0 ? m32[(a - VBASE) >> 2] : this.dv.getInt32(a - VBASE, true);
        this.setR(code[pc + 1], r); return pc + 4;
      }
      case 0x71: {
        const a = this.rv(code[pc + 1]), s = this.rv(code[pc + 2]), w = code[pc + 3] & 3;
        if (a < VBASE || a >= heapEnd) this.memFault(`store 0x${(a >>> 0).toString(16)}`);
        if (w === 0) m8[a - VBASE] = s & 0xFF;
        else if (w === 1) this.st16(a, s & 0xFFFF);
        else if ((a & 3) === 0) m32[(a - VBASE) >> 2] = s; else this.dv.setInt32(a - VBASE, s, true);
        return pc + 4;
      }
      case 0x72: return pc + 1;
      case 0x80: case 0x81: {
        const t = op === 0x80 ? this.rd32(pc + 1) >>> 0 : this.fnAddrToOffset(this.rv(code[pc + 1]));
        const next = pc + (op === 0x80 ? 5 : 2);
        const f = this.compiledFor(t);
        if (f) { this.push(RET_SENTINEL); this.enter(t); this.drive(f); return next; }
        this.doCall(t, next); return this.pc;
      }
      case 0x82: this.doRet(); return this.pc;
      case 0x83: {
        const t = this.rd32(pc + 1) >>> 0;
        this.doTailcall(t);
        const f = this.compiledFor(t);
        if (f) this.drive(f);
        return this.pc;
      }
      case 0x88: { const base = this.bump(16); this.zero(base, base + 16); this.markStart(base); this.markCons(base); this.setR(code[pc + 1], base | 1); return pc + 2; }
      case 0x89: if (this.va >= this.vl) this.gc(); return pc + 1;
      case 0x8C: { const n = this.rd32(pc + 1) >>> 0; if (this.va + n >= this.vl) this.gc(n); return pc + 5; }
      case 0x8D: { const c = this.rv(code[pc + 1]), kind = code[pc + 2];
                   const nbytes = (kind === 0 ? c * 8 : c >> 1) + 32;    // over-estimates a 4-byte word: harmless
                   if (this.va + nbytes >= this.vl) this.gc(Math.min(nbytes, 0x40000000)); return pc + 3; }
      case 0x8A: return pc + 2;
      case 0x8B: return pc + 1;
      case 0x90: case 0x91: this.fault('save-ctx/restore-ctx not supported');
      case 0x92: return pc + 1;
      case 0x93: {
        const a = this.rv(code[pc + 2]);
        if (a < VBASE || a >= heapEnd) this.memFault('xchg');
        const i = (a - VBASE) >> 2, old = m32[i];
        m32[i] = this.rv(code[pc + 3]);
        this.setR(code[pc + 1], old); return pc + 4;
      }
      case 0xA0: case 0xA1: case 0xA2: case 0xA3: case 0xA4: this.fault('port I/O / halt / cli / sti not supported');
      case 0xA5: case 0xA6: this.fault('percpu ops not supported');
      case 0xA7: { const t = this.rd32(pc + 2) >>> 0; this.setR(code[pc + 1], t === FN_UNRESOLVED ? NIL : ((t << 4) | 3)); return pc + 6; }
      case 0xA8: case 0xA9: {
        const x = BigInt(this.rv(code[pc + 2]) >> 1), y = BigInt(this.rv(code[pc + 3]) >> 1), p = x * y;
        this.setR(code[pc + 1], op === 0xA8 ? (Number(p & 0x3FFFFFFn) << 1) : (Number(BigInt.asIntN(32, (p >> 26n) << 1n)) | 0));
        return pc + 4;
      }
      case 0xAD: case 0xAE: case 0xAF: {
        const vd = code[pc + 1], a = this.rv(code[pc + 2]), b = this.rv(code[pc + 3]);
        this.setR(vd, this.checked(op, a, b)); return pc + 4;
      }
      case 0xB0: {
        const base = this.bump(16), i = (base - VBASE) >> 2;
        m32[i] = 0x116; m32[i + 1] = this.rv(code[pc + 2]); m32[i + 2] = 0; m32[i + 3] = 0;
        this.markStart(base);
        this.setR(code[pc + 1], base | 9); return pc + 3;
      }
      case 0xB1: case 0xB2: case 0xB3: {
        const s = this.rv(code[pc + 2]), off = this.rv(code[pc + 3]) >> 1;
        const a = this.ld32(s - 5) + off;
        if (a < VBASE || a >= heapEnd) this.memFault('sap-ref');
        if (op === 0xB1) this.setR(code[pc + 1], m8[a - VBASE] << 1);
        else this.setR(code[pc + 1], op === 0xB2 ? (this.dv.getInt32(a - VBASE, true) << 1) : this.dv.getInt32(a - VBASE, true));
        return pc + 4;
      }
      case 0xB4: case 0xB5: case 0xB6: {
        const s = this.rv(code[pc + 1]), off = this.rv(code[pc + 2]) >> 1, v = this.rv(code[pc + 3]);
        const a = this.ld32(s - 5) + off;
        if (a < VBASE || a >= heapEnd) this.memFault('sap-set');
        if (op === 0xB4) m8[a - VBASE] = (v >> 1) & 0xFF;
        else this.dv.setInt32(a - VBASE, op === 0xB5 ? (v >> 1) : v, true);
        return pc + 4;
      }
      case 0xB7: this.setR(code[pc + 1], this.ld32(this.rv(code[pc + 2]) - 5) << 1); return pc + 3;
      case 0xB8: this.st32(A_MVCOUNT, code[pc + 1] << 1); return pc + 2;
      case 0xB9: this.setR(code[pc + 1], this.allocObj(this.rv(code[pc + 2]), 0x31, false)); return pc + 3;
      case 0xBA: this.st32(A_CENV, this.rv(code[pc + 1])); return pc + 2;
      case 0xBB: this.setR(code[pc + 1], this.ld32(A_CENV)); return pc + 2;
      case 0xBC: this.st32(A_NARGS, code[pc + 1]); return pc + 2;
      case 0xBD: this.setR(code[pc + 1], this.ld32(A_NARGS) << 1); return pc + 2;
      case 0xBE: case 0xBF: case 0xC0: case 0xC1: {
        const x = this.floatVal(this.rv(code[pc + 2])), y = this.floatVal(this.rv(code[pc + 3]));
        const r = op === 0xBE ? x + y : op === 0xBF ? x - y : op === 0xC0 ? x * y : x / y;
        this.setR(code[pc + 1], this.allocFloat(r)); return pc + 4;
      }
      case 0xF4: this.setR(code[pc + 1], this.allocFloat(Math.sqrt(this.floatVal(this.rv(code[pc + 2]))))); return pc + 3;
      case 0xD0: this.setR(code[pc + 1], this.allocFloat(Math.fround(this.floatVal(this.rv(code[pc + 2]))), 0x64)); return pc + 3;
      case 0xC2: this.setR(code[pc + 1], this.allocFloat(this.rv(code[pc + 2]) >> 1)); return pc + 3;
      case 0xC3: {                            // ftoi: fistp dword (integer-indefinite out of range), then tag
        const t = Math.trunc(this.floatVal(this.rv(code[pc + 2])));
        const r = (Number.isFinite(t) && t >= -2147483648 && t < 2147483648) ? (t | 0) : (0x80000000 | 0);
        this.setR(code[pc + 1], r << 1); return pc + 3;
      }
      case 0xC4: { const x = this.floatVal(this.rv(code[pc + 1])), y = this.floatVal(this.rv(code[pc + 2])); this.cmp = x < y ? -1 : x > y ? 1 : 0; return pc + 3; }
      case 0xC5: case 0xC6: {
        const a = this.rv(code[pc + 2]), b = this.rv(code[pc + 3]);
        const r = op === 0xC5 ? (a + b) | 0 : (a - b) | 0;
        this.ovf = op === 0xC5 ? ((a ^ r) & (b ^ r)) < 0 : ((a ^ b) & (a ^ r)) < 0;
        this.setR(code[pc + 1], r); return pc + 4;
      }
      case 0xC7: return this.ovf ? pc + 5 + this.rd32(pc + 1) : pc + 5;
      case 0xC8: {
        const n = this.rv(code[pc + 2]) >> 1;
        if (n < 0 || n > 0xFFFFFF) this.memFault(`u8 vector of ${n}`);
        const total = align16(4 + n), base = this.bump(total);
        this.zero(base + 4, base + total);
        m32[(base - VBASE) >> 2] = (n << 8) | 0x11; this.markStart(base);
        this.setR(code[pc + 1], base | 9); return pc + 3;
      }
      case 0xC9: { const a = this.rv(code[pc + 2]) + (this.rv(code[pc + 3]) >> 1) - 5;
                   if (a < VBASE || a >= heapEnd) this.memFault('u8-ref');
                   this.setR(code[pc + 1], m8[a - VBASE] << 1); return pc + 4; }
      case 0xCA: { const a = this.rv(code[pc + 1]) + (this.rv(code[pc + 2]) >> 1) - 5;
                   if (a < VBASE || a >= heapEnd) this.memFault('u8-set');
                   m8[a - VBASE] = (this.rv(code[pc + 3]) >> 1) & 0xFF; return pc + 4; }
      case 0xCB: {
        const n = this.rv(code[pc + 2]) >> 1;
        if (n < 0 || n > 0xFFFFFF) this.memFault(`f32 vector of ${n}`);
        const total = align16(4 + 4 * n), base = this.bump(total);
        this.zero(base + 4, base + total);
        m32[(base - VBASE) >> 2] = (n << 8) | 0x12; this.markStart(base);
        this.setR(code[pc + 1], base | 9); return pc + 3;
      }
      case 0xCC: case 0xCE: case 0xD3: {
        const a = this.f32Lane(code[pc + 2], code[pc + 3]);
        if (op === 0xCC) this.setR(code[pc + 1], this.dv.getInt32(a, true) << 1);
        else if (op === 0xCE) this.setR(code[pc + 1], this.allocFloat(this.dv.getFloat32(a, true), 0x64));
        else this.fr[code[pc + 1]] = this.dv.getFloat32(a, true);
        return pc + 4;
      }
      case 0xCD: case 0xCF: case 0xD4: {
        const a = this.f32Lane(code[pc + 1], code[pc + 2]), s = code[pc + 3];
        if (op === 0xCD) this.dv.setInt32(a, this.rv(s) >> 1, true);
        else this.dv.setFloat32(a, op === 0xCF ? this.floatVal(this.rv(s)) : this.fr[s], true);
        return pc + 4;
      }
      case 0xD1: this.fr[code[pc + 1]] = Math.fround(this.floatVal(this.rv(code[pc + 2]))); return pc + 3;
      case 0xEB: this.fr[code[pc + 1]] = this.floatVal(this.rv(code[pc + 2])); return pc + 3;
      case 0xD2: this.setR(code[pc + 1], this.allocFloat(this.fr[code[pc + 2]], 0x64)); return pc + 3;
      case 0xEC: this.setR(code[pc + 1], this.allocFloat(this.fr[code[pc + 2]], 0x60)); return pc + 3;
      case 0xE1: this.fr[code[pc + 1]] = this.fr[code[pc + 2]]; return pc + 3;
      case 0xD5: case 0xD6: case 0xD7: case 0xD8: case 0xED: case 0xEE: case 0xEF: case 0xF0: {
        const fr = this.fr, x = fr[code[pc + 2]], y = fr[code[pc + 3]], k = op <= 0xD8 ? op - 0xD5 : op - 0xED;
        const r = k === 0 ? x + y : k === 1 ? x - y : k === 2 ? x * y : x / y;
        fr[code[pc + 1]] = op <= 0xD8 ? Math.fround(r) : r; return pc + 4;
      }
      case 0xF1: this.fr[code[pc + 1]] = this.rv(code[pc + 2]) >> 1; return pc + 3;
      case 0xF2: case 0xF3: {
        const x = this.fr[code[pc + 2]], y = this.fr[code[pc + 3]];
        this.setR(code[pc + 1], (op === 0xF2 ? x < y : x <= y) ? 2 : 0); return pc + 4;
      }
      case 0xF7: {
        const x = this.fr[code[pc + 2]], y = this.fr[code[pc + 3]];
        this.setR(code[pc + 1], (code[pc + 4] === 0 ? x < y : x <= y) ? 2 : 0); return pc + 5;
      }
      case 0xF5: {
        const x = this.fr[code[pc + 2]], sub = code[pc + 3];
        this.fr[code[pc + 1]] = sub < 2 ? Math.abs(x) : sub === 2 ? x : Math.fround(x); return pc + 4;
      }
      case 0xF6: {
        const x = this.fr[code[pc + 2]], y = this.fr[code[pc + 3]];
        this.fr[code[pc + 1]] = (code[pc + 4] & 1) === 0 ? (y > x ? y : x) : (y < x ? y : x); return pc + 5;
      }
      case 0xF8: {
        const x = this.fr[code[pc + 2]], k = code[pc + 3] & 3;
        let r;
        if (k === 0) { const f = Math.floor(x), d = x - f; r = d < 0.5 ? f : d > 0.5 ? f + 1 : (f % 2 === 0 ? f : f + 1); }
        else r = k === 1 ? Math.trunc(x) : k === 2 ? Math.floor(x) : Math.ceil(x);
        const w = (Number.isFinite(r) && r >= -2147483648 && r < 2147483648) ? (r | 0) : (0x80000000 | 0);
        this.setR(code[pc + 1], w << 1); return pc + 4;
      }
      case 0xF9: this.fr[code[pc + 1]] *= Math.pow(2, this.rv(code[pc + 2]) >> 1); return pc + 3;
      case 0xFA: case 0xFB: {
        const hi = (code[pc + 2] | (code[pc + 3] << 8) | (code[pc + 4] << 16) | (code[pc + 5] << 24)) >>> 0;
        const lo = (code[pc + 6] | (code[pc + 7] << 8) | (code[pc + 8] << 16) | (code[pc + 9] << 24)) >>> 0;
        this.f64u32[0] = lo; this.f64u32[1] = hi;
        this.fr[code[pc + 1]] = op === 0xFB ? Math.fround(this.f64[0]) : this.f64[0]; return pc + 10;
      }
      default: this.fault(`unknown opcode 0x${op.toString(16)}`);
    }
  }

  // Checked arithmetic: the fast result, or the generic (bignum) entry on a
  // 32-bit overflow.  Returns the result.
  checked(op, a, b) {
    let r, ovf;
    if (op === 0xAE) { r = (a + b) | 0; ovf = ((a ^ r) & (b ^ r)) < 0; }
    else if (op === 0xAF) { r = (a - b) | 0; ovf = ((a ^ b) & (a ^ r)) < 0; }
    else { const x = a >> 1, p = x * b; r = Math.imul(x, b); ovf = p !== r; }
    if (!ovf) return r;
    const gen = op === 0xAE ? this.genAdd : op === 0xAF ? this.genSub : this.genMul;
    return gen ? this.callLisp(gen, [a, b]) : r;
  }

  callFn(t) {
    this.push(RET_SENTINEL);
    this.enter(t);
    const f = this.compiledFor(t);
    if (f) this.drive(f);
    else this.run();
  }
  // R0: F has already run once and returned R0 (the direct-call fast path
  // hands over a call or tail-call request it cannot finish itself).
  // R0: F already answered with request R0, before this drive -- its callee
  // is entered, so the VM's ebp is THAT frame and F's own is EBP0 (the saved
  // ebp at [vm.ebp]).  Recording the callee's frame as F's filed F's
  // continuation under the wrong frame, and a longjmp to one of F's landings
  // then found no owner.
  drive(f, L0 = 0, r0 = undefined, ebp0 = this.ebp) {
    const cf = this.contF, cl = this.contL, ce = this.contE;
    const base = this.contDepth;
    let d = base;
    let L = L0, ebp = ebp0;
    const jd0 = ++this.jd;
    try {
      for (;;) {
        let r;
        if (r0 !== undefined) { r = r0; r0 = undefined; }
        else try { r = f(this, L); }
        catch (e0) {
          const e = ljOf(e0);
          if (!e) throw e0;
          this.jd = jd0;          // fast-path direct calls it unwound did not count down
          let found = false;
          if (ebp === this.ebp && f.RES && f.RES[e.ip] !== undefined) { L = f.RES[e.ip]; found = true; if (DBG.MVM_WASM_DEBUG) this.dumpFrame('drive'); }
          while (!found && d > base) {
            d--;
            if (ce[d] === this.ebp && cf[d].RES && cf[d].RES[e.ip] !== undefined) { f = cf[d]; L = cf[d].RES[e.ip]; ebp = ce[d]; found = true; }
          }
          if (!found) throw e0;
          continue;
        }
        if (r < 0) {
          if (d === base) return;
          d--; f = cf[d]; L = cl[d]; ebp = ce[d];
          continue;
        }
        const t = r >> 1;
        if ((r & 1) === 0) {
          if (d >= cf.length) { cf.push(null); cl.push(0); ce.push(0); }
          cf[d] = f; cl[d] = this.resumeL; ce[d] = ebp; d++;
        }
        let g = this.nextF;
        if (g === undefined) g = this.compiledFor(t); else this.nextF = undefined;
        if (g) { this.contDepth = d; f = g; L = 0; ebp = this.ebp; }
        else {
          this.contDepth = d;
          try { this.run(); }
          catch (e0) {
            const e = ljOf(e0);
            if (!e) throw e0;
            this.jd = jd0;
            let found = false;
            while (!found && d > base) {
              d--;
              if (ce[d] === this.ebp && cf[d].RES && cf[d].RES[e.ip] !== undefined) { f = cf[d]; L = cf[d].RES[e.ip]; ebp = ce[d]; found = true; }
            }
            if (!found) throw e0;
            continue;
          }
          if (d === base) return;
          d--; f = cf[d]; L = cl[d]; ebp = ce[d];
        }
      }
    } finally { this.contDepth = base; this.jd--; }
  }
  compiledFor(t) {
    if (!this.compileOn || this.trace > 1) return null;
    const f = this.compiled.get(t);
    if (f !== undefined) return f;
    const n = (this.callCounts.get(t) || 0) + 1;
    if (n < this.compileThreshold) { this.callCounts.set(t, n); return null; }
    const t0 = performance.now();
    let g = null;
    if (WASM && this.wmem) g = WASM.compile(this, t, false);
    if (g && g.wexp) WASM.install(this, t, g);
    if (!g) g = this.compileFn(t);
    this.compileStats.ms += performance.now() - t0;
    this.compiled.set(t, g);
    return g;
  }
  forceCompile(name) {
    const f = this.byName.get(name);
    if (!f) return false;
    if (this.compiled.get(f.off) === undefined) this.compiled.set(f.off, this.compileFn(f.off));
    return this.compiled.get(f.off) != null;
  }
  forceCompileMatching(re) {
    let n = 0;
    for (const f of this.mod.fns) if (re.test(f.name) && this.forceCompile(f.name)) n++;
    return n;
  }

  // -- bytecode -> JS (as mvm.js, one int per register) ------------------------------
  // MVM_SHAPES=1: translated code counts every instruction it executes, keyed
  // by pc; shapeReport() aggregates by opcode (VFP-relative frame accesses
  // apart) and by the opcode pair that ran back to back.  For choosing what
  // the emitter should fuse or stop checking.
  shapeId(pc, op, b1, b2) {
    if (!this.shapeIds) { this.shapeIds = new Map(); this.SC = []; this.shapeMeta = []; }
    let id = this.shapeIds.get(pc);
    if (id === undefined) {
      id = this.SC.length; this.shapeIds.set(pc, id); this.SC.push(0);
      const vfp = ((op === 0x61 || op === 0x65) && b2 === 21) || (op === 0x62 && b1 === 21);
      this.shapeMeta.push({ pc, op, key: op.toString(16).padStart(2, '0') + (vfp ? '/vfp' : '') });
    }
    return id;
  }
  shapeReport(n = 40) {
    if (!this.SC) return '[shapes: nothing translated]';
    const by = new Map(), pairs = new Map(), fns = new Map();
    let tot = 0;
    const byPc = new Map(); for (const m of this.shapeMeta) byPc.set(m.pc, m);
    for (let i = 0; i < this.SC.length; i++) {
      const c = this.SC[i]; if (!c) continue;
      const m = this.shapeMeta[i]; tot += c;
      by.set(m.key, (by.get(m.key) || 0) + c);
      const nx = byPc.get(m.pc + INSN_LEN[m.op]);
      if (nx) { const k = m.key + ' ' + nx.key; pairs.set(k, (pairs.get(k) || 0) + Math.min(c, this.SC[this.shapeIds.get(nx.pc)])); }
      const w = this.where(m.pc).replace(/\+\d+$/, ''); fns.set(w, (fns.get(w) || 0) + c);
    }
    const top = (mp) => [...mp.entries()].sort((a, b) => b[1] - a[1]).slice(0, n)
      .map(([k, c]) => `${(100 * c / tot).toFixed(2).padStart(6)}% ${String(c).padStart(11)} ${k}`).join('\n');
    return `[shapes: ${tot} translated insns executed since reset #${this.shapeResets}]\nOPS\n${top(by)}\nPAIRS\n${top(pairs)}\nFUNCTIONS\n${top(fns)}`;
  }
  compileFn(entry, osr = false) {
    const m8 = this.m8;
    if (m8[entry] === 0x72 && m8[entry + 1] === 0x72) return null;   // late-binding thunk: stays interpreted
    const rd32 = (p) => (m8[p] | (m8[p + 1] << 8) | (m8[p + 2] << 16) | (m8[p + 3] << 24));
    const starts = new Set([entry]);
    const seen = new Set();
    const work = [entry];
    const insns = [];
    let hasSetjmp = false;
    while (work.length) {
      let pc = work.pop();
      while (!seen.has(pc)) {
        seen.add(pc);
        const op = m8[pc], len = INSN_LEN[op];
        if (len === 0) { this.compileStats.failed++; return null; }
        insns.push([pc, op, len]);
        if (op >= 0x40 && op <= 0x46 || op === 0xC7) {
          const t = pc + len + rd32(pc + 1); starts.add(t); work.push(t);
          if (op === 0x40) break;
          starts.add(pc + len);
        } else if (op === 0x47 || op === 0x48) {
          const t = pc + len + rd32(pc + 2); starts.add(t); work.push(t); starts.add(pc + len);
        } else if (op === 0x82 || op === 0x83) break;
        else if (op === 0x80 || op === 0x81) starts.add(pc + len);
        else if (op === 0x02 && (m8[pc + 1] | (m8[pc + 2] << 8)) === 0x0510) { hasSetjmp = true; starts.add(pc + len); }
        else if (op === 0x90 || op === 0x91 || op === 0xA2 || op === 0x01) { this.compileStats.failed++; return null; }
        pc += len;
      }
    }
    insns.sort((a, b) => a[0] - b[0]);
    const insnIdx = new Map(); for (let i = 0; i < insns.length; i++) insnIdx.set(insns[i][0], i);
    const label = new Map(); let nl = 0;
    for (const [pc] of insns) if (starts.has(pc)) label.set(pc, nl++);
    // Instructions per JS function: a jump between chunks returns to the
    // dispatcher and reloads every register, and past ~800 V8 stops optimizing
    // the function.  Measured (CPU, BOYER / FRPOLY): 150 -> 450 is 15-25%
    // faster, 300..600 a plateau, 800 and up much slower.
    const CHUNK = DBG.MVM_CHUNK ? parseInt(DBG.MVM_CHUNK, 10) : 450;
    const labelChunk = new Map(); let nch = 0, acc = 0;
    for (const [pc] of insns) {
      if (label.has(pc)) { if (acc >= CHUNK) { nch++; acc = 0; } labelChunk.set(label.get(pc), nch); }
      acc++;
    }
    nch++;
    // V0-V8 in JS locals; V9-V15 and frame slots in memory (as mvm.js).
    const NLOC = 9;
    const R = (v) => v < NLOC ? `r${v}` : v < 16 ? `m32[fb+(${IX[v]})]` : v === 16 ? 'vm.vrl' : v === 17 ? 'vm.va' : v === 18 ? 'vm.vl' : v === 19 ? `${NIL}` : v === 20 ? 'vm.esp' : v === 21 ? 'vm.ebp' : 'vm.pc';
    const W = (v, x) => v < NLOC ? `r${v}=${x};` : v < 16 ? `m32[fb+(${IX[v]})]=${x};` : v === 16 ? `vm.vrl=${x};` : `vm.setR(${v},${x});`;
    let STORE = '', LOAD = '', STORE5 = '', DECL = '';
    for (let v = 0; v < NLOC; v++) {
      STORE += `m32[fb+(${IX[v]})]=r${v};`;
      LOAD += `r${v}=m32[fb+(${IX[v]})];`;
      if (v <= 4) STORE5 += `m32[fb+(${IX[v]})]=r${v};`;
      DECL += `let r${v}=0;`;
    }
    // FRAME SLOTS IN JS LOCALS: each constant-index slot this function names
    // (OBJ-REF / OBJ-SET on VFP) is read from a local s<k>; a write goes to
    // the local AND the frame (write-through), so memory is always current --
    // the collector's stack scan, a dynamic-index AREF on VFP and a re-entry
    // read it there -- and nothing has to be stored before a call.  The
    // locals are reloaded wherever the registers are (entry, after a
    // collection, a trap or a delegated instruction).  OFF BY DEFAULT
    // (MVM_SLOT_LOCALS=1 turns it on): on benchmark loops it measured within
    // noise, and loading cl-bench it cost ~5% CPU (the reloads make every
    // translated function bigger for V8 to compile).
    const slotSet = new Set();
    if (DBG.MVM_SLOT_LOCALS && !DBG.MVM_NO_SLOT_LOCALS)
      for (const [pc, op] of insns) {
        if (op === 0x61 && m8[pc + 2] === 21) slotSet.add(m8[pc + 3]);
        else if (op === 0x62 && m8[pc + 1] === 21) slotSet.add(m8[pc + 2]);
      }
    let SLOAD = '';
    for (const k of slotSet) { SLOAD += `s${k}=m32[fb+(${slotOff(k) >> 2})];`; DECL += `let s${k}=0;`; }
    LOAD += SLOAD;
    // A call leaves this function; V4-V8 come back from the frame on resume
    // (*CALL-TEMPS-SURVIVE*: see mvm.js).
    const LOAD48 = [4, 5, 6, 7, 8].map((v) => `r${v}=m32[fb+(${IX[v]})];`).join('') + SLOAD;
    const ENTRY = `if(L===0){${[0, 1, 2, 3, 4].map((v) => `r${v}=m32[fb+(${IX[v]})];`).join('')}}else{${[4, 5, 6, 7, 8].map((v) => `r${v}=m32[fb+(${IX[v]})];`).join('')}}${SLOAD}`;
    let curChunk = 0;
    const J = (t) => { const l = label.get(t); return labelChunk.get(l) === curChunk ? `{L=${l};continue;}` : `{${STORE}return ${-(2 + l)};}`; };
    const chk = (a, what) => `if(${a}<${VBASE}||${a}>=HE)vm.memFault(${JSON.stringify(what)});`;
    const outs = new Array(nch).fill('');
    let out = '';
    const sites = []; let nsite = 0;
    for (const [pc, op, len] of insns) {
      if (label.has(pc)) {
        const l = label.get(pc), ch = labelChunk.get(l);
        if (ch !== curChunk) {
          out += `{${STORE}return ${-(2 + l)};}\n`;
          outs[curChunk] = out; out = ''; curChunk = ch;
        }
        out += `case ${l}:\n`;
      }
      const b1 = m8[pc + 1], b2 = m8[pc + 2], b3 = m8[pc + 3], b4 = m8[pc + 4];
      const P = `vm.pc=${pc};`;
      if (SHAPES) out += `vm.SC[${this.shapeId(pc, op, b1, b2)}]++;`;
      switch (op) {
        case 0x00: break;
        case 0x02: {
          const c = b1 | (b2 << 8);
          if ((c < 0x100 || c >= 0x8000) && frameEnterParams(c) <= 4) break;
          if (c >= 0x100 && c < 0x300) break;
          out += `${P}${STORE}vm.trap(${c},${pc + len});fb=(vm.ebp-${VBASE})>>2;${LOAD}`;
          break;
        }
        case 0x10: out += W(b1, R(b2)); break;
        case 0x11: out += W(b1, `${rd32(pc + 2)}`); break;
        case 0x12: out += `vm.push(${R(b1)});`; break;
        case 0x13: out += W(b1, 'vm.pop()'); break;
        case 0x14: {
          const idx = rd32(pc + 2);
          if (pc >= JIT_PHYS) out += `{const v=vm.ld32(${A_WEB_CONSTS});if(v===0)vm.fault('li-const: no constant vector');${W(b1, `m32[((v-${VBASE + 5 - 4 * idx})>>2)]`)}}`;
          else { const off = this.mod.addrTable[idx] | 0; out += W(b1, `${off === 0 ? 0 : POOL_ADDR + off}`); }
          break;
        }
        case 0x20: out += W(b1, `(${R(b2)}+${R(b3)})|0`); break;
        case 0x21: out += W(b1, `(${R(b2)}-${R(b3)})|0`); break;
        case 0x22: out += W(b1, `Math.imul(${R(b2)}>>1,${R(b3)})`); break;
        case 0x25: out += W(b1, `(-${R(b2)})|0`); break;
        case 0x26: out += W(b1, `(${R(b1)}+2)|0`); break;
        case 0x27: out += W(b1, `(${R(b1)}-2)|0`); break;
        case 0x28: out += W(b1, `${R(b2)}&${R(b3)}`); break;
        case 0x29: out += W(b1, `${R(b2)}|${R(b3)}`); break;
        case 0x2A: out += W(b1, `${R(b2)}^${R(b3)}`); break;
        case 0x2B: out += W(b1, b3 >= 32 ? '0' : `${R(b2)}<<${b3}`); break;
        case 0x2C: out += W(b1, b3 >= 32 ? '0' : `(${R(b2)}>>>${b3})|0`); break;
        case 0x2D: out += W(b1, `${R(b2)}>>${Math.min(b3, 31)}`); break;
        case 0x2E: out += W(b1, `(${R(b2)}>>>${b3 & 31})&${ldbMask(b4)}`); break;
        case 0x2F: out += W(b1, `shlv(${R(b2)},${R(b3)}>>1)`); break;
        case 0x32: out += W(b1, `sarv(${R(b2)},${R(b3)}>>1)`); break;
        case 0x30: out += `{const a=${R(b1)},b=${R(b2)};cmp=a<b?-1:a>b?1:0;}`; break;
        case 0x31: out += `{const r=${R(b1)}&${R(b2)};cmp=r===0?0:(r<0?-1:1);}`; break;
        case 0x33: { const i = rd32(pc + 2); out += `{const a=${R(b1)};cmp=a<${i}?-1:a>${i}?1:0;}`; break; }
        case 0x34: { const i = rd32(pc + 2); out += `{const r=${R(b1)}&${i};cmp=r===0?0:(r<0?-1:1);}`; break; }
        case 0x68: out += `${P}${W(b1, `vm.allocObj(${R(b2)},0x32,true)`)}`; break;
        case 0xB9: out += `${P}${W(b1, `vm.allocObj(${R(b2)},0x31,false)`)}`; break;
        case 0xC9: out += `${P}{const a=${R(b2)}+(${R(b3)}>>1)-5;${chk('a', 'u8-ref')}${W(b1, `m8[a-${VBASE}]<<1`)}}`; break;
        case 0xCA: out += `${P}{const a=${R(b1)}+(${R(b2)}>>1)-5;${chk('a', 'u8-set')}m8[a-${VBASE}]=(${R(b3)}>>1)&255;}`; break;
        case 0x40: out += J(pc + len + rd32(pc + 1)); break;
        case 0x41: out += `if(cmp===0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x42: out += `if(cmp!==0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x43: out += `if(cmp<0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x44: out += `if(cmp>=0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x45: out += `if(cmp<=0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x46: out += `if(cmp>0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x47: out += `if(${R(b1)}===${NIL})${J(pc + len + rd32(pc + 2))}`; break;
        case 0x48: out += `if(${R(b1)}!==${NIL})${J(pc + len + rd32(pc + 2))}`; break;
        case 0x50: case 0x51: {
          const d = op === 0x50 ? -1 : 3;
          // Straight after the compiler's own cons guard -- OBJ-TAG t x;
          // CMP-IMM t 2; BNE slow; with no other way in -- X is a cons, so
          // its tag and bounds need no second check (68k's cfde9c0f).
          const i = insnIdx.get(pc);
          const g1 = insns[i - 1], g2 = insns[i - 2], g3 = insns[i - 3];
          if (i >= 3 && !label.has(pc) && g1[1] === 0x42 && g1[0] + 5 === pc
              && g2[1] === 0x33 && g2[0] + 6 === g1[0] && rd32(g2[0] + 2) === 2
              && g3[1] === 0x63 && g3[0] + 3 === g2[0] && m8[g3[0] + 1] === m8[g2[0] + 1]
              && m8[g3[0] + 2] === b2 && m8[g3[0] + 1] !== b2) {
            out += W(b1, `m32[(${R(b2)}-${VBASE - d})>>2]`);
            break;
          }
          out += `${P}{const x=${R(b2)},a=x+(${d});if((x&7)!==1||a<${VBASE}||a>=HE)vm.memFault('car/cdr of non-cons');${W(b1, `m32[(a-${VBASE})>>2]`)}}`;
          break;
        }
        case 0x52: out += `${P}${W(b1, `vm.allocCons(${R(b2)},${R(b3)})`)}`; break;
        case 0x53: case 0x54: {
          const d = op === 0x53 ? -1 : 3;
          out += `${P}{const x=${R(b1)},a=x+(${d});if((x&7)!==1||a<${VBASE}||a>=HE)vm.memFault('rplac on non-cons');m32[(a-${VBASE})>>2]=${R(b2)};}`;
          break;
        }
        case 0x55: out += `{const x=${R(b2)};${W(b1, `(x!==${NIL}&&(x&15)===1)?${TV}:${NIL}`)}}`; break;
        case 0x56: out += `{const x=${R(b2)};${W(b1, `(x!==${NIL}&&(x&15)===1)?${NIL}:${TV}`)}}`; break;
        case 0x60: out += `${P}${W(b1, `vm.allocObj(${b2 | (b3 << 8)},${b4},true)`)}`; break;
        case 0x61: {
          if (b2 === 21) out += W(b1, slotSet.has(b3) ? `s${b3}` : `m32[fb+(${slotOff(b3) >> 2})]`);
          else out += `${P}{const a=${R(b2)}+${4 * b3 - 5};${chk('a', 'obj-ref')}${W(b1, `m32[(a-${VBASE})>>2]`)}}`;
          break;
        }
        case 0x62: {
          if (b1 === 21) out += slotSet.has(b2) ? `m32[fb+(${slotOff(b2) >> 2})]=s${b2}=${R(b3)};` : `m32[fb+(${slotOff(b2) >> 2})]=${R(b3)};`;
          else out += `${P}{const a=${R(b1)}+${4 * b2 - 5};${chk('a', 'obj-set')}m32[(a-${VBASE})>>2]=${R(b3)};}`;
          break;
        }
        case 0x63: out += W(b1, `(${R(b2)}&15)<<1`); break;
        case 0x64: out += `{const x=${R(b2)};let r=0;if((x&15)===9&&x!==${TV}&&x-9>=${VBASE}&&x-9<HE)r=(m32[(x-9-${VBASE})>>2]&255)<<1;${W(b1, 'r')}}`; break;
        case 0x65: if (b2 === 21) { out += `{const k=${R(b3)}>>1;${W(b1, `m32[fb+(k<${SLOT_SPLIT}?${SLOT_BASE >> 2}-k:${-FRAME_SIZE >> 2}-(k-${SLOT_SPLIT}))]`)}}`; break; }
                   out += `${P}{const a=${R(b2)}+${R(b3)}*2-5;${chk('a', 'aref')}${W(b1, `m32[(a-${VBASE})>>2]`)}}`; break;
        case 0x66: out += `${P}{const a=${R(b1)}+${R(b2)}*2-5;${chk('a', 'aset')}m32[(a-${VBASE})>>2]=${R(b3)};}`; break;
        case 0x67: out += `{const x=${R(b2)};let r=0;if((x&15)===9&&x!==${TV}&&x-9>=${VBASE}&&x-9<HE)r=(m32[(x-9-${VBASE})>>2]>>>8)<<1;${W(b1, 'r')}}`; break;
        case 0x70: {
          const w = b3 & 3;
          out += `${P}{const a=${R(b2)};${chk('a', 'load')}`;
          if (w === 0) out += W(b1, `m8[a-${VBASE}]`);
          else if (w === 1) out += W(b1, `vm.ld16(a)`);
          else out += W(b1, `((a&3)===0?m32[(a-${VBASE})>>2]:vm.dv.getInt32(a-${VBASE},true))`);
          out += '}';
          break;
        }
        case 0x71: {
          const w = b3 & 3;
          out += `${P}{const a=${R(b1)};${chk('a', 'store')}`;
          if (w === 0) out += `m8[a-${VBASE}]=${R(b2)}&255;`;
          else if (w === 1) out += `vm.st16(a,${R(b2)}&65535);`;
          else out += `if((a&3)===0)m32[(a-${VBASE})>>2]=${R(b2)};else vm.dv.setInt32(a-${VBASE},${R(b2)},true);`;
          out += '}';
          break;
        }
        // W32, open-coded where it is one JS operator
        case 0x69: out += W(b1, `${rd32(pc + 2)}`); break;
        case 0x6A: out += W(b1, `(${R(b2)}+${R(b3)})|0`); break;
        case 0x6B: out += W(b1, `(${R(b2)}-${R(b3)})|0`); break;
        case 0x6C: out += W(b1, `Math.imul(${R(b2)},${R(b3)})`); break;
        case 0x6E: out += W(b1, `${R(b2)}&${R(b3)}`); break;
        case 0x6F: out += W(b1, `${R(b2)}|${R(b3)}`); break;
        case 0x73: out += W(b1, `${R(b2)}^${R(b3)}`); break;
        case 0x74: out += W(b1, `~${R(b2)}`); break;
        case 0x75: out += W(b1, `${R(b2)}<<${b3 & 31}`); break;
        case 0x76: out += W(b1, `(${R(b2)}>>>${b3 & 31})|0`); break;
        case 0x77: out += W(b1, `${R(b2)}>>${b3 & 31}`); break;
        case 0x78: { const n = b3 & 31; out += n === 0 ? W(b1, R(b2)) : `{const x=${R(b2)};${W(b1, `(x<<${n})|(x>>>${32 - n})`)}}`; break; }
        case 0x79: out += W(b1, `((${R(b2)}>>>0)<(${R(b3)}>>>0))?2:0`); break;
        case 0x7A: out += W(b1, `(${R(b2)}<${R(b3)})?2:0`); break;
        case 0x7B: out += W(b1, `(${R(b2)}===${R(b3)})?2:0`); break;
        case 0x7C: out += W(b1, `${R(b2)}>>1`); break;
        case 0x7D: out += W(b1, `(${R(b2)}>>>16)<<1`); break;
        case 0x7E: out += W(b1, `(${R(b2)}&65535)<<1`); break;
        case 0x7F: out += W(b1, `(((${R(b2)}>>1)<<16)|((${R(b3)}>>1)&65535))|0`); break;
        case 0x72: case 0x8A: case 0x8B: case 0x92: break;
        case 0x80: case 0x81: {
          if (op === 0x80) {
            const t = rd32(pc + 1) >>> 0;
            const c = `c${nsite++}`;
            sites.push(c);
            // DIRECT CALL: a compiled callee runs in a nested drive() and this
            // function continues at the next instruction with its registers
            // still in JS locals (reloaded only if a collection moved objects).
            // Past JD_MAX nested drives, or for a callee not translated yet, the
            // call returns to the driver as before.  The site re-asks for an
            // untranslated callee: caching the first answer kept a callee that
            // crossed the threshold later interpreted from this site for good.
            out += `${P}${STORE}if(!${c})${c}=vm.compiledFor(${t});if(${c}&&vm.jd<${JD_MAX}){vm.push(${RET_SENTINEL});vm.enter(${t});const g0=vm.gcCount;if(${c}.RES)vm.drive(${c});else{vm.jd++;const q=${c}(vm,0);vm.jd--;if(q!==-1)vm.drive(${c},0,q,vm.ld32(vm.ebp));}if(vm.gcCount!==g0){${LOAD48}}}else{vm.resumeL=${label.get(pc + len)};vm.nextF=${c};vm.push(${RET_SENTINEL});vm.enter(${t});return ${t * 2};}`;
          } else {
            out += `${P}${STORE}{const t=vm.fnAddrToOffset(${R(b1)}),g=vm.compiledFor(t);if(g&&vm.jd<${JD_MAX}){vm.push(${RET_SENTINEL});vm.enter(t);const g0=vm.gcCount;if(g.RES)vm.drive(g);else{vm.jd++;const q=g(vm,0);vm.jd--;if(q!==-1)vm.drive(g,0,q,vm.ld32(vm.ebp));}if(vm.gcCount!==g0){${LOAD48}}}else{vm.resumeL=${label.get(pc + len)};vm.nextF=g;vm.push(${RET_SENTINEL});vm.enter(t);return t*2;}}`;
          }
          break;
        }
        case 0x82: out += `vm.doRet();return -1;`; break;
        case 0x83: out += `${P}${STORE5}{const t=${rd32(pc + 1) >>> 0};vm.doTailcall(t);return t*2+1;}`; break;
        case 0x88: out += `${P}{const b=vm.bump(16);vm.zero(b,b+16);vm.markStart(b);vm.markCons(b);${W(b1, 'b|1')}}`; break;
        case 0x89: out += `if(vm.va>=vm.vl){${P}${STORE}vm.gc();${LOAD}}`; break;
        case 0xA7: { const t = rd32(pc + 2) >>> 0; out += W(b1, `${t === FN_UNRESOLVED ? NIL : ((t << 4) | 3)}`); break; }
        case 0xB8: out += `m32[${(A_MVCOUNT - VBASE) >> 2}]=${b1 << 1};`; break;
        case 0xBA: out += `m32[${(A_CENV - VBASE) >> 2}]=${R(b1)};`; break;
        case 0xBB: out += W(b1, `m32[${(A_CENV - VBASE) >> 2}]`); break;
        case 0xBC: out += `m32[${(A_NARGS - VBASE) >> 2}]=${b1};`; break;
        case 0xBD: out += W(b1, `m32[${(A_NARGS - VBASE) >> 2}]<<1`); break;
        case 0xC5: case 0xC6: {
          const ov = op === 0xC5 ? `((a^r)&(b^r))<0` : `((a^b)&(a^r))<0`;
          out += `{const a=${R(b2)},b=${R(b3)},r=(a${op === 0xC5 ? '+' : '-'}b)|0;ovf=${ov};${W(b1, 'r')}}`;
          break;
        }
        case 0xC7: out += `if(ovf)${J(pc + len + rd32(pc + 1))}`; break;
        case 0xAE: case 0xAF: {
          const ov = op === 0xAE ? `((a^r)&(b^r))<0` : `((a^b)&(a^r))<0`;
          out += `{const a=${R(b2)},b=${R(b3)};let r=(a${op === 0xAE ? '+' : '-'}b)|0;if(${ov}){${P}${STORE}r=vm.checked(${op},a,b);${LOAD}}${W(b1, 'r')}}`;
          break;
        }
        case 0xAD: out += `{const a=${R(b2)},b=${R(b3)},x=a>>1;let r=Math.imul(x,b);if(x*b!==r){${P}${STORE}r=vm.checked(${op},a,b);${LOAD}}${W(b1, 'r')}}`; break;
        default:
          out += `${P}${STORE}vm.execInsn(${pc});${LOAD}`;
          if (DBG.MVM_DCOUNT) out += `vm.dcount.set(${op},(vm.dcount.get(${op})||0)+1);`;
          this.compileStats.delegated.set(op, (this.compileStats.delegated.get(op) || 0) + 1);
          if (op === 0x31 || op === 0xC4) out += `cmp=vm.cmp;`;
          break;
      }
      out += '\n';
      if (op === 0x82 || op === 0x83 || op === 0x40) out += `vm.fault('fell off a block');\n`;
    }
    outs[curChunk] = out;
    const ENTRYX = `if(L===0){${[0, 1, 2, 3, 4].map((v) => `r${v}=m32[fb+(${IX[v]})];`).join('')}${SLOAD}}else{${LOAD}}`;
    // An OSR entry (a loop head, mid-function) loads every register: they are
    // all live there, not just the arguments.
    const mk = (o, i) => `function(vm,L){const m32=vm.m32,m8=vm.m8;let fb=(vm.ebp-${VBASE})>>2;const HE=vm.heapEnd;let cmp=0,ovf=false;${DECL}${osr ? LOAD : i === 0 && nch === 1 ? ENTRY : ENTRYX}for(;;)switch(L){\n${o}default: vm.fault('bad block label '+L);\n}}`;
    let body;
    if (nch === 1) body = `return ${mk(outs[0], 0)};`;
    else {
      const lc = []; for (const [l, c] of labelChunk) lc[l] = c;
      body = `const CH=[${outs.map(mk).join(',')}];const LC=[${lc.join(',')}];return function(vm,L){for(;;){const r=CH[LC[L]](vm,L);if(r>=-1)return r;L=-2-r;}};`;
    }
    if (DBG.MVM_DUMP && this.where(entry).startsWith(DBG.MVM_DUMP)) this.host.log(`[compiled ${this.where(entry)}]\n${body}`);
    let f;
    const decl = sites.length ? `let ${sites.map((c) => c + '=undefined').join(',')};` : '';
    try { f = eval(`(function(){${decl}${body}})()`); }
    catch (e) { this.host.log(`[compile ${this.where(entry)}: ${e.message}]`); this.compileStats.failed++; return null; }
    if (hasSetjmp) { f.RES = {}; for (const [pc, l] of label) f.RES[pc] = l; }
    f.ENTRYL = label.get(entry);
    this.compileStats.fns++; this.compileStats.insns += insns.length;
    return f;
  }

  // -- snapshots (same container as mvm.js, version 32) -----------------------------
  snapshot() {
    this.gc();
    const fromStart = this.ld32(A_GC_FROM);
    const ranges = [
      [VBASE, BSS_END],
      [POOL_ADDR, POOL_ADDR + this.mod.pool.length],
      [this.esp, STACK_ADDR + STACK_SIZE],
      [fromStart, this.va],
      [JIT_ADDR, this.mmapNext],
    ];
    const g0 = (fromStart - this.pageBase) >> 4, g1 = (this.va - this.pageBase) >> 4;
    return {
      version: 32,
      semi: this.semi,
      regs: { vrl: this.vrl, vrh: 0, va: this.va, vl: this.vl, esp: this.esp, ebp: this.ebp, pc: this.pc,
              cmp: this.cmp, ovf: this.ovf, gcCount: this.gcCount, mmapNext: this.mmapNext, steps: this.steps },
      ranges: ranges.map(([a, b]) => ({ addr: a, bytes: this.m8.slice(a - VBASE, b - VBASE) })),
      bitmaps: { g0, startBmp: this.startBmp.slice(g0 >> 3, (g1 >> 3) + 1),
                 consBmp: this.consBmp.slice(g0 >> 3, (g1 >> 3) + 1) },
    };
  }
  restore(core, argv, env) {
    if (core.version !== 32) throw new Error('core version mismatch (not a 32-bit core)');
    if (core.semi !== this.semi) throw new Error(`core was made with a ${core.semi >> 20} MB semispace`);
    this.m8.fill(0);
    this.installModule();
    for (const r of core.ranges) this.m8.set(r.bytes, r.addr - VBASE);
    this.startBmp.fill(0); this.consBmp.fill(0);
    this.startBmp.set(core.bitmaps.startBmp, core.bitmaps.g0 >> 3);
    this.consBmp.set(core.bitmaps.consBmp, core.bitmaps.g0 >> 3);
    const r = core.regs;
    this.vrl = r.vrl; this.va = r.va; this.vl = r.vl; this.esp = r.esp; this.ebp = r.ebp; this.pc = r.pc;
    this.cmp = r.cmp; this.ovf = r.ovf; this.gcCount = r.gcCount; this.mmapNext = r.mmapNext; this.steps = r.steps;
    this.stageArgv(argv, env);
  }
  static encodeCore(core) {
    const blobs = [];
    const hdr = {
      version: core.version, semi: core.semi, regs: core.regs,
      ranges: core.ranges.map((r) => { blobs.push(r.bytes); return { addr: r.addr, len: r.bytes.length }; }),
      bitmaps: { g0: core.bitmaps.g0, startLen: core.bitmaps.startBmp.length, consLen: core.bitmaps.consBmp.length },
    };
    blobs.push(core.bitmaps.startBmp, core.bitmaps.consBmp);
    const hb = new TextEncoder().encode(JSON.stringify(hdr));
    let total = 8 + hb.length; for (const b of blobs) total += b.length;
    const out = new Uint8Array(total);
    out.set([0x4D, 0x56, 0x4D, 0x43], 0);
    new DataView(out.buffer).setUint32(4, hb.length, true);
    out.set(hb, 8);
    let p = 8 + hb.length;
    for (const b of blobs) { out.set(b, p); p += b.length; }
    return out;
  }
  static decodeCore(bytes) {
    if (bytes[0] !== 0x4D || bytes[1] !== 0x56 || bytes[2] !== 0x4D || bytes[3] !== 0x43) throw new Error('not an MVMC core');
    const hl = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength).getUint32(4, true);
    const hdr = JSON.parse(new TextDecoder().decode(bytes.subarray(8, 8 + hl)));
    let p = 8 + hl;
    const take = (n) => { const b = bytes.subarray(p, p + n); p += n; return b; };
    return {
      version: hdr.version, semi: hdr.semi, regs: hdr.regs,
      ranges: hdr.ranges.map((r) => ({ addr: r.addr, bytes: take(r.len) })),
      bitmaps: { g0: hdr.bitmaps.g0, startBmp: take(hdr.bitmaps.startLen), consBmp: take(hdr.bitmaps.consLen) },
    };
  }

  main(resumed = false) {
    if (!resumed) {
      const km = this.byName.get('KERNEL-MAIN');
      if (!km) throw new Error('no KERNEL-MAIN in module');
      this.ebp = this.esp;
      this.esp -= FRAME_SIZE;
      for (let v = 0; v < 16; v++) this.setR(v, NIL);
      this.doCall(km.off, RET_SENTINEL);
    }
    try { this.run(); return 0; }
    catch (e) { if (e instanceof MvmExit) return e.code; throw e; }
  }
}

const MVM_EXPORTS = { MVM, loadModule, MvmFault, MvmExit, NIL, TV, VBASE };
// MVM_WASM=1: translate to WebAssembly (web/wasm32.js) instead of JS.  Under
// node it is require()d; a browser worker loads it first and leaves its
// exports in self.MVM_WASM32 (worker.js, which also decides MVM_WASM there).
let WASM = null;
if (ENV.MVM_WASM) {
  if (typeof process !== 'undefined' && typeof require !== 'undefined') WASM = require('./wasm32.js');
  else if (typeof self !== 'undefined' && self.MVM_WASM32) WASM = self.MVM_WASM32;
}
if (WASM) {
  WASM.init({ VBASE, NIL, TV, IX, slotOff, FRAME_SIZE, INSN_LEN, JIT_PHYS, A_WEB_CONSTS, POOL_ADDR,
              A_MVCOUNT, A_CENV, A_NARGS, RET_SENTINEL, FN_UNRESOLVED, frameEnterParams, ldbMask,
              JD_MAX, LongJmp, BACK_BRANCH, frameBytes, STACK_ADDR, LJ_TAG,
              A_JMPBUF, A_HDEPTH, A_HSTACK, HMAX, A_HOVF, JMPBUF_WORDS });
}
if (typeof module !== 'undefined' && module.exports) module.exports = MVM_EXPORTS;
else if (typeof self !== 'undefined') self.MVM_EXPORTS = MVM_EXPORTS;
