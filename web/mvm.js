'use strict';
// mvm.js — a JavaScript interpreter for Modus MVM bytecode.
//
// It models the HOSTED LINUX x86-64 machine that mvm/translate-x64.lisp +
// boot/boot-linux-x64.lisp produce: 64-bit words carried as (lo, hi) int32
// pairs, tagged values (fixnum = v<<1, cons = ptr|1, function = addr|3,
// immediate = ..5, object = ptr|9, forward = ..F), a flat linear memory
// holding the BSS block at 0x10000000, a machine stack, and a Cheney
// semispace heap.  All the absolute addresses the compiled runtime hard-codes
// are honoured; the JS side owns the garbage collector, the trap table
// (console, syscalls, setjmp/longjmp handler stack, &rest argument copy), the
// SIGSEGV-to-condition path, and the host I/O.
//
// The module is produced by mvm/build-web.lisp (format documented there).

const NIL = 0xDEAD0001 | 0;
const TV  = 0xDEAD1009 | 0;

// ---- virtual memory layout ------------------------------------------------
const VBASE      = 0x10000000;            // lowest virtual address we model
const BSS_END    = 0x10020000;
const POOL_ADDR  = 0x10020000;            // constant pool (strings) lives here
const CODE_ADDR  = 0x10200000;            // the module's bytecode, executed in place
const JIT_ADDR   = 0x11000000;            // exec pages handed out by %mmap-exec-page / mmap
const JIT_END    = 0x12000000;
const STACK_ADDR = 0x12000000;
const STACK_SIZE = 0x00800000;            // 8 MB
const ARGV_AREA  = 0x00010000;            // top of the stack region: initial argv/envp
const HEAP_ADDR  = 0x12800000;
// pc values are PHYSICAL indices into memory (virtual - VBASE); a function
// value is (phys << 4) | 3.
const CODE_PHYS  = CODE_ADDR - VBASE, JIT_PHYS = JIT_ADDR - VBASE;
const A_WEB_CONSTS = 0x10000D40, A_WEB_RELOCS = 0x10000D48, A_WEB_RELOC_STATUS = 0x10000D50;
// instruction lengths by opcode (mvm.lisp operand specs); 0 = unknown
const INSN_LEN = new Uint8Array(256);
for (const [ops, n] of [[[0x00,0x01,0x72,0x82,0x89,0x8B,0x92,0xA2,0xA3,0xA4],1],[[0x02,0x10,0x25,0x30,0x31,0x50,0x51,0x53,0x54,0x55,0x56,0x63,0x64,0x67,0x68,0xB0,0xB7,0xB9,0xC2,0xC3,0xC4,0xC8],3],
  [[0x11,0x14],10],[[0x12,0x13,0x26,0x27,0x81,0x88,0x8A,0x90,0x91,0xB8,0xBA,0xBB,0xBC,0xBD],2],
  [[0x20,0x21,0x22,0x23,0x24,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2F,0x32,0x52,0x61,0x62,0x65,0x66,0x70,0x71,0x93,0xA5,0xA6,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xBE,0xBF,0xC0,0xC1,0xC5,0xC6,0xC9,0xCA],4],
  [[0x2E,0x60,0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x80,0x83,0xA0,0xA1,0xC7],5],[[0x47,0x48,0xA7],6]]) for (const o of ops) INSN_LEN[o] = n;
const ALLOC_START_OFF = 0x400;            // boot-linux-x64: +linux-x64-heap-alloc-start+
const GUARD      = 0x01000000;            // 16 MB overshoot guard

// BSS words the compiled runtime and the boot stub agree on (8-byte words).
const A_GC_FROM   = 0x10000040, A_GC_TO = 0x10000048, A_GC_SIZE = 0x10000050,
      A_GC_STACKB = 0x10000058, A_GC_COUNT = 0x10000060;
const A_MVCOUNT   = 0x10000090;
const A_CENV      = 0x10000140;           // +closure-env-addr+ (R13 on native)
const A_NARGS     = 0x10000150;           // u32
const A_JMPBUF    = 0x10000180;           // 4 words: rsp rbp ip rbx(V4)
const A_ARGC      = 0x10000200, A_ARGV1 = 0x10000208, A_ARGV2 = 0x10000248;
const A_HDEPTH    = 0x10000400, A_HSTACK = 0x10000408, HMAX = 64, JMPBUF_WORDS = 4;
const A_HOVF      = 0x10000D20;           // live capped-push count (bare-metal slot, unused hosted)
const A_MCGC_PAGEBASE = 0x10000E00;

// Frame layout (byte offsets from RBP).  translate-x64: [rbp-8] saved rbx,
// -16..-32 reserved, V9..V15 spill at -40..-88, frame slot N at -96-8N
// (128 slots, down to -1112), 1120-byte frame.  Every vreg is memory here, so
// V0-V3 and V5-V8 (physical registers on native) get their own per-frame
// slots too: the reserved callee-save words and a 48-byte extension.
const FRAME_SIZE = 1168;
const SLOT_BASE  = -96;
const ROFF = new Int32Array(16);
ROFF[4] = -8; ROFF[0] = -16; ROFF[1] = -24; ROFF[2] = -32;
ROFF[3] = -1128; ROFF[5] = -1136; ROFF[6] = -1144; ROFF[7] = -1152; ROFF[8] = -1160;
for (let v = 9; v <= 15; v++) ROFF[v] = -40 - 8 * (v - 9);
// m32 index offsets (relative to the RBP index) of the low halves of V0..V4
const IX0 = -4, IX1 = -6, IX2 = -8, IX3 = -282, IX4 = -2;

const RET_SENTINEL = -1;
const DBG = (typeof process !== 'undefined' && process.env) ? process.env : {};
const FN_UNRESOLVED = 0xFFFFFFF0;

class LongJmp { constructor(esp, ip) { this.esp = esp; this.ip = ip; } }
class MvmExit { constructor(code) { this.code = code; } }
class MvmFault extends Error {}

function align16(n) { return (n + 15) & ~15; }

// ---- 64-bit arithmetic on int32 pairs; results in RL/RH ------------------
let RL = 0, RH = 0;
function add64(al, ah, bl, bh) {
  const lo = (al >>> 0) + (bl >>> 0);
  RL = lo | 0;
  RH = (ah + bh + (lo > 0xFFFFFFFF ? 1 : 0)) | 0;
}
function sub64(al, ah, bl, bh) {
  RL = (al - bl) | 0;
  RH = (ah - bh - ((al >>> 0) < (bl >>> 0) ? 1 : 0)) | 0;
}
function mul64(al, ah, bl, bh) {            // low 64 bits of the product
  const a48 = ah >>> 16, a32 = ah & 0xFFFF, a16 = al >>> 16, a00 = al & 0xFFFF;
  const b48 = bh >>> 16, b32 = bh & 0xFFFF, b16 = bl >>> 16, b00 = bl & 0xFFFF;
  let c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00; c16 += c00 >>> 16; c00 &= 0xFFFF;
  c16 += a16 * b00; c32 += c16 >>> 16; c16 &= 0xFFFF;
  c16 += a00 * b16; c32 += c16 >>> 16; c16 &= 0xFFFF;
  c32 += a32 * b00; c48 += c32 >>> 16; c32 &= 0xFFFF;
  c32 += a16 * b16; c48 += c32 >>> 16; c32 &= 0xFFFF;
  c32 += a00 * b32; c48 += c32 >>> 16; c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48; c48 &= 0xFFFF;
  RL = (c16 << 16) | c00;
  RH = (c48 << 16) | c32;
}
function shl64(al, ah, n) {
  n &= 63;
  if (n === 0) { RL = al; RH = ah; }
  else if (n < 32) { RL = al << n; RH = (ah << n) | (al >>> (32 - n)); }
  else { RL = 0; RH = al << (n - 32); }
}
function shr64(al, ah, n) {
  n &= 63;
  if (n === 0) { RL = al; RH = ah; }
  else if (n < 32) { RL = (al >>> n) | (ah << (32 - n)); RH = ah >>> n; }
  else { RL = ah >>> (n - 32); RH = 0; }
}
function sar64(al, ah, n) {
  n &= 63;
  if (n === 0) { RL = al; RH = ah; }
  else if (n < 32) { RL = (al >>> n) | (ah << (32 - n)); RH = ah >> n; }
  else { RL = ah >> (n - 32); RH = ah >> 31; }
}
function cmp64(al, ah, bl, bh) {
  if (ah !== bh) return ah < bh ? -1 : 1;
  const a = al >>> 0, b = bl >>> 0;
  return a < b ? -1 : a > b ? 1 : 0;
}
function fits53(lo, hi) { return hi >= -0x200000 && hi < 0x200000; }
function toNum(lo, hi) { return hi * 4294967296 + (lo >>> 0); }
function fromNum(n) {                       // |n| < 2^53
  const hi = Math.floor(n / 4294967296);
  RL = (n - hi * 4294967296) | 0;
  RH = hi | 0;
}
function toBig(lo, hi) { return (BigInt(hi) << 32n) | BigInt(lo >>> 0); }
function fromBig(b) {
  b = BigInt.asIntN(64, b);
  RL = Number(b & 0xFFFFFFFFn) | 0;
  RH = Number(b >> 32n) | 0;
}

// ---- module loading -------------------------------------------------------
function loadModule(bytes) {
  const dv = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let p = 0;
  const u32 = () => { const v = dv.getUint32(p, true); p += 4; return v; };
  const u16 = () => { const v = dv.getUint16(p, true); p += 2; return v; };
  if (String.fromCharCode(bytes[0], bytes[1], bytes[2], bytes[3]) !== 'MVMW')
    throw new Error('not an MVMW module');
  p = 4;
  const version = u32(), wordSize = u32();
  if (version !== 1 || wordSize !== 8) throw new Error('unsupported MVMW version/word size (need the x86-64 module)');
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
  for (const f of fns) byName.set(f.name, f);      // last-defun-wins
  const sorted = fns.slice().sort((a, b) => a.off - b.off);
  return { code, fns, byName, sorted, addrTable, pool };
}

// ---- the machine ----------------------------------------------------------
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
    this.stackTop = STACK_ADDR + STACK_SIZE - ARGV_AREA;   // argv/envp sit above
    this.heapBase = HEAP_ADDR;
    this.heapEnd = HEAP_ADDR + 2 * this.semi + GUARD;
    const size = this.heapEnd - VBASE;
    this.buf = new ArrayBuffer(size);
    this.m8 = new Uint8Array(this.buf);
    this.m32 = new Int32Array(this.buf);
    this.dv = new DataView(this.buf);
    this.f64 = new Float64Array(1);
    this.f64u16 = new Uint16Array(this.f64.buffer);
    const granules = (this.heapEnd - this.heapBase) >> 4;
    this.startBmp = new Uint8Array(granules >> 3);
    this.consBmp = new Uint8Array(granules >> 3);
    this.pageBase = this.heapBase + ALLOC_START_OFF;
    this.mmapNext = JIT_ADDR;
    this.code = this.m8;                      // pc indexes memory directly
    // the module's functions at their physical addresses
    this.fnsPhys = mod.fns.map((f) => ({ ...f, off: f.off + CODE_PHYS }));
    this.byName = new Map(); for (const f of this.fnsPhys) this.byName.set(f.name, f);
    this.sorted = this.fnsPhys.slice().sort((a, b) => a.off - b.off);
    this.gcCount = 0;
    this.steps = 0;
    this.vrl = 0; this.vrh = 0;               // VR (RAX)
    this.va = 0; this.vl = 0;                 // R12 / R14
    this.esp = 0; this.ebp = 0; this.pc = 0;
    this.cmp = 0; this.ovf = false;
    this.snapAt = -1;
    this.onSnapshot = null;
    // sockets: the image's hosted-sockets layer (socket/connect/write/read/
    // close) is answered here as one HTTP request per connection, which the
    // host performs with whatever it has (fetch, curl).  Name resolution comes
    // through the private syscall 4242 and returns a fake IPv4 we can map back.
    this.compiled = new Map();                // phys entry -> JS function, or null (not translatable)
    this.resumeL = 0;
    this.nextF = undefined;
    this.dcount = new Map();
    this.contF = []; this.contL = []; this.contE = []; this.contDepth = 0;
    this.compileOn = opts.compile !== false;
    this.compileThreshold = opts.compileThreshold || 20;
    this.callCounts = new Map();
    this.compileStats = { fns: 0, insns: 0, failed: 0, ms: 0, delegated: new Map() };
    this.socks = new Map();
    this.nextSockFd = 1000;
    this.fakeIps = new Map(); this.ipNames = new Map();
    this.watchAt = -1; this.watchLeft = 0;
    this.genAdd = this.byName.get('GENERIC-ADD');
    this.genSub = this.byName.get('GENERIC-SUBTRACT');
    this.genMul = this.byName.get('GENERIC-MULTIPLY');
    // Memoize keyword interning.  Profiling LOAD/eval shows %INTERN-KEYWORD
    // called ~777k times for only ~175 distinct fixnum keys while reading one
    // file -- each call drags the whole symbol hash-table cascade (%HT-*,
    // %HT-H-STRCMP, GETHASH), which dominates the cold-restore interpreter and
    // makes every form typed at the REPL laggy.  Interning is idempotent, so we
    // cache it: install a JS stand-in as the function's "compiled" entry (the
    // universal call dispatch goes through compiledFor, which returns it).  On a
    // hit we set VR and return exactly like a compiled fn; on a miss we run the
    // real body once (interpreted) and remember its result.  The key is a
    // fixnum (GC-stable); the cached keyword is a heap object that a GC moves,
    // so gc() drops the cache.
    this._ikCache = new Map();
    const ik = this.byName.get('%INTERN-KEYWORD');
    if (ik && this.compileOn) {
      const self = this;
      this.compiled.set(ik.off, function (vm) {
        const m32 = vm.m32, b = (vm.ebp - VBASE) >> 2;
        const a0l = m32[b + IX0], a0h = m32[b + IX0 + 1];
        if ((a0l & 1) === 0) {                        // fixnum arg => GC-stable key
          // numeric key (name-hashes are < 2^53), no per-call string alloc
          const key = a0h * 4294967296 + (a0l >>> 0);
          const hit = self._ikCache.get(key);
          if (hit !== undefined) { vm.vrl = hit[0]; vm.vrh = hit[1]; vm.doRet(); return -1; }
          vm.run();                                   // run the real body once
          self._ikCache.set(key, [vm.vrl, vm.vrh]);
          return -1;
        }
        vm.run(); return -1;                          // non-fixnum arg: don't cache
      });
    }
    this.initMemory(opts.argv || ['modus'], opts.env || []);
  }

  // -- raw memory helpers ---------------------------------------------------
  ld32(a) { return this.m32[(a - VBASE) >> 2]; }
  st32(a, v) { this.m32[(a - VBASE) >> 2] = v; }
  ldlo(a) { return this.m32[(a - VBASE) >> 2]; }
  ldhi(a) { return this.m32[((a - VBASE) >> 2) + 1]; }
  st64(a, lo, hi) { const i = (a - VBASE) >> 2; this.m32[i] = lo; this.m32[i + 1] = hi; }
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

  // -- boot: what boot-linux-x64's entry stub leaves behind -----------------
  stageArgv(argv, env) {
    const enc = (s) => { const b = []; for (let i = 0; i < s.length; i++) b.push(s.charCodeAt(i) & 0xFF); b.push(0); return b; };
    this.st32(A_ARGC, argv.length);
    this.zero(A_ARGV1, A_ARGV1 + 128);
    if (argv.length > 1) this.putBytes(A_ARGV1, enc(argv[1]).slice(0, 63));
    if (argv.length > 2) this.putBytes(A_ARGV2, enc(argv[2]).slice(0, 63));
    // The initial process stack: [argc][argv...][0][envp...][0] with 8-byte
    // slots at stack_base, strings above it.  lib/cli-toplevel walks this
    // through %gc-stack-base.
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
  // Copy the module's bytecode to CODE_ADDR and turn its function-relative
  // call/fn-addr operands into physical addresses (the same rewrite a JIT page
  // gets in relocate()).
  installModule() {
    if (POOL_ADDR + this.mod.pool.length > CODE_ADDR) throw new Error('constant pool too large');
    if (CODE_PHYS + this.mod.code.length > JIT_PHYS) throw new Error('module bytecode too large');
    this.putBytes(POOL_ADDR, this.mod.pool);
    this.m8.set(this.mod.code, CODE_PHYS);
    if (!this.relocate(CODE_PHYS, this.mod.code.length, CODE_PHYS, false)) throw new Error('module relocation failed');
  }
  // Rewrite call / tailcall / fn-addr operands of the code in [phys, phys+len):
  // in-module offsets become phys + offset; synthetic runtime-call offsets
  // (>= 0x40000000, from mvm-eval's rt-table) resolve through the table the
  // Lisp side left at A_WEB_RELOCS (element k = tagged fn word for k).
  relocate(phys, len, base, synthetic) {
    const m8 = this.m8, end = phys + len;
    let p = phys;
    const tab = synthetic ? this.ldlo(A_WEB_RELOCS) : 0;
    while (p < end) {
      const op = m8[p], n = INSN_LEN[op];
      if (n === 0) { this.host.log(`[relocate: unknown opcode 0x${op.toString(16)} at phys 0x${p.toString(16)}]`); return false; }
      if (op === 0x80 || op === 0x83 || op === 0xA7) {
        const at = p + (op === 0xA7 ? 2 : 1);
        const imm = (m8[at] | (m8[at + 1] << 8) | (m8[at + 2] << 16) | (m8[at + 3] << 24)) >>> 0;
        let t;
        if (imm === FN_UNRESOLVED) t = imm;
        else if (imm >= 0x40000000) {
          if (!synthetic) return false;
          const k = imm - 0x40000000;
          const wl = this.ldlo(tab + 7 + 8 * k), wh = this.ldhi(tab + 7 + 8 * k);
          if (wh !== 0 || (wl & 0xF) !== 3) return false;
          t = (wl - 3) >>> 4;
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
    this.st64(A_MCGC_PAGEBASE, from, 0);
    // bitmap base words stay 0 (gc.lisp's bitmap ops degrade to no-ops; the
    // real bitmaps live on the JS side); code bounds stay 0 (every function
    // value carries the +3 tag, which FUNCTIONP tests first).
    this.esp = this.stackTop - 16;
    this.ebp = this.esp;
    this.st64(A_CENV, NIL, 0);
  }

  // -- registers (generic path; the loop inlines the common case) ---------
  rlo(v) {
    if (v < 16) return this.m32[(this.ebp + ROFF[v] - VBASE) >> 2];
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
  rhi(v) {
    if (v < 16) return this.m32[((this.ebp + ROFF[v] - VBASE) >> 2) + 1];
    return v === 16 ? this.vrh : 0;
  }
  setReg(v, lo, hi) {
    if (v < 16) { const i = (this.ebp + ROFF[v] - VBASE) >> 2; this.m32[i] = lo; this.m32[i + 1] = hi; return; }
    switch (v) {
      case 16: this.vrl = lo; this.vrh = hi; return;
      case 17: this.va = lo; return;
      case 18: this.vl = lo; return;
      case 19: return;
      case 20: this.esp = lo; return;
      case 21: this.ebp = lo; return;
      case 22: this.pc = lo; return;
    }
    this.fault('bad vreg ' + v);
  }
  rd32(p) { const c = this.m8; return c[p] | (c[p + 1] << 8) | (c[p + 2] << 16) | (c[p + 3] << 24); }
  setRegR(v) { this.setReg(v, RL, RH); }
  push(lo, hi) { this.esp -= 8; const i = (this.esp - VBASE) >> 2; this.m32[i] = lo; this.m32[i + 1] = hi; }
  pop() { const i = (this.esp - VBASE) >> 2; RL = this.m32[i]; RH = this.m32[i + 1]; this.esp += 8; return RL; }

  // -- diagnostics ----------------------------------------------------------
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
      pc = this.ldlo(ebp + 8);
      if (pc === RET_SENTINEL) break;
      ebp = this.ldlo(ebp);
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
    for (let v = 0; v < 9; v++) out.push(`V${v}=${this.describe(this.rlo(v), this.rhi(v))}`);
    out.push(`VR=${this.describe(this.vrl, this.vrh)}`);
    return out.join(' ');
  }
  // Print a Lisp value the way the reader would see it (bounded).
  lispStr(lo, hi, depth = 0) {
    if (depth > 6) return '…';
    if (hi !== 0) return `#<raw ${hi}:${lo >>> 0}>`;
    if (lo === NIL) return 'NIL'; if (lo === TV) return 'T';
    const tag = lo & 0xF;
    if ((lo & 1) === 0) return String(lo >> 1);
    if (tag === 5) return `#\\${String.fromCharCode(lo >>> 8)}`;
    if (tag === 3) return `#<fn ${this.where((lo - 3) >>> 4)}>`;
    if (lo < this.heapBase || lo >= this.heapEnd) return `#<bad ${(lo >>> 0).toString(16)}>`;
    if (tag === 1) {
      const parts = []; let cur = lo, n = 0;
      while (cur !== NIL && (cur & 0xF) === 1 && n++ < 12) {
        parts.push(this.lispStr(this.ldlo(cur - 1), this.ldhi(cur - 1), depth + 1));
        const nl = this.ldlo(cur + 7), nh = this.ldhi(cur + 7);
        if (nh !== 0 || (nl !== NIL && (nl & 0xF) !== 1)) { parts.push('.', this.lispStr(nl, nh, depth + 1)); break; }
        cur = nl;
      }
      if (n >= 12) parts.push('…');
      return '(' + parts.join(' ') + ')';
    }
    if (tag === 9) {
      const h = this.ldlo(lo - 9), st = h & 0xFF, n = h >>> 8;
      const slot = (i) => [this.ldlo(lo + 7 + 8 * i), this.ldhi(lo + 7 + 8 * i)];
      if (st === 0x31) { let s = ''; for (let i = 0; i < Math.min(n, 40); i++) s += String.fromCharCode(slot(i)[0] >> 1); return JSON.stringify(s); }
      if (st === 0x50 || st === 0x53) { const [nl, nh] = slot(2); const nm = (nh === 0 && (nl & 0xF) === 9 && nl >= this.heapBase) ? this.lispStr(nl, nh, depth + 1).replace(/^"|"$/g, '') : ('#' + toNum(slot(0)[0] >> 1, slot(0)[1])); return (st === 0x53 ? ':' : '') + nm; }
      return `#<obj ${st.toString(16)} n=${n}>`;
    }
    return `#<0x${(lo >>> 0).toString(16)}>`;
  }
  describe(lo, hi) {
    if (hi !== 0) return `raw(${hi},${lo})`;
    if (lo === NIL) return 'NIL'; if (lo === TV) return 'T';
    const tag = lo & 0xF;
    if ((lo & 1) === 0) return `fix ${lo >> 1}`;
    if (tag === 5) return `char ${lo >>> 8}`;
    if (tag === 3) return `fn ${this.where((lo - 3) >>> 4)}`;
    if (lo < VBASE || lo >= this.heapEnd) return `bad 0x${(lo >>> 0).toString(16)}`;
    if (tag === 1) return `cons@${(lo - 1).toString(16)}`;
    if (tag === 9) { const h = this.ldlo(lo - 9); return `obj@${(lo - 9).toString(16)} subtag=0x${(h & 0xFF).toString(16)} n=${h >>> 8}`; }
    return `0x${(lo >>> 0).toString(16)}`;
  }
  fault(msg) {
    const bt = this.backtrace(100000);
    const shown = bt.length > 40 ? bt.slice(0, 20).concat([`... ${bt.length - 40} more frames ...`], bt.slice(-20)) : bt;
    throw new MvmFault(`${msg} at ${this.where()} (step ${this.steps})\n  ` + shown.join('\n  '));
  }
  // A bad dereference.  Native takes SIGSEGV and the handler stub longjmps
  // through the armed handler-case (with T in RAX), which is how (car 5)
  // becomes a TYPE-ERROR.  Nothing armed: the process dies with 139.
  memFault(what) {
    if (this.ldlo(A_JMPBUF) !== 0) { if (this.trace) this.host.log(`[segv: ${what} at ${this.where()}]`); this.longjmp(); }
    this.fault(`SIGSEGV: ${what}`);
  }

  // -- allocation -----------------------------------------------------------
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
  allocObj(count, subtag, fill) {           // count: untagged Number
    const total = align16((count + 2) * 8);
    const base = this.bump(total);
    if (fill) this.zero(base + 8, base + total);
    this.st64(base, ((count << 8) | subtag) | 0, Math.floor(count / 16777216) | 0);
    this.markStart(base);
    return base | 9;
  }
  allocCons(cl, ch, dl, dh) {
    const base = this.bump(16);
    const i = (base - VBASE) >> 2;
    this.m32[i] = cl; this.m32[i + 1] = ch; this.m32[i + 2] = dl; this.m32[i + 3] = dh;
    this.markStart(base); this.markCons(base);
    return base | 1;
  }
  allocFloat(d) {
    const base = this.bump(48);
    this.zero(base, base + 48);
    this.st64(base, (4 << 8) | 0x60, 0);
    this.f64[0] = d;
    for (let i = 0; i < 4; i++) this.st64(base + 16 + 8 * i, this.f64u16[3 - i] << 1, 0);
    this.markStart(base);
    return base | 9;
  }
  floatVal(v) {
    if ((v & 0xF) !== 9 || v < this.heapBase) this.memFault('float op on non-object');
    for (let i = 0; i < 4; i++) this.f64u16[3 - i] = (this.ldlo(v - 9 + 16 + 8 * i) >> 1) & 0xFFFF;
    return this.f64[0];
  }

  // -- garbage collector: Cheney copy with conservative validated roots ------
  gc() {
    if (this._ikCache) this._ikCache.clear();   // cached keyword addresses move
    const m32 = this.m32;
    const fromStart = this.ldlo(A_GC_FROM);
    const spaceSize = this.ldlo(A_GC_SIZE);
    const fromEnd = fromStart + spaceSize;
    const toStart = this.ldlo(A_GC_TO);
    let free = toStart;
    const self = this;
    const t0 = Date.now();
    const used = this.va - fromStart;

    function copy(v) {
      const tag = v & 0xF;
      const raw = v - tag;
      const hi = (raw - VBASE) >> 2;
      const hdr = m32[hi];
      if ((hdr & 0xF) === 0xF) return (hdr & ~0xF) | tag;
      let size;
      if (self.isCons(raw)) size = 16;
      else {
        const subtag = hdr & 0xFF, count = (hdr >>> 8) + m32[hi + 1] * 16777216;
        size = subtag === 0x11 ? align16(16 + count) : align16((count + 2) * 8);
        if (size < 16 || raw + size > fromEnd) return v;
      }
      const dst = free;
      self.m8.copyWithin(dst - VBASE, raw - VBASE, raw - VBASE + size);
      free += size;
      self.markStart(dst);
      if (self.isCons(raw)) self.markCons(dst);
      m32[hi] = dst | 0xF;
      return dst | tag;
    }
    function scanWord(i) {                  // i: m32 index of an 8-byte word's low half
      if (m32[i + 1] !== 0) return;
      const v = m32[i];
      const tag = v & 0xF;
      if (tag !== 1 && tag !== 9) return;
      const raw = v - tag;
      if (raw < fromStart || raw >= fromEnd) return;
      if (!self.isStart(raw)) return;
      if (self.isCons(raw) !== (tag === 1 ? 1 : 0)) return;
      m32[i] = copy(v);
    }
    for (let a = this.esp; a < this.stackTop; a += 8) scanWord((a - VBASE) >> 2);
    for (let a = VBASE; a < BSS_END; a += 8) scanWord((a - VBASE) >> 2);
    if (this.vrh === 0) { const t = this.vrl & 0xF;
      if (t === 1 || t === 9) { m32[0] = this.vrl; m32[1] = 0; scanWord(0); this.vrl = m32[0]; m32[0] = 0; } }
    for (let a = toStart; a < free; a += 8) scanWord((a - VBASE) >> 2);
    this.st64(A_GC_FROM, toStart, 0);
    this.st64(A_GC_TO, fromStart, 0);
    this.va = free;
    this.vl = toStart + spaceSize;
    const g0 = (fromStart - this.pageBase) >> 4, g1 = (fromEnd - this.pageBase) >> 4;
    this.startBmp.fill(0, g0 >> 3, g1 >> 3);
    this.consBmp.fill(0, g0 >> 3, g1 >> 3);
    this.gcCount++;
    this.st64(A_GC_COUNT, this.gcCount, 0);
    if (this.trace) this.host.log(`[gc #${this.gcCount}: ${used >> 10}K -> ${(free - toStart) >> 10}K, ${Date.now() - t0}ms]`);
  }

  // -- calls ----------------------------------------------------------------
  // Native x64: CALL pushes the return address; the prologue pushes RBP,
  // reserves the frame, saves RBX.  Args V0-V3 arrive in registers, so the
  // callee frame starts with the caller's V0-V3 (and its V4, callee-saved).
  enter(target) {
    const m32 = this.m32;
    const cb = (this.ebp - VBASE) >> 2;
    const a0l = m32[cb + IX0], a0h = m32[cb + IX0 + 1], a1l = m32[cb + IX1], a1h = m32[cb + IX1 + 1];
    const a2l = m32[cb + IX2], a2h = m32[cb + IX2 + 1], a3l = m32[cb + IX3], a3h = m32[cb + IX3 + 1];
    const a4l = m32[cb + IX4], a4h = m32[cb + IX4 + 1];
    this.push(this.ebp, 0);
    this.ebp = this.esp;
    this.esp -= FRAME_SIZE;
    if (this.esp < STACK_ADDR + 4096) { this.esp = this.ebp; this.ebp = this.pop(); this.pop(); this.memFault('stack overflow'); }
    const b = (this.ebp - VBASE) >> 2;
    m32[b + IX0] = a0l; m32[b + IX0 + 1] = a0h; m32[b + IX1] = a1l; m32[b + IX1 + 1] = a1h;
    m32[b + IX2] = a2l; m32[b + IX2 + 1] = a2h; m32[b + IX3] = a3l; m32[b + IX3 + 1] = a3h;
    m32[b + IX4] = a4l; m32[b + IX4 + 1] = a4h;
    this.pc = target;
    if (this.prof) this.prof.set(target, (this.prof.get(target) || 0) + 1);
    if (target === this.watchAt && this.watchLeft-- > 0) this.host.log(`[watch ${this.where(target)} V0=${this.lispStr(a0l, a0h)} V1=${this.lispStr(a1l, a1h)} from ${this.backtrace(4).slice(1).join(' < ')}]`);
    if (target === this.snapAt) { this.snapAt = -1; this.onSnapshot(); }
  }
  doCall(target, retpc) { this.push(retpc, 0); this.enter(target); }
  doTailcall(target) {
    // drop this frame but keep its V0-V4 as the callee's incoming registers
    const m32 = this.m32, b = (this.ebp - VBASE) >> 2;
    const r = [m32[b + IX0], m32[b + IX0 + 1], m32[b + IX1], m32[b + IX1 + 1], m32[b + IX2], m32[b + IX2 + 1],
               m32[b + IX3], m32[b + IX3 + 1], m32[b + IX4], m32[b + IX4 + 1]];
    this.esp = this.ebp;
    this.pop(); const oldEbp = RL;
    this.pop(); const ret = RL;
    this.push(ret, 0);
    this.push(oldEbp, 0);
    this.ebp = this.esp;
    this.esp -= FRAME_SIZE;
    const nb = (this.ebp - VBASE) >> 2;
    m32[nb + IX0] = r[0]; m32[nb + IX0 + 1] = r[1]; m32[nb + IX1] = r[2]; m32[nb + IX1 + 1] = r[3];
    m32[nb + IX2] = r[4]; m32[nb + IX2 + 1] = r[5]; m32[nb + IX3] = r[6]; m32[nb + IX3 + 1] = r[7];
    m32[nb + IX4] = r[8]; m32[nb + IX4 + 1] = r[9];
    this.pc = target;
    if (this.prof) this.prof.set(target, (this.prof.get(target) || 0) + 1);
  }
  doRet() {
    this.esp = this.ebp;
    this.pop(); this.ebp = RL;
    this.pop(); this.pc = RL;
  }
  fnAddrToOffset(lo, hi) {
    if ((lo & 0xF) !== 3 || hi !== 0) this.memFault(`call-ind on non-function 0x${(lo >>> 0).toString(16)}`);
    return (lo - 3) >>> 4;
  }
  // Run a nested activation to completion; leaves VR in RL/RH.
  callLisp(fn, args) {
    const savedPc = this.pc, svl = this.vrl, svh = this.vrh;
    const m32 = this.m32, b = (this.ebp - VBASE) >> 2;
    const saved = [m32[b + IX0], m32[b + IX0 + 1], m32[b + IX1], m32[b + IX1 + 1], m32[b + IX2], m32[b + IX2 + 1], m32[b + IX3], m32[b + IX3 + 1]];
    for (let i = 0; i < 4; i++) {
      if (i < args.length) this.setReg(i, args[i][0], args[i][1]); else this.setReg(i, NIL, 0);
    }
    this.st32(A_NARGS, args.length);
    this.callFn(fn.off);
    const rl = this.vrl, rh = this.vrh;
    m32[b + IX0] = saved[0]; m32[b + IX0 + 1] = saved[1]; m32[b + IX1] = saved[2]; m32[b + IX1 + 1] = saved[3];
    m32[b + IX2] = saved[4]; m32[b + IX2 + 1] = saved[5]; m32[b + IX3] = saved[6]; m32[b + IX3 + 1] = saved[7];
    this.pc = savedPc; this.vrl = svl; this.vrh = svh;
    RL = rl; RH = rh;
  }

  // -- handler stack (setjmp / longjmp), translate-x64 layout ---------------
  handlerPush() {
    const depth = this.ldlo(A_HDEPTH);
    if (depth >= HMAX) { this.st32(A_HOVF, this.ld32(A_HOVF) + 1); return 1; }
    const fr = A_HSTACK + depth * 32;
    for (let i = 0; i < JMPBUF_WORDS; i++) this.st64(fr + 8 * i, this.ldlo(A_JMPBUF + 8 * i), this.ldhi(A_JMPBUF + 8 * i));
    this.st64(A_HDEPTH, depth + 1, 0);
    return 0;
  }
  handlerPop() {
    const ovf = this.ld32(A_HOVF);
    if (ovf !== 0) { this.st32(A_HOVF, ovf - 1); return; }
    const depth = this.ldlo(A_HDEPTH);
    if (depth === 0) {
      for (let i = 0; i < JMPBUF_WORDS; i++) this.st64(A_JMPBUF + 8 * i, 0, 0);
      return;
    }
    const fr = A_HSTACK + (depth - 1) * 32;
    this.st64(A_HDEPTH, depth - 1, 0);
    for (let i = 0; i < JMPBUF_WORDS; i++) this.st64(A_JMPBUF + 8 * i, this.ldlo(fr + 8 * i), this.ldhi(fr + 8 * i));
  }
  setjmp(resumePc) {
    if (!this.handlerPush()) {
      const b = (this.ebp - VBASE) >> 2;
      this.st64(A_JMPBUF, this.esp, 0);
      this.st64(A_JMPBUF + 8, this.ebp, 0);
      this.st64(A_JMPBUF + 16, resumePc, 0);
      this.st64(A_JMPBUF + 24, this.m32[b + IX4], this.m32[b + IX4 + 1]);
    }
    this.vrl = NIL; this.vrh = 0;
  }
  longjmp() {
    this.st32(A_HOVF, 0);
    const esp = this.ldlo(A_JMPBUF), ebp = this.ldlo(A_JMPBUF + 8), ip = this.ldlo(A_JMPBUF + 16);
    const v4l = this.ldlo(A_JMPBUF + 24), v4h = this.ldhi(A_JMPBUF + 24);
    if (esp === 0) this.fault('longjmp with no handler armed');
    this.handlerPop();
    this.ebp = ebp; this.esp = esp; this.pc = ip;
    const b = (ebp - VBASE) >> 2;
    this.m32[b + IX4] = v4l; this.m32[b + IX4 + 1] = v4h;
    this.vrl = TV; this.vrh = 0;
    throw new LongJmp(esp, ip);
  }

  // -- traps (translate-x64 hosted arms) -------------------------------------
  trap(codeNum, nextPc) {
    const h = this.host;
    if (codeNum < 0x100) {
      for (let i = 4; i < codeNum; i++) {
        const s = this.ebp + 16 + 8 * (i - 4), d = this.ebp + SLOT_BASE - 8 * i;
        this.st64(d, this.ldlo(s), this.ldhi(s));
      }
      return;
    }
    if (codeNum < 0x300) return;
    switch (codeNum) {
      case 0x0300: h.writeByte(1, (this.rlo(0) >> 1) & 0xFF); return;
      case 0x0301: { const c = h.readByte(0); this.setReg(0, (c < 0 ? 0xFF : c) << 1, 0); return; }
      case 0x0302: case 0x0303: case 0x0304: case 0x0320: case 0x0321: return;
      case 0x0310: { fromNum(Math.floor(h.now() * 1e6)); this.vrl = RL; this.vrh = RH; return; }
      case 0x0500: throw new MvmExit(this.rlo(0) >> 1);
      case 0x0502: {
        const r = this.syscall(this.arg(0), this.arg(1), this.arg(2), this.arg(3), 0, 0, 0);
        fromNum(r * 2); this.setReg(0, RL, RH); return;
      }
      case 0x0503: {
        const r = this.syscall(this.arg(0), toNum(this.rlo(1), this.rhi(1)), toNum(this.rlo(2), this.rhi(2)),
                               toNum(this.rlo(3), this.rhi(3)), 0, 0, 0);
        fromNum(r); this.setReg(0, RL, RH); return;
      }
      case 0x0507: {
        const r = this.syscall(this.arg(0), this.arg(1), this.arg(2), this.arg(3), this.arg(4), this.arg(5), this.arg(6));
        fromNum(r * 2); this.setReg(0, RL, RH); return;
      }
      case 0x0504: case 0x0531: { const a = this.mmap(this.arg(0)); this.setReg(0, a * 2, 0); return; }
      case 0x0510: this.setjmp(nextPc); return;
      case 0x0511: this.longjmp(); return;
      case 0x0512: this.handlerPop(); return;
      case 0x0520: this.setReg(0, NIL, 0); return;
      case 0x0530: {
        let n = this.ld32(A_NARGS);
        if (n < 5) return;
        if (n > 32) n = 32;
        for (let i = 4; i < n; i++) {
          const s = this.ebp + 16 + 8 * (i - 4), d = this.ebp + SLOT_BASE - 8 * i;
          this.st64(d, this.ldlo(s), this.ldhi(s));
        }
        return;
      }
      case 0x0532: {                          // %jit-call: run a page function to completion
        const phys = this.arg(0) - VBASE;
        if (phys < JIT_PHYS || phys >= JIT_END - VBASE) this.fault(`%jit-call outside the exec region: 0x${this.arg(0).toString(16)}`);
        this.callFn(phys);                    // VR holds the result; the caller's frame is intact
        return;
      }
      case 0x0533: {                          // %jit-icache-flush base len: relocate the page
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
  arg(v) { sar64(this.rlo(v), this.rhi(v), 1); return toNum(RL, RH); }
  mmap(size) {
    const a = this.mmapNext;
    const n = (size + 4095) & ~4095;
    if (a + n > JIT_END) return -12;          // ENOMEM
    this.mmapNext += n;
    return a;
  }

  // -- sockets as HTTP requests --------------------------------------------
  resolveHost(name) {
    if (!this.fakeIps.has(name)) {
      const n = this.fakeIps.size + 1;                  // 10.77.x.y
      const ip = (10 << 24) | (77 << 16) | (((n >> 8) & 0xFF) << 8) | (n & 0xFF);
      this.fakeIps.set(name, ip >>> 0); this.ipNames.set(ip >>> 0, name);
    }
    return this.fakeIps.get(name);
  }
  sockRead(sk, m8, off, len) {
    if (!sk.resp) {
      // first read: the request is complete; perform it
      const req = new Uint8Array(sk.req.reduce((a, b) => a + b.length, 0));
      let p = 0; for (const b of sk.req) { req.set(b, p); p += b.length; }
      const text = new TextDecoder('latin1').decode(req);
      const m = /^([A-Z]+) (\S+) HTTP\/1\.[01]\r?\n([\s\S]*?)\r?\n\r?\n([\s\S]*)$/.exec(text);
      if (!m) return -104;                               // ECONNRESET
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

  // Linux x86-64 numbering, the subset the hosted CLI uses.
  syscall(nr, a1, a2, a3, a4, a5, a6) {
    const h = this.host;
    const inMem = (a, n) => a >= VBASE && a + n <= this.heapEnd;
    const sk = (nr === 0 || nr === 1 || nr === 3 || nr === 42) ? this.socks.get(a1) : undefined;
    switch (nr) {
      case 60: case 231: throw new MvmExit(a1);
      case 0: if (!inMem(a2, a3)) return -14;
              return sk ? this.sockRead(sk, this.m8, a2 - VBASE, a3) : h.read(a1, this.m8, a2 - VBASE, a3);
      case 1: if (!inMem(a2, a3)) return -14;
              if (sk) { sk.req.push(this.m8.slice(a2 - VBASE, a2 - VBASE + a3)); return a3; }
              return h.write(a1, this.m8, a2 - VBASE, a3);
      case 2: return h.open(this.cstr(a1), a2, a3);
      case 3: if (sk) { this.socks.delete(a1); return 0; } return h.close(a1);
      case 41: {                                 // socket(AF_INET, type, 0)
        if (a1 !== 2) return -97;                // EAFNOSUPPORT
        const fd = this.nextSockFd++;
        this.socks.set(fd, { type: a2, req: [], resp: null, pos: 0, host: null, ip: 0, port: 0 });
        return fd;
      }
      case 42: {                                 // connect(fd, sockaddr_in*, len)
        if (!sk) return -9;
        const port = (this.m8[a2 - VBASE + 2] << 8) | this.m8[a2 - VBASE + 3];
        const ip = ((this.m8[a2 - VBASE + 4] << 24) | (this.m8[a2 - VBASE + 5] << 16) | (this.m8[a2 - VBASE + 6] << 8) | this.m8[a2 - VBASE + 7]) >>> 0;
        sk.port = port; sk.ip = ip;
        sk.host = this.ipNames.get(ip) || `${ip >>> 24}.${(ip >>> 16) & 255}.${(ip >>> 8) & 255}.${ip & 255}`;
        return 0;
      }
      case 7: return 1;                          // poll: always ready
      case 54: case 55: return 0;                // setsockopt / getsockopt
      case 4242: {                               // web: resolve host name -> fake IPv4
        const name = this.cstr(a1);
        return name ? this.resolveHost(name) : 0;
      }
      case 4: case 5: {                          // stat / fstat: st_size@48, st_mtime@88
        const st = nr === 4 ? h.stat(this.cstr(a1)) : h.fstat(a1);
        if (typeof st === 'number') return st;
        this.st64(a2 + 48, st.size | 0, 0);
        this.st64(a2 + 88, st.mtime | 0, 0);
        return 0;
      }
      case 8: return h.lseek(a1, a2, a3);
      case 9: return this.mmap(a2);
      case 10: case 11: return 0;                // mprotect / munmap
      case 21: return h.access(this.cstr(a1), a2);
      case 24: case 158: return 0;               // sched_yield / arch_prctl
      case 186: return 1;                        // gettid
      case 35: return 0;                         // nanosleep
      case 39: return h.getpid();
      case 74: case 75: case 77: return 0;       // fsync / fdatasync / ftruncate
      case 82: return h.rename(this.cstr(a1), this.cstr(a2));
      case 83: return h.mkdir(this.cstr(a1), a2);
      case 87: return h.unlink(this.cstr(a1));
      case 201: return (Date.now() / 1000) | 0;
      case 228: { const ms = Date.now(); this.st64(a2, (ms / 1000) | 0, 0); this.st64(a2 + 8, ((ms % 1000) * 1e6) | 0, 0); return 0; }
      case 217: {                                // getdents64(fd, buf, size)
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
      // --- GUI bridge (browser): Lisp drives the DOM / a WebGL canvas -------
      case 5000: if (a2 > 0 && inMem(a1, a2) && h.guiSend) h.guiSend(this.m8, a1 - VBASE, a2); return 0;   // send command bytes
      case 5001: return (h.guiPoll && inMem(a1, a2)) ? h.guiPoll(this.m8, a1 - VBASE, a2) : 0;              // drain events -> bytes
      case 5002: if (h.guiWait) h.guiWait(a1); return 0;                                                    // block up to a1 ms
      default:
        h.log(`[mvm: unsupported syscall ${nr}]`);
        return -38;
    }
  }

  // -- the interpreter loop -------------------------------------------------
  run() {
    const baseEsp = this.esp;
    for (;;) {
      try { this.loop(); return; }
      catch (e) {
        if (e instanceof LongJmp && e.esp <= baseEsp) continue;
        throw e;
      }
    }
  }

  loop() {
    let pc = this.pc;
    for (;;) {
      if (this.trace) {
        this.steps++;
        if (this.trace > 1 && (!this.traceFrom || (this.steps >= this.traceFrom && this.steps < this.traceFrom + this.traceCount))) this.host.log(`${this.steps} ${this.where(pc)} op=${code[pc].toString(16)} ${this.traceRegs ? this.regDump() : ''}`);
        else if (this.maxSteps && this.steps >= this.maxSteps) { this.pc = pc; this.fault('step limit'); }
        else if (this.prof && (this.steps & 255) === 0) { const f = this.fnAt(pc); if (f) this.prof.set(-f.off - 1, (this.prof.get(-f.off - 1) || 0) + 1); }
        else if ((this.steps & 0x3FFFFFF) === 0) { this.pc = pc; const bt = this.backtrace(200); this.host.log(`[${this.steps} steps, gc ${this.gcCount}, heap ${(this.va - this.heapBase) >> 10}K, depth ${bt.length}] ${bt.slice(0, 4).join(' < ')} ... ${bt.slice(-4).join(' < ')}`); }
      }
      pc = this.execInsn(pc);
      if (pc === RET_SENTINEL) return;
    }
  }

  // Execute the instruction at PC; return the next pc (RET_SENTINEL when the
  // activation that started this loop has returned).  Calls into a callee
  // that has a compiled form run it to completion here.
  execInsn(pc) {
    const code = this.code, m32 = this.m32, m8 = this.m8;
    const heapEnd = this.heapEnd;
    // no per-call closures here: this runs once per instruction
    {
      const op = code[pc];
      this.pc = pc;
      switch (op) {
        case 0x00: pc += 1; break;
        case 0x01: this.fault('break');
        case 0x02: {
          const c = code[pc + 1] | (code[pc + 2] << 8);
          this.trap(c, pc + 3);
          pc += 3; break;
        }
        case 0x10: { const s = code[pc + 2]; this.setReg(code[pc + 1], this.rlo(s), this.rhi(s)); pc += 3; break; }        // mov
        case 0x11: this.setReg(code[pc + 1], this.rd32(pc + 2), this.rd32(pc + 6)); pc += 10; break;                       // li imm64
        case 0x12: { const s = code[pc + 1]; this.push(this.rlo(s), this.rhi(s)); pc += 2; break; }              // push
        case 0x13: this.pop(); this.setRegR(code[pc + 1]); pc += 2; break;                                        // pop
        case 0x14: {                                                                                     // li-const
          const idx = this.rd32(pc + 2);
          if (pc >= JIT_PHYS) {                                                                          // mvm-eval quote pool
            const vec = this.ldlo(A_WEB_CONSTS);
            if (vec === 0) this.fault('li-const in a page with no constant vector');
            const a = vec + 7 + 8 * idx;
            this.setReg(code[pc + 1], this.ldlo(a), this.ldhi(a));
          } else {
            const off = this.mod.addrTable[idx] | 0;
            this.setReg(code[pc + 1], off === 0 ? 0 : (POOL_ADDR + off), 0);
          }
          pc += 10; break;
        }
        case 0x20: { const a = code[pc + 2], b = code[pc + 3]; add64(this.rlo(a), this.rhi(a), this.rlo(b), this.rhi(b)); this.setRegR(code[pc + 1]); pc += 4; break; }
        case 0x21: { const a = code[pc + 2], b = code[pc + 3]; sub64(this.rlo(a), this.rhi(a), this.rlo(b), this.rhi(b)); this.setRegR(code[pc + 1]); pc += 4; break; }
        case 0x22: {                                                                                     // mul: (a>>1)*b wrap
          const a = code[pc + 2], b = code[pc + 3];
          sar64(this.rlo(a), this.rhi(a), 1);
          mul64(RL, RH, this.rlo(b), this.rhi(b)); this.setRegR(code[pc + 1]); pc += 4; break;
        }
        case 0x23: case 0x24: {                                                                          // div / mod (truncating)
          const a = code[pc + 2], b = code[pc + 3];
          sar64(this.rlo(a), this.rhi(a), 1); const xl = RL, xh = RH;
          sar64(this.rlo(b), this.rhi(b), 1); const yl = RL, yh = RH;
          if (yl === 0 && yh === 0) this.memFault('division by zero');
          if (fits53(xl, xh) && fits53(yl, yh)) {
            const x = toNum(xl, xh), y = toNum(yl, yh);
            const r = op === 0x23 ? Math.trunc(x / y) : x % y;
            fromNum(r * 2);
          } else {
            const X = toBig(xl, xh), Y = toBig(yl, yh);
            fromBig((op === 0x23 ? X / Y : X % Y) << 1n);
          }
          this.setRegR(code[pc + 1]); pc += 4; break;
        }
        case 0x25: { const s = code[pc + 2]; sub64(0, 0, this.rlo(s), this.rhi(s)); this.setRegR(code[pc + 1]); pc += 3; break; }   // neg
        case 0x26: { const d = code[pc + 1]; add64(this.rlo(d), this.rhi(d), 2, 0); this.setRegR(d); pc += 2; break; }
        case 0x27: { const d = code[pc + 1]; sub64(this.rlo(d), this.rhi(d), 2, 0); this.setRegR(d); pc += 2; break; }
        case 0x28: { const a = code[pc + 2], b = code[pc + 3]; this.setReg(code[pc + 1], this.rlo(a) & this.rlo(b), this.rhi(a) & this.rhi(b)); pc += 4; break; }
        case 0x29: { const a = code[pc + 2], b = code[pc + 3]; this.setReg(code[pc + 1], this.rlo(a) | this.rlo(b), this.rhi(a) | this.rhi(b)); pc += 4; break; }
        case 0x2A: { const a = code[pc + 2], b = code[pc + 3]; this.setReg(code[pc + 1], this.rlo(a) ^ this.rlo(b), this.rhi(a) ^ this.rhi(b)); pc += 4; break; }
        case 0x2B: { const s = code[pc + 2]; shl64(this.rlo(s), this.rhi(s), code[pc + 3]); this.setRegR(code[pc + 1]); pc += 4; break; }
        case 0x2C: { const s = code[pc + 2]; shr64(this.rlo(s), this.rhi(s), code[pc + 3]); this.setRegR(code[pc + 1]); pc += 4; break; }
        case 0x2D: { const s = code[pc + 2]; sar64(this.rlo(s), this.rhi(s), code[pc + 3]); this.setRegR(code[pc + 1]); pc += 4; break; }
        case 0x2F: { const s = code[pc + 2]; shl64(this.rlo(s), this.rhi(s), this.rlo(code[pc + 3]) & 63); this.setRegR(code[pc + 1]); pc += 4; break; }
        case 0x32: { const s = code[pc + 2]; sar64(this.rlo(s), this.rhi(s), this.rlo(code[pc + 3]) & 63); this.setRegR(code[pc + 1]); pc += 4; break; }
        case 0x2E: {                                                                                     // ldb pos size
          const s = code[pc + 2], pos = code[pc + 3], size = code[pc + 4];
          shr64(this.rlo(s), this.rhi(s), pos);
          if (size < 32) { RL &= (1 << size) - 1; RH = 0; }
          else if (size === 32) RH = 0;
          else if (size < 64) RH &= (1 << (size - 32)) - 1;
          this.setRegR(code[pc + 1]); pc += 5; break;
        }
        case 0x30: { const a = code[pc + 1], b = code[pc + 2]; this.cmp = cmp64(this.rlo(a), this.rhi(a), this.rlo(b), this.rhi(b)); pc += 3; break; }
        case 0x31: { const a = code[pc + 1], b = code[pc + 2]; const lo = this.rlo(a) & this.rlo(b), hi = this.rhi(a) & this.rhi(b);
                     this.cmp = (lo === 0 && hi === 0) ? 0 : (hi < 0 ? -1 : 1); pc += 3; break; }
        case 0x40: pc = pc + 5 + this.rd32(pc + 1); break;
        case 0x41: pc = this.cmp === 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5; break;
        case 0x42: pc = this.cmp !== 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5; break;
        case 0x43: pc = this.cmp < 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5; break;
        case 0x44: pc = this.cmp >= 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5; break;
        case 0x45: pc = this.cmp <= 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5; break;
        case 0x46: pc = this.cmp > 0 ? pc + 5 + this.rd32(pc + 1) : pc + 5; break;
        case 0x47: { const s = code[pc + 1]; pc = (this.rlo(s) === NIL && this.rhi(s) === 0) ? pc + 6 + this.rd32(pc + 2) : pc + 6; break; }
        case 0x48: { const s = code[pc + 1]; pc = (this.rlo(s) !== NIL || this.rhi(s) !== 0) ? pc + 6 + this.rd32(pc + 2) : pc + 6; break; }
        case 0x50: case 0x51: {                                                                          // car / cdr: bare deref
          const s = code[pc + 2], lo = this.rlo(s), hi = this.rhi(s);
          const a = lo + (op === 0x50 ? -1 : 7);
          if (hi !== 0 || (lo & 7) !== 1 || a < VBASE || a >= heapEnd) this.memFault((op === 0x50 ? 'car' : 'cdr') + ` of non-cons 0x${(lo >>> 0).toString(16)}`);
          const i = (a - VBASE) >> 2;
          this.setReg(code[pc + 1], m32[i], m32[i + 1]); pc += 3; break;
        }
        case 0x52: { const a = code[pc + 2], b = code[pc + 3]; this.setReg(code[pc + 1], this.allocCons(this.rlo(a), this.rhi(a), this.rlo(b), this.rhi(b)), 0); pc += 4; break; }
        case 0x53: case 0x54: {                                                                          // setcar / setcdr
          const c = code[pc + 1], lo = this.rlo(c), hi = this.rhi(c), s = code[pc + 2];
          const a = lo + (op === 0x53 ? -1 : 7);
          if (hi !== 0 || (lo & 7) !== 1 || a < VBASE || a >= heapEnd) this.memFault(`rplac on non-cons 0x${(lo >>> 0).toString(16)}`);
          const i = (a - VBASE) >> 2;
          m32[i] = this.rlo(s); m32[i + 1] = this.rhi(s); pc += 3; break;
        }
        case 0x55: { const s = code[pc + 2], lo = this.rlo(s); this.setReg(code[pc + 1], (!(lo === NIL && this.rhi(s) === 0) && (lo & 0xF) === 1) ? TV : NIL, 0); pc += 3; break; }
        case 0x56: { const s = code[pc + 2], lo = this.rlo(s); this.setReg(code[pc + 1], (!(lo === NIL && this.rhi(s) === 0) && (lo & 0xF) === 1) ? NIL : TV, 0); pc += 3; break; }
        case 0x60: {                                                                                     // alloc-obj count subtag
          const count = code[pc + 2] | (code[pc + 3] << 8), subtag = code[pc + 4];
          this.setReg(code[pc + 1], this.allocObj(count, subtag, true), 0); pc += 5; break;
        }
        case 0x61: {                                                                                     // obj-ref Vd Vobj idx
          const vobj = code[pc + 2], idx = code[pc + 3];
          let a;
          if (vobj === 21) a = this.ebp + SLOT_BASE - 8 * idx;
          else { const lo = this.rlo(vobj); a = lo + 7 + 8 * idx; if (this.rhi(vobj) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`obj-ref on 0x${(lo >>> 0).toString(16)}`); }
          const i = (a - VBASE) >> 2;
          this.setReg(code[pc + 1], m32[i], m32[i + 1]); pc += 4; break;
        }
        case 0x62: {                                                                                     // obj-set Vobj idx Vs
          const vobj = code[pc + 1], idx = code[pc + 2], s = code[pc + 3];
          let a;
          if (vobj === 21) a = this.ebp + SLOT_BASE - 8 * idx;
          else { const lo = this.rlo(vobj); a = lo + 7 + 8 * idx; if (this.rhi(vobj) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`obj-set on 0x${(lo >>> 0).toString(16)}`); }
          const i = (a - VBASE) >> 2;
          m32[i] = this.rlo(s); m32[i + 1] = this.rhi(s); pc += 4; break;
        }
        case 0x63: this.setReg(code[pc + 1], (this.rlo(code[pc + 2]) & 0xF) << 1, 0); pc += 3; break;                  // obj-tag
        case 0x64: {                                                                                     // obj-subtag (guarded)
          const s = code[pc + 2], lo = this.rlo(s), hi = this.rhi(s);
          let r = 0;
          if ((lo & 0xF) === 9 && hi === 0 && lo !== TV && lo - 9 >= VBASE && lo - 9 < heapEnd) r = (m32[(lo - 9 - VBASE) >> 2] & 0xFF) << 1;
          this.setReg(code[pc + 1], r, 0); pc += 3; break;
        }
        case 0x65: {                                                                                     // aref Vd Vobj Vidx
          const o = code[pc + 2], x = code[pc + 3];
          const a = this.rlo(o) + this.rlo(x) * 4 + 7;
          if (this.rhi(o) !== 0 || a < VBASE || a >= heapEnd) this.memFault('aref');
          const i = (a - VBASE) >> 2;
          this.setReg(code[pc + 1], m32[i], m32[i + 1]); pc += 4; break;
        }
        case 0x66: {                                                                                     // aset Vobj Vidx Vs
          const o = code[pc + 1], x = code[pc + 2], s = code[pc + 3];
          const a = this.rlo(o) + this.rlo(x) * 4 + 7;
          if (this.rhi(o) !== 0 || a < VBASE || a >= heapEnd) this.memFault('aset');
          const i = (a - VBASE) >> 2;
          m32[i] = this.rlo(s); m32[i + 1] = this.rhi(s); pc += 4; break;
        }
        case 0x67: {                                                                                     // array-len (guarded)
          const s = code[pc + 2], lo = this.rlo(s), hi = this.rhi(s);
          if ((lo & 0xF) === 9 && hi === 0 && lo !== TV && lo - 9 >= VBASE && lo - 9 < heapEnd) {
            const i = (lo - 9 - VBASE) >> 2;
            const cl = m32[i] >>> 8, ch = m32[i + 1];
            shl64(cl | (ch << 24), ch >>> 8, 1); this.setRegR(code[pc + 1]);
          } else this.setReg(code[pc + 1], 0, 0);
          pc += 3; break;
        }
        case 0x68: { const c = code[pc + 2]; this.setReg(code[pc + 1], this.allocObj(toNum(this.rlo(c), this.rhi(c)), 0x32, true), 0); pc += 3; break; }
        case 0x70: {                                                                                     // load Vd Vaddr width
          const s = code[pc + 2], a = this.rlo(s), w = code[pc + 3] & 3;
          if (this.rhi(s) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`load 0x${(a >>> 0).toString(16)}`);
          if (w === 0) this.setReg(code[pc + 1], m8[a - VBASE], 0);
          else if (w === 1) this.setReg(code[pc + 1], this.ld16(a), 0);
          else if (w === 2) this.setReg(code[pc + 1], (a & 3) === 0 ? m32[(a - VBASE) >> 2] : this.dv.getInt32(a - VBASE, true), 0);
          else if ((a & 3) === 0) { const i = (a - VBASE) >> 2; this.setReg(code[pc + 1], m32[i], m32[i + 1]); }
          else this.setReg(code[pc + 1], this.dv.getInt32(a - VBASE, true), this.dv.getInt32(a - VBASE + 4, true));
          pc += 4; break;
        }
        case 0x71: {                                                                                     // store Vaddr Vs width
          const d = code[pc + 1], s = code[pc + 2], a = this.rlo(d), w = code[pc + 3] & 3;
          if (this.rhi(d) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`store 0x${(a >>> 0).toString(16)}`);
          if (w === 0) m8[a - VBASE] = this.rlo(s) & 0xFF;
          else if (w === 1) this.st16(a, this.rlo(s) & 0xFFFF);
          else if (w === 2) { if ((a & 3) === 0) m32[(a - VBASE) >> 2] = this.rlo(s); else this.dv.setInt32(a - VBASE, this.rlo(s), true); }
          else if ((a & 3) === 0) { const i = (a - VBASE) >> 2; m32[i] = this.rlo(s); m32[i + 1] = this.rhi(s); }
          else { this.dv.setInt32(a - VBASE, this.rlo(s), true); this.dv.setInt32(a - VBASE + 4, this.rhi(s), true); }
          pc += 4; break;
        }
        case 0x72: pc += 1; break;
        case 0x80: case 0x81: {
          const t = op === 0x80 ? this.rd32(pc + 1) >>> 0 : this.fnAddrToOffset(this.rlo(code[pc + 1]), this.rhi(code[pc + 1]));
          const next = pc + (op === 0x80 ? 5 : 2);
          const f = this.compiledFor(t);
          if (f) { this.push(RET_SENTINEL, 0); this.enter(t); this.drive(f); pc = next; }   // compiled callee pops its own frame
          else { this.doCall(t, next); pc = this.pc; }
          break;
        }
        case 0x82: { this.doRet(); pc = this.pc; break; }
        case 0x83: {
          const t = this.rd32(pc + 1) >>> 0;
          this.doTailcall(t);
          const f = this.compiledFor(t);
          if (f) { this.drive(f); pc = this.pc; }                             // returns to our caller's pc
          else pc = this.pc;
          break;
        }
        case 0x88: { const base = this.bump(16); this.zero(base, base + 16); this.markStart(base); this.markCons(base); this.setReg(code[pc + 1], base | 1, 0); pc += 2; break; }
        case 0x89: { if (this.va >= this.vl) this.gc(); pc += 1; break; }
        case 0x8A: pc += 2; break;
        case 0x8B: pc += 1; break;
        case 0x90: case 0x91: this.fault('save-ctx/restore-ctx not supported');
        case 0x92: pc += 1; break;
        case 0x93: {                                                                                     // atomic-xchg
          const x = code[pc + 2], a = this.rlo(x), s = code[pc + 3];
          if (this.rhi(x) !== 0 || a < VBASE || a >= heapEnd) this.memFault('xchg');
          const i = (a - VBASE) >> 2, ol = m32[i], oh = m32[i + 1];
          m32[i] = this.rlo(s); m32[i + 1] = this.rhi(s);
          this.setReg(code[pc + 1], ol, oh); pc += 4; break;
        }
        case 0xA0: case 0xA1: case 0xA2: case 0xA3: case 0xA4: this.fault('port I/O / halt / cli / sti not supported');
        case 0xA5: case 0xA6: this.fault('percpu ops not supported');
        case 0xA7: { const t = this.rd32(pc + 2) >>> 0; this.setReg(code[pc + 1], t === FN_UNRESOLVED ? NIL : ((t << 4) | 3), 0); pc += 6; break; }
        case 0xA8: case 0xA9: {                                                                          // mul26lo / mul26hi
          const a = code[pc + 2], b = code[pc + 3];
          sar64(this.rlo(a), this.rhi(a), 1); const xl = RL, xh = RH;
          sar64(this.rlo(b), this.rhi(b), 1); const yl = RL, yh = RH;
          const p = toBig(xl, xh) * toBig(yl, yh);
          fromBig(op === 0xA8 ? ((p & 0x3FFFFFFn) << 1n) : (((p >> 26n) & 0xFFFFFFFFFFFFFFFFn) << 1n));
          this.setRegR(code[pc + 1]); pc += 4; break;
        }
        case 0xAA: case 0xAB: {                                                                          // mul64lo / mul64hi (raw unsigned)
          const a = code[pc + 2], b = code[pc + 3];
          const p = BigInt.asUintN(64, toBig(this.rlo(a), this.rhi(a))) * BigInt.asUintN(64, toBig(this.rlo(b), this.rhi(b)));
          fromBig(op === 0xAA ? p : (p >> 64n)); this.setRegR(code[pc + 1]); pc += 4; break;
        }
        case 0xAC: {                                                                                     // acc128 Vaddr Vlo Vhi
          const x = code[pc + 1], a = this.rlo(x), l = code[pc + 2], h = code[pc + 3];
          if (this.rhi(x) !== 0 || a < VBASE || a >= heapEnd) this.memFault('acc128');
          const i = (a - VBASE) >> 2;
          const cur = BigInt.asUintN(64, toBig(m32[i], m32[i + 1])) | (BigInt.asUintN(64, toBig(m32[i + 2], m32[i + 3])) << 64n);
          const add = BigInt.asUintN(64, toBig(this.rlo(l), this.rhi(l))) | (BigInt.asUintN(64, toBig(this.rlo(h), this.rhi(h))) << 64n);
          const r = BigInt.asUintN(128, cur + add);
          fromBig(r & 0xFFFFFFFFFFFFFFFFn); m32[i] = RL; m32[i + 1] = RH;
          fromBig(r >> 64n); m32[i + 2] = RL; m32[i + 3] = RH;
          pc += 4; break;
        }
        case 0xAD: case 0xAE: case 0xAF: {                                                               // mul/add/sub-checked
          const vd = code[pc + 1], a = code[pc + 2], b = code[pc + 3];
          const al = this.rlo(a), ah = this.rhi(a), bl = this.rlo(b), bh = this.rhi(b);
          let ovf = false, gen;
          if (op === 0xAE) { add64(al, ah, bl, bh); ovf = ((ah ^ RH) & (bh ^ RH)) < 0; gen = this.genAdd; }
          else if (op === 0xAF) { sub64(al, ah, bl, bh); ovf = ((ah ^ bh) & (ah ^ RH)) < 0; gen = this.genSub; }
          else {
            gen = this.genMul;
            sar64(al, ah, 1); const xl = RL, xh = RH;
            if (xh === (xl >> 31) && bh === (bl >> 31) && xl > -0x4000000 && xl < 0x4000000 && bl > -0x4000000 && bl < 0x4000000) {
              fromNum(xl * bl);
            } else {
              const p = toBig(xl, xh) * toBig(bl, bh);
              ovf = p !== BigInt.asIntN(64, p);
              fromBig(p);
            }
          }
          if (!ovf || !gen) this.setRegR(vd);
          else { this.callLisp(gen, [[al, ah], [bl, bh]]); this.setRegR(vd); }
          pc += 4; break;
        }
        case 0xB0: {                                                                                     // sap-new
          const s = code[pc + 2];
          const base = this.bump(16); this.st64(base, 0x116, 0); this.st64(base + 8, this.rlo(s), this.rhi(s)); this.markStart(base);
          this.setReg(code[pc + 1], base | 9, 0); pc += 3; break;
        }
        case 0xB1: case 0xB2: case 0xB3: {
          const s = code[pc + 2], o = code[pc + 3];
          sar64(this.rlo(o), this.rhi(o), 1);
          const a = this.ldlo(this.rlo(s) - 9 + 8) + RL;
          if (a < VBASE || a >= heapEnd) this.memFault('sap-ref');
          if (op === 0xB1) this.setReg(code[pc + 1], m8[a - VBASE] << 1, 0);
          else if (op === 0xB2) { fromNum(this.dv.getUint32(a - VBASE, true) * 2); this.setRegR(code[pc + 1]); }
          else { const i = (a - VBASE) >> 2; this.setReg(code[pc + 1], m32[i], m32[i + 1]); }
          pc += 4; break;
        }
        case 0xB4: case 0xB5: case 0xB6: {
          const s = code[pc + 1], o = code[pc + 2], v = code[pc + 3];
          sar64(this.rlo(o), this.rhi(o), 1);
          const a = this.ldlo(this.rlo(s) - 9 + 8) + RL;
          if (a < VBASE || a >= heapEnd) this.memFault('sap-set');
          if (op === 0xB4) m8[a - VBASE] = (this.rlo(v) >> 1) & 0xFF;
          else if (op === 0xB5) { sar64(this.rlo(v), this.rhi(v), 1); this.dv.setInt32(a - VBASE, RL, true); }
          else { const i = (a - VBASE) >> 2; m32[i] = this.rlo(v); m32[i + 1] = this.rhi(v); }
          pc += 4; break;
        }
        case 0xB7: { const s = code[pc + 2]; const a = this.rlo(s) - 9 + 8; shl64(this.ldlo(a), this.ldhi(a), 1); this.setRegR(code[pc + 1]); pc += 3; break; }
        case 0xB8: this.st64(A_MVCOUNT, code[pc + 1] << 1, 0); pc += 2; break;
        case 0xB9: { const c = code[pc + 2]; this.setReg(code[pc + 1], this.allocObj(toNum(this.rlo(c), this.rhi(c)), 0x31, false), 0); pc += 3; break; }
        case 0xBA: { const s = code[pc + 1]; this.st64(A_CENV, this.rlo(s), this.rhi(s)); pc += 2; break; }
        case 0xBB: this.setReg(code[pc + 1], this.ldlo(A_CENV), this.ldhi(A_CENV)); pc += 2; break;
        case 0xBC: this.st32(A_NARGS, code[pc + 1]); pc += 2; break;
        case 0xBD: this.setReg(code[pc + 1], this.ld32(A_NARGS) << 1, 0); pc += 2; break;
        case 0xBE: case 0xBF: case 0xC0: case 0xC1: {
          const x = this.floatVal(this.rlo(code[pc + 2])), y = this.floatVal(this.rlo(code[pc + 3]));
          const r = op === 0xBE ? x + y : op === 0xBF ? x - y : op === 0xC0 ? x * y : x / y;
          this.setReg(code[pc + 1], this.allocFloat(r), 0); pc += 4; break;
        }
        case 0xC2: { const s = code[pc + 2]; sar64(this.rlo(s), this.rhi(s), 1); this.setReg(code[pc + 1], this.allocFloat(toNum(RL, RH)), 0); pc += 3; break; }
        case 0xC3: {                                                                                     // ftoi (cvttsd2si)
          const d = this.floatVal(this.rlo(code[pc + 2]));
          const t = Math.trunc(d);
          if (Number.isFinite(t) && t >= -9223372036854775808 && t < 9223372036854775808) {
            if (Math.abs(t) < 4503599627370496) fromNum(t * 2); else fromBig(BigInt(t) << 1n);
          } else { RL = 0; RH = 0; }
          this.setRegR(code[pc + 1]); pc += 3; break;
        }
        case 0xC4: { const x = this.floatVal(this.rlo(code[pc + 1])), y = this.floatVal(this.rlo(code[pc + 2])); this.cmp = x < y ? -1 : x > y ? 1 : 0; pc += 3; break; }
        case 0xC5: case 0xC6: {
          const a = code[pc + 2], b = code[pc + 3], al = this.rlo(a), ah = this.rhi(a), bl = this.rlo(b), bh = this.rhi(b);
          if (op === 0xC5) { add64(al, ah, bl, bh); this.ovf = ((ah ^ RH) & (bh ^ RH)) < 0; }
          else { sub64(al, ah, bl, bh); this.ovf = ((ah ^ bh) & (ah ^ RH)) < 0; }
          this.setRegR(code[pc + 1]); pc += 4; break;
        }
        case 0xC7: pc = this.ovf ? pc + 5 + this.rd32(pc + 1) : pc + 5; break;
        case 0xC8: {                                                                                     // alloc-u8 (tagged count)
          const c = code[pc + 2]; sar64(this.rlo(c), this.rhi(c), 1); const n = toNum(RL, RH), total = align16(16 + n);
          const base = this.bump(total); this.zero(base + 8, base + total);
          this.st64(base, ((n << 8) | 0x11) | 0, Math.floor(n / 16777216) | 0); this.markStart(base);
          this.setReg(code[pc + 1], base | 9, 0); pc += 3; break;
        }
        case 0xC9: { const arr = code[pc + 2], x = code[pc + 3]; const a = this.rlo(arr) + (this.rlo(x) >> 1) + 7;
                     if (this.rhi(arr) !== 0 || a < VBASE || a >= heapEnd) this.memFault('u8-ref');
                     this.setReg(code[pc + 1], m8[a - VBASE] << 1, 0); pc += 4; break; }
        case 0xCA: { const arr = code[pc + 1], x = code[pc + 2]; const a = this.rlo(arr) + (this.rlo(x) >> 1) + 7;
                     if (this.rhi(arr) !== 0 || a < VBASE || a >= heapEnd) this.memFault('u8-set');
                     m8[a - VBASE] = (this.rlo(code[pc + 3]) >> 1) & 0xFF; pc += 4; break; }
        default: this.fault(`unknown opcode 0x${op.toString(16)}`);
      }
    }
    return pc;
  }

  // Overflowing checked arithmetic: the generic (bignum) entry, or wrap if
  // the module has none.  Result in RL/RH.
  checkedSlow(op, al, ah, bl, bh) {
    const gen = op === 0xAE ? this.genAdd : op === 0xAF ? this.genSub : this.genMul;
    if (op === 0xAD) {
      sar64(al, ah, 1);
      const p = toBig(RL, RH) * toBig(bl, bh);
      if (p === BigInt.asIntN(64, p) || !gen) { fromBig(p); return; }
    } else if (!gen) { if (op === 0xAE) add64(al, ah, bl, bh); else sub64(al, ah, bl, bh); return; }
    this.callLisp(gen, [[al, ah], [bl, bh]]);
  }

  // -- calls from JS: compiled or interpreted ----------------------------------
  // Push a return marker, build the callee frame, run the callee to completion.
  // A compiled callee pops its own frame; the interpreter's RET does the same.
  callFn(t) {
    this.push(RET_SENTINEL, 0);
    this.enter(t);
    const f = this.compiledFor(t);
    if (f) this.drive(f);
    else this.run();
  }
  // The driver: runs translated functions without JS recursion.  A translated
  // function returns -1 when it has returned (frame already popped), or
  // target*2 (+1 for a tail call) when it wants to call; the driver keeps the
  // continuation (function, resume label, frame) stack itself.  Interpreted
  // callees run in a nested run() and pop their own frame.  A longjmp unwinds
  // the continuation stack to the frame it targets.
  drive(f) {
    // continuation stack: three parallel arrays, no per-call allocation
    const cf = this.contF, cl = this.contL, ce = this.contE;
    const base = this.contDepth;
    let d = base;
    let L = 0, ebp = this.ebp;
    try {
      for (;;) {
        let r;
        try { r = f(this, L); }
        catch (e) {
          if (!(e instanceof LongJmp)) throw e;
          // find the frame the longjmp restored, in this driver's segment
          let found = false;
          if (ebp === this.ebp && f.RES && f.RES[e.ip] !== undefined) { L = f.RES[e.ip]; found = true; }
          while (!found && d > base) {
            d--;
            if (ce[d] === this.ebp && cf[d].RES && cf[d].RES[e.ip] !== undefined) { f = cf[d]; L = cf[d].RES[e.ip]; ebp = ce[d]; found = true; }
          }
          if (!found) throw e;
          continue;
        }
        if (r < 0) {                            // returned
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
        if (g) { this.contDepth = d; f = g; L = 0; ebp = this.ebp; }   // keep contDepth live: a JS stand-in (e.g. the intern memo) may run() a nested activation
        else {
          this.contDepth = d;
          this.run();                           // interpreted callee, pops its frame
          if (d === base) return;
          d--; f = cf[d]; L = cl[d]; ebp = ce[d];
        }
      }
    } finally { this.contDepth = base; }
  }
  // Translate a function once it has been called a few times: the module has
  // thousands of functions (some with 10k-instruction &rest ladders) and
  // generating + parsing JS for cold ones costs more than interpreting them.
  compiledFor(t) {
    if (!this.compileOn || this.trace > 1) return null;
    const f = this.compiled.get(t);
    if (f !== undefined) return f;
    const n = (this.callCounts.get(t) || 0) + 1;
    if (n < this.compileThreshold) { this.callCounts.set(t, n); return null; }
    const t0 = performance.now();
    const g = this.compileFn(t);
    this.compileStats.ms += performance.now() - t0;
    this.compiled.set(t, g);
    return g;
  }

  // Force a function (by exact name) to be translated now, ignoring the
  // call-count threshold.  Used to pre-warm the introspection RPC so the very
  // first completion is fast (a fresh core has an empty translation cache).
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

  // -- bytecode -> JS translation ---------------------------------------------
  // One bytecode function becomes one JS function: its basic blocks are cases
  // of a switch driven by a label variable, registers stay in the memory
  // frame (so GC roots and longjmp are exactly as for interpreted code), the
  // hot opcodes are open-coded and everything else is delegated to
  // execInsn().  Calls run the callee to completion as a JS call; RET pops
  // the frame and returns.  A function containing SETJMP gets a catch that
  // resumes at the recorded block when a longjmp lands in its frame.
  compileFn(entry) {
    const m8 = this.m8;
    const rd32 = (p) => (m8[p] | (m8[p + 1] << 8) | (m8[p + 2] << 16) | (m8[p + 3] << 24));
    // 1. explore the control-flow graph
    const starts = new Set([entry]);
    const seen = new Set();
    const work = [entry];
    const insns = [];                          // [pc, op, len]
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
    const label = new Map(); let nl = 0;
    for (const [pc] of insns) if (starts.has(pc)) label.set(pc, nl++);
    // V8 will not optimize a function whose bytecode is huge, and the image
    // has 10k-instruction &rest ladders, so a function is emitted as chunks
    // of blocks; a jump across chunks spills the registers and returns to the
    // dispatcher with the target label.
    const CHUNK = DBG.MVM_CHUNK ? parseInt(DBG.MVM_CHUNK, 10) : 150;
    const labelChunk = new Map(); let nch = 0, acc = 0;
    for (const [pc] of insns) {
      if (label.has(pc)) { if (acc >= CHUNK) { nch++; acc = 0; } labelChunk.set(label.get(pc), nch); }
      acc++;
    }
    nch++;
    // 2. emit
    // V0-V8 live in JS locals (the native build's register set); V9-V15 and
    // the frame slots stay in memory.  Locals are spilled to the frame
    // before anything that reads registers from memory, runs other code or
    // may move objects (calls, traps, GC, delegated instructions), and
    // reloaded after; a (re)entry of the function reloads them too.
    const OFF = (v) => (ROFF[v] >> 2);
    const NLOC = 9;
    const lo = (v) => v < NLOC ? `r${v}l` : v < 16 ? `m32[fb+(${OFF(v)})]` : v === 16 ? 'vm.vrl' : v === 17 ? 'vm.va' : v === 18 ? 'vm.vl' : v === 19 ? `${NIL}` : v === 20 ? 'vm.esp' : v === 21 ? 'vm.ebp' : 'vm.pc';
    const hi = (v) => v < NLOC ? `r${v}h` : v < 16 ? `m32[fb+(${OFF(v) + 1})]` : v === 16 ? 'vm.vrh' : '0';
    const W = (v, l, h) => v < NLOC ? `r${v}l=${l};r${v}h=${h};` : v < 16 ? `m32[fb+(${OFF(v)})]=${l};m32[fb+(${OFF(v) + 1})]=${h};` : v === 16 ? `vm.vrl=${l};vm.vrh=${h};` : `vm.setReg(${v},${l},${h});`;
    let STORE = '', LOAD = '', STORE5 = '', DECL = '';
    for (let v = 0; v < NLOC; v++) {
      STORE += `m32[fb+(${OFF(v)})]=r${v}l;m32[fb+(${OFF(v) + 1})]=r${v}h;`;
      LOAD += `r${v}l=m32[fb+(${OFF(v)})];r${v}h=m32[fb+(${OFF(v) + 1})];`;
      if (v <= 4) STORE5 += `m32[fb+(${OFF(v)})]=r${v}l;m32[fb+(${OFF(v) + 1})]=r${v}h;`;
      DECL += `let r${v}l=0,r${v}h=0;`;
    }
    // V0-V3 and V5-V8 are caller-saved on the native machine, so after a
    // call only V4 is meaningful; a fresh entry needs V0-V4 (args, rbx).
    const ENTRY = `if(L===0){${[0, 1, 2, 3, 4].map((v) => `r${v}l=m32[fb+(${OFF(v)})];r${v}h=m32[fb+(${OFF(v) + 1})];`).join('')}}else{r4l=m32[fb+(${OFF(4)})];r4h=m32[fb+(${OFF(4) + 1})];}`;
    const WR = (v) => W(v, 'RL', 'RH');
    let curChunk = 0;
    const J = (t) => { const l = label.get(t); return labelChunk.get(l) === curChunk ? `{L=${l};continue;}` : `{${STORE}return ${-(2 + l)};}`; };
    const chk = (l, h, a, what) => `if(${h}!==0||${a}<${VBASE}||${a}>=HE)vm.memFault(${JSON.stringify(what)});`;
    const outs = new Array(nch).fill('');
    let out = '';
    const sites = []; let nsite = 0;
    for (const [pc, op, len] of insns) {
      if (label.has(pc)) {
        const l = label.get(pc), ch = labelChunk.get(l);
        if (ch !== curChunk) {                 // chunk boundary: fall through becomes a transfer
          out += `{${STORE}return ${-(2 + l)};}\n`;
          outs[curChunk] = out; out = ''; curChunk = ch;
        }
        out += `case ${l}:\n`;
      }
      const b1 = m8[pc + 1], b2 = m8[pc + 2], b3 = m8[pc + 3], b4 = m8[pc + 4];
      const P = `vm.pc=${pc};`;
      switch (op) {
        case 0x00: break;
        case 0x02: {
          const c = b1 | (b2 << 8);
          if (c < 0x100 && c <= 4) break;                                   // frame-enter, nothing to copy
          if (c >= 0x100 && c < 0x300) break;
          out += `${P}${STORE}vm.trap(${c},${pc + len});fb=(vm.ebp-${VBASE})>>2;${LOAD}`;
          break;
        }
        case 0x10: out += W(b1, lo(b2), hi(b2)); break;
        case 0x11: out += W(b1, `${rd32(pc + 2)}`, `${rd32(pc + 6)}`); break;
        case 0x12: out += `vm.push(${lo(b1)},${hi(b1)});`; break;
        case 0x13: out += `vm.pop();${WR(b1)}`; break;
        case 0x20: out += `add64(${lo(b2)},${hi(b2)},${lo(b3)},${hi(b3)});${WR(b1)}`; break;
        case 0x21: out += `sub64(${lo(b2)},${hi(b2)},${lo(b3)},${hi(b3)});${WR(b1)}`; break;
        case 0x22: out += `sar64(${lo(b2)},${hi(b2)},1);mul64(RL,RH,${lo(b3)},${hi(b3)});${WR(b1)}`; break;
        case 0x25: out += `sub64(0,0,${lo(b2)},${hi(b2)});${WR(b1)}`; break;
        case 0x26: out += `add64(${lo(b1)},${hi(b1)},2,0);${WR(b1)}`; break;
        case 0x27: out += `sub64(${lo(b1)},${hi(b1)},2,0);${WR(b1)}`; break;
        case 0x28: out += W(b1, `${lo(b2)}&${lo(b3)}`, `${hi(b2)}&${hi(b3)}`); break;
        case 0x29: out += W(b1, `${lo(b2)}|${lo(b3)}`, `${hi(b2)}|${hi(b3)}`); break;
        case 0x2A: out += W(b1, `${lo(b2)}^${lo(b3)}`, `${hi(b2)}^${hi(b3)}`); break;
        case 0x2B: out += `shl64(${lo(b2)},${hi(b2)},${b3});${WR(b1)}`; break;
        case 0x2C: out += `shr64(${lo(b2)},${hi(b2)},${b3});${WR(b1)}`; break;
        case 0x2D: out += `sar64(${lo(b2)},${hi(b2)},${b3});${WR(b1)}`; break;
        case 0x30: out += `cmp=cmp64(${lo(b1)},${hi(b1)},${lo(b2)},${hi(b2)});`; break;
        case 0x31: out += `{const l=${lo(b1)}&${lo(b2)},h=${hi(b1)}&${hi(b2)};cmp=(l===0&&h===0)?0:(h<0?-1:1);}`; break;
        case 0x14: {
          const idx = rd32(pc + 2);
          if (pc >= JIT_PHYS) out += `{const v=vm.ldlo(${A_WEB_CONSTS});if(v===0)vm.fault('li-const: no constant vector');const a=v+${7 + 8 * idx};${W(b1, 'vm.ldlo(a)', 'vm.ldhi(a)')}}`;
          else { const off = this.mod.addrTable[idx] | 0; out += W(b1, `${off === 0 ? 0 : POOL_ADDR + off}`, '0'); }
          break;
        }
        case 0x68: out += `${P}${W(b1, `vm.allocObj(toNum(${lo(b2)},${hi(b2)}),0x32,true)`, '0')}`; break;
        case 0xB9: out += `${P}${W(b1, `vm.allocObj(toNum(${lo(b2)},${hi(b2)}),0x31,false)`, '0')}`; break;
        case 0xC9: out += `${P}{const a=${lo(b2)}+(${lo(b3)}>>1)+7;${chk(lo(b2), hi(b2), 'a', 'u8-ref')}${W(b1, `m8[a-${VBASE}]<<1`, '0')}}`; break;
        case 0xCA: out += `${P}{const a=${lo(b1)}+(${lo(b2)}>>1)+7;${chk(lo(b1), hi(b1), 'a', 'u8-set')}m8[a-${VBASE}]=(${lo(b3)}>>1)&255;}`; break;
        case 0x40: out += J(pc + len + rd32(pc + 1)); break;
        case 0x41: out += `if(cmp===0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x42: out += `if(cmp!==0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x43: out += `if(cmp<0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x44: out += `if(cmp>=0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x45: out += `if(cmp<=0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x46: out += `if(cmp>0)${J(pc + len + rd32(pc + 1))}`; break;
        case 0x47: out += `if(${lo(b1)}===${NIL}&&${hi(b1)}===0)${J(pc + len + rd32(pc + 2))}`; break;
        case 0x48: out += `if(${lo(b1)}!==${NIL}||${hi(b1)}!==0)${J(pc + len + rd32(pc + 2))}`; break;
        case 0x50: case 0x51: {
          const d = op === 0x50 ? -1 : 7;
          out += `${P}{const l=${lo(b2)},a=l+(${d});if(${hi(b2)}!==0||(l&7)!==1||a<${VBASE}||a>=HE)vm.memFault('car/cdr of non-cons');const i=(a-${VBASE})>>2;${W(b1, 'm32[i]', 'm32[i+1]')}}`;
          break;
        }
        case 0x52: out += `${P}${W(b1, `vm.allocCons(${lo(b2)},${hi(b2)},${lo(b3)},${hi(b3)})`, '0')}`; break;
        case 0x53: case 0x54: {
          const d = op === 0x53 ? -1 : 7;
          out += `${P}{const l=${lo(b1)},a=l+(${d});if(${hi(b1)}!==0||(l&7)!==1||a<${VBASE}||a>=HE)vm.memFault('rplac on non-cons');const i=(a-${VBASE})>>2;m32[i]=${lo(b2)};m32[i+1]=${hi(b2)};}`;
          break;
        }
        case 0x55: out += `{const l=${lo(b2)};${W(b1, `(!(l===${NIL}&&${hi(b2)}===0)&&(l&15)===1)?${TV}:${NIL}`, '0')}}`; break;
        case 0x56: out += `{const l=${lo(b2)};${W(b1, `(!(l===${NIL}&&${hi(b2)}===0)&&(l&15)===1)?${NIL}:${TV}`, '0')}}`; break;
        case 0x60: out += `${P}${W(b1, `vm.allocObj(${b2 | (b3 << 8)},${b4},true)`, '0')}`; break;
        case 0x61: {
          if (b2 === 21) out += `{const i=fb+(${(SLOT_BASE >> 2) - 2 * b3});${W(b1, 'm32[i]', 'm32[i+1]')}}`;
          else out += `${P}{const a=${lo(b2)}+${7 + 8 * b3};${chk(lo(b2), hi(b2), 'a', 'obj-ref')}const i=(a-${VBASE})>>2;${W(b1, 'm32[i]', 'm32[i+1]')}}`;
          break;
        }
        case 0x62: {
          if (b1 === 21) out += `{const i=fb+(${(SLOT_BASE >> 2) - 2 * b2});m32[i]=${lo(b3)};m32[i+1]=${hi(b3)};}`;
          else out += `${P}{const a=${lo(b1)}+${7 + 8 * b2};${chk(lo(b1), hi(b1), 'a', 'obj-set')}const i=(a-${VBASE})>>2;m32[i]=${lo(b3)};m32[i+1]=${hi(b3)};}`;
          break;
        }
        case 0x63: out += W(b1, `(${lo(b2)}&15)<<1`, '0'); break;
        case 0x64: out += `{const l=${lo(b2)};let r=0;if((l&15)===9&&${hi(b2)}===0&&l!==${TV}&&l-9>=${VBASE}&&l-9<HE)r=(m32[(l-9-${VBASE})>>2]&255)<<1;${W(b1, 'r', '0')}}`; break;
        case 0x65: out += `${P}{const a=${lo(b2)}+${lo(b3)}*4+7;${chk(lo(b2), hi(b2), 'a', 'aref')}const i=(a-${VBASE})>>2;${W(b1, 'm32[i]', 'm32[i+1]')}}`; break;
        case 0x66: out += `${P}{const a=${lo(b1)}+${lo(b2)}*4+7;${chk(lo(b1), hi(b1), 'a', 'aset')}const i=(a-${VBASE})>>2;m32[i]=${lo(b3)};m32[i+1]=${hi(b3)};}`; break;
        case 0x67: out += `{const l=${lo(b2)};if((l&15)===9&&${hi(b2)}===0&&l!==${TV}&&l-9>=${VBASE}&&l-9<HE){const i=(l-9-${VBASE})>>2,cl=m32[i]>>>8,ch=m32[i+1];shl64(cl|(ch<<24),ch>>>8,1);${WR(b1)}}else{${W(b1, '0', '0')}}}`; break;
        case 0x70: {
          const w = b3 & 3;
          out += `${P}{const a=${lo(b2)};${chk(lo(b2), hi(b2), 'a', 'load')}`;
          if (w === 0) out += W(b1, `m8[a-${VBASE}]`, '0');
          else if (w === 1) out += W(b1, `vm.ld16(a)`, '0');
          else if (w === 2) out += W(b1, `((a&3)===0?m32[(a-${VBASE})>>2]:vm.dv.getInt32(a-${VBASE},true))`, '0');
          else out += `if((a&3)===0){const i=(a-${VBASE})>>2;${W(b1, 'm32[i]', 'm32[i+1]')}}else{${W(b1, `vm.dv.getInt32(a-${VBASE},true)`, `vm.dv.getInt32(a-${VBASE}+4,true)`)}}`;
          out += '}';
          break;
        }
        case 0x71: {
          const w = b3 & 3;
          out += `${P}{const a=${lo(b1)};${chk(lo(b1), hi(b1), 'a', 'store')}`;
          if (w === 0) out += `m8[a-${VBASE}]=${lo(b2)}&255;`;
          else if (w === 1) out += `vm.st16(a,${lo(b2)}&65535);`;
          else if (w === 2) out += `if((a&3)===0)m32[(a-${VBASE})>>2]=${lo(b2)};else vm.dv.setInt32(a-${VBASE},${lo(b2)},true);`;
          else out += `if((a&3)===0){const i=(a-${VBASE})>>2;m32[i]=${lo(b2)};m32[i+1]=${hi(b2)};}else{vm.dv.setInt32(a-${VBASE},${lo(b2)},true);vm.dv.setInt32(a-${VBASE}+4,${hi(b2)},true);}`;
          out += '}';
          break;
        }
        case 0x72: case 0x8A: case 0x8B: case 0x92: break;
        case 0x80: case 0x81: {
          if (op === 0x80) {
            const t = rd32(pc + 1) >>> 0;
            const c = `c${nsite++}`;
            sites.push(c);
            out += `${P}${STORE5}vm.resumeL=${label.get(pc + len)};if(${c}===undefined)${c}=vm.compiledFor(${t});vm.nextF=${c};vm.push(${RET_SENTINEL},0);vm.enter(${t});return ${t * 2};`;
          } else {
            out += `${P}${STORE5}{const t=vm.fnAddrToOffset(${lo(b1)},${hi(b1)});vm.resumeL=${label.get(pc + len)};vm.push(${RET_SENTINEL},0);vm.enter(t);return t*2;}`;
          }
          break;
        }
        case 0x82: out += `vm.doRet();return -1;`; break;
        case 0x83: out += `${P}${STORE5}{const t=${rd32(pc + 1) >>> 0};vm.doTailcall(t);return t*2+1;}`; break;
        case 0x88: out += `${P}{const b=vm.bump(16);vm.zero(b,b+16);vm.markStart(b);vm.markCons(b);${W(b1, 'b|1', '0')}}`; break;
        case 0x89: out += `if(vm.va>=vm.vl){${P}${STORE}vm.gc();${LOAD}}`; break;
        case 0xA7: { const t = rd32(pc + 2) >>> 0; out += W(b1, `${t === FN_UNRESOLVED ? NIL : ((t << 4) | 3)}`, '0'); break; }
        case 0xB8: out += `vm.st64(${A_MVCOUNT},${b1 << 1},0);`; break;
        case 0xBA: out += `vm.st64(${A_CENV},${lo(b1)},${hi(b1)});`; break;
        case 0xBB: out += W(b1, `vm.ldlo(${A_CENV})`, `vm.ldhi(${A_CENV})`); break;
        case 0xBC: out += `vm.st32(${A_NARGS},${b1});`; break;
        case 0xBD: out += W(b1, `vm.ld32(${A_NARGS})<<1`, '0'); break;
        case 0xC5: case 0xC6: {
          const f = op === 0xC5 ? 'add64' : 'sub64';
          const ov = op === 0xC5 ? `((ah^RH)&(bh^RH))<0` : `((ah^bh)&(ah^RH))<0`;
          out += `{const ah=${hi(b2)},bh=${hi(b3)};${f}(${lo(b2)},ah,${lo(b3)},bh);ovf=${ov};${WR(b1)}}`;
          break;
        }
        case 0xC7: out += `if(ovf)${J(pc + len + rd32(pc + 1))}`; break;
        case 0xAE: case 0xAF: if (DBG.MVM_NOCHK || DBG.MVM_NOCHK_AE) { out += `${P}${STORE}vm.execInsn(${pc});${LOAD}`; break; } {  // add/sub-checked: fast path inline, overflow -> generic
          const f = op === 0xAE ? 'add64' : 'sub64';
          const ov = op === 0xAE ? `((ah^RH)&(bh^RH))<0` : `((ah^bh)&(ah^RH))<0`;
          out += `{const al=${lo(b2)},ah=${hi(b2)},bl=${lo(b3)},bh=${hi(b3)};${f}(al,ah,bl,bh);if(${ov}){${P}${STORE}vm.checkedSlow(${op},al,ah,bl,bh);${LOAD}}${WR(b1)}}`;
          break;
        }
        case 0xAD: if (DBG.MVM_NOCHK || DBG.MVM_NOCHK_AD) { out += `${P}${STORE}vm.execInsn(${pc});${LOAD}`; break; } {
          out += `{const al=${lo(b2)},ah=${hi(b2)},bl=${lo(b3)},bh=${hi(b3)};sar64(al,ah,1);const xl=RL,xh=RH;if(xh===(xl>>31)&&bh===(bl>>31)&&xl>-0x4000000&&xl<0x4000000&&bl>-0x4000000&&bl<0x4000000)fromNum(xl*bl);else{${P}${STORE}vm.checkedSlow(${op},al,ah,bl,bh);${LOAD}}${WR(b1)}}`;
          break;
        }
        default:
          // everything else runs through the interpreter's executor; ops that
          // set flags publish them in vm.cmp / vm.ovf
          out += `${P}${STORE}vm.execInsn(${pc});${LOAD}`;
          if (DBG.MVM_DCOUNT) out += `vm.dcount.set(${op},(vm.dcount.get(${op})||0)+1);`;
          this.compileStats.delegated.set(op, (this.compileStats.delegated.get(op) || 0) + 1);
          if (op === 0x31 || op === 0xC4) out += `cmp=vm.cmp;`;
          break;
      }
      out += '\n';
      if (op === 0x82 || op === 0x83 || op === 0x40 || op === 0x80 || op === 0x81) out += `vm.fault('fell off a block');\n`;
    }
    outs[curChunk] = out;
    const ENTRYX = `if(L===0){${[0, 1, 2, 3, 4].map((v) => `r${v}l=m32[fb+(${OFF(v)})];r${v}h=m32[fb+(${OFF(v) + 1})];`).join('')}}else{${LOAD}}`;
    const mk = (o, i) => `function(vm,L){const m32=vm.m32,m8=vm.m8;let fb=(vm.ebp-${VBASE})>>2;const HE=vm.heapEnd;let cmp=0,ovf=false;${DECL}${i === 0 && nch === 1 ? ENTRY : ENTRYX}for(;;)switch(L){\n${o}default: vm.fault('bad block label '+L);\n}}`;
    let body;
    if (nch === 1) body = `return ${mk(outs[0], 0)};`;
    else {
      const lc = []; for (const [l, c] of labelChunk) lc[l] = c;
      body = `const CH=[${outs.map(mk).join(',')}];const LC=[${lc.join(',')}];return function(vm,L){for(;;){const r=CH[LC[L]](vm,L);if(r>=-1)return r;L=-2-r;}};`;
    }
    if (DBG.MVM_DUMPCHK && body.includes('checkedSlow') && !this.dumped) { this.dumped = true; this.host.log(`[compiled ${this.where(entry)}]\n${body}`); }
    if (DBG.MVM_DUMP && this.where(entry).startsWith(DBG.MVM_DUMP)) this.host.log(`[compiled ${this.where(entry)}]\n${body}`);
    let f;
    const decl = sites.length ? `let ${sites.map((c) => c + '=undefined').join(',')};` : '';
    try { f = eval(`(function(){${decl}${body}})()`); }
    catch (e) { this.host.log(`[compile ${this.where(entry)}: ${e.message}]`); this.compileStats.failed++; return null; }
    if (hasSetjmp) { f.RES = {}; for (const [pc, l] of label) f.RES[pc] = l; }
    this.compileStats.fns++; this.compileStats.insns += insns.length;
    return f;
  }

  // -- snapshots ------------------------------------------------------------
  snapshot() {
    this.gc();
    const fromStart = this.ldlo(A_GC_FROM);
    const ranges = [
      [VBASE, BSS_END],
      [POOL_ADDR, POOL_ADDR + this.mod.pool.length],
      [this.esp, STACK_ADDR + STACK_SIZE],
      [fromStart, this.va],
      [JIT_ADDR, this.mmapNext],
    ];
    const g0 = (fromStart - this.pageBase) >> 4, g1 = (this.va - this.pageBase) >> 4;
    return {
      version: 2,
      semi: this.semi,
      regs: { vrl: this.vrl, vrh: this.vrh, va: this.va, vl: this.vl, esp: this.esp, ebp: this.ebp, pc: this.pc,
              cmp: this.cmp, ovf: this.ovf, gcCount: this.gcCount, mmapNext: this.mmapNext, steps: this.steps },
      ranges: ranges.map(([a, b]) => ({ addr: a, bytes: this.m8.slice(a - VBASE, b - VBASE) })),
      bitmaps: { g0, startBmp: this.startBmp.slice(g0 >> 3, (g1 >> 3) + 1),
                 consBmp: this.consBmp.slice(g0 >> 3, (g1 >> 3) + 1) },
    };
  }
  restore(core, argv, env) {
    if (core.version !== 2) throw new Error('core version mismatch');
    if (core.semi !== this.semi) throw new Error(`core was made with a ${core.semi >> 20} MB semispace`);
    this.m8.fill(0);
    this.installModule();
    for (const r of core.ranges) this.m8.set(r.bytes, r.addr - VBASE);
    this.startBmp.fill(0); this.consBmp.fill(0);
    this.startBmp.set(core.bitmaps.startBmp, core.bitmaps.g0 >> 3);
    this.consBmp.set(core.bitmaps.consBmp, core.bitmaps.g0 >> 3);
    const r = core.regs;
    this.vrl = r.vrl; this.vrh = r.vrh; this.va = r.va; this.vl = r.vl; this.esp = r.esp; this.ebp = r.ebp; this.pc = r.pc;
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

  // -- top level ------------------------------------------------------------
  main(resumed = false) {
    if (!resumed) {
      const km = this.byName.get('KERNEL-MAIN');
      if (!km) throw new Error('no KERNEL-MAIN in module');
      // a pseudo-frame for the boot stub so enter() has registers to copy
      this.ebp = this.esp;
      this.esp -= FRAME_SIZE;
      for (let v = 0; v < 16; v++) this.setReg(v, NIL, 0);
      this.doCall(km.off, RET_SENTINEL);
    }
    try { this.run(); return 0; }
    catch (e) { if (e instanceof MvmExit) return e.code; throw e; }
  }
}

const MVM_EXPORTS = { MVM, loadModule, MvmFault, MvmExit, NIL, TV, VBASE };
if (typeof module !== 'undefined' && module.exports) module.exports = MVM_EXPORTS;
else if (typeof self !== 'undefined') self.MVM_EXPORTS = MVM_EXPORTS;
