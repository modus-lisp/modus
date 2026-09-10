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
const FN_UNRESOLVED = 0xFFFFFFF0;

class LongJmp { constructor(esp) { this.esp = esp; } }
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
    this.watchAt = -1; this.watchLeft = 0;
    this.genAdd = this.byName.get('GENERIC-ADD');
    this.genSub = this.byName.get('GENERIC-SUBTRACT');
    this.genMul = this.byName.get('GENERIC-MULTIPLY');
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
    if (this.esp < STACK_ADDR + 4096) this.fault('stack overflow');
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
    this.doCall(fn.off, RET_SENTINEL);
    this.run();
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
    throw new LongJmp(esp);
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
        this.doCall(phys, RET_SENTINEL);
        this.run();                           // VR holds the result; the caller's frame is intact
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

  // Linux x86-64 numbering, the subset the hosted CLI uses.
  syscall(nr, a1, a2, a3, a4, a5, a6) {
    const h = this.host;
    const inMem = (a, n) => a >= VBASE && a + n <= this.heapEnd;
    switch (nr) {
      case 60: case 231: throw new MvmExit(a1);
      case 0: return inMem(a2, a3) ? h.read(a1, this.m8, a2 - VBASE, a3) : -14;
      case 1: return inMem(a2, a3) ? h.write(a1, this.m8, a2 - VBASE, a3) : -14;
      case 2: return h.open(this.cstr(a1), a2, a3);
      case 3: return h.close(a1);
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
    const code = this.code, m32 = this.m32, m8 = this.m8, self = this;
    const heapEnd = this.heapEnd;
    const rd32 = (p) => (code[p] | (code[p + 1] << 8) | (code[p + 2] << 16) | (code[p + 3] << 24));
    const IX = (v) => (self.ebp + ROFF[v] - VBASE) >> 2;
    const RLO = (v) => (v < 16 ? m32[IX(v)] : self.rlo(v));
    const RHI = (v) => (v < 16 ? m32[IX(v) + 1] : self.rhi(v));
    const W = (v, lo, hi) => { if (v < 16) { const i = IX(v); m32[i] = lo; m32[i + 1] = hi; } else self.setReg(v, lo, hi); };
    const WR = (v) => W(v, RL, RH);
    let pc = this.pc;
    for (;;) {
      if (this.trace) {
        this.steps++;
        if (this.trace > 1 && (!this.traceFrom || (this.steps >= this.traceFrom && this.steps < this.traceFrom + this.traceCount))) this.host.log(`${this.steps} ${this.where(pc)} op=${code[pc].toString(16)} ${this.traceRegs ? this.regDump() : ''}`);
        else if (this.maxSteps && this.steps >= this.maxSteps) { this.pc = pc; this.fault('step limit'); }
        else if (this.prof && (this.steps & 255) === 0) { const f = this.fnAt(pc); if (f) this.prof.set(-f.off - 1, (this.prof.get(-f.off - 1) || 0) + 1); }
        else if ((this.steps & 0x3FFFFFF) === 0) { this.pc = pc; const bt = this.backtrace(200); this.host.log(`[${this.steps} steps, gc ${this.gcCount}, heap ${(this.va - this.heapBase) >> 10}K, depth ${bt.length}] ${bt.slice(0, 4).join(' < ')} ... ${bt.slice(-4).join(' < ')}`); }
      }
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
        case 0x10: { const s = code[pc + 2]; W(code[pc + 1], RLO(s), RHI(s)); pc += 3; break; }        // mov
        case 0x11: W(code[pc + 1], rd32(pc + 2), rd32(pc + 6)); pc += 10; break;                       // li imm64
        case 0x12: { const s = code[pc + 1]; this.push(RLO(s), RHI(s)); pc += 2; break; }              // push
        case 0x13: this.pop(); WR(code[pc + 1]); pc += 2; break;                                        // pop
        case 0x14: {                                                                                     // li-const
          const idx = rd32(pc + 2);
          if (pc >= JIT_PHYS) {                                                                          // mvm-eval quote pool
            const vec = this.ldlo(A_WEB_CONSTS);
            if (vec === 0) this.fault('li-const in a page with no constant vector');
            const a = vec + 7 + 8 * idx;
            W(code[pc + 1], this.ldlo(a), this.ldhi(a));
          } else {
            const off = this.mod.addrTable[idx] | 0;
            W(code[pc + 1], off === 0 ? 0 : (POOL_ADDR + off), 0);
          }
          pc += 10; break;
        }
        case 0x20: { const a = code[pc + 2], b = code[pc + 3]; add64(RLO(a), RHI(a), RLO(b), RHI(b)); WR(code[pc + 1]); pc += 4; break; }
        case 0x21: { const a = code[pc + 2], b = code[pc + 3]; sub64(RLO(a), RHI(a), RLO(b), RHI(b)); WR(code[pc + 1]); pc += 4; break; }
        case 0x22: {                                                                                     // mul: (a>>1)*b wrap
          const a = code[pc + 2], b = code[pc + 3];
          sar64(RLO(a), RHI(a), 1);
          mul64(RL, RH, RLO(b), RHI(b)); WR(code[pc + 1]); pc += 4; break;
        }
        case 0x23: case 0x24: {                                                                          // div / mod (truncating)
          const a = code[pc + 2], b = code[pc + 3];
          sar64(RLO(a), RHI(a), 1); const xl = RL, xh = RH;
          sar64(RLO(b), RHI(b), 1); const yl = RL, yh = RH;
          if (yl === 0 && yh === 0) this.memFault('division by zero');
          if (fits53(xl, xh) && fits53(yl, yh)) {
            const x = toNum(xl, xh), y = toNum(yl, yh);
            const r = op === 0x23 ? Math.trunc(x / y) : x % y;
            fromNum(r * 2);
          } else {
            const X = toBig(xl, xh), Y = toBig(yl, yh);
            fromBig((op === 0x23 ? X / Y : X % Y) << 1n);
          }
          WR(code[pc + 1]); pc += 4; break;
        }
        case 0x25: { const s = code[pc + 2]; sub64(0, 0, RLO(s), RHI(s)); WR(code[pc + 1]); pc += 3; break; }   // neg
        case 0x26: { const d = code[pc + 1]; add64(RLO(d), RHI(d), 2, 0); WR(d); pc += 2; break; }
        case 0x27: { const d = code[pc + 1]; sub64(RLO(d), RHI(d), 2, 0); WR(d); pc += 2; break; }
        case 0x28: { const a = code[pc + 2], b = code[pc + 3]; W(code[pc + 1], RLO(a) & RLO(b), RHI(a) & RHI(b)); pc += 4; break; }
        case 0x29: { const a = code[pc + 2], b = code[pc + 3]; W(code[pc + 1], RLO(a) | RLO(b), RHI(a) | RHI(b)); pc += 4; break; }
        case 0x2A: { const a = code[pc + 2], b = code[pc + 3]; W(code[pc + 1], RLO(a) ^ RLO(b), RHI(a) ^ RHI(b)); pc += 4; break; }
        case 0x2B: { const s = code[pc + 2]; shl64(RLO(s), RHI(s), code[pc + 3]); WR(code[pc + 1]); pc += 4; break; }
        case 0x2C: { const s = code[pc + 2]; shr64(RLO(s), RHI(s), code[pc + 3]); WR(code[pc + 1]); pc += 4; break; }
        case 0x2D: { const s = code[pc + 2]; sar64(RLO(s), RHI(s), code[pc + 3]); WR(code[pc + 1]); pc += 4; break; }
        case 0x2F: { const s = code[pc + 2]; shl64(RLO(s), RHI(s), RLO(code[pc + 3]) & 63); WR(code[pc + 1]); pc += 4; break; }
        case 0x32: { const s = code[pc + 2]; sar64(RLO(s), RHI(s), RLO(code[pc + 3]) & 63); WR(code[pc + 1]); pc += 4; break; }
        case 0x2E: {                                                                                     // ldb pos size
          const s = code[pc + 2], pos = code[pc + 3], size = code[pc + 4];
          shr64(RLO(s), RHI(s), pos);
          if (size < 32) { RL &= (1 << size) - 1; RH = 0; }
          else if (size === 32) RH = 0;
          else if (size < 64) RH &= (1 << (size - 32)) - 1;
          WR(code[pc + 1]); pc += 5; break;
        }
        case 0x30: { const a = code[pc + 1], b = code[pc + 2]; this.cmp = cmp64(RLO(a), RHI(a), RLO(b), RHI(b)); pc += 3; break; }
        case 0x31: { const a = code[pc + 1], b = code[pc + 2]; const lo = RLO(a) & RLO(b), hi = RHI(a) & RHI(b);
                     this.cmp = (lo === 0 && hi === 0) ? 0 : (hi < 0 ? -1 : 1); pc += 3; break; }
        case 0x40: pc = pc + 5 + rd32(pc + 1); break;
        case 0x41: pc = this.cmp === 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x42: pc = this.cmp !== 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x43: pc = this.cmp < 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x44: pc = this.cmp >= 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x45: pc = this.cmp <= 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x46: pc = this.cmp > 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x47: { const s = code[pc + 1]; pc = (RLO(s) === NIL && RHI(s) === 0) ? pc + 6 + rd32(pc + 2) : pc + 6; break; }
        case 0x48: { const s = code[pc + 1]; pc = (RLO(s) !== NIL || RHI(s) !== 0) ? pc + 6 + rd32(pc + 2) : pc + 6; break; }
        case 0x50: case 0x51: {                                                                          // car / cdr: bare deref
          const s = code[pc + 2], lo = RLO(s), hi = RHI(s);
          const a = lo + (op === 0x50 ? -1 : 7);
          if (hi !== 0 || (lo & 7) !== 1 || a < VBASE || a >= heapEnd) this.memFault((op === 0x50 ? 'car' : 'cdr') + ` of non-cons 0x${(lo >>> 0).toString(16)}`);
          const i = (a - VBASE) >> 2;
          W(code[pc + 1], m32[i], m32[i + 1]); pc += 3; break;
        }
        case 0x52: { const a = code[pc + 2], b = code[pc + 3]; W(code[pc + 1], this.allocCons(RLO(a), RHI(a), RLO(b), RHI(b)), 0); pc += 4; break; }
        case 0x53: case 0x54: {                                                                          // setcar / setcdr
          const c = code[pc + 1], lo = RLO(c), hi = RHI(c), s = code[pc + 2];
          const a = lo + (op === 0x53 ? -1 : 7);
          if (hi !== 0 || (lo & 7) !== 1 || a < VBASE || a >= heapEnd) this.memFault(`rplac on non-cons 0x${(lo >>> 0).toString(16)}`);
          const i = (a - VBASE) >> 2;
          m32[i] = RLO(s); m32[i + 1] = RHI(s); pc += 3; break;
        }
        case 0x55: { const s = code[pc + 2], lo = RLO(s); W(code[pc + 1], (!(lo === NIL && RHI(s) === 0) && (lo & 0xF) === 1) ? TV : NIL, 0); pc += 3; break; }
        case 0x56: { const s = code[pc + 2], lo = RLO(s); W(code[pc + 1], (!(lo === NIL && RHI(s) === 0) && (lo & 0xF) === 1) ? NIL : TV, 0); pc += 3; break; }
        case 0x60: {                                                                                     // alloc-obj count subtag
          const count = code[pc + 2] | (code[pc + 3] << 8), subtag = code[pc + 4];
          W(code[pc + 1], this.allocObj(count, subtag, true), 0); pc += 5; break;
        }
        case 0x61: {                                                                                     // obj-ref Vd Vobj idx
          const vobj = code[pc + 2], idx = code[pc + 3];
          let a;
          if (vobj === 21) a = this.ebp + SLOT_BASE - 8 * idx;
          else { const lo = RLO(vobj); a = lo + 7 + 8 * idx; if (RHI(vobj) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`obj-ref on 0x${(lo >>> 0).toString(16)}`); }
          const i = (a - VBASE) >> 2;
          W(code[pc + 1], m32[i], m32[i + 1]); pc += 4; break;
        }
        case 0x62: {                                                                                     // obj-set Vobj idx Vs
          const vobj = code[pc + 1], idx = code[pc + 2], s = code[pc + 3];
          let a;
          if (vobj === 21) a = this.ebp + SLOT_BASE - 8 * idx;
          else { const lo = RLO(vobj); a = lo + 7 + 8 * idx; if (RHI(vobj) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`obj-set on 0x${(lo >>> 0).toString(16)}`); }
          const i = (a - VBASE) >> 2;
          m32[i] = RLO(s); m32[i + 1] = RHI(s); pc += 4; break;
        }
        case 0x63: W(code[pc + 1], (RLO(code[pc + 2]) & 0xF) << 1, 0); pc += 3; break;                  // obj-tag
        case 0x64: {                                                                                     // obj-subtag (guarded)
          const s = code[pc + 2], lo = RLO(s), hi = RHI(s);
          let r = 0;
          if ((lo & 0xF) === 9 && hi === 0 && lo !== TV && lo - 9 >= VBASE && lo - 9 < heapEnd) r = (m32[(lo - 9 - VBASE) >> 2] & 0xFF) << 1;
          W(code[pc + 1], r, 0); pc += 3; break;
        }
        case 0x65: {                                                                                     // aref Vd Vobj Vidx
          const o = code[pc + 2], x = code[pc + 3];
          const a = RLO(o) + RLO(x) * 4 + 7;
          if (RHI(o) !== 0 || a < VBASE || a >= heapEnd) this.memFault('aref');
          const i = (a - VBASE) >> 2;
          W(code[pc + 1], m32[i], m32[i + 1]); pc += 4; break;
        }
        case 0x66: {                                                                                     // aset Vobj Vidx Vs
          const o = code[pc + 1], x = code[pc + 2], s = code[pc + 3];
          const a = RLO(o) + RLO(x) * 4 + 7;
          if (RHI(o) !== 0 || a < VBASE || a >= heapEnd) this.memFault('aset');
          const i = (a - VBASE) >> 2;
          m32[i] = RLO(s); m32[i + 1] = RHI(s); pc += 4; break;
        }
        case 0x67: {                                                                                     // array-len (guarded)
          const s = code[pc + 2], lo = RLO(s), hi = RHI(s);
          if ((lo & 0xF) === 9 && hi === 0 && lo !== TV && lo - 9 >= VBASE && lo - 9 < heapEnd) {
            const i = (lo - 9 - VBASE) >> 2;
            const cl = m32[i] >>> 8, ch = m32[i + 1];
            shl64(cl | (ch << 24), ch >>> 8, 1); WR(code[pc + 1]);
          } else W(code[pc + 1], 0, 0);
          pc += 3; break;
        }
        case 0x68: { const c = code[pc + 2]; W(code[pc + 1], this.allocObj(toNum(RLO(c), RHI(c)), 0x32, true), 0); pc += 3; break; }
        case 0x70: {                                                                                     // load Vd Vaddr width
          const s = code[pc + 2], a = RLO(s), w = code[pc + 3] & 3;
          if (RHI(s) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`load 0x${(a >>> 0).toString(16)}`);
          if (w === 0) W(code[pc + 1], m8[a - VBASE], 0);
          else if (w === 1) W(code[pc + 1], this.ld16(a), 0);
          else if (w === 2) W(code[pc + 1], (a & 3) === 0 ? m32[(a - VBASE) >> 2] : this.dv.getInt32(a - VBASE, true), 0);
          else if ((a & 3) === 0) { const i = (a - VBASE) >> 2; W(code[pc + 1], m32[i], m32[i + 1]); }
          else W(code[pc + 1], this.dv.getInt32(a - VBASE, true), this.dv.getInt32(a - VBASE + 4, true));
          pc += 4; break;
        }
        case 0x71: {                                                                                     // store Vaddr Vs width
          const d = code[pc + 1], s = code[pc + 2], a = RLO(d), w = code[pc + 3] & 3;
          if (RHI(d) !== 0 || a < VBASE || a >= heapEnd) this.memFault(`store 0x${(a >>> 0).toString(16)}`);
          if (w === 0) m8[a - VBASE] = RLO(s) & 0xFF;
          else if (w === 1) this.st16(a, RLO(s) & 0xFFFF);
          else if (w === 2) { if ((a & 3) === 0) m32[(a - VBASE) >> 2] = RLO(s); else this.dv.setInt32(a - VBASE, RLO(s), true); }
          else if ((a & 3) === 0) { const i = (a - VBASE) >> 2; m32[i] = RLO(s); m32[i + 1] = RHI(s); }
          else { this.dv.setInt32(a - VBASE, RLO(s), true); this.dv.setInt32(a - VBASE + 4, RHI(s), true); }
          pc += 4; break;
        }
        case 0x72: pc += 1; break;
        case 0x80: { this.doCall(rd32(pc + 1) >>> 0, pc + 5); pc = this.pc; break; }
        case 0x81: { const s = code[pc + 1]; const t = this.fnAddrToOffset(RLO(s), RHI(s)); this.doCall(t, pc + 2); pc = this.pc; break; }
        case 0x82: { this.doRet(); pc = this.pc; if (pc === RET_SENTINEL) return; break; }
        case 0x83: { this.doTailcall(rd32(pc + 1) >>> 0); pc = this.pc; break; }
        case 0x88: { const base = this.bump(16); this.zero(base, base + 16); this.markStart(base); this.markCons(base); W(code[pc + 1], base | 1, 0); pc += 2; break; }
        case 0x89: { if (this.va >= this.vl) this.gc(); pc += 1; break; }
        case 0x8A: pc += 2; break;
        case 0x8B: pc += 1; break;
        case 0x90: case 0x91: this.fault('save-ctx/restore-ctx not supported');
        case 0x92: pc += 1; break;
        case 0x93: {                                                                                     // atomic-xchg
          const x = code[pc + 2], a = RLO(x), s = code[pc + 3];
          if (RHI(x) !== 0 || a < VBASE || a >= heapEnd) this.memFault('xchg');
          const i = (a - VBASE) >> 2, ol = m32[i], oh = m32[i + 1];
          m32[i] = RLO(s); m32[i + 1] = RHI(s);
          W(code[pc + 1], ol, oh); pc += 4; break;
        }
        case 0xA0: case 0xA1: case 0xA2: case 0xA3: case 0xA4: this.fault('port I/O / halt / cli / sti not supported');
        case 0xA5: case 0xA6: this.fault('percpu ops not supported');
        case 0xA7: { const t = rd32(pc + 2) >>> 0; W(code[pc + 1], t === FN_UNRESOLVED ? NIL : ((t << 4) | 3), 0); pc += 6; break; }
        case 0xA8: case 0xA9: {                                                                          // mul26lo / mul26hi
          const a = code[pc + 2], b = code[pc + 3];
          sar64(RLO(a), RHI(a), 1); const xl = RL, xh = RH;
          sar64(RLO(b), RHI(b), 1); const yl = RL, yh = RH;
          const p = toBig(xl, xh) * toBig(yl, yh);
          fromBig(op === 0xA8 ? ((p & 0x3FFFFFFn) << 1n) : (((p >> 26n) & 0xFFFFFFFFFFFFFFFFn) << 1n));
          WR(code[pc + 1]); pc += 4; break;
        }
        case 0xAA: case 0xAB: {                                                                          // mul64lo / mul64hi (raw unsigned)
          const a = code[pc + 2], b = code[pc + 3];
          const p = BigInt.asUintN(64, toBig(RLO(a), RHI(a))) * BigInt.asUintN(64, toBig(RLO(b), RHI(b)));
          fromBig(op === 0xAA ? p : (p >> 64n)); WR(code[pc + 1]); pc += 4; break;
        }
        case 0xAC: {                                                                                     // acc128 Vaddr Vlo Vhi
          const x = code[pc + 1], a = RLO(x), l = code[pc + 2], h = code[pc + 3];
          if (RHI(x) !== 0 || a < VBASE || a >= heapEnd) this.memFault('acc128');
          const i = (a - VBASE) >> 2;
          const cur = BigInt.asUintN(64, toBig(m32[i], m32[i + 1])) | (BigInt.asUintN(64, toBig(m32[i + 2], m32[i + 3])) << 64n);
          const add = BigInt.asUintN(64, toBig(RLO(l), RHI(l))) | (BigInt.asUintN(64, toBig(RLO(h), RHI(h))) << 64n);
          const r = BigInt.asUintN(128, cur + add);
          fromBig(r & 0xFFFFFFFFFFFFFFFFn); m32[i] = RL; m32[i + 1] = RH;
          fromBig(r >> 64n); m32[i + 2] = RL; m32[i + 3] = RH;
          pc += 4; break;
        }
        case 0xAD: case 0xAE: case 0xAF: {                                                               // mul/add/sub-checked
          const vd = code[pc + 1], a = code[pc + 2], b = code[pc + 3];
          const al = RLO(a), ah = RHI(a), bl = RLO(b), bh = RHI(b);
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
          if (!ovf || !gen) WR(vd);
          else { this.callLisp(gen, [[al, ah], [bl, bh]]); WR(vd); }
          pc += 4; break;
        }
        case 0xB0: {                                                                                     // sap-new
          const s = code[pc + 2];
          const base = this.bump(16); this.st64(base, 0x116, 0); this.st64(base + 8, RLO(s), RHI(s)); this.markStart(base);
          W(code[pc + 1], base | 9, 0); pc += 3; break;
        }
        case 0xB1: case 0xB2: case 0xB3: {
          const s = code[pc + 2], o = code[pc + 3];
          sar64(RLO(o), RHI(o), 1);
          const a = this.ldlo(RLO(s) - 9 + 8) + RL;
          if (a < VBASE || a >= heapEnd) this.memFault('sap-ref');
          if (op === 0xB1) W(code[pc + 1], m8[a - VBASE] << 1, 0);
          else if (op === 0xB2) { fromNum(this.dv.getUint32(a - VBASE, true) * 2); WR(code[pc + 1]); }
          else { const i = (a - VBASE) >> 2; W(code[pc + 1], m32[i], m32[i + 1]); }
          pc += 4; break;
        }
        case 0xB4: case 0xB5: case 0xB6: {
          const s = code[pc + 1], o = code[pc + 2], v = code[pc + 3];
          sar64(RLO(o), RHI(o), 1);
          const a = this.ldlo(RLO(s) - 9 + 8) + RL;
          if (a < VBASE || a >= heapEnd) this.memFault('sap-set');
          if (op === 0xB4) m8[a - VBASE] = (RLO(v) >> 1) & 0xFF;
          else if (op === 0xB5) { sar64(RLO(v), RHI(v), 1); this.dv.setInt32(a - VBASE, RL, true); }
          else { const i = (a - VBASE) >> 2; m32[i] = RLO(v); m32[i + 1] = RHI(v); }
          pc += 4; break;
        }
        case 0xB7: { const s = code[pc + 2]; const a = RLO(s) - 9 + 8; shl64(this.ldlo(a), this.ldhi(a), 1); WR(code[pc + 1]); pc += 3; break; }
        case 0xB8: this.st64(A_MVCOUNT, code[pc + 1] << 1, 0); pc += 2; break;
        case 0xB9: { const c = code[pc + 2]; W(code[pc + 1], this.allocObj(toNum(RLO(c), RHI(c)), 0x31, false), 0); pc += 3; break; }
        case 0xBA: { const s = code[pc + 1]; this.st64(A_CENV, RLO(s), RHI(s)); pc += 2; break; }
        case 0xBB: W(code[pc + 1], this.ldlo(A_CENV), this.ldhi(A_CENV)); pc += 2; break;
        case 0xBC: this.st32(A_NARGS, code[pc + 1]); pc += 2; break;
        case 0xBD: W(code[pc + 1], this.ld32(A_NARGS) << 1, 0); pc += 2; break;
        case 0xBE: case 0xBF: case 0xC0: case 0xC1: {
          const x = this.floatVal(RLO(code[pc + 2])), y = this.floatVal(RLO(code[pc + 3]));
          const r = op === 0xBE ? x + y : op === 0xBF ? x - y : op === 0xC0 ? x * y : x / y;
          W(code[pc + 1], this.allocFloat(r), 0); pc += 4; break;
        }
        case 0xC2: { const s = code[pc + 2]; sar64(RLO(s), RHI(s), 1); W(code[pc + 1], this.allocFloat(toNum(RL, RH)), 0); pc += 3; break; }
        case 0xC3: {                                                                                     // ftoi (cvttsd2si)
          const d = this.floatVal(RLO(code[pc + 2]));
          const t = Math.trunc(d);
          if (Number.isFinite(t) && t >= -9223372036854775808 && t < 9223372036854775808) {
            if (Math.abs(t) < 4503599627370496) fromNum(t * 2); else fromBig(BigInt(t) << 1n);
          } else { RL = 0; RH = 0; }
          WR(code[pc + 1]); pc += 3; break;
        }
        case 0xC4: { const x = this.floatVal(RLO(code[pc + 1])), y = this.floatVal(RLO(code[pc + 2])); this.cmp = x < y ? -1 : x > y ? 1 : 0; pc += 3; break; }
        case 0xC5: case 0xC6: {
          const a = code[pc + 2], b = code[pc + 3], al = RLO(a), ah = RHI(a), bl = RLO(b), bh = RHI(b);
          if (op === 0xC5) { add64(al, ah, bl, bh); this.ovf = ((ah ^ RH) & (bh ^ RH)) < 0; }
          else { sub64(al, ah, bl, bh); this.ovf = ((ah ^ bh) & (ah ^ RH)) < 0; }
          WR(code[pc + 1]); pc += 4; break;
        }
        case 0xC7: pc = this.ovf ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0xC8: {                                                                                     // alloc-u8 (tagged count)
          const c = code[pc + 2]; sar64(RLO(c), RHI(c), 1); const n = toNum(RL, RH), total = align16(16 + n);
          const base = this.bump(total); this.zero(base + 8, base + total);
          this.st64(base, ((n << 8) | 0x11) | 0, Math.floor(n / 16777216) | 0); this.markStart(base);
          W(code[pc + 1], base | 9, 0); pc += 3; break;
        }
        case 0xC9: { const arr = code[pc + 2], x = code[pc + 3]; const a = RLO(arr) + (RLO(x) >> 1) + 7;
                     if (RHI(arr) !== 0 || a < VBASE || a >= heapEnd) this.memFault('u8-ref');
                     W(code[pc + 1], m8[a - VBASE] << 1, 0); pc += 4; break; }
        case 0xCA: { const arr = code[pc + 1], x = code[pc + 2]; const a = RLO(arr) + (RLO(x) >> 1) + 7;
                     if (RHI(arr) !== 0 || a < VBASE || a >= heapEnd) this.memFault('u8-set');
                     m8[a - VBASE] = (RLO(code[pc + 3]) >> 1) & 0xFF; pc += 4; break; }
        default: this.fault(`unknown opcode 0x${op.toString(16)}`);
      }
    }
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
