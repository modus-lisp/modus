'use strict';
// mvm.js — a JavaScript interpreter for Modus MVM bytecode.
//
// It models the HOSTED LINUX i386 machine that mvm/translate-i386.lisp +
// boot/boot-linux-i386.lisp produce: 32-bit words, tagged values
// (fixnum = v<<1, cons = ptr|1, function = addr|3, immediate = ..5,
// object = ptr|9, forward = ..F), a flat linear memory holding the BSS block
// at 0x10000000, a machine stack, and a Cheney semispace heap.  All the
// absolute addresses the compiled runtime hard-codes are honoured; the JS
// side owns the garbage collector, the trap table (console, syscalls,
// setjmp/longjmp handler stack, &rest argument copy) and the host I/O.
//
// The module is produced by mvm/build-web.lisp (format documented there).

const NIL = 0xDEAD0001 | 0;
const TV  = 0xDEAD1009 | 0;

// ---- virtual memory layout ------------------------------------------------
const VBASE      = 0x10000000;            // lowest virtual address we model
const BSS_END    = 0x10020000;
const POOL_ADDR  = 0x10020000;            // constant pool (strings) lives here
const STACK_ADDR = 0x10800000;
const STACK_SIZE = 0x00800000;            // 8 MB, as boot-linux-i386
const HEAP_ADDR  = 0x11000000;
const ALLOC_START_OFF = 0x200;
const GUARD      = 0x01000000;            // 16 MB overshoot guard

// BSS words the compiled runtime and the boot stub agree on.
const A_GC_FROM   = 0x10000040, A_GC_TO = 0x10000048, A_GC_SIZE = 0x10000050,
      A_GC_STACKB = 0x10000058, A_GC_COUNT = 0x10000060;
const A_MVCOUNT   = 0x10000090;
const A_CODE_BASE = 0x10000160, A_CODE_END = 0x10000168;
const A_JMPBUF    = 0x10000180;           // 6 words: esp ebp ip V4 V0 V1
const A_ARGC      = 0x10000200, A_ARGV1 = 0x10000208, A_ARGV2 = 0x10000248;
const A_HDEPTH    = 0x10000400, A_HSTACK = 0x10000408, HMAX = 63, JMPBUF_WORDS = 6;
const A_HCAP      = 0x100003F0, A_HOVF = 0x100003F8;
const A_GLOB      = 0x10000A00;           // i386 global slot block
const A_VA = A_GLOB, A_VL = A_GLOB + 4, A_VN = A_GLOB + 8, A_NARGS = A_GLOB + 12,
      A_CENV = A_GLOB + 16, A_PAGEBASE = A_GLOB + 0x18;
const A_ARGV_PTRS = 0x10009000, A_ARGV_ARENA = 0x1000A000, A_ARENA_END = 0x1001E000;
const A_MCGC_PAGEBASE = 0x10000E00;

// Frame layout (byte offsets from EBP), mirroring translate-i386.lisp.
const FRAME_SIZE = 296;
const SLOT_BASE  = -68;                   // frame slot N at EBP-68-4N
const MAX_SLOTS  = (FRAME_SIZE - 68) / 4; // 57
// vreg -> EBP offset.  V0/V1/V4 use the callee-save slots (on native they are
// the physical ESI/EDI/EBX; here every register is memory so a frame owns its
// own copy and "callee-saved" falls out for free).
const ROFF = new Int32Array(16);
ROFF[0] = -4; ROFF[1] = -8; ROFF[4] = -12; ROFF[2] = -16; ROFF[3] = -20;
for (let v = 5; v <= 15; v++) ROFF[v] = -24 - 4 * (v - 5);

const RET_SENTINEL = -1;
const FN_UNRESOLVED = 0xFFFFFFF0;

class LongJmp { constructor(esp) { this.esp = esp; } }
class MvmExit { constructor(code) { this.code = code; } }
class MvmFault extends Error {}

function align16(n) { return (n + 15) & ~15; }

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
  if (version !== 1 || wordSize !== 4) throw new Error('unsupported MVMW version/word size');
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
    this.code = mod.code;
    this.trace = opts.trace || 0;
    this.maxSteps = opts.maxSteps || 0;
    this.prof = opts.profile ? new Map() : null;
    this.debug = !!opts.debug;
    this.semi = opts.semispace || (128 << 20);
    this.stackTop = STACK_ADDR + STACK_SIZE - 16;
    this.heapBase = HEAP_ADDR;
    this.heapEnd = HEAP_ADDR + 2 * this.semi + GUARD;
    const size = this.heapEnd - VBASE;
    this.buf = new ArrayBuffer(size);
    this.m8 = new Uint8Array(this.buf);
    this.m16 = new Uint16Array(this.buf);
    this.m32 = new Int32Array(this.buf);
    this.dv = new DataView(this.buf);
    this.f64 = new Float64Array(8);            // scratch for float ops
    this.f64u16 = new Uint16Array(this.f64.buffer);
    // object-start + cons-kind bitmaps, one bit per 16-byte granule
    const granules = (this.heapEnd - this.heapBase) >> 4;
    this.startBmp = new Uint8Array(granules >> 3);
    this.consBmp = new Uint8Array(granules >> 3);
    this.pageBase = this.heapBase + ALLOC_START_OFF;
    this.mmapNext = this.heapEnd;             // fake mmap region (not modelled)
    this.gcCount = 0;
    this.steps = 0;
    this.vr = 0; this.esp = 0; this.ebp = 0; this.pc = 0;
    this.cmp = 0; this.ovf = false;
    this.exitCode = null;
    this.snapAt = -1;                         // bytecode offset that triggers onSnapshot
    this.onSnapshot = null;
    // generic-arith slow-path entries (checked add/sub/mul overflow)
    this.genAdd = mod.byName.get('GENERIC-ADD');
    this.genSub = mod.byName.get('GENERIC-SUBTRACT');
    this.genMul = mod.byName.get('GENERIC-MULTIPLY');
    this.initMemory(opts.argv || ['modus'], opts.env || []);
  }

  // -- raw memory helpers ---------------------------------------------------
  ld32(a) { if ((a & 3) === 0) return this.m32[(a - VBASE) >> 2]; return this.dv.getInt32(a - VBASE, true); }
  st32(a, v) { if ((a & 3) === 0) this.m32[(a - VBASE) >> 2] = v; else this.dv.setInt32(a - VBASE, v, true); }
  ld16(a) { if ((a & 1) === 0) return this.m16[(a - VBASE) >> 1]; return this.dv.getUint16(a - VBASE, true); }
  st16(a, v) { if ((a & 1) === 0) this.m16[(a - VBASE) >> 1] = v; else this.dv.setUint16(a - VBASE, v, true); }
  ld8(a) { return this.m8[a - VBASE]; }
  st8(a, v) { this.m8[a - VBASE] = v; }

  cstr(a) {
    let s = '', i = a - VBASE;
    while (this.m8[i] !== 0) s += String.fromCharCode(this.m8[i++]);
    return s;
  }
  putBytes(a, bytes) { this.m8.set(bytes, a - VBASE); }

  // -- boot: what boot-linux-i386's entry stub writes -----------------------
  stageArgv(argv, env) {
    const enc = (s) => { const b = []; for (let i = 0; i < s.length; i++) b.push(s.charCodeAt(i) & 0xFF); b.push(0); return b; };
    // argc + the two fixed argv string copies
    this.st32(A_ARGC, argv.length);
    this.zero(A_ARGV1, A_ARGV1 + 128);
    if (argv.length > 1) this.putBytes(A_ARGV1, enc(argv[1]).slice(0, 63));
    if (argv.length > 2) this.putBytes(A_ARGV2, enc(argv[2]).slice(0, 63));
    // staged argv/envp pointer array + string arena
    let pp = A_ARGV_PTRS, ap = A_ARGV_ARENA;
    const stage = (s) => {
      const b = enc(s);
      if (ap + b.length >= A_ARENA_END) { this.st32(pp, 0); pp += 4; return; }
      this.putBytes(ap, b); this.st32(pp, ap); pp += 4; ap += b.length;
    };
    for (const a of argv) stage(a);
    this.st32(pp, 0); pp += 4;
    for (const e of env) stage(e);
    this.st32(pp, 0);
  }
  initMemory(argv, env) {
    this.stageArgv(argv, env);
    // constant pool
    this.putBytes(POOL_ADDR, this.mod.pool);
    if (POOL_ADDR + this.mod.pool.length > STACK_ADDR) throw new Error('constant pool too large');
    // heap + allocator globals
    const from = this.heapBase + ALLOC_START_OFF;
    const spaceSize = this.semi - ALLOC_START_OFF;
    this.st32(this.heapBase, argv.length);
    this.st32(A_VA, from);
    this.st32(A_VL, from + spaceSize);
    this.st32(A_VN, NIL);
    this.st32(A_GC_FROM, from);
    this.st32(A_GC_TO, this.heapBase + this.semi);
    this.st32(A_GC_SIZE, spaceSize);
    this.st32(A_GC_STACKB, this.stackTop);
    this.st32(A_GC_COUNT, 0);
    this.st32(A_PAGEBASE, from);
    this.st32(A_MCGC_PAGEBASE, from);
    // bitmap base words stay 0: gc.lisp's bitmap ops degrade to no-ops and the
    // real bitmaps live on the JS side.  Code bounds stay 0 too: every
    // function value carries the +3 tag, which FUNCTIONP tests first.
    this.esp = this.stackTop;
    this.ebp = this.esp;
  }

  // -- registers ------------------------------------------------------------
  reg(v) {
    if (v < 16) return this.m32[(this.ebp + ROFF[v] - VBASE) >> 2];
    switch (v) {
      case 16: return this.vr;
      case 17: return this.m32[(A_VA - VBASE) >> 2];
      case 18: return this.m32[(A_VL - VBASE) >> 2];
      case 19: return this.m32[(A_VN - VBASE) >> 2];
      case 20: return this.esp;
      case 21: return this.ebp;
      case 22: return this.pc;
    }
    throw new MvmFault('bad vreg ' + v);
  }
  setReg(v, x) {
    x |= 0;
    if (v < 16) { this.m32[(this.ebp + ROFF[v] - VBASE) >> 2] = x; return; }
    switch (v) {
      case 16: this.vr = x; return;
      case 17: this.m32[(A_VA - VBASE) >> 2] = x; return;
      case 18: this.m32[(A_VL - VBASE) >> 2] = x; return;
      case 19: this.m32[(A_VN - VBASE) >> 2] = x; return;
      case 20: this.esp = x; return;
      case 21: this.ebp = x; return;
      case 22: this.pc = x; return;
    }
    throw new MvmFault('bad vreg ' + v);
  }
  get va() { return this.m32[(A_VA - VBASE) >> 2]; }
  set va(x) { this.m32[(A_VA - VBASE) >> 2] = x; }
  get vl() { return this.m32[(A_VL - VBASE) >> 2]; }
  set vl(x) { this.m32[(A_VL - VBASE) >> 2] = x; }

  push(x) { this.esp -= 4; this.m32[(this.esp - VBASE) >> 2] = x; }
  pop() { const x = this.m32[(this.esp - VBASE) >> 2]; this.esp += 4; return x; }

  // -- diagnostics ----------------------------------------------------------
  fnAt(pc) {
    const s = this.mod.sorted;
    let lo = 0, hi = s.length - 1, best = null;
    while (lo <= hi) {
      const mid = (lo + hi) >> 1;
      if (s[mid].off <= pc) { best = s[mid]; lo = mid + 1; } else hi = mid - 1;
    }
    return best;
  }
  where(pc = this.pc) {
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
  fault(msg) {
    const e = new MvmFault(`${msg} at ${this.where()} (step ${this.steps})\n  ` + this.backtrace().join('\n  '));
    throw e;
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
  zero(from, to) { this.m8.fill(0, from - VBASE, to - VBASE); }

  allocObj(count, subtag, fill) {
    const total = align16((count + 1) * 4);
    const base = this.bump(total);
    if (fill) this.zero(base + 4, base + total);
    this.st32(base, (count << 8) | subtag);
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
  allocFloat(d) {
    const base = this.bump(32);
    this.st32(base, (4 << 8) | 0x60);
    this.f64[0] = d;
    // slot i = bits 63-16i..48-16i = little-endian halfword (3-i), tagged
    for (let i = 0; i < 4; i++) this.st32(base + 4 + 4 * i, this.f64u16[3 - i] << 1);
    this.st32(base + 20, 0); this.st32(base + 24, 0); this.st32(base + 28, 0);
    this.markStart(base);
    return base | 9;
  }
  floatVal(v) {
    if ((v & 0xF) !== 9) this.fault('float op on non-object');
    const base = v - 9;
    for (let i = 0; i < 4; i++) this.f64u16[3 - i] = (this.ld32(base + 4 + 4 * i) >> 1) & 0xFFFF;
    return this.f64[0];
  }

  // -- garbage collector: Cheney copy with conservative validated roots ------
  gc() {
    const m32 = this.m32;
    const fromStart = this.ld32(A_GC_FROM);
    const spaceSize = this.ld32(A_GC_SIZE);
    const fromEnd = fromStart + spaceSize;
    const toStart = this.ld32(A_GC_TO);
    let free = toStart;
    const self = this;
    const t0 = Date.now();
    const used = this.va - fromStart;

    function copy(v) {
      const tag = v & 0xF;
      const raw = v - tag;
      const hdr = m32[(raw - VBASE) >> 2];
      if ((hdr & 0xF) === 0xF) return (hdr & ~0xF) | tag;   // forwarded
      let size;
      if (self.isCons(raw)) size = 16;
      else {
        const subtag = hdr & 0xFF, count = hdr >>> 8;
        size = subtag === 0x11 ? align16(4 + count) : align16((count + 1) * 4);
        if (raw + size > fromEnd) return v;                 // insane header: leave it
      }
      const dst = free;
      self.m8.copyWithin(dst - VBASE, raw - VBASE, raw - VBASE + size);
      free += size;
      self.markStart(dst);
      if (tag === 1 || self.isCons(raw)) self.markCons(dst);
      m32[(raw - VBASE) >> 2] = dst | 0xF;
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
    // roots: machine stack, BSS block, VR
    for (let a = this.esp; a < this.stackTop; a += 4) scanWord((a - VBASE) >> 2);
    for (let a = VBASE; a < BSS_END; a += 4) scanWord((a - VBASE) >> 2);
    { const tag = this.vr & 0xF;
      if (tag === 1 || tag === 9) { m32[0] = this.vr; scanWord(0); this.vr = m32[0]; m32[0] = 0; } }
    // Cheney scan (flat, word by word, like the native trampoline)
    for (let a = toStart; a < free; a += 4) scanWord((a - VBASE) >> 2);
    // swap
    this.st32(A_GC_FROM, toStart);
    this.st32(A_GC_TO, fromStart);
    this.va = free;
    this.vl = toStart + spaceSize;
    // clear bitmaps over the reclaimed range
    // exactly the reclaimed semispace (its granule range is byte-aligned:
    // space_size is a multiple of 128)
    const g0 = (fromStart - this.pageBase) >> 4, g1 = (fromEnd - this.pageBase) >> 4;
    this.startBmp.fill(0, g0 >> 3, g1 >> 3);
    this.consBmp.fill(0, g0 >> 3, g1 >> 3);
    this.gcCount++;
    this.st32(A_GC_COUNT, this.gcCount);
    if (this.trace) this.host.log(`[gc #${this.gcCount}: ${used >> 10}K -> ${(free - toStart) >> 10}K, ${Date.now() - t0}ms]`);
  }

  // -- calls ----------------------------------------------------------------
  // Native i386: caller pushes V3, V2; CALL pushes return address; callee
  // prologue pushes EBP, reserves the frame, copies [EBP+8]/[EBP+12] into its
  // V2/V3 slots.  V0/V1/V4 travel in ESI/EDI/EBX, so the callee starts with
  // the caller's values in them.
  enter(target, a0, a1, a4) {
    this.push(this.ebp);
    this.ebp = this.esp;
    this.esp -= FRAME_SIZE;
    if (this.esp < STACK_ADDR + 4096) this.fault('stack overflow');
    const m32 = this.m32, b = (this.ebp - VBASE) >> 2;
    m32[b - 4] = m32[b + 2];     // V2 <- [ebp+8]
    m32[b - 5] = m32[b + 3];     // V3 <- [ebp+12]
    m32[b - 1] = a0; m32[b - 2] = a1; m32[b - 3] = a4;
    this.pc = target;
    if (this.prof) this.prof.set(target, (this.prof.get(target) || 0) + 1);
    if (target === this.snapAt) { this.snapAt = -1; this.onSnapshot(); }
  }
  doCall(target, retpc) {
    const b = (this.ebp - VBASE) >> 2, m32 = this.m32;
    const a0 = m32[b - 1], a1 = m32[b - 2], a4 = m32[b - 3];
    this.push(m32[b - 5]);       // V3
    this.push(m32[b - 4]);       // V2
    this.push(retpc);
    this.enter(target, a0, a1, a4);
  }
  doTailcall(target) {
    const b = (this.ebp - VBASE) >> 2, m32 = this.m32;
    const a0 = m32[b - 1], a1 = m32[b - 2], a4 = m32[b - 3];
    const v2 = m32[b - 4], v3 = m32[b - 5];
    this.esp = this.ebp;
    this.ebp = this.pop();
    const ret = this.pop();
    this.esp += 8;               // drop the old V2/V3
    this.push(v3); this.push(v2); this.push(ret);
    this.enter(target, a0, a1, a4);
  }
  doRet() {
    this.esp = this.ebp;
    this.ebp = this.pop();
    const ret = this.pop();
    this.esp += 8;               // caller-side V2/V3 cleanup
    this.pc = ret;
  }
  fnAddrToOffset(v) {
    if ((v & 0xF) !== 3) this.fault(`call-ind on non-function 0x${(v >>> 0).toString(16)}`);
    return (v - 3) >>> 4;
  }

  // Run a nested activation to completion and return VR.  Used by the
  // checked-arithmetic slow paths and by the host to call Lisp.
  callLisp(fn, args) {
    const savedPc = this.pc, savedVr = this.vr;
    const b = (this.ebp - VBASE) >> 2, m32 = this.m32;
    const saved = [m32[b - 1], m32[b - 2], m32[b - 4], m32[b - 5]];
    for (let i = 0; i < 4; i++) this.setReg(i, i < args.length ? args[i] : NIL);
    this.st32(A_NARGS, args.length);
    this.doCall(fn.off, RET_SENTINEL);
    const baseEsp = this.esp;
    try { this.run(); }
    finally {
      // if we are unwinding past this activation the frame is gone anyway
      if (this.esp >= baseEsp) { /* normal return */ }
    }
    const r = this.vr;
    m32[b - 1] = saved[0]; m32[b - 2] = saved[1]; m32[b - 4] = saved[2]; m32[b - 5] = saved[3];
    this.pc = savedPc;
    this.vr = savedVr;
    return r;
  }

  // -- handler stack (setjmp / longjmp) -------------------------------------
  handlerPush() {
    const depth = this.ld32(A_HDEPTH);
    if (depth >= HMAX) {
      this.st32(A_HCAP, this.ld32(A_HCAP) + 1);
      this.st32(A_HOVF, this.ld32(A_HOVF) + 1);
      return 1;
    }
    const fr = A_HSTACK + depth * 24;
    for (let i = 0; i < JMPBUF_WORDS; i++) this.st32(fr + 4 * i, this.ld32(A_JMPBUF + 4 * i));
    this.st32(A_HDEPTH, depth + 1);
    return 0;
  }
  handlerPop() {
    const ovf = this.ld32(A_HOVF);
    if (ovf !== 0) { this.st32(A_HOVF, ovf - 1); return; }
    const depth = this.ld32(A_HDEPTH);
    if (depth === 0) {
      for (let i = 0; i < JMPBUF_WORDS; i++) this.st32(A_JMPBUF + 4 * i, 0);
      return;
    }
    const fr = A_HSTACK + (depth - 1) * 24;
    this.st32(A_HDEPTH, depth - 1);
    for (let i = 0; i < JMPBUF_WORDS; i++) this.st32(A_JMPBUF + 4 * i, this.ld32(fr + 4 * i));
  }
  setjmp(resumePc) {
    const capped = this.handlerPush();
    if (!capped) {
      const b = (this.ebp - VBASE) >> 2, m32 = this.m32;
      this.st32(A_JMPBUF, this.esp);
      this.st32(A_JMPBUF + 4, this.ebp);
      this.st32(A_JMPBUF + 8, resumePc);
      this.st32(A_JMPBUF + 12, m32[b - 3]);   // V4
      this.st32(A_JMPBUF + 16, m32[b - 1]);   // V0
      this.st32(A_JMPBUF + 20, m32[b - 2]);   // V1
    }
    this.vr = NIL;
  }
  longjmp() {
    this.st32(A_HOVF, 0);
    const esp = this.ld32(A_JMPBUF), ebp = this.ld32(A_JMPBUF + 4), ip = this.ld32(A_JMPBUF + 8);
    const v4 = this.ld32(A_JMPBUF + 12), v0 = this.ld32(A_JMPBUF + 16), v1 = this.ld32(A_JMPBUF + 20);
    if (esp === 0) this.fault('longjmp with no handler armed');
    this.handlerPop();
    this.ebp = ebp; this.esp = esp; this.pc = ip;
    const b = (ebp - VBASE) >> 2;
    this.m32[b - 3] = v4; this.m32[b - 1] = v0; this.m32[b - 2] = v1;
    this.vr = TV;
    throw new LongJmp(esp);      // unwind nested JS activations, if any
  }

  // -- traps ----------------------------------------------------------------
  trap(codeNum, nextPc) {
    const h = this.host;
    if (codeNum < 0x100) {
      // frame-enter: copy params 4..n-1 from the caller's pushes into slots
      for (let i = 4; i < codeNum; i++) this.st32(this.ebp + SLOT_BASE - 4 * i, this.ld32(this.ebp + 4 * i));
      return;
    }
    if (codeNum < 0x300) return;               // frame-alloc / frame-free
    switch (codeNum) {
      case 0x0300: h.writeByte(1, (this.reg(0) >> 1) & 0xFF); return;
      case 0x0301: { const c = h.readByte(0); this.setReg(0, (c < 0 ? 0xFF : c) << 1); return; }
      case 0x0302: case 0x0304: case 0x0321: return;
      case 0x0303: return;
      case 0x0310: this.vr = (h.now() * 1000) | 0; return;
      case 0x0320: return;
      case 0x0500: throw new MvmExit(this.reg(0) >> 1);
      case 0x0502: {
        const r = this.syscall(this.reg(0) >> 1, this.reg(1) >> 1, this.reg(2) >> 1, this.reg(3) >> 1, 0, 0, 0);
        this.setReg(0, r << 1); return;
      }
      case 0x0503: {
        const r = this.syscall(this.reg(0) >> 1, this.reg(1), this.reg(2), this.reg(3), 0, 0, 0);
        this.setReg(0, r); return;
      }
      case 0x0507: {
        const r = this.syscall(this.reg(0) >> 1, this.reg(1) >> 1, this.reg(2) >> 1, this.reg(3) >> 1,
                               this.reg(4) >> 1, this.reg(5) >> 1, this.reg(6) >> 1);
        this.setReg(0, r << 1); return;
      }
      case 0x0504: case 0x0531: {           // mmap shared / exec page: hand out virtual space
        const size = this.reg(0) >> 1;
        const a = this.mmap(size);
        this.setReg(0, a << 1); return;
      }
      case 0x0510: this.setjmp(nextPc); return;
      case 0x0511: this.longjmp(); return;
      case 0x0512: this.handlerPop(); return;
      case 0x0520: return;                    // signal handlers: nothing to install
      case 0x0530: {                          // copy-overflow-args
        let n = this.ld32(A_NARGS);
        if (n < 5) return;
        if (n > 32) n = 32;
        for (let i = 4; i < n; i++) this.st32(this.ebp + SLOT_BASE - 4 * i, this.ld32(this.ebp + 4 * i));
        return;
      }
      case 0x0533: case 0x0534: return;
      case 0x0532: this.fault('%jit-call is not supported here');
      default: this.fault(`unimplemented trap 0x${codeNum.toString(16)}`);
    }
  }
  mmap(size) {
    // The compiled runtime never dereferences these through our memory model
    // on the paths we support (scratch buffers are forced into the BSS), so a
    // growing fake region is enough.  Fault loudly if it is ever touched.
    const a = this.mmapNext;
    this.mmapNext += align16(size);
    return a;
  }

  // Linux/i386 int 0x80 numbering, the subset the hosted CLI uses.
  syscall(nr, a1, a2, a3, a4, a5, a6) {
    const h = this.host;
    switch (nr) {
      case 1: case 252: throw new MvmExit(a1);
      case 3: {                                  // read(fd, buf, count)
        if (a2 < VBASE || a2 + a3 > this.heapEnd) return -14;
        return h.read(a1, this.m8, a2 - VBASE, a3);
      }
      case 4: {                                  // write
        if (a2 < VBASE || a2 + a3 > this.heapEnd) return -14;
        return h.write(a1, this.m8, a2 - VBASE, a3);
      }
      case 5: return h.open(this.cstr(a1), a2, a3);
      case 6: return h.close(a1);
      case 10: return h.unlink(this.cstr(a1));
      case 19: return h.lseek(a1, a2, a3);
      case 20: return h.getpid();
      case 33: return h.access(this.cstr(a1), a2);
      case 38: return h.rename(this.cstr(a1), this.cstr(a2));
      case 39: return h.mkdir(this.cstr(a1), a2);
      case 90: case 192: return this.mmap(a2);
      case 195: case 197: {                      // stat64 / fstat64 -> size@44, mtime@72
        const st = nr === 195 ? h.stat(this.cstr(a1)) : h.fstat(a1);
        if (typeof st === 'number') return st;
        this.st32(a2 + 44, st.size | 0);
        this.st32(a2 + 72, st.mtime | 0);
        return 0;
      }
      case 220: {                                // getdents64(fd, buf, size)
        const r = h.getdents(a1);
        if (typeof r === 'number') return r;
        let p = a2, total = 0;
        for (const e of r) {
          const nm = e.name;
          const reclen = (19 + nm.length + 1 + 7) & ~7;
          if (total + reclen > a3) break;
          this.st32(p, e.ino | 0); this.st32(p + 4, 0);         // d_ino (64)
          this.st32(p + 8, 0); this.st32(p + 12, 0);            // d_off
          this.st16(p + 16, reclen); this.st8(p + 18, e.type);   // d_reclen, d_type
          for (let i = 0; i < nm.length; i++) this.st8(p + 19 + i, nm.charCodeAt(i) & 0xFF);
          this.st8(p + 19 + nm.length, 0);
          p += reclen; total += reclen;
        }
        h.getdentsConsumed(a1, total === 0 ? 0 : r.length);
        return total;
      }
      case 13: case 201: return (Date.now() / 1000) | 0;    // time
      case 24: case 158: return 0;               // sched_yield / arch_prctl
      case 265: case 228: {                      // clock_gettime(clk, ts)
        const ms = Date.now();
        this.st32(a2, (ms / 1000) | 0); this.st32(a2 + 4, ((ms % 1000) * 1e6) | 0);
        return 0;
      }
      default:
        h.log(`[mvm: unsupported syscall ${nr}]`);
        return -38;                              // ENOSYS
    }
  }

  // -- the interpreter loop -------------------------------------------------
  // Runs until the activation that was current on entry returns (its RET
  // pops RET_SENTINEL).  Re-entrant; a longjmp whose target frame lies above
  // this activation's base unwinds through it as a JS exception.
  run() {
    const baseEsp = this.esp;
    for (;;) {
      try {
        this.loop();
        return;
      } catch (e) {
        if (e instanceof LongJmp) {
          if (e.esp <= baseEsp) continue;      // target frame is inside us: resume here
          throw e;
        }
        throw e;
      }
    }
  }

  loop() {
    const code = this.code, m32 = this.m32, m8 = this.m8;
    const rd32 = (p) => (code[p] | (code[p + 1] << 8) | (code[p + 2] << 16) | (code[p + 3] << 24));
    const self = this;
    const R = (v) => (v < 16 ? m32[(self.ebp + ROFF[v] - VBASE) >> 2] : self.reg(v));
    const W = (v, x) => { if (v < 16) m32[(self.ebp + ROFF[v] - VBASE) >> 2] = x; else self.setReg(v, x); };
    let pc = this.pc;
    for (;;) {
      if (this.trace) {
        this.steps++;
        if (this.trace > 1) this.host.log(`${this.steps} ${this.where(pc)} op=${code[pc].toString(16)}`);
        else if (this.maxSteps && this.steps >= this.maxSteps) this.fault('step limit');
        else if (this.prof && (this.steps & 255) === 0) { const f = this.fnAt(pc); if (f) this.prof.set(-f.off - 1, (this.prof.get(-f.off - 1) || 0) + 1); }
        else if ((this.steps & 0x3FFFFFF) === 0) { const bt = this.backtrace(200); this.host.log(`[${this.steps} steps, gc ${this.gcCount}, heap ${(this.va - this.heapBase) >> 10}K, depth ${bt.length}] ${bt.slice(0, 4).join(' < ')} ... ${bt.slice(-4).join(' < ')}`); }
      }
      const op = code[pc];
      this.pc = pc;
      switch (op) {
        case 0x00: pc += 1; break;                              // nop
        case 0x01: this.fault('break');
        case 0x02: {                                            // trap imm16
          const c = code[pc + 1] | (code[pc + 2] << 8);
          this.pc = pc; this.trap(c, pc + 3);
          pc = this.pc === pc ? pc + 3 : this.pc;               // longjmp changes pc via throw
          break;
        }
        case 0x10: W(code[pc + 1], R(code[pc + 2])); pc += 3; break;          // mov
        case 0x11: W(code[pc + 1], rd32(pc + 2)); pc += 10; break;                   // li (low 32)
        case 0x12: this.push(R(code[pc + 1])); pc += 2; break;                          // push
        case 0x13: W(code[pc + 1], this.pop()); pc += 2; break;                      // pop
        case 0x14: {                                                                            // li-const
          const idx = rd32(pc + 2);
          const off = this.mod.addrTable[idx] | 0;
          W(code[pc + 1], off === 0 ? 0 : (POOL_ADDR + off));
          pc += 10; break;
        }
        case 0x20: W(code[pc + 1], R(code[pc + 2]) + R(code[pc + 3])); pc += 4; break;
        case 0x21: W(code[pc + 1], R(code[pc + 2]) - R(code[pc + 3])); pc += 4; break;
        case 0x22: W(code[pc + 1], Math.imul(R(code[pc + 2]) >> 1, R(code[pc + 3]))); pc += 4; break;
        case 0x23: {                                                                            // div
          const a = R(code[pc + 2]) >> 1, b = R(code[pc + 3]) >> 1;
          if (b === 0) this.fault('division by zero');
          W(code[pc + 1], ((a / b) | 0) << 1); pc += 4; break;
        }
        case 0x24: {                                                                            // mod (remainder)
          const a = R(code[pc + 2]) >> 1, b = R(code[pc + 3]) >> 1;
          if (b === 0) this.fault('division by zero');
          W(code[pc + 1], (a % b) << 1); pc += 4; break;
        }
        case 0x25: W(code[pc + 1], -R(code[pc + 2])); pc += 3; break;         // neg
        case 0x26: W(code[pc + 1], R(code[pc + 1]) + 2); pc += 2; break;       // inc
        case 0x27: W(code[pc + 1], R(code[pc + 1]) - 2); pc += 2; break;       // dec
        case 0x28: W(code[pc + 1], R(code[pc + 2]) & R(code[pc + 3])); pc += 4; break;
        case 0x29: W(code[pc + 1], R(code[pc + 2]) | R(code[pc + 3])); pc += 4; break;
        case 0x2A: W(code[pc + 1], R(code[pc + 2]) ^ R(code[pc + 3])); pc += 4; break;
        case 0x2B: { const n = code[pc + 3]; W(code[pc + 1], n >= 32 ? 0 : R(code[pc + 2]) << n); pc += 4; break; }
        case 0x2C: { const n = code[pc + 3]; W(code[pc + 1], n >= 32 ? 0 : R(code[pc + 2]) >>> n); pc += 4; break; }
        case 0x2D: { const n = code[pc + 3]; W(code[pc + 1], R(code[pc + 2]) >> (n >= 32 ? 31 : n)); pc += 4; break; }
        case 0x2F: W(code[pc + 1], R(code[pc + 2]) << (R(code[pc + 3]) & 31)); pc += 4; break;  // shlv
        case 0x32: W(code[pc + 1], R(code[pc + 2]) >> (R(code[pc + 3]) & 31)); pc += 4; break;  // sarv
        case 0x2E: {                                                                            // ldb pos size
          const pos = code[pc + 3], size = code[pc + 4];
          const mask = size >= 32 ? -1 : ((1 << size) - 1);
          W(code[pc + 1], (R(code[pc + 2]) >>> pos) & mask); pc += 5; break;
        }
        case 0x30: { const a = R(code[pc + 1]), b = R(code[pc + 2]); this.cmp = a < b ? -1 : a > b ? 1 : 0; pc += 3; break; }
        case 0x31: { const a = R(code[pc + 1]) & R(code[pc + 2]); this.cmp = a === 0 ? 0 : (a < 0 ? -1 : 1); pc += 3; break; }
        case 0x40: pc = pc + 5 + rd32(pc + 1); break;                                           // br
        case 0x41: pc = this.cmp === 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x42: pc = this.cmp !== 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x43: pc = this.cmp < 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x44: pc = this.cmp >= 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x45: pc = this.cmp <= 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x46: pc = this.cmp > 0 ? pc + 5 + rd32(pc + 1) : pc + 5; break;
        case 0x47: pc = R(code[pc + 1]) === m32[(A_VN - VBASE) >> 2] ? pc + 6 + rd32(pc + 2) : pc + 6; break;  // bnull
        case 0x48: pc = R(code[pc + 1]) !== m32[(A_VN - VBASE) >> 2] ? pc + 6 + rd32(pc + 2) : pc + 6; break;  // bnnull
        case 0x50: { const v = R(code[pc + 2]); if ((v & 0xF) !== 1) this.fault(`car of non-cons 0x${(v >>> 0).toString(16)}`);
                     W(code[pc + 1], this.ld32(v - 1)); pc += 3; break; }
        case 0x51: { const v = R(code[pc + 2]); if ((v & 0xF) !== 1) this.fault(`cdr of non-cons 0x${(v >>> 0).toString(16)}`);
                     W(code[pc + 1], this.ld32(v + 3)); pc += 3; break; }
        case 0x52: W(code[pc + 1], this.allocCons(R(code[pc + 2]), R(code[pc + 3]))); pc += 4; break;
        case 0x53: { const c = R(code[pc + 1]); this.st32(c - 1, R(code[pc + 2])); pc += 3; break; }   // setcar
        case 0x54: { const c = R(code[pc + 1]); this.st32(c + 3, R(code[pc + 2])); pc += 3; break; }   // setcdr
        case 0x55: { const v = R(code[pc + 2]); W(code[pc + 1], (v !== NIL && (v & 0xF) === 1) ? TV : NIL); pc += 3; break; }
        case 0x56: { const v = R(code[pc + 2]); W(code[pc + 1], (v !== NIL && (v & 0xF) === 1) ? NIL : TV); pc += 3; break; }
        case 0x60: {                                                                            // alloc-obj count subtag
          const count = code[pc + 2] | (code[pc + 3] << 8), subtag = code[pc + 4];
          W(code[pc + 1], this.allocObj(count, subtag, true)); pc += 5; break;
        }
        case 0x61: {                                                                            // obj-ref
          const vobj = code[pc + 2], idx = code[pc + 3];
          if (vobj === 21) {
            if (this.debug && idx >= MAX_SLOTS) this.fault('frame slot overflow ' + idx);
            W(code[pc + 1], this.ld32(this.ebp + SLOT_BASE - 4 * idx));
          } else W(code[pc + 1], this.ld32(R(vobj) - 9 + 4 * (idx + 1)));
          pc += 4; break;
        }
        case 0x62: {                                                                            // obj-set Vobj idx Vs
          const vobj = code[pc + 1], idx = code[pc + 2], v = R(code[pc + 3]);
          if (vobj === 21) {
            if (this.debug && idx >= MAX_SLOTS) this.fault('frame slot overflow ' + idx);
            this.st32(this.ebp + SLOT_BASE - 4 * idx, v);
          } else this.st32(R(vobj) - 9 + 4 * (idx + 1), v);
          pc += 4; break;
        }
        case 0x63: W(code[pc + 1], (R(code[pc + 2]) & 0xF) << 1); pc += 3; break;   // obj-tag
        case 0x64: {                                                                            // obj-subtag
          const v = R(code[pc + 2]);
          W(code[pc + 1], ((v & 0xF) !== 9 || v === TV) ? 0 : (this.ld32(v - 9) & 0xFF) << 1);
          pc += 3; break;
        }
        case 0x65: W(code[pc + 1], this.ld32(R(code[pc + 2]) + R(code[pc + 3]) * 2 - 5)); pc += 4; break;  // aref
        case 0x66: this.st32(R(code[pc + 1]) + R(code[pc + 2]) * 2 - 5, R(code[pc + 3])); pc += 4; break;     // aset
        case 0x67: {                                                                            // array-len
          const v = R(code[pc + 2]);
          W(code[pc + 1], ((v & 0xF) !== 9 || v === TV) ? 0 : ((this.ld32(v - 9) >>> 8) & 0xFFFFFF) << 1);
          pc += 3; break;
        }
        case 0x68: W(code[pc + 1], this.allocObj(R(code[pc + 2]), 0x32, true)); pc += 3; break;   // alloc-array (untagged count)
        case 0x70: {                                                                            // load Vd Vaddr width
          const a = R(code[pc + 2]), w = code[pc + 3] & 3;
          let v;
          if (this.debug && (a < VBASE || a >= this.heapEnd)) this.fault(`load outside memory 0x${(a >>> 0).toString(16)}`);
          if (w === 0) v = m8[a - VBASE]; else if (w === 1) v = this.ld16(a); else v = this.ld32(a);
          W(code[pc + 1], v); pc += 4; break;
        }
        case 0x71: {                                                                            // store Vaddr Vs width
          const a = R(code[pc + 1]), v = R(code[pc + 2]), w = code[pc + 3] & 3;
          if (this.debug && (a < VBASE || a >= this.heapEnd)) this.fault(`store outside memory 0x${(a >>> 0).toString(16)}`);
          if (w === 0) m8[a - VBASE] = v & 0xFF; else if (w === 1) this.st16(a, v & 0xFFFF); else this.st32(a, v);
          pc += 4; break;
        }
        case 0x72: pc += 1; break;                                                              // fence
        case 0x80: { const t = rd32(pc + 1) >>> 0; this.doCall(t, pc + 5); pc = this.pc; break; }   // call
        case 0x81: { const t = this.fnAddrToOffset(R(code[pc + 1])); this.doCall(t, pc + 2); pc = this.pc; break; }
        case 0x82: { this.doRet(); pc = this.pc; if (pc === RET_SENTINEL) return; break; }      // ret
        case 0x83: { const t = rd32(pc + 1) >>> 0; this.doTailcall(t); pc = this.pc; break; }  // tailcall
        case 0x88: { const base = this.bump(16); this.zero(base, base + 16); this.markStart(base); this.markCons(base);
                     W(code[pc + 1], base | 1); pc += 2; break; }                    // alloc-cons
        case 0x89: { if ((this.va >>> 0) >= (this.vl >>> 0)) { this.pc = pc; this.gc(); } pc += 1; break; }   // gc-check
        case 0x8A: pc += 2; break;                                                              // write-barrier
        case 0x8B: pc += 1; break;                                                              // mcgc-collect
        case 0x90: case 0x91: this.fault('save-ctx/restore-ctx not supported');
        case 0x92: pc += 1; break;                                                              // yield
        case 0x93: { const a = R(code[pc + 2]); const old = this.ld32(a); this.st32(a, R(code[pc + 3]));
                     W(code[pc + 1], old); pc += 4; break; }                         // atomic-xchg
        case 0xA0: case 0xA1: case 0xA2: case 0xA3: case 0xA4: this.fault('port I/O / halt / cli / sti not supported');
        case 0xA5: case 0xA6: this.fault('percpu ops not supported');
        case 0xA7: {                                                                            // fn-addr
          const t = rd32(pc + 2) >>> 0;
          W(code[pc + 1], t === FN_UNRESOLVED ? NIL : ((t << 4) | 3)); pc += 6; break;
        }
        case 0xA8: case 0xA9: {                                                                 // mul26lo/hi
          const a = BigInt(R(code[pc + 2]) >>> 1), b = BigInt(R(code[pc + 3]) >>> 1);
          const p = a * b;
          const r = op === 0xA8 ? Number(p & 0x3FFFFFFn) : Number((p >> 26n) & 0xFFFFFFFFn);
          W(code[pc + 1], r << 1); pc += 4; break;
        }
        case 0xAA: case 0xAB: case 0xAC: this.fault('64-bit multiply ops are not available on a 32-bit word');
        case 0xAD: case 0xAE: case 0xAF: {                                                      // mul/add/sub-checked
          const vd = code[pc + 1], a = R(code[pc + 2]), b = R(code[pc + 3]);
          let r, gen;
          if (op === 0xAE) { r = a + b; gen = this.genAdd; }
          else if (op === 0xAF) { r = a - b; gen = this.genSub; }
          else { r = (a >> 1) * b; gen = this.genMul; }
          if (r === (r | 0) || !gen) W(vd, r | 0);
          else {
            this.pc = pc;
            const res = this.callLisp(gen, [a, b]);
            W(vd, res);
          }
          pc += 4; break;
        }
        case 0xB0: {                                                                            // sap-new
          const base = this.bump(16); this.st32(base, 0x116); this.st32(base + 4, R(code[pc + 2]));
          this.st32(base + 8, 0); this.st32(base + 12, 0); this.markStart(base);
          W(code[pc + 1], base | 9); pc += 3; break;
        }
        case 0xB1: case 0xB2: case 0xB3: {
          const a = this.ld32(R(code[pc + 2]) - 5) + (R(code[pc + 3]) >> 1);
          let v = op === 0xB1 ? m8[a - VBASE] << 1 : op === 0xB2 ? this.ld32(a) << 1 : this.ld32(a);
          W(code[pc + 1], v); pc += 4; break;
        }
        case 0xB4: case 0xB5: case 0xB6: {
          const a = this.ld32(R(code[pc + 1]) - 5) + (R(code[pc + 2]) >> 1), v = R(code[pc + 3]);
          if (op === 0xB4) m8[a - VBASE] = (v >> 1) & 0xFF; else if (op === 0xB5) this.st32(a, v >> 1); else this.st32(a, v);
          pc += 4; break;
        }
        case 0xB7: W(code[pc + 1], this.ld32(R(code[pc + 2]) - 5) << 1); pc += 3; break;   // sap-addr
        case 0xB8: this.st32(A_MVCOUNT, code[pc + 1] << 1); pc += 2; break;                    // set-mv-count
        case 0xB9: W(code[pc + 1], this.allocObj(R(code[pc + 2]), 0x31, false)); pc += 3; break;   // alloc-string (untagged count)
        case 0xBA: this.st32(A_CENV, R(code[pc + 1])); pc += 2; break;                  // set-cenv
        case 0xBB: W(code[pc + 1], this.ld32(A_CENV)); pc += 2; break;               // get-cenv
        case 0xBC: this.st32(A_NARGS, code[pc + 1]); pc += 2; break;                           // set-nargs
        case 0xBD: W(code[pc + 1], this.ld32(A_NARGS) << 1); pc += 2; break;         // get-nargs
        case 0xBE: case 0xBF: case 0xC0: case 0xC1: {                                           // fadd fsub fmul fdiv
          const a = this.floatVal(R(code[pc + 2])), b = this.floatVal(R(code[pc + 3]));
          const r = op === 0xBE ? a + b : op === 0xBF ? a - b : op === 0xC0 ? a * b : a / b;
          W(code[pc + 1], this.allocFloat(r)); pc += 4; break;
        }
        case 0xC2: W(code[pc + 1], this.allocFloat(R(code[pc + 2]) >> 1)); pc += 3; break;   // itof
        case 0xC3: {                                                                            // ftoi (cvttsd2si semantics)
          const d = this.floatVal(R(code[pc + 2]));
          const t = Math.trunc(d);
          const i = (Number.isFinite(t) && t >= -2147483648 && t <= 2147483647) ? t : -2147483648;
          W(code[pc + 1], i << 1); pc += 3; break;
        }
        case 0xC4: {                                                                            // fcmp
          const a = this.floatVal(R(code[pc + 1])), b = this.floatVal(R(code[pc + 2]));
          this.cmp = a < b ? -1 : a > b ? 1 : 0;                                                // NaN: ZF=1 like UCOMISD
          pc += 3; break;
        }
        case 0xC5: case 0xC6: {                                                                 // adds / subs
          const a = R(code[pc + 2]), b = R(code[pc + 3]);
          const r = op === 0xC5 ? a + b : a - b;
          this.ovf = r !== (r | 0);
          W(code[pc + 1], r | 0); pc += 4; break;
        }
        case 0xC7: pc = this.ovf ? pc + 5 + rd32(pc + 1) : pc + 5; break;                      // bvs
        case 0xC8: {                                                                            // alloc-u8 (tagged count)
          const n = R(code[pc + 2]) >> 1, total = align16(4 + n);
          const base = this.bump(total); this.zero(base + 4, base + total);
          this.st32(base, (n << 8) | 0x11); this.markStart(base);
          W(code[pc + 1], base | 9); pc += 3; break;
        }
        case 0xC9: W(code[pc + 1], m8[R(code[pc + 2]) + (R(code[pc + 3]) >> 1) - 5 - VBASE] << 1); pc += 4; break;
        case 0xCA: m8[R(code[pc + 1]) + (R(code[pc + 2]) >> 1) - 5 - VBASE] = (R(code[pc + 3]) >> 1) & 0xFF; pc += 4; break;
        default: this.fault(`unknown opcode 0x${op.toString(16)}`);
      }
    }
  }

  // -- snapshots ------------------------------------------------------------
  // A core is the live machine at an instruction boundary: the used parts of
  // memory (BSS, constant pool, live stack, live semispace after a GC), the
  // bitmaps for the live semispace, and the JS-side registers.  Restoring one
  // skips the whole boot (which is deterministic and takes billions of steps).
  snapshot() {
    this.gc();
    const fromStart = this.ld32(A_GC_FROM);
    const ranges = [
      [VBASE, BSS_END],
      [POOL_ADDR, POOL_ADDR + this.mod.pool.length],
      [this.esp, this.stackTop],
      [fromStart, this.va],
    ];
    const g0 = (fromStart - this.pageBase) >> 4, g1 = (this.va - this.pageBase) >> 4;
    return {
      version: 1,
      semi: this.semi,
      regs: { vr: this.vr, esp: this.esp, ebp: this.ebp, pc: this.pc, cmp: this.cmp, ovf: this.ovf,
              gcCount: this.gcCount, mmapNext: this.mmapNext, steps: this.steps },
      ranges: ranges.map(([a, b]) => ({ addr: a, bytes: this.m8.slice(a - VBASE, b - VBASE) })),
      bitmaps: { g0, startBmp: this.startBmp.slice(g0 >> 3, (g1 >> 3) + 1),
                 consBmp: this.consBmp.slice(g0 >> 3, (g1 >> 3) + 1) },
    };
  }
  restore(core, argv, env) {
    if (core.semi !== this.semi) throw new Error(`core was made with a ${core.semi >> 20} MB semispace`);
    this.m8.fill(0);
    for (const r of core.ranges) this.m8.set(r.bytes, r.addr - VBASE);
    this.startBmp.fill(0); this.consBmp.fill(0);
    this.startBmp.set(core.bitmaps.startBmp, core.bitmaps.g0 >> 3);
    this.consBmp.set(core.bitmaps.consBmp, core.bitmaps.g0 >> 3);
    const r = core.regs;
    this.vr = r.vr; this.esp = r.esp; this.ebp = r.ebp; this.pc = r.pc; this.cmp = r.cmp; this.ovf = r.ovf;
    this.gcCount = r.gcCount; this.mmapNext = r.mmapNext; this.steps = r.steps;
    this.stageArgv(argv, env);
  }
  // Binary core encoding: magic, JSON header length, JSON header, then the raw
  // byte blobs in header order.
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
    out.set([0x4D, 0x56, 0x4D, 0x43], 0);                 // "MVMC"
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
      const km = this.mod.byName.get('KERNEL-MAIN');
      if (!km) throw new Error('no KERNEL-MAIN in module');
      for (let i = 0; i < 4; i++) this.setReg(i, NIL);
      this.push(NIL); this.push(NIL); this.push(RET_SENTINEL);
      this.enter(km.off, NIL, NIL, NIL);
    }
    try {
      this.run();
      return 0;
    } catch (e) {
      if (e instanceof MvmExit) return e.code;
      throw e;
    }
  }
}

const MVM_EXPORTS = { MVM, loadModule, MvmFault, MvmExit, NIL, TV, VBASE };
if (typeof module !== 'undefined' && module.exports) module.exports = MVM_EXPORTS;
else if (typeof self !== 'undefined') self.MVM_EXPORTS = MVM_EXPORTS;
