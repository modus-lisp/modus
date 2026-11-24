'use strict';
// host-browser.js — host interface for running the interpreter inside a Web
// Worker.  Console output goes to the page via postMessage; stdin is a ring
// buffer in a SharedArrayBuffer the page fills, and the worker blocks on it
// with Atomics.wait (so the page must be served cross-origin isolated).
// Files live in an in-memory filesystem seeded by the page.

const ENOENT = -2, EBADF = -9, EEXIST = -17, ENOTDIR = -20, EISDIR = -21, EINVAL = -22, EACCES = -13;

// stdin ring: Int32Array header [head, tail, closed] then bytes
const RING_HDR = 16;

// http SAB: Int32 header [state, length] then bytes.  state: 0 idle, 1 request
// posted, 2 response ready.  inbox SAB (files dropped on the page):
// Int32 header [seq, length] then records [nameLen u32][name][dataLen u32][data].
class BrowserHost {
  constructor(stdinSab, post, httpSab, inboxSab, querySab, guiSab) {
    this.ctl = new Int32Array(stdinSab, 0, 4);
    this.ring = new Uint8Array(stdinSab, RING_HDR);
    this.httpCtl = httpSab ? new Int32Array(httpSab, 0, 4) : null;
    this.httpBuf = httpSab ? new Uint8Array(httpSab, 16) : null;
    this.inboxCtl = inboxSab ? new Int32Array(inboxSab, 0, 4) : null;
    this.inboxBuf = inboxSab ? new Uint8Array(inboxSab, 16) : null;
    // introspection RPC: [state, id, reqLen] Int32 header then request bytes
    // ([op:u8][arg...]).  Serviced while blocked in read(0); the answer is a
    // structured-clone message, not a SAB.
    this.queryCtl = querySab ? new Int32Array(querySab, 0, 4) : null;
    this.queryBuf = querySab ? new Uint8Array(querySab, 16) : null;
    this.vm = null; this.webRpc = undefined;
    // GUI event ring: [head, tail, closed] Int32 header, then bytes.  The page
    // pushes event lines; Lisp drains them via guiPoll / blocks via guiWait.
    this.guiCtl = guiSab ? new Int32Array(guiSab, 0, 4) : null;
    this.guiRing = guiSab ? new Uint8Array(guiSab, 16) : null;
    this.inboxSeen = 0;
    this.post = post;
    this.files = new Map();          // path -> { data: Uint8Array, mtime }
    this.dirs = new Set(['/', '/tmp', '/home', '/home/web']);
    this.fds = new Map();
    this.nextFd = 3;
    this.outBuf = [];
    this.outLen = 0;
  }
  log(s) { this.post({ type: 'log', text: s }); }
  // Ask the page to fetch; block until the response bytes are in the SAB.
  httpRequest(url, method, headers, body) {
    if (!this.httpCtl) throw new Error('no http channel');
    this.flush();
    Atomics.store(this.httpCtl, 0, 1);
    this.post({ type: 'http', url, method, headers, body });
    while (Atomics.load(this.httpCtl, 0) !== 2) Atomics.wait(this.httpCtl, 0, 1);
    const n = Atomics.load(this.httpCtl, 1);
    const out = this.httpBuf.slice(0, n);
    Atomics.store(this.httpCtl, 0, 0);
    return out;
  }
  // Files the page dropped into the inbox since we last looked.
  drainInbox() {
    if (!this.inboxCtl) return;
    const seq = Atomics.load(this.inboxCtl, 0);
    if (seq === this.inboxSeen) return;
    const len = Atomics.load(this.inboxCtl, 1);
    const dv = new DataView(this.inboxBuf.buffer, this.inboxBuf.byteOffset, len);
    let p = 0;
    while (p + 8 <= len) {
      const nl = dv.getUint32(p, true); p += 4;
      const name = new TextDecoder().decode(this.inboxBuf.slice(p, p + nl)); p += nl;
      const dl = dv.getUint32(p, true); p += 4;
      this.addFile(name, this.inboxBuf.slice(p, p + dl)); p += dl;
      this.log(`[file: ${name}, ${dl} bytes]`);
    }
    this.inboxSeen = seq;
    Atomics.store(this.inboxCtl, 1, 0);
    Atomics.store(this.inboxCtl, 2, 1);          // tell the page it may write again
    Atomics.notify(this.inboxCtl, 2);
  }
  // Run the baked %web-rpc for a page request, re-entrantly on the idle VM
  // (same mechanism as callLisp for checked arithmetic).  Request bytes are
  // copied into the image's fixed request buffer; the response is read back
  // from its fixed result buffer and posted to the page.
  serviceQuery() {
    if (!this.queryCtl || !this.vm) return;
    if (Atomics.load(this.queryCtl, 0) !== 1) return;
    const X = self.MVM_EXPORTS, VBASE = X.VBASE;
    const WEB_REQ = 0x10016000, WEB_RES = 0x10017000;
    const id = Atomics.load(this.queryCtl, 1);
    const reqLen = Atomics.load(this.queryCtl, 2);
    const vm = this.vm, m8 = vm.m8;
    m8.set(this.queryBuf.subarray(0, reqLen), WEB_REQ - VBASE);   // [op][arg...]
    m8[WEB_REQ - VBASE + reqLen] = 0;                             // NUL-terminate arg
    if (this.webRpc === undefined) this.webRpc = vm.byName.get('%WEB-RPC') || null;
    let bytes = new Uint8Array(0), error = null;
    if (!this.webRpc) error = 'no %WEB-RPC in image';
    else {
      // Snapshot the machine registers: a fault inside the query must not
      // corrupt the REPL read we are suspended in.
      const sEsp = vm.esp, sEbp = vm.ebp, sPc = vm.pc, sVrl = vm.vrl, sVrh = vm.vrh;
      try {
        vm.callLisp(this.webRpc, []);
        const len = vm.ldlo(WEB_RES) >>> 1;   // :u64 stored the tagged word (cur<<1)
        const n = Math.min(len, 30000);
        bytes = m8.slice((WEB_RES + 8) - VBASE, (WEB_RES + 8) - VBASE + n);
      } catch (e) {
        error = String(e && e.message || e);
        vm.esp = sEsp; vm.ebp = sEbp; vm.pc = sPc; vm.vrl = sVrl; vm.vrh = sVrh;
      }
    }
    Atomics.store(this.queryCtl, 0, 0);
    Atomics.notify(this.queryCtl, 0);
    this.post({ type: 'query-result', id, bytes, error }, error ? [] : [bytes.buffer]);
  }
  now() { return performance.now(); }
  // Lisp -> page: forward a batch of GUI command bytes, plus a snapshot of the
  // binary float scratch (0x10015000, VBASE 0x10000000 -> m8 offset 0x15000).  A
  // `gldrawlist ... f OFF` command carries a matrix as raw IEEE-754 doubles at
  // OFF in that buffer, so the page reinterprets bytes as a Float64Array with no
  // text formatting/parsing on either side.  Snapshotting here (not on the page)
  // is essential: linear memory is reused the instant this call returns.
  guiSend(m8, off, len) {
    this.post({ type: 'gui', bytes: m8.slice(off, off + len), floats: m8.slice(0x15000, 0x16000).buffer });
  }
  // page -> Lisp: copy queued event bytes into image memory, return the count.
  guiPoll(m8, off, max) {
    if (!this.guiCtl) return 0;
    const head = Atomics.load(this.guiCtl, 0), tail = Atomics.load(this.guiCtl, 1), cap = this.guiRing.length;
    let n = 0, h = head;
    while (n < max && h !== tail) { m8[off + n++] = this.guiRing[h]; h = (h + 1) % cap; }
    Atomics.store(this.guiCtl, 0, h);
    return n;
  }
  // Block up to MS for an event (efficient game loop / event wait).
  guiWait(ms) {
    if (!this.guiCtl) return;
    const tail = Atomics.load(this.guiCtl, 1);
    if (Atomics.load(this.guiCtl, 0) === tail) Atomics.wait(this.guiCtl, 1, tail, ms > 0 ? ms : 1000);
  }
  getpid() { return 4242; }

  flush() {
    if (this.outLen === 0) return;
    const all = new Uint8Array(this.outLen);
    let p = 0; for (const b of this.outBuf) { all.set(b, p); p += b.length; }
    this.outBuf = []; this.outLen = 0;
    this.post({ type: 'stdout', bytes: all }, [all.buffer]);
  }
  writeByte(fd, b) { this.write(fd, Uint8Array.of(b), 0, 1); }
  readByte(fd) { const b = new Uint8Array(1); return this.read(fd, b, 0, 1) === 1 ? b[0] : -1; }

  write(fd, m8, off, len) {
    if (fd === 1 || fd === 2) {
      this.outBuf.push(m8.slice(off, off + len)); this.outLen += len;
      if (this.outLen > 4096 || m8[off + len - 1] === 10) this.flush();
      return len;
    }
    const f = this.fds.get(fd);
    if (!f || f.dir) return EBADF;
    const file = this.files.get(f.path);
    const end = f.pos + len;
    if (end > file.data.length) {
      const nd = new Uint8Array(Math.max(end, file.data.length * 2));
      nd.set(file.data); file.data = nd.subarray(0, end);
      // keep the buffer, but track logical length
      file.data = nd; file.len = end;
    }
    file.data.set(m8.subarray(off, off + len), f.pos);
    file.len = Math.max(file.len, end);
    file.mtime = (Date.now() / 1000) | 0;
    f.pos = end; f.wrote = true;
    return len;
  }
  read(fd, m8, off, len) {
    if (fd === 0) {
      this.flush();
      for (;;) {
        this.drainInbox();
        this.serviceQuery();
        const head = Atomics.load(this.ctl, 0), tail = Atomics.load(this.ctl, 1);
        if (head !== tail) {
          let n = 0;
          let h = head;
          while (n < len && h !== tail) { m8[off + n++] = this.ring[h]; h = (h + 1) % this.ring.length; }
          Atomics.store(this.ctl, 0, h);
          Atomics.notify(this.ctl, 0);
          return n;
        }
        if (Atomics.load(this.ctl, 2)) return 0;         // closed
        this.post({ type: 'waiting' });
        Atomics.wait(this.ctl, 1, tail);
      }
    }
    const f = this.fds.get(fd);
    if (!f || f.dir) return EBADF;
    const file = this.files.get(f.path);
    const n = Math.max(0, Math.min(len, file.len - f.pos));
    m8.set(file.data.subarray(f.pos, f.pos + n), off);
    f.pos += n;
    return n;
  }
  norm(path) {
    if (!path.startsWith('/')) path = '/home/web/' + path;
    const parts = [];
    for (const seg of path.split('/')) {
      if (seg === '' || seg === '.') continue;
      if (seg === '..') parts.pop(); else parts.push(seg);
    }
    return '/' + parts.join('/');
  }
  addFile(path, bytes) {
    path = this.norm(path);
    this.files.set(path, { data: bytes, len: bytes.length, mtime: (Date.now() / 1000) | 0 });
    let d = path; while ((d = d.slice(0, d.lastIndexOf('/'))) !== '') this.dirs.add(d);
  }
  open(path, flags, mode) {
    path = this.norm(path);
    const acc = flags & 3;
    if (this.dirs.has(path)) {
      if (acc !== 0) return EISDIR;
      const fd = this.nextFd++; this.fds.set(fd, { path, pos: 0, dir: true }); return fd;
    }
    let file = this.files.get(path);
    if (!file) {
      if (!(flags & 0x40)) return ENOENT;
      const parent = path.slice(0, path.lastIndexOf('/')) || '/';
      if (!this.dirs.has(parent)) return ENOENT;
      file = { data: new Uint8Array(256), len: 0, mtime: (Date.now() / 1000) | 0 };
      this.files.set(path, file);
    } else if ((flags & 0x40) && (flags & 0x80)) return EEXIST;
    if (flags & 0x200) file.len = 0;
    const fd = this.nextFd++;
    this.fds.set(fd, { path, pos: (flags & 0x400) ? file.len : 0 });
    return fd;
  }
  close(fd) {
    const f = this.fds.get(fd);
    if (!f) return EBADF;
    this.fds.delete(fd);
    if (f.wrote) { const file = this.files.get(f.path); if (file) this.post({ type: 'file', path: f.path, bytes: file.data.slice(0, file.len) }); }
    return 0;
  }
  lseek(fd, off, whence) {
    const f = this.fds.get(fd); if (!f) return EBADF;
    const len = f.dir ? 0 : this.files.get(f.path).len;
    if (whence === 0) f.pos = off; else if (whence === 1) f.pos += off; else if (whence === 2) f.pos = len + off; else return EINVAL;
    return f.pos;
  }
  unlink(path) { path = this.norm(path); if (!this.files.delete(path)) return ENOENT; return 0; }
  rename(a, b) {
    a = this.norm(a); b = this.norm(b);
    const f = this.files.get(a); if (!f) return ENOENT;
    this.files.delete(a); this.files.set(b, f); return 0;
  }
  mkdir(path) { path = this.norm(path); if (this.dirs.has(path) || this.files.has(path)) return EEXIST; this.dirs.add(path); return 0; }
  access(path) { path = this.norm(path); return (this.files.has(path) || this.dirs.has(path)) ? 0 : ENOENT; }
  stat(path) {
    path = this.norm(path);
    const f = this.files.get(path);
    if (f) return { size: f.len, mtime: f.mtime };
    if (this.dirs.has(path)) return { size: 4096, mtime: 0 };
    return ENOENT;
  }
  fstat(fd) {
    const f = this.fds.get(fd); if (!f) return EBADF;
    if (f.dir) return { size: 4096, mtime: 0 };
    const file = this.files.get(f.path); return { size: file.len, mtime: file.mtime };
  }
  getdents(fd) {
    const f = this.fds.get(fd); if (!f) return EBADF;
    if (!f.dir) return ENOTDIR;
    if (f.listed) return [];
    f.listed = true;
    const out = [{ name: '.', ino: 1, type: 4 }, { name: '..', ino: 1, type: 4 }];
    const prefix = f.path === '/' ? '/' : f.path + '/';
    let ino = 2;
    for (const p of this.files.keys()) if (p.startsWith(prefix) && !p.slice(prefix.length).includes('/')) out.push({ name: p.slice(prefix.length), ino: ino++, type: 8 });
    for (const d of this.dirs) if (d !== f.path && d.startsWith(prefix) && !d.slice(prefix.length).includes('/')) out.push({ name: d.slice(prefix.length), ino: ino++, type: 4 });
    return out;
  }
  getdentsConsumed() {}
}

if (typeof module !== 'undefined' && module.exports) module.exports = { BrowserHost, RING_HDR };
else if (typeof self !== 'undefined') { self.BrowserHost = BrowserHost; self.RING_HDR = RING_HDR; }
