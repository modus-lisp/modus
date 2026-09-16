'use strict';
// host-node.js — the host interface for running the MVM interpreter under
// node: real file descriptors via the fs module, blocking stdin reads.

const fs = require('fs');
const { execFileSync } = require('child_process');

const ENOENT = -2, EBADF = -9, EACCES = -13, EEXIST = -17, ENOTDIR = -20, EISDIR = -21, EINVAL = -22;

function errno(e) {
  switch (e && e.code) {
    case 'ENOENT': return ENOENT;
    case 'EBADF': return EBADF;
    case 'EACCES': case 'EPERM': return EACCES;
    case 'EEXIST': return EEXIST;
    case 'ENOTDIR': return ENOTDIR;
    case 'EISDIR': return EISDIR;
    default: return EINVAL;
  }
}

class NodeHost {
  constructor() {
    this.fds = new Map();          // fd -> { fd (node), pos, dir? }
    this.nextFd = 3;
    this.dirEntries = new Map();
    this.stdoutBuf = [];
  }
  log(s) { fs.writeSync(2, s + '\n'); }
  // One HTTP request, synchronously, via curl; returns the raw response
  // (status line + headers + body) the image's HTTP/1.0 client expects.
  httpRequest(url, method, headers, body) {
    const args = ['-sS', '-i', '-L', '--max-time', '30', '-X', method];
    for (const [k, v] of Object.entries(headers)) if (!/^(host|connection|content-length)$/i.test(k)) args.push('-H', `${k}: ${v}`);
    if (body && body.length) args.push('--data-binary', body);
    args.push(url);
    return new Uint8Array(execFileSync('curl', args, { maxBuffer: 64 << 20 }));
  }
  now() { return performance.now(); }
  // GUI bridge is browser-only; no-ops under node so samples don't crash.
  guiSend() {} guiPoll() { return 0; } guiWait(ms) { return 0; }
  getpid() { return process.pid; }

  writeByte(fd, b) { this.write(fd, Uint8Array.of(b), 0, 1); }
  readByte(fd) {
    const b = new Uint8Array(1);
    const n = this.read(fd, b, 0, 1);
    return n === 1 ? b[0] : -1;
  }

  write(fd, m8, off, len) {
    try {
      if (fd === 1 || fd === 2) { fs.writeSync(fd, m8, off, len); return len; }
      const f = this.fds.get(fd);
      if (!f) return EBADF;
      const n = fs.writeSync(f.fd, m8, off, len, f.pos);
      f.pos += n;
      return n;
    } catch (e) {
      if (e.code === 'EAGAIN') return this.write(fd, m8, off, len);
      return errno(e);
    }
  }
  read(fd, m8, off, len) {
    try {
      if (fd === 0) {
        for (;;) {
          try { return fs.readSync(0, m8, off, len, null); }
          catch (e) {
            if (e.code === 'EAGAIN') { Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 5); continue; }
            if (e.code === 'EOF') return 0;
            throw e;
          }
        }
      }
      const f = this.fds.get(fd);
      if (!f) return EBADF;
      const n = fs.readSync(f.fd, m8, off, len, f.pos);
      f.pos += n;
      return n;
    } catch (e) { return errno(e); }
  }
  open(path, flags, mode) {
    // Linux O_* bits: O_WRONLY 1, O_RDWR 2, O_CREAT 0x40, O_EXCL 0x80, O_TRUNC 0x200, O_APPEND 0x400
    let f;
    const acc = flags & 3;
    if (acc === 0) f = 'r';
    else {
      f = acc === 1 ? 'w' : 'r+';
      if (flags & 0x400) f = 'a';
      else if (!(flags & 0x200) && acc === 1) f = (flags & 0x40) ? 'r+' : 'r+';
      if (flags & 0x80) f += 'x';
      if (flags & 0x40 && f === 'r+') { try { fs.closeSync(fs.openSync(path, 'a')); } catch (e) { return errno(e); } }
    }
    try {
      const st = fs.statSync(path, { throwIfNoEntry: false });
      if (st && st.isDirectory()) {
        if (acc !== 0) return EISDIR;
        const fd = this.nextFd++;
        this.fds.set(fd, { fd: -1, pos: 0, dir: path });
        return fd;
      }
      const nfd = fs.openSync(path, f, mode || 0o644);
      const fd = this.nextFd++;
      this.fds.set(fd, { fd: nfd, pos: (flags & 0x400) ? fs.fstatSync(nfd).size : 0 });
      return fd;
    } catch (e) { return errno(e); }
  }
  close(fd) {
    const f = this.fds.get(fd);
    if (!f) return EBADF;
    this.fds.delete(fd); this.dirEntries.delete(fd);
    if (f.fd >= 0) try { fs.closeSync(f.fd); } catch (e) { return errno(e); }
    return 0;
  }
  lseek(fd, off, whence) {
    const f = this.fds.get(fd);
    if (!f) return EBADF;
    if (whence === 0) f.pos = off;
    else if (whence === 1) f.pos += off;
    else if (whence === 2) f.pos = fs.fstatSync(f.fd).size + off;
    else return EINVAL;
    return f.pos;
  }
  unlink(path) { try { fs.unlinkSync(path); return 0; } catch (e) { return errno(e); } }
  rename(a, b) { try { fs.renameSync(a, b); return 0; } catch (e) { return errno(e); } }
  mkdir(path, mode) { try { fs.mkdirSync(path, mode); return 0; } catch (e) { return errno(e); } }
  access(path, mode) { try { fs.accessSync(path); return 0; } catch (e) { return errno(e); } }
  stat(path) {
    try { const s = fs.statSync(path); return { size: s.size, mtime: Math.floor(s.mtimeMs / 1000) }; }
    catch (e) { return errno(e); }
  }
  fstat(fd) {
    const f = this.fds.get(fd);
    if (!f) return EBADF;
    if (f.dir) return { size: 4096, mtime: 0 };
    try { const s = fs.fstatSync(f.fd); return { size: s.size, mtime: Math.floor(s.mtimeMs / 1000) }; }
    catch (e) { return errno(e); }
  }
  getdents(fd) {
    const f = this.fds.get(fd);
    if (!f) return EBADF;
    if (!f.dir) return ENOTDIR;
    if (!this.dirEntries.has(fd)) {
      try {
        const list = fs.readdirSync(f.dir, { withFileTypes: true }).map((d, i) => ({
          name: d.name, ino: i + 2, type: d.isDirectory() ? 4 : d.isSymbolicLink() ? 10 : 8 }));
        this.dirEntries.set(fd, [{ name: '.', ino: 1, type: 4 }, { name: '..', ino: 1, type: 4 }, ...list]);
      } catch (e) { return errno(e); }
    }
    return this.dirEntries.get(fd);
  }
  getdentsConsumed(fd, n) {
    const l = this.dirEntries.get(fd);
    if (l) this.dirEntries.set(fd, l.slice(n));
  }
}

module.exports = { NodeHost };
