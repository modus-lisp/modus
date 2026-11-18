#!/usr/bin/env python3
"""qemu-sample.py PORT OUT [PERIOD_MS] [ARCH] [PID] [DELAY_S] -- a sampling profiler for a
program under QEMU's gdb stub (qemu-m68k-static -g PORT ...; qemu-system-*
-gdb tcp::PORT -S works the same).  No gdb and no ptrace: it speaks the
remote protocol itself.  Every PERIOD_MS of host time it interrupts the
target, reads PC and the frame pointer, walks the frame chain (68k: LINK A6
frames, [A6] = caller's A6, [A6+4] = return address; x64: RBP), writes
"T PC RET1 RET2 ..." to OUT and continues, until the program exits.

qemu-USER's stub does not read the socket while the guest runs, so the
interrupt byte is never seen: with PID (the qemu-user process) each sample
is a SIGUSR1 sent to it instead (SIGWINCH is ignored, so never stops it) -- the guest stops at the signal, and 'c'
resumes it without delivering it.  qemu-system honours the interrupt byte.

The line format is scripts/period-lisps/lisp.lua's PROF format, so
scripts/mac68k-prof/native.py and tree.native.py read it (no BASE line:
a hosted image runs where it is linked).

Why: the compiler's cost is the same on every machine relative to the code
it generates, so the hosted 68k image under qemu-user profiles what the Mac
runs, at hundreds of times MAME's speed."""
import socket
import struct
import sys
import time

port, out = int(sys.argv[1]), sys.argv[2]
period = float(sys.argv[3]) / 1000 if len(sys.argv) > 3 else 0.005
arch = sys.argv[4] if len(sys.argv) > 4 else 'm68k'
pid = int(sys.argv[5]) if len(sys.argv) > 5 else None
# DELAY_S: run that long unsampled first (boot), at full speed
delay = float(sys.argv[6]) if len(sys.argv) > 6 else 0.0
import os, signal

s = None
for _ in range(100):
    try:
        s = socket.create_connection(('127.0.0.1', port))
        break
    except OSError:
        time.sleep(0.1)
if s is None:
    sys.exit('qemu-sample: no stub on port %d' % port)
s.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
buf = b''


def recv_packet():
    """The next $...#xx packet's payload (acks '+' skipped)."""
    global buf
    while True:
        i = buf.find(b'$')
        if i >= 0:
            j = buf.find(b'#', i)
            if j >= 0 and len(buf) >= j + 3:
                p = buf[i + 1:j]
                buf = buf[j + 3:]
                s.sendall(b'+')
                return p
        d = s.recv(65536)
        if not d:
            return None
        buf += d


def send(cmd):
    c = cmd.encode()
    s.sendall(b'$' + c + b'#' + ('%02x' % (sum(c) & 0xFF)).encode())


def command(cmd):
    """CMD's reply, skipping any stop reply still in flight."""
    send(cmd)
    while True:
        r = recv_packet()
        if r is None or r[:1] not in (b'T', b'S') or cmd[:1] in ('?',):
            return r


def mem(addr, n):
    r = command('m%x,%x' % (addr, n))
    if not r or r.startswith(b'E'):
        return None
    return bytes.fromhex(r.decode())


f = open(out, 'w')
# (qemu-user 7.2 has no QStartNoAckMode, and resends a stop reply until it
# is acknowledged: COMMAND skips the duplicates)
n = 0
t0 = time.time()
first = True
while True:
    send('c')
    time.sleep(delay if first else period)
    first = False
    if pid:
        try:
            os.kill(pid, signal.SIGUSR1)
        except ProcessLookupError:
            break
    else:
        s.sendall(b'\x03')
    r = recv_packet()
    if r is None or r[:1] in (b'W', b'X'):
        break
    regs = command('g')
    if not regs:
        break
    raw = bytes.fromhex(regs.decode())
    if arch == 'm68k':
        w = struct.unpack('>18I', raw[:72])
        pc, fp, lim, step, fmt = w[17], w[14], 0x80000000, 4, '>I'
    else:
        w = struct.unpack('<17Q', raw[:136])
        pc, fp, lim, step, fmt = w[16], w[6], 1 << 47, 8, '<Q'
    chain = []
    for _ in range(64):
        if fp == 0 or fp % 2 or fp >= lim:
            break
        m = mem(fp, 2 * step)
        if not m:
            break
        nxt, ret = struct.unpack(fmt, m[:step])[0], struct.unpack(fmt, m[step:])[0]
        chain.append(ret)
        if nxt <= fp:
            break
        fp = nxt
    f.write('%.4f %X %s\n' % (time.time() - t0, pc, ' '.join('%X' % a for a in chain)))
    n += 1
f.close()
print('qemu-sample: %d samples in %.1f s' % (n, time.time() - t0))
