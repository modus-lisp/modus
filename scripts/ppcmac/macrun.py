#!/usr/bin/env python3
"""Drive the bare PowerPC Mac image under QEMU mac99.

  macrun.py DISK SCRIPT [--log FILE]

SCRIPT is a list of steps, one per line:
  eval FORM...        type FORM at the REPL, wait for the next prompt
  load FILE           type every top-level form of FILE (one REPL line each)
  mon CMD             a QEMU monitor command (sendkey a, mouse_move 10 5, screendump F)
  sleep S
Serial port A is a unix socket; the monitor is another.  The display device is
on (-g 1024x768x32) but shown nowhere: screendump reads it.
"""
import os, re, socket, subprocess, sys, time, tempfile

def main():
    disk, script = sys.argv[1], sys.argv[2]
    log = open(sys.argv[sys.argv.index('--log') + 1], 'w') if '--log' in sys.argv else sys.stdout
    d = tempfile.mkdtemp(prefix='macrun-')
    ser, mon = os.path.join(d, 'ser'), os.path.join(d, 'mon')
    # MACRUN_MACHINE (mac99; mac99,via=pmu-adb is a PowerBook's input), MACRUN_CPU,
    # MACRUN_DEPTH (32; 8 and 16 are what a real Mac's firmware may leave), MACRUN_MEM (MB)
    machine = os.environ.get('MACRUN_MACHINE', 'mac99')
    cpu = ['-cpu', os.environ['MACRUN_CPU']] if os.environ.get('MACRUN_CPU') else []
    depth = os.environ.get('MACRUN_DEPTH', '32')
    mem = os.environ.get('MACRUN_MEM', '1024')
    q = subprocess.Popen(['qemu-system-ppc', '-M', machine] + cpu + ['-m', mem, '-g', f'1024x768x{depth}',
                          '-display', 'none', '-prom-env', 'output-device=ttya', '-prom-env', 'input-device=ttya', '-drive', f'file={disk},format=raw,media=disk,snapshot=on',
                          '-serial', f'unix:{ser},server=on,wait=on',
                          '-monitor', f'unix:{mon},server=on,wait=off'],
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    for _ in range(300):                 # 30 s: a loaded host starts QEMU slowly
        if os.path.exists(ser): break
        time.sleep(0.1)
    s = socket.socket(socket.AF_UNIX); s.connect(ser); s.settimeout(0.5)
    for _ in range(300):
        if os.path.exists(mon): break
        time.sleep(0.1)
    m = socket.socket(socket.AF_UNIX); m.connect(mon); m.settimeout(0.5)
    buf = bytearray()
    t0 = time.time()
    # MACRUN_STAMPS=FILE: every chunk from the serial port, with its time since start
    stamps = open(os.environ['MACRUN_STAMPS'], 'w') if os.environ.get('MACRUN_STAMPS') else None

    def read_until(pat, timeout):
        nonlocal buf
        end = time.time() + timeout
        start = 0                     # search only what is new (a boot can send megabytes)
        while time.time() < end:
            i = buf.find(pat, start)
            if i >= 0:
                out, buf = buf[:i + len(pat)], buf[i + len(pat):]
                return out
            start = max(0, len(buf) - len(pat) + 1)
            try:
                c = s.recv(65536)
                if not c: break
                buf += c
                if stamps:
                    stamps.write(f'{time.time() - t0:8.1f} {c.decode("latin-1")!r}\n'); stamps.flush()
            except socket.timeout:
                pass
        out, buf = buf, bytearray()
        return out + b'[TIMEOUT]'

    def read_until_any(pats, timeout):
        """Until the first of PATS (the earliest in the stream)."""
        nonlocal buf
        end = time.time() + timeout
        start = 0
        while time.time() < end:
            hits = [(buf.find(p, start), p) for p in pats]
            hits = [(i, p) for i, p in hits if i >= 0]
            if hits:
                i, p = min(hits)
                out, buf = buf[:i + len(p)], buf[i + len(p):]
                return out
            start = max(0, len(buf) - max(len(p) for p in pats) + 1)
            try:
                c = s.recv(65536)
                if not c: break
                buf += c
                if stamps:
                    stamps.write(f'{time.time() - t0:8.1f} {c.decode("latin-1")!r}\n'); stamps.flush()
            except socket.timeout:
                pass
        out, buf = buf, bytearray()
        return out + b'[TIMEOUT]'

    def emit(b):
        log.write(b.decode('latin-1').replace('\r', '')); log.flush()

    def monitor(cmd):
        try:
            while True: m.recv(65536)
        except socket.timeout: pass
        m.sendall((cmd + '\n').encode())
        time.sleep(0.3)

    # A machine whose firmware console was its screen keeps the desk; a byte on
    # the serial port hands it back to the REPL (ppcmac-desk.lisp, DESK).
    out = read_until_any([b'glass.wm: up', b'MVM-EVAL.\n> '], 3600)
    if b'glass.wm: up' not in out:
        emit(out)                                  # no desk: the REPL came first
        out = b''
    else:
        emit(out)
        out = read_until(b'MVM-EVAL.\n> ', 60)
        if b'[TIMEOUT]' in out:
            emit(out); s.sendall(b'\r'); out = read_until(b'> ', 600)
    emit(out)
    log.write(f'\n[boot {time.time() - t0:.0f}s]\n')
    for line in open(script):
        line = line.rstrip('\n')
        if not line.strip() or line.startswith('#'): continue
        op, _, arg = line.partition(' ')
        if op == 'eval':
            s.sendall(arg.encode() + b'\r')
            emit(read_until(b'\n> ', 3600))
        elif op == 'load':
            for form in forms(open(arg).read()):
                s.sendall(form.encode() + b'\r')
                out = read_until(b'\n> ', 3600)
                tail = out.decode('latin-1').replace('\r', '').strip().splitlines()
                log.write(f'[{form[:60]!r}] -> {tail[-2] if len(tail) > 1 else tail}\n'); log.flush()
        elif op == 'mon':
            monitor(arg)
        elif op == 'sleep':
            time.sleep(float(arg))
    q.kill()

def forms(text):
    """Top-level forms of TEXT, each flattened to one line (comments dropped)."""
    out, depth, cur, i, n = [], 0, [], 0, len(text)
    while i < n:
        c = text[i]
        if c == ';' and depth >= 0:
            while i < n and text[i] != '\n': i += 1
            continue
        if c == '"':
            j = i + 1
            while j < n and text[j] != '"':
                j += 2 if text[j] == '\\' else 1
            cur.append(text[i:j + 1]); i = j + 1; continue
        if c == '#' and i + 1 < n and text[i + 1] == '\\':
            cur.append(text[i:i + 3]); i += 3; continue
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
        cur.append(' ' if c in '\n\t' else c)
        if depth == 0 and c == ')':
            f = ''.join(cur).strip()
            if f: out.append(re.sub(r'\s+', ' ', f))
            cur = []
        i += 1
    return out

if __name__ == '__main__':
    main()
