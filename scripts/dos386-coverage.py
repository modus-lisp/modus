"""dos386-coverage.py IMAGE SYMMAP FORMS OUT -- which functions a REPL session enters.

Boots IMAGE (a bare i386 CL image, usually with its saved core:
scripts/dos386-core.py) under QEMU with -d in_asm, which logs every block of
guest code the first time it is translated, types each line of FORMS at the
REPL and waits for the next prompt, then maps every logged address through
SYMMAP (the build's MODUS_SYMMAP) to the function holding it.  OUT gets the
names entered, one a line, after a header: the resident list for a
demand-paged image (MODUS_I386_COLD_KEEP).  Prints how much native code
that is.

  QEMU_MEM  MB of RAM (default 64)
"""
import bisect, os, select, subprocess, sys, tempfile, time


def main():
    image, symmap, forms, out = sys.argv[1:5]
    syms = []
    for l in open(symmap):
        if l.startswith('#'):
            continue
        a, size, off, name = l.rstrip('\n').split('\t')
        syms.append((int(a, 16), int(size), name))
    syms.sort()
    addrs = [s[0] for s in syms]
    log = tempfile.mktemp(prefix='dos386-cov-', suffix='.log')
    p = subprocess.Popen(['qemu-system-i386', '-cpu', '486', '-m', os.environ.get('QEMU_MEM', '64'),
                          '-kernel', image, '-display', 'none', '-serial', 'stdio', '-no-reboot',
                          '-d', 'in_asm', '-D', log],
                         stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    def until(marker, timeout):
        buf, end = b'', time.time() + timeout
        while time.time() < end:
            r, _, _ = select.select([p.stdout], [], [], 0.5)
            if r:
                d = os.read(p.stdout.fileno(), 65536)
                if not d:
                    break
                buf += d
                if buf.endswith(marker):
                    return buf
        return buf

    if not until(b'> ', 300).endswith(b'> '):
        sys.exit('no prompt')
    n = 0
    for line in open(forms):
        line = line.strip()
        if not line or line.startswith(';'):
            continue
        for ch in (line + '\r').encode():
            p.stdin.write(bytes([ch]))
            p.stdin.flush()
            time.sleep(0.002)
        got = until(b'\n> ', 600)
        n += 1
        if not got.endswith(b'\n> '):
            print('no prompt after: %s' % line[:70])
            break
    p.kill()
    p.wait()
    hit, pages, insn = set(), set(), set()
    with open(log, errors='replace') as f:
        for l in f:
            if l.startswith('0x'):
                a = int(l.split(':', 1)[0], 16)
                insn.add(a)
                pages.add(a >> 12)
                i = bisect.bisect_right(addrs, a) - 1
                if i >= 0 and a < syms[i][0] + syms[i][1]:
                    hit.add(i)
    os.unlink(log)
    big = sorted(hit, key=lambda i: -syms[i][1])[:12]
    print('largest entered: ' + ', '.join('%s %dK' % (syms[i][2], syms[i][1] // 1024) for i in big))
    lo, hi = syms[0][0], syms[-1][0] + syms[-1][1]
    code_pages = {pg for pg in pages if lo >> 12 <= pg <= hi >> 12}
    print('pages of native code touched: %d of %d (%.0f%%); distinct instructions: %d'
          % (len(code_pages), (hi - lo) // 4096, 100.0 * len(code_pages) * 4096 / (hi - lo), len(insn)))
    total = sum(s[1] for s in syms)
    kept = sum(syms[i][1] for i in hit)
    names = sorted({syms[i][2] for i in hit})
    with open(out, 'w') as o:
        o.write(';; %s -- the functions a REPL session entered (scripts/dos386-coverage.py):\n'
                ';; %s under QEMU, %d forms from %s.  Resident in a demand-paged image.\n'
                % (os.path.basename(out), os.path.basename(image), n, os.path.basename(forms)))
        for nm in names:
            o.write(nm + '\n')
    print('%d forms; %d of %d functions entered; %d of %d bytes of native code (%.0f%%)'
          % (n, len(hit), len(syms), kept, total, 100.0 * kept / total))


if __name__ == '__main__':
    main()
