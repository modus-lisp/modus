"""dos386-core.py IMAGE OUT -- IMAGE with its boot-time heap appended (a SAVED CORE).

IMAGE is a bare i386 CL image (mvm/build-i386-cl-repl.lisp).  Booting it does
the same boot init every time -- the symbol, package and function tables, the
defvar thunks, the runtime macros and the shims compiled from source -- which
is seconds under QEMU and minutes on a 486.  This boots it once under QEMU
with SAVECORE in its Multiboot command line (-append), so that after boot init it
collects until its live data sits at the bottom of the lower semispace and
stops (build-cl-repl-common.lisp %I386-CORE-SAVE).  Then it reads, through
QMP, the low block (#x10000000, 128 KB), the heap from #x19000000 to the
allocation pointer and the matching slices of the two collector bitmaps, and
writes OUT = IMAGE + the core:

  page-aligned header: 'MCOR', version 1, allocation pointer, heap bytes,
                       one bitmap slice's bytes; at +64 the low block, the
                       heap, bitmap slice 1, bitmap slice 2
  last 16 bytes:       'MODUSCOR', the header's physical address, 0

and patches OUT's Multiboot load_end_addr to its new end.  The entry stub
(boot/boot-i386-cl.lisp) restores it when the machine's heap holds it.

It also compares the image as the guest holds it with IMAGE and prints the
ranges boot init wrote to: those would be lost on a restored boot.

  QEMU_MEM  MB of RAM for the save run (default 64)
"""
import json, os, re, select, socket, struct, subprocess, sys, tempfile, time

LOAD = 0x100000
LOW, LOW_SIZE = 0x10000000, 0x20000
HEAP = 0x19000000


def clear_from(bm, nbits):
    """BM with every bit from index NBITS on cleared (bit i = byte i/8, bit i%8)."""
    b = bytearray(bm)
    if nbits % 8:
        b[nbits // 8] &= (1 << (nbits % 8)) - 1
        start = nbits // 8 + 1
    else:
        start = nbits // 8
    b[start:] = bytes(len(b) - start)
    return bytes(b)


def main():
    sys.stdout.reconfigure(line_buffering=True)
    image, out = sys.argv[1], sys.argv[2]
    img = open(image, 'rb').read()
    tmp = tempfile.mkdtemp(prefix='dos386-core-')
    sock = os.path.join(tmp, 'qmp.sock')
    p = subprocess.Popen(['qemu-system-i386', '-cpu', '486', '-m', os.environ.get('QEMU_MEM', '64'),
                          '-kernel', image, '-display', 'none', '-serial', 'stdio', '-no-reboot',
                          '-append', 'SAVECORE',
                          '-qmp', 'unix:%s,server=on,wait=off' % sock],
                         stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    try:
        buf, t0 = b'', time.time()
        while b'CORE-SAVE' not in buf or not buf.split(b'CORE-SAVE', 1)[1].count(b'\n'):
            if time.time() - t0 > 600 or p.poll() is not None:
                sys.exit('no CORE-SAVE line; serial said:\n' + buf.decode('latin1')[-2000:])
            r, _, _ = select.select([p.stdout], [], [], 1)
            if r:
                buf += os.read(p.stdout.fileno(), 65536)
        line = buf.split(b'CORE-SAVE', 1)[1].split(b'\n', 1)[0].decode()
        f = {k: int(v) for k, v in re.findall(r'(\w+)=(-?\d+)', line)}
        print('CORE-SAVE', f, 'boot took %.1f s' % (time.time() - t0))
        free, page = f['free'], f['page']
        heap_bytes = (free - HEAP + 3) & ~3
        bm_bytes = (((free - page + 127) // 128) + 3) & ~3
        q = socket.socket(socket.AF_UNIX)
        q.connect(sock)
        qf = q.makefile('rw')
        qf.readline()

        def qmp(cmd, **args):
            qf.write(json.dumps({'execute': cmd, 'arguments': args}) + '\n')
            qf.flush()
            while True:
                r = json.loads(qf.readline())
                if 'return' in r or 'error' in r:
                    return r

        qmp('qmp_capabilities')
        qmp('stop')

        def read(addr, n):
            path = os.path.join(tmp, 'm%x.bin' % addr)
            r = qmp('memsave', val=addr, size=n, filename=path)
            data = open(path, 'rb').read() if os.path.exists(path) else b''
            if len(data) != n:
                sys.exit('memsave 0x%x %d: %r' % (addr, n, r))
            return data

        low = read(LOW, LOW_SIZE)
        heap = read(HEAP, heap_bytes)
        # Only the bits of granules below the allocation pointer: the slices
        # are rounded up, and what the save itself allocated after reading the
        # pointer (its report) left bits past it.  A restored process
        # allocates there first, and a stale cons-kind bit under a new object
        # makes the collector take it for a cons and leave it behind.
        nbits = (free - page) // 16
        bm1 = clear_from(read(f['bm1'], bm_bytes), nbits)
        bm2 = clear_from(read(f['bm2'], bm_bytes), nbits)
        live = read(LOAD, len(img))
    finally:
        p.kill()
    # what boot init wrote into the image itself (a restored boot has the file's bytes)
    diffs, i = [], 0
    while i < len(img):
        if img[i] != live[i]:
            j = i
            while j < len(img) and (img[j] != live[j] or (j + 16 < len(img) and img[j:j+16] != live[j:j+16])):
                j += 1
            diffs.append((LOAD + i, j - i))
            i = j
        else:
            i += 1
    print('image bytes boot init changed: %d ranges, %d bytes' % (len(diffs), sum(n for _, n in diffs)))
    for a, n in diffs[:20]:
        print('  0x%08x +%d' % (a, n))
    body = bytearray(img)
    body += b'\0' * (-len(body) % 4096)
    hdr_pa = LOAD + len(body)
    hdr = struct.pack('<4sIIII', b'MCOR', 1, free, heap_bytes, bm_bytes)
    body += hdr + b'\0' * (64 - len(hdr)) + low + heap + bm1 + bm2
    body += b'MODUSCOR' + struct.pack('<II', hdr_pa, 0)
    struct.pack_into('<I', body, 20, LOAD + len(body))       # load_end_addr
    open(out, 'wb').write(body)
    print('%s: image %d + core %d bytes (low %d, heap %d, bitmaps 2 x %d)'
          % (out, len(img), len(body) - len(img), LOW_SIZE, heap_bytes, bm_bytes))


if __name__ == '__main__':
    main()
