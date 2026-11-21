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

  page-aligned header: 'MCOR', version 4 (3 without the gap, 2 with no code
                       store; see THE GAP in main), allocation
                       pointer, heap bytes, one bitmap slice's bytes, the
                       boot-only tail's first and last page (IMAGE.tail,
                       from the build; 0 0 when there is none), the code
                       store's address, the pool's size (0: the stub's
                       rule), the gap's bytes (version 4); at +64 the low
                       block, the heap,
                       bitmap slice 1, bitmap slice 2; then, page-aligned,
                       the code store (see CODE-STORE)
  last 16 bytes:       'MODUSCOR', the header's physical address, 0

and patches OUT's Multiboot load_end_addr to its new end.  The entry stub
(boot/boot-i386-cl.lisp) restores it when the machine's heap holds it.

It also compares the image as the guest holds it with IMAGE and prints the
ranges boot init wrote to: those would be lost on a restored boot.

  QEMU_MEM  MB of RAM for the save run (default 64)
  POOL_KB   the demand-paged code's pool, in KB (default 0: the entry
            stub's rule, RAM/4, at least 2 MB, at most the paged code)
"""
import json, os, re, select, socket, struct, subprocess, sys, tempfile, time

LOAD = 0x100000
LOW, LOW_SIZE = 0x10000000, 0x20000
HEAP = 0x19000000


CHUNK = 16384


def code_store(image, img, tmp):
    """The demand-paged code (IMAGE.paged, from the build: native byte 0 to
    the boot-only tail), whole 16 KB chunks of it, each compressed on its own
    (scripts/dos386-lz.c): plo, phi, the chunk size, n, offsets[n+1] from the
    data, the data.  A restored process maps none of it until it is touched
    (boot/boot-i386-cl.lisp's page-fault handler).  None without IMAGE.paged,
    or with MODUS_I386_PAGED=0."""
    if os.environ.get('MODUS_I386_PAGED') == '0' or not os.path.exists(image + '.paged'):
        return b''
    lo, hi = map(int, open(image + '.paged').read().split())
    plo, phi = (lo + CHUNK - 1) // CHUNK * CHUNK, hi // CHUNK * CHUNK
    if phi <= plo:
        return b''
    lz = os.path.join(tmp, 'dos386-lz')
    subprocess.run(['cc', '-O2', '-o', lz, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                                       'dos386-lz.c')], check=True)
    raw = os.path.join(tmp, 'code.bin')
    open(raw, 'wb').write(img[plo - LOAD:phi - LOAD])
    subprocess.run([lz, 'c', raw, raw + '.lz', str(CHUNK)], check=True)
    subprocess.run([lz, 'd', raw + '.lz', raw + '.back', str(CHUNK)], check=True)
    if open(raw + '.back', 'rb').read() != img[plo - LOAD:phi - LOAD]:
        sys.exit('the code store does not decode to the code')
    packed = open(raw + '.lz', 'rb').read()
    print('code store: %d bytes of code in %d chunks -> %d bytes (%.2fx)'
          % (phi - plo, (phi - plo) // CHUNK, len(packed), (phi - plo) / len(packed)))
    return struct.pack('<III', plo, phi, CHUNK) + packed


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
    # CORE_FORMS=FILE: an INSTALL core.  Boot with SAVELATER (the native tier off, nothing
    # saved at boot), type FILE's lines at the REPL one at a time, each after the previous
    # prompt, then (%I386-CORE-SAVE): the core carries whatever they loaded.  kiln t420
    # installs the desk this way.  QEMU_EXTRA adds QEMU arguments (its network), QEMU_CPU
    # the CPU model, CORE_LOG the serial transcript.
    forms_path = os.environ.get('CORE_FORMS')
    forms = []
    if forms_path:
        forms = [l.rstrip('\n') for l in open(forms_path)
                 if l.strip() and not l.lstrip().startswith(';')]
        forms.append('(modus-int::%i386-core-save)')
    p = subprocess.Popen(['qemu-system-i386', '-cpu', os.environ.get('QEMU_CPU', '486'),
                          '-m', os.environ.get('QEMU_MEM', '64'),
                          '-kernel', image, '-display', 'none', '-serial', 'stdio', '-no-reboot',
                          '-append', os.environ.get('CORE_APPEND', 'SAVELATER' if forms else 'SAVECORE'),
                          '-qmp', 'unix:%s,server=on,wait=off' % sock]
                         + os.environ.get('QEMU_EXTRA', '').split(),
                         stdin=subprocess.PIPE if forms else subprocess.DEVNULL,
                         stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    log = open(os.environ['CORE_LOG'], 'wb') if os.environ.get('CORE_LOG') else None
    try:
        buf, t0 = b'', time.time()
        mark = 0                          # where the last form's answer starts
        limit = float(os.environ.get('CORE_FORM_LIMIT', '3600'))
        while b'CORE-SAVE' not in buf or not buf.split(b'CORE-SAVE', 1)[1].count(b'\n'):
            if time.time() - t0 > (600 + limit * len(forms)) or p.poll() is not None:
                sys.exit('no CORE-SAVE line; serial said:\n' + buf.decode('latin1')[-2000:])
            if forms and (b'\n> ' in buf[mark:] or buf[mark:].startswith(b'> ')):
                if mark and b'\nERROR:' in buf[mark:]:
                    sys.exit('a form failed; serial said:\n' + buf[mark:].decode('latin1')[-2000:])
                form = forms.pop(0)
                print('form %.0fs: %s' % (time.time() - t0, form[:100]))
                mark = len(buf)
                p.stdin.write(form.encode() + b'\r')
                p.stdin.flush()
            pf = buf.find(b'#PF ', mark)
            if b'REPL-DIED' in buf[mark:] or (pf >= 0 and b'\n' in buf[pf:]):
                sys.exit('the guest died; serial said:\n' + buf.decode('latin1')[-2000:])
            r, _, _ = select.select([p.stdout], [], [], 1)
            if r:
                d = os.read(p.stdout.fileno(), 65536)
                buf += d
                if log: log.write(d); log.flush()
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
        # THE NATIVE TIER'S CODE (jit=, %I386-CORE-SAVE): what it translated at
        # #x12000000.  A core saved after the tier ran points into it (the function
        # tables, and every closure native code made), so it travels with the heap;
        # the stub copies it back to the same address (boot-i386-cl.lisp, header +44).
        jit_bytes = (f.get('jit', 0) + 3) & ~3
        jit = read(0x12000000, jit_bytes) if jit_bytes > 0 else b''
        if jit_bytes:
            print('native code: %d bytes' % jit_bytes)
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
    # THE GAP (version 4): with a code store the paged code [plo, phi) is in
    # the file twice, raw in the image and packed in the store, and the raw
    # copy is unmapped the moment the stub runs.  Left out, the loader reads
    # 7.7 MB less and the machine needs that much less RAM: the stub maps
    # [phi, ..) onto where the rest landed, PHI - PLO lower.  Every physical
    # address written below (header, store, tail) is where the bytes ARE.
    # MODUS_I386_GAP=0 keeps the raw copy (version 3).
    store = code_store(image, img, tmp)
    gap = plo = phi = 0
    if store and os.environ.get('MODUS_I386_GAP', '1') != '0':
        plo, phi = struct.unpack_from('<II', store, 0)
        gap = phi - plo
        print('gap: [0x%x, 0x%x), %d bytes of raw code left out' % (plo, phi, gap))
    body = bytearray(img[:plo - LOAD] + img[phi - LOAD:]) if gap else bytearray(img)
    body += b'\0' * (-len(body) % 4096)
    hdr_pa = LOAD + len(body)
    # the boot-only tail (IMAGE.tail, written by the build): its whole pages,
    # which a restored process never runs and the stub lays the heap over
    tail_lo = tail_hi = 0
    if os.path.exists(image + '.tail'):
        lo, hi = map(int, open(image + '.tail').read().split())
        tail_lo, tail_hi = (lo + 4095) & ~4095, hi & ~4095
        if tail_hi <= tail_lo:
            tail_lo = tail_hi = 0
        print('boot-only tail: %d pages (%d bytes of code)' % ((tail_hi - tail_lo) // 4096, hi - lo))
        if gap and tail_lo:                         # where its pages are
            assert tail_lo >= phi, 'the boot-only tail is inside the gap'
            tail_lo -= gap
            tail_hi -= gap
    core = low + heap + bm1 + bm2 + jit
    store_pa = 0
    if store:
        store_pa = hdr_pa + 64 + len(core)
        store_pa += -store_pa % 4096
    pool = int(os.environ.get('POOL_KB', '0')) * 1024
    # THE STORE ON THE DISK (version 5, MODUS_I386_DISK_STORE=1, with a gap):
    # the store's index -- plo, phi, the chunk size, n, offsets[n+1] -- stays
    # where it was; its data goes after the trailer, past load_end, so the
    # loader never reads it.  A page fault reads the sectors of the one chunk
    # it needs, through the no-DOS loader's PM_READ (scripts/pcboot2.s), from
    # the image's first sector + its file offset (+40).  Only a disk that
    # boots with scripts/pcboot.sh can run such an image.
    disk = bool(gap) and os.environ.get('MODUS_I386_DISK_STORE') == '1'
    index, data = store, b''
    if disk:
        n = struct.unpack_from('<I', store, 12)[0]
        index, data = store[:16 + 4 * (n + 1)], store[16 + 4 * (n + 1):]
    version = 5 if disk else 4 if gap else 3 if store else 2
    hdr = struct.pack('<4sIIIIIIIIIII', b'MCOR', version, free,
                      heap_bytes, bm_bytes, tail_lo, tail_hi, store_pa, pool, gap, 0,
                      len(jit))
    body += hdr + b'\0' * (64 - len(hdr)) + core
    if store:
        body += b'\0' * (store_pa - LOAD - len(body))
        body += index
    body += b'MODUSCOR' + struct.pack('<II', hdr_pa, 0)
    struct.pack_into('<I', body, 20, LOAD + len(body))       # load_end_addr
    if disk:
        body += b'\0' * (-len(body) % 512)
        struct.pack_into('<I', body, hdr_pa - LOAD + 40, len(body))   # the data's file offset
        body += data
        print('code store on the disk: index %d bytes loaded, %d bytes of data past load_end'
              % (len(index), len(data)))
    open(out, 'wb').write(body)
    print('%s: image %d + core %d bytes (low %d, heap %d, bitmaps 2 x %d)'
          % (out, hdr_pa - LOAD, len(body) - (hdr_pa - LOAD), LOW_SIZE, heap_bytes, bm_bytes))


if __name__ == '__main__':
    main()
