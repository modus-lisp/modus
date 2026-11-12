#!/usr/bin/env python3
"""mac68k-core.py IMAGE DISK -- a Mac Classic boot disk that starts from a SAVED CORE.

IMAGE is a paged bare-Mac image built for the Classic (MODUS_MAC68K_MACHINE=
classic MODUS_68K_PAGED=1 MODUS_68K_CPU=68000, a static-heap build).  A Classic
cannot boot it from scratch -- boot wants a 5 MB heap and ~50 MB of allocation
-- so this boots it ONCE on a big Mac (Basilisk II, 32 MB; MAC68K_BASILISK /
MAC68K_ROM as scripts/mac68k-ladder.py) with the core descriptor's SAVE flag
set.  That boot runs on its :BOOT- heap above 4 MB, freezes, and sends its core
on serial port A (build-cl-repl-common.lisp %MAC-SEND-CORE): "MCOR", the low
block's address, the static objects' and conses' ranges, then the low block,
the objects and the conses.

The static heap is then MOVED to the Classic's static area (boot-mac68k.lisp
:STATIC-LO), objects first, conses after them, as a copying collection would
move it: every word that is a pointer into it -- tag 1 (cons) or 7 (object),
in a pointer-bearing object, a cons, or the low block -- is rewritten.  Leaf
objects (strings, byte vectors, numbers) are not scanned; neither are the low
block's byte tables (the cards, the crossing table, the console's font and
state).  The static block's ranges are set to the new ones and the cards left
off: a core boot rebuilds them (%GC-CARDS-INIT).

DISK: the boot blocks read [image, low block end) in one go -- the resident
image (stream table marked not loaded), the moved static heap at :STATIC-LO,
the low block at :REGION -- and the stream section follows on the disk, its
disk bias in the image's modusPGD descriptor.  The core descriptor's CORE flag
is set.
"""
import os, pty, select, shlex, signal, struct, subprocess, sys, time, tty

sys.dont_write_bytecode = True
os.environ.setdefault('MODUS_MAC68K_MACHINE', 'classic')   # mac68k-disk.py's load address
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)

# boot/boot-mac68k.lisp's :CLASSIC layout
LOAD = 0x00040000
STATIC_LO, STATIC_HI = 0x00110000, 0x002B0000
REGION = 0x002B0000
LOW_BLOCK = 0x20000
LEAF = {0x10, 0x11, 0x12, 0x13, 0x14, 0x16, 0x60, 0x64, 0x65, 0x66}
# low-block byte tables, offsets from the region (not scanned for pointers)
LOW_BYTES = [(0x6000, 0x6100),      # the static block's raw words (set by hand)
             (0x6100, 0x7F00),      # card marks
             (0x9000, 0x9800),      # serial parameter blocks, console state + font
             (0x1E000, 0x20000)]    # card crossing table


def disk_module():
    import importlib.util
    spec = importlib.util.spec_from_file_location('mac68k_disk', os.path.join(HERE, 'mac68k-disk.py'))
    md = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(md)
    return md


def set_flag(img, which, value):
    """The core descriptor 'modusMCR': +8 CORE, +12 SAVE."""
    k = img.find(b'modusMCR')
    if k < 0 or img.find(b'modusMCR', k + 1) >= 0:
        sys.exit('mac68k-core: no unique modusMCR descriptor (a Classic build?)')
    struct.pack_into('>I', img, k + {'core': 8, 'save': 12}[which], value)


def capture_core(disk, timeout):
    """Boot DISK under Basilisk II and read the core it sends."""
    master, slave = pty.openpty()
    tty.setraw(slave)
    cmd = shlex.split(os.environ.get("MAC68K_BASILISK", "BasiliskII")) + [
        "--nogui", "true", "--rom", os.environ["MAC68K_ROM"], "--cpu", "4",
        "--fpu", "false", "--ramsize", str(32 << 20),
        "--jit", os.environ.get("MAC68K_JIT", "false"), "--jitcachesize", "16384",
        "--disk", disk, "--seriala", os.ttyname(slave)]
    p = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                         start_new_session=True)
    got, end, need = b"", time.time() + timeout, None
    try:
        while time.time() < end and p.poll() is None:
            r, _, _ = select.select([master], [], [], 0.5)
            if r:
                got += os.read(master, 65536)
            k = got.find(b'MCOR')
            if k >= 0 and need is None and len(got) >= k + 24:
                base, olo, ohi, clo, chi = struct.unpack('>5I', got[k + 4:k + 24])
                need = k + 24 + LOW_BLOCK + (ohi - olo) + (chi - clo)
                print('mac68k-core: core coming: low block #%x, objects #%x..#%x, conses #%x..#%x'
                      % (base, olo, ohi, clo, chi))
            if need is not None and len(got) >= need:
                break
    finally:
        for sig in (signal.SIGTERM, signal.SIGKILL):
            try:
                os.killpg(p.pid, sig)
            except ProcessLookupError:
                break
            time.sleep(1)
        os.close(master)
        os.close(slave)
    if need is None or len(got) < need:
        sys.stderr.write(got[-2000:].decode('latin-1'))
        sys.exit('mac68k-core: no complete core on serial (%d bytes)' % len(got))
    k = got.find(b'MCOR')
    return got[:k], got[k:need]


def relocate(core):
    base, olo, ohi, clo, chi = struct.unpack('>5I', core[4:24])
    if base != REGION:
        sys.exit('mac68k-core: the low block is at #%x, the layout says #%x' % (base, REGION))
    pos = 24
    low = bytearray(core[pos:pos + LOW_BLOCK]); pos += LOW_BLOCK
    objs = bytearray(core[pos:pos + ohi - olo]); pos += ohi - olo
    conses = bytearray(core[pos:pos + chi - clo])
    nolo = STATIC_LO
    nclo = (nolo + len(objs) + 4095) & ~4095
    if os.environ.get('MAC68K_CORE_NORELOC') == '1':
        # THE BISECT: the static heap left where the core run had it (a 32 MB
        # Mac only), so a failure is the core itself, not the move
        nolo, nclo = olo, clo
        nchi = chi
        print('mac68k-core: NOT relocating (MAC68K_CORE_NORELOC=1)')
        for off, v in ((0x601C, 0), (0x6014, 1),
                       (0x6030, 1 if os.environ.get('MODUS_MAC_CORE_PROBES') == '1' else 0)):
            struct.pack_into('>I', low, off, v)
        return low, nolo, objs, nclo, conses
    nchi = nclo + len(conses)
    if nchi > STATIC_HI:
        sys.exit('mac68k-core: the static heap (%d bytes) does not fit #%x..#%x'
                 % (nchi - nolo, STATIC_LO, STATIC_HI))
    do, dc = nolo - olo, nclo - clo
    moved = [0]

    def rel(buf, off):
        w, = struct.unpack_from('>I', buf, off)
        t = w & 7
        if t in (1, 7):
            a = w - t
            if olo <= a < ohi:
                struct.pack_into('>I', buf, off, w + do); moved[0] += 1
            elif clo <= a < chi:
                struct.pack_into('>I', buf, off, w + dc); moved[0] += 1

    # objects: typed (68k translate-68k m68k-emit-object-size)
    i, nobj = 0, 0
    while i < len(objs):
        h, = struct.unpack_from('>I', objs, i)
        st, n = h & 0xFF, h >> 8
        size = (n + 4) if st in (0x11, 0x13) else (n + 1) * 4
        size = (size + 7) & ~7
        if size <= 0 or i + size > len(objs):
            sys.exit('mac68k-core: a bad object header #%08x at #%x' % (h, olo + i))
        if st not in LEAF:
            for j in range(i + 4, i + 4 + 4 * n, 4):
                rel(objs, j)
        i += size; nobj += 1
    for j in range(0, len(conses), 4):
        rel(conses, j)
    skip = lambda off: any(a <= off < b for a, b in LOW_BYTES)
    for j in range(0, LOW_BLOCK, 4):
        if not skip(j):
            rel(low, j)
    # the static block: the new ranges; cards off until %GC-CARDS-INIT
    for off, v in ((0x6004, nolo), (0x6008, nolo + len(objs)), (0x600C, nclo), (0x6010, nchi),
                   (0x601C, 0), (0x6014, 1),
                   (0x6030, 1 if os.environ.get('MODUS_MAC_CORE_PROBES') == '1' else 0)):
        struct.pack_into('>I', low, off, v)
    # a new collection count: anything keyed by address re-hashes
    c, = struct.unpack_from('>I', low, 0x60)
    struct.pack_into('>I', low, 0x60, c + 1)
    print('mac68k-core: %d objects (%d bytes) -> #%x, %d bytes of conses -> #%x; %d pointers moved'
          % (nobj, len(objs), nolo, len(conses), nclo, moved[0]))
    return low, nolo, objs, nclo, conses


def main(image, disk):
    md = disk_module()
    img0 = open(image, 'rb').read()
    img, resident, count = md.paged(img0)
    img = bytearray(img)
    if LOAD + resident > STATIC_LO:
        sys.exit('mac68k-core: the resident image (%d bytes) runs into the static heap' % resident)
    # 1. the core run: the image as mac68k-disk.py --paged writes it, SAVE set
    run = bytearray(img)
    set_flag(run, 'save', 1)
    run_disk = disk + '.corerun'
    md.write_disk(bytes(run), run_disk, LOAD, read_len=resident)
    t0 = time.time()
    console, core = capture_core(run_disk, float(os.environ.get('MAC68K_CORE_TIMEOUT', '900')))
    print('mac68k-core: the core run took %.0f s; it said:\n%s'
          % (time.time() - t0, console.decode('latin-1')[-400:]))
    open(disk + '.core', 'wb').write(core)        # as sent, for looking into
    low, nolo, objs, nclo, conses = relocate(core)
    # 2. the disk: [image, low block end) in one read, the streams after it
    top = max(REGION + LOW_BLOCK, nclo + len(conses))
    chunk = bytearray(top - LOAD)
    chunk[:resident] = img[:resident]
    chunk[nolo - LOAD:nolo - LOAD + len(objs)] = objs
    chunk[nclo - LOAD:nclo - LOAD + len(conses)] = conses
    chunk[REGION - LOAD:REGION - LOAD + LOW_BLOCK] = low
    streams = img[resident:]
    stream_disk = 1024 + len(chunk)                # disk byte of the section's first
    k = chunk.find(b'modusPGD')
    table, cnt, section = struct.unpack_from('>III', chunk, k + 8)
    struct.pack_into('>I', chunk, k + 20, (section - stream_disk) & 0xFFFFFFFF)  # disk bias (SUB.L wraps)
    set_flag(chunk, 'core', 1)
    md.write_disk(bytes(chunk) + bytes(streams), disk, LOAD, read_len=len(chunk))
    print('mac68k-core: %s: boot reads %d bytes (image %d, static %d, low block %d); %d streams '
          '(%d bytes) on demand from disk byte %d'
          % (disk, len(chunk), resident, len(objs) + len(conses), LOW_BLOCK, cnt,
             len(streams), stream_disk))


if __name__ == '__main__':
    main(*sys.argv[1:3])
