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

# per machine (boot/boot-mac68k.lisp's layouts): the image, the static heap's
# nominal range, the runtime-data region, and the low block's size (the
# layout's :ALLOC-START: the core run sends that much; its last 8 KB is the
# static heap's card crossing table)
LOAD, STATIC_LO, STATIC_HI, REGION, LOW_BLOCK = {
    'classic':   (0x00040000, 0x00110000, 0x002B0000, 0x002B0000, 0x20000),
    'classicos': (0x00040000, 0x00110000, 0x00240000, 0x002B0000, 0x20000),   # JIT area above
    'quadra':    (0x00600000, 0x00B00000, 0x00B80000, 0x00BD0000, 0x20000),   # cache, stack, NIL's page between
    'classic2m': (0x00040000, 0x000D5000, 0x00178000, 0x00178000, 0xE000),
    'plus1m':    (0x00040000, 0x0008E000, 0x000D1000, 0x000D1000, 0xE000),
    'plus1mjit': (0x0003C000, 0x0008E000, 0x000D1000, 0x000D1000, 0xE000),
}[os.environ['MODUS_MAC68K_MACHINE'].lower()]
# MODUS_MAC68K_SHIFT (boot-mac68k): the whole map moved; LOW_BLOCK is a size
SHIFT = int(os.environ.get('MODUS_MAC68K_SHIFT') or '0', 16)
LOAD, STATIC_LO, STATIC_HI, REGION = LOAD + SHIFT, STATIC_LO + SHIFT, STATIC_HI + SHIFT, REGION + SHIFT
# a map that can run on top of Mac OS (boot-mac68k's :CLASSIC-OS; launched by
# scripts/mac68k-chain.py's application): the RAM it needs.  The boot blocks'
# record then names the static block's #x6050, the open file's reference number.
CHAIN_MEMTOP = {'classicos': 0x00400000, 'quadra': 0x00800000}.get(os.environ['MODUS_MAC68K_MACHINE'].lower(), 0)
# the layout's kind map when its young heap is last in the map (boot-mac68k
# :QUADRA): the application's loader then sizes the heap from its partition
KMAP = {'quadra': 0x01000000}.get(os.environ['MODUS_MAC68K_MACHINE'].lower(), 0)
KMAP = KMAP and KMAP + SHIFT
# where the layout LINKS the init-only text (boot-mac68k :INIT-BASE, cross.lisp
# *POOL-INIT-BASE*), when not where it lies in the image: the core run's disk
# puts it there
INIT_BASE = {'plus1m': 0x00400000, 'plus1mjit': 0x00400000}.get(os.environ['MODUS_MAC68K_MACHINE'].lower())
LEAF = {0x10, 0x11, 0x12, 0x13, 0x14, 0x16, 0x60, 0x64, 0x65, 0x66}
# low-block byte tables, offsets from the region (not scanned for pointers)
LOW_BYTES = [(0x6000, 0x6100),      # the static block's raw words (set by hand)
             (0x6100, 0x7F00),      # card marks
             (0x9000, 0x9800),      # serial parameter blocks, console state + font
             (0xA000, 0xC000),      # the console's text buffer
             (LOW_BLOCK - 0x2000, LOW_BLOCK)]    # card crossing table


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
        "--fpu", "false", "--ramsize", str(int(os.environ.get("MAC68K_RAM_MB", "32")) << 20),
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


COLD_FNS = None


def cold_fn_pages(recs):
    """The cold function records (hash.l value.l flags.b len.b name, even) as
    index pages and record pages: page 0.. the count of record pages, then
    each's first hash; a run of one hash never straddles two pages."""
    rows, i = [], 0
    while i < len(recs):
        h, = struct.unpack_from('>I', recs, i)
        n = 10 + recs[i + 9]
        n += n & 1
        rows.append((h, bytes(recs[i + 4:i + 9]), recs[i:i + n])); i += n
    rows.sort(key=lambda r: (r[0], r[2]))
    pages, firsts, cur, k = [], [], bytearray(), 0
    while k < len(rows):
        run = [r for r in rows[k:k + 64] if r[0] == rows[k][0]]
        size = sum(len(r[2]) for r in run)
        if size > 512:
            sys.exit('mac68k-core: %d cold functions share hash #%x' % (len(run), rows[k][0]))
        if len(cur) + size > 512:
            pages.append(bytes(cur) + bytes(512 - len(cur))); cur = bytearray()
        if not cur:
            firsts.append(rows[k][0])
        for r in run:
            cur += r[2]
        k += len(run)
    if cur:
        pages.append(bytes(cur) + bytes(512 - len(cur)))
    index = struct.pack('>I', len(pages)) + b''.join(struct.pack('>I', f) for f in firsts)
    index += bytes((-len(index)) % 512)
    return index + b''.join(pages), len(rows), len(pages)


def relocate(core, static_lo=None):
    base, olo, ohi, clo, chi = struct.unpack('>5I', core[4:24])
    if base != REGION:
        sys.exit('mac68k-core: the low block is at #%x, the layout says #%x' % (base, REGION))
    pos = 24
    low = bytearray(core[pos:pos + LOW_BLOCK]); pos += LOW_BLOCK
    objs = bytearray(core[pos:pos + ohi - olo]); pos += ohi - olo
    conses = bytearray(core[pos:pos + chi - clo])
    nolo = static_lo or STATIC_LO
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
    # COLD FUNCTIONS (mvm/mac68k-cold.lisp %COLD-FNS-CUT): the freeze left the
    # function table's image entries as records in a byte vector, its raw word
    # at #x6048.  Take the bytes; nothing else points at the vector, so the
    # collection below drops it.
    global COLD_FNS
    w, = struct.unpack_from('>I', low, 0x6048)
    struct.pack_into('>I', low, 0x6048, 0)
    struct.pack_into('>I', low, 0x604C, 0)
    if w:
        a = w - 7
        h, = struct.unpack_from('>I', objs, a - olo)
        if w & 7 != 7 or not (olo <= a < ohi) or h & 0xFF != 0x11:
            sys.exit('mac68k-core: #x6048 is not a byte vector (#%08x)' % w)
        COLD_FNS = bytes(objs[a - olo + 4:a - olo + 4 + (h >> 8)])
    # THE CORE'S OWN COLLECTION.  A core boot's only roots are the low block's
    # words (the stack starts fresh), so what they do not reach is garbage the
    # freeze kept -- a stale word on the core run's stack or in a register
    # (measured: a dropped 12,250-name table, 326 KB).  Mark from the low
    # block, as the collector scans (LEAF payloads are not pointers), then copy
    # the marked objects and conses down in their order, as a copying collection
    # would.  MAC68K_CORE_NOGC=1 keeps everything (the old uniform move).
    skip = lambda off: any(a <= off < b for a, b in LOW_BYTES)
    ostarts, osize, otype = [], {}, {}
    i = 0
    while i < len(objs):
        h, = struct.unpack_from('>I', objs, i)
        st, n = h & 0xFF, h >> 8
        size = (n + 4) if st in (0x11, 0x13) else (n + 1) * 4
        size = (size + 7) & ~7
        if size <= 0 or i + size > len(objs):
            sys.exit('mac68k-core: a bad object header #%08x at #%x' % (h, olo + i))
        ostarts.append(olo + i); osize[olo + i] = size; otype[olo + i] = (st, n)
        i += size
    import bisect
    def node_of(w):
        """The object start or cons cell a tagged word points into, or None."""
        t = w & 7
        if t == 7:
            a = w - 7
            if olo <= a < ohi:
                k = bisect.bisect_right(ostarts, a) - 1
                return ostarts[k] if k >= 0 else None
        elif t == 1:
            a = w - 1
            if clo <= a < chi:
                return a & ~7
        return None
    nogc = os.environ.get('MAC68K_CORE_NOGC') == '1'
    # MARKED in the order a Cheney collection would copy: breadth first from
    # the low block's words in address order, each node's slots in order.  The
    # copy below keeps that order, so the static heap's layout is a function
    # of what is reachable alone -- not of the core run's allocation history,
    # which a conservative stack scan makes vary between runs of one build
    # (interrupt frames leave words in frame slots never written).  Two core
    # runs a shift apart must lay out alike: mac68k-reloc-disk.py diffs them.
    import collections
    marked = set()
    order = []
    work = collections.deque()
    def mark_word(w):
        nd = node_of(w)
        if nd is not None and nd not in marked:
            marked.add(nd); work.append(nd); order.append(nd)
    if nogc:
        marked.update(ostarts); marked.update(range(clo, chi, 8))
        order = list(ostarts) + list(range(clo, chi, 8))
    else:
        for j in range(0, LOW_BLOCK, 4):
            if not skip(j):
                mark_word(struct.unpack_from('>I', low, j)[0])
        while work:
            nd = work.popleft()
            if nd in osize:
                st, n = otype[nd]
                if st in LEAF or n == 0:
                    continue
                for w in struct.unpack_from('>%dI' % n, objs, nd - olo + 4):
                    mark_word(w)
            else:
                for w in struct.unpack_from('>II', conses, nd - clo):
                    mark_word(w)
    if os.environ.get('MAC68K_CORE_CENSUS'):
        # MAC68K_CORE_CENSUS=1: what the static heap holds, by subtag, and the
        # largest objects (the candidates for paging or for not keeping)
        import collections
        cn, cb = collections.Counter(), collections.Counter()
        for a in ostarts:
            if a in marked:
                cn[otype[a][0]] += 1; cb[otype[a][0]] += osize[a]
        for st, b in cb.most_common():
            print('mac68k-core: census subtag #x%02x: %6d objects %8d bytes' % (st, cn[st], b))
        big = sorted((osize[a], a) for a in ostarts if a in marked)[-25:]
        for sz, a in reversed(big):
            print('mac68k-core: census big #x%x subtag #x%02x %d bytes' % (a, otype[a][0], sz))
    # the new places: marked objects in order from NOLO, conses from NCLO
    omap, nob, kept = {}, bytearray(), 0
    for a in order:
        if a in osize:
            omap[a] = nolo + len(nob)
            nob += objs[a - olo:a - olo + osize[a]]
    nclo = (nolo + len(nob) + 4095) & ~4095
    cmap, ncs = {}, bytearray()
    for a in order:
        if a not in osize:
            cmap[a] = nclo + len(ncs)
            ncs += conses[a - clo:a - clo + 8]
    nchi = nclo + len(ncs)
    if nchi > STATIC_HI:
        sys.exit('mac68k-core: the static heap (%d bytes) does not fit #%x..#%x'
                 % (nchi - nolo, nolo, STATIC_HI))
    moved = [0]

    def rel(buf, off):
        w, = struct.unpack_from('>I', buf, off)
        nd = node_of(w)
        if nd is None:
            return
        if nd in omap:
            struct.pack_into('>I', buf, off, (w - nd) + omap[nd]); moved[0] += 1
        elif nd in cmap:
            struct.pack_into('>I', buf, off, (w - nd) + cmap[nd]); moved[0] += 1
        else:
            sys.exit('mac68k-core: a pointer to an unmarked node #%x' % nd)

    nobj = 0
    for a in order:
        if a not in omap:
            continue
        st, n = otype[a]; k = omap[a] - nolo; nobj += 1
        if st not in LEAF:
            for j in range(k + 4, k + 4 + 4 * n, 4):
                rel(nob, j)
    for j in range(0, len(ncs), 4):
        rel(ncs, j)
    for j in range(0, LOW_BLOCK, 4):
        if not skip(j):
            rel(low, j)
    print('mac68k-core: the core\'s collection kept %d of %d objects (%d of %d bytes), '
          '%d of %d bytes of conses' % (nobj, len(ostarts), len(nob), len(objs),
                                        len(ncs), len(conses)))
    objs, conses = nob, ncs
    # the static block: the new ranges; cards off until %GC-CARDS-INIT
    for off, v in ((0x6004, nolo), (0x6008, nolo + len(objs)), (0x600C, nclo), (0x6010, nchi),
                   (0x601C, 0), (0x6014, 1),
                   (0x6030, 1 if os.environ.get('MODUS_MAC_CORE_PROBES') == '1' else 0)):
        struct.pack_into('>I', low, off, v)
    # the cards' tables (the marks, the crossing table): %GC-CARDS-INIT
    # rebuilds both at a core boot, and the core run's describe its own layout
    # -- which two runs a shift apart do not share
    low[0x6100:0x7F00] = bytes(0x7F00 - 0x6100)
    low[LOW_BLOCK - 0x2000:LOW_BLOCK] = bytes(0x2000)
    # a new collection count: anything keyed by address re-hashes
    c, = struct.unpack_from('>I', low, 0x60)
    struct.pack_into('>I', low, 0x60, c + 1)
    print('mac68k-core: %d objects (%d bytes) -> #%x, %d bytes of conses -> #%x; %d pointers moved'
          % (nobj, len(objs), nolo, len(conses), nclo, moved[0]))
    return low, nolo, objs, nclo, conses


def main(image, disk):
    md = disk_module()
    img0 = open(image, 'rb').read()
    if img0.find(b'modusPGD') >= 0:
        img, resident, count = md.paged(img0)
    else:                                   # unpaged (MODUS_68K_PAGED=0): all resident
        img, resident, count = img0, len(img0), 0
    img = bytearray(img)
    # COLD NATIVE CODE (translate-68k M68K-EMIT-COLD-ENTER): the descriptor
    # 'modusCLD' (+8 BIAS, +12 the cold section's address, +16 the count); the
    # section is the image's tail, never read at boot -- not even by the core
    # run, whose disk has the image at byte 1024 as the build's BIAS assumes
    cold_desc = img.find(b'modusCLD', 0, resident)
    cold_n = 0
    if cold_desc >= 0:
        cold_at, cold_n = struct.unpack_from('>II', img, cold_desc + 12)
        resident = cold_at - LOAD
        # the boot tail ends there; the core run's scratch and I/O pages, NIL's
        # page and low block start 128 KB under the low block (boot-mac68k)
        if cold_at > REGION - 0x20000:
            sys.exit('mac68k-core: the boot tail (to #%x) runs into the pages under the low block (#%x)'
                     % (cold_at, REGION - 0x20000))
        print('mac68k-core: %d cold functions, %d bytes from #%x, on the disk after the boot read'
              % (cold_n, len(img) - resident, cold_at))
    # what must stay resident: up to the init-only text's marker, when there is
    # one (the static heap is laid over that text; see below)
    kept = img.find(b'modusINI', 0, resident)
    kept = resident if kept < 0 else kept
    if LOAD + kept > STATIC_LO:
        sys.exit('mac68k-core: the resident image (%d bytes) runs into the static heap' % kept)
    # 1. the core run: the image as mac68k-disk.py --paged writes it, SAVE set
    run = bytearray(img)
    set_flag(run, 'save', 1)
    run_disk = disk + '.corerun'
    if INIT_BASE is not None:
        # the init-only text is linked at INIT_BASE: the run reads the
        # resident part up to its marker, a gap, then the text at INIT_BASE;
        # the streams follow on the disk, and the descriptor's disk bias says so
        k = run.find(b'modusINI', 0, resident)
        if k < 0 or LOAD + k > INIT_BASE:
            sys.exit('mac68k-core: no init-only text to put at #%x' % INIT_BASE)
        body = run[:k] + bytes(INIT_BASE - LOAD - k) + run[k:resident]
        body += bytes(-len(body) % 512)     # streams keep their homes' alignment on disk
        d = body.find(b'modusPGD')
        sect, = struct.unpack_from('>I', body, d + 16)
        struct.pack_into('>I', body, d + 20, (sect - (1024 + len(body))) & 0xFFFFFFFF)
        md.write_disk(bytes(body) + bytes(run[resident:]), run_disk, LOAD, read_len=len(body))
        print('mac68k-core: the core run reads %d bytes, the init-only text at #%x'
              % (len(body), INIT_BASE))
    else:
        md.write_disk(bytes(run), run_disk, LOAD, read_len=resident)
    t0 = time.time()
    console, core = capture_core(run_disk, float(os.environ.get('MAC68K_CORE_TIMEOUT', '900')))
    print('mac68k-core: the core run took %.0f s; it said:\n%s'
          % (time.time() - t0, console.decode('latin-1')[-400:]))
    open(disk + '.core', 'wb').write(core)        # as sent, for looking into
    # INIT-ONLY TEXT (cross.lisp *POOL-INIT-ONLY*): the resident image ends in
    # literals only boot reads, after a "modusINI" marker; a core boot never
    # reads them, so the static heap starts there, over them
    k = img.find(b'modusINI', 0, resident)
    static_lo = None
    if k >= 0 and os.environ.get('MAC68K_CORE_KEEP_INIT') == '1':
        print('mac68k-core: init-only text kept (MAC68K_CORE_KEEP_INIT=1)')
    elif k >= 0:
        static_lo = (LOAD + k + 4095) & ~4095
        print('mac68k-core: init-only text: %d bytes at #%x; the static heap starts at #%x'
              % (resident - k, LOAD + k, static_lo))
    # MAC68K_CORE_STATIC_LO=ADDR puts the static heap at ADDR instead.  Basilisk
    # II's LC 475 ROM uses RAM in [#xB3000, #xB9000) while the REPL runs (a core
    # with its static heap there breaks DEFMACRO under Basilisk, measured, and
    # runs on MAME's Classic); for a Basilisk test, start it at #xB9000 or above.
    if os.environ.get('MAC68K_CORE_STATIC_LO'):
        static_lo = int(os.environ['MAC68K_CORE_STATIC_LO'], 0)
        print('mac68k-core: the static heap at #%x (MAC68K_CORE_STATIC_LO)' % static_lo)
    low, nolo, objs, nclo, conses = relocate(core, static_lo)
    # 2. the disk: [image, low block end) in one read, the streams after it
    top = max(REGION + LOW_BLOCK, nclo + len(conses))
    chunk = bytearray(top - LOAD)
    chunk[:resident] = img[:resident]
    # what the static heap does not cover of the init-only text and the boot
    # tail (cross.lisp *POOL-INIT-ONLY*, compiler.lisp *BOOT-TAIL-LAST*) is dead
    # once the core is saved: zeroed, so the disk carries no stale code (and
    # two builds a shift apart agree there, for mac68k-reloc-disk.py)
    if static_lo is not None:
        dead_lo = static_lo - LOAD
        chunk[dead_lo:resident] = bytes(max(0, resident - dead_lo))
    chunk[nolo - LOAD:nolo - LOAD + len(objs)] = objs
    chunk[nclo - LOAD:nclo - LOAD + len(conses)] = conses
    chunk[REGION - LOAD:REGION - LOAD + LOW_BLOCK] = low
    streams = img[resident:]
    stream_disk = 1024 + len(chunk)                # disk byte of the section's first
    k = chunk.find(b'modusPGD')
    cnt = 0
    if k >= 0:
        table, cnt, section = struct.unpack_from('>III', chunk, k + 8)
        struct.pack_into('>I', chunk, k + 20, (section - stream_disk) & 0xFFFFFFFF)  # disk bias (SUB.L wraps)
    set_flag(chunk, 'core', 1)
    if cold_desc >= 0:
        # the cold section's disk byte: BIAS = its address - that
        struct.pack_into('>I', chunk, cold_desc + 8, (LOAD + resident - stream_disk) & 0xFFFFFFFF)
        # what the core run loaded is gone: no cold floor, no JIT top
        chunk[REGION - LOAD + 0x6068:REGION - LOAD + 0x6078] = bytes(16)
    # 3. COLD NAMES (mvm/mac68k-cold.lisp): IMAGE.cold, when the build wrote one,
    # on a block boundary after the streams; the static block's #x6044 says where
    tail = bytes(streams)
    cold_path = image + '.cold'
    if os.path.exists(cold_path):
        cold = open(cold_path, 'rb').read()
        pad = (-(stream_disk + len(tail))) % 512
        cold_disk = stream_disk + len(tail) + pad
        tail += bytes(pad) + cold
        struct.pack_into('>I', chunk, REGION - LOAD + 0x6044, cold_disk)
        print('mac68k-core: cold names: %d bytes at disk byte %d' % (len(cold), cold_disk))
    if COLD_FNS:
        region, n, npages = cold_fn_pages(COLD_FNS)
        pad = (-(stream_disk + len(tail))) % 512
        fn_disk = stream_disk + len(tail) + pad
        tail += bytes(pad) + region
        struct.pack_into('>I', chunk, REGION - LOAD + 0x604C, fn_disk)
        print('mac68k-core: cold functions: %d names in %d pages at disk byte %d'
              % (n, npages, fn_disk))
    md.write_disk(bytes(chunk) + tail, disk, LOAD, read_len=len(chunk),
                  refslot=(REGION + 0x6050) if CHAIN_MEMTOP else 0, memtop=CHAIN_MEMTOP,
                  kmap=KMAP if CHAIN_MEMTOP else 0)
    print('mac68k-core: %s: boot reads %d bytes (image %d, static %d, low block %d); %d streams '
          '(%d bytes) on demand from disk byte %d'
          % (disk, len(chunk), resident, len(objs) + len(conses), LOW_BLOCK, cnt,
             len(streams), stream_disk))


if __name__ == '__main__':
    main(*sys.argv[1:3])
