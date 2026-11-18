#!/usr/bin/env python3
"""mac68k-chain.py DISK OUT.bin [--forks DIR] -- modus as a Mac application.

DISK is a modus boot disk that can run on top of Mac OS: scripts/mac68k-core.py's
output for MODUS_MAC68K_MACHINE=classicos (its boot blocks' record names a
REFSLOT).  OUT.bin is a MacBinary II file -- the usual way to carry a Mac file,
both forks, through anything that is not a Mac -- of an application, "Modus":

  - its resource fork: CODE 0 (the jump table), CODE 1 (scripts/mac68k-chain.s,
    which starts modus), SIZE -1, and its icon, "(m)", with the bundle the
    Finder finds it by (FINDER_RESOURCES);
  - its data fork: DISK, byte for byte.

Double-clicked on the Mac, it reads the image out of its own data fork to the
image's address and jumps to it, leaving Mac OS underneath, and the image pages
its code from the same data fork.  One file to copy; nothing is written to the
Mac's disk.  --forks DIR also writes DIR/data and DIR/rsrc, for tools that
build an HFS volume.
"""
import os
import struct
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
HERE = os.path.dirname(os.path.abspath(__file__))
NAME, TYPE, CREATOR = b'Modus', b'APPL', b'MDUS'


# The icon: a diamond with "(m)" in it and a hand writing below, a pen
# and a cuff.  # is black, o white inside the mask (the diamond is filled,
# not transparent on the desktop), . outside.  The small one is the
# diamond and its "(m)".
ICON = (
    '...............##...............',
    '..............#oo#..............',
    '.............#oooo#.............',
    '............#oooooo#............',
    '...........#oooooooo#...........',
    '..........#oooooooooo#..........',
    '.........#oooooooooooo#.........',
    '........#o#oooooooooo#o#........',
    '.......#o##oooooooooo##o#.......',
    '......#o##oooooooooooo##o#......',
    '.....#oo##oo######oooo##oo#.....',
    '....#ooo##oo##o##o##oo##ooo#....',
    '...#oooo##oo##o##o##oo##oooo#...',
    '..#ooooo##oo##o##o##oo##ooooo#..',
    '.#oooooo##oo##o##o##oo##oooooo#.',
    '#oooooooo##oooooooooo##oooooooo#',
    '#ooooooooo#oooooooooo#ooooooooo#',
    '.#ooooooooooooooooo#oooooooooo#.',
    '..#ooooooooooooooo##ooooooooo#..',
    '...#ooooooooooooo##ooooooooo#...',
    '....#ooooooooooo##oo########....',
    '.....#ooooo######ooo#o#o#o#.....',
    '......#ooo#ooo########o#o#......',
    '.......#o#ooo#oooooo#o#o#.......',
    '........##oo#ooooooo##o#........',
    '.........#o###oooooo#o#.........',
    '..........############..........',
    '...........#oooooooo#...........',
    '............#oooooo#............',
    '.............#oooo#.............',
    '..............#oo#..............',
    '...............##...............',
)
SMALL_ICON = (
    '.......##.......',
    '......#oo#......',
    '.....#oooo#.....',
    '....#oooooo#....',
    '...#oooooooo#...',
    '..#o#ooooo#oo#..',
    '.#o#o####oo#oo#.',
    '#oo#o#o#o#o#ooo#',
    '#oo#o#o#o#o#ooo#',
    '.#o#o#o#o#o#oo#.',
    '..#o#ooooo#oo#..',
    '...#oooooooo#...',
    '....#oooooo#....',
    '.....#oooo#.....',
    '......#oo#......',
    '.......##.......',
)


def icon_list(rows):
    """ICN# / ics# data: the bitmap (#), then the mask (# and o)."""
    def bits(on):
        out = bytearray()
        for r in rows:
            v = 0
            for c in r:
                v = v * 2 + (1 if c in on else 0)
            out += v.to_bytes(len(r) // 8, 'big')
        return bytes(out)
    return bits('#') + bits('#o')


def finder_resources():
    """What the Finder shows the application by: its icons (ICN# and ics#
    128), FREF 128 (an APPL, local icon 0), BNDL 128 (signature MDUS, ICN#
    and ics# local 0 -> 128, FREF local 0 -> 128) and the signature's
    owner resource, MDUS 0.  The file needs the hasBundle and hasCustomIcon
    flags (scripts/period-lisps/mkvol.py sets them)."""
    fref = b'APPL' + struct.pack('>h', 0) + b'\x00'
    bndl = CREATOR + struct.pack('>hh', 0, 2)
    for t in (b'ICN#', b'ics#', b'FREF'):
        bndl += t + struct.pack('>hhh', 0, 0, 128)
    # the same icons as the file's CUSTOM icon (-16455, with the file's
    # hasCustomIcon flag), which the Finder draws straight from the resource
    # fork: a volume's Desktop DB may never register the bundle (machfs's
    # is written empty, and the Finder believes it)
    return [(b'ICN#', 128, icon_list(ICON)), (b'ics#', 128, icon_list(SMALL_ICON)),
            (b'ICN#', -16455, icon_list(ICON)), (b'ics#', -16455, icon_list(SMALL_ICON)),
            (b'FREF', 128, fref), (b'BNDL', 128, bndl),
            (CREATOR, 0, bytes([len(NAME)]) + NAME)]


def code1():
    with tempfile.TemporaryDirectory() as d:
        o, b = os.path.join(d, 'c.o'), os.path.join(d, 'c.bin')
        subprocess.run(['m68k-linux-gnu-as', '-m68000', '-o', o,
                        os.path.join(HERE, 'mac68k-chain.s')], check=True)
        subprocess.run(['m68k-linux-gnu-objcopy', '-O', 'binary', o, b], check=True)
        code = open(b, 'rb').read()
    # the segment header: the first jump-table entry it owns (offset 0), one entry
    return struct.pack('>HH', 0, 1) + code


def code0():
    """The jump table: one entry, CODE 1 at offset 0 (from the code after the
    segment's header), unloaded -- MOVE.W #1,-(SP); _LoadSeg.  Above A5: 32
    bytes of application parameters, then the table.  Below A5: the
    QuickDraw globals InitGraf is given (-4(A5) is thePort)."""
    entry = struct.pack('>HHHH', 0, 0x3F3C, 1, 0xA9F0)
    return struct.pack('>IIII', 32 + len(entry), 512, len(entry), 32) + entry


def size_rsrc(memtop, span=0, kmap=0):
    """SIZE -1, for MultiFinder / System 7: no flags, then the preferred and
    minimum partition.  A map for a 4 MB Mac asks 64 KB (System 6 without
    MultiFinder ignores SIZE, and the image lives above the application zone).
    A bigger map's image sits at fixed addresses in the middle of RAM, where
    System 7's Process Manager would otherwise put the Finder's partition and
    its own heap (measured on a 32 MB Quadra 950 under 7.5.3: the image read
    over them, "The disk Macintosh cannot be found", then a Finder bomb).  So
    it asks for ALL of RAM: the Process Manager gives it the largest free
    block, from the system heap's end to the Finder's partition, and the
    image's range is inside it (the code checks the system heap's end; the
    code and stack, at the partition's bottom and top, are checked too)."""
    if span:
        # a RELOCATED image (mac68k-chain.s's relocating load) goes wherever
        # the partition is, so the partition is what it needs and no more,
        # and the Finder and the others keep the rest: SPAN, plus the heap
        # (the relocation table first, then the Toolbox's), the 512 KB
        # alignment of the image's base, and the application's own stack.
        # Asking for all of RAM, as below, left the Finder unable to open
        # even the desktop (a 32 MB Quadra 950 under 7.5.3).
        need = span + 0x180000
        # a map whose young heap is last (KMAP set) takes its semispaces from
        # whatever partition it gets (mac68k-chain.s HSIZE): SPAN is its
        # smallest heap, and the preferred size -- Get Info's to change, or
        # MODUS_PARTITION_KB here -- is that and 4 MB more
        pref = need
        if kmap:
            kb = os.environ.get('MODUS_PARTITION_KB')
            pref = max(need, int(kb) * 1024) if kb else need + 0x400000
        return struct.pack('>HII', 0x0080, pref, need)
    if memtop <= 4 << 20:
        return struct.pack('>HII', 0, 64 * 1024, 64 * 1024)
    # is32BitCompatible: a relocated image runs at whatever address its partition has
    return struct.pack('>HII', 0x0080, memtop, 20 << 20)


def resource_fork(resources):
    """RESOURCES: [(type, id, data)].  Inside Macintosh: More Macintosh
    Toolbox, "Resource Manager": a 16-byte header, the data (each a 4-byte
    length, then the bytes), the map (a copy of the header, 6 reserved bytes,
    attributes, the type list's and name list's offsets, the type list, the
    reference lists)."""
    data = bytearray()
    offsets = []
    for _, _, d in resources:
        offsets.append(len(data))
        data += struct.pack('>I', len(d)) + d
    types = []
    for t, _, _ in resources:
        if t not in types:
            types.append(t)
    tl = struct.pack('>H', len(types) - 1)
    reflists = bytearray()
    first = 2 + 8 * len(types)              # the reference lists, from the type list
    entries = []
    for t in types:
        mine = [(i, r) for i, r in enumerate(resources) if r[0] == t]
        entries.append(struct.pack('>4sHH', t, len(mine) - 1, first + len(reflists)))
        for i, (_, rid, _) in mine:
            reflists += struct.pack('>hhI', rid, -1, offsets[i] & 0xFFFFFF) + bytes(4)
    tl += b''.join(entries)
    data_off = 256
    map_off = data_off + len(data)
    body = tl + reflists
    map_len = 28 + len(body)
    header = struct.pack('>IIII', data_off, map_off, len(data), map_len)
    rmap = header + bytes(4 + 2 + 2) + struct.pack('>HH', 28, map_len) + body
    return header + bytes(256 - 16) + bytes(data) + rmap


def crc_ccitt(b):
    crc = 0
    for x in b:
        crc ^= x << 8
        for _ in range(8):
            crc = ((crc << 1) ^ 0x1021) & 0xFFFF if crc & 0x8000 else (crc << 1) & 0xFFFF
    return crc


def macbinary(name, ftype, creator, data, rsrc):
    """MacBinary II: a 128-byte header (CRC-CCITT of its first 124 bytes at
    124), the data fork and the resource fork, each padded to 128."""
    h = bytearray(128)
    h[1] = len(name)
    h[2:2 + len(name)] = name
    h[65:69], h[69:73] = ftype, creator
    struct.pack_into('>II', h, 83, len(data), len(rsrc))
    t = 0xA0000000                          # 1985: some date, both
    struct.pack_into('>II', h, 91, t, t)
    h[122] = h[123] = 129                   # MacBinary II, and what it needs
    struct.pack_into('>H', h, 124, crc_ccitt(h[:124]))
    pad = lambda b: b + bytes(-len(b) % 128)
    return bytes(h) + pad(data) + pad(rsrc)


def main(disk, out, forks=None):
    data = open(disk, 'rb').read()
    k = data.find(b'modusCHN', 0, 1024)
    if k < 0:
        sys.exit('mac68k-chain: %s has no launch record in its boot blocks' % disk)
    dest, entry, ln, off, _, _, refslot, memtop, reloff, _, span, kmap = struct.unpack_from('>12I', data, k + 8)
    if not refslot:
        sys.exit('mac68k-chain: %s cannot run on top of Mac OS (build it with '
                 'MODUS_MAC68K_MACHINE=classicos)' % disk)
    rsrc = resource_fork([(b'CODE', 0, code0()), (b'CODE', 1, code1()),
                          (b'SIZE', -1, size_rsrc(memtop, span if reloff else 0, kmap if reloff else 0))] + finder_resources())
    open(out, 'wb').write(macbinary(NAME, TYPE, CREATOR, data, rsrc))
    if forks:
        os.makedirs(forks, exist_ok=True)
        open(os.path.join(forks, 'data'), 'wb').write(data)
        open(os.path.join(forks, 'rsrc'), 'wb').write(rsrc)
    print('mac68k-chain: %s: "%s", data fork %d bytes (reads %d at #%x, needs %d KB of RAM), '
          'resource fork %d bytes' % (out, NAME.decode(), len(data), ln, dest, memtop // 1024, len(rsrc)))


if __name__ == '__main__':
    args = sys.argv[1:]
    forks = None
    if '--forks' in args:
        i = args.index('--forks')
        forks = args[i + 1]
        del args[i:i + 2]
    if len(args) != 2:
        sys.exit(__doc__)
    main(args[0], args[1], forks)
