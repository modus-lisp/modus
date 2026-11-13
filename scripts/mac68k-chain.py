#!/usr/bin/env python3
"""mac68k-chain.py DISK OUT.bin [--forks DIR] -- modus as a Mac application.

DISK is a modus boot disk that can run on top of Mac OS: scripts/mac68k-core.py's
output for MODUS_MAC68K_MACHINE=classicos (its boot blocks' record names a
REFSLOT).  OUT.bin is a MacBinary II file -- the usual way to carry a Mac file,
both forks, through anything that is not a Mac -- of an application, "Modus":

  - its resource fork: CODE 0 (the jump table), CODE 1 (scripts/mac68k-chain.s,
    which starts modus) and SIZE -1;
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


def size_rsrc():
    """SIZE -1, for MultiFinder: no flags, 64 KB preferred and minimum.  The
    image does not live in the partition anyway."""
    return struct.pack('>HII', 0, 64 * 1024, 64 * 1024)


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
    dest, entry, ln, off, _, _, refslot, memtop = struct.unpack_from('>8I', data, k + 8)
    if not refslot:
        sys.exit('mac68k-chain: %s cannot run on top of Mac OS (build it with '
                 'MODUS_MAC68K_MACHINE=classicos)' % disk)
    rsrc = resource_fork([(b'CODE', 0, code0()), (b'CODE', 1, code1()),
                          (b'SIZE', -1, size_rsrc())])
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
