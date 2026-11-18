"""mcl-join-cpt.py MCL1.dsk MCL2.dsk MCL3.dsk OUT.cpt -- join the three-segment Compact Pro
archive "MCL 2.0.sea" (floppies 1-3 of the MCL 2.0 set) into one single-volume archive
that unar can extract.

Each segment starts with an 8-byte header (01 <vol> xx xx <dir offset>); only the last
segment's header points at the directory, and the directory's per-file offsets are
(volume number, offset within that volume's segment).  So: keep segment 1 whole, append
segments 2 and 3 without their 8-byte headers, rebase every file offset into the joined
stream, mark everything volume 1, and recompute the directory CRC the way Compact Pro
stores it (CRC-32 without the final inversion).  Needs machfs (run with the venv python).
"""
import struct, sys, zlib
import machfs

disks, out = sys.argv[1:4], sys.argv[4]
names = ('MCL 2.0.sea', 'MCL 2.0.sea.#2', 'MCL 2.0.sea.#3')
parts = []
for d, name in zip(disks, names):
    v = machfs.Volume(); v.read(open(d, 'rb').read()); parts.append(v[name].data)
l0, l1 = len(parts[0]), len(parts[1])

def g(vol, off):
    return off if vol == 1 else (l0 + (off - 8) if vol == 2 else l0 + (l1 - 8) + (off - 8))

a = bytearray(parts[0] + parts[1][8:] + parts[2][8:])
diroff = g(3, struct.unpack('>I', parts[2][4:8])[0])
a[1] = 1; a[4:8] = struct.pack('>I', diroff)
p = diroff + 4
nent = struct.unpack('>H', a[p:p + 2])[0]; p += 2
p += 1 + a[p]                                    # archive comment
done = 0
while done < nent:
    nl = a[p]; p += 1
    if nl & 0x80:                                # folder entry
        p += (nl & 0x7F) + 2; done += 1; continue
    p += nl
    vol = a[p]; off = struct.unpack('>I', a[p + 1:p + 5])[0]
    a[p] = 1; a[p + 1:p + 5] = struct.pack('>I', g(vol, off))
    p += 45; done += 1
a[diroff:diroff + 4] = struct.pack('>I', (zlib.crc32(bytes(a[diroff + 4:])) ^ 0xffffffff) & 0xffffffff)
open(out, 'wb').write(a)
print('%s: %d bytes, %d directory entries' % (out, len(a), nent))
