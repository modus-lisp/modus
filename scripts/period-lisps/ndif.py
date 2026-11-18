"""ndif.py DATA APPLEDOUBLE OUT -- a Disk Copy 6 NDIF image (data fork + resource fork) to a raw disk."""
import os, struct, sys

def adc(src):
    out = bytearray(); i = 0
    while i < len(src):
        b = src[i]
        if b & 0x80:
            n = (b & 0x7F) + 1; out += src[i + 1:i + 1 + n]; i += 1 + n
        elif b & 0x40:
            n = (b & 0x3F) + 4; off = (src[i + 1] << 8) | src[i + 2]; i += 3
            for _ in range(n): out.append(out[-off - 1])
        else:
            n = ((b >> 2) & 0xF) + 3; off = ((b & 3) << 8) | src[i + 1]; i += 2
            for _ in range(n): out.append(out[-off - 1])
    return bytes(out)

def resource(rsrc, rtype):
    doff, moff = struct.unpack_from('>II', rsrc, 0)
    tl = moff + struct.unpack_from('>H', rsrc, moff + 24)[0]
    ntypes = struct.unpack_from('>H', rsrc, tl)[0] + 1
    for t in range(ntypes):
        ty, cnt, ref = struct.unpack_from('>4sHH', rsrc, tl + 2 + 8 * t)
        if ty == rtype:
            r = tl + ref
            dat = struct.unpack_from('>I', rsrc, r + 4)[0] & 0xFFFFFF
            ln = struct.unpack_from('>I', rsrc, doff + dat)[0]
            return rsrc[doff + dat + 4:doff + dat + 4 + ln]
    raise SystemExit('no %r resource' % rtype)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__))); import adouble
data = open(sys.argv[1], 'rb').read(); rsrc = adouble.parse(open(sys.argv[2], 'rb').read())['rsrc']
bcem = resource(rsrc, b'bcem')
# the chunk table: count at 0x7C, then 12-byte entries sector(3) type(1) offset(4) length(4);
# the last has type 0xFF (end of disk)
count = struct.unpack_from('>I', bcem, 0x7C)[0]
k = 0x80
p = k + 12 * (count - 1)
nsec = struct.unpack_from('>I', bcem, p)[0] >> 8
disk = bytearray(nsec * 512)
q = k
while True:
    sec = struct.unpack_from('>I', bcem, q)[0] >> 8; ty = bcem[q + 3]
    off, ln = struct.unpack_from('>II', bcem, q + 4)
    if ty == 0xFF: break
    nxt = struct.unpack_from('>I', bcem, q + 12)[0] >> 8
    if ty == 0x83: chunk = adc(data[off:off + ln])
    elif ty == 0x02: chunk = data[off:off + ln]
    else: chunk = bytes((nxt - sec) * 512)
    disk[sec * 512:sec * 512 + len(chunk)] = chunk[:(nxt - sec) * 512]
    q += 12
open(sys.argv[3], 'wb').write(disk)
print('%s: %d sectors, %s' % (sys.argv[3], nsec, disk[1024:1026]))
