#!/usr/bin/env python3
"""mac68k-reloc.py A B SHIFT [OUT] -- the relocations of a Mac image, from two
builds of it whose whole memory map is SHIFT (hex, a multiple of 512 KB) apart
(boot-mac68k's MODUS_MAC68K_SHIFT).

A word that holds an address differs between the two by SHIFT; one that holds
an address as a tagged fixnum (address << 1) by twice that; the allocation
sites' kind-map base (kind map - region / 8) by 7/8 of it.  So a relocation's
KIND is eighths of the shift: 8, 16 or 7.  With SHIFT and the run-time shift
both multiples of 512 KB (64 KB x 8), no relocated value's low 16 bits move,
and every relocation is one 16-bit word to which the loader adds
((shift >> 16) * KIND) / 8.  That also covers code that builds an address from
two separate 16-bit halves.

Prints a summary and every difference that is neither (with the symbol map's
function around it, when A's .map exists and LOAD=HEX is set); with OUT, writes
the table: the number of kinds, then per kind its KIND, its count and its
offsets, ascending, all big-endian u32s.
"""
import bisect
import os
import struct
import sys


def fn_map(path):
    if not os.path.exists(path):
        return None
    rows = []
    for line in open(path):
        if line.startswith('#'):
            continue
        p = line.rstrip('\n').split('\t')
        if len(p) >= 4:
            rows.append((int(p[0], 16), p[3]))
    rows.sort()
    return rows


def main(a_path, b_path, shift_hex, out=None):
    shift = int(shift_hex, 16)
    if shift % 0x80000:
        sys.exit('mac68k-reloc: SHIFT must be a multiple of 512 KB')
    a, b = open(a_path, 'rb').read(), open(b_path, 'rb').read()
    if len(a) != len(b):
        sys.exit('mac68k-reloc: the builds differ in size (%d vs %d): the layout moved, not just addresses'
                 % (len(a), len(b)))
    hi = shift >> 16
    KINDS = (8, 16, 7)
    kinds = {k: [] for k in KINDS}
    by_d = {(hi * k // 8) & 0xFFFF: k for k in KINDS}
    odd = []
    i = 0
    n = len(a) & ~1
    while i < n:
        if a[i:i + 2] != b[i:i + 2]:
            wa, = struct.unpack_from('>H', a, i)
            wb, = struct.unpack_from('>H', b, i)
            d = (wb - wa) & 0xFFFF
            k = by_d.get(d)
            if k:
                kinds[k].append(i)
            else:
                odd.append((i, wa, wb))
        i += 2
    print('mac68k-reloc: %d bytes; %d address words, %d tagged-address words, %d kind-map words, '
          '%d other differences' % (len(a), len(kinds[8]), len(kinds[16]), len(kinds[7]), len(odd)))
    fns = fn_map(os.path.splitext(a_path)[0] + '.map')
    load = None
    if fns:
        load = int(os.environ.get('LOAD', '0'), 16) if os.environ.get('LOAD') else None
    for off, wa, wb in odd[:200]:
        where = ''
        if fns and load is not None:
            j = bisect.bisect_right([r[0] for r in fns], load + off) - 1
            where = fns[j][1] if j >= 0 else '?'
        print('  +%07X  %04X -> %04X  %s' % (off, wa, wb, where))
    if len(odd) > 200:
        print('  ... %d more' % (len(odd) - 200))
    if out:
        with open(out, 'wb') as f:
            f.write(struct.pack('>I', len(KINDS)))
            for k in KINDS:
                f.write(struct.pack('>II', k, len(kinds[k])))
                f.write(b''.join(struct.pack('>I', o) for o in kinds[k]))
        print('mac68k-reloc: %s, %d relocations' % (out, sum(len(v) for v in kinds.values())))
    return 0 if not odd else 1


if __name__ == '__main__':
    sys.exit(main(*sys.argv[1:5]))
