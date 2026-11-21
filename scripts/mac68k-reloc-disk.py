#!/usr/bin/env python3
"""mac68k-reloc-disk.py A.dsk B.dsk SHIFT SPAN -- make disk A relocatable.

A and B are the same Mac disk (scripts/mac68k-core.py's) built SHIFT apart
(hex, a multiple of 512 KB; mac68k-snap-build.sh SHIFT=).  The part its loader
reads -- the image, the static heap and the low block, LEN bytes at disk byte
OFF -- is classified with mac68k-reloc.py's CLASSIFY, the table is appended to
A, and A's launch record ("modusCHN", mac68k-bootblocks.s) gets RELOFF, RELLEN
and SPAN (hex: the bytes of map from DEST the loader must allocate).

The table, for scripts/mac68k-chain.s: per KIND (8, 16, 7), a u32 KIND, a u32
COUNT, then COUNT entries, each the gap from the previous offset (from 0): a
byte G in 1..255 = 2*G bytes on, or a 0 byte then the offset as 4 big-endian
bytes.  The loader adds ((shift >> 16) * KIND) / 8 to the 16-bit word there.

Fails, saying where, on any difference that is not a relocation, except the
ones listed in ALLOWED_TEXT (text that records the build, not addresses).
"""
import importlib.util
import os
import struct
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('mac68k_reloc', os.path.join(HERE, 'mac68k-reloc.py'))
reloc = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reloc)


def record(d):
    k = d.find(b'modusCHN', 0, 1024)
    if k < 0:
        sys.exit('mac68k-reloc-disk: no launch record in the boot blocks')
    return k + 8


def encode(offsets):
    out = bytearray()
    prev = 0
    for o in offsets:
        g = o - prev
        if 0 < g <= 510 and g % 2 == 0:
            out.append(g // 2)
        else:
            out.append(0)
            out += struct.pack('>I', o)
        prev = o
    return bytes(out)


def printable(w):
    return all(0x20 <= c < 0x7F or c in (9, 10, 13) for c in w.to_bytes(2, 'big'))


def cold_lists(a, b, dest, off, ln, shift):
    """COLD NATIVE CODE (translate-68k M68K-EMIT-COLD-ENTER): per cold body,
    its words that differ between A and B by a relocation, as COLD-ENTER
    applies them: n8 n16 n7, then each kind's offsets / 2 (u16s).  None when
    the image has no cold code.  Returns (the index's disk byte, [lists])."""
    k = a.find(b'modusCLD', off, off + ln)
    if k < 0:
        return None
    bias, start, count, xindex, stubs = struct.unpack_from('>5I', a, k + 8)
    lists = []
    bad = 0
    for i in range(count):
        st = off + stubs + 16 * i - dest
        home, t = struct.unpack_from('>II', a, st + 6)
        at = home - bias                    # the body's disk byte (A's BIAS)
        kinds, odd = reloc.classify(bytes(a[at:at + t]), b[at:at + t], shift)
        if odd:
            bad += len(odd)
            for o, wa, wb in odd[:4]:
                print('  cold %d +%X: %04X -> %04X' % (i, o, wa, wb))
        lst = struct.pack('>3H', *(len(kinds[kk]) for kk in (8, 16, 7)))
        for kk in (8, 16, 7):
            for o in kinds[kk]:
                if o >= 1 << 17:
                    sys.exit('mac68k-reloc-disk: cold body %d is too big' % i)
                lst += struct.pack('>H', o // 2)
        lists.append(bytes(lst))
    if bad > int(os.environ.get('MAC68K_RELOC_COLD_ALLOW', '0')):
        sys.exit('mac68k-reloc-disk: %d differences in cold bodies that are not relocations' % bad)
    return off + xindex - dest, lists


def main(a_path, b_path, shift_hex, span_hex):
    shift, span = int(shift_hex, 16), int(span_hex, 16)
    a, b = bytearray(open(a_path, 'rb').read()), open(b_path, 'rb').read()
    r = record(a)
    dest, entry, ln, off = struct.unpack_from('>4I', a, r)
    reloff, = struct.unpack_from('>I', a, r + 32)
    if reloff:
        sys.exit('mac68k-reloc-disk: %s already has a table' % a_path)
    kinds, odd = reloc.classify(bytes(a[off:off + ln]), b[off:off + ln], shift)
    # text that records the build (its settings, the names it harvested) and
    # the build id may differ; anything else is an address the table misses
    bad = [(o, wa, wb) for o, wa, wb in odd if not (printable(wa) and printable(wb))]
    print('mac68k-reloc-disk: %d bytes from %#x: %s; %d text differences, %d others'
          % (ln, dest, ', '.join('%d x%d/8' % (len(kinds[k]), k) for k in reloc.KINDS),
             len(odd) - len(bad), len(bad)))
    for o, wa, wb in bad[:40]:
        print('  +%07X (#%X)  %04X -> %04X' % (o, dest + o, wa, wb))
    if len(bad) > int(os.environ.get('MAC68K_RELOC_ALLOW', '4')):
        sys.exit('mac68k-reloc-disk: too many differences that are not relocations')
    cold = cold_lists(a, b, dest, off, ln, shift)
    table = bytearray()
    for k in reloc.KINDS:
        table += struct.pack('>II', k, len(kinds[k])) + encode(kinds[k])
    a += bytes(-len(a) % 512)
    at = len(a)
    a += table + bytes(-len(table) % 512)
    struct.pack_into('>III', a, r + 32, at, len(table), span)
    if cold:
        # the cold bodies' external relocations after the table, and the
        # resident index (blocks << 25 | disk byte) that finds each
        xindex, lists = cold
        base = len(a)
        blob = bytearray()
        seen = {}
        for i, lst in enumerate(lists):
            if lst not in seen:
                seen[lst] = base + len(blob)
                blob += lst
            at_i = seen[lst]
            blocks = (at_i % 512 + len(lst) + 511) // 512
            if blocks > 127 or at_i >= 1 << 25:
                sys.exit('mac68k-reloc-disk: cold list %d (%d bytes at %d) out of reach' % (i, len(lst), at_i))
            struct.pack_into('>I', a, xindex + 4 * i, blocks << 25 | at_i)
        a += blob + bytes(-len(blob) % 512)
        print('mac68k-reloc-disk: %d cold bodies: %d bytes of external relocations at disk byte %d'
              % (len(lists), len(blob), base))
    open(a_path, 'wb').write(a)
    print('mac68k-reloc-disk: %s: table %d bytes at disk byte %d, span %#x'
          % (a_path, len(table), at, span))


if __name__ == '__main__':
    main(*sys.argv[1:5])
