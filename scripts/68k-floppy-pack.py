#!/usr/bin/env python3
"""68k-floppy-pack.py LOADER IMAGE OUT [--test] -- a self-unpacking 68k image.

LOADER is mvm/68k-unlzma.lisp + mvm/unlzma-core.lisp built by
`mvm/build-hosted.lisp 68k` (the two files, in that order); IMAGE is
any hosted 68k image (a core image included).  IMAGE's PT_LOAD contents go
into one flat blob, compressed as a raw LZMA1 stream (lc=2 lp=1 pb=2), and the
result is appended to LOADER, laid out as mvm/68k-unlzma.lisp expects (size,
copier, segment table, stream), with one more PT_LOAD mapping the whole file
at #x19000000 from offset 0 -- no page padding -- and the memory after it
zero-filled for the decoder's output and state.  The payload's file offset
goes in e_ident[12..15], which the ELF loader ignores.  Running OUT
unpacks IMAGE and starts it with the same arguments.

--test: OUT writes the unpacked blob to stdout and exits instead (the check
that the decoder is exact: compare with the blob this script prints the size
of, written to OUT.flat).
"""
import lzma
import os
import struct
import subprocess
import sys
import tempfile

PAGE = 4096
MAP = 0x19000000
OUT_BUF = 0x19200000
STATE_END = 0x19A00000 + 0x100 + 2 * 7991
FILTERS = [{'id': lzma.FILTER_LZMA1, 'preset': 9 | lzma.PRESET_EXTREME,
            'lc': 2, 'lp': 1, 'pb': 2, 'dict_size': 1 << 23, 'nice_len': 273}]
HERE = os.path.dirname(os.path.abspath(__file__))


def loads(elf):
    phoff, = struct.unpack('>I', elf[0x1C:0x20])
    phentsize, phnum = struct.unpack('>HH', elf[0x2A:0x2E])
    out = []
    for i in range(phnum):
        p = elf[phoff + i * phentsize:phoff + (i + 1) * phentsize]
        t, off, va, pa, fs, ms, fl, al = struct.unpack('>8I', p[:32])
        if t == 1:
            out.append((va, off, fs, ms))
    return phoff, phentsize, phnum, out


def copier():
    with tempfile.TemporaryDirectory() as d:
        o, b = os.path.join(d, 'c.o'), os.path.join(d, 'c.bin')
        subprocess.run(['m68k-linux-gnu-as', '-m68040', '-o', o,
                        os.path.join(HERE, '68k-floppy-copier.s')], check=True)
        subprocess.run(['m68k-linux-gnu-objcopy', '-O', 'binary', o, b], check=True)
        return open(b, 'rb').read()


def main(loader, image, out, test=False):
    img = open(image, 'rb').read()
    entry, = struct.unpack('>I', img[0x18:0x1C])
    _, _, _, segs = loads(img)
    flat, table = bytearray(), []
    for va, off, fs, ms in segs:
        if va % PAGE:
            sys.exit('68k-floppy-pack: segment at %#x is not page-aligned' % va)
        table.append((va, OUT_BUF + len(flat), fs, ms))
        flat += img[off:off + fs]
    if OUT_BUF + len(flat) > 0x19A00000:
        sys.exit('68k-floppy-pack: %d bytes unpacked do not fit' % len(flat))
    stream = lzma.compress(bytes(flat), format=lzma.FORMAT_RAW, filters=FILTERS)

    tab = struct.pack('>I', len(table))
    for t in table:
        tab += struct.pack('>4I', *t)
    tab += struct.pack('>I', entry)
    code = copier() + tab
    pay = struct.pack('>HHHH', len(flat) >> 16, len(flat) & 0xFFFF,
                      1 if test else 0, 8 + len(code)) + code + stream

    elf = bytearray(open(loader, 'rb').read())
    phoff, phentsize, phnum, lsegs = loads(elf)
    for va, off, fs, ms in lsegs:
        if va < STATE_END and MAP < va + ms:
            sys.exit('68k-floppy-pack: the loader already maps %#x' % va)
    old = [bytes(elf[phoff + i * phentsize:phoff + (i + 1) * phentsize]) for i in range(phnum)]
    while len(elf) % 4:                 # the copier must be at an even address
        elf.append(0)
    off = len(elf)
    elf[12:16] = struct.pack('>I', off)
    elf += pay
    newoff = len(elf)
    end = MAP + newoff + (phnum + 1) * 32
    if end > OUT_BUF:
        sys.exit('68k-floppy-pack: the file overlaps the output buffer')
    # PT_LOAD RWX: the whole file from offset 0, then the zero-filled buffers
    new = struct.pack('>8I', 1, 0, MAP, MAP, end - MAP, STATE_END - MAP, 7, PAGE)
    for ph in old + [new]:
        elf += ph
    elf[0x1C:0x20] = struct.pack('>I', newoff)
    elf[0x2C:0x2E] = struct.pack('>H', phnum + 1)
    open(out, 'wb').write(elf)
    os.chmod(out, 0o755)
    if test:
        open(out + '.flat', 'wb').write(flat)
    print('68k-floppy-pack: %s: %d segments, %d bytes -> %d LZMA; loader %d; %s %d bytes'
          % (image, len(table), len(flat), len(stream), off, out, len(elf)))


if __name__ == '__main__':
    args = [a for a in sys.argv[1:] if a != '--test']
    main(*args[:3], test='--test' in sys.argv)
