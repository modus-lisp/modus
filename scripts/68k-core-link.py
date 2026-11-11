#!/usr/bin/env python3
"""68k-core-link.py IMAGE CORE OUT -- append a saved core to a 68k image.

IMAGE is a MODUS_68K_GC_STATIC image (ELF32 big-endian); CORE is what that
image wrote under MODUS_68K_SAVE_CORE (%68K-SAVE-CORE in build-68k-cli.lisp):

    magic Y  addr0 len0  addr1 len1  addr2 len2   (eight big-endian longwords)
    region 0 bytes, region 1 bytes, region 2 bytes

Each region becomes its own PT_LOAD -- read/write, file-backed, page-aligned --
so the loader maps the low block and the static heap at their addresses, clean
until written.  The program header table moves to the end of the file (e_phoff
may point anywhere); the original segment is untouched, so no address moves.
Last, the stub's core descriptor (marker "moduscor", then flag and Y) gets
flag 1 and Y.
"""
import struct
import sys

PAGE = 4096


def main(image, core, out):
    elf = bytearray(open(image, 'rb').read())
    c = open(core, 'rb').read()
    magic, y, *regs = struct.unpack('>8I', c[:32])
    if magic != 12865357:
        sys.exit('68k-core-link: %s is not a core' % core)
    regions, pos = [], 32
    for i in range(3):
        addr, length = regs[2 * i], regs[2 * i + 1]
        regions.append((addr, c[pos:pos + length]))
        pos += length

    if elf[:4] != b'\x7fELF' or elf[5] != 2:
        sys.exit('68k-core-link: %s is not a big-endian ELF' % image)
    phoff, = struct.unpack('>I', elf[0x1C:0x20])
    phentsize, phnum = struct.unpack('>HH', elf[0x2A:0x2E])
    old = [bytes(elf[phoff + i * phentsize:phoff + (i + 1) * phentsize]) for i in range(phnum)]

    # the stub's descriptor
    k = elf.find(b'moduscor')
    if k < 0 or elf.find(b'moduscor', k + 1) >= 0:
        sys.exit('68k-core-link: no unique core descriptor in %s' % image)
    elf[k + 8:k + 16] = struct.pack('>II', 1, y)

    new = []
    for addr, data in regions:
        if not data:
            continue
        while len(elf) % PAGE:
            elf.append(0)
        off = len(elf)
        elf += data
        # PT_LOAD, offset, vaddr, paddr, filesz, memsz, flags RW, align
        new.append(struct.pack('>8I', 1, off, addr, addr, len(data), len(data), 6, PAGE))

    while len(elf) % 4:
        elf.append(0)
    newoff = len(elf)
    for ph in old + new:
        elf += ph
    elf[0x1C:0x20] = struct.pack('>I', newoff)
    elf[0x2C:0x2E] = struct.pack('>H', len(old) + len(new))
    open(out, 'wb').write(elf)
    print('68k-core-link: %s + %d regions (%d bytes) -> %s, Y = %#x'
          % (image, len(new), sum(len(d) for _, d in regions), out, y))


if __name__ == '__main__':
    main(*sys.argv[1:4])
