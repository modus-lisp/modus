#!/usr/bin/env python3
"""mac68k-disk.py IMAGE DISK -- a Mac boot disk that IS modus.

IMAGE is a flat bare-Mac image (`mvm/build-hosted.lisp mac68k`, or any image
built for the :mac68k target, boot/boot-mac68k.lisp).  DISK is its boot blocks
(scripts/mac68k-bootblocks.s, assembled here) followed by IMAGE at offset 1024,
padded to whole 512-byte blocks and to at least 800 KB.  The Mac ROM runs the
boot blocks, which read IMAGE to its load address and jump to it: no System,
no Finder, no filesystem.

Run it under Basilisk II (see scripts/mac68k-run.sh) with the console on
serial port A.
"""
import os
import struct
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
LOAD_ADDR = 0x00100000   # boot/boot-mac68k.lisp +MAC68K-LOAD-ADDR+


def bootblocks():
    with tempfile.TemporaryDirectory() as d:
        o, b = os.path.join(d, 'b.o'), os.path.join(d, 'b.bin')
        subprocess.run(['m68k-linux-gnu-as', '-m68000', '-o', o,
                        os.path.join(HERE, 'mac68k-bootblocks.s')], check=True)
        subprocess.run(['m68k-linux-gnu-objcopy', '-O', 'binary', o, b], check=True)
        syms = {}
        for line in subprocess.run(['m68k-linux-gnu-nm', o], check=True,
                                   capture_output=True, text=True).stdout.splitlines():
            addr, _, name = line.split()
            syms[name] = int(addr, 16)
        return bytearray(open(b, 'rb').read()), syms


def write_disk(img, disk, load_addr=LOAD_ADDR, min_size=819200, entry=None):
    """Boot blocks that read IMG (bytes) to LOAD_ADDR and jump to ENTRY (default
    LOAD_ADDR), then IMG at offset 1024, padded to whole blocks and to at
    least MIN_SIZE."""
    bb, syms = bootblocks()
    if syms['start'] != 0x8A:
        sys.exit('mac68k-disk: the loader is at %#x, not 0x8A' % syms['start'])
    if len(bb) > 1024:
        sys.exit('mac68k-disk: boot blocks are %d bytes' % len(bb))
    struct.pack_into('>I', bb, syms['dest'], load_addr)
    struct.pack_into('>I', bb, syms['entry'], load_addr if entry is None else entry)
    # a disk driver reads whole 512-byte blocks (a byte count is paramErr, -50)
    struct.pack_into('>I', bb, syms['len'], (len(img) + 511) & ~511)
    struct.pack_into('>I', bb, syms['off'], 1024)
    out = bb + bytes(1024 - len(bb)) + img
    out += bytes(-len(out) % 512)
    out += bytes(max(0, min_size - len(out)))
    open(disk, 'wb').write(out)
    return len(out)


def main(image, disk):
    img = open(image, 'rb').read()
    n = write_disk(img, disk)
    print('mac68k-disk: %s (%d bytes at %#x) -> %s, %d bytes'
          % (image, len(img), LOAD_ADDR, disk, n))


if __name__ == '__main__':
    main(*sys.argv[1:3])
