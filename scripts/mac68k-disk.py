#!/usr/bin/env python3
"""mac68k-disk.py IMAGE DISK -- a Mac boot disk that IS modus.

SUPERSEDED by scripts/mac68k-disk.lisp (byte-identical, 2026-11-20); kept only
because scripts/mac68k-core.py imports WRITE_DISK and PAGED from it, and goes
when that is ported.

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

def kernel_source(name):
    """scripts/NAME as a build means it: modus's own if it has one, else vessel's
    kernel/scripts/NAME (lib/load-mvm.lisp MODUS-SOURCE's rule; vessel is
    $MODUS_VESSEL, else .vessel/ in the tree, else a sibling ../vessel)."""
    own = os.path.join(HERE, name)
    if os.path.exists(own):
        return own
    root = os.path.dirname(HERE)
    for v in (os.environ.get('MODUS_VESSEL') or None, os.path.join(root, '.vessel'),
              os.path.join(os.path.dirname(root), 'vessel')):
        if v and os.path.exists(os.path.join(v, 'kernel', 'scripts', name)):
            return os.path.join(v, 'kernel', 'scripts', name)
    sys.exit('%s is in neither modus scripts/ nor vessel kernel/scripts/' % name)

# boot/boot-mac68k.lisp's image address for MODUS_MAC68K_MACHINE (lc / classic)
LOAD_ADDR = {'lc': 0x00100000, 'classic': 0x00040000, 'classicos': 0x00040000, 'quadra': 0x00600000, 'floppy': 0x00100000,
             'classic2m': 0x00040000,
             'plus1m': 0x00040000, 'plus1mjit': 0x0003C000}[
    (os.environ.get('MODUS_MAC68K_MACHINE') or 'lc').lower()]
# MODUS_MAC68K_SHIFT (boot-mac68k): the whole map moved
LOAD_ADDR += int(os.environ.get('MODUS_MAC68K_SHIFT') or '0', 16)


def bootblocks():
    with tempfile.TemporaryDirectory() as d:
        o, b = os.path.join(d, 'b.o'), os.path.join(d, 'b.bin')
        subprocess.run(['m68k-linux-gnu-as', '-m68000', '-o', o,
                        kernel_source('mac68k-bootblocks.s')], check=True)
        subprocess.run(['m68k-linux-gnu-objcopy', '-O', 'binary', o, b], check=True)
        syms = {}
        for line in subprocess.run(['m68k-linux-gnu-nm', o], check=True,
                                   capture_output=True, text=True).stdout.splitlines():
            addr, _, name = line.split()
            syms[name] = int(addr, 16)
        return bytearray(open(b, 'rb').read()), syms


SPLASH = os.path.join(HERE, '..', 'boot', 'mac68k-splash.pbm')


def packbits(data):
    """Apple PackBits (TN1023): a header byte n; 0..127, n+1 literal bytes
    follow; -1..-127, the next byte 1-n times."""
    out, i, n = bytearray(), 0, len(data)
    while i < n:
        j = i
        while j + 1 < n and data[j + 1] == data[i] and j - i < 127:
            j += 1
        if j - i + 1 >= 3:
            out += bytes([(257 - (j - i + 1)) & 0xFF, data[i]])
            i = j + 1
            continue
        k = i
        while k < n and k - i < 128 and not (k + 2 < n and data[k] == data[k + 1] == data[k + 2]):
            k += 1
        out += bytes([k - i - 1]) + data[i:k]
        i = k
    return bytes(out)


def unpackbits(data, size):
    out, i = bytearray(), 0
    while len(out) < size:
        h = data[i]
        i += 1
        if h < 128:
            out += data[i:i + h + 1]
            i += h + 1
        elif h > 128:
            out += bytes([data[i]]) * (257 - h)
            i += 1
    return bytes(out[:size])


def splash(path=None):
    """The boot blocks' splash (mac68k-bootblocks.s): a 512 x 342 PBM's middle,
    the box of bytes where it differs from the 50% grey the boot blocks draw
    first -- X0 (bytes), Y0, W (bytes), H as words, then the box XORed with
    itself two rows up and PackBits'd.  None when there is no picture or
    MAC68K_SPLASH=0."""
    path = path or SPLASH
    if os.environ.get("MAC68K_SPLASH") == "0" or not os.path.exists(path):
        return None
    pbm = open(path, 'rb').read()
    fields, pos = [], 0
    while len(fields) < 3:                   # P4, width, height (and # comments)
        while pbm[pos:pos + 1].isspace():
            pos += 1
        if pbm[pos:pos + 1] == b'#':
            pos = pbm.index(b'\n', pos)
            continue
        end = pos
        while not pbm[end:end + 1].isspace():
            end += 1
        fields.append(pbm[pos:end])
        pos = end
    if fields != [b'P4', b'512', b'342']:
        sys.exit('mac68k-disk: %s is not a 512 x 342 binary PBM' % path)
    screen = pbm[pos + 1:pos + 1 + 64 * 342]
    grey = lambda y: 0xAA if y % 2 == 0 else 0x55
    box = [(x, y) for y in range(342) for x in range(64) if screen[64 * y + x] != grey(y)]
    if not box:
        return None
    x0, x1 = min(b[0] for b in box), max(b[0] for b in box)
    y0, y1 = min(b[1] for b in box), max(b[1] for b in box)
    w, h = x1 - x0 + 1, y1 - y0 + 1
    rows = b''.join(screen[64 * y + x0:64 * y + x0 + w] for y in range(y0, y1 + 1))
    delta = bytes(rows[i] ^ (rows[i - 2 * w] if i >= 2 * w else 0) for i in range(len(rows)))
    packed = packbits(delta)
    if unpackbits(packed, len(delta)) != delta:
        sys.exit('mac68k-disk: the splash does not unpack to itself')
    data = struct.pack('>HHHH', x0, y0, w, h) + packed
    if len(data) > 0x2000:                   # unpacked at DEST+#x2000
        sys.exit('mac68k-disk: the splash packs to %d bytes, over 8 KB' % len(data))
    return data


def write_disk(img, disk, load_addr=LOAD_ADDR, min_size=819200, entry=None, read_len=None,
               refslot=0, memtop=0, kmap=0):
    """Boot blocks that read IMG (bytes) to LOAD_ADDR and jump to ENTRY (default
    LOAD_ADDR), then IMG at offset 1024, padded to whole blocks and to at
    least MIN_SIZE.  The splash, when there is one, goes after IMG.  REFSLOT
    and MEMTOP, for an image that can be launched from Mac OS: where it wants
    the open file's reference number, and the RAM its map needs."""
    bb, syms = bootblocks()
    if syms['start'] != 0x8A:
        sys.exit('mac68k-disk: the loader is at %#x, not 0x8A' % syms['start'])
    if len(bb) > 1024:
        sys.exit('mac68k-disk: boot blocks are %d bytes' % len(bb))
    struct.pack_into('>I', bb, syms['dest'], load_addr)
    struct.pack_into('>I', bb, syms['entry'], load_addr if entry is None else entry)
    # a disk driver reads whole 512-byte blocks (a byte count is paramErr, -50)
    struct.pack_into('>I', bb, syms['len'], ((read_len or len(img)) + 511) & ~511)
    struct.pack_into('>I', bb, syms['off'], 1024)
    # for a launch from Mac OS (mac68k-chain.py): 0 = this image cannot
    struct.pack_into('>I', bb, syms['refslot'], refslot)
    struct.pack_into('>I', bb, syms['memtop'], memtop)
    # the kind map's address, when the layout puts the young heap last: the
    # application's loader then sizes the heap from its partition
    struct.pack_into('>I', bb, syms['kmap'], kmap)
    out = bb + bytes(1024 - len(bb)) + img
    out += bytes(-len(out) % 512)
    sp = splash()
    if sp:
        struct.pack_into('>I', out, syms['spoff'], len(out))
        struct.pack_into('>I', out, syms['splen'], (len(sp) + 511) & ~511)
        out += sp + bytes(-len(sp) % 512)
    out += bytes(max(0, min_size - len(out)))
    open(disk, 'wb').write(out)
    return len(out)


def paged(img):
    """A paged image (MODUS_68K_PAGED): its stream table marked NOT LOADED
    (bit 0 of every entry), and how many bytes the boot blocks must read --
    the resident part, up to the stream section.  The streams stay on the
    disk after it; the image's loader reads them when they are called."""
    k = img.find(b'modusPGD')
    if k < 0 or img.find(b'modusPGD', k + 1) >= 0:
        sys.exit('mac68k-disk: --paged needs one modusPGD descriptor in the image')
    table, count, section = struct.unpack('>III', img[k + 8:k + 20])
    img = bytearray(img)
    t = table - LOAD_ADDR
    for i in range(count):
        v, = struct.unpack_from('>I', img, t + 4 * i)
        struct.pack_into('>I', img, t + 4 * i, v | 1)
    return bytes(img), section - LOAD_ADDR, count


def main(image, disk, *opts):
    img = open(image, 'rb').read()
    if '--paged' in opts:
        img, resident, count = paged(img)
        n = write_disk(img, disk, read_len=resident)
        print('mac68k-disk: %s paged: %d resident bytes boot, %d streams (%d bytes) on demand'
              % (image, resident, count, len(img) - resident))
        return
    n = write_disk(img, disk)
    print('mac68k-disk: %s (%d bytes at %#x) -> %s, %d bytes'
          % (image, len(img), LOAD_ADDR, disk, n))


if __name__ == '__main__':
    main(*sys.argv[1:])
