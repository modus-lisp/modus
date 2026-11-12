#!/usr/bin/env python3
"""mac68k-floppy.py IMAGE DISK [--hd] [--loader LOADER] -- compressed Mac boot disks.

IMAGE is a flat bare-Mac image (built for :mac68k, linked at #x00100000).
It is compressed as a raw LZMA1 stream (scripts/68k-floppy-pack.py's settings)
behind a loader -- mvm/mac68k-unlzma.lisp with mvm/unlzma-core.lisp, built
here for :mac68k linked at #x01200000 unless --loader names one already built.
The boot blocks (scripts/mac68k-disk.py) read the blob to #x01200000; the
loader unpacks IMAGE to #x00100000 and jumps to it.

Disks: one 800 KB floppy, DISK, if the blob fits; else, with --hd, one
1.44 MB floppy; else TWO 800 KB floppies, DISK-1 and DISK-2.  A two-disk set
boots from DISK-1, whose boot blocks enter stage 2 (scripts/mac68k-disk2.s)
instead of the loader: it finds DISK-2 in any drive -- or ejects DISK-1 and
asks for it on screen -- and reads its share of the stream in after DISK-1's.
DISK-2's block 0 is a label: "MODUS-D2", the set's id, the data length.
"""
import lzma
import os
import re
import struct
import subprocess
import sys
import tempfile
import zlib

sys.dont_write_bytecode = True   # importing mac68k-disk.py leaves no __pycache__

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
LOADER_ADDR = 0x01200000        # mvm/mac68k-unlzma.lisp +LOADER+
PAY_OFF = 0x4000                # +PAY+ - +LOADER+
OUT = 0x00100000                # +OUT+
STATE = 0x01A00000              # +ST+: the blob must end below it
FLOPPY, HD = 819200, 1474560
FILTERS = [{'id': lzma.FILTER_LZMA1, 'preset': 9 | lzma.PRESET_EXTREME,
            'lc': 2, 'lp': 1, 'pb': 2, 'dict_size': 1 << 23, 'nice_len': 273}]
# the jumper, at +PAY+ + 8:  CPUSHA BC; MOVEA.L #OUT,A0; JMP (A0); NOP (pad)
JUMPER = struct.pack('>HHIHH', 0xF4F8, 0x207C, OUT, 0x4ED0, 0x4E71)
MESSAGE = 'INSERT MODUS DISK 2'


def build_loader(out):
    env = dict(os.environ, MODUS_MAC68K_LOAD_ADDR='#x%08X' % LOADER_ADDR,
               MODUS_MAC68K_OUT=out)
    r = subprocess.run(['sbcl', '--script', 'mvm/build-hosted.lisp', 'mac68k',
                        'mvm/mac68k-unlzma.lisp', 'mvm/unlzma-core.lisp'],
                       cwd=ROOT, env=env, capture_output=True, text=True)
    if r.returncode or not os.path.exists(out):
        sys.stderr.write(r.stdout[-4000:] + r.stderr[-4000:])
        sys.exit('mac68k-floppy: the loader build failed')


def font():
    """The 8x8 font the consoles draw in: boot/boot-uefi-x64.lisp, 95 glyphs."""
    s = open(os.path.join(ROOT, 'boot', 'boot-uefi-x64.lisp')).read()
    s = s[s.index('(defvar *font-8x8-qwords*'):]
    rows = re.findall(r'\(((?:#x[0-9A-Fa-f]{2}\s*){8})\)', s)[:95]
    assert len(rows) == 95
    return [bytes(int(x[2:], 16) for x in r.split()) for r in rows]


def stage2():
    """scripts/mac68k-disk2.s assembled: (code, symbol offsets)."""
    with tempfile.TemporaryDirectory() as d:
        o, b = os.path.join(d, 's.o'), os.path.join(d, 's.bin')
        subprocess.run(['m68k-linux-gnu-as', '-m68000', '-o', o,
                        os.path.join(HERE, 'mac68k-disk2.s')], check=True)
        subprocess.run(['m68k-linux-gnu-objcopy', '-O', 'binary', o, b], check=True)
        syms = {}
        for line in subprocess.run(['m68k-linux-gnu-nm', o], check=True,
                                   capture_output=True, text=True).stdout.splitlines():
            addr, _, name = line.split()
            syms[name] = int(addr, 16)
        return bytearray(open(b, 'rb').read()), syms


def disk_module():
    import importlib.util
    spec = importlib.util.spec_from_file_location('mac68k_disk', os.path.join(HERE, 'mac68k-disk.py'))
    md = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(md)
    return md


def main(image, disk, hd=False, loader=None):
    if loader is None:
        loader = disk + '.loader'
        build_loader(loader)
    lo = open(loader, 'rb').read()
    if len(lo) > PAY_OFF:
        sys.exit('mac68k-floppy: the loader is %d bytes, over %d' % (len(lo), PAY_OFF))
    img = open(image, 'rb').read()
    if OUT + len(img) > LOADER_ADDR:
        sys.exit('mac68k-floppy: the image runs into the loader')
    stream = lzma.compress(img, format=lzma.FORMAT_RAW, filters=FILTERS)
    md = disk_module()
    cap1 = FLOPPY - 1024                # disk 1 after its boot blocks

    def blob(code):
        head = struct.pack('>HHHH', len(img) >> 16, len(img) & 0xFFFF, 0,
                           8 + len(JUMPER) + len(code))
        b = lo + bytes(PAY_OFF - len(lo)) + head + JUMPER + bytes(code) + stream
        if LOADER_ADDR + len(b) > STATE:
            sys.exit('mac68k-floppy: loader + payload run into the coder state')
        return b

    single = blob(b'')
    if 1024 + len(single) <= FLOPPY or hd:
        n = md.write_disk(single, disk, LOADER_ADDR, FLOPPY if 1024 + len(single) <= FLOPPY else HD)
        if n > HD:
            sys.exit('mac68k-floppy: %d bytes do not fit a 1.44 MB floppy' % n)
        print('mac68k-floppy: %s, %d bytes -> %d LZMA + %d loader -> %s, %d bytes (%s)'
              % (image, len(img), len(stream), len(lo), disk, n,
                 '800K floppy' if n == FLOPPY else '1.44 MB floppy'))
        return

    code, syms = stage2()
    b = blob(code)
    part1, part2 = b[:cap1], b[cap1:]
    len2 = (len(part2) + 511) & ~511
    if 512 + len2 > FLOPPY:
        sys.exit('mac68k-floppy: %d bytes do not fit two 800 KB floppies' % (1024 + len(b)))
    setid = zlib.crc32(stream) or 1
    glyphs = font()
    msg = b''.join(glyphs[ord(c) - 32] for c in MESSAGE)
    for name, val in (('loader', LOADER_ADDR), ('dest', LOADER_ADDR + len(part1)),
                      ('len', len2), ('setid', setid)):
        struct.pack_into('>I', code, syms[name], val)
    struct.pack_into('>H', code, syms['msgn'], len(MESSAGE))
    code[syms['msgbuf']:syms['msgbuf'] + len(msg)] = msg
    b = blob(code)                      # the same length, now patched
    part1, part2 = b[:cap1], b[cap1:]
    s2 = LOADER_ADDR + PAY_OFF + 8 + len(JUMPER)
    d1, d2 = disk + '-1', disk + '-2'
    md.write_disk(part1, d1, LOADER_ADDR, FLOPPY, entry=s2)
    label = b'MODUS-D2' + struct.pack('>II', setid, len(part2))
    out2 = label + bytes(512 - len(label)) + part2
    open(d2, 'wb').write(out2 + bytes(FLOPPY - len(out2)))
    print('mac68k-floppy: %s, %d bytes -> %d LZMA + %d loader -> %s (%d bytes) + %s (%d bytes), '
          'two 800K floppies, %d bytes to spare on the second'
          % (image, len(img), len(stream), len(lo), d1, len(part1), d2, len(part2),
             FLOPPY - 512 - len2))


if __name__ == '__main__':
    args = sys.argv[1:]
    hd = '--hd' in args
    args = [a for a in args if a != '--hd']
    loader = None
    if '--loader' in args:
        i = args.index('--loader')
        loader = args[i + 1]
        del args[i:i + 2]
    main(args[0], args[1], hd, loader)
