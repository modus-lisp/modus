#!/usr/bin/env python3
"""mac68k-floppy.py IMAGE DISK [LOADER] -- a compressed Mac boot disk.

IMAGE is a flat bare-Mac image (built for :mac68k, linked at #x00100000).
It is compressed as a raw LZMA1 stream (scripts/68k-floppy-pack.py's settings)
behind a loader -- mvm/mac68k-unlzma.lisp with mvm/unlzma-core.lisp, built
here for :mac68k linked at #x01200000 unless LOADER names one already built --
and written as a boot disk (scripts/mac68k-disk.py): the boot blocks read
loader + payload to #x01200000, the loader unpacks IMAGE to #x00100000 and
jumps to it.  The disk is padded to an 800 KB floppy if it fits, else to a
1.44 MB one (an LC 475's SuperDrive reads both), else left at its size.
"""
import importlib.util
import lzma
import os
import struct
import subprocess
import sys

sys.dont_write_bytecode = True   # importing mac68k-disk.py leaves no __pycache__

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
LOADER_ADDR = 0x01200000        # mvm/mac68k-unlzma.lisp +LOADER+
PAY_OFF = 0x10000               # +PAY+ - +LOADER+
OUT = 0x00100000                # +OUT+
STATE = 0x01A00000              # +ST+: the payload must end below it
FILTERS = [{'id': lzma.FILTER_LZMA1, 'preset': 9 | lzma.PRESET_EXTREME,
            'lc': 2, 'lp': 1, 'pb': 2, 'dict_size': 1 << 23, 'nice_len': 273}]
# the jumper, at +PAY+ + 8:  CPUSHA BC; MOVEA.L #OUT,A0; JMP (A0)
JUMPER = struct.pack('>HHIH', 0xF4F8, 0x207C, OUT, 0x4ED0)


def build_loader(out):
    env = dict(os.environ, MODUS_MAC68K_LOAD_ADDR='#x%08X' % LOADER_ADDR,
               MODUS_MAC68K_OUT=out)
    r = subprocess.run(['sbcl', '--script', 'mvm/build-hosted.lisp', 'mac68k',
                        'mvm/mac68k-unlzma.lisp', 'mvm/unlzma-core.lisp'],
                       cwd=ROOT, env=env, capture_output=True, text=True)
    if r.returncode or not os.path.exists(out):
        sys.stderr.write(r.stdout[-4000:] + r.stderr[-4000:])
        sys.exit('mac68k-floppy: the loader build failed')


def main(image, disk, loader=None):
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
    head = struct.pack('>HHHH', len(img) >> 16, len(img) & 0xFFFF, 0, 8 + len(JUMPER))
    blob = lo + bytes(PAY_OFF - len(lo)) + head + JUMPER + stream
    if LOADER_ADDR + len(blob) > STATE:
        sys.exit('mac68k-floppy: loader + payload run into the coder state')
    spec = importlib.util.spec_from_file_location('mac68k_disk', os.path.join(HERE, 'mac68k-disk.py'))
    md = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(md)
    need = 1024 + len(blob)
    size = 819200 if need <= 819200 else 1474560 if need <= 1474560 else 0
    n = md.write_disk(blob, disk, LOADER_ADDR, size)
    print('mac68k-floppy: %s, %d bytes -> %d LZMA + %d loader -> %s, %d bytes (%s)'
          % (image, len(img), len(stream), len(lo), disk, n,
             '800K floppy' if n == 819200 else '1.44 MB floppy' if n == 1474560
             else 'NOT a floppy'))


if __name__ == '__main__':
    main(*sys.argv[1:4])
