#!/usr/bin/env python3
"""mac68k-hd.py VOLUME OUT [--chd CHDMAN] -- a SCSI hard disk that boots a 68000 Mac.

VOLUME is a modus boot volume (scripts/mac68k-disk.py, mac68k-floppy.py or
mac68k-core.py output: boot blocks at byte 0).  OUT is a raw disk image --
block 0 the DRIVER DESCRIPTOR ('ER', one driver of type 1), a partition map in
blocks 1-3, the driver (scripts/mac68k-scsi.s) from block 4, VOLUME from block
BASE (64) -- and,
with --chd, OUT.chd for MAME (`chdman createhd`).  The ROM loads the driver
and enters it; the driver adds a drive that is VOLUME, and the ROM boots
its boot blocks.
"""
import os, struct, subprocess, sys, tempfile

sys.dont_write_bytecode = True
HERE = os.path.dirname(os.path.abspath(__file__))
BASE = 64
MAPN = 3        # partition map entries: blocks 1..3
DRV = 1 + MAPN  # the driver's first block


def driver_checksum(code):
    """pmBootCksum: each byte added, the 16-bit sum rotated left one bit, 0 read as
    #xFFFF -- what the ROM compares before it trusts a driver (Apple's pdisk)."""
    s = 0
    for b in code:
        s = (s + b) & 0xFFFF
        s = ((s << 1) | (s >> 15)) & 0xFFFF
    return s or 0xFFFF


def driver():
    with tempfile.TemporaryDirectory() as d:
        o, b = os.path.join(d, 's.o'), os.path.join(d, 's.bin')
        subprocess.run(['m68k-linux-gnu-as', '-m68000', '-o', o,
                        os.path.join(HERE, 'mac68k-scsi.s')], check=True)
        subprocess.run(['m68k-linux-gnu-objcopy', '-O', 'binary', o, b], check=True)
        syms = {}
        for line in subprocess.run(['m68k-linux-gnu-nm', o], check=True,
                                   capture_output=True, text=True).stdout.splitlines():
            addr, _, name = line.split()
            syms[name] = int(addr, 16)
        return bytearray(open(b, 'rb').read()), syms


def main(volume, out, chdman=None):
    vol = open(volume, 'rb').read()
    vol += bytes(-len(vol) % 512)
    volblocks = len(vol) // 512
    drv, syms = driver()
    struct.pack_into('>I', drv, syms['base'], BASE)
    struct.pack_into('>I', drv, syms['volblocks'], volblocks)
    drv += bytes(-len(drv) % 512)
    nd = len(drv) // 512
    if DRV + nd > BASE:
        sys.exit('mac68k-hd: the driver (%d blocks) runs into the volume' % nd)
    total = BASE + volblocks
    total += -total % 64                     # whole cylinders for chdman's geometry
    ddr = struct.pack('>HHIHHIH', 0x4552, 512, total, 1, 1, 0, 1)   # 'ER' ... 1 driver
    ddr += struct.pack('>IHH', DRV, nd, 1)   # ddBlock, ddSize, ddType (Mac OS)
    disk = bytearray(total * 512)
    disk[:len(ddr)] = ddr
    # THE PARTITION MAP, as Apple's own disks have one.  Without a map a Plus, a
    # Classic, an SE/30 and a IIci boot from the driver descriptor alone (MAME).
    # With one the Classic's ROM reads it, and boots only when the volume's entry
    # is Apple_HFS: an Apple_Free volume showed the no-startup-disk icon.
    parts = [(1, MAPN, 'Apple', 'Apple_partition_map', 0x37, None),
             (DRV, nd, 'Macintosh', 'Apple_Driver43', 0x37F, drv),
             (BASE, total - BASE, 'Modus', 'Apple_HFS', 0x37, None)]
    for i, (start, count, name, kind, status, code) in enumerate(parts):
        pm = bytearray(512)
        struct.pack_into('>HHIII', pm, 0, 0x504D, 0, len(parts), start, count)
        pm[16:16 + len(name)] = name.encode()
        pm[48:48 + len(kind)] = kind.encode()
        struct.pack_into('>III', pm, 80, 0, count, status)    # pmLgDataStart, pmDataCnt, pmPartStatus
        if code is not None:
            struct.pack_into('>IIIIIII', pm, 92, 0, len(code), 0, 0, 0, 0, driver_checksum(code))
            pm[120:125] = b'68000'
        disk[(1 + i) * 512:(2 + i) * 512] = pm
    disk[DRV * 512:DRV * 512 + len(drv)] = drv
    disk[BASE * 512:BASE * 512 + len(vol)] = vol
    open(out, 'wb').write(disk)
    print('mac68k-hd: %s: partition map at 1, driver %d blocks at %d, volume %s (%d blocks) at %d, %d blocks'
          % (out, nd, DRV, volume, volblocks, BASE, total))
    if chdman:
        chd = out + '.chd'
        if os.path.exists(chd):
            os.unlink(chd)
        r = subprocess.run(chdman.split() + ['createhd', '-i', out, '-o', chd, '-c', 'none',
                                             '-chs', '%d,4,16' % (total // 64), '-ss', '512'],
                           capture_output=True, text=True)
        if r.returncode:
            sys.exit('mac68k-hd: chdman: ' + (r.stderr or r.stdout)[-600:])
        print('mac68k-hd: %s' % chd)


if __name__ == '__main__':
    args = sys.argv[1:]
    chd = None
    if '--chd' in args:
        i = args.index('--chd'); chd = args[i + 1]; del args[i:i + 2]
    main(args[0], args[1], chd)
