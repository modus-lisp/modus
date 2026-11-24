#!/usr/bin/env python3
"""Make a PowerPC Mac disk whose boot replays its payload's fasls.

  fasl-record.py IMAGE.elf PAYLOAD.list OUT.dsk [--dir FASLDIR]

A fasl replays only in the image that recorded it (mvm/fasl.lisp: the key
holds the build), so the image records them itself, once, under QEMU:

  1. a disk with --fasl-record: the boot compiles the payload as always and
     sends each file's fasl on the serial port (FASL key length / hex / FASL-END;
     build-cl-repl-common.lisp %PPCMAC-FASL-DUMP);
  2. those become FASLDIR/KEY.mfasl;
  3. OUT.dsk is built with --fasls FASLDIR: the fasls ride in the payload ahead
     of the sources, and every boot of IMAGE replays instead of compiling.

Rebuild IMAGE and the fasls must be recorded again (their keys will not match:
they are skipped, and the boot compiles, as before).  Needs ppcmac-disk.lisp's
environment (CRAM, FS_APPLE) and qemu-system-ppc; macrun.py does the boot.
"""
import os, re, subprocess, sys, tempfile

def main():
    image, payload, out = sys.argv[1:4]
    here = os.path.dirname(os.path.abspath(__file__))
    disk_lisp = os.path.join(here, '..', 'ppcmac-disk.lisp')
    fdir = sys.argv[sys.argv.index('--dir') + 1] if '--dir' in sys.argv else out + '.fasls'
    os.makedirs(fdir, exist_ok=True)
    work = tempfile.mkdtemp(prefix='fasl-record-')
    rec_disk, log, script = (os.path.join(work, n) for n in ('record.dsk', 'boot.log', 'none.script'))
    open(script, 'w').close()
    subprocess.run(['sbcl', '--script', disk_lisp, image, rec_disk, '--payload', payload, '--fasl-record'], check=True)
    subprocess.run([sys.executable, os.path.join(here, 'macrun.py'), rec_disk, script, '--log', log], check=True)
    text = open(log, encoding='latin-1').read()
    n = 0
    for m in re.finditer(r'FASL (\S+) (\d+)\n(.*?)\nFASL-END', text, re.S):
        key, length, hexs = m.group(1), int(m.group(2)), re.sub(r'\s', '', m.group(3))
        data = bytes.fromhex(hexs)
        if len(data) != length:
            print(f'fasl-record: {key}: {len(data)} bytes, expected {length}: skipped', file=sys.stderr)
            continue
        with open(os.path.join(fdir, key + '.mfasl'), 'wb') as f:
            f.write(data)
        n += 1
    print(f'fasl-record: {n} fasls in {fdir}')
    subprocess.run(['sbcl', '--script', disk_lisp, image, out, '--payload', payload, '--fasls', fdir], check=True)

if __name__ == '__main__':
    main()
