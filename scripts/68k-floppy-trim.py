#!/usr/bin/env python3
"""68k-floppy-trim.py IMAGE SYMMAP EXEC-LOG OUT -- EXPERIMENTAL: blank what a
68k core image never needs again, so it compresses smaller.

A core image (MODUS_68K_CORE) starts from its saved heap and never runs boot
init again, but it still carries boot's code and the name tables boot read.
This fills, in a copy of IMAGE:
  - functions boot init alone runs (%INIT-* / INIT-* / %SEED-* / ...-AUTO-N)
    and the JIT runtime (%JIT-*, %VLANE-*: x64/aarch64 only), with ILLEGAL
    (#x4AFC) -- EXCEPT any function EXEC-LOG shows executing, nested lambdas
    ($$ in the name: an init function's lambdas are live method bodies), and
    the last function in SYMMAP (its range runs on into the collector and the
    shared helpers);
  - printable text runs of 200+ bytes in the first segment that hold no "("
    (the packed name tables), with zeros.  Lisp source runs are kept: the
    SB-* / ASDF / Genera shims are evaluated lazily, at first use.

EXEC-LOG lists the start address (0x...) of every block qemu translated over a
representative workload, one per line, e.g. collected with
`qemu-m68k-static -d in_asm -D LOG IMAGE ...; grep -oE '^0x[0-9a-f]+' LOG`.

THIS IS A POST-LINK HACK.  "Never executed in the workload" is not proof, and
two traps showed it: %INIT-SIGNAL-HANDLING runs at every start, and lambdas
defined in %INIT-CLOS-PROTOCOL are live methods.  The real version is a
build-time shake from the core-boot path.  Validate the result (validate.sh,
DESCRIBE / APROPOS / DOCUMENTATION) against the untrimmed image.
"""
import bisect
import re
import struct
import sys

BOOT_ONLY = re.compile(r'^(%?INIT-|%SEED-|%JIT-|%VLANE-)|AUTO-[0-9]+$')


def main(image, symmap, exec_log, out):
    img = bytearray(open(image, 'rb').read())
    phoff, = struct.unpack('>I', img[0x1C:0x20])
    phnum, = struct.unpack('>H', img[0x2C:0x2E])
    segs = []
    for i in range(phnum):
        t, off, va, pa, fs, ms, fl, al = struct.unpack('>8I', img[phoff + 32 * i:phoff + 32 * i + 32])
        if t == 1:
            segs.append((va, off, fs))

    def file_off(a):
        for va, off, fs in segs:
            if va <= a < va + fs:
                return off + a - va
        return None

    rows = []
    for line in open(symmap):
        if line.startswith('#'):
            continue
        p = line.rstrip('\n').split('\t')
        if len(p) >= 4:
            rows.append((int(p[0], 16), int(p[1]), p[3]))
    rows.sort()
    last = rows[-1][0]
    hits = sorted(int(l, 16) for l in open(exec_log) if l.strip())

    def executed(a, s):
        k = bisect.bisect_left(hits, a)
        return k < len(hits) and hits[k] < a + s

    code = 0
    for a, s, name in rows:
        if a == last or '$$' in name or not BOOT_ONLY.search(name) or executed(a, s):
            continue
        o = file_off(a)
        if o is not None:
            img[o:o + s] = b'\x4a\xfc' * (s // 2) + b'\x4a' * (s % 2)
            code += s

    va, off, fs = segs[0]
    text = 0
    for m in list(re.finditer(rb'[\x09\x0a\x20-\x7e]{200,}', bytes(img[off:off + fs]))):
        if b'(' not in m.group():
            img[off + m.start():off + m.end()] = bytes(m.end() - m.start())
            text += m.end() - m.start()
    open(out, 'wb').write(img)
    print('68k-floppy-trim: %d bytes of code blanked, %d bytes of name tables zeroed -> %s'
          % (code, text, out))


if __name__ == '__main__':
    main(*sys.argv[1:5])
