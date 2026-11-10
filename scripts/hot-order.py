#!/usr/bin/env python3
"""hot-order.py -- a boot profile for MODUS_HOT_ORDER.

    scripts/hot-order.py SYMMAP INASM-LOG > hot.txt

SYMMAP is the image's MODUS_SYMMAP file; INASM-LOG is what qemu-user's
`-d in_asm -D FILE` logged while that image ran.  Prints every function that
executed, one name a line, in first-execution order.  Rebuild the image with
MODUS_HOT_ORDER=hot.txt (and the same MODUS_LEAN_BOOT flags) and those
functions are laid out first, so the pages boot touches hold code that runs.

For example, on 68k:

    MODUS_SYMMAP=/tmp/img.sym ... sbcl --script mvm/build-68k-cli.lisp
    qemu-m68k-static -cpu m68040 -d in_asm -D /tmp/inasm.log /tmp/modus-68k-cli
    scripts/hot-order.py /tmp/img.sym /tmp/inasm.log > /tmp/hot.txt
"""
import bisect
import re
import sys


def main(symmap, inasm):
    syms = []
    with open(symmap) as f:
        for line in f:
            if line.startswith('#'):
                continue
            va, size, _off, name = line.rstrip('\n').split('\t', 3)
            syms.append((int(va, 16), int(size), name))
    syms.sort()
    starts = [s[0] for s in syms]
    seen = set()
    block = re.compile(r'^0x([0-9a-f]+):')
    with open(inasm, errors='replace') as f:
        for line in f:
            m = block.match(line)
            if not m:
                continue
            addr = int(m.group(1), 16)
            i = bisect.bisect_right(starts, addr) - 1
            if i >= 0 and addr < syms[i][0] + syms[i][1]:
                name = syms[i][2]
                if name not in seen:
                    seen.add(name)
                    print(name)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2])
