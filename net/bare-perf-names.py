#!/usr/bin/env python3
"""bare-perf-names.py SYMMAP BASE OUT.lisp -- the kernel's names for a running image.

SYMMAP is the build's MODUS_SYMMAP file (virtual-addr size native-offset name), BASE (hex) what
is added to a virtual address where the kernel runs (the Pi board image: 2FD80000).  OUT is a few
forms -- (perf-image-names BASE :begin), chunks of #(va size "name" ...), :end -- to send to the
image after bare-perf.lisp, a form at a time; from then on PERF-TOP / ALLOC-TOP / CPU-TOP name
kernel code exactly."""
import sys
symmap, base, out = sys.argv[1], int(sys.argv[2], 16), sys.argv[3]
parts = []
for line in open(symmap):
    if line.startswith('#') or not line.strip():
        continue
    va, size, _off, name = line.rstrip('\n').split('\t', 3)
    name = name.replace('\\', '\\\\').replace('"', '\\"')
    parts.append('#x%s %d "%s"' % (va.lstrip('0') or '0', int(size), name))
with open(out, 'w') as f:
    f.write('(perf-image-names #x%X :begin)\n' % base)
    for c in range(0, len(parts), 400):
        f.write('(perf-image-names #x%X #(%s))\n' % (base, ' '.join(parts[c:c + 400])))
    f.write('(perf-image-names #x%X :end)\n' % base)
print(len(parts), 'names ->', out)
