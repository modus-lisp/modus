#!/usr/bin/env python3
"""tree.native.py RUNDIR [T0 T1] -- lisp.lua PROF samples (RUNDIR/prof.txt, symbol
map RUNDIR/cl.map, linked at #x600000) as a call tree from MVM-EVAL down, and self
time.  Without T0/T1: the span where the PC is in the image.  Samples inside the
REPL's reader (MAME still typing) are dropped."""
import bisect, collections, sys
R = sys.argv[1].rstrip('/') + '/'
rows = []
for l in open(R + 'cl.map'):
    if l.startswith('#'):
        continue
    p = l.rstrip('\n').split('\t'); rows.append((int(p[0], 16), int(p[1]), p[3]))
rows.sort(); st = [r[0] for r in rows]
def nm(a):
    i = bisect.bisect_right(st, a) - 1
    return rows[i][2] if i >= 0 and a < rows[i][0] + rows[i][1] + 16 else None
samples = []
d = 0
for l in open(R + 'prof.txt'):
    q = l.split()
    if q[0] == 'BASE':
        d = int(q[1], 16) - 0x600000; continue
    samples.append((float(q[0]), [int(x, 16) - d for x in q[1:] if '=' not in x]))
if len(sys.argv) > 3:
    t0, t1 = float(sys.argv[2]), float(sys.argv[3])
else:
    busy = [t for t, a in samples if rows[0][0] <= a[0] < rows[-1][0] + rows[-1][1]]
    t0, t1 = min(busy), max(busy)
tree, n, selfc = {}, 0, collections.Counter()
for t, addrs in samples:
    if not t0 <= t <= t1:
        continue
    fs = [nm(a) for a in addrs]
    leaf = fs[0] or '<outside>'
    fs = [f for f in fs if f]
    if '%SREPL-READ-FORM-TEXT' in fs or 'MVM-EVAL' not in fs:
        continue
    n += 1; selfc[leaf] += 1
    path = []
    for f in reversed(fs):
        if not path or path[-1] != f:
            path.append(f)
    node = tree
    for f in path[path.index('MVM-EVAL'):]:
        node = node.setdefault(f, [0, {}]); node[0] += 1; node = node[1]
print('%.1f..%.1f s: %d samples' % (t0, t1, n))
def walk(t, dep):
    for f, (c, ch) in sorted(t.items(), key=lambda x: -x[1][0]):
        if 100 * c / n >= 3 and dep < 40:
            print('  ' * dep + '%4.1f%% %s' % (100 * c / n, f)); walk(ch, dep + 1)
walk(tree, 0)
print('self:', ', '.join('%s %.1f%%' % (f, 100 * c / n) for f, c in selfc.most_common(14)))
