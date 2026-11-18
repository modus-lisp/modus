#!/usr/bin/env python3
"""native.py PROF.txt CL.map T0 T1 [LOAD] [TOP] -- a profile of the native
(quadra) image from lisp.lua's PROF samples between emulated seconds T0 and
T1 (env EXCLUDE=NAME drops samples with NAME on the stack -- the REPL's
reader, while MAME is still typing the form): self and inclusive time per function, the PC and the A6 frame chain's
return addresses mapped through the build's symbol map (CL.map, linked at
LOAD, default #x600000; the samples' first line, "BASE b", is where the
relocating load put it).  Samples whose PC is outside the image (the Toolbox,
the ROM, the event loop) count as <outside>."""
import bisect
import os
import collections
import sys

prof, cmap, t0, t1 = sys.argv[1], sys.argv[2], float(sys.argv[3]), float(sys.argv[4])
load = int(sys.argv[5], 16) if len(sys.argv) > 5 else 0x600000
top = int(sys.argv[6]) if len(sys.argv) > 6 else 40
starts, ends, names = [], [], []
for l in open(cmap):
    if l.startswith('#'):
        continue
    p = l.rstrip('\n').split('\t')
    if len(p) < 4:
        continue
    starts.append(int(p[0], 16)); ends.append(int(p[0], 16) + int(p[1])); names.append(p[3])
order = sorted(range(len(starts)), key=lambda i: starts[i])
starts = [starts[i] for i in order]; ends = [ends[i] for i in order]; names = [names[i] for i in order]


def name(a):
    i = bisect.bisect_right(starts, a) - 1
    if i >= 0 and a < ends[i] + 16:
        return names[i]
    return None


delta = 0
selfc, incl, edges = collections.Counter(), collections.Counter(), collections.Counter()
icallers = collections.Counter()
n = 0
for l in open(prof):
    q = l.split()
    if q[0] == 'BASE':
        delta = int(q[1], 16) - load
        continue
    t = float(q[0])
    if not (t0 <= t < t1):
        continue
    n += 1
    addrs = [int(x, 16) - delta for x in q[1:] if '=' not in x]
    fs = [name(a) for a in addrs]
    if os.environ.get('EXCLUDE') in fs:
        continue
    leaf = fs[0] or '<outside>'
    selfc[leaf] += 1
    chain = [f for f in fs if f]
    for f in set(chain) | {leaf}:
        incl[f] += 1
    if len(chain) >= 2:
        edges[(chain[1], chain[0])] += 1
    # inclusive callers: the frame above NAME's outermost occurrence
    w = os.environ.get('CALLERS')
    if w and w in chain:
        k = len(chain) - 1 - chain[::-1].index(w)
        icallers[chain[k + 1] if k + 1 < len(chain) else '<top>'] += 1
print('%d samples, %.1f s' % (n, t1 - t0))
print('--- self')
for f, c in selfc.most_common(top):
    print('%5.1f%%  %s' % (100 * c / n, f))
print('--- inclusive')
for f, c in incl.most_common(top):
    print('%5.1f%%  self %4.1f%%  %s' % (100 * c / n, 100 * selfc[f] / n, f))
# CALLERS=NAME: who calls NAME (its frame's caller, from the chain)
if os.environ.get('CALLERS'):
    want = os.environ['CALLERS']
    callers = collections.Counter()
    for (a, b), c in edges.items():
        if b == want:
            callers[a] += c
    print('--- callers of %s (leaf samples)' % want)
    for f, c in callers.most_common(15):
        print('%5.1f%%  %s' % (100 * c / n, f))
    print('--- callers of %s (inclusive)' % want)
    for f, c in icallers.most_common(15):
        print('%5.1f%%  %s' % (100 * c / n, f))
