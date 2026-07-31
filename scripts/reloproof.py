#!/usr/bin/env python3
"""PROVE that per-function native differences are pure relocation.

Weak claim: "the bytes differ only inside 4-byte fields, which looks like
relocation."  Strong claim, which this makes: for every differing 4-byte
field, the OLD value is a rel32 that lands exactly on some function's entry
in the baseline, and the NEW value is a rel32 that lands on the entry of the
SAME NAMED function in the new build.  That is relocation by construction —
the call still goes to the same callee, the callee just moved.

Usage: reloproof.py BASE.symmap BASE.native NEW.symmap NEW.native
"""
import sys, collections

def load(symmap, native):
    rows = []
    for ln in open(symmap):
        if ln.startswith('#'):
            continue
        f = ln.rstrip('\n').split('\t')
        if len(f) < 4:
            continue
        rows.append((int(f[1]), int(f[2]), f[3]))
    rows.sort(key=lambda r: r[1])
    code = open(native, 'rb').read()
    seen = collections.Counter(); fns = {}; starts = {}
    for size, off, name in rows:
        seen[name] += 1
        key = name if seen[name] == 1 else "%s#%d" % (name, seen[name])
        fns[key] = (off, size)
        starts[off] = key
    return fns, starts, code

bfns, bstarts, bcode = load(sys.argv[1], sys.argv[2])
nfns, nstarts, ncode = load(sys.argv[3], sys.argv[4])

proved = 0
unproved = []
diff_fns = 0
size_changes = []

for k in sorted(set(bfns) & set(nfns)):
    if 'SYM-NAME-AUTO' in k:
        continue
    boff, bsz = bfns[k]; noff, nsz = nfns[k]
    a = bcode[boff:boff+bsz]; b = ncode[noff:noff+nsz]
    if a == b:
        continue
    if len(a) != len(b):
        size_changes.append((k, len(a), len(b)))
        continue
    diff_fns += 1
    idx = [i for i in range(len(a)) if a[i] != b[i]]
    # group into runs, then expand each run to every plausible 4-byte window
    runs = []
    for i in idx:
        if runs and i == runs[-1][-1] + 1:
            runs[-1].append(i)
        else:
            runs.append([i])
    for r in runs:
        ok = False
        for start in range(max(0, r[-1] - 3), min(r[0], len(a) - 4) + 1):
            ov = int.from_bytes(a[start:start+4], 'little', signed=True)
            nv = int.from_bytes(b[start:start+4], 'little', signed=True)
            btgt = boff + start + 4 + ov
            ntgt = noff + start + 4 + nv
            if btgt in bstarts and ntgt in nstarts and bstarts[btgt] == nstarts[ntgt]:
                ok = True
                break
        if ok:
            proved += 1
        else:
            unproved.append((k, r[0], r[-1]))

print("=== relocation proof ===")
print("  functions with byte differences : %d" % diff_fns)
print("  functions with SIZE changes     : %d   <- must be 0" % len(size_changes))
for k, la, lb in size_changes[:20]:
    print("      %-40s %d -> %d" % (k, la, lb))
print("  differing fields PROVED to be a rel32 to the SAME callee : %d" % proved)
print("  differing fields NOT proved                              : %d" % len(unproved))
for k, s, e in unproved[:25]:
    print("      %-40s bytes %d..%d" % (k, s, e))
sys.exit(0 if not unproved and not size_changes else 1)
