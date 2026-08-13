#!/usr/bin/env python3
"""Cluster RT:FAIL / RT:ERR records from a Modus rtest run."""
import re, sys, collections

path = sys.argv[1]
txt = open(path, errors='replace').read().splitlines()

recs = []
i = 0
while i < len(txt):
    m = re.match(r'^RT:(PASS|FAIL|ERR)\s+(\S+)\s*$', txt[i])
    if m:
        kind, name = m.group(1), m.group(2)
        rec = {'kind': kind, 'name': name}
        j = i + 1
        while j < len(txt) and j < i + 8:
            mm = re.match(r'^  (Form|Expected|Actual|Cond|Text): ?(.*)$', txt[j])
            if not mm:
                break
            rec[mm.group(1)] = mm.group(2)
            j += 1
        recs.append(rec)
        i = j
    else:
        i += 1

npass = sum(1 for r in recs if r['kind'] == 'PASS')
nfail = sum(1 for r in recs if r['kind'] == 'FAIL')
nerr = sum(1 for r in recs if r['kind'] == 'ERR')
print(f"records={len(recs)} PASS={npass} FAIL={nfail} ERR={nerr}")

print("\n=== ERR clustered by condition type ===")
by = collections.defaultdict(list)
for r in recs:
    if r['kind'] == 'ERR':
        key = r.get('Cond', '?')
        txt2 = r.get('Text', '')
        # normalise: strip trailing specifics after the first quoted/name token
        by[(key, txt2[:70])].append(r['name'])
for (k, t), names in sorted(by.items(), key=lambda kv: -len(kv[1])):
    print(f"[{len(names):3d}] {k} | {t}")
    print(f"       {' '.join(sorted(names))}")

print("\n=== FAIL (wrong values) ===")
for r in recs:
    if r['kind'] == 'FAIL':
        print(f"  {r['name']}")
        print(f"      form:     {r.get('Form','')[:160]}")
        print(f"      expected: {r.get('Expected','')[:120]}")
        print(f"      actual:   {r.get('Actual','')[:120]}")

print("\n=== failing name prefixes ===")
pref = collections.Counter()
for r in recs:
    if r['kind'] != 'PASS':
        pref[r['name'].split('.')[0]] += 1
for k, v in pref.most_common():
    print(f"  {v:3d}  {k}")
