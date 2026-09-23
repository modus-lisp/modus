#!/usr/bin/env python3
"""compare.py KEYS SBCL INTERP NATIVE -- classify every case three ways.

  both      INTERP == NATIVE != SBCL   a compiler / runtime SEMANTICS bug
  interp    INTERP != NATIVE == SBCL   the INTERPRETER arm diverges
  native    NATIVE != INTERP == SBCL   the TRANSLATOR diverges
  split     all three differ
  missing   a modus run never printed this case (it died, or hung first)

Grouped by operation, with the first example of each so a bucket reads as a
bug report rather than a count."""
import sys, collections

def load(p):
    d = {}
    for ln in open(p, errors="replace"):
        ln = ln.rstrip("\n")
        if not ln or not ln[0].isdigit():
            continue
        i, _, rest = ln.partition(" ")
        if i.isdigit():
            d[int(i)] = rest
    return d

keys = {}
for ln in open(sys.argv[1]):
    i, _, rest = ln.rstrip("\n").partition(" ")
    keys[int(i)] = rest
S, I, N = load(sys.argv[2]), load(sys.argv[3]), load(sys.argv[4])

buckets = collections.defaultdict(lambda: collections.defaultdict(list))
for i, k in keys.items():
    op = k[1:].split(" ", 1)[0]
    s, a, n = S.get(i), I.get(i), N.get(i)
    if a is None or n is None:
        b = "missing"
    elif a == n == s:
        continue
    elif a == n:
        b = "both"
    elif n == s:
        b = "interp"
    elif a == s:
        b = "native"
    else:
        b = "split"
    buckets[b][op].append((i, k, s, a, n))

total = sum(len(v) for ops in buckets.values() for v in ops.values())
print(f"cases {len(keys)}   disagreeing {total}")
for b in ["interp", "native", "split", "both", "missing"]:
    ops = buckets.get(b, {})
    if not ops:
        continue
    n = sum(len(v) for v in ops.values())
    print(f"\n=== {b}: {n} cases over {len(ops)} ops ===")
    for op, rows in sorted(ops.items(), key=lambda kv: -len(kv[1])):
        i, k, s, a, nv = rows[0]
        print(f"  {op:14s} x{len(rows):<5d} e.g. {k}")
        print(f"  {'':14s}        sbcl={s}  interp={a}  native={nv}")
