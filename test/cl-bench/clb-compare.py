#!/usr/bin/env python3
"""test/cl-bench/clb-compare.py SBCL-REPORT MODUS-REPORT -- user-time ratios per
benchmark, slowest first, and their geometric mean (see clb-pmu.lisp)."""
import re, sys
def load(p):
    d = {}
    for l in open(p):
        m = re.match(r';; (\S+)\s+([\d.]+)\s+([\d.]+)', l)
        if m: d[m.group(1)] = float(m.group(3))   # user time
    return d
s = load(sys.argv[1]); m = load(sys.argv[2])
rows = []
for k in s:
    if k in m:
        rows.append((k, s[k], m[k], (m[k] / s[k]) if s[k] > 0 else float('inf')))
rows.sort(key=lambda r: -r[3])
print(f"{'benchmark':28s} {'SBCL s':>8s} {'modus s':>8s} {'ratio':>8s}")
for k, a, b, r in rows:
    print(f"{k:28s} {a:8.2f} {b:8.2f} {r:8.1f}" if r != float('inf') else f"{k:28s} {a:8.2f} {b:8.2f}      inf")
import statistics
fin = [r for *_, r in rows if r != float('inf') and r > 0]
print(f"\n{len(rows)} benchmarks; geometric mean ratio {statistics.geometric_mean(fin):.1f}x (excluding SBCL 0.00)")
