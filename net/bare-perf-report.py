#!/usr/bin/env python3
"""Symbolize net/bare-perf.lisp's PERF-REPORT output.

  bare-perf-report.py SYMMAP[,SYMMAP...] PERF.txt [--folded OUT]

SYMMAP: either "address name" lines in decimal (the runtime map: every fbound symbol's
function, dumped on the board) or the build's image map (MODUS_SYMMAP=FILE at build time:
"vaddr-hex size offset name", tab-separated; its sizes bound each function exactly).  Give
both, comma-separated: the runtime map knows JIT code and the core's libraries, the image map
knows the kernel's internal functions, which no symbol names.  An image map entry is
"FILE@BASE" when the kernel runs somewhere other than its link address (BASE in hex is added).
PERF.txt: the PERF / PERF-PC / PERF-STACK lines the board printed.
Prints the flat profile by function (self), the top stacks symbolized leaf-first, and with
--folded writes "root;...;leaf count" lines for flamegraph.pl / speedscope."""
import bisect, sys
def load_map(spec):
    m = []
    for p in spec.split(","):
        p, _, base = p.partition("@")
        base = int(base, 16) if base else 0
        for line in open(p):
            if line.startswith("#"): continue
            f = line.rstrip("\n").split("\t")
            if len(f) == 4:                          # image map: vaddr-hex size offset name
                m.append((int(f[0], 16) + base, int(f[1]), f[3])); continue
            f = line.split(None, 1)
            if len(f) == 2:
                try: m.append((int(f[0]), None, f[1].strip()))
                except ValueError: pass
    m.sort(key=lambda e: (e[0], e[1] is None)); return [e[0] for e in m], m
addrs, entries = load_map(sys.argv[1])
def sym(x):
    i = bisect.bisect_right(addrs, x) - 1
    if i < 0: return "[%x]" % x
    a, size, name = entries[i]
    ok = x - a < size if size is not None else x - a < 1 << 20
    return name if ok else "[%x]" % x
flat, stacks, total = {}, [], 0
for line in open(sys.argv[2]):
    f = line.split()
    if not f: continue
    if f[0] == "PERF": total = int(f[2])
    elif f[0] == "PERF-PC":
        s = sym(int(f[2], 16)); flat[s] = flat.get(s, 0) + int(f[1])
    elif f[0] == "PERF-STACK":
        pcs = [int(x, 16) for x in f[2].split(";")]
        # pc, interrupted x30, then the frame-record returns.  x30 is only a caller when the
        # leaf has not yet pushed its frame, so it is shown when it differs from frame 0.
        chain = [sym(pcs[0])]
        if len(pcs) > 1 and (len(pcs) < 3 or sym(pcs[1]) != sym(pcs[2])): chain.append("(lr)" + sym(pcs[1]))
        chain += [sym(x) for x in pcs[2:]]
        stacks.append((int(f[1]), chain))
seen = sum(flat.values()) or 1
print("samples %d (flat shows the top PCs: %d)" % (total, seen))
for s, n in sorted(flat.items(), key=lambda e: -e[1])[:25]:
    print("%6.1f%%  %6d  %s" % (100.0 * n / max(total, 1), n, s))
print("\ntop stacks (leaf first):")
for n, chain in sorted(stacks, key=lambda e: -e[0])[:12]:
    print("%6d  %s" % (n, "  <-  ".join(chain[:8])))
if "--folded" in sys.argv:
    out = open(sys.argv[sys.argv.index("--folded") + 1], "w")
    for n, chain in stacks:
        out.write("%s %d\n" % (";".join(reversed([c.replace(";", ":") for c in chain])), n))
