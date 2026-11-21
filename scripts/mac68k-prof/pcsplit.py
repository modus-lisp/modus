# pcsplit.py PROFDIR IMAGE.bin [LOAD] -- per phase: where the PC was (ROM / image / other RAM)
import sys, collections, os
O, IMG = sys.argv[1], sys.argv[2]; LOAD = int(sys.argv[3], 0) if len(sys.argv) > 3 else 0x40000
n = os.path.getsize(IMG)
c = collections.defaultdict(collections.Counter)
for l in open(O + '/samples.txt'):
    q = l.split()
    if len(q) < 4: continue
    ph, pc = int(q[0]), int(q[1], 16)
    k = ('ROM' if 0x400000 <= pc < 0x500000 else
         'image' if LOAD <= pc < LOAD + n else 'other RAM')
    c[ph][k] += 1
for ph in sorted(c):
    t = sum(c[ph].values())
    print(f"phase {ph:2d}: {t:5d} samples  " + "  ".join(f"{k} {100*v/t:4.1f}%" for k, v in sorted(c[ph].items())))
