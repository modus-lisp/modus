#!/usr/bin/env python3
"""scripts/pmu-annotate.py PREFIX [--all]

Disassemble PREFIX.bin (written by modus's PMU-ANNOTATE, net/hosted-pmu.lisp)
with objdump and print each instruction with the samples that landed on it,
from PREFIX.hits.  Without --all only the hot regions are shown (instructions
within 6 of a sampled one).  Addresses are offsets from the function entry;
the entry's absolute address is in the .hits header."""
import subprocess, sys

def main():
    prefix = sys.argv[1]
    show_all = '--all' in sys.argv
    hits, header = {}, ''
    for line in open(prefix + '.hits'):
        if line.startswith('#'):
            header = line[1:].strip(); continue
        off, n = line.split()
        hits[int(off)] = hits.get(int(off), 0) + int(n)
    total = sum(hits.values()) or 1
    out = subprocess.run(['objdump', '-D', '-b', 'binary', '-mi386:x86-64', '-M', 'intel',
                          '--no-show-raw-insn', prefix + '.bin'],
                         capture_output=True, text=True, check=True).stdout
    insns = []
    for line in out.splitlines():
        parts = line.split(':', 1)
        if len(parts) != 2: continue
        try: off = int(parts[0].strip(), 16)
        except ValueError: continue
        insns.append((off, parts[1].strip()))
    # a sample's IP is the instruction AFTER the one that retired (skid): show
    # the count where it landed, as perf annotate does
    hot = set()
    for i, (off, _) in enumerate(insns):
        if hits.get(off):
            hot.update(range(max(0, i - 6), min(len(insns), i + 7)))
    print(f'{header}  ({total} samples in this function)')
    last = -2
    for i, (off, text) in enumerate(insns):
        if not show_all and i not in hot: continue
        if i != last + 1: print('   ...')
        n = hits.get(off, 0)
        bar = f'{100.0 * n / total:5.1f}%' if n else '      '
        print(f'{bar} {n:6d}  {off:5x}:  {text}' if n else f'{bar}         {off:5x}:  {text}')
        last = i

main()
