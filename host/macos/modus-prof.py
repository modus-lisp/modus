#!/usr/bin/env python3
"""modus-prof.py SAMPLES SYMMAP [--folded OUT] [--top N] -- read the shim's profile (MODUS_SHIM_PROFILE).

Each line of SAMPLES is one sample: the interrupted PC, its LR, then the return addresses up the frame chain,
all LINK addresses, innermost first.  SYMMAP is the build's MODUS_SYMMAP (virtual-addr, size, native-offset,
name, tab-separated).  Prints the functions by SELF samples (the PC's) and by INCLUSIVE samples (anywhere on the
stack, once per sample), and with --folded writes the stacks outermost-first, `a;b;c COUNT' per line -- what
flamegraph.pl and speedscope read.

The LR is used only where the PC's function has not yet made its frame (a leaf, or a runtime routine with
none): if the first return address up the chain is already the LR's function, it is not counted twice.
"""
import argparse, bisect, collections


def load_symmap(path):
    syms = []
    for line in open(path):
        if line.startswith('#'):
            continue
        f = line.rstrip('\n').split('\t')
        if len(f) >= 4:
            syms.append((int(f[0], 16), int(f[1]), f[3]))
    syms.sort()
    return syms, [a for a, _, _ in syms]


def namer(syms, addrs):
    cache = {}

    def name(a):
        if a in cache:
            return cache[a]
        i = bisect.bisect_right(addrs, a) - 1
        if i < 0:
            n = '?%x' % a
        else:
            base, size, nm = syms[i]
            # past the end of the nearest function: code the map does not know (the JIT arena, the runtime)
            n = nm if a < base + max(size, 4096) else '?%x' % a
        cache[a] = n
        return n
    return name


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('samples')
    ap.add_argument('symmap')
    ap.add_argument('--folded')
    ap.add_argument('--top', type=int, default=30)
    a = ap.parse_args()
    syms, addrs = load_symmap(a.symmap)
    name = namer(syms, addrs)
    self_c, incl_c, folded = collections.Counter(), collections.Counter(), collections.Counter()
    total = 0
    for line in open(a.samples):
        words = [int(w, 16) for w in line.split()]
        if not words:
            continue
        total += 1
        pc, rest = words[0], words[1:]
        frames = [name(pc)]
        if rest:
            lr, chain = rest[0], rest[1:]
            if not chain or name(chain[0]) != name(lr):
                frames.append(name(lr))
            frames += [name(r) for r in chain]
        # a frame that repeats itself in a row (self recursion, or the LR and the chain agreeing) is one frame
        stack = [f for i, f in enumerate(frames) if i == 0 or f != frames[i - 1]]
        self_c[stack[0]] += 1
        for f in set(stack):
            incl_c[f] += 1
        folded[';'.join(reversed(stack))] += 1
    print(f'{total} samples')
    print(f'\n{"self":>7}  {"%":>5}   function')
    for f, n in self_c.most_common(a.top):
        print(f'{n:7d}  {100 * n / total:5.1f}   {f}')
    print(f'\n{"incl":>7}  {"%":>5}   function')
    for f, n in incl_c.most_common(a.top):
        print(f'{n:7d}  {100 * n / total:5.1f}   {f}')
    if a.folded:
        with open(a.folded, 'w') as out:
            for stack, n in folded.most_common():
                out.write(f'{stack} {n}\n')


if __name__ == '__main__':
    main()
