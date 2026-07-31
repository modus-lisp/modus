#!/usr/bin/env python3
"""Per-function native-code comparison between two Modus builds.

Whole-image md5 is the wrong instrument once NAMES enter the image: the build
derives the runtime symbol-name table by textually scanning the baked source,
so any rename-a-literal-to-a-constant refactor changes the image without
changing a single emitted instruction.  This compares the thing we actually
care about: for every function present in both builds, are its native bytes
identical?

Usage: fndiff.py BASE.symmap BASE.native NEW.symmap NEW.native [arch]
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
        rows.append((int(f[0], 16), int(f[1]), int(f[2]), f[3]))
    rows.sort(key=lambda r: r[2])            # by native offset
    code = open(native, 'rb').read()
    # Disambiguate duplicate names (last-defun-wins leaves dupes in the table)
    seen = collections.Counter()
    out = {}
    for vaddr, size, off, name in rows:
        seen[name] += 1
        key = name if seen[name] == 1 else "%s#%d" % (name, seen[name])
        out[key] = (off, size, code[off:off + size])
    return out, code

def strip_pad(b, arch):
    """Drop trailing function-alignment padding so a function whose START
    address merely shifted is not reported as changed."""
    if arch == 'aa64':
        nop = b'\x1f\x20\x03\xd5'
        while len(b) >= 4 and b[-4:] == nop:
            b = b[:-4]
    else:
        while b and b[-1] == 0x90:
            b = b[:-1]
    return b

def main():
    bs, bn, ns, nn = sys.argv[1:5]
    arch = sys.argv[5] if len(sys.argv) > 5 else 'x64'
    base, bcode = load(bs, bn)
    new,  ncode = load(ns, nn)

    bk, nk = set(base), set(new)
    common = bk & nk
    only_base = bk - nk
    only_new  = nk - bk

    differ, differ_symname, same = [], [], 0
    for k in sorted(common):
        a = strip_pad(base[k][2], arch)
        b = strip_pad(new[k][2], arch)
        if a == b:
            same += 1
        elif 'SYM-NAME-AUTO' in k or 'SYM-NAME' in k:
            differ_symname.append((k, len(a), len(b)))
        else:
            differ.append((k, len(a), len(b)))

    print("=== per-function native comparison (%s) ===" % arch)
    print("  functions in baseline : %d" % len(bk))
    print("  functions in new      : %d" % len(nk))
    print("  common                : %d" % len(common))
    print("  IDENTICAL             : %d" % same)
    print("  differ (sym-name tbl) : %d   <- EXPECTED (content is the token list)"
          % len(differ_symname))
    print("  differ (OTHER)        : %d   <- must be 0" % len(differ))
    print("  only in baseline      : %d" % len(only_base))
    print("  only in new           : %d" % len(only_new))
    if differ_symname:
        for k, la, lb in differ_symname:
            print("    symname %-34s %7d -> %7d  (%+d)" % (k, la, lb, lb - la))
    if differ:
        print("  *** NON-SYMNAME DIFFERENCES ***")
        for k, la, lb in differ[:40]:
            print("    %-40s %7d -> %7d  (%+d)" % (k, la, lb, lb - la))
    if only_base:
        print("  only-baseline sample:", sorted(only_base)[:10])
    if only_new:
        print("  only-new sample:", sorted(only_new)[:10])

    # ---- byte accounting ----
    print()
    print("=== byte accounting ===")
    print("  baseline native total : %d" % len(bcode))
    print("  new native total      : %d" % len(ncode))
    total_delta = len(ncode) - len(bcode)
    print("  TOTAL DELTA           : %+d" % total_delta)
    sym_delta = sum(new[k][1] - base[k][1] for k, _, _ in
                    [(x[0], 0, 0) for x in differ_symname])
    add_new = sum(new[k][1] for k in only_new)
    rm_base = sum(base[k][1] for k in only_base)
    other_delta = sum(new[k][1] - base[k][1] for k in common
                      if k not in [x[0] for x in differ_symname])
    print("    sym-name fns resized : %+d" % sym_delta)
    print("    functions only in new: %+d (%d fns)" % (add_new, len(only_new)))
    print("    functions only in base: %+d (%d fns)" % (-rm_base, len(only_base)))
    print("    all other fns (incl. alignment padding): %+d" % other_delta)
    acct = sym_delta + add_new - rm_base + other_delta
    print("  ACCOUNTED             : %+d   (unexplained: %+d)"
          % (acct, total_delta - acct))
    return 0 if not differ else 1

sys.exit(main())
