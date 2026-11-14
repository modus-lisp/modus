#!/usr/bin/env python3
"""Per-function IR comparison between two builds (MODUS_IR_DUMP).

    irdiff.py BASE.ir NEW.ir [--show N]

fndiff.py compares NATIVE bytes, which is the right instrument when the two
builds compile the same source.  When the source itself changes -- a function
or a struct added anywhere -- every later static-symbol slot, constant-pool
address and counter-numbered closure name moves, and fndiff/reloproof report
thousands of differences that are position, not code.  The IR dump
(build-checks.lisp, "IR DUMP") is normalised for that: labels renumbered per
function, static-symbol slot indices blanked, constant-table indices replaced
by the constant.  This script finishes the job: counter-numbered names
(NAME$$LAMBDAn, $$CLOSUREn, $$FLETn, $$LABELSn, TOPLEVEL-n) are renumbered by their order of
first appearance under each base name, in both the function names and the
IR text, and then every function is compared.

A function whose IR is identical in both builds is compiled to the same code
by a translator that did not change.  So for a change that touches only the
compiler, the "differ" list is the list of functions whose OWN source changed.
Exit 0 always; read the report.
"""
import re, sys
from collections import OrderedDict, defaultdict

NUM = re.compile(r'([A-Za-z0-9%*+\-<>=/!?._]+?)\$\$(LAMBDA|CLOSURE|FLET|LABELS)(\d+)|\bTOPLEVEL-(\d+)')

def load(path):
    fns = OrderedDict()
    seq = defaultdict(dict)          # (base, kind) -> {number: ordinal}

    def renum(text):
        def sub(m):
            if m.group(4) is not None:
                key, n = ("TOPLEVEL", "T"), m.group(4)
                d = seq[key]
                if n not in d:
                    d[n] = len(d)
                return "TOPLEVEL-#%d" % d[n]
            base, kind, n = m.group(1), m.group(2), m.group(3)
            d = seq[(base, kind)]
            if n not in d:
                d[n] = len(d)
            return "%s$$%s#%d" % (base, kind, d[n])
        # renumber repeatedly: nested names (A$$CLOSURE1$$CLOSURE2)
        prev = None
        while prev != text:
            prev, text = text, NUM.sub(sub, text)
        return text

    # AOT global-variable cache slots.  Slots are numbered in source order, so
    # a global reference added or removed anywhere earlier shifts every later
    # slot number -- position, like the static-symbol slot the dump already
    # blanks.  The dump ends with the build's map (@GVMAP SLOT NAME-KEY, untagged; see
    # build-checks.lisp), so a slot operand -- where it feeds a %GV- call or
    # the inline cache probe (:BEQ ..) (:LI r SLOT) (:AREF r base r) -- is
    # replaced by the global it stands for.
    LI = re.compile(r'\(:LI (\d+) (-?\d+)\)')
    GVCALL = re.compile(r'\(:CALL (?:#\d+=)?"%GV-[A-Z-]+"|\(:CALL #\d+# ')
    slot_name = {}

    def split(text):
        return text.replace(") (", ")\n(").split("\n")

    def gvnorm(text):
        insns = split(text)
        out = []
        for i, ins in enumerate(insns):
            m = LI.search(ins)
            # IR immediates are TAGGED (value << 1): slot 469 is (:LI r 938)
            slot = str(int(m.group(2)) // 2) if m and int(m.group(2)) % 2 == 0 else None
            if slot in slot_name and i + 1 < len(insns):
                nxt = insns[i + 1]
                probe = (i > 0 and insns[i - 1].lstrip("(").startswith(":BEQ")
                         and re.match(r'\(:AREF %s \d+ %s\)' % (m.group(1), m.group(1)), nxt))
                if GVCALL.search(nxt) or probe:
                    ins = ins.replace(m.group(0), "(:LI %s :gv=%s)" % (m.group(1), slot_name[slot]))
            out.append(ins)
        return ") (".join(out)

    name = None
    dup = defaultdict(int)
    with open(path, errors="replace") as f:
        for line in f:
            if line.startswith("@FORM "):
                continue
            if line.startswith("@GVMAP "):
                _, slot, key = line.split()
                slot_name[slot] = key
                continue
            if line.startswith("@FN "):
                name = renum(line[4:].strip())
                dup[name] += 1
                if dup[name] > 1:
                    name = "%s<%d>" % (name, dup[name])
                continue
            if name is not None:
                fns[name] = fns.get(name, "") + renum(line)
                name = None
    for k in fns:
        fns[k] = gvnorm(fns[k])
    return fns

def main():
    a, b = load(sys.argv[1]), load(sys.argv[2])
    show = 60
    if "--show" in sys.argv:
        show = int(sys.argv[sys.argv.index("--show") + 1])
    common = [k for k in a if k in b]
    differ = [k for k in common if a[k] != b[k]]
    only_a = [k for k in a if k not in b]
    only_b = [k for k in b if k not in a]
    print("=== per-function IR comparison ===")
    print("  functions in base : %d" % len(a))
    print("  functions in new  : %d" % len(b))
    print("  identical         : %d" % (len(common) - len(differ)))
    print("  DIFFER            : %d" % len(differ))
    print("  only in base      : %d" % len(only_a))
    print("  only in new       : %d" % len(only_b))
    for title, lst in (("differ", differ), ("only in base", only_a), ("only in new", only_b)):
        if lst:
            print("--- %s (%d) ---" % (title, len(lst)))
            for k in lst[:show]:
                print("    " + k)
            if len(lst) > show:
                print("    ... %d more" % (len(lst) - show))

main()
