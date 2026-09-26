#!/usr/bin/env python3
"""score.py <logdir> [drivers-dir] -- uniform ladder score: per-library probe outcomes.

A library is CLEAN when it exited 0, reached LF-END, every probe answered
without (ERR ...), none is missing, and no dependency/install aborted.
MISSING = probes the driver contains that never printed a line.

drivers-dir defaults to <logdir>/drivers, where test/ladder/run.sh puts the
drivers it generated for that run.  Logs from before the ladder moved into the
repo (/home/claude/lf/logs/<tag>) score with drivers-dir /home/claude/lf/drivers.
"""
import sys, os, re, glob
if len(sys.argv) < 2:
    sys.exit(__doc__)
d = sys.argv[1]
drivers = sys.argv[2] if len(sys.argv) > 2 else os.path.join(d, "drivers")
if not os.path.isdir(drivers):
    sys.exit("no drivers directory %s -- pass it as the second argument" % drivers)
tot_ok = tot_err = tot_miss = 0
clean = 0; libs = 0
rows = []
for f in sorted(glob.glob(os.path.join(d, "*.log"))):
    name = os.path.basename(f)[:-4]
    txt = open(f, errors="replace").read()
    exitm = re.search(r"EXIT=(\d+) SECS=(\d+)", txt)
    ex = exitm.group(1) if exitm else "?"
    secs = exitm.group(2) if exitm else "?"
    end = "LF-END=" in txt
    probes = re.findall(r"^(P[12]\.[^=]+)=(.*)$", txt, re.M)
    # Modus now princ's keywords CONFORMANTLY (CLHS 22.1.3.3: the package
    # prefix is gated on *print-escape*), so (princ :ERR) prints "ERR", not
    # ":ERR" -- matching SBCL.  Accept BOTH spellings or every failure scores
    # as a pass.
    _ERR = re.compile(r"\(:?ERR\b")
    ok = sum(1 for _, v in probes if not _ERR.search(v))
    err = sum(1 for _, v in probes if _ERR.search(v))
    # expected probe count: from the driver
    drv = os.path.join(drivers, name + ".lisp")
    exp = 0
    if os.path.exists(drv):
        exp = len(re.findall(r"\(lf-probe ", open(drv).read()))
    miss = max(0, exp - len(probes))
    dep_abort = len(re.findall(r"LF-(?:DEP|INSTALL)[^=]*=\(:?LOAD-ABORT", txt))
    libs += 1
    if err == 0 and miss == 0 and end and ex == "0" and dep_abort == 0:
        clean += 1
    tot_ok += ok; tot_err += err; tot_miss += miss
    rows.append((name, ex, secs, ok, err, miss, "END" if end else "-", dep_abort))
print("%-26s %4s %5s %4s %4s %5s %4s %5s" % ("LIB","EXIT","SECS","OK","ERR","MISS","END","LDABT"))
for r in rows:
    print("%-26s %4s %5s %4s %4s %5s %4s %5s" % r)
print("---")
print("libs=%d clean=%d  probes ok=%d err=%d missing=%d  FAILURES=%d"
      % (libs, clean, tot_ok, tot_err, tot_miss, tot_err+tot_miss))
