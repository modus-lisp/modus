#!/usr/bin/env python3
"""Compare two ladder log dirs by ERROR TEXT, LF-FILE results and probe results."""
import os,re,sys
A,B = sys.argv[1], sys.argv[2]
libs=[l.strip() for l in open("/home/claude/lf/ladder.txt") if l.strip()]+["alexandria-ql","sha1-ql"]
def parse(path):
    d={"files":{}, "probes":{}, "errs":[], "exit":None, "secs":None}
    if not os.path.exists(path): return None
    for ln in open(path,errors='replace').read().splitlines():
        s=ln.rstrip()
        if s.startswith("EXIT="):
            m=re.match(r'EXIT=(\d+) SECS=(\d+)',s)
            if m: d["exit"]=m.group(1); d["secs"]=int(m.group(2))
            continue
        if "!!" in s: d["errs"].append(s.strip()); continue
        m=re.match(r'^LF-FILE (\S+)=(.*)$',s)
        if m: d["files"][m.group(1)]=m.group(2); continue
        m=re.match(r'^(P[12]\.[A-Za-z0-9_.-]+)=(.*)$',s)
        if m: d["probes"][m.group(1)]=m.group(2); continue
    return d
def norm(e):
    # strip volatile addresses/ids so texts compare
    return re.sub(r'0x[0-9a-fA-F]+','0xADDR',e)
ta,tb=0,0
for lib in libs:
    da=parse(f"{A}/{lib}.log"); db=parse(f"{B}/{lib}.log")
    if da is None or db is None:
        print(f"### {lib}: MISSING ({'A' if da is None else ''}{'B' if db is None else ''})"); continue
    na,nb=len(da["errs"]),len(db["errs"]); ta+=na; tb+=nb
    ea=[norm(x) for x in da["errs"]]; eb=[norm(x) for x in db["errs"]]
    fdiff={k:(da["files"].get(k),db["files"].get(k)) for k in set(da["files"])|set(db["files"])
           if da["files"].get(k)!=db["files"].get(k)}
    pdiff={k:(da["probes"].get(k),db["probes"].get(k)) for k in set(da["probes"])|set(db["probes"])
           if da["probes"].get(k)!=db["probes"].get(k)}
    changed = (sorted(ea)!=sorted(eb)) or fdiff or pdiff or da["exit"]!=db["exit"]
    tag = "SAME" if not changed else ("BETTER" if nb<na else ("WORSE" if nb>na else "CHANGED"))
    print(f"### {lib}: errs {na} -> {nb}  exit {da['exit']}->{db['exit']}  [{tag}]")
    if not changed: continue
    import collections
    ca,cb=collections.Counter(ea),collections.Counter(eb)
    for e in sorted((ca-cb).elements()): print(f"    -ERR {e}")
    for e in sorted((cb-ca).elements()): print(f"    +ERR {e}")
    for k in sorted(fdiff): print(f"    FILE {k}\n        A={fdiff[k][0]}\n        B={fdiff[k][1]}")
    for k in sorted(pdiff): print(f"    PROBE {k}: A={pdiff[k][0]}  B={pdiff[k][1]}")
print(f"\nTOTAL ERRORS: A={ta}  B={tb}")
