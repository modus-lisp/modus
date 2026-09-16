#!/usr/bin/env python3
"""Phase 2 under QEMU: push HVS + demo forms, load the clip from RAM, snapshot,
dump the core via the gdbstub.  Run after qdrive.py reports INSTALL PHASE DONE."""
import socket, time, re, random, subprocess, os
S = "/tmp/claude-1002/-home-claude-modus/9e874ffc-83a3-493c-9640-e84a344b598b/scratchpad"
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM); s.connect(S + "/q.sock"); s.settimeout(0.3)
def rd(w):
    e = time.time() + w; b = b""
    while time.time() < e:
        try:
            d = s.recv(65536)
            if d: b += d; e = time.time() + min(w, 1.5)
        except socket.timeout: pass
    return b.decode("utf-8", "replace")
def snd(t, w): s.sendall((t + "\r\n").encode()); return rd(w)
def rd_until(pat, timeout):
    e = time.time() + timeout; b = b""
    while time.time() < e:
        try:
            d = s.recv(65536)
            if d: b += d
        except socket.timeout: pass
        if re.search(pat, b.decode("utf-8", "replace")): break
    return b.decode("utf-8", "replace")
def q(form, w=8.0):
    t = random.randint(1000, 9999); out = snd("(list %d %s)" % (t, form), w)
    m = re.search(r"\(%d (.+?)\)\s*\n" % t, out + "\n"); return (m.group(1) if m else None), out
def ev(form, w=8.0):
    v, out = q(form, w); print("  %-58s => %s" % (form[:58], v if v is not None else out.strip()[-90:]), flush=True); return v
def push(path):
    n = bad = 0
    for line in open(path):
        line = line.strip()
        if not line: continue
        out = snd(line, 3.0); n += 1
        if "ERROR" in out: bad += 1; print("  FORM ERR:", line[:60], "->", out.strip()[-120:], flush=True)
    print("  pushed %d forms from %s, errors %d" % (n, os.path.basename(path), bad), flush=True)
rd(2.0)
# wait until the REPL has drained the install + queued forms: a tagged probe answers
t0 = time.time(); ok = False
while time.time() - t0 < 5400:
    tg = random.randint(10000, 99999); s.sendall(("(list %d (+ 20 22))\r\n" % tg).encode())
    out = rd_until(r"\(%d 42\)" % tg, 30.0)
    if re.search(r"\(%d 42\)" % tg, out): ok = True; break
print("  REPL caught up after %.0fs: %s" % (time.time() - t0, ok), flush=True)
if not ok: raise SystemExit(1)
ev("(+ 20 22)")
ev('(if (find-package "REEL") 1 0)')
print("=== forms ===", flush=True)
push("/tmp/claude-1002/hvs-all-forms.txt")
push(S + "/reel-hvs-forms.txt")
ev("(if (fboundp (quote hvs-scaled-plane)) 1 0)")
ev("(if (fboundp (quote rh-play)) 1 0)")
print("=== clip from RAM ===", flush=True)
ev("(progn (setq *rh-ivf* (ramv 452984832 63792) *rh-len* 63792) *rh-len*)", 120.0)
ev("(list (aref *rh-ivf* 0) (aref *rh-ivf* 1) (aref *rh-ivf* 2) (aref *rh-ivf* 3))")   # D K I F
ev("(setq *rh-tarv* nil)")
print("=== jit-eager before save ===", flush=True)
ev("(jit-eager)", 1800.0)
ev("(list *jit-native-defun-count* (if (fboundp (quote reel::decode-frame)) (%jit-fn-native-p \"DECODE-FRAME\") :nofn))")
print("=== save ===", flush=True)
s.sendall(b'(%save-image "reel.core")\r\n'); out = rd_until(r"CORE-END=\d+", 1200.0)
print("  save tail:", out.strip()[-200:].replace("\n", " | "), flush=True)
m = re.search(r"CORE-END=(\d+)", out)
if not m: print("NO CORE-END"); raise SystemExit(1)
end = int(m.group(1)); print("  core range 0x18000000 .. %#x (%.1f MB)" % (end, (end - 0x18000000) / 1e6), flush=True)
g = subprocess.run(["gdb-multiarch", "-q", "-batch", "-ex", "set architecture aarch64", "-ex", "target remote :1234",
                    "-ex", "dump binary memory %s/reel.core 0x18000000 %d" % (S, end), "-ex", "detach"],
                   capture_output=True, text=True, timeout=600)
print("  gdb:", (g.stdout + g.stderr).strip()[-200:], flush=True)
print("  core file:", os.path.getsize(S + "/reel.core"), "bytes", flush=True)
print("=== SAVE PHASE DONE ===", flush=True)
