#!/usr/bin/env python3
"""After reel + forms + clip are on the board (NIC alive): do the slow/NIC-hostile part over SERIAL.
jit-eager -> reel-demo-pass x3 (DECODE-MS) -> rh-init/rh-first-frame -> webcam record during rh-play."""
import serial, time, re, random, subprocess, sys
s=serial.Serial("/dev/ttyAMA0",115200,timeout=0.3); s.reset_input_buffer()
def rd_until(pat,t):
    e=time.time()+t; b=b""
    while time.time()<e:
        d=s.read(8192)
        if d: b+=d
        if re.search(pat,b.decode("utf-8","replace")): break
    return b.decode("utf-8","replace").replace("\0","")
def q(form,w=10.0):
    t=random.randint(1000,9999); s.reset_input_buffer()
    for ch in "(list %d %s)\r\n"%(t,form): s.write(ch.encode()); time.sleep(0.006)
    out=rd_until(r"\(%d .+?\)\s*\n"%t,w); m=re.search(r"\(%d (.+?)\)\s*\n"%t,out+"\n")
    return (m.group(1) if m else None), out
def ev(form,w=10.0):
    v,out=q(form,w); print("  %-50s => %s"%(form[:50], v if v is not None else "?? "+out.strip()[-120:]),flush=True); return v
for _ in range(5):
    for ch in ")))))))))\r\n(+ 0 0)\r\n": s.write(ch.encode()); time.sleep(0.006)
    time.sleep(0.6); v,_=q("(+ 21 21)",4.0)
    if v=="42": break
else: print("SERIAL REPL NOT READY"); sys.exit(1)
print("SERIAL READY",flush=True)
def raw(t,settle):
    s.reset_input_buffer()
    for ch in t+"\r\n": s.write(ch.encode()); time.sleep(0.006)
    return rd_until(r"never",settle)
ev("(setq *jit-hot-only* nil)")
raw("(defun pstats (pic) (let* ((w (reel.decode::picture-width pic)) (h (reel.decode::picture-height pic)) (y (reel.decode::picture-y pic)) (ys (reel.decode::picture-y-stride pic)) (yo (reel.decode::picture-y-offset pic)) (u (reel.decode::picture-u pic)) (cs (reel.decode::picture-uv-stride pic)) (co (reel.decode::picture-uv-offset pic)) (sy 0) (su 0) (f8 nil)) (dotimes (r h) (dotimes (c w) (setq sy (+ sy (aref y (+ yo (* r ys) c)))))) (dotimes (r (ash h -1)) (dotimes (c (ash w -1)) (setq su (+ su (aref u (+ co (* r cs) c)))))) (dotimes (c 8) (push (aref y (+ yo c)) f8)) (list w h sy su (reverse f8))))", 4.0)
raw("(defun rh-stats () (let* ((vd (reel:make-decoder)) (sz (rh-le32 *rh-ivf* 32)) (pic (reel:decode-frame vd *rh-ivf* :start 44 :end (+ 44 sz)))) (pstats pic)))", 4.0)
ev("(if (fboundp (quote rh-stats)) 1 0)")
t0=time.time(); v=ev("(rh-stats)",900.0); print("  BOARD0 %s  (%.1fs)"%(v,time.time()-t0),flush=True)
