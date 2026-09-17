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
ev("(list (first *rh-bufs*) (second *rh-bufs*))")
raw("(defun rh-probe () (let* ((vd (reel:make-decoder)) (sz (rh-le32 *rh-ivf* 32)) (pic (reel:decode-frame vd *rh-ivf* :start 44 :end (+ 44 sz))) (ptrs (rh-copy-planes pic (first *rh-bufs*))) (y (reel.decode::picture-y pic)) (yo (reel.decode::picture-y-offset pic)) (b (first *rh-bufs*)) (r nil)) (dotimes (c 8) (push (mem-ref (+ b c) :u8) r)) (list ptrs (list (aref y yo) (aref y (+ yo 1)) (aref y (+ yo 2))) (reverse r))))", 4.0)
ev("(if (fboundp (quote rh-probe)) 1 0)")
v=ev("(rh-probe)",600.0); print("  PROBE %s"%v,flush=True)
