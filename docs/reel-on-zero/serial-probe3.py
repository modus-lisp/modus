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
raw("(defun rh-vis () (let* ((vd (reel:make-decoder)) (sz (rh-le32 *rh-ivf* 32)) (pic (reel:decode-frame vd *rh-ivf* :start 44 :end (+ 44 sz))) (ptrs (rh-copy-planes pic (first *rh-bufs*))) (y (reel.decode::picture-y pic)) (yo (reel.decode::picture-y-offset pic)) (p (first ptrs)) (a nil) (b nil)) (dotimes (c 8) (push (aref y (+ yo c)) a) (push (mem-ref (+ p c) :u8) b)) (list (reverse a) (reverse b))))", 4.0)
v=ev("(rh-vis)",600.0); print("  VIS aref-vs-NCbuf %s"%v,flush=True)
ev("(let ((ppf (lambda (s d) (logior (ash 1 30) (ash (floor (* 65536 s) d) 8))))) (list (funcall ppf 320 1920) (funcall ppf 180 1080)))",30.0)
raw("(defun fnv4k () (let ((h 2166136261) (i 0)) (loop (when (>= i 4096) (return h)) (setq h (logand (* (logxor h (aref *rh-ivf* i)) 16777619) #xFFFFFFFF)) (setq i (+ i 1)))))",4.0)
ev("(fnv4k)",120.0)
