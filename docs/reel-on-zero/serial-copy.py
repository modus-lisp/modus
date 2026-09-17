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
ev("(setq *jit-hot-only* nil)"); ev("(jit-eager)",2400.0)
ev("(list (rh-ceil64 98304) (rh-ceil64 24576) (rh-ceil64 100))",30.0)
ev("(rh-init)",120.0)
raw("(defun rh-pic0 () (let* ((vd (reel:make-decoder)) (sz (rh-le32 *rh-ivf* 32))) (reel:decode-frame vd *rh-ivf* :start 44 :end (+ 44 sz))))",4.0)
ev("(progn (setq *pic* (rh-pic0)) (length (reel.decode::picture-y *pic*)))",600.0)
ev("(let ((b (first *rh-bufs*))) (hvs-nfill-nc b 2097152 #x11111111) (list (mem-ref b :u8) (mem-ref (+ b 2000000) :u8)))",60.0)
ev("(rh-copy-planes *pic* (first *rh-bufs*))",120.0)
ev("(let* ((b (first *rh-bufs*)) (y (reel.decode::picture-y *pic*)) (r nil)) (dolist (row (list 0 32 60 100 150 200 240)) (let ((o (* row 384))) (push (list row (mem-ref (+ b o 40) :u8) (aref y (+ o 40))) r))) (reverse r))",60.0)
ev("(let* ((b (first *rh-bufs*)) (u (reel.decode::picture-u *pic*))) (list :u0 (mem-ref (+ b 98304) :u8) (aref u 0) :u3088 (mem-ref (+ b 98304 3088) :u8) (aref u 3088) :v3088 (mem-ref (+ b 98304 24576 3088) :u8)))",60.0)
