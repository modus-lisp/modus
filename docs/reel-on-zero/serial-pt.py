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
ev("(setq *jit-hot-only* nil)")
ev("(rh-init)",120.0)
ev("(let* ((l1 (logand (mem-ref 458752 :u64) (lognot 4095))) (b (first *rh-bufs*)) (idx (ash b -21)) (e (mem-ref (+ l1 (* idx 8)) :u64))) (list :buf b :l1 l1 :idx idx :entry e :pa (logand e #xFFFFE00000) :attr (logand e #xFFF)))",30.0)
ev("(let* ((l1 (logand (mem-ref 458752 :u64) (lognot 4095))) (r nil)) (dolist (va (list #x00300000 #x14000000 #x14400000 #x14800000 #x18000000 #x30000000 #x3F400000)) (let ((e (mem-ref (+ l1 (* (ash va -21) 8)) :u64))) (push (list va (logand e #xFFFFE00000) (logand e #xFFF)) r))) (reverse r))",30.0)
