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
ev("(list (hvs-base) (mem-ref 458752 :u32) (mem-ref (+ (hvs-base) 8192 (* 3000 4)) :u32) (mem-ref (+ (hvs-base) 8192 (* 3001 4)) :u32))",30.0)
ev("(progn (hvs-window-nc t) (list *hvs-attr*))",60.0)
ev("(progn (hvs-slot-wr32 3000 #xC0C0C0C0) (hvs-slot-wr32 3001 #x12345678) (hvs-slot-wr32 3002 305419896) :written)",30.0)
ev("(list :read-under-nc (mem-ref (+ (hvs-base) 8192 (* 3000 4)) :u32) (mem-ref (+ (hvs-base) 8192 (* 3001 4)) :u32))",30.0)
ev("(progn (hvs-window-nc nil) (list :read-under-dev (mem-ref (+ (hvs-base) 8192 (* 3000 4)) :u32) (mem-ref (+ (hvs-base) 8192 (* 3001 4)) :u32) (mem-ref (+ (hvs-base) 8192 (* 3002 4)) :u32)))",30.0)
ev("(progn (setf (mem-ref (+ (hvs-base) 8192 (* 3003 4)) :u32) #xC0C0C0C0) (list :dev-store-readback (mem-ref (+ (hvs-base) 8192 (* 3003 4)) :u32)))",30.0)
print("EXPECT nc/dev readback 3233857728 305419896 305419896")
