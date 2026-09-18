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
rd_until(r"never",2.0)
ev("(progn (in-package :cl-user) (package-name *package*))")
ev("(setq *jit-hot-only* nil)")
ev("(list :armclk (hdmi-arm-clock-max))",30.0)
ev("(init)",180.0)
ev("(list :eager (jit-eager))",900.0)   # re-link the restored core (the working session always ran it)   # HDMI framebuffer: play-ivf draws; without it a restored core faults silently on the first pass
ev("(list :base (reel-demo-pass 1))",120.0)
ev("(progn (defvar reel.decode::*o-loopf* (symbol-function (quote reel.decode::loop-filter-frame))) (defvar reel.decode::*t-loopf* 0) :ok-loopf)",10.0)
ev("(progn (defun reel.decode::loop-filter-frame (vd key) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-loopf* vd key) (setq reel.decode::*t-loopf* (+ reel.decode::*t-loopf* (- (rdtsc) t0)))))) :def-loopf)",20.0)
ev("(list :native (%jit-fn-native-p \"REEL.DECODE::LOOP-FILTER-FRAME\") (cntfrq))",10.0)
ev("(list :one-wrapper (reel-demo-pass 1) (round reel.decode::*t-loopf* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-residue* (symbol-function (quote reel.decode::decode-residue))) (defvar reel.decode::*t-residue* 0) :ok-residue)",10.0)
ev("(progn (defun reel.decode::decode-residue (d bd mbx skip has-y2 dq) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-residue* d bd mbx skip has-y2 dq) (setq reel.decode::*t-residue* (+ reel.decode::*t-residue* (- (rdtsc) t0)))))) :def-residue)",20.0)
ev("(list :two-wrappers (reel-demo-pass 1) (round reel.decode::*t-residue* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-mbloop* (symbol-function (quote reel.decode::decode-macroblocks))) (defvar reel.decode::*t-mbloop* 0) :ok-mbloop)",10.0)
ev("(progn (defun reel.decode::decode-macroblocks (vd part0 tokens nparts key) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-mbloop* vd part0 tokens nparts key) (setq reel.decode::*t-mbloop* (+ reel.decode::*t-mbloop* (- (rdtsc) t0)))))) :def-mbloop)",20.0)
ev("(list :three-wrappers (reel-demo-pass 1) (round reel.decode::*t-mbloop* (* 90 (floor (cntfrq) 1000))))",120.0)
print("=== BISECT DONE ===",flush=True)
