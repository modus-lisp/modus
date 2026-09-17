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
ev("(list :after-loopf (reel-demo-pass 1) (round reel.decode::*t-loopf* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-residue* (symbol-function (quote reel.decode::decode-residue))) (defvar reel.decode::*t-residue* 0) :ok-residue)",10.0)
ev("(progn (defun reel.decode::decode-residue (d bd mbx skip has-y2 dq) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-residue* d bd mbx skip has-y2 dq) (setq reel.decode::*t-residue* (+ reel.decode::*t-residue* (- (rdtsc) t0)))))) :def-residue)",20.0)
ev("(list :after-residue (reel-demo-pass 1) (round reel.decode::*t-residue* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-mbloop* (symbol-function (quote reel.decode::decode-macroblocks))) (defvar reel.decode::*t-mbloop* 0) :ok-mbloop)",10.0)
ev("(progn (defun reel.decode::decode-macroblocks (vd part0 tokens nparts key) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-mbloop* vd part0 tokens nparts key) (setq reel.decode::*t-mbloop* (+ reel.decode::*t-mbloop* (- (rdtsc) t0)))))) :def-mbloop)",20.0)
ev("(list :after-mbloop (reel-demo-pass 1) (round reel.decode::*t-mbloop* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-luma16* (symbol-function (quote reel.decode::reconstruct-luma16))) (defvar reel.decode::*t-luma16* 0) :ok-luma16)",10.0)
ev("(progn (defun reel.decode::reconstruct-luma16 (d mx my ymode ha hl) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-luma16* d mx my ymode ha hl) (setq reel.decode::*t-luma16* (+ reel.decode::*t-luma16* (- (rdtsc) t0)))))) :def-luma16)",20.0)
ev("(list :after-luma16 (reel-demo-pass 1) (round reel.decode::*t-luma16* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-bpred* (symbol-function (quote reel.decode::reconstruct-bpred))) (defvar reel.decode::*t-bpred* 0) :ok-bpred)",10.0)
ev("(progn (defun reel.decode::reconstruct-bpred (d mx my) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-bpred* d mx my) (setq reel.decode::*t-bpred* (+ reel.decode::*t-bpred* (- (rdtsc) t0)))))) :def-bpred)",20.0)
ev("(list :after-bpred (reel-demo-pass 1) (round reel.decode::*t-bpred* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-chroma* (symbol-function (quote reel.decode::reconstruct-chroma))) (defvar reel.decode::*t-chroma* 0) :ok-chroma)",10.0)
ev("(progn (defun reel.decode::reconstruct-chroma (d mbx mby uvmode ha hl) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-chroma* d mbx mby uvmode ha hl) (setq reel.decode::*t-chroma* (+ reel.decode::*t-chroma* (- (rdtsc) t0)))))) :def-chroma)",20.0)
ev("(list :after-chroma (reel-demo-pass 1) (round reel.decode::*t-chroma* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-interpred* (symbol-function (quote reel.decode::predict-inter-mb))) (defvar reel.decode::*t-interpred* 0) :ok-interpred)",10.0)
ev("(progn (defun reel.decode::predict-inter-mb (vd mi mbx mby mode) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-interpred* vd mi mbx mby mode) (setq reel.decode::*t-interpred* (+ reel.decode::*t-interpred* (- (rdtsc) t0)))))) :def-interpred)",20.0)
ev("(list :after-interpred (reel-demo-pass 1) (round reel.decode::*t-interpred* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-interres* (symbol-function (quote reel.decode::add-inter-residual))) (defvar reel.decode::*t-interres* 0) :ok-interres)",10.0)
ev("(progn (defun reel.decode::add-inter-residual (d mx my cx cy) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-interres* d mx my cx cy) (setq reel.decode::*t-interres* (+ reel.decode::*t-interres* (- (rdtsc) t0)))))) :def-interres)",20.0)
ev("(list :after-interres (reel-demo-pass 1) (round reel.decode::*t-interres* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-mbmodes* (symbol-function (quote reel.decode::read-mb-modes))) (defvar reel.decode::*t-mbmodes* 0) :ok-mbmodes)",10.0)
ev("(progn (defun reel.decode::read-mb-modes (d part0 mbx) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-mbmodes* d part0 mbx) (setq reel.decode::*t-mbmodes* (+ reel.decode::*t-mbmodes* (- (rdtsc) t0)))))) :def-mbmodes)",20.0)
ev("(list :after-mbmodes (reel-demo-pass 1) (round reel.decode::*t-mbmodes* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-intermodes* (symbol-function (quote reel.decode::read-inter-modes))) (defvar reel.decode::*t-intermodes* 0) :ok-intermodes)",10.0)
ev("(progn (defun reel.decode::read-inter-modes (vd part0 mi mbx mby) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-intermodes* vd part0 mi mbx mby) (setq reel.decode::*t-intermodes* (+ reel.decode::*t-intermodes* (- (rdtsc) t0)))))) :def-intermodes)",20.0)
ev("(list :after-intermodes (reel-demo-pass 1) (round reel.decode::*t-intermodes* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-intramodes* (symbol-function (quote reel.decode::read-intra-modes-inter-frame))) (defvar reel.decode::*t-intramodes* 0) :ok-intramodes)",10.0)
ev("(progn (defun reel.decode::read-intra-modes-inter-frame (vd part0) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-intramodes* vd part0) (setq reel.decode::*t-intramodes* (+ reel.decode::*t-intramodes* (- (rdtsc) t0)))))) :def-intramodes)",20.0)
ev("(list :after-intramodes (reel-demo-pass 1) (round reel.decode::*t-intramodes* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-lfparams* (symbol-function (quote reel.decode::mb-filter-params))) (defvar reel.decode::*t-lfparams* 0) :ok-lfparams)",10.0)
ev("(progn (defun reel.decode::mb-filter-params (vd d mi key) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-lfparams* vd d mi key) (setq reel.decode::*t-lfparams* (+ reel.decode::*t-lfparams* (- (rdtsc) t0)))))) :def-lfparams)",20.0)
ev("(list :after-lfparams (reel-demo-pass 1) (round reel.decode::*t-lfparams* (* 90 (floor (cntfrq) 1000))))",120.0)
ev("(progn (defvar reel.decode::*o-refcopy* (symbol-function (quote reel.decode::plane->rframe-plane))) (defvar reel.decode::*t-refcopy* 0) :ok-refcopy)",10.0)
ev("(progn (defun reel.decode::plane->rframe-plane (pl dst stride border w h) (let ((t0 (rdtsc))) (multiple-value-prog1 (funcall reel.decode::*o-refcopy* pl dst stride border w h) (setq reel.decode::*t-refcopy* (+ reel.decode::*t-refcopy* (- (rdtsc) t0)))))) :def-refcopy)",20.0)
ev("(list :after-refcopy (reel-demo-pass 1) (round reel.decode::*t-refcopy* (* 90 (floor (cntfrq) 1000))))",120.0)
print("=== BISECT DONE ===",flush=True)
