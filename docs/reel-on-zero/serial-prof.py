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
ev("(list :armclk (hdmi-arm-clock))",30.0)
raw('(in-package :reel.decode)',3.0)
raw('(defvar *o-residue* (symbol-function (quote decode-residue)))',3.0)
raw('(defvar *t-residue* 0)',3.0)
raw('(defun decode-residue (d bd mbx skip has-y2 dq) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-residue* d bd mbx skip has-y2 dq) (setq *t-residue* (+ *t-residue* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-luma16* (symbol-function (quote reconstruct-luma16)))',3.0)
raw('(defvar *t-luma16* 0)',3.0)
raw('(defun reconstruct-luma16 (d mx my ymode ha hl) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-luma16* d mx my ymode ha hl) (setq *t-luma16* (+ *t-luma16* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-bpred* (symbol-function (quote reconstruct-bpred)))',3.0)
raw('(defvar *t-bpred* 0)',3.0)
raw('(defun reconstruct-bpred (d mx my) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-bpred* d mx my) (setq *t-bpred* (+ *t-bpred* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-chroma* (symbol-function (quote reconstruct-chroma)))',3.0)
raw('(defvar *t-chroma* 0)',3.0)
raw('(defun reconstruct-chroma (d mbx mby uvmode ha hl) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-chroma* d mbx mby uvmode ha hl) (setq *t-chroma* (+ *t-chroma* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-interpred* (symbol-function (quote predict-inter-mb)))',3.0)
raw('(defvar *t-interpred* 0)',3.0)
raw('(defun predict-inter-mb (vd mi mbx mby mode) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-interpred* vd mi mbx mby mode) (setq *t-interpred* (+ *t-interpred* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-interres* (symbol-function (quote add-inter-residual)))',3.0)
raw('(defvar *t-interres* 0)',3.0)
raw('(defun add-inter-residual (d mx my cx cy) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-interres* d mx my cx cy) (setq *t-interres* (+ *t-interres* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-mbmodes* (symbol-function (quote read-mb-modes)))',3.0)
raw('(defvar *t-mbmodes* 0)',3.0)
raw('(defun read-mb-modes (d part0 mbx) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-mbmodes* d part0 mbx) (setq *t-mbmodes* (+ *t-mbmodes* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-intermodes* (symbol-function (quote read-inter-modes)))',3.0)
raw('(defvar *t-intermodes* 0)',3.0)
raw('(defun read-inter-modes (vd part0 mi mbx mby) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-intermodes* vd part0 mi mbx mby) (setq *t-intermodes* (+ *t-intermodes* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-intramodes* (symbol-function (quote read-intra-modes-inter-frame)))',3.0)
raw('(defvar *t-intramodes* 0)',3.0)
raw('(defun read-intra-modes-inter-frame (vd part0) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-intramodes* vd part0) (setq *t-intramodes* (+ *t-intramodes* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-lfparams* (symbol-function (quote mb-filter-params)))',3.0)
raw('(defvar *t-lfparams* 0)',3.0)
raw('(defun mb-filter-params (vd d mi key) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-lfparams* vd d mi key) (setq *t-lfparams* (+ *t-lfparams* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-loopf* (symbol-function (quote loop-filter-frame)))',3.0)
raw('(defvar *t-loopf* 0)',3.0)
raw('(defun loop-filter-frame (vd key) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-loopf* vd key) (setq *t-loopf* (+ *t-loopf* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-refcopy* (symbol-function (quote plane->rframe-plane)))',3.0)
raw('(defvar *t-refcopy* 0)',3.0)
raw('(defun plane->rframe-plane (pl dst stride border w h) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-refcopy* pl dst stride border w h) (setq *t-refcopy* (+ *t-refcopy* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw('(defvar *o-mbloop* (symbol-function (quote decode-macroblocks)))',3.0)
raw('(defvar *t-mbloop* 0)',3.0)
raw('(defun decode-macroblocks (vd part0 tokens nparts key) (let ((t0 (mem-ref 1056980996 :u32))) (multiple-value-prog1 (funcall *o-mbloop* vd part0 tokens nparts key) (setq *t-mbloop* (+ *t-mbloop* (logand (- (mem-ref 1056980996 :u32) t0) 4294967295))))))',3.0)
raw("(in-package :cl-user)",2.0)
ev("(list (fboundp (quote reel.decode::decode-residue)) reel.decode::*t-residue*)",30.0)
ev("(progn (reel-demo-pass 4) :warm)",900.0)
ev('(progn (in-package :reel.decode) (progn (setq *t-residue* 0) (setq *t-luma16* 0) (setq *t-bpred* 0) (setq *t-chroma* 0) (setq *t-interpred* 0) (setq *t-interres* 0) (setq *t-mbmodes* 0) (setq *t-intermodes* 0) (setq *t-intramodes* 0) (setq *t-lfparams* 0) (setq *t-loopf* 0) (setq *t-refcopy* 0) (setq *t-mbloop* 0) :reset))',30.0)
ev("(reel-demo-pass 4)",900.0)
ev("(let ((*package* (find-package :reel.decode))) (eval (read-from-string '(list (list :residue (round *t-residue* 90000)) (list :luma16 (round *t-luma16* 90000)) (list :bpred (round *t-bpred* 90000)) (list :chroma (round *t-chroma* 90000)) (list :interpred (round *t-interpred* 90000)) (list :interres (round *t-interres* 90000)) (list :mbmodes (round *t-mbmodes* 90000)) (list :intermodes (round *t-intermodes* 90000)) (list :intramodes (round *t-intramodes* 90000)) (list :lfparams (round *t-lfparams* 90000)) (list :loopf (round *t-loopf* 90000)) (list :refcopy (round *t-refcopy* 90000)) (list :mbloop (round *t-mbloop* 90000)))')))",60.0)
print("=== PROF DONE (ms per frame, 90 frames) ===",flush=True)
