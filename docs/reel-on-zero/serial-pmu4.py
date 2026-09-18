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
raw("(defun dfr0 () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577218304 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
raw("(defun ceid1 () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577453792 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
raw("(defun pmu-init2 () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3531606241 3575356417 3533701089 4072669153 3575356481 3533701089 4072669153 3575160385 3533701089 4072669153 3575356513 3533766657 3575377889 3573759967 3577453568 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
raw("(defun ev0-swincr () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3533766657 3575376897 3531604001 3575356449 3573759967 3531603968 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
raw("(defun swinc () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3531604001 3575356545 3573759967 3577473024 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
raw("(defun ev0-inst () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3531604225 4070703105 3575376897 3531604001 3575356449 3573759967 3531603968 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
raw("(defun cntr0 () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577473024 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
ev("(progn (defun addloop (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s)) (addloop 10))",20.0)
ev("(list :dfr0 (dfr0) :ceid1 (ceid1))",10.0)
ev("(list :pmcr-after-init2 (pmu-init2))",10.0)
ev("(list :swincr-setup (ev0-swincr) :after-3-swinc (progn (swinc) (swinc) (swinc)))",10.0)
ev("(list :inst-setup (ev0-inst) :cntr0-before (cntr0) :after-addloop (progn (addloop 100000) (cntr0)))",60.0)
print("=== PMU4 DONE ===",flush=True)
