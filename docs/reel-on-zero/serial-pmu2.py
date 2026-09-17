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
ev("(progn (defun rb-enset () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577453600 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p))) :rb-enset)",10.0)
ev("(progn (defun rb-typer0 () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577474048 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p))) :rb-typer0)",10.0)
ev("(progn (defun rb-cntr0 () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577473024 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p))) :rb-cntr0)",10.0)
ev("(progn (defun rb-ceid0 () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577453760 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p))) :rb-ceid0)",10.0)
ev("(progn (defun rb-pmcr () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577453568 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p))) :rb-pmcr)",10.0)
ev("(progn (defun rb-selr () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577453728 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p))) :rb-selr)",10.0)
ev("(list :init (pmu-init) :setup (pmu-event-setup 0 8) :enset (rb-enset) :typer0 (rb-typer0) :selr (rb-selr) :pmcr (rb-pmcr) :ceid0 (rb-ceid0))",20.0)
ev("(let ((a (rb-cntr0)) (b (pmu-event-read 0))) (addloop 100000) (list :cntr0-before a :xev-before b :cntr0-after (rb-cntr0) :xev-after (pmu-event-read 0)))",60.0)
print("=== PMU2 DONE ===",flush=True)
