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
raw('(defun pmu-pmcr-read () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577453568 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))',3.0)
raw('(defun pmu-init () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3531604129 3575356417 3534749697 3575356449 3573759967 3577453568 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))',3.0)
raw('(defun pmu-cycles () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577453824 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))',3.0)
raw('(defun pmu-ctr-read () (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list 3577413664 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))',3.0)
raw("(defun pmu-event-setup (n ev) (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list (logior 3531603969 (ash n 5)) 3575356577 (logior 3531603969 (ash ev 5)) 3575356705 (logior 3531603969 (ash (ash 1 n) 5)) 3575356449 3573759967 3531603968 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
raw("(defun pmu-event-read (n) (let ((p (%mmap-exec-page 4096)) (i 0)) (dolist (w (list (logior 3531603969 (ash n 5)) 3575356577 3573759967 3577453888 3548379136 3596551104)) (setf (mem-ref (+ p i) :u32) w) (setq i (+ i 4))) (%jit-icache-flush p 128) (%jit-call p)))",3.0)
ev("(list :pmcr (pmu-pmcr-read))",20.0)
ev("(list :pmcr-after-init (pmu-init))",20.0)
ev("(list :cyc (pmu-cycles) (pmu-cycles))",20.0)
ev("(let ((a (pmu-cycles))) (addloop 1000000) (list :cycles-per-addloop-iter (/ (logand (- (pmu-cycles) a) 4294967295) 1000000.0)))",120.0)
ev("(list :setup (pmu-event-setup 0 8) (pmu-event-setup 1 16))",20.0)
ev("(let ((c (pmu-cycles)) (i (pmu-event-read 0)) (b (pmu-event-read 1))) (addloop 1000000) (list :cycles (logand (- (pmu-cycles) c) 4294967295) :inst (logand (- (pmu-event-read 0) i) 4294967295) :brmis (logand (- (pmu-event-read 1) b) 4294967295)))",120.0)
ev("(list :temp-mC (hdmi-mbox-property-1 196614 0 0) :clock (hdmi-arm-clock))",20.0)
print("=== PMU DONE ===",flush=True)
