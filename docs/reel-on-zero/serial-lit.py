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
ev("(if (boundp (quote *aarch64-jit-constvec-p*)) *aarch64-jit-constvec-p* :unbound)")
ev("(mem-ref 268439312 :u64)")   # #x10000F10 constvec root
ev("(setq *jit-hot-only* nil)")
raw('(defun words (dx dy dw dh sw sh p0 p1 p2 ys cs ks) (let ((ppf (lambda (s d) (logior (ash 1 30) (ash (floor (* 65536 s) d) 8))))) (list (logior (ash 1 30) (ash 28 24) 8) (logior #xFF000000 (ash dy 12) dx) (logior (ash dh 16) dw) (logior (ash 1 30) (ash sh 16) sw) #xC0C0C0C0 (logior #xC0000000 p0) (logior #xC0000000 p1) (logior #xC0000000 p2) #xC0C0C0C0 #xC0C0C0C0 #xC0C0C0C0 ys cs cs #x00f00000 #xe73304a8 #x00066604 0 (funcall ppf (ash sw -1) dw) (funcall ppf (ash sh -1) dh) #xC0C0C0C0 (funcall ppf sw dw) (funcall ppf sh dh) #xC0C0C0C0 ks ks ks ks #x80000000)))', 4.0)
ev("(words 0 60 1920 1080 320 180 340025376 340114448 340139024 384 192 2100)",60.0)
ev("(list #xC0C0C0C0 #xC0000000 #xFF000000 #x80000000 4278435840)",30.0)
print("EXP (1543503880 4278435840 70780800 1085538624 3233857728 3561250848 3561339920 3561364496 3233857728 3233857728 3233857728 384 192 192 15728640 3878880424 419332 0 1075139840 1075139840 3233857728 1076537856 1076537856 3233857728 2100 2100 2100 2100 2147483648)")
