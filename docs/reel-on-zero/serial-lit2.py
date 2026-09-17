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
ev("(hvs-mair-words 4096)",30.0)
ev("(cddr (hvs-blit-nc-words :copy 4096))",30.0)
ev("(list (hvs-ppf-word 0 -2 -6) (hvs-base) (list *hvs-attr* *hvs-blit-nc*))",30.0)
ev("(list (%jit-fn-native-p \"HVS-MAIR-WORDS\") (%jit-fn-native-p \"HVS-SLOT-WR32\") (%jit-fn-native-p \"HVS-WINDOW-NC\"))",30.0)
raw("(defun hvs-mair-words-fresh (scr) (list (logior #xD2800003 (ash (logand scr #xFFFF) 5)) (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5)) #xF9400060 #xD5033F9F #xD51CA200 #xD50C871F #xD5033F9F #xD5033FDF #xD65F03C0))",4.0)
ev("(hvs-mair-words-fresh 4096)",30.0)
ev("(jit-eager)",2400.0)
ev("(hvs-mair-words 4096)",30.0)
