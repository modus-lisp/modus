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
ev('(if (find-package "REEL") 1 0)'); ev('(if (fboundp (quote reel-demo-pass)) 1 0)'); ev('(if (boundp (quote *demo-ivf*)) *demo-len* -1)')
ev("(setq *jit-hot-only* nil)")
t0=time.time(); print("=== jit-eager (long) ===",flush=True)
ev("(jit-eager)", 2400.0); print("  jit-eager took %.0fs"%(time.time()-t0),flush=True)
print("=== DECODE (NEON, native) ===",flush=True)
for k in range(3): ev("(reel-demo-pass 4)", 900.0)
print("=== FAST HVS ===",flush=True)
ev("(rh-init)",120.0); ev("(rh-first-frame)",600.0)
subprocess.run(["ffmpeg","-hide_banner","-loglevel","error","-f","v4l2","-input_format","mjpeg","-video_size","1280x720","-i","/dev/video0","-frames:v","1","-y","/home/modus/fast-first.jpg"])
ff=subprocess.Popen(["ffmpeg","-hide_banner","-loglevel","error","-f","v4l2","-input_format","mjpeg","-video_size","1280x720","-framerate","30","-t","15","-i","/dev/video0","-c:v","libx264","-preset","veryfast","-y","/home/modus/fast-play.mp4"])
time.sleep(1); ev("(rh-play nil)",900.0); ff.wait()
print("=== ALL DONE ===",flush=True)
