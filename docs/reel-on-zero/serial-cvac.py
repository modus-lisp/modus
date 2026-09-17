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
def shot(name): subprocess.run(["ffmpeg","-hide_banner","-loglevel","error","-f","v4l2","-input_format","mjpeg","-video_size","1280x720","-i","/dev/video0","-frames:v","3","-y","/home/modus/"+name+"-%d.jpg"])
ev("(setq *jit-hot-only* nil)")
ev("(let* ((l1 (logand (mem-ref 458752 :u32) (lognot 4095))) (b (first *rh-bufs*)) (e (+ l1 (* (ash b -21) 8)))) (list :buf b :lo (mem-ref e :u32) :hi (mem-ref (+ e 4) :u32)))",30.0)
ev("(rh-yuv-plane *pic* (first *rh-bufs*) 0 60 1920 1080)",120.0); shot("nc")
raw("(defun rh-copy-planes (pic buf) (let* ((y (reel.decode::picture-y pic)) (u (reel.decode::picture-u pic)) (v (reel.decode::picture-v pic)) (ysz (rh-ceil64 (length y))) (usz (rh-ceil64 (length u)))) (hvs-ncopy buf (+ (%val->word y) 7) ysz) (hvs-ncopy (+ buf ysz) (+ (%val->word u) 7) usz) (hvs-ncopy (+ buf ysz usz) (+ (%val->word v) 7) usz) (list (+ buf (reel.decode::picture-y-offset pic)) (+ buf ysz (reel.decode::picture-uv-offset pic)) (+ buf ysz usz (reel.decode::picture-uv-offset pic)))))",4.0)
ev("(rh-yuv-plane *pic* (first *rh-bufs*) 0 60 1920 1080)",120.0); shot("cvac")
ev("(let* ((b (first *rh-bufs*)) (y (reel.decode::picture-y *pic*))) (list (mem-ref (+ b (* 100 384) 40) :u8) (aref y (+ (* 100 384) 40))))",30.0)
print("=== CVAC DONE ===",flush=True)
