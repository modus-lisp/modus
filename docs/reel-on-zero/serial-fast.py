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
ev('(if (find-package "REEL") 1 0)'); ev("(setq *jit-hot-only* nil)")
def raw(t,settle):
    s.reset_input_buffer()
    for ch in t+"\r\n": s.write(ch.encode()); time.sleep(0.006)
    return rd_until(r"never",settle)
raw('(defun rh-yuv-plane (pic buf dx dy dw dh) (let* ((sw (reel.decode::picture-width pic)) (sh (reel.decode::picture-height pic)) (ys (reel.decode::picture-y-stride pic)) (cs (reel.decode::picture-uv-stride pic)) (ptrs (rh-copy-planes pic buf)) (ks *hvs-kernel-slot*) (ppf (lambda (s d) (logior (ash 1 30) (ash (floor (* 65536 s) d) 8)))) (words (list (logior (ash 1 30) (ash 28 24) 8) (logior #xFF000000 (ash dy 12) dx) (logior (ash dh 16) dw) (logior (ash 1 30) (ash sh 16) sw) #xC0C0C0C0 (logior #xC0000000 (first ptrs)) (logior #xC0000000 (second ptrs)) (logior #xC0000000 (third ptrs)) #xC0C0C0C0 #xC0C0C0C0 #xC0C0C0C0 ys cs cs #x00f00000 #xe73304a8 #x00066604 0 (funcall ppf (ash sw -1) dw) (funcall ppf (ash sh -1) dh) #xC0C0C0C0 (funcall ppf sw dw) (funcall ppf sh dh) #xC0C0C0C0 ks ks ks ks #x80000000)) (i 0)) (hvs-window-nc t) (hvs-upload-kernel) (dolist (w words) (hvs-slot-wr32 (+ *rh-plane* i) w) (setq i (+ i 1))) (setf (mem-ref (+ (hvs-base) #x24) :u32) *rh-plane*) (hvs-window-nc nil) (list sw sh ys cs 0)))', 4.0)
ev("(if (fboundp (quote rh-yuv-plane)) 1 0)")
ev("(rh-init)",120.0); ev("(rh-first-frame)",600.0)
subprocess.run(["ffmpeg","-hide_banner","-loglevel","error","-f","v4l2","-input_format","mjpeg","-video_size","1280x720","-i","/dev/video0","-frames:v","1","-y","/home/modus/fast-first.jpg"])
ff=subprocess.Popen(["ffmpeg","-hide_banner","-loglevel","error","-f","v4l2","-input_format","mjpeg","-video_size","1280x720","-framerate","30","-t","15","-i","/dev/video0","-c:v","libx264","-preset","veryfast","-y","/home/modus/fast-play.mp4"])
time.sleep(1); ev("(rh-play nil)",900.0); ff.wait()
print("=== FAST DONE ===",flush=True)
