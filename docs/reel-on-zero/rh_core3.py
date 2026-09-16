#!/usr/bin/env python3
"""Board driver for the restored reel core: everything over serial, no network."""
import serial, time, re, random, subprocess, sys
s = serial.Serial("/dev/ttyAMA0", 115200, timeout=0.3); s.reset_input_buffer()
def raw(t, settle):
    for ch in t + "\r\n": s.write(ch.encode()); time.sleep(0.006)
    e = time.time() + settle; b = b""
    while time.time() < e:
        d = s.read(8192)
        if d: b += d; e = time.time() + min(settle, 1.2)
    return b.decode("utf-8", "replace").replace("\0", "")
def rd_until(pat, timeout):
    e = time.time() + timeout; b = b""
    while time.time() < e:
        d = s.read(8192)
        if d: b += d
        if re.search(pat, b.decode("utf-8", "replace")): break
    return b.decode("utf-8", "replace").replace("\0", "")
def q(form, w=8.0):
    t = random.randint(1000, 9999); s.reset_input_buffer()
    for ch in "(list %d %s)\r\n" % (t, form): s.write(ch.encode()); time.sleep(0.006)
    out = rd_until(r"\(%d .+?\)\s*\n" % t, w)
    m = re.search(r"\(%d (.+?)\)\s*\n" % t, out + "\n"); return (m.group(1) if m else None), out
def ev(form, w=8.0):
    v, out = q(form, w); print("  %-58s => %s" % (form[:58], v if v is not None else out.strip()[-100:]), flush=True); return v
def capture(name, n=6):
    subprocess.run(["ffmpeg","-hide_banner","-loglevel","error","-f","v4l2","-input_format","mjpeg","-video_size","1280x720",
                    "-i","/dev/video0","-frames:v",str(n),"-y","/home/modus/"+name], stderr=subprocess.DEVNULL)
    print("  captured", name, flush=True)
for _ in range(8):
    raw(")))))))))", 0.5); raw("(+ 0 0)", 0.5)
    v, _ = q("(+ 21 21)", 3.0)
    if v == "42": break
    time.sleep(2)
else: print("REPL NOT READY"); sys.exit(1)
print("READY", flush=True)
ev('(if (find-package "REEL") 1 0)')
ev("(if (fboundp (quote rh-play)) 1 0)")
ev("(list *rh-len* (if *rh-ivf* (length *rh-ivf*) -1))")
ev("(setq *jit-on* t)"); ev("(setq *jit-hot-only* nil)")
ev("(%gc-read64 335544304)")                       # arena bump word 0x13FFFFF0 (stale after a core restore)
a = ev("(%mmap-exec-page 4096)")
if a is None or int(a) < 0x14000000 or int(a) >= 0x18000000: print("ARENA BROKEN (%s) — abort" % a); sys.exit(2)
ev("(rh-init)", 60.0)
print("=== first frame ===", flush=True)
ev("(rh-first-frame)", 180.0)
capture("rh-first-08.jpg")
print("=== play (no vsync) ===", flush=True)
ev("(rh-play nil)", 600.0)
subprocess.Popen(["ffmpeg","-hide_banner","-loglevel","error","-f","v4l2","-input_format","mjpeg","-video_size","1280x720",
                  "-framerate","30","-t","12","-y","/home/modus/rh-play.mp4"], stderr=subprocess.DEVNULL)
time.sleep(1)
print("=== play (vsync) ===", flush=True)
ev("(rh-play t)", 600.0)
time.sleep(3)
print("=== DONE ===", flush=True)
