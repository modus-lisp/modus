#!/usr/bin/env python3
"""Record the Zero 2 W playing clips on its HDMI output — clips delivered over HTTP.

Runs on modus-pi (the Pi 5 that holds the Zero's serial, RUN line, RTL8153 link
and the webcam).  One session:
  1. netboot IMG + CORE (netboot-core-gz.py), the core already holding reel;
  2. over SERIAL: (setq *jit-hot-only* nil) (jit-eager) — a restored core must
     re-link before its first decode (BOARD-RUNBOOK 2026-09-28) — then (ssh-boot);
  3. over SSH (test@10.0.0.2): push reel-hvs-forms.txt (the core's copy is stale), (rh-init), then per clip (rh-load "http://10.0.0.1:8099/CLIP")
     from `python3 -m http.server 8099` in /home/modus, and (rh-play t) LOOPS times
     while ffmpeg records /dev/video0 to OUT-CLIP.mkv, then trims the ssh
     handshake lead-in (~12 s per connection) into OUT-CLIP.mp4.

  python3 board-video.py --img board-z.img.gz --core reel-z.core \
      --clip bars.ivf --clip testsrc2.ivf --loops 3
"""
import argparse, re, subprocess, sys, time, random
import serial

ap = argparse.ArgumentParser()
ap.add_argument("--img", required=True); ap.add_argument("--core", required=True)
ap.add_argument("--clip", action="append", required=True)
ap.add_argument("--loops", type=int, default=3)
ap.add_argument("--http", default="http://10.0.0.1:8099")
ap.add_argument("--out", default="zero")
ap.add_argument("--pre-go", action="append", default=[], help="U-Boot command(s) to run before `go`, e.g. \"dcache off\" \"icache off\" to hand over MMU/caches OFF like the firmware does")
ap.add_argument("--unzip-addr", default=None, help="netboot: where to unzip the image (default 0x300000)")
ap.add_argument("--boot-cmd", default=None, help="netboot: U-Boot command that starts the image instead of `go 0x300000`, e.g. a booti of an ARM64-Image-wrapped copy")
ap.add_argument("--no-boot", action="store_true", help="board already up with the network; skip netboot/serial")
ap.add_argument("--forms", default="/home/modus/reel-hvs-forms.txt")
a = ap.parse_args()

def log(*x): print(*x, flush=True)

# 1. netboot
if a.no_boot: log("=== no-boot: using the live board")
else:
  log("=== netboot", a.img, a.core)
  r = subprocess.run(["python3", "netboot-core-gz.py", "--img", a.img, "--core", a.core, "--tftp-tries", "6"] + sum([["--pre-go", c] for c in a.pre_go], []) + (["--unzip-addr", a.unzip_addr] if a.unzip_addr else []) + (["--boot-cmd", a.boot_cmd] if a.boot_cmd else []),
                     capture_output=True, text=True, cwd="/home/modus")
  open("/home/modus/%s-netboot.log" % a.out, "w").write(r.stdout)   # U-Boot transcript: pre-go echoes, ARMCLK line
  if "CORE-RESTORED" not in r.stdout: log(r.stdout[-800:]); sys.exit("netboot failed")
  for key in ("pre-go", "ARMCLK"):
    for l in r.stdout.split("\n"):
      if key in l: log("  netboot:", l.strip()[:100])
  time.sleep(15)

  # 2. serial: re-link, then network
  s = serial.Serial("/dev/ttyAMA0", 115200, timeout=0.3)
  def rd_until(pat, to):
      e = time.time() + to; b = b""
      while time.time() < e:
          d = s.read(8192)
          if d: b += d
          if re.search(pat, b.decode("utf-8", "replace")): break
      return b.decode("utf-8", "replace").replace("\0", "")
  def sq(form, to):
      t = random.randint(1000, 9999); s.reset_input_buffer()
      for ch in "(list %d %s)\r\n" % (t, form): s.write(ch.encode()); time.sleep(0.006)
      out = rd_until(r"\(%d .+?\)\s*\n" % t, to)
      m = re.search(r"\(%d (.+?)\)\s*\n" % t, out + "\n")
      v = m.group(1) if m else None; log("  serial %-40s => %s" % (form[:40], v)); return v
  for _ in range(8):
      if sq("(+ 21 21)", 3.0) == "42": break
      time.sleep(2)
  else: sys.exit("serial REPL not ready")
  sq("(setq *jit-hot-only* nil)", 60); sq("(list :eager (jit-eager))", 900)
  log("=== ssh-boot"); s.write(b"(ssh-boot)\r\n"); s.close()

# 3. ssh
SSH = ["ssh", "-n", "-o", "StrictHostKeyChecking=no", "-o", "UserKnownHostsFile=/dev/null", "-o", "ConnectTimeout=20",
       "-o", "PreferredAuthentications=password", "-o", "PubkeyAuthentication=no", "test@10.0.0.2"]
def run(form, to=120):
    for _ in range(3):
        try:
            r = subprocess.run(SSH + [form], capture_output=True, text=True, timeout=to, stdin=subprocess.DEVNULL)
            out = [l[2:] for l in r.stdout.replace("\0", "").replace("\r", "").split("\n") if l.startswith("= ")]
            if out: log("  ssh %-40s => %s" % (form[:40], out[-1][:160])); return out[-1]
        except subprocess.TimeoutExpired: pass
        time.sleep(5)
    log("  ssh %-40s => NO ANSWER" % form[:40]); return None
deadline = time.time() + 240
while time.time() < deadline and run("(+ 2 3)", 30) != "5": time.sleep(5)
run("(setq *jit-hot-only* nil)")
# the restored core carries whatever rh-* forms it was saved with; push the current ones
for form in open(a.forms).read().split("\n"):
    if form.startswith("(def"): run(form, 120)
bufs = run("(rh-init)", 300)
if not bufs or int(bufs.strip("()").split()[0]) < 0x1000000: sys.exit("rh-init returned a suspicious buffer address: %s" % bufs)
for clip in a.clip:
    log("=== clip", clip)
    if run('(rh-load "%s/%s")' % (a.http, clip), 300) is None: continue
    out = "/home/modus/%s-%s.mkv" % (a.out, clip.rsplit(".", 1)[0])
    rec = subprocess.Popen(["ffmpeg", "-hide_banner", "-loglevel", "error", "-f", "v4l2", "-input_format", "mjpeg",
                            "-video_size", "1280x720", "-framerate", "30", "-i", "/dev/video0", "-t", "120", "-c:v", "copy", "-y", out])
    time.sleep(2)
    t0 = time.time()
    res = run("(let ((r nil)) (dotimes (i %d) (setq r (rh-play t))) r)" % a.loops, 600) or ""
    t1 = time.time()
    time.sleep(1); rec.terminate(); rec.wait(); log("  recorded", out)
    # Each ssh connection costs the board's key exchange (~12 s) before the
    # form even starts, so the recording opens on the previous screen.  The
    # play's own TOTAL-MS x loops says how long it ran; it ended when run()
    # returned, so trim to [end - play - 1 s, end] and encode to h264.
    m = re.search(r"TOTAL-MS (\d+)", res)
    if m:
        play = int(m.group(1)) * a.loops / 1000.0
        start = max(0.0, (t1 - t0 - 2) - play - 1.0)   # rec started 2 s before t0
        mp4 = out[:-4] + ".mp4"
        subprocess.run(["ffmpeg", "-hide_banner", "-loglevel", "error", "-ss", "%.2f" % start, "-i", out,
                        "-c:v", "libx264", "-preset", "veryfast", "-crf", "24", "-pix_fmt", "yuv420p", "-movflags", "+faststart", "-y", mp4])
        log("  trimmed %.1fs of ssh lead-in -> %s" % (start, mp4))
log("=== BOARD-VIDEO DONE")
