#!/usr/bin/env python3
"""rpi-core-driver.py -- drive a Pi CL image under QEMU raspi3b over its mini-UART
socket: wait for the banner, install tarballs that QEMU placed in RAM, run a
probe, save-and-die, and report CORE-END.  Used by test/run-rpi-core.sh.

  rpi-core-driver.py SOCK [--tar ADDR:LEN:NAME ...] [--form FORM ...] [--probe FORM] [--save] [--banner-wait S]

--form runs after the installs and before the probe, in order (fonts from RAM, a
(jit-eager) so the saved core is native -- a hot-only core restores every DEFUN
interpreted, since hot means a form re-evaluated, never a function called often).

Prints one line per step; the last lines are PROBE=<reply> and CORE-END=<n>.
Rules it obeys (docs/reel-on-zero/BOARD-RUNBOOK.md 6c): send NOTHING before the
banner; a reply is matched on a unique tag; CORE-END needs a terminator after
the digits (CORE-END=41 was once read out of CORE-END=415417424).
"""
import argparse, re, socket, sys, time, random
ap = argparse.ArgumentParser()
ap.add_argument("sock"); ap.add_argument("--tar", action="append", default=[])
ap.add_argument("--probe", default=None); ap.add_argument("--save", action="store_true")
ap.add_argument("--banner-wait", type=float, default=1800.0); ap.add_argument("--install-wait", type=float, default=5400.0)
ap.add_argument("--expect-restored", action="store_true")
ap.add_argument("--form", action="append", default=[])
a = ap.parse_args()
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
for _ in range(600):
    try: s.connect(a.sock); break
    except OSError: time.sleep(0.5)
s.settimeout(0.3)
log = open(a.sock + ".serial.txt", "ab")
def rd_until(pat, to):
    end = time.time() + to; b = b""
    while time.time() < end:
        try:
            d = s.recv(65536)
            if d: b += d; log.write(d); log.flush()
        except socket.timeout: pass
        if re.search(pat, b.decode("utf-8", "replace")): break
    return b.decode("utf-8", "replace")
def say(*x): print(*x, flush=True)
def q(form, to=30.0):
    t = random.randint(100000, 999999)
    s.sendall(("(list %d %s)\r\n" % (t, form)).encode())
    out = rd_until(r"\(%d .+?\)\s*\n" % t, to)
    m = re.search(r"\(%d (.+?)\)\s*\n" % t, out + "\n")
    return m.group(1) if m else None
banner = rd_until(r"Modus CL REPL", a.banner_wait)
if "Modus CL REPL" not in banner:
    say("FAIL no banner within %ds; serial tail: %r" % (a.banner_wait, banner[-300:])); sys.exit(1)
say("BANNER yes RESTORED=%s" % ("yes" if "CORE-RESTORED" in banner else "no"))
if a.expect_restored and "CORE-RESTORED" not in banner:
    say("FAIL expected CORE-RESTORED before the banner"); sys.exit(1)
time.sleep(2); rd_until(r"never", 1.0)
say("WARMUP", q("(+ 20 22)"))
if a.tar:
    q("(setq *tar-block-size* 512)")
    # RAMV NATIVE: under the default hot-only JIT the defun stays interpreted, and
    # copying a 1.3 MB tarball out of RAM took ~10 min under TCG (native: seconds).
    hot = q("*jit-hot-only*")
    q("(setq *jit-hot-only* nil)")
    s.sendall(b"(defun ramv (a n) (let ((v (make-array n :element-type (quote (unsigned-byte 8))))) (dotimes (i n v) (setf (aref v i) (mem-ref (+ a i) :u8)))))\r\n")
    rd_until(r"\n> ", 30)
    q("(setq *jit-hot-only* %s)" % (hot if hot in ("T", "NIL") else "T"))
for spec in a.tar:
    addr, ln, name = spec.split(":"); t0 = time.time()
    v = q("(progn (setq *kt* (ramv %d %d)) (length *kt*))" % (int(addr, 0), int(ln)), 600)
    say("RAMV %s %s bytes -> %s" % (name, ln, v))
    s.sendall(('(install-tarball-from-bytes *kt* "%s")\r\n' % name).encode())
    out = rd_until(r"install-tarball: done|INCOMPLETE|!! form eval error in [^\n]*\n[^\n]*\n> ", a.install_wait)
    ok = "install-tarball: done" in out
    say("INSTALL %s %s %.0fs" % (name, "ok" if ok else "FAILED", time.time() - t0))
    if not ok: say("FAIL install %s: %r" % (name, out[-400:])); sys.exit(1)
    q("(setq *kt* nil)")
for f in a.form:
    t0 = time.time(); say("FORM %s => %s %.0fs" % (f[:70], q(f, 3600), time.time() - t0))
if a.probe:
    say("PROBE=%s" % q(a.probe, 300))
if a.save:
    s.sendall(b'(save-and-die "core")\r\n')
    out = rd_until(r"CORE-END=\d+[^0-9]", 3600)
    m = re.search(r"CORE-END=(\d+)[^0-9]", out)
    if not m: say("FAIL no CORE-END: %r" % out[-300:]); sys.exit(1)
    n = int(m.group(1)); base = 0x18000000
    end = n if n >= base else base + n          # an END ADDRESS, or a BYTE COUNT (both have been seen)
    say("CORE-END=%d" % end)
