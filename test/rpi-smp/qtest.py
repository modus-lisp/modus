#!/usr/bin/env python3
"""qtest.py IMG -- boot a Pi board image under QEMU raspi3b, wait for the REPL, send forms, print replies."""
import subprocess, socket, sys, time, os, re
img = sys.argv[1]; blob = None
if sys.argv[2].startswith("--blob="): blob = sys.argv[2][7:]; sys.argv.pop(2)
sock = "/tmp/claude-1001/qtest-%d.sock" % os.getpid()
forms = sys.argv[2:]
RAW = open(os.environ["QTEST_RAW"], "ab") if os.environ.get("QTEST_RAW") else None   # whole serial transcript
q = subprocess.Popen(["qemu-system-aarch64", "-M", "raspi3b", "-kernel", img, "-serial", "null",
                      "-serial", "unix:%s,server" % sock, "-display", "none", "-no-reboot"] + (["-device", "loader,file=%s,addr=%s" % (blob, os.environ.get("QTEST_BLOB_ADDR", "0x1A000000"))] if blob else []),
                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
time.sleep(2)
s = socket.socket(socket.AF_UNIX); s.connect(sock); s.settimeout(1)
buf = b""
def until(pat, to):
    global buf
    end = time.time() + to
    while time.time() < end:
        try:
            d = s.recv(65536)
            if d:
                buf += d
                if RAW: RAW.write(d); RAW.flush()
        except socket.timeout: pass
        if re.search(pat, buf): return True
    return False
t0 = time.time()
if os.environ.get("QTEST_POKE"):
    # A restored core with a restart hook starts it unless a byte arrives on serial
    # within 2 s: keep sending one until the hook offer has been answered.
    end = time.time() + 600
    while time.time() < end and not re.search(rb"CORE-RESTART-HOOK|CL REPL", buf):
        until(rb"CORE-RESTART-HOOK|CL REPL", 0.5)
    for i in range(4):
        s.sendall(b"\r"); until(rb"$^", 0.3)
    buf += b"CL REPL\n> "      # the hook's REPL has no banner line; prompt detection below
ok = until(rb"CL REPL[^\n]*\n> ?$|!!FAULT|\n> ?$", 300)
tail = buf[-300:].decode("latin1")
print("BOOT %ds %s" % (time.time() - t0, "FAULT" if b"!!FAULT" in buf else ("REPL" if ok else "TIMEOUT")))
if b"!!FAULT" in buf or not ok: print(tail)
if ok and b"!!FAULT" not in buf:
    for f in forms:
        buf = b""
        s.settimeout(900); s.sendall((f + "\r\n").encode()); s.settimeout(1)
        r = until(rb"\n> ?$|!!FAULT", 900)
        out = buf.decode("latin1").replace("\r", "")
        lines = [l for l in out.split("\n") if l.strip() and not l.startswith(f[:20]) and "WARN" not in l]
        print("FORM", f[:60], "=>", " | ".join(lines[-3:]) if r else "TIMEOUT")
q.kill()
