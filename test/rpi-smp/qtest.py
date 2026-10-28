#!/usr/bin/env python3
"""qtest.py IMG -- boot a Pi board image under QEMU raspi3b, wait for the REPL, send forms, print replies."""
import subprocess, socket, sys, time, os, re
img = sys.argv[1]; blob = None
if sys.argv[2].startswith("--blob="): blob = sys.argv[2][7:]; sys.argv.pop(2)
sock = os.environ.get("QTEST_SOCKDIR","/tmp/claude-1001")+"/qtest-%d.sock" % os.getpid()
forms = sys.argv[2:]
RAW = open(os.environ["QTEST_RAW"], "ab") if os.environ.get("QTEST_RAW") else None   # whole serial transcript
q = subprocess.Popen(["qemu-system-aarch64", "-M", "raspi3b", "-kernel", img, "-serial", "null",
                      "-serial", "unix:%s,server" % sock, "-display", "none", "-no-reboot"] + (["-device", "loader,file=%s,addr=%s" % (blob, os.environ.get("QTEST_BLOB_ADDR", "0x1A000000"))] if blob else [])
                     # QTEST_EXTRA="FILE@ADDR,FILE@ADDR": more blobs in guest RAM (a clip, a disk)
                     + sum([["-device", "loader,file=%s,addr=%s" % tuple(e.rsplit("@", 1))]
                            for e in os.environ.get("QTEST_EXTRA", "").split(",") if e], [])
                     # QTEST_QEMU_ARGS: more QEMU arguments, space-separated (a usb-net, a hostfwd)
                     + os.environ.get("QTEST_QEMU_ARGS", "").split(),
                     # QTEST_QEMU_LOG: QEMU's own stderr (an emulator abort says why there)
                     stdout=subprocess.DEVNULL,
                     stderr=open(os.environ["QTEST_QEMU_LOG"], "w") if os.environ.get("QTEST_QEMU_LOG") else subprocess.DEVNULL)
import atexit; atexit.register(q.kill)      # a harness that dies must not leave QEMU holding ports
# QEMU makes the serial socket when it is up -- seconds on a loaded box, not a fixed 2
s = socket.socket(socket.AF_UNIX)
for _ in range(120):
    try: s.connect(sock); break
    except (FileNotFoundError, ConnectionRefusedError): time.sleep(0.5)
else: s.connect(sock)
s.settimeout(1)
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
        # a prompt may be followed by the echo of the line end sent after a form: still a prompt
        if re.search(pat, buf) or re.search(pat, buf.rstrip(b"\r\n")): return True
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
        # QTEST_FORM_TIMEOUT: seconds one form may take (a JIT-EAGER over a few thousand
        # functions under TCG runs past the default 900)
        ft = float(os.environ.get("QTEST_FORM_TIMEOUT", "900"))
        s.settimeout(ft); s.sendall((f + "\r\n").encode()); s.settimeout(1)
        r = until(rb"\n(DESK)?> ?$|!!FAULT", ft)        # DESK>: kiln's desk REPL
        out = buf.decode("latin1").replace("\r", "")
        lines = [l for l in out.split("\n") if l.strip() and not l.startswith(f[:20]) and "WARN" not in l]
        print("FORM", f[:60], "=>", " | ".join(lines[-3:]) if r else "TIMEOUT", flush=True)
        # QTEST_PACE: seconds between forms, waited HERE -- a wait evaluated on the board
        # would hold whatever owns its main thread (the desk) still for that long
        time.sleep(float(os.environ.get("QTEST_PACE", "0")))
q.kill()
