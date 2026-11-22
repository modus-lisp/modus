#!/usr/bin/env python3
"""test/w32-x25519-bare.py -- time X25519 in a bare-metal CL image (W32 M0/M4).

    test/w32-x25519-bare.py IMAGE [N] [check]

Boots IMAGE -- a bare CL image built with MODUS_NET_BUILD=1 MODUS_SSH_BUILD=1
MODUS_NET_NOAUTO=1 (crypto in, the NIC left alone) -- under qemu-system-i386,
or qemu-system-arm (QEMU virt) with W32_X25519_ARCH=arm32; waits for the REPL
prompt, and evaluates one form that runs RFC 7748 section 5.2's first vector
once (warm-up, and the correctness check), then N more times between two
markers.  Two clocks: the image's own GET-INTERNAL-REAL-TIME and the HOST's
monotonic clock at the moment each marker line arrives on the serial port.
Exit 0 iff every result equals the RFC's output.  W32_X25519_DEADLINE (s,
default 1800) bounds the whole run.

With "check", EVERY one of the N results is compared with the RFC's (not just
the last), and %GC-EPOCH is printed before and after: the collector test for
the field layer (net/crypto-fast.lisp).  Run N large enough that the epoch
moves, or the test proves nothing; MODUS_I386_MAX_HEAP_MB=32 at build time
makes a bare i386 image collect several times per X25519.
"""
import os, select, subprocess, sys, time

IMAGE = sys.argv[1]
N = int(sys.argv[2]) if len(sys.argv) > 2 else 5
CHECK = len(sys.argv) > 3 and sys.argv[3] == "check"
K = "a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4"
U = "e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c"
WANT = "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552"

FORM = (
    "(let* ((h (lambda (s) (let ((v (make-array 32))) (dotimes (i 32 v) "
    "(setf (aref v i) (parse-integer s :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))) "
    "(hx (lambda (v) (let ((o (make-string 64)) (d \"0123456789abcdef\")) (dotimes (i 32 o) "
    "(setf (char o (* 2 i)) (char d (ash (aref v i) -4)) "
    "(char o (+ 1 (* 2 i))) (char d (logand (aref v i) 15))))))) "
    f"(k (funcall h \"{K}\")) (u (funcall h \"{U}\")) "
    "(r0 (modus.crypto::x25519 k u)) (r r0)) "
    "(format t \"~%XWARM ~A~%\" (funcall hx r0)) "
    "(format t \"~%XBEGIN~%\") "
    "(let ((t0 (get-internal-real-time))) "
    + (f"(let ((bad 0) (e0 (modus-int::%gc-epoch))) "
       f"(dotimes (i {N}) (setq r (modus.crypto::x25519 k u)) "
       f"(unless (string= (funcall hx r) \"{WANT}\") (setq bad (+ bad 1)))) "
       "(format t \"~%XCHECK bad=~D epoch=~D->~D~%\" bad e0 (modus-int::%gc-epoch))) "
       if CHECK else f"(dotimes (i {N}) (setq r (modus.crypto::x25519 k u))) ")
    + "(format t \"~%XEND us=~D per=~D ~A~%\" (- (get-internal-real-time) t0) "
    "internal-time-units-per-second (funcall hx r))))"
)

# W32_X25519_ARCH=arm32 boots an arm32 CL image (QEMU virt, run.sh's machine).
QEMU = {"i386": ["qemu-system-i386"],
        "arm32": ["qemu-system-arm", "-M", "virt,highmem=off", "-cpu", "cortex-a15"]}
q = subprocess.Popen(
    QEMU[os.environ.get("W32_X25519_ARCH", "i386")] +
    ["-kernel", IMAGE, "-m", "1024", "-display", "none",
     "-serial", "stdio", "-no-reboot"],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
buf = b""
stamps = {}
sent = False
t_boot = time.monotonic()
deadline = t_boot + int(os.environ.get("W32_X25519_DEADLINE", "1800"))
try:
    while time.monotonic() < deadline:
        r, _, _ = select.select([q.stdout], [], [], 1.0)
        if not r:
            if q.poll() is not None:
                break
            continue
        chunk = os.read(q.stdout.fileno(), 65536)
        if not chunk:
            break
        buf += chunk
        if not sent and buf.replace(b"\r", b"").endswith(b"\n> "):
            print(f"prompt after {time.monotonic() - t_boot:.1f} s", flush=True)
            q.stdin.write(FORM.encode() + b"\r")
            q.stdin.flush()
            sent = True
        for tag in (b"XWARM", b"XBEGIN", b"XEND"):
            if tag not in stamps and b"\n" + tag in buf:
                stamps[tag] = time.monotonic()
        if b"XEND" in stamps and buf.rstrip().endswith(b">"):
            break
finally:
    q.kill()
    q.wait()

text = buf.decode("latin-1").replace("\r", "")
lines = [l for l in text.split("\n") if l.startswith(("XWARM", "XEND"))]
for l in text.split("\n"):
    if l.startswith("XCHECK"):
        print(l)
        if "bad=0 " not in l:
            lines = []          # any mismatch fails the run
for l in lines:
    print(l)
ok = len(lines) == 2 and all(l.endswith(WANT) for l in lines)
if b"XEND" in stamps and lines:
    host = stamps[b"XEND"] - stamps[b"XBEGIN"]
    end = lines[-1].split()
    us = int(end[1][3:]); per = int(end[2][4:])
    print(f"x25519 x{N}: image clock {us / per:.3f} s ({us / per / N * 1000:.1f} ms each); "
          f"host clock {host:.3f} s ({host / N * 1000:.1f} ms each)")
else:
    print("no XEND marker; tail of the console:")
    print(text[-2000:])
print("RFC 7748 5.2 vector 1:", "PASS" if ok else "FAIL")
sys.exit(0 if ok else 1)
