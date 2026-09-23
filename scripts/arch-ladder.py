#!/usr/bin/env python3
"""arch-ladder.py — run one rung on one architecture and CHECK THE ANSWER.

The oracle is MEMORY, not serial.  The rung's `probe' is called, its answer is
stored at a fixed physical address, and the image spins; this reads that
address back over QMP `pmemsave' and compares it to an expected value.  That
needs no UART driver, no interrupt controller and no boot work beyond the stub
each architecture already has — just what QEMU needs to load the image and
start it.  It is also the repo's own idiom: run-fixpoint-i386.sh extracts each
generation the same way.

Two rules this file exists to enforce, both learned the hard way:

  * "The image wrote something" is NOT a pass.  An earlier version of this
    harness reported OK whenever the magic word landed and printed the answer
    without comparing it.  It called `(dbl 21)' returning 21 a pass for an
    entire ladder run.  The expected value is mandatory.

  * The probe address MUST be clear of the heap.  i386's was 0x00800000 —
    which is +i386-cons-base+ exactly — so any rung that allocated wrote an
    object header over the answer and the harness read that back.  Each address
    below is above the image and below that target's cons space.

Usage:
    scripts/arch-ladder.py <arch> <rung.lisp> --expect=<n> [--keep]
Exit code is 0 on PASS, 1 on any failure.
"""
import json, os, socket, subprocess, sys, tempfile, time

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MAGIC = 195948557          # 0x0BADF00D — fits a 30-bit fixnum

# arch -> (build target, qemu binary, machine args, result addr, magic addr)
#
# Machine choices that are NOT the obvious one, and why:
#   ppc64  powernv, not pseries — boot-ppc64.lisp targets skiboot/OPAL and its
#          entry is an ELFv1 function descriptor that SLOF rejects outright.
#   68k    virt, not an5206 — boot-68k.lisp drives a Goldfish TTY at
#          0xFF008000, which the an5206 board does not have.
#   arm32  raspi2b, whose RAM starts at 0; `virt' puts RAM at 0x40000000,
#          out of reach of a 30-bit-fixnum tagged address.
ARCHES = {
  "i386":    ("i386",      "qemu-system-i386",    ["-m","256","-no-reboot"],                      0x00700000, 0x00700010),
  "x64":     ("x86-64",    "qemu-system-x86_64",  ["-m","512","-no-reboot"],                      0x00700000, 0x00700010),
  "aarch64": ("aarch64",   "qemu-system-aarch64", ["-machine","virt","-cpu","cortex-a57","-m","512","-semihosting"], 0x41000000, 0x41000010),
  "arm32":   ("armv7-rpi", "qemu-system-arm",     ["-M","raspi2b","-m","1G"],                     0x00800000, 0x00800010),
  "riscv64": ("riscv64",   "qemu-system-riscv64", ["-machine","virt","-m","512"],                 0x80300000, 0x80300010),
  "ppc64":   ("ppc64",     "qemu-system-ppc64",   ["-M","powernv","-m","2G"],                     0x20800000, 0x20800010),
  "ppc32":   ("ppc32",     "qemu-system-ppc",     ["-M","ppce500","-m","512"],                    0x00800000, 0x00800010),
  "68k":     ("68k",       "qemu-system-m68k",    ["-M","virt","-m","512"],                       0x00700000, 0x00700010),
}

# skiboot takes ~15s before it hands off; everything else is up in a second.
SETTLE = {"ppc64": 25.0}


def store32(addr, expr):
    """Four :u8 stores, little-endian by construction.

    NOT one :u32 store: on a 30-bit-fixnum target compile-mem-ref splits a
    promoting width into two :u16 halves and that path writes the LOW half into
    both (measured on i386 — the probe read back 0x5f005f00 for 0x00375f00).
    Byte stores never promote and never depend on target endianness.

    Byte 0 is written WITHOUT a shift because `(ash x 0)' returns 0 on ARM32,
    and a harness must not depend on the thing it is measuring."""
    lines = [f"  (setf (mem-ref {addr} :u8) (logand {expr} 255))"]
    lines += [f"  (setf (mem-ref {addr+i} :u8) (logand (ash {expr} {-8*i}) 255))"
              for i in range(1, 4)]
    return "\n".join(lines)


def payload(rung_source, res_addr, mag_addr):
    """Wrap a rung: call `probe', store the answer, store the magic, spin.

    The magic is written AFTER the answer, so "magic present" implies "answer
    valid" and a legitimately-zero answer cannot be confused with never having
    arrived."""
    return (rung_source + "\n(defun kernel-main ()\n"
            "  (let ((v (probe)))\n"
            + store32(res_addr, "v") + "\n"
            + store32(mag_addr, str(MAGIC)) + "\n"
            "    (loop)))\n")


def qmp(sock_path, cmd, args):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect(sock_path)
    def recv():
        d = b""
        while True:
            chunk = s.recv(65536)
            if not chunk:
                raise IOError("qmp closed")
            d += chunk
            try:
                return json.loads(d.decode().split("\n")[0])
            except Exception:
                continue
    recv()
    s.sendall(json.dumps({"execute": "qmp_capabilities"}).encode() + b"\n"); recv()
    s.sendall(json.dumps({"execute": cmd, "arguments": args}).encode() + b"\n")
    out = recv()
    s.sendall(json.dumps({"execute": "quit"}).encode() + b"\n")
    s.close()
    return out


def run(arch, image):
    _, qemu, machine, res_addr, mag_addr = ARCHES[arch]
    settle = SETTLE.get(arch, 8.0)
    tmp = tempfile.mkdtemp()
    sock, dump, log = (os.path.join(tmp, n) for n in ("qmp.sock", "mem.bin", "serial.log"))
    cmd = [qemu, *machine, "-nographic", "-kernel", image,
           "-qmp", f"unix:{sock},server=on,wait=off"]
    with open(log, "wb") as lf:
        p = subprocess.Popen(cmd, stdout=lf, stderr=subprocess.STDOUT,
                             stdin=subprocess.DEVNULL)
    try:
        for _ in range(100):
            if os.path.exists(sock) or p.poll() is not None:
                break
            time.sleep(0.1)
        time.sleep(settle)
        if p.poll() is not None:
            return None, "QEMU EXITED: " + open(log, errors="replace").read()[-400:]
        base = min(res_addr, mag_addr)
        span = max(res_addr, mag_addr) + 8 - base
        qmp(sock, "pmemsave", {"val": base, "size": span, "filename": dump})
        time.sleep(0.5)
        raw = open(dump, "rb").read() if os.path.exists(dump) else b""
    finally:
        p.kill(); p.wait()
    if not raw:
        return None, "NO DUMP"
    word = lambda off: int.from_bytes(raw[off:off + 4], "little")
    if word(mag_addr - base) != MAGIC:
        return None, f"NO MAGIC (probe region: {raw[:24].hex()})"
    return word(res_addr - base), None


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    expect = next((int(a[9:]) for a in sys.argv if a.startswith("--expect=")), None)
    keep = "--keep" in sys.argv
    if len(args) != 2 or expect is None:
        print(__doc__.strip().splitlines()[-3], file=sys.stderr)
        return 2
    arch, rung = args
    if arch not in ARCHES:
        print(f"unknown arch {arch}; known: {' '.join(ARCHES)}", file=sys.stderr)
        return 2

    _, qemu = ARCHES[arch][0], ARCHES[arch][1]
    if not any(os.access(os.path.join(d, qemu), os.X_OK)
               for d in os.environ.get("PATH", "").split(os.pathsep)):
        print(f"{arch:8s} SKIP      {qemu} not on PATH")
        return 0

    work = tempfile.mkdtemp(prefix="arch-ladder-")
    _, _, _, res, mag = ARCHES[arch]
    src = os.path.join(work, "payload.lisp")
    img = os.path.join(work, "payload.bin")
    open(src, "w").write(payload(open(rung).read(), res, mag))

    r = subprocess.run(["sbcl", "--script", "test/arch-ladder-build.lisp",
                        ARCHES[arch][0], img, src],
                       cwd=REPO, capture_output=True, text=True, timeout=3600)
    if not os.path.exists(img):
        tail = [l for l in (r.stdout + r.stderr).splitlines() if l.strip()][-3:]
        print(f"{arch:8s} BUILD-FAIL  {' | '.join(tail)[:150]}")
        return 1

    got, err = run(arch, img)
    if keep:
        print(f"{arch:8s} (kept {work})")
    if err:
        print(f"{arch:8s} FAIL      {err[:150]}")
        return 1
    if got != expect:
        print(f"{arch:8s} WRONG     got {got} want {expect}")
        return 1
    print(f"{arch:8s} PASS      {os.path.basename(rung)} = {got}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
