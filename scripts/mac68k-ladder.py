#!/usr/bin/env python3
"""mac68k-ladder.py [RUNG...] -- the architecture ladder on a bare 68k Mac.

Each rung (test/arch-rungs/) is built for the :mac68k target
(`mvm/build-hosted.lisp mac68k`, boot/boot-mac68k.lisp), written to a boot
disk (scripts/mac68k-disk.py) and booted under Basilisk II: the Mac ROM runs
our boot blocks, which load the image, which answers over serial port A --
four bytes, as on the hosted ladder.  Basilisk's serial port A is a
pseudo-terminal this script reads.

Environment:
  MAC68K_ROM        the Mac ROM file (required; Apple's, so not in the tree).
                    Measured with the LC 475 / Quadra 605 ROM (FF7439EE).
  MAC68K_BASILISK   the command that runs Basilisk II (default BasiliskII); it
                    gets Basilisk's own arguments appended.  It must have a
                    display (Basilisk's GTK insists) -- a wrapper that starts an
                    Xvfb is the usual answer on a headless box.
  MAC68K_FPU        "false" (default: a 68LC040) or "true" (a 68040).
  MAC68K_EMULATOR   "mame": a Mac Classic (68000, 4 MB) in MAME instead --
                    MAC68K_MAME (the command), MAC68K_ROMPATH (its ROM folder:
                    macclasc's ROM and the ADB modem's 342s0440-b.bin); build
                    with MODUS_MAC68K_MACHINE=classic MODUS_68K_CPU=68000.
  MAC68K_JIT        "true": Basilisk's JIT, ~8x faster.  Debian's
                    BasiliskII-jit is a PIE and its JIT truncates addresses;
                    relink it low first (scripts/basilisk-unpie.py) and run it
                    with MALLOC_MMAP_MAX_=0.
  MAC68K_LADDER_JOBS  cells at once (default 4).

The rules are hosted-ladder.py's: a deliberately wrong expectation runs first
and must come back WRONG (built, ran, rejected), or nothing is run.
"""
import os, pty, re, select, shlex, signal, subprocess, sys, tempfile, time, tty
from concurrent.futures import ThreadPoolExecutor

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RUNGS = os.path.join(REPO, "test", "arch-rungs")


def payload(rung_source):
    """The rung, then KERNEL-MAIN: the answer as 4 bytes, then halt."""
    return (rung_source + "\n(defun kernel-main ()\n"
            "  (let ((v (probe)))\n"
            "    (write-char-serial (logand v 255))\n"
            "    (write-char-serial (logand (ash v -8) 255))\n"
            "    (write-char-serial (logand (ash v -16) 255))\n"
            "    (write-char-serial (logand (ash v -24) 255))\n"
            "    (sys-exit 0)))\n")


def expected_of(path):
    with open(path) as f:
        m = re.search(r"expect\s+(-?\d+)", f.read())
    return int(m.group(1)) if m else None


def boot_mame(disk, nbytes, timeout=600):
    """Boot DISK on MAME's Mac Classic (macclasc, a 68000, 4 MB); return the
    first NBYTES the Mac sends on serial port A (scripts/mac68k-mame-serial.lua
    taps the SCC).  MAC68K_MAME is the command, MAC68K_ROMPATH the ROM path."""
    out = disk + ".serial"
    cmd = shlex.split(os.environ.get("MAC68K_MAME", "mame")) + [
        "macclasc", "-rompath", os.environ["MAC68K_ROMPATH"], "-ramsize", "4M",
        "-flop1", disk, "-video", "none", "-sound", "none", "-nothrottle",
        "-skip_gameinfo", "-autoboot_script",
        os.path.join(REPO, "scripts", "mac68k-mame-serial.lua")]
    env = dict(os.environ, MAC68K_SERIAL_OUT=out, MAC68K_SERIAL_N=str(nbytes),
               MAC68K_SECONDS=os.environ.get("MAC68K_MAME_SECONDS", "150"))
    try:
        subprocess.run(cmd, env=env, stdout=subprocess.DEVNULL,   # MAME writes cfg/ and
                       stderr=subprocess.DEVNULL, timeout=timeout,  # nvram/ where it runs
                       cwd=os.path.dirname(disk))
    except subprocess.TimeoutExpired:
        pass
    try:
        with open(out, "rb") as f:
            return f.read()
    except FileNotFoundError:
        return b""


def boot(disk, nbytes, timeout=90):
    """Boot DISK under Basilisk II; return the first NBYTES from serial port A."""
    if os.environ.get("MAC68K_EMULATOR") == "mame":
        return boot_mame(disk, nbytes)
    master, slave = pty.openpty()
    tty.setraw(slave)
    cmd = shlex.split(os.environ.get("MAC68K_BASILISK", "BasiliskII")) + [
        "--nogui", "true", "--rom", os.environ["MAC68K_ROM"],
        "--cpu", "4", "--fpu", os.environ.get("MAC68K_FPU", "false"),
        "--ramsize", str(32 << 20),
        "--jit", os.environ.get("MAC68K_JIT", "false"), "--jitcachesize", "16384",
        "--disk", disk, "--seriala", os.ttyname(slave)]
    p = subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                         start_new_session=True)
    got, end = b"", time.time() + timeout
    try:
        while len(got) < nbytes and time.time() < end and p.poll() is None:
            r, _, _ = select.select([master], [], [], 0.5)
            if r:
                got += os.read(master, 64)
    finally:
        # Basilisk II ignores SIGTERM: TERM the group, then KILL what is left
        for sig in (signal.SIGTERM, signal.SIGKILL):
            try:
                os.killpg(p.pid, sig)
            except ProcessLookupError:
                break
            time.sleep(1)
        p.wait(timeout=30)
        os.close(master)
        os.close(slave)
    return got


def run_cell(rung_path, expect, keep=False):
    with open(rung_path) as f:
        src = f.read()
    tmpdir = tempfile.mkdtemp(prefix="mac68k-ladder-")
    srcfile = os.path.join(tmpdir, "payload.lisp")
    binfile = os.path.join(tmpdir, "image.bin")
    disk = os.path.join(tmpdir, "boot.dsk")
    with open(srcfile, "w") as f:
        f.write(payload(src))
    b = subprocess.run(["sbcl", "--dynamic-space-size", "4096", "--script",
                        os.path.join(REPO, "mvm/build-hosted.lisp"), "mac68k", srcfile],
                       cwd=REPO, env=dict(os.environ, MODUS_MAC68K_OUT=binfile),
                       capture_output=True, text=True, timeout=1800)
    if not os.path.exists(binfile):
        return "BUILD", "BUILD FAILED: " + (b.stderr or b.stdout)[-300:]
    subprocess.run([sys.executable, os.path.join(REPO, "scripts/mac68k-disk.py"),
                    binfile, disk], check=True, capture_output=True)
    out = boot(disk, 4)
    if not keep:
        subprocess.run(["rm", "-rf", tmpdir])
    if len(out) < 4:
        return "RUN", f"SHORT OUTPUT ({len(out)} bytes) {out!r}"
    got = int.from_bytes(out[:4], "little")
    return ("PASS" if got == expect else "WRONG"), \
           f"{'=' if got == expect else '!='} {got} (want {expect})"


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    keep = "--keep" in sys.argv
    if "MAC68K_ROM" not in os.environ and os.environ.get("MAC68K_EMULATOR") != "mame":
        print("mac68k-ladder: set MAC68K_ROM to a Mac ROM file")
        return 2
    rungs = sorted(os.path.join(RUNGS, f) for f in os.listdir(RUNGS) if f.endswith(".lisp"))
    if args:
        rungs = [a if os.path.exists(a) else os.path.join(RUNGS, a) for a in args]
    outcome, why = run_cell(rungs[0], expected_of(rungs[0]) + 1)
    if outcome != "WRONG":
        print(f"mac68k: POSITIVE CONTROL {outcome} ({why}) -- refusing to run")
        return 2
    print(f"mac68k: positive control answered WRONG as required ({why})")
    n = max(1, min(int(os.environ.get("MAC68K_LADDER_JOBS", "4")), len(rungs)))
    with ThreadPoolExecutor(max_workers=n) as pool:
        results = list(pool.map(lambda r: (r,) + run_cell(r, expected_of(r), keep), rungs))
    failures = 0
    for r, outcome, why in results:
        if outcome != "PASS":
            failures += 1
        print(f"  {os.path.basename(r):<26} mac68k   "
              f"{outcome if outcome != 'WRONG' else 'FAIL':<5} {why}")
    print(f"\nmac68k ladder: {failures} failed")
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
