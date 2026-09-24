#!/usr/bin/env python3
"""hosted-ladder.py — run the arch-ladder rungs on the HOSTED Linux ports.

The bare-metal gate (scripts/arch-ladder-gate.sh) boots a full system per cell
and reads the answer out of guest memory over QMP.  That is the right oracle for
an image with no console, and it costs about ninety minutes for the full matrix.
A HOSTED image has a console — write(2) — so the same rungs can be graded in
seconds by reading four bytes off stdout.

THE ORACLE IS THE SAME SHAPE AND FOR THE SAME REASON.  The rung's `probe' is
called and its answer is emitted as four bytes, low byte first, assembled here
and compared against an expected value.  "The image printed something" is not a
pass; --expect is mandatory, exactly as in the full-system harness.

Byte 0 is written WITHOUT a shift because `(ash x 0)' returns 0 on ARM32, and a
harness must not depend on the thing it is measuring.

A CELL IS INDEPENDENT BY CONSTRUCTION — its own temp directory, its own output
path, its own emulator process — so cells run in PARALLEL.  HOSTED_LADDER_JOBS
sets the width (default: half the cores, since each cell is one sbcl build).
The work is CPU-bound in a subprocess, so a thread pool is the right shape here:
the threads only wait on wait().

Usage:
    scripts/hosted-ladder.py <arch> [rung.lisp ...] [--keep]
    scripts/hosted-ladder.py --all

Exit code is 0 only when every cell passed AND the positive control failed.
"""
import os, re, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RUNGS = os.path.join(REPO, "test", "arch-rungs")

# arch -> (build script, output env var, qemu user-mode binary)
#
# Only the ports with a build script that takes a SOURCE FILE are here.  The
# hosted x64/aarch64/i386 images are built by the CLI lineage
# (build-cli-common.lisp) and bake a whole runtime rather than one file, so
# grading them this way would measure a different thing; they have ./modus and
# the ANSI gate instead.
ARCHES = {
  "riscv64": ("mvm/build-riscv-linux.lisp",   "MODUS_RISCV_LINUX_OUT",   "qemu-riscv64-static"),
  "riscv32": ("mvm/build-riscv32-linux.lisp", "MODUS_RISCV32_LINUX_OUT", "qemu-riscv32-static"),
  "arm32":   ("mvm/build-arm32-linux.lisp",   "MODUS_ARM32_LINUX_OUT",   "qemu-arm-static"),
  # BIG-ENDIAN.  The oracle is endian-NEUTRAL by construction: the payload emits
  # its four bytes one at a time, low byte first, through write-char-serial, so
  # nothing here depends on the target's byte order — which is the whole reason
  # these three could join without touching the harness.
  "ppc64":   ("mvm/build-ppc64-linux.lisp",   "MODUS_PPC64_LINUX_OUT",   "qemu-ppc64-static"),
  "ppc32":   ("mvm/build-ppc32-linux.lisp",   "MODUS_PPC32_LINUX_OUT",   "qemu-ppc-static"),
  "68k":     ("mvm/build-68k-linux.lisp",     "MODUS_68K_LINUX_OUT",     "qemu-m68k-static"),
}


def payload(rung_source):
    """Wrap a rung: call `probe', emit the answer as 4 bytes, exit(0).

    SYS-EXIT is not optional.  Falling off the end of kernel-main in a hosted
    image runs into whatever the linker put next, which is a SIGSEGV that looks
    like a failing rung."""
    return (rung_source + "\n(defun kernel-main ()\n"
            "  (let ((v (probe)))\n"
            "    (write-char-serial (logand v 255))\n"
            "    (write-char-serial (logand (ash v -8) 255))\n"
            "    (write-char-serial (logand (ash v -16) 255))\n"
            "    (write-char-serial (logand (ash v -24) 255))\n"
            "    (sys-exit 0)))\n")


def expected_of(path):
    """Each rung's header names its answer: `-- expect 3628800'."""
    with open(path) as f:
        m = re.search(r"expect\s+(-?\d+)", f.read())
    return int(m.group(1)) if m else None


def run_cell(arch, rung_path, expect, keep=False):
    script, out_env, qemu = ARCHES[arch]
    with open(rung_path) as f:
        src = f.read()
    tmpdir = tempfile.mkdtemp(prefix="hosted-ladder-")
    srcfile = os.path.join(tmpdir, "payload.lisp")
    binfile = os.path.join(tmpdir, "image")
    with open(srcfile, "w") as f:
        f.write(payload(src))
    env = dict(os.environ, **{out_env: binfile})
    b = subprocess.run(["sbcl", "--dynamic-space-size", "4096", "--script",
                        os.path.join(REPO, script), srcfile],
                       cwd=REPO, env=env, capture_output=True, text=True,
                       timeout=1800)
    if not os.path.exists(binfile):
        return "BUILD", "BUILD FAILED: " + (b.stderr or b.stdout)[-300:]
    # The image is WRITTEN, not created executable, and qemu-user resolves its
    # argument as a program: a non-executable file is rejected SILENTLY with
    # exit 1 and not one word on stderr.  That cost a debugging cycle; chmod
    # here so no caller can hit it.
    os.chmod(binfile, 0o755)
    try:
        r = subprocess.run([qemu, binfile], capture_output=True, timeout=120)
    except subprocess.TimeoutExpired:
        return "RUN", "TIMEOUT"
    if len(r.stdout) < 4:
        return "RUN", f"SHORT OUTPUT ({len(r.stdout)} bytes, rc={r.returncode}) {r.stderr[-120:]!r}"
    got = int.from_bytes(r.stdout[:4], "little")
    if not keep:
        subprocess.run(["rm", "-rf", tmpdir])
    # THREE OUTCOMES, NOT TWO.  "PASS", "WRONG" (it ran and answered something
    # else) and the failure kinds above are different facts, and the control
    # below is only meaningful if it can tell them apart.
    return ("PASS" if got == expect else "WRONG"), \
           f"{'=' if got == expect else '!='} {got} (want {expect})"


def jobs():
    """How many cells to run at once.  Half the cores by default: each cell is an
    sbcl build, and leaving headroom keeps this from starving whatever else is on
    the box — which on a shared machine is the difference between fast and rude."""
    env = os.environ.get("HOSTED_LADDER_JOBS")
    if env:
        return max(1, int(env))
    return max(1, (os.cpu_count() or 4) // 2)


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    flags = [a for a in sys.argv[1:] if a.startswith("--")]
    keep = "--keep" in flags
    arches = list(ARCHES) if ("--all" in flags or not args) else [args[0]]
    rungs = sorted(os.path.join(RUNGS, f) for f in os.listdir(RUNGS)
                   if f.endswith(".lisp"))
    if len(args) > 1:
        rungs = [a if os.path.exists(a) else os.path.join(RUNGS, a) for a in args[1:]]

    failures = 0
    for arch in arches:
        # THE GATE MUST PROVE IT CAN FAIL, AND FAIL FOR THE RIGHT REASON.  One
        # rung runs FIRST with a deliberately wrong expected value, and the
        # required outcome is "WRONG" -- it BUILT, it RAN, and the comparison
        # rejected the answer.
        #
        # Accepting any failure here is not a control.  Measured: a broken
        # build makes every cell BUILD-FAIL, and a control that counts that as
        # "failed as required" then certifies a harness in which nothing works
        # at all.  scripts/arch-ladder-gate.sh printed exactly that -- "the gate
        # can fail" above 14 BUILD-FAILs -- which is how this was found.
        outcome, why = run_cell(arch, rungs[0], expected_of(rungs[0]) + 1)
        if outcome == "PASS":
            print(f"{arch}: POSITIVE CONTROL PASSED — the ladder cannot fail; refusing to run")
            return 2
        if outcome != "WRONG":
            print(f"{arch}: POSITIVE CONTROL DID NOT RUN ({outcome}: {why}) — "
                  f"a control that cannot answer proves nothing; refusing to run")
            return 2
        print(f"{arch}: positive control answered WRONG as required ({why})")
        n = min(jobs(), len(rungs))
        with ThreadPoolExecutor(max_workers=n) as pool:
            results = list(pool.map(
                lambda r: (r,) + run_cell(arch, r, expected_of(r), keep), rungs))
        for r, outcome, why in results:
            if outcome != "PASS":
                failures += 1
            print(f"  {os.path.basename(r):<24} {arch:<8} "
                  f"{outcome if outcome != 'WRONG' else 'FAIL':<5} {why}")
    print(f"\nhosted ladder: {failures} failed")
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
