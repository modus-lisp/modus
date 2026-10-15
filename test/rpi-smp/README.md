# SMP phase 2 on the bare Pi image: secondary cores running Lisp

Build the board image with the per-thread window (opt-in):

    MODUS_RPI_THREADS=1 MODUS_CL_REPL_OUT=thr.img MODUS_NET_BUILD=1 MODUS_SSH_BUILD=1 \
      MODUS_NET_NOAUTO=1 MODUS_RPI_CHAINLOAD=1 sbcl --dynamic-space-size 8192 --script mvm/build-rpi-cl-repl.lisp

Run the driver under QEMU raspi3b (4 cores; the same spin-table release as the firmware):

    N=$(stat -c %s test/rpi-smp/smp-lisp.lisp)
    python3 test/rpi-smp/qtest.py thr.img --blob=$PWD/test/rpi-smp/smp-lisp.lisp \
      "(defun ramv (a n) (let ((v (make-array n :element-type (quote (unsigned-byte 8))))) (dotimes (i n v) (setf (aref v i) (mem-ref (+ a i) :u8)))))" \
      "(progn (load (make-string-input-stream (map (quote string) (function code-char) (ramv #x1A000000 $N)))) :loaded)" \
      "(progn (jit-eager) :jitted)" "(smp-snapshot)" \
      "(smp-start 2 (quote core-spin))" "(smp-start 3 (quote core-spin))" "(smp-start 1 (quote core-spin))" \
      "(smp-counters)" "(smp-counters)"

Measured 2026-10-15: every core reaches STATUS 1, and the counters climb on cores 1-3 while core
0's REPL keeps answering, e.g. (0 319675 5167854 1006349).  Each core finds its own number through
ITS window and per-CPU block (PERCPU-REF 16), so a counter in the right slot is the check.

What a core gets (smp-lisp.lisp SMP-START): core 0's MAIR/TCR/TTBR0/SCTLR/VBAR and x28 (snapshot),
a stack (0x05400000 + 2 MB per core), a per-thread window block at 0x10400000 + n*0x20000 (delta in
TPIDR_EL0), a per-CPU block at 0x10080000 + n*0x4000 (CPU id at +16, pointer in window slot
0x1000FFE8), SMPEN, FP, x18/x26.  The stubs are stubs.s (assembled with GNU as on the Pi 5).

NOT YET: allocation on a secondary core (it needs a GC region of its own and the per-CPU
active-region cell), stop-the-world for region 0, and a thread API.  core-spin allocates nothing.
