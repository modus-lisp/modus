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

ALLOCATION (2b): SMP-CARVE shrinks region 0 (after a collection, refusing if live data reaches the
carve), gives core N a region at the top of both semispaces with its own stack as the root window,
publishes it in the per-CPU active-region cell (0x10000F08 + 8N), stamps core 0's CPU id (nothing
initialised 0x10080010 before), and turns per-CPU mode on (0x10000FF8).  Measured under QEMU, a
16 MB region on core 2:

    (let ((r (smp-carve 2 #x1000000))) (smp-start 2 (quote core-alloc) (getf r :x24) (getf r :x25)))
    (list (smp-counters) (smp-region-gcs 2))   =>  ((0 0 115847 0) 221)

i.e. 115,847 iterations of "cons a 1000-element list, check its sum is 499500" and 221 collections
of core 2's OWN region, while core 0's REPL conses 200,000 cells correctly beside it.  Regions must
be well over 8 MB: every limit sits *AARCH64-GC-LIMIT-GUARD* (8 MB) below the region's end.

THREADS (phase 3, net/bare-threads.lisp, linked when MODUS_RPI_THREADS=1): BT-SPAWN runs a
no-argument function on a free core and BT-JOIN returns its value (conses, strings and vectors in
the core's region are copied out, since the region is reused).  A core is released once into
%BT-CORE-MAIN and then takes jobs from its thread record, waiting SAFE between them.  The record
layout is hosted aarch64's, so translate-aarch64's stop-the-world collector runs unchanged: core 0
collecting region 0 parks every running worker at its next allocation and scans its stack, window
and region.  On bare metal the park/wait spin is the YIELD hint, not sched_yield, and the
safe-region traps (#x0541..#x0544) are emitted whenever the window is on.  test/rpi-smp/threads.lisp:

    run as above with --blob=test/rpi-smp/threads.lisp, then
    (setq *big* (loop for i below 100000 collect i))
    (progn (setq w1 (bt-spawn #'t-walk2)) (setq w2 (bt-spawn #'t-walk2)) (setq w3 (bt-spawn #'t-cons))
           (t-main-churn 40))
    (list (bt-join w1) (bt-join w2) (bt-join w3))
      => ((OK 3000 BAD 0 R0-GCS-DURING 41 CHANGES-SEEN 41) (same) (3000 5))

i.e. two cores walking a 100,000-element list IN REGION 0 3000 times each while core 0 collected
region 0 41 times under them -- every sum right -- and each worker collected its own region 1144
times meanwhile.  NEGATIVE CONTROL, same image: clear the table's armed word (STW off) and the
same run takes a data abort on a worker stack (SP 0x053FFBC0, core 1) during the first churn.

NOT YET: a worker storing its objects into region-0 structures (fresh INTERN, SETQ of a global,
special bindings, SETQ of a closed-over variable) -- needs the runtime lock + lock arena; a
non-allocating worker loop holds a region-0 collection up; the sb-thread shim mapping; heap size
(each worker region must be well over the 8 MB limit guard, carved from region 0's 56 MB); and the
board run (core 1 did not start on one board boot; it does under QEMU).
