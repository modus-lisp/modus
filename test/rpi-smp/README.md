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

COROUTINE SLOTS (phase 4, net/bare-threads.lisp): cores 1..3 each run a scheduler; a thread is
a SLOT (records 4..15: stack, window, per-CPU block) pinned to a core and run there as a
coroutine, sharing the core's heap.  A slot runs until it blocks -- %MUTEX-LOCK, %COND-WAIT-MS,
%SLEEP-MS, BT-YIELD -- and the core runs another; the switch (test/rpi-smp/switch.s) pushes the
registers and publishes the slot SAFE first, so stop-the-world scans it, and a collection of the
core's own heap scans it too (translate-aarch64 SIBLING mode).  Results come back by copy
(modus-channel's encoder).  test/rpi-smp/slots.lisp, QEMU raspi3b:

    (join-all (spawn-n 8 #'t-sum))                    8 threads on 3 cores, all 4499998500000
    (join-all (spawn-n 3 #'t-list))                   a list/string result, copied out intact
    (join-all (spawn-n 6 #'t-chain))                  2 slots per core, each yielding with a
                                                      2000-cons chain live while its sibling
                                                      collects the shared heap: 28 GCs per core,
                                                      1200/1200 sums right
      ... with *HIDE* (a slot hides from the sibling scan while parked): 538 bad + 2 errors
    6 x 2000 increments under %MUTEX-LOCK, yielding inside        => 12000
      ... without the mutex                                         => 8567 (lost updates)
    (join-all (spawn-n 3 #'t-sleep))                  (300 300 300) ms
    6 slots walk a region-0 list while core 0 collects it 40x     all 3000/3000

Also found: the in-image JIT never got the per-thread window (its gate read a variable defined
after the text was built), so runtime-JIT'd code on a thread wrote MAIN's window -- main's REPL
died with READ-ERROR after the first thread that ran the channel encoder.

THE RUNTIME LOCK (phase 5): hosted's B-LITE with a spinlock that BLOCKS a slot.  %BT-INIT opens
the threads gate (0x10000DB8) after carving a 4 MB lock arena off region 0's semispaces (leaving
their top 2 MB alone: the I/O staging pages are at 0x0FE00000); a locked section (SYMBOL-VALUE,
INTERN, ...) allocates in its CPU's slice of the arena, and a stop-the-world collection of region 0
evacuates it.  With the gate open a slot's special bindings are its own (%DYNBIND: the window's
self slot + a per-record dynamic-binding stack at 0x10740000), and each slot runs inside
%WITH-COMPUTATION-STATE.  The switch now parks/loads the allocation registers in each slot's
ACTIVE region (a slot switched out inside a locked section is in its slice).  The main thread's
waits (join, mutex spin, sleep, condition wait) are SAFE regions: a thread's region-0 collection
(arena refill) waited forever for a joiner that never allocates -- the first version hung so.

    (join-all (spawn-n 6 #'t-intern))   6 threads x 300 fresh INTERNs + FORMAT, 10-13 own-heap
                                        GCs each: 0 bad names; main finds all 1800; 2780 lock
                                        entries (980 contended), 0 arena fallbacks, region 0
                                        collected 17 times under them (arena refills)
    (join-all (spawn-n 6 #'t-special))  (let ((*sp* ...)) ...) per thread across 50 collections:
                                        50/50 each, and main's *SP* is still :GLOBAL
    NEGATIVE CONTROL, same image: close the gate after %BT-INIT -> the intern run dies
                                        TYPE-ERROR (datum 108, expected LIST) on main and the
                                        REPL is gone (READ-ERROR): the shared tables were
                                        pointing into the threads' heaps

SB-THREAD AND CHANNELS (phase 6): net/sb-thread-shim.lisp and net/modus-channel.lisp run
unchanged on the slots -- %HA-CARVE & co. map onto %BT-INIT, and the condition-variable entry
points are the slot runtime's.  Each slot's window carries the SHARED-STORE GUARD words (its
core's region pair), so the shim's JOIN-THREAD copies a result out of the thread's heap, and a
compiled store of a thread's object into shared memory traps -- on bare metal that is !!FAULT
and the core halts (loud, not a dangling pointer).  test/rpi-smp/shim.lisp, one boot:

    (s-value)      make-thread / join-thread                    (1 2 "three" 4)
    (s-channel)    3 threads x 200 messages -> main on one channel (4 KB ring, back-pressure)
                                                                SEEN (200 200 200) BAD 0
    (s-pingpong)   2 threads, 300 round trips over two channels  (OK OK)
    (s-mutex)      6 threads x 1000 under SB-THREAD:WITH-MUTEX   6000
    then intern / chain / sum as above, all clean
    threads start threads (phase 7): a thread BT-SPAWNs 3 and joins them; a thread
    SB-THREAD:MAKE-THREADs one and joins it (the shim carries the body in shared memory);
    a closure made in the thread's own heap is REFUSED (code -7), not stored where its
    heap's next collection would leave it dangling

PREEMPTION (phase 8): a slot runs a 10 ms slice -- a CNTVCT deadline the scheduler writes in its
per-CPU block (+0x3F00).  Past it, a loop back-edge clamps the allocation limit (the stop-the-world
poll, translate-aarch64 STW-CLAMP) and the next allocation enters the collector entry, which calls
the preempt routine (test/rpi-smp/preempt.s, address at 0x10213018): mark the slot runnable and
switch to the core's scheduler, every register already in the trampoline's frame.  Only at an
allocation, the one place a thread can stop safely; a loop that neither allocates nor blocks keeps
its core.  (preempt-run nil/t), one hog thread looping and consing on each core, then three
workers placed on the same cores:

    preemption on        workers done 3/3 after 3.4 s, all sums right
    slices off (control) workers done 0/3 after 20 s -- stuck behind the hogs

NOT YET: a thread SETQ-ing a global it has not bound, or a variable main closed over (with the
guard armed that is now a !!FAULT, not corruption -- making it a Lisp error on bare metal needs the
vector to resume into the guard's error path); and the board run (core 1 did not start on one board boot).
