;;;; hosted-threads.lisp — STEP 2: A SECOND NATIVE OS THREAD, NO ACTORS.
;;;;
;;;;   ./modus --script test/hosted-threads.lisp
;;;;
;;;; Everything Modus has ever called concurrency was COOPERATIVE FIBERS in one
;;;; thread: `clone' appears nowhere in the tree, +OP-YIELD+ is a NOP on Linux,
;;;; and until step 1 the scheduler lock was a no-op.  This is the first time an
;;;; image has had two kernel-scheduled threads in it.
;;;;
;;;; WHY IT NEEDED A DEDICATED ASSEMBLY STUB (translate-x64 TRAP #x0540) rather
;;;; than `(syscall6 56 …)'.  clone RETURNS TWICE.  The child resumes at the
;;;; instruction after SYSCALL with RAX=0 and RSP on the NEW stack — but with
;;;; every other register, RBP included, still holding the PARENT's values.
;;;; This compiler keeps let-bound locals in RBP-RELATIVE FRAME SLOTS
;;;; (translate-x64's +FRAME-SLOT-BASE+), so one more line of compiled Lisp in
;;;; the child reads and WRITES the parent's live frame from a second thread;
;;;; and returning through the caller's frame would RET off a stack that has no
;;;; return address on it.  So the branch happens in the instruction stream
;;;; before any compiled code runs: the child zeroes RBP, CALLs a zero-argument
;;;; entry function on its own stack, and when that returns issues SYS_exit (60,
;;;; which ends the THREAD — 231/exit_group would end the process).
;;;;
;;;; WHAT WOULD MAKE THIS TEST A LIE, and what stops it.
;;;;
;;;;   "Two TIDs exist" proves nothing — a thread that ran to completion before
;;;;   the parent looked would produce the same two numbers.  So the two threads
;;;;   meet at a BARRIER: each bumps a shared counter under a lock and then
;;;;   spins until the counter reads 2.  Run them sequentially and the first
;;;;   arrival spins out its entire budget alone and reports a TIMEOUT.  Both
;;;;   sides report 0, so both were inside the barrier at the same instant.
;;;;   (The budget is deliberate: a wrong answer must FAIL, not hang.)
;;;;
;;;;   "Both made progress" proves nothing on its own either, so each thread
;;;;   also counts how many of its own iterations saw the OTHER thread's counter
;;;;   change underneath it.  Sequential execution scores exactly zero.
;;;;
;;;;   "The thread finished" is not the thread's own say-so.  clone is issued
;;;;   with CLONE_PARENT_SETTID|CLONE_CHILD_CLEARTID against one word: the
;;;;   kernel writes the TID there before clone returns and ZEROES it when the
;;;;   thread has actually exited.  The join polls that word.
;;;;
;;;; THE ONE THING THE SECOND THREAD MUST NOT DO IS ALLOCATE, and the test
;;;; MEASURES it rather than trusting it.  R12 (bump-allocation pointer) and R14
;;;; (its limit) are ordinary registers, so the child gets a COPY of the
;;;; parent's — two threads allocating from two copies of one pointer hand out
;;;; the SAME addresses.  Giving each thread its own is step 3.  Here the thread
;;;; records its alloc pointer at entry and at exit and they must be equal.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *budget* 200000000)
(defvar *work* 4000000)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(let ((res (%ha-threads-selftest *budget* *work*)))
  (if (= res 0)
      (format t "~%SKIP: no actor band, or the thread stack could not be mapped.~%")
      (let* ((tid     (w res #x00))
             (ktid    (w res #x08))
             (t2tid   (w res #x10))
             (t1tid   (w res #x18))
             (t2pid   (w res #x20))
             (t1pid   (w res #x28))
             (started (w res #x30))
             (t2to    (w res #x38))
             (t1to    (w res #x40))
             (arrived (w res #x48))
             (t2prog  (w res #x50))
             (t1prog  (w res #x58))
             (t2saw   (w res #x60))
             (t1saw   (w res #x68))
             (clean   (w res #x70))
             (join    (w res #x78))
             (tidafter (w res #x80))
             (work    (w res #x88))
             (stkbase (w res #x98))
             (stktop  (w res #xA0))
             (t2a0    (w res #xA8))
             (t2a1    (w res #xB0))
             (t1a0    (w res #xB8)))

        (format t "~%=== THE CLONE ============================================~%")
        (format t "  thread stack ~X .. ~X (mmap, PROT_RW, outside every heap)~%"
                stkbase stktop)
        (chk-true "clone(2) returned a positive TID to the parent" (> tid 0))
        (format t "  %spawn-thread returned TID  ~D~%" tid)
        (format t "  CLONE_PARENT_SETTID word    ~D~%" ktid)
        (chk "and the kernel put the same TID in the join word" ktid tid)

        (format t "~%=== TWO THREADS, ONE PROCESS =============================~%")
        (format t "  thread 1 gettid ~D   getpid ~D~%" t1tid t1pid)
        (format t "  thread 2 gettid ~D   getpid ~D~%" t2tid t2pid)
        (chk "the second thread ran its entry function" started 1)
        (chk-true "the two gettids are DIFFERENT" (not (= t1tid t2tid)))
        (chk "thread 2's gettid is the TID clone reported" t2tid tid)
        (chk "the two getpids are the SAME (one thread group)" t2pid t1pid)
        (chk-true "and thread 1's gettid IS the pid (it is the group leader)"
                  (= t1tid t1pid))

        (format t "~%=== THE BARRIER: THEY WERE BOTH THERE AT ONCE ============~%")
        (format t "  Each thread bumps a counter under a lock and then spins~%")
        (format t "  until it reads 2.  Sequential execution CANNOT pass this:~%")
        (format t "  whichever ran first would spin out its whole budget alone.~%")
        (chk "arrivals" arrived 2)
        (chk "thread 1 spun out its budget alone (1 = yes)" t1to 0)
        (chk "thread 2 spun out its budget alone (1 = yes)" t2to 0)

        (format t "~%=== AND THEN BOTH MADE PROGRESS, INTERLEAVED =============~%")
        (format t "  Each ran ~D iterations bumping its OWN counter and~%" work)
        (format t "  watching the other's.  A sequential run scores ZERO~%")
        (format t "  observations; these are the counts actually recorded.~%")
        (chk "thread 1 iterations" t1prog work)
        (chk "thread 2 iterations" t2prog work)
        (format t "  thread 1 saw thread 2's counter change ~D times~%" t1saw)
        (format t "  thread 2 saw thread 1's counter change ~D times~%" t2saw)
        (chk-true "thread 1 observed thread 2 advancing" (> t1saw 0))
        (chk-true "thread 2 observed thread 1 advancing" (> t2saw 0))

        (format t "~%=== A CLEAN EXIT =========================================~%")
        (chk "thread 2 reached the end of its entry function" clean 1)
        (chk "the join saw the kernel clear the TID word (0 = joined)" join 0)
        (chk "and the TID word afterwards" tidafter 0)

        (format t "~%=== THE SECOND THREAD ALLOCATED NOTHING ==================~%")
        (format t "  R12 is an ordinary register, so the child got a COPY of~%")
        (format t "  this thread's bump pointer.  Two threads allocating from~%")
        (format t "  two copies of one pointer hand out the same addresses;~%")
        (format t "  a region per thread is step 3.  Until then: measure it.~%")
        (format t "  thread 2 alloc ptr at entry ~X~%" t2a0)
        (format t "  thread 2 alloc ptr at exit  ~X~%" t2a1)
        (format t "  thread 1 alloc ptr at spawn ~X~%" t1a0)
        (chk "thread 2's alloc pointer moved by" (- t2a1 t2a0) 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "HOSTED x64 NATIVE THREADS: PASS (~D checks)~%" *checks*)
            (format t "HOSTED x64 NATIVE THREADS: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
