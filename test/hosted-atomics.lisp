;;;; hosted-atomics.lisp — %ATOMIC-INCF / %ATOMIC-DECF UNDER FOUR REAL THREADS.
;;;;
;;;;   ./modus --script test/hosted-atomics.lisp                 (the test)
;;;;   MODUS_ATOMICS_MODE=unsync ./modus --script …              (the control)
;;;;
;;;; net/cooperative-atomics.lisp's three operations were atomic ONLY because
;;;; Modus ran a cooperative, single-core scheduler.  That argument died when
;;;; this branch grew native OS threads, and the file is not inert: every CLI
;;;; image bakes it as *GENERA-COMPAT-TEXT* and evaluates it at boot, so a
;;;; portable threading library that finds :GENERA in *FEATURES* lands on these.
;;;;
;;;; %ATOMIC-INCF and %ATOMIC-DECF now take a TTAS spinlock on
;;;; +OP-ATOMIC-XCHG+ when %RT-THREADS-LIVE-P says more than one thread of
;;;; control is live, and run the pre-SMP body verbatim when it does not.
;;;; %ATOMIC-CAS IS UNCHANGED AND STILL UNSAFE — it needs a compare-exchange
;;;; instruction the MVM does not have, and this test says so out loud rather
;;;; than quietly not covering it.
;;;;
;;;; WHAT WOULD MAKE THIS TEST A LIE, AND WHAT STOPS IT.
;;;;
;;;;   "The counter reached N*K" proves nothing unless the SAME WORKLOAD
;;;;   WITHOUT THE PROTECTION reaches less.  There are TWO negative controls,
;;;;   and they fail in different ways on purpose:
;;;;
;;;;     - IN THIS PROCESS, phase 5 runs the identical four-thread loop with
;;;;       the read-modify-write written out — which is character-for-character
;;;;       what %ATOMIC-INCF used to expand to — and REQUIRES the result to be
;;;;       strictly less than N*K.  A run in which it came out exact FAILS,
;;;;       because then phases 2-4 measured nothing.
;;;;     - IN A SECOND PROCESS, MODUS_ATOMICS_MODE=unsync runs the REAL MACRO
;;;;       with the gate cleared.  Same binary, same macro, same workload, one
;;;;       BSS word apart.  That is the arm that shows the GATE is what selects
;;;;       the safe path, rather than something else in the process.
;;;;
;;;;   "The gate bypasses the lock when it is off" would pass equally if the
;;;;   lock were taken every time, so it is not asserted from the ANSWER.
;;;;   %ATOMICS-ACQUISITIONS is bumped inside the critical section and only by
;;;;   the armed path, and phase 0 plants a SENTINEL in the lock word itself:
;;;;   1000 unarmed atomics must leave the sentinel intact and the counter at
;;;;   zero, and the armed phases must move the counter by EXACTLY the number
;;;;   of operations they performed.  A counter that could only ever read zero
;;;;   is the trap this tree has walked into twice.
;;;;
;;;;   "Four threads existed" is not simultaneity.  Every worker passes a
;;;;   barrier with a spin budget; the first arrival spins its whole budget out
;;;;   alone and reports a timeout, so all four barrier results must be 0.
;;;;
;;;;   "Each thread ran" is an ANSWER, not a flag: every worker reports the
;;;;   iteration count it actually completed and the driver requires K.
;;;;
;;;;   "Increments work" is not "the operations work": phase 3 is DECF only and
;;;;   phase 4 is INCF and DECF interleaved by the same threads on the same
;;;;   word, which must return a known value that neither alone would produce.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nthreads* 4)
(defvar *k* 20000)
(defvar *budget* 400000000)
(defvar *mode* nil)
(defvar *counter* 0)
(defvar *cell* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

;;; ------------------------------------------------------------------
;;; The shared word every worker hammers, and the per-worker reports.
;;;
;;; It is a RAW BAND WORD and not a Lisp global on purpose: SYMBOL-VALUE takes
;;; the runtime-table lock, which would serialise the workload all by itself
;;; and make the negative control come out exact — i.e. the instrument would
;;; move the bug, which is this campaign's most expensive recurring mistake.
;;; %THR-REGION-REPORT hands out 128-byte blocks at band+0xB000; slots 0..15
;;; belong to the N-regions test's workers, so this takes slot 12 while using
;;; only four threads, and %THR-REPORT (a different array, in the thread page)
;;; for the per-worker reports.
;;; ------------------------------------------------------------------

(defun shared-word () (%thr-region-report 12))

(defun read-shared () (mem-ref (shared-word) :u64))
(defun write-shared (v) (setf (mem-ref (shared-word) :u64) v))

(defun report-barrier (i) (%gc-read64 (+ (%thr-report i) #x00)))
(defun report-iters (i)   (%gc-read64 (+ (%thr-report i) #x08)))
(defun report-done (i)    (%gc-read64 (+ (%thr-report i) #x30)))

;;; The four worker bodies.  Each is returned by a function so the capture is
;;; real rather than a reference to a global the driver could change underneath.

(defun mk-incf (out addr k want budget)
  (lambda ()
    (%gc-write64 (+ out #x00) (%thr-arrive-and-wait want budget))
    (let ((i 0))
      (loop
        (when (>= i k) (return 0))
        (%atomic-incf (mem-ref addr :u64) 1)
        (setq i (+ i 1)))
      (%gc-write64 (+ out #x08) i))
    (%gc-write64 (+ out #x30) 99)
    0))

(defun mk-decf (out addr k want budget)
  (lambda ()
    (%gc-write64 (+ out #x00) (%thr-arrive-and-wait want budget))
    (let ((i 0))
      (loop
        (when (>= i k) (return 0))
        (%atomic-decf (mem-ref addr :u64) 1)
        (setq i (+ i 1)))
      (%gc-write64 (+ out #x08) i))
    (%gc-write64 (+ out #x30) 99)
    0))

(defun mk-mixed (out addr k want budget)
  "+3 then -2 per iteration, so the net is +K and neither operation alone can
   produce the answer."
  (lambda ()
    (%gc-write64 (+ out #x00) (%thr-arrive-and-wait want budget))
    (let ((i 0))
      (loop
        (when (>= i k) (return 0))
        (%atomic-incf (mem-ref addr :u64) 3)
        (%atomic-decf (mem-ref addr :u64) 2)
        (setq i (+ i 1)))
      (%gc-write64 (+ out #x08) i))
    (%gc-write64 (+ out #x30) 99)
    0))

(defun mk-unprotected (out addr k want budget)
  "THE NEGATIVE CONTROL.  The body is character-for-character what
   %ATOMIC-INCF expanded to before the fix."
  (lambda ()
    (%gc-write64 (+ out #x00) (%thr-arrive-and-wait want budget))
    (let ((i 0))
      (loop
        (when (>= i k) (return 0))
        (setf (mem-ref addr :u64) (+ (mem-ref addr :u64) 1))
        (setq i (+ i 1)))
      (%gc-write64 (+ out #x08) i))
    (%gc-write64 (+ out #x30) 99)
    0))

(defun run-arm (maker start)
  "Reset, seed the shared word, spawn N workers on MAKER, join them all.
   Returns the number of threads the kernel confirmed had exited."
  (%thr-reset-table)
  (write-shared start)
  (let ((h (make-array *nthreads*)) (i 0) (joined 0))
    (loop
      (when (>= i *nthreads*) (return 0))
      (aset h i (%make-native-thread
                 (funcall maker (%thr-report i) (shared-word)
                          *k* *nthreads* *budget*)))
      (setq i (+ i 1)))
    (setq i 0)
    (loop
      (when (>= i *nthreads*) (return 0))
      (if (> (aref h i) 0)
          (if (zerop (%join-native-thread (aref h i) *budget*))
              (setq joined (+ joined 1))
              0)
          0)
      (setq i (+ i 1)))
    joined))

(defun barrier-timeouts ()
  (let ((i 0) (n 0))
    (loop
      (when (>= i *nthreads*) (return 0))
      (if (zerop (report-barrier i)) 0 (setq n (+ n 1)))
      (setq i (+ i 1)))
    n))

(defun short-workers ()
  "Workers that did not complete their full K iterations, or did not finish."
  (let ((i 0) (n 0))
    (loop
      (when (>= i *nthreads*) (return 0))
      (if (= (report-iters i) *k*) 0 (setq n (+ n 1)))
      (if (= (report-done i) 99) 0 (setq n (+ n 1)))
      (setq i (+ i 1)))
    n))

(defun grade-arm (label want)
  (format t "  ~A: shared word = ~D (want ~D)~%" label (read-shared) want)
  (chk (concatenate 'string label " — barrier timeouts") (barrier-timeouts) 0)
  (chk (concatenate 'string label " — workers short of K") (short-workers) 0)
  (chk (concatenate 'string label " — the word is EXACT") (read-shared) want))

(setq *mode* (handler-case (sb-ext:posix-getenv "MODUS_ATOMICS_MODE")
               (t (c) nil)))

(format t "~%=== 0. THE UNARMED PATH IS THE OLD CODE =================~%")
(format t "  Before any bringup the gate word #x10000DB8 reads BSS zero, so~%")
(format t "  the macro's then-branch runs: the pre-SMP body, verbatim.~%")
(chk "the threads-live gate before bringup" (%rt-threads-live-p) 0)
(format t "  lock word   #x~X~%" (%atomics-lock-addr))
(format t "  acquisitions #x~X~%" (%atomics-acquisitions-addr))

;; A SENTINEL, not a zero.  If the unarmed path took the lock, %ATOMICS-RELEASE
;; would leave this word at 0 and the sentinel would be gone; a zero-vs-zero
;; check could not tell the two apart.
(%gc-write64 (%atomics-acquisitions-addr) 0)
(%gc-write64 (%atomics-lock-addr) 90)
(setq *counter* 0)
(let ((i 0))
  (loop
    (when (>= i 1000) (return 0))
    (%atomic-incf *counter* 1)
    (setq i (+ i 1))))
(let ((i 0))
  (loop
    (when (>= i 400) (return 0))
    (%atomic-decf *counter* 1)
    (setq i (+ i 1))))
(chk "1000 unarmed INCFs then 400 unarmed DECFs" *counter* 600)
(chk "the sentinel in the lock word is UNTOUCHED" (%gc-read64 (%atomics-lock-addr)) 90)
(chk "the acquisition counter never moved" (%atomics-acquisitions) 0)
(%gc-write64 (%atomics-lock-addr) 0)

(format t "~%  DELTA arguments and non-fixnum places still work:~%")
(setq *cell* (cons 5 nil))
(%atomic-incf (car *cell*) 7)
(chk "(%atomic-incf (car cell) 7) on 5" (car *cell*) 12)
(%atomic-decf (car *cell*) 20)
(chk "(%atomic-decf (car cell) 20)" (car *cell*) -8)

(format t "~%=== 0b. %ATOMIC-CAS IS UNCHANGED AND STILL UNSAFE ========~%")
(format t "  It is NOT covered by a threaded arm below, and that is the~%")
(format t "  finding, not an omission: XCHG is an UNCONDITIONAL exchange and~%")
(format t "  cannot express compare-and-swap.  Wrapping it in this spinlock~%")
(format t "  would pass a lock-based test and lie to the first caller that~%")
(format t "  mixed it with a plain SETF.  It needs LOCK CMPXCHG / LDAXR-STLXR.~%")
(setq *counter* 10)
(chk-true "CAS semantics preserved: matching OLD swaps" (%atomic-cas *counter* 10 11))
(chk "and the place took NEW" *counter* 11)
(chk-true "non-matching OLD does not swap" (null (%atomic-cas *counter* 10 99)))
(chk "and the place is untouched" *counter* 11)

(format t "~%=== 1. BRINGUP ==========================================~%")
(if (null (%sb-threads-up))
    (progn (format t "  SKIP: the actor band could not be carved.~%")
           (setq *fail* (+ *fail* 1)))
    (progn
      (if (equal *mode* "unsync")
          (progn
            (%rt-threads-off)
            (format t "~%  *** MODE=unsync: THE GATE IS CLEARED ON PURPOSE. ***~%")
            (format t "  Same binary, same macro, same workload, one BSS word~%")
            (format t "  apart.  Every arm below is EXPECTED to lose updates.~%"))
          0)
      (format t "  threads-live gate ~D   per-CPU active-region mode ~D~%"
              (%rt-threads-live-p) (%ha-percpu-mode))
      (if (equal *mode* "unsync")
          (chk "the gate is OFF (this is the control)" (%rt-threads-live-p) 0)
          (chk "the gate is ARMED" (%rt-threads-live-p) 1))

      (format t "~%=== 2. FOUR THREADS, ~D INCFs EACH ====================~%" *k*)
      (%gc-write64 (%atomics-acquisitions-addr) 0)
      (chk "threads the kernel confirmed had exited"
           (run-arm #'mk-incf 0) *nthreads*)
      (if (equal *mode* "unsync")
          (progn
            (format t "  shared word = ~D of ~D~%" (read-shared) (* *nthreads* *k*))
            (format t "  LOST UPDATES: ~D~%" (- (* *nthreads* *k*) (read-shared)))
            (chk-true "CONTROL: updates WERE lost with the gate off"
                      (< (read-shared) (* *nthreads* *k*))))
          (progn
            (grade-arm "INCF" (* *nthreads* *k*))
            (chk "every operation went through the lock (acquisitions)"
                 (%atomics-acquisitions) (* *nthreads* *k*))))

      (format t "~%=== 3. FOUR THREADS, ~D DECFs EACH ====================~%" *k*)
      (%gc-write64 (%atomics-acquisitions-addr) 0)
      (chk "threads the kernel confirmed had exited"
           (run-arm #'mk-decf (* *nthreads* *k*)) *nthreads*)
      (if (equal *mode* "unsync")
          (progn
            (format t "  shared word = ~D (want 0)~%" (read-shared))
            (chk-true "CONTROL: DECF also lost updates with the gate off"
                      (> (read-shared) 0)))
          (progn
            (grade-arm "DECF" 0)
            (chk "every operation went through the lock (acquisitions)"
                 (%atomics-acquisitions) (* *nthreads* *k*))))

      (format t "~%=== 4. INCF AND DECF INTERLEAVED ========================~%")
      (format t "  Each thread does (+3) then (-2), ~D times.  Net +~D each,~%"
              *k* *k*)
      (format t "  so the answer is one NEITHER operation alone produces.~%")
      (%gc-write64 (%atomics-acquisitions-addr) 0)
      (chk "threads the kernel confirmed had exited"
           (run-arm #'mk-mixed 0) *nthreads*)
      (if (equal *mode* "unsync")
          (progn
            (format t "  shared word = ~D (want ~D)~%"
                    (read-shared) (* *nthreads* *k*))
            (chk-true "CONTROL: the mixed arm did not return the known value"
                      (not (= (read-shared) (* *nthreads* *k*)))))
          (progn
            (grade-arm "MIXED" (* *nthreads* *k*))
            (chk "every operation went through the lock (acquisitions)"
                 (%atomics-acquisitions) (* 2 (* *nthreads* *k*)))))

      (format t "~%=== 5. THE IN-PROCESS NEGATIVE CONTROL ==================~%")
      (format t "  The same four threads, the same shared word, and the~%")
      (format t "  read-modify-write written out — which is exactly what~%")
      (format t "  %ATOMIC-INCF expanded to before the fix.~%")
      (chk "threads the kernel confirmed had exited"
           (run-arm #'mk-unprotected 0) *nthreads*)
      (format t "  shared word = ~D of ~D~%" (read-shared) (* *nthreads* *k*))
      (format t "  LOST UPDATES: ~D~%" (- (* *nthreads* *k*) (read-shared)))
      (chk "control — barrier timeouts" (barrier-timeouts) 0)
      (chk "control — workers short of K" (short-workers) 0)
      (chk-true "and updates WERE lost, so phases 2-4 had something to prove"
                (< (read-shared) (* *nthreads* *k*)))

      (format t "~%=== 6. THE LOCK IS FREE AGAIN ===========================~%")
      (chk "the lock word after every arm" (%gc-read64 (%atomics-lock-addr)) 0)))

(format t "~%=== VERDICT ==============================================~%")
(if (= *fail* 0)
    (format t "ATOMICS UNDER FOUR THREADS: PASS (~D checks)~%" *checks*)
    (format t "ATOMICS UNDER FOUR THREADS: FAIL (~D of ~D checks)~%"
            *fail* *checks*))
