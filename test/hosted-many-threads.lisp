;;;; hosted-many-threads.lisp — EIGHT THREADS, EIGHT CLOSURES, EIGHT ANSWERS.
;;;;
;;;;   ./modus --script test/hosted-many-threads.lisp
;;;;
;;;; Until this, the image ran exactly TWO OS threads and %SPAWN-THREAD took a
;;;; RAW NATIVE ENTRY ADDRESS — one stack, one thread block, one TID word, and
;;;; a thread body that had to be a zero-argument top-level DEFUN.  A closure
;;;; could not be a thread body, so a thread could carry no state of its own,
;;;; which is the first thing MAKE-THREAD is ever asked for.
;;;;
;;;; WHAT WOULD MAKE THIS A LIE, AND WHAT STOPS IT.
;;;;
;;;;   "Eight threads existed" is not simultaneity.  Every worker goes through
;;;;   a barrier with a spin budget: the first arrival spins its whole budget
;;;;   out alone and reports a TIMEOUT.  All eight barrier results must be 0,
;;;;   which cannot happen unless all eight were inside the barrier at once.
;;;;
;;;;   "Each did its own work" is an ANSWER, not a flag.  Each closure captures
;;;;   a different K and sums K two hundred thousand times the long way; the
;;;;   driver requires each worker's total to be exactly its own K*200000.  A
;;;;   thread running the wrong closure produces a wrong number, and two
;;;;   threads sharing one captured value produce two identical ones.
;;;;
;;;;   "They were really separate threads" is the KERNEL's answer, twice over:
;;;;   each reports its own gettid (all eight must differ, and all must differ
;;;;   from the driver's), and the join waits on the TID word the kernel itself
;;;;   clears on exit (CLONE_CHILD_CLEARTID) rather than on a flag the thread
;;;;   set about itself.
;;;;
;;;;   "They each got their own machine state" is checked directly: each
;;;;   reports the CPU id it reads through its OWN GS base and the per-thread
;;;;   window base it reads through its OWN FS base.  All eight of each must be
;;;;   distinct, and none may be the driver's.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nthreads* 8)
(defvar *iters* 200000)
(defvar *budget* 400000000)

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

(defun all-distinct-p (n off)
  "T when the N workers' values at report offset OFF are pairwise different."
  (let ((i 0) (ok t))
    (loop
      (when (>= i n) (return 0))
      (let ((j (+ i 1)))
        (loop
          (when (>= j n) (return 0))
          (when (= (%gc-read64 (+ (%thr-report i) off))
                   (%gc-read64 (+ (%thr-report j) off)))
            (setq ok nil))
          (setq j (+ j 1))))
      (setq i (+ i 1)))
    ok))

(format t "~%=== EIGHT THREADS, EACH FROM A CLOSURE ===================~%")

(if (zerop (%thr-reset-table))
    (progn (format t "  SKIP: the thread page could not be mapped.~%")
           (setq *fail* (+ *fail* 1)))
    (let ((handles (make-array *nthreads*))
          (mytid (syscall3 186 0 0 0))
          (spawned 0)
          (i 0))

      ;; Spawn: each closure captures a DIFFERENT k and a DIFFERENT report block.
      (loop
        (when (>= i *nthreads*) (return 0))
        (let ((h (%make-native-thread
                  (%thr-make-counter (+ i 1) (%thr-report i)
                                     *nthreads* *budget*))))
          (aset handles i h)
          (if (> h 0) (setq spawned (+ spawned 1)) 0))
        (setq i (+ i 1)))

      (format t "  spawned ~D of ~D~%" spawned *nthreads*)
      (chk "threads successfully started" spawned *nthreads*)

      ;; Join every one of them, on the kernel's answer.
      (setq i 0)
      (let ((joined 0))
        (loop
          (when (>= i *nthreads*) (return 0))
          (if (> (aref handles i) 0)
              (if (zerop (%join-native-thread (aref handles i) *budget*))
                  (setq joined (+ joined 1))
                  0)
              0)
          (setq i (+ i 1)))
        (chk "threads the kernel confirmed had exited" joined *nthreads*))

      (format t "~%=== THEY WERE ALL INSIDE THE BARRIER AT ONCE =============~%")
      (format t "  A non-zero barrier result is what a SEQUENTIAL run scores:~%")
      (format t "  the first arrival spins its whole budget out alone.~%")
      (setq i 0)
      (let ((timeouts 0))
        (loop
          (when (>= i *nthreads*) (return 0))
          (if (zerop (%gc-read64 (+ (%thr-report i) #x00)))
              0 (setq timeouts (+ timeouts 1)))
          (setq i (+ i 1)))
        (chk "workers that spun their barrier budget out alone" timeouts 0))

      (format t "~%=== EACH DID ITS OWN WORK ================================~%")
      (setq i 0)
      (loop
        (when (>= i *nthreads*) (return 0))
        (let ((r (%thr-report i)))
          (format t "  worker ~D: k=~D  sum=~D  tid=~D  cpu=~D  window=~D~%"
                  i (%gc-read64 (+ r #x08)) (%gc-read64 (+ r #x10))
                  (%gc-read64 (+ r #x18)) (%gc-read64 (+ r #x20))
                  (%gc-read64 (+ r #x28))))
        (setq i (+ i 1)))
      (setq i 0)
      (let ((wrongk 0) (wrongsum 0) (unfinished 0))
        (loop
          (when (>= i *nthreads*) (return 0))
          (let ((r (%thr-report i)))
            (if (= (%gc-read64 (+ r #x08)) (+ i 1)) 0 (setq wrongk (+ wrongk 1)))
            (if (= (%gc-read64 (+ r #x10)) (* (+ i 1) *iters*))
                0 (setq wrongsum (+ wrongsum 1)))
            (if (= (%gc-read64 (+ r #x30)) 99) 0 (setq unfinished (+ unfinished 1))))
          (setq i (+ i 1)))
        (chk "workers that ran the wrong closure (wrong captured K)" wrongk 0)
        (chk "workers whose arithmetic did not match their own K" wrongsum 0)
        (chk "workers that did not reach the end of their body" unfinished 0))

      (format t "~%=== EACH HAD ITS OWN MACHINE STATE =======================~%")
      (chk-true "all eight gettids differ" (all-distinct-p *nthreads* #x18))
      (chk-true "all eight CPU ids differ" (all-distinct-p *nthreads* #x20))
      (chk-true "all eight per-thread window bases differ"
                (all-distinct-p *nthreads* #x28))
      (setq i 0)
      (let ((asdriver 0) (zerowin 0))
        (loop
          (when (>= i *nthreads*) (return 0))
          (let ((r (%thr-report i)))
            (if (= (%gc-read64 (+ r #x18)) mytid) (setq asdriver (+ asdriver 1)) 0)
            (if (zerop (%gc-read64 (+ r #x28))) (setq zerowin (+ zerowin 1)) 0))
          (setq i (+ i 1)))
        (chk "workers reporting the DRIVER's tid" asdriver 0)
        (chk "workers still on the process-wide window (base 0)" zerowin 0))

      (chk "threads the table counted as started" (%gc-read64 (%thr-started))
           *nthreads*)))

(format t "~%=== VERDICT ==============================================~%")
(if (= *fail* 0)
    (format t "EIGHT THREADS FROM EIGHT CLOSURES: PASS (~D checks)~%" *checks*)
    (format t "EIGHT THREADS FROM EIGHT CLOSURES: FAIL (~D of ~D checks)~%"
            *fail* *checks*))
