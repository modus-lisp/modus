;;;; hosted-many-regions.lisp — EIGHT THREADS, EIGHT HEAPS, EIGHT COLLECTORS.
;;;;
;;;;   ./modus --script test/hosted-many-regions.lisp
;;;;
;;;; Until this, %MAKE-NATIVE-THREAD handed back a thread THAT COULD NOT CONS.
;;;; net/hosted-actors.lisp's carve produced exactly TWO GC regions out of
;;;; region 0 — enough for two actors on two threads — while the thread layer
;;;; had sixteen slots, so fourteen of them had a stack, a per-thread window and
;;;; a per-CPU block but no heap.  A thread body was restricted to "fixnum
;;;; arithmetic, raw memory and syscalls only", which is not a thread anybody
;;;; can be given.
;;;;
;;;; The carve now produces one region per thread slot.  This is the acceptance.
;;;;
;;;; WHAT WOULD MAKE THIS A LIE, AND WHAT STOPS IT.
;;;;
;;;;   "Eight threads existed" is not simultaneity.  Every worker goes through a
;;;;   barrier with a spin budget: the first arrival spins its whole budget out
;;;;   alone and reports a timeout.  All eight barrier results must be 0, which
;;;;   cannot happen unless all eight were inside it at once.
;;;;
;;;;   "Each collected" is a COUNT ON ITS OWN CONTROL BLOCK.  Eight blocks each
;;;;   rising by at least the twelve collections its worker forced cannot be
;;;;   produced by one shared counter, which would rise eight times as fast on
;;;;   one block and not at all on the others.
;;;;
;;;;   "Its data survived" is a WALK.  The chain is re-walked between every pair
;;;;   of collections, not only at the end, and the final walk must return
;;;;   1 + (0+1+…+(m-1)) for ITS OWN m — an ANSWER derived from the data, not a
;;;;   flag, and a DIFFERENT answer for every worker, because worker i holds
;;;;   400 + 7i links.  A thread running somebody else's closure is a wrong
;;;;   number rather than an indistinguishable success.
;;;;
;;;;   "Another thread's region is untouched" is measured WITH THE WORKERS GONE
;;;;   and with the checksum asserted NON-ZERO first.  A checksum over an empty
;;;;   range is equal to itself for any two runs, so "unchanged" would otherwise
;;;;   be free.  After the join the driver collects ITS OWN region twice and
;;;;   every worker's heap and control block must be bit-for-bit identical.
;;;;   THE EIGHT CHECKSUMS MUST ALSO BE DISTINCT: %GC-SUM-RANGE folds to 24 bits
;;;;   and the regions are 16 MB — exactly 2^24 — apart, so eight IDENTICAL
;;;;   chains produce eight IDENTICAL checksums (measured: they did), and
;;;;   "unchanged" would then also hold if one region held another's data.
;;;;
;;;;   "The regions are isolated" is %GC-COUNT-FOREIGN-REFS over EVERY ORDERED
;;;;   PAIR — 56 sweeps — with a POSITIVE CONTROL that must answer 1 for a
;;;;   planted pointer and 0 for the same window against a different region.
;;;;
;;;; SCOPE, STATED.  The workers do arithmetic, raw memory access and CONS.  No
;;;; FORMAT, no INTERN, no EVAL, no symbol or keyword literal: the runtime's
;;;; shared tables are unsynchronised unless %RT-THREADS-ON has been called, and
;;;; that lock is a different test's subject.  What is exercised here is the
;;;; HEAP.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nthreads* 8)
(defvar *nlinks* 400)
(defvar *ngc* 12)
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

(defun w (a off) (%gc-read64 (+ a off)))

(format t "~%=== EIGHT THREADS, EIGHT HEAPS ===========================~%")

(let ((res (%thr-nregion-selftest *nthreads* *nlinks* *ngc* *budget*)))
  (if (zerop res)
      (progn (format t "  SKIP: the band could not be carved.~%")
             (setq *fail* (+ *fail* 1)))
      (progn

        (format t "~%-- the carve --------------------------------------------~%")
        (chk "threads asked for" (w res #x00) *nthreads*)
        (chk "regions carved" (w res #x08) 16)
        (chk "metadata scale" (w res #xA0) 1)
        (chk "region-alignment violations" (w res #x30) 0)
        (chk "last violation mask" (w res #x38) 0)
        (format t "  ... region i's from-space = 0x~X + i * 0x~X~%"
                (%ha-region-from 0) *ha-rsize*)
        (format t "  ... region i's control block = 0x~X + i * 0x40~%" (%ha-rcb 0))

        (format t "~%-- spawning and joining ---------------------------------~%")
        (chk "spawn failures" (w res #x10) 0)
        (chk "join timeouts" (w res #x18) 0)

        (format t "~%-- every worker ran its own body ------------------------~%")
        (let ((i 0))
          (loop
            (when (>= i *nthreads*) (return 0))
            (let ((rp (%thr-region-report (+ i 1))))
              (chk (format nil "worker ~A reached the end" i) (w rp #x60) 99))
            (setq i (+ i 1))))

        (format t "~%-- ALL EIGHT WERE INSIDE THE BARRIER AT ONCE ------------~%")
        (let ((i 0))
          (loop
            (when (>= i *nthreads*) (return 0))
            (chk (format nil "worker ~A barrier" i)
                 (w (%thr-region-report (+ i 1)) #x00) 0)
            (setq i (+ i 1))))

        (format t "~%-- each thread landed in ITS OWN region -----------------~%")
        (chk "workers in the wrong region" (w res #x88) 0)
        (chk "workers on the wrong active-region cell" (w res #x90) 0)
        (chk "distinct worker regions" (w res #xC8) *nthreads*)
        (chk "distinct worker gettids" (w res #xD0) *nthreads*)
        (let ((i 0))
          (loop
            (when (>= i *nthreads*) (return 0))
            (chk (format nil "worker ~A region" i)
                 (w (%thr-region-report (+ i 1)) #x08) (%ha-rcb (+ i 1)))
            (setq i (+ i 1))))

        (format t "~%-- EACH COLLECTED ITS OWN REGION, AND THE COUNTS ROSE ---~%")
        (format t "-- INDEPENDENTLY (one block per thread, not one counter) --~%")
        (let ((i 0))
          (loop
            (when (>= i *nthreads*) (return 0))
            (let* ((rp (%thr-region-report (+ i 1)))
                   (before (w rp #x28))
                   (after (w rp #x30)))
              (chk (format nil "worker ~A started at 0 collections" i) before 0)
              (chk-true (format nil "worker ~A collected >= ~A times (~A)"
                                i *ngc* after)
                        (>= after *ngc*)))
            (setq i (+ i 1))))

        (format t "~%-- ITS DATA SURVIVED ITS OWN COLLECTIONS ----------------~%")
        (let ((i 0))
          (loop
            (when (>= i *nthreads*) (return 0))
            (let* ((rp (%thr-region-report (+ i 1)))
                   (m (+ *nlinks* (* i 7)))
                   (want (+ 1 (ash (* m (- m 1)) -1))))
              (chk (format nil "worker ~A broken walks mid-run" i) (w rp #x38) 0)
              (chk (format nil "worker ~A walked its OWN ~A links" i m)
                   (w rp #x40) want))
            (setq i (+ i 1))))
        (chk "distinct chain answers" (w res #xD8) *nthreads*)

        (format t "~%-- ANOTHER THREAD'S REGION IS BIT-FOR-BIT UNCHANGED -----~%")
        (format t "-- across the driver collecting ITS own region twice -----~%")
        (chk "driver's own region collected" (- (w res #x50) (w res #x48)) 2)
        (chk "driver's chain survived"
             (w res #x98) (+ 1 (ash (* *nlinks* (- *nlinks* 1)) -1)))
        (chk "workers whose heap checksum was ZERO (unassertable)" (w res #x80) 0)
        (chk "distinct heap checksums" (w res #xE0) *nthreads*)
        (chk "workers whose heap checksum MOVED" (w res #x70) 0)
        (chk "workers whose control block MOVED" (w res #x78) 0)
        (let ((i 0))
          (loop
            (when (>= i *nthreads*) (return 0))
            (let ((s (+ res (+ #x100 (* i #x20)))))
              (chk-true (format nil "worker ~A heap checksum ~A is non-zero"
                                i (w s 0))
                        (> (w s 0) 0))
              (chk (format nil "worker ~A heap checksum unchanged" i)
                   (w s 8) (w s 0))
              (chk (format nil "worker ~A control block unchanged" i)
                   (w s #x18) (w s #x10)))
            (setq i (+ i 1))))

        (format t "~%-- NO REGION POINTS INTO ANOTHER (56 ordered pairs) -----~%")
        (chk "foreign references, all ordered pairs" (w res #x58) 0)
        (chk "POSITIVE CONTROL: a planted pointer IS counted" (w res #x60) 1)
        (chk "... and is not counted against another region" (w res #x68) 0)

        (format t "~%-- REGION 0 DID NOT COLLECT, AND THE DRIVER CAME BACK ---~%")
        (chk "region 0 collections" (w res #x28) (w res #x20))
        (chk "collectors still inside at the end" (w res #xB0) 0)
        (chk "per-CPU mode restored" (%ha-percpu-mode) (w res #xC0))
        (format t "  ... collectors that entered while another was inside: ~A~%"
                (w res #xA8))

        (format t "~%-- the toplevel is still alive and can still allocate ---~%")
        (chk-true "a fresh cons in region 0" (consp (cons 1 2)))
        (chk "region 0 is the active region" (%gc-region) (%gc-region-0)))))

(format t "~%=== VERDICT ==============================================~%")
(if (zerop *fail*)
    (format t "EIGHT THREADS, EIGHT HEAPS: PASS (~A checks)~%" *checks*)
    (format t "EIGHT THREADS, EIGHT HEAPS: FAIL (~A of ~A checks)~%"
            (- *checks* *fail*) *checks*))
