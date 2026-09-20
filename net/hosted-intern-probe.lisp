;;;; hosted-intern-probe.lisp — THE SAME INTERN LOOP, COMPILED INTO THE IMAGE.
;;;;
;;;; This file exists to settle ONE question and nothing else.
;;;;
;;;; test/hosted-intern-layers.lisp's `low' arm — a worker thread calling
;;;; %INTERN-SYMBOL-PKG on fresh name-hashes in a loop — dies about half the
;;;; time with `MVM LONGJMP (TRAP #x0511) with no active handler-case', while
;;;; its cross-region audit reads ZERO on every run that completes.  So it is
;;;; not the region violation that CL:INTERN commits, and it is a SECOND defect.
;;;;
;;;; Against that sits test/hosted-thread-lisp.lisp, which interns fresh symbols
;;;; through the SAME %INTERN-SYMBOL-PKG, on two threads, 300 iterations each,
;;;; and scores 100 of 100.  Two differences were left standing between them:
;;;; the green one holds the runtime lock across a whole iteration (already
;;;; measured, and NOT the variable — one lock held across K interns is WORSE
;;;; than lock-per-call), and the green one's loop is AOT-COMPILED INSIDE THE
;;;; IMAGE (%TL-SELFTEST, net/hosted-sync.lisp) while the red one's loop is
;;;; runtime-compiled script code.
;;;;
;;;; That last difference cannot be measured from a binary that does not contain
;;;; the loop.  This file puts it there.
;;;;
;;;; WHY IT MATTERS BEYOND THE TEST.  glass is LOADed at runtime, so its RFB
;;;; sender loop is runtime-compiled.  If runtime-compiled is the variable, this
;;;; is the same defect that stops the frame, not a cousin of it.
;;;;
;;;; ============================================================
;;;; WHAT MAKES THE COMPARISON FAIR
;;;; ============================================================
;;;;
;;;; The functions below are a LINE-FOR-LINE copy of the ones in
;;;; test/hosted-intern-layers.lisp, renamed `%ip-' so both can live in one
;;;; process at once.  test/hosted-intern-aot.lisp defines the script-side copy
;;;; itself and then runs EITHER this one OR that one, chosen by an environment
;;;; variable, in the SAME process shape: the same %HA-ACTORS-BRINGUP, the same
;;;; SB-THREAD:MAKE-THREAD, the same lambda, the same audit, the same driver.
;;;; The only thing that moves between the two arms is WHERE THE LOOP WAS
;;;; COMPILED.  Everything the loop is called FROM is runtime-compiled in both.
;;;;
;;;; A difference between the arms is therefore the compilation mode.  No
;;;; difference between them says the compilation mode is NOT the variable and
;;;; sends the search back to the harness shape.

(defun %ip-count () (hash-table-count (mem-ref #x10000088 :u64)))

(defun %ip-fwd (r0)
  "region 0's LIVE span -> this worker's region.  The forbidden direction."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0alloc (%gc-meta-read (+ r0 #x30) k)))
    (%gc-count-foreign-refs r0from r0alloc wfrom wsize)))

(defun %ip-in-worker (w)
  "1 when machine word W points inside THIS worker's region, 0 otherwise."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (from (%gc-meta-read (+ rw #x00) k))
         (size (%gc-meta-read (+ rw #x10) k)))
    (if (and (>= w from) (< w (+ from size))) 1 0)))

(defun %ip-body-str (i) (concatenate 'string "ILS-" (write-to-string i)))
(defun %ip-body-low (i) (%intern-symbol-pkg (+ 900000000 i) 0))
(defun %ip-body-cl  (i)
  (intern (concatenate 'string "ILC-" (write-to-string i)) "COMMON-LISP-USER"))

(defun %ip-worker (arm r0 scr k)
  (let ((c0 (%ip-count))
        (f0 (%ip-fwd r0))
        (i 0)
        (last nil))
    (loop
      (when (>= i k) (return 0))
      (setq last (cond ((string= arm "str") (%ip-body-str i))
                       ((string= arm "low") (%ip-body-low i))
                       (t                   (%ip-body-cl i))))
      (setq i (+ i 1)))
    (list c0 (%ip-count) f0 (%ip-fwd r0)
          (%ip-in-worker (%gc-word-of last (+ scr 512)))
          (if last 1 0))))
