;;;; hosted-slot-reuse.lisp — A THREAD SLOT'S REGION STARTS CLEAN.
;;;;
;;;;   ./modus --script test/hosted-slot-reuse.lisp
;;;;
;;;; Every thread slot owns one region, and the next thread in that slot gets
;;;; it re-initialised (net/hosted-sync.lisp %THR-PREPARE-REGION).  Its GC
;;;; bitmaps were not: the last thread's object-start and cons-kind bits stayed
;;;; set in both semispaces, the collector trusted them, and the new thread's
;;;; first collections walked into the middle of objects that were gone.  An
;;;; interpreted loop allocates as it runs, so a JIT-off image hit it at once:
;;;; the second thread in a slot died with a TYPE-ERROR its own HANDLER-CASE
;;;; never saw (the old thread's region collected an even number of times) or
;;;; the collector read past the heap's end (odd).  JIT code allocates too little
;;;; in such a loop to leave the bits.
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS: the slot not being reused (each round
;;;; reports the slot it ran on and must match the first), or the first thread
;;;; not collecting its region (its count must rise, both parities).

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

(%sb-threads-up)
(defun own-gcs () (%gc-meta-read (+ (%gc-region) #x20) (%gc-meta-scale)))

(defun churn (target)
  "Allocate until this thread's region has collected TARGET times (bounded),
   and report slot*1000000 + own-collections.  By collections, not
   iterations: JIT code allocates far less per turn than the interpreter.  A
   FIXNUM: a joined thread's heap result lives in its region, which the next
   thread in the slot reuses (sb-thread-shim JOIN-THREAD)."
  (let ((keep nil) (i 0))
    (loop
      (when (or (>= (own-gcs) target) (> i 20000000)) (return nil))
      (setq keep (cons (make-list 8) (if (> (length keep) 50) nil keep)))
      (setq i (+ i 1)))
    (+ (* (%thr-cpu) 1000000) (own-gcs))))

(defun count-to (n)
  (let ((c 0)) (dotimes (i n) (setq c (+ c 1))) c))

(format t "~%=== ONE SLOT, MANY THREADS, BOTH PARITIES =================~%")
(let ((slot0 nil) (parities nil))
  (dotimes (round 6)
    ;; A different collection count each round, so the previous thread
    ;; leaves the region with both even and odd counts.
    (let* ((n (+ 3 round))
           (r (sb-thread:join-thread
               (sb-thread:make-thread
                (lambda () (handler-case (churn n) (error (e) (list :err (%escape-describe e))))))))
           (c (sb-thread:join-thread
               (sb-thread:make-thread
                (lambda () (handler-case (count-to 60000) (error (e) (list :err (%escape-describe e)))))))))
      (format t "  round ~D: churn ~S, then a loop -> ~S~%" round r c)
      (when (integerp r)
        (unless slot0 (setq slot0 (floor r 1000000)))
        (chk (format nil "round ~D ran on the same slot" round) (floor r 1000000) slot0)
        (setq parities (cons (mod (mod r 1000000) 2) parities)))
      (chk (format nil "round ~D: the churn thread finished" round) (integerp r) t)
      (chk (format nil "round ~D: the next thread in the slot counts right" round) c 60000)))
  (chk "the churn threads collected their region with both parities"
       (and (member 0 parities) (member 1 parities) t) t))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "THREAD SLOT REUSE: PASS" "THREAD SLOT REUSE: FAIL"))
