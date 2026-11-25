;;;; test/coroutine-quote-pool.lisp — a coroutine grows the quote pool as main
;;;; does (mvm/mvm-eval.lisp, %E2-CONST-REGISTER).
;;;; Run: ./modus --dynamic-space-size 16384 --load test/coroutine-quote-pool.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; A coroutine has a binding block, and %E2-CONST-REGISTER used to take that
;;;; for a worker: the pool's doubled vector was made under the runtime lock, in
;;;; the 32 MB lock arena.  At about a million entries the doubling no longer
;;;; fit, the slice was collected again and again, and the process died "heap
;;;; exhausted" with gigabytes of region 0 free.  Each compiled literal is an
;;;; entry, so a long-running coroutine that EVALs -- cortez's TI machine -- got
;;;; there in its first hour.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defvar *main* 0)
(defvar *n* 4200000)   ; past two doublings beyond a million: 16 MB, then 32 MB

(coroutines-start)
(setq *main* (coroutine-current))

(let* ((reg (find-symbol "%E2-CONST-REGISTER" "MODUS-INT"))
       (ref (find-symbol "%E2-CONST-REF" "MODUS-INT"))
       (c (coroutine-create
           (lambda (v)
             (declare (ignore v))
             (let ((first nil) (last nil) (probe (list :probe)))
               (dotimes (i *n*)
                 (let ((idx (funcall reg (if (= i 0) probe i))))
                   (when (= i 0) (setq first idx))
                   (setq last idx)))
               (coroutine-transfer *main*
                                   (list first last
                                         (eq (funcall ref first) probe)
                                         (eql (funcall ref last) (- *n* 1)))))))))
  (let ((r (coroutine-transfer c nil)))
    (check "a coroutine registers four million constants and comes back" (consp r))
    (when (consp r)
      (destructuring-bind (first last first-back last-back) r
        (check "with dense indices, one per constant" (= (- last first) (- *n* 1)))
        (check "and the first entry reads back the object registered, after the doublings"
               first-back)
        (check "and so does the last" last-back)))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
