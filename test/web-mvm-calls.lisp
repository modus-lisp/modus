;;;; test/web-mvm-calls.lisp -- the JS MVM (web/mvm.js) on calls the gate's
;;;; (* 6 7) never makes.  Two bugs it let through, both web-only:
;;;;
;;;;   1. AREF on VFP.  The &rest prologue's loop walks frame slots with
;;;;      (:aref val VFP idx); mvm.js read the frame as a vector, so every call
;;;;      with four or more &rest arguments -- (LIST a b c d e), OPEN's keywords,
;;;;      so every LOAD -- got garbage (adebd4fe, 2026-10-22).
;;;;   2. A THROW from an interpreted function to a CATCH in a translated one
;;;;      escaped DRIVE() and was taken by the caller's interpreter loop, which
;;;;      then ended as if the caller had returned.  Seen as the FASL cache
;;;;      failing to record any toplevel EVAL-WHEN (so any DEFPACKAGE).
;;;;
;;;;   node web/run-node.js --mvmw web/modus.mvmw --load test/web-mvm-calls.lisp --quit
;;;;
;;;; Prints WEB-MVM-CALLS PASS / FAIL.  The hot loops run past the translator's
;;;; threshold (20 calls) so both the interpreted and the translated paths run.

(defvar *wmc-bad* 0)
(defmacro wmc-check (name form want)
  `(let ((got (handler-case ,form (error (e) (list :error (princ-to-string e))))))
     (unless (equal got ,want)
       (incf *wmc-bad*)
       (format t "~&FAIL ~a: got ~s want ~s~%" ,name got ,want))))

;;; 1. positional and &rest calls past four arguments
(defun wmc-five (a b c d e) (list a b c d e))
(defun wmc-rest (&rest r) r)
(defun wmc-req2 (x y &rest r) (list x y r))
(wmc-check "five positional" (wmc-five 1 2 3 4 5) '(1 2 3 4 5))
(dotimes (n 41)
  (let ((args (loop for i below n collect i)))
    (wmc-check (format nil "&rest ~d" n) (apply #'wmc-rest args) args)
    (wmc-check (format nil "LIST ~d" n) (apply #'list args) args)
    (when (>= n 2)
      (wmc-check (format nil "req-2 &rest ~d" n) (apply #'wmc-req2 args) (list 0 1 (cddr args))))))
(dotimes (i 50)
  (wmc-check "hot &rest" (wmc-rest i 1 2 3 4 5 6) (list i 1 2 3 4 5 6)))

;;; 2. CATCH in a hot function, THROW from a callee that is still interpreted
;;; when the catcher is already translated (the callee is defined to be called
;;; less often than the catcher, so it crosses the threshold later)
(defun wmc-thrower (x) (when (eql x 3) (throw 'wmc-rej "rejected")) x)
(defun wmc-catcher (items) (catch 'wmc-rej (dolist (x items) (wmc-thrower x)) :none))
(defun wmc-catcher-hc (items)
  (catch 'wmc-rej
    (handler-case (progn (dolist (x items) (wmc-thrower x)) :none)
      (error (e) :caught))))
(defun wmc-closure-catcher (items)
  (catch 'wmc-rej
    (let ((k 0))
      (dolist (x items) (funcall (lambda (y) (when (eql y 3) (throw 'wmc-rej "rejected")) (setq k (+ k 1))) x))
      k)))
(let ((completed 0))
  (dotimes (i 60)
    (wmc-check "catch / callee throw" (wmc-catcher '(1 2 3 4)) "rejected")
    (wmc-check "catch > handler-case / callee throw" (wmc-catcher-hc '(1 2 3 4)) "rejected")
    (wmc-check "catch / closure throw" (wmc-closure-catcher '(1 2 3 4)) "rejected")
    (setq completed (+ completed 1)))
  ;; the bug ended THIS loop early, its caller's work silently skipped
  (wmc-check "caller loop ran to the end" completed 60))

(format t "~&WEB-MVM-CALLS ~a (~d failed)~%" (if (eql *wmc-bad* 0) "PASS" "FAIL") *wmc-bad*)
