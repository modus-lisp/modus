;;;; test/redefine-kernel-callers.lisp -- a runtime redefinition of a function
;;;; compiled into the IMAGE reaches the image's own callers, which call it by a
;;;; direct BL / CALL (mvm-eval.lisp, REDEFINING A KERNEL FUNCTION).
;;;;   modus --script test/redefine-kernel-callers.lisp   => REDEFINE-KERNEL PASS
;;;; STRING-UPCASE (kernel) calls LOWER-CASE-P (kernel).  The redefinitions only
;;;; misreport #\q / #\a, so the reader and printer keep working meanwhile.
;;;; Part 4 is the &REST case: LOAD (kernel) calls CLOSE (kernel, &rest) with
;;;; its rest list PRE-PACKED by the caller, a convention only the callee knows.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(check "before" (string-upcase "aqb") "AQB")
(defvar *orig* (fdefinition 'lower-case-p))
(defvar *calls* 0)
;; 1. advice: the kernel caller sees it, and the OLD object still runs the old code
(setf (fdefinition 'lower-case-p)
      (lambda (c) (setq *calls* (+ *calls* 1)) (if (char= c #\q) nil (funcall *orig* c))))
(check "setf fdefinition reaches the kernel caller" (string-upcase "aqb") "AqB")
(check "the old object runs the original" (funcall *orig* #\q) t)
(check "the advice ran" (> *calls* 0) t)
;; 2. DEFUN: interpreted first, native once hot
(defun lower-case-p (c) (if (char= c #\a) nil (funcall *orig* c)))
(check "defun reaches the kernel caller" (string-upcase "aqb") "aQB")
(dotimes (i 300) (string-upcase "aqb"))
(check "and still after the hot JIT" (string-upcase "aqb") "aQB")
;; 3. the original object back
(setf (fdefinition 'lower-case-p) *orig*)
(check "restored" (string-upcase "aqb") "AQB")
(check "restored, called directly" (lower-case-p #\q) t)
;; 4. a kernel function with &rest, which kernel callers call PRE-PACKED
(defvar *close-orig* (fdefinition 'close))
(defvar *closes* 0)
(defvar *rk-file* "/tmp/modus-redefine-kernel-callers.lisp")
(with-open-file (o *rk-file* :direction :output :if-exists :supersede)
  (format o "(defvar *rk-loaded* 42)~%"))
(setf (fdefinition 'close)
      (lambda (stream &rest args) (setq *closes* (+ *closes* 1)) (apply *close-orig* stream args)))
(check "LOAD through an advised CLOSE" (progn (load *rk-file*) *rk-loaded*) 42)
(check "the kernel's pre-packed call reached the advice" (> *closes* 0) t)
(setf (fdefinition 'close) *close-orig*)
(setq *closes* 0)
(load *rk-file*)
(check "restored CLOSE" *closes* 0)
(format t "REDEFINE-KERNEL ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
