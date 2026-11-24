;;; A runtime DEFUN's function object taken BEFORE it goes native (stored in a
;;; plist, a variable) must keep working after: its trampoline forwards to the
;;; native code through %JIT-SET-FORWARDS's box.  On the web the box held an
;;; x64-encoded word, so calling the old object ran garbage (cl-bench DDERIV:
;;; "The value 0 is not of type LIST", MVM FAULT unknown opcode).
;;;   node web/run-node.js --mvmw IMG --load test/web-jit-forward.lisp
;;; Hot promotion off (read when a DEFUN is installed), so NA goes native
;;; exactly at the %JIT-HOT-BUILD below, after *OLD* is taken.
(setq *jit-hot-calls* 0)
(defvar *fails* 0)
(defmacro check (name form want)
  `(let ((got (handler-case ,form (error (e) (list :error (princ-to-string e))))))
     (if (equal got ,want) (format t "~&ok   ~a~%" ,name)
         (progn (incf *fails*) (format t "~&FAIL ~a: ~s, want ~s~%" ,name got ,want)))))
(defun na (x) (if (consp x) (cons :a (mapcar #'ib x)) x))
(defun ib (x) (if (consp x) (funcall (get 'k 'f) x) (list :b x)))
(setf (get 'k 'f) #'na)
(defvar *old* #'na)
(defvar *want* (na '(1 (2 (3)))))
(modus-int::%jit-hot-build "NA" (modus-int::%jit-skip-list) (cons nil 1))
(check "na went native" (not (eq *old* #'na)) t)
(check "old object, flat" (funcall *old* '(1 2)) '(:a (:b 1) (:b 2)))
(check "native, re-entered through the old object" (na '(1 (2 (3)))) *want*)
(check "plist object" (funcall (get 'k 'f) '((1))) '(:a (:a (:b 1))))
(format t "~&WEB-JIT-FORWARD ~a (~d failed)~%" (if (zerop *fails*) "PASS" "FAIL") *fails*)
