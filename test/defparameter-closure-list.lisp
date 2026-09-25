;;;; test/defparameter-closure-list.lisp -- a lambda stored INSIDE a structure
;;;; by DEFPARAMETER / DEFVAR must be callable from a later form.
;;;;   ./modus --script test/defparameter-closure-list.lisp     ; exit 0 = PASS
;;;; Through the compiled %GV-SET the closure inside the list stayed a raw
;;;; bytecode offset, and FUNCALL jumped to it (upstream BOOLE.1 faulted at
;;;; RIP 0x4E9).  %GV-SET-DEF is a storage sink the interpreter deep-wraps.
(defparameter *dcl-fs* (list #'(lambda (x y) (declare (ignore y)) x) #'logand (constantly 0)))
(defvar *dcl-v* (list (lambda (x) (* x 10))))
(defvar *dcl-fail* 0)
(defun chk (name got want)
  (unless (equal got want) (setq *dcl-fail* (+ *dcl-fail* 1)))
  (format t "~&~a ~a got=~s~%" (if (equal got want) "ok  " "FAIL") name got))
(chk "defparameter-list" (mapcar (lambda (f) (funcall f 5 3)) *dcl-fs*) '(5 1 0))
(chk "defvar-list" (funcall (car *dcl-v*) 4) 40)
(defun dcl-call (f) (funcall (the function f) 7 8))
(chk "compiled-caller" (dcl-call (first *dcl-fs*)) 7)
(format t "~&~a (~d failures)~%" (if (zerop *dcl-fail*) "PASS" "FAIL") *dcl-fail*)
(sys-exit (if (zerop *dcl-fail*) 0 1))
