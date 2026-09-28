;;;; test/setq-closure-escape.lisp -- a lambda SETQ'd into a global must be
;;;; callable from a later form.
;;;;   ./modus --script test/setq-closure-escape.lisp     ; exit 0 = PASS
;;;; A SETQ of a global whose cell is already known stores in place, with no
;;;; bridge call, and only the bridge call wraps an in-module lambda in a
;;;; native trampoline.  The raw #x52 closure kept this module's bytecode
;;;; offset in slot 0 (#x124 = offset 146), and a later form's FUNCALL jumped
;;;; to it: a BRK on aarch64, an error on x86-64.  Fixnums and NIL still take
;;;; the in-place store; the two counter checks keep that path honest.
(defvar *sce-fail* 0)
(defvar *sce-fn* nil)
(defvar *sce-bare* nil)
(defvar *sce-tbl* nil)
(defvar *sce-n* 0)
(defvar *sce-flag* t)
(defvar *sce-acc* nil)
(defun chk (name got want)
  (unless (equal got want) (setq *sce-fail* (+ *sce-fail* 1)))
  (format t "~&~a ~a got=~s~%" (if (equal got want) "ok  " "FAIL") name got))

(setq *sce-fn* (let ((k 7)) (lambda (x) (+ x k))))
(setq *sce-bare* (lambda (x) (* x 10)))
(let ((k 3)) (setq *sce-tbl* (list (lambda (x) (- x k)) (lambda (x) (+ x k)))))
(dotimes (i 1000) (setq *sce-n* (+ *sce-n* i)))
(setq *sce-flag* nil)
(dotimes (i 300) (push i *sce-acc*))

(chk "capturing closure" (funcall *sce-fn* 1) 8)
(chk "captureless lambda" (funcall *sce-bare* 4) 40)
(chk "closures inside a list" (mapcar (lambda (f) (funcall f 10)) *sce-tbl*) '(7 13))
(defun sce-call (f) (funcall (the function f) 2))
(chk "compiled caller" (sce-call *sce-fn*) 9)
(chk "fixnum counter" *sce-n* 499500)
(chk "flag set to NIL" *sce-flag* nil)
(chk "push onto a global" (length *sce-acc*) 300)
(format t "~&~a (~d failures)~%" (if (zerop *sce-fail*) "PASS" "FAIL") *sce-fail*)
(sys-exit (if (zerop *sce-fail*) 0 1))
