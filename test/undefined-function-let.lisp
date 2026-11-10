;;;; undefined-function-let.lisp — calling an undefined function inside a LET signals
;;;; UNDEFINED-FUNCTION, wherever the call was compiled and whichever thread makes it.
;;;;
;;;;   ./modus --script test/undefined-function-let.lisp
;;;;
;;;; Reported as a segfault while porting glass's REPL app.  Not reproduced on
;;;; e6f135b7 (+ run-program) or after: every shape below signals.  Kept as the guard.

(defvar *fail* 0)
(defun chk (name got want)
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(defmacro signals-uf (form)
  `(handler-case (progn ,form :no-error)
     (undefined-function (e) (list :undefined (string (cell-error-name e))))
     (error (e) (list :other-error (type-of e)))))

(defun on-worker (fn)
  (sb-thread:join-thread (sb-thread:make-thread fn)))

(defun in-defun () (let ((x 1)) (uf-let-a x)))
(defun in-defun-specials () (let ((*print-base* 10)) (uf-let-b 1)))
(defun in-defun-unwind () (let ((x (list 1))) (unwind-protect (uf-let-c x) (setf (car x) 2))))
(defun hot (n) (let ((x n)) (if (> x 1000) (uf-let-d x) x)))
(dotimes (i 1000) (hot i))

(chk "toplevel LET" (signals-uf (let ((x 1)) (uf-let-a x))) '(:undefined "UF-LET-A"))
(chk "in a DEFUN" (signals-uf (in-defun)) '(:undefined "UF-LET-A"))
(chk "LET of a special" (signals-uf (in-defun-specials)) '(:undefined "UF-LET-B"))
(chk "under UNWIND-PROTECT" (signals-uf (in-defun-unwind)) '(:undefined "UF-LET-C"))
(chk "a hot (native) function" (signals-uf (hot 5000)) '(:undefined "UF-LET-D"))
(chk "COMPILE" (signals-uf (funcall (compile nil '(lambda (y) (let ((x y)) (cons (uf-let-e x) 1)))) 3))
     '(:undefined "UF-LET-E"))
(chk "EVAL" (signals-uf (eval '(let ((x 1)) (uf-let-f x)))) '(:undefined "UF-LET-F"))
(chk "a DEFUN, on a worker" (on-worker (lambda () (signals-uf (in-defun)))) '(:undefined "UF-LET-A"))
(chk "a hot function, on a worker" (on-worker (lambda () (signals-uf (hot 5000)))) '(:undefined "UF-LET-D"))
(chk "EVAL on a worker" (on-worker (lambda () (signals-uf (eval (read-from-string "(let ((x 1)) (uf-let-g x))")))))
     '(:undefined "UF-LET-G"))

(format t "~%UNDEFINED-FUNCTION-LET: ~:[FAIL~;PASS~]~%" (zerop *fail*))
