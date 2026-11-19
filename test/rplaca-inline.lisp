;;;; test/rplaca-inline.lisp -- a two-argument RPLACA / RPLACD (and so every
;;;; SETF of CAR / CDR, and every assignment to a variable a closure
;;;; captures) is a store, not a call that conses.
;;;;
;;;;   ./modus --script test/rplaca-inline.lisp
;;;;
;;;; The functions take &REST to report a wrong argument count, so each call
;;;; consed its argument list: 16 bytes per (setf (car c) x), and per (incf i)
;;;; of an I that an FLET closes over -- json-simple's UTF-8 decoder allocated
;;;; 16 bytes per character for it.  The compiler now inlines the two-argument
;;;; form; the semantics (value, TYPE-ERROR, PROGRAM-ERROR) are the control.

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (incf *pass*)
      (progn (incf *fail*) (format t "~&FAIL ~A: got ~S want ~S~%" name got want))))
(defmacro outcome (form)
  `(handler-case (list :value ,form)
     (type-error () :type-error)
     (program-error () :program-error)
     (error (c) (list :other (type-of c)))))

(check "rplaca value" (let ((c (list 1 2))) (list (eq (rplaca c 9) c) c)) '(t (9 2)))
(check "rplacd value" (let ((c (list 1 2))) (list (eq (rplacd c 9) c) c)) '(t (1 . 9)))
(check "rplaca non-cons" (outcome (rplaca 5 1)) :type-error)
(check "rplacd nil" (outcome (rplacd nil 1)) :type-error)
(check "funcall rplaca" (funcall #'rplaca (list 1) 2) '(2))
(check "apply rplaca 1 arg" (outcome (apply #'rplaca (list (list 1)))) :program-error)
(check "apply rplacd 3 args" (outcome (apply #'rplacd (list (list 1) 2 3))) :program-error)
(check "compiled non-cons" (outcome (funcall (compile nil '(lambda (x) (rplaca x 3))) 7)) :type-error)
(check "setf car" (funcall (compile nil '(lambda (x) (let ((c (list 0 1))) (setf (car c) x (cdr c) x) c))) 7) '(7 . 7))
(check "order: cell before value"
       (let ((log nil)) (rplaca (progn (push :c log) (list 0)) (progn (push :v log) 1)) (nreverse log)) '(:c :v))

(defun consed (thunk)
  (let ((b (sb-ext:get-bytes-consed))) (funcall thunk) (- (sb-ext:get-bytes-consed) b)))
(defun stores (c) (dotimes (k 10000) (rplaca c k) (setf (cdr c) k)) c)
(defun captured () (let ((i 0)) (flet ((f () i)) (dotimes (k 10000) (incf i)) (f))))
(stores (list 0)) (captured)
(check "10000 rplaca+setf cdr cons nothing" (< (consed (lambda () (stores (list 0)))) 4096) t)
(check "10000 incf of a captured variable cons nothing" (< (consed #'captured) 4096) t)
(check "captured value" (captured) 10000)

(format t "~&rplaca-inline: ~D passed, ~D failed~%" *pass* *fail*)
(format t "~&~:[FAIL~;PASS~]~%" (zerop *fail*))
