;;;; test/flet-default-names-outer.lisp -- a FLET/LABELS parameter whose default
;;;; names the SAME-NAMED outer variable.
;;;;
;;;;   ./modus --script test/flet-default-names-outer.lisp
;;;;
;;;; (flet ((hp (&key (secret secret)) ...)) ...) defaults SECRET to the outer
;;;; SECRET: a parameter is not in scope in its own default (CLHS 3.4.1).  The
;;;; capture scan excluded every parameter name from the defaults, so it saw no
;;;; capture, compiled HP as a global function, and the default read NIL.
;;;; cl-payments' rail gate hit it (a payment secret of NIL).  Fails before the
;;;; fix (NIL where 42 / 7 are wanted), passes after.

(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok    ~a~%" name)
      (progn (incf *fails*) (format t "FAIL  ~a: got ~s want ~s~%" name got want))))

(defun f-key (secret) (flet ((hp (&key (secret secret) total) (list secret total))) (list (hp) (hp :secret 9))))
(defun f-opt (x) (flet ((g (&optional (x x)) x)) (g)))
(defun f-labels (x) (labels ((g (&optional (x x)) x)) (g)))
(defun f-earlier (a) (flet ((g (a &optional (b a)) (list a b))) (g 3)))   ; B's default is G's own A
(defun f-supplied (s) (flet ((g (&key (s s s-p)) (list s s-p))) (list (g) (g :s 1))))

(check "&key default names the outer variable" (f-key 42) '((42 nil) (9 nil)))
(check "&optional default names the outer variable" (f-opt 7) 7)
(check "labels, same" (f-labels 8) 8)
(check "a default naming an EARLIER parameter still means that parameter" (f-earlier 100) '(3 3))
(check "supplied-p still works" (f-supplied 5) '((5 nil) (1 t)))

(format t "~&flet-default-names-outer: ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
