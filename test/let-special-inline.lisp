;;;; test/let-special-inline.lisp -- a LET of specials binds through the cell inline.
;;;;   modus --script test/let-special-inline.lisp   => LET-SPECIAL-INLINE PASS
;;;;
;;;; A LET of a special tests the thread gate once and saves, binds and restores each variable
;;;; with a CDR / SET-CDR of its cell (%COMPILE-LET-SPECIALS-FAST), instead of three out-of-line
;;;; calls per variable.  These are the cases that must still hold: values seen by callees,
;;;; restores on every exit, unbound variables staying unbound, LET* order, multiple values
;;;; through the binding, and variables with no value when the code was compiled.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defvar *a* 1) (defvar *b* 2) (defvar *c* 3)
(defvar *u*)                                  ; never given a value
(defun abc () (list *a* *b* *c*))
(defun u-bound-p () (boundp '*u*))

(defun bind3 () (let ((*a* 10) (*b* 20) (*c* 30)) (abc)))
(defun swap () (let ((*a* *b*) (*b* *a*)) (abc)))
(defun seq () (let* ((*a* 5) (*b* (+ *a* 1)) (*c* (abc))) (list *a* *b* *c*)))
(defun nested () (let ((*a* :o)) (list (let ((*a* :i) (*b* :i)) (abc)) (abc))))
(defun throw-out () (catch 'k (let ((*a* :t) (*b* :t)) (throw 'k (abc)))))
(defun return-out () (block b (let ((*a* :r)) (return-from b (abc)))))
(defun go-out () (let ((n 0)) (tagbody top (let ((*c* n)) (incf n) (when (< n 3) (go top)))) n))
(defun error-out () (handler-case (let ((*b* :e)) (error "x")) (error () (abc))))
(defun mv () (multiple-value-list (let ((*a* 7)) (values *a* 8 9))))
(defun bind-unbound () (let ((*u* :now)) (list *u* (u-bound-p))))
(defun setq-inside () (let ((*a* 1)) (setq *a* 99) (abc)))
(defun empty () (let ((*a* 4))))
(defun loop-bind (n) (let ((s 0)) (dotimes (i n) (let ((*a* i)) (setq s (+ s (car (abc)))))) s))

(check "bind3" (bind3) '(10 20 30))
(check "after-bind3" (abc) '(1 2 3))
(check "swap" (swap) '(2 1 3))
(check "seq" (seq) '(5 6 (5 6 3)))
(check "nested" (nested) '((:i :i 3) (:o 2 3)))
(check "throw" (throw-out) '(:t :t 3))
(check "after-throw" (abc) '(1 2 3))
(check "return" (return-out) '(:r 2 3))
(check "go" (go-out) 3)
(check "error" (error-out) '(1 2 3))
(check "mv" (mv) '(7 8 9))
(check "unbound-inside" (bind-unbound) '(:now t))
(check "unbound-after" (u-bound-p) nil)
(check "setq-inside" (setq-inside) '(99 2 3))
(check "after-setq" (abc) '(1 2 3))
(check "empty" (empty) nil)
(check "loop" (loop-bind 100) 4950)
(check "after-all" (abc) '(1 2 3))

(format t "~&LET-SPECIAL-INLINE ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
