;;;; test/limitations.lisp -- does each CLAUDE.md "Active Limitation" still hold?
;;;;
;;;; Two ways, because most of them were about the BUILD-TIME compiler:
;;;;   baked:    MODUS_EXTRA_SOURCE=test/limitations.lisp  (build-generic-cli)
;;;;             ./modus --eval '(progn (lim-report) (sys-exit 0))'
;;;;   runtime:  ./modus --script test/limitations.lisp   (runs LIM-REPORT at the end
;;;;             when *LIM-AUTORUN* is bound, i.e. under --script)
;;;; Each probe: (name got want); a row is OK when GOT is EQUAL to WANT.

;; #1 last-defun-wins: a function object taken BEFORE a redefinition is the OLD
;; one in CL.
(defun lim1-f () 1)
(defvar *lim1-old* #'lim1-f)
(defun lim1-f () 2)
(defun lim1 () (list (funcall *lim1-old*) (lim1-f)))

;; #2 variable-index ASET as a non-last form.
(defun lim2 (i) (let ((a (make-array 5 :initial-element 0))) (aset a i 7) (aref a i)))
(defun lim2b (i) (let ((a (make-array 5 :initial-element 0))) (setf (aref a i) 7) (aref a i)))

;; #7 defvar initforms run at load; a NIL initform binds to NIL (not unbound).
(defvar *lim7* 42)
(defvar *lim7-nil* nil)
(defun lim7 () (list *lim7* (boundp '*lim7-nil*) *lim7-nil*))

;; #8 constant small-count ASH of a BIGNUM (right shift).
(defun lim8 (x) (list (ash x -1) (ash x -200)))
;; #8a constant small-count LEFT shift of a big fixnum must promote.
(defun lim8a (x) (ash x 1))
;; #8b VARIABLE-count ASH of a bignum, left.
(defun lim8b (x n) (ash x n))
;; #8c checked * and + overflowing from NEGATIVE fixnums.
(defun lim8c (x) (list (* x 2) (+ x x) (- x 1)))

;; implicit globals: an undeclared free variable read/set at toplevel.
(defun limg () (setq *lim-undeclared* 5) *lim-undeclared*)

(defun lim-row (name got want)
  (format t "~&~:[FAIL~;ok  ~] ~16A got ~S~:[  want ~S~;~*~]~%"
          (equal got want) name got (equal got want) want))

(defun lim-report ()
  (let ((mpf most-positive-fixnum) (mnf most-negative-fixnum) (big (ash 1 200)))
    (lim-row "#1 old fn" (lim1) '(1 2))
    (lim-row "#2 aset var idx" (lim2 3) 7)
    (lim-row "#2 setf aref" (lim2b 3) 7)
    (lim-row "#7 defvar init" (lim7) '(42 t nil))
    (lim-row "#8 ash big -k" (lim8 big) (list (expt 2 199) 1))
    (lim-row "#8a ash fix 1" (lim8a mpf) (* 2 mpf))
    (lim-row "#8b ash var n" (lim8b 3 100) (* 3 (expt 2 100)))
    (lim-row "#8b ash big var" (lim8b big 5) (expt 2 205))
    (lim-row "#8c neg overflow" (lim8c mnf) (list (* 2 mnf) (* 2 mnf) (- mnf 1)))
    (lim-row "implicit global" (limg) 5)
    t))

(when (boundp '*lim-autorun*) (lim-report))
