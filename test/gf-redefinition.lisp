;;; test/gf-redefinition.lisp -- generic functions survive reloading and
;;; redefinition.  Expected values are SBCL's.
;;;
;;;   ./modus --script test/gf-redefinition.lisp
;;;
;;; Covers: a file of DEFCLASS / DEFGENERIC / DEFMETHOD loaded twice, callers
;;; compiled before each change, a #'NAME saved at the start, FMAKUNBOUND then
;;; a plain DEFUN, FMAKUNBOUND then DEFGENERIC again, (SETF FDEFINITION), and
;;; a lambda list of another arity.
;;;
;;; Two defects this guards:
;;; - REMHASH's O(1) removal reused the removed (key . value) pair for another
;;;   entry, and the JIT bridge / interpreter cache a symbol-function pair:
;;;   after FMAKUNBOUND a caller compiled earlier called an unrelated function
;;;   (SIGSEGV).
;;; - FMAKUNBOUND left NAME naming its generic function, so a later
;;;   DEFGENERIC revived the old one, methods of another arity included
;;;   (PROGRAM-ERROR), and a saved #'NAME saw the new methods.

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (what got want)
  (setq *checks* (+ *checks* 1))
  (unless (equal got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a: got ~s want ~s~%" what got want)))

(defvar *lib*
  (format nil "~a/gf-redef-lib-~d.lisp"
          (or (ignore-errors (sb-ext:posix-getenv "TMPDIR")) "/tmp")
          (get-universal-time)))
(with-open-file (o *lib* :direction :output :if-exists :supersede)
  (format o "(defclass shape () ())
(defclass sq (shape) ((s :initarg :s :accessor sq-s)))
(defgeneric area (x))
(defmethod area ((x sq)) (* (sq-s x) (sq-s x)))
(defmethod area ((x integer)) (* x 100))
"))

(load *lib*)
(defvar *s* (make-instance 'sq :s 3))
(defun use-area (o) (area o))
(defvar *saved* #'area)
(dotimes (i 100) (use-area *s*))
(check "first" (list (use-area *s*) (use-area 2) (funcall *saved* *s*)) '(9 200 9))
(load *lib*)
(check "reload" (list (use-area *s*) (use-area 2) (funcall *saved* *s*)) '(9 200 9))
(defmethod area ((x sq)) :redefined)
(check "redefined method" (list (use-area *s*) (funcall *saved* *s*)) '(:redefined :redefined))
(fmakunbound 'area)
(defun area (x) (list :plain x))
(check "plain defun" (list (use-area 5) (area 5)) '((:plain 5) (:plain 5)))
(fmakunbound 'area)
(check "after fmakunbound"
       (handler-case (use-area 5) (undefined-function () :undefined))
       :undefined)
(defgeneric area (x))
(defmethod area ((x integer)) (list :gf-again x))
(check "defgeneric again" (list (use-area 5) (area 6) (funcall *saved* 7))
       '((:gf-again 5) (:gf-again 6) 700))
(setf (fdefinition 'area) (lambda (x) (list :lambda x)))
(check "setf fdefinition" (use-area 8) '(:lambda 8))
(fmakunbound 'area)
(defgeneric area (x y))
(defmethod area ((x integer) y) (list :two x y))
(check "two args" (list (area 1 2) (handler-case (area 1) (program-error () :arity)))
       '((:two 1 2) :arity))
(fmakunbound 'area)
(load *lib*)
(check "reload after arity change" (list (area *s*) (area 3)) '(9 300))
(ignore-errors (delete-file *lib*))

(format t "~&gf-redefinition: ~d checks, ~d failed~%" *checks* *fails*)
(when (> *fails* 0) (sys-exit 1))
