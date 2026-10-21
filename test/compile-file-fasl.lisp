;;;; test/compile-file-fasl.lisp -- COMPILE-FILE writes a COMPILED fasl and LOAD
;;;; replays it (mvm/fasl.lisp, "COMPILE-FILE's output").
;;;;
;;;;   ./modus --script test/compile-file-fasl.lisp
;;;;
;;;; COMPILE-FILE used to copy the source text into the .fasl, so every LOAD
;;;; compiled the file again, and the FASL cache could not serve a second load in
;;;; one session (its key chains every file loaded before).  cl-bench LOAD-FASL --
;;;; twenty loads of one compiled file -- was 222x SBCL.  The checks: CLHS
;;;; 3.2.2.2/3.2.3.1 semantics (compiling defines nothing; a DEFMACRO and an
;;;; IN-PACKAGE take effect for the rest of the file), the replayed code is
;;;; right on a first and a repeated load, a function reading a special is a
;;;; module (its per-site cache cons was a "shared constant"), and the file IS a
;;;; compiled fasl, not source.

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (setq *checks* (+ *checks* 1))
  (unless ok (setq *fails* (+ *fails* 1)))
  (format t "~a ~a~%" (if ok "ok  " "FAIL") name))

(defvar *dir* (or (ignore-errors (sb-ext:posix-getenv "TMPDIR")) "/tmp"))
(defvar *src* (format nil "~a/modus-cf-test-~d.lisp" *dir* (random 1000000)))

(with-open-file (o *src* :direction :output :if-exists :supersede)
  (write-string "(defpackage :cf-test (:use :cl))
(in-package :cf-test)
(defvar *count* 0)
(defvar *level* 1)
(defmacro twice (x) `(progn ,x ,x))
(defun bump () (twice (incf *count*)) *count*)
(defun level () *level*)
(defun rebound () (let ((*level* 7)) (level)))
(defun shared () (let ((a '(1 2))) (eq a '(1 2))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun helper (n) (* n 10)))
(defmacro via-helper (n) (helper n))
(defun uses-helper () (via-helper 4))
(defstruct pt x y)
(defun pt-sum (p) (+ (pt-x p) (pt-y p)))
(setq *count* 100)
" o))

(defvar *fasl* (compile-file *src*))
(check "compile-file returns the output truename" (pathnamep *fasl*))
(check "compiling defines nothing: BUMP is not fbound"
       (not (fboundp (find-symbol "BUMP" "CF-TEST"))))
(check "the fasl is compiled, not source"
       (with-open-file (i *fasl* :element-type '(unsigned-byte 8))
         (let ((b (make-array 13 :element-type '(unsigned-byte 8))))
           (read-sequence b i)
           (equal (map 'string #'code-char b) ";MODUS-FASL 1"))))

(dolist (round '(1 2 3))
  (load *fasl*)
  (let ((sym (lambda (n) (find-symbol n "CF-TEST"))))
    (check (format nil "load ~d: toplevel SETQ ran (count 100)" round)
           (eql (symbol-value (funcall sym "*COUNT*")) 100))
    (check (format nil "load ~d: macro used later in the file (BUMP -> 102)" round)
           (eql (funcall (funcall sym "BUMP")) 102))
    (check (format nil "load ~d: a special read by a compiled module" round)
           (eql (funcall (funcall sym "LEVEL")) 1))
    (check (format nil "load ~d: LET of the special binds dynamically" round)
           (eql (funcall (funcall sym "REBOUND")) 7))
    (check (format nil "load ~d: EVAL-WHEN helper used by a later macro" round)
           (eql (funcall (funcall sym "USES-HELPER")) 40))
    (check (format nil "load ~d: DEFSTRUCT accessors" round)
           (eql (funcall (funcall sym "PT-SUM")
                         (funcall (funcall sym "MAKE-PT") :x 3 :y 4))
                7))
    (check (format nil "load ~d: in-package kept *package* for the caller" round)
           (string= (package-name *package*) "COMMON-LISP-USER"))))

(ignore-errors (delete-file *fasl*))
(ignore-errors (delete-file *src*))
(format t "~&compile-file-fasl: ~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
