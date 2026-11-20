;;;; test/bignum-literal-compare.lisp -- a numeric comparison against an
;;;; integer literal past the fixnum range answers right in compiled code.
;;;;
;;;;   ./modus --script test/bignum-literal-compare.lisp
;;;;
;;;; COMPILE-COMPARE-2 takes a "leaf" right operand straight into a temp with
;;;; no push/pop of the left one.  %LEAF-OPERAND-P called EVERY integer
;;;; literal a leaf, but a bignum literal is consed at run time (:gc-check +
;;;; :alloc-obj), and runtime x64 code writes that through rax = VR.  With
;;;; DEST = VR (the compare is the function's value) the left operand was
;;;; overwritten before the compare: (= x #xa129ca6149be45e5) answered NIL for
;;;; x = that value, which failed beacon's SipHash reference vector.  Literal
;;;; on the LEFT, a quoted literal, or an interpreted top-level DEFUN were
;;;; fine, which is the control half below.
;;;;
;;;; Every form goes through COMPILE NIL, a DEFUN compiled by LOAD of a source
;;;; file, the tarball loader (%IT-EVAL-SOURCE) and ASDF:LOAD-SYSTEM,
;;;; and each answer is checked against the functional operator via APPLY.

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name got want)
  (if (eql got want)
      (incf *pass*)
      (progn (incf *fail*) (format t "~&FAIL ~A: got ~S want ~S~%" name got want))))

(defparameter *literals*
  (list 0 1 -1 1000 -1000
        most-positive-fixnum most-negative-fixnum
        (1+ most-positive-fixnum) (1- most-negative-fixnum)
        (expt 2 62) (- (expt 2 62)) (expt 2 63) (- (expt 2 63))
        #xa129ca6149be45e5 (- #xa129ca6149be45e5)
        (expt 2 64) (expt 2 100) (- (expt 2 100)) (1+ (expt 2 127))))

(defparameter *ops* '(= /= < > <= >=))

(defun probes (lit)
  "Run-time values to compare against LIT: equal, one either side, and far."
  (list lit (1+ lit) (1- lit) 0 7 -7 (expt 2 200) (- (expt 2 200))))

(defun lambda-forms ()
  "(op lambda-form literal side) for every op x literal x side."
  (let ((out '()))
    (dolist (op *ops* (nreverse out))
      (dolist (lit *literals*)
        (push (list op `(lambda (x) (,op x ,lit)) lit :right) out)
        (push (list op `(lambda (x) (,op ,lit x)) lit :left) out)
        ;; the compare as an IF test (the fused-branch path)
        (push (list op `(lambda (x) (if (,op x ,lit) :yes :no)) lit :if-right) out)
        (push (list op `(lambda (x) (if (,op ,lit x) :yes :no)) lit :if-left) out)))))

(defun want (op lit side x)
  (let ((v (if (member side '(:right :if-right))
               (funcall op x lit)
               (funcall op lit x))))
    (if (member side '(:if-right :if-left)) (if v :yes :no) v)))

(defun check-fn (via fn op lit side)
  (dolist (x (probes lit))
    (check (format nil "~A ~A ~A ~A x=~A" via op side lit x)
           (funcall fn x) (want op lit side x))))

;;; 1. COMPILE NIL
(dolist (e (lambda-forms))
  (destructuring-bind (op form lit side) e
    (check-fn "compile" (compile nil form) op lit side)))

;;; 2/3. DEFUNs from source text: LOAD of a file, and %IT-EVAL-SOURCE (the
;;; loader install-tarball and ASDF use, present after (require :asdf)).
(defun defun-source (prefix)
  (with-output-to-string (s)
    (let ((*print-base* 10) (i 0))
      (dolist (e (lambda-forms))
        (destructuring-bind (op form lit side) e
          (declare (ignore op lit side))
          (format s "(defun ~A-~D ~S ~S)~%" prefix i (second form) (third form))
          (incf i))))))

(defun check-defuns (via prefix)
  (let ((i 0))
    (dolist (e (lambda-forms))
      (destructuring-bind (op form lit side) e
        (declare (ignore form))
        (let ((name (find-symbol (format nil "~A-~D" prefix i))))
          (if (and name (fboundp name))
              (check-fn via (symbol-function name) op lit side)
              (check (format nil "~A ~A-~D defined" via prefix i) nil t)))
        (incf i)))))

(let ((path "/tmp/modus-bignum-literal-compare.lisp"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string (defun-source "BLC-LOAD") s))
  (load path)
  (delete-file path)
  (check-defuns "load" "BLC-LOAD"))

(require :asdf)
(let ((it (find-symbol "%IT-EVAL-SOURCE" "MODUS-INT")))
  (if (and it (fboundp it))
      (progn (funcall it (defun-source "BLC-IT") "bignum-literal-compare")
             (check-defuns "it-eval-source" "BLC-IT"))
      (check "%IT-EVAL-SOURCE present after (require :asdf)" nil t)))

;;; 4. A real ASDF system.
(let* ((dir "/tmp/modus-blc-system/")
       (asd (concatenate 'string dir "blc-sys.asd"))
       (src (concatenate 'string dir "blc.lisp")))
  (ensure-directories-exist dir)
  (with-open-file (s asd :direction :output :if-exists :supersede)
    (write-string "(asdf:defsystem \"blc-sys\" :components ((:file \"blc\")))" s))
  (with-open-file (s src :direction :output :if-exists :supersede)
    (write-string "(in-package :cl-user)" s) (terpri s)
    (write-string (defun-source "BLC-ASDF") s))
  (push (pathname dir) (symbol-value (find-symbol "*CENTRAL-REGISTRY*" "ASDF")))
  (funcall (find-symbol "LOAD-SYSTEM" "ASDF") "blc-sys")
  (check-defuns "asdf" "BLC-ASDF")
  (ignore-errors (delete-file asd))
  (ignore-errors (delete-file src)))

;;; The original report, verbatim.
(check "siphash literal"
       (funcall (compile nil '(lambda (x) (= x #xa129ca6149be45e5))) #xa129ca6149be45e5)
       t)

(format t "~&bignum-literal-compare: ~D passed, ~D failed~%" *pass* *fail*)
(format t "~&~:[FAIL~;PASS~]~%" (zerop *fail*))
