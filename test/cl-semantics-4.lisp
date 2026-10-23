;;; test/cl-semantics-4.lisp -- characters, complex hyperbolics, macro
;;; environments, string comparison, printer and type fixes made against
;;; the upstream ansi-test suite.  Expected values are SBCL's.
;;;
;;;   ./modus --script test/cl-semantics-4.lisp

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (what got want)
  (setq *checks* (+ *checks* 1))
  (unless (equalp got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a: got ~s want ~s~%" what got want)))

;;; Characters
(check "non-graphic characters have names"
       (list (char-name (code-char 1)) (char-name (code-char 7))
             (graphic-char-p (code-char 7)) (graphic-char-p (code-char 200)))
       '("Soh" "Bel" nil t))
(check "~:C spells the name" (format nil "~:c" (code-char 7)) "Bel")
(check "standard-char is the 96" (list (typep #\a 'standard-char) (typep (code-char 127) 'standard-char))
       '(t nil))

;;; Complex hyperbolics keep the argument's float format
(check "cosh of a complex"
       (typep (cosh (complex 1.0 2.0)) '(complex single-float))
       t)
(check "acosh of complex one"
       (equal (acosh (coerce 1 '(complex single-float))) (coerce 0 '(complex single-float)))
       t)
(check "coerce to (complex double-float)" (coerce 1 '(complex double-float)) #c(1d0 0d0))
(check "least-positive-single-float is single"
       (typep least-positive-single-float 'single-float) t)

;;; Macro environments
(check "macroexpand sees the enclosing symbol-macrolet"
       (symbol-macrolet ((a b))
         (macrolet ((foo (x &environment env)
                      (if (eq (macroexpand x env) 'a) 1 2)))
           (foo a)))
       2)

;;; Strings
(check "string<= on equal slices answers END1"
       (list (string<= "xabc" "abc" :start1 1) (string>= "xabc" "yabc" :start1 1 :start2 1))
       '(4 4))
(check "any single escape works inside a string"
       (let ((*readtable* (copy-readtable nil)))
         (set-syntax-from-char #\z #\\)
         (read-from-string "\"az\"\""))
       "a\"")

;;; Printer
(check "with-standard-io-syntax restores the pprint dispatch table"
       (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
         (set-pprint-dispatch 'symbol (lambda (s o) (declare (ignore o)) (write-string "FOO" s)))
         (list (let ((*print-pretty* t)) (princ-to-string 'bar))
               (with-standard-io-syntax (let ((*print-pretty* t)) (princ-to-string 'bar)))))
       '("FOO" "BAR"))
(defvar *cs4-params* nil)
(defun cs4-slash (stream arg colon at &rest params)
  (declare (ignore stream arg colon at))
  (setq *cs4-params* params))
(check "~/ passes character and every parameter"
       (progn (format nil "~'X,2,3,4,5,6/cs4-slash/" 1) *cs4-params*)
       '(#\X 2 3 4 5 6))
(check "pprint-fill of a circular list"
       (let ((*print-pretty* t) (*print-circle* t) (*print-right-margin* 100))
         (with-output-to-string (s)
           (pprint-fill s (let ((x (list 'a))) (setf (cdr x) x) x))))
       "#1=(A . #1#)")

;;; Types
(check "type-of a positive bignum is an unsigned-byte type"
       (list (subtypep (type-of (expt 2 70)) 'unsigned-byte)
             (subtypep (type-of (expt 2 70)) 'bignum))
       '(t t))
(check "a fill-pointered vector is not a simple-array"
       (typep (make-array 3 :fill-pointer 2) 'simple-array)
       nil)

;;; Batch 6
(check "DEFUN returns its name in any context"
       (list (prog1 (defun cs4-df () 1) 2) (list (defun cs4-df2 () 2)))
       '(cs4-df (cs4-df2)))
(check "&whole may be a pattern"
       (destructuring-bind (&whole (a . b) c . d) '(1 . 2) (list a b c d))
       '(1 2 1 2))
(check "keyword case is preserved"
       (list (eq (intern "qwerty" "KEYWORD") :qwerty) (symbol-name :qwerty)
             (symbol-name (intern "qwerty" "KEYWORD")))
       '(nil "QWERTY" "qwerty"))

;;; Batch 9
(check "#'F of a local function is one object"
       (list (labels ((f () 'foo)) (eq #'f #'f))
             (labels ((f () #'f)) (eq (f) #'f)))
       '(t t))
(defun cs4-acc (x) (car x))
(let ((z 'car))
  (defsetf cs4-acc (y) (val) `(setf (,z ,y) ,val)))
(check "a long DEFSETF sees its LET"
       (let ((x (cons 'a 'b))) (list (setf (cs4-acc x) 'c) x))
       '(c (c . b)))
(let ((n 7))
  (defstruct cs4-st (a n)))
(check "a DEFSTRUCT initform sees its LET" (cs4-st-a (make-cs4-st)) 7)
(check "zero values through a function defined in the same form"
       (progn (eval '(defun cs4-zv () (values))) (multiple-value-list (cs4-zv)))
       nil)
(check "readable printing uses the standard readtable case"
       (let ((*readtable* (copy-readtable nil)))
         (setf (readtable-case *readtable*) :invert)
         (list (let ((*print-case* :capitalize)) (prin1-to-string '(nil t)))
               (let ((*print-case* :capitalize) (*print-readably* t))
                 (prin1-to-string '(nil t)))))
       '("(nil t)" "(Nil T)"))

;;; Batch 10
(check "| inside a |...| symbol name is escaped"
       (prin1-to-string (intern "x|y" "CL-USER")) "|x\\|y|")
(check "* in a namestring is :WILD"
       (list (pathname-name #p"*") (pathname-directory #p"/a/*/**/b.c"))
       '(:wild (:absolute "a" :wild :wild-inferiors)))
(let ((a 1))
  (deftype cs4-upto (&optional (x a)) (declare (special a)) `(integer 0 ,x)))
(check "a DEFTYPE default sees its LET, not a special"
       (let ((a 2)) (declare (special a)) (list (typep 1 'cs4-upto) (typep 2 'cs4-upto)))
       '(t nil))
(check "#n= only when the second reference is printed"
       (let ((*print-pretty* t) (*print-circle* t) (*print-length* 2) (*print-right-margin* 100))
         (with-output-to-string (os)
           (let* ((tail (list 1)) (x (list* tail 2 tail)))
             (pprint-logical-block (os x :prefix "<" :suffix ">")
               (pprint-exit-if-list-exhausted)
               (write (pprint-pop) :stream os)
               (loop (pprint-exit-if-list-exhausted)
                     (write-char #\Space os)
                     (write (pprint-pop) :stream os))))))
       "<(1) 2 ...>")

(format t "~&cl-semantics-4: ~d checks, ~d failed~%" *checks* *fails*)
(when (and (> *fails* 0) (fboundp (quote sys-exit))) (funcall (quote sys-exit) 1))
