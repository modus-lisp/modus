;;; test/cl-semantics-3.lisp -- control flow, restarts, types and printing
;;; fixed against the upstream ansi-test suite.  Expected values are SBCL's.
;;;
;;;   ./modus --script test/cl-semantics-3.lisp

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (what got want)
  (setq *checks* (+ *checks* 1))
  (unless (equalp got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a: got ~s want ~s~%" what got want)))

;;; Control flow
(check "go to an outer tagbody's tag"
       (let ((x 0)) (tagbody (tagbody (go a) (setq x 1)) a) x)
       0)
(check "dotimes count form inside the block"
       (block nil (dotimes (i (return 1))) 2)
       2)
(check "dotimes body is a tagbody"
       (let ((r nil)) (dotimes (i 4 (nreverse r)) (when (oddp i) (go skip)) (push i r) skip))
       '(0 2))
(check "psetf of symbol macros"
       (let ((a (vector 0 1 2 3 4 5)) (i 0))
         (symbol-macrolet ((x (aref a (incf i))) (y (aref a (incf i))))
           (psetf x (aref a (incf i)) y (aref a (incf i))))
         (list a i))
       '(#(0 2 2 4 4 5) 4))
(check "setf values evaluates the places first"
       (let ((a (vector nil nil)) (i 0) x y z)
         (setf (values (aref a (progn (setf x (incf i)) 0))
                       (aref a (progn (setf y (incf i)) 1)))
               (progn (setf z (incf i)) (values 'foo 'bar)))
         (list a x y z))
       '(#(foo bar) 1 2 3))
(check "setf subseq returns the new value"
       (let ((s (copy-seq "abcdefgh"))) (setf (subseq s 1 4) "xyz"))
       "xyz")
(check "loop collect returns one value"
       (multiple-value-list
        (flet ((%f (s) (read-from-string s)))
          (loop for s in '("1" "2") unless (eql (%f s) 1) collect s)))
       '(("2")))
(check "nineteen values through eval"
       (length (multiple-value-list (eval '(values 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))))
       19)

;;; Restarts
(check "restart-case associates restarts with a macro's error"
       (macrolet ((%m (&rest args) (cons 'error args)))
         (handler-bind ((error (lambda (c2) (invoke-restart (find-restart 'foo c2)))))
           (handler-bind ((error (lambda (c) (declare (ignore c)) (error "Blah"))))
             (restart-case (restart-case (%m "Boo") (foo () 'bad)) (foo () 'good)))))
       'good)
(check "with-condition-restarts"
       (let ((c1 (make-condition 'error)) (c2 (make-condition 'error)))
         (restart-case (with-condition-restarts c1 (list (first (compute-restarts)))
                         (abort c2))
           (abort () 'bad)
           (abort () 'good)))
       'good)
(check "ccase store-value"
       (handler-bind ((type-error (lambda (c) (store-value 7 c))))
         (let ((x 0)) (ccase x (1 :bad) (7 :good))))
       :good)

;;; Types and classes
(check "class-of a broadcast stream"
       (class-name (class-of (make-broadcast-stream)))
       'broadcast-stream)
(check "type-of a package, random state; a string stream's type"
       (list (type-of (find-package "CL")) (type-of (make-random-state))
             (subtypep (type-of (make-string-input-stream "a")) 'string-stream))
       '(package random-state t))
(check "consp of tagged objects"
       (list (consp (find-package "CL")) (consp (make-hash-table)) (consp *random-state*)
             (funcall #'consp *standard-output*) (listp *readtable*))
       '(nil nil nil nil nil))
(check "integer subtypes"
       (list (subtypep '(integer -10 10) 'fixnum) (subtypep 'integer 'fixnum)
             (subtypep '(unsigned-byte 8) 'fixnum) (subtypep 'integer 'signed-byte))
       '(t nil t t))
(check "complex subtypes"
       (list (subtypep '(complex integer) '(complex rational))
             (multiple-value-list (subtypep '(unsigned-byte 8) 'complex)))
       '(t (nil t)))
(check "typep simple-vector of a string" (typep "x" 'simple-vector) nil)
(check "functionp of a number" (list (functionp 1048576) (functionp 1.5)) '(nil nil))
(check "stringp of a 2-d character array"
       (stringp (make-array '(2 2) :element-type 'character :initial-element #\x))
       nil)

;;; Printing
(check "format justification pads the right gaps"
       (format nil "~15,,,'*<AA~4T~;BBBB~;CCCC~>")
       "AA  *BBBB**CCCC")
(check "struct printer keywords and print-length"
       (progn (defstruct cls3 foo)
              (list (let ((*print-length* 0)) (format nil "~a" (make-cls3 :foo 1)))
                    (format nil "~a" (make-cls3 :foo 1))))
       '("#S(CLS3 ...)" "#S(CLS3 :FOO 1)"))
(check "print-unreadable-object with :type"
       (with-output-to-string (s) (print-unreadable-object (nil s :type t)))
       "#<NULL >")
(check "uninterned colon symbol" (write-to-string (make-symbol ":") :readably t) "#:|:|")
(check "#:. reads" (symbol-name (read-from-string "#:.")) ".")

(format t "~&cl-semantics-3: ~d checks, ~d failed~%" *checks* *fails*)
(when (and (> *fails* 0) (fboundp (quote sys-exit))) (funcall (quote sys-exit) 1))
