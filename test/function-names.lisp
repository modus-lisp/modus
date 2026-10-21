;;;; test/function-names.lisp -- a function says what it is called.
;;;;
;;;;   ./modus --script test/function-names.lisp
;;;;
;;;; Every function printed as #<?0> or #<?CLOSURE>, FUNCTION-LAMBDA-EXPRESSION's
;;;; third value was always NIL and SB-KERNEL:%FUN-NAME did not exist -- so a
;;;; test framework, a documentation tool or a backtrace could not name a
;;;; function.  SBCL's answers are the reference (run this under SBCL to see
;;;; them); modus's for anonymous functions are (LAMBDA :IN OWNER) where SBCL
;;;; adds the lambda list, and LABELS reports as FLET.

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok &optional got)
  (setq *checks* (+ *checks* 1))
  (unless ok (setq *fails* (+ *fails* 1)))
  (format t "~a ~a~@[  got ~s~]~%" (if ok "ok  " "FAIL") name (unless ok got)))

(defun named-fn (x) x)
(defgeneric gf (x))
(defmethod gf ((x integer)) x)
(defun make-clo (n) (lambda (x) (+ x n)))
(defun fname (f) (nth-value 2 (function-lambda-expression f)))
(defun fun-name (f) (funcall (find-symbol "%FUN-NAME" "SB-KERNEL") f))

(let ((old #'named-fn))
  (check "a DEFUN by its name" (eq (fname #'named-fn) 'named-fn) (fname #'named-fn))
  (check "it prints as #<FUNCTION NAMED-FN>"
         (string= (prin1-to-string #'named-fn) "#<FUNCTION NAMED-FN>") (prin1-to-string #'named-fn))
  (check "SB-KERNEL:%FUN-NAME agrees" (eq (fun-name #'named-fn) 'named-fn) (fun-name #'named-fn))
  (compile 'named-fn)
  (check "after COMPILE, the new function by name" (eq (fname #'named-fn) 'named-fn) (fname #'named-fn))
  (check "the replaced function keeps its name" (eq (fname old) 'named-fn) (fname old)))
(check "a built-in: CAR" (eq (fname #'car) 'car) (fname #'car))
(check "a generic function by its name" (eq (fname #'gf) 'gf) (fname #'gf))
(check "... printed as a generic function"
       (string= (prin1-to-string #'gf) "#<STANDARD-GENERIC-FUNCTION GF>") (prin1-to-string #'gf))
(let ((c (make-clo 3)))
  (dotimes (i 70) (funcall (make-clo 1) i))         ; hot JIT: MAKE-CLO native
  (setq c (make-clo 3))
  (check "a closure: (LAMBDA :IN MAKE-CLO)" (equal (fname c) '(lambda :in make-clo)) (fname c)))
(check "an unnameable function is NIL or (LAMBDA ...), never another function's name"
       (let ((n (fname (lambda (x) x)))) (or (null n) (and (consp n) (eq (car n) 'lambda)))))
(check "FUNCTION-LAMBDA-EXPRESSION's first two values are unchanged"
       (equal (multiple-value-list (function-lambda-expression #'named-fn)) (list nil nil 'named-fn)))
(format t "~&function-names: ~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
