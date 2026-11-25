;;; Self tail calls compile as loops (compiler.lisp %SELF-TAIL-LOOP-BODY): deep
;;; self recursion and LABELS loops run in constant stack, with CL semantics
;;; intact.  ./modus --script test/self-tail-calls.lisp ; web: --load it.
(defvar *fails* 0)
(defmacro check (name form want)
  `(let ((got (handler-case ,form (serious-condition (e) (list :error (type-of e))))))
     (if (equal got ,want) (format t "~&ok   ~a~%" ,name)
         (progn (incf *fails*) (format t "~&FAIL ~a: ~s, want ~s~%" ,name got ,want)))))
(defvar *depth* 1000000)
;; deep: self, LABELS, LABELS over a captured variable (WALK-LIST/MESS's MERGE-LOOP)
(defun st-count (n acc) (if (= n 0) acc (st-count (1- n) (1+ acc))))
(check "self 1M" (st-count *depth* 0) *depth*)
(defun st-labels (n) (labels ((lp (i acc) (if (= i 0) acc (lp (1- i) (+ acc 2))))) (lp n 0)))
(check "labels 1M" (st-labels *depth*) (* 2 *depth*))
(defun st-merge (l pred)
  (labels ((ml (r a) (setf (cdr r) a) (if (or (null (cdr a)) (funcall pred (car a) 0)) :end (ml a (cdr a)))))
    (ml (list 0) l)))
(check "labels capturing 1M" (st-merge (make-list *depth* :initial-element 1) (lambda (x y) (< x y))) :end)
;; through COND / WHEN / AND / OR / CASE / LET / LET* / THE / LOCALLY / PROGN
(defun st-shapes (n k)
  (cond ((= n 0) k)
        ((evenp n) (let ((m (1- n))) (st-shapes m (1+ k))))
        (t (let* ((m (1- n)) (j (+ k 1))) (the integer (locally (progn (st-shapes m j))))))))
(check "cond/let/let*/the/locally 1M" (st-shapes *depth* 0) *depth*)
(defun st-case (n) (case (mod n 3) (0 (if (= n 0) :z (st-case (1- n)))) (1 (when t (st-case (1- n)))) (t (or nil (st-case (1- n))))))
(check "case/when/or 300K" (st-case 300000) :z)
;; arguments see the OLD values (PSETQ): a swap
(defun st-swap (n a b) (if (= n 0) (list a b) (st-swap (1- n) b a)))
(check "swap odd" (st-swap 5 :x :y) '(:y :x))
(check "swap even" (st-swap 4 :x :y) '(:x :y))
;; each iteration's closures keep that iteration's bindings
(defun st-clos (n acc) (if (= n 0) (mapcar #'funcall acc) (st-clos (1- n) (cons (lambda () n) acc))))
(check "closures per iteration" (st-clos 4 nil) '(1 2 3 4))
(defun st-setq-clos (n acc) (if (= n 0) (mapcar #'funcall acc) (let ((c (lambda () (setq n (* n 10)) n))) (st-setq-clos (1- n) (cons c acc)))))
(check "closures over a SETQ'd parameter" (st-setq-clos 3 nil) '(10 20 30))
;; multiple values out of the exit; RETURN-FROM the function from inside the loop
(defun st-mv (n) (if (= n 0) (values 1 2 3) (st-mv (1- n))))
(check "values at exit" (multiple-value-list (st-mv 1000)) '(1 2 3))
(defun st-floor (x) (if x (st-floor nil) (floor 7 2)))
(check "a call's values at exit" (multiple-value-list (st-floor t)) '(3 1))
;; ...and a single-valued exit is clamped to ONE value (TREE-EQUAL.6): the GO
;; that replaced the self call must not read as possibly multi-valued
(defun st-one (n f) (cond ((= n 0) (if (funcall f) t nil)) (t (st-one (1- n) f))))
(check "single value after a 2-valued callee" (multiple-value-list (st-one 3 (lambda () (values 1 2)))) '(t))
(check "tree-equal :test's second value does not leak"
       (multiple-value-list (tree-equal '(1 (2)) '(1 (2)) :test (lambda (x y) (values (eql x y) t)))) '(t))
(defun st-ret (n) (when (= n 7) (return-from st-ret :seven)) (if (= n 0) :zero (st-ret (1- n))))
(check "return-from in the loop" (st-ret 100) :seven)
;; a docstring and declarations at the head are kept
(defun st-doc (n) "docs" (declare (fixnum n)) (if (= n 0) :d (st-doc (1- n))))
(check "declared" (st-doc 100000) :d)
;; NOT loops: inside a special binding the call is not a tail call
(defvar *st-x* nil)
(defun st-dyn (n) (if (= n 0) *st-x* (let ((*st-x* n)) (st-dyn (1- n)))))
(check "inside a special binding (not a tail call)" (st-dyn 5) 1)
(defun st-spec (*st-x* n) (if (= n 0) *st-x* (st-spec (1+ *st-x*) (1- n))))
(check "special parameter" (st-spec 0 5) 5)
(defun st-opt (n &optional (acc 0)) (if (= n 0) acc (st-opt (1- n) (1+ acc))))
(check "&optional (left a call)" (st-opt 1000) 1000)
(defun st-arity (n) (if (= n 0) :done (st-arity)))
(check "wrong argument count still an error" (handler-case (progn (st-arity 1) :no-error) (error () :error)) :error)
;; non-tail self calls are untouched
(defun st-fact (n) (if (= n 0) 1 (* n (st-fact (1- n)))))
(check "non-tail recursion" (st-fact 20) 2432902008176640000)
(defun st-fib (n) (if (< n 2) n (+ (st-fib (- n 1)) (st-fib (- n 2)))))
(check "fib" (st-fib 20) 6765)
;; FLET's name is the OUTER function
(defun st-outer (x) (list :outer x))
(defun st-flet (n) (flet ((st-outer (y) (if (= y 0) :inner (st-outer (1- y))))) (st-outer n)))
(check "flet's self name is the outer function" (st-flet 3) '(:outer 2))
(format t "~&SELF-TAIL-CALLS ~a (~d failed)~%" (if (zerop *fails*) "PASS" "FAIL") *fails*)
