;;;; test/symbol-plist-slot.lisp -- plists in the symbol, struct predicates by EQ, global SETQ inline.
;;;;   modus --script test/symbol-plist-slot.lisp   => SYMBOL-PLIST-SLOT PASS
;;;;
;;;; A heap symbol keeps its property list in slot 3 and its qualified-name memo in slot 4 (both
;;;; used to be EQ hash tables); struct predicates compare type names by EQ before hashing; a
;;;; runtime SETQ of a global stores any value but a cons or a closure in place.  The behaviour
;;;; these must keep: GET / (SETF GET) / REMPROP / SYMBOL-PLIST on every kind of symbol, struct
;;;; subtypes, and closures stored in globals still callable.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(setf (get 'foo 'a) 1 (get 'foo 'b) 2)
(check "get" (list (get 'foo 'a) (get 'foo 'b) (get 'foo 'c :d)) '(1 2 :d))
(check "plist" (symbol-plist 'foo) '(b 2 a 1))
(check "remprop" (list (not (null (remprop 'foo 'a))) (symbol-plist 'foo) (remprop 'foo 'zz)) '(t (b 2) nil))
(setf (symbol-plist 'bar) (list 'x 9))
(check "setf-plist" (get 'bar 'x) 9)
(check "keyword-empty" (get :kw-plist-test 'z) nil)
(setf (get :kw-plist-test 'z) 5)
(check "keyword" (get :kw-plist-test 'z) 5)
(setf (get 'car 'plist-test) 3)
(check "cl-symbol" (get 'car 'plist-test) 3)
(let ((g (gensym)) (m (make-symbol "M")))
  (setf (get g 'p) :g (get m 'p) :m)
  (check "uninterned" (list (get g 'p) (get m 'p)) '(:g :m)))
(let ((c (copy-symbol 'foo t)))
  (check "copy-symbol" (list (get c 'b) (eq (symbol-plist c) (symbol-plist 'foo))) '(2 nil)))
(defun get-in-loop (n) (let ((s 0)) (dotimes (i n) (when (get 'foo 'b) (incf s))) s))
(check "get-compiled" (get-in-loop 1000) 1000)

(defstruct pt x) (defstruct (pt3 (:include pt)) z) (defstruct other y)
(check "struct-p" (list (pt-p (make-pt)) (pt-p (make-pt3)) (pt3-p (make-pt)) (pt-p (make-other))
                        (typep (make-pt3) 'pt) (typep (make-pt) 'pt3) (pt-p 5) (pt-p nil))
       '(t t nil nil t nil nil nil))
(check "struct-accessor" (pt-x (make-pt3 :x 4 :z 5)) 4)

(defvar *g* nil)
(defun sg (v) (setq *g* v))
(sg (make-pt :x 4))       (check "setq-struct" (pt-x *g*) 4)
(sg "str")                (check "setq-string" *g* "str")
(sg 1.5d0)                (check "setq-float" *g* 1.5d0)
(sg (vector 1 2))         (check "setq-vector" (aref *g* 1) 2)
(sg (lambda () 8))        (check "setq-closure" (funcall *g*) 8)
(sg (list (lambda () 7))) (check "setq-list-of-closure" (funcall (car *g*)) 7)
(let ((k 3)) (sg (vector (lambda () k))))
(check "setq-vector-of-closure" (funcall (aref *g* 0)) 3)
(sg nil)                  (check "setq-nil" *g* nil)
(sg 42)                   (check "setq-fixnum" *g* 42)

(format t "~&SYMBOL-PLIST-SLOT ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
