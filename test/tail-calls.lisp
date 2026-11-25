;;; test/tail-calls.lisp -- a call in tail position does not grow the stack
;;;
;;; ./modus --script test/tail-calls.lisp
;;;
;;; Mutual recursion a million deep: run interpreted first (a runtime DEFUN
;;; starts as an interpreter trampoline) and native once it is hot, so both
;;; tiers' TAILCALL are exercised.  Then what must NOT change: a call that is
;;; not in tail position (inside a dynamic binding, HANDLER-CASE,
;;; UNWIND-PROTECT, CATCH, a block a closure returns from, with five
;;; arguments) still returns to its caller, and values, &rest, closures and
;;; redefinition behave as calls.
;;;
;;; On a target whose compiler emits no TAILCALL (+TARGET-TAIL-CALLS+ 0) the
;;; deep checks are reported as skipped, not failed.

(defvar *tc-failed* 0)
(defvar *tc-skipped* 0)

(defun tc-check (name got want)
  (if (equal got want)
      (format t "~&ok   ~a~%" name)
      (progn (setq *tc-failed* (+ *tc-failed* 1))
             (format t "~&FAIL ~a: got ~s want ~s~%" name got want))))

(defmacro tc-deep (name form want)
  `(let ((r (handler-case ,form
              (serious-condition (c) (list :error (type-of c))))))
     (if (and (consp r) (eq (car r) :error) (not modus-int::*tc-target*))
         (progn (setq *tc-skipped* (+ *tc-skipped* 1))
                (format t "~&skip ~a (no tail calls on this target)~%" ,name))
         (tc-check ,name r ,want))))

(defvar modus-int::*tc-target*
  (handler-case (modus-int::ct-tail-calls-p) (error () nil)))
(format t "~&tail calls on this target: ~a~%" modus-int::*tc-target*)

;;; --- mutual recursion -------------------------------------------------------

(defun tc-even (n) (if (= n 0) t (tc-odd (- n 1))))
(defun tc-odd (n) (if (= n 0) nil (tc-even (- n 1))))
(tc-check "even/odd small" (list (tc-even 10) (tc-odd 10) (tc-even 7)) '(t nil nil))
(tc-deep "even/odd 1M" (tc-even 1000000) t)
(tc-deep "even/odd 1M again (native by now)" (tc-odd 1000001) t)

;; three functions, through COND / LET / WHEN
(defun tc-a (n acc) (cond ((<= n 0) acc) (t (let ((m (- n 1))) (tc-b m (+ acc 1))))))
(defun tc-b (n acc) (when (>= n 0) (tc-c n acc)))
(defun tc-c (n acc) (tc-a n acc))
(tc-deep "a->b->c 1M, cond/let/when" (tc-a 1000000 0) 1000000)

;; a tail call to a function defined later in the file
(defun tc-fwd (n) (if (= n 0) :fwd (tc-fwd2 n)))
(defun tc-fwd2 (n) (tc-fwd (- n 1)))
(tc-deep "forward reference 1M" (tc-fwd 1000000) :fwd)

;; through a closure's body
(defun tc-mk (k) (lambda (n) (tc-cl n k)))
(defun tc-cl (n k) (if (= n 0) k (funcall (tc-mk k) (- n 1))))
(tc-check "closure calls (not flat: FUNCALL)" (tc-cl 1000 :k) :k)
;; a closure's own tail call to a global function is a tail call; FUNCALL is
;; not (yet), so this ping-pong grows by one frame per FUNCALL: shallow
(defun tc-cl2 (n) (let ((f (lambda (m) (if (= m 0) :done (tc-cl2 (- m 1)))))) (funcall f n)))
(tc-check "tail call from a closure body (shallow)" (tc-cl2 10000) :done)
(defun tc-kc (n k) (if (= n 0) (funcall k :kdone) (tc-kc2 (- n 1) (lambda (v) (funcall k v)))))
(defun tc-kc2 (n k) (tc-kc n k))
(tc-check "continuation passing, closures carried through tail calls" (tc-kc 1000 (lambda (v) v)) :kdone)

;;; --- values, arguments ------------------------------------------------------

(defun tc-v2 (x) (values x (* x 2) (* x 3)))
(defun tc-v1 (x) (tc-v2 (+ x 1)))
(tc-check "a tail call's values are the caller's" (multiple-value-list (tc-v1 1)) '(2 4 6))
(defun tc-one (x) (floor x 2))
(defun tc-one-caller (x) (tc-one x))
(tc-check "FLOOR's two values through a tail call" (multiple-value-list (tc-one-caller 7)) '(3 1))
(defun tc-single (x) (if (> x 0) (car (list x)) (tc-v2 x)))
(tc-check "single-valued branch still one value" (multiple-value-list (tc-single 5)) '(5))

(defun tc-rest (&rest r) r)
(defun tc-to-rest (a b) (tc-rest a b 3))
(tc-check "tail call to an &rest function" (tc-to-rest 1 2) '(1 2 3))
(defun tc-opt (a &optional (b :default)) (list a b))
(defun tc-to-opt (a) (tc-opt a))
(tc-check "tail call to an &optional function" (tc-to-opt 1) '(1 :default))
(defun tc-key (&key (x 1) (y 2)) (list x y))
(defun tc-to-key (v) (tc-key :y v))
(tc-check "tail call to a &key function" (tc-to-key 9) '(1 9))

(defun tc-five (a b c d e) (list a b c d e))
(defun tc-to-five (n) (tc-five n n n n n))
(tc-check "five arguments (a call, not a tail call)" (tc-to-five 1) '(1 1 1 1 1))
(defun tc-p5 (a b c d e) (if (= a 0) (list b c d e) (tc-q5 (- a 1) b c d e)))
(defun tc-q5 (a b c d e) (tc-p5 a b c d e))
(tc-check "five-argument mutual recursion, shallow" (tc-p5 1000 1 2 3 4) '(1 2 3 4))

(defun tc-builtin (x) (length x))
(tc-check "tail call to an image function" (tc-builtin '(1 2 3)) 3)
(defun tc-builtin-mv (x) (truncate x 3))
(tc-check "tail call to an image function's values" (multiple-value-list (tc-builtin-mv 10)) '(3 1))

;;; --- what is not a tail call ------------------------------------------------

(defvar *tc-special* :outer)
(defun tc-read-special () *tc-special*)
(defun tc-in-binding () (let ((*tc-special* :inner)) (tc-read-special)))
(tc-check "inside a dynamic binding" (list (tc-in-binding) *tc-special*) '(:inner :outer))

(defun tc-signals () (error "boom"))
(defun tc-in-handler () (handler-case (tc-signals) (error () :caught)))
(tc-check "inside HANDLER-CASE" (tc-in-handler) :caught)

(defvar *tc-cleaned* nil)
(defun tc-in-uwp () (unwind-protect (tc-read-special) (setq *tc-cleaned* t)))
(tc-check "inside UNWIND-PROTECT" (list (tc-in-uwp) *tc-cleaned*) '(:outer t))

(defun tc-thrower () (throw :tc-tag :thrown))
(defun tc-in-catch () (catch :tc-tag (tc-thrower)))
(tc-check "inside CATCH" (tc-in-catch) :thrown)

(defun tc-escape (f) (funcall f))
(defun tc-block-escape () (tc-escape (lambda () (return-from tc-block-escape :escaped))))
(tc-check "a RETURN-FROM out of a closure" (tc-block-escape) :escaped)
(defun tc-blk2 (x) (block b (tc-escape (lambda () (return-from b (list x)))) :not))
(tc-check "a BLOCK a closure returns from" (tc-blk2 4) '(4))

(defun tc-handler-bind ()
  (handler-bind ((warning (lambda (c) (muffle-warning c))))
    (tc-warns)))
(defun tc-warns () (warn "w") :after-warning)
(tc-check "inside HANDLER-BIND" (tc-handler-bind) :after-warning)

;;; --- redefinition reaches a tail caller ------------------------------------

(defun tc-target (x) (list :old x))
(defun tc-caller (x) (tc-target x))
(dotimes (i 100) (tc-caller i))
(defun tc-target (x) (list :new x))
(tc-check "redefinition reaches a native tail caller" (tc-caller 1) '(:new 1))

;;; --- COMPILE and EVAL -------------------------------------------------------

(defun tc-c1 (n) (if (= n 0) :compiled (tc-c2 (- n 1))))
(defun tc-c2 (n) (tc-c1 n))
(compile 'tc-c1) (compile 'tc-c2)
(tc-deep "compiled mutual recursion 1M" (tc-c1 1000000) :compiled)
(tc-check "EVAL of a tail call" (eval '(tc-even 1000)) t)

(format t "~&TAIL-CALLS ~a (~d failed, ~d skipped)~%"
        (if (= *tc-failed* 0) "PASS" "FAIL") *tc-failed* *tc-skipped*)
