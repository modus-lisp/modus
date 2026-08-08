;; T1: the reported repro — nil-step var must reset each iteration
(princ "T1 ") (princ
 (let ((seen nil))
   (do ((x '(a b) (cdr x)) (lookat nil nil)) ((null x) (reverse seen))
     (when (eq (car x) 'a) (setq lookat 'a))
     (push lookat seen))))
(princ "  want (A NIL)") (terpri)

;; T2: control — NO step form, var persists
(princ "T2 ") (princ
 (let ((seen nil))
   (do ((x '(a b) (cdr x)) (lookat nil)) ((null x) (reverse seen))
     (when (eq (car x) 'a) (setq lookat 'a))
     (push lookat seen))))
(princ "  want (A A)") (terpri)

;; T3: control — non-NIL step form
(princ "T3 ") (princ
 (do ((i 0 (+ i 1)) (acc nil (cons i acc))) ((= i 3) (reverse acc))))
(princ "  want (0 1 2)") (terpri)

;; T4: multiple nil-step vars
(princ "T4 ") (princ
 (let ((seen nil))
   (do ((n 0 (+ n 1)) (a nil nil) (b nil nil)) ((= n 3) (reverse seen))
     (push (list a b) seen)
     (setq a n) (setq b (* n 10)))))
(princ "  want ((NIL NIL) (NIL NIL) (NIL NIL))") (terpri)

;; T5: nil-step mixed with real steps, parallel semantics
;;   x and y swap each iteration -> parallel binding required
(princ "T5 ") (princ
 (do ((n 0 (+ n 1)) (x 1 y) (y 2 x) (z nil nil)) ((= n 2) (list x y z))
   (setq z 99)))
(princ "  want (1 2 NIL)") (terpri)

;; T6: DO* nil-step var
(princ "T6 ") (princ
 (let ((seen nil))
   (do* ((x '(a b) (cdr x)) (lookat nil nil)) ((null x) (reverse seen))
     (when (eq (car x) 'a) (setq lookat 'a))
     (push lookat seen))))
(princ "  want (A NIL)") (terpri)

;; T7: DO* sequential step semantics (y sees the NEW x)
(princ "T7 ") (princ
 (do* ((n 0 (+ n 1)) (x 0 (+ x 10)) (y 0 x) (q nil nil)) ((= n 2) (list x y q))
   (setq q 7)))
(princ "  want (20 20 NIL)") (terpri)

;; T8: nil-step + tagbody/go in body
(princ "T8 ") (princ
 (let ((seen nil))
   (do ((n 0 (+ n 1)) (flag nil nil)) ((= n 3) (reverse seen))
     (when (evenp n) (go skip))
     (setq flag :odd)
    skip
     (push flag seen))))
(princ "  want (NIL :ODD NIL)") (terpri)

;; T9: nil-step, result form multiple values position / var read in result
(princ "T9 ") (princ
 (do ((n 0 (+ n 1)) (last nil nil)) ((= n 3) (list :done last))
   (setq last n)))
(princ "  want (:DONE NIL)") (terpri)

;; T10: step form that is the literal 0 / literal NIL mixed, DO
(princ "T10 ") (princ
 (do ((n 0 (+ n 1)) (z 5 0) (w 5 nil)) ((= n 2) (list z w))
   (setq z 100) (setq w 100)))
(princ "  want (0 NIL)") (terpri)

;; T11: no bindings at all (steps list empty)
(princ "T11 ") (princ
 (let ((c 0)) (do () ((= c 3) c) (setq c (+ c 1)))))
(princ "  want 3") (terpri)

;; T12: bare symbol binding (no init, no step) - must persist
(princ "T12 ") (princ
 (do ((n 0 (+ n 1)) v) ((= n 3) v) (when (= n 1) (setq v :set))))
(princ "  want :SET") (terpri)

;; T13: puri-shaped if* state machine miniature
(princ "T13 ") (princ
 (do ((xx '(1 2 3) (cdr xx)) (state :init) (lookat nil nil) (col nil))
     ((null xx) (list state lookat col))
   (setq lookat (car xx))
   (setq col (cons lookat col))))
(princ "  want (:INIT NIL (3 2 1))") (terpri)
