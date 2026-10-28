;;;; test/call-arguments-limit.lisp -- calls with more arguments than fit a frame
;;;;
;;;;   ./modus --script test/call-arguments-limit.lisp
;;;;
;;;; Past 120 (+APPLY-SPREAD-MAX+, mvm/compiler.lisp) a call passes 120
;;;; arguments positionally and then the LIST of the rest, with nargs 254
;;;; (+REST-TAIL-NARGS+); an &rest prologue takes that list as its tail.  So
;;;; CALL-ARGUMENTS-LIMIT is MOST-POSITIVE-FIXNUM.  The counts here straddle
;;;; every boundary the scheme has: the 4 register arguments, the old 32-word
;;;; copy on the 32-bit back ends, 120/121, and the 254/255 sentinels.  Each
;;;; check runs 150 times, so a runtime DEFUN is exercised interpreted AND
;;;; after the hot-function JIT has made it native (64 calls).
;;;; CALL_LIMIT_REPS=2 for an image with no JIT (i386), where 150 is only slow.

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name ok)
  (if ok (setq *pass* (+ *pass* 1))
      (progn (setq *fail* (+ *fail* 1)) (format t "FAIL ~a~%" name))))

(defun iota1 (n) (let ((acc nil)) (dotimes (i n) (push (- n i) acc)) acc))

(defun r0 (&rest r) r)
(defun r2 (a b &rest r) (list a b (length r) (car (last r))))
(defun o2 (a &optional (b :none) &rest r) (list a b (length r)))
(defun f3 (a b c) (list a b c))

(defstruct big70
  a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
  a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39
  a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56 a57 a58 a59
  a60 a61 a62 a63 a64 a65 a66 a67 a68 a69)

(defmacro call-with-n (fn n) `(,fn ,@(iota1 n)))
(defmacro funcall-with-n (fn n) `(funcall ,fn ,@(iota1 n)))

(check "limit" (>= call-arguments-limit 100000))

(dotimes (rep (let ((e (sb-ext:posix-getenv "CALL_LIMIT_REPS")))
                (if e (parse-integer e) 150)))
  (dolist (n '(0 1 4 5 31 32 33 64 119 120 121 122 130 254 255 256 1000 100000))
    (let ((l (iota1 n)))
      (check (format nil "apply r0 ~d" n) (equal (apply #'r0 l) l))
      (when (>= n 2)
        (check (format nil "apply r2 ~d" n)
               (equal (apply #'r2 l) (list 1 2 (- n 2) (if (> n 2) n nil)))))
      (when (>= n 1)
        (check (format nil "apply o2 ~d" n)
               (equal (apply #'o2 l) (list 1 (if (>= n 2) 2 :none) (max 0 (- n 2))))))
      (check (format nil "apply leading ~d" n)
             (equal (apply #'r0 :x :y l) (list* :x :y l)))
      (check (format nil "closure ~d" n)
             (let ((k 7)) (= (apply (lambda (&rest r) (+ k (length r))) l) (+ k n))))))
  ;; 140 keyword arguments into a 70-slot constructor
  (let* ((keys '(:a00 :a01 :a02 :a03 :a04 :a05 :a06 :a07 :a08 :a09 :a10 :a11 :a12 :a13
                 :a14 :a15 :a16 :a17 :a18 :a19 :a20 :a21 :a22 :a23 :a24 :a25 :a26 :a27
                 :a28 :a29 :a30 :a31 :a32 :a33 :a34 :a35 :a36 :a37 :a38 :a39 :a40 :a41
                 :a42 :a43 :a44 :a45 :a46 :a47 :a48 :a49 :a50 :a51 :a52 :a53 :a54 :a55
                 :a56 :a57 :a58 :a59 :a60 :a61 :a62 :a63 :a64 :a65 :a66 :a67 :a68 :a69))
         (args (let ((acc nil) (i 0))
                 (dolist (k keys (nreverse acc))
                   (push k acc) (push (* 10 i) acc) (setq i (+ i 1)))))
         (s (apply #'make-big70 args)))
    (check "constructor first" (eql (big70-a00 s) 0))
    (check "constructor 60" (eql (big70-a60 s) 600))
    (check "constructor last" (eql (big70-a69 s) 690)))
  (check "literal call 130" (equal (call-with-n r0 130) (iota1 130)))
  (check "funcall 130" (equal (funcall-with-n #'r0 130) (iota1 130)))
  (check "fixed lambda list, 130"
         (eq :error (handler-case (apply #'f3 (iota1 130)) (program-error () :error)))))

(format t "~&call-arguments-limit: ~d passed, ~d failed~%" *pass* *fail*)
(sys-exit (if (zerop *fail*) 0 1))
