;;;; test/web-mvm-calls.lisp -- the JS MVM (web/mvm.js) on what the gate's
;;;; (* 6 7) never runs.  Bugs it let through, all web-only:
;;;;
;;;;   1. AREF on VFP.  The &rest prologue's loop walks frame slots with
;;;;      (:aref val VFP idx); mvm.js read the frame as a vector, so every call
;;;;      with four or more &rest arguments -- (LIST a b c d e), OPEN's keywords,
;;;;      so every LOAD -- got garbage (adebd4fe, 2026-10-22).
;;;;   2. A THROW from an interpreted function to a CATCH in a translated one
;;;;      escaped DRIVE() and was taken by the caller's interpreter loop, which
;;;;      then ended as if the caller had returned.  Seen as the FASL cache
;;;;      failing to record any toplevel EVAL-WHEN (so any DEFPACKAGE).
;;;;   3. SHLV / SARV shifted by the TAGGED count: every runtime (ash x n) with a
;;;;      variable N went 2N bits (sha1's ROTATE-WORD; 06d2bd84, 2026-10-13).
;;;;   4. Frame slots 129+ were the homes of V3 and V5..V8.
;;;;   5. alloc-f32 and the FP register ops were unimplemented: a single-float
;;;;      MAKE-ARRAY stopped the VM, float pages ran on the Lisp interpreter.
;;;;   6. DEFUN of an image function tried to patch a native call pad and failed
;;;;      with PROGRAM-ERROR (Gray streams, so every ASDF load).
;;;;
;;;;   node web/run-node.js --mvmw web/modus.mvmw --load test/web-mvm-calls.lisp --quit
;;;;
;;;; Prints WEB-MVM-CALLS PASS / FAIL.  The hot loops run past the translator's
;;;; threshold (20 calls) so both the interpreted and the translated paths run.

(defvar *wmc-bad* 0)
(defmacro wmc-check (name form want)
  `(let ((got (handler-case ,form (error (e) (list :error (princ-to-string e))))))
     (unless (equal got ,want)
       (incf *wmc-bad*)
       (format t "~&FAIL ~a: got ~s want ~s~%" ,name got ,want))))

;;; 1. positional and &rest calls past four arguments
(defun wmc-five (a b c d e) (list a b c d e))
(defun wmc-rest (&rest r) r)
(defun wmc-req2 (x y &rest r) (list x y r))
(wmc-check "five positional" (wmc-five 1 2 3 4 5) '(1 2 3 4 5))
(dotimes (n 41)
  (let ((args (loop for i below n collect i)))
    (wmc-check (format nil "&rest ~d" n) (apply #'wmc-rest args) args)
    (wmc-check (format nil "LIST ~d" n) (apply #'list args) args)
    (when (>= n 2)
      (wmc-check (format nil "req-2 &rest ~d" n) (apply #'wmc-req2 args) (list 0 1 (cddr args))))))
(dotimes (i 50)
  (wmc-check "hot &rest" (wmc-rest i 1 2 3 4 5 6) (list i 1 2 3 4 5 6)))

;;; 2. CATCH in a hot function, THROW from a callee that is still interpreted
;;; when the catcher is already translated (the callee is defined to be called
;;; less often than the catcher, so it crosses the threshold later)
(defun wmc-thrower (x) (when (eql x 3) (throw 'wmc-rej "rejected")) x)
(defun wmc-catcher (items) (catch 'wmc-rej (dolist (x items) (wmc-thrower x)) :none))
(defun wmc-catcher-hc (items)
  (catch 'wmc-rej
    (handler-case (progn (dolist (x items) (wmc-thrower x)) :none)
      (error (e) :caught))))
(defun wmc-closure-catcher (items)
  (catch 'wmc-rej
    (let ((k 0))
      (dolist (x items) (funcall (lambda (y) (when (eql y 3) (throw 'wmc-rej "rejected")) (setq k (+ k 1))) x))
      k)))
(let ((completed 0))
  (dotimes (i 60)
    (wmc-check "catch / callee throw" (wmc-catcher '(1 2 3 4)) "rejected")
    (wmc-check "catch > handler-case / callee throw" (wmc-catcher-hc '(1 2 3 4)) "rejected")
    (wmc-check "catch / closure throw" (wmc-closure-catcher '(1 2 3 4)) "rejected")
    (setq completed (+ completed 1)))
  ;; the bug ended THIS loop early, its caller's work silently skipped
  (wmc-check "caller loop ran to the end" completed 60))

;;; 3. ASH by a variable count (runtime-compiled: the count is a register)
(defun wmc-ash (x n) (ash x n))
(defun wmc-rotl (w bits) (logior (logand (ash w (- bits 32)) (1- (ash 1 bits))) (logand (ash w bits) #xffffffff)))
(dotimes (i 30)
  (wmc-check "ash left" (wmc-ash 1 5) 32)
  (wmc-check "ash right" (wmc-ash -1000 -3) -125)
  (wmc-check "ash to a bignum" (wmc-ash 3 70) (* 3 (expt 2 70)))
  (wmc-check "rotate-word" (wmc-rotl #x80000001 1) 3)
  (wmc-check "rotate-word 30" (wmc-rotl #xefcdab89 30) #x7bf36ae2))

;;; 4. a frame of more than 129 slots: 160 locals live across a call
(defmacro wmc-big-let (n)
  (let ((vars (loop for i below n collect (intern (format nil "WMC-B~d" i)))))
    `(lambda (seed)
       (let* ,(loop for v in vars for i from 0 collect `(,v (+ seed ,i)))
         (funcall (lambda (x) x) seed)
         (list ,@vars)))))
(let ((f (eval '(wmc-big-let 160))))
  (dotimes (j 25)
    (wmc-check "160-slot frame" (funcall f (* j 1000)) (loop for i below 160 collect (+ (* j 1000) i)))))

;;; 5. floats: packed single-float vectors, unboxed single and double trees
(defun wmc-f32 (v) (declare (type (simple-array single-float (*)) v))
  (dotimes (i (length v)) (setf (aref v i) (* (aref v i) 1.5f0)))
  (list (aref v 0) (aref v 2) (length v)))
(defun wmc-dsum (v) (declare (type (simple-array double-float (*)) v))
  (let ((s 0d0)) (declare (double-float s)) (dotimes (i (length v) s) (incf s (* (aref v i) 0.5d0)))))
(defun wmc-single (a b) (declare (single-float a b)) (list (+ (* a 2f0) b 0.5f0) (< a b) (abs (- a 3f0)) (max (* a 1f0) (* b 1f0)) (round (* a 1f0))))
(defun wmc-double (a n) (declare (double-float a) (fixnum n)) (list (+ (* a 3.25d0) n) (<= (* a a) 2d0) (floor (* a 1d0)) (min (* a 1d0) -2d0)))
(dotimes (i 30)
  (wmc-check "f32 vector" (wmc-f32 (make-array 3 :element-type 'single-float :initial-contents '(1f0 2f0 -4f0))) (list 1.5f0 -6f0 3))
  (wmc-check "double vector" (wmc-dsum (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0))) 5d0)
  (wmc-check "single tree" (wmc-single 2.5f0 2f0) (list 7.5f0 nil 0.5f0 2.5f0 2))
  (wmc-check "double tree" (wmc-double 1.5d0 3) (list 7.875d0 nil 1 -2d0)))

;;; 6. DEFUN replacing an image function, then calling the original through it
(let ((orig (symbol-function 'lisp-implementation-version)))
  (wmc-check "redefine an image function"
             (progn (eval `(defun lisp-implementation-version () (funcall ,orig)))
                    (lisp-implementation-version))
             (funcall orig)))

(format t "~&WEB-MVM-CALLS ~a (~d failed)~%" (if (eql *wmc-bad* 0) "PASS" "FAIL") *wmc-bad*)
