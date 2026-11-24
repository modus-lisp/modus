;;;; test/w32-fe-mul-sim.lisp -- FE-MUL's and FE-SQ's W32 bodies against the generic body, on SBCL
;;;;
;;;;   sbcl --script test/w32-fe-mul-sim.lisp [N]
;;;;
;;; net/crypto-fast.lisp's FE-MUL carries two bodies (docs/w32.md, M3b): the
;;; fixnum one every 64-bit target compiles, and, under (%IF-W32 ...), one in
;;; raw 32-bit words for the 30-bit towers.  This reads both FROM THE SOURCE --
;;; its DEFMACROs and its FE-MUL DEFUN, nothing copied -- models the W32
;;; operations as functions mod 2^32 (W32-LTU a fixnum 0/1, W32-HI16 / -LO16
;;; the halves, W32-REF / W32-RAW the identity), and compares the W32 body's
;;; result with the generic body's on N random inputs (default 200000) with
;;; limbs drawn below 2^25, 2^26, 2^27 (the W32 guard's bound) and 2^27 + 1
;;; (past it: the generic fallback inside the W32 body), each 1 in 8 at its
;;; top value.  FE-SQ's W32 body (55 products) is checked the same way against
;;; the generic body's f*f, with DST the same vector as F.  Exit 0 iff every
;;; result agrees.  It checks the arithmetic, not
;;; the compiler: the bare X25519 / Ed25519 runs (test/w32-x25519-bare.py)
;;; are the check of the compiled code.
(defpackage :w32-fe-mul-sim (:use :common-lisp))
(in-package :w32-fe-mul-sim)

(defvar *pick-w32* t)
(declaim (declaration w32-raw))   ; compiler.lisp's raw-word scope declaration
(defmacro %if-w32 (w32-form other-form) (if *pick-w32* w32-form other-form))
(defmacro %w32 (x) x)
(defmacro w32-raw (x) x)
(defmacro w32-ref (v) v)
(defun m32 (x) (logand x #xFFFFFFFF))
(defun w32-from-fix (x) (m32 x))
(defun w32-li (x) x)
(defun w32-add (a b) (m32 (+ a b)))
(defun w32-mul (a b) (m32 (* a b)))
(defun w32-mulhu (a b) (ash (* a b) -32))
(defun w32-and (a b) (logand a b))
(defun w32-or (a b) (logior a b))
(defun w32-shl (a n) (m32 (ash a n)))
(defun w32-shr (a n) (ash a (- n)))
(defun w32-ltu (a b) (if (< a b) 1 0))
(defun w32-hi16 (a) (ash a -16))
(defun w32-lo16 (a) (logand a #xFFFF))

(defparameter *source*
  (merge-pathnames "../net/crypto-fast.lisp" (directory-namestring *load-truename*)))

;; Every DEFMACRO in the file, and the FE-MUL DEFUN twice: compiled with each
;; body chosen (the choice is made at macroexpansion, as the compiler makes it).
(let ((forms (with-open-file (s *source*)
               (let ((*package* (find-package :w32-fe-mul-sim)))
                 (loop for f = (read s nil :eof) until (eq f :eof) collect f)))))
  (dolist (f forms)
    (when (and (consp f) (eq (car f) 'defmacro)) (eval f)))
  (let ((fe-mul (find-if (lambda (f) (and (consp f) (eq (car f) 'defun) (eq (cadr f) 'fe-mul)))
                         forms)))
    (unless fe-mul (error "no FE-MUL in ~A" *source*))
    (let ((*pick-w32* t))
      (compile 'fe-mul-w32 `(lambda ,@(cddr fe-mul))))
    (let ((*pick-w32* nil))
      (compile 'fe-mul-generic `(lambda ,@(cddr fe-mul)))))
  ;; FE-SQ's W32 body; its fallback calls FE-MUL, here the generic one
  (let ((fe-sq (find-if (lambda (f) (and (consp f) (eq (car f) 'defun) (eq (cadr f) 'fe-sq)))
                        forms)))
    (unless fe-sq (error "no FE-SQ in ~A" *source*))
    (setf (fdefinition 'fe-mul) (lambda (d f g) (funcall 'fe-mul-generic d f g)))
    (let ((*pick-w32* t))
      (compile 'fe-sq-w32 `(lambda ,@(cddr fe-sq))))))

(let* ((n (if (second sb-ext:*posix-argv*) (parse-integer (second sb-ext:*posix-argv*)) 200000))
       (*random-state* (sb-ext:seed-random-state 7))
       (bad 0) (fast 0) (sq-bad 0) (sq-fast 0))
  (dotimes (k n)
    (let ((lim (case (mod k 4) (0 #x2000000) (1 #x4000000) (2 #x8000000) (t #x8000001)))
          (f (make-array 40 :initial-element 0)) (g (make-array 40 :initial-element 0))
          (d1 (make-array 40 :initial-element 0)) (d2 (make-array 40 :initial-element 0)))
      (dotimes (i 10)
        (setf (svref f i) (if (zerop (random 8)) (1- lim) (random lim))
              (svref g i) (if (zerop (random 8)) (1- lim) (random lim))))
      (when (and (every (lambda (x) (< x #x8000000)) (subseq f 0 10))
                 (every (lambda (x) (< x #x8000000)) (subseq g 0 10)))
        (incf fast))
      (funcall 'fe-mul-generic d1 f g)
      (funcall 'fe-mul-w32 d2 f g)
      (unless (equalp (subseq d1 0 10) (subseq d2 0 10)) (incf bad))
      ;; fe-sq's W32 body against the generic f*f (dst aliasing f, as callers do)
      (let ((s1 (make-array 40 :initial-element 0)) (s2 (copy-seq f)))
        (when (every (lambda (x) (< x #x8000000)) (subseq f 0 10)) (incf sq-fast))
        (funcall 'fe-mul-generic s1 f f)
        (funcall 'fe-sq-w32 s2 s2)
        (unless (equalp (subseq s1 0 10) (subseq s2 0 10)) (incf sq-bad)))))
  (format t "fe-mul W32 body vs generic: ~D inputs (~D inside the W32 guard), ~D differ~%"
          n fast bad)
  (format t "fe-sq W32 body vs generic f*f: ~D inputs (~D inside the W32 guard), ~D differ~%"
          n sq-fast sq-bad)
  (sb-ext:exit :code (if (and (zerop bad) (zerop sq-bad)) 0 1)))
