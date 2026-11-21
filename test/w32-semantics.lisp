;;;; test/w32-semantics.lisp -- the W32 rewrite's semantic edges (docs/w32.md, M3)
;;;;
;;; Portable CL.  Each W32S-NN is a function whose result is an integer; the
;;; runner prints "NN value".  Run three ways and compare line for line:
;;;   SBCL (the reference):    sbcl --script test/w32-semantics-run.lisp
;;;   a Modus image, baked:    MODUS_EXTRA_SOURCE=test/w32-semantics.lisp ... build
;;;                            IMAGE --eval '(modus-int::w32s-run)' --quit
;;; and against the SAME source baked into the base (pre-W32) image, so a case
;;; where Modus already differed from SBCL is told apart from one W32 changed.
;;; The rewrite must never change a result: eligible trees are proved exact,
;;; and everything else must compile as before.

(declaim (inline w32s-rotl))
(defun w32s-rotl (x n)
  (declare (type (unsigned-byte 32) x) (type (integer 0 31) n))
  (logand #xffffffff (logior (ash x n) (ash x (- n 32)))))

(defun w32s-f (x) (+ x 1))                        ; a call leaf

;;; masked wraps over declared (unsigned-byte 32) leaves
(defun w32s-01 (a b) (declare (type (unsigned-byte 32) a b)) (logand (+ a b) #xFFFFFFFF))
(defun w32s-02 (a b) (declare (type (unsigned-byte 32) a b)) (logand (- a b) #xFFFFFFFF))
(defun w32s-03 (a b) (declare (type (unsigned-byte 32) a b)) (logand (* a b) #xFFFFFFFF))
(defun w32s-04 (a) (declare (type (unsigned-byte 32) a)) (logand (lognot a) #xFFFFFFFF))
(defun w32s-05 (a) (declare (type (unsigned-byte 32) a)) (logand (- a) #xFFFFFFFF))
(defun w32s-06 (a) (declare (type (unsigned-byte 32) a)) (logand (+ a -1) #xFFFFFFFF))
(defun w32s-07 (a) (declare (type (unsigned-byte 32) a)) (logand (+ a a a) #xFFFFFFFF))
(defun w32s-08 (a) (declare (type (unsigned-byte 32) a)) (logand (ash a 31) #xFFFFFFFF))
;;; fixnum and signed leaves, negative values
(defun w32s-09 (x y) (declare (fixnum x y)) (logand (+ x y) #xFFFFFFFF))
(defun w32s-10 (x) (declare (type (signed-byte 32) x)) (logand (+ x 1) #xFFFFFFFF))
(defun w32s-11 (x) (declare (type (signed-byte 32) x)) (logand (logxor x #xDEADBEEF) #xFFFFFFFF))
;;; exact trees with no mask (byte assembly, SHA-256's sigma)
(defun w32s-12 (v j)
  (declare (type (simple-array (unsigned-byte 8) (*)) v) (fixnum j))
  (logior (ash (aref v j) 24) (ash (aref v (+ j 1)) 16) (ash (aref v (+ j 2)) 8) (aref v (+ j 3))))
(defun w32s-13 (x) (declare (type (unsigned-byte 32) x))
  (logxor (logior (ash x -7) (ldb (byte 32 0) (ash x 25)))
          (logior (ash x -18) (ldb (byte 32 0) (ash x 14)))
          (ash x -3)))
(defun w32s-14 (a b) (declare (type (unsigned-byte 32) a b))
  (ash (logand (+ a b) #xFFFFFFFF) -4))
;;; rotates: recognised (the inline ROTL32 shape) and not (through LOGXOR)
(defun w32s-15 (x) (declare (type (unsigned-byte 32) x)) (w32s-rotl x 7))
(defun w32s-16 (x) (declare (type (unsigned-byte 32) x)) (w32s-rotl x 16))
(defun w32s-17 (x) (declare (type (unsigned-byte 32) x))
  (logand (logxor (ash x 7) (ash x -25)) #xFFFFFFFF))
;;; MUST NOT be rewritten: the value is not 32 bits, or the tree is not provable
(defun w32s-18 (a b) (declare (type (unsigned-byte 32) a b)) (+ a b))          ; 33 bits
(defun w32s-19 (a b) (declare (type (unsigned-byte 32) a b)) (ash (+ a b) -4)) ; high bit shifted down
(defun w32s-20 (a b) (declare (type (unsigned-byte 32) a b)) (logand (+ a (w32s-f b)) #xFFFFFFFF))
(defun w32s-21 (v) (declare (simple-vector v)) (logand (+ (svref v 0) (svref v 1)) #xFFFFFFFF))
(defun w32s-22 (a b) (declare (type (unsigned-byte 32) a b)) (* a b))           ; 64 bits
;;; a declared (unsigned-byte 32) binding of an out-of-range value at safety 1:
;;; the rewrite does not touch bindings, so this answers as the base image does
(defun w32s-23 (a b)
  (declare (type (unsigned-byte 32) a b) (optimize (safety 1)))
  (handler-case (let ((x (+ a b))) (declare (type (unsigned-byte 32) x)) x)
    (type-error () -1)))

(defun w32s-run ()
  (let ((m #xFFFFFFFF) (h #x80000000) (bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref bytes 0) #xDE (aref bytes 1) #xAD (aref bytes 2) #xBE (aref bytes 3) #xEF)
    (dolist (r (list (list 1 (w32s-01 m 1)) (list 1 (w32s-01 h h)) (list 1 (w32s-01 12345 67890))
                     (list 2 (w32s-02 0 1)) (list 2 (w32s-02 5 3)) (list 3 (w32s-03 m m))
                     (list 3 (w32s-03 #x10000 #x10000)) (list 3 (w32s-03 #x12345 #x6789A))
                     (list 4 (w32s-04 0)) (list 4 (w32s-04 h)) (list 5 (w32s-05 1)) (list 5 (w32s-05 0))
                     (list 6 (w32s-06 0)) (list 7 (w32s-07 h)) (list 8 (w32s-08 3))
                     (list 9 (w32s-09 -5 3)) (list 9 (w32s-09 most-positive-fixnum 1))
                     (list 10 (w32s-10 (- 0 #x80000000))) (list 10 (w32s-10 -1))
                     (list 11 (w32s-11 -1)) (list 11 (w32s-11 #x7FFFFFFF))
                     (list 12 (w32s-12 bytes 0)) (list 13 (w32s-13 #x6A09E667)) (list 13 (w32s-13 m))
                     (list 14 (w32s-14 m m)) (list 15 (w32s-15 #x80000001)) (list 15 (w32s-15 m))
                     (list 16 (w32s-16 #x12345678)) (list 17 (w32s-17 #x80000001))
                     (list 18 (w32s-18 m m)) (list 19 (w32s-19 m m)) (list 20 (w32s-20 m 0))
                     (list 21 (w32s-21 (vector m 1))) (list 22 (w32s-22 m m)) (list 23 (w32s-23 m 1))))
      (format t "~D ~D~%" (first r) (second r)))
    t))
