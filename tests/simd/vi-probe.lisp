;;; vi-probe.lisp — integer-lane primitives vs plain-Lisp references.
;;; Run with the JIT on (aarch64: native arms) and MODUS_NO_JIT=1 (interpreter arms).
(setq *jit-hot-only* nil)
(defvar *fails* 0)
(defun u8v (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun s16-store (a i x) (let ((u (logand x #xFFFF))) (setf (aref a i) (logand u 255) (aref a (+ i 1)) (ash u -8))))
(defun s16-load (a i) (let ((u (logior (aref a i) (ash (aref a (+ i 1)) 8)))) (if (>= u 32768) (- u 65536) u)))
(defun clamp255 (x) (max 0 (min 255 x)))
(defun check (name got want)
  (if (equalp got want) (format t "PASS ~a~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~a~%  got  ~a~%  want ~a~%" name got want))))
(defun bytes (a n) (let ((l nil)) (dotimes (i n) (push (aref a i) l)) (nreverse l)))

;; 1. ld16 / st16 round trip at an unaligned index
(defun t1 (src dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) src dst))
  (%vi-st16 dst 0 (%vi-ld16 src 3)))
(let ((src (u8v 32)) (dst (u8v 16)))
  (dotimes (i 32) (setf (aref src i) i))
  (t1 src dst)
  (check "1 ld16/st16" (bytes dst 16) (let ((l nil)) (dotimes (i 16) (push (+ i 3) l)) (nreverse l))))

;; 2. uxtl + add16 (dup) + sqxtun : saturating byte add
(defun t2 (a dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) a dst))
  (%vi-st8 dst 0 (%vi-sqxtun (%vi-add16 (%vi-uxtl (%vi-ld8 a 0)) (%vi-dup16 200)))))
(let ((a (u8v 8)) (dst (u8v 8)) (vals '(0 10 50 100 150 200 250 255)))
  (dotimes (i 8) (setf (aref a i) (nth i vals)))
  (t2 a dst)
  (check "2 uxtl/add16/sqxtun" (bytes dst 8) (mapcar (lambda (x) (clamp255 (+ x 200))) vals)))

;; 3. two-tap filter with by-lane MUL/MLA and SQRSHRUN #2 : ((x0*t0 + x1*t1) + 2) >> 2, clamp
(defun t3 (a taps dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) a taps dst))
  (%vi-st8 dst 0 (%vi-sqrshrun (%vi-mla16l (%vi-mul16l (%vi-uxtl (%vi-ld8 a 0)) (%vi-ld16 taps 0) 0)
                                            (%vi-uxtl (%vi-ld8 a 1)) (%vi-ld16 taps 0) 1)
                                2)))
(let ((a (u8v 16)) (taps (u8v 16)) (dst (u8v 8)))
  (dotimes (i 16) (setf (aref a i) (* i 17)))
  (s16-store taps 0 3) (s16-store taps 2 -1)
  (t3 a taps dst)
  (check "3 mul16l/mla16l/sqrshrun" (bytes dst 8)
         (let ((l nil)) (dotimes (i 8) (push (clamp255 (ash (+ (* 3 (aref a i)) (* -1 (aref a (+ i 1))) 2) -2)) l)) (nreverse l))))

;; 4. umov16 of sqdmulh16 : (2*a*b)>>16
(defun t4 () (%vi-umov16 (%vi-sqdmulh16 (%vi-dup16 20091) (%vi-dup16 1000)) 3))
(check "4 sqdmulh16/umov16" (t4) (ash (* 2 20091 1000) -16))

;; 5. VI-PACK let inside a call-free loop; add16 on s16 lanes of raw bytes; mls16 accumulate
(defun t5 (src dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) src dst))
  (let ((k (%vi-dup16 3)))
    (declare (type vi-pack k))
    (dotimes (i 2)
      (%vi-st16 dst (* i 16) (%vi-mls16 (%vi-add16 (%vi-ld16 src (* i 16)) k) k k)))))
(let ((src (u8v 32)) (dst (u8v 32)))
  (dotimes (i 32) (setf (aref src i) (+ i 1)))
  (t5 src dst)
  (check "5 vi-pack let/add16/mls16"
         (let ((l nil)) (dotimes (j 16) (push (s16-load dst (* 2 j)) l)) (nreverse l))
         (let ((l nil)) (dotimes (j 16) (push (- (+ (s16-load src (* 2 j)) 3) 9) l)) (nreverse l))))

;; 6. loop-filter-style mask: uabd, cmhs (thr >= |a-b|), bsl select
(defun t6 (a b dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) a b dst))
  (%vi-st16 dst 0 (%vi-bsl (%vi-cmhs8 (%vi-dup8 20) (%vi-uabd8 (%vi-ld16 a 0) (%vi-ld16 b 0)))
                           (%vi-ld16 a 0) (%vi-ld16 b 0))))
(let ((a (u8v 16)) (b (u8v 16)) (dst (u8v 16)))
  (dotimes (i 16) (setf (aref a i) (* i 10) (aref b i) (+ (* i 10) (if (evenp i) 5 40))))
  (t6 a b dst)
  (check "6 uabd/cmhs/bsl" (bytes dst 16)
         (let ((l nil)) (dotimes (i 16) (push (if (<= (abs (- (aref a i) (aref b i))) 20) (aref a i) (aref b i)) l)) (nreverse l))))

;; 7. sqadd8/sqsub8 with movi and eor #x80 (signed view of unsigned samples)
(defun t7 (a dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) a dst))
  (%vi-st16 dst 0 (%vi-eor (%vi-sqadd8 (%vi-eor (%vi-ld16 a 0) (%vi-movi8 128)) (%vi-movi8 100)) (%vi-movi8 128))))
(let ((a (u8v 16)) (dst (u8v 16)))
  (dotimes (i 16) (setf (aref a i) (* i 17)))
  (t7 a dst)
  (check "7 eor/sqadd8/movi8" (bytes dst 16)
         (let ((l nil)) (dotimes (i 16) (push (clamp255 (+ (aref a i) 100)) l)) (nreverse l))))

;; 8. sshr16 / shl16 / sub16 / xtn / sxtl chain, read back via umov32
(defun t8 () (%vi-umov32 (%vi-sxtl (%vi-xtn (%vi-sub32 (%vi-dup32 100000) (%vi-dup32 7)))) 1))
(check "8 sxtl/xtn/sub32/umov32" (t8) (logand (let ((u (logand (- 100000 7) #xFFFF))) (if (>= u 32768) (- u 65536) u)) #xFFFFFFFF))   ; sxtl sign-extends
(defun t9 () (%vi-umov16 (%vi-sshr16 (%vi-shl16 (%vi-dup16 -300) 2) 3) 5))
(check "9 shl16/sshr16 signed value" (let ((u (t9))) (if (>= u 32768) (- u 65536) u)) (ash (- (* 300 4)) -3))

;; 10. trn1-8/trn2-8 (.8B) and trn1-32/trn2-32 (.2S): the 64-bit transposes; high half is zero
(defun t10 (a b dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) a b dst))
  (%vi-st8 dst 0 (%vi-trn1-8 (%vi-ld8 a 0) (%vi-ld8 b 0)))
  (%vi-st8 dst 8 (%vi-trn2-8 (%vi-ld8 a 0) (%vi-ld8 b 0)))
  (%vi-st8 dst 16 (%vi-trn1-32 (%vi-ld8 a 0) (%vi-ld8 b 0)))
  (%vi-st8 dst 24 (%vi-trn2-32 (%vi-ld8 a 0) (%vi-ld8 b 0))))
(let ((a (u8v 8)) (b (u8v 8)) (dst (u8v 32)))
  (dotimes (i 8) (setf (aref a i) (+ 10 i) (aref b i) (+ 20 i)))
  (t10 a b dst)
  (check "10 trn1-8/trn2-8/trn1-32/trn2-32" (bytes dst 32)
         '(10 20 12 22 14 24 16 26  11 21 13 23 15 25 17 27  10 11 12 13 20 21 22 23  14 15 16 17 24 25 26 27)))

;; 11. literal lane constants compile to MOVI / MVNI immediates (dup16 -1, -128, 200; dup8 7)
(defun t11 (dst)
  (declare (type (simple-array (unsigned-byte 8) (*)) dst))
  (%vi-st16 dst 0 (%vi-dup16 -1))
  (%vi-st16 dst 16 (%vi-add16 (%vi-dup16 -128) (%vi-dup16 200)))
  (%vi-st16 dst 32 (%vi-dup8 7)))
(let ((dst (u8v 48)))
  (t11 dst)
  (check "11 dup const -> movi/mvni" (list (s16-load dst 0) (s16-load dst 14) (s16-load dst 16) (s16-load dst 30) (aref dst 32) (aref dst 47))
         (list -1 -1 72 72 7 7)))

(format t "~%VI-PROBE ~a~%" (if (zerop *fails*) "ALL PASS" (format nil "~a FAILURES" *fails*)))
