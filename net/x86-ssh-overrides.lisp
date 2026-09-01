;;;; x86-ssh-overrides.lisp - Single-threaded SSH overrides for x86-64
;;;;
;;;; The MVM x64 translator emits NOP for YIELD, so actor scheduling
;;;; doesn't work. These overrides replace actor-based SSH with a
;;;; single-threaded model where net-actor-main runs directly.
;;;;
;;;; Extracted from aarch64-overrides.lisp (portable Lisp, not arch-specific).

;;; ============================================================
;;; Override Ed25519 field arithmetic for x64 MVM translator
;;; Original fe-mul uses 10-arg + and let* with 47 bindings.
;;; Original fe-to-bytes uses nested logior/logand/ash (3+ levels).
;;; Override fe-carry/fe-reduce which have 36+ let* bindings.
;;; ============================================================

;; Accumulate 10 products for one fe-mul output limb.
;; fa/ga arrays hold the coefficients for one h[i] computation.
;; Each array has 10 entries.
(defun fe-mul-sum10 (fa ga)
  (let ((s0 (* (aref fa 0) (aref ga 0))))
    (let ((s1 (+ s0 (* (aref fa 1) (aref ga 1)))))
      (let ((s2 (+ s1 (* (aref fa 2) (aref ga 2)))))
        (let ((s3 (+ s2 (* (aref fa 3) (aref ga 3)))))
          (let ((s4 (+ s3 (* (aref fa 4) (aref ga 4)))))
            (fe-mul-sum10b fa ga s4)))))))

(defun fe-mul-sum10b (fa ga acc)
  (let ((s5 (+ acc (* (aref fa 5) (aref ga 5)))))
    (let ((s6 (+ s5 (* (aref fa 6) (aref ga 6)))))
      (let ((s7 (+ s6 (* (aref fa 7) (aref ga 7)))))
        (let ((s8 (+ s7 (* (aref fa 8) (aref ga 8)))))
          (+ s8 (* (aref fa 9) (aref ga 9))))))))

;; Compute h[idx] of fe-mul. Reads f/g limbs and pre-computed values
;; from fv (f values: f0..f9,f1-2,f3-2,f5-2,f7-2,f9-2) and
;; gv (g values: g0..g9,g1-19..g9-19).
;; fv[0..9] = f0..f9, fv[10..14] = f1-2,f3-2,f5-2,f7-2,f9-2
;; gv[0..9] = g0..g9, gv[10..18] = g1-19..g9-19
(defun fe-mul-h-even (fv gv idx)
  ;; For h0,h2,h4,h6,h8: uses f0,f1-2,f2,f3-2,f4,f5-2,f6,f7-2,f8,f9-2
  (let ((fa (make-array 10))
        (ga (make-array 10)))
    (aset fa 0 (aref fv 0))
    (aset fa 1 (aref fv 10))
    (aset fa 2 (aref fv 2))
    (aset fa 3 (aref fv 11))
    (aset fa 4 (aref fv 4))
    (aset fa 5 (aref fv 12))
    (aset fa 6 (aref fv 6))
    (aset fa 7 (aref fv 13))
    (aset fa 8 (aref fv 8))
    (aset fa 9 (aref fv 14))
    (fe-mul-fill-ga-even ga gv idx)
    (fe-mul-sum10 fa ga)))

(defun fe-mul-fill-ga-even (ga gv idx)
  ;; Fill ga for even h[idx]: g[idx], g[idx-1], g[idx-2], ...
  ;; with wrap-around using g*19 versions
  (let ((hi (ash idx -1)))
    ;; ga[0] = g[idx]
    (aset ga 0 (aref gv idx))
    ;; For j=1..9, ga[j] = g[idx-j] or g*19[idx-j+10] if wrapped
    (fe-mul-fill-ga-loop ga gv idx 1)))

(defun fe-mul-fill-ga-loop (ga gv idx start)
  (let ((j start))
    (loop
      (when (>= j 10) (return 0))
      (let ((k (- idx j)))
        (if (>= k 0)
            (aset ga j (aref gv k))
            (aset ga j (aref gv (+ k 19)))))
      (setq j (+ j 1)))))

(defun fe-mul-h-odd (fv gv idx)
  ;; For h1,h3,h5,h7,h9: uses f0,f1,f2,f3,f4,f5,f6,f7,f8,f9
  (let ((fa (make-array 10))
        (ga (make-array 10)))
    (dotimes (i 10) (aset fa i (aref fv i)))
    (fe-mul-fill-ga-odd ga gv idx)
    (fe-mul-sum10 fa ga)))

(defun fe-mul-fill-ga-odd (ga gv idx)
  (aset ga 0 (aref gv idx))
  (fe-mul-fill-ga-loop ga gv idx 1))

;; Override fe-mul: compute via arrays to avoid deeply nested lets
(defun fe-mul (dst f g)
  (let ((fv (make-array 15))
        (gv (make-array 19)))
    ;; Load f limbs
    (aset fv 0 (buf-read-u32 f 0))
    (aset fv 1 (buf-read-u32 f 4))
    (aset fv 2 (buf-read-u32 f 8))
    (aset fv 3 (buf-read-u32 f 12))
    (aset fv 4 (buf-read-u32 f 16))
    (aset fv 5 (buf-read-u32 f 20))
    (aset fv 6 (buf-read-u32 f 24))
    (aset fv 7 (buf-read-u32 f 28))
    (aset fv 8 (buf-read-u32 f 32))
    (aset fv 9 (buf-read-u32 f 36))
    ;; Pre-doubled odd f limbs
    (aset fv 10 (* 2 (aref fv 1)))
    (aset fv 11 (* 2 (aref fv 3)))
    (aset fv 12 (* 2 (aref fv 5)))
    (aset fv 13 (* 2 (aref fv 7)))
    (aset fv 14 (* 2 (aref fv 9)))
    (fe-mul-load-g gv g)
    (fe-mul-compute dst fv gv))
  dst)

(defun fe-mul-load-g (gv g)
  ;; Load g limbs
  (aset gv 0 (buf-read-u32 g 0))
  (aset gv 1 (buf-read-u32 g 4))
  (aset gv 2 (buf-read-u32 g 8))
  (aset gv 3 (buf-read-u32 g 12))
  (aset gv 4 (buf-read-u32 g 16))
  (aset gv 5 (buf-read-u32 g 20))
  (aset gv 6 (buf-read-u32 g 24))
  (aset gv 7 (buf-read-u32 g 28))
  (aset gv 8 (buf-read-u32 g 32))
  (aset gv 9 (buf-read-u32 g 36))
  ;; g*19 versions: gv[10] = g1*19, gv[11] = g2*19, ..., gv[18] = g9*19
  (aset gv 10 (* 19 (aref gv 1)))
  (aset gv 11 (* 19 (aref gv 2)))
  (aset gv 12 (* 19 (aref gv 3)))
  (aset gv 13 (* 19 (aref gv 4)))
  (aset gv 14 (* 19 (aref gv 5)))
  (fe-mul-load-g2 gv))

(defun fe-mul-load-g2 (gv)
  (aset gv 15 (* 19 (aref gv 6)))
  (aset gv 16 (* 19 (aref gv 7)))
  (aset gv 17 (* 19 (aref gv 8)))
  (aset gv 18 (* 19 (aref gv 9))))

(defun fe-mul-compute (dst fv gv)
  ;; Compute h0..h9 and carry
  (let ((h (make-array 10)))
    (aset h 0 (fe-mul-h-even fv gv 0))
    (aset h 1 (fe-mul-h-odd fv gv 1))
    (aset h 2 (fe-mul-h-even fv gv 2))
    (aset h 3 (fe-mul-h-odd fv gv 3))
    (aset h 4 (fe-mul-h-even fv gv 4))
    (fe-mul-compute2 dst fv gv h)))

(defun fe-mul-compute2 (dst fv gv h)
  (aset h 5 (fe-mul-h-odd fv gv 5))
  (aset h 6 (fe-mul-h-even fv gv 6))
  (aset h 7 (fe-mul-h-odd fv gv 7))
  (aset h 8 (fe-mul-h-even fv gv 8))
  (aset h 9 (fe-mul-h-odd fv gv 9))
  ;; Carry propagation
  (fe-mul-carry-to dst h))

(defun fe-mul-carry-to (dst h)
  ;; Carry propagation: h0..h9 -> dst limbs
  (let ((c0 (ash (aref h 0) -26)))
    (let ((r0 (logand (aref h 0) #x3FFFFFF)))
      (let ((h1b (+ (aref h 1) c0)))
        (let ((c1 (ash h1b -25)))
          (let ((r1 (logand h1b #x1FFFFFF)))
            (let ((h2b (+ (aref h 2) c1)))
              (let ((c2 (ash h2b -26)))
                (let ((r2 (logand h2b #x3FFFFFF)))
                  (let ((h3b (+ (aref h 3) c2)))
                    (let ((c3 (ash h3b -25)))
                      (let ((r3 (logand h3b #x1FFFFFF)))
                        (let ((h4b (+ (aref h 4) c3)))
                          (let ((c4 (ash h4b -26)))
                            (let ((r4 (logand h4b #x3FFFFFF)))
                              (fe-mul-carry-to2 dst h c4 r0 r1 r2 r3 r4))))))))))))))))

(defun fe-mul-carry-to2 (dst h c4 r0 r1 r2 r3 r4)
  (let ((h5b (+ (aref h 5) c4)))
    (let ((c5 (ash h5b -25)))
      (let ((r5 (logand h5b #x1FFFFFF)))
        (let ((h6b (+ (aref h 6) c5)))
          (let ((c6 (ash h6b -26)))
            (let ((r6 (logand h6b #x3FFFFFF)))
              (let ((h7b (+ (aref h 7) c6)))
                (let ((c7 (ash h7b -25)))
                  (let ((r7 (logand h7b #x1FFFFFF)))
                    (let ((h8b (+ (aref h 8) c7)))
                      (let ((c8 (ash h8b -26)))
                        (let ((r8 (logand h8b #x3FFFFFF)))
                          (let ((h9b (+ (aref h 9) c8)))
                            (let ((c9 (ash h9b -25)))
                              (let ((r9 (logand h9b #x1FFFFFF)))
                                (fe-mul-carry-finish dst c9 r0 r1 r2 r3 r4 r5 r6 r7 r8 r9)))))))))))))))))

(defun fe-mul-carry-finish (dst c9 r0 r1 r2 r3 r4 r5 r6 r7 r8 r9)
  (let ((r0w (+ r0 (* c9 19))))
    (let ((c0b (ash r0w -26)))
      (let ((r0f (logand r0w #x3FFFFFF)))
        (let ((r1f (+ r1 c0b)))
          (buf-write-u32 dst 0 r0f)
          (buf-write-u32 dst 4 r1f)
          (buf-write-u32 dst 8 r2)
          (buf-write-u32 dst 12 r3)
          (buf-write-u32 dst 16 r4)
          (fe-mul-write-tail dst r5 r6 r7 r8 r9))))))

(defun fe-mul-write-tail (dst r5 r6 r7 r8 r9)
  (buf-write-u32 dst 20 r5)
  (buf-write-u32 dst 24 r6)
  (buf-write-u32 dst 28 r7)
  (buf-write-u32 dst 32 r8)
  (buf-write-u32 dst 36 r9))

;; Override fe-sq to use overridden fe-mul
(defun fe-sq (dst f)
  (fe-mul dst f f))

;; Override fe-carry: split let* into two halves
(defun fe-carry (h)
  (let ((v0 (buf-read-u32 h 0)))
    (let ((c0 (ash v0 -26)))
      (let ((r0 (logand v0 #x3FFFFFF)))
        (let ((v1 (+ (buf-read-u32 h 4) c0)))
          (let ((c1 (ash v1 -25)))
            (let ((r1 (logand v1 #x1FFFFFF)))
              (let ((v2 (+ (buf-read-u32 h 8) c1)))
                (let ((c2 (ash v2 -26)))
                  (let ((r2 (logand v2 #x3FFFFFF)))
                    (let ((v3 (+ (buf-read-u32 h 12) c2)))
                      (let ((c3 (ash v3 -25)))
                        (let ((r3 (logand v3 #x1FFFFFF)))
                          (let ((v4 (+ (buf-read-u32 h 16) c3)))
                            (let ((c4 (ash v4 -26)))
                              (let ((r4 (logand v4 #x3FFFFFF)))
                                (fe-carry-hi h c4 r0 r1 r2 r3 r4)))))))))))))))))

(defun fe-carry-hi (h c4 r0 r1 r2 r3 r4)
  (let ((v5 (+ (buf-read-u32 h 20) c4)))
    (let ((c5 (ash v5 -25)))
      (let ((r5 (logand v5 #x1FFFFFF)))
        (let ((v6 (+ (buf-read-u32 h 24) c5)))
          (let ((c6 (ash v6 -26)))
            (let ((r6 (logand v6 #x3FFFFFF)))
              (let ((v7 (+ (buf-read-u32 h 28) c6)))
                (let ((c7 (ash v7 -25)))
                  (let ((r7 (logand v7 #x1FFFFFF)))
                    (let ((v8 (+ (buf-read-u32 h 32) c7)))
                      (let ((c8 (ash v8 -26)))
                        (let ((r8 (logand v8 #x3FFFFFF)))
                          (let ((v9 (+ (buf-read-u32 h 36) c8)))
                            (let ((c9 (ash v9 -25)))
                              (let ((r9 (logand v9 #x1FFFFFF)))
                                (fe-carry-finish h c9 r0 r1 r2 r3 r4 r5 r6 r7 r8 r9)))))))))))))))))

(defun fe-carry-finish (h c9 r0 r1 r2 r3 r4 r5 r6 r7 r8 r9)
  (let ((r0w (+ r0 (* c9 19))))
    (let ((c0b (ash r0w -26)))
      (let ((r0f (logand r0w #x3FFFFFF)))
        (let ((r1f (+ r1 c0b)))
          (buf-write-u32 h 0 r0f)
          (buf-write-u32 h 4 r1f)
          (buf-write-u32 h 8 r2)
          (buf-write-u32 h 12 r3)
          (buf-write-u32 h 16 r4)
          (fe-carry-finish2 h r5 r6 r7 r8 r9))))))

(defun fe-carry-finish2 (h r5 r6 r7 r8 r9)
  (buf-write-u32 h 20 r5)
  (buf-write-u32 h 24 r6)
  (buf-write-u32 h 28 r7)
  (buf-write-u32 h 32 r8)
  (buf-write-u32 h 36 r9)
  h)

;; Override fe-to-bytes: avoid nested logior/logand/ash
;; Split into helper functions to keep sequential forms under 15
(defun fe-to-bytes-combine (a-shifted b-shifted)
  (let ((combined (logior a-shifted b-shifted)))
    (logand combined #xFF)))

(defun fe-to-bytes (fe)
  (fe-reduce fe)
  (let ((r (make-array 32))
        (l0 (buf-read-u32 fe 0))
        (l1 (buf-read-u32 fe 4))
        (l2 (buf-read-u32 fe 8))
        (l3 (buf-read-u32 fe 12))
        (l4 (buf-read-u32 fe 16)))
    (fe-to-bytes-lo r l0 l1 l2 l3 l4)
    (let ((l5 (buf-read-u32 fe 20))
          (l6 (buf-read-u32 fe 24))
          (l7 (buf-read-u32 fe 28))
          (l8 (buf-read-u32 fe 32))
          (l9 (buf-read-u32 fe 36)))
      (fe-to-bytes-hi r l5 l6 l7 l8 l9)
      r)))

(defun fe-to-bytes-lo (r l0 l1 l2 l3 l4)
  (aset r 0 (logand l0 #xFF))
  (aset r 1 (logand (ash l0 -8) #xFF))
  (aset r 2 (logand (ash l0 -16) #xFF))
  (aset r 3 (fe-to-bytes-combine (ash l0 -24) (ash l1 2)))
  (aset r 4 (logand (ash l1 -6) #xFF))
  (aset r 5 (logand (ash l1 -14) #xFF))
  (aset r 6 (fe-to-bytes-combine (ash l1 -22) (ash l2 3)))
  (aset r 7 (logand (ash l2 -5) #xFF))
  (aset r 8 (logand (ash l2 -13) #xFF))
  (aset r 9 (fe-to-bytes-combine (ash l2 -21) (ash l3 5)))
  (aset r 10 (logand (ash l3 -3) #xFF))
  (aset r 11 (logand (ash l3 -11) #xFF))
  (fe-to-bytes-lo2 r l3 l4))

(defun fe-to-bytes-lo2 (r l3 l4)
  (aset r 12 (fe-to-bytes-combine (ash l3 -19) (ash l4 6)))
  (aset r 13 (logand (ash l4 -2) #xFF))
  (aset r 14 (logand (ash l4 -10) #xFF))
  (aset r 15 (logand (ash l4 -18) #xFF)))

(defun fe-to-bytes-hi (r l5 l6 l7 l8 l9)
  (aset r 16 (logand l5 #xFF))
  (aset r 17 (logand (ash l5 -8) #xFF))
  (aset r 18 (logand (ash l5 -16) #xFF))
  (aset r 19 (fe-to-bytes-combine (ash l5 -24) (ash l6 1)))
  (aset r 20 (logand (ash l6 -7) #xFF))
  (aset r 21 (logand (ash l6 -15) #xFF))
  (aset r 22 (fe-to-bytes-combine (ash l6 -23) (ash l7 3)))
  (aset r 23 (logand (ash l7 -5) #xFF))
  (aset r 24 (logand (ash l7 -13) #xFF))
  (aset r 25 (fe-to-bytes-combine (ash l7 -21) (ash l8 4)))
  (aset r 26 (logand (ash l8 -4) #xFF))
  (aset r 27 (logand (ash l8 -12) #xFF))
  (fe-to-bytes-hi2 r l8 l9))

(defun fe-to-bytes-hi2 (r l8 l9)
  (aset r 28 (fe-to-bytes-combine (ash l8 -20) (ash l9 6)))
  (aset r 29 (logand (ash l9 -2) #xFF))
  (aset r 30 (logand (ash l9 -10) #xFF))
  (aset r 31 (logand (ash l9 -18) #xFF)))

;; Override fe-reduce: split 33-binding let* into helper functions
(defun fe-reduce (h)
  (fe-carry h)
  (fe-carry h)
  (fe-reduce-check h))

(defun fe-reduce-check (h)
  (let ((t0 (+ (buf-read-u32 h 0) 19)))
    (let ((c0 (ash t0 -26)))
      (let ((t0m (logand t0 #x3FFFFFF)))
        (let ((t1 (+ (buf-read-u32 h 4) c0)))
          (let ((c1 (ash t1 -25)))
            (let ((t1m (logand t1 #x1FFFFFF)))
              (let ((t2 (+ (buf-read-u32 h 8) c1)))
                (let ((c2 (ash t2 -26)))
                  (let ((t2m (logand t2 #x3FFFFFF)))
                    (let ((t3 (+ (buf-read-u32 h 12) c2)))
                      (let ((c3 (ash t3 -25)))
                        (let ((t3m (logand t3 #x1FFFFFF)))
                          (let ((t4 (+ (buf-read-u32 h 16) c3)))
                            (let ((c4 (ash t4 -26)))
                              (let ((t4m (logand t4 #x3FFFFFF)))
                                (fe-reduce-check2 h c4 t0m t1m t2m t3m t4m)))))))))))))))))

(defun fe-reduce-check2 (h c4 t0m t1m t2m t3m t4m)
  (let ((t5 (+ (buf-read-u32 h 20) c4)))
    (let ((c5 (ash t5 -25)))
      (let ((t5m (logand t5 #x1FFFFFF)))
        (let ((t6 (+ (buf-read-u32 h 24) c5)))
          (let ((c6 (ash t6 -26)))
            (let ((t6m (logand t6 #x3FFFFFF)))
              (let ((t7 (+ (buf-read-u32 h 28) c6)))
                (let ((c7 (ash t7 -25)))
                  (let ((t7m (logand t7 #x1FFFFFF)))
                    (let ((t8 (+ (buf-read-u32 h 32) c7)))
                      (let ((c8 (ash t8 -26)))
                        (let ((t8m (logand t8 #x3FFFFFF)))
                          (let ((t9 (+ (buf-read-u32 h 36) c8)))
                            (let ((c9 (ash t9 -25)))
                              (let ((t9m (logand t9 #x1FFFFFF)))
                                (fe-reduce-write h c9 t0m t1m t2m t3m t4m t5m t6m t7m t8m t9m)))))))))))))))))

(defun fe-reduce-write (h c9 t0m t1m t2m t3m t4m t5m t6m t7m t8m t9m)
  (when (not (zerop c9))
    (buf-write-u32 h 0 t0m)
    (buf-write-u32 h 4 t1m)
    (buf-write-u32 h 8 t2m)
    (buf-write-u32 h 12 t3m)
    (buf-write-u32 h 16 t4m)
    (buf-write-u32 h 20 t5m)
    (buf-write-u32 h 24 t6m)
    (buf-write-u32 h 28 t7m)
    (buf-write-u32 h 32 t8m)
    (buf-write-u32 h 36 t9m))
  h)

;;; ============================================================
;;; Override ssh-compute-exchange-hash: original has 24 nested lets
;;; (18+ nested lets may miscompile in MVM)
;;; Uses flat buffer approach from 32bit-overrides.lisp (max 8 nested lets)

;; Helper: write big-endian u32 to array at pos, return pos+4
(defun ssh-eh-write-u32 (buf pos val)
  (aset buf pos (logand (ash val -24) #xFF))
  (aset buf (+ pos 1) (logand (ash val -16) #xFF))
  (aset buf (+ pos 2) (logand (ash val -8) #xFF))
  (aset buf (+ pos 3) (logand val #xFF))
  (+ pos 4))

;; Helper: write SSH string (4-byte len + data from memory) to buf at pos
(defun ssh-eh-write-mem (buf pos src len)
  (let ((p (ssh-eh-write-u32 buf pos len)))
    (dotimes (i len)
      (aset buf (+ p i) (mem-ref (+ src i) :u8)))
    (+ p len)))

;; Helper: write SSH string (4-byte len + data from array) to buf at pos
(defun ssh-eh-write-arr (buf pos src len)
  (let ((p (ssh-eh-write-u32 buf pos len)))
    (dotimes (i len)
      (aset buf (+ p i) (aref src i)))
    (+ p len)))

(defun ssh-compute-exchange-hash (ssh cli-eph srv-eph shared-secret)
  (sha256-init)
  (let ((cb (- ssh #x20))
        (buf (make-array 2048))
        (pos 0))
    ;; V_C: client version string
    (let ((vc-len (mem-ref (+ ssh #x6D0) :u32)))
      (setq pos (ssh-eh-write-mem buf pos (+ ssh #x650) vc-len)))
    ;; V_S: server version "SSH-2.0-Modus_1.0" (17 bytes)
    (let ((vs (make-array 17)))
      (aset vs 0 83) (aset vs 1 83) (aset vs 2 72) (aset vs 3 45)
      (aset vs 4 50) (aset vs 5 46) (aset vs 6 48) (aset vs 7 45)
      (aset vs 8 77) (aset vs 9 111) (aset vs 10 100) (aset vs 11 117)
      (aset vs 12 115) (aset vs 13 95) (aset vs 14 49) (aset vs 15 46)
      (aset vs 16 48)
      (setq pos (ssh-eh-write-arr buf pos vs 17)))
    ;; I_C: client kexinit
    (let ((ic-len (mem-ref (+ ssh #x20) :u32)))
      (setq pos (ssh-eh-write-mem buf pos (+ cb #x1F00) ic-len)))
    ;; I_S: server kexinit
    (let ((is-len (mem-ref (+ ssh #x1C) :u32)))
      (setq pos (ssh-eh-write-mem buf pos (+ cb #x1700) is-len)))
    ;; K_S: host key blob
    (let ((hk-enc (ssh-encode-host-key ssh)))
      (setq pos (ssh-eh-write-arr buf pos hk-enc (array-length hk-enc))))
    ;; Q_C: client ephemeral public key
    (setq pos (ssh-eh-write-arr buf pos cli-eph 32))
    ;; Q_S: server ephemeral public key
    (setq pos (ssh-eh-write-arr buf pos srv-eph 32))
    ;; K: shared secret as mpint (raw bytes, not SSH string)
    (let ((k-mpint (ssh-make-mpint shared-secret)))
      (let ((klen (array-length k-mpint)))
        (dotimes (i klen)
          (aset buf (+ pos i) (aref k-mpint i)))
        (setq pos (+ pos klen))))
    ;; Trim buffer and hash
    (let ((input (make-array pos)))
      (dotimes (i pos)
        (aset input i (aref buf i)))
      (sha256 input))))

;;; ============================================================
;;; Pre-computed crypto
;;; ============================================================

;; Pre-compute Ed25519 host key derivatives for fast signing.
;; Stores clamped scalar s at state+0x680, prefix at state+0x6A0.
;; Must be called AFTER host key setup.
(defun pre-compute-host-sign ()
  (let ((state (e1000-state-base)))
    (sha512-init)
    ;; Load host private key from state+0x710
    (let ((privkey (make-array 32)))
      (dotimes (i 32)
        (aset privkey i (mem-ref (+ state (+ #x710 i)) :u8)))
      ;; SHA-512(privkey) -> 64-byte hash
      (let ((hash (sha512 privkey)))
        ;; Clamp first 32 bytes -> scalar s -> store at state+0x680
        (dotimes (i 32)
          (setf (mem-ref (+ state (+ #x680 i)) :u8) (aref hash i)))
        (setf (mem-ref (+ state #x680) :u8)
              (logand (mem-ref (+ state #x680) :u8) #xF8))
        (let ((b31 (mem-ref (+ state #x69F) :u8)))
          (setf (mem-ref (+ state #x69F) :u8)
                (logior (logand b31 #x7F) #x40)))
        ;; Second 32 bytes -> prefix -> store at state+0x6A0
        (dotimes (i 32)
          (setf (mem-ref (+ state (+ #x6A0 i)) :u8) (aref hash (+ i 32))))
        ;; Mark as pre-computed
        (setf (mem-ref (+ state #x6C0) :u32) 1)))))

;; Pre-compute X25519 server ephemeral key pair.
;; Stores private key at state+0x6C4, public key at state+0x6E4.
(defun pre-compute-server-eph (ssh)
  (let ((state (e1000-state-base)))
    ;; Generate random private key
    (let ((priv (make-array 32)))
      (dotimes (i 32) (aset priv i (ssh-random ssh)))
      ;; Store private key at state+0x6C4
      (dotimes (i 32)
        (setf (mem-ref (+ state (+ #x6C4 i)) :u8) (aref priv i)))
      ;; Compute and store public key at state+0x6E4
      (let ((pub (x25519-public-key priv)))
        (dotimes (i 32)
          (setf (mem-ref (+ state (+ #x6E4 i)) :u8) (aref pub i)))))))

;; Override ed25519-sign-fast: use full ed25519-sign (slow but correct)
;; Bypasses pre-computation to test if pre-compute-host-sign is the issue
(defun ed25519-sign-fast (message msg-len)
  (let ((state (e1000-state-base)))
    (let ((privkey (make-array 32)))
      (dotimes (i 32)
        (aset privkey i (mem-ref (+ state (+ #x710 i)) :u8)))
      (ed25519-sign privkey message msg-len))))

;; No-op usb-keepalive for x86 (no USB, E1000 NIC)
(defun usb-keepalive () 0)

;; Override ssh-use-default-key to hardcode known-correct public key
;; for zero private key (bypasses ed25519-public-key computation)
(defun ssh-use-default-key ()
  (let ((state (e1000-state-base)))
    ;; Zero private key → store at state+0x710
    (dotimes (i 32)
      (setf (mem-ref (+ state (+ #x710 i)) :u8) 0))
    ;; Known-correct public key for zero privkey: 3b6a27bc...
    (setf (mem-ref (+ state #x730) :u8) 59)
    (setf (mem-ref (+ state #x731) :u8) 106)
    (setf (mem-ref (+ state #x732) :u8) 39)
    (setf (mem-ref (+ state #x733) :u8) 188)
    (setf (mem-ref (+ state #x734) :u8) 206)
    (setf (mem-ref (+ state #x735) :u8) 182)
    (setf (mem-ref (+ state #x736) :u8) 164)
    (setf (mem-ref (+ state #x737) :u8) 45)
    (ssh-use-default-key2 state)))

(defun ssh-use-default-key2 (state)
  (setf (mem-ref (+ state #x738) :u8) 98)
  (setf (mem-ref (+ state #x739) :u8) 163)
  (setf (mem-ref (+ state #x73A) :u8) 168)
  (setf (mem-ref (+ state #x73B) :u8) 208)
  (setf (mem-ref (+ state #x73C) :u8) 42)
  (setf (mem-ref (+ state #x73D) :u8) 111)
  (setf (mem-ref (+ state #x73E) :u8) 13)
  (setf (mem-ref (+ state #x73F) :u8) 115)
  (setf (mem-ref (+ state #x740) :u8) 101)
  (setf (mem-ref (+ state #x741) :u8) 50)
  (ssh-use-default-key3 state))

(defun ssh-use-default-key3 (state)
  (setf (mem-ref (+ state #x742) :u8) 21)
  (setf (mem-ref (+ state #x743) :u8) 119)
  (setf (mem-ref (+ state #x744) :u8) 29)
  (setf (mem-ref (+ state #x745) :u8) 226)
  (setf (mem-ref (+ state #x746) :u8) 67)
  (setf (mem-ref (+ state #x747) :u8) 166)
  (setf (mem-ref (+ state #x748) :u8) 58)
  (setf (mem-ref (+ state #x749) :u8) 192)
  (setf (mem-ref (+ state #x74A) :u8) 72)
  (setf (mem-ref (+ state #x74B) :u8) 161)
  (ssh-use-default-key4 state))

(defun ssh-use-default-key4 (state)
  (setf (mem-ref (+ state #x74C) :u8) 139)
  (setf (mem-ref (+ state #x74D) :u8) 89)
  (setf (mem-ref (+ state #x74E) :u8) 218)
  (setf (mem-ref (+ state #x74F) :u8) 41)
  ;; Mark host key as set
  (setf (mem-ref (+ state #x624) :u32) 1))

;; Override ssh-buf-consume: original has 3-arg and 4-arg + patterns
(defun ssh-buf-consume (ssh n)
  (let ((buf-len (mem-ref (+ ssh #x6D4) :u32)))
    (let ((remaining (- buf-len n)))
      (when (> remaining 0)
        (let ((buf-base (+ ssh #x6D8)))
          (dotimes (i remaining)
            (setf (mem-ref (+ buf-base i) :u8)
                  (mem-ref (+ buf-base (+ n i)) :u8)))))
      (setf (mem-ref (+ ssh #x6D4) :u32) remaining)
      remaining)))

;; Override ssh-buf-to-array: original has 3-arg +
(defun ssh-buf-to-array (ssh len)
  (let ((arr (make-array len)))
    (let ((buf-base (+ ssh #x6D8)))
      (dotimes (i len)
        (aset arr i (mem-ref (+ buf-base i) :u8)))
      arr)))

;; Override ssh-copy-host-key: original has 3-arg +
(defun ssh-copy-host-key (conn)
  (let ((state (e1000-state-base)))
    (let ((ssh (conn-ssh conn)))
      ;; Copy private key (32 bytes) from state+0x710 to ssh+0x110
      (let ((src-priv (+ state #x710)))
        (let ((dst-priv (+ ssh #x110)))
          (dotimes (i 32)
            (setf (mem-ref (+ dst-priv i) :u8)
                  (mem-ref (+ src-priv i) :u8)))))
      ;; Copy public key (32 bytes) from state+0x730 to ssh+0x130
      (let ((src-pub (+ state #x730)))
        (let ((dst-pub (+ ssh #x130)))
          (dotimes (i 32)
            (setf (mem-ref (+ dst-pub i) :u8)
                  (mem-ref (+ src-pub i) :u8))))))))

;; Override ssh-receive-version: avoid 3-arg + and fix nil/0 check
(defun ssh-receive-version (ssh)
  (let ((got-version 0))
    (let ((tries 0))
      (loop
        (when (not (zerop got-version)) (return 1))
        (when (> tries 50) (return 0))
        (let ((msg (receive)))
          (when (zerop msg) (return 0)))
        (let ((blen (mem-ref (+ ssh #x6D4) :u32)))
          (when (> blen 8)
            (let ((buf-base (+ ssh #x6D8)))
              (when (eq (mem-ref buf-base :u8) 83)
                (when (eq (mem-ref (+ buf-base 1) :u8) 83)
                  (when (eq (mem-ref (+ buf-base 2) :u8) 72)
                    (let ((end 0))
                      (let ((i 3))
                        (loop
                          (when (not (zerop end)) (return ()))
                          (when (> i blen) (return ()))
                          (when (eq (mem-ref (+ buf-base i) :u8) 10)
                            (setq end i))
                          (setq i (+ i 1))))
                      (when (not (zerop end))
                        (let ((vlen end))
                          (let ((ver-base (+ ssh #x650)))
                            (when (eq (mem-ref (+ buf-base (- end 1)) :u8) 13)
                              (setq vlen (- end 1)))
                            (dotimes (i vlen)
                              (setf (mem-ref (+ ver-base i) :u8)
                                    (mem-ref (+ buf-base i) :u8)))
                            (setf (mem-ref (+ ssh #x6D0) :u32) vlen)
                            (ssh-buf-consume ssh (+ end 1))
                            (setq got-version 1)))))))))))
        (setq tries (+ tries 1))))))

;;; ============================================================
;;; Network overrides
;;; ============================================================

;; Override receive: poll E1000 directly instead of actor mailbox
(defun receive ()
  (io-delay)
  (let ((pkt-len (e1000-receive)))
    (if (zerop pkt-len)
        1
        (let ((buf (e1000-rx-buf)))
          (let ((et-hi (mem-ref (+ buf 12) :u8))
                (et-lo (mem-ref (+ buf 13) :u8)))
            (if (eq et-hi #x08)
                (if (eq et-lo #x06)
                    (let ((arp-op (buf-read-u16-mem buf 20)))
                      (when (eq arp-op 1)
                        (arp-reply buf)))
                    (when (eq et-lo 0)
                      ;; Learn source MAC as gateway MAC
                      (let ((st (e1000-state-base)))
                        (dotimes (m 6)
                          (setf (mem-ref (+ st (+ #x28 m)) :u8)
                                (mem-ref (+ buf (+ 6 m)) :u8))))
                      (let ((proto (mem-ref (+ buf 23) :u8)))
                        (if (eq proto 1)
                            (icmp-handle buf 14)
                            (when (eq proto 6)
                              (net-handle-tcp buf pkt-len))))))
                ()))
          1))))

;; Override ssh-server for single-threaded x86
(defun ssh-server (port)
  (ssh-seed-random)
  (ssh-init-strings)
  (init-gc-helper)
  (when (zerop (mem-ref (+ (e1000-state-base) #x624) :u32))
    (ssh-use-default-key))
  ;; Pre-compute Ed25519 signing derivatives (s, prefix)
  (pre-compute-host-sign)
  ;; SHA-256 test: hash("") should start with E3B0C442
  (sha256-test)
  ;; SHA-512 test: hash("") should start with CF83E135
  (sha512-test)
  (%serial-byte 83) (%serial-byte 83) (%serial-byte 72)
  (%serial-byte 58) (print-dec port) (%serial-byte 10)
  ;; Store listen port
  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) port)
  ;; Clear connection table
  (let ((i 0))
    (loop
      (when (>= i 4) (return 0))
      (setf (mem-ref (conn-base i) :u32) 0)
      (setq i (+ i 1))))
  ;; Pre-compute X25519 server ephemeral key pair for first connection
  (pre-compute-server-eph (conn-ssh 0))
  ;; Single-threaded: run network loop directly (never returns)
  (net-actor-main))

;; Override ssh-connection-handler: no actor-exit
(defun ssh-connection-handler (conn)
  (let ((ssh (conn-ssh conn))
        (cb (conn-base conn)))
    (ssh-handle-connection ssh)
    ;; Regenerate server ephemeral X25519 key pair for next connection
    (pre-compute-server-eph ssh)
    (tcp-close-conn cb)
    (conn-free conn)))

;; Override ssh-handle-connection for single-threaded operation
;; Matches AArch64 version closely - minimal sequential forms to avoid
;; MVM 25+ sequential forms miscompilation bug.
(defun ssh-handle-connection (ssh)
  (let ((cb (- ssh #x20)))
    (ssh-send-version ssh)
    (when (zerop (ssh-receive-version ssh))
      (return ()))
    (let ((kexinit (ssh-build-kexinit ssh)))
      (ssh-send-payload ssh kexinit (array-length kexinit)))
    (let ((cli-kex (ssh-receive-packet ssh 50000)))
      (when (zerop cli-kex) (return ()))
      (let ((cli-kex-payload (car cli-kex)))
        (when (not (eq (aref cli-kex-payload 0) 20)) (return ()))
        (ssh-mem-store (+ cb #x1F00) cli-kex-payload (cdr cli-kex))
        (setf (mem-ref (+ ssh #x20) :u32) (cdr cli-kex))
        (let ((kex-init (ssh-receive-packet ssh 50000)))
          (when (zerop kex-init) (return ()))
          (let ((kex-payload (car kex-init)))
            (when (not (eq (aref kex-payload 0) 30)) (return ()))
            (ssh-handle-kex ssh kex-payload (cdr kex-init))
            (ssh-send-newkeys ssh)
            (let ((nk (ssh-receive-packet ssh 50000)))
              (when (zerop nk) (return ()))
              (when (not (eq (aref (car nk) 0) 21)) (return ()))
              (ssh-derive-keys ssh)
              (ssh-message-loop ssh))))))))

;; Message dispatch - flat when chain
(defun ssh-dispatch-msg (ssh payload plen flag-addr)
  (let ((msg-type (aref payload 0)))
    (when (eq msg-type 5)
      (let ((svc-len (ssh-get-u32 payload 1))
            (svc (make-array 32)))
        (dotimes (i svc-len)
          (aset svc i (aref payload (+ 5 i))))
        (ssh-send-service-accept ssh svc svc-len)))
    (when (eq msg-type 50)
      (ssh-handle-userauth ssh payload plen))
    (when (eq msg-type 90)
      (let ((ctype-len (ssh-get-u32 payload 1)))
        (let ((cli-chan (ssh-get-u32 payload (+ 5 ctype-len))))
          (setf (mem-ref (+ ssh #x18) :u32) cli-chan)
          (setf (mem-ref (+ ssh #x14) :u32) 0)
          (ssh-send-channel-confirm ssh cli-chan 0))))
    (when (eq msg-type 98)
      (let ((rtype-len (ssh-get-u32 payload 5)))
        (let ((want-reply (aref payload (+ 9 rtype-len))))
          (when (not (zerop want-reply))
            (ssh-send-channel-success ssh
             (mem-ref (+ ssh #x18) :u32)))
          ;; "shell" -> send prompt
          (when (eq rtype-len 5)
            (when (eq (aref payload 9) 115)
              (ssh-send-prompt ssh)))
          ;; "exec" -> execute command
          (when (eq rtype-len 4)
            (when (eq (aref payload 9) 101)
              (let ((cmd-off (+ 10 rtype-len)))
                (let ((cmd-len (ssh-get-u32 payload cmd-off)))
                  (let ((cmd (make-array cmd-len)))
                    (dotimes (i cmd-len)
                      (aset cmd i (aref payload (+ cmd-off 4 i))))
                    (ssh-eval-line ssh cmd cmd-len)
                    ;; Send EOF + CLOSE after exec
                    (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
                      (let ((eof-msg (make-array 5)))
                        (aset eof-msg 0 96)
                        (ssh-put-u32 eof-msg 1 cli-chan)
                        (ssh-send-payload ssh eof-msg 5))
                      (let ((close-msg (make-array 5)))
                        (aset close-msg 0 97)
                        (ssh-put-u32 close-msg 1 cli-chan)
                        (ssh-send-payload ssh close-msg 5)))
                    (setf (mem-ref flag-addr :u32) 0)))))))))
    (when (eq msg-type 94)
      (ssh-handle-channel-data ssh payload plen))
    (when (eq msg-type 97)
      (setf (mem-ref flag-addr :u32) 0))
    (when (eq msg-type 1)
      (setf (mem-ref flag-addr :u32) 0))))

;; Message loop - polls until client disconnects
(defun ssh-message-loop (ssh)
  (let ((flag-addr (+ (conn-ssh 3) #x700)))
    (setf (mem-ref flag-addr :u32) 1)
    (loop
      (when (zerop (mem-ref flag-addr :u32)) (return ()))
      (let ((pkt (ssh-receive-packet ssh 50000)))
        (when pkt
          (ssh-dispatch-msg ssh (car pkt) (cdr pkt) flag-addr))))))

;; Override net-wait-ack: deliver piggybacked data on ACK+data packets
(defun net-wait-ack (conn)
  (let ((cb (conn-base conn))
        (acked 0)
        (tries 0))
    (loop
      (when (not (zerop acked)) (return 1))
      (when (> tries 500) (return 0))
      (io-delay)
      (let ((pkt-len (e1000-receive)))
        (when (not (zerop pkt-len))
          (let ((b2 (e1000-rx-buf)))
            (when (eq (mem-ref (+ b2 12) :u8) #x08)
              (when (eq (mem-ref (+ b2 13) :u8) 0)
                (when (eq (mem-ref (+ b2 23) :u8) 6)
                  (let ((f2 (mem-ref (+ b2 47) :u8)))
                    (when (eq (logand f2 #x10) #x10)
                      (setf (mem-ref cb :u32) 2)
                      (setq acked 1)
                      ;; Also deliver any data payload
                      (net-deliver-data conn b2 pkt-len f2)))))))))
      (setq tries (+ tries 1)))))

;; Override net-accept-connection: run handler directly
(defun net-accept-connection (src-ip src-port dst-port buf)
  (let ((conn (conn-alloc)))
    (when (not (= conn (- 0 1)))
      (conn-init conn dst-port src-port src-ip)
      (setf (mem-ref (+ (conn-base conn) #x1C) :u32) 0)
      (when (eq dst-port 80)
        (setf (mem-ref (+ (conn-base conn) #x1C) :u32) 1))
      (let ((their-seq (buf-read-u32-mem buf 38)))
        (setf (mem-ref (+ (conn-base conn) #x014) :u32) (+ their-seq 1)))
      ;; Init recv buffer BEFORE net-wait-ack
      (let ((ssh (conn-ssh conn)))
        (setf (mem-ref (+ ssh #x6D4) :u32) 0))
      (tcp-send-segment-conn (conn-base conn) 18 (make-array 0) 0)
      (net-wait-ack conn)
      (when (eq (mem-ref (conn-base conn) :u32) 2)
        (if (eq (mem-ref (+ (conn-base conn) #x1C) :u32) 1)
            (http-connection-handler conn)
            (progn
              (ssh-copy-host-key conn)
              (let ((ssh (conn-ssh conn)))
                (setf (mem-ref ssh :u32) 0)
                (setf (mem-ref (+ ssh #x04) :u32) 0)
                (setf (mem-ref (+ ssh #x08) :u32) 0)
                (setf (mem-ref (+ ssh #x0C) :u32) 0)
                (setf (mem-ref (+ ssh #x10) :u32) 0)
                (setf (mem-ref (+ ssh #x28) :u32) 0)
                (setf (mem-ref (+ ssh #x150) :u32) 0)
                (setf (mem-ref (+ ssh #x154) :u32) 0)
                (setf (mem-ref (+ ssh #x2C) :u32)
                      (+ src-port (* src-ip 7))))
              (setf (mem-ref (+ (ssh-ipc-base) #x60448) :u32) conn)
              (ssh-connection-handler conn)))))))
