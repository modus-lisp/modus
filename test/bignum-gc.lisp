;;;; test/bignum-gc.lisp -- a BIG bignum (>= 3 limbs: slot 1 points at its limb
;;;; array) held only by a global must survive collections that reuse its old
;;;; from-space.  The collectors skipped bignum #x30 as a raw leaf, so the limb
;;;; array was never forwarded: aarch64 (native) and gc.lisp until 2026-10-10,
;;;; x64 until 9d0790c.  Symptom: natrium's *P25519* after a tarball install on
;;;; the Pi -- INTEGERP true, INTEGER-LENGTH never returning.
;;;;   modus --script test/bignum-gc.lisp      prints BIGNUM-GC PASS / FAIL
;;;; Reproduced on the bare Pi image under QEMU (old collector: INTEGER-LENGTH
;;;; -62, slot 1 not an array); see docs/reel-on-zero/zlinux for the driver.
(defparameter *bg-p* (- (ash 1 255) 19))
(defparameter *bg-list* (list (ash 3 200) (- (ash 1 300)) (* 7 (ash 1 130))))
;; churn 1.2 semispaces a round: the stale limb array survives until allocation
;; reaches its address, so a small churn passes on a broken collector (it did)
(defun bg-churn () (dotimes (k (floor (* 6 (%core-space-size)) (* 5 144))) (make-array 16)))
(dotimes (i 4) (bg-churn) (%gc-force))
(let ((ok (and (integerp *bg-p*)
               (= (integer-length *bg-p*) 255)
               (= (logcount *bg-p*) 253)                ; 2^255-19 = 0b111..1101101
               (= (mod *bg-p* 1000000007) (mod (- (ash 1 255) 19) 1000000007))
               (= (integer-length (first *bg-list*)) 202)
               (= (integer-length (second *bg-list*)) 300)
               (= (third *bg-list*) (* 7 (ash 1 130))))))
  (format t "~&BIGNUM-GC ~:[FAIL~;PASS~] (epoch ~a)~%" ok (%gc-epoch)))
