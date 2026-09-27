;;;; r30-float-bits.lisp -- expect 42
;;;;
;;; A DOUBLE WHOSE LOW MANTISSA BITS ARE NOT ZERO, through box and unbox.
;;;
;;; r18-float computes (48-8)*2/4+22: every intermediate is a small integer, so
;;; the low 32 bits of every double it builds are ZERO.  A boxed double is four
;;; 16-bit chunks, and a back end that stores them in the wrong order, drops one,
;;; or misplaces them in memory (RV32 moves doubles by writing the chunks into a
;;; stack scratch with SH and loading it with FLD) passes r18 anyway.
;;;
;;; Each term below puts a single bit into a different chunk of the fraction and
;;; then recovers it EXACTLY -- every value is a dyadic rational, so no rounding
;;; is involved and no tolerance is needed:
;;;   1 + 2^-10  sets fraction bit 42  (chunk 47..32)
;;;   1 + 2^-25  sets fraction bit 27  (chunk 31..16)
;;;   1 + 2^-50  sets fraction bit  2  (chunk 15..0)
;;; ((1 + 2^-n) - 1) * 2^n = 1 for each, times 14, summed: 42.  Lose or misplace
;;; any chunk and that term is 0 or garbage.  The top chunk (sign, exponent and
;;; fraction bits 51..48) carries the leading 1 of every value, so it is covered
;;; too.  2^n is built by multiplying 2^5 and 2^10, both exact, so no integer
;;; wider than a 32-bit fixnum is needed.

(defun pow2 (base-exp times)            ; (2^base-exp)^times as a double
  (let ((b (%float-from-int (if (= base-exp 5) 32 1024)))
        (acc (%float-from-int 1))
        (i 0))
    (loop
      (when (= i times) (return acc))
      (setq acc (%float-mul acc b))
      (setq i (+ i 1)))))

(defun recover (scale)                  ; ((1 + 1/scale) - 1) * scale  => 1.0
  (let* ((one (%float-from-int 1))
         (v (%float-add one (%float-div one scale))))
    (%float-mul (%float-sub v one) scale)))

(defun probe ()
  (%float-to-int
    (%float-mul
      (%float-add (recover (pow2 10 1))         ; 2^10
                  (%float-add (recover (pow2 5 5))      ; 2^25
                              (recover (pow2 10 5))))   ; 2^50
      (%float-from-int 14))))
