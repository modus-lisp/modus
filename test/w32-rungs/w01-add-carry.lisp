;;;; w01-add-carry.lisp -- expect 31
;;;;
;;; W32 rung (docs/w32.md, M2): w32-add + w32-ltu across the 2^32 wrap -- the
;;; pair a 64-bit accumulator built from two words needs (carry = sum ltu a).
;;; Also w32-sub's wrap and the hi16/lo16 that read results out.  The answer is
;;; a bitmask, one bit per check, so a wrong value says WHICH check failed.
;;; W32-capable ports only (riscv32, arm32, 68k hosted; i386 bare).
(defun probe ()
  (let ((ffff 65535) (z 0) (one 1) (h8 32768))
    (let ((c0 (%w32 (w32-eq (w32-add (w32-join ffff ffff) (w32-join z one)) (w32-li 0))))
          (c1 (%w32 (w32-ltu (w32-add (w32-join ffff ffff) (w32-join z one)) (w32-join ffff ffff))))
          (c2 (%w32 (w32-ltu (w32-add (w32-join z one) (w32-join z one)) (w32-join z one))))
          (c3 (%w32 (w32-lo16 (w32-add (w32-join h8 z) (w32-join h8 one)))))
          (c4 (%w32 (w32-hi16 (w32-sub (w32-join z z) (w32-join z one))))))
      (+ (if (= c0 1) 1 0) (if (= c1 1) 2 0) (if (= c2 0) 4 0)
         (if (= c3 1) 8 0) (if (= c4 65535) 16 0)))))
