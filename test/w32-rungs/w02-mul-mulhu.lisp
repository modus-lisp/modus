;;;; w02-mul-mulhu.lisp -- expect 63
;;;;
;;; W32 rung: w32-mul + w32-mulhu, the low and high halves of the full 64-bit
;;; unsigned product, across #xFFFFFFFF * #xFFFFFFFF = #xFFFFFFFE00000001 and
;;; #x10000 * #x10000 = #x1_00000000.  On 68000 these go through a helper; a
;;; port that truncates the high half (ppc32's MUL26HI did) fails bits 2-3.
(defun probe ()
  (let ((ffff 65535) (z 0) (one 1))
    (let ((c0 (%w32 (w32-lo16 (w32-mul (w32-join ffff ffff) (w32-join ffff ffff)))))
          (c1 (%w32 (w32-hi16 (w32-mul (w32-join ffff ffff) (w32-join ffff ffff)))))
          (c2 (%w32 (w32-hi16 (w32-mulhu (w32-join ffff ffff) (w32-join ffff ffff)))))
          (c3 (%w32 (w32-lo16 (w32-mulhu (w32-join ffff ffff) (w32-join ffff ffff)))))
          (c4 (%w32 (w32-eq (w32-mul (w32-join one z) (w32-join one z)) (w32-li 0))))
          (c5 (%w32 (w32-eq (w32-mulhu (w32-join one z) (w32-join one z)) (w32-li 1)))))
      (+ (if (= c0 1) 1 0) (if (= c1 0) 2 0) (if (= c2 65535) 4 0)
         (if (= c3 65534) 8 0) (if (= c4 1) 16 0) (if (= c5 1) 32 0)))))
