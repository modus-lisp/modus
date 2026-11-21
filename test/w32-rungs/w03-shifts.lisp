;;;; w03-shifts.lisp -- expect 255
;;;;
;;; W32 rung: w32-shr vs w32-sar of a word with bit 31 SET (#x80000010) -- the
;;; pair that differs exactly in the sign fill -- plus w32-shl and the signed /
;;; unsigned compares, which disagree on the same word.
(defun probe ()
  (let ((h8 32768) (sixteen 16) (z 0) (one 1))
    (let ((c0 (%w32 (w32-hi16 (w32-shr (w32-join h8 sixteen) 4))))
          (c1 (%w32 (w32-lo16 (w32-shr (w32-join h8 sixteen) 4))))
          (c2 (%w32 (w32-hi16 (w32-sar (w32-join h8 sixteen) 4))))
          (c3 (%w32 (w32-lo16 (w32-shl (w32-join h8 sixteen) 1))))
          (c4 (%w32 (w32-eq (w32-sar (w32-join h8 sixteen) 31) (w32-li #xFFFFFFFF))))
          (c5 (%w32 (w32-eq (w32-shr (w32-join h8 sixteen) 31) (w32-join z one))))
          (c6 (%w32 (w32-lts (w32-join h8 sixteen) (w32-join z one))))
          (c7 (%w32 (w32-ltu (w32-join h8 sixteen) (w32-join z one)))))
      (+ (if (= c0 #x0800) 1 0) (if (= c1 1) 2 0) (if (= c2 #xF800) 4 0)
         (if (= c3 #x20) 8 0) (if (= c4 1) 16 0) (if (= c5 1) 32 0)
         (if (= c6 1) 64 0) (if (= c7 0) 128 0)))))
