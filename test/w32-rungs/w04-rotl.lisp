;;;; w04-rotl.lisp -- expect 31
;;;;
;;; W32 rung: w32-rotl by 1, 16, 31 and 0 of #x80000001.  31 is rotate-RIGHT
;;; by one, the form the compiler uses for rotr (32-n), and 0 must be the
;;; identity (RV32 has no rotate: shifts by 32 - 0 are not encodable).
(defun probe ()
  (let ((h8 32768) (one 1))
    (let ((c0 (%w32 (w32-eq (w32-rotl (w32-join h8 one) 1) (w32-li 3))))
          (c1 (%w32 (w32-hi16 (w32-rotl (w32-join h8 one) 16))))
          (c2 (%w32 (w32-lo16 (w32-rotl (w32-join h8 one) 16))))
          (c3 (%w32 (w32-eq (w32-rotl (w32-join h8 one) 31) (w32-li #xC0000000))))
          (c4 (%w32 (w32-eq (w32-rotl (w32-join h8 one) 0) (w32-join h8 one)))))
      (+ (if (= c0 1) 1 0) (if (= c1 1) 2 0) (if (= c2 #x8000) 4 0)
         (if (= c3 1) 8 0) (if (= c4 1) 16 0)))))
