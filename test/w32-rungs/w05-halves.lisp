;;;; w05-halves.lisp -- expect 1023
;;;;
;;; W32 rung: the boxing pieces -- w32-from-fix (two's complement, mod 2^32)
;;; and the hi16 / lo16 / join round trip at #x80000000, #xFFFFFFFF and the
;;; fixnum edge 2^29-1 -- plus w32-li of a word with bit 31 set and the three
;;; bitwise ops with not.
(defun probe ()
  (let ((m1 -1) (fmax 536870911) (h8 32768) (z 0) (ffff 65535) (f0f #x0F0F))
    (let ((c0 (%w32 (w32-hi16 (w32-from-fix m1))))
          (c1 (%w32 (w32-hi16 (w32-from-fix fmax))))
          (c2 (%w32 (w32-lo16 (w32-from-fix fmax))))
          (c3 (%w32 (w32-hi16 (w32-join h8 z))))
          (c4 (%w32 (w32-eq (w32-not (w32-join z z)) (w32-join ffff ffff))))
          (c5 (%w32 (w32-hi16 (w32-and (w32-join ffff z) (w32-join f0f f0f)))))
          (c6 (%w32 (w32-lo16 (w32-or (w32-join ffff z) (w32-join f0f f0f)))))
          (c7 (%w32 (w32-hi16 (w32-xor (w32-join ffff ffff) (w32-li #x12345678)))))
          (c8 (%w32 (w32-lo16 (w32-xor (w32-join ffff ffff) (w32-li #x12345678)))))
          (c9 (%w32 (w32-hi16 (w32-li #x80000000)))))
      (+ (if (= c0 65535) 1 0) (if (= c1 #x1FFF) 2 0) (if (= c2 65535) 4 0)
         (if (= c3 #x8000) 8 0) (if (= c4 1) 16 0) (if (= c5 #x0F0F) 32 0)
         (if (= c6 #x0F0F) 64 0) (if (= c7 #xEDCB) 128 0) (if (= c8 #xA987) 256 0)
         (if (= c9 #x8000) 512 0)))))
