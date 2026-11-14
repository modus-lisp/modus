;;;; w06-load-store.lisp -- expect 15
;;;;
;;; W32 rung: w32-store + w32-load, in the CPU's byte order on both orders
;;; (68k and ppc32 are big-endian).  The words go to multiple-values extra
;;; slots, +MV-VALUES-ADDR+ + 80 = #x100000E8 on every W32-capable port (only
;;; riscv64 and bare ppc32 move the MV slots): ordinary RAM, idle inside a probe.
;;;
;;; A CLEAN image's arithmetic is inline only below ~2^28, and its :u32 MEM-REF
;;; promotes through runtime helpers it does not have (both measured on the
;;; base tree too), so every address is a LITERAL and the stored bytes are read
;;; back one at a time with :u8, checked against +TARGET-BIG-ENDIAN+.
(defun probe ()
  (let ((a #x100000E8) (b #x100000EC) (h #x1122) (l #x3344) (h8 32768) (one 1))
    (%w32 (w32-store a 0 (w32-join h l)))
    (%w32 (w32-store b 0 (w32-join h8 one)))
    (let* ((be (= +target-big-endian+ 1))
           (b0 (mem-ref #x100000E8 :u8)) (b1 (mem-ref #x100000E9 :u8))
           (b2 (mem-ref #x100000EA :u8)) (b3 (mem-ref #x100000EB :u8))
           (c0 (if be
                   (and (= b0 #x11) (= b1 #x22) (= b2 #x33) (= b3 #x44))
                   (and (= b0 #x44) (= b1 #x33) (= b2 #x22) (= b3 #x11))))
           (c1 (%w32 (w32-eq (w32-load a 0) (w32-join h l))))
           (c2 (%w32 (w32-hi16 (w32-load b 0))))
           (c3 (= (mem-ref (if be #x100000EC #x100000EF) :u8) #x80)))
      (+ (if c0 1 0) (if (= c1 1) 2 0) (if (= c2 #x8000) 4 0) (if c3 8 0)))))
