;;;; r33-big-object.lisp -- expect 42
;;;;
;;; A CONSTANT-SIZE OBJECT BIGGER THAN AN ADD-IMMEDIATE.
;;;
;;; The compiler inlines constant sizes up to 65535 slots as `:alloc-obj', and
;;; every back end bumps the allocation pointer by the object's byte size.
;;; RISC-V did it with ADDI (12-bit signed immediate) and PowerPC with ADDI
;;; (16-bit): a 4096-character stream buffer is 16400 bytes on RV32, ADDI
;;; wrapped it to 16, and the stream's own conses were then allocated INSIDE
;;; its buffer -- every refill's byte count showed up as character 7 of the
;;; file, which is how the RV32 CL image read `(print 1)' as `(print <Tab>)'.
;;;
;;; 16384 slots is chosen so the WRAPPED bump is +16 everywhere: 65552 bytes
;;; at 4-byte words, 131088 at 8, both = 16 mod 4096 (ADDI 12) and mod 65536
;;; (ADDI 16).  A size that wraps NEGATIVE puts the conses below the object and
;;; hides the bug -- 9000 did exactly that, and passed on the unfixed tree.
;;; Zero the first 16 slots, THEN cons: a wrapped bump lands the conses on them.
;;;   28 + 7 + 7 + (sum of 16 zeroed slots = 0) = 42

(defun fill0 (s i)
  (when (< i 16) (%prim-aset s i 0) (fill0 s (+ i 1))))
(defun sum16 (s i acc)
  (if (< i 16) (sum16 s (+ i 1) (+ acc (%prim-aref s i))) acc))

(defun probe ()
  (let ((s (%make-string-array 16384)))
    (fill0 s 0)
    (let ((c (cons 7 7)) (d (cons 7 7)))
      (+ 28 (car c) (car d) (sum16 s 0 0)))))
