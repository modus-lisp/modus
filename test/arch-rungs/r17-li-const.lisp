;;;; r17-li-const.lisp -- expect 42
;;;;
;;; :LI-CONST — a reference into the CONSTANT POOL, whose address is not known
;;; until the image is assembled, so the translator emits a placeholder and
;;; cross.lisp patches it afterwards.  Three back ends had three different
;;; placeholder shapes (x64 a MOVABS immediate, i386 a MOV r32,imm32, AArch64 a
;;; MOVZ+MOVK quad) and four had none at all.
;;;
;;; A STRING LITERAL OVER 255 CHARACTERS is what reaches it: :obj-set encodes its
;;; slot index as an imm8, so the ordinary char-by-char path silently wraps at
;;; 256 (index 256 writes slot 0) and long strings route through the pool
;;; instead.  The census found 44 such sites in the real CL image.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe () (- (%prim-array-length "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA") 258))
