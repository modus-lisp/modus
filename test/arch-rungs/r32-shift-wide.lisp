;;;; r32-shift-wide.lisp -- expect 42
;;;;
;;; AN IMMEDIATE SHIFT AS WIDE AS THE REGISTER.
;;;
;;; compile-ash inlines every constant RIGHT shift as `:sar N', and N can reach
;;; or pass the word width: (ash x -32) is how mvm-emit-u64 takes the high word
;;; of every 64-bit MVM immediate.  On a 32-bit port the shift field cannot hold
;;; 32 -- RISC-V's SRAI shamt and PowerPC's SRAWI SH are 5 bits, and arm32 was
;;; masking with (logand amount 31) -- so the shift silently became a shift by
;;; ZERO and returned X.  On RV32 that duplicated the low word of every
;;; immediate the in-image compiler wrote into the high word, which is why
;;; character literals and quoted constants loaded garbage while integer
;;; literals (emitted as two halves) were fine.
;;;
;;; A wide arithmetic right shift is the sign fill:
;;;   (ash 12345 -32) = 0,  (ash -12345 -32) = -1,  (ash -5 -40) = -1
;;;   0 + 10*-1 + -1 + 53 = 42
;;; Arguments, so nothing can be folded at compile time.

(defun sh32 (x) (ash x -32))
(defun sh40 (x) (ash x -40))

(defun probe ()
  (+ (sh32 12345) (* 10 (sh32 -12345)) (sh40 -5) 53))
