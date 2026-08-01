;;;; hash.lisp — Dual-FNV-1a symbol hashing
;;;;
;;;; *** NOT THE LIVE DEFINITION. ***  Nothing references MODUS.HASH; every
;;;; build goes through one of the copies that derive their width from the
;;;; TARGET (mvm/compiler.lisp, mvm/cross.lisp, mvm/prelude.lisp,
;;;; mvm/build-mvm.lisp, mvm/build-compiler-test.lisp — all four constants come
;;;; from mvm/target.lisp's WIDTH-CONSTANTS-SOURCE).  This file is kept in step
;;;; ONLY so it cannot become a second, silently divergent algorithm; if you
;;;; change the hash, change it here too or delete this file.
;;;;
;;;; The width is NOT a free choice: the same name is hashed on both sides of
;;;; the build boundary (baked literal vs recomputed in-image) and the two must
;;;; be the SAME NUMBER, so the hash has to fit the target's fixnum.  See the
;;;; +NAME-HASH-BITS+ block in mvm/target.lisp for the full failure chain.

(defpackage :modus.hash
  (:use :cl)
  (:export #:compute-name-hash
           #:normalize-name
           #:name-eq
           #:+name-hash-bits+))

(in-package :modus.hash)

(defparameter +name-hash-bits+ 60
  "Hash width.  60 matches a 62-bit fixnum tower; the live copies derive
   (min 60 (1- +fixnum-bits+)) from the target instead of hardcoding it.")

(defun compute-name-hash (name-string)
  "Compute dual-FNV-1a hash for a name string, +NAME-HASH-BITS+ wide.

   FIXNUM-SAFE 16-BIT STATE.  The streams used to be held as full 32-bit FNV
   values, masked with #xFFFFFFFF.  A 32-bit value is a BIGNUM on i386's 30-bit
   tower, so every character of every name allocated through the generic bignum
   engine — on the hottest path in the compiler.
   They are held in 16 bits instead, and the result is BIT-IDENTICAL, not
   approximately so.  Two facts make that exact:
     (1) FNV-1a is  h <- (h XOR c) * p  mod 2^32, and mod 2^16 that reads
         h_lo <- (h_lo XOR c_lo) * (p mod 2^16)  mod 2^16 — the low half never
         depends on the high half, so it is a CLOSED system;
     (2) the 29-bit result takes only the LOW 15 bits of stream 1 and the LOW
         14 bits of stream 2, so the high halves were never read.
   Hence the low-16 constants: 16777619 mod 2^16 = 403, 805306457 mod 2^16 =
   89, and the offset bases 2166136261 / 3735928559 mod 2^16 = #x9DC5 / #xBEEF.
   Verified over 93,842 distinct names from the tree: ZERO differences from the
   32-bit form, and the largest intermediate is 2^25 — comfortably a fixnum on
   a 30-bit tower (max 2^30-1).

   THIS REDUCTION IS ONLY VALID WHILE THE OUTPUT TAKES <= 16 LOW BITS FROM EACH
   STREAM.  Widening +NAME-HASH-BITS+ past 32 would start reading high halves
   that are no longer computed, and the streams must go back to 32-bit (and
   then bignum-free arithmetic has to be solved some other way).
   SET-TARGET-FIXNUM-BITS asserts the bound so this cannot rot silently."
  (let* ((name (string-upcase (string name-string)))
         (lo-bits (floor +name-hash-bits+ 2))
         (hi-bits (- +name-hash-bits+ lo-bits))
         (h1 #x9DC5) (h2 #xBEEF))
    (loop for c across name
          do (let ((cc (logand (char-code c) #xFFFF)))
               (setq h1 (logand (* (logxor h1 cc) 403) #xFFFF))
               (setq h2 (logand (* (logxor h2 cc) 89) #xFFFF))))
    (let ((combined (logior (ash (logand h1 (1- (ash 1 hi-bits))) lo-bits)
                            (logand h2 (1- (ash 1 lo-bits))))))
      (if (zerop combined) 1 combined))))

(defun normalize-name (sym)
  "Convert a symbol to its name hash for comparison."
  (if (integerp sym)
      sym
      (compute-name-hash (symbol-name sym))))

(defun name-eq (sym name-string)
  "Check if SYM's name matches NAME-STRING via hash comparison."
  (and (symbolp sym)
       (= (compute-name-hash (symbol-name sym))
          (compute-name-hash name-string))))
