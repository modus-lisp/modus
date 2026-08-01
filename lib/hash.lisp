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
   Two independent FNV-1a-32 hashes, split as evenly as the width allows."
  (let* ((name (string-upcase (string name-string)))
         (lo-bits (floor +name-hash-bits+ 2))
         (hi-bits (- +name-hash-bits+ lo-bits))
         (h1 2166136261) (h2 3735928559))
    (loop for c across name
          do (setq h1 (logand (* (logxor h1 (char-code c)) 16777619) #xFFFFFFFF))
             (setq h2 (logand (* (logxor h2 (char-code c)) 805306457) #xFFFFFFFF)))
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
