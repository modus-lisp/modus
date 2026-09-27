;;;; r28-object-predicates.lisp -- expect 42
;;;;
;;; INTEGERP AND STRINGP ON A HEAP OBJECT.  The shared compiler recognises a
;;; fixnum by its low bit alone (compile-integerp is `test x, 1'), because
;;; fixnums are value<<1 and every x64 pointer tag is ODD: cons 1, function 3,
;;; object 9.  It recognises a string or array by comparing OBJ-TAG with a baked
;;; +TAG-OBJECT+ of 9 and OBJ-SUBTAG with a TAGGED subtag constant.
;;;
;;; RISC-V tagged objects 2 -- even -- and returned OBJ-SUBTAG raw, so on RV64
;;; every object was an INTEGER and nothing was a STRING.  In the real CL image
;;; SYMBOL-NAME took its gensym-integer branch for a symbol, and CONCATENATE
;;; rejected two well-formed one-character strings as non-sequences.
;;;
;;; Twenty-seven rungs never asked a type predicate about an object: r14-string
;;; builds and reads a string, but reads it with operations that already know
;;; what it is.
;;;
;;; RV32 still tags objects 2 (its 8-byte granule cannot fit 9) and is expected
;;; to FAIL this rung until that is solved -- a named gap, not a regression.

(defun probe ()
  (let ((s "ab"))
    (+ (if (integerp s) 0 20)           ; a string is not an integer
       (if (stringp s) 10 0)            ; ... it is a string
       (if (stringp 7) 0 8)             ; a fixnum is not a string
       (if (integerp 5) 4 0))))         ; ... it is an integer
