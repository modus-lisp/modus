;;;; interp-wide-word.lisp -- interpreted loops whose range checks make WIDE words.
;;;;
;;;; Run: ./modus --script test/interp-wide-word.lisp   (any CLI; the subject is
;;;; the 30-bit tower: i386, arm32, RV32, 68k)
;;;;
;;;; compile-mul guards its fast path by shifting each operand left and back
;;;; (SHL 16/21/11, SAR, CMP); on the 30-bit tower the shifted word of a fixnum
;;;; >= 2^14..2^19 is past the fixnum range.  The interpreter stored it as the
;;;; BIGNUM w/2 and the following SAR shifted that bignum's heap POINTER: the
;;;; loop faulted (MEMORY-FAULT-ERROR) whenever the bignum's address had bit 21
;;;; set, so where it died moved with the heap -- `(* i 7)' at i = 35242 in one
;;;; run, 30000 in another.  300,000 iterations allocate enough to pass every
;;;; address bit both ways.  Expected values are exact arithmetic (SBCL).

(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form want)
  `(let ((got (handler-case ,form (error (e) (list :error (type-of e))))))
     (setq *checks* (+ *checks* 1))
     (unless (equal got ,want)
       (setq *fails* (+ *fails* 1))
       (format t "FAIL ~A: ~S, want ~S~%" ,name got ,want))))

(defun mul-lit (n) (let ((s 0)) (dotimes (i n s) (setq s (* i 7)))))
(defun mul-lit-rev (n) (let ((s 0)) (dotimes (i n s) (setq s (* 7 i)))))
(defun mul-var (n) (let ((s 0) (k 7)) (dotimes (i n s) (setq s (* i k)))))
(defun mul-sum (n) (let ((s 0)) (dotimes (i n s) (setq s (logand (+ s (* i 7)) #xFFFFF)))))
(defun mul-neg (n) (let ((s 0)) (dotimes (i n s) (setq s (* (- i) 13)))))
(defun ash-loop (n) (let ((s 0)) (dotimes (i n s) (setq s (logand (+ s (ash i 9)) #x3FFFFFF)))))

(check "(* i 7)"         (mul-lit 300000)     2099993)
(check "(* 7 i)"         (mul-lit-rev 300000) 2099993)
(check "(* i k)"         (mul-var 300000)     2099993)
(check "(+ s (* i 7))"   (mul-sum 300000)     (let ((s 0)) (dotimes (i 300000 s) (setq s (logand (+ s (* i 7)) #xFFFFF)))))
(check "(* (- i) 13)"    (mul-neg 300000)     -3899987)
(check "(ash i 9)"       (ash-loop 300000)    (let ((s 0)) (dotimes (i 300000 s) (setq s (logand (+ s (ash i 9)) #x3FFFFFF)))))
(check "(ash 35241 21)"  (ash 35241 21)       73905733632)
(check "(ash -35241 21)" (ash -35241 21)      -73905733632)

(format t "interp-wide-word: ~D of ~D passed~%" (- *checks* *fails*) *checks*)
