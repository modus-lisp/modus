;;;; r35-rest-hundred.lisp -- expect 5050
;;;;
;;; &REST WITH A HUNDRED ARGUMENTS.  r29 is eight; this is past the 32 words the
;;; 32-bit back ends' #x0530 copy used to stop at (i386, riscv, ppc, 68k and
;;; arm32 all capped there, against x64/aarch64's 120), so arguments 33..100
;;; reached the &rest prologue as whatever the frame held.  The copy is a loop
;;; capped at 121 on every back end now: +APPLY-SPREAD-MAX+ positional words and
;;; the tail list of a longer call (+REST-TAIL-NARGS+, mvm/compiler.lisp).
;;;
;;; Through a variable for r29's reason: a known &rest callee gets its list
;;; built in the caller.  1..100 summed: a truncated or gapped list cannot make
;;; 5050 (stopping at 32 gives 528).
(defun filler (x) (+ x x x))

(defun sum-all (&rest xs)
  (let ((s 0))
    (loop
      (when (null xs) (return s))
      (setq s (+ s (car xs)))
      (setq xs (cdr xs)))))

(defun probe ()
  (let ((f #'sum-all))
    (funcall f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100)))
