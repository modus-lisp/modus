;;;; test/hot-jit.lisp -- a runtime DEFUN goes native once it is called often
;;;; enough, without JIT-EAGER, and nothing about calling it changes.
;;;;
;;;;   ./modus --script test/hot-jit.lisp
;;;;
;;;; Before mvm-eval.lisp's %MVM-MAKE-DEFUN-TRAMPOLINE a DEFUN stayed an
;;;; interpreter trampoline forever unless JIT-EAGER ran: hotness was decided
;;;; per toplevel form, and a DEFUN form runs once.  A DEFUN goes native after
;;;; 64 calls, or once its calls have spent 10 ms interpreted.  The checks run
;;;; across that point, so the same call is made before, at and after it.

;; This file tests the HOT promotion itself, so its DEFUNs must start
;; interpreted: no batch JIT of the script's own DEFUNs (cl-eval.lisp).
(setq modus-int::*jit-script-defs* nil)

(defvar *checks* 0)
(defvar *fails* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (unless (equal got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a~%   got  ~s~%   want ~s~%" name got want)))

(defun native-p (nm)
  (if (or (modus-int::%jit-fn-native-p nm)
          (modus-int::%jit-fn-native-p (concatenate 'string "COMMON-LISP-USER::" nm)))
      t nil))

;;; 1. multiple values, every call around the threshold
(defun three (x) (values x (* 2 x) (list x)))
(chk "three starts interpreted" (native-p "THREE") nil)
(let ((bad 0))
  (dotimes (i 200)
    (unless (equal (multiple-value-list (three i)) (list i (* 2 i) (list i)))
      (setq bad (+ bad 1))))
  (chk "three: all 200 calls return 3 values" bad 0))
(chk "three is native after 200 calls" (native-p "THREE") t)

;;; 2. callee first: MID's 64th call promotes MID, and LEAF -- called only by
;;; MID, 63 times by then -- goes native with it rather than staying behind a
;;; bridge.
(defun leaf (x) (* x 3))
(defun mid (x) (+ (leaf x) 1))
(let ((sum 0))
  (dotimes (i 64) (setq sum (+ sum (mid i))))
  (chk "mid sum" sum (+ 64 (* 3 (/ (* 63 64) 2)))))
(chk "mid native" (native-p "MID") t)
(chk "leaf native with its caller" (native-p "LEAF") t)

;;; 3. a trampoline object taken before promotion keeps working (forwarded)
(defun held (x) (values (+ x 100) :second))
(defparameter *held* (symbol-function 'held))
(dotimes (i 100) (held i))
(chk "held native" (native-p "HELD") t)
(chk "old object still answers, both values"
     (multiple-value-list (funcall *held* 5)) (list 105 :second))
(chk "old object through APPLY" (apply *held* (list 7)) 107)

;;; 4. redefinition after promotion is seen by callers
(defun redef (x) (+ x 1))
(defun calls-redef (x) (redef x))
(dotimes (i 100) (calls-redef i))
(chk "calls-redef native" (native-p "CALLS-REDEF") t)
(defun redef (x) (+ x 1000))
(chk "caller sees the redefinition" (calls-redef 1) 1001)
(dotimes (i 100) (calls-redef i))
(chk "and after the new definition is hot" (calls-redef 2) 1002)

;;; 5. the time budget, and recursion.  A self-call is a call inside the
;;; function's own bytecode, so it never reaches the trampoline: one deep
;;; (FIB 20) is ONE call.  But it spends far more than %JIT-HOT-NS interpreted,
;;; so FIB is built as soon as that call returns -- the next outside call, and
;;; its whole recursion, run native.  A cheap function called a few times is
;;; not built.
(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(chk "fib 20, one expensive outside call" (fib 20) 6765)
(chk "fib native after it" (native-p "FIB") t)
(chk "fib 25 native" (fib 25) 75025)
(defun cheap (x) (+ x 1))
(dotimes (i 10) (cheap i))
(chk "cheap: 10 calls, stays interpreted" (native-p "CHEAP") nil)

;;; 6. a non-local exit through a function promoted during the loop
(defun thrower (x) (if (> x 90) (throw :out x) x))
(chk "throw through the hot function"
     (catch :out (dotimes (i 200) (thrower i)) :no-throw) 91)
(chk "thrower native" (native-p "THROWER") t)

;;; 7. a condition from native code after promotion
(defun picky (x) (if (eql x 150) (error "picky ~a" x) x))
(chk "error caught after promotion"
     (handler-case (progn (dotimes (i 200) (picky i)) :none)
       (error (c) (princ-to-string c)))
     "picky 150")

;;; 8. a generic function defined over a DEFUN keeps the DEFUN's trampoline as its
;;; fallback method.  That trampoline getting hot must neither forward to the GF
;;; (which falls back to the trampoline: a loop until the stack is gone) nor
;;; install the DEFUN's native code over the GF.  Both orders: the GF arriving
;;; before the DEFUN is hot, and after.
(defun gf-early (x) (* x 2))
(dotimes (i 5) (gf-early i))                        ; not hot yet
(defmethod gf-early ((x string)) (concatenate 'string x x))
(let ((ok t))
  (dotimes (i 200) (unless (eql (gf-early i) (* i 2)) (setq ok nil)))
  (chk "GF over a cold DEFUN: the fallback stays right across promotion" ok t))
(chk "and the GF's own method still runs" (gf-early "ab") "abab")
(defun gf-late (x) (+ x 100))
(dotimes (i 100) (gf-late i))                       ; hot: native
(defmethod gf-late ((x string)) (length x))
(chk "GF over a hot DEFUN: the fallback" (gf-late 1) 101)
(chk "GF over a hot DEFUN: the method" (gf-late "abc") 3)
(let ((ok t))
  (dotimes (i 200) (unless (and (eql (gf-late i) (+ i 100)) (eql (gf-late "xy") 2)) (setq ok nil)))
  (chk "and stays right, with JIT-EAGER after it" (progn (jit-eager) ok) t))
(chk "the GF survived JIT-EAGER" (gf-late "abcd") 4)

;;; 8b. a (SETF FDEFINITION) wrapper that calls the function object it replaced
;;; (secp256k1-fast's threadsafe.lisp does this to every entry point).  The
;;; original trampoline is no longer its name's definition, so it must not be
;;; forwarded BY NAME (that is the wrapper: a loop) -- but it must still go
;;; native, to its OWN code: it used to stay interpreted forever.
(defun forwarded-p (nm)
  (let ((f (or (gethash nm modus-int::*jit-trampoline-forwards*)
               (gethash (concatenate 'string "COMMON-LISP-USER::" nm) modus-int::*jit-trampoline-forwards*))))
    (and f (car (cdr f)) (eql (logand (%val->word (car (cdr f))) 15) 3))))
(defun wrapped (x) (* x 3))
(defvar *wrapper-calls* 0)
(let ((inner (fdefinition 'wrapped)))
  (setf (fdefinition 'wrapped) (lambda (x) (setq *wrapper-calls* (+ *wrapper-calls* 1)) (funcall inner x))))
(chk "wrapped original starts interpreted" (forwarded-p "WRAPPED") nil)
(let ((ok t))
  (dotimes (i 200) (unless (eql (wrapped i) (* 3 i)) (setq ok nil)))
  (chk "wrapped: right across the original's promotion" ok t))
(chk "the original went native, to its own code" (forwarded-p "WRAPPED") t)
(chk "and the wrapper is still the definition, still running"
     (list (native-p "WRAPPED") *wrapper-calls* (wrapped 7) *wrapper-calls*)
     (list nil 200 21 201))
(jit-eager)
(chk "JIT-EAGER leaves the wrapper in place" (list (wrapped 1) *wrapper-calls*) (list 3 202))
(chk "a GF's fallback trampoline also forwards to its own code" (forwarded-p "GF-EARLY") t)

;;; 9. *JIT-HOT-CALLS* 0 turns it off for DEFUNs installed afterwards
(setq *jit-hot-calls* 0)
(defun cold (x) (+ x 1))
(dotimes (i 500) (cold i))
(chk "threshold 0: stays interpreted" (native-p "COLD") nil)
(setq *jit-hot-calls* nil)

(format t "~&HOT-JIT: ~a checks, ~a failed  (promoted ~a, failed ~a)~%"
        *checks* *fails* modus-int::*jit-hot-promoted* modus-int::*jit-hot-failed*)
(sys-exit (if (zerop *fails*) 0 1))
