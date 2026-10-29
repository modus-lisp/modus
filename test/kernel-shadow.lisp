;;;; test/kernel-shadow.lisp -- a CL-USER DEFUN that shares a kernel function's
;;;; NAME is a different symbol, so it defines its own function and the kernel
;;;; keeps calling its own; redefining the kernel's OWN symbol (MODUS-INT::NAME)
;;;; still reaches the kernel's callers.  Builder packages step 4, phase B.
;;;;   modus --script test/kernel-shadow.lisp   => KERNEL-SHADOW PASS
;;;; FORMAT (kernel) calls %FORMAT-IMPL (kernel, homed in MODUS-INT).  Before
;;;; phase B a CL-USER (defun %format-impl ...) rewired FORMAT itself.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(check "before" (format nil "~a" 42) "42")
(check "CL-USER's name is not the kernel's symbol"
       (eq (quote %format-impl) (find-symbol "%FORMAT-IMPL" "MODUS-INT")) nil)

;; 1. a CL-USER definition of a kernel internal's NAME
(defun %format-impl (&rest a) (declare (ignore a)) "SHADOWED")
(check "the CL-USER function is CL-USER's" (%format-impl 1 2) "SHADOWED")
(check "FORMAT still uses the kernel's" (format nil "~a" 42) "42")
(dotimes (i 300) (format nil "~a" i))
(check "and still after the hot JIT" (format nil "~a" 7) "7")

;; 2. the kernel's OWN symbol: the deliberate fix-it-from-the-REPL path
(defvar *orig* (fdefinition (quote modus-int::%format-impl)))
(defvar *hits* 0)
(setf (fdefinition (quote modus-int::%format-impl))
      (lambda (&rest a) (setq *hits* (+ *hits* 1)) (apply *orig* a)))
(format nil "~a" 1)
(setf (fdefinition (quote modus-int::%format-impl)) *orig*)
(check "MODUS-INT::NAME reaches the kernel's callers" (> *hits* 0) t)
(check "restored" (format nil "~a" 42) "42")

;; 3. an internal NOT redefined here is undefined under its bare name, as in CL
(check "a bare kernel internal is not CL-USER's" (fboundp (quote %gc-read64)) nil)
(check "MODUS-INT::NAME is" (fboundp (quote modus-int::%gc-read64)) t)

;; 4. the public API is MODUS's, and MODUS:NAME is the kernel's symbol
(check "CL-USER uses MODUS" (eq (quote sys-exit) (quote modus:sys-exit)) t)
(check "MODUS:SYS-EXIT is the kernel's"
       (eq (quote modus:sys-exit) (find-symbol "SYS-EXIT" "MODUS-INT")) t)

(format t "KERNEL-SHADOW ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
