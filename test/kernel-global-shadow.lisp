;;;; test/kernel-global-shadow.lisp -- a CL-USER global that shares a kernel
;;;; global's NAME is a different variable: defining, setting or binding it
;;;; leaves the kernel's alone, and MODUS-INT::NAME still reaches the kernel's.
;;;;   modus --script test/kernel-global-shadow.lisp   => KERNEL-GLOBAL-SHADOW PASS
;;;; *HA-RSIZE* (net/hosted-actors.lisp) is a kernel global; *TAR-BLOCK-SIZE*
;;;; (lib/install-tarball.lisp) is one with a value the kernel reads.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defvar *kernel-tbs* modus-int::*tar-block-size*)
(check "a kernel global is known by name"
       (and (gethash "*TAR-BLOCK-SIZE*" modus-int::*kernel-global-names*) t) t)
(check "CL-USER's name is not the kernel's symbol"
       (eq (quote *tar-block-size*) (find-symbol "*TAR-BLOCK-SIZE*" "MODUS-INT")) nil)

;; 1. DEFVAR / SETQ / LET of the CL-USER symbol
(defvar *tar-block-size* :mine)
(check "CL-USER's DEFVAR makes its own variable" *tar-block-size* :mine)
(check "the kernel's keeps its value" modus-int::*tar-block-size* *kernel-tbs*)
(setq *tar-block-size* :set)
(check "SETQ sets CL-USER's" *tar-block-size* :set)
(check "and not the kernel's" modus-int::*tar-block-size* *kernel-tbs*)
(let ((*tar-block-size* :bound))
  (check "LET binds CL-USER's" (symbol-value (quote *tar-block-size*)) :bound)
  (check "and not the kernel's" modus-int::*tar-block-size* *kernel-tbs*))
(check "SYMBOL-VALUE agrees with compiled reads" (symbol-value (quote *tar-block-size*)) :set)
(check "BOUNDP" (boundp (quote *tar-block-size*)) t)

;; 2. the kernel's own symbol: deliberate access works
(let ((modus-int::*tar-block-size* 4096))
  (check "MODUS-INT::NAME binds the kernel's" (symbol-value (quote modus-int::*tar-block-size*)) 4096))
(check "and restores it" modus-int::*tar-block-size* *kernel-tbs*)

;; 3. MODUS's exported settings ARE the kernel's
(check "*JIT-HOT-ONLY* is MODUS's" (eq (quote *jit-hot-only*) (quote modus-int::*jit-hot-only*)) t)
(let ((*jit-hot-only* :x))
  (check "binding it binds the kernel's" modus-int::*jit-hot-only* :x))

(format t "KERNEL-GLOBAL-SHADOW ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
