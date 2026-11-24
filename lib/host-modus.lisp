;;;; host-modus.lisp -- what the build scripts ask of their host Lisp, on a MODUS host.
;;;;
;;;; The build scripts were written for SBCL and say SB-EXT:POSIX-GETENV, SB-EXT:EXIT and
;;;; SB-EXT:*POSIX-ARGV*.  A full modus CLI has an SB-EXT shim (net/sb-sys-shim.lisp); the
;;;; self-hosting image a bootstrap runs on (modus-sh) is slimmer and has none.  Here are
;;;; the few the build needs, over the kernel's own functions, made only when the host has
;;;; no SB-EXT of its own.  lib/load-mvm.lisp loads this first on a non-SBCL host.

(unless (find-package "SB-EXT")
  (make-package "SB-EXT" :use '("COMMON-LISP"))
  (let ((p (find-package "SB-EXT")))
    (dolist (n '("POSIX-GETENV" "EXIT" "*POSIX-ARGV*" "RUN-PROGRAM" "PROCESS-EXIT-CODE"
                 "WITHOUT-PACKAGE-LOCKS"))
      (export (intern n p) p)))
  (setf (fdefinition (find-symbol "POSIX-GETENV" "SB-EXT"))
        (lambda (name) (modus-int::%cli-getenv name)))
  (setf (fdefinition (find-symbol "EXIT" "SB-EXT"))
        (lambda (&key (code 0) abort) (declare (ignore abort)) (modus-int::sys-exit code)))
  ;; No processes to run on this host: a caller that needs one is told so, and one that
  ;; can do without (image-config's git) has a portable path of its own.
  (setf (fdefinition (find-symbol "RUN-PROGRAM" "SB-EXT"))
        (lambda (&rest args) (error "RUN-PROGRAM is not available on a modus host: ~S" args)))
  (setf (fdefinition (find-symbol "PROCESS-EXIT-CODE" "SB-EXT")) (lambda (p) (declare (ignore p)) 1))
  (setf (macro-function (find-symbol "WITHOUT-PACKAGE-LOCKS" "SB-EXT"))
        (lambda (form env) (declare (ignore env)) (cons 'progn (cdr form))))
  (proclaim (list 'special (find-symbol "*POSIX-ARGV*" "SB-EXT")))
  (setf (symbol-value (find-symbol "*POSIX-ARGV*" "SB-EXT"))
        (ignore-errors (modus-int::%cli-collect-argv))))
