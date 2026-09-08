;;;; build-abcl-driver.lisp — drive a Modus build under ABCL at compiled speed.
;;;;
;;;; ABCL's `load` leaves defuns INTERPRETED, and the Modus compiler is far too
;;;; slow interpreted (the 5 MB blob-check + image compile take many minutes to
;;;; hours).  So this driver: (1) loads the MVM system, (2) JVM-compiles every
;;;; MODUS* function once (~8 s), (3) installs a `load` wrapper that skips the
;;;; build script's re-load of load-mvm (which would re-interpret everything),
;;;; then (4) drives the build script form-by-form, recompiling once more just
;;;; before the top-level (build-image …) form.
;;;;
;;;; The build script is $MODUS_ABCL_BUILD_SCRIPT (default build-generic-cli).
;;;; Run with MODUS_GLOBAL_CHECK=warn (build-abcl.sh sets it) — see the note in
;;;; lib/abcl-host-compat.lisp about the host-side check-source-parses sweep.

(load "lib/abcl-host-compat.lisp")

(defun %compile-modus (&optional (tag ""))
  (let ((ok 0) (fail 0) (t0 (get-internal-real-time)))
    (handler-bind ((warning #'muffle-warning))
      (dolist (p (list-all-packages))
        (let ((pn (package-name p)))
          (when (and (>= (length pn) 5) (string= (subseq pn 0 5) "MODUS"))
            (do-symbols (s p)
              (when (and (eq (symbol-package s) p) (fboundp s)
                         (not (macro-function s))
                         (not (compiled-function-p (symbol-function s))))
                (handler-case (progn (compile s) (incf ok))
                  (error () (incf fail)))))))))
    (format t "~&[abcl-fast]~A JVM-compiled ok=~D fail=~D in ~,1F s~%" tag ok fail
            (/ (- (get-internal-real-time) t0) internal-time-units-per-second))
    (finish-output)))

;; 1. Load + compile the whole MVM system.
(handler-case
    (handler-bind ((warning #'muffle-warning)) (load "lib/load-mvm.lisp"))
  (error (e) (format t "~&LOAD-MVM ERROR: ~A~%" e) (ext:quit :status 2)))
(%compile-modus)

;; (read-all-forms-with-locations used to be O(n^2) and was overridden here;
;; it is now O(n) in mvm/cross.lisp itself, so no host-side override is needed.)

;; 2. Skip the build script's re-load of load-mvm (already loaded + compiled).
(let ((real #'cl:load))
  (setf (symbol-function 'cl:load)
        (lambda (f &rest args)
          (if (search "load-mvm" (namestring (pathname f)))
              t
              (apply real f args)))))

;; 3. Drive the build script form-by-form; recompile just before build-image.
(defun %build-image-form-p (form)
  (and (consp form) (eq (car form) 'cl:let)
       (consp (cadr form)) (consp (caadr form))
       (let ((v (cadr (caadr form))))
         (and (consp v) (symbolp (car v))
              (string= (symbol-name (car v)) "BUILD-IMAGE")))))

(let* ((script (or (ext:getenv "MODUS_ABCL_BUILD_SCRIPT")
                   "mvm/build-generic-cli.lisp"))
       (path (merge-pathnames script *default-pathname-defaults*))
       (*load-truename* (truename path))
       (*load-pathname* path)
       (*package* (find-package :cl-user)))
  (handler-bind ((warning #'muffle-warning))
    (with-open-file (s path)
      (loop for form = (read s nil :eof)
            until (eq form :eof)
            do (when (%build-image-form-p form) (%compile-modus "(pre-build)"))
               (eval form)))))
(ext:quit :status 0)
