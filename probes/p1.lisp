(defun say (k v) (princ (concatenate 'string k "=" (princ-to-string v))) (terpri) (finish-output))
(defun pkgname () (package-name *package*))
(say "P0.start" (pkgname))
;; 1. is a LET of *package* dynamic (visible to a callee)?
(say "P1.dyn" (let ((*package* (find-package "KEYWORD"))) (pkgname)))
(say "P2.after" (pkgname))
;; 3. escape-safety of let-special
(defun esc () (let ((*package* (find-package "KEYWORD"))) (throw 'lf-tag 1)))
(say "P3.throw" (catch 'lf-tag (esc)))
(say "P4.after-throw" (pkgname))
;; 5. unwind-protect + setq form
(defun esc2 () (let ((sv *package*)) (unwind-protect (progn (setq *package* (find-package "KEYWORD")) (throw 'lf-tag2 2)) (setq *package* sv))))
(say "P5.throw2" (catch 'lf-tag2 (esc2)))
(say "P6.after-throw2" (pkgname))
;; 7. error escape through handler-case
(defun esc3 () (let ((sv *package*)) (unwind-protect (progn (setq *package* (find-package "KEYWORD")) (error "boom")) (setq *package* sv))))
(say "P7.err" (handler-case (esc3) (t (c) :CAUGHT)))
(say "P8.after-err" (pkgname))
(defun esc4 () (let ((*package* (find-package "KEYWORD"))) (error "boom")))
(say "P9.err-let" (handler-case (esc4) (t (c) :CAUGHT)))
(say "P10.after-err-let" (pkgname))
(say "LF-END" "p1")
