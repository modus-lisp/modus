;;;; gauntlet.lisp — drive Modus's runtime EVAL through asdf.lisp.
;;;;
;;;; Run: /tmp/modus vendor/asdf/gauntlet.lisp
;;;;
;;;; Reads vendor/asdf/asdf.lisp one toplevel form at a time and evals
;;;; each inside handler-case.  One pass surveys the whole landscape:
;;;;   FAILFORM <n> head=<op> — eval of form n signaled
;;;;   INPKG <name> @ <n>     — section progress marker
;;;;   READ-ERROR after <n>   — reader died; cannot resync, stop
;;;; Failures cascade (a failed defmacro breaks its users), so fix
;;;; top-down: the FIRST failures are the real ones.

(%install-runtime-cl-macros)

(defvar *g-form-n* 0)
(defvar *g-fail-n* 0)
(defvar *g-tick* 0)

(let ((s (open "/home/claude/modus/vendor/asdf/asdf.lisp" :direction :input)))
  (loop
    (let ((form (handler-case (read s nil :%gauntlet-eof)
                  (t (c) :%gauntlet-readerr))))
      (cond
        ((eq form :%gauntlet-eof) (return))
        ((eq form :%gauntlet-readerr)
         (write-string-serial "READ-ERROR after form ")
         (print-dec *g-form-n*)
         (terpri)
         (return))
        (t
         (setq *g-form-n* (+ *g-form-n* 1))
         ;; Progress marker on package switches.
         (when (and (consp form) (symbolp (car form))
                    (string= (symbol-name (car form)) "IN-PACKAGE"))
           (write-string-serial "INPKG ")
           (write-object (cadr form))
           (write-string-serial " @ ")
           (print-dec *g-form-n*)
           (terpri))
         ;; Sparse tick so a hang is locatable.
         (setq *g-tick* (+ *g-tick* 1))
         (when (>= *g-tick* 50)
           (setq *g-tick* 0)
           (write-string-serial ";; tick ")
           (print-dec *g-form-n*)
           (terpri))
         (handler-case (eval form)
           (t (c)
              (setq *g-fail-n* (+ *g-fail-n* 1))
              (write-string-serial "FAILFORM ")
              (print-dec *g-form-n*)
              (write-string-serial " head=")
              (write-object (if (consp form)
                                (if (and (consp (cdr form)) (symbolp (cadr form)))
                                    (list (car form) (cadr form))
                                    (car form))
                                form))
              (terpri)))))))
  (close s))

(write-string-serial "GAUNTLET DONE forms=")
(print-dec *g-form-n*)
(write-string-serial " fails=")
(print-dec *g-fail-n*)
(terpri)
