;;;; test/handler-bind-escape.lisp -- a handler-bind handler that exits by
;;;; RETURN-FROM must not inhibit the handlers of the NEXT signal.
;;;;
;;;;   ./modus --script test/handler-bind-escape.lisp     ; exit 0 = PASS
;;;;
;;;; %SIGNAL-CONDITION raised *handler-bind-effective-skip* around a handler and
;;;; restored it only on normal return; the lazy heal rewinds it only when the
;;;; handler-bind stack is empty, which under LOAD it never is.  So the second
;;;; of these escaped, and rt's DO-ENTRY (exactly this shape) lost every
;;;; erroring test after the first in the real ansi-test.  Also checks CLHS
;;;; inhibition still holds: a handler re-signalling does not re-enter itself.

(defun hbe-try (thunk)
  (block aborted
    (handler-bind ((error (lambda (c) (return-from aborted (list :caught (princ-to-string c))))))
      (funcall thunk))))

(defvar *hbe-fail* 0)
(defun hbe-check (name got want)
  (unless (equal got want)
    (setq *hbe-fail* (+ *hbe-fail* 1))
    (format t "~&FAIL ~a: got ~s want ~s~%" name got want)))

(hbe-check "first"  (hbe-try (lambda () (error "a"))) '(:caught "a"))
(hbe-check "second" (hbe-try (lambda () (error "b"))) '(:caught "b"))
(hbe-check "third"  (hbe-try (lambda () (error "c"))) '(:caught "c"))
;; Inhibition (CLHS 9.1.4.1): while a handler runs, it and the handlers
;; established inside its binding are not active; outer ones are.
(defvar *hbe-log* nil)
(block out
  (handler-bind ((error (lambda (c) (push :outer *hbe-log*) (return-from out nil))))
    (handler-bind ((error (lambda (c) (push :inner *hbe-log*) (error "again"))))
      (error "first"))))
(hbe-check "inhibition" (reverse *hbe-log*) '(:inner :outer))

(format t "~&~a (~d failures)~%" (if (zerop *hbe-fail*) "PASS" "FAIL") *hbe-fail*)
(sys-exit (if (zerop *hbe-fail*) 0 1))
