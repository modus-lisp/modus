;;;; test/handler-case-order.lisp -- CLHS 9.1.4: handlers are searched in
;;;; DYNAMIC order, and HANDLER-CASE is a handler too.
;;;;
;;;;   ./modus --script test/handler-case-order.lisp     ; exit 0 = PASS
;;;;
;;;; Before the handler-case barriers, an OUTER HANDLER-BIND ran before an INNER
;;;; HANDLER-CASE (so rt's DO-ENTRY aborted every ansi-test whose own
;;;; handler-case should have caught its error), and SIGNAL / WARN never
;;;; transferred to a handler-case at all.

(define-condition hco-note (condition) ())
(defvar *hco-fail* 0)
(defun chk (name got want)
  (unless (equal got want) (setq *hco-fail* (+ *hco-fail* 1)))
  (format t "~&~a ~a got=~s~%" (if (equal got want) "ok  " "FAIL") name got))

(chk "inner-hc-beats-outer-hb"
     (block o (handler-bind ((error (lambda (c) (return-from o :outer-hb))))
                (handler-case (error "x") (error () :inner-hc))))
     :inner-hc)
(chk "signal-inner-hc"
     (block o (handler-bind ((error (lambda (c) (return-from o :outer-hb))))
                (handler-case (signal 'simple-error) (error () :inner-hc))))
     :inner-hc)
(chk "inner-hb-beats-hc"
     (block o (handler-bind ((error (lambda (c) (return-from o :outer-hb))))
                (handler-case (handler-bind ((error (lambda (c) (return-from o :inner-hb))))
                                (error "x"))
                  (error () :hc))))
     :inner-hb)
(chk "signal->hc" (handler-case (progn (signal 'hco-note) :fell) (hco-note () :caught)) :caught)
(chk "signal-unhandled" (handler-case (progn (signal 'hco-note) :fell) (error () :err)) :fell)
(chk "warn->hc" (handler-case (progn (warn "w") :fell) (warning () :caught)) :caught)
(chk "mv" (multiple-value-list (handler-case (values 1 2 3) (error () :e))) '(1 2 3))
(chk "no-error" (handler-case (values 4 5) (error () :e) (:no-error (a b) (list a b))) '(4 5))
(chk "clause-error-goes-out" (handler-case (handler-case (error "a") (error () (error "b"))) (error (c) (princ-to-string c))) "b")
(chk "restart-from-outer-hb"
     (handler-bind ((error (lambda (c) (invoke-restart 'use-it 42))))
       (restart-case (handler-case (error "x") (type-error () :wrong)) (use-it (v) (list :restarted v))))
     '(:restarted 42))
(chk "nonmatch-inner" (block o (handler-bind ((error (lambda (c) (return-from o :outer)))) (handler-case (error "x") (type-error () :inner)))) :outer)
(chk "ignore-errors" (ignore-errors (error "x")) nil)
(chk "return-from-body" (block b (handler-case (return-from b :ret) (error () :e))) :ret)
(chk "after-return-from" (block o (handler-bind ((error (lambda (c) (return-from o :outer)))) (error "y"))) :outer)
(chk "loop-100" (let ((n 0)) (dotimes (i 100) (handler-case (error "z") (error () (incf n)))) n) 100)
(format t "~&~a (~d failures)~%" (if (zerop *hco-fail*) "PASS" "FAIL") *hco-fail*)
(sys-exit (if (zerop *hco-fail*) 0 1))
