;;;; numeric-type-checks.lisp -- LENGTH on non-sequences; arithmetic unaffected.
;;;;
;;;; Run: ./modus --script test/numeric-type-checks.lisp
;;;;
;;;; (length :foo) answered 1 (ARRAY-LENGTH read any object's header count);
;;;; CLHS requires a TYPE-ERROR, and it is one now.
;;;;
;;;; ARITHMETIC AND COMPARISON ON A NON-NUMBER ARE STILL NOT CHECKED, AND THAT
;;;; IS MEASURED, NOT FORGOTTEN.  (+ 1 :foo), (+ NIL 1), (> :foo 3) still
;;;; produce values.  Three attempts, all reverted:
;;;;   * NUMBERP in the generic fallbacks broke boot -- internals pass RAW
;;;;     MACHINE WORDS through them (the interpreter's %MVM-WORD does
;;;;     (* 2 <raw word>)), and the large-bignum array fails REALP;
;;;;   * "recognisably not a number" (symbol/char/cons/string/function) broke
;;;;     boot too -- a raw word can satisfy SYMBOLP or CONSP, and on i386
;;;;     SYMBOLP of one faulted reading the memory it named;
;;;;   * rejecting only NIL and T booted, passed every CLI test and both
;;;;     library ladders -- and cost ~1000 ANSI tests: runtime-eval machinery
;;;;     increments counters that are still NIL (CLAUDE.md limitation 7; the
;;;;     interpreter's own *NLX-STATE-SERIAL* was one, fixed).
;;;; Checking arithmetic needs a census of those sites first (a build that
;;;; LOGS the NIL/T and raw-word arrivals instead of signalling).

(defvar *ntc-fail* 0)
(defmacro ntc-te (form)
  `(handler-case (progn ,form :no-error) (type-error () :type-error) (error (c) (list :other (type-of c)))))
(defun ntc (name got want)
  (unless (equal got want)
    (setq *ntc-fail* (+ *ntc-fail* 1))
    (format t "~&FAIL ~A: got ~S want ~S~%" name got want)))

(ntc "errors"
     (list (ntc-te (length :foo)) (ntc-te (length 5)) (ntc-te (length #\a))
           (ntc-te (length 'sym)))
     (make-list 4 :initial-element :type-error))
(ntc "ordinary arithmetic unaffected"
     (list (+ 1 2) (+ 1/2 1/3) (* 2.5 2) (- (expt 2 70) 1) (< 1 2.5) (= 3 3.0) (> 1/3 0.3)
           (length "abc") (length '(1 2)) (length #(1 2 3)) (length nil) (= #c(1 2) #c(1 2))
           (< (expt 10 40) 0))
     (list 3 5/6 5.0 1180591620717411303423 t t t 3 2 3 0 t nil))

(format t "~&numeric-type-checks: ~D failure(s)~%" *ntc-fail*)
(when (> *ntc-fail* 0) (error "numeric-type-checks: ~D failure(s)" *ntc-fail*))
