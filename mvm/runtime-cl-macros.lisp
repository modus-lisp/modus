;;;; runtime-cl-macros.lisp — CL macros that need to expand at RUNTIME EVAL.
;;;;
;;;; Modus's compile-time *macro-table* contains many macros installed via
;;;; mvm-define-macro with SBCL-side lambdas — those lambdas can't cross
;;;; into the runtime image, so the runtime *macro-function-table* only
;;;; knows the macros' NAMES (as T markers), not their expanders.
;;;;
;;;; This file stores a list of (defmacro …) source forms as STRINGS so
;;;; backquote inside doesn't confuse compile-quote at build time.  At
;;;; boot, %install-runtime-cl-macros walks the list, READ-FROM-STRINGs
;;;; each entry (the runtime reader expands its backquote), and EVALs
;;;; the resulting form — which routes through cl-eval.lisp's runtime
;;;; DEFMACRO handler and builds an %interp-closure expander in
;;;; *macro-function-table*.

(defvar *modus-runtime-macros*
  (list
    "(defmacro when (test &rest body) `(if ,test (progn ,@body) nil))"

    "(defmacro unless (test &rest body) `(if ,test nil (progn ,@body)))"

    "(defmacro and (&rest forms)
       (cond ((null forms) t)
             ((null (cdr forms)) (car forms))
             (t (list 'if (car forms) (cons 'and (cdr forms)) nil))))"

    "(defmacro or (&rest forms)
       (cond ((null forms) nil)
             ((null (cdr forms)) (car forms))
             (t (let ((tmp (gensym \"OR\")))
                  (list 'let (list (list tmp (car forms)))
                        (list 'if tmp tmp (cons 'or (cdr forms))))))))"

    ;; NOTE: COND is a runtime-EVAL special form already (cl-eval.lisp);
    ;; we don't install a user macro for it because that would shadow
    ;; the special-form dispatch.

    "(defmacro setf (place value)
       (cond
         ((symbolp place) (list 'setq place value))
         ((consp place)
          (let ((acc (car place)) (args (cdr place)))
            (cond
              ((eq acc 'car) (list 'rplaca (car args) value))
              ((eq acc 'cdr) (list 'rplacd (car args) value))
              ((eq acc 'aref) (cons 'aset (append args (list value))))
              ((eq acc 'gethash) (list 'puthash (car args) (cadr args) value))
              ((eq acc 'symbol-value) (list 'set-symbol-value (car args) value))
              ((eq acc 'first) (list 'rplaca (car args) value))
              ((eq acc 'rest) (list 'rplacd (car args) value))
              ((eq acc 'nth) (list 'rplaca (list 'nthcdr (car args) (cadr args)) value))
              (t nil))))
         (t nil)))"

    "(defmacro incf (place &rest delta)
       (let ((d (if delta (car delta) 1)))
         (list 'setf place (list '+ place d))))"

    "(defmacro decf (place &rest delta)
       (let ((d (if delta (car delta) 1)))
         (list 'setf place (list '- place d))))"

    "(defmacro push (val place)
       (list 'setf place (list 'cons val place)))"

    "(defmacro pop (place)
       (let ((tmp (gensym \"POP\")))
         (list 'let (list (list tmp (list 'car place)))
               (list 'setf place (list 'cdr place))
               tmp)))"

    "(defmacro pushnew (val place)
       (let ((vtmp (gensym \"PN\")))
         (list 'let (list (list vtmp val))
               (list 'unless (list 'member vtmp place)
                     (list 'setf place (list 'cons vtmp place))))))"

    "(defmacro dolist (spec &rest body)
       (let* ((var (car spec))
              (lst-form (cadr spec))
              (result (caddr spec))
              (lst (gensym \"DL\")))
         (list 'let (list (list lst lst-form))
               (list 'loop
                 (list 'when (list 'null lst) (list 'return result))
                 (cons 'let (cons (list (list var (list 'car lst))) body))
                 (list 'setq lst (list 'cdr lst))))))"

    "(defmacro dotimes (spec &rest body)
       (let* ((var (car spec))
              (count-form (cadr spec))
              (result (caddr spec))
              (n (gensym \"DT\")))
         (list 'let (list (list var 0) (list n count-form))
               (list 'loop
                 (list 'when (list '>= var n) (list 'return result))
                 (cons 'progn body)
                 (list 'setq var (list '+ var 1))))))"

    "(defmacro case (key &rest clauses)
       (let ((kv (gensym \"CASE\")))
         (let ((acc nil) (cur clauses))
           (loop
             (when (null cur) (return nil))
             (let ((clause (car cur)))
               (let ((tst (car clause)) (body (cdr clause)))
                 (cond
                   ((or (eq tst t)
                        (and (symbolp tst) (string= (symbol-name tst) \"OTHERWISE\")))
                    (setq acc (cons (cons t body) acc)))
                   ((consp tst)
                    (let ((tests nil) (vs tst))
                      (loop (when (null vs) (return nil))
                        (setq tests (cons (list 'eql kv (list 'quote (car vs))) tests))
                        (setq vs (cdr vs)))
                      (setq acc (cons (cons (cons 'or (nreverse tests)) body) acc))))
                   (t
                    (setq acc (cons (cons (list 'eql kv (list 'quote tst)) body) acc))))))
             (setq cur (cdr cur)))
           (list 'let (list (list kv key))
                 (cons 'cond (nreverse acc))))))"

    "(defmacro prog1 (first &rest rest)
       (let ((tmp (gensym \"P1\")))
         (list 'let (list (list tmp first))
               (cons 'progn rest)
               tmp)))"

    "(defmacro prog2 (first second &rest rest)
       (list 'progn first (cons 'prog1 (cons second rest))))"

    "(defmacro multiple-value-setq (vars form)
       (let ((vals (gensym \"MVS\")))
         (let ((acc nil) (i 0) (vs vars))
           (loop (when (null vs) (return nil))
             (setq acc (cons (list 'setq (car vs) (list 'nth i vals)) acc))
             (setq i (+ i 1))
             (setq vs (cdr vs)))
           (cons 'let (cons (list (list vals (list 'multiple-value-list form)))
                            (append (nreverse acc) (list (car vars))))))))"

    "(defmacro return (&rest args)
       (list 'return-from nil (if args (car args) nil)))"

    "(defmacro 1+ (x) (list '+ x 1))"
    "(defmacro 1- (x) (list '- x 1))"

    "(defmacro locally (&rest body) (cons 'progn body))"
    "(defmacro the (type form) form)"
    "(defmacro declare (&rest decls) nil)"
    "(defmacro proclaim (form) nil)"
    "(defmacro declaim (&rest decls) nil)"
    "(defmacro check-type (place type &rest args) nil)"
    "(defmacro ignore-errors (&rest body)
       (list 'handler-case (cons 'progn body) (list t (list 'c) (list 'values nil 'c))))"
    "(defmacro with-standard-io-syntax (&rest body) (cons 'progn body))"))

(defun %rt-install-one (src)
  "Read SRC (a defmacro source string) and eval.  Caller wraps in
   handler-case if it wants resilience — wrapping each call here
   triggered a stability bug where installs 3+ stopped registering."
  (eval (read-from-string src)))


(defun %install-runtime-cl-macros ()
  "Eval each entry of *modus-runtime-macros* at boot.  The walks
   below are unrolled because a (loop … (%rt-install-one (car forms))
   (setq forms (cdr forms))) sequence somehow leaves later iterations
   unable to register the macro — even with each individual call in a
   fresh handler-case frame, calls 4+ install a #<INTERP-CLOSURE> but
   set-macro-function doesn't seem to actually puthash it.  Cause not
   yet diagnosed; unrolling sidesteps the issue completely."
  (when (boundp '*modus-runtime-macros*)
    (let ((lst *modus-runtime-macros*))
      ;; NO handler-case here — wrapping each install with handler-case
      ;; triggers a stability bug where installs 3+ fail to register.
      ;; If a source string is malformed, we WILL crash here.  Caller
      ;; must wrap the whole call if it wants resilience.
      (%rt-install-one (nth 0 lst))
      (%rt-install-one (nth 1 lst))
      (%rt-install-one (nth 2 lst))
      (%rt-install-one (nth 3 lst))
      (%rt-install-one (nth 4 lst))
      (%rt-install-one (nth 5 lst))
      (%rt-install-one (nth 6 lst))
      (%rt-install-one (nth 7 lst))
      (%rt-install-one (nth 8 lst))
      (%rt-install-one (nth 9 lst))
      (%rt-install-one (nth 10 lst))
      (%rt-install-one (nth 11 lst))
      (%rt-install-one (nth 12 lst))
      (%rt-install-one (nth 13 lst))
      (%rt-install-one (nth 14 lst))
      (%rt-install-one (nth 15 lst))
      (%rt-install-one (nth 16 lst))
      (%rt-install-one (nth 17 lst))
      (%rt-install-one (nth 18 lst))
      (%rt-install-one (nth 19 lst))
      (%rt-install-one (nth 20 lst))
      (%rt-install-one (nth 21 lst))
      (%rt-install-one (nth 22 lst))
      (%rt-install-one (nth 23 lst))
      (%rt-install-one (nth 24 lst))
      (%rt-install-one (nth 25 lst))
      (%rt-install-one (nth 26 lst)))))
