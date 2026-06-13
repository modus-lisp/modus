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
              (t
               (let ((d (%find-setf-expander acc)))
                 (if d
                     (%apply-setf-expander d args value)
                     nil))))))
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

    ;; DO and DO* — sequential variable update (technically DO is
    ;; supposed to be parallel; we approximate as sequential, which
    ;; matches single-variable forms and most test patterns).
    "(defmacro do (var-specs end-spec &rest body)
       (let ((test (car end-spec))
             (result-forms (cdr end-spec))
             (vars nil)
             (steps nil))
         (let ((cur var-specs))
           (loop (when (null cur) (return nil))
             (let ((spec (car cur)))
               (cond
                 ((symbolp spec)
                  (setq vars (cons (list spec nil) vars)))
                 (t
                  (setq vars (cons (list (car spec) (cadr spec)) vars))
                  (when (cddr spec)
                    (setq steps (cons (list (car spec) (caddr spec)) steps))))))
             (setq cur (cdr cur))))
         (let ((step-setqs nil)
               (rev-steps (nreverse steps)))
           (dolist (s rev-steps)
             (setq step-setqs (cons (list 'setq (car s) (cadr s)) step-setqs)))
           (setq step-setqs (nreverse step-setqs))
           (let ((result-form (cond
                                ((null result-forms) nil)
                                ((null (cdr result-forms)) (car result-forms))
                                (t (cons 'progn result-forms)))))
             (list 'block 'nil
                   (list 'let* (nreverse vars)
                         (cons 'loop
                               (cons (list 'when test (list 'return result-form))
                                     (append body step-setqs)))))))))"

    "(defmacro do* (var-specs end-spec &rest body)
       (cons 'do (cons var-specs (cons end-spec body))))"

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

    ;; ECASE / CCASE — like CASE but signal an error when no clause matches
    ;; instead of returning NIL (CLHS 5.3).  No T/OTHERWISE clause is allowed,
    ;; so every key list becomes an (or (eql ...) ...) test and the cond ends
    ;; with a (t (error ...)) fallthrough.  CCASE is a correctable error in
    ;; full CL (store-value + retry); minimally we degrade it to an ECASE-like
    ;; signal (matching the CLAUDE.md CCASE→ECASE degrade pattern).
    "(defmacro ecase (key &rest clauses)
       (let ((kv (gensym \"ECASE\")))
         (let ((acc nil) (cur clauses))
           (loop
             (when (null cur) (return nil))
             (let ((clause (car cur)))
               (let ((tst (car clause)) (body (cdr clause)))
                 (if (consp tst)
                     (let ((tests nil) (vs tst))
                       (loop (when (null vs) (return nil))
                         (setq tests (cons (list 'eql kv (list 'quote (car vs))) tests))
                         (setq vs (cdr vs)))
                       (setq acc (cons (cons (cons 'or (nreverse tests)) body) acc)))
                     (setq acc (cons (cons (list 'eql kv (list 'quote tst)) body) acc)))))
             (setq cur (cdr cur)))
           (setq acc (cons (list t (list 'error \"ECASE: no clause matches\")) acc))
           (list 'let (list (list kv key))
                 (cons 'cond (nreverse acc))))))"

    "(defmacro ccase (key &rest clauses)
       (cons 'ecase (cons key clauses)))"

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
    "(defmacro with-standard-io-syntax (&rest body) (cons 'progn body))"

    ;; TYPECASE / ETYPECASE — expand to (let ((g KEY)) (cond ((typep g 'T1) ...) ...)).
    ;; Each clause's type spec is quoted and tested with TYPEP.  An OTHERWISE/T
    ;; clause becomes the cond's (t …).  ETYPECASE adds a type-error fallthrough.
    ;; uiop/package's reify-package uses (etypecase pkg ((eql (find-package :cl)) …)).
    "(defmacro typecase (key &rest clauses)
       (let ((g (gensym \"TC\")))
         (list 'let (list (list g key))
               (cons 'cond
                 (mapcar (lambda (cl)
                           (let ((ty (car cl)) (body (cdr cl)))
                             (if (or (eq ty t) (and (symbolp ty) (string= (symbol-name ty) \"OTHERWISE\")))
                                 (cons t body)
                                 (cons (list 'typep g (list 'quote ty)) body))))
                         clauses)))))"

    "(defmacro etypecase (key &rest clauses)
       (let ((g (gensym \"ETC\")))
         (list 'let (list (list g key))
               (append
                 (cons 'cond
                   (mapcar (lambda (cl)
                             (let ((ty (car cl)) (body (cdr cl)))
                               (cons (list 'typep g (list 'quote ty)) body)))
                           clauses))
                 (list (list t (list 'error \"ETYPECASE: no clause matches\")))))))"

    ;; CTYPECASE — like ETYPECASE but a correctable (store-and-retry) error in
    ;; full CL.  Minimally we degrade it to an ETYPECASE-like signal on no
    ;; match (mirroring the CCASE→ECASE degrade); no retry/store-value yet.
    "(defmacro ctypecase (key &rest clauses)
       (let ((g (gensym \"CTC\")))
         (list 'let (list (list g key))
               (append
                 (cons 'cond
                   (mapcar (lambda (cl)
                             (let ((ty (car cl)) (body (cdr cl)))
                               (cons (list 'typep g (list 'quote ty)) body)))
                           clauses))
                 (list (list t (list 'error \"CTYPECASE: no clause matches\")))))))"

    ;; DEFINE-MODIFY-MACRO — (define-modify-macro name lambda-list fn [doc]).
    ;; Defines NAME as a macro: (name place a b …) => (setf place (fn place a b …)).
    ;; We ignore the lambda-list's structure and capture all post-place args
    ;; via &rest, which covers the `(&rest args)' shape ASDF's APPENDF uses.
    "(defmacro define-modify-macro (name lambda-list fn &rest doc)
       (list 'defmacro name (list 'place '&rest 'args)
             (list 'list (list 'quote 'setf) 'place
                   (list 'list* (list 'quote fn) 'place 'args))))"

    ;; DESTRUCTURING-BIND — expand to (apply (lambda PATTERN BODY) EXPR).
    ;; %bind-params handles &optional/&rest/&key/&aux AND dotted tails, so
    ;; the apply-lambda trick covers ASDF's `(destructuring-bind (car . cdr)
    ;; form …)' (a dotted macro-style lambda list).
    "(defmacro destructuring-bind (pattern expr &rest body)
       (list 'apply (cons 'lambda (cons pattern body)) expr))"

    ;; DO-SYMBOLS / DO-EXTERNAL-SYMBOLS / DO-ALL-SYMBOLS — package
    ;; iteration.  Mirror compiler.lisp's mvm-define-macro expansions so
    ;; runtime EVAL of these (used heavily by uiop/package:ensure-package)
    ;; materializes accessible symbols via %do-*-symbols-fn then walks.
    "(defmacro do-symbols (spec &rest body)
       (let ((var (car spec))
             (pkg (if (cdr spec) (cadr spec) '*package*))
             (result (and (cddr spec) (caddr spec)))
             (syms (gensym \"DS\")) (cur (gensym \"DSC\")))
         (list 'block nil
           (list 'let (list (list syms nil))
             (list '%do-symbols-fn
                   (list 'lambda (list var) (list 'setq syms (list 'cons var syms)))
                   pkg)
             (list 'let (list (list cur syms))
               (list 'loop
                 (list 'when (list 'null cur) (list 'return result))
                 (list 'let (list (list var (list 'car cur)))
                   (cons 'progn body))
                 (list 'setq cur (list 'cdr cur))))))))"

    "(defmacro do-external-symbols (spec &rest body)
       (let ((var (car spec))
             (pkg (if (cdr spec) (cadr spec) '*package*))
             (result (and (cddr spec) (caddr spec)))
             (syms (gensym \"DES\")) (cur (gensym \"DESC\")))
         (list 'block nil
           (list 'let (list (list syms nil))
             (list '%do-external-symbols-fn
                   (list 'lambda (list var) (list 'setq syms (list 'cons var syms)))
                   pkg)
             (list 'let (list (list cur syms))
               (list 'loop
                 (list 'when (list 'null cur) (list 'return result))
                 (list 'let (list (list var (list 'car cur)))
                   (cons 'progn body))
                 (list 'setq cur (list 'cdr cur))))))))"

    "(defmacro do-all-symbols (spec &rest body)
       (let ((var (car spec))
             (result (and (cdr spec) (cadr spec)))
             (syms (gensym \"DAS\")) (cur (gensym \"DASC\")))
         (list 'block nil
           (list 'let (list (list syms nil))
             (list '%do-all-symbols-fn
                   (list 'lambda (list var) (list 'setq syms (list 'cons var syms))))
             (list 'let (list (list cur syms))
               (list 'loop
                 (list 'when (list 'null cur) (list 'return result))
                 (list 'let (list (list var (list 'car cur)))
                   (cons 'progn body))
                 (list 'setq cur (list 'cdr cur))))))))"))

(defun %rt-install-one (src)
  "Read SRC (a defmacro source string) and eval.  Caller wraps in
   handler-case if it wants resilience — wrapping each call here
   triggered a stability bug where installs 3+ stopped registering.
   A NIL SRC (unrolled (nth k) past the list end) is a no-op so the
   unroll can safely overshoot the current entry count."
  (when src
    (eval (read-from-string src))))


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
      (%rt-install-one (nth 26 lst))
      (%rt-install-one (nth 27 lst))
      (%rt-install-one (nth 28 lst))
      (%rt-install-one (nth 29 lst))
      (%rt-install-one (nth 30 lst))
      (%rt-install-one (nth 31 lst))
      (%rt-install-one (nth 32 lst))
      (%rt-install-one (nth 33 lst))
      (%rt-install-one (nth 34 lst))
      (%rt-install-one (nth 35 lst))
      (%rt-install-one (nth 36 lst))
      (%rt-install-one (nth 37 lst))
      (%rt-install-one (nth 38 lst))
      (%rt-install-one (nth 39 lst))
      (%rt-install-one (nth 40 lst))))
  ;; Re-assert standard CL symbol exports.  %init-packages exports them
  ;; at boot, but a subsequent boot-time read of a &rest-bearing form
  ;; demotes CL:&REST from :external back to :internal (other lambda-list
  ;; keywords stay :external).  CLHS requires &REST be :external in
  ;; COMMON-LISP; an :internal &REST is not inherited by use-CL packages,
  ;; so uiop's define-package :use-reexport path — which find-symbol*'s
  ;; every CL external — signals on &REST.  Re-running the export is
  ;; idempotent (only ever promotes :internal -> :external for the
  ;; standard names) and leaves reader / package state otherwise intact.
  (handler-case (%export-standard-cl-symbols) (t (c) nil)))
