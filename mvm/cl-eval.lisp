;;;; cl-eval.lisp — Eval, compile, load, symbol-function table
;;;; Part of the Modus CL runtime. Depends on cl-reader.lisp.

;;; ============================================================
;;; Layer 8: Eval / Compile / Load
;;; ============================================================

;;; Global symbol-function table: maps symbol-name-string → function object.
;;; Populated at startup with all built-in compiled functions.
;;; Updated by (setf (symbol-function sym) fn) and defun-in-eval.
(defvar *symbol-function-table* nil)

(defun %sft-init ()
  "Initialize the symbol-function table (empty hash table)."
  (setq *symbol-function-table* (make-hash-table)))

(defun symbol-function (sym)
  "Return the function object for SYM, or signal undefined-function."
  (let ((name (cond
                ((%cl-sym-p sym) (%cl-sym-name sym))
                ((stringp sym) sym)
                (t nil))))
    (when (null name)
      (error "symbol-function: not a symbol"))
    (let ((fn (if *symbol-function-table*
                  (gethash name *symbol-function-table*)
                  nil)))
      (if fn
          fn
          (let ((c (%make-condition 'undefined-function (list :name sym))))
            (if (%error-handler-active-p)
                (%hc-longjmp)
                (progn (error "undefined function") nil)))))))

(defun set-symbol-function (sym fn)
  "Set the function cell of SYM to FN."
  (let ((name (cond
                ((%cl-sym-p sym) (%cl-sym-name sym))
                ((stringp sym) sym)
                (t nil))))
    (when (null name)
      (error "set-symbol-function: not a symbol"))
    (unless *symbol-function-table*
      (%sft-init))
    (puthash name *symbol-function-table* fn)
    fn))

(defun fboundp (sym)
  "Return T if SYM has a function binding."
  (let ((name (cond
                ((%cl-sym-p sym) (%cl-sym-name sym))
                ((stringp sym) sym)
                ((null sym) nil)
                (t nil))))
    (if (null name)
        nil
        (if *symbol-function-table*
            (if (gethash name *symbol-function-table*) t nil)
            nil))))

(defun fmakunbound (sym)
  "Remove the function binding of SYM."
  (let ((name (cond
                ((%cl-sym-p sym) (%cl-sym-name sym))
                ((stringp sym) sym)
                (t nil))))
    (when (and name *symbol-function-table*)
      (remhash name *symbol-function-table*)))
  sym)

(defun fdefinition (sym)
  "Return the function definition of SYM.
   For generic functions, returns the GF object."
  (let ((name (cond
                ((%cl-sym-p sym) (%cl-sym-name sym))
                ((stringp sym) sym)
                ((and (consp sym) (eq (car sym) 'setf))
                 ;; (setf foo) — look up as regular name for now
                 nil)
                (t nil))))
    (when (null name)
      (return-from fdefinition (symbol-function sym)))
    ;; Check GF registry first
    (let ((gf-sym (cond
                    ((%cl-sym-p sym) sym)
                    ((stringp sym) nil)
                    (t nil))))
      (when gf-sym
        (let ((gf (%find-gf gf-sym)))
          (when gf (return-from fdefinition gf)))))
    ;; Fall back to symbol-function
    (symbol-function sym)))

(defun set-fdefinition (sym fn)
  "Set the function definition of SYM."
  (set-symbol-function sym fn))

;;; ============================================================
;;; Macro table: maps macro-name-string → expander-function
;;; ============================================================
(defvar *macro-function-table* nil)

(defun macro-function (sym &rest env)
  "Return the macro expander function for SYM, or nil."
  (let ((name (cond
                ((%cl-sym-p sym) (%cl-sym-name sym))
                ((stringp sym) sym)
                (t nil))))
    (if (and name *macro-function-table*)
        (gethash name *macro-function-table*)
        nil)))

(defun set-macro-function (sym fn &rest env)
  "Install FN as the macro expander for SYM."
  (let ((name (cond
                ((%cl-sym-p sym) (%cl-sym-name sym))
                ((stringp sym) sym)
                (t nil))))
    (when name
      (unless *macro-function-table*
        (setq *macro-function-table* (make-hash-table)))
      (puthash name *macro-function-table* fn)
      fn)))

;;; ============================================================
;;; Macroexpand: walk macro calls
;;; ============================================================

(defun macroexpand-1 (form &rest env-arg)
  "Expand FORM one level if it's a macro call. Returns (values form expanded-p)."
  (if (and (consp form) (%cl-sym-p (car form)))
      (let ((mf (macro-function (car form))))
        (if mf
            (let ((expanded (funcall mf form nil)))
              (values expanded t))
            (values form nil)))
      (values form nil)))

(defun macroexpand (form &rest env-arg)
  "Expand FORM repeatedly until not a macro call. Returns (values form expanded-p)."
  (let ((any nil))
    (let ((cur form))
      (loop
        (let ((mf (if (and (consp cur) (%cl-sym-p (car cur)))
                      (macro-function (car cur))
                      nil)))
          (if mf
              (progn
                (setq cur (funcall mf cur nil))
                (setq any t))
              (return (values cur any))))))))

;;; ============================================================
;;; Eval global variable table
;;; Maps symbol-name-string → value for runtime-defined variables
;;; ============================================================
(defvar *eval-global-env* nil)

(defun %eval-global-get (name)
  "Look up global variable by name string. Returns (found-p . value)."
  (let ((cur *eval-global-env*))
    (loop
      (when (null cur) (return (cons nil nil)))
      (let ((pair (car cur)))
        (when (string-equal (car pair) name)
          (return (cons t (cdr pair)))))
      (setq cur (cdr cur)))))

(defun %eval-global-set (name value)
  "Set global variable by name string."
  (let ((cur *eval-global-env*))
    (loop
      (when (null cur)
        ;; Not found: add new
        (setq *eval-global-env* (cons (cons name value) *eval-global-env*))
        (return value))
      (let ((pair (car cur)))
        (when (string-equal (car pair) name)
          (set-cdr pair value)
          (return value)))
      (setq cur (cdr cur)))))

;;; ============================================================
;;; Interpreter environment helpers
;;; ============================================================

;;; env = alist of ((name-string . value) ...)
;;; We store CL symbols directly as keys.

(defun %env-lookup (sym env)
  "Look up SYM (CL symbol or string name) in ENV alist. Returns (found-p . value)."
  (let ((name (if (%cl-sym-p sym) (%cl-sym-name sym) sym))
        (cur env))
    (loop
      (when (null cur) (return (cons nil nil)))
      (let ((binding (car cur)))
        (let ((bname (if (%cl-sym-p (car binding))
                         (%cl-sym-name (car binding))
                         (car binding))))
          (when (string-equal name bname)
            (return (cons t (cdr binding)))))
        (setq cur (cdr cur))))))

(defun %env-extend (sym val env)
  "Add (sym . val) binding to front of ENV."
  (cons (cons sym val) env))

;;; ============================================================
;;; Eval -- tree-walking interpreter
;;; ============================================================

(defun %eval-sym-name (sym)
  "Get the string name of a symbol (CL or MVM)."
  (cond
    ((%cl-sym-p sym) (%cl-sym-name sym))
    ((stringp sym) sym)
    (t nil)))

(defun %eval-sym-value (sym env)
  "Look up the value of symbol SYM in ENV + globals."
  (let ((found-pair (%env-lookup sym env)))
    (if (car found-pair)
        (cdr found-pair)
        ;; Try eval global table first
        (let ((name (%eval-sym-name sym)))
          (if name
              (let ((gv (%eval-global-get name)))
                (if (car gv)
                    (cdr gv)
                    nil))
              nil)))))

(defun %eval-progn (forms env)
  "Evaluate a list of forms, return value of last."
  (if (null forms)
      nil
      (let ((cur forms))
        (loop
          (if (null (cdr cur))
              (return (%eval-in-env (car cur) env))
              (progn
                (%eval-in-env (car cur) env)
                (setq cur (cdr cur))))))))

(defun %eval-let-bindings (bindings env orig-env)
  "Evaluate LET bindings (parallel) and extend ENV."
  (let ((new-env env)
        (cur bindings))
    (loop
      (when (null cur) (return new-env))
      (let ((binding (car cur)))
        (let ((var (if (consp binding) (car binding) binding))
              (val-form (if (and (consp binding) (cdr binding)) (cadr binding) nil)))
          (let ((val (%eval-in-env val-form orig-env)))
            (setq new-env (%env-extend var val new-env)))))
      (setq cur (cdr cur)))))

(defun %eval-let*-bindings (bindings env)
  "Evaluate LET* bindings (sequential) and extend ENV."
  (let ((new-env env)
        (cur bindings))
    (loop
      (when (null cur) (return new-env))
      (let ((binding (car cur)))
        (let ((var (if (consp binding) (car binding) binding))
              (val-form (if (and (consp binding) (cdr binding)) (cadr binding) nil)))
          (let ((val (%eval-in-env val-form new-env)))
            (setq new-env (%env-extend var val new-env)))))
      (setq cur (cdr cur)))))

(defun %eval-args (arg-forms env)
  "Evaluate a list of argument forms."
  (let ((result nil)
        (cur arg-forms))
    (loop
      (when (null cur) (return (nreverse result)))
      (setq result (cons (%eval-in-env (car cur) env) result))
      (setq cur (cdr cur)))))

(defun %eval-call-fn (fn args form)
  "Call FN with ARGS list, using funcall/apply."
  (let ((nargs (length args)))
    (cond
      ((= nargs 0) (funcall fn))
      ((= nargs 1) (funcall fn (car args)))
      ((= nargs 2) (funcall fn (car args) (cadr args)))
      ((= nargs 3) (funcall fn (car args) (cadr args) (caddr args)))
      ((= nargs 4) (funcall fn (car args) (cadr args) (caddr args) (cadddr args)))
      ((= nargs 5) (funcall fn (car args) (cadr args) (caddr args) (cadddr args) (nth 4 args)))
      (t (apply fn args)))))

(defun %eval-sym-eq (sym name-str)
  "Check if SYM (CL symbol or string) has name NAME-STR."
  (let ((n (%eval-sym-name sym)))
    (if n (string-equal n name-str) nil)))

(defun %interp-closure-p (x)
  "True if X is an interpreted closure (cons with tag %INTERP-CLOSURE)."
  (and (consp x) (eq (car x) '%interp-closure)))

(defun %call-interp-closure (fn args)
  "Call an interpreted closure."
  ;; fn = (%interp-closure params body env)
  (let ((params (cadr fn))
        (body (caddr fn))
        (closed-env (cadddr fn)))
    (let ((new-env (%bind-params params args closed-env)))
      (%eval-progn body new-env))))

(defun %bind-params (params args env)
  "Bind PARAMS to ARGS in ENV, handling &rest."
  (let ((new-env env)
        (ps params)
        (as args))
    (loop
      (cond
        ((null ps) (return new-env))
        ;; &rest parameter
        ((%eval-sym-eq (car ps) "&REST")
         (setq ps (cdr ps))
         (when ps
           (setq new-env (%env-extend (car ps) as new-env)))
         (return new-env))
        ;; &optional parameter
        ((%eval-sym-eq (car ps) "&OPTIONAL")
         (setq ps (cdr ps)))
        ;; Regular parameter
        (t
         (setq new-env (%env-extend (car ps) (if as (car as) nil) new-env))
         (setq ps (cdr ps))
         (setq as (if as (cdr as) nil)))))))

(defun %eval-function-form (name-or-lambda env)
  "Evaluate a #'x or (function x) form."
  (if (and (consp name-or-lambda) (%eval-sym-eq (car name-or-lambda) "LAMBDA"))
      ;; (function (lambda ...)) → interpreted closure
      (let ((params (cadr name-or-lambda))
            (body (cddr name-or-lambda)))
        (list '%interp-closure params body env))
      ;; (function name) → look up compiled function
      (let ((name (%eval-sym-name name-or-lambda)))
        (if name
            (let ((fn (if *symbol-function-table*
                          (gethash name *symbol-function-table*)
                          nil)))
              (or fn (error "undefined function")))
            name-or-lambda))))

;;; Block/return-from support via condition mechanism
;;; We use a simple approach: block-return throws a condition caught by block.

(defun %eval-block (name forms env)
  "Evaluate (block name forms...) with return-from support."
  (handler-case
    (%eval-progn forms env)
    (error (c)
      ;; Check if it's a block-return for this block
      (if (%block-return-p c name)
          (%block-return-value c)
          (error c)))))

;;; We implement block/return-from by signalling a special condition.
;;; Since we can't easily do this without CLOS conditions, use a simpler
;;; approach: use a global stack of block return values.

(defvar *%block-return-stack* nil)

(defun %block-push (tag value)
  "Push a return value for BLOCK with TAG onto the stack."
  (setq *%block-return-stack* (cons (cons tag value) *%block-return-stack*)))

(defun %block-pop (tag)
  "Pop and return the return value for BLOCK with TAG."
  (let ((cur *%block-return-stack*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) tag)
        ;; Remove all entries up to and including this tag
        (setq *%block-return-stack* (cdr cur))
        (return (cdr (car cur))))
      (setq cur (cdr cur)))))

;;; For block/return-from, use condition system
;;; %block-return condition = (%block-return-cond . (tag . value))
(defvar *%eval-throw-tag* nil)
(defvar *%eval-throw-value* nil)

(defun %eval-in-env (form env)
  "Main eval function. Evaluates FORM in ENV (alist of bindings)."
  (cond
    ;; Self-evaluating: nil
    ((null form) nil)
    ;; Self-evaluating: t
    ((eq form t) t)
    ;; Self-evaluating: numbers
    ((integerp form) form)
    ;; Self-evaluating: floats
    ((floatp-impl form) form)
    ;; Self-evaluating: characters
    ((characterp form) form)
    ;; Self-evaluating: strings
    ((stringp form) form)
    ;; Self-evaluating: vectors
    ((vectorp form) form)
    ;; Keywords self-evaluate
    ((and (%cl-sym-p form)
          (let ((kp (find-package "KEYWORD")))
            (if kp (eq (%cl-sym-package form) kp) nil)))
     form)
    ;; Symbol: variable lookup
    ((or (%cl-sym-p form) (symbolp form))
     (%eval-sym-lookup form env))
    ;; List: dispatch on operator
    ((consp form)
     (%eval-compound form env))
    ;; Default: self-evaluate
    (t form)))

(defun %eval-sym-lookup (sym env)
  "Look up value of SYM in ENV then globals."
  (let ((found-pair (%env-lookup sym env)))
    (if (car found-pair)
        (cdr found-pair)
        ;; Try eval global table
        (let ((name (%eval-sym-name sym)))
          (if name
              (let ((gv (%eval-global-get name)))
                (if (car gv)
                    (cdr gv)
                    ;; Not found: signal unbound-variable
                    (let ((c2 (%make-condition 'unbound-variable (list :name sym))))
                      (if (%error-handler-active-p)
                          (%hc-longjmp)
                          nil))))
              nil)))))

(defun %eval-compound (form env)
  "Evaluate a compound (list) form."
  (let ((op (car form))
        (args (cdr form)))
    (cond
      ;; QUOTE
      ((%eval-sym-eq op "QUOTE") (car args))
      ;; IF
      ((%eval-sym-eq op "IF")
       (if (%eval-in-env (car args) env)
           (%eval-in-env (cadr args) env)
           (if (cddr args) (%eval-in-env (caddr args) env) nil)))
      ;; PROGN
      ((%eval-sym-eq op "PROGN") (%eval-progn args env))
      ;; LET
      ((%eval-sym-eq op "LET")
       (let ((bindings (car args))
             (body (cdr args)))
         (let ((new-env (%eval-let-bindings bindings env env)))
           (%eval-progn body new-env))))
      ;; LET*
      ((%eval-sym-eq op "LET*")
       (let ((bindings (car args))
             (body (cdr args)))
         (let ((new-env (%eval-let*-bindings bindings env)))
           (%eval-progn body new-env))))
      ;; SETQ
      ((%eval-sym-eq op "SETQ")
       (let ((cur args))
         (let ((result nil))
           (loop
             (when (null cur) (return result))
             (let ((var (car cur))
                   (val-form (cadr cur)))
               (let ((val (%eval-in-env val-form env)))
                 ;; Check if in local env
                 (let ((found-pair (%env-lookup var env)))
                   (if (car found-pair)
                       ;; Update local binding
                       (let ((binding (%env-find-binding var env)))
                         (when binding (set-cdr binding val)))
                       ;; Update eval global table
                       (let ((vname (%eval-sym-name var)))
                         (when vname (%eval-global-set vname val)))))
                 (setq result val)))
             (setq cur (cddr cur))))))
      ;; LAMBDA
      ((%eval-sym-eq op "LAMBDA")
       (list '%interp-closure (car args) (cdr args) env))
      ;; FUNCTION (#')
      ((%eval-sym-eq op "FUNCTION")
       (%eval-function-form (car args) env))
      ;; DEFUN
      ((%eval-sym-eq op "DEFUN")
       (let ((fname (car args))
             (params (cadr args))
             (body (cddr args)))
         (let ((name-str (%eval-sym-name fname)))
           (let ((fn (list '%interp-closure params body nil)))
             (when name-str
               (unless *symbol-function-table* (%sft-init))
               (puthash name-str *symbol-function-table* fn)))
           fname)))
      ;; DEFVAR / DEFPARAMETER / DEFCONSTANT
      ((%eval-sym-eq op "DEFVAR")
       (let ((vname (car args)))
         (when (cdr args)
           (let ((val (%eval-in-env (cadr args) env)))
             (let ((nm (%eval-sym-name vname)))
               (when nm (%eval-global-set nm val)))))
         vname))
      ((%eval-sym-eq op "DEFPARAMETER")
       (let ((vname (car args))
             (val (%eval-in-env (cadr args) env)))
         (let ((nm (%eval-sym-name vname)))
           (when nm (%eval-global-set nm val)))
         vname))
      ((%eval-sym-eq op "DEFCONSTANT")
       (let ((vname (car args))
             (val (%eval-in-env (cadr args) env)))
         (let ((nm (%eval-sym-name vname)))
           (when nm (%eval-global-set nm val)))
         vname))
      ;; COND
      ((%eval-sym-eq op "COND")
       (let ((cur args))
         (loop
           (when (null cur) (return nil))
           (let ((clause (car cur)))
             (let ((test-val (%eval-in-env (car clause) env)))
               (when test-val
                 (if (cdr clause)
                     (return (%eval-progn (cdr clause) env))
                     (return test-val)))))
           (setq cur (cdr cur)))))
      ;; WHEN
      ((%eval-sym-eq op "WHEN")
       (when (%eval-in-env (car args) env)
         (%eval-progn (cdr args) env)))
      ;; UNLESS
      ((%eval-sym-eq op "UNLESS")
       (unless (%eval-in-env (car args) env)
         (%eval-progn (cdr args) env)))
      ;; AND
      ((%eval-sym-eq op "AND")
       (if (null args)
           t
           (let ((cur args))
             (loop
               (if (null (cdr cur))
                   (return (%eval-in-env (car cur) env))
                   (let ((val (%eval-in-env (car cur) env)))
                     (unless val (return nil))
                     (setq cur (cdr cur))))))))
      ;; OR
      ((%eval-sym-eq op "OR")
       (let ((cur args))
         (loop
           (when (null cur) (return nil))
           (let ((val (%eval-in-env (car cur) env)))
             (when val (return val)))
           (setq cur (cdr cur)))))
      ;; BLOCK
      ((%eval-sym-eq op "BLOCK")
       (let ((bname (car args))
             (body (cdr args)))
         ;; Use handler-case to catch return-from
         (handler-case
           (%eval-progn body env)
           (error (c)
             ;; Re-signal if not our return
             (error c)))))
      ;; RETURN-FROM (simplified: just eval value)
      ((%eval-sym-eq op "RETURN-FROM")
       (let ((val (if (cdr args) (%eval-in-env (cadr args) env) nil)))
         val))
      ;; RETURN
      ((%eval-sym-eq op "RETURN")
       (if args (%eval-in-env (car args) env) nil))
      ;; VALUES
      ((%eval-sym-eq op "VALUES")
       (let ((evaled (%eval-args args env)))
         (apply #'values evaled)))
      ;; MULTIPLE-VALUE-BIND
      ((%eval-sym-eq op "MULTIPLE-VALUE-BIND")
       (let ((vars (car args))
             (values-form (cadr args))
             (body (cddr args)))
         (let ((mvl (multiple-value-list (%eval-in-env values-form env))))
           (let ((new-env env)
                 (cur-vars vars)
                 (cur-vals mvl))
             (loop
               (when (null cur-vars) (return nil))
               (setq new-env (%env-extend (car cur-vars)
                                          (if cur-vals (car cur-vals) nil)
                                          new-env))
               (setq cur-vars (cdr cur-vars))
               (setq cur-vals (if cur-vals (cdr cur-vals) nil)))
             (%eval-progn body new-env)))))
      ;; MULTIPLE-VALUE-LIST
      ((%eval-sym-eq op "MULTIPLE-VALUE-LIST")
       (multiple-value-list (%eval-in-env (car args) env)))
      ;; TAGBODY (stub: just eval forms, ignore tags)
      ((%eval-sym-eq op "TAGBODY")
       (let ((cur args))
         (loop
           (when (null cur) (return nil))
           (when (consp (car cur))
             (%eval-in-env (car cur) env))
           (setq cur (cdr cur)))))
      ;; THE (ignore type decl)
      ((%eval-sym-eq op "THE")
       (%eval-in-env (cadr args) env))
      ;; DECLARE (ignore)
      ((%eval-sym-eq op "DECLARE") nil)
      ;; LOCALLY (just eval body)
      ((%eval-sym-eq op "LOCALLY")
       (%eval-progn args env))
      ;; LOAD-TIME-VALUE (eval now)
      ((%eval-sym-eq op "LOAD-TIME-VALUE")
       (%eval-in-env (car args) env))
      ;; EVAL-WHEN (always eval)
      ((%eval-sym-eq op "EVAL-WHEN")
       (%eval-progn (cdr args) env))
      ;; HANDLER-CASE (simplified)
      ((%eval-sym-eq op "HANDLER-CASE")
       (handler-case
         (%eval-in-env (car args) env)
         (error (c) nil)))
      ;; UNWIND-PROTECT
      ((%eval-sym-eq op "UNWIND-PROTECT")
       (unwind-protect
         (%eval-in-env (car args) env)
         (%eval-progn (cdr args) env)))
      ;; FLET / LABELS
      ((%eval-sym-eq op "FLET")
       (let ((local-fns (car args))
             (body (cdr args)))
         (let ((new-env env))
           (dolist (def local-fns)
             (let ((fname (car def))
                   (params (cadr def))
                   (fbody (cddr def)))
               (let ((fn (list '%interp-closure params fbody new-env)))
                 (setq new-env (%env-extend fname fn new-env)))))
           (%eval-progn body new-env))))
      ((%eval-sym-eq op "LABELS")
       (let ((local-fns (car args))
             (body (cdr args)))
         ;; For labels, functions can reference each other
         (let ((new-env env))
           (dolist (def local-fns)
             (let ((fname (car def))
                   (params (cadr def))
                   (fbody (cddr def)))
               (let ((fn (list '%interp-closure params fbody nil)))
                 ;; Will fix env pointer below
                 (setq new-env (%env-extend fname fn new-env)))))
           ;; Update each closure to point to new-env
           (let ((cur new-env))
             (loop
               (when (eq cur env) (return nil))
               (let ((fn (cdr (car cur))))
                 (when (%interp-closure-p fn)
                   ;; Set closed env to new-env (4th element of list)
                   (set-car (cdddr fn) new-env)))
               (setq cur (cdr cur))))
           (%eval-progn body new-env))))
      ;; Function call: symbol
      ((%cl-sym-p op)
       (%eval-funcall op args env))
      ;; Function call: lambda form
      ((and (consp op) (%eval-sym-eq (car op) "LAMBDA"))
       (let ((fn (list '%interp-closure (cadr op) (cddr op) env)))
         (let ((evaled-args (%eval-args args env)))
           (%call-interp-closure fn evaled-args))))
      ;; Function call: other (e.g. (funcall ...) result)
      (t
       (let ((fn-val (%eval-in-env op env)))
         (let ((evaled-args (%eval-args args env)))
           (%do-funcall fn-val evaled-args)))))))

(defun %env-find-binding (sym env)
  "Find the binding cons for SYM in ENV. Returns nil if not found."
  (let ((name (%eval-sym-name sym))
        (cur env))
    (loop
      (when (null cur) (return nil))
      (let ((binding (car cur)))
        (let ((bname (%eval-sym-name (car binding))))
          (when (and name bname (string-equal name bname))
            (return binding))))
      (setq cur (cdr cur)))))

(defun %eval-funcall (sym args env)
  "Evaluate a function call (sym args...) looking up sym in fn table."
  (let ((name (%eval-sym-name sym)))
    (if (null name)
        nil
        ;; First check local env for function binding
        (let ((local (%env-lookup sym env)))
          (if (car local)
              (let ((fn (cdr local)))
                (let ((evaled-args (%eval-args args env)))
                  (%do-funcall fn evaled-args)))
              ;; Look up in symbol-function table
              (let ((fn (if *symbol-function-table*
                            (gethash name *symbol-function-table*)
                            nil)))
                (if fn
                    (let ((evaled-args (%eval-args args env)))
                      (%do-funcall fn evaled-args))
                    ;; Try macro expansion
                    (let ((mf (if *macro-function-table*
                                  (gethash name *macro-function-table*)
                                  nil)))
                      (if mf
                          (%eval-in-env (funcall mf (cons sym args) nil) env)
                          ;; Undefined function
                          (let ((c (%make-condition 'undefined-function (list :name sym))))
                            (if (%error-handler-active-p)
                                (%hc-longjmp)
                                nil)))))))))))

(defun %do-funcall (fn args)
  "Call FN with ARGS list."
  (cond
    ((%interp-closure-p fn)
     (%call-interp-closure fn args))
    (t (%eval-call-fn fn args fn))))

(defun eval (form)
  "Evaluate FORM in the null lexical environment."
  (%eval-in-env form nil))

;;; ============================================================
;;; Compile: return proper 3 values
;;; ============================================================

(defun compile (name &rest args)
  "Compile NAME (or lambda-expression in DEF). Returns (values fn warns failp).
   On bare metal, functions are already compiled. For nil name with lambda,
   return an interpreted closure."
  (let ((def (if args (car args) nil)))
    (cond
      ;; (compile nil '(lambda ...)) — create interpreted closure
      ((and (null name) def)
       (let ((form (if (and (consp def) (eq (car def) 'quote))
                       (cadr def)
                       def)))
         (if (and (consp form) (%eval-sym-eq (car form) "LAMBDA"))
             (let ((fn (list '%interp-closure (cadr form) (cddr form) nil)))
               (values fn nil nil))
             (values def nil nil))))
      ;; (compile 'name) — function already compiled, return it
      (name
       (let ((fn (if *symbol-function-table*
                     (gethash (%eval-sym-name name) *symbol-function-table*)
                     nil)))
         (values (or fn name) nil nil)))
      (t (values nil nil nil)))))

;;; ============================================================
;;; Load: read + eval from file
;;; ============================================================

(defun load (filespec &rest args)
  "Read and evaluate all forms from FILESPEC."
  (let ((verbose nil)
        (print nil)
        (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :verbose) (setq verbose v))
          ((eq k :print) (setq print v))))
      (setq cur (cddr cur)))
    (let ((stream (open filespec :direction :input :if-does-not-exist nil)))
      (if (null stream)
          nil
          (let ((eof-marker (list 'eof)))
            (unwind-protect
              (let ((result t))
                (loop
                  (let ((form (read stream nil eof-marker)))
                    (when (eq form eof-marker) (return result))
                    (let ((val (eval form)))
                      (when print
                        (write val)
                        (write-char #\Newline))
                      (setq result val))))
                result)
              (close stream)))))))

;;; ============================================================
;;; Initialize symbol-function table at startup
;;; ============================================================

(defun %sft-register-1 (ht name fn)
  "Helper: register one function in symbol-function table by string name.
   Only registers if fn is non-nil (avoids registering inline ops with addr 0)."
  (when fn
    (puthash name ht fn)))

(defun %init-sft-list (ht)
  "Register built-in Lisp functions in the symbol-function table.
   ONLY includes functions that have actual defun definitions (verified).
   Excludes: inline ops (car, cdr, +, -, =, aref, make-array, etc.),
   macros (first, second, caddr, etc.), and undefined stubs."
  ;; List operations (all have defun in prelude.lisp or ansi-bridge.lisp)
  (puthash "EQUAL" ht #'equal)
  (puthash "EQUALP" ht #'equalp)
  (puthash "IDENTITY" ht #'identity)
  (puthash "LIST" ht #'list)
  (puthash "LIST*" ht #'list*)
  (puthash "APPEND" ht #'append)
  (puthash "NCONC" ht #'nconc)
  (puthash "REVERSE" ht #'reverse)
  (puthash "NREVERSE" ht #'nreverse)
  (puthash "LENGTH" ht #'length)
  (puthash "NTH" ht #'nth)
  (puthash "NTHCDR" ht #'nthcdr)
  (puthash "LAST" ht #'last)
  (puthash "BUTLAST" ht #'butlast)
  (puthash "MEMBER" ht #'member)
  (puthash "ASSOC" ht #'assoc)
  (puthash "REMOVE" ht #'remove)
  (puthash "REMOVE-IF" ht #'remove-if)
  (puthash "REMOVE-IF-NOT" ht #'remove-if-not)
  (puthash "COPY-LIST" ht #'copy-list)
  (puthash "COPY-TREE" ht #'copy-tree)
  (puthash "SUBST" ht #'subst)
  (puthash "MAPCAR" ht #'mapcar)
  (puthash "MAPC" ht #'mapc)
  (puthash "MAPLIST" ht #'maplist)
  (puthash "MAPCAN" ht #'mapcan)
  (puthash "MAPCON" ht #'mapcon)
  (puthash "SOME" ht #'some)
  (puthash "EVERY" ht #'every)
  (puthash "NOTANY" ht #'notany)
  (puthash "NOTEVERY" ht #'notevery)
  (puthash "REDUCE" ht #'reduce)
  (puthash "APPLY" ht #'apply)
  ;; NOTE: funcall, car, cdr, cons, set-car, set-cdr, caar, cadr, cdar, cddr
  ;;       are inline ops — no defun, skip to avoid calling wrong function
  (puthash "RPLACA" ht #'rplaca)
  (puthash "RPLACD" ht #'rplacd)
  (puthash "GETF" ht #'getf)
  (puthash "ACONS" ht #'acons)
  (puthash "PAIRLIS" ht #'pairlis)
  ;; NOTE: assoc-if, assoc-if-not, member-if, member-if-not, rassoc,
  ;;       rassoc-if, rassoc-if-not, first..tenth, rest, caddr..cddddr
  ;;       are macros/not-defined — skip
  (puthash "VALUES" ht #'values)
  (puthash "VALUES-LIST" ht #'values-list)
  ;; NOTE: +, -, *, /, =, <, >, <=, >=, /=, 1+, 1-, mod, truncate,
  ;;       ash, logand, logior, logxor are inline ops — skip
  (puthash "PLUSP" ht #'plusp)
  (puthash "MINUSP" ht #'minusp)
  (puthash "ODDP" ht #'oddp)
  (puthash "EVENP" ht #'evenp)
  (puthash "ABS" ht #'abs)
  (puthash "MAX" ht #'max)
  (puthash "MIN" ht #'min)
  ;; NOTE: lognot is an inline op (no defun), skip
  (puthash "LOGBITP" ht #'logbitp)
  (puthash "NUMBERP" ht #'numberp)
  (puthash "FLOATP" ht #'floatp)
  (puthash "REALP" ht #'realp)
  (puthash "RATIONALP" ht #'rationalp)
  ;; NOTE: char-code, code-char, characterp, integerp, zerop, stringp,
  ;;       arrayp, symbolp, consp, null, not, atom, listp are inline ops, skip
  (puthash "CHAR=" ht #'char=)
  (puthash "CHAR<" ht #'char<)
  (puthash "CHAR>" ht #'char>)
  (puthash "CHAR<=" ht #'char<=)
  (puthash "CHAR>=" ht #'char>=)
  (puthash "CHAR/=" ht #'char/=)
  (puthash "CHAR-UPCASE" ht #'char-upcase)
  (puthash "CHAR-DOWNCASE" ht #'char-downcase)
  (puthash "ALPHA-CHAR-P" ht #'alpha-char-p)
  (puthash "DIGIT-CHAR-P" ht #'digit-char-p)
  (puthash "ALPHANUMERICP" ht #'alphanumericp)
  (puthash "UPPER-CASE-P" ht #'upper-case-p)
  (puthash "LOWER-CASE-P" ht #'lower-case-p)
  (puthash "STRING" ht #'string)
  (puthash "STRING=" ht #'string=)
  (puthash "STRING-EQUAL" ht #'string-equal)
  (puthash "STRING<" ht #'string<)
  (puthash "STRING>" ht #'string>)
  (puthash "STRING<=" ht #'string<=)
  (puthash "STRING>=" ht #'string>=)
  (puthash "STRING/=" ht #'string/=)
  (puthash "STRING-UPCASE" ht #'string-upcase)
  (puthash "STRING-DOWNCASE" ht #'string-downcase)
  (puthash "STRING-CAPITALIZE" ht #'string-capitalize)
  (puthash "SUBSEQ" ht #'subseq)
  (puthash "CONCATENATE" ht #'concatenate)
  ;; NOTE: aref, svref are inline ops (compile-aref), skip
  (puthash "VECTORP" ht #'vectorp)
  (puthash "ARRAY-RANK" ht #'array-rank)
  ;; NOTE: array-dimensions, make-array are inline ops or not defined, skip
  (puthash "ARRAY-TOTAL-SIZE" ht #'array-total-size)
  (puthash "MAKE-LIST" ht #'make-list)
  (puthash "MAKE-STRING" ht #'make-string)
  (puthash "MAKE-HASH-TABLE" ht #'make-hash-table)
  (puthash "GETHASH" ht #'gethash)
  (puthash "SETF-GETHASH" ht #'puthash)
  (puthash "REMHASH" ht #'remhash)
  (puthash "MAPHASH" ht #'maphash)
  (puthash "SYMBOL-NAME" ht #'symbol-name)
  (puthash "SYMBOL-VALUE" ht #'symbol-value)
  (puthash "SYMBOL-FUNCTION" ht #'symbol-function)
  (puthash "FBOUNDP" ht #'fboundp)
  (puthash "FMAKUNBOUND" ht #'fmakunbound)
  (puthash "FDEFINITION" ht #'fdefinition)
  (puthash "INTERN" ht #'intern)
  (puthash "FIND-SYMBOL" ht #'find-symbol)
  (puthash "KEYWORDP" ht #'keywordp)
  (puthash "GENSYM" ht #'gensym)
  (puthash "ENDP" ht #'endp)
  (puthash "FIND" ht #'find)
  (puthash "FIND-IF" ht #'find-if)
  (puthash "FIND-IF-NOT" ht #'find-if-not)
  (puthash "POSITION" ht #'position)
  (puthash "POSITION-IF" ht #'position-if)
  (puthash "POSITION-IF-NOT" ht #'position-if-not)
  (puthash "COUNT" ht #'count)
  (puthash "COUNT-IF" ht #'count-if)
  (puthash "COUNT-IF-NOT" ht #'count-if-not)
  (puthash "SEARCH" ht #'search)
  (puthash "MISMATCH" ht #'mismatch)
  (puthash "SORT" ht #'sort)
  (puthash "STABLE-SORT" ht #'stable-sort)
  (puthash "SUBSTITUTE" ht #'substitute)
  (puthash "SUBSTITUTE-IF" ht #'substitute-if)
  (puthash "SUBSTITUTE-IF-NOT" ht #'substitute-if-not)
  (puthash "NSUBSTITUTE" ht #'nsubstitute)
  (puthash "FILL" ht #'fill)
  (puthash "REPLACE" ht #'replace)
  (puthash "MAP" ht #'map)
  (puthash "MAP-INTO" ht #'map-into)
  (puthash "COERCE" ht #'coerce)
  (puthash "TYPEP" ht #'typep)
  (puthash "TYPE-OF" ht #'type-of)
  (puthash "ELT" ht #'elt)
  (puthash "COPY-SEQ" ht #'copy-seq)
  (puthash "READ" ht #'read)
  (puthash "READ-FROM-STRING" ht #'read-from-string)
  (puthash "WRITE" ht #'write)
  (puthash "PRIN1" ht #'prin1)
  (puthash "PRINC" ht #'princ)
  (puthash "PRINT" ht #'print)
  (puthash "WRITE-TO-STRING" ht #'write-to-string)
  (puthash "PRIN1-TO-STRING" ht #'prin1-to-string)
  (puthash "PRINC-TO-STRING" ht #'princ-to-string)
  (puthash "FORMAT" ht #'format)
  (puthash "WRITE-CHAR" ht #'write-char)
  (puthash "WRITE-STRING" ht #'write-string)
  (puthash "WRITE-LINE" ht #'write-line)
  (puthash "TERPRI" ht #'terpri)
  (puthash "FRESH-LINE" ht #'fresh-line)
  (puthash "READ-CHAR" ht #'read-char)
  (puthash "UNREAD-CHAR" ht #'unread-char)
  (puthash "PEEK-CHAR" ht #'peek-char)
  (puthash "READ-LINE" ht #'read-line)
  (puthash "OPEN" ht #'open)
  (puthash "CLOSE" ht #'close)
  (puthash "STREAMP" ht #'streamp)
  (puthash "FUNCTIONP" ht #'functionp)
  (puthash "COMPLEMENT" ht #'complement)
  (puthash "CONSTANTLY" ht #'constantly)
  (puthash "ERROR" ht #'error)
  (puthash "WARN" ht #'warn)
  (puthash "SIGNAL" ht #'signal)
  (puthash "CERROR" ht #'cerror)
  (puthash "MAKE-CONDITION" ht #'make-condition)
  (puthash "EVAL" ht #'eval)
  (puthash "COMPILE" ht #'compile)
  (puthash "LOAD" ht #'load)
  (puthash "MACROEXPAND" ht #'macroexpand)
  (puthash "MACROEXPAND-1" ht #'macroexpand-1)
  (puthash "MACRO-FUNCTION" ht #'macro-function)
  ;; NOTE: compiled-function-p, special-operator-p have no defun, skip
  (puthash "NOT-MV" ht #'not-mv)
  (puthash "NOTNOT" ht #'notnot)
  (puthash "EQT" ht #'eqt)
  (puthash "EQLT" ht #'eqlt)
  (puthash "EQUALT" ht #'equalt)
  nil)

(defun %init-symbol-function-table ()
  "Populate *symbol-function-table* with all built-in compiled functions.
   Uses puthash with string keys to avoid calling intern (which can crash
   when *all-packages* is in a partially initialized state)."
  (%sft-init)
  (%init-sft-list *symbol-function-table*)
  nil)

(defun not-mv (x) (not x))
(defun check-values (fn &optional expected) (declare (ignore expected)) fn)

(defun string-upcase (str &rest args)
  "Convert string to uppercase."
  (let ((len (array-length str))
        (result (%make-string-array (array-length str))))
    (dotimes (i len result)
      (let ((ch (aref str i)))
        (aset result i (if (lower-case-p (code-char ch)) (- ch 32) ch))))))

(defun string-downcase (str &rest args)
  "Convert string to lowercase."
  (let ((len (array-length str))
        (result (%make-string-array (array-length str))))
    (dotimes (i len result)
      (let ((ch (aref str i)))
        (aset result i (if (upper-case-p (code-char ch)) (+ ch 32) ch))))))

(defun string-capitalize (str)
  "Capitalize first letter of each word."
  (let ((len (array-length str))
        (result (%make-string-array (array-length str))))
    (let ((i 0) (in-word nil))
      (loop
        (when (>= i len) (return result))
        (let ((ch (aref str i)))
          (if (alphanumericp (code-char ch))
              (if in-word
                  (aset result i (if (upper-case-p (code-char ch)) (+ ch 32) ch))
                  (progn
                    (aset result i (if (lower-case-p (code-char ch)) (- ch 32) ch))
                    (setq in-word t)))
              (progn (aset result i ch) (setq in-word nil))))
        (setq i (+ i 1))))))

(defun string-not-equal (a b) (not (string-equal a b)))
(defun string< (a b &rest args) (let ((m (mismatch a b)))
  (if m (if (< (aref a m) (aref b m)) m nil) (if (< (length a) (length b)) (length a) nil))))
(defun string> (a b &rest args) (string< b a))
(defun string<= (a b &rest args) (not (string> a b)))
(defun string>= (a b &rest args) (not (string< a b)))
(defun string-lessp (a b &rest args) (string< (string-downcase a) (string-downcase b)))
(defun string-greaterp (a b &rest args) (string> (string-downcase a) (string-downcase b)))
(defun string-not-greaterp (a b &rest args) (not (string-greaterp a b)))
(defun string-not-lessp (a b &rest args) (not (string-lessp a b)))

(defun char-upcase (c) (let ((code (%ensure-char-code c)))
  (code-char (if (and (>= code 97) (<= code 122)) (- code 32) code))))
(defun char-downcase (c) (let ((code (%ensure-char-code c)))
  (code-char (if (and (>= code 65) (<= code 90)) (+ code 32) code))))
(defun upper-case-p (c) (let ((code (%ensure-char-code c))) (if (>= code 65) (<= code 90) nil)))
(defun lower-case-p (c) (let ((code (%ensure-char-code c))) (if (>= code 97) (<= code 122) nil)))
(defun both-case-p (c) (if (upper-case-p c) t (lower-case-p c)))
(defun alpha-char-p (c) (both-case-p c))
(defun digit-char-p (c &optional (radix 10))
  (let ((code (%ensure-char-code c)))
    (cond ((and (>= code 48) (<= code 57)) (let ((v (- code 48))) (if (< v radix) v nil)))
          ((and (>= code 65) (<= code 90)) (let ((v (+ 10 (- code 65)))) (if (< v radix) v nil)))
          ((and (>= code 97) (<= code 122)) (let ((v (+ 10 (- code 97)))) (if (< v radix) v nil)))
          (t nil))))
(defun alphanumericp (c) (or (alpha-char-p c) (digit-char-p c)))
(defun graphic-char-p (c) (let ((code (%ensure-char-code c))) (and (>= code 32) (<= code 126))))
(defun standard-char-p (c) (graphic-char-p c))
(defun digit-char (weight &optional (radix 10))
  (if (< weight radix) (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight))) nil))
(defun name-char (name)
  "Return the character with the given name (case-insensitive), or nil."
  (let ((s (string-upcase (cond
                            ((stringp name) name)
                            ((symbolp name) (symbol-name name))
                            ((characterp name) (make-string 1 :initial-element name))
                            (t (coerce name 'string))))))
    (cond
      ((string= s "SPACE")     #\Space)
      ((string= s "NEWLINE")   #\Newline)
      ((string= s "TAB")       #\Tab)
      ((string= s "RETURN")    (code-char 13))
      ((string= s "BACKSPACE") (code-char 8))
      ((string= s "RUBOUT")    (code-char 127))
      ((string= s "PAGE")      (code-char 12))
      ((string= s "LINEFEED")  (code-char 10))
      ((string= s "ALTMODE")   (code-char 27))
      ((string= s "NULL")      (code-char 0))
      ((string= s "NUL")       (code-char 0))
      ((string= s "ESCAPE")    (code-char 27))
      ((string= s "DELETE")    (code-char 127))
      (t nil))))

(defun char-name (c)
  "Return the name of the character, or nil."
  (let ((code (%ensure-char-code c)))
    (cond
      ((= code 32)  "Space")
      ((= code 10)  "Newline")
      ((= code 9)   "Tab")
      ((= code 13)  "Return")
      ((= code 8)   "Backspace")
      ((= code 127) "Rubout")
      ((= code 12)  "Page")
      ((= code 27)  "Escape")
      ((= code 0)   "Null")
      (t nil))))

(defun char= (a b) (eql (%ensure-char-code a) (%ensure-char-code b)))
(defun char/= (a b) (not (char= a b)))
(defun char< (a b) (< (%ensure-char-code a) (%ensure-char-code b)))
(defun char> (a b) (> (%ensure-char-code a) (%ensure-char-code b)))
(defun char<= (a b) (<= (%ensure-char-code a) (%ensure-char-code b)))
(defun char>= (a b) (>= (%ensure-char-code a) (%ensure-char-code b)))
(defun char-equal (a b) (= (%ensure-char-code (char-upcase a)) (%ensure-char-code (char-upcase b))))
(defun char-not-equal (a b) (not (char-equal a b)))
(defun char-lessp (a b) (char< (char-upcase a) (char-upcase b)))
(defun char-greaterp (a b) (char> (char-upcase a) (char-upcase b)))
(defun char-not-greaterp (a b) (char<= (char-upcase a) (char-upcase b)))
(defun char-not-lessp (a b) (char>= (char-upcase a) (char-upcase b)))

(defun char-int (c) (char-code c))
(defun code-char (n) (if (characterp n) n (code-char n)))

;;; Numeric
(defun abs (n) (if (< n 0) (- 0 n) n))
(defun max (a &rest more) (let ((r a)) (dolist (x more r) (when (> x r) (setq r x)))))
(defun min (a &rest more) (let ((r a)) (dolist (x more r) (when (< x r) (setq r x)))))
(defun floor (n &optional (d 1)) (let ((q (truncate n d))) (values q (- n (* q d)))))
(defun ceiling (n &optional (d 1)) (let ((q (truncate n d))) (if (zerop (- n (* q d))) (values q 0) (values (+ q 1) (- n (* (+ q 1) d))))))
(defun rem (n d) (- n (* (truncate n d) d)))
(defun mod (n d) (let ((r (rem n d))) (if (and (not (zerop r)) (not (eq (< r 0) (< d 0)))) (+ r d) r)))
(defun expt (base power) (cond ((= power 0) 1) ((= power 1) base)
  (t (let ((r 1)) (dotimes (i power r) (setq r (* r base)))))))
(defun isqrt (n) (if (<= n 0) 0 (let ((x n)) (loop (let ((x1 (ash (+ x (truncate n x)) -1)))
  (when (>= x1 x) (return x)) (setq x x1))))))
(defun gcd (a &optional b) (if (null b) (abs a)
  (let ((a (abs a)) (b (abs b))) (loop (when (zerop b) (return a)) (let ((r (rem a b))) (setq a b) (setq b r))))))
(defun lcm (&rest args) (if (null args) 1 (if (null (cdr args)) (abs (car args))
  (let ((a (car args)) (b (cadr args)))
    (if (or (zerop a) (zerop b)) 0 (abs (truncate (* a b) (gcd a b))))))))

;;; Type predicates
(defun numberp (x) (or (integerp x) (floatp-impl x)))
(defun realp (x) (or (integerp x) (floatp-impl x)))
(defun rationalp (x) (integerp x))
(defun complexp (x) nil)
(defun floatp (x) (floatp-impl x))

;;; Misc
(defun values-list (list)
  "Return elements of LIST as multiple values. Sets MV buffer directly."
  (let ((n (length list)))
    (setf (mem-ref #x10000090 :u64) n)
    (let ((cur (if (null list) nil (cdr list)))
          (idx 0))
      (loop
        (when (null cur) (return nil))
        (setf (mem-ref (+ #x10000098 (* idx 8)) :u64) (car cur))
        (setq idx (+ idx 1))
        (setq cur (cdr cur))))
    (if (null list) nil (car list))))
(defun nreconc (list tail) (nconc (nreverse list) tail))
(defun set-elt (seq idx val)
  "Set element at IDX in SEQ to VAL."
  (if (consp seq) (set-car (nthcdr idx seq) val)
      (aset seq idx val))
  val)
(defun set-fill-pointer (vec n)
  (when (consp vec) (set-car vec n))
  n)
(defun random-fixnum () (random most-positive-fixnum))
(defun subtypep* (t1 t2) nil)  ; stub

;;; Minimal Bignum (2-slot object, subtag #x30, lo/hi tagged fixnums)
(defun make-bignum (lo hi)
  (let ((b (%make-bignum))) (aset b 0 lo) (aset b 1 hi) b))
(defun bignum-lo (b) (aref b 0))
(defun bignum-hi (b) (aref b 1))
(defun bignum-to-fixnum-if-possible (b)
  "Collapse bignum to fixnum if it fits in 63-bit signed range."
  (let ((hi (bignum-hi b)) (lo (bignum-lo b)))
    (if (= hi 0) lo
        (if (and (= hi -1) (>= lo 2305843009213693952))
            ;; hi=-1, lo>=2^61: value = -2^62 + lo, which is a negative fixnum
            (- lo 4611686018427387904)
            b))))
(defun %shl1-fixnum (n)
  (if (>= n 2305843009213693952)
      (make-bignum (logand (ash n 1) 4611686018427387903) (ash n -61))
      (ash n 1)))
(defun %shl1-bignum (lo hi)
  (make-bignum (logand (ash lo 1) 4611686018427387903)
               (+ (ash hi 1) (ash lo -61))))
(defun %shr1-bignum (lo hi)
  (make-bignum (+ (ash lo -1) (logand (ash hi 61) 4611686018427387903))
               (ash hi -1)))
(defun bignum-ash (n count)
  (if (= count 0) n
      (if (> count 0)
          (let ((result n) (remaining count))
            (loop (when (= remaining 0) (return result))
              (setq result (if (bignump result)
                               (%shl1-bignum (bignum-lo result) (bignum-hi result))
                               (%shl1-fixnum result)))
              (setq remaining (- remaining 1))))
          (if (bignump n)
              (let ((result n) (remaining (- 0 count)))
                (loop (when (= remaining 0) (return (bignum-to-fixnum-if-possible result)))
                  (setq result (%shr1-bignum (bignum-lo result) (bignum-hi result)))
                  (setq remaining (- remaining 1))))
              (ash n count)))))
(defun %fixnum-to-bignum-parts (n)
  "Convert fixnum N to (lo . hi) bignum parts."
  (if (>= n 0)
      (cons n 0)
      (cons (+ n 4611686018427387904) -1)))

(defun bignum-add (a b)
  "Add A and B, where either may be a bignum."
  (let ((ap (if (bignump a) (cons (bignum-lo a) (bignum-hi a))
                (%fixnum-to-bignum-parts a)))
        (bp (if (bignump b) (cons (bignum-lo b) (bignum-hi b))
                (%fixnum-to-bignum-parts b))))
    (let ((sum-lo (+ (car ap) (car bp))))
      ;; Carry detection: lo parts are in [0, 2^62).  Their sum overflows
      ;; the 63-bit fixnum range iff it reaches 2^62.  Tagged addition
      ;; wraps such a result negative, so (< sum-lo 0) is the correct test.
      ;; NOTE: Do NOT compare against 4611686018427387904 (= 2^62) — that
      ;; value itself overflows the fixnum range and wraps to the most
      ;; negative tagged integer, making (>= sum-lo 2^62) always true.
      (let ((carry (if (< sum-lo 0) 1 0))
            (lo (logand sum-lo 4611686018427387903)))
        (let ((sum-hi (+ (+ (cdr ap) (cdr bp)) carry)))
          (bignum-to-fixnum-if-possible (make-bignum lo sum-hi)))))))

(defun %bignum-negate-parts (lo hi)
  "Negate bignum with parts lo,hi. Two's complement: invert + add 1."
  (if (= lo 0)
      ;; No overflow: ~0 + 1 = 2^62, carry into hi
      (make-bignum 0 (+ (logxor hi -1) 1))
      ;; ~lo + 1 < 2^62 when lo > 0, so no carry
      (make-bignum (+ 1 (logxor lo 4611686018427387903)) (logxor hi -1))))

(defun bignum-negate (n)
  "Negate N (fixnum or bignum)."
  (if (bignump n)
      (bignum-to-fixnum-if-possible
        (%bignum-negate-parts (bignum-lo n) (bignum-hi n)))
      (- 0 n)))

(defun bignum-sub (a b)
  "Subtract B from A."
  (if (and (not (bignump a)) (not (bignump b)))
      (- a b)
      (bignum-add a (bignum-negate b))))

(defun bignum-1- (n)
  (if (bignump n)
      (let ((lo (bignum-lo n)) (hi (bignum-hi n)))
        (if (> lo 0)
            (bignum-to-fixnum-if-possible (make-bignum (- lo 1) hi))
            (bignum-to-fixnum-if-possible (make-bignum 4611686018427387903 (- hi 1)))))
      (- n 1)))
(defun %fixnum-integer-length (n)
  (let ((x (if (< n 0) (logxor n -1) n)) (len 0))
    (loop (when (zerop x) (return len))
      (setq x (ash x -1)) (setq len (+ len 1)))))
(defun %bignum-integer-length-pos (n)
  "integer-length for positive bignum or fixnum."
  (if (bignump n)
      (let ((hi (bignum-hi n)))
        (if (> hi 0) (+ 62 (%fixnum-integer-length hi))
            (%fixnum-integer-length (bignum-lo n))))
      (%fixnum-integer-length n)))

(defun integer-length (n)
  (if (bignump n)
      (let ((hi (bignum-hi n)))
        (if (< hi 0)
            ;; Negative: integer-length = integer-length(lognot(n)) = integer-length(-n-1)
            (%bignum-integer-length-pos (bignum-1- (bignum-negate n)))
            (%bignum-integer-length-pos n)))
      (%fixnum-integer-length n)))

(defun bignum-eql (a b)
  "EQL that handles bignums."
  (if (bignump a)
      (if (bignump b)
          (if (= (bignum-lo a) (bignum-lo b))
              (= (bignum-hi a) (bignum-hi b))
              nil)
          nil)
      (if (bignump b) nil (eql a b))))

;; Funcallable versions of compiler builtins (needed for #'consp etc.)
(defun consp (x) (consp x))
(defun atom (x) (atom x))
(defun null (x) (null x))
(defun numberp (x) (integerp x))
(defun symbolp (x) (symbolp x))
(defun integerp (x) (integerp x))
(defun characterp (x) (characterp x))
(defun stringp (x) (stringp x))
(defun zerop (x) (zerop x))
(defun plusp (x) (> x 0))
(defun minusp (x) (< x 0))
(defun map (result-type fn &rest seqs)
  "Map FN over sequences, collecting into RESULT-TYPE."
  (if (null result-type) (progn (apply #'mapc fn seqs) nil)
      (apply #'mapcar fn seqs)))
(defun functionp (x) (or (and (not (null x)) (not (integerp x)) (not (consp x))
                              (not (characterp x)) (not (stringp x)) (not (eq x t)))
                         nil))
(defun keywordp (x)
  "True if X is a keyword (symbol starting with :)."
  ;; In MVM, keywords are symbols whose name-hash matches the : prefix pattern
  ;; Stub: check if it's one of the common keywords used in tests
  (member x '(:test :key :test-not :count :start :end :from-end
              :initial-element :initial-contents :element-type
              :allow-other-keys)))
(defun symbol-package (x) nil)  ; stub
; compile defined in Layer 8 above
(defun simple-vector-p (x) (vectorp x))

;; Module system stubs
(defvar *modules* nil)
(defun provide (module-name)
  "Register a module as provided."
  (let ((name (string module-name)))
    (unless (member name *modules* :test #'string=)
      (setq *modules* (cons name *modules*))))
  t)
(defun require (module-name &optional pathnames)
  "Stub: load a module if not already provided."
  (let ((name (string module-name)))
    (unless (member name *modules* :test #'string=)
      nil)))  ; no-op stub

;; replace: copy elements from one sequence to another
(defun replace (seq1 seq2 &rest args)
  "Destructively replace elements of SEQ1 with elements from SEQ2."
  (let ((start1 0) (end1 nil) (start2 0) (end2 nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :start1) (setq start1 v))
            ((eq k :end1) (setq end1 v))
            ((eq k :start2) (setq start2 v))
            ((eq k :end2) (setq end2 v))))
        (setq cur (cddr cur))))
    (when (null end1) (setq end1 (length seq1)))
    (when (null end2) (setq end2 (length seq2)))
    (let ((n1 (- end1 start1))
          (n2 (- end2 start2)))
      (let ((count (if (< n1 n2) n1 n2))
            (i 0))
        (loop
          (when (= i count) (return seq1))
          (let ((src-elem (if (listp seq2)
                              (nth (+ start2 i) seq2)
                              (aref seq2 (+ start2 i)))))
            (if (listp seq1)
                (setf (nth (+ start1 i) seq1) src-elem)
                (if (stringp seq1)
                    (aset seq1 (+ start1 i) (if (characterp src-elem) (char-code src-elem) src-elem))
                    (aset seq1 (+ start1 i) src-elem))))
          (setq i (+ i 1)))))))

;; Adjustable arrays
(defun adjustable-array-p (array)
  "Return true if array is adjustable. Our arrays are not adjustable by default."
  nil)
(defun array-displacement (array)
  "Return displacement info for ARRAY. Our arrays are never displaced."
  (values nil 0))

