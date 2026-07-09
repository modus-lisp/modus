;;;; tree-walker.lisp — the LEGACY tree-walking evaluator (%eval-in-env).
;;;;
;;;; WS3 STEP 4 (2026-07-09): production eval is eval2 (compile to MVM
;;;; bytecode via the self-hosted compiler + mvm-interpret); this engine was
;;;; extracted VERBATIM from cl-eval.lisp (STEP 4a: QUARANTINE).  Loaded by:
;;;;   - the legacy fork builds (build-aarch64-ansi-test,
;;;;     build-aarch64-linux-ansi-test, build-x64-modus-ansi-test),
;;;;     where it IS the eval engine — each
;;;;     defines the (defun eval2 (form) (%eval-in-env form nil)) bridge in
;;;;     its own source list, and the two engine-provider overrides at the
;;;;     bottom of this file win last-defun-wins (load AFTER ansi-bridge).
;;;;   - the PRODUCTION images (build-ansi-test, build-generic), where it is
;;;;     ONLY the %e2ic fallback for interp-closure shapes eval2's
;;;;     %e2ic-compile cannot yet serve (loaded BEFORE eval2.lisp so eval2's
;;;;     overrides win; *e2ic-fallback-count* measures the remaining
;;;;     inventory).  STEP 4b — dropping this file from the production
;;;;     builds — is gated on that counter reaching ZERO on the full
;;;;     corpus + gauntlet.

(defun %eval-global-get (name)
  "Look up global variable by name string. Returns (found-p . value)."
  (let ((cur *eval-global-env*))
    (loop
      (when (null cur) (return (cons nil nil)))
      (let ((pair (car cur)))
        (when (string-equal (car pair) name)
          (return (cons t (cdr pair)))))
      (setq cur (cdr cur)))))

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

(defun %env-extend-pair (pair env)
  "Add the EXISTING binding cons PAIR (sym . val) to front of ENV.
   Unlike %env-extend, no fresh cons is made for the binding — so a
   SETQ that mutates the binding via set-cdr persists through every
   env built on this same PAIR.  Used for LOOP :WITH vars, whose
   value must survive re-extension across iterations (CLHS 6.1.1.3
   — a WITH var is bound once and mutable in the body)."
  (cons pair env))

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

(defun %earmuff-name-p (name)
  "T if NAME is a string of length ≥2 starting AND ending with `*`."
  (and (stringp name)
       (>= (length name) 2)
       (char= (char name 0) #\*)
       (char= (char name (1- (length name))) #\*)))

(defun %earmuff-var-p (sym)
  "T if SYM is an *earmuffs* symbol — treat as special in LET/LET*."
  (let ((n (%eval-sym-name sym)))
    (and n (%earmuff-name-p n))))

(defun %eval-save-special (sym)
  "Return current global value of SYM by reading the SAME store
   compiled code reads (symbol-value's hash-keyed alist at #x10000080).
   Reading the eval-only alist would mis-restore — compiled defvars
   like *print-base* are initialized into the hash alist but never
   written to the eval-only alist, so a save/restore via the eval-only
   alist sees :%unbound and restores to NIL, breaking output.  Caller
   uses this with %eval-restore-special inside an unwind-protect."
  (let ((nm (%eval-sym-name sym)))
    (if nm (symbol-value (compute-name-hash nm)) nil)))

(defun %eval-restore-special (sym saved)
  "Restore a saved special-var value to BOTH the compiled-code hash
   alist AND the eval-only alist (mirrors %eval-global-set's two-store
   write so the next lookup is consistent)."
  (let ((nm (%eval-sym-name sym)))
    (when nm (%eval-global-set nm saved))))

(defun %eval-set-special (sym value)
  "Establish a fresh dynamic binding for SYM at VALUE.  Writes the
   eval-alist + compiled-code symbol-value alist so any compiled
   reader (e.g. compiled PRIN1 reading *print-base*) sees it."
  (let ((nm (%eval-sym-name sym)))
    (when nm (%eval-global-set nm value))))

(defun %eval-args (arg-forms env)
  "Evaluate a list of argument forms."
  (let ((result nil)
        (cur arg-forms))
    (loop
      (when (null cur) (return (nreverse result)))
      (setq result (cons (%eval-in-env (car cur) env) result))
      (setq cur (cdr cur)))))

(defun %return-from-name-match-p (name block-name)
  "True if RETURN-FROM target NAME refers to BLOCK-NAME.  Both may be
   native MVM syms (compare by name) or CL syms / lists (setf names)."
  (cond
    ((and (consp name) (consp block-name))
     ;; (setf foo) names — match on the foo part by name.
     (and (cdr name) (cdr block-name)
          (let ((n1 (%eval-sym-name (cadr name)))
                (n2 (%eval-sym-name (cadr block-name))))
            (and n1 n2 (string-equal n1 n2)))))
    ((or (consp name) (consp block-name)) nil)
    (t (let ((n1 (%eval-sym-name name))
             (n2 (%eval-sym-name block-name)))
         (and n1 n2 (string-equal n1 n2))))))

(defun %body-returns-from-p (body block-name)
  "Conservatively scan BODY (a form or list of forms) for a
   (RETURN-FROM BLOCK-NAME …) anywhere in the tree, so DEFUN can decide
   whether the function's implicit BLOCK must be materialised.  Walks
   conses; ignores quoted data only at the QUOTE-head level."
  (cond
    ((not (consp body)) nil)
    ;; (return-from NAME …) with a matching NAME?
    ((and (%eval-sym-eq (car body) "RETURN-FROM")
          (consp (cdr body))
          (%return-from-name-match-p (cadr body) block-name))
     t)
    ;; Don't descend into (quote …) literal data.
    ((%eval-sym-eq (car body) "QUOTE") nil)
    ;; Otherwise recurse car + cdr.
    (t (or (%body-returns-from-p (car body) block-name)
           (%body-returns-from-p (cdr body) block-name)))))

(defun %call-interp-closure-walker (fn args)
  "Call an interpreted closure on the TREE-WALKER: bind the lambda list to
   ARGS in the captured env and %eval-progn the body.  This is the walker
   engine proper; %call-interp-closure (the entry every call site uses) is a
   thin wrapper here, OVERRIDDEN in eval2.lisp (last-defun-wins, eval2-enabled
   images only) with an eval2-first dispatcher that compiles the closure body
   against its captured env and falls back to this function for shapes eval2
   can't serve (WS3 Phase 3 — retiring the tree-walker)."
  ;; fn = (%interp-closure params body env)
  (let ((params (cadr fn))
        (body (caddr fn))
        (closed-env (cadddr fn)))
    (let ((new-env (%bind-params params args closed-env)))
      (%eval-progn body new-env))))

(defun %bind-params (params args env)
  "Bind PARAMS to ARGS in ENV, handling &optional / &rest / &key / &aux.
   Per CLHS 3.4.1 the lambda-list syntax is:
     (required* [&optional optional*] [&rest var] [&key key* [&allow-other-keys]] [&aux aux*])
   &key entries are :KEY-symbol designators; each may be (var [init [suppliedp]])
   or just var.  &aux entries are (var [init]) and are NOT consumed from ARGS."
  (let ((new-env env)
        (ps params)
        (as args)
        (rest-list nil)
        ;; Section tracker (CLHS 3.4.1): we start in the REQUIRED section.
        ;; A cons in the REQUIRED section is a nested sub-pattern to
        ;; destructure (`(level)` -> bind LEVEL to the CAR of the arg);
        ;; the SAME cons shape in the &optional/&key section is a
        ;; (var init [supplied-p]) spec.  These are syntactically
        ;; ambiguous, so the nested-required branch below must ONLY fire
        ;; while IN-REQUIRED.  Cleared by the first
        ;; &optional/&rest/&body/&key/&aux marker.
        (in-required t))
    (loop
      (cond
        ((null ps) (return new-env))
        ;; Dotted lambda-list tail — `(a b . rest)` binds REST to the
        ;; remaining ARGS, exactly like &rest.  DESTRUCTURING-BIND and
        ;; macro lambda-lists (e.g. ASDF's
        ;; `(destructuring-bind (car . cdr) form …)`) use this shape.
        ((and ps (not (consp ps)))
         (setq new-env (%env-extend ps as new-env))
         (return new-env))
        ;; &rest / &body parameter — bind to remainder of ARGS but keep
        ;; walking so &key after &rest still binds normally.  &body is a
        ;; synonym for &rest in macro lambda-lists (CLHS 3.4.4) and ASDF's
        ;; with-upgradability uses `((&optional) &body body)`.
        ((or (%eval-sym-eq (car ps) "&REST") (%eval-sym-eq (car ps) "&BODY"))
         (setq in-required nil)
         (setq ps (cdr ps))
         (when ps
           (setq rest-list as)
           ;; The &rest var may itself be a destructuring pattern.
           (let ((rvar (car ps)))
             (if (consp rvar)
                 (setq new-env (%loop-bind-pattern rvar as new-env))
                 (setq new-env (%env-extend rvar as new-env))))
           (setq ps (cdr ps))))
        ;; &optional — fall through; subsequent positionals consume from AS
        ;; with NIL fallback.
        ((%eval-sym-eq (car ps) "&OPTIONAL")
         (setq in-required nil)
         (setq ps (cdr ps)))
        ;; &key — every remaining param (until &aux or end) is matched by
        ;; :KEY-name in (rest-list or AS).
        ((%eval-sym-eq (car ps) "&KEY")
         (setq in-required nil)
         (setq ps (cdr ps))
         (let ((kw-args (or rest-list as)))
           (loop
             (cond
               ((null ps) (return new-env))
               ((%eval-sym-eq (car ps) "&ALLOW-OTHER-KEYS")
                (setq ps (cdr ps)))
               ((%eval-sym-eq (car ps) "&AUX")
                (return nil))   ; outer loop's &AUX clause picks up
               (t
                ;; Spec shapes: var | (var [init [supplied-p]]) |
                ;; ((:keyword var) [init [supplied-p]]).  The last form
                ;; (CLHS 3.4.1) names the keyword indicator explicitly,
                ;; separate from the bound variable.  Mirror the compile-
                ;; time path (preprocess-params): when (car spec) is itself
                ;; a cons, it is (kw-indicator var) and the keyword name
                ;; comes from that indicator, not from VAR.
                (let* ((spec (car ps))
                       (named (and (consp spec) (consp (car spec))))
                       (var (if named (cadr (car spec))
                                (if (consp spec) (car spec) spec)))
                       ;; Non-named keyword indicator is the VAR symbol (not
                       ;; the whole spec cons — (symbol-name '(b 3)) is garbage
                       ;; and silently un-binds (b 3)-style &key specs, which
                       ;; broke defmethod &key value binding).
                       (kw-spec (if named (car (car spec)) var))
                       (init (if (and (consp spec) (cdr spec)) (cadr spec) nil))
                       (supplied-p-var (if (and (consp spec) (cdr spec) (cddr spec))
                                           (caddr spec) nil))
                       (keyname (symbol-name kw-spec))
                       (found-flag nil)
                       (value (let ((cur kw-args) (found nil) (val nil))
                                (loop
                                  (when (or found (null cur) (null (cdr cur))) (return val))
                                  (let ((k (car cur)))
                                    (when (and k (or (symbolp k) (%cl-sym-p k))
                                               (string= (symbol-name k) keyname))
                                      (setq val (cadr cur))
                                      (setq found t)
                                      (setq found-flag t)))
                                  (setq cur (cddr cur))))))
                  ;; Bind var: use the matched value when found, else init
                  ;; evaluated in the partial env (so later defaults can
                  ;; reference earlier params).
                  (cond
                    (found-flag
                     (setq new-env (%env-extend var value new-env)))
                    (init
                     (setq new-env (%env-extend var
                                                (%eval-in-env init new-env)
                                                new-env)))
                    (t (setq new-env (%env-extend var nil new-env))))
                  ;; Bind the supplied-p flag if the spec named one.
                  (when supplied-p-var
                    (setq new-env (%env-extend supplied-p-var
                                               found-flag new-env)))
                  (setq ps (cdr ps)))))))
         ;; If the inner loop stopped because it hit &AUX, leave ps at
         ;; &AUX and let the outer loop pick it up.  If it stopped at
         ;; (null ps), the next outer iter's (null ps) branch returns.
         ;; Either way: don't return here — falling through continues
         ;; the outer loop.
         )
        ;; &aux — bind each subsequent (var init) without consuming AS.
        ((%eval-sym-eq (car ps) "&AUX")
         (setq in-required nil)
         (setq ps (cdr ps))
         (loop
           (when (null ps) (return new-env))
           (let* ((spec (car ps))
                  (var (if (consp spec) (car spec) spec))
                  (init (if (and (consp spec) (cdr spec)) (cadr spec) nil)))
             (setq new-env (%env-extend var (if init (%eval-in-env init new-env) nil) new-env))
             (setq ps (cdr ps))))
         (return new-env))
        ;; Nested REQUIRED sub-pattern in a (macro) lambda-list — a cons
        ;; in the REQUIRED section (before any &optional/&rest/&key/&aux)
        ;; is NEVER an (var init) spec; it is a sub-lambda-list to
        ;; destructure against the corresponding ARG (CLHS 3.4.4.1).
        ;; e.g. with-deprecation's `((level) &body definitions)`: the
        ;; first arg `(version-deprecation ...)` must be destructured so
        ;; LEVEL = (version-deprecation ...), NOT the whole 1-elt list.
        ;; SECTION-AWARENESS is load-bearing: the same `(level)` shape in
        ;; the &optional/&key section means `(var init)` and must fall to
        ;; the positional `(t ...)` branch — so this branch is gated on
        ;; IN-REQUIRED.  Recurse via %bind-params (not %loop-bind-pattern)
        ;; so the sub-pattern may itself contain &optional/&rest/&key.
        ((and in-required (consp (car ps)))
         (let ((subpat (car ps))
               (subarg (if as (car as) nil)))
           (setq new-env (%bind-params subpat
                                       (if (consp subarg) subarg nil)
                                       new-env))
           (setq ps (cdr ps))
           (setq as (if as (cdr as) nil))))
        ;; Nested destructuring pattern in a (macro) lambda-list — a cons
        ;; whose CAR is itself a list or a lambda-list keyword can't be an
        ;; &optional (var init) spec; it's a sub-pattern destructured
        ;; against the corresponding ARG.  Covers ASDF's
        ;; `((&optional) &body body)` (the empty (&optional) pattern
        ;; consumes one arg and binds nothing useful).
        ((and (consp (car ps))
              (let ((h (car (car ps))))
                (and (or (symbolp h) (%cl-sym-p h))
                     (let ((n (symbol-name h)))
                       (and n (> (length n) 0)
                            (char= (char n 0) #\&))))))
         (let ((pat (car ps)))
           (setq new-env (%loop-bind-pattern pat (if as (car as) nil) new-env))
           (setq ps (cdr ps))
           (setq as (if as (cdr as) nil))))
        ;; Regular / &optional positional parameter
        ;; &optional spec shapes: var | (var [init [supplied-p]])
        (t
         (let* ((spec (car ps))
                (var (if (consp spec) (car spec) spec))
                (init (if (and (consp spec) (cdr spec)) (cadr spec) nil))
                (supplied-p-var (if (and (consp spec) (cdr spec) (cddr spec))
                                    (caddr spec) nil))
                (have-arg as)
                (val (cond (as (car as))
                           (init (%eval-in-env init new-env))
                           (t nil))))
           (setq new-env (%env-extend var val new-env))
           (when supplied-p-var
             (setq new-env (%env-extend supplied-p-var
                                        (if have-arg t nil) new-env)))
           (setq ps (cdr ps))
           (setq as (if as (cdr as) nil))))))))

(defun %eval-function-form (name-or-lambda env)
  "Evaluate a #'x or (function x) form."
  (if (and (consp name-or-lambda) (%eval-sym-eq (car name-or-lambda) "LAMBDA"))
      ;; (function (lambda ...)) → interpreted closure
      (let ((params (cadr name-or-lambda))
            (body (cddr name-or-lambda)))
        (list '%interp-closure params body env))
      ;; (function name): check the LOCAL function environment (FLET/LABELS)
      ;; FIRST — CLHS §3.1.2.1.2.3 — then fall back to the global function
      ;; table.  FLET/LABELS bind each local function into ENV (via
      ;; %env-extend, keyed by the function-name symbol) as an %interp-closure,
      ;; exactly like %eval-funcall resolves a local CALL.  Without this,
      ;; #'local-fn (e.g. (position-if #'separatorp ...) inside uiop's
      ;; SPLIT-STRING) errored "undefined function" — and in runtime EVAL that
      ;; abort is non-local and corrupts in-flight state (e.g. an open LOAD
      ;; stream), which was the ASDF-gauntlet form-109 wall.
      (let ((local (%env-lookup name-or-lambda env)))
        (if (and (car local) (%interp-closure-p (cdr local)))
            (cdr local)
            (let ((name (%eval-sym-name name-or-lambda)))
              (if name
                  (let ((fn (if *symbol-function-table*
                                (gethash name *symbol-function-table*)
                                nil)))
                    (or fn (error "undefined function")))
                  name-or-lambda))))))

(defun %eval-escape-push (tag value)
  "Push an escape descriptor and signal."
  (setq *%eval-escape-stack* (cons (cons tag value) *%eval-escape-stack*))
  (error "%eval-escape"))

(defun %eval-escape-pop-if (tag)
  "If the top-of-stack escape's TAG matches, pop and return its value.
   Returns the special value :%eval-no-escape if no match — caller
   should re-signal.  Values pushed by RETURN / RETURN-FROM are stored
   as (cons '%mvs mv-list); callers that propagate them should pass
   the raw cons through %eval-escape-return so it surfaces multiple
   values via (apply #'values …)."
  (cond
    ((and *%eval-escape-stack*
          (let ((top (car *%eval-escape-stack*)))
            (or (eq (car top) tag)
                ;; LOOP's tag T matches RETURN's tag (which uses T too).
                ;; Symbol equality otherwise.
                (and (symbolp (car top)) (symbolp tag)
                     (eq (car top) tag)))))
     (let ((val (cdr (car *%eval-escape-stack*))))
       (setq *%eval-escape-stack* (cdr *%eval-escape-stack*))
       val))
    (t :%eval-no-escape)))

(defun %eval-escape-return (val)
  "Surface VAL as the caller's return value.  When VAL is the
   (cons '%mvs mv-list) marker from RETURN / RETURN-FROM, apply #'values
   to unwrap multiple values; otherwise return VAL as-is."
  (cond
    ((and (consp val) (eq (car val) '%mvs))
     (apply #'values (cdr val)))
    (t val)))

(defun %eval-escape-snapshot ()
  "Return the current *%eval-escape-stack* cons as a snapshot marker.
   Capture this BEFORE evaluating a construct's body; pass it to
   %eval-escape-unwind on the error-reraise path so any descriptor
   pushed-but-not-consumed during the failed body eval is discarded.

   Why this is safe: an escape (RETURN-FROM/RETURN/GO/THROW) pushes a
   descriptor and IMMEDIATELY signals \"%eval-escape\"; the nearest
   matching catcher pops it.  So during normal execution the stack only
   ever holds descriptors at-or-below the dynamically enclosing catchers'
   snapshots.  When a GENUINE (non-escape) error C propagates through a
   catcher, anything ABOVE that catcher's snapshot is therefore a stale
   descriptor stranded by an escape whose own catcher was skipped because
   C unwound past it first.  Truncating back to the snapshot reclaims
   exactly those strays without disturbing legitimately-pending outer
   escapes (which live at or below the snapshot)."
  *%eval-escape-stack*)

(defun %eval-escape-error-p (c)
  "T iff condition C is the host \"%eval-escape\" SIMPLE-ERROR that the
   tree-walker uses to lower a non-local exit (RETURN-FROM/GO/THROW/RETURN).
   When such a condition is in flight, the top-of-*%eval-escape-stack*
   descriptor is LIVE — an escape heading to an outer catcher — and MUST
   NOT be reclaimed by %eval-escape-unwind.  Only a GENUINE error
   propagating past a catcher leaves stranded descriptors to reclaim."
  (and (%condition-p c)
       (let ((fc (handler-case (simple-condition-format-control c)
                   (t (c2) (declare (ignore c2)) nil))))
         (and (stringp fc) (string= fc "%eval-escape")))))

(defun %eval-escape-unwind (snap c)
  "When a GENUINE error C (NOT an in-flight %eval-escape) is being
   re-raised past a catcher, restore *%eval-escape-stack* to SNAP,
   discarding any descriptors an escape stranded above it while C unwound
   past that escape's own catcher.  No-op if C IS a live %eval-escape (its
   descriptor is heading to an outer catcher) or the stack is already
   at-or-below SNAP."
  (unless (%eval-escape-error-p c)
    (let ((cur *%eval-escape-stack*))
      ;; Walk from the head; if we reach SNAP, the head..SNAP region is
      ;; the set of strays — drop them by resetting to SNAP.  If we don't
      ;; reach SNAP (stack already shorter), leave it untouched.
      (loop
        (when (eq cur snap)
          (setq *%eval-escape-stack* snap)
          (return nil))
        (when (null cur)
          ;; SNAP no longer on the stack (it was consumed) — don't grow it
          ;; back; leave the current (shorter) stack as-is.
          (return nil))
        (setq cur (cdr cur))))))

(defun %loop-keyword-p (sym)
  "True if SYM is a top-level extended-LOOP keyword.  Used to decide
   whether `(loop X …)' is the new extended path or the old simple
   `repeat body forever' path."
  (let ((n (%eval-sym-name sym)))
    (and n
         (or (string-equal n "FOR")
             (string-equal n "AS")
             (string-equal n "WHILE")
             (string-equal n "UNTIL")
             (string-equal n "REPEAT")
             (string-equal n "WITH")
             (string-equal n "INITIALLY")
             (string-equal n "DO")
             (string-equal n "DOING")
             (string-equal n "COLLECT")
             (string-equal n "COLLECTING")
             (string-equal n "SUM")
             (string-equal n "COUNT")
             (string-equal n "MINIMIZE")
             (string-equal n "MAXIMIZE")
             (string-equal n "APPEND")
             (string-equal n "APPENDING")
             (string-equal n "NCONC")
             (string-equal n "NCONCING")
             (string-equal n "WHEN")
             (string-equal n "IF")
             (string-equal n "UNLESS")
             (string-equal n "ALWAYS")
             (string-equal n "NEVER")
             (string-equal n "THEREIS")
             (string-equal n "NAMED")
             (string-equal n "RETURN")))))

(defun %loop-kw= (sym name)
  "Symbol-name match without forcing intern semantics."
  (and (symbolp sym)
       (let ((n (%eval-sym-name sym))) (and n (string-equal n name)))))

(defun %eval-extended-loop (clauses env)
  "Parse and execute an extended-LOOP body.  CLAUSES is the cdr of the
   user form (everything after the LOOP head)."
  (let ((iters nil)         ; list of iter records, see %loop-make-iter
        (accums nil)        ; list of (name kind state)
        (default-accum nil) ; for un-named collects
        (initial nil)
        (finally nil)
        (return-form nil)
        (body-actions nil)  ; list of actions to run each iteration
        (rest clauses))
    ;; ---- Parse clauses ----
    (loop
      (when (null rest) (return nil))
      (let ((kw (car rest)))
        (cond
          ((or (%loop-kw= kw "FOR") (%loop-kw= kw "AS"))
           (let ((parsed (%loop-parse-for (cdr rest))))
             (push (car parsed) iters)
             (setq rest (cdr parsed))))
          ((%loop-kw= kw "WITH")
           ;; (with VAR [= INIT])
           (let ((var (cadr rest))
                 (rs (cddr rest))
                 (init nil))
             (when (and rs (%loop-kw= (car rs) "="))
               (setq init (cadr rs))
               (setq rs (cddr rs)))
             ;; Treat as initially-bound let: prepend a setq.
             (push (list :with var init) iters)
             (setq rest rs)))
          ((%loop-kw= kw "WHILE")
           (push (list :while (cadr rest)) iters)
           (setq rest (cddr rest)))
          ((%loop-kw= kw "UNTIL")
           (push (list :until (cadr rest)) iters)
           (setq rest (cddr rest)))
          ((%loop-kw= kw "REPEAT")
           (let ((cnt-sym (gensym "RPT")))
             (push (list :for-from cnt-sym 0 (cadr rest) nil :below 1) iters))
           (setq rest (cddr rest)))
          ((%loop-kw= kw "INITIALLY")
           (push (cadr rest) initial)
           (setq rest (cddr rest)))
          ((%loop-kw= kw "FINALLY")
           ;; FINALLY RETURN form OR FINALLY (DO form) OR FINALLY form
           (let ((nx (cadr rest)))
             (cond
               ((%loop-kw= nx "RETURN")
                (setq return-form (caddr rest))
                (setq rest (cdddr rest)))
               (t
                (push nx finally)
                (setq rest (cddr rest))))))
          ((or (%loop-kw= kw "DO") (%loop-kw= kw "DOING"))
           (push (list :do (cadr rest)) body-actions)
           (setq rest (cddr rest)))
          ((or (%loop-kw= kw "COLLECT") (%loop-kw= kw "COLLECTING"))
           (let* ((expr (cadr rest))
                  (rs (cddr rest))
                  (into nil))
             (when (and rs (%loop-kw= (car rs) "INTO"))
               (setq into (cadr rs)) (setq rs (cddr rs)))
             (let ((name (or into '%loop-default-collect)))
               (unless (assoc name accums)
                 (push (list name :collect nil) accums))
               (unless into (setq default-accum name))
               (push (list :accum name :collect expr) body-actions))
             (setq rest rs)))
          ((%loop-kw= kw "SUM")
           (let* ((expr (cadr rest))
                  (rs (cddr rest))
                  (into nil))
             (when (and rs (%loop-kw= (car rs) "INTO"))
               (setq into (cadr rs)) (setq rs (cddr rs)))
             ;; Share one accumulator with COUNT per CLHS 6.1.3.3 when
             ;; no :INTO — sum and count both belong to the numeric
             ;; accumulator group (loop10 82/83).
             (let ((name (or into '%loop-default-numeric)))
               (unless (assoc name accums)
                 (push (list name :sum 0) accums))
               (unless into (setq default-accum name))
               (push (list :accum name :sum expr) body-actions))
             (setq rest rs)))
          ((%loop-kw= kw "COUNT")
           (let* ((expr (cadr rest))
                  (rs (cddr rest))
                  (into nil))
             (when (and rs (%loop-kw= (car rs) "INTO"))
               (setq into (cadr rs)) (setq rs (cddr rs)))
             (let ((name (or into '%loop-default-numeric)))
               (unless (assoc name accums)
                 (push (list name :count 0) accums))
               (unless into (setq default-accum name))
               (push (list :accum name :count expr) body-actions))
             (setq rest rs)))
          ((%loop-kw= kw "MINIMIZE")
           (let* ((expr (cadr rest))
                  (rs (cddr rest)))
             ;; Share one accumulator with MAXIMIZE so paired clauses
             ;; (CLHS 6.1.3.3) update a single running extremum — when
             ;; both appear without :INTO the final value is min/max of
             ;; all per-iter samples interleaved.
             (unless (assoc '%loop-default-extremum accums)
               (push (list '%loop-default-extremum :minimize nil) accums))
             (setq default-accum '%loop-default-extremum)
             (push (list :accum '%loop-default-extremum :minimize expr) body-actions)
             (setq rest rs)))
          ((%loop-kw= kw "MAXIMIZE")
           (let* ((expr (cadr rest))
                  (rs (cddr rest)))
             (unless (assoc '%loop-default-extremum accums)
               (push (list '%loop-default-extremum :maximize nil) accums))
             (setq default-accum '%loop-default-extremum)
             (push (list :accum '%loop-default-extremum :maximize expr) body-actions)
             (setq rest rs)))
          ((or (%loop-kw= kw "APPEND") (%loop-kw= kw "APPENDING"))
           (let* ((expr (cadr rest))
                  (rs (cddr rest))
                  (into nil))
             (when (and rs (%loop-kw= (car rs) "INTO"))
               (setq into (cadr rs)) (setq rs (cddr rs)))
             (let ((name (or into '%loop-default-append)))
               (unless (assoc name accums)
                 (push (list name :append nil) accums))
               (unless into (setq default-accum name))
               (push (list :accum name :append expr) body-actions))
             (setq rest rs)))
          ((or (%loop-kw= kw "NCONC") (%loop-kw= kw "NCONCING"))
           (let* ((expr (cadr rest))
                  (rs (cddr rest))
                  (into nil))
             (when (and rs (%loop-kw= (car rs) "INTO"))
               (setq into (cadr rs)) (setq rs (cddr rs)))
             (let ((name (or into '%loop-default-nconc)))
               (unless (assoc name accums)
                 (push (list name :nconc nil) accums))
               (unless into (setq default-accum name))
               (push (list :accum name :nconc expr) body-actions))
             (setq rest rs)))
          ((%loop-kw= kw "ALWAYS")
           ;; (always TEST) — terminate-with-NIL if test ever fails;
           ;; default-return T at end of loop.
           (push (list :always (cadr rest)) body-actions)
           (setq rest (cddr rest)))
          ((%loop-kw= kw "NEVER")
           (push (list :never (cadr rest)) body-actions)
           (setq rest (cddr rest)))
          ((%loop-kw= kw "THEREIS")
           (push (list :thereis (cadr rest)) body-actions)
           (setq rest (cddr rest)))
          ((or (%loop-kw= kw "WHEN") (%loop-kw= kw "IF"))
           ;; (when TEST CLAUSE [AND CLAUSE]* [ELSE CLAUSE [AND CLAUSE]*] [END])
           ;; CLHS §6.1.1.5.  The yes-branch (and optional no-branch)
           ;; is a sequence of selectable-clauses joined by AND.
           (let* ((test (cadr rest))
                  (yes-pair (%loop-parse-branch (cddr rest)))
                  (yes-acts (car yes-pair))
                  (rs (cdr yes-pair))
                  (no-acts nil))
             (when (and rs (%loop-kw= (car rs) "ELSE"))
               (let ((np (%loop-parse-branch (cdr rs))))
                 (setq no-acts (car np))
                 (setq rs (cdr np))))
             (when (and rs (%loop-kw= (car rs) "END"))
               (setq rs (cdr rs)))
             ;; Register accumulator cells for any :accum action that
             ;; appears in either branch (parse-one-action returns the
             ;; record but doesn't touch the outer accums alist).
             (dolist (a (%loop-collect-accums (append yes-acts no-acts)))
               (let ((nm (cadr a)) (sk (caddr a)))
                 (unless (assoc nm accums)
                   (push (list nm sk (if (eq sk :sum) 0
                                          (if (eq sk :count) 0 nil)))
                         accums))
                 (unless default-accum (setq default-accum nm))))
             (push (list :when test yes-acts no-acts) body-actions)
             (setq rest rs)))
          ((%loop-kw= kw "UNLESS")
           (let* ((test (cadr rest))
                  (yes-pair (%loop-parse-branch (cddr rest)))
                  (yes-acts (car yes-pair))
                  (rs (cdr yes-pair))
                  (no-acts nil))
             (when (and rs (%loop-kw= (car rs) "ELSE"))
               (let ((np (%loop-parse-branch (cdr rs))))
                 (setq no-acts (car np))
                 (setq rs (cdr np))))
             (when (and rs (%loop-kw= (car rs) "END"))
               (setq rs (cdr rs)))
             (dolist (a (%loop-collect-accums (append yes-acts no-acts)))
               (let ((nm (cadr a)) (sk (caddr a)))
                 (unless (assoc nm accums)
                   (push (list nm sk (if (eq sk :sum) 0
                                          (if (eq sk :count) 0 nil)))
                         accums))
                 (unless default-accum (setq default-accum nm))))
             (push (list :unless test yes-acts no-acts) body-actions)
             (setq rest rs)))
          ((%loop-kw= kw "RETURN")
           (setq return-form (cadr rest))
           (setq rest (cddr rest)))
          (t
           ;; Unknown — best-effort: treat as a DO form.
           (push (list :do kw) body-actions)
           (setq rest (cdr rest))))))
    (setq iters (nreverse iters))
    (setq accums (nreverse accums))
    (setq body-actions (nreverse body-actions))
    (setq initial (nreverse initial))
    (setq finally (nreverse finally))
    (%loop-execute iters accums default-accum body-actions
                   initial finally return-form env)))

(defun %loop-collect-accums (acts)
  "Walk a list of action records, descending into nested :when/:unless
   records, and return the flat list of :accum records found.  The
   top-level WHEN/UNLESS handlers register an INTO cell for each — a
   nested conditional (else-when ladder) buries its accumulators one
   or more levels down, so a flat scan would miss them."
  (let ((out nil) (stack (list acts)))
    (loop
      (when (null stack) (return (nreverse out)))
      (let ((cur (car stack)))
        (setq stack (cdr stack))
        (dolist (a cur)
          (cond
            ((eq (car a) :accum) (push a out))
            ((or (eq (car a) :when) (eq (car a) :unless))
             ;; record shape: (:when test yes-acts no-acts)
             (push (caddr a) stack)
             (when (cadddr a) (push (cadddr a) stack)))))))))

(defun %loop-parse-one-action (rest)
  "Read one DO/COLLECT/SUM/COUNT/MIN/MAX/APPEND/NCONC action clause off
   REST, including optional `INTO NAME'.  Returns (cons action-record
   rest-after).  Used inside WHEN/IF/UNLESS branches and ELSE branches —
   handles the same INTO-naming that the top-level parser does so
       (when (evenp x) collect x into evens)
   names the right accumulator instead of defaulting to %loop-default-
   collect."
  (let ((kw (car rest)))
    (cond
      ((or (%loop-kw= kw "DO") (%loop-kw= kw "DOING"))
       (cons (list :do (cadr rest)) (cddr rest)))
      ((or (%loop-kw= kw "COLLECT") (%loop-kw= kw "COLLECTING"))
       (let* ((expr (cadr rest))
              (rs (cddr rest))
              (into nil))
         (when (and rs (%loop-kw= (car rs) "INTO"))
           (setq into (cadr rs)) (setq rs (cddr rs)))
         (cons (list :accum (or into '%loop-default-collect) :collect expr)
               rs)))
      ((%loop-kw= kw "SUM")
       (let* ((expr (cadr rest))
              (rs (cddr rest))
              (into nil))
         (when (and rs (%loop-kw= (car rs) "INTO"))
           (setq into (cadr rs)) (setq rs (cddr rs)))
         (cons (list :accum (or into '%loop-default-sum) :sum expr) rs)))
      ((%loop-kw= kw "COUNT")
       (let* ((expr (cadr rest))
              (rs (cddr rest))
              (into nil))
         (when (and rs (%loop-kw= (car rs) "INTO"))
           (setq into (cadr rs)) (setq rs (cddr rs)))
         (cons (list :accum (or into '%loop-default-count) :count expr) rs)))
      ((%loop-kw= kw "MINIMIZE")
       (let* ((expr (cadr rest))
              (rs (cddr rest))
              (into nil))
         (when (and rs (%loop-kw= (car rs) "INTO"))
           (setq into (cadr rs)) (setq rs (cddr rs)))
         ;; Share one extremum accumulator with MAXIMIZE when no :INTO,
         ;; per CLHS 6.1.3.3 — paired clauses update one running value.
         (cons (list :accum (or into '%loop-default-extremum) :minimize expr)
               rs)))
      ((%loop-kw= kw "MAXIMIZE")
       (let* ((expr (cadr rest))
              (rs (cddr rest))
              (into nil))
         (when (and rs (%loop-kw= (car rs) "INTO"))
           (setq into (cadr rs)) (setq rs (cddr rs)))
         (cons (list :accum (or into '%loop-default-extremum) :maximize expr)
               rs)))
      ((or (%loop-kw= kw "APPEND") (%loop-kw= kw "APPENDING"))
       (let* ((expr (cadr rest))
              (rs (cddr rest))
              (into nil))
         (when (and rs (%loop-kw= (car rs) "INTO"))
           (setq into (cadr rs)) (setq rs (cddr rs)))
         (cons (list :accum (or into '%loop-default-append) :append expr)
               rs)))
      ((or (%loop-kw= kw "NCONC") (%loop-kw= kw "NCONCING"))
       (let* ((expr (cadr rest))
              (rs (cddr rest))
              (into nil))
         (when (and rs (%loop-kw= (car rs) "INTO"))
           (setq into (cadr rs)) (setq rs (cddr rs)))
         (cons (list :accum (or into '%loop-default-nconc) :nconc expr)
               rs)))
      ((%loop-kw= kw "RETURN")
       (cons (list :return (cadr rest)) (cddr rest)))
      ;; Nested conditional inside a branch — the CLHS `selectable-clause'
      ;; can itself be a WHEN/IF/UNLESS form, which is how the common
      ;; `when A do X else when B do Y else do Z' ladder is written.
      ;; Parse it recursively into a :when / :unless action record that
      ;; %loop-run-action already understands, returning the accumulator
      ;; sub-actions so the caller can register their INTO cells.
      ((or (%loop-kw= kw "WHEN") (%loop-kw= kw "IF") (%loop-kw= kw "UNLESS"))
       (let* ((negate (%loop-kw= kw "UNLESS"))
              (test (cadr rest))
              (yes-pair (%loop-parse-branch (cddr rest)))
              (yes-acts (car yes-pair))
              (rs (cdr yes-pair))
              (no-acts nil))
         (when (and rs (%loop-kw= (car rs) "ELSE"))
           (let ((np (%loop-parse-branch (cdr rs))))
             (setq no-acts (car np))
             (setq rs (cdr np))))
         (when (and rs (%loop-kw= (car rs) "END"))
           (setq rs (cdr rs)))
         (cons (list (if negate :unless :when) test yes-acts no-acts) rs)))
      (t
       ;; Bare form = implicit DO.
       (cons (list :do kw) (cdr rest))))))

(defun %loop-parse-branch (rest)
  "Parse one action then any number of `AND' continuation actions
   (CLHS §6.1.1.5 — `selectable-clause { and selectable-clause }*').
   Returns (cons action-list rest-after).  Drives the WHEN / IF /
   UNLESS / ELSE branch parser."
  (let* ((first (%loop-parse-one-action rest))
         (acts (list (car first)))
         (rs (cdr first)))
    (loop
      (when (or (null rs) (not (%loop-kw= (car rs) "AND")))
        (return (cons (nreverse acts) rs)))
      (let ((nxt (%loop-parse-one-action (cdr rs))))
        (push (car nxt) acts)
        (setq rs (cdr nxt))))))

(defun %loop-hash-keys (ht)
  "Return a fresh list of HT's keys (for LOOP being hash-keys)."
  (let ((acc nil))
    (maphash (lambda (k v) (declare (ignore v)) (setq acc (cons k acc))) ht)
    acc))

(defun %loop-hash-values (ht)
  "Return a fresh list of HT's values (for LOOP being hash-values)."
  (let ((acc nil))
    (maphash (lambda (k v) (declare (ignore k)) (setq acc (cons v acc))) ht)
    acc))

(defun %loop-parse-for (rest)
  "Parse a FOR clause: returns (cons iter-record rest-after).
   Recognised: in LIST | on LIST | from N [{to|below|downto|above} M]
   [by S] | = INIT [then NEXT] | across VEC |
   being {the|each} {hash-keys|hash-values} {of|in} HT."
  (let ((var (car rest))
        (rs (cdr rest)))
    (let ((kw (car rs)))
      (cond
        ;; BEING {THE|EACH} {HASH-KEY[S]|HASH-VALUE[S]} {OF|IN} HT.
        ;; (Symbol iteration variants are handled elsewhere.)  We strip
        ;; the optional USING clause that may trail HT.
        ((%loop-kw= kw "BEING")
         (let ((tail (cdr rs)))
           ;; skip THE / EACH
           (when (or (%loop-kw= (car tail) "THE") (%loop-kw= (car tail) "EACH"))
             (setq tail (cdr tail)))
           (let ((what (car tail))
                 (after (cdr tail)))
             ;; skip OF / IN
             (when (or (%loop-kw= (car after) "OF") (%loop-kw= (car after) "IN"))
               (setq after (cdr after)))
             (let ((ht-form (car after))
                   (rest2 (cdr after)))
               ;; optional USING (hash-value V) / (hash-key K) — skip it.
               (when (%loop-kw= (car rest2) "USING")
                 (setq rest2 (cddr rest2)))
               (cond
                 ((or (%loop-kw= what "HASH-KEY") (%loop-kw= what "HASH-KEYS"))
                  (cons (list :for-in var (list '%loop-hash-keys ht-form)) rest2))
                 ((or (%loop-kw= what "HASH-VALUE") (%loop-kw= what "HASH-VALUES"))
                  (cons (list :for-in var (list '%loop-hash-values ht-form)) rest2))
                 ((or (%loop-kw= what "SYMBOL") (%loop-kw= what "SYMBOLS"))
                  (cons (list :for-in var
                              (list '%loop-collect-symbols (or ht-form '*package*)))
                        rest2))
                 ((or (%loop-kw= what "EXTERNAL-SYMBOL")
                      (%loop-kw= what "EXTERNAL-SYMBOLS"))
                  (cons (list :for-in var
                              (list '%loop-collect-external-symbols
                                    (or ht-form '*package*)))
                        rest2))
                 ((or (%loop-kw= what "PRESENT-SYMBOL")
                      (%loop-kw= what "PRESENT-SYMBOLS"))
                  (cons (list :for-in var
                              (list '%loop-collect-present-symbols
                                    (or ht-form '*package*)))
                        rest2))
                 (t
                  ;; Unsupported BEING target — degrade to empty iteration.
                  (cons (list :for-in var nil) rest2)))))))
        ((%loop-kw= kw "IN")
         (cons (list :for-in var (cadr rs)) (cddr rs)))
        ((%loop-kw= kw "ON")
         (cons (list :for-on var (cadr rs)) (cddr rs)))
        ((%loop-kw= kw "ACROSS")
         (cons (list :for-across var (cadr rs)) (cddr rs)))
        ((%loop-kw= kw "=")
         (let ((init (cadr rs))
               (tail (cddr rs))
               (then nil))
           (when (%loop-kw= (car tail) "THEN")
             (setq then (cadr tail))
             (setq tail (cddr tail)))
           (cons (list :for-eq var init then) tail)))
        ((%loop-kw= kw "FROM")
         (let ((from (cadr rs))
               (tail (cddr rs))
               (bound nil) (bound-kind :to) (step 1))
           (cond
             ((%loop-kw= (car tail) "TO")
              (setq bound (cadr tail))   (setq bound-kind :to)
              (setq tail (cddr tail)))
             ((%loop-kw= (car tail) "BELOW")
              (setq bound (cadr tail))   (setq bound-kind :below)
              (setq tail (cddr tail)))
             ((%loop-kw= (car tail) "DOWNTO")
              (setq bound (cadr tail))   (setq bound-kind :downto)
              (setq tail (cddr tail)))
             ((%loop-kw= (car tail) "ABOVE")
              (setq bound (cadr tail))   (setq bound-kind :above)
              (setq tail (cddr tail))))
           (when (%loop-kw= (car tail) "BY")
             (setq step (cadr tail))
             (setq tail (cddr tail)))
           (cons (list :for-from var from bound nil bound-kind step) tail)))
        (t
         ;; (for VAR) with no specifier — repeat forever with VAR bound to nil
         (cons (list :for-eq var nil nil) rs))))))

(defun %loop-bind-pattern (pat val env)
  "Extend ENV binding PAT to VAL.  PAT may be a symbol (regular bind)
   or a cons (destructuring on a list — `(for (q r) = vals)` and
   similar).  Destructuring walks the pattern and matches NTH of VAL;
   nested patterns descend recursively.  Supports the flat list
   shapes the ANSI loop tests use and no further."
  (cond
    ((null pat) env)
    ((symbolp pat) (%env-extend pat val env))
    ((consp pat)
     ;; Destructure: walk pat and val in parallel.  Treat trailing
     ;; dotted tail as &rest binding.
     (let ((p pat) (v val) (new-env env))
       (loop
         (cond
           ((null p) (return new-env))
           ((consp p)
            (setq new-env (%loop-bind-pattern (car p)
                                              (if (consp v) (car v) nil)
                                              new-env))
            (setq p (cdr p))
            (setq v (if (consp v) (cdr v) nil)))
           ;; Dotted tail: bind to remaining value
           (t (setq new-env (%loop-bind-pattern p v new-env))
              (return new-env))))))
    (t env)))

(defun %loop-execute (iters accums default-accum body initial finally
                            return-form env)
  "Run the parsed extended-LOOP.  All iteration state is held in a
   list of mutable cons cells `iter-state' (one per iter spec) and a
   list of accumulator cons cells `acc-state' (one per accum spec).
   The env carries ONLY the user-facing variables (X for FOR X IN …,
   WITH-bound vars, etc.) — we re-extend it each iteration so the
   user's expression sees fresh values.

   The state cells use shared shapes:
     :for-in     (REMAINING-LIST)
     :for-on     (REMAINING-LIST)
     :for-across (INDEX VECTOR)
     :for-from   (CURRENT-N BOUND-VAL BOUND-KIND STEP)
     :for-eq     (THEN-FORM FIRST-ITER-P)
     :with       — no state; binding done once
   Accumulators:
     :collect    head-most-recent reversed at end
     :sum        running total
     :count      running count
     :min/:max   running extremum"
  (let ((iter-state nil)    ; list of state cells (mutable)
        (with-bindings nil) ; ((var . val) …) for WITH
        (while-form nil)
        (until-form nil)
        ;; Snapshot for escape-stack cleanup on real-error re-raise.
        (loop-snap (%eval-escape-snapshot)))
    ;; Build state cells.  Walk iters in parsed-source order.
    (let ((cur iters))
      (loop
        (when (null cur) (return nil))
        (let ((it (car cur)))
          (let ((kind (car it)))
            (cond
              ((eq kind :with)
               (let ((var (cadr it)) (init (caddr it)))
                 (push (cons var (if init (%eval-in-env init env) nil))
                       with-bindings)
                 (push (list :with var) iter-state)))
              ((eq kind :for-in)
               (push (list :for-in (cadr it)
                           (%eval-in-env (caddr it) env))
                     iter-state))
              ((eq kind :for-on)
               (push (list :for-on (cadr it)
                           (%eval-in-env (caddr it) env))
                     iter-state))
              ((eq kind :for-across)
               (push (list :for-across (cadr it)
                           0 (%eval-in-env (caddr it) env))
                     iter-state))
              ((eq kind :for-from)
               ;; Defer evaluation of FROM/BOUND/STEP forms to first-iter
               ;; step time.  Prior for-clauses' vars (e.g. `for i ...
               ;; for j from 2 to i`) are only bound in step-env.  State
               ;; shape: (:for-from VAR CUR BOUND-VAL BK STEP FROM-F
               ;; BOUND-F STEP-F FIRST-P).
               (push (list :for-from (cadr it) nil nil (nth 5 it) nil
                           (caddr it) (cadddr it) (nth 6 it) nil)
                     iter-state))
              ((eq kind :for-eq)
               (let ((init (caddr it)) (then (cadddr it)))
                 ;; State shape: (:for-eq VAR CUR THEN FIRST-FLAG INIT).
                 ;; INIT is preserved so the step path can re-evaluate it
                 ;; each iteration when no THEN is supplied (CLHS 6.1.2.1.2).
                 ;; Leave CUR as nil at build time — the first-iter step
                 ;; evaluates INIT in the step-env (which has prior iter
                 ;; clauses' vars bound), the only env where forms like
                 ;; `for i2 = (1+ i)` can resolve `i`.
                 (push (list :for-eq (cadr it) nil then nil init)
                       iter-state)))
              ((eq kind :while)
               (setq while-form (cadr it)))
              ((eq kind :until)
               (setq until-form (cadr it))))))
        (setq cur (cdr cur))))
    (setq iter-state (nreverse iter-state))
    (setq with-bindings (nreverse with-bindings))
    ;; Build accumulator state — list of cons cells (NAME . VALUE).
    (let ((acc-state (let ((acc nil))
                       (dolist (a accums (nreverse acc))
                         (push (cons (car a) (caddr a)) acc)))))
      ;; Run initially with WITH bindings in scope.
      (let ((iter-env env))
        (dolist (wb with-bindings)
          (setq iter-env (%env-extend-pair wb iter-env)))
        (dolist (f initial) (%eval-in-env f iter-env)))
      ;; Main loop.
      (handler-case
          (let ((stopped nil))
            (loop
              (when stopped (return nil))
              ;; Advance every iter.  Build a fresh env per iteration
              ;; carrying the freshly-stepped user-facing vars + WITH.
              (let ((step-env env))
                (dolist (wb with-bindings)
                  (setq step-env (%env-extend-pair wb step-env)))
                ;; Pre-bind every iteration var's PRIOR value so a step
                ;; form may forward-reference a var introduced by a LATER
                ;; FOR clause (CLHS-legal `:for prev = nil :then c
                ;; :for c :across vec`).  Cells then re-extend with their
                ;; freshly-stepped value, which shadows the prior binding.
                (dolist (st iter-state)
                  (let ((cb (%loop-cell-cur-binding st)))
                    (when cb
                      (setq step-env (%env-extend (car cb) (cdr cb) step-env)))))
                ;; Short-circuit on first iter that signals stop, so a
                ;; later for-eq doesn't try to evaluate `init` in a
                ;; step-env where the prior for-clause's var was never
                ;; bound this iteration.
                (dolist (st iter-state)
                  (unless stopped
                    (let ((res (%loop-step-cell st step-env)))
                      (cond
                        ((null res) (setq stopped t))
                        (t (setq step-env (car res)))))))
                (when stopped (return nil))
                ;; While/until check.
                (when while-form
                  (unless (%eval-in-env while-form step-env)
                    (setq stopped t) (return nil)))
                (when until-form
                  (when (%eval-in-env until-form step-env)
                    (setq stopped t) (return nil)))
                ;; Body actions.
                (dolist (act body)
                  (%loop-run-action act step-env acc-state)))))
        (t (c)
          (let ((val (%eval-escape-pop-if nil)))
            (if (eq val :%eval-no-escape)
                ;; Not one of our RETURN/RETURN-FROM escapes — it's a
                ;; genuine error signalled inside the loop body (e.g. uiop
                ;; detect-os's `(error "…")' when no OS feature matches).
                ;; Re-raise the ORIGINAL condition C so its real type and
                ;; message reach the caller's handler, instead of masking
                ;; it as a generic "%eval-escape" SIMPLE-ERROR.  First
                ;; discard any descriptor an inner escape stranded while
                ;; C unwound past its catcher.
                (progn (%eval-escape-unwind loop-snap c) (error c))
                (return-from %loop-execute (%eval-escape-return val))))))
      ;; finally — wrap in handler-case so a `(return X)` or
      ;; `(return-from NIL X)` form inside finally (the parenthesised
      ;; CLHS shape; the keyword-only `FINALLY RETURN form` shape was
      ;; already stripped into return-form at parse time) escapes to
      ;; the loop's return value instead of propagating uncaught past
      ;; the loop's implicit (block nil) wrapping.  Without this wrap,
      ;; `(loop ... finally (return t))` halts the runtime after the
      ;; final iteration prints because %eval-escape-push's signal
      ;; reaches no handler in this neighborhood.
      ;;
      ;; Bind every named INTO accumulator into fin-env so a finally
      ;; body like `(return (values evens odds))` can reference the
      ;; accumulator's user-given name.  Apply each accumulator's final
      ;; transformation (nreverse for :collect, flatten for :append /
      ;; :nconc) so the value the user sees matches what the loop would
      ;; have returned via default-accum.
      (let ((fin-env env))
        (dolist (wb with-bindings)
          (setq fin-env (%env-extend-pair wb fin-env)))
        ;; Bind every iteration var to its FINAL value so a FINALLY body
        ;; (or `FINALLY (RETURN ...)`) can reference loop vars after the
        ;; loop ends — CLHS keeps loop variables in scope through FINALLY.
        ;; e.g. (loop :for c :across s :finally (return (digit-char-p c))).
        (dolist (st iter-state)
          (let ((cb (%loop-cell-cur-binding st)))
            (when cb
              (setq fin-env (%env-extend (car cb) (cdr cb) fin-env)))))
        (dolist (a accums)
          (let* ((nm (car a))
                 (sk (cadr a))
                 (cell (assoc nm acc-state))
                 (raw (and cell (cdr cell)))
                 ;; reverse (non-destructive) so the cell's stored list
                 ;; survives untouched — the default-accum return path
                 ;; below would otherwise see a partially-reversed list
                 ;; and produce the wrong final value.
                 (val (cond
                        ((eq sk :collect) (reverse raw))
                        ((eq sk :append)
                         (let ((acc-list nil) (cur (reverse raw)))
                           (loop
                             (when (null cur) (return acc-list))
                             (setq acc-list (append acc-list (car cur)))
                             (setq cur (cdr cur)))))
                        ((eq sk :nconc)
                         (let ((acc-list nil) (cur (reverse raw)))
                           (loop
                             (when (null cur) (return acc-list))
                             (setq acc-list (append acc-list (car cur)))
                             (setq cur (cdr cur)))))
                        (t raw))))
            (setq fin-env (%env-extend nm val fin-env))))
        (handler-case
          (dolist (f finally) (%eval-in-env f fin-env))
          (t (c)
            (let ((val (%eval-escape-pop-if nil)))
              (unless (eq val :%eval-no-escape)
                (return-from %loop-execute (%eval-escape-return val)))
              ;; not one of our escapes → re-raise the ORIGINAL condition C
              ;; (a real `(error …)' in the FINALLY clause, e.g. detect-os's
              ;; no-OS error), preserving its type/message rather than
              ;; masking it as a generic "%eval-escape".  Discard any
              ;; descriptor an inner escape stranded first.
              (%eval-escape-unwind loop-snap c)
              (error c)))))
      ;; Resolve return value.
      (cond
        (return-form
         ;; `FINALLY RETURN form` (keyword shape): the form may reference
         ;; loop variables, which remain in scope per CLHS.  Re-extend a
         ;; fresh env with WITH bindings + each iteration var's final value.
         (let ((rf-env env))
           (dolist (wb with-bindings)
             (setq rf-env (%env-extend-pair wb rf-env)))
           (dolist (st iter-state)
             (let ((cb (%loop-cell-cur-binding st)))
               (when cb
                 (setq rf-env (%env-extend (car cb) (cdr cb) rf-env)))))
           (%eval-in-env return-form rf-env)))
        (default-accum
         (let ((acc (assoc default-accum accums))
               (cell (assoc default-accum acc-state)))
           (cond
             ((null acc) nil)
             ((eq (cadr acc) :collect) (nreverse (cdr cell)))
             ((eq (cadr acc) :append)
              ;; cell value is head-most-recent list of pushed lists;
              ;; flatten via append on the reverse.
              (let ((acc-list nil) (cur (nreverse (cdr cell))))
                (loop
                  (when (null cur) (return acc-list))
                  (setq acc-list (append acc-list (car cur)))
                  (setq cur (cdr cur)))))
             ((eq (cadr acc) :nconc)
              (let ((acc-list nil) (cur (nreverse (cdr cell))))
                (loop
                  (when (null cur) (return acc-list))
                  (setq acc-list (nconc acc-list (car cur)))
                  (setq cur (cdr cur)))))
             (t (cdr cell)))))
        ;; If the loop ran to completion with no early-exit, return
        ;; T for `always'/`never' clauses (CLHS §6.1.4.4), NIL
        ;; otherwise.  We detect always/never presence by scanning
        ;; body-actions.
        (t (if (%loop-has-always-or-never body) t nil))))))

(defun %loop-has-always-or-never (body)
  (let ((cur body) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (let ((k (car (car cur))))
        (when (or (eq k :always) (eq k :never))
          (setq found t)))
      (setq cur (cdr cur)))))

(defun %loop-cell-cur-binding (st)
  "Return (VAR . CURRENT-VALUE) for the iteration var of state cell ST as
   it was bound at the END of the PREVIOUS iteration, or NIL if the var
   has no current value yet (first iteration, before this cell stepped).

   Used to pre-extend the per-iteration step-env with every loop var's
   prior value BEFORE any cell steps, so a step form may forward-reference
   a var introduced by a later FOR clause — e.g. CLHS-legal
     (loop :for prev = nil :then c :for c :across vec ...)
   where PREV's `:then c` reads C, bound by the later `:for c :across`.
   Without this, sequential cell-by-cell env threading leaves C unbound
   when PREV's THEN form runs (PREV is stepped first)."
  (let ((kind (car st)))
    (cond
      ((eq kind :for-eq)
       ;; CUR is (caddr st); valid once the first-flag (nth 4 st) is set.
       (when (nth 4 st)
         (let ((var (cadr st)))
           (when (symbolp var) (cons var (caddr st))))))
      ((eq kind :for-from)
       ;; CUR is (caddr st); valid once first-p flag (nth 9 st) is set.
       (when (nth 9 st)
         (cons (cadr st) (caddr st))))
      ((eq kind :for-across)
       ;; Index (caddr st) points at the NEXT element; the currently-bound
       ;; value is vec[i-1] once i>0.
       (let ((i (caddr st)) (vec (cadddr st)))
         (when (and (integerp i) (> i 0))
           (cons (cadr st) (aref vec (- i 1))))))
      (t nil))))

(defun %loop-step-cell (st env)
  "Advance one iteration-state cell and return (CONS NEW-ENV NIL)
   if a step was taken, NIL if the iter source is exhausted.

   Each state cell carries a tag in its CAR and the iterator's user-
   facing var in its CADR; the rest of the cell holds source state.
   We mutate the cell in place with rplacd/rplaca where needed."
  (let ((kind (car st)))
    (cond
      ((eq kind :with) (cons env nil))
      ((eq kind :for-in)
       (let ((var (cadr st)) (lst (caddr st)))
         (if (null lst)
             nil
             (progn
               (rplaca (cddr st) (cdr lst))   ; advance remaining
               ;; VAR may be a destructuring pattern, e.g.
               ;; `(for (kw . args) in alist)` — bind via the pattern
               ;; walker so a cons VAR destructures the element.
               (cons (%loop-bind-pattern var (car lst) env) nil)))))
      ((eq kind :for-on)
       (let ((var (cadr st)) (lst (caddr st)))
         (if (null lst)
             nil
             (progn
               (rplaca (cddr st) (cdr lst))
               (cons (%loop-bind-pattern var lst env) nil)))))
      ((eq kind :for-across)
       (let* ((var (cadr st))
              (i (caddr st))
              (vec (cadddr st))
              (n (array-length vec)))
         (if (>= i n)
             nil
             (progn
               (rplaca (cddr st) (+ i 1))
               (cons (%env-extend var (aref vec i) env) nil)))))
      ((eq kind :for-from)
       (let* ((var (cadr st))
              (bk (nth 4 st))
              (first-p (not (nth 9 st))))
         (when first-p
           ;; Evaluate FROM / BOUND / STEP forms now, in step-env, so
           ;; prior for-clauses' vars are visible.  Default STEP to 1
           ;; when no BY clause was given.
           (let ((from-f (nth 6 st))
                 (bound-f (nth 7 st))
                 (step-f (nth 8 st)))
             (rplaca (cddr st) (%eval-in-env from-f env))
             (rplaca (cdddr st)
                     (and bound-f (%eval-in-env bound-f env)))
             (rplaca (nthcdr 5 st)
                     (if step-f (%eval-in-env step-f env) 1))
             (rplaca (nthcdr 9 st) t)))
         (let* ((cur (caddr st))
                (bound (cadddr st))
                (step (nth 5 st))
                (done
                 (cond
                   ((null bound) nil)
                   ((eq bk :to)     (> cur bound))
                   ((eq bk :below)  (>= cur bound))
                   ((eq bk :downto) (< cur bound))
                   ((eq bk :above)  (<= cur bound))
                   (t nil))))
           (if done
               nil
               (let ((nx (if (or (eq bk :downto) (eq bk :above))
                             (- cur step) (+ cur step))))
                 (rplaca (cddr st) nx)
                 (cons (%env-extend var cur env) nil))))))
      ((eq kind :for-eq)
       (let* ((var (cadr st))
              (cur (caddr st))
              (then (cadddr st))
              (first-p (not (nth 4 st)))
              (init (nth 5 st)))
         (cond
           (first-p
            ;; Mark not-first.  First-iter VAR = INIT, evaluated NOW in
            ;; step-env so prior for-clauses' vars are visible.
            (rplaca (cddddr st) t)
            (let ((init-val (%eval-in-env init env)))
              (rplaca (cddr st) init-val)
              (cons (%loop-bind-pattern var init-val env) nil)))
           (then
            (let ((env2 (%loop-bind-pattern var cur env)))
              (let ((nx (%eval-in-env then env2)))
                (rplaca (cddr st) nx)
                (cons (%loop-bind-pattern var nx env) nil))))
           (t
            ;; No THEN: re-evaluate INIT each iteration (CLHS 6.1.2.1.2).
            ;; Same step-env so prior iter clauses' current bindings are
            ;; visible to the init form.
            (let ((nx (%eval-in-env init env)))
              (rplaca (cddr st) nx)
              (cons (%loop-bind-pattern var nx env) nil))))))
      (t (cons env nil)))))

(defun %loop-run-action (act env acc-state)
  "Execute one body-action record.  ACC-STATE is the list of mutable
   (NAME . VALUE) cells for the accumulators set up by %loop-execute;
   :ACCUM actions rplacd the appropriate cell."
  (let ((kind (car act)))
    (cond
      ((eq kind :do)
       (%eval-in-env (cadr act) env))
      ((eq kind :return)
       (%eval-escape-push nil (%eval-in-env (cadr act) env)))
      ((eq kind :when)
       ;; Shape: (:when test yes-actions no-actions).  yes-actions and
       ;; no-actions are LISTS of action records (one per AND-joined
       ;; clause).  no-actions is nil when no ELSE branch.
       (let ((yes-acts (caddr act))
             (no-acts (cadddr act)))
         (cond
           ((%eval-in-env (cadr act) env)
            (dolist (a yes-acts) (%loop-run-action a env acc-state)))
           (no-acts
            (dolist (a no-acts) (%loop-run-action a env acc-state))))))
      ((eq kind :unless)
       (let ((yes-acts (caddr act))
             (no-acts (cadddr act)))
         (cond
           ((not (%eval-in-env (cadr act) env))
            (dolist (a yes-acts) (%loop-run-action a env acc-state)))
           (no-acts
            (dolist (a no-acts) (%loop-run-action a env acc-state))))))
      ((eq kind :accum)
       (let* ((name (cadr act))
              (sub  (caddr act))
              (expr (cadddr act))
              (val  (%eval-in-env expr env))
              (cell (assoc name acc-state))
              (cur  (and cell (cdr cell))))
         (when cell
           (cond
             ((eq sub :collect)
              (rplacd cell (cons val cur)))
             ((eq sub :sum)
              (rplacd cell (+ cur val)))
             ((eq sub :count)
              (when val (rplacd cell (+ cur 1))))
             ((eq sub :minimize)
              (rplacd cell (if (null cur) val (if (< val cur) val cur))))
             ((eq sub :maximize)
              (rplacd cell (if (null cur) val (if (> val cur) val cur))))
             ((eq sub :append)
              ;; Push elements head-most-recent; reverse + flatten at end
              (rplacd cell (cons val cur)))
             ((eq sub :nconc)
              (rplacd cell (cons val cur)))))))
      ((eq kind :always)
       ;; If test fails, the whole loop returns NIL via RETURN-FROM.
       (unless (%eval-in-env (cadr act) env)
         (%eval-escape-push nil nil)))
      ((eq kind :never)
       ;; If test ever succeeds, whole loop returns NIL.
       (when (%eval-in-env (cadr act) env)
         (%eval-escape-push nil nil)))
      ((eq kind :thereis)
       ;; If test yields non-NIL, whole loop returns that value.
       (let ((v (%eval-in-env (cadr act) env)))
         (when v (%eval-escape-push nil v)))))))

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
    ;; Keywords self-evaluate.  Native keywords (#x53) self-evaluate by
    ;; type; KEYWORD-package CL symbols match via package eq.
    ((keywordp form) form)
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
  "Look up value of SYM in ENV then globals.  Falls back to symbol-value
   (the compiled-code hash store at #x10000080) when the eval-only alist
   doesn't have an entry, so values written via compiled `setq` or
   `defvar` from kernel-main are visible to runtime EVAL.  boundp uses
   the same hash store, so without this fall-back boundp could be T
   while a runtime read of the same symbol would signal unbound-variable."
  (let ((found-pair (%env-lookup sym env)))
    (if (car found-pair)
        (cdr found-pair)
        ;; Try eval global table
        (let ((name (%eval-sym-name sym)))
          (if name
              (let ((gv (%eval-global-get name)))
                (if (car gv)
                    (cdr gv)
                    ;; Fall back to the compiled-code symbol-value store.
                    ;; boundp checks this same store, so treating its
                    ;; presence as "bound" keeps boundp / read consistent.
                    (if (boundp sym)
                        (symbol-value sym)
                        (let ((c2 (%make-condition 'unbound-variable (list :name sym))))
                          ;; Publish the condition so a handler-case
                          ;; (cell-error (c) …) / (unbound-variable (c) …)
                          ;; matches the right type and CELL-ERROR-NAME
                          ;; sees the :name slot (cell-error-name.1).
                          ;; Also run the handler-bind stack first.
                          (setq *current-condition* c2)
                          (%associate-active-restart-frames c2)
                          (let ((handled (%signal-condition c2)))
                            (if handled
                                nil
                                (if (%error-handler-active-p)
                                    (%hc-longjmp)
                                    nil)))))))
              nil)))))

(defun %runtime-define-condition (form env)
  "Runtime EVAL of (define-condition NAME (PARENT…) (SLOT…) OPTION…).
   Mirrors compiler.lisp's mvm-define-macro \"DEFINE-CONDITION\" expansion,
   but builds the registration data directly (no quote/expand round-trip)
   and EVALs the :report lambda so the registry holds a real callable.
   Reader/accessor slots become interp-closure defuns.  Returns NAME."
  (let* ((name        (cadr form))
         (parents     (or (caddr form) '(condition)))
         (slot-specs  (cadddr form))
         (options     (cddddr form))
         (slot-descriptors nil)
         (reader-pairs nil))     ; list of (reader-name . slot-name)
    ;; Parse each slot-spec into a descriptor (name (initargs) initform)
    ;; and collect reader/accessor names.
    (dolist (spec slot-specs)
      (if (atom spec)
          (push (list spec nil :no-initform) slot-descriptors)
          (let ((sname (car spec))
                (opts (cdr spec))
                (initargs nil)
                (initform :no-initform))
            (loop
              (when (null opts) (return))
              (let ((k (car opts)) (v (cadr opts)))
                (cond
                  ((eq k :initarg) (setq initargs (append initargs (list v))))
                  ((eq k :initform) (setq initform v))
                  ((or (eq k :reader) (eq k :accessor))
                   (push (cons v sname) reader-pairs))))
              (setq opts (cddr opts)))
            (push (list sname initargs initform) slot-descriptors))))
    (setq slot-descriptors (nreverse slot-descriptors))
    (setq reader-pairs (nreverse reader-pairs))
    ;; Find :report / :default-initargs options.
    (let ((default-initargs nil)
          (report-fn nil)
          (cur options))
      (loop
        (when (null cur) (return))
        (let ((o (car cur)))
          (when (consp o)
            (cond
              ((eq (car o) :default-initargs) (setq default-initargs (cdr o)))
              ((eq (car o) :report)
               (let ((r (cadr o)))
                 (cond
                   ;; (:report (lambda (c s) …)) — eval to a closure
                   ((and (consp r) (%eval-sym-eq (car r) "LAMBDA"))
                    (setq report-fn (%eval-in-env r env)))
                   ;; (:report name) — named reporter function
                   ((symbolp r) (setq report-fn r))
                   ;; (:report "string")
                   ((stringp r) (setq report-fn r)))))))
          (setq cur (cdr cur))))
      ;; Register the type.
      (%define-condition name parents slot-descriptors default-initargs report-fn)
      ;; Define reader/accessor functions.
      (dolist (rp reader-pairs)
        (%eval-in-env
          (list 'defun (car rp) (list 'c)
                (list '%condition-slot 'c (list 'quote (cdr rp))))
          env))
      name)))

(defun %eval-compound (form env)
  "Evaluate a compound (list) form."
  (let ((op (car form))
        (args (cdr form)))
    (cond
      ;; QUOTE
      ((%eval-sym-eq op "QUOTE") (car args))
      ;; FUNCALL — special-cased because there's no defun for funcall
      ;; (the compiler emits it inline at every call site) and routing
      ;; through %eval-funcall's SFT lookup would signal undefined-
      ;; function.  Evaluate fn + args here and dispatch via
      ;; %do-funcall, which handles interp-closures, compiled defuns,
      ;; native MVM symbols, and &rest packing uniformly.
      ((%eval-sym-eq op "FUNCALL")
       (let* ((fn-val (%eval-in-env (car args) env))
              (call-args (%eval-args (cdr args) env)))
         (%do-funcall fn-val call-args)))
      ;; APPLY — same shape as FUNCALL but the trailing arg is a list
      ;; that gets spread.  Per CLHS (apply fn a b c rest-list) calls
      ;; fn with args (a b c . rest-list) — i.e. leading args are
      ;; consed onto rest-list to form the full arg list.
      ((%eval-sym-eq op "APPLY")
       (let* ((fn-val (%eval-in-env (car args) env))
              (rest-forms (cdr args))
              (n (length rest-forms))
              (evaled (%eval-args rest-forms env))
              (call-args
               (cond
                 ((= n 0) nil)
                 ((= n 1) (car evaled))
                 (t
                  ;; All but the last are individual args; the last is
                  ;; the spread list.  Walk and reverse-cons.
                  (let ((leading nil) (cur evaled))
                    (loop
                      (when (null (cdr cur))
                        (return (let ((acc (car cur)))
                                  (dolist (x leading acc)
                                    (setq acc (cons x acc))))))
                      (setq leading (cons (car cur) leading))
                      (setq cur (cdr cur))))))))
         (%do-funcall fn-val call-args)))
      ;; IF
      ((%eval-sym-eq op "IF")
       (if (%eval-in-env (car args) env)
           (%eval-in-env (cadr args) env)
           (if (cddr args) (%eval-in-env (caddr args) env) nil)))
      ;; PROGN
      ((%eval-sym-eq op "PROGN") (%eval-progn args env))
      ;; LET — parallel bindings.  *earmuff* vars are dynamically bound
      ;; (global save/set + unwind-protect restore); the rest extend the
      ;; lexical env.  Critical so e.g. `(let ((*print-base* 2)) (write 15))`
      ;; affects the compiled PRIN1 reader.
      ((%eval-sym-eq op "LET")
       (let ((bindings (car args))
             (body (cdr args)))
         (let ((new-env env)
               (specials nil)
               (vb-list nil))
           ;; Phase 1: evaluate all init forms in the OUTER env (parallel).
           (let ((cur bindings))
             (loop
               (when (null cur) (return nil))
               (let ((b (car cur)))
                 (let ((var (if (consp b) (car b) b))
                       (val-form (if (and (consp b) (cdr b)) (cadr b) nil)))
                   (let ((val (%eval-in-env val-form env)))
                     (push (cons var val) vb-list))))
               (setq cur (cdr cur))))
           (setq vb-list (nreverse vb-list))
           ;; Phase 2: establish bindings — specials via global rebind,
           ;; ordinary via lexical extend.
           (dolist (vb vb-list)
             (let ((sym (car vb))
                   (val (cdr vb)))
               (cond
                 ((%earmuff-var-p sym)
                  (push (cons sym (%eval-save-special sym)) specials)
                  (%eval-set-special sym val))
                 (t (setq new-env (%env-extend sym val new-env))))))
           ;; Phase 3: run body; restore specials on NORMAL exit only.
           ;; Modus's compile-time unwind-protect drops the in-flight
           ;; condition before its longjmp, so any error inside body
           ;; would be silently swallowed if we wrapped here.  Live
           ;; bindings on abnormal exit are the lesser evil — most
           ;; tests don't error inside body, and the next LET-special
           ;; or PROGV will overwrite anyway.
           ;;
           ;; Capture MV around the restore-specials loop: each call to
           ;; %eval-restore-special invokes set-symbol-value which
           ;; clobbers the MV buffer.  Without this, `(let ((*x* …))
           ;; (declare (special *x*)) (values 1 2 3 4))' returned only
           ;; the primary value — dgmc.* tests all use this shape.
           (let ((result (multiple-value-list (%eval-progn body new-env))))
             (dolist (sv specials)
               (%eval-restore-special (car sv) (cdr sv)))
             (values-list result)))))
      ;; LET* — sequential bindings; specials bind earlier so subsequent
      ;; init forms see the prior special's new value.  CLHS 5.1.2.1.
      ((%eval-sym-eq op "LET*")
       (let ((bindings (car args))
             (body (cdr args)))
         (let ((new-env env)
               (specials nil))
           (let ((cur bindings))
             (loop
               (when (null cur) (return nil))
               (let ((b (car cur)))
                 (let ((var (if (consp b) (car b) b))
                       (val-form (if (and (consp b) (cdr b)) (cadr b) nil)))
                   (let ((val (%eval-in-env val-form new-env)))
                     (cond
                       ((%earmuff-var-p var)
                        (push (cons var (%eval-save-special var)) specials)
                        (%eval-set-special var val))
                       (t (setq new-env (%env-extend var val new-env)))))))
               (setq cur (cdr cur))))
           ;; Same MV-preservation as LET above: capture all values
           ;; around the restore loop so set-symbol-value calls don't
           ;; clobber the MV buffer.
           (let ((result (multiple-value-list (%eval-progn body new-env))))
             (dolist (sv specials)
               (%eval-restore-special (car sv) (cdr sv)))
             (values-list result)))))
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
      ;; DEFUN — register an %interp-closure in BOTH the name-string SFT
      ;; AND the hash-keyed native-sym table (mirror).  Without the
      ;; mirror, fboundp/funcall on a compile-time-quoted symbol like
      ;; `'foo` (a native MVM #x50 sym) can't find the runtime-defined
      ;; fn — it consults *native-sym-function-table* by hash.
      ((%eval-sym-eq op "DEFUN")
       (let ((fname (car args))
             (params (cadr args))
             (body (cddr args)))
         (let ((name-str (%eval-sym-name fname)))
           ;; CLHS 3.1.2.1.3 / 5.3 DEFUN: the body is enclosed in an
           ;; implicit BLOCK whose name is the function name (the CADR for
           ;; a (SETF foo) name).  Without this, a `(return-from foo …)`
           ;; in the body escapes with no catcher → empty escape stack →
           ;; the spurious SIMPLE-ERROR "%eval-escape".  Only wrap the
           ;; stored body in a `(block BLOCK-NAME orig-body…)` form when
           ;; the body actually contains a RETURN-FROM to that block — so
           ;; the vast majority of functions keep the original zero-extra-
           ;; allocation call path (the block's handler-case + escape-
           ;; stack machinery runs per call, which is GC-timing-sensitive).
           (let* ((block-name (if (consp fname) (cadr fname) fname))
                  (wrapped (if (%body-returns-from-p body block-name)
                               (list (cons 'block (cons block-name body)))
                               body))
                  (fn (list '%interp-closure params wrapped nil)))
             (when name-str
               (unless *symbol-function-table* (%sft-init))
               (puthash name-str *symbol-function-table* fn)
               (when *native-sym-function-table*
                 (puthash (compute-name-hash name-str)
                          *native-sym-function-table* fn))))
           fname)))
      ;; DEFVAR / DEFPARAMETER / DEFCONSTANT — go through %eval-set-global
      ;; (handles native MVM symbols by hash; CL symbols by name+hash).
      ((%eval-sym-eq op "DEFVAR")
       (let ((vname (car args)))
         (when (cdr args)
           (let ((val (%eval-in-env (cadr args) env)))
             (%eval-set-global vname val)))
         vname))
      ((%eval-sym-eq op "DEFPARAMETER")
       (let ((vname (car args))
             (val (%eval-in-env (cadr args) env)))
         (%eval-set-global vname val)
         vname))
      ((%eval-sym-eq op "DEFCONSTANT")
       (let ((vname (car args))
             (val (%eval-in-env (cadr args) env)))
         (%eval-set-global vname val)
         vname))
      ;; DEFTYPE — register a type expander keyed by name and return the
      ;; name (as CLHS specifies).  Modus's typep/subtypep don't yet
      ;; consult this table, but real-world code (uiop, asdf) DEFTYPEs at
      ;; load time and only relies on the form not erroring; storing the
      ;; (params . body) lets a later typep enhancement expand it.  Without
      ;; this branch DEFTYPE fell through to the funcall path and signalled
      ;; %eval-escape (DEFTYPE is not a function).
      ((%eval-sym-eq op "DEFTYPE")
       (let ((tname (car args))
             (params (cadr args))
             (body (cddr args)))
         (let ((name-str (%eval-sym-name tname)))
           (when name-str
             (unless *%runtime-deftype-table*
               (setq *%runtime-deftype-table* (make-hash-table :test 'equal)))
             (puthash name-str *%runtime-deftype-table*
                      (cons params body))))
         tname))
      ;; DEFINE-CONDITION — register a condition type at runtime EVAL.
      ;; The compile-time mvm-define-macro expander can't cross into the
      ;; image (SBCL-side lambda), so runtime EVAL of uiop/asdf's
      ;; (define-condition …) inside with-upgradability bodies fell through
      ;; to the funcall path and signalled %eval-escape.  See
      ;; %runtime-define-condition.
      ((%eval-sym-eq op "DEFINE-CONDITION")
       (%runtime-define-condition form env))
      ;; DEFSETF — register a SETF expander at runtime EVAL.  The
      ;; compile-time mvm-define-macro "DEFSETF" expander is an SBCL-side
      ;; lambda that can't cross into the image, so the runtime macro
      ;; table only knew the name; (defsetf NAME …) fell through to the
      ;; funcall path and signalled %eval-escape (uiop's (defsetf getenv
      ;; (x) (val) …) — gauntlet forms 44/56).  Store a descriptor in
      ;; *setf-expanders* that the runtime SETF macro and
      ;; get-setf-expansion consult via %find-setf-expander /
      ;; %apply-setf-expander.  CLHS short + long forms:
      ;;   (defsetf NAME setter-fn [doc])
      ;;   (defsetf NAME (var…) (store-var…) body…)
      ((%eval-sym-eq op "DEFSETF")
       (let* ((accessor (car args))
              (rest     (cdr args)))
         (cond
           ;; Long form: (defsetf NAME (vars…) (store-vars…) body…)
           ((and (consp rest) (consp (car rest))
                 (consp (cdr rest)) (consp (cadr rest)))
            (let ((vars       (car rest))
                  (store-vars (cadr rest))
                  (body       (cddr rest)))
              ;; Strip a leading docstring.
              (when (and (stringp (car body)) (cdr body))
                (setq body (cdr body)))
              (%register-setf-expander
               accessor
               (cons :long (cons vars (cons store-vars body))))))
           ;; Short form: (defsetf NAME setter-fn [doc])
           ((and (consp rest)
                 (or (%cl-sym-p (car rest)) (%native-mvm-sym-p (car rest))
                     (%native-sym-p (car rest))))
            (%register-setf-expander accessor (cons :short (car rest))))
           (t nil))
         accessor))
      ;; DEFMACRO — register an expander so subsequent forms in this
      ;; eval / load stream macroexpand through it.  The expander is
      ;; stored as a plain %interp-closure with the user's lambda-list
      ;; and body verbatim.  macroexpand-1 below knows to call it with
      ;; (cdr whole-form) instead of (whole-form env) so the user's
      ;; params bind to the macro's actual arguments.
      ;;
      ;; Today this only handles flat lambda-lists (what %bind-params
      ;; handles for DEFUN); &whole/&environment and nested destructuring
      ;; are follow-ups.  The gcl ansi-test suite's `deftest` is flat.
      ((%eval-sym-eq op "DEFMACRO")
       (let* ((mname (car args))
              (params (cadr args))
              (body (cddr args))
              (expander (list '%interp-closure params body nil)))
         (set-macro-function mname expander)
         mname))
      ;; (define-compiler-macro name (params) body...)
      ;; CLHS §define-compiler-macro: registers a compiler-macro expander
      ;; for NAME (or (SETF NAME)).  Modus's compiler does not consult
      ;; user-registered compiler-macros during compilation, but ANSI
      ;; tests `(eval `(define-compiler-macro ,sym ...))` followed by
      ;; `(compiler-macro-function sym)` need the expander stored so the
      ;; lookup returns a function rather than NIL.  Register into
      ;; *compiler-macro-function-table* — a separate registry from
      ;; *macro-function-table* — so MACRO-FUNCTION still distinguishes
      ;; "is a real macro" from "is just a compiler-macro hint".
      ;; Returns NAME per CLHS's return-value contract.
      ((%eval-sym-eq op "DEFINE-COMPILER-MACRO")
       (let* ((mname (car args))
              (params (cadr args))
              (body (cddr args))
              (expander (list '%interp-closure params body nil))
              (key (%macro-sym-key mname)))
         (when key
           (unless *compiler-macro-function-table*
             (setq *compiler-macro-function-table* (make-hash-table)))
           (puthash key *compiler-macro-function-table* expander))
         mname))
      ;; (define-symbol-macro name expansion)
      ;; CLHS §define-symbol-macro: similar — register at runtime so
      ;; subsequent eval references to NAME expand.  Without a runtime
      ;; symbol-macro table, treat as no-op returning NAME.
      ((%eval-sym-eq op "DEFINE-SYMBOL-MACRO")
       (car args))
      ;; MACROLET — (macrolet ((name (params) body) ...) body...)
      ;; Registers local macros via set-macro-function for the duration
      ;; of the inner body, then restores prior bindings.  Used heavily
      ;; by the ANSI test suite to test compile-time expansion in
      ;; isolation, especially with EXPAND-IN-CURRENT-ENV.
      ((%eval-sym-eq op "MACROLET")
       (let* ((defs (car args))
              (body (cdr args))
              (saved nil))
         ;; Save prior bindings + install new expanders
         (dolist (d defs)
           (let* ((name (car d))
                  (params (cadr d))
                  (mbody (cddr d))
                  (key-name (cond ((stringp name) name)
                                  ((%cl-sym-p name) (%cl-sym-name name))
                                  (t (symbol-name name)))))
             (push (cons key-name
                         (and *macro-function-table*
                              (gethash key-name *macro-function-table*)))
                   saved)
             (set-macro-function key-name
                                 (list '%interp-closure params mbody env))))
         (handler-case
             ;; multiple-value-prog1 preserves the body's full MV list when
             ;; the let/result-binding pattern would otherwise drop all
             ;; but the first value.  ANSI deftests like macrolet.1 wrap
             ;; (values …) inside the macrolet body and expect both
             ;; values to propagate out.
             (multiple-value-prog1
               (%eval-progn body env)
               ;; Restore prior bindings.
               (dolist (s saved)
                 (let ((nm (car s)) (old (cdr s)))
                   (if old
                       (puthash nm *macro-function-table* old)
                       (remhash nm *macro-function-table*)))))
           (t (c)
             ;; Restore even on error
             (dolist (s saved)
               (let ((nm (car s)) (old (cdr s)))
                 (if old
                     (puthash nm *macro-function-table* old)
                     (remhash nm *macro-function-table*))))
             (%signal-error c)))))
      ;; ----- CLOS forms — runtime evaluation of defmethod/defgeneric/defclass.
      ;; All three reuse the back-end functions the build-time rewriter
      ;; targets (%defmethod, %defgeneric, %defclass) so eval'd CLOS forms
      ;; share the same registry and dispatch as compiled ones.
      ;;
      ;; (defmethod gf-name [qualifier] specialized-lambda-list body...)
      ((%eval-sym-eq op "DEFMETHOD")
       (let* ((gf-name (car args))
              (rest (cdr args))
              ;; qualifier: leading non-list symbol
              (has-qual (and rest (symbolp (car rest)) (not (listp (car rest)))))
              (qualifier (if has-qual (car rest) nil))
              (rest2 (if has-qual (cdr rest) rest))
              (sll (car rest2))
              (body (cdr rest2)))
         ;; Build specializers list: T for plain var, class-name for (var class),
         ;; (eql VAL) for (var (eql expr)) — VAL is evaluated NOW in env.
         ;; in-tail-section tracks whether we've passed &optional/&rest/&key/
         ;; &aux — past that point, params are preserved as-is (e.g.
         ;; (z :missing z-p) keeps the supplied-p binding) and NO
         ;; specializer is added (CLHS 7.6.4 — specializers only on
         ;; required positionals).
         (let ((specs nil)
               (params nil)
               (cur sll)
               (in-tail-section nil))
           (loop
             (when (null cur) (return nil))
             (let ((p (car cur)))
               (cond
                 ;; lambda-list keyword — stop collecting specializers
                 ((and (symbolp p)
                       (or (%eval-sym-eq p "&OPTIONAL")
                           (%eval-sym-eq p "&REST")
                           (%eval-sym-eq p "&KEY")
                           (%eval-sym-eq p "&AUX")
                           (%eval-sym-eq p "&ALLOW-OTHER-KEYS")))
                  (setq in-tail-section t)
                  (setq params (cons p params)))
                 (in-tail-section
                  ;; After &optional/&key/&aux — preserve full param form.
                  (setq params (cons p params)))
                 ((consp p)
                  (let ((var (car p))
                        (spec (cadr p)))
                    (setq params (cons var params))
                    (if (and (consp spec)
                             (symbolp (car spec))
                             (%eval-sym-eq (car spec) "EQL"))
                        ;; eql specializer: evaluate value form NOW in env
                        (setq specs (cons (list 'eql (%eval-in-env (cadr spec) env))
                                          specs))
                        (setq specs (cons spec specs)))))
                 (t
                  ;; Plain var — specializer is t
                  (setq params (cons p params))
                  (setq specs (cons 't specs)))))
             (setq cur (cdr cur)))
           (setq params (nreverse params))
           (setq specs (nreverse specs))
           ;; CLHS 7.6.4 congruence: method LL shape vs GF declared LL.
           (let ((gf (%find-gf gf-name)))
             (when (and gf (%gf-lambda-list gf))
               (let ((gf-ll (%gf-lambda-list gf)))
                 (let ((gf-shape (%lambda-list-shape gf-ll))
                       (m-shape  (%lambda-list-shape params)))
                   (unless (%method-ll-congruent-p gf-shape (length specs) m-shape)
                     (%signal-program-error)))
                 ;; Beyond shape: each keyword named in the GF's &key list
                 ;; must be accepted by the method (defmethod.error.10).
                 (unless (%method-accepts-gf-keys-p gf-ll params)
                   (%signal-program-error)))))
           ;; CLHS 7.6.5: method body is implicitly enclosed in a block
           ;; whose name is the generic function name (or, for (setf X)
           ;; gf-names, the symbol X — block names can't be lists).
           (let* ((block-name (cond ((symbolp gf-name) gf-name)
                                    ((and (consp gf-name)
                                          (consp (cdr gf-name))
                                          (symbolp (cadr gf-name)))
                                     (cadr gf-name))
                                    (t nil)))
                  (wrapped-body (if block-name
                                    (list (cons 'block (cons block-name body)))
                                    body)))
             ;; Build the method body as an interp-closure that captures env.
             (let ((fn (list '%interp-closure params wrapped-body env)))
               ;; Ensure gf exists with a runtime stub installed under gf-name.
               ;; CLHS 7.6.4: when DEFMETHOD implicitly creates the GF, derive
               ;; the GF lambda-list from the method's so the GF knows its
               ;; required-arg count + variadic shape — %gf-check-arity then
               ;; rejects wrong-arity calls (defmethod.error.13/.14/.15).
               (when (null (%find-gf gf-name))
                 (%defgeneric gf-name (%derive-gf-ll-from-method params) nil))
               (let ((fname (cond ((%cl-sym-p gf-name) (%cl-sym-name gf-name))
                                  ((stringp gf-name) gf-name)
                                  (t nil))))
                 (when fname
                   (let ((existing (gethash fname *symbol-function-table*)))
                     (when (null existing)
                       (set-symbol-function gf-name (%make-gf-stub gf-name))))))
               ;; Add the method to the gf and return it.  Record
               ;; key-acceptance meta from the full lambda-list so
               ;; %gf-check-keys (CLHS 7.6.5) can validate keywords.
               (let ((m (%defmethod gf-name qualifier specs fn)))
                 (%gf-record-method-meta (%find-gf gf-name) m params)
                 m))))))
      ;; (defgeneric name lambda-list &rest options)
      ;; Options handled: :method-combination, :method (inline)
      ((%eval-sym-eq op "DEFGENERIC")
       (let* ((gf-name (car args))
              (lambda-list (cadr args))
              (options (cddr args))
              (combination nil))
         ;; CLHS: a name bound to a macro / special operator / ordinary
         ;; function cannot be made generic — PROGRAM-ERROR
         ;; (defgeneric.error.1/2/3).  Only checked when no GF exists
         ;; yet (redefinition of an existing GF is fine — its dispatch
         ;; defun/stub IS fbound).
         (when (null (%find-gf gf-name))
           (let ((symish (or (%cl-sym-p gf-name)
                             (and (symbolp gf-name)
                                  (not (consp gf-name))))))
             (when symish
               (when (macro-function gf-name)
                 (%signal-program-error))
               (when (special-operator-p gf-name)
                 (%signal-program-error))
               (when (and (fboundp gf-name)
                          (not (%generic-function-p (fdefinition gf-name))))
                 (%signal-program-error)))))
         ;; CLHS option validation: unknown/repeated options, bad
         ;; :argument-precedence-order, incongruent inline :method
         ;; lambda-lists — PROGRAM-ERROR (defgeneric.error.4-19).
         (%validate-defgeneric-options gf-name lambda-list options)
         (dolist (opt options)
           ;; Both keywords (:method-combination) and bare symbols
           ;; (method-combination) match — Modus's symbol-name strips the
           ;; leading colon, so the literal we compare against does too.
           (when (and (consp opt) (symbolp (car opt))
                      (%eval-sym-eq (car opt) "METHOD-COMBINATION"))
             ;; (:method-combination NAME [:most-specific-last]) — encode
             ;; the ordering as (NAME . :MOST-SPECIFIC-LAST), mirroring
             ;; the build-time rewriter; %gf-dispatch reverses primaries.
             (setq combination
                   (if (and (consp (cddr opt)) (symbolp (caddr opt))
                            (%eval-sym-eq (caddr opt) "MOST-SPECIFIC-LAST"))
                       (cons (cadr opt) ':most-specific-last)
                       (cadr opt)))))
         ;; Pass lambda-list so %defmethod / find-method can validate
         ;; method-vs-GF arity congruence.
         (%defgeneric gf-name lambda-list combination)
         ;; :argument-precedence-order — store as arg-index list for
         ;; %method-more-specific-p.
         (dolist (opt options)
           (when (and (consp opt) (symbolp (car opt))
                      (%eval-sym-eq (car opt) "ARGUMENT-PRECEDENCE-ORDER"))
             (%gf-set-arg-precedence gf-name (cdr opt) lambda-list)))
         ;; Install runtime stub so (funcall sym ...) dispatches.
         (set-symbol-function gf-name (%make-gf-stub gf-name))
         ;; Inline (:method ...) options — re-eval each as a defmethod
         ;; form.  Same name-vs-keyword note as above.  Record each
         ;; resulting method as defgeneric-owned so a redefinition
         ;; removes it (CLHS DEFGENERIC).
         (dolist (opt options)
           (when (and (consp opt) (symbolp (car opt))
                      (%eval-sym-eq (car opt) "METHOD"))
             (let ((m (%eval-in-env (cons 'defmethod (cons gf-name (cdr opt))) env)))
               (let ((gf (%find-gf gf-name)))
                 (when (and gf m (consp m))
                   (%gf-set-defgeneric-methods
                    gf (cons m (%gf-defgeneric-methods gf))))))))
         ;; Return the GENERIC-FUNCTION OBJECT (the GF struct) — CLHS says
         ;; DEFGENERIC returns the generic-function object, and %dg-gf-callable
         ;; (the value the build-rewritten `(eval '(defgeneric …))` path also
         ;; returns) is that same struct.  Returning the struct HERE keeps the
         ;; two DEFGENERIC value paths consistent so ENSURE-GENERIC-FUNCTION
         ;; eql-identity holds: egf.7-13 use `(eval `(defgeneric ,f …))`
         ;; (BACKQUOTE — NOT build-rewritten, so it hits THIS handler), then
         ;; check `(eqlt fn (ensure-generic-function f …))`.  Both now return
         ;; %find-gf's stable struct.  Formerly this returned the stub closure
         ;; (symbol-function), which SIGSEGV'd when funcalled >4 args and broke
         ;; eql vs %dg-gf-callable — the >4-arg GF-struct funcall is now handled
         ;; in compile-funcall (struct slot-8 stub closure), so the struct is
         ;; safely funcallable at any arity.
         (%dg-gf-callable gf-name)))
      ;; (defclass name supers slot-specs &rest options)
      ;; Per-slot options handled: :reader, :writer, :accessor, :initarg,
      ;; :initform, :allocation.  Class-level options handled:
      ;; (:default-initargs k1 v1 ...).  Match against the BARE name
      ;; (no leading colon) because Modus's symbol-name strips the
      ;; leading colon from keywords — same convention as the defgeneric
      ;; handler's "METHOD-COMBINATION" match.
      ((%eval-sym-eq op "DEFCLASS")
       (let* ((class-name (car args))
              (supers (cadr args))
              (slot-specs (caddr args))
              (rest-opts (cdddr args))
              (slot-names nil)
              (initarg-pairs nil)
              (initform-pairs nil)
              (default-initarg-pairs nil)
              (class-allocated-slots nil))
         (dolist (spec slot-specs)
           (let* ((sname (if (consp spec) (car spec) spec))
                  (opts (if (consp spec) (cdr spec) nil)))
             (setq slot-names (cons sname slot-names))
             (let ((cur opts))
               (loop
                 (when (null cur) (return nil))
                 (let ((key (car cur)) (val (cadr cur)))
                   (cond
                     ((and (symbolp key) (%eval-sym-eq key "ALLOCATION"))
                      (when (and (symbolp val) (%eval-sym-eq val "CLASS"))
                        (setq class-allocated-slots
                              (cons sname class-allocated-slots))))
                     ((and (symbolp key) (%eval-sym-eq key "READER"))
                      (let ((slot sname))
                        (set-symbol-function val
                                             (lambda (obj) (slot-value obj slot)))))
                     ((and (symbolp key) (%eval-sym-eq key "ACCESSOR"))
                      (let ((slot sname))
                        (set-symbol-function val
                                             (lambda (obj) (slot-value obj slot)))
                        (let ((set-name
                               (intern (concatenate 'string "SET-"
                                                    (symbol-name val)))))
                          (set-symbol-function set-name
                                               (lambda (obj nv)
                                                 (set-slot-value obj slot nv))))))
                     ((and (symbolp key) (%eval-sym-eq key "WRITER"))
                      (let ((slot sname))
                        (set-symbol-function val
                                             (lambda (nv obj) (set-slot-value obj slot nv)))))
                     ((and (symbolp key) (%eval-sym-eq key "INITARG"))
                      (setq initarg-pairs (cons (cons val sname) initarg-pairs)))
                     ((and (symbolp key) (%eval-sym-eq key "INITFORM"))
                      (let ((form val) (thunk-env env))
                        (setq initform-pairs
                              (cons (cons sname
                                          (lambda () (%eval-in-env form thunk-env)))
                                    initform-pairs))))))
                 (setq cur (cddr cur))))))
         ;; Walk class-level (:default-initargs k1 v1 ...) per CLHS 7.1.4.
         (dolist (opt rest-opts)
           (when (and (consp opt) (symbolp (car opt))
                      (%eval-sym-eq (car opt) "DEFAULT-INITARGS"))
             (let ((cur (cdr opt)))
               (loop
                 (when (or (null cur) (null (cdr cur))) (return nil))
                 (let ((dk (car cur))
                       (dv (cadr cur)))
                   (let ((form dv) (thunk-env env))
                     (setq default-initarg-pairs
                           (cons (cons dk
                                       (lambda () (%eval-in-env form thunk-env)))
                                 default-initarg-pairs))))
                 (setq cur (cddr cur))))))
         (setq slot-names (nreverse slot-names))
         (%defclass class-name slot-names supers)
         (%register-clos-slot-info class-name
                                   (nreverse initarg-pairs)
                                   (nreverse initform-pairs))
         (%register-clos-direct-slots class-name slot-names)
         (%register-clos-class-slots class-name (nreverse class-allocated-slots))
         (%register-clos-default-initargs class-name
                                          (nreverse default-initarg-pairs))
         ;; CLHS 7.7: defclass returns the class object, not the name.
         ;; find-class.15 etc. rely on (eq (eval `(defclass …)) (find-class …)).
         (find-class class-name)))
      ;; DEFSTRUCT — runtime structure definition.  Mirrors the DEFCLASS
      ;; branch: builds a tagged-array representation (%struct-instance
      ;; marker in slot 0, type-name in slot 1, user slots from slot 2),
      ;; registers the type for TYPEP, and installs MAKE-/-P/accessors/
      ;; setters/COPY- in the symbol-function table so the suite's runtime
      ;; calls (make-s-1), (s-1-p x), (s-1-foo x) resolve.  Only the
      ;; plain-and-keyword-constructor subset is handled here; exotic BOA
      ;; constructors still come from the build-time defstruct handler /
      ;; ansi-bridge overrides.
      ((%eval-sym-eq op "DEFSTRUCT")
       (let* ((name-and-opts (car args))
              (struct-name (if (consp name-and-opts)
                               (car name-and-opts) name-and-opts))
              (struct-str (symbol-name struct-name))
              (opts (if (consp name-and-opts) (cdr name-and-opts) nil))
              (raw-slots (cdr args))
              ;; option state
              (conc-specified nil)
              (conc-name (concatenate 'string struct-str "-"))
              (include-parent nil)
              (want-predicate t)
              (pred-name-override nil)
              (want-copier t)
              (copier-override nil)
              (ctor-override-seen nil)
              (ctor-suppressed nil)
              (ctor-boa-seen nil)
              (type-option-seen nil)
              (default-ctor-name nil))
         ;; --- parse options ---
         (dolist (opt opts)
           (cond
             ((and (consp opt) (symbolp (car opt)))
              (let ((on (car opt)))
                (cond
                  ((%eval-sym-eq on "CONC-NAME")
                   (setq conc-specified t)
                   (setq conc-name
                         (if (cadr opt)
                             (let ((cn (cadr opt)))
                               (if (stringp cn) cn (string cn)))
                             "")))
                  ((%eval-sym-eq on "INCLUDE")
                   (setq include-parent (cadr opt)))
                  ((%eval-sym-eq on "PREDICATE")
                   (if (cddr opt)
                       (if (cadr opt)
                           (setq pred-name-override (cadr opt))
                           (setq want-predicate nil))
                       ;; (:predicate) with no arg — keep default
                       nil))
                  ((%eval-sym-eq on "COPIER")
                   (if (cddr opt)
                       (if (cadr opt)
                           (setq copier-override (cadr opt))
                           (setq want-copier nil))
                       nil))
                  ((%eval-sym-eq on "CONSTRUCTOR")
                   (setq ctor-override-seen t)
                   (cond
                     ;; (:constructor nil) — suppress the default ctor.
                     ((and (cdr opt) (null (cadr opt)))
                      (setq ctor-suppressed t))
                     ;; (:constructor name BOA-arglist) — a BOA constructor;
                     ;; the keyword ctor here would be wrong, so leave it to
                     ;; the build-time handler / ansi-bridge overrides and
                     ;; don't install our default keyword ctor.
                     ((cddr opt)
                      (setq ctor-boa-seen t))
                     ;; (:constructor name) — keyword ctor under NAME.
                     ((and (cdr opt) (cadr opt))
                      (setq default-ctor-name (cadr opt)))))
                  ;; (:TYPE ...) — Modus represents structs only as the
                  ;; native tagged array; list/vector typed structs are
                  ;; unsupported.  CLHS also makes several option combos
                  ;; (e.g. :predicate without :named under :type) an error.
                  ;; Signal so defstruct.error.3/.4 (which wrap this in
                  ;; signals-error) see a real condition rather than a
                  ;; silently-accepted-but-wrong struct.
                  ((%eval-sym-eq on "TYPE")
                   (setq type-option-seen t)))))
             ;; bare symbol option e.g. :conc-name — treat as flag-with-default
             (t nil)))
         (when type-option-seen
           (error "DEFSTRUCT :TYPE option is not supported"))
         (unless conc-specified
           (setq conc-name (concatenate 'string struct-str "-")))
         ;; --- parse slots: own slot names + default forms ---
         (let* ((own-slot-names
                 (mapcar (lambda (s) (if (consp s) (car s) s)) raw-slots))
                (own-default-forms
                 (mapcar (lambda (s) (if (consp s) (cadr s) nil)) raw-slots))
                ;; effective slots: parent slots first, then own
                (parent-desc (and include-parent
                                  (%find-struct-type include-parent)))
                (parent-slots (if parent-desc
                                  (%struct-type-desc-slots parent-desc)
                                  nil))
                (eff-slot-names (append parent-slots own-slot-names))
                (thunk-env env))
           ;; --- register the type ---
           (%register-struct-type struct-name include-parent
                                   eff-slot-names conc-name)
           ;; --- constructor: MAKE-<name> (or named) taking keyword args ---
           ;; Build a defaults-vector closure: evaluate each slot's default
           ;; form fresh at each construction (CLHS 3.4.x — initforms run
           ;; per make).  Parent default forms are looked up from the
           ;; parent's stored thunks isn't tracked, so inherited slots
           ;; default to NIL unless overridden — acceptable for the suite's
           ;; cases (no :include + slot-default combos in structures-0x).
           (let* ((eff-defaults
                   ;; align defaults with eff-slot-names: parent slots → nil
                   (append (mapcar (lambda (x) (declare (ignore x)) nil)
                                   parent-slots)
                           own-default-forms))
                  (sname struct-name)
                  (slots eff-slot-names)
                  (defs eff-defaults)
                  (denv thunk-env)
                  (ctor-fn
                   (lambda (&rest kwargs)
                     ;; positional values in effective-slot order
                     (let ((vals nil) (sc slots) (dc defs))
                       (loop
                         (when (null sc) (return nil))
                         (let* ((slot (car sc))
                                (def-form (car dc))
                                ;; scan kwargs for :slot
                                (provided nil) (pval nil) (kc kwargs))
                           (loop
                             (when (null kc) (return nil))
                             (when (and (cdr kc)
                                        (%struct-keyword-matches-slot-p
                                         (car kc) slot))
                               (setq provided t) (setq pval (cadr kc)))
                             (setq kc (cddr kc)))
                           (setq vals
                                 (cons (if provided pval
                                           (if def-form
                                               (%eval-in-env def-form denv)
                                               nil))
                                       vals)))
                         (setq sc (cdr sc))
                         (setq dc (cdr dc)))
                       (%alloc-struct sname (reverse vals))))))
             (let ((ctor-sym
                    (if default-ctor-name default-ctor-name
                        (intern (concatenate 'string "MAKE-" struct-str)))))
               ;; Install the default keyword constructor unless it was
               ;; explicitly suppressed (:constructor nil) or this struct
               ;; uses only a BOA constructor (whose semantics our keyword
               ;; ctor can't reproduce — left to build-time / overrides).
               (unless (or ctor-suppressed
                           (and ctor-boa-seen (not default-ctor-name)))
                 (set-symbol-function ctor-sym ctor-fn))))
           ;; --- predicate: <name>-P ---
           (when want-predicate
             (let* ((pred-sym
                     (if pred-name-override pred-name-override
                         (intern (concatenate 'string struct-str "-P"))))
                    (tname struct-name))
               (set-symbol-function
                pred-sym
                (lambda (x) (if (%struct-instance-typep x tname) t nil)))))
           ;; --- copier: COPY-<name> ---
           (when want-copier
             (let ((copy-sym
                    (if copier-override copier-override
                        (intern (concatenate 'string "COPY-" struct-str)))))
               (set-symbol-function copy-sym
                                    (lambda (x) (%struct-copy x)))))
           ;; --- accessors + setters per effective slot ---
           (let ((sc eff-slot-names))
             (loop
               (when (null sc) (return nil))
               (let* ((slot (car sc))
                      (slot-str (symbol-name slot))
                      (acc-str (concatenate 'string conc-name slot-str))
                      (acc-sym (intern acc-str))
                      (this-slot slot))
                 (set-symbol-function
                  acc-sym
                  (lambda (x) (%struct-ref x this-slot)))
                 ;; SET-<acc> for setf support
                 (let ((set-sym (intern (concatenate 'string "SET-" acc-str))))
                   (set-symbol-function
                    set-sym
                    (lambda (x v) (%struct-set x this-slot v)))))
               (setq sc (cdr sc))))
           struct-name)))
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
             (body (cdr args))
             (block-snap (%eval-escape-snapshot)))
         (handler-case
           (%eval-progn body env)
           (t (c)
             (let ((val (%eval-escape-pop-if bname)))
               (if (eq val :%eval-no-escape)
                   ;; genuine error inside the block — discard any
                   ;; stranded descriptor, then re-raise original C
                   (progn (%eval-escape-unwind block-snap c) (error c))
                   (%eval-escape-return val)))))))
      ;; RETURN-FROM — push (name . value) onto escape stack + signal.
      ;; Capture multiple values via multiple-value-list so a form like
      ;;     (return-from FOO (values a b c))
      ;; carries all values; the escape-recovery side checks for the
      ;; '%mvs marker and applies #'values to surface them.
      ((%eval-sym-eq op "RETURN-FROM")
       (let* ((name (car args))
              (val-form (cadr args))
              (vals (if (cdr args)
                        (multiple-value-list (%eval-in-env val-form env))
                        (list nil))))
         (%eval-escape-push name (cons '%mvs vals))))
      ;; RETURN — escape from the innermost block named NIL or LOOP
      ((%eval-sym-eq op "RETURN")
       (let ((vals (if args
                       (multiple-value-list (%eval-in-env (car args) env))
                       (list nil))))
         (%eval-escape-push nil (cons '%mvs vals))))
      ;; PROG / PROG* — (prog ((var [init])*) tagbody-body).  CLHS:
      ;; equivalent to (block nil (let/let* (...) (tagbody body))).
      ;; Reuses the LET / TAGBODY / BLOCK runtime branches we already
      ;; have so RETURN escapes the surrounding block correctly and
      ;; GO works inside the tagbody.
      ((%eval-sym-eq op "PROG")
       (%eval-in-env
         (list 'block nil
               (list 'let (car args)
                     (cons 'tagbody (cdr args))))
         env))
      ((%eval-sym-eq op "PROG*")
       (%eval-in-env
         (list 'block nil
               (list 'let* (car args)
                     (cons 'tagbody (cdr args))))
         env))
      ;; PROGV — (progv VARS-FORM VALS-FORM BODY...).  Evaluate both
      ;; forms; save and restore via %eval-save-special / %eval-set-special
      ;; so the binding lands in BOTH stores runtime-EVAL consults
      ;; (compiled symbol-value hash AND eval-only alist) — using
      ;; %progv-set alone would write only the compiled store, so a
      ;; runtime read inside body would still see the eval-only value
      ;; from any enclosing LET.
      ;;
      ;; Restore on NORMAL exit only — Modus's compiled unwind-protect
      ;; cleanup path drops the in-flight condition before longjmp, so
      ;; wrapping body in unwind-protect silently eats errors that
      ;; would otherwise propagate to an outer handler-case.  Leaving
      ;; the binding live on abnormal exit is the lesser evil: most
      ;; tests don't error inside the progv body, and the binding
      ;; will be overwritten by the next progv / let-special.
      ((%eval-sym-eq op "PROGV")
       (let* ((vars (%eval-in-env (car args) env))
              (vals (%eval-in-env (cadr args) env))
              (body (cddr args))
              (saved nil))
         (let ((vc vars))
           (loop
             (when (null vc) (return nil))
             (push (cons (car vc) (%eval-save-special (car vc))) saved)
             (setq vc (cdr vc))))
         (let ((vc vars) (vlc vals))
           (loop
             (when (or (null vc) (null vlc)) (return nil))
             (%eval-set-special (car vc) (car vlc))
             (setq vc (cdr vc))
             (setq vlc (cdr vlc))))
         ;; Same MV-preservation: set-symbol-value clobbers MV buffer.
         (let ((result (multiple-value-list (%eval-progn body env))))
           (dolist (sv saved)
             (%eval-restore-special (car sv) (cdr sv)))
           (values-list result))))
      ;; CATCH — (catch TAG-FORM BODY...).  Evaluate TAG-FORM and run
      ;; BODY in a handler that recovers a THROW whose tag is EQ to
      ;; this CATCH's tag.  Tag is reused as the escape-stack key —
      ;; arbitrary objects (typically symbols), not the BLOCK-name set
      ;; %eval-escape-pop-if normally compares.
      ((%eval-sym-eq op "CATCH")
       (let ((tag (%eval-in-env (car args) env))
             (body (cdr args))
             (catch-snap (%eval-escape-snapshot)))
         (handler-case
           (%eval-progn body env)
           (t (c)
             (let ((val (%eval-escape-pop-if tag)))
               (if (eq val :%eval-no-escape)
                   ;; genuine error inside the catch body — discard any
                   ;; stranded descriptor, then re-raise original C
                   (progn (%eval-escape-unwind catch-snap c) (error c))
                   (%eval-escape-return val)))))))
      ;; THROW — (throw TAG-FORM VALUE-FORM).  Push (tag . (cons '%mvs mvs))
      ;; onto escape stack so the surrounding CATCH with matching TAG
      ;; recovers the value(s).  Multiple values via multiple-value-list
      ;; for symmetry with RETURN.
      ((%eval-sym-eq op "THROW")
       (let ((tag (%eval-in-env (car args) env))
             (vals (multiple-value-list (%eval-in-env (cadr args) env))))
         (%eval-escape-push tag (cons '%mvs vals))))
      ;; LOOP — repeat body forever until RETURN (or RETURN-FROM nil)
      ;; escapes via the stack.  Body is treated as an implicit progn
      ;; with implicit (block nil) wrapping for RETURN to target.
      ((%eval-sym-eq op "LOOP")
       (cond
         ;; Extended LOOP: first arg is a bare keyword symbol (for /
         ;; while / until / repeat / with / collect / sum / count / do).
         ;; Hand off to %eval-extended-loop, which parses the clauses
         ;; and runs the iteration.  Simple LOOP (body of forms) keeps
         ;; the old `repeat-until-return' semantics below.
         ((and args (symbolp (car args))
               (%loop-keyword-p (car args)))
          (%eval-extended-loop args env))
         (t
          (let ((sloop-snap (%eval-escape-snapshot)))
            (handler-case
              (let ((dummy nil))
                (declare (ignore dummy))
                (loop (%eval-progn args env)))
              (t (c)
                (let ((val (%eval-escape-pop-if nil)))
                  (if (eq val :%eval-no-escape)
                      ;; genuine error inside the simple-LOOP body —
                      ;; discard any stranded descriptor, then re-raise C
                      (progn (%eval-escape-unwind sloop-snap c) (error c))
                      (%eval-escape-return val)))))))))
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
      ;; TAGBODY — eval forms in order, jumping to tag on (GO TAG).
      ;; Tags are atoms (symbols or integers) at the top level; forms
      ;; are lists.  GO signals via the escape stack with tag = the
      ;; go-target-symbol; TAGBODY catches and resumes from that label.
      ((%eval-sym-eq op "TAGBODY")
       (let ((tags-and-forms args)
             (tb-snap (%eval-escape-snapshot)))
         (let ((start tags-and-forms))
           (loop
             (handler-case
               (let ((cur start))
                 (loop
                   (when (null cur) (return nil))
                   (when (consp (car cur))
                     (%eval-in-env (car cur) env))
                   (setq cur (cdr cur)))
                 ;; Fell off end — exit TAGBODY normally
                 (return nil))
               (t (c)
                 ;; Did the escape target one of OUR tags?  Pop only
                 ;; if matched, resume from that label.
                 (cond
                   ((and *%eval-escape-stack*
                         (let ((esc-tag (car (car *%eval-escape-stack*))))
                           (and (atom esc-tag)
                                (let ((sub tags-and-forms))
                                  (loop
                                    (when (null sub) (return nil))
                                    (when (and (atom (car sub))
                                               (eq (car sub) esc-tag))
                                      (return t))
                                    (setq sub (cdr sub)))))))
                    ;; Pop the escape, resume scan from the tag.
                    (let ((tag (car (car *%eval-escape-stack*))))
                      (setq *%eval-escape-stack*
                            (cdr *%eval-escape-stack*))
                      ;; Advance start to point AFTER the tag.
                      (let ((sub tags-and-forms))
                        (loop
                          (when (null sub) (return nil))
                          (when (and (atom (car sub)) (eq (car sub) tag))
                            (setq start (cdr sub))
                            (return nil))
                          (setq sub (cdr sub))))))
                   (t
                    ;; Not one of our GO tags — re-raise the original
                    ;; condition C (a genuine error inside the tagbody).
                    ;; Discard any descriptor an inner escape stranded
                    ;; while C unwound past its catcher.
                    (%eval-escape-unwind tb-snap c)
                    (error c))))))
           nil)))
      ;; GO — push tag onto escape stack and signal
      ((%eval-sym-eq op "GO")
       (%eval-escape-push (car args) nil))
      ;; THE (ignore type decl)
      ((%eval-sym-eq op "THE")
       (%eval-in-env (cadr args) env))
      ;; DECLARE (ignore)
      ((%eval-sym-eq op "DECLARE") nil)
      ;; DECLAIM / PROCLAIM — global declarations are no-ops at runtime.
      ;; Without these, runtime-EVAL of a (declaim (ftype …)) form (common
      ;; in uiop's eval-when bootstrap, e.g. asdf.lisp's ensure-exported
      ;; ftype declaim that sits BETWEEN two defuns in one eval-when) falls
      ;; through to the funcall path, %eval-escape's with an empty stack,
      ;; and aborts the whole eval-when progn — so ensure-package and every
      ;; later define-package never gets defined.  CLHS: DECLAIM/PROCLAIM
      ;; affect compilation/declaration context only; at runtime the tree-
      ;; walking interpreter ignores type/ftype/inline/optimize declarations,
      ;; so returning NIL is conformant for our purposes.
      ((%eval-sym-eq op "DECLAIM") nil)
      ((%eval-sym-eq op "PROCLAIM") nil)
      ;; LOCALLY (just eval body)
      ((%eval-sym-eq op "LOCALLY")
       (%eval-progn args env))
      ;; LOAD-TIME-VALUE (eval now)
      ((%eval-sym-eq op "LOAD-TIME-VALUE")
       (%eval-in-env (car args) env))
      ;; EVAL-WHEN — (eval-when (SITUATION*) BODY*)
      ;; The runtime tree-walking interpreter IS the `eval` processing path
      ;; (CLHS 3.2.3.1 "not-compile-time"), never compile-file top-level
      ;; processing.  Per CLHS, when an EVAL-WHEN is evaluated by EVAL the
      ;; body runs iff :EXECUTE (or the deprecated EVAL) is among the
      ;; situations; :COMPILE-TOPLEVEL / :LOAD-TOPLEVEL (and deprecated
      ;; COMPILE / LOAD) alone do NOT trigger evaluation here.  Empty/absent
      ;; situations → NIL.
      ((%eval-sym-eq op "EVAL-WHEN")
       (let ((situations (car args))
             (run nil))
         (dolist (s situations)
           (when (or (%eval-sym-eq s "EXECUTE") (%eval-sym-eq s "EVAL"))
             (setq run t)))
         (if run
             (%eval-progn (cdr args) env)
             nil)))
      ;; HANDLER-BIND — (handler-bind ((TYPE FN-FORM)*) BODY*)
      ;; Mirrors build-ansi-test.lisp's compile-time rewrite into a
      ;; %with-handler-bind call.  Each binding's FN-FORM is evaluated
      ;; in the surrounding env to produce the handler closure; the body
      ;; runs inside a thunk so %with-handler-bind can establish the
      ;; restart frame around it.
      ((%eval-sym-eq op "HANDLER-BIND")
       (let* ((bindings (car args))
              (body (cdr args))
              (binding-pairs nil))
         (dolist (b bindings)
           (let ((type (car b))
                 (fn (%eval-in-env (cadr b) env)))
             (push (list type fn) binding-pairs)))
         (setq binding-pairs (nreverse binding-pairs))
         (cond
           ((null binding-pairs) (%eval-progn body env))
           (t (%with-handler-bind
                binding-pairs
                (list '%interp-closure nil body env))))))
      ;; WITH-SIMPLE-RESTART — same as the build rewrite (just the body).
      ;; A full impl would establish a named ABORT-style restart; tests
      ;; that just rely on body's value pass with this stub.
      ((%eval-sym-eq op "WITH-SIMPLE-RESTART")
       (%eval-progn (cdr args) env))
      ;; RESTART-CASE — (restart-case PROTECTED-FORM (NAME (ARGS) [:report …] BODY)*)
      ;; Mirrors build-ansi-test.lisp's compile-time rewrite into a
      ;; %with-restarts call.  Compiled %with-restarts handles the
      ;; runtime restart frame + INVOKE-RESTART dispatch directly; we
      ;; just hand it lists of (NAME interp-closure REPORT) and a
      ;; thunk that evaluates PROTECTED-FORM in the current env.
      ((%eval-sym-eq op "RESTART-CASE")
       (let* ((protected-form (car args))
              (clauses (cdr args))
              (restart-cells nil))
         (dolist (clause clauses)
           (let* ((rname (car clause))
                  (cl-args (cadr clause))
                  (rest-opts (cddr clause))
                  (body nil))
             ;; Skip :report / :interactive / :test options until we
             ;; hit a non-keyword head — the rest is the body.
             (let ((done nil))
               (loop
                 (when done (return nil))
                 (cond
                   ((null rest-opts) (setq done t))
                   ((keywordp (car rest-opts))
                    (setq rest-opts (cddr rest-opts)))
                   (t (setq body rest-opts) (setq done t)))))
             (push (list rname
                         (list '%interp-closure cl-args body env)
                         nil)
                   restart-cells)))
         (setq restart-cells (nreverse restart-cells))
         (%with-restarts
           restart-cells
           (list '%interp-closure nil (list protected-form) env))))
      ;; HANDLER-CASE — evaluate body; on error, find a matching handler
      ;; clause and evaluate its body with the condition bound to the var.
      ;; Clauses look like (TYPE (VAR) BODY...) or (TYPE () BODY...).
      ;; TYPE T matches anything; ERROR matches any error.
      ((%eval-sym-eq op "HANDLER-CASE")
       (let ((body-form (car args))
             (clauses (cdr args)))
         (handler-case
           (%eval-in-env body-form env)
           (t (c)
             ;; In-flight %eval-escape (BLOCK/RETURN-FROM/GO/THROW/RETURN)?
             ;; A non-local exit whose target is OUTSIDE this handler-case
             ;; signals via *%eval-escape-stack* + (error "%eval-escape").
             ;; HANDLER-CASE must NOT treat that as a real condition — its
             ;; (error ...)/(t ...)/condition clauses would swallow the escape,
             ;; leaving the descriptor stranded on the stack and silently
             ;; dropping the non-local exit (the UIOP ensure-package
             ;; %eval-escape leak — also hit via IGNORE-ERRORS, which expands
             ;; to HANDLER-CASE).  Re-propagate so the matching BLOCK / CATCH /
             ;; TAGBODY / LOOP pops its own descriptor.
             (when *%eval-escape-stack* (error c))
             (let ((found nil) (result nil))
               (dolist (cl clauses)
                 (unless found
                   (let* ((type-spec (car cl))
                          (lambda-list (cadr cl))
                          (clause-body (cddr cl))
                          (matches (or (%eval-sym-eq type-spec "T")
                                       (%eval-sym-eq type-spec "ERROR")
                                       (%eval-sym-eq type-spec "CONDITION")
                                       (typep c type-spec))))
                     (when matches
                       (setq found t)
                       (let ((new-env
                              (if (and (consp lambda-list) (car lambda-list))
                                  (%env-extend (car lambda-list) c env)
                                  env)))
                         (setq result (%eval-progn clause-body new-env)))))))
               ;; ANSI (CLHS 9.1.4.1): if NO clause type matched, HANDLER-CASE
               ;; must DECLINE — the condition keeps propagating to an outer
               ;; handler.  The outer compiled (t (c) ...) above catches EVERY
               ;; condition so we can inspect/dispatch it, so a no-match must
               ;; RE-SIGNAL rather than fall through to NIL.  Returning NIL here
               ;; silently swallowed conditions (e.g. an UNDEFINED-FUNCTION
               ;; raised inside an inner handler-case that only names TYPE-ERROR
               ;; never reached the outer UNDEFINED-FUNCTION clause) — the
               ;; ASDF-gauntlet form-109 corruption: uiop wraps pathname ops in
               ;; handler-case/ignore-errors that swallowed a deep undefined-fn,
               ;; cascading on the leaked NIL.
               (if found result (error c)))))))
      ;; UNWIND-PROTECT
      ((%eval-sym-eq op "UNWIND-PROTECT")
       (unwind-protect
         (%eval-in-env (car args) env)
         (%eval-progn (cdr args) env)))
      ;; WITH-OUTPUT-TO-STRING — (with-output-to-string (var) body...)
      ;; Bind var to a fresh string-output-stream, run body, return collected string.
      ;; The optional second spec slot is a target string (NYI; ignored — body just runs).
      ;; If var is an *earmuff*, dynamic-bind it so compiled writers
      ;; (e.g. (prin1 X) without explicit stream) see the new stream.
      ((%eval-sym-eq op "WITH-OUTPUT-TO-STRING")
       (let* ((spec (car args))
              (var (car spec))
              (body (cdr args))
              (stream (make-string-output-stream)))
         (cond
           ((%earmuff-var-p var)
            (let ((saved (%eval-save-special var)))
              (%eval-set-special var stream)
              (unwind-protect
                   (%eval-progn body env)
                (%eval-restore-special var saved))
              (get-output-stream-string stream)))
           (t
            (%eval-progn body (%env-extend var stream env))
            (get-output-stream-string stream)))))
      ;; WITH-INPUT-FROM-STRING — (with-input-from-string (var string) body...)
      ;; Bind var to a string-input-stream over the evaluated string, run body.
      ((%eval-sym-eq op "WITH-INPUT-FROM-STRING")
       (let* ((spec (car args))
              (var (car spec))
              (str-form (cadr spec))
              (body (cdr args))
              (str (%eval-in-env str-form env))
              (stream (make-string-input-stream str)))
         (cond
           ((%earmuff-var-p var)
            (let ((saved (%eval-save-special var)))
              (%eval-set-special var stream)
              (unwind-protect
                   (%eval-progn body env)
                (%eval-restore-special var saved))))
           (t
            (%eval-progn body (%env-extend var stream env))))))
      ;; FLET / LABELS — per CLHS §6.1.1.4 each definition's body is
      ;; implicitly wrapped in (block NAME ...) so (return-from NAME ...)
      ;; from inside the body escapes to that function's call.
      ((%eval-sym-eq op "FLET")
       (let ((local-fns (car args))
             (body (cdr args)))
         (let ((new-env env))
           (dolist (def local-fns)
             (let ((fname (car def))
                   (params (cadr def))
                   (fbody (cddr def)))
               (let ((wrapped-body (list (cons 'block (cons fname fbody)))))
                 (let ((fn (list '%interp-closure params wrapped-body new-env)))
                   (setq new-env (%env-extend fname fn new-env))))))
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
               (let ((wrapped-body (list (cons 'block (cons fname fbody)))))
                 (let ((fn (list '%interp-closure params wrapped-body nil)))
                   ;; Will fix env pointer below
                   (setq new-env (%env-extend fname fn new-env))))))
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
      ;; Macro call (CL sym or native MVM sym).  Must be checked BEFORE
      ;; the function-call branches: macros do not evaluate their args.
      ;; ONLY fires for a CALLABLE expander — runtime DEFMACRO stores
      ;; an %interp-closure here.  Use %RAW-MACRO-EXPANDER, NOT
      ;; macro-function: macro-function WRAPS the raw %interp-closure in a
      ;; user-facing closure object (subtag #x52, via %interp-macro-shim)
      ;; so `(funcall (macro-function 'X) form env)` works — but that
      ;; wrapper is NOT an %interp-closure, so it would fall to the
      ;; `(funcall mf form)` branch below and crash (the shim expects the
      ;; macro's *args*, not the whole form).  This bug surfaced via
      ;; uiop's runtime-DEFMACRO'd DEFINE-PACKAGE.  %raw-macro-expander
      ;; returns the bare expander, mirroring macroexpand-1.
      ;; A raw T marker means a compiler-only macro (PUSH/POP/COND/…)
      ;; with no runtime expander — fall through to the function-call
      ;; path; eval then errors with undefined-function, which the suite
      ;; expects when it evals a form that's only a compiler macro.
      ((and (or (%cl-sym-p op) (%native-mvm-sym-p op))
            (let ((mf (%raw-macro-expander op)))
              (and mf (not (eq mf t)))))
       (let* ((mf (%raw-macro-expander op))
              (expanded (cond
                          ((%interp-closure-p mf)
                           (%call-interp-closure mf args))
                          (t
                           ;; mvm-define-macro lambda — (lambda (form) ...)
                           (handler-case (funcall mf form)
                             (t (c) (funcall mf form nil)))))))
         (%eval-in-env expanded env)))
      ;; Function call: CL symbol
      ((%cl-sym-p op)
       (%eval-funcall op args env))
      ;; Function call: native MVM symbol (subtag #x50, hash-only).
      ;; Use %eval-call-fn which dispatches by arg count via funcall —
      ;; runtime funcall has %NATIVE-SYM-RESOLVE built in for native syms.
      ((%native-sym-p op)
       (let ((evaled-args (%eval-args args env)))
         (%eval-call-fn op evaled-args nil)))
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
        ;; First check local env for function binding.
        ;; CLHS 3.1.2.1.2: operator position resolves in the FUNCTION
        ;; namespace.  FLET/LABELS bind local functions into the SAME ENV
        ;; alist as LET variables, so an unguarded name match lets a
        ;; variable that happens to share a global function's name shadow
        ;; the function.  UIOP's DIRECTORY-PATHNAME-P does exactly this:
        ;;   (let ((pathname (pathname pathname))) ...)
        ;; where PATHNAME is both the bound variable and #'pathname.  The
        ;; unguarded lookup found the variable (a pathname object) and
        ;; %do-funcall'd it; a #x32 array routes through GF dispatch and
        ;; destructively aborts (uncatchable — skips handler-case), which
        ;; corrupted the open LOAD stream and was the ASDF-gauntlet
        ;; form-109 wall.  Only accept the local binding when its value is
        ;; an %interp-closure (what FLET/LABELS store) — mirroring the
        ;; #'name guard already in %eval-function-form.
        (let ((local (%env-lookup sym env)))
          (if (and (car local) (%interp-closure-p (cdr local)))
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
                          ;; Undefined function — publish + run handler-bind
                          ;; so (cell-error (c) (cell-error-name c)) matches
                          ;; and carries the :name slot (cell-error-name.2).
                          (let ((c (%make-condition 'undefined-function (list :name sym))))
                            (setq *current-condition* c)
                            (%associate-active-restart-frames c)
                            (let ((handled (%signal-condition c)))
                              (if handled
                                  nil
                                  (if (%error-handler-active-p)
                                      (%hc-longjmp)
                                      nil)))))))))))))

(defun %register-setf-expander (name fn)
  "Add NAME → FN to *runtime-setf-expanders*, replacing any prior entry."
  (let ((found nil)
        (cur *runtime-setf-expanders*)
        (acc nil))
    (loop
      (when (null cur) (return nil))
      (cond
        ((eq (car (car cur)) name)
         (setq acc (cons (cons name fn) acc))
         (setq found t))
        (t (setq acc (cons (car cur) acc))))
      (setq cur (cdr cur)))
    (unless found (setq acc (cons (cons name fn) acc)))
    (setq *runtime-setf-expanders* acc))
  name)



;;; Dead-code walker helpers (no callers anywhere; kept with the engine
;;; they belong to rather than deleted blind — candidates for removal).

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

(defun %eval-block (name forms env)
  "Evaluate (block name forms...) with return-from support."
  (let ((snap (%eval-escape-snapshot)))
    (handler-case
      (%eval-progn forms env)
      (t (c)
        (let ((val (%eval-escape-pop-if name)))
          (if (eq val :%eval-no-escape)
              ;; Not one of our RETURN-FROM escapes — re-raise the ORIGINAL
              ;; condition C (a genuine error signalled inside the block) so
              ;; an outer handler sees its real type/message instead of a
              ;; masked "%eval-escape".  First discard any descriptor an
              ;; inner escape stranded while C unwound past its catcher.
              (progn (%eval-escape-unwind snap c) (error c))
              (%eval-escape-return val)))))))

;;; ------------------------------------------------------------------
;;; Engine-provider overrides (last-defun-wins over the cl-eval.lisp /
;;; ansi-bridge.lisp stubs — this file loads after both in fork builds).
;;; ------------------------------------------------------------------

(defun %call-interp-closure (fn args)
  "Walker engine provider: run the interp closure on the tree-walker."
  (%call-interp-closure-walker fn args))

(defun %expand-deftype (type)
  "Walker engine provider: expand a user deftype by binding the registered
   lambda list to the UNEVALUATED type args and walking the body."
  (let* ((head (if (consp type) (car type) type))
         (args (if (consp type) (cdr type) nil))
         (entry (%deftype-lookup head)))
    (if (null entry)
        nil
        (let ((env (%bind-params (car entry) args nil)))
          (%eval-progn (cdr entry) env)))))
