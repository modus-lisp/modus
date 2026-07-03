;;;; eval2.lisp — in-image runtime evaluator: compile a list of top-level
;;;; forms to ONE MVM bytecode module and execute the trailing expression via
;;;; mvm-interpret.  Extracted verbatim from build-generic.lisp's
;;;; *stage2-test-source* so the same source feeds BOTH the generic oracle
;;;; binary and the ANSI image (WS3: self-host the compiler + retire the
;;;; tree-walker).  Depends on mvm.lisp (ISA), interp.lisp (mvm-interpret),
;;;; and compiler.lisp (mvm-compile-toplevel / emit pipeline) being loaded
;;;; earlier in the image source.
;; eval2-forms: compile a LIST of top-level forms (helper defuns followed by a
;; trailing expression) into ONE bytecode module and run the expression.  CALLs
;; between the functions resolve in-module (bytecode->bytecode) so NO native
;; bridge / value marshalling is needed — one representation throughout.  This
;; is the 'drop native' model: the interpreter runs everything as bytecode.
(defvar *eval2-buffer* nil
  "PERF: persistent 64KB bytecode buffer reused across eval2-forms calls (see
   the reuse site) instead of (make-array 65536) every call — mvm-buffer-used-
   bytes copies the bytecode out before the next call, so nothing retains it.")

(defvar *eval2-cache* nil
  "PERF Round 2 (compile-caching): EQUAL hash, FORMS → (bc entry fn-table
   rt-table lam-offsets) — the compiled MVM module for those forms.  eval2 of a
   tiny form is ~26x compile-bound; the harness/asdf re-eval the same forms
   constantly, so caching the compiled bytecode skips the whole ~15M-cycle
   compile and pays only ~0.6M interpret.  Re-interpreting the cached bytecode
   is correct: the RESULT depends on live runtime state, not the cache.  bc is a
   COPY (mvm-buffer-used-bytes) so it's safe despite *eval2-buffer* reuse.")

(defun %eval2-cacheable-p (forms)
  "T unless any top-level form is a DEF* / IN-PACKAGE / EVAL-WHEN whose FIRST
   compile has side effects (macro/trampoline/package registration) that a
   cache HIT would skip.  Those always take the full (uncached) compile path;
   plain expressions — the bulk of harness evals — are cached."
  (dolist (f forms t)
    (when (and (consp f) (symbolp (car f)))
      (let ((n (symbol-name (car f))))
        (when (or (string-equal n "DEFUN") (string-equal n "DEFMACRO")
                  (string-equal n "DEFPACKAGE") (string-equal n "IN-PACKAGE")
                  (string-equal n "DEFVAR") (string-equal n "DEFPARAMETER")
                  (string-equal n "DEFCONSTANT") (string-equal n "DEFSTRUCT")
                  (string-equal n "DEFCLASS") (string-equal n "DEFGENERIC")
                  (string-equal n "DEFMETHOD") (string-equal n "DEFTYPE")
                  (string-equal n "DEFINE-CONDITION") (string-equal n "DEFSETF")
                  (string-equal n "DEFINE-SYMBOL-MACRO") (string-equal n "MACROLET")
                  (string-equal n "DEFINE-METHOD-COMBINATION")
                  (string-equal n "EVAL-WHEN"))
          (return-from %eval2-cacheable-p nil))))))

(defun %e2-scan-persist (f acc depth)
  "WS3 def persistence pre-scan (see persist-names in eval2-forms): return ACC
   extended with the names (strings) of every toplevel-context DEFUN in form F.
   CLHS 3.2.3.1: forms in a top-level EVAL-WHEN / PROGN / LOCALLY body are
   THEMSELVES top level — recurse those.  A top-level MACRO call's expansion is
   also processed as top level, so a head that is none of the above is expanded
   ONE step (macroexpand-1-mvm sees both the compiler *macro-table* and, under
   *eval2-runtime-p*, the runtime macro tables) and re-inspected — this is what
   lets the scan see uiop's (with-upgradability () (defun featurep ...)) →
   eval-when → defun* → progn → defun chain.  The old raw-heads-only scan
   missed every macro-wrapped defun: single-form (eval FORM) compiles FORM as
   the %eval2-thunk BODY (compile-form, never mvm-compile-toplevel), so the
   compiler-recorded *e2-persist-defuns* path never fired either, and the
   block's own (detect-os) funcall-by-symbol died with CALL-IND non-callable
   (asdf gauntlet FAILFORM 43/45).  DEPTH guards runaway expansion; expansion
   errors are treated as no-expansion (the real compile will report them)."
  (if (or (> depth 40) (not (and (consp f) (symbolp (car f)))))
      acc
      (let ((hn (symbol-name (car f))))
        (cond
          ((and (string-equal hn "DEFUN") (symbolp (cadr f)))
           (cons (string (cadr f)) acc))
          ((string-equal hn "EVAL-WHEN")
           (dolist (sub (cddr f) acc)
             (setq acc (%e2-scan-persist sub acc 0))))
          ((or (string-equal hn "PROGN") (string-equal hn "LOCALLY"))
           (dolist (sub (cdr f) acc)
             (setq acc (%e2-scan-persist sub acc 0))))
          ;; PERF GATE: only a RUNTIME user macro (with-upgradability et al,
          ;; resolved via macroexpand-1-mvm's *eval2-runtime-p* fallback) can
          ;; hide a toplevel defun the raw heads miss.  Bootstrap/compile-time
          ;; macros in *macro-table* (loop, handler-case, dolist, …) never
          ;; expand to toplevel defuns — skip them so the pre-scan doesn't
          ;; double-run their expanders on every eval2 call.
          ((and *macro-table* (gethash (normalize-name (car f)) *macro-table*))
           acc)
          (t
           (let ((ex (handler-case (macroexpand-1-mvm f)
                       (t (c) (cons f nil)))))
             (if (cdr ex)
                 (%e2-scan-persist (car ex) acc (+ depth 1))
                 acc)))))))

(defun %eval2-run-tuple (tuple)
  "Interpret a cached compiled module tuple (bc entry fn-table rt-table lam-offsets).
   Conditions PROPAGATE to the caller (production EVAL semantics): an error
   signalled inside the evaluated form must reach the caller's handler-case.
   The old `(handler-case ... (error (e) (list :interp-err e)))` swallowed the
   signal into a return VALUE, so every `(eval ...) must signal` ANSI test
   (vector-push*.error, defmethod.error, signals-error helpers) failed under
   the WS3 flip.  Callers that want the capture behaviour (the e2diff gate)
   wrap eval2 in their own handler-case."
  ;; MULTIPLE VALUES propagate (WS3 flip): mvm-interpret stashes the run's
  ;; simulated MV state in *mvm-last-mv* (read IMMEDIATELY — see interp.lisp);
  ;; re-emit via values-list so the native caller's multiple-value-list /
  ;; m-v-bind around (eval …) sees every value, matching the tree-walker.
  ;; The tail if/values-list shape is values-preserving per
  ;; tail-form-is-values-p, so this function's epilogue does NOT reset
  ;; MV-count back to 1.
  (let* ((%prim (%mvm-wrap-escaping-result
                  (mvm-interpret (car tuple) :entry-point (cadr tuple)
                                 :function-table (caddr tuple)
                                 :runtime-table (cadddr tuple) :return-raw nil
                                 :lambda-offsets (car (cddddr tuple)))
                  (car tuple) (caddr tuple) (cadddr tuple) (car (cddddr tuple))))
         (%mv *mvm-last-mv*))
    (if %mv
        (if (eql (car %mv) 0)
            (values)
            (values-list (cons %prim (cdr %mv))))
        %prim)))

(defun eval2-forms (forms)
  ;; In-image: emit integer literals as fixnum-safe :li-halves (set the GLOBAL,
  ;; not a let-binding — compiled LET of a special may not establish a dynamic
  ;; binding the compiler's compile-integer reads).  Native builds never call
  ;; eval2-forms, so the global stays NIL there.
  (setq *mvm-emit-halves* t)
  ;; Mark in-image runtime compilation so mvm-compile-toplevel routes package
  ;; side-effecting forms (DEFPACKAGE) to their runtime impl instead of the
  ;; build-time no-op.  setq (not let): compiled let of a special is unreliable
  ;; in-image (same reason as *mvm-emit-halves* above).  Native builds never
  ;; call eval2-forms, so the global stays NIL at build time.
  (setq *eval2-runtime-p* t)
  ;; LAZY opcode-table init.  encode-instruction (mvm.lisp) reads *opcode-table*
  ;; for each instruction's operand spec during Pass-2 emit; with a NIL table
  ;; (the ANSI image skips init-all-globals, so the defparameter init thunk
  ;; never ran) every operand is silently DROPPED → corrupt bytecode → garbage
  ;; result.  Create + populate here, on first eval2 use, NOT at boot: a
  ;; permanent populated *opcode-table* GC root shifts GC timing enough to
  ;; surface a latent crash elsewhere (GET-INTERNAL-RUN-TIME.2 / 0xDEAD0004),
  ;; and eval2 is dead code until the WS3 flip, so lazy keeps the normal image's
  ;; live set identical to baseline.  Skips when already populated (the generic
  ;; image, or a 2nd eval2 call).  NB %populate-opcode-table's `setf gethash`
  ;; no-ops on a NIL table, so the table MUST be created first.
  (unless (and *opcode-table* (> (hash-table-count *opcode-table*) 0))
    (setq *opcode-table* (make-hash-table :test (quote eql)))
    (%populate-opcode-table))
  ;; PERF Round 2 (compile-caching): if these FORMS are cacheable (no side-
  ;; effecting DEF*) and already compiled, re-interpret the cached module and
  ;; skip the whole compile pipeline.  Checked BEFORE the big let so a hit pays
  ;; none of the setup/hash-alloc cost either.
  (let ((%cacheable (%eval2-cacheable-p forms)))
    (when %cacheable
      (unless *eval2-cache*
        (setq *eval2-cache* (make-hash-table :test (quote equal))))
      (let ((%hit (gethash forms *eval2-cache*)))
        (when %hit (return-from eval2-forms (%eval2-run-tuple %hit)))))
  (let ((*functions* (make-hash-table :test (quote equal)))
        (*function-table* nil)
        (*constant-table* nil)
        (*label-counter* 0)
        (*unresolved-calls* (make-hash-table :test (quote equal)))
        (*macro-table* (make-hash-table :test (quote eql)))
        (*globals* (make-hash-table :test (quote eql)))
        (*constants* (make-hash-table :test (quote eql)))
        (*loop-exit-label* nil)
        (*block-labels* nil)
        (*tagbody-tags* nil)
        (*pending-flet-ir* nil)
        (*init-thunk-names* nil)
        (all-ir nil)
        (buf nil)
        (global-offset 0)
        (entry nil)
        (rt-table (make-hash-table))
        (rt-next #x40000000)
        ;; WS3 def persistence: names of top-level user DEFUNs in FORMS.  Each is
        ;; compiled as a real module function and, after the module builds,
        ;; installed as a re-entrant interp trampoline in the global function
        ;; tables — so a LATER (eval2 …) call OR the tree-walker can call it.
        ;; Without this every (eval2 '(defun f …)) discarded f (closed-world
        ;; module), so no multi-form program (asdf/load/REPL) could run on eval2.
        ;; Computed AFTER register-mvm-bootstrap-macros (below) via
        ;; %e2-scan-persist, which sees through EVAL-WHEN / PROGN / LOCALLY
        ;; AND macro wrappers (with-upgradability) — see its docstring.
        (persist-names nil))
    (register-mvm-bootstrap-macros)
    ;; WS3 def persistence pre-scan.  Runs with *macro-table* freshly populated
    ;; (bootstrap macros) so %e2-scan-persist's macroexpand-1-mvm sees the same
    ;; macro environment the compile loop below will; runtime user macros
    ;; resolve through the *eval2-runtime-p* fallback.
    (dolist (f forms)
      (setq persist-names (%e2-scan-persist f persist-names 0)))
    ;; Split: last form is the expression; preceding forms are definitions.
    ;; A TRAILING defun is moved into the module definitions (so it gets a real
    ;; module function + offset to install a trampoline for) and the thunk
    ;; returns its NAME (matching real defun's value), instead of being wrapped
    ;; as a nested defun that compiles in-module and yields NIL.
    (let* ((rforms (reverse forms))
           (last-form (car rforms))
           (last-defun-p (and (consp last-form) (symbolp (car last-form))
                              (string-equal (symbol-name (car last-form)) "DEFUN")
                              (symbolp (cadr last-form))))
           (expr (if last-defun-p (list (quote quote) (cadr last-form)) last-form))
           (defs (if last-defun-p forms (reverse (cdr rforms))))
           (toplevel (append defs (list (list (quote defun) (quote %eval2-thunk) nil expr))))
           ;; REENTRANCY: a NESTED eval2 during this compile loop (the toplevel
           ;; DEFMACRO handler's expander eval, build-macrolet-expander,
           ;; DEFCONSTANT value eval) re-enters eval2-forms, which clears and
           ;; consumes *e2-persist-defuns* — clobbering the outer call's
           ;; accumulated names.  Save the current value in a LEXICAL here and
           ;; restore it after the union below, so each nesting level sees only
           ;; its own recordings.  (Explicit save+setq+restore, NOT a let of
           ;; the special — compiled let of a special is unreliable in-image.)
           (%e2pd-saved *e2-persist-defuns*))
      ;; Compiler-recorded persistence: clear the global BEFORE the compile
      ;; loop; the toplevel DEFUN handler pushes every toplevel-context defun
      ;; name (post-macroexpansion) onto it.  setq, not let (compiled let of
      ;; a special is unreliable in-image — see *mvm-emit-halves*).
      (setq *e2-persist-defuns* nil)
      (dolist (f toplevel)
        (let ((result (mvm-compile-toplevel f)))
          (cond
            ((null result) nil)
            ((and (consp result) (eq (car result) :multi-result))
             (dolist (sub (cdr result))
               (when (and (car sub) (cdr sub))
                 (setq all-ir (cons (cons (car sub) (cdr sub)) all-ir)))))
            (t (when (and (car result) (cdr result))
                 (setq all-ir (cons (cons (car result) (cdr result)) all-ir)))))))
      ;; Drain *pending-flet-ir*: captureless lambdas, flet/labels bodies, and
      ;; nested defuns register their (info . ir) here instead of returning it
      ;; from mvm-compile-toplevel.  Without emitting these, a `#'(lambda ...)`
      ;; (funcall lambda) would resolve its li-func to an UN-OFFSET function and
      ;; jump to bytecode 0 → infinite recursion.  Append so they get Pass-1
      ;; offsets and Pass-2 bytecode like any other function.
      (dolist (pend *pending-flet-ir*)
        (when (and (consp pend) (car pend) (cdr pend))
          (setq all-ir (cons pend all-ir))))
      (setq all-ir (reverse all-ir))
      ;; Union the compiler-recorded defun names (macro-hidden defuns the
      ;; raw-forms pre-scan can't see — with-upgradability → eval-when →
      ;; defun) into persist-names for the trampoline install loop below.
      ;; %EVAL2-THUNK excludes itself by name.  NB: a NESTED eval2 during
      ;; this compile loop (a macroexpander that itself calls EVAL) would
      ;; clear/consume the global mid-flight; the pre-scan names remain as
      ;; the safety net in that (rare) case.
      (dolist (pn *e2-persist-defuns*)
        (unless (or (string-equal pn "%EVAL2-THUNK")
                    (member pn persist-names :test (function string=)))
          (setq persist-names (cons pn persist-names))))
      ;; REENTRANCY: restore the enclosing eval2-forms call's recordings
      ;; (see %e2pd-saved above).  Outermost call restores NIL — harmless.
      (setq *e2-persist-defuns* %e2pd-saved))
    ;; Small buffer (the default 128MB byte array blows the in-image heap).
    ;; PERF: REUSE a persistent 64KB buffer instead of (make-array 65536) every
    ;; call.  mvm-buffer-used-bytes copies the bytecode out before the next call,
    ;; so nothing retains the buffer — safe to reset + reuse.
    (if *eval2-buffer*
        (progn
          (setf (mvm-buffer-position *eval2-buffer*) 0)
          (clrhash (mvm-buffer-labels *eval2-buffer*))
          (setf (mvm-buffer-fixups *eval2-buffer*) nil)
          (setq buf *eval2-buffer*))
        (progn
          (setq buf (make-mvm-buffer :bytes (make-array 65536)))
          (setq *eval2-buffer* buf)))
    ;; Pass 1: size each function, assign cumulative bytecode offsets, register
    ;; in *functions* so emit-time CALL resolution finds them.
    (dolist (e all-ir)
      (let* ((info (car e)) (ir (cdr e))
             (fn-size (let ((s 0)) (dolist (insn ir) (setq s (+ s (ir-instruction-size insn)))) s)))
        (setf (function-info-bytecode-offset info) global-offset)
        (setf (function-info-bytecode-length info) fn-size)
        (setf (gethash (function-info-name info) *functions*) info)
        (setq global-offset (+ global-offset fn-size))))
    ;; Pass 1.5: any CALL to a name NOT defined in this module is a RUNTIME call
    ;; (a native function in the image).  Register a synthetic stub at a high
    ;; offset so emit resolves the CALL there; the interpreter's runtime-table
    ;; maps that offset back to the name and funcalls the native fn (WS1 bridge).
    ;; LI-FUNC (#'NAME / (function NAME)) to an out-of-module name is also a
    ;; runtime reference: the FN-ADDR opcode reads function-info-bytecode-offset,
    ;; so registering the name at a runtime stub offset lets the interp's
    ;; op-FN-ADDR map that offset back to the name and load the REAL native
    ;; function OBJECT.  The subsequent CALL-INDIRECT then bridge-calls it —
    ;; this is the higher-order eval2 path (funcall/apply/mapcar #'NAME).
    (dolist (e all-ir)
      (dolist (insn (cdr e))
        ;; The fn NAME is operand 1 for :call ((:call name nargs)) but operand 2
        ;; for :li-func ((:li-func dest name)) — pick the right slot per op.
        (let ((name (cond ((eq (car insn) :call) (cadr insn))
                          ((eq (car insn) :li-func) (caddr insn))
                          (t nil))))
          (when (and name (stringp name) (not (gethash name *functions*)))
            (let ((info (make-function-info :name name :bytecode-offset rt-next
                                            :bytecode-length 0)))
              (setf (gethash name *functions*) info)
              (setf (gethash rt-next rt-table) name)
              (setq rt-next (+ rt-next 1)))))))
    ;; Pass 2: emit (CALLs resolve to in-module OR synthetic offsets via *functions*).
    (dolist (e all-ir)
      (let* ((ir (cdr e)) (lp (compute-label-positions ir)))
        (emit-bytecode-for-ir buf ir lp)))
    (dolist (e all-ir)
      (when (string-equal (string (function-info-name (car e))) "%EVAL2-THUNK")
        (setq entry (function-info-bytecode-offset (car e)))))
    (let ((bc (mvm-buffer-used-bytes buf)))
      (if entry
          (let ((fn-table (make-array (length all-ir))) (i 0)
                ;; LAMBDA-OFFSETS: the bytecode entry offsets of in-module LAMBDA /
                ;; CLOSURE bodies (named *$$LAMBDA* / *$$CLOSURE* by compile-lambda).
                ;; The interp's native-bridge uses this to recognise an eval2 lambda
                ;; VALUE escaping to a native higher-order fn (mapcar/reduce/…) and
                ;; wrap it in a re-entrant trampoline.  Keyed by offset; ONLY genuine
                ;; lambda bodies are recorded (never the %eval2-thunk / helper defuns
                ;; / the fn at offset 0), so an ordinary fixnum DATA argument — a loop
                ;; counter 0/1/2, an index — is never mistaken for a callable.
                (lam-offsets (make-hash-table)))
            (dolist (e all-ir)
              (aset fn-table i (function-info-bytecode-offset (car e)))
              (setq i (+ i 1))
              (let ((nm (string (function-info-name (car e))))
                    (off (function-info-bytecode-offset (car e))))
                ;; NEVER record offset 0: at the native bridge a DATA fixnum 0
                ;; argument (make-list 0 / member 0 / (- 10 j)=0 in nsubstitute's
                ;; bounds loops) is indistinguishable from a lambda-at-offset-0,
                ;; and wrapping it into a #x52 trampoline corrupted the callee
                ;; (make-list "non-negative fixnum", remove returned input
                ;; unchanged).  %mvm-lambda-offset-p has the matching read-side
                ;; guard; data-0 priority is correct since the first module
                ;; function is always a non-lambda (defun / %EVAL2-THUNK).
                (if (and (or (search "$$LAMBDA" nm) (search "$$CLOSURE" nm))
                         (not (eql off 0)))
                    (puthash off lam-offsets t)
                    ;; NON-lambda module fns (defuns, flet bodies, the thunk)
                    ;; record under the distinct :DEFUN marker: the #x52
                    ;; branches (module-closure vs native-closure
                    ;; discrimination via %mvm-module-fn-offset-p in
                    ;; %mvm-wrap-escaping[-result] and op-obj-subtag) accept
                    ;; any entry INCLUDING offset 0 (the first module fn — a
                    ;; materialized #'SELF closure's slot-0 is very often 0),
                    ;; which is safe because slot-0 of a #x52 is never DATA.
                    ;; The BARE-INTEGER wrap branch keeps the stricter
                    ;; %mvm-lambda-offset-p (value T + never 0), so a data
                    ;; fixnum equal to a defun offset still crosses the
                    ;; bridge unwrapped (make-list-0/member-0 protection).
                    (puthash off lam-offsets (quote :defun)))))
            ;; WS3 def persistence: install each top-level user DEFUN as a
            ;; re-entrant interp trampoline in BOTH global function tables —
            ;; *symbol-function-table* by NAME (the eval2 native-call bridge's
            ;; %mvm-resolve-runtime-fn key) and *native-sym-function-table* by
            ;; HASH (symbol-function / funcall key).  The trampoline closes over
            ;; BC so the module bytecode stays GC-alive; fn-table + lam-offsets
            ;; are fully built by now; env = NIL (a top-level defun captures
            ;; nothing).  A later (eval2 …) call OR the tree-walker now resolves f.
            (when persist-names
              (dolist (e all-ir)
                (let ((pn (string (function-info-name (car e)))))
                  (when (member pn persist-names :test (function string=))
                    (let ((tramp (%mvm-make-trampoline
                                   bc fn-table rt-table
                                   (function-info-bytecode-offset (car e))
                                   nil lam-offsets)))
                      ;; puthash signature is (KEY HT VALUE) — store the
                      ;; trampoline as the VALUE under PN / its name-hash.  (The
                      ;; earlier `(puthash pn tramp <table>)` had HT and VALUE
                      ;; swapped, so the closure was treated as the hash table and
                      ;; nothing was actually stored — every later resolve of PF
                      ;; returned NIL, so the trampoline never ran.)
                      (when (boundp (quote *symbol-function-table*))
                        (puthash pn *symbol-function-table* tramp))
                      (when (boundp (quote *native-sym-function-table*))
                        (puthash (compute-name-hash pn)
                                 *native-sym-function-table* tramp)))))))
            ;; PERF Round 2: cache the compiled module for these forms so a
            ;; later eval2 of the same forms skips the whole compile.  bc is a
            ;; fresh copy (mvm-buffer-used-bytes), safe despite buffer reuse.
            (when %cacheable
              (setf (gethash forms *eval2-cache*)
                    (list bc entry fn-table rt-table lam-offsets)))
            ;; Conditions PROPAGATE (see %eval2-run-tuple): production EVAL
            ;; must let an error signalled by the form reach the caller's
            ;; handler-case instead of returning (:interp-err e) as a value.
            ;; The RESULT is wrapped when it is an in-module #x52 lambda
            ;; closure (see %mvm-wrap-escaping-result) so `(eval '#'(lambda
            ;; ...))` hands back a natively-funcallable, per-call-distinct
            ;; function object.
            ;; MULTIPLE VALUES propagate — same *mvm-last-mv* + values-list
            ;; re-emission as %eval2-run-tuple (see the comment there).
            (let* ((%prim (%mvm-wrap-escaping-result
                            (mvm-interpret bc :entry-point entry :function-table fn-table
                                           :runtime-table rt-table :return-raw nil
                                           :lambda-offsets lam-offsets)
                            bc fn-table rt-table lam-offsets))
                   (%mv *mvm-last-mv*))
              (if %mv
                  (if (eql (car %mv) 0)
                      (values)
                      (values-list (cons %prim (cdr %mv))))
                  %prim)))
          :no-entry))))
  )
;; Single-expression convenience.
(defun eval2 (form) (eval2-forms (list form)))
