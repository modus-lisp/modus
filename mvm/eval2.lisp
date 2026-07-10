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

(defvar *eval2-no-cache* nil
  "When T, eval2-forms bypasses *eval2-cache* entirely (neither hit nor
   store) for the current call.  Set (setq + unwind-protect restore) by
   %e2ic-eval2-nocache: the interp-closure entry compiles forms that embed
   CAPTURED ENV CONSES via the quote pool — two DIFFERENT closures can have
   EQUAL forms (same params/body, structurally-equal but non-eq env pairs),
   and an EQUAL-keyed cache hit would hand closure B a module whose const
   pool holds closure A's cells (shared state, the two-counters bug).")

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
          ;; EXCEPTIONS: DEFCLASS / DEFINE-CONDITION / DEFSTRUCT are bootstrap
          ;; macros whose expansions DO contain toplevel defuns (slot
          ;; accessor/reader/writer fns, struct constructors/accessors).
          ;; Gated out, a single-form (eval '(defclass X … :accessor A-X))
          ;; compiled A-X only in-module (the form is the %eval2-thunk BODY,
          ;; so the compiler-recorded path never fires either) and every
          ;; LATER eval got UNDEFINED-FUNCTION A-X — asdf's COMPONENT-CHILDREN
          ;; / COMPONENT-OPERATION-TIMES (mark-component-preloaded, gauntlet
          ;; form 241).  Let those three fall through to the expansion branch.
          ((and *macro-table*
                (not (string-equal hn "DEFCLASS"))
                (not (string-equal hn "DEFINE-CONDITION"))
                (not (string-equal hn "DEFSTRUCT"))
                (gethash (normalize-name (car f)) *macro-table*))
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
  (let ((%cacheable (and (not *eval2-no-cache*) (%eval2-cacheable-p forms))))
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

;;; ============================================================
;;; WS3 Phase 3 — eval2 lambda-body-against-env entry (%e2ic)
;;; ============================================================
;;;
;;; The tree-walker (%eval-in-env) could not be deleted because it was the
;;; only engine that could evaluate a LAMBDA BODY AGAINST A CAPTURED ENV —
;;; the %interp-closure call path (compile nil '(lambda …), coerce 'function,
;;; runtime define-compiler-macro expanders, walker-created closures) and
;;; DEFTYPE expansion (%expand-deftype).  This block gives eval2 that entry:
;;;
;;;   %e2ic-compile PARAMS BODY ENV → native trampoline (or NIL = fallback)
;;;
;;; Design: compile `(lambda PARAMS (symbol-macrolet SM . BODY))` via eval2,
;;; where SM maps each captured env binding NAME to `(cdr 'PAIR)` — PAIR
;;; being the walker's OWN alist binding cons, passed by identity through
;;; the eval2 quote pool (*e2-const-pool*).  Reads see the live cell value;
;;; `(setq NAME v)` expands (compile-setq symbol-macro path) to
;;; `(set-cdr 'PAIR v)` — mutating the SAME cons the walker and any sibling
;;; closures share, so mutation semantics match the walker exactly.
;;; eval2 returns the lambda as a re-entrant native trampoline
;;; (%mvm-wrap-escaping-result → %mvm-make-trampoline), cached per closure
;;; (5th slot of the %interp-closure list) so repeated calls pay only the
;;; ~0.6M-cycle interpret, not the ~16M-cycle compile.
;;;
;;; WS3 STEP 4 (tree-walker DELETED from production images): there is no
;;; walker fallback any more.  A shape outside eval2's lambda support —
;;;   - junk lambda list (NIL / non-symbol atom element, unknown &-marker;
;;;     MACRO lambda lists — dotted tails, nested destructuring, &whole/
;;;     &environment/&body — are handled since the WS3 finisher via
;;;     compile-lambda's %transform-macro-lambda-list),
;;;   - a captured binding whose name is unresolvable,
;;;   - eval2 COMPILE failure (conditions during the body's execution are NOT
;;;     caught — they propagate, matching production eval semantics)
;;; — now signals an honest ERROR instead of silently degrading to a second
;;; evaluator.  The legacy fork builds get the walker from tree-walker.lisp
;;; (which overrides %call-interp-closure/%expand-deftype there).
;;; (The nested-lambda-over-captured-cells fallback is GONE: compile-lambda
;;; now threads symbol-macro bindings into nested compilation units —
;;; %collect-free-vars compiles SM names via their expansions instead of
;;; snapshot-capturing them — so an inner lambda over a captured name reads
;;; and setqs the SAME live env cons the walker and sibling closures share.
;;; The env-held-interp-closure fallback is ALSO gone: FLET/LABELS locals in
;;; a captured env compile to flet wrappers + SM entries — see
;;; %e2ic-env-pairs / %e2ic-flet-bindings.)
;;;
(defvar *e2ic-deftype-cache* nil
  "name-string → (entry . trampoline-or-:walker) for %expand-deftype's eval2
   route.  The registered (params . body) ENTRY cons is stored alongside so a
   re-registered deftype (entry no longer eq) recompiles.")

(defun %e2ic-eval2-nocache (form)
  "eval2 FORM with *eval2-cache* bypassed (see *eval2-no-cache*): the form
   embeds captured env conses via the quote pool, so EQUAL-keyed caching
   would alias two different closures' cells.  setq + unwind-protect (not a
   let of the special — compiled let of a special is unreliable in-image)."
  (let ((%saved *eval2-no-cache*))
    (setq *eval2-no-cache* t)
    (unwind-protect
        (eval2 form)
      (setq *eval2-no-cache* %saved))))

(defun %e2ic-ll-marker (x)
  "If X is a lambda-list &-marker symbol, return its NAME string; else NIL."
  (let ((nm (%eval-sym-name x)))
    (if (and nm (> (length nm) 0) (= (%prim-aref nm 0) 38))  ; 38 = &
        nm
        nil)))

(defun %e2ic-simple-ll-p (params)
  "T when PARAMS is a lambda list the eval2 compile path handles.  Since
   the WS3 finisher (stage 2), MACRO-style lambda lists are ACCEPTED too —
   dotted tails, nested destructuring in the required section, and
   &whole/&environment/&body — because compile-lambda rewrites them to a
   plain (&rest …) + DESTRUCTURING-BIND form (%transform-macro-lambda-list,
   stage 1).  This is what lets walker-created macro expanders (runtime
   DEFMACRO / MACROLET / DEFINE-COMPILER-MACRO closures) and destructuring
   DEFTYPE lambda lists compile+run via %e2ic instead of the tree-walker.
   NIL (walker fallback) only for junk: a NIL element, a non-symbol
   non-cons atom in a binding position, or an unknown &-marker.
   (&OPTIONAL+&KEY combos accepted since 2026-07-08 — preprocess-params'
   &key→&rest transform handles the combination.)"
  (let ((ps params))
    (loop
      (cond
        ((null ps) (return t))
        ((not (consp ps)) (return (symbolp ps))) ; dotted tail var
        (t
         (let ((p (car ps)))
           (cond
             ((null p) (return nil))
             ((symbolp p)
              (let ((mk (%e2ic-ll-marker p)))
                (when mk
                  (unless (or (string-equal mk "&OPTIONAL")
                              (string-equal mk "&REST")
                              (string-equal mk "&KEY")
                              (string-equal mk "&AUX")
                              (string-equal mk "&ALLOW-OTHER-KEYS")
                              (string-equal mk "&WHOLE")
                              (string-equal mk "&ENVIRONMENT")
                              (string-equal mk "&BODY"))
                    (return nil)))))             ; unknown &-marker
             ((consp p) nil)   ; nested pattern / spec — compile side handles
             (t (return nil)))
           (setq ps (cdr ps))))))))

(defun %e2ic-ll-var-names-1 (params out)
  "Extend OUT with the NAME string of every variable PARAMS binds — FULL
   macro-lambda-list grammar: nested required patterns (recursive), dotted
   tail vars, &whole/&environment vars, spec vars and supplied-p vars.
   Section-aware: a cons in the REQUIRED section is a sub-pattern to
   recurse into; the same cons after an &-marker is a (var init
   [supplied-p]) / ((:kw var) …) spec.  Precision matters both ways: a
   MISSED name leaves a symbol-macrolet env entry colliding with the
   real parameter binding; an EXTRA name silently drops a live captured
   cell (body reads an unbound global instead)."
  (let ((ps params)
        (in-required t))
    (loop
      (cond
        ((null ps) (return out))
        ((not (consp ps))                        ; dotted tail var
         (let ((nm (%eval-sym-name ps)))
           (when nm (setq out (cons nm out))))
         (return out))
        (t
         (let ((p (car ps)))
           (cond
             ((and (symbolp p) (%e2ic-ll-marker p))
              (let ((mk (%e2ic-ll-marker p)))
                (if (or (string-equal mk "&WHOLE")
                        (string-equal mk "&ENVIRONMENT"))
                    ;; The NEXT element is the bound var — consume it here.
                    (progn
                      (setq ps (cdr ps))
                      (when (and (consp ps) (symbolp (car ps)))
                        (let ((nm (%eval-sym-name (car ps))))
                          (when nm (setq out (cons nm out))))))
                    (setq in-required nil))))
             ((symbolp p)
              (let ((nm (%eval-sym-name p)))
                (when nm (setq out (cons nm out)))))
             ((consp p)
              (if in-required
                  (setq out (%e2ic-ll-var-names-1 p out))
                  (progn
                    (let ((v (car p)))
                      (let ((var (if (consp v) (car (cdr v)) v)))
                        (let ((nm (%eval-sym-name var)))
                          (when nm (setq out (cons nm out))))))
                    (when (and (cdr p) (cdr (cdr p))
                               (symbolp (car (cdr (cdr p)))))
                      (let ((snm (%eval-sym-name (car (cdr (cdr p))))))
                        (when snm (setq out (cons snm out)))))))))
           (setq ps (cdr ps))))))))

(defun %e2ic-ll-var-names (params)
  "NAME strings of every variable a lambda list binds (params, nested
   pattern vars, dotted tails, &whole/&environment vars, spec vars,
   supplied-p vars) — used to drop captured-env bindings a parameter
   shadows."
  (reverse (%e2ic-ll-var-names-1 params nil)))

(defun %e2ic-env-pairs (env params)
  "Captured-env binding conses to expose in the compiled unit: dedup by NAME
   keeping the FIRST (innermost) occurrence, drop names a lambda param
   shadows.  Returns (VALUE-PAIRS . FN-PAIRS): FN-PAIRS are bindings whose
   value is an %interp-closure (what FLET/LABELS store in the walker's env —
   it conflates the namespaces, guarding call-position lookups by the
   interp-closure shape; see cl-eval's %eval-funcall).  FN-PAIRS get BOTH a
   symbol-macrolet entry (value refs, matching the walker's unguarded
   variable lookup) AND an flet wrapper (call position + #'NAME).  Returns
   the marker symbol :E2IC-BAD when a binding's name is unresolvable."
  (let ((pnames (%e2ic-ll-var-names params))
        (seen nil)
        (vout nil)
        (fout nil)
        (bad nil))
    (dolist (pair env)
      (when (and (not bad) (consp pair))
        (let ((nm (%eval-sym-name (car pair))))
          (cond
            ((null nm) (setq bad t))
            ((member nm seen :test (function string-equal)) nil)
            ((member nm pnames :test (function string-equal))
             (setq seen (cons nm seen)))
            ((%interp-closure-p (cdr pair))
             (setq seen (cons nm seen))
             (setq fout (cons pair fout)))
            (t
             (setq seen (cons nm seen))
             (setq vout (cons pair vout)))))))
    (if bad (quote :e2ic-bad) (cons (reverse vout) (reverse fout)))))

(defun %e2ic-sm-bindings (pairs)
  "symbol-macrolet bindings: NAME → (cdr 'PAIR), PAIR by identity via the
   eval2 quote pool."
  (let ((out nil))
    (dolist (pair pairs (reverse out))
      (setq out (cons (list (car pair)
                            (list (quote cdr) (list (quote quote) pair)))
                      out)))))

(defun %e2ic-flet-bindings (pairs)
  "flet bindings exposing captured %interp-closure env bindings as LOCAL
   FUNCTIONS: (NAME (&rest A) (apply (cdr \'PAIR) A)).  The (cdr \'PAIR)
   read happens at CALL time, so a setq of the same name (via its
   symbol-macrolet entry) redirects subsequent local calls too — exactly
   the walker\'s live-env behavior.  APPLY dispatches interp-closures
   through %call-interp-closure (cl-printer apply), so the callee gets its
   own %e2ic compile+cache on first call; recursion (LABELS) terminates
   because each level is one cached wrapper hop."
  (let ((out nil))
    (dolist (pair pairs (reverse out))
      (let ((av (gensym "%E2ICA")))
        ;; Tail shape (values-list (multiple-value-list …)): APPLY's
        ;; interp-closure fast path preserves the callee's multiple values,
        ;; but a bare (apply …) tail is not values-shaped, so the compiled
        ;; wrapper's epilogue would set-mv-count 1 and truncate them (probe
        ;; F7).  VALUES-LIST at the tail makes tail-form-is-values-p skip
        ;; that epilogue — MV parity with the walker holds through the
        ;; wrapper.
        (setq out (cons (list (car pair)
                              (list (quote &rest) av)
                              (list (quote values-list)
                                    (list (quote multiple-value-list)
                                          (list (quote apply)
                                                (list (quote cdr)
                                                      (list (quote quote) pair))
                                                av))))
                        out))))))

(defun %e2ic-compile (params body env)
  "Compile an interp-closure\'s PARAMS/BODY/ENV to a native re-entrant
   trampoline via eval2, or NIL when the shape needs the walker.  Only the
   COMPILE step is guarded by handler-case — the body does not run here, so
   falling back cannot double side effects.  Captured env: value bindings
   become symbol-macrolet entries over the live env conses; interp-closure
   bindings (flet/labels locals) get BOTH an SM entry and an flet wrapper
   (see %e2ic-env-pairs / %e2ic-flet-bindings)."
  (if (not (%e2ic-simple-ll-p params))
      nil
      (let ((pairs (%e2ic-env-pairs env params)))
        (cond
          ((eq pairs (quote :e2ic-bad)) nil)
          (t
           (let* ((vpairs (car pairs))
                  (fpairs (cdr pairs))
                  (inner (if fpairs
                             (list (cons (quote flet)
                                         (cons (%e2ic-flet-bindings fpairs)
                                               body)))
                             body))
                  (form (if (or vpairs fpairs)
                            (list (quote lambda) params
                                  (cons (quote symbol-macrolet)
                                        (cons (%e2ic-sm-bindings
                                                (append vpairs fpairs))
                                              inner)))
                            (cons (quote lambda) (cons params body)))))
             (handler-case
                 (let ((tramp (%e2ic-eval2-nocache form)))
                   (if (functionp tramp) tramp nil))
               (t (c) nil))))))))

(defun %e2ic-cached (fn)
  "The per-closure compile cache: slot 5 of the %interp-closure list
   (NIL = untried, :E2IC-WALKER = known-fallback, else the trampoline).
   Walker readers touch only slots 1-3, so the extension is invisible."
  (let ((tail (cdr (cdr (cdr (cdr fn))))))
    (if (consp tail) (car tail) nil)))

(defun %e2ic-cache-set (fn val)
  (let ((tail (cdr (cdr (cdr fn)))))
    (when (consp tail)
      (set-cdr tail (cons val nil))))
  val)

(defun %e2ic-apply (tramp args)
  "Apply the compiled trampoline and re-emit the interpret run's multiple
   values (same *mvm-last-mv* protocol as %eval2-run-tuple) so MV parity
   with production eval2 holds through the closure boundary."
  (let* ((%r (apply tramp args))
         (%mv *mvm-last-mv*))
    (if %mv
        (if (eql (car %mv) 0)
            (values)
            (values-list (cons %r (cdr %mv))))
        %r)))

(defvar *e2ic-fallback-count* 0
  "Number of interp-closure / deftype-expander invocations %e2ic-compile
   could NOT serve.  Historical: counted tree-walker fallbacks until the
   walker was DELETED (the deletion census measured ZERO hits across the
   full ANSI corpus + gauntlet); now counts SIGNALED unsupported-shape
   errors — any nonzero value marks a fresh eval2 capability gap.
   BOOTS AS NIL, not 0: defvar init-thunks never run in the ANSI image
   (CLAUDE.md Active Limitation 7) — increment ONLY via %e2ic-bump-fallback.")

(defun %e2ic-bump-fallback ()
  "NIL-safe increment of *e2ic-fallback-count*.  The defvar's `0' init-form
   never runs (init-all-globals is skipped in the ANSI image), so the
   counter boots as NIL.  A bare (+ 1 NIL) tag-checks into GENERIC-ADD,
   whose (t (%fixnum-+ a b)) clause silently returns NIL+2 = #xDEAD0003;
   further bumps walk the garbage to #xDEAD0009 — OBJECT tag 9 — after
   which the first obj-subtag predicate (bignump etc.) dereferences the
   unmapped #xDEAD0000 header → SIGSEGV inside the walker-fallback /
   deftype-expansion path (i.e. mid-macroexpansion or mid-typep during
   handler-case condition matching), which leaks signal-dispatch state and
   let conditions escape the per-form handler-case → the CHUNK-CRASH
   regression on the fallback families (format-b/d/o/x, macrolet, some,
   notany, times, defmacro, handler-case…)."
  (setq *e2ic-fallback-count*
        (if *e2ic-fallback-count* (+ 1 *e2ic-fallback-count*) 1)))

(defun %call-interp-closure (fn args)
  "The ONLY interp-closure engine (the tree-walker is DELETED): compiles
   the closure body against its captured env ONCE (cached on the closure),
   applies the trampoline.  An unsupported shape / compile failure SIGNALS
   an honest error (cached as :e2ic-fail; *e2ic-fallback-count* bumps as
   the diagnostic — the deletion census measured zero such shapes)."
  (let ((c (%e2ic-cached fn)))
    (cond
      ((eq c (quote :e2ic-fail))
       (%e2ic-bump-fallback)
       (error "eval2: interp-closure shape unsupported (cached compile failure)"))
      (c (%e2ic-apply c args))
      (t
       (let ((tramp (%e2ic-compile (cadr fn) (caddr fn) (cadddr fn))))
         (%e2ic-cache-set fn (if tramp tramp (quote :e2ic-fail)))
         (if tramp
             (%e2ic-apply tramp args)
             (progn
               (%e2ic-bump-fallback)
               (error "eval2: interp-closure compile failed (params=~S)"
                      (cadr fn)))))))))

(defun %expand-deftype (type)
  "OVERRIDE (eval2 images; last-defun-wins) of ansi-bridge's engine stub:
   route the deftype body eval through the eval2 lambda-body entry, cached
   per registration (name → (entry . trampoline)).  A deftype body eval2
   can't compile SIGNALS (the tree-walker is deleted)."
  (let* ((head (if (consp type) (car type) type))
         (args (if (consp type) (cdr type) nil))
         (entry (%deftype-lookup head)))
    (cond
      ((null entry) nil)
      (t
       (let* ((nm (%eval-sym-name head))
              (hit (if (and nm *e2ic-deftype-cache*)
                       (gethash nm *e2ic-deftype-cache*)
                       nil)))
         (if (and hit (eq (car hit) entry))
             (if (eq (cdr hit) (quote :e2ic-fail))
                 (progn (%e2ic-bump-fallback)
                        (error "eval2: deftype expander compile failed (type=~S)"
                               type))
                 (%e2ic-apply (cdr hit) args))
             (let ((tramp (%e2ic-compile (car entry) (cdr entry) nil)))
               (unless *e2ic-deftype-cache*
                 (setq *e2ic-deftype-cache*
                       (make-hash-table :test (quote equal))))
               (when nm
                 (puthash nm *e2ic-deftype-cache*
                          (cons entry (if tramp tramp (quote :e2ic-fail)))))
               (if tramp
                   (%e2ic-apply tramp args)
                   (progn (%e2ic-bump-fallback)
                          (error "eval2: deftype expander compile failed (type=~S)"
                                 type))))))))))
