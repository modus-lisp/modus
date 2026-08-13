;;;; mvm-eval.lisp — in-image runtime evaluator: compile a list of top-level
;;;; forms to ONE MVM bytecode module and execute the trailing expression via
;;;; mvm-interpret.  Extracted verbatim from build-generic.lisp's
;;;; *stage2-test-source* so the same source feeds BOTH the generic oracle
;;;; binary and the ANSI image (WS3: self-host the compiler + retire the
;;;; tree-walker).  Depends on mvm.lisp (ISA), interp.lisp (mvm-interpret),
;;;; and compiler.lisp (mvm-compile-toplevel / emit pipeline) being loaded
;;;; earlier in the image source.
;; mvm-eval-forms: compile a LIST of top-level forms (helper defuns followed by a
;; trailing expression) into ONE bytecode module and run the expression.  CALLs
;; between the functions resolve in-module (bytecode->bytecode) so NO native
;; bridge / value marshalling is needed — one representation throughout.  This
;; is the 'drop native' model: the interpreter runs everything as bytecode.
(defvar *mvm-eval-buffer* nil
  "PERF: persistent 64KB bytecode buffer reused across mvm-eval-forms calls (see
   the reuse site) instead of (make-array 65536) every call — mvm-buffer-used-
   bytes copies the bytecode out before the next call, so nothing retains it.")

(defvar *mvm-eval-cache* nil
  "PERF Round 2 (compile-caching): EQUAL hash, FORMS → (bc entry fn-table
   rt-table lam-offsets) — the compiled MVM module for those forms.  mvm-eval of a
   tiny form is ~26x compile-bound; the harness/asdf re-eval the same forms
   constantly, so caching the compiled bytecode skips the whole ~15M-cycle
   compile and pays only ~0.6M interpret.  Re-interpreting the cached bytecode
   is correct: the RESULT depends on live runtime state, not the cache.  bc is a
   COPY (mvm-buffer-used-bytes) so it's safe despite *mvm-eval-buffer* reuse.")

(defvar *e2-active-defun-names* nil
  "Names (strings) of the top-level DEFUNs of the mvm-eval module CURRENTLY
   being interpreted.  Def persistence installs those defuns' trampolines
   at module-BUILD time — BEFORE the module's code runs — so a runtime
   (fmakunbound 'f) executing before the (defun f …) in the same module
   (uiop's defun* expands `(defun* (f) …)` to exactly
   `(progn (fmakunbound 'f) (defun f …))`) would UNDO the installation
   and leave F undefined (the asdf resolve-location /
   process-source-registry-directive silent-death class).  fmakunbound
   consults this list and SKIPS removal for names the running module is
   (re)defining, honoring source order.  Set (lexical-save + setq-restore)
   around the module run in mvm-eval-forms; a condition escaping the run can
   leave it stale, which self-heals at the next mvm-eval-forms call and at
   worst makes an unrelated fmakunbound of one of these names a no-op.")

;;; ============================================================
;;; WS4 STAGE 5b — runtime JIT seam (bytecode → native → execute)
;;; ============================================================
;;;
;;; *use-jit* (default NIL): when T, mvm-eval-forms / %mvm-eval-run-tuple execute a
;;; compiled module by TRANSLATING its bytecode to native x64, mmap'ing an
;;; exec page, relocating out-of-module CALLs + patching const-pool immediates
;;; (the Stage 1-4 mechanism), and %jit-call'ing the entry — instead of
;;; mvm-interpret.  Set at boot (build-time default baked from MODUS_USE_JIT).
;;; ANY failure in the JIT path falls back to mvm-interpret, so correctness is
;;; never worse than JIT-off (unsupported forms — strings/length/MV — just take
;;; the interpret path).  When *use-jit* is NIL the seam is inert: mvm-eval is
;;; byte-for-byte behavior-identical to before.
(defvar *use-jit* nil
  "WS4-S5b: T = execute compiled mvm-eval modules as native JIT'd code (with
   interpret fallback on any error); NIL = pure mvm-interpret (unchanged).
   NB: the seam gates on the FUNCTION %jit-enabled-p (below), not on this
   special directly — a global-variable setq in kernel-main did NOT reliably
   propagate to mvm-eval's compiled read in the ANSI image (the value stayed NIL),
   whereas a defun resolves through the proven SFT (name-hash) path.  This
   special is kept for documentation / an alternate runtime override; the
   default %jit-enabled-p returns its value if bound, else NIL.")

;;; WS5 #203 — THE RE-EXECUTION GUARD.
;;;
;;; The interpret fallback re-runs the WHOLE FORM.  That is only SAFE while no
;;; user code has run yet; once control has entered JIT'd native code, the
;;; form's side effects have already happened, and re-running duplicates them.
;;; The old `(t (c) ...interpret...)` handler could not tell the two apart, so
;;; an ordinary error signalled by the user's own code replayed every side
;;; effect that preceded it (measured: pre=2 post=1 for a 3-setq progn).
;;;
;;; These two flags let the handler decide.  Both default to NIL, which is also
;;; what an uninitialised defvar reads as (compiler limitation #7), so no
;;; explicit init is required — but they MUST be saved/restored around each
;;; attempt rather than dynamically rebound, because JIT'd native code can call
;;; back into mvm-eval (a macroexpander running during a later form's compile),
;;; and because an escaping condition would otherwise leak the set value the
;;; same way *handler-bind-stack* used to (see 279f2cc).
(defvar *jit-native-ran* nil
  "T once control has been transferred INTO JIT'd native code for the current
   %mvm-eval-jit-run attempt.  While NIL, an escaping condition can only have
   come from JIT setup (translate / page build / relocation) — no user code has
   executed, so falling back to mvm-interpret is safe and invisible.  Once T,
   an escaping condition is the user's own and MUST be re-signalled, never
   answered by re-running the form.")

(defvar *jit-infra-fallback* nil
  "T when %mvm-eval-jit-run has DELIBERATELY signalled an internal sentinel
   asking for the interpret fallback even though native code already ran — i.e.
   knowingly double-executing the form's side effects.

   WS5 #218 CLOSED THE MAIN CASE.  This used to fire for EVERY form that left
   MV-count > 1 — which is not just `(values …)` but floor / truncate /
   ceiling / round / gethash / subtypep / read-from-string /
   multiple-value-bind / -list / -call / -prog1, i.e. a large fraction of
   ordinary code.  %mvm-eval-jit-run now READS the native MV block back out of
   BSS (tagged count at #x10000090, the count-1 extras at #x10000098+) and
   publishes it as *mvm-last-mv* in the exact shape mvm-interpret produces, so
   the seam returns the values directly and the form runs exactly ONCE.

   The only remaining trigger is a count outside [0,21] — beyond the 20-slot
   MV-VALUES area — which cannot be read back without handing the collector
   words that are not MV storage.  *jit-mv-fallback-count* measures it: it
   reads 0 on every workload measured (tests/jit-diff.lisp, where it read 444
   before this change, and tests/jit-census.lisp, where R-MV went 8 -> 0).")

(defvar *jit-mv-fallback-count* nil
  "How many times the MV path forced a re-run (the remaining double-execute).
   Post-#218 this counts ONLY the out-of-range residual described in
   *jit-infra-fallback*; 0 is the expected reading.")

(defvar *jit-resignal-count* nil
  "How many times an escaping USER condition was correctly re-signalled instead
   of being answered by re-running the form.  Non-zero here is the guard doing
   exactly its job; before the fix every one of these was a duplicated form.")

(defvar *jit-target-arch* :x64
  "WS4-S5 (aarch64): which native back-end the runtime-JIT seam drives.
   :x64 (default) → %jit-translate-page-1 (translate-mvm-to-x64, contiguous
   imm64 relocation).  :aarch64 → %jit-translate-page-1-aarch64
   (translate-mvm-to-aarch64, MOVZ/MOVK-quad relocation).  Set by the aarch64
   image's boot init; the x64 image leaves it :x64 so the x64 JIT path is
   behavior-identical to pre-Stage-5 (the aarch64 branch is dead code there —
   its translate-mvm-to-aarch64 / a64-* references resolve to unused sentinels,
   symmetric to the x64 image's existing dead translate-mvm-to-x64 ref).")

(defun %jit-enabled-p ()
  "WS4-S5b JIT gate.  The build OVERRIDES this defun (build-x64-linux.lisp,
   baked from MODUS_USE_JIT) to return T or NIL — last-defun-wins.  This base
   version honors *use-jit* if it is set (an alternate override point), else
   NIL (safe default: pure interpret)."
  (and (boundp (quote *use-jit*)) *use-jit*))

(defvar *jit-inhibit* nil
  "CLASS-3 dynamic JIT inhibit.  When non-nil, %jit-active-p forces the mvm-eval
   seam onto the INTERPRET path regardless of %jit-enabled-p.  Needed because
   the ANSI gate BAKES %jit-enabled-p to a CONSTANT (build-x64-linux.lisp,
   MODUS_USE_JIT) that ignores *use-jit* — so a plain `(setq *use-jit* nil)`
   around the interp-closure trampoline compile (see %e2ic-mvm-eval-nocache) does
   NOT disable the JIT there in the gate image, and the native<->interp
   nested-call boundary fault (define-compiler-macro.1/.2/.7) resurfaces.
   %e2ic-mvm-eval-nocache sets THIS flag (lexical save + setq-restore) so the
   inhibit works uniformly in BOTH the CLI (%jit-enabled-p reads *use-jit*)
   and the gate (baked constant).  BOOTS AS NIL (defvar init-thunks are
   skipped in the ANSI image); a bare NIL read is exactly the wanted default
   (JIT not inhibited).")

(defun %jit-active-p ()
  "The mvm-eval seam gate: JIT is live only when the build enabled it AND it is
   not dynamically inhibited (see *jit-inhibit*)."
  (and (%jit-enabled-p)
       (not (and (boundp (quote *jit-inhibit*)) *jit-inhibit*))))

(defvar *jit-native-count* 0
  "WS5 diag: number of mvm-eval forms that completed via the NATIVE JIT path.")
(defvar *jit-fallback-count* 0
  "WS5 diag: number of mvm-eval forms that FELL BACK to mvm-interpret.")

;;; ------------------------------------------------------------------
;;; JIT FALLBACK CENSUS (WS4 differential coverage)
;;; ------------------------------------------------------------------
;;; *jit-fallback-count* answers "how often", never "why".  Every fallback in
;;; the seam now ALSO bumps exactly one reason counter below, so native% can be
;;; decomposed into a ranked list of blockers — the roadmap to JIT-only.
;;;
;;; The reasons are mutually exclusive and together they sum to
;;; *jit-fallback-count* (modulo the double-counting the pre-existing
;;; %jit-translate-page guard does: it bumps *jit-fallback-count* itself AND
;;; the caller's je=NIL branch bumps it again; R-TRANSLATE-ERR records the
;;; guard hit, R-PAGE-NIL the je=NIL branch, so the census reports both and the
;;; report subtracts).
;;;
;;;   R-TRANSLATE-ERR   translate-mvm-to-<arch> SIGNALLED — a translator gap.
;;;                     *jit-translate-err-count* (pre-existing) is this.
;;;   R-RELOC-CALL-UNRESOLVED   an out-of-module CALL names a function that
;;;                     %mvm-resolve-runtime-fn cannot find at all.
;;;   R-RELOC-CALL-NONNATIVE    the callee resolved but is a HEAP closure
;;;                     (tag 9, a runtime DEFUN) — no PROT_EXEC, so the
;;;                     #206 guard refuses the patch.  THE structural blocker
;;;                     for JIT-only: it fires for any form calling code that
;;;                     was itself defined at runtime.
;;;   R-RELOC-FNADDR-FAIL       a #'NAME value load could not be resolved.
;;;   R-MMAP-FAIL       %mmap-exec-page returned a non-address.
;;;   R-CONST-BAKED     a const-pool heap address would be baked into a
;;;                     function that OUTLIVES the module (a persisted DEFUN,
;;;                     a lambda body, an flet body) — where the next
;;;                     collection makes it stale and nothing re-bakes it.
;;;                     See the GC-SAFETY GATE in %jit-translate-page-1.
;;;   R-MV              the form left an MV count OUTSIDE [0,21] — past the
;;;                     20-slot MV-VALUES area — so the native block cannot be
;;;                     read back and the form is re-interpreted.
;;;                     (*jit-mv-fallback-count*.)  Since WS5 #218 an ordinary
;;;                     multiple-value form is NOT a fallback at all: the block
;;;                     is reproduced from BSS into *mvm-last-mv*.
;;;   R-NATIVE-ESCAPE   native code RAN and escaped with a non-condition
;;;                     (the unresolved-runtime-function sentinel).  This is
;;;                     the one reason that DOUBLE-EXECUTES side effects.
;;;   R-PAGE-NIL        the page build returned NIL (the union of the RELOC/
;;;                     MMAP reasons above, counted at the seam).
(defvar *jit-r-reloc-call-unresolved* nil)
(defvar *jit-r-reloc-call-nonnative* nil)
(defvar *jit-r-reloc-fnaddr-fail* nil)
(defvar *jit-r-mmap-fail* nil)
(defvar *jit-r-page-nil* nil)
(defvar *jit-r-native-escape* nil)
(defvar *jit-r-const-baked* nil
  "R-CONST-BAKED: the module's native code would contain BAKED const-pool heap
   addresses (%jit-patch-consts movabs sites), which the next collection makes
   stale with no way to re-bake.  See the GC-SAFETY GATE in
   %jit-translate-page-1 — such a module is INTERPRETED, so every quoted
   constant is read live out of *e2-const-pool* by op-LI-CONST.")

(defvar *jit-census-on* nil
  "When T the census ALSO records the NAMES/messages behind each blocked
   relocation and translator gap (bounded lists below), not just the tallies.
   Off by default so a production JIT run pays only fixnum increments.")

(defvar *jit-blocked-callees* nil
  "Census detail: bounded alist (NAME . COUNT) of out-of-module CALL targets
   that failed relocation.  Ranked, this IS the JIT-only roadmap.")
(defvar *jit-blocked-fnaddrs* nil
  "Census detail: bounded alist (NAME . COUNT) for failed #'NAME relocations.")
(defvar *jit-translate-err-msgs* nil
  "Census detail: bounded alist (MESSAGE . COUNT) of translator-gap errors.")

(defun %jit-census-note (name place)
  "Bump NAME's count in the bounded alist held in the census PLACE list, and
   return the (possibly extended) list.  Callers setq the special from this."
  (let ((l place) (hit nil))
    (loop
      (when (null l) (return nil))
      (when (and (stringp (car (car l))) (stringp name)
                 (string= (car (car l)) name))
        (setq hit (car l))
        (return nil))
      (setq l (cdr l)))
    (cond (hit (rplacd hit (+ 1 (cdr hit))) place)
          ((>= (length place) 96) place)          ; bounded — never unbounded growth
          (t (cons (cons name 1) place)))))

(defun %jit-err-tag (c)
  "A short, groupable STRING for condition C — its simple-condition format
   control when it has one, else a generic marker.  Never signals."
  (handler-case
      (let ((fc (simple-condition-format-control c))
            (fa (handler-case (simple-condition-format-arguments c)
                  (t (c3) nil))))
        ;; The translator's own errors are all raised as
        ;;   (error "~A (fn ~D '~A' mvm-pos ~D opcode ~D operands ~S)" msg ...)
        ;; so the CONTROL string is the same for every gap and the first
        ;; ARGUMENT is the actual message.  Group on control+first-arg.
        (if (stringp fc)
            (if (and (consp fa) (stringp (car fa)))
                (concatenate 'string (car fa) " | " fc)
                fc)
            "<condition-no-string-control>"))
    (t (c2) "<condition-unreportable>")))

(defvar *jit-page-cache* nil
  "WS4-S5b per-module JIT cache.  EQ hash keyed by the compiled BC byte-array
   identity → a jit-entry list (base eoff cpatches gc-stamp).  A repeated mvm-eval
   of the SAME cached module reuses the already-translated exec page instead of
   re-translating (that's the speedup: translate cost amortizes over re-evals).
   After a GC the const-pool objects MOVE (the pool is a GC root the collector
   updates); each cached page's baked const immediates go stale, so we compare
   gc-stamp to (%gc-count) and RE-PATCH from the updated pool before re-exec.")

(defvar *mvm-eval-no-cache* nil
  "When T, mvm-eval-forms bypasses *mvm-eval-cache* entirely (neither hit nor
   store) for the current call.  Set (setq + unwind-protect restore) by
   %e2ic-mvm-eval-nocache: the interp-closure entry compiles forms that embed
   CAPTURED ENV CONSES via the quote pool — two DIFFERENT closures can have
   EQUAL forms (same params/body, structurally-equal but non-eq env pairs),
   and an EQUAL-keyed cache hit would hand closure B a module whose const
   pool holds closure A's cells (shared state, the two-counters bug).")

(defun %mvm-eval-cacheable-p (forms)
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
          (return-from %mvm-eval-cacheable-p nil))))))

(defun %e2-scan-persist (f acc depth)
  "WS3 def persistence pre-scan (see persist-names in mvm-eval-forms): return ACC
   extended with the names (strings) of every toplevel-context DEFUN in form F.
   CLHS 3.2.3.1: forms in a top-level EVAL-WHEN / PROGN / LOCALLY body are
   THEMSELVES top level — recurse those.  A top-level MACRO call's expansion is
   also processed as top level, so a head that is none of the above is expanded
   ONE step (macroexpand-1-mvm sees both the compiler *macro-table* and, under
   *mvm-eval-runtime-p*, the runtime macro tables) and re-inspected — this is what
   lets the scan see uiop's (with-upgradability () (defun featurep ...)) →
   eval-when → defun* → progn → defun chain.  The old raw-heads-only scan
   missed every macro-wrapped defun: single-form (eval FORM) compiles FORM as
   the %mvm-eval-thunk BODY (compile-form, never mvm-compile-toplevel), so the
   compiler-recorded *e2-persist-defuns* path never fired either, and the
   block's own (detect-os) funcall-by-symbol died with CALL-IND non-callable
   (asdf gauntlet FAILFORM 43/45).  DEPTH guards runaway expansion; expansion
   errors are treated as no-expansion (the real compile will report them)."
  (if (or (> depth 40) (not (and (consp f) (symbolp (car f)))))
      acc
      (let ((hn (symbol-name (car f))))
        (cond
          ((and (string-equal hn "DEFUN") (symbolp (cadr f)))
           ;; Key the persist name by the SAME string the compiler uses for
           ;; function-info-name (%rt-fn-name): package-qualified PKG::NAME
           ;; for a runtime-born library's symbols, bare otherwise.  If this
           ;; stayed bare while the compiler qualified, the install loop's
           ;; (member pn persist-names) below would miss and the trampoline
           ;; would only get installed via the compiler-recorded path.
           (cons (%rt-fn-name (cadr f)) acc))
          ((string-equal hn "EVAL-WHEN")
           (dolist (sub (cddr f) acc)
             (setq acc (%e2-scan-persist sub acc 0))))
          ((or (string-equal hn "PROGN") (string-equal hn "LOCALLY"))
           (dolist (sub (cdr f) acc)
             (setq acc (%e2-scan-persist sub acc 0))))
          ;; PERF GATE: only a RUNTIME user macro (with-upgradability et al,
          ;; resolved via macroexpand-1-mvm's *mvm-eval-runtime-p* fallback) can
          ;; hide a toplevel defun the raw heads miss.  Bootstrap/compile-time
          ;; macros in *macro-table* (loop, handler-case, dolist, …) never
          ;; expand to toplevel defuns — skip them so the pre-scan doesn't
          ;; double-run their expanders on every mvm-eval call.
          ;; EXCEPTIONS: DEFCLASS / DEFINE-CONDITION / DEFSTRUCT are bootstrap
          ;; macros whose expansions DO contain toplevel defuns (slot
          ;; accessor/reader/writer fns, struct constructors/accessors).
          ;; Gated out, a single-form (eval '(defclass X … :accessor A-X))
          ;; compiled A-X only in-module (the form is the %mvm-eval-thunk BODY,
          ;; so the compiler-recorded path never fires either) and every
          ;; LATER eval got UNDEFINED-FUNCTION A-X — asdf's COMPONENT-CHILDREN
          ;; / COMPONENT-OPERATION-TIMES (mark-component-preloaded, gauntlet
          ;; form 241).  Let those three fall through to the expansion branch.
          ((and *macro-table*
                (not (string-equal hn "DEFCLASS"))
                (not (string-equal hn "DEFINE-CONDITION"))
                (not (string-equal hn "DEFSTRUCT"))
                (%macro-expander (car f) (normalize-name (car f))))
           acc)
          (t
           (let ((ex (handler-case (macroexpand-1-mvm f)
                       (t (c) (cons f nil)))))
             (if (cdr ex)
                 (%e2-scan-persist (car ex) acc (+ depth 1))
                 acc)))))))

;;; --- WS4-S5b JIT machinery (small factored helpers) ---
;;;
;;; Kept deliberately SMALL / non-inlined into kernel-main or mvm-eval-forms
;;; (the boot-crash/layout class — a big body baked past a branch-displacement
;;; boundary SIGSEGVs at boot).  Each does one step; the seam calls %mvm-eval-jit-run.

(defun %jit-write-imm64 (base off word)
  "Write the 64-bit WORD little-endian into the exec page at BASE+OFF (the
   10-byte movabs's imm64 slot)."
  (let ((a (+ base off)) (v word) (j 0))
    (loop while (< j 8)
          do (progn
               (setf (mem-ref (+ a j) :u8) (logand v 255))
               (setq v (ash v -8))
               (setq j (+ j 1))))))

(defun %jit-write-movz-quad (base off word)
  "WS4-S5 (aarch64): patch a MOVZ/MOVK quad (4 consecutive 32-bit words) at
   BASE+OFF with the 4 imm16 halves of WORD.  Register-agnostic: reads each
   placeholder word and REPLACES its imm16 field (bits 5..20) with (half << 5),
   preserving the placeholder's Rd + move-wide opcode base — so a li-const/
   fn-addr site targeting ANY Xd (not just x16) patches correctly.  The aarch64
   analogue of %jit-write-imm64.

   RE-RUNNABLE (WS5 aa64 const-staleness fix): the imm16 field is CLEARED
   (#xFFE0001F keeps every bit except 5..20) before the half is OR'd in.  The
   old code OR'd into a field it assumed was still the 0 placeholder, which is
   true on the FIRST bake but silently corrupts a re-bake — and a re-bake is
   exactly what %jit-entry-for must do after a collection moves the const-pool
   objects.  On a virgin placeholder the mask is a no-op, so first-bake output
   is byte-identical to before."
  (let ((k 0))
    (loop
      (when (>= k 4) (return nil))
      (let* ((wo (+ off (* k 4)))
             (w (logior (mem-ref (+ base wo) :u8)
                        (ash (mem-ref (+ base (+ wo 1)) :u8) 8)
                        (ash (mem-ref (+ base (+ wo 2)) :u8) 16)
                        (ash (mem-ref (+ base (+ wo 3)) :u8) 24)))
             (imm (logand (ash word (- (* k 16))) #xFFFF))
             (nw (logior (logand w #xFFE0001F) (ash imm 5))))
        (setf (mem-ref (+ base wo) :u8) (logand nw 255))
        (setf (mem-ref (+ base (+ wo 1)) :u8) (logand (ash nw -8) 255))
        (setf (mem-ref (+ base (+ wo 2)) :u8) (logand (ash nw -16) 255))
        (setf (mem-ref (+ base (+ wo 3)) :u8) (logand (ash nw -24) 255)))
      (setq k (+ k 1)))))

(defun %jit-patch-consts (base cpatches)
  "Bake each live const-pool object's tagged native word into its movabs imm64.
   CPATCHES = list of (imm64-native-off . pool-idx).  Re-runnable after GC."
  (dolist (p cpatches)
    (let ((obj (if *e2-const-pool* (gethash (cdr p) *e2-const-pool*) nil)))
      (%jit-write-imm64 base (car p) (%val->word obj)))))

(defun %jit-patch-consts-aarch64 (base cpatches)
  "aarch64 sibling of %jit-patch-consts: bake each live const-pool object's
   tagged word into its MOVZ/MOVK quad.  CPATCHES = list of (movz-byte-off .
   pool-idx).  Re-runnable after GC (see %jit-write-movz-quad's imm16 clear).
   The caller must flush the I-cache over the patched range."
  (dolist (p cpatches)
    (let ((obj (if *e2-const-pool* (gethash (cdr p) *e2-const-pool*) nil)))
      (%jit-write-movz-quad base (car p) (%val->word obj)))))

(defun %jit-reloc-calls (base relocs rt-table)
  "Patch each out-of-module CALL movabs with the resolved native callee address.
   Returns T if every reloc resolved, NIL if any failed (→ caller falls back)."
  (let ((ok t))
    (dolist (r relocs)
      (let* ((name (gethash (cdr r) rt-table))
             (fn (and name (%mvm-resolve-runtime-fn name)))
             ;; %val->word FN = the callee's tagged native word (entry|3; the
             ;; LEA+OR-3 tag from mvm-fn-addr — entry is 16-aligned so low nibble
             ;; is 3).  The JIT's `movabs rax, imm64; call rax` calls the imm
             ;; DIRECTLY (verified: no runtime shift of the imm), so imm must be
             ;; the UNTAGGED entry = word - 3.  The old `(- (ash word -1) 3)`
             ;; HALVED the entry (it assumed %val->word left-shifts the native
             ;; word; it does NOT — the VALUE it returns already IS the native
             ;; word).  A halved entry is either below the load base (unmapped)
             ;; or lands mid-instruction — and for EVAL it landed on an INT3
             ;; (0xCC) byte, so `(eval …)` SIGTRAP'd whenever its (mapped) halved
             ;; target got called.
             ;; WS5 #206: the callee must be NATIVE code.  A function defined at
             ;; RUNTIME (a defun evaluated by an earlier top-level form) is not
             ;; — symbol-function holds a HEAP closure object, and the heap is
             ;; mapped PROT_READ|WRITE with no PROT_EXEC.  Patching its address
             ;; into `movabs rax, imm64; call rax` jumps into non-executable
             ;; memory: SEGV_ACCERR, caught by the boot signal handler, surfaced
             ;; as an escape MID-EXECUTION, and answered by re-running the whole
             ;; form — which is the observed side-effect doubling (task #203).
             ;; Same class as the DGMC GF-array bug that SUB-3'd into the heap.
             ;;
             ;; The low nibble already separates them and cannot collide (see
             ;; CLAUDE.md: cons=1, fn=3, char=5, obj=9 are disjoint, and
             ;; mvm-fn-addr's LEA+OR-3 makes a native fn word ALWAYS end in 3).
             ;;   (symbol-function 'car) -> 15165315        low nibble 3, in-image
             ;;   a runtime (defun kk …)  -> 129844928268201 low nibble 9, on-heap
             ;; So: only patch a tag-3 word.  Anything else fails the reloc,
             ;; which fails the page build, which makes %mvm-eval-jit-run take
             ;; its je=NIL branch and interpret the form ONCE — the correct
             ;; value with no duplicated side effects.  Losing the JIT for forms
             ;; that call runtime-defined functions is the price until a
             ;; late-bound bridge exists; a correct interpret beats a wrong
             ;; native jump.
             (word (if fn (%val->word fn) 0))
             (nativep (eql (logand word 15) 3))
             (raw (if nativep (- word 3) 0)))
        (if (> raw 0)
            (%jit-write-imm64 base (car r) raw)
            (progn
              ;; CENSUS: distinguish "no such runtime function" from "resolved
              ;; but it is a heap closure" — the latter is the #206 guard and
              ;; the dominant structural blocker for JIT-only.
              (if fn
                  (setq *jit-r-reloc-call-nonnative*
                        (if *jit-r-reloc-call-nonnative*
                            (+ 1 *jit-r-reloc-call-nonnative*) 1))
                  (setq *jit-r-reloc-call-unresolved*
                        (if *jit-r-reloc-call-unresolved*
                            (+ 1 *jit-r-reloc-call-unresolved*) 1)))
              (when (and *jit-census-on* name)
                (setq *jit-blocked-callees*
                      (%jit-census-note
                        (if fn (concatenate 'string name " [heap-closure]")
                            (concatenate 'string name " [unresolved]"))
                        *jit-blocked-callees*)))
              (setq ok nil)))))
    ok))

(defun %jit-reloc-fn-addrs (base relocs rt-table)
  "Class 2: patch each out-of-module FN-ADDR movabs with the resolved fn's
   TAGGED native word — the SAME object %mvm-resolve-runtime-fn / symbol-function
   returns.  Unlike a CALL target (which is patched UNTAGGED = word-3 because the
   `call rax` jumps to it directly), a FN-ADDR is a VALUE load: the callable
   object flows into eq / eql / funcall, so the imm must be the full tagged word
   (entry|3).  This makes `(eq #'eq (symbol-function 'eq))` T and
   `%ht-canonicalize-test`'s `(eql v (function %eq-fn))` succeed under JIT.
   Returns T if every reloc resolved, NIL if any failed (→ caller falls back).

   TAG-3 REQUIRED, same rule %jit-reloc-calls has had since #206 — but here for
   a GC reason rather than a PROT_EXEC one.  `#'NAME` of a RUNTIME-defined
   function that kept its interpreter trampoline resolves to a HEAP closure
   (tag 9), and baking a heap address into an immediate is exactly the hazard
   the GC-SAFETY GATE in %jit-translate-page-1 exists to prevent: the collector
   moves the closure and the immediate is never re-patched.  Measured on main —
   a const-free lambda that merely mentions `(function tf)` for a const-bearing
   runtime `tf` returns correctly, then after ONE collection faults hard enough
   to escape its own handler-case (`UNHANDLED-ESCAPE … NIL`).  Const patches and
   these two relocation classes are the ONLY places an immediate is baked, so
   with this guard in place a built page provably holds no heap address at all
   and is GC-immune by construction.  A non-tag-3 target now fails the reloc,
   which fails the page build, which interprets the form once — correct, and
   the trampoline is late-bound there anyway."
  (let ((ok t))
    (dolist (r relocs)
      (let* ((name (gethash (cdr r) rt-table))
             (fn (and name (%mvm-resolve-runtime-fn name)))
             (word0 (if fn (%val->word fn) 0))
             (word (if (eql (logand word0 15) 3) word0 0)))
        (if (> word 0)
            (%jit-write-imm64 base (car r) word)
            (progn
              (setq *jit-r-reloc-fnaddr-fail*
                    (if *jit-r-reloc-fnaddr-fail*
                        (+ 1 *jit-r-reloc-fnaddr-fail*) 1))
              (when (and *jit-census-on* name)
                (setq *jit-blocked-fnaddrs*
                      (%jit-census-note name *jit-blocked-fnaddrs*)))
              (setq ok nil)))))
    ok))

;;; ============================================================
;;; WS5 #222 — NATIVE INSTALLATION OF RUNTIME-DEFINED FUNCTIONS
;;; ============================================================
;;;
;;; The last JIT fallback reason.  `mvm-eval` of `(defun f …)` installed F as
;;; %mvm-make-trampoline — a HEAP CLOSURE (tag 9) that re-enters mvm-interpret.
;;; A later top-level form calling F then failed the #206 native-callee
;;; relocation in %jit-reloc-calls (which requires tag nibble 3) and the WHOLE
;;; calling module interpreted.  That single gap was three blockers at once:
;;;   1. it was the only remaining JIT fallback reason (R-RELOC-CALL-NONNATIVE);
;;;   2. actor-spawn does `(actor-set id #x30 (untag fn))` and JUMPS there, and
;;;      an untagged heap closure is a non-executable heap address, so no
;;;      runtime-defined function could ever be an actor entry;
;;;   3. a --load-able ANSI corpus is entirely runtime DEFUNs, so it would run
;;;      100% interpreted.
;;;
;;; The mechanism: the module the JIT already translates for the top-level form
;;; ALREADY CONTAINS native code for every function the form defines — the same
;;; translate-mvm-to-x64 output, in the same exec page, with the same ABI as a
;;; build-time-native function.  Nothing new needs to be compiled.  All that was
;;; missing was to publish each defun's entry ADDRESS instead of a trampoline:
;;; fn-map gives NAME → native label, label-position gives its byte offset, and
;;; the exec page is 4096-aligned while the in-JIT *x64-native-code-offset* is 0
;;; (build-generic-cli.lisp / build-ansi-common.lisp co-init), so every function
;;; entry is 16-BYTE aligned inside the page and the OR-3 discipline of
;;; translate-x64.lisp (mvm-fn-addr = LEA + OR-3) applies verbatim:
;;; `(logior (+ base off) 3)` is a well-formed tag-3 native function word,
;;; disjoint from cons(1)/char(5)/obj(9).  %word->val reinterprets it as the
;;; function VALUE, which is exactly the object symbol-function returns for a
;;; build-time function — so eq/funcall/symbol-function identity is preserved
;;; and %jit-reloc-fn-addrs keeps patching the full TAGGED word.
;;;
;;; LIFETIME.  The exec page must outlive the compilation unit, and it does: it
;;; is mmap'd memory OUTSIDE the GC heap, and x64 never munmaps (the
;;; %jit-free-page reclamation is gated to *jit-target-arch* :aarch64 and is
;;; further gated on the result not being a function).  It is never scanned or
;;; moved by the collector, and a tag-3 word on the stack is not a pointer tag
;;; so the conservative root scan ignores it.  The cost is that a REDEFINITION
;;; leaks its predecessor's page (a few KB); see %jit-install-native-fns.
;;;
;;; THE CONST-POOL RESTRICTION (the reason this is per-FUNCTION, not per-page).
;;; %jit-patch-consts bakes each const-pool object's CURRENT tagged heap address
;;; into a movabs immediate.  Those objects MOVE under the Cheney collector, and
;;; the existing re-bake (%jit-entry-for, keyed on %gc-count) only fires when a
;;; page is re-entered THROUGH THE SEAM.  A persistently-installed function is
;;; entered directly by native code, so nothing would re-bake it and a GC would
;;; leave it holding a dangling from-space address.  There is no Lisp-visible
;;; post-GC hook to fix this from here (gc_trampoline is emitted assembly and
;;; never calls back into Lisp).  So a function is installed natively only when
;;; NO const patch site falls inside ITS OWN native byte range — which is why
;;; %jit-fn-native-offsets computes per-function ranges rather than gating the
;;; whole page on (null cpatches).  A defun that closes over a quoted literal or
;;; a string keeps its interpreter trampoline; it is correct, just not native.
;;; Lifting that restriction needs an indirection through a GC-updated constant
;;; vector (a translator + collector change) and is deliberately out of scope.

(defun %jit-fn-native-offsets (ft-list fn-map nlen cpatches)
  "Alist (NAME . NATIVE-BYTE-OFFSET) for every function of FT-LIST whose native
   code range in the freshly translated page contains NO const-pool patch site.
   FT-LIST entries are (name mvm-offset length) in EMISSION order, so a
   function's native range ends at the next function's native start (or NLEN for
   the last one — conservatively over-wide, since the GC trampoline and the
   handler-stack helpers are emitted after the last function; over-wide can only
   REJECT a function, never admit a dirty one).

   A name appearing twice in FT-LIST is skipped entirely: fn-map is keyed by
   NAME, so a duplicate resolves to the LAST label and the range attribution
   would be wrong for the earlier one."
  (let ((raw nil))
    (dolist (e ft-list)
      (let* ((nm (car e))
             (dup nil))
        ;; reject duplicate names (fn-map would alias them)
        (let ((seen 0))
          (dolist (e2 ft-list)
            (when (string= (car e2) nm) (setq seen (+ seen 1))))
          (when (> seen 1) (setq dup t)))
        (unless dup
          (let* ((lbl (gethash nm fn-map))
                 (p (if lbl (label-position lbl) nil)))
            (when (and p (integerp p))
              (setq raw (cons (cons nm p) raw)))))))
    ;; For each candidate, END = the smallest start strictly greater than its
    ;; own (scan, not "next in list", so the result does not depend on FT-LIST
    ;; being sorted by native offset).
    (let ((out nil))
      (dolist (c raw)
        (let ((start (cdr c))
              (end nlen)
              (clean t))
          (dolist (o raw)
            (when (and (> (cdr o) start) (< (cdr o) end)) (setq end (cdr o))))
          (dolist (cp cpatches)
            (when (and (>= (car cp) start) (< (car cp) end)) (setq clean nil)))
          (when clean (setq out (cons c out)))))
      out)))

(defun %jit-native-defuns-p ()
  "Gate for native installation of runtime-defined functions.  A FUNCTION, not a
   defvar: defvar init-thunks do not run in-image (CLAUDE.md limitation 7), so a
   `(defvar *x* t)` would read NIL — the same reason %jit-enabled-p is a defun."
  t)

(defvar *jit-native-defun-count* nil
  "DIAGNOSTIC: how many runtime-defined functions have been installed as real
   native code by %jit-install-native-fns.  NIL on a JIT-off image.")

(defun %jit-install-native-fns (base fnoffs names)
  "Publish each function of FNOFFS whose NAME is in NAMES (the top-level DEFUNs
   of this module) as a REAL NATIVE function at BASE+offset, in both global
   function tables — *symbol-function-table* by name (the mvm-eval native-call
   bridge / %mvm-resolve-runtime-fn key, hence %jit-reloc-calls) and
   *native-sym-function-table* by name-hash (symbol-function / funcall).  This
   OVERWRITES the interpreter trampoline mvm-eval-forms installed moments
   earlier; puthash is last-write-wins, matching the last-defun-wins rule.
   Returns the number installed.

   The entry must be 16-BYTE aligned for the OR-3 tag to read back as 3; the
   page is page-aligned and the translator aligns every function entry mod 16
   against *x64-native-code-offset* (0 in JIT mode), so this holds — but it is
   CHECKED, and a misaligned entry is skipped rather than published as a word
   whose low nibble could collide with cons(1)/char(5)/obj(9).

   REDEFINITION.  Publishing a new address does NOT retract addresses already
   baked into previously built exec pages, so a page that baked the OLD native
   address of NM keeps calling it — EARLY BINDING, the same semantics a
   build-time native caller already has (CLAUDE.md active limitation 1,
   last-defun-wins).  The CACHED-module half of that is handled, but NOT here:
   it is handled in mvm-eval-forms' trampoline install loop, which is the only
   place NM's PREVIOUS binding is still visible (this function runs after that
   loop has already overwritten it).  See the WS5 #222 REDEFINITION
   INVALIDATION comment there.  The residual — an already-installed native
   function whose page baked a callee that is later redefined — is a real
   divergence from the interpreter's late binding and is reported, not fixed;
   see GATE-RESULT-jit-defun.md.

   The superseded page is NOT unmapped (a live caller may hold its address), so
   each redefinition leaks its predecessor's page — a few KB per redefinition,
   bounded by redefinition count, not by call count."
  (let ((n 0))
    (dolist (e fnoffs)
      (let ((nm (car e)))
        (when (member nm names :test (function string=))
          (let ((addr (+ base (cdr e))))
            (when (eql (logand addr 15) 0)
              (let ((fn (%word->val (logior addr 3))))
                (when (boundp (quote *symbol-function-table*))
                  (puthash nm *symbol-function-table* fn))
                (when (boundp (quote *native-sym-function-table*))
                  (puthash (compute-name-hash nm) *native-sym-function-table* fn))
                (setq n (+ n 1))))))))
    (setq *jit-native-defun-count*
          (if *jit-native-defun-count* (+ *jit-native-defun-count* n) n))
    n))

(defun %jit-translate-page-1 (bc ft-list rt-table)
  "Inner: translate BC → native x64, mmap an exec page, copy bytes, relocate
   calls + patch consts.  Returns a jit-entry list (base eoff cpatches
   gc-stamp psize fn-native-offsets) or NIL if RELOCATION failed.  MAY signal
   (translator gap) — the guard wrapper %jit-translate-page turns any signal
   into NIL.

   Element 6 (FN-NATIVE-OFFSETS) is the per-function (name . native-offset)
   alist %jit-install-native-fns needs to publish this module's top-level
   DEFUNs as real native functions; see the WS5 #222 block above."
  (setq *x64-jit-mode* t)
  (multiple-value-bind (nbuf fn-map) (translate-mvm-to-x64 bc ft-list)
    ;; ============================================================
    ;; GC-SAFETY GATE (R-CONST-BAKED) — a page that would bake const-pool
    ;; HEAP addresses is not built at all; the module INTERPRETS instead.
    ;; ============================================================
    ;; %jit-patch-consts writes each const-pool object's CURRENT tagged heap
    ;; address into a `movabs` immediate.  Those objects MOVE under the Cheney
    ;; collector, and the only re-bake (%jit-entry-for, keyed on %gc-count)
    ;; fires when a cached page is re-entered THROUGH THE SEAM.  Three ways
    ;; native code with baked consts is reached WITHOUT crossing that seam —
    ;; all three were live wrong-answer bugs on main, all measured:
    ;;
    ;;   1. A CLOSURE built by this page outlives it.  A runtime DEFMACRO's
    ;;      expander (set-macro-function), a lambda stored in a global or in a
    ;;      data structure, a lambda returned as the EVAL result: each is a
    ;;      #x52 closure whose slot-0 points into this exec page.  It is
    ;;      funcalled directly, never through the seam, so nothing re-bakes it.
    ;;        (eval '(defmacro m (a b) (list '+ a b)))  … one GC …
    ;;        (macroexpand-1 '(m 2 3))  =>  (#<?> 2 3)   [garbage head]
    ;;      That is the bug this gate was written for.
    ;;
    ;;   2. A #222-installed native DEFUN reaches dirty code by CALLING it.
    ;;      %jit-fn-native-offsets only checks a function's OWN byte range, but
    ;;      an in-module call is a direct in-page branch, so a const-CLEAN
    ;;      function installed natively can jump straight into a const-DIRTY
    ;;      sibling:
    ;;        (eval '(progn (defun h1 () (list 'aa "bb")) (defun h2 () (h1))))
    ;;        … one GC …   (h1) => (AA "bb")   (h2) => (#<?> #<?>)
    ;;      h1 kept its trampoline and is fine; h2 was published native and is
    ;;      not.  The per-function check cannot see this; a page-level gate can.
    ;;
    ;;   3. A collection fires DURING this page's own execution.  The entry
    ;;      re-bake happens at entry; a top-level form that allocates past the
    ;;      threshold has every remaining const go stale mid-flight:
    ;;        (eval '(progn <allocate until gc_count moves> (list 'ee "ff")))
    ;;        => (#<?> #<?>)
    ;;      No persistence and no closure involved — an ordinary form, silently
    ;;      wrong.  Nothing short of a page-level gate covers this one.
    ;;
    ;;   4. `#'RUNTIME-DEFUN` as a VALUE.  %jit-reloc-fn-addrs baked whatever
    ;;      word the name resolved to, and a runtime DEFUN that kept its
    ;;      interpreter trampoline is a tag-9 HEAP closure.  A const-free lambda
    ;;      that merely mentions `(function tf)` therefore held a heap address
    ;;      too, and after one collection faulted hard enough to escape its own
    ;;      handler-case.  Closed in %jit-reloc-fn-addrs itself (tag-3 required,
    ;;      matching %jit-reloc-calls) rather than here, because the right answer
    ;;      is to fail that reloc, not to inspect ranges.
    ;;
    ;; Rejecting the page is CORRECT-BY-CONSTRUCTION, in the same sense as the
    ;; #206 tag-3 callee guard: with no const patch sites the page contains NO
    ;; baked heap address at all.  The only other places an immediate is baked
    ;; are the two relocation classes (%jit-reloc-calls / %jit-reloc-fn-addrs),
    ;; and BOTH now require a tag-3 native word — build-time image code or a
    ;; `%mmap-exec-page` page, neither of which the collector ever moves.  So
    ;; `cpatches = NIL` implies GC-immune, with no post-GC hook needed anywhere.
    ;;
    ;; The interpreted module is immune for the reason the DEFUN trampolines
    ;; already were: mvm-interpret's op-LI-CONST does
    ;; `(gethash idx *e2-const-pool*)` at EXECUTION time, so it reads whatever
    ;; the collector has updated the pool to hold.  Measured directly — inside
    ;; ONE lambda body after a collection, an explicit `(gethash idx
    ;; *e2-const-pool*)` returned the live object while the quoted literal
    ;; beside it returned the stale one.
    ;;
    ;; SCOPE — why the gate is "outside the THUNK", not "any const at all".
    ;; The blanket rule (reject on ANY cpatch) is sound but unaffordable: the
    ;; top-level thunk of `(defun f …)` carries the NAME 'F as a const, so
    ;; EVERY runtime defun form would be rejected and #222 would never install
    ;; anything native again — a runtime DEFUN's body would go back to being
    ;; interpreted.  Measured, not assumed: with the blanket rule the class
    ;; probes' own `(dotimes (j 100000) (make-list 40))` allocation loop went
    ;; from seconds to not finishing in two minutes.
    ;;
    ;; The thunk is the ONE function in the module that is entered only through
    ;; the seam, exactly once, with %jit-entry-for's re-bake in front of it.
    ;; Every OTHER function in the page — top-level defuns, lambda/closure
    ;; bodies, flet/labels bodies — either persists past the module (installed,
    ;; captured in a closure, stored) or is the in-page callee of something that
    ;; does.  So: a const patch site inside ANY non-thunk function rejects the
    ;; page; const sites confined to the thunk are allowed to bake.
    ;;
    ;; RESIDUAL, stated rather than hidden: hazard 3 above survives for the
    ;; THUNK'S OWN consts — a single top-level form holding a quoted literal
    ;; that itself allocates past the collection threshold still reads a stale
    ;; address after that mid-flight GC.  Closing that too needs the const pool
    ;; reached through a GC-UPDATED INDIRECTION (task #226's constant vector),
    ;; after which this whole gate can be deleted.  *jit-r-const-baked* counts
    ;; how often the gate fires.
    (let ((%cpatches *x64-li-const-patches*))
      (when %cpatches
        (let* ((%tlbl (gethash "%MVM-EVAL-THUNK" fn-map))
               (%tstart (if %tlbl (label-position %tlbl) nil)))
          (if (not (integerp %tstart))
              ;; No identifiable thunk range → cannot prove any const site is
              ;; seam-guarded.  Reject.
              (progn
                (setq *jit-r-const-baked*
                      (if *jit-r-const-baked* (+ 1 *jit-r-const-baked*) 1))
                (return-from %jit-translate-page-1 nil))
              ;; THUNK END = the smallest OTHER function start strictly greater
              ;; than %TSTART, else the whole buffer.  Scanned (not "next in
              ;; ft-list") so the answer does not depend on ft-list ordering.
              ;; When the thunk is emitted LAST the range runs to the end of the
              ;; buffer, which over-covers the translator's trailing GC/handler
              ;; helpers — those are hand-emitted asm and carry no :li-const, so
              ;; nothing dirty can hide there.
              (let ((%tend (code-buffer-position nbuf)))
                (dolist (%e ft-list)
                  (let* ((%l (gethash (car %e) fn-map))
                         (%p (if %l (label-position %l) nil)))
                    (when (and (integerp %p) (> %p %tstart) (< %p %tend))
                      (setq %tend %p))))
                (dolist (%cp %cpatches)
                  (when (or (< (car %cp) %tstart) (>= (car %cp) %tend))
                    (setq *jit-r-const-baked*
                          (if *jit-r-const-baked* (+ 1 *jit-r-const-baked*) 1))
                    (return-from %jit-translate-page-1 nil))))))))
    (let* ((nlen (code-buffer-position nbuf))
           (nbytes (code-buffer-bytes nbuf))
           (relocs *x64-call-relocs*)
           (fa-relocs *x64-fn-addr-relocs*)
           (cpatches *x64-li-const-patches*)
           (elabel (gethash "%MVM-EVAL-THUNK" fn-map))
           (eoff (if elabel (label-position elabel) 0))
           ;; JIT-FIX (Fix 2): the exec page must be big enough for the ACTUAL
           ;; native code length.  A fixed 4096-byte page overran on every
           ;; flet/labels/CLOS-dispatch module (their native code is 1k-15k+
           ;; bytes) — the copy loop below wrote past the mapped region → a
           ;; SIGSEGV the mvm-eval seam caught as a (nil-condition) fallback, so
           ;; those whole shape classes ran interpret-only (native%≈0).  Round
           ;; nlen UP to a 4096-byte multiple (+1 page of slack for the copy's
           ;; tail / alignment) so the full native module fits.
           (npages (+ 1 (ash nlen -12)))
           (psize (ash npages 12))
           (page (%mmap-exec-page psize))
           (base (sap-address (make-sap page))))
      ;; SAFE-FLIP GUARD (x64 parity with the aarch64 sibling): mmap failure
      ;; returns -errno, a small/negative integer.  Writing to it would fault;
      ;; return NIL so the seam interprets this form.
      (when (< base 4096)
        (setq *jit-r-mmap-fail*
              (if *jit-r-mmap-fail* (+ 1 *jit-r-mmap-fail*) 1))
        (return-from %jit-translate-page-1 nil))
      ;; Copy native bytes into the exec page.
      (let ((k 0))
        (loop while (< k nlen)
              do (progn
                   (setf (mem-ref (+ base k) :u8) (aref nbytes k))
                   (setq k (+ k 1)))))
      (if (and (%jit-reloc-calls base relocs rt-table)
               (%jit-reloc-fn-addrs base fa-relocs rt-table))
          (progn
            (%jit-patch-consts base cpatches)
            ;; 5th = PSIZE (read only on the aarch64 reclaim path; harmless
            ;; here).  6th = the per-function native-offset alist for WS5 #222
            ;; native DEFUN installation.
            (list base eoff cpatches (%gc-count) psize
                  (%jit-fn-native-offsets ft-list fn-map nlen cpatches)))
          nil))))

(defun %jit-translate-page-1-aarch64 (bc mvm-entry ft-list rt-table)
  "WS4-S5 (aarch64) sibling of %jit-translate-page-1.  translate-mvm-to-aarch64
   wants an eql-keyed func-idx→MVM-offset HASH (not x64's (name offset length)
   list), so build it from FT-LIST.  Translate BC under *aarch64-jit-mode*, mmap
   an exec page, copy the native words, relocate out-of-module CALLs (untagged
   callee addr = word-3) + #'NAME fn-addrs (tagged word) + patch li-const quads
   (pool object tagged word), flush the I-cache, and return a jit-entry
   (base eoff cpatches gc-stamp psize).
   Returns NIL if any reloc failed to resolve (→ interpret fallback).  MAY signal
   a translator gap — the %jit-translate-page guard turns it into NIL.

   WS5 #203 CORRECTION (this was THE aarch64 library-macro bug).  The old
   docstring here claimed \"cpatches is NIL: GC is off on aarch64-linux so the
   const-pool never moves and %jit-entry-for's post-GC re-bake never fires\".
   GC is NOT off on aarch64-linux — it collects exactly like x64, just sooner.
   So every JIT'd aarch64 page baked const-pool HEAP addresses that went
   permanently stale at the first collection, with (a) no GC-safety gate and
   (b) cpatches = NIL, which made %jit-entry-for's re-bake a no-op.  A runtime
   DEFMACRO's expander is a closure living in such a page, so after one GC:
       (eval '(defmacro m (a b) (list '+ a b)))  … one GC …
       (macroexpand-1 '(m 2 3))  =>  (#<?> 2 3)
   — the whole \"library FUNCTIONS work but library MACROS fail\" class
   (alexandria/bordeaux-threads/iterate/cl-utilities on the aarch64 ladder).
   x64 closed this with the GC-SAFETY GATE in %jit-translate-page-1; that gate
   is now ported here verbatim in intent, and cpatches is carried so the
   thunk's own consts are re-baked on a post-GC cache hit."
  (setq *aarch64-jit-mode* t)
  ;; WS5 aarch64 JIT: function entries must be 16-BYTE aligned inside the exec
  ;; page.  A closure's fn-addr slot is tagged with +tag-function+ (3) and
  ;; +op-call-ind+ validates `addr & 15 == 3` before SUB-3/BLR, so the raw
  ;; entry address needs low nibble 0.  The exec page from %mmap-exec-page is
  ;; page (4096) aligned, so the constraint reduces to native-offset mod 16 = 0
  ;; — which is what translate-mvm-to-aarch64's fn-entry NOP pad computes when
  ;; *aarch64-fn-align-offset* is 0.  In-image that global has no value (defvar
  ;; init-thunks don't run — CLAUDE.md item 7) and the pad loop degenerated to
  ;; zero padding, so entries landed on arbitrary 4-byte boundaries.  Set it
  ;; explicitly here (JIT-only; the host image builds set their own value
  ;; host-side and never call this function).
  (setq *aarch64-fn-align-offset* 0)
  (let ((ftbl (make-hash-table :test (quote eql))))
    (let ((i 0)) (dolist (e ft-list) (setf (gethash i ftbl) (cadr e)) (setq i (+ i 1))))
    (multiple-value-bind (nbuf fn-map) (translate-mvm-to-aarch64 bc ftbl)
      ;; ============================================================
      ;; GC-SAFETY GATE (R-CONST-BAKED) — aarch64 port of the x64 gate in
      ;; %jit-translate-page-1.  See that function's long comment for the four
      ;; hazard shapes; the reasoning is arch-independent, only the encoding
      ;; (MOVZ/MOVK quad vs movabs imm64) and the offset units differ.
      ;;
      ;; A const patch site inside ANY function OTHER than the top-level thunk
      ;; means a heap address is baked into code that OUTLIVES the seam (a
      ;; lambda body / closure — e.g. a runtime DEFMACRO expander — or an
      ;; in-page callee of one).  Nothing re-bakes those, so reject the page
      ;; and let the module INTERPRET: op-LI-CONST reads *e2-const-pool* at
      ;; execution time, which the collector keeps live.  Const sites confined
      ;; to the thunk are allowed — the thunk is entered only through the seam,
      ;; behind %jit-entry-for's post-GC re-bake.
      ;;
      ;; Offsets are BYTE offsets on both sides: *aarch64-li-const-patches*
      ;; records (movz-byte-pos . pool-idx) and FN-MAP maps an MVM function
      ;; offset to a native BYTE offset (same units %jit-write-movz-quad and
      ;; the lrel patch loop below already use).
      (let ((%cpat *aarch64-li-const-patches*))
        (when %cpat
          (let ((%tstart (gethash mvm-entry fn-map)))
            (if (not (integerp %tstart))
                ;; No identifiable thunk range → cannot prove any const site is
                ;; seam-guarded.  Reject.
                (progn
                  (setq *jit-r-const-baked*
                        (if *jit-r-const-baked* (+ 1 *jit-r-const-baked*) 1))
                  (return-from %jit-translate-page-1-aarch64 nil))
                ;; THUNK END = the smallest OTHER function start strictly
                ;; greater than %TSTART, else the end of the buffer.  Scanned
                ;; (not "next in ft-list") so the answer does not depend on
                ;; ft-list ordering.
                (let ((%tend (* (a64-buffer-position nbuf) 4)))
                  (dolist (%e ft-list)
                    (let ((%p (gethash (cadr %e) fn-map)))
                      (when (and (integerp %p) (> %p %tstart) (< %p %tend))
                        (setq %tend %p))))
                  (dolist (%cp %cpat)
                    (when (or (< (car %cp) %tstart) (>= (car %cp) %tend))
                      (setq *jit-r-const-baked*
                            (if *jit-r-const-baked* (+ 1 *jit-r-const-baked*) 1))
                      (return-from %jit-translate-page-1-aarch64 nil))))))))
      (let* ((nwords (a64-buffer-position nbuf))
             (code (a64-buffer-code nbuf))
             (nlen (* nwords 4))
             (crel *aarch64-call-relocs*)
             (frel *aarch64-fn-addr-relocs*)
             (lrel *aarch64-fn-addr-local-relocs*)
             (cpat *aarch64-li-const-patches*)
             (eoff (gethash mvm-entry fn-map))
             (npages (+ 1 (ash nlen -12)))
             (psize (ash npages 12))
             (base (%mmap-exec-page psize))
             (k 0) (ok t))
        ;; SAFE-FLIP GUARD: MAP_FAILED (mmap returns -errno = a NEGATIVE/tiny Lisp
        ;; integer, vs a huge-positive address) → DON'T write/call a bad page;
        ;; return NIL so the seam falls back to mvm-interpret for this form.
        (when (< base 4096)
          (return-from %jit-translate-page-1-aarch64 nil))
        (loop
          (when (>= k nwords) (return nil))
          (let ((w (aref code k)) (o (* k 4)))
            (setf (mem-ref (+ base o) :u8) (logand w 255))
            (setf (mem-ref (+ base (+ o 1)) :u8) (logand (ash w -8) 255))
            (setf (mem-ref (+ base (+ o 2)) :u8) (logand (ash w -16) 255))
            (setf (mem-ref (+ base (+ o 3)) :u8) (logand (ash w -24) 255)))
          (setq k (+ k 1)))
        ;; Out-of-module CALL relocations (untagged callee addr = word-3).
        ;; WS5 #206: the callee must carry the FN tag — see %jit-reloc-calls for
        ;; the full account.  A RUNTIME-defined function (a defun evaluated by an
        ;; earlier top-level form) is a HEAP CLOSURE, tag 9, and the heap has no
        ;; PROT_EXEC; branching to it faults mid-execution and the fallback then
        ;; re-runs the form, duplicating its side effects.  The tag test is
        ;; arch-independent (cons=1/fn=3/char=5/obj=9 are disjoint everywhere),
        ;; so aarch64 needs exactly the same guard as x64 — this is the same
        ;; defect, not a port of an x64-specific workaround.
        (dolist (r crel)
          (let* ((name (gethash (cdr r) rt-table))
                 (fn (and name (%mvm-resolve-runtime-fn name)))
                 (word (if fn (%val->word fn) 0))
                 (addr (if (eql (logand word 15) 3) (- word 3) 0)))
            (if (> addr 0) (%jit-write-movz-quad base (car r) addr) (setq ok nil))))
        ;; Out-of-module #'NAME fn-addr relocations (full TAGGED fn word).
        (dolist (r frel)
          (let* ((name (gethash (cdr r) rt-table))
                 (fn (and name (%mvm-resolve-runtime-fn name)))
                 (word (if fn (%val->word fn) 0)))
            (if (> word 0) (%jit-write-movz-quad base (car r) word) (setq ok nil))))
        ;; WS5: IN-MODULE fn-addr relocations.  These are the sites that build a
        ;; CLOSURE (make-closure stores the lambda's fn-addr in slot 0) and any
        ;; #'LOCAL-FN value-load.  The whole-image path defers them to
        ;; apply-aarch64-fn-addr-patches, which the JIT never runs, so before
        ;; this loop every JIT-built closure carried fn-addr 0 and funcall of it
        ;; took +op-call-ind+'s bad-callable trap → PROGRAM-ERROR with the body
        ;; never entered.  Patch value = (base + fn-native-offset) | +tag-function+.
        ;; If the entry is not 16-aligned the tag nibble would not read back as
        ;; 3 and call-ind would reject it, so fail the reloc instead of shipping
        ;; a closure that traps — the seam then interprets this form (correct,
        ;; just not native).
        (dolist (r lrel)
          (let* ((noff (gethash (cdr r) fn-map))
                 (addr (if noff (+ base noff) 0)))
            (if (and noff (eql (logand addr 15) 0))
                (%jit-write-movz-quad base (car r) (logior addr 3))
                (setq ok nil))))
        ;; Quoted-literal / string li-const patches (pool object tagged word).
        ;; Post-gate these are THUNK-ONLY sites, so %jit-entry-for's post-GC
        ;; re-bake (which runs before the seam re-enters the thunk) covers them.
        (%jit-patch-consts-aarch64 base cpat)
        (%jit-icache-flush base nlen)
        (if (and ok eoff)
            ;; 3rd element = CPATCHES, so %jit-entry-for can re-bake after a
            ;; collection moved the pool objects (was NIL, which silently
            ;; disabled the re-bake — see the docstring).
            ;; 5th element = PSIZE, so a transient form's page can be munmap'd
            ;; (%jit-free-page base psize) after its native call — see
            ;; %mvm-eval-jit-run's reclamation.
            (list base eoff cpat (%gc-count) psize)
            nil)))))

(defvar *jit-translate-err-count* 0
  "DIAGNOSTIC: count of translate-time errors caught by %jit-translate-page's
   guard (distinct from MV/reloc/page-unavailable fallbacks).")
(defvar *jit-last-translate-err* nil
  "DIAGNOSTIC: the last condition caught by %jit-translate-page's guard.")

(defun %jit-translate-page (bc mvm-entry ft-list rt-table)
  "FLIP-SAFETY GUARD (Fix 1): translate + build a JIT exec page for BC, but
   turn ANY error signalled while translating/relocating (e.g. a translator
   gap such as `Unknown register: 0` in a flet/labels/CLOS-dispatch shape)
   into a clean NIL return, so the caller falls back to mvm-interpret and the
   program's RESULT is never changed.  Every path from a top-level/load eval
   to translate-mvm-to-x64 runs through this function (both %mvm-eval-jit-run
   call shapes call %jit-entry-for or %jit-translate-page), so a translator
   gap can NEVER escape as an uncaught error nor produce a wrong value — it
   only costs one interpret fallback.  NB: the outer seam handler-case in
   %mvm-eval-run-tuple / mvm-eval-forms is kept as belt-and-suspenders; this inner
   guard exists because a translate error must degrade to NIL (interpret)
   even in any context whose outer catch might not see it."
  (handler-case
      (if (eq *jit-target-arch* :aarch64)
          (%jit-translate-page-1-aarch64 bc mvm-entry ft-list rt-table)
          (%jit-translate-page-1 bc ft-list rt-table))
    (t (c)
       (setq *jit-translate-err-count*
             (if (boundp (quote *jit-translate-err-count*))
                 (+ 1 *jit-translate-err-count*) 1))
       (setq *jit-last-translate-err* c)
       (when *jit-census-on*
         (setq *jit-translate-err-msgs*
               (%jit-census-note (%jit-err-tag c) *jit-translate-err-msgs*)))
       (setq *jit-fallback-count*
             (if *jit-fallback-count* (+ 1 *jit-fallback-count*) 1))
       nil)))

(defun %jit-entry-for (bc mvm-entry ft-list rt-table)
  "Return a ready-to-call jit-entry (base eoff cpatches gc-stamp) for BC, using
   *jit-page-cache* (keyed by BC identity) if present — RE-PATCHING const
   immediates when a GC has fired since the page was built (the pool moved).
   Builds + caches a fresh page on a miss.  NIL if the page can't be built."
  (unless *jit-page-cache*
    (setq *jit-page-cache* (make-hash-table :test (quote eq))))
  (let ((hit (gethash bc *jit-page-cache*)))
    (if hit
        (let ((now (%gc-count)))
          (if (eql now (cadddr hit))
              hit
              ;; A GC moved the const-pool objects; re-bake immediates, then
              ;; re-store a fresh entry with the new stamp (avoid mutating a
              ;; nested list place — build + puthash is compiler-safe).
              (let ((fresh (list (car hit) (cadr hit) (caddr hit) now
                                 (car (cddddr hit)) (cadr (cddddr hit)))))
                ;; Arch-aware: x64 bakes movabs imm64, aarch64 a MOVZ/MOVK
                ;; quad, and self-modified aarch64 code needs an I-cache flush
                ;; over the page before it is branched into again.
                (if (eq *jit-target-arch* :aarch64)
                    (progn
                      (%jit-patch-consts-aarch64 (car fresh) (caddr fresh))
                      (when (caddr fresh)
                        (%jit-icache-flush (car fresh)
                                           (if (car (cddddr fresh))
                                               (car (cddddr fresh))
                                               4096))))
                    (%jit-patch-consts (car fresh) (caddr fresh)))
                (setf (gethash bc *jit-page-cache*) fresh)
                fresh)))
        (let ((je (%jit-translate-page bc mvm-entry ft-list rt-table)))
          (when je (setf (gethash bc *jit-page-cache*) je))
          je))))

(defun %mvm-eval-jit-run (bc entry ft-list fn-table rt-table lam-offsets cache-p
                          persist-names)
  "WS4-S5b: run compiled module BC as NATIVE JIT'd code, wrapping the result
   like the interpret path (%mvm-wrap-escaping-result).  If the JIT can't build
   a page we throw to the caller's handler-case, which falls back to
   mvm-interpret (safe: no user code ran).

   MULTIPLE VALUES (WS5 #218): native writes a real BSS MV block — the count as
   a tagged fixnum at #x10000090, the count-1 extras at #x10000098+ — and this
   function READS IT BACK, publishing (count . secondaries) in *mvm-last-mv*,
   the same shape mvm-interpret leaves behind.  The seam therefore re-emits the
   values directly and MV forms run exactly ONCE.  Previously they signalled an
   MV sentinel and the interpreter re-ran the whole form, duplicating every
   side effect that had already happened.  Only a count outside [0,21] (past
   the 20-slot MV-VALUES area) still falls back; see *jit-infra-fallback*.
   CACHE-P: T = reuse/store an exec page in *jit-page-cache* (cached modules,
   where the SAME bc re-evals — the speedup source); NIL = translate once, no
   cache (the fresh-bc DEF* path, whose page would never be re-hit).

   WS4 #160 Piece 2 — CONSERVATIVE-CORRECT transient reclamation (aarch64):
   a TRANSIENT form has EMPTY lam-offsets, meaning NO lambda/closure body was
   compiled INTO its exec page — the page holds only the top-level thunk's
   straight-line code, which nothing references once the native call returns.
   Such a page is translated WITHOUT caching and munmap'd immediately after the
   call (bounding a long run of distinct transient forms — e.g. a REPL/stress
   evaluating thousands of `(+ i 1)` — to O(1) live RWX pages).  CODE-BEARING
   forms (non-empty lam-offsets: a lambda body IS in the page, so an escaping
   closure may reference it) keep the cache/retain path and are NEVER freed —
   so there is NO use-after-free by construction.  As extra insurance a page is
   also retained if the form's RESULT is itself a function.  Reclaim is gated to
   *jit-target-arch* :aarch64; on x64 TRANSIENT is always NIL so behaviour and
   emitted-runtime are unchanged (the %jit-free-page trap is a no-op there)."
  ;; #160 Piece 2 (round 2): lam-offsets is NOT a usable transient signal on
  ;; aarch64 — it is ALWAYS non-nil (measured length 7 for every top-level form,
  ;; including `(+ i 1)`), so the earlier `(null lam-offsets)` never fired and
  ;; nothing was reclaimed (still exhausting ~2350).  Use the RESULT TYPE
  ;; instead: on aarch64 translate the run-tuple page WITHOUT caching (top-level
  ;; forms rarely re-eval the SAME bc, so the cache mostly just accumulates —
  ;; the exhaustion source) and, after the native call, FREE the page UNLESS the
  ;; result is a FUNCTION (an escaping closure whose code lives IN this page).
  ;; A `defun`/`setf-fn` form returns a symbol/non-function, and its installed
  ;; body is a SEPARATE module (its own page, built on first call) — so freeing
  ;; the top-level installer page is safe (validated by the defun/closure
  ;; retention probes).  x64 is untouched (aa64 is nil → original cache path).
  ;; STEP-A: reclamation is gated behind *jit-reclaim-on* (default NIL = the
  ;; CACHED baseline — %jit-entry-for, no free).  With it NIL, aa64 behaves
  ;; exactly like x64's cache path, so STEP A measures the true baseline; the
  ;; MAP_FAILED guard in %jit-translate-page-1-aarch64 still applies to both.
  (let* ((aa64 (and (eq *jit-target-arch* :aarch64)
                    (boundp (quote *jit-reclaim-on*)) *jit-reclaim-on*))
         (je (if (and cache-p (not aa64))
                 (%jit-entry-for bc entry ft-list rt-table)
                 (%jit-translate-page bc entry ft-list rt-table))))
    (if je
        (progn
          ;; WS5 #222: publish this module's top-level DEFUNs as REAL NATIVE
          ;; functions, replacing the interpreter trampolines mvm-eval-forms
          ;; installed a moment ago.  Done HERE — after every relocation and
          ;; const patch succeeded, so the page is fully formed, and BEFORE
          ;; %jit-call, so the thunk itself (and anything it calls) already
          ;; sees the native definition, exactly as it saw the trampoline.
          ;; On a page-build failure (je NIL) nothing is published and the
          ;; trampolines stand — the interpret fallback is unchanged.
          (when (and persist-names (cadr (cddddr je)) (%jit-native-defuns-p))
            (%jit-install-native-fns (car je) (cadr (cddddr je)) persist-names))
          ;; Native code stamps MV-count into real BSS; seed it to 1 so a
          ;; single-value form reads back 1 (the thunk epilogue sets it, but
          ;; be defensive).  A form that sets it >1 falls back for correct MV.
          (setf (mem-ref #x10000090 :u64) 1)
          ;; WS5 #203 POINT OF NO RETURN.  Everything above this line is JIT
          ;; setup: if it signals, no user code has run and the interpret
          ;; fallback is safe.  The instant we branch into the page, the form's
          ;; side effects begin, so any condition escaping from here on is the
          ;; user's and re-running the form would duplicate them.  Set the flag
          ;; BEFORE the call, not after — a condition signalled by the very
          ;; first instruction of the form must already count as "native ran".
          (setq *jit-native-ran* t)
          (let ((raw (%jit-call (+ (car je) (cadr je)))))
            ;; WS5 #218 — READ BACK THE NATIVE MV BLOCK.  This replaces the old
            ;; "MV form → re-interpret" sentinel, which was the last path that
            ;; re-ran a form whose side effects had already happened (measured:
            ;; `(progn (incf *k*) (floor 7 2))` left *k* = 2).  Its reach was
            ;; never limited to `(values …)`: ANY form whose last operation
            ;; leaves MV-count > 1 qualifies — floor / truncate / ceiling /
            ;; round / gethash / subtypep / read-from-string /
            ;; multiple-value-bind / -list / -call / -prog1.
            ;;
            ;; The block is the SAME location on every back-end: +mv-count-addr+
            ;; = #x10000090 holds the count as a TAGGED fixnum and the (count-1)
            ;; extras live at #x10000098 + i*8.  compile-values, op-set-mv-count
            ;; and both translators (translate-x64.lisp / translate-aarch64.lisp)
            ;; all use the compiler constant, so NO *jit-target-arch* branch is
            ;; needed here.  `mem-ref … :u64` yields the raw word, which IS the
            ;; tagged value, so mvc reads back as an ordinary integer and each
            ;; extra reads back as its Lisp object (exactly what compiled
            ;; MULTIPLE-VALUE-LIST does, compiler.lisp compile-multiple-value-list).
            ;;
            ;; ORDERING IS LOAD-BEARING — this block must be the first thing
            ;; after %jit-call and must not CALL anything until the last extra
            ;; is latched:
            ;;   * every function epilogue emits op-set-mv-count 1, so one call
            ;;     resets the count slot and we would lose the extras' length;
            ;;   * the collector scans exactly (count-1) words from #x10000098
            ;;     (translate-x64.lisp, the MV-area root scan).  If the count
            ;;     were reset while extras were still unread, a GC triggered by
            ;;     the cons loop would strand them at from-space addresses —
            ;;     the same class as the interp bug documented at interp.lisp:166.
            ;; Everything below compiles INLINE (mem-ref, fixnump, <, +, -, *,
            ;; cons), so the count stays authoritative throughout, and the
            ;; back-to-front read re-reads each BSS slot AFTER the previous
            ;; cons's possible GC — so a forwarded extra is picked up post-move.
            (let ((mvc (mem-ref #x10000090 :u64))
                  (secs nil)
                  (mv-ok nil))
              ;; Reproducible range: 0..21.  The MV-VALUES area is 20 slots
              ;; (#x10000098 .. #x10000130; +closure-env-addr+ starts at
              ;; #x10000140), so a count above 21 would read words that are NOT
              ;; MV storage and hand the GC bogus roots.  Counts that large are
              ;; already truncated by compile-values' own 16-slot storage cap on
              ;; BOTH paths, so re-interpreting them yields nothing better — but
              ;; the fallback is kept (and counted) rather than guessing.
              (if (fixnump mvc)
                  (if (< mvc 0) nil (if (< mvc 22) (setq mv-ok t) nil))
                  nil)
              (if mv-ok
                  (let ((i (- mvc 2)))
                    (loop
                      (when (< i 0) (return nil))
                      (setq secs (cons (mem-ref (+ #x10000098 (* i 8)) :u64) secs))
                      (setq i (- i 1))))
                  nil)
              (if mv-ok
                  (progn
                    ;; Publish the block in the SAME shape mvm-interpret does
                    ;; (interp.lisp: (cons count secondaries)) so the seam's
                    ;; `(values-list (cons %prim (cdr %mv)))` re-emission is
                    ;; identical for the native and interpreted paths.  mvc = 1
                    ;; means single value: NIL, which also clears a STALE
                    ;; *mvm-last-mv* left by an earlier interpret run.
                    (setq *mvm-last-mv* (if (eql mvc 1) nil (cons mvc secs)))
                    (setq *jit-native-count*
                          (if *jit-native-count* (+ 1 *jit-native-count*) 1))
                    (let ((result (%mvm-wrap-escaping-result raw bc fn-table rt-table lam-offsets)))
                      ;; Reclaim the (uncached) aarch64 page unless the RESULT is a
                      ;; function/closure whose code is in it.  car(cddddr je) = PSIZE.
                      (when (and aa64 (car (cddddr je)) (not (functionp result)))
                        (%jit-free-page (car je) (car (cddddr je))))
                      result))
                  ;; RESIDUAL SHAPE ONLY: a count outside [0,21] (or a non-fixnum
                  ;; word aliasing the slot).  Still double-executes, still
                  ;; counted — *jit-mv-fallback-count* is the proof that it does
                  ;; not fire in practice.  DON'T free the page (result unknown).
                  (progn
                    (setq *jit-infra-fallback* t)
                    (setq *jit-mv-fallback-count*
                          (if *jit-mv-fallback-count* (+ 1 *jit-mv-fallback-count*) 1))
                    (error "jit-mv-fallback"))))))
        ;; NO PAGE: %jit-translate-page's flip-safety guard returned NIL (a
        ;; translator gap it converted from a signal, or a page-build failure).
        ;; Its contract is "a translator gap can NEVER escape as an uncaught
        ;; error — it only costs one interpret fallback", so DEGRADE HERE, at
        ;; the single point of detection, rather than signalling and relying on
        ;; every caller to convert the signal back into an interpret.
        ;;
        ;; WS5 #203: `(error "jit-page-unavailable")` broke that contract on the
        ;; path that matters most for loading a library.  Both existing callers
        ;; (mvm-eval-forms and the %e2ic seam) do wrap this in a handler-case
        ;; that interprets — but MACROEXPANSION of a runtime-defined macro runs
        ;; the expander from inside %mvm-eval-compile-tuple, i.e. while COMPILING
        ;; a later top-level form, and that path has no such wrapper.  Net
        ;; effect on aarch64 (JIT on by default since #199): a macro defined by
        ;; one top-level form and used by ANY later form — the shape of every
        ;; real library, and nearly all of alexandria — died with
        ;; "jit-page-unavailable", uncatchable even by a handler-case around the
        ;; use, because the failure happened before that form ever ran.
        ;;
        ;; Interpreting here is exactly what the two callers' handlers already
        ;; do, so their behaviour is unchanged; they stay as belt-and-suspenders.
        (progn
          (setq *jit-r-page-nil*
                (if *jit-r-page-nil* (+ 1 *jit-r-page-nil*) 1))
          (setq *jit-fallback-count*
                (if *jit-fallback-count* (+ 1 *jit-fallback-count*) 1))
          (%mvm-wrap-escaping-result
            (mvm-interpret bc :entry-point entry
                           :function-table fn-table :runtime-table rt-table
                           :return-raw nil :lambda-offsets lam-offsets)
            bc fn-table rt-table lam-offsets)))))

(defun %mvm-eval-run-tuple (tuple)
  "Interpret a cached compiled module tuple (bc entry ft-list fn-table rt-table lam-offsets).
   Conditions PROPAGATE to the caller (production EVAL semantics): an error
   signalled inside the evaluated form must reach the caller's handler-case.
   The old `(handler-case ... (error (e) (list :interp-err e)))` swallowed the
   signal into a return VALUE, so every `(eval ...) must signal` ANSI test
   (vector-push*.error, defmethod.error, signals-error helpers) failed under
   the WS3 flip.  Callers that want the capture behaviour (the e2diff gate)
   wrap mvm-eval in their own handler-case."
  ;; MULTIPLE VALUES propagate (WS3 flip): mvm-interpret stashes the run's
  ;; simulated MV state in *mvm-last-mv* (read IMMEDIATELY — see interp.lisp);
  ;; re-emit via values-list so the native caller's multiple-value-list /
  ;; m-v-bind around (eval …) sees every value, matching the tree-walker.
  ;; The tail if/values-list shape is values-preserving per
  ;; tail-form-is-values-p, so this function's epilogue does NOT reset
  ;; MV-count back to 1.
  ;;
  ;; TUPLE layout (WS4-S5b): (bc entry ft-list fn-table rt-table lam-offsets)
  ;;   car=bc caddr=ft-list cadddr=fn-table (car(cddddr))=rt-table
  ;;   (cadr(cddddr))=lam-offsets.  ft-list was inserted at index 2 for the JIT.
  (let* ((%bc (car tuple))
         (%entry (cadr tuple))
         (%ftl (caddr tuple))
         (%fnt (cadddr tuple))
         (%rt (car (cddddr tuple)))
         (%lam (cadr (cddddr tuple)))
         ;; WS4-S5b: when *use-jit*, run native; fall back to interpret on a JIT
         ;; SETUP failure (unsupported form / page-build failure), and on the MV
         ;; sentinel.  %jit-active-p (not %jit-enabled-p) so *jit-inhibit*
         ;; (Class 3) works even in the gate image whose %jit-enabled-p is a
         ;; baked constant.
         ;;
         ;; WS5 #203: the handler used to be `(t (c) ...interpret...)` — it
         ;; answered EVERY condition by re-running the whole form, including a
         ;; genuine error signalled by the user's own code after that code had
         ;; already run.  Save the guard flags lexically and clear them before
         ;; the attempt (setq, not a dynamic rebind — see 279f2cc: a dynamic
         ;; rebind does not survive the native/interpret longjmp boundary the
         ;; way a lexical save + explicit restore does), then decide in the
         ;; handler on the basis of whether native code actually ran.
         (%jnr-save *jit-native-ran*)
         (%jif-save *jit-infra-fallback*)
         (%ignore1 (setq *jit-native-ran* nil))
         (%ignore2 (setq *jit-infra-fallback* nil))
         (%prim (if (%jit-active-p)
                    ;; PERSIST-NAMES = NIL on the CACHED-tuple path: this module
                    ;; was already compiled and its DEFUNs already published by
                    ;; the mvm-eval-forms call that built the tuple, so there is
                    ;; nothing new to install (and the tuple does not carry the
                    ;; name list).
                    (handler-case (%mvm-eval-jit-run %bc %entry %ftl %fnt %rt %lam t nil)
                      (t (c)
                         (let ((%ran *jit-native-ran*)
                               (%infra *jit-infra-fallback*))
                           ;; Restore the enclosing attempt's flags before we
                           ;; either re-signal or recurse into the interpreter;
                           ;; a nested mvm-eval (a macroexpander invoked from
                           ;; JIT'd code) must not inherit ours.
                           (setq *jit-native-ran* %jnr-save)
                           (setq *jit-infra-fallback* %jif-save)
                           (if (and %ran (not %infra) (%condition-p c))
                               ;; USER CONDITION, raised after the form's side
                               ;; effects began.  Re-running would duplicate
                               ;; them.  Propagate it, exactly as the non-JIT
                               ;; path does — production EVAL semantics.
                               (progn
                                 (setq *jit-resignal-count*
                                       (if *jit-resignal-count*
                                           (+ 1 *jit-resignal-count*) 1))
                                 (error c))
                               ;; Fall back.  THREE ways to get here:
                               ;;  (1) JIT setup failed before any user code ran
                               ;;      — safe and invisible, the original intent;
                               ;;  (2) the MV out-of-range residual (WS5 #218
                               ;;      reduced this from "every MV form" to a
                               ;;      count > 21) — known double-execute;
                               ;;  (3) native ran and escaped with something that
                               ;;      is NOT a well-formed condition (%condition-p
                               ;;      false).  (3) is the unresolved-runtime-
                               ;;      function sentinel: an INFRASTRUCTURE failure
                               ;;      that happens DURING native execution, which
                               ;;      is the case the re-flip gate's
                               ;;      "infrastructure vs user condition" dichotomy
                               ;;      did not anticipate.  Re-signalling it yields
                               ;;      a malformed #(NIL NIL) at toplevel, so we
                               ;;      still fall back (and still double) until the
                               ;;      JIT can resolve runtime-defined functions.
                               (progn
                                 ;; CENSUS: split the seam-handler fallback into
                                 ;; "native already RAN" (the doubling case, an
                                 ;; infrastructure escape) vs pure setup failure.
                                 ;; NB: %infra excludes the MV residual, which
                                 ;; reaches this handler with %ran already T --
                                 ;; counting it here would double-count R-MV.
                                 (when (and %ran (not %infra))
                                   (setq *jit-r-native-escape*
                                         (if *jit-r-native-escape*
                                             (+ 1 *jit-r-native-escape*) 1)))
                                 (setq *jit-fallback-count*
                                       (if *jit-fallback-count*
                                           (+ 1 *jit-fallback-count*) 1))
                                 (%mvm-wrap-escaping-result
                                   (mvm-interpret %bc :entry-point %entry
                                                  :function-table %fnt :runtime-table %rt
                                                  :return-raw nil :lambda-offsets %lam)
                                   %bc %fnt %rt %lam))))))
                    (%mvm-wrap-escaping-result
                      (mvm-interpret %bc :entry-point %entry
                                     :function-table %fnt :runtime-table %rt
                                     :return-raw nil :lambda-offsets %lam)
                      %bc %fnt %rt %lam)))
         ;; *mvm-last-mv* must be read IMMEDIATELY after the run (see the note
         ;; at the top of this function and interp.lisp) — nothing may come
         ;; between %prim and %mv.
         (%mv *mvm-last-mv*)
         ;; WS5 #203: restore on the SUCCESS path too, but only AFTER %mv has
         ;; been latched.  The handler restores before it re-signals or falls
         ;; back; a run that completes normally would otherwise leave
         ;; *jit-native-ran* set to T, and the NEXT form's handler would read
         ;; that stale T and re-signal a setup failure it should have quietly
         ;; interpreted.
         (%ignore3 (setq *jit-native-ran* %jnr-save))
         (%ignore4 (setq *jit-infra-fallback* %jif-save)))
    (if %mv
        (if (eql (car %mv) 0)
            (values)
            (values-list (cons %prim (cdr %mv))))
        %prim)))

(defun %mvm-eval-compile-tuple (forms)
  "WS4 STAGE 2 seam tap.  Compile FORMS to an MVM bytecode module via the
   SAME self-hosted compiler pipeline mvm-eval-forms uses (mvm-compile-toplevel
   + Pass1/1.5/2), but STOP before interpreting.  Returns a 6-tuple:

     (bc entry ft-list fn-table rt-table lam-offsets)

   where BC is the module bytecode, ENTRY the %MVM-EVAL-THUNK native entry
   OFFSET into BC, FN-TABLE the offset-array mvm-eval-forms hands mvm-interpret
   (:function-table), RT-TABLE the runtime-call table (out-of-module CALLs;
   EMPTY = hazard-free = JIT-executable without Stage-3 relocation),
   LAM-OFFSETS the lambda-body offset set, and FT-LIST the
   (name offset length) function-table list translate-mvm-to-x64 wants
   (the ADAPTER — built from the SAME function-info structs the compiler
   produced, so it is consistent with FN-TABLE / ENTRY).

   This does NOT install trampolines, does NOT cache, and does NOT interpret
   — it is a pure compile.  mvm-eval-forms is UNCHANGED (behavior-identical);
   this is a parallel, dead-code path reachable only from the WS4-S2 probe.
   It intentionally omits the persist/reentrancy bookkeeping because a JIT
   probe form is a single self-contained expression (no cross-call defuns)."
  (unless (and *opcode-table* (> (hash-table-count *opcode-table*) 0))
    (setq *opcode-table* (make-hash-table :test (quote eql)))
    (%populate-opcode-table))
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
        (rt-next #x40000000))
    (register-mvm-bootstrap-macros)
    (let* ((rforms (reverse forms))
           (last-form (car rforms))
           (last-defun-p (and (consp last-form) (symbolp (car last-form))
                              (string-equal (symbol-name (car last-form)) "DEFUN")
                              (symbolp (cadr last-form))))
           (expr (if last-defun-p (list (quote quote) (cadr last-form)) last-form))
           (defs (if last-defun-p forms (reverse (cdr rforms))))
           (toplevel (append defs (list (list (quote defun) (quote %mvm-eval-thunk) nil expr)))))
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
      (dolist (pend *pending-flet-ir*)
        (when (and (consp pend) (car pend) (cdr pend))
          (setq all-ir (cons pend all-ir))))
      (setq all-ir (reverse all-ir))
      ;; Fresh buffer (do NOT touch *mvm-eval-buffer* — keep mvm-eval-forms's reuse
      ;; buffer pristine for the oracle interpret path that runs after us).
      (setq buf (make-mvm-buffer :bytes (make-array 65536)))
      ;; Pass 1: assign cumulative bytecode offsets + register in *functions*.
      (dolist (e all-ir)
        (let* ((info (car e)) (ir (cdr e))
               (fn-size (let ((s 0)) (dolist (insn ir) (setq s (+ s (ir-instruction-size insn)))) s)))
          (setf (function-info-bytecode-offset info) global-offset)
          (setf (function-info-bytecode-length info) fn-size)
          (setf (gethash (function-info-name info) *functions*) info)
          (setq global-offset (+ global-offset fn-size))))
      ;; Pass 1.5: out-of-module CALL / LI-FUNC names → synthetic runtime stubs.
      (dolist (e all-ir)
        (dolist (insn (cdr e))
          (let ((name (cond ((eq (car insn) :call) (cadr insn))
                            ((eq (car insn) :li-func) (caddr insn))
                            (t nil))))
            (when (and name (stringp name) (not (gethash name *functions*)))
              (let ((info (make-function-info :name name :bytecode-offset rt-next
                                              :bytecode-length 0)))
                (setf (gethash name *functions*) info)
                (setf (gethash rt-next rt-table) name)
                (setq rt-next (+ rt-next 1)))))))
      ;; Pass 2: emit bytecode.
      (dolist (e all-ir)
        (let* ((ir (cdr e)) (lp (compute-label-positions ir)))
          (emit-bytecode-for-ir buf ir lp)))
      (dolist (e all-ir)
        (when (string-equal (string (function-info-name (car e))) "%MVM-EVAL-THUNK")
          (setq entry (function-info-bytecode-offset (car e)))))
      (let ((bc (mvm-buffer-used-bytes buf))
            (fn-table (make-array (length all-ir)))
            (lam-offsets (make-hash-table))
            (ft-list nil)
            (i 0))
        (dolist (e all-ir)
          (aset fn-table i (function-info-bytecode-offset (car e)))
          (setq i (+ i 1))
          (let ((nm (string (function-info-name (car e))))
                (off (function-info-bytecode-offset (car e)))
                (len (function-info-bytecode-length (car e))))
            ;; ADAPTER: (name offset length) for translate-mvm-to-x64 — same
            ;; structs, string name.  Reverse-consed then reversed to preserve
            ;; the all-ir order (which is what fn-table indices track too).
            (setq ft-list (cons (list nm off len) ft-list))
            (if (and (or (search "$$LAMBDA" nm) (search "$$CLOSURE" nm))
                     (not (eql off 0)))
                (puthash off lam-offsets t)
                (puthash off lam-offsets (quote :defun)))))
        (list bc entry (reverse ft-list) fn-table rt-table lam-offsets)))))

(defun mvm-eval-forms (forms)
  ;; In-image: emit integer literals as fixnum-safe :li-halves (set the GLOBAL,
  ;; not a let-binding — compiled LET of a special may not establish a dynamic
  ;; binding the compiler's compile-integer reads).  Native builds never call
  ;; mvm-eval-forms, so the global stays NIL there.
  (setq *mvm-emit-halves* t)
  ;; Mark in-image runtime compilation so mvm-compile-toplevel routes package
  ;; side-effecting forms (DEFPACKAGE) to their runtime impl instead of the
  ;; build-time no-op.  setq (not let): compiled let of a special is unreliable
  ;; in-image (same reason as *mvm-emit-halves* above).  Native builds never
  ;; call mvm-eval-forms, so the global stays NIL at build time.
  (setq *mvm-eval-runtime-p* t)
  ;; LAZY opcode-table init.  encode-instruction (mvm.lisp) reads *opcode-table*
  ;; for each instruction's operand spec during Pass-2 emit; with a NIL table
  ;; (the ANSI image skips init-all-globals, so the defparameter init thunk
  ;; never ran) every operand is silently DROPPED → corrupt bytecode → garbage
  ;; result.  Create + populate here, on first mvm-eval use, NOT at boot: a
  ;; permanent populated *opcode-table* GC root shifts GC timing enough to
  ;; surface a latent crash elsewhere (GET-INTERNAL-RUN-TIME.2 / 0xDEAD0004),
  ;; and mvm-eval is dead code until the WS3 flip, so lazy keeps the normal image's
  ;; live set identical to baseline.  Skips when already populated (the generic
  ;; image, or a 2nd mvm-eval call).  NB %populate-opcode-table's `setf gethash`
  ;; no-ops on a NIL table, so the table MUST be created first.
  (unless (and *opcode-table* (> (hash-table-count *opcode-table*) 0))
    (setq *opcode-table* (make-hash-table :test (quote eql)))
    (%populate-opcode-table))
  ;; PERF Round 2 (compile-caching): if these FORMS are cacheable (no side-
  ;; effecting DEF*) and already compiled, re-interpret the cached module and
  ;; skip the whole compile pipeline.  Checked BEFORE the big let so a hit pays
  ;; none of the setup/hash-alloc cost either.
  (let ((%cacheable (and (not *mvm-eval-no-cache*) (%mvm-eval-cacheable-p forms))))
    (when %cacheable
      (unless *mvm-eval-cache*
        (setq *mvm-eval-cache* (make-hash-table :test (quote equal))))
      (let ((%hit (gethash forms *mvm-eval-cache*)))
        (when %hit (return-from mvm-eval-forms (%mvm-eval-run-tuple %hit)))))
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
        ;; tables — so a LATER (mvm-eval …) call OR the tree-walker can call it.
        ;; Without this every (mvm-eval '(defun f …)) discarded f (closed-world
        ;; module), so no multi-form program (asdf/load/REPL) could run on mvm-eval.
        ;; Computed AFTER register-mvm-bootstrap-macros (below) via
        ;; %e2-scan-persist, which sees through EVAL-WHEN / PROGN / LOCALLY
        ;; AND macro wrappers (with-upgradability) — see its docstring.
        (persist-names nil))
    (register-mvm-bootstrap-macros)
    ;; WS3 def persistence pre-scan.  Runs with *macro-table* freshly populated
    ;; (bootstrap macros) so %e2-scan-persist's macroexpand-1-mvm sees the same
    ;; macro environment the compile loop below will; runtime user macros
    ;; resolve through the *mvm-eval-runtime-p* fallback.
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
           (toplevel (append defs (list (list (quote defun) (quote %mvm-eval-thunk) nil expr))))
           ;; REENTRANCY: a NESTED mvm-eval during this compile loop (the toplevel
           ;; DEFMACRO handler's expander eval, build-macrolet-expander,
           ;; DEFCONSTANT value eval) re-enters mvm-eval-forms, which clears and
           ;; consumes *e2-persist-defuns* — clobbering the outer call's
           ;; accumulated names.  Save the current value in a LEXICAL here and
           ;; restore it after the union below, so each nesting level sees only
           ;; its own recordings.  (Explicit save+setq+restore, NOT a let of
           ;; the special — compiled let of a special is unreliable in-image.)
           (%e2pd-saved *e2-persist-defuns*)
           ;; Task #244.  Same reentrancy discipline as %E2PD-SAVED: publish
           ;; THIS module's pre-scanned defun names for compile-function-ref's
           ;; forward-reference test (%e2-fn-in-module-p), and put the
           ;; enclosing call's list back when the compile loop is done.
           (%e2md-saved *e2-module-defuns*))
      ;; Compiler-recorded persistence: clear the global BEFORE the compile
      ;; loop; the toplevel DEFUN handler pushes every toplevel-context defun
      ;; name (post-macroexpansion) onto it.  setq, not let (compiled let of
      ;; a special is unreliable in-image — see *mvm-emit-halves*).
      (setq *e2-persist-defuns* nil)
      ;; Task #244: publish the pre-scanned module defun names for the
      ;; duration of the compile loop, so `#'LATER-FN' inside an EARLIER
      ;; function of this same module is recognised as in-module and
      ;; materialised as a closure instead of escaping as a raw offset.
      (setq *e2-module-defuns* persist-names)
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
      ;; %MVM-EVAL-THUNK excludes itself by name.  NB: a NESTED mvm-eval during
      ;; this compile loop (a macroexpander that itself calls EVAL) would
      ;; clear/consume the global mid-flight; the pre-scan names remain as
      ;; the safety net in that (rare) case.
      (dolist (pn *e2-persist-defuns*)
        (unless (or (string-equal pn "%MVM-EVAL-THUNK")
                    (member pn persist-names :test (function string=)))
          (setq persist-names (cons pn persist-names))))
      ;; REENTRANCY: restore the enclosing mvm-eval-forms call's recordings
      ;; (see %e2pd-saved above).  Outermost call restores NIL — harmless.
      (setq *e2-persist-defuns* %e2pd-saved)
      (setq *e2-module-defuns* %e2md-saved))
    ;; Small buffer (the default 128MB byte array blows the in-image heap).
    ;; PERF: REUSE a persistent 64KB buffer instead of (make-array 65536) every
    ;; call.  mvm-buffer-used-bytes copies the bytecode out before the next call,
    ;; so nothing retains the buffer — safe to reset + reuse.
    (if *mvm-eval-buffer*
        (progn
          (setf (mvm-buffer-position *mvm-eval-buffer*) 0)
          (clrhash (mvm-buffer-labels *mvm-eval-buffer*))
          (setf (mvm-buffer-fixups *mvm-eval-buffer*) nil)
          (setq buf *mvm-eval-buffer*))
        (progn
          (setq buf (make-mvm-buffer :bytes (make-array 65536)))
          (setq *mvm-eval-buffer* buf)))
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
    ;; this is the higher-order mvm-eval path (funcall/apply/mapcar #'NAME).
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
      (when (string-equal (string (function-info-name (car e))) "%MVM-EVAL-THUNK")
        (setq entry (function-info-bytecode-offset (car e)))))
    (let ((bc (mvm-buffer-used-bytes buf)))
      (if entry
          (let ((fn-table (make-array (length all-ir))) (i 0)
                ;; LAMBDA-OFFSETS: the bytecode entry offsets of in-module LAMBDA /
                ;; CLOSURE bodies (named *$$LAMBDA* / *$$CLOSURE* by compile-lambda).
                ;; The interp's native-bridge uses this to recognise an mvm-eval lambda
                ;; VALUE escaping to a native higher-order fn (mapcar/reduce/…) and
                ;; wrap it in a re-entrant trampoline.  Keyed by offset; ONLY genuine
                ;; lambda bodies are recorded (never the %mvm-eval-thunk / helper defuns
                ;; / the fn at offset 0), so an ordinary fixnum DATA argument — a loop
                ;; counter 0/1/2, an index — is never mistaken for a callable.
                (lam-offsets (make-hash-table))
                ;; WS4-S5b: (name offset length) list for translate-mvm-to-x64
                ;; (the JIT adapter — same function-info structs as fn-table).
                (ft-list nil))
            (dolist (e all-ir)
              (aset fn-table i (function-info-bytecode-offset (car e)))
              (setq i (+ i 1))
              (setq ft-list
                    (cons (list (string (function-info-name (car e)))
                                (function-info-bytecode-offset (car e))
                                (function-info-bytecode-length (car e)))
                          ft-list))
              (let ((nm (string (function-info-name (car e))))
                    (off (function-info-bytecode-offset (car e))))
                ;; The T vs :DEFUN distinction is now COSMETIC (both entries
                ;; are accepted by %mvm-module-fn-offset-p, the only reader).
                ;; It used to matter because %mvm-wrap-escaping had a
                ;; BARE-INTEGER branch that fired on the T entries alone, and
                ;; offset 0 was excluded there so a DATA fixnum 0 (make-list 0 /
                ;; member 0 / (- 10 j)=0 in nsubstitute's bounds loops) would
                ;; not be wrapped into a #x52 trampoline.  That 0-guard was
                ;; only ever a partial patch — ANY small data fixnum can equal
                ;; a lambda offset — so the branch itself is gone; see
                ;; %MVM-WRAP-ESCAPING in interp.lisp (alexandria EXTREMUM.1).
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
                    (puthash off lam-offsets (quote :defun)))))
            ;; WS3 def persistence: install each top-level user DEFUN as a
            ;; re-entrant interp trampoline in BOTH global function tables —
            ;; *symbol-function-table* by NAME (the mvm-eval native-call bridge's
            ;; %mvm-resolve-runtime-fn key) and *native-sym-function-table* by
            ;; HASH (symbol-function / funcall key).  The trampoline closes over
            ;; BC so the module bytecode stays GC-alive; fn-table + lam-offsets
            ;; are fully built by now; env = NIL (a top-level defun captures
            ;; nothing).  A later (mvm-eval …) call OR the tree-walker now resolves f.
            (when persist-names
              (dolist (e all-ir)
                (let ((pn (string (function-info-name (car e)))))
                  (when (member pn persist-names :test (function string=))
                    ;; WS5 #222 REDEFINITION INVALIDATION.  This is the ONLY
                    ;; point at which PN's PREVIOUS binding is still visible —
                    ;; the puthash below overwrites it, and by the time
                    ;; %jit-install-native-fns runs (after the page is built)
                    ;; the old value is already gone.  If the previous binding
                    ;; was NATIVE code (tag nibble 3 — either build-time or a
                    ;; #222 install), some exec page in *jit-page-cache* may
                    ;; have BAKED that address into a `movabs rax, imm64; call
                    ;; rax` site, and re-running that cached page would silently
                    ;; call the SUPERSEDED definition.  Drop the whole page
                    ;; cache so every cached module re-translates and
                    ;; re-relocates against the new definition.
                    ;;
                    ;; This must happen even when the NEW definition ends up
                    ;; being only a trampoline (a const-bearing body, or a
                    ;; module whose relocation failed): the stale cached page
                    ;; still holds the OLD native address and would keep
                    ;; calling it — the first redefinition bug this feature
                    ;; produced (`(defun f …)` twice, then the SAME call form
                    ;; text, returned the second definition's answer for the
                    ;; third definition).  A FIRST definition never invalidates
                    ;; anything: no page can have baked an address for a name
                    ;; that did not previously resolve to native code, so
                    ;; loading a library pays nothing here.
                    (let ((%prev (%mvm-resolve-runtime-fn pn)))
                      (when (and %prev *jit-page-cache*
                                 (eql (logand (%val->word %prev) 15) 3))
                        (clrhash *jit-page-cache*)))
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
            ;; later mvm-eval of the same forms skips the whole compile.  bc is a
            ;; fresh copy (mvm-buffer-used-bytes), safe despite buffer reuse.
            (when %cacheable
              (setf (gethash forms *mvm-eval-cache*)
                    ;; WS4-S5b: ft-list inserted at index 2 (see %mvm-eval-run-tuple
                    ;; layout comment).  reverse → source order (fn-table order).
                    (list bc entry (reverse ft-list) fn-table rt-table lam-offsets)))
            ;; Conditions PROPAGATE (see %mvm-eval-run-tuple): production EVAL
            ;; must let an error signalled by the form reach the caller's
            ;; handler-case instead of returning (:interp-err e) as a value.
            ;; The RESULT is wrapped when it is an in-module #x52 lambda
            ;; closure (see %mvm-wrap-escaping-result) so `(eval '#'(lambda
            ;; ...))` hands back a natively-funcallable, per-call-distinct
            ;; function object.
            ;; MULTIPLE VALUES propagate — same *mvm-last-mv* + values-list
            ;; re-emission as %mvm-eval-run-tuple (see the comment there).
            ;; *e2-active-defun-names* = persist-names for the duration of the
            ;; run so fmakunbound honors source order vs the pre-run defun
            ;; installation (see the defvar).  Lexical-save + setq-restore
            ;; (nested mvm-eval during the run saves/restores its own).
            (let* ((%adn-saved *e2-active-defun-names*)
                   ;; WS5 #203: the re-execution guard, same as in
                   ;; %mvm-eval-run-tuple.  THIS is the site the doubling was
                   ;; measured at (a top-level form calling a runtime-defined
                   ;; function): the old `(t (c) ...interpret...)` answered a
                   ;; condition raised AFTER the form's side effects had already
                   ;; run by re-running the whole form.  Lexical-save +
                   ;; setq-restore, matching %adn-saved directly above.
                   (%jnr-saved *jit-native-ran*)
                   (%jif-saved *jit-infra-fallback*)
                   (%prim (progn
                            (setq *e2-active-defun-names* persist-names)
                            (setq *jit-native-ran* nil)
                            (setq *jit-infra-fallback* nil)
                            ;; WS4-S5b: JIT when enabled, interpret-fallback on a
                            ;; SETUP failure (page-build fail / unsupported) or
                            ;; the MV out-of-range residual — but NOT on a user condition
                            ;; raised once native code is running.
                            ;; %jit-active-p honors *jit-inhibit* (Class 3).
                            (if (%jit-active-p)
                                (handler-case
                                    (%mvm-eval-jit-run bc entry (reverse ft-list)
                                                    fn-table rt-table lam-offsets nil
                                                    persist-names)
                                  (t (c)
                                     (let ((%ran *jit-native-ran*)
                                           (%infra *jit-infra-fallback*))
                                       (setq *jit-native-ran* %jnr-saved)
                                       (setq *jit-infra-fallback* %jif-saved)
                                       (if (and %ran (not %infra) (%condition-p c))
                                           ;; User condition, after side effects
                                           ;; began → propagate, never re-run.
                                           (progn
                                             (setq *e2-active-defun-names* %adn-saved)
                                             (setq *jit-resignal-count*
                                                   (if *jit-resignal-count*
                                                       (+ 1 *jit-resignal-count*) 1))
                                             (error c))
                                           (progn
                                     (when (and %ran (not %infra))
                                       (setq *jit-r-native-escape*
                                             (if *jit-r-native-escape*
                                                 (+ 1 *jit-r-native-escape*) 1)))
                                     (setq *jit-fallback-count*
                                           (if *jit-fallback-count* (+ 1 *jit-fallback-count*) 1))
                                     (%mvm-wrap-escaping-result
                                       (mvm-interpret bc :entry-point entry
                                                      :function-table fn-table
                                                      :runtime-table rt-table
                                                      :return-raw nil
                                                      :lambda-offsets lam-offsets)
                                       bc fn-table rt-table lam-offsets))))))
                                (%mvm-wrap-escaping-result
                                  (mvm-interpret bc :entry-point entry :function-table fn-table
                                                 :runtime-table rt-table :return-raw nil
                                                 :lambda-offsets lam-offsets)
                                  bc fn-table rt-table lam-offsets))))
                   (%mv *mvm-last-mv*))
              (setq *e2-active-defun-names* %adn-saved)
              ;; WS5 #203: restore the guard flags on the SUCCESS path too, or a
              ;; completed native run leaves *jit-native-ran* set and the NEXT
              ;; form's handler re-signals a setup failure it should have
              ;; quietly interpreted.  After %mv is latched, as above.
              (setq *jit-native-ran* %jnr-saved)
              (setq *jit-infra-fallback* %jif-saved)
              (if %mv
                  (if (eql (car %mv) 0)
                      (values)
                      (values-list (cons %prim (cdr %mv))))
                  %prim)))
          :no-entry))))
  )
;; Single-expression convenience.
(defun mvm-eval (form) (mvm-eval-forms (list form)))

;;; ============================================================
;;; WS3 Phase 3 — mvm-eval lambda-body-against-env entry (%e2ic)
;;; ============================================================
;;;
;;; The tree-walker (%eval-in-env) could not be deleted because it was the
;;; only engine that could evaluate a LAMBDA BODY AGAINST A CAPTURED ENV —
;;; the %interp-closure call path (compile nil '(lambda …), coerce 'function,
;;; runtime define-compiler-macro expanders, walker-created closures) and
;;; DEFTYPE expansion (%expand-deftype).  This block gives mvm-eval that entry:
;;;
;;;   %e2ic-compile PARAMS BODY ENV → native trampoline (or NIL = fallback)
;;;
;;; Design: compile `(lambda PARAMS (symbol-macrolet SM . BODY))` via mvm-eval,
;;; where SM maps each captured env binding NAME to `(cdr 'PAIR)` — PAIR
;;; being the walker's OWN alist binding cons, passed by identity through
;;; the mvm-eval quote pool (*e2-const-pool*).  Reads see the live cell value;
;;; `(setq NAME v)` expands (compile-setq symbol-macro path) to
;;; `(set-cdr 'PAIR v)` — mutating the SAME cons the walker and any sibling
;;; closures share, so mutation semantics match the walker exactly.
;;; mvm-eval returns the lambda as a re-entrant native trampoline
;;; (%mvm-wrap-escaping-result → %mvm-make-trampoline), cached per closure
;;; (5th slot of the %interp-closure list) so repeated calls pay only the
;;; ~0.6M-cycle interpret, not the ~16M-cycle compile.
;;;
;;; WS3 STEP 4 (tree-walker DELETED from production images): there is no
;;; walker fallback any more.  A shape outside mvm-eval's lambda support —
;;;   - junk lambda list (NIL / non-symbol atom element, unknown &-marker;
;;;     MACRO lambda lists — dotted tails, nested destructuring, &whole/
;;;     &environment/&body — are handled since the WS3 finisher via
;;;     compile-lambda's %transform-macro-lambda-list),
;;;   - a captured binding whose name is unresolvable,
;;;   - mvm-eval COMPILE failure (conditions during the body's execution are NOT
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
  "name-string → (entry . trampoline-or-:walker) for %expand-deftype's mvm-eval
   route.  The registered (params . body) ENTRY cons is stored alongside so a
   re-registered deftype (entry no longer eq) recompiles.")

(defun %e2ic-mvm-eval-nocache (form)
  "mvm-eval FORM with *mvm-eval-cache* bypassed (see *mvm-eval-no-cache*): the form
   embeds captured env conses via the quote pool, so EQUAL-keyed caching
   would alias two different closures' cells.  setq + unwind-protect (not a
   let of the special — compiled let of a special is unreliable in-image).

   CLASS-3 (JIT interp-closure nested-call boundary): compile+build this
   interp-closure trampoline with the JIT DISABLED.  An %interp-closure's
   trampoline is ALWAYS run via mvm-interpret (%mvm-make-trampoline re-enters
   the interpreter; *jit-native-count* does not move for its body), so
   JIT-translating its %mvm-eval-thunk is pure overhead — AND it triggers a
   native<->interp boundary fault: when this thunk is built under the JIT and
   the resulting interp-closure body makes a REAL out-of-module call whose
   callee is itself an interp trampoline (a user DEFUN persisted via
   %mvm-make-trampoline), invoking that inner trampoline from the outer
   interp bridge — while a JIT-native frame is on the stack — jumps to a
   corrupted native target (RIP=RAX=an unaligned exec-page address) and the
   boot SIGSEGV handler longjmps out.  `(gcd a b)` in an interp-closure works
   (gcd's symbol-function is a native builtin, not a trampoline); `(g x)` for
   a user defun faults (g's symbol-function is a trampoline).  Forcing the
   interp-closure module through the interpret path removes the extra native
   frame, so the nested interp->interp bridge call runs correctly — matching
   the JIT-off baseline exactly.  The inhibit is done via *jit-inhibit* (NOT
   `(setq *use-jit* nil)`): the ANSI gate BAKES %jit-enabled-p to a constant
   that ignores *use-jit*, so *jit-inhibit* + the %jit-active-p seam gate is
   the only inhibit that works in BOTH the CLI and the gate image.  Lexically
   saved + setq-restored (do NOT dynamically rebind the special: compiled let
   of a special is unreliable in-image — see *mvm-eval-no-cache* above)."
  (let ((%saved *mvm-eval-no-cache*)
        (%savedinh (if (boundp (quote *jit-inhibit*)) *jit-inhibit* nil)))
    (setq *mvm-eval-no-cache* t)
    (setq *jit-inhibit* t)
    (unwind-protect
        (mvm-eval form)
      (setq *mvm-eval-no-cache* %saved)
      (setq *jit-inhibit* %savedinh))))

(defun %e2ic-ll-marker (x)
  "If X is a lambda-list &-marker symbol, return its NAME string; else NIL."
  (let ((nm (%eval-sym-name x)))
    (if (and nm (> (length nm) 0) (= (%prim-aref nm 0) 38))  ; 38 = &
        nm
        nil)))

(defun %e2ic-simple-ll-p (params)
  "T when PARAMS is a lambda list the mvm-eval compile path handles.  Since
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
   mvm-eval quote pool."
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
   trampoline via mvm-eval, or NIL when the shape needs the walker.  Only the
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
                 (let ((tramp (%e2ic-mvm-eval-nocache form)))
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
   values (same *mvm-last-mv* protocol as %mvm-eval-run-tuple) so MV parity
   with production mvm-eval holds through the closure boundary."
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
   errors — any nonzero value marks a fresh mvm-eval capability gap.
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
       (error "mvm-eval: interp-closure shape unsupported (cached compile failure)"))
      (c (%e2ic-apply c args))
      (t
       (let ((tramp (%e2ic-compile (cadr fn) (caddr fn) (cadddr fn))))
         (%e2ic-cache-set fn (if tramp tramp (quote :e2ic-fail)))
         (if tramp
             (%e2ic-apply tramp args)
             (progn
               (%e2ic-bump-fallback)
               (error "mvm-eval: interp-closure compile failed (params=~S)"
                      (cadr fn)))))))))

(defun %expand-deftype (type)
  "OVERRIDE (mvm-eval images; last-defun-wins) of ansi-bridge's engine stub:
   route the deftype body eval through the mvm-eval lambda-body entry, cached
   per registration (name → (entry . trampoline)).  A deftype body mvm-eval
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
                        (error "mvm-eval: deftype expander compile failed (type=~S)"
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
                          (error "mvm-eval: deftype expander compile failed (type=~S)"
                                 type))))))))))
