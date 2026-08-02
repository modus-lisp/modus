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
   asking for the interpret fallback even though native code already ran — at
   present only the multiple-values case, where the native MV block cannot yet
   be reproduced into the interpreter's simulated *mvm-last-mv*.  This is the
   one path that still double-executes side effects; it is narrow (single-value
   forms, the overwhelming majority, never take it) and is counted separately
   in *jit-mv-fallback-count* so its true frequency is measurable rather than
   assumed.  Reproducing the MV block from BSS (tagged count at #x10000090,
   extras at #x10000098+) would close it — tracked separately.")

(defvar *jit-mv-fallback-count* nil
  "How many times the MV path forced a re-run (the remaining double-execute).")

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
   placeholder word (imm=0) and ORs in (half << 5), preserving the placeholder's
   Rd + move-wide opcode base — so a li-const/fn-addr site targeting ANY Xd (not
   just x16) patches correctly.  The aarch64 analogue of %jit-write-imm64."
  (let ((k 0))
    (loop
      (when (>= k 4) (return nil))
      (let* ((wo (+ off (* k 4)))
             (w (logior (mem-ref (+ base wo) :u8)
                        (ash (mem-ref (+ base (+ wo 1)) :u8) 8)
                        (ash (mem-ref (+ base (+ wo 2)) :u8) 16)
                        (ash (mem-ref (+ base (+ wo 3)) :u8) 24)))
             (imm (logand (ash word (- (* k 16))) #xFFFF))
             (nw (logior w (ash imm 5))))
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
            (setq ok nil))))
    ok))

(defun %jit-reloc-fn-addrs (base relocs rt-table)
  "Class 2: patch each out-of-module FN-ADDR movabs with the resolved fn's
   TAGGED native word — the SAME object %mvm-resolve-runtime-fn / symbol-function
   returns.  Unlike a CALL target (which is patched UNTAGGED = word-3 because the
   `call rax` jumps to it directly), a FN-ADDR is a VALUE load: the callable
   object flows into eq / eql / funcall, so the imm must be the full tagged word
   (entry|3).  This makes `(eq #'eq (symbol-function 'eq))` T and
   `%ht-canonicalize-test`'s `(eql v (function %eq-fn))` succeed under JIT.
   Returns T if every reloc resolved, NIL if any failed (→ caller falls back)."
  (let ((ok t))
    (dolist (r relocs)
      (let* ((name (gethash (cdr r) rt-table))
             (fn (and name (%mvm-resolve-runtime-fn name)))
             (word (if fn (%val->word fn) 0)))
        (if (> word 0)
            (%jit-write-imm64 base (car r) word)
            (setq ok nil))))
    ok))

(defun %jit-translate-page-1 (bc ft-list rt-table)
  "Inner: translate BC → native x64, mmap an exec page, copy bytes, relocate
   calls + patch consts.  Returns a jit-entry list (base eoff cpatches
   gc-stamp) or NIL if RELOCATION failed.  MAY signal (translator gap) — the
   guard wrapper %jit-translate-page turns any signal into NIL."
  (setq *x64-jit-mode* t)
  (multiple-value-bind (nbuf fn-map) (translate-mvm-to-x64 bc ft-list)
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
            (list base eoff cpatches (%gc-count)))
          nil))))

(defun %jit-translate-page-1-aarch64 (bc mvm-entry ft-list rt-table)
  "WS4-S5 (aarch64) sibling of %jit-translate-page-1.  translate-mvm-to-aarch64
   wants an eql-keyed func-idx→MVM-offset HASH (not x64's (name offset length)
   list), so build it from FT-LIST.  Translate BC under *aarch64-jit-mode*, mmap
   an exec page, copy the native words, relocate out-of-module CALLs (untagged
   callee addr = word-3) + #'NAME fn-addrs (tagged word) + patch li-const quads
   (pool object tagged word), flush the I-cache, and return a jit-entry
   (base eoff nil gc-stamp).  cpatches is NIL: GC is off on aarch64-linux so the
   const-pool never moves and %jit-entry-for's post-GC re-bake never fires.
   Returns NIL if any reloc failed to resolve (→ interpret fallback).  MAY signal
   a translator gap — the %jit-translate-page guard turns it into NIL."
  (setq *aarch64-jit-mode* t)
  (let ((ftbl (make-hash-table :test (quote eql))))
    (let ((i 0)) (dolist (e ft-list) (setf (gethash i ftbl) (cadr e)) (setq i (+ i 1))))
    (multiple-value-bind (nbuf fn-map) (translate-mvm-to-aarch64 bc ftbl)
      (let* ((nwords (a64-buffer-position nbuf))
             (code (a64-buffer-code nbuf))
             (nlen (* nwords 4))
             (crel *aarch64-call-relocs*)
             (frel *aarch64-fn-addr-relocs*)
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
        ;; Quoted-literal / string li-const patches (pool object tagged word).
        (dolist (p cpat)
          (let ((obj (if *e2-const-pool* (gethash (cdr p) *e2-const-pool*) nil)))
            (%jit-write-movz-quad base (car p) (%val->word obj))))
        (%jit-icache-flush base nlen)
        (if (and ok eoff)
            ;; 5th element = PSIZE, so a transient form's page can be munmap'd
            ;; (%jit-free-page base psize) after its native call — see
            ;; %mvm-eval-jit-run's reclamation.
            (list base eoff nil (%gc-count) psize)
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
              (let ((fresh (list (car hit) (cadr hit) (caddr hit) now)))
                (%jit-patch-consts (car fresh) (caddr fresh))
                (setf (gethash bc *jit-page-cache*) fresh)
                fresh)))
        (let ((je (%jit-translate-page bc mvm-entry ft-list rt-table)))
          (when je (setf (gethash bc *jit-page-cache*) je))
          je))))

(defun %mvm-eval-jit-run (bc entry ft-list fn-table rt-table lam-offsets cache-p)
  "WS4-S5b: run compiled module BC as NATIVE JIT'd code, wrapping the result
   like the interpret path (%mvm-wrap-escaping-result).  If the JIT can't build
   a page, OR the form produced MULTIPLE VALUES (native writes real BSS MV-count
   at #x10000090 — a different mechanism from the interpreter's simulated
   *mvm-last-mv*, so we do NOT try to reproduce it), we throw to the caller's
   handler-case which falls back to mvm-interpret.  Single-value forms — the
   bulk of runtime evals — take the fast native path.
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
            (let ((mvc (mem-ref #x10000090 :u64)))
              (unless (eql mvc 1)
                ;; MV form → fall back to interpret; DON'T free (result unknown —
                ;; could be a function; leak the rare MV-form page conservatively).
                ;;
                ;; WS5 #203: this is the ONE remaining path that re-runs a form
                ;; whose side effects already happened.  Native stamped a real
                ;; MV block (tagged count at #x10000090, extras at #x10000098+)
                ;; which we cannot yet translate into the interpreter's
                ;; simulated *mvm-last-mv*, so the only way to produce correct
                ;; VALUES today is to re-interpret.  Flag it so the handler
                ;; allows the fallback despite *jit-native-ran*, and count it
                ;; so the cost is measured rather than assumed.
                (setq *jit-infra-fallback* t)
                (setq *jit-mv-fallback-count*
                      (if *jit-mv-fallback-count* (+ 1 *jit-mv-fallback-count*) 1))
                (error "jit-mv-fallback")))
            ;; SINGLE value: clear the interpreter's simulated MV state so a
            ;; STALE *mvm-last-mv* from a prior interpret run doesn't leak into
            ;; this form's return (the seam reads *mvm-last-mv* after us).
            (setq *mvm-last-mv* nil)
            (setq *jit-native-count*
                  (if *jit-native-count* (+ 1 *jit-native-count*) 1))
            (let ((result (%mvm-wrap-escaping-result raw bc fn-table rt-table lam-offsets)))
              ;; Reclaim the (uncached) aarch64 page unless the RESULT is a
              ;; function/closure whose code is in it.  car(cddddr je) = PSIZE.
              (when (and aa64 (car (cddddr je)) (not (functionp result)))
                (%jit-free-page (car je) (car (cddddr je))))
              result)))
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
                    (handler-case (%mvm-eval-jit-run %bc %entry %ftl %fnt %rt %lam t)
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
                               ;;  (2) the MV sentinel — known double-execute;
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
      (setq *e2-persist-defuns* %e2pd-saved))
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
                ;; NEVER record offset 0: at the native bridge a DATA fixnum 0
                ;; argument (make-list 0 / member 0 / (- 10 j)=0 in nsubstitute's
                ;; bounds loops) is indistinguishable from a lambda-at-offset-0,
                ;; and wrapping it into a #x52 trampoline corrupted the callee
                ;; (make-list "non-negative fixnum", remove returned input
                ;; unchanged).  %mvm-lambda-offset-p has the matching read-side
                ;; guard; data-0 priority is correct since the first module
                ;; function is always a non-lambda (defun / %MVM-EVAL-THUNK).
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
                            ;; the MV sentinel — but NOT on a user condition
                            ;; raised once native code is running.
                            ;; %jit-active-p honors *jit-inhibit* (Class 3).
                            (if (%jit-active-p)
                                (handler-case
                                    (%mvm-eval-jit-run bc entry (reverse ft-list)
                                                    fn-table rt-table lam-offsets nil)
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
