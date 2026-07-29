;;;; build-ansi-common-aarch64.lisp — shared ANSI CL test-runner harness (AArch64)
;;;;
;;;; Loaded by the two AArch64 ANSI gate runners AFTER each sets the target
;;;; flag *ANSI-TARGET-BARE-METAL*:
;;;;   build-aarch64-linux.lisp  (nil) — Linux AArch64 ELF gate
;;;;   build-aarch64.lisp        (t)   — bare-metal AArch64 (QEMU virt)
;;;;
;;;; Holds everything the two share: load the MVM system, read the first-party
;;;; + mvm-eval sources, all build-time transforms, and load the ANSI corpus into
;;;; *real-ansi-sources* / *ansi-file-names*.  Each wrapper appends its OWN
;;;; target-specific tail (runner-source assembly + build-image), and the bare
;;;; wrapper additionally defines the MODUS_NET_BUILD net-driver source (that
;;;; feature is bare-metal only).  Only prefix divergence — *mcgc-pin-source* —
;;;; is parameterized below on the target flag.  See CLAUDE.md "Build taxonomy"
;;;; and build-ansi-common-x64.lisp (the x64 sibling).

(declaim (special *ansi-target-bare-metal*))

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building ANSI CL test runner ===~%")

;;; ============================================================
;;; 2. Read source files (SBCL-side)
;;; ============================================================

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  "Read a first-party source file as text, verifying it parses cleanly
   first. A paren mismatch here fails fast at the specific file instead
   of getting silently skipped later during the concatenated compile."
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    (read-file-text path)))

(defvar *prelude-source* (mvm-text "mvm/prelude.lisp"))
(defvar *gc-source*      (mvm-text "mvm/gc.lisp"))
;; MCGC stage-4d pin API.  Bare metal has no mmap page pool, so it is always
;; empty there; on Linux it is included only when MODUS_MCGC_PINNING=1 (the
;; leading/trailing newline keeps the flag-off binary byte-identical to
;; canonical — the surrounding concatenate needs no extra separator).
(defvar *mcgc-pin-source*
  (if *ansi-target-bare-metal*
      ""
      (let ((v (sb-ext:posix-getenv "MODUS_MCGC_PINNING")))
        (if (and v (plusp (length v)) (not (string= v "0")))
            (concatenate 'string (string #\Newline)
                         (mvm-text "mvm/mcgc-pin.lisp") (string #\Newline))
            ""))))
(defvar *rt-source*      (mvm-text "mvm/rt.lisp"))
(defvar *bridge-source*
  (concatenate 'string
    ;; Load order matches original ansi-bridge.lisp concatenation order.
    ;; cl-sequences first because floatp-impl is needed by the printer.
    (mvm-text "mvm/cl-sequences.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-streams.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-fileio.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-printer.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-reader.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-eval.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-clos.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-types.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-packages.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-conditions.lisp")
    (string #\Newline)
    (mvm-text "mvm/ansi-bridge.lisp")))
;; WS3 STEP 4b (2026-07-09): mvm/tree-walker.lisp is NO LONGER part of this
;; image — production eval is mvm-eval only.  The full-corpus + gauntlet census
;; measured ZERO %e2ic walker-fallback hits (the earlier "-142 fallback
;; inventory" was the :li-func offset-0 phantom, fixed in a07fe7d), and the
;; walker-free image gates clean (16335-16336 / CHUNK-CRASH=0 / FILE-WEDGE=30,
;; gauntlet 243/243 x2 at 11 FAILFORMs).  tree-walker.lisp remains the eval
;; engine of the four legacy fork builds ONLY.  If %e2ic-compile ever fails on
;; a new shape it signals honestly (UNDEFINED-FUNCTION via the NIL fn sentinel).
(defvar *test-source*    (mvm-text "mvm/ansi-tests.lisp"))

;;; --- WS3: self-host the MVM compiler in the image (mvm-eval foundation) ---
;;; Replicates build-generic.lisp's STAGE-1 (ISA + bytecode interpreter) and
;;; STAGE-2 (compiler + in-image float-bit override + opcode table + mvm-eval)
;;; source blocks so the ANSI image can compile-and-interpret a form at
;;; runtime.  At this stage mvm-eval is DEAD CODE (nothing routes to it); the
;;; 32-shard gate must stay unchanged.  Order matters: loaded AFTER
;;; *bridge-source* (needs %prim-aref, the CL runtime) — see *full-source*.
(defvar *isa-source*      (mvm-text "mvm/mvm.lisp"))      ; opcode/vreg constants + structs
(defvar *interp-source*   (mvm-text "mvm/interp.lisp"))   ; mvm-interpret (bytecode executor)
(defvar *compiler-image-source* (mvm-text "mvm/compiler.lisp")) ; the 3-phase MVM compiler
(defvar *mvm-eval-source*    (mvm-text "mvm/mvm-eval.lisp"))    ; mvm-eval-forms / mvm-eval
;; In-image override of the host-only ieee-float-* / bignum-literal helpers
;; (compiler.lisp uses sb-kernel:double-float-*).  Appended AFTER the compiler
;; source so it wins (last-defun).  Verbatim from build-generic.lisp.
(defvar *stage2-float-override* "
(defun ieee-float-bits (f)
  (logior (ash (logand (%prim-aref f 0) 4294967295) 32)
          (logand (%prim-aref f 1) 4294967295)))
(defun ieee-float-hi32 (f) (logand (%prim-aref f 0) 4294967295))
(defun ieee-float-lo32 (f) (logand (%prim-aref f 1) 4294967295))
(defun %lit-bignum-big-p (value) (big-bignum-p value))
(defun %lit-bn-lo (value) (bignum-lo value))
(defun %lit-bn-hi (value) (bignum-hi value))
(defun %lit-bb-sign (value) (%bb-sign value))
(defun %lit-bb-nlimbs (value) (%bb-nlimbs value))
(defun %lit-bb-limb (value k) (%bb-limb value k))
;; In-image override of %GLOBAL-NAME-KEY (the SYMBOL-VALUE / SET-SYMBOL-VALUE
;; alist key the mvm-eval compiler emits for a GLOBAL variable read/write).  The
;; host/native version is NORMALIZE-NAME = compute-name-hash(symbol-name sym).
;; In the image, symbol-name reverse-resolves a native #x50/#x53 sym via the
;; build-generated *sym-name-table*, which only covers SCANNED sources.  A
;; symbol from an UNSCANNED ANSI test dir (e.g. *cons-test-4* from cons/cxr.lsp)
;; has no reverse entry, so symbol-name returns \"\" and compute-name-hash(\"\")
;; is a CONSTANT wrong key — mvm-eval's global read then missed the store (keyed
;; by the symbol's REAL reader/setq-assigned hash) and returned NIL, so the cxr
;; tests cons.38-53 signalled inside CAAAAR..CDDDDR (E2-UNSUP) where the tree-
;; walker (which keys symbol-value by the symbol's stored hash slot) returned
;; the value.  Read the symbol object's stored hash (slot 0) directly here --
;; authoritative and always present.  SCOPE: global var read/write key ONLY;
;; compile-quote symbol interning still uses NORMALIZE-NAME, so quoted-symbol
;; identity (and the type-name registry it feeds) is untouched.
(defun %global-name-key (sym)
  (cond
    ((integerp sym) sym)
    ((symbolp sym)
     (let ((h (cond
                ((null sym) nil) ((eq sym t) nil)
                ((fixnump sym) nil) ((characterp sym) nil)
                ((stringp sym) nil) ((consp sym) nil)
                (t (let ((st (obj-subtag sym)))
                     (if (or (= st 80) (= st 83)) (aref sym 0) nil))))))
       (if h h (normalize-name sym))))
    ((stringp sym) (compute-name-hash sym))
    (t 0)))
")
;; Generated boot-time populator for *opcode-table* (the host *opcode-table*
;; is populated when mvm.lisp/compiler.lisp load host-side for macro scanning).
(defvar *opcode-table-init-source*
  (with-output-to-string (s)
    (format s "(defun %~A-opcode-table ()~%" "populate")
    (maphash (lambda (code info)
               (format s "  (setf (gethash ~D *opcode-table*) (make-opcode-info :code ~D :name ~S :operands (quote ~S) :description ~S))~%"
                       code code
                       (modus.mvm::opcode-info-name info)
                       (modus.mvm::opcode-info-operands info)
                       (modus.mvm::opcode-info-description info)))
             modus.mvm::*opcode-table*)
    (format s "  t)~%")
    (format s "(defparameter *%opcode-table-ready* (progn (%populate-opcode-table) t))~%")))
;;; WS4-AA64 STAGE 1: bake the AArch64 MVM→native translator into the image so
;;; TRANSLATE-MVM-TO-AARCH64 compiles in-image and is runtime-callable — the
;;; foundation for the AArch64 runtime JIT (mirrors build-ansi-common-x64's
;;; Stage-1 baking of x64-asm + translate-x64).  DEAD CODE at this stage:
;;; nothing routes to it, so the gate must stay unchanged.
(defvar *translate-aarch64-source* (mvm-text "mvm/translate-aarch64.lisp"))
;; Shrink the a64-buffer code array default (16M general slots ≈ 128MB tagged)
;; to 64K — a64-emit grows it on demand (%code-buffer doubling); a 128MB alloc
;; per make-a64-buffer would exhaust the ANSI image semispace.
(let ((needle "(code (make-array 16777216))")
      (repl   "(code (make-array 65536))"))
  (let ((p (search needle *translate-aarch64-source*)))
    (unless p (error "WS4-AA64-S1: could not find a64-buffer 16M code default to shrink"))
    (setf *translate-aarch64-source*
          (concatenate 'string (subseq *translate-aarch64-source* 0 p) repl
                       (subseq *translate-aarch64-source* (+ p (length needle)))))))
;; Strip install-aarch64-translator and the host-only ELF/target-descriptor/
;; disassemble tail after it (refs *target-aarch64*, &key disassemble — not
;; compilable in-image).  Same idea as translate-x64's install-x64-translator.
(let ((marker "(defun install-aarch64-translator"))
  (let ((pos (search marker *translate-aarch64-source*)))
    (unless pos (error "WS4-AA64-S1: could not find install-aarch64-translator strip marker"))
    (setf *translate-aarch64-source* (subseq *translate-aarch64-source* 0 pos))))
;; Co-init: *a64-vreg-to-phys* is a defparameter init-thunk (NOT run at boot —
;; CLAUDE.md item 7), so populate it explicitly from %init-aarch64-translator.
;; Values are the vreg-index → aarch64-phys-reg map from translate-aarch64.lisp
;; (V0-V3→x0-x3, V4-V8→x19-x23, VR→x0, VA→x24, VL→x25, VN→x26, VSP→sp, VFP→x29;
;; V9-V15 + V22 spill = nil).  Raw numbers (defconstants don't fold in-image);
;; spill slots set to nil explicitly (aarch64 alloc-array does NOT zero-init).
(defvar *aarch64-translator-coinit-source* "
(defun %init-aarch64-translator ()
  (let ((map (make-array 23)))
    (aset map 0 0) (aset map 1 1) (aset map 2 2) (aset map 3 3)
    (aset map 4 19) (aset map 5 20) (aset map 6 21) (aset map 7 22) (aset map 8 23)
    (aset map 9 nil) (aset map 10 nil) (aset map 11 nil) (aset map 12 nil)
    (aset map 13 nil) (aset map 14 nil) (aset map 15 nil)
    (aset map 16 0) (aset map 17 24) (aset map 18 25) (aset map 19 26)
    (aset map 20 31) (aset map 21 29) (aset map 22 nil)
    (setq *a64-vreg-to-phys* map))
  ;; *mvm-label-counter* is a (defvar … 0) whose init-thunk does NOT run at boot
  ;; (CLAUDE.md item 7) → nil at runtime; translate-aarch64's (incf …) would
  ;; crash.  The compiler doesn't use it, so no earlier path initialised it.
  (when (null *mvm-label-counter*) (setq *mvm-label-counter* 0))
  ;; CRITICAL for the JIT: *aarch64-stack-align-16* gates :push/:pop codegen.
  ;; nil (its runtime default) emits the bare-metal `str [sp,#-8]!` / `ldr
  ;; [sp],#8` which MISALIGNS SP to 8-mod-16 → Linux EL0 SP-alignment fault
  ;; (SIGBUS) on the next SP access.  The normal build sets it t host-side
  ;; (build-aarch64-linux.lisp:1445) but that doesn't reach runtime (item 7),
  ;; so set it here for JIT-translated code.  *aarch64-linux-mode* likewise so
  ;; any TRAP codegen emits Linux syscalls.
  (setq *aarch64-stack-align-16* t)
  (setq *aarch64-linux-mode* t)
  t)
")
;; The assembled self-host block, spliced into *full-source* after the bridge.
(defvar *compiler-in-image-source*
  (concatenate 'string
    *isa-source*      (string #\Newline)
    *interp-source*   (string #\Newline)
    *compiler-image-source* (string #\Newline)
    *stage2-float-override* (string #\Newline)
    *opcode-table-init-source* (string #\Newline)
    *mvm-eval-source*    (string #\Newline)
    ;; WS4-AA64 STAGE 1: translator + co-init (DEAD CODE — nothing calls it yet).
    *translate-aarch64-source* (string #\Newline)
    *aarch64-translator-coinit-source* (string #\Newline)))

;;; ============================================================
;;; WS4-AA64 FLIP knob: default runtime-JIT on/off for the aarch64 image
;;; ============================================================
;;; Mirrors build-generic-cli.lisp's *jit-on* logic: MODUS_NO_JIT → off,
;;; MODUS_USE_JIT → explicit, else *aarch64-jit-default*.  When ON, the wrapper
;;; bakes *aarch64-jit-flip-source* (LAST, so its %jit-enabled-p wins over
;;; mvm-eval.lisp's base) and kernel-main calls (%aa64-jit-boot-init), so
;;; production mvm-eval JITs native via the Stage-5 seam.  When OFF the boot
;;; init is an inert no-op and %jit-enabled-p stays the base (interpret) version
;;; — byte-neutral vs pre-flip.  x64 is untouched (this is aarch64-only source).
(defvar *aarch64-jit-default* nil
  "Default runtime-JIT state when neither MODUS_NO_JIT nor MODUS_USE_JIT is set.
   nil = interpret (current behavior); the knob exposes the JIT via
   MODUS_USE_JIT=1 for validation.  The FLIP commit sets this t once the JIT-on
   gate is proven to survive the corpus + be pass-neutral on real hardware.")
(defvar *aarch64-jit-on*
  (let ((no (sb-ext:posix-getenv "MODUS_NO_JIT"))
        (v  (sb-ext:posix-getenv "MODUS_USE_JIT")))
    (cond ((and no (> (length no) 0)) nil)
          ((and v (> (length v) 0))
           (or (string= v "1") (string-equal v "t") (string-equal v "yes")))
          (t *aarch64-jit-default*))))
(format t "~%  AArch64 runtime JIT: ~A~%" (if *aarch64-jit-on* "ON" "OFF (interpret)"))
;; Baked LAST in *full-source* (after driver) so %jit-enabled-p wins (last-defun).
;; %aa64-jit-boot-init runs the translator co-init (vreg map, stack-align-16,
;; linux-mode, label counter) and selects the aarch64 JIT back-end; the Stage-5
;; seam sets *aarch64-jit-mode* per translation itself.
(defvar *aarch64-jit-flip-source*
  (if *aarch64-jit-on*
      "
(defun %jit-enabled-p () t)
(defun %aa64-jit-boot-init ()
  (%init-aarch64-translator)
  (setq *jit-target-arch* :aarch64)
  t)
"
      "
(defun %aa64-jit-boot-init () nil)
"))

;;; --- Gap A: symbol-function table auto-registration ---
;;;
;;; cl-eval.lisp's %init-sft-list is a hand-curated allowlist (~229
;;; entries).  Any defun not on it is unreachable by runtime EVAL: probes
;;; 56303/56304 fail because write-string-serial isn't on the list.
;;; Conformance — i.e. (eval form) for any form a Common Lisp programmer
;;; would write — requires every defun'd function to be addressable by
;;; name.  Walking the source via SBCL's reader on the build host gives
;;; us the complete name list cheaply; we then emit %init-sft-auto with
;;; (puthash NAME ht #'NAME) for each name and call it from the driver.
;;;
;;; Only plain `(defun NAME args body)` is collected:
;;;   - `(defun (setf NAME) ...)` setf functions are skipped (#'(setf …)
;;;     doesn't go through the SFT today; will revisit if it gates probes).
;;;   - defmacro/defgeneric/defmethod/defstruct/defclass are skipped —
;;;     they don't produce a callable by the bare symbol name (macros) or
;;;     have their own dispatch (CLOS).
;;;
;;; Chunking: a single puthash-heavy defun blows past the codegen-size
;;; threshold (the run-ansi-lambda class — see CLAUDE.md).  We split into
;;; CHUNK-sized %init-sft-auto-N defuns called from %init-sft-auto.

(defun %scan-defun-names-host (source-str)
  "Walk SOURCE-STR via the SBCL reader, return the upcased name-strings
   of every top-level `(defun NAME ...)` form.  Returns names as strings
   (the SFT key type) in source order; duplicates kept (last-defun-wins
   is desirable here — the generated puthash list will re-register the
   final binding, matching Modus's runtime behavior)."
  (let ((names nil)
        (eof (list :eof)))
    (with-input-from-string (s source-str)
      (loop
        (let ((form (handler-case (read s nil eof)
                      ;; Reader errors (e.g. #+sbcl feature exprs we don't
                      ;; resolve identically, malformed #. forms): skip
                      ;; that form and continue scanning.  This is just
                      ;; defun-name discovery; ignoring is safe.
                      (error () (return)))))
          (when (eq form eof) (return))
          (when (and (consp form)
                     (eq (car form) 'defun)
                     (symbolp (cadr form)))   ; skip (defun (setf X) …)
            (push (symbol-name (cadr form)) names)))))
    (nreverse names)))

(defun %generate-sft-auto-source (names &key (chunk 120))
  "Emit Lisp source text for %init-sft-auto-N (chunked) + the master
   %init-sft-auto that calls them all and remirrors the native-sym table.
   NAMES is a list of upcased name strings."
  ;; Dedup while preserving last-occurrence order (matches last-defun-wins).
  (let* ((seen (make-hash-table :test 'equal))
         (uniq (let ((rev nil))
                 (dolist (n (reverse names))
                   (unless (gethash n seen)
                     (setf (gethash n seen) t)
                     (push n rev)))
                 rev))
         (n-chunks 0)
         (out (with-output-to-string (o)
                (let ((cur uniq))
                  (loop
                    (when (null cur) (return))
                    (incf n-chunks)
                    (format o "(defun %init-sft-auto-~D ()~%" n-chunks)
                    (format o "  (let ((ht *symbol-function-table*))~%")
                    (let ((k 0))
                      (loop
                        (when (or (null cur) (>= k chunk)) (return))
                        (format o "    (puthash ~S ht #'~A)~%"
                                (car cur) (car cur))
                        (setq cur (cdr cur))
                        (incf k)))
                    (format o "    nil))~%")))
                (format o "(defun %init-sft-auto ()~%")
                (let ((c 0))
                  (loop
                    (incf c)
                    (when (> c n-chunks) (return))
                    (format o "  (%init-sft-auto-~D)~%" c)))
                (format o "  (when *native-sym-function-table*~%")
                (format o "    (%nsft-populate-from *symbol-function-table*))~%")
                (format o "  nil)~%"))))
    (values out (length uniq) n-chunks)))

(defvar *sft-auto-source*
  (let ((all-names (append
                     (%scan-defun-names-host *prelude-source*)
                     (%scan-defun-names-host *gc-source*)
                     (%scan-defun-names-host *mcgc-pin-source*)
                     (%scan-defun-names-host *rt-source*)
                     (%scan-defun-names-host *bridge-source*))))
    (multiple-value-bind (src count chunks)
        (%generate-sft-auto-source all-names)
      (format t "  SFT auto-init: ~D unique defun names across ~D chunk(s)~%"
              count chunks)
      src)))

;;; -----------------------------------------------------------------
;;; Symbol-name reverse table (hash → name).
;;;
;;; Native MVM symbols (subtag #x50, 1-slot hash-only) have NO name
;;; slot.  symbol-name walks all package symtabs looking for a matching
;;; hash; if not found returns "".  This breaks tests that compute
;;; symbol-name of arbitrary symbols, and breaks symbol-keyed macro
;;; registration.
;;;
;;; Fix: at build time scan every source file for every SYMBOL that
;;; appears anywhere (defun names, quoted refs, function calls, ...)
;;; and emit %init-sym-name-auto that puts (compute-name-hash NAME, NAME)
;;; into *sym-name-table* at boot.  symbol-name then consults that
;;; table for native syms.

(defun %scan-symbol-names-host (source-str &optional (%pkg nil))
  "Walk SOURCE-STR via SBCL reader, recursively collecting every SYMBOL
   that appears in any form.  Returns a hash-table of upcased name
   strings → T.

   %PKG (optional): bind *package* to it for the read.  The WS3
   self-host block (*compiler-in-image-source*) must be read under
   :modus.mvm — interp.lisp contains #.+op-nop+ READ-TIME EVALS whose
   constants live in :modus.mvm.  Under a :cl-user read, the first #.
   hit an unbound +OP-NOP+, the read errored, and the
   `(error () (return))` ABORTED THE WHOLE SCAN — so everything
   concatenated after interp.lisp (compiler.lisp, mvm-eval.lisp) silently
   contributed NO names.  Compiler-backquote literals like
   %SETF-MEM-REF then had no *sym-name-table* entry, in-image
   SYMBOL-NAME returned \"\" for them, and mvm-eval's compile of any
   (setf (mem-ref …) …) — i.e. any (values …) form — mis-dispatched
   into an unresolvable call (the WS3 flip MV-cluster hang).
   All other sources keep the historical default-package read."
  (let ((names (make-hash-table :test 'equal))
        (eof (list :eof))
        (*package* (if %pkg (or (find-package %pkg) *package*) *package*)))
    (labels ((walk (f)
               (cond
                 ((symbolp f)
                  (let ((n (symbol-name f)))
                    (when (and n (> (length n) 0))
                      (setf (gethash n names) t))))
                 ((consp f)
                  (walk (car f))
                  (walk (cdr f)))
                 (t nil))))
      (with-input-from-string (s source-str)
        (loop
          (let ((form (handler-case (read s nil eof) (error () (return)))))
            (when (eq form eof) (return))
            (handler-case (walk form) (error () nil))))))
    names))

(defun %generate-sym-name-auto-source (name-hash-table &key (chunk 200))
  "Emit Lisp source for chunked %init-sym-name-auto-N defuns + master
   %init-sym-name-auto.  NAME-HASH-TABLE is hash of name strings → T."
  (let* ((uniq nil)
         (n-chunks 0))
    (maphash (lambda (k v) (declare (ignore v)) (push k uniq)) name-hash-table)
    (let ((out (with-output-to-string (o)
                 (let ((cur uniq))
                   (loop
                     (when (null cur) (return))
                     (incf n-chunks)
                     (format o "(defun %init-sym-name-auto-~D ()~%" n-chunks)
                     (format o "  (let ((ht *sym-name-table*))~%")
                     (let ((k 0))
                       (loop
                         (when (or (null cur) (>= k chunk)) (return))
                         (let* ((name (car cur))
                                (h (modus.mvm::compute-name-hash name)))
                           (format o "    (puthash ~D ht ~S)~%" h name))
                         (setq cur (cdr cur))
                         (incf k)))
                     (format o "    nil))~%")))
                 (format o "(defun %init-sym-name-auto ()~%")
                 (let ((c 0))
                   (loop
                     (incf c)
                     (when (> c n-chunks) (return))
                     (format o "  (%init-sym-name-auto-~D)~%" c)))
                 (format o "  nil)~%"))))
      (values out (hash-table-count name-hash-table) n-chunks))))

;; -----------------------------------------------------------------
;; Runtime macro-table populator.
;;
;; mvm/compiler.lisp's register-mvm-bootstrap-macros lives ONLY on the
;; SBCL build host — it isn't part of *bridge-source* and mvm-define-
;; macro is undefined at runtime.  But the lambda expansion bodies
;; ARE valid Modus source: they use backquote / list / cons / car etc.
;; So we WALK compiler.lisp at build time, find each
;; (mvm-define-macro NAME EXPANDER) form, and EMIT a runtime puthash
;; call with the expander LAMBDA as a fresh Modus source form.  The
;; runtime then compiles the lambdas into real closures and stores
;; them in *macro-table* keyed by compute-name-hash(NAME).

(defun %scan-mvm-define-macro-forms (source-str)
  "Walk SOURCE-STR via SBCL reader (bound into :modus.mvm so its symbols
   resolve correctly), collect every (mvm-define-macro NAME EXPANDER)
   form.  Returns a list of (NAME-STR EXPANDER-FORM) pairs."
  (let ((found nil)
        (eof (list :eof))
        (modus-pkg (find-package :modus.mvm)))
    (labels ((walk (form)
               (cond
                 ((not (consp form)) nil)
                 ((and (symbolp (car form))
                       ;; Accept either fully-qualified modus.mvm::mvm-define-macro
                       ;; OR any other home package — the suite source can be
                       ;; read from either :cl-user or :modus.mvm.  Name match
                       ;; is sufficient since "MVM-DEFINE-MACRO" is unique.
                       (string= (symbol-name (car form)) "MVM-DEFINE-MACRO")
                       (consp (cdr form)) (stringp (cadr form))
                       (consp (cddr form)))
                  (push (list (cadr form) (caddr form)) found))
                 (t (dolist (sub form) (walk sub))))))
      (let ((*package* modus-pkg))   ;; symbols read into :modus.mvm
        (with-input-from-string (s source-str)
          (loop
            (let ((form (handler-case (read s nil eof) (error () (return)))))
              (when (eq form eof) (return))
              (handler-case (walk form) (error () nil)))))))
    (nreverse found)))

(defun %generate-runtime-macro-init (pairs &key (chunk 5))
  "Emit a (defun %init-runtime-macros ...) that puthashes each macro's
   expander LAMBDA into *macro-table*.  Chunked so any one defun stays
   small enough to compile cleanly."
  (let* ((n-chunks 0)
         (out (with-output-to-string (o)
                (let ((cur pairs))
                  (loop
                    (when (null cur) (return))
                    (incf n-chunks)
                    (format o "(defun %init-runtime-macros-~D ()~%" n-chunks)
                    (let ((k 0))
                      (loop
                        (when (or (null cur) (>= k chunk)) (return))
                        (let* ((p (car cur))
                               (name (first p))
                               (expander-src (second p)))
                          ;; 5 macros (DECF/DOLIST/POP/PROG1/PUSH) are defined
                          ;; twice in compiler.lisp; last-wins via hash overwrite.
                          ;; Net unique = 70.
                          (handler-case
                            (let ((*package* (find-package :modus.mvm)))
                              (format o "  (puthash ~D *macro-table* ~S)~%"
                                      (modus.mvm::compute-name-hash name)
                                      expander-src))
                            (error () nil)))
                        (setq cur (cdr cur))
                        (incf k)))
                    (format o "  nil)~%")))
                (format o "(defun %init-runtime-macros ()~%")
                (format o "  (setq *macro-table* (make-hash-table))~%")
                (let ((c 0))
                  (loop
                    (incf c)
                    (when (> c n-chunks) (return))
                    (format o "  (%init-runtime-macros-~D)~%" c)))
                (format o "  nil)~%"))))
    (values out (length pairs) n-chunks)))

(defvar *runtime-macros-auto-source*
  (let ((pairs (%scan-mvm-define-macro-forms (mvm-text "mvm/compiler.lisp"))))
    (multiple-value-bind (src count chunks)
        (%generate-runtime-macro-init pairs)
      (format t "  runtime macros: ~D mvm-define-macro entries across ~D chunk(s)~%"
              count chunks)
      src)))

(defun %scan-ansi-test-dir-host (dir)
  "Walk every *.lsp file in DIR (one chapter directory) with the SBCL
   reader and harvest symbol names, just like %scan-symbol-names-host but
   from disk.  Used to ensure ANSI test-file symbols (e.g. literal
   `'|XYZ|') have their names populated in *sym-name-table* so the
   printer's SYMBOL-NAME doesn't return \"\" for them."
  (let ((tbl (make-hash-table :test 'equal)))
    (when (probe-file dir)
      (dolist (f (directory (concatenate 'string dir "*.lsp")))
        (handler-case
            (let ((src (with-output-to-string (o)
                         (with-open-file (s f :direction :input)
                           (loop for line = (read-line s nil :eof)
                                 until (eq line :eof)
                                 do (write-line line o))))))
              (let ((found (%scan-symbol-names-host src)))
                (maphash (lambda (k v) (declare (ignore v))
                           (setf (gethash k tbl) t))
                         found)))
          (error () nil))))
    tbl))

;; Builds the %init-sym-name-auto source from EXTRA-SOURCES (a list of source
;; strings to scan in addition to the always-scanned first-party + ANSI-test
;; symbols).  Factored into a function so it can be called AFTER *driver-source*
;; is defined — the driver carries quoted symbol literals (e.g. the mvm-eval
;; self-check's `(defun sq …)`) whose names SYMBOL-NAME must be able to recover;
;; without scanning the driver those symbols reverse-map to "" and mvm-eval's
;; defun-registration keys every such function under the empty string → name
;; collision → in-module cross-call resolves to the wrong (last) function →
;; infinite self-recursion.  (Reader-interned symbols already work via the
;; package-symtab fallback; this closes the build-literal gap.)
(defun %build-sym-name-auto-source (extra-sources &optional modus-pkg-sources)
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (src (append (list *prelude-source* *gc-source* *mcgc-pin-source*
                               *rt-source* *bridge-source* *test-source*)
                         extra-sources))
      (let ((found (%scan-symbol-names-host src)))
        (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k tbl) t))
                 found)))
    ;; Sources that must be READ under :modus.mvm (see %scan-symbol-names-host).
    (dolist (src modus-pkg-sources)
      (let ((found (%scan-symbol-names-host src :modus.mvm)))
        (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k tbl) t))
                 found)))
    ;; Also scan ANSI test files so test-only symbol literals (e.g.
    ;; `'|XYZ|' from print-symbols.lsp) have their name strings in
    ;; *SYM-NAME-TABLE* — otherwise SYMBOL-NAME returns "" for them
    ;; and the printer emits empty output, failing CLHS §22.1.3.3
    ;; symbol-printing tests.
    (dolist (d '("/home/claude/modus/tmp/ansi-test/printer/"
                 "/home/claude/modus/tmp/ansi-test/printer/format/"
                 "/home/claude/modus/tmp/ansi-test/symbols/"
                 "/home/claude/modus/tmp/ansi-test/packages/"
                 "/home/claude/modus/tmp/ansi-test/reader/"
                 "/home/claude/modus/tmp/ansi-test/auxiliary/"
                 ;; objects/: CLOS-only literals like the keyword
                 ;; :ARGUMENT-PRECEDENCE-ORDER otherwise have no
                 ;; reverse name (SYMBOL-NAME returns "") and defeat
                 ;; %validate-defgeneric-options' option matching.
                 "/home/claude/modus/tmp/ansi-test/objects/"))
      (let ((found (%scan-ansi-test-dir-host d)))
        (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k tbl) t))
                 found)))
    (multiple-value-bind (src count chunks)
        (%generate-sym-name-auto-source tbl)
      (format t "  sym-name auto-init: ~D unique symbol names across ~D chunk(s)~%"
              count chunks)
      src)))

;; Forward declaration; the real value is computed after *driver-source* is
;; defined (so the driver's quoted symbols get scanned) — see the SETQ below.
(defvar *sym-name-auto-source* nil)

;; SBCL-level stubs for functions called during macro expansion
(defun notnot (x) (not (not x)))
(defun notnot-mv (x) (not (not x)))
(defun classify-error* (form) nil)
(in-package :modus.mvm)
(defun notnot (x) (not (not x)))
(defun notnot-mv (x) (not (not x)))
(in-package :cl-user)

;; Create SBCL-side packages that ANSI test files reference
;; (needed so SBCL's reader can resolve qualified symbols like DS1:A)
(ignore-errors (delete-package "FS-A"))
(ignore-errors (delete-package "FS-B"))
(ignore-errors (delete-package "FS-Q"))
(ignore-errors (delete-package "DS4"))
(ignore-errors (delete-package "DS3"))
(ignore-errors (delete-package "DS2"))
(ignore-errors (delete-package "DS1"))
(ignore-errors (delete-package "A"))
(ignore-errors (delete-package "B"))
(ignore-errors (delete-package "Q"))
(ignore-errors (delete-package "CL-TEST"))
;; REGRESSION-TEST package: needed so SBCL reader can parse
;; regression-test::my-aref, regression-test::*compile-tests*, etc.
;; in ansi-aux.lsp without signaling "Package does not exist".
(ignore-errors (delete-package "REGRESSION-TEST"))
(ignore-errors (delete-package "RTEST"))
(ignore-errors (delete-package "RT"))
(defpackage "REGRESSION-TEST"
  (:use "CL")
  (:nicknames "RTEST" "RT")
  (:export "MY-AREF" "MY-ROW-MAJOR-AREF" "*COMPILE-TESTS*"
           "*DO-TESTS-WHEN-DEFINED*" "*TEST*" "DEFTEST" "DO-TESTS"
           "PENDING-TESTS" "REM-ALL-TESTS" "REM-TEST"
           "*CATCH-ERRORS*" "*PASSED-TESTS*" "*FAILED-TESTS*"))
(defpackage "A" (:use) (:nicknames "Q") (:export "FOO"))
(defpackage "B" (:use "A") (:export "BAR"))
(defpackage "FS-A" (:use) (:nicknames "FS-Q") (:export "FOO"))
(defpackage "FS-B" (:use "FS-A") (:export "BAR"))
(defpackage "DS1" (:use) (:intern "C" "D") (:export "A" "B"))
(defpackage "DS2" (:use) (:intern "E" "F") (:export "G" "H" "A"))
(defpackage "DS3"
  (:shadow "B")
  (:shadowing-import-from "DS1" "A")
  (:use "DS1" "DS2")
  (:export "A" "B" "G" "I" "J" "K")
  (:intern "L" "M"))
(defpackage "DS4"
  (:shadowing-import-from "DS1" "B")
  (:use "DS1" "DS3")
  (:intern "X" "Y" "Z")
  (:import-from "DS2" "F"))
(defpackage "CL-TEST" (:use "CL"))
;; LISP package: the Paul Dietz suite (e.g. iteration/loop7.lsp) refers to
;; standard operators as `lisp:intern`, `lisp:export`, etc.  SBCL has no
;; LISP package, so reading those files used to TRUNCATE at the first
;; `lisp:`-qualified form — silently dropping the package-setup forms
;; (intern/export of the symbols the test then iterates).  Define LISP as
;; a CL-using package that re-exports every external CL symbol, so
;; `lisp:intern` reads as (and is EQ to) `cl:intern`.
(ignore-errors (delete-package "LISP"))
(defpackage "LISP" (:use "CL"))
(let ((lisp-pkg (find-package "LISP")))
  (do-external-symbols (s (find-package "CL"))
    (export s lisp-pkg)))

;; SBCL-side CLOS class registry for make-instance initarg expansion
;; Each entry: (class-name slot-names . initarg-map)
;; where initarg-map = list of (initarg-string . slot-symbol)
(defvar *sbcl-clos-classes* nil)

;; Stubs for variables defined in test source files we don't load
;; (cl-symbol-names.lsp etc.).  Without these stubs, files that
;; reference them at SBCL load-time (e.g. class-precedence-lists.lsp)
;; get SKIP'd entirely.
(defvar *cl-types-that-are-classes-symbols* nil)
(defvar *cl-type-symbols* nil)
(defvar *cl-symbol-names* nil)

;; Counter for generating unique slot-unbound method function names
(defvar *slot-unbound-method-counter* 0)

;; Helper: safe mapcar that handles dotted lists (returns dotted list)
(defun mapcar-dotted (fn list)
  "Like mapcar but handles dotted lists. The dotted cdr is passed through fn."
  (cond
    ((null list) nil)
    ((atom list) (funcall fn list))
    (t (cons (funcall fn (car list))
             (mapcar-dotted fn (cdr list))))))

;; Helper: extract keyword value from plist-style args
(defun make-array-kwarg (args key)
  "Get keyword value from make-array keyword args list."
  (loop for (k v) on args by #'cddr
        when (eq k key) return v))

;; Helper: check if element-type is a character type
(defun char-element-type-p (et)
  "True if element-type is a character type (character, standard-char, base-char, nil)."
  (or (null et)   ; bare nil
      (and (consp et) (eq (car et) 'quote)
           (member (cadr et) '(character standard-char base-char nil)))))

;; Helper: check if element-type was explicitly specified as a character type.
;; Returns NIL when no :element-type kwarg was given (default is T, not char).
(defun explicit-char-element-type-p (et)
  (and (consp et) (eq (car et) 'quote)
       (member (cadr et) '(character standard-char base-char))))

;; Helper: check if any kwarg was explicitly specified (key present in plist).
(defun has-kwarg-p (kwargs key)
  (loop for (k v) on kwargs by #'cddr
        when (eq k key) return t))

;; Helper: check if element-type is BIT
(defun bit-element-type-p (et)
  "True if element-type is 'bit."
  (and (consp et) (eq (car et) 'quote) (eq (cadr et) 'bit)))

;; Helper: filter out unknown kwargs (e.g., :allow-other-keys, :nonsense-argument)
;; from a make-array kwarg list.  Returns a fresh list containing only the
;; kwargs that make-array's rewriter actually recognises.
(defun %filter-make-array-kwargs (kwargs)
  (let ((known '(:element-type :initial-contents :initial-element
                 :adjustable :fill-pointer :displaced-to :displaced-index-offset))
        (out nil))
    (loop for (k v) on kwargs by #'cddr
          when (member k known)
          do (push k out) (push v out))
    (nreverse out)))

;; Rewrite (make-array-with-checks DIMS . OPTS) → (make-array DIMS . FILTERED-OPTS).
;; The aux defun for make-array-with-checks does many CL-conformance checks
;; (typep with complex types, simple-array detection, etc.) that our runtime
;; doesn't fully support — so calling it through apply+#'make-array silently
;; returns garbage.  By rewriting at compile time we let the existing
;; rewrite-make-array-{dims,initcontents} handle the actual array creation.
(defun rewrite-make-array-with-checks (form)
  (cond
    ((atom form) form)
    ((and (eq (car form) 'make-array-with-checks)
          (consp (cdr form)))
     (let* ((dims (cadr form))
            (opts (cddr form))
            (filtered (%filter-make-array-kwargs opts)))
       (rewrite-make-array-with-checks
        (cons 'make-array (cons dims filtered)))))
    (t (mapcar-dotted #'rewrite-make-array-with-checks form))))

;; Rewrite make-array with :initial-contents and/or character :element-type
;; into %make-string-array + aset calls
(defun rewrite-make-array-initcontents (form)
  "Walk form tree, converting make-array with :initial-contents or char :element-type
   into %make-string-array + initialization code.

   Adjustable arrays use a 'wrap-with-marker' convention so adjustable-array-p
   can detect them at runtime:
     adjustable-only:    (cons 8765432 underlying)
     adjustable + fp:    (cons 8765432 (cons fp underlying))
   The marker 8765432 is distinct from the multi-dim marker 9867654 and from
   any plausible fill-pointer value.  fill-pointer-only arrays keep the
   existing (cons fp underlying) layout."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (integerp (cadr form))
          (cddr form))  ; has keyword args
     (let* ((size (cadr form))
            (kwargs (cddr form))
            (et (make-array-kwarg kwargs :element-type))
            (contents (make-array-kwarg kwargs :initial-contents))
            (fill-p-raw (make-array-kwarg kwargs :fill-pointer))
            ;; :fill-pointer t means fp = size; :fill-pointer N is N; :fill-pointer nil means no fp
            (fill-p (cond ((eq fill-p-raw t) size)
                          ((null fill-p-raw) nil)
                          (t fill-p-raw)))
            (adj-p (eq (make-array-kwarg kwargs :adjustable) t))
            (displaced (make-array-kwarg kwargs :displaced-to))
            (disp-offset (or (make-array-kwarg kwargs :displaced-index-offset) 0))
            (char-et (char-element-type-p et))
            ;; Was :element-type explicitly given?  If not, the default is T
            ;; (general object array), and we must NOT route to %make-string-array
            ;; just because (or (null et) ...) was true.
            (et-given (has-kwarg-p kwargs :element-type))
            (explicit-char-et (and et-given (explicit-char-element-type-p et))))
       (cond
         ;; Displaced array: (cons (cons declared-size offset) underlying-string)
         (displaced
          (let ((disp-form (rewrite-make-array-initcontents displaced)))
            `(cons (cons ,size ,disp-offset) ,disp-form)))
         ;; Fill-pointer: (cons fill-pointer underlying-string)
         ((and fill-p (stringp contents))
          (if adj-p
              `(cons 8765432 (cons ,fill-p (copy-seq ,contents)))
              `(cons ,fill-p (copy-seq ,contents))))
         ((and fill-p (integerp contents))
          ;; fill-pointer with non-string contents — unlikely but handle
          (mapcar-dotted #'rewrite-make-array-initcontents form))
         ;; :initial-contents is a string literal — copy it as a string
         ((stringp contents)
          (if adj-p
              `(cons 8765432 (copy-seq ,contents))
              `(copy-seq ,contents)))
         ;; :initial-contents is a quoted list of characters
         ((and (consp contents) (eq (car contents) 'quote)
               (consp (cadr contents)) (characterp (car (cadr contents))))
          (let* ((chars (cadr contents))
                 (var '%str-init-tmp)  ; fixed name, not gensym (survives ~S print+read)
                 (asets (loop for ch in chars for i from 0
                              collect `(aset ,var ,i ,(char-code ch)))))
            (let ((body `(let ((,var (%make-string-array ,size)))
                           ,@asets
                           ,var)))
              (cond
                ((and adj-p fill-p) `(cons 8765432 (cons ,fill-p ,body)))
                (adj-p              `(cons 8765432 ,body))
                (fill-p             `(cons ,fill-p ,body))
                (t                  body)))))
         ;; char element-type, no initial-contents — just %make-string-array,
         ;; optionally filled with :initial-element if provided.
         ;; Only matches when :element-type was EXPLICITLY given as a char type.
         ;; Otherwise the default element-type T means a general object array,
         ;; not a string.
         ((and explicit-char-et (not contents))
          (let* ((init-elem (make-array-kwarg kwargs :initial-element))
                 (body
                   (if init-elem
                       `(%make-string-fill-char ,size
                                                ,(rewrite-make-array-initcontents init-elem))
                       `(%make-string-array ,size))))
            (cond
              ((and adj-p fill-p) `(cons 8765432 (cons ,fill-p ,body)))
              (adj-p              `(cons 8765432 ,body))
              (fill-p             `(cons ,fill-p ,body))
              (t                  body))))
         ;; bit element-type with :initial-contents — array of fixnum 0/1
         ((and (bit-element-type-p et) contents)
          (let ((init-form (rewrite-make-array-initcontents contents)))
            (let ((body `(%make-bit-vector-from-contents ,size ,init-form)))
              (cond
                ((and adj-p fill-p) `(cons 8765432 (cons ,fill-p ,body)))
                (adj-p              `(cons 8765432 ,body))
                (fill-p             `(cons ,fill-p ,body))
                (t                  body)))))
         ;; bit element-type — make a bit vector with :initial-element default 0
         ((bit-element-type-p et)
          (let ((init (or (make-array-kwarg kwargs :initial-element) 0)))
            (let ((body `(make-bit-vector ,size ,init)))
              (cond
                ((and adj-p fill-p) `(cons 8765432 (cons ,fill-p ,body)))
                (adj-p              `(cons 8765432 ,body))
                (fill-p             `(cons ,fill-p ,body))
                (t                  body)))))
         ;; :adjustable t and/or :fill-pointer with no other handler matched
         ;; (general object array path).  Build a fresh array with optional
         ;; :initial-element fill or :initial-contents fill, then wrap.
         ((or adj-p fill-p)
          (let* ((init-elem (make-array-kwarg kwargs :initial-element))
                 (body
                  (cond
                    ((and contents (consp contents) (eq (car contents) 'quote)
                          (consp (cadr contents)))
                     `(%make-array-fill-list ,size ',(cadr contents)))
                    ((and contents (vectorp contents) (not (stringp contents)))
                     `(%make-array-fill-vec ,size ',contents))
                    (contents
                     `(%make-array-fill-vec ,size
                                            ,(rewrite-make-array-initcontents contents)))
                    (init-elem
                     `(%make-array-fill-init ,size
                                             ,(rewrite-make-array-initcontents init-elem)))
                    (t `(make-array ,size)))))
            (cond
              ((and adj-p fill-p) `(cons 8765432 (cons ,fill-p ,body)))
              (adj-p              `(cons 8765432 ,body))
              (fill-p             `(cons ,fill-p ,body)))))
         ;; ---------------------------------------------------------------
         ;; Plain (non-adjustable, no fill-pointer, no displaced) make-array
         ;; with :initial-element or :initial-contents.  Generate a single
         ;; runtime call to %make-array-fill-* helpers (defined in
         ;; ansi-bridge.lisp) instead of per-element asets — this keeps
         ;; the per-test source small enough that it doesn't push the
         ;; enclosing run-ansi-XXX function past the size threshold that
         ;; flips unrelated tests.
         ;; ---------------------------------------------------------------
         ;; :initial-contents is a quoted list literal — keep the literal
         ;; quoted so the runtime helper walks it directly.
         ((and contents (consp contents) (eq (car contents) 'quote)
               (consp (cadr contents)))
          `(%make-array-fill-list ,size ',(cadr contents)))
         ;; :initial-contents is a vector literal #(...) — keep the
         ;; literal quoted so the runtime helper aref's it.
         ((and contents (vectorp contents) (not (stringp contents)))
          `(%make-array-fill-vec ,size ',contents))
         ;; :initial-contents is any other expression (a function call
         ;; producing a list or vector, etc.).  Dispatch at runtime
         ;; via %make-array-fill-any.
         (contents
          `(%make-array-fill-any ,size ,(rewrite-make-array-initcontents contents)))
         ;; :initial-element provided — fill all slots
         ((make-array-kwarg kwargs :initial-element)
          `(%make-array-fill-init ,size
                                  ,(rewrite-make-array-initcontents
                                    (make-array-kwarg kwargs :initial-element))))
         ;; fallback
         (t (mapcar-dotted #'rewrite-make-array-initcontents form)))))
    (t (mapcar-dotted #'rewrite-make-array-initcontents form))))

;; Helper: flatten nested initial-contents list to a flat list, in row-major
;; order.  DIMS is the list of dimensions, CONTENTS is the (already unquoted)
;; nested list literal.
(defun %flatten-initial-contents (dims contents)
  (cond
    ((null dims) (list contents))
    ((null (cdr dims))
     ;; Last dim: contents is a flat sequence of elements
     (if (listp contents) (copy-list contents) nil))
    (t
     ;; contents is a list of length (car dims), each a sub-array
     (let ((acc nil))
       (dolist (sub contents)
         (dolist (e (%flatten-initial-contents (cdr dims) sub))
           (push e acc)))
       (nreverse acc)))))

;; Helper: build the body for a wrapped make-array with optional :initial-element
;; or :initial-contents handling.  DIMS is the dim list (NIL for 0-dim).  TOTAL
;; is the flat-array length (1 for 0-dim).  KWARGS is the original kwarg list.
(defun %build-wrapped-make-array (dims total kwargs)
  (let* ((init-elem (rewrite-make-array-dims
                     (make-array-kwarg kwargs :initial-element)))
         (init-contents (make-array-kwarg kwargs :initial-contents))
         (adj-p (eq (make-array-kwarg kwargs :adjustable) t))
         (var '%mda-init-tmp))
    (let ((md-form
           (cond
             ;; :initial-contents is a quoted nested list literal — flatten it
             ((and init-contents (consp init-contents) (eq (car init-contents) 'quote))
              (let* ((nested (cadr init-contents))
                     (flat (%flatten-initial-contents dims nested))
                     (asets (loop for v in flat for i from 0
                                  collect `(aset ,var ,i (quote ,v)))))
                `(cons 9867654
                       (cons (quote ,dims)
                             (let ((,var (make-array ,total)))
                               ,@asets
                               ,var)))))
             ;; 0-dim array with scalar :initial-contents — treat as the single element
             ((and init-contents (null dims))
              (let* ((iv '%mda-init-val))
                `(cons 9867654
                       (cons (quote ,dims)
                             (let ((,var (make-array ,total))
                                   (,iv ,(rewrite-make-array-dims init-contents)))
                               (aset ,var 0 ,iv)
                               ,var)))))
             ;; :initial-element provided — fill all slots.
             (init-elem
              (let* ((init-var '%mda-init-val)
                     (asets (loop for i from 0 below total
                                  collect `(aset ,var ,i ,init-var))))
                `(cons 9867654
                       (cons (quote ,dims)
                             (let ((,var (make-array ,total))
                                   (,init-var ,init-elem))
                               ,@asets
                               ,var)))))
             ;; No init — just wrap a fresh flat array
             (t
              `(cons 9867654
                     (cons (quote ,dims) (make-array ,total)))))))
      (if adj-p `(cons 8765432 ,md-form) md-form))))

;; Rewrite (make-array '(N) ...) → (make-array N ...) for MVM compatibility
;;
;; For 0-dim and multi-dim arrays we wrap the underlying flat 1-D array in
;; a sentinel cons so the printer / array-dimensions / array-rank can detect
;; the rank.  Wrapper layout:
;;   (cons 9867654 (cons DIMS-LIST FLAT-ARRAY))
;; where DIMS-LIST is a list of integer dimensions (NIL for 0-dim) and
;; FLAT-ARRAY is the underlying 1-D array of (product DIMS-LIST) elements
;; (or 1 element when DIMS-LIST is NIL).
(defun rewrite-make-array-dims (form)
  "Walk form tree, converting list-dimension make-array to integer-dimension.
   Also wraps 0-dim and multi-dim make-array results in a md-array tag cons
   so array-dimensions/array-rank/printer can recover the rank."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'quote)
          (consp (cadr (cadr form)))
          (null (cdr (cadr (cadr form)))))
     ;; (make-array '(N) ...) → (make-array N ...)  [single-dim list]
     (cons 'make-array (cons (car (cadr (cadr form)))
                             (mapcar #'rewrite-make-array-dims (cddr form)))))
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'quote)
          (consp (cadr (cadr form))))
     ;; (make-array '(N M ...) ...) — multi-dim array.  Flatten to a vector
     ;; of (product dims) elements and wrap so we remember the dims.
     (let* ((dims (cadr (cadr form)))
            (total (let ((p 1))
                     (dolist (d dims p) (setq p (* p d)))))
            (kwargs (cddr form)))
       (%build-wrapped-make-array dims total kwargs)))
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (null (cadr form)))
     ;; (make-array nil ...) — 0-dim scalar array.  Use a 1-elem vector and
     ;; wrap with NIL dims.
     (let ((r (%build-wrapped-make-array nil 1 (cddr form))))
       (format *error-output* "~&;;DEBUG-NIL-DIM in=~S~%~%out=~S~%" form r)
       r))
    (t (mapcar-dotted #'rewrite-make-array-dims form))))

;; Rewrite (eval '(FORM)) → (FORM) for MVM compatibility
;; MVM doesn't have a runtime eval; these just ensure runtime evaluation
;; which MVM already does for all compiled code.
(defun rewrite-eval-quote (form)
  "Walk form tree, converting (eval '(FORM)) to FORM."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'eval)
          (consp (cdr form))
          (null (cddr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'quote)
          (consp (cadr (cadr form))))
     ;; (eval '(FORM)) → FORM, then recursively rewrite the result
     (rewrite-eval-quote (cadr (cadr form))))
    (t (mapcar-dotted #'rewrite-eval-quote form))))

;; Rewrite (let ((*earmuff* val)) body) → (let ((*earmuff* val)) (declare (special *earmuff*)) body)
;; This ensures dynamic binding for standard stream variables like *terminal-io*, *standard-output* etc.
(defun %earmuff-sym-p (sym)
  "Check if SYM is a *earmuff* variable."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 2)
              (char= (char name 0) #\*)
              (char= (char name (1- (length name))) #\*)))))

(defun rewrite-earmuff-specials (form)
  "Walk form tree, adding (declare (special ...)) to let/let* forms binding earmuff variables."
  (cond
    ((atom form) form)
    ((and (member (car form) '(let let*))
          (consp (cdr form))
          (consp (cadr form)))
     ;; Check if any bindings are earmuff variables
     (let ((bindings (cadr form))
           (body (cddr form)))
       (let ((earmuffs (remove-if-not
                        (lambda (b)
                          (let ((var (if (consp b) (car b) b)))
                            (%earmuff-sym-p var)))
                        bindings)))
         ;; Check if there's already a (declare (special ...)) covering these
         (let* ((existing-specials nil)
                (has-decl (and (consp body) (consp (car body))
                               (eq (caar body) 'declare))))
           (when has-decl
             (dolist (spec (cdar body))
               (when (and (consp spec) (eq (car spec) 'special))
                 (setf existing-specials (append (cdr spec) existing-specials)))))
           (let ((new-earmuffs (remove-if
                                (lambda (b)
                                  (member (if (consp b) (car b) b) existing-specials))
                                earmuffs)))
             (if new-earmuffs
                 (let ((special-decl `(declare (special ,@(mapcar (lambda (b) (if (consp b) (car b) b)) new-earmuffs)))))
                   `(,(car form) ,(mapcar (lambda (b) (if (consp b) (cons (car b) (mapcar #'rewrite-earmuff-specials (cdr b))) b)) bindings)
                     ,special-decl
                     ,@(mapcar #'rewrite-earmuff-specials body)))
                 `(,(car form) ,(mapcar (lambda (b) (if (consp b) (cons (car b) (mapcar #'rewrite-earmuff-specials (cdr b))) b)) bindings)
                   ,@(mapcar #'rewrite-earmuff-specials body))))))))
    (t (mapcar-dotted #'rewrite-earmuff-specials form))))

;; Convert SBCL symbols/keywords used as package designators to strings
;; so MVM can handle them (MVM symbols are name-hashes, not printable)
(defun %stringify-pkg-designator (x)
  "Convert a package-designator LITERAL source-form to a string.
   Handles: string/character literals, keyword literals (:foo), and
   quoted-symbol literals ('foo i.e. (quote foo)).  A bare unquoted symbol
   is a VARIABLE reference at runtime and is returned unchanged."
  (cond
    ((stringp x) x)
    ((characterp x) (string x))
    ((keywordp x) (symbol-name x))
    ;; quoted-symbol literal: (quote foo) → \"FOO\"
    ((and (consp x) (eq (car x) 'quote) (consp (cdr x)) (symbolp (cadr x)))
     (symbol-name (cadr x)))
    ;; A bare unquoted symbol is a variable — leave it alone.
    (t x)))

(defun %stringify-defpackage-name (x)
  "Stringify a DEFPACKAGE name — an UNEVALUATED string-designator position,
   so a bare symbol IS a literal name (CLHS: not a form).  Distinct from
   %stringify-pkg-designator which leaves bare symbols (runtime variables)
   alone in function-call argument position."
  (cond
    ((stringp x) x)
    ((characterp x) (string x))
    ((and (consp x) (eq (car x) 'quote) (consp (cdr x)) (symbolp (cadr x)))
     (symbol-name (cadr x)))
    ((symbolp x) (symbol-name x))
    (t x)))

(defun %pkg-designator-literal-p (x)
  "True iff X (a source form in package-designator position) is a genuine
   LITERAL designator: a string literal, a character literal, a keyword
   literal (:foo), or a quoted symbol ('foo i.e. (quote foo)).  A BARE
   unquoted symbol is a VARIABLE reference (evaluate at runtime) and is
   NOT a literal — return NIL so the rewriter leaves it untouched."
  (or (stringp x)
      (characterp x)
      (keywordp x)
      (and (consp x) (eq (car x) 'quote) (consp (cdr x)) (symbolp (cadr x))
           ;; 'nil / 't are not package names
           (not (member (cadr x) '(nil t))))))

;; Rewrite do-symbols/do-external-symbols/do-all-symbols/with-package-iterator
;; These are macros in CL that SBCL expands to SBCL-internal code.
;; We rewrite them into loop-based iteration that supports RETURN.
(defvar *pkg-iter-counter* 0)

(defun rewrite-package-iteration (form)
  "Walk form tree, converting do-symbols/do-external-symbols/do-all-symbols
   and with-package-iterator into MVM-compatible forms."
  (cond
    ((atom form) form)
    ;; (do-symbols (var pkg result) body...)
    ;; → collect symbols, then iterate with block nil for return support
    ((and nil (eq (car form) 'do-symbols) (consp (cdr form)) (consp (cadr form))) ; RETIRED — compiler-side macro
     (incf *pkg-iter-counter*)
     (let* ((binding (cadr form))
            (var (first binding))
            (pkg (if (second binding) (rewrite-package-iteration (second binding)) '*package*))
            (result (if (cddr binding) (rewrite-package-iteration (third binding)) 'nil))
            (body (mapcar #'rewrite-package-iteration (cddr form)))
            (real-body (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) body))
            (syms-var (intern (format nil "%PKG-SYMS~D" *pkg-iter-counter*)))
            (cur-var (intern (format nil "%PKG-CUR~D" *pkg-iter-counter*))))
       `(let ((,syms-var nil))
          (%do-symbols-fn (lambda (,var) (setq ,syms-var (cons ,var ,syms-var))) ,pkg)
          (let ((,cur-var ,syms-var))
            (block nil
              (loop
                (when (null ,cur-var) (return ,result))
                (let ((,var (car ,cur-var)))
                  ,@real-body)
                (setq ,cur-var (cdr ,cur-var))))))))
    ;; (do-external-symbols (var pkg result) body...)
    ((and nil (eq (car form) 'do-external-symbols) (consp (cdr form)) (consp (cadr form))) ; RETIRED — compiler-side macro
     (incf *pkg-iter-counter*)
     (let* ((binding (cadr form))
            (var (first binding))
            (pkg (if (second binding) (rewrite-package-iteration (second binding)) '*package*))
            (result (if (cddr binding) (rewrite-package-iteration (third binding)) 'nil))
            (body (mapcar #'rewrite-package-iteration (cddr form)))
            (real-body (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) body))
            (syms-var (intern (format nil "%PKG-ESYMS~D" *pkg-iter-counter*)))
            (cur-var (intern (format nil "%PKG-ECUR~D" *pkg-iter-counter*))))
       `(let ((,syms-var nil))
          (%do-external-symbols-fn (lambda (,var) (setq ,syms-var (cons ,var ,syms-var))) ,pkg)
          (let ((,cur-var ,syms-var))
            (block nil
              (loop
                (when (null ,cur-var) (return ,result))
                (let ((,var (car ,cur-var)))
                  ,@real-body)
                (setq ,cur-var (cdr ,cur-var))))))))
    ;; (do-all-symbols (var result) body...)
    ((and nil (eq (car form) 'do-all-symbols) (consp (cdr form)) (consp (cadr form))) ; RETIRED — compiler-side macro
     (incf *pkg-iter-counter*)
     (let* ((binding (cadr form))
            (var (first binding))
            (result (if (cdr binding) (rewrite-package-iteration (second binding)) 'nil))
            (body (mapcar #'rewrite-package-iteration (cddr form)))
            (real-body (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) body))
            (syms-var (intern (format nil "%PKG-ASYMS~D" *pkg-iter-counter*)))
            (cur-var (intern (format nil "%PKG-ACUR~D" *pkg-iter-counter*))))
       `(let ((,syms-var nil))
          (%do-all-symbols-fn (lambda (,var) (setq ,syms-var (cons ,var ,syms-var))))
          (let ((,cur-var ,syms-var))
            (block nil
              (loop
                (when (null ,cur-var) (return ,result))
                (let ((,var (car ,cur-var)))
                  ,@real-body)
                (setq ,cur-var (cdr ,cur-var))))))))
    ;; (with-package-iterator ...) — RETIRED stub.  Was `0`, which clobbered
    ;; every with-package-iterator form to a body-less literal 0 *before* the
    ;; real, complete handler in rewrite-reader-forms (~line 2909) could see
    ;; it.  Disabled (matching the retired do-symbols branches above) so the
    ;; form falls through to that handler.
    ((and nil (eq (car form) 'with-package-iterator))
     0)
    ;; (defpackage name option...) → (%defpackage-impl name '(option...))
    ;; Options are bare lists like (:use) that would be evaluated as forms.
    ;; Convert to a single quoted list of options.
    ;; In DEFPACKAGE the NAME is an UNEVALUATED string-designator (CLHS: the
    ;; defpackage name is a literal, not a form), so a bare symbol here IS a
    ;; literal name and must be stringified — unlike the function-call branches
    ;; below where a bare symbol is a runtime variable.
    ((and (eq (car form) 'defpackage) (cdr form))
     (let ((name (rewrite-package-iteration (%stringify-defpackage-name (cadr form))))
           (options (cddr form)))
       `(%defpackage-impl ,name (quote ,options))))
    ;; Package functions with keyword/symbol designator args → stringify.
    ;; INTERN and FIND-SYMBOL are EXCLUDED here: their first arg is the
    ;; string NAME (not a package designator), and their optional second
    ;; arg is the package — handled separately just below.
    ;; NOTE: only a genuine package-designator LITERAL is stringified here —
    ;; a keyword (:foo) or a quoted symbol ('foo).  A *bare unquoted symbol*
    ;; in package-arg position is a VARIABLE REFERENCE in conformant CL (e.g.
    ;; (let ((pkg-name "P")) (make-package pkg-name) ...)) and MUST be left
    ;; alone so it is evaluated at runtime.  Stringifying it would corrupt the
    ;; test (make-package would create a package literally named "PKG-NAME").
    ;; The underlying make-package/intern-with-a-runtime-variable path works in
    ;; cl-eval, so no rewrite is needed for variables.
    ((and (member (car form) '(make-package find-package delete-package
                               safely-delete-package rename-package
                               use-package unuse-package
                               in-package export unexport import unintern
                               shadow shadowing-import
                               package-name package-nicknames
                               package-use-list package-used-by-list
                               package-shadowing-symbols))
          (cdr form)
          (%pkg-designator-literal-p (cadr form)))
     (let ((str-arg (%stringify-pkg-designator (cadr form))))
       `(,(car form) ,str-arg ,@(mapcar #'rewrite-package-iteration (cddr form)))))
    ;; (intern NAME [PACKAGE]) / (find-symbol NAME [PACKAGE]) — only the
    ;; SECOND arg may need stringification; the first is a runtime string
    ;; and must be left alone (was the cause of the FORMATTER-TEST-NAME-STRING
    ;; macroexpansion bug — see commit log).
    ;; Only the SECOND arg (the PACKAGE designator) may be stringified, and
    ;; only when it is a LITERAL (keyword or quoted symbol) — never a bare
    ;; unquoted symbol (a runtime variable).  See note above.
    ((and (member (car form) '(intern find-symbol))
          (consp (cdr form))
          (consp (cddr form))
          (%pkg-designator-literal-p (caddr form)))
     (let* ((name-arg (rewrite-package-iteration (cadr form)))
            (pkg-arg  (%stringify-pkg-designator (caddr form)))
            (rest     (cdddr form)))
       `(,(car form) ,name-arg ,pkg-arg
                     ,@(mapcar #'rewrite-package-iteration rest))))
    ;; (ignore-errors form) → CLHS 9.1.5.3.1 returns the body's primary
    ;; values on success and (values nil c) when c is the condition that
    ;; caused the error.  The previous rewriter returned plain NIL,
    ;; which broke ignore-errors.{4,5,6} that check for the captured
    ;; condition as a second value.
    ((and (eq (car form) 'ignore-errors) (cdr form))
     (let ((body (rewrite-package-iteration (cadr form))))
       `(handler-case ,body (error (%ie-c) (values nil %ie-c)))))
    ;; (report-and-ignore-errors form...) → (progn form...) (ignore errors)
    ;; report-and-ignore-errors takes a BODY (multiple forms), not a single
    ;; form.  The old (cadr form) kept only the FIRST body form and silently
    ;; dropped the rest — so a setup block like
    ;;   (report-and-ignore-errors (defvar *c* (define-method-combination ..))
    ;;                             (defgeneric g ..) (defmethod g ..) ..)
    ;; registered only the defvar; the defgeneric + every defmethod vanished,
    ;; leaving the GF undefined at dispatch time (define-method-combination
    ;; 01/04 returned all-NIL).  Wrap the whole body in PROGN so emit-sub
    ;; flattens each form into run-init-FILE.  Error-ignoring isn't needed in
    ;; the build (each init-form is already wrapped in its own handler-case).
    ((eq (car form) 'report-and-ignore-errors)
     (cons 'progn (mapcar #'rewrite-package-iteration (cdr form))))
    ;; (return-from block-name value) - need to rewrite body
    ((eq (car form) 'return-from)
     `(return-from ,(cadr form) ,@(mapcar #'rewrite-package-iteration (cddr form))))
    (t (mapcar-dotted #'rewrite-package-iteration form))))

;; Rewrite reader-related forms for MVM compatibility
;;; ============================================================
;;; Printer-related SBCL-side macros (expanded at build time)
;;; ============================================================

;; def-print-test: expanded at SBCL side using printer-aux.lsp definition
(defmacro def-print-test (name form result &rest bindings)
  `(deftest ,name
     (if (equalpt
          (my-with-standard-io-syntax
           (lambda ()
             (let ((*print-readably* nil))
               (declare (special *print-readably*))
               ,(if bindings
                    `(let ,bindings
                       (declare (special ,@(mapcar (lambda (b) (if (consp b) (car b) b)) bindings)))
                       (with-output-to-string (*standard-output*)
                         (declare (special *standard-output*))
                         (prin1 ,form)))
                    `(with-output-to-string (*standard-output*)
                       (declare (special *standard-output*))
                       (prin1 ,form))))))
          ,result)
         t
       ,result)
     t))

;; def-pprint-test: uses pprint features — stub to basic prin1
(defmacro def-pprint-test (name form expected-value &rest keys)
  (let ((margin (getf keys :margin 100))
        (miser (getf keys :miser nil))
        (circle (getf keys :circle nil))
        (len (getf keys :len nil))
        (pretty (getf keys :pretty t))
        (escape (getf keys :escape nil))
        (readably (getf keys :readably nil))
        (package (or (getf keys :package) '(find-package "CL-TEST"))))
    `(deftest ,name
       (%with-standard-io-syntax
        (lambda ()
          (let ((*print-pretty* ,pretty)
                (*print-escape* ,escape)
                (*print-readably* ,readably)
                (*print-right-margin* ,margin)
                (*package* ,package)
                (*print-length* ,len)
                (*print-miser-width* ,miser)
                (*print-circle* ,circle))
            (declare (special *print-pretty* *print-escape* *print-readably*
                              *print-right-margin* *package* *print-length*
                              *print-miser-width* *print-circle*))
            ,form)))
       ,expected-value)))

;; def-format-test: expand both format and formatter variants
(defmacro def-format-test (name string args expected-output &optional (num-left 0))
  (let* ((s (symbol-name name))
         (expected-prefix (string 'format.))
         (expected-prefix-length (length expected-prefix))
         (formatter-test-name-string
          (concatenate 'string (string 'formatter.)
                       (subseq s expected-prefix-length)))
         (formatter-test-name (intern formatter-test-name-string
                                      (symbol-package name))))
    `(progn
       (deftest ,name
         (%with-standard-io-syntax
          (lambda ()
            (let ((*print-readably* nil)
                  (*package* (find-package "CL-TEST")))
              (declare (special *print-readably* *package*))
              (format nil ,string ,@args))))
         ,expected-output)
       (deftest ,formatter-test-name
         (let ((fn (formatter ,string))
               (args (list ,@args)))
           (%with-standard-io-syntax
            (lambda ()
              (let ((*print-readably* nil)
                    (*package* (find-package "CL-TEST")))
                (declare (special *print-readably* *package*))
                (with-output-to-string
                  (stream)
                  (declare (special stream))
                  (let ((tail (apply fn stream args)))
                    tail))))))
         ,expected-output))))

;; formatter: SBCL-level stub (will be a function at MVM level)
;; We don't expand formatter at SBCL level; it's a runtime function
;; However, (formatter "~D") needs to work as a lambda at runtime
;; The MVM runtime defines formatter as a function already

;; def-ppblock-test: pprint logical block test
(defmacro def-ppblock-test (name form expected-value &rest key-args)
  `(def-pprint-test ,name
     (with-output-to-string
       (*standard-output*)
       (pprint-logical-block (*standard-output* nil) ,form))
     ,expected-value
     ,@key-args))

;; Handle print-unreadable-object at SBCL level
;; (print-unreadable-object (obj stream &key type identity) body)
;; → (%print-unreadable-object obj stream type-p identity-p (lambda () body))

;; my-with-standard-io-syntax: alias for %with-standard-io-syntax (thunk version)
;; This is called from printer-aux tests
;; When called with a thunk (lambda), pass directly
;; When called with a body... (after SBCL expansion), wrap in lambda

;; coin: random boolean (used in randomly-check-readability)
;; random-from-seq: pick random element from sequence
;; random-thing: generate random test object
;; These are needed for random write tests

;; Since these depend on random, we define stubs
;; that produce deterministic "random" values for MVM

;;; ============================================================
;;; SBCL-side condition/define-condition rewriters
;;; ============================================================

;; Parse a slot-spec from define-condition:
;; (slot-name :initarg :kw1 :initarg :kw2 :initform form :reader reader ...)
;; Returns: (slot-name initargs initform-or-:no-initform readers)
(defun parse-dc-slot (slot-spec)
  (if (atom slot-spec)
      (list slot-spec nil :no-initform nil)
      (let ((name (car slot-spec))
            (opts (cdr slot-spec))
            (initargs nil)
            (initform :no-initform)
            (readers nil))
        (loop
          (when (null opts) (return))
          (let ((key (car opts))
                (val (cadr opts)))
            (cond
              ((eq key :initarg)
               (setf initargs (append initargs (list val)))
               (setf opts (cddr opts)))
              ((eq key :initform)
               (setf initform val)
               (setf opts (cddr opts)))
              ((eq key :reader)
               (setf readers (append readers (list val)))
               (setf opts (cddr opts)))
              ((eq key :accessor)
               (setf readers (append readers (list val)))
               (setf opts (cddr opts)))
              ((eq key :type)
               (setf opts (cddr opts)))
              ((eq key :documentation)
               (setf opts (cddr opts)))
              ((eq key :writer)
               (setf opts (cddr opts)))
              (t (setf opts (cddr opts))))))
        (list name initargs initform readers))))

;; Build the slot-descriptor list for %define-condition
;; Returns a quoted list: '((name (initarg...) initform-or-:no-initform) ...)
(defun build-slot-descriptors (slot-specs)
  (mapcar (lambda (s)
            (let* ((parsed (parse-dc-slot s))
                   (name (first parsed))
                   (initargs (second parsed))
                   (initform (third parsed)))
              (list name initargs initform)))
          slot-specs))

;; Extract option from define-condition options list
(defun dc-option (options key)
  (let ((found (assoc key options)))
    (if found (cdr found) nil)))

;; Expand (define-condition name parents slot-specs &rest options)
;; into (%define-condition ...) + reader/accessor defun forms
(defun rewrite-define-condition (form)
  (let* ((name (second form))
         (parents (or (third form) '(condition)))
         (slot-specs (or (fourth form) nil))
         (rest-opts (cddr (cddr form)))
         ;; Parse &rest options
         (options (loop for opt in rest-opts
                        when (consp opt) collect (cons (car opt) (cdr opt))))
         ;; default-initargs option
         (default-initargs-opt (dc-option options :default-initargs))
         ;; report option
         (report-opt (dc-option options :report))
         ;; Build slot descriptors
         (slot-descriptors (build-slot-descriptors slot-specs))
         ;; Collect all reader defuns
         (reader-defuns
          (loop for s in slot-specs
                append
                (let* ((parsed (parse-dc-slot s))
                       (slot-name (first parsed))
                       (readers (fourth parsed)))
                  (mapcar (lambda (r)
                            `(defun ,r (c) (%condition-slot c ',slot-name)))
                          readers))))
         ;; Build report-fn arg (nil or a quoted lambda/symbol)
         (report-fn-arg
          (cond
            ((null report-opt) nil)
            ((and (consp report-opt) (eq (car report-opt) 'lambda))
             `',report-opt)
            ((symbolp report-opt) `',report-opt)
            ((stringp report-opt)
             ;; String report: lambda (c s) (write-string "msg" s)
             `(lambda (c s) (declare (ignore c)) (write-string ,report-opt s)))
            (t nil)))
         ;; Build default-initargs arg
         (default-initargs-arg
          (if default-initargs-opt
              `',default-initargs-opt
              nil))
         ;; Define-condition call
         (def-call `(%define-condition ',name ',parents ',slot-descriptors
                                       ,default-initargs-arg ,report-fn-arg)))
    `(progn
       ,def-call
       ,@reader-defuns)))

;; Make test name from condition name + suffixes (like make-def-cond-name)
(defun make-dc-test-name (name-str &rest suffixes)
  (intern (apply #'concatenate 'string name-str suffixes) :cl-test))

;; Expand define-condition-with-tests inline
(defun rewrite-define-condition-with-tests (form)
  (let* ((name-symbol (second form))
         (parents (or (third form) nil))
         (slot-specs (or (fourth form) nil))
         (options (nthcdr 4 form))
         ;; Gensym-free name to use in tests
         (name-str (if (symbolp name-symbol) (symbol-name name-symbol) nil)))
    ;; Skip #:uninterned symbols (like #:condition-3)
    (unless name-str
      (return-from rewrite-define-condition-with-tests '(progn)))
    (let* ((dc-form (append (list 'define-condition name-symbol parents slot-specs)
                            options))
           (dc-rewritten (rewrite-define-condition dc-form))
           ;; Parents augmented with 'condition always
           (all-parents (if (member 'condition parents) parents (append parents '(condition))))
           ;; Generate subtype tests for each parent + condition
           (tests nil))
      ;; IS-SUBTYPE-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-SUBTYPE-OF/" (symbol-name parent))
                 (subtypep* ',name-symbol ',parent)
                 t t)
               tests))
      ;; IS-SUBTYPE-OF-2 tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-SUBTYPE-OF-2/" (symbol-name parent))
                 (check-all-subtypep ',name-symbol ',parent)
                 nil)
               tests))
      ;; IS-NOT-SUPERTYPE-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-NOT-SUPERTYPE-OF/" (symbol-name parent))
                 (subtypep* ',parent ',name-symbol)
                 nil t)
               tests))
      ;; IS-A tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-A/" (symbol-name parent))
                 (let ((c (make-condition ',name-symbol)))
                   (notnot-mv (typep c ',parent)))
                 t)
               tests))
      ;; IS-SUBCLASS-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-SUBCLASS-OF/" (symbol-name parent))
                 (subtypep* (find-class ',name-symbol) (find-class ',parent))
                 t t)
               tests))
      ;; IS-NOT-SUPERCLASS-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-NOT-SUPERCLASS-OF/" (symbol-name parent))
                 (subtypep* (find-class ',parent) (find-class ',name-symbol))
                 nil t)
               tests))
      ;; IS-A-MEMBER-OF-CLASS tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-A-MEMBER-OF-CLASS/" (symbol-name parent))
                 (let ((c (make-condition ',name-symbol)))
                   (notnot-mv (typep c (find-class ',parent))))
                 t)
               tests))
      ;; HANDLER-CASE-1
      (push `(deftest ,(make-dc-test-name name-str "/HANDLER-CASE-1")
               (let ((c (make-condition ',name-symbol)))
                 (handler-case (signal c)
                               (,name-symbol (c1) (eqt c c1))))
               t)
             tests)
      ;; HANDLER-CASE-2
      (push `(deftest ,(make-dc-test-name name-str "/HANDLER-CASE-2")
               (let ((c (make-condition ',name-symbol)))
                 (handler-case (signal c)
                               (condition (c1) (eqt c c1))))
               t)
             tests)
      ;; HANDLER-CASE-3 — only if none of parents is-error
      (let ((has-error-parent nil))
        (dolist (p parents)
          (when (member p '(error serious-condition simple-error simple-type-error
                            type-error cell-error unbound-variable undefined-function
                            unbound-slot arithmetic-error division-by-zero
                            program-error control-error package-error
                            stream-error end-of-file reader-error parse-error
                            print-not-readable file-error storage-condition
                            floating-point-overflow floating-point-underflow
                            floating-point-inexact floating-point-invalid-operation))
            (setf has-error-parent t)))
        (unless has-error-parent
          (push `(deftest ,(make-dc-test-name name-str "/HANDLER-CASE-3")
                   (let ((c (make-condition ',name-symbol)))
                     (handler-case (signal c)
                                   (error () nil)
                                   (,name-symbol (c2) (eqt c c2))))
                   t)
                 tests)))
      ;; Emit define-condition first, then tests in order
      `(progn
         ,dc-rewritten
         ,@(nreverse tests)))))


(defun dmc-parse-group-spec (spec)
  "Parse a long-form method-group specifier SPEC into
   (values group-var patterns order-form required-form).
   SPEC = (group-var qualifier-pattern... [:order O] [:required R]
           [:description D]).  Patterns run from after GROUP-VAR up to the
   first keyword.  ORDER/REQUIRED default forms preserve CLHS defaults
   (:most-specific-first, NIL)."
  (let* ((group-var (car spec))
         (rest (cdr spec))
         (patterns nil)
         (order-form '':most-specific-first)
         (required-form 'nil))
    ;; Collect patterns until the first keyword.
    (loop while (and rest (not (keywordp (car rest))))
          do (push (car rest) patterns) (pop rest))
    (setq patterns (nreverse patterns))
    ;; Parse keyword options.
    (loop while rest
          do (let ((k (car rest)) (v (cadr rest)))
               (cond
                 ((eq k :order) (setq order-form v))
                 ((eq k :required) (setq required-form v))
                 ((eq k :description) nil)  ; ignored — doc only
                 (t nil))
               (setq rest (cddr rest))))
    (values group-var patterns order-form required-form)))

(defun rewrite-dmc-long-form (mc-name options)
  "Rewrite a long-form DEFINE-METHOD-COMBINATION into a registration of a
   builder closure.  OPTIONS = (lambda-list method-group-specs . body).

   The builder, given the applicable methods and the combination's runtime
   args, partitions the methods into the declared groups (signalling on a
   method that matches no group, or an empty :required group), binds the
   combination lambda-list params + the group vars, and evaluates BODY to
   produce the effective-method form."
  (let* ((lambda-list (car options))
         (group-specs  (cadr options))
         (body-and-aux (cddr options))
         ;; Skip (:arguments ...) / (:generic-function ...) option forms and
         ;; declarations at the head of the body; keep the rest as BODY.
         (body
          (let ((b body-and-aux))
            (loop while (and b (consp (car b))
                             (member (car (car b))
                                     '(:arguments :generic-function declare)))
                  do (pop b))
            b))
         ;; Per-group parsed records + the group-var binding list.
         (group-rec-forms nil)
         (group-var-bindings nil)
         (gi 0))
    (dolist (spec group-specs)
      (multiple-value-bind (gv patterns order-form required-form)
          (dmc-parse-group-spec spec)
        (push `(list (quote ,patterns) ,order-form ,required-form)
              group-rec-forms)
        (push `(,gv (%dmc-nth %dmc-groups ,gi)) group-var-bindings)
        (incf gi)))
    (setq group-rec-forms (nreverse group-rec-forms))
    (setq group-var-bindings (nreverse group-var-bindings))
    ;; Build the registration form.  The inner lambda binds the combination
    ;; params from %dmc-cargs via APPLY (handles &optional/&key/&rest); its
    ;; body partitions and binds the group vars, then runs BODY.
    `(%define-method-combination-long
      (quote ,mc-name)
      (function
       (lambda (%dmc-applicable %dmc-cargs)
         (apply
          (function
           (lambda ,lambda-list
             (let* ((%dmc-group-recs (list ,@group-rec-forms))
                    (%dmc-groups
                     (%dmc-partition-groups %dmc-applicable %dmc-group-recs))
                    ,@group-var-bindings)
               ,@(if body body (list nil)))))
          %dmc-cargs)))
      ,(length group-specs))))

(defun %pplb-body-iterates-p (forms)
  "T if FORMS (the original, not-yet-rewritten body of a
   pprint-logical-block) reference pprint-pop or
   pprint-exit-if-list-exhausted anywhere in their tree.  Used to pick
   between the simple (prefix/body/suffix) and full (begin/catch/end)
   expansions."
  (cond
    ((symbolp forms)
     (or (eq forms 'pprint-pop)
         (eq forms 'pprint-exit-if-list-exhausted)))
    ((consp forms)
     (or (%pplb-body-iterates-p (car forms))
         (%pplb-body-iterates-p (cdr forms))))
    (t nil)))

(defun %build-reader-macrolet-expander (mparams mbody)
  "Build a one-arg (form)->expansion expander for a MACROLET local macro,
   honouring macro lambda-list semantics (CLHS 3.4.4): a leading &WHOLE var
   binds the WHOLE macro form, an &ENVIRONMENT var (anywhere) binds NIL, and
   the remaining pattern destructures (cdr form) — supporting nested
   destructuring / &optional / &rest / &key via the host DESTRUCTURING-BIND.
   Used by rewrite-reader-forms to pre-expand local-macro calls that appear
   inside generalized-variable places (rotatef/incf/pop/...), which must be
   expanded before the compiler's modify-macro analysis runs.  Mirrors
   modus.mvm::build-macrolet-expander but kept local to the build script."
  (let ((whole-var nil) (env-var nil) (rest-params nil))
    (when (and (consp mparams) (symbolp (car mparams))
               (string= (symbol-name (car mparams)) "&WHOLE"))
      (setf whole-var (cadr mparams))
      (setf mparams (cddr mparams)))
    (let ((p mparams))
      (loop while (consp p) do
        (let ((elt (car p)))
          (if (and (symbolp elt) (string= (symbol-name elt) "&ENVIRONMENT"))
              (progn (setf env-var (cadr p)) (setf p (cddr p)))
              (progn (push elt rest-params) (setf p (cdr p))))))
      (setf rest-params (nreverse rest-params)))
    (let* ((bindings (append (when whole-var (list (list whole-var 'form)))
                             (when env-var (list (list env-var nil)))))
           (db-form `(destructuring-bind (,@rest-params) (cdr form) ,@mbody)))
      (eval `(lambda (form)
               (let ,bindings
                 (declare (ignorable ,@(mapcar #'car bindings)))
                 ,db-form))))))

;; Forward declaration: *mvm-eval-diff-mode* is defined (from MODUS_MVM_EVAL_DIFF)
;; further down, but rewrite-reader-forms references it to gate the RESTART-CASE
;; rewrite (raw special form in diff mode; %with-restarts otherwise).
(defvar *mvm-eval-diff-mode*)

(defun rewrite-reader-forms (form)
  "Walk form tree, rewriting reader-related forms for MVM."
  (cond
    ;; Pathname objects (created by SBCL from #P"..." reader syntax) → namestring
    ((and (not (null form)) (typep form 'pathname))
     (namestring form))
    ((atom form) form)
    ;; (multiple-value-call fn arg1 arg2 ...)
    ;; Collect all MV from each arg, pass to fn.
    ;; For #'list specifically: (multiple-value-call #'list a b c)
    ;;   = (append (mvl a) (mvl b) (mvl c))  [because list just collects all values]
    ;; For other functions: (apply fn (append (mvl a1) (mvl a2) ...))
    ;;   NOTE: apply requires a function, not a macro. #'list is a compiler macro
    ;;   in MVM, so (apply #'list ...) would fail. Use append for #'list.
    ((and (eq (car form) 'multiple-value-call) (cdr form))
     (let* ((fn-form (rewrite-reader-forms (cadr form)))
            (arg-forms (mapcar #'rewrite-reader-forms (cddr form)))
            (mvl-forms (mapcar (lambda (a) `(multiple-value-list ,a)) arg-forms)))
       (cond
         ;; No args: (funcall fn)
         ((null arg-forms)
          `(funcall ,fn-form))
         ;; Single arg, no fn: (multiple-value-list arg)
         ;; fn=#'list: (multiple-value-call #'list arg) = (multiple-value-list arg)
         ((and (null (cdr arg-forms))
               (equal fn-form '(function list)))
          `(multiple-value-list ,(car arg-forms)))
         ;; fn=#'list multi-arg: collect all as flat list via append
         ((equal fn-form '(function list))
          `(append ,@mvl-forms))
         ;; Single arg, generic fn: (apply fn (multiple-value-list arg))
         ((null (cdr arg-forms))
          `(apply ,fn-form (multiple-value-list ,(car arg-forms))))
         ;; Multiple args, generic fn: (apply fn (append ...))
         (t
          `(apply ,fn-form (append ,@mvl-forms))))))
    ;; (multiple-value-prog1 first-form . rest) RETIRED — compile-time
    ;; macro in compiler.lisp via mvm-define-macro "MULTIPLE-VALUE-PROG1".
    ;; Gated off here with `nil` so the clause stays in place for
    ;; reference; clean-deletion in a later pass.
    ((and nil (eq (car form) 'multiple-value-prog1) (cdr form))
     (let* ((first-form (rewrite-reader-forms (cadr form)))
            (rest-forms (mapcar #'rewrite-reader-forms (cddr form)))
            (result-var '%mvp1-result))
       (if rest-forms
           `(let ((,result-var (multiple-value-list ,first-form)))
              ,@rest-forms
              (values-list ,result-var))
           `(values-list (multiple-value-list ,first-form)))))
    ;; (with-output-to-string (var &optional string-form) body...)
    ;; → (let ((var (make-string-output-stream))) body... (get-output-stream-string var))
    ;; If var is an earmuff special (e.g. *standard-output*), add (declare
    ;; (special var)) so prin1/format inside the body see the new binding
    ;; via dynamic lookup. Without this, the binding is lexical, prin1 reads
    ;; the global *standard-output* (often nil → serial fallback), and the
    ;; captured output ends up empty.
    ((and (eq (car form) 'with-output-to-string)
          (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (stream-var (car binding))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (is-special (and (symbolp stream-var) (%earmuff-sym-p stream-var))))
       (if is-special
           `(let ((,stream-var (make-string-output-stream)))
              (declare (special ,stream-var))
              ,@body
              (get-output-stream-string ,stream-var))
           `(let ((,stream-var (make-string-output-stream)))
              ,@body
              (get-output-stream-string ,stream-var)))))
    ;; (with-standard-io-syntax body...) → (%with-standard-io-syntax (lambda () body...))
    ((and (eq (car form) 'with-standard-io-syntax) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (cdr form))))
       `(%with-standard-io-syntax (lambda () ,@body))))
    ;; (print-unreadable-object (obj stream &key type identity) body...)
    ;; → (%print-unreadable-object obj stream type-p identity-p (lambda () body...))
    ((and (eq (car form) 'print-unreadable-object)
          (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (obj (first binding))
            (stream (second binding))
            (keys (cddr binding))
            (type-p (if (getf keys :type) (getf keys :type) nil))
            (identity-p (if (getf keys :identity) (getf keys :identity) nil))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(%print-unreadable-object ,obj ,stream ,type-p ,identity-p
                                  ,(if body `(lambda () ,@body) nil))))
    ;; (my-with-standard-io-syntax body...) — printer-aux.lsp's def-print-test
    ;; redefines def-print-test (when we eval its defmacro from load-ansi-aux)
    ;; to omit the lambda wrap, so the test bodies arrive here as a direct
    ;; form rather than a thunk. Our runtime my-with-standard-io-syntax is a
    ;; function (takes a thunk), so calling it on a direct form would funcall
    ;; the form's value (e.g., a string) and crash. Expand at codegen time
    ;; into the same let-bindings the ANSI-aux macro produces.
    ((and (eq (car form) 'my-with-standard-io-syntax) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (cdr form))))
       `(let ((*package* (find-package "COMMON-LISP-USER"))
              (*print-array* t)
              (*print-base* 10)
              (*print-case* :upcase)
              (*print-circle* nil)
              (*print-escape* t)
              (*print-gensym* t)
              (*print-length* nil)
              (*print-level* nil)
              (*print-readably* t)
              (*print-pretty* nil)
              (*print-radix* nil)
              (*read-base* 10)
              (*read-suppress* nil)
              (*read-eval* t))
          (declare (special *package* *print-array* *print-base* *print-case*
                            *print-circle* *print-escape* *print-gensym*
                            *print-length* *print-level* *print-readably*
                            *print-pretty* *print-radix*
                            *read-base* *read-suppress* *read-eval*))
          ,@body)))
    ;; (formatter string) → (formatter string) — runtime function
    ;; (pprint-logical-block (stream list &key prefix suffix per-line-prefix) body...)
    ;; Modus has no real pretty-printing infrastructure, but the test suite
    ;; mostly checks: (a) prefix is written, (b) body runs with the stream var
    ;; bound, (c) suffix is written, (d) when body is empty, the list-arg is
    ;; written.  That's enough to pass PPRINT-LOGICAL-BLOCK.1..N which were
    ;; failing previously because the rewriter dropped everything but body.
    ;; Real implementation: each logical block establishes a CATCH '%pp-tag
    ;; frame plus dynamic state (*%pp-list* / *%pp-count* / *%pp-stream* /
    ;; *%pp-listp* / *%pp-level*) that the runtime helpers %pprint-pop-fn and
    ;; %pprint-exit-fn read.  pprint-pop / pprint-exit-if-list-exhausted are
    ;; expanded to those helpers (below).  Honors *print-level* ("#") and
    ;; *print-length* ("...").  See cl-printer.lisp for the helpers.
    ((and (eq (car form) 'pprint-logical-block)
          (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (stream-raw (first binding))
            (list-arg (rewrite-reader-forms (second binding)))
            (kwlist (cddr binding))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (prefix nil) (per-line nil) (suffix nil)
            (have-prefix nil) (have-per-line nil))
       (let ((kw kwlist))
         (loop (when (or (null kw) (null (cdr kw))) (return))
           (let ((k (car kw)) (v (rewrite-reader-forms (cadr kw))))
             (cond ((eq k :prefix) (setq prefix v have-prefix t))
                   ((eq k :per-line-prefix) (setq per-line v have-per-line t))
                   ((eq k :suffix) (setq suffix v))))
           (setq kw (cddr kw))))
       (let ((stream-expr (cond ((null stream-raw) '*standard-output*)
                                ((eq stream-raw t) '*terminal-io*)
                                (t stream-raw)))
             (svar (gensym "PPS")))
         ;; CLHS: supplying BOTH :prefix and :per-line-prefix is an error.
         ;; %pprint-lb-begin pushes block state on *%pp-ctx*; the body runs
         ;; inside CATCH '%pp-tag (so pprint-pop / pprint-exit can escape it);
         ;; %pprint-lb-end writes the suffix and pops — balanced on both
         ;; normal and thrown exit.  *print-level* depth = (length *%pp-ctx*)
         ;; at block entry → "#" when it meets/exceeds *print-level*.
         ;; Does the body iterate the block list?  (pprint-pop /
         ;; pprint-exit-if-list-exhausted appear in the ORIGINAL, not-yet-
         ;; rewritten body — they're rewritten to %pprint-pop-fn /
         ;; %pprint-exit-fn.)  Only then do we need the begin/catch/end
         ;; state machine.  The simple branch (write prefix; body; write
         ;; suffix) is far less code, so it compiles cleanly even deep
         ;; inside the giant per-file run-ansi-FOO function.
         (let ((iterates (%pplb-body-iterates-p (cddr form))))
           (if (and have-prefix have-per-line)
               `(error "pprint-logical-block: both :prefix and :per-line-prefix supplied")
               (if iterates
                   ;; Full state-machine form.
                   `(let ((,svar (%resolve-output-stream ,stream-expr)))
                      (declare (special *print-level*))
                      (if (let ((lvl *print-level*))
                            (and lvl (integerp lvl) (>= (length *%pp-ctx*) lvl)))
                          (write-string "#" ,svar)
                          (let ((,svar (%pprint-lb-begin
                                        ,svar ,list-arg
                                        ,(if have-prefix prefix nil)
                                        ,(if have-per-line per-line nil))))
                            (catch '%pp-tag
                              ,@(or body `((write ,list-arg :stream ,svar))))
                            (%pprint-lb-end ,svar ,suffix)))
                      nil)
                   ;; Simple form: write prefix, run body, write suffix —
                   ;; deliberately as close to the old lean stub as possible
                   ;; (raw stream, no helper calls / LET / DECLARE) so it
                   ;; compiles cleanly deep inside the giant per-file
                   ;; run-ansi-FOO function.  CLHS: prefix/suffix omitted when
                   ;; the block object is not a list — guarded inline by
                   ;; %pp-list-arg-p.  *print-level* "#" truncation: depth is
                   ;; tracked by %pp-level-deep-p / a bump of *%pp-level*
                   ;; around the body (setq save/restore keeps it lean).
                   `(if (%pp-level-deep-p)
                        (write-string "#" ,stream-expr)
                        (progn
                          (setq *%pp-level* (+ 1 (or *%pp-level* 0)))
                          ,@(when (or have-prefix have-per-line)
                              `((when (%pp-list-arg-p ,list-arg)
                                  (write-string ,(if have-prefix prefix per-line)
                                                ,stream-expr))))
                          ,@(or body `((write ,list-arg :stream ,stream-expr)))
                          ,@(when suffix
                              `((when (%pp-list-arg-p ,list-arg)
                                  (write-string ,suffix ,stream-expr))))
                          (setq *%pp-level* (- (or *%pp-level* 1) 1))
                          nil))))))))
    ;; (pprint-exit-if-list-exhausted) → runtime helper (throws to %pp-tag)
    ((and (eq (car form) 'pprint-exit-if-list-exhausted) (null (cdr form)))
     '(%pprint-exit-fn))
    ;; (pprint-pop) → runtime helper
    ((and (eq (car form) 'pprint-pop) (null (cdr form)))
     '(%pprint-pop-fn))
    ;; (setf (readtable-case rt) val) → (%set-readtable-case rt val)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'readtable-case))
     (let ((rt-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(%set-readtable-case ,rt-arg ,val)))
    ;; (setf (slot-value obj slot) val) → (set-slot-value obj slot val)
    ;; Must handle before the generic setf fallthrough (MVM setf macro only passes 1 arg)
    ;; Multi-pair SETF: (setf (slot-value o 'a) 1 (slot-value o 'b) 16)
    ;; previously dropped every pair after the first — change-class.2.3's
    ;; second slot silently stayed unbound.  Recurse on the remaining
    ;; pairs as a fresh (setf ...) form wrapped in progn.
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'slot-value)
          (cddr form))
     (let ((place (cadr form))
           (val (rewrite-reader-forms (caddr form)))
           (more (cdddr form)))
       (let ((obj (rewrite-reader-forms (cadr place)))
             (slot (rewrite-reader-forms (caddr place))))
         (if more
             `(progn (set-slot-value ,obj ,slot ,val)
                     ,(rewrite-reader-forms `(setf ,@more)))
             `(set-slot-value ,obj ,slot ,val)))))
    ;; (setf (symbol-function sym) fn) → (set-symbol-function sym fn)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'symbol-function)
          (cddr form))
     (let ((sym-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(set-symbol-function ,sym-arg ,val)))
    ;; (setf (fdefinition sym) fn) → (set-fdefinition sym fn)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'fdefinition)
          (cddr form))
     (let ((sym-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(set-fdefinition ,sym-arg ,val)))
    ;; (setf (get sym indicator) val) → (set-get sym indicator val)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'get)
          (cddr form))
     (let ((place (cadr form))
           (val (rewrite-reader-forms (caddr form))))
       (let ((sym-arg (rewrite-reader-forms (cadr place)))
             (ind-arg (rewrite-reader-forms (caddr place))))
         `(set-get ,sym-arg ,ind-arg ,val))))
    ;; (setf (symbol-plist sym) val) → (set-symbol-plist sym val)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'symbol-plist)
          (cddr form))
     (let ((sym-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(set-symbol-plist ,sym-arg ,val)))
    ;; (setf (getf plist ind) val) → (setq plist (set-getf plist ind val))
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'getf)
          (cddr form))
     (let* ((place (cadr form))
            (plist-form (rewrite-reader-forms (cadr place)))
            (ind-form (rewrite-reader-forms (caddr place)))
            (val-form (rewrite-reader-forms (caddr form))))
       ;; getf plist may be a variable - update it
       (if (symbolp (cadr place))
           `(setq ,(cadr place) (set-getf ,plist-form ,ind-form ,val-form))
           `(set-getf ,plist-form ,ind-form ,val-form))))
    ;; NOTE: (setf (ldb spec n) val) is NO LONGER rewritten here.  The old
    ;; rewriter expanded to `(setq n (dpb val spec n))`, which RETURNS the
    ;; updated place (n) rather than VAL — violating CLHS (setf yields the
    ;; newly-stored value).  ldb.place.1/.2 expect the VALUE.  The compiler's
    ;; SETF macro (compiler.lisp ~1662) now handles (setf (ldb …)) correctly
    ;; (binds val to a temp, stores via dpb, returns val) and also covers the
    ;; non-symbol place case via a nested (setf place (dpb …)).  Let the test
    ;; source flow through to it untouched.
    ;; (setf (values v1 v2 ...) expr) → (multiple-value-setq (v1 v2 ...) expr)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'values)
          (cddr form))
     (let ((vars (cdr (cadr form)))
           (val-form (rewrite-reader-forms (caddr form))))
       `(multiple-value-setq ,vars ,val-form)))
    ;; (with-simple-restart (name report) body...)
    ;; → (block nil (handler-bind (...) body...))  OR just body
    ((and (eq (car form) 'with-simple-restart) (cdr form) (consp (cadr form)))
     (let ((body (mapcar #'rewrite-reader-forms (cddr form))))
       `(progn ,@body)))
    ;; (check-type place type-spec &optional string)
    ;; → (unless (typep place type-spec) (error "..."))
    ((and (eq (car form) 'check-type) (cdr form) (cddr form))
     (let ((place (rewrite-reader-forms (cadr form)))
           (type-spec (rewrite-reader-forms (caddr form)))
           (string (if (cdddr form) (rewrite-reader-forms (cadddr form)) nil)))
       `(unless (typep ,place ',type-spec)
          (error ,(or string (format nil "~A is not of type ~A" (cadr form) (caddr form)))))))
    ;; (signals-error form type) → (handler-case (progn form nil) (error (c) t))
    ((and (eq (car form) 'signals-error) (cdr form) (cddr form))
     (let ((body (rewrite-reader-forms (cadr form))))
       `(handler-case (progn ,body nil) (error (c) t))))
    ;; (signals-error-always form type) →
    ;;   (let ((r (handler-case (progn form nil) (t (c) t)))) (values r r))
    ;; ANSI definition produces TWO values (one per :safety level); tests
    ;; using multiple-value-list expect (T T) not (T).  Single-eval to
    ;; avoid doubling work and to match the AArch64 build (commit 26ee8ef).
    ((and (eq (car form) 'signals-error-always) (cdr form))
     (let ((body (rewrite-reader-forms (cadr form))))
       `(let ((%sea-r (handler-case (progn ,body nil) (t (c) t))))
          (values %sea-r %sea-r))))
    ;; (classify-error form) → nil stub
    ((eq (car form) 'classify-error)
     nil)
    ;; (classify-error* form) → nil stub
    ((eq (car form) 'classify-error*)
     nil)
    ;; (check-type-error fn type) → nil stub
    ((and (eq (car form) 'check-type-error) (cdr form))
     nil)
    ;; (def-syntax-test name form expected...) → (deftest name (with-standard-io-syntax ...) expected...)
    ;; We handle this by making def-syntax-test a known form
    ((and (eq (car form) 'def-syntax-test) (cdr form) (cddr form))
     (let ((name (cadr form))
           (test-form (rewrite-reader-forms (caddr form)))
           (expected (mapcar #'rewrite-reader-forms (cdddr form))))
       `(deftest ,name
          (%with-standard-io-syntax
            (lambda () (let ((*package* (find-package "CL-TEST"))) ,test-form)))
          ,@expected)))
    ;; (psetq) — empty form returns NIL (CLHS).
    ((and (eq (car form) 'psetq) (null (cdr form)))
     nil)

    ;; (psetq var1 val1 var2 val2 ...) → evaluate all values, then set all
    ;; Parallel setq: (let ((t1 v1) (t2 v2) ...) (setq var1 t1) (setq var2 t2) ...)
    ((and (eq (car form) 'psetq) (consp (cdr form)))
     (let* ((pairs (cdr form))
            (vars nil)
            (vals nil)
            (tmps nil))
       ;; Collect pairs
       (let ((p pairs))
         (loop
           (when (null p) (return))
           (push (car p) vars)
           (push (rewrite-reader-forms (cadr p)) vals)
           (push (intern (format nil "%PSETQ-TMP-~D" (length vars))) tmps)
           (setq p (cddr p))))
       (let ((bindings (mapcar #'list (nreverse tmps) (nreverse vals)))
             (assignments (mapcar (lambda (var tmp) `(setq ,var ,tmp))
                                  (nreverse vars) (nreverse tmps))))
         `(let ,bindings ,@assignments nil))))

    ;; (psetf ...) — NO rewrite.  The MVM compiler's PSETF macro now
    ;; expands places via mvm-place-expansion (CLHS 5.1.1.1: each place
    ;; subform evaluated exactly once, left-to-right, value forms
    ;; interleaved), which the old %PSETF-TMP rewriter got wrong — it
    ;; bound all VALUES first, re-evaluating place subforms and reordering
    ;; them relative to the value forms (broke psetf.order.{1,2}).  Pass
    ;; the form through so the real compiler macro handles it.

    ;; (multiple-value-bind* (vars...) form &body body)
    ;; → (let ((tmp (multiple-value-list form)))
    ;;      (check-values-length tmp N 'form)
    ;;      (destructuring-bind (vars...) tmp body...))
    ;; Simplified: just use multiple-value-bind
    ((and (eq (car form) 'multiple-value-bind*) (consp (cdr form)) (consp (cadr form)))
     (let* ((vars (cadr form))
            (expr (rewrite-reader-forms (caddr form)))
            (body (mapcar #'rewrite-reader-forms (cdddr form)))
            (n (length vars))
            (tmp-var '%mvb*-tmp))
       `(let ((,tmp-var (multiple-value-list ,expr)))
          (check-values-length ,tmp-var ,n ',expr)
          (let ,(loop for var in vars for i from 0
                      collect `(,var (if (< ,i (length ,tmp-var))
                                         (nth ,i ,tmp-var)
                                         nil)))
            ,@body))))

    ;; (symbol-macrolet ((name expansion)*) body...) — PRESERVE the wrapper
    ;; and recurse the rewriter into the body.  The compiler's
    ;; SYMBOL-MACROLET handler (mvm/compiler.lisp) extends the compile-env
    ;; with :symbol-macro bindings, so variable references and SETF on the
    ;; names expand correctly (incl. nested shadowing and lexical-var
    ;; references).  The previous `(progn body…)` rewrite DROPPED the
    ;; bindings entirely — every symbol-macro name then compiled as an
    ;; unbound variable returning NIL (symbol-macrolet.4 nested shadowing,
    ;; .5 lexical-ref, etc.).  We do NOT rewrite the expansion forms (they
    ;; are places/lexical refs the compiler resolves), only the body.
    ((eq (car form) 'symbol-macrolet)
     (let ((bindings (cadr form))
           (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(symbol-macrolet ,bindings ,@body)))
    ;; (macrolet (bindings...) body...) — PRESERVE the macrolet wrapper AND
    ;; pre-expand local-macro calls in the body with a CLHS-correct macro
    ;; lambda-list expander.
    ;;
    ;; Why both?  Two distinct test populations:
    ;;   1. macrolet.lsp's &WHOLE / &ENVIRONMENT / nested-destructuring tests —
    ;;      need the WRAPPER so the compiler's compile-macrolet (which builds a
    ;;      correct expander via build-macrolet-expander) handles them.  The
    ;;      old build-side expander used an ORDINARY lambda-list `(lambda
    ;;      ,args …)`, erroring on &WHOLE/&ENVIRONMENT; the error was swallowed
    ;;      AND the wrapper was replaced by `(progn …)`, so the local macro
    ;;      compiled to a bare NIL-returning call.
    ;;   2. places/rotatef/incf/pop/… tests that use a local macro INSIDE a
    ;;      generalized-variable place, e.g.
    ;;        (macrolet ((%m (z) z)) (rotatef (expand-in-current-env (%m x)) y))
    ;;      The modify-macro analyses its place argument before the compiler's
    ;;      macroexpansion runs, so the place macro must already be expanded
    ;;      (%m x) → x at build time, else the place is unrecognised.
    ;; Pre-expanding here (with a &WHOLE-aware expander so it can't error out)
    ;; satisfies (2); keeping the wrapper satisfies (1).  Pre-expansion is a
    ;; semantic no-op for (1) — those tests reference the local macro only in
    ;; ordinary (already-handled) positions.
    ((eq (car form) 'macrolet)
     (let* ((bindings (cadr form))
            (body (cddr form))
            ;; Only pre-expand local macros with a SIMPLE ordinary lambda-list
            ;; (plain required vars: no &WHOLE/&ENVIRONMENT/&REST/&KEY/&OPTIONAL
            ;; and no nested-destructuring sub-patterns).  These are the
            ;; place-macro shapes like (%m (z) z) used inside rotatef/incf/pop
            ;; that must be expanded before modify-macro analysis.  Complex
            ;; macro lambda-lists (incl. &WHOLE, which can re-emit its own
            ;; whole form and loop expand-one) are left to the preserved
            ;; wrapper + compile-macrolet — pre-expanding them here would
            ;; either error or recurse to the depth cap producing garbage.
            (expanders
             (mapcan (lambda (b)
                       (let ((args (cadr b)))
                         (if (and (listp args)
                                  (every (lambda (a)
                                           (and (symbolp a)
                                                (not (and (> (length (symbol-name a)) 0)
                                                          (char= (char (symbol-name a) 0) #\&)))))
                                         args))
                             (handler-case
                               (list (cons (car b)
                                           (%build-reader-macrolet-expander
                                            args (cddr b))))
                               (error () nil))
                             nil)))
                     bindings))
            (expanded-body
             (if expanders
                 (labels ((expand-one (f depth)
                            (cond
                              ((> depth 50) f)
                              ((atom f) f)
                              ((and (consp f) (symbolp (car f))
                                    (assoc (car f) expanders))
                               (let* ((expander (cdr (assoc (car f) expanders)))
                                      (result (handler-case
                                                (funcall expander f)
                                                (error () f))))
                                 (if (equal result f)
                                     (mapcar-dotted (lambda (x) (expand-one x (1+ depth))) f)
                                     (expand-one result (1+ depth)))))
                              (t (mapcar-dotted (lambda (x) (expand-one x depth)) f)))))
                   (mapcar (lambda (x) (expand-one x 0)) body))
                 body))
            (rewritten-body (mapcar #'rewrite-reader-forms expanded-body)))
       ;; Keep the wrapper so compile-macrolet sees the local macros too.
       `(macrolet ,bindings ,@rewritten-body)))
    ;; (do-special-strings (var string-form ret-form) body...) → (let ((var string-form)) body... ret-form)
    ((and (eq (car form) 'do-special-strings) (consp (cdr form)) (consp (cadr form)))
     (let* ((binding (cadr form))
            (var (first binding))
            (string-form (rewrite-reader-forms (second binding)))
            (ret-form (if (cddr binding) (rewrite-reader-forms (third binding)) nil))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(let ((,var ,string-form)) ,@body ,ret-form)))
    ;; (do-special-integer-vectors (var vec-form ret-form) body...) →
    ;;   (let ((var vec-form)) body... ret-form)
    ;; The aux macro iterates over fill-pointer/adjust/etype/displace
    ;; combinations to stress different vector representations; for
    ;; tests that just want one round to confirm the body's invariant
    ;; (assert, eql, length, etc.) the single-iteration shape is
    ;; enough.  Tests that depend on hitting EVERY variant still fail
    ;; on Modus today, but the simpler ones gate behind this rewriter.
    ((and (eq (car form) 'do-special-integer-vectors)
          (consp (cdr form)) (consp (cadr form)))
     (let* ((binding (cadr form))
            (var (first binding))
            (vec-form (rewrite-reader-forms (second binding)))
            (ret-form (if (cddr binding) (rewrite-reader-forms (third binding)) nil))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(let ((,var ,vec-form)) ,@body ,ret-form)))
    ;; (flet ((name (args) body)) outer-body)
    ;; Leave as-is but rewrite bodies
    ((eq (car form) 'flet)
     (let ((bindings (mapcar (lambda (b)
                               (if (consp b)
                                   (cons (car b)
                                         (cons (cadr b)
                                               (mapcar #'rewrite-reader-forms (cddr b))))
                                   b))
                             (cadr form)))
           (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(flet ,bindings ,@body)))
    ;; (handler-case body &rest clauses) — normalize class objects to type names
    ((and (eq (car form) 'handler-case) (cdr form))
     (let* ((body (rewrite-reader-forms (cadr form)))
            (clauses (mapcar (lambda (clause)
                               (if (consp clause)
                                   (let* ((type-spec (car clause))
                                          ;; Normalize SBCL class objects to their names
                                          (norm-type
                                           (cond
                                             ((and (not (symbolp type-spec))
                                                   (not (consp type-spec))
                                                   (typep type-spec 'class))
                                              (class-name type-spec))
                                             (t type-spec)))
                                          (rest (mapcar #'rewrite-reader-forms (cdr clause))))
                                     (cons norm-type rest))
                                   clause))
                             (cddr form))))
       `(handler-case ,body ,@clauses)))
    ;; (define-condition name parents slots &rest options)
    ;; → (%define-condition ...) + reader defuns
    ;; RETIRED: handled by compile-time macro in compiler.lisp via
    ;; mvm-define-macro "DEFINE-CONDITION".  Gated off here.
    ((and nil (eq (car form) 'define-condition) (cdr form))
     (rewrite-reader-forms (rewrite-define-condition form)))
    ;; (define-condition-with-tests name parents slots &rest options)
    ;; → expand macro inline → (%define-condition ...) + tests
    ((and (eq (car form) 'define-condition-with-tests) (cdr form))
     (rewrite-reader-forms (rewrite-define-condition-with-tests form)))
    ;; (normally form) → form (since *should-always-be-true* is always T)
    ((and (eq (car form) 'normally) (cdr form))
     (rewrite-reader-forms (cadr form)))
    ;; (report-and-ignore-errors form...) → (handler-case (progn form...) (error () nil))
    ((and (eq (car form) 'report-and-ignore-errors) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (cdr form))))
       `(handler-case (progn ,@body) (error () nil))))
    ;; (handler-bind bindings body...) rewrite RETIRED (cecf4f3): the
    ;; compiler now has a HANDLER-BIND special form (compile-handler-bind)
    ;; that produces the same %with-handler-bind expansion, so raw test
    ;; source compiles through the REAL construct — the strongest possible
    ;; validation of the compiled path.  Subforms still get rewritten by
    ;; the elementwise default below.  The tree-walker keeps its own
    ;; HANDLER-BIND branch for runtime-eval'd forms.
    ;; (restart-case form &rest clauses)
    ;; DIFF MODE (mvm-eval gate): leave RESTART-CASE RAW (only rewrite subforms)
    ;; so mvm-eval compiles it via the compile-restart-case SPECIAL FORM, keeping
    ;; it IN BYTECODE instead of routing through the native %with-restarts
    ;; bridge (which corrupts mvm-interpret's loop state on return).  The
    ;; tree-walker (cl-eval.lisp) handles raw restart-case too, so the diff
    ;; gate's `eval` side stays correct.
    ;; NON-DIFF MODE (native production build): keep the historical rewrite to
    ;; (%with-restarts …) — the native run-ansi-* runners rely on it and the
    ;; existing restart-case tests pass through it; not touching that path
    ;; keeps the native ANSI gate byte-for-byte unchanged.
    ((and (eq (car form) 'restart-case) (cdr form) *mvm-eval-diff-mode*)
     (let* ((protected-form (rewrite-reader-forms (cadr form)))
            (clauses (cddr form))
            (new-clauses
             (mapcar (lambda (clause)
                       (let* ((rname (first clause))
                              (arglist (second clause))
                              (rest-opts (cddr clause))
                              (opts nil)
                              (body-forms nil))
                         ;; Preserve leading :report/:interactive/:test options
                         ;; verbatim; rewrite only the clause BODY forms.
                         (let ((remaining rest-opts))
                           (loop
                             (when (or (null remaining)
                                       (not (keywordp (car remaining))))
                               (setf body-forms remaining)
                               (return))
                             (cond
                               ((member (car remaining)
                                        '(:report :interactive :test))
                                (setf opts (append opts
                                                   (list (car remaining)
                                                         (cadr remaining))))
                                (setf remaining (cddr remaining)))
                               (t (setf body-forms remaining) (return)))))
                         `(,rname ,arglist ,@opts
                                  ,@(mapcar #'rewrite-reader-forms body-forms))))
                     clauses)))
       `(restart-case ,protected-form ,@new-clauses)))
    ;; (restart-case form &rest clauses)  [NON-DIFF: rewrite to %with-restarts]
    ;; → (%with-restarts restarts-list (lambda () form))
    ;; Each clause: (name (args) &key interactive test report . body)
    ((and (eq (car form) 'restart-case) (cdr form))
     (let* ((protected-form (rewrite-reader-forms (cadr form)))
            (clauses (cddr form))
            (restart-forms
             (mapcar (lambda (clause)
                       (let* ((rname (first clause))
                              (args (second clause))
                              (rest-opts (cddr clause))
                              ;; Extract :report, :interactive, :test options
                              (report-opt nil)
                              (interactive-opt nil)
                              (test-opt nil)
                              (body-forms nil))
                         ;; Separate options from body
                         (let ((remaining rest-opts))
                           (loop
                             (when (or (null remaining)
                                       (not (keywordp (car remaining))))
                               (setf body-forms remaining)
                               (return))
                             (cond
                               ((eq (car remaining) :report)
                                (setf report-opt (cadr remaining))
                                (setf remaining (cddr remaining)))
                               ((eq (car remaining) :interactive)
                                (setf interactive-opt (cadr remaining))
                                (setf remaining (cddr remaining)))
                               ((eq (car remaining) :test)
                                (setf test-opt (cadr remaining))
                                (setf remaining (cddr remaining)))
                               (t
                                (setf body-forms remaining)
                                (return)))))
                         ;; Coerce an option value (symbol / lambda / string)
                         ;; into a form yielding a function (or string for
                         ;; :report).  Used for :report, :interactive, :test.
                         (flet ((opt-fn-form (opt allow-string)
                                  (cond
                                    ((null opt) nil)
                                    ((and allow-string (stringp opt)) `',opt)
                                    ((symbolp opt) `#',opt)
                                    ((and (consp opt) (eq (car opt) 'lambda)) opt)
                                    ((and (consp opt) (eq (car opt) 'function)) opt)
                                    (t (rewrite-reader-forms opt)))))
                           (let* ((body (mapcar #'rewrite-reader-forms body-forms))
                                  (fn-form `(lambda ,args ,@body))
                                  (report-form (opt-fn-form report-opt t))
                                  (interactive-form (opt-fn-form interactive-opt nil))
                                  (test-form (opt-fn-form test-opt nil)))
                             ;; Cell shape passed to %with-restarts:
                             ;; (NAME FN REPORT INTERACTIVE TEST).  %with-restarts
                             ;; re-wraps as (NAME FN REPORT INTERACTIVE TEST :CASE).
                             ;; Trailing NILs are harmless.
                             `(list ',rname ,fn-form ,report-form
                                    ,interactive-form ,test-form)))))
                     clauses)))
       `(%with-restarts (list ,@restart-forms) (lambda () ,protected-form))))
    ;; (restart-bind bindings body...)
    ;; → (%push-restarts restarts (lambda () body...))
    ((and (eq (car form) 'restart-bind) (cdr form))
     (let* ((bindings (cadr form))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (restart-forms
             (mapcar (lambda (b)
                       (let* ((rname (first b))
                              (fn (rewrite-reader-forms (second b)))
                              (opts (cddr b))
                              (report-opt (getf opts :report-function)))
                         (if report-opt
                             `(list ',rname ,fn ,report-opt)
                             `(list ',rname ,fn nil))))
                     bindings)))
       `(%push-restarts (list ,@restart-forms) (lambda () ,@body))))
    ;; (with-condition-restarts condition restarts-form body...)
    ;; stub: just execute body
    ((and (eq (car form) 'with-condition-restarts) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (nthcdr 3 form))))
       (if body
           `(progn ,@body)
           nil)))

    ;; ---- Minimal CLOS support ----

    ;; (defclass name supers slots &rest options)
    ;; → (%defclass 'name '(slot-names...) '(supers...)) + reader/accessor/writer defuns
    ((and (eq (car form) 'defclass) (cdr form) (cddr form))
     (let* ((class-name (cadr form))
            (raw-supers (caddr form))  ; list of parent class names
            (raw-slots (or (cadddr form) nil))
            (rest-opts (cddddr form))
            ;; Parse slot specs
            (slot-names nil)
            (extra-defuns nil)
            ;; initarg→slot mapping: list of (initarg-string . slot-name)
            (initarg-map nil)
            ;; initform map: list of (slot-name . form)
            (initform-map nil)
            ;; ANSI defclass errors: signal a program-error at runtime
            ;; if any of these structural defects are detected.
            (defect-msg nil))
       ;; Detect duplicate slot names
       (let ((seen nil))
         (dolist (slot-spec raw-slots)
           (let ((sname (if (consp slot-spec) (car slot-spec) slot-spec)))
             (when (and sname (member sname seen))
               (setq defect-msg "duplicate slot name in defclass"))
             (push sname seen))))
       ;; Detect duplicate :initform/:type/:documentation/:allocation
       ;; within a single slot spec (ANSI requires program-error).
       (dolist (slot-spec raw-slots)
         (when (consp slot-spec)
           (let ((opts (cdr slot-spec))
                 (n-initform 0) (n-type 0) (n-doc 0) (n-alloc 0))
             (let ((cur opts))
               (loop
                 (when (or (null cur) (null (cdr cur))) (return))
                 (let ((key (car cur)))
                   (cond
                     ((eq key :initform) (incf n-initform))
                     ((eq key :type) (incf n-type))
                     ((eq key :documentation) (incf n-doc))
                     ((eq key :allocation) (incf n-alloc))))
                 (setq cur (cddr cur))))
             (when (or (> n-initform 1) (> n-type 1)
                       (> n-doc 1) (> n-alloc 1))
               (setq defect-msg "duplicate slot option in defclass")))))
       ;; Detect duplicate :default-initargs key in class options
       (dolist (opt rest-opts)
         (when (and (consp opt) (eq (car opt) :default-initargs))
           (let ((seen nil) (cur (cdr opt)))
             (loop
               (when (or (null cur) (null (cdr cur))) (return))
               (let ((k (car cur)))
                 (when (member k seen)
                   (setq defect-msg "duplicate :default-initargs key"))
                 (push k seen))
               (setq cur (cddr cur))))))
       ;; class-slots: per-slot :allocation tracking (closed over by the emit form below).
       ;; default-initarg-forms: list of (key . value-form) from any
       ;;   (:default-initargs k1 v1 ...) class option — emit form wraps
       ;;   each value in a thunk so CLHS 7.1.4's re-eval-per-call holds.
       (let ((class-slots nil)
             (default-initarg-forms nil))
         (dolist (opt rest-opts)
           (when (and (consp opt) (eq (car opt) :default-initargs))
             (let ((cur (cdr opt)))
               (loop
                 (when (or (null cur) (null (cdr cur))) (return))
                 (let ((k (car cur))
                       (v (cadr cur)))
                   (push (cons k v) default-initarg-forms))
                 (setq cur (cddr cur))))))
         (setq default-initarg-forms (nreverse default-initarg-forms))
       ;; Process each slot spec
       (dolist (slot-spec raw-slots)
         (let* ((sname (if (consp slot-spec) (car slot-spec) slot-spec))
                (opts (if (consp slot-spec) (cdr slot-spec) nil)))
           (push sname slot-names)
           ;; Extract :reader, :writer, :accessor, :initarg, :initform, :allocation
           (let ((cur opts))
             (loop
               (when (null cur) (return))
               (let ((key (car cur))
                     (val (cadr cur)))
                 (cond
                   ((eq key :allocation)
                    ;; :allocation :class — slot is class-shared.  :instance is default.
                    (when (eq val :class)
                      (push sname class-slots)))
                   ((eq key :reader)
                    (push `(defun ,val (obj) (slot-value obj ',sname)) extra-defuns)
                    ;; Register so (typep #',val 'generic-function) → T
                    (push `(%register-gf-fn (function ,val)) extra-defuns))
                   ((eq key :accessor)
                    (push `(defun ,val (obj) (slot-value obj ',sname)) extra-defuns)
                    (push `(%register-gf-fn (function ,val)) extra-defuns)
                    ;; Two setter aliases, different arg orders:
                    ;;   SET-NAME (obj val)  — what compiler.lisp's SETF macro
                    ;;     fallback emits for (setf (NAME obj) val) → (SET-NAME obj val)
                    ;;   SETF-NAME (val obj) — what compile-function-ref resolves
                    ;;     #'(setf NAME) to (lookup "SETF-NAME") and what an
                    ;;     ANSI SETF expansion would funcall as (val place-args...)
                    ;; Both register so (typep #' on either) → T.
                    (let ((set-name (intern (concatenate 'string "SET-" (symbol-name val))))
                          (setf-name (intern (concatenate 'string "SETF-" (symbol-name val)))))
                      (push `(defun ,set-name (obj nv) (set-slot-value obj ',sname nv)) extra-defuns)
                      (push `(%register-gf-fn (function ,set-name)) extra-defuns)
                      (push `(defun ,setf-name (nv obj) (set-slot-value obj ',sname nv)) extra-defuns)
                      (push `(%register-gf-fn (function ,setf-name)) extra-defuns)))
                   ((eq key :writer)
                    ;; writer: (fn new-value object)
                    (push `(defun ,val (nv obj) (set-slot-value obj ',sname nv)) extra-defuns)
                    (push `(%register-gf-fn (function ,val)) extra-defuns))
                   ((eq key :initarg)
                    ;; val is a keyword like :b; map to slot name
                    (push (cons (symbol-name val) sname) initarg-map))
                   ((eq key :initform)
                    ;; Save the form; it'll be wrapped in a thunk at expansion
                    (push (cons sname val) initform-map))))
               (setq cur (cddr cur))))))
       (let* ((slot-list (nreverse slot-names))
              ;; Build (initarg-keyword . slot-name) cons pairs as quoted forms.
              ;; We store the keyword symbol itself (not its name string) so
              ;; runtime comparison works with bare-metal native MVM symbols
              ;; (where symbol-name returns "" for native syms — their identity
              ;; is the hash). Keyword like :b2 will be re-interned by the
              ;; reader to a sym with matching hash.
              (initarg-pairs
               (mapcar (lambda (p)
                         (let ((kw-sym (intern (car p) :keyword)))
                           `(cons ',kw-sym ',(cdr p))))
                       initarg-map))
              ;; Build (slot-name . thunk) pairs; thunk evaluates the initform
              (initform-pairs
               (mapcar (lambda (p)
                         `(cons ',(car p)
                                (lambda () ,(rewrite-reader-forms (cdr p)))))
                       initform-map))
              ;; Build (initarg-keyword . thunk) pairs for default-initargs.
              ;; Per CLHS 7.1.4 the value form is re-evaluated each call, so
              ;; each value gets wrapped in a 0-arity thunk.
              (default-initarg-pairs
               (mapcar (lambda (p)
                         `(cons ',(car p)
                                (lambda () ,(rewrite-reader-forms (cdr p)))))
                       default-initarg-forms)))
         ;; Register in SBCL-side class registry for make-instance expansion
         (setf *sbcl-clos-classes*
               (cons (cons class-name (cons slot-list initarg-map))
                     *sbcl-clos-classes*))
         (if defect-msg
             ;; ANSI: signal program-error so signals-error catches it.
             ;; Don't register the broken class.
             `(error ,defect-msg)
             `(progn
                (%defclass ',class-name ',slot-list ',raw-supers)
                (%register-clos-slot-info ',class-name
                                          (list ,@initarg-pairs)
                                          (list ,@initform-pairs))
                ;; Register directly-declared slot names for SLOT-CLASS-OWNER
                ;; shadow detection (CLHS 7.5.2 — subclass :allocation
                ;; :instance hides ancestor :allocation :class).  Always
                ;; emit so re-defining a class refreshes the list.
                (%register-clos-direct-slots ',class-name ',slot-list)
                ;; Register :allocation :class slot names so slot-value /
                ;; set-slot-value can route them to per-class storage.
                ;; Always emit (even empty) so re-defining clears prior.
                (%register-clos-class-slots ',class-name
                                            ',(nreverse class-slots))
                ;; Register default-initargs so make-instance can apply them
                ;; per CLHS 7.1.4.  Always emit (even empty list) so
                ;; redefining a class clears any prior entry.
                (%register-clos-default-initargs ',class-name
                                                 (list ,@default-initarg-pairs))
                ,@(mapcar #'rewrite-reader-forms (nreverse extra-defuns))
                ;; CLHS 7.7: defclass evaluates to the class object (so
                ;; (eval '(defclass …)) and the rewritten (eval-quote
                ;; unwrap) hand back something class-name / eqt can use).
                ;; class-redefinition.1/2 etc. assert (class-name cobj)
                ;; and (eqt cobj1 cobj3); the progn previously returned the
                ;; last registration's value (NIL).
                (find-class ',class-name nil)))))))

    ;; (defgeneric name lambda-list &rest options)
    ;; → (%defgeneric 'name 'lambda-list combination)
    ;;   + (defun name (&rest %gf-args) (%gf-dispatch 'name %gf-args))
    ;; Also handles inline (:method ...) options and :method-combination.
    ((and (eq (car form) 'defgeneric) (cdr form))
     (let* ((gf-name (cadr form))
            (lambda-list (caddr form))
            (options (cdddr form))
            (combination nil)
            (apo nil)
            (inline-methods nil))
       ;; Guard: gf-name must be a symbol or (setf SYM) — NOT a comma
       ;; struct from a quasiquoted (defgeneric ,sym ...) inside (eval
       ;; `...).  Backquoted defgeneric is a runtime form that must hit
       ;; the cl-eval.lisp DEFGENERIC handler (which does the CLHS
       ;; macro/special-operator/ordinary-fn name checks —
       ;; defgeneric.error.1/2/3).  Same guard as the defmethod
       ;; rewriter below.
       (unless (or (symbolp gf-name)
                   (and (consp gf-name) (eq (car gf-name) 'setf)))
         (return-from rewrite-reader-forms form))
       (dolist (opt options)
         (when (consp opt)
           (cond
             ((eq (car opt) :method-combination)
              ;; (:method-combination NAME [args...]) — three encodings:
              ;;  - bare NAME (no args): the symbol NAME
              ;;  - short form with :most-specific-last: (NAME . :MOST-SPECIFIC-LAST)
              ;;  - long form with combination args: (NAME arg1 arg2 ...)
              ;;    so %gf-dispatch-custom-long can read (cdr comb-raw) as the
              ;;    combination args list and APPLY them to the builder's
              ;;    lambda-list.
              (let ((cargs (cddr opt)))
                (setq combination
                      (cond
                        ((null cargs) (cadr opt))
                        ((and (null (cdr cargs)) (eq (car cargs) :most-specific-last))
                         (cons (cadr opt) :most-specific-last))
                        (t (cons (cadr opt) cargs))))))
             ((eq (car opt) :argument-precedence-order)
              (setq apo (cdr opt)))
             ((eq (car opt) :method)
              (push opt inline-methods)))))
       ;; Simplified options for the runtime validator: keep option
       ;; heads + structural args, drop method BODIES and doc strings
       ;; (the validator only needs lambda-list shapes / counts).
       (let* ((simplified-options
               (mapcar (lambda (opt)
                         (cond
                           ((not (consp opt)) opt)
                           ((eq (car opt) :method)
                            (let ((rest (cdr opt)))
                              (loop while (and rest (symbolp (car rest))
                                               (not (listp (car rest))))
                                    do (pop rest))
                              (list :method (car rest))))
                           ((eq (car opt) :documentation) (list :documentation))
                           (t opt)))
                       options))
              (method-counter 0)
              (method-forms
               (mapcar (lambda (mopt)
                         ;; mopt = (:method [qualifier] specialized-ll body...)
                         (setf method-counter (1+ method-counter))
                         (let* ((rest (cdr mopt))
                                ;; qualifier: non-list symbol that is not the lambda list
                                (has-qualifier (and rest (cdr rest) (symbolp (car rest))
                                                    (not (listp (car rest)))))
                                (qualifier (if has-qualifier (car rest) nil))
                                (rest2 (if has-qualifier (cdr rest) rest))
                                (sll (car rest2))
                                (body (cdr rest2))
                                ;; Required params = everything before the first
                                ;; lambda-list keyword.  (The old remove-if kept
                                ;; &optional/&key params too, so every optional
                                ;; var became a bogus T specializer and tripped
                                ;; %defmethod's CLHS 7.6.4 specializer-count
                                ;; check — defgeneric.8/9/10+ all program-error'd
                                ;; at definition time.)
                                (required (loop for p in sll
                                                until (and (symbolp p)
                                                           (member p lambda-list-keywords))
                                                collect p))
                                ;; Specializers from the required params only
                                (specs
                                 (mapcar (lambda (p)
                                           (cond
                                             ((consp p)
                                              (let ((spec (cadr p)))
                                                (if (and (consp spec) (eq (car spec) 'eql))
                                                  `(list 'eql ,(rewrite-reader-forms (cadr spec)))
                                                  `',spec)))
                                             (t ''t)))
                                         required))
                                ;; Params: strip specializers from required
                                ;; params; keep &optional/&key/&aux entries
                                ;; INTACT (defaults + supplied-p vars — the old
                                ;; (car p) flattening silently dropped defaults,
                                ;; so (y 10) bound y to NIL).  Default forms get
                                ;; the same reader-form rewriting as the body.
                                (params
                                 (let* ((seen-amp nil)
                                        (base
                                         (mapcar (lambda (p)
                                                   (cond
                                                     ((and (symbolp p)
                                                           (member p lambda-list-keywords))
                                                      (setq seen-amp t)
                                                      p)
                                                     ((not seen-amp)
                                                      (if (consp p) (car p) p))
                                                     ((and (consp p) (cdr p))
                                                      (cons (car p)
                                                            (cons (rewrite-reader-forms (cadr p))
                                                                  (cddr p))))
                                                     (t p)))
                                                 sll)))
                                   ;; CLHS 7.6.5: keyword validity for a GF
                                   ;; call is checked against the UNION of
                                   ;; the GF's and the applicable methods'
                                   ;; keys (%gf-check-keys does that at
                                   ;; dispatch).  The method LAMBDA itself
                                   ;; must therefore be lenient — without
                                   ;; &allow-other-keys, the compiled &key
                                   ;; binder's %validate-kw-list signaled on
                                   ;; keys belonging to OTHER methods
                                   ;; (defgeneric.14/28/29 crashes).  Insert
                                   ;; it after the &key section (before &aux
                                   ;; if present).
                                   (if (and (member '&key base)
                                            (not (member '&allow-other-keys base)))
                                       (let ((aux-tail (member '&aux base)))
                                         (if aux-tail
                                             (append (ldiff base aux-tail)
                                                     '(&allow-other-keys)
                                                     aux-tail)
                                             (append base '(&allow-other-keys))))
                                       base)))
                                ;; Lambda-list SHAPE for runtime key-checking
                                ;; meta ((x number) → x, ((:bar foo) 'a) → (:bar foo))
                                (meta-ll (mapcar (lambda (p)
                                                   (if (and (consp p)
                                                            (not (and (symbolp (car p))
                                                                      (member (car p) lambda-list-keywords))))
                                                       (car p)
                                                       p))
                                                 sll))
                                ;; CLHS 7.6.5: method bodies are wrapped in a
                                ;; BLOCK named after the GF (defgeneric.7's
                                ;; return-from).  Only wrap when the body
                                ;; actually mentions RETURN-FROM — a BLOCK
                                ;; around a (values ...) tail risks collapsing
                                ;; multiple values in the MVM compiler.
                                (block-name (if (and (consp gf-name)
                                                     (eq (car gf-name) 'setf))
                                                (cadr gf-name)
                                                gf-name))
                                (needs-block
                                 (let ((stack (list body)) (found nil))
                                   (loop while stack do
                                     (let ((f (pop stack)))
                                       (cond
                                         ((eq f 'return-from)
                                          (setq found t) (setq stack nil))
                                         ((consp f)
                                          (push (car f) stack)
                                          (push (cdr f) stack)))))
                                   found))
                                (rewritten-body (mapcar #'rewrite-reader-forms body)))
                           `(%defgeneric-method ',gf-name ',(if qualifier qualifier nil)
                                        (list ,@specs)
                                        (lambda ,params
                                          ,@(if needs-block
                                                `((block ,block-name ,@rewritten-body))
                                                rewritten-body))
                                        ',meta-ll)))
                       (nreverse inline-methods))))
         `(progn
            ;; CLHS option validation FIRST — program-error on duplicate /
            ;; unknown options, bad :argument-precedence-order, or inline
            ;; methods not congruent with the lambda-list.
            (%validate-defgeneric-options ',gf-name ',lambda-list
                                          ',simplified-options)
            (%defgeneric ',gf-name ',lambda-list ',(if combination combination nil))
            ,@(when apo
                `((%gf-set-arg-precedence ',gf-name ',apo ',lambda-list)))
            (defun ,gf-name (&rest %gf-args)
              (%gf-dispatch ',gf-name %gf-args))
            ;; (defgeneric (setf X) ...): also emit the SET-X alias the
            ;; compiler's generic-setf expansion calls for unknown places
            ;; — (setf (X args...) v) compiles to (SET-X args... v), but
            ;; the CLHS (setf X) function signature is (NEW-VALUE args...),
            ;; so the alias rotates the value to the front (defgeneric.33).
            ,@(when (and (consp gf-name) (eq (car gf-name) 'setf)
                         (symbolp (cadr gf-name)))
                (let ((alias (intern (format nil "SET-~A"
                                             (symbol-name (cadr gf-name)))
                                     (symbol-package (cadr gf-name)))))
                  `((defun ,alias (&rest %gf-args)
                      (%gf-dispatch ',gf-name
                                    (cons (car (last %gf-args))
                                          (butlast %gf-args)))))))
            ;; Register the dispatch defun's fn-addr so
            ;; (typep #',gf-name 'generic-function) → T (cl-clos.lisp's
            ;; %generic-function-p consults *gf-stub-closures*), and record
            ;; the fn → name mapping so COMPUTE-APPLICABLE-METHODS /
            ;; FIND-METHOD / DOCUMENTATION can resolve it back to the GF.
            ;; handler-case wrap: when defgeneric is INSIDE a lambda body
            ;; (eg DG-MC tests inline both defgeneric and the test call),
            ;; (function ,gf-name) at build time may resolve to 0 because
            ;; the just-defined defun isn't visible to the function-ref
            ;; compiler.  Don't take the whole lambda down with us.
            (handler-case (%register-gf-fn (function ,gf-name) ',gf-name)
              (t (c) nil))
            ,@method-forms
            ;; ANSI: defgeneric returns the GF.  Return the dispatch
            ;; DEFUN (#'NAME, registered → name above, so it typep's as
            ;; generic-function) rather than the raw GF struct: a real
            ;; function pointer is funcall/apply-able at ANY arity,
            ;; whereas compile-funcall's GF-struct path caps at 4 args
            ;; (defgeneric.14/22/28 do 5-8-arg funcalls on the value).
            ;; %dg-gf-callable falls back to the GF struct when the
            ;; (function NAME) registration resolved to 0 (defgeneric
            ;; nested inside a lambda body).  Single plain call — no
            ;; handler-case/let in value position, which miscompiled in
            ;; let-initializer contexts.
            (%dg-gf-callable ',gf-name)))))

    ;; (define-method-combination name &rest options)
    ;; SHORT form: (define-method-combination name :operator op
    ;;               :documentation ... :identity-with-one-argument t)
    ;;   — the args after NAME start with a keyword, or there are none.
    ;; LONG form:  (define-method-combination name (lambda-list)
    ;;               ((group-var qualifier-pattern... [:order o] [:required r]
    ;;                 [:description d]) ...)
    ;;               [(:arguments ...)] [(:generic-function ...)]
    ;;               [declarations] [doc] body...)
    ;;   — the first arg after NAME is the combination lambda-list (a list
    ;;     or NIL), never a keyword.
    ((and (eq (car form) 'define-method-combination) (cdr form))
     (let* ((mc-name (cadr form))
            (options (cddr form)))
       (if (or (null options) (keywordp (car options)))
           ;; ---- SHORT FORM ----
           (let ((operator mc-name)
                 (identity-with-one nil))
             (let ((cur options))
               (loop
                 (when (null cur) (return))
                 (let ((key (car cur)) (val (cadr cur)))
                   (cond
                     ((eq key :operator) (setq operator val))
                     ((eq key :identity-with-one-argument) (setq identity-with-one val))
                     ((eq key :documentation) nil)  ; ignored
                     (t nil)))
                 (setq cur (cddr cur))))
             `(%define-method-combination ',mc-name ',operator ,identity-with-one))
           ;; ---- LONG FORM ----
           (rewrite-dmc-long-form mc-name options))))

    ;; (defmethod slot-unbound (...) body...) → defun + %add-slot-unbound-method
    ;; Specializer on obj (2nd param) by class name and slot-name (3rd param)
    ;; We generate a named defun instead of a lambda to avoid MVM closure issues.
    ((and (eq (car form) 'defmethod)
          (cdr form)
          (eq (cadr form) 'slot-unbound)
          (consp (caddr form)))
     (let* ((lambda-list (caddr form))
            (body (cdddr form))
            ;; Extract specializers: ((class spec) (obj spec) (slot-name spec))
            (class-spec (first lambda-list))
            (obj-spec   (second lambda-list))
            (slot-spec  (third lambda-list))
            ;; Get param names
            (class-param (if (consp class-spec) (car class-spec) class-spec))
            (obj-param   (if (consp obj-spec)   (car obj-spec)   obj-spec))
            (slot-param  (if (consp slot-spec)  (car slot-spec)  slot-spec))
            ;; Get obj class specializer
            (obj-class
             (if (and (consp obj-spec) (consp (cadr obj-spec)))
                 ;; (obj class-name) — class specializer
                 (cadr obj-spec)
                 (if (consp obj-spec)
                     (cadr obj-spec)
                     t)))
            ;; Get slot-name specializer: t or (eql 'sym)
            (slot-specializer
             (if (and (consp slot-spec) (consp (cadr slot-spec)))
                 ;; (slot-name (eql 'x)) → extract x
                 (let ((eql-form (cadr slot-spec)))
                   (if (and (consp eql-form)
                            (eq (car eql-form) 'eql)
                            (consp (cadr eql-form))
                            (eq (car (cadr eql-form)) 'quote))
                       ;; (eql 'sym) → sym
                       (cadr (cadr eql-form))
                       nil))
                 nil))
            (rewritten-body (mapcar #'rewrite-reader-forms body))
            ;; Generate unique function name to avoid lambda/closure issues
            (fn-name (intern (format nil "%SLOT-UNBOUND-METHOD-~D"
                                     (incf *slot-unbound-method-counter*))
                             :cl-user))
            ;; Use nil as slot-spec for "match any", or quoted symbol for specific
            (slot-arg (if slot-specializer `',slot-specializer nil)))
       `(progn
          (defun ,fn-name (,class-param ,obj-param ,slot-param)
            ,@rewritten-body)
          (%add-slot-unbound-method ',obj-class ,slot-arg #',fn-name))))

    ;; (defmethod slot-missing (...) body...) → defun + %add-slot-missing-method
    ;; Lambda list: (class obj slot-name operation &optional (new-value nil new-value-p))
    ;; Specializer on obj (2nd param) by class.  We dispatch only on
    ;; obj's class — simpler than the full method protocol.
    ((and (eq (car form) 'defmethod)
          (cdr form)
          (eq (cadr form) 'slot-missing)
          (consp (caddr form)))
     (let* ((lambda-list (caddr form))
            (body (cdddr form))
            (class-spec (first lambda-list))
            (obj-spec   (second lambda-list))
            (slot-spec  (third lambda-list))
            (op-spec    (fourth lambda-list))
            (rest-spec  (nthcdr 4 lambda-list))
            (class-param (if (consp class-spec) (car class-spec) class-spec))
            (obj-param   (if (consp obj-spec)   (car obj-spec)   obj-spec))
            (slot-param  (if (consp slot-spec)  (car slot-spec)  slot-spec))
            (op-param    (if (consp op-spec)    (car op-spec)    op-spec))
            (obj-class
             (if (and (consp obj-spec) (consp (cdr obj-spec)))
                 (cadr obj-spec)
                 t))
            ;; eql-specializers on slot-name / operation (CLHS lets
            ;; slot-missing methods specialize on the slot-name and
            ;; operation, e.g. (slot-name (eql 'not-there)).  Extract the
            ;; eql value so dispatch only fires the method when applicable;
            ;; otherwise an eql-specialized method (registered last, so at
            ;; the front of the registry) shadows the class-only method.
            ;; Form is `((eql 'X))' → spec=(EQL (QUOTE X)) → value X.
            (slot-eql (if (and (consp slot-spec) (consp (cadr slot-spec))
                               (eq (car (cadr slot-spec)) 'eql))
                          (list (cadr (cadr (cadr slot-spec))))
                          :any))
            (op-eql   (if (and (consp op-spec) (consp (cadr op-spec))
                               (eq (car (cadr op-spec)) 'eql))
                          (list (cadr (cadr (cadr op-spec))))
                          :any))
            ;; new-value / supplied-p params.  rest-spec is
            ;; (&optional (new-value nil new-value-p)) or (&optional new-value).
            ;; MVM's funcall doesn't apply &optional defaults reliably when
            ;; fewer args are supplied (the slot reads stack garbage), so we
            ;; emit a FIXED 6-param signature and have %dispatch-slot-missing
            ;; always pass both new-value and the supplied-p flag explicitly.
            (nv-spec (cond ((and rest-spec (eq (car rest-spec) '&optional))
                            (cadr rest-spec))
                           (t nil)))
            (nv-param (cond ((consp nv-spec) (car nv-spec))
                            (nv-spec nv-spec)
                            (t (intern "NEW-VALUE" :cl-user))))
            (nvp-param (if (and (consp nv-spec) (consp (cddr nv-spec)))
                           (caddr nv-spec)
                           (intern "%SM-NEW-VALUE-P" :cl-user)))
            (rewritten-body (mapcar #'rewrite-reader-forms body))
            (fn-name (intern (format nil "%SLOT-MISSING-METHOD-~D"
                                     (incf *slot-unbound-method-counter*))
                             :cl-user)))
       `(progn
          (defun ,fn-name (,class-param ,obj-param ,slot-param ,op-param
                           ,nv-param ,nvp-param)
            ,@rewritten-body)
          (%add-slot-missing-method ',obj-class #',fn-name
                                    ',slot-eql ',op-eql))))

    ;; (defmethod name [qualifier] specialized-lambda-list body...)
    ;; → (%defmethod 'name qualifier '(specializers) (lambda params body))
    ((and (eq (car form) 'defmethod) (cdr form))
     (let* ((gf-name (cadr form))
            (rest (cddr form))
            ;; Check for qualifier: if (car rest) is a non-list symbol, it's a qualifier
            (has-qualifier (and rest (symbolp (car rest)) (not (listp (car rest)))))
            (qualifier (if has-qualifier (car rest) nil))
            (rest2 (if has-qualifier (cdr rest) rest))
            (sll (car rest2))      ; specialized lambda list
            (body (cdr rest2)))
       ;; Guard: gf-name must be a symbol (or (setf SYM) form), not a
       ;; comma struct from a quasiquoted (defmethod ,sym ...) inside
       ;; (eval ...).  Backquoted defmethod is a runtime form that
       ;; should hit our cl-eval.lisp eval-defmethod handler — DON'T
       ;; rewrite it at build time.  Return the form unchanged so the
       ;; surrounding quasiquote expansion preserves it for runtime eval.
       (unless (or (symbolp gf-name)
                   (and (consp gf-name) (eq (car gf-name) 'setf)))
         (return-from rewrite-reader-forms form))
       (when (null sll) (return-from rewrite-reader-forms nil))
       (when (not (listp sll)) (return-from rewrite-reader-forms nil))
       ;; Extract specializers (skip &optional, &rest, &key, &aux, &allow-other-keys
       ;; and everything after them — only positional / specialized args have specializers).
       (let* ((specs
               (let ((stop-at-keyword nil))
                 (let ((spec-result nil))
                   (dolist (p sll)
                     (cond
                       ((and (symbolp p)
                             (member p '(&optional &rest &key &aux &allow-other-keys)))
                        (setq stop-at-keyword t))
                       ((not stop-at-keyword)
                        (push
                         (cond
                           ;; (var class-name) or (var (eql val))
                           ((consp p)
                            (let ((spec (cadr p)))
                              (if (and (consp spec) (eq (car spec) 'eql))
                                  ;; eql specializer: preserve as (eql val)
                                  `(list 'eql ,(rewrite-reader-forms (cadr spec)))
                                  `',(cadr p))))
                           ;; plain var — specializer is t
                           (t ''t))
                         spec-result))))
                   (nreverse spec-result))))
              ;; Build parameter list.  Positional args use bare names (strip
              ;; specializer).  After &optional / &key / &aux, preserve the
              ;; full (var default supplied-p) form so supplied-p vars exist
              ;; in the body — defmethods like
              ;;   (defmethod foo ((x c) &key (a nil a-p)) (when a-p ...))
              ;; depend on A-P being bound to T/NIL by the caller's arg list.
              (params
               (let ((p-list nil)
                     (in-keyword nil))
                 (dolist (p sll)
                   (cond
                     ((and (symbolp p)
                           (member p '(&optional &rest &key &aux &allow-other-keys)))
                      (setq in-keyword t)
                      (push p p-list))
                     (in-keyword
                      ;; After &optional/&key/&aux: preserve full form so
                      ;; (var default supplied-p) keeps supplied-p binding.
                      (push p p-list))
                     (t
                      ;; Positional / specialized: strip specializer.
                      (push (if (consp p) (car p) p) p-list))))
                 (nreverse p-list)))
              (rewritten-body (mapcar #'rewrite-reader-forms body)))
         ;; Use lambda directly — can be inside init expressions
         `(%defmethod ',gf-name ',(if qualifier qualifier nil)
                      (list ,@specs)
                      (lambda ,params ,@rewritten-body)))))

    ;; (make-instance 'class-name &rest initargs)
    ;; → (let ((tmp (%make-instance 'class)))
    ;;     (%shared-init-default-spread (list tmp t k1 v1 k2 v2 ...))
    ;;     tmp)
    ;; %shared-init-default-spread (in ansi-bridge.lisp) does leftmost-
    ;; wins initarg application AND applies initforms for unset slots —
    ;; matching CLHS make-instance semantics.  The old expansion emitted
    ;; set-slot-value calls left-to-right (last-write-wins, wrong) and
    ;; never applied initforms at all.
    ((and (eq (car form) 'make-instance) (cdr form))
     (let* ((class-arg-raw (cadr form))
            (class-arg (rewrite-reader-forms class-arg-raw))
            (rest-args (cddr form)))
       (if (null rest-args)
           ;; No initargs: still want initforms applied.  Bind
           ;; *clos-applying-defaults* T so default-initargs apply (CLHS
           ;; 7.1.4) — distinguishing make-instance from bare shared-init.
           `(let ((%clos-make-instance-tmp (%make-instance ,class-arg))
                  (*clos-applying-defaults* t))
              (%shared-init-default-spread
                (list %clos-make-instance-tmp t))
              %clos-make-instance-tmp)
           ;; Has initargs: pass them through to the spread helper which
           ;; matches them against the runtime initarg-map and applies
           ;; initforms for any unset slots.  Values are recursively
           ;; rewritten so quoted/embedded forms still resolve correctly.
           (let ((rewritten-args (mapcar #'rewrite-reader-forms rest-args)))
             ;; CLHS 7.1.2: validate the initarg plist — odd-length plist →
             ;; program-error (make-instance.error.2), unknown initarg →
             ;; error (make-instance.error.3/.4).  The runtime make-instance
             ;; fn does this, but the compiled expansion bypasses it, so call
             ;; the designator-accepting validator here.  The initarg VALUE
             ;; forms may have side effects (order-of-evaluation tests), so
             ;; bind the plist ONCE into %clos-mi-initargs and reuse it for
             ;; both validation and the spread — never re-evaluate.
             ;; Bind the class designator ONCE too — class-arg may have
             ;; side effects ((prog1 'name (incf i)) in make-instance.order.3),
             ;; so evaluating it for both %make-instance and validation would
             ;; double-count.
             `(let* ((%clos-mi-class ,class-arg)
                     (%clos-make-instance-tmp (%make-instance %clos-mi-class))
                     (%clos-mi-initargs (list ,@rewritten-args))
                     (*clos-applying-defaults* t))
                (%clos-validate-initargs-d %clos-mi-class %clos-mi-initargs)
                (%shared-init-default-spread
                  (cons %clos-make-instance-tmp (cons t %clos-mi-initargs)))
                %clos-make-instance-tmp)))))

    ;; (slot-value obj slot) → (slot-value obj slot) — already defined at runtime
    ;; (slot-boundp obj slot) → (slot-boundp obj slot) — already defined
    ;; (slot-makunbound obj slot) → (slot-makunbound obj slot) — already defined

    ;; (with-slots (slot-bindings...) obj body...)
    ;; Keep the WITH-SLOTS head intact and let compiler.lisp's WITH-SLOTS
    ;; macro expand it via SYMBOL-MACROLET (so SETF/SETQ on a slot var
    ;; writes back to the slot).  The old rewriter here expanded to a plain
    ;; (let ((var (slot-value …))) …), making var a non-place local — reads
    ;; worked but (setf a 'p)/(setq a 'p) silently mutated the local instead
    ;; of the slot (with-slots.8/9/11/12/13 GOT the right setf RETURN but the
    ;; slot stayed unchanged).  Only rewrite the obj-form and body subforms.
    ((and (eq (car form) 'with-slots) (cddr form))
     `(with-slots ,(cadr form)
        ,(rewrite-reader-forms (caddr form))
        ,@(mapcar #'rewrite-reader-forms (cdddr form))))

    ;; (with-accessors (accessor-bindings...) obj body...)
    ;; Same rationale: defer to compiler.lisp's WITH-ACCESSORS symbol-macrolet
    ;; macro so SETF on an accessor var routes through (setf (acc obj) v).
    ((and (eq (car form) 'with-accessors) (cddr form))
     `(with-accessors ,(cadr form)
        ,(rewrite-reader-forms (caddr form))
        ,@(mapcar #'rewrite-reader-forms (cdddr form))))

    ;; (with-open-file (var filespec &rest opts) body...)
    ;; → (let ((var (open filespec opts...))) (unwind-protect (progn body) (when var (close var))))
    ;; Since MVM has no unwind-protect, we use let + close at end (no exception safety for now)
    ((and (eq (car form) 'with-open-file) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (var (car binding))
            (filespec (rewrite-reader-forms (cadr binding)))
            (opts (mapcar #'rewrite-reader-forms (cddr binding)))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(let ((,var (open ,filespec ,@opts)))
          (when ,var
            (let ((%wof-result (progn ,@body)))
              (close ,var)
              %wof-result)))))

    ;; (with-open-stream (var stream-form) body...)
    ;; → (let ((var stream-form)) (progn body... (close var)))
    ((and (eq (car form) 'with-open-stream) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (var (car binding))
            (stream-form (rewrite-reader-forms (cadr binding)))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(let ((,var ,stream-form))
          (let ((%wos-result (progn ,@body)))
            (close ,var)
            %wos-result))))

    ;; (with-hash-table-iterator (next ht) body...)
    ;; Expands to: collect ht pairs as alist, iterate
    ;; (next) returns (values more-p key val)
    ((and (eq (car form) 'with-hash-table-iterator) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (iter-name (car binding))
            (ht-form (rewrite-reader-forms (cadr binding)))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (ht-var '%whti-ht)
            (pairs-var '%whti-pairs))
       `(let* ((,ht-var ,ht-form)
               (,pairs-var (%ht-to-alist ,ht-var)))
          (flet ((,iter-name ()
                   (if (null ,pairs-var)
                       (values nil nil nil)
                       (let ((%whti-pair (car ,pairs-var)))
                         (setq ,pairs-var (cdr ,pairs-var))
                         (values t (car %whti-pair) (cdr %whti-pair))))))
            ,@body))))

    ;; (with-package-iterator (next pkg-or-pkgs &rest types) body...)
    ;; Per CLHS 11.1.5.1.1: bind ITER-NAME so that each call returns
    ;; (values more-p symbol access-type containing-package).  TYPES is
    ;; some non-empty subset of (:internal :external :inherited).  The
    ;; rewriter materialises the symbol list up-front and pops one
    ;; entry per (next) call.
    ((and (eq (car form) 'with-package-iterator) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (iter-name (car binding))
            (pkg-form (cadr binding))
            (types (cddr binding))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (pkgs-var (gensym "WPI-PKGS"))
            (entries-var (gensym "WPI-ENTRIES"))
            (pk-var (gensym "WPI-PK"))
            (e-var (gensym "WPI-E"))
            (use-var (gensym "WPI-USE"))
            (top-var (gensym "WPI-TOP")))
       `(let* ((,pkgs-var (let ((__p ,pkg-form))
                            (cond ((listp __p) (mapcar #'find-package __p))
                                  ((or (stringp __p) (symbolp __p)) (list (find-package __p)))
                                  (t (list __p)))))
               (,entries-var nil))
          (dolist (,pk-var ,pkgs-var)
            ,@(when (member :internal types :test #'eq)
                `((dolist (,e-var (%pkg-internal ,pk-var))
                    (setq ,entries-var (cons (list (cdr ,e-var) :internal ,pk-var) ,entries-var)))))
            ,@(when (member :external types :test #'eq)
                `((dolist (,e-var (%pkg-external ,pk-var))
                    (setq ,entries-var (cons (list (cdr ,e-var) :external ,pk-var) ,entries-var)))))
            ,@(when (member :inherited types :test #'eq)
                `((dolist (,use-var (%pkg-use-list ,pk-var))
                    (dolist (,e-var (%pkg-external ,use-var))
                      (setq ,entries-var (cons (list (cdr ,e-var) :inherited ,pk-var) ,entries-var)))))))
          (flet ((,iter-name ()
                   (cond ((null ,entries-var) (values nil nil nil nil))
                         (t (let ((,top-var (car ,entries-var)))
                              (setq ,entries-var (cdr ,entries-var))
                              (values t (car ,top-var) (cadr ,top-var) (caddr ,top-var)))))))
            ,@body))))

    ;; (catch-type-error form) → (handler-case form
    ;;                              (type-error (c) (declare (ignore c)) 'type-error)
    ;;                              (error (c) c))
    ;; ansi-aux.lsp defines this as a macro; build emits SBCL READ output
    ;; (S-expression printout) without macroexpansion, so test bodies see
    ;; the bare CATCH-TYPE-ERROR symbol as a function call.  Rewriting it
    ;; here makes the modus side compile the real handler form.
    ((and (eq (car form) 'catch-type-error) (cdr form))
     (let ((inner (rewrite-reader-forms (cadr form))))
       `(handler-case ,inner
          (type-error (c) (declare (ignore c)) 'type-error)
          (error (c) c))))

    ;; (handle-non-abort-restart . body) → (catch 'handled
    ;;                                       (handler-bind ((error #'has-non-abort-restart))
    ;;                                         ,@body))
    ;; ansi-aux.lsp definition; same SBCL-defmacro-not-expanded issue.
    ;; Provided as a stub that just runs the body for the common case
    ;; where no restart-establishing error fires.
    ((and (eq (car form) 'handle-non-abort-restart) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (cdr form))))
       `(catch 'handled
          (handler-case (progn ,@body)
            (error (c) (declare (ignore c)) 'fail)))))

    ;; (expand-in-current-env inner) → inner — ansi-aux's macroexpand
    ;; wrapper.  For ANSI tests it mostly receives literals and simple
    ;; forms; modus has no macro-environment introspection, so just
    ;; pass the inner form through.  Real macros inside still expand
    ;; at modus compile time via the *macro-table*.
    ((and (eq (car form) 'expand-in-current-env) (cdr form))
     (rewrite-reader-forms (cadr form)))

    (t (rewrite-reader-forms-list form))))

(defun rewrite-reader-forms-list (list)
  "Walk a possibly-dotted list, applying rewrite-reader-forms to each element."
  (cond
    ((null list) nil)
    ((atom list) (rewrite-reader-forms list))
    (t (cons (rewrite-reader-forms (car list))
             (rewrite-reader-forms-list (cdr list))))))

;; True if FORM (or any sub-tree) contains a 0-dim or N-dim array literal.
;; Walk avoids transforming forms that have no array literals at all
;; (so the simple '~S quoting still applies to plain values like (1 2 3)).
(defun %md-contains-array-literal-p (form)
  (cond
    ((and (arrayp form) (not (stringp form))
          (or (= (array-rank form) 0) (> (array-rank form) 1))) t)
    ((consp form)
     (or (%md-contains-array-literal-p (car form))
         (%md-contains-array-literal-p (cdr form))))
    (t nil)))

;; Convert a literal 0-dim or N-dim SBCL array into a Modus
;; runtime-construction form building a NATIVE MDA (subtag #x34) via
;; %alloc-mda, so %mda-p / array-rank / fill-pointer / typep see it as a
;; real multi-dim array (legacy (cons 9867654 ...) wrappers don't satisfy
;; %mda-p, which broke array-t / fill-pointer.error reader-literal tests).
;; Recursively rewrites elements so nested array literals also become
;; native MDAs.  1-D vectors and strings are returned unchanged — their
;; printed form round-trips through the Modus reader as a real array/string.
(defun %mdrewrite-array-literals (form)
  (cond
    ((and (arrayp form) (not (stringp form))
          (or (= (array-rank form) 0) (> (array-rank form) 1)))
     (let* ((dims  (array-dimensions form))
            (rank  (array-rank form))
            (total (array-total-size form))
            (sz    (if (= total 0) 1 total))
            (asets (loop for i below total
                         collect
                           (let ((v (%mdrewrite-array-literals
                                     (row-major-aref form i))))
                             `(aset %md-tmp ,i ',v)))))
       `(%alloc-mda ,rank ',dims nil nil 0 t
                    (let ((%md-tmp (make-array ,sz)))
                      ,@asets
                      %md-tmp))))
    ((consp form)
     (cons (%mdrewrite-array-literals (car form))
           (%mdrewrite-array-literals (cdr form))))
    (t form)))

;; Load real ANSI test files (if available)
(defvar *ansi-aux-sources* "")       ; auxiliary/helper files (loaded before test files)
(defvar *real-ansi-sources* "")
(defvar *hoisted-gf-defuns* (make-hash-table :test 'equal)
  "Printed forms of GF dispatch defuns already hoisted to top level
   from inside deftest thunks — dedup set (one copy per defun).")
(defvar *ansi-test-counter* 10000)
(defvar *ansi-file-names* nil)
;; Per-file test ID ranges, list of (name first-id last-id).
;; Used to skip files whose range doesn't overlap the active shard range,
;; so init-forms in unrelated files don't run (many crash the parent).
(defvar *ansi-file-ranges* nil)

;;; --- WS3 Phase 1: differential gate (eval vs mvm-eval) ---
;; MODUS_MVM_EVAL_DIFF=1 → ALSO emit, per test, an (id . actual-form) capture into
;; a separate *e2diff-sources* block and run the tree-walker-vs-mvm-eval differential
;; gate (run-real-e2diff) INSTEAD of the normal run-real-ansi-tests.  When the
;; flag is unset the whole block below is dead/empty and the produced binary is
;; byte-identical to a normal build (verified flag-off).
(defvar *mvm-eval-diff-mode*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_MVM_EVAL_DIFF") #-sbcl nil))
    (and v (plusp (length v)) (not (string= v "0")))))
;; WS3 Phase 3 (tree-walker retired as a production evaluator): production
;; EVAL/LOAD go straight to mvm-eval unconditionally (cl-eval.lisp EVAL = (mvm-eval
;; form)); there is no longer a *use-mvm-eval* flag or a tree-walker rollback path.
;; The diagnostic probe suite (run-all-tests) runs under mvm-eval like everything
;; else — measured to complete in ~1s (the old ~50x-slower premise was closed by
;; the 194bbfb/8953c39/c4d9403/3281efe perf fixes).
;; WS3 flip gate (retained for corpus/probe de-confounding): MODUS_FLIP_SKIP_PROBES
;; drops run-all-tests from the driver so the REAL ANSI corpus can be gated
;; without the diagnostic probes' P: lines being counted.
(defvar *flip-skip-probes*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_FLIP_SKIP_PROBES") #-sbcl nil))
    (and v (plusp (length v)) (not (string= v "0")))))
;; Accumulated emitted source for the e2diff chunk fns + run-e2diff-FILE fns.
(defvar *e2diff-sources* "")
;; Per-file accumulator of (id . actual-form) captured this file; reset per file.
(defvar *e2diff-forms* nil)

(defun %count-source-deftests (path)
  "Count deftest-family forms in the RAW source text at PATH by scanning
   for an opening paren immediately followed by a deftest-family head.
   Text-based (no reader) so it still works when the file has a form the
   host reader can't parse — that's exactly the case the BUILD-SKIP audit
   needs to detect.  Conservative: matches `(deftest`, `(def-print-test`,
   etc. with the paren and head adjacent (ANSI test files always write
   them that way).  Overcounts slightly if those tokens appear inside a
   string or comment, which is acceptable for an advisory report."
  (handler-case
      (let ((text (with-output-to-string (o)
                    (with-open-file (s path :direction :input)
                      (loop for line = (read-line s nil :eof)
                            until (eq line :eof)
                            do (write-line line o)))))
            (heads '("(deftest" "(def-print-test" "(def-pprint-test"
                     "(def-format-test" "(def-ppblock-test"
                     "(def-adjust-array-test" "(def-adjust-array-fp-test"))
            (n 0))
        (let ((up (string-upcase text)))
          (dolist (h heads n)
            (let ((hu (string-upcase h)) (start 0))
              (loop (let ((pos (search hu up :start2 start)))
                      (when (null pos) (return))
                      ;; require a delimiter after the head so (deftest-foo
                      ;; doesn't match (deftest
                      (let ((after (+ pos (length hu))))
                        (when (or (>= after (length up))
                                  (member (char up after)
                                          '(#\Space #\Tab #\Newline #\Return #\()))
                          (incf n)))
                      (setf start (+ pos (length hu)))))))))
    (error () 0)))

(defun %form-has-clos-reg-p (form)
  "True if FORM's tree contains a CLOS / package registration call that the
   defclass / defgeneric / defmethod / defpackage rewriters emit.  Used to
   decide whether a top-level (let …)/(flet …)-wrapped form must be hoisted
   into run-init-FILE so its lexical-capturing initform thunks actually run."
  (cond
    ((consp form)
     (if (member (car form)
                 '(%defclass %register-clos-slot-info %register-clos-direct-slots
                   %register-clos-class-slots %register-clos-default-initargs
                   %defgeneric %defmethod %define-condition %defpackage-impl
                   %register-gf-fn))
         t
         (or (%form-has-clos-reg-p (car form))
             (%form-has-clos-reg-p (cdr form)))))
    (t nil)))

(defun %form-has-nested-defun-p (form)
  "True if FORM's tree contains a (defun …) — used to keep let/flet-wrapped
   defclasses with :reader/:writer/:accessor (which expand to nested defuns)
   on the top-level write path rather than hoisting into a runtime let."
  (cond
    ((consp form)
     (if (eq (car form) 'defun)
         t
         (or (%form-has-nested-defun-p (car form))
             (%form-has-nested-defun-p (cdr form)))))
    (t nil)))

(defun load-ansi-chapter (dir files)
  "Transform ANSI test files from DIR into MVM-compatible source.
   Skips files that cause read errors."
  (dolist (file files)
    (handler-case
      (let ((path (concatenate 'string dir file)))
        (when (probe-file path)
          (format t "  Transforming: ~A~%" file)
          (let ((forms nil)
                (read-truncated nil)   ; T if a read error stopped us before EOF
                (read-err nil))        ; the read error condition, for the report
            (with-open-file (s path :direction :input)
              ;; *read-eval* T so `#.(make-array …)` literals (adjust-array.lsp
              ;; form 54+, print-*.lsp, etc.) read cleanly.  The build env sets
              ;; *read-eval* NIL globally for safety, which made the WHOLE
              ;; adjust-array.lsp abort with "can't read #. while *READ-EVAL*
              ;; is NIL" — losing all ~78 ADJUST-ARRAY tests.  These #. forms
              ;; only construct arrays/pathnames at read time; safe to eval.
              (let ((*package* (find-package :cl-user))
                    (*read-eval* t))
                ;; Read per-form, catching a read error (END-OF-FILE / reader
                ;; error) so a single un-readable form doesn't drop the WHOLE
                ;; file.  adjust-array.lsp wraps its string/base-char variants
                ;; in one giant `(loop ... for forms = `( …many deftests… )
                ;; do (eval …))' form that the host reader chokes on; the 53
                ;; plain deftests before it were being lost.  On a read error
                ;; we keep the forms read so far and stop (the remaining text
                ;; belongs to the unreadable form).
                ;; Per-form read with RECOVERY.  Two mechanisms:
                ;;  (1) Eval defun/defmacro forms as they are read, so a later
                ;;      `#.(symbol-function 'foo)` / `#.(find-class …)` form
                ;;      can reference a helper defined earlier in the SAME file
                ;;      (handler-bind.lsp form 9 referenced a defun from form 8;
                ;;      the host reader hadn't eval'd it yet → undefined-fn →
                ;;      whole tail of the file lost).  Guarded — a defun whose
                ;;      body the host can't compile is skipped, not fatal.
                ;;  (2) On a read error, record it but try to RESYNC to the
                ;;      next top-level form (a line beginning with "(") and keep
                ;;      reading, instead of dropping every remaining test.  This
                ;;      recovers files whose ONE bad form (undefined package in
                ;;      a `#.`, etc.) sits in the middle (make-load-form-saving-
                ;;      slots.lsp, handler-bind.lsp).  A file whose bad form runs
                ;;      to EOF (adjust-array.lsp's giant loop) simply stops.
                (labels ((eval-defs (form)
                           (when (and (consp form)
                                      ;; defpackage/make-package too: a later
                                      ;; form may reference a package-qualified
                                      ;; symbol (mlfss line 132 uses
                                      ;; cl-test-mlfss-package:a as a slot name);
                                      ;; the host reader needs the package to
                                      ;; exist when it reads that token.
                                      (member (car form)
                                              '(defun defmacro defpackage make-package)))
                             (handler-case (eval form) (error () nil))))
                         (resync ()
                           ;; Discard the rest of the current (unreadable) line
                           ;; then skip forward until a line starts with "(".
                           (read-line s nil :eof)
                           (loop
                             (let ((c (peek-char nil s nil :eof)))
                               (cond ((eq c :eof) (return :eof))
                                     ((char= c #\() (return :ok))
                                     (t (read-line s nil :eof)))))))
                  (loop
                    (let ((form (handler-case (read s nil :eof)
                                  (error (e)
                                    (unless read-truncated
                                      (setf read-truncated t read-err e))
                                    (if (eq (resync) :eof) :eof :resynced)))))
                      (cond ((eq form :eof) (return))
                            ((eq form :resynced) nil) ; skipped a bad form; continue
                            (t (eval-defs form)
                               (push form forms))))))))
            ;; BUILD-SKIP audit: count deftest-family forms in the RAW source
            ;; text and compare to what we actually read.  A fully-skipped
            ;; file (0 forms) or a read-truncated file silently drops every
            ;; test after the truncation point — these used to vanish with no
            ;; trace (dgmc-aux: zero-test chunk; adjust-array.lsp: read error).
            ;; The report makes both visible so a recoverable skip can be
            ;; fixed (per-form read recovery) instead of silently lost.
            (let* ((raw-deftests (%count-source-deftests path))
                   (read-deftests
                     (count-if (lambda (f)
                                 (and (consp f)
                                      (member (car f)
                                              '(deftest def-print-test def-pprint-test
                                                def-format-test def-ppblock-test
                                                def-adjust-array-test
                                                def-adjust-array-fp-test))))
                               forms)))
              (declare (ignorable read-deftests))
              (cond
                ((null forms)
                 (format t "  BUILD-SKIP ~A: 0 forms read (file fully skipped, ~D deftest(s) in source lost)~%"
                         file raw-deftests))
                (read-truncated
                 ;; A read error stopped us mid-file: every form after the
                 ;; offending one is silently dropped.  This is the high-signal
                 ;; case (adjust-array.lsp's giant loop, package-undefined #.
                 ;; reads in loop7/mlfss).  BUILD-WARN on the small
                 ;; macro-expansion gaps (deftest count off by 1-2) is noise,
                 ;; so only report genuine truncations.
                 (format t "  BUILD-SKIP ~A: read TRUNCATED — ~D form(s) read, ~D/~D raw deftest(s) recovered; lost the rest [~A]~%"
                         file (length forms) read-deftests raw-deftests
                         (handler-case (format nil "~A" read-err) (error () "?"))))))
            (push (pathname-name file) *ansi-file-names*)
            ;; WS3 P1: reset the per-file differential capture accumulator.
            (setf *e2diff-forms* nil)
            ;; Snapshot the test-id counter on entry so we can record the
            ;; file's [first .. last] test-id range after processing.
            (let ((file-first-id (1+ *ansi-test-counter*)))
              (push (list (pathname-name file) file-first-id nil) *ansi-file-ranges*))
            ;; Per-form mapcar that catches errors: a defmacro with
            ;; SBCL backquote-comma objects (SB-INT:UNQUOTE) is not
            ;; consp, so tree-walking rewriters can hit (car comma-struct)
            ;; and type-error.  Without the per-form catch the WHOLE file
            ;; is dropped and all its tests silently disappear.
            (flet ((mapcar-safe (fn lst)
                     (mapcar (lambda (f) (handler-case (funcall fn f)
                                           (error () f)))
                             lst)))
              (setf forms (mapcar-safe #'rewrite-package-iteration (nreverse forms)))
              ;; rewrite-make-array-with-checks RETIRED (Phase 4): wrapper
              ;; just renamed make-array-with-checks → make-array with kwargs
              ;; filtered down to ones the rewriter knew about.  Native MDA
              ;; runtime handles the same kwargs.  Form pass-through tests
              ;; show no behavioral diff (P=13067 both ways).
              ;;(setf forms (mapcar-safe #'rewrite-make-array-with-checks forms))
              ;; rewrite-make-array-dims RETIRED (Phase 4 multi-dim arrays):
              ;; native MDA subtag #x34 + compile-make-array dispatcher handles
              ;; (make-array '(N M ...) ...) directly.  Kept here in source for
              ;; comparison/rollback only.
              ;;(setf forms (mapcar-safe #'rewrite-make-array-dims forms))
              (setf forms (mapcar-safe #'rewrite-eval-quote forms))
              ;; rewrite-make-array-initcontents RETIRED (Phase 4): runtime
              ;; make-array in ansi-bridge.lisp now handles :initial-contents,
              ;; :initial-element, :fill-pointer, :adjustable, :displaced-to,
              ;; :element-type via native MDA storage.
              ;;(setf forms (mapcar-safe #'rewrite-make-array-initcontents forms))
              (setf forms (mapcar-safe #'rewrite-earmuff-specials forms))
              (setf forms (mapcar-safe #'rewrite-reader-forms forms))
              ;; Rewrite multi-arg apply: (apply fn a1 a2 ... list) → 2-arg form
              (setf forms (mapcar-safe #'rewrite-multi-arg-apply forms)))
            (when (string= file "integer-length.lsp")
              (labels ((rw (f)
                         (cond ((atom f) f)
                               ((and (eq (car f) 'ash) (cddr f))
                                (cons 'bignum-ash (mapcar #'rw (cdr f))))
                               ((and (eq (car f) '1-) (cdr f))
                                (cons 'bignum-1- (mapcar #'rw (cdr f))))
                               ((and (eq (car f) '-) (cdr f))
                                (if (cddr f)
                                    (cons 'bignum-sub (mapcar #'rw (cdr f)))
                                    (cons 'bignum-negate (mapcar #'rw (cdr f)))))
                               ((and (eq (car f) 'eql) (cddr f))
                                ;; eql needs to handle bignum=fixnum comparison
                                (cons 'bignum-eql (mapcar #'rw (cdr f))))
                               (t (mapcar-dotted #'rw f)))))
                (setf forms (mapcar-dotted #'rw forms))))
            ;; Rewrite arithmetic in real.lsp for ratio support:
            ;; / → exact-divide, - → generic-subtract, 1+ → generic-1+
            ;; Also limit LOOP REPEAT 200 → 60 (63-bit fixnum overflow)
            (when (string= file "real.lsp")
              (labels ((rw (f)
                         (cond ((atom f) f)
                               ;; (/ a b) → (exact-divide a b)
                               ((and (eq (car f) '/) (cddr f) (null (cdddr f)))
                                (cons 'exact-divide (mapcar #'rw (cdr f))))
                               ;; (- a) → (generic-negate a), (- a b) → (generic-subtract a b)
                               ((and (eq (car f) '-) (cdr f))
                                (if (cddr f)
                                    (list 'generic-subtract (rw (cadr f)) (rw (caddr f)))
                                    (list 'generic-negate (rw (cadr f)))))
                               ;; (1+ a) → (generic-1+ a)
                               ((and (eq (car f) '1+) (cdr f) (null (cddr f)))
                                (list 'generic-1+ (rw (cadr f))))
                               (t
                                ;; Patch REPEAT 200 → REPEAT 55 (safe for 63-bit fixnum with ratio cross-multiply)
                                (let ((result (mapcar-dotted #'rw f)))
                                  (when (and (eq (car result) 'loop))
                                    (let ((tail result))
                                      (loop (when (null tail) (return))
                                        (when (and (eq (car tail) 'repeat)
                                                   (cdr tail) (eql (cadr tail) 200))
                                          (setf (cadr tail) 55))
                                        (setq tail (cdr tail)))))
                                  result)))))
                (setf forms (mapcar #'rw forms))))
            ;; Evaluate defun/defmacro forms at SBCL side so that macros
            ;; defined within the file can be used during macroexpansion below.
            ;; This is needed for files like adjust-array.lsp that define
            ;; helper functions/macros used only at SBCL compile time.
            (dolist (form forms)
              (when (and (consp form)
                         (member (car form) '(defun defmacro)))
                (handler-case (eval form) (error () nil))))
            ;; Macroexpand def-print-test, def-pprint-test, def-format-test,
            ;; def-adjust-array-test, etc. into deftest forms before processing
            (setf forms
                  (mapcan (lambda (form)
                            (if (and (consp form)
                                     (member (car form) '(def-print-test def-pprint-test
                                                          def-format-test def-ppblock-test
                                                          def-adjust-array-test
                                                          def-adjust-array-fp-test)))
                                (handler-case
                                  (let ((expanded (macroexpand-1 form)))
                                    ;; def-format-test expands to (progn deftest deftest)
                                    (if (and (consp expanded) (eq (car expanded) 'progn))
                                        (cdr expanded)
                                        (list expanded)))
                                  (error (e)
                                    (format t "    SKIP-MACRO ~A: ~A~%" (car form) e)
                                    nil))
                                (list form)))
                          forms))
            ;; Re-run select rewriters after macroexpansion.  Use the
            ;; per-form catching mapcar-safe so a single comma-bearing
            ;; macro body doesn't drop the whole file.
            (flet ((mapcar-safe (fn lst)
                     (mapcar (lambda (f) (handler-case (funcall fn f)
                                           (error () f)))
                             lst)))
              (setf forms (mapcar-safe #'rewrite-reader-forms forms))
              (setf forms (mapcar-safe #'rewrite-multi-arg-apply forms))
              ;; &aux retired: preprocess-params handles it natively.
              (setf forms (mapcar-safe #'rewrite-earmuff-specials forms))
              ;; rewrite-make-array-* RETIRED (post-macroexpansion phase) —
              ;; native MDA + runtime make-array + MDA-aware rt-equal/printer
              ;; handle the unmodified test forms.
              )
            (let ((out (make-string-output-stream)) (test-forms nil) (init-forms nil))
              (format out "~%;; === ~A ===~%" file)
(dolist (form forms)
                (cond
                  ((and (consp form) (eq (car form) 'deftest))
                   (let* ((rest-after-name (cddr form))
                          ;; Skip :notes (...) if present
                          (rest-after-notes
                           (if (eq (car rest-after-name) :notes)
                               (cddr rest-after-name)
                               rest-after-name))
                          (name (cadr form))
                          (test-form (car rest-after-notes))
                          (expected (cdr rest-after-notes)))
                     (setf *ansi-test-counter* (1+ *ansi-test-counter*))
                     (let ((test-id *ansi-test-counter*))
                       (format t "      ~D = ~A~%" test-id name)
                       ;; Hoist GF dispatch defuns nested inside the test
                       ;; form (from rewritten inline defgenerics) to top
                       ;; level.  Defuns nested in a deftest thunk are NOT
                       ;; visible to compile-call's name resolution, so a
                       ;; named call like (DEFGENERIC.FUN.1 'D 'E 'F) in
                       ;; the same thunk compiled into garbage that
                       ;; returned its last argument (defgeneric.1/8/31/32
                       ;; GOT (... F) / B / X).  A top-level copy gives the
                       ;; name a real compile-time address; the nested
                       ;; original is harmless.
                       (labels ((hoist-gf-defuns (f)
                                  (when (consp f)
                                    (if (and (eq (car f) 'defun)
                                             (consp (cdr f))
                                             (consp (cddr f))
                                             (consp (cdddr f))
                                             (consp (car (cdddr f)))
                                             (eq (car (car (cdddr f)))
                                                 '%gf-dispatch))
                                        (let ((s (handler-case
                                                     (format nil "~S" f)
                                                   (error () nil))))
                                          ;; Dedup — the same GF dispatch
                                          ;; defun appears in many tests;
                                          ;; one top-level copy suffices.
                                          (when (and s
                                                     (not (gethash s *hoisted-gf-defuns*)))
                                            (setf (gethash s *hoisted-gf-defuns*) t)
                                            (write-string s out)
                                            (terpri out)))
                                        (progn
                                          (hoist-gf-defuns (car f))
                                          (hoist-gf-defuns (cdr f)))))))
                         (handler-case (hoist-gf-defuns test-form)
                           (error () nil)))
                       ;; WS3 Phase 1 differential capture: stash (id . actual)
                       ;; where ACTUAL = the cooked test-form (the form deftest
                       ;; evaluates).  Rendered later as (%e2diff ID (quote ...)).
                       ;; Same unreadable-object filters as test-str — a form
                       ;; that prints #<FUNCTION ...> can't round-trip through
                       ;; the in-image reader so it's not capturable.
                       (when *mvm-eval-diff-mode*
                         (let ((e2form-str
                                (handler-case
                                  (let ((cooked
                                         (if (%md-contains-array-literal-p test-form)
                                             (%mdrewrite-array-literals test-form)
                                             test-form)))
                                    (format nil "~S" cooked))
                                  (error () nil))))
                           (when (and e2form-str
                                      (not (search "#<FUNCTION" e2form-str))
                                      (not (search "#<CLASS" e2form-str))
                                      (not (search "#<BUILT-IN-CLASS" e2form-str))
                                      (not (search "#<PACKAGE" e2form-str))
                                      (not (search "#<SB-" e2form-str))
                                      (not (search "#<STANDARD" e2form-str))
                                      (not (search "#<STRUCTURE" e2form-str))
                                      (not (search "#<CLOSURE" e2form-str))
                                      (not (search "&ENVIRONMENT" e2form-str))
                                      (not (search "STRUCT-TEST-" e2form-str)))
                             (push (cons test-id e2form-str) *e2diff-forms*))))
                       (let ((test-str (handler-case
                                         (cond
                                           ((= (length expected) 1)
                                            (let* ((exp-raw (car expected))
                                                   (form-cooked
                                                    (if (%md-contains-array-literal-p test-form)
                                                        (%mdrewrite-array-literals test-form)
                                                        test-form)))
                                              (if (%md-contains-array-literal-p exp-raw)
                                                  (format nil "(run-test ~D (lambda () ~S) ~S)"
                                                          test-id form-cooked
                                                          (%mdrewrite-array-literals exp-raw))
                                                  (format nil "(run-test ~D (lambda () ~S) '~S)"
                                                          test-id form-cooked exp-raw))))
                                           ((> (length expected) 0)
                                            (format nil "(run-test-mv ~D (lambda () (multiple-value-list ~S)) '~S)"
                                                    test-id test-form expected))
                                           ;; (deftest NAME FORM) with no explicit
                                           ;; expected — test expects zero values.
                                           ;; Render as run-test-mv with '() expected.
                                           (t
                                            (format nil "(run-test-mv ~D (lambda () (multiple-value-list ~S)) 'NIL)"
                                                    test-id test-form)))
                                         (error () nil))))
                         ;; For real.lsp: fix / and - inside backquote commas
                         ;; (tree rewriter can't reach inside SBCL comma objects)
                         (when (and test-str (string= file "real.lsp"))
                           (labels ((str-replace-all (old new str)
                                      (let ((pos (search old str)))
                                        (if pos
                                            (str-replace-all old new
                                              (concatenate 'string
                                                (subseq str 0 pos) new
                                                (subseq str (+ pos (length old)))))
                                            str))))
                             ;; Replace (/ → (EXACT-DIVIDE inside comma contexts
                             ;; Order matters: / first, then - (so ,(- (/ x y)) works)
                             (setf test-str (str-replace-all ",(/ " ",(EXACT-DIVIDE " test-str))
                             (setf test-str (str-replace-all ",(- (" ",(GENERIC-NEGATE (" test-str))
                             ;; Also fix (/ inside ,(- ...): after GENERIC-NEGATE, inner / remains
                             (setf test-str (str-replace-all ",(GENERIC-NEGATE (/ " ",(GENERIC-NEGATE (EXACT-DIVIDE " test-str))
                             (when (member name '(real.3 real.4) :test #'string=)
                               (format *error-output* "~%POST-REPLACE ~A:~%~A~%~%" name test-str))))
                         ;; Filter: unreadable-object printouts can't round-trip.
                         ;; Match SBCL's `#<CLASS-NAME ...>` pattern specifically
                         ;; — `(search "#<" ...)` was too broad, rejecting any test
                         ;; whose source contains the 2-char string "#<" (e.g.
                         ;; print.array.2.28 checks whether output starts with "#<").
                         (when (and test-str
                                    (not (search "#<FUNCTION" test-str))
                                    (not (search "#<CLASS" test-str))
                                    (not (search "#<BUILT-IN-CLASS" test-str))
                                    (not (search "#<PACKAGE" test-str))
                                    (not (search "#<SB-" test-str))
                                    (not (search "#<STANDARD" test-str))
                                    (not (search "#<STRUCTURE" test-str))
                                    (not (search "#<CLOSURE" test-str))
                                    (not (search "&ENVIRONMENT" test-str))
                                    (not (search "STRUCT-TEST-" test-str)))
                           (push test-str test-forms))))))
                  ((and (consp form) (member (car form)
                          '(defharmless def-fold-test def-macro-test
                            declaim))) nil)
                  (t
                   ;; For progn forms (from rewritten defclass/defmethod/defgeneric):
                   ;; - defun sub-forms → top-level (compiled as global functions)
                   ;; - non-defun sub-forms (like %defclass, %defmethod calls) → init-forms
                   ;;   (run inside run-ansi-* since TOPLEVEL thunks never execute)
                   ;; Handles nested progn forms recursively.
                   ;; For non-progn forms: write to top-level as before.
                   ;; For (defvar|defparameter NAME VALUE ...) queue an
                   ;; equivalent (setq NAME VALUE) into init-forms: at runtime
                   ;; defvar init-thunks never run, so NAME would otherwise
                   ;; stay NIL and any test referencing it fails silently.
                   (flet ((queue-defvar-setq (f)
                            (when (and (consp f) (member (car f) '(defvar defparameter))
                                       (consp (cdr f)) (consp (cddr f))
                                       (symbolp (cadr f)))
                              (let ((setq-s (handler-case
                                              (format nil "(setq ~S ~S)" (cadr f) (caddr f))
                                              (error () nil))))
                                (when (and setq-s
                                           (not (search "#<" setq-s))
                                           (not (search "&ENVIRONMENT" setq-s))
                                           (not (search "STRUCT-TEST-" setq-s)))
                                  (push setq-s init-forms))))))
                     (labels ((emit-sub (sub)
                                (when (consp sub)
                                  ;; Recursively flatten nested progns AND eval-when
                                  ;; bodies — `(eval-when (...) body)' must not be
                                  ;; written opaquely, since modus' runtime doesn't
                                  ;; auto-run eval-when-load-toplevel init thunks at
                                  ;; top level.  Pull body forms into init-forms so
                                  ;; run-init-X executes them.
                                  (if (or (eq (car sub) 'progn) (eq (car sub) 'eval-when))
                                      (let ((body (if (eq (car sub) 'eval-when)
                                                      (cddr sub) (cdr sub))))
                                        (dolist (inner body) (emit-sub inner)))
                                      (let ((sub-s (handler-case (format nil "~S" sub)
                                                     (error () nil))))
                                        (when (and sub-s
                                                   (not (search "#<" sub-s))
                                                   (not (search "&ENVIRONMENT" sub-s))
                                                   (not (search "STRUCT-TEST-" sub-s)))
                                          (cond
                                            ((member (car sub) '(defun defstruct))
                                             (write-string sub-s out) (terpri out))
                                            ((member (car sub) '(defvar defparameter))
                                             (write-string sub-s out) (terpri out)
                                             (queue-defvar-setq sub))
                                            (t (push sub-s init-forms)))))))))
                       (if (and (consp form)
                                (or (eq (car form) 'progn) (eq (car form) 'eval-when)))
                           (let ((body (if (eq (car form) 'eval-when)
                                           (cddr form) (cdr form))))
                             (dolist (sub body) (emit-sub sub)))
                         (if (and (consp form)
                                  ;; A lexical-binding wrapper around CLOS
                                  ;; registrations, e.g.
                                  ;;   (let ((x 7)) (defclass c () ((s :initform x))))
                                  ;;   (flet ((%f () 'x)) (defclass c () ((s :initform (%f)))))
                                  ;; The defclass rewriter expands the inner
                                  ;; defclass to (progn (%defclass ..) (%register-clos-slot-info ..) ..)
                                  ;; whose initform thunks capture the wrapper's
                                  ;; lexical vars.  Written as a TOP-LEVEL form it
                                  ;; would never run (bare metal: toplevel thunks
                                  ;; don't auto-execute), so the class went
                                  ;; unregistered (defclass-01 class-11/12).  Push
                                  ;; the WHOLE wrapper into init-forms so run-init-FILE
                                  ;; executes it and the captures resolve at runtime.
                                  (member (car form)
                                          '(let let* flet labels locally
                                            symbol-macrolet macrolet))
                                  (%form-has-clos-reg-p form)
                                  ;; A reader/writer/accessor expands to a
                                  ;; nested (defun …); hoisting it into a
                                  ;; runtime let would NOT define a global fn.
                                  ;; Skip such forms (none of the targeted
                                  ;; defclass-01 let/flet cases have readers).
                                  (not (%form-has-nested-defun-p (cdr form))))
                             (let ((s (handler-case (format nil "~S" form)
                                        (error () nil))))
                               (when (and s
                                          (not (search "#<" s))
                                          (not (search "&ENVIRONMENT" s))
                                          (not (search "STRUCT-TEST-" s)))
                                 (push s init-forms)))
                           (let ((s (handler-case (format nil "~S" form)
                                      (error () nil))))
                             (when (and s
                                        (not (search "#<" s))
                                        (not (search "&ENVIRONMENT" s))
                                        (not (search "STRUCT-TEST-" s)))
                               ;; ALSO emit root-level (%defpackage-impl ...)
                               ;; calls into init-forms — they were previously
                               ;; only being written as TOPLEVEL-N thunks, and
                               ;; those thunks never run on bare metal (defvar
                               ;; init thunks aren't run; same applies here).
                               ;; We keep the top-level write so any compile-time
                               ;; side effects stay in place.
                               ;;
                               ;; %defmethod: the defmethod rewriter emits a BARE
                               ;; (%defmethod 'gf ...) form (defclass wraps its
                               ;; registrations in progn, so those flow through
                               ;; emit-sub into init-forms — defmethod didn't).
                               ;; Bare top-level %defmethod therefore NEVER ran:
                               ;; user methods on CHANGE-CLASS /
                               ;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS etc. were
                               ;; silently unregistered, so :before/:after/
                               ;; primary methods in change-class.lsp and
                               ;; u-i-f-d-c.lsp never fired.  Route them into
                               ;; init-forms so run-init-FILE registers them.
                               (write-string s out)
                               (terpri out)
                               (queue-defvar-setq form)
                               (when (and (consp form)
                                          (member (car form)
                                                  '(%defpackage-impl %defmethod
                                                    ;; Package-mutation SETUP forms
                                                    ;; (make-package/intern/export/
                                                    ;; in-package …) written at top
                                                    ;; level otherwise run only as
                                                    ;; bare-metal toplevel thunks,
                                                    ;; which never execute — so the
                                                    ;; package is never created or
                                                    ;; populated and `loop for x being
                                                    ;; the symbols of PKG` (loop7)
                                                    ;; iterates an empty/absent pkg.
                                                    ;; Route them into run-init-FILE.
                                                    make-package in-package
                                                    delete-package
                                                    safely-delete-package
                                                    rename-package use-package
                                                    unuse-package export unexport
                                                    import unintern intern shadow
                                                    shadowing-import)))
                                 (push s init-forms)))))))))))
              ;; Emit run-init-X — a separate function holding ONLY the init
              ;; forms (defclass / defmethod / setq for defvar's value, etc.).
              ;; run-real-ansi-tests now calls all run-init-* in the PARENT
              ;; before any fork-file, so cross-file class references like
              ;; reinitialize-instance.lsp's `(make-instance 'class-01)` —
              ;; where class-01 is defined in defclass-01.lsp — see a
              ;; populated *clos-classes* in their fork.
              (let ((init-list (nreverse init-forms)))
                (format out "(defun run-init-~A ()~%" (pathname-name file))
                (if (null init-list)
                    (format out "  nil~%")
                    (dolist (s init-list)
                      (format out "  (handler-case ~A (t (c) nil))~%" s)))
                (format out ")~%")
                ;; Test forms — wrap EACH fork-test call in its own handler-case
                ;; so a crash during parent-side arg-evaluation (vector literal,
                ;; closure creation, etc.) of test N doesn't kill test N+1.
                ;; On catch, call (%test-crash-fail <id>) which emits
                ;; \"\\nFAIL <id>\\n\" so the sharded summary accounts for it.
                ;;
                ;; CHUNKING: when a file's per-test bodies are huge (loop /
                ;; multiple-value-bind / handler-case nests — see times.lsp,
                ;; expt.lsp, floor.lsp), packing them all into one
                ;; run-ansi-FILE function tips the MVM compiler past some
                ;; codegen threshold and the resulting fork crashes
                ;; uncatchably BEFORE any T:N marker — losing every test
                ;; in the file (times.lsp = 0 / 30 historically).  Split
                ;; into chunks of +chunk-size+ tests each and have
                ;; run-ansi-FILE call them in sequence.
                ;;
                ;; WS3 P1: in diff mode we run ONLY the e2diff runners, so SKIP
                ;; the normal run-ansi-FILE/chunk emission entirely — the diff
                ;; image otherwise carries BOTH the run-test thunks AND the
                ;; (quote FORM) e2diff literals, doubling the corpus footprint
                ;; and overflowing the fixed 128MB MVM bytecode buffer.  (The
                ;; companion run-real-ansi-tests dispatcher is likewise skipped
                ;; below; the driver calls run-real-e2diff instead.)
                (unless *mvm-eval-diff-mode*
                (let ((chunk-size 8)
                      (forms (nreverse test-forms))
                      (chunk-num 0)
                      (chunk-defs nil))
                  ;; Emit run-ansi-FILE-CHUNK-N defuns first.  Each
                  ;; handler-case calls the same %test-crash-fail helper as
                  ;; the old monolithic emission did.
                  (let ((remaining forms))
                    (loop while remaining do
                      (incf chunk-num)
                      (let ((this-chunk (subseq remaining 0
                                                (min chunk-size (length remaining)))))
                        (setq remaining (subseq remaining (length this-chunk)))
                        (push chunk-num chunk-defs)
                        (format out "(defun run-ansi-~A-chunk-~D ()~%"
                                (pathname-name file) chunk-num)
                        (dolist (tf this-chunk)
                          (let* ((form-str tf)
                                 (id-start (position #\Space form-str))
                                 (id-end (position #\Space form-str :start (1+ id-start)))
                                 (id-num (parse-integer form-str :start (1+ id-start)
                                                         :end id-end :junk-allowed t)))
                            (cond
                              ((null id-num)
                               (format out "  (handler-case ~A (t (c) nil))~%" form-str))
                              ;; Known uncatchable-hang skip list — kept empty.
                              ;; The original 13567-13577 (floatp + floor.1-6)
                              ;; and 25630 entries predated the SIGSEGV signal
                              ;; handler and file-alarm-secs; revisiting showed
                              ;; floatp 5/5 + floor.1-3 pass cleanly with no
                              ;; hang.  Any reintroduced hang surfaces as a
                              ;; FILE-WEDGE REASON=no-progress and can be
                              ;; re-added here.
                              (t
                               (format out "  (handler-case ~A (t (c) (%test-crash-fail-c ~D c)))~%"
                                       form-str id-num)))))
                        (format out ")~%"))))
                  ;; Now the dispatcher.  Re-runs init forms (idempotent —
                  ;; defclass updates the registry) so a fork's run-ansi-X
                  ;; still populates the registry even if the parent's
                  ;; run-init-* pass somehow missed it.  Then calls each
                  ;; chunk in order, gated on %chunk-crashed-p so we don't
                  ;; re-enter a chunk that crashed its prologue in a previous
                  ;; fork attempt.  The handler-case around the call records
                  ;; a fresh crash (writes to the MAP_SHARED page and prints
                  ;; CHUNK-CRASH for visibility) so the next fork attempt's
                  ;; %chunk-crashed-p sees it.
                  (let* ((file-name (pathname-name file))
                         (file-hash (logand (modus.mvm::compute-name-hash
                                              (string-upcase file-name))
                                            #xFFFFFF)))
                    (format out "(defun run-ansi-~A ()~%" file-name)
                    (dolist (s init-list)
                      (format out "  (handler-case ~A (t (c) nil))~%" s))
                    (dolist (c (nreverse chunk-defs))
                      (format out "  (%try-chunk ~S ~D ~D #'run-ansi-~A-chunk-~D)~%"
                              file-name file-hash c file-name c))
                    (format out ")~%"))))  ; close let* + let chunk-size + unless diff-mode
                ;; WS3 P1 differential gate: emit a parallel run-e2diff-FILE
                ;; that, after the SAME init-forms as run-ansi-FILE, calls
                ;; (%e2diff ID '(actual)) for every captured test.  Chunked +
                ;; %try-chunk-wrapped like the normal run (huge files blow MVM
                ;; codegen otherwise / a chunk-prologue crash mustn't lose the
                ;; whole file).  Only emitted under MODUS_MVM_EVAL_DIFF.
                (when *mvm-eval-diff-mode*
                  (let ((e2out (make-string-output-stream))
                        (e2forms (nreverse *e2diff-forms*))  ; (id . form-str)
                        (e2chunk-size 8)
                        (e2chunk-num 0)
                        (e2chunk-defs nil)
                        (file-name (pathname-name file))
                        (file-hash (logand (modus.mvm::compute-name-hash
                                             (string-upcase (pathname-name file)))
                                           #xFFFFFF)))
                    ;; Leading newline so this file's block separates from the
                    ;; preceding *real-ansi-sources* content (no separator in
                    ;; *full-source*, keeping flag-off byte-identical).
                    (format e2out "~%;; === e2diff ~A ===~%" file-name)
                    (let ((remaining e2forms))
                      (loop while remaining do
                        (incf e2chunk-num)
                        (let ((this-chunk (subseq remaining 0
                                                  (min e2chunk-size (length remaining)))))
                          (setq remaining (subseq remaining (length this-chunk)))
                          (push e2chunk-num e2chunk-defs)
                          (format e2out "(defun run-e2diff-~A-chunk-~D ()~%"
                                  file-name e2chunk-num)
                          (dolist (pair this-chunk)
                            (format e2out
                                    "  (handler-case (%e2diff ~D (quote ~A)) (t (c) (%e2-chunk-fail ~D c)))~%"
                                    (car pair) (cdr pair) (car pair)))
                          (format e2out ")~%"))))
                    (format e2out "(defun run-e2diff-~A ()~%" file-name)
                    (dolist (s init-list)
                      (format e2out "  (handler-case ~A (t (c) nil))~%" s))
                    (dolist (c (nreverse e2chunk-defs))
                      (format e2out "  (%try-chunk ~S ~D ~D #'run-e2diff-~A-chunk-~D)~%"
                              file-name file-hash c file-name c))
                    (format e2out ")~%")
                    (setf *e2diff-sources*
                          (concatenate 'string *e2diff-sources*
                                       (get-output-stream-string e2out))))))
              (setf *real-ansi-sources*
                    (concatenate 'string *real-ansi-sources*
                                 (get-output-stream-string out)))
              ;; Record the last test-id used in this file (may be nil if none).
              (let ((entry (car *ansi-file-ranges*)))
                (setf (third entry) *ansi-test-counter*))))))
      (error (e)
        (format t "    SKIP ~A: ~A~%" file e)))))

;; Rewrite multi-arg apply calls into 2-arg form that MVM's apply supports.
;; MVM's apply only takes (fn args-list). CL allows (apply fn a1 a2 ... list).
;; (apply fn a1 a2 ... aN list) → (apply fn (append (list a1 a2 ... aN) list))
;; (apply fn list) → unchanged (already 2-arg form)
(defun rewrite-multi-arg-apply (form)
  "RETIRED — prelude.lisp's `(defun apply (fn &rest spread) …)` already
   handles `(apply fn a1 a2 … list)` by building the merged arg list
   and dispatching by length.  The build-side rewrite was redundant
   scaffolding.  Kept as an identity stub to avoid churning every call
   site; the existing calls now do `(mapcar-safe #'identity …)` worth
   of work.  Remove the calls and the stub together in a later
   cleanup pass."
  form)

;; &aux rewriting retired 2026-05-23: preprocess-params in mvm/compiler.lisp
;; expands &aux into a let* wrapping the body at compile time, so the
;; build-side rewrite was redundant scaffolding.  Deletion is the first
;; step of retiring the rewrite-* family (see feedback_no_test_rewrites).
;; Shard delta: −2 P, −15 F, +17 lost vs Sym-identity v2 (essentially flat).

(defvar *ansi-aux-loaded* nil)  ; track which aux files already loaded (avoid duplicates)

(defun load-ansi-aux (filename)
  "Transform an ANSI test auxiliary file into MVM-compatible source.
   Emits defun/defstruct/defvar/defparameter/defconstant forms into *ansi-aux-sources*.
   Skips CLOS methods, defgeneric, and forms that reference unsupported features."
  (let ((path (concatenate 'string "/home/claude/modus/tmp/ansi-test/auxiliary/" filename)))
    (when (member filename *ansi-aux-loaded* :test #'string=)
      (return-from load-ansi-aux nil))
    (unless (probe-file path)
      (format t "  AUX MISSING: ~A~%" filename)
      (return-from load-ansi-aux nil))
    (push filename *ansi-aux-loaded*)
    (format t "  Loading aux: ~A~%" filename)
    (handler-case
      (let ((forms nil))
        (with-open-file (s path :direction :input)
          (let ((*package* (find-package :cl-user)))
            (loop (let ((form (read s nil :eof)))
                    (when (eq form :eof) (return))
                    (push form forms)))))
        ;; Apply the same rewriter pipeline as test files.  Use a
        ;; per-form mapcar that catches errors and leaves the form
        ;; unchanged — a defmacro body containing SBCL backquote
        ;; comma objects (SB-INT:UNQUOTE) is not consp, so the
        ;; tree-walking rewriters can hit (car comma-struct) and
        ;; type-error.  Without the per-form catch the WHOLE file is
        ;; dropped from the aux source.
        (setf forms (nreverse forms))
        ;; Expand aux-defined macros BEFORE the rewriters run.  Some aux
        ;; helpers are defuns whose body is a call to a macro defined in the
        ;; SAME aux file — e.g. package-aux.lsp's
        ;;   (defun with-package-iterator-internal (packages)
        ;;     (test-with-package-iterator packages :internal))
        ;; where TEST-WITH-PACKAGE-ITERATOR is a defmacro that expands into a
        ;; literal WITH-PACKAGE-ITERATOR form.  The WITH-PACKAGE-ITERATOR
        ;; rewriter (in rewrite-reader-forms) only fires on a *literal*
        ;; with-package-iterator head, so if the form is still hidden behind
        ;; the helper macro at rewrite time, it is never rewritten and the
        ;; emitted MVM source calls an unknown macro → NIL at runtime.  Eval
        ;; this file's own defmacros first, then macroexpand the remaining
        ;; forms so the hidden constructs surface for the rewriters.
        (dolist (form forms)
          (when (and (consp form) (eq (car form) 'defmacro))
            (handler-case (eval form) (error () nil))))
        ;; Expand a TOP-LEVEL aux-macro call appearing as a defun body form.
        ;; Narrowly targets the helper-defun pattern
        ;;   (defun f (...) (AUX-MACRO ...))
        ;; e.g. package-aux.lsp's with-package-iterator-internal, whose body
        ;; (test-with-package-iterator ...) expands into a literal
        ;; with-package-iterator form the rewriter then handles.  A full
        ;; recursive macroexpand-all blows the host stack on the large
        ;; deeply-nested aux forms, so we only expand each body form's own
        ;; head (repeatedly, until it is no longer a host macro call) — no
        ;; descent into sub-forms.  defmacro templates are left untouched.
        ;;
        ;; CRITICAL: only expand AUX-DEFINED macros (interned in CL-USER),
        ;; NOT standard CL macros.  Expanding a CL macro (loop/cond/when/...)
        ;; would splice SBCL-internal forms (sb-loop, tagbody, ...) into the
        ;; emitted MVM source and corrupt those aux defuns — the MVM compiler
        ;; has its own macros and must receive the surface form.  An aux macro
        ;; like TEST-WITH-PACKAGE-ITERATOR lives in CL-USER; CL:LOOP lives in
        ;; COMMON-LISP, so the home-package gate cleanly separates them.
        (flet ((aux-macro-p (sym)
                 (and (symbolp sym)
                      (macro-function sym)
                      (let ((p (symbol-package sym)))
                        (and p (string= (package-name p) "COMMON-LISP-USER")))))
               (defining-form-p (sym)
                 (member sym '(quote defmacro defun defstruct deftype
                               defparameter defvar defconstant
                               defclass defgeneric defmethod))))
          (flet ((expand-head (f)
                 (handler-case
                     (let ((g f) (guard 0))
                       (loop
                         (when (or (not (consp g)) (not (symbolp (car g)))
                                   (>= guard 50)
                                   (defining-form-p (car g))
                                   (not (aux-macro-p (car g))))
                           (return g))
                         (let ((ex (macroexpand-1 g)))
                           (when (eq ex g) (return g))
                           (setf g ex)
                           (incf guard))))
                   (error () f))))
          (setf forms
                (mapcar (lambda (form)
                          (if (and (consp form) (eq (car form) 'defun))
                              (handler-case
                                  (list* 'defun (cadr form) (caddr form)
                                         (mapcar #'expand-head (cdddr form)))
                                (error () form))
                              form))
                        forms))))
        (flet ((mapcar-safe (fn lst)
                 (mapcar (lambda (f) (handler-case (funcall fn f)
                                       (error () f)))
                         lst)))
          (setf forms (mapcar-safe #'rewrite-package-iteration forms))
          ;; rewrite-make-array-* RETIRED (two-pass / aux path) — native MDA
          ;; + runtime make-array + MDA-aware rt-equal/printer handle the
          ;; unmodified test forms.
          (setf forms (mapcar-safe #'rewrite-eval-quote forms))
          (setf forms (mapcar-safe #'rewrite-earmuff-specials forms))
          (setf forms (mapcar-safe #'rewrite-reader-forms forms))
          ;; &aux retired: preprocess-params handles it natively.
          ;; Rewrite (apply fn a1 a2 ... list) → (apply fn (append (list a1 a2 ...) list))
          ;; MVM's apply only handles (fn list) form; CL allows spread args before final list.
          (setf forms (mapcar-safe #'rewrite-multi-arg-apply forms)))
        ;; Evaluate defun/defmacro forms at SBCL side so macros defined here
        ;; can be used during macroexpansion of test files loaded after this.
        (dolist (form forms)
          (when (and (consp form)
                     (member (car form) '(defun defmacro defstruct defparameter defvar)))
            (handler-case (eval form) (error () nil))))
        (labels
          ;; Strip package prefixes that MVM doesn't understand:
          ;; REGRESSION-TEST:: and CL-TEST:: symbols → unqualified symbols.
          ;; This is done as a tree walk so the symbols themselves are renamed.
          ((strip-pkg-prefix (form)
             (cond
               ((symbolp form)
                (let* ((name (symbol-name form))
                       (pkg  (symbol-package form))
                       (pname (and pkg (package-name pkg))))
                  (if (and pname (or (string= pname "REGRESSION-TEST")
                                     (string= pname "CL-TEST")))
                      (intern name)   ; re-intern in cl-user
                      form)))
               ((consp form)
                (cons (strip-pkg-prefix (car form))
                      (strip-pkg-prefix (cdr form))))
               (t form))))
          (let ((out (make-string-output-stream)))
            (format out "~%;; === aux: ~A ===~%" filename)
            (dolist (form forms)
              ;; Per-form handler-case: errors processing one form (e.g.
              ;; comma objects inside a defmacro's backquote body that
              ;; the simple cons-tree walker can't traverse) don't kill
              ;; the rest of the file's defuns.
              (handler-case
              (when (consp form)
                ;; Process (declaim (special VAR ...)) before stripping so
                ;; the names get added to the file-local special-var set;
                ;; otherwise (let ((*x* ...)) ...) inside the file binds
                ;; *x* lexically and method bodies (interp-closures from
                ;; runtime eval) read the global instead — dgmc.* tests
                ;; all use *x* this way.
                (when (and (eq (car form) 'declaim) (consp (cdr form)))
                  (dolist (decl (cdr form))
                    (when (and (consp decl) (eq (car decl) 'special))
                      (dolist (var (cdr decl))
                        (when (symbolp var)
                          (pushnew (symbol-name var)
                                   modus.mvm::*clhs-extra-specials*
                                   :test #'string=))))))
                ;; Skip forms that can't compile on MVM or reference SBCL internals:
                ;; defgeneric, defmethod, eval-when, declaim, proclaim, compile-and-load
                (when (member (car form) '(defgeneric defmethod eval-when declaim proclaim
                                          compile-and-load in-package))
                  (setf form nil))
                (when form
                  ;; Strip REGRESSION-TEST:: and CL-TEST:: prefixes
                  (setf form (strip-pkg-prefix form))
                  ;; For progn wrapping (from rewritten defclass etc.):
                  ;; split into sub-forms and process each
                  (if (and (consp form) (eq (car form) 'progn))
                      (dolist (sub (cdr form))
                        (when (and (consp sub)
                                   (member (car sub) '(defun defvar defparameter defstruct
                                                       defconstant %defclass)))
                          (let ((s (handler-case (format nil "~S" sub) (error () nil))))
                            (when (and s
                                       (not (search "#<" s))
                                       (not (search "&ENVIRONMENT" s)))
                              (write-string s out)
                              (terpri out)))))
                      ;; Top-level form: emit if it's a defun/defstruct definition.
                      ;; defvar/defparameter/defconstant: only emit if init value is a
                      ;; simple literal (string, number, nil, t, or quoted form).
                      ;; Complex init expressions may call undefined functions and crash.
                      (cond
                        ((member (car form) '(defun defstruct deftype %defclass))
                         (let ((s (handler-case (format nil "~S" form) (error () nil))))
                           (when (and s
                                      (not (search "#<" s))
                                      (not (search "&ENVIRONMENT" s)))
                             (write-string s out)
                             (terpri out))))
                        ((member (car form) '(defvar defparameter defconstant))
                         ;; Skip defparameters whose init values are known to crash on MVM:
                         ;; - (make-int-array N): uses funcall with keyword args that MVM doesn't handle
                         ;; - (if (boundp ...)):  boundp not available at init time for specials
                         ;; All other defparameters are emitted as-is.
                         (let ((name (cadr form))
                               (init (if (cddr form) (caddr form) nil))
                               (skip-p nil))
                           ;; Skip *displaced* (make-int-array 100000) — complex funcall
                           (when (and (symbolp name)
                                      (string= (symbol-name name) "*DISPLACED*"))
                             (setf skip-p t))
                           ;; Skip *initial-print-pprint-dispatch* (uses boundp)
                           (when (and (symbolp name)
                                      (string= (symbol-name name)
                                               "*INITIAL-PRINT-PPRINT-DISPATCH*"))
                             (setf skip-p t))
                           ;; Skip *similarity-list* (defgeneric is-similar* not in MVM)
                           (when (and (symbolp name)
                                      (string= (symbol-name name) "*SIMILARITY-LIST*"))
                             (setf skip-p t))
                           (unless skip-p
                             (let ((s (handler-case (format nil "~S" form) (error () nil))))
                               (when (and s
                                          (not (search "#<" s))
                                          (not (search "&ENVIRONMENT" s)))
                                 (write-string s out)
                                 (terpri out))))))))))
                (error (e) (declare (ignore e)) nil)))
            (setf *ansi-aux-sources*
                  (concatenate 'string *ansi-aux-sources*
                               (get-output-stream-string out))))))
      (error (e)
        (format t "    SKIP AUX ~A: ~A~%" filename e)))))

;;; ============================================================
;;; Load ANSI test files by chapter
;;; ============================================================

;;; Load ALL ANSI test files by chapter
;;; ============================================================

;;; Load auxiliary files first — these define scaffolding used by all chapters
(format t "~%Loading auxiliary files...~%")

;; Core aux: used by almost everything
(load-ansi-aux "ansi-aux.lsp")
(load-ansi-aux "cons-aux.lsp")

;; Chapter-specific aux files
(load-ansi-aux "types-aux.lsp")
(load-ansi-aux "array-aux.lsp")
(load-ansi-aux "bit-aux.lsp")
(load-ansi-aux "char-aux.lsp")
(load-ansi-aux "hash-table-aux.lsp")
(load-ansi-aux "numbers-aux.lsp")
(load-ansi-aux "random-aux.lsp")
(load-ansi-aux "floor-aux.lsp")
(load-ansi-aux "ffloor-aux.lsp")
(load-ansi-aux "ceiling-aux.lsp")
(load-ansi-aux "fceiling-aux.lsp")
(load-ansi-aux "truncate-aux.lsp")
(load-ansi-aux "ftruncate-aux.lsp")
(load-ansi-aux "round-aux.lsp")
(load-ansi-aux "fround-aux.lsp")
(load-ansi-aux "times-aux.lsp")
(load-ansi-aux "division-aux.lsp")
(load-ansi-aux "exp-aux.lsp")
(load-ansi-aux "gcd-aux.lsp")
(load-ansi-aux "string-aux.lsp")
(load-ansi-aux "subseq-aux.lsp")
(load-ansi-aux "search-aux.lsp")
(load-ansi-aux "remove-aux.lsp")
(load-ansi-aux "remove-duplicates-aux.lsp")
(load-ansi-aux "printer-aux.lsp")
(load-ansi-aux "backquote-aux.lsp")
(load-ansi-aux "reader-aux.lsp")
(load-ansi-aux "package-aux.lsp")
(load-ansi-aux "packages00-aux.lsp")
(load-ansi-aux "pathnames-aux.lsp")
(load-ansi-aux "cl-symbols-aux.lsp")
(load-ansi-aux "define-condition-aux.lsp")
(load-ansi-aux "defclass-aux.lsp")

;; Re-establish the build's form-producing def-pprint-test (and the
;; def-ppblock-test that expands into it) AFTER aux loading.  printer-aux.lsp
;; (loaded above) defines def-pprint-test with a &key default
;; `(package (find-package "CL-TEST"))` — the default is EVALUATED at
;; macroexpand time, so `,package` splices a live `#<PACKAGE "CL-TEST">`
;; object into the deftest.  That `#<` print then trips the unreadable-object
;; filter in load-ansi-chapter, silently dropping every def-pprint-test in
;; format-t.lsp (and any other file expanded with that macro) — the whole
;; file then forks to a zero-tests FILE-WEDGE.  Our version (defined far
;; above) keeps the package as the FORM `(find-package "CL-TEST")`, which
;; round-trips through the Modus reader.
(eval '(defmacro def-pprint-test (name form expected-value &rest keys)
        (let ((margin (getf keys :margin 100))
              (miser (getf keys :miser nil))
              (circle (getf keys :circle nil))
              (len (getf keys :len nil))
              (pretty (getf keys :pretty t))
              (escape (getf keys :escape nil))
              (readably (getf keys :readably nil))
              (package (or (getf keys :package) '(find-package "CL-TEST"))))
          `(deftest ,name
             (%with-standard-io-syntax
              (lambda ()
                (let ((*print-pretty* ,pretty)
                      (*print-escape* ,escape)
                      (*print-readably* ,readably)
                      (*print-right-margin* ,margin)
                      (*package* ,package)
                      (*print-length* ,len)
                      (*print-miser-width* ,miser)
                      (*print-circle* ,circle))
                  (declare (special *print-pretty* *print-escape* *print-readably*
                                    *print-right-margin* *package* *print-length*
                                    *print-miser-width* *print-circle*))
                  ,form)))
             ,expected-value))))

(format t "  aux sources: ~D chars~%" (length *ansi-aux-sources*))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/cons/"
  '("acons.lsp" "adjoin.lsp" "append.lsp" "assoc-if-not.lsp" "assoc-if.lsp" "assoc.lsp" "atom.lsp" "butlast.lsp" "cons-test-01.lsp" "cons-test-03.lsp" "cons-test-05.lsp" "cons.lsp" "consp.lsp" "copy-alist.lsp" "copy-list.lsp" "copy-tree.lsp" "cxr.lsp" "endp.lsp" "get-properties.lsp" "getf.lsp" "intersection.lsp" "last.lsp" "ldiff.lsp" "list-length.lsp" "list.lsp" "listp.lsp" "load.lsp" "make-list.lsp" "mapc.lsp" "mapcan.lsp" "mapcar.lsp" "mapcon.lsp" "mapl.lsp" "maplist.lsp" "member-if-not.lsp" "member-if.lsp" "member.lsp" "nbutlast.lsp" "nconc.lsp" "nintersection.lsp" "nreconc.lsp" "nset-difference.lsp" "nset-exclusive-or.lsp" "nsublis.lsp" "nsubst-if-not.lsp" "nsubst-if.lsp" "nsubst.lsp" "nth.lsp" "nthcdr.lsp" "nunion.lsp" "pairlis.lsp" "pop.lsp" "push.lsp" "pushnew.lsp" "rassoc-if-not.lsp" "rassoc-if.lsp" "rassoc.lsp" "remf.lsp" "rest.lsp" "revappend.lsp" "rplaca.lsp" "rplacd.lsp" "set-difference.lsp" "set-exclusive-or.lsp" "sublis.lsp" "subsetp.lsp" "subst-if-not.lsp" "subst-if.lsp" "subst.lsp" "tailp.lsp" "tree-equal.lsp" "union.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/data-and-control-flow/"
  '("and.lsp" "apply.lsp" "block.lsp" "call-arguments-limit.lsp" "case.lsp" "catch.lsp" "ccase.lsp" "compiled-function-p.lsp" "complement.lsp" "cond.lsp" "constantly.lsp" "ctypecase.lsp" "data-and-control-flow.lsp" "defconstant.lsp" "define-modify-macro.lsp" "define-setf-expander.lsp" "defparameter.lsp" "defsetf.lsp" "defun.lsp" "defvar.lsp" "destructuring-bind.lsp" "ecase.lsp" "eql.lsp" "equal.lsp" "equalp.lsp" "etypecase.lsp" "every.lsp" "fboundp.lsp" "fdefinition.lsp" "flet.lsp" "fmakunbound.lsp" "funcall.lsp" "function-lambda-expression.lsp" "function.lsp" "functionp.lsp" "get-setf-expansion.lsp" "identity.lsp" "if.lsp" "labels.lsp" "lambda-list-keywords.lsp" "lambda-parameters-limit.lsp" "let.lsp" "letstar.lsp" "load.lsp" "macrolet.lsp" "multiple-value-bind.lsp" "multiple-value-call.lsp" "multiple-value-list.lsp" "multiple-value-prog1.lsp" "multiple-value-setq.lsp" "nil.lsp" "not-and-null.lsp" "notany.lsp" "notevery.lsp" "nth-value.lsp" "or.lsp" "places.lsp" "prog.lsp" "prog1.lsp" "prog2.lsp" "progn.lsp" "progv.lsp" "psetf.lsp" "psetq.lsp" "return-from.lsp" "return.lsp" "rotatef.lsp" "shiftf.lsp" "some.lsp" "t.lsp" "tagbody.lsp" "typecase.lsp" "unless.lsp" "unwind-protect.lsp" "values-list.lsp" "values.lsp" "when.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/hash-tables/"
  '("clrhash.lsp" "gethash.lsp" "hash-table-count.lsp" "hash-table-p.lsp" "hash-table-rehash-size.lsp" "hash-table-rehash-threshold.lsp" "hash-table-size.lsp" "hash-table-test.lsp" "hash-table.lsp" "load.lsp" "make-hash-table.lsp" "maphash.lsp" "remhash.lsp" "sxhash.lsp" "with-hash-table-iterator.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/numbers/"
  '("abs.lsp" "acos.lsp" "acosh.lsp" "arithmetic-error.lsp" "ash.lsp" "asin.lsp" "asinh.lsp" "atan.lsp" "atanh.lsp" "boole.lsp" "byte.lsp" "ceiling.lsp" "cis.lsp" "complex.lsp" "complexp.lsp" "conjugate.lsp" "cos.lsp" "cosh.lsp" "decf.lsp" "deposit-field.lsp" "divide.lsp" "dpb.lsp" "epsilons.lsp" "evenp.lsp" "exp.lsp" "expt.lsp" "fceiling.lsp" "ffloor.lsp" "float.lsp" "floatp.lsp" "floor.lsp" "fround.lsp" "ftruncate.lsp" "gcd.lsp" "imagpart.lsp" "incf.lsp" "integer-length.lsp" "integerp.lsp" "isqrt.lsp" "lcm.lsp" "ldb.lsp" "load.lsp" "log.lsp" "logand.lsp" "logandc1.lsp" "logandc2.lsp" "logbitp.lsp" "logcount.lsp" "logeqv.lsp" "logior.lsp" "lognand.lsp" "lognor.lsp" "lognot.lsp" "logorc1.lsp" "logorc2.lsp" "logtest.lsp" "logxor.lsp" "make-random-state.lsp" "mask-field.lsp" "max.lsp" "min.lsp" "minus.lsp" "minusp.lsp" "number-comparison.lsp" "numberp.lsp" "numerator-denominator.lsp" "oddp.lsp" "oneminus.lsp" "oneplus.lsp" "parse-integer.lsp" "phase.lsp" "plus.lsp" "plusp.lsp" "random-state-p.lsp" "random.lsp" "rational.lsp" "rationalize.lsp" "rationalp.lsp" "real.lsp" "realp.lsp" "realpart.lsp" "round.lsp" "signum.lsp" "sin.lsp" "sinh.lsp" "sqrt.lsp" "tan.lsp" "tanh.lsp" "times.lsp" "truncate.lsp" "upgraded-complex-part-type.lsp" "zerop.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/symbols/"
  '("boundp.lsp" "cl-symbols.lsp" "copy-symbol.lsp" "gensym.lsp" "gentemp.lsp" "get.lsp" "keywordp.lsp" "load.lsp" "make-symbol.lsp" "makunbound.lsp" "remprop.lsp" "set.lsp" "special-operator-p.lsp" "symbol-function.lsp" "symbol-name.lsp" "symbolp.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/structures/"
  '("load.lsp" "structure-00.lsp" "structures-01.lsp" "structures-02.lsp" "structures-03.lsp" "structures-04.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/strings/"
  '("base-string.lsp" "char-schar.lsp" "load.lsp" "make-string.lsp" "nstring-capitalize.lsp" "nstring-downcase.lsp" "nstring-upcase.lsp" "simple-base-string.lsp" "simple-string-p.lsp" "simple-string.lsp" "string-capitalize.lsp" "string-comparisons.lsp" "string-downcase.lsp" "string-left-trim.lsp" "string-right-trim.lsp" "string-trim.lsp" "string-upcase.lsp" "string.lsp" "stringp.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/characters/"
  '("char-compare.lsp" "character.lsp" "load.lsp" "name-char.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/sequences/"
  '("concatenate.lsp" "copy-seq.lsp" "count-if-not.lsp" "count-if.lsp" "count.lsp" "elt.lsp" "fill-strings.lsp" "fill.lsp" "find-if-not.lsp" "find-if.lsp" "find.lsp" "length.lsp" "load.lsp" "make-sequence.lsp" "map-into.lsp" "map.lsp" "merge.lsp" "mismatch.lsp" "nreverse.lsp" "nsubstitute-if-not.lsp" "nsubstitute-if.lsp" "nsubstitute.lsp" "position-if-not.lsp" "position-if.lsp" "position.lsp" "reduce.lsp" "remove-duplicates.lsp" "remove.lsp" "replace.lsp" "reverse.lsp" "search-bitvector.lsp" "search-list.lsp" "search-string.lsp" "search-vector.lsp" "sort.lsp" "stable-sort.lsp" "subseq.lsp" "substitute-if-not.lsp" "substitute-if.lsp" "substitute.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/arrays/"
  '("adjust-array.lsp" "adjustable-array-p.lsp" "aref.lsp" "array-as-class.lsp" "array-dimension.lsp" "array-dimensions.lsp" "array-displacement.lsp" "array-element-type.lsp" "array-has-fill-pointer-p.lsp" "array-in-bounds-p.lsp" "array-misc.lsp" "array-rank.lsp" "array-row-major-index.lsp" "array-t.lsp" "array-total-size.lsp" "array.lsp" "arrayp.lsp" "bit-and.lsp" "bit-andc1.lsp" "bit-andc2.lsp" "bit-eqv.lsp" "bit-ior.lsp" "bit-nand.lsp" "bit-nor.lsp" "bit-not.lsp" "bit-orc1.lsp" "bit-orc2.lsp" "bit-vector-p.lsp" "bit-vector.lsp" "bit-xor.lsp" "bit.lsp" "fill-pointer.lsp" "load.lsp" "make-array.lsp" "row-major-aref.lsp" "sbit.lsp" "simple-array-t.lsp" "simple-array.lsp" "simple-bit-vector-p.lsp" "simple-bit-vector.lsp" "simple-vector-p.lsp" "svref.lsp" "upgraded-array-element-type.lsp" "vector-pop.lsp" "vector-push-extend.lsp" "vector-push.lsp" "vector.lsp" "vectorp.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/iteration/"
  '("do.lsp" "dolist.lsp" "dostar.lsp" "dotimes.lsp" "load.lsp" "loop.lsp" "loop1.lsp" "loop10.lsp" "loop11.lsp" "loop12.lsp" "loop13.lsp" "loop14.lsp" "loop15.lsp" "loop16.lsp" "loop17.lsp" "loop2.lsp" "loop3.lsp" "loop4.lsp" "loop5.lsp" "loop6.lsp" "loop7.lsp" "loop8.lsp" "loop9.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/printer/"
  '("copy-pprint-dispatch.lsp" "pprint-dispatch.lsp" "pprint-exit-if-list-exhausted.lsp" "pprint-fill.lsp" "pprint-indent.lsp" "pprint-linear.lsp" "pprint-logical-block.lsp" "pprint-newline.lsp" "pprint-tab.lsp" "pprint-tabular.lsp" "pprint.lsp" "prin1-to-string.lsp" "prin1.lsp" "princ-to-string.lsp" "princ.lsp" "print-array.lsp" "print-bit-vector.lsp" "print-characters.lsp" "print-complex.lsp" "print-cons.lsp" "print-floats.lsp" "print-integers.lsp" "print-length.lsp" "print-level.lsp" "print-lines.lsp" "print-pathname.lsp" "print-random-state.lsp" "print-ratios.lsp" "print-strings.lsp" "print-structure.lsp" "print-symbols.lsp" "print-unreadable-object.lsp" "print-vector.lsp" "print.lsp" "printer-control-vars.lsp" "write-to-string.lsp" "write.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/printer/format/"
  '("format-a.lsp" "format-ampersand.lsp" "format-b.lsp" "format-brace.lsp" "format-c.lsp" "format-circumflex.lsp" "format-conditional.lsp" "format-d.lsp" "format-f.lsp" "format-goto.lsp" "format-justify.lsp" "format-logical-block.lsp" "format-newline.lsp" "format-o.lsp" "format-p.lsp" "format-page.lsp" "format-paren.lsp" "format-percent.lsp" "format-question.lsp" "format-r.lsp" "format-s.lsp" "format-t.lsp" "format-tilde.lsp" "format-x.lsp" "formatter-c.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/streams/"
  '("broadcast-stream-streams.lsp" "clear-input.lsp" "clear-output.lsp" "concatenated-stream-streams.lsp" "echo-stream-input-stream.lsp" "echo-stream-output-stream.lsp" "file-length.lsp" "file-position.lsp" "file-string-length.lsp" "finish-output.lsp" "force-output.lsp" "fresh-line.lsp" "get-output-stream-string.lsp" "input-stream-p.lsp" "interactive-stream-p.lsp" "listen.lsp" "load.lsp" "make-broadcast-stream.lsp" "make-concatenated-stream.lsp" "make-echo-stream.lsp" "make-string-input-stream.lsp" "make-string-output-stream.lsp" "make-synonym-stream.lsp" "make-two-way-stream.lsp" "open-stream-p.lsp" "open.lsp" "output-stream-p.lsp" "peek-char.lsp" "read-byte.lsp" "read-char-no-hang.lsp" "read-char.lsp" "read-line.lsp" "read-sequence.lsp" "stream-element-type.lsp" "stream-error-stream.lsp" "stream-external-format.lsp" "streamp.lsp" "synonym-stream-symbol.lsp" "terpri.lsp" "two-way-stream-input-stream.lsp" "two-way-stream-output-stream.lsp" "unread-char.lsp" "with-input-from-string.lsp" "with-open-file.lsp" "with-open-stream.lsp" "with-output-to-string.lsp" "write-char.lsp" "write-line.lsp" "write-sequence.lsp" "write-string.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/packages/"
  '("defpackage.lsp" "delete-package.lsp" "do-all-symbols.lsp" "do-external-symbols.lsp" "do-symbols.lsp" "export.lsp" "find-all-symbols.lsp" "find-package.lsp" "find-symbol.lsp" "import.lsp" "in-package.lsp" "intern.lsp" "keyword.lsp" "list-all-packages.lsp" "load.lsp" "make-package.lsp" "package-error-package.lsp" "package-error.lsp" "package-name.lsp" "package-nicknames.lsp" "package-shadowing-symbols.lsp" "package-use-list.lsp" "package-used-by-list.lsp" "packagep.lsp" "rename-package.lsp" "shadow.lsp" "shadowing-import.lsp" "unexport.lsp" "unintern.lsp" "unuse-package.lsp" "use-package.lsp" "with-package-iterator.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/eval-and-compile/"
  '("compile.lsp" "compiler-macros.lsp" "constantp.lsp" "declaim.lsp" "declaration.lsp" "define-compiler-macro.lsp" "define-symbol-macro.lsp" "defmacro.lsp" "dynamic-extent.lsp" "eval-and-compile.lsp" "eval-when.lsp" "eval.lsp" "ignorable.lsp" "ignore.lsp" "lambda.lsp" "load.lsp" "locally.lsp" "macro-function.lsp" "macroexpand-1.lsp" "macroexpand.lsp" "optimize.lsp" "proclaim.lsp" "special.lsp" "symbol-macrolet.lsp" "the.lsp" "type.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/types-and-classes/"
  '("class-precedence-lists.lsp" "coerce.lsp" "deftype.lsp" "load.lsp" "standard-generic-function.lsp" "subtypep-array.lsp" "subtypep-complex.lsp" "subtypep-cons.lsp" "subtypep-eql.lsp" "subtypep-float.lsp" "subtypep-function.lsp" "subtypep-integer.lsp" "subtypep-member.lsp" "subtypep-rational.lsp" "subtypep-real.lsp" "subtypep.lsp" "type-of.lsp" "typep.lsp" "types-and-class-2.lsp" "types-and-class.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/reader/"
  '("copy-readtable.lsp" "dispatch-macro-characters.lsp" "get-macro-character.lsp" "load.lsp" "read-delimited-list.lsp" "read-from-string.lsp" "read-preserving-whitespace.lsp" "read-suppress.lsp" "read.lsp" "reader-test.lsp" "readtable-case.lsp" "readtablep.lsp" "set-macro-character.lsp" "set-syntax-from-char.lsp" "syntax-tokens.lsp" "syntax.lsp" "with-standard-io-syntax.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/environment/"
  '("apropos-list.lsp" "apropos.lsp" "decode-universal-time.lsp" "describe.lsp" "disassemble.lsp" "documentation.lsp" "dribble.lsp" "ed.lsp" "encode-universal-time.lsp" "environment-functions.lsp" "get-internal-time.lsp" "get-universal-time.lsp" "inspect.lsp" "load.lsp" "room.lsp" "sleep.lsp" "time.lsp" "trace.lsp" "user-homedir-pathname.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/conditions/"
  '("abort.lsp" "assert.lsp" "cell-error-name.lsp" "cerror.lsp" "check-type.lsp" "compute-restarts.lsp" "condition.lsp" "continue.lsp" "define-condition.lsp" "error.lsp" "handler-bind.lsp" "handler-case.lsp" "ignore-errors.lsp" "invoke-debugger.lsp" "load.lsp" "make-condition.lsp" "muffle-warning.lsp" "restart-bind.lsp" "restart-case.lsp" "store-value.lsp" "use-value.lsp" "warn.lsp" "with-condition-restarts.lsp" "with-simple-restart.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/pathnames/"
  '("directory-namestring.lsp" "enough-namestring.lsp" "file-namestring.lsp" "host-namestring.lsp" "load-logical-pathname-translations.lsp" "load.lsp" "logical-pathname-translations.lsp" "logical-pathname.lsp" "make-pathname.lsp" "merge-pathnames.lsp" "namestring.lsp" "parse-namestring.lsp" "pathname-device.lsp" "pathname-directory.lsp" "pathname-host.lsp" "pathname-match-p.lsp" "pathname-name.lsp" "pathname-type.lsp" "pathname-version.lsp" "pathname.lsp" "pathnamep.lsp" "pathnames.lsp" "translate-logical-pathname.lsp" "wild-pathname-p.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/system-construction/"
  '("compile-file.lsp" "features.lsp" "load-file.lsp" "load.lsp" "modules.lsp" "with-compilation-unit.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/files/"
  '("delete-file.lsp" "directory.lsp" "ensure-directories-exist.lsp" "file-author.lsp" "file-error.lsp" "file-write-date.lsp" "load.lsp" "probe-file.lsp" "rename-file.lsp" "truename.lsp" ))

(load-ansi-chapter "/home/claude/modus/tmp/ansi-test/objects/"
  '("add-method.lsp" "allocate-instance.lsp" "call-next-method.lsp" "change-class.lsp" "class-name.lsp" "class-of.lsp" "compute-applicable-methods.lsp" "defclass-01.lsp" "defclass-02.lsp" "defclass-03.lsp" "defclass-errors.lsp" "defclass-forward-reference.lsp" "defclass.lsp" "defgeneric-method-combination-and.lsp" "defgeneric-method-combination-append.lsp" "defgeneric-method-combination-aux.lsp" "defgeneric-method-combination-list.lsp" "defgeneric-method-combination-max.lsp" "defgeneric-method-combination-min.lsp" "defgeneric-method-combination-nconc.lsp" "defgeneric-method-combination-or.lsp" "defgeneric-method-combination-plus.lsp" "defgeneric-method-combination-progn.lsp" "defgeneric.lsp" "define-method-combination-long-form.lsp" "define-method-combination.lsp" "defmethod.lsp" "ensure-generic-function.lsp" "find-class.lsp" "find-method.lsp" "load.lsp" "make-instance.lsp" "make-instances-obsolete.lsp" "make-load-form-saving-slots.lsp" "make-load-form.lsp" "method-qualifiers.lsp" "next-method-p.lsp" "no-applicable-method.lsp" "no-next-method.lsp" "reinitialize-instance.lsp" "remove-method.lsp" "shared-initialize.lsp" "slot-boundp.lsp" "slot-exists-p.lsp" "slot-makunbound.lsp" "slot-missing.lsp" "slot-unbound.lsp" "slot-value.lsp" "unbound-slot.lsp" "update-instance-for-different-class.lsp" "with-accessors.lsp" "with-slots.lsp" ))

