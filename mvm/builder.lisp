;;;; mvm/builder.lisp -- the builder CLI's BUILD-IMAGE entry point.
;;;;
;;;; Baked into the x64 CLI built with MODUS_BUILDER=1, after the cross tooling
;;;; (mvm/selfhost-tooling.lisp: targets, cross.lisp's BUILD-IMAGE, the boot
;;;; descriptors, the AArch64 and RV32 translators).  That image runs a build
;;;; script -- mvm/build-generic-cli.lisp and friends -- in place of SBCL:
;;;; lib/load-mvm.lisp leaves the compiler alone on a modus host (the image's own
;;;; compiler IS the tree's, by construction of the bootstrap), the script sets
;;;; its emission flags with the same SETFs it runs under SBCL, and those reach
;;;; this image's compiler and translators directly because they are the same
;;;; globals.  What the script cannot do is undo the settings this image's OWN
;;;; RUNTIME needs -- its JIT and its evaluator's tables -- which SBCL never had.
;;;; %BUILDER-BUILD-IMAGE does that around the compile and puts them back after,
;;;; so the script runs on.
;;;;
;;;; The rule it embodies is the DDC rule (CLAUDE.md "DDC RULE"): a flag the SBCL
;;;; host never sets has its DEFVAR default there.  Every flag listed below is a
;;;; runtime-JIT setting no build script sets; a flag a script DOES set is the
;;;; script's to decide and is not touched.  A byte difference against SBCL's
;;;; image names the next one to add.

(in-package :modus.mvm)

;;; READING the build's source binds its integer DEFCONSTANTs first.
;;; interp.lisp's CASE keys are #.+op-nop+ &c.: read-time evaluation, and in an
;;; image a DEFCONSTANT is a folded literal with no symbol value.  Scan the
;;; text for "(defconstant +NAME+ <integer>)" on one line and SET each name in
;;; MODUS.MVM (= MODUS-INT here), as SBCL's host has them bound.  Port of
;;; modus-sh's %SELFHOST-PREBIND-CONSTANTS.
(defun %builder-prebind-constants (src)
  (let ((mm (find-package "MODUS.MVM")) (pos 0) (n 0))
    (loop
      (let ((p (search "(defconstant +" src :start2 pos)))
        (when (null p) (return n))
        (let* ((ns (+ p 13))
               (ne (position #\Space src :start ns))
               (close (position #\) src :start p))
               (nl (position #\Newline src :start p)))
          (when (and ne close (< ne close) (or (null nl) (< close nl)))
            (let* ((name (subseq src ns ne))
                   (vs (string-trim " " (subseq src (+ ne 1) close)))
                   (val (handler-case
                            (cond ((zerop (length vs)) nil)
                                  ((and (> (length vs) 2) (char= (char vs 0) #\#)
                                        (char-equal (char vs 1) #\x))
                                   (parse-integer vs :start 2 :radix 16))
                                  ((or (digit-char-p (char vs 0)) (char= (char vs 0) #\-))
                                   (parse-integer vs))
                                  (t nil))
                          (error () nil))))
              (when (and val (plusp (length name)))
                (set (intern name mm) val)
                (setq n (+ n 1)))))
          (setq pos (if close (+ close 1) (+ p 1))))))))

;;; The build's packages, as lib/load-mvm.lisp gives the SBCL host.  Most exist
;;; in this image already (MODUS.MVM is MODUS-INT's nickname); a missing one is
;;; made and un-marked RUNTIME-BORN, or the compiler would qualify every
;;; function key homed there (port of %SELFHOST-MIRROR-HOST-PACKAGES).
(defun %builder-mirror-host-packages ()
  (dolist (spec '(("MODUS.MVM" "COMMON-LISP")
                  ("MODUS.ASM" "COMMON-LISP")
                  ("MODUS.RUNTIME")
                  ("MODUS.MVM.X64" "COMMON-LISP" "MODUS.MVM" "MODUS.ASM")
                  ("MODUS.MVM.I386" "COMMON-LISP" "MODUS.MVM")))
    (unless (find-package (car spec))
      (make-package (car spec) :use (cdr spec))
      (when (and (boundp '*runtime-born-pkgs*) *runtime-born-pkgs*)
        (remhash (compute-name-hash (car spec)) *runtime-born-pkgs*)
        (setq *runtime-born-answers* nil)))))

(defvar *builder-targets-ready* nil)

;;; THE COMPILER'S WORKING STATE AT SBCL'S BUILD CALL, measured: every DEFVAR /
;;; DEFPARAMETER of compiler.lisp, translate-x64.lisp, cross.lisp and x64-asm.lisp,
;;; dumped at build-generic-cli's BUILD-IMAGE call on SBCL and on this image, and
;;; diffed.  These are the ones that differ: this image's runtime compiler and JIT
;;; have filled the tables, advanced the counters (*KW-REST-COUNTER* and
;;; *UWP-SEQ-COUNTER* name generated functions) and left translator state behind
;;; (*X64-LAST-SET-NARGS* lets the translator drop a repeated SET-NARGS).  None is
;;; set by a build script.  (NAME PACKAGE SBCL-VALUE): NIL, 0 or -1.
(defparameter *builder-fresh-state*
  '(("*%NOT-MACRO*" "MODUS.MVM" nil) ("*%SHADOWS-CL-FN*" "MODUS.MVM" nil)
    ("*%SOLE-EVAL-FORM*" "MODUS.MVM" nil)
    ("*BOOTSTRAP-MACRO-TEMPLATE*" "MODUS.MVM" nil)
    ("*CLHS-STANDARD-SPECIALS-HASHES*" "MODUS.MVM" nil)
    ("*COMPOUND-DISPATCH-HASHES*" "MODUS.MVM" nil)
    ("*DEFSTRUCT-EFF-RO*" "MODUS.MVM" nil) ("*DEFSTRUCT-EFF-SLOTS*" "MODUS.MVM" nil)
    ("*E2-GF-NAME-MEMO*" "MODUS.MVM" nil)
    ("*FASL-TAINT*" "MODUS.MVM" nil) ("*FASL-TAINT-WHO*" "MODUS.MVM" nil)
    ("*FUNCTION-TABLE*" "MODUS.MVM" nil)
    ("*GLOBAL-SYMBOL-MACROS*" "MODUS.MVM" nil) ("*GLOBAL-SYMBOL-MACROS-PKG*" "MODUS.MVM" nil)
    ("*GV-CACHE-NAMES*" "MODUS.MVM" nil) ("*HASH-DISPATCH-TABLE*" "MODUS.MVM" nil)
    ("*HC-FAULT-FIXUP*" "MODUS.MVM" nil)
    ("*KW-REST-COUNTER*" "MODUS.MVM" 0) ("*LABEL-COUNTER*" "MODUS.MVM" 0)
    ("*LET-SPECIAL-SITES*" "MODUS.MVM" nil) ("*MACRO-DEFS-EPOCH*" "MODUS.MVM" nil)
    ("*MACRO-NAME-TABLE*" "MODUS.MVM" nil) ("*MEXP-MEMO-GC*" "MODUS.MVM" -1)
    ("*MVM-EMIT-HALVES*" "MODUS.MVM" nil) ("*MVM-EVAL-RUNTIME-P*" "MODUS.MVM" nil)
    ("*PARK-SAFEPOINTS*" "MODUS.MVM" nil)
    ("*RUNTIME-CONSTANTS*" "MODUS.MVM" nil) ("*RUNTIME-SPECIAL-NAMES*" "MODUS.MVM" nil)
    ("*SETF-EXPANDERS-PKG*" "MODUS.MVM" nil) ("*SETF-FUNCTION-REGISTRY*" "MODUS.MVM" nil)
    ("*STRUCT-ACCESSORS*" "MODUS.MVM" nil) ("*UWP-SEQ-COUNTER*" "MODUS.MVM" 0)
    ("*CF-TRACE*" "MODUS.MVM" nil) ("*CF-TRACE-I*" "MODUS.MVM" 0)
    ("*REG-INFO-TABLE*" "MODUS.ASM" nil)
    ("*MCGC-CONS-BIT-LABEL*" "MODUS.MVM.X64" nil)
    ("*X64-CALL-RELOC-NARGS*" "MODUS.MVM.X64" nil) ("*X64-CALL-RELOCS*" "MODUS.MVM.X64" nil)
    ("*X64-CUR-FN-NAME*" "MODUS.MVM.X64" nil)
    ("*X64-JIT-CONSTVEC-FULL-P*" "MODUS.MVM.X64" nil) ("*X64-JIT-MODE*" "MODUS.MVM.X64" nil)
    ("*X64-JIT-SHARED-GC-TRAMP*" "MODUS.MVM.X64" nil)
    ("*X64-LAST-SET-NARGS*" "MODUS.MVM.X64" nil)
    ("*X64-LI-CONST-PATCHES*" "MODUS.MVM.X64" nil) ("*X64-LI-CONSTVEC-MAX*" "MODUS.MVM.X64" -1)))

(defparameter *builder-host-jit-flag-names*
  '(("*X64-WIN-TLS*" . "MODUS.MVM.X64") ("*X64-WIN-SHIM-VA*" . "MODUS.MVM.X64")
    ("*X64-GEN-GC*" . "MODUS.MVM.X64") ("*X64-GC-ENABLED*" . "MODUS.MVM.X64")
    ("*X64-GC-REGION-PERCPU*" . "MODUS.MVM.X64") ("*X64-LINUX-MODE*" . "MODUS.MVM.X64")
    ("*X64-NATIVE-CODE-OFFSET*" . "MODUS.MVM.X64") ("*X64-SCHED-LOCK-ADDR*" . "MODUS.MVM.X64")
    ("*X64-STW*" . "MODUS.MVM.X64") ("*X64-TLS-WINDOW*" . "MODUS.MVM.X64")
    ("*X64-JIT-CONSTVEC-P*" . "MODUS.MVM.X64")
    ("*MCGC-KIND-BITMAP-ENABLED*" . "MODUS.MVM.X64") ("*MCGC-KIND-CHECK-ENABLED*" . "MODUS.MVM.X64")
    ("*MCGC-PINNING-ENABLED*" . "MODUS.MVM.X64") ("*WS5-FORCE-NO-KINDCHECK*" . "MODUS.MVM.X64")
    ;; the compiler's layout settings (mvm/hosted-layout-env.lisp APPLY-LAYOUT-HOST
    ;; sets them for an AArch64 target): read while compiling code to run here
    ("*CONV-RELATIVE*" . "MODUS.MVM") ("*CONV-DELTA*" . "MODUS.MVM")
    ("*HOSTED-LAYOUT*" . "MODUS.MVM") ("*PCREL-LAYOUT*" . "MODUS.MVM")
    ("*TLS-WINDOW*" . "MODUS.MVM"))
  "Translator and compiler variables a build script sets for its TARGET that the
   host's own compiles (runtime JIT, EVAL/LOAD) also read; %BUILDER-BOOT-INIT
   records the host's values, which %JIT-TRANSLATE-PAGE and
   %HOST-COMPILE-TOPLEVEL (mvm-eval.lisp) bind.")

(defparameter *builder-host-jit-flag-defaults*
  '(("*CONV-DELTA*" . 0))
  "Host values for *BUILDER-HOST-JIT-FLAG-NAMES* entries that are unbound at
   boot; an unbound one not listed here is not recorded.")

(defun %builder-state-symbol (name pkg)
  "The symbol of a compiler variable: in PKG or, failing that, MODUS.MVM's (whose
   packages it uses), so a name homed elsewhere in this image is still found."
  (or (let ((p (find-package pkg))) (and p (find-symbol name p)))
      (find-symbol name (find-package "MODUS.MVM"))
      ;; a kernel global no source quotes has no symbol yet: its cell is
      ;; reached by name, so interning it in MODUS-INT names that cell
      (intern name (find-package "MODUS-INT"))))

(defun %builder-enter-fresh-state ()
  "Set the compiler's working state to SBCL's at its build call; the saved values."
  (let ((saved nil))
    (dolist (e *builder-fresh-state*)
      (let ((s (%builder-state-symbol (first e) (second e))))
        (when (and s (boundp s))
          (setq saved (cons (cons s (symbol-value s)) saved))
          (set s (third e)))))
    saved))

(defun %builder-leave-fresh-state (saved)
  (dolist (p saved) (set (car p) (cdr p))))

(defun %builder-boot-init ()
  "At boot (%JIT-BOOT-INIT): the AArch64 and RV32 translators' configuration
   and the target registry, which their build scripts set host-side."
  (handler-case (%init-aarch64-translator) (error () nil))
  (handler-case (%init-riscv32-translator) (error () nil))
  (handler-case (%init-selfhost-targets) (error () nil))
  ;; The host JIT's own translator settings (%JIT-BOOT-INIT has just set them),
  ;; kept apart from what a build script sets for its target (mvm-eval.lisp
  ;; *JIT-HOST-FLAGS*).
  (let ((pairs nil))
    (dolist (e *builder-host-jit-flag-names*)
      (let ((sym (%builder-state-symbol (car e) (cdr e))))
        ;; unbound at boot (a DEFPARAMETER of baked tooling whose initform
        ;; never ran): the host's value is the default the compiler assumes,
        ;; when there is one; otherwise it is left alone
        (let ((dflt (assoc (car e) *builder-host-jit-flag-defaults* :test #'string=)))
          (cond ((and sym (boundp sym))
                 (setq pairs (cons (cons sym (symbol-value sym)) pairs)))
                ((and sym dflt)
                 (setq pairs (cons (cons sym (cdr dflt)) pairs)))))))
    (setq *jit-host-flags* pairs))
  t)

(defun %builder-prepare-target (target)
  "Register the targets (their DEFPARAMETERs are host data) and give the
   foreign translators the configuration their build scripts give them."
  (%init-selfhost-targets)
  (setq *builder-targets-ready* t)
  (cond ((eq target :linux-aarch64) (%init-aarch64-translator))
        ((eq target :linux-riscv32) (%init-riscv32-translator))))

;;; Translator and runtime-JIT state %BUILDER-BUILD-IMAGE resets for the target that the
;;; host's own JIT translations read: besides *BUILDER-FRESH-STATE*'s translator entries.
(defparameter *builder-host-translator-names*
  '(("*X64-JIT-MODE*" . "MODUS.MVM.X64") ("*X64-JIT-CONSTVEC-FULL-P*" . "MODUS.MVM.X64")
    ("*JIT-XLATE-ERR-INFO*" . "MODUS.MVM") ("*JIT-LINKAGE-CELLS*" . "MODUS.MVM")
    ("*PARK-SAFEPOINTS*" . "MODUS.MVM") ("*USE-JIT*" . "MODUS.MVM")
    ("*HC-FAULT-FIXUP*" . "MODUS.MVM")))

(defun %builder-host-translator-state (host-flags)
  "HOST-FLAGS (*JIT-HOST-FLAGS*) plus the current value of every translator variable
   a build resets: the fresh state's MODUS.MVM.X64 / MODUS.ASM entries and
   *BUILDER-HOST-TRANSLATOR-NAMES*.  Fresh conses: %JIT-TRANSLATE-PAGE writes the
   host's changes back into them."
  (let ((pairs nil))
    (flet ((add (name pkg)
             (let ((s (%builder-state-symbol name pkg)))
               (when (and s (boundp s)
                          (not (assoc s host-flags :test #'eq))
                          (not (assoc s pairs :test #'eq)))
                 (setq pairs (cons (cons s (symbol-value s)) pairs))))))
      (dolist (e *builder-fresh-state*)
        (when (member (second e) '("MODUS.MVM.X64" "MODUS.ASM") :test #'string=)
          (add (first e) (second e))))
      (dolist (e *builder-host-translator-names*) (add (car e) (cdr e))))
    (append (mapcar (lambda (p) (cons (car p) (cdr p))) host-flags) pairs)))

(defun %builder-build-image (&rest args)
  "BUILD-IMAGE as the SBCL host runs it: compiler tables empty, this image's
   runtime-JIT settings at their DEFVAR defaults.  Everything is put back on
   the way out, so the calling script keeps evaluating.  ARGS are
   BUILD-IMAGE's -- :TARGET and :SOURCE-TEXT -- and :FINALIZE, a function of
   (SOURCE TARGET) that fills the image-configuration record once the flags are
   the compile's.  A flag a build script sets (*X64-GEN-GC*, *TLS-WINDOW* ...)
   is the script's and is not touched here."
  (let* ((finalize (getf args :finalize))
         (args (let ((out nil) (l args))
                 (loop (when (null l) (return (nreverse out)))
                       (unless (eq (car l) :finalize)
                         (setq out (cons (cadr l) (cons (car l) out))))
                       (setq l (cddr l)))))
         (target (getf args :target))
         (src (getf args :source-text)))
    (%builder-mirror-host-packages)
    (when (stringp src) (%builder-prebind-constants src))
    (%builder-prepare-target target)
    (let ((s-functions *functions*) (s-function-table *function-table*)
          (s-globals *globals*) (s-constants *constants*)
          (s-macro-table *macro-table*) (s-macro-name-table *macro-name-table*)
          (s-bootstrap *bootstrap-macro-template*)
          (s-hash-dispatch *hash-dispatch-table*)
          (s-clhs *clhs-standard-specials-hashes*)
          (s-mexp-memo *mexp-memo*) (s-mexp-memo-gc *mexp-memo-gc*)
          (s-pending-flet *pending-flet-ir*) (s-label *label-counter*)
          (s-use-jit *use-jit*) (s-linkage *jit-linkage-cells*)
          (s-park *park-safepoints*) (s-jit-mode *x64-jit-mode*)
          (s-cv-full *x64-jit-constvec-full-p*) (s-xerr *jit-xlate-err-info*)
          (s-hc-fixup *hc-fault-fixup*)
          (s-runtime-p *mvm-eval-runtime-p*) (s-halves *mvm-emit-halves*)
          (s-static *static-build-p*) (fresh nil)
          (s-host-flags *jit-host-flags*))
      ;; THE HOST'S TRANSLATOR STATE, as it stands now: what follows resets it for
      ;; the target (*X64-JIT-MODE* NIL, the fresh state's translator entries), and
      ;; a function the hot JIT translates mid-build -- a runtime-loaded translator
      ;; is exactly that -- runs HERE.  Translated as image code instead of a JIT
      ;; page, its collector entry and allocation bitmaps were not this image's,
      ;; and a precise root (#x10000FB0) was left pointing at a dead copy.
      (setq *jit-host-flags* (%builder-host-translator-state s-host-flags))
      (unwind-protect
           (progn
             ;; compiler state: the host's empty tables (mvm-compile-all
             ;; registers the bootstrap macros itself)
             (setq *functions* (make-hash-table :test 'equal))
             (setq *function-table* nil)
             (setq *globals* (make-hash-table :test 'eql))
             (setq *constants* (make-hash-table :test 'eql))
             (setq *macro-table* (make-hash-table :test 'eql))
             (setq *macro-name-table* nil)
             (setq *bootstrap-macro-template* nil)
             (setq *hash-dispatch-table* nil)
             (setq *clhs-standard-specials-hashes* nil)
             (setq *mexp-memo* nil)
             (setq *mexp-memo-gc* -1)
             (setq *pending-flet-ir* nil)
             (setq *label-counter* 0)
             ;; this image's runtime JIT and evaluator, at SBCL's defaults
             (setq *use-jit* nil)
             (setq *jit-linkage-cells* nil)
             (setq *park-safepoints* nil)
             (setq *x64-jit-mode* nil)
             (setq *x64-jit-constvec-full-p* nil)
             (setq *jit-xlate-err-info* nil)
             (setq *hc-fault-fixup* nil)
             (setq *mvm-eval-runtime-p* nil)
             (setq *mvm-emit-halves* nil)
             (setq *static-build-p* nil)
             ;; the image's configuration record is taken HERE, after the reset,
             ;; so it names the flags this compile uses (SBCL's are its defaults)
             (setq fresh (%builder-enter-fresh-state))
             ;; an EVAL mid-compile (a target macro's expander) leaves the
             ;; compiler as it found it (mvm-eval.lisp MVM-EVAL-FORMS)
             (setq *in-image-build* t)
             (when (and finalize (stringp src))
               (setf (getf args :source-text) (funcall finalize src target)))
             (apply #'build-image args))
        (setq *in-image-build* nil)
        (setq *jit-host-flags* s-host-flags)
        (%builder-leave-fresh-state fresh)
        (setq *functions* s-functions *function-table* s-function-table
              *globals* s-globals *constants* s-constants
              *macro-table* s-macro-table *macro-name-table* s-macro-name-table
              *bootstrap-macro-template* s-bootstrap
              *hash-dispatch-table* s-hash-dispatch
              *clhs-standard-specials-hashes* s-clhs
              *mexp-memo* s-mexp-memo *mexp-memo-gc* s-mexp-memo-gc
              *pending-flet-ir* s-pending-flet *label-counter* s-label
              *use-jit* s-use-jit *jit-linkage-cells* s-linkage
              *park-safepoints* s-park *x64-jit-mode* s-jit-mode
              *x64-jit-constvec-full-p* s-cv-full *jit-xlate-err-info* s-xerr
              *hc-fault-fixup* s-hc-fixup
              *mvm-eval-runtime-p* s-runtime-p *mvm-emit-halves* s-halves
              *static-build-p* s-static)))))
