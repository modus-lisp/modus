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

(defun %builder-boot-init ()
  "At boot (%JIT-BOOT-INIT): the AArch64 and RV32 translators' configuration
   and the target registry, which their build scripts set host-side."
  (handler-case (%init-aarch64-translator) (error () nil))
  (handler-case (%init-riscv32-translator) (error () nil))
  (handler-case (%init-selfhost-targets) (error () nil))
  t)

(defun %builder-prepare-target (target)
  "Register the targets (their DEFPARAMETERs are host data) and give the
   foreign translators the configuration their build scripts give them."
  (%init-selfhost-targets)
  (setq *builder-targets-ready* t)
  (cond ((eq target :linux-aarch64) (%init-aarch64-translator))
        ((eq target :linux-riscv32) (%init-riscv32-translator))))

(defun %builder-build-image (&rest args)
  "BUILD-IMAGE as the SBCL host runs it: compiler tables empty, this image's
   runtime-JIT settings at their DEFVAR defaults.  Everything is put back on
   the way out, so the calling script keeps evaluating.  ARGS are
   BUILD-IMAGE's: :TARGET and :SOURCE-TEXT."
  (let ((target (getf args :target))
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
          (s-gen-gc modus.mvm.x64::*x64-gen-gc*) (s-hc-fixup *hc-fault-fixup*)
          (s-runtime-p *mvm-eval-runtime-p*) (s-halves *mvm-emit-halves*)
          (s-static *static-build-p*))
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
             (setq modus.mvm.x64::*x64-gen-gc* nil)
             (setq *hc-fault-fixup* nil)
             (setq *mvm-eval-runtime-p* nil)
             (setq *mvm-emit-halves* nil)
             (setq *static-build-p* nil)
             (apply #'build-image args))
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
              modus.mvm.x64::*x64-gen-gc* s-gen-gc *hc-fault-fixup* s-hc-fixup
              *mvm-eval-runtime-p* s-runtime-p *mvm-emit-halves* s-halves
              *static-build-p* s-static)))))
