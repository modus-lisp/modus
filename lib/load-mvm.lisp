;;;; load-mvm.lisp — Shared MVM system loading boilerplate
;;;;
;;;; Defines *modus-base* and mvm-load, then loads the complete MVM system:
;;;; packages, assembler, ISA, compiler, interpreter, boot descriptors,
;;;; and all architecture translators.
;;;;
;;;; Usage from build scripts:
;;;;   (load "lib/load-mvm.lisp")  ; or via mvm-load after *modus-base* is set

;;; Base directory (parent of mvm/)
(defvar *modus-base*
  (let* ((this-dir (directory-namestring (truename *load-truename*)))
         (modus-dir (namestring (truename (merge-pathnames "../" this-dir)))))
    (pathname modus-dir)))

(defun mvm-load (relative-path)
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (load path :verbose nil :print nil)))

(format t "Loading MVM system...~%")

;; Load the whole system inside one compilation unit so SBCL DEFERS
;; forward-reference warnings (undefined-function / undefined-variable) to
;; the end of the load and suppresses those that resolve by then.  The MVM
;; sources reference each other freely across load order (e.g. compiler.lisp
;; uses helpers defined later, boot-aarch64.lisp uses A64-* from
;; translate-aarch64.lisp) — all resolve by the final form, so a single unit
;; makes those refs silent while STILL reporting anything genuinely never
;; defined (this improves the signal-to-noise of the build log; it changes
;; only warning reporting, never codegen).
(with-compilation-unit ()

;; Packages and x86-64 assembler
(mvm-load "mvm/packages.lisp")
(mvm-load "mvm/x64-asm.lisp")

;; MVM core
(mvm-load "mvm/mvm.lisp")
(mvm-load "mvm/target.lisp")
(mvm-load "mvm/compiler.lisp")
(mvm-load "mvm/interp.lisp")

;; Boot descriptors (all architectures)
(mvm-load "boot/boot-x64.lisp")
(mvm-load "boot/boot-riscv.lisp")
(mvm-load "boot/boot-aarch64.lisp")
(mvm-load "boot/boot-rpi.lisp")
(mvm-load "boot/boot-ppc64.lisp")
(mvm-load "boot/boot-ppc32.lisp")
(mvm-load "boot/boot-i386.lisp")
(mvm-load "boot/boot-68k.lisp")
(mvm-load "boot/boot-arm32.lisp")
(mvm-load "boot/boot-uefi-x64.lisp")

;; Architecture translators
(mvm-load "mvm/translate-x64.lisp")
(mvm-load "mvm/translate-riscv.lisp")
(mvm-load "mvm/translate-aarch64.lisp")
(mvm-load "mvm/translate-ppc.lisp")
(mvm-load "mvm/translate-i386.lisp")
(mvm-load "mvm/translate-68k.lisp")
(mvm-load "mvm/translate-arm32.lisp")

;; Cross-compilation pipeline
(mvm-load "mvm/cross.lisp")

;; HOST-ONLY build-time sanity checks (never baked into an image).  Installs
;; an encapsulation around BUILD-IMAGE that audits the assembled blob for
;; globals whose initialisation never runs in that particular image —
;; CLAUDE.md Active Limitation #7.  Must load AFTER cross.lisp, which defines
;; BUILD-IMAGE.  See mvm/build-checks.lisp.
(mvm-load "mvm/build-checks.lisp")

)  ; end with-compilation-unit
