;;;; load-mvm.lisp — Shared MVM system loading boilerplate
;;;;
;;;; Defines *modus-base* and mvm-load, then loads the complete MVM system:
;;;; packages, assembler, ISA, compiler, interpreter, boot descriptors,
;;;; and all architecture translators.
;;;;
;;;; Usage from build scripts:
;;;;   (load "lib/load-mvm.lisp")  ; or via mvm-load after *modus-base* is set

;;; A modus host: the SB-EXT entry points the build scripts call (lib/host-modus.lisp).
#-sbcl
(load (merge-pathnames "host-modus.lisp" (directory-namestring (truename *load-truename*))))

;;; Base directory (parent of mvm/)
(defvar *modus-base*
  (let* ((this-dir (directory-namestring (truename *load-truename*)))
         (modus-dir (namestring (truename (merge-pathnames "../" this-dir)))))
    (pathname modus-dir)))

;;; vessel: what makes modus a machine.  modus is the Lisp; the bare-metal drivers, boot
;;; stubs and board runtime a kernel image is compiled from live in the modus-lisp/vessel
;;; repo, under kernel/, at the same relative paths they would have here
;;; (kernel/net/dwc2.lisp is "net/dwc2.lisp").  vessel.pin names the commit a build is
;;; meant to use.  The checkout is MODUS_VESSEL; else .vessel/ in this tree (made at the
;;; pin by scripts/vessel-checkout.sh); else the sibling ../vessel of a workspace.
(defvar *vessel-base*
  (let* ((env (sb-ext:posix-getenv "MODUS_VESSEL"))
         (dir (if (and env (plusp (length env)))
                  (probe-file (concatenate 'string (string-right-trim "/" env) "/"))
                  ;; .vessel/: the checkout scripts/vessel-checkout.sh makes at vessel.pin,
                  ;; inside this tree (gitignored) -- what the gate and a fresh clone use
                  (or (probe-file (merge-pathnames ".vessel/kernel/" *modus-base*))
                      (probe-file (merge-pathnames "../vessel/kernel/" *modus-base*))))))
    (cond ((null dir) nil)
          ((and env (plusp (length env))) (pathname dir))
          (t (make-pathname :directory (butlast (pathname-directory dir))   ; .../kernel/ -> the checkout
                            :name nil :type nil :defaults dir))))
  "The vessel checkout modus's bare-metal builds read their drivers from, or NIL.")

(defun modus-source (relative-path)
  "The file a build means by RELATIVE-PATH: modus's own if modus has it, else vessel's
   kernel/ copy.  Signals, naming both places, when neither has it -- a missing vessel
   checkout must be loud, not a build that silently leaves a driver out."
  (let ((here (merge-pathnames relative-path *modus-base*)))
    (cond ((probe-file here) here)
          ((and *vessel-base*
                (probe-file (merge-pathnames (concatenate 'string "kernel/" relative-path)
                                             *vessel-base*))))
          (t (error "~A is in neither modus (~A) nor vessel (~A).  The bare-metal ~
                     sources live in modus-lisp/vessel: check it out beside modus at the ~
                     commit in vessel.pin, or set MODUS_VESSEL."
                    relative-path *modus-base*
                    (if *vessel-base* (merge-pathnames "kernel/" *vessel-base*) "no checkout found"))))))

(defun mvm-load (relative-path)
  (load (modus-source relative-path) :verbose nil :print nil))

;;; ON A MODUS HOST: the ISA's constants, as values.  The sources read them at READ
;;; time (#.+OP-W32-ADD+ -- 185 names, interp.lisp and the translators), so the build's
;;; parse check and reader need them bound; in a running image they were folded into its
;;; code and are not.  Only mvm/mvm.lisp's DEFCONSTANTs whose symbol is unbound are
;;; evaluated -- not its DEFPACKAGE or its structs, which are the running kernel's.
(defvar *host-opcode-entries* nil
  "On a modus host: the opcode table read from mvm/mvm.lisp's DEFOPCODE forms.")
#-sbcl
(defun %host-bind-isa-constants ()
  (let ((*package* (find-package "COMMON-LISP-USER")) (n 0))
    (with-open-file (in (modus-source "mvm/mvm.lisp"))
      (loop for form = (read in nil in)
            until (eq form in)
            do (cond ((and (consp form) (eq (car form) 'in-package))
                      (setq *package* (find-package (cadr form))))
                     ((and (consp form) (eq (car form) 'defconstant)
                           (not (boundp (cadr form))))
                      (eval form)
                      (incf n))
                     ;; (DEFOPCODE name code operands description): the opcode table,
                     ;; as the data %OPCODE-ENTRIES answers, in the file's order
                     ((and (consp form) (symbolp (car form))
                           (string= (symbol-name (car form)) "DEFOPCODE"))
                      (destructuring-bind (name code operands description) (cdr form)
                        (let ((hit (assoc code *host-opcode-entries*)))
                          (if hit
                              (setf (cdr hit) (list name operands description))
                              (push (list code name operands description) *host-opcode-entries*))))))))
    (setq *host-opcode-entries* (nreverse *host-opcode-entries*))
    n))
#-sbcl
(format t "~&;; modus host: ~D ISA constants bound from mvm/mvm.lisp~%" (%host-bind-isa-constants))

;; HOST-ONLY: record every MODUS_* variable this build reads, FIRST, so the knobs
;; the compiler and translators read while loading are in the record too.  The
;; record is baked into the image as data (lib/image-config.lisp).
(unless (find-package :modus.image-config)
  (mvm-load "lib/image-config.lisp"))

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
;;; ON A MODUS HOST (a build running on modus itself, no SBCL): the compiler, the
;;; boot descriptors and the translators are ALREADY IN THE RUNNING IMAGE -- in the
;;; packages the build would load them into (MODUS-INT, MODUS.ASM, MODUS.MVM.X64 ...),
;;; because a baked symbol records its home by package NAME.  Loading this tree's copy
;;; over them would redefine the running compiler in place (a DEFSTRUCT of a live
;;; kernel struct kills the process), so a modus host USES its own: the forms marked
;;; #+sbcl below are what it skips.  That is the two-stage bootstrap -- a release image
;;; builds a stage-1 image from this tree, and stage 1, whose compiler IS this tree's,
;;; builds again (docs: project memory "build without SBCL").
(with-compilation-unit ()

;; Packages and x86-64 assembler
#+sbcl (mvm-load "mvm/packages.lisp")
#+sbcl (mvm-load "mvm/x64-asm.lisp")

;; MVM core
#+sbcl (mvm-load "mvm/mvm.lisp")
;; the kernel's packages beyond MODUS-INT (builder packages step 3), made now
;; so the build can read source into them
#+sbcl (mvm-load "mvm/kernel-packages.lisp")
#+sbcl (funcall (find-symbol "%ENSURE-KERNEL-PACKAGES" "MODUS.MVM"))
#+sbcl (mvm-load "mvm/target.lisp")
#+sbcl (mvm-load "mvm/compiler.lisp")
#+sbcl (mvm-load "mvm/interp.lisp")

;; Boot descriptors (all architectures)
#+sbcl (mvm-load "boot/boot-uefi-snp.lisp")
#+sbcl (mvm-load "boot/boot-x64.lisp")
#+sbcl (mvm-load "boot/boot-riscv.lisp")
#+sbcl (mvm-load "boot/boot-aarch64.lisp")
#+sbcl (mvm-load "boot/boot-rpi.lisp")
#+sbcl (mvm-load "boot/boot-ppc64.lisp")
#+sbcl (mvm-load "boot/boot-ppc32.lisp")
#+sbcl (mvm-load "boot/boot-i386.lisp")
#+sbcl (mvm-load "boot/boot-68k.lisp")
#+sbcl (mvm-load "boot/boot-arm32.lisp")
#+sbcl (mvm-load "boot/boot-uefi-x64.lisp")

;; Architecture translators
#+sbcl (mvm-load "mvm/translate-x64.lisp")
#+sbcl (mvm-load "mvm/translate-riscv.lisp")
#+sbcl (mvm-load "mvm/translate-aarch64.lisp")
#+sbcl (mvm-load "mvm/translate-ppc.lisp")
#+sbcl (mvm-load "mvm/translate-i386.lisp")
#+sbcl (mvm-load "mvm/translate-68k.lisp")
#+sbcl (mvm-load "mvm/translate-arm32.lisp")

;; The image code that runs the x64 JIT is read in MODUS.MVM (mvm-eval.lisp's
;; page translator, the %JIT-BOOT-INIT glue) and calls the assembler and the
;; translator.  Make the names it writes unqualified READ as the symbols those
;; packages define, so each call names the function it means and none relies
;; on the build compiler's bare-name fallback (builder packages step 2).  Host-
;; only: image text cannot say MODUS.ASM:LABEL-POSITION, because no build
;; package exists in a running image yet (step 4), and the self-host image
;; re-reads this text.  %INIT-X64-TRANSLATOR is interned here, before the
;; co-init text that defines it is read in MODUS.ASM.
#+sbcl
(let ((mvm (find-package "MODUS.MVM")))
  (dolist (s (list (find-symbol "LABEL-POSITION" "MODUS.ASM")
                   (find-symbol "CODE-BUFFER-POSITION" "MODUS.ASM")
                   (find-symbol "CODE-BUFFER-BYTES" "MODUS.ASM")
                   (intern "%INIT-X64-TRANSLATOR" "MODUS.ASM")
                   (find-symbol "TRANSLATE-MVM-TO-X64" "MODUS.MVM.X64")
                   ;; the cross-compiling images' x86-64 target descriptor
                   (find-symbol "EMIT-FUNCTION-PROLOGUE" "MODUS.MVM.X64")
                   (find-symbol "EMIT-FUNCTION-EPILOGUE" "MODUS.MVM.X64")))
    (import s mvm))
  ;; ...and the x64 co-init, read in MODUS.ASM, asks boot-linux-x64 (MODUS.MVM)
  ;; for the native code offset.
  (import (intern "LINUX-X64-NATIVE-CODE-OFFSET" mvm) (find-package "MODUS.ASM"))
  ;; GLOBALS likewise.  The build compiler keys globals by name hash, so these
  ;; cross-package references worked by accident (and mvm-eval's bare DEFVARs
  ;; of two translator variables were, by accident, the translator's).  Make
  ;; each read name the variable it means (CHECK-CROSS-PACKAGE-GLOBALS).
  (let ((x64 (find-package "MODUS.MVM.X64")) (asm (find-package "MODUS.ASM")))
    ;; the co-init (read in MODUS.ASM) sets the translator's configuration
    (dolist (n '("*X64-GC-REGION-PERCPU*" "*X64-NATIVE-CODE-OFFSET*" "*X64-GC-ENABLED*"
                 "*X64-STW*" "*VREG-TO-X64*" "*JIT-XLATE-ERR-INFO*" "*X64-JIT-CONSTVEC-P*"
                 "*X64-JIT-CONSTVEC-FULL-P*" "*X64-SCHED-LOCK-ADDR*" "*X64-TLS-WINDOW*"
                 "*X64-LINUX-MODE*" "*WS5-FORCE-NO-KINDCHECK*" "*MCGC-KIND-BITMAP-ENABLED*"))
      (import (find-symbol n x64) asm))
    (dolist (n '("*HC-FAULT-FIXUP*" "*TLS-WINDOW*" "*LINUX-X64-R14-OFFSET*" "*PARK-SAFEPOINTS*"))
      (import (intern n mvm) asm))
    ;; the cross-compiling images (aarch64 CLI, self-host) configure the x86-64
    ;; translator from text read in MODUS.MVM
    (dolist (n '("*X64-TLS-WINDOW*" "*X64-NATIVE-CODE-OFFSET*" "*X64-LINUX-MODE*"
                 "*X64-JIT-CONSTVEC-P*" "*X64-JIT-CONSTVEC-FULL-P*" "*X64-GC-ENABLED*"
                 "*WS5-FORCE-NO-KINDCHECK*" "*JIT-XLATE-ERR-INFO*"))
      (import (find-symbol n x64) mvm))
    ;; mvm-eval (MODUS.MVM) reads the translator's relocation lists and state
    (dolist (n '("*X64-FN-ADDR-RELOCS*" "*X64-CALL-RELOCS*" "*X64-LI-CONST-PATCHES*"
                 "*X64-JIT-MODE*" "*X64-CALL-RELOC-NARGS*" "*X64-LAST-GC-TRAMP-LABEL*"
                 "*X64-JIT-SHARED-GC-TRAMP*"))
      (import (find-symbol n x64) mvm))))

;; Cross-compilation pipeline
#+sbcl (mvm-load "mvm/cross.lisp")

;; HOST-ONLY build-time sanity checks (never baked into an image).  Installs
;; an encapsulation around BUILD-IMAGE that audits the assembled blob for
;; globals whose initialisation never runs in that particular image —
;; CLAUDE.md Active Limitation #7.  Must load AFTER cross.lisp, which defines
;; BUILD-IMAGE.  See mvm/build-checks.lisp.
#+sbcl (mvm-load "mvm/build-checks.lisp")

;; HOST-ONLY: the fixed-address registry (low-block words, window slots,
;; per-port extension words) and CHECK H -- two features claiming the same
;; address fail the build here, naming both owners.  See
;; mvm/fixed-addresses.lisp and docs/fixed-addresses.md.
#+sbcl (mvm-load "mvm/fixed-addresses.lisp")

;; HOST-ONLY: MODUS_A64_NO_FRAME_RECORDS=1 builds AArch64 code without frame
;; records (translate-aarch64.lisp, A64-FRAME-RECORDS-P), for A/B comparison.
(let ((v (sb-ext:posix-getenv "MODUS_A64_NO_FRAME_RECORDS")))
  (when (and v (plusp (length v)))
    (setf (symbol-value (intern "*A64-NO-FRAME-RECORDS*" :modus.mvm)) t)
    (format t ";; MODUS_A64_NO_FRAME_RECORDS: no AArch64 frame records~%")))

;; HOST-ONLY knobs for the AOT special-variable cell cache (compiler.lisp,
;; %COMPILE-GLOBAL-READ-AOT).  Read here rather than in compiler.lisp because
;; that file is also image source and must never call POSIX-GETENV.
;;   (the cache is ON by default -- docstring of *GV-CACHE-ENABLED*)
;;   MODUS_GVCACHE=1        force on (the default)
;;   MODUS_NO_GVCACHE=1     force off, even if MODUS_GVCACHE is set
;;   MODUS_GVCACHE_LO/HI=N  triage: only slots in [LO,HI) are emitted cached
;;   MODUS_GVCACHE_ONLY=A,B triage: only these global names are cached
;;   MODUS_GVCACHE_DUMP=F   triage: write the slot -> name table to F
;;   MODUS_GVCACHE_MODE=call  triage: emit only the fill call, no inline load
(let ((on  (sb-ext:posix-getenv "MODUS_GVCACHE"))
      (off (sb-ext:posix-getenv "MODUS_NO_GVCACHE"))
      (md  (sb-ext:posix-getenv "MODUS_GVCACHE_MODE")))
  (when (and on (plusp (length on)) (not (string= on "0")))
    (setf (symbol-value (find-symbol "*GV-CACHE-ENABLED*" :modus.mvm)) t)
    (format t ";; MODUS_GVCACHE: AOT special reads use the cached cell~%"))
  (when (and off (plusp (length off)) (not (string= off "0")))
    (setf (symbol-value (find-symbol "*GV-CACHE-ENABLED*" :modus.mvm)) nil)
    (format t ";; MODUS_NO_GVCACHE: AOT special reads use the %GV-REF call~%"))
  (dolist (k '("MODUS_GVCACHE_LO" "MODUS_GVCACHE_HI"))
    (let ((v (sb-ext:posix-getenv k)))
      (when (and v (plusp (length v)))
        (setf (symbol-value (find-symbol (if (search "_LO" k) "*GV-CACHE-LO*" "*GV-CACHE-HI*") :modus.mvm))
              (parse-integer v))
        (format t ";; ~A=~A~%" k v))))
  (let ((o (sb-ext:posix-getenv "MODUS_GVCACHE_ONLY")))
    (when (and o (plusp (length o)))
      (setf (symbol-value (find-symbol "*GV-CACHE-ONLY*" :modus.mvm))
            (let ((r nil) (start 0))
              (loop for i = (position #\, o :start start)
                    do (push (subseq o start i) r)
                       (if i (setq start (1+ i)) (return (nreverse r))))))
      (format t ";; MODUS_GVCACHE_ONLY=~A~%" o)))
  (when (and md (plusp (length md)))
    (setf (symbol-value (find-symbol "*GV-CACHE-MODE*" :modus.mvm))
          (intern (string-upcase md) :keyword))
    (format t ";; MODUS_GVCACHE_MODE=~A~%" md)))

)  ; end with-compilation-unit

;;; ------------------------------------------------------------------
;;; Source-text helpers every build script uses.  Each script used to carry
;;; its own copy (eighteen of READ-FILE-TEXT, six of MVM-TEXT, all the same
;;; function).
;;; ------------------------------------------------------------------

(defun read-file-text (path)
  "Read entire file as a string."
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  "Read a first-party source file (relative to *MODUS-BASE*) as text,
   parse-checking it first: a missing close paren in %format-impl once hid
   behind the lenient mid-build reader for weeks.  The text is wrapped so
   its own (in-package ...) cannot leak into the next file of the
   concatenated build blob (#211, modus.mvm::*build-package-reset-text*)."
  (let ((path (modus-source relative-path)))
    (modus.mvm::check-parses path)
    (modus.mvm::%build-package-scoped-source (read-file-text path) relative-path)))

(defun %opcode-entries ()
  "The MVM opcode table as data, in its order: a list of (CODE NAME OPERANDS
   DESCRIPTION).  What the build generates an image's own table from.  On SBCL it is
   read out of MODUS.MVM's table; on a modus host, whose table is the running kernel's
   and whose OPCODE-INFO accessors are the kernel's, from mvm/mvm.lisp's DEFOPCODE
   forms (%HOST-BIND-ISA-CONSTANTS)."
  #+sbcl
  (let ((r nil))
    (maphash (lambda (code info)
               (push (list code (modus.mvm::opcode-info-name info)
                           (modus.mvm::opcode-info-operands info)
                           (modus.mvm::opcode-info-description info))
                     r))
             modus.mvm::*opcode-table*)
    (nreverse r))
  #-sbcl
  *host-opcode-entries*)

(defun host-build-image (&rest args)
  "BUILD-IMAGE as a build script calls it, on either host.  SBCL: the compiler's
   own BUILD-IMAGE, through mvm/build-checks.lisp's encapsulation (which fills
   the image-configuration placeholder).  A modus host -- the builder CLI,
   MODUS_BUILDER=1: the placeholder is filled here, and %BUILDER-BUILD-IMAGE
   (mvm/builder.lisp) runs the image's own BUILD-IMAGE with this image's
   runtime settings out of the way."
  #+sbcl (apply 'modus.mvm::build-image args)
  #-sbcl
  (let* ((args (copy-list args))
         (src (getf args :source-text)))
    (when (stringp src)
      (setf (getf args :source-text)
            (modus.image-config:finalize-image-configuration src (getf args :target))))
    (apply #'modus-int::%builder-build-image args)))
