;;;; mvm/selfhost-tooling.lisp -- the cross-build tooling a self-hosting image
;;;; carries as TEXT: target descriptors, the cross pipeline (build-image), the
;;;; boot descriptors and ELF/PE writers for Linux x64 / AArch64 / RV32 and bare
;;;; x64 / UEFI, the AArch64 and RV32 translators and their co-inits, and the
;;;; in-image target registration (%INIT-SELFHOST-TARGETS).
;;;;
;;;; HOST-SIDE.  Loaded by a build script after lib/load-mvm.lisp; it defines
;;;; string variables and bakes nothing itself.  Two images take it:
;;;;   mvm/build-modus-selfhost.lisp  modus-sh, after its own x64 translator;
;;;;   mvm/build-cli-common.lisp      the CLI with MODUS_BUILDER=1, after the
;;;;                                  JIT's x64 translator -- the builder CLI.
;;;; *SELFHOST-CROSS-TOOLING-SOURCE* is everything here, in baking order.
;;;; It needs the x64 translator (translate-mvm-to-x64) baked before it.

;;; --- target descriptors (target.lisp, modus.mvm) ---
(defvar *target-source* (mvm-text "mvm/target.lisp"))

;;; --- cross-compilation build pipeline (cross.lisp, modus.mvm) ---
;; Strip the host-only TEST tail: test-cross-compilation + write-kernel-image
;; use file I/O / matrix loops we don't need.  KEEP everything after them
;; (read-all-forms / read-all-forms-with-locations / check-parses /
;; compute-name-hash / compiled-module-to-mvm-module) — compile-source-to-module
;; needs read-all-forms-with-locations and compiled-module-to-mvm-module.
;; Strategy: cut out just the write-kernel-image + test-cross-compilation block
;; (from write-kernel-image up to read-all-forms) so the self-hosting helpers
;; below it survive.
(defvar *cross-source* (mvm-text "mvm/cross.lisp"))
(let ((cut-start (search "(defun write-kernel-image" *cross-source*))
      (cut-end   (search "(defun read-all-forms" *cross-source*)))
  (unless (and cut-start cut-end (< cut-start cut-end))
    (error "WS5-S1: could not locate cross.lisp write-kernel-image..read-all-forms cut"))
  (setf *cross-source*
        (concatenate 'string
                     (subseq *cross-source* 0 cut-start)
                     (subseq *cross-source* cut-end))))

;;; --- linux-x64 boot descriptor + ELF64 wrapper (boot-linux-x64.lisp) ---
;; get-boot-descriptor(:linux-x64) → linux-x64-boot-descriptor, which names
;; emit-linux-x64-entry + +linux-x64-load-addr+.  wrap-in-elf64-le is called
;; by assemble-kernel-image for the :linux-x64 elf-format.  All are here.
;; Strip the top-level eval-when assert block (it references
;; modus.mvm.x64::+mcgc-kindbitmap-delta+ and does host-side asserts —
;; a build-time-only layout check that would run at image boot otherwise).
(defvar *boot-linux-desc-source* (mvm-text "boot/boot-linux-x64.lisp"))
(let ((aw-start (search "(eval-when (:compile-toplevel" *boot-linux-desc-source*)))
  (when aw-start
    ;; Find the matching close of the eval-when form (balance parens).
    (let ((depth 0) (i aw-start) (end nil) (len (length *boot-linux-desc-source*)))
      (loop while (< i len) do
        (let ((ch (char *boot-linux-desc-source* i)))
          (cond ((char= ch #\() (incf depth))
                ((char= ch #\)) (decf depth)
                 (when (zerop depth) (setf end (1+ i)) (return)))))
        (incf i))
      (when end
        (setf *boot-linux-desc-source*
              (concatenate 'string
                           (subseq *boot-linux-desc-source* 0 aw-start)
                           (subseq *boot-linux-desc-source* end)))))))
;; boot-linux-x64's EMIT-BYTES (buf &rest bytes → mvm-emit-byte) collides by
;; name with x64-asm's EMIT-BYTES (code-buffer → emit-byte); under
;; last-defun-wins the boot one (baked later) would shadow x64-asm's and break
;; the translator's 549 (emit-bytes …) call sites.  Rename the boot copy (def +
;; its only caller, emit-linux-x64-entry) to %LINUX-BOOT-EMIT-BYTES so both
;; survive.  (Dead code either way at Stage 1, but keeps the translator sound
;; for the self-compile inventory.)
(labels ((repl-all (text needle replacement)
           (let ((out "") (pos 0))
             (loop
               (let ((p (search needle text :start2 pos)))
                 (if p
                     (progn
                       (setf out (concatenate 'string out
                                              (subseq text pos p) replacement))
                       (setf pos (+ p (length needle))))
                     (return (concatenate 'string out (subseq text pos)))))))))
  ;; Replace only whole-token occurrences: "(defun emit-bytes " and
  ;; "(emit-bytes ".  mvm-emit-byte / emit-le32 / emit-le64 are untouched.
  (setf *boot-linux-desc-source*
        (repl-all (repl-all *boot-linux-desc-source*
                            "(defun emit-bytes " "(defun %linux-boot-emit-bytes ")
                  "(emit-bytes " "(%linux-boot-emit-bytes ")))

;;; --- bare-metal / UEFI x64 boot emitters, for `--compile-uefi` (DDC) ---
;; boot/boot-x64.lisp (multiboot header, boot32, kernel64 entry, IDT/PIC/PIT,
;; per-CPU layout), boot/boot-uefi-x64.lisp (the UEFI entry stub, PE32+ wrap,
;; :uefi-x64-cl descriptor) and boot/boot-uefi-snp.lisp (SEV-SNP hooks).  Pure
;; byte emitters over mvm-buffer, same as boot-linux-x64; no name collides with
;; anything else baked here (checked 2026-09-24).  Order matters: snp defines
;; *x64-snp-mode*, which boot-x64's interrupt setup and the UEFI stub test.
(defvar *boot-x64-bare-files*
  ;; Bisect knob: MODUS_SH_BOOT_FILES = comma list of the files to bake
  ;; ("snp,x64,uefi" = all, "" = none — the pre-2026-09-24 image).
  (let ((v (sb-ext:posix-getenv "MODUS_SH_BOOT_FILES")))
    (if (null v) '("snp" "x64" "uefi")
        (let ((out nil) (start 0))
          (loop for i from 0 to (length v)
                do (when (or (= i (length v)) (char= (char v i) #\,))
                     (when (> i start) (push (subseq v start i) out))
                     (setq start (1+ i))))
          (nreverse out)))))
(defvar *boot-x64-bare-sources*
  (apply #'concatenate 'string
         (mapcar (lambda (k)
                   (concatenate 'string
                                (mvm-text (cond ((string= k "snp") "boot/boot-uefi-snp.lisp")
                                                ((string= k "x64") "boot/boot-x64.lisp")
                                                ((string= k "uefi") "boot/boot-uefi-x64.lisp")
                                                (t (error "MODUS_SH_BOOT_FILES: ~A" k))))
                                (string #\Newline)))
                 *boot-x64-bare-files*)))
(format t "~&;; modus-sh: baked bare/UEFI boot files: ~S~%" *boot-x64-bare-files*)

;;; ============================================================
;;; #210 RUNG 1: bake the AARCH64 build tooling into the SAME image.
;;;
;;; Goal: a HOSTED x64 Modus image that emits a runnable Linux/AArch64 ELF
;;; from source baked into itself — no SBCL in the loop for the foreign emit.
;;; This replaces mvm/build-fixpoint.lisp's approach (a single multi-arch
;;; binary selected by an *override-fns* runtime dispatch, which broke when
;;; the image outgrew its layout) with the expected shape: boot a Modus
;;; image → compile the foreign architecture's translator from embedded
;;; source → emit a foreign image.
;;;
;;; Why x64 → aarch64 and not x64 → i386: +FRAME-SLOT-BASE+ is -96 in
;;; translate-x64.lisp and -68 in translate-i386.lisp and BOTH are bare-named,
;;; so an i386+x64 co-bake collides under the flat image namespace.  The
;;; AArch64 translator prefixes its own (+A64-FRAME-SLOT-BASE+), and a
;;; name-by-name comparison of every toplevel DEFUN/DEFMACRO/DEFSTRUCT in
;;; translate-aarch64.lisp against translate-x64.lisp, x64-asm.lisp,
;;; boot-linux-x64.lisp and the rest of the baked image finds ZERO
;;; collisions — the x64 translator uses TRANSLATE-INSTRUCTION /
;;; TRANSLATE-FUNCTION where the aarch64 one uses TRANSLATE-MVM-INSN /
;;; TRANSLATE-MVM-FUNCTION.  So the co-bake needs no renaming at all.
;;; ============================================================

;;; --- MVM-bytecode → AArch64 translator (modus.mvm) ---
(defvar *translate-aa64-source* (mvm-text "mvm/translate-aarch64.lisp"))
;; Same code-buffer GC-corruption class as the x64/mvm-buffer shrinks above:
;; a64-buffer's `code` slot defaults to 16M (UNSIGNED-BYTE 32) slots (64 MB; as a
;; generic vector it was 128 MB) and A64-EMIT DOUBLES
;; it on overflow, and each grow's replace-across-GC can move the buffer out
;; from under a stale local.  Rung 1 emits a trivial program (a few hundred
;; instructions), so 1M entries is a single up-front alloc that NEVER grows.
(let ((needle "(code (make-array 16777216 :element-type '(unsigned-byte 32)))")
      (repl   "(code (make-array 1048576 :element-type '(unsigned-byte 32)))"))
  (let ((p (search needle *translate-aa64-source*)))
    (unless p
      (error "#210: could not find a64-buffer 16M code-array default to shrink"))
    (setf *translate-aa64-source*
          (concatenate 'string
                       (subseq *translate-aa64-source* 0 p) repl
                       (subseq *translate-aa64-source* (+ p (length needle)))))))
;; Truncate at install-aarch64-translator, exactly as the x64 bake does: it
;; mutates *TARGET-AARCH64*, whose DEFPARAMETER init-thunk does NOT run at
;; boot (CLAUDE.md limitation #7), so in-image it would (setf (target-... NIL)).
;; The target-slot wiring is reconstructed in *selfhost-target-coinit-source*.
;; Only the three a64-disassemble/count/size diagnostics follow it in the file.
(let ((marker "(defun install-aarch64-translator"))
  (let ((pos (search marker *translate-aa64-source*)))
    (unless pos
      (error "#210: could not find install-aarch64-translator strip marker"))
    (setf *translate-aa64-source*
          ;; #211: re-append the package reset the trim just cut off.
          (concatenate 'string (subseq *translate-aa64-source* 0 pos)
                       modus.mvm::*build-package-reset-text*
                     (modus.mvm::%source-file-trailer "mvm/translate-aarch64.lisp")))))

;;; --- The 4-function AArch64 encoder closure the Linux boot entry needs ---
;; EMIT-LINUX-AARCH64-ENTRY calls EMIT-AARCH64-U32 and EMIT-AARCH64-LOAD-IMM64,
;; which live in boot/boot-aarch64.lisp — a BARE-METAL boot file (MMU page
;; tables, fixpoint re-entry guard, PSCI/GIC setup) that has no business in a
;; hosted Linux image.  The transitive closure is exactly four tiny pure
;; encoders: u32 → a64-emit, load-imm64 → movz + movk.  Extract them BY NAME
;; from the real file (paren-balanced) rather than hand-copying, so an encoding
;; change upstream cannot silently desync this bake.
(defun %extract-toplevel-defuns (text names)
  "Return the concatenated source of the toplevel (defun NAME …) forms in TEXT
   whose names are in NAMES, in NAMES order.  Errors if any is missing."
  (let ((out ""))
    (dolist (name names out)
      (let* ((needle (concatenate 'string "(defun " name " "))
             (start (search needle text)))
        (unless start
          (error "#210: could not extract ~A from boot-aarch64.lisp" name))
        (let ((depth 0) (i start) (end nil) (len (length text)) (in-str nil))
          (loop while (< i len) do
            (let ((ch (char text i)))
              (cond ((and in-str (char= ch #\\)) (incf i))   ; skip escaped char
                    ((char= ch #\") (setf in-str (not in-str)))
                    (in-str)
                    ((char= ch #\;)                            ; line comment
                     (loop while (and (< i len) (char/= (char text i) #\Newline))
                           do (incf i)))
                    ((char= ch #\() (incf depth))
                    ((char= ch #\)) (decf depth)
                     (when (zerop depth) (setf end (1+ i)) (return)))))
            (incf i))
          (unless end (error "#210: unbalanced defun ~A" name))
          (setf out (concatenate 'string out (subseq text start end)
                                 (string #\Newline) (string #\Newline))))))))

(defvar *aa64-boot-encoder-source*
  (let ((text (let ((p (merge-pathnames "boot/boot-aarch64.lisp" *modus-base*)))
                (modus.mvm::check-parses p)
                (read-file-text p))))
    (modus.mvm::%build-package-scoped-source
     (concatenate 'string
                  ";;; #210: extracted from boot/boot-aarch64.lisp (see build script)."
                  (string #\Newline)
                  (%extract-toplevel-defuns
                   text '("emit-aarch64-u32" "emit-aarch64-movz"
                          "emit-aarch64-movk" "emit-aarch64-load-imm64"))))))

;;; --- AArch64 translator co-init ---
;; Same limitation-#7 problem the x64 co-init above solves: DEFVAR/DEFPARAMETER
;; init-thunks do NOT run at boot, so every translator table/knob reads NIL
;; unless an explicit SETQ puts it there.  Three classes here:
;;
;;   (a) *A64-VREG-TO-PHYS* (translate-aarch64.lisp:392) — a DEFPARAMETER whose
;;       init-thunk builds the vreg→phys map.  NIL in-image ⇒ A64-PHYS-REG's
;;       (length NIL) faults on the very first instruction.  Rebuilt below.
;;
;;   (b) *AARCH64-SERIAL-WIDTH* (:65) and *AARCH64-FN-ALIGN-OFFSET* (:140) are
;;       written `(defvar … 0)`.  The trap is that they do NOT read 0 — they
;;       read NIL, and *AARCH64-FN-ALIGN-OFFSET* feeds ARITHMETIC in the
;;       function-entry alignment loop, so leaving it is a latent NIL-arith
;;       fault (and would mis-align every function entry even if it survived).
;;
;;   (c) The Linux-mode knobs build-aarch64-linux.lisp sets HOST-side at its
;;       tail (:1410-1441).  Those setfs run in SBCL and do not persist into an
;;       image, so the in-image emitter must set them itself.
;;
;; NO SYMBOL-IDENTITY TRAP HERE.  The x64 co-init carries a pointed warning
;; that its *REGISTERS* replica must be read in :MODUS.ASM, because that table
;; is an ALIST KEYED BY QUOTED SYMBOLS (MODUS.ASM::RBP ≠ MODUS.MVM::RBP under
;; per-package interning, #211/CLHS 11.1.2) and REG-INFO's ASSOC silently
;; missed, killing the JIT with "Unknown-register RBP".  The AArch64 map is a
;; vector of RAW INTEGER register numbers (+A64-X19+ = 19) with no symbols in
;; it at all, so no cross-package identity can be wrong.  What still matters is
;; that the VARIABLE *A64-VREG-TO-PHYS* be the same symbol the translator
;; reads: translate-aarch64.lisp declares (in-package :modus.mvm) and this
;; string is read in :MODUS.MVM (the ambient package — *build-package-reset-text*
;; restores it after every file), so it is.  Verified by the emit working at
;; all: a mismatched replica leaves the real variable NIL and A64-PHYS-REG
;; faults immediately rather than degrading silently.
(defvar *aa64-translator-coinit-source* "
(defun %a64-target-emit-prologue (target buf) (a64-emit-prologue buf))
(defun %a64-target-emit-epilogue (target buf) (a64-emit-epilogue buf))
;; #210 diagnostic: emit N known words into a fresh a64-buffer, read them back
;; through a64-buffer-to-bytes, and report the FIRST index that fails to
;; round-trip.  This is compiled code (struct accessors resolve), unlike the
;; same expression typed at the REPL.  Run with a size below and above the
;; initial code-array capacity to isolate the array-grow path specifically.
(defun %a64-emit-roundtrip-check (n)
  (let ((buf (make-a64-buffer)) (cap0 0) (bad -1) (i 0))
    (setq cap0 (length (a64-buffer-code buf)))
    (loop
      (when (>= i n) (return nil))
      ;; Distinct per-index word, kept inside 32 bits.
      (a64-emit buf (logand (+ #xD5000000 i) #xFFFFFFFF))
      (setq i (+ i 1)))
    (let ((bytes (a64-buffer-to-bytes buf)))
      (setq i 0)
      (loop
        (when (>= i n) (return nil))
        (let ((want (logand (+ #xD5000000 i) #xFFFFFFFF))
              (got (+ (aref bytes (* i 4))
                      (* 256 (aref bytes (+ (* i 4) 1)))
                      (* 65536 (aref bytes (+ (* i 4) 2)))
                      (* 16777216 (aref bytes (+ (* i 4) 3))))))
          (when (and (< bad 0) (not (= want got))) (setq bad i)))
        (setq i (+ i 1)))
      (write-string-serial \"a64-roundtrip n=\") (print-dec n)
      (write-string-serial \" cap0=\") (print-dec cap0)
      (write-string-serial \" endpos=\") (print-dec (a64-buffer-position buf))
      (write-string-serial \" endcap=\") (print-dec (length (a64-buffer-code buf)))
      (write-string-serial \" bytelen=\") (print-dec (length bytes))
      (write-string-serial \" first-bad=\") (print-dec bad)
      (write-char-serial 10))
    bad))
(declaim (overridable %init-aarch64-translator))
(defun %init-aarch64-translator ()
  ;; (a) vreg -> AArch64 physical register number.  Mirrors the
  ;; *a64-vreg-to-phys* defparameter AND *target-aarch64*'s :reg-map:
  ;;   V0-V3 -> x0-x3, V4-V8 -> x19-x23, V9-V15 spill (NIL),
  ;;   VR(16) -> x0, VA(17) -> x24, VL(18) -> x25, VN(19) -> x26,
  ;;   VSP(20) -> sp(31), VFP(21) -> x29, VPC(22) unmapped (NIL).
  ;; The spill/unmapped slots MUST be NIL, not 0: make-array zero-inits to
  ;; FIXNUM 0 (GC safety) and 0 is a VALID register number (x0), so a left-
  ;; zero slot would silently alias x0 instead of spilling.  Same class as the
  ;; x64 co-init's explicit (aset v 9 nil) block.
  (let ((v (make-array 23)))
    (aset v 0 0)   (aset v 1 1)   (aset v 2 2)   (aset v 3 3)
    (aset v 4 19)  (aset v 5 20)  (aset v 6 21)  (aset v 7 22)
    (aset v 8 23)
    (aset v 9 6)  (aset v 10 7) (aset v 11 4) (aset v 12 5)
    (aset v 13 8) (aset v 14 nil) (aset v 15 nil)
    (aset v 16 0)  (aset v 17 24) (aset v 18 25) (aset v 19 26)
    (aset v 20 31) (aset v 21 29) (aset v 22 nil)
    (setq *a64-vreg-to-phys* v)
  (setq *a64-x18-base* t))   ; x18 = convention base (translate-aarch64), a defvar whose init never runs in-image
  ;; (b) `(defvar … 0)` knobs that actually read NIL in-image.
  (setq *aarch64-serial-width* 0)
  ;; (c) Linux/AArch64 emitter config — the in-image equivalent of
  ;; build-aarch64-linux.lisp:1410-1441.
  (setq *aarch64-linux-mode* t)
  ;; Linux EL0 SP-alignment (SCTLR.SA0) demands 16-byte aligned SP at every
  ;; SP-based load/store; switch :push/:pop to the 16-byte aligned form.
  (setq *aarch64-stack-align-16* t)
  ;; The ELF wrap prepends 120 bytes of ehdr+phdr before the LOAD payload; the
  ;; function-entry alignment loop must account for it so runtime VAs land on
  ;; 16-byte boundaries and the OR-3 fn-pointer tag stays clean.
  (setq *aarch64-fn-align-offset* 120)
  ;; RUNG 1 emits a trivial, non-allocating program, so run it with the GC OFF
  ;; (x25 = alloc limit = full heap end) — the smallest correct configuration.
  ;; Turning the native MCGC on (metadata-shl / midpoint r25 / bitmap /
  ;; native-mcgc / absolute in-module calls, per build-aarch64-linux.lisp) is
  ;; rung 2's job, together with a real runtime payload to exercise it.
  (setq *linux-aarch64-r25-offset* #x38000000)   ; +linux-aarch64-heap-size+
  ;; SECOND NIL-ARITHMETIC TRAP, same class as *aarch64-fn-align-offset*:
  ;; *linux-aarch64-gc-midpoint* is `(defvar … +linux-aarch64-gc-midpoint+)`,
  ;; so it reads NIL in-image — and emit-linux-aarch64-entry does REAL
  ;; ARITHMETIC on it (boot-linux-aarch64.lisp:312,
  ;; `(- *linux-aarch64-gc-midpoint* +linux-aarch64-heap-alloc-start+)`) while
  ;; emitting the boot preamble's GC-metadata block, which is emitted
  ;; UNCONDITIONALLY — i.e. even with the collector off.  Left NIL the very
  ;; first cross-arch emit faults inside the boot stub.  #x1C000000 is the
  ;; +linux-aarch64-gc-midpoint+ constant, i.e. exactly what the SBCL-side
  ;; build gets from the init-thunk it actually runs.
  (setq *linux-aarch64-gc-midpoint* #x1C000000)
  (setq *linux-aarch64-gc-metadata-shl* nil)
  (setq *aarch64-gc-bitmap-enabled* nil)
  (setq *aarch64-gc-native-mcgc* nil)
  (setq *aarch64-force-absolute-inmodule-calls* nil)
  ;; Bare-metal-only hooks that must stay off in a Linux image.
  (setq *aarch64-sched-lock-addr* nil)
  (setq *aarch64-setup-irq-enable* nil)
  (setq *aarch64-jit-mode* nil)
  (setq *aarch64-translate-into-buf* nil)
  t)
")

;;; --- #210 RUNG-1 BLOCKER: cross.lisp's :into-buf special never binds ------
;; THE bug that made the first cross-arch emit produce a SIGILL ELF.
;;
;; translate-module-to-native hands the unified a64-buffer to the translator
;; through a DYNAMIC BINDING:
;;
;;   (let ((translator (target-translate-fn target))
;;         (modus.mvm::*aarch64-translate-into-buf* into-buf))
;;     … (funcall translator …) …)
;;
;; In-image that binding DOES NOT REACH THE CALLEE.  The image's compiler
;; treats a LET of a special as an ordinary lexical binding, so the global
;; keeps its old value for anything called from the body.  Demonstrated
;; directly in the built image:
;;
;;   (let ((*aarch64-translate-into-buf* 42)) (%rd))   =>  NIL   (want 42)
;;
;; Consequence: translate-mvm-to-aarch64 sees NIL, decides it is NOT appending,
;; allocates a FRESH buffer, translates the whole module into it — correctly —
;; and returns it, while assemble-kernel-image goes on to slice the UNIFIED
;; buffer, which only ever received the boot preamble.  The emitted ELF gets a
;; byte-perfect 94-instruction boot stub, a correct symbol table, and ZERO
;; bytes of function code.  Execution falls off the preamble into the constant
;; pool → SIGILL.
;;
;; The tell that identifies this precisely (rather than "code went missing"):
;; KERNEL-MAIN's recorded native offset was 8 — exactly the 2 alignment NOPs a
;; translation starting at buffer index 0 emits under
;; *aarch64-fn-align-offset* = 120 ((0*4+120) mod 16 = 8 → pad; 124 mod 16 = 12
;; → pad; 128 mod 16 = 0 → stop).  The unified path starts at index 95 and
;; would have recorded 12 (3 NOPs), which is what the SBCL reference emits.
;; A fresh-buffer translation is the only thing that yields 8.
;;
;; Fix, applied to the BAKED COPY ONLY so no shared file changes semantics:
;; assign the global with SETQ instead of LET-binding it.  Every call site sets
;; it unconditionally (nil for non-AArch64 targets, which never read it), so
;; dropping the restore is safe here.  Fixing this in cross.lisp itself would
;; be the real repair, but cross.lisp is on the x64 ANSI-gate path and this
;; rung does not need to touch it.
(let ((needle "  (let ((translator (target-translate-fn target))
        (modus.mvm::*aarch64-translate-into-buf* into-buf))")
      (repl   "  (let ((translator (target-translate-fn target)))
    (setq modus.mvm::*aarch64-translate-into-buf* into-buf)"))
  (let ((p (search needle *cross-source*)))
    (unless p
      (error "#210: could not find translate-module-to-native :into-buf LET"))
    (setf *cross-source*
          (concatenate 'string
                       (subseq *cross-source* 0 p) repl
                       (subseq *cross-source* (+ p (length needle)))))))

;;; --- linux-aarch64 boot descriptor + ELF64-LE(EM_AARCH64) wrapper ---
;; get-boot-descriptor(:linux-aarch64) → linux-aarch64-boot-descriptor, which
;; names emit-linux-aarch64-entry + +linux-aarch64-load-addr+;
;; wrap-in-elf64-le-aa64 is called by assemble-kernel-image for that elf-format.
;; No name collides with boot-linux-x64.lisp EXCEPT %SANITIZE-SYMBOL-NAME, and
;; this file only defines that one under an (unless (fboundp …)) guard — a
;; toplevel conditional wrapping a DEFUN, which is precisely the IR-drop bug
;; class (nested defun compiles but its IR is dropped; the name links to a
;; zero-length stub).  boot-linux-x64.lisp is baked BEFORE this and defines
;; %SANITIZE-SYMBOL-NAME unconditionally, so strip the guarded copy entirely.
;; NOTE: unlike boot-linux-x64.lisp this file has no EMIT-BYTES of its own
;; (it emits through emit-aarch64-u32 / mvm-emit-*), so no rename is needed.
(defvar *boot-linux-aa64-desc-source* (mvm-text "boot/boot-linux-aarch64.lisp"))
(let ((start (search "(unless (fboundp '%sanitize-symbol-name)"
                     *boot-linux-aa64-desc-source*)))
  (unless start
    (error "#210: could not find the %sanitize-symbol-name fboundp guard"))
  (let ((depth 0) (i start) (end nil)
        (len (length *boot-linux-aa64-desc-source*)) (in-str nil))
    (loop while (< i len) do
      (let ((ch (char *boot-linux-aa64-desc-source* i)))
        (cond ((and in-str (char= ch #\\)) (incf i))
              ((char= ch #\") (setf in-str (not in-str)))
              (in-str)
              ((char= ch #\() (incf depth))
              ((char= ch #\)) (decf depth)
               (when (zerop depth) (setf end (1+ i)) (return)))))
      (incf i))
    (unless end (error "#210: unbalanced fboundp guard block"))
    (setf *boot-linux-aa64-desc-source*
          (concatenate 'string
                       (subseq *boot-linux-aa64-desc-source* 0 start)
                       (subseq *boot-linux-aa64-desc-source* end)))))

;;; --- W32 M1: the RV32 translator, for `--compile-riscv32` (the RV32 DDC) ---
;; The third DDC: a self-hosted x64 compiler emitting a 30-BIT image.  The
;; UEFI and AArch64 DDCs are both 64-bit little-endian, so neither can see a
;; compiler decision taken by the image's OWN width (docs/w32.md, M1); this
;; one can.  translate-riscv.lisp is truncated at install-riscv-translator for
;; the aarch64 bake's reason (it SETFs a target struct whose defparameter
;; init-thunk does not run in-image); the slot wiring is in
;; *selfhost-target-coinit-source*.  Every name it defines is rv-/riscv-
;; prefixed apart from TRANSLATE-MVM-TO-RISCV / TRANSLATE-MVM-INSN-RISCV, and
;; neither collides (checked against every file baked here).
(defvar *translate-rv-source* (mvm-text "mvm/translate-riscv.lisp"))
(let ((marker "(defun install-riscv-translator"))
  (let ((pos (search marker *translate-rv-source*)))
    (unless pos
      (error "W32 M1: could not find the install-riscv-translator strip marker"))
    (setf *translate-rv-source*
          (concatenate 'string (subseq *translate-rv-source* 0 pos)
                       modus.mvm::*build-package-reset-text*
                       (modus.mvm::%source-file-trailer "mvm/translate-riscv.lisp")))))
;; THE BOOT CONSTANTS THE TRANSLATOR LOOKS UP BY NAME.  RV-HOSTED-HEAP-GEOMETRY
;; and RV-HOSTED-ROOT-BLOCK-END read +LINUX-RISCV32-HEAP-ADDR+ & co. with
;; FIND-SYMBOL + BOUNDP + SYMBOL-VALUE, because on the host the translator is
;; loaded before the boot files.  In an image a DEFCONSTANT is a folded literal
;; with no symbol value, so both lookups fail and the translator gives up
;; ("boot file not loaded"; measured: native 0, a 3.5 MB image of constant pool
;; only).  In the BAKED copy only, substitute the values -- read here from the
;; boot file's own DEFCONSTANT forms, so they cannot drift from it.  The baked
;; copy builds RV32 only; it refuses RV64 rather than guess.
(defun %w32-boot-constant (rel name)
  (with-open-file (s (cl-user::modus-source rel))
    (let ((*package* (find-package :modus.mvm)) (*read-eval* nil))
      (loop for form = (read s nil s)
            until (eq form s)
            when (and (consp form) (eq (car form) 'defconstant)
                      (string= (symbol-name (cadr form)) name))
              do (if (integerp (caddr form))
                     (return-from %w32-boot-constant (caddr form))
                     (error "W32 M1: ~A in ~A is not a literal integer" name rel))))
    (error "W32 M1: no DEFCONSTANT ~A in ~A" name rel)))
(flet ((swap (needle repl)
         (let ((p (search needle *translate-rv-source*)))
           (unless p (error "W32 M1: translate-riscv.lisp has moved: ~S" needle))
           (setf *translate-rv-source*
                 (concatenate 'string (subseq *translate-rv-source* 0 p) repl
                              (subseq *translate-rv-source* (+ p (length needle))))))))
  (let ((f "boot/boot-linux-riscv32.lisp"))
    (swap "(values (c \"+LINUX-RISCV32-HEAP-ADDR+\") (c \"+LINUX-RISCV32-HEAP-SIZE+\")
                (c \"+LINUX-RISCV32-GC-GUARD+\"))"
          (format nil "(values ~D ~D ~D)"
                  (%w32-boot-constant f "+LINUX-RISCV32-HEAP-ADDR+")
                  (%w32-boot-constant f "+LINUX-RISCV32-HEAP-SIZE+")
                  (%w32-boot-constant f "+LINUX-RISCV32-GC-GUARD+")))
    (swap "(values (c \"+LINUX-RISCV-HEAP-ADDR+\") (c \"+LINUX-RISCV-HEAP-SIZE+\")
                (c \"+LINUX-RISCV-GC-GUARD+\"))"
          "(error \"baked RISC-V translator: RV32 only\")")
    (swap "  (let ((sym (if *riscv-64-bit*
                 (find-symbol \"+LINUX-RISCV-HEAP-ALLOC-START+\" :modus.mvm)
                 (find-symbol \"+LINUX-RISCV32-HEAP-ALLOC-START+\" :modus.mvm))))
    (unless (and sym (boundp sym))
      (error \"hosted RISC-V collector: the ~:[RV32~;RV64~] boot file is not ~
              loaded, so the root block's end is unknown\" *riscv-64-bit*))
    (+ +rv-hosted-convention-base+ (symbol-value sym))))"
          (format nil "  (if *riscv-64-bit*
      (error \"baked RISC-V translator: RV32 only\")
      (+ +rv-hosted-convention-base+ ~D)))"
                  (%w32-boot-constant f "+LINUX-RISCV32-HEAP-ALLOC-START+")))))

;; RV32's boot descriptor, and the ELF32-LE writer it calls
;; (WRAP-IN-ELF32-LE-I386, generic apart from its name) -- in
;; build-riscv32-cli.lisp's order; boot-linux-aarch64.lisp is baked already.
(defvar *boot-linux-i386-desc-source* (mvm-text "boot/boot-linux-i386.lisp"))
(defvar *boot-linux-rv32-desc-source* (mvm-text "boot/boot-linux-riscv32.lisp"))

;; THE CO-INIT.  The defvar/defparameter init-thunks of the files above do not
;; run in-image, so each knob translation reads is set here to the value SBCL
;; holds after build-riscv32-cli.lisp's (setf *riscv-unimpl-ops* nil)
;; (install-riscv32-translator) (riscv-set-linux-mode t).  test/ddc-riscv32-host.lisp
;; does exactly that on the SBCL side.  Order matters: RISCV-SET-LINUX-MODE
;; reads *RV-HOSTED-GLOBALS-BASE*, so that is set first.
(defvar *rv32-translator-coinit-source* "
(defun %init-riscv32-translator ()
  (setq *riscv-reg-map*
        (vector +rv-a0+ +rv-a1+ +rv-a2+ +rv-a3+
                +rv-s11+ +rv-s1+ +rv-s2+ +rv-s3+
                +rv-s4+ +rv-s5+ +rv-s6+ +rv-s7+
                nil nil nil nil
                +rv-a0+ +rv-s8+ +rv-s9+ +rv-s10+ +rv-sp+ +rv-fp+ nil))
  (setq *riscv-64-bit* nil)
  (setq *riscv-record-unimpl* t)
  (setq *riscv-unimpl-ops* nil)
  (setq *riscv-li-const-patches* nil)
  (setq *rv-hosted-globals-base* #x10000380)
  (setq *rv-locals-bytes* +rv-local-frame-size+)
  (setq *rv-last-cmp-rs1* +rv-t3+)
  (setq *rv-last-cmp-rs2* +rv-t4+)
  (setq *riscv-genarith-offsets* nil)
  (setq *rv-gc-collector-offset* nil)
  (setq *rv-fn-starts* nil)
  (setq *linux-i386-vl-offset* (- #x10000000 #x1000000))
  (riscv-set-linux-mode t)
  t)
")

;; The whole self-host tooling block, spliced into *full-source* after mvm-eval.
;; Order (from the task): x64-asm → translate-x64 → translator-coinit → target
;; → cross → boot-desc.  The target/translator target-slot coinit
;; (%init-selfhost-targets) is a generated defun appended last.
(defvar *selfhost-target-coinit-source* "
(defun %init-selfhost-targets ()
  ;; Rebuild *target-x86-64* (its defparameter init-thunk doesn't run at boot)
  ;; and register it so (find-target :x86-64) resolves.  Then wire the x64
  ;; translator fns into its slots (replicating install-x64-translator).
  (setq *targets* (make-hash-table :test (quote eq)))
  (setq *target-x86-64*
        (make-target
         :name :x86-64
         :word-size 8
         :endianness :little
         :reg-map (vector :rsi :rdi :r8 :r9
                          :rbx :rcx :rdx :r10
                          :r11 nil nil nil
                          nil nil nil nil
                          :rax :r12 :r14 :r15 :rsp :rbp nil)
         :n-phys-regs 16
         :callee-saved (list 4 21)
         :arg-regs (list 0 1 2 3)
         :scratch-regs (list 5 6 7 8)
         :max-inline-regs 8
         :page-size 4096
         :translate-fn nil
         :emit-prologue nil
         :emit-epilogue nil
         :emit-boot nil
         :float-support :native
         :features (list :has-io-ports t :has-lapic t :has-sipi t)))
  (setf (target-translate-fn  *target-x86-64*) (function translate-mvm-to-x64))
  (setf (target-emit-prologue *target-x86-64*) (function emit-function-prologue))
  (setf (target-emit-epilogue *target-x86-64*) (function emit-function-epilogue))
  (register-target *target-x86-64*)
  ;; #210 rung 1: same treatment for *target-aarch64* (target.lisp:134), so
  ;; (find-target :aarch64) — which build-image reaches via
  ;; target-base-arch(:linux-aarch64) → :aarch64 — resolves in-image.  The
  ;; slot wiring replicates install-aarch64-translator (translate-aarch64.lisp
  ;; :5306).  Values transcribed from the *target-aarch64* defparameter; note
  ;; :float-support is left at make-target's :none default, matching it.
  (setq *target-aarch64*
        (make-target
         :name :aarch64
         :word-size 8
         :endianness :little
         :reg-map (vector :x0 :x1 :x2 :x3
                          :x19 :x20 :x21 :x22
                          :x23 nil nil nil
                          nil nil nil nil
                          :x0 :x24 :x25 :x26 :sp :x29 nil)
         :n-phys-regs 31
         :callee-saved (list 4 5 6 7 8)
         :arg-regs (list 0 1 2 3)
         :scratch-regs (list 5 6 7 8)
         :max-inline-regs 8
         :page-size 4096
         :translate-fn nil
         :emit-prologue nil
         :emit-epilogue nil
         :emit-boot nil
         :features (list :has-gic t :has-psci t)))
  ;; install-aarch64-translator wraps the prologue/epilogue in LAMBDAs that
  ;; drop the TARGET argument.  Use named shims instead so nothing stores a
  ;; closure in a struct slot (matching the x64 branch's (function …) style).
  ;; cross.lisp only ever reads TARGET-TRANSLATE-FN, but set all three so the
  ;; descriptor is faithful.
  (setf (target-translate-fn  *target-aarch64*)
        (function translate-mvm-to-aarch64))
  (setf (target-emit-prologue *target-aarch64*)
        (function %a64-target-emit-prologue))
  (setf (target-emit-epilogue *target-aarch64*)
        (function %a64-target-emit-epilogue))
  (register-target *target-aarch64*)
  ;; W32 M1: *target-riscv32* (target.lisp), for --compile-riscv32.  Values
  ;; transcribed from its defparameter; the slot wiring replicates
  ;; install-riscv32-translator, which the bake truncates away.
  (setq *target-riscv32*
        (make-target
         :name :riscv32
         :word-size 4
         :endianness :little
         :reg-map (vector :a0 :a1 :a2 :a3
                          :s11 :s1 :s2 :s3
                          :s4 :s5 :s6 :s7
                          nil nil nil nil
                          :a0 :s8 :s9 :s10 :sp :fp nil)
         :n-phys-regs 32
         :callee-saved (list 4 5 6 7 8 9 10 11)
         :arg-regs (list 0 1 2 3)
         :scratch-regs (list 5 6 7 8)
         :max-inline-regs 11
         :page-size 4096
         :translate-fn nil
         :emit-prologue nil
         :emit-epilogue nil
         :emit-boot nil
         :features (list :32-bit t :has-sbi t :has-plic t)))
  (setf (target-translate-fn  *target-riscv32*) (function translate-mvm-to-riscv))
  (setf (target-emit-prologue *target-riscv32*) (function riscv-emit-prologue-fn))
  (setf (target-emit-epilogue *target-riscv32*) (function riscv-emit-epilogue-fn))
  (register-target *target-riscv32*)
  t)
")

;;; --- Windows x86-64: boot/boot-win64.lisp (PE32+ writer, :WIN64 descriptor) ---
(defvar *boot-win64-desc-source* (mvm-text "boot/boot-win64.lisp"))

(defvar *selfhost-cross-tooling-source*
  (concatenate 'string
    ;; #210 rung 1: the aarch64 translator + its co-init go here, BEFORE
    ;; target/cross/boot-desc, for the same reason the x64 pair does — the
    ;; translator uses modus.mvm compiler symbols and must precede the
    ;; sft-auto scan that registers translate-mvm-to-aarch64 et al.  The
    ;; boot-encoder shim must follow the translator (emit-aarch64-u32 calls
    ;; a64-emit) and precede the boot descriptor that calls it.
    *translate-aa64-source*         (string #\Newline)
    *aa64-translator-coinit-source* (string #\Newline)
    *aa64-boot-encoder-source*      (string #\Newline)
    ;; W32 M1: the RISC-V translator (RV32 arm) and its co-init, placed with
    ;; the other translators for the same sft-auto reason.
    *translate-rv-source*           (string #\Newline)
    *rv32-translator-coinit-source* (string #\Newline)
    *target-source*                 (string #\Newline)
    *cross-source*                  (string #\Newline)
    *boot-linux-desc-source*        (string #\Newline)
    *boot-linux-aa64-desc-source*   (string #\Newline)
    *boot-linux-i386-desc-source*   (string #\Newline)
    *boot-linux-rv32-desc-source*   (string #\Newline)
    *boot-x64-bare-sources*         (string #\Newline)
    ;; Windows: the PE32+ writer and the :WIN64 boot descriptor
    ;; (mvm/build-win64-cli.lisp), so a builder can build modus.exe
    *boot-win64-desc-source*        (string #\Newline)
    *selfhost-target-coinit-source* (string #\Newline)))
