;;;; build-fixpoint.lisp - Build the Fixpoint of Theseus
;;;;
;;;; The fixpoint proves the MVM compiler is a fixed point across
;;;; architectures: SBCL→Gen0(x64)→Gen2(aarch64)→Gen3(x64),
;;;; where SHA256(Gen3)==SHA256(Gen0).
;;;;
;;;; Architecture:
;;;;   - All source is compiled to MVM bytecode (architecture-independent)
;;;;   - The MVM bytecode is the "fixed point" — same source always produces
;;;;     the same bytecode regardless of which architecture does the compilation
;;;;   - Each generation translates the bytecode to native code for the target
;;;;     architecture using the embedded translator
;;;;
;;;; This build script:
;;;;   1. Loads the MVM system (compiler, translators, boot descriptors)
;;;;   2. Collects all fixpoint source (REPL + MVM system + translators +
;;;;      boot descriptors + cross pipeline + build-image-cross)
;;;;   3. Compiles all source through MVM → MVM bytecode
;;;;   4. Translates bytecode → x64 native code for Gen0
;;;;   5. Assembles Gen0 image with embedded bytecode + function table
;;;;   6. Writes Gen0 ELF to /tmp/fixpoint-gen0.elf
;;;;
;;;; Usage: sbcl --script mvm/build-fixpoint.lisp
;;;;   With SSH: sbcl --script mvm/build-fixpoint.lisp -- --ssh
;;;;
;;;; Boot:
;;;;   qemu-system-x86_64 -kernel /tmp/fixpoint-gen0.elf -m 512 -nographic -no-reboot

(format t "=== Fixpoint of Theseus ===~%")
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

;;; ============================================================
;;; Read all fixpoint source files
;;; ============================================================

(defun read-file-text (path)
  "Read entire file as a string."
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (let ((n (read-sequence text s)))
        (subseq text 0 n)))))

(defun mvm-text (relative-path)
  "Read a file relative to *modus-base* as text, parse-checking it first.
   A missing close paren in %format-impl once hid behind the lenient
   mid-build reader for weeks — see ansi-notes.md: SOLVED:
   late-cond-branch."
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    ;; #211: wrap each file so its own (in-package …) cannot leak into the
    ;; next file of the concatenated build blob.  See
    ;; modus.mvm::*build-package-reset-text*.
    (modus.mvm::%build-package-scoped-source (read-file-text path))))

(format t "Collecting fixpoint source...~%")

;;; The fixpoint source includes:
;;; 1. REPL (basic I/O, eval, print)
;;; 2. MVM prelude (library functions)
;;; 3. MVM ISA definitions (opcodes, structs)
;;; 4. Target descriptors
;;; 5. x64 assembler (code-buffer, labels, instruction emission)
;;; 6. x64 translator (MVM bytecode → x86-64 native)
;;; 7. AArch64 translator (MVM bytecode → AArch64 native)
;;; 8. Cross-compilation pipeline (translation, image assembly)
;;; 9. Boot descriptors (x64, AArch64)
;;; 10. build-image-cross (bare-metal cross-compilation entry point)
;;; 11. kernel-main

(defvar *mvm-source-files*
  '("mvm/prelude.lisp"
    "mvm/mvm.lisp"
    "mvm/target.lisp"
    "mvm/x64-asm.lisp"
    "mvm/translate-x64.lisp"
    "mvm/translate-aarch64.lisp"
    "mvm/translate-i386.lisp"
    "mvm/translate-arm32.lisp"
    "mvm/cross.lisp"
    "boot/boot-x64.lisp"
    "boot/boot-aarch64.lisp"
    "boot/boot-i386.lisp"
    "boot/boot-arm32.lisp"))

(defvar *mvm-source-text*
  (with-output-to-string (s)
    (dolist (file *mvm-source-files*)
      (let ((path (merge-pathnames file *modus-base*)))
        (format t "  ~A~%" file)
        ;; #211: this list is the one place a build concatenates first-party
        ;; files WITHOUT going through MVM-TEXT, and three of them declare their
        ;; own package (x64-asm = MODUS.ASM, translate-x64 = MODUS.MVM.X64,
        ;; translate-i386 = MODUS.MVM.I386).  The reader honours those now, so
        ;; wrap each one the same way MVM-TEXT does or the declaration leaks
        ;; into every file after it (translate-arm32, cross, the boot
        ;; descriptors, and the whole *fixpoint-source* tail).
        (write-string (modus.mvm::%build-package-scoped-source
                       (read-file-text path))
                      s)
        (terpri s)))))

;;; ============================================================
;;; Source preprocessing (SBCL-side)
;;; Fixes &key calls, macrolet, constants for bare-metal MVM compilation
;;; ============================================================

(mvm-load "mvm/fixpoint-preprocess.lisp")

;;; ============================================================
;;; Command-line flags
;;; ============================================================

(defvar *fixpoint-ssh-mode*
  (not (member "--no-ssh" sb-ext:*posix-argv* :test #'string=)))

(defvar *fixpoint-gen0-arch*
  (let ((pos (position "--arch" sb-ext:*posix-argv* :test #'string=)))
    (if (and pos (< (1+ pos) (length sb-ext:*posix-argv*)))
        (intern (string-upcase (nth (1+ pos) sb-ext:*posix-argv*)) :keyword)
        :x64))
  "Architecture for Gen0 image: :x64 (default) or :aarch64")

;;; ============================================================
;;; 32-bit crypto dispatch (SBCL-side)
;;; Defines *override-fns* and generates dispatch wrappers
;;; ============================================================

(mvm-load "mvm/fixpoint-dispatch.lisp")

;;; ============================================================
;;; Networking source (SBCL-side, conditional on SSH mode)
;;; Loads and preprocesses net/*.lisp for bare-metal
;;; ============================================================

(mvm-load "mvm/fixpoint-net-source.lisp")

;;; ============================================================
;;; Bare-metal extra source: translator overrides for fixpoint
;;; Each module is a plain Lisp file read as text and concatenated
;;; into *fixpoint-extra-source* for bare-metal compilation.
;;; ============================================================

;;; Bare-metal source module list — edit to enable/disable modules
(defvar *fixpoint-extra-modules*
  '("mvm/fixpoint-common.lisp"       ; metadata, error, registers, labels, x64 translation
    "mvm/fixpoint-cross.lisp"        ; image buffer, image assembly, build-image-cross dispatch
    "mvm/fixpoint-aarch64.lisp"      ; AArch64 translator overrides + image assembly
    "mvm/fixpoint-i386.lisp"         ; i386 translator overrides + HOST overrides
    "mvm/fixpoint-i386-x64.lisp"     ; i386-safe x64 cross-compilation
    "mvm/fixpoint-i386-aarch64.lisp" ; i386-safe AArch64 translator
    "mvm/fixpoint-arm32.lisp"        ; ARM32 translator + image assembly + HOST overrides
    ))

(defvar *fixpoint-extra-source*
  (with-output-to-string (s)
    (dolist (mod *fixpoint-extra-modules*)
      (format t "  ~A~%" mod)
      (write-string (mvm-text mod) s)
      (terpri s))))

;;; SSH driver dispatch + crypto fixes (conditional on SSH mode)
(when *fixpoint-ssh-mode*
  (format t "  mvm/fixpoint-ssh.lisp~%")
  (setf *fixpoint-extra-source*
        (concatenate 'string *fixpoint-extra-source*
                     (mvm-text "mvm/fixpoint-ssh.lisp"))))

(format t "Extra source: ~D chars (~D modules~A)~%"
        (length *fixpoint-extra-source*)
        (length *fixpoint-extra-modules*)
        (if *fixpoint-ssh-mode* " + SSH" ""))

;;; ============================================================
;;; Generate opcode table init source (in cl-user, before package switch)
;;; ============================================================

;; Generate init-opcode-entries from SBCL-side *opcode-table*
;; The defopcode calls in mvm.lisp are top-level side effects that compile
;; to TOPLEVEL-* thunks but are never called on bare metal. This function
;; explicitly registers all opcodes.
;; Must use cl:maphash here — the modus.mvm package shadows maphash.
(defvar *opcode-init-source*
  (let ((ot modus.mvm::*opcode-table*)
        (count 0))
    (with-output-to-string (s)
      (format s "(defun init-opcode-entries ()~%")
      (cl:maphash (lambda (code info)
                    (incf count)
                    ;; Progress marker every 10 entries
                    (when (zerop (mod count 10))
                      (format s "  (write-char-serial ~D)~%" (+ 48 (floor count 10))))
                    (let ((operands (modus.mvm::opcode-info-operands info)))
                      (format s "  (puthash ~D *opcode-table* (%make-opcode-info ~D ~D "
                              code code (modus.mvm::normalize-name (modus.mvm::opcode-info-name info)))
                      (if (null operands)
                          (format s "nil")
                          (progn
                            (loop for op in operands
                                  for first = t then nil
                                  do (unless first (format s " "))
                                     (format s "(cons ~D" (modus.mvm::normalize-name op)))
                            (format s " nil")
                            (dotimes (j (length operands))
                              (format s ")"))))
                      (format s " nil))~%")))
                  ot)
      ;; Final marker
      (format s "  (write-char-serial 33)~%")  ; '!'
      (format s ")~%"))))

(format t "Generated init-opcode-entries: ~D chars~%" (length *opcode-init-source*))
(format t "~A~%" (subseq *opcode-init-source* 0 (min 1500 (length *opcode-init-source*))))

;;; ============================================================
;;; Compile fixpoint source through MVM
;;; ============================================================

(in-package :modus.mvm)

;; Forward x64-asm bindings so defmacro bodies can access them at expansion time.
;; #211 note: x64-asm.lisp's source is now read in MODUS.ASM (its own
;; declaration) rather than MODUS.MVM, so its own references resolve to
;; MODUS.ASM::*REGISTERS* directly.  This host-side forward is kept for any
;; build-time expansion that still names the symbol unqualified in MODUS.MVM.
(defparameter *registers* modus.asm::*registers*)

;; Install translators
(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(install-aarch64-translator)
(funcall (intern "INSTALL-I386-TRANSLATOR" "MODUS.MVM.I386"))
(install-armv7-rpi-translator)

;; init-opcode-entries source was generated above (before in-package switch)
;; to avoid maphash compiler-macro conflict in modus.mvm package.
;; (See *opcode-init-source* defvar near top of this section)

;; (old maphash code removed — now generated in cl-user section above)

;; Combine all source: REPL first, then networking (if --ssh), then MVM system,
;; then fixpoint cross-compilation, then kernel-main LAST.
(defvar *fixpoint-source*
  (concatenate 'string
               ;; REPL source first (defines most runtime functions)
               *repl-source*
               (string #\Newline)
               ;; Networking source (only when --ssh)
               (or cl-user::*net-source-text* "")
               (string #\Newline)
               ;; MVM system source
               cl-user::*mvm-source-text*
               (string #\Newline)
               ;; Fixpoint cross-compilation functions
               cl-user::*fixpoint-extra-source*
               (string #\Newline)
               ;; Opcode table initialization
               cl-user::*opcode-init-source*
               (string #\Newline)
               ;; kernel-main LAST — "last-defun-wins" makes this the entry point.
               ;; The JMP in boot code targets the last kernel-main in the function table.
               ;; Must use write-char-serial (the MVM compiler builtin), not write-byte.
               "(defun kernel-main ()
  ;; Set up timer interrupts (PIC+PIT+IDT on x64/i386, NOP on aarch64/arm32).
  ;; Boot code also sets this up, but setup-irq ensures it works for all
  ;; generations regardless of boot path. Safe to call twice.
  (setup-irq)
  ;; Initialize MVM runtime tables
  (write-char-serial 73) (write-char-serial 49) (write-char-serial 10)
  (init-*gensym-counter*)
  (write-char-serial 73) (write-char-serial 50) (write-char-serial 10)
  (init-*vreg-names*)
  (write-char-serial 73) (write-char-serial 51) (write-char-serial 10)
  (init-*opcode-table*)
  (init-opcode-entries)
  (write-char-serial 73) (write-char-serial 52) (write-char-serial 10)
  (init-*mvm-label-counter*)
  (write-char-serial 73) (write-char-serial 53) (write-char-serial 10)
  (init-*target-x86-64*)
  (write-char-serial 73) (write-char-serial 54) (write-char-serial 10)
  (init-*condition-codes*)
  (write-char-serial 73) (write-char-serial 55) (write-char-serial 10)
  (init-*vreg-to-x64*)
  ;; Diagnostic: verify *vreg-to-x64* initialization
  (write-char-serial 73) (write-char-serial 56) (write-char-serial 10)
  (write-char-serial 88) ;; X
  (print-dec (length *vreg-to-x64*))
  (write-char-serial 32)
  ;; Print first 9 elements (V0-V8 = physical regs)
  (let ((vi 0))
    (loop
      (when (>= vi 9) (return nil))
      (let ((v (aref *vreg-to-x64* vi)))
        (if (null v)
            (write-char-serial 78) ;; N
            (progn (write-char-serial 89) ;; Y
                   (print-dec v)))
        (write-char-serial 44))
      (setq vi (+ vi 1))))
  (write-char-serial 10)
  ;; Initialize AArch64 translator tables
  (init-*a64-vreg-to-phys*)
  ;; Initialize i386 translator tables
  (init-*i386-vreg-map*)
  ;; Initialize arm32 translator tables
  (init-*arm32-vreg-map*)
  (write-char-serial 73) (write-char-serial 57) (write-char-serial 10)
  ;; Check mode: 0=cross-compile, 1=SSH server, 2=REPL
  (let ((mode (td-read-u32 #x500038)))
    (write-char-serial 77) (write-char-serial 61) ;; M=
    (print-dec mode) (write-char-serial 10)
    (cond
      ((= mode 1)
       ;; SSH server mode
       (ssh-kernel-main))
      ((= mode 2)
       ;; REPL-only mode (no networking)
       (repl (cons nil nil)))
      (t
       ;; Cross-compile mode: read target from metadata
       (let ((target (td-read-u32 #x500034)))
         (build-image-cross target)))))
  (write-char-serial 68) (write-char-serial 10)
  ;; Drop to REPL
  (let ((globals (cons nil nil)))
    (repl globals)))"))

(format t "~%Total fixpoint source: ~D chars~%" (length *fixpoint-source*))

;; Debug: check paren balance of each fixpoint source piece
(flet ((check-depth (src label)
         (let ((depth 0) (in-string nil) (in-comment nil) (escape nil))
           (dotimes (i (length src))
             (let ((ch (char src i)))
               (cond
                 (escape (setf escape nil))
                 ((char= ch #\Newline) (setf in-comment nil))
                 (in-comment nil)
                 (in-string (cond ((char= ch #\\) (setf escape t))
                                  ((char= ch #\") (setf in-string nil))))
                 ((char= ch #\;) (setf in-comment t))
                 ((char= ch #\") (setf in-string t))
                 ((char= ch #\() (incf depth))
                 ((char= ch #\)) (decf depth)))))
           (format t "  ~A: depth=~D (~D chars)~%" label depth (length src))
           depth)))
  (check-depth modus.mvm::*repl-source* "repl-source")
  (check-depth cl-user::*mvm-source-text* "mvm-source-text")
  (check-depth cl-user::*fixpoint-extra-source* "fixpoint-extra")
  (check-depth cl-user::*opcode-init-source* "opcode-init")
  (check-depth *fixpoint-source* "TOTAL"))

;; Phase 1: Compile to MVM bytecode (THE FIXED POINT)
(format t "~%Phase 1: Compiling fixpoint source → MVM bytecode...~%")
(let* ((module (compile-source-to-module *fixpoint-source* :name :fixpoint))
       (bytecode (mvm-module-bytecode module))
       (fn-table (mvm-module-function-table module)))
  (format t "  MVM bytecode: ~D bytes~%" (length bytecode))
  (format t "  Functions: ~D~%" (length fn-table))
  ;; XOR checksum for bare-metal comparison
  (let ((xsum 0))
    (dotimes (i (length bytecode))
      (setf xsum (logxor xsum (aref bytecode i))))
    (format t "  Bytecode XOR checksum: ~D~%" xsum))
  ;; Print function 13 info
  (let ((fi13 (nth 13 fn-table)))
    (format t "  Fn 13: ~A offset=~D length=~D~%"
            (mvm-function-info-name fi13)
            (mvm-function-info-bytecode-offset fi13)
            (mvm-function-info-bytecode-length fi13)))
  ;; Print function 1751 info (hangs during x64 cross-compile from non-x64 hosts)
  (when (> (length fn-table) 1751)
    (let ((fi1751 (nth 1751 fn-table)))
      (format t "  Fn 1751: ~A offset=~D length=~D~%"
              (mvm-function-info-name fi1751)
              (mvm-function-info-bytecode-offset fi1751)
              (mvm-function-info-bytecode-length fi1751))))
  ;; Print functions 1745-1760 for context
  (format t "  Functions 1745-1760:~%")
  (loop for i from 1745 to (min 1760 (1- (length fn-table)))
        for fi = (nth i fn-table)
        do (format t "    ~D: ~A (offset=~D len=~D)~%"
                   i (mvm-function-info-name fi)
                   (mvm-function-info-bytecode-offset fi)
                   (mvm-function-info-bytecode-length fi)))
  ;; Top 20 functions by bytecode size (with index)
  (let ((indexed nil))
    (loop for fi in fn-table for i from 0
          do (push (list i fi) indexed))
    (setf indexed (sort indexed
                        (lambda (a b)
                          (> (mvm-function-info-bytecode-length (second a))
                             (mvm-function-info-bytecode-length (second b))))))
    (format t "  Top 20 functions by size:~%")
    (loop for entry in indexed
          for j from 0 below 20
          do (let ((i (first entry)) (fi (second entry)))
               (format t "    [~D] ~A: ~D bytes (offset=~D)~%"
                       i (mvm-function-info-name fi)
                       (mvm-function-info-bytecode-length fi)
                       (mvm-function-info-bytecode-offset fi)))))
  ;; Check for emit-add-reg-reg and related functions
  (dolist (fi fn-table)
    (let ((name (string (mvm-function-info-name fi))))
      (when (or (search "ADD-REG" name)
                (search "SUB-REG" name)
                (search "AND-REG" name)
                (search "OR-REG" name)
                (search "XOR-REG" name))
        (format t "  FOUND: ~A offset=~D~%" name (mvm-function-info-bytecode-offset fi)))))

  ;; Count opcodes that create internal labels
  (format t "~%Opcode census:~%")
  (let ((n-consp 0) (n-atom 0) (n-gc 0) (n-tailcall 0) (n-call 0) (n-total 0)
        (n-call0 0) (n-fnaddr 0) (n-fnaddr0 0)
        (pos 0))
    (loop while (< pos (length bytecode))
          do (let ((decoded (modus.mvm::decode-instruction bytecode pos)))
               (incf n-total)
               (let ((opcode (car decoded))
                     (operands (cadr decoded)))
                 (case opcode
                   (#x55 (incf n-consp))
                   (#x56 (incf n-atom))
                   (#x89 (incf n-gc))
                   (#x83 (incf n-tailcall))
                   (#x80 (incf n-call)
                         (when (zerop (first operands))
                           (incf n-call0)))
                   (#xA7 (incf n-fnaddr)
                         (when (zerop (second operands))
                           (incf n-fnaddr0)))))
               (setf pos (cddr decoded))))
    (format t "  CONSP(#x55): ~D -> ~D labels~%" n-consp (* 2 n-consp))
    (format t "  ATOM(#x56): ~D -> ~D labels~%" n-atom (* 2 n-atom))
    (format t "  GC-CHECK(#x89): ~D -> ~D labels~%" n-gc n-gc)
    (format t "  TAILCALL(#x83): ~D~%" n-tailcall)
    (format t "  CALL(#x80): ~D (target=0: ~D)~%" n-call n-call0)
    (format t "  FN-ADDR(#xA7): ~D (target=0: ~D)~%" n-fnaddr n-fnaddr0)
    (format t "  Total internal labels: ~D~%" (+ (* 2 n-consp) (* 2 n-atom) n-gc))
    (format t "  Total instructions: ~D~%~%" n-total))

  ;; Verify: translate bytecode on SBCL side (reference native code size)
  (format t "~%Reference translation (SBCL side)...~%")
  (let ((fn-table-for-translator
         (mapcar (lambda (fi)
                   (list (string (mvm-function-info-name fi))
                         (mvm-function-info-bytecode-offset fi)
                         (mvm-function-info-bytecode-length fi)))
                 fn-table)))
    (multiple-value-bind (ref-buf ref-fn-map)
        (modus.mvm.x64::translate-mvm-to-x64 bytecode fn-table-for-translator)
      (let ((ref-size (modus.asm:code-buffer-position ref-buf))
            (ref-bytes (modus.asm:code-buffer-bytes ref-buf)))
        (format t "  SBCL-side native code: ~D bytes~%" ref-size)
        ;; Pre-fixup FNV: zero fixup positions, compute FNV, restore
        (let ((fixups (modus.asm::code-buffer-fixups ref-buf))
              (saved nil))
          ;; Save and zero fixup bytes
          (dolist (fixup fixups)
            (destructuring-bind (pos label size) fixup
              (declare (ignore label))
              (when (eql size 4)
                (push (list pos (aref ref-bytes pos) (aref ref-bytes (+ pos 1))
                            (aref ref-bytes (+ pos 2)) (aref ref-bytes (+ pos 3)))
                      saved)
                (setf (aref ref-bytes pos) 0
                      (aref ref-bytes (+ pos 1)) 0
                      (aref ref-bytes (+ pos 2)) 0
                      (aref ref-bytes (+ pos 3)) 0))))
          ;; Restore fixup bytes
          (dolist (entry saved)
            (destructuring-bind (pos b0 b1 b2 b3) entry
              (setf (aref ref-bytes pos) b0
                    (aref ref-bytes (+ pos 1)) b1
                    (aref ref-bytes (+ pos 2)) b2
                    (aref ref-bytes (+ pos 3)) b3))))
        ;; Post-fixup FNV-1a checksum of native code
        (let ((hash 2166136261))
          (dotimes (i ref-size)
            (setf hash (logand #xFFFFFFFF
                               (* (logxor hash (aref ref-bytes i)) 16777619))))
          (format t "  SBCL-side x64 FNV-1a: ~D~%" hash)))))

  ;; AArch64 reference translation (SBCL side)
  (format t "~%AArch64 reference translation (SBCL side)...~%")
  (let ((*aarch64-serial-base* #x20000000)
        (*aarch64-serial-width* 0)
        (*aarch64-serial-tx-poll* nil)
        (*aarch64-sched-lock-addr* nil))
    ;; Build hash table: func-idx → mvm-byte-offset (AArch64 translator format)
    (let ((a64-fn-ht (make-hash-table :test 'eql)))
      (loop for fi in fn-table
            for i from 0
            do (setf (gethash i a64-fn-ht)
                     (mvm-function-info-bytecode-offset fi)))
      (let ((a64-buf (translate-mvm-to-aarch64 bytecode a64-fn-ht)))
        (let* ((a64-bytes (a64-buffer-to-bytes a64-buf))
               (a64-size (length a64-bytes)))
          (format t "  SBCL-side AArch64 native: ~D bytes~%" a64-size)
          ;; FNV-1a checksum
          (let ((hash 2166136261))
            (dotimes (i a64-size)
              (setf hash (logand #xFFFFFFFF
                                 (* (logxor hash (aref a64-bytes i)) 16777619))))
            (format t "  SBCL-side AArch64 FNV-1a: ~D~%" hash))))))

  ;; i386 reference translation (SBCL side) — diagnostic FNV census only.
  ;; Guarded: the i386 translator uses a fixed-size buffer that the (growing)
  ;; fixpoint source can overflow.  Not needed for the x64/aarch64 Gen0 image.
  (format t "~%i386 reference translation (SBCL side)...~%")
  (handler-case
      (let ((i386-fn-table-for-translator
             (mapcar (lambda (fi)
                       (list (string (mvm-function-info-name fi))
                             (mvm-function-info-bytecode-offset fi)
                             (mvm-function-info-bytecode-length fi)))
                     fn-table)))
        (multiple-value-bind (i386-buf i386-fn-map)
            (modus.mvm.i386::translate-mvm-to-i386 bytecode i386-fn-table-for-translator)
          (declare (ignore i386-fn-map))
          (let* ((i386-size (modus.mvm.i386::i386-buffer-position i386-buf))
                 (i386-bytes (modus.mvm.i386::i386-buffer-bytes i386-buf)))
            (format t "  SBCL-side i386 native: ~D bytes~%" i386-size)
            (let ((hash 2166136261))
              (dotimes (i i386-size)
                (setf hash (logand #xFFFFFFFF
                                   (* (logxor hash (aref i386-bytes i)) 16777619))))
              (format t "  SBCL-side i386 FNV-1a: ~D~%" hash)))))
    (error (e)
      (format t "  (i386 reference translation SKIPPED: ~A)~%" e)))

  ;; Phase 2: Assemble fixpoint image for Gen0 architecture
  (format t "~%Phase 2: Assembling fixpoint kernel image (~A)...~%"
          cl-user::*fixpoint-gen0-arch*)

  ;; Architecture-specific parameters
  ;; AArch64 fixpoint uses :fixpoint boot descriptor (MMU offset mapping)
  ;; so x64-compatible addresses (0x500000, 0x08000000) work on AArch64.
  ;; The UART is mapped at VA 0x20000000 → PA 0x09000000.
  (let* ((gen0-arch cl-user::*fixpoint-gen0-arch*)
         (build-target (case gen0-arch
                         (:x64     :x86-64)
                         (:aarch64 :aarch64)
                         (t (error "Unsupported Gen0 arch: ~A" gen0-arch))))
         (boot-target (case gen0-arch
                        (:x64     :x86-64)
                        (:aarch64 :fixpoint)))  ; MMU offset mapping for x64-compat addresses
         (arch-id (case gen0-arch (:x64 0) (:aarch64 1)))
         ;; VA where image appears after boot:
         ;; x64: loaded at PA 0x100000, identity mapped, so VA = 0x100000
         ;; aarch64: loaded at PA 0x40080000, MMU maps VA=PA-0x40000000, so VA = 0x80000
         (load-addr (case gen0-arch (:x64 #x100000) (:aarch64 #x80000)))
         ;; Metadata image offset: chosen so VA = load-addr + offset = 0x500000
         (metadata-offset (case gen0-arch (:x64 #x400000) (:aarch64 #x480000)))
         (jmp-size (case gen0-arch (:x64 5) (:aarch64 4)))
         (output-path (case gen0-arch
                        (:x64     "/tmp/fixpoint-gen0.elf")
                        (:aarch64 "/tmp/fixpoint-gen0-aarch64.bin")))
         (qemu-cmd (case gen0-arch
                     (:x64 (format nil "qemu-system-x86_64 -kernel ~A -m 512 -nographic -no-reboot" output-path))
                     (:aarch64 (format nil "qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 -kernel ~A -nographic -semihosting" output-path)))))

  ;; AArch64 translator config
  (when (eq gen0-arch :aarch64)
    (setq *aarch64-serial-base* #x20000000)   ; VA-mapped UART (MMU: VA 0x20000000 → PA 0x09000000)
    (setq *aarch64-serial-width* 0)
    (setq *aarch64-serial-tx-poll* nil)
    (setq *aarch64-sched-lock-addr* nil))

  (let ((boot-desc (get-boot-descriptor boot-target)))
    (let ((image (assemble-kernel-image module (find-target build-target)
                                        :boot-descriptor boot-desc)))
        (let* ((img-bytes (kernel-image-image-bytes image))
               (img-len (length img-bytes)))
          (format t "  Base image: ~D bytes~%" img-len)
          (format t "  Functions: ~D~%" (length fn-table))

          ;; Append fixpoint data: MVM bytecode + function table + metadata
          (let* ((ft-entries
                  (mapcar (lambda (fi)
                            (list (mvm-function-info-name-hash fi)
                                  (mvm-function-info-bytecode-offset fi)
                                  (mvm-function-info-bytecode-length fi)))
                          fn-table))
                 (bc-len (length bytecode))
                 (ft-count (length ft-entries))
                 (ft-entry-size (* ft-count 12))
                 (bc-offset img-len)
                 (ft-offset (+ bc-offset bc-len))
                 (metadata-size 64)
                 (final-size (+ metadata-offset metadata-size))
                 (extended (make-array final-size
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 0)))

            ;; Verify content fits before metadata
            (let ((content-end (+ ft-offset ft-entry-size)))
              (when (> content-end metadata-offset)
                (error "Fixpoint content (~D bytes) exceeds metadata offset 0x~X"
                       content-end metadata-offset)))

            ;; Copy base image + append bytecode + function table
            (dotimes (i img-len)
              (setf (aref extended i) (aref img-bytes i)))
            (dotimes (i bc-len)
              (setf (aref extended (+ bc-offset i)) (aref bytecode i)))
            (loop for entry in ft-entries
                  for idx from 0
                  for base = (+ ft-offset (* idx 12))
                  do (let ((hash (logand (first entry) #xFFFFFFFF))
                           (off (second entry))
                           (len (third entry)))
                       (dotimes (b 4)
                         (setf (aref extended (+ base b))
                               (logand (ash hash (* b -8)) #xFF)))
                       (dotimes (b 4)
                         (setf (aref extended (+ base 4 b))
                               (logand (ash off (* b -8)) #xFF)))
                       (dotimes (b 4)
                         (setf (aref extended (+ base 8 b))
                               (logand (ash len (* b -8)) #xFF)))))

            ;; Write fixpoint metadata (architecture-neutral u32 LE format)
            (let ((preamble-size (length (kernel-image-boot-code image))))
              (flet ((write-u32-le (offset value)
                       (dotimes (b 4)
                         (setf (aref extended (+ offset b))
                               (logand (ash value (* b -8)) #xFF)))))
                (write-u32-le metadata-offset #x544D564D)              ; +0x00: magic "MVMT"
                (write-u32-le (+ metadata-offset 4) 1)                 ; +0x04: version
                (write-u32-le (+ metadata-offset 8) arch-id)           ; +0x08: my-architecture
                (write-u32-le (+ metadata-offset 12) bc-offset)        ; +0x0C: bytecode-offset
                (write-u32-le (+ metadata-offset 16) bc-len)           ; +0x10: bytecode-length
                (write-u32-le (+ metadata-offset 20) ft-offset)        ; +0x14: fn-table-offset
                (write-u32-le (+ metadata-offset 24) ft-count)         ; +0x18: fn-table-count
                (write-u32-le (+ metadata-offset 28) (+ preamble-size jmp-size)) ; +0x1C: native-code-offset
                (write-u32-le (+ metadata-offset 32)
                              (- img-len (+ preamble-size jmp-size)))   ; +0x20: native-code-length
                (write-u32-le (+ metadata-offset 36) preamble-size)    ; +0x24: preamble-size
                (write-u32-le (+ metadata-offset 40)
                              (logand (compute-name-hash "KERNEL-MAIN") #xFFFFFFFF)) ; +0x28: kernel-main-hash
                (write-u32-le (+ metadata-offset 44)
                              (kernel-image-entry-point image))         ; +0x2C: kernel-main-native-offset
                (write-u32-le (+ metadata-offset 48) load-addr)        ; +0x30: image-load-addr
                (write-u32-le (+ metadata-offset 52) 1)                ; +0x34: target-architecture (default)
                (write-u32-le (+ metadata-offset 56) 0))               ; +0x38: mode (0=cross-compile)
              (format t "  Preamble: ~D bytes, native offset: ~D~%"
                      preamble-size (kernel-image-entry-point image)))

            (format t "  Fixpoint data: ~D bytes bytecode + ~D function entries~%"
                    bc-len ft-count)
            (format t "  Final image: ~D bytes (~DKB)~%" final-size
                    (ceiling final-size 1024))

            ;; Write to disk
            (with-open-file (out output-path
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
              (write-sequence extended out))
            (format t "~%Fixpoint Gen0 (~A) written to ~A~%" gen0-arch output-path)
            (format t "~%Boot with:~%  ~A~%" qemu-cmd)
            (format t "~%Fixpoint data:~%")
            (format t "  MVM bytecode: ~D bytes (the fixed point)~%" bc-len)
            (format t "  Functions: ~D~%" ft-count)
            (format t "  Bytecode at: 0x~X (offset 0x~X)~%"
                    (+ load-addr bc-offset) bc-offset)
            (format t "  Fn table at: 0x~X (offset 0x~X, ~D entries)~%"
                    (+ load-addr ft-offset) ft-offset ft-count)
            (format t "  Metadata at: 0x~X (offset 0x~X)~%"
                    (+ load-addr metadata-offset) metadata-offset)))))))

