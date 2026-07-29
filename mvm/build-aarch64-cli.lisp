;;;; build-aarch64-cli.lisp — minimal AArch64/Linux JIT host (WS4 Stage 3-5 sandbox)
;;;;
;;;; A :linux-aarch64 ELF that bakes the FULL self-hosted stack — prelude + gc +
;;;; rt + CL bridge + MVM ISA/interp/compiler + mvm-eval + the AArch64 native
;;;; translator (translate-aarch64) + %init-aarch64-translator co-init — but NOT
;;;; the ANSI test corpus.  Dropping the corpus (a) leaves layout headroom the
;;;; gate image no longer has (adding baked JIT-driver source overflows the gate
;;;; at PC=0), and (b) cuts build time.  kernel-main runs a baked JIT self-test
;;;; (primitive probe → mvm-eval parity → Stage-3 out-of-module call relocation)
;;;; and exits.  This is the iteration vehicle for WS4 AArch64 Stages 3/4/5.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-aarch64-cli.lisp
;;;;   → /home/claude/modus-aa64-cli   (override with MODUS_CLI_OUT)
;;;;
;;;; We LOAD build-ansi-common-aarch64.lisp to reuse its proven source-var
;;;; construction (mvm-text reads, the a64-buffer/translate shrink+strip, the
;;;; %init-aarch64-translator co-init, the sft/sym-name/runtime-macro scanners)
;;;; and its helpers.  The corpus it reads into *real-ansi-sources* is simply
;;;; NOT concatenated into our *full-source*, so it is never compiled in.

(defvar *ansi-target-bare-metal* nil)
(load (merge-pathnames "build-ansi-common-aarch64.lisp"
                       (directory-namestring (truename *load-truename*))))

(format t "~%=== Building minimal AArch64 CLI/JIT host (no corpus) ===~%")

;;; strip (in-package ...) forms from source text (the gate wrapper does this;
;;; the common file leaves the raw text with in-package forms in it).
(defun cli-strip-in-package (text)
  (let ((result text))
    (loop
      (let ((pos (search "(in-package " result)))
        (unless pos (return result))
        (let ((end (position #\) result :start pos)))
          (if end
              (setf result (concatenate 'string
                                        (subseq result 0 pos)
                                        (subseq result (1+ end))))
              (return result)))))))

(setf *prelude-source* (cli-strip-in-package *prelude-source*))
(setf *rt-source*      (cli-strip-in-package *rt-source*))
(setf *bridge-source*  (cli-strip-in-package *bridge-source*))

;;; ============================================================
;;; Driver (sys-exit + kernel-main JIT self-test)
;;; ============================================================

(defvar *driver-source* "

(defun halt ()
  (syscall3 93 1 0 0))

(defun sys-exit (code)
  (let ((c code))
    (syscall3 93 c 0 0)))

(defun %parse-decimal-at-fixed-208 ()
  (let ((n 0) (i 0))
    (loop
      (let ((b (mem-ref (+ #x10000208 i) :u8)))
        (when (or (< b 48) (> b 57)) (return n))
        (setq n (+ (* n 10) (- b 48)))
        (setq i (+ i 1))))))

(defun kernel-main ()
  ;; Banner: CLI-BOOT
  (write-string-serial \"CLI-BOOT\") (write-char-serial 10)

  ;; Zero the runtime-metadata BSS slots (Linux/AArch64 kernels don't reliably
  ;; zero a ~900MB BSS tail; garbage here corrupts the global alist / handler
  ;; frames).  Same slots the ANSI gate kernel-main clears.
  (setf (mem-ref #x10000080 :u64) 0)
  (setf (mem-ref #x10000088 :u64) 0)
  (setf (mem-ref #x10000090 :u64) 0)
  (setf (mem-ref #x10000098 :u64) 0)
  (setf (mem-ref #x10000158 :u64) 0)
  (setf (mem-ref #x10000160 :u64) 0)
  (setf (mem-ref #x10000168 :u64) 0)
  (setf (mem-ref #x10000180 :u64) 0)
  (setf (mem-ref #x10000188 :u64) 0)
  (setf (mem-ref #x10000190 :u64) 0)
  (setf (mem-ref #x10000198 :u64) 0)
  (setf (mem-ref #x100001A0 :u64) 0)
  (setf (mem-ref #x100001A8 :u64) 0)
  (setf (mem-ref #x100001B0 :u64) 0)
  (setf (mem-ref #x100001B8 :u64) 0)
  (setf (mem-ref #x100001C0 :u64) 0)
  (setf (mem-ref #x100001C8 :u64) 0)
  (setf (mem-ref #x10000400 :u64) 0)

  ;; Runtime init (mirrors the ANSI gate kernel-main prefix, trimmed).
  (init-symbol-table)
  (init-keyword-table)
  (%init-packages)
  (%init-streams)
  (%init-reader)
  (%init-condition-types)
  (%init-method-combinations)
  (%init-symbol-function-table)
  (%init-sft-auto)
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)
  (%init-runtime-macros)
  (init-compiler-macro-set)
  (%init-signal-handling)
  (%init-signal-symbols)
  (%init-make-load-form)
  (%init-clos-protocol)

  ;; File-I/O scratch buffers + counters (defvar init-thunks don't run at boot).
  (setq *cstr-scratch* #x0FE00000)
  (setq *io-buf-addr*  #x0FF00000)
  (setq *scratch-mmapped* nil)
  (setq *filesystem* nil)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)
  (setq *write-object-budget* 1000000)
  (setq *gensym-counter* 0)
  (setq *gentemp-counter* 0)
  (setq array-total-size-limit  (ash 1 24))
  (setq array-dimension-limit   (ash 1 24))
  (setq array-rank-limit        256)
  (setq call-arguments-limit    256)
  (setq lambda-parameters-limit 256)
  (setq lambda-list-keywords    (quote (&allow-other-keys &aux &body &environment &key
                                        &optional &rest &whole)))
  (setq multiple-values-limit   16)
  (setq most-positive-fixnum  4611686018427387903)
  (setq most-negative-fixnum -4611686018427387904)
  (setq pi 3.141592653589793d0)
  (setq *default-pathname-defaults* \"/tmp/\")

  ;; WS4 AArch64 JIT co-init: populate the translator's defparameter tables
  ;; (item 7 — init-thunks don't run) and, for the JIT, set stack-align-16 +
  ;; linux-mode + jit-mode.  %init-aarch64-translator sets the first two;
  ;; Stage-3 relocation additionally needs *aarch64-jit-mode* t.
  (%init-aarch64-translator)
  (setq *aarch64-jit-mode* t)

  ;; --- JIT SELF-TEST -------------------------------------------------------
  ;; (1) Primitive probe: mmap PROT_RWX, write `movz x0,#84 ; ret` (84 = tagged
  ;;     fixnum 42), icache-flush, %jit-call.  Exercises traps #x0531/#x0533/
  ;;     #x0532 end-to-end.  Expect jitprim=42.
  (write-string-serial \"jitprim=\")
  (print-dec
   (handler-case
       (let ((base (%mmap-exec-page 4096)))
         (setf (mem-ref (+ base 0) :u8) #x80)
         (setf (mem-ref (+ base 1) :u8) #x0A)
         (setf (mem-ref (+ base 2) :u8) #x80)
         (setf (mem-ref (+ base 3) :u8) #xD2)
         (setf (mem-ref (+ base 4) :u8) #xC0)
         (setf (mem-ref (+ base 5) :u8) #x03)
         (setf (mem-ref (+ base 6) :u8) #x5F)
         (setf (mem-ref (+ base 7) :u8) #xD6)
         (%jit-icache-flush base 8)
         (%jit-call base))
     (t (c) -1)))
  (write-char-serial 10)

  ;; (2) mvm-eval interpret parity (self-hosted compile → MVM → interpret).
  (write-string-serial \"add=\")
  (print-dec (handler-case (mvm-eval (quote (+ 1 2))) (t (c) -1))) (write-char-serial 10)
  (write-string-serial \"sqr=\")
  (print-dec (handler-case (mvm-eval (quote (let ((x 5)) (* x x)))) (t (c) -1))) (write-char-serial 10)
  (write-string-serial \"len=\")
  (print-dec (handler-case (mvm-eval (quote (length (list 1 2 3)))) (t (c) -1))) (write-char-serial 10)

  ;; (3) WS4 aarch64 STAGE 3: out-of-module CALL relocation.  JIT a form whose
  ;;     MAIN path takes a real out-of-module native call: (length (list 1 2 3))
  ;;     — LENGTH/LIST are native runtime fns (synthetic offset >= #x40000000).
  ;;     Under *aarch64-jit-mode* the translator emits a relocatable MOVZ/MOVK-
  ;;     quad + BLR and records the patch site in *aarch64-call-relocs*.  Here
  ;;     we resolve each reloc (rt-table → %mvm-resolve-runtime-fn → addr =
  ;;     word-3), patch the 4 imm16 fields, icache-flush, %jit-call, and compare
  ;;     to mvm-interpret.  aa64s3=rel=N MATCH ⟹ the JIT relocated + called a
  ;;     main-image runtime helper correctly.
  (write-string-serial \"aa64s3=\")
  (handler-case
      (let* ((tuple (%mvm-eval-compile-tuple (list (quote (length (list 1 2 3))))))
             (bc (car tuple)) (entry (cadr tuple)) (ft-list (caddr tuple))
             (fn-table (cadddr tuple)) (rt-table (car (cddddr tuple)))
             (lam-offsets (cadr (cddddr tuple)))
             (ftbl (make-hash-table :test (quote eql))))
        (let ((i 0)) (dolist (e ft-list) (setf (gethash i ftbl) (cadr e)) (setq i (+ i 1))))
        (let ((interp (mvm-interpret bc :entry-point entry :function-table fn-table
                                     :runtime-table rt-table :return-raw nil
                                     :lambda-offsets lam-offsets)))
          (multiple-value-bind (nbuf fn-map) (translate-mvm-to-aarch64 bc ftbl)
            (let ((nwords (a64-buffer-position nbuf)) (code (a64-buffer-code nbuf))
                  (eoff (gethash entry fn-map)) (relocs *aarch64-call-relocs*)
                  (base (%mmap-exec-page 8192)) (k 0) (ok t))
              (loop while (< k nwords)
                    do (let ((w (aref code k)) (o (* k 4)))
                         (setf (mem-ref (+ base o) :u8) (logand w 255))
                         (setf (mem-ref (+ base (+ o 1)) :u8) (logand (ash w -8) 255))
                         (setf (mem-ref (+ base (+ o 2)) :u8) (logand (ash w -16) 255))
                         (setf (mem-ref (+ base (+ o 3)) :u8) (logand (ash w -24) 255))
                         (setq k (+ k 1))))
              (write-string-serial \"rel=\") (print-dec (length relocs))
              (dolist (r relocs)
                (let* ((moff (car r)) (synth (cdr r))
                       (name (gethash synth rt-table))
                       (fn (and name (%mvm-resolve-runtime-fn name)))
                       (addr (if fn (- (%val->word fn) 3) 0)))
                  (if (> addr 0)
                      (let ((w0 (logior #xD2800010 (ash (logand addr #xFFFF) 5)))
                            (w1 (logior #xF2A00010 (ash (logand (ash addr -16) #xFFFF) 5)))
                            (w2 (logior #xF2C00010 (ash (logand (ash addr -32) #xFFFF) 5)))
                            (w3 (logior #xF2E00010 (ash (logand (ash addr -48) #xFFFF) 5))))
                        (setf (mem-ref (+ base moff) :u8) (logand w0 255))
                        (setf (mem-ref (+ base (+ moff 1)) :u8) (logand (ash w0 -8) 255))
                        (setf (mem-ref (+ base (+ moff 2)) :u8) (logand (ash w0 -16) 255))
                        (setf (mem-ref (+ base (+ moff 3)) :u8) (logand (ash w0 -24) 255))
                        (setf (mem-ref (+ base (+ moff 4)) :u8) (logand w1 255))
                        (setf (mem-ref (+ base (+ moff 5)) :u8) (logand (ash w1 -8) 255))
                        (setf (mem-ref (+ base (+ moff 6)) :u8) (logand (ash w1 -16) 255))
                        (setf (mem-ref (+ base (+ moff 7)) :u8) (logand (ash w1 -24) 255))
                        (setf (mem-ref (+ base (+ moff 8)) :u8) (logand w2 255))
                        (setf (mem-ref (+ base (+ moff 9)) :u8) (logand (ash w2 -8) 255))
                        (setf (mem-ref (+ base (+ moff 10)) :u8) (logand (ash w2 -16) 255))
                        (setf (mem-ref (+ base (+ moff 11)) :u8) (logand (ash w2 -24) 255))
                        (setf (mem-ref (+ base (+ moff 12)) :u8) (logand w3 255))
                        (setf (mem-ref (+ base (+ moff 13)) :u8) (logand (ash w3 -8) 255))
                        (setf (mem-ref (+ base (+ moff 14)) :u8) (logand (ash w3 -16) 255))
                        (setf (mem-ref (+ base (+ moff 15)) :u8) (logand (ash w3 -24) 255)))
                      (setq ok nil))))
              (%jit-icache-flush base (* nwords 4))
              (write-char-serial 32)
              (if (and ok eoff)
                  (if (= (%jit-call (+ base eoff)) interp)
                      (write-string-serial \"MATCH\")
                      (write-string-serial \"MISMATCH\"))
                  (write-string-serial \"NORELOC\"))))))
    (t (c) (write-string-serial \"ERR\")))
  (write-char-serial 10)

  (write-string-serial \"CLI-DONE\") (write-char-serial 10)
  (sys-exit 0))
")

;;; ============================================================
;;; Auto-generated sym-name reverse table (incl. driver + compiler-in-image
;;; literals).  Reuses the common file's helper (which also scans the driver
;;; source we just defined).
;;; ============================================================

(setq *sym-name-auto-source*
      (%build-sym-name-auto-source (list *driver-source*)
                                   (list *compiler-in-image-source*)))

;;; ============================================================
;;; Assemble corpus-free *full-source*
;;; ============================================================

(format t "~%Assembling corpus-free full source...~%")

(defvar *full-source*
  (concatenate 'string
    *prelude-source*  (string #\Newline)
    *gc-source*       (string #\Newline)
    *mcgc-pin-source*
    *rt-source*       (string #\Newline)
    *bridge-source*   (string #\Newline)
    ;; MVM ISA + interp + compiler + mvm-eval + translate-aarch64 + co-init.
    *compiler-in-image-source* (string #\Newline)
    "(defvar *sym-name-table* nil)" (string #\Newline)
    *sft-auto-source*          (string #\Newline)
    *sym-name-auto-source*     (string #\Newline)
    *runtime-macros-auto-source* (string #\Newline)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; Build the Linux/AArch64 ELF (same target machinery as the gate wrapper)
;;; ============================================================

(mvm-load "boot/boot-linux-aarch64.lisp")

(in-package :modus.mvm)

(install-aarch64-translator)

(setf *aarch64-stack-align-16* t)
(setf *aarch64-linux-mode* t)
(setf *aarch64-fn-align-offset* 120)
(setf *linux-aarch64-r25-offset* +linux-aarch64-heap-size+)
(setf *aarch64-handler-pop-label* nil)
(setf *aarch64-handler-push-label* nil)
(setf *aarch64-gc-trampoline-label* nil)

(format t "~%Compiling AArch64 CLI/JIT host (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-aarch64 :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_CLI_OUT")
                  "/home/claude/modus-aa64-cli")))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)))
