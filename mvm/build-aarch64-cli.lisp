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

;; WS4-AA64 FLIP (#199, 2026-07-31): the CLI (JIT-capable shipping image)
;; DEFAULTS the runtime JIT ON.  Justification: on real Cortex-A76 the JIT is a
;; ~4.6x hot-code speedup (repeated mvm-eval 35168ms interpret -> 7661ms JIT,
;; BENCH-NATIVE=300005 so the native path amortized via the page cache), it is
;; correctness-proven (55555 SURVIVED-4000, odd-form battery, full-corpus
;; answers correct), and it is load-bearing for the cooperative-threading model.
;; MODUS_NO_JIT=1 reverts to interpret.  This is DECOUPLED from the gate's
;; *aarch64-jit-default* (which STAYS nil): the ANSI gate is a one-shot test
;; harness where the JIT amortizes nothing and cripples throughput (it times out
;; ~587 vs 17189 interpret), so the gate must remain interpret to stay usable.
(defvar *cli-jit-default-source*
  (format nil "(defun %~A-jit-default () ~A)~%" "cli"
          (if (let ((no #+sbcl (sb-ext:posix-getenv "MODUS_NO_JIT") #-sbcl nil))
                (and no (> (length no) 0)))
              "nil" "t")))
(format t "  CLI default runtime JIT: ~A~%"
        (if (let ((no #+sbcl (sb-ext:posix-getenv "MODUS_NO_JIT") #-sbcl nil))
              (and no (> (length no) 0)))
            "OFF (MODUS_NO_JIT)" "ON (flipped #199)"))

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

;; WS4 aarch64 Stage 4 helper: patch a MOVZ/MOVK quad (4 consecutive words) at
;; BASE+OFF with the 4 imm16 halves of VAL.  Reads each placeholder word and ORs
;; in (half << 5) — register-agnostic (li-const / fn-addr sites may target any
;; Xd, not just x16), because a64-movz/a64-movk emit the correct rd+opcode base
;; with imm=0 and the imm16 field (bits 5-20) is zero in the placeholder.
(defun %jit-patch-quad (base off val)
  (let ((k 0))
    (loop
      (when (>= k 4) (return nil))
      (let* ((wo (+ off (* k 4)))
             (w (logior (mem-ref (+ base wo) :u8)
                        (ash (mem-ref (+ base (+ wo 1)) :u8) 8)
                        (ash (mem-ref (+ base (+ wo 2)) :u8) 16)
                        (ash (mem-ref (+ base (+ wo 3)) :u8) 24)))
             (imm (logand (ash val (- (* k 16))) #xFFFF))
             (nw (logior w (ash imm 5))))
        (setf (mem-ref (+ base wo) :u8) (logand nw 255))
        (setf (mem-ref (+ base (+ wo 1)) :u8) (logand (ash nw -8) 255))
        (setf (mem-ref (+ base (+ wo 2)) :u8) (logand (ash nw -16) 255))
        (setf (mem-ref (+ base (+ wo 3)) :u8) (logand (ash nw -24) 255)))
      (setq k (+ k 1)))))

;; WS4 aarch64 Stage 3/4: JIT-compile FORM, copy to an exec page, patch ALL
;; three reloc classes (out-of-module CALL = untagged word-3; out-of-module
;; #'NAME fn-addr = tagged word; quoted-literal li-const = pool obj tagged word),
;; icache-flush, %jit-call.  Returns (interp-result . jit-result) for a
;; differential compare (NORELOC in cdr if a reloc failed to resolve).
(defun %jit-run-form (form)
  (let* ((tuple (%mvm-eval-compile-tuple (list form)))
         (bc (car tuple)) (entry (cadr tuple)) (ft-list (caddr tuple))
         (rt-table (car (cddddr tuple)))
         (ftbl (make-hash-table :test (quote eql))))
    (let ((i 0)) (dolist (e ft-list) (setf (gethash i ftbl) (cadr e)) (setq i (+ i 1))))
    (multiple-value-bind (nbuf fn-map) (translate-mvm-to-aarch64 bc ftbl)
      (let ((nwords (a64-buffer-position nbuf)) (code (a64-buffer-code nbuf))
            (eoff (gethash entry fn-map))
            (crel *aarch64-call-relocs*)
            (frel *aarch64-fn-addr-relocs*)
            (cpat *aarch64-li-const-patches*)
            (base (%mmap-exec-page 16384)) (k 0) (ok t))
        (loop
          (when (>= k nwords) (return nil))
          (let ((w (aref code k)) (o (* k 4)))
            (setf (mem-ref (+ base o) :u8) (logand w 255))
            (setf (mem-ref (+ base (+ o 1)) :u8) (logand (ash w -8) 255))
            (setf (mem-ref (+ base (+ o 2)) :u8) (logand (ash w -16) 255))
            (setf (mem-ref (+ base (+ o 3)) :u8) (logand (ash w -24) 255)))
          (setq k (+ k 1)))
        ;; Out-of-module CALL relocations (untagged callee addr = word-3).
        (dolist (r crel)
          (let* ((name (gethash (cdr r) rt-table))
                 (fn (and name (%mvm-resolve-runtime-fn name)))
                 (addr (if fn (- (%val->word fn) 3) 0)))
            (if (> addr 0) (%jit-patch-quad base (car r) addr) (setq ok nil))))
        ;; Out-of-module #'NAME fn-addr relocations (full TAGGED fn word).
        (dolist (r frel)
          (let* ((name (gethash (cdr r) rt-table))
                 (fn (and name (%mvm-resolve-runtime-fn name)))
                 (word (if fn (%val->word fn) 0)))
            (if (> word 0) (%jit-patch-quad base (car r) word) (setq ok nil))))
        ;; Quoted-literal / string li-const patches (pool object tagged word).
        (dolist (p cpat)
          (let* ((obj (if *e2-const-pool* (gethash (cdr p) *e2-const-pool*) nil)))
            (%jit-patch-quad base (car p) (%val->word obj))))
        (%jit-icache-flush base (* nwords 4))
        (if (and ok eoff)
            (%jit-call (+ base eoff))
            (quote NORELOC))))))

(defun %jit-pv (x)
  (if (integerp x) (print-dec x) (write-string-serial \"NI\")))

;; WS4 aarch64 Stage 5: runtime-controllable JIT gate for the in-image
;; differential.  Overrides mvm-eval.lisp's base %jit-enabled-p (last-defun-wins)
;; so we can flip the SEAM on/off per form via *cli-jit-on* and confirm the
;; native path (translate-mvm-to-aarch64 → exec page → %jit-call) agrees with
;; pure interpret AND actually ran native (*jit-native-count* advanced).
(defvar *cli-jit-on* nil)
(defun %jit-enabled-p () *cli-jit-on*)

;; Stage-5 probe: eval FORM through the REAL mvm-eval seam twice — interpret
;; (jit off) then JIT (jit on) — compare, and report whether the JIT run took
;; the NATIVE path (native-count advanced) vs fell back to interpret.
(defun %s5-probe (label form)
  (setq *cli-jit-on* nil)
  (let ((iv (handler-case (mvm-eval form) (t (c) (quote IERR))))
        (nc0 *jit-native-count*))
    (setq *cli-jit-on* t)
    (let ((jv (handler-case (mvm-eval form) (t (c) (quote JERR)))))
      (setq *cli-jit-on* nil)
      (let ((native (> *jit-native-count* nc0)))
        (write-string-serial label)
        (if (eql iv jv)
            (progn (write-string-serial \"MATCH v=\") (%jit-pv iv)
                   (write-string-serial (if native \" NATIVE\" \" fellback\")))
            (progn (write-string-serial \"MISMATCH i=\") (%jit-pv iv)
                   (write-string-serial \" j=\") (%jit-pv jv)))
        (write-char-serial 10)))))

(defun %jit-diff-probe (label form)
  (write-string-serial label)
  ;; Force the interpret baseline (independent of the built-in default) so this
  ;; probe always compares manual-JIT vs pure interpret.
  (let ((save *cli-jit-on*))
    (setq *cli-jit-on* nil)
    (let ((iv (handler-case (mvm-eval form) (t (c) (quote IERR))))
          (jv (handler-case (%jit-run-form form) (t (c) (quote JERR)))))
      (setq *cli-jit-on* save)
      (if (eql iv jv)
          (progn (write-string-serial \"MATCH v=\") (%jit-pv iv))
          (progn (write-string-serial \"MISMATCH i=\") (%jit-pv iv)
                 (write-string-serial \" j=\") (%jit-pv jv)))))
  (write-char-serial 10))

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
  ;; WS4-AA64 #160: object-start bitmap config words (page_base / bitmap_base).
  ;; Zero them, then reserve the bitmap BEFORE any allocation (init-symbol-table
  ;; below is the first allocator) so every mutator alloc records its start bit.
  ;; %gc-bitmap-init is non-allocating; the boot already published from_start at
  ;; 0x10000040 (which it reads as page_base).
  (setf (mem-ref #x10000E00 :u64) 0)
  (setf (mem-ref #x10000E18 :u64) 0)
  (setf (mem-ref #x10000E40 :u64) 0)   ; #160 bug#4: cons-kind bitmap base
  (%gc-bitmap-init)

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
  ;; Select the aarch64 JIT back-end and seed the CLI's default JIT gate from
  ;; the build-time MODUS_USE_JIT/MODUS_NO_JIT knob (%cli-jit-default).  Probes
  ;; below still force their own on/off around each form.
  (setq *jit-target-arch* :aarch64)
  (setq *cli-jit-on* (%cli-jit-default))
  (write-string-serial \"cli-jit-default=\") (print-dec (if *cli-jit-on* 1 0))
  (write-char-serial 10)

  ;; ISOLATED pure-cons GC milestone (argv 33333) — runs BEFORE any JIT/mvm-eval
  ;; probe so its collection count is UNAMBIGUOUS (Stage-1 native-GC milestone).
  (when (eql (%parse-decimal-at-fixed-208) 33333)
    (write-string-serial \"GCLIST-START\") (write-char-serial 10)
    (let ((live nil) (i 0)
          (gmax (let ((a (%parse-decimal-at-fixed-248)))
                  (if (> a 0) (* a 1000000) 20000000))))
      (loop (when (>= i 100000) (return nil)) (setq live (cons i live)) (setq i (+ i 1)))
      (write-string-serial \"built gc=\") (print-dec (mem-ref #x10000060 :u64)) (write-char-serial 10)
      (write-string-serial \"head0=\") (print-dec (if (consp live) (car live) -7)) (write-char-serial 10)
      (setq i 0)
      (loop (when (>= i gmax) (return nil)) (cons i i) (setq i (+ i 1)))
      (write-string-serial \"gc-after=\") (print-dec (mem-ref #x10000060 :u64)) (write-char-serial 10)
      (let ((n 0) (p live))
        (loop
          (when (null p) (return nil))
          (when (not (consp p)) (return nil))
          (setq n (+ n 1)) (setq p (cdr p)))
        (write-string-serial \"walked=\") (print-dec n) (write-char-serial 10)
        (write-string-serial \"headcar=\") (print-dec (if (consp live) (car live) -7))
        (write-char-serial 10)))
    (write-string-serial \"GCLIST-END\") (write-char-serial 10)
    (sys-exit 0))

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

  ;; (4) WS4 aarch64 STAGE 4: const-pool + fn-addr relocation, via %jit-run-form
  ;;     (patches call + fn-addr + li-const quads).  Each probe compares the JIT
  ;;     result to mvm-interpret.
  ;;       const: (car '(42 7))                     — quoted-list li-const → 42
  ;;       str:   (length \"hello\")                   — string li-const + CALL → 5
  ;;       fn:    (funcall #'car '(9 8)) via a var   — out-of-module fn-addr → 9
  (%jit-diff-probe \"aa64s4-const=\" (quote (car (quote (42 7)))))
  (%jit-diff-probe \"aa64s4-str=\"   (quote (length \"hello\")))
  (%jit-diff-probe \"aa64s4-fn=\"    (quote (let ((f (function length))) (funcall f (quote (9 8 7))))))

  ;; (5) WS4 aarch64 STAGE 5: drive the GENERICIZED mvm-eval SEAM.  With
  ;;     *jit-target-arch* = :aarch64 and the JIT gate on, production mvm-eval
  ;;     routes through %jit-translate-page → %jit-translate-page-1-aarch64
  ;;     (translate → exec page → MOVZ-quad relocation → %jit-call).  Each probe
  ;;     compares the seam's JIT result to pure interpret AND reports whether the
  ;;     native path actually ran (NATIVE) or cleanly fell back (fellback).
  (setq *jit-target-arch* :aarch64)
  (setq *jit-native-count* 0)
  (setq *jit-fallback-count* 0)
  (setq *jit-page-cache* nil)
  (%s5-probe \"aa64s5-add=\"  (quote (+ 1 2)))
  (%s5-probe \"aa64s5-sqr=\"  (quote (let ((x 5)) (* x x))))
  (%s5-probe \"aa64s5-len=\"  (quote (length (list 1 2 3))))
  (%s5-probe \"aa64s5-const=\" (quote (car (quote (42 7)))))
  (%s5-probe \"aa64s5-str=\"  (quote (length \"hello\")))
  (write-string-serial \"aa64s5-native-total=\") (print-dec *jit-native-count*)
  (write-char-serial 10)
  (write-string-serial \"aa64s5-fallback-total=\") (print-dec *jit-fallback-count*)
  (write-char-serial 10)

  ;; (P) WS4-AA64 #160 GC-POISON REPRO (argv1 = 44444): with GC ON but NOT yet
  ;;     hardened, fill from-space with garbage to force collections, then run a
  ;;     bignum random/abs loop.  Surviving bignums' raw limb words are scanned
  ;;     by %gc-scan-copied as candidate pointers; a limb that looks like a
  ;;     from-space cons/object pointer gets %gc-copy-object'd → a forwarding
  ;;     pointer stamped over mid-object data → heap poison.  Canaries before/
  ;;     after detect it; a crash (SIGSEGV) or c-after != 1 = poison confirmed.
  ;; (pure-cons GC test moved ABOVE the JIT self-test — see the 33333 probe near
  ;; %init-aarch64-translator — so its collection count is isolated.)

  (when (eql (%parse-decimal-at-fixed-208) 44444)
    (write-string-serial \"GCPOISON-START\") (write-char-serial 10)
    (write-string-serial \"c-before=\")
    (print-dec (handler-case (if (eq (quote zorka) (quote zorka)) 1 0) (t (c) -1)))
    (write-char-serial 10)
    (write-string-serial \"gc-before=\") (print-dec (mem-ref #x10000060 :u64))
    (write-char-serial 10)
    ;; Build a LIVE list of ~20000 random bignums (each = abs of a 300-bit
    ;; random, all non-negative), held in `keep`.  Then allocate garbage conses
    ;; to drive the alloc pointer past the semispace many times → many GCs while
    ;; `keep` (and every bignum in it) is LIVE and gets COPIED each cycle.  Each
    ;; copy runs %gc-scan-copied over the bignums' raw limb words; a limb that
    ;; looks like a from-space cons/object pointer is %gc-copy-object'd → a
    ;; forwarding pointer stamped over live data = poison.  Detect it 3 ways:
    ;;   crash (SIGSEGV) / bn-ok != 20000 (a kept bignum went negative or its
    ;;   abs changed) / c-after != 1.
    (write-string-serial \"poison-test=\")
    (print-dec (handler-case
       (let ((keep nil) (i 0) (bound (ash 1 300))
             (gmax (let ((a (%parse-decimal-at-fixed-248)))
                     (if (> a 0) (* a 1000000) 70000000))))
         ;; phase 1: build live bignum list
         (loop (when (>= i 8000) (return nil))
           (setq keep (cons (abs (random-from-interval bound)) keep))
           (setq i (+ i 1)))
         ;; phase 2: allocate garbage (argv2 millions, default 70M) while `keep`
         ;; stays LIVE on the stack (a real root across every collection).
         (setq i 0)
         (loop (when (>= i gmax) (return nil))
           (cons i i)
           (setq i (+ i 1)))
         ;; phase 3: integrity check — every kept bignum must still be >= 0 and
         ;; equal to its own abs (poison would corrupt a limb → sign/value flip).
         (let ((bad 0))
           (loop for b in keep
                 do (unless (and (>= b 0) (eql b (abs b))) (setq bad (+ bad 1))))
           bad))                     ; 0 = clean, >0 = corrupted bignums
     (t (c) -3)))
    (write-char-serial 10)
    (write-string-serial \"gc-after=\") (print-dec (mem-ref #x10000060 :u64))
    (write-char-serial 10)
    (write-string-serial \"c-after=\")
    (print-dec (handler-case (if (eq (quote zorkz) (quote zorkz)) 1 0) (t (c) -1)))
    (write-char-serial 10)
    (write-string-serial \"GCPOISON-END\") (write-char-serial 10)
    (sys-exit 0))

  ;; (6) GC-OFF EXHAUSTION STRESS (diagnostic for the FLIP decision): JIT many
  ;;     DISTINCT forms in a NON-forked loop (like run-all-tests / a long-lived
  ;;     REPL).  GC is OFF on aarch64-linux, so each ~1.5-1.7MB translation
  ;;     accumulates.  Heartbeat every 50 forms; the last one printed before the
  ;; WS4-AA64 JIT PERF BENCHMARK (argv1 = 777777 JIT-on / 777778 interpret;
  ;; argv2 = iters, default 300000).  Repeatedly mvm-eval a CACHEABLE compute
  ;; form (no DEF*): the first eval compiles + (JIT) translates a page, every
  ;; later eval hits the cache + reuses the exec page so translate cost
  ;; amortizes — the hot/repeated case where the JIT is supposed to win.
  ;; External `time` on the two argv modes on the Pi = the real aarch64 speedup.
  (when (or (eql (%parse-decimal-at-fixed-208) 777777)
            (eql (%parse-decimal-at-fixed-208) 777778))
    (setq *cli-jit-on* (eql (%parse-decimal-at-fixed-208) 777777))
    (write-string-serial \"BENCH-START jit=\") (print-dec (if *cli-jit-on* 1 0)) (write-char-serial 10)
    (let ((n (let ((a (%parse-decimal-at-fixed-248))) (if (and a (> a 0)) a 300000)))
          (i 0) (acc 0))
      (write-string-serial \"BENCH-ITERS=\") (print-dec n) (write-char-serial 10)
      (loop
        (when (>= i n) (return nil))
        (setq acc (mvm-eval (quote (let ((a 6) (b 7)) (if (< a b) (* a b) (+ a b))))))
        (setq i (+ i 1)))
      (write-string-serial \"BENCH-DONE acc=\") (print-dec acc) (write-char-serial 10)
      (write-string-serial \"BENCH-NATIVE=\") (print-dec *jit-native-count*) (write-char-serial 10))
    (sys-exit 0))

  ;;     process dies marks where the 896MB heap exhausts.  Runs only when argv1
  ;;     = 55555 (so the normal CLI run isn't destroyed by it).
  (when (eql (%parse-decimal-at-fixed-208) 55555)
    (setq *cli-jit-on* t)
    (let ((i 0))
      (loop
        (when (>= i 4000) (return nil))
        (when (eql (mod i 100) 0)
          (write-string-serial \"stress i=\") (print-dec i) (write-char-serial 10))
        ;; Distinct form each iteration (varying literal) → distinct bytecode →
        ;; distinct exec page → fresh ~1.7MB translation (no page-cache hit).
        (mvm-eval (list (quote +) i 1))
        (setq i (+ i 1))))
    (setq *cli-jit-on* nil)
    (write-string-serial \"stress-SURVIVED-4000\") (write-char-serial 10))

  ;; (8) WS4 #160 Piece 2 DEFUN-RETENTION regression net (argv1 = 56667):
  ;;     define a fn via JIT (returns a SYMBOL → non-function result → its
  ;;     installer page IS reclaimed).  Drive many transient forms (freeing
  ;;     pages), then call the fn 100x.  If the fn's BODY had lived in the freed
  ;;     installer page, this would UAF-crash; defun-bad must be 0 (body is a
  ;;     SEPARATE module, built on first call).
  (when (eql (%parse-decimal-at-fixed-208) 56667)
    (setq *cli-jit-on* t)
    (write-string-serial \"DEFUNPROBE-START\") (write-char-serial 10)
    (mvm-eval (list (quote defun) (quote probefoo) (quote (x)) (list (quote +) (quote x) 100)))
    (let ((i 0)) (loop (when (>= i 3000) (return nil)) (mvm-eval (list (quote +) i 1)) (setq i (+ i 1))))
    (let ((bad 0) (j 0))
      (loop (when (>= j 100) (return nil))
        (unless (eql (mvm-eval (list (quote probefoo) j)) (+ j 100)) (setq bad (+ bad 1)))
        (setq j (+ j 1)))
      (write-string-serial \"defun-bad=\") (print-dec bad) (write-char-serial 10)
      )
    (write-string-serial \"DEFUNPROBE-END\") (write-char-serial 10)
    (sys-exit 0))

  ;; (9) FLIP-READINESS odd-form battery (argv1 = 56668): JIT-on must NEVER
  ;;     SIGSEGV on any valid form — it either goes native or degrades to
  ;;     interpret via %jit-translate-page's handler-case guard.  Each form
  ;;     below is odd-but-valid; all must return the correct value.  A crash =
  ;;     a translator HARDWARE-FAULT the Lisp-error guard can't catch.
  (when (eql (%parse-decimal-at-fixed-208) 56668)
    (setq *cli-jit-on* t)
    (write-string-serial \"BATTERY-START\") (write-char-serial 10)
    ;; b1: captureless constant lambda → funcall → 5
    (write-string-serial \"b1=\") (print-dec (funcall (mvm-eval (list (quote lambda) (quote ()) 5)))) (write-char-serial 10)
    ;; b2: empty progn → NIL (print 1 if null, 0 otherwise)
    (write-string-serial \"b2null=\") (print-dec (if (null (mvm-eval (list (quote progn)))) 1 0)) (write-char-serial 10)
    ;; b3: immediate literal
    (write-string-serial \"b3=\") (print-dec (mvm-eval (list (quote +) 7 0))) (write-char-serial 10)
    ;; b4: nested lambda → funcall funcall → 1
    (write-string-serial \"b4=\") (print-dec (funcall (funcall (mvm-eval (list (quote lambda) (quote ()) (list (quote lambda) (quote ()) 1)))))) (write-char-serial 10)
    ;; b5: identity lambda → funcall 42 → 42
    (write-string-serial \"b5=\") (print-dec (funcall (mvm-eval (list (quote lambda) (quote (x)) (quote x))) 42)) (write-char-serial 10)
    ;; b6: quoted-literal + arithmetic → (+ (car '(10 20)) 5) → 15
    (write-string-serial \"b6=\") (print-dec (mvm-eval (list (quote +) (list (quote car) (list (quote quote) (list 10 20))) 5))) (write-char-serial 10)
    ;; b7: a form that may be a translator GAP (flet) → must fall back, not crash
    (write-string-serial \"b7=\") (print-dec (mvm-eval (list (quote flet) (list (list (quote g) (quote (y)) (list (quote * ) (quote y) 3))) (list (quote g) 4)))) (write-char-serial 10)
    (write-string-serial \"BATTERY-END\") (write-char-serial 10)
    (sys-exit 0))

  ;; (7) WS4 #160 Piece 2 CLOSURE-RETENTION regression net (argv1 = 56666):
  ;;     JIT N escaping closures (each (lambda () K) — NON-empty lam-offsets +
  ;;     function result → CODE-BEARING → page retained forever, never freed).
  ;;     Keep them live, then drive many TRANSIENT (+ i 1) forms through
  ;;     reclamation (which munmaps their pages).  Finally funcall every closure;
  ;;     the sum must equal 0+1+..+(N-1) = 19900 for N=200.  A crash or wrong sum
  ;;     = a use-after-free (the transient/code-bearing classifier has a hole).
  ;;     Must PASS by construction (code-bearing pages are never reclaimed).
  (when (eql (%parse-decimal-at-fixed-208) 56666)
    (setq *cli-jit-on* t)
    (write-string-serial \"CLOSPROBE-START\") (write-char-serial 10)
    (let ((fns nil) (i 0) (n 200))
      (loop (when (>= i n) (return nil))
        (setq fns (cons (mvm-eval (list (quote lambda) (quote ()) i)) fns))
        (setq i (+ i 1)))
      (setq i 0)
      (loop (when (>= i 6000) (return nil)) (mvm-eval (list (quote +) i 1)) (setq i (+ i 1)))
      (let ((sum 0) (p fns))
        (loop (when (null p) (return nil))
          (setq sum (+ sum (funcall (car p))))
          (setq p (cdr p)))
        (write-string-serial \"clos-sum=\") (print-dec sum) (write-char-serial 10)
        (write-string-serial \"clos-expected=\") (print-dec (* (/ n 2) (- n 1))) (write-char-serial 10)))
    (write-string-serial \"CLOSPROBE-END\") (write-char-serial 10)
    (sys-exit 0))

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
    *driver-source*            (string #\Newline)
    *cli-jit-default-source*))

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
;; WS4-AA64 #160: ENABLE GC on this Linux CLI image.  Three knobs:
;;   (a) *linux-aarch64-gc-metadata-shl* t — store GC metadata <<1 (the latent
;;       raw-store bug that would halve every address once a collection fires).
;;   (b) *linux-aarch64-gc-midpoint* — semispace boundary.  Shrunk to 128MB
;;       (MODUS_GC_MIDPOINT hex override) so collections fire on a modest
;;       allocation (the 448MB default needs ~448MB/GC — too coarse to repro).
;;   (c) *linux-aarch64-r25-offset* = midpoint — x25 (alloc limit) = from-space
;;       end, so the gc-check trampoline fires instead of running off-space.
;; Trampoline + gc-check are already emitted (cross.lisp binds the labels for
;; :arch :aarch64); the boot publishes from/to/space_size/stack_base metadata.
(setf *linux-aarch64-gc-metadata-shl* t)
(setf *linux-aarch64-gc-midpoint*
      (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_GC_MIDPOINT")))
        (if (and v (> (length v) 0)) (parse-integer v :radix 16) #x08000000)))
(setf *linux-aarch64-r25-offset* *linux-aarch64-gc-midpoint*)
;; WS4-AA64 #160 Stage B: emit the object-start-bit SET at every alloc site so
;; gc.lisp's %gc-forward-slot / %gc-scan-copied can reject false roots.
(setf *aarch64-gc-bitmap-enabled* t)
;; WS4-AA64 #160 Stage 1: use the NATIVE Cheney collector (not the Lisp
;; %gc-collect path).  Allocation-free → can't re-enter; object-start-validated.
(setf *aarch64-gc-native-mcgc* t)
(format t "~%  AArch64 GC: ON (NATIVE MCGC)  midpoint=#x~X  metadata-shl=t  bitmap=t~%"
        *linux-aarch64-gc-midpoint*)
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
