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

;; WS4-AA64 FLIP: honor the same MODUS_USE_JIT / MODUS_NO_JIT knob the gate uses
;; (*aarch64-jit-on*, computed in build-ansi-common-aarch64.lisp) as the CLI's
;; DEFAULT runtime-JIT state.  Baked as %cli-jit-default; kernel-main seeds
;; *cli-jit-on* from it.  The Stage 3/4/5 probes still force their own on/off
;; around each form so the differential stays rigorous regardless of the default.
(defvar *cli-jit-default-source*
  (format nil "(defun %~A-jit-default () ~A)~%" "cli"
          (if *aarch64-jit-on* "t" "nil")))
(format t "  CLI default runtime JIT: ~A~%" (if *aarch64-jit-on* "ON" "OFF"))

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

  ;; (6) GC-OFF EXHAUSTION STRESS (diagnostic for the FLIP decision): JIT many
  ;;     DISTINCT forms in a NON-forked loop (like run-all-tests / a long-lived
  ;;     REPL).  GC is OFF on aarch64-linux, so each ~1.5-1.7MB translation
  ;;     accumulates.  Heartbeat every 50 forms; the last one printed before the
  ;;     process dies marks where the 896MB heap exhausts.  Runs only when argv1
  ;;     = 55555 (so the normal CLI run isn't destroyed by it).
  (when (eql (%parse-decimal-at-fixed-208) 55555)
    (setq *cli-jit-on* t)
    (let ((i 0))
      (loop
        (when (>= i 4000) (return nil))
        (when (eql (mod i 50) 0)
          (write-string-serial \"stress i=\") (print-dec i) (write-char-serial 10))
        ;; Distinct form each iteration (varying literal) → distinct bytecode →
        ;; distinct exec page → fresh ~1.7MB translation (no page-cache hit).
        (mvm-eval (list (quote +) i 1))
        (setq i (+ i 1))))
    (setq *cli-jit-on* nil)
    (write-string-serial \"stress-SURVIVED-4000\") (write-char-serial 10))

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
