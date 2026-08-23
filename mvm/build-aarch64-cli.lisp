;;;; build-aarch64-cli.lisp — the hosted Modus CLI for Linux/AArch64.
;;;;
;;;; A :linux-aarch64 ELF you run as an ordinary process; same SBCL-faithful
;;;; toplevel as x64's ./modus (argv + --eval/--load/--script/--quit + ~/.modusrc
;;;; + REPL), plus a set of baked WS4 JIT regression probes selected by a bare
;;;; numeric argv[1].
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-aarch64-cli.lisp
;;;;   -> /home/claude/modus-aa64-cli   (override with MODUS_CLI_OUT)
;;;;
;;;; STRUCTURE — CONVERGED WITH x64 (2026-08).  This wrapper is a thin AArch64
;;;; tail over mvm/build-cli-common.lisp, the SAME shared assembly
;;;; mvm/build-generic-cli.lisp uses.  It previously (a) loaded
;;;; build-ansi-common.lisp — the ANSI GATE-RUNNER harness, the wrong taxonomy
;;;; class for a shipping image (CLAUDE.md "Build taxonomy") — and (b) re-derived
;;;; the whole source set by hand.  Parity was maintained by comment, and every
;;;; drift was found as a production bug rather than by review (#245's missing
;;;; (init-all-globals), the absent RTEST that made arm library suites
;;;; unrunnable, the absent hosted socket/storage/HTTP layer).
;;;;
;;;; What this file may legitimately contain is AArch64 HARDWARE FACT and
;;;; nothing else.  A capability belongs in build-cli-common.lisp, where both
;;;; arches get it at once.

;;; ============================================================
;;; ARCH SLOTS — AArch64
;;; ============================================================

(defvar *cli-arch* :aarch64)

;;; ABI FACT, corrected: on the AArch64 generic ABI 93 is `exit' (calling
;;; THREAD only) and 94 is `exit_group' (the process).  This slot used 93, which
;;; is indistinguishable from 94 in a single-threaded image and a permanent HANG
;;; once a second thread exists: the leader becomes an unreapable zombie and a
;;; parked sibling is never woken.  x86-64 had the identical bug (60 vs 231).
;;;
;;; KNOWN BUG, NOT a divergence: every NONZERO exit this image produces is
;;; DOUBLED (`(sys-exit 1)' -> rc 2), because the value reaching the SVC is
;;; still tagged.  Probes 11111/11112/11113 bisect it: an inline literal and an
;;; inline let-variable both exit correctly, so the loss is specific to routing
;;; a DEFUN PARAMETER through compile-syscall3's operand shuffle on aarch64.
;;; x64 compiles the identical source correctly.  The fix belongs in
;;; translate-aarch64 and needs its own ANSI-gated session; exit 0 is
;;; unaffected, so success paths are correct.  The `(let ((c code)) ...)' rebind
;;; is kept identical to x64's shape -- dropping it was MEASURED not to help.
(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 94 c 0 0)))
(defun halt ()
  (syscall3 94 1 0 0))
")

;;; AArch64 diagnostic + WS4-JIT probe apparatus, baked ahead of kernel-main.
;;; Selected by a bare numeric argv[1]; a normal `modus --eval ...' run never
;;; enters probe mode and prints nothing extra.
(defvar *cli-arch-probe-source* "

;; Shared with x64 (build-generic-cli's probe slot): argv/argc off the fixed BSS
;; slots the boot preamble publishes, and native probes for the handler-frame
;; chain.  These must be NATIVE fns -- an INTERPRETED mem-ref only sees the
;; interpreter's simulated per-state memory hash, so only compiled code can read
;; the real RAM.  The BSS layout is identical on both arches, so this block is
;; here for PARITY (x64 registers these names in the SFT; arm did not).
(defun %argv-string-at (addr)
  (let ((len 0))
    (let ((i 0))
      (loop
        (let ((b (mem-ref (+ addr i) :u8)))
          (when (= b 0) (return nil))
          (setq i (+ i 1)))
        (setq len i)))
    (if (zerop len) nil
        (let ((s (%make-string-array len)) (i 0))
          (loop
            (when (>= i len) (return s))
            (aset s i (mem-ref (+ addr i) :u8))
            (setq i (+ i 1)))))))
(defun %argv1 () (%argv-string-at #x10000208))
(defun %argv2 () (%argv-string-at #x10000248))
(defun %argc  () (mem-ref #x10000200 :u32))
(defun %hc-depth () (mem-ref #x10000400 :u32))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u32) 0) nil t))
(defun %hc-frame-ip (n) (mem-ref (+ #x10000408 (* 32 n) 16) :u32))
(defun %hc-cur-ip () (mem-ref #x10000190 :u32))

;; WS5 #203: TRUE when argv[1] parses as a nonzero decimal, i.e. this run
;; selects one of the baked regression probes rather than the hosted CLI.
;; Everything the probe vehicle prints (the CLI-BOOT banner, the JIT self-test)
;; is gated on this so a plain `modus --eval FORM` writes only what FORM writes.
(defun %cli-probe-mode-p ()
  (> (%parse-decimal-at-fixed-208) 0))

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
        ;; WS5 #206: require the FN tag, matching the production path in
        ;; mvm-eval.lisp's %jit-translate-page-1-aarch64.  A RUNTIME-defined
        ;; callee is a heap closure (tag 9) and the heap has no PROT_EXEC.
        ;; This is a validation probe, so it MUST reject exactly what production
        ;; rejects — a probe that relocates more permissively than the code it
        ;; is validating reports success for cases that fault in production.
        (dolist (r crel)
          (let* ((name (gethash (cdr r) rt-table))
                 (fn (and name (%mvm-resolve-runtime-fn name)))
                 (word (if fn (%val->word fn) 0))
                 (addr (if (eql (logand word 15) 3) (- word 3) 0)))
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

;; WS4 aarch64 Stage 5 probe helpers.  The JIT gate itself is now the SHARED
;; *use-jit* that build-cli-common's *jit-boot-source* installs on both arches
;; -- this file used to carry its own *use-jit* + %jit-enabled-p pair, a
;; second gate that could disagree with the one production mvm-eval consults.

;; Stage-5 probe: eval FORM through the REAL mvm-eval seam twice — interpret
;; (jit off) then JIT (jit on) — compare, and report whether the JIT run took
;; the NATIVE path (native-count advanced) vs fell back to interpret.
(defun %s5-probe (label form)
  (setq *use-jit* nil)
  (let ((iv (handler-case (mvm-eval form) (t (c) (quote IERR))))
        (nc0 *jit-native-count*))
    (setq *use-jit* t)
    (let ((jv (handler-case (mvm-eval form) (t (c) (quote JERR)))))
      (setq *use-jit* nil)
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
  (let ((save *use-jit*))
    (setq *use-jit* nil)
    (let ((iv (handler-case (mvm-eval form) (t (c) (quote IERR))))
          (jv (handler-case (%jit-run-form form) (t (c) (quote JERR)))))
      (setq *use-jit* save)
      (if (eql iv jv)
          (progn (write-string-serial \"MATCH v=\") (%jit-pv iv))
          (progn (write-string-serial \"MISMATCH i=\") (%jit-pv iv)
                 (write-string-serial \" j=\") (%jit-pv jv)))))
  (write-char-serial 10))

")

;;; ARCH SLOT: hardware setup that must precede the FIRST allocation.
;;;
;;; Linux/AArch64 does not reliably zero a ~900MB BSS tail, and garbage in these
;;; runtime-metadata slots corrupts the global alist / handler frames.  The
;;; object-start bitmap must then be reserved BEFORE the first allocator call
;;; (init-symbol-table, immediately below) so every mutator allocation records
;;; its start bit.  %gc-bitmap-init is non-allocating.  x64 needs none of this:
;;; boot/boot-linux-x64.lisp has already zeroed these slots and the MCGC bitmap
;;; is reserved host-side.
(defvar *cli-arch-kernel-prologue* "
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

")

;;; ARCH SLOT: file-I/O scratch.  Identical to x64 -- both 64-bit ports park
;;; these just BELOW the heap base (0x10000000), inside the ELF's own mapped
;;; BSS tail.  i386 cannot: its heap is at 0x30000000 and 0x0FE00000 is
;;; unmapped there.
(defvar *cli-arch-io-scratch-source* "")

;;; ARCH SLOT: the toplevel entry / probe program.  This is the one place the
;;; two images are genuinely DIFFERENT PROGRAMS: x64's shipping CLI goes
;;; straight to cli-toplevel, while this image additionally carries the WS4
;;; AArch64 JIT self-test and the #160 GC probes, reached only via a bare
;;; numeric argv[1].  Kept separate deliberately -- these are AArch64 JIT
;;; bring-up instruments (MOVZ/MOVK quad relocation, a64-buffer translation,
;;; the aarch64 GC-poison repro) with no x64 counterpart, and folding them into
;;; the shared file would put arch-specific debug apparatus in every image.
(defvar *cli-arch-kernel-epilogue* "
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
  ;; WS5 #203: the whole self-test (steps 1-5) is now PROBE-MODE ONLY.  It used
  ;; to run unconditionally and wrote 16 lines to stdout on every boot — which
  ;; is fine for a probe vehicle but fatal for a hosted CLI (`modus --eval` must
  ;; emit exactly what the form prints, like SBCL).  Probe runs are unaffected:
  ;; every numeric-argv probe still gets the identical self-test prologue,
  ;; because %cli-probe-mode-p is true for all of them.
  (when (%cli-probe-mode-p)
  ;; WS5 #203: the JIT self-test is ABOUT the JIT, so it forces *use-jit* on
  ;; rather than inheriting the shipping default — which is now OFF (the #199
  ;; flip was reverted; see the big note at the top of this file).  This keeps
  ;; every downstream probe line (aa64s3/s4/s5, and the 44444 GC-poison probe
  ;; that runs after) byte-identical to the pre-revert binary; the ONLY probe
  ;; output that changes is the `cli-jit-default=' line, 1 -> 0, which is the
  ;; honest report of the new default.
  (setq *use-jit* t)
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
  (write-char-serial 10))
  ;; --- end PROBE-MODE-ONLY JIT self-test -----------------------------------

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
    (setq *use-jit* (eql (%parse-decimal-at-fixed-208) 777777))
    (write-string-serial \"BENCH-START jit=\") (print-dec (if *use-jit* 1 0)) (write-char-serial 10)
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
    (setq *use-jit* t)
    (let ((i 0))
      (loop
        (when (>= i 4000) (return nil))
        (when (eql (mod i 100) 0)
          (write-string-serial \"stress i=\") (print-dec i) (write-char-serial 10))
        ;; Distinct form each iteration (varying literal) → distinct bytecode →
        ;; distinct exec page → fresh ~1.7MB translation (no page-cache hit).
        (mvm-eval (list (quote +) i 1))
        (setq i (+ i 1))))
    (setq *use-jit* nil)
    (write-string-serial \"stress-SURVIVED-4000\") (write-char-serial 10))

  ;; (8) WS4 #160 Piece 2 DEFUN-RETENTION regression net (argv1 = 56667):
  ;;     define a fn via JIT (returns a SYMBOL → non-function result → its
  ;;     installer page IS reclaimed).  Drive many transient forms (freeing
  ;;     pages), then call the fn 100x.  If the fn's BODY had lived in the freed
  ;;     installer page, this would UAF-crash; defun-bad must be 0 (body is a
  ;;     SEPARATE module, built on first call).
  (when (eql (%parse-decimal-at-fixed-208) 56667)
    (setq *use-jit* t)
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
    (setq *use-jit* t)
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
    (setq *use-jit* t)
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

  ;; (10) ARGV APPARATUS probe (argv1 = 22222): validate the aarch64 arm of
  ;;      cli-toplevel BEFORE trusting anything built on it.  Prints argc, the
  ;;      raw %gc-stack-base read, the computed argv base, every argv[i] as
  ;;      collected by cli-toplevel's OWN %cli-collect-argv, and $HOME via
  ;;      %cli-getenv.  If %cli-argv-base's arch arm were wrong these would be
  ;;      garbage/empty rather than the shell's actual argv.
  (when (eql (%parse-decimal-at-fixed-208) 22222)
    (write-string-serial \"ARGVPROBE-START\") (write-char-serial 10)
    (write-string-serial \"argc=\") (print-dec (%cli-argc)) (write-char-serial 10)
    (write-string-serial \"stack-base=\") (print-dec (%gc-stack-base)) (write-char-serial 10)
    (write-string-serial \"argv-base=\") (print-dec (%cli-argv-base)) (write-char-serial 10)
    (let ((av (%cli-collect-argv)) (i 0))
      (loop
        (when (null av) (return nil))
        (write-string-serial \"argv[\") (print-dec i) (write-string-serial \"]=\")
        (write-string-serial (car av)) (write-char-serial 10)
        (setq av (cdr av)) (setq i (+ i 1))))
    (let ((h (%cli-getenv \"HOME\")))
      (write-string-serial \"HOME=\")
      (when h (write-string-serial h))
      (write-char-serial 10))
    (write-string-serial \"ARGVPROBE-END\") (write-char-serial 10)
    (sys-exit 0))

  ;; (11) EXIT-CODE bisect (argv1 = 11111/11112/11113).  The SBCL differential
  ;;      found `modus --eval (sys-exit 7)` exiting 14 on aarch64 while x64
  ;;      exits 7 — exactly 2n, i.e. the value reaching the SVC is still tagged.
  ;;      compile-syscall3 (arch-neutral) passes all four operands TAGGED and
  ;;      trap #x0502 untags them, so one of the three shapes below must break.
  ;;      Each variant exits with 3; the observed rc identifies the culprit:
  ;;        11111 inline literal        rc 3 = trap OK
  ;;        11112 inline let-variable   rc 3 = variable operand OK
  ;;        11113 through the sys-exit wrapper (a defun parameter)
  ;;      Run all three and compare; do NOT reason about it from the source.
  (when (eql (%parse-decimal-at-fixed-208) 11111)
    (syscall3 93 3 0 0))
  (when (eql (%parse-decimal-at-fixed-208) 11112)
    (let ((c 3)) (syscall3 93 c 0 0)))
  (when (eql (%parse-decimal-at-fixed-208) 11113)
    (sys-exit 3))

  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  ;; Anything that is NOT a numeric probe selector (i.e. argv[1] does not start
  ;; with a digit — every SBCL-style flag starts with '-', and no argument at
  ;; all leaves the fixed BSS zeroed) falls through to cli-toplevel, which
  ;; re-reads the FULL argv off the live initial stack and parses it SBCL-style.
  ;;
  ;; COLLISION NOTE: %parse-decimal-at-fixed-208 reads argv[1] only, and only
  ;; as a leading decimal.  So the ONLY shape that collides with SBCL flag
  ;; parsing is a bare positive-integer first argument (`modus 33333`), which
  ;; SBCL would treat as the start of the trailing args.  Flags (`--eval`,
  ;; `-e`, `--script`) all start with '-' and parse as 0; a bare `0` also
  ;; parses as 0 and reaches the toplevel.  Probe IDs are 5-6 digit constants,
  ;; so the collision is confined to those exact integers.
  ;; A probe that runs to completion WITHOUT its own (sys-exit) — 55555 is the
  ;; one — must still end at CLI-DONE, exactly as before this file grew a
  ;; toplevel.  Only a NON-probe run reaches cli-toplevel.
  (if (%cli-probe-mode-p)
      (progn (write-string-serial \"CLI-DONE\") (write-char-serial 10)
             (sys-exit 0))
      (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
  (sys-exit 0))
")

;;; ARCH SLOT: late last-defun-wins overrides, spliced right after the bridge.
;;;
;;; (1) FILE I/O.  cl-fileio.lisp hardcodes x86-64 syscall numbers (open=2,
;;;     stat=4, unlink=87, mkdir=83, rename=82).  The AArch64 generic ABI
;;;     DROPPED all of them in favour of the `*at' variants with a dirfd
;;;     argument, so on aarch64 every one hits a bogus/unimplemented number:
;;;     %sys-stat-exists returned NIL for a file that exists and LOAD failed
;;;     with FILE-ERROR for any path, making --load / --script / ~/.modusrc dead.
;;;
;;; (2) %CLI-ARGV-BASE.  lib/cli-toplevel.lisp turns %gc-stack-base into the
;;;     byte address of argv[0]'s stack slot.  boot-linux-x64 stores the initial
;;;     RSP RAW at 0x10000058, so a (mem-ref ... :u64) yields RSP/2 and the
;;;     shared file doubles it.  boot-linux-aarch64 stores stack_base through
;;;     `maybe-shl' (this build sets *linux-aarch64-gc-metadata-shl*), so the
;;;     word in memory is already SP<<1 and the same read yields the REAL SP --
;;;     doubling would land at 2*SP, far outside the mapped stack.  So the
;;;     aarch64 arm is exactly "do not double".  An argv[i]/envp[i] POINTER is
;;;     stored RAW by the kernel on BOTH arches, so the shared file's (* 2 ptr)
;;;     is already correct here.
(defvar *cli-arch-override-source* "
(defun %sys-open-rdonly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%aarch64-openat *cstr-scratch* 0 0))
(defun %sys-open-wronly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%aarch64-openat *cstr-scratch* 577 420))
(defun %sys-open-append (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%aarch64-openat *cstr-scratch* 1089 420))
(defun %sys-open-rdwr (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%aarch64-openat *cstr-scratch* 66 420))
(defun %sys-open-create-excl (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%aarch64-openat *cstr-scratch* 193 420))
(defun %sys-unlink (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%aarch64-unlinkat *cstr-scratch* 0 0))
(defun %sys-rename (old-str new-str)
  (%string-to-cstr old-str *cstr-scratch*)
  (let ((new-addr (+ *cstr-scratch* 2048)))
    (%string-to-cstr new-str new-addr)
    (%aarch64-renameat *cstr-scratch* new-addr 0)))
(defun %sys-mkdir (path-str mode)
  (%string-to-cstr path-str *cstr-scratch*)
  (%aarch64-mkdirat *cstr-scratch* mode 0))
(defun %sys-stat-size (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (%aarch64-newfstatat path-addr buf-addr 0)))
      (if (< ret 0)
          -1
          ;; struct stat on AArch64 differs from x86-64 layout —
          ;; st_size is at offset 48 in both, so the same load works.
          (mem-ref (+ buf-addr 48) :u32)))))
(defun %sys-stat-exists (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (%aarch64-newfstatat path-addr buf-addr 0)))
      (if (< ret 0) nil t))))
(defun %sys-stat-mtime (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (%aarch64-newfstatat path-addr buf-addr 0)))
      (if (< ret 0)
          0
          (mem-ref (+ buf-addr 88) :u32)))))


(defun %cli-argv-base ()
  (+ (%gc-stack-base) 8))
")

(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

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

(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (plusp (length sm)))
    (setf modus.mvm::*write-symmap-path* sm)))

(let ((image (build-image :target :linux-aarch64 :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_CLI_OUT")
                  "/home/claude/modus-aa64-cli")))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    (when (sb-ext:posix-getenv "MODUS_DUMP_NATIVE")
      (with-open-file (o (concatenate 'string path ".native")
                         :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
        (write-sequence (kernel-image-native-code image) o))
      (format t "  native code: ~D bytes -> ~A.native~%"
              (length (kernel-image-native-code image)) path))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (when (string= path "/home/claude/modus-aa64-cli")
      (format t "~%NOTE: wrote the SHARED default path.  Set MODUS_CLI_OUT for any~%      gate or comparison build — the default is outside the worktree, so~%      two agents building at once overwrite each other.~%"))
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)))
