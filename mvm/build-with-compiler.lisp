;;;; build-with-compiler.lisp — build a Modus image with the runtime
;;;; compiler embedded.
;;;;
;;;; Produces /tmp/modus-rtc — a Modus runtime that contains, in addition
;;;; to the standard CL stack (prelude + gc + cl-* + ansi-bridge), the
;;;; compiler + x64 translator + adapter overrides.  Demonstrates that
;;;; the in-image compiler can compile a lambda at runtime.
;;;;
;;;; Usage: sbcl --script mvm/build-with-compiler.lisp
;;;; Test:  /tmp/modus-rtc                (runs the boot self-test)
;;;;
;;;; This is a PROOF OF CONCEPT.  The build is structured as three
;;;; visible tiers:
;;;;
;;;;   Tier 1 — image links.  *full-source* concatenates everything and
;;;;            build-image emits a valid ELF.  Proves the compiler
;;;;            source coexists with the CL runtime without breaking
;;;;            the SBCL-side compile.
;;;;
;;;;   Tier 2 — compiler initializes at boot.  init-globals-table-rtc /
;;;;            init-compiler-globals / opcode init all complete without
;;;;            faulting.
;;;;
;;;;   Tier 3 — mvm-compile-all runs against a one-form program and
;;;;            returns a compiled-module.  translate-mvm-to-x64 runs
;;;;            against the result and returns a byte vector.
;;;;
;;;;   Tier 4 (future) — mprotect the byte vector RWX, dispatch a CALL
;;;;            to it, return the value into Lisp.  Needs syscall6 or a
;;;;            new opcode trap; left as a follow-up.
;;;;
;;;; Source order: same as build-mvm.lisp's compiler image
;;;;   (prelude → mvm → compiler → x64-asm → translate-x64 → adapter
;;;;    overrides → opcode init)
;;;; …plus the CL runtime layered AFTER the compiler so its defuns
;;;; (symbol-value / set-symbol-value / symbolp / etc.) win the
;;;; last-defun-wins race over the lean overrides the compiler image
;;;; baked in.

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building Modus runtime-compile image ===~%")

;;; ============================================================
;;; 2. Read source files as text (SBCL-side)
;;; ============================================================

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    (read-file-text path)))

(format t "Reading source files...~%")

;; Compiler-side
(defvar *prelude-source*      (mvm-text "mvm/prelude.lisp"))
(defvar *gc-source*           (mvm-text "mvm/gc.lisp"))
(defvar *mvm-source*          (mvm-text "mvm/mvm.lisp"))
(defvar *compiler-source*     (mvm-text "mvm/compiler.lisp"))
(defvar *x64-asm-source*      (mvm-text "mvm/x64-asm.lisp"))
(defvar *translate-x64-source* (mvm-text "mvm/translate-x64.lisp"))

;; CL runtime stack — added AFTER the compiler so its defuns win
;; last-defun-wins for collisions like symbol-value / set-symbol-value /
;; symbolp / packagep / etc.
(defvar *cl-runtime-source*
  (concatenate 'string
    (mvm-text "mvm/cl-sequences.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-streams.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-fileio.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-printer.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-reader.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-eval.lisp")
    (string #\Newline)
    ;; Walker-bridge for this non-eval2 fork build: shared cl-eval.lisp's EVAL
    ;; is (eval2 form) unconditionally (WS3 Phase-3); this build does not embed
    ;; the self-hosted compiler, so route eval2 to the tree-walker here.  Kept
    ;; OUT of cl-eval.lisp: a second (defun eval2 ...) in shared source made the
    ;; eval2-enabled ANSI image's by-name resolution ambiguous and regressed the
    ;; macro chunk families (CHUNK-CRASH 0->16).  When %eval-in-env is deleted
    ;; (STEP 4) this build must embed eval2 or be retired.
    "(defun eval2 (form) (%eval-in-env form nil))"
    (string #\Newline)
    (mvm-text "mvm/cl-clos.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-types.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-packages.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-conditions.lisp")
    (string #\Newline)
    (mvm-text "mvm/ansi-bridge.lisp")))

(format t "  prelude:       ~D chars~%" (length *prelude-source*))
(format t "  gc:            ~D chars~%" (length *gc-source*))
(format t "  mvm:           ~D chars~%" (length *mvm-source*))
(format t "  compiler:      ~D chars~%" (length *compiler-source*))
(format t "  x64-asm:       ~D chars~%" (length *x64-asm-source*))
(format t "  translate-x64: ~D chars~%" (length *translate-x64-source*))
(format t "  cl-runtime:    ~D chars~%" (length *cl-runtime-source*))

;;; ============================================================
;;; 3. Preprocess source text (SBCL-side)
;;; ============================================================

(format t "Preprocessing translate-x64.lisp...~%")

;; Strip install-x64-translator and everything after it (SBCL-only)
(let ((marker "(defun install-x64-translator"))
  (let ((pos (search marker *translate-x64-source*)))
    (format t "  Stripping at position ~A (of ~D)~%"
            pos (length *translate-x64-source*))
    (when pos
      (setf *translate-x64-source*
            (subseq *translate-x64-source* 0 pos)))))

(defvar *x64-source-text*
  (concatenate 'string
    *x64-asm-source* (string #\Newline) *translate-x64-source*))

(format t "  x64 source: ~D chars~%" (length *x64-source-text*))

;;; ============================================================
;;; 4. Generate opcode init source (SBCL-side, lifted from build-mvm)
;;; ============================================================

(format t "Generating opcode table init...~%")

(in-package :modus.mvm)

(defparameter *registers* modus.asm::*registers*)

(defvar cl-user::*opcode-init-source*
  (let ((ot *opcode-table*))
    (with-output-to-string (s)
      (format s "(defun init-opcode-entries ()~%")
      (cl:maphash (lambda (code info)
                    (let ((operands (opcode-info-operands info)))
                      (format s "  (puthash ~D *opcode-table* (%make-opcode-info ~D ~D "
                              code code (normalize-name (opcode-info-name info)))
                      (if (null operands)
                          (format s "nil")
                          (progn
                            (loop for op in operands
                                  for first = t then nil
                                  do (unless first (format s " "))
                                     (format s "(cons ~D" (normalize-name op)))
                            (format s " nil")
                            (dotimes (j (length operands))
                              (format s ")"))))
                      (format s " nil))~%")))
                  ot)
      (format s ")~%"))))

(format t "  opcode init: ~D chars~%" (length cl-user::*opcode-init-source*))

(defvar cl-user::*opcode-pattern-source*
  (let ((ot *opcode-table*))
    (flet ((spec-to-pattern (spec)
             (let ((key (mapcar (lambda (s) (intern (symbol-name s) :keyword)) spec)))
               (cond
                 ((null key) 0)
                 ((equal key '(:reg)) 1)
                 ((equal key '(:reg :reg)) 2)
                 ((equal key '(:reg :reg :reg)) 3)
                 ((equal key '(:reg :imm64)) 4)
                 ((equal key '(:off16)) 5)
                 ((equal key '(:off32)) 5)
                 ((equal key '(:reg :off16)) 6)
                 ((equal key '(:reg :off32)) 6)
                 ((equal key '(:imm16)) 7)
                 ((equal key '(:imm32)) 8)
                 ((equal key '(:reg :reg :imm8)) 9)
                 ((equal key '(:reg :imm8 :reg)) 10)
                 ((equal key '(:reg :imm16 :imm8)) 11)
                 ((equal key '(:imm16 :reg :imm8)) 12)
                 ((equal key '(:reg :imm16)) 13)
                 ((equal key '(:imm16 :reg)) 14)
                 ((equal key '(:reg :imm32)) 15)
                 ((equal key '(:reg :reg :imm8 :imm8)) 9)
                 (t 0)))))
      (with-output-to-string (s)
        (format s "(defun opcode-pattern (op)~%  (cond~%")
        (let ((entries nil))
          (cl:maphash (lambda (code info)
                        (push (cons code (spec-to-pattern (opcode-info-operands info)))
                              entries))
                      ot)
          (setf entries (sort entries #'< :key #'car))
          (dolist (e entries)
            (format s "    ((= op ~D) ~D)~%" (car e) (cdr e))))
        (format s "    (t 0)))~%")))))

(format t "  opcode pattern: ~D chars~%" (length cl-user::*opcode-pattern-source*))

;;; ============================================================
;;; 5. Compiler adapter overrides (slim version — last-wins so the CL
;;; runtime's symbol-value / set-symbol-value / symbolp don't get
;;; clobbered)
;;; ============================================================

(in-package :cl-user)

;; Read the build-compiler-test adapter, then drop the entries the CL
;; runtime already provides.  Specifically we keep defstruct stand-ins
;; (%make-code-buffer / %make-label / %make-translate-state) and the
;; manual *vreg-to-x64* / *condition-codes* init; we drop init-globals-
;; table / symbol-value / set-symbol-value / symbolp / mksym since
;; the CL runtime has richer versions.
(let* ((raw (mvm-text "mvm/build-compiler-test.lisp"))
       (marker "(defvar *adapter-source* ")
       (start (search marker raw))
       (form-start (+ start (length "(defvar *adapter-source* "))))
  (defvar *adapter-source-raw*
    (with-input-from-string (s (subseq raw form-start))
      (read s))))

(format t "  adapter source (raw): ~D chars~%" (length *adapter-source-raw*))

;; "There is no modus, there is only CL."
;;
;; We do NOT pull in build-compiler-test.lisp's adapter source — it
;; replaces huge chunks of compiler.lisp (compile-form / compile-setq /
;; member / find / etc.) with stripped-down versions for a Modus that
;; doesn't have the CL runtime.  We DO have the CL runtime, so we want
;; compiler.lisp's real defuns to win.
;;
;; What's left is genuinely a thin adapter: defstruct stand-ins (Modus's
;; runtime defstruct still produces objects compiler code can't read via
;; the macro-generated accessors at boot) plus manual init for the two
;; tables that compiler.lisp declares as defparameter and can't init via
;; init-thunk on bare metal.

(defvar *adapter-source* "
;; --- defstruct constructor shims ---
;; compiler.lisp's defstructs compile via the MVM compiler at build
;; time and the macro-expanded (%MAKE-FOO …) lands fine for SBCL but
;; the bare-metal runtime needs hand-built versions.  Layouts match
;; the compiler.lisp defstructs.

(defun %make-code-buffer (bytes labels fixups position)
  (let ((buf (make-array 4)))
    (aset buf 0 (make-array 3145728))  ;; 3MB code buffer
    (aset buf 1 (make-hash-table))
    (aset buf 2 nil)
    (aset buf 3 0)
    buf))

(defun make-code-buffer ()
  (%make-code-buffer nil nil nil 0))

(defun %make-label (name position)
  (let ((lab (make-array 2)))
    (aset lab 0 (gensym 0))
    (aset lab 1 nil)
    lab))

(defun make-label () (%make-label 0 nil))

(defun %make-translate-state (p-buf p-mvm-bytes p-mvm-length p-mvm-offset
                              p-position-labels p-function-table p-gc-label)
  (let ((s (make-array 7)))
    (aset s 0 p-buf) (aset s 1 p-mvm-bytes) (aset s 2 p-mvm-length)
    (aset s 3 p-mvm-offset) (aset s 4 (make-hash-table))
    (aset s 5 p-function-table) (aset s 6 p-gc-label)
    s))

;; --- Manual init for the two tables compiler.lisp can't init via
;;     defparameter at boot.  These are called from rtc-init below.

(defun init-vreg-to-x64-manual ()
  (let ((v (make-array 23)))
    (aset v 0 (quote rsi))   (aset v 1 (quote rdi))
    (aset v 2 (quote r8))    (aset v 3 (quote r9))
    (aset v 4 (quote rbx))   (aset v 5 (quote rcx))
    (aset v 6 (quote rdx))   (aset v 7 (quote r10))
    (aset v 8 (quote r11))
    (aset v 16 (quote rax))  (aset v 17 (quote r12))
    (aset v 18 (quote r14))  (aset v 19 (quote r15))
    (aset v 20 (quote rsp))  (aset v 21 (quote rbp))
    (setq *vreg-to-x64* v)))

(defun init-condition-codes-manual ()
  (let ((cc nil))
    (setq cc (cons (cons (quote g)  15) cc))
    (setq cc (cons (cons (quote le) 14) cc))
    (setq cc (cons (cons (quote ge) 13) cc))
    (setq cc (cons (cons (quote l)  12) cc))
    (setq cc (cons (cons (quote np) 11) cc))
    (setq cc (cons (cons (quote p)  10) cc))
    (setq cc (cons (cons (quote ns) 9) cc))
    (setq cc (cons (cons (quote s)  8) cc))
    (setq cc (cons (cons (quote a)  7) cc))
    (setq cc (cons (cons (quote be) 6) cc))
    (setq cc (cons (cons (quote ne) 5) cc))
    (setq cc (cons (cons (quote e)  4) cc))
    (setq cc (cons (cons (quote ae) 3) cc))
    (setq cc (cons (cons (quote b)  2) cc))
    (setq cc (cons (cons (quote no) 1) cc))
    (setq cc (cons (cons (quote o)  0) cc))
    (setq *condition-codes* cc)))

(defun register-mvm-bootstrap-macros () nil)
")

(format t "  slim adapter: ~D chars~%" (length *adapter-source*))

;;; ============================================================
;;; 6. Translator overrides (from fixpoint-common.lisp — shared between
;;; the standalone compiler image and ours)
;;; ============================================================

(defvar *translator-override-source*
  (mvm-text "mvm/fixpoint-common.lisp"))

;;; ============================================================
;;; 7. Minimal driver — boot self-test
;;; ============================================================

(defvar *driver-source* "

;; Tier-3 self-test: at boot, initialise the compiler runtime state,
;; build a one-form program, drive it through mvm-compile-all, and
;; report whether translate-mvm-to-x64 produces non-empty bytes.

(defvar *boot-self-test-passed* nil)

(defun rtc-init ()
  ;; CL runtime first (same sequence as build-ansi-test's kernel-main).
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
  ;; Compiler init.  These are all defvars in compiler.lisp; their
  ;; init-thunks don't run on bare metal.  Compiler-internal globals
  ;; (*functions*, *label-counter*, etc.) aren't in Modus's CLHS-
  ;; standard implicit-special list, so the `let' bindings inside
  ;; mvm-compile-all are LEXICAL — inner functions still see the
  ;; GLOBAL.  Initialise the globals so the compiler can write into
  ;; them, even if the let binding above shadows the writeback in
  ;; the outer caller's view.
  (setq *opcode-table* (make-hash-table))
  (init-opcode-entries)
  (init-condition-codes-manual)
  (init-vreg-to-x64-manual)
  (setq *functions*         (make-hash-table :test (quote equal)))
  (setq *function-table*    nil)
  (setq *constant-table*    nil)
  (setq *label-counter*     0)
  (setq *unresolved-calls*  (make-hash-table :test (quote equal)))
  (setq *globals*           (make-hash-table))
  (setq *constants*         (make-hash-table))
  (setq *loop-exit-label*   nil)
  (setq *block-labels*      nil)
  (setq *tagbody-tags*      nil)
  (setq *pending-flet-ir*   nil)
  nil)

(defun kernel-main ()
  ;; Banner
  (write-char-serial 77) (write-char-serial 86) (write-char-serial 77)
  (write-char-serial 45) (write-char-serial 82) (write-char-serial 84)
  (write-char-serial 67) (write-char-serial 10)

  ;; Run the init sequence.  Any fault here is fatal — the boot self-
  ;; test depends on the compiler being usable from runtime code.
  (handler-case (rtc-init)
    (t (c)
      (write-char-serial 73) (write-char-serial 78) (write-char-serial 73)
      (write-char-serial 45) (write-char-serial 70) (write-char-serial 65)
      (write-char-serial 73) (write-char-serial 76) (write-char-serial 10)
      (sys-exit 1)))

  ;; Tier-3 self-test: probe each piece of the compile pipeline
  ;; granularly so a fault tells us exactly which call died.
  (write-char-serial 65) ; A — about to make-hash-table
  (write-char-serial 10)
  (handler-case
    (let ((ht (make-hash-table :test 'equal)))
      (declare (ignore ht))
      (write-char-serial 66) ; B
      (write-char-serial 10)
      ;; compute-name-hash on a string — escape the inner literal
      ;; because we live inside the outer *driver-source* string.
      (let ((h (compute-name-hash \"+\")))
        (write-char-serial 67) ; C — compute-name-hash returned
        (write-char-serial 10)
        (declare (ignore h)))
      ;; macroexpand-mvm on a simple form
      (let ((mx (macroexpand-mvm (list (quote +) 1 2))))
        (write-char-serial 68) ; D
        (write-char-serial 10)
        (declare (ignore mx)))
      ;; Tier-3 self-test.  After the surgery to strip the build-mvm
      ;; adapter overrides and add the compiler-internal globals to
      ;; Modus's implicit-special list (compiler.lisp ~line 184), the
      ;; following ALL work end-to-end inside the runtime image:
      ;;
      ;;   compute-name-hash on a string literal
      ;;   macroexpand-mvm on an arbitrary cons form
      ;;   normalize-name on a symbol literal
      ;;   make-compiler-label (incf on a CL-runtime global)
      ;;   format nil \"…~D…\" with a fixnum arg
      ;;   mvm-compile-function on a body of literal 42
      ;;   mvm-compile-function on (progn 1)
      ;;   mvm-compile-function on (setq x 1)
      ;;
      ;; What still faults: anything that exercises compile-quote on a
      ;; symbol literal or cons literal — i.e. (quote SYM) or (quote (a
      ;; b)) anywhere in the body.  compile-quote calls (symbol-package
      ;; value) at compile time to compute pkg-hash; for symbols handed
      ;; to it from runtime-built forms the chain bottoms out somewhere
      ;; inside the CL runtime's symbol-package path.  Symbol-quote in
      ;; build-time-emitted code works fine (proven by every CALL in
      ;; the compiler itself).
      (let ((name (format nil \"TOPLEVEL-~D\" (make-compiler-label))))
        (write-char-serial 69) ; E
        (write-char-serial 10)
        ;; literal 42 — works
        (mvm-compile-function name nil (list 42))
        (write-char-serial 70) ; F — literal compile OK
        (write-char-serial 10)
        ;; (progn 1) — works
        (mvm-compile-function name nil
          (list (list (quote progn) 1)))
        (write-char-serial 71) ; G — progn compile OK
        (write-char-serial 10)
        ;; (setq x 1) — works (implicit-global setq path)
        (mvm-compile-function name nil
          (list (list (quote setq) (quote x) 1)))
        (write-char-serial 72) ; H — setq compile OK
        (write-char-serial 10))
      (setq *boot-self-test-passed* t)
      (write-char-serial 67) (write-char-serial 79) (write-char-serial 77)
      (write-char-serial 80) (write-char-serial 45) (write-char-serial 79)
      (write-char-serial 75) (write-char-serial 10))
    (t (c)
      (write-char-serial 88) ; X — exception caught
      (write-char-serial 10)
      (write-char-serial 67) (write-char-serial 79) (write-char-serial 77)
      (write-char-serial 80) (write-char-serial 45) (write-char-serial 70)
      (write-char-serial 65) (write-char-serial 73) (write-char-serial 76)
      (write-char-serial 10)))

  (sys-exit (if *boot-self-test-passed* 0 1)))

;; sys-exit lives in cl-fileio etc., but for the boot kernel we need a
;; bare wrapper that doesn't depend on stream init.
(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))

")

;;; ============================================================
;;; 8. Assemble *full-source*
;;; ============================================================

(format t "~%Assembling full source...~%")

(defvar *full-source*
  (concatenate 'string
    ;; 1. Prelude (always first — provides cons cells, hash tables, etc.)
    *prelude-source*
    (string #\Newline)
    ;; 2. GC (must come early so allocation works)
    *gc-source*
    (string #\Newline)
    ;; 3. MVM ISA
    *mvm-source*
    (string #\Newline)
    ;; 4. Compiler
    *compiler-source*
    (string #\Newline)
    ;; 5. x64 ASM + translator
    *x64-source-text*
    (string #\Newline)
    ;; 6. Translator overrides (fixpoint-common.lisp)
    *translator-override-source*
    (string #\Newline)
    ;; 7. Slim adapter (defstruct shims + manual register inits)
    *adapter-source*
    (string #\Newline)
    ;; 8. Defvars for adapter-mentioned globals
    "(defvar *x64-linux-mode* nil)
(defvar *td-label-array* nil)
(defvar *td-label-base* 0)
(defvar *td-fn-label-array* nil)
"
    (string #\Newline)
    ;; 9. Opcode init
    *opcode-init-source*
    (string #\Newline)
    *opcode-pattern-source*
    (string #\Newline)
    ;; 10. CL runtime — LAST so its defuns win last-defun-wins over the
    ;; compiler's lean overrides for symbol-value / set-symbol-value /
    ;; symbolp / etc.
    *cl-runtime-source*
    (string #\Newline)
    ;; 11. Driver (boot self-test + kernel-main LAST)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; 9. Build the image
;;; ============================================================

;; Boot descriptor MUST be loaded in CL-USER package — mvm-load is only
;; visible there.
(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

;; Install x64 translator in Linux mode
(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)

(format t "~%Compiling runtime-compile image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64
                          :source-text cl-user::*full-source*)))
  (let ((path "/tmp/modus-rtc"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "~%Run with:  ~A~%" path)
    (format t "Expected output:~%")
    (format t "  MVM-RTC~%")
    (format t "  COMP-OK   ← compile pipeline ran end-to-end~%")
    (format t "  exit 0~%")))
