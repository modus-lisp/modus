;;;; build-generic-cli.lisp — the canonical HOSTED Modus image (`./modus').
;;;;
;;;; A native ELF you run as an ordinary Linux process; it drops into Modus's
;;;; own self-hosted CL REPL on stdin (eval = mvm-eval = compile -> MVM bytecode ->
;;;; interpret) and parses SBCL-style toplevel flags.  It is `build-generic'
;;;; (full CL runtime, no baked ANSI tests) plus ONE baked file — the shared,
;;;; SBCL-faithful CLI toplevel:
;;;;   - lib/cli-toplevel.lisp    (argv + SBCL flags + ~/.modusrc + %cli-repl)
;;;;
;;;; NO quicklisp is baked in.  Exactly like stock SBCL — which has no `ql'
;;;; until you install Quicklisp and its ~/.sbclrc loads quicklisp/setup.lisp —
;;;; `ql:quickload' is a LOADABLE setup you pull in at runtime:
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp # → ./modus
;;;;   ./modus
;;;;   > (load "modus-quicklisp/setup.lisp")   ; the "install quicklisp" step
;;;;   > (ql:quickload :sha1)                  ; loads systems/sha1.tar (offline)
;;;;   > (sha1:sha1-hex "abc")                 ; => "A9993E36...9CD0D89D"
;;;;
;;;; setup.lisp itself (load)s lib/tar.lisp + lib/install-tarball.lisp and
;;;; defines the QL package over them — the Quicklisp-client-loads-its-own-
;;;; source model.  See modus-quicklisp/setup.lisp and QUICKLOAD.md.
;;;;
;;;; Output binary name is `modus' (override with MODUS_CLI_OUT).  This image is
;;;; a clean CLI with NO `ql' symbol present until setup is loaded.

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building generic Modus image ===~%")

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
    ;; #211: wrap each file so its own (in-package …) cannot leak into the
    ;; next file of the concatenated build blob.  See
    ;; modus.mvm::*build-package-reset-text*.
    (modus.mvm::%build-package-scoped-source (read-file-text path))))

;; Strip `chipz::' / `chipz:' package qualifiers from a source string so the
;; flat-namespace image reader doesn't error `Package CHIPZ does not exist'
;; (which would silently drop the whole enclosing form).  Longer prefix first.
;; Mirrors build-aarch64.lisp's strip-package-prefixes.  Used ONLY for baking
;; lib/install-tarball.lisp — see the *bridge-source* note.
(defun %cli-strip-one-prefix (text pfx)
  (let ((result text))
    (loop
      (let ((pos (search pfx result)))
        (unless pos (return result))
        (setf result (concatenate 'string
                                  (subseq result 0 pos)
                                  (subseq result (+ pos (length pfx)))))))))

(defun %cli-strip-chipz (text)
  (%cli-strip-one-prefix (%cli-strip-one-prefix text "chipz::") "chipz:"))

(format t "Reading source files...~%")

(defvar *prelude-source*  (mvm-text "mvm/prelude.lisp"))
(defvar *gc-source*       (mvm-text "mvm/gc.lisp"))
;; MCGC stage-4d pin API + pin-stress probe.  Included ONLY when
;; MODUS_MCGC_PINNING=1; flag-off omits it (byte-identical to canonical).
(defvar *mcgc-pin-source*
  (let ((v (sb-ext:posix-getenv "MODUS_MCGC_PINNING")))
    (if (and v (plusp (length v)) (not (string= v "0")))
        (concatenate 'string (string #\Newline)
                     (mvm-text "mvm/mcgc-pin.lisp") (string #\Newline))
        "")))
(defvar *rt-source*       (mvm-text "mvm/rt.lisp"))
;; STAGE 1 of retiring the tree-walker: ship the MVM ISA + bytecode
;; interpreter into the image so `eval` can eventually = compile→interpret
;; (one semantics, shared with the compiler) rather than the divergent
;; tree-walker.  mvm.lisp = opcode/vreg constants + structs; interp.lisp =
;; the bytecode executor (mvm-interpret).  interp.lisp uses #.+op-nop+
;; read-time eval, resolved by the readers binding *package* to :modus.mvm
;; (cross.lisp check-parses / read-all-forms-with-locations).
(defvar *isa-source*      (mvm-text "mvm/mvm.lisp"))
;; WS5 CODE-BUFFER GC-CORRUPTION FIX (2026-07-19): keep the mvm-buffer at a
;; size that NEVER GROWS during the self-compile.  Like the x64-asm code
;; buffer, the 64KB shrink forced doubling grows, and each grow's stale
;; local-across-make-array/GC-move corrupts the bytecode → wrong native code
;; for functions compiled after the grow → hang (init-keyword-table deadloop).
;; The old "128MB blows the 448MB semispace" reasoning is STALE (pre-u8-packing
;; #183; post-#183 a 128MB u8 array is 128MB, and the semispace is now 896MB).
;; Keep the original 128MB single up-front alloc → ZERO grows → no corruption.
(let ((needle "(bytes (make-array 134217728 :element-type '(unsigned-byte 8)))")
      (repl   "(bytes (make-array 1048576 :element-type '(unsigned-byte 8)))"))
  (let ((p (search needle *isa-source*)))
    (unless p (error "WS5: could not find mvm-buffer 128MB default to shrink"))
    (setf *isa-source*
          (concatenate 'string
                       (subseq *isa-source* 0 p) repl
                       (subseq *isa-source* (+ p (length needle)))))))
(defvar *interp-source*   (mvm-text "mvm/interp.lisp"))
;; STAGE 2: the MVM compiler itself, so (mvm-compile-all forms) runs in-image.
(defvar *compiler-source* (mvm-text "mvm/compiler.lisp"))
;; In-image override of ieee-float-bits: the build-time version uses
;; sb-kernel:double-float-* (host-only).  In-image a float is a 2-slot boxed
;; object (hi32/lo32); read them directly.  Appended AFTER the compiler source
;; so it wins (last-defun).  Only matters for compiling FLOAT literals.
(defvar *stage2-float-override*
  ;; #201: ONE definition, in mvm/float-slot-overrides.lisp.  These accessors
  ;; encode the float object's SLOT LAYOUT; they used to be duplicated verbatim
  ;; in six build scripts, where a layout change that missed one copy would be
  ;; silent numeric corruption rather than a build error.  Read as raw text (no
  ;; MVM-TEXT package wrapper) so the blob is byte-identical to the old inline
  ;; string.
  (let ((p (merge-pathnames "mvm/float-slot-overrides.lisp" *modus-base*)))
    (modus.mvm::check-parses p)
    (concatenate 'string (string #\Newline) (read-file-text p))))
;; STAGE 2 probe: mvm-eval = compile form to MVM bytecode + interpret it.
;; Closed-world (calls only resolve within the compiled module) — fine for
;; pure-arithmetic forms whose ops inline to MVM opcodes.
;; UNIFIED (post-flip): mvm-eval-forms/mvm-eval now come from the CANONICAL
;; mvm/mvm-eval.lisp — the same source the ANSI image compiles — instead of the
;; stale inline copy this string used to carry.  Unification was blocked by
;; mvm-eval.lisp's `\"` extraction damage until the flip wave-3 fidelity fix
;; (77cea7c) repaired it; the file is now clean, directly mvm-text-readable.
(defvar *mvm-eval-canonical-source* (mvm-text "mvm/mvm-eval.lisp"))

;;; ============================================================
;;; WS5 STAGE 1: bake the x64 build TOOLING into the image as DEAD CODE.
;;;
;;; Adds the x64 native translator (x64-asm + translate-x64), the target
;;; descriptors (target.lisp), and the cross-compilation build pipeline
;;; (cross.lisp) + the linux-x64 boot descriptor/ELF wrapper (boot-linux-x64)
;;; so that BUILD-IMAGE and its whole dependency graph
;;; (compile-source-to-module → translate-module-to-native →
;;;  assemble-kernel-image → wrap-in-elf64-le → get-boot-descriptor →
;;;  find-target) SELF-COMPILE in-image and are runtime-callable.
;;;
;;; At this stage the tooling is DEAD CODE — nothing routes to build-image;
;;; the CLI REPL is unchanged.  The proven translator recipe is
;;; mvm/build-ansi-common.lisp (the :x64 arch block) (WS4 Stage 1); the build
;;; pipeline (target/cross/boot-desc) is new to WS5.
;;;
;;; Package handling: source is read by cross.lisp's READ-ALL-FORMS in
;;; :MODUS.MVM and the in-image compiler hashes every symbol by NAME
;;; (package-independent), so modus.mvm.x64::/modus.asm::/modus.mvm:: all
;;; resolve by name in the flat image namespace; (in-package …)/(defpackage …)
;;; forms are build-time no-ops.
;;; ============================================================

;;; --- x64 instruction encoder (modus.asm) ---
(defvar *x64-asm-source* (mvm-text "mvm/x64-asm.lisp"))
;; WS5 CODE-BUFFER GC-CORRUPTION FIX (2026-07-19): size the in-image code
;; buffer big enough to NEVER GROW.  The old 64KB shrink forced ~16
;; doubling grows (64KB→16MB), and each grow's `(let ((bytes …))
;; (make-array new-cap) (replace new-bytes bytes))` can trigger a GC that
;; MOVES the old buffer while a stale local/struct-slot reference is held →
;; the copy/emission truncates → all native code after the collection point
;; is lost (self-compile: code truncated at ~9.5MB, kernel-main zero'd →
;; SIGILL/hang).  The original "96MB = 768MB tagged, exhausts the 448MB
;; semispace" reasoning is STALE: post-u8-packing (#183) a 96MB u8 array is
;; 96MB, and the semispace is now 896MB.  The self-compile's native code is
;; ~15MB; a 32MB single up-front allocation covers it with NO grow → NO
;; grow-triggered GC on the buffer → no truncation.
(let ((needle "(bytes (make-array 100663296 :element-type '(unsigned-byte 8)))")
      (repl   "(bytes (make-array 1048576 :element-type '(unsigned-byte 8)))"))
  (let ((p (search needle *x64-asm-source*)))
    (unless p
      (error "WS5-S1: could not find code-buffer 96MB default to shrink"))
    (setf *x64-asm-source*
          (concatenate 'string
                       (subseq *x64-asm-source* 0 p) repl
                       (subseq *x64-asm-source* (+ p (length needle)))))))

;;; --- MVM-bytecode → x86-64 translator (modus.mvm.x64) ---
;; Truncate at install-x64-translator: the host-only tail references
;; *target-x86-64* / uses &key etc.  We reconstruct the target->translate-fn
;; wiring in *selfhost-target-coinit-source* below (replicating what
;; install-x64-translator sets: translate-fn / emit-prologue / emit-epilogue).
(defvar *translate-x64-source* (mvm-text "mvm/translate-x64.lisp"))
(let ((marker "(defun install-x64-translator"))
  (let ((pos (search marker *translate-x64-source*)))
    (unless pos
      (error "WS5-S1: could not find install-x64-translator strip marker"))
    (setf *translate-x64-source*
          ;; #211: re-append the package reset the trim just cut off.
          (concatenate 'string (subseq *translate-x64-source* 0 pos)
                       modus.mvm::*build-package-reset-text*))))

;; Translator co-init: defvar/defparameter init-thunks do NOT run at boot
;; (CLAUDE.md item 7), so the translator's three lookup tables (*registers*,
;; *condition-codes*, *vreg-to-x64*) and *x64-native-code-offset* must be
;; populated explicitly.  %init-x64-translator is called from kernel-main.
;; Verbatim from the WS4 recipe (build-ansi-common.lisp *x64-translator-coinit-source*).
(defvar *x64-translator-coinit-source* "
;; #211: read this replica in :MODUS.ASM, the package x64-asm.lisp itself
;; declares.  The image interns symbols PER PACKAGE (CLHS 11.1.2), so a
;; register name quoted here must be the SAME symbol reg-info's ASSOC sees in
;; x64-asm.lisp's own *REGISTERS* — MODUS.ASM::RBP, not MODUS.MVM::RBP.  Read
;; in MODUS.MVM this table silently mismatched and the JIT died with
;; Unknown-register RBP, falling back to interpret for EVERY form.
(in-package :modus.asm)
(defun %init-x64-translator ()
  (setq *registers*
        (list (list (quote rax)  0 64 nil) (list (quote rcx)  1 64 nil)
              (list (quote rdx)  2 64 nil) (list (quote rbx)  3 64 nil)
              (list (quote rsp)  4 64 nil) (list (quote rbp)  5 64 nil)
              (list (quote rsi)  6 64 nil) (list (quote rdi)  7 64 nil)
              (list (quote r8)   8 64 t)   (list (quote r9)   9 64 t)
              (list (quote r10) 10 64 t)   (list (quote r11) 11 64 t)
              (list (quote r12) 12 64 t)   (list (quote r13) 13 64 t)
              (list (quote r14) 14 64 t)   (list (quote r15) 15 64 t)
              (list (quote eax)  0 32 nil) (list (quote ecx)  1 32 nil)
              (list (quote edx)  2 32 nil) (list (quote ebx)  3 32 nil)
              (list (quote esp)  4 32 nil) (list (quote ebp)  5 32 nil)
              (list (quote esi)  6 32 nil) (list (quote edi)  7 32 nil)
              (list (quote r8d)  8 32 t)   (list (quote r9d)  9 32 t)
              (list (quote r10d) 10 32 t)  (list (quote r11d) 11 32 t)
              (list (quote r12d) 12 32 t)  (list (quote r13d) 13 32 t)
              (list (quote r14d) 14 32 t)  (list (quote r15d) 15 32 t)
              (list (quote al)   0 8 nil)  (list (quote cl)   1 8 nil)
              (list (quote dl)   2 8 nil)  (list (quote bl)   3 8 nil)
              (list (quote spl)  4 8 t)    (list (quote bpl)  5 8 t)
              (list (quote sil)  6 8 t)    (list (quote dil)  7 8 t)
              (list (quote r8b)  8 8 t)    (list (quote r9b)  9 8 t)
              (list (quote r10b) 10 8 t)   (list (quote r11b) 11 8 t)
              (list (quote r12b) 12 8 t)   (list (quote r13b) 13 8 t)
              (list (quote r14b) 14 8 t)   (list (quote r15b) 15 8 t)))
  (setq *condition-codes*
        (list (cons :o 0)  (cons :no 1)  (cons :b 2)   (cons :ae 3)
              (cons :e 4)   (cons :ne 5)  (cons :be 6)  (cons :a 7)
              (cons :s 8)   (cons :ns 9)  (cons :p 10)  (cons :np 11)
              (cons :l 12)  (cons :ge 13) (cons :le 14) (cons :g 15)
              (cons :z 4)   (cons :nz 5)  (cons :c 2)   (cons :nc 3)
              (cons :nae 2) (cons :nb 3)  (cons :nbe 7) (cons :na 6)
              (cons :nge 12)(cons :nl 13) (cons :ng 14) (cons :nle 15)))
  (let ((v (make-array 23)))
    (aset v 0 (quote rsi))  (aset v 1 (quote rdi))
    (aset v 2 (quote r8))   (aset v 3 (quote r9))
    (aset v 4 (quote rbx))  (aset v 5 (quote rcx))
    (aset v 6 (quote rdx))  (aset v 7 (quote r10))
    (aset v 8 (quote r11))
    ;; V9..V15 spill to the stack (no physical reg) and V22 (VPC) is not
    ;; mapped -- they MUST hold NIL so VREG-PHYS returns NIL for them.  make-array
    ;; zero-inits unwritten slots to FIXNUM 0 (GC safety), not NIL, so set them
    ;; explicitly.  Otherwise (vreg-phys 9) yields 0 and DEST-PHYS-OR-SCRATCH s
    ;; (or (vreg-phys v) +scratch-reg+) picks 0 (truthy), so emit-mov-reg-mem gets
    ;; register 0 and reg-info fails with Unknown register 0 on every spilled obj-ref.
    (aset v 9 nil)  (aset v 10 nil) (aset v 11 nil) (aset v 12 nil)
    (aset v 13 nil) (aset v 14 nil) (aset v 15 nil) (aset v 22 nil)
    (aset v 16 (quote rax)) (aset v 17 (quote r12))
    (aset v 18 (quote r14)) (aset v 19 (quote r15))
    (aset v 20 (quote rsp)) (aset v 21 (quote rbp))
    (setq *vreg-to-x64* v))
  (setq *x64-native-code-offset* 397)  ; WS5 modus3 FIX: linux-x64 boot preamble
  ;; is 397 bytes; install-x64-translator runs IN-IMAGE and is the authoritative
  ;; runtime setter (the host-only (setf … 397) at file tail doesn't persist —
  ;; limitation #7).  With 0 here, in-image --compile aligned fn file-offsets to
  ;; ≡0 mod 16, and the 397-byte preamble (≡13) shifted every RUNTIME entry to
  ;; nibble 0xd → OR-3 closure-ptr tags broke → local capturing-closure call
  ;; jumped mid-instruction → SEGV (the modus3 blocker).  397 → entries nibble 0.
  (setq *x64-linux-mode* t)
  ;; WS5 modus3 BUG#2 FIX (same limitation-#7 persistence class as the offset):
  ;; the host-side (setf *x64-gc-enabled* t / *linux-x64-r14-offset* midpoint /
  ;; *mcgc-kind-bitmap-enabled* t) at this file's tail run HOST-side only, so the
  ;; in-image --compile emitted modus2 with GC DISABLED (no gc-checks) and R14 at
  ;; the full heap END.  Result: modus2's own --compile of self-clean5 never
  ;; collects, accumulates all garbage IR, and R12 walks off the mmap end at
  ;; ~form 2402 → SIGSEGV (crash site varies: string-upcase / %fmt-integer —
  ;; whichever alloc crosses the line).  NOT a GC bug — GC was never emitted.
  ;; install-x64-translator is the authoritative in-image setter, so set them
  ;; here: R14 = midpoint (#x38000000) so gc-checks fire at half-heap and Cheney
  ;; copies the live IR into to-space, reclaiming the garbage.
  (setq *x64-gc-enabled* t)
  (setq *mcgc-kind-bitmap-enabled* t)
  ;; WS5 A/B: persist the kind-check-reject override into the in-image runtime
  ;; (limitation #7 — the host-side defvar/setf does NOT reach modus2's runtime,
  ;; so set it here where install-x64-translator authoritatively runs in-image).
  (setq *ws5-force-no-kindcheck* WS5_NOKCHECK_VALUE)
  (setq *linux-x64-r14-offset* #x38000000)
  t)
(in-package :modus.mvm)
")

;; WS5 A/B substitution: bake t/nil into the coinit source for the kind-check
;; override.  MODUS_WS5_NOKCHECK=1 -> the in-image translator co-init sets
;; *ws5-force-no-kindcheck* T (kind-check reject OFF in the emitted collector).
#+sbcl
(let ((val (let ((v (sb-ext:posix-getenv "MODUS_WS5_NOKCHECK")))
             (if (and v (string= v "1")) "t" "nil")))
      (needle "WS5_NOKCHECK_VALUE"))
  (let ((pos (search needle *x64-translator-coinit-source*)))
    (when pos
      (setf *x64-translator-coinit-source*
            (concatenate 'string
                         (subseq *x64-translator-coinit-source* 0 pos)
                         val
                         (subseq *x64-translator-coinit-source* (+ pos (length needle)))))
      (format t "~&;; WS5-NOKCHECK coinit substitution: *ws5-force-no-kindcheck* = ~A~%" val))))

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
;; a64-buffer's `code` slot defaults to 16M TAGGED slots (= 128 MB in-image,
;; not 64 MB — these are boxed words, not a u8 vector) and A64-EMIT DOUBLES
;; it on overflow, and each grow's replace-across-GC can move the buffer out
;; from under a stale local.  Rung 1 emits a trivial program (a few hundred
;; instructions), so 1M entries is a single up-front alloc that NEVER grows.
(let ((needle "(code (make-array 16777216))")
      (repl   "(code (make-array 1048576))"))
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
                       modus.mvm::*build-package-reset-text*))))

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
  t)
")

(defvar *selfhost-tooling-source*
  (concatenate 'string
    *x64-asm-source*                (string #\Newline)
    *translate-x64-source*          (string #\Newline)
    *x64-translator-coinit-source*  (string #\Newline)
    ;; #210 rung 1: the aarch64 translator + its co-init go here, BEFORE
    ;; target/cross/boot-desc, for the same reason the x64 pair does — the
    ;; translator uses modus.mvm compiler symbols and must precede the
    ;; sft-auto scan that registers translate-mvm-to-aarch64 et al.  The
    ;; boot-encoder shim must follow the translator (emit-aarch64-u32 calls
    ;; a64-emit) and precede the boot descriptor that calls it.
    *translate-aa64-source*         (string #\Newline)
    *aa64-boot-encoder-source*      (string #\Newline)
    *target-source*                 (string #\Newline)
    *cross-source*                  (string #\Newline)
    *boot-linux-desc-source*        (string #\Newline)
    *boot-linux-aa64-desc-source*   (string #\Newline)
    *selfhost-target-coinit-source* (string #\Newline)))

(defvar *stage2-test-source* "
;; Multiply overflow promotion regression probes (compiled native mul-checked).
(defun %nat-mul-20 () (* 10000000000 10000000000))    ; bignum 10^20
(defun %nat-mul-small () (* 12345 678))               ; fixnum 8369910
;; Runtime EVAL of a defun-with-param then call it; guards the intern
;; composite-key %fixnum-* fix.  Expect 6.
(defun %s2-defun-add () (mvm-eval-forms (list (list (quote defun) (quote g) (list (quote y)) (list (quote +) (quote y) 1)) (list (quote g) 5))))
;; Direct emit+fetch round-trip probe for a u64 immediate (isolates the
;; bytecode encoder/decoder from compile-integer).
(defun %ws1-u64rt (imm)
  (let ((buf (make-mvm-buffer :bytes (make-array 16))))
    (mvm-emit-u64 buf imm)
    (let ((bc (mvm-buffer-used-bytes buf)))
      (list (aref bc 0) (aref bc 1) (aref bc 2) (aref bc 3)
            (aref bc 4) (aref bc 5) (aref bc 6) (aref bc 7)
            (quote =>) (fetch-u64 bc 0)))))
(defun %ws1-rt-neg10 () (%ws1-u64rt -10))
(defun %ws1-rt-pos10 () (%ws1-u64rt 10))
(defun %ws1-ev (v) (mvm-eval-forms (list v)))
(defun %ws1-halves-of (v)
  (list (quote v=) v
        (quote lo) (* (logand v 2147483647) 2)
        (quote hi) (logand (ash v -31) 4294967295)))
(defun %ws1-v2w (v) (%val->word v))
(defun %ws1-w2v (w) (%word->val w))
(defun %ws1-roundtrip (v) (%word->val (%val->word v)))
(defun %s2-if-false () (mvm-eval (list (quote if) (list (quote >) 1 2) 100 200)))
(defun %s2-eq-false () (mvm-eval (list (quote if) (list (quote =) 1 2) 100 200)))
(defun %s2-nested () (mvm-eval (list (quote +) (list (quote *) 2 3) (list (quote -) 10 4))))
(defun %s2-add () (mvm-eval (list (quote +) 1 2)))
(defun %s2-mul () (mvm-eval (list (quote *) 3 4)))
(defun %s2-sub () (mvm-eval (list (quote -) 10 3)))
(defun %s2-if  () (mvm-eval (list (quote if) (list (quote <) 1 2) 100 200)))
;; STAGE 3 (drop-native model): mvm-eval calling other BYTECODE functions.
;; A helper defun + an expression that calls it — bytecode->bytecode CALL,
;; no marshalling, one value representation.
(defun %s2-call-helper ()
  ;; (defun sq (x) (* x x)) (sq 7) -> 49
  (mvm-eval-forms (list (list (quote defun) (quote sq) (list (quote x))
                           (list (quote *) (quote x) (quote x)))
                     (list (quote sq) 7))))
(defun %s2-call-two ()
  ;; (defun add3 (a b c) (+ a (+ b c))) (add3 10 20 30) -> 60
  (mvm-eval-forms (list (list (quote defun) (quote add3) (list (quote a) (quote b) (quote c))
                           (list (quote +) (quote a) (list (quote +) (quote b) (quote c))))
                     (list (quote add3) 10 20 30))))
(defun %s2-recursion ()
  ;; (defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 5) -> 120
  (mvm-eval-forms (list (list (quote defun) (quote fact) (list (quote n))
                           (list (quote if) (list (quote <) (quote n) 2)
                                 1
                                 (list (quote *) (quote n)
                                       (list (quote fact) (list (quote -) (quote n) 1)))))
                     (list (quote fact) 5))))
;; WS1.0 spike: the value<->word reinterpret boundary (unified representation).
(defun %ws10-word-of-1 () (%val->word 1))            ; raw word of fixnum 1 = 2
(defun %ws10-fixnum-rt () (%word->val (%val->word 42))) ; round-trip fixnum -> 42
(defun %ws10-cons-rt ()
  ;; THE key test: reinterpret a real cons to its raw word and back; identity
  ;; (eq) must survive -> the boundary is non-copying = no marshalling.
  (let ((c (cons 11 22)))
    (eq c (%word->val (%val->word c)))))
(defun %ws10-cons-rt-car ()
  ;; after the round-trip, the reconstructed cons must still car/cdr correctly.
  (let* ((c (cons 11 22)) (c2 (%word->val (%val->word c))))
    (list (car c2) (cdr c2))))                       ; want (11 22)
(defun %ws10-mutate-through ()
  ;; mutate via the reconstructed pointer; the ORIGINAL must see it (shared,
  ;; not a copy) -> proves heap-sharing / identity end to end.
  (let* ((c (cons 11 22)) (c2 (%word->val (%val->word c))))
    (rplaca c2 99)
    (car c)))                                        ; want 99
;; A real NATIVE runtime function (compiled into the image, in the sft), NOT in
;; any mvm-eval module — the target for the WS1 runtime-call bridge.
(defun %rt-double (x) (* x 2))
(defun %rt-add3 (a b c) (+ a (+ b c)))
(defun %rt-carplus (c) (+ (car c) (cdr c)))   ; takes a CONS arg
(defun %rt-mkpair (a b) (cons a b))           ; returns a CONS
;; WS1 milestone: mvm-eval calling a real native runtime function (no marshalling).
(defun %ws1-call-native ()
  (mvm-eval-forms (list (list (quote %rt-double) 21))))      ; want 42
(defun %ws1-call-native3 ()
  (mvm-eval-forms (list (list (quote %rt-add3) 10 20 30))))  ; want 60
(defun %ws1-call-native-nested ()
  ;; native call whose arg is itself a native call -> proves value flow across
  ;; the bridge composes.
  (mvm-eval-forms (list (list (quote %rt-double)
                           (list (quote %rt-double) 5)))))  ; want 20
;; WS1 structure: aligned op-cons/op-car on the real heap.
(defun %ws1-cons-car ()
  ;; pure in-interpreter: op-cons then op-car (no native call) -> 11
  (mvm-eval-forms (list (list (quote car) (list (quote cons) 11 22)))))
(defun %ws1-cons-arg ()
  ;; mvm-eval builds a CONS and passes it to a NATIVE fn that cars/cdrs it ->
  ;; proves the interpreter's cons is a real native cons (no marshalling) -> 30
  (mvm-eval-forms (list (list (quote %rt-carplus) (list (quote cons) 10 20)))))
(defun %ws1-cons-return ()
  ;; native fn RETURNS a cons; mvm-eval should hand back a real (7 . 8)
  (mvm-eval-forms (list (list (quote %rt-mkpair) 7 8))))
;; Frontier batch: map what the aligned model handles vs what needs work.
;; Returns a list of results; labels/expected printed from the test script
;; (no string literals here — they would terminate the source-string).
(defun %ws1-try (form)
  (handler-case (mvm-eval-forms (list form))
    (error (e) (list :err e))))
;; WS1 strings/vectors (interpreter-internal, make-mvm-object basis).
(defun %ws1-vec-const ()  ; constant index -> obj-ref
  (mvm-eval-forms (list (list (quote aref) (vector 10 20 30) 1))))         ; 20
(defun %ws1-vec-var ()    ; variable index -> aref opcode
  (mvm-eval-forms (list (list (quote let) (list (list (quote i) 2))
                           (list (quote aref) (vector 10 20 30) (quote i)))))) ; 30
(defun %ws1-str-aref ()   ; string elt via obj-ref + code-char wrap
  (mvm-eval-forms (list (list (quote aref) \"abc\" 1))))                   ; #\b
(defun %ws1-vec-fn-var () ; variable index via function param (isolate let vs aref)
  (mvm-eval-forms (list (list (quote defun) (quote g) (list (quote n))
                           (list (quote aref) (vector 10 20 30) (quote n)))
                     (list (quote g) 2))))                              ; 30
(defun %ws1-aref-raw ()   ; direct aref-opcode test: build vec, var idx from arith
  (mvm-eval-forms (list (list (quote aref) (vector 10 20 30) (list (quote + ) 1 1))))) ; 30
;; WS1 #2: native strings cross the bridge to NATIVE string functions.
(defun %ws1-str-length () (mvm-eval-forms (list (list (quote length) \"hello\"))))  ; 5
(defun %ws1-str-char ()   (mvm-eval-forms (list (list (quote char) \"hello\" 1))))   ; #\e
(defun %ws1-str-upcase () (mvm-eval-forms (list (list (quote string-upcase) \"abc\")))) ; -> ABC
;; NATIVE objects cross the bridge: vector length + FLOAT arithmetic.
(defun %ws1-vec-len ()   (mvm-eval-forms (list (list (quote length) (vector 10 20 30))))) ; 3
(defun %ws1-float-add () (mvm-eval-forms (list (list (quote +) 1.5 2.5))))   ; 4.0
(defun %ws1-float-mul () (mvm-eval-forms (list (list (quote *) 2.0 3.0))))   ; 6.0
(defun %ws1-float-lit () (mvm-eval-forms (list 1.5)))                        ; 1.5 (literal round-trip)
(defun %ws1-float-1arg () (mvm-eval-forms (list (list (quote %rt-double) 1.5))))      ; 3.0 (1 float arg via bridge)
(defun %ws1-float-3arg () (mvm-eval-forms (list (list (quote %rt-add3) 1.0 2.0 3.0)))) ; 6.0 (3 float args)
(defun %ws1-float-2lit () (mvm-eval-forms (list (list (quote list) 1.5 2.5))))         ; (1.5 2.5) build 2 floats
(defun %ws1-2cons () (mvm-eval-forms (list (list (quote cons) 1.0 2.0))))              ; (1.0 . 2.0) 2 floats via cons
(defun %ws1-2cons-fix () (mvm-eval-forms (list (list (quote cons) 100 200))))          ; (100 . 200) 2 fixnums via cons (control)
;; Isolate build-vs-print: these don't PRINT the floats.
(defun %ws1-consp2f () (mvm-eval-forms (list (list (quote consp) (list (quote cons) 1.0 2.0))))) ; T if build ok
(defun %ws1-floatp1 () (mvm-eval-forms (list (list (quote floatp) (list (quote car) (list (quote cons) 1.0 2.0)))))) ; T
(defun %ws1-floatp2 () (mvm-eval-forms (list (list (quote floatp) (list (quote cdr) (list (quote cons) 1.0 2.0)))))) ; T if 2nd float valid
(defun %ws1-eqfloat () (mvm-eval-forms (list (list (quote eql) (list (quote car) (list (quote cons) 5.0 6.0)) 5.0)))) ; T if 1st = 5.0
(defun %ws1-eql-same () (mvm-eval-forms (list (list (quote eql) 5.0 5.0))))   ; T? (eql semantics on 2 float literals)
(defun %ws1-car-gt () (mvm-eval-forms (list (list (quote >) (list (quote car) (list (quote cons) 9.0 2.5)) 1.0)))) ; T (car 9.0 > 1.0)
(defun %ws1-cdr-gt () (mvm-eval-forms (list (list (quote >) (list (quote cdr) (list (quote cons) 9.0 2.5)) 1.0)))) ; T (cdr 2.5 > 1.0); NIL if cdr=0.0
(defun %ws1-gt-ff () (mvm-eval-forms (list (list (quote >) 9.0 1))))   ; T : ONE float (9.0) vs fixnum 1
(defun %ws1-lt-ff () (mvm-eval-forms (list (list (quote <) 1 9.0))))   ; T : fixnum 1 vs ONE float
(defun %ws1-fp9 () (mvm-eval-forms (list (list (quote floatp) 9.0))))  ; T : single float valid
(defun %ws1-int9 () (mvm-eval-forms (list (list (quote truncate) 9.0)))) ; 9 : single-float value check via truncate
(defun %ws1-lit9 () (mvm-eval-forms (list 9.0)))    ; 9.0 ? (hi bit31 set -> suspect 0.0)
(defun %ws1-lit2 () (mvm-eval-forms (list 2.0)))    ; 2.0 ? (hi=0x40000000, hi<<1 bit31 set)
(defun %ws1-lit05 () (mvm-eval-forms (list 0.5)))   ; 0.5 (hi<<1 bit31 NOT set -> expect ok)
(defun %ws1-lit-neg () (mvm-eval-forms (list -1.5))) ; -1.5
(defun %ws1-2to31 () (mvm-eval-forms (list 2147483648)))   ; 2^31 = 0x80000000 (bit31 set) -> round-trip?
(defun %ws1-2to31m () (mvm-eval-forms (list 2147483647)))  ; 2^31-1 (bit31 NOT set) control
(defun %ws1-bigfix () (mvm-eval-forms (list 2151677952)))  ; 0x80440000 (the 9.0 hi<<1)
(defun %ws1-hival () (mvm-eval-forms (list 1075843072)))   ; 0x40220000 = 9.0 hi VALUE; LI imm = 0x80440000 (bit31)
(defun %ws1-hi2 () (mvm-eval-forms (list 1073741824)))     ; 0x40000000 = 2.0 hi VALUE; LI imm = 0x80000000 (bit31)
;; Ratios (subtag #x33) and bignums (subtag #x30): same native-alloc + obj-set
;; path as floats — validate they round-trip and arithmetic builds them.
(defun %ws1-rat-lit () (mvm-eval-forms (list 1/2)))                         ; 1/2 literal round-trip
(defun %ws1-rat-lit2 () (mvm-eval-forms (list 3/4)))                        ; 3/4 literal
(defun %ws1-rat-div () (mvm-eval-forms (list (list (quote /) 4 3))))        ; 4/3 via division
(defun %ws1-rat-num () (mvm-eval-forms (list (list (quote numerator) 3/4)))) ; 3
(defun %ws1-rat-den () (mvm-eval-forms (list (list (quote denominator) 3/4)))) ; 4
(defun %ws1-big-lit () (mvm-eval-forms (list 4611686018427387904)))        ; 2^62 bignum literal
(defun %ws1-big-mul () (mvm-eval-forms (list (list (quote *) 1000000000000 1000000000000)))) ; 10^24 bignum
(defun %ws1-big-add () (mvm-eval-forms (list (list (quote +) 4611686018427387904 1)))) ; 2^62+1
(defun %ws1-big130 () (mvm-eval-forms (list 1361129467683753853853498429727072845824))) ; 2^130 big-bignum literal
(defun %ws1-big-neg () (mvm-eval-forms (list -4611686018427387904)))                     ; -2^62 negative small bignum
(defun %ws1-big-neg130 () (mvm-eval-forms (list -1361129467683753853853498429727072845824))) ; -2^130 negative big-bignum
(defun %ws1-neg5 () (mvm-eval-forms (list -5)))                      ; negative FIXNUM literal round-trip
(defun %ws1-neg-sub () (mvm-eval-forms (list (list (quote -) 3 8)))) ; -5 via subtraction
(defun %ws1-neg-big () (mvm-eval-forms (list -1000000)))             ; -10^6 negative fixnum
;; GC-stress: build an N-element list via interpreter recursion (op-cons each
;; step), holding the growing list `a` in a register across every allocation.
;; With early GC forced (MODUS_GC_R14 small), collections fire MID-eval; if the
;; regs are GC-safe (slots hold real values the collector traces+updates), the
;; held list survives and length is exact.  A stale-pointer bug would corrupt or
;; crash.
(defun %ws1-gc-stress (n)
  (mvm-eval-forms (list (list (quote defun) (quote bld) (list (quote k) (quote a))
                           (list (quote if) (list (quote =) (quote k) 0)
                                 (quote a)
                                 (list (quote bld) (list (quote -) (quote k) 1)
                                       (list (quote cons) (quote k) (quote a)))))
                     (list (quote length) (list (quote bld) n (quote nil))))))
(defun %ws1-frontier ()
  (list
    (%ws1-try (list (quote quote) (list 1 2 3)))                                ; (1 2 3)
    (%ws1-try (list (quote car) (list (quote quote) (list 7 8 9))))             ; 7
    (%ws1-try (list (quote cons) 3 nil))                                        ; (3)
    (%ws1-try (list (quote let) (list (list (quote x) 5)) (list (quote *) (quote x) (quote x)))) ; 25
    (%ws1-try (list (quote let) (list (list (quote a) 3) (list (quote b) 4))
                    (list (quote +) (list (quote *) (quote a) (quote a)) (list (quote *) (quote b) (quote b))))) ; 25
    (%ws1-try (list (quote progn) 1 2 3))                                       ; 3
    (%ws1-try (list (quote list) 1 2 3))                                        ; (1 2 3)
    (%ws1-try (list (quote length) (list (quote quote) (list 1 2 3))))          ; 3
    (%ws1-try (list (quote reverse) (list (quote quote) (list 1 2 3))))         ; (3 2 1)
    (%ws1-try (list (quote append) (list (quote quote) (list 1 2)) (list (quote quote) (list 3 4)))))) ; (1 2 3 4)
;; Hand-built bytecode to isolate inline ADD/SUB/MUL opcodes in-image,
;; bypassing the compiler's fast-path branch structure.
;; (li v0 IMM)(li v1 IMM)(OP v16 v0 v1)(halt).  op-li=17, add=32 sub=33 mul=34,
;; halt=162, vr=16.  Values are TAGGED (n<<1).
(defun %s2-raw-add ()
  ;; (+ 10 3): tagged 20,6 -> add -> 26 -> untag 13
  (ash (mvm-interpret (make-array 25 :initial-contents
        (list 17 0 20 0 0 0 0 0 0 0  17 1 6 0 0 0 0 0 0 0  32 16 0 1  162))) -1))
(defun %s2-raw-sub ()
  ;; (- 10 3): tagged 20,6 -> sub -> 14 -> untag 7
  (ash (mvm-interpret (make-array 25 :initial-contents
        (list 17 0 20 0 0 0 0 0 0 0  17 1 6 0 0 0 0 0 0 0  33 16 0 1  162))) -1))
(defun %s2-raw-mul ()
  ;; (* 3 4): tagged 6,8 -> mul(untag*untag,retag) -> 24 -> untag 12
  (ash (mvm-interpret (make-array 25 :initial-contents
        (list 17 0 6 0 0 0 0 0 0 0  17 1 8 0 0 0 0 0 0 0  34 16 0 1  162))) -1))
")

;; STAGE 2: *opcode-table* is populated on the HOST by mvm.lisp's defopcode
;; toplevel setf forms, but in-image those bare toplevel setfs DON'T run at
;; boot (only def* init-thunks do).  encode-instruction reads the table for
;; each instruction's operand spec — an empty table means it emits opcodes
;; with NO operands (truncated bytecode).  Generate a populate-defun from the
;; host table and trigger it via a defparameter init-thunk (those DO run).
(defvar *opcode-table-init-source*
  (with-output-to-string (s)
    (format s "(defun %~A-opcode-table ()~%" "populate")
    (maphash (lambda (code info)
               (format s "  (setf (gethash ~D *opcode-table*) (make-opcode-info :code ~D :name ~S :operands (quote ~S) :description ~S))~%"
                       code code
                       (modus.mvm::opcode-info-name info)
                       (modus.mvm::opcode-info-operands info)
                       (modus.mvm::opcode-info-description info)))
             modus.mvm::*opcode-table*)
    (format s "  t)~%")
    ;; defparameter init-thunk runs %populate-opcode-table at boot, AFTER the
    ;; *opcode-table* defparameter (earlier in source order) creates the table.
    (format s "(defparameter *%opcode-table-ready* (progn (%populate-opcode-table) t))~%")))
;; STAGE 1 diagnostics: real (defun ...) so scan-defuns registers them in the
;; sft, letting runtime EVAL exercise the defstruct + interpreter in isolation.
(defvar *stage1-test-source* "
(defstruct (baz (:conc-name baz-)) (q 77))
(defun %s1-baz () (baz-q (make-baz)))
(defstruct (bar (:conc-name bar-)) (x (make-array 3 :initial-element 0) :type simple-vector) (y 5))
(defun %s1-barx () (bar-x (make-bar)))
(defun %s1-bary () (bar-y (make-bar)))
(defun %s1-mms () (make-mvm-state) 7)
(defun %s1-regs () (mvm-regs (make-mvm-state)))
(defun %s1-flags () (mvm-flags (make-mvm-state)))
(defun %s1-explicit-regs ()
  (mvm-regs (make-mvm-state :regs (make-array 23 :initial-element 0))))
(defun %s1-slot2 () (aref (make-mvm-state) 2))
(defun %s1-mklen () (length (make-array 23 :initial-element 0)))
(defun %s1-interp ()
  (mvm-interpret (make-array 11 :initial-contents (list 17 16 42 0 0 0 0 0 0 0 162))))
")
(defvar *rt-macros-source* (mvm-text "mvm/runtime-cl-macros.lisp"))
(defvar *bridge-source*
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
    (mvm-text "mvm/cl-clos.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-types.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-packages.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-conditions.lisp")
    (string #\Newline)
    (mvm-text "mvm/ansi-bridge.lisp")
    (string #\Newline)
    ;; tar + install-tarball are baked as GENERAL library primitives (NOT ql).
    ;; They are the untar->parse-.asd->topo-sort->eval pipeline; nothing about
    ;; them is quicklisp-specific.  They are baked (not runtime-(load)ed by
    ;; setup) because a key line — %tar-slice's `(make-array LEN)` with a
    ;; VARIABLE size — hits a pre-existing mvm-eval bug: `(make-array n)` for a
    ;; variable n returns an array of length n/2, so a >512-byte tar entry gets
    ;; truncated in half and its source fails to READ.  make-array with a
    ;; variable arg compiles correctly through the build's native compiler, so
    ;; baking sidesteps the interpreter gap.  (A runtime-(load) of these files
    ;; was verified to truncate sha1.lisp 7311->3655 bytes; baking loads it
    ;; whole.)  The QL package + ql:quickload still come ONLY from a runtime
    ;; (load) of modus-quicklisp/setup.lisp — never baked here.
    ;;
    ;; install-tarball.lisp names `chipz:decompress'/`chipz:gzip' on the
    ;; never-taken .tar.gz path; the flat image has no CHIPZ package so the
    ;; build reader would error `Package CHIPZ does not exist' and SILENTLY DROP
    ;; the form.  Strip the `chipz:' prefixes so it collapses to a bare
    ;; `decompress'/`gzip'; v1 ships PLAIN .tar so decompress is never called
    ;; (install-tarball only calls it on the gzip magic 1f 8b).  Read via
    ;; read-file-text (NOT mvm-text) — mvm-text's host check-parses errors on
    ;; `chipz:' before we strip it.
    (mvm-text "lib/tar.lisp")
    (string #\Newline)
    (%cli-strip-chipz (read-file-text (merge-pathnames "lib/install-tarball.lisp"
                                                       *modus-base*)))
    (string #\Newline)
    ;; The SHARED SBCL-faithful CLI toplevel: full argv (via the initial-stack
    ;; walk), SBCL-style flag parsing, ~/.modusrc, and the REPL.  It references
    ;; %gc-read64/%gc-stack-base (from gc.lisp, already in *all-runtime-source*).
    ;; Other hosted builds adopt this toplevel by baking this file and calling
    ;; (cli-toplevel) from kernel-main.
    (mvm-text "lib/cli-toplevel.lisp")))
;; WS3 STEP 4b (2026-07-09): mvm/tree-walker.lisp is NO LONGER part of this
;; image — production eval is mvm-eval only.  The full-corpus + gauntlet census
;; measured ZERO %e2ic walker-fallback hits (the earlier "-142 fallback
;; inventory" was the :li-func offset-0 phantom, fixed in a07fe7d), and the
;; walker-free image gates clean (16335-16336 / CHUNK-CRASH=0 / FILE-WEDGE=30,
;; gauntlet 243/243 x2 at 11 FAILFORMs).  tree-walker.lisp remains the eval
;; engine of the four legacy fork builds ONLY.  If %e2ic-compile ever fails on
;; a new shape it signals honestly (UNDEFINED-FUNCTION via the NIL fn sentinel).

(format t "  prelude: ~D chars~%" (length *prelude-source*))
(format t "  gc:      ~D chars~%" (length *gc-source*))
(format t "  rt:      ~D chars~%" (length *rt-source*))
(format t "  bridge:  ~D chars~%" (length *bridge-source*))

;;; ============================================================
;;; 3. Build-time scanners (same as build-x64-linux) so runtime LOAD
;;; can find every defun's source — without these the symbol-function
;;; table only knows the ~229 hand-curated entries from
;;; cl-eval.lisp's %init-sft-list and runtime-EVAL of any other name
;;; resolves to %%unresolved-fn.
;;; ============================================================

(format t "Generating sft-auto / sym-name-auto / runtime-macros init...~%")

;; sft-auto: collect every (defun NAME ...) form's NAME across all
;; runtime source and emit (puthash "NAME" *symbol-function-table*
;; #'NAME) calls so the runtime can resolve any defun by name.

(defun scan-defuns (text)
  "Return list of defun names (strings) found in TEXT."
  (let ((names nil)
        (pos 0))
    (loop
      (let ((p (search "(defun " text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p 7))
               (end (or (position-if (lambda (c)
                                       (or (char= c #\Space)
                                           (char= c #\Newline)
                                           (char= c #\()
                                           (char= c #\)))) text
                                     :start start)
                        (length text))))
          (push (string-upcase (subseq text start end)) names)
          (setq pos end))))))

;; Driver source is concatenated into the image AND scanned for
;; defun / symbol names, so defuns added in the driver (sys-exit,
;; %argv1, runtime-bq-expand, etc.) land in *symbol-function-table*
;; just like the CL runtime ones.  Forward-declared here; the actual
;; content lives below (section 4).  See the trailing setf-via-symbol-
;; value-set trick to overwrite this without re-reading the file.
(defvar *driver-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))
(defun halt ()
  (syscall3 60 1 0 0))
(defun %rbq-sym-name-eq (sym name)
  (and (symbolp sym) (string= (symbol-name sym) name)))
;; Level-tracking backquote expander.  LEVEL counts open backquotes
;; whose commas are still pending; the entry from the macro is LEVEL 1.
;; A COMMA at LEVEL 1 unquotes (its expr stays live); a COMMA at deeper
;; LEVEL is data that drops one level — this is what makes the classic
;; `,',x / `,,x tunnelling work (a `(... `(... ,',def ...)) form).  A
;; nested BACKQUOTE bumps the level by one for its template.
(defun runtime-bq-expand (template) (%rbq template 1))
(defun %rbq (template level)
  (cond
    ((null template) nil)
    ((atom template) (list 'quote template))
    ;; nested COMMA
    ((%rbq-sym-name-eq (car template) \"COMMA\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) \"COMMA-AT\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-at) (%rbq (cadr template) (- level 1)))))
    ((%rbq-sym-name-eq (car template) \"COMMA-DOT\")
     (if (= level 1)
         (cadr template)
         (list 'list (list 'quote 'comma-dot) (%rbq (cadr template) (- level 1)))))
    ;; nested BACKQUOTE — descend one deeper level, rebuild the marker
    ((%rbq-sym-name-eq (car template) \"BACKQUOTE\")
     (list 'list (list 'quote 'backquote) (%rbq (cadr template) (+ level 1))))
    (t (%rbq-list template level))))
(defun %rbq-list (lst level)
  (cond
    ((null lst) (list 'quote nil))
    ((not (consp lst)) (%rbq lst level))
    ;; a dotted/atom whole-form COMMA tail like `(a . ,b)
    ((%rbq-sym-name-eq (car lst) \"COMMA\")
     (if (= level 1)
         (cadr lst)
         (%rbq lst level)))
    (t
     (let ((first (car lst)) (rest (cdr lst)))
       (cond
         ((and (consp first) (%rbq-sym-name-eq (car first) \"COMMA-AT\") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         ((and (consp first) (%rbq-sym-name-eq (car first) \"COMMA-DOT\") (= level 1))
          (list 'append2 (cadr first) (%rbq-list rest level)))
         (t
          (list 'cons (%rbq first level)
                (%rbq-list rest level))))))))
;; COMPILE-TIME closure, NOT (eval '(lambda …)): this install runs at boot in
;; kernel-main, and under WS3 Phase-3 production EVAL is mvm-eval — a boot-time
;; mvm-eval of the lambda runs before init-all-globals (mvm-eval's state defvars all
;; NIL) and silently produced a broken expander (callee resolution fell into
;; the :li-func offset-0 fallback), so every runtime defmacro with a backquote
;; body \"expanded\" to its raw (BACKQUOTE …) template — uiop define-package
;; became a silent no-op and the asdf gauntlet died at the first read-time #.
;; that depended on an earlier defparameter.  The historical eval-based install
;; only worked because boot-time eval used to be the tree-walker (flag NIL).
;; Convention: non-interp-closure *macro-function-table* entries are funcalled
;; with the WHOLE form — (cadr mform) is the template.
(defun %install-runtime-backquote ()
  (set-macro-function 'backquote
                      (function (lambda (mform) (runtime-bq-expand (cadr mform))))))
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
;; Native diagnostic probes for the handler-frame chain (real RAM — an
;; interpreted mem-ref only sees the interp's per-state simulated memory
;; hash, so scripts must call these NATIVE fns to observe [#x10000400]
;; (handler-stack depth) and [#x10000180] (current armed frame RSP)).
(defun %hc-depth () (mem-ref #x10000400 :u32))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u32) 0) nil t))
;; Saved resume-IP (low 32 bits) of stacked frame N / the current frame —
;; code addrs are < 4GB so :u32 (tagged load) is exact.
(defun %hc-frame-ip (n) (mem-ref (+ #x10000408 (* 32 n) 16) :u32))
(defun %hc-cur-ip () (mem-ref #x10000190 :u32))
;; ---- WS5 STAGE 2: modus --compile <in.lisp> <out> (self-hosting) ----------
;; Read a Lisp source file, compile+translate+ELF-wrap it entirely in-image
;; via build-image (:linux-x64), and write a runnable Linux ELF.  NO SBCL.
(defun %sys-close (fd) (syscall3 3 fd 0 0))
(defun %selfhost-slurp-text (path)
  ;; Read PATH's full text into a fresh string (ASCII source: bytes == chars).
  (let ((s (open path :direction :input)))
    (if (null s)
        nil
        (let ((n (file-length s)))
          (let ((buf (%make-string-array n)))
            (let ((got (read-sequence buf s)))
              (close s)
              (if (< got n) (subseq buf 0 got) buf)))))))
(defun %selfhost-open-exec (path)
  ;; open(path, O_WRONLY|O_CREAT|O_TRUNC, 0755) -> fd
  (%string-to-cstr path *cstr-scratch*)
  (syscall3 2 *cstr-scratch* 577 493))
(defun %selfhost-write-bytes (fd bytes)
  ;; Bulk-write the (unsigned-byte 8) image vector in 64KB chunks packed into
  ;; *io-buf-addr* (write-sequence is one syscall/byte — far too slow at ~36MB).
  (let ((n (length bytes)) (off 0))
    (loop
      (when (>= off n) (return nil))
      (let ((chunk (if (< (- n off) 65536) (- n off) 65536)) (j 0))
        (loop
          (when (>= j chunk) (return nil))
          (setf (mem-ref (+ *io-buf-addr* j) :u8) (aref bytes (+ off j)))
          (setq j (+ j 1)))
        (%sys-write-raw fd *io-buf-addr* chunk)
        (setq off (+ off chunk))))))
(defun %selfhost-compile-file (in out)
  (let ((src (%selfhost-slurp-text in)))
    (if (null src)
        (progn (write-string-serial \"modus --compile: cannot read \")
               (write-string-serial in) (write-char-serial 10) (sys-exit 1))
       (progn
        ;; WS5 SYNTHESIS (minimal change from the clean-booting modus2-lb):
        ;; leave *mvm-eval-runtime-p* / *mvm-emit-halves* at their mvm-eval-leaked
        ;; T values (that gives the CHECKED + lazy-bind-nil global read, which
        ;; boots cleanly and DOESN'T crash on argv — the plain static
        ;; SYMBOL-VALUE read returned garbage for globals unbound at early boot
        ;; → wild funcall).  Add ONLY *static-build-p* so compile-quote bakes
        ;; string/list/symbol literals into the child's OWN code (via the static
        ;; alloc-obj paths) instead of the runtime *e2-const-pool* (which is
        ;; EMPTY in the child → garbage strings → flag string= never matched).
        ;; Net: static strings (work) + checked reads (no crash).
        ;; C=nil (mvm-emit-halves): the mvm-eval-leaked T makes quoted-symbol
        ;; literals emit :li-halves + :set-nargs (interpret-path shapes) which
        ;; the NATIVE translator mis-handles → broken symbol interning → the
        ;; reader crashes even on bare stdin.  Force nil so symbols emit the
        ;; plain native raw-:li path (proven OK: modus2-sb & modus2-host).
        ;; WS5: force the EXACT config modus2-host uses (SBCL build-image with
        ;; mvm-eval unused): mvm-eval-runtime-p=nil → compile-quote STATIC + compile-
        ;; variable-ref plain SYMBOL-VALUE + mvm-emit-halves nil native emit.
        ;; modus2-host (this config, SBCL-compiled) RUNS; modus2-sb (SAME config,
        ;; in-image-compiled) crashes on argv → the bug is an IN-IMAGE COMPILER
        ;; divergence (a mislinked fn), NOT the static/checked read choice.  This
        ;; build + the FNMAP dump localizes that mislinked fn.
        ;; WS5: reproduce modus2-sb EXACTLY (mvm-eval-runtime-p leaked T + static
        ;; strings + static reads + halves nil) but WITH the FNMAP dump, so the
        ;; identical-across-inputs early wild-jump crash (0x1816bad, RBP=0) can
        ;; be mapped to a function name in sb's OWN layout.
        (setq *static-build-p* t)
        (setq *mvm-emit-halves* nil)
        ;; WS5 DECISIVE: force mvm-eval-runtime-p NIL for the OUTPUT codegen — the
        ;; EXACT config modus2-hoststatic uses (which BOOTS + --version works
        ;; under SBCL).  With path-1 strings (no char-by-char GC-corruption bloat)
        ;; this is the first clean test of mvm-eval-runtime-p=NIL in-image.  If the
        ;; resulting modus2 boots+evals → the p1 hang was the mvm-eval-runtime-p T
        ;; codegen LEAK, not GC corruption.  (Compiler-operation branches that
        ;; needed T were char-by-char/GC artifacts now removed by path-1.)
        (setq *mvm-eval-runtime-p* nil)
        (let ((image (build-image :target :linux-x64 :source-text src)))
          ;; WS5 diag: dump the child's fn map (name → vaddr) so a crash RIP
          ;; can be resolved.  vaddr = 0x400000 + native-image-offset + off.
          (let ((fns (getf (kernel-image-metadata image) :fn-table))
                (nio (or (kernel-image-native-image-offset image) 0)))
            (when fns
              (dolist (fi fns)
                (write-string-serial \"FNMAP \")
                (print-dec (+ nio (or (mvm-function-info-native-offset fi) 0)))
                (write-char-serial 32)
                (write-string-serial (string (mvm-function-info-name fi)))
                (write-char-serial 10))))
          (let ((bytes (kernel-image-image-bytes image))
                (fd (%selfhost-open-exec out)))
            (if (< fd 0)
                (progn (write-string-serial \"modus --compile: cannot write \")
                       (write-string-serial out) (write-char-serial 10) (sys-exit 1))
                (progn
                  (%selfhost-write-bytes fd bytes)
                  (%sys-close fd)
                  (write-string-serial \"modus: wrote \")
                  (print-dec (length bytes))
                  (write-string-serial \" bytes to \")
                  (write-string-serial out) (write-char-serial 10)))))))))
(defun kernel-main ()
  (init-symbol-table)
  (init-keyword-table)
  (%init-packages)
  (%init-streams)
  (%init-reader)
  ;; WS5 self-host: READ our own source, which carries host-only package
  ;; qualifiers (modus.asm:… / modus.mvm.x64::…) for packages that don't exist
  ;; in the collapsed single-package image.  Lenient mode interns the bare name
  ;; in *PACKAGE* instead of a reader-error, so those ~44 cross.lisp /
  ;; boot-linux-x64 forms READ (were being SKIPped → calls resolved to
  ;; %unresolved-fn → NIL → TYPE-ERROR NIL during build-image).  ANSI-gate
  ;; images never set this (defvar default NIL), so they stay byte-identical.
  (setq *reader-missing-package-lenient* t)
  (%init-condition-types)
  (%init-method-combinations)
  (%init-symbol-function-table)
  (%init-sft-auto)
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)
  (setq *macro-table* (make-hash-table))
  (%init-runtime-macros)
  ;; Limitation #7: (defvar *setf-expanders* (make-hash-table)) init doesn't run
  ;; at boot, so the self-compiled compiler's *setf-expanders* is NIL.  Reads
  ;; (mvm-find-setf-expander) tolerate a NIL table (gethash->NIL), but the FIRST
  ;; in-image (defsetf ...) WRITES it via (setf (gethash h *setf-expanders*) ..)
  ;; -> PUTHASH derefs NIL -> SIGSEGV (the modus2 self-compile blocker at the
  ;; first defsetf, `(defsetf vref vset)`).  Init it here like *macro-table*.
  (setq *setf-expanders* (make-hash-table :test 'eql))
  (setq *cstr-scratch* #x0FE00000)  ; moved below heap base
  (setq *io-buf-addr*  #x0FF00000)  ; moved out of heap semispace 0; see memory note
  (%init-signal-handling)
  (%init-signal-symbols)
  (%init-make-load-form)
  (%install-runtime-backquote)
  ;; Init RT counters + registry (defvar init thunks don't run on bare metal)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)
  (setq *rt-registered-tests* nil)
  ;; WS5: gensym/gentemp counters (defvar inits don't run at boot — limitation
  ;; #7; every other build image sets these in its own kernel-main).  Without
  ;; this *gensym-counter* is NIL, so (gensym) yields #:GNIL every time and
  ;; `(format nil \"G~D\" NIL)` collides — breaking emit-handler-helpers during
  ;; the self-compiled product's translate-x64 (--compile threw a NIL condition).
  (setq *gensym-counter* 0)
  (setq *gentemp-counter* 0)
  ;; WS5 modus3 BUG#2: same defvar-init-at-boot gap (limitation #7) for two
  ;; compiler counters that are NOT let-bound in mvm-compile-all and were never
  ;; setq'd here, so they boot NIL.  *kw-rest-counter* is `incf`'d in
  ;; preprocess-params when compiling the FIRST &key defun (self-source ~form
  ;; 1750): (incf NIL) yields a tag-9 garbage value, which flows into
  ;; (format nil \"%KWF-~A-~D\" name <garbage>) → %FMT-INTEGER dereferences the
  ;; garbage as an object header → SIGSEGV (surfaced as modus3 `error: NIL`).
  ;; *nonlocal-block-tag-counter* has the identical latent gap (incf'd when a
  ;; nonlocal block/return-from compiles).  Init both to 0.
  (setq *kw-rest-counter* 0)
  (setq *nonlocal-block-tag-counter* 0)
  ;; WS5 modus3 BUG#3 (2026-07-25): same limitation-#7 gap for the string-bake
  ;; threshold.  *ws5-str-bake-min* (defvar 0) is UNBOUND in the self-compiled
  ;; gen2, so compile-quote's `(>= (length s) *ws5-str-bake-min*)` bake test
  ;; errors for EVERY string literal during gen2's compile of gen3 → all of
  ;; gen3's strings fall to the runtime *e2-const-pool* path (empty in the
  ;; child → garbage) → gen3's CLI string= never matches (--compile echoed,
  ;; rc=1), REPL prints mangled, image 4.6MB smaller (missing const pools).
  ;; Init to 0 = bake ALL strings, matching the host/gen1 default.
  (setq *ws5-str-bake-min* 0)
  ;; WS5 modus3 FIX: *x64-native-code-offset* is a defvar; its build-time
  ;; (setf … 397) at the tail of this file runs HOST-side and does NOT persist
  ;; into the image (limitation #7 — defvar init-thunks don't run at boot).  So
  ;; the in-image --compile defaulted it to 0, which padded fn file-offsets to
  ;; ≡0 mod 16; the real 397-byte linux-x64 boot preamble (≡13 mod 16) then
  ;; shifted every RUNTIME fn entry to nibble 0xd instead of 0.  The OR-3
  ;; closure-pointer tag convention (compile-lambda :li-func + `or $3`,
  ;; CALL-IND `sub $3`) needs entries ending in 0 (→ tag 0x3); at nibble d a
  ;; LOCAL call of a capturing flet/labels/lambda jumps MID-INSTRUCTION onto
  ;; the `89 e5` byte (= `mov %esp,%ebp`, 32-bit) → RBP high-half zeroed → SEGV.
  ;; This is the modus3 self-reproduction blocker (the compiler's own
  ;; vars-mutated-in-lambdas uses a recursive capturing `labels scan`).  Set 397
  ;; so in-image --compile aligns entries to nibble 0, matching the seed's own
  ;; build.  Proof: nm seed = all fns nibble 0; nm modus2-v2 = all nibble d.
  ;; See reference_modus3_optional_lambda_miscompile.
  (setq *x64-native-code-offset* 397)
  (%install-deftest-macro)
  ;; Run all built-in defvar init thunks.  Each is wrapped in
  ;; handler-case at compile time so a thunk that references a not-yet-
  ;; bound symbol can't kill the chain — see CLAUDE.md known limitation
  ;; #7 history.  Most thunks succeed and we get init values for free.
  (init-all-globals)
  ;; ANSI numeric/array constants whose DEFCONSTANT init thunks don't run
  ;; at boot (limitation #7).  The x64-linux gate image sets these in its
  ;; own kernel-main; build-generic relied only on init-all-globals, so
  ;; array-dimension-limit et al. were UNBOUND — third-party code that
  ;; reads them at read-time (chipz types-and-tables.lisp:
  ;; (deftype index () '(mod #.array-dimension-limit))) got UNBOUND-VARIABLE
  ;; during READ, silently dropping the whole file.
  (setq array-total-size-limit  (ash 1 24))
  (setq array-dimension-limit   (ash 1 24))
  (setq array-rank-limit        256)
  (setq call-arguments-limit    256)
  (setq lambda-parameters-limit 256)
  (setq pi 3.141592653589793d0)
  ;; AFTER init-all-globals — overrides defvar's init.  *write-object-
  ;; budget* defvars to 0 which immediately exhausts; we want a huge
  ;; budget so test names print fully.
  (setq *write-object-budget* 1000000)
  ;; Runtime CL macros (when/unless/setf/incf/case/dolist/etc.) — must
  ;; come AFTER init-all-globals so *modus-runtime-macros* has its
  ;; defvar value before we walk it.  Outer handler-case in case a
  ;; macro source string fails to parse (the install fn itself doesn't
  ;; wrap — see %install-runtime-cl-macros docstring).
  (handler-case (%install-runtime-cl-macros) (t (c) nil))
  ;; tar.lisp's *tar-block-size* defvar init-thunk doesn't run at boot (MVM
  ;; Active Limitation 7); set it so the baked tar reader works.  This is a
  ;; general library primitive, NOT ql wiring — the QL package + ql:quickload
  ;; come only from a runtime (load) of modus-quicklisp/setup.lisp.
  (setq *tar-block-size* 512)
  ;; WS5 STAGE 1: initialize the x64 build tooling (DEAD CODE at this stage).
  ;; Populate the translator lookup tables and rebuild+register the x86-64
  ;; target descriptor with its translate-fn/prologue/epilogue slots.  Their
  ;; defparameter init-thunks don't run at boot (MVM Active Limitation 7), so
  ;; do it explicitly here.  Wrapped so a self-compile gap can't kill boot.
  (handler-case (%init-x64-translator) (t (c) nil))
  (handler-case (%init-selfhost-targets) (t (c) nil))
  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  ;; cli-toplevel reads the FULL argv off the initial stack, parses SBCL-style
  ;; flags left-to-right (--eval/--load/--script/--quit/--version/--help/rc/
  ;; --end-toplevel-options), loads ~/.modusrc before an interactive REPL, and
  ;; either runs the REPL or exits.  It never returns (exits via sys-exit); the
  ;; outer handler-case is belt-and-suspenders in case of a parse-path crash.
  ;; WS5 STAGE 2 dispatch: `modus --compile IN OUT` self-compiles IN to a
  ;; native Linux ELF and exits; otherwise fall through to the normal CLI.
  (let ((av (handler-case (%cli-collect-argv) (t (c) nil))))
    (if (and (consp av) (consp (cdr av)) (stringp (car (cdr av)))
             (string= (car (cdr av)) \"--compile\"))
        (handler-case
            (progn (%selfhost-compile-file (nth 2 av) (nth 3 av)) (sys-exit 0))
          (t (c) (progn (write-string-serial \"modus --compile: error at \")
                        (handler-case (write-object *current-source-location*)
                          (t (c3) (write-string-serial \"?loc\")))
                        (write-string-serial \" cond-type=\")
                        (handler-case (write-object (type-of c))
                          (t (c4) (write-string-serial \"?type\")))
                        (write-string-serial \" cond=\")
                        (handler-case (write-object c) (t (c2) (write-string-serial \"<unprintable>\")))
                        (write-char-serial 10) (sys-exit 1))))
        (handler-case (cli-toplevel) (t (c) (sys-exit 1))))))
")

(defvar *all-runtime-source*
  (concatenate 'string *prelude-source*  (string #\Newline)
                       *gc-source*       (string #\Newline)
                       *mcgc-pin-source*
                       *rt-source*       (string #\Newline)
                       ;; STAGE 1: so the sft-auto scanner registers the
                       ;; interpreter's defuns (mvm-interpret, make-mvm-state,
                       ;; …) in *symbol-function-table* — without this they
                       ;; compile into the image but are unreachable from
                       ;; runtime EVAL.
                       *isa-source*      (string #\Newline)
                       *interp-source*   (string #\Newline)
                       *stage1-test-source* (string #\Newline)
                       *compiler-source* (string #\Newline)
                       *stage2-float-override* (string #\Newline)
                       *opcode-table-init-source* (string #\Newline)
                       *mvm-eval-canonical-source* (string #\Newline)
                       ;; WS5 STAGE 1: bake the x64 build tooling as DEAD CODE
                       ;; (translator + target + cross pipeline + linux-x64
                       ;; boot descriptor) so the sft-auto scanner registers
                       ;; build-image et al. in *symbol-function-table*.
                       *selfhost-tooling-source* (string #\Newline)
                       *stage2-test-source* (string #\Newline)
                       *rt-macros-source* (string #\Newline)
                       *bridge-source*   (string #\Newline)
                       *driver-source*))

(defvar *all-defun-names*
  ;; Filter to names that look like valid CL identifiers.
  (remove-if-not (lambda (s)
                   (and (stringp s)
                        (> (length s) 0)
                        (every (lambda (c)
                                 (or (alphanumericp c)
                                     (find c "+-*/<=>?!@$%^&_:|.~")))
                               s)))
                 (scan-defuns *all-runtime-source*)))

(format t "  defuns found: ~D~%" (length *all-defun-names*))

;; Emit %init-sft-auto in chunks of ~200 to avoid the compiler's
;; function-size limits.

(defun emit-sft-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sft-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash ~S *symbol-function-table* #'~A)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sft-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sft-auto-~D)~%" c))
      (format out ")~%"))))

(defvar *sft-auto-source* (emit-sft-auto *all-defun-names* 200))
(format t "  sft-auto: ~D chars~%" (length *sft-auto-source*))

;; sym-name-auto: collect every symbol NAME mentioned in source so
;; runtime symbol-name can recover the name for a native MVM symbol.

(defun scan-symbol-names (text)
  "Return list of distinct symbol-shaped tokens in TEXT."
  (let ((seen (make-hash-table :test 'equal))
        (result nil)
        (pos 0)
        (len (length text)))
    (loop
      (when (>= pos len) (return (nreverse result)))
      (let ((c (char text pos)))
        (cond
          ((or (char= c #\Space) (char= c #\Newline) (char= c #\Tab)
               (char= c #\Return) (char= c #\Page))
           (incf pos))
          ((char= c #\;)
           ;; comment to end of line
           (loop while (and (< pos len)
                            (not (char= (char text pos) #\Newline)))
                 do (incf pos)))
          ((char= c #\")
           ;; string literal — skip to closing quote
           (incf pos)
           (loop while (and (< pos len) (not (char= (char text pos) #\")))
                 do (when (char= (char text pos) #\\) (incf pos))
                    (incf pos))
           (incf pos))
          ;; Sharp dispatch: #\X character literal, #| ... |# block
          ;; comment, #(...) vector literal, #+/#- feature, etc.  Without
          ;; this special-case, #\" / #\; / #\( etc. fool the string- and
          ;; comment-skippers in the branches above and the scanner ends
          ;; up consuming dozens of legitimate tokens as if they were
          ;; inside a string.  In particular COMMA-AT on line 992 of
          ;; cl-reader.lisp was being eaten by the bogus string-skip
          ;; triggered by #\" on line 957.
          ((char= c #\#)
           (incf pos)
           (when (< pos len)
             (let ((next (char text pos)))
               (cond
                 ;; #\X — skip the backslash + next char (which may be
                 ;; any single character) + any trailing word (e.g.
                 ;; #\Newline / #\Space).
                 ((char= next #\\)
                  (incf pos)  ; past the backslash
                  (when (< pos len) (incf pos))  ; the literal char
                  ;; Multi-char names like Newline / Space / Tab — eat
                  ;; the rest of the word.
                  (loop while (and (< pos len)
                                   (alphanumericp (char text pos)))
                        do (incf pos)))
                 ;; #| ... |# block comment
                 ((char= next #\|)
                  (incf pos)
                  (loop while (and (< (1+ pos) len)
                                   (not (and (char= (char text pos) #\|)
                                             (char= (char text (1+ pos)) #\#))))
                        do (incf pos))
                  (when (< (1+ pos) len) (incf pos) (incf pos))))))
           ;; Other # forms (#( vector, #+ feature, etc.) just fall
           ;; through — the next iteration reads them as ordinary
           ;; tokens.
           )
          ((or (char= c #\() (char= c #\)) (char= c #\') (char= c #\`)
               (char= c #\,))
           (incf pos))
          (t
           ;; symbol token
           (let ((start pos))
             (loop while (and (< pos len)
                              (let ((ch (char text pos)))
                                (not (or (char= ch #\Space) (char= ch #\Newline)
                                         (char= ch #\() (char= ch #\))
                                         (char= ch #\Tab) (char= ch #\")
                                         (char= ch #\;)))))
                   do (incf pos))
             (let ((token (string-upcase (subseq text start pos))))
               (unless (or (gethash token seen)
                           (zerop (length token))
                           (every #'digit-char-p token))
                 (setf (gethash token seen) t)
                 (push token result))
               ;; Also register a keyword token's bare name (without the
               ;; leading colon).  compile-keyword uses (normalize-name
               ;; KW) → hash of "FOO" (not ":FOO"); symbol-name at
               ;; runtime looks up that hash in *sym-name-table*.
               ;; Without this, keywords interned at build-time print
               ;; as :|| because the colon-prefixed entry doesn't match.
               (when (and (> (length token) 1)
                          (char= (char token 0) #\:))
                 (let ((bare (subseq token 1)))
                   (unless (or (gethash bare seen)
                               (zerop (length bare)))
                     (setf (gethash bare seen) t)
                     (push bare result))))))))))))

(defvar *all-symbol-names* (scan-symbol-names *all-runtime-source*))

(format t "  symbol names found: ~D~%" (length *all-symbol-names*))

(defun emit-sym-name-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sym-name-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash (compute-name-hash ~S) *sym-name-table* ~S)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sym-name-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sym-name-auto-~D)~%" c))
      (format out ")~%"))))

(defvar *sym-name-auto-source* (emit-sym-name-auto *all-symbol-names* 200))

;; Macro table — extract every (mvm-define-macro NAME ...) from
;; compiler.lisp at build time so runtime macroexpand-1 can see them.

(defun scan-mvm-define-macro-forms (text)
  "Find (mvm-define-macro \"NAME\" ...) forms and return their names."
  (let ((names nil)
        (pos 0))
    (loop
      (let ((p (search "(mvm-define-macro \"" text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p (length "(mvm-define-macro \"")))
               (end (position #\" text :start start)))
          (push (subseq text start end) names)
          (setq pos (1+ end)))))))

(defvar *compiler-source* (mvm-text "mvm/compiler.lisp"))
(defvar *macro-names* (scan-mvm-define-macro-forms *compiler-source*))
(format t "  macro names found: ~D~%" (length *macro-names*))

;; Emit %init-runtime-macros — register each macro NAME → a marker
;; (T) in *macro-table*.  This makes macroexpand-1 report "yes I know
;; about that macro" without actually expanding it.  Real expansion
;; (for runtime EVAL of macro forms) requires the actual lambdas which
;; we can't ship as easily; for now mark them as known.
(defvar *runtime-macros-source*
  (with-output-to-string (out)
    (format out "(defun %init-runtime-macros ()~%")
    (dolist (name *macro-names*)
      (format out "  (puthash (compute-name-hash ~S) *macro-table* t)~%" name))
    (format out ")~%")))

;;; ============================================================
;;; 4. Driver — moved to forward-declaration above so the scanner
;;; picks up its defuns (sys-exit / runtime-bq-expand / etc.).
;;; ============================================================

;;; ============================================================
;;; 5. Assemble *full-source*
;;; ============================================================

(format t "Assembling full source...~%")

(defvar *full-source*
  (concatenate 'string
    *prelude-source*
    (string #\Newline)
    *gc-source*
    (string #\Newline)
    *mcgc-pin-source*
    *rt-source*
    (string #\Newline)
    *rt-macros-source*
    (string #\Newline)
    *bridge-source*
    (string #\Newline)
    ;; STAGE 1: MVM ISA constants/structs + bytecode interpreter.
    *isa-source*
    (string #\Newline)
    *interp-source*
    (string #\Newline)
    *stage1-test-source*
    (string #\Newline)
    ;; STAGE 2: the MVM compiler + in-image float-bits override + mvm-eval probe.
    *compiler-source*
    (string #\Newline)
    *stage2-float-override*
    (string #\Newline)
    *opcode-table-init-source*
    (string #\Newline)
    *mvm-eval-canonical-source*
    (string #\Newline)
    ;; WS5 STAGE 1: x64 build tooling baked as DEAD CODE (translator +
    ;; target descriptors + cross-compilation pipeline + linux-x64 boot
    ;; descriptor/ELF wrapper).  Loaded AFTER the compiler + mvm-eval (the
    ;; translator uses modus.mvm compiler symbols) and BEFORE the sft-auto
    ;; block so scan-defuns registers build-image / assemble-kernel-image /
    ;; wrap-in-elf64-le / etc.  Nothing routes to build-image — the CLI REPL
    ;; is unchanged.
    *selfhost-tooling-source*
    (string #\Newline)
    *stage2-test-source*
    (string #\Newline)
    ;; Defvar for *sym-name-table* (compiler.lisp now supplies *macro-table*'s
    ;; defvar; runtime macroexpand-1 references it).
    "(defvar *sym-name-table* nil)
"
    (string #\Newline)
    *sft-auto-source*
    (string #\Newline)
    *sym-name-auto-source*
    (string #\Newline)
    *runtime-macros-source*
    (string #\Newline)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; 5a. WS5 STAGE 3: strip HOST-ONLY package qualifiers from the self-source
;;; so Modus's own reader (which has no SB-KERNEL / SB-IMPL / SB-INT / SB-EXT
;;; packages) can READ it.  These qualifiers name SBCL internals used only by
;;; BUILD-TIME-ONLY, in-image-DEAD compiler helpers (ieee-float-* overridden by
;;; *stage2-float-override*; the SBCL backquote expander replaced in-image by
;;; runtime-bq-expand).  In-image the flat reader would error `Package SB-KERNEL
;;; does not exist` and SKIP the enclosing defun (+ cascade).  HOST-SAFE: the
;;; SBCL host build only COMPILES these dead defuns (a call to a name-hashed
;;; symbol it never runs) — byte-neutral for the shipped image.  Longer prefixes
;;; first so `::' variants win over `:'.
(let ((n-before (length *full-source*)))
  (dolist (pfx '("sb-kernel::" "sb-impl::" "sb-int::" "sb-ext::" "sb-sys::"
                 "sb-kernel:"  "sb-impl:"  "sb-int:"  "sb-ext:"  "sb-sys:"
                 "SB-KERNEL::" "SB-IMPL::" "SB-INT::" "SB-EXT::" "SB-SYS::"
                 "SB-KERNEL:"  "SB-IMPL:"  "SB-INT:"  "SB-EXT:"  "SB-SYS:"))
    (setf *full-source* (%cli-strip-one-prefix *full-source* pfx)))
  (format t "WS5-S3: stripped host-only SB-* package qualifiers (~D chars removed)~%"
          (- n-before (length *full-source*))))

;;; ============================================================
;;; 5b. WS5 STAGE 3: pre-expand `#.<SYMBOL>` read-time-eval sites so the
;;; SELF-SOURCE is cleanly re-readable BY MODUS ITSELF.  The only real `#.`
;;; read-eval forms are the ~95 `#.+op-NAME+` case keys in mvm/interp.lisp's
;;; mvm-interpret dispatch — ISA opcode DEFCONSTANTs.  In-image `defconstant`
;;; only folds its value into the compiler's *constants* table and emits NO
;;; runtime binding (CLAUDE.md limitation #7), so the in-image reader's
;;; `(eval '+op-nop+)` is UNBOUND and SKIPs the whole mvm-interpret form (+
;;; cascade READER-ERRORs).  Fix: textually replace each `#.<sym>` bound in
;;; :MODUS.MVM with its host-evaluated literal — EXACTLY what the host reader
;;; already produced, so byte-identical for the baked image build.  `#.(<list>)`
;;; forms and unbound-symbol `#.` sites (none today) are left verbatim.
(defun %selfhost-expand-read-eval-consts (text)
  (let ((out (make-string-output-stream))
        (pos 0)
        (len (length text))
        (n-expanded 0))
    (loop
      (let ((p (search "#." text :start2 pos)))
        (unless p
          (write-string (subseq text pos) out)
          (return))
        (write-string (subseq text pos p) out)
        (let ((tstart (+ p 2)))
          (if (or (>= tstart len)
                  (let ((c (char text tstart)))
                    (or (char= c #\() (char= c #\)) (char= c #\Space)
                        (char= c #\Newline) (char= c #\Tab) (char= c #\")
                        (char= c #\;))))
              (progn (write-string "#." out) (setf pos tstart))
              (let ((tend tstart))
                (loop while (and (< tend len)
                                 (let ((c (char text tend)))
                                   (not (or (char= c #\() (char= c #\)) (char= c #\Space)
                                            (char= c #\Newline) (char= c #\Tab)
                                            (char= c #\") (char= c #\;)))))
                      do (incf tend))
                (let* ((tok (subseq text tstart tend))
                       (sym (find-symbol (string-upcase tok) :modus.mvm)))
                  (if (and sym (boundp sym))
                      (progn (prin1 (symbol-value sym) out) (incf n-expanded))
                      (progn (write-string "#." out) (write-string tok out)))
                  (setf pos tend)))))))
    (values (get-output-stream-string out) n-expanded)))

(multiple-value-bind (expanded n)
    (%selfhost-expand-read-eval-consts *full-source*)
  (setf *full-source* expanded)
  (format t "WS5-S3: pre-expanded ~D `#.<const>` read-eval site(s); ~
            full source now ~D chars~%"
          n (length *full-source*)))

;; WS5 STAGE 3: dump the exact baked self-source to a file (for `modus --compile
;; <self-source> modus2`) and exit before the slow image build.  Env-guarded so
;; normal builds are unaffected.
#+sbcl
(let ((dump (sb-ext:posix-getenv "MODUS_DUMP_SOURCE")))
  (when (and dump (> (length dump) 0))
    (with-open-file (out dump :direction :output :if-exists :supersede
                              :element-type 'character)
      (write-string cl-user::*full-source* out))
    (format t "Dumped self-source (~D chars) to ~A~%"
            (length cl-user::*full-source*) dump)
    (sb-ext:exit :code 0)))

;;; ============================================================
;;; 6. Build the image
;;; ============================================================

(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)
;; Boot preamble for linux-x64 ends 397 bytes into the file (ELF header
;; + entry stub).  Native code starts there, so the fn-entry alignment
;; loop must account for this offset — otherwise `:li-func` + OR-3 +
;; CALL-IND's sub-3 lands one byte before the prologue.  When the
;; preceding function's last byte happens to be RET (0xC3), the
;; misaligned call returns immediately, leaving the caller's RAX
;; intact (silently looks like the fn returned T or whatever else
;; was in RAX).  See reference_append_funcall_bug.md.
(setf modus.mvm.x64::*x64-native-code-offset* 397)

;; Enable the GC trampoline: without this, every :alloc-obj advances R12
;; unchecked and the heap walks past the mapped region in long-running
;; sessions.  build-x64-linux / build-x64 set this; we
;; need it too so the generic image survives ANSI sweeps.
(setf modus.mvm.x64::*x64-gc-enabled* t)
;; Linux-x64 layout: enable the CONS-KIND bitmap (GC correctness fix for the
;; cons-tagged-scratch symbol-truncation bug).  The kind-bitmap base delta is
;; a boot-linux-x64 layout constant, so the master flag is Linux-only for now.
(setf modus.mvm.x64::*mcgc-kind-bitmap-enabled* t)
;; Bring R14 to the heap midpoint so GC actually fires before the
;; from-space is exhausted.  Default leaves R14 at full heap end which
;; means the gc-check `cmp r12, r14; jl skip` only triggers after
;; allocation walked all the way to the end — too late for a Cheney
;; copy that needs the other half free.
(setf modus.mvm::*linux-x64-r14-offset* modus.mvm::+linux-x64-gc-midpoint+)
;; WS5 GC-BISECT knobs (default OFF → normal build unchanged).  Each "=0"
;; disables one GC mechanism so a parallel sweep can isolate which one
;; corrupts the tail-function labels at self-compile scale.
#+sbcl (let ((v (sb-ext:posix-getenv "MODUS_WS5_GC")))
  (when (and v (string= v "0"))
    (setf modus.mvm.x64::*x64-gc-enabled* nil)
    (format t "~&;; WS5-BISECT: *x64-gc-enabled* = NIL (GC fully off)~%")))
#+sbcl (let ((v (sb-ext:posix-getenv "MODUS_WS5_BITMAP")))
  (when (and v (string= v "0"))
    (setf modus.mvm.x64::*mcgc-bitmap-enabled* nil)
    (format t "~&;; WS5-BISECT: *mcgc-bitmap-enabled* = NIL (no object-start gate)~%")))
#+sbcl (let ((v (sb-ext:posix-getenv "MODUS_WS5_COLLECTOR")))
  (when (and v (string= v "0"))
    (setf modus.mvm.x64::*mcgc-collector-enabled* nil)
    (format t "~&;; WS5-BISECT: *mcgc-collector-enabled* = NIL~%")))
#+sbcl (let ((v (sb-ext:posix-getenv "MODUS_WS5_KINDBM")))
  (when (and v (string= v "0"))
    (setf modus.mvm.x64::*mcgc-kind-bitmap-enabled* nil)
    (format t "~&;; WS5-BISECT: *mcgc-kind-bitmap-enabled* = NIL~%")))
#+sbcl (let ((v (sb-ext:posix-getenv "MODUS_WS5_KINDCHECK")))
  (when (and v (string= v "0"))
    (setf modus.mvm.x64::*mcgc-kind-check-enabled* nil)
    (format t "~&;; WS5-BISECT: *mcgc-kind-check-enabled* = NIL~%")))
;; WS5 A/B: force the cons-kind CHECK reject OFF unconditionally (the WIP's
;; mcgc-kind-check-on-p routes a NIL flag back through the bitmap decision, so
;; MODUS_WS5_KINDCHECK=0 alone no longer disables it — this hard override does).
#+sbcl (let ((v (sb-ext:posix-getenv "MODUS_WS5_NOKCHECK")))
  (when (and v (string= v "1"))
    (setf modus.mvm.x64::*ws5-force-no-kindcheck* t)
    (format t "~&;; WS5-BISECT: *ws5-force-no-kindcheck* = T (reject OFF)~%")))
;; WS5 string-bake threshold probe (sweep to bracket the GC-corruption size).
#+sbcl (let ((v (sb-ext:posix-getenv "MODUS_WS5_STRMIN")))
  (when (and v (> (length v) 0))
    (setf modus.mvm::*ws5-str-bake-min* (parse-integer v))
    (format t "~&;; WS5-PROBE: *ws5-str-bake-min* = ~D~%" modus.mvm::*ws5-str-bake-min*)))
;; MCGC page-pinning test knob (stage 4).  OFF by default.  MODUS_MCGC_PINNING=1
;; enables the page-pool allocator + page collector for a pinning test build.
#+sbcl
(when (let ((v (sb-ext:posix-getenv "MODUS_MCGC_PINNING")))
        (and v (plusp (length v)) (not (string= v "0"))))
  (setf modus.mvm.x64::*mcgc-pinning-enabled* t)
  (format t "~&;; MCGC PAGE-PINNING ENABLED (test build)~%"))
;; Test knob: MODUS_MCGC_TORUN_CAP=<pages> caps each to-run segment so the
;; copy_object refill / to-run-chain path is exercised on ordinary workloads.
#+sbcl
(let ((cap (sb-ext:posix-getenv "MODUS_MCGC_TORUN_CAP")))
  (when (and cap (> (length cap) 0))
    (setf modus.mvm.x64::*mcgc-torun-cap-pages* (parse-integer cap))
    (format t "~&;; MCGC TO-RUN SEGMENT CAP = ~D pages (refill stress)~%"
            modus.mvm.x64::*mcgc-torun-cap-pages*)))
;; Debug knob: MODUS_GC_R14=<hex-or-dec bytes> forces R14 to a small offset
;; so GC fires early (fast repro of GC-from-runtime-EVAL faults).  Leaves
;; the from/to semispaces 448MB apart (unchanged), only moves the trigger.
#+sbcl
(let ((dbg (sb-ext:posix-getenv "MODUS_GC_R14")))
  (when (and dbg (> (length dbg) 0))
    (let ((v (parse-integer dbg :radix (if (and (> (length dbg) 1)
                                                (char= (char dbg 0) #\#))
                                           16 10)
                            :start (if (char= (char dbg 0) #\#) 1 0))))
      (setf modus.mvm::*linux-x64-r14-offset* v)
      (format t "~%[DEBUG] R14 offset forced to ~X (GC fires early)~%" v))))
#+sbcl
(when (let ((d (sb-ext:posix-getenv "MODUS_GC_DEBUG"))) (and d (> (length d) 0)))
  (setf modus.mvm.x64::*x64-gc-debug* t)
  (format t "~%[DEBUG] GC trampoline debug bytes enabled~%"))
;; A/B knob: MODUS_MCGC_KINDCHECK=0 keeps the cons-kind bitmap SET side
;; (image layout ~unchanged) but DISABLES the scan_word reject, to prove the
;; CHECK — not incidental layout shift — restores correctness.  Default on.
#+sbcl
(let ((kc (sb-ext:posix-getenv "MODUS_MCGC_KINDCHECK")))
  (when (and kc (string= kc "0"))
    (setf modus.mvm.x64::*mcgc-kind-check-enabled* nil)
    (format t "~%[DEBUG] MCGC cons-kind CHECK disabled (set side still on)~%")))

#+sbcl
(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf modus.mvm::*write-symmap-path* sm)))

;; WS5 STAGE 4: SBCL-side compile of an arbitrary program (for apples-to-apples
;; vs `modus --compile`).  MODUS_COMPILE_FILE="in.lisp:out" → host build-image
;; compiles IN to a Linux ELF at OUT and exits.  Same build-image/translator the
;; in-image path uses, so the emitted bytes are directly comparable (fixpoint).
#+sbcl
(let ((cf (sb-ext:posix-getenv "MODUS_COMPILE_FILE")))
  (when (and cf (> (length cf) 0))
    ;; WS5 DEFINITIVE TEST: MODUS_WS5_STATIC=1 makes this HOST (SBCL) build use
    ;; the SAME static-build codegen as the in-image `--compile` (all strings →
    ;; constant-table, static reads), but under SBCL's GC (NO Modus-GC → NO
    ;; corruption possible).  If the result HANGS/crashes → the bug is in the
    ;; static codegen path (build-constant-pool short strings etc.), NOT GC.
    ;; If it WORKS → the in-image failure is GC corruption.
    (when (let ((v (sb-ext:posix-getenv "MODUS_WS5_STATIC")))
            (and v (string= v "1")))
      ;; ONLY *static-build-p* — this alone triggers the string→constant-table
      ;; path (all strings, since *ws5-str-bake-min*=0).  Do NOT force
      ;; *mvm-eval-runtime-p* T (its in-image-runtime branches break the SBCL host
      ;; build).  Leaving it NIL keeps symbols/lists/reads on the normal host
      ;; static paths — so this isolates EXACTLY the short-string constant-table
      ;; change under SBCL's GC.
      (setf modus.mvm::*static-build-p* t)
      (format t "~&;; WS5-STATIC-TEST: host build, strings→constant-table only~%"))
    (let* ((colon (position #\: cf))
           (inp (subseq cf 0 colon))
           (outp (subseq cf (1+ colon)))
           (src (with-open-file (s inp)
                  (let ((str (make-string (file-length s))))
                    (subseq str 0 (read-sequence str s)))))
           (image (build-image :target :linux-x64 :source-text src)))
      (with-open-file (o outp :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence (kernel-image-image-bytes image) o))
      (sb-ext:run-program "/bin/chmod" (list "+x" outp) :wait t)
      (format t "~%SBCL-compiled ~A → ~A (~D bytes)~%"
              inp outp (length (kernel-image-image-bytes image)))
      (sb-ext:exit :code 0))))

(format t "~%Compiling generic image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64
                          :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_CLI_OUT") "modus")))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Usage: ~A <script.lisp>~%" path)))
