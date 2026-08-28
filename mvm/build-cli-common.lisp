;;;; build-cli-common.lisp — shared source assembly for the hosted Modus CLI.
;;;;
;;;; Loaded by BOTH clean-image CLI wrappers, after each sets *CLI-ARCH* and
;;;; DEFVARs its arch slots:
;;;;   mvm/build-generic-cli.lisp   (:x64)      -> ./modus
;;;;   mvm/build-aarch64-cli.lisp   (:aarch64)  -> modus-aa64-cli
;;;;
;;;; It assembles *FULL-SOURCE*: the CL runtime bridge, RTEST, the in-image MVM
;;;; ISA/interpreter/compiler/mvm-eval, the arch's native JIT translator, tar +
;;;; install-tarball, the hosted socket/storage/HTTP layer, the :GENERA and ASDF
;;;; surfaces, lib/cli-toplevel.lisp, the driver (sys-exit + kernel-main) and the
;;;; three auto-generated scanners (sft-auto, sym-name-auto, runtime-macros).
;;;; Each wrapper supplies only its arch tail: boot preamble, translator install,
;;;; GC knobs, build-image.
;;;;
;;;; WHY THIS FILE EXISTS.  The two wrappers were parallel hand-maintained
;;;; lineages whose parity was asserted by COMMENT ("matching build-generic-cli's
;;;; knob set exactly") and enforced by nothing.  Every drift between them was
;;;; found as a production bug, never by review:
;;;;   * task #245 — (init-all-globals) was x64-only for months, leaving ~150
;;;;     aarch64 globals UNBOUND, each one a latent UNBOUND-VARIABLE.
;;;;   * RTEST was x64-only, so no library's own test suite could RUN on
;;;;     aarch64 at all — the ladder was x64-measurable and arm-unmeasurable.
;;;;   * net/hosted-sockets, -storage and -http were x64-only, so networked
;;;;     ql:quickload could not exist on arm.
;;;;   * the aarch64 wrapper based a SHIPPING image on build-ansi-common.lisp,
;;;;     the ANSI GATE-RUNNER harness — the wrong taxonomy class (CLAUDE.md
;;;;     "Build taxonomy"), which also made its symbol-name table depend on an
;;;;     ANSI corpus checked out at a hardcoded absolute path.
;;;; Sharing the assembly makes the DEFAULT "both arches get it", and makes every
;;;; remaining difference an explicit, named, greppable slot.
;;;;
;;;; THE ARCH SLOTS ARE THE WHOLE CONTRACT.  A divergence is legitimate only if
;;;; it is a hardware/target fact.  Each wrapper must DEFVAR all of these before
;;;; loading this file:
;;;;
;;;;   *CLI-ARCH*                    :x64 | :aarch64 | :i386
;;;;   *CLI-ARCH-SYSCALL-SOURCE*     sys-exit / halt.  exit_group is syscall 231
;;;;                                 on x86-64, 94 on the AArch64 generic ABI
;;;;                                 and 252 on the i386 int-0x80 ABI.  NOT
;;;;                                 60/93/1 — those are `exit', which ends only
;;;;                                 the calling thread and hangs a threaded
;;;;                                 image forever.
;;;;   *CLI-ARCH-PROBE-SOURCE*       arch-address diagnostic probes, baked ahead
;;;;                                 of kernel-main.
;;;;   *CLI-ARCH-KERNEL-PROLOGUE*    hardware setup that must precede the FIRST
;;;;                                 allocation (aarch64: BSS zeroing + GC
;;;;                                 object-start bitmap reservation).
;;;;   *CLI-ARCH-IO-SCRATCH-SOURCE*  the *cstr-scratch* / *io-buf-addr* setqs.
;;;;                                 A memory-map fact: i386's heap is at
;;;;                                 0x30000000, so the 64-bit ports' 0x0FE00000
;;;;                                 is unmapped there, and an i386 syscall
;;;;                                 argument travels as a TAGGED fixnum so the
;;;;                                 address must also be below 2^30.
;;;;   *CLI-ARCH-KERNEL-EPILOGUE*    the toplevel entry / probe program.
;;;;   *CLI-ARCH-OVERRIDE-SOURCE*    late last-defun-wins overrides spliced right
;;;;                                 after the bridge (aarch64: the `*at'-syscall
;;;;                                 file I/O, and %cli-argv-base).
;;;;
;;;; The one arch branch that lives HERE rather than in a wrapper is the JIT
;;;; translator block, because reading it needs MVM-TEXT, which this file
;;;; defines.  Anything that differs between the two images and is NOT one of
;;;; the slots above is a BUG, not a divergence.
;;;;
;;;; GATE: MODUS_DUMP_FULL_SOURCE=<path> writes the assembled blob and exits
;;;; without building.  The blob is the only thing a wrapper contributes to the
;;;; binary, so byte-identity of the blob proves a wrapper refactor changed
;;;; nothing — in ~20s instead of a 10-20 minute image build.

(declaim (special *cli-arch*
                  *cli-arch-syscall-source* *cli-arch-probe-source*
                  *cli-arch-kernel-prologue* *cli-arch-io-scratch-source*
                  *cli-arch-kernel-epilogue* *cli-arch-override-source*
                  *cli-bare-metal* *cli-bare-metal-tarball*
                  *cli-bare-metal-net-source* *cli-omit-ansi-bridge*))

;;; BARE-METAL SEAM.  DEFVAR, so a wrapper that binds these BEFORE loading this
;;; file keeps its value and everything else defaults to the hosted behaviour —
;;; i.e. every existing build stays byte-identical by construction.
;;;
;;;   *CLI-BARE-METAL*          T => omit the hosted payload (Linux syscalls,
;;;                             fds, argv, cli-toplevel) from *BRIDGE-SOURCE*.
;;;   *CLI-BARE-METAL-TARBALL*  T => bare metal, but still bake lib/tar.lisp +
;;;                             lib/install-tarball.lisp so the image can
;;;                             install a library it fetched itself.
;;;   *CLI-BARE-METAL-NET-SOURCE*  the target's OWN network stack + installer,
;;;                             spliced right after *STAGE2-TEST-SOURCE*.  A
;;;                             bare-metal image has no host kernel, so it bakes
;;;                             a real driver/IP/HTTP stack where a hosted image
;;;                             uses *CLI-HOSTED-PAYLOAD-SOURCE*'s syscalls.
;;;                             POSITION IS LOAD-BEARING: after the CL runtime,
;;;                             the compiler and mvm-eval so the arch adapter's
;;;                             own definitions win under last-defun-wins (it
;;;                             deliberately overrides WRITE-BYTE), and BEFORE
;;;                             *DRIVER-SOURCE*, whose kernel-main calls
;;;                             run-net-pipeline.  "" on hosted builds.
;;;
;;; This exists so the bare-metal CL images are THIN TAILS over this shared
;;; assembly rather than private forks of it.  The forks are exactly how the
;;; 2026-08-15 console (PL011 vs mini UART) and USB-DMA-past-end-of-RAM bugs
;;; survived: each was fixed in one assembly and not the other.  The 2026-08-20
;;; entry in that list: build-rpi-cl-repl carried a private copy of the x64 JIT
;;; block (%init-x64-translator + x64-asm.lisp) on an AARCH64 image, dead only
;;; because its *jit-on* was nil — see #266.
(defvar *cli-bare-metal* nil)
(defvar *cli-bare-metal-tarball* nil)
(defvar *cli-bare-metal-net-source* "")

;;; BRING-UP CULL, not part of the bare-metal seam: T drops mvm/ansi-bridge.lisp
;;; (~1.7 MB of source -> ~3.5 MB of image) from *BRIDGE-SOURCE*.  It exists for
;;; the Pi chain-load path, where every megabyte is minutes of UART time.  NOT
;;; proven boot-safe — ansi-bridge.lisp is load-bearing (it defines list /
;;; make-array / rplaca / the CLOS defaults and %init-clos-protocol), so measure,
;;; do not assume.  Default NIL => every existing build is byte-identical.
(defvar *cli-omit-ansi-bridge* nil)

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

;;; GUARDED: a wrapper that must READ SOURCE FILES to build one of its arch
;;; slots (the RPi bare-metal image reads its USB/net stack into
;;; *CLI-BARE-METAL-NET-SOURCE*) has to load the MVM system itself, because a
;;; slot must be bound BEFORE this file is loaded.  Loading the whole system a
;;; second time is ~20s of pure waste and doubles every build warning.  Hosted
;;; wrappers do not pre-load, so the package does not exist and this loads
;;; exactly as it always did.
(unless (find-package "MODUS.MVM")
  (load (merge-pathnames "../lib/load-mvm.lisp"
                         (directory-namestring (truename *load-truename*)))))
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
;; RTEST — the RT (Paul Dietz) regression tester as a real CL package, so a
;; library's OWN test suite (`(:use :cl :foo #+sbcl :sb-rt #-sbcl :rtest)' —
;; alexandria and everything shaped like it) can be loaded and RUN on Modus
;; instead of being measured by a handful of hand-written smoke probes.
;; This is providing a DEPENDENCY, like ASDF, not shimming a test framework:
;; the semantics are RT's, validated test-for-test against SB-RT.  MUST come
;; after *rt-source* — its DO-TESTS deliberately shadows rt.lisp's
;; counter-printing one via last-defun-wins (see mvm/rtest.lisp header).
(defvar *rtest-pkg-source* (mvm-text "mvm/rtest.lisp"))
;; STAGE 1 of retiring the tree-walker: ship the MVM ISA + bytecode
;; interpreter into the image so `eval` can eventually = compile→interpret
;; (one semantics, shared with the compiler) rather than the divergent
;; tree-walker.  mvm.lisp = opcode/vreg constants + structs; interp.lisp =
;; the bytecode executor (mvm-interpret).  interp.lisp uses #.+op-nop+
;; read-time eval, resolved by the readers binding *package* to :modus.mvm
;; (cross.lisp check-parses / read-all-forms-with-locations).
(defvar *isa-source*      (mvm-text "mvm/mvm.lisp"))
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
;;; WS5 rung 2: OPTIONAL runtime JIT (MODUS_USE_JIT=1)
;;; ============================================================
;;; The default CLI image is pure interpret (mvm-eval = compile→MVM→mvm-interpret).
;;; With MODUS_USE_JIT=1 we ALSO bake the x64 native translator into the image so
;;; mvm-eval's JIT seam (%jit-translate-page / %jit-call in mvm-eval.lisp) can execute
;;; forms as NATIVE code.  This mirrors what the ANSI gate (build-x64-linux.lisp)
;;; does via build-ansi-common.lisp: bake x64-asm.lisp + translate-x64.lisp +
;;; a %init-x64-translator co-init, run %init-x64-translator at boot, and bake
;;; %jit-enabled-p to return T.  When JIT is OFF none of this source is added, so
;;; the default image stays byte-identical to before.
;; FLIP (2026-07, WS4 #197): the runtime JIT is now the DEFAULT for the shipping
;; CLI.  It is flip-clean — the full ANSI gate (64-shard NET) is reg=0 / gain=+2 /
;; CHUNK-CRASH=0 JIT-on vs JIT-off, and the JIT-vs-interpret differential harness
;; (422 boundary forms) shows zero JIT wrong-values.  Any form the translator
;; can't handle falls back cleanly to mvm-interpret (the %jit-translate-page
;; guard), so correctness is preserved by construction.  Rollback: MODUS_NO_JIT=1
;; (or an explicit MODUS_USE_JIT=0) rebuilds the pure-interpret image.
(defvar *jit-on*
  (let ((no (sb-ext:posix-getenv "MODUS_NO_JIT"))
        (v  (sb-ext:posix-getenv "MODUS_USE_JIT")))
    (cond ;; NAMED DIVERGENCE — i386 is pure-interpret.  mvm/translate-i386.lisp
          ;; exists and is what BUILDS this image, but there is no in-image JIT
          ;; arm for it: no shrink needle for its code buffer, no co-init to
          ;; populate the vreg/register tables limitation #7 leaves empty, and
          ;; no runtime PROT_EXEC page primitive on the i386 trap table.  Baking
          ;; the translator without those would leave %jit-translate-page
          ;; failing every form — i.e. the same interpret path, plus ~1 MB of
          ;; dead code.  mvm-eval's JIT seam falls back cleanly, so the image is
          ;; correct, just slower.  Removing this line is the WHOLE change when
          ;; an i386 JIT arm lands; it is tracked as an open item, not a fact
          ;; about the hardware.
          ((eq *cli-arch* :i386) nil)
          ((and no (> (length no) 0)) nil)           ; MODUS_NO_JIT → rollback to interpret
          (v (or (string= v "1") (string-equal v "t") (string-equal v "yes")))  ; explicit
          ;; WS5 #206: DEFAULT is ON again.  It was turned OFF in 2d95d3e
          ;; because a JIT'd form that called a RUNTIME-DEFINED function
          ;; re-executed every side effect preceding the call (measured:
          ;; pre=2 post=1 for a 3-setq progn; `(setq *use-jit* nil)` alone
          ;; flipped it).  The root cause is fixed, not worked around:
          ;;
          ;;   (symbol-function 'car)       -> 15165315         nibble 3, in-image
          ;;   a runtime (defun kk (x) ...) -> 129844928268201  nibble 9, ON HEAP
          ;;
          ;; A runtime-defined function is a HEAP CLOSURE, and %jit-reloc-calls
          ;; patched `word - 3` into `movabs rax, imm64; call rax` regardless —
          ;; a jump into PROT_READ|WRITE memory with no PROT_EXEC.  SEGV_ACCERR
          ;; mid-execution, then the fallback re-ran the whole form.  aec8341
          ;; requires the FN tag on the callee, so a non-tag-3 callee fails the
          ;; reloc, fails the page build, and the form is INTERPRETED ONCE.
          ;;
          ;; The re-flip gate 2d95d3e recorded is MET, and by the metric it
          ;; named rather than by value-only evidence:
          ;;   tests/runtime-metric.lisp, MODUS_USE_JIT=1, x64 vs SBCL
          ;;     -> EMPTY DIFF, all 16 checks, form-ran-once=1.
          ;; (Note the gate's own premise was incomplete — see d139c73; a
          ;; handler-level "infrastructure vs user condition" test cannot fix
          ;; this, because the failure is an infrastructure fault that happens
          ;; DURING native execution.  Preventing it was the only fix.)
          ;;
          ;; ANSI-gated: 64-shard NET over 10001..27800, fresh detached
          ;; worktrees both sides, same 17625-test corpus —
          ;;   BASE 17476 / NET 17475, CHUNK-CRASH 0=0, FILE-WEDGE 30=30.
          ;; The lone -1 (P:13445) passes 3/3 on BOTH binaries in isolation, so
          ;; it is 600s shard-truncation noise.  See GATE-RESULT-206-207.md.
          ;;
          ;; Perf cost of the fix measured ~zero (3.991s -> 4.033s on the same
          ;; workload): only the TOP-LEVEL form degrades to interpret, and
          ;; top-level forms run once — the hot code is in function bodies,
          ;; which are separate modules that still JIT.
          ;;
          ;; Rollback: MODUS_NO_JIT=1 (or MODUS_USE_JIT=0).
          (t t))))                                    ; DEFAULT: JIT ON

;;; ARCH SLOT — the MVM->native translator baked into the image.  This is the one
;;; arch branch that lives in the shared file rather than in a wrapper, because
;;; reading these files needs MVM-TEXT (defined above).
;;;
;;; A genuine hardware divergence: x86-64 needs its variable-length instruction
;;; encoder (x64-asm.lisp) plus translate-x64.lisp; AArch64 needs only
;;; translate-aarch64.lisp, which carries its own fixed-width encoder.  Both are
;;; read, size-shrunk (their code-buffer defaults are sized for a whole-image
;;; translation, not a single JIT page) and trimmed of the host-only
;;; install-*-translator tail, then followed by a co-init defun that populates
;;; the lookup tables the files' own defvar init-thunks would have filled —
;;; CLAUDE.md limitation #7 means those thunks never run at boot.
;;;
;;; COND, not CASE: SBCL's CASE family expands with a gensym, which advances the
;;; host *GENSYM-COUNTER* and can rename a gensym in an auto-generated table —
;;; enough to break the byte-identity proof this refactor is gated on.  Same
;;; reason build-ansi-common.lisp's *arch-translator-block* uses COND.
(defvar *x64-asm-source*
  (when (and *jit-on* (eq *cli-arch* :x64)) (mvm-text "mvm/x64-asm.lisp")))
;; Shrink the code-buffer default from 96MB to 64KB (grows on demand) so a JIT
;; page translation doesn't try to alloc ~768MB tagged per make-code-buffer.
(when (and *jit-on* (eq *cli-arch* :x64))
  (let ((needle "(bytes (make-array 100663296 :element-type '(unsigned-byte 8)))")
        (repl   "(bytes (make-array 65536 :element-type '(unsigned-byte 8)))"))
    (let ((p (search needle *x64-asm-source*)))
      (unless p (error "WS5-JIT: could not find code-buffer 96MB default to shrink"))
      (setf *x64-asm-source*
            (concatenate 'string
                         (subseq *x64-asm-source* 0 p) repl
                         (subseq *x64-asm-source* (+ p (length needle))))))))
;; translate-x64.lisp minus the host-only install-x64-translator tail (it refs
;; *target-x86-64* / disassemble-native &key — not compilable in-image).
(defvar *translate-x64-source*
  (when (and *jit-on* (eq *cli-arch* :x64))
    (let ((src (mvm-text "mvm/translate-x64.lisp"))
          (marker "(defun install-x64-translator"))
      (let ((pos (search marker src)))
        (unless pos (error "WS5-JIT: could not find install-x64-translator strip marker"))
        ;; #211: the trim drops mvm-text's trailing package reset with the
        ;; tail, so re-append it — the next chunk concatenated after this one
        ;; (*jit-coinit-source*) must not read in MODUS.MVM.X64.
        (concatenate 'string (subseq src 0 pos)
                     modus.mvm::*build-package-reset-text*)))))
;; Co-init that populates the translator's defvar lookup tables at boot AND sets
;; the runtime JIT globals.  Byte-for-byte the same table data the ANSI gate
;; installs (build-ansi-common.lisp *x64-translator-coinit-source*), plus a
;; RUNTIME (setq *x64-gc-enabled* …) mirroring the value the host baked into the
;; image's fixed code — limitation #7 means the file-tail (setf …) does NOT reach
;; the image runtime, so %jit-translate-page would otherwise read the defvar
;; default (NIL) and emit gc-checks/trampoline inconsistently with the baked code.
(defvar *jit-coinit-source*
  (when (and *jit-on* (eq *cli-arch* :x64))
   (concatenate 'string "
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
    ;; V9..V15 SPILL to the stack and V22 (VPC) is unmapped, so VREG-PHYS must
    ;; return NIL for them.  They MUST be written explicitly: make-array
    ;; zero-inits unwritten slots to FIXNUM 0 (GC safety), NOT to NIL, and 0 is
    ;; TRUE in CL -- so (vreg-phys 9) yielded 0, DEST-PHYS-OR-SCRATCH's
    ;; `(or (vreg-phys v) +scratch-reg+)` picked 0 over the scratch register,
    ;; and reg-info then signalled `Unknown register: 0`.  The JIT's flip-safety
    ;; guard turned that signal into an interpret fallback, so the whole class
    ;; of forms that spills past V8 -- CLOS defclass/defmethod dispatch and
    ;; nested LOOP -- silently NEVER ran native.  (build-modus-selfhost.lisp has
    ;; carried this exact fix and its explanation since the self-host work; the
    ;; JIT co-inits were never brought to parity.)
    (aset v 9 nil)  (aset v 10 nil) (aset v 11 nil) (aset v 12 nil)
    (aset v 13 nil) (aset v 14 nil) (aset v 15 nil) (aset v 22 nil)
    (aset v 16 (quote rax)) (aset v 17 (quote r12))
    (aset v 18 (quote r14)) (aset v 19 (quote r15))
    (aset v 20 (quote rsp)) (aset v 21 (quote rbp))
    (setq *vreg-to-x64* v))
  ;; Fn-entry alignment offset.  The JIT page is a STANDALONE mmap (not the ELF
  ;; image), so the translator only uses this to NOP-align emitted fn entries
  ;; RELATIVE to the buffer base — 0 gives clean 16-byte-relative alignment.
  ;; This matches the ANSI gate's co-init (build-ansi-common.lisp), whose
  ;; JIT battery runs native with offset 0.
  (setq *x64-native-code-offset* 0)
  (setq *x64-linux-mode* t)
  ;; RUNTIME JIT globals (limitation #7: file-tail host setf doesn't reach here).
  ;; Match the values the host baked into the image's fixed code so the JIT emits
  ;; consistent gc-check/trampoline code.
  (setq *x64-gc-enabled* t)
  (setq *linux-x64-r14-offset* #x38000000)
  ;; THE PER-THREAD WINDOW must match what the host baked into the image's
  ;; fixed code, for the same reason *x64-gc-enabled* must: a JIT page that
  ;; emitted absolute handler-frame accesses while the surrounding AOT code
  ;; emits FS-relative ones would disagree on any thread whose FS base is not
  ;; zero.  On the MAIN thread the two are the same address either way, which
  ;; is why getting this wrong would be silent.
  ;;
  ;; BOTH HALVES, AND ONLY ONE WAS HERE.  *X64-TLS-WINDOW* is the TRANSLATOR
  ;; half — it emits the FS prefix when the width carries the thread-local bit.
  ;; *TLS-WINDOW* is the COMPILER half — it is what SETS that bit, at
  ;; %TLS-WIDTH / %MV-WIDTH, for an address it can prove lands in the window.
  ;; With only the translator half on, the bit was never set and every
  ;; JIT-compiled MULTIPLE-VALUE-BIND, HANDLER-CASE and UNWIND-PROTECT reached
  ;; the MAIN THREAD's window from whatever thread it ran on.  CLAUDE.md
  ;; recorded that as a stated boundary -- runtime EVAL on a second thread was
  ;; never attempted anyway -- and it is attempted now, because glass is loaded at
  ;; runtime and its RFB server is HANDLER-CASE on a worker thread.
  ;;
  ;; MEASURED BEFORE THE FIX, hosted x64: a JIT-compiled thread body whose
  ;; whole content was (handler-case 222 (t (c) -1)) died reporting an
  ;; MVM CALL-IND with a non-callable target -- an indirect call through a
  ;; handler frame that belonged to another thread-s window.  MULTIPLE-VALUE-
  ;; BIND and LOOP in the same position were fine, which is exactly the shape
  ;; of a shared HANDLER-FRAME STACK rather than a shared MV buffer.
  ;; (No quotation marks in this comment on purpose: it is INSIDE the co-init
  ;; SOURCE STRING, so a double quote here ends the string literal.)
  (setq *x64-tls-window* t)
  (setq *tls-window* t)
  (setq *jit-xlate-err-info* nil)
  ;; WS5 #223 / #278: emit the THUNK's li-const as a load from the GC-updated
  ;; constant vector instead of a baked heap address, so a mid-flight
  ;; collection cannot stale a running top-level form's literals.  MUST match
  ;; the host-side (setf modus.mvm.x64::*x64-jit-constvec-p* t) that put the
  ;; vector's BSS word in this image's collector root list — the emitted load
  ;; is only sound because that root exists.
  (setq *x64-jit-constvec-p* t)
  ;; #226 FULL SCOPE is a BUILD-TIME OPT-IN (MODUS_CONSTVEC_FULL=1): every
  ;; JIT li-const goes through the vector, so const-bearing runtime defuns
  ;; (the interpreted-deflate class) install native — ql:quickload drops
  ;; from ~25min to ~45s and is verified end-to-end at that setting.  It is
  ;; NOT the default because the JIT-on ANSI gate exposes a residual
  ;; eval-result corruption under full scope (RUN-TYPEP-DEBUG-TESTS calls a
  ;; fixnum; deterministic repro `gate-bin 11080 11140`, task #226) that
  ;; thunk-only scope does not have.  The optional setq is spliced in by the
  ;; concatenation below when the env knob is set.
"
   ;; #226 is FIXED (the in-image root-address defvar read; see the
   ;; translate-x64 literal-fallback commit), so FULL scope is the DEFAULT —
   ;; it is what makes ql:quickload take ~45s instead of ~25min.  Set
   ;; MODUS_CONSTVEC_FULL=0 to fall back to thunk-only scope.
   (if (let ((v (sb-ext:posix-getenv "MODUS_CONSTVEC_FULL")))
         (and v (string= v "0")))
       ""
       "  (setq *x64-jit-constvec-full-p* t)
")
   "  t)
(in-package :modus.mvm)
")))

;; Baked boot hook + JIT gate.  Appended LAST so its %jit-enabled-p wins over
;; mvm-eval.lisp's base version (last-defun).  When JIT is OFF both defuns are
;; no-ops (%jit-boot-init returns nil, %jit-enabled-p returns nil) so the seam
;; stays inert — and the whole string is "" only when JIT is off would change
;; layout, so we ALWAYS bake these two tiny defuns (JIT-off variant is inert
;; and keeps *use-jit* nil = interpret).
;;; --- AArch64: MVM->aarch64 translator (read + shrink + trim) + co-init ---
;;; Recipe, shrink needle, strip marker and co-init text are verbatim from
;;; mvm/build-ansi-common.lisp — the four ANSI gate runners' source of truth for
;;; this block; only the MODUS_NO_JIT guard is added, to match the x64 branch.
(defvar *translate-aarch64-source*
  (when (and *jit-on* (eq *cli-arch* :aarch64))
    (let ((src (mvm-text "mvm/translate-aarch64.lisp")))
      ;; Shrink the a64-buffer code array default (16M general slots ~ 128MB
      ;; tagged) to 64K — a64-emit grows it on demand, and a 128MB alloc per
      ;; make-a64-buffer would exhaust the image semispace on every JIT page.
      (let ((needle "(code (make-array 16777216))")
            (repl   "(code (make-array 65536))"))
        (let ((p (search needle src)))
          (unless p (error "CLI-JIT: could not find a64-buffer 16M code default to shrink"))
          (setq src (concatenate 'string (subseq src 0 p) repl
                                 (subseq src (+ p (length needle)))))))
      ;; Strip install-aarch64-translator and the host-only ELF/target-descriptor
      ;; tail after it (refs *target-aarch64*, &key disassemble — not compilable
      ;; in-image).  #211: re-append the package reset the trim cuts off.
      (let ((marker "(defun install-aarch64-translator"))
        (let ((pos (search marker src)))
          (unless pos (error "CLI-JIT: could not find install-aarch64-translator strip marker"))
          (concatenate 'string (subseq src 0 pos)
                       modus.mvm::*build-package-reset-text*))))))

;; AArch64 co-init.  Same role as %init-x64-translator: populate the tables the
;; translator's defvar init-thunks would have filled (limitation #7).  Verbatim
;; from build-ansi-common.lisp's *aarch64-translator-coinit-source*.
(defvar *aarch64-jit-coinit-source*
  (when (and *jit-on* (eq *cli-arch* :aarch64)) "
(defun %init-aarch64-translator ()
  (let ((map (make-array 23)))
    (aset map 0 0) (aset map 1 1) (aset map 2 2) (aset map 3 3)
    (aset map 4 19) (aset map 5 20) (aset map 6 21) (aset map 7 22) (aset map 8 23)
    (aset map 9 nil) (aset map 10 nil) (aset map 11 nil) (aset map 12 nil)
    (aset map 13 nil) (aset map 14 nil) (aset map 15 nil)
    (aset map 16 0) (aset map 17 24) (aset map 18 25) (aset map 19 26)
    (aset map 20 31) (aset map 21 29) (aset map 22 nil)
    (setq *a64-vreg-to-phys* map))
  ;; *mvm-label-counter* is a (defvar ... 0) whose init-thunk does NOT run at
  ;; boot (CLAUDE.md item 7) -> nil at runtime; translate-aarch64's (incf ...)
  ;; would crash.  The compiler does not use it, so nothing else initialises it.
  (when (null *mvm-label-counter*) (setq *mvm-label-counter* 0))
  ;; CRITICAL for the JIT: *aarch64-stack-align-16* gates :push/:pop codegen.
  ;; nil (its runtime default) emits the bare-metal str/ldr pre-index pair which
  ;; MISALIGNS SP to 8-mod-16 -> Linux EL0 SP-alignment fault (SIGBUS) on the
  ;; next SP access.  The host-side setf does not reach runtime (item 7), so set
  ;; it here for JIT-translated code.  *aarch64-linux-mode* likewise, so any
  ;; TRAP codegen emits Linux syscalls.
  (setq *aarch64-stack-align-16* t)
  (setq *aarch64-linux-mode* t)
  t)
"))

;;; The assembled translator slot, spliced into *all-runtime-source* and
;;; *full-source* as ONE unit.  Both arches: <encoder?> <translator> <co-init>,
;;; each followed by a newline — byte-identical on x64 to the three separate
;;; (or *x64-...* "") + newline pairs this replaced.
(defvar *cli-arch-jit-translator-source*
  (cond
    ((eq *cli-arch* :x64)
     (concatenate 'string
       (or *x64-asm-source* "")       (string #\Newline)
       (or *translate-x64-source* "") (string #\Newline)
       (or *jit-coinit-source* "")    (string #\Newline)))
    ((eq *cli-arch* :aarch64)
     (concatenate 'string
       ""                                  (string #\Newline)
       (or *translate-aarch64-source* "")  (string #\Newline)
       (or *aarch64-jit-coinit-source* "") (string #\Newline)))
    ;; i386: no in-image JIT translator — see the *JIT-ON* comment above.
    ((eq *cli-arch* :i386) "")
    (t (error "build-cli-common: unknown *cli-arch* ~S (want :x64, :aarch64 or :i386)"
              *cli-arch*))))

;;; ARCH SLOT — boot hook + JIT gate.  Appended LAST so its %jit-enabled-p wins
;;; over mvm-eval.lisp's base version (last-defun-wins).  When JIT is OFF both
;;; defuns are inert; the two tiny defuns are ALWAYS baked (an empty string here
;;; would shift layout).
;;;
;;; The arch difference is real: aarch64 must additionally select the back-end
;;; (*jit-target-arch*) and enable *aarch64-jit-mode*, under which in-module
;;; +op-fn-addr+ emits a RELOCATABLE MOVZ/MOVK quad instead of the placeholder
;;; that apply-aarch64-fn-addr-patches fixes up at image-assembly time — which
;;; the runtime JIT never runs, so every JIT-built closure would get slot 0 =
;;; literal 0 and trap in +op-call-ind+ before its body (WS5 #207 / 21347e4).
;;; x64 is structurally immune: its in-module fn-addr is a PC-relative LEA+OR-3,
;;; with nothing to patch.
(defvar *jit-boot-source*
  (cond
    ((not *jit-on*)
     "
(defun %jit-boot-init () nil)
")
    ((eq *cli-arch* :x64)
     "
(defun %jit-boot-init () (%init-x64-translator) (setq *use-jit* t) t)
(defun %jit-enabled-p () (and (boundp (quote *use-jit*)) *use-jit*))
")
    (t
     "
(defun %jit-boot-init ()
  (%init-aarch64-translator)
  (setq *aarch64-jit-mode* t)
  (setq *jit-target-arch* :aarch64)
  (setq *use-jit* t)
  t)
(defun %jit-enabled-p () (and (boundp (quote *use-jit*)) *use-jit*))
")))

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
;;; THE LIBRARY-LOADING LAYER — COMMON TO EVERY TARGET THAT LOADS LIBRARIES.
;;;
;;; This is the untar -> parse-.asd -> topo-sort -> eval pipeline, and there is
;;; nothing hosted about it: the same code serves `./modus' on Linux and the
;;; bare-metal Pi, which fetches a .tar over its own DWC2/CDC stack and installs
;;; it in RAM.  It used to live INSIDE the hosted arm of one big `cond', welded
;;; to the Linux syscall layers below, so a bare-metal target that could not
;;; take `hosted-sockets' also lost `install-tarball' — an accident of how the
;;; cond was written, never a requirement.  Split so the two can be selected
;;; independently: platform backing is per-target, library loading is not.
(defvar *cli-library-payload-source*
  (if (or (not *cli-bare-metal*) *cli-bare-metal-tarball*)
      (concatenate 'string
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
                                                       *modus-base*))))
      ""))

;;; THE HOSTED PLATFORM LAYER — Linux syscalls, hosted targets only.
;;;
;;; Sockets / block storage / HTTP / the SBCL-faithful toplevel.  These are
;;; IMPLEMENTATIONS of contracts, not the contracts themselves: bare metal
;;; satisfies the same ones through ip.lisp + a device driver (and, for the
;;; filesystem, pagetree/cabinet).  This is the ONLY part that is genuinely
;;; per-target.
(defvar *cli-hosted-platform-source*
  (if (not *cli-bare-metal*)
      (concatenate 'string
    (string #\Newline)
    ;; Hosted TCP+UDP sockets (Linux syscalls): connected-socket primitives +
    ;; DNS-over-UDP/TCP resolver.  The gateway to networked ql:quickload.
    (mvm-text "net/hosted-sockets.lisp")
    (string #\Newline)
    ;; Hosted block storage + durability: raw-fd positioned block I/O + fsync/
    ;; ftruncate.  The persistence layer for pagetree/cabinet/cl-consensus.
    (mvm-text "net/hosted-storage.lisp")
    (string #\Newline)
    ;; Hosted HTTP/1.0 client on the socket layer: URL -> DNS -> TCP -> GET.
    ;; The gateway to networked ql:quickload (fetch dist + tarballs).
    (mvm-text "net/hosted-http.lisp")
    (string #\Newline)
    ;; The SHARED SBCL-faithful CLI toplevel: full argv (via the initial-stack
    ;; walk), SBCL-style flag parsing, ~/.modusrc, and the REPL.  It references
    ;; %gc-read64/%gc-stack-base (from gc.lisp, already in *all-runtime-source*).
    ;; Other hosted builds adopt this toplevel by baking this file and calling
    ;; (cli-toplevel) from kernel-main.
    (mvm-text "lib/cli-toplevel.lisp"))
      ""))

;;; What the image actually bakes: libraries first, then platform.  Hosted
;;; builds get exactly the text they always did, in the same order, so their
;;; blobs stay byte-identical across this split; a bare-metal-tarball build
;;; gets the library layer and an empty platform layer.
(defvar *cli-hosted-payload-source*
  (concatenate 'string *cli-library-payload-source* *cli-hosted-platform-source*))

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
    (if *cli-omit-ansi-bridge* "" (mvm-text "mvm/ansi-bridge.lisp"))
    (string #\Newline)
    ;; ---- BARE METAL STOPS HERE -------------------------------------------
    ;; Everything below is the HOSTED payload: Linux syscalls, fds, argv.  A
    ;; bare-metal target (no OS, no filesystem, no argv) sets *CLI-BARE-METAL*
    ;; and gets "" here, optionally keeping just the tar/install-tarball
    ;; library pipeline via *CLI-BARE-METAL-TARBALL* (the RPi net build fetches
    ;; a .tar over its own USB/CDC stack and installs it in RAM).
    ;;
    ;; Splitting this out is what lets the bare-metal CL images (RPi, x64) be
    ;; THIN TAILS over this file instead of private copies of it.  Those copies
    ;; are how the console and DMA-region bugs of 2026-08-15 survived: a fix
    ;; landed in one assembly and not the other.
    *cli-hosted-payload-source*))

;; WS3 STEP 4b (2026-07-09): mvm/tree-walker.lisp is NO LONGER part of this
;; image — production eval is mvm-eval only.  The full-corpus + gauntlet census
;; measured ZERO %e2ic walker-fallback hits (the earlier "-142 fallback
;; inventory" was the :li-func offset-0 phantom, fixed in a07fe7d), and the
;; walker-free image gates clean (16335-16336 / CHUNK-CRASH=0 / FILE-WEDGE=30,
;; gauntlet 243/243 x2 at 11 FAILFORMs).  tree-walker.lisp remains the eval
;; engine of the four legacy fork builds ONLY.  If %e2ic-compile ever fails on
;; a new shape it signals honestly (UNDEFINED-FUNCTION via the NIL fn sentinel).

;;; ============================================================
;;; HOSTED ACTORS (x86-64) — net/actors.lisp's arch adapter
;;; ============================================================
;;;
;;; net/actors.lisp is architecture-independent but has only ever been linked
;;; into BARE-METAL images, because its twelve address hooks were only ever
;;; supplied by a board file handing out fixed physical addresses.  A hosted
;;; process has no such RAM, so net/hosted-actors.lisp derives the same
;;; addresses by SHRINKING REGION 0 and using the top of the semispaces that
;;; frees — the same carve mvm/gc.lisp's stage-1/2/3 selftests already use.
;;;
;;; x64 ONLY, and hosted only.  aarch64's per-CPU storage is TPIDR_EL1 (a
;;; system register the kernel does not let userspace write) rather than a GS
;;; base an ordinary arch_prctl can set, so the aarch64 CLI gets "" here and
;;; its blob is byte-identical to before.  Bare-metal targets already have a
;;; board file and do not want this one.
;;; THE ORDER IS LOAD-BEARING.  net/hosted-actors.lisp supplies the twelve
;;; address hooks and must precede net/actors.lisp (a forward reference across
;;; the blob does not resolve).  net/hosted-actors-post.lisp must FOLLOW it,
;;; because its SPIN-LOCK / SPIN-UNLOCK / AP-SCHEDULER are last-defun-wins
;;; overrides of definitions net/actors.lisp itself makes.
(defvar *cli-hosted-actors-source*
  (if (and (eq *cli-arch* :x64) (not *cli-bare-metal*))
      (concatenate 'string (string #\Newline)
                   (mvm-text "net/hosted-actors.lisp")
                   (string #\Newline)
                   (mvm-text "net/actors.lisp")
                   (string #\Newline)
                   (mvm-text "net/hosted-actors-post.lisp")
                   (string #\Newline)
                   ;; REAL TIME AND REAL BLOCKING.  Last in the group, because
                   ;; its SLEEP is a last-defun-wins override of the CL
                   ;; bridge's no-op stub and its %THR-PAGE calls SPIN-LOCK
                   ;; (net/actors.lisp) and %HA-ZERO (net/hosted-actors.lisp).
                   (mvm-text "net/hosted-sync.lisp")
                   (string #\Newline)
                   ;; THE SOCKET LAYER ON TWO CPUs.  Last in the group, and
                   ;; that is load-bearing three times over: its %SOCK-IO-BUF /
                   ;; %SOCK-ADDR-BUF / %SOCK-IO-CAP are last-defun-wins
                   ;; overrides of the seam net/hosted-sockets.lisp defines
                   ;; (which is baked inside *BRIDGE-SOURCE*, well above this);
                   ;; it calls SPIN-LOCK (net/actors.lisp) and %THR-CPU /
                   ;; %SLEEP-MS / %MONOTONIC-NS (net/hosted-sync.lisp), neither
                   ;; of which resolves as a forward reference; and it spawns
                   ;; thread 2 through %HA-SPAWN-T2 (net/hosted-actors-post).
                   ;; "" on aarch64 and on bare metal with the rest of the
                   ;; group, so those images keep the single-buffer socket
                   ;; layer and their blobs are unaffected.
                   (mvm-text "net/hosted-sockets-post.lisp")
                   (string #\Newline)
                   ;; THE AOT HALF OF AN A/B.  test/hosted-intern-layers.lisp's
                   ;; `low' arm — a worker interning fresh symbols through
                   ;; %INTERN-SYMBOL-PKG — dies about half the time while its
                   ;; cross-region audit reads ZERO, and the green test that
                   ;; interns through the SAME function differs from it in
                   ;; exactly one unmeasured way: the green one's loop is
                   ;; compiled INTO THE IMAGE.  This file is that loop, in the
                   ;; image, so the difference can be measured instead of
                   ;; argued.  Last in the group: it calls %INTERN-SYMBOL-PKG
                   ;; (prelude), %GC-* (mvm/gc.lisp) and CL:INTERN /
                   ;; CONCATENATE (the CL bridge), none of which resolve as
                   ;; forward references across the blob.  "" on aarch64 and on
                   ;; bare metal with the rest of the group.
                   (mvm-text "net/hosted-intern-probe.lisp")
                   (string #\Newline))
      ""))

(format t "  prelude: ~D chars~%" (length *prelude-source*))
(format t "  gc:      ~D chars~%" (length *gc-source*))
(format t "  hosted-actors: ~D chars~%" (length *cli-hosted-actors-source*))
(format t "  rt:      ~D chars~%" (length *rt-source*))
(format t "  rtest:   ~D chars~%" (length *rtest-pkg-source*))
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
;; WS5 #203: the runtime backquote expander was extracted to
;; lib/runtime-backquote.lisp so the aarch64 hosted CLI can share it instead of
;; the text living only inside this wrapper.  It is spliced back INTO
;; *driver-source* here, at exactly the position it used to occupy, so every
;; existing driver scanner (defun names -> SFT, symbol names -> *SYM-NAME-TABLE*)
;; still sees these defuns unchanged.
(defvar *driver-source*
 (concatenate 'string
  ;; ARCH SLOT: sys-exit / halt.  exit_group — the one that ends the PROCESS
  ;; and not merely the calling thread — is syscall 231 on x86-64, 94 on the
  ;; AArch64 generic ABI and 252 on i386.  Using plain `exit' (60/93/1) is a
  ;; hang the moment a second thread is alive; see the arch files.
  *cli-arch-syscall-source*
  (mvm-text "lib/runtime-backquote.lisp")
  ;; ARCH SLOT: arch-address diagnostic probes, baked ahead of kernel-main.
  *cli-arch-probe-source*
  "(defun kernel-main ()
"
  ;; ARCH SLOT: hardware setup that must precede the FIRST allocation
  ;; (aarch64 zeroes the runtime-metadata BSS slots and reserves the GC
  ;; object-start bitmap here; on x64 the boot preamble already did it).
  *cli-arch-kernel-prologue*
  ;; ---- SHARED boot init.  Identical on every arch, and the whole reason this
  ;; ---- file exists: task #245 (the missing (init-all-globals)) lived here.
  "  (init-symbol-table)
  (init-keyword-table)
  (%init-packages)
  (%init-streams)
"
  ;; BARE-METAL SEAM.  %init-streams ends with
  ;;   (setq *error-output* (%make-file-stream-full 2 1))
  ;; — a Linux fd-2 stream.  Writing one char to it runs %fs-write-char ->
  ;; %sys-write-raw -> (syscall3 1 …) -> TRAP #x0502, a literal SVC/SYSCALL with
  ;; no OS behind it.  That made reading ANY global by name fail at COMPILE time
  ;; (task #212): compile-variable-ref's implicit-global arm FORMATs to
  ;; *error-output* before it emits the read, so `*n*' faulted, and HANDLER-CASE
  ;; could not even be compiled to catch it.  There is exactly one console on a
  ;; bare board, so stderr is the serial port.
  (if *cli-bare-metal*
      "  (setq *error-output* *standard-output*)
"
      "")
  "  (%init-reader)
  (%init-condition-types)
  (%init-method-combinations)
  (%init-symbol-function-table)
  (%init-sft-auto)
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)
  (setq *macro-table* (make-hash-table))
  (%init-runtime-macros)
  (setq *cstr-scratch* #x0FE00000)  ; moved below heap base
  (setq *io-buf-addr*  #x0FF00000)  ; moved out of heap semispace 0; see memory note
"
  ;; BARE-METAL SEAM.  %init-signal-handling -> %install-signal-handlers ->
  ;; TRAP #x0520, which BOTH translators emit as unconditional rt_sigaction
  ;; syscalls (translate-aarch64.lisp ~2523 is NOT gated on
  ;; *aarch64-linux-mode*).  On bare metal that is an SVC with no OS to service
  ;; it — a synchronous exception, and on a Pi every vector is `b .', so it
  ;; wedges the machine silently.  Hardware-fault recovery on bare metal is the
  ;; boot's own vector table (x64: boot-x64.lisp IDT 13/14), not sigaction.
  (if *cli-bare-metal* "" "  (%init-signal-handling)
")
  "  (%init-signal-symbols)
  (%init-make-load-form)
  (%install-runtime-backquote)
  ;; Init RT counters + registry (defvar init thunks don't run on bare metal)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)
  (setq *rt-registered-tests* nil)
  (%install-deftest-macro)
  ;; Run all built-in defvar init thunks.  Each is wrapped in
  ;; handler-case at compile time so a thunk that references a not-yet-
  ;; bound symbol can't kill the chain — see CLAUDE.md known limitation
  ;; #7 history.  Most thunks succeed and we get init values for free.
"
  ;; BARE-METAL SEAM: an outer wrap as well.  A thunk that ESCAPES the
  ;; compile-time handler-case is a recoverable Lisp error on a hosted image and
  ;; an unrecoverable spin on a board whose exception vectors are `b .'.
  (if *cli-bare-metal*
      "  (handler-case (init-all-globals) (t (c) nil))
"
      "  (init-all-globals)
")
  ;; ARCH SLOT: file-I/O scratch addresses, spliced HERE — after
  ;; (init-all-globals), deliberately.
  ;;
  ;; FINDING (2026-08, i386 convergence): the two `(setq *cstr-scratch* ...)' /
  ;; `(setq *io-buf-addr* ...)' lines ~20 lines above are DEAD CODE on both
  ;; 64-bit ports.  init-all-globals re-runs cl-fileio.lisp's defvar init
  ;; thunks, which restore #x1DF00000 / #x1DE00000; a running ./modus reports
  ;; exactly those, not the 0x0FE00000 pair the setqs name.  It is harmless
  ;; there only because the 64-bit BSS happens to cover both addresses.  It is
  ;; NOT harmless on a port with a smaller BSS: i386 reserves through
  ;; +linux-i386-bss-end+ = 0x10020000, so 0x1DF00000 is unmapped and every
  ;; %string-to-cstr would fault.  Hence a slot that lands AFTER the thunks.
  ;; The dead lines above are left byte-for-byte alone so the two shipping
  ;; 64-bit images stay identical; fixing them is a separate, gated change.
  ;;
  ;; x64/aarch64 pass "" here, so this splice adds nothing to their blobs.
  *cli-arch-io-scratch-source*
  "  ;; ANSI numeric/array constants whose DEFCONSTANT init thunks don't run
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
  ;; CHAR-CODE-LIMIT.  Modus characters are a 21-bit code field in the
  ;; character immediate (runtime/tags.lisp), and CODE-CHAR/CHAR-CODE
  ;; round-trip exactly across the whole Unicode range (65, 255, 256,
  ;; 1000, 65535, 65536, 100000, 1114111 all verified), so the CLHS-
  ;; conformant value is the full Unicode codespace: #x110000.  It was
  ;; UNBOUND in the shipping image (limitation #7 — DEFCONSTANT init
  ;; thunks don't run at boot), which broke babel src/strings.lisp:42:
  ;; a CASE over (eval char-code-limit) that accepts EXACTLY #x100 /
  ;; #x10000 / #x110000 and errors otherwise.  #x110000 selects babel's
  ;; full-unicode string path.
  ;; NOTE: this whole kernel-main is inside a Lisp STRING literal — never
  ;; put a double-quote character in these comments.
  (setq char-code-limit         #x110000)
  ;; lambda-list-keywords: a standard CL constant.  alexandria's macros.lisp
  ;; reads `#.(set-difference lambda-list-keywords '(...))' at READ time — if
  ;; unbound the whole form is silently dropped, cascading to undefined
  ;; parse-ordinary-lambda-list and downstream defuns.  (Same read-time class
  ;; as array-dimension-limit above; the aarch64 builds already set this.)
  (setq lambda-list-keywords    '(&allow-other-keys &aux &body &environment &key
                                   &optional &rest &whole))
  (setq multiple-values-limit   16)
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
  ;; Compiler-macro NAME SET.  *%compiler-macro-hashes* is a DEFVAR whose
  ;; init form does not run at boot (MVM Active Limitation 7) — in this
  ;; image the variable read back UNBOUND, so %COMPILER-MACRO-P was
  ;; always NIL and MACRO-FUNCTION reported the compiler-implemented CL
  ;; macros that have NO runtime expander entry (LOOP DEFUN DEFMACRO
  ;; DEFVAR DEFSTRUCT MULTIPLE-VALUE-BIND HANDLER-CASE HANDLER-BIND
  ;; RESTART-CASE WITH-OPEN-FILE WITH-OUTPUT-TO-STRING
  ;; WITH-INPUT-FROM-STRING LAMBDA) as ORDINARY FUNCTIONS.  A library
  ;; code walker that dispatches on MACRO-FUNCTION then treats
  ;; (handler-case …) as a call.  The x64-linux / x64 / aarch64* / i386
  ;; images have called this in their own kernel-main all along; the
  ;; generic images never did.  Same class as the char-code-limit and
  ;; *gensym-counter* fixes above: an explicit call, not an init form.
  ;; MUST come after init-all-globals (which would re-NIL the defvar).
  (init-compiler-macro-set)
  ;; tar.lisp's *tar-block-size* defvar init-thunk doesn't run at boot (MVM
  ;; Active Limitation 7); set it so the baked tar reader works.  This is a
  ;; general library primitive, NOT ql wiring — the QL package + ql:quickload
  ;; come only from a runtime (load) of modus-quicklisp/setup.lisp.
  (setq *tar-block-size* 512)
  ;; WS5 rung 2: initialize the native JIT if it was baked (MODUS_USE_JIT=1).
  ;; %jit-boot-init is baked to a no-op when JIT is off, or to the translator
  ;; table co-init + (setq *use-jit* t) when JIT is on.  Wrapped so a JIT-init
  ;; crash can never take down a normal boot.
  (handler-case (%jit-boot-init) (t (c) nil))
  ;; :GENERA — install the Genera compatibility surface and push the feature.
  ;; MUST come after %install-runtime-cl-macros (the compat source uses
  ;; DOLIST / WHEN / UNLESS / SETF) and before cli-toplevel, so that
  ;; ~/.modusrc, --load and --eval all already see :genera on *features*.
  ;; Wrapped: a failure here must never take down a normal boot — it just
  ;; leaves Modus unrecognised, which is the pre-#237 status quo.
  ;; MODUS_NO_GENERA=1 skips it (see %install-genera-compat).
  (handler-case (%install-genera-compat) (t (c) nil))
  ;; THE SBCL COMPATIBILITY SURFACE — SB-THREAD, SB-BSD-SOCKETS, SB-POSIX,
  ;; SB-SYS, SB-ALIEN.  MUST come after %install-runtime-cl-macros (the source
  ;; uses DEFCLASS / DOLIST / WHEN / UNLESS / LOOP) and BEFORE cli-toplevel, so
  ;; that ~/.modusrc, --load, --script and --eval all see the packages BEFORE
  ;; any form mentioning them is READ.  That ordering is not a nicety: a package
  ;; that does not exist at read time is a READER-ERROR on the whole form, so a
  ;; script saying `sb-thread:make-thread' does not fail at the call, it fails
  ;; at the read.  Wrapped: a failure here must never take down a normal boot.
  ;; MODUS_NO_SB=1 skips it.
  (handler-case (%install-sb-shims) (t (c) nil))
  ;; RTEST — the RT regression tester package (mvm/rtest.lisp).  MUST be
  ;; created HERE, at boot, from image code: a package born at runtime is
  ;; marked runtime-born and its symbols get package-folded function-table
  ;; keys, so a runtime-born RTEST would give RTEST:DO-TESTS a different key
  ;; than the DO-TESTS compiled into this image and every library suite that
  ;; inherits it through (:use :rtest) would hit UNDEFINED-FUNCTION.  Also
  ;; overrides the deftest macro installed by %install-deftest-macro above
  ;; with the RT-faithful one (lazy form, literal expected values).  Wrapped:
  ;; a failure here must never take down a normal boot.
  (handler-case (%init-rtest) (t (c) nil))
  ;; ASDF INTERFACE — the ASDF / UIOP / ASDF-USER packages and entry points
  ;; over Modus's OWN loader (net/asdf-interface.lisp).  MUST come after
  ;; %install-runtime-cl-macros (the source uses DOLIST/WHEN/UNLESS/DEFCLASS)
  ;; and before cli-toplevel, so ~/.modusrc, --load and --eval all see the
  ;; :asdf* features and can (install-tarball) a system whose .asd opens with
  ;; a read-time `#.(version<= \"3.1\" (asdf-version))' guard.  Wrapped: a
  ;; failure here must never take down a normal boot.  MODUS_NO_ASDF=1 skips.
  (handler-case (%install-asdf-interface) (t (c) nil))
"
  ;; ARCH SLOT: the toplevel entry / probe program.
  *cli-arch-kernel-epilogue*))

;;; ============================================================
;;; :GENERA compatibility surface (task #237)
;;;
;;; Modus advertises :GENERA.  The rationale, the honest-degeneracy list and
;;; the ladder measurement live in net/genera-compat.lisp's header.
;;;
;;; WHY THIS IS BAKED AS A SOURCE **STRING** EVALUATED AT BOOT, rather than
;;; compiled into the image like every other file above:
;;;
;;;   The Genera surface is definitions of symbols in packages that do not
;;;   exist outside a running Modus — `scl::locf', `sys::store-conditional',
;;;   `process::atomic-incf'.  Every first-party build source is read by
;;;   CHECK-PARSES with SBCL's reader, which rejects those with "Package SCL
;;;   does not exist"; and the build blob itself is read host-side, so the
;;;   same wall stands one layer down.  Making the MVM compiler able to
;;;   compile non-CL package-qualified definitions from build source is real
;;;   work and is NOT this change.  So the text is carried as a literal and
;;;   handed to %IT-EVAL-SOURCE — the SAME code path a `--load' of the file
;;;   takes, which is exactly how the ladder result was measured.
;;;
;;;   MEASURED COST: +386 ms on a 1143 ms boot (+34%), of which ~305 ms is
;;;   genera-compat.lisp and ~81 ms is cooperative-atomics.lisp.  That is the
;;;   honest current price of a runtime-evaluated prelude; the way to remove
;;;   it is to teach the compiler the packages, not to trim the shim.
;;;
;;;   ESCAPE HATCH: MODUS_NO_GENERA=1 in the environment skips the install
;;;   entirely — no :genera, no packages, boot cost back to baseline.  Same
;;;   reversible-flip pattern as MODUS_NO_EVAL2.
;;; ============================================================

(defun %escape-lisp-string (text)
  "Escape TEXT so it can be emitted as a Lisp string literal."
  (with-output-to-string (out)
    (loop for c across text
          do (cond ((char= c #\\) (write-string "\\\\" out))
                   ((char= c #\") (write-string "\\\"" out))
                   (t (write-char c out))))))

(defvar *genera-compat-text*
  (concatenate 'string
               (read-file-text (merge-pathnames "net/cooperative-atomics.lisp"
                                                *modus-base*))
               (string #\Newline)
               (read-file-text (merge-pathnames "net/genera-compat.lisp"
                                                *modus-base*))))

(defvar *genera-source*
  (concatenate 'string "
(defun %genera-compat-source ()
  \"" (%escape-lisp-string *genera-compat-text*) "\")

(defun %install-genera-compat ()
  ;; MODUS_NO_GENERA=1 => do not advertise :genera and do not create the
  ;; Genera packages.  Everything downstream keys off the feature, so this
  ;; one check is the whole rollback.
  (let ((off (%cli-getenv \"MODUS_NO_GENERA\")))
    (if (and off (> (length off) 0) (not (string= off \"0\")))
        nil
        (progn (%it-eval-source (%genera-compat-source) \"genera-compat\") t))))
"))

(format t "  genera:  ~D chars (compat source baked for boot-time eval)~%"
        (length *genera-compat-text*))

;;; ============================================================
;;; THE SBCL COMPATIBILITY SURFACE (SB-THREAD, SB-BSD-SOCKETS, SB-POSIX,
;;; SB-SYS, SB-ALIEN)
;;;
;;; net/sb-thread-shim.lisp and net/sb-sys-shim.lisp have the rationale and
;;; the PARTIAL list.  Baked as boot-evaluated SOURCE STRINGS for the same
;;; reason the Genera surface is, and for one more: these packages EXIST on
;;; the host.  `(defpackage "SB-THREAD" …)' read and evaluated host-side
;;; would collide with SBCL's own, and `(defun sb-thread:make-thread …)'
;;; would be a package-lock violation.  Carried as a literal, nothing
;;; host-side ever reads a form in them.
;;;
;;; ORDER: THREAD BEFORE SYS.  The socket shim's SOCKET-MAKE-STREAM and the
;;; thread shim are independent, but %SB-THREADS-UP is the thing that turns
;;; per-CPU regions on, and anything that wants a thread wants that first.
;;;
;;; WHY IT IS INSTALLED AT ALL, given that modus advertises :GENERA and not
;;; :SBCL.  Because `#+sb-thread' in portable code — glass/fb's framebuffer
;;; and clipboard locks are exactly this — is a question about a SURFACE, not
;;; about a vendor.  With the surface present, that code takes the same arm
;;; SBCL takes, which is the arm it is tested on.  The features pushed are
;;; :SB-THREAD and :SB-BSD-SOCKETS and NOT :SBCL: modus is not SBCL and code
;;; that asks whether it is still gets the right answer.
;;;
;;; NOTHING HERE LISTENS, CONNECTS OR STARTS A THREAD.  Installing the shim
;;; creates packages and defines functions.  A socket is opened when a caller
;;; makes one; a thread starts when a caller asks for one.
;;;
;;; ESCAPE HATCH: MODUS_NO_SB=1 skips both entirely — no packages, no
;;; features, boot cost back to baseline.  Same reversible-flip pattern as
;;; MODUS_NO_GENERA.
;;; ============================================================

(defvar *sb-shim-text*
  (concatenate 'string
               (read-file-text (merge-pathnames "net/sb-thread-shim.lisp"
                                                *modus-base*))
               (string #\Newline)
               (read-file-text (merge-pathnames "net/sb-sys-shim.lisp"
                                                *modus-base*))))

(defvar *sb-shim-source*
  (concatenate 'string "
(defun %sb-shim-source ()
  \"" (%escape-lisp-string *sb-shim-text*) "\")

(defun %install-sb-shims ()
  (let ((off (%cli-getenv \"MODUS_NO_SB\")))
    (if (and off (> (length off) 0) (not (string= off \"0\")))
        nil
        (progn (%it-eval-source (%sb-shim-source) \"sb-shims\") t))))
"))

(format t "  sb-shim: ~D chars (sb-thread/sb-bsd-sockets source baked for boot-time eval)~%"
        (length *sb-shim-text*))

;;; ============================================================
;;; ASDF INTERFACE over Modus's own loader
;;;
;;; net/asdf-interface.lisp's header has the rationale and the honest
;;; degeneracy list.  Baked as a boot-evaluated SOURCE STRING for the SAME
;;; reason the Genera surface above is: it defines `asdf::load-system' and
;;; `uiop::version<=', and CHECK-PARSES reads first-party build source with
;;; SBCL's reader, which has no ASDF package to resolve those against.  The
;;; compiled half — version arithmetic, .asd reading, component ordering —
;;; already lives in lib/install-tarball.lisp under `%it-' names, so what is
;;; evaluated here is only the naming layer.
;;;
;;; ESCAPE HATCH: MODUS_NO_ASDF=1 skips the install entirely (no ASDF /
;;; UIOP / ASDF-USER packages, no :asdf* features) — same reversible-flip
;;; pattern as MODUS_NO_GENERA.
;;; ============================================================

(defvar *asdf-interface-text*
  (read-file-text (merge-pathnames "net/asdf-interface.lisp" *modus-base*)))

(defvar *asdf-source*
  (concatenate 'string "
(defun %asdf-interface-source ()
  \"" (%escape-lisp-string *asdf-interface-text*) "\")

(defun %install-asdf-interface ()
  (let ((off (%cli-getenv \"MODUS_NO_ASDF\")))
    (if (and off (> (length off) 0) (not (string= off \"0\")))
        nil
        (progn (%it-eval-source (%asdf-interface-source) \"asdf-interface\") t))))
"))

(format t "  asdf:    ~D chars (interface source baked for boot-time eval)~%"
        (length *asdf-interface-text*))

(defvar *all-runtime-source*
  (concatenate 'string *prelude-source*  (string #\Newline)
                       *gc-source*       (string #\Newline)
                       *mcgc-pin-source*
                       *rt-source*       (string #\Newline)
                       *rtest-pkg-source* (string #\Newline)
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
                       ;; WS5 rung 2: register the JIT translator's defuns +
                       ;; quoted symbols in the SFT / sym-name tables so
                       ;; %init-x64-translator / translate-mvm-to-x64 are
                       ;; runtime-reachable and their quoted reg symbols
                       ;; (rax, :e, …) have recoverable names.  "" when JIT off.
                       *cli-arch-jit-translator-source*
                       *jit-boot-source* (string #\Newline)
                       *stage2-test-source* (string #\Newline)
                       *rt-macros-source* (string #\Newline)
                       *bridge-source*   (string #\Newline)
                       *cli-arch-override-source*
                       ;; HOSTED ACTORS: "" on aarch64 and on bare metal, so
                       ;; their scanner input — hence SFT-AUTO and
                       ;; SYM-NAME-AUTO — is byte-identical to before.
                       *cli-hosted-actors-source*
                       ;; BARE-METAL NET SEAM, second use site.  The scanners
                       ;; below are what put a defun in *SYMBOL-FUNCTION-TABLE*
                       ;; and a token in *SYM-NAME-TABLE*; a bare-metal target's
                       ;; OWN driver/IP/HTTP/installer stack is real image source
                       ;; and must be scanned like any other, or every one of its
                       ;; functions is unreachable from runtime EVAL / the REPL
                       ;; and its symbols print as :||.  "" on hosted builds, so
                       ;; their scanner input — and therefore SFT-AUTO and
                       ;; SYM-NAME-AUTO — is byte-identical.
                       *cli-bare-metal-net-source*
                       *driver-source*))

;;; NOTE: *genera-source* is deliberately NOT part of *all-runtime-source*.
;;; That variable exists only to feed the SCANNERS (scan-defuns -> the
;;; runtime symbol-function table, scan-symbol-names -> *sym-name-table*),
;;; and %genera-compat-source's body is a 22 KB STRING LITERAL containing
;;; the compat source — including its `(defun sys::store-conditional …)'
;;; text.  scan-defuns is a textual scanner: it happily harvested those
;;; names out of the string literal and emit-sft-auto then emitted
;;; `#'sys::store-conditional', which does not exist, so the whole
;;; 200-function sft-auto chunk failed to compile and the build reported
;;; "1 × %INIT-SFT-AUTO-15" unresolved — silently dropping 200 functions
;;; from runtime EVAL's reach.  The two genera defuns are only ever called
;;; directly from kernel-main, so they need no SFT entry.



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
    ;; RTEST package — AFTER rt.lisp so its DO-TESTS wins (last-defun-wins).
    *rtest-pkg-source*
    (string #\Newline)
    *rt-macros-source*
    (string #\Newline)
    *bridge-source*
    (string #\Newline)
    ;; ARCH SLOT: late last-defun-wins overrides.  Must follow the bridge (they
    ;; override cl-fileio.lisp and lib/cli-toplevel.lisp, both baked inside it).
    ;; "" on x64.
    *cli-arch-override-source*
    ;; HOSTED ACTORS (x86-64 hosted only; "" elsewhere).  POSITION IS
    ;; LOAD-BEARING: after mvm/gc.lisp (whose %gc-region-* / %gc-read64 it
    ;; calls — a forward reference across the blob does not resolve) and after
    ;; the CL bridge (whose CONS/CAR/CONSP it uses), and before the compiler so
    ;; nothing here can shadow a compiler internal.
    *cli-hosted-actors-source*
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
    ;; WS5 rung 2: OPTIONAL native JIT translator (only under MODUS_USE_JIT=1;
    ;; each var is "" when JIT is off → the default image is byte-identical).
    *cli-arch-jit-translator-source*
    *stage2-test-source*
    (string #\Newline)
    ;; BARE-METAL NET SEAM.  "" on hosted builds, so their blob is unchanged.
    ;; A bare-metal image has no host kernel to syscall into, so it bakes its own
    ;; driver/IP/HTTP stack + tarball installer here.  POSITION IS LOAD-BEARING:
    ;; after the CL runtime, the compiler and mvm-eval so the arch adapter's own
    ;; definitions win under last-defun-wins (it deliberately overrides
    ;; WRITE-BYTE), and BEFORE *driver-source*, whose kernel-main calls
    ;; run-net-pipeline.
    *cli-bare-metal-net-source*
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
    ;; :GENERA surface (task #237).  BEFORE *driver-source*, because
    ;; kernel-main lives in the driver and calls %install-genera-compat:
    ;; a forward reference across the blob does NOT resolve — the first
    ;; attempt placed this after the driver and the build reported
    ;; "WARN li-func: unresolved function %INSTALL-GENERA-COMPAT —
    ;; emitting NIL sentinel", i.e. a silent no-op boot hook.
    *genera-source*
    (string #\Newline)
    ;; THE SBCL COMPATIBILITY SURFACE.  Same placement rule and the same
    ;; two reasons as *genera-source*: BEFORE *driver-source* because
    ;; kernel-main calls %install-sb-shims and a forward reference across
    ;; the blob emits a NIL sentinel (a silent no-op boot hook), and NOT in
    ;; *all-runtime-source* because its body is one large string literal
    ;; that the TEXTUAL scan-defuns scanner would mine for names like
    ;; `sb-thread::make-thread' and then emit `#'sb-thread::make-thread'
    ;; into a chunk that cannot compile.
    *sb-shim-source*
    (string #\Newline)
    ;; ASDF interface (net/asdf-interface.lisp).  Same placement rule as
    ;; *genera-source*: BEFORE *driver-source*, because kernel-main calls
    ;; %install-asdf-interface and a forward reference across the blob does
    ;; not resolve (it emits a NIL sentinel — a silent no-op boot hook).
    ;; Also, like *genera-source*, deliberately NOT in *all-runtime-source*:
    ;; that variable feeds the TEXTUAL scan-defuns scanner, which would
    ;; harvest `asdf::load-system' & co. out of the string literal and emit
    ;; `#'asdf::load-system' into a sft-auto chunk that cannot compile,
    ;; silently dropping ~200 functions from runtime EVAL's reach.
    *asdf-source*
    (string #\Newline)
    *driver-source*
    (string #\Newline)
    ;; WS5 rung 2: baked JIT boot hook + gate (LAST so %jit-enabled-p wins).
    *jit-boot-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; BLOB READ CHECK — the assembled source must READ CLEANLY
;;; ============================================================
;;;
;;; CLAUDE.md's `check-parses' guards each first-party FILE.  Nothing guarded
;;; the ASSEMBLED BLOB, and a wrapper's arch slot can unbalance it without
;;; touching any file: this check was added after a duplicated
;;; `(defun %cli-argv-base ()' line in the aarch64 slot left an open form that
;;; swallowed source to EOF.  The build reader (cross.lisp
;;; READ-ALL-FORMS-WITH-LOCATIONS) is DELIBERATELY lenient -- it must be, since
;;; the same reader ingests ANSI fixtures whose packages need not exist
;;; host-side -- so it printed two "SKIP read at line ..." notes, dropped a few
;;; hundred functions, and built a perfectly valid binary that SIGSEGV'd on a
;;; NIL sentinel before its first write().  Cost: a 20-minute build plus a
;;; qemu -strace session to find what a 30-second check reports directly.
;;;
;;; Scope is deliberately the TWO CLI WRAPPERS, not BUILD-IMAGE.  A check added
;;; inside build-image changes all 29 build scripts at once, and the ANSI gate
;;; runners legitimately carry corpus text this reader skips -- see
;;; [[reference_build_ratchet_corpus_images]], where a fatal-at-0 check in
;;; build-image made all four gate runners unbuildable.
(let* ((log (with-output-to-string (*standard-output*)
              (modus.mvm::read-all-forms-with-locations *full-source*)))
       (skips (let ((n 0) (pos 0))
                (loop
                  (let ((p (search "SKIP read at line" log :start2 pos)))
                    (unless p (return n))
                    (incf n)
                    (setq pos (+ p 17)))))))
  (if (zerop skips)
      (format t "Blob read check: OK (0 skipped forms)~%")
      (error "~&BLOB READ CHECK FAILED: the assembled *full-source* has ~D~%~
              unreadable form(s).  The build reader is lenient, so these would~%~
              be SILENTLY DROPPED and the image would fault on a NIL sentinel.~%~
              Almost always an unbalanced paren or unterminated string in a~%~
              *CLI-ARCH-* slot in the wrapper.  Reader output:~%~A"
             skips log)))

;;; MODUS_DUMP_FULL_SOURCE=<path> — write the assembled blob and STOP, without
;;; building an image.  This is the refactor gate: the blob is the ONLY thing a
;;; wrapper contributes to the emitted binary, so a wrapper refactor that leaves
;;; it byte-identical is behaviour-preserving by construction, and proving that
;;; takes ~20s instead of a ~10min image build.  Placed AFTER assembly and
;;; BEFORE build-image so it cannot perturb what it measures.
#+sbcl
(let ((p (sb-ext:posix-getenv "MODUS_DUMP_FULL_SOURCE")))
  (when (and p (plusp (length p)))
    (with-open-file (o p :direction :output :if-exists :supersede
                         :external-format :utf-8)
      (write-string *full-source* o))
    (format t "Dumped *full-source* to ~A (~D chars); skipping image build.~%"
            p (length *full-source*))
    (sb-ext:exit :code 0)))

