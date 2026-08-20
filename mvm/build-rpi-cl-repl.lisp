;;;; build-rpi-cl-repl.lisp — the first BARE-METAL RASPBERRY PI image that runs
;;;; the REAL CL (task #209 rung 1).
;;;;
;;;; A Raspberry Pi 3B (BCM2837, AArch64) kernel8.img that drops into Modus's
;;;; own self-hosted Common Lisp REPL over the PL011 serial port: the CL
;;;; reader, `eval' = mvm-eval (compile -> MVM bytecode -> mvm-interpret), and
;;;; the CL printer.  There is NO second Lisp here — `mvm/repl-source.lisp'
;;;; (the 708-line toy reader/printer/tree-walker that every existing
;;;; build-rpi-* and build-pizero2w-* script bakes) is not loaded and not
;;;; referenced.
;;;;
;;;; This is the RPi family's migration ONTO the CL/mvm image, which is the
;;;; standing instruction for #209 — NOT a repair of the legacy `:RPI'
;;;; repl-source path.
;;;;
;;;; LINEAGE.  This is `build-x64-cl-repl.lisp' (the clean bare-metal x64 CL
;;;; REPL, itself build-generic-cli retargeted to bare metal) moved to the Pi
;;;; board.  The CL stack, the in-image compiler, the scanners, the driver and
;;;; kernel-main's init order are IDENTICAL — deliberately, so the two bare
;;;; CL images stay diffable.  The deltas are exactly:
;;;;
;;;;   - boot/boot-rpi-cl.lisp + (build-image :target :rpi)  [kernel8.img
;;;;     @ 0x80000, MMU OFF, identity addressing].  boot-rpi-cl.lisp defines a
;;;;     new entry emitter and redefines `rpi-boot-descriptor' in THIS process
;;;;     only; no shared file is touched, so the shipped x64/aarch64 CLI
;;;;     binaries stay byte-identical by construction.
;;;;   - the AArch64 translator in bare-metal mode instead of the x64 one:
;;;;     TRAP #x0300/#x0301 emit PL011 MMIO instead of COM1 port I/O.
;;;;   - (%gc-init #x09000000 #x07000000 #x08000000) in kernel-main — the
;;;;     Cheney metadata publish the x64 bare image gets from boot-x64.lisp's
;;;;     kernel64 entry instead.
;;;;   - `halt' is a WFI loop (TRAP #x0304), not `(loop (hlt))'.
;;;;   - no NX-on-data-pages knob and no IDT: the x64 image relies on
;;;;     boot-x64.lisp's IDT 13/14 to turn a wild (funcall 42) into a recovered
;;;;     Lisp error.  The Pi vectors all spin (see boot-rpi-cl.lisp), so a
;;;;     HARDWARE fault wedges the machine here.  Software-signalled Lisp
;;;;     errors unwind through handler-case normally.
;;;;
;;;; MEMORY MAP.  Byte-for-byte the same VAs as the QEMU-virt bare image, which
;;;; is what makes this a port and not a redesign — but on the Pi they are
;;;; plain identity-mapped DRAM rather than an MMU remap, because Pi DRAM
;;;; starts at 0 (QEMU virt's starts at 0x40000000).  Image @0x80000, stack top
;;;; 0x08000000 growing down, Cheney heap [0x09000000, 0x10000000) with the
;;;; semispace midpoint at 0x0C800000, runtime metadata ("BSS") at 0x1000xxxx,
;;;; peripherals at 0x3F000000+.  Everything must stay below 0x3F000000.
;;;; raspi3b has a FIXED 1 GiB of RAM (QEMU rejects a mismatched -m), so the
;;;; headroom is real on both QEMU and hardware.
;;;;
;;;; SERIAL IS PL011 (0x3F201000), NOT the mini UART at 0x3F215040 that
;;;; CLAUDE.md documents for the Pi.  This is FORCED, not preferred:
;;;; `read-char-serial' (TRAP #x0301) is hardcoded to the PL011 register layout
;;;; in translate-aarch64.lisp (~1843) — poll UARTFR at +0x18 for RXFE bit 4.
;;;; Transmit is parameterized (*aarch64-serial-width* / -tx-poll) so the mini
;;;; UART can print, which is why the legacy build-rpi-periph image works, but
;;;; there is no *aarch64-serial-rx-poll* and a REPL needs input.  Adding that
;;;; parameter is rung-2 work and is REQUIRED for the Pi Zero 2 W, whose only
;;;; console is the mini UART.
;;;;
;;;; Usage: sbcl --dynamic-space-size 8192 --script mvm/build-rpi-cl-repl.lisp
;;;; Run:   qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \
;;;;          -serial stdio -serial null -display none
;;;;        (PL011 is serial_hd(0) on raspi3b; the legacy mini-UART images use
;;;;         the reverse, -serial null -serial stdio.)
;;;; Output path override: MODUS_CL_REPL_OUT.

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

(format t "~%=== Building bare-metal RPi 3B CL REPL image ===~%")

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
    ;; next file of the concatenated build blob.  This build bakes
    ;; x64-asm.lisp (:modus.asm), translate-x64.lisp (:modus.mvm.x64),
    ;; mvm.lisp, compiler.lisp, interp.lisp, prelude.lisp and gc.lisp — the
    ;; build reader HONOURS in-package, and it is handed one concatenated
    ;; blob, so without this each declaration would set the read package for
    ;; every file after it.  See modus.mvm::*build-package-reset-text*; keep
    ;; every mvm-text copy identical.
    (modus.mvm::%build-package-scoped-source (read-file-text path))))

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
;;; Runtime JIT: OFF for bare metal
;;; ============================================================
;; The runtime JIT is OFF here.  %jit-translate-page mmaps a
;; PROT_EXEC page and mprotects it — there is no mmap on bare metal, so the
;; native-code seam is a later rung of #204.  eval = mvm-eval = compile ->
;; MVM bytecode -> mvm-interpret, which is exactly what the bare ANSI gate
;; (build-x64.lisp) already runs.
(defvar *jit-on* nil)

(defvar *x64-asm-source* (when *jit-on* (mvm-text "mvm/x64-asm.lisp")))
;; Shrink the code-buffer default from 96MB to 64KB (grows on demand) so a JIT
;; page translation doesn't try to alloc ~768MB tagged per make-code-buffer.
(when *jit-on*
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
  (when *jit-on*
    (let ((src (mvm-text "mvm/translate-x64.lisp"))
          (marker "(defun install-x64-translator"))
      (let ((pos (search marker src)))
        (unless pos (error "WS5-JIT: could not find install-x64-translator strip marker"))
        ;; #211: the trim cuts off mvm-text's trailing reset — re-append it, or
        ;; translate-x64.lisp's (in-package :modus.mvm.x64) leaks into whatever
        ;; is concatenated next.
        (concatenate 'string (subseq src 0 pos)
                     modus.mvm::*build-package-reset-text*)))))
;; Co-init that populates the translator's defvar lookup tables at boot AND sets
;; the runtime JIT globals.  Byte-for-byte the same table data the ANSI gate
;; installs (build-ansi-common-x64.lisp *x64-translator-coinit-source*), plus a
;; RUNTIME (setq *x64-gc-enabled* …) mirroring the value the host baked into the
;; image's fixed code — limitation #7 means the file-tail (setf …) does NOT reach
;; the image runtime, so %jit-translate-page would otherwise read the defvar
;; default (NIL) and emit gc-checks/trampoline inconsistently with the baked code.
(defvar *jit-coinit-source*
  (when *jit-on* "
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
  ;; This matches the ANSI gate's co-init (build-ansi-common-x64.lisp), whose
  ;; JIT battery runs native with offset 0.
  (setq *x64-native-code-offset* 0)
  (setq *x64-linux-mode* t)
  ;; RUNTIME JIT globals (limitation #7: file-tail host setf doesn't reach here).
  ;; Match the values the host baked into the image's fixed code so the JIT emits
  ;; consistent gc-check/trampoline code.
  (setq *x64-gc-enabled* t)
  (setq *linux-x64-r14-offset* #x38000000)
  (setq *jit-xlate-err-info* nil)
  t)
"))

;; Baked boot hook + JIT gate.  Appended LAST so its %jit-enabled-p wins over
;; mvm-eval.lisp's base version (last-defun).  When JIT is OFF both defuns are
;; no-ops (%jit-boot-init returns nil, %jit-enabled-p returns nil) so the seam
;; stays inert — and the whole string is "" only when JIT is off would change
;; layout, so we ALWAYS bake these two tiny defuns (JIT-off variant is inert
;; and keeps *use-jit* nil = interpret).
(defvar *jit-boot-source*
  (if *jit-on*
      "
(defun %jit-boot-init () (%init-x64-translator) (setq *use-jit* t) t)
(defun %jit-enabled-p () (and (boundp (quote *use-jit*)) *use-jit*))
"
      "
(defun %jit-boot-init () nil)
"))

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
;;; ------------------------------------------------------------
;;; BRING-UP CULLS (#209 rung 4).  Chain-loading a 20MB image over a
;;; 9600-baud mini-UART takes ~5.6 hours, so the bring-up image can drop
;;; payload that the bare Pi does not need YET.  Both knobs default OFF so
;;; every existing build stays byte-identical; set the env var to cull.
;;;
;;;   MODUS_RPI_NO_BLOB=1    drop the embedded self-source blob (~3.7MB).
;;;                          `read-embedded-source' is the only consumer and
;;;                          it is commented out at its single call site; the
;;;                          "MVMS" header is still emitted so the image
;;;                          footer's offset arithmetic stays valid.
;;;   MODUS_RPI_NO_BRIDGE=1  drop mvm/ansi-bridge.lisp (~1.7MB of source ->
;;;                          ~3.5MB of image).  Test scaffolding (eqlt, the
;;;                          rt-* harness, ANSI stand-ins) with one real
;;;                          consumer: cl-eval.lisp's (puthash "EQLT" ...).
;;;                          NOT proven boot-safe — measure, don't assume.
(defun %rpi-cull-p (var)
  (let ((v (sb-ext:posix-getenv var)))
    (and v (plusp (length v)) (not (string= v "0")))))
(defvar *rpi-no-blob* (%rpi-cull-p "MODUS_RPI_NO_BLOB"))
(defvar *rpi-no-bridge* (%rpi-cull-p "MODUS_RPI_NO_BRIDGE"))
(when *rpi-no-blob*
  (setf modus.mvm::*embed-source-blob* nil)
  (format t "~&;; CULL: embedded source blob DROPPED~%"))
(when *rpi-no-bridge*
  (format t "~&;; CULL: mvm/ansi-bridge.lisp DROPPED~%"))

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
    (if *rpi-no-bridge* "" (mvm-text "mvm/ansi-bridge.lisp"))
    ;; BARE METAL stops here.  The hosted payload build-generic-cli bakes
    ;; after ansi-bridge — lib/tar.lisp, lib/install-tarball.lisp,
    ;; net/hosted-{sockets,storage,http}.lisp and lib/cli-toplevel.lisp — is
    ;; all Linux-syscall / fd / argv machinery with no bare-metal meaning.
    ;; cli-toplevel in particular reads argv off the initial process stack
    ;; and opens fd 0; the serial driver below is its bare-metal counterpart.
    ""))
;;; ============================================================
;;; NET BUILD (MODUS_NET_BUILD=1) — #209 rung 2: HTTP over DWC2/USB
;;; ============================================================
;;;
;;; When enabled, append the Pi's USB net stack (arch adapter + DWC2 host
;;; controller + USB core + CDC Ethernet + IP/TCP/ARP/DHCP + HTTP client) to
;;; the image and drive a DHCP -> TCP -> HTTP GET pipeline from kernel-main.
;;; This is the DWC2 counterpart of build-aarch64.lisp's MODUS_NET_BUILD, which
;;; does the same thing over E1000 on QEMU virt; the driver text below is
;;; deliberately shaped like that one so the two stay diffable.
;;;
;;; EVERY var here is "" when the flag is off, and the kernel-main call site is
;;; a spliced "" too, so the default (rung 1) image is BYTE-IDENTICAL.
;;;
;;; No SSH and no crypto: a plain-HTTP fetch needs only the NIC, IP and the
;;; HTTP client.  No actors either — the fetch runs synchronously in
;;; kernel-main, so there is no yield/context-switch to corrupt cons cells
;;; (MVM active limitation #5).
;;;
;;; WHY THIS WORKS WHERE build-rpi-ssh DOES NOT.  The legacy repl-source RPi
;;; SSH image wedges under QEMU 7.2 at the FIRST USB control transfer
;;; (`D1E' = GET_DEVICE_DESCRIPTOR failed), and a `-trace usb_dwc2*' capture
;;; shows why: HCCHAR is written twice as 0x00000040, i.e. WITHOUT the CHENA
;;; enable bit, so QEMU is never asked to run a packet (zero usb_dwc2_packet_*
;;; events in 265k trace lines).  `(hcchar-chena)' is `(ash 1 31)', and
;;; compile-ash only INLINES a constant shift count <= 30 — 31 routes to the
;;; runtime function `bignum-ash', which does not exist in a repl-source image,
;;; so the constant evaluates to 0 and the OR is a no-op.  (`(ash 1 29)' for
;;; GUSBCFG force-host is <= 30 and does reach the register, which is why the
;;; controller inits and the port reports FS before it stalls.)  In THIS image
;;; bignum-ash is present — it comes in with mvm/cl-eval.lisp as part of
;;; *bridge-source*, which is concatenated BEFORE the net stack — so
;;; (ash 1 31) yields 2147483648, CHENA is set, and enumeration proceeds.
;;; The DWC2 driver was never broken; it was the runtime under it.
(defvar *net-build-p*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_NET_BUILD")))
    (and v (string= v "1"))))

(defvar *net-dir* (merge-pathnames "net/" *modus-base*))
(defun net-text (rel)
  (let ((p (merge-pathnames rel *net-dir*)))
    (modus.mvm::check-parses p)
    (read-file-text p)))

(defvar *net-source*
  (if *net-build-p*
      (concatenate 'string
        ;; arch-rpi-cl.lisp, NOT arch-raspi3b.lisp: the legacy adapter's DMA
        ;; regions at 0x01000000 land INSIDE this ~20 MB image, and it carries a
        ;; miniature make-array/aref/aset runtime that would replace the real CL
        ;; one under last-defun-wins.  See that file's header.
        (net-text "arch-rpi-cl.lisp")   (string #\Newline)
        (net-text "dwc2.lisp")          (string #\Newline)
        (net-text "usb.lisp")           (string #\Newline)
        (net-text "cdc-ether.lisp")     (string #\Newline)
        (net-text "ip.lisp")            (string #\Newline)
        (net-text "http-client.lisp")   (string #\Newline)
        ;; Bigger HTTP response buffer.  The stock http-fetch-impl caps a
        ;; response at 4096 bytes and tcp-rx-copy bounds its copy to 4096 — too
        ;; small for a tarball.  Default is the same 32 KB override
        ;; build-aarch64.lisp's net build uses, so the two images fetch
        ;; identically sized bodies; MODUS_NET_BUFSZ raises it for a bigger
        ;; library (alexandria's .tar is 276480 bytes).  Baked as a defun, not a
        ;; defvar: MVM active limitation #7 means a defvar initform never runs.
        (format nil "~%(defun %net-resp-cap () ~D)~%"
                (or #+sbcl (let ((v (sb-ext:posix-getenv "MODUS_NET_BUFSZ")))
                             (and v (plusp (length v)) (parse-integer v)))
                    32768))
        "
(defun tcp-rx-copy (dest dest-off)
  (let ((buf (e1000-rx-buf)))
    (let ((ip-total (buf-read-u16-mem buf 16))
          (tcp-hdr-len (ash (logand (mem-ref (+ buf 46) :u8) #xF0) -2)))
      (let ((data-len (- ip-total (+ 20 tcp-hdr-len))))
        (let ((data-base (+ (+ buf 34) tcp-hdr-len)))
          (let ((i 0))
            (loop
              (when (>= i data-len) (return data-len))
              (let ((dst-idx (+ dest-off i)))
                (when (< dst-idx (%net-resp-cap))
                  (aset dest dst-idx (mem-ref (+ data-base i) :u8))))
              (setq i (+ i 1))))
          data-len)))))
(defun http-fetch-impl (url url-len)
  (let ((scheme-end (url-skip-http url url-len)))
    (let ((host-end (url-host-end url scheme-end url-len)))
      (let ((port (url-parse-port url host-end url-len))
            (path-start (url-path-off url scheme-end url-len)))
        (let ((host-len (- host-end scheme-end))
              (path-len (- url-len path-start)))
          (let ((ip (resolve-host url scheme-end host-end)))
            (when (zerop ip)
              (write-byte 68) (write-byte 78) (write-byte 83)
              (write-byte 58) (write-byte 48) (write-byte 10)
              (return 0))
            (when (zerop (tcp-connect ip port))
              (write-byte 84) (write-byte 67) (write-byte 80)
              (write-byte 58) (write-byte 70) (write-byte 10)
              (return 0))
            (let ((req-buf (make-array 512)))
              (let ((req-len (http-build-get url scheme-end host-len
                                              path-start path-len req-buf)))
                (tcp-send req-buf req-len)))
            (let ((resp (make-array (%net-resp-cap)))
                  (resp-len 0)
                  (done 0)
                  (idle 0))
              (loop
                (when (not (zerop done)) (return 0))
                (let ((n (tcp-receive 300)))
                  (if (> n 0)
                      (progn
                        (let ((copied (tcp-rx-copy resp resp-len)))
                          (setq resp-len (+ resp-len copied)))
                        (setq idle 0))
                      (setq idle (+ idle 1))))
                (when (zerop (tcp-state)) (setq done 1))
                (when (> idle 20) (setq done 1)))
              (tcp-close)
              (cons resp resp-len))))))))
"
        (string #\Newline))
      ""))

;;; --- #263 rung 3: install + load the fetched library ------------------------
;;;
;;; lib/tar.lisp (ustar reader) + lib/install-tarball.lisp (untar -> find the
;;; .asd -> parse its :components -> topo-sort by :depends-on -> read+eval each
;;; source file).  BAKED into the image, not runtime-(load)ed, for two reasons:
;;;
;;;   1. There is no filesystem on bare metal, so there is nothing to LOAD from.
;;;      install-tarball-from-bytes takes the archive as an in-RAM byte vector,
;;;      which is exactly what net-fetch-bytes returns.
;;;   2. %tar-slice calls (make-array LEN) with a VARIABLE size, and eval2's
;;;      interpreter returns an array of length LEN/2 for that shape — which
;;;      truncates any tar entry over 512 bytes so its source will not READ.
;;;      The native build compiles the same make-array correctly, so baking
;;;      sidesteps the gap.  Same reasoning as mvm/build-cli-common.lisp.
;;;
;;; install-tarball.lisp names `chipz:decompress' / `chipz:gzip' on the gunzip
;;; branch.  This image has no CHIPZ package, so the build reader would error
;;; `Package CHIPZ does not exist' and SILENTLY DROP the whole enclosing form
;;; (taking %it-install-from-gz-bytes-1 with it).  Strip the qualifiers so they
;;; collapse to bare `decompress'/`gzip' in the flat image namespace; the branch
;;; is never taken because we serve a PLAIN .tar (install-tarball-from-bytes
;;; only calls DECOMPRESS when the gzip magic 1F 8B is present).  Read via
;;; read-file-text, NOT net-text/mvm-text — the host check-parses errors on
;;; `chipz:' before we get a chance to strip it.  Mirrors %cli-strip-chipz in
;;; mvm/build-cli-common.lisp and strip-package-prefixes in build-aarch64.lisp.
(defun %rpi-strip-one-prefix (text pfx)
  (let ((result text))
    (loop
      (let ((pos (search pfx result)))
        (unless pos (return result))
        (setf result (concatenate 'string
                                  (subseq result 0 pos)
                                  (subseq result (+ pos (length pfx)))))))))

(defun %rpi-strip-chipz (text)
  (%rpi-strip-one-prefix (%rpi-strip-one-prefix text "chipz::") "chipz:"))

(defvar *install-source*
  (if *net-build-p*
      (concatenate 'string
        (mvm-text "lib/tar.lisp") (string #\Newline)
        (%rpi-strip-chipz
         (read-file-text (merge-pathnames "lib/install-tarball.lisp" *modus-base*)))
        (string #\Newline))
      ""))

;; The rung-2 pipeline: bring the USB NIC up, DHCP for an address, fetch a
;; .tar over real HTTP and report its length + an FNV-1a-32 checksum so the
;; body can be verified byte-for-byte against the file the host served; then
;; (rung 3) INSTALL it, LOAD its sources and CALL a function from it.
(defvar *net-driver-source*
  (if *net-build-p* "
;; Build a byte-array URL from a Lisp string (chars are CHARACTERS in the real
;; CL reader — char-code them, unlike the legacy repl-source images where a
;; string slot already held the code).
(defun %net-url (s)
  (let* ((n (length s)) (arr (make-array n)))
    (dotimes (i n) (aset arr i (char-code (char s i))))
    (cons arr n)))

;; FNV-1a 32-bit over the first N bytes of ARR.  Every intermediate fits in a
;; 62-bit fixnum (2^32 * 2^24), so no bignum path is involved.
(defun %net-fnv1a (arr n)
  (let ((h 2166136261) (i 0))
    (loop
      (when (>= i n) (return h))
      (when (zerop (logand i 1023))
        (write-string-serial \"[f\") (print-dec i) (write-string-serial \"]\"))
      (setq h (logand (* (logxor h (aref arr i)) 16777619) #xFFFFFFFF))
      (setq i (+ i 1)))))

;; Fetch URL-STRING; return (body-array . body-length) or NIL.
(defun net-fetch-bytes (url-string)
  (let* ((u (%net-url url-string))
         (result (http-fetch-impl (car u) (cdr u))))
    (if (or (null result) (eq result 0))
        nil
        (let* ((resp (car result))
               (resp-len (cdr result))
               (body-off (http-find-body resp resp-len)))
          (let* ((blen (- resp-len body-off))
                 (out (make-array blen)))
            (let ((i 0))
              (loop
                (when (>= i blen) (return nil))
                (aset out i (aref resp (+ body-off i)))
                (setq i (+ i 1))))
            (cons out blen))))))

;; Fetch URL-STRING and REPORT it: body length, the first four bytes (a .tar.gz
;; must start 1F 8B 08) and an FNV-1a-32 of the whole body, which is what makes
;; the transfer verifiable byte-for-byte against the file the server holds.
;; A real defun (not driver-only text) so it lands in the symbol-function table
;; and can be called from the REPL as well as from the boot pipeline.
(defun net-fetch-report (url-string)
  (handler-case
      (let ((tb (net-fetch-bytes url-string)))
        (if (null tb)
            (progn (write-string-serial \"FETCH-FAIL\") (write-char-serial 10) 0)
            (progn
              (write-string-serial \"FETCHED bytes=\")
              (print-dec (cdr tb)) (write-char-serial 10)
              (write-string-serial \"HEAD4=\")
              (let ((k 0))
                (loop (when (>= k 4) (return nil))
                      (print-hex-byte (aref (car tb) k)) (write-char-serial 32)
                      (setq k (+ k 1))))
              (write-char-serial 10)
              (write-string-serial \"OUTLEN=\") (print-dec (length (car tb)))
              (write-char-serial 10)
              (write-string-serial \"FNV1A=\")
              (print-dec (%net-fnv1a (car tb) (cdr tb)))
              (write-char-serial 10)
              (cdr tb))))
    (t (c) (write-string-serial \"FETCH-ERR\") (write-char-serial 10) -1)))

;; Print the dotted quad held as a big-endian NUMBER in the u32 at ADDR.  The
;; DHCP router option (state+0x1C) is stored this way by dhcp-parse-offer, so
;; reading its four bytes in memory order prints them reversed.
(defun %net-print-ip-u32 (addr)
  (let ((v (mem-ref addr :u32)))
    (print-dec (logand (ash v -24) 255)) (write-char-serial 46)
    (print-dec (logand (ash v -16) 255)) (write-char-serial 46)
    (print-dec (logand (ash v -8) 255))  (write-char-serial 46)
    (print-dec (logand v 255))))

;; --- #263 rung 3 -------------------------------------------------------------
;; Fetch the archive ONCE, report it (rung-2 evidence: length / magic / FNV-1a),
;; then INSTALL it (untar -> .asd -> component order -> read+eval each file) and
;; CALL a function from the freshly loaded system.
;;
;; The call expression is a STRING read at runtime, never a literal in this
;; source: `(sha1:sha1-hex \"abc\")' cannot be READ at build time because the
;; SHA1 package does not exist until the library has loaded.  %lib-call-expr is
;; baked as a defun (MVM active limitation #7: a defvar initform never runs at
;; boot) and is overridable with MODUS_LIB_EXPR.
(defun net-install-and-call (url-string)
  ;; lib/tar.lisp's (defvar *tar-block-size* 512) init-thunk does NOT run at
  ;; boot, so the variable is NIL and (+ off *tar-block-size*) wedges
  ;; tar-do-entries.  Set it explicitly.  Same fix build-aarch64.lisp makes.
  (setq *tar-block-size* 512)
  (handler-case
      (let ((tb (net-fetch-bytes url-string)))
        (if (null tb)
            (progn (write-string-serial \"LIB-FETCH-FAIL\") (write-char-serial 10) nil)
            (progn
              (write-string-serial \"FETCHED bytes=\")
              (print-dec (cdr tb)) (write-char-serial 10)
              (write-string-serial \"HEAD4=\")
              (let ((k 0))
                (loop (when (>= k 4) (return nil))
                      (print-hex-byte (aref (car tb) k)) (write-char-serial 32)
                      (setq k (+ k 1))))
              (write-char-serial 10)
              (write-string-serial \"FNV1A=\")
              (print-dec (%net-fnv1a (car tb) (cdr tb))) (write-char-serial 10)
              ;; --- INSTALL: untar + parse .asd + read/eval every component ---
              (let ((sys (handler-case (install-tarball-from-bytes (car tb))
                           (t (c) nil))))
                (write-string-serial \"LIB-SYSTEM=\")
                (if (stringp sys) (write-string-serial sys)
                    (write-string-serial \"<install-failed>\"))
                (write-char-serial 10))
              ;; --- CALL: evaluate an expression from the installed system ---
              (write-string-serial \"LIB-EXPR=\")
              (write-string-serial (%lib-call-expr)) (write-char-serial 10)
              (write-string-serial \"LIB-VALUE=\")
              (handler-case
                  (let ((v (eval (read-from-string (%lib-call-expr)))))
                    (cond ((stringp v) (write-string-serial v))
                          ((integerp v) (print-dec v))
                          ((null v) (write-string-serial \"NIL\"))
                          (t (handler-case (write-object v)
                               (t (c) (write-string-serial \"<unprintable>\"))))))
                (t (c)
                  (write-string-serial \"<call-error> \")
                  (handler-case (write-object c)
                    (t (c2) (write-string-serial \"<err>\")))))
              (write-char-serial 10)
              t)))
    (t (c) (write-string-serial \"LIB-ERR\") (write-char-serial 10) nil)))

(defun run-net-pipeline ()
  (write-string-serial \"NET-PIPELINE-START\") (write-char-serial 10)
  ;; 1. DWC2 host controller + USB enumeration + CDC Ethernet.
  ;;    Prints DWC2:OK / PORT:xx / USB:vvvv:pppp / MAC:.. / CDC:OK itself.
  (let ((r (handler-case (cdc-ether-init) (t (c) 0))))
    (write-string-serial \"CDC-INIT=\") (print-dec r) (write-char-serial 10)
    (if (zerop r)
        (progn (write-string-serial \"NET-PIPELINE-ABORT\") (write-char-serial 10))
        (progn
          ;; 2. DHCP.  Prints DHCP:D / DHCP:O / DHCP:R / DHCP:A itself.
          (handler-case (dhcp-client) (t (c) nil))
          (let ((state (e1000-state-base)))
            (write-string-serial \"IP=\")
            (print-dec (mem-ref (+ state #x18) :u8)) (write-char-serial 46)
            (print-dec (mem-ref (+ state #x19) :u8)) (write-char-serial 46)
            (print-dec (mem-ref (+ state #x1A) :u8)) (write-char-serial 46)
            (print-dec (mem-ref (+ state #x1B) :u8)) (write-char-serial 10)
            (write-string-serial \"GW=\")
            (%net-print-ip-u32 (+ state #x1C)) (write-char-serial 10))
          ;; 3. HTTP GET of the library .tar from the QEMU slirp gateway, then
          ;;    (rung 3) install it, load it, and call a function from it.
          (net-install-and-call (%net-fetch-url)))))
  (write-string-serial \"NET-PIPELINE-DONE\") (write-char-serial 10))
"
      ""))

;; The URL is baked as a real defun (not a defvar — MVM active limitation #7
;; means a defvar initform never runs at boot).  MODUS_NET_URL overrides it.
(defvar *net-url-source*
  (if *net-build-p*
      (format nil "~%(defun %net-fetch-url () ~S)~%(defun %lib-call-expr () ~S)~%"
              (or #+sbcl (sb-ext:posix-getenv "MODUS_NET_URL")
                  "http://10.0.2.2:8080/sha1.tar")
              ;; #263 rung 3: the expression evaluated AFTER the fetched system
              ;; is installed + loaded.  A STRING, read at runtime — the build
              ;; reader cannot read `sha1:sha1-hex' (no SHA1 package until the
              ;; library loads).  Override with MODUS_LIB_EXPR.
              (or #+sbcl (sb-ext:posix-getenv "MODUS_LIB_EXPR")
                  "(sha1:sha1-hex \"abc\")"))
      ""))

;; Spliced into kernel-main.  "" when the flag is off => zero bytes added.
(defvar *net-pipeline-call*
  (if *net-build-p*
      "  (handler-case (run-net-pipeline) (t (c) nil))
"
      ""))

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
;; WS5 #203: the runtime backquote expander was extracted to
;; lib/runtime-backquote.lisp so the aarch64 hosted CLI can share it instead of
;; the text living only inside this wrapper.  It is spliced back INTO
;; *driver-source* here, at exactly the position it used to occupy, so every
;; existing driver scanner (defun names -> SFT, symbol names -> *SYM-NAME-TABLE*)
;; still sees these defuns unchanged.
(defvar *driver-source*
 (concatenate 'string "
;; BARE METAL: no process model.  WFI in a loop (TRAP #x0304 = WFI on
;; AArch64) wakes on any IRQ and immediately WFIs again — effectively idle.
;; The x64 sibling uses (loop (hlt)); this is the same shape on ARM.
(defun halt ()
  (loop (trap #x0304)))
(defun sys-exit (code)
  (let ((c code)) c)
  (halt))
"
;; The runtime backquote expander (shared with the hosted CLIs) — needed so a
;; macro TYPED AT THE REPL can use `(...) ,x ,@xs in its expansion.
(mvm-text "lib/runtime-backquote.lisp")
(string #\Newline)
;; The serial REPL itself: CL reader + EVAL(=mvm-eval) + CL printer over COM1.
(mvm-text "lib/serial-repl.lisp")
"
;; #160 bitmaps, bare-metal flavour — see the call in kernel-main for why.
;; gc.lisp's %gc-bitmap-init uses %mmap-exec-page, which does not exist here.
;;
;; Both bases are published with the SAME convention gc.lisp's readers expect:
;; (setf (mem-ref .. :u64)) stores value<<1 and %gc-bitmap-base et al read it
;; back through the matching halving, exactly as the hosted path does — do NOT
;; \"helpfully\" pre-shift these.  page_base is from_start (the lowest object
;; address), matching %gc-bitmap-init.
;;
;; NON-ALLOCATING by construction: raw mem-ref stores and fixnum arithmetic
;; only.  It runs before init-symbol-table, so there is no heap to allocate
;; from yet, and a bignum here would fault rather than collect.
(defun %rpi-gc-bitmap-init (obj-base cons-base nbytes)
  (let ((i 0))
    (loop
      (when (>= i nbytes) (return nil))
      (setf (mem-ref (+ obj-base i) :u64) 0)
      (setf (mem-ref (+ cons-base i) :u64) 0)
      (setq i (+ i 8))))
  (setf (mem-ref #x10000E00 :u64) (%gc-from-start))
  (setf (mem-ref #x10000E18 :u64) obj-base)
  (setf (mem-ref #x10000E40 :u64) cons-base)
  nil)

(defun kernel-main ()
  ;; Banner first: proves native code is executing and the UART is alive
  ;; before any runtime init runs.
  (write-string-serial \"MODUS-CL\")
  (write-char-serial 10)

  ;; BARE-METAL BSS-EQUIVALENT INIT.  On Linux the ELF BSS is zero-filled by
  ;; the kernel; on bare metal these words hold whatever the firmware left in
  ;; RAM, and symbol-value would dereference the garbage at 0x10000080 as the
  ;; global-alist head.  Unlike the x64 bare image — whose boot-x64.lisp
  ;; pre-zeroes 0x10000180 / 0x10000400 / 0x10000C70 — boot-rpi-cl.lisp zeroes
  ;; NOTHING, so this list is the union of the x64 CL REPL's and the
  ;; QEMU-virt bare AArch64 image's (build-aarch64.lisp kernel-main).
  ;; On the Pi these are plain identity-mapped DRAM (QEMU virt needs an MMU
  ;; remap to make 0x1000xxxx exist at all).
  (setf (mem-ref #x10000080 :u64) 0)   ; global variable alist head
  (setf (mem-ref #x10000088 :u64) 0)   ; symbol intern table
  (setf (mem-ref #x10000090 :u64) 0)   ; MV count
  (setf (mem-ref #x10000098 :u64) 0)   ; MV values
  (setf (mem-ref #x10000148 :u64) 0)   ; keyword intern table
  (setf (mem-ref #x10000150 :u64) 0)   ; dynamic nargs
  (setf (mem-ref #x10000158 :u64) 0)   ; intern counter
  (setf (mem-ref #x10000170 :u64) 0)   ; package-by-hash root
  ;; NOT 0x10000160/168 — those are code_base/code_end, written by
  ;; emit-aarch64-code-bounds-init in the boot preamble.  FUNCTIONP's
  ;; in-code-range arm reads them; zeroing here would undo the boot.
  (setf (mem-ref #x10000180 :u64) 0)   ; handler-case setjmp frame
  (setf (mem-ref #x10000188 :u64) 0)
  (setf (mem-ref #x100001C0 :u64) 0)   ; no-handler rescue fallback
  (setf (mem-ref #x10000C30 :u64) 0)   ; fault diag slots
  (setf (mem-ref #x10000C38 :u64) 0)
  (setf (mem-ref #x10000C40 :u64) 0)
  (setf (mem-ref #x10000C48 :u64) 0)
  (setf (mem-ref #x10000C50 :u64) 0)
  (setf (mem-ref #x10000C58 :u64) 0)
  (setf (mem-ref #x10000C70 :u64) 0)   ; deadline countdown (no IRQ here)
  (setf (mem-ref #x10000C80 :u64) 0)   ; %intern-symbol depth counter
  (setf (mem-ref #x10000DA0 :u64) 0)   ; safepoint boundary
  (setf (mem-ref #x10010000 :u64) 0)   ; handler-stack depth (aarch64 helpers)

  ;; GC METADATA — must precede the first allocation.  The x64 bare image gets
  ;; this from boot-x64.lisp's kernel64 entry; the AArch64 boot publishes only
  ;; x24/x25, so kernel-main has to publish the semispace metadata itself
  ;; (same call the QEMU-virt bare image makes).  Heap is 112 MB split into two
  ;; 56-MB semispaces: from-start 0x09000000, to-start 0x0C800000.
  ;; boot-rpi-cl.lisp sets x25 = 0x0C800000 so the first overflow trips the GC
  ;; trampoline rather than running off the end of the from-space.  The third
  ;; argument is the conservative stack scan base — keep it equal to the boot
  ;; SP (+rpi-cl-stack-top+).
  (%gc-init #x09000000 #x07000000 #x08000000)

  ;; #160 OBJECT-START + CONS-KIND BITMAPS, bare-metal flavour.
  ;;
  ;; %gc-bitmap-init (gc.lisp) reserves these with %mmap-exec-page, which does
  ;; not exist here, so bare metal had NO bitmap: %gc-is-start degraded to T and
  ;; every conservative candidate was copied.  That was survivable only while
  ;; the collector forwarded almost nothing (the %gc-read64 word/2 bug, fixed in
  ;; b65730c).  With forwarding actually working, false roots get copied using
  ;; DATA as a header — measured twice on this image: a copy loop walking off
  ;; DRAM (ESR #x97000010), and a false root near the top of the upper semispace
  ;; copying past 0x10000000 onto the GC metadata at 0x10000040, i.e. the
  ;; collector overwriting its own saved_rsp (tell: phase trace 12345 -> 1S2345).
  ;;
  ;; Fixed RAM instead of mmap.  1 bit / 16-byte granule over the 112 MB heap =
  ;; 0x07000000/128 = 0xE0000 bytes (896 KB) per bitmap.  Placed at 80 MB
  ;; (0x05000000 / 0x05100000), which clears the image — note the image is
  ;; ~54 MB with the bit-set emitted inline at every alloc site, NOT the ~21 MB
  ;; of a bitmap-off build — and sits ~46 MB below the stack top; both bounds
  ;; are asserted at build time.  MUST run before the first
  ;; allocation (init-symbol-table, just below) so every mutator alloc records
  ;; its start bit, and after %gc-init because page_base is read from from_start.
  ;;
  ;; DRAM is not guaranteed zero at reset, so the regions are cleared explicitly
  ;; — a stale bit would validate a false root, which is the whole failure this
  ;; exists to prevent.  Non-allocating: raw mem-ref stores only.
  (%rpi-gc-bitmap-init #x05000000 #x05100000 #xE0000)

  ;; NOTE: no (setup-irq) / (nic-irq-unmask) here.  Those program a GICv2,
  ;; which a BCM2837 does not have (it uses the BCM interrupt controller), and
  ;; nothing in this image needs interrupts — the REPL polls the UART.

  ;; --- runtime bring-up (same order as the hosted CLI) --------------------
  (init-symbol-table)
  (init-keyword-table)
  (%init-packages)
  (%init-streams)
  ;; BARE METAL HAS NO FILE DESCRIPTORS.  %init-streams (cl-streams.lisp,
  ;; WS5 #203 gap 1) ends with (setq *error-output* (%make-file-stream-full 2 1))
  ;; — a type-9 Linux fd-2 stream.  Writing one char to it runs
  ;;   %fs-write-char -> %sys-write-raw -> (syscall3 1 …) -> :trap #x0502
  ;; which the x64 translator emits as a literal SYSCALL instruction.  Long mode
  ;; is entered here with IA32_EFER.LME|NXE but NOT SCE, so that SYSCALL raises
  ;; #UD, boot-x64.lisp's IDT entry 6 longjmps out, and the write surfaces as
  ;; #(PROGRAM-ERROR NIL).
  ;;
  ;; That made reading ANY global by name fail at COMPILE time (task #212).
  ;; The in-image *GLOBALS* registry starts empty in every compilation unit, so
  ;; EVERY free-variable reference — user (*n*) or built-in (*print-base*) —
  ;; lands in compile-variable-ref's final `t' arm (compiler.lisp ~4302), whose
  ;; FIRST act is (format *error-output* \"~&  WARN: implicit global ~A~%\" name).
  ;; The format blew up before the compiler ever emitted the read, so `*n*'
  ;; failed, `(if nil *n* 7)' failed (the ref is never evaluated), and no
  ;; handler-case could catch it — HANDLER-CASE's own expansion references
  ;; *CATCH-ACTIVE* / *CURRENT-CONDITION* and so failed to compile too.
  ;;
  ;; There is exactly one console here, so stderr is the serial port.  This is
  ;; what *error-output* was before WS5 #203 split it onto fd 2 for the hosted
  ;; differential table.  LATENT ELSEWHERE: build-x64.lisp / build-aarch64.lisp
  ;; (the bare-metal ANSI gate images) call %init-streams too and have the same
  ;; unwritable stderr; not touched here to keep the gate images byte-identical.
  (setq *error-output* *standard-output*)
  (%init-reader)
  (%init-condition-types)
  (%init-method-combinations)
  (%init-symbol-function-table)
  (%init-sft-auto)
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)
  (setq *macro-table* (make-hash-table))
  (%init-runtime-macros)
  (init-compiler-macro-set)
  (%init-signal-symbols)
  (%init-make-load-form)
  (%init-clos-protocol)
  (%install-runtime-backquote)
  ;; BARE METAL: no Linux sigaction — hardware-fault recovery is boot-x64.lisp's
  ;; IDT entries 13/14 (#GP/#PF -> handler-case longjmp).  %init-signal-handling
  ;; (rt_sigaction) is deliberately NOT called.

  ;; RUN THE DEFVAR INIT THUNKS.  Not optional for a REPL, and NOT the same
  ;; call the bare ANSI gate skips.  The in-image COMPILER keeps its
  ;; compile-time tables in defvars — *CONSTANTS*, *GLOBALS*,
  ;; *GLOBAL-SYMBOL-MACROS* — and compile-variable-ref does an UNGUARDED
  ;;   (gethash (normalize-name name) *constants* :not-found)
  ;; on every free-variable reference.  With the init thunks unrun those
  ;; tables are NIL (limitation #7) and the gethash signals PROGRAM-ERROR, so
  ;; every reference to a global typed at the REPL failed —
  ;;   (defparameter *n* 41)  =>  *N*        ; defparameter is a runtime call
  ;;   (symbol-value '*n*)    =>  41         ; the binding really is there
  ;;   *n*                    =>  PROGRAM-ERROR
  ;; — while (let ((x 5)) …) worked, because a lexical never reaches that
  ;; branch.  Measured on this image before the fix; the hosted CLI never saw
  ;; it because it has always called this.  Each thunk is wrapped in
  ;; handler-case at compile time, so one that references a not-yet-bound
  ;; symbol cannot break the chain.
  (handler-case (init-all-globals) (t (c) nil))

  ;; --- globals whose defvar/defconstant init thunks do not run at boot ----
  ;; (MVM active limitation #7.)
  (setq *cstr-scratch* #x0FE00000)
  (setq *io-buf-addr*  #x0FF00000)
  (setq *scratch-mmapped* nil)
  (setq *filesystem* nil)
  (setq *default-pathname-defaults* \"/\")
  (setq *gensym-counter* 0)
  (setq *gentemp-counter* 0)
  (setq *write-object-budget* 1000000)
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)
  (setq *rt-registered-tests* nil)
  (setq array-total-size-limit  (ash 1 24))
  (setq array-dimension-limit   (ash 1 24))
  (setq array-rank-limit        256)
  (setq call-arguments-limit    256)
  (setq lambda-parameters-limit 256)
  (setq lambda-list-keywords    '(&allow-other-keys &aux &body &environment &key
                                   &optional &rest &whole))
  (setq multiple-values-limit   16)
  (setq char-code-limit         #x110000)   ; full Unicode codespace (21-bit char immediate)
  (setq internal-time-units-per-second 1000000)
  (setq most-positive-fixnum  4611686018427387903)
  (setq most-negative-fixnum -4611686018427387904)
  (setq pi 3.141592653589793d0)
  (%init-standard-chars)
  (%init-boole-constants)
  ;; Runtime CL macros (when/unless/setf/incf/case/dolist/…) for forms typed at
  ;; the REPL.  Must follow init-all-globals: it walks *modus-runtime-macros*,
  ;; whose value is a defvar init thunk.
  (handler-case (%install-runtime-cl-macros) (t (c) nil))
  (setq *serial-repl-buf* nil)
  (setq *serial-repl-len* 0)
  (setq *serial-repl-cap* 0)

  ;; --- in-image self-check ------------------------------------------------
  ;; Same E2SMOKE probes the bare ANSI gate runs: prove compile->bytecode->
  ;; interpret works (including a defun in one form called from a later one)
  ;; BEFORE handing the machine to the user.  add=3 sqr=25 defcall=49
  ;; persist-call=36 persist-fn=45.
  (write-string-serial \"E2SMOKE-START\") (write-char-serial 10)
  (write-string-serial \"add=\")
  (print-dec (handler-case (mvm-eval (quote (+ 1 2))) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"sqr=\")
  (print-dec (handler-case (mvm-eval (quote (let ((x 5)) (* x x)))) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"defcall=\")
  (print-dec (handler-case
                 (mvm-eval-forms (list (quote (defun sq (x) (* x x))) (quote (sq 7))))
               (t (c) -1)))
  (write-char-serial 10)
  (handler-case (mvm-eval (quote (defun pf (x) (* x 9)))) (t (c) nil))
  (write-string-serial \"persist-call=\")
  (print-dec (handler-case (mvm-eval (quote (pf 4))) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"persist-fn=\")
  (print-dec (handler-case (funcall (quote pf) 5) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"E2SMOKE-END\") (write-char-serial 10)
"
;; --- #209 rung 2: DWC2/USB -> DHCP -> TCP -> HTTP GET -------------------
;; "" unless MODUS_NET_BUILD=1, so the rung-1 image is byte-identical.
*net-pipeline-call*
"
  ;; --- the REPL (lib/serial-repl.lisp) ------------------------------------
  (handler-case (cl-serial-repl) (t (c) nil))
  (halt))
"))

;;; WHY kernel-main ends in (halt) AND NOT (sys-exit 0)   [2026-08-19]
;;;
;;; SYS-EXIT is a COMPILER SPECIAL FORM, not a function call: compiler.lisp's
;;; dispatch sends it to compile-sys-exit, filed under "--- Linux Syscalls ---",
;;; which unconditionally emits TRAP #x0500 -> a literal SVC.  Bare metal has no
;;; OS to service that, so it raises a synchronous exception.  Measured:
;;;   !!FAULT E0000000056000500 L000000000325cd4c      (ESR EC=0x15 = SVC)
;;; and the emitted sequence  mov x0,x26 / mov x0,#0 / svc #0x500  was read back
;;; out of RAM through the QEMU gdbstub and found byte-identical in the image
;;; FILE (file offset = vaddr - #x80000) — i.e. BAKED IN, not corruption.
;;;
;;; The (defun sys-exit (code) ... (halt)) in *driver-source* does NOT protect
;;; us: the compiler intrinsifies the NAME, so that defun is dead code.  A build
;;; CANNOT override a compiler special form by defining a function of the same
;;; name.  See task #269 — the other three bare-metal builds have the same live
;;; bug, and build-aarch64/build-x64 are ANSI gate runners where sys-exit is how
;;; the run TERMINATES, so do NOT blanket-replace them.
;;;
;;; It stayed hidden because it is only reachable when cl-serial-repl RETURNS;
;;; the healthy path loops in the REPL forever.
;;;
;;; KEEP THIS COMMENT OUTSIDE THE SOURCE STRING.  Text inside the string above
;;; is COMPILED INTO THE IMAGE; adding a comment there changed the emitted image
;;; from offset #x89 onward (GENERIC-MULTIPLY's code included), which destroys
;;; any ability to attribute a behaviour change to the one-form edit.

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
                       ;; WS5 rung 2: register the JIT translator's defuns +
                       ;; quoted symbols in the SFT / sym-name tables so
                       ;; %init-x64-translator / translate-mvm-to-x64 are
                       ;; runtime-reachable and their quoted reg symbols
                       ;; (rax, :e, …) have recoverable names.  "" when JIT off.
                       (or *x64-asm-source* "") (string #\Newline)
                       (or *translate-x64-source* "") (string #\Newline)
                       (or *jit-coinit-source* "") (string #\Newline)
                       *jit-boot-source* (string #\Newline)
                       *stage2-test-source* (string #\Newline)
                       *rt-macros-source* (string #\Newline)
                       *bridge-source*   (string #\Newline)
                       ;; #209 rung 2: scan the net stack's defuns + quoted
                       ;; symbols into the SFT / sym-name tables too.  "" when
                       ;; MODUS_NET_BUILD is off.
                       *net-source*      (string #\Newline)
                       ;; #263 rung 3: tar + install-tarball, so their defuns
                       ;; (install-tarball-from-bytes, tar-extract, …) land in
                       ;; the SFT and are reachable from runtime EVAL / the REPL.
                       *install-source*  (string #\Newline)
                       *net-url-source*  (string #\Newline)
                       *net-driver-source* (string #\Newline)
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
    ;; WS5 rung 2: OPTIONAL native JIT translator (only under MODUS_USE_JIT=1;
    ;; each var is "" when JIT is off → the default image is byte-identical).
    (or *x64-asm-source* "")
    (string #\Newline)
    (or *translate-x64-source* "")
    (string #\Newline)
    (or *jit-coinit-source* "")
    (string #\Newline)
    *stage2-test-source*
    (string #\Newline)
    ;; #209 rung 2: the Pi USB net stack.  Placed AFTER the CL runtime, the
    ;; compiler and mvm-eval so its own definitions win under last-defun-wins
    ;; (the arch adapter deliberately overrides WRITE-BYTE), and BEFORE
    ;; *driver-source*, whose kernel-main calls run-net-pipeline.
    ;; All three are "" unless MODUS_NET_BUILD=1.
    *net-source*
    ;; #263 rung 3: tar reader + tarball installer, AFTER the net stack (they
    ;; report progress through its write-string-serial / print-dec) and after
    ;; the CL runtime + mvm-eval whose EVAL loads each component file.
    *install-source*
    *net-url-source*
    *net-driver-source*
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
    *driver-source*
    (string #\Newline)
    ;; WS5 rung 2: baked JIT boot hook + gate (LAST so %jit-enabled-p wins).
    *jit-boot-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; MODUS_DUMP_FULL_SOURCE=<path> — write the assembled blob and STOP, without
;;; building an image.  Mirrors the gate in mvm/build-cli-common.lisp verbatim,
;;; and exists for the same reason: the blob is the ONLY thing this wrapper
;;; contributes to the emitted binary, so a refactor that leaves it
;;; byte-identical is behaviour-preserving BY CONSTRUCTION, and proving that
;;; takes ~20s instead of a ~20min image build + ~4min QEMU boot.
;;;
;;; This is the gate for #266 (migrating this file to a thin tail over
;;; build-cli-common).  Capture the blob BEFORE restructuring, diff after; the
;;; only differences should be the ones intended (e.g. the aarch64 JIT block
;;; replacing the x64 one).
;;;
;;; WHY IT MATTERS HERE SPECIFICALLY: image byte-identity is NOT an achievable
;;; bar for this build — measured 2026-08-20, a 1973-char source addition
;;; reflowed 17.5% of the hosted aarch64 image and 7.5% of this one, because
;;; emission is sensitive to source length/content (gensym numbering, intern
;;; order, offsets).  The SOURCE BLOB is the level at which identity is both
;;; achievable and meaningful.  Do not try to A/B the images.
;;;
;;; Placed AFTER assembly and BEFORE build-image so it cannot perturb what it
;;; measures.
#+sbcl
(let ((p (sb-ext:posix-getenv "MODUS_DUMP_FULL_SOURCE")))
  (when (and p (plusp (length p)))
    (with-open-file (o p :direction :output :if-exists :supersede
                         :external-format :utf-8)
      (write-string *full-source* o))
    (format t "Dumped *full-source* to ~A (~D chars); skipping image build.~%"
            p (length *full-source*))
    (sb-ext:exit :code 0)))

;;; ============================================================
;;; 6. Build the image
;;; ============================================================

;; Bare-metal Raspberry Pi 3B boot descriptors.  boot-rpi.lisp arrives via
;; lib/load-mvm.lisp already; boot-rpi-cl.lisp is loaded AFTER it and redefines
;; `rpi-boot-descriptor' to the CL-lineage one (kernel8.img @0x80000, MMU off,
;; identity addressing, PL011 console, Cheney heap registers).  The
;; redefinition is process-local, so `build-rpi-ssh' / `-hid' / `-periph' are
;; untouched.
(mvm-load "boot/boot-rpi-cl.lisp")

(in-package :modus.mvm)

;; Install the AArch64 translator in BARE-METAL mode (*aarch64-linux-mode* is
;; NIL by default), so TRAP #x0300/#x0301 emit PL011 MMIO rather than Linux
;; syscalls — which is exactly what write-char-serial / read-char-serial need.
(install-aarch64-translator)

;; CONSOLE SELECTION.  Two different boards, two different UARTs:
;;
;;   QEMU raspi3b (SD-card / -kernel path)  -> PL011 UART0 at 0x3F201000,
;;     which is what QEMU wires to serial0.
;;   REAL Pi Zero 2 W (UART chain-load)     -> BCM2835 mini UART at 0x3F215040.
;;     On a Pi Zero 2 W the PL011 is routed to Bluetooth and the mini UART owns
;;     GPIO14/15, so an image that writes PL011 transmits into the Bluetooth
;;     modem and the wires stay SILENT.
;;
;; That difference is invisible under emulation and cost a full 88-minute
;; chain-load cycle to notice: the loader's own BOOT/RDY/AD:/SZ: arrive over
;; the mini UART (proving the pins), yet the image we hand control to was
;; built for PL011, so a PERFECTLY SUCCESSFUL chain-load would look exactly
;; like a dead transfer.
;;
;; The old comment here said the mini UART "could print but never read"
;; because `read-char-serial' hardcoded PL011's UARTFR+0x18/RXFE-bit-4.  That
;; is FIXED (1e84418): the RX ready-poll is now parameterized via
;; *aarch64-serial-rx-poll*, exactly mirroring the TX side.
(defvar *rpi-cl-miniuart*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_MINIUART")))
    (if (and v (plusp (length v)))
        (not (string= v "0"))
        ;; Default: follow the chain-load flag, because chain-loading is the
        ;; real-hardware path and SD/QEMU is the PL011 path.
        *rpi-cl-chainload*))
  "T => target the BCM2835 mini UART (real Pi Zero 2 W).  NIL => PL011 (QEMU).")

(if *rpi-cl-miniuart*
    (progn
      ;; AUX mini UART: AUX_MU_IO at base+0, AUX_MU_LSR at base+0x14.
      ;; TX: wait while LSR bit 5 (transmitter empty) is CLEAR.
      ;; RX: wait while LSR bit 0 (data ready)        is CLEAR.
      ;; AUX registers require 32-bit access, hence width 2.
      (setf *aarch64-serial-base* #x3F215040)
      (setf *aarch64-serial-width* 2)
      (setf *aarch64-serial-tx-poll* '(#x14 5 :tbz))
      (setf *aarch64-serial-rx-poll* '(#x14 0 :tbz))
      (format t "~&;; CONSOLE: BCM2835 mini UART 0x3F215040 (real Pi Zero 2 W)~%"))
    (progn
      (setf *aarch64-serial-base* #x3F201000)
      (setf *aarch64-serial-width* 0)
      (setf *aarch64-serial-tx-poll* nil)
      (setf *aarch64-serial-rx-poll* '(#x18 4 :tbnz))
      (format t "~&;; CONSOLE: PL011 UART0 0x3F201000 (QEMU raspi3b)~%")))

;; No GICv2 on a BCM2837, and nothing here needs interrupts (the REPL polls
;; the UART), so leave *aarch64-setup-irq-enable* NIL — the QEMU-virt bare
;; image only turns it on for its per-test vtimer deadline IRQ.
;; No actor scheduler either, so no sched lock: the translator then emits no
;; load/store-exclusive, which matters because this image runs MMU-off and
;; exclusives are UNPREDICTABLE on Device memory.
;; #160: emit the object-start + cons-kind bit-set at every alloc site, so
;; %gc-is-start can reject a conservative candidate that lands mid-object
;; instead of degrading to T and copying it.  Matches build-aarch64-cli.lisp
;; and build-aarch64-linux.lisp; the RPi image was the last aarch64 target
;; still running the collector with NO bitmap.  The backing RAM is reserved and
;; zeroed by %rpi-gc-bitmap-init in kernel-main.
(setf *aarch64-gc-bitmap-enabled* t)

;; #267 step 1: use the NATIVE aarch64 Cheney collector — the same one
;; build-aarch64-cli.lisp runs — instead of falling through to gc.lisp's
;; INTERPRETED %gc-collect.  This image was the LAST target in the tree still on
;; the Lisp collector (x64 has its native trampoline, hosted aarch64 sets this
;; flag, i386 has its own arm), and it is also the only target that fails to
;; load alexandria.
;;
;; The BL flag is REQUIRED here, not cosmetic.  Native MCGC normally calls the
;; trampoline as `BLR x28`, and x28 is materialised by emit-linux-aarch64-entry
;; — the LINUX entry.  boot-rpi-cl.lisp never touches x28, so the mcgc flag
;; ALONE made every gc-check an indirect call through a garbage register:
;; measured 2026-08-20, the image branched into space on its first collection and
;; re-entered at the image start (boot banner printed twice) with ZERO logged
;; exceptions, since control never reached the EL2 vectors.  BL's ±128MB reach
;; covers this ~57MB image with room to spare.
(setf *aarch64-gc-native-mcgc* t)
(setf *aarch64-gc-trampoline-call-via-bl* t)

(setf *aarch64-sched-lock-addr* nil)

;; SP alignment stays 8-byte (bare-metal EL1 with SCTLR.SA off), unlike Linux
;; EL0 which demands 16.  *aarch64-fn-align-offset* stays 0: the unified
;; buffer's alignment loop measures absolute position INCLUDING the boot
;; preamble, and the image base 0x80000 is 16-byte aligned, so fn entries land
;; on 16-byte VAs and the OR-3 fn tagging yields clean nibble-3 tags.

;; Bare-metal handler-stack helpers: the label vars stay NIL at toplevel —
;; cross.lisp's unified aarch64 emit binds fresh labels around the boot-entry
;; and translate calls (assemble-kernel-image).
(setf *aarch64-handler-pop-label* nil)
(setf *aarch64-handler-push-label* nil)
(setf *aarch64-gc-trampoline-label* nil)

#+sbcl
(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf modus.mvm::*write-symmap-path* sm)))

(format t "~%Compiling bare-metal RPi CL REPL image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :rpi
                          :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_CL_REPL_OUT")
                  "/tmp/piboot/kernel8.img")))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (let ((n (length modus.mvm::*redefinition-log*)))
      (when (> n 0)
        (format t "~%REDEFINITIONS: ~D total (grep the build log for \"NOTE: redefining\")~%" n)))

    ;; ------------------------------------------------------------------
    ;; BUILD-TIME MEMORY-MAP ASSERTS (Pi 3B geometry).
    ;;
    ;; The Pi map is TIGHTER than QEMU virt's in one direction and roomier in
    ;; another: DRAM starts at 0 (so the runtime VAs are plain RAM, no remap),
    ;; but the BCM2837 peripheral window at 0x3F000000 caps everything, and the
    ;; image loads at 0x80000 rather than being handed a whole 2 MB-aligned
    ;; region.  Assert the three ways a too-large image silently corrupts:
    ;;   1. image end reaching the stack region  -> pushes shred native code
    ;;      (this is precisely what boot-rpi.lisp's 0x00200000 stack top does
    ;;      to any CL-sized image, task #47's failure class)
    ;;   2. image end reaching the heap base     -> allocation over code
    ;;   3. heap end reaching the peripherals    -> DMA/MMIO aliasing
    ;; ------------------------------------------------------------------
    (let* ((image-bytes (length (kernel-image-image-bytes image)))
           (image-va-start #x80000)
           (image-va-end   (+ image-va-start image-bytes))
           (stack-top      +rpi-cl-stack-top+)
           (stack-headroom (* 8 1024 1024))
           (stack-va-lo    (- stack-top stack-headroom))
           (heap-base      +rpi-cl-heap-base+)
           (heap-end       +rpi-cl-heap-end+)
           (periph-base    #x3F000000))
      (when (>= image-va-end stack-va-lo)
        (error "BUILD-TIME ASSERT: image [~X..~X] (~,1F MB) reaches the stack ~
                region [~X..~X].  Stack pushes would overwrite native code."
               image-va-start image-va-end (/ image-bytes 1024.0 1024.0)
               stack-va-lo stack-top))
      (when (>= image-va-end heap-base)
        (error "BUILD-TIME ASSERT: image end ~X reached the heap base ~X."
               image-va-end heap-base))
      ;; #160 bitmaps live at 0x05000000 / 0x05100000, 0xE0000 bytes each
      ;; (1 bit / 16-byte granule over the 112 MB heap).  They must clear the
      ;; image below and the stack above; both are checked here rather than
      ;; discovered as heap corruption at runtime.  Keep in step with the
      ;; %rpi-gc-bitmap-init call in kernel-main.
      (let* ((objmap-lo  #x05000000)
             (consmap-lo #x05100000)
             (map-bytes  #xE0000)
             (maps-hi    (+ consmap-lo map-bytes)))
        (when (< objmap-lo image-va-end)
          (error "BUILD-TIME ASSERT: #160 object-start bitmap at ~X is inside ~
                  the image (ends ~X)." objmap-lo image-va-end))
        (when (> (+ objmap-lo map-bytes) consmap-lo)
          (error "BUILD-TIME ASSERT: #160 object-start bitmap ~X+~X overlaps ~
                  the cons-kind bitmap at ~X." objmap-lo map-bytes consmap-lo))
        (when (>= maps-hi stack-va-lo)
          (error "BUILD-TIME ASSERT: #160 bitmaps end ~X reach the stack low ~
                  water ~X." maps-hi stack-va-lo))
        (format t "  gc bitmaps ~8,'0X .. ~8,'0X  (#160 object-start + cons-kind)~%"
                objmap-lo maps-hi))
      (when (> heap-end periph-base)
        (error "BUILD-TIME ASSERT: heap end ~X is inside the BCM2837 ~
                peripheral window at ~X." heap-end periph-base))
      (format t "~%Pi 3B memory map (all identity-mapped DRAM, MMU off):~%")
      (format t "  image      ~8,'0X .. ~8,'0X  (~,2F MB)~%"
              image-va-start image-va-end (/ image-bytes 1024.0 1024.0))
      (format t "  stack top  ~8,'0X            (grows down, ~,1F MB clear of image)~%"
              stack-top (/ (- stack-va-lo image-va-end) 1024.0 1024.0))
      (format t "  heap       ~8,'0X .. ~8,'0X  (112 MB, midpoint ~8,'0X)~%"
              heap-base heap-end +rpi-cl-heap-mid+)
      (format t "  periph     ~8,'0X ..            (PL011 UART0 at 3F201000)~%"
              periph-base)
      ;; #209 rung 2: the USB/net DMA + state block (net/arch-rpi-cl.lisp).
      ;; It must clear BOTH the runtime metadata window (which ends at
      ;; 0x10200000) and the peripherals, or the NIC DMAs over live data —
      ;; the exact failure build-aarch64.lisp's net relocation exists to stop.
      (when cl-user::*net-build-p*
        (let ((net-lo #x11000000)
              (net-hi #x11113000)
              (meta-end #x10200000)
              ;; SMALLEST board this image must run on: Pi Zero 2 W, 512 MB,
              ;; minus gpu_mem=16 => the ARM sees 0x1F000000 (496 MB).  QEMU
              ;; raspi3b models a 1 GiB Pi 3B, so anything between 496 MB and
              ;; 1 GiB passes every emulated test and addresses a HOLE on the
              ;; real target.  The net block used to sit at 0x20000000 =
              ;; exactly 512 MB and was unusable on hardware for that reason.
              (min-board-ram #x1F000000))
          (when (< net-lo meta-end)
            (error "BUILD-TIME ASSERT: net DMA base ~X is inside the runtime ~
                    metadata window (ends ~X)." net-lo meta-end))
          (when (>= net-hi periph-base)
            (error "BUILD-TIME ASSERT: net region end ~X is inside the ~
                    BCM2837 peripheral window at ~X." net-hi periph-base))
          (when (>= net-hi min-board-ram)
            (error "BUILD-TIME ASSERT: net region end ~X is at/above ~X, the ~
                    RAM the ARM sees on a Pi Zero 2 W (512 MB - gpu_mem=16). ~
                    It would DMA into a hole on the real target while passing ~
                    every QEMU raspi3b test, because raspi3b models 1 GiB."
                   net-hi min-board-ram))
          (format t "  net/DMA    ~8,'0X .. ~8,'0X  (USB + E1000-shaped state)~%"
                  net-lo net-hi))))

    (format t "~%Run: qemu-system-aarch64 -M raspi3b -kernel ~A -serial stdio -serial null -display none -no-reboot~A~%"
            path
            (if cl-user::*net-build-p*
                " -device usb-net,netdev=net0 -netdev user,id=net0"
                ""))))
