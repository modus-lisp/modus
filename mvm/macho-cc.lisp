;;;; mvm/macho-cc.lisp -- a small compiler from an untagged, C-shaped Lisp to AArch64,
;;;; for the native code of the macOS executable (mvm/build-macos-cli.lisp).
;;;;
;;;; WHY NOT THE MVM COMPILER.  The macOS host's job (host/macos/shim.lisp; it was
;;;; host/macos/modus-shim.c) is C-ABI work: it is entered by dyld as main(), by the kernel
;;;; as a signal handler, by libpthread as a thread's start routine, and by the image through
;;;; its syscall slot; it calls libSystem with AAPCS64 arguments (variadic ones on the stack,
;;;; Apple's way), and it moves raw 64-bit words between Linux and Darwin structures.  The MVM
;;;; compiler produces tagged-value code whose every non-fixnum path calls a runtime helper
;;;; (mvm/ape-stub-win64.lisp's header lists the traps), it has no C-call primitive on
;;;; aarch64, and nothing in it can be called by C.  Rather than build adapters for each of
;;;; those, this file compiles a language whose values ARE machine words to code that IS
;;;; AAPCS64.  It also needs none of the translator's emission flags, so a modus builder
;;;; (mvm/builder.lisp) produces the same bytes as SBCL with nothing to pin.
;;;;
;;;; THE LANGUAGE.  Every value is a 64-bit word.  Toplevel:
;;;;   (defconst NAME VALUE)                 compile-time constant (VALUE is evaluated by the host)
;;;;   (defimport NAME "_csym" [RESULT])     a libSystem symbol; RESULT :i64 (default) :i32 :u32 :data
;;;;   (defdata NAME BYTES [ALIGN])          zero-filled writable bytes in __DATA
;;;;   (defstring NAME "text")               a NUL-terminated string in __TEXT,__cstring
;;;;   (defmacro NAME LAMBDA-LIST BODY...)   a host-Lisp macro over shim forms
;;;;   (defun NAME (PARAMS...) BODY...)      an AAPCS64 function, at most 8 parameters
;;;;   (defasm NAME INSTRUCTION...)          hand-written instructions (see ASM-INSN)
;;;;   (defextern NAME)                      a label the writer places (MODUS-IMAGE: the image)
;;;; Forms: integers; constants; variables; (let ((V E) ...) ...) -- SEQUENTIAL, like LET*;
;;;; (with-buffers ((V BYTES) ...) ...) -- V is the address of BYTES of frame storage, 16-aligned;
;;;; setq progn if when unless cond and or not; (while C BODY...) (break) (return [E]);
;;;; + - * (sdiv A B) (udiv A B) (urem A B) logand logior logxor lognot (shl A N) (shr A N)
;;;; (sar A N); = /= < > <= >= (signed) u< u> u<= u>=; (sext32 E) (zext32 E);
;;;; (ld8 A) (ld16 A) (ld32 A) (lds32 A) (ld64 A) -- each takes an optional constant offset --
;;;; (st8 A V [OFF]) ... (st64 A V [OFF]); (atomic-ld32 A) (atomic-st32 A V);
;;;; (addr NAME) the address of a function, data, string or asm label;
;;;; (import-value NAME) the address dyld bound for an import (a data import's address);
;;;; (NAME ARGS...) calls a defun; (ccall NAME ARGS...) calls an import;
;;;; (ccallv NAME NFIXED ARGS...) calls a variadic import (arguments past NFIXED go on the
;;;; stack, 8 bytes each: Apple's arm64 ABI); (callptr F ARGS...) calls a computed address.
;;;; More than 8 arguments to a non-variadic call go on the stack in 8-byte slots, which is
;;;; Apple's layout whenever each is 8-byte aligned (all this program passes).
;;;;
;;;; CODE SHAPE.  The accumulator is x0; a binary operation pushes its left operand (16
;;;; bytes, so SP stays aligned), evaluates the right into x0, moves it to x1 and pops x0.
;;;; Locals live in the frame below x29.  The compiled code uses x0-x10, x16, x29, x30 and
;;;; SP only: it never touches x18 (Apple's) nor x19-x28, so it preserves the callee-saved
;;;; registers by never using them -- which is what lets the syscall stub keep the image's
;;;; x19-x28 intact across a translated call.
;;;;
;;;; OUTPUT.  COMPILE-SHIM returns a SHIM-OBJECT: the text as 32-bit words, the strings,
;;;; the data layout, the imports in declaration order and the relocations, which
;;;; mvm/macho-writer.lisp resolves once it has placed the sections.

(defpackage :modus.macho
  (:use :common-lisp)
  (:export #:compile-shim #:read-shim-file #:shim-object #:make-shim-object))
(in-package :modus.macho)

;;; ---------------------------------------------------------------- the encoder ----

(defun reg (r)
  "A register designator: 0..30, :sp / :xzr (31), or a symbol X0..X30 / W0..W30."
  (cond ((integerp r) r)
        ((member r '(:sp :xzr :wzr sp xzr wzr)) 31)
        ((and (symbolp r) (member (char (symbol-name r) 0) '(#\X #\W #\Q))
              (every #'digit-char-p (subseq (symbol-name r) 1)))
         (parse-integer (symbol-name r) :start 1))
        (t (error "macho-cc: not a register: ~S" r))))

(defun u32 (x) (logand x #xFFFFFFFF))
(defun sbits (v n)
  "V as an N-bit two's complement field; an error if it does not fit."
  (unless (<= (- (ash 1 (1- n))) v (1- (ash 1 (1- n))))
    (error "macho-cc: ~D does not fit in ~D signed bits" v n))
  (logand v (1- (ash 1 n))))

(defun enc-movz (rd imm16 hw) (u32 (logior #xD2800000 (ash hw 21) (ash imm16 5) (reg rd))))
(defun enc-movk (rd imm16 hw) (u32 (logior #xF2800000 (ash hw 21) (ash imm16 5) (reg rd))))
(defun enc-movn (rd imm16 hw) (u32 (logior #x92800000 (ash hw 21) (ash imm16 5) (reg rd))))

(defun enc-addsub-imm (op rd rn imm &optional (shift12 nil))
  "ADD (OP 0) / SUB (OP 1) Xd|SP, Xn|SP, #IMM (12 bits, optionally LSL 12)."
  (unless (<= 0 imm 4095) (error "macho-cc: immediate ~D out of range" imm))
  (u32 (logior #x91000000 (ash op 30) (if shift12 (ash 1 22) 0)
               (ash imm 10) (ash (reg rn) 5) (reg rd))))
(defun enc-subs-imm (rd rn imm)
  (u32 (logior #xF1000000 (ash imm 10) (ash (reg rn) 5) (reg rd))))

(defun enc-alu-reg (op rd rn rm)
  "64-bit shifted-register ALU, shift 0.  OP: :add :sub :and :orr :eor :subs :ands."
  (let ((base (ecase op
                (:add #x8B000000) (:sub #xCB000000) (:subs #xEB000000)
                (:and #x8A000000) (:orr #xAA000000) (:eor #xCA000000) (:ands #xEA000000)
                (:orn #xAA200000))))
    (u32 (logior base (ash (reg rm) 16) (ash (reg rn) 5) (reg rd)))))

(defun enc-mov (rd rm)
  "MOV Xd, Xm (ORR Xd, XZR, Xm).  Neither may be SP."
  (enc-alu-reg :orr rd 31 rm))
(defun enc-mov-sp (rd rn) (enc-addsub-imm 0 rd rn 0))    ; MOV to/from SP

(defun enc-madd (rd rn rm ra) (u32 (logior #x9B000000 (ash (reg rm) 16) (ash (reg ra) 10) (ash (reg rn) 5) (reg rd))))
(defun enc-msub (rd rn rm ra) (u32 (logior #x9B008000 (ash (reg rm) 16) (ash (reg ra) 10) (ash (reg rn) 5) (reg rd))))
(defun enc-div (signed rd rn rm)
  (u32 (logior (if signed #x9AC00C00 #x9AC00800) (ash (reg rm) 16) (ash (reg rn) 5) (reg rd))))
(defun enc-shiftv (kind rd rn rm)
  (u32 (logior (ecase kind (:lsl #x9AC02000) (:lsr #x9AC02400) (:asr #x9AC02800))
               (ash (reg rm) 16) (ash (reg rn) 5) (reg rd))))
(defun enc-ubfm (rd rn immr imms) (u32 (logior #xD3400000 (ash immr 16) (ash imms 10) (ash (reg rn) 5) (reg rd))))
(defun enc-sbfm (rd rn immr imms) (u32 (logior #x93400000 (ash immr 16) (ash imms 10) (ash (reg rn) 5) (reg rd))))
(defun enc-lsl-imm (rd rn n) (enc-ubfm rd rn (mod (- 64 n) 64) (- 63 n)))
(defun enc-lsr-imm (rd rn n) (enc-ubfm rd rn n 63))
(defun enc-asr-imm (rd rn n) (enc-sbfm rd rn n 63))
(defun enc-sxtw (rd rn) (enc-sbfm rd rn 0 31))
(defun enc-uxtw (rd rn) (u32 (logior #x2A0003E0 (ash (reg rn) 16) (reg rd))))   ; MOV Wd, Wn

(defparameter +conds+ '(:eq 0 :ne 1 :hs 2 :lo 3 :mi 4 :pl 5 :vs 6 :vc 7
                        :hi 8 :ls 9 :ge 10 :lt 11 :gt 12 :le 13 :al 14))
(defun cond-code (c) (or (getf +conds+ c) (error "macho-cc: no condition ~S" c)))
(defun enc-cset (rd c)
  "CSET Xd, C  =  CSINC Xd, XZR, XZR, invert(C)."
  (u32 (logior #x9A9F07E0 (ash (logxor (cond-code c) 1) 12) (reg rd))))

;;; loads and stores, unsigned scaled offset (size 0..3 = 1/2/4/8 bytes)
(defun enc-ldst-uimm (load size rt rn off &key signed32)
  (let ((scale (ash 1 size)))
    (unless (and (zerop (mod off scale)) (<= 0 (floor off scale) 4095))
      (error "macho-cc: offset ~D not encodable for size ~D" off scale))
    (u32 (logior (ash size 30) #x39000000
                 (ash (cond (signed32 2) (load 1) (t 0)) 22)
                 (ash (floor off scale) 10) (ash (reg rn) 5) (reg rt)))))
(defun enc-ldst-simm9 (load size rt rn off)
  "LDUR/STUR: unscaled signed 9-bit offset."
  (u32 (logior (ash size 30) #x38000000 (ash (if load 1 0) 22)
               (ash (sbits off 9) 12) (ash (reg rn) 5) (reg rt))))
(defun enc-ldst-pre/post (load rt rn off pre)
  "LDR/STR Xt, [Xn, #off]! (PRE) or [Xn], #off."
  (u32 (logior #xF8000000 (ash (if load 1 0) 22) (ash (sbits off 9) 12)
               (if pre #xC00 #x400) (ash (reg rn) 5) (reg rt))))
(defun enc-ldstp (load kind rt1 rt2 rn off mode)
  "LDP/STP. KIND :x (64-bit) or :q (128-bit SIMD).  MODE :offset :pre :post."
  (let* ((scale (if (eq kind :x) 8 16))
         (opc (if (eq kind :x) #b10 #b10))
         (v (if (eq kind :q) 1 0))
         (base (ecase mode (:offset #b010) (:pre #b011) (:post #b001))))
    (unless (zerop (mod off scale)) (error "macho-cc: pair offset ~D unaligned" off))
    (when (eq kind :q) (setq opc #b10))
    (u32 (logior (ash opc 30) (ash #b101 27) (ash v 26) (ash base 23) (ash (if load 1 0) 22)
                 (ash (sbits (floor off scale) 7) 15) (ash (reg rt2) 10) (ash (reg rn) 5) (reg rt1)))))
(defun enc-ldar32 (rt rn) (u32 (logior #x88DFFC00 (ash (reg rn) 5) (reg rt))))
(defun enc-stlr32 (rt rn) (u32 (logior #x889FFC00 (ash (reg rn) 5) (reg rt))))
(defconstant +dmb-ish+ #xD5033BBF)
(defconstant +ret+ #xD65F03C0)
(defun enc-br (rn) (u32 (logior #xD61F0000 (ash (reg rn) 5))))
(defun enc-blr (rn) (u32 (logior #xD63F0000 (ash (reg rn) 5))))
(defun enc-brk (imm) (u32 (logior #xD4200000 (ash imm 5))))

;;; ---------------------------------------------------------- the shim object ----

(defstruct shim-object
  (text (make-array 0 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer 0))
  (labels (make-hash-table :test 'equal))   ; label -> (SECTION . OFFSET); SECTION :text :cstring :data
  (relocs nil)                              ; (INDEX KIND TARGET) newest first
  (cstring (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (data-size 0)
  (imports nil)                             ; ((NAME CSYM RESULT) ...) in declaration order
  (functions nil)                           ; defun / defasm names in order, for the symbol table
  (entry nil))

(defvar *obj*)
(defvar *consts*)      ; name -> value
(defvar *macros*)      ; name -> expander function (form -> form)
(defvar *globals*)     ; name -> (:defun nparams) (:import index result) (:data) (:string) (:asm)
(defvar *label-counter*)

(defun emit (w) (vector-push-extend (u32 w) (shim-object-text *obj*)) nil)
(defun here () (fill-pointer (shim-object-text *obj*)))
(defun new-label () (list :l (incf *label-counter*)))
(defun place-label (l)
  (when (gethash l (shim-object-labels *obj*)) (error "macho-cc: label ~S placed twice" l))
  (setf (gethash l (shim-object-labels *obj*)) (cons :text (* 4 (here)))))
(defun reloc (kind target)
  "Record that the word about to be emitted refers to TARGET (KIND says how)."
  (push (list (here) kind target) (shim-object-relocs *obj*)))
(defun emit-b (target &optional link)
  (reloc :b26 target) (emit (if link #x94000000 #x14000000)))
(defun emit-bcond (c target) (reloc :b19 target) (emit (logior #x54000000 (cond-code c))))
(defun emit-cbz (rt target &optional nz)
  (reloc :b19 target) (emit (logior (if nz #xB5000000 #xB4000000) (reg rt))))
(defun emit-adrp-add (rd target)
  "Xd = the address of TARGET, wherever the writer places it (ADRP + ADD :lo12)."
  (reloc :adrp target) (emit (logior #x90000000 (reg rd)))
  (reloc :add-lo12 target) (emit (enc-addsub-imm 0 rd rd 0)))
(defun emit-adrp-ldr (rd target)
  "Xd = the 8-byte word at TARGET (ADRP + LDR :lo12, scaled)."
  (reloc :adrp target) (emit (logior #x90000000 (reg rd)))
  (reloc :ldr64-lo12 target) (emit (enc-ldst-uimm t 3 rd rd 0)))

(defun emit-li (rd value)
  "Load the 64-bit VALUE (any integer, taken mod 2^64) into Xd."
  (let* ((v (logand value #xFFFFFFFFFFFFFFFF))
         (hws (loop for i below 4 collect (ldb (byte 16 (* 16 i)) v)))
         (zeros (count 0 hws)) (ones (count #xFFFF hws)))
    (cond ((> ones zeros)
           (let ((first t))
             (dotimes (i 4)
               (let ((h (nth i hws)))
                 (cond (first (when (or (/= h #xFFFF) (= i 3))
                                (emit (enc-movn rd (logxor h #xFFFF) i)) (setq first nil)))
                       ((/= h #xFFFF) (emit (enc-movk rd h i))))))
             (when first (emit (enc-movn rd 0 0)))))
          ((zerop v) (emit (enc-movz rd 0 0)))
          (t (let ((first t))
               (dotimes (i 4)
                 (let ((h (nth i hws)))
                   (cond (first (when (or (/= h 0) (= i 3))
                                  (emit (enc-movz rd h i)) (setq first nil)))
                         ((/= h 0) (emit (enc-movk rd h i)))))))))))

(defun emit-push (r) (emit (enc-ldst-pre/post nil r :sp -16 t)))      ; str Xr, [sp, #-16]!
(defun emit-pop (r) (emit (enc-ldst-pre/post t r :sp 16 nil)))        ; ldr Xr, [sp], #16
(defun emit-add-imm-big (rd rn n)
  "Xd = Xn + N for 0 <= N < 2^24 (SP allowed)."
  (let ((hi (ldb (byte 12 12) n)) (lo (ldb (byte 12 0) n)) (src rn))
    (unless (< n (ash 1 24)) (error "macho-cc: ~D too big" n))
    (when (plusp hi) (emit (enc-addsub-imm 0 rd src hi t)) (setq src rd))
    (when (or (plusp lo) (zerop hi) ) (emit (enc-addsub-imm 0 rd src lo)))))
(defun emit-sub-imm-big (rd rn n)
  (let ((hi (ldb (byte 12 12) n)) (lo (ldb (byte 12 0) n)) (src rn))
    (unless (< n (ash 1 24)) (error "macho-cc: ~D too big" n))
    (when (plusp hi) (emit (enc-addsub-imm 1 rd src hi t)) (setq src rd))
    (when (or (plusp lo) (zerop hi)) (emit (enc-addsub-imm 1 rd src lo)))))

;;; ------------------------------------------------------------- the compiler ----

(defstruct fn-ctx
  (slots nil)          ; ((NAME . FRAME-OFFSET) ...): the variable lives at [x29 - OFFSET]
  (frame 0)            ; bytes of frame below x29 used so far
  (max-frame 0)
  (exit nil)           ; the epilogue's label
  (break nil))         ; the innermost WHILE's exit label
(defvar *fn*)

(defun alloc-frame (bytes &optional (align 8))
  (let ((f (* align (ceiling (+ (fn-ctx-frame *fn*) bytes) align))))
    (setf (fn-ctx-frame *fn*) f)
    (setf (fn-ctx-max-frame *fn*) (max f (fn-ctx-max-frame *fn*)))
    f))

(defun var-offset (name env)
  (cdr (assoc name env)))

(defun emit-frame-addr (rd off)
  "Xd = x29 - OFF."
  (emit-sub-imm-big rd 29 off))
(defun emit-load-var (rd off)
  (if (<= off 256)
      (emit (enc-ldst-simm9 t 3 rd 29 (- off)))
      (progn (emit-frame-addr 9 off) (emit (enc-ldst-uimm t 3 rd 9 0)))))
(defun emit-store-var (rs off)
  (if (<= off 256)
      (emit (enc-ldst-simm9 nil 3 rs 29 (- off)))
      (progn (emit-frame-addr 9 off) (emit (enc-ldst-uimm nil 3 rs 9 0)))))

(defun const-fold (form)
  "FORM's value if it is a compile-time constant (an integer, a DEFCONST, or + - * LOGIOR
   LOGAND of such), else NIL."
  (cond ((integerp form) form)
        ((symbolp form) (and form (nth-value 1 (gethash form *consts*)) (gethash form *consts*)))
        ((and (consp form) (member (car form) '(+ - * logior logand)) (cdr form))
         (let ((vals (mapcar #'const-fold (cdr form))))
           (and (every #'integerp vals) (apply (car form) vals))))))
(defun constantp* (form) (integerp (const-fold form)))
(defun const-value (form) (const-fold form))

(defun macroexpand-shim (form)
  (loop
    (let ((m (and (consp form) (symbolp (car form)) (gethash (car form) *macros*))))
      (if m (setq form (funcall m form)) (return form)))))

(defun cx (form env)
  "Compile FORM so that its value is in x0."
  (setq form (macroexpand-shim form))
  (cond
    ((integerp (const-fold form)) (emit-li 0 (const-fold form)))
    ((symbolp form)
     (cond ((nth-value 1 (gethash form *consts*)) (emit-li 0 (gethash form *consts*)))
           ((var-offset form env) (emit-load-var 0 (var-offset form env)))
           ((null form) (emit-li 0 0))
           ((eq form t) (emit-li 0 1))
           (t (error "macho-cc: unknown variable ~S" form))))
    ((not (consp form)) (error "macho-cc: cannot compile ~S" form))
    (t (cx-op (car form) (cdr form) env form))))

(defun cx-progn (body env)
  (if body (dolist (f body) (cx f env)) (emit-li 0 0)))

(defun cx-binary (args env op)
  "Fold ARGS left to right with OP, which combines x0 (left) and x1 (right) into x0."
  (cx (first args) env)
  (dolist (b (rest args))
    (if (and (constantp* b) (member op '(:add :sub)) (<= 0 (const-value b) 4095))
        (emit (enc-addsub-imm (if (eq op :add) 0 1) 0 0 (const-value b)))
        (progn (emit-push 0) (cx b env) (emit (enc-mov 1 0)) (emit-pop 0)
               (ecase op
                 ((:add :sub :and :orr :eor) (emit (enc-alu-reg op 0 0 1)))
                 (:mul (emit (enc-madd 0 0 1 31)))
                 (:sdiv (emit (enc-div t 0 0 1)))
                 (:udiv (emit (enc-div nil 0 0 1)))
                 (:urem (emit (enc-div nil 2 0 1)) (emit (enc-msub 0 2 1 0)))
                 ((:lsl :lsr :asr) (emit (enc-shiftv op 0 0 1))))))))

(defun cx-compare (a b env c)
  (cx a env)
  (if (and (constantp* b) (<= 0 (const-value b) 4095))
      (emit (enc-subs-imm 31 0 (const-value b)))
      (progn (emit-push 0) (cx b env) (emit (enc-mov 1 0)) (emit-pop 0)
             (emit (enc-alu-reg :subs 31 0 1))))
  (emit (enc-cset 0 c)))

(defun cx-mem-addr (args env)
  "The address (first ARGS) plus the optional constant offset (second ARGS), into x0."
  (cx (first args) env)
  (when (second args)
    (let ((off (const-value (second args))))
      (unless (integerp off) (error "macho-cc: memory offset must be constant: ~S" args))
      (cond ((zerop off))
            ((< 0 off 4096) (emit (enc-addsub-imm 0 0 0 off)))
            ((< -4096 off 0) (emit (enc-addsub-imm 1 0 0 (- off))))
            (t (emit-li 1 off) (emit (enc-alu-reg :add 0 0 1)))))))

(defun cx-call-args (args env n-reg n-var-stack callee-x16)
  "Evaluate ARGS, then load the first N-REG into x0.. and the rest onto the outgoing stack
   (8-byte slots from [sp]).  CALLEE-X16, a thunk, loads the call target into x16 last.
   Leaves SP lowered by the outgoing area; returns the bytes to add back after the call."
  (let* ((n (length args)) (n-stack (max 0 (- n n-reg)))
         (out (* 16 (ceiling (* 8 n-stack) 16))))
    (declare (ignorable n-var-stack))
    (dolist (a args) (cx a env) (emit-push 0))
    ;; the I-th argument is at [sp + 16*(n-1-i)] before the outgoing area is made
    (when (plusp out) (emit-sub-imm-big :sp :sp out))
    (loop for i from n-reg below n
          do (emit (enc-ldst-uimm t 3 9 :sp (+ out (* 16 (- n 1 i)))))
             (emit (enc-ldst-uimm nil 3 9 :sp (* 8 (- i n-reg)))))
    (loop for i below (min n n-reg)
          do (emit (enc-ldst-uimm t 3 i :sp (+ out (* 16 (- n 1 i))))))
    (funcall callee-x16)
    (+ out (* 16 n))))

(defun cx-finish-call (pop result)
  (emit (enc-blr 16))
  (when (plusp pop) (emit-add-imm-big :sp :sp pop))
  (ecase result
    ((:i64 :data) nil)
    (:i32 (emit (enc-sxtw 0 0)))
    (:u32 (emit (enc-uxtw 0 0)))
    (:u8 (emit (u32 (logior #x92401C00 (ash 0 5) 0)))) ; and x0, x0, #0xff
    (:void nil)))

(defun import-info (name)
  (let ((g (gethash name *globals*)))
    (unless (and g (eq (car g) :import)) (error "macho-cc: ~S is not an import" name))
    g))

(defun cx-op (op args env whole)
  (case op
    (quote (cx (first args) env))
    (progn (cx-progn args env))
    ((let let*) (let ((env2 env))
           (dolist (b (first args))
             (let* ((name (if (consp b) (first b) b))
                    (init (if (consp b) (second b) 0)))
               (cx init env2)
               (let ((off (alloc-frame 8)))
                 (emit-store-var 0 off)
                 (push (cons name off) env2))))
           (cx-progn (rest args) env2)))
    (with-buffers
     (let ((env2 env))
       (dolist (b (first args))
         (let* ((size (const-value (second b)))
                (boff (alloc-frame size 16))
                (off (alloc-frame 8)))
           (emit-frame-addr 0 boff)
           (emit-store-var 0 off)
           (push (cons (first b) off) env2)))
       (cx-progn (rest args) env2)))
    (setq (loop for (v e) on args by #'cddr
                do (let ((off (var-offset v env)))
                     (unless off (error "macho-cc: SETQ of unknown variable ~S" v))
                     (cx e env) (emit-store-var 0 off))))
    (if (let ((else (new-label)) (end (new-label)))
          (cx (first args) env)
          (emit-cbz 0 else)
          (cx (second args) env)
          (emit-b end)
          (place-label else)
          (cx (third args) env)
          (place-label end)))
    (when (cx `(if ,(first args) (progn ,@(rest args)) 0) env))
    (unless (cx `(if ,(first args) 0 (progn ,@(rest args))) env))
    (cond (cx (if (null args) 0
                  (destructuring-bind ((test . body) . more) args
                    `(if ,test ,(if body `(progn ,@body) 1) (cond ,@more))))
              env))
    (and (cond ((null args) (emit-li 0 1))
               ((null (rest args)) (cx (first args) env))
               (t (cx `(if ,(first args) (and ,@(rest args)) 0) env))))
    (or (let ((end (new-label)))
          (dolist (a args) (cx a env) (emit-cbz 0 end t))
          (emit-li 0 0)
          (place-label end)))
    (not (cx (first args) env) (emit (enc-subs-imm 31 0 0)) (emit (enc-cset 0 :eq)))
    (while (let ((top (new-label)) (end (new-label)) (outer (fn-ctx-break *fn*)))
             (place-label top)
             (cx (first args) env)
             (emit-cbz 0 end)
             (setf (fn-ctx-break *fn*) end)
             (cx-progn (rest args) env)
             (setf (fn-ctx-break *fn*) outer)
             (emit-b top)
             (place-label end)
             (emit-li 0 0)))
    (break (unless (fn-ctx-break *fn*) (error "macho-cc: BREAK outside WHILE"))
     (emit-b (fn-ctx-break *fn*)))
    (return (if args (cx (first args) env) (emit-li 0 0)) (emit-b (fn-ctx-exit *fn*)))
    (+ (cx-binary args env :add))
    (- (if (rest args) (cx-binary args env :sub)
           (progn (cx (first args) env) (emit (enc-alu-reg :sub 0 31 0)))))
    (* (cx-binary args env :mul))
    (sdiv (cx-binary args env :sdiv))
    (udiv (cx-binary args env :udiv))
    (urem (cx-binary args env :urem))
    (logand (cx-binary args env :and))
    (logior (cx-binary args env :orr))
    (logxor (cx-binary args env :eor))
    (lognot (cx (first args) env) (emit (enc-alu-reg :orn 0 31 0)))
    ((shl shr sar)
     (if (constantp* (second args))
         (let ((n (const-value (second args))))
           (cx (first args) env)
           (unless (zerop n)
             (emit (ecase op (shl (enc-lsl-imm 0 0 n)) (shr (enc-lsr-imm 0 0 n)) (sar (enc-asr-imm 0 0 n))))))
         (cx-binary args env (ecase op (shl :lsl) (shr :lsr) (sar :asr)))))
    (sext32 (cx (first args) env) (emit (enc-sxtw 0 0)))
    (zext32 (cx (first args) env) (emit (enc-uxtw 0 0)))
    (= (cx-compare (first args) (second args) env :eq))
    (/= (cx-compare (first args) (second args) env :ne))
    (< (cx-compare (first args) (second args) env :lt))
    (> (cx-compare (first args) (second args) env :gt))
    (<= (cx-compare (first args) (second args) env :le))
    (>= (cx-compare (first args) (second args) env :ge))
    (u< (cx-compare (first args) (second args) env :lo))
    (u> (cx-compare (first args) (second args) env :hi))
    (u<= (cx-compare (first args) (second args) env :ls))
    (u>= (cx-compare (first args) (second args) env :hs))
    ((ld8 ld16 ld32 lds32 ld64)
     (cx-mem-addr args env)
     (ecase op
       (ld8 (emit (enc-ldst-uimm t 0 0 0 0)))
       (ld16 (emit (enc-ldst-uimm t 1 0 0 0)))
       (ld32 (emit (enc-ldst-uimm t 2 0 0 0)))
       (lds32 (emit (enc-ldst-uimm t 2 0 0 0 :signed32 t)))
       (ld64 (emit (enc-ldst-uimm t 3 0 0 0)))))
    ((st8 st16 st32 st64)
     ;; (stN ADDR VALUE [OFF]); the value is the form's value
     (cx-mem-addr (list (first args) (third args)) env)
     (emit-push 0)
     (cx (second args) env)
     (emit-pop 1)
     (emit (enc-ldst-uimm nil (ecase op (st8 0) (st16 1) (st32 2) (st64 3)) 0 1 0)))
    (atomic-ld32 (cx (first args) env) (emit (enc-ldar32 0 0)))
    (atomic-st32 (cx (first args) env) (emit-push 0) (cx (second args) env) (emit-pop 1)
     (emit (enc-stlr32 0 1)) (emit +dmb-ish+))
    (addr (let* ((name (first args)) (g (gethash name *globals*)))
            (unless (and g (member (car g) '(:defun :data :string :asm)))
              (error "macho-cc: (ADDR ~S): not a function, data, string or asm label" name))
            (emit-adrp-add 0 name)))
    (import-value (import-info (first args)) (emit-adrp-ldr 0 (list :got (first args))))
    (ccall (destructuring-bind (name &rest cargs) args
             (let ((g (import-info name)))
               (cx-finish-call (cx-call-args cargs env 8 0 (lambda () (emit-adrp-ldr 16 (list :got name))))
                               (third g)))))
    (ccallv (destructuring-bind (name nfixed &rest cargs) args
              (let ((g (import-info name)))
                (when (> nfixed 8) (error "macho-cc: variadic with > 8 fixed"))
                (cx-finish-call (cx-call-args cargs env nfixed 0
                                              (lambda () (emit-adrp-ldr 16 (list :got name))))
                                (third g)))))
    (callptr (destructuring-bind (f &rest cargs) args
               (cx f env) (emit-push 0)
               (let ((pop (cx-call-args cargs env 8 0
                                        (lambda ()
                                          ;; the target was pushed first: it sits above the
                                          ;; arguments (and the outgoing area, if any)
                                          (let ((n (length cargs)) (stk (max 0 (- (length cargs) 8))))
                                            (emit (enc-ldst-uimm t 3 16 :sp (+ (* 16 (ceiling (* 8 stk) 16)) (* 16 n)))))))))
                 (cx-finish-call (+ pop 16) :i64))))
    (t
     (let ((g (gethash op *globals*)))
       (cond ((and g (member (car g) '(:defun :asm)))
              (unless (or (eq (car g) :asm) (= (length args) (second g)))
                (error "macho-cc: ~S takes ~D arguments, given ~D: ~S" op (second g) (length args) whole))
              (when (> (length args) 8) (error "macho-cc: more than 8 arguments to ~S" op))
              (let ((pop (cx-call-args args env 8 0 (lambda () nil))))
                (emit-b op t)
                (when (plusp pop) (emit-add-imm-big :sp :sp pop))))
             (t (error "macho-cc: unknown operator ~S in ~S" op whole)))))))

;;; ------------------------------------------------------------------ toplevel ----

(defun compile-defun (name params body)
  (when (> (length params) 8) (error "macho-cc: ~S has more than 8 parameters" name))
  (let* ((*fn* (make-fn-ctx :exit (new-label)))
         (env nil)
         (frame-word nil))
    (place-label name)
    (emit (enc-ldstp nil :x 29 30 :sp -16 :pre))      ; stp x29, x30, [sp, #-16]!
    (emit (enc-mov-sp 29 :sp))                        ; mov x29, sp
    ;; the frame size is known only at the end: reserve two words for SUB SP and patch them
    (setq frame-word (here)) (emit 0) (emit 0)
    (loop for p in params for i from 0
          do (let ((off (alloc-frame 8)))
               (emit-store-var i off)
               (push (cons p off) env)))
    (cx-progn body env)
    (place-label (fn-ctx-exit *fn*))
    (emit (enc-mov-sp :sp 29))                        ; mov sp, x29
    (emit (enc-ldstp t :x 29 30 :sp 16 :post))        ; ldp x29, x30, [sp], #16
    (emit +ret+)
    (let* ((frame (* 16 (ceiling (fn-ctx-max-frame *fn*) 16)))
           (hi (ldb (byte 12 12) frame)) (lo (ldb (byte 12 0) frame))
           (text (shim-object-text *obj*)))
      (unless (< frame (ash 1 24)) (error "macho-cc: ~S's frame is too big" name))
      (setf (aref text frame-word) (enc-addsub-imm 1 :sp :sp hi t)
            (aref text (1+ frame-word)) (enc-addsub-imm 1 :sp :sp lo)))))

(defun asm-insn (i)
  "One hand-written instruction (DEFASM).  The forms are the few the stubs need."
  (destructuring-bind (op &rest a) i
    (ecase op
      (:label (place-label (first a)))
      (:word (emit (first a)))
      (:stp (emit (enc-ldstp nil :x (first a) (second a) (third a) (fourth a) (or (fifth a) :offset))))
      (:ldp (emit (enc-ldstp t :x (first a) (second a) (third a) (fourth a) (or (fifth a) :offset))))
      (:stpq (emit (enc-ldstp nil :q (first a) (second a) (third a) (fourth a) :offset)))
      (:ldpq (emit (enc-ldstp t :q (first a) (second a) (third a) (fourth a) :offset)))
      (:ldr (emit (enc-ldst-uimm t 3 (first a) (second a) (third a))))
      (:str (emit (enc-ldst-uimm nil 3 (first a) (second a) (third a))))
      (:ldr32 (emit (enc-ldst-uimm t 2 (first a) (second a) (third a))))
      (:add (emit (enc-addsub-imm 0 (first a) (second a) (third a))))
      (:sub (emit (enc-addsub-imm 1 (first a) (second a) (third a))))
      (:mov (emit (if (or (member (first a) '(:sp sp)) (member (second a) '(:sp sp)))
                      (enc-mov-sp (first a) (second a))
                      (enc-mov (first a) (second a)))))
      (:movz (emit (enc-movz (first a) (second a) (or (third a) 0))))
      (:li (emit-li (first a) (second a)))
      (:uxtw (emit (enc-uxtw (first a) (second a))))
      (:bl (emit-b (first a) t))
      (:b (emit-b (first a)))
      (:blr (emit (enc-blr (first a))))
      (:br (emit (enc-br (first a))))
      (:adr-addr (emit-adrp-add (first a) (second a)))
      (:ret (emit +ret+))
      (:brk (emit (enc-brk (first a))))
      (:svc (emit #xD4000001)))))                     ; SVC #0: the Linux test harness only

(defun define-global (name info)
  (when (gethash name *globals*) (error "macho-cc: ~S defined twice" name))
  (setf (gethash name *globals*) info))

(defun align-up (n a) (* a (ceiling n a)))

(defun compile-shim (forms &key (entry 'main))
  "Compile the toplevel FORMS (a list) into a SHIM-OBJECT."
  (let ((*obj* (make-shim-object))
        (*consts* (make-hash-table :test 'eq))
        (*macros* (make-hash-table :test 'eq))
        (*globals* (make-hash-table :test 'eq))
        (*label-counter* 0)
        (imports nil))
    ;; pass 1: names, constants, macros, data, strings
    (dolist (f forms)
      (ecase (car f)
        (defconst (setf (gethash (second f) *consts*)
                        (let ((v (third f)))
                          (if (integerp v) v (eval (subst-consts v))))))
        (defmacro (destructuring-bind (name ll &body body) (rest f)
                    (setf (gethash name *macros*)
                          (let ((fn (coerce `(lambda (form) (destructuring-bind ,ll (cdr form) ,@body))
                                            'function)))
                            fn))))
        (defimport (destructuring-bind (name csym &optional (result :i64)) (rest f)
                     (define-global name (list :import (length imports) result))
                     (push (list name csym result) imports)))
        (defdata (destructuring-bind (name bytes &optional (align 8)) (rest f)
                   (define-global name (list :data))
                   (let ((at (align-up (shim-object-data-size *obj*) align)))
                     (setf (gethash name (shim-object-labels *obj*)) (cons :data at))
                     (setf (shim-object-data-size *obj*) (+ at (const-value* bytes))))))
        (defstring (destructuring-bind (name text) (rest f)
                     (define-global name (list :string))
                     (let ((cs (shim-object-cstring *obj*)))
                       (setf (gethash name (shim-object-labels *obj*)) (cons :cstring (fill-pointer cs)))
                       (loop for ch across text do (vector-push-extend (char-code ch) cs))
                       (vector-push-extend 0 cs))))
        (defun (define-global (second f) (list :defun (length (third f)))))
        (defasm (define-global (second f) (list :asm)))
        (defextern (define-global (second f) (list :data)))))
    (setf (shim-object-imports *obj*) (reverse imports))
    ;; pass 2: code
    (dolist (f forms)
      (case (car f)
        (defun (push (second f) (shim-object-functions *obj*))
         (let ((body (cdddr f)))
           (when (and (stringp (first body)) (rest body)) (pop body))   ; a docstring
           (compile-defun (second f) (third f) body)))
        (defasm (push (second f) (shim-object-functions *obj*))
         (place-label (second f))
         (dolist (i (cddr f)) (asm-insn i)))))
    (setf (shim-object-functions *obj*) (reverse (shim-object-functions *obj*)))
    (unless (gethash entry *globals*) (error "macho-cc: no entry function ~S" entry))
    (setf (shim-object-entry *obj*) entry)
    *obj*))

(defun subst-consts (form)
  "FORM with every constant name replaced by its value (for a DEFCONST computed from others)."
  (cond ((and (symbolp form) (nth-value 1 (gethash form *consts*))) (gethash form *consts*))
        ((consp form) (cons (subst-consts (car form)) (subst-consts (cdr form))))
        (t form)))
(defun const-value* (x)
  (let ((v (if (integerp x) x (gethash x *consts*))))
    (unless (integerp v) (error "macho-cc: ~S is not a constant" x))
    v))

(defun read-shim-file (path)
  "The toplevel forms of the shim source at PATH, read in this package."
  (let ((*package* (find-package :modus.macho))
        (*read-base* 10) (*read-eval* nil))
    (with-open-file (in path)
      (loop for f = (read in nil :eof) until (eq f :eof) collect f))))
