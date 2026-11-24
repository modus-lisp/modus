;;;; jit-ppc-native.lisp -- REPL code as NATIVE PowerPC, by the image's own emitter
;;;;
;;;; The i386 native tier's design (mvm/jit-i386-native.lisp, itself the 68k
;;;; tier's) on 32-bit PowerPC: a function defined at run time goes through
;;;; translate-ppc.lisp's per-instruction translator (PPC-TRANSLATE-INSN) -- the
;;;; same code that translated the image, baked into it (MODUS_PPC_JIT=1) --
;;;; into a code area, instead of staying MVM bytecode for the interpreter.  The
;;;; port has no other JIT: without this, everything loaded at run time (glass,
;;;; scribe: a window manager and its fonts) is interpreted.
;;;;
;;;; What differs from translating the image:
;;;;   1. WHERE THINGS ARE.  The buffer's final address (BASE) is known before
;;;;      emission, so a routine outside it -- the collector, the handler-frame
;;;;      stubs -- is a label at (ADDRESS - BASE), found through the export table
;;;;      the AOT translator writes in front of %NPPC-EXPORT-TABLE.  Those
;;;;      routines are at the image's END, within a `bl''s +-32 MB of the code
;;;;      area just above it; image FUNCTIONS are not (the image is 35 MB), so
;;;;      a call to one is absolute: lis/ori r12, mtctr, bctrl.
;;;;   2. CALL OPERANDS name a function-table index, a bytecode offset, or a
;;;;      runtime name (+NPPC-RT-BASE+ + n).  This module's functions are called
;;;;      by `bl', native IMAGE functions absolutely; any other runtime name --
;;;;      a runtime DEFUN, a generic function, a name not defined yet -- is
;;;;      LATE-BOUND, through a cell of the constant vector that every DEFUN of
;;;;      the name refreshes, dispatched as FUNCALL is (a closure sets the
;;;;      closure environment; NIL signals UNDEFINED-FUNCTION).
;;;;   3. CONSTANTS are heap objects the collector moves, so LI-CONST loads its
;;;;      object from the vector the root word +PPC-NATIVE-CONST-ROOT+ holds
;;;;      (inside the low block, which every collection scans).
;;;; The emitter's configuration is the image's own, carried in by %NPPC-COINIT,
;;;; whose body the build writes (translate-ppc PPC-NATIVE-COINIT-TEXT).
;;;;
;;;; A DEFUN is translated at its first call (%RUNTIME-DEFUN-HOOK installs a
;;;; stub), (compile 'f) at once (%JIT-EAGER-ALL), and a top-level form that
;;;; builds closures as it runs (%RUNTIME-FORM-NATIVE).  Anything this cannot
;;;; do leaves the module interpreted, with the reason in *NPPC-REFUSED*.

(in-package :modus.mvm)

(defconstant +nppc-rt-base+ #x40000000)        ; interp.lisp +mvm-runtime-call-base+
(defconstant +nppc-area-addr+ #x02800000
  "The code area, just above the image (which the build keeps below it): the
   collector and the handler stubs at the image's end are within `bl' reach,
   and an address here is a fixnum on the 30-bit tower.  Hosted: mapped here
   (MAP_FIXED).  The PowerPC Mac: plain RAM under boot-ppcmac's DBAT0, below
   the dead buffer Open Firmware read the file into.")
(defconstant +nppc-area-size+ #x01800000)

;;; THE TIER'S STATE HAS NO INITIAL VALUES, on purpose (see jit-i386-native).
(defvar *nppc-off*)
(defvar *nppc-ready*)
(defvar *nppc-exports*)
(defvar *nppc-why*)
(defvar *nppc-at*)
(defvar *nppc-ptr*)
(defvar *nppc-end*)
(defvar *nppc-consts*)
(defvar *nppc-const-slots*)
(defvar *nppc-const-fill*)
(defvar *nppc-translated*)
(defvar *nppc-refused*)
(defvar *nppc-done*)
(defvar *nppc-pending*)
(defvar *nppc-name-slots*)
(defvar *nppc-forms*)
(defvar *nppc-forms-translated*)

(defun %nppc-coinit ()
  "The emitter's configuration as this image was translated with it."
  '(:nppc-coinit :pending))

;;; ---------------------------------------------------------------------------
;;; The image's facts

(defun %nppc-object (w)
  "The Lisp object whose machine word is W (odd: a tagged pointer)."
  (%fixnum-+ (ash (- w 1) -1) (%word->val 1)))

(defun %nppc-u32 (a)
  "The big-endian 32-bit word at A as an exact integer (two 16-bit loads)."
  (+ (* 65536 (mem-ref a :u16)) (mem-ref (+ a 2) :u16)))

(defun %nppc-magic-at-p (a)
  ;; "modusPXL", big-endian halves
  (and (= (mem-ref a :u16) #x6D6F) (= (mem-ref (+ a 2) :u16) #x6475)
       (= (mem-ref (+ a 4) :u16) #x7350) (= (mem-ref (+ a 6) :u16) #x584C)))

(defun %nppc-export-table ()
  "A marker: the AOT translator writes the export table just in front of this
   function's code (translate-ppc, PPC-EMIT-NATIVE-EXPORTS)."
  nil)

(defun %nppc-find-exports ()
  "The export table: a few words before %NPPC-EXPORT-TABLE's code."
  (let ((a (logand (- (%val->word (function %nppc-export-table)) 3) -4)) (found nil))
    (loop repeat 64
          until found
          do (setq a (- a 4))
             (when (and (%nppc-magic-at-p a) (= (%nppc-u32 (+ a 8)) 3))
               (setq found a)))
    (unless found (error "jit-ppc-native: no export table in the image"))
    (let* ((k (%nppc-u32 (+ found 8))) (v (make-array k :initial-element nil)))
      (dotimes (i k)
        (let ((off (%nppc-u32 (+ found 12 (* 4 i)))))
          (unless (= off #x7FFFFFFF)
            (setf (aref v i) (+ found (if (>= off #x80000000) (- off #x100000000) off))))))
      (setq *nppc-exports* v))))

(defun %nppc-area ()
  "The code area at +NPPC-AREA-ADDR+.  Hosted: mmap(addr, size, RWX,
   PRIVATE|ANONYMOUS|FIXED); on the Mac the address is RAM already."
  (unless *ppc-mac-mode*
    (let ((a (syscall6 90 +nppc-area-addr+ +nppc-area-size+ 7 #x32 -1 0)))
      (unless (eql a +nppc-area-addr+)
        (error "jit-ppc-native: mmap at #x~X failed: ~S" +nppc-area-addr+ a))))
  (setq *nppc-ptr* +nppc-area-addr+ *nppc-end* (+ +nppc-area-addr+ +nppc-area-size+)))

(defun %nppc-root-set (v)
  "Point the root word at constant vector V: its raw (tagged) word, which the
   collector forwards in place (the low block is scanned whole).  A heap word
   is below 2^29, a fixnum here."
  (setf (mem-ref +ppc-native-const-root+ :u32) (%val->word v)))

(defun %nppc-grow-consts ()
  (let* ((n *nppc-const-fill*)
         (new (make-array (* 2 (length *nppc-consts*)) :initial-element nil)))
    (dotimes (i n) (setf (svref new i) (svref *nppc-consts* i)))
    (setq *nppc-consts* new)
    (%nppc-root-set new)))

(defun %nppc-const-slot (idx)
  "The slot of *NPPC-CONSTS* holding pool constant IDX (filled on first use)."
  (or (gethash idx *nppc-const-slots*)
      (let ((n *nppc-const-fill*))
        (when (>= n (length *nppc-consts*)) (%nppc-grow-consts))
        (setf (svref *nppc-consts* n) (%e2-const-ref idx))
        (setq *nppc-const-fill* (+ n 1))
        (setf (gethash idx *nppc-const-slots*) n))))

(defun %nppc-current-fn (name)
  "What a call to NAME reaches now: its function object, or NIL."
  (%mvm-resolve-runtime-fn name))

(defun %nppc-name-slot (name)
  "The cell of runtime function NAME, made on first use holding its object now."
  (or (gethash name *nppc-name-slots*)
      (let ((n *nppc-const-fill*))
        (when (>= n (length *nppc-consts*)) (%nppc-grow-consts))
        (setf (svref *nppc-consts* n) (%nppc-current-fn name))
        (setq *nppc-const-fill* (+ n 1))
        (setf (gethash name *nppc-name-slots*) n))))

(defun %nppc-refresh (names)
  "NAMES were (re)defined: their cells, if any, hold what a call reaches now."
  (when (and (boundp (quote *nppc-name-slots*)) (hash-table-p *nppc-name-slots*))
    (dolist (nm names)
      (let ((slot (gethash nm *nppc-name-slots*)))
        (when slot (setf (svref *nppc-consts* slot) (%nppc-current-fn nm)))))))

(defun %nppc-init ()
  (unless *nppc-ready*
    (%nppc-coinit)
    (unless (integerp *mvm-label-counter*) (setq *mvm-label-counter* 0))
    (%nppc-find-exports)
    (%nppc-area)
    (setq *nppc-consts* (make-array 256 :initial-element nil)
          *nppc-const-slots* (make-hash-table) *nppc-const-fill* 0
          *nppc-name-slots* (make-hash-table :test (function equal)))
    (%nppc-root-set *nppc-consts*)
    (setq *nppc-translated* 0 *nppc-refused* nil *nppc-done* nil *nppc-pending* nil
          *nppc-forms* (make-hash-table) *nppc-forms-translated* 0 *nppc-why* nil
          *nppc-at* nil *nppc-ready* t))
  t)

;;; ---------------------------------------------------------------------------
;;; Translation

(defun %nppc-abs-label (buf base addr)
  "A label at absolute ADDR, for a buffer that will be written at BASE."
  (let ((l (mvm-make-label)))
    (setf (gethash l (ppc-buffer-labels buf)) (- addr base))
    l))

(defun %nppc-image-fn-addr (name)
  "The entry address of image (or native runtime) function NAME, or NIL."
  (let* ((fn (%mvm-resolve-runtime-fn name))
         (w (and fn (%val->word fn))))
    (and w (= (logand w 15) 3) (- w 3))))

(defun %nppc-call-operands (bc start end)
  "Every CALL / TAILCALL / FN-ADDR operand in [START, END) of BC."
  (let ((pc start) (out nil))
    (loop while (< pc end)
          do (let* ((d (decode-instruction bc pc)) (op (car d)) (operands (cadr d)))
               (cond ((or (= op +op-call+) (= op +op-tailcall+)) (push (first operands) out))
                     ((= op +op-fn-addr+)
                      (unless (eql (second operands) #xFFFFFFF0) (push (second operands) out))))
               (setq pc (cddr d))))
    out))

(defun %nppc-resolve (operand rt fn-table entries)
  "What call operand OPERAND names: (:LOCAL . label) for an entry of this
   module, (:ABS . address) for a native image function, (:LATE . slot) for a
   late-bound runtime name, or NIL (*NPPC-WHY*)."
  (if (>= operand +nppc-rt-base+)
      (let* ((name (gethash operand rt))
             (a (and name (%nppc-image-fn-addr name))))
        (cond ((null name) (setq *nppc-why* (list :no-name operand)) nil)
              ;; an IMAGE function: a direct call (the code area holds runtime
              ;; functions, which are late-bound so a redefinition is seen)
              ((and a (< a +nppc-area-addr+)) (cons :abs a))
              (t (cons :late (%nppc-name-slot name)))))
      (let* ((pc (if (< operand (length fn-table)) (aref fn-table operand) operand))
             (l (gethash pc entries)))
        (if l (cons :local l)
            (progn (setq *nppc-why* (list :no-callee operand)) nil)))))

(defun %nppc-scan-branch-targets (bc start end label-map)
  "Labels for every branch target in [START, END), as the AOT translator's
   first pass makes them."
  (let ((pc start))
    (loop while (< pc end)
          do (let* ((d (decode-instruction bc pc)) (op (car d)) (operands (cadr d))
                    (npc (cddr d)) (info (gethash op *opcode-table*)))
               (when info
                 (let ((types (opcode-info-operands info)))
                   (when (member :off32 types)
                     (let ((target (+ npc (if (member :reg types) (second operands) (first operands)))))
                       (unless (gethash target label-map)
                         (setf (gethash target label-map) (mvm-make-label)))))))
               (setq pc npc)))))

(defun %nppc-emit-abs-call (buf addr &optional tail)
  "Call (or with TAIL, jump to) absolute ADDR through CTR: r12 is scratch."
  (ppc-emit-li buf +ppc-r12+ addr)
  (ppc-emit-mtctr buf +ppc-r12+)
  (if tail (ppc-emit-bctr buf) (ppc-emit-bctrl buf)))

(defun %nppc-emit-load-slot (buf dst slot)
  "DST = the object in slot SLOT of the constant vector the root word holds."
  (let ((off (- (* 4 (+ slot 1)) +tag-object+)))
    (ppc-emit-li buf dst +ppc-native-const-root+)
    (ppc-emit-lwz buf dst dst 0)
    (if (< off 32768)
        (ppc-emit-lwz buf dst dst off)
        (progn (ppc-emit-li buf +ppc-r0+ off)
               (ppc-emit-add buf dst dst +ppc-r0+)
               (ppc-emit-lwz buf dst dst 0)))))

(defun %nppc-emit-tail-frame-pop (buf)
  "What +OP-TAILCALL+ does before its branch: callee-saved registers, VFP and LR
   back from this frame, then the frame popped."
  (let* ((ws 4) (base (* 6 ws)))
    (ppc-emit-load-word buf +ppc-r14+ +ppc-r1+ base)
    (ppc-emit-load-word buf +ppc-r15+ +ppc-r1+ (+ base (* 1 ws)))
    (ppc-emit-load-word buf +ppc-r16+ +ppc-r1+ (+ base (* 2 ws)))
    (ppc-emit-load-word buf +ppc-r17+ +ppc-r1+ (+ base (* 3 ws)))
    (ppc-emit-load-word buf +ppc-r18+ +ppc-r1+ (+ base (* 4 ws)))
    (ppc-emit-restore-locals buf)
    (ppc-emit-load-word buf +ppc-r31+ +ppc-r1+ (+ base (* 8 ws)))
    (ppc-emit-load-word buf +ppc-r0+ +ppc-r1+ (ppc-lr-slot))
    (ppc-emit-mtlr buf +ppc-r0+)
    (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ (ppc-frame-size))))

(defun %nppc-emit-late-call (buf slot undefined)
  "CALL through cell SLOT, as FUNCALL dispatches: the object in the cell; NIL ->
   UNDEFINED-FUNCTION (absolute UNDEFINED, nargs 0); a closure (tag 9, subtag
   +SUBTAG-CLOSURE+) -> its environment into the CENV slot and its code; else
   the function's own code.  r11/r12/r0 only: the arguments are untouched."
  (let ((code (mvm-make-label)) (nil-l (mvm-make-label)) (done (mvm-make-label)))
    (%nppc-emit-load-slot buf +ppc-r11+ slot)
    (ppc-emit-cmpw buf +ppc-r11+ +ppc-r21+)
    (ppc-emit-beq buf nil-l)
    (ppc-emit-andi-dot buf +ppc-r12+ +ppc-r11+ 15)
    (ppc-emit-cmpwi buf +ppc-r12+ +tag-object+)
    (ppc-emit-bne buf code)
    (ppc-emit-lbz buf +ppc-r12+ +ppc-r11+ (- 3 +tag-object+))   ; the header's low byte
    (ppc-emit-cmpwi buf +ppc-r12+ +subtag-closure+)
    (ppc-emit-bne buf code)
    ;; env in r0 as DATA, the address in r12: r0 is never a base (rA=0 reads 0)
    (ppc-emit-lwz buf +ppc-r0+ +ppc-r11+ (- 8 +tag-object+))    ; slot 1: env
    (ppc-emit-li buf +ppc-r12+ (ppc-cenv-addr))
    (ppc-emit-stw buf +ppc-r0+ +ppc-r12+ 0)
    (ppc-emit-lwz buf +ppc-r11+ +ppc-r11+ (- 4 +tag-object+))   ; slot 0: fn
    (ppc-emit-label buf code)
    (ppc-emit-mtctr buf +ppc-r11+)                               ; BCCTR ignores the tag's low bits
    (ppc-emit-bctrl buf)
    (ppc-emit-b buf done)
    (ppc-emit-label buf nil-l)
    (ppc-translate-insn buf +op-set-nargs+ (list 0) 0 (make-hash-table) nil)
    (%nppc-emit-abs-call buf undefined)
    (ppc-emit-label buf done)))

(defun %nppc-emit-module (buf base bc fns fn-table rt)
  "Emit FNS' code into BUF written at BASE: an alist (NAME . POSITION), or NIL
   (*NPPC-WHY*)."
  (let ((entries (make-hash-table)) (calls (make-hash-table)) (out nil) (bad nil)
        (undefined (%nppc-image-fn-addr "%SIGNAL-UNDEFINED-FUNCTION"))
        (label-map (make-hash-table)))
    (unless undefined (setq *nppc-why* :no-undefined-function-signaller bad t))
    (dolist (f fns) (setf (gethash (second f) entries) (mvm-make-label)))
    ;; every call operand, resolved once
    (dolist (f fns)
      (dolist (o (%nppc-call-operands bc (second f) (+ (second f) (third f))))
        (unless (or bad (nth-value 1 (gethash o calls)))
          (let ((r (%nppc-resolve o rt fn-table entries)))
            (if r (setf (gethash o calls) r) (setq bad t))))))
    (unless bad
      (dolist (f fns)
        (%nppc-scan-branch-targets bc (second f) (+ (second f) (third f)) label-map))
      (dolist (f fns)
        (let ((off (second f)) (len (third f)))
          ;; 16-aligned at the RUNTIME address: :fn-addr ORs in tag 3
          (loop until (zerop (mod (+ base (ppc-buffer-position buf)) 16))
                do (ppc-emit-nop buf))
          (push (cons (first f) (ppc-buffer-position buf)) out)
          (setq *ppc-saved-locals* (ppc-fn-local-regs bc off (+ off len)))
          (ppc-emit-label buf (gethash off entries))
          (let ((pc off) (limit (+ off len)))
            (loop while (< pc limit)
                  do (let ((l (gethash pc label-map)))
                       (when l (ppc-emit-label buf l))
                       (let* ((d (decode-instruction bc pc))
                              (op (car d)) (operands (cadr d)) (npc (cddr d))
                              (c (and (or (= op +op-call+) (= op +op-tailcall+) (= op +op-fn-addr+))
                                      (gethash (if (= op +op-fn-addr+) (second operands) (first operands))
                                               calls))))
                         (setq *nppc-at* (list pc op operands))
                         (let ((*ppc-cur-mvm-pc* pc))
                           (cond
                             ((= op +op-li-const+)
                              (let* ((vd (first operands)) (pd (ppc-vreg-phys vd))
                                     (dst (or pd +ppc-scratch1+)))
                                (%nppc-emit-load-slot buf dst (%nppc-const-slot (second operands)))
                                (unless pd (ppc-store-vreg buf vd dst))))
                             ((null c)
                              (ppc-translate-insn buf op operands npc label-map nil))
                             ;; CALL
                             ((= op +op-call+)
                              (case (car c)
                                (:local (ppc-emit-bl buf (cdr c)))
                                (:abs (%nppc-emit-abs-call buf (cdr c)))
                                (t (%nppc-emit-late-call buf (cdr c) undefined))))
                             ;; TAILCALL
                             ((= op +op-tailcall+)
                              (case (car c)
                                (:local (%nppc-emit-tail-frame-pop buf) (ppc-emit-b buf (cdr c)))
                                (:abs (%nppc-emit-tail-frame-pop buf)
                                 (%nppc-emit-abs-call buf (cdr c) t))
                                ;; no tail call through a cell: a call, then the epilogue
                                (t (%nppc-emit-late-call buf (cdr c) undefined)
                                   (ppc-emit-epilogue buf))))
                             ;; FN-ADDR
                             (t
                              (let* ((vd (first operands)) (pd (ppc-vreg-phys vd))
                                     (dst (or pd +ppc-scratch1+)))
                                (case (car c)
                                  (:local
                                   (ppc-emit-word buf #x48000005)          ; bl .+4
                                   (ppc-emit-mflr buf dst)
                                   (ppc-emit-addis buf dst dst 0)
                                   (ppc-emit-fixup buf (cdr c) :pcrel-ha)
                                   (ppc-emit-addi buf dst dst 0)
                                   (ppc-emit-fixup buf (cdr c) :pcrel-lo)
                                   (ppc-emit-ori buf dst dst +tag-function+))
                                  (:abs (ppc-emit-li buf dst (+ (cdr c) +tag-function+)))
                                  (t (%nppc-emit-load-slot buf dst (cdr c))))
                                (unless pd (ppc-store-vreg buf vd dst))))))
                         (setq pc npc))))))))
    (if bad nil (nreverse out))))

(defun %nppc-translate-1 (m with-thunk long)
  (setq *nppc-why* nil)
  (let* ((bc (first m)) (ft-list (third m)) (fn-table (fourth m)) (rt (fifth m))
         (fns (sort (if with-thunk
                        (copy-list ft-list)
                        (remove "%MVM-EVAL-THUNK" (copy-list ft-list) :key (function car)
                                :test (function string=)))
                    (function <) :key (function second)))
         (base (logand (+ *nppc-ptr* 15) -16))
         (x *nppc-exports*)
         ;; EVERY slot given: a baked constructor's omitted slots come out NIL
         ;; rather than their DEFSTRUCT initforms (POSITION NIL: the first word
         ;; emitted signalled "NIL is not of type REAL")
         (buf (make-ppc-buffer :words (make-array 4096) :labels (make-hash-table)
                               :fixups nil :position 0 :word-count 0 :pools nil)))
    (flet ((ex (i) (let ((a (aref x i))) (and a (%nppc-abs-label buf base a)))))
      (let* ((*ppc-64-bit* nil)
             (*ppc-saved-locals* nil)
             (*ppc-gc-label* (ex 0))
             (*ppc-hstack-push-slow-label* (ex 1))
             (*ppc-hstack-pop-ext-label* (ex 2))
             (*ppc-li-const-patches* nil)
             (*ppc-unimpl-opcodes* nil)
             (*ppc-long-branch-pcs* long)
             (*ppc-branch14-sites* (make-hash-table))
             (*ppc-branch14-overflow* nil)
             (entries (%nppc-emit-module buf base bc fns fn-table rt)))
        (when (and entries *ppc-unimpl-opcodes*)
          (setq *nppc-why* (list :unimplemented *ppc-unimpl-opcodes*) entries nil))
        (when entries
          (ppc-fixup-labels buf)
          (when *ppc-branch14-overflow*
            ;; a conditional branch out of reach: again, those emitted long
            (dolist (p *ppc-branch14-overflow*) (setf (gethash p long) t))
            (return-from %nppc-translate-1 :again))
          (let* ((n (ppc-buffer-word-count buf)) (size (* 4 n))
                 (words (ppc-buffer-words buf)))
            (if (> (+ base size) *nppc-end*)
                (setq *nppc-why* :area-full entries nil)
                (progn
                  (dotimes (i n)
                    (let ((w (aref words i)) (a (+ base (* 4 i))))
                      (setf (mem-ref a :u16) (ash w -16))
                      (setf (mem-ref (+ a 2) :u16) (logand w #xFFFF))))
                  (%jit-icache-flush base size)
                  (setq *nppc-ptr* (+ base size))))))
        (mapcar (lambda (e) (cons (car e) (%nppc-object (+ base (cdr e) 3)))) entries)))))

(defun %nppc-translate (m &optional with-thunk)
  "Module M (*JIT-MODULE-REGISTRY*'s (bc entry ft-list fn-table rt-table ...))
   as native PowerPC in the code area: an alist (NAME . FUNCTION-OBJECT), or NIL
   (*NPPC-WHY*).  Nothing is written unless everything resolves."
  (%nppc-init)
  (let ((long (make-hash-table)))
    (loop repeat 4
          do (let ((r (%nppc-translate-1 m with-thunk long)))
               (unless (eq r :again) (return r))))))

;;; ---------------------------------------------------------------------------
;;; Publishing, and the seams (jit-i386-native.lisp's, with its names)

(defun %runtime-has-fpd-p ()
  "The compiler's: may runtime-compiled code use the unboxed float ops?  Yes:
   translate-ppc's FP-register class (PPC-TRANSLATE-FP, F0..F11 = f14..f25).
   The tier runs only on a classic-FPU ppc32 (hosted, the Mac)."
  t)

(defun %nppc-install (nm fn)
  "Publish FN as runtime DEFUN NM: by name and by name hash; a generic
   function's dispatcher is registered for the new object."
  (when (boundp (quote *symbol-function-table*))
    (let* ((old (gethash nm *symbol-function-table*))
           (gf (and old (boundp (quote *gf-fn-to-name*)) (assoc old *gf-fn-to-name*))))
      (when gf (%register-gf-fn fn (cdr gf))))
    (puthash nm *symbol-function-table* fn))
  (when (boundp (quote *native-sym-function-table*))
    (puthash (compute-name-hash nm) *native-sym-function-table* fn)))

(defun %nppc-done-p (bc)
  (and (member bc *nppc-done* :test (function eq)) t))

(defun %nppc-on-defun (names)
  "Translate the module mvm-eval just installed for NAMES and publish its
   DEFUNs; on any failure they stay interpreted.  T when translated.  A success
   retries the modules that were waiting for a callee, until none resolves."
  (when (%nppc-on-defun-1 names)
    (loop
      (let ((waiting *nppc-pending*) (progress nil))
        (setq *nppc-pending* nil)
        (dolist (w waiting)
          (when (%nppc-on-defun-1 w) (setq progress t)))
        (unless progress (return))))
    t))

(defun %nppc-on-defun-1 (names)
  (let ((m (and names *jit-module-registry* (gethash (car names) *jit-module-registry*))))
    (when (and m (not (%nppc-done-p (first m))))
      (let ((fns (handler-case (%nppc-translate m)
                   (error (c) (setq *nppc-why* (list :error (princ-to-string c) :at *nppc-at*))
                     nil))))
        (if fns
            (progn
              (setq *nppc-translated* (+ *nppc-translated* 1)
                    *nppc-done* (cons (first m) *nppc-done*))
              (dolist (f fns)
                (when (member (car f) names :test (function string=))
                  (%nppc-install (car f) (cdr f))))
              (%nppc-refresh names)
              t)
            (progn
              (setq *nppc-refused* (cons (cons (car names) *nppc-why*) *nppc-refused*))
              (if (and (consp *nppc-why*) (member (car *nppc-why*) '(:not-native :no-callee)))
                  (setq *nppc-pending* (cons names *nppc-pending*))
                  (setq *nppc-done* (cons (first m) *nppc-done*)))
              nil))))))

(defun %nppc-ready ()
  "Is the native tier ready?  Readies it if not; one that cannot start turns
   itself off (*NPPC-OFF*, *NPPC-WHY*)."
  (unless (and (boundp (quote *nppc-ready*)) *nppc-ready*)
    (handler-case (%nppc-init)
      (error (c) (setq *nppc-off* t *nppc-why* (list :init (princ-to-string c))))))
  (and (boundp (quote *nppc-ready*)) *nppc-ready* t))

(defun %nppc-first-call-stub (nm names orig)
  "What NM is until it is first called: a stub that translates NM's module (all
   of NAMES), then forwards -- to NM's native code, or to ORIG, the interpreter
   trampoline, put back when the module will not translate."
  (let ((box (list nil)) (self (list nil)))
    (setf (car self)
          (lambda (&rest args)
            (unless (car box)
              (%nppc-on-defun names)
              (let ((now (%nppc-current-fn nm)))
                (if (or (null now) (eq now (car self)))
                    (progn (%nppc-install nm orig) (%nppc-refresh (list nm))
                           (setf (car box) orig))
                    (setf (car box) now))))
            (apply (car box) args)))
    (car self)))

(defun %runtime-defun-hook (names)
  "mvm-eval's hook (overridden here, last DEFUN wins): a runtime DEFUN is
   translated at its FIRST CALL; each name gets %NPPC-FIRST-CALL-STUB in place
   of its trampoline.  (compile 'f) translates at once (%JIT-EAGER-ALL)."
  (when (and names (not (and (boundp (quote *nppc-off*)) *nppc-off*))
             (%nppc-ready))
    (let ((m (and *jit-module-registry* (gethash (car names) *jit-module-registry*))))
      (when (and m (not (%nppc-done-p (first m))))
        (dolist (nm names)
          (let ((orig (%nppc-current-fn nm)))
            (when orig
              (%nppc-install nm (%nppc-first-call-stub nm names orig)))))))
    (%nppc-refresh names)))

(defun %runtime-form-native (bc entry ft-list fn-table rt-table lam-offsets)
  "mvm-eval's form seam (overridden here): a top-level form whose module builds
   closures is translated with its thunk; its thunk, or NIL to interpret it."
  (when (and (not (and (boundp (quote *nppc-off*)) *nppc-off*)) (%nppc-ready)
             (not (%nppc-done-p bc)))
    (or (gethash bc *nppc-forms*)
        (let* ((m (list bc entry ft-list fn-table rt-table lam-offsets nil nil))
               (fns (handler-case (%nppc-translate m t)
                      (error (c) (setq *nppc-why* (list :error (princ-to-string c) :at *nppc-at*))
                        nil)))
               (th (cdr (assoc "%MVM-EVAL-THUNK" fns :test (function string=)))))
          (if th
              (progn (setq *nppc-forms-translated* (+ *nppc-forms-translated* 1))
                     (setf (gethash bc *nppc-forms*) th))
              (progn (setq *nppc-refused* (cons (cons :form *nppc-why*) *nppc-refused*)
                           *nppc-done* (cons bc *nppc-done*))
                     nil))))))

(defun %jit-eager-all (&optional name)
  "COMPILE's seam: (compile 'F) translates F's module now, or with no NAME every
   module not yet tried.  The number translated."
  (let ((n 0))
    (when (and (not (and (boundp (quote *nppc-off*)) *nppc-off*)) (%nppc-ready)
               *jit-module-registry*)
      (if name
          (let* ((key (string name)) (m (gethash key *jit-module-registry*)))
            (when (and m (not (%nppc-done-p (first m))))
              (let ((names nil))
                (maphash (lambda (k v) (when (eq v m) (setq names (cons k names))))
                         *jit-module-registry*)
                (when (%nppc-on-defun (cons key (remove key names :test (function string=))))
                  (setq n 1)))))
          (let ((todo nil))
            (maphash (lambda (k v)
                       (unless (%nppc-done-p (first v))
                         (let ((e (assoc (first v) todo :test (function eq))))
                           (if e (rplacd e (cons k (cdr e)))
                               (setq todo (cons (list (first v) k) todo))))))
                     *jit-module-registry*)
            (dolist (e todo)
              (when (%nppc-on-defun (cdr e)) (setq n (+ n 1)))))))
    n))
