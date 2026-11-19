;;;; jit-i386-native.lisp -- REPL code as NATIVE i386, by the image's own emitter
;;;;
;;;; The 68k native tier's design (mvm/jit-68k-native.lisp, docs/68k-threaded-jit.md)
;;;; for i386: a function defined at run time goes through translate-i386.lisp's
;;;; per-instruction translator (I386-TRANSLATE-INSN) -- the same code that
;;;; translated the image, baked into it (MODUS_I386_JIT=1) -- into a code area,
;;;; instead of staying MVM bytecode for the interpreter (measured: `(fib 24)'
;;;; 7.2 s interpreted, 6.4 ms as image code, under qemu-i386).  Three things
;;;; differ from translating the image, and only three:
;;;;   1. WHERE THINGS ARE.  The buffer's final address (BASE) is known before
;;;;      emission, so anything outside it -- an image function, the collector,
;;;;      the handler-case helpers -- is a label at (ADDRESS - BASE), and the
;;;;      translator's own rel32 / diff32 fixups come out right.  The routines
;;;;      that are not functions are found through the export table the AOT
;;;;      translator writes after %N386-EXPORT-TABLE (I386-EMIT-NATIVE-EXPORTS).
;;;;   2. CALL OPERANDS name a function-table index, a bytecode offset, or a
;;;;      runtime name (+N386-RT-BASE+ + n): resolved here to a label of this
;;;;      module or an image / earlier runtime function's address.  A callee
;;;;      still interpreted refuses the module (it can be translated later).
;;;;   3. CONSTANTS are heap objects the collector moves, so LI-CONST loads its
;;;;      object from the vector the root word +I386-NATIVE-CONST-ROOT+ holds,
;;;;      which every collection forwards.
;;;; The emitter's configuration is the image's own, carried in by %N386-COINIT,
;;;; whose body the build writes (translate-i386 I386-NATIVE-COINIT-TEXT).
;;;;
;;;; A DEFUN is translated when it is defined (%RUNTIME-DEFUN-HOOK), and
;;;; (compile 'f) translates F's module (%JIT-EAGER-ALL).  Anything this cannot
;;;; do leaves the module interpreted, with the reason in *N386-REFUSED*.

(in-package :modus.mvm)

(defconstant +n386-rt-base+ #x40000000)        ; interp.lisp +mvm-runtime-call-base+
(defconstant +n386-area-addr+ #x12000000
  "The code area: hosted i386 leaves [#x10020000, #x17800000) free between
   the low block and the stack; an address here is a fixnum on the 30-bit tower.")
(defconstant +n386-area-size+ #x800000)

(defvar *n386-off* nil "T: leave runtime DEFUNs interpreted.")
(defvar *n386-ready* nil)
(defvar *n386-exports* nil "Image routine addresses, I386-NATIVE-EXPORT-LABELS' order.")
(defvar *n386-why* nil "Why the last module was not translated natively.")
(defvar *n386-at* nil "The instruction being translated: (PC OPCODE OPERANDS).")
(defvar *n386-ptr* 0 "The code area's next free byte.")
(defvar *n386-end* 0 "The code area's end.")
(defvar *n386-consts* nil "The constant vector runtime code reads (rooted at +I386-NATIVE-CONST-ROOT+).")
(defvar *n386-const-slots* nil "Pool index -> slot of *N386-CONSTS*.")
(defvar *n386-const-fill* 0)
(defvar *n386-translated* 0 "CENSUS: modules translated natively.")
(defvar *n386-refused* nil "CENSUS: (name . why), newest first.")
(defvar *n386-done* nil "Modules (their bytecode) translated or refused for good.")
(defvar *n386-pending* nil
  "Name lists of modules refused only because a callee was still interpreted
   (:NOT-NATIVE / :NO-CALLEE): retried after each module that does translate.")

(defun %n386-coinit ()
  "The emitter's configuration as this image was translated with it."
  '(:n386-coinit :pending))

;;; ---------------------------------------------------------------------------
;;; The image's facts

(defun %n386-object (w)
  "The Lisp object whose machine word is W (odd: a tagged pointer).  %WORD->VAL
   would shift the tag away; the raw add of word 1 keeps it."
  (%fixnum-+ (ash (- w 1) -1) (%word->val 1)))

(defun %n386-u32 (a)
  "The 32-bit word at A as an exact integer (two 16-bit loads: a :U32 load of
   a word >= 2^30 is not exact on the 30-bit tower)."
  (+ (mem-ref a :u16) (* 65536 (mem-ref (+ a 2) :u16))))

(defun %n386-magic-at-p (a)
  ;; "modusJXL", little-endian halves
  (and (= (mem-ref a :u16) #x6F6D) (= (mem-ref (+ a 2) :u16) #x7564)
       (= (mem-ref (+ a 4) :u16) #x4A73) (= (mem-ref (+ a 6) :u16) #x4C58)))

(defun %n386-export-table ()
  "A marker: the AOT translator writes the export table right after this
   function's code (translate-i386, I386-EMIT-NATIVE-EXPORTS)."
  nil)

(defun %n386-find-exports ()
  "The export table: a few words past %N386-EXPORT-TABLE's code."
  (let ((a (logand (- (%val->word (function %n386-export-table)) 3) -4)) (found nil))
    (loop repeat 1024
          until found
          do (if (and (%n386-magic-at-p a) (= (%n386-u32 (+ a 8)) 3))
                 (setq found a)
                 (setq a (+ a 4))))
    (unless found (error "jit-i386-native: no export table in the image"))
    (let* ((k (%n386-u32 (+ found 8))) (v (make-array k :initial-element nil)))
      (dotimes (i k)
        (let ((off (%n386-u32 (+ found 12 (* 4 i)))))
          (unless (= off #x7FFFFFFF)
            (setf (aref v i) (+ found (if (>= off #x80000000) (- off #x100000000) off))))))
      (setq *n386-exports* v))))

(defun %n386-area ()
  "The code area: anonymous RWX memory at +N386-AREA-ADDR+ exactly
   (MAP_FIXED_NOREPLACE).  old_mmap (90), whose one argument is a block of six
   words, because mmap2 takes its sixth in EBP, the frame pointer."
  (let ((blk *cstr-scratch*))
    (setf (mem-ref blk :u32) +n386-area-addr+)
    (setf (mem-ref (+ blk 4) :u32) +n386-area-size+)
    (setf (mem-ref (+ blk 8) :u32) 7)                ; PROT_READ|WRITE|EXEC
    (setf (mem-ref (+ blk 12) :u32) #x100022)        ; FIXED_NOREPLACE|PRIVATE|ANONYMOUS
    (setf (mem-ref (+ blk 16) :u16) #xFFFF)          ; fd = -1
    (setf (mem-ref (+ blk 18) :u16) #xFFFF)
    (setf (mem-ref (+ blk 20) :u32) 0)
    (let ((a (syscall3 90 blk 0 0)))
      (unless (eql a +n386-area-addr+)
        (error "jit-i386-native: mmap at #x~X failed: ~S" +n386-area-addr+ a))
      (setq *n386-ptr* a *n386-end* (+ a +n386-area-size+)))))

(defun %n386-root-set (v)
  "Point the collector's root word at constant vector V (its raw word)."
  (setf (mem-ref modus.mvm.i386::+i386-native-const-root+ :u64) v))

(defun %n386-const-slot (idx)
  "The slot of *N386-CONSTS* holding pool constant IDX (filled on first use)."
  (or (gethash idx *n386-const-slots*)
      (let ((n *n386-const-fill*))
        (when (>= n (length *n386-consts*))
          (let ((new (make-array (* 2 (length *n386-consts*)) :initial-element nil)))
            (dotimes (i n) (setf (svref new i) (svref *n386-consts* i)))
            (setq *n386-consts* new)
            (%n386-root-set new)))
        (setf (svref *n386-consts* n) (%e2-const-ref idx))
        (setq *n386-const-fill* (+ n 1))
        (setf (gethash idx *n386-const-slots*) n))))

(defun %n386-init ()
  (unless *n386-ready*
    (%n386-coinit)
    ;; the emitter's labels come from MVM-MAKE-LABEL's counter, a DEFVAR whose
    ;; initial value never ran here
    (unless (integerp *mvm-label-counter*) (setq *mvm-label-counter* 0))
    (%n386-find-exports)
    (%n386-area)
    (setq *n386-consts* (make-array 256 :initial-element nil)
          *n386-const-slots* (make-hash-table) *n386-const-fill* 0)
    (%n386-root-set *n386-consts*)
    (setq *n386-translated* 0 *n386-refused* nil *n386-done* nil *n386-pending* nil
          *n386-ready* t))
  t)

;;; ---------------------------------------------------------------------------
;;; Translation

(defun %n386-abs-label (buf base addr)
  "A label at absolute ADDR, for a buffer that will be written at BASE."
  (let ((l (mvm-make-label)))
    (setf (gethash l (modus.mvm.i386::i386-buffer-labels buf)) (- addr base))
    l))

(defun %n386-image-fn-addr (name)
  "The entry address of image (or native runtime) function NAME, or NIL."
  (let* ((fn (%mvm-resolve-runtime-fn name))
         (w (and fn (%val->word fn))))
    (and w (= (logand w 15) 3) (- w 3))))

(defun %n386-call-operands (bc start end)
  "Every CALL / TAILCALL / FN-ADDR operand in [START, END) of BC."
  (let ((pc start) (out nil))
    (loop while (< pc end)
          do (let* ((d (decode-instruction bc pc)) (op (car d)) (operands (cadr d)))
               (cond ((or (= op +op-call+) (= op +op-tailcall+)) (push (first operands) out))
                     ((= op +op-fn-addr+)
                      (unless (eql (second operands) #xFFFFFFF0) (push (second operands) out))))
               (setq pc (cddr d))))
    out))

(defun %n386-resolve (buf base operand rt fn-table entries)
  "The label call operand OPERAND names: an entry of this module, or a label at
   an image / native runtime function's address.  NIL (*N386-WHY*) otherwise."
  (if (>= operand +n386-rt-base+)
      (let* ((name (gethash operand rt))
             (a (and name (%n386-image-fn-addr name))))
        (if a
            (%n386-abs-label buf base a)
            (progn (setq *n386-why* (list :not-native name)) nil)))
      (let ((pc (if (< operand (length fn-table)) (aref fn-table operand) operand)))
        (or (gethash pc entries)
            (progn (setq *n386-why* (list :no-callee operand)) nil)))))

(defun %n386-emit-li-const (buf vd idx)
  "(LI-CONST Vd idx): the object from the constant vector's slot, through the
   root word, so a collection that moves either is seen."
  (let* ((slot (%n386-const-slot idx))
         (pd (modus.mvm.i386::i386-vreg-phys vd))
         (dst (or pd modus.mvm.i386::+i386-eax+)))
    (modus.mvm.i386::i386-emit-mov-reg-abs buf dst modus.mvm.i386::+i386-native-const-root+)
    (modus.mvm.i386::i386-emit-mov-reg-mem buf dst dst (- (* 4 (+ slot 1)) +tag-object+))
    (unless pd (modus.mvm.i386::i386-store-vreg buf vd dst))))

(defun %n386-emit-module (buf base bc fns fn-table rt)
  "Emit FNS' code into BUF written at BASE: an alist (NAME . POSITION), or NIL
   (*N386-WHY*)."
  (let ((entries (make-hash-table)) (calls (make-hash-table)) (out nil) (bad nil))
    (dolist (f fns) (setf (gethash (second f) entries) (mvm-make-label)))
    ;; every call operand's label, once
    (dolist (f fns)
      (dolist (o (%n386-call-operands bc (second f) (+ (second f) (third f))))
        (unless (or bad (nth-value 1 (gethash o calls)))
          (let ((l (%n386-resolve buf base o rt fn-table entries)))
            (if l (setf (gethash o calls) l) (setq bad t))))))
    (unless bad
      (dolist (f fns)
        (let* ((off (second f)) (len (third f))
               (state (modus.mvm.i386::make-i386-translate-state
                       :buf buf :mvm-bytes bc :mvm-length len :mvm-offset off
                       :gc-label modus.mvm.i386::*i386-gc-collect-label*
                       :function-table calls)))
          ;; 16-aligned at the RUNTIME address: :fn-addr ORs in tag 3
          (loop until (zerop (mod (+ base (modus.mvm.i386::i386-current-pos buf)) 16))
                do (modus.mvm.i386::i386-emit-nop buf))
          (push (cons (first f) (modus.mvm.i386::i386-current-pos buf)) out)
          (modus.mvm.i386::i386-emit-label buf (gethash off entries))
          (modus.mvm.i386::i386-emit-prologue
           buf (if (zerop len) :legacy (modus.mvm.i386::i386-function-frame-slots bc off)))
          (modus.mvm.i386::i386-scan-branch-targets state)
          (let ((pc off) (limit (+ off len)))
            (loop while (< pc limit)
                  do (let ((l (gethash pc (modus.mvm.i386::i386-translate-state-label-map state))))
                       (when l (modus.mvm.i386::i386-emit-label buf l))
                       (let* ((d (decode-instruction bc pc))
                              (op (car d)) (operands (cadr d)) (npc (cddr d)))
                         (setq *n386-at* (list pc op operands))
                         (if (= op +op-li-const+)
                             (%n386-emit-li-const buf (first operands) (second operands))
                             (modus.mvm.i386::i386-translate-insn state op operands npc))
                         (setq pc npc))))))))
    (if bad nil (nreverse out))))

(defun %n386-translate (m)
  "Module M (*JIT-MODULE-REGISTRY*'s (bc entry ft-list fn-table rt-table ...))
   as native i386 in the code area: an alist (NAME . FUNCTION-OBJECT), or NIL
   (*N386-WHY*).  Nothing is written unless everything resolves."
  (%n386-init)
  (setq *n386-why* nil)
  (let* ((bc (first m)) (ft-list (third m)) (fn-table (fourth m)) (rt (fifth m))
         (fns (sort (remove "%MVM-EVAL-THUNK" (copy-list ft-list) :key (function car)
                            :test (function string=))
                    (function <) :key (function second)))
         (base (logand (+ *n386-ptr* 15) -16))
         (x *n386-exports*)
         (buf (modus.mvm.i386::make-i386-buffer :bytes (make-array 4096)
                                                :labels (make-hash-table))))
    (flet ((ex (i) (let ((a (aref x i))) (and a (%n386-abs-label buf base a))))
           (fnl (name) (let ((a (%n386-image-fn-addr name))) (and a (%n386-abs-label buf base a)))))
      (let* ((modus.mvm.i386::*i386-native-code-offset* base)
             (modus.mvm.i386::*i386-gc-collect-label* (ex 0))
             (modus.mvm.i386::*i386-handler-push-label* (ex 1))
             (modus.mvm.i386::*i386-handler-pop-label* (ex 2))
             (modus.mvm.i386::*i386-genadd-label* (fnl "GENERIC-ADD"))
             (modus.mvm.i386::*i386-gensub-label* (fnl "GENERIC-SUBTRACT"))
             (modus.mvm.i386::*i386-genmul-label* (fnl "GENERIC-MULTIPLY"))
             (modus.mvm.i386::*i386-li-const-patches* nil)
             (modus.mvm.i386::*i386-unimpl-ops* nil)
             (modus.mvm.i386::*i386-eax-violations* nil)
             (modus.mvm.i386::*i386-check-eax-invariant* nil)
             (modus.mvm.i386::*i386-eax-invariant-fatal* nil)
             (modus.mvm.i386::*i386-cur-opname* nil)
             (modus.mvm.i386::*i386-cur-dest-is-vr* nil)
             (entries (%n386-emit-module buf base bc fns fn-table rt)))
        (when (and entries modus.mvm.i386::*i386-unimpl-ops*)
          (setq *n386-why* (list :unimplemented modus.mvm.i386::*i386-unimpl-ops*) entries nil))
        (when entries
          (modus.mvm.i386::i386-fixup-labels buf)
          (let ((size (modus.mvm.i386::i386-current-pos buf))
                (bytes (modus.mvm.i386::i386-buffer-bytes buf)))
            (if (> (+ base size) *n386-end*)
                (setq *n386-why* :area-full entries nil)
                (progn
                  (dotimes (i size) (setf (mem-ref (+ base i) :u8) (aref bytes i)))
                  (setq *n386-ptr* (+ base size))))))
        (mapcar (lambda (e) (cons (car e) (%n386-object (+ base (cdr e) 3)))) entries)))))

;;; ---------------------------------------------------------------------------
;;; Publishing, and the seams

(defun %runtime-has-fpd-p ()
  "The compiler's (compiler.lisp): may runtime-compiled code use the unboxed
   double ops (fpd-*)?  Not here: translate-i386 has none, so a module using
   them would be refused and stay interpreted.  Runtime float code boxes, as
   the image's own float code does."
  nil)

(defun %n386-install (nm fn)
  "Publish FN as runtime DEFUN NM, as the 68k JIT's %T68-INSTALL does: by name
   (later translations link to it) and by name hash (SYMBOL-FUNCTION, FUNCALL);
   a generic function's dispatcher is registered for the new object."
  (when (boundp (quote *symbol-function-table*))
    (let* ((old (gethash nm *symbol-function-table*))
           (gf (and old (boundp (quote *gf-fn-to-name*)) (assoc old *gf-fn-to-name*))))
      (when gf (%register-gf-fn fn (cdr gf))))
    (puthash nm *symbol-function-table* fn))
  (when (boundp (quote *native-sym-function-table*))
    (puthash (compute-name-hash nm) *native-sym-function-table* fn)))

(defun %n386-done-p (bc)
  (and (member bc *n386-done* :test (function eq)) t))

(defun %n386-on-defun (names)
  "Translate the module mvm-eval just installed for NAMES and publish its
   DEFUNs; on any failure they stay interpreted.  T when translated.  A
   success retries the modules that were waiting for a callee (a file's
   function calling one defined further down), until none resolves."
  (when (%n386-on-defun-1 names)
    (loop
      (let ((waiting *n386-pending*) (progress nil))
        (setq *n386-pending* nil)
        (dolist (w waiting)
          (when (%n386-on-defun-1 w) (setq progress t)))
        (unless progress (return))))
    t))

(defun %n386-on-defun-1 (names)
  (let ((m (and names *jit-module-registry* (gethash (car names) *jit-module-registry*))))
    (when (and m (not (%n386-done-p (first m))))
      (let ((fns (handler-case (%n386-translate m)
                   (error (c) (setq *n386-why* (list :error (princ-to-string c) :at *n386-at*))
                     nil))))
        (if fns
            (progn
              (setq *n386-translated* (+ *n386-translated* 1)
                    *n386-done* (cons (first m) *n386-done*))
              (dolist (f fns)
                (when (member (car f) names :test (function string=))
                  (%n386-install (car f) (cdr f))))
              t)
            (progn
              (setq *n386-refused* (cons (cons (car names) *n386-why*) *n386-refused*))
              ;; a callee not translated yet can be later; nothing else changes
              (if (and (consp *n386-why*) (member (car *n386-why*) '(:not-native :no-callee)))
                  (setq *n386-pending* (cons names *n386-pending*))
                  (setq *n386-done* (cons (first m) *n386-done*)))
              nil))))))

(defun %n386-ready ()
  "Is the native tier ready?  Readies it if not; one that cannot start turns
   itself off (*N386-OFF*, *N386-WHY*)."
  (unless (and (boundp (quote *n386-ready*)) *n386-ready*)
    (handler-case (%n386-init)
      (error (c) (setq *n386-off* t *n386-why* (list :init (princ-to-string c))))))
  (and (boundp (quote *n386-ready*)) *n386-ready* t))

(defun %runtime-defun-hook (names)
  "mvm-eval's hook (overridden here, last DEFUN wins): every runtime DEFUN is
   translated as it is defined, unless *N386-OFF*."
  ;; BOUNDP before each read: this runs at every DEFUN from boot on
  (when (and names (not (and (boundp (quote *n386-off*)) *n386-off*))
             (%n386-ready))
    (%n386-on-defun names)))

(defun %jit-eager-all (&optional name)
  "COMPILE's seam: (compile 'F) translates F's module now (if DEFUN did not),
   or with no NAME every module not yet tried.  The number translated."
  (let ((n 0))
    (when (and (not (and (boundp (quote *n386-off*)) *n386-off*)) (%n386-ready)
               *jit-module-registry*)
      (if name
          (let* ((key (string name)) (m (gethash key *jit-module-registry*)))
            (when (and m (not (%n386-done-p (first m))))
              (let ((names nil))
                (maphash (lambda (k v) (when (eq v m) (setq names (cons k names))))
                         *jit-module-registry*)
                (when (%n386-on-defun (cons key (remove key names :test (function string=))))
                  (setq n 1)))))
          (let ((todo nil))
            (maphash (lambda (k v)
                       (unless (%n386-done-p (first v))
                         (let ((e (assoc (first v) todo :test (function eq))))
                           (if e (rplacd e (cons k (cdr e)))
                               (setq todo (cons (list (first v) k) todo))))))
                     *jit-module-registry*)
            (dolist (e todo)
              (when (%n386-on-defun (cdr e)) (setq n (+ n 1)))))))
    n))
