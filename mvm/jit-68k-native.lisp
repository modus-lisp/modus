;;;; jit-68k-native.lisp -- REPL code as NATIVE 68k, by the image's own emitter
;;;;
;;;; A function defined at the REPL used to become threaded code
;;;; (jit-68k.lisp): its MVM bytecode decoded back into instructions, each
;;;; classified into a shape key, the key binary-searched among the image's
;;;; ~3,000 shapes for a token, the tokens run by the image's handlers.  That
;;;; encoding exists to keep the IMAGE small (SMALL.md: one byte a token
;;;; against ~10 bytes of native code, for 6,800 functions on a 1 MB Mac); a
;;;; handful of REPL functions gain nothing from it, it costs more than half of
;;;; a DEFUN, and threaded code runs ~14x slower than native.
;;;;
;;;; Here the module goes through translate-68k.lisp's per-instruction emitter
;;;; (M68K-TRANSLATE-INSN) -- the same code that translated the image, baked
;;;; into it -- into the code area jit-68k.lisp already keeps.  Three things
;;;; differ from translating the image, and only three:
;;;;   1. WHERE THINGS ARE.  The buffer's final address (BASE) is known before
;;;;      emission, so anything outside it -- an image function, the collector,
;;;;      a shared CONS sequence -- is a label placed at (ADDRESS - BASE): the
;;;;      emitter's own fixups then produce the right displacement or absolute
;;;;      address.  The image's routines are found through the table the AOT
;;;;      translator writes after the shape table (M68K-EMIT-NATIVE-EXPORTS).
;;;;   2. CALL OPERANDS name a function-table index, a bytecode offset, or a
;;;;      runtime name (+T68-RT-BASE+ + n), resolved exactly as the threaded JIT
;;;;      resolves them (%T68-CALL-TARGET).
;;;;   3. CONSTANTS are heap objects the collector moves, so LI-CONST loads its
;;;;      object from a CELL of the low block (%T68-CELL), which every
;;;;      collection forwards -- the threaded JIT's rule.
;;;; The emitter's configuration (68000 or not, Mac mode, shared sequences,
;;;; the convention slots ...) is the image's own, carried in by %N68-COINIT,
;;;; whose body the build writes (translate-68k M68K-NATIVE-COINIT-TEXT).
;;;;
;;;; Anything this cannot do leaves the module to the threaded JIT, and the
;;;; reason in *N68-REFUSED*.

(in-package :modus.mvm)

(defvar *n68-off* nil "T: leave REPL code to the threaded JIT.  (A DEFVAR's initial value
   never runs in the image, so the switch is the one whose default is NIL.)")
(defvar *n68-ready* nil)
(defvar *n68-exports* nil "Image routine addresses, M68K-NATIVE-EXPORT-LABELS' order (NIL: none).")
(defvar *n68-why* nil "Why the last module was not translated natively.")
(defvar *n68-at* nil "The instruction being translated: (PC OPCODE OPERANDS), for a refusal's report.")
(defvar *n68-translated* 0 "CENSUS: modules translated natively.")
(defvar *n68-refused* nil "CENSUS: (name . why), newest first.")

(defun %n68-no-getenv (name)
  "The baked translator's SB-EXT:POSIX-GETENV (the build rewrites the calls):
   the build's environment does not exist here, so every variable is unset."
  (declare (ignore name))
  nil)

(defun %n68-coinit ()
  "The emitter's configuration as this image was translated with it."
  '(:n68-coinit :pending))

(defun %n68-find-exports ()
  "Read the export table that follows the shape table (\"modusJXL\")."
  (let* ((tab *t68-table*)
         (n (%t68-u32 (+ tab 12)))
         ;; the entries' offsets are big-endian u16s from the magic, in layout
         ;; order, so the last one is the last entry: index (2), length (1), key
         (hi (mem-ref (+ tab 16 (* 2 (- n 1))) :u8))
         (lo (mem-ref (+ tab 17 (* 2 (- n 1))) :u8))
         (e (+ tab (* hi 256) lo))
         (a (logand (+ e 3 (mem-ref (+ e 2) :u8) 3) -4)))
    ;; the magic, a few words on at most (word, then 4-alignment)
    (loop repeat 8
          until (and (= (%t68-u32 a) #x6D6F6475) (= (%t68-u32 (+ a 4)) #x734A584C))
          do (setq a (+ a 4)))
    (unless (and (= (%t68-u32 a) #x6D6F6475) (= (%t68-u32 (+ a 4)) #x734A584C))
      (error "jit-68k-native: no export table after the shape table"))
    (let* ((k (%t68-u32 (+ a 8)))
           (v (make-array k :initial-element nil)))
      (dotimes (i k)
        (let ((hi (mem-ref (+ a 12 (* 4 i)) :u16))
              (lo (mem-ref (+ a 14 (* 4 i)) :u16)))
          (unless (and (= hi #x7FFF) (= lo #xFFFF))
            (setf (aref v i) (+ a (if (>= hi #x8000) (- (* hi 65536) #x100000000) (* hi 65536)) lo)))))
      (setq *n68-exports* v))))

(defun %n68-init ()
  (unless *n68-ready*
    (%n68-coinit)
    ;; the emitter's labels come from MVM-MAKE-LABEL's counter, a DEFVAR whose
    ;; initial value never ran here
    (unless (integerp *mvm-label-counter*) (setq *mvm-label-counter* 0))
    (%n68-find-exports)
    (setq *n68-translated* 0 *n68-refused* nil *n68-ready* t))
  t)

(defun %n68-abs-label (buf base addr)
  "A label at absolute ADDR, for a buffer that will be written at BASE."
  (let ((l (mvm-make-label)))
    (setf (gethash l (m68k-buffer-labels buf)) (- addr base))
    l))

(defun %n68-branch-targets (bc start end label-map)
  "Labels for every branch target in [START, END), as translate-mvm-to-68k-1's
   first pass makes them."
  (let ((pc start))
    (loop while (< pc end)
          do (let* ((d (decode-instruction bc pc))
                    (operands (cadr d)) (new-pc (cddr d))
                    (info (gethash (car d) *opcode-table*)))
               (when info
                 (let ((types (opcode-info-operands info)))
                   (when (member :off32 types)
                     (let ((target (+ new-pc (if (member :reg types) (second operands) (first operands)))))
                       (unless (gethash target label-map)
                         (setf (gethash target label-map) (mvm-make-label)))))))
               (setq pc new-pc)))))

(defun %n68-emit-module (buf base bc fns fn-table rt)
  "Emit FNS' code into BUF; NIL (and *N68-WHY*) when a call cannot be resolved."
  (let ((label-map (make-hash-table)) (entries nil) (stubs (make-hash-table)) (bad nil))
    ;; every function's entry, and the branch targets inside it
    (dolist (f fns)
      (let ((start (second f)))
        (setf (gethash start label-map) (mvm-make-label))
        (%n68-branch-targets bc start (+ start (third f)) label-map)))
    ;; in-module calls go to the callee's entry label: %T68-CALL-TARGET's STUBS
    (dolist (f fns) (setf (gethash (second f) stubs) (gethash (second f) label-map)))
    (let ((*68k-label-map* label-map) (*68k-bytecode* bc))
      (dolist (f fns)
        (unless bad
          (let ((start (second f)) (end (+ (second f) (third f))))
            ;; 16-aligned entries (translate-mvm-to-68k-1): FN-ADDR ORs in tag 3
            (m68k-word-align buf)
            (loop while (/= 0 (mod (m68k-buffer-position buf) 16)) do (m68k-emit-nop buf))
            (push (cons (first f) (m68k-buffer-position buf)) entries)
            (let ((pc start))
              (loop while (and (< pc end) (not bad))
                    do (let* ((d (decode-instruction bc pc))
                              (op (car d)) (operands (cadr d)) (new-pc (cddr d))
                              (l (gethash pc label-map)))
                         (setq *n68-at* (list pc op operands))
                         (when l (m68k-emit-label buf l))
                         (cond
                           ;; a call / tail call / function address: resolve the
                           ;; operand as the threaded JIT does; an in-module
                           ;; target is its entry label, any other an absolute one
                           ((or (= op +op-call+) (= op +op-tailcall+) (= op +op-fn-addr+))
                            (let* ((k (if (= op +op-fn-addr+) 1 0))
                                   (operand (nth k operands))
                                   (tgt (if (and (= op +op-fn-addr+) (= operand #xFFFFFFF0))
                                            :undefined
                                            (%n68-call-label buf base operand rt fn-table stubs))))
                              (if (null tgt)
                                  (setq bad t)
                                  (let ((lm (make-hash-table)))
                                    (unless (eq tgt :undefined) (setf (gethash operand lm) tgt))
                                    (m68k-translate-insn buf op operands new-pc lm nil)))))
                           ;; a constant: its object from the pool index's cell
                           ((= op +op-li-const+)
                            (m68k-emit-word buf #x2039)               ; MOVE.L abs.L,D0
                            (m68k-emit-long buf (%t68-cell (second operands)))
                            (m68k-store-vreg buf (first operands) +68k-d0+))
                           (t (m68k-translate-insn buf op operands new-pc label-map nil)))
                         (setq pc new-pc))))))))
    (if bad nil (nreverse entries))))

(defun %n68-call-label (buf base operand rt fn-table stubs)
  "The label a call operand names: an entry label of this module, or a label
   at the absolute address of an image / earlier runtime function."
  (if (and (not (%t68-small-p operand)) (>= operand #x40000000))   ; jit-68k +T68-RT-BASE+, defined after this file: a
                                                 ; constant folds only once seen
      (let ((a (%t68-call-target operand rt fn-table stubs)))
        (setq *n68-why* *t68-why*)
        (and a (%n68-abs-label buf base a)))
      (let ((pc (if (< operand (length fn-table)) (aref fn-table operand) operand)))
        (or (gethash pc stubs)
            (progn (setq *n68-why* (list :no-callee operand)) nil)))))

(defun %n68-translate (m)
  "Module M (*JIT-MODULE-REGISTRY*'s (bc entry ft-list fn-table rt-table ...))
   as native 68k in the code area: an alist (NAME . FUNCTION-OBJECT), or NIL
   (*N68-WHY*).  Nothing is written unless everything resolves."
  (%n68-init)
  (setq *n68-why* nil)
  (let* ((bc (first m)) (ft-list (third m)) (fn-table (fourth m)) (rt (fifth m))
         (fns (sort (remove "%MVM-EVAL-THUNK" (copy-list ft-list) :key (function car)
                            :test (function string=))
                    (function <) :key (function second)))
         (base (logand (+ *t68-ptr* 15) -16))
         (buf (make-m68k-buffer :words (make-array 1024) :labels (make-hash-table)))
         (x *n68-exports*)
         (entries nil))
    ;; *68K-THREADED* stays the IMAGE's: native code in a threaded image builds
    ;; the image's 8-word handler frames (with the token pointer), as its hot
    ;; native functions do.  No function is cold here (*68K-COLD-FNS* NIL).
    (let ((*68k-native-p* t) (*68k-cold-fns* nil) (*68k-profile* nil)
          (*68k-paged* nil) (*68k-skip-to* nil)
          (*68k-branch-plan* nil) (*68k-branch-ordinal* 0) (*68k-branch-distances* nil)
          (*68k-fn-entry-positions* nil) (*68k-abs-patches* nil) (*68k-li-const-patches* nil)
          (*68k-unimpl-opcodes* nil)
          (*68k-cold-enter-label* nil) (*68k-reload-label* nil) (*68k-stream-table-label* nil)
          (*68k-stream-section-label* nil) (*68k-stream-home-label* nil)
          (*68k-stream-end-label* nil))
      (flet ((ex (i) (let ((a (and (< i (length x)) (aref x i))))
                       (and a (%n68-abs-label buf base a)))))
        (let ((*68k-gc-label* (ex 0)) (*68k-barrier-label* (ex 1)) (*68k-cons-label* (ex 2))
              (*68k-subtag-label* (ex 3)) (*68k-car-label* (ex 4)) (*68k-cdr-label* (ex 5))
              (*68k-array-len-label* (ex 6)) (*68k-alloc-obj-label* (ex 7))
              (*68k-mul32-label* (ex 8)) (*68k-mulu64-label* (ex 9)) (*68k-divs32-label* (ex 10))
              (*68k-mac-console-label* (ex 11)) (*68k-mac-getc-label* (ex 12))
              (*68k-mac-service-label* (ex 13)) (*68k-thread-next-label* (ex 14)))
          (setq entries (%n68-emit-module buf base bc fns fn-table rt))
          (when entries
            (when *68k-unimpl-opcodes*
              (setq *n68-why* (list :unimplemented *68k-unimpl-opcodes*) entries nil)))
          (when entries
            (m68k-fixup-labels buf)
            (let* ((nw (m68k-buffer-word-count buf)) (size (* 2 nw)) (w (m68k-buffer-words buf)))
              (if (> (+ base size) *t68-end*)
                  (setq *n68-why* :area-full entries nil)
                  (progn
                    (dotimes (i nw) (setf (mem-ref (+ base (* 2 i)) :u16) (aref w i)))
                    ;; 68000 absolute addresses: base + the target's position
                    (dolist (p *68k-abs-patches*)
                      (let ((a (+ base (cdr p))) (at (+ base (car p))))
                        (setf (mem-ref at :u16) (logand (ash a -16) #xFFFF))
                        (setf (mem-ref (+ at 2) :u16) (logand a #xFFFF))))
                    (setq *t68-ptr* (+ base size))
                    (%t68-flush-code)))))))
      (mapcar (lambda (e) (cons (car e) (%t68-object (+ base (cdr e) 3)))) entries))))
