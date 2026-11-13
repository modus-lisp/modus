;;;; jit-68k.lisp -- runtime DEFUNs as THREADED code on the 68k
;;;; (docs/68k-threaded-jit.md).
;;;;
;;;; A threaded 68k image runs its own functions as token streams: one token
;;;; per MVM instruction (a compare and its branch are one), naming a native
;;;; handler with the registers baked in, dispatched by NEXT
;;;; (translate-68k.lisp, "THREADED COLD CODE").  A function defined at run
;;;; time is MVM bytecode from the same compiler; here it gets the same last
;;;; step.  Each instruction is classified exactly as m68k-thread-classify
;;;; does and numbered with the IMAGE's shape table, so its token is the
;;;; image's own and runs the image's own handler.
;;;;
;;;; MILESTONE 1: existing shapes only.  A module with an instruction whose
;;;; shape the image does not have, or a call that does not resolve to native
;;;; code, is not translated: %T68-TRANSLATE answers NIL and the module stays
;;;; interpreted.
;;;;
;;;; CONSTANTS.  LI-CONST loads *E2-CONST-POOL*[idx], a heap object the
;;;; collector moves, so the stream cannot hold its address.  It holds the
;;;; address of a CELL instead -- a word of the low block, which every
;;;; collection scans and forwards in place -- and the image's (:LI-CELL R)
;;;; shape (MODUS_68K_JIT=1) loads R from it.  One cell per pool index.
;;;;
;;;; THE CODE AREA is outside the heap (the collector neither moves nor scans
;;;; it): a function is the unpaged cold-function shape at a 16-byte boundary
;;;; -- MOVEA.L A5,A1 / LEA stream(PC),A5 / JMP NEXT -- then its stream, and
;;;; its function object is that address tagged 3, like an image function's.

(in-package :modus-int)

;;; the opcodes the classifier treats specially (mvm/mvm.lisp)
(defconstant +t68-op-trap+ 2)
(defconstant +t68-op-li+ 17)
(defconstant +t68-op-li-const+ 20)
(defconstant +t68-op-cmp+ 48)
(defconstant +t68-op-test+ 49)
(defconstant +t68-op-cmp-imm+ 51)
(defconstant +t68-op-test-imm+ 52)
(defconstant +t68-op-br+ 64)
(defconstant +t68-op-beq+ 65)
(defconstant +t68-op-bgt+ 70)
(defconstant +t68-op-bnull+ 71)
(defconstant +t68-op-bnnull+ 72)
(defconstant +t68-op-call+ 128)
(defconstant +t68-op-ret+ 130)
(defconstant +t68-op-tailcall+ 131)
(defconstant +t68-op-save-ctx+ 144)
(defconstant +t68-op-restore-ctx+ 145)
(defconstant +t68-op-fn-addr+ 167)
(defconstant +t68-op-bvs+ 199)
(defconstant +t68-rt-base+ #x40000000)   ; interp.lisp +mvm-runtime-call-base+

(defvar *t68-shapes* nil "Key string (as the build printed it) -> token index.")
(defvar *t68-next* nil "NEXT's address: where every stub jumps.")
(defvar *t68-ptr* 0 "The code area's next free byte.")
(defvar *t68-end* 0 "The code area's end.")
(defvar *t68-why* nil "Why the last module was not translated.")
(defvar *t68-cells* nil "Pool index -> its cell's address.")
(defvar *t68-cell-ptr* 0 "The next free cell.")
(defvar *t68-cell-end* 0)

(defun %t68-cells-init ()
  "Cells at #x10018000..#x1001C000 of the runtime-data region: free low block
   below the static heap's crossing table (#x1001E000)."
  (setq *t68-cells* (make-hash-table) *t68-cell-ptr* (%conv-addr #x10018000)
        *t68-cell-end* (%conv-addr #x1001C000)))

(defun %t68-cell (idx)
  "The cell holding pool constant IDX (made on first use)."
  (or (gethash idx *t68-cells*)
      (let ((a *t68-cell-ptr*))
        (when (>= a *t68-cell-end*) (setq *t68-why* :cells-full) (return-from %t68-cell nil))
        (let ((w (%val->word (%e2-const-ref idx))))
          ;; the raw word in two halves: it need not be a fixnum on a 30-bit tower
          (setf (mem-ref a :u16) (ldb (byte 16 16) w))
          (setf (mem-ref (+ a 2) :u16) (ldb (byte 16 0) w)))
        (setq *t68-cell-ptr* (+ a 4))
        (setf (gethash idx *t68-cells*) a))))

;;; ---------------------------------------------------------------------------
;;; The image's facts

(defun %t68-load-shapes (file)
  "The build's shape table (MODUS_68K_SHAPES_OUT): lines \"INDEX KEY\"."
  (let ((h (make-hash-table :test (function equal))))
    (with-open-file (in file)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((sp (position #\Space line)))
                 (when sp
                   (setf (gethash (subseq line (+ sp 1)) h)
                         (parse-integer line :end sp))))))
    (setq *t68-shapes* h)))

(defun %t68-find-next ()
  "NEXT, read out of an image function's stub: MOVEA.L A5,A1 (224D), LEA
   d16(PC),A5 (4BFA d16), JMP abs.L (4EF9 addr) -- the 68000's long BRA."
  (let ((a (- (%val->word (function car)) 3)))
    (unless (and (= (mem-ref a :u16) #x224D) (= (mem-ref (+ a 2) :u16) #x4BFA)
                 (= (mem-ref (+ a 6) :u16) #x4EF9))
      (error "jit-68k: CAR's entry is not a threaded stub"))
    (setq *t68-next* (+ (* (mem-ref (+ a 8) :u16) 65536) (mem-ref (+ a 10) :u16)))))

(defun %t68-area (size &optional (hint #x08000000))
  "SIZE bytes of code area: anonymous RWX memory (mmap2) on hosted Linux, at
   HINT exactly (MAP_FIXED_NOREPLACE): an address must be a fixnum on a 30-bit
   tower, so the kernel's own choice (near 2^30 under qemu-user) will not do."
  (let ((a (syscall6 192 hint size 7 #x100022 -1 0)))
    (unless (eql a hint)
      (error "jit-68k: mmap2 at #x~X failed: ~S" hint a))
    (setq *t68-ptr* a *t68-end* (+ a size))
    a))

(defun %t68-object (w)
  "The Lisp object whose machine word is W (odd: a tagged pointer).  %WORD->VAL
   would shift the tag away; the interpreter's way is a raw add of word 1."
  (%fixnum-+ (ash (- w 1) -1) (%word->val 1)))

;;; ---------------------------------------------------------------------------
;;; Classification (translate-68k.lisp m68k-thread-classify / -walk)

(defun %t68-s32 (x)
  (let ((u (logand x #xFFFFFFFF))) (if (logbitp 31 u) (- u #x100000000) u)))

(defun %t68-imm-class (x)
  (let ((s (%t68-s32 x))) (cond ((<= -128 s 127) 1) ((<= -32768 s 32767) 2) (t 4))))

(defun %t68-cond-br-p (op) (and (>= op +t68-op-beq+) (<= op +t68-op-bgt+)))
(defun %t68-flag-op-p (op)
  (or (= op +t68-op-cmp+) (= op +t68-op-test+) (= op +t68-op-cmp-imm+) (= op +t68-op-test-imm+)))

(defun %t68-classify (op operands new-pc w)
  "(values KEY PARAMS), or :FAIL.  PARAMS: (:i SIZE VALUE) (:br TARGET-PC)
   (:call TARGET) -- TARGET an MVM call operand, resolved at emission."
  (cond
    ((= op +t68-op-li+)
     (let ((n (%t68-imm-class (second operands))))
       (values (list :li (first operands) n) (list (list :i n (second operands))))))
    ((= op +t68-op-li-const+)
     (values (list :li-cell (first operands)) (list (list :cell (second operands)))))
    ((or (= op +t68-op-cmp-imm+) (= op +t68-op-test-imm+))
     (let ((n (%t68-imm-class (second operands))))
       (values (list op (first operands) n) (list (list :i n (second operands))))))
    ((or (= op +t68-op-call+) (= op +t68-op-tailcall+))
     (values (list op) (list (list :call (first operands)))))
    ((= op +t68-op-fn-addr+)
     (if (= (second operands) #xFFFFFFF0)
         (values (list op (first operands) :nil) nil)
         (values (list op (first operands)) (list (list :call (second operands))))))
    ((= op +t68-op-br+)
     (values (list :br w) (list (list :br (+ new-pc (first operands))))))
    ((or (= op +t68-op-bnull+) (= op +t68-op-bnnull+))
     (values (list op (first operands) w) (list (list :br (+ new-pc (second operands))))))
    ((or (%t68-cond-br-p op) (= op +t68-op-bvs+)) (setq *t68-why* :unfused-branch) :fail)
    ((= op +t68-op-ret+) (values (list :ret) nil))
    ((= op +t68-op-trap+)
     (let ((code (first operands)))
       (if (>= code #x8000)
           (values (list :enter code) nil)
           (values (list op code) nil))))
    ((or (= op +t68-op-save-ctx+) (= op +t68-op-restore-ctx+)) (setq *t68-why* :ctx) :fail)
    (t (values (cons op operands) nil))))

(defun %t68-labels (bc)
  "Every branch target in BC (a hash pc -> T): where a fusion must not reach."
  (let ((h (make-hash-table)) (pc 0) (len (length bc)))
    (loop while (< pc len)
          do (let* ((d (decode-instruction bc pc)) (op (car d)) (ops (cadr d)) (npc (cddr d)))
               (cond ((or (= op +t68-op-br+) (%t68-cond-br-p op) (= op +t68-op-bvs+))
                      (setf (gethash (+ npc (first ops)) h) t))
                     ((or (= op +t68-op-bnull+) (= op +t68-op-bnnull+))
                      (setf (gethash (+ npc (second ops)) h) t)))
               (setq pc npc)))
    h))

(defun %t68-walk (bc start end w labels fn)
  "FN (pc key params) for each token of [START, END); :FAIL if one will not thread."
  (let ((pc start))
    (loop while (< pc end)
          do (let* ((d (decode-instruction bc pc)) (op (car d)) (ops (cadr d)) (npc (cddr d))
                    (nxt (and (< npc end) (not (gethash npc labels)) (decode-instruction bc npc))))
               (if (and (%t68-flag-op-p op) nxt (%t68-cond-br-p (car nxt)))
                   (multiple-value-bind (k p) (%t68-classify op ops npc w)
                     (when (eq k :fail) (return-from %t68-walk :fail))
                     (let ((bnpc (cddr nxt)))
                       (funcall fn pc (list :fused k (car nxt) w)
                                (append p (list (list :br (+ bnpc (first (cadr nxt)))))))
                       (setq pc bnpc)))
                   (multiple-value-bind (k p) (%t68-classify op ops npc w)
                     (when (eq k :fail) (return-from %t68-walk :fail))
                     (funcall fn pc k p)
                     (setq pc npc)))))
    t))

;;; ---------------------------------------------------------------------------
;;; Translation

(defun %t68-token (key)
  (let ((i (gethash (let ((*print-base* 10) (*print-radix* nil)) (prin1-to-string key))
                    *t68-shapes*)))
    (or i (progn (setq *t68-why* (list :no-shape key)) nil))))

(defun %t68-param-size (p w)
  (ecase (first p) (:i (second p)) (:br w) (:call 4) (:cell 4)))

(defun %t68-put (addr n v)
  "V's low N bytes, big-endian, at ADDR."
  (let ((u (logand v (- (ash 1 (* 8 n)) 1))))
    (loop for i from 0 below n
          do (setf (mem-ref (+ addr i) :u8) (ldb (byte 8 (* 8 (- n 1 i))) u)))))

(defun %t68-translate (m)
  "Module M (*JIT-MODULE-REGISTRY*'s (bc entry ft-list fn-table rt-table ...))
   as threaded code: an alist (NAME . FUNCTION-OBJECT), or NIL (*T68-WHY*)."
  (let* ((bc (first m)) (ft-list (third m)) (fn-table (fourth m)) (rt (fifth m))
         (labels (%t68-labels bc))
         ;; not the module's eval thunk: it runs once, at the DEFUN, and
         ;; returns the quoted name (an LI-CONST)
         (fns (sort (remove "%MVM-EVAL-THUNK" (copy-list ft-list)
                            :key (function car) :test (function string=))
                    (function <) :key (function second)))
         (plan nil))
    (setq *t68-why* nil)
    ;; pass 1: tokens and sizes, so every label has an address
    (dolist (f fns)
      (let* ((start (second f)) (end (+ start (third f)))
             (w (if (> (third f) 20000) 4 2))
             (toks nil)
             (r (%t68-walk bc start end w labels
                           (lambda (pc k p)
                             (let ((i (%t68-token k)))
                               (unless i (return-from %t68-translate nil))
                               (push (list pc i p) toks))))))
        (when (eq r :fail) (return-from %t68-translate nil))
        (push (list f w (nreverse toks)) plan)))
    (setq plan (nreverse plan))
    ;; layout: each function a 16-aligned stub (12 bytes) then its stream
    (let ((addr *t68-ptr*) (pcaddr (make-hash-table)) (stubs (make-hash-table)))
      (dolist (e plan)
        (setq addr (logand (+ addr 15) -16))
        (setf (gethash (second (first e)) stubs) addr)
        (setq addr (+ addr 12))
        (dolist (tk (third e))
          (setf (gethash (first tk) pcaddr) addr)
          (setq addr (+ addr (if (< (second tk) 255) 1 3)
                        (loop for p in (third tk) sum (%t68-param-size p (second e)))))))
      (when (> addr *t68-end*) (setq *t68-why* :area-full) (return-from %t68-translate nil))
      (flet ((target-addr (operand)
               ;; an MVM call operand: a function-table index, a bytecode
               ;; offset, or a runtime stub (+T68-RT-BASE+ + n) named in RT
               (cond ((>= operand +t68-rt-base+)
                      (let* ((name (gethash operand rt))
                             (fn (and name (%mvm-resolve-runtime-fn name)))
                             (w (and fn (%val->word fn))))
                        (if (and w (= (logand w 15) 3))
                            (- w 3)
                            (progn (setq *t68-why* (list :not-native name))
                                   (return-from %t68-translate nil)))))
                     (t (let ((pc (if (< operand (length fn-table)) (aref fn-table operand) operand)))
                          (or (gethash pc stubs)
                              (progn (setq *t68-why* (list :no-callee operand))
                                     (return-from %t68-translate nil))))))))
        ;; pass 2: write
        (dolist (e plan)
          (let* ((start (second (first e))) (s (gethash start stubs)) (w (second e)))
            (%t68-put s 2 #x224D)                     ; MOVEA.L A5,A1
            (%t68-put (+ s 2) 2 #x4BFA)               ; LEA d16(PC),A5 ...
            (%t68-put (+ s 4) 2 8)                    ; ... the stream, 8 past this word
            (%t68-put (+ s 6) 2 #x4EF9)               ; JMP abs.L NEXT
            (%t68-put (+ s 8) 4 *t68-next*)
            (dolist (tk (third e))
              (let ((a (gethash (first tk) pcaddr)) (i (second tk)))
                (if (< i 255)
                    (progn (%t68-put a 1 i) (setq a (+ a 1)))
                    (progn (%t68-put a 1 255) (%t68-put (+ a 1) 2 i) (setq a (+ a 3))))
                (dolist (p (third tk))
                  (ecase (first p)
                    (:i (%t68-put a (second p) (third p)) (setq a (+ a (second p))))
                    (:br (let ((ta (or (gethash (second p) pcaddr)
                                       (progn (setq *t68-why* (list :no-label (second p)))
                                              (return-from %t68-translate nil)))))
                           (%t68-put a w (- ta (+ a w))) (setq a (+ a w))))
                    (:call (%t68-put a 4 (- (target-addr (second p)) (+ a 4)))
                     (setq a (+ a 4)))
                    (:cell (let ((c (%t68-cell (second p))))
                             (unless c (return-from %t68-translate nil))
                             (%t68-put a 4 c) (setq a (+ a 4))))))))))
        (setq *t68-ptr* addr)
        (mapcar (lambda (e) (cons (first (first e))
                                  (%t68-object (+ (gethash (second (first e)) stubs) 3))))
                plan)))))

(defun %t68-defun (name)
  "Translate runtime DEFUN NAME's module and make NAME the threaded function.
   T, or NIL with the reason in *T68-WHY*."
  (let* ((key (string name))
         (m (and *jit-module-registry* (gethash key *jit-module-registry*)))
         (fns (and m (%t68-translate m))))
    (when fns
      (dolist (f fns)
        (when (string= (car f) key)
          (setf (symbol-function name) (cdr f))))
      t)))
