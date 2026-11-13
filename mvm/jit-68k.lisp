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

(defun %t68-permutable ()
  "Operations with no result -- ASET U8-SET F32-SET SETCAR SETCDR OBJ-SET STORE
   -- and their operand kinds: one the image lacks for these registers can run
   as one it has, its registers permuted in and back (%T68-PERMUTED).  A
   function, not a DEFPARAMETER: a variable's initial value never runs in the
   image (CLAUDE.md limitation 7)."
  '((102 :r :r :r) (202 :r :r :r) (205 :r :r :r) (83 :r :r) (84 :r :r)
    (98 :r :i :r) (113 :r :r :i)))
(defvar *t68-by-op* nil "Permutable op -> the operand lists the image has shapes for.")
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

(defvar *t68-table* nil "The image's shape table (translate-68k m68k-emit-shape-table).")
(defvar *t68-table-n* 0)

(defun %t68-key-keywords ()
  "translate-68k *68K-KEY-KEYWORDS*, code for code."
  '((:li . 1) (:li-const . 2) (:br . 3) (:ret . 4) (:enter . 5) (:fused . 6) (:nil . 7) (:li-cell . 8)))

(defun %t68-key-bytes (key)
  "KEY's bytes, exactly as translate-68k M68K-KEY-BYTES writes the table's."
  (let ((out nil))
    (labels ((enc (x)
               (cond ((consp x)
                      (push (+ #xD0 (length x)) out)
                      (dolist (e x) (enc e)))
                     ((and (integerp x) (<= 0 x 127)) (push x out))
                     ((and (integerp x) (<= 0 x #xFFFFFF))
                      (push #x81 out) (push (ldb (byte 8 16) x) out)
                      (push (ldb (byte 8 8) x) out) (push (ldb (byte 8 0) x) out))
                     ((and (keywordp x) (assoc x (%t68-key-keywords)))
                      (push (+ #xC0 (cdr (assoc x (%t68-key-keywords)))) out))
                     ;; not encodable: no image shape can have it
                     (t (push #xFF out)))))
      (enc key))
    (nreverse out)))

(defun %t68-u32 (a)
  (+ (* (mem-ref a :u16) 65536) (mem-ref (+ a 2) :u16)))

(defun %t68-entry (i)
  (+ *t68-table* (%t68-u32 (+ *t68-table* 16 (* 4 i)))))

(defun %t68-compare (bytes e)
  "-1 / 0 / 1: BYTES against the key of the table entry at E."
  (let ((n (mem-ref (+ e 2) :u8)) (k (+ e 3)) (i 0))
    (dolist (b bytes (if (< i n) -1 0))
      (when (>= i n) (return 1))
      (let ((c (mem-ref (+ k i) :u8)))
        (cond ((< b c) (return -1)) ((> b c) (return 1))))
      (setq i (+ i 1)))))

(defun %t68-lookup (key)
  "KEY's token index in the image's table, by binary search; NIL if absent."
  (let ((bytes (%t68-key-bytes key)) (lo 0) (hi (- *t68-table-n* 1)) (found nil))
    (loop while (and (null found) (<= lo hi))
          do (let* ((mid (floor (+ lo hi) 2)) (e (%t68-entry mid)) (c (%t68-compare bytes e)))
               (cond ((= c 0) (setq found (mem-ref e :u16)))
                     ((< c 0) (setq hi (- mid 1)))
                     (t (setq lo (+ mid 1))))))
    found))

(defun %t68-decode-key (e)
  "The key of the table entry at E, as a list (the inverse of %T68-KEY-BYTES)."
  (let ((p (+ e 3)))
    (labels ((dec ()
               (let ((b (mem-ref p :u8)))
                 (setq p (+ p 1))
                 (cond ((< b #x80) b)
                       ((= b #x81) (let ((v (+ (* (mem-ref p :u8) 65536) (* (mem-ref (+ p 1) :u8) 256)
                                               (mem-ref (+ p 2) :u8))))
                                     (setq p (+ p 3)) v))
                       ((< b #xD0) (car (rassoc (- b #xC0) (%t68-key-keywords))))
                       (t (loop repeat (- b #xD0) collect (dec)))))))
      (dec))))

(defun %t68-branch-target (a)
  "The target of the BRA / JMP at A: JMP abs.L (the 68000's long branch),
   BRA.W, or BRA.S; NIL if A holds none of them."
  (let ((w (mem-ref a :u16)))
    (cond ((= w #x4EF9) (%t68-u32 (+ a 2)))
          ((= w #x6000) (let ((d (mem-ref (+ a 2) :u16)))
                          (+ a 2 (if (>= d #x8000) (- d #x10000) d))))
          ((= (logand w #xFF00) #x6000) (let ((d (logand w #xFF)))
                                          (+ a 2 (if (>= d #x80) (- d #x100) d))))
          (t nil))))

(defun %t68-next-p (a)
  "Is A NEXT: MOVEQ #0,D0 / MOVE.B (A5)+,D0?"
  (and a (= (mem-ref a :u16) #x7000) (= (mem-ref (+ a 2) :u16) #x101D)))

(defun %t68-find-next ()
  "NEXT, read out of CAR's entry, which is a threaded stub either way:
   unpaged, MOVEA.L A5,A1 / LEA stream(PC),A5 / JMP NEXT; paged (the Mac),
   MOVEA.L A5,A1 / MOVE.W #i,D0 / JMP COLD-ENTER, whose loaded-stream path is
   EXT.L D0 / ADD.L D0,D0 x2 / LEA table,A5 / BTST #0,(3,A5,D0.L) / BNE miss /
   MOVEA.L (0,A5,D0.L),A5 / JMP NEXT (translate-68k)."
  (let* ((a (- (%val->word (function car)) 3))
         (next
           (cond ((not (= (mem-ref a :u16) #x224D)) nil)
                 ((= (mem-ref (+ a 2) :u16) #x4BFA) (%t68-branch-target (+ a 6)))
                 ((= (mem-ref (+ a 2) :u16) #x303C)
                  (let ((c (%t68-branch-target (+ a 6))))
                    (when (and c (= (mem-ref c :u16) #x48C0))
                      (let* ((p (+ c 6))
                             (p (cond ((= (mem-ref p :u16) #x4BF9) (+ p 6))      ; LEA abs.L
                                      ((= (mem-ref p :u16) #x4BFA) (+ p 4))      ; LEA d16(PC)
                                      (t nil)))
                             (p (and p (= (mem-ref p :u16) #x0835) (+ p 6)))     ; BTST
                             (p (and p (cond ((= (mem-ref p :u16) #x6600) (+ p 4))
                                             ((= (logand (mem-ref p :u16) #xFF00) #x6600) (+ p 2))
                                             ((= (logand (mem-ref p :u16) #xFF00) #x6700) (+ p 8)) ; BEQ.S over JMP abs.L
                                             (t nil))))
                             (p (and p (= (mem-ref p :u16) #x2A75) (+ p 4))))    ; MOVEA.L
                        (and p (%t68-branch-target p))))))
                 (t nil))))
    (unless (%t68-next-p next) (error "jit-68k: cannot find NEXT from CAR's entry"))
    (setq *t68-next* next)))

(defun %t68-find-table ()
  "The shape table (translate-68k m68k-emit-shape-table): right after the
   dispatch table, which NEXT's MOVE.L (d8,PC,D0.W),D0 names (its extension
   word 6 bytes in, or 10 on the 68000, which scales the index with two ADD.W
   D0,D0), 4-aligned, behind \"modusSHP\".  The operand lists of the
   permutable operations are collected too."
  (%t68-find-next)
  (let* ((ext (if (= (mem-ref (+ *t68-next* 6) :u16) #xD040) (+ *t68-next* 10) (+ *t68-next* 6)))
         (tab (+ ext (logand (mem-ref ext :u16) #xFF)))
         (a (logand (+ tab 3) -4)))
    (setq *t68-table* nil)
    (loop repeat 65536
          while (null *t68-table*)
          do (when (and (= (%t68-u32 a) #x6D6F6475) (= (%t68-u32 (+ a 4)) #x73534850))
               (setq *t68-table* a *t68-table-n* (%t68-u32 (+ a 12))))
             (setq a (+ a 4)))
    (unless *t68-table* (error "jit-68k: no shape table in the image (built without MODUS_68K_JIT=1?)"))
    ;; the permutable operations' operand lists
    (let ((byop (make-hash-table)))
      (dotimes (i *t68-table-n*)
        (let ((e (%t68-entry i)))
          ;; a list whose first element is a small integer: an op's key
          (when (and (> (mem-ref (+ e 2) :u8) 1) (>= (mem-ref (+ e 3) :u8) #xD0)
                     (< (mem-ref (+ e 4) :u8) #x80) (assoc (mem-ref (+ e 4) :u8) (%t68-permutable)))
            (let ((k (%t68-decode-key e)))
              (push (cdr k) (gethash (car k) byop))))))
      (setq *t68-by-op* byop))
    *t68-table*))

(defun %t68-fixed-area ()
  "(LO . HI) of a code area the map reserves, or NIL to map one (hosted
   Linux).  The Mac build overrides it with its layout's :JIT-AREA."
  nil)

(defun %t68-init (&optional (area-size 1048576))
  "Ready the JIT: the shape table and NEXT, the constant cells, the code area,
   the counters (a variable's initial value never runs in the image)."
  (%t68-find-table) (%t68-cells-init)
  (let ((fixed (%t68-fixed-area)))
    (if fixed
        (setq *t68-ptr* (car fixed) *t68-end* (cdr fixed))
        (%t68-area area-size)))
  (setq *t68-translated* 0 *t68-refused* nil)
  t)

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

(defun %t68-walk (bc start end w lbls fn)
  "FN (pc key params) for each token of [START, END); :FAIL if one will not thread."
  (let ((pc start))
    (loop while (< pc end)
          do (let* ((d (decode-instruction bc pc)) (op (car d)) (ops (cadr d)) (npc (cddr d))
                    (nxt (and (< npc end) (not (gethash npc lbls)) (decode-instruction bc npc))))
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

(defun %t68-mirror-branch (op)
  "The branch that tests the same thing with the compare's operands swapped."
  (cond ((= op 67) 70) ((= op 70) 67)        ; BLT <-> BGT
        ((= op 68) 69) ((= op 69) 68)        ; BGE <-> BLE
        (t op)))                             ; BEQ, BNE

(defun %t68-equivalents (key)
  "Keys that compute the same thing as KEY with the same stream operands: a
   register CMP and its branch with the operands swapped and the branch
   mirrored, a commutative operation's two sources swapped (ADD SUB? no: ADD
   MUL BAND BOR BXOR and the checked ADD / MUL), TEST's two operands swapped."
  (let ((out nil))
    (when (and (eq (first key) :fused) (consp (second key)))
      (let ((k (second key)) (br (third key)) (w (fourth key)))
        (when (and (= (first k) +t68-op-cmp+) (= (length k) 3))
          (push (list :fused (list (first k) (third k) (second k)) (%t68-mirror-branch br) w) out))
        (when (and (= (first k) +t68-op-test+) (= (length k) 3))
          (push (list :fused (list (first k) (third k) (second k)) br w) out))))
    (when (and (integerp (first key)) (member (first key) '(32 34 40 41 42 173 174))
               (= (length key) 4))
      (push (list (first key) (second key) (fourth key) (third key)) out))
    out))

(defun %t68-permuted (key)
  "Token indices running KEY -- a permutable operation the image has no shape
   for with these registers -- as one it has: push the registers it will
   overwrite, move the operands in through the stack, run it, pop them back.
   NIL when no candidate fits."
  (let* ((op (first key)) (kinds (cdr (assoc op (%t68-permutable)))) (src (rest key)))
    (when (and kinds (= (length kinds) (length src)))
      (dolist (cand (gethash op *t68-by-op*) nil)
        (let ((ok t) (moves nil) (idents nil))
          (loop for k in kinds for sr in src for c in cand
                do (cond ((eq k :i) (unless (eql sr c) (setq ok nil)))
                         ((eql sr c) (push sr idents))
                         (t (push (cons sr c) moves))))
          (setq moves (nreverse moves))
          (let ((targets (mapcar (function cdr) moves)))
            (when (and ok moves
                       (= (length (remove-duplicates targets)) (length targets))
                       ;; a register an unmoved operand reads must not be overwritten
                       (notany (lambda (r) (member r targets)) idents))
              (let ((exec (%t68-lookup (cons op cand)))
                    (pushes (mapcar (lambda (r) (%t68-lookup (list 18 r))) (append targets (mapcar (function car) moves))))
                    (pops (mapcar (lambda (r) (%t68-lookup (list 19 r))) targets)))
                (when (and exec (every (function identity) pushes) (every (function identity) pops))
                  (let ((n (length targets)))
                    (return
                      (append (subseq pushes 0 n)                  ; save the targets
                              (subseq pushes n)                    ; the operands' values
                              (reverse pops)                       ; ... into the targets
                              (list exec)
                              (reverse pops)))))))))))))         ; the targets back

(defun %t68-token (key)
  "KEY's token index in the image's table, or an equivalent key's; NIL (and
   *T68-WHY*) when the image has neither."
  (or (%t68-lookup key)
      (dolist (k (%t68-equivalents key) nil)
        (let ((i (%t68-lookup k))) (when i (return i))))
      (progn (setq *t68-why* (list :no-shape key)) nil)))

(defun %t68-param-size (p w)
  (ecase (first p) (:i (second p)) (:br w) (:call 4) (:cell 4)))

(defun %t68-put (addr n v)
  "V's low N bytes, big-endian, at ADDR."
  (let ((u (logand v (- (ash 1 (* 8 n)) 1))))
    (loop for i from 0 below n
          do (setf (mem-ref (+ addr i) :u8) (ldb (byte 8 (* 8 (- n 1 i))) u)))))

(defun %t68-call-target (operand rt fn-table stubs)
  "The address an MVM call operand names -- a function-table index, a bytecode
   offset (a stub of this module), or a runtime stub (+T68-RT-BASE+ + n) named
   in RT that must be native image code -- or NIL (*T68-WHY*)."
  (if (>= operand +t68-rt-base+)
      (let* ((name (gethash operand rt))
             (fn (and name (%mvm-resolve-runtime-fn name)))
             (w (and fn (%val->word fn))))
        (if (and w (= (logand w 15) 3))
            (- w 3)
            (progn (setq *t68-why* (list :not-native name)) nil)))
      (let ((pc (if (< operand (length fn-table)) (aref fn-table operand) operand)))
        (or (gethash pc stubs)
            (progn (setq *t68-why* (list :no-callee operand)) nil)))))

;;; NO NON-LOCAL EXITS in here: a RETURN-FROM %T68-TRANSLATE out of the walk's
;;; lambda returned from the LAMBDA only (measured, interpreted on the hosted
;;; 68k), so a missing shape was skipped, not refused -- (SETF AREF)'s token
;;; vanished and the store with it.  Failures set BAD and are checked after.
(defun %t68-translate (m)
  "Module M (*JIT-MODULE-REGISTRY*'s (bc entry ft-list fn-table rt-table ...))
   as threaded code: an alist (NAME . FUNCTION-OBJECT), or NIL (*T68-WHY*).
   Nothing is written to the code area unless everything resolves."
  (let* ((bc (first m)) (ft-list (third m)) (fn-table (fourth m)) (rt (fifth m))
         (lbls (%t68-labels bc))
         ;; not the module's eval thunk: it runs once, at the DEFUN, and
         ;; returns the quoted name (an LI-CONST)
         (fns (sort (remove "%MVM-EVAL-THUNK" (copy-list ft-list)
                            :key (function car) :test (function string=))
                    (function <) :key (function second)))
         (plan nil) (bad nil))
    (setq *t68-why* nil)
    ;; pass 1: tokens and sizes, so every label has an address
    (dolist (f fns)
      (unless bad
        (let* ((start (second f)) (end (+ start (third f)))
               (w (if (> (third f) 20000) 4 2))
               (toks nil)
               (r (%t68-walk bc start end w lbls
                             (lambda (pc k p)
                               (let ((i (%t68-token k)))
                                 (if i
                                     (push (list pc i p nil) toks)
                                     ;; a permutable op: several tokens, the first at PC
                                     (let ((seq (and (null p) (%t68-permuted k))))
                                       (if seq
                                           (progn (push (list pc (car seq) nil nil) toks)
                                                  (dolist (j (cdr seq)) (push (list nil j nil nil) toks)))
                                           (setq bad t)))))))))
          (when (eq r :fail) (setq bad t))
          (push (list f w (nreverse toks)) plan))))
    (when bad (return-from %t68-translate nil))
    (setq plan (nreverse plan))
    ;; layout: each function a 16-aligned stub (12 bytes) then its stream
    (let ((addr *t68-ptr*) (pcaddr (make-hash-table)) (stubs (make-hash-table)))
      (dolist (e plan)
        (setq addr (logand (+ addr 15) -16))
        (setf (gethash (second (first e)) stubs) addr)
        (setq addr (+ addr 12))
        (dolist (tk (third e))
          (when (first tk) (setf (gethash (first tk) pcaddr) addr))
          (rplaca (cdddr tk) addr)
          (setq addr (+ addr (if (< (second tk) 255) 1 3)
                        (loop for p in (third tk) sum (%t68-param-size p (second e)))))))
      (when (> addr *t68-end*) (setq *t68-why* :area-full) (return-from %t68-translate nil))
      ;; pass 2: every operand's value -- call targets, cells, branch
      ;; targets -- before anything is written
      (dolist (e plan)
        (dolist (tk (third e))
          (rplaca (cddr tk)
                (mapcar (lambda (p)
                          (let ((v (ecase (first p)
                                     (:i (third p))
                                     (:br (or (gethash (second p) pcaddr)
                                              (progn (setq *t68-why* (list :no-label (second p))) nil)))
                                     (:call (%t68-call-target (second p) rt fn-table stubs))
                                     (:cell (%t68-cell (second p))))))
                            (unless v (setq bad t))
                            (list (first p) (if (eq (first p) :i) (second p) 4) v)))
                        (third tk)))))
      (when bad (return-from %t68-translate nil))
      ;; pass 3: write
      (dolist (e plan)
        (let* ((s (gethash (second (first e)) stubs)) (w (second e)))
          (%t68-put s 2 #x224D)                     ; MOVEA.L A5,A1
          (%t68-put (+ s 2) 2 #x4BFA)               ; LEA d16(PC),A5 ...
          (%t68-put (+ s 4) 2 8)                    ; ... the stream, 8 past this word
          (%t68-put (+ s 6) 2 #x4EF9)               ; JMP abs.L NEXT
          (%t68-put (+ s 8) 4 *t68-next*)
          (dolist (tk (third e))
            (let ((a (fourth tk)) (i (second tk)))
              (if (< i 255)
                  (progn (%t68-put a 1 i) (setq a (+ a 1)))
                  (progn (%t68-put a 1 255) (%t68-put (+ a 1) 2 i) (setq a (+ a 3))))
              (dolist (p (third tk))
                (ecase (first p)
                  (:i (%t68-put a (second p) (third p)) (setq a (+ a (second p))))
                  (:br (%t68-put a w (- (third p) (+ a w))) (setq a (+ a w)))
                  (:call (%t68-put a 4 (- (third p) (+ a 4))) (setq a (+ a 4)))
                  (:cell (%t68-put a 4 (third p)) (setq a (+ a 4)))))))))
      (setq *t68-ptr* addr)
      (mapcar (lambda (e) (cons (first (first e))
                                (%t68-object (+ (gethash (second (first e)) stubs) 3))))
              plan))))

(defun %t68-install (nm fn)
  "Publish FN as runtime DEFUN NM in both function tables, as the x64 JIT's
   %JIT-INSTALL-NATIVE-FNS does: by name (the bridge %MVM-RESOLVE-RUNTIME-FN
   reads, so later translations link to it) and by name hash (SYMBOL-FUNCTION,
   FUNCALL); a generic function's dispatcher is registered for the new object."
  (when (boundp (quote *symbol-function-table*))
    (let* ((old (gethash nm *symbol-function-table*))
           (gf (and old (boundp (quote *gf-fn-to-name*)) (assoc old *gf-fn-to-name*))))
      (when gf (%register-gf-fn fn (cdr gf))))
    (puthash nm *symbol-function-table* fn))
  (when (boundp (quote *native-sym-function-table*))
    (puthash (compute-name-hash nm) *native-sym-function-table* fn))
  (setq *jit-native-defun-count* (if *jit-native-defun-count* (+ *jit-native-defun-count* 1) 1)))

(defvar *t68-translated* 0 "CENSUS: modules translated.")
(defvar *t68-refused* nil "CENSUS: (name . why) of modules left interpreted, newest first.")

(defun %t68-on-defun (names)
  "*RUNTIME-DEFUN-HOOK*: translate the module mvm-eval just installed for
   NAMES and publish its DEFUNs; on any failure they stay interpreted."
  (let ((m (and names *jit-module-registry* (gethash (car names) *jit-module-registry*))))
    (when m
      (let ((fns (handler-case (%t68-translate m)
                   (error (c) (setq *t68-why* (list :error (princ-to-string c))) nil))))
        (if fns
            (progn
              (setq *t68-translated* (+ *t68-translated* 1))
              (dolist (f fns)
                (when (member (car f) names :test (function string=))
                  (%t68-install (car f) (cdr f)))))
            (setq *t68-refused* (cons (cons (car names) *t68-why*) *t68-refused*)))))))

(defun %t68-defun (name)
  "Translate runtime DEFUN NAME (a symbol) now; T or NIL (*T68-WHY*)."
  (let* ((key (string name))
         (m (and *jit-module-registry* (gethash key *jit-module-registry*)))
         (fns (and m (%t68-translate m))))
    (when fns
      (dolist (f fns)
        (when (string= (car f) key) (%t68-install key (cdr f))))
      t)))

(defvar *t68-off* nil "T: leave runtime DEFUNs interpreted.")

(defun %runtime-defun-hook (names)
  "mvm-eval's hook (overridden here, last DEFUN wins): every runtime DEFUN is
   translated as it is defined, the JIT readied on the first one, unless
   *T68-OFF*.  A JIT that cannot start leaves DEFUNs interpreted."
  (when (and names (not *t68-off*))
    (when (null *t68-table*)
      (handler-case (%t68-init)
        (error (c) (setq *t68-off* t *t68-why* (list :init (princ-to-string c))))))
    (when *t68-table* (%t68-on-defun names))))

(defun t68-enable (&optional (area-size 1048576))
  "Ready the JIT now (it starts itself at the first DEFUN otherwise)."
  (setq *t68-off* nil)
  (%t68-init area-size))
