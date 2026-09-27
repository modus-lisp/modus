;;; Override write-string-serial to be nil-safe
;;; (MVM compiles string literals as NIL — no heap strings on bare metal)
(defun write-string-serial (str)
  (if (null str)
      nil
      (let ((len (array-length str))
            (i 0))
        (loop
          (when (= i len) (return nil))
          (write-char-serial (aref str i))
          (setq i (+ i 1))))))

;;; Architecture-neutral metadata helpers: read/write u32 LE via :u8
;;; Works on any architecture (64/32-bit, LE/BE)
(defun td-read-u32 (addr)
  (let ((b0 (mem-ref addr :u8)))
    (let ((b1 (mem-ref (+ addr 1) :u8)))
      (let ((b2 (mem-ref (+ addr 2) :u8)))
        (let ((b3 (mem-ref (+ addr 3) :u8)))
          (+ b0 (+ (ash b1 8) (+ (ash b2 16) (ash b3 24)))))))))

(defun td-write-u32 (addr val)
  (setf (mem-ref addr :u8) (logand val 255))
  (setf (mem-ref (+ addr 1) :u8) (logand (ash val -8) 255))
  (setf (mem-ref (+ addr 2) :u8) (logand (ash val -16) 255))
  (setf (mem-ref (+ addr 3) :u8) (logand (ash val -24) 255)))

;;; Override error to use direct char output + print diagnostics + actually halt
;;; Diagnostics at 0x380040+ use u32 LE format
(defun error (msg)
  ;; Print ERR: using direct char codes (not string literals)
  (write-char-serial 69) (write-char-serial 82)
  (write-char-serial 82) (write-char-serial 58)
  ;; Print current function index from diagnostic address
  (write-char-serial 102) ;; f
  (print-dec (td-read-u32 #x3000040))
  ;; Print step checkpoint
  (write-char-serial 115) ;; s
  (print-dec (td-read-u32 #x3000044))
  ;; Print current bytecode position
  (write-char-serial 64) ;; @
  (print-dec (td-read-u32 #x3000048))
  ;; Print opcode
  (write-char-serial 111) ;; o
  (print-dec (td-read-u32 #x300004C))
  ;; Print new-pos
  (write-char-serial 110) ;; n
  (print-dec (td-read-u32 #x3000050))
  ;; Print MOV operands
  (write-char-serial 32) (write-char-serial 100) ;; d
  (print-dec (td-read-u32 #x3000054))
  (write-char-serial 47) ;; /
  (print-dec (td-read-u32 #x3000058))
  ;; Print completion marker
  (write-char-serial 32) (write-char-serial 99) ;; c
  (print-dec (td-read-u32 #x300005C))
  ;; Print msg value (if fixnum, print as number; helps identify which error)
  (write-char-serial 32) (write-char-serial 109) ;; m
  (print-dec msg)
  (write-char-serial 10)
  ;; Actually halt: (hlt) is the compiler built-in, (halt) is undefined
  (loop (hlt)))

;;; Override warn to use direct char output
(defun warn (msg)
  (write-char-serial 87) (write-char-serial 65)
  (write-char-serial 82) (write-char-serial 78)
  (write-char-serial 58) (write-char-serial 10))

;;; Fixpoint metadata layout (all u32 LE at VA 0x500000):
;;;   +0x00: magic 0x544D564D (MVMT)
;;;   +0x04: version (1)
;;;   +0x08: my-architecture (0=x64, 1=aarch64)
;;;   +0x0C: bytecode-offset (image offset)
;;;   +0x10: bytecode-length
;;;   +0x14: fn-table-offset (image offset)
;;;   +0x18: fn-table-count
;;;   +0x1C: native-code-offset (image offset)
;;;   +0x20: native-code-length
;;;   +0x24: preamble-size
;;;   +0x28: kernel-main-hash-lo (lower 32 bits)
;;;   +0x2C: kernel-main-native-offset
;;;   +0x30: image-load-addr
;;;   +0x34: (reserved)
;;;   +0x38: (reserved)
;;;   +0x3C: (reserved)
;;; Diagnostics at +0x40:
;;;   +0x40: current fn index (u32)
;;;   +0x44: step checkpoint (u32)
;;;   +0x48: bytecode position (u32)
;;;   +0x4C: opcode (u32)

;;; Fixpoint: read embedded MVM bytecode into an array
(defun td-read-bytecode ()
  (let ((img-base (td-read-u32 #x3000030)))
    (let ((bc-off (td-read-u32 #x300000C)))
      (let ((bc-len (td-read-u32 #x3000010)))
        (if (zerop bc-len)
            nil
            (let ((addr (+ img-base bc-off)))
              ;; byte array: 10 MB, not an 81 MB general array (see
              ;; a64-buffer-to-bytes in fixpoint-aarch64.lisp, #252)
              (let ((bc (make-array bc-len :element-type '(unsigned-byte 8)))
                    (i 0))
                (loop
                  (when (>= i bc-len) (return bc))
                  (aset bc i (mem-ref (+ addr i) :u8))
                  (setq i (+ i 1))))))))))

;;; Fixpoint: read embedded function table as a list of (name offset length)
;;; Function table entries are 12 bytes each (3 x u32 LE)
(defun td-read-fn-table-list ()
  (let ((img-base (td-read-u32 #x3000030)))
    (let ((ft-off (td-read-u32 #x3000014)))
      (let ((ft-count (td-read-u32 #x3000018)))
        (if (zerop ft-count)
            nil
            (let ((addr (+ img-base ft-off)))
              (let ((result nil)
                    (i 0))
                (loop
                  (when (>= i ft-count) (return (nreverse result)))
                  (let ((base (+ addr (* i 12))))
                    (let ((name (td-read-u32 base)))
                      (let ((offset (td-read-u32 (+ base 4))))
                        (let ((length (td-read-u32 (+ base 8))))
                          (setq result (cons (cons name (cons offset (cons length nil)))
                                             result))
                          (setq i (+ i 1))))))))))))))

;;; Direct register lookup functions — bypass *registers* alist
;;; The 48-entry *registers* alist in x64-asm.lisp is too large for
;;; compile-quote (causes triple fault from deep recursive cons nesting).
;;; These overrides compute register info directly via cond, no consing.

;;; #252: everything from here to the end of the x64 driver is read in
;;; MODUS.MVM.X64.  Register names are SYMBOLS and symbol identity is
;;; per-package: read in MODUS.MVM, (quote r15) was a different object from
;;; the MODUS.ASM:R15 the assembler's tables and translate-x64's vreg map use,
;;; so every register the fixpoint's own intercepts emitted (CONSP/ATOM's
;;; `mov d, r15', GC-CHECK's `cmp r12, r14', TAILCALL's rbx/rbp/rsp) encoded
;;; as EAX: Gen2 booted, printed R1 and died on `mov rbx,[rbx+7]' with a
;;; garbage rbx.  In this package the names are the inherited MODUS.ASM
;;; symbols; helper calls into MODUS.MVM resolve through the bare-name
;;; fallback and globals are keyed by name hash alone.
(in-package :modus.mvm.x64)
(defun reg-code-lookup (reg)
  (cond
    ((eql reg 'rax) 0) ((eql reg 'eax) 0) ((eql reg 'al) 0)
    ((eql reg 'rcx) 1) ((eql reg 'ecx) 1) ((eql reg 'cl) 1)
    ((eql reg 'rdx) 2) ((eql reg 'edx) 2) ((eql reg 'dl) 2)
    ((eql reg 'rbx) 3) ((eql reg 'ebx) 3) ((eql reg 'bl) 3)
    ((eql reg 'rsp) 4) ((eql reg 'esp) 4) ((eql reg 'spl) 4)
    ((eql reg 'rbp) 5) ((eql reg 'ebp) 5) ((eql reg 'bpl) 5)
    ((eql reg 'rsi) 6) ((eql reg 'esi) 6) ((eql reg 'sil) 6)
    ((eql reg 'rdi) 7) ((eql reg 'edi) 7) ((eql reg 'dil) 7)
    ((eql reg 'r8) 8) ((eql reg 'r8d) 8) ((eql reg 'r8b) 8)
    ((eql reg 'r9) 9) ((eql reg 'r9d) 9) ((eql reg 'r9b) 9)
    ((eql reg 'r10) 10) ((eql reg 'r10d) 10) ((eql reg 'r10b) 10)
    ((eql reg 'r11) 11) ((eql reg 'r11d) 11) ((eql reg 'r11b) 11)
    ((eql reg 'r12) 12) ((eql reg 'r12d) 12) ((eql reg 'r12b) 12)
    ((eql reg 'r13) 13) ((eql reg 'r13d) 13) ((eql reg 'r13b) 13)
    ((eql reg 'r14) 14) ((eql reg 'r14d) 14) ((eql reg 'r14b) 14)
    ((eql reg 'r15) 15) ((eql reg 'r15d) 15) ((eql reg 'r15b) 15)
    (t 0)))

(defun reg-size-lookup (reg)
  (cond
    ((eql reg 'rax) 64) ((eql reg 'rcx) 64) ((eql reg 'rdx) 64) ((eql reg 'rbx) 64)
    ((eql reg 'rsp) 64) ((eql reg 'rbp) 64) ((eql reg 'rsi) 64) ((eql reg 'rdi) 64)
    ((eql reg 'r8) 64) ((eql reg 'r9) 64) ((eql reg 'r10) 64) ((eql reg 'r11) 64)
    ((eql reg 'r12) 64) ((eql reg 'r13) 64) ((eql reg 'r14) 64) ((eql reg 'r15) 64)
    ((eql reg 'eax) 32) ((eql reg 'ecx) 32) ((eql reg 'edx) 32) ((eql reg 'ebx) 32)
    ((eql reg 'esp) 32) ((eql reg 'ebp) 32) ((eql reg 'esi) 32) ((eql reg 'edi) 32)
    ((eql reg 'r8d) 32) ((eql reg 'r9d) 32) ((eql reg 'r10d) 32) ((eql reg 'r11d) 32)
    ((eql reg 'r12d) 32) ((eql reg 'r13d) 32) ((eql reg 'r14d) 32) ((eql reg 'r15d) 32)
    ((eql reg 'al) 8) ((eql reg 'cl) 8) ((eql reg 'dl) 8) ((eql reg 'bl) 8)
    ((eql reg 'spl) 8) ((eql reg 'bpl) 8) ((eql reg 'sil) 8) ((eql reg 'dil) 8)
    ((eql reg 'r8b) 8) ((eql reg 'r9b) 8) ((eql reg 'r10b) 8) ((eql reg 'r11b) 8)
    ((eql reg 'r12b) 8) ((eql reg 'r13b) 8) ((eql reg 'r14b) 8) ((eql reg 'r15b) 8)
    (t 64)))

(defun reg-rex-flag (reg)
  (cond
    ((eql reg 'r8) t) ((eql reg 'r9) t) ((eql reg 'r10) t) ((eql reg 'r11) t)
    ((eql reg 'r12) t) ((eql reg 'r13) t) ((eql reg 'r14) t) ((eql reg 'r15) t)
    ((eql reg 'r8d) t) ((eql reg 'r9d) t) ((eql reg 'r10d) t) ((eql reg 'r11d) t)
    ((eql reg 'r12d) t) ((eql reg 'r13d) t) ((eql reg 'r14d) t) ((eql reg 'r15d) t)
    ((eql reg 'spl) t) ((eql reg 'bpl) t) ((eql reg 'sil) t) ((eql reg 'dil) t)
    ((eql reg 'r8b) t) ((eql reg 'r9b) t) ((eql reg 'r10b) t) ((eql reg 'r11b) t)
    ((eql reg 'r12b) t) ((eql reg 'r13b) t) ((eql reg 'r14b) t) ((eql reg 'r15b) t)
    (t nil)))

;;; Override reg-info to build result from direct lookups (last-defun-wins)
(defun reg-info (reg)
  (let ((code (reg-code-lookup reg)))
    (let ((size (reg-size-lookup reg)))
      (let ((rex (reg-rex-flag reg)))
        (let ((t1 (cons rex nil)))
          (let ((t2 (cons size t1)))
            (let ((t3 (cons code t2)))
              (cons reg t3))))))))

;;; Array-based position labels: bypass hash table for branch target labels.
;;; Hash table gethash/puthash inconsistently fails on AArch64 bare metal,
;;; causing 140 labels with target=0 (position never set). Array indexing
;;; is simple and reliable.
(defvar *td-label-array* nil)
(defvar *td-label-base* 0)
;;; Global fn-offset-to-label array: indexed by bytecode offset.
;;; CALL opcode looks up function labels by bytecode offset.
;;; Hash table gethash is unreliable on AArch64 bare metal.
(defvar *td-fn-label-array* nil)
;;; #252: the helper labels the HOST translate-mvm-to-x64 threads through every
;;; translate-state and emits bodies for after the last function.  Left NIL,
;;; every cons-alloc site, handler-case and gc-check emitted a CALL to a NIL
;;; label (x64-asm's "label-p FALSE-NEGATIVE" lines), and the fixup pass then
;;; recursed BIGNUM-ASH to death on (aref NIL 1).  Name hashes are baked by
;;; build-fixpoint's kernel-main text (the fn table here carries hashes).
(defvar *td-x64-gc-label* nil)
(defvar *td-x64-hpush-label* nil)
(defvar *td-x64-hpop-label* nil)
(defvar *td-x64-yield-label* nil)
(defvar *td-gc-collect-hash* 0)
(defvar *td-genmul-hash* 0)
(defvar *td-genadd-hash* 0)
(defvar *td-gensub-hash* 0)

;;; Override ensure-label-at: use array instead of position-labels hash table
;;; #252 / #211: since the compiler keys calls by PKG::NAME (falling back to
;;; the bare name), an override of a MODUS.ASM or MODUS.MVM.X64 function that
;;; is read in MODUS.MVM registers as MODUS.MVM::NAME and does NOT replace it
;;; for callers inside those packages: translate-x64 kept using its own
;;; hash-table ENSURE-LABEL-AT and x64-asm's EMIT-CALL/EMIT-LABEL while the
;;; fixpoint driver emitted labels through its array-based ones -- two label
;;; systems on one code buffer.  Gen1's hop 2 died in FIXUP-LABELS (h43) and
;;; later ran away at a BR (h44-h46).  Each override below is therefore read
;;; in the package of the function it replaces.  Helper references inside
;;; them (write-char-serial, print-dec, td-*, set-code-buffer-fixups) still
;;; resolve through the bare-name fallback.
(in-package :modus.mvm.x64)
(defun ensure-label-at (state mvm-pos)
  (let ((idx (- mvm-pos *td-label-base*)))
    (let ((existing (aref *td-label-array* idx)))
      (if (zerop existing)
          (let ((label (make-label)))
            (let ((dummy (aset *td-label-array* idx label)))
              label))
          existing))))

;;; Override emit-label: simpler version that just sets position (no hash table)
(in-package :modus.asm)
(defun emit-label (buf label)
  (let ((pos (code-buffer-position buf)))
    (aset label 1 pos)))

;;; Override scan-branch-targets: use numeric opcode checks instead of
;;; (member :off16 op-specs) which fails on bare metal (equal broken for cons cells).
;;; Branch opcodes: BR(64)-BGT(70) have offset in operand 0,
;;;                 BNULL(71)/BNNULL(72) have offset in operand 1.
(in-package :modus.mvm.x64)
(defun scan-branch-targets (state)
  (let ((bytes (translate-state-mvm-bytes state))
        (offset (translate-state-mvm-offset state))
        (length (translate-state-mvm-length state)))
    (let ((pos offset)
          (limit (+ offset length)))
      (loop
        (when (>= pos limit) (return nil))
        (let ((decoded (decode-instruction bytes pos)))
          (let ((opcode (car decoded))
                (operands (car (cdr decoded)))
                (new-pos (cdr (cdr decoded))))
            ;; Check if branch opcode (0x40-0x48 = 64-72)
            (when (and (>= opcode 64) (<= opcode 72))
              (let ((off (if (<= opcode 70)
                             (car operands)         ;; BR-BGT: offset is operand 0
                             (car (cdr operands))))) ;; BNULL/BNNULL: offset is operand 1
                (let ((target-pos (+ new-pos off)))
                  (ensure-label-at state target-pos))))
            (setq pos new-pos)))))))

(in-package :modus.mvm.x64)
(defun reg-code (reg)
  (logand 7 (reg-code-lookup reg)))

(defun reg-size (reg)
  (reg-size-lookup reg))

(defun reg-needs-rex-p (reg)
  (let ((code (reg-code-lookup reg)))
    (if (>= code 8) t (reg-rex-flag reg))))

(defun reg-extended-p (reg)
  (>= (reg-code-lookup reg) 8))

;;; Override emit-load-vreg/emit-store-vreg with diagnostic (last-defun-wins)
(defun emit-load-vreg (buf vreg phys-dest)
  (let ((phys (when (< vreg (length *vreg-to-x64*))
                (vreg-phys vreg))))
    (cond
      (phys
       (if (eql phys phys-dest)
           nil
           (emit-mov-reg-reg buf phys-dest phys)))
      ((vreg-spills-p vreg)
       (emit-mov-reg-mem buf phys-dest 'rbp (spill-offset vreg)))
      ;; VPC=22, VSP=20, VFP=21 are special registers that shouldn't appear as operands
      ;; Handle gracefully: load 0 for VPC, use RBP for VSP, use RBP for VFP
      ((= vreg 22)  ; VPC - load current position (not meaningful in translation, use 0)
       (emit-mov-reg-imm buf phys-dest 0))
      ((= vreg 20)  ; VSP - use RBP as stack pointer
       (emit-mov-reg-reg buf phys-dest 'rbp))
      ((= vreg 21)  ; VFP - use RBP as frame pointer
       (emit-mov-reg-reg buf phys-dest 'rbp))
      ;; Out-of-bounds vreg - treat as load 0
      (t
       (emit-mov-reg-imm buf phys-dest 0)))))

(defun emit-store-vreg (buf vreg phys-src)
  (let ((phys (when (< vreg (length *vreg-to-x64*))
                (vreg-phys vreg))))
    (cond
      (phys
       (if (eql phys phys-src)
           nil
           (emit-mov-reg-reg buf phys phys-src)))
      ((vreg-spills-p vreg)
       (emit-mov-mem-reg buf 'rbp phys-src (spill-offset vreg)))
      ;; VPC=22, VSP=20, VFP=21 are special registers - ignore stores to them
      ;; Also ignore out-of-bounds vregs
      (t
       nil))))

;;; Direct ALU rrr helpers — no funcall/#'function (avoids FN-ADDR on AArch64)
;;; Each function inlines the specific emit-xxx-reg-reg call.
(defun td-alu-core (buf vd va vb op-tag)
  ;; op-tag: 0=add, 1=sub, 2=and, 3=or, 4=xor
  (let ((d (dest-phys-or-scratch vd)))
    (let ((pa (vreg-phys va)))
      (let ((pb (vreg-phys vb)))
        ;; Step 1: load Va into d
        (cond
          (pa (if (eql pa d) nil (emit-mov-reg-reg buf d pa)))
          (t (emit-load-vreg buf va d)))
        ;; Step 2: apply op (dispatch by tag, no funcall)
        (let ((src (if pb pb nil)))
          (if src
              (cond
                ((= op-tag 0) (emit-add-reg-reg buf d src))
                ((= op-tag 1) (emit-sub-reg-reg buf d src))
                ((= op-tag 2) (emit-and-reg-reg buf d src))
                ((= op-tag 3) (emit-or-reg-reg buf d src))
                ((= op-tag 4) (emit-xor-reg-reg buf d src)))
              (let ((tmp (if (eql d 'rax) 'r13 'rax)))
                (emit-push buf tmp)
                (emit-load-vreg buf vb tmp)
                (cond
                  ((= op-tag 0) (emit-add-reg-reg buf d tmp))
                  ((= op-tag 1) (emit-sub-reg-reg buf d tmp))
                  ((= op-tag 2) (emit-and-reg-reg buf d tmp))
                  ((= op-tag 3) (emit-or-reg-reg buf d tmp))
                  ((= op-tag 4) (emit-xor-reg-reg buf d tmp)))
                (emit-pop buf tmp))))
        ;; Step 3: store if spilled
        (maybe-store-scratch buf vd)))))

(defun td-emit-add (buf operands)
  (let ((vd (car operands)))
    (let ((va (car (cdr operands))))
      (let ((vb (car (cdr (cdr operands)))))
        (td-alu-core buf vd va vb 0)))))

(defun td-emit-sub (buf operands)
  (let ((vd (car operands)))
    (let ((va (car (cdr operands))))
      (let ((vb (car (cdr (cdr operands)))))
        (td-alu-core buf vd va vb 1)))))

(defun td-emit-and (buf operands)
  (let ((vd (car operands)))
    (let ((va (car (cdr operands))))
      (let ((vb (car (cdr (cdr operands)))))
        (td-alu-core buf vd va vb 2)))))

(defun td-emit-or (buf operands)
  (let ((vd (car operands)))
    (let ((va (car (cdr operands))))
      (let ((vb (car (cdr (cdr operands)))))
        (td-alu-core buf vd va vb 3)))))

(defun td-emit-xor (buf operands)
  (let ((vd (car operands)))
    (let ((va (car (cdr operands))))
      (let ((vb (car (cdr (cdr operands)))))
        (td-alu-core buf vd va vb 4)))))

(defun td-translate-fn-body (state)
  (let ((bytes (translate-state-mvm-bytes state))
        (offset (translate-state-mvm-offset state))
        (len (translate-state-mvm-length state)))
    (let ((pos offset)
          (limit (+ offset len))
          (ninsn 0))
      (loop
        (when (>= pos limit) (return nil))
        ;; #252 diag: a function body that decodes more than 2M instructions
        ;; is looping (Gen1 hop 2 consed itself into GC thrash at fn ~5450);
        ;; report the function and its position and abandon the body.
        (setq ninsn (+ ninsn 1))
        (when (> ninsn 2000000)
          (write-char-serial 33) (write-char-serial 76) ;; !L<fn offset> <pos> <limit>
          (print-dec offset) (write-char-serial 32) (print-dec pos) (write-char-serial 32) (print-dec limit) (write-char-serial 10)
          (return nil))
        ;; Use array-based label lookup (bypasses hash table)
        (let ((lbl-idx (- pos *td-label-base*)))
          (let ((label (aref *td-label-array* lbl-idx)))
            (when (not (zerop label))
              (emit-label (translate-state-buf state) label))))
        (let ((decoded (decode-instruction bytes pos)))
          (let ((opcode (car decoded))
                (operands (car (cdr decoded)))
                (new-pos (cdr (cdr decoded))))
            ;; #252 diag: current opcode / bytecode position / first operand in
            ;; the metadata scratch words, readable from gdb at a stall.
            (td-write-u32 #x3000040 opcode)
            (td-write-u32 #x3000044 pos)
            (td-write-u32 #x3000048 (let ((o (car operands))) (if (integerp o) (logand o 4294967295) 4294967295)))
            ;; Intercept opcodes that need bare-metal fixes
            (cond
              ;; ALU rrr: direct call, no funcall/#'function
              ((= opcode 32)  ;; ADD
               (td-emit-add (translate-state-buf state) operands))
              ((= opcode 33)  ;; SUB
               (td-emit-sub (translate-state-buf state) operands))
              ((= opcode 40)  ;; AND
               (td-emit-and (translate-state-buf state) operands))
              ((= opcode 41)  ;; OR
               (td-emit-or (translate-state-buf state) operands))
              ((= opcode 42)  ;; XOR
               (td-emit-xor (translate-state-buf state) operands))
              ;; CONSP: handle internal labels explicitly (bypass translate-instruction)
              ((= opcode 85)  ;; CONSP (#x55)
               (let ((vd (car operands))
                     (vs (car (cdr operands))))
                 (let ((buf2 (translate-state-buf state)))
                   (let ((d (dest-phys-or-scratch vd))
                         (true-label (make-label))
                         (end-label (make-label)))
                     (emit-load-vreg buf2 vs d)
                     (emit-and-reg-imm buf2 d 15)
                     (emit-cmp-reg-imm buf2 d 1)
                     (emit-jcc buf2 :e true-label)
                     (emit-mov-reg-reg buf2 d (quote r15))
                     (emit-jmp buf2 end-label)
                     (let ((p1 (code-buffer-position buf2)))
                       (aset true-label 1 p1))
                     (emit-mov-reg-imm buf2 d 3735883785)
                     (let ((p2 (code-buffer-position buf2)))
                       (aset end-label 1 p2))
                     (maybe-store-scratch buf2 vd)))))
              ;; ATOM: handle internal labels explicitly
              ((= opcode 86)  ;; ATOM (#x56)
               (let ((vd (car operands))
                     (vs (car (cdr operands))))
                 (let ((buf2 (translate-state-buf state)))
                   (let ((d (dest-phys-or-scratch vd))
                         (true-label (make-label))
                         (end-label (make-label)))
                     (emit-load-vreg buf2 vs d)
                     (emit-and-reg-imm buf2 d 15)
                     (emit-cmp-reg-imm buf2 d 1)
                     (emit-jcc buf2 :ne true-label)
                     (emit-mov-reg-reg buf2 d (quote r15))
                     (emit-jmp buf2 end-label)
                     (let ((p1 (code-buffer-position buf2)))
                       (aset true-label 1 p1))
                     (emit-mov-reg-imm buf2 d 3735883785)
                     (let ((p2 (code-buffer-position buf2)))
                       (aset end-label 1 p2))
                     (maybe-store-scratch buf2 vd)))))
              ;; GC-CHECK: handle skip-label explicitly
              ((= opcode 137) ;; GC-CHECK (#x89)
               (let ((buf2 (translate-state-buf state)))
                 (let ((skip-label (make-label)))
                   (emit-cmp-reg-reg buf2 (quote r12) (quote r14))
                   (emit-jcc buf2 :l skip-label)
                   (let ((gc-lbl (translate-state-gc-label state)))
                     (if gc-lbl
                         (emit-call buf2 gc-lbl)
                         (emit-int buf2 49)))
                   (let ((p1 (code-buffer-position buf2)))
                     (aset skip-label 1 p1)))))
              ;; TAILCALL: use fn-label-array (like CALL)
              ((= opcode 131) ;; TAILCALL (#x83)
               (let ((target-offset (car operands)))
                 (let ((buf2 (translate-state-buf state))
                       (label (or (gethash target-offset *td-fn-label-array*) 0)))
                   (emit-mov-reg-mem buf2 (quote rbx) (quote rbp) -8)
                   (emit-mov-reg-reg buf2 (quote rsp) (quote rbp))
                   (emit-pop buf2 (quote rbp))
                   (if (zerop label)
                       (emit-jmp buf2 (make-label))
                       (emit-jmp buf2 label)))))
              ;; CALL: use array-based fn label lookup (bypass hash table)
              ((= opcode 128) ;; CALL
               (let ((target-offset (car operands)))
                 (let ((label (or (gethash target-offset *td-fn-label-array*) 0)))
                   (if (zerop label)
                       (emit-call (translate-state-buf state) (make-label))
                       (emit-call (translate-state-buf state) label)))))
              ;; FN-ADDR: same array lookup
              ((= opcode 167) ;; FN-ADDR
               (let ((vd (car operands))
                     (target-offset (car (cdr operands))))
                 (let ((label (or (gethash target-offset *td-fn-label-array*) 0)))
                   (let ((d (dest-phys-or-scratch vd)))
                     (if (zerop label)
                         ;; Unknown target — load 0 (matches original)
                         (emit-mov-reg-imm (translate-state-buf state) d 0)
                         (emit-lea-label (translate-state-buf state) d label))
                     (maybe-store-scratch (translate-state-buf state) vd)))))
              (t
               (translate-instruction state opcode operands new-pos)))
            (setq pos new-pos)))))))

(defun td-translate-one-fn (ctx fn-label offset len)
  ;; ctx = (cons buf (cons bytecode fn-offset-to-label))
  ;; 4 args → all in registers, no overflow stack args
  (let ((buf (car ctx))
        (bytecode (car (cdr ctx)))
        (fn-offset-to-label (cdr (cdr ctx))))
    (let ((state (make-translate-state)))
      (set-translate-state-buf state buf)
      (set-translate-state-mvm-bytes state bytecode)
      (set-translate-state-mvm-length state len)
      (set-translate-state-mvm-offset state offset)
      (set-translate-state-function-table state fn-offset-to-label)
      (set-translate-state-gc-label state *td-x64-gc-label*)
      (set-translate-state-handler-push-label state *td-x64-hpush-label*)
      (set-translate-state-handler-pop-label state *td-x64-hpop-label*)
      (set-translate-state-yield-longjmp-label state *td-x64-yield-label*)
      ;; 16-align the entry, as translate-mvm-to-x64 proper does: fn pointers are
      ;; addr|3 and funcall checks the low nibble, so the raw start must end in 0
      ;; (the boot preamble NOP-pads itself so native code starts 16-aligned;
      ;; *x64-native-code-offset* carries any residue).  Same gap as the AArch64
      ;; arm's, found via Gen1's first FUNCALL (#252).
      (loop
        (let ((n (logand (+ *x64-native-code-offset* (code-buffer-position buf)) 15)))
          (if (zerop n) (return nil) (emit-nop buf))))
      ;; Emit function label
      (let ((lpos (code-buffer-position buf)))
        (aset fn-label 1 lpos)
        (let ((labels-ht (code-buffer-labels buf)))
          (puthash fn-label labels-ht lpos)))
      ;; Emit prologue
      (emit-function-prologue buf)
      ;; Set up array-based position labels (bypass hash table)
      (setq *td-label-array* (make-array len))
      ;; Zero-initialize: make-array doesn't clear memory on bare metal
      (let ((zi 0))
        (loop
          (when (>= zi len) (return nil))
          (let ((dummy (aset *td-label-array* zi 0)))
            dummy)
          (setq zi (+ zi 1))))
      (setq *td-label-base* offset)
      ;; Pre-scan branch targets
      (scan-branch-targets state)
      ;; Translate instructions
      (td-translate-fn-body state))))

;;; Override translate-mvm-to-x64 (last-defun-wins)
(defun translate-mvm-to-x64 (bytecode function-table)
  (let ((buf (make-code-buffer)))
    (let ((n-functions (length function-table)))
      (print-dec n-functions) (write-char-serial 10)
      (let ((fn-labels (make-array n-functions)))
        (let ((fn-map (make-hash-table))
              (fn-offset-to-label (make-hash-table)))
          ;; Create global array for fn-offset-to-label (bypass hash table)
          ;; #252: allocated ONCE, at the top of build-image-cross while the
          ;; heap is still nearly empty -- at 80 MB it is far larger than any
          ;; guard band, so allocated here (after the bytecode, the fn table
          ;; and everything else) its gc-check fires only after it has run
          ;; that far past the limit into to-space.
          ;; #252: a HASH TABLE keyed by function offset, not a bytecode-sized
          ;; array -- the array was 80 MB of live heap for 6041 entries, and on
          ;; Gen1 (254 MB semispaces) it left so little room between
          ;; collections that hop 2 thrashed in the collector at fn ~2200.
          (setq *td-fn-label-array* (make-hash-table))
          (let ((rest-ft function-table)
                (i 0))
            (loop
              (when (>= i n-functions) (return nil))
              (let ((entry (car rest-ft)))
                (let ((name (car entry))
                      (offset (car (cdr entry))))
                  (let ((label (make-label)))
                    (aset fn-labels i label)
                    (puthash name fn-map label)
                    (puthash offset fn-offset-to-label label)
                    (let ((dummy (puthash offset *td-fn-label-array* label)))
                      dummy))))
              (setq rest-ft (cdr rest-ft))
              (setq i (+ i 1))))
          ;; #252: helper labels (see *td-x64-gc-label*).  The GC trampoline is
          ;; wired only if %GC-COLLECT is in the table; the cons-kind-bit
          ;; subroutine only if the kind bitmap is on (kernel-main sets it).
          (let ((gcc (gethash *td-gc-collect-hash* fn-map)))
            (setq *td-x64-gc-label* (if gcc (make-label) nil)))
          (setq *td-x64-hpush-label* (make-label))
          (setq *td-x64-hpop-label* (make-label))
          ;; #252: NIL for now -- YIELD sites then stay NOPs in Gen2, as they were
          ;; in every fixpoint generation before the helper labels were wired.
          ;; With the stub label set, hop 2 ran away (150 MB of allocation per
          ;; collection, forever) while translating a backward BR right after
          ;; a loop's YIELD; isolating which of the two it is comes first.
          (setq *td-x64-yield-label* nil)
          (setq *mcgc-cons-bit-label* (if (mcgc-kind-bitmap-on-p) (make-label) nil))
          (setq *x64-genmul-label* (gethash *td-genmul-hash* fn-map))
          (setq *x64-genadd-label* (gethash *td-genadd-hash* fn-map))
          (setq *x64-gensub-label* (gethash *td-gensub-hash* fn-map))
          (write-char-serial 72) (write-char-serial 76) (write-char-serial 58) ;; HL:<gc> <consbit> <genmul> <genadd> <gensub>
          (print-dec (if *td-x64-gc-label* 1 0)) (write-char-serial 32)
          (print-dec (if *mcgc-cons-bit-label* 1 0)) (write-char-serial 32)
          (print-dec (if *x64-genmul-label* 1 0)) (write-char-serial 32)
          (print-dec (if *x64-genadd-label* 1 0)) (write-char-serial 32)
          (print-dec (if *x64-gensub-label* 1 0)) (write-char-serial 10)
          ;; Translate each function
          (write-char-serial 84) (write-char-serial 10) ;; T
          (let ((ctx (cons buf (cons bytecode fn-offset-to-label))))
            (let ((rest-ft function-table)
                  (i 0))
              (loop
                (when (>= i n-functions) (return nil))
                (let ((entry (car rest-ft)))
                  (let ((offset (car (cdr entry)))
                        (len (car (cdr (cdr entry)))))
                    (let ((fn-label (aref fn-labels i)))
                      (td-translate-one-fn ctx fn-label offset len))))
                (setq rest-ft (cdr rest-ft))
                (setq i (+ i 1))
                (when (or (zerop (mod i 50)) (>= i 5400))
                  (write-char-serial 35) ;; #<i>@<code position>
                  (print-dec i) (write-char-serial 64)
                  (print-dec (code-buffer-position buf))
                  (write-char-serial 10)))))
          ;; #252: helper bodies, exactly the host's tail order (translate-x64
          ;; translate-mvm-to-x64): GC trampoline, cons-kind-bit subroutine,
          ;; handler push/pop, yield-longjmp stub.  HB:<n> traces each step.
          (loop
            (let ((n (logand (+ *x64-native-code-offset* (code-buffer-position buf)) 15)))
              (if (zerop n) (return nil) (emit-nop buf))))
          (write-char-serial 72) (write-char-serial 66) (write-char-serial 58) ;; HB:
          (when *td-x64-gc-label*
            (print-dec 1) (write-char-serial 32)
            (emit-gc-trampoline buf *td-x64-gc-label* (gethash *td-gc-collect-hash* fn-map)))
          (when *mcgc-cons-bit-label*
            (print-dec 2) (write-char-serial 32)
            (emit-mcgc-cons-bit-subroutine buf *mcgc-cons-bit-label*))
          (print-dec 3) (write-char-serial 32)
          (emit-handler-helpers buf *td-x64-hpush-label* *td-x64-hpop-label*)
          (when *td-x64-yield-label*
            (print-dec 4) (write-char-serial 32)
            (emit-yield-longjmp-stub buf *td-x64-yield-label* *td-x64-hpop-label*))
          (print-dec 5) (write-char-serial 10)
          ;; Apply fixups — print count for debug
          (let ((fc 0) (rf (code-buffer-fixups buf)))
            (loop (when (null rf) (return nil))
              (setq fc (+ fc 1)) (setq rf (cdr rf)))
            (write-char-serial 70) (write-char-serial 67) ;; FC
            (write-char-serial 61) (print-dec fc) (write-char-serial 10))
          (fixup-labels buf)
          ;; Post-fixup checksum (uses td-fnv-native — i386-safe)
          (let ((pos (code-buffer-position buf))
                (bytes (code-buffer-bytes buf)))
            (print-dec pos) (write-char-serial 10)
            (let ((fnv (td-fnv-native bytes pos)))
              (write-char-serial 70) (write-char-serial 78) ;; FN
              (write-char-serial 86) (write-char-serial 58) ;; V:
              (print-dec fnv) (write-char-serial 10)))
          (cons buf fn-map))))))

;;; Override emit-label-ref-rel32: bare-metal compatible version
(in-package :modus.asm)
;;; #252 diag: a code-buffer growth is a large late allocation; say so.
(defun %code-buffer-ensure (buf pos)
  (let ((bytes (code-buffer-bytes buf)))
    (let ((cap (length bytes)))
      (when (>= pos cap)
        (write-char-serial 67) (write-char-serial 66) (write-char-serial 33) ;; CB!<pos> <cap>
        (print-dec pos) (write-char-serial 32) (print-dec cap) (write-char-serial 10)
        (let ((new-cap cap))
          (loop (when (> new-cap pos) (return nil)) (setq new-cap (* new-cap 2)))
          (let ((new-bytes (make-array new-cap :element-type (quote (unsigned-byte 8)))))
            (let ((i 0))
              (loop (when (>= i cap) (return nil))
                (aset new-bytes i (aref bytes i))
                (setq i (+ i 1))))
            (set-code-buffer-bytes buf new-bytes)))))))
(defun emit-label-ref-rel32 (buf label)
  (let ((pos (code-buffer-position buf)))
    (let ((inner (cons 4 nil)))
      (let ((mid (cons label inner)))
        (let ((entry (cons pos mid)))
          (let ((old-fixups (code-buffer-fixups buf)))
            (let ((new-fixups (cons entry old-fixups)))
              (set-code-buffer-fixups buf new-fixups)))))))
  (emit-u32 buf 0))

;;; Override emit-call: skip label-p check (broken on AArch64 due to tag mismatch:
;;; MVM compiler bakes tag=9 into bytecodes, AArch64 translator uses tag=2)
;;; In fixpoint, target is ALWAYS a label from translate-instruction.
(defun emit-call (buf target)
  (emit-byte buf #xE8)
  (emit-label-ref-rel32 buf target))

;;; Override emit-jmp: skip label-p check
(defun emit-jmp (buf target)
  (emit-byte buf #xE9)
  (emit-label-ref-rel32 buf target))

;;; Override emit-jcc: skip label-p check, use assoc for condition code lookup
(defun emit-jcc (buf cc target)
  (let ((code (cdr (assoc cc *condition-codes*))))
    (emit-bytes buf #x0F (+ #x80 code))
    (emit-label-ref-rel32 buf target)))

;;; Helper: patch 4 LE bytes at pos in bytes array
;;; Separate function avoids variable-index ASET dest=nil bug
;;; (when ASET is non-last form with dest=nil, value register doesn't load
;;; on AArch64 due to different register allocation vs x64)
(defun fixup-patch-one (bytes pos rel)
  (let ((v0 (logand rel 255)))
    (let ((d0 (aset bytes pos v0)))
      (let ((v1 (logand (ash rel -8) 255)))
        (let ((d1 (aset bytes (+ pos 1) v1)))
          (let ((v2 (logand (ash rel -16) 255)))
            (let ((d2 (aset bytes (+ pos 2) v2)))
              (let ((v3 (logand (ash rel -24) 255)))
                (let ((d3 (aset bytes (+ pos 3) v3)))
                  d3)))))))))

;;; Override fixup-labels: MVM-compatible version (no destructuring-bind/ecase/setf)
(defun fixup-labels (buf)
  (let ((bytes (code-buffer-bytes buf))
        (fixups (code-buffer-fixups buf)))
    (let ((rest-fixups fixups) (fi 0))
      (loop
        (when (null rest-fixups) (return nil))
        (let ((fixup (car rest-fixups)))
          (let ((pos (car fixup))
                (label (car (cdr fixup)))
                (size (car (cdr (cdr fixup)))))
            (let ((target (if (null label) nil (aref label 1))))
              (when (null target)
                (write-char-serial 78) (write-char-serial 85) ;; NU
                (write-char-serial 76) (write-char-serial 64) ;; L@
                (print-dec fi) (write-char-serial 10))
              (when (and target (eql size 4))
                (let ((sum (+ pos size)))
                  (let ((rel (- target sum)))
                    (fixup-patch-one bytes pos rel)))))))
        (setq fi (+ fi 1))
        (when (zerop (mod fi 100))
          (write-char-serial 46)) ;; . every 100 instead of 1000
        (setq rest-fixups (cdr rest-fixups)))))
  buf)


(in-package :modus.mvm)