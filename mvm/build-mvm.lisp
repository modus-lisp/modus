;;;; build-mvm.lisp - Build the MVM compiler driver (Linux x86-64)
;;;;
;;;; Produces /tmp/mvm — a self-hosted compiler that reads MVM-subset Lisp
;;;; source and produces Linux x86-64 ELF binaries.
;;;;
;;;; Usage: sbcl --script mvm/build-mvm.lisp
;;;; Test:  /tmp/mvm hello.lisp        → /tmp/modus-out
;;;;        /tmp/mvm hello.lisp out    → /tmp/out
;;;;
;;;; The mvm binary contains: reader + MVM compiler + x64 translator + ELF builder.
;;;; Source order: repl → prelude → mvm → compiler → x64-asm → translate-x64
;;;;              → adapters → overrides → opcode-init → file-io → elf-builder → driver

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building MVM compiler driver ===~%")

;;; ============================================================
;;; 2. Read source files as text (SBCL-side)
;;; ============================================================

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  "Read a first-party source file as text, parse-checking it first.
   A missing close paren in %format-impl once hid behind the lenient
   mid-build reader for weeks — see ansi-notes.md: SOLVED:
   late-cond-branch."
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    ;; #211: wrap each file so its own (in-package …) cannot leak into the
    ;; next file of the concatenated build blob.  See
    ;; modus.mvm::*build-package-reset-text*.
    (modus.mvm::%build-package-scoped-source (read-file-text path))))

(format t "Reading source files...~%")

(defvar *prelude-source*  (mvm-text "mvm/prelude.lisp"))
(defvar *mvm-source*      (mvm-text "mvm/mvm.lisp"))
(defvar *compiler-source*  (mvm-text "mvm/compiler.lisp"))
(defvar *x64-asm-source*  (mvm-text "mvm/x64-asm.lisp"))
(defvar *translate-x64-source* (mvm-text "mvm/translate-x64.lisp"))

(format t "  prelude: ~D chars~%" (length *prelude-source*))
(format t "  mvm: ~D chars~%" (length *mvm-source*))
(format t "  compiler: ~D chars~%" (length *compiler-source*))
(format t "  x64-asm: ~D chars~%" (length *x64-asm-source*))
(format t "  translate-x64: ~D chars~%" (length *translate-x64-source*))

;;; ============================================================
;;; 3. Preprocess source text (SBCL-side)
;;; Strip &key, replace unsupported constructs for MVM compilation.
;;; Only x64-relevant preprocessing needed.
;;; ============================================================

(format t "Preprocessing source...~%")

;; Strip install-x64-translator and everything after it
;; (install-x64-translator references *target-x86-64* which isn't on bare metal;
;;  disassemble-native uses &key; translate-single-instruction uses target struct)
(let ((marker "(defun install-x64-translator"))
  (let ((pos (search marker *translate-x64-source*)))
    (format t "  Stripping at position ~A (of ~D)~%" pos (length *translate-x64-source*))
    (when pos
      (setf *translate-x64-source*
            ;; #211: re-append the package reset the trim just cut off.
            (concatenate 'string (subseq *translate-x64-source* 0 pos)
                         modus.mvm::*build-package-reset-text*)))))

;; Combine x64-asm + translate-x64 (preprocessed)
(defvar *x64-source-text*
  (concatenate 'string *x64-asm-source* (string #\Newline) *translate-x64-source*))

(format t "  x64 source: ~D chars~%" (length *x64-source-text*))

;;; ============================================================
;;; 4. Generate opcode table init source (SBCL-side)
;;; ============================================================

(format t "Generating opcode table init...~%")

(in-package :modus.mvm)

;; Forward x64-asm bindings
(defparameter *registers* modus.asm::*registers*)

;; Generate init-opcode-entries
(defvar cl-user::*opcode-init-source*
  (let ((ot *opcode-table*)
        (count 0))
    (with-output-to-string (s)
      (format s "(defun init-opcode-entries ()~%")
      (cl:maphash (lambda (code info)
                    (incf count)
                    (let ((operands (opcode-info-operands info)))
                      (format s "  (puthash ~D *opcode-table* (%make-opcode-info ~D ~D "
                              code code (normalize-name (opcode-info-name info)))
                      (if (null operands)
                          (format s "nil")
                          (progn
                            (loop for op in operands
                                  for first = t then nil
                                  do (unless first (format s " "))
                                     (format s "(cons ~D" (normalize-name op)))
                            (format s " nil")
                            (dotimes (j (length operands))
                              (format s ")"))))
                      (format s " nil))~%")))
                  ot)
      (format s ")~%"))))

(format t "  opcode init: ~D chars~%" (length cl-user::*opcode-init-source*))

;; Generate correct opcode-pattern function from opcode table
;; Maps operand specs to pattern numbers used by the bare-metal encode-instruction
(defvar cl-user::*opcode-pattern-source*
  (let ((ot *opcode-table*))
    (flet ((spec-to-pattern (spec)
             (let ((key (mapcar (lambda (s) (intern (symbol-name s) :keyword)) spec)))
               (cond
                 ((null key) 0)                          ;; no operands
                 ((equal key '(:reg)) 1)                 ;; :reg
                 ((equal key '(:reg :reg)) 2)            ;; :reg :reg
                 ((equal key '(:reg :reg :reg)) 3)       ;; :reg :reg :reg
                 ((equal key '(:reg :imm64)) 4)          ;; :reg :u64
                 ((equal key '(:off16)) 5)               ;; :off16
                 ((equal key '(:off32)) 5)               ;; :off32 (same encoding)
                 ((equal key '(:reg :off16)) 6)          ;; :reg :off16
                 ((equal key '(:reg :off32)) 6)          ;; :reg :off32
                 ((equal key '(:imm16)) 7)               ;; :u16
                 ((equal key '(:imm32)) 8)               ;; :u32
                 ((equal key '(:reg :reg :imm8)) 9)      ;; :reg :reg :u8
                 ((equal key '(:reg :imm8 :reg)) 10)     ;; :reg :u8 :reg
                 ((equal key '(:reg :imm16 :imm8)) 11)   ;; :reg :u16 :u8
                 ((equal key '(:imm16 :reg :imm8)) 12)   ;; :u16 :reg :u8
                 ((equal key '(:reg :imm16)) 13)          ;; :reg :u16
                 ((equal key '(:imm16 :reg)) 14)          ;; :u16 :reg
                 ((equal key '(:reg :imm32)) 15)          ;; :reg :u32
                 ((equal key '(:reg :reg :imm8 :imm8)) 9) ;; LDB: same as :reg :reg :u8
                 (t (format t "WARNING: unhandled operand pattern ~S for opcode~%" key) 0)))))
      (with-output-to-string (s)
        (format s "(defun opcode-pattern (op)~%  (cond~%")
        (let ((entries nil))
          (cl:maphash (lambda (code info)
                        (push (cons code (spec-to-pattern (opcode-info-operands info))) entries))
                      ot)
          (setf entries (sort entries #'< :key #'car))
          (dolist (e entries)
            (format s "    ((= op ~D) ~D)~%" (car e) (cdr e))))
        (format s "    (t 0)))~%")))))

(format t "  opcode pattern: ~D chars~%" (length cl-user::*opcode-pattern-source*))

;;; ============================================================
;;; 5. Compiler adapter source (from build-compiler-test.lisp)
;;; Overrides for bare-metal / MVM-subset compilation.
;;; ============================================================

(in-package :cl-user)

;; Read the adapter source by evaluating the defvar from build-compiler-test.lisp
;; This properly handles string escaping
(let* ((raw (mvm-text "mvm/build-compiler-test.lisp"))
       (marker "(defvar *adapter-source* ")
       (start (search marker raw))
       (form-start (+ start (length "(defvar *adapter-source* "))))
  ;; Read the string literal using SBCL's reader
  (defvar *adapter-source*
    (with-input-from-string (s (subseq raw form-start))
      (read s))))

(format t "  adapter source: ~D chars~%" (length *adapter-source*))

;; Append additional overrides needed for the mvm command
(setf *adapter-source*
  (concatenate 'string *adapter-source* "

;;; Override register-mvm-bootstrap-macros to no-op
(defun register-mvm-bootstrap-macros () nil)

;;; ============================================================
;;; Interned symbol support
;;; ============================================================

;;; symbolp function — needed by the SBCL-compiled compiler code (called via CALL,
;;; not the compiler builtin). Checks object subtag #x50.
(defun symbolp (x)
  (if (null x) nil
    (if (fixnump x) nil
      (if (consp x) nil
        (if (stringp x) nil
          (= (obj-subtag x) 80))))))

;;; check-arith-nesting no-op — *arith-ops* defvar isn't initialized on bare metal
(defun check-arith-nesting (op operand) nil)

;;; Override flatten-arith-args — the original receives SBCL symbol 'LOGIOR as op,
;;; but form heads are interned symbol objects. Use normalize-name hash comparison.
(defun flatten-arith-args (op args)
  (let ((op-hash (normalize-name op))
        (result nil))
    (let ((tmp args))
      (loop
        (when (null tmp) (return (nreverse result)))
        (let ((arg (car tmp)))
          (if (and (consp arg)
                   (symbolp (car arg))
                   (= (normalize-name (car arg)) op-hash))
              (let ((inner (flatten-arith-args op (cdr arg))))
                (let ((itmp inner))
                  (loop
                    (when (null itmp) (return nil))
                    (setq result (cons (car itmp) result))
                    (setq itmp (cdr itmp)))))
              (setq result (cons arg result))))
        (setq tmp (cdr tmp))))))

;;; Override normalize-name — extract hash from symbol object slot 0
(defun normalize-name (name-input)
  (if (fixnump name-input)
      name-input
    (if (symbolp name-input)
        (aref name-input 0)
      (if (consp name-input)
          (if (= (car name-input) 9999)
              (compute-name-hash-from-chars (cdr name-input))
            0)
        0))))

;;; Override sym-equal — with interned symbols, eq suffices
(defun sym-equal (a b) (eql a b))

;;; Override mksym in the reader — intern symbols instead of (cons 9999 chars)
(defun mksym (chars)
  (let ((hash (compute-name-hash-from-chars chars)))
    (%intern-symbol hash)))

;;; compute-name-hash-from-chars: dual-FNV-1a on a list of char codes
(defun compute-name-hash-from-chars (chars)
  ;; 16-bit state (403 / 89 / #x9DC5 / #xBEEF) — bit-identical to the 32-bit
  ;; FNV form and fixnum-safe on a 30-bit tower.  See mvm/compiler.lisp.
  (let ((h1 40389)
        (h2 48879)
        (cur chars))
    (loop
      (when (null cur) (return nil))
      (let ((c (logand (car cur) 65535)))
        (setq h1 (logand (* (logxor h1 c) 403) 65535))
        (setq h2 (logand (* (logxor h2 c) 89) 65535)))
      (setq cur (cdr cur)))
    (let ((combined (logior (ash (logand h1 +name-hash-hi-mask+) +name-hash-shift+)
                            (logand h2 +name-hash-lo-mask+))))
      (if (zerop combined) 1 combined))))

;;; Override globals functions to use 0x600000 (Linux BSS)
;;; The build-compiler-test adapter uses 0x380000 which is below Linux load address.
(defun init-globals-table ()
  (let ((ht (make-hash-table)))
    (setf (mem-ref #x600000 :u64) ht)))

(defun symbol-value (name-hash)
  (gethash name-hash (mem-ref #x600000 :u64)))

(defun set-symbol-value (name-hash value)
  (puthash name-hash (mem-ref #x600000 :u64) value)
  value)

;;; Override %make-code-buffer and %make-label for bare metal
;;; The defstruct macro expands (make-code-buffer) to (%make-code-buffer ...)
;;; at SBCL compile time. We override the internal constructor.
;;; code-buffer slots: [bytes(0), labels(1), fixups(2), position(3)]
(defun %make-code-buffer (bytes labels fixups position)
  (let ((buf (make-array 4)))
    (aset buf 0 (make-array 3145728))  ;; 3MB bytes (ignore passed default)
    (aset buf 1 (make-hash-table))     ;; labels
    (aset buf 2 nil)                   ;; fixups
    (aset buf 3 0)                     ;; position
    buf))

;; Also provide make-code-buffer for direct calls
(defun make-code-buffer ()
  (%make-code-buffer nil nil nil 0))

;; label slots: [name(0), position(1)]
(defun %make-label (name position)
  (let ((lab (make-array 2)))
    (aset lab 0 (gensym 0))
    (aset lab 1 nil)
    lab))

(defun make-label ()
  (%make-label 0 nil))

;; translate-state: needed by the translator
;; translate-state slots from translate-x64.lisp defstruct:
;; [mvm-bytes(0), mvm-offset(1), mvm-length(2), current-fn-label(3),
;;  code-buf(4), position-labels(5), fn-offset-to-label(6)]
;; translate-state: [buf(0), mvm-bytes(1), mvm-length(2), mvm-offset(3),
;;                   position-labels(4), function-table(5), gc-label(6)]
;; The %make constructor receives args in defstruct field order:
;; (buf, mvm-bytes, mvm-length, mvm-offset, position-labels, function-table, gc-label)
(defun %make-translate-state (p-buf p-mvm-bytes p-mvm-length p-mvm-offset
                              p-position-labels p-function-table p-gc-label)
  (let ((s (make-array 7)))
    (aset s 0 p-buf)
    (aset s 1 p-mvm-bytes)
    (aset s 2 p-mvm-length)
    (aset s 3 p-mvm-offset)
    (aset s 4 (make-hash-table))
    (aset s 5 p-function-table)
    (aset s 6 p-gc-label)
    s))

;; #211: read this replica in :MODUS.ASM, the package x64-asm.lisp itself
;; declares.  The image interns symbols PER PACKAGE (CLHS 11.1.2), so a
;; register name quoted here must be the SAME symbol reg-info's ASSOC sees in
;; x64-asm.lisp's own *REGISTERS* — MODUS.ASM::RBP, not MODUS.MVM::RBP.  Read
;; in MODUS.MVM this table silently mismatched and the JIT died with
;; Unknown-register RBP, falling back to interpret for EVERY form.
(in-package :modus.asm)
;;; Manual init for *vreg-to-x64* (can't use vector literal on bare metal)
(defun init-vreg-to-x64-manual ()
  (let ((v (make-array 23)))
    (aset v 0 (quote rsi))   (aset v 1 (quote rdi))
    (aset v 2 (quote r8))    (aset v 3 (quote r9))
    (aset v 4 (quote rbx))   (aset v 5 (quote rcx))
    (aset v 6 (quote rdx))   (aset v 7 (quote r10))
    (aset v 8 (quote r11))
    ;; V9-V15: nil (spill)
    ;; V16=VR V17=VA V18=VL V19=VN V20=VSP V21=VFP V22=VPC
    (aset v 16 (quote rax))  (aset v 17 (quote r12))
    (aset v 18 (quote r14))  (aset v 19 (quote r15))
    (aset v 20 (quote rsp))  (aset v 21 (quote rbp))
    (setq *vreg-to-x64* v)))

;;; Manual init for *condition-codes*
(defun init-condition-codes-manual ()
  (let ((cc nil))
    (setq cc (cons (cons (quote g)  15) cc))
    (setq cc (cons (cons (quote le) 14) cc))
    (setq cc (cons (cons (quote ge) 13) cc))
    (setq cc (cons (cons (quote l)  12) cc))
    (setq cc (cons (cons (quote np) 11) cc))
    (setq cc (cons (cons (quote p)  10) cc))
    (setq cc (cons (cons (quote ns) 9) cc))
    (setq cc (cons (cons (quote s)  8) cc))
    (setq cc (cons (cons (quote a)  7) cc))
    (setq cc (cons (cons (quote be) 6) cc))
    (setq cc (cons (cons (quote ne) 5) cc))
    (setq cc (cons (cons (quote e)  4) cc))
    (setq cc (cons (cons (quote ae) 3) cc))
    (setq cc (cons (cons (quote b)  2) cc))
    (setq cc (cons (cons (quote no) 1) cc))
    (setq cc (cons (cons (quote o)  0) cc))
    (setq *condition-codes* cc)))
(in-package :modus.mvm)


;;; Blob reader: uses addresses in Linux BSS area (not bare-metal 0x390000)
;;; 0x610000 = blob-arr, 0x610008 = blob-pos

(defvar *blob-arr* nil)
(defvar *blob-pos* 0)

(defun read-char-input ()
  (let ((pos (mem-ref #x610008 :u64)))
    (let ((arr (mem-ref #x610000 :u64)))
      (if (null arr) 4
        (let ((alen (array-length arr)))
          (if (>= pos alen) 4
            (let ((ch (aref arr pos)))
              (setf (mem-ref #x610008 :u64) (+ pos 1))
              ch)))))))

(defun read-skip-ws ()
  (let ((c (read-char-input)))
    (cond
      ((= c 4) 0)
      ((= c 59)
       (let ((cc (read-char-input)))
         (loop
           (when (= cc 4) (return 0))
           (when (= cc 10) (return (read-skip-ws)))
           (setq cc (read-char-input)))))
      ((is-whitespace c) (read-skip-ws))
      (t c))))

(defun read-all-forms-bare ()
  (let ((forms nil))
    (loop
      (let ((c (read-skip-ws)))
        (when (= c 0) (return (nreverse forms)))
        (let ((result (read-sexp-inner c)))
          (setq forms (cons (car result) forms)))))))

;;; write-char-output: no echo during compilation
(defun write-char-output (c) nil)

;;; Globals table: use 0x600000 (same as prelude.lisp)
(defun init-globals-table ()
  (setf (mem-ref #x600000 :u64) nil))

"))

;;; ============================================================
;;; 6. x64 translator overrides (from fixpoint-common.lisp)
;;; ============================================================

(defvar *translator-override-source*
  (mvm-text "mvm/fixpoint-common.lisp"))

;;; ============================================================
;;; 7. File I/O + ELF builder + Driver source
;;; ============================================================

(defvar *driver-source* "

;;; ============================================================
;;; Linux syscall wrappers
;;; ============================================================

(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))

(defun sys-open-read (path-sap)
  (let ((p path-sap))
    (syscall3 2 (sap-address p) 0 0)))

(defun sys-read (fd buf-sap len)
  (let ((f fd))
    (let ((b buf-sap))
      (let ((l len))
        (syscall3 0 f (sap-address b) l)))))

(defun sys-write-buf (fd buf-sap len)
  (let ((f fd))
    (let ((b buf-sap))
      (let ((l len))
        (syscall3 1 f (sap-address b) l)))))

(defun sys-close (fd)
  (let ((f fd))
    (syscall3 3 f 0 0)))

;; Open for writing (O_WRONLY|O_CREAT|O_TRUNC = 0x241, mode 0755 = 493)
(defun sys-open-write (path-sap)
  (let ((p path-sap))
    (syscall3 2 (sap-address p) 577 493)))

;;; ============================================================
;;; SAP helpers (System Area Pointer)
;;; ============================================================

(defun sys-argc () (mem-ref #x10000000 :u32))

(defun argv1-sap ()
  (make-sap-raw (mem-ref #x10000018 :u64)))

(defun argv2-sap ()
  (make-sap-raw (mem-ref #x10000020 :u64)))

;; I/O buffer SAP near end of heap
(defun io-buf-sap ()
  (make-sap #x1DF00000))

;;; ============================================================
;;; Print helpers for diagnostics
;;; ============================================================

(defun print-str (chars)
  (if (null chars) nil
    (progn
      (write-char-serial (car chars))
      (print-str (cdr chars)))))

(defun print-nl ()
  (write-char-serial 10))

(defun print-dec (n)
  (if (< n 0)
      (progn (write-char-serial 45) (print-dec-pos (- 0 n)))
    (if (= n 0) (write-char-serial 48)
      (print-dec-pos n))))

(defun print-dec-pos (n)
  (when (> n 9) (print-dec-pos (truncate n 10)))
  (write-char-serial (+ 48 (mod n 10))))

;;; ============================================================
;;; Read file into array
;;; ============================================================

(defun read-file-to-array (path-sap)
  (let ((fd (sys-open-read path-sap)))
    (if (< fd 0)
        nil
        (let ((buf (io-buf-sap)))
          (let ((n (sys-read fd buf 2097152)))
            (sys-close fd)
            (if (<= n 0)
                nil
                (let ((arr (make-array n))
                      (i 0))
                  (loop
                    (when (>= i n) (return arr))
                    (aset arr i (mem-ref (+ (sap-address buf) i) :u8))
                    (setq i (+ i 1))))))))))

;;; ============================================================
;;; Write array to file
;;; ============================================================

(defun write-array-to-file (path-sap arr len)
  (let ((fd (sys-open-write path-sap)))
    (if (< fd 0)
        (progn
          (write-char-serial 69) ;; E
          (sys-exit 1))
        (let ((buf (io-buf-sap))
              (i 0))
          (loop
            (when (>= i len) (return nil))
            (setf (mem-ref (+ (sap-address buf) i) :u8) (aref arr i))
            (setq i (+ i 1)))
          (sys-write-buf fd buf len)
          (sys-close fd)))))

;;; ============================================================
;;; ELF64 builder (Linux x86-64)
;;; Builds complete ELF binary in an array.
;;; ============================================================

;; Pre-computed Linux x64 boot stub bytes (set up heap, argv, registers)
;; Initialized by init-boot-stub (generated at SBCL build time).

(defun elf-emit-byte (arr pos val)
  (aset arr pos (logand val 255))
  (+ pos 1))

(defun elf-emit-u16 (arr pos val)
  (aset arr pos (logand val 255))
  (aset arr (+ pos 1) (logand (ash val -8) 255))
  (+ pos 2))

(defun elf-emit-u32 (arr pos val)
  (aset arr pos (logand val 255))
  (aset arr (+ pos 1) (logand (ash val -8) 255))
  (aset arr (+ pos 2) (logand (ash val -16) 255))
  (aset arr (+ pos 3) (logand (ash val -24) 255))
  (+ pos 4))

(defun elf-emit-u64 (arr pos val)
  (let ((p (elf-emit-u32 arr pos (logand val 4294967295))))
    (elf-emit-u32 arr p (logand (ash val -32) 4294967295))))

(defun build-elf-binary (native-code native-len km-offset)
  ;; ELF header (64) + program header (56) + boot stub + JMP rel32 (5) + native code
  (let ((ehdr-size 64))
    (let ((phdr-size 56))
      (let ((hdr-total (+ ehdr-size phdr-size)))
        (let ((boot-len *boot-stub-len*))
          (let ((jmp-size 5))
            (let ((total-size (+ hdr-total (+ boot-len (+ jmp-size native-len)))))
              (let ((load-addr #x400000))
                (let ((entry-point (+ load-addr hdr-total)))
                  (let ((arr (make-array total-size))
                        (pos 0))
                    ;; ELF header
                    (setq pos (elf-emit-byte arr pos #x7F))
                    (setq pos (elf-emit-byte arr pos 69))  ;; E
                    (setq pos (elf-emit-byte arr pos 76))  ;; L
                    (setq pos (elf-emit-byte arr pos 70))  ;; F
                    (setq pos (elf-emit-byte arr pos 2))   ;; ELFCLASS64
                    (setq pos (elf-emit-byte arr pos 1))   ;; ELFDATA2LSB
                    (setq pos (elf-emit-byte arr pos 1))   ;; EV_CURRENT
                    (setq pos (elf-emit-byte arr pos 0))   ;; ELFOSABI_NONE
                    ;; padding (8 bytes)
                    (let ((pi 0))
                      (loop
                        (when (>= pi 8) (return nil))
                        (setq pos (elf-emit-byte arr pos 0))
                        (setq pi (+ pi 1))))
                    (setq pos (elf-emit-u16 arr pos 2))    ;; ET_EXEC
                    (setq pos (elf-emit-u16 arr pos 62))   ;; EM_X86_64
                    (setq pos (elf-emit-u32 arr pos 1))    ;; e_version
                    (setq pos (elf-emit-u64 arr pos entry-point))
                    (setq pos (elf-emit-u64 arr pos ehdr-size))  ;; e_phoff
                    (setq pos (elf-emit-u64 arr pos 0))    ;; e_shoff
                    (setq pos (elf-emit-u32 arr pos 0))    ;; e_flags
                    (setq pos (elf-emit-u16 arr pos ehdr-size))
                    (setq pos (elf-emit-u16 arr pos phdr-size))
                    (setq pos (elf-emit-u16 arr pos 1))    ;; e_phnum
                    (setq pos (elf-emit-u16 arr pos 0))
                    (setq pos (elf-emit-u16 arr pos 0))
                    (setq pos (elf-emit-u16 arr pos 0))
                    ;; Program header
                    (setq pos (elf-emit-u32 arr pos 1))    ;; PT_LOAD
                    (setq pos (elf-emit-u32 arr pos 7))    ;; PF_R|PF_W|PF_X
                    (setq pos (elf-emit-u64 arr pos 0))    ;; p_offset
                    (setq pos (elf-emit-u64 arr pos load-addr))
                    (setq pos (elf-emit-u64 arr pos load-addr))
                    (setq pos (elf-emit-u64 arr pos total-size))
                    (setq pos (elf-emit-u64 arr pos (+ total-size #x0E000000))) ;; p_memsz
                    (setq pos (elf-emit-u64 arr pos #x200000))
                    ;; Boot stub
                    (let ((bi 0))
                      (loop
                        (when (>= bi boot-len) (return nil))
                        (setq pos (elf-emit-byte arr pos (aref *boot-stub* bi)))
                        (setq bi (+ bi 1))))
                    ;; JMP rel32 to kernel-main
                    (setq pos (elf-emit-byte arr pos #xE9))
                    (setq pos (elf-emit-u32 arr pos km-offset))
                    ;; Native code
                    (let ((ni 0))
                      (loop
                        (when (>= ni native-len) (return nil))
                        (setq pos (elf-emit-byte arr pos (aref native-code ni)))
                        (setq ni (+ ni 1))))
                    (cons arr total-size)))))))))))

;;; Diagnostic wrapper for translate-mvm-to-x64
;;; This overrides fixpoint-common.lisp's version (last-defun-wins)
(defun translate-mvm-to-x64 (bytecode function-table)
  (write-char-serial 84) ;; T
  (let ((buf (make-code-buffer)))
    (write-char-serial 49) ;; 1
    (let ((n-fns (length function-table)))
      (let ((fn-labels (make-array n-fns)))
        (write-char-serial 50) ;; 2
        (let ((fn-map (make-hash-table))
              (fn-offset-to-label (make-hash-table)))
          (setq *td-fn-label-array* (make-array (array-length bytecode)))
          ;; Setup function labels — key by BOTH name hash AND bytecode offset
          (let ((rest-ft function-table)
                (fi 0))
            (loop
              (when (>= fi n-fns) (return nil))
              (let ((entry (car rest-ft)))
                (let ((name (car entry))
                      (offset (cadr entry)))
                  (let ((label (make-label)))
                    (let ((d0 (aset fn-labels fi label)))
                      (puthash name fn-map label)
                      (puthash offset fn-offset-to-label label)
                      d0))))
              (setq rest-ft (cdr rest-ft))
              (setq fi (+ fi 1))))
          ;; Translate each function using td-translate-one-fn
          ;; ctx = (buf . (bytecode . fn-offset-to-label))
          (let ((ctx (cons buf (cons bytecode fn-offset-to-label))))
            (let ((rest-ft function-table)
                  (fi 0))
              (loop
                (when (>= fi n-fns) (return nil))
                (let ((entry (car rest-ft)))
                  (let ((offset (cadr entry))
                        (len (caddr entry)))
                    (let ((fn-label (aref fn-labels fi)))
                      (write-char-serial 70) ;; F
                      (td-translate-one-fn ctx fn-label offset len)
                      (write-char-serial 101)))) ;; e
                (setq rest-ft (cdr rest-ft))
                (setq fi (+ fi 1)))))
          (write-char-serial 88) ;; X
          (fixup-labels buf)
          (write-char-serial 90) ;; Z
          (let ((pos (code-buffer-position buf)))
            (let ((bytes (code-buffer-bytes buf)))
              (cons bytes pos))))))))

;;; Override td-translate-one-fn
;;; Skips zero-fill loop (Linux mmap returns zeroed pages) and inlines prologue.
(defun td-translate-one-fn (ctx fn-label offset len)
  (let ((buf (car ctx))
        (bytecode (car (cdr ctx)))
        (fn-ot (cdr (cdr ctx))))
    (let ((state (make-translate-state)))
      (set-translate-state-buf state buf)
      (set-translate-state-mvm-bytes state bytecode)
      (set-translate-state-mvm-length state len)
      (set-translate-state-mvm-offset state offset)
      (set-translate-state-function-table state fn-ot)
      ;; Emit function label
      (let ((lpos (code-buffer-position buf)))
        (aset fn-label 1 lpos)
        (let ((labels-ht (code-buffer-labels buf)))
          (puthash fn-label labels-ht lpos)))
      ;; Emit prologue (inlined — avoids extra call overhead)
      (emit-push buf (quote rbp))
      (emit-mov-reg-reg buf (quote rbp) (quote rsp))
      (emit-sub-reg-imm buf (quote rsp) 1120)
      (emit-mov-mem-reg buf (quote rbp) (quote rbx) -8)
      ;; Set up position labels array (no zero-fill needed — mmap gives zeroed pages)
      (let ((la (make-array len)))
        (setq *td-label-array* la)
        (setq *td-label-base* offset))
      ;; Pre-scan branch targets + translate
      (scan-branch-targets state)
      (td-translate-fn-body state))))

;;; Default output path initialized by init-default-out-path

;;; ============================================================
;;; kernel-main: the MVM compiler driver
;;; ============================================================

(defun kernel-main ()
  ;; Banner
  (write-char-serial 77)  ;; M
  (write-char-serial 86)  ;; V
  (write-char-serial 77)  ;; M
  (write-char-serial 32)
  (write-char-serial 118) ;; v
  (write-char-serial 48)  ;; 0
  (print-nl)

  ;; Initialize globals table first (needed by setq/defvar)
  (init-globals-table)
  ;; Initialize symbol intern table (needed by reader and quoted symbols)
  (init-symbol-table)
  ;; Initialize boot stub and default output path
  (init-boot-stub)
  (init-default-out-path)

  ;; Check argc
  (let ((argc (sys-argc)))
    (when (< argc 2)
      (write-char-serial 85)  ;; U
      (write-char-serial 115) ;; s
      (write-char-serial 97)  ;; a
      (write-char-serial 103) ;; g
      (write-char-serial 101) ;; e
      (write-char-serial 58)  ;; :
      (write-char-serial 32)
      (write-char-serial 109) ;; m
      (write-char-serial 118) ;; v
      (write-char-serial 109) ;; m
      (write-char-serial 32)
      (write-char-serial 60)  ;; <
      (write-char-serial 115) ;; s
      (write-char-serial 114) ;; r
      (write-char-serial 99)  ;; c
      (write-char-serial 62)  ;; >
      (print-nl)
      (sys-exit 1)))

  ;; Read source file
  (write-char-serial 82)  ;; R
  (let ((src-arr (read-file-to-array (argv1-sap))))
    (when (null src-arr)
      (write-char-serial 69)  ;; E (error)
      (write-char-serial 114) ;; r
      (print-nl)
      (sys-exit 1))

    (write-char-serial 49)  ;; 1 — file read
    (print-dec (array-length src-arr))
    (print-nl)

    ;; Set up blob reader
    (setf (mem-ref #x610000 :u64) src-arr)   ;; 0x390000 = blob-arr
    (setf (mem-ref #x610008 :u64) 0)          ;; 0x390008 = blob-pos

    ;; Parse forms
    (let ((forms (read-all-forms-bare)))
      (write-char-serial 50)  ;; 2 — forms parsed
      (let ((nf 0))
        (let ((tmp forms))
          (loop
            (when (null tmp) (return nil))
            (setq nf (+ nf 1))
            (setq tmp (cdr tmp))))
        (print-dec nf))
      (print-nl)

      ;; Initialize compiler
      (write-char-serial 103) ;; g
      (init-globals-table)
      (write-char-serial 99)  ;; c
      (init-compiler-globals)
      (write-char-serial 109) ;; m
      (register-mvm-bootstrap-macros)
      (write-char-serial 111) ;; o
      ;; Don't call init-*opcode-table* — it uses (make-hash-table :test 'eql)
      ;; which passes keyword args to make-hash-table (0-arg on bare metal)
      (setq *opcode-table* (make-hash-table))
      (write-char-serial 101) ;; e
      (init-opcode-entries)
      (write-char-serial 108) ;; l
      (setq *mvm-label-counter* 0)
      (write-char-serial 51)  ;; 3 — compiler initialized
      (print-nl)

      ;; Compile
      (let ((module (mvm-compile-all forms)))
        (write-char-serial 52)  ;; 4 — compiled
        (let ((bc (aref module 0)))
          (print-dec (cdr bc)))
        (print-nl)

        ;; Trim bytecode array to actual used size
        (let ((bc-data (aref module 0)))
          (let ((full-arr (car bc-data)))
            (let ((actual-len (cdr bc-data)))
              (let ((trimmed (make-array actual-len))
                    (ti 0))
                (loop
                  (when (>= ti actual-len) (return nil))
                  (aset trimmed ti (aref full-arr ti))
                  (setq ti (+ ti 1)))
                (aset module 0 (cons trimmed actual-len))))))

        ;; Initialize x64 translator
        ;; Can't use init-* thunks — they use (vector ...) and keyword args
        ;; which don't work on bare metal. Initialize manually.
        (setq *x64-linux-mode* 1)  ;; output uses Linux syscalls
        ;; *vreg-names* — not needed for translation, skip
        ;; *target-x86-64* — not needed (fixpoint overrides don't use it)
        ;; *condition-codes* — needed by x64-asm
        (init-condition-codes-manual)
        ;; *vreg-to-x64* — needed by translator
        (init-vreg-to-x64-manual)
        (write-char-serial 53)  ;; 5 — translator initialized
        (print-nl)

        ;; Translate to x64
        (let ((bc-data (aref module 0)))
          (let ((bc-arr (car bc-data)))
            (let ((bc-len (cdr bc-data)))
              (let ((fn-table (aref module 1)))
                ;; Build function table list in (name-hash offset length) format
                ;; fn-table entries are arrays: [name, param-count, bc-offset, bc-length]
                (let ((ft-list nil))
                  (let ((tmp fn-table))
                    (loop
                      (when (null tmp) (return nil))
                      (let ((entry (car tmp)))
                        (let ((name-hash (compute-name-hash (aref entry 0))))
                          (let ((bc-off (aref entry 2)))
                            (let ((bc-sz (aref entry 3)))
                              (setq ft-list (cons (cons name-hash
                                                        (cons bc-off
                                                              (cons bc-sz nil)))
                                                  ft-list))))))
                      (setq tmp (cdr tmp))))
                  (let ((ft (nreverse ft-list)))

                    ;; Set up label arrays for translator
                    (setq *td-label-array* (make-array (+ bc-len 256)))
                    (setq *td-label-base* 0)
                    (setq *td-fn-label-array* (make-array (+ bc-len 256)))

                    ;; Call translator (2 args: bytecode-array, fn-table)
                    (let ((result (translate-mvm-to-x64 bc-arr ft)))
                      (write-char-serial 54)  ;; 6 — translated
                      (let ((native-arr (car result)))
                        (let ((native-len (cdr result)))
                          (print-dec native-len)
                          (print-nl)

                          ;; Find kernel-main offset
                          (let ((km-hash (compute-name-hash-bare 75 69 82 78 69 76 45 77 65 73 78)))
                            (let ((km-offset 0))
                              (let ((tmp2 ft))
                                (loop
                                  (when (null tmp2) (return nil))
                                  (let ((entry (car tmp2)))
                                    (when (= (car entry) km-hash)
                                      (setq km-offset (cadr entry))))
                                  (setq tmp2 (cdr tmp2))))

                              ;; Build ELF
                              (write-char-serial 55)  ;; 7 — building ELF
                              (print-nl)
                              (let ((elf (build-elf-binary native-arr native-len km-offset)))
                                (let ((elf-arr (car elf)))
                                  (let ((elf-len (cdr elf)))
                                    ;; Write output
                                    (let ((out-path (if (>= (sys-argc) 3)
                                                        (argv2-sap)
                                                        *default-out-path*)))
                                      (write-array-to-file out-path elf-arr elf-len)
                                      (write-char-serial 56)  ;; 8 — done
                                      (print-dec elf-len)
                                      (print-nl)
                                      (sys-exit 0))))))))))))))))))))

;;; Bare-metal name hash (no strings on bare metal)
;;; Hash of KERNEL-MAIN using FNV-1a style
(defun compute-name-hash-bare (c0 c1 c2 c3 c4 c5 c6 c7 c8 c9)
  ;; 16-bit state — see compute-name-hash-from-chars above.
  (let ((h1 40389))
    (let ((h2 48879))
      (setq h1 (logand (* (logxor h1 (logand c0 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c0 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c1 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c1 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c2 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c2 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c3 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c3 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c4 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c4 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c5 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c5 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c6 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c6 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c7 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c7 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c8 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c8 65535)) 89) 65535))
      (setq h1 (logand (* (logxor h1 (logand c9 65535)) 403) 65535))
      (setq h2 (logand (* (logxor h2 (logand c9 65535)) 89) 65535))
      (let ((combined (logior (ash (logand h1 +name-hash-hi-mask+) +name-hash-shift+)
                              (logand h2 +name-hash-lo-mask+))))
        (if (zerop combined) 1 combined)))))

")

;;; ============================================================
;;; 8. Pre-compute boot stub bytes (SBCL-side)
;;; ============================================================

;; Load Linux boot descriptor (must be in cl-user for mvm-load access)
(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

(format t "Pre-computing boot stub...~%")

;; Override linux-x64-boot-descriptor to include nil page mmap
(defun mvm-linux-x64-entry (buf)
  "Emit Linux x64 entry stub with NIL page mmap."
  (emit-linux-x64-entry buf)
  ;; Add mmap for NIL page: car/cdr nil must not segfault
  ;; NIL=0xDEAD0001, car accesses [0xDEAD0001 - 1] = 0xDEAD0000
  ;; mmap(0xDEAD0000, 4096, PROT_READ, MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0)
  ;; MUST use movabs (64-bit immediate) — mov rdi,imm32 sign-extends!
  (emit-bytes buf #x48 #xBF #x00 #x00 #xAD #xDE #x00 #x00 #x00 #x00)
  (emit-bytes buf #x48 #xC7 #xC6 #x00 #x10 #x00 #x00) ; mov rsi, 4096
  (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, PROT_READ|PROT_WRITE
  (emit-bytes buf #x49 #xC7 #xC2 #x32 #x00 #x00 #x00) ; mov r10, flags
  (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
  (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
  (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, SYS_mmap
  (emit-bytes buf #x0F #x05)                             ; syscall
  ;; Fill nil page with NIL (0xDEAD0001) values using rep stosq
  ;; After mmap: RAX = 0xDEAD0000 (mmap result)
  (emit-bytes buf #x48 #x89 #xC7)                       ; mov rdi, rax (dest for stosq)
  (emit-bytes buf #x48 #xC7 #xC1 #x00 #x02 #x00 #x00) ; mov rcx, 512 (4096/8 qwords)
  (emit-bytes buf #x4C #x89 #xF8)                       ; mov rax, r15 (rax = NIL = 0xDEAD0001)
  (emit-bytes buf #xF3 #x48 #xAB))                       ; rep stosq

;; Redefine linux-x64-boot-descriptor to use our entry with nil page mmap
(defun linux-x64-boot-descriptor ()
  (list :arch :x86-64
        :entry-fn #'mvm-linux-x64-entry
        :load-addr +linux-x64-load-addr+
        :elf-format :linux-x64))

;; Compute boot stub size for the pre-computed version
(let* ((boot-buf (make-mvm-buffer)))
  (mvm-linux-x64-entry boot-buf)
  (let* ((boot-bytes (mvm-buffer-used-bytes boot-buf))
         (boot-len (length boot-bytes)))
    (format t "  Boot stub: ~D bytes~%" boot-len)

    ;; Generate boot stub initializer source
    (defvar cl-user::*boot-stub-init-source*
      (with-output-to-string (s)
        (format s "(defun init-boot-stub ()~%")
        (format s "  (setq *boot-stub* (make-array ~D))~%" boot-len)
        (format s "  (setq *boot-stub-len* ~D)~%" boot-len)
        (loop for b across boot-bytes
              for i from 0
              do (format s "  (aset *boot-stub* ~D ~D)~%" i b))
        (format s ")~%")))))

;;; ============================================================
;;; 9. Pre-compute default output path as SAP init
;;; ============================================================

;; Default output path: /tmp/modus-out (as byte array init)
(defvar cl-user::*default-path-init-source*
  (let ((path "/tmp/modus-out"))
    (with-output-to-string (s)
      (format s "(defun init-default-out-path ()~%")
      (format s "  (setq *default-out-path* (make-sap #x1DE00000))~%")
      ;; Write path bytes at the SAP address
      (loop for c across path
            for i from 0
            do (format s "  (setf (mem-ref (+ #x1DE00000 ~D) :u8) ~D)~%" i (char-code c)))
      ;; Null terminator
      (format s "  (setf (mem-ref (+ #x1DE00000 ~D) :u8) 0))~%" (length path)))))

;;; ============================================================
;;; 10. Assemble full source
;;; ============================================================

(in-package :cl-user)

(format t "~%Assembling full source...~%")

(defvar *full-source*
  (concatenate 'string
    ;; 1. REPL source (reader, I/O, eval, print)
    modus.mvm::*repl-source*
    (string #\Newline)
    ;; 2. Prelude
    *prelude-source*
    (string #\Newline)
    ;; 3. MVM ISA
    *mvm-source*
    (string #\Newline)
    ;; 4. Compiler
    *compiler-source*
    (string #\Newline)
    ;; 5. x64 ASM + translator (preprocessed)
    *x64-source-text*
    (string #\Newline)
    ;; 6. Compiler adapters (last-defun-wins overrides)
    *adapter-source*
    (string #\Newline)
    ;; 7. Translator overrides (fixpoint-common.lisp)
    *translator-override-source*
    (string #\Newline)
    ;; 8. Opcode table init + correct opcode-pattern
    *opcode-init-source*
    (string #\Newline)
    *opcode-pattern-source*
    (string #\Newline)
    ;; 9. Defvar declarations for boot stub and default path
    ;; (must come before init functions that setq them)
    "(defvar *boot-stub* nil)
(defvar *boot-stub-len* 0)
(defvar *default-out-path* nil)
(defvar *x64-linux-mode* nil)
(defvar *td-label-array* nil)
(defvar *td-label-base* 0)
(defvar *td-fn-label-array* nil)
"
    (string #\Newline)
    ;; 10. Boot stub init
    *boot-stub-init-source*
    (string #\Newline)
    ;; 11. Default path init
    *default-path-init-source*
    (string #\Newline)
    ;; 12. Driver (file I/O, ELF builder, kernel-main LAST)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

;;; ============================================================
;;; 11. Build Linux ELF via MVM pipeline
;;; ============================================================

(in-package :modus.mvm)

;; Install x64 translator in Linux mode
(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)

;; Load Linux boot descriptor (already loaded above)

(format t "~%Compiling MVM binary (~D chars)...~%" (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64 :source-text cl-user::*full-source*)))
  (let ((path "/tmp/mvm"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "~%Test with:~%")
    (format t "  echo '(defun kernel-main () (write-char-serial 72) (sys-exit 0))' > /tmp/test.lisp~%")
    (format t "  /tmp/mvm /tmp/test.lisp~%")
    (format t "  /tmp/modus-out~%")))
