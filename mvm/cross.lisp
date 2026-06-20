;;;; cross.lisp - Universal Cross-Compilation for Modus
;;;;
;;;; The endgame: a running Modus instance on any architecture can
;;;; build kernel images for any other architecture.
;;;;
;;;; Pipeline:
;;;;   1. Read embedded source (plain Lisp text)
;;;;   2. Compile through MVM compiler (target-independent)
;;;;   3. Select target descriptor for desired architecture
;;;;   4. Translate MVM bytecode to native code via target translator
;;;;   5. Emit bootable kernel image with target's boot code
;;;;   6. Embed same source text in new image (for self-replication)

(in-package :modus.mvm)

;;; ============================================================
;;; Compilation Module
;;; ============================================================

(defstruct mvm-module
  name              ; module name (keyword)
  bytecode          ; byte vector of MVM bytecode
  function-table    ; list of function-info structs
  constant-table    ; list of constant values
  source-text       ; original source text (for embedding)
  metadata)         ; plist of additional info

(defstruct mvm-function-info
  name              ; function name (symbol or string)
  name-hash         ; hash of function name (for runtime lookup)
  param-count       ; number of parameters
  bytecode-offset   ; offset into module bytecode
  bytecode-length   ; length of this function's bytecode
  native-offset     ; offset in native code (filled by translator)
  native-length)    ; length of native code (filled by translator)

;;; ============================================================
;;; Image Builder
;;; ============================================================

(defstruct kernel-image
  target            ; target descriptor
  boot-code         ; byte vector of boot code (architecture-specific)
  native-code       ; byte vector of translated native code
  constant-data     ; byte vector of constant pool
  source-blob       ; embedded source text (for next-generation compilation)
  symbol-table      ; NFN table data
  gc-metadata       ; GC root information
  image-bytes       ; final assembled image (byte vector)
  entry-point       ; offset of kernel-main in native code
  native-image-offset ; byte offset of native-code start within raw image bytes
                      ; (i.e. (length boot-code) + jmp-size).  Used by ELF wrappers
                      ; to compute correct virtual addresses for the symbol table.
  constant-pool-offset ; byte offset of constant-pool start within raw image
                       ; bytes, after the 16-byte-virtual-address alignment pad.
                       ; Used by apply-li-const-patches to compute pool vaddr.
  metadata)         ; plist of image metadata

;;; ============================================================
;;; Cross-Compilation Pipeline
;;; ============================================================

(defun compile-source-to-module (source-text &key (name :kernel))
  "Phase 1: Compile Lisp source text to an MVM module.
   This is 100% target-independent.
   Delegates to the MVM compiler (compiler.lisp) for the actual
   compilation, then converts the result to an mvm-module for
   the image building pipeline."
  ;; Read source forms with line numbers
  (let* ((forms-and-lines (read-all-forms-with-locations source-text))
         (forms (car forms-and-lines))
         (source-lines (cdr forms-and-lines))
         ;; Compile all forms through the MVM compiler
         (compiled-mod (mvm-compile-all forms :source-lines source-lines))
         ;; Convert to mvm-module for the image pipeline
         (module (compiled-module-to-mvm-module compiled-mod source-text)))
    (setf (mvm-module-name module) name)
    module))

(defun translate-module-to-native (module target &key into-buf)
  "Phase 2: Translate MVM bytecode to native code for TARGET.
   Calls the target's bulk translator with (bytecode function-table),
   extracts native bytes from the architecture-specific buffer,
   and maps native offsets back to mvm-function-info structs.

   When INTO-BUF is an a64-buffer (AArch64 only), the translator appends
   into that buffer instead of creating a fresh one, and we return the
   buffer itself (so the caller can append more — e.g. handler-stack
   helpers, deferred boot-side fixups — before resolving fixups once)."
  (let ((translator (target-translate-fn target))
        (modus.mvm::*aarch64-translate-into-buf* into-buf))
    (unless translator
      (error "No translator installed for target ~A" (target-name target)))
    (let* ((fn-list (mvm-module-function-table module))
           (bytecode (mvm-module-bytecode module))
           ;; Build function table in the format each translator expects
           (fn-table (build-translator-fn-table fn-list target)))
      ;; Call the bulk translator — returns (values buf fn-map)
      ;; fn-map maps function-name → label (x86-64/i386/arm32)
      ;; or bytecode-offset → native-byte-offset (aarch64/riscv/ppc/68k)
      (multiple-value-bind (buf fn-map)
          (let ((table fn-table)
                (max-retries 10))
            (loop for attempt from 1
                  do (handler-case (return (funcall translator bytecode table))
                       (error (e)
                         (format t "  WARN: translator error #~D: ~A~%" attempt e)
                         (when (>= attempt max-retries)
                           ;; Give up — return empty buffer
                           (format t "  WARN: giving up on translator, using partial result~%")
                           (return (values (modus.asm:make-code-buffer) nil)))
                         ;; Remove the failing function by name from table
                         (let ((bad-name (let* ((msg (format nil "~A" e))
                                               (pos (search "'" msg)))
                                           (when pos
                                             (subseq msg (1+ pos)
                                                     (position #\' msg :start (1+ pos)))))))
                           (when bad-name
                             (setf table
                                   (remove-if (lambda (entry)
                                                (and (consp entry)
                                                     (string= (first entry) bad-name)))
                                              table))))))))
        (let ((native-bytes
               ;; When appending into a shared a64-buffer, fixups aren't
               ;; resolved yet — extracting bytes now would yield placeholder
               ;; bytes for unresolved BL/B targets.  The caller resolves
               ;; and slices afterwards.  Return a length-only fake so
               ;; fn-info native-offset proportional fallback math still
               ;; works for arches that need it.
               (if into-buf
                   (make-array 0 :element-type '(unsigned-byte 8))
                   (extract-native-bytes buf target))))
          (if (and fn-map (hash-table-p fn-map))
              ;; Accurate mapping from translator
              (dolist (fn-info fn-list)
                (let* ((name (string (mvm-function-info-name fn-info)))
                       ;; Try name-keyed lookup first (x86-64/i386/arm32)
                       (entry (gethash name fn-map))
                       ;; Fall back to bytecode-offset-keyed (aarch64 etc)
                       (bc-entry (unless entry
                                   (gethash (mvm-function-info-bytecode-offset fn-info)
                                            fn-map))))
                  (let ((pos (cond
                               ((integerp entry) entry)
                               ((and entry (typep entry 'modus.asm::label))
                                (modus.asm:label-position entry))
                               ((integerp bc-entry) bc-entry)
                               (t nil))))
                    (if pos
                        (setf (mvm-function-info-native-offset fn-info) pos)
                        ;; Fallback for functions not in fn-map
                        (let ((total-bc (max 1 (length bytecode)))
                              (total-native (length native-bytes)))
                          (setf (mvm-function-info-native-offset fn-info)
                                (truncate (* (mvm-function-info-bytecode-offset fn-info)
                                             total-native) total-bc)))))
                  ;; Native length: distance to next function or end of code
                  (setf (mvm-function-info-native-length fn-info) 0)))
              ;; Proportional mapping (fallback for architectures without fn-map)
              (let ((total-bc (max 1 (length bytecode)))
                    (total-native (length native-bytes)))
                (dolist (fn-info fn-list)
                  (let* ((bc-off (mvm-function-info-bytecode-offset fn-info))
                         (bc-len (mvm-function-info-bytecode-length fn-info))
                         (native-off (truncate (* bc-off total-native) total-bc))
                         (native-len (truncate (* bc-len total-native) total-bc)))
                    (setf (mvm-function-info-native-offset fn-info) native-off)
                    (setf (mvm-function-info-native-length fn-info) native-len)))))
          native-bytes)))))

(defun build-translator-fn-table (fn-list target)
  "Build a function table in the format the target's translator expects.
   x86-64 and i386 want a list of (name offset length).
   Others want a hash-table of index → bytecode-offset."
  (let ((name (target-name target)))
    (if (member name '(:x86-64 :i386 :arm32 :armv7 :armv7-rpi))
        ;; List of (name offset length)
        (mapcar (lambda (fi)
                  (list (string (mvm-function-info-name fi))
                        (mvm-function-info-bytecode-offset fi)
                        (mvm-function-info-bytecode-length fi)))
                fn-list)
        ;; Hash-table of index → bytecode-offset (riscv64, aarch64, ppc64, ppc32, 68k)
        (let ((ht (make-hash-table)))
          (loop for fi in fn-list
                for i from 0
                do (setf (gethash i ht)
                         (mvm-function-info-bytecode-offset fi)))
          ht))))

(defun extract-native-bytes (buf target)
  "Extract a byte vector from an architecture-specific native code buffer."
  (cond
    ;; If it's already a byte vector, return as-is
    ((and (typep buf 'vector)
          (or (zerop (length buf))
              (integerp (aref buf 0))))
     buf)
    ;; Try known buffer types by target name
    (t
     (let ((name (target-name target)))
       (handler-case
           (ecase name
             (:riscv64
              ;; rv-buffer has bytes slot with position tracking fill
              (rv-buffer-to-bytes buf))
             (:aarch64  (a64-buffer-to-bytes buf))
             ((:ppc64 :ppc32) (ppc-buffer-to-bytes buf))
             (:i386     (modus.mvm.i386:i386-buffer-to-bytes buf))
             (:68k      (m68k-buffer-to-bytes buf))
             ((:arm32 :armv7 :armv7-rpi) (arm32-buffer-to-bytes buf))
             (:x86-64
              ;; x64 translator uses code-buffer from modus.asm
              (let* ((raw (modus.asm:code-buffer-bytes buf))
                     (len (modus.asm:code-buffer-position buf))
                     (result (make-array len :element-type '(unsigned-byte 8))))
                (dotimes (i len result)
                  (setf (aref result i) (aref raw i))))))
         (error (e)
           (declare (ignore e))
           (make-array 0 :element-type '(unsigned-byte 8))))))))

(defun build-constant-pool (module target)
  "Build the constant pool for the image.
   Lays out compound constants (currently: strings) as full heap objects
   so that LI-CONST can load tagged pointers into them directly.
   Returns (values pool-bytes addr-table) where ADDR-TABLE is a vector
   indexed by constant-pool index, holding a POOL-RELATIVE tagged offset
   (with object/cons tag already OR'd in).  Image-assembly adds the pool's
   image-relative base to each entry to compute the final tagged address
   used when patching LI-CONST instructions in native code.

   Currently only STRING constants are laid out as real heap objects;
   other compound types keep falling through compile-quote's inline
   emit path and never reach the constant table.  Primitives also do
   not reach here — compile-quote handles them directly.  Any constant
   that does land here without a heap-object layout gets a single zero
   word and addr-table[idx] = 0 (LI-CONST will load tag-0, which the
   patch step leaves unmodified — caller code must not rely on it)."
  (let* ((buf (make-mvm-buffer))
         (word-size (target-word-size target))
         (constants (mvm-module-constant-table module))
         (n (length constants))
         (addr-table (make-array n :initial-element 0)))
    (loop for constant in constants
          for idx from 0
          do (typecase constant
               (string
                ;; Each heap object is 16-byte aligned at its header.
                (loop while (/= 0 (mod (mvm-buffer-position buf) 16))
                      do (mvm-emit-byte buf 0))
                (let ((obj-offset (mvm-buffer-position buf))
                      (len (length constant)))
                  (cond
                    ((= word-size 8)
                     ;; 64-bit layout: header (8B) | padding (8B) | N tagged char slots (8B each)
                     ;; Header: (count << 8) | subtag-string (#x31).
                     ;; Matches alloc-obj's runtime header format and the
                     ;; (count + 2) * 8 align-16 alloc-size convention.
                     (mvm-emit-u64 buf (logior #x31 (ash len 8)))
                     (mvm-emit-u64 buf 0)
                     (loop for c across constant
                           do (mvm-emit-u64 buf (ash (char-code c) 1)))
                     (loop while (/= 0 (mod (mvm-buffer-position buf) 16))
                           do (mvm-emit-byte buf 0))
                     ;; Tagged offset: object tag (#x09) on the header address.
                     (setf (aref addr-table idx) (logior obj-offset #x09)))
                    (t
                     ;; 32-bit: not yet supported; leave addr=0 and emit 0 word.
                     (mvm-emit-u32 buf 0)))))
               (t
                ;; Non-string constants in the table are unexpected for now —
                ;; primitives are inlined by compile-quote, and other
                ;; compound types haven't been routed through yet.  Emit a
                ;; single zero so subsequent layout doesn't drift if such an
                ;; entry creeps in; addr-table[idx] stays 0.
                (if (= word-size 8)
                    (mvm-emit-u64 buf 0)
                    (mvm-emit-u32 buf 0)))))
    (values (mvm-buffer-used-bytes buf) addr-table)))

(defun build-nfn-table (module target)
  "Build the NFN (Name-to-Function-Number) table.
   Maps name hashes to native code offsets."
  (let ((buf (make-mvm-buffer))
        (word-size (target-word-size target)))
    ;; Table format: [count:word] [hash:word addr:word]*
    (let ((functions (mvm-module-function-table module)))
      (if (= word-size 8)
          (mvm-emit-u64 buf (length functions))
          (mvm-emit-u32 buf (length functions)))
      (dolist (fn-info functions)
        (if (= word-size 8)
            (progn
              (mvm-emit-u64 buf (mvm-function-info-name-hash fn-info))
              (mvm-emit-u64 buf (mvm-function-info-native-offset fn-info)))
            (progn
              (mvm-emit-u32 buf (mvm-function-info-name-hash fn-info))
              (mvm-emit-u32 buf (mvm-function-info-native-offset fn-info))))))
    (mvm-buffer-used-bytes buf)))

(defun embed-source-blob (source-text target)
  "Prepare the source text for embedding in the kernel image.
   The source is stored as plain ASCII text, readable by the
   self-contained reader (Phase 1a)."
  (let ((buf (make-mvm-buffer)))
    ;; Source blob header: [magic:4 | length:4 | text...]
    (mvm-emit-u32 buf #x4D564D53)  ; "MVMS" magic
    (mvm-emit-u32 buf (length source-text))
    (loop for c across source-text
          do (mvm-emit-byte buf (char-code c)))
    ;; Align to word boundary
    (loop while (/= 0 (mod (mvm-buffer-position buf) (target-word-size target)))
          do (mvm-emit-byte buf 0))
    (mvm-buffer-used-bytes buf)))

;;; ============================================================
;;; ELF Wrappers (for QEMU -kernel loading)
;;; ============================================================

(defun emit-elf-be16 (buf val)
  "Emit 16-bit value in big-endian order."
  (mvm-emit-byte buf (logand (ash val -8) #xFF))
  (mvm-emit-byte buf (logand val #xFF)))

(defun emit-elf-be32 (buf val)
  "Emit 32-bit value in big-endian order."
  (mvm-emit-byte buf (logand (ash val -24) #xFF))
  (mvm-emit-byte buf (logand (ash val -16) #xFF))
  (mvm-emit-byte buf (logand (ash val -8) #xFF))
  (mvm-emit-byte buf (logand val #xFF)))

(defun emit-elf-be64 (buf val)
  "Emit 64-bit value in big-endian order."
  (emit-elf-be32 buf (logand (ash val -32) #xFFFFFFFF))
  (emit-elf-be32 buf (logand val #xFFFFFFFF)))

(defun wrap-in-elf32-be (raw-bytes load-addr e-machine)
  "Wrap raw image bytes in a minimal big-endian ELF32 executable.
   The entire raw image is loaded at LOAD-ADDR. Entry point is LOAD-ADDR
   (first byte of raw image = boot code entry stub)."
  (let* ((buf (make-mvm-buffer))
         (ehdr-size 52)
         (phdr-size 32)
         (hdr-total (+ ehdr-size phdr-size))
         (total-size (+ hdr-total (length raw-bytes)))
         (entry-addr (+ load-addr hdr-total)))
    ;; ---- ELF header (52 bytes) ----
    ;; e_ident: magic
    (mvm-emit-byte buf #x7F)
    (mvm-emit-byte buf (char-code #\E))
    (mvm-emit-byte buf (char-code #\L))
    (mvm-emit-byte buf (char-code #\F))
    ;; EI_CLASS=1 (32-bit), EI_DATA=2 (big-endian), EI_VERSION=1
    (mvm-emit-byte buf 1) (mvm-emit-byte buf 2) (mvm-emit-byte buf 1)
    ;; EI_OSABI + padding (9 bytes)
    (dotimes (i 9) (mvm-emit-byte buf 0))
    ;; e_type = ET_EXEC (2)
    (emit-elf-be16 buf 2)
    ;; e_machine
    (emit-elf-be16 buf e-machine)
    ;; e_version = 1
    (emit-elf-be32 buf 1)
    ;; e_entry = entry point (after ELF headers)
    (emit-elf-be32 buf entry-addr)
    ;; e_phoff = 52
    (emit-elf-be32 buf ehdr-size)
    ;; e_shoff = 0
    (emit-elf-be32 buf 0)
    ;; e_flags = 0
    (emit-elf-be32 buf 0)
    ;; e_ehsize = 52
    (emit-elf-be16 buf ehdr-size)
    ;; e_phentsize = 32
    (emit-elf-be16 buf phdr-size)
    ;; e_phnum = 1
    (emit-elf-be16 buf 1)
    ;; e_shentsize=0, e_shnum=0, e_shstrndx=0
    (emit-elf-be16 buf 0)
    (emit-elf-be16 buf 0)
    (emit-elf-be16 buf 0)
    ;; ---- Program header (32 bytes) ----
    ;; p_type = PT_LOAD (1)
    (emit-elf-be32 buf 1)
    ;; p_offset = 0 (load from start of file, including headers)
    (emit-elf-be32 buf 0)
    ;; p_vaddr = load_addr
    (emit-elf-be32 buf load-addr)
    ;; p_paddr = load_addr
    (emit-elf-be32 buf load-addr)
    ;; p_filesz = total file size
    (emit-elf-be32 buf total-size)
    ;; p_memsz = file size + 1MB extra for BSS/stack
    (emit-elf-be32 buf (+ total-size #x100000))
    ;; p_flags = PF_R|PF_W|PF_X (7)
    (emit-elf-be32 buf 7)
    ;; p_align = 4096
    (emit-elf-be32 buf #x1000)
    ;; ---- Raw image data ----
    (loop for b across raw-bytes do (mvm-emit-byte buf b))
    (mvm-buffer-used-bytes buf)))

(defun wrap-in-elf64-be (raw-bytes load-addr e-machine &optional (e-flags 0))
  "Wrap raw image bytes in a minimal big-endian ELF64 executable."
  (let* ((buf (make-mvm-buffer))
         (ehdr-size 64)
         (phdr-size 56)
         (hdr-total (+ ehdr-size phdr-size))
         (total-size (+ hdr-total (length raw-bytes)))
         (entry-addr (+ load-addr hdr-total)))
    ;; ---- ELF header (64 bytes) ----
    (mvm-emit-byte buf #x7F)
    (mvm-emit-byte buf (char-code #\E))
    (mvm-emit-byte buf (char-code #\L))
    (mvm-emit-byte buf (char-code #\F))
    ;; EI_CLASS=2 (64-bit), EI_DATA=2 (big-endian), EI_VERSION=1
    (mvm-emit-byte buf 2) (mvm-emit-byte buf 2) (mvm-emit-byte buf 1)
    (dotimes (i 9) (mvm-emit-byte buf 0))
    ;; e_type = ET_EXEC (2)
    (emit-elf-be16 buf 2)
    ;; e_machine
    (emit-elf-be16 buf e-machine)
    ;; e_version = 1
    (emit-elf-be32 buf 1)
    ;; e_entry (64-bit)
    (emit-elf-be64 buf entry-addr)
    ;; e_phoff = 64
    (emit-elf-be64 buf ehdr-size)
    ;; e_shoff = 0
    (emit-elf-be64 buf 0)
    ;; e_flags (e.g. 2 for PPC64 ELFv2 ABI)
    (emit-elf-be32 buf e-flags)
    ;; e_ehsize = 64
    (emit-elf-be16 buf ehdr-size)
    ;; e_phentsize = 56
    (emit-elf-be16 buf phdr-size)
    ;; e_phnum = 1
    (emit-elf-be16 buf 1)
    ;; e_shentsize=0, e_shnum=0, e_shstrndx=0
    (emit-elf-be16 buf 0)
    (emit-elf-be16 buf 0)
    (emit-elf-be16 buf 0)
    ;; ---- Program header (56 bytes) ----
    ;; p_type = PT_LOAD (1)
    (emit-elf-be32 buf 1)
    ;; p_flags = PF_R|PF_W|PF_X (7) — note: flags at offset 4 in ELF64
    (emit-elf-be32 buf 7)
    ;; p_offset = 0
    (emit-elf-be64 buf 0)
    ;; p_vaddr = load_addr
    (emit-elf-be64 buf load-addr)
    ;; p_paddr = load_addr
    (emit-elf-be64 buf load-addr)
    ;; p_filesz
    (emit-elf-be64 buf total-size)
    ;; p_memsz
    (emit-elf-be64 buf (+ total-size #x100000))
    ;; p_align = 4096
    (emit-elf-be64 buf #x1000)
    ;; ---- Raw image data ----
    (loop for b across raw-bytes do (mvm-emit-byte buf b))
    (mvm-buffer-used-bytes buf)))

;;; ============================================================
;;; Image Assembly
;;; ============================================================

(defun wrap-header-size-for-boot (boot-descriptor)
  "Return the byte size of any wrapper headers prepended to raw-bytes
   when wrapping the image for the given boot descriptor.  This is
   added to load-addr to compute virtual addresses for raw-bytes
   content (boot code, native code, constant pool, etc.).
   Returns 0 for un-wrapped (bare-metal) builds."
  (cond
    ((null boot-descriptor) 0)
    ;; Linux x64 ELF64: ELF header (64) + 1 program header (56) = 120
    ((eq (getf boot-descriptor :elf-format) :linux-x64) 120)
    ;; Linux AArch64 ELF64: same 64+56 layout as x64.
    ((eq (getf boot-descriptor :elf-format) :linux-aarch64) 120)
    ;; UEFI PE wrap: complex, no LI-CONST support yet
    ((getf boot-descriptor :uefi) 0)
    ;; Generic ELF32-be / ELF64-be wraps
    ((and (getf boot-descriptor :elf-machine)
          (= (getf boot-descriptor :elf-class 32) 32))
     (+ 52 32))    ; ELF32 ehdr (52) + 1 phdr (32)
    ((and (getf boot-descriptor :elf-machine)
          (= (getf boot-descriptor :elf-class 32) 64))
     (+ 64 56))    ; ELF64 ehdr (64) + 1 phdr (56)
    (t 0)))

(defun apply-aarch64-fn-addr-patches (raw-bytes image module boot-descriptor)
  "Walk *aarch64-fn-addr-patches* and write the runtime address of each
   target function into the MOVZ + MOVK immediate fields that the
   translator emitted as placeholders.

   Each patch entry is (native-byte-offset . target-bytecode-offset):
   the byte position of the MOVZ in the native code, and the bytecode
   offset of the target function.  We look up the function's
   native-offset (in mvm-function-info), compute its runtime virtual
   address, and write the low 16 bits into the MOVZ imm16 field and
   the high 16 bits into the MOVK imm16 field (4 bytes later).

   AArch64 MOVZ/MOVK encoding has imm16 at bits [20:5] of the 32-bit
   instruction word.  We read the existing instruction word, clear
   bits [20:5], OR in the new imm16, write back."
  (let ((patches (and (boundp '*aarch64-fn-addr-patches*)
                      *aarch64-fn-addr-patches*)))
    (when patches
      (let* ((native-image-offset (or (kernel-image-native-image-offset image) 0))
             (declared-load-addr
              (or (and boot-descriptor (getf boot-descriptor :load-addr)) 0))
             ;; QEMU virt's `-kernel` loads AArch64 binaries at PA
             ;; load-addr + 0x80000 (Linux Image convention).  After
             ;; the boot stub enables MMU with offset mapping, native
             ;; code runs at identity-mapped VAs which equal those PAs.
             ;; So function-address constants must use load-addr + 0x80000.
             (arch (and boot-descriptor (getf boot-descriptor :arch)))
             (elf-fmt (and boot-descriptor (getf boot-descriptor :elf-format)))
             ;; Bare-metal QEMU AArch64 / RPi loads the kernel at
             ;; load-addr+0x80000 (Linux Image convention), so fn-addr
             ;; constants must point at load-addr+0x80000+offset.
             ;; Linux/AArch64 (`:linux-aarch64` elf-format) goes through
             ;; the normal ELF loader — no 0x80000 offset.
             (image-load-offset
              (if (and (member arch '(:aarch64 :rpi))
                       (not (eq elf-fmt :linux-aarch64)))
                  #x80000
                  0))
             ;; ELF wrapping prepends an ehdr+phdr (120 bytes for both
             ;; Linux/x64 and Linux/AArch64) ahead of the raw image
             ;; bytes.  native-image-offset is measured from the start
             ;; of raw-bytes (which doesn't include the ELF header), but
             ;; the runtime sees the file mapped at p_vaddr with file
             ;; offset 0 → raw byte K lives at vaddr load-addr+120+K.
             ;; So fn-addr constants must add the wrap header too.
             (wrap-header (wrap-header-size-for-boot boot-descriptor))
             (load-addr (+ declared-load-addr image-load-offset wrap-header))
             ;; Build bytecode-offset → native-offset lookup.
             (bc-to-native (make-hash-table :test 'eql)))
        (dolist (fn-info (mvm-module-function-table module))
          (when (mvm-function-info-native-offset fn-info)
            (setf (gethash (mvm-function-info-bytecode-offset fn-info) bc-to-native)
                  (mvm-function-info-native-offset fn-info))))
        (dolist (patch patches)
          (let* ((movz-byte-pos (car patch))
                 (target-bc-offset (cdr patch))
                 (target-native-offset (gethash target-bc-offset bc-to-native))
                 (movz-file-pos (+ native-image-offset movz-byte-pos))
                 (movk-file-pos (+ movz-file-pos 4)))
            (when target-native-offset
              ;; OR with +tag-function+ (3) at patch time so the
              ;; loaded value carries the function tag, matching the
              ;; x64 emit-or-reg-imm-3 path.  CALL-IND strips the
              ;; tag via SUB-3 before BLR.  Function entries are
              ;; 16-byte aligned (NOP pad after fn-addr alignment
              ;; loop) so the OR-3 produces a clean tag value.
              (let* ((target-vaddr (logior (+ load-addr
                                              native-image-offset
                                              target-native-offset)
                                           3))
                     (lo16 (logand target-vaddr #xFFFF))
                     (hi16 (logand (ash target-vaddr -16) #xFFFF)))
                (patch-aarch64-mov-imm16 raw-bytes movz-file-pos lo16)
                (patch-aarch64-mov-imm16 raw-bytes movk-file-pos hi16)))))))))

(defun apply-aarch64-code-bounds-patches (raw-bytes image boot-descriptor)
  "Patch the MOVZ+MOVK pairs that emit-aarch64-code-bounds-init left
   as placeholders in the boot stub.  The cross-link writes:
     code_base = load-addr + 0x80000 (image_load_offset for AArch64)
                  + native-image-offset
     code_end  = code_base + native-code-length
   into slots 0x10000160 / 0x10000168, matching the x64 emit-code-
   bounds-init contract that functionp (cl-eval.lisp) consumes."
  (let ((cb-off (and (boundp 'modus.mvm::*aarch64-code-base-patch-offset*)
                     modus.mvm::*aarch64-code-base-patch-offset*))
        (ce-off (and (boundp 'modus.mvm::*aarch64-code-end-patch-offset*)
                     modus.mvm::*aarch64-code-end-patch-offset*)))
    (when (and cb-off ce-off boot-descriptor)
      (let* ((declared-load-addr (or (getf boot-descriptor :load-addr) 0))
             (arch (getf boot-descriptor :arch))
             (elf-fmt (getf boot-descriptor :elf-format))
             (image-load-offset
              (if (and (member arch '(:aarch64 :rpi))
                       (not (eq elf-fmt :linux-aarch64)))
                  #x80000
                  0))
             (wrap-header (wrap-header-size-for-boot boot-descriptor))
             (load-addr (+ declared-load-addr image-load-offset wrap-header))
             (native-image-offset (or (kernel-image-native-image-offset image) 0))
             (native-code-length (length (kernel-image-native-code image)))
             (code-base (+ load-addr native-image-offset))
             (code-end  (+ code-base native-code-length)))
        ;; Each patch site is a MOVZ at offset N (lo16) and a MOVK at
        ;; offset N+4 (hi16 lsl 16) — same convention the fn-addr
        ;; patcher uses.
        (patch-aarch64-mov-imm16 raw-bytes cb-off
                                 (logand code-base #xFFFF))
        (patch-aarch64-mov-imm16 raw-bytes (+ cb-off 4)
                                 (logand (ash code-base -16) #xFFFF))
        (patch-aarch64-mov-imm16 raw-bytes ce-off
                                 (logand code-end #xFFFF))
        (patch-aarch64-mov-imm16 raw-bytes (+ ce-off 4)
                                 (logand (ash code-end -16) #xFFFF))
        ;; Reset for next build.
        (setf modus.mvm::*aarch64-code-base-patch-offset* nil)
        (setf modus.mvm::*aarch64-code-end-patch-offset*  nil)))))

(defun patch-aarch64-mov-imm16 (raw-bytes file-pos imm16)
  "Patch the imm16 field of an AArch64 MOVZ or MOVK instruction at
   FILE-POS in RAW-BYTES.  imm16 lives at bits [20:5] of the 32-bit
   instruction word (little-endian).  We read 4 bytes, clear bits
   [20:5], OR in (imm16 << 5), write back."
  (let* ((b0 (aref raw-bytes file-pos))
         (b1 (aref raw-bytes (+ file-pos 1)))
         (b2 (aref raw-bytes (+ file-pos 2)))
         (b3 (aref raw-bytes (+ file-pos 3)))
         (insn (logior b0 (ash b1 8) (ash b2 16) (ash b3 24)))
         ;; Clear bits [20:5] (16 bits starting at position 5)
         (cleared (logand insn (lognot (ash #xFFFF 5))))
         (patched (logior cleared (ash (logand imm16 #xFFFF) 5))))
    (setf (aref raw-bytes file-pos)       (logand patched #xFF))
    (setf (aref raw-bytes (+ file-pos 1)) (logand (ash patched -8) #xFF))
    (setf (aref raw-bytes (+ file-pos 2)) (logand (ash patched -16) #xFF))
    (setf (aref raw-bytes (+ file-pos 3)) (logand (ash patched -24) #xFF))))

(defun apply-li-const-patches (raw-bytes image module boot-descriptor
                                pool-addr-table native-code)
  "Walk the architecture-specific LI-CONST patch list and write tagged
   constant-pool addresses into the placeholder MOVABS immediates that
   the translator emitted.  The patches were collected during translation
   (one per LI-CONST instruction, holding the native-byte-offset of the
   8-byte immediate field and the pool index it should resolve to).
   We compute the absolute virtual address of each pool slot from the
   final image layout and write it as little-endian into raw-bytes."
  (declare (ignore module))
  (let* ((arch (and boot-descriptor (getf boot-descriptor :arch)))
         (patches (case arch
                    ((:x86-64 :linux-x64)
                     (and (boundp 'modus.mvm.x64::*x64-li-const-patches*)
                          (symbol-value 'modus.mvm.x64::*x64-li-const-patches*)))
                    (t nil))))
    (when patches
      (let* ((native-image-offset (or (kernel-image-native-image-offset image) 0))
             (native-code-length (length native-code))
             ;; The pool may have been padded for 16-byte VADDR alignment;
             ;; use the recorded offset if present.
             (pool-offset-in-raw (or (kernel-image-constant-pool-offset image)
                                     (+ native-image-offset native-code-length)))
             (load-addr (or (getf boot-descriptor :load-addr) 0))
             (wrap-header (wrap-header-size-for-boot boot-descriptor))
             (pool-vaddr (+ load-addr wrap-header pool-offset-in-raw)))
        (dolist (patch patches)
          (let* ((native-pos (car patch))
                 (idx (cdr patch))
                 (file-pos (+ native-image-offset native-pos))
                 (offset (if (and pool-addr-table
                                  (< idx (length pool-addr-table)))
                             (aref pool-addr-table idx)
                             0))
                 (tagged-addr (if (zerop offset) 0 (+ pool-vaddr offset))))
            ;; Little-endian 8-byte write at raw-bytes[file-pos..file-pos+8].
            (dotimes (i 8)
              (setf (aref raw-bytes (+ file-pos i))
                    (logand (ash tagged-addr (* i -8)) #xFF)))))))))

(defun assemble-kernel-image (module target &key boot-descriptor)
  "Assemble a complete bootable kernel image for TARGET."
  (let* ((boot-arch (and boot-descriptor (getf boot-descriptor :arch)))
         (aarch64-unified-p (member boot-arch '(:aarch64 :rpi)))
         ;; Phase 2b unification: for AArch64/RPi we create ONE a64-buffer,
         ;; emit boot into it, then call translate-module-to-native with
         ;; :into-buf to append translated code into the same buffer.
         ;; That lets boot's BL/B-to-kernel-main and translator-side BLs
         ;; (handler-stack helpers from IRQ entries etc.) share one fixup
         ;; pass.  After translation we patch boot's placeholder B with
         ;; the kernel-main offset, resolve all fixups, and slice bytes.
         (aarch64-unified-buf
          (when aarch64-unified-p (modus.mvm::make-a64-buffer)))
         (aarch64-boot-end-instr nil)
         ;; Allocate fresh label IDs for the per-fork handler-stack
         ;; push/pop helpers.  Bound dynamically around the translate
         ;; call so emit-aarch64-handler-helpers knows to emit them
         ;; and so any future trap/boot BL site can a64-add-fixup
         ;; against the same IDs.  At Phase 3(a) the helpers are
         ;; emitted but unreferenced (dead code).
         (aarch64-push-label
          (when aarch64-unified-p (incf modus.mvm::*mvm-label-counter*)))
         (aarch64-pop-label
          (when aarch64-unified-p (incf modus.mvm::*mvm-label-counter*)))
         (aarch64-gc-label
          (when aarch64-unified-p (incf modus.mvm::*mvm-label-counter*)))
         ;; Look up %gc-collect's bytecode-offset from the module's
         ;; function table.  Returns NIL if not present (then the GC
         ;; trampoline emit is skipped and +op-gc-check+ falls back
         ;; to legacy BRK #1 — i.e., no behaviour change from earlier
         ;; builds that didn't include gc.lisp).
         (gc-collect-bc-offset
          (when aarch64-unified-p
            (let ((found nil))
              (dolist (fi (mvm-module-function-table module))
                (when (string-equal (mvm-function-info-name fi) "%GC-COLLECT")
                  (setf found (mvm-function-info-bytecode-offset fi))))
              found)))
         (native-code
          (cond
            (aarch64-unified-p
             ;; Bind handler-stack helper labels for the entire AArch64
             ;; unified emit.  Phase 3(e) lets boot's IRQ entry 4/5
             ;; emit BL fixups against the pop label too, so the
             ;; dynamic binding now wraps entry-fn AND translate-mvm.
             (let ((modus.mvm::*aarch64-handler-push-label* aarch64-push-label)
                   (modus.mvm::*aarch64-handler-pop-label*  aarch64-pop-label)
                   (modus.mvm::*aarch64-gc-trampoline-label* aarch64-gc-label)
                   (modus.mvm::*aarch64-gc-collect-bytecode-offset*
                    gc-collect-bc-offset))
               ;; Phase A: emit boot preamble into the unified buffer first.
               (let ((entry-fn (getf boot-descriptor :entry-fn)))
                 (when entry-fn (funcall entry-fn aarch64-unified-buf)))
               ;; Phase B: remember the position before the B placeholder.
               ;; We emit one instruction-word as a placeholder; assemble's
               ;; final byte concat skips re-emitting JMP/B for AArch64
               ;; unified (the B is inside our unified bytes already).
               (setf aarch64-boot-end-instr
                     (modus.mvm::a64-buffer-position aarch64-unified-buf))
               (modus.mvm::a64-emit aarch64-unified-buf 0)  ; placeholder B
               ;; Phase C: translate into the same buffer.
               (translate-module-to-native module target
                                           :into-buf aarch64-unified-buf)))
            (t
             ;; Original path for other arches: translate first, then boot.
             (translate-module-to-native module target)))))
    (multiple-value-bind (constant-pool pool-addr-table)
        (build-constant-pool module target)
      (let* ((nfn-table (build-nfn-table module target))
             (source-blob (embed-source-blob
                           (mvm-module-source-text module) target))
             (image (make-kernel-image
                     :target target
                     :native-code native-code
                     :constant-data constant-pool
                     :source-blob source-blob
                     :symbol-table nfn-table)))
    ;; Emit boot code (architecture-specific)
    (when (and boot-descriptor (not aarch64-unified-p))
      (let* ((boot-buf (make-mvm-buffer)))
        ;; x86-64 has a multi-stage boot: multiboot header → 32-bit stub → 64-bit entry
        (let ((mb-fn  (getf boot-descriptor :multiboot-header-fn))
              (b32-fn (getf boot-descriptor :boot32-fn))
              (k64-fn (getf boot-descriptor :kernel64-entry-fn))
              (entry-fn (getf boot-descriptor :entry-fn)))
          (when mb-fn  (funcall mb-fn boot-buf))
          (when b32-fn (funcall b32-fn boot-buf))
          (when k64-fn (funcall k64-fn boot-buf))
          ;; Generic entry-fn (RISC-V, ARM32, …)
          (when entry-fn (funcall entry-fn boot-buf)))
        (setf (kernel-image-boot-code image)
              (mvm-buffer-used-bytes boot-buf))))
    ;; Find kernel-main entry point (native offset within code buffer)
    ;; Use LAST match — "last-defun-wins" means the last kernel-main is the real one.
    (dolist (fn-info (mvm-module-function-table module))
      (when (string-equal (string (mvm-function-info-name fn-info)) "KERNEL-MAIN")
        (setf (kernel-image-entry-point image)
              (mvm-function-info-native-offset fn-info))))

    ;; AArch64 unified emit: patch placeholder B with kernel-main offset,
    ;; resolve all fixups, slice the unified buffer's bytes into boot+B
    ;; and native code parts.  Set both on the kernel-image so the
    ;; downstream final-buf assembly works unchanged (except for skipping
    ;; the explicit JMP emission since our B is already inside boot-code).
    (when aarch64-unified-p
      (let* ((b-instr-idx aarch64-boot-end-instr)
             ;; kernel-image-entry-point is kernel-main's offset in BYTES
             ;; within the translated region (Phase 2a kept it relative).
             (km-byte-offset (or (kernel-image-entry-point image) 0))
             ;; Translated region starts immediately after B (1 instruction).
             (km-instr-idx (+ b-instr-idx 1 (/ km-byte-offset 4)))
             ;; AArch64 B imm26 = target_pc - current_pc, in instruction units.
             (b-imm26 (- km-instr-idx b-instr-idx))
             (b-insn (logior #x14000000 (logand b-imm26 #x3FFFFFF))))
        ;; Patch the placeholder B with the real instruction.
        (setf (aref (modus.mvm::a64-buffer-code aarch64-unified-buf) b-instr-idx)
              b-insn)
        ;; Resolve all fixups across boot + native + helpers.
        (modus.mvm::a64-resolve-fixups aarch64-unified-buf)
        ;; Extract bytes and split.  boot-code-bytes includes the patched
        ;; B at its tail; native-code-bytes starts at the byte right after.
        (let* ((all-bytes (modus.mvm::a64-buffer-to-bytes aarch64-unified-buf))
               (boot-end-bytes (* (+ b-instr-idx 1) 4)))
          (setf (kernel-image-boot-code image)
                (subseq all-bytes 0 boot-end-bytes))
          (setf native-code (subseq all-bytes boot-end-bytes))
          (setf (kernel-image-native-code image) native-code))))
    ;; Assemble final image
    (let ((final-buf (make-mvm-buffer)))
      ;; Boot code (architecture-specific preamble)
      (when (kernel-image-boot-code image)
        (loop for b across (kernel-image-boot-code image)
              do (mvm-emit-byte final-buf b)))
      ;; Emit JMP to kernel-main after boot code (x86-64).
      ;; Boot code falls through here; we need to jump past any functions
      ;; defined before kernel-main in source order.
      ;; JMP rel32 = E9 + 4-byte signed offset (relative to next instruction).
      ;; After the JMP, native code starts, so rel32 = kernel-main native offset.
      (let ((entry-native-offset (kernel-image-entry-point image))
            (jmp-size 0))
        (when (and entry-native-offset boot-descriptor)
          (let ((arch (getf boot-descriptor :arch)))
            (cond
              ((member arch '(:x86-64 :i386))
               ;; x86/x86-64 JMP rel32 (5 bytes)
               (mvm-emit-byte final-buf #xE9)
               (mvm-emit-u32 final-buf (logand #xFFFFFFFF entry-native-offset))
               (setq jmp-size 5))
              ((member arch '(:arm32 :armv7 :armv7-rpi))
               ;; ARM32 B (unconditional branch, 4 bytes)
               ;; offset in instruction words: (entry_native_offset - 4) / 4
               ;; -4 because ARM reads PC as current+8, and native code starts
               ;; 4 bytes after this instruction (1 instruction unit)
               (let* ((byte-offset entry-native-offset)  ; bytes from native code start
                      (arm-offset (ash (- byte-offset 4) -2))  ; instruction units, adjusted for PC+8
                      (insn (logior #xEA000000 (logand arm-offset #xFFFFFF))))
                 (mvm-emit-byte final-buf (logand insn #xFF))
                 (mvm-emit-byte final-buf (logand (ash insn -8) #xFF))
                 (mvm-emit-byte final-buf (logand (ash insn -16) #xFF))
                 (mvm-emit-byte final-buf (logand (ash insn -24) #xFF))
                 (setq jmp-size 4)))
              ((and (member arch '(:aarch64 :rpi))
                    (not aarch64-unified-p))
               ;; AArch64 B (unconditional branch, 4 bytes)
               ;; B target = PC + imm26*4 (PC = this instruction).
               ;; Native code starts 4 bytes after this B instruction,
               ;; so imm26 = (byte-offset + 4) / 4 to skip past the B itself.
               (let* ((byte-offset entry-native-offset)
                      (a64-offset (ash (+ byte-offset 4) -2))  ; +4 for B instruction size
                      (insn (logior #x14000000 (logand a64-offset #x3FFFFFF))))
                 (mvm-emit-byte final-buf (logand insn #xFF))
                 (mvm-emit-byte final-buf (logand (ash insn -8) #xFF))
                 (mvm-emit-byte final-buf (logand (ash insn -16) #xFF))
                 (mvm-emit-byte final-buf (logand (ash insn -24) #xFF))
                 (setq jmp-size 4)))
              ;; AArch64 unified path: the B is already inside boot-code
              ;; (emitted into the unified a64-buffer as a placeholder
              ;; and patched after translation in the block above).
              ;; jmp-size stays 0 — native-image-offset = boot-code-length.
              ((and (member arch '(:aarch64 :rpi)) aarch64-unified-p)
               nil))))
        ;; Native code
        (let ((code-offset (mvm-buffer-position final-buf)))
          (setf (kernel-image-native-image-offset image) code-offset)
          (loop for b across native-code
                do (mvm-emit-byte final-buf b))
          ;; Update entry point to absolute offset
          (when (kernel-image-entry-point image)
            (incf (kernel-image-entry-point image) code-offset))
          ;; Patch the boot stub's code-bounds init block (if the
          ;; entry stub emitted one).  See translate-x64.lisp's
          ;; emit-code-bounds-init.  The patch offsets are recorded
          ;; in *x64-code-base-patch-offset* / *x64-code-end-patch-offset*
          ;; relative to the boot-code buffer; here we have the boot
          ;; code at the START of final-buf so the patch offsets are
          ;; the same as image offsets.  The values written are
          ;; load_addr + code-offset (code_base) and load_addr +
          ;; code-offset + native-code-length (code_end).
          (let* ((load-addr (and boot-descriptor
                                 (getf boot-descriptor :load-addr)))
                 (cb-off (and (boundp 'modus.mvm.x64::*x64-code-base-patch-offset*)
                              modus.mvm.x64::*x64-code-base-patch-offset*))
                 (ce-off (and (boundp 'modus.mvm.x64::*x64-code-end-patch-offset*)
                              modus.mvm.x64::*x64-code-end-patch-offset*)))
            (when (and load-addr cb-off ce-off)
              (let ((code-base (+ load-addr code-offset))
                    (code-end  (+ load-addr code-offset (length native-code)))
                    (raw       (mvm-buffer-bytes final-buf)))
                (dotimes (i 8)
                  (setf (aref raw (+ cb-off i))
                        (logand (ash code-base (- (* i 8))) #xFF))
                  (setf (aref raw (+ ce-off i))
                        (logand (ash code-end (- (* i 8))) #xFF)))
                ;; Reset for next build (they're SBCL-side dynamic state).
                (setf modus.mvm.x64::*x64-code-base-patch-offset* nil)
                (setf modus.mvm.x64::*x64-code-end-patch-offset*  nil))))))
      ;; Pad to 16-byte alignment of the constant pool's VIRTUAL ADDRESS,
      ;; not just the buffer position.  Pool entries are heap-format objects
      ;; whose tagged addresses must be 16-byte-aligned for OBJ-REF/OBJ-SET
      ;; (offset = idx*8 + 7 with object tag #x09 expects header at addr&-16).
      (let* ((load-addr (or (and boot-descriptor (getf boot-descriptor :load-addr)) 0))
             (wrap-header (wrap-header-size-for-boot boot-descriptor))
             (cur-pos (mvm-buffer-position final-buf))
             (cur-vaddr (+ load-addr wrap-header cur-pos))
             (pad (mod (- cur-vaddr) 16)))
        (dotimes (i pad) (mvm-emit-byte final-buf 0))
        ;; Record where the pool actually starts so the patcher uses
        ;; the right offset.
        (setf (kernel-image-constant-pool-offset image)
              (mvm-buffer-position final-buf)))
      ;; Constant pool
      (loop for b across constant-pool
            do (mvm-emit-byte final-buf b))
      ;; NFN table
      (loop for b across nfn-table
            do (mvm-emit-byte final-buf b))
      ;; Source blob (for next-generation self-hosting)
      (loop for b across source-blob
            do (mvm-emit-byte final-buf b))
      ;; Image metadata footer
      (let ((word-size (target-word-size target)))
        ;; Total image size
        (if (= word-size 8)
            (mvm-emit-u64 final-buf (mvm-buffer-position final-buf))
            (mvm-emit-u32 final-buf (mvm-buffer-position final-buf)))
        ;; Source blob offset (for self-hosting reader)
        (if (= word-size 8)
            (mvm-emit-u64 final-buf (- (mvm-buffer-position final-buf)
                                        (length source-blob) 8))
            (mvm-emit-u32 final-buf (- (mvm-buffer-position final-buf)
                                        (length source-blob) 4))))
      (let ((raw-bytes (mvm-buffer-used-bytes final-buf)))
        ;; LI-CONST patches: walk the per-arch patch list and write
        ;; tagged constant-pool addresses into the placeholder MOVABS
        ;; immediates that the translator emitted.  The translator's
        ;; patch list holds (native-byte-offset . pool-index) pairs;
        ;; we add native-image-offset to land in raw-bytes, and compute
        ;; tagged-addr from pool-vaddr + addr-table[idx].
        (apply-li-const-patches raw-bytes image module boot-descriptor
                                pool-addr-table native-code)
        ;; AArch64 fn-addr patches: write the absolute runtime address
        ;; of each target function into the MOVZ + MOVK placeholder
        ;; pair that +op-fn-addr+ emitted.  See translate-aarch64.lisp
        ;; *aarch64-fn-addr-patches* docstring.
        (apply-aarch64-fn-addr-patches raw-bytes image module boot-descriptor)
        ;; AArch64 code-bounds patches: fill in slot 0x10000160 (code_base)
        ;; and 0x10000168 (code_end) so functionp's range arm classifies
        ;; raw fn-addrs correctly.  See translate-aarch64.lisp's
        ;; emit-aarch64-code-bounds-init.
        (apply-aarch64-code-bounds-patches raw-bytes image boot-descriptor)
        ;; UEFI: patch stub with kernel data offset/size, then wrap in PE32+
        (when (and boot-descriptor (getf boot-descriptor :uefi))
          (patch-uefi-stub raw-bytes
                           (length (kernel-image-boot-code image))))
        ;; Wrap in target-appropriate format
        (setf (kernel-image-image-bytes image)
              (if boot-descriptor
                  (cond
                    ((getf boot-descriptor :uefi)
                     (wrap-in-pe32plus raw-bytes))
                    ((eq (getf boot-descriptor :elf-format) :linux-x64)
                     (wrap-in-elf64-le raw-bytes
                                       (or (getf boot-descriptor :load-addr) #x400000)
                                       :function-table
                                       (mvm-module-function-table module)
                                       :native-image-offset
                                       (or (kernel-image-native-image-offset image) 0)
                                       :native-code-length
                                       (length (kernel-image-native-code image))))
                    ((eq (getf boot-descriptor :elf-format) :linux-aarch64)
                     (wrap-in-elf64-le-aa64 raw-bytes
                                            (or (getf boot-descriptor :load-addr) #x400000)
                                            :function-table
                                            (mvm-module-function-table module)
                                            :native-image-offset
                                            (or (kernel-image-native-image-offset image) 0)
                                            :native-code-length
                                            (length (kernel-image-native-code image))))
                    (t (let ((elf-machine (getf boot-descriptor :elf-machine))
                             (load-addr (or (getf boot-descriptor :load-addr) 0))
                             (elf-class (getf boot-descriptor :elf-class 32))
                             (elf-flags (getf boot-descriptor :elf-flags 0)))
                         (cond
                           ((and elf-machine (= elf-class 32))
                            (wrap-in-elf32-be raw-bytes load-addr elf-machine))
                           ((and elf-machine (= elf-class 64))
                            (wrap-in-elf64-be raw-bytes load-addr elf-machine elf-flags))
                           (t raw-bytes)))))
                  raw-bytes))))
    ;; Side-channel symbol map: tab-separated, easy to grep/awk.
    ;; Independent of the ELF .symtab — useful for non-ELF targets too,
    ;; and for resolving RIPs without parsing the binary.
    (when *write-symmap-path*
      (write-symmap *write-symmap-path*
                    (mvm-module-function-table module)
                    image
                    boot-descriptor))
    image))))

(defun write-symmap (path function-table image boot-descriptor)
  "Write a tab-separated symbol map to PATH.
   Columns: virtual-addr<TAB>size<TAB>native-offset<TAB>name
   Rows are sorted by virtual-addr ascending."
  (let* ((load-addr (or (and boot-descriptor (getf boot-descriptor :load-addr))
                        #x400000))
         ;; ehdr+phdr for the ELF wrapper.  For non-ELF targets this is 0;
         ;; the image-byte 0 already lives at the load address.
         (elf-header (if (and boot-descriptor
                              (or (eq (getf boot-descriptor :elf-format) :linux-x64)
                                  (eq (getf boot-descriptor :elf-format) :linux-aarch64)))
                         120
                         0))
         (nio (or (kernel-image-native-image-offset image) 0))
         (ncl (length (kernel-image-native-code image)))
         (sorted (stable-sort (copy-list function-table)
                              #'< :key #'mvm-function-info-native-offset)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (format out "# virtual-addr~Csize~Cnative-offset~Cname~%"
              #\Tab #\Tab #\Tab)
      (loop for fi in sorted
            for i from 0
            for nat-off = (or (mvm-function-info-native-offset fi) 0)
            for next-off = (if (< (1+ i) (length sorted))
                               (mvm-function-info-native-offset
                                 (nth (1+ i) sorted))
                               ncl)
            for size = (max 0 (- next-off nat-off))
            for vaddr = (+ load-addr elf-header nio nat-off)
            do (format out "~16,'0X~C~D~C~D~C~A~%"
                       vaddr #\Tab size #\Tab nat-off #\Tab
                       (string (mvm-function-info-name fi))))
      (format t "  Wrote symbol map: ~A~%" path))))

;;; ============================================================
;;; Top-Level API
;;; ============================================================

(defun resolve-target-arch (target)
  "Resolve a target keyword to its underlying architecture keyword.
   Board-specific targets (e.g. :rpi) map to their base architecture."
  (case target
    (:rpi :aarch64)
    (:fixpoint :aarch64)
    (:uefi-x64 :x86-64)
    (:linux-x64 :x86-64)
    (:linux-aarch64 :aarch64)
    (:x64-console :x86-64)
    (:i386-console :i386)
    (otherwise target)))

(defun build-image (&key (target :x86-64) (source nil) (source-text nil))
  "Build a bootable kernel image for TARGET.

   TARGET: keyword naming the target architecture or board
           (:x86-64, :riscv64, :aarch64, :ppc64, :ppc32, :i386, :68k,
            :arm32, :rpi)

   SOURCE: list of Lisp forms to compile (alternative to source-text)

   SOURCE-TEXT: raw Lisp source text string (alternative to source)

   Returns a KERNEL-IMAGE struct with the assembled image.

   Usage:
     (build-image :target :riscv64)     ; from any running Modus
     (build-image :target :x86-64)      ; cross-compile back
     (build-image :target :aarch64)     ; or to ARM
     (build-image :target :rpi)         ; Raspberry Pi (AArch64)"
  (let* ((arch (resolve-target-arch target))
         (target-desc (find-target arch)))
    (unless target-desc
      (error "Unknown target architecture: ~A~%Known targets: ~{~A~^, ~}"
             target (list-targets)))
    ;; Get source
    (let ((src-text (or source-text
                       (when source
                         (with-output-to-string (s)
                           (dolist (form source)
                             (prin1 form s)
                             (terpri s))))
                       ;; In a running Modus kernel, read embedded source
                       ;; (read-embedded-source)
                       (error "No source provided"))))
      ;; Compile
      (let ((module (compile-source-to-module src-text)))
        ;; Get boot descriptor for target
        (let* ((boot-desc (get-boot-descriptor target))
               (serial-base (getf boot-desc :serial-base)))
          ;; Serial base priority: explicit setf > boot descriptor > QEMU virt default
          (let ((*aarch64-serial-base* (or *aarch64-serial-base* serial-base #x09000000)))
            ;; Assemble image
            (assemble-kernel-image module target-desc
                                   :boot-descriptor boot-desc)))))))

(defun get-boot-descriptor (target-name)
  "Get the boot descriptor for the given target architecture.
   Returns a boot descriptor plist, or NIL for architectures
   whose boot code has not yet been implemented."
  (case target-name
    (:x86-64  (x64-boot-descriptor))
    (:riscv64 (riscv-boot-descriptor))
    (:aarch64 (aarch64-boot-descriptor))
    (:ppc64   (ppc64-boot-descriptor))
    (:ppc32   (ppc32-boot-descriptor))
    (:i386    (i386-boot-descriptor))
    (:68k     (m68k-boot-descriptor))
    (:arm32   (arm32-boot-descriptor))
    (:armv7   (armv7-boot-descriptor))
    (:armv7-rpi (armv7-rpi-boot-descriptor))
    (:rpi     (rpi-boot-descriptor))
    (:fixpoint (aarch64-fixpoint-boot-descriptor))
    (:uefi-x64 (uefi-x64-boot-descriptor))
    (:linux-x64 (linux-x64-boot-descriptor))
    (:linux-aarch64 (linux-aarch64-boot-descriptor))
    (:x64-console (x64-console-boot-descriptor))
    (:i386-console (i386-console-boot-descriptor))
    (otherwise nil)))

(defun write-kernel-image (image pathname)
  "Write a kernel image to disk as a flat binary."
  (let ((bytes (kernel-image-image-bytes image)))
    (with-open-file (out pathname :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
      (write-sequence bytes out))
    (format t "Wrote ~D bytes to ~A~%" (length bytes) pathname)
    pathname))

;;; ============================================================
;;; Cross-Compilation Matrix Test
;;; ============================================================

(defun test-cross-compilation ()
  "Test that cross-compilation works for all target pairs.
   Each architecture should be able to produce images for every other."
  (let ((targets (list-targets))
        (test-source '((defun add1 (x) (+ x 1))
                       (defun kernel-main () (add1 41)))))
    (dolist (target targets)
      (format t "~%Building for ~A...~%" target)
      (handler-case
          (let ((image (build-image :target target :source test-source)))
            (format t "  Success: ~D bytes~%"
                    (length (kernel-image-image-bytes image))))
        (error (e)
          (format t "  FAILED: ~A~%" e))))))

;;; ============================================================
;;; Self-Hosting Support
;;; ============================================================

(defun read-all-forms (source-text)
  "Read all Lisp forms from SOURCE-TEXT string.
   Returns a list of forms."
  (with-input-from-string (stream source-text)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun read-all-forms-with-locations (source-text)
  "Read all Lisp forms from SOURCE-TEXT, tracking line numbers.
   Returns (forms . line-numbers) where line-numbers is a vector
   mapping form index to approximate source line.

   Reader errors SKIP to the next line and continue — needed for ANSI
   test fixtures that contain platform-specific reader syntax (`#<`
   etc.) the SBCL reader rejects. First-party sources should be
   verified via CHECK-PARSES first (see that function's docstring)."
  (let ((forms nil)
        (lines nil)
        (line-count 1)
        ;; Read in :MODUS.MVM so #.<reader-eval> of MVM constants (interp.lisp's
        ;; (#.+op-nop+ ...) case keys) resolves in the package where load-mvm
        ;; bound them.  Symbols are name-hashed by the compiler (package-
        ;; independent), so this doesn't change how the cl-* runtime compiles.
        (*package* (or (find-package :modus.mvm) *package*)))
    (with-input-from-string (stream source-text)
      (loop
        (let ((pos (file-position stream)))
          (setf line-count (1+ (count #\Newline source-text :end pos)))
          (let ((form (handler-case (read stream nil :eof)
                        (error (e)
                          (format t "  SKIP read at line ~D: ~A~%" line-count e)
                          (loop for ch = (read-char stream nil nil)
                                while (and ch (char/= ch #\Newline)))
                          :skip))))
            (when (eq form :eof) (return))
            (unless (eq form :skip)
              (push form forms)
              (push line-count lines))))))
    (cons (nreverse forms) (coerce (nreverse lines) 'vector))))

(defun check-parses (path)
  "Verify PATH reads cleanly with SBCL's reader — errors loudly otherwise.
   Build scripts call this on every first-party source file (prelude,
   cl-*, boot, translator, etc.) BEFORE concatenating them into the
   build blob, so a paren mismatch fails fast at a specific file
   instead of getting silently skipped later by
   READ-ALL-FORMS-WITH-LOCATIONS (which must stay lenient for ANSI
   test fixtures). A missing close paren in %format-impl once hid
   behind that leniency for weeks — see ansi-notes.md: 'SOLVED:
   late-cond-branch'."
  (handler-case
      (with-open-file (f path)
        ;; Read in :MODUS.MVM so #.<reader-eval> of MVM constants (e.g.
        ;; interp.lisp's (#.+op-nop+ ...) case keys) resolves the symbol in
        ;; the package where load-mvm bound it, not the build script's
        ;; CL-USER.  Forms are discarded (this only checks parens), so the
        ;; package choice is harmless for the non-#. first-party files.
        (let ((*package* (or (find-package :modus.mvm) *package*)))
          (loop for next = (read f nil :eof)
                until (eq next :eof))))
    (error (e)
      (error "check-parses: ~A failed to parse: ~A" path e))))

(defun compute-name-hash (name-string)
  "Compute dual-FNV-1a hash for a function name.
   Two independent FNV-1a-32 hashes combined into a 60-bit value.
   Same algorithm as compute-hash-chars in build.lisp."
  (let ((name (string-upcase (string name-string)))
        (h1 2166136261) (h2 3735928559))
    (loop for c across name
          do (setq h1 (logand (* (logxor h1 (char-code c)) 16777619) #xFFFFFFFF))
             (setq h2 (logand (* (logxor h2 (char-code c)) 805306457) #xFFFFFFFF)))
    (let ((combined (logior (ash (logand h1 #x3FFFFFFF) 30)
                            (logand h2 #x3FFFFFFF))))
      (if (zerop combined) 1 combined))))

(defun compiled-module-to-mvm-module (compiled-mod source-text)
  "Convert a compiled-module (from compiler.lisp) to an mvm-module
   (used by the cross-compilation pipeline).
   Bridges the compiler's function-info to mvm-function-info with
   name hashes for the NFN table."
  (make-mvm-module
   :bytecode (compiled-module-bytecode compiled-mod)
   :function-table
   (mapcar (lambda (fi)
             (make-mvm-function-info
              :name (function-info-name fi)
              :name-hash (compute-name-hash (function-info-name fi))
              :param-count (function-info-param-count fi)
              :bytecode-offset (function-info-bytecode-offset fi)
              :bytecode-length (function-info-bytecode-length fi)
              :native-offset nil
              :native-length nil))
           (compiled-module-function-table compiled-mod))
   :constant-table (compiled-module-constant-table compiled-mod)
   :source-text source-text))
