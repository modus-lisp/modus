;;;; boot-linux-x64.lisp - Linux x86-64 ELF entry for Modus
;;;;
;;;; Produces a Linux userspace executable instead of a bare-metal kernel.
;;;; Entry point receives argc/argv on the stack from the Linux loader.
;;;; Uses Linux syscalls for I/O instead of hardware port I/O.

(in-package :modus.mvm)

;;; ============================================================
;;; Linux x64 Constants
;;; ============================================================

(defconstant +linux-x64-load-addr+ #x400000)    ; Traditional Linux x64 load address
(defconstant +linux-x64-heap-addr+ #x10000000)  ; Heap start (same as bare-metal)
(defconstant +linux-x64-heap-size+ #x0E000000)  ; 224MB heap
(defconstant +linux-x64-stack-size+ #x100000)   ; 1MB stack (Linux provides this)

;;; ============================================================
;;; ELF64 Little-Endian Wrapper
;;; ============================================================

(defun wrap-in-elf64-le (raw-bytes load-addr)
  "Wrap raw image bytes in a Linux ELF64 little-endian executable."
  (let* ((ehdr-size 64)
         (phdr-size 56)
         (header-total (+ ehdr-size phdr-size))
         (total-size (+ header-total (length raw-bytes)))
         (entry-point (+ load-addr header-total))
         (buf (make-mvm-buffer)))
    ;; ---- ELF Header (64 bytes) ----
    ;; e_ident: magic + class + data + version + OS/ABI + padding
    (mvm-emit-byte buf #x7F)           ; EI_MAG0
    (mvm-emit-byte buf (char-code #\E)) ; EI_MAG1
    (mvm-emit-byte buf (char-code #\L)) ; EI_MAG2
    (mvm-emit-byte buf (char-code #\F)) ; EI_MAG3
    (mvm-emit-byte buf 2)              ; EI_CLASS = ELFCLASS64
    (mvm-emit-byte buf 1)              ; EI_DATA = ELFDATA2LSB (little-endian)
    (mvm-emit-byte buf 1)              ; EI_VERSION = EV_CURRENT
    (mvm-emit-byte buf 0)              ; EI_OSABI = ELFOSABI_NONE
    (dotimes (i 8) (mvm-emit-byte buf 0)) ; EI_ABIVERSION + padding
    ;; e_type = ET_EXEC (2)
    (mvm-emit-u16 buf 2)
    ;; e_machine = EM_X86_64 (62)
    (mvm-emit-u16 buf 62)
    ;; e_version = 1
    (mvm-emit-u32 buf 1)
    ;; e_entry = entry point (after headers)
    (mvm-emit-u64 buf entry-point)
    ;; e_phoff = offset to program header (immediately after ELF header)
    (mvm-emit-u64 buf ehdr-size)
    ;; e_shoff = 0 (no section headers)
    (mvm-emit-u64 buf 0)
    ;; e_flags = 0
    (mvm-emit-u32 buf 0)
    ;; e_ehsize = 64
    (mvm-emit-u16 buf ehdr-size)
    ;; e_phentsize = 56
    (mvm-emit-u16 buf phdr-size)
    ;; e_phnum = 1
    (mvm-emit-u16 buf 1)
    ;; e_shentsize = 0
    (mvm-emit-u16 buf 0)
    ;; e_shnum = 0
    (mvm-emit-u16 buf 0)
    ;; e_shstrndx = 0
    (mvm-emit-u16 buf 0)
    ;; ---- Program Header (56 bytes) ----
    ;; p_type = PT_LOAD (1)
    (mvm-emit-u32 buf 1)
    ;; p_flags = PF_R | PF_W | PF_X (7)
    (mvm-emit-u32 buf 7)
    ;; p_offset = 0 (load from start of file)
    (mvm-emit-u64 buf 0)
    ;; p_vaddr = load address
    (mvm-emit-u64 buf load-addr)
    ;; p_paddr = load address
    (mvm-emit-u64 buf load-addr)
    ;; p_filesz = total file size
    (mvm-emit-u64 buf total-size)
    ;; p_memsz = file size + heap (BSS-like expansion)
    (mvm-emit-u64 buf (+ total-size +linux-x64-heap-size+))
    ;; p_align = 2MB
    (mvm-emit-u64 buf #x200000)
    ;; ---- Raw image data ----
    (loop for b across raw-bytes do (mvm-emit-byte buf b))
    (mvm-buffer-used-bytes buf)))

;;; ============================================================
;;; Linux x64 Entry Point
;;; ============================================================

(defun emit-linux-x64-entry (buf)
  "Emit the Linux x64 entry stub.
   On entry from Linux: RSP points to [argc, argv[0], argv[1], ..., NULL, envp...]
   We save argc/argv, set up MVM runtime registers, allocate heap via mmap, and
   fall through to native code (which starts with a JMP to kernel-main)."
  ;; Save argc and argv pointer for later use
  ;; [RSP] = argc, [RSP+8] = argv[0], etc.
  ;; Store argc at a fixed address for later retrieval
  (emit-bytes buf #x48 #x8B #x04 #x24)        ; mov rax, [rsp]  (argc)
  (emit-bytes buf #x48 #x89 #x04 #x25)         ; mov [abs32], rax
  (emit-le32 buf #x600000)                       ; address: 0x600000 (globals area)
  (emit-bytes buf #x48 #x8D #x44 #x24 #x08)    ; lea rax, [rsp+8]  (argv pointer)
  (emit-bytes buf #x48 #x89 #x04 #x25)         ; mov [abs32], rax
  (emit-le32 buf #x600008)                       ; address: 0x600008

  ;; Set up MVM runtime registers
  ;; R15 = NIL (#xDEAD0001)
  (emit-bytes buf #x49 #xBF)                    ; mov r15, imm64
  (emit-le64 buf #xDEAD0001)
  ;; R12 = alloc pointer (will be set after mmap)
  ;; R14 = alloc limit (will be set after mmap)

  ;; Allocate heap via mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
  ;; syscall: rax=9 (mmap), rdi=addr, rsi=length, rdx=prot, r10=flags, r8=fd, r9=offset
  (emit-bytes buf #x48 #xC7 #xC7 #x00 #x00 #x00 #x10) ; mov rdi, 0x10000000 (hint addr)
  (emit-bytes buf #x48 #xC7 #xC6)              ; mov rsi, imm32 (length)
  (emit-le32 buf +linux-x64-heap-size+)
  (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, 3 (PROT_READ|PROT_WRITE)
  (emit-bytes buf #x49 #xC7 #xC2 #x22 #x00 #x00 #x00) ; mov r10, 0x22 (MAP_PRIVATE|MAP_ANONYMOUS)
  (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1 (fd)
  (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0 (offset)
  (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, 9 (SYS_mmap)
  (emit-bytes buf #x0F #x05)                    ; syscall

  ;; rax = mmap result (heap base address, or -errno on failure)
  ;; R12 = alloc pointer = mmap result
  (emit-bytes buf #x49 #x89 #xC4)              ; mov r12, rax
  ;; R14 = alloc limit = R12 + heap size
  (emit-bytes buf #x49 #x89 #xC6)              ; mov r14, rax
  (emit-bytes buf #x49 #x81 #xC6)              ; add r14, imm32
  (emit-le32 buf +linux-x64-heap-size+)

  ;; Set up frame pointer
  (emit-bytes buf #x48 #x89 #xE5))             ; mov rbp, rsp

  ;; Fall through to JMP kernel-main (emitted by assemble-kernel-image)

;;; ============================================================
;;; Helper: emit raw bytes
;;; ============================================================

(defun emit-bytes (buf &rest bytes)
  "Emit raw bytes to the buffer."
  (dolist (b bytes)
    (mvm-emit-byte buf b)))

(defun emit-le32 (buf val)
  "Emit a 32-bit little-endian value."
  (mvm-emit-byte buf (logand val #xFF))
  (mvm-emit-byte buf (logand (ash val -8) #xFF))
  (mvm-emit-byte buf (logand (ash val -16) #xFF))
  (mvm-emit-byte buf (logand (ash val -24) #xFF)))

(defun emit-le64 (buf val)
  "Emit a 64-bit little-endian value."
  (emit-le32 buf (logand val #xFFFFFFFF))
  (emit-le32 buf (logand (ash val -32) #xFFFFFFFF)))

;;; ============================================================
;;; Boot Descriptor
;;; ============================================================

(defun linux-x64-boot-descriptor ()
  "Return the Linux x86-64 boot descriptor for ELF executable building."
  (list :arch :x86-64
        :entry-fn #'emit-linux-x64-entry
        :load-addr +linux-x64-load-addr+
        :elf-format :linux-x64))
