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
(defconstant +linux-x64-heap-size+ #x38000000)  ; 896MB heap
;; Globals at start of heap region (after mmap succeeds)
;; +0x00: argc, +0x08: argv base, +0x10: argv[0], +0x18: argv[1], etc.
(defconstant +linux-x64-globals+  #x10000000)

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
    (mvm-emit-byte buf #x7F)
    (mvm-emit-byte buf (char-code #\E))
    (mvm-emit-byte buf (char-code #\L))
    (mvm-emit-byte buf (char-code #\F))
    (mvm-emit-byte buf 2)              ; ELFCLASS64
    (mvm-emit-byte buf 1)              ; ELFDATA2LSB
    (mvm-emit-byte buf 1)              ; EV_CURRENT
    (mvm-emit-byte buf 0)              ; ELFOSABI_NONE
    (dotimes (i 8) (mvm-emit-byte buf 0))
    (mvm-emit-u16 buf 2)              ; ET_EXEC
    (mvm-emit-u16 buf 62)             ; EM_X86_64
    (mvm-emit-u32 buf 1)              ; e_version
    (mvm-emit-u64 buf entry-point)    ; e_entry
    (mvm-emit-u64 buf ehdr-size)      ; e_phoff
    (mvm-emit-u64 buf 0)              ; e_shoff
    (mvm-emit-u32 buf 0)              ; e_flags
    (mvm-emit-u16 buf ehdr-size)      ; e_ehsize
    (mvm-emit-u16 buf phdr-size)      ; e_phentsize
    (mvm-emit-u16 buf 1)              ; e_phnum
    (mvm-emit-u16 buf 0)              ; e_shentsize
    (mvm-emit-u16 buf 0)              ; e_shnum
    (mvm-emit-u16 buf 0)              ; e_shstrndx
    ;; ---- Program Header (56 bytes) ----
    (mvm-emit-u32 buf 1)              ; PT_LOAD
    (mvm-emit-u32 buf 7)              ; PF_R|PF_W|PF_X
    (mvm-emit-u64 buf 0)              ; p_offset
    (mvm-emit-u64 buf load-addr)      ; p_vaddr
    (mvm-emit-u64 buf load-addr)      ; p_paddr
    (mvm-emit-u64 buf total-size)     ; p_filesz
    (mvm-emit-u64 buf (+ total-size +linux-x64-heap-size+)) ; p_memsz
    (mvm-emit-u64 buf #x200000)       ; p_align
    ;; ---- Raw image data ----
    (loop for b across raw-bytes do (mvm-emit-byte buf b))
    (mvm-buffer-used-bytes buf)))

;;; ============================================================
;;; Linux x64 Entry Point
;;; ============================================================

(defun emit-linux-x64-entry (buf)
  "Emit the Linux x64 entry stub.
   On entry: RSP → [argc, argv[0], argv[1], ..., NULL, envp...]
   Strategy: save argc/argv to callee-saved regs, mmap heap, then
   copy argc/argv to globals area in the mmap'd heap."
  ;; Save argc and argv[0..3] in callee-saved registers (survive mmap syscall)
  ;; RBX = argc, R13 = argv[0], R14 = argv[1], R15 = argv[2]
  ;; (R14/R15 will be overwritten later for alloc ptr/NIL)
  (emit-bytes buf #x48 #x8B #x1C #x24)          ; mov rbx, [rsp]    (argc)
  (emit-bytes buf #x4C #x8B #x6C #x24 #x08)     ; mov r13, [rsp+8]  (argv[0])
  (emit-bytes buf #x4C #x8B #x74 #x24 #x10)     ; mov r14, [rsp+16] (argv[1])
  (emit-bytes buf #x4C #x8B #x7C #x24 #x18)     ; mov r15, [rsp+24] (argv[2])

  ;; mmap heap: rax=9, rdi=hint, rsi=size, rdx=prot, r10=flags, r8=fd, r9=off
  (emit-bytes buf #x48 #xC7 #xC7 #x00 #x00 #x00 #x10) ; mov rdi, 0x10000000
  (emit-bytes buf #x48 #xC7 #xC6)                ; mov rsi, imm32
  (emit-le32 buf +linux-x64-heap-size+)
  (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, 3 (PROT_RW)
  (emit-bytes buf #x49 #xC7 #xC2 #x22 #x00 #x00 #x00) ; mov r10, 0x22 (MAP_PRIV|MAP_ANON)
  (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
  (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
  (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, 9 (SYS_mmap)
  (emit-bytes buf #x0F #x05)                      ; syscall

  ;; Store argc/argv to globals area at heap base (mmap result in RAX)
  ;; argc at [rax+0], argv[0] at [rax+0x10], argv[1] at [rax+0x18], argv[2] at [rax+0x20]
  (emit-bytes buf #x48 #x89 #x18)                ; mov [rax], rbx     (argc)
  (emit-bytes buf #x4C #x89 #x68 #x10)           ; mov [rax+0x10], r13 (argv[0])
  (emit-bytes buf #x4C #x89 #x70 #x18)           ; mov [rax+0x18], r14 (argv[1])
  (emit-bytes buf #x4C #x89 #x78 #x20)           ; mov [rax+0x20], r15 (argv[2])

  ;; Set up MVM runtime registers
  ;; R12 = alloc pointer (skip first 256 bytes used for globals)
  (emit-bytes buf #x49 #x89 #xC4)                ; mov r12, rax
  (emit-bytes buf #x49 #x81 #xC4 #x00 #x01 #x00 #x00) ; add r12, 256 (skip globals)
  ;; R14 = alloc limit
  (emit-bytes buf #x49 #x89 #xC6)                ; mov r14, rax
  (emit-bytes buf #x49 #x81 #xC6)                ; add r14, heap_size
  (emit-le32 buf +linux-x64-heap-size+)
  ;; R15 = NIL
  (emit-bytes buf #x49 #xBF)                      ; mov r15, imm64
  (emit-le64 buf #xDEAD0001)
  ;; Frame pointer
  (emit-bytes buf #x48 #x89 #xE5))               ; mov rbp, rsp

  ;; Fall through to JMP kernel-main

;;; ============================================================
;;; Helper: emit raw bytes
;;; ============================================================

(defun emit-bytes (buf &rest bytes)
  (dolist (b bytes) (mvm-emit-byte buf b)))

(defun emit-le32 (buf val)
  (mvm-emit-byte buf (logand val #xFF))
  (mvm-emit-byte buf (logand (ash val -8) #xFF))
  (mvm-emit-byte buf (logand (ash val -16) #xFF))
  (mvm-emit-byte buf (logand (ash val -24) #xFF)))

(defun emit-le64 (buf val)
  (emit-le32 buf (logand val #xFFFFFFFF))
  (emit-le32 buf (logand (ash val -32) #xFFFFFFFF)))

;;; ============================================================
;;; Boot Descriptor
;;; ============================================================

(defun linux-x64-boot-descriptor ()
  (list :arch :x86-64
        :entry-fn #'emit-linux-x64-entry
        :load-addr +linux-x64-load-addr+
        :elf-format :linux-x64))
