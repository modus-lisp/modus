;;;; boot-win64.lisp - Windows x86-64 hosted entry for Modus: a PE32+ console EXE.
;;;;
;;;; The hosted Linux CLI (boot-linux-x64.lisp) is an ELF whose runtime issues
;;;; Linux system calls directly.  Windows has no stable system-call interface,
;;;; so this image keeps the SAME runtime and the SAME syscall ABI, and replaces
;;;; the one place a syscall leaves the image: every `syscall' instruction the
;;;; translator emits becomes `call SHIM', where SHIM is a routine in the boot
;;;; stub that takes the Linux number in RAX and the arguments in RDI RSI RDX
;;;; R10 R8 R9, dispatches to kernel32 through the import address table, and
;;;; returns a Linux-style result (-errno on failure).  It preserves every
;;;; register a Linux syscall preserves (all but RAX, RCX and R11).
;;;;
;;;; IMAGE LAYOUT.  The PE headers occupy file bytes [0, 0x400) and the code
;;;; starts at file offset 0x1000, which the .text section maps at RVA 0x1000.
;;;; So the image byte at raw offset O sits at virtual address
;;;;     #x400000 + #x1000 + O
;;;; exactly as the ELF wrapper's raw bytes sit at load-addr + header-total.
;;;; The image base is FIXED (no relocations): the runtime's absolute addresses
;;;; are compiled in, as they are for the ELF.  The BSS that holds the fixed low
;;;; block (#x10000000..) is a zero-filled section, like the ELF's p_memsz tail.
;;;;
;;;; THE STUB (raw offsets from 0), all of it before the JMP cross.lisp puts
;;;; after the boot code:
;;;;     0        JMP prologue
;;;;     5        IAT: one 8-byte slot per import (filled by the loader)
;;;;     ...      SHIM: the syscall dispatcher
;;;;     ...      PROLOGUE: VirtualAlloc the heap, argc, then the shared heap tail
;;;;              (emit-linux-x64-heap-tail), which falls through into the JMP to
;;;;              kernel-main.
;;;;
;;;; SCOPE (this milestone): one OS thread.  The per-CPU GS block and the FS
;;;; per-thread window are OFF in the Windows build (build-win64-cli.lisp), since
;;;; on Windows GS base is the TEB.  clone, futex, arch_prctl and real signal
;;;; handlers are not provided: they return -ENOSYS, or 0 for sigaction, which
;;;; is correct only while nothing runs a second thread or depends on a signal.

(in-package :modus.mvm)

(defconstant +win64-image-base+ #x400000)
(defconstant +win64-header-size+ #x1000)    ; PE headers + padding; raw code starts here
(defconstant +win64-text-va+ #x401000)      ; = image base + header size
(defconstant +win64-bss-end-va+ #x20000000) ; the zero-filled low block runs to here
;; The Lisp stack: a fixed, fully committed region well above 4 GB, like the
;; Linux process stack.  The loader's main-thread stack lands at 0x20000000,
;; inside the runtime's [0x10000000, 0x40000000) data window.
(defconstant +win64-stack-va+ #x4000000000)
(defconstant +win64-stack-size+ #x1000000)  ; 16 MB
;; The bottom 1 MB of that region is the shim's own stack: every Win32 call runs
;; there, so kernel32 and ntdll never write to the Lisp stack.  (A Linux SYSCALL
;; writes nothing to the user stack, and the runtime was only ever run that way.)
(defconstant +win64-win32-stack-top+ (+ #x4000000000 #x100000))
;; Where hint-less mmaps are placed.  Linux puts them high (0x7f...); Wine's
;; VirtualAlloc(NULL) returns the lowest free region (0x240000, below the image),
;; and this runtime classifies some addresses by range ("code addrs are < 4GB").
(defconstant +win64-mmap-base+ #x5000000000)

(defparameter *win64-imports*
  '("ExitProcess" "GetStdHandle" "WriteFile" "ReadFile" "VirtualAlloc"
    "VirtualFree" "GetCurrentProcessId" "GetSystemTimeAsFileTime" "Sleep"
    "GetCommandLineA" "GetEnvironmentStringsA")
  "kernel32 functions the stub imports, in IAT slot order.")

;; MODUS_WIN64_TRACE=1 (build time): the shim writes "s<nr>" (hex) to stderr for
;; every syscall, before dispatching it.  Syscalls the shim does not implement
;; return -ENOSYS without reaching Win32, so a Win32 trace (WINEDEBUG=+relay)
;; cannot see them; this can.
(defvar *win64-trace-syscalls*
  (let ((v (sb-ext:posix-getenv "MODUS_WIN64_TRACE")))
    (and v (plusp (length v)) (string/= v "0"))))

(defun win64-import-slot (name)
  (or (position name *win64-imports* :test #'string=)
      (error "boot-win64: ~A is not in *win64-imports*" name)))

;;; ============================================================
;;; A tiny two-pass assembler for the stub.  Jumps and calls are relative to
;;; the stub's own start, so the stub needs no absolute addresses of its own.
;;; ============================================================

(defun w64-le32 (n)
  (list (logand n #xFF) (logand (ash n -8) #xFF)
        (logand (ash n -16) #xFF) (logand (ash n -24) #xFF)))

(defun w64-le64 (n)
  (append (w64-le32 (logand n #xFFFFFFFF))
          (w64-le32 (logand (ash n -32) #xFFFFFFFF))))

(defun w64-pad (n align) (* align (ceiling n align)))

(defun w64-item-size (item pos)
  (ecase (first item)
    (:label 0)
    (:bytes (length (rest item)))
    (:raw (length (second item)))
    (:zeros (second item))
    (:jmp 5)
    (:jcc 6)
    (:call-iat 6)
    ((:rip-load :rip-store) 7)
    (:align (let ((n (second item))) (mod (- n (mod pos n)) n)))))

(defun w64-assemble (items)
  "Assemble ITEMS to a byte vector.  Returns (values bytes label-offsets)."
  (let ((label-alist nil) (pos 0))
    (dolist (it items)
      (when (eq (first it) :label) (push (cons (second it) pos) label-alist))
      (incf pos (w64-item-size it pos)))
    (flet ((target (name)
             (or (cdr (assoc name label-alist))
                 (error "boot-win64: undefined label ~S" name))))
      (let ((out (make-array pos :element-type '(unsigned-byte 8) :initial-element #xCC))
            (p 0))
        (flet ((put (b) (setf (aref out p) (logand b #xFF)) (incf p))
               (put-rel32 (dest next)
                 (dolist (b (w64-le32 (- dest next))) (setf (aref out p) b) (incf p))))
          (dolist (it items)
            (ecase (first it)
              (:label nil)
              (:bytes (dolist (b (rest it)) (put b)))
              (:raw (loop for b across (second it) do (put b)))
              (:zeros (dotimes (i (second it)) (put 0)))
              (:align (dotimes (i (w64-item-size it p)) (put #x90)))
              (:jmp (put #xE9)
               (put-rel32 (target (second it)) (+ p 4)))
              (:jcc (put #x0F) (put (+ #x80 (second it)))
               (put-rel32 (target (third it)) (+ p 4)))
              (:call-iat (put #xFF) (put #x15)
               (put-rel32 (+ (target :iat) (* 8 (win64-import-slot (second it))))
                          (+ p 4)))
              ;; (:rip-load REG LABEL) / (:rip-store REG LABEL): mov REG,[rip+LABEL]
              ;; / mov [rip+LABEL],REG, for REG 0..7 (rax..rdi).
              ((:rip-load :rip-store)
               (put #x48) (put (if (eq (first it) :rip-load) #x8B #x89))
               (put (logior #x05 (ash (second it) 3)))
               (put-rel32 (target (third it)) (+ p 4))))))
        (values out label-alist)))))

;;; ============================================================
;;; The syscall shim.
;;; Frame after the prologue (rbp-relative): [rbp-8] rdi, [rbp-16] rsi,
;;; [rbp-24] rdx, [rbp-32] r10, [rbp-40] r8, [rbp-48] r9, [rbp-56] syscall nr.
;;; Locals: [rsp+0x20] the 5th Win32 argument, [rsp+0x28] the 6th, [rsp+0x30] a
;;; dword count, [rsp+0x38] a qword temporary.  The locals and every Win32 call
;;; are on the shim's private stack (+WIN64-WIN32-STACK-TOP+, 16-aligned, then
;;; `sub rsp,0x40'); only the return address and the seven pushes touch the
;;; Lisp stack, and the call site puts those below a 128-byte red zone.
;;; ============================================================

(defun win64-trace-items ()
  "Write \"s\" + three hex digits of the syscall number + newline to stderr.
   Uses the shim's locals ([rsp+0x38] buffer, [rsp+0x30] count); RAX is the
   number on entry and again on exit."
  `((:bytes #xC6 #x44 #x24 #x38 #x73)               ; mov byte [rsp+0x38],'s'
    ,@(loop for shift in '(8 4 0)
            for disp from #x39
            append `((:bytes #x48 #x89 #xC1)        ; mov rcx,rax
                     (:bytes #x48 #xC1 #xE9 ,shift) ; shr rcx,shift
                     (:bytes #x83 #xE1 #x0F)        ; and ecx,0xF
                     (:bytes #x80 #xC1 #x30)        ; add cl,'0'
                     (:bytes #x80 #xF9 #x39)        ; cmp cl,'9'
                     (:bytes #x76 #x03)             ; jbe +3
                     (:bytes #x80 #xC1 #x07)        ; add cl,7   ('A'-'9'-1)
                     (:bytes #x88 #x4C #x24 ,disp))) ; mov [rsp+disp],cl
    (:bytes #xC6 #x44 #x24 #x3C #x0A)               ; mov byte [rsp+0x3C],10
    (:bytes #xB9 ,@(w64-le32 -12))                  ; mov ecx,STD_ERROR_HANDLE
    (:call-iat "GetStdHandle")
    (:bytes #x48 #x89 #xC1)                         ; mov rcx,rax
    (:bytes #x48 #x8D #x54 #x24 #x38)               ; lea rdx,[rsp+0x38]
    (:bytes #x41 #xB8 #x05 #x00 #x00 #x00)          ; mov r8d,5
    (:bytes #x4C #x8D #x4C #x24 #x30)               ; lea r9,[rsp+0x30]
    (:bytes #x48 #xC7 #x44 #x24 #x20 0 0 0 0)       ; mov qword [rsp+0x20],0
    (:call-iat "WriteFile")
    (:bytes #x48 #x8B #x45 #xC8)))                  ; mov rax,[rbp-56]

(defun win64-shim-items ()
  (let ((dispatch '((0 . :h-read) (1 . :h-write) (9 . :h-mmap) (11 . :h-munmap)
                    (13 . :h-zero) (14 . :h-zero) (35 . :h-nanosleep)
                    (39 . :h-getpid) (60 . :h-exit) (228 . :h-clock)
                    (231 . :h-exit))))
    `((:label :shim)
      ;; push rbp; mov rbp,rsp; push rdi,rsi,rdx,r10,r8,r9,rax
      (:bytes #x55 #x48 #x89 #xE5 #x57 #x56 #x52 #x41 #x52 #x41 #x50 #x41 #x51 #x50)
      ;; Onto the shim's private stack (16-aligned), then sub rsp,0x40; mov rax,[rbp-56].
      ;; LEAVE in the epilogue restores the Lisp RSP from RBP.
      (:bytes #x48 #xBC ,@(w64-le64 +win64-win32-stack-top+)) ; mov rsp,imm64
      (:bytes #x48 #x83 #xEC #x40 #x48 #x8B #x45 #xC8)
      ,@(when *win64-trace-syscalls* (win64-trace-items))
      ,@(loop for (nr . lbl) in dispatch
              append `((:bytes #x48 #x3D ,@(w64-le32 nr)) (:jcc 4 ,lbl)))
      (:jmp :h-enosys)

      ;; read(fd, buf, count): fd 0 only, from the console or pipe on stdin.
      ;; A failed ReadFile reads as end-of-file (0 bytes).
      (:label :h-read)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x48 #x85 #xC9)                      ; test rcx,rcx
      (:jcc 5 :h-ebadf)                            ; jnz
      (:bytes #xB9 ,@(w64-le32 -10))               ; mov ecx,STD_INPUT_HANDLE
      (:call-iat "GetStdHandle")
      (:bytes #x48 #x89 #xC1)                      ; mov rcx,rax
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  buf
      (:bytes #x4C #x8B #x45 #xE8)                 ; mov r8,[rbp-24]   count
      (:bytes #x4C #x8D #x4C #x24 #x30)            ; lea r9,[rsp+0x30]
      (:bytes #xC7 #x44 #x24 #x30 0 0 0 0)         ; mov dword [rsp+0x30],0
      (:bytes #x48 #xC7 #x44 #x24 #x20 0 0 0 0)    ; mov qword [rsp+0x20],0
      (:call-iat "ReadFile")
      (:bytes #x8B #x44 #x24 #x30)                 ; mov eax,[rsp+0x30]
      (:jmp :done)

      ;; write(fd, buf, count): fd 1 -> STD_OUTPUT, fd 2 -> STD_ERROR.
      (:label :h-write)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x48 #x83 #xF9 #x01)                 ; cmp rcx,1
      (:jcc 4 :w-out)                              ; je
      (:bytes #x48 #x83 #xF9 #x02)                 ; cmp rcx,2
      (:jcc 5 :h-ebadf)                            ; jne
      (:bytes #xB9 ,@(w64-le32 -12))               ; mov ecx,STD_ERROR_HANDLE
      (:jmp :w-go)
      (:label :w-out)
      (:bytes #xB9 ,@(w64-le32 -11))               ; mov ecx,STD_OUTPUT_HANDLE
      (:label :w-go)
      (:call-iat "GetStdHandle")
      (:bytes #x48 #x89 #xC1)                      ; mov rcx,rax
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  buf
      (:bytes #x4C #x8B #x45 #xE8)                 ; mov r8,[rbp-24]   count
      (:bytes #x4C #x8D #x4C #x24 #x30)            ; lea r9,[rsp+0x30]
      (:bytes #x48 #xC7 #x44 #x24 #x20 0 0 0 0)    ; mov qword [rsp+0x20],0
      (:call-iat "WriteFile")
      (:bytes #x85 #xC0)                           ; test eax,eax
      (:jcc 4 :h-eio)                              ; je -> failure
      (:bytes #x8B #x44 #x24 #x30)                 ; mov eax,[rsp+0x30]
      (:jmp :done)

      ;; mmap(addr, len, prot, ...): anonymous memory via VirtualAlloc.  A
      ;; non-null hint that cannot be honoured is retried with no hint, as the
      ;; kernel does; callers that need a fixed address check the result.
      ;; PROT_RWX maps to PAGE_EXECUTE_READWRITE, everything else to READWRITE.
      (:label :h-mmap)
      (:bytes #xC7 #x44 #x24 #x38 4 0 0 0)         ; mov dword [rsp+0x38],PAGE_READWRITE
      (:bytes #x8B #x45 #xE8)                      ; mov eax,[rbp-24]  prot
      (:bytes #x83 #xF8 #x07)                      ; cmp eax,7
      (:jcc 5 :m-try)                              ; jne
      (:bytes #xC7 #x44 #x24 #x38 #x40 0 0 0)      ; mov dword [rsp+0x38],PAGE_EXECUTE_READWRITE
      (:label :m-try)
      (:bytes #x48 #x83 #x7D #xF8 #x00)            ; cmp qword [rbp-8],0
      (:jcc 5 :m-hint)                             ; jne: the caller named an address
      ;; No hint: take the next bump address, as Linux would place it high.
      (:rip-load 1 :mmap-bump)                     ; mov rcx,[rip+bump]
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  len
      (:bytes #x41 #xB8 #x00 #x30 #x00 #x00)       ; mov r8d,MEM_RESERVE|MEM_COMMIT
      (:bytes #x44 #x8B #x4C #x24 #x38)            ; mov r9d,[rsp+0x38]
      (:call-iat "VirtualAlloc")
      (:bytes #x48 #x85 #xC0)                      ; test rax,rax
      (:jcc 4 :m-hint)                             ; je: occupied, fall back to NULL below
      (:bytes #x48 #x8B #x4D #xF0)                 ; mov rcx,[rbp-16]  len
      (:bytes #x48 #x81 #xC1 #xFF #xFF #x00 #x00)  ; add rcx,0xFFFF
      (:bytes #x48 #x81 #xE1 #x00 #x00 #xFF #xFF)  ; and rcx,-0x10000  (64 KB granule)
      (:bytes #x48 #x01 #xC1)                      ; add rcx,rax
      (:rip-store 1 :mmap-bump)                    ; mov [rip+bump],rcx
      (:jmp :done)
      (:label :m-hint)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]   addr
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  len
      (:bytes #x41 #xB8 #x00 #x30 #x00 #x00)       ; mov r8d,MEM_RESERVE|MEM_COMMIT
      (:bytes #x44 #x8B #x4C #x24 #x38)            ; mov r9d,[rsp+0x38]
      (:call-iat "VirtualAlloc")
      (:bytes #x48 #x85 #xC0)                      ; test rax,rax
      (:jcc 5 :done)                               ; jnz
      (:bytes #x48 #x83 #x7D #xF8 #x00)            ; cmp qword [rbp-8],0
      (:jcc 4 :h-enomem)                           ; je: there was no hint to drop
      (:bytes #x31 #xC9)                           ; xor ecx,ecx
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]
      (:bytes #x41 #xB8 #x00 #x30 #x00 #x00)       ; mov r8d,MEM_RESERVE|MEM_COMMIT
      (:bytes #x44 #x8B #x4C #x24 #x38)            ; mov r9d,[rsp+0x38]
      (:call-iat "VirtualAlloc")
      (:bytes #x48 #x85 #xC0)                      ; test rax,rax
      (:jcc 4 :h-enomem)                           ; je
      (:jmp :done)

      ;; munmap(addr, len): VirtualFree MEM_RELEASE (the region is freed whole).
      (:label :h-munmap)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x31 #xD2)                           ; xor edx,edx
      (:bytes #x41 #xB8 #x00 #x80 #x00 #x00)       ; mov r8d,MEM_RELEASE
      (:call-iat "VirtualFree")
      (:bytes #x85 #xC0)                           ; test eax,eax
      (:jcc 4 :h-einval)                           ; je -> failure
      (:bytes #x31 #xC0)                           ; xor eax,eax
      (:jmp :done)

      ;; clock_gettime(clk, ts): wall clock from GetSystemTimeAsFileTime.
      ;; FILETIME counts 100 ns units from 1601; the Unix epoch is
      ;; 116444736000000000 of them.
      (:label :h-clock)
      (:bytes #x48 #x8D #x4C #x24 #x38)            ; lea rcx,[rsp+0x38]
      (:call-iat "GetSystemTimeAsFileTime")
      (:bytes #x48 #x8B #x44 #x24 #x38)            ; mov rax,[rsp+0x38]
      (:bytes #x49 #xBA ,@(w64-le64 116444736000000000)) ; mov r10,epoch offset
      (:bytes #x4C #x29 #xD0)                      ; sub rax,r10
      (:bytes #x31 #xD2)                           ; xor edx,edx
      (:bytes #xB9 ,@(w64-le32 10000000))          ; mov ecx,10^7
      (:bytes #x48 #xF7 #xF1)                      ; div rcx        rax=sec rdx=rem
      (:bytes #x48 #x6B #xD2 #x64)                 ; imul rdx,rdx,100   nsec
      (:bytes #x48 #x8B #x4D #xF0)                 ; mov rcx,[rbp-16]  ts
      (:bytes #x48 #x89 #x01)                      ; mov [rcx],rax
      (:bytes #x48 #x89 #x51 #x08)                 ; mov [rcx+8],rdx
      (:bytes #x31 #xC0)                           ; xor eax,eax
      (:jmp :done)

      ;; nanosleep(req, rem): Sleep(sec*1000 + nsec/1000000).  rem is not written.
      (:label :h-nanosleep)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]   req
      (:bytes #x48 #x8B #x01)                      ; mov rax,[rcx]     sec
      (:bytes #x48 #x69 #xC0 #xE8 #x03 #x00 #x00)  ; imul rax,rax,1000
      (:bytes #x48 #x89 #x44 #x24 #x38)            ; mov [rsp+0x38],rax
      (:bytes #x48 #x8B #x41 #x08)                 ; mov rax,[rcx+8]   nsec
      (:bytes #xB9 ,@(w64-le32 1000000))           ; mov ecx,10^6
      (:bytes #x31 #xD2)                           ; xor edx,edx
      (:bytes #x48 #xF7 #xF1)                      ; div rcx
      (:bytes #x48 #x03 #x44 #x24 #x38)            ; add rax,[rsp+0x38]
      (:bytes #x89 #xC1)                           ; mov ecx,eax
      (:call-iat "Sleep")
      (:bytes #x31 #xC0)                           ; xor eax,eax
      (:jmp :done)

      ;; exit(code) / exit_group(code): ExitProcess, which does not return.
      (:label :h-exit)
      (:bytes #x8B #x4D #xF8)                      ; mov ecx,[rbp-8]
      (:call-iat "ExitProcess")
      (:jmp :done)

      (:label :h-getpid)
      (:call-iat "GetCurrentProcessId")
      (:jmp :done)

      ;; rt_sigaction / rt_sigprocmask: Windows delivers no POSIX signals.
      (:label :h-zero)
      (:bytes #x31 #xC0)                           ; xor eax,eax
      (:jmp :done)

      (:label :h-enomem)
      (:bytes #x48 #xC7 #xC0 ,@(w64-le32 -12))     ; mov rax,-ENOMEM
      (:jmp :done)
      (:label :h-einval)
      (:bytes #x48 #xC7 #xC0 ,@(w64-le32 -22))     ; mov rax,-EINVAL
      (:jmp :done)
      (:label :h-ebadf)
      (:bytes #x48 #xC7 #xC0 ,@(w64-le32 -9))      ; mov rax,-EBADF
      (:jmp :done)
      (:label :h-eio)
      (:bytes #x48 #xC7 #xC0 ,@(w64-le32 -5))      ; mov rax,-EIO
      (:jmp :done)
      (:label :h-enosys)
      (:bytes #x48 #xC7 #xC0 ,@(w64-le32 -38))     ; mov rax,-ENOSYS
      (:jmp :done)

      ;; Epilogue: restore the argument registers the Win32 calls clobbered (a
      ;; Linux syscall preserves them), then leave and return.  RAX is untouched.
      (:label :done)
      (:bytes #x48 #x8B #x7D #xF8                  ; mov rdi,[rbp-8]
              #x48 #x8B #x75 #xF0                  ; mov rsi,[rbp-16]
              #x48 #x8B #x55 #xE8                  ; mov rdx,[rbp-24]
              #x4C #x8B #x55 #xE0                  ; mov r10,[rbp-32]
              #x4C #x8B #x45 #xD8                  ; mov r8,[rbp-40]
              #x4C #x8B #x4D #xD0                  ; mov r9,[rbp-48]
              #xC9 #xC3))))                        ; leave; ret

;;; ============================================================
;;; The prologue and the stub.
;;; ============================================================

(defun win64-heap-tail-bytes ()
  "The shared heap tail (argc/argv, GC metadata, registers) as bytes.  It is
   position independent: absolute addresses and rel8 jumps inside it only."
  (let ((b (make-mvm-buffer)))
    (emit-linux-x64-heap-tail b)
    (coerce (mvm-buffer-used-bytes b) '(vector (unsigned-byte 8)))))

(defun win64-prologue-items ()
  `((:label :prologue)
    (:bytes #x48 #x83 #xEC #x28)                         ; sub rsp,0x28  (shadow space + align)
    (:bytes #x48 #xB9 ,@(w64-le64 +linux-x64-fixed-heap-base+)) ; mov rcx,heap base
    (:bytes #xBA ,@(w64-le32 +linux-x64-heap-size+))     ; mov edx,heap size
    (:bytes #x41 #xB8 #x00 #x30 #x00 #x00)               ; mov r8d,MEM_RESERVE|MEM_COMMIT
    (:bytes #x41 #xB9 #x04 #x00 #x00 #x00)               ; mov r9d,PAGE_READWRITE
    (:call-iat "VirtualAlloc")
    (:bytes #x48 #x85 #xC0)                              ; test rax,rax
    (:jcc 5 :p-ok)                                       ; jnz
    (:bytes #xB9 1 0 0 0)                                ; mov ecx,1
    (:call-iat "ExitProcess")                            ; no heap: nothing can run
    (:label :p-ok)
    ;; The JIT arena, as the Linux entry maps it: one RWX region at a fixed base,
    ;; its bump word [0x10000FB8] set to the base, or 0 if refused.  Done even
    ;; with the JIT off, because the Linux image has this word set at startup and
    ;; the Windows image did not (the GC fault in the win64 notes).
    (:bytes #x48 #x89 #xC5)                              ; mov rbp,rax  (heap base)
    (:bytes #x48 #xB9 ,@(w64-le64 +linux-x64-jit-arena-base+)) ; mov rcx,arena base
    (:bytes #xBA ,@(w64-le32 +linux-x64-jit-arena-size+))  ; mov edx,arena size
    (:bytes #x41 #xB8 #x00 #x30 #x00 #x00)               ; mov r8d,MEM_RESERVE|MEM_COMMIT
    (:bytes #x41 #xB9 #x40 #x00 #x00 #x00)               ; mov r9d,PAGE_EXECUTE_READWRITE
    (:call-iat "VirtualAlloc")
    (:bytes #x48 #xB9 ,@(w64-le64 +linux-x64-jit-arena-base+)) ; mov rcx,arena base
    (:bytes #x48 #x39 #xC8)                              ; cmp rax,rcx
    (:jcc 4 :p-arena)                                    ; je: keep the base
    (:bytes #x31 #xC0)                                   ; xor eax,eax  (refused -> 0)
    (:label :p-arena)
    (:bytes #x48 #x89 #x04 #x25 ,@(w64-le32 +linux-x64-jit-bump-slot+)) ; mov [bump],rax
    ;; The Lisp stack, and on top of it the block a Linux process starts with:
    ;;     [argc] [argv0] [NULL] [envp0 .. envpN-1] [NULL]
    ;; with RSP (the GC's stack base) pointing at argc.  The runtime reads argv
    ;; and envp from just above the stack base (lib/cli-toplevel.lisp), so the
    ;; block is not optional: on the loader's stack those reads walked ntdll's
    ;; frames.  argc is 1 and argv0 is the whole command line from
    ;; GetCommandLineA; the Windows build splits it (%CLI-COLLECT-ARGV).  envp
    ;; points into GetEnvironmentStringsA's NAME=value\0...\0\0 block.
    (:bytes #x48 #xB9 ,@(w64-le64 +win64-stack-va+))      ; mov rcx,stack base
    (:bytes #xBA ,@(w64-le32 +win64-stack-size+))        ; mov edx,stack size
    (:bytes #x41 #xB8 #x00 #x30 #x00 #x00)               ; mov r8d,MEM_RESERVE|MEM_COMMIT
    (:bytes #x41 #xB9 #x04 #x00 #x00 #x00)               ; mov r9d,PAGE_READWRITE
    (:call-iat "VirtualAlloc")
    (:bytes #x48 #x85 #xC0)                              ; test rax,rax
    (:jcc 5 :p-stack-ok)                                 ; jnz
    (:bytes #xB9 1 0 0 0)                                ; mov ecx,1
    (:call-iat "ExitProcess")                            ; no stack: nothing can run
    (:label :p-stack-ok)
    ;; Tell Windows where the stack is (NT_TIB StackLimit / StackBase), as a
    ;; fiber switch does, then move onto it.
    (:bytes #x65 #x48 #x89 #x04 #x25 #x10 0 0 0)         ; mov gs:[0x10],rax  StackLimit
    (:bytes #x48 #x05 ,@(w64-le32 +win64-stack-size+))   ; add rax,stack size
    (:bytes #x65 #x48 #x89 #x04 #x25 #x08 0 0 0)         ; mov gs:[0x08],rax  StackBase
    (:bytes #x49 #x89 #xC6)                              ; mov r14,rax  (top)
    (:bytes #x48 #x89 #xC4)                              ; mov rsp,rax
    (:bytes #x48 #x83 #xEC #x20)                         ; sub rsp,0x20 (shadow, aligned)
    (:call-iat "GetCommandLineA")
    (:bytes #x49 #x89 #xC4)                              ; mov r12,rax  (command line)
    (:call-iat "GetEnvironmentStringsA")
    (:bytes #x49 #x89 #xC5)                              ; mov r13,rax  (environment block)
    ;; Count the environment strings into RCX.
    (:bytes #x31 #xC9)                                   ; xor ecx,ecx
    (:bytes #x4C #x89 #xEA)                              ; mov rdx,r13
    (:label :p-cnt)
    (:bytes #x80 #x3A #x00)                              ; cmp byte [rdx],0
    (:jcc 4 :p-cnt-done)                                 ; je: the block's final NUL
    (:bytes #x48 #xFF #xC1)                              ; inc rcx
    (:label :p-cnt-skip)
    (:bytes #x48 #xFF #xC2)                              ; inc rdx
    (:bytes #x80 #x7A #xFF #x00)                         ; cmp byte [rdx-1],0
    (:jcc 5 :p-cnt-skip)                                 ; jne: still inside this string
    (:jmp :p-cnt)
    (:label :p-cnt-done)
    ;; RDI = block base = (top - 8*(n+4)) & -16.
    (:bytes #x48 #x8D #x04 #xCD #x20 0 0 0)              ; lea rax,[rcx*8+0x20]
    (:bytes #x4C #x89 #xF7)                              ; mov rdi,r14
    (:bytes #x48 #x29 #xC7)                              ; sub rdi,rax
    (:bytes #x48 #x83 #xE7 #xF0)                         ; and rdi,-16
    (:bytes #x48 #xC7 #x07 1 0 0 0)                      ; mov qword [rdi],1      argc
    (:bytes #x4C #x89 #x67 #x08)                         ; mov [rdi+8],r12        argv0
    (:bytes #x48 #xC7 #x47 #x10 0 0 0 0)                 ; mov qword [rdi+16],0   argv NULL
    (:bytes #x48 #x8D #x77 #x18)                         ; lea rsi,[rdi+24]       envp
    (:bytes #x4C #x89 #xEA)                              ; mov rdx,r13
    (:label :p-fill)
    (:bytes #x80 #x3A #x00)                              ; cmp byte [rdx],0
    (:jcc 4 :p-fill-done)                                ; je
    (:bytes #x48 #x89 #x16)                              ; mov [rsi],rdx
    (:bytes #x48 #x83 #xC6 #x08)                         ; add rsi,8
    (:label :p-fill-skip)
    (:bytes #x48 #xFF #xC2)                              ; inc rdx
    (:bytes #x80 #x7A #xFF #x00)                         ; cmp byte [rdx-1],0
    (:jcc 5 :p-fill-skip)                                ; jne
    (:jmp :p-fill)
    (:label :p-fill-done)
    (:bytes #x48 #xC7 #x06 0 0 0 0)                      ; mov qword [rsi],0      envp NULL
    ;; The add rsp,0x28 / sub rsp,8 below leave RSP = RDI, pointing at argc.
    (:bytes #x48 #x8D #x67 #xE0)                         ; lea rsp,[rdi-0x20]
    (:bytes #x48 #x89 #xE8)                              ; mov rax,rbp  (heap base back)
    (:bytes #x48 #x83 #xC4 #x28)                         ; add rsp,0x28  (rax = heap base)
    ;; RSP is now the block base (16-byte aligned, as a Linux process enters).
    (:bytes #x48 #x83 #xEC #x08)                         ; sub rsp,8
    ;; argc = 1 at [0x10000200], matching the block's argc.
    (:bytes #xC7 #x04 #x25 ,@(w64-le32 #x10000200) 1 0 0 0)
    (:bytes #x48 #xC7 #xC3 1 0 0 0)                      ; mov rbx,1   (argc, for the tail)
    (:bytes #x45 #x31 #xED #x45 #x31 #xF6 #x45 #x31 #xFF) ; r13, r14, r15 = 0
    (:raw ,(win64-heap-tail-bytes))))

(defun win64-stub-items ()
  `((:jmp :prologue)
    (:label :iat)
    (:zeros ,(* 8 (length *win64-imports*)))
    ;; The next address a hint-less mmap tries (the shim's h-mmap).  In .text,
    ;; which is writable.
    (:label :mmap-bump)
    (:bytes ,@(w64-le64 +win64-mmap-base+))
    (:align 16)
    ,@(win64-shim-items)
    (:align 16)
    ,@(win64-prologue-items)))

(defvar *win64-stub-cache* nil
  "(stub-bytes shim-offset iat-offset), assembled once per process: the LAYOUT
   the translator is told about.  EMIT-WIN64-ENTRY assembles the real bytes.")

(defun win64-stub ()
  (unless *win64-stub-cache*
    (multiple-value-bind (bytes labels) (w64-assemble (win64-stub-items))
      (setf *win64-stub-cache*
            (list bytes (cdr (assoc :shim labels)) (cdr (assoc :iat labels))))))
  (first *win64-stub-cache*))

(defun win64-shim-offset ()
  (win64-stub)
  (second *win64-stub-cache*))

(defun win64-iat-offset ()
  (win64-stub)
  (third *win64-stub-cache*))

(defun win64-shim-va ()
  "Virtual address of the syscall shim.  translate-x64 calls it for every syscall
   (its *X64-WIN-SHIM-VA*)."
  (+ +win64-text-va+ (win64-shim-offset)))

(defun win64-native-code-offset ()
  "Where native code starts in a Windows image: the header, the stub, and the
   5-byte JMP cross.lisp emits after the stub.  Computed from the stub, as the
   ELF value is."
  (+ +win64-header-size+ (length (win64-stub)) 5))

(defun emit-win64-entry (buf)
  "Emit the Windows boot stub.  cross.lisp appends the JMP to kernel-main.
   ASSEMBLED AFRESH HERE, not taken from the cache: the cache is made early (the
   build needs the shim address and the native-code offset before translating),
   but the shared heap tail bakes values the build sets later -- above all
   *LINUX-X64-R14-OFFSET*, the allocation limit.  A stub taken from the early
   cache had the default limit (the whole heap), so the collector never ran and
   allocation marched into the GC metadata.  The layout (length, shim and IAT
   offsets) must not have moved since the cache was made; check it."
  (win64-stub)
  (multiple-value-bind (bytes labels) (w64-assemble (win64-stub-items))
    (unless (and (= (length bytes) (length (first *win64-stub-cache*)))
                 (eql (cdr (assoc :shim labels)) (second *win64-stub-cache*))
                 (eql (cdr (assoc :iat labels)) (third *win64-stub-cache*)))
      (error "boot-win64: the stub's layout changed after the translator was told ~
              where the shim and native code are (~D -> ~D bytes)"
             (length (first *win64-stub-cache*)) (length bytes)))
    (loop for b across bytes do (mvm-emit-byte buf b))))

(defun win64-boot-descriptor ()
  (list :arch :x86-64
        :entry-fn #'emit-win64-entry
        :load-addr +win64-image-base+
        :elf-format :win64))

;;; ============================================================
;;; The PE32+ wrapper.
;;; ============================================================

(defun w64-build-idata (idata-rva iat-rva)
  "The import directory for KERNEL32.dll, as bytes that live at IDATA-RVA.
   The IAT (in .text, at IAT-RVA) is the caller's to fill.  Returns
   (values bytes hint-name-rvas): the RVA of each import's hint/name entry, which
   is also the value the IAT starts out holding."
  (let* ((n (length *win64-imports*))
         (desc-size 40)                              ; two IMAGE_IMPORT_DESCRIPTORs
         (ilt-off desc-size)
         (names-off (+ ilt-off (* 8 (1+ n))))
         (hint-offs nil)
         (cursor names-off))
    (dolist (name *win64-imports*)
      (push cursor hint-offs)
      (incf cursor (w64-pad (+ 2 (length name) 1) 2)))   ; WORD hint, name, NUL, even
    (setf hint-offs (nreverse hint-offs))
    (let* ((dll-name "KERNEL32.dll")
           (dll-off cursor)
           (total (+ dll-off (length dll-name) 1))
           (v (make-array total :element-type '(unsigned-byte 8) :initial-element 0)))
      (flet ((put32 (off value)
               (loop for b in (w64-le32 value) for k from off do (setf (aref v k) b)))
             (put64 (off value)
               (loop for b in (w64-le64 value) for k from off do (setf (aref v k) b))))
        ;; IMAGE_IMPORT_DESCRIPTOR: OriginalFirstThunk, TimeDateStamp,
        ;; ForwarderChain, Name, FirstThunk.  The second descriptor is the terminator.
        (put32 0 (+ idata-rva ilt-off))
        (put32 12 (+ idata-rva dll-off))
        (put32 16 iat-rva)
        (loop for hn in hint-offs
              for i from 0
              do (put64 (+ ilt-off (* 8 i)) (+ idata-rva hn))
                 (loop for c across (nth i *win64-imports*)
                       for k from (+ hn 2)
                       do (setf (aref v k) (char-code c))))
        (loop for c across dll-name for k from dll-off
              do (setf (aref v k) (char-code c)))
        (values v (mapcar (lambda (hn) (+ idata-rva hn)) hint-offs))))))

(defun wrap-in-pe64-win (raw-bytes)
  "Wrap the Modus image in a PE32+ console executable at image base #x400000.
   Sections: .text (the raw image, RWX, RVA #x1000), .idata (the import
   directory), .bss (zero-filled up to #x20000000, like the ELF's p_memsz tail)."
  (let* ((raw-len (length raw-bytes))
         (text-rva #x1000)
         (text-raw-off #x1000)
         (text-raw-size (w64-pad raw-len #x200))
         (idata-rva (w64-pad (+ text-rva raw-len) #x1000))
         (iat-rva (+ text-rva (win64-iat-offset))))
    (multiple-value-bind (idata hint-rvas) (w64-build-idata idata-rva iat-rva)
      (let* ((idata-len (length idata))
             (idata-raw-off (+ text-raw-off text-raw-size))
             (idata-raw-size (w64-pad idata-len #x200))
             (bss-rva (w64-pad (+ idata-rva idata-len) #x1000))
             (bss-vsize (max 0 (- +win64-bss-end-va+ (+ +win64-image-base+ bss-rva))))
             (size-of-image (w64-pad (+ bss-rva bss-vsize) #x1000))
             (file-size (+ idata-raw-off idata-raw-size))
             (out (make-array file-size :element-type '(unsigned-byte 8) :initial-element 0)))
        (labels ((w8 (off v) (setf (aref out off) (logand v #xFF)))
                 (w16 (off v) (loop for i from 0 below 2 do (w8 (+ off i) (ldb (byte 8 (* 8 i)) v))))
                 (w32 (off v) (loop for b in (w64-le32 v) for k from off do (setf (aref out k) b)))
                 (w64 (off v) (loop for b in (w64-le64 v) for k from off do (setf (aref out k) b)))
                 (str (off s) (loop for c across s for k from off do (w8 k (char-code c)))))
          ;; The raw image at file offset #x1000.
          (replace out raw-bytes :start1 text-raw-off)
          ;; The IAT in .text starts out holding the hint/name RVAs; the loader
          ;; overwrites each slot with the resolved function address.
          (loop for rva in hint-rvas
                for i from 0
                do (loop for b in (w64-le64 rva)
                         for k from (+ text-raw-off (win64-iat-offset) (* 8 i))
                         do (setf (aref out k) b)))
          ;; The import directory.
          (replace out idata :start1 idata-raw-off)
          ;; ---- DOS header: "MZ", e_lfanew = #x80 ----
          (w8 0 (char-code #\M)) (w8 1 (char-code #\Z))
          (w32 #x3C #x80)
          ;; ---- PE signature and COFF header at #x80 ----
          (str #x80 "PE")
          (let ((coff #x84))
            (w16 coff #x8664)                  ; Machine = AMD64
            (w16 (+ coff 2) 3)                 ; NumberOfSections
            (w32 (+ coff 4) 0)                 ; TimeDateStamp
            (w32 (+ coff 8) 0)                 ; PointerToSymbolTable
            (w32 (+ coff 12) 0)                ; NumberOfSymbols
            (w16 (+ coff 16) 240)              ; SizeOfOptionalHeader (PE32+)
            (w16 (+ coff 18) #x0023))          ; EXECUTABLE | LARGE_ADDRESS_AWARE | RELOCS_STRIPPED
          ;; ---- PE32+ optional header at #x98 (240 bytes) ----
          (let ((opt #x98))
            (w16 opt #x20B)                    ; Magic = PE32+
            (w8 (+ opt 2) 1)                   ; MajorLinkerVersion
            (w32 (+ opt 4) text-raw-size)      ; SizeOfCode
            (w32 (+ opt 8) idata-raw-size)     ; SizeOfInitializedData
            (w32 (+ opt 12) bss-vsize)         ; SizeOfUninitializedData
            (w32 (+ opt 16) text-rva)          ; AddressOfEntryPoint: the stub's first byte
            (w32 (+ opt 20) text-rva)          ; BaseOfCode
            (w64 (+ opt 24) +win64-image-base+) ; ImageBase
            (w32 (+ opt 32) #x1000)            ; SectionAlignment
            (w32 (+ opt 36) #x200)             ; FileAlignment
            (w16 (+ opt 40) 6)                 ; MajorOperatingSystemVersion 6.0
            (w16 (+ opt 48) 6)                 ; MajorSubsystemVersion 6.0
            (w32 (+ opt 56) size-of-image)     ; SizeOfImage
            (w32 (+ opt 60) #x400)             ; SizeOfHeaders
            (w16 (+ opt 68) 3)                 ; Subsystem = console
            (w16 (+ opt 70) #x8100)            ; NX_COMPAT | TERMINAL_SERVER_AWARE
            (w64 (+ opt 72) #x1000000)         ; SizeOfStackReserve (16 MB)
            (w64 (+ opt 80) #x100000)          ; SizeOfStackCommit
            (w64 (+ opt 88) #x100000)          ; SizeOfHeapReserve
            (w64 (+ opt 96) #x1000)            ; SizeOfHeapCommit
            (w32 (+ opt 108) 16)               ; NumberOfRvaAndSizes
            ;; DataDirectory[1] = import table; the array starts at opt+112.
            (w32 (+ opt 112 8) idata-rva)
            (w32 (+ opt 112 12) 40))
          ;; ---- Section table at #x188: .text, .idata, .bss ----
          (flet ((section (i name vsize va rawsize rawptr chars)
                   (let ((o (+ #x188 (* 40 i))))
                     (str o name)
                     (w32 (+ o 8) vsize) (w32 (+ o 12) va)
                     (w32 (+ o 16) rawsize) (w32 (+ o 20) rawptr)
                     (w32 (+ o 36) chars))))
            (section 0 ".text" raw-len text-rva text-raw-size text-raw-off #xE0000060)
            (section 1 ".idata" idata-len idata-rva idata-raw-size idata-raw-off #xC0000040)
            (section 2 ".bss" bss-vsize bss-rva 0 0 #xC0000080)))
        out))))

(defun win64-image-check ()
  "Build-time invariants the boot stub depends on."
  (unless (= (win64-iat-offset) 5)
    (error "boot-win64: the IAT is at ~D, not 5 (the JMP at the start must be 5 bytes)"
           (win64-iat-offset)))
  (unless (= +win64-text-va+ (+ +win64-image-base+ +win64-header-size+))
    (error "boot-win64: text VA and header size disagree")))
