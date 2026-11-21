;;;; boot-win64.lisp - Windows x86-64 hosted entry for Modus: a PE32+ console EXE.
;;;;
;;;; The hosted Linux CLI (boot-linux-x64.lisp) is an ELF whose runtime issues
;;;; Linux system calls directly.  Windows has no stable system-call interface,
;;;; so this image keeps the SAME runtime and the SAME syscall ABI, and replaces
;;;; the one place a syscall leaves the image: every `syscall' instruction the
;;;; translator emits becomes `call SHIM' (translate-x64's EMIT-X64-SYSCALL),
;;;; where SHIM takes the Linux number in RAX and the arguments in RDI RSI RDX
;;;; R10 R8 R9, dispatches to kernel32 through the import address table, and
;;;; returns a Linux-style result (-errno on failure).  It preserves every
;;;; register a Linux syscall preserves (all but RAX, RCX and R11), and like
;;;; SYSCALL it leaves the Lisp stack alone: Win32 runs on the shim's own stack
;;;; and the call site steps over the red zone.  docs/win64.md is the overview.
;;;;
;;;; IMAGE LAYOUT.  The PE headers occupy file bytes [0, 0x400) and the code
;;;; starts at file offset 0x1000, which the .text section maps at RVA 0x1000.
;;;; So the image byte at raw offset O sits at virtual address
;;;;     #x400000 + #x1000 + O
;;;; exactly as the ELF wrapper's raw bytes sit at load-addr + header-total.
;;;; The image base is FIXED (no relocations): the runtime's absolute addresses
;;;; are compiled in, as they are for the ELF.
;;;;
;;;; MEMORY.  Every region the runtime knows by address is where the ELF has it:
;;;;     0x401000      .text: boot stub, native code, constants (RWX)
;;;;     ..0x20000000  .bss: the fixed low block (0x10000000..), zero-filled
;;;;     0x2000000000  heap, VirtualAlloc'd by the prologue
;;;;     0x3000000000  JIT arena (RWX), bump word at 0x10000FB8
;;;;     0x4000000000  16 MB stack: the shim's Win32 stack in the bottom 1 MB,
;;;;                   the Lisp stack above it, the argc/argv/envp block on top
;;;;     0x5000000000+ hint-less mmaps, bump-placed (Linux puts them high too)
;;;;
;;;; THE STUB (raw offsets from 0), all of it before the JMP cross.lisp puts
;;;; after the boot code:
;;;;     0        JMP prologue
;;;;     5        IAT: one 8-byte slot per import (filled by the loader)
;;;;     ...      the mmap bump word
;;;;     ...      SHIM: the syscall dispatcher
;;;;     ...      PROLOGUE: heap, JIT arena, stack and its argv/envp block, argc,
;;;;              then the shared heap tail (emit-linux-x64-heap-tail), which
;;;;              falls through into the JMP to kernel-main.
;;;; EMIT-WIN64-ENTRY assembles the stub when the image is built; an early copy
;;;; (WIN64-STUB) only tells the translator where the shim and native code are.
;;;;
;;;; SCOPE: one OS thread.  The per-CPU GS block and the FS per-thread window are
;;;; OFF in the Windows build (build-win64-cli.lisp): on Windows GS base is the
;;;; TEB, and FS is not the runtime's to set.  clone, futex and arch_prctl return
;;;; -ENOSYS; sigaction returns 0 and nothing is delivered.  Files and sockets
;;;; are not mapped yet (-ENOSYS).

(in-package :modus.mvm)

(defconstant +win64-image-base+ #x400000)
(defconstant +win64-header-size+ #x1000)    ; PE headers + padding; raw code starts here
(defconstant +win64-text-va+ #x401000)      ; = image base + header size
(defconstant +win64-bss-end-va+ #x20000000) ; the zero-filled low block runs to here
;; The stack: a fixed, fully committed region, so the runtime finds the block a
;; Linux process starts with above its stack base (argv and envp are read from
;; there) and no Windows guard-page logic is involved.
(defconstant +win64-stack-va+ #x4000000000)
(defconstant +win64-stack-size+ #x1000000)  ; 16 MB
;; The bottom 1 MB of that region is the shim's own stack: every Win32 call runs
;; there, so kernel32 and ntdll never write to the Lisp stack.  (A Linux SYSCALL
;; writes nothing to the user stack, and the runtime was only ever run that way.)
(defconstant +win64-win32-stack-top+ (+ #x4000000000 #x100000))
;; Where hint-less mmaps are placed.  Linux puts them high; Wine's
;; VirtualAlloc(NULL) returns the lowest free region (0x240000, below the image).
(defconstant +win64-mmap-base+ #x5000000000)

(defparameter *win64-imports*
  '(("KERNEL32.dll"
     "ExitProcess" "GetStdHandle" "WriteFile" "ReadFile" "VirtualAlloc"
     "VirtualFree" "GetCurrentProcessId" "GetSystemTimeAsFileTime" "Sleep"
     "GetCommandLineA" "GetEnvironmentStringsA" "GetLastError"
     "CreateFileA" "CloseHandle" "SetFilePointerEx" "GetFileSizeEx"
     "GetFileAttributesA" "GetFileAttributesExA" "DeleteFileA" "MoveFileExA"
     "CreateDirectoryA" "RemoveDirectoryA" "GetCurrentDirectoryA")
    ("WS2_32.dll"
     "WSAStartup" "WSAGetLastError" "socket" "bind" "listen" "accept" "connect"
     "getsockname" "getpeername" "setsockopt" "getsockopt" "send" "recv"
     "sendto" "recvfrom" "closesocket" "shutdown" "ioctlsocket" "WSAPoll"))
  "The functions the image imports, by DLL.  The IAT holds each DLL's slots in
   this order followed by a NULL terminator slot, DLL after DLL.  The shim calls
   them by name; the Windows runtime calls them by slot (+W32-NAME+, a syscall
   number #x1000 + slot, WIN64-IMPORT-CONSTANTS-SOURCE).")

(defun win64-import-slot (name)
  "NAME's slot in the IAT (terminator slots counted)."
  (let ((slot 0))
    (dolist (dll *win64-imports*)
      (dolist (n (rest dll))
        (when (string= n name) (return-from win64-import-slot slot))
        (incf slot))
      (incf slot))                      ; the DLL's NULL terminator
    (error "boot-win64: ~A is not in *win64-imports*" name)))

(defun win64-iat-slots ()
  (loop for dll in *win64-imports* sum (length dll)))   ; names + 1 terminator each

;; MODUS_WIN64_TRACE=1 (build time): the shim writes "s<nr>" (hex) to stderr for
;; every syscall, before dispatching it.  Syscalls the shim does not implement
;; return -ENOSYS without reaching Win32, so a Win32 trace (WINEDEBUG=+relay)
;; cannot see them; this can.
(defvar *win64-trace-syscalls*
  (let ((v (sb-ext:posix-getenv "MODUS_WIN64_TRACE")))
    (and v (plusp (length v)) (string/= v "0"))))

(defconstant +win64-call-base+ #x1000
  "Syscall numbers from here call an import directly: #x1000 + its IAT slot.")

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
    ((:rip-load :rip-store :rip-lea) 7)
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
              ((:rip-load :rip-store :rip-lea)
               (put #x48) (put (ecase (first it) (:rip-load #x8B) (:rip-store #x89) (:rip-lea #x8D)))
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

(defparameter *win64-wsa-errno*
  '((10004 . 4) (10009 . 9) (10013 . 13) (10014 . 14) (10022 . 22) (10024 . 24)
    (10035 . 11) (10036 . 115) (10037 . 114) (10038 . 88) (10039 . 89) (10040 . 90)
    (10041 . 91) (10042 . 92) (10043 . 93) (10045 . 95) (10047 . 97) (10048 . 98)
    (10049 . 99) (10050 . 100) (10051 . 101) (10052 . 102) (10053 . 103)
    (10054 . 104) (10055 . 105) (10056 . 106) (10057 . 107) (10058 . 108)
    (10060 . 110) (10061 . 111) (10064 . 112) (10065 . 113))
  "Winsock error -> Linux errno.  Anything else is EIO.")

(defparameter *win64-sol-socket-options*
  '((2 . 4) (9 . 8) (8 . #x1002) (7 . #x1001) (6 . #x20) (13 . #x80)
    (4 . #x1007) (3 . #x1008) (15 . 4))
  "SOL_SOCKET option, Linux -> Windows: REUSEADDR, KEEPALIVE, RCVBUF, SNDBUF,
   BROADCAST, LINGER, ERROR, TYPE; REUSEPORT is taken as REUSEADDR.")

(defun w64-args-items (n)
  "Load the first N Linux arguments into the Win64 argument places."
  (subseq '((:bytes #x48 #x8B #x4D #xF8)            ; mov rcx,[rbp-8]
            (:bytes #x48 #x8B #x55 #xF0)            ; mov rdx,[rbp-16]
            (:bytes #x4C #x8B #x45 #xE8)            ; mov r8,[rbp-24]
            (:bytes #x4C #x8B #x4D #xE0)            ; mov r9,[rbp-32]
            (:bytes #x4C #x8B #x55 #xD8 #x4C #x89 #x54 #x24 #x20)  ; [rsp+20] = arg5
            (:bytes #x4C #x8B #x55 #xD0 #x4C #x89 #x54 #x24 #x28)) ; [rsp+28] = arg6
          0 n))

(defun win64-socket-items ()
  "Winsock behind the Linux socket syscalls.  sockaddr_in and sockaddr_un are
   laid out alike on both, so most calls pass straight through; option levels
   and names, pollfd and the error numbers are translated.  An int result of -1
   (or INVALID_SOCKET) goes to :WSA-FAIL, which answers -errno."
  `(;; socket(domain, type, protocol).  The type's SOCK_NONBLOCK (0x800) and
    ;; SOCK_CLOEXEC bits are not Windows'; NONBLOCK becomes FIONBIO.
    (:label :h-socket)
    (:bytes #x48 #x8B #x4D #xF8)                   ; mov rcx,[rbp-8]  domain
    (:bytes #x8B #x55 #xF0 #x83 #xE2 #x0F)         ; mov edx,[rbp-16]; and edx,15
    (:bytes #x4C #x8B #x45 #xE8)                   ; mov r8,[rbp-24]  protocol
    (:call-iat "socket")
    (:bytes #x48 #x83 #xF8 #xFF)                   ; cmp rax,-1
    (:jcc 4 :wsa-fail)
    (:bytes #x48 #x89 #x44 #x24 #x38)              ; mov [rsp+0x38],rax
    (:bytes #x8B #x45 #xF0 #xA9 0 8 0 0)           ; mov eax,[rbp-16]; test eax,0x800
    (:jcc 4 :s-done)
    (:bytes #x48 #x8B #x4C #x24 #x38)              ; mov rcx,[rsp+0x38]
    (:bytes #xBA #x7E #x66 #x04 #x80)              ; mov edx,FIONBIO
    (:bytes #xC7 #x44 #x24 #x30 1 0 0 0)           ; mov dword [rsp+0x30],1
    (:bytes #x4C #x8D #x44 #x24 #x30)              ; lea r8,[rsp+0x30]
    (:call-iat "ioctlsocket")
    (:label :s-done)
    (:bytes #x48 #x8B #x44 #x24 #x38)              ; mov rax,[rsp+0x38]
    (:jmp :done)

    ;; Three-argument int calls, as Linux has them.
    ,@(loop for (lbl . fn) in '((:h-bind . "bind") (:h-connect . "connect")
                                (:h-getsockname . "getsockname")
                                (:h-getpeername . "getpeername"))
            append `((:label ,lbl) ,@(w64-args-items 3) (:call-iat ,fn) (:jmp :ws-int)))
    ,@(loop for (lbl . fn) in '((:h-listen . "listen") (:h-shutdown . "shutdown"))
            append `((:label ,lbl) ,@(w64-args-items 2) (:call-iat ,fn) (:jmp :ws-int)))
    ;; accept / accept4 (the flags are dropped): a SOCKET.
    (:label :h-accept)
    ,@(w64-args-items 3)
    (:call-iat "accept")
    (:bytes #x48 #x83 #xF8 #xFF)                   ; cmp rax,-1
    (:jcc 4 :wsa-fail)
    (:jmp :done)
    ;; sendto / recvfrom: six arguments; only MSG_OOB/PEEK/DONTROUTE (1 2 4)
    ;; mean the same on both, so the rest of the flags (MSG_NOSIGNAL ...) go.
    ,@(loop for (lbl . fn) in '((:h-sendto . "sendto") (:h-recvfrom . "recvfrom"))
            append `((:label ,lbl) ,@(w64-args-items 6)
                     (:bytes #x41 #x83 #xE1 #x07)  ; and r9d,7
                     (:call-iat ,fn) (:jmp :ws-int)))
    ;; setsockopt / getsockopt: SOL_SOCKET is 0xFFFF and its option names differ.
    ,@(loop for (lbl . fn) in '((:h-setsockopt . "setsockopt") (:h-getsockopt . "getsockopt"))
            for go = (if (string= fn "setsockopt") :so-set :so-get)
            append `((:label ,lbl) ,@(w64-args-items 5)
                     (:bytes #x83 #xFA #x01)       ; cmp edx,1
                     (:jcc 5 ,go)
                     (:bytes #xBA #xFF #xFF 0 0)   ; mov edx,0xFFFF
                     ,@(loop for (lin . win) in *win64-sol-socket-options*
                             for next = (gensym "SO")
                             append `((:bytes #x41 #x83 #xF8 ,lin)        ; cmp r8d,LIN
                                      (:jcc 5 ,next)
                                      (:bytes #x41 #xB8 ,@(w64-le32 win))  ; mov r8d,WIN
                                      (:jmp ,go)
                                      (:label ,next)))
                     (:label ,go)
                     (:call-iat ,fn) (:jmp :ws-int)))
    ;; fcntl(fd, cmd, arg): F_SETFL's O_NONBLOCK becomes FIONBIO; F_GETFL says
    ;; O_RDWR; the rest (FD_CLOEXEC ...) succeed and do nothing.  On a file the
    ;; ioctl fails with WSAENOTSOCK, which is fine: files do not block here.
    (:label :h-fcntl)
    (:bytes #x8B #x45 #xF0)                        ; mov eax,[rbp-16]  cmd
    (:bytes #x83 #xF8 #x03)                        ; cmp eax,3  F_GETFL
    (:jcc 5 :f-set)
    (:bytes #xB8 2 0 0 0)                          ; mov eax,2  O_RDWR
    (:jmp :done)
    (:label :f-set)
    (:bytes #x83 #xF8 #x04)                        ; cmp eax,4  F_SETFL
    (:jcc 5 :h-zero)
    (:bytes #x8B #x45 #xE8 #xC1 #xE8 #x0B #x83 #xE0 #x01) ; mov eax,[rbp-24]; shr eax,11; and eax,1
    (:bytes #x89 #x44 #x24 #x30)                   ; mov [rsp+0x30],eax
    (:bytes #x48 #x8B #x4D #xF8)                   ; mov rcx,[rbp-8]
    (:bytes #xBA #x7E #x66 #x04 #x80)              ; mov edx,FIONBIO
    (:bytes #x4C #x8D #x44 #x24 #x30)              ; lea r8,[rsp+0x30]
    (:call-iat "ioctlsocket")
    (:jmp :h-zero)
    ;; ioctl(fd, req, argp): FIONREAD (0x541B); anything else (SIOCOUTQ ...)
    ;; answers 0 in *argp.
    (:label :h-ioctl)
    (:bytes #x8B #x45 #xF0)                        ; mov eax,[rbp-16]
    (:bytes #x3D #x1B #x54 0 0)                    ; cmp eax,0x541B
    (:jcc 5 :i-zero)
    (:bytes #x48 #x8B #x4D #xF8)                   ; mov rcx,[rbp-8]
    (:bytes #xBA #x7F #x66 #x04 #x40)              ; mov edx,FIONREAD
    (:bytes #x4C #x8B #x45 #xE8)                   ; mov r8,[rbp-24]
    (:call-iat "ioctlsocket")
    (:jmp :ws-int)
    (:label :i-zero)
    (:bytes #x48 #x8B #x4D #xE8 #xC7 #x01 0 0 0 0) ; mov rcx,[rbp-24]; mov dword [rcx],0
    (:jmp :h-zero)

    ;; poll(fds, n, timeout): Linux pollfd {int fd; short events, revents} to
    ;; WSAPOLLFD {SOCKET fd; short events, revents} in a buffer on the shim's
    ;; stack, and back.  POLLIN 1 <-> POLLRDNORM|POLLRDBAND 0x300, POLLOUT 4 <->
    ;; POLLWRNORM 0x10; ERR 1->8, HUP 2->16, NVAL 4->32.  The console fds (0-2)
    ;; are not sockets: WSAPoll skips them (fd -1) and they report ready for
    ;; what was asked, so LISTEN on a console never blocks the answer.
    (:label :h-poll)
    (:bytes #x48 #x8B #x45 #xF0 #x48 #x83 #xF8 #x40) ; mov rax,[rbp-16]; cmp rax,64
    (:jcc 7 :h-einval)                             ; ja: more than the buffer holds
    (:bytes #x48 #x81 #xEC #x20 #x04 0 0)          ; sub rsp,0x420
    (:bytes #x4C #x8B #x55 #xF8)                   ; mov r10,[rbp-8]   src
    (:bytes #x4C #x8D #x5C #x24 #x20)              ; lea r11,[rsp+0x20] dst
    (:bytes #x48 #x8B #x4D #xF0)                   ; mov rcx,[rbp-16]  n
    (:bytes #x45 #x31 #xC9)                        ; xor r9d,r9d       sockets seen
    (:label :pi-loop)
    (:bytes #x48 #x85 #xC9) (:jcc 4 :pi-done)      ; test rcx,rcx; jz
    (:bytes #x49 #x63 #x02)                        ; movsxd rax,[r10]
    (:bytes #x48 #x83 #xF8 #x02) (:jcc #xF :pi-sock) ; cmp rax,2; jg
    (:bytes #x48 #xC7 #xC0 #xFF #xFF #xFF #xFF)    ; mov rax,-1  (WSAPoll skips it)
    (:bytes #x49 #xFF #xC9)                        ; dec r9  (undone below)
    (:label :pi-sock)
    (:bytes #x49 #xFF #xC1)                        ; inc r9
    (:bytes #x49 #x89 #x03)                        ; mov [r11],rax
    (:bytes #x41 #x0F #xB7 #x42 #x04 #x31 #xD2)    ; movzx eax,word [r10+4]; xor edx,edx
    (:bytes #xA8 #x01) (:jcc 4 :pi-noin)           ; test al,1
    (:bytes #x81 #xCA 0 3 0 0)                     ; or edx,0x300
    (:label :pi-noin)
    (:bytes #xA8 #x04) (:jcc 4 :pi-noout)          ; test al,4
    (:bytes #x83 #xCA #x10)                        ; or edx,0x10
    (:label :pi-noout)
    (:bytes #x66 #x41 #x89 #x53 #x08)              ; mov [r11+8],dx
    (:bytes #x66 #x41 #xC7 #x43 #x0A 0 0)          ; mov word [r11+10],0
    (:bytes #x66 #x41 #xC7 #x42 #x06 0 0)          ; mov word [r10+6],0
    (:bytes #x49 #x83 #xC2 #x08 #x49 #x83 #xC3 #x10 #x48 #xFF #xC9) ; src+=8 dst+=16 n--
    (:jmp :pi-loop)
    (:label :pi-done)
    (:bytes #x31 #xC0)                             ; xor eax,eax  (no sockets: 0 ready)
    (:bytes #x4D #x85 #xC9) (:jcc 4 :po-start)     ; test r9,r9; jz: nothing for WSAPoll
    (:bytes #x48 #x8D #x4C #x24 #x20)              ; lea rcx,[rsp+0x20]
    (:bytes #x8B #x55 #xF0 #x44 #x8B #x45 #xE8)    ; mov edx,[rbp-16]; mov r8d,[rbp-24]
    (:call-iat "WSAPoll")
    (:bytes #x83 #xF8 #xFF) (:jcc 4 :wsa-fail)     ; cmp eax,-1
    (:label :po-start)
    (:bytes #x4C #x8B #x55 #xF8 #x4C #x8D #x5C #x24 #x20 #x48 #x8B #x4D #xF0) ; src, dst, n again
    (:bytes #x45 #x31 #xC9)                        ; xor r9d,r9d  ready count
    (:label :po-loop)
    (:bytes #x48 #x85 #xC9) (:jcc 4 :po-done)
    (:bytes #x31 #xD2)                             ; xor edx,edx
    (:bytes #x49 #x63 #x02 #x48 #x83 #xF8 #x02) (:jcc #xF :po-sock) ; movsxd rax,[r10]; cmp rax,2; jg
    (:bytes #x41 #x0F #xB7 #x52 #x04 #x83 #xE2 #x05) ; movzx edx,word [r10+4]; and edx,5
    (:jmp :po-store)
    (:label :po-sock)
    (:bytes #x41 #x0F #xB7 #x43 #x0A)              ; movzx eax,word [r11+10]
    ,@(loop for (bits . lin) in '((#x300 . 1) (#x10 . 4) (1 . 8) (2 . 16) (4 . 32))
            for skip = (gensym "PO")
            append `((:bytes #xA9 ,@(w64-le32 bits)) (:jcc 4 ,skip)   ; test eax,BITS
                     (:bytes #x83 #xCA ,lin)                          ; or edx,LIN
                     (:label ,skip)))
    (:label :po-store)
    (:bytes #x66 #x41 #x89 #x52 #x06)              ; mov [r10+6],dx
    (:bytes #x85 #xD2) (:jcc 4 :po-next)           ; test edx,edx
    (:bytes #x49 #xFF #xC1)                        ; inc r9
    (:label :po-next)
    (:bytes #x49 #x83 #xC2 #x08 #x49 #x83 #xC3 #x10 #x48 #xFF #xC9)
    (:jmp :po-loop)
    (:label :po-done)
    (:bytes #x4C #x89 #xC8)                        ; mov rax,r9
    (:jmp :done)

    ;; Common tails.  :WS-INT: an int result, -1 = failure.
    (:label :ws-int)
    (:bytes #x83 #xF8 #xFF) (:jcc 4 :wsa-fail)     ; cmp eax,-1
    (:bytes #x48 #x63 #xC0)                        ; movsxd rax,eax
    (:jmp :done)
    (:label :wsa-fail)
    (:call-iat "WSAGetLastError")
    (:label :wsa-map)                              ; eax = Winsock error
    (:rip-lea 1 :wsa-table)                        ; lea rcx,[rip+table]
    (:label :wm-loop)
    (:bytes #x8B #x11 #x85 #xD2) (:jcc 4 :h-eio)   ; mov edx,[rcx]; test edx,edx; jz
    (:bytes #x39 #xC2) (:jcc 4 :wm-hit)            ; cmp edx,eax
    (:bytes #x48 #x83 #xC1 #x08) (:jmp :wm-loop)   ; add rcx,8
    (:label :wm-hit)
    (:bytes #x8B #x41 #x04 #x48 #xF7 #xD8)         ; mov eax,[rcx+4]; neg rax
    (:jmp :done)
    (:label :wsa-table)
    (:bytes ,@(loop for (w . e) in *win64-wsa-errno* append (append (w64-le32 w) (w64-le32 e)))
            0 0 0 0 0 0 0 0)))

(defun win64-shim-items ()
  (let ((dispatch '((0 . :h-read) (1 . :h-write) (3 . :h-close) (8 . :h-lseek)
                    (9 . :h-mmap) (11 . :h-munmap) (79 . :h-getcwd)
                    (41 . :h-socket) (42 . :h-connect) (43 . :h-accept) (288 . :h-accept)
                    (44 . :h-sendto) (45 . :h-recvfrom) (48 . :h-shutdown) (49 . :h-bind)
                    (50 . :h-listen) (51 . :h-getsockname) (52 . :h-getpeername)
                    (54 . :h-setsockopt) (55 . :h-getsockopt) (7 . :h-poll)
                    (72 . :h-fcntl) (16 . :h-ioctl)
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
      ;; #x1000 + slot: call that import with the Linux argument registers in
      ;; Win64 order (rcx rdx r8 r9 [rsp+20] [rsp+28], and 0 as a seventh), and
      ;; return its RAX as it is.  The Windows runtime makes its own sense of it.
      (:bytes #x48 #x3D ,@(w64-le32 +win64-call-base+))       ; cmp rax,#x1000
      (:jcc 2 :h-enosys)                                      ; jb
      (:bytes #x48 #x3D ,@(w64-le32 (+ +win64-call-base+ (win64-iat-slots)))) ; cmp rax,end
      (:jcc 3 :h-enosys)                                      ; jae
      (:bytes #x48 #x2D ,@(w64-le32 +win64-call-base+))       ; sub rax,#x1000  (slot)
      (:bytes #x49 #xC7 #xC3 ,@(w64-le32 (+ +win64-text-va+ 5))) ; mov r11,IAT (the stub's slot 5)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]
      (:bytes #x4C #x8B #x45 #xE8)                 ; mov r8,[rbp-24]
      (:bytes #x4C #x8B #x4D #xE0)                 ; mov r9,[rbp-32]
      (:bytes #x4C #x8B #x55 #xD8)                 ; mov r10,[rbp-40]
      (:bytes #x4C #x89 #x54 #x24 #x20)            ; mov [rsp+0x20],r10
      (:bytes #x4C #x8B #x55 #xD0)                 ; mov r10,[rbp-48]
      (:bytes #x4C #x89 #x54 #x24 #x28)            ; mov [rsp+0x28],r10
      (:bytes #x48 #xC7 #x44 #x24 #x30 0 0 0 0)    ; mov qword [rsp+0x30],0
      (:bytes #x41 #xFF #x14 #xC3)                 ; call [r11+rax*8]
      (:jmp :done)

      ;; read(fd, buf, count): fd 0 is stdin; any other fd is a SOCKET (recv) or,
      ;; when recv says WSAENOTSOCK, a file HANDLE (the Windows %SYS-OPEN-*
      ;; return the HANDLE as the fd).  A failed ReadFile reads as end-of-file.
      (:label :h-read)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x48 #x85 #xC9)                      ; test rcx,rcx
      (:jcc 5 :r-sock)                             ; jnz: a socket or a file handle
      (:bytes #xB9 ,@(w64-le32 -10))               ; mov ecx,STD_INPUT_HANDLE
      (:call-iat "GetStdHandle")
      (:bytes #x48 #x89 #xC1)                      ; mov rcx,rax
      (:jmp :r-go)
      (:label :r-sock)
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  buf
      (:bytes #x4C #x8B #x45 #xE8)                 ; mov r8,[rbp-24]   count
      (:bytes #x45 #x31 #xC9)                      ; xor r9d,r9d       flags
      (:call-iat "recv")
      (:bytes #x83 #xF8 #xFF)                      ; cmp eax,-1
      (:jcc 5 :ws-int)                             ; jne: it was a socket
      (:call-iat "WSAGetLastError")
      (:bytes #x3D #x36 #x27 #x00 #x00)            ; cmp eax,WSAENOTSOCK
      (:jcc 5 :wsa-map)                            ; jne: a socket error
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]   a file: handle
      (:label :r-go)
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  buf
      (:bytes #x4C #x8B #x45 #xE8)                 ; mov r8,[rbp-24]   count
      (:bytes #x4C #x8D #x4C #x24 #x30)            ; lea r9,[rsp+0x30]
      (:bytes #xC7 #x44 #x24 #x30 0 0 0 0)         ; mov dword [rsp+0x30],0
      (:bytes #x48 #xC7 #x44 #x24 #x20 0 0 0 0)    ; mov qword [rsp+0x20],0
      (:call-iat "ReadFile")
      (:bytes #x8B #x44 #x24 #x30)                 ; mov eax,[rsp+0x30]
      (:jmp :done)

      ;; write(fd, buf, count): fd 1 -> STD_OUTPUT, fd 2 -> STD_ERROR, any other fd
      ;; is a file handle.
      (:label :h-write)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x48 #x83 #xF9 #x01)                 ; cmp rcx,1
      (:jcc 4 :w-out)                              ; je
      (:bytes #x48 #x83 #xF9 #x02)                 ; cmp rcx,2
      (:jcc 5 :w-sock)                             ; jne: a socket or a file handle
      (:bytes #xB9 ,@(w64-le32 -12))               ; mov ecx,STD_ERROR_HANDLE
      (:jmp :w-std)
      (:label :w-out)
      (:bytes #xB9 ,@(w64-le32 -11))               ; mov ecx,STD_OUTPUT_HANDLE
      (:label :w-std)
      (:call-iat "GetStdHandle")
      (:bytes #x48 #x89 #xC1)                      ; mov rcx,rax
      (:jmp :w-go)
      (:label :w-sock)
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  buf
      (:bytes #x4C #x8B #x45 #xE8)                 ; mov r8,[rbp-24]   count
      (:bytes #x45 #x31 #xC9)                      ; xor r9d,r9d       flags
      (:call-iat "send")
      (:bytes #x83 #xF8 #xFF)                      ; cmp eax,-1
      (:jcc 5 :ws-int)                             ; jne: it was a socket
      (:call-iat "WSAGetLastError")
      (:bytes #x3D #x36 #x27 #x00 #x00)            ; cmp eax,WSAENOTSOCK
      (:jcc 5 :wsa-map)                            ; jne: a socket error
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]   a file: handle
      (:label :w-go)
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

;; close(fd): closesocket, else (WSAENOTSOCK) CloseHandle; the standard fds
      ;; are left open.
      (:label :h-close)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x48 #x83 #xF9 #x02)                 ; cmp rcx,2
      (:jcc 6 :h-zero)                             ; jbe: 0, 1, 2
      (:call-iat "closesocket")
      (:bytes #x85 #xC0)                           ; test eax,eax
      (:jcc 4 :h-zero)                             ; je: it was a socket
      (:call-iat "WSAGetLastError")
      (:bytes #x3D #x36 #x27 #x00 #x00)            ; cmp eax,WSAENOTSOCK
      (:jcc 5 :wsa-map)                            ; jne
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]   a file
      (:call-iat "CloseHandle")
      (:bytes #x85 #xC0)                           ; test eax,eax
      (:jcc 4 :h-ebadf)                            ; je -> failure
      (:bytes #x31 #xC0)                           ; xor eax,eax
      (:jmp :done)

      ;; lseek(fd, offset, whence): SetFilePointerEx.  SEEK_SET/CUR/END are
      ;; FILE_BEGIN/CURRENT/END (0, 1, 2).
      (:label :h-lseek)
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]   handle
      (:bytes #x48 #x8B #x55 #xF0)                 ; mov rdx,[rbp-16]  offset
      (:bytes #x4C #x8D #x44 #x24 #x38)            ; lea r8,[rsp+0x38] new position
      (:bytes #x44 #x8B #x4D #xE8)                 ; mov r9d,[rbp-24]  whence
      (:call-iat "SetFilePointerEx")
      (:bytes #x85 #xC0)                           ; test eax,eax
      (:jcc 4 :h-einval)                           ; je -> failure
      (:bytes #x48 #x8B #x44 #x24 #x38)            ; mov rax,[rsp+0x38]
      (:jmp :done)

      ;; getcwd(buf, size): GetCurrentDirectoryA, backslashes made slashes so
      ;; the runtime's pathname code reads it.  Returns the length with the NUL,
      ;; as Linux does, or -ERANGE.
      (:label :h-getcwd)
      (:bytes #x8B #x4D #xF0)                      ; mov ecx,[rbp-16]  size
      (:bytes #x48 #x8B #x55 #xF8)                 ; mov rdx,[rbp-8]   buf
      (:call-iat "GetCurrentDirectoryA")
      (:bytes #x85 #xC0)                           ; test eax,eax
      (:jcc 4 :h-einval)                           ; je -> failure
      (:bytes #x3B #x45 #xF0)                      ; cmp eax,[rbp-16]
      (:jcc 3 :h-erange)                           ; jae: did not fit
      (:bytes #x48 #x8B #x4D #xF8)                 ; mov rcx,[rbp-8]
      (:bytes #x31 #xD2)                           ; xor edx,edx
      (:label :g-loop)
      (:bytes #x39 #xC2)                           ; cmp edx,eax
      (:jcc 3 :g-done)                             ; jae
      (:bytes #x80 #x3C #x11 #x5C)                 ; cmp byte [rcx+rdx],'\'
      (:jcc 5 :g-next)                             ; jne
      (:bytes #xC6 #x04 #x11 #x2F)                 ; mov byte [rcx+rdx],'/'
      (:label :g-next)
      (:bytes #xFF #xC2)                           ; inc edx
      (:jmp :g-loop)
      (:label :g-done)
      (:bytes #xFF #xC0)                           ; inc eax  (the NUL)
      (:jmp :done)
      (:label :h-erange)
      (:bytes #x48 #xC7 #xC0 ,@(w64-le32 -34))     ; mov rax,-ERANGE
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

      ,@(win64-socket-items)

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
    ;; its bump word [0x10000FB8] set to the base, or 0 if refused (then the
    ;; runtime's exec-page trap falls back to mmap).  Mapped with the JIT off
    ;; too, so the startup state is the Linux image's.
    (:bytes #x48 #x89 #xC5)                              ; mov rbp,rax  (heap base)
    ;; Winsock, once: WSAStartup(2.2, &WSADATA) with the 408-byte WSADATA here.
    (:bytes #x48 #x81 #xEC #xC0 #x01 0 0)                ; sub rsp,0x1C0
    (:bytes #xB9 #x02 #x02 0 0)                          ; mov ecx,0x202
    (:bytes #x48 #x8D #x54 #x24 #x20)                    ; lea rdx,[rsp+0x20]
    (:call-iat "WSAStartup")
    (:bytes #x48 #x81 #xC4 #xC0 #x01 0 0)                ; add rsp,0x1C0
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
    (:zeros ,(* 8 (win64-iat-slots)))
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
  "The import directory, as bytes that live at IDATA-RVA: one
   IMAGE_IMPORT_DESCRIPTOR per DLL plus the terminator, each DLL's ILT, the
   hint/name entries and the DLL names.  The IAT itself is in .text at IAT-RVA;
   the caller fills it.  Returns (values bytes iat-init), IAT-INIT being
   (slot . hint/name RVA) for every import -- what the IAT starts out holding."
  (let* ((ndll (length *win64-imports*))
         (cursor (* 20 (1+ ndll)))
         (ilt-offs nil) (hint-offs nil) (dll-offs nil))
    ;; Layout pass: ILTs, then hint/name entries, then DLL names.
    (dolist (dll *win64-imports*)
      (push cursor ilt-offs)
      (incf cursor (* 8 (length dll))))         ; names + NULL
    (setf ilt-offs (nreverse ilt-offs))
    (dolist (dll *win64-imports*)
      (dolist (name (rest dll))
        (push cursor hint-offs)
        (incf cursor (w64-pad (+ 2 (length name) 1) 2))))   ; WORD hint, name, NUL, even
    (setf hint-offs (nreverse hint-offs))
    (dolist (dll *win64-imports*)
      (push cursor dll-offs)
      (incf cursor (1+ (length (first dll)))))
    (setf dll-offs (nreverse dll-offs))
    (let ((v (make-array cursor :element-type '(unsigned-byte 8) :initial-element 0))
          (iat-init nil) (h hint-offs) (slot 0))
      (flet ((put32 (off value)
               (loop for b in (w64-le32 value) for k from off do (setf (aref v k) b)))
             (put64 (off value)
               (loop for b in (w64-le64 value) for k from off do (setf (aref v k) b)))
             (put-string (off str)
               (loop for c across str for k from off do (setf (aref v k) (char-code c)))))
        (loop for dll in *win64-imports*
              for d from 0
              for ilt in ilt-offs
              for dll-off in dll-offs
              do ;; OriginalFirstThunk, TimeDateStamp, ForwarderChain, Name, FirstThunk
                 (put32 (* 20 d) (+ idata-rva ilt))
                 (put32 (+ (* 20 d) 12) (+ idata-rva dll-off))
                 (put32 (+ (* 20 d) 16) (+ iat-rva (* 8 slot)))
                 (put-string dll-off (first dll))
                 (loop for name in (rest dll)
                       for i from 0
                       for hn = (pop h)
                       do (put64 (+ ilt (* 8 i)) (+ idata-rva hn))
                          (put-string (+ hn 2) name)
                          (push (cons slot (+ idata-rva hn)) iat-init)
                          (incf slot))
                 (incf slot)))                   ; the terminator slot stays 0
      (values v (nreverse iat-init)))))

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
    (multiple-value-bind (idata iat-init) (w64-build-idata idata-rva iat-rva)
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
          (loop for (slot . rva) in iat-init
                do (loop for b in (w64-le64 rva)
                         for k from (+ text-raw-off (win64-iat-offset) (* 8 slot))
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
            (w32 (+ opt 112 12) (* 20 (1+ (length *win64-imports*)))))
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

;;; ============================================================
;;; The build: what build-generic-cli.lisp calls when *CLI-WIN64* is set.
;;; ============================================================

;; WINDOWS RUNTIME SOURCE, appended to the image's own source.  It must go on
;; *FULL-SOURCE* itself: that text is assembled when build-cli-common.lisp loads,
;; so appending to *CLI-ARCH-OVERRIDE-SOURCE* here (after the load) is lost.
;;
;; %JIT-WINDOW-PREFIX: the JIT's hand-assembled bridge thunks store to window
;; slots with this byte first; 144 (#x90, NOP) instead of the FS override, since
;; this image has no window and FS is not ours (mvm/mvm-eval.lisp).
;; %CLI-COLLECT-ARGV: the stub puts the whole command line (GetCommandLineA)
;;    in argv[0]; split it with the Windows rules -- whitespace separates outside
;;    double quotes, a quote toggles quoting, \" is a literal quote.  Characters
;;    are written as codes (92 backslash, 34 quote, 32 space, 9 tab) to keep the
;;    text free of escapes.  Last definition wins (the original is overridable).
(defparameter *win64-runtime-source*
  (format nil "
(defun %jit-window-prefix () 144)

;; FILES.  A Windows HANDLE is the fd: the shim reads, writes, seeks and closes
;; any fd above 2 as a handle, so streams need nothing else.  Paths go to the
;; ANSI Win32 calls as they are (forward slashes work; a relative path is
;; relative to the process's current directory).
(defun %w32-errno ()
  (let ((e (syscall6 +w32-getlasterror+ 0 0 0 0 0 0)))
    (cond ((or (eql e 2) (eql e 3)) 2)          ; FILE/PATH_NOT_FOUND -> ENOENT
          ((eql e 5) 13)                         ; ACCESS_DENIED -> EACCES
          ((or (eql e 80) (eql e 183)) 17)       ; FILE/ALREADY_EXISTS -> EEXIST
          ((eql e 145) 39)                       ; DIR_NOT_EMPTY -> ENOTEMPTY
          (t 5))))                               ; -> EIO
(defun %w32-ok (r) (if (eql r 0) (- 0 (%w32-errno)) 0))
(defun %w32-open (path-str access disposition)
  (%string-to-cstr path-str *cstr-scratch*)
  ;; share READ|WRITE|DELETE (7), FILE_ATTRIBUTE_NORMAL (128); the shim passes
  ;; the seventh argument, hTemplateFile, as 0.
  (let ((h (syscall6 +w32-createfilea+ *cstr-scratch* access 7 0 disposition 128)))
    (if (< h 0) (- 0 (%w32-errno)) h)))
;; GENERIC_READ 2147483648, GENERIC_WRITE 1073741824, FILE_APPEND_DATA 4;
;; CREATE_NEW 1, CREATE_ALWAYS 2, OPEN_EXISTING 3, OPEN_ALWAYS 4.
(defun %sys-open-rdonly (path-str) (%w32-open path-str 2147483648 3))
(defun %sys-open-wronly (path-str) (%w32-open path-str 1073741824 2))
(defun %sys-open-append (path-str) (%w32-open path-str 4 4))
(defun %sys-open-rdwr (path-str) (%w32-open path-str 3221225472 4))
(defun %sys-open-wronly-existing (path-str) (%w32-open path-str 1073741824 3))
(defun %sys-open-create-excl (path-str) (%w32-open path-str 1073741824 1))
(defun %sys-unlink (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%w32-ok (syscall6 +w32-deletefilea+ *cstr-scratch* 0 0 0 0 0)))
(defun %sys-rename (old-str new-str)
  (%string-to-cstr old-str *cstr-scratch*)
  (let ((new-addr (+ *cstr-scratch* 2048)))
    (%string-to-cstr new-str new-addr)
    (%w32-ok (syscall6 +w32-movefileexa+ *cstr-scratch* new-addr 1 0 0 0))))  ; REPLACE_EXISTING
(defun %sys-mkdir (path-str mode)
  (%string-to-cstr path-str *cstr-scratch*)
  (%w32-ok (syscall6 +w32-createdirectorya+ *cstr-scratch* 0 0 0 0 0)))
(defun %sys-rmdir (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%w32-ok (syscall6 +w32-removedirectorya+ *cstr-scratch* 0 0 0 0 0)))
(defun %sys-stat-exists (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (not (eql (syscall6 +w32-getfileattributesa+ *cstr-scratch* 0 0 0 0 0) 4294967295)))
;; WIN32_FILE_ATTRIBUTE_DATA: attributes +0, creation +4, access +12, write
;; +20 (FILETIME: low, high), size high +28, size low +32.
(defun %w32-attribute-data (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (not (eql (syscall6 +w32-getfileattributesexa+ *cstr-scratch* 0 *io-buf-addr* 0 0 0) 0)))
(defun %sys-stat-size (path-str)
  (if (%w32-attribute-data path-str)
      (+ (* (mem-ref (+ *io-buf-addr* 28) :u32) 4294967296) (mem-ref (+ *io-buf-addr* 32) :u32))
      -1))
(defun %sys-stat-mtime (path-str)
  (if (%w32-attribute-data path-str)
      (floor (- (+ (* (mem-ref (+ *io-buf-addr* 24) :u32) 4294967296)
                   (mem-ref (+ *io-buf-addr* 20) :u32))
                116444736000000000)
             10000000)
      0))
;; DRIVES: \"C:/a/b\" is absolute here (mvm/cl-fileio.lisp, %DRIVE-NAMESTRING-P).
(defun %drive-namestring-p (s)
  (and (>= (length s) 2)
       (let ((c (char-code (char s 0))))
         (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))))
       (= (char-code (char s 1)) 58)
       (or (= (length s) 2) (= (char-code (char s 2)) 47))))

;; THE FASL CACHE.  Its key covers the source, not the target, so this image
;; must not share the Linux image's cache: $HOME/.cache/modus/fasl/ seen through
;; Wine's Z: drive IS the Linux cache, and loading its FASLs runs native code
;; built for the Linux layout (it failed silently).  Its own directory:
;; $MODUS_FASL_DIR, else $HOME/.cache/modus/fasl-win64/ (Wine passes no HOME),
;; else %LOCALAPPDATA%/modus/fasl/.
(defun %w32-slashes (s)
  (let ((r (copy-seq s)))
    (dotimes (i (length r) r)
      (when (= (char-code (char r i)) 92) (setf (char r i) (code-char 47))))))
(defvar *w32-fasl-dir* nil)
(defun %w32-mkdirs (dir)
  ;; mkdir -p on the raw string, prefix by prefix: the pathname layer does not
  ;; know drive letters yet, so ENSURE-DIRECTORIES-EXIST does nothing with
  ;; C:/... .  An existing directory just fails its mkdir.
  (let ((i 1) (n (length dir)))
    (loop
      (when (>= i n) (return dir))
      (when (= (char-code (char dir i)) 47) (%sys-mkdir (subseq dir 0 i) 493))
      (setq i (+ i 1)))))
(defun %fasl-dir ()
  (or *w32-fasl-dir*
      (setq *w32-fasl-dir*
            (let ((d (ignore-errors (%cli-getenv \"MODUS_FASL_DIR\"))))
              (if (and d (> (length d) 0))
                  (if (= (char-code (char d (- (length d) 1))) 47) d (concatenate 'string d \"/\"))
                  (let ((home (ignore-errors (%cli-getenv \"HOME\")))
                        (local (ignore-errors (%cli-getenv \"LOCALAPPDATA\"))))
                    (cond ((and home (> (length home) 0))
                           (%w32-mkdirs (concatenate 'string (%w32-slashes home) \"/.cache/modus/fasl-win64/\")))
                          ((and local (> (length local) 0))
                           (%w32-mkdirs (concatenate 'string (%w32-slashes local) \"/modus/fasl/\")))
                          (t nil))))))))
(defun %sys-fstat-size (fd)
  (if (eql (syscall6 +w32-getfilesizeex+ fd *io-buf-addr* 0 0 0 0) 0)
      -1
      (+ (* (mem-ref (+ *io-buf-addr* 4) :u32) 4294967296) (mem-ref *io-buf-addr* :u32))))

(defun %cli-collect-argv ()
  (let* ((line (%cli-cstr-at (%cli-read-ptr (%cli-argv-base))))
         (n (length line)) (acc nil) (out (make-string-output-stream))
         (in-token nil) (quoted nil) (i 0))
    (loop
      (when (>= i n) (return))
      (let ((c (char-code (char line i))))
        (cond ((and (= c 92) (< (+ i 1) n) (= (char-code (char line (+ i 1))) 34))
               (write-char (code-char 34) out) (setq in-token t) (setq i (+ i 1)))
              ((= c 34) (setq quoted (not quoted)) (setq in-token t))
              ((and (not quoted) (or (= c 32) (= c 9)))
               (when in-token
                 (push (get-output-stream-string out) acc)
                 (setq in-token nil)))
              (t (write-char (code-char c) out) (setq in-token t))))
      (setq i (+ i 1)))
    (when in-token (push (get-output-stream-string out) acc))
    (nreverse acc)))
"))

(defun win64-import-constants-source ()
  "(defconstant +W32-NAME+ n) for every import: n is the syscall number that
   calls it (#x1000 + its IAT slot)."
  (with-output-to-string (o)
    (dolist (dll *win64-imports*)
      (dolist (name (rest dll))
        (format o "(defconstant +w32-~(~A~)+ ~D)~%"
                name (+ +win64-call-base+ (win64-import-slot name)))))))

(defun win64-configure-build ()
  "Point the translator at this image's layout and add the Windows runtime
   source.  Called by build-generic-cli.lisp in place of the Linux native-code
   offset, after build-cli-common has assembled *FULL-SOURCE*."
  (win64-image-check)
  (setf modus.mvm.x64::*x64-native-code-offset* (win64-native-code-offset))
  ;; Every syscall the image makes goes through the stub's shim.
  (setf modus.mvm.x64::*x64-win-shim-va* (win64-shim-va))
  (setq cl-user::*full-source*
        (concatenate 'string cl-user::*full-source* (string #\Newline)
                     (win64-import-constants-source) *win64-runtime-source*))
  ;; The JIT co-init's shim address (build-cli-common, *CLI-WIN64*).  Every copy of
  ;; the co-init in the source gets it; with the JIT on there must be at least one.
  (let ((token "WIN64-SHIM-VA-PLACEHOLDER")
        (value (princ-to-string (modus.mvm::win64-shim-va)))
        (n 0))
    (loop
      (let* ((src cl-user::*full-source*)
             (at (search token src)))
        (unless at (return))
        (incf n)
        (setq cl-user::*full-source*
              (concatenate 'string (subseq src 0 at) value
                           (subseq src (+ at (length token)))))))
    (format t "~&;; win64: shim address #x~X in ~D JIT co-init cop~:@P~%"
            (modus.mvm::win64-shim-va) n n)
    (when (and cl-user::*jit-on* (zerop n))
      (error "win64-configure-build: the JIT is on but its co-init has no ~A" token))))
