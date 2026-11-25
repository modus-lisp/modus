;;;; host/macos/shim.lisp -- the native half of the macOS modus CLI, in mvm/macho-cc.lisp's
;;;; language: what host/macos/modus-shim.c and syscall-stub.S do for `build-macos.sh IMAGE
;;;; OUT cli', with no C compiler.  mvm/build-macos-cli.lisp compiles it and links it, with
;;;; the image, into a signed Mach-O (mvm/macho-writer.lisp).
;;;;
;;;; THE IMAGE is the ordinary Linux/aarch64 ELF built with the Darwin layout (MODUS_DARWIN=1
;;;; MODUS_CODE_BASE=... ; see build-macos.sh), carried in __TEXT,__modus.  At start-up MAIN:
;;;;   1. mach_vm_remaps those signed pages to the image's link address (macOS lets nothing
;;;;      else create executable pages at a fixed address), and maps the BSS tail after them;
;;;;   2. maps the page one 16 KB below the code base and stores SYSCALL-STUB's address in its
;;;;      first word: the slot every syscall site in the image loads (translate-aarch64 A64-SVC);
;;;;   3. builds the Linux initial stack (argc, argv, envp, auxv) and jumps to the ELF entry.
;;;; From then on the image calls SYSCALL-STUB with a LINUX aarch64 syscall number in x8 and
;;;; its arguments in x0..x5, and MODUS-SYSCALL translates the call to libSystem.
;;;;
;;;; KEPT from modus-shim.c, case for case: every Linux call it translates (files, mmap with
;;;; MAP_JIT and MAP_FIXED_NOREPLACE, the JIT arena's RW data end, signals and the fault
;;;; handler's Linux frames, the JIT write/exec flip, getdents64, sockets, clone, futex,
;;;; clocks), the pseudo-calls 1000 (thread delta), 1020 (mlock) and 1081 (backtrace), the
;;;; fault report, and MODUS_SHIM_STRACE.
;;;; DROPPED, because the CLI image never asks for them: the window (1001-1008, the GUI app),
;;;; the speaker (1010-1013, used only by vessel's desk app, which also needs the window) and
;;;; the keychain (1030-1032, vessel's iOS terminal) -- each now answers -ENOSYS, as an
;;;; unknown call always did; the iOS / tvOS / watchOS arms; the in-place layout
;;;; (MODUS_IN_PLACE, image-segments.sh: __MODUSS/__MODUSB/reservations), MODUS_SLIDE and the
;;;; preloaded __MODUSC core, which are build-macos.sh options this builder does not make;
;;;; modus.args and @NAME arguments (app bundles); the sampling profiler
;;;; (MODUS_SHIM_PROFILE), MODUS_SHIM_RESIDENT and MODUS_SHIM_FAULTS.
;;;;
;;;; Darwin constants and layouts come from Apple's open-source headers: xnu bsd/sys/fcntl.h,
;;;; mman.h, signal.h, errno.h, socket.h, stat.h, dirent.h, _types/_ucontext.h, bsd/arm/
;;;; _mcontext.h, osfmk/mach/arm/_structs.h, vm_statistics.h, vm_inherit.h; Libc include/
;;;; _time.h (clock ids); libplatform os/os_sync_wait_on_address.h; libdispatch os/clock.h;
;;;; libpthread pthread.h and _pthread_types.h.  Every value is the one modus-shim.c got from
;;;; the same headers by name.

;;; ------------------------------------------------------------------ helpers ----

(defmacro logtest (a b) `(/= (logand ,a ,b) 0))
(defmacro ret-c (form)
  "A libSystem result as Linux returns it: -errno (Linux's numbering) on failure."
  `(let ((%r ,form)) (if (< %r 0) (neg-errno) %r)))
(defmacro set-or (var bits) `(setq ,var (logior ,var ,bits)))

(defconst page16k #x4000)
(defconst linux-at-fdcwd -100)
(defconst darwin-at-fdcwd -2)

;;; ------------------------------------------------------------------ imports ----

(defimport c-write "_write")
(defimport c-read "_read")
(defimport c-openat "_openat" :i32)
(defimport c-close "_close" :i32)
(defimport c-lseek "_lseek")
(defimport c-fstatat "_fstatat" :i32)
(defimport c-fstat "_fstat" :i32)
(defimport c-mmap "_mmap")
(defimport c-munmap "_munmap" :i32)
(defimport c-mprotect "_mprotect" :i32)
(defimport c-mlock "_mlock" :i32)
(defimport c-sigaltstack "_sigaltstack" :i32)
(defimport c-sigaction "_sigaction" :i32)
(defimport c-sigprocmask "_sigprocmask" :i32)
(defimport c-kill "_kill" :i32)
(defimport c-exit "__exit" :void)
(defimport c-fcntl "_fcntl" :i32)
(defimport c-poll "_poll" :i32)
(defimport c-clock-gettime "_clock_gettime" :i32)
(defimport c-gettimeofday "_gettimeofday" :i32)
(defimport c-nanosleep "_nanosleep" :i32)
(defimport c-sched-yield "_sched_yield" :i32)
(defimport c-getpid "_getpid" :i32)
(defimport c-getcwd "_getcwd")
(defimport c-chdir "_chdir" :i32)
(defimport c-mkdirat "_mkdirat" :i32)
(defimport c-unlinkat "_unlinkat" :i32)
(defimport c-renameat "_renameat" :i32)
(defimport c-faccessat "_faccessat" :i32)
(defimport c-readlinkat "_readlinkat")
(defimport c-ftruncate "_ftruncate" :i32)
(defimport c-dup "_dup" :i32)
(defimport c-arc4random-buf "_arc4random_buf" :void)
(defimport c-socket "_socket" :i32)
(defimport c-bind "_bind" :i32)
(defimport c-connect "_connect" :i32)
(defimport c-listen "_listen" :i32)
(defimport c-accept "_accept" :i32)
(defimport c-getsockname "_getsockname" :i32)
(defimport c-getpeername "_getpeername" :i32)
(defimport c-sendto "_sendto")
(defimport c-recvfrom "_recvfrom")
(defimport c-setsockopt "_setsockopt" :i32)
(defimport c-getsockopt "_getsockopt" :i32)
(defimport c-shutdown "_shutdown" :i32)
(defimport c-error "___error")
(defimport c-fdopendir "_fdopendir")
(defimport c-readdir "_readdir")
(defimport c-closedir "_closedir" :i32)
(defimport c-strlen "_strlen")
(defimport c-memcpy "_memcpy")
(defimport c-memset "_memset")
(defimport c-malloc "_malloc")
(defimport c-calloc "_calloc")
(defimport c-free "_free" :void)
(defimport c-getenv "_getenv")
(defimport c-pthread-key-create "_pthread_key_create" :i32)
(defimport c-pthread-setspecific "_pthread_setspecific" :i32)
(defimport c-pthread-getspecific "_pthread_getspecific")
(defimport c-pthread-create "_pthread_create" :i32)
(defimport c-pthread-attr-init "_pthread_attr_init" :i32)
(defimport c-pthread-attr-setdetachstate "_pthread_attr_setdetachstate" :i32)
(defimport c-pthread-attr-setstacksize "_pthread_attr_setstacksize" :i32)
(defimport c-pthread-attr-destroy "_pthread_attr_destroy" :i32)
(defimport c-pthread-threadid-np "_pthread_threadid_np" :i32)
(defimport c-pthread-main-np "_pthread_main_np" :i32)
(defimport c-pthread-exit "_pthread_exit" :void)
(defimport c-pthread-jit-write-protect-np "_pthread_jit_write_protect_np" :void)
(defimport c-os-sync-wait "_os_sync_wait_on_address" :i32)
(defimport c-os-sync-wait-timeout "_os_sync_wait_on_address_with_timeout" :i32)
(defimport c-os-sync-wake-any "_os_sync_wake_by_address_any" :i32)
(defimport c-os-sync-wake-all "_os_sync_wake_by_address_all" :i32)
(defimport c-mach-vm-remap "_mach_vm_remap" :i32)
(defimport c-mach-vm-allocate "_mach_vm_allocate" :i32)
(defimport mach-task-self "_mach_task_self_" :data)

;;; --------------------------------------------------------------------- state ----

(defextern modus-image)                 ; __TEXT,__modus: the ELF
(defdata jit-regions 1024)              ; 64 x (lo, hi)
(defdata n-jit 8)
(defdata data-slice-lo 8)
(defdata data-slice-hi 8)
(defdata image-fault-handler 128)       ; 4 x Linux struct sigaction: SEGV BUS FPE ILL
(defdata dirs 8192)                     ; DIR* per fd, for getdents64
(defdata pending 8192)                  ; a dirent that did not fit, per fd
(defdata unknown-seen 512)
(defdata delta-key 8)                   ; pthread key 300: the per-thread-window delta
(defdata tstate-key 8)                  ; a key of our own: per-thread shim state (below)
(defdata strace-on 8)
(defdata said-rwx 8)
(defdata g-code-lo 8)
(defdata g-code-hi 8)

;;; Per-thread shim state (modus-shim.c's __thread variables), 32 bytes from calloc, under
;;; TSTATE-KEY:  +0 is_clone  +8 CHILD_CLEARTID word  +16 last JIT-flip pc  +24 flips there

(defstring s-prefix "modus-shim: ")
(defstring s-nl "
")
(defstring s-not-elf "not an aarch64 ELF image in __TEXT,__modus")
(defstring s-bad-phdr "unexpected program header")
(defstring s-remap "mach_vm_remap of the image code failed")
(defstring s-bss "could not map the image's BSS tail")
(defstring s-slot "could not map the syscall slot page")
(defstring s-stack "could not map the image stack")
(defstring s-key "pthread_key_create failed")
(defstring s-key-taken "the per-thread-window pthread key is already taken")
(defstring s-unimpl "unimplemented Linux syscall ")
(defstring s-rwx "RWX mmap refused by macOS; mapping RW (no JIT)")
(defstring s-slice "could not map the arena's data end RW; keeping MAP_JIT")
(defstring s-jit-pingpong "JIT code writes JIT memory (cannot run under MAP_JIT) at pc=")
(defstring s-slice-ran "code ran in the JIT arena's RW data end (see JIT_DATA_SLICE) at ")
(defstring s-signal "signal ")
(defstring s-at-pc " at pc=")
(defstring s-fault-addr " fault-addr=")
(defstring s-sp " ")
(defstring s-x "x")
(defstring s-eq "=")
(defstring s-fp " fp=")
(defstring s-lr " lr=")
(defstring s-spr " sp=")
(defstring s-sys "] sys ")
(defstring s-lb "[")
(defstring s-lp "(")
(defstring s-comma ", ")
(defstring s-rp-eq ") = ")
(defstring s-image-at " (image +")
(defstring s-rparen ")")
(defstring s-env-strace "MODUS_SHIM_STRACE")

;;; -------------------------------------------------------------- diagnostics ----

(defun say (s) (ccall c-write 2 s (ccall c-strlen s)))
(defun say-hex (v)
  "V in hex, 0x and no leading zeros."
  (with-buffers ((b 32))
    (st8 b 48 0) (st8 b 120 1)
    (let ((n 2) (started 0) (i 60))
      (while (>= i 0)
        (let ((d (logand (shr v i) 15)))
          (when (or started (/= d 0) (= i 0))
            (st8 (+ b n) (if (< d 10) (+ 48 d) (+ 87 d)))
            (setq n (+ n 1)) (setq started 1)))
        (setq i (- i 4)))
      (ccall c-write 2 b n))))
(defun say-dec (v)
  "V in signed decimal."
  (with-buffers ((b 32))
    (let ((neg (< v 0)) (x v) (i 31))
      (when neg (setq x (- 0 v)))
      (while 1
        (st8 (+ b i) (+ 48 (urem x 10)))
        (setq x (udiv x 10))
        (setq i (- i 1))
        (when (= x 0) (break)))
      (when neg (st8 (+ b i) 45) (setq i (- i 1)))
      (ccall c-write 2 (+ b i 1) (- 31 i)))))
(defun die (what code)
  (say (addr s-prefix)) (say what) (say (addr s-lp)) (say-dec code) (say (addr s-rparen)) (say (addr s-nl))
  (ccall c-exit 111))
(defun say-reg (name v)
  "NAME=V, and the offset into the image when V points into it."
  (say name) (say-hex v)
  (when (and (u>= v (ld64 (addr g-code-lo))) (u< v (ld64 (addr g-code-hi))))
    (say (addr s-image-at)) (say-hex (- v (ld64 (addr g-code-lo)))) (say (addr s-rparen))))

;;; ------------------------------------------------------------------- errno ----

(defun errno () (lds32 (ccall c-error)))
(defun lx-errno (e)
  "Darwin errno -> Linux errno, for the values that differ (1..34 agree)."
  (cond ((= e 35) 11) ((= e 36) 115) ((= e 37) 114) ((= e 38) 88) ((= e 39) 89)
        ((= e 40) 90) ((= e 41) 91) ((= e 42) 92) ((= e 43) 93) ((= e 45) 95)
        ((= e 47) 97) ((= e 48) 98) ((= e 49) 99) ((= e 50) 100) ((= e 51) 101)
        ((= e 53) 103) ((= e 54) 104) ((= e 55) 105) ((= e 56) 106) ((= e 57) 107)
        ((= e 60) 110) ((= e 61) 111) ((= e 62) 40) ((= e 63) 36) ((= e 65) 113)
        ((= e 66) 39) ((= e 77) 37) ((= e 78) 38)
        (t e)))
(defun neg-errno () (- 0 (lx-errno (errno))))

;;; ---------------------------------------------------------- flags, constants ----

(defun dx-dirfd (fd)
  (let ((f (sext32 fd))) (if (= f linux-at-fdcwd) darwin-at-fdcwd f)))
(defun dx-open-flags (f)
  (let ((o (logand f 3)))                          ; O_RDONLY/WRONLY/RDWR agree
    (when (logtest f #x40) (set-or o #x200))        ; O_CREAT
    (when (logtest f #x80) (set-or o #x800))        ; O_EXCL
    (when (logtest f #x100) (set-or o #x20000))     ; O_NOCTTY
    (when (logtest f #x200) (set-or o #x400))       ; O_TRUNC
    (when (logtest f #x400) (set-or o #x8))         ; O_APPEND
    (when (logtest f #x800) (set-or o #x4))         ; O_NONBLOCK
    (when (logtest f #x14000) (set-or o #x100000))  ; O_DIRECTORY (aarch64 0x4000; x86-64's 0x10000 too)
    (when (logtest f #x8000) (set-or o #x100))      ; O_NOFOLLOW
    (when (logtest f #x80000) (set-or o #x1000000)) ; O_CLOEXEC
    o))
(defun dx-at-flags (f)
  (let ((o 0))
    (when (logtest f #x100) (set-or o #x20))        ; AT_SYMLINK_NOFOLLOW
    (when (logtest f #x200) (set-or o #x80))        ; AT_REMOVEDIR
    o))
(defun dx-clock (c)
  (cond ((= c 0) 0) ((= c 1) 6) ((= c 2) 12) ((= c 3) 16) ((= c 4) 4) ((= c 7) 6) (t 0)))

(defun to-lx-stat (s l)
  "Darwin struct stat (arm64, 144 bytes) at S -> Linux aarch64 struct stat (128) at L."
  (ccall c-memset l 0 128)
  (st64 l (lds32 s 0) 0)                 ; dev
  (st64 l (ld64 s 8) 8)                  ; ino
  (st32 l (ld16 s 4) 16)                 ; mode
  (st32 l (ld16 s 6) 20)                 ; nlink
  (st32 l (ld32 s 16) 24)                ; uid
  (st32 l (ld32 s 20) 28)                ; gid
  (st64 l (lds32 s 24) 32)               ; rdev
  (st64 l (ld64 s 96) 48)                ; size
  (st32 l (ld32 s 112) 56)               ; blksize
  (st64 l (ld64 s 104) 64)               ; blocks
  (st64 l (ld64 s 32) 72) (st64 l (ld64 s 40) 80)     ; atime
  (st64 l (ld64 s 48) 88) (st64 l (ld64 s 56) 96)     ; mtime
  (st64 l (ld64 s 64) 104) (st64 l (ld64 s 72) 112)   ; ctime
  0)

;;; ------------------------------------------------------------------- JIT ----

(defun jit-wp (exec) (ccall c-pthread-jit-write-protect-np exec))
(defun in-jit (a)
  (let ((i 0) (n (ld64 (addr n-jit))) (r (addr jit-regions)) (hit 0))
    (while (< i n)
      (when (and (u>= a (ld64 (+ r (* 16 i)))) (u< a (ld64 (+ r (* 16 i)) 8)))
        (setq hit 1) (break))
      (setq i (+ i 1)))
    hit))
(defun note-jit (lo hi)
  (let ((n (ld64 (addr n-jit))))
    (when (< n 64)
      (st64 (+ (addr jit-regions) (* 16 n)) lo)
      (st64 (+ (addr jit-regions) (* 16 n)) hi 8)
      (st64 (addr n-jit) (+ n 1)))
    0))

;;; THE ARENA'S DATA END IS NOT MAP_JIT (modus-shim.c, JIT_DATA_SLICE): a JIT region big
;;; enough to be the arena keeps its top 16 MB as ordinary RW memory, so the image's writes
;;; there (linkage and mutex cells) do not flip the thread's write/exec mode twice a lock.
(defun jit-split (p len)
  (if (u< len #x4000000)
      len
      (let ((lo (- (+ p len) #x1000000)))
        (ccall c-munmap lo #x1000000)
        (let ((d (ccall c-mmap lo #x1000000 3 #x1012 -1 0)))     ; RW, PRIVATE|ANON|FIXED
          (if (= d -1)
              (progn (say (addr s-prefix)) (say (addr s-slice)) (say (addr s-nl))
                     (ccall c-mmap lo #x1000000 7 #x1802 -1 0)    ; RWX, PRIVATE|ANON|JIT
                     len)
              (progn (st64 (addr data-slice-lo) lo) (st64 (addr data-slice-hi) (+ lo #x1000000))
                     (- len #x1000000)))))))

;;; mmap: Linux flags -> Darwin.  macOS refuses RWX anonymous memory without MAP_JIT, and
;;; MAP_JIT with MAP_FIXED; it honours MAP_JIT's address HINT, which keeps the JIT arena at
;;; its fixed address.  MAP_FIXED_NOREPLACE (Linux 0x100000) is emulated: a mapping that did
;;; not land where asked is undone and the answer is EEXIST.
(defun dx-mmap (addr len prot flags fd off)
  (let ((f (logand flags 3))                       ; MAP_SHARED / MAP_PRIVATE agree
        (noreplace (logtest flags #x100000))
        (jit (and (logtest prot 4) (logtest flags #x20))))
    (when (logtest flags #x20) (set-or f #x1000))  ; MAP_ANON
    (if jit
        (set-or f #x800)                           ; MAP_JIT
        (when (and (logtest flags #x10) (not noreplace)) (set-or f #x10)))   ; MAP_FIXED
    (let ((p (ccall c-mmap addr len prot f fd off)))
      (when (and (/= p -1) jit)
        (when (and noreplace (/= p addr)) (ccall c-munmap p len) (return -17))
        (let ((jl (jit-split p len)))
          (note-jit p (+ p jl))
          (jit-wp 1)                               ; running mode: executable
          (return p)))
      (when (and (= p -1) (logtest prot 4) (logtest prot 2) (= addr 0))
        ;; the runtime's exec-page primitive with no JIT arena only ever holds data
        (when (= (ld64 (addr said-rwx)) 0)
          (st64 (addr said-rwx) 1)
          (say (addr s-prefix)) (say (addr s-rwx)) (say (addr s-nl)))
        (setq p (ccall c-mmap 0 len (logand prot 3) f fd off)))
      (when (= p -1) (return (neg-errno)))
      (when (and noreplace (/= p addr)) (ccall c-munmap p len) (return -17))
      p)))

;;; read(2) INTO the JIT arena fails with EFAULT (the kernel's copy-out ignores the thread's
;;; MAP_JIT write mode): read into ordinary memory and copy in with write mode open.
(defun jit-read (fd buf n)
  (if (or (= n 0) (and (= (in-jit buf) 0) (= (in-jit (+ buf n -1)) 0)))
      (ccall c-read fd buf n)
      (let ((tmp (ccall c-malloc n)))
        (if (= tmp 0)
            (progn (st32 (ccall c-error) 12) -1)
            (let ((r (ccall c-read fd tmp n)) (e (errno)))
              (when (> r 0) (jit-wp 0) (ccall c-memcpy buf tmp r) (jit-wp 1))
              (ccall c-free tmp)
              (st32 (ccall c-error) e)
              r)))))

;;; ------------------------------------------------------------------ signals ----

(defun dx-sig (l)
  (cond ((= l 7) 10) ((= l 10) 30) ((= l 12) 31) ((= l 17) 20) ((= l 18) 19)
        ((= l 19) 17) ((= l 20) 18) ((= l 23) 16) ((= l 29) 23) ((= l 31) 12) (t l)))
(defun dx-sigset (lx)
  (let ((s 0) (l 1))
    (while (< l 32)
      (when (logtest lx (shl 1 (- l 1))) (set-or s (shl 1 (- (dx-sig l) 1))))
      (setq l (+ l 1)))
    s))
(defun lx-sigset (s)
  (let ((m 0) (l 1))
    (while (< l 32)
      (when (logtest s (shl 1 (- (dx-sig l) 1))) (set-or m (shl 1 (- l 1))))
      (setq l (+ l 1)))
    m))
(defun fault-slot (sig)
  (cond ((= sig 11) 0) ((= sig 10) 1) ((= sig 8) 2) ((= sig 4) 3) (t -1)))
(defun fault-slot-lx-sig (slot)
  (cond ((= slot 0) 11) ((= slot 1) 7) ((= slot 2) 8) (t 4)))

;;; rt_sigaction.  SIGSEGV/BUS/FPE/ILL stay the shim's own (ON-FAULT): what the image
;;; registers for them is kept and chained to, with Linux's siginfo and ucontext, which its
;;; fault stub reads.  Linux struct sigaction: handler, flags, restorer, mask (8 bytes each).
;;; Darwin's: handler, sa_mask (u32 @8), sa_flags (int @12).
(defun dx-rt-sigaction (lsig nw old)
  (let* ((sig (dx-sig lsig)) (slot (fault-slot sig)))
    (when (>= slot 0)
      (let ((h (+ (addr image-fault-handler) (* 32 slot))))
        (when old (ccall c-memcpy old h 32))
        (when nw (ccall c-memcpy h nw 32))
        (return 0)))
    (with-buffers ((dn 16) (dold 16))
      (ccall c-memset dn 0 16)
      (ccall c-memset dold 0 16)
      (when nw
        (let ((fl (ld64 nw 8)) (f 0))
          (when (logtest fl #x4) (set-or f #x40))          ; SA_SIGINFO
          (when (logtest fl #x08000000) (set-or f #x1))    ; SA_ONSTACK
          (when (logtest fl #x10000000) (set-or f #x2))    ; SA_RESTART
          (when (logtest fl #x40000000) (set-or f #x10))   ; SA_NODEFER
          (when (logtest fl #x80000000) (set-or f #x4))    ; SA_RESETHAND
          (st64 dn (ld64 nw 0))
          (st32 dn (dx-sigset (ld64 nw 24)) 8)
          (st32 dn f 12)))
      (when (< (ccall c-sigaction sig (if nw dn 0) dold) 0) (return (neg-errno)))
      (when old
        (let ((df (ld32 dold 12)) (f 0))
          (when (logtest df #x40) (set-or f #x4))
          (when (logtest df #x10) (set-or f #x40000000))
          (when (logtest df #x2) (set-or f #x10000000))
          (when (logtest df #x1) (set-or f #x08000000))
          (st64 old (ld64 dold 0))
          (st64 old f 8)
          (st64 old 0 16)
          (st64 old (lx-sigset (ld32 dold 8)) 24)))
      0)))

;;; sigaltstack: Linux stack_t { sp, int flags, size }, Darwin's { sp, size, int flags };
;;; SS_DISABLE is 2 on Linux and 4 here.
(defun dx-sigaltstack (nw old)
  (with-buffers ((dn 24) (dold 24))
    (when nw
      (st64 dn (ld64 nw 0))
      (st64 dn (ld64 nw 16) 8)
      (st32 dn (if (logtest (ld32 nw 8) 2) 4 0) 16))
    (when (< (ccall c-sigaltstack (if nw dn 0) dold) 0) (return (neg-errno)))
    (when old
      (let ((f (ld32 dold 16)) (lf 0))
        (when (logtest f 1) (set-or lf 1))
        (when (logtest f 4) (set-or lf 2))
        (st64 old (ld64 dold 0))
        (st64 old lf 8)
        (st64 old (ld64 dold 8) 16)))
    0))

(defun dx-sigprocmask (how set old)
  (with-buffers ((ns 8) (os 8))
    (let ((h (cond ((= how 0) 1) ((= how 1) 2) (t 3))))
      (when set (st32 ns (dx-sigset (ld64 set))))
      (when (< (ccall c-sigprocmask h (if set ns 0) os) 0) (return (neg-errno)))
      (when old (st64 old (lx-sigset (ld32 os))))
      0)))

;;; ------------------------------------------------------------- directories ----
;;; getdents64 packs linux_dirent64 records; Darwin has no public equivalent, so keep a DIR*
;;; per fd (fdopendir on a dup, so closedir cannot close the image's fd) and refill from
;;; readdir; one record that did not fit waits for the next call.  Darwin struct dirent:
;;; d_ino @0, d_seekoff @8, d_reclen @16, d_namlen @18, d_type @20, d_name @21.

(defun dx-getdents64 (fd buf len)
  (when (or (< fd 0) (>= fd 1024)) (return -9))
  (let ((dslot (+ (addr dirs) (* 8 fd))) (pslot (+ (addr pending) (* 8 fd))) (used 0))
    (when (= (ld64 dslot) 0)
      (let ((d (ccall c-dup fd)))
        (when (< d 0) (return (neg-errno)))
        (let ((dp (ccall c-fdopendir d)))
          (when (= dp 0)
            (let ((e (errno))) (ccall c-close d) (return (- 0 (lx-errno e)))))
          (st64 dslot dp))))
    (while 1
      (let ((e (ld64 pslot)))
        (st64 pslot 0)
        (when (= e 0) (setq e (ccall c-readdir (ld64 dslot))))
        (when (= e 0) (break))
        ;; ROUND_UP(sizeof(struct lx_dirent64) = 24, + name + NUL, 8); the name is at +19
        (let* ((nlen (ccall c-strlen (+ e 21)))
               (rec (logand (+ 24 nlen 1 7) -8)))
          (when (> (+ used rec) len)
            (when (= used 0) (return -22))
            (st64 pslot e)
            (break))
          (let ((d (+ buf used)))
            (st64 d (ld64 e 0))
            (st64 d (+ used rec) 8)
            (st16 d rec 16)
            (st8 d (ld8 e 20) 18)                     ; DT_* values agree
            (ccall c-memcpy (+ d 19) (+ e 21) (+ nlen 1))
            (setq used (+ used rec))))))
    used))
(defun forget-dir (fd)
  (when (and (>= fd 0) (< fd 1024))
    (let ((dslot (+ (addr dirs) (* 8 fd))))
      (when (/= (ld64 dslot) 0)
        (ccall c-closedir (ld64 dslot))
        (st64 dslot 0)
        (st64 (+ (addr pending) (* 8 fd)) 0))))
  0)

;;; ----------------------------------------------------------------- sockets ----
;;; Linux sockaddr: u16 family, data; Darwin: u8 length, u8 family, data.  AF_INET is 2 on
;;; both; AF_INET6 is 10 on Linux and 30 here.

(defun dx-sockaddr (lx len out)
  (when (or (= lx 0) (< len 2)) (return 0))
  (when (> len 128) (setq len 128))
  (ccall c-memcpy out lx len)
  (let ((fam (ld16 lx)))
    (st8 out len)
    (st8 out (if (= fam 10) 30 fam) 1))
  len)
(defun lx-sockaddr (d lx lxlen dlen)
  (when (and lx lxlen)
    (let ((n (if (< dlen (ld32 lxlen)) dlen (ld32 lxlen)))
          (fam (if (= (ld8 d 1) 30) 10 (ld8 d 1))))
      (ccall c-memcpy lx d n)
      (when (>= n 2) (st8 lx fam) (st8 lx (shr fam 8) 1))
      (st32 lxlen dlen)))
  0)
(defun dx-sock-level (level) (if (= level 1) #xFFFF level))
(defun dx-sock-opt (level opt)
  (if (= level 1)
      (cond ((= opt 2) 4) ((= opt 4) #x1007) ((= opt 6) #x20) ((= opt 7) #x1001)
            ((= opt 8) #x1002) ((= opt 9) 8) ((= opt 13) #x80) ((= opt 15) #x200)
            ((= opt 20) #x1006) ((= opt 21) #x1005) (t -1))
      opt))                                     ; IPPROTO_TCP = 6, TCP_NODELAY = 1 agree
(defun dx-socket (domain type proto)
  (let ((fd (ccall c-socket (if (= domain 10) 30 domain) (logand type 15) proto)))
    (when (< fd 0) (return (neg-errno)))
    (when (logtest type #x800)                  ; SOCK_NONBLOCK
      (ccallv c-fcntl 2 fd 4 (logior (ccallv c-fcntl 2 fd 3 0) 4)))
    (when (logtest type #x80000)                ; SOCK_CLOEXEC
      (ccallv c-fcntl 2 fd 2 1))
    (no-sigpipe fd)
    fd))
(defun no-sigpipe (fd)
  "SO_NOSIGPIPE: Linux callers expect EPIPE, not a signal."
  (with-buffers ((one 8))
    (st32 one 1)
    (ccall c-setsockopt fd #xFFFF #x1022 one 4)))

(defun dx-sockopt-call (nr fd level opt val lenp)
  (let ((lvl (dx-sock-level level)) (o (dx-sock-opt level opt)))
    (when (< o 0) (return (if (= nr 208) 0 -92)))   ; unknown: accept a set, ENOPROTOOPT a get
    (when (and (= lvl #xFFFF) (or (= o #x1006) (= o #x1005)))
      ;; a Linux timeval is two 64-bit words; Darwin's is { long tv_sec; int tv_usec }
      (with-buffers ((tv 16) (n 8))
        (ccall c-memset tv 0 16)
        (when (= nr 208)
          (st64 tv (ld64 val))
          (st32 tv (ld64 val 8) 8)
          (return (ret-c (ccall c-setsockopt fd lvl o tv 16))))
        (st32 n 16)
        (when (< (ccall c-getsockopt fd lvl o tv n) 0) (return (neg-errno)))
        (st64 val (ld64 tv))
        (st64 val (lds32 tv 8) 8)
        (when lenp (st32 lenp 16))
        (return 0)))
    (when (= nr 208) (return (ret-c (ccall c-setsockopt fd lvl o val lenp))))
    (with-buffers ((n 8))
      (st32 n (if lenp (ld32 lenp) 0))
      (when (< (ccall c-getsockopt fd lvl o val n) 0) (return (neg-errno)))
      (when lenp (st32 lenp (ld32 n)))
      0)))

;;; ----------------------------------------------------------------- threads ----
;;; clone(CLONE_VM|...|CLONE_THREAD|PARENT_SETTID|CHILD_CLEARTID) returns twice with every
;;; register but x0 and SP the parent's.  A pthread cannot be born that way, so the stub's
;;; frame is copied into a resume context and a detached pthread jumps into the image with
;;; it: PC after the stub's call, SP 16 below the new stack (the stub's LR pop lands on it),
;;; x0 = 0.  The delta lives in pthread key 300, which the image reads at TPIDRRO_EL0 + 2400
;;; (hosted-layout-env.lisp +LAYOUT-DARWIN-TSD-KEY+).
;;;   stub frame (SYSCALL-STUB, 608 bytes): x1..x15 @0, x17 @120, x29 @128, x30 @136,
;;;     q0-7 q16-31 @144, x19..x28 @528
;;;   resume context (MODUS-RESUME): x0..x30 @0, sp @248, pc @256
;;;   clone start: the context, then the delta @264 and the CHILD_CLEARTID word @272

(defun tstate () (ccall c-pthread-getspecific (ld64 (addr tstate-key))))
(defun set-delta (d) (ccall c-pthread-setspecific (ld64 (addr delta-key)) d))
(defun get-delta () (ccall c-pthread-getspecific (ld64 (addr delta-key))))

(defun reserve-delta-key ()
  "Create pthread keys until we hold 300 (keys are handed out lowest-free-first)."
  (with-buffers ((k 8))
    (while 1
      (when (/= (ccall c-pthread-key-create k 0) 0) (die (addr s-key) 0))
      (when (= (ld64 k) 300) (break))
      (when (> (ld64 k) 300) (die (addr s-key-taken) (ld64 k))))
    (st64 (addr delta-key) 300)
    (when (/= (ccall c-pthread-key-create k 0) 0) (die (addr s-key) 1))
    (st64 (addr tstate-key) (ld64 k))
    0))

(defun lx-tid (id) (let ((t1 (logand id #x7FFFFFFF))) (if t1 t1 1)))
(defun my-tid ()
  (if (/= (ccall c-pthread-main-np) 0)
      (ccall c-getpid)                   ; Linux: the group leader's TID is the PID
      (with-buffers ((id 8))
        (st64 id 0)
        (ccall c-pthread-threadid-np 0 id)
        (lx-tid (ld64 id)))))

(defun new-altstack ()
  (with-buffers ((ss 24))
    (st64 ss (ccall c-malloc #x10000))
    (st64 ss #x10000 8)
    (st32 ss 0 16)
    (ccall c-sigaltstack ss 0)))

(defun clone-child (cs)
  (with-buffers ((ctx 272))
    (ccall c-memcpy ctx cs 264)
    (let ((ts (ccall c-calloc 1 32)))
      (st64 ts 1 0)                      ; is_clone
      (st64 ts (ld64 cs 272) 8)          ; CHILD_CLEARTID
      (ccall c-pthread-setspecific (ld64 (addr tstate-key)) ts))
    (set-delta (ld64 cs 264))            ; the parent's window, as Linux inherits TPIDR_EL0
    (ccall c-free cs)
    (new-altstack)                       ; per thread: the fault handler needs one here too
    (modus-resume ctx)))

(defun dx-clone (flags newsp ptid ctid frame)
  (when (not (logtest flags #x10000)) (return -38))     ; only CLONE_THREAD
  (let ((cs (ccall c-calloc 1 280)) (i 1))
    (while (<= i 15) (st64 (+ cs (* 8 i)) (ld64 (+ frame (* 8 (- i 1))))) (setq i (+ i 1)))
    (st64 cs (ld64 frame 120) (* 8 17))
    (setq i 19)
    (while (<= i 28) (st64 (+ cs (* 8 i)) (ld64 (+ frame 528 (* 8 (- i 19))))) (setq i (+ i 1)))
    (st64 cs (ld64 frame 128) (* 8 29))
    (st64 cs (ld64 frame 136) (* 8 30))
    (st64 cs 0 0)
    (st64 cs (- newsp 16) 248)
    (st64 cs (ld64 frame 136) 256)
    (st64 cs (get-delta) 264)
    (st64 cs (if (logtest flags #x200000) ctid 0) 272)
    (with-buffers ((at 64) (th 8) (id 8))
      (ccall c-pthread-attr-init at)
      (ccall c-pthread-attr-setdetachstate at 2)      ; PTHREAD_CREATE_DETACHED
      (ccall c-pthread-attr-setstacksize at #x40000)
      (let ((e (ccall c-pthread-create th at (addr clone-child) cs)))
        (ccall c-pthread-attr-destroy at)
        (when (/= e 0) (ccall c-free cs) (return (- 0 (lx-errno e)))))
      (st64 id 0)
      (ccall c-pthread-threadid-np (ld64 th) id)
      (let ((tid (lx-tid (ld64 id))))
        ;; PARENT_SETTID: Linux writes it before clone returns in the parent -- the join polls it
        (when (logtest flags #x100000) (atomic-st32 ptid tid))
        tid))))

(defun thread-exit ()
  (let ((ts (tstate)))
    (when (and ts (ld64 ts 8))
      (atomic-st32 (ld64 ts 8) 0)
      (ccall c-os-sync-wake-all (ld64 ts 8) 4 0)))
  (ccall c-pthread-exit 0))

;;; futex(uaddr, op, val, timeout): WAIT and WAKE, private or not.
(defun dx-futex (ua op val to)
  (let ((o (logand op #x7F)))
    (cond ((= o 0)
           (when (/= (atomic-ld32 ua) (zext32 val)) (return -11))       ; EAGAIN
           (let ((r 0))
             (if to
                 (let ((ns (+ (* (ld64 to) 1000000000) (ld64 to 8))))
                   (when (= ns 0) (return -110))
                   ;; OS_CLOCK_MACH_ABSOLUTE_TIME = 32
                   (setq r (ccall c-os-sync-wait-timeout ua (zext32 val) 4 0 32 ns)))
                 (setq r (ccall c-os-sync-wait ua (zext32 val) 4 0)))
             (when (>= r 0) (return 0))
             (let ((e (errno)))
               (cond ((= e 60) -110) ((= e 4) -4) (t 0)))))
          ((= o 1)
           (when (<= (sext32 val) 0) (return 0))
           (let ((r (if (= (sext32 val) 1)
                        (ccall c-os-sync-wake-any ua 4 0)
                        (ccall c-os-sync-wake-all ua 4 0))))
             (if (= r 0) 1 0)))              ; ENOENT: nobody was waiting
          (t -38))))

;;; THE CALLER'S FRAME CHAIN (pseudo-call 1081: buffer, capacity in words).  The stub saved
;;; the calling function's x29; [x29] is the caller's x29, [x29+8] the return address.
(defun modus-backtrace (fp out cap)
  (let ((n 0) (start fp))
    (while (and (< n cap) (u>= fp 4096) (= (logand fp 7) 0))
      (let ((next (ld64 fp)) (ret (ld64 fp 8)))
        (when (u< ret 4096) (break))
        (st64 (+ out (* 8 n)) ret)
        (setq n (+ n 1))
        (when (or (u<= next fp) (u> (- next start) #x10000000)) (break))
        (setq fp next)))
    n))

;;; ---------------------------------------------------------------- dispatch ----

(defun modus-syscall (a0 a1 a2 a3 a4 a5 nr frame)
  (let ((r (modus-syscall-1 a0 a1 a2 a3 a4 a5 nr frame)))
    (when (ld64 (addr strace-on))
      (say (addr s-lb)) (say-dec (my-tid)) (say (addr s-sys)) (say-dec nr) (say (addr s-lp))
      (say-hex a0) (say (addr s-comma)) (say-hex a1) (say (addr s-comma)) (say-hex a2)
      (say (addr s-comma)) (say-hex a3) (say (addr s-rp-eq)) (say-dec r) (say (addr s-nl)))
    r))

(defun modus-syscall-1 (a0 a1 a2 a3 a4 a5 nr frame)
  (cond
    ((= nr 63) (ret-c (jit-read a0 a1 a2)))
    ((= nr 64) (ret-c (ccall c-write a0 a1 a2)))
    ((= nr 56) (ret-c (ccallv c-openat 3 (dx-dirfd a0) a1 (dx-open-flags a2) a3)))
    ((= nr 57) (forget-dir (sext32 a0)) (ret-c (ccall c-close a0)))
    ((= nr 61) (dx-getdents64 (sext32 a0) a1 a2))
    ((= nr 62) (ret-c (ccall c-lseek a0 a1 a2)))
    ((= nr 79) (with-buffers ((st 144))                        ; newfstatat
                 (if (< (ccall c-fstatat (dx-dirfd a0) a1 st (dx-at-flags a3)) 0)
                     (neg-errno)
                     (to-lx-stat st a2))))
    ((= nr 80) (with-buffers ((st 144))                        ; fstat
                 (if (< (ccall c-fstat a0 st) 0) (neg-errno) (to-lx-stat st a1))))
    ((= nr 222) (dx-mmap a0 a1 a2 a3 (sext32 a4) a5))
    ((= nr 215) (ret-c (ccall c-munmap a0 a1)))
    ((= nr 226) (ret-c (ccall c-mprotect a0 a1 a2)))
    ((= nr 132) (dx-sigaltstack a0 a1))
    ((= nr 93) (let ((ts (tstate)))                           ; exit: the THREAD
                 (when (and ts (ld64 ts 0)) (thread-exit))
                 (ccall c-exit a0)))
    ((= nr 94) (ccall c-exit a0))                              ; exit_group
    ((= nr 220) (dx-clone a0 a1 a2 a4 frame))
    ((= nr 98) (dx-futex a0 a1 a2 a3))
    ((= nr 1000) (set-delta a0) 0)                             ; set the thread delta
    ((= nr 1020) (ret-c (ccall c-mlock a0 a1)))                ; natrium's secret arena
    ((= nr 1081) (modus-backtrace (ld64 frame 128) a0 a1))
    ((= nr 25)                                                 ; fcntl: O_NONBLOCK 2048 <-> 4
     (let ((cmd (sext32 a1)))
       (cond ((= cmd 3)
              (let ((fl (ccallv c-fcntl 2 a0 3 0)))
                (if (< fl 0) (neg-errno)
                    (logior (logand fl (lognot 4)) (if (logtest fl 4) 2048 0)))))
             ((= cmd 4)
              (ret-c (ccallv c-fcntl 2 a0 4 (logior (logand a2 (lognot 2048)) (if (logtest a2 2048) 4 0)))))
             (t (ret-c (ccallv c-fcntl 2 a0 cmd (sext32 a2)))))))
    ((= nr 73)                                                 ; ppoll as poll(2)
     (ret-c (ccall c-poll a0 a1 (if a2 (+ (* (ld64 a2) 1000) (sdiv (ld64 a2 8) 1000000)) -1))))
    ((= nr 113) (ret-c (ccall c-clock-gettime (dx-clock a0) a1)))   ; timespec: same layout
    ((= nr 169) (with-buffers ((tv 16))                        ; gettimeofday
                  (when (< (ccall c-gettimeofday tv 0) 0) (return (neg-errno)))
                  (when a0 (st64 a0 (ld64 tv)) (st64 a0 (lds32 tv 8) 8))
                  0))
    ((= nr 101) (ret-c (ccall c-nanosleep a0 a1)))
    ((= nr 124) (ret-c (ccall c-sched-yield)))
    ((= nr 172) (ccall c-getpid))
    ((= nr 178) (my-tid))
    ((= nr 17) (if (= (ccall c-getcwd a0 a1) 0) (neg-errno) (+ (ccall c-strlen a0) 1)))
    ((= nr 49) (ret-c (ccall c-chdir a0)))
    ((= nr 34) (ret-c (ccall c-mkdirat (dx-dirfd a0) a1 a2)))
    ((= nr 35) (ret-c (ccall c-unlinkat (dx-dirfd a0) a1 (dx-at-flags a2))))
    ((= nr 38) (ret-c (ccall c-renameat (dx-dirfd a0) a1 (dx-dirfd a2) a3)))
    ((= nr 48) (ret-c (ccall c-faccessat (dx-dirfd a0) a1 a2 0)))
    ((= nr 78) (ret-c (ccall c-readlinkat (dx-dirfd a0) a1 a2 a3)))
    ((= nr 46) (ret-c (ccall c-ftruncate a0 a1)))
    ((= nr 23) (ret-c (ccall c-dup a0)))
    ((= nr 29) -25)                                            ; ioctl: ENOTTY
    ((= nr 278) (ccall c-arc4random-buf a0 a1) a1)             ; getrandom
    ((= nr 214) -12)                                           ; brk: the image mmaps
    ((= nr 134) (dx-rt-sigaction a0 a1 a2))
    ((= nr 135) (dx-sigprocmask a0 a1 a2))
    ((= nr 129) (ret-c (ccall c-kill a0 (dx-sig a1))))
    ((= nr 198) (dx-socket a0 a1 a2))
    ((or (= nr 200) (= nr 203))                                ; bind, connect
     (with-buffers ((ss 128))
       (let ((n (dx-sockaddr a1 a2 ss)))
         (if (= nr 200) (ret-c (ccall c-bind a0 ss n)) (ret-c (ccall c-connect a0 ss n))))))
    ((= nr 201) (ret-c (ccall c-listen a0 a1)))
    ((or (= nr 202) (= nr 242))                                ; accept, accept4
     (with-buffers ((ss 128) (n 8))
       (st32 n 128)
       (let ((fd (ccall c-accept a0 ss n)))
         (when (< fd 0) (return (neg-errno)))
         (lx-sockaddr ss a1 a2 (ld32 n))
         (no-sigpipe fd)
         fd)))
    ((or (= nr 204) (= nr 205))                                ; getsockname, getpeername
     (with-buffers ((ss 128) (n 8))
       (st32 n 128)
       (when (< (if (= nr 204) (ccall c-getsockname a0 ss n) (ccall c-getpeername a0 ss n)) 0)
         (return (neg-errno)))
       (lx-sockaddr ss a1 a2 (ld32 n))
       0))
    ((= nr 206)                                                ; sendto
     (with-buffers ((ss 128))
       (let ((n (if a4 (dx-sockaddr a4 a5 ss) 0)))
         (ret-c (ccall c-sendto a0 a1 a2 (logand a3 1) (if a4 ss 0) n)))))
    ((= nr 207)                                                ; recvfrom
     (with-buffers ((ss 128) (n 8))
       (st32 n 128)
       (let ((r (ccall c-recvfrom a0 a1 a2 (if (logtest a3 2) 2 0) (if a4 ss 0) (if a4 n 0))))
         (when (< r 0) (return (neg-errno)))
         (when a4 (lx-sockaddr ss a4 a5 (ld32 n)))
         r)))
    ((or (= nr 208) (= nr 209)) (dx-sockopt-call nr a0 a1 a2 a3 a4))
    ((= nr 210) (ret-c (ccall c-shutdown a0 a1)))
    ((= nr 103) 0)                                             ; setitimer
    (t (when (and (>= nr 0) (< nr 512) (= (ld8 (+ (addr unknown-seen) nr)) 0))
         (st8 (+ (addr unknown-seen) nr) 1)
         (say (addr s-prefix)) (say (addr s-unimpl)) (say-dec nr) (say (addr s-nl)))
       -38)))

;;; ------------------------------------------------------------ the fault path ----
;;; Darwin siginfo: si_signo @0, si_code @8, si_addr @24.  ucontext: uc_mcontext @48 ->
;;; mcontext64: __es (16 bytes), __ss @16: x[29] @16, fp @248, lr @256, sp @264, pc @272,
;;; cpsr @280.  Linux's, as the image's fault stub reads them (translate-aarch64
;;; A64-EMIT-FAULT-RECORD): siginfo si_signo @0, si_code @8, si_addr @16; ucontext
;;; uc_mcontext @176 = fault_address, regs[31] @184, sp @432, pc @440, pstate @448.

(defun lx-fault-frames (lsig si uc lsi luc)
  (ccall c-memset lsi 0 128)
  (ccall c-memset luc 0 464)
  (let ((mc (ld64 uc 48)) (i 0))
    (st32 lsi lsig 0)
    (st32 lsi (ld32 si 8) 8)
    (st64 lsi (ld64 si 24) 16)
    (st64 luc (ld64 si 24) 176)
    (while (< i 29) (st64 (+ luc 184 (* 8 i)) (ld64 (+ mc 16 (* 8 i)))) (setq i (+ i 1)))
    (st64 luc (ld64 mc 248) (+ 184 (* 8 29)))
    (st64 luc (ld64 mc 256) (+ 184 (* 8 30)))
    (st64 luc (ld64 mc 264) 432)
    (st64 luc (ld64 mc 272) 440)
    (st64 luc (ld32 mc 280) 448))
  0)

(defun report-fault (sig si uc)
  (let ((mc (ld64 uc 48)) (i 0))
    (say (addr s-nl)) (say (addr s-prefix)) (say (addr s-signal)) (say-dec sig)
    (say-reg (addr s-at-pc) (ld64 mc 272))
    (say (addr s-fault-addr)) (say-hex (ld64 si 24)) (say (addr s-nl))
    (while (< i 29)
      (say (addr s-sp)) (say (addr s-x)) (say-dec i) (say-reg (addr s-eq) (ld64 (+ mc 16 (* 8 i))))
      (when (= (logand i 3) 3) (say (addr s-nl)))
      (setq i (+ i 1)))
    (say-reg (addr s-fp) (ld64 mc 248)) (say-reg (addr s-lr) (ld64 mc 256))
    (say-reg (addr s-spr) (ld64 mc 264)) (say (addr s-nl))
    (ccall c-exit (+ 128 sig))))

(defun on-fault (sig si uc)
  (let* ((mc (ld64 uc 48)) (pc (ld64 mc 272)) (a (ld64 si 24)) (slot (fault-slot sig)))
    (when (and (or (= sig 10) (= sig 11)) (in-jit a))
      ;; A fetch from the JIT arena in write mode (pc == fault address), or a write to it
      ;; in exec mode: flip and retry.  JIT code that writes JIT memory would ping-pong:
      ;; stop loudly after 8 flips at one pc.
      (let ((ts (tstate)))
        (when ts
          (if (= pc (ld64 ts 16))
              (progn (st64 ts (+ (ld64 ts 24) 1) 24)
                     (when (> (ld64 ts 24) 8)
                       (say (addr s-nl)) (say (addr s-prefix)) (say (addr s-jit-pingpong))
                       (say-hex pc) (say (addr s-nl))
                       (report-fault sig si uc)))
              (progn (st64 ts pc 16) (st64 ts 0 24)))))
      (jit-wp (if (= a pc) 1 0))
      (return 0))
    (when (and (= a pc) (u>= a (ld64 (addr data-slice-lo))) (u< a (ld64 (addr data-slice-hi))))
      (say (addr s-nl)) (say (addr s-prefix)) (say (addr s-slice-ran)) (say-hex pc) (say (addr s-nl)))
    (when (>= slot 0)
      (let ((h (ld64 (+ (addr image-fault-handler) (* 32 slot)))))
        (when (u> h 1)
          ;; the image's handler branches into the armed handler-case frame and never returns
          (with-buffers ((lsi 128) (luc 464))
            (let ((lsig (fault-slot-lx-sig slot)))
              (lx-fault-frames lsig si uc lsi luc)
              (callptr h lsig lsi luc))))))
    (report-fault sig si uc)))

(defun install-fault-report ()
  (new-altstack)
  (with-buffers ((sa 16))
    ;; SA_NODEFER: a chained image handler never returns, so the signal must not stay blocked
    (st64 sa (addr on-fault))
    (st32 sa 0 8)
    (st32 sa (logior #x40 #x1 #x10) 12)         ; SA_SIGINFO | SA_ONSTACK | SA_NODEFER
    (ccall c-sigaction 11 sa 0)                 ; SIGSEGV
    (ccall c-sigaction 10 sa 0)                 ; SIGBUS
    (ccall c-sigaction 4 sa 0)                  ; SIGILL
    (ccall c-sigaction 5 sa 0)                  ; SIGTRAP
    (ccall c-sigaction 8 sa 0))                 ; SIGFPE
  0)

;;; --------------------------------------------------------------------- main ----

(defun main (argc argv envp apple)
  (let* ((img (addr modus-image))
         (task (ld32 (import-value mach-task-self))))
    ;; "\177ELF", EM_AARCH64; one PT_LOAD at file offset 0
    (when (or (/= (ld32 img) #x464C457F) (/= (ld16 img 18) 183)) (die (addr s-not-elf) 0))
    (let* ((ph (+ img (ld64 img 32)))
           (vaddr (ld64 ph 16)) (filesz (ld64 ph 32)) (memsz (ld64 ph 40))
           (span (logand (+ filesz (- page16k 1)) (- 0 page16k)))
           (slot (- vaddr page16k)))
      (when (or (/= (ld32 ph) 1) (/= (ld64 ph 8) 0)) (die (addr s-bad-phdr) (ld32 ph)))
      ;; 1. the code: remap our own signed pages to the link address
      (with-buffers ((cell 8) (cur 8) (mx 8))
        (st64 cell vaddr)
        ;; mach_vm_remap(target, &addr, size, mask, VM_FLAGS_FIXED, src, src_addr, copy=FALSE,
        ;;               &cur, &max, VM_INHERIT_NONE)
        (let ((kr (ccall c-mach-vm-remap task cell span 0 0 task img 0 cur mx 2)))
          (when (or (/= kr 0) (/= (ld64 cell) vaddr)) (die (addr s-remap) kr)))
        ;; p_memsz slack past the file
        (when (u> memsz span)
          (st64 cell (+ vaddr span))
          (let ((kr (ccall c-mach-vm-allocate task cell
                           (logand (+ (- memsz span) (- page16k 1)) (- 0 page16k)) 0)))
            (when (/= kr 0) (die (addr s-bss) kr))))
        ;; 2. the syscall slot, one 16 KB page below the code base
        (st64 cell slot)
        (let ((kr (ccall c-mach-vm-allocate task cell page16k 0)))
          (when (/= kr 0) (die (addr s-slot) kr))))
      (st64 slot (addr syscall-stub))
      (st64 (addr g-code-lo) vaddr)
      (st64 (addr g-code-hi) (+ vaddr memsz))
      (st64 (addr strace-on) (if (ccall c-getenv (addr s-env-strace)) 1 0))
      ;; 3. a Linux initial stack: argc, argv..., NULL, envp..., NULL, AT_NULL.  Every
      ;; string is copied to a 16-byte-aligned home: some runtime paths read a char* back
      ;; through a fixnum-shaped load that drops bit 0, and macOS puts argv at odd addresses.
      (let* ((stack-size #x4000000)
             (stack (ccall c-mmap 0 stack-size 3 #x1002 -1 0))     ; RW, PRIVATE|ANON
             (envc 0) (total 0) (i 0))
        (when (= stack -1) (die (addr s-stack) (errno)))
        (while (ld64 (+ envp (* 8 envc))) (setq envc (+ envc 1)))
        (while (< i (+ argc envc))
          (let ((s (if (< i argc) (ld64 (+ argv (* 8 i))) (ld64 (+ envp (* 8 (- i argc)))))))
            (setq total (+ total (logand (+ (ccall c-strlen s) 1 15) -16))))
          (setq i (+ i 1)))
        (let* ((strs (ccall c-mmap 0 (+ total 16) 3 #x1002 -1 0))
               (words (+ 1 argc 1 envc 1 2))
               (sp (logand (- (+ stack stack-size) (logand (+ (* 8 words) 15) -16) 64) -16))
               (k 0) (at strs))
          (when (= strs -1) (die (addr s-stack) (errno)))
          (st64 sp argc) (setq k 1)
          (setq i 0)
          (while (< i (+ argc envc))
            (when (= i argc) (st64 (+ sp (* 8 k)) 0) (setq k (+ k 1)))   ; argv's NULL
            (let* ((s (if (< i argc) (ld64 (+ argv (* 8 i))) (ld64 (+ envp (* 8 (- i argc))))))
                   (n (+ (ccall c-strlen s) 1)))
              (ccall c-memcpy at s n)
              (st64 (+ sp (* 8 k)) at)
              (setq k (+ k 1))
              (setq at (+ at (logand (+ n 15) -16))))
            (setq i (+ i 1)))
          (when (= envc 0) (st64 (+ sp (* 8 k)) 0) (setq k (+ k 1)))      ; argv's NULL
          (st64 (+ sp (* 8 k)) 0) (setq k (+ k 1))                       ; envp's NULL
          (st64 (+ sp (* 8 k)) 0) (st64 (+ sp (* 8 k)) 0 8)              ; auxv: AT_NULL
          (reserve-delta-key)
          (ccall c-pthread-setspecific (ld64 (addr tstate-key)) (ccall c-calloc 1 32))
          (install-fault-report)
          (modus-enter sp (ld64 img 24)))))))

;;; ------------------------------------------------------------------ assembly ----
;;; syscall-stub.S, instruction for instruction.  The image emits, where a Linux image has
;;; SVC #0:  str x30, [sp, #-16]! ; x16 = slot ; ldr x16, [x16] ; blr x16 ; ldr x30, [sp], #16
;;; with the LINUX aarch64 syscall number in x8 and arguments in x0..x5.  This must behave
;;; like SVC: every register but x0 comes back unchanged.  x19..x28 are stored too (never
;;; reloaded -- MODUS-SYSCALL preserves them) so clone can start a thread with the caller's
;;; whole register file; the frame's address is MODUS-SYSCALL's 8th argument.

(defasm syscall-stub
  (:sub sp sp 608)
  (:stp x1 x2 sp 0) (:stp x3 x4 sp 16) (:stp x5 x6 sp 32) (:stp x7 x8 sp 48)
  (:stp x9 x10 sp 64) (:stp x11 x12 sp 80) (:stp x13 x14 sp 96) (:stp x15 x17 sp 112)
  (:stp x29 x30 sp 128)
  (:stpq q0 q1 sp 144) (:stpq q2 q3 sp 176) (:stpq q4 q5 sp 208) (:stpq q6 q7 sp 240)
  (:stpq q16 q17 sp 272) (:stpq q18 q19 sp 304) (:stpq q20 q21 sp 336) (:stpq q22 q23 sp 368)
  (:stpq q24 q25 sp 400) (:stpq q26 q27 sp 432) (:stpq q28 q29 sp 464) (:stpq q30 q31 sp 496)
  (:add x16 sp 528)                     ; STP reaches only 504: address the tail through x16
  (:stp x19 x20 x16 0) (:stp x21 x22 x16 16) (:stp x23 x24 x16 32) (:stp x25 x26 x16 48)
  (:stp x27 x28 x16 64)
  (:add x29 sp 128)                     ; a frame record the walk can follow
  (:mov x6 x8)                          ; 7th argument: the Linux syscall number
  (:mov x7 sp)                          ; 8th: this frame
  (:bl modus-syscall)
  (:ldpq q30 q31 sp 496) (:ldpq q28 q29 sp 464) (:ldpq q26 q27 sp 432) (:ldpq q24 q25 sp 400)
  (:ldpq q22 q23 sp 368) (:ldpq q20 q21 sp 336) (:ldpq q18 q19 sp 304) (:ldpq q16 q17 sp 272)
  (:ldpq q6 q7 sp 240) (:ldpq q4 q5 sp 208) (:ldpq q2 q3 sp 176) (:ldpq q0 q1 sp 144)
  (:ldp x29 x30 sp 128) (:ldp x15 x17 sp 112) (:ldp x13 x14 sp 96) (:ldp x11 x12 sp 80)
  (:ldp x9 x10 sp 64) (:ldp x7 x8 sp 48) (:ldp x5 x6 sp 32) (:ldp x3 x4 sp 16) (:ldp x1 x2 sp 0)
  (:add sp sp 608)
  (:ret))

;;; Enter the image: x0 = initial SP (pointing at argc, Linux layout), x1 = entry.
(defasm modus-enter
  (:mov sp x0) (:movz x29 0) (:movz x30 0) (:br x1))

;;; Start a cloned thread inside the image.  x0 = the resume context: the parent's register
;;; file at its clone call, with x0 = 0.  x16/x17 carry SP and PC; x18 is left alone (Apple's).
(defasm modus-resume
  (:ldr x16 x0 248) (:mov sp x16) (:ldr x17 x0 256)
  (:ldp x1 x2 x0 8) (:ldp x3 x4 x0 24) (:ldp x5 x6 x0 40) (:ldp x7 x8 x0 56)
  (:ldp x9 x10 x0 72) (:ldp x11 x12 x0 88) (:ldp x13 x14 x0 104) (:ldr x15 x0 120)
  (:ldp x19 x20 x0 152) (:ldp x21 x22 x0 168) (:ldp x23 x24 x0 184) (:ldp x25 x26 x0 200)
  (:ldp x27 x28 x0 216) (:ldp x29 x30 x0 232)
  (:ldr x0 x0 0)
  (:br x17))
