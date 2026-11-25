;;;; test/macho-shim-cases.lisp -- what test/macho-shim-test.lisp runs under qemu-aarch64:
;;;; host/macos/shim.lisp compiled together with this file, every libSystem import bound to
;;;; an IMPL-<name> test double below (or to IMPL-MISSING, which traps).  Two kinds of check:
;;;;   1. the compiler (mvm/macho-cc.lisp): arithmetic, comparisons, memory widths, control,
;;;;      calls with stack and variadic arguments, indirect calls, atomics;
;;;;   2. the shim's own translation code, driven with known inputs: flags, errno, signal
;;;;      numbers and sets, struct stat, getdents64 packing, mmap's MAP_JIT / FIXED /
;;;;      FIXED_NOREPLACE paths, sigaction, futex, the fault frames, clone's resume context,
;;;;      and SYSCALL-STUB's promise that every register but x0 survives a call.
;;;; Expected values are written out by hand from the Linux and Darwin headers, not computed
;;;; by the code under test.

(defasm linux-write (:li x8 64) (:svc) (:ret))
(defasm linux-exit (:li x8 94) (:svc) (:ret))

(defdata t-fails 8)
(defdata t-count 8)
(defdata rec 512)                       ; what the test doubles record
(defdata rec2 512)
(defdata fake-errno 8)
(defdata area 1024)                     ; register dumps
(defdata saved 64)
(defdata tstack 8192 16)
(defdata fake-tsd 4096)
(defstring s-fail "FAIL ")
(defstring s-got " got ")
(defstring s-want " want ")
(defstring s-done "macho-shim-test: ")
(defstring s-of " checks, ")
(defstring s-failed " failed
")
(defstring s-hello "hello")
(defstring s-missing "IMPL-MISSING called
")

(defun check (n got want)
  (st64 (addr t-count) (+ (ld64 (addr t-count)) 1))
  (when (/= got want)
    (st64 (addr t-fails) (+ (ld64 (addr t-fails)) 1))
    (say (addr s-fail)) (say-dec n) (say (addr s-got)) (say-hex got) (say (addr s-want))
    (say-hex want) (say (addr s-nl)))
  0)

;;; ------------------------------------------------------------ test doubles ----

(defun impl-c-write (fd buf n) (linux-write fd buf n))
(defun impl-c-strlen (s) (let ((n 0)) (while (ld8 (+ s n)) (setq n (+ n 1))) n))
(defun impl-c-memcpy (d s n) (let ((i 0)) (while (< i n) (st8 (+ d i) (ld8 (+ s i))) (setq i (+ i 1)))) d)
(defun impl-c-memset (d v n) (let ((i 0)) (while (< i n) (st8 (+ d i) v) (setq i (+ i 1)))) d)
(defun impl-c-error () (addr fake-errno))
(defun impl-c-pthread-setspecific (k v) (st64 (+ (addr fake-tsd) (* 8 k)) v) 0)
(defun impl-c-pthread-getspecific (k) (ld64 (+ (addr fake-tsd) (* 8 k))))
(defun impl-c-pthread-main-np () 1)
(defun impl-c-getpid () 4242)
(defun impl-c-dup (fd) (+ fd 100))
(defun impl-c-close (fd) (st64 (addr rec2) fd 200) 0)
(defun impl-c-fdopendir (fd) (st64 (addr rec2) fd 208) #x77770000)
(defun impl-c-closedir (d) (st64 (addr rec2) d 216) 0)
;; readdir: hands out the dirents laid out at REC (each 64 bytes), then NULL
(defun impl-c-readdir (d)
  (let ((i (ld64 (addr rec2) 224)))
    (st64 (addr rec2) (+ i 1) 224)
    (if (< i (ld64 (addr rec2) 232)) (+ (addr rec) (* 64 i)) 0)))
(defun impl-c-mmap (a l p f fd off)
  (let ((r (+ (addr rec2) (* 48 (ld64 (addr rec2) 504)))))
    (st64 (addr rec2) (+ (ld64 (addr rec2) 504) 1) 504)
    (st64 r a 0) (st64 r l 8) (st64 r p 16) (st64 r f 24) (st64 r fd 32) (st64 r off 40)
    (ld64 (addr rec) 496)))              ; the answer the test chose
(defun impl-c-munmap (a l) (st64 (addr rec) a 480) (st64 (addr rec) l 488) 0)
(defun impl-c-pthread-jit-write-protect-np (x) (st64 (addr rec) x 472) 0)
(defun impl-c-sigaction (sig nw old)
  (st64 (addr rec) sig 0)
  (when nw (st64 (addr rec) (ld64 nw) 8) (st64 (addr rec) (ld32 nw 8) 16) (st64 (addr rec) (ld32 nw 12) 24))
  (when old (st64 old #x1234 0) (st32 old 3 8) (st32 old #x43 12))   ; mask {1,2}, SIGINFO|RESTART|ONSTACK
  0)
(defun impl-c-os-sync-wait-timeout (ua v sz fl clk ns)
  (st64 (addr rec) ua 0) (st64 (addr rec) v 8) (st64 (addr rec) sz 16) (st64 (addr rec) fl 24)
  (st64 (addr rec) clk 32) (st64 (addr rec) ns 40)
  (st32 (addr fake-errno) 60) -1)       ; ETIMEDOUT
(defun impl-c-os-sync-wake-any (ua sz fl) (st64 (addr rec) 1 48) 0)
(defun impl-c-os-sync-wake-all (ua sz fl) (st64 (addr rec) 2 48) -1)
(defun impl-c-calloc (n s)
  (let ((p (ld64 (addr rec2) 496)))
    (when (= p 0) (setq p (addr area)))
    (impl-c-memset p 0 (* n s))
    (st64 (addr rec2) (+ p (* n s) 15) 496)
    p))
(defun impl-c-free (p) 0)
(defun impl-c-pthread-attr-init (a) 0)
(defun impl-c-pthread-attr-setdetachstate (a s) (st64 (addr rec) s 56) 0)
(defun impl-c-pthread-attr-setstacksize (a s) (st64 (addr rec) s 64) 0)
(defun impl-c-pthread-attr-destroy (a) 0)
(defun impl-c-pthread-create (th at start arg)
  (st64 th #x5151) (st64 (addr rec) start 72) (st64 (addr rec) arg 80) 0)
(defun impl-c-pthread-threadid-np (th idp) (st64 (addr rec) th 88) (st64 idp #x180000005) 0)
(defun impl-missing () (say (addr s-missing)) (linux-exit 99))

;;; an import with 11 arguments (mach_vm_remap's shape): record every argument, the last
;;; three from the stack, then return 7
(defasm impl-c-mach-vm-remap
  (:adr-addr x16 rec)
  (:str x0 x16 0) (:str x1 x16 8) (:str x2 x16 16) (:str x3 x16 24)
  (:str x4 x16 32) (:str x5 x16 40) (:str x6 x16 48) (:str x7 x16 56)
  (:ldr x9 sp 0) (:str x9 x16 64) (:ldr x9 sp 8) (:str x9 x16 72) (:ldr x9 sp 16) (:str x9 x16 80)
  (:li x0 7) (:ret))
;;; variadic: openat(fd, path, flags, ...) with the mode on the stack; fcntl(fd, cmd, ...)
(defasm impl-c-openat
  (:adr-addr x16 rec)
  (:str x0 x16 0) (:str x1 x16 8) (:str x2 x16 16) (:str x3 x16 24)
  (:ldr x9 sp 0) (:str x9 x16 32) (:li x0 5) (:ret))
(defasm impl-c-fcntl
  (:adr-addr x16 rec)
  (:str x0 x16 0) (:str x1 x16 8) (:str x2 x16 16)
  (:ldr x9 sp 0) (:str x9 x16 24) (:li x0 #x804) (:ret))

;;; ------------------------------------------------------------ compiler cases ----

(defun eight (a b c d e f g h) (+ a (* 2 b) (* 3 c) (* 4 d) (* 5 e) (* 6 f) (* 7 g) (* 8 h)))
(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defun early (n)
  (let ((i 0))
    (while 1
      (when (= i n) (return (* i 10)))
      (setq i (+ i 1)))
    -1))
(defun add3 (a b c) (+ a b c))

(defun test-compiler ()
  (check 1 (+ 1 2) 3)
  (check 2 (- 5 7) -2)
  (let ((x 123456789)) (check 3 (* x 1000) 123456789000))
  (let ((m7 -7) (two 2)) (check 4 (sdiv m7 two) -3) (check 5 (udiv -1 two) #x7FFFFFFFFFFFFFFF))
  (let ((a 17)) (check 6 (urem a 5) 2))
  (let ((a #xF0F0)) (check 7 (logand a #xFF) #xF0) (check 8 (logior a 1) #xF0F1)
    (check 9 (logxor a #xFFFF) #x0F0F) (check 10 (lognot a) -61681))
  (let ((m1 -1) (m16 -16) (k 4)) (check 11 (shr m1 60) 15) (check 12 (sar m16 2) -4)
    (check 13 (shl 1 63) #x8000000000000000) (check 14 (shr m1 k) #x0FFFFFFFFFFFFFFF)
    (check 15 (sar m16 k) -1) (check 16 (shl 3 k) 48))
  (let ((m1 -1) (z 0)) (check 17 (< m1 z) 1) (check 18 (u< m1 z) 0) (check 19 (u> m1 z) 1)
    (check 20 (>= 5 5) 1) (check 21 (> z m1) 1) (check 22 (<= m1 m1) 1) (check 23 (/= m1 z) 1))
  (let ((v #xFFFFFFFF)) (check 24 (sext32 v) -1) (check 25 (zext32 -1) #xFFFFFFFF))
  (check 26 #x123456789ABCDEF0 (+ #x123456789ABCDEF0 0))
  (check 27 (- 0 #x8000) -32768)
  (with-buffers ((b 32))
    (st64 b #x1122334455667788)
    (check 28 (ld8 b) #x88) (check 29 (ld16 b 2) #x5566) (check 30 (ld32 b 4) #x11223344)
    (st32 b #x80000000 8) (check 31 (lds32 b 8) -2147483648) (check 32 (ld32 b 8) #x80000000)
    (st16 b #xBEEF 12) (st8 b #x7F 14) (check 33 (ld64 b 8) #x007FBEEF80000000)
    (check 34 (ld64 b) #x1122334455667788)
    (atomic-st32 (+ b 16) #xCAFEBABE) (check 35 (atomic-ld32 (+ b 16)) #xCAFEBABE))
  (let ((s 0) (i 1)) (while (<= i 100) (setq s (+ s i)) (setq i (+ i 1))) (check 36 s 5050))
  (let ((i 0)) (while 1 (when (= i 7) (break)) (setq i (+ i 1))) (check 37 i 7))
  (check 38 (early 6) 60)
  (check 39 (fib 20) 6765)
  (check 40 (eight 1 2 3 4 5 6 7 8) 204)
  (check 41 (cond ((= 1 2) 10) ((= 2 2) 20) (t 30)) 20)
  (check 42 (and 1 2 3) 3) (check 43 (and 1 0 3) 0) (check 44 (or 0 0 9) 9) (check 45 (or 0 0) 0)
  (check 46 (not 0) 1) (check 47 (not 5) 0)
  (check 48 (callptr (addr add3) 1 2 3) 6)
  ;; 11 arguments: 8 in registers, 3 on the stack in 8-byte slots
  (check 49 (ccall c-mach-vm-remap 10 11 12 13 14 15 16 17 18 19 20) 7)
  (let ((r (addr rec)) (k 0) (ok 1))
    (while (< k 11) (when (/= (ld64 (+ r (* 8 k))) (+ 10 k)) (setq ok 0)) (setq k (+ k 1)))
    (check 50 ok 1))
  ;; variadic: the fixed arguments in registers, the rest from [sp]
  (check 51 (ccallv c-openat 3 -2 (addr s-hello) #x601 #o644) 5)
  (check 52 (ld64 (addr rec) 32) #o644)
  (check 53 (sext32 (ld64 (addr rec) 0)) -2)
  (check 54 (ld64 (addr rec) 16) #x601)
  (check 55 (ccall c-strlen (addr s-hello)) 5)
  0)

;;; ------------------------------------------------------- translation cases ----

(defun test-translation ()
  ;; open flags: O_WRONLY|O_CREAT|O_TRUNC; O_RDONLY|O_DIRECTORY|O_CLOEXEC; O_RDWR|O_APPEND|
  ;; O_NONBLOCK|O_EXCL|O_NOFOLLOW|O_NOCTTY
  (check 100 (dx-open-flags #x241) #x601)
  (check 101 (dx-open-flags #x84000) #x1100000)
  (check 102 (dx-open-flags #x10000) #x100000)
  (check 103 (dx-open-flags (logior 2 #x400 #x800 #x80 #x8000 #x100)) (logior 2 8 4 #x800 #x100 #x20000))
  (check 104 (dx-at-flags #x300) #xA0)
  (check 105 (dx-dirfd -100) -2) (check 106 (dx-dirfd #xFFFFFF9C) -2) (check 107 (dx-dirfd 7) 7)
  (check 108 (lx-errno 35) 11) (check 109 (lx-errno 78) 38) (check 110 (lx-errno 2) 2)
  (check 111 (lx-errno 61) 111) (check 112 (lx-errno 54) 104)
  (check 113 (dx-sig 7) 10) (check 114 (dx-sig 10) 30) (check 115 (dx-sig 11) 11) (check 116 (dx-sig 17) 20)
  ;; Linux {SIGBUS 7, SIGUSR1 10, SIGCHLD 17} -> Darwin {10, 30, 20}
  (check 117 (dx-sigset (logior (shl 1 6) (shl 1 9) (shl 1 16))) (logior (shl 1 9) (shl 1 29) (shl 1 19)))
  (check 118 (lx-sigset (dx-sigset #x7FFFFFFF)) #x7FFFFFFF)
  (check 119 (dx-clock 1) 6) (check 120 (dx-clock 2) 12) (check 121 (dx-clock 3) 16)
  (check 122 (dx-sock-opt 1 2) 4) (check 123 (dx-sock-opt 1 20) #x1006) (check 124 (dx-sock-opt 6 1) 1)
  (check 125 (dx-sock-level 1) #xFFFF)
  ;; struct stat
  (with-buffers ((ds 144) (ls 128))
    (impl-c-memset ds 0 144)
    (st32 ds #x1000003 0) (st16 ds #o100644 4) (st16 ds 2 6) (st64 ds 987654321 8)
    (st32 ds 501 16) (st32 ds 20 20) (st64 ds 4096 96) (st64 ds 8 104) (st32 ds 4096 112)
    (st64 ds 1700000000 32) (st64 ds 111 40) (st64 ds 1700000001 48) (st64 ds 222 56)
    (st64 ds 1700000002 64) (st64 ds 333 72)
    (to-lx-stat ds ls)
    (check 130 (ld64 ls 0) #x1000003) (check 131 (ld64 ls 8) 987654321) (check 132 (ld32 ls 16) #o100644)
    (check 133 (ld32 ls 20) 2) (check 134 (ld32 ls 24) 501) (check 135 (ld32 ls 28) 20)
    (check 136 (ld64 ls 48) 4096) (check 137 (ld32 ls 56) 4096) (check 138 (ld64 ls 64) 8)
    (check 139 (ld64 ls 72) 1700000000) (check 140 (ld64 ls 80) 111) (check 141 (ld64 ls 88) 1700000001)
    (check 142 (ld64 ls 96) 222) (check 143 (ld64 ls 104) 1700000002) (check 144 (ld64 ls 112) 333))
  0)

(defun make-dirent (i ino type name)
  (let ((e (+ (addr rec) (* 64 i))))
    (st64 e ino 0) (st8 e type 20)
    (impl-c-memcpy (+ e 21) name (+ (impl-c-strlen name) 1))
    e))
(defstring dn-a "a")
(defstring dn-long "a-much-longer-name")
(defstring dn-dot "..")
(defun test-getdents ()
  (impl-c-memset (addr rec) 0 512) (impl-c-memset (addr rec2) 0 512)
  (make-dirent 0 11 4 (addr dn-dot)) (make-dirent 1 12 8 (addr dn-a)) (make-dirent 2 13 8 (addr dn-long))
  (st64 (addr rec2) 3 232)                         ; three entries
  (with-buffers ((b 256))
    ;; 56 bytes hold ".." (rec 32) and "a" (rec 32)? -- 32 + 32 = 64 > 56: only ".."
    (check 200 (dx-getdents64 5 b 56) 32)
    (check 201 (ld64 (addr rec2) 208) 105)          ; fdopendir on a dup of fd 5
    (check 202 (ld64 b 0) 11) (check 203 (ld64 b 8) 32) (check 204 (ld16 b 16) 32) (check 205 (ld8 b 18) 4)
    (check 206 (ld8 b 19) 46) (check 207 (ld8 b 20) 46) (check 208 (ld8 b 21) 0)
    ;; the rest: "a" (32) and "a-much-longer-name" (24+18+1 -> 48); the pending one comes first
    (check 210 (dx-getdents64 5 b 256) 80)
    (check 211 (ld64 b 0) 12) (check 212 (ld64 b 8) 32) (check 213 (ld8 b 19) 97)
    (check 214 (ld64 b 32) 13) (check 215 (ld64 b 40) 80) (check 216 (ld16 b 48) 48)
    (check 217 (ld8 b (+ 32 19 17)) 101) (check 218 (ld8 b (+ 32 19 18)) 0)
    (check 219 (dx-getdents64 5 b 256) 0)
    (check 220 (dx-getdents64 2000 b 256) -9)
    (forget-dir 5)
    (check 221 (ld64 (addr rec2) 216) #x77770000)   ; closedir
    (check 222 (ld64 (+ (addr dirs) 40)) 0))
  0)

(defun test-mmap ()
  (impl-c-memset (addr rec) 0 512) (impl-c-memset (addr rec2) 0 512)
  (st64 (addr n-jit) 0)
  ;; PROT_READ|WRITE|EXEC, MAP_PRIVATE|MAP_ANONYMOUS: MAP_JIT, a JIT region, exec mode
  (st64 (addr rec) #x7100000000 496)
  (check 300 (dx-mmap #x7100000000 #x100000 7 #x22 -1 0) #x7100000000)
  (check 301 (ld64 (addr rec2) 24) #x1802)
  (check 302 (in-jit #x7100000010) 1) (check 303 (in-jit #x7100100000) 0)
  (check 304 (ld64 (addr rec) 472) 1)
  ;; MAP_FIXED, RW: MAP_FIXED|MAP_ANON|MAP_PRIVATE
  (st64 (addr rec2) 0 504)
  (st64 (addr rec) #x7080000000 496)
  (check 305 (dx-mmap #x7080000000 #x4000 3 #x32 -1 0) #x7080000000)
  (check 306 (ld64 (addr rec2) 24) #x1012)
  ;; MAP_FIXED_NOREPLACE that lands elsewhere: unmapped again, EEXIST
  (st64 (addr rec2) 0 504)
  (st64 (addr rec) #x9000 496)
  (check 307 (dx-mmap #x7090000000 #x4000 3 #x100022 -1 0) -17)
  (check 308 (ld64 (addr rec2) 24) #x1002)          ; not MAP_FIXED: the hint only
  (check 309 (ld64 (addr rec) 480) #x9000)
  ;; a JIT region of 64 MB or more keeps its top 16 MB RW (four calls: map, munmap, map RW)
  (st64 (addr rec2) 0 504)
  (st64 (addr rec) #x7100000000 496)
  (check 310 (dx-mmap #x7100000000 #x8000000 7 #x22 -1 0) #x7100000000)
  (check 311 (ld64 (addr rec2) (+ 48 24)) #x1012)   ; the second mmap: RW, FIXED
  (check 312 (ld64 (addr rec2) (+ 48 0)) #x7107000000)
  (check 313 (in-jit #x7106FFFFFF) 1) (check 314 (in-jit #x7107000000) 0)
  (check 315 (ld64 (addr data-slice-lo)) #x7107000000)
  0)

(defun test-signals ()
  (impl-c-memset (addr rec) 0 512)
  (with-buffers ((nw 32) (old 32))
    ;; SIGINT (2), SA_SIGINFO|SA_RESTART, mask {SIGUSR1 (Linux 10)}
    (st64 nw #x40001000) (st64 nw (logior 4 #x10000000) 8) (st64 nw 0 16) (st64 nw (shl 1 9) 24)
    (check 400 (dx-rt-sigaction 2 nw old) 0)
    (check 401 (ld64 (addr rec) 0) 2) (check 402 (ld64 (addr rec) 8) #x40001000)
    (check 403 (ld64 (addr rec) 16) (shl 1 29)) (check 404 (ld64 (addr rec) 24) #x42)
    ;; the old action back in Linux form: SIGINFO|RESTART|ONSTACK, mask {SIGHUP 1, SIGINT 2}
    (check 405 (ld64 old 0) #x1234) (check 406 (ld64 old 8) (logior 4 #x10000000 #x08000000))
    (check 407 (ld64 old 24) 3)
    ;; SIGSEGV is the shim's own: kept, not installed
    (st64 (addr rec) 0 0)
    (check 408 (dx-rt-sigaction 11 nw 0) 0)
    (check 409 (ld64 (addr rec) 0) 0)
    (check 410 (ld64 (addr image-fault-handler)) #x40001000))
  ;; the Linux fault frames from a Darwin siginfo / ucontext
  (with-buffers ((si 128) (uc 64) (mc 816) (lsi 128) (luc 464))
    (impl-c-memset si 0 128) (impl-c-memset mc 0 816)
    (st32 si 11 0) (st32 si 2 8) (st64 si #xDEAD0008 24)
    (st64 uc mc 48)
    (let ((i 0)) (while (< i 29) (st64 (+ mc 16 (* 8 i)) (+ #x100 i)) (setq i (+ i 1))))
    (st64 mc #x7000 248) (st64 mc #x7008 256) (st64 mc #x7010 264) (st64 mc #x7018 272) (st32 mc #x60000000 280)
    (lx-fault-frames 11 si uc lsi luc)
    (check 420 (ld32 lsi 0) 11) (check 421 (ld32 lsi 8) 2) (check 422 (ld64 lsi 16) #xDEAD0008)
    (check 423 (ld64 luc 176) #xDEAD0008) (check 424 (ld64 luc 184) #x100) (check 425 (ld64 luc (+ 184 (* 8 28))) #x11C)
    (check 426 (ld64 luc (+ 184 (* 8 29))) #x7000) (check 427 (ld64 luc (+ 184 (* 8 30))) #x7008)
    (check 428 (ld64 luc 432) #x7010) (check 429 (ld64 luc 440) #x7018) (check 430 (ld64 luc 448) #x60000000)
    ;; a write fault inside a JIT region flips to write mode and returns
    (st64 (addr n-jit) 0) (note-jit #x7100000000 #x7101000000)
    (st64 si #x7100000040 24)
    (st64 (addr rec) 9 472)
    (check 431 (on-fault 11 si uc) 0)
    (check 432 (ld64 (addr rec) 472) 0)
    ;; a fetch fault there (pc == address) flips back to exec mode
    (st64 mc #x7100000040 272)
    (check 433 (on-fault 10 si uc) 0)
    (check 434 (ld64 (addr rec) 472) 1))
  0)

(defun test-futex ()
  (impl-c-memset (addr rec) 0 512)
  (with-buffers ((w 8) (ts 16))
    (st32 w 5)
    (check 500 (dx-futex w 128 4 0) -11)                ; value differs: EAGAIN
    (st64 ts 2) (st64 ts 500 8)
    (check 501 (dx-futex w 128 5 ts) -110)              ; private WAIT, timed out
    (check 502 (ld64 (addr rec) 0) w) (check 503 (ld64 (addr rec) 8) 5) (check 504 (ld64 (addr rec) 16) 4)
    (check 505 (ld64 (addr rec) 32) 32) (check 506 (ld64 (addr rec) 40) 2000000500)
    (st64 ts 0) (st64 ts 0 8)
    (check 507 (dx-futex w 0 5 ts) -110)                ; zero timeout
    (check 508 (dx-futex w 1 1 0) 1) (check 509 (ld64 (addr rec) 48) 1)
    (check 510 (dx-futex w 1 #x7FFFFFFF 0) 0) (check 511 (ld64 (addr rec) 48) 2)
    (check 512 (dx-futex w 1 0 0) 0)
    (check 513 (dx-futex w 9 0 0) -38))
  0)

(defun test-clone ()
  (impl-c-memset (addr rec) 0 512) (impl-c-memset (addr rec2) 0 512)
  (st64 (addr rec2) (+ (addr area) 512) 496)          ; calloc's arena
  (set-delta -4096)
  (with-buffers ((fr 608) (pt 8))
    (let ((i 0)) (while (< i 76) (st64 (+ fr (* 8 i)) (+ #x5000 i)) (setq i (+ i 1))))
    (st32 pt 0)
    (check 600 (dx-clone #x3D0F00 #x7700000000 pt #x7800000000 fr) 5)
    (check 601 (ld32 pt) 5)                             ; PARENT_SETTID
    (check 602 (ld64 (addr rec) 72) (addr clone-child))
    (check 603 (ld64 (addr rec) 56) 2) (check 604 (ld64 (addr rec) 64) #x40000)
    (check 605 (ld64 (addr rec) 88) #x5151)
    (let ((cs (ld64 (addr rec) 80)))
      (check 610 (ld64 cs 0) 0)
      (check 611 (ld64 cs 8) #x5000) (check 612 (ld64 cs (* 8 15)) #x500E)
      (check 613 (ld64 cs (* 8 17)) #x500F)
      (check 614 (ld64 cs (* 8 19)) (+ #x5000 66)) (check 615 (ld64 cs (* 8 28)) (+ #x5000 75))
      (check 616 (ld64 cs (* 8 29)) #x5010) (check 617 (ld64 cs (* 8 30)) #x5011)
      (check 618 (ld64 cs 248) (- #x7700000000 16)) (check 619 (ld64 cs 256) #x5011)
      (check 620 (ld64 cs 264) -4096) (check 621 (ld64 cs 272) #x7800000000)))
  (check 622 (dx-clone 0 0 0 0 0) -38)
  0)

;;; SYSCALL-STUB: set every register to a pattern, call it with Linux number 1000 (set the
;;; thread delta: no output, one pthread_setspecific), and dump what comes back.
(defasm stub-probe
  (:stp x29 x30 sp -16 :pre) (:mov x29 sp)
  (:li x1 #x1111) (:li x2 #x2222) (:li x3 #x3333) (:li x4 #x4444) (:li x5 #x5555)
  (:li x6 #x6666) (:li x7 #x7777) (:li x9 #x9999) (:li x10 #xAAAA) (:li x11 #xBBBB)
  (:li x12 #xCCCC) (:li x13 #xDDDD) (:li x14 #xEEEE) (:li x15 #xFFFF) (:li x17 #x1717)
  (:li x19 #x1919) (:li x20 #x2020) (:li x21 #x2121) (:li x22 #x2222) (:li x23 #x2323)
  (:li x24 #x2424) (:li x25 #x2525) (:li x26 #x2626) (:li x27 #x2727) (:li x28 #x2828)
  (:li x9 #xA000) (:word #x9E670120)               ; fmov d0, x9
  (:li x9 #xA007) (:word #x9E670127)               ; fmov d7, x9
  (:li x9 #xA016) (:word #x9E670130)               ; fmov d16, x9
  (:li x9 #xA031) (:word #x9E67013F)               ; fmov d31, x9
  (:li x9 #x9999)
  (:li x8 1000) (:li x0 #x1234)
  (:bl syscall-stub)
  (:adr-addr x16 area)
  (:str x0 x16 0) (:str x1 x16 8) (:str x2 x16 16) (:str x3 x16 24) (:str x4 x16 32)
  (:str x5 x16 40) (:str x6 x16 48) (:str x7 x16 56) (:str x8 x16 64) (:str x9 x16 72)
  (:str x10 x16 80) (:str x11 x16 88) (:str x12 x16 96) (:str x13 x16 104) (:str x14 x16 112)
  (:str x15 x16 120) (:str x17 x16 136) (:str x19 x16 152) (:str x20 x16 160) (:str x21 x16 168)
  (:str x22 x16 176) (:str x23 x16 184) (:str x24 x16 192) (:str x25 x16 200) (:str x26 x16 208)
  (:str x27 x16 216) (:str x28 x16 224) (:str x29 x16 232)
  (:word #x9E660009) (:str x9 x16 256)            ; fmov x9, d0
  (:word #x9E6600E9) (:str x9 x16 264)            ; fmov x9, d7
  (:word #x9E660209) (:str x9 x16 272)            ; fmov x9, d16
  (:word #x9E6603E9) (:str x9 x16 280)            ; fmov x9, d31
  (:mov x9 sp) (:str x9 x16 288)
  (:mov x9 x29) (:str x9 x16 296)
  (:ldp x29 x30 sp 16 :post) (:ret))

(defun test-stub ()
  (st64 (addr strace-on) 0)
  (st64 (addr delta-key) 300)
  (st64 (+ (addr fake-tsd) 2400) 0)
  (stub-probe)
  (let ((a (addr area)))
    (check 700 (ld64 a 0) 0)                              ; set-delta answers 0
    (check 701 (ld64 (+ (addr fake-tsd) 2400)) #x1234)     ; the delta went to key 300
    (check 702 (ld64 a 8) #x1111) (check 703 (ld64 a 16) #x2222) (check 704 (ld64 a 24) #x3333)
    (check 705 (ld64 a 32) #x4444) (check 706 (ld64 a 40) #x5555) (check 707 (ld64 a 48) #x6666)
    (check 708 (ld64 a 56) #x7777) (check 709 (ld64 a 64) 1000) (check 710 (ld64 a 72) #x9999)
    (check 711 (ld64 a 80) #xAAAA) (check 712 (ld64 a 88) #xBBBB) (check 713 (ld64 a 96) #xCCCC)
    (check 714 (ld64 a 104) #xDDDD) (check 715 (ld64 a 112) #xEEEE) (check 716 (ld64 a 120) #xFFFF)
    (check 717 (ld64 a 136) #x1717) (check 718 (ld64 a 152) #x1919) (check 719 (ld64 a 160) #x2020)
    (check 720 (ld64 a 168) #x2121) (check 721 (ld64 a 176) #x2222) (check 722 (ld64 a 184) #x2323)
    (check 723 (ld64 a 192) #x2424) (check 724 (ld64 a 200) #x2525) (check 725 (ld64 a 208) #x2626)
    (check 726 (ld64 a 216) #x2727) (check 727 (ld64 a 224) #x2828)
    (check 728 (ld64 a 256) #xA000) (check 729 (ld64 a 264) #xA007) (check 730 (ld64 a 272) #xA016)
    (check 731 (ld64 a 280) #xA031)
    (check 732 (ld64 a 296) (+ (ld64 a 288) 0)))          ; x29 back where the probe set it
  0)

;;; MODUS-RESUME: jump into a context, land on RESUME-TARGET, dump the registers, and come
;;; back to the caller through SAVED.
(defasm call-resume                                       ; x0 = the context
  (:stp x29 x30 sp -16 :pre) (:mov x29 sp)
  (:adr-addr x16 saved) (:mov x9 sp) (:str x9 x16 0)
  (:b modus-resume))
(defasm resume-target
  (:adr-addr x16 area)
  (:str x0 x16 0) (:str x1 x16 8) (:str x15 x16 120) (:str x19 x16 152) (:str x28 x16 224)
  (:str x29 x16 232) (:str x30 x16 240) (:mov x9 sp) (:str x9 x16 248)
  (:adr-addr x16 saved) (:ldr x9 x16 0) (:mov sp x9)
  (:ldp x29 x30 sp 16 :post) (:ret))
(defun test-resume ()
  (with-buffers ((ctx 272))
    (let ((i 0)) (while (< i 31) (st64 (+ ctx (* 8 i)) (+ #x900 i)) (setq i (+ i 1))))
    (st64 ctx (+ (addr tstack) 4096) 248)
    (st64 ctx (addr resume-target) 256)
    (call-resume ctx)
    (let ((a (addr area)))
      (check 800 (ld64 a 0) #x900) (check 801 (ld64 a 8) #x901) (check 802 (ld64 a 120) #x90F)
      (check 803 (ld64 a 152) #x913) (check 804 (ld64 a 224) #x91C) (check 805 (ld64 a 232) #x91D)
      (check 806 (ld64 a 240) #x91E) (check 807 (ld64 a 248) (+ (addr tstack) 4096))))
  0)

(defun test-main (sp)
  (test-compiler)
  (test-translation)
  (test-getdents)
  (test-mmap)
  (test-signals)
  (test-futex)
  (test-clone)
  (test-stub)
  (test-resume)
  (say (addr s-done)) (say-dec (ld64 (addr t-count))) (say (addr s-of))
  (say-dec (ld64 (addr t-fails))) (say (addr s-failed))
  (if (ld64 (addr t-fails)) 1 0))

(defasm test-start
  (:mov x0 sp)
  (:bl test-main)
  (:bl linux-exit))
