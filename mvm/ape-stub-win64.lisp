;;;; mvm/ape-stub-win64.lisp -- the Windows half of modus.com: a launcher.
;;;;
;;;; modus.com (scripts/make-ape.lisp) carries the Windows CLI gzip-compressed, as the
;;;; segment windows-x64.exe.gz.  Windows runs the file's own PE, which is this program:
;;;; it finds the segment through the file's trailer, inflates it, checks its CRC-32 and
;;;; length, caches it as %LOCALAPPDATA%\modus\modus-CRC-LENGTH.exe (once), and runs it
;;;; with this process's command line and MODUS_APE set to the .com, so the CLI finds
;;;; its segments; its exit code is this process's.  The cached file is the release's
;;;; modus-windows-x64.exe byte for byte.
;;;;
;;;; mvm/build-ape-stub.lisp builds it for :WIN64 with no CL runtime at all, so it is
;;;; written in what the compiler gives with nothing behind it: raw memory (MEM-REF),
;;;; fixnum arithmetic, plain LOOP / DOTIMES / LET, the boot stub's Linux-syscall shim
;;;; (mmap, read, write, exit_group) and kernel32 through syscall #x3000 (LoadLibraryA +
;;;; GetProcAddress, as net/win32-self.lisp does).  No strings, no lists, no conses: the
;;;; byte strings it needs are written by %S-* functions the build script generates.
;;;;
;;;; Arithmetic must never leave the fixnum fast path: its slow path is a call into the
;;;; runtime (GENERIC-ADD, BIGNUM-ASH, ...), which is not there, so the call goes to offset 0
;;;; -- %S-KERNEL32, the first function -- and faults writing address 0.  Two ways in, both
;;;; met: a constant left shift of 31 or more is always BIGNUM-ASH (G64 multiplies instead),
;;;; and a Win32 BOOL or DWORD comes back with junk in the top of RAX (CALL zero-extends).
;;;; To find which helper a crash reached, build with MODUS_APE_STUB_EXTRA naming a file
;;;; that defines each unresolved name (the build log lists them) to print a letter and exit.
;;;;
;;;; The scratch block (64 KB, mmap'd):
;;;;   +0x0000  byte strings          +0x1000  #x3000 arguments
;;;;   +0x1100  file size / exit code +0x1200  the trailer (32 bytes)
;;;;   +0x1300  inflate state         +0x2000  this file's path     +0x2400  cache dir
;;;;   +0x2800  cached image path     +0x2C00  its temporary name
;;;;   +0x3000  STARTUPINFOA          +0x3100  PROCESS_INFORMATION
;;;;   +0x4000  Huffman tables        +0x6000  CRC-32 table

;;; --- words -------------------------------------------------------------------

(defun p64 (a v)
  (setf (mem-ref a :u32) (logand v #xFFFFFFFF))
  (setf (mem-ref (+ a 4) :u32) (logand (ash v -32) #xFFFFFFFF))
  v)
;; a multiply, not (ash hi 32): a constant left shift of 31 or more is a call to BIGNUM-ASH
(defun g64 (a) (+ (mem-ref a :u32) (* (mem-ref (+ a 4) :u32) 4294967296)))
(defun g16 (a) (+ (mem-ref a :u8) (ash (mem-ref (+ a 1) :u8) 8)))
(defun p16 (a v) (setf (mem-ref a :u8) (logand v 255)) (setf (mem-ref (+ a 1) :u8) (logand (ash v -8) 255)) v)
(defun zero (a n) (dotimes (i n) (setf (mem-ref (+ a i) :u8) 0)) a)
(defun cstr-len (a) (let ((n 0)) (loop (when (= (mem-ref (+ a n) :u8) 0) (return n)) (setq n (+ n 1)))))
(defun cstr-copy (dst src)
  "Copy the C string at SRC to DST; the address of DST's terminating 0."
  (let ((i 0))
    (loop (let ((b (mem-ref (+ src i) :u8)))
            (setf (mem-ref (+ dst i) :u8) b)
            (when (= b 0) (return (+ dst i)))
            (setq i (+ i 1))))))
;;; No variable-count ASH, FLOOR or MIN here: with no runtime behind them they would be
;;; calls to nothing.  Shifts are by constants; division is repeated subtraction.
(defun put-hex8 (a v)
  "The low 32 bits of V as 8 hex digits at A; the address after them."
  (let ((x (logand v #xFFFFFFFF)))
    (dotimes (i 8)
      (let ((d (logand (ash x -28) 15)))
        (setf (mem-ref (+ a i) :u8) (if (< d 10) (+ 48 d) (+ 87 d)))
        (setq x (logand (ash x 4) #xFFFFFFFF)))))
  (+ a 8))
(defun pow10 (k) (let ((p 1)) (dotimes (i k) (setq p (* p 10))) p))
(defun put-dec (a v)
  "V (below 10^12) in decimal at A, no leading zeros; the address after it."
  (let ((p a) (x v) (started nil) (k 11))
    (loop
      (when (< k 0) (return nil))
      (let ((pw (pow10 k)) (d 0))
        (loop (when (< x pw) (return nil)) (setq x (- x pw)) (setq d (+ d 1)))
        (when (or started (> d 0) (= k 0))
          (setf (mem-ref p :u8) (+ 48 d)) (setq p (+ p 1)) (setq started t)))
      (setq k (- k 1)))
    p))

;;; --- the platform -------------------------------------------------------------

(defun mmap (n) (syscall6 9 0 n 3 34 -1 0))
(defun die (s code)
  "Write the C string at S to stderr and exit CODE."
  (syscall3 1 2 s (cstr-len s))
  (syscall3 231 code 0 0))

(defun k32-fn (s name)
  "kernel32's NAME (a C string), or die."
  ;; the name first, into a local: not computed as an argument of SYSCALL6
  (let* ((k (%s-kernel32 (+ s #x800)))
         (lib (syscall6 +w32-loadlibrarya+ k 0 0 0 0 0)))
    (let ((fn (if (= lib 0) 0 (syscall6 +w32-getprocaddress+ lib name 0 0 0 0))))
      (when (= fn 0) (die (%s-no-function (+ s #x900)) 1))
      fn)))

;;; A Win32 function that returns a BOOL or a DWORD sets EAX only, and leaves the top of RAX
;;; as it was (under Wine, pointer bits).  The shim returns RAX as it is, and in an image with
;;; no bignums a value that large cannot become a Lisp integer: it is a call to BIGNUM-ASH,
;;; which is not there.  So such calls go through a thunk in the RWX arena that calls the
;;; function and zero-extends EAX (CALL); CALL64 is for functions that return a handle or a
;;; pointer.  The thunk forwards ten arguments: four in registers, six on the stack.
(defconstant +thunk+ #x3000000000)                  ; the JIT arena: RWX, committed at startup
(defconstant +thunk-fn+ (+ #x3000000000 #x100))     ; the function the thunk calls
(defun pb (p b) (setf (mem-ref p :u8) b) (+ p 1))
(defun put-thunk ()
  "Write the thunk at +THUNK+ (no lists: this image has no runtime to cons with)."
  (let ((p (pb (pb (pb (pb +thunk+ #x48) #x83) #xEC) #x58)))      ; sub rsp,0x58
    (dotimes (k 6)             ; argument 5+k: mov rax,[rsp+0x80+8k]; mov [rsp+0x20+8k],rax
      (setq p (pb (pb (pb (pb p #x48) #x8B) #x84) #x24))
      (setq p (pb (pb (pb (pb p (+ #x80 (* 8 k))) 0) 0) 0))
      (setq p (pb (pb (pb (pb (pb p #x48) #x89) #x44) #x24) (+ #x20 (* 8 k)))))
    (setq p (pb (pb p #xFF) #x15))                               ; call [rip+rel32]
    (setq p (pb (pb (pb (pb p (- +thunk-fn+ (+ p 4))) 0) 0) 0))  ; rel < 256
    (setq p (pb (pb p #x89) #xC0))                               ; mov eax,eax
    (pb (pb (pb (pb (pb p #x48) #x83) #xC4) #x58) #xC3)))        ; add rsp,0x58; ret
(defun call (s fn n)
  "Call FN with N arguments from S+0x1000; its result as an unsigned 32-bit value."
  (p64 +thunk-fn+ fn)
  (syscall3 #x3000 +thunk+ (+ s #x1000) n))
(defun call64 (s fn n) (syscall3 #x3000 fn (+ s #x1000) n))
(defun arg (s i v) (p64 (+ s #x1000 (* 8 i)) v))

(defun read-at (s pos buf n)
  "Read N bytes at POS of the handle at S+0x1110 into BUF, or die."
  (let ((h (g64 (+ s #x1110))))
  (arg s 0 h) (arg s 1 pos) (arg s 2 0) (arg s 3 0)
  (call s (k32-fn s (%s-setfilepointerex s)) 4)
  (let ((done 0) (rd (k32-fn s (%s-readfile s))))
    (loop
      (when (>= done n) (return done))
      (let ((want (if (< (- n done) #x40000000) (- n done) #x40000000)))
        (arg s 0 h) (arg s 1 (+ buf done)) (arg s 2 want) (arg s 3 (+ s #x1108)) (arg s 4 0)
        (p64 (+ s #x1108) 0)
        (call s rd 5)
        (let ((got (logand (g64 (+ s #x1108)) #xFFFFFFFF)))
          (when (= got 0) (die (%s-short-read (+ s #x900)) 1))
          (setq done (+ done got))))))))

;;; --- CRC-32 --------------------------------------------------------------------

(defun crc-table (tab)
  (dotimes (n 256)
    (let ((c n))
      (dotimes (k 8) (setq c (if (= (logand c 1) 1) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
      (setf (mem-ref (+ tab (* 4 n)) :u32) c))))
(defun crc32 (tab buf n)
  (let ((c #xFFFFFFFF))
    (dotimes (i n)
      (setq c (logxor (mem-ref (+ tab (* 4 (logand (logxor c (mem-ref (+ buf i) :u8)) 255))) :u32)
                      (ash c -8))))
    (logxor c #xFFFFFFFF)))

;;; --- inflate (RFC 1951), as puff.c does it ---------------------------------------
;;; State at ST: +0 input pointer, +8 input end, +16 bit buffer, +24 bit count,
;;; +32 output pointer, +40 output start, +48 output end.

(defun bits (st need)
  "NEED bits, least significant first, or -1 past the input.  A bit at a time, so every
   shift is by a constant: +16 holds the current byte's unread bits, +24 how many."
  (let ((val (g64 (+ st 16))) (cnt (g64 (+ st 24))) (r 0) (m 1))
    (dotimes (k need)
      (when (= cnt 0)
        (let ((p (g64 st)))
          (when (>= p (g64 (+ st 8))) (return-from bits -1))
          (setq val (mem-ref p :u8))
          (p64 st (+ p 1))
          (setq cnt 8)))
      (when (= (logand val 1) 1) (setq r (+ r m)))
      (setq val (ash val -1))
      (setq cnt (- cnt 1))
      (setq m (ash m 1)))
    (p64 (+ st 16) val)
    (p64 (+ st 24) cnt)
    r))

(defun decode (st cnt sym)
  "One symbol of the canonical code with counts CNT (16 u16) and symbols SYM (u16)."
  (let ((code 0) (first 0) (index 0) (len 1))
    (loop
      (when (> len 15) (return -10))
      (let ((b (bits st 1)))
        (when (< b 0) (return -1))
        (setq code (logior code b)))
      (let ((count (g16 (+ cnt (* 2 len)))))
        (when (< (- code count) first)
          (return (g16 (+ sym (* 2 (+ index (- code first)))))))
        (setq index (+ index count))
        (setq first (ash (+ first count) 1))
        (setq code (ash code 1)))
      (setq len (+ len 1)))))

(defun construct (cnt lengths n offs)
  "Build CNT and its symbols (at CNT+0x40) from N code lengths (u16 at LENGTHS); OFFS is 16
   u16 of scratch."
  (let ((sym (+ cnt #x40)))
  (dotimes (l 16) (p16 (+ cnt (* 2 l)) 0))
  (dotimes (s n) (let ((l (g16 (+ lengths (* 2 s))))) (p16 (+ cnt (* 2 l)) (+ 1 (g16 (+ cnt (* 2 l)))))))
  (p16 (+ offs 2) 0)
  (let ((l 1)) (loop (when (>= l 15) (return nil))
                     (p16 (+ offs (* 2 (+ l 1))) (+ (g16 (+ offs (* 2 l))) (g16 (+ cnt (* 2 l)))))
                     (setq l (+ l 1))))
  (dotimes (s n)
    (let ((l (g16 (+ lengths (* 2 s)))))
      (unless (= l 0)
        (let ((o (g16 (+ offs (* 2 l)))))
          (p16 (+ sym (* 2 o)) s)
          (p16 (+ offs (* 2 l)) (+ o 1))))))))

(defun put-out (st b)
  (let ((p (g64 (+ st 32))))
    (when (>= p (g64 (+ st 48))) (return-from put-out nil))
    (setf (mem-ref p :u8) b)
    (p64 (+ st 32) (+ p 1))
    t))

(defun codes (st h tabs)
  "Decode literal/length and distance codes until end of block: the codes are at H (lengths:
   counts, symbols at +0x40) and H+0x300 (distances).  TABS: the length and distance
   base/extra tables (u16 each, 29/29/30/30 entries)."
  (let ((lc h) (ls (+ h #x40)) (dc (+ h #x300)) (ds (+ h #x340)))
  (loop
    (let ((sym (decode st lc ls)))
      (cond ((< sym 0) (return sym))
            ((< sym 256) (unless (put-out st sym) (return -2)))
            ((= sym 256) (return 0))
            (t (let ((i (- sym 257)))
                 (when (>= i 29) (return -10))
                 (let ((len (+ (g16 (+ tabs (* 2 i))) (bits st (g16 (+ tabs 58 (* 2 i)))))))
                   (let ((d (decode st dc ds)))
                     (when (or (< d 0) (>= d 30)) (return -11))
                     (let* ((dist (+ (g16 (+ tabs 116 (* 2 d))) (bits st (g16 (+ tabs 176 (* 2 d))))))
                            (p (g64 (+ st 32))))
                       (when (> dist (- p (g64 (+ st 40)))) (return -12))
                       (when (> (+ p len) (g64 (+ st 48))) (return -2))
                       (dotimes (k len) (setf (mem-ref (+ p k) :u8) (mem-ref (+ (- p dist) k) :u8)))
                       (p64 (+ st 32) (+ p len))))))))))))

(defun init-tabs (tabs)
  "Length bases 3..258 with extra bits, distance bases 1..24577 with extra bits."
  (let ((base 3) (i 0))
    ;; 8 codes with no extra bits, then 4 each with 1..5 (lengths 3..257), then 258
    (let ((step 1))
      (dotimes (g 6)
        (dotimes (k (if (= g 0) 8 4))
          (p16 (+ tabs (* 2 i)) base)
          (p16 (+ tabs 58 (* 2 i)) g)
          (setq base (+ base step))
          (setq i (+ i 1)))
        (setq step (ash step 1))))
    (p16 (+ tabs (* 2 28)) 258) (p16 (+ tabs 58 (* 2 28)) 0))
  (let ((base 1) (i 0))
    ;; 4 codes with no extra bits, then 2 each with 1..13 (distances 1..32768)
    (let ((step 1))
      (dotimes (g 14)
        (dotimes (k (if (= g 0) 4 2))
          (p16 (+ tabs 116 (* 2 i)) base)
          (p16 (+ tabs 176 (* 2 i)) g)
          (setq base (+ base step))
          (setq i (+ i 1)))
        (setq step (ash step 1))))))

(defun stored (st)
  (p64 (+ st 16) 0) (p64 (+ st 24) 0)          ; to a byte boundary
  (let ((p (g64 st)))
    (when (> (+ p 4) (g64 (+ st 8))) (return-from stored -1))
    (let ((len (g16 p)) (nlen (g16 (+ p 2))))
      (unless (= len (logxor nlen #xFFFF)) (return-from stored -2))
      (setq p (+ p 4))
      (when (> (+ p len) (g64 (+ st 8))) (return-from stored -1))
      (dotimes (k len) (unless (put-out st (mem-ref (+ p k) :u8)) (return-from stored -2)))
      (p64 st (+ p len))
      0)))

(defun fixed (st h tabs)
  (let ((lengths (+ h #x1800)))
    (dotimes (s 288) (p16 (+ lengths (* 2 s)) (cond ((< s 144) 8) ((< s 256) 9) ((< s 280) 7) (t 8))))
    (construct h lengths 288 (+ h #x1700))
    (dotimes (s 30) (p16 (+ lengths (* 2 s)) 5))
    (construct (+ h #x300) lengths 30 (+ h #x1700))
    (codes st h tabs)))

(defun dynamic (st h tabs)
  (let ((nlen (+ 257 (bits st 5))) (ndist (+ 1 (bits st 5))) (ncode (+ 4 (bits st 4)))
        (lengths (+ h #x1800)) (order (+ h #x1600)))
    (when (or (> nlen 286) (> ndist 30)) (return-from dynamic -3))
    ;; the code-length alphabet's order: 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15
    (p16 order 16) (p16 (+ order 2) 17) (p16 (+ order 4) 18) (p16 (+ order 6) 0) (p16 (+ order 8) 8)
    (p16 (+ order 10) 7) (p16 (+ order 12) 9) (p16 (+ order 14) 6) (p16 (+ order 16) 10)
    (p16 (+ order 18) 5) (p16 (+ order 20) 11) (p16 (+ order 22) 4) (p16 (+ order 24) 12)
    (p16 (+ order 26) 3) (p16 (+ order 28) 13) (p16 (+ order 30) 2) (p16 (+ order 32) 14)
    (p16 (+ order 34) 1) (p16 (+ order 36) 15)
    (dotimes (i 19) (p16 (+ lengths (* 2 (g16 (+ order (* 2 i))))) 0))
    (dotimes (i ncode) (p16 (+ lengths (* 2 (g16 (+ order (* 2 i))))) (bits st 3)))
    (construct h lengths 19 (+ h #x1700))
    (let ((i 0))
      (loop
        (when (>= i (+ nlen ndist)) (return nil))
        (let ((sym (decode st h (+ h #x40))))
          (when (< sym 0) (return-from dynamic sym))
          (if (< sym 16)
              (progn (p16 (+ lengths (* 2 i)) sym) (setq i (+ i 1)))
              (let ((len 0) (rep 0))
                (cond ((= sym 16)
                       (when (= i 0) (return-from dynamic -5))
                       (setq len (g16 (+ lengths (* 2 (- i 1)))))
                       (setq rep (+ 3 (bits st 2))))
                      ((= sym 17) (setq rep (+ 3 (bits st 3))))
                      (t (setq rep (+ 11 (bits st 7)))))
                (when (> (+ i rep) (+ nlen ndist)) (return-from dynamic -6))
                (dotimes (k rep) (p16 (+ lengths (* 2 (+ i k))) len))
                (setq i (+ i rep)))))))
    (construct h lengths nlen (+ h #x1700))
    (construct (+ h #x300) (+ lengths (* 2 nlen)) ndist (+ h #x1700))
    (codes st h tabs)))

(defun inflate (st h tabs)
  "Every block; 0, or a negative error."
  (loop
    (let* ((last (bits st 1)) (type (bits st 2))
           (err (cond ((= type 0) (stored st))
                      ((= type 1) (fixed st h tabs))
                      ((= type 2) (dynamic st h tabs))
                      (t -4))))
      (when (< err 0) (return err))
      (when (= last 1) (return 0)))))

;;; --- the launcher ---------------------------------------------------------------
;;; Small steps, each handing what it found to the next through the scratch block:
;;;   +0x1100 file size   +0x1110 handle   +0x1120 segment offset   +0x1128 its length
;;;   +0x1130 the compressed image          +0x1138 CRC-32           +0x1140 length
;;; (one long KERNEL-MAIN holding all of it in locals crashed in this runtime-free image).

(defun find-entry (dir n s)
  "The offset of the number after \"windows-x64.exe.gz\" in the directory text, or -1."
  (let* ((name (%s-segment-name (+ s #xA00))) (m (cstr-len name)) (i 0))
    (loop
      (when (> (+ i m) n) (return -1))
      (let ((k 0))
        (loop (when (>= k m) (return nil))
              (unless (= (mem-ref (+ dir i k) :u8) (mem-ref (+ name k) :u8)) (return nil))
              (setq k (+ k 1)))
        (when (= k m) (return (+ i m))))
      (setq i (+ i 1)))))

(defun parse-num (dir at n)
  "The decimal number at or after AT (skipping quotes and blanks); where it ended goes to
   DIR+N+8, past the text."
  (let ((i at) (v 0))
    (loop (when (>= i n) (return nil))
          (let ((b (mem-ref (+ dir i) :u8))) (when (and (>= b 48) (<= b 57)) (return nil)))
          (setq i (+ i 1)))
    (loop (when (>= i n) (return nil))
          (let ((b (mem-ref (+ dir i) :u8)))
            (unless (and (>= b 48) (<= b 57)) (return nil))
            (setq v (+ (* v 10) (- b 48))))
          (setq i (+ i 1)))
    (p64 (+ dir n 8) i)
    v))

(defun self-path (s)
  "This executable's path, at S+0x2000."
  (let ((self (+ s #x2000)) (f (k32-fn s (%s-getmodulefilenamea s))))
    (arg s 0 0) (arg s 1 self) (arg s 2 1000)
    (let ((n (logand (call s f 3) #xFFFFFFFF)))
      (when (or (= n 0) (>= n 1000)) (die (%s-no-self (+ s #x900)) 1))
      self)))

(defun open-self (s)
  "Open this file for reading: its handle to +0x1110, its size to +0x1100."
  (let ((f (k32-fn s (%s-createfilea s))))
    (arg s 0 (+ s #x2000)) (arg s 1 #x80000000) (arg s 2 1) (arg s 3 0) (arg s 4 3) (arg s 5 0) (arg s 6 0)
    (let ((h (call64 s f 7)))
      (when (or (= h -1) (= h 0)) (die (%s-no-self (+ s #x900)) 1))
      (p64 (+ s #x1110) h)))
  (let ((f (k32-fn s (%s-getfilesizeex s))))
    (arg s 0 (g64 (+ s #x1110))) (arg s 1 (+ s #x1100))
    (call s f 2)))

(defun find-segment (s)
  "The Windows image's offset and length (+0x1120, +0x1128), from the trailer's directory."
  (let ((tr (+ s #x1200)))
    (read-at s (- (g64 (+ s #x1100)) 32) tr 32)
    (unless (and (= (mem-ref tr :u8) 77) (= (mem-ref (+ tr 7) :u8) 69))   ; MODUSAPE
      (die (%s-no-trailer (+ s #x900)) 1))
    (let* ((dir-at (g64 (+ tr 8))) (dir-n (g64 (+ tr 16))) (dir (mmap (+ dir-n 64))))
      (read-at s dir-at dir dir-n)
      (let ((at (find-entry dir dir-n s)))
        (when (< at 0) (die (%s-no-segment (+ s #x900)) 1))
        (p64 (+ s #x1120) (parse-num dir at dir-n))
        (p64 (+ s #x1128) (parse-num dir (g64 (+ dir dir-n 8)) dir-n))))))

(defun read-segment (s)
  "Read the compressed image (+0x1130); its CRC-32 and length from the gzip trailer."
  (let* ((len (g64 (+ s #x1128))) (z (mmap len)))
    (read-at s (g64 (+ s #x1120)) z len)
    (p64 (+ s #x1130) z)
    (p64 (+ s #x1138) (mem-ref (+ z (- len 8)) :u32))
    (p64 (+ s #x1140) (mem-ref (+ z (- len 4)) :u32))
    (let ((f (k32-fn s (%s-closehandle s))))
      (arg s 0 (g64 (+ s #x1110)))
      (call s f 1))))

(defun cache-dir (s)
  "%LOCALAPPDATA%\\modus (else %TEMP%\\modus) at S+0x2400, created; the address of its NUL."
  (let ((dirp (+ s #x2400)) (f (k32-fn s (%s-getenvironmentvariablea s))))
    (arg s 0 (%s-localappdata (+ s #xB00))) (arg s 1 dirp) (arg s 2 900)
    (let ((m (logand (call s f 3) #xFFFFFFFF)))
      (when (or (= m 0) (> m 900))
        (arg s 0 (%s-temp (+ s #xB00))) (arg s 1 dirp) (arg s 2 900)
        (setq m (logand (call s f 3) #xFFFFFFFF))
        (when (or (= m 0) (> m 900)) (die (%s-no-cache (+ s #x900)) 1)))
      (let ((e (cstr-copy (+ dirp m) (%s-modus-dir (+ s #xB00)))) (g (k32-fn s (%s-createdirectorya s))))
        (arg s 0 dirp) (arg s 1 0)
        (call s g 2)
        e))))

(defun cache-path (s)
  "...\\modus\\modus-CRC-LENGTH.exe at S+0x2800; T if it is there already."
  (cache-dir s)
  (let ((e (cstr-copy (+ s #x2800) (+ s #x2400))))
    (setq e (cstr-copy e (%s-modus-dash (+ s #xB00))))
    (setq e (put-hex8 e (g64 (+ s #x1138))))
    (setf (mem-ref e :u8) 45)
    (setq e (put-dec (+ e 1) (g64 (+ s #x1140))))
    (cstr-copy e (%s-dot-exe (+ s #xB00))))
  (let ((f (k32-fn s (%s-getfileattributesa s))))
    (arg s 0 (+ s #x2800))
    (not (= (logand (call s f 1) #xFFFFFFFF) #xFFFFFFFF))))

(defun inflate-image (s)
  "The image, inflated and checked: its address."
  (let* ((isize (g64 (+ s #x1140))) (z (g64 (+ s #x1130))) (len (g64 (+ s #x1128)))
         (out (mmap (+ isize 4096))) (st (+ s #x1300)) (tabs (+ s #x5C00)) (ct (+ s #x6000)))
    (p64 st (+ z 10)) (p64 (+ st 8) (- (+ z len) 8)) (p64 (+ st 16) 0) (p64 (+ st 24) 0)
    (p64 (+ st 32) out) (p64 (+ st 40) out) (p64 (+ st 48) (+ out isize))
    (init-tabs tabs)
    (unless (= (inflate st (+ s #x4000) tabs) 0) (die (%s-bad-image (+ s #x900)) 1))
    (unless (= (- (g64 (+ st 32)) out) isize) (die (%s-bad-image (+ s #x900)) 1))
    (crc-table ct)
    (unless (= (crc32 ct out isize) (g64 (+ s #x1138))) (die (%s-bad-image (+ s #x900)) 1))
    out))

(defun write-cache (s out)
  "OUT (the image) to a temporary file beside the cache path, then renamed into place."
  (let ((isize (g64 (+ s #x1140))) (tmp (+ s #x2C00)))
    (let ((e (cstr-copy tmp (+ s #x2800))))
      (setf (mem-ref e :u8) 46)
      (cstr-copy (put-dec (+ e 1) (syscall3 39 0 0 0)) (%s-dot-tmp (+ s #xB00))))
    (let ((f (k32-fn s (%s-createfilea s))))
      (arg s 0 tmp) (arg s 1 #x40000000) (arg s 2 0) (arg s 3 0) (arg s 4 2) (arg s 5 #x80) (arg s 6 0)
      (p64 (+ s #x1110) (call64 s f 7)))
    (let ((w (g64 (+ s #x1110))) (done 0) (wf (k32-fn s (%s-writefile s))))
      (when (or (= w -1) (= w 0)) (die (%s-no-cache (+ s #x900)) 1))
      (loop
        (when (>= done isize) (return nil))
        (arg s 0 w) (arg s 1 (+ out done))
        (arg s 2 (if (< (- isize done) #x40000000) (- isize done) #x40000000))
        (arg s 3 (+ s #x1108)) (arg s 4 0) (p64 (+ s #x1108) 0)
        (call s wf 5)
        (let ((got (logand (g64 (+ s #x1108)) #xFFFFFFFF)))
          (when (= got 0) (die (%s-no-cache (+ s #x900)) 1))
          (setq done (+ done got))))
      (let ((c (k32-fn s (%s-closehandle s)))) (arg s 0 w) (call s c 1)))
    (let ((m (k32-fn s (%s-movefileexa s))))
      (arg s 0 tmp) (arg s 1 (+ s #x2800)) (arg s 2 1)        ; MOVEFILE_REPLACE_EXISTING
      (call s m 3))))

(defun run-cached (s)
  "Run the cached image with this command line and MODUS_APE = this file; its exit code."
  (let ((f (k32-fn s (%s-setenvironmentvariablea s))))
    (arg s 0 (%s-modus-ape (+ s #xB00))) (arg s 1 (+ s #x2000))
    (call s f 2))
  (let ((cmd (call64 s (k32-fn s (%s-getcommandlinea s)) 0))
        (si (zero (+ s #x3000) 104)) (pi (zero (+ s #x3100) 24))
        (f (k32-fn s (%s-createprocessa s))))
    (setf (mem-ref si :u32) 104)
    (arg s 0 (+ s #x2800)) (arg s 1 cmd) (arg s 2 0) (arg s 3 0) (arg s 4 1) (arg s 5 0)
    (arg s 6 0) (arg s 7 0) (arg s 8 si) (arg s 9 pi)
    (when (= (logand (call s f 10) #xFFFFFFFF) 0) (die (%s-no-run (+ s #x900)) 1))
    (let ((w (k32-fn s (%s-waitforsingleobject s))) (x (k32-fn s (%s-getexitcodeprocess s))))
      (arg s 0 (g64 pi)) (arg s 1 #xFFFFFFFF)
      (call s w 2)
      (p64 (+ s #x1100) 1)
      (arg s 0 (g64 pi)) (arg s 1 (+ s #x1100))
      (call s x 2)
      (logand (g64 (+ s #x1100)) #xFFFFFFFF))))

(defun kernel-main ()
  (let ((s (mmap #x10000)))
    (zero s #x10000)
    (put-thunk)
    (self-path s)
    (open-self s)
    (find-segment s)
    (read-segment s)
    (unless (cache-path s)
      (write-cache s (inflate-image s)))
    (syscall3 231 (run-cached s) 0 0)))
