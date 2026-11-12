;;;; build-68k-cli.lisp -- the hosted Modus CLI for Linux/m68k.
;;;;
;;;; A :linux-68k ELF (68020+ with a 6888x/68040 FPU) run under qemu-m68k-static.
;;;; Same SBCL-faithful toplevel as ./modus (x64) and the other CLI ports.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-68k-cli.lisp
;;;;   -> /tmp/modus-68k-cli   (override with MODUS_CLI_OUT)
;;;;
;;;; The FIRST BIG-ENDIAN CL image.  A thin tail over build-cli-common.lisp, cut
;;;; from build-arm32-cli.lisp: Linux/m68k uses the legacy (i386) syscall table
;;;; for most of what this slot touches, but not all -- every number here was
;;;; MEASURED with qemu-m68k -strace, because two guesses were wrong:
;;;;   exit_group 247 (i386 252)   clock_gettime 260 (i386 265; 263 is statfs64)
;;;;   getdents64 220   stat64 195   fstat64 197   rt_sigaction 174
;;;; struct stat64 (big-endian): st_size is 64 bits at +40, so its low word is
;;;; at +44; st_atime +60, st_mtime +68 (measured on a file with known values).
;;;; The heap is sized at startup (--dynamic-space-size), argv is staged at
;;;; #x10009000 and NIL's page is mapped at #xDEAD0000 by boot-linux-68k.lisp.
;;;;
;;;; STATUS: bring-up.  The build prints the translator's census of opcodes and
;;;; service traps it has no arm for.
(defvar *cli-arch* :68k)

;; MODUS_68K_SOFT_FLOAT=1: no FPU instructions at all, for the 68LC040 and the
;; 68EC0x0 -- floats go through mvm/softfloat.lisp (see build-cli-common).
(defvar *cli-soft-float*
  (let ((v (sb-ext:posix-getenv "MODUS_68K_SOFT_FLOAT")))
    (and v (plusp (length v)) (not (string= v "0")))))

(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 247 c 0 0)))
(defun halt ()
  (syscall3 247 1 0 0))
")

(defvar *cli-arch-probe-source* "
(defun %argv-string-at (addr)
  (let ((len 0))
    (let ((i 0))
      (loop
        (let ((b (mem-ref (+ addr i) :u8)))
          (when (= b 0) (return nil))
          (setq i (+ i 1)))
        (setq len i)))
    (if (zerop len) nil
        (let ((s (%make-string-array len)) (i 0))
          (loop
            (when (>= i len) (return s))
            (aset s i (mem-ref (+ addr i) :u8))
            (setq i (+ i 1)))))))
(defun %argc  () (mem-ref #x10000200 :u32))
(defun %hc-depth () (mem-ref #x10000400 :u32))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u32) 0) nil t))
")

;;; The heap mapping is zero-filled and covers the low block; nothing to set up.
(defvar *cli-arch-kernel-prologue* "")

;;; Scratch pages below the allocator (heap+#x20000): cstr at #x10001000
;;; (rename uses +2048), the 4 KB I/O page at #x10002000 -- RV32's layout.
(defvar *cli-arch-io-scratch-source* "  (setq *cstr-scratch* #x10001000)
  ;; getdents64 scratch: cl-fileio's default is a fixed #x1DD00000, which on
  ;; this layout is INSIDE the heap (it starts at #x10000000 and reaches
  ;; #x1F800000), so listing a directory wrote dirents over live objects --
  ;; found when the 68k CLI died in install-tarball, in a directory with a
  ;; thousand entries.  4 KB is all it uses.
  (setq *%dirent-buf-addr* #x10003000)
  (setq *io-buf-addr*  #x10002000)
  (setq *scratch-mmapped* t)
")

(defvar *cli-arch-kernel-epilogue*
"  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
")

(defvar *cli-arch-override-source* "
(defun %sys-open-rdonly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 0 0))
(defun %sys-open-wronly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 577 420))
(defun %sys-open-append (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 1089 420))
(defun %sys-open-rdwr (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 66 420))
(defun %sys-open-wronly-existing (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 1 0))
(defun %sys-open-create-excl (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 193 420))
(defun %sys-close (fd) (syscall3 6 fd 0 0))
(defun %sys-getpid () (syscall3 20 0 0 0))
(defun %sys-read-raw (fd buf-addr count) (syscall3 3 fd buf-addr count))
(defun %sys-write-raw (fd buf-addr count) (syscall3 4 fd buf-addr count))
(defun %sys-lseek (fd offset whence) (syscall3 19 fd offset whence))
(defun %sys-unlink (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 10 *cstr-scratch* 0 0))
(defun %sys-rename (old-str new-str)
  (%string-to-cstr old-str *cstr-scratch*)
  (let ((new-addr (+ *cstr-scratch* 2048)))
    (%string-to-cstr new-str new-addr)
    (syscall3 38 *cstr-scratch* new-addr 0)))
(defun %sys-mkdir (path-str mode)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 39 *cstr-scratch* mode 0))
(defun %sys-readlink (path-str) (declare (ignore path-str)) nil)   ; links not followed here yet

(defun %sys-stat-exists (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*)))
    (if (< (syscall3 33 path-addr 0 0) 0) nil t)))
(defun %sys-stat-size (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 195 path-addr buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 44) :u32)))))
(defun %sys-stat-mtime (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 195 path-addr buf-addr 0)))
      (if (< ret 0) 0 (mem-ref (+ buf-addr 68) :u32)))))
(defun %sys-fstat-size (fd)
  (let ((buf-addr *io-buf-addr*))
    (let ((ret (syscall3 197 fd buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 44) :u32)))))
(defun %sys-getdents64 (fd buf-addr buf-size) (syscall3 220 fd buf-addr buf-size))
;; clock_gettime is 260 on m68k (measured with qemu -strace: 263 is statfs64 there,
;; 265 tgkill; ARM EABI 263, i386 265).  struct timespec is
;; two 32-bit longs, so tv_nsec is at +4, not +8.  Without this override
;; GET-INTERNAL-REAL-TIME fell back to its call counter and every timing on
;; i386 read 1 tick.  (32-bit time_t: good until 2038.)
(defun %clock-gettime-ns (clk)
  (let ((buf *io-buf-addr*))
    (if (eql (syscall3 260 clk buf 0) 0)
        (+ (* (mem-ref buf :u32) 1000000000) (mem-ref (+ buf 4) :u32))
        0)))

(defun %cli-argv-base () 268472320)   ; #x10009000 — the staged pointer array

(defun %cli-collect-argv ()
  (let ((argc (%cli-argc)) (base (%cli-argv-base)) (acc nil) (i 0))
    (loop
      (when (>= i argc) (return (reverse acc)))
      (let ((ptr (mem-ref (+ base (* 4 i)) :u32)))
        (when (eql ptr 0) (return (reverse acc)))
        (setq acc (cons (%cli-cstr-at ptr) acc)))
      (setq i (+ i 1)))))

(defun %cli-getenv (name)
  ;; envp begins one slot past argv's NULL terminator, exactly as on the real
  ;; stack.  NB the equals sign is built with code-char: this source is a LISP
  ;; STRING and a double quote in it would terminate the string early.
  (let ((argc (%cli-argc)) (base (%cli-argv-base)) (i 0))
    (let ((envp (+ base (* 4 (+ argc 1))))
          (prefix (concatenate (quote string) name (string (code-char 61)))))
      (let ((plen (length prefix)))
        (loop
          (let ((ptr (mem-ref (+ envp (* 4 i)) :u32)))
            (when (eql ptr 0) (return nil))
            (let ((entry (%cli-cstr-at ptr)))
              (when (and entry (>= (length entry) plen)
                         (string= (subseq entry 0 plen) prefix))
                (return (subseq entry plen)))))
          (setq i (+ i 1)))))))
")

(require :sb-posix)

;; NARROW STRINGS (prelude.lisp %NARROW-STRINGS-P): the 68k collector sizes
;; #x13 as bytes (translate-68k.lisp M68K-EMIT-OBJECT-SIZE); MODUS_NARROW_STRINGS=1
;; has the reader and the runtime's name tables make them in this build.
(when (let ((v (sb-ext:posix-getenv "MODUS_NARROW_STRINGS"))) (and v (> (length v) 0) (string/= v "0")))
  (setq *cli-arch-override-source*
        (concatenate 'string *cli-arch-override-source*
                     (string #\Newline) "(defun %narrow-strings-p () t)" (string #\Newline)
                     ;; but no 32-bit (#x15) strings from the in-image compiler:
                     ;; their accessors assume the 64-bit layout (compiler.lisp
                     ;; *STRING32-P*)
                     "(defun %string32-strings-p () nil)" (string #\Newline)))
  ;; ... and pooled literals are narrow too (cross.lisp *POOL-NARROW-STRINGS*,
  ;; set from this below: MODUS.MVM does not exist yet here).
  (defparameter cl-user::*68k-pool-narrow* t))

;; MODUS_68K_GC_STATIC: boot's heap becomes static once boot is done -- held
;; once, walked by every collection, never copied (translate-68k.lisp
;; *68K-GC-STATIC*).  1 = on, 0 = off; unset, it follows MODUS_SMALL_IMAGE.
;; Read HERE, before build-cli-common assembles the image source.
(let ((v (sb-ext:posix-getenv "MODUS_68K_GC_STATIC"))
      (small (let ((s (sb-ext:posix-getenv "MODUS_SMALL_IMAGE")))   ; *SMALL-IMAGE*'s rule;
               (and s (plusp (length s)) (not (string= s "0"))))))  ; it is not defined yet
  ;; CL-USER here: the translator's package is not current until below, where
  ;; MODUS.MVM::*68K-GC-STATIC* is set from this.
  (defparameter cl-user::*68k-gc-static-on*
    (cond ((null v) small)
          ((string= v "0") nil)
          (t t))))
;; MODUS_68K_TAGS3 (read here, in CL-USER, for the card code's constants;
;; MODUS.MVM::*68K-TAGS3* is set from it below): unset follows MODUS_SMALL_IMAGE.
(defparameter cl-user::*68k-tags3-on*
  (let ((v (sb-ext:posix-getenv "MODUS_68K_TAGS3"))
        (s (sb-ext:posix-getenv "MODUS_SMALL_IMAGE")))
    (cond ((null v) (and s (plusp (length s)) (not (string= s "0"))))
          ((string= v "0") nil)
          (t t))))
(defun cl-user::%68k-granule-subst (text)
  "TEXT with the heap granule's constants filled in."
  (let ((pairs (if cl-user::*68k-tags3-on*
                   '(("GRAN-1" . "7") ("MINUS-GRAN" . "-8") ("MAX-SHIFT" . "10") ("MINUS-KSHIFT" . "-3"))
                   '(("GRAN-1" . "15") ("MINUS-GRAN" . "-16") ("MAX-SHIFT" . "11") ("MINUS-KSHIFT" . "-4")))))
    (dolist (p pairs text)
      (loop for i = (search (car p) text) while i
            do (setq text (concatenate 'string (subseq text 0 i) (cdr p)
                                       (subseq text (+ i (length (car p))))))))))

(when cl-user::*68k-gc-static-on*
  (setq *cli-arch-override-source*
        (concatenate 'string *cli-arch-override-source* (cl-user::%68k-granule-subst (concatenate 'string "
" (with-open-file (in (merge-pathnames "68k-gc-static.lisp"     ; MVM-TEXT is not loaded yet
                                       (directory-namestring (truename *load-truename*))))
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))) "

;; A SAVED CORE (scripts/68k-core-link.py): the low block and the static heap,
;; mapped by the ELF itself.  #x10006014 is non-zero only in a core, so boot
;; init is skipped; the stub has already done the restoring.
(defun %core-requested-p () (eql (mem-ref #x10006014 :u32) 1))
(defun %restore-image () nil)

(defun %68k-write-all (fd addr len)
  (loop
    (when (<= len 0) (return t))
    (let ((n (%sys-write-raw fd addr len)))
      (when (<= n 0) (return nil))
      (setq addr (+ addr n) len (- len n)))))

(defun %68k-save-core (path)
  \"Write the core to PATH: a header (magic, Y, three regions as address and
   length) at #x10003000, then the low block [#x10000000, #x10020000) and the
   static objects and conses, each rounded out to pages.  Called right after
   the freeze and allocates NOTHING, so no saved word points at the young
   space, which the core does not carry.  The low block is saved as a core
   boot must see it: flag set, no freeze request, no handler frames.\"
  (let* ((olo (logand (mem-ref #x10006004 :u32) -4096))
         (ohi (logand (+ (mem-ref #x10006008 :u32) 4095) -4096))
         (clo (logand (mem-ref #x1000600C :u32) -4096))
         (chi (logand (+ (mem-ref #x10006010 :u32) 4095) -4096))
         (y (logand (+ (max ohi chi) 65535) -65536))
         (h #x10003000)
         (fd 0))
    (setf (mem-ref #x10006014 :u32) 1)
    (setf (mem-ref #x10006000 :u32) 0)
    (setf (mem-ref #x10000400 :u32) 0)             ; handler depth
    (setf (mem-ref #x10000180 :u32) 0)             ; armed jmpbuf
    (setf (mem-ref #x10000360 :u32) 0)             ; capped pushes
    (setf (mem-ref h :u32) 12865357)               ; magic
    (setf (mem-ref (+ h 4) :u32) y)
    (setf (mem-ref (+ h 8) :u32) #x10000000)  (setf (mem-ref (+ h 12) :u32) #x20000)
    (setf (mem-ref (+ h 16) :u32) olo)        (setf (mem-ref (+ h 20) :u32) (- ohi olo))
    (setf (mem-ref (+ h 24) :u32) clo)        (setf (mem-ref (+ h 28) :u32) (- chi clo))
    (setq fd (%sys-open-wronly path))
    (when (< fd 0) (%cli-exit 2))
    (unless (and (%68k-write-all fd h 32)
                 (%68k-write-all fd #x10000000 #x20000)
                 (%68k-write-all fd olo (- ohi olo))
                 (%68k-write-all fd clo (- chi clo)))
      (%cli-exit 3))
    (%sys-close fd)
    (%cli-exit 0)))
"))))
  (setq *cli-arch-kernel-epilogue*
        "  ;; --- the heap boot built becomes static (MODUS_68K_GC_STATIC) ---
  ;; MODUS_68K_SAVE_CORE=PATH (the build's core run): freeze, then write the
  ;; core and exit -- read BEFORE the freeze, since nothing may allocate after.
  (let ((core-path (%cli-getenv \"MODUS_68K_SAVE_CORE\")))
    (%gc-static-freeze)
    (when (and core-path (> (length core-path) 0))
      (%68k-save-core core-path)))
  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
"))

(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

(mvm-load "boot/boot-linux-68k.lisp")

(in-package :modus.mvm)
(setf *68k-gc-static* cl-user::*68k-gc-static-on*)
(setf *68k-tags3* cl-user::*68k-tags3-on*)
(when *68k-tags3* (format t "~&;; MODUS_68K_TAGS3: 3-bit tags, 8-byte heap granule~%"))
(when (and (boundp 'cl-user::*68k-pool-narrow*) cl-user::*68k-pool-narrow*)
  (setf *pool-narrow-strings* t))

(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf *write-symmap-path* sm)))

(install-68k-translator)
(m68k-set-linux-mode t)
;; MODUS_68K_RELAX_BRANCHES: each branch as short as its distance allows
;; (translate-68k.lisp, *68K-RELAX-BRANCHES*: two translation passes).  1 = on,
;; 0 = off; unset, it follows MODUS_SMALL_IMAGE.
(let ((v (sb-ext:posix-getenv "MODUS_68K_RELAX_BRANCHES")))
  (setf *68k-relax-branches*
        (cond ((null v) (and cl-user::*small-image* t))
              ((string= v "0") nil)
              (t t)))
  (when *68k-relax-branches*
    (format t "~&;; MODUS_68K_RELAX_BRANCHES: branches relaxed (two passes)~%")))
;; MODUS_68K_SHARED: the most repeated sequences (CONS so far) as one shared copy
;; each, called (translate-68k.lisp *68K-SHARED-SEQUENCES*).  1 = on, 0 = off;
;; unset, it follows MODUS_SMALL_IMAGE.
(let ((v (sb-ext:posix-getenv "MODUS_68K_SHARED")))
  (setf *68k-shared-sequences*
        (cond ((null v) (and cl-user::*small-image* t))
              ((string= v "0") nil)
              (t t))))

;; Small images: shifted-byte immediates as MOVEQ + LSL (translate-68k.lisp
;; *68K-LI-SHIFT*).
(setf *68k-li-shift* (and cl-user::*small-image* t))
;; MODUS_68K_A5_GLOBALS: A5 holds the globals base, so the calling-convention
;; slots are (A5) / d16(A5) (translate-68k.lisp *68K-A5-GLOBALS*).  1 = on,
;; 0 = off; unset, it follows MODUS_SMALL_IMAGE.
(let ((v (sb-ext:posix-getenv "MODUS_68K_A5_GLOBALS")))
  (setf *68k-a5-globals*
        (cond ((null v) (and cl-user::*small-image* t))
              ((string= v "0") nil)
              (t t))))

(let ((v (sb-ext:posix-getenv "MODUS_68K_GC_POISON")))
  (when (and v (plusp (length v)) (not (string= v "0")))
    (setf modus.mvm::*68k-gc-poison* t)
    (format t "~&;; MODUS_68K_GC_POISON: collections poison the evacuated semispace~%")))

;; MODUS_68K_PROFILE=1: per-function execution counters (translate-68k.lisp
;; *68K-PROFILE*) -- a profiling build, for choosing what stays native.
(when (let ((v (sb-ext:posix-getenv "MODUS_68K_PROFILE"))) (and v (string/= v "0")))
  (setf *68k-profile* t))

;; MODUS_68K_TAGS3=1: the 3-bit pointer tags and the 8-byte heap granule
;; (target.lisp *68K-TAGS3*).  Read early: the card code below is baked with
;; the granule.
;; MODUS_68K_THREADED=1: every function that can be is THREADED cold code --
;; a byte token stream run by shared native handlers (translate-68k.lisp
;; *68K-THREADED*).  A5 is then the token pointer, so it does not hold the
;; globals base.
(when (let ((v (sb-ext:posix-getenv "MODUS_68K_THREADED"))) (and v (string/= v "0")))
  (setf *68k-threaded* :all *68k-a5-globals* nil)
  (format t "~&;; MODUS_68K_THREADED: cold functions as threaded code~%")
  ;; MODUS_68K_HOT=FILE: function names, one a line, that stay native (a
  ;; profile's hottest: MODUS_68K_PROFILE).
  (let ((f (sb-ext:posix-getenv "MODUS_68K_HOT")))
    (when (and f (plusp (length f)))
      (let ((h (make-hash-table :test 'equal)))
        (with-open-file (in f)
          (loop for l = (read-line in nil) while l
                do (let ((n (string-trim " " l))) (when (plusp (length n)) (setf (gethash n h) t)))))
        (setf *68k-hot-names* h)))))
(let* ((out (or (sb-ext:posix-getenv "MODUS_CLI_OUT")
                "/tmp/modus-68k-cli"))
       (image (build-image :target :linux-68k
                           :source-text cl-user::*full-source*)))
  (if *68k-unimpl-opcodes*
      (format t "~%  *** TRANSLATOR GAPS: ~{#x~X~^ ~} (#x1xxxx = trap xxxx) ***~%"
              (sort (copy-list *68k-unimpl-opcodes*) #'<))
      (format t "~%  TRANSLATOR: no unimplemented opcodes or traps.~%"))
  (ensure-directories-exist out)
  (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
    (write-sequence (kernel-image-image-bytes image) o))
  (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
  (format t "~%Wrote ~D bytes to ~A~%"
          (length (kernel-image-image-bytes image)) out)
  (format t "Run: qemu-m68k-static ~A --eval EXPR --quit~%" out)
  ;; MODUS_68K_CORE=1 (a MODUS_68K_GC_STATIC image): boot it once under qemu to
  ;; save its core, then link the core in as PT_LOADs (scripts/68k-core-link.py)
  ;; -- the image then starts from the saved heap.  OUT.nocore keeps the plain
  ;; image.  MODUS_68K_CORE_HEAP: the heap of that one boot (default 8 MB).
  (when (and *68k-gc-static*
             (let ((v (sb-ext:posix-getenv "MODUS_68K_CORE"))) (and v (string/= v "0"))))
    (let* ((plain (concatenate 'string out ".nocore"))
           (core (concatenate 'string out ".core"))
           (heap (or (sb-ext:posix-getenv "MODUS_68K_CORE_HEAP") "8"))
           (script (namestring (merge-pathnames "scripts/68k-core-link.py" cl-user::*modus-base*))))
      (rename-file out plain)
      (let ((p (sb-ext:run-program "qemu-m68k-static"
                                   (list "-cpu" "m68040" plain "--dynamic-space-size" heap)
                                   :search t :input nil :output t
                                   :environment (append (list (format nil "MODUS_68K_SAVE_CORE=~A" core)
                                                              "MODUS_FASL=0")
                                                        (sb-ext:posix-environ)))))
        (unless (and (zerop (sb-ext:process-exit-code p)) (probe-file core))
          (error "68k core: the core run failed (exit ~D)" (sb-ext:process-exit-code p))))
      (let ((p (sb-ext:run-program "python3" (list script plain core out)
                                   :search t :output t)))
        (unless (zerop (sb-ext:process-exit-code p))
          (error "68k core: linking the core failed")))
      (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
      (format t "~&Wrote ~A (with its core)~%" out))))
