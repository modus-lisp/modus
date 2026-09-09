
;;; ============================================================
;;; ARCH SLOTS — Linux/i386
;;; ============================================================

(defvar *cli-arch* :i386)

;;; ABI FACT: the i386 int-0x80 table is its own numbering.  exit is 1 (60 on
;;; x86-64, 93 on the AArch64 generic ABI); exit_group is 252 (231 / 94).
;;; This slot used plain `exit' on the reasoning that the image is
;;; single-threaded — true when it was written, and the two are then
;;; indistinguishable.  It is no longer true, and `exit' with a live sibling
;;; thread is an unreapable zombie leader and a permanent hang, so all three
;;; arches now use exit_group.
;;; The `(let ((c code)) ...)' rebind is kept identical in shape to the other
;;; two arches so a codegen difference in the operand shuffle shows up as a
;;; difference in the TRANSLATOR, not in the source.
(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 252 c 0 0)))
(defun halt ()
  (syscall3 252 1 0 0))
")

;;; NOT a divergence: byte-for-byte the x64 probe slot.
;;;
;;; boot/boot-linux-i386.lisp publishes argc/argv[1]/argv[2] at the SAME fixed
;;; BSS addresses as the 64-bit boots (0x10000200 / 0x10000208 / 0x10000248),
;;; and mvm/translate-i386.lisp puts the handler-frame jmpbuf at 0x10000180 and
;;; the handler-stack depth at 0x10000400 — again the same.  Every load here is
;;; :u8 or :u32, both of which tag their result on i386 exactly as on x64, and
;;; every address is below 2^29 so it survives a 30-bit fixnum.  So this block
;;; is shared verbatim rather than re-derived; if the layouts ever diverge, the
;;; right fix is to say so here, not to fork the text silently.
(defvar *cli-arch-probe-source*
"
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
(defun %argv1 () (%argv-string-at #x10000208))
(defun %argv2 () (%argv-string-at #x10000248))
(defun %argc  () (mem-ref #x10000200 :u32))
(defun %hc-depth () (mem-ref #x10000400 :u32))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u32) 0) nil t))
(defun %hc-frame-ip (n) (mem-ref (+ #x10000408 (* 32 n) 16) :u32))
(defun %hc-cur-ip () (mem-ref #x10000190 :u32))
")

;;; i386 needs no pre-init hardware setup.  boot/boot-linux-i386.lisp's entry
;;; stub has already published the Cheney metadata and mmap'd BOTH bitmaps
;;; (object-start and cons-kind) before kernel-main runs — it must, because
;;; gc.lisp's %gc-bitmap-init allocates via trap #x0531 (%mmap-exec-page),
;;; which i386 does not implement.  The BSS is demand-zeroed by the kernel
;;; (p_memsz > p_filesz in a single PT_LOAD), so there is nothing to clear.
(defvar *cli-arch-kernel-prologue* "")

;;; MEMORY-MAP FACT — the one place i386 genuinely cannot follow the 64-bit
;;; ports.  Two independent constraints:
;;;
;;;   (a) The 64-bit default #x1DF00000 / #x1DE00000 (cl-fileio.lisp's defvar
;;;       initforms, which init-all-globals restores) lies inside the 64-bit
;;;       ELF's huge BSS tail.  i386's BSS ends at +linux-i386-bss-end+ =
;;;       0x10020000 and its heap starts at 0x30000000, so 0x1DF00000 is in
;;;       neither — every %string-to-cstr would fault.
;;;   (b) A syscall argument reaches trap #x0502 as a TAGGED fixnum and is
;;;       SAR'd back, so any address handed to a syscall must be < 2^30.
;;;
;;; The free window is 0x10004000..0x10009000 (0x10009000 upward is the boot
;;; stub's staged argv pointer array and string arena).  cstr gets 16 KiB,
;;; io-buf the 4 KiB page cl-fileio reads/writes in.
;;;
;;; *scratch-mmapped* is forced T for a THIRD i386 reason: cl-fileio.lisp's
;;; %ensure-scratch-mmapped issues `(syscall3 9 ...)', which is mmap on x86-64
;;; but link(2) on i386 — a bogus link() with two garbage pointers.  It returns
;;; EFAULT and is harmless today, but it is not something to leave armed.
(defvar *cli-arch-io-scratch-source*
"  (setq *cstr-scratch* #x10004000)  ; i386 BSS window, below 2^30
  (setq *io-buf-addr*  #x10008000)  ; one 4 KiB page, ends at the argv arena
  (setq *scratch-mmapped* t)        ; the BSS is already mapped; never mmap
")

;;; i386 toplevel entry.  Byte-for-byte x64's: no baked probe program, because
;;; this is a shipping CLI (see the RETIRED note in the header).
(defvar *cli-arch-kernel-epilogue*
"  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
")

;;; ARCH SLOT — late last-defun-wins overrides.  Two ABI facts.
;;;
;;; 1. SYSCALL NUMBERS.  mvm/cl-fileio.lisp hardcodes the x86-64 table
;;;    (open=2 close=3 read=0 write=1 lseek=8 stat=4 fstat=5 unlink=87
;;;    rename=82 mkdir=83 getpid=39 getdents64=217).  i386 shares almost none
;;;    of them.  Same class build-aarch64-cli.lisp fixed for the *at-only
;;;    AArch64 ABI (be1aef1): without these, LOAD, OPEN and every path
;;;    predicate are silently dead.
;;;
;;;    Two deliberate departures from a literal transliteration:
;;;      * EXISTENCE goes through access(2) (33), not stat.  It is all OPEN
;;;        needs and it has no struct layout to get wrong.
;;;      * SIZE/MTIME use stat64 (195) / fstat64 (197), whose struct stat64 is
;;;        NOT x86-64's struct stat: st_size is at 44, not 48, and st_mtime at
;;;        72, not 88.
;;;
;;; 2. ARGV/ENVP.  lib/cli-toplevel.lisp walks the LIVE initial stack with
;;;    8-byte slots and the :u64 raw>>1 convention.  Neither holds here: slots
;;;    are 4 bytes, and the i386 kernel stack sits at 0x40800390 — above the
;;;    2^30 ceiling a tagged mem-ref address can express.  So i386 reads the
;;;    copy the boot stub STAGED into the BSS at +linux-i386-argv-ptrs+
;;;    (0x10009000), whose slots hold BSS byte addresses directly: a :u32 load
;;;    tags its result, so the Lisp value IS the pointer — no doubling, nothing
;;;    unrepresentable.
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
      (if (< ret 0) 0 (mem-ref (+ buf-addr 72) :u32)))))
(defun %sys-fstat-size (fd)
  (let ((buf-addr *io-buf-addr*))
    (let ((ret (syscall3 197 fd buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 44) :u32)))))
(defun %sys-getdents64 (fd buf-addr buf-size) (syscall3 220 fd buf-addr buf-size))

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

;;; WEB-ONLY OVERRIDES.  Appended to the i386 override slot so they win by
;;; last-defun-wins.  The JS machine is single-threaded and never sets the
;;; threads-live gate at 0x10000DB8, so the runtime-lock probes are pure cost:
;;; on the 30-bit tower a (mem-ref .. :u32) promotes through BIGNUM-ASH, and
;;; %rt-enter/%rt-leave bracket every intern.
(setq *cli-arch-override-source*
      (concatenate 'string *cli-arch-override-source* "
(defun %rt-enter () 0)
(defun %rt-leave () 0)
;; %ht-hash's FNV-1a uses 32-bit constants, which on the 30-bit tower makes
;; every character a bignum multiply.  Same bucket quality, fixnum-only:
;; h stays below 2^20, the multiplier is 5 bits, so the product is < 2^25.
(defun %ht-hash (key strcmp?)
  (cond
    ((stringp key)
     (if strcmp?
         (let ((h 5381) (len (array-length key)) (i 0))
           (loop
             (when (>= i len) (return nil))
             (setq h (logand (+ (* h 31) (%prim-aref key i)) #xFFFFF))
             (setq i (+ i 1)))
           (logand (logxor h (ash h -8)) 255))
         (%ht-nohash)))
    ((fixnump key)  (logand key 255))
    ((characterp key) (logand (char-code key) 255))
    ((null key) 17)
    ((eq key t) 19)
    ((%cl-sym-p key) (logand (%cl-sym-hash key) 255))
    ((%native-mvm-sym-p key) (logand (%native-mvm-sym-hash key) 255))
    (t (%ht-nohash))))
;; EQL is the hottest function in the image (every non-EQ inline eql falls
;; into it) and the stock body runs four type-predicate chains before it can
;; say NIL.  Same answers, ordered so the common cases exit in a few steps.
(defun eql (a b)
  (cond
    ((eq a b) t)
    ((fixnump a) nil) ((fixnump b) nil)
    ((consp a) nil) ((consp b) nil)
    ((null a) nil) ((null b) nil)
    ((characterp a) nil) ((characterp b) nil)
    ((eq a t) nil) ((eq b t) nil)
    (t (let ((sa (obj-subtag a)) (sb (obj-subtag b)))
         (cond
           ((/= sa sb) nil)
           ((or (= sa #x60) (and (>= sa #x64) (<= sa #x66)))
            (and (= (%float-hi32 a) (%float-hi32 b))
                 (= (%float-lo32 a) (%float-lo32 b))))
           ((= sa #x33)
            (and (= (aref a 0) (aref b 0)) (= (aref a 1) (aref b 1))))
           ((= sa #x30) (numeric-equal-p a b))
           ((and (= sa #x32) (%complex-p a) (%complex-p b))
            (and (eql (%complex-real a) (%complex-real b))
                 (eql (%complex-imag a) (%complex-imag b))))
           (t nil))))))
"))

(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

;;; ============================================================
;;; WEB TARGET — dump the MVM module for the JavaScript interpreter
;;; ============================================================
;;;
;;; Everything above this line is byte-for-byte the i386 CLI wrapper's arch
;;; slots: the JS interpreter (web/mvm.js) models the HOSTED i386 machine —
;;; 32-bit words, the int-0x80 syscall numbers, argv staged at 0x10009000 —
;;; so the compiled runtime must be the i386 one.  Instead of translating to
;;; native code we stop after compile-source-to-module and serialise:
;;;
;;;   magic "MVMW" | version u32 | word-size u32
;;;   bytecode:     len u32, bytes
;;;   fn table:     count u32, then per fn: name-hash u32, param-count u32,
;;;                 bytecode-offset u32, bytecode-length u32,
;;;                 name-len u16, name bytes (ASCII)
;;;   const pool:   count u32, addr-table u32 x count (pool-relative tagged
;;;                 offsets), pool-len u32, pool bytes (32-bit string layout:
;;;                 header (len<<8|#x31), len char slots char<<1, align 16)
;;;
;;;   sbcl --dynamic-space-size 8192 --script mvm/build-web.lisp
;;;   -> web/modus.mvmw  (override with MODUS_WEB_OUT)

(in-package :modus.mvm)

(defvar *web-image-path*
  (or #+sbcl (sb-ext:posix-getenv "MODUS_WEB_OUT")
      (namestring (merge-pathnames "web/modus.mvmw" cl-user::*modus-base*))))

(defun web-build-constant-pool-32 (module)
  "32-bit string-pool layout (build-constant-pool only knows 64-bit)."
  (let* ((buf (make-mvm-buffer))
         (constants (mvm-module-constant-table module))
         (n (length constants))
         (addr-table (make-array n :initial-element 0)))
    (loop for constant in constants
          for idx from 0
          do (typecase constant
               (string
                (loop while (/= 0 (mod (mvm-buffer-position buf) 16))
                      do (mvm-emit-byte buf 0))
                (let ((obj-offset (mvm-buffer-position buf))
                      (len (length constant)))
                  (mvm-emit-u32 buf (logior #x31 (ash len 8)))
                  (loop for c across constant
                        do (mvm-emit-u32 buf (ash (char-code c) 1)))
                  (loop while (/= 0 (mod (mvm-buffer-position buf) 16))
                        do (mvm-emit-byte buf 0))
                  (setf (aref addr-table idx) (logior obj-offset #x09))))
               (t (mvm-emit-u32 buf 0))))
    (values (mvm-buffer-used-bytes buf) addr-table)))

(defun web-write-module (module path)
  (let ((out (make-mvm-buffer))
        (bc (mvm-module-bytecode module))
        (fns (mvm-module-function-table module)))
    (flet ((u32 (v) (mvm-emit-u32 out (logand v #xFFFFFFFF)))
           (u16 (v) (mvm-emit-u16 out v))
           (bytes (v) (loop for b across v do (mvm-emit-byte out b))))
      (bytes (map 'vector #'char-code "MVMW"))
      (u32 1) (u32 4)
      (u32 (length bc)) (bytes bc)
      (u32 (length fns))
      (dolist (f fns)
        (let ((name (let ((n (mvm-function-info-name f)))
                      (if (stringp n) n (format nil "~A" n)))))
          (u32 (or (mvm-function-info-name-hash f) 0))
          (u32 (or (mvm-function-info-param-count f) 0))
          (u32 (mvm-function-info-bytecode-offset f))
          (u32 (mvm-function-info-bytecode-length f))
          (u16 (min 65535 (length name)))
          (loop for c across name
                for i below 65535
                do (mvm-emit-byte out (logand (char-code c) #xFF)))))
      (multiple-value-bind (pool addr-table) (web-build-constant-pool-32 module)
        (u32 (length addr-table))
        (loop for a across addr-table do (u32 a))
        (u32 (length pool)) (bytes pool)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence (mvm-buffer-used-bytes out) o))
    (format t "~%Wrote ~D bytes to ~A (~D fns, ~D bytecode bytes)~%"
            (length (mvm-buffer-used-bytes out)) path
            (length fns) (length bc))))

(format t "~%Compiling web (i386-word) module (~D chars)...~%"
        (length cl-user::*full-source*))
;;; The 30-bit tower makes every (mem-ref .. :u32) promote through two u16
;;; loads and BIGNUM-ASH (%mem-width-promotes-p), which is what SYMBOL-VALUE,
;;; the runtime-lock gate and the handler probes do on every call.  Every
;;; :u32 site in the image reads a word that is provably below 2^30 on this
;;; machine (flag words, argc, handler depth, stat fields, BSS addresses), so
;;; the web build takes the plain single-load path.
(defun %mem-width-promotes-p (width-code needs-tag)
  (declare (ignore width-code needs-tag))
  nil)
(set-target-fixnum-bits-for (find-target :i386))
(let ((module (compile-source-to-module cl-user::*full-source*)))
  (web-write-module module *web-image-path*))
