;;;; build-i386-cli.lisp — the hosted Modus CLI for Linux/i386 (32-bit).
;;;;
;;;; A :linux-i386 ELF you run as an ordinary process (under qemu-i386-static
;;;; here — binfmt_misc is not registered, so a 32-bit ELF silently fails to
;;;; exec rather than reporting an error).  Same SBCL-faithful toplevel as
;;;; x64's ./modus: full argv, --eval/--load/--script/--quit/--version,
;;;; ~/.modusrc, and the REPL.
;;;;
;;;;   sbcl --dynamic-space-size 8192 --script mvm/build-i386-cli.lisp
;;;;   -> /tmp/modus-i386-cli   (override with MODUS_I386_OUT / MODUS_CLI_OUT)
;;;;   qemu-i386-static /tmp/modus-i386-cli --eval '(print (+ 1 2))' --quit
;;;;
;;;; STRUCTURE — CONVERGED WITH x64/aarch64 (2026-08).  This wrapper is a thin
;;;; i386 tail over mvm/build-cli-common.lisp, the SAME shared assembly
;;;; mvm/build-generic-cli.lisp and mvm/build-aarch64-cli.lisp use.
;;;;
;;;; WHAT IT USED TO BE, AND WHY THAT WAS A PROBLEM.  This file was a 2145-line
;;;; THIRD lineage: it loaded lib/load-mvm.lisp directly and re-derived the
;;;; whole source set by hand, with its own copies of the defun scanner, the
;;;; symbol-name scanner, the macro-table scanner, the opcode-table emitter,
;;;; the float-slot override, the CL-bridge file list and its own boot
;;;; sequence (%l5-boot).  The consequence was not stylistic.  It baked ZERO
;;;; library surface — no RTEST, no tar/install-tarball, no ASDF interface, no
;;;; hosted sockets/storage/HTTP — so the 22-library ladder and alexandria's
;;;; own test suite could not RUN on 32 bits at all.  Not "scored badly":
;;;; could not run.  i386 was the only release-gate platform with no library
;;;; measurement, and there was no mechanism that would ever have produced one,
;;;; because every capability added to the hosted CLI landed in the shared file
;;;; this wrapper did not read.
;;;;
;;;; RETIRED WITH THE OLD LINEAGE (recover from git history at ba693fa):
;;;;   * MODUS_I386_LAYER=1..5.  A bring-up scaffold: bake only the prelude,
;;;;     then +gc/rt, then the CL bridge, then crypto, then the compiler.  The
;;;;     port is past layer 5, and the shared file bakes one fixed set.
;;;;   * the ~1300-line baked probe suite reached by a numeric argv[1]
;;;;     (`run-i386.sh test/gc/bulk/chain/argv/probe N`), including the
;;;;     SHA-256-over-N-KiB GC-survival gate, which needed net/crypto.lisp
;;;;     baked in.  Per CLAUDE.md's build taxonomy a shipping image bakes no
;;;;     test corpus; the replacement gate is the ladder (scripts/
;;;;     run-ladder-i386.sh), and the probes should come back as a --load-able
;;;;     script, which this image can now actually run.
;;;;
;;;; What this file may legitimately contain is i386 HARDWARE/ABI FACT and
;;;; nothing else.  A capability belongs in build-cli-common.lisp, where all
;;;; three arches get it at once.

;;; ============================================================
;;; ARCH SLOTS — Linux/i386
;;; ============================================================

(defvar *cli-arch* :i386)

;;; ABI FACT: the i386 int-0x80 table is its own numbering.  exit is 1 (60 on
;;; x86-64, 93 on the AArch64 generic ABI); exit_group is 252 but plain exit is
;;; what this single-threaded image wants and what the port has always used.
;;; The `(let ((c code)) ...)' rebind is kept identical in shape to the other
;;; two arches so a codegen difference in the operand shuffle shows up as a
;;; difference in the TRANSLATOR, not in the source.
(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 1 c 0 0)))
(defun halt ()
  (syscall3 1 1 0 0))
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

(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

;;; ============================================================
;;; Build the Linux/i386 ELF
;;; ============================================================

(mvm-load "boot/boot-linux-i386.lisp")

(in-package :modus.mvm)

(modus.mvm.i386:install-i386-translator)

(setf modus.mvm.i386::*i386-linux-mode* t)
(modus.mvm.i386::i386-set-globals-base +linux-i386-globals+)
;; Function entries are 16-byte aligned and OR-3 tagged, exactly as on x64, so
;; the fn / cons / char / obj low nibbles stay disjoint.  The i386 boot preamble
;; is emitted as a separate section, so native code starts at offset 0.
(setf modus.mvm.i386::*i386-fn-tag-3* t)
(setf modus.mvm.i386::*i386-fn-align* 16)
(setf modus.mvm.i386::*i386-native-code-offset* 0)
(setf modus.mvm.i386::*i386-checked-arith-slowpath*
      (let ((v (sb-ext:posix-getenv "MODUS_I386_NO_CHECKED")))
        (not (and v (plusp (length v))))))

;; ---- ENV KNOBS (the complete list; defaults are what ships) ---------------
;; PRODUCTION
;;   MODUS_I386_OUT=<path>    where to write the image (default
;;                            /tmp/modus-i386-cli).  MODUS_CLI_OUT is accepted
;;                            too, for parity with the other two wrappers.
;;   MODUS_I386_SYMMAP=<path> symbol map location (default: image path +.symmap)
;; DEV / TRIAGE ONLY
;;   MODUS_I386_GC=0          build WITHOUT the collector.  Every allocation is
;;                            then permanent and bulk work dies at the arena
;;                            edge, honestly.
;;   MODUS_I386_BMP=0         drop the object-start / cons-kind bit-set.  Only
;;                            sensible with GC=0: without the bitmaps scan_word
;;                            has no conservative-root validation and
;;                            copy_object stamps forwarding pointers over
;;                            mid-object data.
;;   MODUS_I386_VL=<bytes>    first GC trigger, so a collection can be provoked
;;                            in seconds instead of gigabytes.
;;   MODUS_I386_GCSTRESS=<bytes>
;;                            keep collecting at that interval forever.  A
;;                            copying collector's corruption shows at the
;;                            SECOND collection (the first leaves the old
;;                            semispace intact), so forcing dozens of cycles
;;                            out of a small workload is what makes survival
;;                            tests cheap.
;;   MODUS_I386_NO_CHECKED=1  disable the checked-arith slow paths, isolating
;;                            the overflow-promotion ops when triaging a
;;                            miscompile.
(let ((v (sb-ext:posix-getenv "MODUS_I386_GCSTRESS")))
  (when (and v (plusp (length v)))
    (setf modus.mvm.i386::*i386-gc-stress-limit* (parse-integer v))
    (format t "~&  GC stress limit: ~D bytes between collections~%"
            modus.mvm.i386::*i386-gc-stress-limit*)))
(let ((v (sb-ext:posix-getenv "MODUS_I386_VL")))
  (when (and v (plusp (length v)))
    (setf modus.mvm::*linux-i386-vl-offset* (parse-integer v))
    (format t "~&  VL offset overridden: ~D~%" modus.mvm::*linux-i386-vl-offset*)))
;; The NATIVE i386 Cheney collector is ON by default — the third arch arm,
;; alongside x64's emit-gc-trampoline and aarch64's native MCGC — with BOTH
;; conservative-root bitmaps (object-start + cons-kind) enabled.
(flet ((envp (n dflt) (let ((v (sb-ext:posix-getenv n)))
                        (if (and v (plusp (length v)))
                            (not (string= v "0"))
                            dflt))))
  (let ((gcon (envp "MODUS_I386_GC" t))
        (bmpon (envp "MODUS_I386_BMP" t)))
    (setf modus.mvm.i386::*i386-gc-bitmap-enabled* bmpon)
    (setf modus.mvm.i386::*i386-gc-enabled* gcon)
    (format t "~&  GC: collector=~A bitmap=~A~%" gcon bmpon)))
;; On a target this young a SILENT int3 is the most expensive possible failure
;; mode, so the unimplemented-opcode report is always collected and printed.
(setf modus.mvm.i386::*i386-record-unimpl* t)
(setf modus.mvm.i386::*i386-unimpl-ops* nil)

;;; The symmap follows the IMAGE path (or MODUS_I386_SYMMAP).  It used to be a
;;; fixed absolute path, which meant any build — including a verification build
;;; in a detached worktree pointed at its own MODUS_I386_OUT — silently
;;; overwrote the shared symmap of whatever else was there.
(defvar *i386-image-path*
  (or #+sbcl (sb-ext:posix-getenv "MODUS_I386_OUT")
      #+sbcl (sb-ext:posix-getenv "MODUS_CLI_OUT")
      "/tmp/modus-i386-cli"))
(setf *write-symmap-path*
      (or #+sbcl (sb-ext:posix-getenv "MODUS_I386_SYMMAP")
          (concatenate 'string *i386-image-path* ".symmap")))

(format t "~%Compiling i386 CLI image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-i386
                          :source-text cl-user::*full-source*)))
  (format t "  boot code:   ~D bytes~%" (length (kernel-image-boot-code image)))
  (format t "  native code: ~D bytes~%" (length (kernel-image-native-code image)))
  (format t "~%  checked-arith slow paths: GENADD=~A GENSUB=~A GENMUL=~A  (NIL = degrades to wrapping)~%"
          (and modus.mvm.i386::*i386-genadd-label* t)
          (and modus.mvm.i386::*i386-gensub-label* t)
          (and modus.mvm.i386::*i386-genmul-label* t))
  ;; Mechanized i386 register-invariant audit (see i386-check-eax-write).
  (let ((viol (modus.mvm.i386::i386-eax-invariant-report)))
    (if (null viol)
        (format t "~%  EAX/VR INVARIANT: clean (no opcode writes EAX with a non-VR dest).~%")
        (progn
          (format t "~%  *** EAX/VR INVARIANT VIOLATIONS (~D opcodes) ***~%" (length viol))
          (dolist (e viol) (format t "    ~A  x~D~%" (car e) (cdr e))))))
  (let ((report (modus.mvm.i386::i386-unimplemented-report)))
    (if (null report)
        (format t "~%  TRANSLATOR: no unimplemented opcodes.~%")
        (progn
          (format t "~%  *** TRANSLATOR GAPS (~D distinct) ***~%" (length report))
          (dolist (e report)
            (let ((k (car e)) (n (cdr e)))
              (if (>= k #x10000)
                  (format t "    trap #x~4,'0X  x~D~%" (- k #x10000) n)
                  (format t "    opcode #x~2,'0X  ~A  x~D~%"
                          k
                          (let ((info (gethash k *opcode-table*)))
                            (if info (opcode-info-name info) "?"))
                          n)))))))
  (let ((path *i386-image-path*))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    (when (sb-ext:posix-getenv "MODUS_DUMP_NATIVE")
      (with-open-file (o (concatenate 'string path ".native")
                         :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
        (write-sequence (kernel-image-native-code image) o)))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (when (string= path "/tmp/modus-i386-cli")
      (format t "~%NOTE: wrote the SHARED default path.  Set MODUS_I386_OUT for any~%      gate or comparison build — two agents building at once overwrite~%      each other.~%"))
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Run: qemu-i386-static ~A --eval EXPR --quit~%" path)))
