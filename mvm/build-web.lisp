

;;; ============================================================
;;; ARCH SLOTS — x86-64
;;;
;;; Everything below is x86-64 hardware fact.  The shared source assembly lives
;;; in mvm/build-cli-common.lisp, which mvm/build-aarch64-cli.lisp loads too:
;;; add a CAPABILITY there (both arches get it), add a HARDWARE FACT here.
;;; ============================================================

(defvar *cli-arch* :x64)

;; ABI FACT, and it was WRONG here until the image grew threads: on x86-64
;; syscall 60 is `exit', which ends ONLY THE CALLING THREAD; `exit_group', the
;; one that ends the process, is 231.  (93 vs 94 on the AArch64 generic ABI,
;; 1 vs 252 on i386.)  In a single-threaded image the two are indistinguishable,
;; which is why this stood for so long.
;;
;; With a second thread alive the difference is a HANG.  The main thread exits
;; through `exit', the kernel leaves the group leader as an unreapable ZOMBIE
;; because the thread group is not empty, the second thread stays parked in
;; futex_wait with nobody left to wake it, and any parent reading this process's
;; output through a pipe never sees EOF.  That is the whole of the
;; test/hosted-thread-lisp.lisp "flake": a fault on the main thread turned into
;; a silent infinite hang instead of a report.  See test/run-thread-exit.sh.
(defvar *cli-arch-syscall-source*
"
(defun sys-exit (code)
  (let ((c code))
    (syscall3 231 c 0 0)))
(defun halt ()
  (syscall3 231 1 0 0))
")

;; argv/argc off the fixed BSS slots the x64 boot preamble publishes, plus
;; native probes for the handler-frame chain.  These must be NATIVE fns: an
;; INTERPRETED mem-ref only sees the interpreter's simulated per-state memory
;; hash, so only compiled code can observe the real RAM at #x10000400 (handler
;; stack depth) and #x10000180 (current armed frame RSP).
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
;; Native diagnostic probes for the handler-frame chain (real RAM — an
;; interpreted mem-ref only sees the interp's per-state simulated memory
;; hash, so scripts must call these NATIVE fns to observe [#x10000400]
;; (handler-stack depth) and [#x10000180] (current armed frame RSP)).
(defun %hc-depth () (mem-ref #x10000400 :u32))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u32) 0) nil t))
;; Saved resume-IP (low 32 bits) of stacked frame N / the current frame —
;; code addrs are < 4GB so :u32 (tagged load) is exact.
(defun %hc-frame-ip (n) (mem-ref (+ #x10000408 (* 32 n) 16) :u32))
(defun %hc-cur-ip () (mem-ref #x10000190 :u32))
")

;; x86-64 needs no pre-init hardware setup: boot/boot-linux-x64.lisp has already
;; zeroed the runtime-metadata BSS slots by the time kernel-main runs, and the
;; MCGC object-start bitmap is reserved host-side.
(defvar *cli-arch-kernel-prologue* "")

;; File-I/O scratch.  Both 64-bit ports park these just BELOW the heap base
;; (0x10000000), inside the ELF's own mapped BSS tail.  i386 cannot: its heap
;; is at 0x30000000 and 0x0FE00000 is unmapped there.
(defvar *cli-arch-io-scratch-source* "")

;; x86-64 toplevel entry.  No baked probe program — this is a shipping CLI.
(defvar *cli-arch-kernel-epilogue*
"  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  ;; cli-toplevel reads the FULL argv off the initial stack, parses SBCL-style
  ;; flags left-to-right (--eval/--load/--script/--quit/--version/--help/rc/
  ;; --end-toplevel-options), loads ~/.modusrc before an interactive REPL, and
  ;; either runs the REPL or exits.  It never returns (exits via sys-exit); the
  ;; outer handler-case is belt-and-suspenders in case of a parse-path crash.
  (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
")

;; No late arch overrides on x86-64: cl-fileio.lisp's syscall numbers and
;; lib/cli-toplevel.lisp's %cli-argv-base are already the x86-64 forms.
(defvar *cli-arch-override-source* "")

;;; WEB-ONLY OVERRIDES, appended to the x64 override slot (last-defun-wins).
;;; Not semantics changes — the same answers, on the interpreter's cost model:
;;; a non-EQ `eql' fall-through and a hash-table bucket walk are where the
;;; boot and every REPL form spend most of their steps.
(setq *cli-arch-override-source*
      (concatenate 'string *cli-arch-override-source* "
;; EQL: exit early for the common non-numeric cases instead of running four
;; predicate chains.
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
;; Hash tables: 4096 buckets instead of 256 (the two functions that agree on
;; the bucket count).
(defun %ht-hash (key strcmp?)
  (cond
    ((stringp key)
     (if strcmp?
         (let ((h 2166136261) (len (array-length key)) (i 0))
           (loop
             (when (>= i len) (return nil))
             (setq h (logand (* (logxor h (%prim-aref key i)) 16777619) #xFFFFFFFF))
             (setq i (+ i 1)))
           (logand (logxor h (ash h -12)) 4095))
         (%ht-nohash)))
    ((fixnump key)  (logand (logxor key (ash key -12)) 4095))
    ((characterp key) (logand (char-code key) 4095))
    ((null key) 17)
    ((eq key t) 19)
    ((%cl-sym-p key) (logand (%cl-sym-hash key) 4095))
    ((%native-mvm-sym-p key) (logand (%native-mvm-sym-hash key) 4095))
    (t (%ht-nohash))))
(defun %ht-new-bucket-vec ()
  (let ((vec (make-array 4096)) (i 0))
    (loop
      (when (>= i 4096) (return vec))
      (%ht-vec-set vec i nil)
      (setq i (+ i 1)))))
"))

(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

;;; ============================================================
;;; WEB TARGET — dump the MVM module for the JavaScript interpreter
;;; ============================================================
;;;
;;; Everything above this line is byte-for-byte the x86-64 CLI wrapper's arch
;;; slots: the JS interpreter (web/mvm.js) models the HOSTED x86-64 machine —
;;; 64-bit words, the Linux x86-64 syscall numbers, argv on the initial stack
;;; — so the compiled runtime must be the x64 one.  Instead of translating to
;;; native code we stop after compile-source-to-module and serialise:
;;;
;;;   magic "MVMW" | version u32 | word-size u32
;;;   bytecode:     len u32, bytes
;;;   fn table:     count u32, then per fn: name-hash u32, param-count u32,
;;;                 bytecode-offset u32, bytecode-length u32,
;;;                 name-len u16, name bytes (ASCII)
;;;   const pool:   count u32, addr-table u32 x count (pool-relative tagged
;;;                 offsets, see cross.lisp build-constant-pool), pool-len u32,
;;;                 pool bytes
;;;
;;;   MODUS_NO_JIT=1 sbcl --dynamic-space-size 8192 --script mvm/build-web.lisp
;;;   -> web/modus.mvmw  (override with MODUS_WEB_OUT)
;;;
;;; MODUS_NO_JIT=1 is required: the runtime JIT needs executable pages and
;;; native calls, which the interpreter does not provide; with it off
;;; mvm-eval falls back to mvm-interpret, which is pure Lisp.

(in-package :modus.mvm)

(defvar *web-image-path*
  (or #+sbcl (sb-ext:posix-getenv "MODUS_WEB_OUT")
      (namestring (merge-pathnames "web/modus.mvmw" cl-user::*modus-base*))))

(defun web-write-module (module target path)
  (let ((out (make-mvm-buffer))
        (bc (mvm-module-bytecode module))
        (fns (mvm-module-function-table module)))
    (flet ((u32 (v) (mvm-emit-u32 out (logand v #xFFFFFFFF)))
           (u16 (v) (mvm-emit-u16 out v))
           (bytes (v) (loop for b across v do (mvm-emit-byte out b))))
      (bytes (map 'vector #'char-code "MVMW"))
      (u32 1) (u32 (target-word-size target))
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
      (multiple-value-bind (pool addr-table) (build-constant-pool module target)
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

(unless (null cl-user::*jit-on*)
  (error "build-web: run with MODUS_NO_JIT=1 — the web target has no runtime JIT"))
(format t "~%Compiling web (x86-64 word) module (~D chars)...~%"
        (length cl-user::*full-source*))
(let ((target (find-target :x86-64)))
  (set-target-fixnum-bits-for target)
  (let ((module (compile-source-to-module cl-user::*full-source*)))
    (web-write-module module target *web-image-path*)))
