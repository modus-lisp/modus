

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
(defvar *cli-arch-io-scratch-source*
";; WEB: the x64 defaults (#x1DF00000 / #x1DE00000) fall inside the JS machine's
;; heap arena.  Park the C-string scratch and the 4 KB I/O page in the BSS
;; block, which is mapped, GC-scanned harmlessly, and below every heap address.
  (setq *cstr-scratch* #x10010000)
  (setq *io-buf-addr*  #x10014000)
  (setq *scratch-mmapped* t)
")

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
;;; MODUS_NO_JIT=1 keeps the x64 native translator out of the image; the web
;;; JIT arm (see the override slot above) then runs eval'd bytecode directly
;;; on the JS interpreter instead of through mvm-interpret.

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
;;; *jit-boot-source* is assembled LAST so its %jit-enabled-p wins; for the web
;;; target the JIT is on (the arm is the interpreter itself), so win once more.
;;; The web JIT arm.  It REPLACES mvm-eval.lisp's %jit-translate-page-1 and
;;; %jit-install-native-fns, so it must come after them (last-defun-wins) — the
;;; arch override slot is compiled before stage 2, so it goes here instead.
(setq cl-user::*full-source*
      (concatenate 'string cl-user::*full-source* "
;; ---- the web JIT arm -------------------------------------------------
;; The x64 JIT translates an eval'd module to native code in an exec page.
;; Here the JS interpreter executes MVM bytecode directly, so the page IS the
;; bytecode: copy it in, hand the JS side a table of resolved out-of-module
;; callees (BSS 0x10000D48) and the quote-constant vector (0x10000D40), and let
;; %jit-icache-flush relocate the page in place.  Constants are read through
;; the vector at run time, so nothing bakes a heap address and no GC gate is
;; needed.  Function values are (phys-index << 4) | 3, phys = addr - 0x10000000.
(defun %jit-enabled-p () t)
(defvar *web-jit-consts* nil)
(defvar *web-jit-consts-synced* nil)
(defun %web-sync-consts ()
  (let ((n (if (boundp (quote *e2-const-count*)) *e2-const-count* 0))
        (vec *web-jit-consts*)
        (synced (if *web-jit-consts-synced* *web-jit-consts-synced* 0)))
    (when (or (null vec) (> n (array-length vec)))
      (let ((new (make-array (if (< (* 2 n) 256) 256 (* 2 n)))) (i 0))
        (when vec
          (loop (when (>= i (array-length vec)) (return nil))
                (aset new i (aref vec i))
                (setq i (+ i 1))))
        (setq vec new)
        (setq *web-jit-consts* new)))
    (let ((i synced))
      (loop (when (>= i n) (return nil))
            (aset vec i (gethash i *e2-const-pool*))
            (setq i (+ i 1))))
    (setq *web-jit-consts-synced* n)
    (setf (mem-ref #x10000D40 :u64) vec)))
(defun %jit-translate-page-1 (bc ft-list rt-table)
  (let* ((nlen (length bc))
         (psize (+ nlen 4096))
         (page (%mmap-exec-page psize))
         (base (sap-address (make-sap page)))
         (eoff nil) (fnoffs nil))
    (when (< base 4096)
      (setq *jit-r-mmap-fail* (if *jit-r-mmap-fail* (+ 1 *jit-r-mmap-fail*) 1))
      (return-from %jit-translate-page-1 nil))
    (let ((k 0))
      (loop (when (>= k nlen) (return nil))
            (setf (mem-ref (+ base k) :u8) (aref bc k))
            (setq k (+ k 1))))
    (let* ((n (hash-table-count rt-table))
           (tab (make-array (if (< n 1) 1 n)))
           (ok t) (k 0))
      (loop (when (>= k n) (return nil))
            (let* ((name (gethash (+ #x40000000 k) rt-table))
                   (fn (and name (%mvm-resolve-runtime-fn name)))
                   (word (if fn (%val->word fn) 0)))
              (if (eql (logand word 15) 3)
                  (aset tab k fn)
                  (progn
                    (if fn
                        (setq *jit-r-reloc-call-nonnative*
                              (if *jit-r-reloc-call-nonnative* (+ 1 *jit-r-reloc-call-nonnative*) 1))
                        (setq *jit-r-reloc-call-unresolved*
                              (if *jit-r-reloc-call-unresolved* (+ 1 *jit-r-reloc-call-unresolved*) 1)))
                    (setq ok nil))))
            (setq k (+ k 1)))
      (unless ok (return-from %jit-translate-page-1 nil))
      (setf (mem-ref #x10000D48 :u64) tab))
    (%web-sync-consts)
    (%jit-icache-flush base nlen)
    (unless (eql (mem-ref #x10000D50 :u64) 0)
      (return-from %jit-translate-page-1 nil))
    (dolist (e ft-list)
      (let ((nm (car e)) (off (cadr e)))
        (when (string-equal nm \"%MVM-EVAL-THUNK\") (setq eoff off))
        (setq fnoffs (cons (cons nm off) fnoffs))))
    (list base (if eoff eoff 0) nil (%gc-count) psize (reverse fnoffs))))
;; ---- web networking -------------------------------------------------------
;; The browser has no sockets.  Name resolution goes to the host through a
;; private syscall number (4242: host string -> a fake IPv4 the host remembers),
;; and the socket calls that follow (41 socket, 42 connect, 1 write, 0 read,
;; 3 close) are answered by the host as an HTTP fetch of the request it sees.
(defun %host-string-to-ip (host)
  (%string-to-cstr host *cstr-scratch*)
  (syscall3 4242 *cstr-scratch* (length host) 0))
(defun %jit-install-native-fns (base fnoffs names)
  (let ((n 0))
    (dolist (e fnoffs)
      (let ((nm (car e)))
        (when (member nm names :test (function string=))
          (let ((fn (%word->val (logior (* (- (+ base (cdr e)) #x10000000) 16) 3))))
            (when (boundp (quote *symbol-function-table*))
              (puthash nm *symbol-function-table* fn))
            (when (boundp (quote *native-sym-function-table*))
              (puthash (compute-name-hash nm) *native-sym-function-table* fn))
            (setq n (+ n 1))))))
    (setq *jit-native-defun-count*
          (if *jit-native-defun-count* (+ *jit-native-defun-count* n) n))
    n))
"))
(setq cl-user::*full-source*
      (concatenate 'string cl-user::*full-source* "
;; MOTD: list the interfaces this image exposes.  From the image's side the
;; host is opaque, so name it for what it is - a generic JavaScript bridge.
;; (Overrides cli-toplevel's one-line banner; last-defun-wins.)
(defun %cli-repl (banner-p)
  (when banner-p
    (%cli-line \"Modus - Common Lisp, self-hosted on the MVM.\")
    (%cli-line \"I/O is a generic JavaScript host bridge (no OS underneath):\")
    (%cli-line \"  console  - this REPL, standard input and output over the bridge\")
    (%cli-line \"  files    - open / with-open-file / load, on the bridge filesystem\")
    (%cli-line \"  network  - (http-get \\\"https://host/path\\\") and the socket layer, over bridge HTTP\")
    (%cli-line \"Ctrl-D (EOF) to exit.\"))
  (let ((in (%make-file-stream-full 0 0))
        (eof (list 'eof))
        (rerr (list 'rerr)))
    (loop
      (%cli-msg \"> \")
      (let ((form (handler-case (read in nil eof)
                    (t (c) rerr))))
        (cond
          ;; A read error (bad token, unknown package like `foo:bar' before FOO
          ;; exists, unbalanced input) must NOT end the session — it used to
          ;; return EOF and fall through to the exit below, so one typo killed
          ;; the REPL.  Report it, drop the rest of the line so the next prompt
          ;; starts clean instead of re-reading the garbage, and keep going.
          ((eq form rerr)
           (%cli-line \"READ-ERROR\")
           (read-line in nil eof))   ; drop the rest of the offending line
          ((eq form eof) (%cli-nl) (return-from %cli-repl nil))
          (t (handler-case
                 (let ((v (eval form)))
                   (write-object v) (%cli-nl))
               (t (c)
                 (%cli-msg \"ERROR: \")
                 (handler-case (write-object c) (t (c2) (%cli-msg \"<condition>\")))
                 (%cli-nl)))))))))
"))
;; ---- introspection RPC: only a dispatch STUB is baked ----
;; The RPC's logic (completion / apropos / describe) is STRIPPED to
;; web/kernel/web-rpc.lisp, which the page SHA-256-verifies and LOADs at boot
;; (a step toward a minimal kernel that admits the rest of the system one
;; verified file at a time).  This stub stays baked so the host's re-entrant
;; call resolves %WEB-RPC via the module table whether or not the logic has been
;; admitted; the loaded file sets *web-rpc-fn* to the real implementation.
(setq cl-user::*full-source*
      (concatenate 'string cl-user::*full-source* "
(defvar *web-rpc-fn* nil)
(defun %web-rpc () (when *web-rpc-fn* (funcall *web-rpc-fn*)))
"))

(format t "~%Compiling web (x86-64 word) module (~D chars)...~%"
        (length cl-user::*full-source*))
(let ((target (find-target :x86-64)))
  (set-target-fixnum-bits-for target)
  (let ((module (compile-source-to-module cl-user::*full-source*)))
    (web-write-module module target *web-image-path*)))
