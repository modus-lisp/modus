

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
;; THE RAW I/O STAGING PAGE IS A FIXED BSS PAGE HERE, and saying so at this
;; seam is what makes it true for the WHOLE life of the image.  *IO-BUF-ADDR*
;; is a global, and a global is only as good as the last thing that wrote it:
;; kernel-main's shared boot text sets #x0FF00000 (build-cli-common.lisp's
;; known-dead pair), INIT-ALL-GLOBALS then re-runs cl-fileio.lisp's defvar
;; thunk and restores #x1DE00000, and only the arch io-scratch slot BELOW —
;; which lands after INIT-ALL-GLOBALS, deliberately — puts #x10014000 back.
;; On x64 all three are mapped BSS so the churn is invisible.  On this machine
;; #x0FF00000 is BELOW the modelled address space (mvm.js VBASE) and
;; #x1DE00000 is INSIDE the heap arena, so the two intermediate values are an
;; unmapped page and a live-heap page.  That is not theoretical: boot reaches
;; %INIT-MAKE-LOAD-FORM -> ... -> EVAL long before the slot runs, and
;; compile-variable-ref's implicit-global arm FORMATs a WARN to *ERROR-OUTPUT*
;; (see the bare-metal seam in build-cli-common.lisp, task #212 — the same
;; warning, the same early-boot window, one layer down), so %FS-WRITE-CHAR ->
;; %SYS-WRITE-BYTE-1 stored a byte at #x0FF00000 and faulted.
;; %FS-IO-PAGE is the seam cl-fileio.lisp names for exactly this question and
;; net/hosted-sync.lisp already overrides for per-CPU pages; the driver is the
;; LAST source block in the image, so this definition wins over both.  The web
;; target has no threads (%SPAWN-THREAD is unsupported), so one fixed page is
;; the whole answer, and it cannot be un-set by a defvar thunk or by boot order.
(defun %fs-io-page () #x10014000)
")

;; x86-64 needs no pre-init hardware setup: boot/boot-linux-x64.lisp has already
;; zeroed the runtime-metadata BSS slots by the time kernel-main runs, and the
;; MCGC object-start bitmap is reserved host-side.  WEB: pin the file-I/O
;; scratch addresses from the FIRST instruction of kernel-main, for the reason
;; spelled out at %FS-IO-PAGE above — the shared boot text's #x0FE00000 /
;; #x0FF00000 pair is unmapped here, and *CSTR-SCRATCH* is read as a bare
;; global (no seam to override), so the only place to be early enough is here.
(defvar *cli-arch-kernel-prologue*
"  (setq *cstr-scratch* #x10010000)
  (setq *io-buf-addr*  #x10014000)
  (setq *scratch-mmapped* t)
")

;; File-I/O scratch.  Both 64-bit ports park these just BELOW the heap base
;; (0x10000000), inside the ELF's own mapped BSS tail.  i386 cannot: its heap
;; is at 0x30000000 and 0x0FE00000 is unmapped there.
;; This slot lands AFTER (init-all-globals), which is why it is still needed
;; even though the prologue above has already set the same two addresses: the
;; defvar thunks restore cl-fileio.lisp's #x1DF00000 / #x1DE00000, and both of
;; those are inside this machine's heap arena.
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
;; NO HASH-TABLE OVERRIDE HERE ANY MORE, and the reason is worth the paragraph.
;; This slot used to carry a `%ht-hash (key strcmp?)' / `%ht-new-bucket-vec ()'
;; pair that pinned every bucket index at 4096 slots instead of prelude.lisp's
;; then-fixed 256 — a straight win on an interpreter.  fa2b77a (GETHASH was the
;; in-image wall) then made the index GROWABLE and changed both signatures:
;; %HT-HASH takes a MASK and %HT-NEW-BUCKET-VEC takes a bucket COUNT.  The
;; overrides kept the old arity, so `(%ht-new-bucket-vec n)' ignored n and
;; handed back 4096 buckets forever — and PUTHASH rebuilds the index whenever
;; `count > 2 * (array-length vec)', a condition a fixed 4096 can never clear.
;; Past 8192 entries every single insert therefore re-indexed the entire table:
;; the boot spent ~4 of its ~4.85 billion steps inside %HT-REBUILD-INDEX,
;; allocating ~45 MB of bucket vectors per 67M steps (12 collections of a
;; 256 MB semispace just to boot), and the tables that cross 8192 are exactly
;; the ones boot fills — *SYM-NAME-TABLE* alone holds ~9000 names.
;; Mainline now sizes the index to the next power of two at or above 2x the
;; entry count (min 256, max 65536), which is what this override was
;; hand-rolling at one fixed size, so deleting it is both the fix and the
;; feature.  A web-only copy of a shared algorithm is a liability across a
;; rebase; %GV-CELL's inlined copy of this same hash was the previous casualty.
"))

;; No BULK-MOVE / BULK-FILL: web/mvm.js models the x64 machine but does not
;; implement those two opcodes, so REPLACE and FILL keep their Lisp loops here.
;; NARROW STRINGS on the 32-bit module (the 68k image's MODUS_NARROW_STRINGS):
;; a byte per character (#x13) instead of a word, which web/mvm32.js's
;; collector sizes as bytes; no #x15 strings (their accessors assume 64-bit
;; words, compiler.lisp *STRING32-P*).  Pooled literals follow below
;; (*POOL-NARROW-STRINGS*).  MODUS_WEB_NARROW=0 keeps word strings.
(defvar cl-user::*web-narrow*
  (and (not (equal (sb-ext:posix-getenv "MODUS_WEB_WORD") "8"))     ; *WEB-WORD-SIZE*'s rule
       (let ((v (sb-ext:posix-getenv "MODUS_WEB_NARROW"))) (not (and v (string= v "0"))))))
(when cl-user::*web-narrow*
  (setq *cli-arch-override-source*
        (concatenate 'string *cli-arch-override-source*
                     (string #\Newline) "(defun %narrow-strings-p () t)"
                     (string #\Newline) "(defun %string32-strings-p () nil)" (string #\Newline))))

;; Bound before build-cli-common's DEFVAR, which then leaves it alone.
(defvar *cli-native-bulk-source* "")

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
            (aset vec i (%e2-const-ref i))
            (setq i (+ i 1))))
    (setq *web-jit-consts-synced* n)
    (setf (mem-ref #x10000D40 :u64) vec)))
;; LATE-BINDING THUNKS.  32 bytes of bytecode per callee NAME, in a JIT page:
;;   +0  FENCE FENCE                 mvm.js never translates a fn opening so
;;   +2  LI V5, #x10000178           \
;;   +12 LI V6, idx                   > the bridge index, out of band
;;   +22 STORE [V5], V6, :u32        /
;;   +26 TAILCALL %JIT-BRIDGE-ANY    funcalls NAME's current definition
;; A tail call keeps V0-V4, the stack arguments and the nargs slot, so any
;; arity passes through.  When NAME is installed native, +2 becomes a direct
;; TAILCALL to it (%WEB-THUNK-POINT); the thunk stays interpreted so the
;; rewrite reaches callers whose translated call sites cached it.
(defvar *web-thunks* nil)
(defvar *web-thunk-page* nil)
(defvar *web-thunk-bump* 4096)
(defun %web-thunk-word (addr) (logior (* (- addr #x10000000) 16) 3))
(defun %web-put-u32 (at v)
  (setf (mem-ref at :u8) (logand v 255))
  (setf (mem-ref (+ at 1) :u8) (logand (ash v -8) 255))
  (setf (mem-ref (+ at 2) :u8) (logand (ash v -16) 255))
  (setf (mem-ref (+ at 3) :u8) (logand (ash v -24) 255)))
(defun %web-put-tailcall (at word)
  (%web-put-u32 (+ at 1) (ash word -4))
  (setf (mem-ref at :u8) #x83))
(defun %web-late-thunk (key)
  (unless (hash-table-p *web-thunks*)
    (setq *web-thunks* (make-hash-table :test (function equal))))
  (or (gethash key *web-thunks*)
      (when (and key (%jit-bridge-ensure) (< *jit-bridge-count* (%jit-bridge-cap)))
        (when (> (+ *web-thunk-bump* 32) 4096)
          (let ((b (sap-address (make-sap (%mmap-exec-page 4096)))))
            (when (< b 4096)
              (return-from %web-late-thunk nil))
            (setq *web-thunk-page* b *web-thunk-bump* 0)))
        (let ((a (+ *web-thunk-page* *web-thunk-bump*))
              (idx *jit-bridge-count*))
          (setq *web-thunk-bump* (+ *web-thunk-bump* 32))
          (setf (aref *jit-bridge-names* idx) key)
          (setq *jit-bridge-count* (+ idx 1))
          (setf (mem-ref a :u8) #x72)
          (setf (mem-ref (+ a 1) :u8) #x72)
          (setf (mem-ref (+ a 2) :u8) #x11)
          (setf (mem-ref (+ a 3) :u8) 5)
          (%web-put-u32 (+ a 4) #x10000178)
          (%web-put-u32 (+ a 8) 0)
          (setf (mem-ref (+ a 12) :u8) #x11)
          (setf (mem-ref (+ a 13) :u8) 6)
          (%web-put-u32 (+ a 14) idx)
          (%web-put-u32 (+ a 18) 0)
          (setf (mem-ref (+ a 22) :u8) #x71)
          (setf (mem-ref (+ a 23) :u8) 5)
          (setf (mem-ref (+ a 24) :u8) 6)
          (setf (mem-ref (+ a 25) :u8) 2)
          (%web-put-tailcall (+ a 26) (%val->word (symbol-function (quote %jit-bridge-any))))
          (setf (gethash key *web-thunks*) a)
          a))))
;; NAME just went native as FN: send its thunk, if it has one, straight there.
(defun %web-thunk-point (key fn)
  (let ((a (and (hash-table-p *web-thunks*) (gethash key *web-thunks*))))
    (when a (%web-put-tailcall (+ a 2) (%val->word fn)))))
;; NAME was (re)defined or unbound (mvm-eval's redefinition hook and FMAKUNBOUND
;; call this with ADDR NIL; x64's version retargets a linkage stub): put its
;; thunk back to late binding -- +2 is LI V5 again, so a call funcalls whatever
;; NAME is now -- until the new definition goes native and %WEB-THUNK-POINT
;; sends it there.  Without it a translated caller kept calling the OLD code.
(defun %jit-x64-stub-retarget (name addr)
  (let ((a (and (null addr) (hash-table-p *web-thunks*) (gethash name *web-thunks*))))
    (when a
      (%web-put-u32 (+ a 4) #x10000178)
      (setf (mem-ref (+ a 3) :u8) 5)
      (setf (mem-ref (+ a 2) :u8) #x11))))
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
    (let* ((n (%rt-stub-count rt-table))
           (tab (make-array (if (< n 1) 1 n)))
           (ok t) (k 0) (native0 nil))
      (loop (when (>= k n) (return nil))
            (let* ((name (gethash (+ #x40000000 k) rt-table))
                   (fn (and name (%mvm-resolve-runtime-fn name)))
                   (word (if fn (%val->word fn) 0)))
              ;; A callee that is a runtime DEFUN not yet native (its
              ;; interpreter trampoline, tag 9): build its module now, as a hot
              ;; promotion would, and look again.  The native arms bridge such
              ;; a callee through a per-name thunk instead; this page holds
              ;; plain bytecode calls, so without this a lambda or method body
              ;; made before its callees went hot was refused and stayed
              ;; interpreted for good (82 us a call, measured).
              (when (and fn (not (eql (logand word 15) 3)) (not *jit-hot-busy*))
                (setq *jit-hot-busy* t)
                (unwind-protect
                     (%jit-hot-build (%mvm-runtime-key name) (%jit-skip-list) (cons nil 64))
                  (setq *jit-hot-busy* nil))
                (setq fn (%mvm-resolve-runtime-fn name)
                      word (if fn (%val->word fn) 0)))
              ;; Still not native (a heap closure: a trampoline, a GF): call it
              ;; late-bound through NAME's thunk, which funcalls whatever NAME
              ;; is at call time.  Without this, mutually recursive DEFUNs
              ;; (FRPOLY's PTIMES/PPLUS, BOYER's REWRITE/APPLY-SUBST) each
              ;; waited for the other to go native and all stayed interpreted.
              (setq native0 (and fn (eql (logand word 15) 3)))
              (when (and fn (not (eql (logand word 15) 3)))
                (let ((th (%web-late-thunk (%mvm-runtime-key name))))
                  (when th
                    (setq fn (%word->val (%web-thunk-word th))
                          word (%val->word fn)))))
              ;; A runtime DEFUN that IS native (a JIT page, at or above the web
              ;; VM's JIT_ADDR #x12400000; image functions are below): through its
              ;; thunk too, pointed straight at the code, so that redefining it
              ;; reaches this caller (%JIT-X64-STUB-RETARGET resets the thunk).
              ;; Baking the address here was the web's runtime-redefinition bug.
              ;; NATIVE0, not the tag now: a callee just swapped for its thunk is
              ;; a JIT-page function too, and pointing a thunk at itself loops.
              (when (and native0
                         (>= (+ (ash word -4) #x10000000) #x12400000))
                (let* ((key (%mvm-runtime-key name)) (th (%web-late-thunk key)))
                  (when th
                    (%web-thunk-point key fn)
                    (setq fn (%word->val (%web-thunk-word th))
                          word (%val->word fn)))))
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
;; NO CALL PADS.  The x64-target guard in mvm-eval.lisp's hook admits this image
;; (its JIT is on and its target arch is :X64), but the code it would patch is
;; bytecode the JS MVM runs, not x64 with KPAD prologues: reading the pad faulted,
;; and the fault escaped the DEFUN as a bare PROGRAM-ERROR -- so no DEFUN could
;; replace an image function (sb-gray's 31 %GRAY-WRAP wrappers: Gray streams, and
;; with them every ASDF load, failed in the browser).  A redefinition reaches what
;; looks the name up (FUNCALL, the REPL, translated pages); the image's own direct
;; callers keep the original, as on a read-only native image.
(defun %kernel-redirect-hook (name) nil)
(defun %jit-native-fn-at (addr)
  (%word->val (logior (* (- addr #x10000000) 16) 3)))
(defun %jit-install-native-fns (base fnoffs names)
  (let ((n 0))
    (dolist (e fnoffs)
      (let ((nm (car e)))
        (when (member nm names :test (function string=))
          (let ((fn (%jit-native-fn-at (+ base (cdr e)))))
            (when (boundp (quote *symbol-function-table*))
              (puthash nm *symbol-function-table* fn))
            (when (boundp (quote *native-sym-function-table*))
              (puthash (compute-name-hash nm) *native-sym-function-table* fn))
            (%web-thunk-point nm fn)
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

;;; The web module is compiled with the i386 TARGET -- 4-byte words, 30-bit
;;; fixnums, W32 opcodes -- for web/mvm32.js, which keeps one value in one int32
;;; instead of a (lo, hi) pair: runtime compile / FORMAT / consing ~2.3x faster
;;; than the x86-64 module on web/mvm.js.  MODUS_WEB_WORD=8 builds that one.  Only the compile target changes: the
;;; arch slots above (the host ABI: x86-64 syscall numbers, argv on the stack,
;;; stat/timespec layouts) stay, because the image reads those through
;;; promoting :u32 loads, which are exact on a 32-bit word.
(defvar *web-word-size*
  (let ((v (sb-ext:posix-getenv "MODUS_WEB_WORD")))
    (if (and v (string= v "8")) 8 4)))
(format t "~%Compiling web (~D-byte word) module (~D chars)...~%"
        *web-word-size* (length cl-user::*full-source*))
(let* ((target (find-target (if (= *web-word-size* 4) :i386 :x86-64)))
       (saved-ct *compile-target*))
  ;; BUILD-IMAGE binds the compile target the same way; the x86-64 module got
  ;; away without it because x86-64 is the default.
  (setq *compile-target* (set-target-fixnum-bits-for target))
  ;; The x86-64 TARGET turns on *TARGET-HAS-FPD-P* (unboxed double trees in
  ;; baked code), but this module runs on web/mvm.js, which has no FP register
  ;; file: the first FPD-UNBOX stopped module relocation and the image did not
  ;; boot.  The JS engine models x86-64's words and syscalls, not its FPU.
  (setf *target-has-fpd-p* nil)
  ;; The 68k builds' compiler-side options (build-68k-cli.lisp), each sound here:
  ;; - *CALL-TEMPS-SURVIVE*: every vreg mvm.js keeps is a slot in its own
  ;;   activation's frame (ROFF), so a callee cannot clobber the caller's live
  ;;   temps; no PUSH/POP around calls.  MODUS_WEB_CALL_SAVES=1 keeps them.
  ;; - *ELIDE-FIXED-NARGS*: a fixed-arity callee never reads the count, and a
  ;;   runtime redefinition does not reach the image's own callers here (the
  ;;   web %KERNEL-REDIRECT-HOOK).  MODUS_WEB_FIXED_NARGS=1 stores every count.
  ;; - *SHALLOW-THREAD-GATE* NIL: no threads on this target (%SPAWN-THREAD is
  ;;   unsupported), so the gate always fell through.  MODUS_WEB_THREAD_GATE=1.
  ;; - *TYPE-ERROR-CODES*, *STATIC-SYMBOL-SHORT-MISS*: smaller cold arms
  ;;   (MODUS_TYPE_ERROR_CODES=0 / MODUS_STATIC_SYMBOL_SHORT_MISS=0, as there).
  (flet ((env= (n v) (equal (sb-ext:posix-getenv n) v)))
    (setf *call-temps-survive* (not (env= "MODUS_WEB_CALL_SAVES" "1"))
          *elide-fixed-nargs* (not (env= "MODUS_WEB_FIXED_NARGS" "1"))
          *shallow-thread-gate* (env= "MODUS_WEB_THREAD_GATE" "1"))
    (unless (env= "MODUS_TYPE_ERROR_CODES" "0")
      (setf *type-error-codes* (make-hash-table :test 'equal)))
    (unless (env= "MODUS_STATIC_SYMBOL_SHORT_MISS" "0")
      (setf *static-symbol-short-miss* t))
    ;; - *ARITH-RANGE-BY-SHIFT* (the small 68k image's, build-cli-common): the
    ;;   30-bit range guard as SHL / SAR / CMP / BNE, not two immediates, two
    ;;   compares and two branches.  Sound only where a shift is 32 bits wide,
    ;;   which it is in web/mvm32.js, interpreted and translated alike; it is
    ;;   a build-time setting, so the image's own (Lisp-interpreted) runtime
    ;;   compiles keep the immediate form.  MODUS_WEB_RANGE_SHIFT=0 turns it off.
    (when (and (= *web-word-size* 4) (not (env= "MODUS_WEB_RANGE_SHIFT" "0")))
      (setf *arith-range-by-shift* t))
    (when cl-user::*web-narrow* (setf *pool-narrow-strings* t)))
  ;; The web image never calls BUILD-IMAGE, so build-checks' wrapper does not
  ;; see it: run the redefinition check around the compile directly.
  (setf *undeclared-redefinitions* nil)
  (clrhash *overridable-names*)
  (clrhash *gck-fallback-census*)
  (clrhash *gck-registered-keys*)
  (setf *global-uses* (make-hash-table :test 'equal))
  (let ((module (compile-source-to-module cl-user::*full-source*)))
    (check-undeclared-redefinitions)
    (check-cross-package-calls)
    (check-cross-package-globals)
    (web-write-module module target *web-image-path*))
  (setq *compile-target* saved-ct))
