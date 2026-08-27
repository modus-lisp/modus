;;;; build-generic-cli.lisp — the canonical HOSTED Modus image (`./modus').
;;;;
;;;; A native ELF you run as an ordinary Linux process; it drops into Modus's
;;;; own self-hosted CL REPL on stdin (eval = mvm-eval = compile -> MVM bytecode ->
;;;; interpret) and parses SBCL-style toplevel flags.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp # → ./modus
;;;;   ./modus
;;;;   > (load "modus-quicklisp/setup.lisp")   ; the "install quicklisp" step
;;;;   > (ql:quickload :sha1)                  ; loads systems/sha1.tar (offline)
;;;;   > (sha1:sha1-hex "abc")                 ; => "A9993E36...9CD0D89D"
;;;;
;;;; NO quicklisp is baked in — exactly like stock SBCL.  See QUICKLOAD.md.
;;;;
;;;; STRUCTURE.  This wrapper is a THIN ARCH TAIL.  Every byte of baked source
;;;; — the CL runtime bridge, the in-image MVM compiler, rtest, tar/asdf/genera,
;;;; cli-toplevel, kernel-main and the three auto-generated scanners — is
;;;; assembled by mvm/build-cli-common.lisp, which build-aarch64-cli.lisp loads
;;;; too.  What is left here is x86-64 hardware fact: the boot preamble, the
;;;; native-code offset, the x64 translator install and the x64 GC knobs.
;;;;
;;;; Adding a capability to the hosted CLI means editing build-cli-common.lisp,
;;;; which gives it to BOTH arches at once.  That is the point: the two lineages
;;;; used to be hand-maintained parallel copies, and every drift between them
;;;; was a bug found in production (task #245's missing (init-all-globals) left
;;;; 150 aarch64 globals unbound for months; RTEST was x64-only, so no library's
;;;; own test suite could run on arm at all).
;;;;
;;;; Output binary name is `modus' (override with MODUS_CLI_OUT).


;;; ============================================================
;;; ARCH SLOTS — x86-64
;;;
;;; Everything below is x86-64 hardware fact.  The shared source assembly lives
;;; in mvm/build-cli-common.lisp, which mvm/build-aarch64-cli.lisp loads too:
;;; add a CAPABILITY there (both arches get it), add a HARDWARE FACT here.
;;; ============================================================

(defvar *cli-arch* :x64)

;; exit_group is syscall 60 on x86-64 (93 on the AArch64 generic ABI).
(defvar *cli-arch-syscall-source*
"
(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))
(defun halt ()
  (syscall3 60 1 0 0))
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
;; BALANCED-CAP counters (all values < 2^32, so tagged :u32 loads are
;; exact).
(defun %hc-overflow () (mem-ref #x10000D20 :u32))
(defun %hc-capcount () (mem-ref #x10000D00 :u32))
;; Fault-stub scratch (RIP/RSP/[RSP]/RAX at 0x10000C30/C38/C40/C48) as
;; tagged u32 halves — values can exceed 32 bits, and interpreted :u64
;; reads are untrustworthy.  i = slot 0..3, j = 0 (lo) / 1 (hi).
(defun %hc-fault-w (i j) (mem-ref (+ #x10000C30 (* 8 i) (* 4 j)) :u32))
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

(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

;;; ============================================================
;;; 6. Build the image
;;; ============================================================

(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)
;; Boot preamble for linux-x64 ends 397 bytes into the file (ELF header
;; + entry stub).  Native code starts there, so the fn-entry alignment
;; loop must account for this offset — otherwise `:li-func` + OR-3 +
;; CALL-IND's sub-3 lands one byte before the prologue.  When the
;; preceding function's last byte happens to be RET (0xC3), the
;; misaligned call returns immediately, leaving the caller's RAX
;; intact (silently looks like the fn returned T or whatever else
;; was in RAX).  See reference_append_funcall_bug.md.
(setf modus.mvm.x64::*x64-native-code-offset* 397)

;; Enable the GC trampoline: without this, every :alloc-obj advances R12
;; unchecked and the heap walks past the mapped region in long-running
;; sessions.  build-x64-linux / build-x64 set this; we
;; need it too so the generic image survives ANSI sweeps.
(setf modus.mvm.x64::*x64-gc-enabled* t)
;; WS5 #223 / #278: this IMAGE's collector must scan the JIT constant-vector
;; root (BSS 0x10000F00), or a const the JIT mirrored into the vector dangles
;; after the first collection.  Gated on *jit-on* so a MODUS_NO_JIT build's
;; emitted collector is byte-identical to pre-#223.  The in-image translator
;; gets the matching setq from the %init-x64-translator co-init source
;; (build-cli-common.lisp), which only runs when JIT is on.
(when cl-user::*jit-on*
  (setf modus.mvm.x64::*x64-jit-constvec-p* t))
;; Linux-x64 layout: enable the CONS-KIND bitmap (GC correctness fix for the
;; cons-tagged-scratch symbol-truncation bug).  The kind-bitmap base delta is
;; a boot-linux-x64 layout constant, so the master flag is Linux-only for now.
(setf modus.mvm.x64::*mcgc-kind-bitmap-enabled* t)
;; Bring R14 to the heap midpoint so GC actually fires before the
;; from-space is exhausted.  Default leaves R14 at full heap end which
;; means the gc-check `cmp r12, r14; jl skip` only triggers after
;; allocation walked all the way to the end — too late for a Cheney
;; copy that needs the other half free.
(setf modus.mvm::*linux-x64-r14-offset* modus.mvm::+linux-x64-gc-midpoint+)
;; MCGC page-pinning test knob (stage 4).  OFF by default.  MODUS_MCGC_PINNING=1
;; enables the page-pool allocator + page collector for a pinning test build.
#+sbcl
(when (let ((v (sb-ext:posix-getenv "MODUS_MCGC_PINNING")))
        (and v (plusp (length v)) (not (string= v "0"))))
  (setf modus.mvm.x64::*mcgc-pinning-enabled* t)
  (format t "~&;; MCGC PAGE-PINNING ENABLED (test build)~%"))
;; Test knob: MODUS_MCGC_TORUN_CAP=<pages> caps each to-run segment so the
;; copy_object refill / to-run-chain path is exercised on ordinary workloads.
#+sbcl
(let ((cap (sb-ext:posix-getenv "MODUS_MCGC_TORUN_CAP")))
  (when (and cap (> (length cap) 0))
    (setf modus.mvm.x64::*mcgc-torun-cap-pages* (parse-integer cap))
    (format t "~&;; MCGC TO-RUN SEGMENT CAP = ~D pages (refill stress)~%"
            modus.mvm.x64::*mcgc-torun-cap-pages*)))
;; Debug knob: MODUS_GC_R14=<hex-or-dec bytes> forces R14 to a small offset
;; so GC fires early (fast repro of GC-from-runtime-EVAL faults).  Leaves
;; the from/to semispaces 448MB apart (unchanged), only moves the trigger.
#+sbcl
(let ((dbg (sb-ext:posix-getenv "MODUS_GC_R14")))
  (when (and dbg (> (length dbg) 0))
    (let ((v (parse-integer dbg :radix (if (and (> (length dbg) 1)
                                                (char= (char dbg 0) #\#))
                                           16 10)
                            :start (if (char= (char dbg 0) #\#) 1 0))))
      (setf modus.mvm::*linux-x64-r14-offset* v)
      (format t "~%[DEBUG] R14 offset forced to ~X (GC fires early)~%" v))))
#+sbcl
(when (let ((d (sb-ext:posix-getenv "MODUS_GC_DEBUG"))) (and d (> (length d) 0)))
  (setf modus.mvm.x64::*x64-gc-debug* t)
  (format t "~%[DEBUG] GC trampoline debug bytes enabled~%"))
;; A/B knob: MODUS_MCGC_KINDCHECK=0 keeps the cons-kind bitmap SET side
;; (image layout ~unchanged) but DISABLES the scan_word reject, to prove the
;; CHECK — not incidental layout shift — restores correctness.  Default on.
#+sbcl
(let ((kc (sb-ext:posix-getenv "MODUS_MCGC_KINDCHECK")))
  (when (and kc (string= kc "0"))
    (setf modus.mvm.x64::*mcgc-kind-check-enabled* nil)
    (format t "~%[DEBUG] MCGC cons-kind CHECK disabled (set side still on)~%")))

#+sbcl
(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf modus.mvm::*write-symmap-path* sm)))

(format t "~%Compiling generic image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64
                          :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_CLI_OUT") "modus")))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    ;; WS5 GATE AID: also dump the NATIVE CODE section on its own.  The full
    ;; image embeds a source blob and an auto-generated symbol-name table,
    ;; both of which track the source TEXT, so a pure refactor that renames a
    ;; literal to a constant changes the image without changing a single
    ;; emitted instruction.  Hashing the native code separates "the compiler
    ;; emitted the same code" from "the file happens to be the same size".
    (when (sb-ext:posix-getenv "MODUS_DUMP_NATIVE")
      (with-open-file (o (concatenate 'string path ".native")
                         :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
        (write-sequence (kernel-image-native-code image) o))
      (format t "  native code: ~D bytes -> ~A.native~%"
              (length (kernel-image-native-code image)) path))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Usage: ~A <script.lisp>~%" path)))
