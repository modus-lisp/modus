;;;; build-i386-cli.lisp — WS5: the CL / mvm-eval image for hosted Linux i386.
;;;;
;;;; Counterpart of mvm/build-aarch64-cli.lisp, but for a 32-bit target.  Today
;;;; i386 runs only mvm/repl-source.lisp — a 708-line toy Lisp with its own
;;;; reader/printer/evaluator.  The real CL stack (prelude + gc + rt + the CL
;;;; bridge + the MVM compiler + mvm-eval) has only ever run on x64 and
;;;; aarch64.  This is the vehicle for getting it onto a 32-bit word.
;;;;
;;;;   sbcl --dynamic-space-size 8192 --script mvm/build-i386-cli.lisp
;;;;   qemu-i386-static /home/claude/ws5-gate-out/modus-i386-cli <probe>
;;;;
;;;; MODUS_I386_LAYER selects how much of the stack is baked in.  Bringing a
;;;; new word size up one layer at a time is the whole point: each layer is a
;;;; smaller, attributable failure than "the 200k-line image doesn't boot".
;;;;   1  prelude only                       (cons/arith/string primitives)
;;;;   2  + gc + rt                          (runtime support)
;;;;   3  + the CL bridge (cl-*.lisp)        (packages/streams/reader/printer)
;;;;   4  + the unforked net/crypto.lisp   (SHA-256 / ChaCha20)
;;;   5  + MVM ISA + interp + compiler + mvm-eval   (so EVAL evaluates)
;;;; Default is 1 while the port is being brought up.
;;;;
;;;; The build ALWAYS prints the translator's unimplemented-opcode report
;;;; (see *i386-record-unimpl*) before writing the image: on a new target a
;;;; silent INT3 is the most expensive possible failure mode.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

;; NB: load the boot file BEFORE switching packages — inside :modus.mvm the
;; name MVM-LOAD is the 4-argument MVM ISA emitter, not the file loader.
(mvm-load "boot/boot-linux-i386.lisp")

(defparameter *i386-layer*
  (let ((v (sb-ext:posix-getenv "MODUS_I386_LAYER")))
    (if (and v (plusp (length v))) (parse-integer v) 1)))

(format t "~%=== Building hosted Linux/i386 CL image (layer ~D) ===~%" *i386-layer*)

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    (read-file-text path)))

;;; Strip (in-package ...) forms — the image reader is flat-namespace.
(defun cli-strip-in-package (text)
  (let ((result text))
    (loop
      (let ((pos (search "(in-package " result)))
        (unless pos (return result))
        (let ((end (position #\) result :start pos)))
          (if end
              (setf result (concatenate 'string
                                        (subseq result 0 pos)
                                        (subseq result (1+ end))))
              (return result)))))))

(format t "Reading source files...~%")

(defvar *prelude-source*
  (if (>= *i386-layer* 1)
      (cli-strip-in-package (mvm-text "mvm/prelude.lisp"))
      ""))
(defvar *gc-source*      (if (>= *i386-layer* 2) (mvm-text "mvm/gc.lisp") ""))
(defvar *rt-source*      (if (>= *i386-layer* 2)
                             (cli-strip-in-package (mvm-text "mvm/rt.lisp")) ""))

;; Layer 3: the CL bridge, in the SAME order build-generic-cli.lisp uses.
;; (Order matters — later files override earlier defuns, last-defun-wins.)
(defvar *bridge-source*
  (if (>= *i386-layer* 3)
      (apply #'concatenate 'string
             ;; ansi-bridge.lisp is LAST, exactly as build-generic-cli.lisp
             ;; orders it.  It is not only test scaffolding: it holds the
             ;; runtime MAKE-ARRAY defun, and compile-make-array routes every
             ;; kwarg-bearing call — including the
             ;; `(make-array n :element-type '(unsigned-byte 8))` in
             ;; mvm-buffer-used-bytes, i.e. every mvm-eval — to
             ;; `(funcall #'make-array …)`.  Without the file that funcall
             ;; produced NIL, and the very next `(setf (aref result i) …)`
             ;; ran OBJ-SUBTAG on NIL: SIGSEGV inside MVM-BUFFER-USED-BYTES,
             ;; two frames under (eval 42).
             (loop for f in '("mvm/cl-sequences.lisp" "mvm/cl-streams.lisp"
                              "mvm/cl-fileio.lisp"    "mvm/cl-printer.lisp"
                              "mvm/cl-reader.lisp"    "mvm/cl-eval.lisp"
                              "mvm/cl-clos.lisp"      "mvm/cl-types.lisp"
                              "mvm/cl-packages.lisp"  "mvm/cl-conditions.lisp"
                              "mvm/ansi-bridge.lisp")
                   append (list (cli-strip-in-package (mvm-text f))
                                (string #\Newline))))
      ""))

;;; Layer 4: the UNFORKED net/crypto.lisp.  NOT crypto-32 / crypto-w32 /
;;; crypto-fast / crypto-32-fast / crypto-mvm-split — the base 2018-line file,
;;; verbatim.  The only extra source is ARCH GLUE, which is exactly what
;;; net/arch-*.lisp supplies for every other target: e1000-state-base (a
;;; scratch region for the K constants) and buf-read/write-u32, copied
;;; VERBATIM from net/ip.lisp (the canonical unforked definitions — note the
;;; forks in 32bit-overrides.lisp / crypto-mvm-split.lisp are NOT used).
(defvar *crypto-glue-source* "
(defun e1000-state-base () 268438528)   ; #x10000C00, inside the hosted BSS
(defun buf-write-u32 (buf off val)
  (aset buf off (logand (ash val -24) 255))
  (aset buf (+ off 1) (logand (ash val -16) 255))
  (aset buf (+ off 2) (logand (ash val -8) 255))
  (aset buf (+ off 3) (logand val 255)))
(defun buf-read-u32 (buf off)
  (let ((b0 (aref buf off)))
    (let ((b1 (aref buf (+ off 1))))
      (let ((b2 (aref buf (+ off 2))))
        (let ((b3 (aref buf (+ off 3))))
          (let ((hi (ash b0 24)))
            (let ((mid (ash b1 16)))
              (let ((lo (ash b2 8)))
                (let ((hm (logior hi mid)))
                  (let ((hml (logior hm lo)))
                    (logior hml b3)))))))))))
")


;;; ============================================================
;;; Layer 5: the MVM compiler + interpreter (so EVAL actually evaluates)
;;; ============================================================
;;; Layers 1-4 give the CL bridge, which DEFINES eval/load/read — but Modus's
;;; EVAL is mvm-eval: compile the form to MVM bytecode with the self-hosted
;;; compiler, then run mvm-interpret.  Without these four files EVAL is
;;; compiled in with an unresolved callee, so lib/cli-toplevel.lisp's
;;; --eval/--load/REPL cannot work at all.  Verified by symmap before this
;;; existed: EVAL/LOAD/READ present, MVM-EVAL/COMPILE-SOURCE-TO-MODULE/
;;; MVM-INTERPRET absent.
;;;
;;; Order and contents mirror build-generic-cli.lisp, which bakes them
;;; directly (build-aarch64-cli gets them via the aarch64 common file).  NO JIT
;;; sources: mvm-eval's JIT seam falls back to mvm-interpret when the
;;; translator is absent, which is exactly what a pure-interpret image wants.
(defvar *isa-source*      (if (>= *i386-layer* 5) (mvm-text "mvm/mvm.lisp") ""))
(defvar *interp-source*   (if (>= *i386-layer* 5) (mvm-text "mvm/interp.lisp") ""))
(defvar *compiler-source* (if (>= *i386-layer* 5) (mvm-text "mvm/compiler.lisp") ""))
(defvar *mvm-eval-source* (if (>= *i386-layer* 5) (mvm-text "mvm/mvm-eval.lisp") ""))

;;; The runtime CL macros (when/unless/setf/incf/case/dolist/...): source
;;; strings in *modus-runtime-macros*, installed at boot by
;;; %install-runtime-cl-macros.  Same file both 64-bit CLIs bake.
(defvar *rt-macros-source*
  (if (>= *i386-layer* 5) (mvm-text "mvm/runtime-cl-macros.lisp") ""))

;;; The LOAD-TIME backquote expander (WS5 #203 gap 5).  Without it a macro
;;; defined at RUNTIME keeps the reader's COMMA markers in its expansion and
;;; the first call dies with UNDEFINED-FUNCTION NAME="COMMA".
;;;
;;; MUST be fed to BOTH auto-scanners below — the SFT one so runtime EVAL can
;;; resolve its defuns by name, and (critically) the sym-name one, because
;;; %rbq-sym-name-eq dispatches on (symbol-name sym) being "COMMA" /
;;; "COMMA-AT" / "BACKQUOTE".  A missing sym-name entry makes symbol-name
;;; return "" and the expander degrades to a SILENT NO-OP — it does not
;;; error, so nothing downstream tells you the bootstrap only half happened.
(defvar *runtime-backquote-source*
  (if (>= *i386-layer* 5) (mvm-text "lib/runtime-backquote.lisp") ""))

;;; %init-runtime-macros — mark every compiler macro name as KNOWN in
;;; *macro-table*, so macroexpand-1 at runtime answers "yes, that is a macro".
;;; Generated exactly the way build-generic-cli.lisp generates it (scan
;;; compiler.lisp for MVM-DEFINE-MACRO names).
(defun scan-mvm-define-macro-forms (text)
  (let ((names nil) (pos 0))
    (loop
      (let ((p (search "(mvm-define-macro \"" text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p (length "(mvm-define-macro \"")))
               (end (position #\" text :start start)))
          (push (subseq text start end) names)
          (setq pos (1+ end)))))))

(defvar *macro-names*
  (if (>= *i386-layer* 5) (scan-mvm-define-macro-forms *compiler-source*) nil))

(defvar *runtime-macros-source*
  (if (>= *i386-layer* 5)
      (with-output-to-string (out)
        (format out "(defun %init-runtime-macros ()~%")
        (dolist (name *macro-names*)
          (format out "  (puthash (compute-name-hash ~S) *macro-table* t)~%" name))
        (format out ")~%"))
      "(defun %init-runtime-macros () nil)
"))

;;; ============================================================
;;; Linux/i386 file-I/O syscall overrides
;;; ============================================================
;;; mvm/cl-fileio.lisp hardcodes x86-64 syscall numbers (open=2, close=3,
;;; read=0, write=1, lseek=8, stat=4, fstat=5, unlink=87, rename=82,
;;; mkdir=83, getpid=39, getdents64=217).  i386 shares almost none of them.
;;; Same class build-aarch64-cli.lisp fixed for the *at-only AArch64 ABI
;;; (be1aef1): without these overrides LOAD, OPEN and every path predicate
;;; are silently dead on this image.  Concatenated AFTER *bridge-source*, so
;;; last-defun-wins makes every call site resolve here.
;;;
;;; Addresses are passed as TAGGED fixnums and untagged by trap #x0502's SAR,
;;; so every scratch address must be below 2^30 — see +linux-i386-bss-end+.
;;;
;;; Two deliberate departures from a literal transliteration:
;;;   * EXISTENCE goes through access(2) (33), not stat.  It is the only
;;;     thing OPEN needs, and it has no struct layout to get wrong.
;;;   * SIZE/MTIME use stat64 (195) / fstat64 (197), whose struct stat64 is
;;;     NOT the x86-64 struct stat: st_size sits at 44, not 48, and st_mtime
;;;     at 72, not 88.  Probe 9 measures both against a file whose size the
;;;     BUILD computes, so a wrong offset fails loudly instead of returning
;;;     a plausible number.
(defvar *i386-fileio-source*
  (if (>= *i386-layer* 5)
      "
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
"
      ""))

;;; In-image override of ieee-float-bits: the build-time version uses
;;; sb-kernel:double-float-* (host-only).  Appended AFTER the compiler source
;;; so it wins (last-defun-wins).  Only matters for compiling FLOAT literals.
(defvar *stage2-float-override* "
(defun ieee-float-bits (f)
  (logior (ash (logand (%prim-aref f 0) 4294967295) 32)
          (logand (%prim-aref f 1) 4294967295)))
;; Read hi/lo 32-bit halves directly from the boxed float's slots.  NEVER
;; combine into a 64-bit integer: for floats >= 2.0 the hi half >= #x40000000,
;; so (ash hi 32) >= 2^62 and Modus's bignum-range ASH is lossy, corrupting
;; the literal's bits at compile time (2.0/9.0/-1.5 all read back as garbage).
;; These two stay <= #xFFFFFFFF, safely in fixnum range.
(defun ieee-float-hi32 (f) (logand (%prim-aref f 0) 4294967295))
(defun ieee-float-lo32 (f) (logand (%prim-aref f 1) 4294967295))
;; Bignum-literal decomposition: read the already-built bignum object's slots
;; directly.  The host recompute path uses (logand value mask62), but the
;; compiled `logand` primop is a raw machine AND of tagged words — for a bignum
;; operand (a heap pointer) that yields garbage (e.g. (logand 2^62 (1- 2^62))
;; returned 66281634333124 instead of 0).  These read the decomposed limbs the
;; host already stored.
(defun %lit-bignum-big-p (value) (big-bignum-p value))
(defun %lit-bn-lo (value) (bignum-lo value))
(defun %lit-bn-hi (value) (bignum-hi value))
(defun %lit-bb-sign (value) (%bb-sign value))
(defun %lit-bb-nlimbs (value) (%bb-nlimbs value))
(defun %lit-bb-limb (value k) (%bb-limb value k))
")
(defvar *float-override-source*
  (if (>= *i386-layer* 5) *stage2-float-override* ""))

;;; The opcode table, materialised as code.  *opcode-table* is a host hash
;;; table built by the ISA definition at load time; the image needs the same
;;; contents, so emit a populate function and a defparameter whose init thunk
;;; runs it at boot.
(defvar *opcode-table-init-source*
  (if (>= *i386-layer* 5)
      (with-output-to-string (s)
        (format s "(defun %populate-opcode-table ()~%")
        (maphash (lambda (code info)
                   (format s "  (setf (gethash ~D *opcode-table*) (make-opcode-info :code ~D :name ~S :operands (quote ~S) :description ~S))~%"
                           code code
                           (modus.mvm::opcode-info-name info)
                           (modus.mvm::opcode-info-operands info)
                           (modus.mvm::opcode-info-description info)))
                 modus.mvm::*opcode-table*)
        (format s "  t)~%")
        (format s "(defparameter *%opcode-table-ready* (progn (%populate-opcode-table) t))~%"))
      ""))

(defvar *crypto-source*
  (if (>= *i386-layer* 4)
      (concatenate 'string *crypto-glue-source* (string #\Newline)
                   (mvm-text "net/crypto.lisp") (string #\Newline))
      ""))

;;; ============================================================
;;; Driver — argv-dispatched probes, mirroring build-aarch64-cli
;;; ============================================================

;;; *** WARNING: the driver below is a LISP STRING. ***  Its comments cannot
;;; contain a double-quote character — one `"` terminates the string early and
;;; SBCL reports the nonsense `defvar ... got 27 args`, which sends you hunting
;;; in entirely the wrong place.  Cost this twice; write SHA256 of abc, not
;;; SHA256("abc").
(defvar *driver-source* "
;;; ============================================================
;;; Driver: the regression suite plus three diagnostics
;;; ============================================================
;;; GENERATED, not written by hand.  Every expected value in the suite —
;;; digests, quarter-round words, magnitude-ladder residues — was computed by
;;; ONE script (scratchpad gendriver.py) from the same expression the image
;;; evaluates, then baked in.  Hand-transcribing hex produced four separate
;;; false findings during the i386 bring-up, so the rule is now structural.
;;;
;;; *** WARNING: this is a LISP STRING. ***  Its text cannot contain a double
;;; quote — one terminates the string early and SBCL reports the nonsense
;;; `defvar ... got 27 args`, which sends you hunting in the wrong file.
;;; The generator asserts on it.








(defun sys-exit (code)
  (let ((c code))
    (syscall3 1 c 0 0)))

;; argv[1] / argv[2] are staged NUL-terminated by the entry stub
;; (boot/boot-linux-i386.lisp) at these fixed BSS addresses.
(defun %argv1 ()
  (let ((n 0) (i 0))
    (loop
      (let ((b (mem-ref (+ 268435976 i) :u8)))
        (when (or (< b 48) (> b 57)) (return n))
        (setq n (+ (* n 10) (- b 48)))
        (setq i (+ i 1))))))

(defun %argv2 ()
  (let ((n 0) (i 0))
    (loop
      (let ((b (mem-ref (+ 268436040 i) :u8)))
        (when (or (< b 48) (> b 57)) (return n))
        (setq n (+ (* n 10) (- b 48)))
        (setq i (+ i 1))))))

(defun putnl () (write-char-serial 10))

;; Decimal printer built ONLY from +, -, < and > — deliberately NOT from
;; TRUNCATE/MOD, which are real CL-runtime defuns taking &rest.  Using them
;; here would couple every numeric check to the &rest/nargs machinery instead
;; of measuring what it claims to measure.
(defun %pdec (n)
  (if (< n 0)
      (progn (write-char-serial 45) (%pdec (- 0 n)))
      (let ((tt 0) (th 0) (h 0) (t10 0) (r n))
        (loop (when (< r 10000) (return nil)) (setq r (- r 10000)) (setq tt (+ tt 1)))
        (loop (when (< r 1000) (return nil)) (setq r (- r 1000)) (setq th (+ th 1)))
        (loop (when (< r 100) (return nil)) (setq r (- r 100)) (setq h (+ h 1)))
        (loop (when (< r 10) (return nil)) (setq r (- r 10)) (setq t10 (+ t10 1)))
        (when (> tt 0) (write-char-serial (+ 48 tt)))
        (when (or (> tt 0) (> th 0)) (write-char-serial (+ 48 th)))
        (when (or (> tt 0) (> th 0) (> h 0)) (write-char-serial (+ 48 h)))
        (when (or (> tt 0) (> th 0) (> h 0) (> t10 0)) (write-char-serial (+ 48 t10)))
        (write-char-serial (+ 48 r)))))

(defun %hexdig (n) (if (< n 10) (+ 48 n) (+ 87 n)))
(defun %phex (b)
  (write-char-serial (%hexdig (ash b -4)))
  (write-char-serial (%hexdig (logand b 15))))
(defun %print-digest (h)
  (let ((i 0))
    (loop (when (>= i 32) (return nil)) (%phex (aref h i)) (setq i (+ i 1))))
  (putnl))
(defun %phex32 (s off)
  (%phex (aref s off)) (%phex (aref s (+ off 1)))
  (%phex (aref s (+ off 2))) (%phex (aref s (+ off 3))))
(defun %phexw (a)
  (%phex (mem-ref (+ a 3) :u8)) (%phex (mem-ref (+ a 2) :u8))
  (%phex (mem-ref (+ a 1) :u8)) (%phex (mem-ref a :u8)))

(defun %gccount () (mem-ref 268435552 :u32))

;;; ---- self-checking scoreboard -------------------------------------------
;;; Counters live in BSS rather than defvars: a defvar's init form is not run
;;; at boot on this image (Active Limitation #7), so a defvar would silently
;;; read NIL.
(defun %sc-reset () (setf (mem-ref 268438400 :u32) 0)
                    (setf (mem-ref 268438408 :u32) 0)
                    (setf (mem-ref 268438416 :u32) 0))
(defun %sc-pass () (setf (mem-ref 268438400 :u32) (+ (mem-ref 268438400 :u32) 1)))
(defun %sc-fail () (setf (mem-ref 268438408 :u32) (+ (mem-ref 268438408 :u32) 1)))
(defun %sc-gap () (setf (mem-ref 268438416 :u32) (+ (mem-ref 268438416 :u32) 1)))

;; Every check prints its own verdict: a suite you have to eyeball is not a
;; suite.  Callers emit the label, then one of these closes the line.
(defun %chk (got want)
  (write-char-serial 32)
  (if (eql got want)
      (progn (%sc-pass) (write-char-serial 80) (write-char-serial 65) (write-char-serial 83) (write-char-serial 83))
      (progn (%sc-fail) (write-char-serial 70) (write-char-serial 65) (write-char-serial 73) (write-char-serial 76)
             (write-char-serial 32) (%pdec got)
             (write-char-serial 32) (write-char-serial 119) (write-char-serial 97) (write-char-serial 110) (write-char-serial 116) (write-char-serial 32) (%pdec want)))
  (putnl))


;; A check for a defect that is KNOWN and still open.  It reports loudly on
;; every run — got and want, same as a failure — but does not fail the suite,
;; so the gate stays honest about the gap without going permanently red.  If
;; one of these starts passing, that is a signal to promote it to a %chk.
(defun %xgap (got want)
  (write-char-serial 32)
  (if (eql got want)
      (progn (%sc-pass) (write-char-serial 80) (write-char-serial 65) (write-char-serial 83) (write-char-serial 83) (write-char-serial 32) (write-char-serial 40) (write-char-serial 107) (write-char-serial 110) (write-char-serial 111) (write-char-serial 119) (write-char-serial 110) (write-char-serial 32) (write-char-serial 103) (write-char-serial 97) (write-char-serial 112) (write-char-serial 32) (write-char-serial 110) (write-char-serial 111) (write-char-serial 119) (write-char-serial 32) (write-char-serial 70) (write-char-serial 73) (write-char-serial 88) (write-char-serial 69) (write-char-serial 68) (write-char-serial 32) (write-char-serial 45) (write-char-serial 32) (write-char-serial 112) (write-char-serial 114) (write-char-serial 111) (write-char-serial 109) (write-char-serial 111) (write-char-serial 116) (write-char-serial 101) (write-char-serial 32) (write-char-serial 116) (write-char-serial 111) (write-char-serial 32) (write-char-serial 37) (write-char-serial 99) (write-char-serial 104) (write-char-serial 107) (write-char-serial 41))
      (progn (%sc-gap) (write-char-serial 75) (write-char-serial 78) (write-char-serial 79) (write-char-serial 87) (write-char-serial 78) (write-char-serial 45) (write-char-serial 71) (write-char-serial 65) (write-char-serial 80)
             (write-char-serial 32) (%pdec got)
             (write-char-serial 32) (write-char-serial 119) (write-char-serial 97) (write-char-serial 110) (write-char-serial 116) (write-char-serial 32) (%pdec want)))
  (putnl))

(defun %chkge (got want)
  (write-char-serial 32)
  (if (>= got want)
      (progn (%sc-pass) (write-char-serial 80) (write-char-serial 65) (write-char-serial 83) (write-char-serial 83))
      (progn (%sc-fail) (write-char-serial 70) (write-char-serial 65) (write-char-serial 73) (write-char-serial 76)
             (write-char-serial 32) (%pdec got)
             (write-char-serial 32) (write-char-serial 109) (write-char-serial 105) (write-char-serial 110) (write-char-serial 32) (%pdec want)))
  (putnl))

(defun %chkdig (got want)
  (let ((bad 0) (i 0))
    (loop (when (>= i 32) (return nil))
          (when (not (eql (aref got i) (aref want i))) (setq bad (+ bad 1)))
          (setq i (+ i 1)))
    (write-char-serial 32)
    (if (eql bad 0)
        (progn (%sc-pass) (write-char-serial 80) (write-char-serial 65) (write-char-serial 83) (write-char-serial 83) (putnl))
        (progn (%sc-fail) (write-char-serial 70) (write-char-serial 65) (write-char-serial 73) (write-char-serial 76) (putnl)
               (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 103) (write-char-serial 111) (write-char-serial 116) (write-char-serial 32) (%print-digest got)))))


(defun sq (x) (* x x))
(defun f5 (a b c d e) (+ a (+ b (+ c (+ d e)))))
(defun f6 (a b c d e f) (+ a (+ b (+ c (+ d (+ e f))))))
(defun f5id (a b c d e) e)
(defun f6id (a b c d e f) f)

;; chacha-qr shape: 5 params, 4 let bindings, repeated setq — the exact body
;; that exposed the >4-argument calling-convention bug.
(defun g5 (s a b c d)
  (let ((sa a) (sb b) (sc c) (sd d))
    (setq sa (+ sa sb))
    (setq sd (logxor sd sa))
    (setq sc (+ sc sd))
    (setq sb (logxor sb sc))
    (+ sa (+ sb (+ sc sd)))))
(defun g5d (s a b c d)
  (let ((x 0))
    (setq x (+ x a)) (setq x (+ x b)) (setq x (+ x c))
    d))
(defun g5buf (s a b c d)
  (let ((va (buf-read-u32 s a)) (vd (buf-read-u32 s d)))
    (+ va vd)))

(defun %mkchain (n)
  (let ((c nil) (i 0))
    (loop (when (>= i n) (return c))
          (setq c (cons i c))
          (setq i (+ i 1)))))

;; Walk a chain built by %mkchain and count elements that are not what they
;; must be.  Element j counting from the head is (n-1-j).
(defun %walkchain (c n)
  (let ((walked 0) (p c) (bad 0))
    (loop
      (when (eql p nil) (return nil))
      (when (>= walked n) (return nil))
      (when (not (eql (car p) (- n (+ walked 1)))) (setq bad (+ bad 1)))
      (setq walked (+ walked 1))
      (setq p (cdr p)))
    (+ bad (- n walked))))

(defun %fill-bytes (m n)
  (let ((i 0))
    (loop (when (>= i n) (return nil)) (aset m i (logand i 255)) (setq i (+ i 1)))))

(defun %exp-abc (a)
  (aset a 0 186)
  (aset a 1 120)
  (aset a 2 22)
  (aset a 3 191)
  (aset a 4 143)
  (aset a 5 1)
  (aset a 6 207)
  (aset a 7 234)
  (aset a 8 65)
  (aset a 9 65)
  (aset a 10 64)
  (aset a 11 222)
  (aset a 12 93)
  (aset a 13 174)
  (aset a 14 34)
  (aset a 15 35)
  (aset a 16 176)
  (aset a 17 3)
  (aset a 18 97)
  (aset a 19 163)
  (aset a 20 150)
  (aset a 21 23)
  (aset a 22 122)
  (aset a 23 156)
  (aset a 24 180)
  (aset a 25 16)
  (aset a 26 255)
  (aset a 27 97)
  (aset a 28 242)
  (aset a 29 0)
  (aset a 30 21)
  (aset a 31 173)
  a)

(defun %exp-empty (a)
  (aset a 0 227)
  (aset a 1 176)
  (aset a 2 196)
  (aset a 3 66)
  (aset a 4 152)
  (aset a 5 252)
  (aset a 6 28)
  (aset a 7 20)
  (aset a 8 154)
  (aset a 9 251)
  (aset a 10 244)
  (aset a 11 200)
  (aset a 12 153)
  (aset a 13 111)
  (aset a 14 185)
  (aset a 15 36)
  (aset a 16 39)
  (aset a 17 174)
  (aset a 18 65)
  (aset a 19 228)
  (aset a 20 100)
  (aset a 21 155)
  (aset a 22 147)
  (aset a 23 76)
  (aset a 24 164)
  (aset a 25 149)
  (aset a 26 153)
  (aset a 27 27)
  (aset a 28 120)
  (aset a 29 82)
  (aset a 30 184)
  (aset a 31 85)
  a)

(defun %exp-bulk (a)
  (aset a 0 161)
  (aset a 1 242)
  (aset a 2 89)
  (aset a 3 212)
  (aset a 4 54)
  (aset a 5 94)
  (aset a 6 212)
  (aset a 7 50)
  (aset a 8 12)
  (aset a 9 55)
  (aset a 10 124)
  (aset a 11 226)
  (aset a 12 111)
  (aset a 13 92)
  (aset a 14 140)
  (aset a 15 86)
  (aset a 16 220)
  (aset a 17 220)
  (aset a 18 154)
  (aset a 19 137)
  (aset a 20 231)
  (aset a 21 182)
  (aset a 22 65)
  (aset a 23 191)
  (aset a 24 216)
  (aset a 25 234)
  (aset a 26 191)
  (aset a 27 187)
  (aset a 28 234)
  (aset a 29 200)
  (aset a 30 102)
  (aset a 31 84)
  a)

(defun probe-suite ()
  (%sc-reset)
  (sha256-init)
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 99) (write-char-serial 111) (write-char-serial 100) (write-char-serial 101) (write-char-serial 103) (write-char-serial 101) (write-char-serial 110) (write-char-serial 32) (write-char-serial 98) (write-char-serial 97) (write-char-serial 115) (write-char-serial 105) (write-char-serial 99) (write-char-serial 115) (putnl)
  (write-char-serial 97) (write-char-serial 100) (write-char-serial 100) (%chk (+ 1 2) 3)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (%chk (let ((x 5)) (* x x)) 25)
  (write-char-serial 115) (write-char-serial 117) (write-char-serial 98) (%chk (- 100 58) 42)
  (write-char-serial 100) (write-char-serial 101) (write-char-serial 102) (write-char-serial 117) (write-char-serial 110) (write-char-serial 45) (write-char-serial 99) (write-char-serial 97) (write-char-serial 108) (write-char-serial 108) (%chk (sq 7) 49)
  (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 45) (write-char-serial 99) (write-char-serial 97) (write-char-serial 114) (write-char-serial 45) (write-char-serial 99) (write-char-serial 100) (write-char-serial 114) (%chk (let ((p (cons 3 4))) (+ (car p) (cdr p))) 7)
  (write-char-serial 102) (write-char-serial 117) (write-char-serial 110) (write-char-serial 99) (write-char-serial 97) (write-char-serial 108) (write-char-serial 108) (%chk (funcall (function sq) 9) 81)
  (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 112) (write-char-serial 45) (write-char-serial 110) (write-char-serial 105) (write-char-serial 108) (%chk (if (consp nil) 1 0) 0)
  (write-char-serial 97) (write-char-serial 116) (write-char-serial 111) (write-char-serial 109) (write-char-serial 45) (write-char-serial 110) (write-char-serial 105) (write-char-serial 108) (%chk (if (atom nil) 1 0) 1)
  (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 112) (write-char-serial 45) (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (%chk (if (consp (cons 1 2)) 1 0) 1)
  (write-char-serial 97) (write-char-serial 116) (write-char-serial 111) (write-char-serial 109) (write-char-serial 45) (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (%chk (if (atom (cons 1 2)) 1 0) 0)
  (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 112) (write-char-serial 45) (write-char-serial 102) (write-char-serial 105) (write-char-serial 120) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (%chk (if (consp 7) 1 0) 0)
  (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 112) (write-char-serial 45) (write-char-serial 108) (write-char-serial 105) (write-char-serial 115) (write-char-serial 116) (write-char-serial 45) (write-char-serial 119) (write-char-serial 97) (write-char-serial 108) (write-char-serial 107) (%chk (let ((n 0) (cur (list 1 2 3))) (loop (when (not (consp cur)) (return n)) (setq n (+ n 1)) (setq cur (cdr cur)))) 3)
  (write-char-serial 105) (write-char-serial 110) (write-char-serial 116) (write-char-serial 101) (write-char-serial 103) (write-char-serial 101) (write-char-serial 114) (write-char-serial 112) (write-char-serial 45) (write-char-serial 116) (%chk (if (integerp t) 1 0) 0)
  (write-char-serial 98) (write-char-serial 105) (write-char-serial 103) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (write-char-serial 112) (write-char-serial 45) (write-char-serial 116) (%chk (if (bignump t) 1 0) 0)
  (write-char-serial 115) (write-char-serial 121) (write-char-serial 109) (write-char-serial 98) (write-char-serial 111) (write-char-serial 108) (write-char-serial 112) (write-char-serial 45) (write-char-serial 116) (%chk (if (symbolp t) 1 0) 1)
  (write-char-serial 115) (write-char-serial 116) (write-char-serial 114) (write-char-serial 105) (write-char-serial 110) (write-char-serial 103) (write-char-serial 112) (write-char-serial 45) (write-char-serial 116) (%chk (if (stringp t) 1 0) 0)
  (write-char-serial 105) (write-char-serial 110) (write-char-serial 116) (write-char-serial 101) (write-char-serial 103) (write-char-serial 101) (write-char-serial 114) (write-char-serial 112) (write-char-serial 45) (write-char-serial 102) (write-char-serial 105) (write-char-serial 120) (%chk (if (integerp 5) 1 0) 1)
  (write-char-serial 105) (write-char-serial 110) (write-char-serial 116) (write-char-serial 101) (write-char-serial 103) (write-char-serial 101) (write-char-serial 114) (write-char-serial 112) (write-char-serial 45) (write-char-serial 122) (write-char-serial 101) (write-char-serial 114) (write-char-serial 111) (%chk (if (integerp 0) 1 0) 1)
  (write-char-serial 97) (write-char-serial 114) (write-char-serial 114) (write-char-serial 97) (write-char-serial 121) (write-char-serial 45) (write-char-serial 108) (write-char-serial 101) (write-char-serial 110) (write-char-serial 103) (write-char-serial 116) (write-char-serial 104) (write-char-serial 45) (write-char-serial 116) (%chk (array-length t) 0)
  (write-char-serial 97) (write-char-serial 114) (write-char-serial 114) (write-char-serial 97) (write-char-serial 121) (write-char-serial 45) (write-char-serial 108) (write-char-serial 101) (write-char-serial 110) (write-char-serial 103) (write-char-serial 116) (write-char-serial 104) (write-char-serial 45) (write-char-serial 114) (write-char-serial 101) (write-char-serial 97) (write-char-serial 108) (%chk (array-length (make-array 5)) 5)
  (write-char-serial 97) (write-char-serial 115) (write-char-serial 104) (write-char-serial 45) (write-char-serial 110) (write-char-serial 101) (write-char-serial 103) (write-char-serial 51) (write-char-serial 50) (%chk (if (eql (ash 7 -32) 0) 1 0) 1)
  (write-char-serial 97) (write-char-serial 115) (write-char-serial 104) (write-char-serial 45) (write-char-serial 110) (write-char-serial 101) (write-char-serial 103) (write-char-serial 54) (write-char-serial 52) (%chk (if (eql (ash 7 -64) 0) 1 0) 1)
  (write-char-serial 97) (write-char-serial 115) (write-char-serial 104) (write-char-serial 45) (write-char-serial 110) (write-char-serial 101) (write-char-serial 103) (write-char-serial 51) (write-char-serial 50) (write-char-serial 45) (write-char-serial 110) (write-char-serial 101) (write-char-serial 103) (%chk (if (eql (ash -7 -32) -1) 1 0) 1)
  (write-char-serial 97) (write-char-serial 115) (write-char-serial 104) (write-char-serial 45) (write-char-serial 110) (write-char-serial 101) (write-char-serial 103) (write-char-serial 51) (write-char-serial 49) (%chk (if (eql (ash 7 -31) 0) 1 0) 1)
  (write-char-serial 97) (write-char-serial 115) (write-char-serial 104) (write-char-serial 45) (write-char-serial 110) (write-char-serial 101) (write-char-serial 103) (write-char-serial 50) (%chk (if (eql (ash 7 -2) 1) 1 0) 1)
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 119) (write-char-serial 111) (write-char-serial 114) (write-char-serial 100) (write-char-serial 32) (write-char-serial 119) (write-char-serial 105) (write-char-serial 100) (write-char-serial 116) (write-char-serial 104) (putnl)
  (write-char-serial 102) (write-char-serial 105) (write-char-serial 120) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (write-char-serial 45) (write-char-serial 109) (write-char-serial 97) (write-char-serial 120) (write-char-serial 45) (write-char-serial 105) (write-char-serial 115) (write-char-serial 45) (write-char-serial 102) (write-char-serial 105) (write-char-serial 120) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (%chk (if (fixnump 1073741823) 1 0) 1)
  (write-char-serial 112) (write-char-serial 97) (write-char-serial 115) (write-char-serial 116) (write-char-serial 45) (write-char-serial 102) (write-char-serial 105) (write-char-serial 120) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (write-char-serial 45) (write-char-serial 105) (write-char-serial 115) (write-char-serial 45) (write-char-serial 98) (write-char-serial 105) (write-char-serial 103) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (%chk (if (fixnump 1073741824) 1 0) 0)
  (write-char-serial 100) (write-char-serial 111) (write-char-serial 117) (write-char-serial 98) (write-char-serial 108) (write-char-serial 105) (write-char-serial 110) (write-char-serial 103) (write-char-serial 45) (write-char-serial 112) (write-char-serial 114) (write-char-serial 111) (write-char-serial 109) (write-char-serial 111) (write-char-serial 116) (write-char-serial 101) (write-char-serial 115) (write-char-serial 45) (write-char-serial 110) (write-char-serial 111) (write-char-serial 116) (write-char-serial 45) (write-char-serial 119) (write-char-serial 114) (write-char-serial 97) (write-char-serial 112) (write-char-serial 115) (%chk (if (> (ash 1 40) 0) 1 0) 1)
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 98) (write-char-serial 105) (write-char-serial 103) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (write-char-serial 32) (write-char-serial 112) (write-char-serial 114) (write-char-serial 111) (write-char-serial 109) (write-char-serial 111) (write-char-serial 116) (write-char-serial 105) (write-char-serial 111) (write-char-serial 110) (putnl)
  (write-char-serial 108) (write-char-serial 111) (write-char-serial 103) (write-char-serial 97) (write-char-serial 110) (write-char-serial 100) (write-char-serial 45) (write-char-serial 117) (write-char-serial 51) (write-char-serial 50) (%chk (logand 1116352408 255) 152)
  (write-char-serial 97) (write-char-serial 115) (write-char-serial 104) (write-char-serial 45) (write-char-serial 117) (write-char-serial 51) (write-char-serial 50) (%chk (ash 1116352408 -24) 66)
  (write-char-serial 99) (write-char-serial 97) (write-char-serial 114) (write-char-serial 114) (write-char-serial 121) (write-char-serial 45) (write-char-serial 50) (write-char-serial 94) (write-char-serial 51) (write-char-serial 48) (%chk (logand (+ 1073741824 1) 255) 1)
  (write-char-serial 108) (write-char-serial 111) (write-char-serial 103) (write-char-serial 97) (write-char-serial 110) (write-char-serial 100) (write-char-serial 45) (write-char-serial 109) (write-char-serial 97) (write-char-serial 120) (write-char-serial 45) (write-char-serial 117) (write-char-serial 51) (write-char-serial 50) (%chk (logand 4294967295 255) 255)
  (write-char-serial 108) (write-char-serial 111) (write-char-serial 103) (write-char-serial 120) (write-char-serial 111) (write-char-serial 114) (write-char-serial 45) (write-char-serial 117) (write-char-serial 51) (write-char-serial 50) (%chk (logand (logxor 4294967295 1116352408) 255) 103)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (write-char-serial 45) (write-char-serial 112) (write-char-serial 114) (write-char-serial 111) (write-char-serial 109) (write-char-serial 111) (write-char-serial 116) (write-char-serial 101) (write-char-serial 45) (write-char-serial 108) (write-char-serial 111) (%chk (logand (* 17034 65536) 255) 0)
  (write-char-serial 115) (write-char-serial 104) (write-char-serial 108) (write-char-serial 45) (write-char-serial 111) (write-char-serial 118) (write-char-serial 101) (write-char-serial 114) (write-char-serial 102) (write-char-serial 108) (write-char-serial 111) (write-char-serial 119) (%chk (ash (ash 3 30) -24) 192)
  (write-char-serial 109) (write-char-serial 97) (write-char-serial 115) (write-char-serial 107) (write-char-serial 45) (write-char-serial 116) (write-char-serial 104) (write-char-serial 101) (write-char-serial 110) (write-char-serial 45) (write-char-serial 115) (write-char-serial 104) (write-char-serial 105) (write-char-serial 102) (write-char-serial 116) (%chk (ash (logand (ash 255 24) 4294967295) -24) 255)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (write-char-serial 45) (write-char-serial 112) (write-char-serial 114) (write-char-serial 111) (write-char-serial 109) (write-char-serial 111) (write-char-serial 116) (write-char-serial 101) (write-char-serial 45) (write-char-serial 104) (write-char-serial 105) (%chk (ash (* 17034 65536) -24) 66)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (write-char-serial 45) (write-char-serial 114) (write-char-serial 101) (write-char-serial 115) (write-char-serial 117) (write-char-serial 108) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 115) (write-char-serial 45) (write-char-serial 98) (write-char-serial 105) (write-char-serial 103) (write-char-serial 110) (write-char-serial 117) (write-char-serial 109) (%chk (if (fixnump (* 17034 65536)) 1 0) 0)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (write-char-serial 45) (write-char-serial 98) (write-char-serial 105) (write-char-serial 103) (write-char-serial 45) (write-char-serial 108) (write-char-serial 111) (%chk (logand (* 100000 100000) 255) 0)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (write-char-serial 45) (write-char-serial 98) (write-char-serial 105) (write-char-serial 103) (write-char-serial 45) (write-char-serial 104) (write-char-serial 105) (%chk (ash (* 100000 100000) -24) 596)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (write-char-serial 45) (write-char-serial 104) (write-char-serial 117) (write-char-serial 103) (write-char-serial 101) (%chk (logand (ash (* 123456789 987654321) -32) 255) 20)
  (write-char-serial 109) (write-char-serial 117) (write-char-serial 108) (write-char-serial 45) (write-char-serial 110) (write-char-serial 101) (write-char-serial 103) (%chk (logand (- 0 (* 40000 40000)) 255) 0)
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 109) (write-char-serial 101) (write-char-serial 109) (write-char-serial 45) (write-char-serial 114) (write-char-serial 101) (write-char-serial 102) (write-char-serial 32) (write-char-serial 117) (write-char-serial 51) (write-char-serial 50) (write-char-serial 32) (write-char-serial 114) (write-char-serial 111) (write-char-serial 117) (write-char-serial 110) (write-char-serial 100) (write-char-serial 32) (write-char-serial 116) (write-char-serial 114) (write-char-serial 105) (write-char-serial 112) (putnl)
  (setf (mem-ref 268438272 :u32) 1116352408)
  (write-char-serial 109) (write-char-serial 101) (write-char-serial 109) (write-char-serial 114) (write-char-serial 101) (write-char-serial 102) (write-char-serial 45) (write-char-serial 117) (write-char-serial 51) (write-char-serial 50) (write-char-serial 45) (write-char-serial 108) (write-char-serial 111) (%chk (logand (mem-ref 268438272 :u32) 255) 152)
  (write-char-serial 109) (write-char-serial 101) (write-char-serial 109) (write-char-serial 114) (write-char-serial 101) (write-char-serial 102) (write-char-serial 45) (write-char-serial 117) (write-char-serial 51) (write-char-serial 50) (write-char-serial 45) (write-char-serial 104) (write-char-serial 105) (%chk (ash (mem-ref 268438272 :u32) -24) 66)
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 99) (write-char-serial 97) (write-char-serial 108) (write-char-serial 108) (write-char-serial 105) (write-char-serial 110) (write-char-serial 103) (write-char-serial 32) (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 118) (write-char-serial 101) (write-char-serial 110) (write-char-serial 116) (write-char-serial 105) (write-char-serial 111) (write-char-serial 110) (write-char-serial 32) (write-char-serial 40) (write-char-serial 62) (write-char-serial 52) (write-char-serial 32) (write-char-serial 97) (write-char-serial 114) (write-char-serial 103) (write-char-serial 115) (write-char-serial 41) (putnl)
  (write-char-serial 97) (write-char-serial 114) (write-char-serial 103) (write-char-serial 115) (write-char-serial 53) (write-char-serial 45) (write-char-serial 115) (write-char-serial 117) (write-char-serial 109) (%chk (f5 1 2 4 8 16) 31)
  (write-char-serial 97) (write-char-serial 114) (write-char-serial 103) (write-char-serial 115) (write-char-serial 54) (write-char-serial 45) (write-char-serial 115) (write-char-serial 117) (write-char-serial 109) (%chk (f6 1 2 4 8 16 32) 63)
  (write-char-serial 97) (write-char-serial 114) (write-char-serial 103) (write-char-serial 115) (write-char-serial 53) (write-char-serial 45) (write-char-serial 108) (write-char-serial 97) (write-char-serial 115) (write-char-serial 116) (%chk (f5id 1 2 4 8 16) 16)
  (write-char-serial 97) (write-char-serial 114) (write-char-serial 103) (write-char-serial 115) (write-char-serial 54) (write-char-serial 45) (write-char-serial 108) (write-char-serial 97) (write-char-serial 115) (write-char-serial 116) (%chk (f6id 1 2 4 8 16 32) 32)
  (write-char-serial 102) (write-char-serial 114) (write-char-serial 97) (write-char-serial 109) (write-char-serial 101) (write-char-serial 115) (write-char-serial 108) (write-char-serial 111) (write-char-serial 116) (write-char-serial 45) (write-char-serial 115) (write-char-serial 101) (write-char-serial 116) (write-char-serial 113) (%chk (g5 0 1 2 4 8) 42)
  (write-char-serial 102) (write-char-serial 114) (write-char-serial 97) (write-char-serial 109) (write-char-serial 101) (write-char-serial 115) (write-char-serial 108) (write-char-serial 111) (write-char-serial 116) (write-char-serial 45) (write-char-serial 99) (write-char-serial 97) (write-char-serial 114) (write-char-serial 114) (write-char-serial 121) (%chk (g5d 0 1 2 4 8) 8)
  (let ((s (make-array 64)))
    (%fill-bytes s 64)
    (buf-write-u32 s 0 5) (buf-write-u32 s 48 9)
    (write-char-serial 102) (write-char-serial 114) (write-char-serial 97) (write-char-serial 109) (write-char-serial 101) (write-char-serial 115) (write-char-serial 108) (write-char-serial 111) (write-char-serial 116) (write-char-serial 45) (write-char-serial 98) (write-char-serial 117) (write-char-serial 102) (%chk (g5buf s 0 16 32 48) 14))
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 109) (write-char-serial 97) (write-char-serial 103) (write-char-serial 110) (write-char-serial 105) (write-char-serial 116) (write-char-serial 117) (write-char-serial 100) (write-char-serial 101) (write-char-serial 32) (write-char-serial 108) (write-char-serial 97) (write-char-serial 100) (write-char-serial 100) (write-char-serial 101) (write-char-serial 114) (write-char-serial 32) (write-char-serial 40) (write-char-serial 51) (write-char-serial 48) (write-char-serial 47) (write-char-serial 51) (write-char-serial 50) (write-char-serial 47) (write-char-serial 52) (write-char-serial 56) (write-char-serial 47) (write-char-serial 54) (write-char-serial 48) (write-char-serial 47) (write-char-serial 54) (write-char-serial 50) (write-char-serial 32) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 115) (write-char-serial 41) (putnl)
  (write-char-serial 51) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 97) (write-char-serial 110) (write-char-serial 100) (%chk (logand 1073741823 255) 255)
  (write-char-serial 51) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 115) (write-char-serial 104) (write-char-serial 114) (%chk (logand (ash 1073741823 -8) 255) 255)
  (write-char-serial 51) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logior 1073741823 1) 255) 255)
  (write-char-serial 51) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 120) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logxor 1073741823 255) 255) 0)
  (write-char-serial 51) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 110) (write-char-serial 99) (%chk (logand (+ 1073741823 1) 255) 0)
  (write-char-serial 51) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 100) (write-char-serial 101) (write-char-serial 99) (%chk (logand (- 1073741823 1) 255) 254)
  (write-char-serial 51) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 97) (write-char-serial 110) (write-char-serial 100) (%chk (logand 2609737539 255) 67)
  (write-char-serial 51) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 115) (write-char-serial 104) (write-char-serial 114) (%chk (logand (ash 2609737539 -8) 255) 111)
  (write-char-serial 51) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logior 2609737539 1) 255) 67)
  (write-char-serial 51) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 120) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logxor 2609737539 255) 255) 188)
  (write-char-serial 51) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 110) (write-char-serial 99) (%chk (logand (+ 2609737539 1) 255) 68)
  (write-char-serial 51) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 100) (write-char-serial 101) (write-char-serial 99) (%chk (logand (- 2609737539 1) 255) 66)
  (write-char-serial 52) (write-char-serial 56) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 97) (write-char-serial 110) (write-char-serial 100) (%chk (logand 171031759355904 255) 0)
  (write-char-serial 52) (write-char-serial 56) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 115) (write-char-serial 104) (write-char-serial 114) (%chk (logand (ash 171031759355904 -8) 255) 0)
  (write-char-serial 52) (write-char-serial 56) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logior 171031759355904 1) 255) 1)
  (write-char-serial 52) (write-char-serial 56) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 120) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logxor 171031759355904 255) 255) 255)
  (write-char-serial 52) (write-char-serial 56) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 110) (write-char-serial 99) (%chk (logand (+ 171031759355904 1) 255) 1)
  (write-char-serial 52) (write-char-serial 56) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 100) (write-char-serial 101) (write-char-serial 99) (%chk (logand (- 171031759355904 1) 255) 255)
  (write-char-serial 54) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 97) (write-char-serial 110) (write-char-serial 100) (%chk (logand 576460752303435833 255) 57)
  (write-char-serial 54) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 115) (write-char-serial 104) (write-char-serial 114) (%chk (logand (ash 576460752303435833 -8) 255) 48)
  (write-char-serial 54) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logior 576460752303435833 1) 255) 57)
  (write-char-serial 54) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 120) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logxor 576460752303435833 255) 255) 198)
  (write-char-serial 54) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 110) (write-char-serial 99) (%chk (logand (+ 576460752303435833 1) 255) 58)
  (write-char-serial 54) (write-char-serial 48) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 100) (write-char-serial 101) (write-char-serial 99) (%chk (logand (- 576460752303435833 1) 255) 56)
  (write-char-serial 54) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 97) (write-char-serial 110) (write-char-serial 100) (%chk (logand 4611686018427387911 255) 7)
  (write-char-serial 54) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 115) (write-char-serial 104) (write-char-serial 114) (%chk (logand (ash 4611686018427387911 -8) 255) 0)
  (write-char-serial 54) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logior 4611686018427387911 1) 255) 7)
  (write-char-serial 54) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 120) (write-char-serial 111) (write-char-serial 114) (%chk (logand (logxor 4611686018427387911 255) 255) 248)
  (write-char-serial 54) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 105) (write-char-serial 110) (write-char-serial 99) (%chk (logand (+ 4611686018427387911 1) 255) 8)
  (write-char-serial 54) (write-char-serial 50) (write-char-serial 98) (write-char-serial 105) (write-char-serial 116) (write-char-serial 45) (write-char-serial 100) (write-char-serial 101) (write-char-serial 99) (%chk (logand (- 4611686018427387911 1) 255) 6)
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 99) (write-char-serial 114) (write-char-serial 121) (write-char-serial 112) (write-char-serial 116) (write-char-serial 111) (putnl)
  (let ((m (make-array 3)) (e (make-array 32)))
    (aset m 0 97) (aset m 1 98) (aset m 2 99)
    (write-char-serial 115) (write-char-serial 104) (write-char-serial 97) (write-char-serial 50) (write-char-serial 53) (write-char-serial 54) (write-char-serial 45) (write-char-serial 97) (write-char-serial 98) (write-char-serial 99) (%chkdig (sha256 m) (%exp-abc e)))
  (let ((e (make-array 32)))
    (write-char-serial 115) (write-char-serial 104) (write-char-serial 97) (write-char-serial 50) (write-char-serial 53) (write-char-serial 54) (write-char-serial 45) (write-char-serial 101) (write-char-serial 109) (write-char-serial 112) (write-char-serial 116) (write-char-serial 121) (%chkdig (sha256 (make-array 0)) (%exp-empty e)))
  (let ((s (make-array 64)))
    (let ((i 0)) (loop (when (>= i 64) (return nil)) (aset s i 0) (setq i (+ i 1))))
    (buf-write-u32 s 0 286331153)
    (buf-write-u32 s 16 16909060)
    (buf-write-u32 s 32 2609737539)
    (buf-write-u32 s 48 19088743)
    (chacha-qr s 0 16 32 48)
    (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 45) (write-char-serial 113) (write-char-serial 114) (write-char-serial 45) (write-char-serial 119) (write-char-serial 48) (%chk (buf-read-u32 s 0) 3928658676)
    (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 45) (write-char-serial 113) (write-char-serial 114) (write-char-serial 45) (write-char-serial 119) (write-char-serial 49) (%chk (buf-read-u32 s 16) 3407673550)
    (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 45) (write-char-serial 113) (write-char-serial 114) (write-char-serial 45) (write-char-serial 119) (write-char-serial 50) (%chk (buf-read-u32 s 32) 1166100270)
    (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 45) (write-char-serial 113) (write-char-serial 114) (write-char-serial 45) (write-char-serial 119) (write-char-serial 51) (%chk (buf-read-u32 s 48) 1484899515)
    0)
  (write-char-serial 45) (write-char-serial 45) (write-char-serial 32) (write-char-serial 99) (write-char-serial 111) (write-char-serial 108) (write-char-serial 108) (write-char-serial 101) (write-char-serial 99) (write-char-serial 116) (write-char-serial 111) (write-char-serial 114) (write-char-serial 58) (write-char-serial 32) (write-char-serial 98) (write-char-serial 117) (write-char-serial 108) (write-char-serial 107) (write-char-serial 32) (write-char-serial 83) (write-char-serial 72) (write-char-serial 65) (write-char-serial 32) (write-char-serial 43) (write-char-serial 32) (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 105) (write-char-serial 110) (write-char-serial 32) (write-char-serial 115) (write-char-serial 117) (write-char-serial 114) (write-char-serial 118) (write-char-serial 105) (write-char-serial 118) (write-char-serial 97) (write-char-serial 108) (putnl)
  (let ((chain (%mkchain 1000)))
    (let ((m (make-array 16384)) (e (make-array 32)))
      (%fill-bytes m 16384)
      (write-char-serial 115) (write-char-serial 104) (write-char-serial 97) (write-char-serial 50) (write-char-serial 53) (write-char-serial 54) (write-char-serial 45) (write-char-serial 98) (write-char-serial 117) (write-char-serial 108) (write-char-serial 107) (write-char-serial 45) (write-char-serial 49) (write-char-serial 54) (write-char-serial 107) (%chkdig (sha256 m) (%exp-bulk e)))
    (write-char-serial 99) (write-char-serial 111) (write-char-serial 108) (write-char-serial 108) (write-char-serial 101) (write-char-serial 99) (write-char-serial 116) (write-char-serial 105) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 45) (write-char-serial 114) (write-char-serial 97) (write-char-serial 110) (%chkge (%gccount) 1)
    (write-char-serial 99) (write-char-serial 104) (write-char-serial 97) (write-char-serial 105) (write-char-serial 110) (write-char-serial 45) (write-char-serial 115) (write-char-serial 117) (write-char-serial 114) (write-char-serial 118) (write-char-serial 105) (write-char-serial 118) (write-char-serial 101) (write-char-serial 100) (write-char-serial 45) (write-char-serial 103) (write-char-serial 99) (%chk (%walkchain chain 1000) 0))
  (write-char-serial 61) (write-char-serial 61) (write-char-serial 32) (write-char-serial 115) (write-char-serial 117) (write-char-serial 109) (write-char-serial 109) (write-char-serial 97) (write-char-serial 114) (write-char-serial 121) (write-char-serial 32) (write-char-serial 32) (write-char-serial 112) (write-char-serial 97) (write-char-serial 115) (write-char-serial 115) (write-char-serial 32) (%pdec (mem-ref 268438400 :u32))
  (write-char-serial 32) (write-char-serial 32) (write-char-serial 102) (write-char-serial 97) (write-char-serial 105) (write-char-serial 108) (write-char-serial 32) (%pdec (mem-ref 268438408 :u32))
  (write-char-serial 32) (write-char-serial 32) (write-char-serial 107) (write-char-serial 110) (write-char-serial 111) (write-char-serial 119) (write-char-serial 110) (write-char-serial 45) (write-char-serial 103) (write-char-serial 97) (write-char-serial 112) (write-char-serial 32) (%pdec (mem-ref 268438416 :u32))
  (putnl)
  (mem-ref 268438408 :u32))

;; Heap / collector state.  The one diagnostic that is not a pass/fail check:
;; it reports the map, which is what you want when something is wrong rather
;; than merely failing.
(defun probe-gcmeta ()
  (write-char-serial 102) (write-char-serial 114) (write-char-serial 111) (write-char-serial 109) (write-char-serial 95) (write-char-serial 115) (write-char-serial 116) (write-char-serial 97) (write-char-serial 114) (write-char-serial 116) (write-char-serial 61) (%phexw 268435520) (putnl)
  (write-char-serial 116) (write-char-serial 111) (write-char-serial 95) (write-char-serial 115) (write-char-serial 116) (write-char-serial 97) (write-char-serial 114) (write-char-serial 116) (write-char-serial 32) (write-char-serial 32) (write-char-serial 61) (%phexw 268435528) (putnl)
  (write-char-serial 115) (write-char-serial 112) (write-char-serial 97) (write-char-serial 99) (write-char-serial 101) (write-char-serial 95) (write-char-serial 115) (write-char-serial 105) (write-char-serial 122) (write-char-serial 101) (write-char-serial 61) (%phexw 268435536) (putnl)
  (write-char-serial 115) (write-char-serial 116) (write-char-serial 97) (write-char-serial 99) (write-char-serial 107) (write-char-serial 95) (write-char-serial 98) (write-char-serial 97) (write-char-serial 115) (write-char-serial 101) (write-char-serial 61) (%phexw 268435544) (putnl)
  (write-char-serial 103) (write-char-serial 99) (write-char-serial 95) (write-char-serial 99) (write-char-serial 111) (write-char-serial 117) (write-char-serial 110) (write-char-serial 116) (write-char-serial 32) (write-char-serial 32) (write-char-serial 61) (%phexw 268435552) (putnl)
  (write-char-serial 86) (write-char-serial 65) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 61) (%phexw 268438016) (putnl)
  (write-char-serial 86) (write-char-serial 76) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 61) (%phexw 268438020) (putnl)
  (write-char-serial 67) (write-char-serial 69) (write-char-serial 78) (write-char-serial 86) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 32) (write-char-serial 61) (%phexw 268438032) (putnl)
  (write-char-serial 112) (write-char-serial 97) (write-char-serial 103) (write-char-serial 101) (write-char-serial 95) (write-char-serial 98) (write-char-serial 97) (write-char-serial 115) (write-char-serial 101) (write-char-serial 32) (write-char-serial 61) (%phexw 268438040) (putnl)
  (write-char-serial 115) (write-char-serial 116) (write-char-serial 97) (write-char-serial 114) (write-char-serial 116) (write-char-serial 95) (write-char-serial 98) (write-char-serial 109) (write-char-serial 112) (write-char-serial 32) (write-char-serial 61) (%phexw 268438044) (putnl)
  (write-char-serial 99) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 95) (write-char-serial 98) (write-char-serial 109) (write-char-serial 112) (write-char-serial 32) (write-char-serial 32) (write-char-serial 61) (%phexw 268438048) (putnl)
  0)

;; Bulk SHA over argv[2] KiB: the size knob for pushing the collector.  Prints
;; the digest and the collection count; verify the digest against hashlib.
(defun probe-bulk ()
  (sha256-init)
  (let ((n (* (%argv2) 1024)))
    (let ((m (make-array n)))
      (%fill-bytes m n)
      (write-char-serial 100) (write-char-serial 105) (write-char-serial 103) (write-char-serial 101) (write-char-serial 115) (write-char-serial 116) (write-char-serial 61) (%print-digest (sha256 m))
      (write-char-serial 99) (write-char-serial 111) (write-char-serial 108) (write-char-serial 108) (write-char-serial 101) (write-char-serial 99) (write-char-serial 116) (write-char-serial 105) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 61) (%pdec (%gccount)) (putnl)))
  0)

;; Cons-chain forwarding over argv[2]*100 conses, churned by a 4 KiB SHA so
;; real collections happen underneath it.  Prints elements wrong (want 0).
(defun probe-chain ()
  (sha256-init)
  (let ((n (* (%argv2) 100)))
    (let ((chain (%mkchain n)))
      (let ((m (make-array 4096)))
        (%fill-bytes m 4096)
        (sha256 m))
      (write-char-serial 119) (write-char-serial 114) (write-char-serial 111) (write-char-serial 110) (write-char-serial 103) (write-char-serial 45) (write-char-serial 101) (write-char-serial 108) (write-char-serial 101) (write-char-serial 109) (write-char-serial 101) (write-char-serial 110) (write-char-serial 116) (write-char-serial 115) (write-char-serial 61) (%pdec (%walkchain chain n)) (putnl)
      (write-char-serial 99) (write-char-serial 111) (write-char-serial 108) (write-char-serial 108) (write-char-serial 101) (write-char-serial 99) (write-char-serial 116) (write-char-serial 105) (write-char-serial 111) (write-char-serial 110) (write-char-serial 115) (write-char-serial 61) (%pdec (%gccount)) (putnl)))
  0)


;; probe-argv: the ARCH-SPECIFIC half of lib/cli-toplevel.lisp, proven on i386
;; without needing the rest of it.  cli-toplevel walks the LIVE initial process
;; stack for the full argv/envp; only three of its functions are arch-specific,
;; and all three differ for exactly two reasons: pointers are 4 bytes, not 8,
;; and i386 RELOCATES its stack at boot (the kernel's sits near 0x40800000,
;; above the 2^30 fixnum ceiling), so %gc-stack-base is NOT the initial SP.
;; The boot stub therefore saves the initial ESP to 0x10000290, and these are
;; the i386 forms of %cli-argv-base / %cli-collect-argv / %cli-getenv.
;;
;; mem-ref :u64 is RAW, so a loaded pointer reads back as raw/2 — double it to
;; get the real byte address.  Same convention cli-toplevel documents.
(defun %i386-argv-base ()
  (+ (* 2 (mem-ref 268436112 :u64)) 4))

(defun %i386-argv-ptr (i)
  (* 2 (mem-ref (+ (%i386-argv-base) (* 4 i)) :u64)))

(defun %pcstr (addr)
  (let ((i 0))
    (loop
      (let ((b (mem-ref (+ addr i) :u8)))
        (when (eql b 0) (return nil))
        (write-char-serial b)
        (setq i (+ i 1))))))

(defun probe-argv ()
  (write-char-serial 97) (write-char-serial 114) (write-char-serial 103) (write-char-serial 99) (write-char-serial 61) (%pdec (mem-ref 268435968 :u32)) (putnl)
  ;; The saved initial ESP, printed as raw bytes (%phexw reads byte-wise, so it
  ;; works even when the VALUE cannot be represented).  This is the measurement
  ;; behind the blocker below: the kernel-supplied stack sits near 0x40800000.
  (write-char-serial 101) (write-char-serial 115) (write-char-serial 112) (write-char-serial 61)
  (%phexw 268436112) (putnl)
  ;; HARD LIMIT, not a bug to be worked around here.  An MVM mem-ref carries its
  ;; ADDRESS as a tagged fixnum (the opcode untags with SHR 1), so on a 32-bit
  ;; word an address at or above 2^30 has no representation at all -- its tagged
  ;; form overflows.  ARGC reads fine only because the boot stub copies it into
  ;; the BSS at 0x10000200; the argv POINTERS and the strings they name are
  ;; still up on the kernel stack, out of reach.  Dereferencing one is an
  ;; immediate SIGSEGV, which is what this probe used to do.
  ;; Wiring lib/cli-toplevel.lisp on i386 therefore needs the BOOT STUB to stage
  ;; the whole argv/envp vector down into the BSS, extending what it already
  ;; does for argv[1]/argv[2] -- it is not a matter of an arch arm in Lisp.
  (when (>= (mem-ref 268436112 :u8) 0)
    (let ((hi (mem-ref 268436115 :u8)))
      (when (>= hi 64)
        (write-char-serial 65) (write-char-serial 66) (write-char-serial 79)
        (write-char-serial 86) (write-char-serial 69) (write-char-serial 50)
        (write-char-serial 94) (write-char-serial 51) (write-char-serial 48)
        (putnl)
        (return-from probe-argv 0))))
  (let ((argc (mem-ref 268435968 :u32)) (i 0))
    (loop
      (when (>= i argc) (return nil))
      (write-char-serial 97) (write-char-serial 114) (write-char-serial 103) (write-char-serial 118) (write-char-serial 91) (%pdec i) (write-char-serial 61)
      (%pcstr (%i386-argv-ptr i)) (putnl)
      (setq i (+ i 1)))
    ;; envp follows argv's NULL terminator; print the first entry as proof the
    ;; environment vector is reachable too (cli-toplevel needs HOME for ~/.modusrc)
    (let ((envp (+ (%i386-argv-base) (* 4 (+ argc 1)))))
      (write-char-serial 101) (write-char-serial 110) (write-char-serial 118) (write-char-serial 112) (write-char-serial 91) (write-char-serial 48) (write-char-serial 93) (write-char-serial 61) (%pcstr (* 2 (mem-ref envp :u64))) (putnl)))
  0)

;;; ---- probe-hc: the handler-case traps, mechanics only ------------------
;;; TRAP #x0510/#x0511/#x0512 in ISOLATION, with no condition system in the
;;; picture: a (t (c) ...) clause compiles its type check to the literal T, so
;;; the whole round trip is setjmp / longjmp / clear-handler and nothing else.
;;; %HC-LONGJMP is the compiler special form that emits #x0511 directly.
;;;
;;; Kept OUT of probe-suite deliberately.  The suite is a fixed 71-check gate
;;; used to detect regressions across this port; changing its count would make
;;; every future comparison ambiguous.
(defun %hc1 () (handler-case 7 (t (c) 9)))
(defun %hc2 () (handler-case (progn (%hc-longjmp) 7) (t (c) 9)))
;; Inner frame catches; the outer must NOT be the one that fires.
(defun %hc3 ()
  (handler-case (handler-case (progn (%hc-longjmp) 1) (t (c) 2)) (t (c) 3)))
;; THE NESTING CHECK.  An inner handler-case runs to completion FIRST, so its
;; CLEAR-HANDLER pops.  If the pop restored the wrong frame — or if there were
;; no handler stack at all — the outer frame would be gone and this longjmp
;; would leave the function.  A single-level implementation passes %hc1/%hc2
;; and fails here.
(defun %hc4 ()
  (handler-case (progn (handler-case 1 (t (c) 2)) (%hc-longjmp) 5) (t (c) 3)))
;; Live vregs across the longjmp: ESI/EDI/EBX are V0/V1/V4, so a four-word
;; jmp_buf resumes with garbage in them.
(defun %hc5 (a b)
  (let ((x (+ a 1)) (y (+ b 2)) (z (+ a b)))
    (let ((r (handler-case (progn (%hc-longjmp) 0) (t (c) 5))))
      (+ r (+ x (+ y z))))))
;; Sequential handler-cases — the init-all-globals shape.
(defun %hc6 ()
  (let ((s 0))
    (setq s (+ s (handler-case (progn (%hc-longjmp) 0) (t (c) 1))))
    (setq s (+ s (handler-case (progn (%hc-longjmp) 0) (t (c) 10))))
    (setq s (+ s (handler-case 100 (t (c) 0))))
    s))
;; unwind-protect, both paths.  ASDF depends specifically on the second.
(defun %uwp1 ()
  (let ((flag 0))
    (let ((v (unwind-protect 7 (setq flag 1))))
      (+ v flag))))
(defun %uwp2 ()
  (let ((flag 0))
    (let ((v (handler-case (unwind-protect (progn (%hc-longjmp) 0)
                             (setq flag 1))
               (t (c) 20))))
      (+ v flag))))

(defun %tag2 (a b) (write-char-serial a) (write-char-serial b))

(defun probe-hc ()
  (%sc-reset)
  (%tag2 104 49) (%chk (%hc1) 7)
  (%tag2 104 50) (%chk (%hc2) 9)
  (%tag2 104 51) (%chk (%hc3) 2)
  (%tag2 104 52) (%chk (%hc4) 3)
  (%tag2 104 53) (%chk (%hc5 3 4) 22)
  (%tag2 104 54) (%chk (%hc6) 111)
  (%tag2 117 49) (%chk (%uwp1) 8)
  (%tag2 117 50) (%chk (%uwp2) 21)
  (write-char-serial 80) (write-char-serial 61) (%pdec (mem-ref 268438400 :u32))
  (write-char-serial 32) (write-char-serial 70) (write-char-serial 61) (%pdec (mem-ref 268438408 :u32))
  (write-char-serial 32) (write-char-serial 67) (write-char-serial 65) (write-char-serial 80) (write-char-serial 61) (%pdec (mem-ref 268436464 :u32))
  (putnl)
  (mem-ref 268438408 :u32))

(defun kernel-main ()
  (let ((which (%argv1)))
    (cond
      ((eql which 1) (probe-gcmeta))
      ((eql which 2) (probe-bulk))
      ((eql which 3) (probe-chain))
      ((eql which 4) (probe-argv))
      ((eql which 5) (probe-eval))
      ((eql which 6) (sys-exit (if (eql (probe-hc) 0) 0 1)))
      ((eql which 7) (sys-exit (if (eql (probe-l5) 0) 0 1)))
      ((eql which 8) (sys-exit (if (eql (probe-fileio) 0) 0 1)))
      ((eql which 9) (probe-load))
      ((eql which 10) (sys-exit (if (eql (probe-sym) 0) 0 1)))
      (t (sys-exit (if (eql (probe-suite) 0) 0 1)))))
  (sys-exit 0))

")


;;; ============================================================
;;; Assemble
;;; ============================================================


;;; ============================================================
;;; Layer-5 boot init + the EVAL probe
;;; ============================================================
;;; Baking the compiler in makes MVM-EVAL RESOLVE; it does not by itself make
;;; EVAL work.  mvm-eval needs the runtime tables the CL bridge builds at boot
;;; (symbols, packages, streams, reader, conditions, the symbol-function
;;; table).  i386's kernel-main never ran any of them — layers 1-4 only ever
;;; used compiled primitives and crypto, which need none.  This is the first
;;; half of the reachability work; the build-GENERATED half (%init-sft-auto,
;;; %init-sym-name-auto, init-all-globals, %install-runtime-cl-macros,
;;; %install-runtime-backquote) does not exist for i386 yet.
;;; Below layer 5 both definitions are stubs, so layer 4 is untouched.

;;; ============================================================
;;; Layer 5, second half: the build-GENERATED bootstrap
;;; ============================================================
;;; Baking the compiler in makes MVM-EVAL resolve; it does not make a compiled
;;; form's CALLS resolve.  mvm-eval compiles a form to bytecode whose calls are
;;; looked up by name in *symbol-function-table*, and that table is not built
;;; from the image's function table — it is POPULATED AT BOOT by a generated
;;; %init-sft-auto that puthashes every defun name to its #'function.  Without
;;; it, (eval 42) SIGSEGVs: the compiled thunk calls into nothing.
;;;
;;; Ported verbatim in shape from build-generic-cli.lisp, which is the arm that
;;; bakes these directly.  Chunked at 200 entries per function for the same
;;; reason it is there: one 3000-entry defun exceeds the compiler's function
;;; size limits.
(defun scan-defuns (text)
  "Return list of defun names (strings) found in TEXT."
  (let ((names nil)
        (pos 0))
    (loop
      (let ((p (search "(defun " text :start2 pos)))
        (unless p (return (nreverse names)))
        (let* ((start (+ p 7))
               (end (or (position-if (lambda (c)
                                       (or (char= c #\Space)
                                           (char= c #\Newline)
                                           (char= c #\()
                                           (char= c #\)))) text
                                     :start start)
                        (length text))))
          (push (string-upcase (subseq text start end)) names)
          (setq pos end))))))

;;; Everything that ends up in the image EXCEPT the generated bootstrap itself
;;; (which is derived FROM this, and whose own defuns — %init-sft-auto-N — do
;;; not need to be callable by name from runtime eval).
(defvar *scanned-source*
  (concatenate 'string
    *prelude-source* (string #\Newline)
    *gc-source* (string #\Newline)
    *rt-source* (string #\Newline)
    *rt-macros-source* (string #\Newline)
    *isa-source* (string #\Newline)
    *interp-source* (string #\Newline)
    *compiler-source* (string #\Newline)
    *float-override-source* (string #\Newline)
    *opcode-table-init-source* (string #\Newline)
    *mvm-eval-source* (string #\Newline)
    *bridge-source* (string #\Newline)
    *i386-fileio-source* (string #\Newline)
    *crypto-source* (string #\Newline)
    ;; BOTH scanners see the backquote expander: the SFT scan below takes its
    ;; defun names, and *sym-name-auto-source* scans this same text for symbol
    ;; names — %rbq-sym-name-eq compares SYMBOL-NAME against "COMMA" /
    ;; "COMMA-AT" / "BACKQUOTE", and a missing sym-name entry turns the whole
    ;; expander into a silent no-op rather than an error.
    *runtime-backquote-source* (string #\Newline)
    *driver-source*))

(defvar *all-defun-names*
  ;; Filter to names that look like valid CL identifiers.
  (remove-if-not (lambda (s)
                   (and (stringp s)
                        (> (length s) 0)
                        (every (lambda (c)
                                 (or (alphanumericp c)
                                     (find c "+-*/<=>?!@$%^&_:|.~")))
                               s)))
                 (scan-defuns *scanned-source*)))

(format t "  defuns found: ~D~%" (length *all-defun-names*))

(defun emit-sft-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sft-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash ~S *symbol-function-table* #'~A)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sft-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sft-auto-~D)~%" c))
      (format out ")~%"))))

(defvar *sft-auto-source*
  (if (>= *i386-layer* 5) (emit-sft-auto *all-defun-names* 200) ""))

;;; ============================================================
;;; Layer 5, third half: *sym-name-table*
;;; ============================================================
;;; Ported from build-generic-cli.lisp, which is the arm that already had it.
;;; Native MVM symbols carry only a name HASH; SYMBOL-NAME recovers the string
;;; by reverse lookup in *sym-name-table*, populated at boot from a build-time
;;; scan of the image source.  Without it EVERY native symbol's name is "",
;;; so NAME-EQ (which compares compute-name-hash of the names) matches
;;; NOTHING — the in-image compiler then fails to recognise even QUOTE, treats
;;; (quote X) as a function call, compiles X as a variable reference, and
;;; recurses until the 8 MB stack is gone.  That is exactly what (eval 42) did:
;;; one `WARN: implicit global #:||` (the empty name) and a SIGSEGV with
;;; ESP below the stack region.

(defun scan-symbol-names (text)
  "Return list of distinct symbol-shaped tokens in TEXT."
  (let ((seen (make-hash-table :test 'equal))
        (result nil)
        (pos 0)
        (len (length text)))
    (loop
      (when (>= pos len) (return (nreverse result)))
      (let ((c (char text pos)))
        (cond
          ((or (char= c #\Space) (char= c #\Newline) (char= c #\Tab)
               (char= c #\Return) (char= c #\Page))
           (incf pos))
          ((char= c #\;)
           ;; comment to end of line
           (loop while (and (< pos len)
                            (not (char= (char text pos) #\Newline)))
                 do (incf pos)))
          ((char= c #\")
           ;; string literal — skip to closing quote
           (incf pos)
           (loop while (and (< pos len) (not (char= (char text pos) #\")))
                 do (when (char= (char text pos) #\\) (incf pos))
                    (incf pos))
           (incf pos))
          ;; Sharp dispatch: #\X character literal, #| ... |# block
          ;; comment, #(...) vector literal, #+/#- feature, etc.  Without
          ;; this special-case, #\" / #\; / #\( etc. fool the string- and
          ;; comment-skippers in the branches above and the scanner ends
          ;; up consuming dozens of legitimate tokens as if they were
          ;; inside a string.  In particular COMMA-AT on line 992 of
          ;; cl-reader.lisp was being eaten by the bogus string-skip
          ;; triggered by #\" on line 957.
          ((char= c #\#)
           (incf pos)
           (when (< pos len)
             (let ((next (char text pos)))
               (cond
                 ;; #\X — skip the backslash + next char (which may be
                 ;; any single character) + any trailing word (e.g.
                 ;; #\Newline / #\Space).
                 ((char= next #\\)
                  (incf pos)  ; past the backslash
                  (when (< pos len) (incf pos))  ; the literal char
                  ;; Multi-char names like Newline / Space / Tab — eat
                  ;; the rest of the word.
                  (loop while (and (< pos len)
                                   (alphanumericp (char text pos)))
                        do (incf pos)))
                 ;; #| ... |# block comment
                 ((char= next #\|)
                  (incf pos)
                  (loop while (and (< (1+ pos) len)
                                   (not (and (char= (char text pos) #\|)
                                             (char= (char text (1+ pos)) #\#))))
                        do (incf pos))
                  (when (< (1+ pos) len) (incf pos) (incf pos))))))
           ;; Other # forms (#( vector, #+ feature, etc.) just fall
           ;; through — the next iteration reads them as ordinary
           ;; tokens.
           )
          ((or (char= c #\() (char= c #\)) (char= c #\') (char= c #\`)
               (char= c #\,))
           (incf pos))
          (t
           ;; symbol token
           (let ((start pos))
             (loop while (and (< pos len)
                              (let ((ch (char text pos)))
                                (not (or (char= ch #\Space) (char= ch #\Newline)
                                         (char= ch #\() (char= ch #\))
                                         (char= ch #\Tab) (char= ch #\")
                                         (char= ch #\;)))))
                   do (incf pos))
             (let ((token (string-upcase (subseq text start pos))))
               (unless (or (gethash token seen)
                           (zerop (length token))
                           (every #'digit-char-p token))
                 (setf (gethash token seen) t)
                 (push token result))
               ;; Also register a keyword token's bare name (without the
               ;; leading colon).  compile-keyword uses (normalize-name
               ;; KW) → hash of "FOO" (not ":FOO"); symbol-name at
               ;; runtime looks up that hash in *sym-name-table*.
               ;; Without this, keywords interned at build-time print
               ;; as :|| because the colon-prefixed entry doesn't match.
               (when (and (> (length token) 1)
                          (char= (char token 0) #\:))
                 (let ((bare (subseq token 1)))
                   (unless (or (gethash bare seen)
                               (zerop (length bare)))
                     (setf (gethash bare seen) t)
                     (push bare result))))))))))))



(defun emit-sym-name-auto (names chunk-size)
  (with-output-to-string (out)
    (let ((n-chunks (ceiling (length names) chunk-size)))
      (dotimes (c n-chunks)
        (format out "(defun %init-sym-name-auto-~D ()~%" c)
        (let ((start (* c chunk-size))
              (end (min (* (1+ c) chunk-size) (length names))))
          (loop for i from start below end
                do (format out "  (puthash (compute-name-hash ~S) *sym-name-table* ~S)~%"
                          (nth i names) (nth i names))))
        (format out ")~%"))
      (format out "(defun %init-sym-name-auto ()~%")
      (dotimes (c n-chunks)
        (format out "  (%init-sym-name-auto-~D)~%" c))
      (format out ")~%"))))

;;; ============================================================
;;; The build-time / runtime hash-agreement probe
;;; ============================================================
;;; THE invariant behind the name-hash: the SAME name hashed by the BUILD and
;;; by the IMAGE must be the SAME NUMBER.  Build-time hashes are baked in as
;;; literals (which :li truncates to the target word); runtime hashes are
;;; recomputed with target arithmetic (where anything wider than a fixnum
;;; becomes a bignum).  Nothing checked that they agreed, and on i386 they do
;;; NOT — COMPUTE-NAME-HASH is 60 bits wide and an i386 fixnum holds 30.
;;;
;;; This bakes the build's own answer in so the image can compare it against
;;; its own.  It is the check that would have caught the whole class, and it
;;; is now a HARD CHECK (%chk): with the hash fixed at 29 bits on every target
;;; the build's literal and the image's recomputation are the same number, and
;;; probe 7 fails the moment they diverge again.
;;;
;;; The width is set here because BUILD-IMAGE only calls
;;; SET-TARGET-FIXNUM-BITS-FOR later; it sets the same value again, so this is
;;; idempotent, not a second source of truth.
(modus.mvm::set-target-fixnum-bits 30)

;;; The --load target and its EXPECTED size, both from ONE computation at
;;; build time.  Hand-transcribing either would repeat the mistake that
;;; produced four false findings during this bring-up; and a stat64 offset
;;; that is merely plausible returns a plausible number, so the expected
;;; value has to come from the same place the file does.
;;;
;;; Generated with FORMAT rather than written into *driver-source*: that
;;; string cannot contain a double-quote character, so a path literal is
;;; impossible there.
(defvar *loadtest-source*
  (if (>= *i386-layer* 5)
      (let* ((path (namestring (merge-pathnames "tests/runtime-metric.lisp"
                                                *modus-base*)))
             (size (with-open-file (s path :element-type '(unsigned-byte 8))
                     (file-length s))))
        (format t "  load target: ~A (~D bytes)~%" path size)
        (format nil "(defun %lt-path () ~S)~%(defun %lt-size () ~D)~%~
                     (defun %lt-nopath () ~S)~%~
                     (defun %lt-s-foo () ~S)~%~
                     (defun %lt-s-defmacro () ~S)~%~
                     (defun %lt-s-use () ~S)~%~
                     (defun %lt-s-defun () ~S)~%~
                     (defun %lt-s-call () ~S)~%~
                     (defun %lt-s-ltm () ~S)~%~
                     (defun %lt-s-defmacro-nobq () ~S)~%~
                     (defun %lt-s-use-nobq () ~S)~%~
                     (defun %lt-s-plus () ~S)~%~
                     (defun %lt-s-carlist () ~S)~%~
                     (defun %lt-s-ltn () ~S)~%~
                     (defun %lt-s-lambda3 () ~S)~%~
                     (defun %lt-s-lambda1 () ~S)~%~
                     (defun %lt-s-list2 () ~S)~%~
                     (defun %lt-s-list3 () ~S)~%~
                     (defun %lt-s-hc-plain () ~S)~%~
                     (defun %lt-s-hc-err () ~S)~%~
                     (defun %lt-s-hc-native () ~S)~%~
                     (defun %lt-s-ift () ~S)~%~
                     (defun %lt-s-cil () ~S)~%"
                path size
                (concatenate 'string path ".does-not-exist")
                "FOO"
                "(defmacro %ltm (a) `(+ ,a 1))"
                "(%ltm 41)"
                "(defun %ltf (a) (* a 6))"
                "(%ltf 7)"
                "%LTM"
                "(defmacro %ltn (a) (list (quote +) a 1))"
                "(%ltn 41)"
                "(+ 41 1)"
                "(car (list 1 2))"
                "%LTN"
                "(lambda (x) (list (quote +) x 1))"
                "(lambda (x) (list x))"
                "(list 1 2)"
                "(list 1 2 3)"
                "(handler-case 7 (error (c) 9))"
                "(handler-case (error (quote simple-error)) (error (c) 9))"
                "(handler-case (progn (rt-inc-probe 41)) (error (c) 9))"
                "(if t 1 2)"
                "(funcall (car (list (lambda () 7))))"))
      "(defun %lt-path () nil)
(defun %lt-size () 0)
(defun %lt-nopath () nil)
(defun %lt-s-foo () nil)
(defun %lt-s-defmacro () nil)
(defun %lt-s-use () nil)
(defun %lt-s-defun () nil)
(defun %lt-s-call () nil)
(defun %lt-s-ltm () nil)
(defun %lt-s-defmacro-nobq () nil)
(defun %lt-s-use-nobq () nil)
(defun %lt-s-plus () nil)
(defun %lt-s-carlist () nil)
(defun %lt-s-ltn () nil)
(defun %lt-s-lambda3 () nil)
(defun %lt-s-lambda1 () nil)
(defun %lt-s-list2 () nil)
(defun %lt-s-list3 () nil)
(defun %lt-s-hc-plain () nil)
(defun %lt-s-hc-err () nil)
(defun %lt-s-hc-native () nil)
(defun %lt-s-ift () nil)
(defun %lt-s-cil () nil)
"))

(defvar *hash-probe-source*
  (if (>= *i386-layer* 5)
      (format nil "(defun %build-hash-abc () ~D)~%(defun %build-hash-quote () ~D)~%"
              (modus.mvm::compute-name-hash "ABC")
              (modus.mvm::compute-name-hash "QUOTE"))
      ""))
(format t "  build-time hash of ABC: ~D (fits a target fixnum: ~A)~%"
        (modus.mvm::compute-name-hash "ABC")
        (if (<= (modus.mvm::compute-name-hash "ABC") modus.mvm::+fixnum-max+) "YES" "NO"))

(format t "  sft-auto: ~D chars~%" (length *sft-auto-source*))

(defvar *l5-init-source*
  (if (>= *i386-layer* 5)
      "
;; One letter per completed step, unbuffered, so a crash names the LAST step
;; that finished rather than leaving the whole boot as one opaque SIGSEGV.
(defun %l5-step (c) (write-char-serial c))

(defun %l5-boot ()
  (init-symbol-table)          (%l5-step 97)   ; a
  (init-keyword-table)         (%l5-step 98)   ; b
  (%init-packages)             (%l5-step 99)   ; c
  (%init-streams)              (%l5-step 100)  ; d
  (%init-reader)               (%l5-step 101)  ; e
  (%init-condition-types)      (%l5-step 102)  ; f
  (%init-method-combinations)  (%l5-step 103)  ; g
  (%init-symbol-function-table) (%l5-step 104) ; h
  ;; The generated table: every compiled defun, by name, so a form compiled
  ;; at runtime can actually CALL them.
  (%init-sft-auto)             (%l5-step 105)  ; i
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)        (%l5-step 109)  ; m
  ;; Run every defvar/defparameter init thunk.  The compiler EMITS
  ;; init-all-globals from the source it compiled, so it is already in the
  ;; image (verified by symmap); nothing had ever called it, which left every
  ;; defparameter the compiler and mvm-eval depend on at NIL — Active
  ;; Limitation #7, and the reason (eval 42) had nothing to work with.
  (init-all-globals)           (%l5-step 106)  ; j
  (%init-signal-handling)      (%l5-step 107)  ; k
  (%init-signal-symbols)       (%l5-step 108)  ; l
  ;; ---- macro / backquote bootstrap ------------------------------------
  ;; AFTER init-all-globals, deliberately: the *macro-table* defvar init
  ;; thunk would otherwise replace the table %init-runtime-macros just
  ;; filled, and %install-runtime-cl-macros needs *modus-runtime-macros*
  ;; to already hold its defvar value.
  (setq *macro-table* (make-hash-table))
  (%init-runtime-macros)       (%l5-step 110)  ; n
  (init-compiler-macro-set)    (%l5-step 111)  ; o
  (%init-make-load-form)       (%l5-step 112)  ; p
  ;; The load-time backquote expander.  A macro defined at RUNTIME keeps
  ;; the reader's COMMA markers in its expansion without it, and the first
  ;; call dies with UNDEFINED-FUNCTION NAME=COMMA.
  (%install-runtime-backquote) (%l5-step 113)  ; q
  ;; Each entry is EVAL of a READ source string, so this is also the first
  ;; real exercise of eval+read together.  %install-runtime-cl-macros
  ;; deliberately carries no internal handler-case (see its docstring), so
  ;; the resilience wrapper belongs here, exactly as in the 64-bit CLIs.
  (handler-case (%install-runtime-cl-macros) (t (c) nil))
                               (%l5-step 114)  ; r
  ;; defvar init thunks that other targets also set explicitly.
  (setq *gensym-counter* 0)
  (setq *gentemp-counter* 0)
  (setq *write-object-budget* 1000000)
  ;; File-I/O scratch, inside the demand-zeroed BSS reserved by
  ;; +linux-i386-bss-end+.  Both are RAW byte addresses below 2^30 so
  ;; syscall3 can carry them as tagged fixnums.
  (setq *cstr-scratch* 268451840)   ; #x10004000
  (setq *io-buf-addr*  268468224)   ; #x10008000
  (setq *scratch-mmapped* t)        ; the BSS is already mapped; never mmap
  (setq *filesystem* nil)
                               (%l5-step 115)  ; s
  (putnl)
  0)

;; probe-l5: the boot steps BEFORE init-all-globals, then the pieces
;; init-all-globals depends on, one at a time.  init-all-globals runs 56
;; generated thunks with no output of its own, so a fault inside one is
;; otherwise a bare SIGSEGV.
(defparameter *probe-tbl* (make-hash-table :test (quote eql)))

(defun %l5-pre ()
  (init-symbol-table)
  (init-keyword-table)
  (%init-packages)
  (%init-streams)
  (%init-reader)
  (%init-condition-types)
  (%init-method-combinations)
  (%init-symbol-function-table)
  (%init-sft-auto)
  (setq *sym-name-table* (make-hash-table))
  (%init-sym-name-auto)
  0)

(defun probe-l5 ()
  (%l5-pre)
  (%sc-reset)
  ;; t1 hash-table basics (make-hash-table takes a &key, so this also
  ;;    exercises the &rest/nargs path init thunks lean on)
  (%tag2 116 49)
  (%chk (let ((h (make-hash-table :test (quote eql)))) (puthash 1 h 2) (gethash 1 h)) 2)
  ;; t2 *opcode-table* is expected NIL before any init thunk has run
  (%tag2 116 50) (%chk (if *opcode-table* 1 0) 0)
  ;; t3 plain SETQ of that same global — isolates the global slot machinery
  ;;    from the generated thunk
  (setq *opcode-table* 7)
  (%tag2 116 51) (%chk (if (eql *opcode-table* 7) 1 0) 1)
  (setq *opcode-table* nil)
  ;; t4 a generated init thunk for a global defined HERE, in the driver
  (init-*probe-tbl*)
  (%tag2 116 52) (%chk (if *probe-tbl* 1 0) 1)
  ;; t5 the real one, in isolation
  (init-*opcode-table*)
  (%tag2 116 53) (%chk (if *opcode-table* 1 0) 1)
  ;; t6 the populate thunk, which is what SIGSEGVs inside init-all-globals
  (%populate-opcode-table)
  (%tag2 116 54) (%chk (if (gethash 1 *opcode-table*) 1 0) 1)
  ;; t7-t9 THE (EVAL 42) FRONTIER.  compute-name-hash is 60 bits wide by
  ;; construction and i386 fixnums hold 30, so the hash is a BIGNUM at
  ;; RUNTIME.  compile-variable-ref splices it into
  ;; (%e2-symbol-value-checked <hash> (quote <name>)) as a literal; if
  ;; compile-form then misclassifies that bignum as a SYMBOL the compiler
  ;; recurses on it forever — which is what (eval 42) does, printing
  ;; `WARN: implicit global #:||` (empty name) and running the 8 MB stack out.
  (%tag2 116 55) (%chk (if (integerp (normalize-name (quote abc))) 1 0) 1)
  (%tag2 116 56) (%chk (if (bignump (normalize-name (quote abc))) 1 0) 0)
  (%tag2 116 57) (%chk (if (symbolp (normalize-name (quote abc))) 1 0) 0)
  ;; t10-t11 does the *sym-name-table* reverse lookup actually work?  Its keys
  ;; are name hashes, i.e. BIGNUMS here, so every lookup rides EQL-on-bignums.
  (%tag2 49 48) (%chk (length (symbol-name (quote abc))) 3)
  (%tag2 49 49) (%chk (if (gethash (normalize-name (quote abc)) *sym-name-table*) 1 0) 1)
  ;; t12 THE ROOT: two hashes of the SAME name are two distinct bignum
  ;; OBJECTS, and EQL on bignums is identity here — so every hash-keyed
  ;; table (sym-name, intern, macro, symbol-function) misses on i386.
  (%tag2 49 50) (%chk (if (eql (normalize-name (quote abc))
                                (normalize-name (quote abc))) 1 0) 1)
  ;; t13 THE MISMATCH, both halves side by side.  A quoted symbol's STORED
  ;; hash comes from a BUILD-TIME literal, which :li truncates to 32 bits, so
  ;; it is a fixnum.  The same name hashed IN-IMAGE is a 60-bit bignum (t8).
  ;; They are different numbers, so no hash-keyed table can match them.
  (%tag2 49 51) (%chk (if (eql (%prim-aref (quote abc) 0)
                                (normalize-name (quote abc))) 1 0) 1)
  ;; t14-t15 BUILD-TIME vs RUNTIME.  %build-hash-abc returns the hash the
  ;; BUILD computed, baked in as a literal and therefore subject to the same
  ;; :li truncation every other baked hash gets.  normalize-name recomputes it
  ;; in-image with target arithmetic.  These two agreeing is the invariant.
  (%tag2 49 52) (%chk (if (eql (%build-hash-abc) (normalize-name (quote abc))) 1 0) 1)
  (%tag2 49 53) (%chk (if (eql (%build-hash-quote) (normalize-name (quote quote))) 1 0) 1)
  (write-char-serial 80) (write-char-serial 61) (%pdec (mem-ref 268438400 :u32))
  (write-char-serial 32) (write-char-serial 70) (write-char-serial 61) (%pdec (mem-ref 268438408 :u32))
  (putnl)
  (mem-ref 268438408 :u32))

;; Does EVAL actually evaluate?  Each step is separate so a failure localises:
;; a bare constant needs only the interpreter, arithmetic needs the compiler's
;; opcode emission, and the multiply exercises the promotion path through the
;; whole compile-to-bytecode-and-interpret round trip.
(defun probe-eval ()
  (%l5-boot)
  (write-char-serial 101) (write-char-serial 49) (write-char-serial 61)
  (%pdec (eval 42)) (putnl)
  (write-char-serial 101) (write-char-serial 50) (write-char-serial 61)
  (%pdec (eval (list (quote +) 1 2))) (putnl)
  (write-char-serial 101) (write-char-serial 51) (write-char-serial 61)
  (%pdec (eval (list (quote *) 6 7))) (putnl)
  (write-char-serial 101) (write-char-serial 52) (write-char-serial 61)
  (%pdec (ash (eval (list (quote *) 17034 65536)) -24)) (putnl)
  ;; e5: a REAL cross-function call through the symbol-function table.  CAR of
  ;; a freshly consed LIST cannot be constant-folded, so this only answers 1 if
  ;; the compiled thunk actually resolved and called two native functions by
  ;; name.
  (write-char-serial 101) (write-char-serial 53) (write-char-serial 61)
  (%pdec (eval (list (quote car) (list (quote list) 1 2)))) (putnl)
  ;; e6: DEFINE in one top-level form, CALL BY NAME from a later one — the
  ;; shape every library's interior has, and the one a load-time metric cannot
  ;; see.  Forms are built with LIST rather than written as literals so the
  ;; build's scan-defuns does not mistake a quoted (defun ...) for a real one.
  (write-char-serial 101) (write-char-serial 54) (write-char-serial 61)
  (%pdec (mvm-eval-forms
           (list (list (quote defun) (quote %e6f) (list (quote x))
                       (list (quote *) (quote x) 3))
                 (list (quote %e6f) 14))))
  (putnl)
  ;; e7: the same define/call split across TWO SEPARATE eval calls — which is
  ;; what LOAD does, one top-level form at a time.  e6 only proved a call
  ;; inside one compilation unit; this proves the definition PERSISTS into the
  ;; next one.
  (write-char-serial 101) (write-char-serial 55) (write-char-serial 61)
  (eval (list (quote defun) (quote %e7f) (list (quote x))
              (list (quote +) (quote x) 4)))
  (%pdec (eval (list (quote %e7f) 38))) (putnl)
  ;; e8: a RUNTIME defmacro whose body is a BACKQUOTE template, again across
  ;; two evals.  The reader produces exactly this (BACKQUOTE (+ (COMMA a) 1))
  ;; shape; building it by hand keeps the probe independent of the reader.
  ;; Answers 42 only if %install-runtime-backquote registered the expander AND
  ;; symbol-name can recover COMMA / BACKQUOTE from *sym-name-table* — without
  ;; the latter the expander is a SILENT no-op and the COMMA marker survives
  ;; into the expansion.
  ;; PRINTS 0 OR 1, NEVER THE VALUE.  This check currently FAILS (0), and the
  ;; failing value is a heap object, not a number -- handing that to %pdec
  ;; printed a tagged pointer as byte garbage, which reads like display noise
  ;; rather than the real failure it is.  A probe must never let a red light
  ;; look like static.
  (write-char-serial 101) (write-char-serial 56) (write-char-serial 61)
  (eval (list (quote defmacro) (quote %e8m) (list (quote a))
              (list (quote backquote)
                    (list (quote +) (list (quote comma) (quote a)) 1))))
  (%pdec (if (eql (eval (list (quote %e8m) 41)) 42) 1 0)) (putnl)
  0)

;; probe-fileio: the i386 syscall numbers and the struct stat64 offsets,
;; measured rather than assumed.  Every expected value is baked by the build
;; from the same file the image opens.
(defun probe-fileio ()
  (%l5-boot)
  (%sc-reset)
  (%tag2 102 49) (%chk (if (%sys-stat-exists (%lt-path)) 1 0) 1)
  (%tag2 102 50) (%chk (if (%sys-stat-exists (%lt-nopath)) 1 0) 0)
  (%tag2 102 51) (%chk (%sys-stat-size (%lt-path)) (%lt-size))
  (%tag2 102 52) (%chk (let ((fd (%sys-open-rdonly (%lt-path))))
                         (let ((n (%sys-fstat-size fd)))
                           (%sys-close fd)
                           n))
                       (%lt-size))
  (%tag2 102 53) (%chk (if (> (%sys-stat-mtime (%lt-path)) 1000000000) 1 0) 1)
  (%tag2 102 54) (%chk (let ((s (open (%lt-path))))
                         (let ((c (read-char s nil nil)))
                           (close s)
                           (if c (char-code c) 0)))
                       59)
  (write-char-serial 80) (write-char-serial 61) (%pdec (mem-ref 268438400 :u32))
  (write-char-serial 32) (write-char-serial 70) (write-char-serial 61) (%pdec (mem-ref 268438408 :u32))
  (putnl)
  (mem-ref 268438408 :u32))

;; probe-load: the real thing — LOAD a file of ordinary CL, form by form.
;; WATCH THE OUTPUT SHAPE, not just the values: a failed-first-compile retry
;; re-executes a whole top-level form, which prints `name=name=42' rather
;; than `name=42'.  A value-only check cannot see that.
(defun probe-load ()
  (%l5-boot)
  (load (%lt-path))
  0)

;; probe-sym: the symbol / macro-registration battery.  Runtime DEFMACRO is
;; the one gap LOAD still trips over, and it fails SILENTLY (%macro-sym-key
;; returning NIL makes set-macro-function a no-op), so each tier is measured
;; separately: what a native quoted symbol looks like, what the READER
;; produces, whether the two key helpers accept each, and finally the whole
;; read-defmacro-eval-use round trip.
(defun %lt-dummy-expander (mform) 7)
;; NATIVE control for the &rest-arity probes: same body shape as a macro
;; expander, but compiled by the BUILD rather than by mvm-eval.
(defun %lt-native-list3 (x) (list (quote +) x 1))
;; Stand-in for runtime-metric's rt-inc: a NATIVE fn called from inside an
;; EVAL'd handler-case body, which is the exact shape that fails.
(defun rt-inc-probe (x) (+ x 1))

;; Byte-level helpers for the :li-const wire-format probe.  Each returns a
;; single small integer so %pdec can never be handed a bignum or NIL (it would
;; spin forever subtracting).
(defun %dbg-u64-byte (val i)
  (let ((b (make-mvm-buffer :bytes (make-array 64))))
    (mvm-emit-u64 b val)
    (aref (mvm-buffer-bytes b) i)))
(defun %dbg-u32-byte (val i)
  (let ((b (make-mvm-buffer :bytes (make-array 64))))
    (mvm-emit-u32 b val)
    (aref (mvm-buffer-bytes b) i)))
(defun %dbg-u64-fetch-lo (val)
  (let ((b (make-mvm-buffer :bytes (make-array 64))))
    (mvm-emit-u64 b val)
    (let ((bc (mvm-buffer-bytes b)))
      (logior (aref bc 0) (ash (aref bc 1) 8)
              (ash (aref bc 2) 16) (ash (aref bc 3) 24)))))

(defun probe-sym ()
  (%l5-boot)
  (%sc-reset)
  (%tag2 115 49) (%chk (obj-subtag (quote foo)) 80)
  (%tag2 115 50) (%chk (if (symbolp (quote foo)) 1 0) 1)
  (%tag2 115 51) (%chk (length (symbol-name (quote foo))) 3)
  (%tag2 115 52) (%chk (if (%sym-name-or-hash (quote foo)) 1 0) 1)
  (%tag2 115 53) (%chk (if (%macro-sym-key (quote foo)) 1 0) 1)
  (%tag2 115 54) (%chk (if (%cl-sym-p (read-from-string (%lt-s-foo))) 1 0) 1)
  (%tag2 115 55) (%chk (if (symbolp (read-from-string (%lt-s-foo))) 1 0) 1)
  (%tag2 115 56) (%chk (if (%sym-name-or-hash (read-from-string (%lt-s-foo))) 1 0) 1)
  (%tag2 115 57) (%chk (if (%macro-sym-key (read-from-string (%lt-s-foo))) 1 0) 1)
  ;; read + eval a DEFUN, then read + eval a call to it — the LOAD shape,
  ;; through the real reader rather than a hand-built form.
  (%tag2 114 49) (%chk (progn (eval (read-from-string (%lt-s-defun)))
                              (eval (read-from-string (%lt-s-call))))
                       42)
  ;; the same for a DEFMACRO whose body is a real reader backquote.
  (%tag2 114 50) (%chk (progn (eval (read-from-string (%lt-s-defmacro)))
                              (if (macro-function (read-from-string
                                                    (%lt-s-foo)))
                                  0 1))
                       1)
  (%tag2 114 51) (%chk (progn (eval (read-from-string (%lt-s-defmacro)))
                              (eval (read-from-string (%lt-s-use))))
                       42)
  ;; Where along the registration chain does a runtime DEFMACRO stop?  Each
  ;; tier separately, all AFTER the defmacro above has been eval'd.
  (%tag2 109 49) (%chk (if *macro-function-table* 1 0) 1)
  (%tag2 109 50) (%chk (if (%macro-sym-key (%lt-s-ltm)) 1 0) 1)
  (%tag2 109 51) (%chk (if (gethash (%lt-s-ltm) *macro-function-table*) 1 0) 1)
  (%tag2 109 52) (%chk (if (%raw-macro-expander (read-from-string (%lt-s-ltm))) 1 0) 1)
  (%tag2 109 53) (%chk (if (macro-function (read-from-string (%lt-s-ltm))) 1 0) 1)
  ;; set-macro-function in isolation: does the plumbing work at all when the
  ;; compiled DEFMACRO body is taken out of the picture?
  (%tag2 109 54) (%chk (progn (set-macro-function (quote %ltz)
                                                  (function %lt-dummy-expander))
                              (if (macro-function (quote %ltz)) 1 0))
                       1)
  (%tag2 109 55) (%chk (if (%raw-macro-expander (quote %ltz)) 1 0) 1)
  ;; QUOTE under mvm-eval.  compile-quote's runtime arm registers the ORIGINAL
  ;; object in *e2-const-pool* and emits :li-const IDX; the interpreter loads
  ;; it back, which is what makes (eval '(quote X)) return X itself.  If that
  ;; round trip drops the object, EVERY compiled `',name' is NIL — which is
  ;; exactly the shape set-macro-function was handed above (obj-subtag 0).
  ;; Every check yields 0 or 1: %pdec of a NIL got would spin forever, so a
  ;; probe must never hand it one.
  (%tag2 113 49) (%chk (if (eval (list (quote quote) (quote foo))) 1 0) 1)
  (%tag2 113 50) (%chk (if (eql (eval (list (quote quote) (quote foo)))
                                (quote foo)) 1 0) 1)
  (%tag2 113 51) (%chk (if (eval (list (quote quote) (list 7 8))) 1 0) 1)
  (%tag2 113 52) (%chk (if (eql (car (eval (list (quote quote) (list 7 8)))) 7) 1 0) 1)
  (%tag2 113 53) (%chk (if *e2-const-pool* 1 0) 1)
  (%tag2 113 54) (%chk (if (gethash 0 *e2-const-pool*) 1 0) 1)
  ;; The wire format underneath :li-const.  Under mvm-eval every OTHER 64-bit
  ;; immediate goes out as :li-halves; :li-const is the last user of the raw
  ;; mvm-emit-u64 / fetch-u64 pair, and that pair is built from #xFFFFFFFF
  ;; masks — a BIGNUM at this word size, where it is a fixnum on both 64-bit
  ;; targets.  Round-trip it directly rather than inferring.
  (%tag2 120 49) (%chk (let ((b (make-mvm-buffer :bytes (make-array 64))))
                         (mvm-emit-u64 b 7)
                         (mvm-buffer-position b))
                       8)
  (%tag2 120 50) (%chk (%dbg-u64-byte 7 0) 7)
  (%tag2 120 51) (%chk (%dbg-u64-byte 7 1) 0)
  (%tag2 120 52) (%chk (%dbg-u64-byte 7 2) 0)
  (%tag2 120 53) (%chk (%dbg-u64-byte 7 3) 0)
  (%tag2 120 54) (%chk (%dbg-u64-byte 7 4) 0)
  (%tag2 120 55) (%chk (%dbg-u32-byte 7 0) 7)
  (%tag2 120 56) (%chk (%dbg-u32-byte 7 1) 0)
  (%tag2 120 57) (%chk (%dbg-u32-byte 7 2) 0)
  (%tag2 122 49) (%chk (%dbg-u64-fetch-lo 7) 7)
  (%tag2 122 50) (%chk (if (eql (ash 7 -16) 0) 1 0) 1)
  (%tag2 122 51) (%chk (if (eql (ash 7 -32) 0) 1 0) 1)
  (%tag2 122 52) (%chk (logand (ash 7 -16) 255) 0)
  (%tag2 122 53) (%chk (let ((b (make-mvm-buffer :bytes (make-array 64))))
                         (mvm-li-const b 5 3)
                         (mvm-buffer-position b))
                       10)
  ;; The sub-step underneath, isolated.  #xFFFFFFFF is a BIGNUM at this word
  ;; size, so `(logand 7 #xFFFFFFFF)' can hand back a bignum whose value is 7;
  ;; `(ash <bignum> -16)' then shifts the HEAP POINTER (Active Limitation 8)
  ;; and the byte comes out as (address >> 16) & 255.  y1 says which of the
  ;; two it is; y2-y4 say whether the corruption is visible downstream.
  (%tag2 121 49) (%chk (if (fixnump (logand 7 4294967295)) 1 0) 1)
  (%tag2 121 50) (%chk (if (eql (logand 7 4294967295) 7) 1 0) 1)
  (%tag2 121 51) (%chk (if (eql (ash (logand 7 4294967295) -16) 0) 1 0) 1)
  (%tag2 121 52) (%chk (if (eql (logand (ash (logand 7 4294967295) -16) 255) 0) 1 0) 1)
  ;; r3 is the last red light: a runtime DEFMACRO now REGISTERS (m3-m5) but its
  ;; call still yields a heap object rather than 42.  Split expansion from
  ;; evaluation, and backquote from macro dispatch, so the next session starts
  ;; from a located fault rather than a symptom.
  (%tag2 110 49) (%chk (if (consp (macroexpand-1 (read-from-string (%lt-s-use)))) 1 0) 1)
  (%tag2 110 50) (%chk (if (eql (cadr (macroexpand-1 (read-from-string (%lt-s-use)))) 41) 1 0) 1)
  (%tag2 110 51) (%chk (if (eql (caddr (macroexpand-1 (read-from-string (%lt-s-use)))) 1) 1 0) 1)
  (%tag2 110 52) (%chk (if (eql (eval (macroexpand-1 (read-from-string (%lt-s-use)))) 42) 1 0) 1)
  ;; A macro whose body builds the expansion with LIST instead of a backquote:
  ;; separates the runtime backquote expander from the macro-dispatch path.
  (%tag2 110 53) (%chk (progn (eval (read-from-string (%lt-s-defmacro-nobq)))
                              (if (eql (eval (read-from-string (%lt-s-use-nobq))) 42) 1 0))
                       1)
  ;; The narrowest form of the same question, with no macro in sight: a
  ;; TOP-LEVEL expression whose operator is a READER-produced CL symbol naming
  ;; a compiler builtin.  e2 already proves the identical form works when the
  ;; operator is a NATIVE symbol built with LIST.
  ;; The expansion's OPERATOR, which n1-n3 never looked at.
  (%tag2 118 49) (%chk (length (macroexpand-1 (read-from-string (%lt-s-use)))) 3)
  (%tag2 118 50) (%chk (if (symbolp (car (macroexpand-1 (read-from-string (%lt-s-use))))) 1 0) 1)
  (%tag2 118 51) (%chk (length (symbol-name (car (macroexpand-1 (read-from-string (%lt-s-use)))))) 1)
  (%tag2 118 52) (%chk (if (eql (normalize-name (car (macroexpand-1 (read-from-string (%lt-s-use)))))
                                (normalize-name (quote +))) 1 0)
                       1)
  ;; And the same for a fresh list built at runtime with the SAME operator
  ;; object, to separate a wrong operator from a wrong list.  (No double
  ;; quote may appear anywhere in this source -- it is a LISP STRING; the
  ;; file says so at the top and it still caught me once.)
  (%tag2 118 53) (%chk (if (eql (eval (list (car (macroexpand-1 (read-from-string (%lt-s-use))))
                                            41 1))
                                42) 1 0)
                       1)
  ;; v1 says the expansion is 4 long while v2-v5 say its first three elements
  ;; are exactly right and eval-able.  So the expander appends ONE extra
  ;; argument.  v6 asks what it is; v7 asks whether a LIST-built macro body
  ;; (no backquote at all) gets the same extra element, which separates the
  ;; backquote expander from the expander CALL convention.
  (%tag2 118 54) (%chk (if (null (cadddr (macroexpand-1 (read-from-string (%lt-s-use))))) 1 0) 1)
  (%tag2 118 55) (%chk (progn (eval (read-from-string (%lt-s-defmacro-nobq)))
                              (length (macroexpand-1 (read-from-string (%lt-s-use-nobq)))))
                       3)
  ;; WHERE is the extra NIL introduced?  w1/w2 call the REGISTERED expander
  ;; directly with exactly one argument, bypassing macroexpand-1 entirely.
  ;; w3 is the native control (must be 3).  w4 is the decisive one: a lambda
  ;; compiled at RUNTIME by mvm-eval, funcalled with one argument, whose body
  ;; is the same (list ...) call and NO macro machinery is involved at all.
  ;;   w4 = 4  -> every runtime-compiled lambda over-counts its &rest callee
  ;;   w4 = 3  -> the fault is in the macro dispatch, not the lambda body
  ;; w5 uses a 1-element list so an off-by-one shows as 2 rather than needing
  ;; the operator to be right.
  (%tag2 119 49) (%chk (length (funcall (%raw-macro-expander (read-from-string (%lt-s-ltm)))
                                        (read-from-string (%lt-s-use))))
                       3)
  (%tag2 119 50) (%chk (progn (eval (read-from-string (%lt-s-defmacro-nobq)))
                              (length (funcall (%raw-macro-expander
                                                 (read-from-string (%lt-s-ltn)))
                                               (read-from-string (%lt-s-use-nobq)))))
                       3)
  (%tag2 119 51) (%chk (length (%lt-native-list3 41)) 3)
  (%tag2 119 52) (%chk (length (funcall (eval (read-from-string (%lt-s-lambda3))) 41)) 3)
  (%tag2 119 53) (%chk (length (funcall (eval (read-from-string (%lt-s-lambda1))) 41)) 1)
  ;; Is the +1 confined to a runtime-compiled LAMBDA, or does it hit every
  ;; mvm-eval call into a native &rest function?  u1-u3 are native controls.
  ;; u4/u5 are the same &rest call made from a TOP-LEVEL eval, with no lambda
  ;; anywhere.  p2 could not see this: (car (list 1 2 NIL)) is still 1.
  ;; NOTE the same shape answers correctly on x64, so whatever this is, it is
  ;; i386-local -- the shared compiler/interp are not at fault.
  (%tag2 117 49) (%chk (length (list 1 2)) 2)
  (%tag2 117 50) (%chk (length (funcall (function list) 1 2)) 2)
  (%tag2 117 51) (%chk (length (apply (function list) (list 1 2))) 2)
  (%tag2 117 52) (%chk (length (eval (read-from-string (%lt-s-list2)))) 2)
  (%tag2 117 53) (%chk (length (eval (read-from-string (%lt-s-list3)))) 3)
  ;; LIST is a BOOTSTRAP MACRO (register-mvm-bootstrap-macros expands
  ;; (list a b) -> (cons a (cons b nil))), not a call -- which is why d1/d2
  ;; measured some other bridge call and looked deceptively clean.  So the
  ;; extra element is introduced either by the READER (g1: does the read form
  ;; itself carry a trailing NIL?) or by the expansion (g2: same form built
  ;; natively, so the reader cannot be involved).
  (%tag2 103 49) (%chk (length (read-from-string (%lt-s-list2))) 3)
  (%tag2 103 50) (%chk (length (eval (list (quote list) 1 2))) 2)
  (%tag2 103 51) (%chk (length (eval (list (quote cons) 1 (list (quote cons) 2 nil)))) 2)
  ;; g3 removes the LIST macro, the reader and &rest from the picture, so the
  ;; remaining suspects are the innermost (cons X nil) itself and mvm-eval's
  ;; COMPILE CACHE (keyed by EQUAL on the form list -- a wrong hit returns
  ;; another form's module).  h1-h4 test the cons; h5-h6 re-run the failing
  ;; shapes with the cache switched off.
  (%tag2 104 49) (%chk (length (eval (list (quote cons) 2 nil))) 1)
  (%tag2 104 50) (%chk (if (null (cdr (eval (list (quote cons) 2 nil)))) 1 0) 1)
  (%tag2 104 51) (%chk (if (null (eval nil)) 1 0) 1)
  (%tag2 104 52) (%chk (if (consp (eval nil)) 1 0) 0)
  (setq *mvm-eval-no-cache* t)
  (%tag2 104 53) (%chk (length (eval (list (quote cons) 1 (list (quote cons) 2 nil)))) 2)
  (%tag2 104 54) (%chk (length (eval (list (quote list) 1 2))) 2)
  (%tag2 104 55) (%chk (length (eval (read-from-string (%lt-s-list2)))) 2)
  (setq *mvm-eval-no-cache* nil)
  ;; HANDLER-CASE under mvm-eval -- the last gap.  It works NATIVELY (probe 6
  ;; is 8/8), so this splits the eval'd path three ways: a body that cannot
  ;; signal at all (c1 -- if THIS fails, setjmp/clear-handler is broken under
  ;; eval, nothing to do with signalling), a body that deliberately signals
  ;; (c2), and a body that merely calls a NATIVE function (c3 -- the
  ;; runtime-metric shape).
  (%tag2 99 49) (%chk (if (eql (eval (read-from-string (%lt-s-hc-plain))) 7) 1 0) 1)
  (%tag2 99 50) (%chk (if (eql (eval (read-from-string (%lt-s-hc-err))) 9) 1 0) 1)
  (%tag2 99 51) (%chk (if (eql (eval (read-from-string (%lt-s-hc-native))) 42) 1 0) 1)
  ;; The T IMMEDIATE through mvm-eval.  op-LI picks its load path from the
  ;; HIGH 32 bits only (>= 2^30), which is a 62-bit-tower test: a word with
  ;; hi=0 but lo >= 2^30 -- exactly #xDEAD1009 and #xDEAD0001 -- takes the
  ;; reg-set branch, where %word->val :sar's what is a BIGNUM at this width.
  ;; compile-t emits (:li dest +t-value+), so this would corrupt every T
  ;; literal in an eval'd form.  MEASURE before believing it.
  (%tag2 107 49) (%chk (if (eval t) 1 0) 1)
  (%tag2 107 50) (%chk (if (eq (eval t) t) 1 0) 1)
  (%tag2 107 51) (%chk (if (eql (eval (read-from-string (%lt-s-ift))) 1) 1 0) 1)
  ;; And the runtime-metric SIGSEGV, isolated: a closure reached through a list
  ;; and funcalled with ZERO arguments.
  (%tag2 107 52) (%chk (if (eql (eval (read-from-string (%lt-s-cil))) 7) 1 0) 1)
  (%tag2 112 49) (%chk (if (eql (eval (read-from-string (%lt-s-plus))) 42) 1 0) 1)
  (%tag2 112 50) (%chk (if (eql (eval (read-from-string (%lt-s-carlist))) 1) 1 0) 1)
  (write-char-serial 80) (write-char-serial 61) (%pdec (mem-ref 268438400 :u32))
  (write-char-serial 32) (write-char-serial 70) (write-char-serial 61) (%pdec (mem-ref 268438408 :u32))
  (putnl)
  (mem-ref 268438408 :u32))
"
      "
(defun %l5-boot () 0)
(defun probe-eval () (write-char-serial 110) (write-char-serial 97) (putnl) 0)
(defun probe-l5 () (write-char-serial 110) (write-char-serial 97) (putnl) 0)
"))

;;; Names the token scanner CANNOT see, fed to it explicitly.
;;;
;;; scan-symbol-names deliberately skips string literals, so the reader's
;;; backquote markers — which appear in lib/runtime-backquote.lisp and
;;; mvm/cl-reader.lisp only as the STRINGS "COMMA" / "COMMA-AT" /
;;; "COMMA-DOT" / "BACKQUOTE" — never reach *SYM-NAME-TABLE*.  That is not a
;;; loud failure: %rbq-sym-name-eq compares (symbol-name sym) against those
;;; strings, symbol-name returns "" for an unregistered hash, no branch
;;; matches, and the whole runtime backquote expander degrades to a SILENT
;;; NO-OP.  Listing them here is scanner input only — this text is never
;;; compiled into the image.
(defvar *extra-sym-names*
  "comma comma-at comma-dot backquote quasiquote")

(defvar *sym-name-auto-source*
  (if (>= *i386-layer* 5)
      ;; Scan the GENERATED layer-5 sources too, not just *scanned-source*.
      ;; A name that is only ever written in %l5-boot / probe-l5 (or in the
      ;; baked hash probe) still needs a *sym-name-table* entry, or
      ;; SYMBOL-NAME returns "" for it and every name-based check silently
      ;; degrades — which is exactly how t10/t11/t13/t14 read as real gaps
      ;; when they were measuring an unscanned probe name.
      (emit-sym-name-auto
       (scan-symbol-names (concatenate 'string *scanned-source* (string #\Newline)
                                       *l5-init-source* (string #\Newline)
                                       *hash-probe-source* (string #\Newline)
                                       *loadtest-source* (string #\Newline)
                                       *extra-sym-names*))
       200)
      ""))
(format t "  sym-name-auto: ~D chars~%" (length *sym-name-auto-source*))

;;; MODUS_I386_DBG=<path>: append one extra source file LAST, so a
;;; last-defun-wins override can instrument an in-image function without
;;; touching the shared mvm/*.lisp (which x64/aarch64 also compile).  Empty
;;; by default; nothing in a normal build reads it.
(defvar *dbg-source*
  (let ((p (sb-ext:posix-getenv "MODUS_I386_DBG")))
    (if (and p (plusp (length p)))
        (progn (format t "  DBG override: ~A~%" p) (read-file-text p))
        "")))

(defvar *full-source*
  (concatenate 'string
    *prelude-source* (string #\Newline)
    *gc-source* (string #\Newline)
    *rt-source* (string #\Newline)
    *rt-macros-source* (string #\Newline)
    ;; Layer 5, in build-generic-cli's order: ISA + interpreter first (so the
    ;; compiler's emitted opcodes have their definitions), then the compiler,
    ;; then the in-image float override, the materialised opcode table, and
    ;; mvm-eval last since it ties compile and interpret together.
    *isa-source* (string #\Newline)
    *interp-source* (string #\Newline)
    *compiler-source* (string #\Newline)
    *float-override-source* (string #\Newline)
    *opcode-table-init-source* (string #\Newline)
    *mvm-eval-source* (string #\Newline)
    *runtime-macros-source* (string #\Newline)
    *sft-auto-source* (string #\Newline)
    *sym-name-auto-source* (string #\Newline)
    *hash-probe-source* (string #\Newline)
    *loadtest-source* (string #\Newline)
    *l5-init-source* (string #\Newline)
    *bridge-source* (string #\Newline)
    *i386-fileio-source* (string #\Newline)
    *crypto-source* (string #\Newline)
    *runtime-backquote-source* (string #\Newline)
    *driver-source* (string #\Newline)
    *dbg-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

(in-package :modus.mvm)

(modus.mvm.i386:install-i386-translator)

(setf modus.mvm.i386::*i386-linux-mode* t)
(modus.mvm.i386::i386-set-globals-base +linux-i386-globals+)
(let ((v (sb-ext:posix-getenv "MODUS_I386_GCSTRESS")))
  (when (and v (plusp (length v)))
    (setf modus.mvm.i386::*i386-gc-stress-limit* (parse-integer v))
    (format t "~&  GC stress limit: ~D bytes between collections~%"
            modus.mvm.i386::*i386-gc-stress-limit*)))
(let ((v (sb-ext:posix-getenv "MODUS_I386_VL")))
  (when (and v (plusp (length v)))
    (setf modus.mvm::*linux-i386-vl-offset* (parse-integer v))
    (format t "~&  VL offset overridden: ~D~%" modus.mvm::*linux-i386-vl-offset*)))
(setf modus.mvm.i386::*i386-fn-tag-3* t)
(setf modus.mvm.i386::*i386-fn-align* 16)
(setf modus.mvm.i386::*i386-native-code-offset* 0)
(setf modus.mvm.i386::*i386-checked-arith-slowpath*
      (let ((v (sb-ext:posix-getenv "MODUS_I386_NO_CHECKED")))
        (not (and v (plusp (length v))))))
;; WS5: the NATIVE i386 Cheney collector is ON by default (the third arch arm,
;; alongside x64's emit-gc-trampoline and aarch64's native MCGC).  It landed
;; green: SHA-256 over 64 KiB produces the digest Python hashlib does, across 8
;; collections; a cons chain survives 6 forced collections intact; 4 KiB of
;; SHA-256 is exact under a collection every 1 MB.
;;
;; ---- ENV KNOBS (the complete list; defaults are what ships) ---------------
;; PRODUCTION
;;   MODUS_I386_OUT=<path>    where to write the image
;;                            (default /home/claude/ws5-gate-out/modus-i386-cli)
;;   MODUS_I386_SYMMAP=<path> symbol map location (default: image path + .symmap)
;;   MODUS_I386_LAYER=1..5    how much of the stack to bake in.  1 prelude,
;;                            2 +gc/rt, 3 +the CL bridge, 4 +crypto.  Default 1
;;                            during bring-up; the suite needs 4.
;; DEV / TRIAGE ONLY
;;   MODUS_I386_GC=0          build WITHOUT the collector.  Every allocation is
;;                            then permanent, the arena is the whole lifetime
;;                            budget, and bulk work dies honestly at its edge.
;;   MODUS_I386_BMP=0         drop the object-start / cons-kind bit-set.  Only
;;                            sensible together with GC=0: without the bitmaps
;;                            scan_word has no conservative-root validation and
;;                            copy_object stamps forwarding pointers over
;;                            mid-object data.
;;   MODUS_I386_VL=<bytes>    first GC trigger, so a collection can be provoked
;;                            in seconds instead of gigabytes.
;;   MODUS_I386_GCSTRESS=<bytes>
;;                            keep collecting at that interval forever.  A
;;                            copying collector's corruption shows up at the
;;                            SECOND collection — the first leaves the old
;;                            semispace intact — so forcing dozens of cycles
;;                            out of a small workload is what makes the
;;                            survival tests cheap.
;;   MODUS_I386_NO_CHECKED=1  disable the checked-arith slow paths, isolating
;;                            the overflow-promotion ops from the rest of
;;                            i386 codegen when triaging a miscompile.
;; Everything is driven through scripts/run-i386.sh; see that script first.
(flet ((envp (n dflt) (let ((v (sb-ext:posix-getenv n)))
                        (if (and v (plusp (length v)))
                            (not (string= v "0"))
                            dflt))))
  (let ((gcon (envp "MODUS_I386_GC" t))
        (bmpon (envp "MODUS_I386_BMP" t)))
    (setf modus.mvm.i386::*i386-gc-bitmap-enabled* bmpon)
    (setf modus.mvm.i386::*i386-gc-enabled* gcon)
    (format t "~&  GC: collector=~A bitmap=~A~%" gcon bmpon)))
(setf modus.mvm.i386::*i386-record-unimpl* t)
(setf modus.mvm.i386::*i386-unimpl-ops* nil)

;;; The symmap follows the IMAGE path (or MODUS_I386_SYMMAP if you want it
;;; somewhere else).  It used to be a fixed absolute path, which meant any
;;; build — including a verification build in a detached worktree, pointed at
;;; its own MODUS_I386_OUT — silently overwrote the shared symmap of whatever
;;; else was there.  A fixed output path is exactly the wrong shape for the
;;; concurrent / detached-worktree gating this workstream now uses.
(defvar *i386-image-path*
  (or #+sbcl (sb-ext:posix-getenv "MODUS_I386_OUT")
      "/home/claude/ws5-gate-out/modus-i386-cli"))
(setf *write-symmap-path*
      (or #+sbcl (sb-ext:posix-getenv "MODUS_I386_SYMMAP")
          (concatenate 'string *i386-image-path* ".symmap")))

(format t "~%Compiling i386 CL image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-i386
                          :source-text cl-user::*full-source*)))
  (format t "  boot code:   ~D bytes~%" (length (kernel-image-boot-code image)))
  (format t "  native code: ~D bytes~%" (length (kernel-image-native-code image)))
  ;; ---- Unimplemented-opcode report (the whole point of layering) ----
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
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Run: qemu-i386-static ~A [1-5]~%" path)))
