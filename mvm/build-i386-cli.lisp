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
             (loop for f in '("mvm/cl-sequences.lisp" "mvm/cl-streams.lisp"
                              "mvm/cl-fileio.lisp"    "mvm/cl-printer.lisp"
                              "mvm/cl-reader.lisp"    "mvm/cl-eval.lisp"
                              "mvm/cl-clos.lisp"      "mvm/cl-types.lisp"
                              "mvm/cl-packages.lisp"  "mvm/cl-conditions.lisp")
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
    *isa-source* (string #\Newline)
    *interp-source* (string #\Newline)
    *compiler-source* (string #\Newline)
    *float-override-source* (string #\Newline)
    *opcode-table-init-source* (string #\Newline)
    *mvm-eval-source* (string #\Newline)
    *bridge-source* (string #\Newline)
    *crypto-source* (string #\Newline)
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

(defvar *sym-name-auto-source*
  (if (>= *i386-layer* 5)
      (emit-sym-name-auto (scan-symbol-names *scanned-source*) 200)
      ""))
(format t "  sym-name-auto: ~D chars~%" (length *sym-name-auto-source*))
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
  (%tag2 116 56) (%xgap (if (bignump (normalize-name (quote abc))) 1 0) 0)
  (%tag2 116 57) (%chk (if (symbolp (normalize-name (quote abc))) 1 0) 0)
  ;; t10-t11 does the *sym-name-table* reverse lookup actually work?  Its keys
  ;; are name hashes, i.e. BIGNUMS here, so every lookup rides EQL-on-bignums.
  (%tag2 49 48) (%xgap (length (symbol-name (quote abc))) 3)
  (%tag2 49 49) (%xgap (if (gethash (normalize-name (quote abc)) *sym-name-table*) 1 0) 1)
  ;; t12 THE ROOT: two hashes of the SAME name are two distinct bignum
  ;; OBJECTS, and EQL on bignums is identity here — so every hash-keyed
  ;; table (sym-name, intern, macro, symbol-function) misses on i386.
  (%tag2 49 50) (%xgap (if (eql (normalize-name (quote abc))
                                (normalize-name (quote abc))) 1 0) 1)
  ;; t13 THE MISMATCH, both halves side by side.  A quoted symbol's STORED
  ;; hash comes from a BUILD-TIME literal, which :li truncates to 32 bits, so
  ;; it is a fixnum.  The same name hashed IN-IMAGE is a 60-bit bignum (t8).
  ;; They are different numbers, so no hash-keyed table can match them.
  (%tag2 49 51) (%xgap (if (eql (%prim-aref (quote abc) 0)
                                (normalize-name (quote abc))) 1 0) 1)
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
  0)
"
      "
(defun %l5-boot () 0)
(defun probe-eval () (write-char-serial 110) (write-char-serial 97) (putnl) 0)
(defun probe-l5 () (write-char-serial 110) (write-char-serial 97) (putnl) 0)
"))

(defvar *full-source*
  (concatenate 'string
    *prelude-source* (string #\Newline)
    *gc-source* (string #\Newline)
    *rt-source* (string #\Newline)
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
    *sft-auto-source* (string #\Newline)
    *sym-name-auto-source* (string #\Newline)
    *l5-init-source* (string #\Newline)
    *bridge-source* (string #\Newline)
    *crypto-source* (string #\Newline)
    *driver-source*))

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
