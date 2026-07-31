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
;;;;   4  + MVM ISA + interp + compiler + mvm-eval   (runtime compile)
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

;;; ============================================================
;;; Driver — argv-dispatched probes, mirroring build-aarch64-cli
;;; ============================================================

(defvar *driver-source* "

(defun sys-exit (code)
  (let ((c code))
    (syscall3 1 c 0 0)))

;; argv[1] was staged as a NUL-terminated ASCII string at #x10000208 by the
;; entry stub (see boot/boot-linux-i386.lisp); parse it as a decimal integer.
(defun %parse-decimal-at-fixed-208 ()
  (let ((n 0) (i 0))
    (loop
      (let ((b (mem-ref (+ #x10000208 i) :u8)))
        (when (or (< b 48) (> b 57)) (return n))
        (setq n (+ (* n 10) (- b 48)))
        (setq i (+ i 1))))))

;; Decimal printer built ONLY from +, -, < and > — deliberately NOT from
;; TRUNCATE/MOD.  Those are real CL-runtime defuns (cl-types.lisp /
;; cl-eval.lisp) and TRUNCATE takes &rest, so using them here would couple
;; every numeric probe to the &rest/nargs machinery.  Repeated subtraction
;; keeps the probes measuring what they claim to measure.  Values < 1000.
(defun %pdec (n)
  (if (< n 0)
      (progn (write-char-serial 45) (%pdec (- 0 n)))
      (let ((h 0) (t10 0) (r n))
        (loop (when (< r 100) (return nil)) (setq r (- r 100)) (setq h (+ h 1)))
        (loop (when (< r 10) (return nil)) (setq r (- r 10)) (setq t10 (+ t10 1)))
        (when (> h 0) (write-char-serial (+ 48 h)))
        (when (or (> h 0) (> t10 0)) (write-char-serial (+ 48 t10)))
        (write-char-serial (+ 48 r)))))

(defun putnl () (write-char-serial 10))

(defun emit-str-ok () (write-char-serial 79) (write-char-serial 75))

;; Layer-1 probes: no CL runtime at all, just the compiler's own codegen on a
;; 32-bit word.  Each of these is a DIFFERENT translator path, so a failure
;; localises immediately.
(defun probe-arith ()
  (write-char-serial 97) (write-char-serial 61)   ; a=
  (%pdec (+ 1 2)) (putnl)
  (write-char-serial 98) (write-char-serial 61)   ; b=
  (%pdec (let ((x 5)) (* x x))) (putnl)
  (write-char-serial 99) (write-char-serial 61)   ; c=
  (%pdec (- 100 58)) (putnl)
  (write-char-serial 100) (write-char-serial 61)  ; d=  (plain sub)
  (%pdec (- 144 132)) (putnl))

(defun sq (x) (* x x))

(defun probe-defcall ()
  (write-char-serial 101) (write-char-serial 61)  ; e=
  (%pdec (sq 7)) (putnl))

(defun probe-cons ()
  (let ((p (cons 3 4)))
    (write-char-serial 102) (write-char-serial 61)  ; f=
    (%pdec (+ (car p) (cdr p))) (putnl)))

(defun probe-funcall ()
  ;; exercises :fn-addr (the position-independent call/pop/add form) plus the
  ;; +3 function tag and :call-ind's strip
  (write-char-serial 103) (write-char-serial 61)  ; g=
  (%pdec (funcall (function sq) 9)) (putnl))

;; ---- 30-bit-fixnum / bignum-promotion battery ----------------------
;; SHA-256 is u32 arithmetic.  A u32 does not fit an i386 fixnum (~30 bits),
;; so every one of these is a value that MUST transparently promote.  Each
;; probe prints a small expected number so %pdec (built only from +,-,<,>)
;; can report it.
(defun probe-bignum ()
  (write-char-serial 107) (write-char-serial 49) (write-char-serial 61) ; k1=
  (%pdec (logand #x428a2f98 255)) (putnl)          ; expect 152
  (write-char-serial 107) (write-char-serial 50) (write-char-serial 61) ; k2=
  (%pdec (ash #x428a2f98 -24)) (putnl)             ; expect 66
  (write-char-serial 107) (write-char-serial 51) (write-char-serial 61) ; k3=
  (%pdec (logand (+ #x40000000 1) 255)) (putnl)    ; expect 1
  (write-char-serial 107) (write-char-serial 52) (write-char-serial 61) ; k4=
  (%pdec (logand #xFFFFFFFF 255)) (putnl)          ; expect 255
  (write-char-serial 107) (write-char-serial 53) (write-char-serial 61) ; k5=
  (%pdec (logand (logxor #xFFFFFFFF #x428a2f98) 255)) (putnl)  ; expect 103
  (write-char-serial 107) (write-char-serial 54) (write-char-serial 61) ; k6=
  (%pdec (logand (ash 1 29) 255)) (putnl))         ; expect 0

(defun probe-memu32 ()
  ;; mem-ref :u32 must survive a u32 that cannot be a fixnum.  Uses the
  ;; scratch BSS above the i386 global slot block.
  (setf (mem-ref #x10000b00 :u32) #x428a2f98)
  (write-char-serial 109) (write-char-serial 61)   ; m=
  (%pdec (logand (mem-ref #x10000b00 :u32) 255)) (putnl)   ; expect 152
  (write-char-serial 110) (write-char-serial 61)   ; n=
  (%pdec (ash (mem-ref #x10000b00 :u32) -24)) (putnl))     ; expect 66

(defun probe-fixnum-width ()
  ;; How wide IS a fixnum here?  Count doublings until the value stops
  ;; growing monotonically (wrap) — on a 32-bit word with a 1-bit tag this
  ;; should report 30.
  (write-char-serial 119) (write-char-serial 61)  ; w=
  ;; Double until the value stops growing OR we pass 40 doublings.  The
  ;; bound keeps this probe from running into the (currently broken) bignum
  ;; path, so it reports the fixnum width rather than SIGSEGVing.
  (let ((n 1) (k 0))
    (loop
      (when (> k 40) (return nil))
      (let ((n2 (+ n n)))
        (when (< n2 n) (return nil))
        (setq n n2)
        (setq k (+ k 1))))
    (%pdec k))
  (putnl))

(defun probe-argv ()
  ;; Echo the raw bytes the entry stub staged at #x10000208, then the parsed
  ;; integer.  Proves argv staging + mem-ref :u8 independently of everything.
  (write-char-serial 118) (write-char-serial 61)   ; v=
  (let ((i 0))
    (loop
      (let ((b (mem-ref (+ #x10000208 i) :u8)))
        (when (eql b 0) (return nil))
        (write-char-serial b)
        (setq i (+ i 1)))))
  (putnl)
  (write-char-serial 110) (write-char-serial 61)   ; n=
  (%pdec (%parse-decimal-at-fixed-208)) (putnl))

(defun kernel-main ()
  (let ((which (%parse-decimal-at-fixed-208)))
    (cond
      ((eql which 1) (probe-arith))
      ((eql which 2) (probe-defcall))
      ((eql which 3) (probe-cons))
      ((eql which 4) (probe-funcall))
      ((eql which 5) (probe-fixnum-width))
      ((eql which 6) (probe-argv))
      ((eql which 7) (probe-bignum))
      ((eql which 8) (probe-memu32))
      (t (progn (probe-argv) (probe-arith) (probe-defcall) (probe-cons)
                (probe-funcall) (probe-fixnum-width)))))
  (write-char-serial 68) (write-char-serial 79) (write-char-serial 78)
  (write-char-serial 69) (putnl)   ; DONE
  (sys-exit 0))
")

;;; ============================================================
;;; Assemble
;;; ============================================================

(defvar *full-source*
  (concatenate 'string
    *prelude-source* (string #\Newline)
    *gc-source* (string #\Newline)
    *rt-source* (string #\Newline)
    *bridge-source* (string #\Newline)
    *driver-source*))

(format t "Full source: ~D characters~%" (length *full-source*))

(in-package :modus.mvm)

(modus.mvm.i386:install-i386-translator)

(setf modus.mvm.i386::*i386-linux-mode* t)
(modus.mvm.i386::i386-set-globals-base +linux-i386-globals+)
(setf modus.mvm.i386::*i386-fn-tag-3* t)
(setf modus.mvm.i386::*i386-fn-align* 16)
(setf modus.mvm.i386::*i386-native-code-offset* 0)
(setf modus.mvm.i386::*i386-checked-arith-slowpath*
      (let ((v (sb-ext:posix-getenv "MODUS_I386_NO_CHECKED")))
        (not (and v (plusp (length v))))))
(setf modus.mvm.i386::*i386-record-unimpl* t)
(setf modus.mvm.i386::*i386-unimpl-ops* nil)

(setf *write-symmap-path* "/home/claude/ws5-gate-out/modus-i386-cli.symmap")

(format t "~%Compiling i386 CL image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :linux-i386
                          :source-text cl-user::*full-source*)))
  (format t "  boot code:   ~D bytes~%" (length (kernel-image-boot-code image)))
  (format t "  native code: ~D bytes~%" (length (kernel-image-native-code image)))
  ;; ---- Unimplemented-opcode report (the whole point of layering) ----
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
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_I386_OUT")
                  "/home/claude/ws5-gate-out/modus-i386-cli")))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "Run: qemu-i386-static ~A [1-5]~%" path)))
