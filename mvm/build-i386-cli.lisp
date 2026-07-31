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

(defun probe-promote ()
  ;; Isolate the promoting-arithmetic path that compile-mem-refs reconstruction
  ;; depends on: hi*65536 + lo must become a BIGNUM, not a wrapped fixnum.
  (write-char-serial 112) (write-char-serial 49) (write-char-serial 61) ; p1=
  (%pdec (logand (* 17034 65536) 255)) (putnl)              ; expect 0
  (write-char-serial 112) (write-char-serial 50) (write-char-serial 61) ; p2=
  (%pdec (ash (* 17034 65536) -24)) (putnl)                 ; expect 66
  (write-char-serial 112) (write-char-serial 51) (write-char-serial 61) ; p3=
  (%pdec (logand (+ (* 17034 65536) 152) 255)) (putnl)      ; expect 152
  (write-char-serial 112) (write-char-serial 52) (write-char-serial 61) ; p4=
  (%pdec (ash (+ (* 17034 65536) 152) -24)) (putnl))        ; expect 66

(defun probe-memu32-split ()
  ;; RE-MEASURE FROM SCRATCH.  probe 8s earlier attribution -- that the
  ;; :shl 1 tagging overflows -- was made on a substrate that still had 15
  ;; VR-clobbering opcodes, so it
  ;; is not trusted.  Split the two directions apart:
  ;;   s* = value that FITS a 30-bit fixnum -> isolates the LOAD path
  ;;   b* = value that does NOT fit (a bignum literal now) -> adds the STORE path
  (setf (mem-ref #x10000b00 :u32) 4660)          ; #x1234, a fixnum
  (write-char-serial 115) (write-char-serial 49) (write-char-serial 61) ; s1=
  (%pdec (logand (mem-ref #x10000b00 :u32) 255)) (putnl)   ; expect 52
  (write-char-serial 115) (write-char-serial 50) (write-char-serial 61) ; s2=
  (%pdec (ash (mem-ref #x10000b00 :u32) -8)) (putnl)       ; expect 18
  (setf (mem-ref #x10000b00 :u32) 1116352408)    ; #x428a2f98, a BIGNUM here
  (write-char-serial 98) (write-char-serial 49) (write-char-serial 61)  ; b1=
  (%pdec (logand (mem-ref #x10000b00 :u32) 255)) (putnl)   ; expect 152
  (write-char-serial 98) (write-char-serial 50) (write-char-serial 61)  ; b2=
  (%pdec (ash (mem-ref #x10000b00 :u32) -24)) (putnl))     ; expect 66

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

(defun %hexdig (n) (if (< n 10) (+ 48 n) (+ 87 n)))
(defun %phex (b)
  (write-char-serial (%hexdig (ash b -4)))
  (write-char-serial (%hexdig (logand b 15))))
(defun %print-digest (h)
  (let ((i 0))
    (loop (when (>= i 32) (return nil)) (%phex (aref h i)) (setq i (+ i 1))))
  (putnl))

(defun probe-lshift ()
  ;; SHA-256s sigma functions do (ash (logand x 3) 30) — a LEFT shift whose
  ;; RESULT leaves a 30-bit fixnum even though the INPUT is a small fixnum.
  ;; compile-ashs guard only covers a bignum INPUT, so this is the shape it
  ;; would still miss.  (ash 3 30) = 3221225472.
  (write-char-serial 108) (write-char-serial 49) (write-char-serial 61) ; l1=
  (%pdec (logand (ash 3 30) 255)) (putnl)          ; expect 0
  (write-char-serial 108) (write-char-serial 50) (write-char-serial 61) ; l2=
  (%pdec (ash (ash 3 30) -24)) (putnl)             ; expect 192
  (write-char-serial 108) (write-char-serial 51) (write-char-serial 61) ; l3=
  (%pdec (logand (ash 255 24) 255)) (putnl)        ; expect 0
  (write-char-serial 108) (write-char-serial 52) (write-char-serial 61) ; l4=
  (%pdec (ash (logand (ash 255 24) 4294967295) -24)) (putnl))  ; expect 255

(defun f5 (a b c d e) (+ a (+ b (+ c (+ d e)))))
(defun f6 (a b c d e f) (+ a (+ b (+ c (+ d (+ e f))))))
(defun f5id (a b c d e) e)
(defun f6id (a b c d e f) f)

(defun probe-arity ()
  ;; KNOWN-ANSWER tests for the >4-argument calling convention.
  ;; Powers of two so a wrong param is identifiable from the sum.
  (write-char-serial 97) (write-char-serial 53) (write-char-serial 61) ; a5=
  (%pdec (f5 1 2 4 8 16)) (putnl)          ; expect 31
  (write-char-serial 97) (write-char-serial 54) (write-char-serial 61) ; a6=
  (%pdec (f6 1 2 4 8 16 32)) (putnl)       ; expect 63
  (write-char-serial 105) (write-char-serial 53) (write-char-serial 61) ; i5=
  (%pdec (f5id 1 2 4 8 16)) (putnl)        ; expect 16 (the 5th arg alone)
  (write-char-serial 105) (write-char-serial 54) (write-char-serial 61) ; i6=
  (%pdec (f6id 1 2 4 8 16 32)) (putnl))    ; expect 32 (the 6th arg alone)

(defun %phex32 (s off)
  (%phex (aref s off)) (%phex (aref s (+ off 1)))
  (%phex (aref s (+ off 2))) (%phex (aref s (+ off 3))))

(defun probe-rot ()
  ;; Isolate the four ChaCha rotations on 0x01020304 = 16909060.
  ;; rotl16 -> 03040102 (50594050)   rotl12 -> 20304010 (540016656)
  ;; rotl8  -> 02030401 (33818625)   rotl7  -> 81018200 (2164326912)
  ;; Printed as low byte + high byte so %pdec (which handles < 1000) suffices.
  (write-char-serial 114) (write-char-serial 49) (write-char-serial 61) ; r1=
  (%pdec (logand (chacha-rotl16 16909060) 255)) (putnl)        ; expect 2
  (write-char-serial 114) (write-char-serial 50) (write-char-serial 61) ; r2=
  (%pdec (ash (chacha-rotl16 16909060) -24)) (putnl)           ; expect 3
  (write-char-serial 114) (write-char-serial 51) (write-char-serial 61) ; r3=
  (%pdec (logand (chacha-rotl12 16909060) 255)) (putnl)        ; expect 16
  (write-char-serial 114) (write-char-serial 52) (write-char-serial 61) ; r4=
  (%pdec (ash (chacha-rotl12 16909060) -24)) (putnl)           ; expect 32
  (write-char-serial 114) (write-char-serial 53) (write-char-serial 61) ; r5=
  (%pdec (logand (chacha-rotl8 16909060) 255)) (putnl)         ; expect 1
  (write-char-serial 114) (write-char-serial 54) (write-char-serial 61) ; r6=
  (%pdec (ash (chacha-rotl7 16909060) -24)) (putnl))           ; expect 129

(defun probe-chacha2 ()
  ;; RFC 8439 section 2.1.1 quarter-round test vector.  NB chacha-qr takes BYTE
  ;; OFFSETS into a 64-byte buffer, not word indices — an earlier probe passed
  ;; 0 1 2 3 and got a meaningless answer; that was the probes bug, not the code.
  ;;   in : a=11111111 b=01020304 c=9b8d6f43 d=01234567
  ;;   out: a=ea2a92f4 b=cb1cf8ce c=4581472e d=5881c4bb
  (let ((s (make-array 64)))
    (let ((i 0)) (loop (when (>= i 64) (return nil)) (aset s i 0) (setq i (+ i 1))))
    (buf-write-u32 s 0 286331153)
    (buf-write-u32 s 16 16909060)
    (buf-write-u32 s 32 2610427203)
    (buf-write-u32 s 48 19088743)
    (chacha-qr s 0 16 32 48)
    (write-char-serial 113) (write-char-serial 61)
    (%phex32 s 0) (%phex32 s 16) (%phex32 s 32) (%phex32 s 48) (putnl)))

(defun probe-sha-bulk ()
  ;; Throughput: SHA-256 over 64 KiB.  Digest printed so the run is verifiable
  ;; rather than merely timed.
  (sha256-init)
  (let ((n 4096))
    (let ((m (make-array n)))
      (let ((i 0))
        (loop (when (>= i n) (return nil)) (aset m i (logand i 255)) (setq i (+ i 1))))
      (write-char-serial 98) (write-char-serial 61)
      (%print-digest (sha256 m)))))

(defun probe-fixnum-spin ()
  ;; Pure-fixnum workload, NO bignum promotion and NO allocation: the
  ;; emulation-overhead baseline.  Same loop on i386-under-qemu and x64 lets
  ;; the qemu factor be divided out of the crypto comparison.
  (let ((i 0) (acc 0))
    (loop (when (>= i 3000000) (return nil))
          (setq acc (logand (+ acc i) 65535))
          (setq i (+ i 1)))
    (write-char-serial 122) (write-char-serial 61)
    (%pdec (logand acc 255)) (putnl)))

(defun probe-chacha ()
  ;; chacha-qr takes FIVE parameters.  i386s :call pushes only V2/V3 and trap
  ;; 0530 COPY-OVERFLOW-ARGS is still unimplemented, so this should now hit the
  ;; LOUD trap and say so, instead of silently computing on garbage.
  (let ((s (make-array 16)))
    (let ((i 0)) (loop (when (>= i 16) (return nil)) (aset s i i) (setq i (+ i 1))))
    (chacha-qr s 0 1 2 3)
    (write-char-serial 113) (write-char-serial 61)   ; q=
    (%pdec (logand (aref s 0) 255)) (putnl)))

(defun probe-sha256 ()
  (sha256-init)
  ;; SHA256 of abc -> ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
  (let ((m (make-array 3)))
    (aset m 0 97) (aset m 1 98) (aset m 2 99)
    (write-char-serial 97) (write-char-serial 98) (write-char-serial 99)
    (write-char-serial 61)
    (%print-digest (sha256 m)))
  ;; SHA256 of the empty string -> e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
  (write-char-serial 101) (write-char-serial 61)
  (%print-digest (sha256 (make-array 0))))

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
      ((eql which 9) (probe-memu32-split))
      ((eql which 10) (probe-promote))
      ((eql which 11) (probe-sha256))
      ((eql which 12) (probe-lshift))
      ((eql which 13) (probe-chacha))
      ((eql which 14) (probe-arity))
      ((eql which 15) (probe-chacha2))
      ((eql which 16) (probe-sha-bulk))
      ((eql which 17) (probe-fixnum-spin))
      ((eql which 18) (probe-rot))
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
    *crypto-source* (string #\Newline)
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
