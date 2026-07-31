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

(defun kernel-main ()
  (let ((which (%argv1)))
    (cond
      ((eql which 1) (probe-gcmeta))
      ((eql which 2) (probe-bulk))
      ((eql which 3) (probe-chain))
      ((eql which 4) (probe-argv))
      (t (sys-exit (if (eql (probe-suite) 0) 0 1)))))
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
;;   MODUS_I386_LAYER=1..4    how much of the stack to bake in.  1 prelude,
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
