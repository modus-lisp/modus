;;;; term.lisp -- THE WIRE FORMAT: how a term (nil, fixnum, cons, string, symbol, vector, bignum, double)
;;;; becomes bytes and back.  Actor messages use it (net/actors.lisp), and so does the iOS bridge (the Swift
;;;; side reads and writes the same bytes: host/ios/swift/Term.swift).
;;;;
;;;; The caller provides DECODE-PTR-ADDR: the address of the 8-byte cell that holds the decoder's read
;;;; position (the staging buffer's cursor in the actor path; a cell of the caller's own elsewhere).  A
;;;; bignum (tag 5) also needs MAKE-BIGNUM-N, which net/actors.lisp defines; nothing else is needed from it.
;;;;
;;;; Floats go through the image's halves (modus-int::%float-lo32 / %float-hi32) to the wire, and back through
;;;; TERM-DOUBLE-FROM-BITS below, which builds the double by arithmetic and so needs no helper from the
;;;; prelude.  Strings are UTF-8.  NIL is tag 0, separate from a fixnum zero.

;; Read subtag byte from object header.
;; Compatible with try-alloc-obj pointer format (tag in low 2 bits).
;; Uses the same raw-address derivation as aref/array-length.
(defun soft-subtag (obj)
  (let ((raw (ash (logand obj (- 0 4)) 1)))
    (mem-ref raw :u8)))

;; THE WIRE FORMAT, by tag.  A term is a tree: nil, fixnum, cons, string, symbol, vector, bignum.
;;   0 nil            1 fixnum (u64)            2 cons (car, then cdr)
;;   3 string, 8-bit  (decoded for a sender that predates 7; not written any more)
;;   4 vector (bytes) 5 bignum (limbs)          6 symbol (name, then package)
;;   7 string, UTF-8  (u32 byte count, the bytes)
;;   8 double-float   (IEEE bits: the low u32, then the high u32)
;; A string travels as UTF-8, so a character past Latin-1 arrives as itself.  A double travels as its
;; bits, so zero, the sign of zero and the NaNs all arrive as they left.

(defun term-float-p (val)
  "A boxed double-float.  Tested before anything else: zerop would send 0.0 as nil, and a float
   is not a fixnum.  The subtag is read with OBJ-SUBTAG, the primitive: SOFT-SUBTAG reads 0 for a
   float (measured: literal, quoted and computed doubles alike), so it would send a float as a pointer."
  (and (numberp val) (not (integerp val)) (= (obj-subtag val) #x60)))

(defun %utf8-length (s)
  "The bytes S takes as UTF-8."
  (let ((n 0) (i 0))
    (loop
      (when (>= i (length s)) (return n))
      (let ((c (char-code (aref s i))))
        (setq n (+ n (cond ((< c #x80) 1) ((< c #x800) 2) ((< c #x10000) 3) (t 4)))))
      (setq i (+ i 1)))))

(defun %utf8-put (s buf)
  "Write S as UTF-8 at the raw address BUF.  The number of bytes written."
  (let ((p buf) (i 0))
    (loop
      (when (>= i (length s)) (return (- p buf)))
      (let ((c (char-code (aref s i))))
        (cond ((< c #x80)
               (setf (mem-ref p :u8) c)
               (setq p (+ p 1)))
              ((< c #x800)
               (setf (mem-ref p :u8) (logior #xC0 (ash c -6)))
               (setf (mem-ref (+ p 1) :u8) (logior #x80 (logand c #x3F)))
               (setq p (+ p 2)))
              ((< c #x10000)
               (setf (mem-ref p :u8) (logior #xE0 (ash c -12)))
               (setf (mem-ref (+ p 1) :u8) (logior #x80 (logand (ash c -6) #x3F)))
               (setf (mem-ref (+ p 2) :u8) (logior #x80 (logand c #x3F)))
               (setq p (+ p 3)))
              (t
               (setf (mem-ref p :u8) (logior #xF0 (ash c -18)))
               (setf (mem-ref (+ p 1) :u8) (logior #x80 (logand (ash c -12) #x3F)))
               (setf (mem-ref (+ p 2) :u8) (logior #x80 (logand (ash c -6) #x3F)))
               (setf (mem-ref (+ p 3) :u8) (logior #x80 (logand c #x3F)))
               (setq p (+ p 4)))))
      (setq i (+ i 1)))))

(defun %utf8-get (base nbytes)
  "The string made of the NBYTES of UTF-8 at the raw address BASE."
  (let ((count 0) (i 0))
    ;; a character starts at every byte that is not a continuation byte (10xxxxxx)
    (loop
      (when (>= i nbytes) (return 0))
      (unless (= (logand (mem-ref (+ base i) :u8) #xC0) #x80) (setq count (+ count 1)))
      (setq i (+ i 1)))
    (let ((s (make-string count)) (k 0) (i 0))
      (loop
        (when (>= i nbytes) (return s))
        (let* ((b0 (mem-ref (+ base i) :u8))
               (len (cond ((< b0 #x80) 1) ((< b0 #xE0) 2) ((< b0 #xF0) 3) (t 4)))
               (code (cond ((= len 1) b0)
                           ((= len 2)
                            (logior (ash (logand b0 #x1F) 6)
                                    (logand (mem-ref (+ base i 1) :u8) #x3F)))
                           ((= len 3)
                            (logior (ash (logand b0 #x0F) 12)
                                    (ash (logand (mem-ref (+ base i 1) :u8) #x3F) 6)
                                    (logand (mem-ref (+ base i 2) :u8) #x3F)))
                           (t
                            (logior (ash (logand b0 #x07) 18)
                                    (ash (logand (mem-ref (+ base i 1) :u8) #x3F) 12)
                                    (ash (logand (mem-ref (+ base i 2) :u8) #x3F) 6)
                                    (logand (mem-ref (+ base i 3) :u8) #x3F))))))
          (setf (aref s k) (code-char code))
          (setq k (+ k 1))
          (setq i (+ i len)))))))

;; Compute serialized byte count for a term.  The float and the string are measured first, as they
;; are written first (term-float-p before the zero test, for the reason on that function).
;;
;; A LIST IS WALKED ALONG ITS SPINE, NOT RECURSED DOWN IT: only a car is a recursion.  Recursing on the
;; cdr took two frames an element, so a list of 5000 rows (a big folder, sent to iOS's UI in one call)
;; ran off the end of an iPhone thread's stack -- a memory fault, and with no handler armed, the app gone.
(defun term-size (val)
  (let ((n 0))
    (loop
      (if (consp val)
          (progn (setq n (+ n (+ 1 (term-size (car val)))))
                 (setq val (cdr val)))
          (return (+ n (term-size-1 val)))))))

(defun term-size-1 (val)
  (cond ((term-float-p val) 9)
        ((null val) 1)
        ((stringp val) (+ 5 (%utf8-length val)))
        ((symbolp val)
         (let ((nm (symbol-name val)) (pk (symbol-package val)))
           (+ 9 (length nm) (if pk (length (package-name pk)) 0))))
        (t (term-size-value val))))

(defun term-size-value (val)
  (if (null val) 1
      (if (consp val)
          (let ((s1 (term-size (car val))))
            (let ((s2 (term-size (cdr val))))
              (+ 1 (+ s1 s2))))
          (if (numberp val) 9
              (let ((st (soft-subtag val)))
                (if (= st #x32) (+ 5 (array-length val))
                    (if (= st #x30)
                        (let ((addr (ash (logand val (- 0 4)) 1)))
                          (+ 5 (ash (ash (mem-ref addr :u64) -15) 3)))
                        9)))))))

;; Serialize term to buffer at raw address buf. Returns bytes written.
;; Uses soft-subtag for object type dispatch (compatible with try-alloc-obj).
;; A list along its spine, as TERM-SIZE: tag 2, its car, and on to its cdr.
(defun term-encode (val buf)
  (let ((start buf))
    (loop
      (if (consp val)
          (progn (setf (mem-ref buf :u8) 2)
                 (setq buf (+ (+ buf 1) (term-encode (car val) (+ buf 1))))
                 (setq val (cdr val)))
          (return (+ (- buf start) (term-encode-1 val buf)))))))

(defun term-encode-1 (val buf)
  (if (term-float-p val)
      (progn
        (setf (mem-ref buf :u8) 8)
        (setf (mem-ref (+ buf 1) :u32) (modus-int::%float-lo32 val))
        (setf (mem-ref (+ buf 5) :u32) (modus-int::%float-hi32 val))
        9)
      (term-encode-value val buf)))

(defun term-encode-value (val buf)
  (if (null val)
      (progn (setf (mem-ref buf :u8) 0) 1)
      (if (consp val)
          (progn
            (setf (mem-ref buf :u8) 2)
            (let ((n1 (term-encode (car val) (+ buf 1))))
              (let ((buf2 (+ (+ buf 1) n1)))
                (let ((n2 (term-encode (cdr val) buf2)))
                  (+ (+ 1 n1) n2)))))
          (if (numberp val)
              ;; Fixnum (non-zero)
              (progn
                ;; The VALUE, as a little-endian u64, in two u32 halves: a :u64 store writes the
                ;; machine word, which is the fixnum shifted left by one, and no other image could
                ;; read that as the integer.  Two's complement, so a negative fixnum is its u64 too.
                (setf (mem-ref buf :u8) 1)
                (setf (mem-ref (+ buf 1) :u32) (logand val 4294967295))
                (setf (mem-ref (+ buf 5) :u32) (logand (ash val -32) 4294967295))
                9)
          ;; ---- STRINGS AND SYMBOLS GO BY VALUE, NOT BY POINTER ----------
          ;;
          ;; Both used to fall off the end of this dispatch into the "unknown
          ;; object type — encode as fixnum" arm, which writes THE OBJECT'S OWN
          ;; TAGGED POINTER and labels it a fixnum.  Measured, not inferred: a
          ;; symbol encoded to tag 1 with a payload equal, bit for bit, to its
          ;; own machine word.  So every actor message carrying a string or a
          ;; symbol handed the receiver a pointer into the SENDER'S REGION —
          ;; the exact violation of "no region holds a pointer into another"
          ;; that the whole per-region collector rests on, sitting in the
          ;; message path.  Serialising is the ONLY thing that makes separate
          ;; regions per actor sound, and for the two commonest payload types
          ;; it was not happening.
          ;;
          ;; TAG 3 IS NOT NEW.  TERM-DECODE-STEP has always had the arm that
          ;; reads a length, reads the bytes and MAKE-STRINGs a copy IN THE
          ;; RECEIVER'S REGION; it was dead code because nothing ever emitted
          ;; it.  The asymmetry was the bug.  This emits it.
          (if (stringp val)
              ;; UTF-8 (tag 7): the bytes go down first, then the tag and their count in front
              (let ((nbytes (%utf8-put val (+ buf 5))))
                (setf (mem-ref buf :u8) 7)
                (setf (mem-ref (+ buf 1) :u32) nbytes)
                (+ 5 nbytes))
          ;; TAG 6 — A SYMBOL, BY NAME AND PACKAGE NAME.
          ;;
          ;; The receiver INTERNS it, which is the whole point: the intern
          ;; happens on the RECEIVING thread, so the symbol and the table that
          ;; points at it are allocated in the SAME region by construction.  No
          ;; barrier, no promotion, no delegation — the pointer that would have
          ;; had to be fixed up is never created.
          ;;
          ;; A symbol with no home package (an uninterned gensym) encodes a
          ;; ZERO-length package and decodes through MAKE-SYMBOL: two actors
          ;; cannot share an uninterned symbol by name and must not pretend to.
              (if (symbolp val)
                  (let* ((nm (symbol-name val))
                         (pk (symbol-package val))
                         (pn (if pk (package-name pk) ""))
                         (nlen (length nm))
                         (plen (length pn)))
                    (setf (mem-ref buf :u8) 6)
                    (setf (mem-ref (+ buf 1) :u32) nlen)
                    (let ((i 0))
                      (loop
                        (when (>= i nlen) (return 0))
                        (setf (mem-ref (+ (+ buf 5) i) :u8) (char-code (aref nm i)))
                        (setq i (+ i 1))))
                    (setf (mem-ref (+ (+ buf 5) nlen) :u32) plen)
                    (let ((i 0) (pbase (+ (+ buf 9) nlen)))
                      (loop
                        (when (>= i plen) (return 0))
                        (setf (mem-ref (+ pbase i) :u8) (char-code (aref pn i)))
                        (setq i (+ i 1))))
                    (+ (+ 9 nlen) plen))
              ;; Object: dispatch on subtag
              (let ((st (soft-subtag val)))
                (if (= st #x32)
                    ;; Array
                    (let ((alen (array-length val)))
                      (setf (mem-ref buf :u8) 4)
                      (setf (mem-ref (+ buf 1) :u32) alen)
                      (let ((i 0))
                        (loop
                          (when (>= i alen) (return 0))
                          (setf (mem-ref (+ (+ buf 5) i) :u8) (aref val i))
                          (setq i (+ i 1))))
                      (+ 5 alen))
                    (if (= st #x30)
                        ;; Bignum
                        (let ((addr (ash (logand val (- 0 4)) 1)))
                          (let ((nlimbs (ash (mem-ref addr :u64) -15)))
                            (setf (mem-ref buf :u8) 5)
                            (setf (mem-ref (+ buf 1) :u32) nlimbs)
                            (let ((i 0))
                              (loop
                                (when (>= i nlimbs) (return 0))
                                (setf (mem-ref (+ (+ buf 5) (ash i 3)) :u64)
                                      (mem-ref (+ (+ addr 8) (ash i 3)) :u64))
                                (setq i (+ i 1))))
                            (+ 5 (ash nlimbs 3))))
                        ;; STILL UNKNOWN — and still a pointer, deliberately
                        ;; left as it was.  Everything that has been MEASURED
                        ;; to come through here (symbols, strings) is now
                        ;; handled above; narrowing this arm further without a
                        ;; demonstration of what lands in it would be guessing.
                        ;; It remains a soundness hole for any type nobody has
                        ;; looked at yet.
                        (progn
                          (setf (mem-ref buf :u8) 1)
                          (setf (mem-ref (+ buf 1) :u64) val)
                          9))))))))))

;; Deserialize one term from staging buffer.
;; Uses global read pointer at decode-ptr-addr to track position.
(defun term-decode-step ()
  (let ((da (decode-ptr-addr)))
    (let ((buf (mem-ref da :u64)))
      (let ((tag (mem-ref buf :u8)))
        (if (zerop tag)
            (progn (setf (mem-ref da :u64) (+ buf 1)) nil)
            (if (= tag 1)
                ;; the integer from its two u32 halves (see term-encode): a high half with its top
                ;; bit set is negative, in two's complement
                (let* ((lo (mem-ref (+ buf 1) :u32))
                       (hi (mem-ref (+ buf 5) :u32))
                       (v (+ lo (ash hi 32))))
                  (setf (mem-ref da :u64) (+ buf 9))
                  (if (>= hi 2147483648) (- v 18446744073709551616) v))
                (if (= tag 2)
                    ;; along the spine, as TERM-ENCODE wrote it: the cars, then the tail, consed back up
                    (let ((items nil) (tail nil))
                      (loop
                        (let ((at (mem-ref da :u64)))
                          (if (= (mem-ref at :u8) 2)
                              (progn (setf (mem-ref da :u64) (+ at 1))
                                     (setq items (cons (term-decode-step) items)))
                              (progn (setq tail (term-decode-step))
                                     (return 0)))))
                      (loop
                        (when (null items) (return tail))
                        (setq tail (cons (car items) tail))
                        (setq items (cdr items))))
                    (if (= tag 6)
                        ;; A SYMBOL, BY NAME — AND THE INTERN HAPPENS HERE.
                        ;;
                        ;; That is the entire point of the tag.  Interning on
                        ;; the RECEIVING thread allocates the symbol in the
                        ;; RECEIVER'S region, which is the same region as the
                        ;; table that will point at it — so the cross-region
                        ;; pointer that used to be shipped in the message is
                        ;; never created rather than being created and then
                        ;; repaired.
                        (let ((nlen (mem-ref (+ buf 1) :u32)))
                          (let ((nm (make-string nlen)))
                            (let ((i 0))
                              (loop
                                (when (>= i nlen) (return 0))
                                (setf (aref nm i) (code-char (mem-ref (+ (+ buf 5) i) :u8)))
                                (setq i (+ i 1))))
                            (let ((plen (mem-ref (+ (+ buf 5) nlen) :u32)))
                              (let ((pn (make-string plen))
                                    (pbase (+ (+ buf 9) nlen)))
                                (let ((i 0))
                                  (loop
                                    (when (>= i plen) (return 0))
                                    (setf (aref pn i) (code-char (mem-ref (+ pbase i) :u8)))
                                    (setq i (+ i 1))))
                                (setf (mem-ref da :u64) (+ pbase plen))
                                ;; No home package — an uninterned symbol.  Two
                                ;; actors cannot share one by name, so give the
                                ;; receiver its own rather than pretend.
                                (if (= plen 0)
                                    (make-symbol nm)
                                    (intern nm pn))))))
                    (if (= tag 3)
                        ;; STRING-SET USED TO FILL THIS, AND STRING-SET DOES NOT
                        ;; EXIST.  Not "was removed" — `defun string-set' appears
                        ;; nowhere in the tree, (FBOUNDP 'STRING-SET) is NIL, and
                        ;; this was its ONLY call site.  So this arm was dead in
                        ;; two senses at once: nothing ever emitted tag 3, and it
                        ;; could not have worked if anything had.  MAKE-STRING
                        ;; fills with spaces, so decoding through it gave a
                        ;; string of the right LENGTH and no content — which is
                        ;; exactly what came back the first time the encoder
                        ;; emitted tag 3.
                        (let ((slen (mem-ref (+ buf 1) :u32)))
                          (let ((s (make-string slen)))
                            (let ((i 0))
                              (loop
                                (when (>= i slen) (return 0))
                                (setf (aref s i) (code-char (mem-ref (+ (+ buf 5) i) :u8)))
                                (setq i (+ i 1))))
                            (setf (mem-ref da :u64) (+ (+ buf 5) slen))
                            s))
                        (if (= tag 4)
                            (let ((alen (mem-ref (+ buf 1) :u32)))
                              (let ((a (make-array alen)))
                                (let ((i 0))
                                  (loop
                                    (when (>= i alen) (return 0))
                                    (aset a i (mem-ref (+ (+ buf 5) i) :u8))
                                    (setq i (+ i 1))))
                                (setf (mem-ref da :u64) (+ (+ buf 5) alen))
                                a))
                            (if (= tag 7)
                                ;; UTF-8 string (tag 7)
                                (let ((nbytes (mem-ref (+ buf 1) :u32)))
                                  (let ((s (%utf8-get (+ buf 5) nbytes)))
                                    (setf (mem-ref da :u64) (+ (+ buf 5) nbytes))
                                    s))
                            (if (= tag 8)
                                ;; double-float (tag 8): the low half, then the high half, as IEEE bits
                                (let ((lo (mem-ref (+ buf 1) :u32)) (hi (mem-ref (+ buf 5) :u32)))
                                  (setf (mem-ref da :u64) (+ buf 9))
                                  (term-double-from-bits hi lo))
                            ;; Bignum (tag 5)
                            (let ((nlimbs (mem-ref (+ buf 1) :u32)))
                              (let ((b (make-bignum-n nlimbs)))
                                (let ((baddr (ash (logand b (- 0 4)) 1)))
                                  (let ((i 0))
                                    (loop
                                      (when (>= i nlimbs) (return 0))
                                      (setf (mem-ref (+ (+ baddr 8) (ash i 3)) :u64)
                                            (mem-ref (+ (+ buf 5) (ash i 3)) :u64))
                                      (setq i (+ i 1))))
                                (setf (mem-ref da :u64)
                                      (+ (+ buf 5) (ash nlimbs 3)))
                                b)))))))))))))))


(defun term-double-from-bits (hi lo)
  "The double whose IEEE bits are HI (the high 32) and LO (the low 32).  Built by arithmetic, so no
   constructor or setter from the prelude is needed: exact for every finite double and zero, and
   infinity and NaN come from overflow and an invalid operation, as the hardware makes them."
  (let* ((sign (ash hi -31))
         (expo (logand (ash hi -20) #x7FF))
         (mant (+ (* (logand hi #xFFFFF) 4294967296) lo))
         (value (cond ((= expo 2047)
                       (let ((inf (* 1d300 1d300)))
                         (if (= mant 0) inf (- inf inf))))
                      ((and (= expo 0) (= mant 0)) 0d0)
                      ((= expo 0) (* (float mant 1d0) (expt 2d0 -1074)))
                      (t (* (+ 1d0 (/ (float mant 1d0) 4503599627370496d0))
                            (expt 2d0 (- expo 1023)))))))
    (if (= sign 1) (- 0d0 value) value)))
