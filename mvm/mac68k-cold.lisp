;;;; mac68k-cold.lisp -- COLD NAMES: symbol names a bare Mac reads from disk.
;;;;
;;;; *SYM-NAME-TABLE* maps the hash of every symbol-shaped name in the source
;;;; (12,000 of them) to its string, so SYMBOL-NAME can name a native symbol,
;;;; which holds only its hash.  On a Mac Classic's saved core it was 430 KB of
;;;; a 1.3 MB static heap, and a REPL session reads almost none of it.  So the
;;;; build also writes the names to a file of 512-byte pages sorted by hash
;;;; (build-cli-common.lisp, COLD NAMES), scripts/mac68k-core.lisp puts the file
;;;; on the disk after the code streams and the disk byte it starts at in the
;;;; static block's word #x10006044, and the freeze drops the table: from then
;;;; on a name the table would have given comes from %COLD-SYM-NAME -- the
;;;; page's first hashes are in the image (%COLD-NAMES-INDEX), so a miss is
;;;; one block read (SYSCALL3 service 1, translate-68k M68K-EMIT-MAC-SERVICE),
;;;; and a name read once is kept (*COLD-NAMES-MEMO*).
;;;;
;;;; Image source for the bare Mac only (build-cl-repl-common.lisp); the
;;;; defaults it overrides are in cl-packages.lisp.

(defvar *cold-names-memo* nil)

(defun %cold-names-first-le (s page h)
  "Is PAGE's first hash -- its 8 hex digits of S, %COLD-NAMES-INDEX -- <= H?
   Compared a digit at a time, the low digit first so a higher one overrides.
   The index was once parsed into a vector: 403 PARSE-INTEGERs through the
   bignum reader, measured as 15 s of a Mac Plus's first (compile 'f), and a
   vector the young heap then carried."
  (let ((i (+ (* page 8) 7)) (x h) (r 0) (k 0))
    (loop
      (when (>= k 8) (return (<= r 0)))
      (let* ((c (char-code (char s i))) (d (if (< c 58) (- c 48) (- c 55))) (e (logand x 15)))
        (cond ((< d e) (setq r -1)) ((> d e) (setq r 1))))
      (setq i (- i 1) x (ash x -4) k (+ k 1)))))

(defun %cold-names-ready-p ()
  "Can names come from disk?  The core builder said where they are, and this
   image hashes a name as the build that wrote them did."
  (and (> (mem-ref #x10006044 :u32) 0)
       (eql (compute-name-hash "CAR") (%cold-names-check))))

(defun %cold-disk-read (at buf)
  "512 bytes of a cold table into BUF; 0 when read.  AT is a disk byte (the
   Mac disk service), or -- when #x10006078 is non-zero, a core booted from a
   compressed floppy that carries the tables in RAM (mac68k-core.lisp
   MAC68K_CORE_RAW) -- their address."
  (if (eql (mem-ref #x10006078 :u32) 0)
      (syscall3 1 at buf 512)
      (let ((i 0))
        (loop (when (>= i 512) (return 0))
          (setf (mem-ref (+ buf i) :u16) (mem-ref (+ at i) :u16))
          (setq i (+ i 2))))))

(defun %cold-sym-name (h)
  "The name whose hash is H, from the cold names on disk; NIL when there is none
   or no disk to read it from."
  (when (and (fixnump h) (> h 0) (%cold-names-ready-p))
    (or (and *cold-names-memo* (gethash h *cold-names-memo*))
        (let* ((v (%cold-names-index)) (n (floor (length v) 8)))
          (when (and (> n 0) (%cold-names-first-le v 0 h))
            ;; the last page whose first hash is <= H
            (let ((lo 0) (hi (- n 1)))
              (loop (when (>= lo hi) (return nil))
                (let ((mid (ash (+ lo hi 1) -1)))
                  (if (%cold-names-first-le v mid h) (setq lo mid) (setq hi (- mid 1)))))
              (let ((buf *io-buf-addr*))
                (when (eql (%cold-disk-read (+ (mem-ref #x10006044 :u32) (* lo 512)) buf) 0)
                  (let ((i 0) (found nil))
                    (loop
                      (when (> i 507) (return nil))
                      (let ((k (+ (* (mem-ref (+ buf i) :u8) 16777216)
                                  (* (mem-ref (+ buf i 1) :u8) 65536)
                                  (* (mem-ref (+ buf i 2) :u8) 256)
                                  (mem-ref (+ buf i 3) :u8)))
                            (len (mem-ref (+ buf i 4) :u8)))
                        (when (eql k 0) (return nil))
                        (when (eql k h)
                          (let ((s (%make-base-string len)) (j 0))
                            (loop (when (>= j len) (return nil))
                              (%u8-set s j (mem-ref (+ buf i 5 j) :u8))
                              (setq j (+ j 1)))
                            (setq found s))
                          (return nil))
                        (setq i (+ i 5 len))))
                    (when found
                      (unless *cold-names-memo*
                        (setq *cold-names-memo* (make-hash-table)))
                      (puthash h *cold-names-memo* found))
                    found)))))))))

(defun %static-symbol-tableless-p ()
  "After the freeze the table is gone ON PURPOSE: every name resolves from the
   cold pages, and INTERN replaces an occupant only when SYMBOL-NAME says it has
   none -- so a static-symbol slot may cache (compiler.lisp %STATIC-SYMBOL-FILL;
   the default, in cl-packages.lisp, is the reason it may not otherwise)."
  (%cold-names-ready-p))

(defun %cold-names-drop ()
  "At the freeze of the CORE RUN (the SAVE flag, #x1000602C): drop
   *SYM-NAME-TABLE*, whose names the saved core's disk carries cold
   (mac68k-core.lisp writes them and #x10006044).  Its strings that something
   else holds (a symbol's cached name, the function table's keys) stay.  A boot
   from scratch keeps the table: its disk has no cold names."
  (when (and (eql (mem-ref #x1000602C :u32) 1)
             (eql (compute-name-hash "CAR") (%cold-names-check)))
    (setq *sym-name-table* nil)))

;;; RIGHT-SIZED BUCKETS.  A pinned table (the symbol-function table, the globals
;;; table: their (key . value) pairs are cached outside them) keeps a bucket
;;; vector sized at TWO buckets an entry -- 16,384 words, 64 KB, for the
;;; function table's 4,711 names.  At the freeze the Mac rebuilds it at one
;;; bucket per two entries, rounded up to a power of two (4,096: chains of 1.15
;;; on average).  PUTHASH regrows an index only past two entries a bucket, so
;;; the frozen size holds until the table has doubled.
(defun %mac-shrink-buckets (ht)
  (let ((holder (and ht (modus.runtime::%ht-bucket-holder ht))))
    (when (and holder (modus.runtime::%ht-pinned-p ht))
      (let ((old (modus.runtime::%ht-h-vec holder)) (cnt (modus.runtime::%ht-h-count holder)) (n 256))
        (when (and old (not (eq old (modus.runtime::%ht-nohash))) (not (consp old)))
          (loop (when (>= (* 2 n) cnt) (return nil)) (setq n (* n 2)))
          (when (< n (%prim-array-length old))
            (let ((vec (modus.runtime::%ht-new-bucket-vec n)) (cur (car ht)) (strcmp? (modus.runtime::%ht-h-strcmp holder)))
              (loop
                (when (null cur) (return nil))
                (let ((pair (car cur)))
                  (modus.runtime::%ht-bucket-put vec (car pair) pair strcmp?))
                (setq cur (cdr cur)))
              (modus.runtime::%ht-h-set-vec holder vec)
              n)))))))

;;; RIGHT-SIZED DICTS (a table that will not grow).  A compact dict doubles its capacity when it fills, so a
;;; boot table sits at up to twice its count: the symbol-function table's dict
;;; was 100 KB for 4,711 names.  At the freeze the Mac rebuilds a table's dict
;;; at its count plus SLACK (%HTD-REBUILD: the same entries, in order, a fresh
;;; index).  A table that will not grow (the kernel-name snapshots) gets none;
;;; one that will (the function and symbol tables) gets room for a session's
;;; definitions before it doubles again -- in the young heap then.
(defun %mac-right-size (ht slack)
  (let ((holder (and ht (modus.runtime::%ht-bucket-holder ht))))
    (when holder
      (let ((dict (modus.runtime::%ht-h-vec holder)))
        (when (consp dict)
          (let ((cap (+ (modus.runtime::%ht-h-count holder) slack))
                (old (ash (- (%prim-array-length (cdr dict)) 2) -1)))
            (when (< cap old)
              (modus.runtime::%htd-rebuild ht holder dict cap
                                           (modus.runtime::%ht-h-strcmp holder))
              cap)))))))

;;; COLD FUNCTIONS.  The function table (*SYMBOL-FUNCTION-TABLE*, name -> function)
;;; holds every DEFUN in the image, 4,700 names, and *KERNEL-FN-NAMES* most of them
;;; again: ~200 KB of a Mac Classic's static heap with their name strings, of which
;;; a session asks for a few dozen.  At the freeze of the core run %COLD-FNS-CUT
;;; takes out every entry whose function is the image's own (its code never
;;; moves) and writes it as a record -- hash.l, the function's raw word.l,
;;; flags.b (bit 0: a kernel name), length.b, the name, even -- into one byte
;;; vector whose raw word goes to the static block's #x10006048.
;;; scripts/mac68k-core.lisp takes the records out of the core, sorts them by hash
;;; into 512-byte pages after an index of each page's first hash, and puts the
;;; disk byte they start at in #x1000604C.  From then on a miss in either table
;;; (prelude's GETHASH, *%HT-COLD-TABLES*) asks %HT-COLD-MISS: the index once,
;;; then one block a name, and a name found is entered in the table again (and
;;; one not found in *COLD-FNS-MISSES*), so each costs a read once.
;;; NOT seen by a walk of the table (MAPHASH, APROPOS): only what a session added
;;; or asked for.  An FMAKUNBOUND of a cold name lasts until it is next asked for.
(defvar *cold-fns-out* nil)
(defvar *cold-fns-firsts* nil)
(defvar *cold-fns-misses* nil)

(defun %image-function-p (v)
  "Is V a function of the image itself (not a closure or an interpreted one)?"
  (and (functionp v) (eql (logand (%val->word v) 7) 3)))

(defun %cold-fns-cut ()
  (when (and (eql (mem-ref #x1000602C :u32) 1) *symbol-function-table*)
    (let ((names nil) (vals nil) (bytes 0) (kfn *kernel-fn-names*))
      (maphash (lambda (k v)
                 (when (and (stringp k) (< (length k) 256) (%image-function-p v))
                   (setq names (cons k names) vals (cons v vals))
                   (setq bytes (+ bytes 10 (length k) (logand (length k) 1)))))
               *symbol-function-table*)
      (let ((out (make-array bytes :element-type '(unsigned-byte 8))) (i 0) (n 0))
        (flet ((put4 (x)
                 (setf (aref out i) (logand (ash x -24) 255)
                       (aref out (+ i 1)) (logand (ash x -16) 255)
                       (aref out (+ i 2)) (logand (ash x -8) 255)
                       (aref out (+ i 3)) (logand x 255))
                 (setq i (+ i 4))))
          (loop
            (when (null names) (return nil))
            (let* ((k (car names)) (len (length k))
                   (kernel (and kfn (multiple-value-bind (x present) (gethash k kfn) x present))))
              (put4 (compute-name-hash k))
              ;; FROM CAR'S ENTRY, not absolute: these pages are not in the
              ;; loaded image, so a relocating load (mac68k-chain.s) does not
              ;; fix them; CAR's address in the running image does move.
              ;; Measured relocated on a Quadra 950: an absolute record sent
              ;; (+ 1 2) to a JSR of address 4.
              (put4 (logand (- (%val->word (car vals)) (%val->word (function car))) #xFFFFFFFF))
              (setf (aref out i) (if kernel 1 0) (aref out (+ i 1)) len)
              (setq i (+ i 2))
              (let ((j 0))
                (loop (when (>= j len) (return nil))
                  (setf (aref out i) (char-code (char k j)))
                  (setq i (+ i 1) j (+ j 1))))
              (when (oddp len) (setf (aref out i) 0) (setq i (+ i 1)))
              (remhash k *symbol-function-table*)
              (when kernel (remhash k kfn))
              (setq n (+ n 1)))
            (setq names (cdr names) vals (cdr vals))))
        (setq *cold-fns-out* out)
        (setq *%ht-cold-tables* t)
        n))))

(defun %cold-fns-hand-over ()
  "After the freeze: the records' raw word where mac68k-core.lisp looks."
  (when *cold-fns-out*
    (setf (mem-ref #x10006048 :u64) *cold-fns-out*)
    (setq *cold-fns-out* nil)))

(defun %cold-fns-read (page buf)
  (eql (%cold-disk-read (+ (mem-ref #x1000604C :u32) (* page 512)) buf) 0))

(defun %cold-fns-u32 (a)
  (+ (* (mem-ref a :u8) 16777216) (* (mem-ref (+ a 1) :u8) 65536)
     (* (mem-ref (+ a 2) :u8) 256) (mem-ref (+ a 3) :u8)))

(defun %cold-fn-delta (u)
  "A record's 32 bits as the signed distance from CAR's entry they are."
  (if (>= u #x80000000) (- u #x100000000) u))

(defun %cold-fns-firsts ()
  "The record pages' first hashes (the index pages, read once)."
  (or *cold-fns-firsts*
      (let ((buf *io-buf-addr*))
        (when (%cold-fns-read 0 buf)
          (let* ((n (%cold-fns-u32 buf)) (v (make-array n)) (i 0) (page 0) (off 4))
            (loop
              (when (>= i n) (return nil))
              (when (>= off 512)
                (setq page (+ page 1) off 0)
                (unless (%cold-fns-read page buf) (return-from %cold-fns-firsts nil)))
              (setf (svref v i) (%cold-fns-u32 (+ buf off)))
              (setq i (+ i 1) off (+ off 4)))
            (setq *cold-fns-firsts* (cons (floor (+ 4 (* 4 n) 511) 512) v)))))))

(defun %cold-fn-find (key)
  "(VALUE . FLAGS) for the name KEY from the cold functions on disk, or NIL."
  (when (and (stringp key) (< (length key) 256) (> (mem-ref #x1000604C :u32) 0)
             (not (and *cold-fns-misses* (gethash key *cold-fns-misses*))))
    (let* ((fi (%cold-fns-firsts)) (v (cdr fi)) (n (if fi (length v) 0))
           (h (compute-name-hash key)) (len (length key)) (found nil))
      (when (and (> n 0) (>= h (svref v 0)))
        (let ((lo 0) (hi (- n 1)) (buf *io-buf-addr*))
          (loop (when (>= lo hi) (return nil))
            (let ((mid (ash (+ lo hi 1) -1)))
              (if (<= (svref v mid) h) (setq lo mid) (setq hi (- mid 1)))))
          (when (%cold-fns-read (+ (car fi) lo) buf)
            (let ((i 0))
              (loop
                (when (> i 501) (return nil))
                (let ((k (%cold-fns-u32 (+ buf i))) (l (mem-ref (+ buf i 9) :u8)))
                  (when (or (eql k 0) (> k h)) (return nil))
                  (when (and (eql k h) (eql l len))
                    (let ((j 0))
                      (loop (when (>= j len) (return nil))
                        (unless (eql (mem-ref (+ buf i 10 j) :u8) (char-code (char key j)))
                          (return nil))
                        (setq j (+ j 1)))
                      (when (>= j len)
                        (setq found (cons (%word->val (+ (%cold-fn-delta (%cold-fns-u32 (+ buf i 4)))
                                                          (%val->word (function car))))
                                          (mem-ref (+ buf i 8) :u8)))
                        (return nil))))
                  (setq i (+ i 10 l (logand l 1)))))))))
      (unless found
        (unless *cold-fns-misses* (setq *cold-fns-misses* (make-hash-table)))
        (puthash key *cold-fns-misses* t))
      found)))

(defun %ht-cold-miss (key ht)
  (cond ((eq ht *symbol-function-table*)
         (let ((r (%cold-fn-find key)))
           (when r (puthash key ht (car r)) (list (car r)))))
        ((eq ht *kernel-fn-names*)
         (let ((r (%cold-fn-find key)))
           (when (and r (eql (logand (cdr r) 1) 1)) (puthash key ht t) (list t))))
        (t nil)))
