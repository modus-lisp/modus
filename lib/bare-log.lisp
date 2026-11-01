;;;; bare-log.lisp - a message log for bare metal that you read when you want to,
;;;; not as it scrolls past on the serial console.
;;;;
;;;;   (kmsg 2 "USB " "port disabled")          ; level, a 4-character tag, a string
;;;;   (kmsg-event 3 "HPRT" hprt0 gintsts 0)    ; level, tag, three integers
;;;;   (kmsg-dump)                              ; this boot's messages, oldest first
;;;;   (kmsg-dump :boot :previous)              ; the boot before (survives a reset)
;;;;   (kmsg-echo-level 2)                      ; also print level >= 2 on the console
;;;;
;;;; WHO MAY LOG.  Any code, on any core, at any time: an append ALLOCATES NOTHING and
;;;; takes no lock, so the I/O core (which must never cons) and a fault path can both
;;;; use it.  KMSG-EVENT's arguments are integers stored as they are; KMSG's text is
;;;; copied byte by byte, so formatting a message (FORMAT NIL ...) is the CALLER's
;;;; allocation, never the log's.  Reading (KMSG-DUMP) allocates and belongs on a
;;;; thread that may.
;;;;
;;;; ONE RING PER CORE, so each ring has exactly one writer and no append needs an
;;;; atomic.  A ring is an array of fixed 128-byte records; the writer zeroes a
;;;; record's sequence word, fills it, issues a barrier and writes the sequence last,
;;;; so a reader on another core sees a record whole or skips it.  Rings overwrite
;;;; their oldest record when full; the dump merges all rings by timestamp (CNTVCT,
;;;; which every core shares).
;;;;
;;;; WHERE.  %KMSG-REGION is the board's choice (base, bytes); the default (0, 0) means
;;;; this image keeps no log, and every call returns at once.  The Pi puts it in the
;;;; Device-RAM window, which a board reset does not clear, so a boot that hung is
;;;; read by the next one: %KMSG-BOOT keeps the records and counts the boot.
;;;;
;;;; LAYOUT (u32 words unless said):
;;;;   header (4 KB):  +0 'KMSG'  +4 version  +8 boot number  +C ring bytes
;;;;                   +10 echo level (16 = never)
;;;;                   +40 + 16*core: next record index, records written, 0, 0
;;;;   ring c at base + 4 KB + c * ring bytes, records of 128 bytes:
;;;;     +0 seq (0: empty or being written)  +4 boot  +8 ticks low  +C ticks high
;;;;     +10 tag  +14 level | nargs<<4 | text length<<8 | core<<16 | flags<<20
;;;;     +18/+20/+28 three integers, each as low then high word
;;;;     +30..+7F text (80 bytes)

(declaim (overridable %kmsg-region))
(defun %kmsg-region () (values 0 0))
(declaim (overridable %kmsg-core))
(defun %kmsg-core () 0)

(defun %kmsg-ncores () 4)
(defun %kmsg-rec-bytes () 128)
(defun %kmsg-text-max () 80)
(defun %kmsg-version () 1)

(defun %kmsg-base () (multiple-value-bind (b n) (%kmsg-region) (if (> n #x2000) b 0)))
(defun %kmsg-ring-bytes ()
  (multiple-value-bind (b n) (%kmsg-region)
    (if (zerop b) 0
        (* (%kmsg-rec-bytes) (floor (floor (- n #x1000) (%kmsg-ncores)) (%kmsg-rec-bytes))))))
(defun %kmsg-ring-n () (floor (%kmsg-ring-bytes) (%kmsg-rec-bytes)))

(defun %kmsg-u32 (a) (mem-ref a :u32))
(defun %kmsg-set (a v) (setf (mem-ref a :u32) (logand v #xFFFFFFFF)))
(defun %kmsg-ready-p (b)
  (and (> b 0) (= (%kmsg-u32 b) #x474F4C4B) (= (%kmsg-u32 (+ b 4)) (%kmsg-version))
       (= (%kmsg-u32 (+ b #xC)) (%kmsg-ring-bytes))))

(defun %kmsg-boot ()
  "At boot, on the main thread, before any other core runs: keep a valid log and count
   this boot, or lay out an empty one.  Returns the boot number (0: no log here)."
  (let ((b (%kmsg-base)))
    (cond ((zerop b) 0)
          ((%kmsg-ready-p b)
           (%kmsg-set (+ b 8) (+ (%kmsg-u32 (+ b 8)) 1))
           (%kmsg-u32 (+ b 8)))
          (t
           (multiple-value-bind (base n) (%kmsg-region)
             (dotimes (i (floor n 4)) (setf (mem-ref (+ base (* 4 i)) :u32) 0)))
           (%kmsg-set (+ b 4) (%kmsg-version))
           (%kmsg-set (+ b 8) 1)
           (%kmsg-set (+ b #xC) (%kmsg-ring-bytes))
           (%kmsg-set (+ b #x10) 16)
           (%kmsg-set b #x474F4C4B)
           1))))

(defun kmsg-echo-level (level)
  "Also print messages at LEVEL and above on the console as they are logged (16: none)."
  (let ((b (%kmsg-base))) (when (> b 0) (%kmsg-set (+ b #x10) level))) level)

(defun kmsg-tag (s)
  "The tag word of the first four characters of S (padded with spaces)."
  (let ((w 0))
    (dotimes (i 4)
      (setq w (logior w (ash (if (< i (length s)) (logand (char-code (char s i)) #xFF) 32)
                             (* 8 i)))))
    w))

(defun %kmsg-int-p (x) (fixnump x))   ; NOT a range test against 2^62: that literal is a bignum, built on every call

(defun %kmsg-put-int (a x)
  (if (%kmsg-int-p x)
      (progn (%kmsg-set a (logand x #xFFFFFFFF))
             (%kmsg-set (+ a 4) (logand (ash x -32) #xFFFFFFFF)))
      (progn (%kmsg-set a 0) (%kmsg-set (+ a 4) 0))))

(defun %kmsg-append (level tag nargs a b c text)
  "Write one record on this core's ring.  Allocates nothing.  The record's index."
  (let ((base (%kmsg-base)))
    (if (or (zerop base) (not (= (%kmsg-u32 base) #x474F4C4B)))
        -1
        (let* ((core (logand (%kmsg-core) 3))
               (h (+ base #x40 (* 16 core)))
               (idx (%kmsg-u32 h))
               (n (%kmsg-ring-n))
               (r (+ base #x1000 (* core (%kmsg-ring-bytes)) (* (mod idx n) (%kmsg-rec-bytes))))
               (count (+ (%kmsg-u32 (+ h 4)) 1))
               (ticks (rdtsc))
               (tlen (if (stringp text) (min (length text) (%kmsg-text-max)) 0))
               (bad (if (and (or (< nargs 1) (%kmsg-int-p a)) (or (< nargs 2) (%kmsg-int-p b))
                             (or (< nargs 3) (%kmsg-int-p c)))
                        0 1)))
          (%kmsg-set r 0)
          (memory-barrier)
          (%kmsg-set (+ r 4) (%kmsg-u32 (+ base 8)))
          (%kmsg-set (+ r 8) (logand ticks #xFFFFFFFF))
          (%kmsg-set (+ r #xC) (logand (ash ticks -32) #xFFFFFFFF))
          (%kmsg-set (+ r #x10) tag)
          (%kmsg-set (+ r #x14) (logior (logand level 15) (ash (logand nargs 15) 4) (ash tlen 8)
                                        (ash core 16) (ash bad 20)))
          (%kmsg-put-int (+ r #x18) a)
          (%kmsg-put-int (+ r #x20) b)
          (%kmsg-put-int (+ r #x28) c)
          (dotimes (i tlen)
            (setf (mem-ref (+ r #x30 i) :u8) (logand (char-code (char text i)) #xFF)))
          (memory-barrier)
          (%kmsg-set r (if (zerop (logand count #xFFFFFFFF)) 1 count))
          (%kmsg-set (+ h 4) count)
          (%kmsg-set h (+ idx 1))
          (when (>= level (%kmsg-u32 (+ base #x10))) (%kmsg-echo r))
          idx))))

(defun kmsg-event (level tag a b c)
  "Log three integers under TAG (a string of up to 4 characters, or a KMSG-TAG word)."
  (%kmsg-append level (if (stringp tag) (kmsg-tag tag) tag) 3 a b c nil))

(defun kmsg (level tag text)
  "Log the string TEXT (its first 80 characters) under TAG."
  (%kmsg-append level (if (stringp tag) (kmsg-tag tag) tag) 0 0 0 0 text))

;;; ---- echo: printing without allocating ------------------------------------------

(defun %kmsg-hex (v digits)
  (let ((d digits))
    (loop
      (when (<= d 0) (return nil))
      (setq d (- d 1))
      (let ((n (logand (ash v (- (* 4 d))) 15)))
        (write-char-serial (if (< n 10) (+ n 48) (+ n 87)))))))

(defun %kmsg-echo (r)
  (let ((info (%kmsg-u32 (+ r #x14))) (tag (%kmsg-u32 (+ r #x10))))
    (write-char-serial 91)                            ; [
    (write-char-serial (+ 48 (logand (ash info -16) 15)))
    (write-char-serial 32)
    (dotimes (i 4) (write-char-serial (logand (ash tag (- (* 8 i))) #xFF)))
    (write-char-serial 93)                            ; ]
    (dotimes (k (logand (ash info -4) 15))
      (write-char-serial 32)
      (%kmsg-hex (%kmsg-u32 (+ r #x18 (* 8 k))) 8))
    (when (> (logand (ash info -8) #xFF) 0) (write-char-serial 32))
    (dotimes (i (logand (ash info -8) #xFF)) (write-char-serial (mem-ref (+ r #x30 i) :u8)))
    (write-char-serial 13) (write-char-serial 10)))

;;; ---- reading --------------------------------------------------------------------

(defun %kmsg-s64 (r off)
  (let ((v (+ (%kmsg-u32 (+ r off)) (* (%kmsg-u32 (+ r off 4)) 4294967296))))
    (if (>= v 9223372036854775808) (- v 18446744073709551616) v)))

(defun %kmsg-read (r)
  "The record at R as a list, or NIL if it is empty or was being written."
  (let ((seq (%kmsg-u32 r)))
    (unless (zerop seq)
      (let* ((info (%kmsg-u32 (+ r #x14)))
             (tlen (min (logand (ash info -8) #xFF) (%kmsg-text-max)))
             (text (make-string tlen))
             (tag (%kmsg-u32 (+ r #x10)))
             (rec (list :ticks (+ (%kmsg-u32 (+ r 8)) (* (%kmsg-u32 (+ r #xC)) 4294967296))
                        :boot (%kmsg-u32 (+ r 4)) :core (logand (ash info -16) 15) :seq seq
                        :level (logand info 15)
                        :tag (let ((s (make-string 4)))
                               (dotimes (i 4) (setf (char s i) (code-char (logand (ash tag (- (* 8 i))) #xFF))))
                               s)
                        :args (let ((acc nil))
                                (dotimes (k (logand (ash info -4) 15))
                                  (push (%kmsg-s64 r (+ #x18 (* 8 k))) acc))
                                (nreverse acc))
                        :text text)))
        (dotimes (i tlen) (setf (char text i) (code-char (mem-ref (+ r #x30 i) :u8))))
        (when (= (%kmsg-u32 r) seq) rec)))))

(defun kmsg-records (&key (boot :current) core)
  "Every readable record of BOOT (:current, :previous, :all, or a number), oldest first."
  (let ((base (%kmsg-base)) (acc nil))
    (when (%kmsg-ready-p base)
      (let* ((now (%kmsg-u32 (+ base 8)))
             (want (case boot (:current now) (:previous (- now 1)) (:all nil) (t boot))))
        (dotimes (c (%kmsg-ncores))
          (when (or (null core) (= c core))
            (dotimes (i (%kmsg-ring-n))
              (let ((rec (%kmsg-read (+ base #x1000 (* c (%kmsg-ring-bytes)) (* i (%kmsg-rec-bytes))))))
                (when (and rec (or (null want) (= (getf rec :boot) want)))
                  (push rec acc))))))))
    (sort acc (lambda (x y) (or (< (getf x :boot) (getf y :boot))
                                (and (= (getf x :boot) (getf y :boot))
                                     (< (getf x :ticks) (getf y :ticks))))))))

(defun kmsg-dump (&key (boot :current) core (last 100) (stream *standard-output*))
  "Print the LAST messages of BOOT (see KMSG-RECORDS), oldest first.  Seconds are since
   the counter started."
  (let* ((recs (kmsg-records :boot boot :core core))
         (n (length recs))
         (hz (max 1 (cntfrq))))
    (when (and last (> n last)) (setq recs (nthcdr (- n last) recs)))
    (dolist (r recs)
      (multiple-value-bind (s frac) (floor (getf r :ticks) hz)
        (format stream "~&b~D c~D ~6D.~6,'0D ~A ~A~{ ~D~}~@[ ~A~]~%"
                (getf r :boot) (getf r :core) s (floor (* frac 1000000) hz)
                (case (getf r :level) (0 "debug") (1 "info ") (2 "WARN ") (3 "ERROR") (t "?    "))
                (getf r :tag) (getf r :args)
                (let ((tx (getf r :text))) (and (plusp (length tx)) tx)))))
    (format stream "~&;; ~D message~:P~@[ (showing ~D)~]~%" n (and last (> n last) last))
    n))

(defun kmsg-status ()
  "Where the log is and how full each ring is."
  (let ((b (%kmsg-base)))
    (if (not (%kmsg-ready-p b))
        (list :log nil)
        (list :base b :boot (%kmsg-u32 (+ b 8)) :ring-records (%kmsg-ring-n)
              :echo-level (%kmsg-u32 (+ b #x10))
              :written (let ((acc nil))
                         (dotimes (c (%kmsg-ncores)) (push (%kmsg-u32 (+ b #x44 (* 16 c))) acc))
                         (nreverse acc))))))
