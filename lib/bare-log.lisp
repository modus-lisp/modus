;;;; bare-log.lisp - a message log for bare metal that you read when you want to,
;;;; not as it scrolls past on the serial console.
;;;;
;;;;   (klog 2 "USB " "port disabled")          ; level, a 4-character tag, a string
;;;;   (klog-event 3 "HPRT" hprt0 gintsts 0)    ; level, tag, three integers
;;;;   (klog-dump)                              ; this boot's messages, oldest first
;;;;   (klog-dump :boot :previous)              ; the boot before (survives a reset)
;;;;   (klog-echo-level 2)                      ; also print level >= 2 on the console
;;;;
;;;; WHO MAY LOG.  Any code, on any core, at any time: an append ALLOCATES NOTHING and
;;;; takes no lock, so the I/O core (which must never cons) and a fault path can both
;;;; use it.  KLOG-EVENT's arguments are integers stored as they are; KLOG's text is
;;;; copied byte by byte, so formatting a message (FORMAT NIL ...) is the CALLER's
;;;; allocation, never the log's.  Reading (KLOG-DUMP) allocates and belongs on a
;;;; thread that may.
;;;;
;;;; ONE RING PER CORE, so each ring has exactly one writer and no append needs an
;;;; atomic.  A ring is an array of fixed 128-byte records; the writer zeroes a
;;;; record's sequence word, fills it, issues a barrier and writes the sequence last,
;;;; so a reader on another core sees a record whole or skips it.  Rings overwrite
;;;; their oldest record when full; the dump merges all rings by timestamp (CNTVCT,
;;;; which every core shares).
;;;;
;;;; WHERE.  %KLOG-REGION is the board's choice (base, bytes); the default (0, 0) means
;;;; this image keeps no log, and every call returns at once.  The Pi puts it in the
;;;; Device-RAM window, which a board reset does not clear, so a boot that hung is
;;;; read by the next one: %KLOG-BOOT keeps the records and counts the boot.
;;;;
;;;; LAYOUT (u32 words unless said):
;;;;   header (4 KB):  +0 'KLOG'  +4 version  +8 boot number  +C ring bytes
;;;;                   +10 echo level (16 = never)
;;;;                   +40 + 16*core: next record index, records written, 0, 0
;;;;   ring c at base + 4 KB + c * ring bytes, records of 128 bytes:
;;;;     +0 seq (0: empty or being written)  +4 boot  +8 ticks low  +C ticks high
;;;;     +10 tag  +14 level | nargs<<4 | text length<<8 | core<<16 | flags<<20
;;;;     +18/+20/+28 three integers, each as low then high word
;;;;     +30..+7F text (80 bytes)

(declaim (overridable %klog-region))
(defun %klog-region () (values 0 0))
(declaim (overridable %klog-core))
(defun %klog-core () 0)

(defun %klog-ncores () 4)
(defun %klog-rec-bytes () 128)
(defun %klog-text-max () 80)
(defun %klog-version () 1)

(defun %klog-base () (multiple-value-bind (b n) (%klog-region) (if (> n #x2000) b 0)))
(defun %klog-ring-bytes ()
  (multiple-value-bind (b n) (%klog-region)
    (if (zerop b) 0
        (* (%klog-rec-bytes) (floor (floor (- n #x1000) (%klog-ncores)) (%klog-rec-bytes))))))
(defun %klog-ring-n () (floor (%klog-ring-bytes) (%klog-rec-bytes)))

(defun %klog-u32 (a) (mem-ref a :u32))
(defun %klog-set (a v) (setf (mem-ref a :u32) (logand v #xFFFFFFFF)))
(defun %klog-ready-p (b)
  (and (> b 0) (= (%klog-u32 b) #x474F4C4B) (= (%klog-u32 (+ b 4)) (%klog-version))
       (= (%klog-u32 (+ b #xC)) (%klog-ring-bytes))))

(defun %klog-boot ()
  "At boot, on the main thread, before any other core runs: keep a valid log and count
   this boot, or lay out an empty one.  Returns the boot number (0: no log here)."
  (let ((b (%klog-base)))
    (cond ((zerop b) 0)
          ((%klog-ready-p b)
           (%klog-set (+ b 8) (+ (%klog-u32 (+ b 8)) 1))
           (%klog-u32 (+ b 8)))
          (t
           (multiple-value-bind (base n) (%klog-region)
             (dotimes (i (floor n 4)) (setf (mem-ref (+ base (* 4 i)) :u32) 0)))
           (%klog-set (+ b 4) (%klog-version))
           (%klog-set (+ b 8) 1)
           (%klog-set (+ b #xC) (%klog-ring-bytes))
           (%klog-set (+ b #x10) 16)
           (%klog-set b #x474F4C4B)
           1))))

(defun klog-echo-level (level)
  "Also print messages at LEVEL and above on the console as they are logged (16: none)."
  (let ((b (%klog-base))) (when (> b 0) (%klog-set (+ b #x10) level))) level)

(defun klog-tag (s)
  "The tag word of the first four characters of S (padded with spaces)."
  (let ((w 0))
    (dotimes (i 4)
      (setq w (logior w (ash (if (< i (length s)) (logand (char-code (char s i)) #xFF) 32)
                             (* 8 i)))))
    w))

(defun %klog-int-p (x) (and (integerp x) (< x 4611686018427387904) (>= x -4611686018427387904)))

(defun %klog-put-int (a x)
  (if (%klog-int-p x)
      (progn (%klog-set a (logand x #xFFFFFFFF))
             (%klog-set (+ a 4) (logand (ash x -32) #xFFFFFFFF)))
      (progn (%klog-set a 0) (%klog-set (+ a 4) 0))))

(defun %klog-append (level tag nargs a b c text)
  "Write one record on this core's ring.  Allocates nothing.  The record's index."
  (let ((base (%klog-base)))
    (if (or (zerop base) (not (= (%klog-u32 base) #x474F4C4B)))
        -1
        (let* ((core (logand (%klog-core) 3))
               (h (+ base #x40 (* 16 core)))
               (idx (%klog-u32 h))
               (n (%klog-ring-n))
               (r (+ base #x1000 (* core (%klog-ring-bytes)) (* (mod idx n) (%klog-rec-bytes))))
               (count (+ (%klog-u32 (+ h 4)) 1))
               (ticks (rdtsc))
               (tlen (if (stringp text) (min (length text) (%klog-text-max)) 0))
               (bad (if (and (or (< nargs 1) (%klog-int-p a)) (or (< nargs 2) (%klog-int-p b))
                             (or (< nargs 3) (%klog-int-p c)))
                        0 1)))
          (%klog-set r 0)
          (memory-barrier)
          (%klog-set (+ r 4) (%klog-u32 (+ base 8)))
          (%klog-set (+ r 8) (logand ticks #xFFFFFFFF))
          (%klog-set (+ r #xC) (logand (ash ticks -32) #xFFFFFFFF))
          (%klog-set (+ r #x10) tag)
          (%klog-set (+ r #x14) (logior (logand level 15) (ash (logand nargs 15) 4) (ash tlen 8)
                                        (ash core 16) (ash bad 20)))
          (%klog-put-int (+ r #x18) a)
          (%klog-put-int (+ r #x20) b)
          (%klog-put-int (+ r #x28) c)
          (dotimes (i tlen)
            (setf (mem-ref (+ r #x30 i) :u8) (logand (char-code (char text i)) #xFF)))
          (memory-barrier)
          (%klog-set r (if (zerop (logand count #xFFFFFFFF)) 1 count))
          (%klog-set (+ h 4) count)
          (%klog-set h (+ idx 1))
          (when (>= level (%klog-u32 (+ base #x10))) (%klog-echo r))
          idx))))

(defun klog-event (level tag a b c)
  "Log three integers under TAG (a string of up to 4 characters, or a KLOG-TAG word)."
  (%klog-append level (if (stringp tag) (klog-tag tag) tag) 3 a b c nil))

(defun klog (level tag text)
  "Log the string TEXT (its first 80 characters) under TAG."
  (%klog-append level (if (stringp tag) (klog-tag tag) tag) 0 0 0 0 text))

;;; ---- echo: printing without allocating ------------------------------------------

(defun %klog-hex (v digits)
  (let ((d digits))
    (loop
      (when (<= d 0) (return nil))
      (setq d (- d 1))
      (let ((n (logand (ash v (- (* 4 d))) 15)))
        (write-char-serial (if (< n 10) (+ n 48) (+ n 87)))))))

(defun %klog-echo (r)
  (let ((info (%klog-u32 (+ r #x14))) (tag (%klog-u32 (+ r #x10))))
    (write-char-serial 91)                            ; [
    (write-char-serial (+ 48 (logand (ash info -16) 15)))
    (write-char-serial 32)
    (dotimes (i 4) (write-char-serial (logand (ash tag (- (* 8 i))) #xFF)))
    (write-char-serial 93)                            ; ]
    (dotimes (k (logand (ash info -4) 15))
      (write-char-serial 32)
      (%klog-hex (%klog-u32 (+ r #x18 (* 8 k))) 8))
    (when (> (logand (ash info -8) #xFF) 0) (write-char-serial 32))
    (dotimes (i (logand (ash info -8) #xFF)) (write-char-serial (mem-ref (+ r #x30 i) :u8)))
    (write-char-serial 13) (write-char-serial 10)))

;;; ---- reading --------------------------------------------------------------------

(defun %klog-s64 (r off)
  (let ((v (+ (%klog-u32 (+ r off)) (* (%klog-u32 (+ r off 4)) 4294967296))))
    (if (>= v 9223372036854775808) (- v 18446744073709551616) v)))

(defun %klog-read (r)
  "The record at R as a list, or NIL if it is empty or was being written."
  (let ((seq (%klog-u32 r)))
    (unless (zerop seq)
      (let* ((info (%klog-u32 (+ r #x14)))
             (tlen (min (logand (ash info -8) #xFF) (%klog-text-max)))
             (text (make-string tlen))
             (tag (%klog-u32 (+ r #x10)))
             (rec (list :ticks (+ (%klog-u32 (+ r 8)) (* (%klog-u32 (+ r #xC)) 4294967296))
                        :boot (%klog-u32 (+ r 4)) :core (logand (ash info -16) 15) :seq seq
                        :level (logand info 15)
                        :tag (let ((s (make-string 4)))
                               (dotimes (i 4) (setf (char s i) (code-char (logand (ash tag (- (* 8 i))) #xFF))))
                               s)
                        :args (let ((acc nil))
                                (dotimes (k (logand (ash info -4) 15))
                                  (push (%klog-s64 r (+ #x18 (* 8 k))) acc))
                                (nreverse acc))
                        :text text)))
        (dotimes (i tlen) (setf (char text i) (code-char (mem-ref (+ r #x30 i) :u8))))
        (when (= (%klog-u32 r) seq) rec)))))

(defun klog-records (&key (boot :current) core)
  "Every readable record of BOOT (:current, :previous, :all, or a number), oldest first."
  (let ((base (%klog-base)) (acc nil))
    (when (%klog-ready-p base)
      (let* ((now (%klog-u32 (+ base 8)))
             (want (case boot (:current now) (:previous (- now 1)) (:all nil) (t boot))))
        (dotimes (c (%klog-ncores))
          (when (or (null core) (= c core))
            (dotimes (i (%klog-ring-n))
              (let ((rec (%klog-read (+ base #x1000 (* c (%klog-ring-bytes)) (* i (%klog-rec-bytes))))))
                (when (and rec (or (null want) (= (getf rec :boot) want)))
                  (push rec acc))))))))
    (sort acc (lambda (x y) (or (< (getf x :boot) (getf y :boot))
                                (and (= (getf x :boot) (getf y :boot))
                                     (< (getf x :ticks) (getf y :ticks))))))))

(defun klog-dump (&key (boot :current) core (last 100) (stream *standard-output*))
  "Print the LAST messages of BOOT (see KLOG-RECORDS), oldest first.  Seconds are since
   the counter started."
  (let* ((recs (klog-records :boot boot :core core))
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

(defun klog-status ()
  "Where the log is and how full each ring is."
  (let ((b (%klog-base)))
    (if (not (%klog-ready-p b))
        (list :log nil)
        (list :base b :boot (%klog-u32 (+ b 8)) :ring-records (%klog-ring-n)
              :echo-level (%klog-u32 (+ b #x10))
              :written (let ((acc nil))
                         (dotimes (c (%klog-ncores)) (push (%klog-u32 (+ b #x44 (* 16 c))) acc))
                         (nreverse acc))))))
