;;;; modus-channel.lisp — MODUS:MAKE-CHANNEL, the message channel between threads.
;;;; Carried as a source string and evaluated at boot right after the SB-THREAD
;;;; surface it is built on (mvm/build-cli-common.lisp, *SB-SHIM-TEXT*).
;;;;
;;;; WHY.  Modus threads share no state: a thread's objects live in its own GC
;;;; region, and the shared-store guard refuses a store that would let another
;;;; thread reach one.  The sanctioned way across was MODUS:SHARE (and
;;;; JOIN-THREAD's result, which is the same copy), and it copies into the
;;;; runtime lock arena: measured, ~8.9 KB of a 64 MB arena per 1000-character
;;;; string.  That arena is reclaimed only by a STOP-THE-WORLD collection of
;;;; region 0 that evacuates it, run once it is three-quarters full -- so a
;;;; thread passing messages that way stops every thread every few thousand
;;;; messages.  A relay subscription or a request loop passes that many in
;;;; minutes.  Threads -- and actors on top of them -- need a channel whose
;;;; messages cost the shared heap nothing.
;;;;
;;;; HOW.  A channel is ONE ring of bytes in shared memory, allocated once, plus
;;;; a mutex and two wait queues.  CHANNEL-SEND encodes the value in the SENDER's
;;;; own heap, copies the BYTES into the ring (a byte is not an object, so the
;;;; guard has nothing to refuse), and CHANNEL-RECEIVE copies them out and
;;;; decodes a fresh value in the RECEIVER's heap.  Per message the arena is not
;;;; touched at all; both sides' garbage is ordinary garbage.  This is the actor
;;;; system's TERM-ENCODE / TERM-DECODE idea (copy, never share) without its fixed
;;;; staging buffers.
;;;;
;;;; WHAT CAN BE SENT: NIL, T, integers of any size, ratios, single and double
;;;; floats, complexes, characters, strings, symbols (interned again by name in
;;;; the receiver; uninterned ones arrive as fresh uninterned symbols), lists
;;;; (dotted too), simple vectors, (unsigned-byte 8) vectors and hash tables
;;;; (EQ/EQL/EQUAL/EQUALP).  Anything else -- a closure, a struct, a condition, a
;;;; stream -- SIGNALS in the sender, before the channel is touched.  Values
;;;; arrive EQUAL, never EQ: it is a copy.  A shared substructure arrives as two
;;;; copies, and a circular one is refused.
;;;;
;;;; BOUNDS.  A message larger than the ring signals.  A full ring blocks the
;;;; sender (or times out); an empty one blocks the receiver (or times out).  Each
;;;; channel takes one mutex and two wait queues from the synchronisation arena
;;;; (65536 cells for the life of the process, net/hosted-sync.lisp) and its ring
;;;; from the runtime arena: make channels once and reuse them.

(defstruct (modus::channel (:constructor %make-channel) (:conc-name %chan-)
                           (:predicate modus::channel-p))
  (name nil)
  (ring nil)                 ; (unsigned-byte 8) vector in shared memory
  (cap 0)
  (head 0)                   ; read offset
  (used 0)                   ; bytes in the ring
  (mutex nil)
  (not-empty nil)
  (not-full nil))

;;; ---- encoding (in the sender's own heap) ----------------------------------

(defun %chan-out-new () (cons (make-array 256 :element-type '(unsigned-byte 8)) 0))

(defun %chan-reserve (out k)
  "OUT's vector, grown if needed so K more bytes fit."
  (let ((v (car out)) (n (cdr out)))
    (if (<= (+ n k) (length v))
        v
        (let ((w (make-array (max (* 2 (length v)) (+ n k 256))
                             :element-type '(unsigned-byte 8))))
          (replace w v :end2 n)
          (setf (car out) w)
          w))))

(defun %chan-put (out b)
  (let ((v (%chan-reserve out 1)) (n (cdr out)))
    (declare (type (simple-array (unsigned-byte 8) (*)) v) (type fixnum n))
    (setf (aref v n) b)
    (setf (cdr out) (+ n 1))))

(defun %chan-put-u32 (out x)
  (%chan-put out (logand x 255))
  (%chan-put out (logand (floor x 256) 255))
  (%chan-put out (logand (floor x 65536) 255))
  (%chan-put out (logand (floor x 16777216) 255)))

(defun %chan-put-uint (out x)
  "A non-negative integer of any size: byte count, then bytes, low first."
  (let ((bytes nil) (m x))
    (loop while (> m 0)
          do (multiple-value-bind (q r) (floor m 256)
               (setq bytes (cons r bytes) m q)))
    (setq bytes (nreverse bytes))
    (%chan-put-u32 out (length bytes))
    (dolist (b bytes) (%chan-put out b))))

(defun %chan-put-int (out x)
  (%chan-put out (if (minusp x) 1 0))
  (%chan-put-uint out (abs x)))

(defun %chan-put-utf8 (out s)
  "S's characters as UTF-8, preceded by the BYTE count."
  (let* ((len (length s))
         (v (%chan-reserve out (+ 4 (* 4 len))))
         (mark (cdr out))
         (n (+ mark 4)))
    (declare (type (simple-array (unsigned-byte 8) (*)) v) (type fixnum len mark n))
    (dotimes (i len)
      (let ((c (char-code (char s i))))
        (declare (type fixnum c))
        (cond ((< c #x80) (setf (aref v n) c) (setq n (+ n 1)))
              ((< c #x800)
               (setf (aref v n) (logior #xC0 (ash c -6))
                     (aref v (+ n 1)) (logior #x80 (logand c 63)))
               (setq n (+ n 2)))
              ((< c #x10000)
               (setf (aref v n) (logior #xE0 (ash c -12))
                     (aref v (+ n 1)) (logior #x80 (logand (ash c -6) 63))
                     (aref v (+ n 2)) (logior #x80 (logand c 63)))
               (setq n (+ n 3)))
              (t
               (setf (aref v n) (logior #xF0 (ash c -18))
                     (aref v (+ n 1)) (logior #x80 (logand (ash c -12) 63))
                     (aref v (+ n 2)) (logior #x80 (logand (ash c -6) 63))
                     (aref v (+ n 3)) (logior #x80 (logand c 63)))
               (setq n (+ n 4))))))
    (let ((k (- n mark 4)))
      (setf (aref v mark) (logand k 255)
            (aref v (+ mark 1)) (logand (ash k -8) 255)
            (aref v (+ mark 2)) (logand (ash k -16) 255)
            (aref v (+ mark 3)) (logand (ash k -24) 255)))
    (setf (cdr out) n)))

(defun %chan-put-float (out x tag)
  (multiple-value-bind (m e s) (integer-decode-float x)
    (%chan-put out tag)
    (%chan-put out (if (minusp s) 1 0))
    (%chan-put-int out e)
    (%chan-put-uint out m)))

(defun %chan-refuse (x why)
  (error "modus:channel-send: ~A cannot be sent between threads (~A)." x why))

(defun %chan-encode-1 (out x depth)
  (when (> depth 4000)
    (%chan-refuse "a value" "nested more than 4000 deep, or circular"))
  (cond
    ((null x) (%chan-put out 0))
    ((eq x t) (%chan-put out 1))
    ((integerp x) (%chan-put out 2) (%chan-put-int out x))
    ((typep x 'ratio)
     (%chan-put out 3)
     (%chan-encode-1 out (numerator x) (+ depth 1))
     (%chan-encode-1 out (denominator x) (+ depth 1)))
    ((typep x 'double-float) (%chan-put-float out x 4))
    ((typep x 'single-float) (%chan-put-float out x 5))
    ((complexp x)
     (%chan-put out 6)
     (%chan-encode-1 out (realpart x) (+ depth 1))
     (%chan-encode-1 out (imagpart x) (+ depth 1)))
    ((characterp x)
     (let ((c (char-code x)))
       (%chan-put out 7)
       (%chan-put out (logand c 255))
       (%chan-put out (logand (floor c 256) 255))
       (%chan-put out (logand (floor c 65536) 255))))
    ((stringp x) (%chan-put out 8) (%chan-put-utf8 out x))
    ((symbolp x)
     (let ((p (symbol-package x)))
       (%chan-put out 9)
       (if p
           (progn (%chan-put out 1) (%chan-put-utf8 out (package-name p)))
           (%chan-put out 0))
       (%chan-put-utf8 out (symbol-name x))))
    ((consp x)
     ;; COUNT, the elements, then the tail -- iterative along the CDR, with a
     ;; tortoise so a circular spine is refused rather than looped on.
     (let ((n 0) (p x) (slow x))
       (loop while (consp p)
             do (setq p (cdr p) n (+ n 1))
                (when (evenp n) (setq slow (cdr slow)))
                (when (and (consp p) (eq p slow))
                  (%chan-refuse "a list" "it is circular")))
       (%chan-put out 10)
       (%chan-put-u32 out n)
       (let ((q x))
         (dotimes (i n)
           (%chan-encode-1 out (car q) (+ depth 1))
           (setq q (cdr q)))
         (%chan-encode-1 out q (+ depth 1)))))
    ((typep x '(simple-array (unsigned-byte 8) (*)))
     (%chan-put out 12)
     (%chan-put-u32 out (length x))
     (dotimes (i (length x)) (%chan-put out (aref x i))))
    ((simple-vector-p x)
     (%chan-put out 11)
     (%chan-put-u32 out (length x))
     (dotimes (i (length x)) (%chan-encode-1 out (svref x i) (+ depth 1))))
    ((hash-table-p x)
     (let ((test (hash-table-test x)))
       (%chan-put out 13)
       (%chan-encode-1 out (if (functionp test)
                               (cond ((eq test (function eq)) 'eq)
                                     ((eq test (function eql)) 'eql)
                                     ((eq test (function equal)) 'equal)
                                     (t 'equalp))
                               test)
                       (+ depth 1))
       (%chan-put-u32 out (hash-table-count x))
       (maphash (lambda (k v)
                  (%chan-encode-1 out k (+ depth 1))
                  (%chan-encode-1 out v (+ depth 1)))
                x)))
    ((functionp x) (%chan-refuse "a function" "a closure cannot be copied"))
    (t (%chan-refuse x (format nil "an object of type ~S" (type-of x))))))

(defun %chan-encode (x)
  "X as bytes: (VECTOR . LENGTH), in this thread's own heap."
  (let ((out (%chan-out-new)))
    (%chan-encode-1 out x 0)
    out))

;;; ---- decoding (in the receiver's own heap) --------------------------------

(defun %chan-decode (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (let ((pos 0))
    (declare (type fixnum pos))
    (labels ((u8 () (prog1 (aref v pos) (setq pos (+ pos 1))))
             (u32 () (let ((a (u8)) (b (u8)) (c (u8)) (d (u8)))
                       (+ a (* 256 b) (* 65536 c) (* 16777216 d))))
             (uint () (let ((n (u32)) (x 0) (scale 1))
                        (dotimes (i n x)
                          (setq x (+ x (* (u8) scale)) scale (* scale 256)))))
             (int () (let ((neg (u8)) (m (uint))) (if (= neg 1) (- m) m)))
             (utf8 ()
               ;; Count the characters (bytes that do not start 10xxxxxx), make
               ;; the string once, fill it: no cons per character.
               (let* ((nbytes (u32)) (end (+ pos nbytes)) (n 0))
                 (declare (type fixnum nbytes end n))
                 (let ((i pos))
                   (declare (type fixnum i))
                   (loop while (< i end)
                         do (unless (= (logand (aref v i) #xC0) #x80) (setq n (+ n 1)))
                            (setq i (+ i 1))))
                 ;; A LOCAL index: POS is shared with the other local
                 ;; functions, so every (U8) is a heap-cell read and write.
                 (let ((s (make-string n)) (k 0) (i pos))
                   (declare (type fixnum k i))
                   (loop while (< i end)
                         do (let ((b (aref v i)))
                              (declare (type fixnum b))
                              (setf (char s k)
                                    (code-char
                                     (cond ((< b #x80) (setq i (+ i 1)) b)
                                           ((< b #xE0)
                                            (setq i (+ i 2))
                                            (+ (* 64 (logand b 31))
                                               (logand (aref v (- i 1)) 63)))
                                           ((< b #xF0)
                                            (setq i (+ i 3))
                                            (+ (* 4096 (logand b 15))
                                               (* 64 (logand (aref v (- i 2)) 63))
                                               (logand (aref v (- i 1)) 63)))
                                           (t
                                            (setq i (+ i 4))
                                            (+ (* 262144 (logand b 7))
                                               (* 4096 (logand (aref v (- i 3)) 63))
                                               (* 64 (logand (aref v (- i 2)) 63))
                                               (logand (aref v (- i 1)) 63))))))
                              (setq k (+ k 1))))
                   (setq pos i)
                   s)))
             (flt (proto)
               (let* ((neg (u8)) (e (int)) (m (uint))
                      (f (scale-float (float m proto) e)))
                 (if (= neg 1) (- f) f)))
             (val ()
               (let ((tag (u8)))
                 (cond
                   ((= tag 0) nil)
                   ((= tag 1) t)
                   ((= tag 2) (int))
                   ((= tag 3) (let* ((a (val)) (b (val))) (/ a b)))
                   ((= tag 4) (flt 1d0))
                   ((= tag 5) (flt 1f0))
                   ((= tag 6) (let* ((a (val)) (b (val))) (complex a b)))
                   ((= tag 7) (let* ((a (u8)) (b (u8)) (c (u8)))
                                (code-char (+ a (* 256 b) (* 65536 c)))))
                   ((= tag 8) (utf8))
                   ((= tag 9) (let* ((has (u8))
                                     (pkg (if (= has 1) (utf8) nil))
                                     (name (utf8)))
                                (if pkg
                                    (let ((p (find-package pkg)))
                                      (if p
                                          (intern name p)
                                          (error "modus:channel-receive: no package ~A here (for the symbol ~A)."
                                                 pkg name)))
                                    (make-symbol name))))
                   ((= tag 10)
                    (let* ((n (u32)) (head (cons nil nil)) (tail head))
                      (dotimes (i n)
                        (let ((c (cons (val) nil)))
                          (setf (cdr tail) c)
                          (setq tail c)))
                      (setf (cdr tail) (val))
                      (cdr head)))
                   ((= tag 11)
                    (let* ((n (u32)) (r (make-array n)))
                      (dotimes (i n r) (setf (svref r i) (val)))))
                   ((= tag 12)
                    (let* ((n (u32)) (r (make-array n :element-type '(unsigned-byte 8))))
                      (dotimes (i n r) (setf (aref r i) (u8)))))
                   ((= tag 13)
                    (let* ((test (val)) (n (u32))
                           (h (make-hash-table :test test)))
                      (dotimes (i n h)
                        (let* ((k (val)) (x (val)))
                          (setf (gethash k h) x)))))
                   (t (error "modus:channel-receive: corrupt message (tag ~D at byte ~D)."
                             tag (- pos 1)))))))
      (val))))

;;; ---- the channel ----------------------------------------------------------

;;; Copies between a message and the ring: at most two straight runs (the ring
;;; wraps at most once per message), with types declared so the byte accesses
;;; compile inline.
(defun %chan-copy-in (ring cap at bytes n)
  (declare (type (simple-array (unsigned-byte 8) (*)) ring bytes)
           (type fixnum cap at n))
  (let ((first (min n (- cap at))))
    (declare (type fixnum first))
    (dotimes (i first) (setf (aref ring (+ at i)) (aref bytes i)))
    (dotimes (i (- n first)) (setf (aref ring i) (aref bytes (+ first i))))))

(defun %chan-copy-out (ring cap at bytes n)
  "Copy N bytes out of RING from AT into BYTES; the offset after them."
  (declare (type (simple-array (unsigned-byte 8) (*)) ring bytes)
           (type fixnum cap at n))
  (let ((first (min n (- cap at))))
    (declare (type fixnum first))
    (dotimes (i first) (setf (aref bytes i) (aref ring (+ at i))))
    (dotimes (i (- n first)) (setf (aref bytes (+ first i)) (aref ring i)))
    (if (< first n) (- n first) (let ((e (+ at n))) (if (= e cap) 0 e)))))

(defun modus::make-channel (&key (capacity 1048576) name)
  "A channel between threads: CHANNEL-SEND copies a value in, CHANNEL-RECEIVE
   copies it out into the receiving thread's own heap.  CAPACITY is the size of
   its ring in bytes and bounds one message.  Make channels once and reuse them:
   the ring and the channel's lock and wait queues are never reclaimed."
  (when (or (not (integerp capacity)) (< capacity 64))
    (error "modus:make-channel: :CAPACITY must be an integer of at least 64 bytes, not ~S."
           capacity))
  (modus::share
   (%make-channel :name name
                  :ring (modus::make-shared-array capacity :element-type '(unsigned-byte 8))
                  :cap capacity
                  :mutex (sb-thread:make-mutex :name "modus channel")
                  :not-empty (sb-thread:make-waitqueue :name "modus channel: not empty")
                  :not-full (sb-thread:make-waitqueue :name "modus channel: not full"))))

(defun %chan-remaining (deadline)
  "Seconds left before DEADLINE (a %MONOTONIC-NS value), NIL for no deadline."
  (if deadline (/ (max 0 (- deadline (%monotonic-ns))) 1000000000) nil))

(defun %chan-deadline (timeout)
  (if timeout (+ (%monotonic-ns) (truncate (* timeout 1000000000) 1)) nil))

(defun modus::channel-send (channel value &key timeout)
  "Copy VALUE into CHANNEL.  T once it is in; NIL if TIMEOUT seconds passed
   with the channel too full to take it (no TIMEOUT: wait as long as it takes).
   Signals, before touching the channel, if VALUE cannot be sent (see the file
   header) or does not fit in the channel at all."
  (let* ((out (%chan-encode value))
         (bytes (car out)) (n (cdr out)) (need (+ n 4))
         (cap (%chan-cap channel)) (ring (%chan-ring channel))
         (mutex (%chan-mutex channel))
         (deadline (%chan-deadline timeout))
         (ok nil))
    (when (> need cap)
      (error "modus:channel-send: a ~D-byte message does not fit in a ~D-byte channel."
             need cap))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (progn
           (loop while (> need (- cap (%chan-used channel)))
                 do (when (and deadline (<= (%chan-remaining deadline) 0)) (return nil))
                    (sb-thread:condition-wait (%chan-not-full channel) mutex
                                              :timeout (%chan-remaining deadline)))
           (when (<= need (- cap (%chan-used channel)))
             (let ((at (mod (+ (%chan-head channel) (%chan-used channel)) cap)))
               (dotimes (i 4)
                 (setf (aref ring at) (logand (ash n (* -8 i)) 255))
                 (setq at (if (= (+ at 1) cap) 0 (+ at 1))))
               (%chan-copy-in ring cap at bytes n))
             (setf (%chan-used channel) (+ (%chan-used channel) need))
             (sb-thread:condition-broadcast (%chan-not-empty channel))
             (setq ok t)))
      (sb-thread:release-mutex mutex))
    ok))

(defun modus::channel-receive (channel &key timeout)
  "The next value sent on CHANNEL, as a fresh copy in this thread's heap, and T.
   With TIMEOUT (seconds), NIL and NIL if nothing arrived in time; without,
   wait as long as it takes."
  (let* ((cap (%chan-cap channel)) (ring (%chan-ring channel))
         (mutex (%chan-mutex channel))
         (deadline (%chan-deadline timeout))
         (bytes nil))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (progn
           (loop while (< (%chan-used channel) 4)
                 do (when (and deadline (<= (%chan-remaining deadline) 0)) (return nil))
                    (sb-thread:condition-wait (%chan-not-empty channel) mutex
                                              :timeout (%chan-remaining deadline)))
           (when (>= (%chan-used channel) 4)
             (let ((at (%chan-head channel)) (n 0))
               (dotimes (i 4)
                 (setq n (+ n (ash (aref ring at) (* 8 i))))
                 (setq at (if (= (+ at 1) cap) 0 (+ at 1))))
               (setq bytes (make-array n :element-type '(unsigned-byte 8)))
               (setq at (%chan-copy-out ring cap at bytes n))
               (setf (%chan-head channel) at)
               (setf (%chan-used channel) (- (%chan-used channel) (+ n 4)))
               (sb-thread:condition-broadcast (%chan-not-full channel)))))
      (sb-thread:release-mutex mutex))
    (if bytes
        (values (%chan-decode bytes) t)
        (values nil nil))))

(defun modus::channel-try-receive (channel)
  "CHANNEL-RECEIVE without waiting: the next value and T, or NIL and NIL."
  (modus::channel-receive channel :timeout 0))

(export (list (intern "CHANNEL" "MODUS") (intern "CHANNEL-P" "MODUS")
              (intern "MAKE-CHANNEL" "MODUS") (intern "CHANNEL-SEND" "MODUS")
              (intern "CHANNEL-RECEIVE" "MODUS") (intern "CHANNEL-TRY-RECEIVE" "MODUS"))
        "MODUS")

;;; Portable code asks #+modus-channel, so a library can use channels and still
;;; load on a modus that predates them.
(pushnew :modus-channel *features*)
