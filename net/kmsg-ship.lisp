;;;; kmsg-ship.lisp - the message log (lib/bare-log.lisp) leaves the board on its own.
;;;;
;;;;   (modus:kmsg-ship-to "10.0.0.1" 514)   ; ship every record to that host's syslog
;;;;   (modus:kmsg-ship-to nil)               ; stop
;;;;   (modus:kmsg-ship-status)
;;;;
;;;; THE RINGS STAY THE TRUTH.  Appending never touches the network -- it never allocates,
;;;; blocks or locks, which is what the I/O core and fault paths need -- so shipping is a
;;;; separate step that READS the rings: whatever is new since it last ran, oldest first
;;;; per core, one UDP datagram per record.  Network down, nothing is lost: the records wait
;;;; in the rings (and on the Pi the rings survive a reset, so a hung boot's last records
;;;; ship after the next one).  Records a ring overwrote before they could ship are
;;;; COUNTED, not skipped in silence.
;;;;
;;;; WHO RUNS IT.  The network domain, when it is idle: NET-IDLE-HOOK (net/ip.lisp; called
;;;; from NET-RX-IDLE and NET-ACTOR-MAIN's empty poll) is KMSG-SHIP-MAYBE here, at most
;;;; every 100 ms.  The TCP/IP stack is not thread-safe, so
;;;; shipping happens where the stack already runs -- under the SSH server and the desk
;;;; alike -- and not on a thread of its own.
;;;;
;;;; FORMAT: RFC 5424 syslog, facility local0, severity from the record's level:
;;;;   <PRI>1 - modus kmsg CORE TAG - b<BOOT> c<CORE> <SECONDS> <LEVEL> <TAG> <ARGS> <TEXT>
;;;; KMSG-SHIP-FORMAT is the seam for another format -- a signed Nostr event for a relay
;;;; such as beacon's UDP ingest, say -- installed with (setf fdefinition).
;;;;
;;;; STATE, in the log's own header (so it survives a reset with the rings):
;;;;   +0x14 destination IPv4 (0 = off)  +0x18 UDP port  +0x1C last step (counter >> 16)
;;;;   +0x20 records shipped  +0x24 records lost to overwriting  +0x28 send failures
;;;;   +0x48 + 16*core: the next index of that core's ring to ship

(defun %ks-get (off) (let ((b (%kmsg-base))) (if (zerop b) 0 (mem-ref (+ b off) :u32))))
(defun %ks-put (off v) (let ((b (%kmsg-base))) (unless (zerop b) (%kmsg-set (+ b off) v))))

(defun %ks-parse-ip (s)
  "\"10.0.0.1\" as the integer net/ip.lisp takes (10.0.0.1 = #x0A000001)."
  (let ((n 0) (start 0))
    (dotimes (k 4 n)
      (let ((end (or (position #\. s :start start) (length s))))
        (setq n (+ (* n 256) (parse-integer s :start start :end end))
              start (+ end 1))))))

(defun kmsg-ship-to (host &optional (port 514))
  "Ship the log to HOST's syslog (UDP PORT), from the next record not yet shipped; NIL
   stops.  Records from before this call ship too, oldest first (KMSG-SHIP-SKIP-OLD to
   start from now instead)."
  (if (null host)
      (%ks-put #x14 0)
      (progn (%ks-put #x18 port) (%ks-put #x14 (%ks-parse-ip host))))
  (kmsg-ship-status))

(defun kmsg-ship-skip-old ()
  "Mark everything already in the rings as shipped."
  (dotimes (c (%kmsg-ncores)) (%ks-put (+ #x48 (* 16 c)) (%ks-get (+ #x40 (* 16 c))))))

(declaim (overridable kmsg-ship-format))
(defun kmsg-ship-format (rec)
  "One record (a KMSG-RECORDS plist) as the bytes of one datagram: RFC 5424 syslog."
  (let* ((level (getf rec :level))
         (sev (case level (0 7) (1 6) (2 4) (t 3)))
         (hz (max 1 (cntfrq))))
    (multiple-value-bind (s frac) (floor (getf rec :ticks) hz)
      (let ((line (format nil "<~D>1 - modus kmsg ~D ~A - b~D c~D ~D.~6,'0D ~A ~A~{ ~D~}~@[ ~A~]"
                          (+ (* 16 8) sev) (getf rec :core) (string-trim " " (getf rec :tag))
                          (getf rec :boot) (getf rec :core) s (floor (* frac 1000000) hz)
                          (case level (0 "debug") (1 "info") (2 "WARN") (t "ERROR"))
                          (getf rec :tag) (getf rec :args)
                          (let ((tx (getf rec :text))) (and (plusp (length tx)) tx)))))
        (let ((b (make-array (length line) :element-type '(unsigned-byte 8))))
          (dotimes (i (length line) b) (setf (aref b i) (logand (char-code (char line i)) 255))))))))

(defun kmsg-ship-step (&optional (budget 32))
  "Ship up to BUDGET new records.  How many went."
  (let ((ip (%ks-get #x14)) (port (%ks-get #x18)) (n (%kmsg-ring-n)) (sent 0) (base (%kmsg-base)))
    (when (and (> ip 0) (%kmsg-ready-p base))
      (dotimes (c (%kmsg-ncores))
        (let* ((next (%ks-get (+ #x40 (* 16 c))))
               (sh (%ks-get (+ #x48 (* 16 c)))))
          (when (> (logand (- next sh) #xFFFFFFFF) n)                 ; overwritten unshipped
            (%ks-put #x24 (+ (%ks-get #x24) (- (logand (- next sh) #xFFFFFFFF) n)))
            (setq sh (logand (- next n) #xFFFFFFFF)))
          (loop
            (when (or (= sh next) (>= sent budget)) (return nil))
            (let ((rec (%kmsg-read (+ base #x1000 (* c (%kmsg-ring-bytes))
                                      (* (mod sh n) (%kmsg-rec-bytes))))))
              (when rec
                (let ((b (kmsg-ship-format rec)))
                  (if (eql (udp-send ip port 514 b (length b)) 0)
                      (%ks-put #x28 (+ (%ks-get #x28) 1))
                      (%ks-put #x20 (+ (%ks-get #x20) 1))))
                (setq sent (+ sent 1))))
            (setq sh (logand (+ sh 1) #xFFFFFFFF)))
          (%ks-put (+ #x48 (* 16 c)) sh))))
    sent))

(defun kmsg-ship-maybe ()
  "From the network domain's idle poll: a step at most every 100 ms, when shipping is on."
  (unless (zerop (%ks-get #x14))
    (let ((now (logand (ash (rdtsc) -16) #xFFFFFFFF)))
      (when (>= (logand (- now (%ks-get #x1C)) #xFFFFFFFF) (max 1 (floor (cntfrq) 655360)))
        (%ks-put #x1C now)
        (kmsg-ship-step)))))

(defun kmsg-ship-status ()
  (let ((ip (%ks-get #x14)))
    (list :to (if (zerop ip) nil
                  (format nil "~D.~D.~D.~D" (ldb (byte 8 24) ip) (ldb (byte 8 16) ip)
                          (ldb (byte 8 8) ip) (ldb (byte 8 0) ip)))
          :port (%ks-get #x18) :shipped (%ks-get #x20) :lost (%ks-get #x24)
          :send-failures (%ks-get #x28))))
