;;;; net-service.lisp - the network's CORE-0 side, with the stack itself on the I/O core.
;;;;
;;;; The stack -- ARP, ICMP, IP, TCP, UDP -- runs on core 1 inside the USB engine's loop
;;;; (net/io-net.lisp), and every connection's bytes live in a static block any core reads and
;;;; writes.  Core 0 is the Lisp system, and a busy evaluation there holds up Lisp only.  What
;;;; is left here is what belongs to Lisp:
;;;;
;;;;   INETD       (NET-SERVICE-LISTEN PORT SPAWN): an accept thread on core 0 blocks on the
;;;;               port and calls SPAWN with (HANDLE SLOT) for each connection.  SPAWN starts
;;;;               the handler -- a thread anywhere, cores 2..3 included -- which reads and
;;;;               writes the connection itself (ION-READ / ION-WRITE / ION-CLOSE: no relay, no
;;;;               copy through the service).  SLOT names the handler's reply channel.
;;;;   EVALUATION  (NET-SERVICE-EVAL SLOT STRING): STRING is read and evaluated on the MAIN
;;;;               thread, in turn with the serial REPL, while it waits at the console for a key
;;;;               (%BT-MAIN-JOBS); the reply, (:RESULT BYTES), arrives on SLOT's channel.  Not on
;;;;               a thread of its own: evaluation off the main thread loses SETQs under
;;;;               DOTIMES/DOLIST and can wedge core 0 (measured on the Zero: (let ((s 0))
;;;;               (dotimes (i 3) (setq s (+ s 1))) s) is 3 on main and 0 on a core-0 thread), and
;;;;               kiln relies on it: `(kiln-zero-main)' sent over SSH takes the main thread over.
;;;;   THE WATCHDOG  a core-0 thread that asks to run every 10 ms and logs (kmsg PAUS) every
;;;;               time it was kept waiting past *NS-PAUSE-MS*: a main thread that holds core 0
;;;;               is a log line with a size.  It no longer holds up the network.
;;;;   LOG SHIPPING  KMSG-SHIP-STEP once a second (UDP, built on this core, sent by the engine).
;;;;
;;;; Loaded at runtime after net/tcp-conns.lisp (evaluated by SSH-BOOT on threaded Pi builds).
;;;;   (modus-int::net-service-start)    (modus-int::net-service-status)

(in-package :modus-int)

(defvar *ns-up* nil)
(defvar *ns-watchdog* nil)
(defvar *ns-shipper* nil)
(defvar *ns-stop* nil)
(defvar *ns-pause-ms* 100 "A wait longer than this is logged as a pause.")
(defvar *ns-stats* (make-array 16 :initial-element 0))
;; 10 accepted 11 evaluations 12 refused (no free slot) 13 handler errors
(defvar *ns-wd* (make-array 8 :initial-element 0))

(defmacro %ns-incf (vec i &optional (n 1)) `(setf (svref ,vec ,i) (+ (svref ,vec ,i) ,n)))

(defun %ns-engine-p () (and (fboundp 'usbe-running-p) (usbe-running-p)))

;;; ---- channels ---------------------------------------------------------------------------
;; MODUS:MAKE-CHANNEL and friends, found when the service starts: the MODUS package exists
;; only in the image (its surface is installed at boot), and the build host reads this file.
(defun %ns-nslots () 8)
(defvar *ns-chan-send* nil)
(defvar *ns-chan-try* nil)
(defvar *ns-chan-recv* nil)
(defvar *ns-main-jobs* nil "(SLOT STRING) evaluations for the main thread, oldest first.")
(defvar *ns-main-busy* nil)
(defvar *ns-slot-ch* nil "Each slot's reply channel, made once.")
(defvar *ns-slots* nil "Each slot: 0 free, 1 a handler holds it (a fixnum: handlers on any core write it).")
(defvar *ns-listeners* nil "Alist port -> accept thread.")

(defun %ns-modus-fn (name) (symbol-function (find-symbol name "MODUS")))
(defun %ns-send (ch msg) (funcall *ns-chan-send* ch msg))
(defun %ns-try (ch) (funcall *ns-chan-try* ch))

(defun %ns-channels-init ()
  (unless *ns-main-jobs*
    (setq *ns-chan-send* (%ns-modus-fn "CHANNEL-SEND")
          *ns-chan-try* (%ns-modus-fn "CHANNEL-TRY-RECEIVE")
          *ns-chan-recv* (%ns-modus-fn "CHANNEL-RECEIVE"))
    (setq *ns-main-jobs* (funcall (%ns-modus-fn "MAKE-CHANNEL") :capacity 262144 :name "to the main thread")
          *ns-slot-ch* (make-array (%ns-nslots))
          *ns-slots* (make-array (%ns-nslots) :initial-element 0))
    (dotimes (i (%ns-nslots))
      (setf (svref *ns-slot-ch* i) (funcall (%ns-modus-fn "MAKE-CHANNEL") :capacity 262144 :name "to a handler")))))

(defun net-service-slot-channel (slot) (svref *ns-slot-ch* slot))
(defun net-service-slot-done (slot) (setf (svref *ns-slots* slot) 0))

;;; ---- evaluation, on the main thread -----------------------------------------------------
(defun net-service-eval (slot text)
  "Evaluate TEXT on the main thread; (:RESULT BYTES) arrives on SLOT's channel."
  (%ns-incf *ns-stats* 11)
  (%ns-send *ns-main-jobs* (list slot text)))

(defun %ns-eval-bytes (s)
  "The reply to evaluating S: what it printed, then `= VALUE'."
  (let* ((out nil)
         (result (handler-case
                     (let ((so (make-string-output-stream)))
                       (let ((r (let ((*standard-output* so)) (eval (read-from-string s)))))
                         (setq out (get-output-stream-string so))
                         r))
                   (error (c) (list 'error c))))
         (rs (handler-case (prin1-to-string result) (error () "#<unprintable>")))
         (text (concatenate 'string (or out "") "= " rs (string #\Newline)))
         (v (make-array (length text) :element-type '(unsigned-byte 8))))
    (dotimes (i (length text) v) (setf (aref v i) (logand (char-code (char text i)) 255)))))

;;; The main thread's side: the console's wait (net/bare-threads.lisp %CONSOLE-IDLE) runs one
;;; job at a time, never nested -- an evaluation that itself waits at the console does not
;;; start the next one.
(defun %bt-main-jobs ()
  (when (and *ns-main-jobs* (not *ns-main-busy*) (zerop (percpu-ref 16)))
    (let ((m (%ns-try *ns-main-jobs*)))
      (when m
        (setq *ns-main-busy* t)
        (unwind-protect
             (%ns-send (svref *ns-slot-ch* (car m)) (list :result (%ns-eval-bytes (cadr m))))
          (setq *ns-main-busy* nil))
        t))))

;;; ---- inetd ------------------------------------------------------------------------------
(defun %ns-free-slot ()
  (dotimes (i (%ns-nslots) nil) (when (zerop (svref *ns-slots* i)) (return i))))

(defun %ns-accept-main (port k spawn)
  (loop
    (when *ns-stop* (return :stopped))
    (let ((h (ion-accept k :timeout-ms 1000)))
      (when h
        (%ns-incf *ns-stats* 10)
        (let ((slot (%ns-free-slot)))
          (if (null slot)
              (progn (%ns-incf *ns-stats* 12) (ion-abort h))
              (let ((ch (svref *ns-slot-ch* slot)))
                (loop (unless (%ns-try ch) (return)))          ; a reused slot
                (setf (svref *ns-slots* slot) 1)
                (unless (handler-case (funcall spawn h slot) (error () nil))
                  (setf (svref *ns-slots* slot) 0)
                  (ion-abort h)))))))))

(defun net-service-listen (port spawn)
  "Serve PORT: each connection goes to the handler SPAWN starts, called with (HANDLE SLOT) on
   the accept thread; it returns the handler's thread, or NIL to refuse."
  (%ns-channels-init)
  (unless (assoc port *ns-listeners*)
    (let* ((k (ion-listen port))
           (th (sb-thread:make-thread (lambda () (%ns-accept-main port k spawn)) :name "accept")))
      (push (cons port th) *ns-listeners*)))
  port)

;;; ---- the watchdog -----------------------------------------------------------------------
;;;
;;; *NS-WD*: 0 checks, 1 pauses, 2 longest (ms), 3 last pause (ms), 4 total paused (ms).
(defun %ns-watchdog-main ()
  (let ((tick (%bt-ms-ticks 10)) (ms (max 1 (%bt-ms-ticks 1))) (wd *ns-wd*))
    (loop
      (when *ns-stop* (return :stopped))
      (let ((dl (+ (%bt-ticks) tick)) (gc (%gc-epoch)))
        (%bt-block 0 0 dl)
        (let ((late (floor (- (%bt-ticks) dl) ms)))
          (%ns-incf wd 0)
          (when (> late (svref wd 2)) (setf (svref wd 2) late))
          (when (>= late *ns-pause-ms*)
            (%ns-incf wd 1) (%ns-incf wd 4 late) (setf (svref wd 3) late)
            (kmsg-event 2 "PAUS" late (logand (- (%gc-epoch) gc) #xFFFF) 0)))))))

;;; ---- log shipping, a client like any other ----------------------------------------------
(defun %ns-shipper-main ()
  (loop
    (when *ns-stop* (return :stopped))
    (when (fboundp 'kmsg-ship-step) (kmsg-ship-step))
    (%ns-wait 0 0 1000)))

;;; ---- control ----------------------------------------------------------------------------
(defun net-service-start (&key (pause-ms *ns-pause-ms*) (ship t))
  "Give the network to the I/O core and start core 0's side.  :STARTED, :ALREADY or why not."
  (cond
    (*ns-up* :already)
    ((not (%ns-engine-p)) :no-engine)
    (t
     (let ((r (ion-start)))
       (unless (member r '(:started :already)) (return-from net-service-start r)))
     (%ns-channels-init)
     (setq *ns-pause-ms* pause-ms *ns-stop* nil)
     (fill *ns-stats* 0) (fill *ns-wd* 0)
     (setq *ns-watchdog* (sb-thread:make-thread #'%ns-watchdog-main :name "core-0 watchdog")
           *ns-shipper* (and ship (sb-thread:make-thread #'%ns-shipper-main :name "kmsg shipper"))
           *ns-up* t)
     (kmsg-event 1 "NETS" 1 pause-ms 0)
     :started)))

(defun net-service-status ()
  (let ((s *ns-stats*) (wd *ns-wd*))
    (list :running *ns-up* :io-net (ion-status)
          :accepted (svref s 10) :refused (svref s 12) :evaluations (svref s 11)
          :handlers (if *ns-slots* (count 1 *ns-slots*) 0)
          :listening (mapcar #'car *ns-listeners*)
          :watchdog (list :checks (svref wd 0) :pauses (svref wd 1) :longest-ms (svref wd 2)
                          :last-ms (svref wd 3) :total-ms (svref wd 4) :threshold-ms *ns-pause-ms*))))
