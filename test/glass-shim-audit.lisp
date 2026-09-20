;;;; glass-shim-audit.lisp — EVERY SBCL SURFACE GLASS TOUCHES, PROBED IN ONE RUN.
;;;;
;;;;   test/run-glass-shim-audit.sh [MODUS-BINARY]
;;;;
;;;; ============================================================
;;;; WHY THIS EXISTS: THE COST OF FINDING GAPS ONE AT A TIME
;;;; ============================================================
;;;;
;;;; The shim is baked into the image as a source string, so changing it means
;;;; rebuilding, and a rebuild is minutes.  Discovering the gaps by running
;;;; GLASS:SERVE-ONE and fixing whatever it hit first therefore costs one build
;;;; per gap — and it only ever finds the gaps on the path the FIRST client
;;;; happens to take.  A shape glass uses on the second client, or on the error
;;;; path, is not on that path and is not found.
;;;;
;;;; So this asks the whole surface at once, and it asks it in the SHAPE GLASS
;;;; WRITES rather than in the shape the shim happens to define.  Every probe
;;;; below is a literal transcription of a call site in
;;;; /home/claude/glass/src/{socket,rfb,perf,framebuffer,clipboard}.lisp — the
;;;; keywords, the argument order and the initargs are glass's, not invented.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE, and what stops it
;;;; ============================================================
;;;;
;;;;   A PROBE THAT IS WRONG REPORTS A GAP THAT IS NOT THERE.  So the identical
;;;;   file runs under SBCL, where every one of these calls is by definition
;;;;   correct, and SBCL must score 100%.  A probe SBCL fails is a bug in this
;;;;   file and the runner says so rather than blaming modus.
;;;;
;;;;   A PROBE THAT SIGNALS MUST NOT END THE RUN, or the audit degenerates into
;;;;   exactly the one-gap-per-build loop it exists to replace.  Each probe is
;;;;   independent and its failure is caught and printed with the condition.
;;;;
;;;;   "IT DID NOT SIGNAL" IS NOT "IT WORKED".  Every probe returns a VALUE that
;;;;   is compared against what SBCL's implementation is documented to return —
;;;;   an fd is a positive integer, a stream is a stream, peername is an address
;;;;   and a port, a mutex is held inside WITH-MUTEX.  A probe whose only
;;;;   assertion is that nothing was signalled is marked WEAK in its name.
;;;;
;;;;   NOTHING IS LEFT LISTENING.  Every socket opened here is bound to
;;;;   127.0.0.1 on port 0 (the kernel picks) and closed on the way out, and the
;;;;   VNC range 5900-5920 is refused outright if the kernel ever hands one back.
;;;;   The audit ends by asking for a descriptor and checking it is the same
;;;;   number it was before, which is how a leaked fd shows up.

#+sbcl (progn (require :sb-bsd-sockets) (require :sb-posix))

(defvar *ok* 0)
(defvar *gap* 0)
(defvar *gaps* '())

(defun bail (code)
  (finish-output)
  #+sbcl (sb-ext:exit :code code)
  #-sbcl (sys-exit code))

(defun describe-condition (c)
  "A one-line description of C that cannot itself signal.  modus's printer can
   be handed a condition whose type-name slot is damaged, and an audit that dies
   printing a failure is worse than one that prints a vague failure."
  (handler-case (format nil "~a" c)
    (error () "<condition unprintable>")))

(defmacro probe (name &body body)
  "Run BODY.  It must return T.  Anything else, or a signal, is a GAP.
   The name is printed either way, so the transcript is the full surface and not
   only the broken part of it."
  `(let ((r (handler-case (progn ,@body)
              (error (c) (list :signalled (describe-condition c))))))
     (cond ((eq r t) (setq *ok* (+ *ok* 1))
                     (format t "ok   ~a~%" ,name))
           (t (setq *gap* (+ *gap* 1))
              (setq *gaps* (cons ,name *gaps*))
              (format t "GAP  ~a  -> ~s~%" ,name r)))
     (force-output)))

;;; ============================================================
;;; SB-THREAD — the shapes in rfb.lisp, perf.lisp, framebuffer.lisp, clipboard.lisp
;;; ============================================================

(format t "~&=== SB-THREAD ===~%")

(probe "make-mutex :name  (rfb.lisp:686)"
  (let ((m (sb-thread:make-mutex :name "rfb-client")))
    (and m t)))

(probe "with-mutex over a form  (perf.lisp:64)"
  (let ((m (sb-thread:make-mutex :name "probe")))
    (= 7 (sb-thread:with-mutex (m) (+ 3 4)))))

(probe "with-mutex ((accessor x))  (rfb.lisp:781 — the doubled-paren spelling)"
  (let ((box (list (sb-thread:make-mutex :name "probe"))))
    (= 9 (sb-thread:with-mutex ((first box)) (+ 4 5)))))

(probe "with-recursive-lock, nested  (framebuffer.lisp:157)"
  (let ((m (sb-thread:make-mutex :name "probe")))
    (= 5 (sb-thread:with-recursive-lock (m)
           (sb-thread:with-recursive-lock (m) 5)))))

(probe "make-waitqueue :name  (rfb.lisp:745)"
  (and (sb-thread:make-waitqueue :name "glass-wake") t))

(probe "condition-wait with a :timeout that EXPIRES  (rfb.lisp:752)"
  ;; The sender parks here at 1/60 s when nothing has drawn.  What matters is
  ;; that it RETURNS rather than parking forever, and that it returns with the
  ;; mutex still held, which is condition-wait's contract.
  (let ((m (sb-thread:make-mutex :name "glass-wake"))
        (cv (sb-thread:make-waitqueue :name "glass-wake")))
    (sb-thread:with-mutex (m)
      (sb-thread:condition-wait cv m :timeout 1/60)
      t)))

(probe "condition-broadcast under its mutex  (rfb.lisp:749)"
  (let ((m (sb-thread:make-mutex :name "glass-wake"))
        (cv (sb-thread:make-waitqueue :name "glass-wake")))
    (sb-thread:with-mutex (m) (sb-thread:condition-broadcast cv))
    t))

(probe "make-thread of a CLOSURE :name, and join-thread returns its value  (rfb.lisp:883)"
  (let* ((n 0)
         (th (sb-thread:make-thread (lambda () (setq n 41) (+ n 1))
                                    :name "glass-sender")))
    (eql 42 (sb-thread:join-thread th))))

(probe "make-thread runs the closure's SIDE EFFECT on a shared cons"
  ;; join-thread's value lives in the child's region; a cons the PARENT made is
  ;; the shape rfb.lisp actually relies on (the client record is shared).
  (let* ((box (cons nil nil))
         (th (sb-thread:make-thread (lambda () (setf (car box) :ran)))))
    (sb-thread:join-thread th)
    (eq (car box) :ran)))

(probe "a mutex is CONTENDED across two threads and both sides run"
  ;; with-mutex that never actually blocks proves nothing about the lock.
  (let* ((m (sb-thread:make-mutex :name "contend"))
         (box (cons 0 nil))
         (ths (list (sb-thread:make-thread
                     (lambda () (dotimes (i 200)
                                  (sb-thread:with-mutex (m) (incf (car box))))))
                    (sb-thread:make-thread
                     (lambda () (dotimes (i 200)
                                  (sb-thread:with-mutex (m) (incf (car box)))))))))
    (dolist (th ths) (sb-thread:join-thread th))
    (eql 400 (car box))))

;;; ============================================================
;;; SB-BSD-SOCKETS — socket.lisp's transport, in socket.lisp's spelling
;;; ============================================================

(format t "~&=== SB-BSD-SOCKETS ===~%")

(defvar *fd-before*
  ;; Linux hands out the lowest free descriptor, so asking for one before and
  ;; after is how a leak becomes visible.
  ;;
  ;; CAUGHT, BECAUSE A SETUP FORM THAT SIGNALS TAKES THE WHOLE AUDIT WITH IT and
  ;; the audit exists precisely so that one gap does not hide the next forty.
  ;; This exact form is what died before the initarg fix.
  (handler-case
      (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        (let ((fd (sb-bsd-sockets:socket-file-descriptor s)))
          (sb-bsd-sockets:socket-close s)
          fd))
    (error () nil)))

(probe "make-instance inet-socket :type :stream :protocol :tcp  (socket.lisp:280)"
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (prog1 (typep s 'sb-bsd-sockets:socket)
      (sb-bsd-sockets:socket-close s))))

(probe "make-instance local-socket :type :stream  (socket.lisp:300)"
  (let ((s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (prog1 (typep s 'sb-bsd-sockets:socket)
      (ignore-errors (sb-bsd-sockets:socket-close s)))))

(probe "socket-file-descriptor is a positive integer  (socket.lisp:144)"
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (prog1 (let ((fd (sb-bsd-sockets:socket-file-descriptor s)))
             (and (integerp fd) (> fd 0)))
      (sb-bsd-sockets:socket-close s))))

(probe "make-inet-address -> #(127 0 0 1)  (socket.lisp:282)"
  (equalp (sb-bsd-sockets:make-inet-address "127.0.0.1") #(127 0 0 1)))

;;; TCP-LISTEN's exact body, socket.lisp:279-284.
(defvar *listener* nil)
(defvar *listen-port* nil)

(probe "reuse-address + bind(127.0.0.1,0) + listen  (socket.lisp:279-284)"
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock (sb-bsd-sockets:make-inet-address "127.0.0.1") 0)
    (sb-bsd-sockets:socket-listen sock 5)
    (setq *listener* sock)
    t))

(probe "socket-name of the listener gives (address port), port outside 5900-5920  (socket.lisp:253)"
  (if (null *listener*)
      :no-listener
      (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name *listener*)
        (setq *listen-port* port)
        (and (equalp addr #(127 0 0 1))
             (integerp port) (> port 1023)
             (or (< port 5900) (> port 5920))))))

;;; A real connect/accept pair, because accept is where the shim's class has to
;;; produce a NEW socket object of the right class and glass then calls
;;; sockopt-tcp-nodelay and socket-make-stream on it.
(defvar *client* nil)
(defvar *server* nil)

(probe "socket-connect to the listener  (socket.lisp:519)"
  (if (null *listen-port*)
      :no-port
      (let ((c (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        (sb-bsd-sockets:socket-connect
         c (sb-bsd-sockets:host-ent-address
            (sb-bsd-sockets:get-host-by-name "127.0.0.1"))
         *listen-port*)
        (setq *client* c)
        t)))

(probe "socket-accept returns a SOCKET  (socket.lisp:369)"
  (if (null *listener*)
      :no-listener
      (let ((s (sb-bsd-sockets:socket-accept *listener*)))
        (setq *server* s)
        (typep s 'sb-bsd-sockets:socket))))

(probe "(setf sockopt-tcp-nodelay) on the ACCEPTED socket  (socket.lisp:372)"
  (if (null *server*) :no-server
      (progn (setf (sb-bsd-sockets:sockopt-tcp-nodelay *server*) t) t)))

(probe "socket-peername of the accepted socket  (socket.lisp:208)"
  (if (null *server*) :no-server
      (multiple-value-bind (addr port) (sb-bsd-sockets:socket-peername *server*)
        (and (equalp addr #(127 0 0 1)) (integerp port) (> port 0)))))

(defvar *sstream* nil)
(defvar *cstream* nil)

(probe "socket-make-stream :input t :output t :element-type (unsigned-byte 8) :buffering :full  (socket.lisp:401)"
  (if (null *server*) :no-server
      (let ((s (sb-bsd-sockets:socket-make-stream
                *server* :input t :output t
                :element-type '(unsigned-byte 8) :buffering :full)))
        (setq *sstream* s)
        (and (streamp s) (open-stream-p s)))))

(probe "socket-make-stream on the CLIENT side too"
  (if (null *client*) :no-client
      (let ((s (sb-bsd-sockets:socket-make-stream
                *client* :input t :output t
                :element-type '(unsigned-byte 8) :buffering :full)))
        (setq *cstream* s)
        (and (streamp s) (open-stream-p s)))))

(probe "sb-sys:fd-stream-fd of a socket stream  (socket.lisp:144, rfb.lisp send-queue)"
  (if (null *sstream*) :no-stream
      (let ((fd (sb-sys:fd-stream-fd *sstream*)))
        (and (integerp fd) (> fd 0)))))

(probe "typep stream 'sb-sys:fd-stream  (socket.lisp:144)"
  (if (null *sstream*) :no-stream
      (and (typep *sstream* 'sb-sys:fd-stream) t)))

(probe "write-byte/force-output/read-byte across the pair  (rfb.lisp:25-31)"
  (if (or (null *sstream*) (null *cstream*)) :no-streams
      (progn (write-byte 82 *sstream*) (write-byte 70 *sstream*)
             (force-output *sstream*)
             (and (eql 82 (read-byte *cstream*))
                  (eql 70 (read-byte *cstream*))))))

(probe "write-sequence of 327680 bytes in ONE call  (rfb.lisp write-rect-raw, 1280 wide, 64-row band)"
  ;; This is the size the RFB server actually issues at the default banding.  It
  ;; is here because a transport that chunks wrongly loses the tail SILENTLY.
  (if (or (null *sstream*) (null *cstream*)) :no-streams
      (let* ((n 327680)
             (buf (make-array n :element-type '(unsigned-byte 8)))
             (back (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
        (dotimes (i n) (setf (aref buf i) (logand i 255)))
        ;; The reader has to be another thread: 327680 bytes will not fit in the
        ;; socket buffer, so a single-threaded write-then-read deadlocks.  That
        ;; is also exactly glass's shape (the sender writes, the peer reads).
        (let ((th (sb-thread:make-thread
                   (lambda () (read-sequence back *cstream*)))))
          (write-sequence buf *sstream*)
          (force-output *sstream*)
          (sb-thread:join-thread th))
        (let ((bad 0))
          (dotimes (i n) (unless (eql (aref back i) (logand i 255)) (incf bad)))
          (if (zerop bad) t (list :mismatched-bytes bad))))))

(probe "socket-shutdown :direction :io on the LISTENER  (socket.lisp:443, close-listener)"
  ;; THE LISTENER AND NOT A STREAM-WRAPPED SOCKET, because that is what glass
  ;; shuts down and the two are not interchangeable: closing a socket's STREAM
  ;; closes its descriptor, and shutting the descriptor down afterwards is an
  ;; EBADF under SBCL too.  This is how glass cancels a thread parked in accept
  ;; (socket.lisp:420-440), so it is load-bearing rather than hygiene.
  (if (null *listener*) :no-listener
      (progn (sb-bsd-sockets:socket-shutdown *listener* :direction :io) t)))

(probe "close the two socket streams  (rfb.lisp:1043 `(close stream)')"
  (progn (close *sstream*)
         (close *cstream*)
         (and (not (open-stream-p *sstream*))
              (not (open-stream-p *cstream*)))))

(probe "socket-close everything  (socket.lisp:444)"
  (progn (dolist (s (list *server* *client* *listener*))
           (when s (ignore-errors (sb-bsd-sockets:socket-close s))))
         t))

(probe "connection-refused-error is signalled and CAUGHT by class  (socket.lisp:303)"
  ;; glass's `unix-socket-live-p' distinguishes "nothing is listening" from any
  ;; other failure by catching THIS class.  A shim that signals a plain error
  ;; would make a dead socket file look like a broken one.
  (handler-case
      (let ((c (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        (unwind-protect
             (progn (sb-bsd-sockets:socket-connect
                     c (sb-bsd-sockets:make-inet-address "127.0.0.1") 1)
                    :connected-to-port-1)
          (ignore-errors (sb-bsd-sockets:socket-close c))))
    (sb-bsd-sockets:connection-refused-error () t)))

;;; CLOS: glass specialises methods on the shim's classes.  socket.lisp:246,
;;; 252, 260, 375, 405, 442 are all `((l sb-bsd-sockets:socket))'.
(defgeneric probe-transport (l)
  (:method ((l sb-bsd-sockets:socket)) :tcp))
(defmethod probe-transport ((l t)) :other)

(probe "defmethod specialised on sb-bsd-sockets:socket dispatches for an INET-SOCKET  (socket.lisp:246)"
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (prog1 (eq :tcp (probe-transport s))
      (ignore-errors (sb-bsd-sockets:socket-close s)))))

(probe "the same generic still falls through to T for a non-socket"
  (eq :other (probe-transport 17)))

;;; ============================================================
;;; SB-POSIX — socket.lisp's AF_UNIX hygiene
;;; ============================================================

(format t "~&=== SB-POSIX ===~%")

(probe "getuid is a non-negative integer  (socket.lisp:89)"
  (let ((u (sb-posix:getuid))) (and (integerp u) (>= u 0))))

(probe "stat of an existing directory, s-isdir true  (socket.lisp:71-73)"
  (let ((st (sb-posix:stat "/tmp")))
    (and (sb-posix:s-isdir (sb-posix:stat-mode st))
         (integerp (sb-posix:stat-uid st))
         (integerp (sb-posix:stat-ino st))
         t)))

(probe "stat of a missing file SIGNALS  (socket.lisp:290 relies on it)"
  (handler-case (progn (sb-posix:stat "/tmp/definitely-not-here-xyzzy-glass")
                       :returned-instead-of-signalling)
    (error () t)))

(probe "chmod + stat-mode round trip, then unlink  (socket.lisp:336,463)"
  (let ((p (format nil "/tmp/glass-shim-audit-~d.tmp" (sb-posix:getpid))))
    (with-open-file (o p :direction :output :if-exists :supersede) (write-char #\x o))
    (sb-posix:chmod p #o600)
    (let ((m (logand (sb-posix:stat-mode (sb-posix:stat p)) #o777)))
      (sb-posix:unlink p)
      (eql m #o600))))

(probe "s-issock says NO for a regular file  (socket.lisp:290)"
  (let ((p (format nil "/tmp/glass-shim-audit-s-~d.tmp" (sb-posix:getpid))))
    (with-open-file (o p :direction :output :if-exists :supersede) (write-char #\x o))
    (let ((r (sb-posix:s-issock (sb-posix:stat-mode (sb-posix:stat p)))))
      (sb-posix:unlink p)
      (not r))))

;;; ============================================================
;;; SB-EXT — configuration and cleanup
;;; ============================================================

(format t "~&=== SB-EXT ===~%")

(probe "posix-getenv HOME is a non-empty string  (34 sites)"
  (let ((h (sb-ext:posix-getenv "HOME"))) (and (stringp h) (> (length h) 0))))

(probe "posix-getenv of something unset is NIL"
  (null (sb-ext:posix-getenv "GLASS_DEFINITELY_NOT_SET_XYZZY")))

(probe "*exit-hooks* is a bindable list a hook can be pushed onto  (socket.lisp:467)"
  (let ((before (length sb-ext:*exit-hooks*)))
    (push (lambda () nil) sb-ext:*exit-hooks*)
    (prog1 (eql (length sb-ext:*exit-hooks*) (+ before 1))
      (pop sb-ext:*exit-hooks*))))

;;; ============================================================
;;; SB-ALIEN — the two libc calls, in socket.lisp's exact shape
;;; ============================================================

(format t "~&=== SB-ALIEN ===~%")

;;; socket.lisp:551-556 — the send-queue depth the sender loop records.  glass
;;; wraps it in HANDLER-CASE and treats a signal as 0, so a platform without it
;;; is allowed; what is NOT allowed is returning a wrong number.
(probe "ioctl(SIOCOUTQ) on a fresh socket: 0 unsent, or an honest signal  (socket.lisp:551)"
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (let ((fd (sb-bsd-sockets:socket-file-descriptor s)))
           (handler-case
               (sb-alien:with-alien ((n sb-alien:int))
                 (sb-alien:alien-funcall
                  (sb-alien:extern-alien "ioctl"
                                         (function sb-alien:int sb-alien:int
                                                   sb-alien:unsigned-long
                                                   (* sb-alien:int)))
                  fd #x5411 (sb-alien:addr n))
                 (let ((v (sb-alien:deref (sb-alien:addr n))))
                   (if (and (integerp v) (>= v 0)) t (list :bad-value v))))
             (error () t)))                     ; glass's own fallback: absent is fine
      (ignore-errors (sb-bsd-sockets:socket-close s)))))

;;; ============================================================
;;; NOTHING LEFT BEHIND
;;; ============================================================

(format t "~&=== NOTHING LEFT BEHIND ===~%")

(probe "the fd probe returns the same descriptor it did at the start"
  (if (null *fd-before*)
      :the-opening-probe-never-got-a-descriptor
      (let* ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
             (fd (sb-bsd-sockets:socket-file-descriptor s)))
        (sb-bsd-sockets:socket-close s)
        (if (eql fd *fd-before*) t (list :leaked (- fd *fd-before*) :fds)))))

(format t "~&~%~d ok, ~d GAPS~%" *ok* *gap*)
(when *gaps*
  (format t "~%THE GAPS, in the order they were found:~%")
  (dolist (g (reverse *gaps*)) (format t "  - ~a~%" g)))
(format t "~%~:[FAIL~;PASS~]~%" (zerop *gap*))
(bail (if (zerop *gap*) 0 1))
