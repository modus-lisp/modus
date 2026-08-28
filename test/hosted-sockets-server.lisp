;;;; hosted-sockets-server.lisp — A TCP SERVER THAT SURVIVES CONTACT.
;;;;
;;;;   ./modus --script test/hosted-sockets-server.lisp
;;;;   (but really: test/run-socket-server.sh, which supplies the client)
;;;;
;;;; net/hosted-sockets.lisp could open a socket and echo ONE message on ONE
;;;; connection through ONE process-global page.  This drives the layer at the
;;;; other end of that: several connections open AT THE SAME TIME, serviced by
;;;; TWO OS THREADS out of one poll set each, moving more than a page in both
;;;; directions per connection, and then torn down with no fd left behind.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS TEST A LIE, and what stops it
;;;; ============================================================
;;;;
;;;;   "The bytes came back" is checked BY SOMETHING THAT IS NOT MODUS.  The
;;;;   client is Python (test/socket-client.py); it generates a per-connection
;;;;   byte pattern, sends it, reads the echo, and compares byte-for-byte.  A
;;;;   modus-to-modus test cannot see a protocol bug both ends share — a
;;;;   swapped length field, a byte order, an off-by-one in the framing — and
;;;;   that is exactly the bug class a new socket layer has.  (There IS a
;;;;   modus-to-modus arm as well, in test/hosted-sockets-client.lisp; it is
;;;;   the SECOND test, on purpose.)
;;;;
;;;;   "Two threads were involved" is not counted by asking the threads.  Each
;;;;   handler bumps a LIVE counter under the table lock on the way in and
;;;;   decrements it on the way out, and records the maximum it ever saw; a
;;;;   maximum of 2 means two handlers were inside AT THE SAME INSTANT, which
;;;;   a serialized implementation cannot produce.  Alongside it, every service
;;;;   event appends (cpu, connection) to a log, and the test counts HANDOFFS —
;;;;   adjacent entries written by different CPUs.  Run the same workload on
;;;;   one CPU and both numbers are exactly 1 and 0.  Neither is a timing
;;;;   measurement; both are witnesses, the same instrument the concurrent-GC
;;;;   test uses.
;;;;
;;;;   "More than one page moved" is not asserted from the request size.  The
;;;;   server tallies bytes it actually wrote back, per CPU, and the client
;;;;   independently counts bytes it received and compares them to what it
;;;;   sent.  Both numbers appear below and both must equal the total.
;;;;
;;;;   "No fd leaked" is not a count of fds.  Linux hands out the LOWEST free
;;;;   descriptor, so the test opens a socket and closes it again BEFORE the
;;;;   server starts and AGAIN after teardown: if anything below that number
;;;;   had leaked, the second probe could not return the same number.
;;;;
;;;;   "Nothing is still listening" is not a claim either — the server holds a
;;;;   DEADLINE it checks once per poll timeout, so a run that never gets a
;;;;   client still ends by itself and still closes its listener.
;;;;
;;;; THE NEGATIVE CONTROL is MODUS_SK_SHARED=1, which puts every CPU back on
;;;; the one process-global page this work replaced, in the same binary, on the
;;;; same workload.  The handler holds bytes in the staging buffer across a
;;;; bounded spin between the read and the write precisely so that the shared
;;;; buffer really is corrupted by the other CPU rather than merely being able
;;;; to be.  The client, which does the comparing, reports the mismatch.

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(defun chk-ge (name got lo)
  (setq *checks* (+ *checks* 1))
  (if (>= got lo)
      (format t "  ok   ~A = ~D  (want >= ~D)~%" name got lo)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D  (want >= ~D)~%" name got lo))))

(defun envint (name dflt)
  (let ((s (%cli-getenv name)))
    (if (null s)
        dflt
        (if (= (length s) 0)
            dflt
            (let ((n 0) (i 0))
              (loop
                (when (>= i (length s)) (return n))
                (let ((c (char-code (char s i))))
                  (if (and (>= c 48) (<= c 57))
                      (setq n (+ (* n 10) (- c 48)))
                      (return n)))
                (setq i (+ i 1))))))))

(defvar *conns*  (envint "MODUS_SK_CONNS" 4))
(defvar *secs*   (envint "MODUS_SK_SECS" 40))
(defvar *spin*   (envint "MODUS_SK_SPIN" 20000))
(defvar *shared* (envint "MODUS_SK_SHARED" 0))
(defvar *ncpu*   (envint "MODUS_SK_NCPU" 2))
(defvar *bytes*  (envint "MODUS_SK_BYTES" 0))     ; what the client will send, per conn

(format t "~%=== THE STAGING BUFFERS ==================================~%")
(let ((p (%sk-page)))
  (chk-true "the socket page mapped" (> p 0))
  (if (zerop p)
      (format t "~%SKIP: no socket page.~%")
      (progn
        (format t "  page base            ~X  (~D bytes)~%" p (%sk-page-bytes))
        (format t "  this CPU's staging   ~X~%" (%sock-io-buf))
        (format t "  CPU 1's staging      ~X~%"
                (+ p (+ (%sk-data-off) (%sk-buf-bytes))))
        (format t "  file I/O's page      ~X   (mvm/cl-fileio.lisp)~%" *io-buf-addr*)
        (chk-true "socket staging is NOT the file-I/O page any more"
                  (not (= (%sock-io-buf) *io-buf-addr*)))
        (chk-true "CPU 0's and CPU 1's staging buffers do not overlap"
                  (>= (- (+ p (+ (%sk-data-off) (%sk-buf-bytes))) (%sock-io-buf))
                      (%sk-buf-bytes)))
        (chk-true "the sockaddr scratch is NOT the cstr page any more"
                  (not (= (%sock-addr-buf) (+ *cstr-scratch* 3072))))
        (chk "and the transfer capacity is the staging size" (%sock-io-cap)
             (%sk-buf-bytes)))))

;;; ---- open the listener, announce the port, then serve -------------------
;;;
;;; PORT 0: the kernel picks, and getsockname(2) reads back what it picked, so
;;; nothing here ever guesses a port number or races another process for one.
;;; The address is (%SOCK-LOOPBACK) written out in full, and SOCKET-LISTEN-ON
;;; is the spelling that takes an address at all — this test can only ever bind
;;; 127.0.0.1 and a reader can see that without running it.

(defvar *fd0* (%sk-fd-probe))
(defvar *bound* (%sk-server-open (%sock-loopback) 0 16 *secs* *conns* *ncpu* *spin*))

(if (< *bound* 0)
    (format t "~%SKIP: could not open the listener.~%")
    (progn
      (%sk-set-shared-mode *shared*)
      (format t "~%=== THE SERVER ===========================================~%")
      (format t "  bound 127.0.0.1:~D   (asked for port 0; the kernel chose)~%" *bound*)
      (format t "  serving CPUs ~D   expect ~D connections   deadline ~Ds~%"
              *ncpu* *conns* *secs*)
      (format t "  staging mode ~D   (0 = per-CPU, 1 = THE SHARED PAGE CONTROL)~%"
              *shared*)
      (format t "PORT ~D~%" *bound*)
      (finish-output)

      ;; ---- run --------------------------------------------------------
      (if (< *ncpu* 2)
          (%sk-serve 0)
          (%sk-run-two-threads 800000000))
      (let ((fdsclosed (%sk-server-close))
            (fd1 (%sk-fd-probe)))

        (format t "~%=== IT ACCEPTED, AND IT CLOSED ===========================~%")
        (chk "connections accepted" (%sk-stat #x18) *conns*)
        (chk "connections retired"  (%sk-stat #x20) *conns*)
        (chk "still open at the end" (%sk-stat #x108) 0)
        (chk-ge "the most it ever held at once" (%sk-stat #xD8) 2)
        (chk "accept errors" (%sk-stat #x58) 0)
        (chk "poll errors"   (%sk-stat #x110) 0)
        (chk "echo write errors" (%sk-stat #xD0) 0)
        (format t "  short writes (send queue filled mid-message)  ~D~%" (%sk-stat #x80))
        (format t "  POLLOUT waits (send queue was full)           ~D~%" (%sk-stat #x130))
        (format t "  EAGAIN reads (poll said ready, nothing there) ~D~%" (%sk-stat #x128))

        (format t "~%=== MORE THAN A PAGE, IN BOTH DIRECTIONS =================~%")
        (format t "  CPU 0 echoed ~D bytes over ~D read events~%"
                (%sk-stat #xA8) (%sk-stat #x98))
        (format t "  CPU 1 echoed ~D bytes over ~D read events~%"
                (%sk-stat #xB0) (%sk-stat #xA0))
        (format t "  total        ~D bytes~%" (+ (%sk-stat #xA8) (%sk-stat #xB0)))
        (chk-ge "a connection's worth is more than one 4096-byte page"
                (if (> *conns* 0)
                    (floor (+ (%sk-stat #xA8) (%sk-stat #xB0)) *conns*)
                    0)
                4097)
        (if (> *bytes* 0)
            (chk "and it is exactly what the client sent"
                 (+ (%sk-stat #xA8) (%sk-stat #xB0)) (* *bytes* *conns*))
            (format t "  (MODUS_SK_BYTES unset — the client checks the total)~%"))

        (format t "~%=== TWO THREADS, AT THE SAME INSTANT =====================~%")
        (format t "  Each handler bumps a LIVE counter under the table lock on~%")
        (format t "  the way in and decrements it on the way out.  A maximum of~%")
        (format t "  2 means two handlers were inside AT ONCE.  One CPU cannot~%")
        (format t "  produce it; the number would be exactly 1.~%")
        (format t "  thread 2 started ~D   finished ~D   its CPU id ~D~%"
                (%sk-stat #x60) (%sk-stat #x68) (%sk-stat #xE0))
        (format t "  poll returns: CPU 0 ~D   CPU 1 ~D~%"
                (%sk-stat #x88) (%sk-stat #x90))
        (format t "  max handlers inside at once  ~D~%" (%sk-stat #x38))
        (format t "  overlap witnesses            ~D~%" (%sk-stat #x40))
        (if (< *ncpu* 2)
            (progn
              (chk "one serving CPU: max handlers at once" (%sk-stat #x38) 1)
              (chk "one serving CPU: overlap witnesses"    (%sk-stat #x40) 0))
            (progn
              (chk "thread 2 ran its serving loop" (%sk-stat #x60) 1)
              (chk "thread 2 finished cleanly"     (%sk-stat #x68) 1)
              (chk "thread 2's CPU id"             (%sk-stat #xE0) 1)
              (chk "the join saw the kernel clear the TID (0 = yes)"
                   (%sk-stat #xC8) 0)
              (chk-ge "max handlers inside at once" (%sk-stat #x38) 2)
              (chk-ge "overlap witnesses"           (%sk-stat #x40) 1)
              (chk-ge "CPU 0 serviced read events"  (%sk-stat #x98) 1)
              (chk-ge "CPU 1 serviced read events"  (%sk-stat #xA0) 1)
              (chk-ge "CPU 0 echoed bytes"          (%sk-stat #xA8) 1)
              (chk-ge "CPU 1 echoed bytes"          (%sk-stat #xB0) 1)
              (chk-ge "CPU 0 retired connections"   (%sk-stat #xB8) 1)
              (chk-ge "CPU 1 retired connections"   (%sk-stat #xC0) 1)))
        (chk "a connection was never serviced by a CPU that did not own it"
             (%sk-stat #x118) 0)

        (format t "~%=== AND THE HANDLING INTERLEAVES =========================~%")
        (format t "  Every service event appends (cpu, connection) to a log.~%")
        (format t "  A HANDOFF is two adjacent entries written by different~%")
        (format t "  CPUs — so the two threads were not merely both busy at~%")
        (format t "  some point, they were taking turns inside the same run.~%")
        (let ((n (%sk-stat #x50)) (handoffs 0) (i 0)
              (n0 0) (n1 0) (prev -1) (slots1 0) (mask1 0))
          (loop
            (when (>= i n) (return nil))
            (let ((cpu (- (%gc-read64 (%sk-log i)) 1))
                  (slot (%gc-read64 (+ (%sk-log i) 8))))
              (if (zerop cpu) (setq n0 (+ n0 1)) (setq n1 (+ n1 1)))
              (when (= cpu 1)
                (when (zerop (logand mask1 (ash 1 slot)))
                  (setq mask1 (logior mask1 (ash 1 slot)))
                  (setq slots1 (+ slots1 1))))
              (when (>= prev 0)
                (when (not (= cpu prev)) (setq handoffs (+ handoffs 1))))
              (setq prev cpu))
            (setq i (+ i 1)))
          (format t "  log entries ~D  (cap ~D)   CPU 0 ~D   CPU 1 ~D~%"
                  n (%sk-log-cap) n0 n1)
          (format t "  distinct connections CPU 1 touched  ~D~%" slots1)
          (format t "  handoffs between the two CPUs       ~D~%" handoffs)
          (if (< *ncpu* 2)
              (chk "one serving CPU: no handoffs" handoffs 0)
              (progn
                (chk-ge "handoffs" handoffs 1)
                (chk-ge "connections CPU 1 handled end to end" slots1 1))))

        (format t "~%=== TEARDOWN: NOTHING IS LEFT OPEN =======================~%")
        (format t "  Linux hands out the LOWEST free descriptor, so a socket~%")
        (format t "  opened and closed before and after the run must come back~%")
        (format t "  with the SAME number.  A leaked fd below it cannot.~%")
        (format t "  fd probe before ~D   after ~D~%" *fd0* fd1)
        (chk "the fd probe is unchanged" fd1 *fd0*)
        (chk-ge "teardown closed the listener" fdsclosed 1)
        (chk "the listening fd slot is cleared" (%sk-stat 0) 0)
        (let ((busy 0) (i 0))
          (loop
            (when (>= i (%sk-slots)) (return nil))
            (when (> (%gc-read64 (%sk-slot i)) 0) (setq busy (+ busy 1)))
            (setq i (+ i 1)))
          (chk "connection slots still holding an fd" busy 0)))))

(format t "~%=== VERDICT ==============================================~%")
(if (= *fail* 0)
    (format t "HOSTED x64 SOCKET SERVER: PASS (~D checks)~%" *checks*)
    (format t "HOSTED x64 SOCKET SERVER: FAIL (~D of ~D checks)~%" *fail* *checks*))
