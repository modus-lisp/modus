;;;; hosted-sockets-client.lisp — THE MODUS-TO-MODUS ARM.
;;;;
;;;;   MODUS_SK_PORT=<port> ./modus --script test/hosted-sockets-client.lisp
;;;;   (but really: test/run-socket-modus.sh, which starts the server)
;;;;
;;;; test/hosted-sockets-server.lisp is checked by a Python client, because a
;;;; modus-to-modus test cannot see a protocol bug both ends share.  This is
;;;; the other half: the same server, driven by MODUS, which is what a modus
;;;; program that talks to a modus service will actually do.  It is the SECOND
;;;; test on purpose — if this one passes and the Python one does not, believe
;;;; the Python one.
;;;;
;;;; IT USES THE SAME BYTE PATTERN AS test/socket-client.py, deliberately:
;;;; (i*31 + k*97 + 11) mod 256 for byte i of connection k.  Two independent
;;;; implementations of one pattern is what makes "the bytes were right" mean
;;;; something; if only modus knew the pattern, modus agreeing with itself
;;;; would prove nothing.
;;;;
;;;; TWO PHASES, testing two different things.
;;;;
;;;;   A. N CONNECTIONS OPEN AT ONCE, driven round-robin: open every fd first,
;;;;      then walk them sending a chunk and reading its echo back.  All N are
;;;;      live on the server simultaneously, so the server's connection table,
;;;;      its poll set and (when it is running two threads) its second CPU are
;;;;      all exercised from a modus client.  The chunk is small enough that a
;;;;      send cannot fill the socket buffers, so a single-threaded client
;;;;      cannot deadlock against an echo server — which is a real hazard and
;;;;      the reason the Python client uses a second thread instead.
;;;;
;;;;   B. ONE TRANSFER LARGER THAN THE STAGING BUFFER, in a SINGLE
;;;;      SOCKET-SEND-FROM call.  This is the thing the old socket layer could
;;;;      not do at all: it copied LEN bytes into a 4096-byte page and issued
;;;;      one write(2), so anything past 4096 was written out of somebody
;;;;      else's memory.  96 KB against a 64 KB staging buffer means the send
;;;;      loop must chunk at least twice and the receive side must loop, and
;;;;      every byte is checked.

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

(defvar *port*   (envint "MODUS_SK_PORT" 0))
(defvar *nconn*  (envint "MODUS_SK_CONNS" 4))
(defvar *rounds* (envint "MODUS_SK_ROUNDS" 16))
(defvar *chunk*  (envint "MODUS_SK_CCHUNK" 8192))
(defvar *big*    (envint "MODUS_SK_BIG" 98304))

;;; The pattern test/socket-client.py generates, reimplemented here.
(defun pat (k i) (logand (+ (+ (* i 31) (* k 97)) 11) 255))

(defun fill-pat (arr k base n)
  (let ((i 0))
    (loop
      (when (>= i n) (return nil))
      (aset arr i (pat k (+ base i)))
      (setq i (+ i 1)))))

;;; Index of the first byte that is not what it should be, or -1.
(defun first-bad (arr k base n)
  (let ((i 0) (bad -1))
    (loop
      (when (>= i n) (return nil))
      (when (not (= (aref arr i) (pat k (+ base i)))) (setq bad i) (return nil))
      (setq i (+ i 1)))
    bad))

(defvar *senderr* 0)
(defvar *recverr* 0)
(defvar *mismatch* 0)
(defvar *firstbad* -1)
(defvar *sent* 0)
(defvar *got* 0)

(format t "~%=== A MODUS CLIENT, AGAINST THE MODUS SERVER =============~%")
(if (< *port* 1)
    (format t "~%SKIP: MODUS_SK_PORT is not set.~%")
    (progn
      (format t "  127.0.0.1:~D   ~D connections open at once~%" *port* *nconn*)
      (format t "  ~D rounds x ~D bytes each, round-robin~%" *rounds* *chunk*)

      ;; ---- PHASE A: N connections, all open, driven round-robin ---------
      (let ((fds (make-array *nconn*))
            (sb (make-array *chunk*))
            (rb (make-array *chunk*))
            (opened 0) (k 0) (r 0))
        (loop
          (when (>= k *nconn*) (return nil))
          (let ((fd (socket-connect (%sock-loopback) *port* t)))
            (aset fds k fd)
            (when (> fd 0) (setq opened (+ opened 1))))
          (setq k (+ k 1)))
        (chk "connections opened" opened *nconn*)

        ;; A ROUND IS A SEND PASS OVER EVERY CONNECTION AND THEN A RECEIVE
        ;; PASS, not a send/receive per connection.  That matters: with
        ;; send-then-receive per connection only one connection is ever
        ;; readable at the server, so a single-threaded client would make even
        ;; a two-thread server look serialized, and the server's overlap
        ;; witnesses would read zero for a reason that is the CLIENT's.  With
        ;; the passes split, all N connections are readable at once.
        ;; NCONN*CHUNK bytes are outstanding at the turn, which is far inside
        ;; the socket buffers, so this still cannot deadlock against an echo.
        (loop
          (when (>= r *rounds*) (return nil))
          (setq k 0)
          (loop
            (when (>= k *nconn*) (return nil))
            (let ((fd (aref fds k)) (base (* r *chunk*)))
              (when (> fd 0)
                (fill-pat sb k base *chunk*)
                (let ((w (socket-send-from fd sb 0 *chunk*)))
                  (if (= w *chunk*)
                      (setq *sent* (+ *sent* w))
                      (setq *senderr* (+ *senderr* 1))))))
            (setq k (+ k 1)))
          (setq k 0)
          (loop
            (when (>= k *nconn*) (return nil))
            (let ((fd (aref fds k)) (base (* r *chunk*)))
              (when (> fd 0)
                (let ((g (socket-recv-fully fd rb 0 *chunk*)))
                  (if (= g *chunk*)
                      (setq *got* (+ *got* g))
                      (setq *recverr* (+ *recverr* 1)))
                  (let ((b (first-bad rb k base *chunk*)))
                    (when (>= b 0)
                      (setq *mismatch* (+ *mismatch* 1))
                      (when (< *firstbad* 0) (setq *firstbad* (+ base b))))))))
            (setq k (+ k 1)))
          (setq r (+ r 1)))

        ;; Half-close so the server sees EOF and retires the connection, then
        ;; close.  A client that just closes leaves the server to notice a
        ;; reset; shutdown(SHUT_WR) is the orderly version and is what glass's
        ;; close-listener relies on too.
        (setq k 0)
        (loop
          (when (>= k *nconn*) (return nil))
          (let ((fd (aref fds k)))
            (when (> fd 0)
              (syscall3 48 fd 1 0)            ; shutdown(fd, SHUT_WR)
              (socket-close fd)))
          (setq k (+ k 1))))

      (format t "~%=== EVERY BYTE CAME BACK, AND CAME BACK RIGHT ============~%")
      (format t "  sent ~D bytes   received ~D bytes~%" *sent* *got*)
      (chk "short sends"    *senderr* 0)
      (chk "short receives" *recverr* 0)
      (chk "chunks whose contents were wrong" *mismatch* 0)
      (chk "bytes received" *got* (* (* *nconn* *rounds*) *chunk*))
      (chk "and it equals what was sent" *got* *sent*)
      (when (>= *firstbad* 0)
        (format t "  first wrong byte at offset ~D~%" *firstbad*))

      ;; ---- PHASE B: one transfer bigger than the staging buffer ---------
      (format t "~%=== ONE SEND LARGER THAN THE STAGING BUFFER ==============~%")
      (format t "  ~D bytes in a SINGLE socket-send-from, against a ~D-byte~%"
              *big* (%sock-io-cap))
      (format t "  staging buffer.  The old layer wrote LEN bytes out of a~%")
      (format t "  4096-byte page and issued one write(2); anything past 4096~%")
      (format t "  was somebody else's memory.~%")
      (let ((fd (socket-connect (%sock-loopback) *port* t)))
        (chk-true "connected" (> fd 0))
        (when (> fd 0)
          (let ((sb (make-array *big*)) (rb (make-array *big*)))
            (fill-pat sb 0 0 *big*)
            (let ((w (socket-send-from fd sb 0 *big*)))
              (chk "bytes the single send reports" w *big*))
            (let ((g (socket-recv-fully fd rb 0 *big*)))
              (chk "bytes read back" g *big*)
              (chk "first wrong byte (-1 = none)" (first-bad rb 0 0 *big*) -1)))
          (syscall3 48 fd 1 0)
          (socket-close fd)))))

(format t "~%=== VERDICT ==============================================~%")
(if (= *fail* 0)
    (format t "MODUS-TO-MODUS SOCKETS: PASS (~D checks)~%" *checks*)
    (format t "MODUS-TO-MODUS SOCKETS: FAIL (~D of ~D checks)~%" *fail* *checks*))
