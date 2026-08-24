;;;; glass-serve.lisp — GLASS'S OWN RFB SERVER, RUNNING ON MODUS, SERVING A REAL CLIENT.
;;;;
;;;;   test/run-glass-serve.sh [MODUS-BINARY] [CLIENTS]
;;;;
;;;; ============================================================
;;;; WHAT THIS IS, AND WHAT IT IS NOT
;;;; ============================================================
;;;;
;;;; It is GLASS:SERVE — glass/src/rfb.lisp, unmodified, loaded from the glass
;;;; tree where it sits — accepting connections on modus and answering them with
;;;; glass's handshake, glass's pixel format, glass's Raw encoder and glass's
;;;; banding, over modus's sockets, with glass's per-client reader thread and
;;;; per-client SENDER thread both running on modus's threads.
;;;;
;;;; It is NOT test/rfb-static.lisp.  That file is a minimal RFB server written
;;;; here, against modus's own socket layer, because glass's would not load.  It
;;;; stays, and it stays passing, because it answers a different question (can
;;;; modus's transport carry RFB at all) and because a rung is not removed for
;;;; having been climbed.  THIS file answers the campaign's question: does the
;;;; actual compositor's actual server run here.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE, and what stops it
;;;; ============================================================
;;;;
;;;;   MODUS DOES NOT GRADE ITSELF.  The client is Python (test/glass-rfb-client.py)
;;;;   and it decides.  It does the handshake, checks every field of ServerInit,
;;;;   reassembles however many rectangles glass chose to band the screen into,
;;;;   and compares the pixels against an image IT GENERATES from the rule below
;;;;   — it is never handed the answer.  A protocol bug both ends share (a byte
;;;;   order, a field width, an off-by-one) is invisible to a Lisp-on-both-sides
;;;;   test and is exactly the bug class a wire protocol has.
;;;;
;;;;   THE PIXELS ARE NOT FLAT AND NOT SYMMETRIC.  A one-colour framebuffer
;;;;   passes a byte-order bug, a stride bug and a width/height swap at once.
;;;;
;;;;   THE SCREEN IS TALLER THAN ONE BAND.  glass caps a rectangle at
;;;;   *MAX-BAND-ROWS* = 64 rows, so 96 rows is TWO rectangles in one update and
;;;;   the client has to put them back in the right places.  A 64-row screen
;;;;   would have tested one rect and called it the server.
;;;;
;;;;   THE PORT IS NOT CHOSEN, AND NOT BY US EITHER.  glass's own TCP-LISTEN is
;;;;   called with port 0 and address 127.0.0.1; the kernel picks, and
;;;;   SOCKET-NAME says which.  5900-5920 — where this box's real desktops live
;;;;   — is REFUSED outright: if the kernel ever handed one back this refuses to
;;;;   serve rather than touching a port a desktop might want.
;;;;
;;;;   IT IS LOOPBACK, EXPLICITLY.  glass's SERVE defaults to "0.0.0.0"; that
;;;;   default is never taken here.  The listener is created before SERVE is
;;;;   called and handed to it with :LISTEN, which is also how the port becomes
;;;;   knowable before anything is serving on it — "is it listening yet?" is
;;;;   answered by the call that returned the socket, not by a sleep.
;;;;
;;;;   NOTHING IS LEFT LISTENING.  SERVE closes the listener on the way out on
;;;;   every path (its own UNWIND-PROTECT), and this file checks afterwards that
;;;;   asking the kernel for a descriptor returns the same number it did before
;;;;   — which is how a leak becomes visible.
;;;;
;;;; glass and cram are READ-ONLY: their sources are loaded where they sit.

;;; The manifest (test/glass-manifest.lisp, generated from the .asd files) is
;;; prepended by the runner and binds *GLASS-FILES*.
(unless (boundp '*glass-files*)
  (format t "~&test/glass-serve.lisp needs a manifest; run test/run-glass-serve.sh~%")
  (finish-output)
  (sys-exit 2))

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "ok   ~a~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a~%" name))))

;;; ---- load :glass -----------------------------------------------------------

(format t "~&=== loading :glass (~d files) ===~%" (length *glass-files*))
(force-output)
(dolist (entry *glass-files*)
  (load (first entry)))
(format t "~&=== loaded ===~%")
(force-output)

;;; ---- THE IMAGE -------------------------------------------------------------
;;;
;;; GLASS-TEST-PIXEL is the rule.  test/glass-rfb-client.py implements the SAME
;;; rule independently, in Python, and compares against what it computes rather
;;; than against anything this process sends it.  Keep the two in step by
;;; changing both, deliberately — that they must agree is the test.
;;;
;;;   red   varies with x only     (a transpose moves it)
;;;   green varies with y only     (a transpose moves it the other way)
;;;   blue  marks a block that is off-centre in BOTH axes, so a screen that is
;;;         flipped, mirrored or assembled band-out-of-order does not match.

(defun glass-test-pixel (x y)
  (let ((r (logand (* x 2) 255))
        (g (logand (* y 3) 255))
        (b (if (and (>= x 16) (< x 48) (>= y 8) (< y 24)) #x40 #x10)))
    (logior (ash r 16) (logior (ash g 8) b))))

(defvar *fbw* 128)
(defvar *fbh* 96)                       ; > 64 on purpose: glass bands at 64 rows

(defun paint (fb)
  "Write the image into FB through glass's own FB-PUT, one pixel at a time.
   NOT through fb-rect/fb-fill: those are glass/fb's drawing primitives and are
   already tested byte-for-byte against SBCL by test/run-glass-fb.sh.  What is
   under test here is the WIRE, so the framebuffer is filled by the dullest
   means available and every pixel is stated."
  ;; FIND-SYMBOL IS HOISTED.  It is a package-table walk, and inside the loop it
  ;; would be run once per pixel — 12 288 of them here — which is the test
  ;; spending its time on the harness rather than on glass.
  (let ((fb-put (find-symbol "FB-PUT" "GLASS"))
        (y 0))
    (loop
      (when (>= y *fbh*) (return nil))
      (let ((x 0))
        (loop
          (when (>= x *fbw*) (return nil))
          (funcall fb-put fb x y (glass-test-pixel x y))
          (setq x (+ x 1))))
      (setq y (+ y 1)))))

;;; ---- serve -----------------------------------------------------------------

(defvar *clients* 1)
(let ((s (%cli-getenv "GLASS_SERVE_CLIENTS")))
  (when (and s (> (length s) 0))
    (setq *clients* (parse-integer s))))

(defvar *fd-before*
  ;; Linux hands out the lowest free descriptor, so asking for one before and
  ;; after is how a leaked descriptor becomes visible.
  (let* ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
         (fd (sb-bsd-sockets:socket-file-descriptor s)))
    (sb-bsd-sockets:socket-close s)
    fd))

(let* ((make-fb   (find-symbol "MAKE-FRAMEBUFFER" "GLASS"))
       (tcp-listen (find-symbol "TCP-LISTEN" "GLASS"))
       (serve     (find-symbol "SERVE" "GLASS"))
       (fb (funcall make-fb *fbw* *fbh*)))
  (paint fb)
  (chk "framebuffer width"  (funcall (find-symbol "FB-WIDTH" "GLASS") fb) *fbw*)
  (chk "framebuffer height" (funcall (find-symbol "FB-HEIGHT" "GLASS") fb) *fbh*)
  (chk "a painted pixel reads back"
       (funcall (find-symbol "FB-GET" "GLASS") fb 20 10) (glass-test-pixel 20 10))

  ;; GLASS'S OWN TCP-LISTEN, on loopback, on a port the kernel picks.
  (let ((listener (funcall tcp-listen 0 :address "127.0.0.1")))
    (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name listener)
      ;; AS A LIST, because EQUAL on two vectors is EQ — #(127 0 0 1) never
      ;; EQUALs a different #(127 0 0 1), so this check would fail while
      ;; printing two identical values, which is a confusing way to be right.
      (chk "glass's listener is bound to loopback"
           (list (aref addr 0) (aref addr 1) (aref addr 2) (aref addr 3))
           (list 127 0 0 1))
      (chk-true "the port is outside 5900-5920 (the VNC range)"
                (and (> port 1023) (or (< port 5900) (> port 5920))))
      (if (and (>= port 5900) (<= port 5920))
          (progn
            (format t "~&REFUSING TO SERVE: the kernel handed back ~d, inside the~%" port)
            (format t "~&VNC range where this box's real desktops live.~%")
            (sb-bsd-sockets:socket-close listener)
            (finish-output)
            (sys-exit 1))
          nil)

      ;; The harness reads this line and starts the client.  Printed BEFORE
      ;; SERVE, which is safe precisely because the socket is already listening:
      ;; TCP-LISTEN returned, so a connect() will be queued by the kernel even
      ;; if this thread has not reached accept() yet.
      (format t "~&PORT ~d~%" port)
      (format t "~&CLIENTS ~d~%" *clients*)
      (finish-output)

      ;; ---- GLASS:SERVE ------------------------------------------------------
      ;;
      ;; :LISTEN     the socket above, so the port was knowable before serving.
      ;; :ADDRESS    stated even though :LISTEN makes it unused, so that the
      ;;             0.0.0.0 default is not merely un-taken but visibly refused.
      ;; :PASSWORD   NIL — this wire demands nothing, which is what makes the
      ;;             client's "security type None" check meaningful.  It is a
      ;;             loopback socket on an ephemeral port for the length of one
      ;;             test.
      ;; :ONCE       when one client is expected.  With more, SERVE loops and
      ;;             each client gets its own thread — which is the point of the
      ;;             multi-client run — so the loop is ended by closing the
      ;;             listener from a watchdog thread once they have all been
      ;;             counted in and out again.
      ;; :INSTALL-INJECTOR NIL — nothing here types, and the session-wide key
      ;;             injector is not this test's to set.
      ;; A REAL WAKE, because that is how glass is actually deployed.  SERVE's
      ;; :WAKE is a condvar the reader thread signals the instant a request
      ;; lands, so the sender fulfils it immediately instead of finding it on
      ;; the next 1/60 poll; every caller in the glass tree passes one.
      ;;
      ;; IT IS NOT A WORKAROUND AND IT DID NOT FIX ANYTHING.  Measured both
      ;; ways on this image: with :WAKE NIL the sender polls and never delivers
      ;; the frame; with a WAKE the server process dies outright, with no
      ;; condition and no output, right after the client is counted in.  Each
      ;; ingredient passes on its own (test/glass-shim-audit.lisp: timed
      ;; CONDITION-WAIT on a worker 200/200; a worker writing 4112 bytes to the
      ;; stream the main thread is parked reading; RFB-SENDER-LOOP itself
      ;; producing a correct 4112-byte Raw update).  What is left is the
      ;; documented runtime-JIT concurrency ceiling, and this test is the thing
      ;; that will notice when it lifts.
      (let ((seen 0) (done nil) (wake (funcall (find-symbol "MAKE-WAKE" "GLASS"))))
        (flet ((on-clients (n)
                 ;; N is how many are connected NOW.  Count arrivals; when as
                 ;; many have arrived as were expected AND all have gone, the
                 ;; run is over.
                 (when (> n 0) (setq seen (max seen n)))
                 (when (and (= n 0) (>= seen *clients*)) (setq done t))
                 (format *error-output* "~&glass-serve: ~d connected (peak ~d)~%" n seen)
                 (force-output *error-output*)))
          (if (= *clients* 1)
              (funcall serve fb port :listen listener :address "127.0.0.1"
                                     :password nil :install-injector nil
                                     :wake wake
                                     :on-clients #'on-clients :once t)
              ;; MORE THAN ONE: SERVE's accept loop only ends when the listener
              ;; is closed under it, so a watchdog thread does exactly that once
              ;; every expected client has connected and disconnected.  A budget
              ;; bounds it, so a run that never gets its clients still ends.
              (let ((watchdog
                      (sb-thread:make-thread
                       (lambda ()
                         (let ((waited 0))
                           (loop
                             (when (or done (> waited 120000)) (return nil))
                             (%sleep-ms 50)
                             (setq waited (+ waited 50)))
                           ;; Closing the listening socket is how glass itself
                           ;; cancels a parked accept (src/socket.lisp:420-440).
                           (ignore-errors
                            (sb-bsd-sockets:socket-shutdown listener :direction :io))
                           (ignore-errors (sb-bsd-sockets:socket-close listener))
                           t))
                       :name "glass-serve-watchdog")))
                (ignore-errors
                 (funcall serve fb port :listen listener :address "127.0.0.1"
                                        :password nil :install-injector nil
                                        :wake wake
                                        :on-clients #'on-clients))
                (setq done t)
                (ignore-errors (sb-thread:join-thread watchdog))))
          (chk-true "at least one client was served" (>= seen 1))
          (chk "the peak number of concurrent clients" seen *clients*)))))

  (format t "~&~%=== AND NOTHING WAS LEFT BEHIND =========================~%")
  (let* ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
         (fd (sb-bsd-sockets:socket-file-descriptor s)))
    (sb-bsd-sockets:socket-close s)
    (chk "the fd probe returns the same descriptor it did before serving"
         fd *fd-before*)))

(format t "~&~%=== VERDICT =============================================~%")
(format t "~&~d checks, ~d failed~%" *checks* *fail*)
(if (zerop *fail*)
    (format t "GLASS'S RFB SERVER ON MODUS (server side): PASS~%")
    (format t "GLASS'S RFB SERVER ON MODUS (server side): FAIL~%"))
(finish-output)
(sys-exit (if (zerop *fail*) 0 1))
