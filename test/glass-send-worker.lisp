;;;; glass-send-worker.lisp — GLASS'S ENCODER, ON A WORKER THREAD, WITHOUT THE SESSION.
;;;;
;;;;   test/run-glass-send-worker.sh [MODUS-BINARY] [MODE]
;;;;
;;;; ============================================================
;;;; WHY THIS EXISTS
;;;; ============================================================
;;;;
;;;; test/run-glass-serve.sh says "the frame did not arrive" and it is right,
;;;; but it says it about a program with an RFB handshake, two threads, a
;;;; condvar, a clipboard listener and a protocol state machine in it.  This
;;;; file is the SAME failure with all of that removed: one socket, one worker
;;;; thread, one call to GLASS:SEND-RECTS, and a Python peer that counts bytes.
;;;;
;;;; It is a BISECT, not a smoke test.  MODE names which of the two wrappers
;;;; the real sender loop puts around SEND-RECTS are present:
;;;;
;;;;   plain   SEND-RECTS on the worker, bare
;;;;   tx      inside (let ((glass::*tx* (list 0))) …)         — a special rebound
;;;;   lock    inside (glass::with-fb-locked (fb) …)           — a recursive mutex
;;;;   both    inside BOTH, nested, exactly as RFB-SENDER-LOOP nests them
;;;;
;;;; TWO FAILURES, AND THEY ARE NOT THE SAME SIZE OF PROBLEM.
;;;;
;;;; (1) `both' STOPS AT 32 785 BYTES — three bytes into the SECOND rectangle's
;;;;     header — with `MVM LONGJMP (TRAP #x0511) with no active handler-case',
;;;;     the same byte count and the same place the real RFB server stops.
;;;;     Reproduces on every machine it has been run on.  Since plain, tx and
;;;;     lock all COMPLETE the transfer, this stop is not the socket, not the
;;;;     thread and not either wrapper alone: it is the two of them NESTED.
;;;;
;;;; (2) A SINGLE ZERO PIXEL, in the arms that complete, AND IT IS
;;;;     ENVIRONMENT-DEPENDENT.  On one machine all three completing arms
;;;;     reported index 2591 = (31,20) reading 0x000000; on another, an earlier
;;;;     run reported 2528 = (96,19), also 0x000000; and on that same second
;;;;     machine the CURRENT tree passes those three arms 14 runs running, and
;;;;     3 more under a 32 MB collection trigger, from a binary verified
;;;;     BYTE-IDENTICAL to the first.  So:
;;;;
;;;;       * the baseline is NOT clean, and an earlier version of this header
;;;;         said "every pixel correct" on the strength of a handful of runs on
;;;;         one box.  It was a sample, reported as a property;
;;;;       * the index MOVES between environments, so it is not a fixed
;;;;         structural offset — not an encoder or buffer boundary;
;;;;       * the value is ZERO both times, and zero is what MAKE-FRAMEBUFFER
;;;;         fills with, so it reads as ONE LOST STORE rather than one wrong
;;;;         value.
;;;;
;;;;     WHICH LOST STORE IS EXACTLY WHAT FB-SELFCHECK BELOW ANSWERS, and the
;;;;     answer costs one run: `bad' non-zero BEFORE serving means the PAINT
;;;;     lost it and the wire is innocent; `bad=0' before with the client still
;;;;     reporting a zero means the picture was right and the ENCODE or the
;;;;     TRANSPORT lost it.  Report that line with any failure here.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE PEER IS PYTHON AND IT COUNTS.  A Lisp-side "it returned" proves
;;;;   nothing about a transfer that dies mid-buffer; the byte count is taken
;;;;   on the other side of the kernel by a process that is not modus.
;;;;
;;;;   THE SCREEN IS TALLER THAN ONE BAND.  96 rows against glass's
;;;;   *MAX-BAND-ROWS* = 64 is TWO rectangles, and the failure is in the
;;;;   second one — a 64-row framebuffer would pass every mode.
;;;;
;;;;   THE MODES SHARE ONE BODY.  The four arms differ by their wrapper and by
;;;;   nothing else, so a difference between them is the wrapper.
;;;;
;;;;   NOTHING IS LEFT LISTENING.  The listener is 127.0.0.1 on a port the
;;;;   KERNEL picks (bind 0 + SOCKET-NAME); 5900-5920 is refused outright; the
;;;;   socket is closed on the way out and the runner asks `ss'.
;;;;
;;;; glass and cram are READ-ONLY: their sources are loaded where they sit.

;;; The manifest (test/glass-manifest.lisp) is prepended by the runner.
(unless (boundp '*glass-files*)
  (format t "~&test/glass-send-worker.lisp needs a manifest; run its runner~%")
  (finish-output)
  (sys-exit 2))

(dolist (entry *glass-files*)
  (load (first entry)))
(format t "~&=== loaded ===~%")
(force-output)

(defvar *fbw* 128)
(defvar *fbh* 96)                       ; > 64 on purpose: glass bands at 64 rows

(defun gsw-pixel (x y)
  (logior (ash (logand (* x 2) 255) 16)
          (ash (logand (* y 3) 255) 8)
          16))

;;; THE BOUNDS ARE LOCALS, NOT THE GLOBALS, and that is not style.  A special
;;; read compiles to SYMBOL-VALUE, which takes the runtime-table lock and hops
;;; the active GC region to region 0 — twelve thousand times through this loop.
;;; That is a mechanism under test elsewhere and it has no business being in the
;;; fixture that paints the reference image.
(defun gsw-make-fb ()
  (let ((w *fbw*) (h *fbh*))
    (let ((fb (funcall (find-symbol "MAKE-FRAMEBUFFER" "GLASS") w h))
          (put (find-symbol "FB-PUT" "GLASS"))
          (y 0))
      (loop
        (when (>= y h) (return nil))
        (let ((x 0))
          (loop
            (when (>= x w) (return nil))
            (funcall put fb x y (gsw-pixel x y))
            (setq x (+ x 1))))
        (setq y (+ y 1)))
      fb)))

(defun fb-width-of  (fb) (funcall (find-symbol "FB-WIDTH" "GLASS") fb))
(defun fb-height-of (fb) (funcall (find-symbol "FB-HEIGHT" "GLASS") fb))

;;; ---- IS THE PICTURE WRONG, OR IS THE WIRE WRONG? ---------------------------
;;;
;;; A client that reports a bad pixel cannot tell those apart, and they are
;;; different bugs in different code.  So the framebuffer is READ BACK through
;;; glass's own FB-GET and compared against the same rule that painted it —
;;; once BEFORE anything is served and once AFTER, because "never painted" and
;;; "painted, then clobbered by the encoder" are also different bugs.
;;;
;;; MAKE-FRAMEBUFFER fills with +BLACK+ = 0, so an unpainted pixel reads 0 —
;;; which is exactly the value a bad pixel has come back as every time this has
;;; been seen.  That makes "how many read 0" worth reporting separately from
;;; "how many are wrong": a wrong-but-non-zero pixel would be a different
;;; mechanism entirely.
;;; GLASS_SEND_SELFCHECK=0 turns it off.  The knob is not decoration: this
;;; failure has behaved as though it depends on where things land, so an
;;; instrument that runs 12 288 extra calls before the encode could in
;;; principle move the thing it is trying to measure.  The BEFORE check cannot
;;; hide a painting bug — the painting has already happened by then — but the
;;; AFTER check and the encode share a process, so if a run fails WITHOUT this
;;; and passes WITH it, that is itself the finding, and the knob is how you
;;; establish it in one command instead of one edit.
(defun gsw-selfcheck-on-p ()
  (let ((s (%cli-getenv "GLASS_SEND_SELFCHECK")))
    (not (and s (string= s "0")))))

(defun gsw-check-fb (fb tag)
  (unless (gsw-selfcheck-on-p) (return-from gsw-check-fb 0))
  (let ((get (find-symbol "FB-GET" "GLASS"))
        (w (fb-width-of fb)) (h (fb-height-of fb))
        (bad 0) (zero 0) (fx -1) (fy -1) (fgot 0) (fwant 0) (y 0))
    (loop
      (when (>= y h) (return nil))
      (let ((x 0))
        (loop
          (when (>= x w) (return nil))
          (let ((got (funcall get fb x y))
                (want (gsw-pixel x y)))
            (unless (eql got want)
              (setq bad (+ bad 1))
              (when (eql got 0) (setq zero (+ zero 1)))
              (when (< fx 0) (setq fx x fy y fgot got fwant want))))
          (setq x (+ x 1))))
      (setq y (+ y 1)))
    (format t "~&FB-SELFCHECK ~a: bad=~d zero=~d" tag bad zero)
    (when (>= fx 0)
      (format t " first=(~d,~d) index=~d got=~x want=~x"
              fx fy (+ (* fy w) fx) fgot fwant))
    (format t "~%")
    (finish-output)
    bad))

(defvar *mode*
  (let ((s (%cli-getenv "GLASS_SEND_MODE")))
    (cond ((null s) "both")
          ((= (length s) 0) "both")
          (t s))))

;;; The four arms.  ONE body, four wrappers — see the header.
(defun gsw-send (send-rects s fb mode)
  (let ((rects (list (list 0 0 (fb-width-of fb) (fb-height-of fb)))))
    (cond
      ((string= mode "plain")
       (funcall send-rects s fb rects 0 nil nil nil nil))
      ((string= mode "tx")
       (let ((glass::*tx* (list 0)))
         (funcall send-rects s fb rects 0 nil nil nil nil)))
      ((string= mode "lock")
       (glass::with-fb-locked (fb)
         (funcall send-rects s fb rects 0 nil nil nil nil)))
      (t
       ;; RFB-SENDER-LOOP's own nesting: the pixel lock outside, the transfer
       ;; counter inside.
       (glass::with-fb-locked (fb)
         (let ((glass::*tx* (list 0)))
           (funcall send-rects s fb rects 0 nil nil nil nil)))))))

(let* ((send-rects (find-symbol "SEND-RECTS" "GLASS"))
       (fb (gsw-make-fb))
       (listener (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
  (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
  (sb-bsd-sockets:socket-listen listener 4)
  (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name listener)
    (declare (ignore addr))
    (if (and (>= port 5900) (<= port 5920))
        (progn
          (format t "~&REFUSING: the kernel handed back ~d, inside 5900-5920.~%" port)
          (sb-bsd-sockets:socket-close listener)
          (finish-output)
          (sys-exit 1))
        nil)
    (format t "~&MODE ~a~%" *mode*)
    ;; BEFORE the port line, so the picture is graded before any client can
    ;; connect and nothing about the transfer can be blamed for it.
    (gsw-check-fb fb "before-serving")
    (format t "~&PORT ~d~%" port)
    (finish-output))
  (let* ((conn (sb-bsd-sockets:socket-accept listener))
         (s (sb-bsd-sockets:socket-make-stream conn :input t :output t
                                                    :element-type '(unsigned-byte 8))))
    ;; THE SEND IS ON A WORKER, which is the whole point: the same call on this
    ;; thread completes in every mode.
    (let ((th (sb-thread:make-thread
               (lambda ()
                 (write-string-serial "worker: sending")
                 (write-char-serial 10)
                 (gsw-send send-rects s fb *mode*)
                 (write-string-serial "worker: sent")
                 (write-char-serial 10)
                 t)
               :name "gsw-sender")))
      ;; The main thread parks reading, exactly as glass's reader thread does,
      ;; so the two threads share the socket the way the real server does.
      (format t "~&reader got ~s~%" (read-byte s nil :eof))
      (finish-output)
      (format t "~&worker joined: ~s~%" (sb-thread:join-thread th))
      (finish-output))
    (gsw-check-fb fb "after-serving")
    (sb-bsd-sockets:socket-close conn))
  (sb-bsd-sockets:socket-close listener))

(format t "~&SERVER DONE~%")
(finish-output)
(sys-exit 0)
