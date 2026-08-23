;;;; rfb-static.lisp — ONE STATIC FRAMEBUFFER, SERVED OVER RFB, TO A REAL VNC CLIENT.
;;;;
;;;;   ./modus --script test/rfb-static.lisp            (but really: test/run-rfb-static.sh,
;;;;                                                     which supplies the client)
;;;;
;;;; ============================================================
;;;; WHY THIS IS WRITTEN HERE AND NOT REUSED FROM GLASS
;;;; ============================================================
;;;;
;;;; glass's own RFB server (glass/src/rfb.lisp) will not load under modus, and
;;;; not marginally: it uses SB-THREAD unconditionally (a mutex per client, a
;;;; waitqueue for the wake, and a MAKE-THREAD per connection for the sender
;;;; loop — no #-sb-thread arm anywhere), SB-BSD-SOCKETS for the transport,
;;;; SB-SYS:FD-STREAM-FD to read the socket queue depth, SB-POSIX for the
;;;; socket-file mode, and CRAM for the ZRLE zlib stream.  :glass/fb was
;;;; loadable because it was *written* to be (its one seam is feature-gated);
;;;; :glass is not, and making it so is a change to glass, which is read-only.
;;;;
;;;; So this is a minimal RFB 3.8 (RFC 6143) server written against modus's own
;;;; socket layer.  It serves exactly one thing — a static image, Raw-encoded —
;;;; which is the whole point: the question being answered is whether a REAL VNC
;;;; CLIENT can complete a handshake with modus and display pixels modus drew.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE, and what stops it
;;;; ============================================================
;;;;
;;;;   "IT SERVED A FRAME" IS NOT ASSERTED BY THIS PROCESS.  The client is
;;;;   Python (test/rfb-client.py) and it is the thing that decides.  It does
;;;;   the RFB handshake itself, parses ServerInit, sends a
;;;;   FramebufferUpdateRequest, decodes the rectangle, and compares the pixels
;;;;   against the image this file describes — which it generates independently
;;;;   from the same rule.  A modus-to-modus check cannot see a protocol bug
;;;;   both ends share (a byte order, a field width, an off-by-one in the
;;;;   pixel format), and that is exactly the bug class a new wire protocol has.
;;;;
;;;;   THE PIXELS ARE NOT A FLAT COLOUR.  A one-colour framebuffer passes a
;;;;   byte-order bug, a stride bug and a width/height swap all at once.  The
;;;;   image is asymmetric in x and y and uses all three channels at different
;;;;   values, so a red/blue swap, a transposed frame or a wrong stride each
;;;;   show up as a mismatch and not as a pass.
;;;;
;;;;   THE PORT IS NOT CHOSEN.  It is bind(0) + getsockname, so nothing races
;;;;   another process for a number and no number is written down.  5900-5920 —
;;;;   the VNC range, where this box's REAL desktops live — is refused outright:
;;;;   if the kernel ever handed one of those back, this REFUSES TO SERVE rather
;;;;   than touching a port a desktop might want.  The bind is 127.0.0.1 via
;;;;   SOCKET-LISTEN, which is the loopback-only spelling; SOCKET-LISTEN-ON, the
;;;;   one that can reach the network, is deliberately not called here.
;;;;
;;;;   NOTHING IS LEFT LISTENING.  The listener is closed before this returns,
;;;;   on every path, and the run is bounded: SO_RCVTIMEO on the listening fd
;;;;   means a run that never gets a client still ends by itself and still
;;;;   closes its fd, rather than parking on accept(2) forever.

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

;;; ============================================================
;;; THE IMAGE
;;; ============================================================
;;;
;;; Deliberately asymmetric in x and y, and all three channels differ, so that a
;;; red/blue swap, a transpose or a bad stride cannot pass.  test/rfb-client.py
;;; carries this same rule and generates the expected image from it
;;; independently.
;;;
;;;   red   = x * 4   (mod 256)      varies along x only
;;;   green = y * 8   (mod 256)      varies along y only
;;;   blue  = 0x40 inside a rectangle offset from centre, else 0x10

(defvar *fbw* 64)
(defvar *fbh* 40)

(defun rfb-pixel (x y)
  (let ((r (logand (* x 4) 255))
        (g (logand (* y 8) 255))
        (b (if (and (>= x 8) (and (< x 24) (and (>= y 4) (< y 12)))) 64 16)))
    (+ (* r 65536) (+ (* g 256) b))))

;;; ============================================================
;;; BYTE HELPERS — RFB is big-endian on the wire
;;; ============================================================

(defun put8 (arr i v) (aset arr i (logand v 255)) (+ i 1))

(defun put16 (arr i v)
  (aset arr i (logand (ash v -8) 255))
  (aset arr (+ i 1) (logand v 255))
  (+ i 2))

(defun put32 (arr i v)
  (aset arr i (logand (ash v -24) 255))
  (aset arr (+ i 1) (logand (ash v -16) 255))
  (aset arr (+ i 2) (logand (ash v -8) 255))
  (aset arr (+ i 3) (logand v 255))
  (+ i 4))

(defun get16 (arr i) (+ (* (aref arr i) 256) (aref arr (+ i 1))))

(defun get32 (arr i)
  (+ (* (aref arr i) 16777216)
     (+ (* (aref arr (+ i 1)) 65536)
        (+ (* (aref arr (+ i 2)) 256) (aref arr (+ i 3))))))

(defun put-string (arr i s)
  (let ((n (length s)) (k 0))
    (loop
      (when (>= k n) (return (+ i n)))
      (aset arr (+ i k) (char-code (aref s k)))
      (setq k (+ k 1)))))

;;; ============================================================
;;; THE PIXEL FORMAT WE ADVERTISE
;;; ============================================================
;;; 32 bits per pixel, depth 24, BIG-endian on the wire, true-colour, with
;;; r/g/b at shifts 16/8/0.  Writing the pixel big-endian means the four bytes
;;; are 00 RR GG BB in that order, which is the same value glass/fb holds
;;; (0x00RRGGBB) written out most-significant first — so there is no conversion
;;; step here to get wrong.  16 bytes, per RFC 6143 section 7.4.

(defun put-pixel-format (arr i)
  (let ((p i))
    (setq p (put8 arr p 32))          ; bits-per-pixel
    (setq p (put8 arr p 24))          ; depth
    (setq p (put8 arr p 1))           ; big-endian-flag: 1
    (setq p (put8 arr p 1))           ; true-colour-flag: 1
    (setq p (put16 arr p 255))        ; red-max
    (setq p (put16 arr p 255))        ; green-max
    (setq p (put16 arr p 255))        ; blue-max
    (setq p (put8 arr p 16))          ; red-shift
    (setq p (put8 arr p 8))           ; green-shift
    (setq p (put8 arr p 0))           ; blue-shift
    (setq p (put8 arr p 0))           ; padding
    (setq p (put8 arr p 0))
    (setq p (put8 arr p 0))
    p))

;;; ============================================================
;;; THE SERVER
;;; ============================================================

(defvar *rfb-name* "modus")

(defun rfb-set-rcvtimeo (fd secs)
  "SO_RCVTIMEO on FD, so accept(2) and every read on it are BOUNDED.

   This is what makes `nothing is left listening' true on the path where no
   client ever arrives: without it a run with no peer parks in accept(2)
   forever, still holding the listening socket, and the only thing that closes
   it is somebody killing the process.  setsockopt is a 5-argument syscall, so
   it rides syscall6 exactly as SO_REUSEADDR does.  Level SOL_SOCKET = 1,
   SO_RCVTIMEO = 20, and the value is a 16-byte struct timeval."
  (let ((tv (+ (%sock-addr-buf) 32)))
    (setf (mem-ref tv :u64) secs)
    (setf (mem-ref (+ tv 8) :u64) 0)
    (syscall6 54 fd 1 20 tv 16 0)))

(defun rfb-serve-one (lfd)
  "Accept ONE client, do the RFB 3.8 handshake, then answer its messages until
   the peer goes away.  Returns the number of frames sent, or a NEGATIVE number
   naming the handshake step that did not complete.

   Written as a straight sequence guarded by RC rather than as a staircase of
   nested IFs: the staircase version of this function is where a paren went
   missing, and a protocol handshake is exactly the kind of code where the shape
   should make the order of steps obvious."
  (let ((cfd (socket-accept lfd)))
    (if (< cfd 0)
        -1
        (let ((buf (make-array 512)) (frames 0) (rc 0))
          ;; ---- ProtocolVersion: we send ours, the client answers ----
          (put-string buf 0 "RFB 003.008
")
          (socket-send cfd buf 12)
          (if (< (socket-recv-fully cfd buf 0 12) 12) (setq rc -2) 0)
          ;; ---- Security: offer exactly one type, None(1) ----
          (if (= rc 0)
              (progn
                (put8 buf 0 1)
                (put8 buf 1 1)
                (socket-send cfd buf 2)
                (if (< (socket-recv-fully cfd buf 0 1) 1)
                    (setq rc -3)
                    (if (= (aref buf 0) 1) 0 (setq rc -4))))
              0)
          ;; ---- SecurityResult(0), ClientInit, then ServerInit ----
          (if (= rc 0)
              (progn
                (put32 buf 0 0)
                (socket-send cfd buf 4)
                (if (< (socket-recv-fully cfd buf 0 1) 1)
                    (setq rc -5)
                    (let ((p 0))
                      (setq p (put16 buf p *fbw*))
                      (setq p (put16 buf p *fbh*))
                      (setq p (put-pixel-format buf p))
                      (setq p (put32 buf p (length *rfb-name*)))
                      (setq p (put-string buf p *rfb-name*))
                      (socket-send cfd buf p)
                      0)))
              0)
          ;; ---- and then serve, until the peer stops talking ----
          (if (= rc 0) (setq frames (rfb-message-loop cfd buf)) 0)
          (socket-close cfd)
          (if (= rc 0) frames rc)))))

(defun rfb-message-loop (cfd buf)
  "Read client messages and answer them.  EVERY message this consumes has its
   length consumed too, including the ones that are ignored: a client-to-server
   message whose body is left in the socket desynchronises the stream, and the
   next message type read would be a byte from the middle of the last one.  A
   type we do not recognise therefore ENDS the loop rather than guessing."
  (let ((frames 0))
    (loop
      (let ((n (socket-recv cfd buf 1)))
        (when (< n 1) (return frames))
        (let ((msg (aref buf 0)))
          (cond
            ;; 0 SetPixelFormat: 3 padding + a 16-byte pixel format.
            ;; We ignore it: this server serves ONE format, the one it
            ;; advertised, and the client is told so by ServerInit.
            ((= msg 0) (socket-recv-fully cfd buf 0 19))
            ;; 2 SetEncodings: 1 padding + u16 count + count * s32
            ((= msg 2)
             (socket-recv-fully cfd buf 0 3)
             (let ((cnt (get16 buf 1)))
               (if (> cnt 0) (socket-recv-fully cfd buf 0 (* cnt 4)) 0)))
            ;; 3 FramebufferUpdateRequest: incremental + x + y + w + h.
            ;; The image is static, so incremental and non-incremental get the
            ;; same answer: the whole screen.
            ((= msg 3)
             (socket-recv-fully cfd buf 0 9)
             (rfb-send-frame cfd)
             (setq frames (+ frames 1)))
            ;; 4 KeyEvent: down-flag + 2 padding + u32 keysym
            ((= msg 4) (socket-recv-fully cfd buf 0 7))
            ;; 5 PointerEvent: button-mask + u16 x + u16 y
            ((= msg 5) (socket-recv-fully cfd buf 0 5))
            ;; 6 ClientCutText: 3 padding + u32 length + that many bytes
            ((= msg 6)
             (socket-recv-fully cfd buf 0 7)
             (let ((len (get32 buf 3)) (got 0))
               (loop
                 (when (>= got len) (return 0))
                 (let ((want (- len got)))
                   (when (> want 256) (setq want 256))
                   (let ((r (socket-recv-fully cfd buf 0 want)))
                     (when (< r 1) (return 0))
                     (setq got (+ got r)))))))
            (t (return frames))))))))


(defun rfb-send-frame (cfd)
  "One FramebufferUpdate carrying ONE Raw rectangle covering the whole screen.
   Header (RFC 6143 7.6.1): u8 msg=0, u8 pad, u16 number-of-rectangles; then per
   rectangle u16 x, y, w, h and s32 encoding (0 = Raw), then w*h pixels."
  (let ((hdr (make-array 16)) (p 0))
    (setq p (put8 hdr p 0))            ; FramebufferUpdate
    (setq p (put8 hdr p 0))            ; padding
    (setq p (put16 hdr p 1))           ; one rectangle
    (setq p (put16 hdr p 0))           ; x
    (setq p (put16 hdr p 0))           ; y
    (setq p (put16 hdr p *fbw*))       ; w
    (setq p (put16 hdr p *fbh*))       ; h
    (setq p (put32 hdr p 0))           ; encoding: Raw
    (socket-send cfd hdr p))
  ;; The pixels, a ROW AT A TIME.  Row-at-a-time and not whole-frame because
  ;; SOCKET-SEND stages through a bounded buffer; a row is 256 bytes here, which
  ;; is inside it, so no send is ever split across a chunk boundary by us.
  (let ((row (make-array (* *fbw* 4))) (y 0))
    (loop
      (when (>= y *fbh*) (return 0))
      (let ((x 0) (i 0))
        (loop
          (when (>= x *fbw*) (return 0))
          (setq i (put32 row i (rfb-pixel x y)))
          (setq x (+ x 1))))
      (socket-send cfd row (* *fbw* 4))
      (setq y (+ y 1)))))

;;; ============================================================
;;; THE RUN
;;; ============================================================

(format t "~%=== A STATIC FRAMEBUFFER, OVER RFB, TO SOMETHING THAT IS NOT MODUS ===~%")

;; A probe fd taken and released BEFORE the server, so the same number can be
;; asked for again afterwards: Linux hands out the LOWEST free descriptor, so if
;; anything below it had leaked, the second probe could not return the same one.
(defvar *fd-before* (let ((f (socket-listen 0 1))) (socket-close f) f))

(defvar *lfd* (socket-listen 0 4))

(if (< *lfd* 0)
    (format t "~%SKIP: could not bind a loopback listener.~%")
    (let ((port (socket-local-port *lfd*)))
      (if (and (>= port 5900) (<= port 5920))
          ;; The VNC range is where this box's REAL desktops live.  Refuse.
          (progn
            (socket-close *lfd*)
            (format t "~%REFUSED: the kernel handed back port ~D, which is inside~%" port)
            (format t "5900-5920.  Not serving there.~%"))
          (progn
            ;; Bound the whole run BEFORE announcing the port: a run that never
            ;; gets a client must still end by itself and still close its fd.
            (rfb-set-rcvtimeo *lfd* 60)
            (format t "PORT ~D~%" port)   ; the harness reads this line
            (finish-output)
            (let ((frames (rfb-serve-one *lfd*)))
              (socket-close *lfd*)
              (let ((fd-after (let ((f (socket-listen 0 1))) (socket-close f) f)))
                (format t "~%=== WHAT THE SERVER SAW ==================================~%")
                (chk-true "the listener bound a port outside 5900-5920"
                          (and (> port 1023) (or (< port 5900) (> port 5920))))
                (chk-true "the handshake completed and a client was served"
                          (> frames 0))
                (format t "  frames sent: ~D~%" frames)
                (format t "~%=== AND NOTHING WAS LEFT BEHIND ==========================~%")
                (format t "  Linux hands out the LOWEST free fd, so asking for one~%")
                (format t "  again must return the SAME number it did before.~%")
                (chk "the fd probe returns the same descriptor" fd-after *fd-before*)
                (format t "~%=== VERDICT ==============================================~%")
                (if (= *fail* 0)
                    (format t "STATIC FRAMEBUFFER OVER RFB: PASS (~D checks)~%" *checks*)
                    (format t "STATIC FRAMEBUFFER OVER RFB: FAIL (~D of ~D checks)~%"
                            *fail* *checks*))))))))
