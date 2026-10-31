;;;; dwc2-split.lisp - SPLIT transactions: full/low-speed devices behind a
;;;; high-speed hub (keyboard, mouse) on the DWC2 host.
;;;;
;;;; A full- or low-speed device behind a high-speed hub is reached through the
;;;; hub's Transaction Translator.  Every packet is two transactions: a
;;;; START-SPLIT, which the hub ACKs once it has queued the packet for the slow
;;;; port, and then COMPLETE-SPLITs, repeated while the hub answers NYET, until
;;;; it hands back the device's answer (data / ACK, NAK, or STALL).  The host
;;;; programs this per channel: HCSPLT carries SPLTENA, the hub address and the
;;;; port, COMPSPLT selects the second phase, and HCCHAR.LSPDDEV marks a
;;;; low-speed device.  Periodic (interrupt) channels also need HCCHAR.ODDFRM.
;;;;
;;;; The sequence follows U-Boot's drivers/usb/host/dwc2.c (chunk_msg), which
;;;; enumerates exactly these devices on the Zero 2 W before Modus starts.
;;;; Everything here is new functions on its own channel; the NIC's control
;;;; and bulk paths (dwc2.lisp) are untouched.
;;;;
;;;; Depends on: dwc2.lisp.

;; CHANNEL 6, NOT 2.  The comment above says "its own channel", and 2 was not: the NIC's bulk
;; OUT (usb.lisp USB-BULK-SEND, r8152.lisp) is hardwired to channel 2 too.  A keyboard or mouse
;; poll leaves its split transaction on the channel between the start-split and the
;; complete-split, so a frame sent in between reprogrammed a channel mid-split.  On a Zero with
;; the hub's Ethernet up that was a transmit one packet behind (the SYN of a fetch left on the
;; wire only when the next frame pushed it), channels stuck enabled-and-disabling, and in the
;; end the root port disabled -- hub, NIC, keyboard and mouse gone at once.  0 is control, 1
;; the NIC's bulk IN, 2 its bulk OUT; hid.lisp's non-split driver takes 3..5.  Measured with
;; this one change, live on the board: a whole HTTP exchange clean on the wire (SYN to FIN,
;; every segment ACKed), where before the SYN never left.
(defun split-ch () 6)

;; DMA buffers in the Device (uncached) USB window, past the NIC's buffers.
(defun split-dma-base () #x11180000)
(defun split-setup-buf () (split-dma-base))
(defun split-data-buf () (+ (split-dma-base) #x40))

(defun hcsplt-spltena () (ash 1 31))
(defun hcsplt-compsplt () (ash 1 16))
(defun hcchar-lspddev () (ash 1 17))
(defun hcchar-oddfrm () (ash 1 29))
(defun hcint-nyet () #x40)

(defun split-trace-base () (+ (split-dma-base) #x800))

(defun split-trace (word)
  ;; Ring of the last 64 (phase << 16 | HCINT) words, count at +0.
  (let ((n (mem-ref (split-trace-base) :u32)))
    (setf (mem-ref (+ (split-trace-base) 4 (* 4 (logand n 63))) :u32) word)
    (setf (mem-ref (split-trace-base) :u32) (+ n 1))))

(defun split-trace-reset () (setf (mem-ref (split-trace-base) :u32) 0))

(defun split-trace-list ()
  (let ((n (min 64 (mem-ref (split-trace-base) :u32))) (r nil) (i 0))
    (loop
      (when (>= i n) (return (reverse r)))
      (setq r (cons (mem-ref (+ (split-trace-base) 4 (* 4 i)) :u32) r))
      (setq i (+ i 1)))))

(defun dwc2-split-wait (ch)
  ;; Wait for CHHLTD.  Returns HCINT, or -1 after halting a channel that
  ;; never halted on its own.
  (let ((i 0) (r -1))
    (loop
      (when (>= i 20000) (dwc2-halt-channel ch) (return r))
      (let ((hcint (dwc2-read (dwc2-hcint ch))))
        (when (not (zerop (logand hcint (hcint-chhltd))))
          (setq r hcint)
          (return r)))
      (setq i (+ i 1)))))

(defun dwc2-wait-next-uframe ()
  ;; Until HFNUM's frame number moves on (a microframe is 125 us), bounded.  On the I/O
  ;; core the wait is not wasted: the engine serves its receive and send once first
  ;; (USBE-BACKGROUND does nothing anywhere else).
  (usbe-background)
  (let ((f (logand (dwc2-read (dwc2-hfnum)) #x3FFF)) (i 0))
    (loop
      (when (or (/= f (logand (dwc2-read (dwc2-hfnum)) #x3FFF)) (> i 200000)) (return nil))
      (setq i (+ i 1)))))

(defun dwc2-split-packet (ch hcchar hcsplt hctsiz buf periodic)
  ;; One packet through the hub's TT.  Returns 1 = done (data or ACK),
  ;; 0 = the device NAKed, -1 = STALL, -2 = transaction error, -3 = timeout.
  (let ((csplit nil) (tries 0) (errs 0) (result -3))
    (loop
      (when (> tries (if periodic 40 400)) (return result))
      (setq tries (+ tries 1))
      (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
      (dwc2-write (dwc2-hcintmsk ch) #x7FF)
      (dwc2-write (dwc2-hcchar ch) hcchar)
      (dwc2-write (dwc2-hcsplt ch)
                  (if csplit (logior hcsplt (hcsplt-compsplt)) hcsplt))
      (dwc2-write (dwc2-hctsiz ch) hctsiz)
      (dwc2-write (dwc2-hcdma ch) (dwc2-bus-addr buf))
      (memory-barrier)
      ;; U-Boot: an interrupt channel goes out in the next ODD (micro)frame
      ;; when the current one is even.
      (let ((odd (if (and periodic
                          (zerop (logand (dwc2-read (dwc2-hfnum)) 1)))
                     (hcchar-oddfrm)
                     0)))
        (dwc2-write (dwc2-hcchar ch)
                    (logior (logand hcchar (logxor (hcchar-chdis) #xFFFFFFFF))
                            (logior (hcchar-chena) odd))))
      (let ((hcint (dwc2-split-wait ch)))
        (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
        (split-trace (logior (if csplit #x20000 #x10000)
                             (if (< hcint 0) #xFFFF (logand hcint #xFFFF))))
        (cond
          ((< hcint 0) (setq result -3) (return result))
          ((not (zerop (logand hcint (hcint-stall)))) (setq result -1) (return result))
          ((not csplit)
           (cond
             ;; The start-split was accepted: the complete-split belongs in a LATER
             ;; microframe.  Issued at once, a periodic CSPLIT never halted at all
             ;; (every poll a -3 timeout, the keyboard and mouse lit but dead) once
             ;; this code ran fast enough to get there inside the same microframe.
             ((not (zerop (logand hcint (hcint-ack))))
              (setq csplit t)
              (when periodic (dwc2-wait-next-uframe)))
             ((not (zerop (logand hcint (hcint-nak)))) nil)
             (t (setq errs (+ errs 1))
                (when (> errs 3) (setq result -2) (return result)))))
          (t
           (cond
             ;; NYET: the hub's TT has not finished the full/low-speed transaction
             ;; yet.  Ask again in the NEXT microframe, as U-Boot does -- retried back
             ;; to back, a LOW-speed device (well over a millisecond for its packet)
             ;; used up every try in microseconds, and the mouse never answered.
             ((not (zerop (logand hcint (hcint-nyet)))) (dwc2-wait-next-uframe))
             ((not (zerop (logand hcint (hcint-xfercompl)))) (setq result 1) (return result))
             ((not (zerop (logand hcint (hcint-ack)))) (setq result 1) (return result))
             ((not (zerop (logand hcint (hcint-nak))))
              (if periodic
                  (progn (setq result 0) (return result))
                  (setq csplit nil)))
             (t (setq errs (+ errs 1)) (setq csplit nil)
                (when (> errs 3) (setq result -2) (return result))))))))))

(defun split-hcsplt (hubaddr hubport)
  ;; SPLTENA, XACTPOS = ALL (3 << 14), hub address, port.
  (logior (hcsplt-spltena)
          (logior (ash 3 14)
                  (logior (ash (logand hubaddr #x7F) 7) (logand hubport #x7F)))))

(defun split-hcchar (mps epnum epdir eptype devaddr ls)
  (logior (dwc2-build-hcchar mps epnum epdir eptype devaddr)
          (if ls (hcchar-lspddev) 0)))

(defun usb-split-control-in (devaddr hubaddr hubport ls mps0 rtype req value index len)
  ;; A control transfer to a split device.  IN data (LEN > 0, RTYPE bit 7)
  ;; lands in (split-data-buf); LEN = 0 is a no-data request.  Returns the
  ;; number of bytes received (>= 0), or a negative dwc2-split-packet code.
  (let ((ch (split-ch)) (hs (split-hcsplt hubaddr hubport)))
    (let ((sb (split-setup-buf)))
      (setf (mem-ref sb :u8) rtype)
      (setf (mem-ref (+ sb 1) :u8) req)
      (setf (mem-ref (+ sb 2) :u8) (logand value #xFF))
      (setf (mem-ref (+ sb 3) :u8) (logand (ash value -8) #xFF))
      (setf (mem-ref (+ sb 4) :u8) (logand index #xFF))
      (setf (mem-ref (+ sb 5) :u8) (logand (ash index -8) #xFF))
      (setf (mem-ref (+ sb 6) :u8) (logand len #xFF))
      (setf (mem-ref (+ sb 7) :u8) (logand (ash len -8) #xFF)))
    (let ((r (dwc2-split-packet ch (split-hcchar mps0 0 0 0 devaddr ls) hs
                                (dwc2-build-hctsiz 8 1 (hctsiz-pid-setup))
                                (split-setup-buf) nil)))
      (if (< r 1)
          (if (= r 0) -4 r)
          (let ((got 0) (pid (hctsiz-pid-data1)) (fail 0))
            ;; DATA stage, one packet per split.
            (loop
              (when (or (>= got len) (not (zerop fail))) (return nil))
              (let ((want (min mps0 (- len got))))
                (let ((r2 (dwc2-split-packet
                           ch (split-hcchar mps0 0 1 0 devaddr ls) hs
                           (dwc2-build-hctsiz want 1 pid)
                           (+ (split-data-buf) got) nil)))
                  (if (< r2 1)
                      (setq fail (if (= r2 0) -4 r2))
                      (let ((n (- want (logand (dwc2-read (dwc2-hctsiz ch)) #x7FFFF))))
                        (setq got (+ got n))
                        (setq pid (if (= pid (hctsiz-pid-data1))
                                      (hctsiz-pid-data0)
                                      (hctsiz-pid-data1)))
                        (when (< n want) (setq fail 1)))))))
            (if (< fail 0)
                fail
                ;; STATUS stage: zero-length DATA1 in the opposite direction.
                (let ((r3 (dwc2-split-packet
                           ch (split-hcchar mps0 0 (if (zerop len) 1 0) 0 devaddr ls) hs
                           (dwc2-build-hctsiz 0 1 (hctsiz-pid-data1))
                           (split-data-buf) nil)))
                  (if (< r3 1) (if (= r3 0) -4 r3) got))))))))

(defun usb-split-interrupt-in (devaddr hubaddr hubport ls ep mps pid buf)
  ;; One interrupt-IN poll.  PID is the DATA0/DATA1 toggle to use.  Returns
  ;; (values bytes next-pid): bytes > 0 = a report in BUF, 0 = NAK (nothing
  ;; new, toggle unchanged), < 0 = error.
  (let ((ch (split-ch)))
    (let ((r (dwc2-split-packet ch (split-hcchar mps ep 1 3 devaddr ls)
                                (split-hcsplt hubaddr hubport)
                                (dwc2-build-hctsiz mps 1 pid) buf t)))
      (if (= r 1)
          (values (- mps (logand (dwc2-read (dwc2-hctsiz ch)) #x7FFFF))
                  (if (= pid (hctsiz-pid-data1)) (hctsiz-pid-data0) (hctsiz-pid-data1)))
          (values r pid)))))
