;;;; hwrng-bcm2835.lisp -- the BCM2835/2837 hardware random number generator
;;;; (Pi 3, Pi Zero 2 W; QEMU raspi3b models it), as net/ssh.lisp's
;;;; ARCH-HW-RANDOM-FILL.  Spliced AFTER ssh.lisp in the Pi SSH builds so it
;;;; replaces the no-hardware default by last-defun-wins.
;;;;
;;;; Registers, as Linux's bcm2835-rng drives them:
;;;;   +0x00 CTRL      bit 0 RBGEN: generator on
;;;;   +0x04 STATUS    bits 0..19 warm-up count; bits 24..31 words in the FIFO
;;;;   +0x08 DATA      the next 32-bit word
;;;;   +0x10 INT_MASK  bit 0: mask its interrupt (we poll)
;;;; Peripheral base 0x3F000000 (BCM2837).  A Pi 4/5 RNG is elsewhere and
;;;; different; this image does not run there.

(defun %bcm-rng-base () #x3F104000)

(defun %bcm-rng-init ()
  (let ((b (%bcm-rng-base)))
    (when (zerop (logand (mem-ref b :u32) 1))
      (setf (mem-ref (+ b 4) :u32) #x40000)        ; discard the first 0x40000 bits
      (setf (mem-ref (+ b #x10) :u32) (logior (mem-ref (+ b #x10) :u32) 1))
      (setf (mem-ref b :u32) 1))))

;; The next 32-bit word, or -1 if the FIFO stayed empty (not NIL: in the
;; legacy Pi images NIL is the word 0, which is also a possible RNG word).  ONE 32-bit read:
;; a read of DATA pops the FIFO.  (Only AArch64 images include this file, so a
;; 32-bit value is an ordinary fixnum.)
(defun %bcm-rng-word ()
  (let ((b (%bcm-rng-base)) (n 0) (w -1))
    (loop
      (when (or (>= w 0) (> n 4000000)) (return w))
      (if (zerop (logand (ash (mem-ref (+ b 4) :u32) -24) 255))
          (setq n (+ n 1))
          (setq w (mem-ref (+ b 8) :u32))))))

(defun arch-hw-random-fill (addr n)
  (%bcm-rng-init)
  (let ((ok 1) (i 0))
    (loop
      (when (or (>= i n) (zerop ok)) (return ok))
      (let ((w (%bcm-rng-word)))
        (if (< w 0)
            (setq ok 0)
            (dotimes (k 4)
              (when (< (+ i k) n)
                (setf (mem-ref (+ addr i k) :u8) (logand (ash w (* -8 k)) 255))))))
      (setq i (+ i 4)))))

;;; The same words as a fresh (unsigned-byte 8) vector -- the shape natrium's
;;; *OS-ENTROPY* seam wants (natrium looks this name up in CL-USER and, when it
;;; is fbound, uses it in place of reading /dev/urandom, which bare metal does
;;; not have: the OPEN is a Linux syscall, an SVC trap here).  Fails CLOSED: an
;;; RNG that never fills its FIFO is an error, never a short or zeroed buffer.
(defun %modus-hardware-entropy (n)
  (%bcm-rng-init)
  (let ((out (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
        (i 0))
    (loop
      (when (>= i n) (return out))
      (let ((w (%bcm-rng-word)))
        (when (< w 0)
          (error "modus: the BCM2835 hardware RNG produced no data"))
        (dotimes (k 4)
          (when (< (+ i k) n)
            (setf (aref out (+ i k)) (logand (ash w (* -8 k)) 255)))))
      (setq i (+ i 4)))))
