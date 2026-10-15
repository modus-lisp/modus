;;;; hdmi-audio.lisp - LPCM audio over HDMI on the Pi (BCM2835/2836/2837, the VC4 HDMI block)
;;;;
;;;; The GPU firmware has HDMI up (config.txt hdmi_drive=2: HDMI, not DVI, so its packet RAM
;;;; is on); this adds the audio stream to it.  The sequence and every value are circle's
;;;; (rsta2/circle lib/sound/hdmisoundbasedevice.cpp, Pi 1-3, its POLLED mode) and match
;;;; Linux's drivers/gpu/drm/vc4/vc4_hdmi.c:
;;;;   1. RAM_PACKET_CONFIG bit 16 set by the firmware, else the link is DVI: no audio.
;;;;   2. Clocks from CPRMAN: HSM = PLLD (500 MHz) / CM_HSMDIV (4.8 fixed point), the pixel clock
;;;;      = PLLH / 10.
;;;;   3. MAI reset; MAI_SMP = best N/M for HSM / fs; format; thresholds; channel config and map;
;;;;      audio packet config (B-frame preamble 0xF); N = 128 fs / 1000 with external CTS =
;;;;      pixclk N / (128 fs); the audio InfoFrame (2 channels) in packet RAM; the PHY's RNG on;
;;;;      MAI enabled for 2 channels, whole samples, aligned.
;;;;   4. Samples: one IEC958 subframe word per channel per frame, written to MAI_DATA while the
;;;;      FIFO is not FULL.  A word is the 24-bit sample << 4, the channel-status bit (30) for
;;;;      frames 0..39 of each 192-frame block, even parity over bits 30:4 (31), and the
;;;;      preamble nibble 0xF on frame 0.
;;;;
;;;; FEEDING IT: a ring of stereo 16-bit frames in raw memory (any thread may fill it with
;;;; HDMI-AUDIO-WRITE / -WRITE-FRAMES) and HDMI-AUDIO-PUMP, which moves frames into the FIFO
;;;; while it has room -- silence when the ring is empty, so the stream never underflows.  The
;;;; pump is meant to run over and over on a core of its own (an executor's idle function); the
;;;; ring is single-producer, single-consumer.
;;;;
;;;;   0x3F808000 HD block: +0x14 MAI_CTL  +0x18 MAI_THR  +0x1C MAI_FMT  +0x20 MAI_DATA  +0x2C MAI_SMP
;;;;   0x3F902000 HDMI block: +0x90 MAI_CHANNEL_MAP +0x94 MAI_CONFIG +0x9C AUDIO_PACKET_CONFIG
;;;;              +0xA0 RAM_PACKET_CONFIG +0xA4 RAM_PACKET_STATUS +0xA8 CRP_CFG +0xAC/+0xB0 CTS
;;;;              +0x2C4 TX_PHY_CTL_0 +0x490.. the audio InfoFrame in packet RAM
;;;;   0x1085F000 ring header: +0 frames written, +4 frames played, +8 subframe-block position,
;;;;              +0xC running, +0x10 underruns (frames of silence fed)
;;;;   0x10860000 ring: 65536 frames, one u32 each (L low 16 bits, R high) = 256 KB

(defun %ha-hd () #x3F808000)
(defun %ha-hdmi () #x3F902000)
(defun %ha-rd (a) (mem-ref a :u32))
(defun %ha-wr (a v) (setf (mem-ref a :u32) v))

(defun %ha-hdr () #x1085F000)
(defun %ha-ring () #x10860000)
(defun %ha-ring-frames () 65536)

;;; ---- clocks (circle GetHSMClockRate / GetPixelClockRate) ---------------------------------

(defun hdmi-audio-hsm-rate ()
  "The HSM clock: PLLD (500 MHz on Pi 1-3) over CM_HSMDIV's divider (4.8 fixed point)."
  (let ((div (logand (ash (%ha-rd #x3F10108C) -4) #xFFF)))
    (if (zerop div) 0 (floor (* 500000000 256) div))))

(defun hdmi-audio-pixel-rate ()
  "The pixel clock: PLLH (19.2 MHz x (NDIV + FRAC/2^20) / PDIV) over 10."
  (let* ((ctrl (%ha-rd #x3F102960))
         (fdiv (logand (%ha-rd #x3F102A60) #xFFFFF))
         (ndiv (logand ctrl #x3FF))
         (pdiv (logand (ash ctrl -12) 7)))
    (when (logbitp 11 (%ha-rd #x3F102074))
      (setq ndiv (* ndiv 2) fdiv (* fdiv 2)))
    (if (zerop pdiv)
        0
        (floor (ash (floor (* 19200000 (+ (ash ndiv 20) fdiv)) pdiv) -20) 10))))

(defun %ha-rational (gn gd maxn maxd)
  "Linux's rational_best_approximation: (values N D), N/D closest to GN/GD with N <= MAXN, D <= MAXD."
  (let ((n gn) (d gd) (n0 0) (d1 0) (n1 1) (d0 1))
    (loop
      (when (zerop d) (return nil))
      (let* ((dp d) (a (floor n d)))
        (setq d (mod n d) n dp)
        (let ((n2 (+ n0 (* a n1))) (d2 (+ d0 (* a d1))))
          (when (or (> n2 maxn) (> d2 maxd))
            (let ((tt (min (floor (- maxn n0) n1) (floor (- maxd d0) d1))))
              (when (or (> (* 2 tt) a) (and (= (* 2 tt) a) (> (* d0 dp) (* d1 d))))
                (setq n1 (+ n0 (* tt n1)) d1 (+ d0 (* tt d1)))))
            (return nil))
          (setq n0 n1 n1 n2 d0 d1 d1 d2))))
    (values n1 d1)))

(defun %ha-rate-code (rate)
  (let ((codes '(8000 11025 12000 16000 22050 24000 32000 44100 48000 64000 88200 96000
                 128000 176400 192000)))
    (let ((p (position rate codes))) (if p (+ p 1) 0))))

(defun %ha-wait-bit (addr bit want ms)
  "Wait up to MS ms for bit BIT of the register at ADDR to be WANT (T/NIL).  T if it was."
  (let ((dl (+ (rdtsc) (floor (* ms (cntfrq)) 1000))))
    (loop
      (when (eq (logbitp bit (%ha-rd addr)) want) (return t))
      (when (>= (rdtsc) dl) (return nil)))))

;;; ---- start / stop --------------------------------------------------------------------

(defun hdmi-audio-start (&optional (rate 48000))
  "Put an LPCM stereo stream at RATE into the HDMI link the firmware brought up.  T, or a
   keyword saying why not: :DVI (the link has no packet RAM: config.txt needs hdmi_drive=2),
   :CLOCKS (a clock read 0), :INFOFRAME (the packet RAM did not take the audio InfoFrame)."
  (let ((hd (%ha-hd)) (hdmi (%ha-hdmi)))
    (unless (logbitp 16 (%ha-rd (+ hdmi #xA0))) (return-from hdmi-audio-start :dvi))
    (let ((hsm (hdmi-audio-hsm-rate)) (pix (hdmi-audio-pixel-rate)))
      (when (or (zerop hsm) (zerop pix)) (return-from hdmi-audio-start :clocks))
      (multiple-value-bind (n m) (%ha-rational hsm rate #xFFFFFF 256)
        (let* ((rate128 (* rate 128))
               (nn (floor rate128 1000))
               (cts (floor (* pix nn) rate128)))
          (%ha-wr (+ hd #x14) #x8207)                         ; RESET FLUSH DLATE ERRORE ERRORF
          (%ha-wr (+ hd #x2C) (logior (ash n 8) (- m 1)))     ; MAI_SMP
          (%ha-wr (+ hd #x1C) (logior (ash (%ha-rate-code rate) 8) (ash 2 16)))   ; PCM
          (%ha-wr (+ hd #x18) #x10101010)                     ; thresholds
          (%ha-wr (+ hdmi #x94) #x0C000003)                   ; bit/format reverse, 2 channels
          (%ha-wr (+ hdmi #x90) #x08)                         ; channel map 0->0 1->1
          (%ha-wr (+ hdmi #x9C) #x21003C03)                   ; zero flat/inactive, B = 0xF, CEA 2ch
          (%ha-wr (+ hdmi #xA8) (logior (ash 1 24) nn))       ; external CTS, N
          (%ha-wr (+ hdmi #xAC) cts)
          (%ha-wr (+ hdmi #xB0) cts))))
    ;; the audio InfoFrame: off, wait, write, on, wait
    (%ha-wr (+ hdmi #xA0) (logand (%ha-rd (+ hdmi #xA0)) (lognot #x10)))
    (unless (%ha-wait-bit (+ hdmi #xA4) 4 nil 100) (return-from hdmi-audio-start :infoframe))
    (%ha-wr (+ hdmi #x490) #x000A0184)                         ; type 0x84, version 1, length 10
    (%ha-wr (+ hdmi #x494) #x00000170)                         ; checksum 0x70, 2 channels
    (do ((a (+ hdmi #x498) (+ a 4))) ((> a (+ hdmi #x4B0))) (%ha-wr a 0))
    (%ha-wr (+ hdmi #xA0) (logior (%ha-rd (+ hdmi #xA0)) #x10))
    (unless (%ha-wait-bit (+ hdmi #xA4) 4 t 100) (return-from hdmi-audio-start :infoframe))
    ;; the ring, empty; then the PHY's RNG on and the MAI enabled
    (let ((h (%ha-hdr)))
      (dotimes (k 8) (%ha-wr (+ h (* 4 k)) 0)))
    (%ha-wr (+ hdmi #x2C4) (logand (%ha-rd (+ hdmi #x2C4)) (lognot (ash 1 25))))
    (%ha-wr (+ hd #x14) #x3028)                                ; 2 ch, WHOLSMP, CHALIGN, ENABLE
    (%ha-wr (+ (%ha-hdr) #xC) 1)
    t))

(defun hdmi-audio-stop ()
  (%ha-wr (+ (%ha-hdr) #xC) 0)
  (%ha-wr (+ (%ha-hd) #x14) #x8006)                            ; DLATE ERRORE ERRORF
  (%ha-wr (+ (%ha-hdmi) #x2C4) (logior (%ha-rd (+ (%ha-hdmi) #x2C4)) (ash 1 25)))
  t)

(defun hdmi-audio-status ()
  "(running MAI_CTL frames-written frames-played underruns): MAI_CTL bits 2 ERRORE (underflow),
   1 ERRORF (overflow), 10 EMPTY, 11 FULL, 14 BUSY, 15 DLATE."
  (let ((h (%ha-hdr)))
    (list (%ha-rd (+ h #xC)) (%ha-rd (+ (%ha-hd) #x14))
          (%ha-rd h) (%ha-rd (+ h 4)) (%ha-rd (+ h #x10)))))

;;; ---- samples -------------------------------------------------------------------------

;; IEC958 channel status for 48 kHz LPCM (circle soundbasedevice.cpp): consumer, PCM, fs code 2,
;; 24-bit word, original fs 13.  Bit F of the 40-bit string goes out in frame F.
(defun %ha-cs-bit (f)
  (if (>= f 40)
      0
      (let ((byte (case (floor f 8) (0 #x04) (1 #x00) (2 #x00) (3 #x02) (t #xDB))))
        (logand (ash byte (- (mod f 8))) 1))))

(defun %ha-word (s16 f)
  "The IEC958 subframe word for 16-bit sample S16 in frame F (0..191) of its block."
  (let ((w (ash (logand (* s16 256) #xFFFFFF) 4)))
    (when (= (%ha-cs-bit f) 1) (setq w (logior w #x40000000)))
    (when (oddp (logcount w)) (setq w (logior w #x80000000)))
    (when (zerop f) (setq w (logior w #xF)))
    w))

(defun %ha-s16 (u) (if (> u 32767) (- u 65536) u))

(defun hdmi-audio-pump ()
  "Move frames from the ring into the MAI FIFO while it has room (silence when the ring is
   empty).  Returns the frames moved.  On the audio thread, over and over."
  (when (zerop (%ha-rd (+ (%ha-hdr) #xC))) (return-from hdmi-audio-pump 0))
  (let* ((h (%ha-hdr)) (ctl (+ (%ha-hd) #x14)) (data (+ (%ha-hd) #x20))
         (f (%ha-rd (+ h 8))) (moved 0))
    (loop
      (when (or (logbitp 11 (%ha-rd ctl)) (>= moved 96)) (return nil))   ; FULL, or enough
      (let ((wr (%ha-rd h)) (rd (%ha-rd (+ h 4))) (l 0) (r 0))
        (if (= wr rd)
            (%ha-wr (+ h #x10) (logand (+ (%ha-rd (+ h #x10)) 1) #xFFFFFFFF))
            (let ((a (+ (%ha-ring) (* 4 (logand rd (- (%ha-ring-frames) 1))))))
              (mfence)                                   ; the frame after the index that said so
              (let ((w (mem-ref a :u32)))                ; one frame: L low half, R high
                (setq l (%ha-s16 (logand w #xFFFF)) r (%ha-s16 (logand (ash w -16) #xFFFF))))
              (%ha-wr (+ h 4) (logand (+ rd 1) #xFFFFFFFF))))
        (%ha-wr data (%ha-word l f))
        (%ha-wr data (%ha-word r f))
        (setq f (if (= f 191) 0 (+ f 1)) moved (+ moved 1))))
    (%ha-wr (+ h 8) f)
    moved))

(defun hdmi-audio-space ()
  "Frames the ring can take now."
  (let ((h (%ha-hdr)))
    (- (%ha-ring-frames) 1 (logand (- (%ha-rd h) (%ha-rd (+ h 4))) #xFFFFFFFF))))

(defun hdmi-audio-write (l r)
  "Queue one stereo frame (16-bit signed L and R).  NIL when the ring is full."
  (let* ((h (%ha-hdr)) (wr (%ha-rd h)))
    (if (<= (hdmi-audio-space) 0)
        nil
        (let ((a (+ (%ha-ring) (* 4 (logand wr (- (%ha-ring-frames) 1))))))
          (setf (mem-ref a :u32) (logior (logand l #xFFFF) (ash (logand r #xFFFF) 16)))
          (mfence)                                       ; the frame before the index
          (%ha-wr h (logand (+ wr 1) #xFFFFFFFF))
          t))))

(defun hdmi-audio-write-frames (samples &optional (start 0) end)
  "Queue interleaved stereo SAMPLES (a sequence of 16-bit signed L R L R ...) from START;
   returns the index after the last sample queued (stops when the ring is full)."
  (let ((i start) (n (or end (length samples))))
    (loop
      (when (or (>= (+ i 1) n) (not (hdmi-audio-write (elt samples i) (elt samples (+ i 1)))))
        (return i))
      (setq i (+ i 2)))))

(defun hdmi-audio-tone (hz ms &optional (amplitude 6000))
  "Queue MS milliseconds of a HZ triangle wave at 48 kHz (a test that needs no decoder)."
  (let* ((frames (floor (* 48 ms) 1)) (period (max 2 (floor 48000 hz))) (half (floor period 2)))
    (dotimes (k frames)
      (let* ((p (mod k period))
             (v (if (< p half)
                    (- (floor (* 2 amplitude p) half) amplitude)
                    (- amplitude (floor (* 2 amplitude (- p half)) half)))))
        (loop (when (hdmi-audio-write v v) (return nil)))))
    frames))
