;;;; emmc.lisp - the boot SD card through the Arasan SDHCI controller (BCM2835 "EMMC"),
;;;; for a Zero booted by the firmware alone: read AND write.
;;;;
;;;; net/sdhost.lisp adopts the card U-Boot left on SDHOST.  A card-booted board has no
;;;; U-Boot: the firmware leaves both controllers off, and GPIO 48-53 in a mix.  This
;;;; routes 48-53 to ALT3 (the Arasan), resets the controller, and brings the card up with
;;;; the standard SD sequence at ~400 kHz -- as circle's emmc.cpp does on a firmware-booted
;;;; Pi 3.  Measured: after the reset the pin writes stick (before it, GPFSEL4 changes to
;;;; pins 48/49 read back reverted), and CMD8 answers #x1AA.
;;;;
;;;; SDHCI registers (32-bit access only on this part): ARG1 #x08, CMDTM #x0C, RESP0
;;;; #x10, DATA #x20, STATUS #x24, CONTROL0 #x28, CONTROL1 #x2C, INTERRUPT #x30.
;;;; INTERRUPT: CMD_DONE 0, DATA_DONE 1, WRITE_RDY 4, READ_RDY 5, ERR 15 (+ bits 16-24).
;;;; The card is SDHC: block addresses are sector numbers.

(defun em-reg (o) (mem-ref (+ #x3F300000 o) :u32))
(defun em-set (o v) (setf (mem-ref (+ #x3F300000 o) :u32) v))

(defvar *em-rca* 0)

(defun em-wait (o mask want n)
  "Iterations until (REG O & MASK) = WANT, or NIL after N."
  (let ((i 0))
    (loop (when (= (logand (em-reg o) mask) want) (return i))
          (when (> i n) (return nil))
          (setq i (+ i 1)))))

(defun em-wait-irpt (mask what)
  "Wait for any INTERRUPT bit in MASK; signal on an error bit or a timeout."
  (let ((i 0))
    (loop
      (let ((r (em-reg #x30)))
        (when (logbitp 15 r)
          (em-set #x30 #xFFFFFFFF)
          (error "emmc: ~A failed: INTERRUPT ~X" what r))
        (unless (zerop (logand r mask)) (em-set #x30 mask) (return r))
        (when (> i 2000000) (error "emmc: ~A timed out: INTERRUPT ~X" what r))
        (setq i (+ i 1))))))

(defun em-cmd (cmdtm arg what)
  "Issue a command (CMDTM already encoded); RESP0 after CMD_DONE."
  (em-wait #x24 1 0 200000)
  (em-set #x30 #xFFFFFFFF)
  (em-set #x08 arg)
  (em-set #x0C cmdtm)
  (em-wait-irpt 1 what)
  (em-reg #x10))

(defun em-route-pins ()
  "GPIO 48-53 to ALT3, the Arasan controller.  Whole-register writes, no read-modify-write:
   the ARM reads the GPIO block back as \"gpi\" plus one byte (#x677069xx for every
   register), so a read-modify-write would store garbage.  These are the values whose
   write preceded the first CMD8 answer."
  (setf (mem-ref #x3F200010 :u32) #x7F706949)
  (setf (mem-ref #x3F200014 :u32) #x67706FFF))

(defun sd-mbox-power (state)
  "Firmware SET_POWER_STATE for device 0, the SD card: 2 off, 3 on (both wait), as
   circle's emmc.cpp does before it touches the card.  Through the HDMI console's
   property buffer (net/hdmi-fb.lisp), which every Pi image carries."
  (let ((b (hdmi-mbox-buf)))
    (hdmi-wr (+ b 0) 32) (hdmi-wr (+ b 4) 0)
    (hdmi-wr (+ b 8) #x00028001) (hdmi-wr (+ b 12) 8) (hdmi-wr (+ b 16) 8)
    (hdmi-wr (+ b 20) 0) (hdmi-wr (+ b 24) state) (hdmi-wr (+ b 28) 0)
    (let ((i 0)) (loop (when (> i 1000000) (return nil))
                   (when (zerop (logand (hdmi-rd (+ (hdmi-mbox-base) #x38)) #x80000000)) (return nil))
                   (setq i (+ i 1))))
    (hdmi-wr (hdmi-mbox-write) (logior (hdmi-mbox-buf-bus) 8))
    (let ((i 0)) (loop (when (> i 1000000) (return nil))
                   (when (zerop (logand (hdmi-rd (hdmi-mbox-status)) #x40000000))
                     (hdmi-rd (hdmi-mbox-read)) (return nil))
                   (setq i (+ i 1))))
    (list :code (hdmi-rd (+ b 4)) :state (hdmi-rd (+ b 24)))))

(defun em-mbox-2 (tag a b)
  "One property-mailbox tag with a two-word value {A, B}; returns the two answer words."
  (let ((buf (hdmi-mbox-buf)))
    (hdmi-wr (+ buf 0) 32) (hdmi-wr (+ buf 4) 0)
    (hdmi-wr (+ buf 8) tag) (hdmi-wr (+ buf 12) 8) (hdmi-wr (+ buf 16) 8)
    (hdmi-wr (+ buf 20) a) (hdmi-wr (+ buf 24) b) (hdmi-wr (+ buf 28) 0)
    (let ((i 0)) (loop (when (> i 1000000) (return nil))
                   (when (zerop (logand (hdmi-rd (+ (hdmi-mbox-base) #x38)) #x80000000)) (return nil))
                   (setq i (+ i 1))))
    (hdmi-wr (hdmi-mbox-write) (logior (hdmi-mbox-buf-bus) 8))
    (let ((i 0)) (loop (when (> i 1000000) (return nil))
                   (when (zerop (logand (hdmi-rd (hdmi-mbox-status)) #x40000000))
                     (hdmi-rd (hdmi-mbox-read)) (return nil))
                   (setq i (+ i 1))))
    (list (hdmi-rd (+ buf 20)) (hdmi-rd (+ buf 24)))))

(defun emmc-speed (&optional (div 5))
  "Run the card clock at base / (2 x DIV), a 10-bit divisor over CONTROL1 bits 8-15 and
   6-7.  The base (the EMMC clock) is 200-250 MHz on these boards, so the default 5 keeps
   the card at or under SD default speed, 25 MHz, either way.  (GET_CLOCK_RATE through
   the property mailbox answered 0 for every clock here, even the core: not trusted.)"
  (em-set #x2C (logand (em-reg #x2C) (lognot 4)))            ; SD clock off
  (em-set #x2C (logior (logand (em-reg #x2C) (lognot #xFFC0))
                       (ash (logand div #xFF) 8) (ash (logand (ash div -8) 3) 6)))
  (unless (em-wait #x2C 2 2 2000000) (error "emmc: clock never stable at divisor ~D" div))
  (em-set #x2C (logior (em-reg #x2C) 4))                     ; SD clock on
  div)

(defun emmc-init ()
  "Reset the controller, route the pins, bring the card up 4-bit.  T, or signals."
  (sd-mbox-power 2)                                          ; the card off, then on
  (sd-mbox-power 3)
  (em-set #x2C (ash 1 24))                                   ; SRST_HC
  (unless (em-wait #x2C (ash 1 24) 0 200000) (error "emmc: reset never cleared"))
  (em-route-pins)
  (em-set #x34 #xFFFFFFFF) (em-set #x38 0)                   ; report every status bit, no IRQ
  (em-set #x2C (logior (ash #xE 16) (ash 1 14) 1))           ; timeout, divider 256, internal clk
  (unless (em-wait #x2C 2 2 2000000) (error "emmc: clock never stable"))
  (em-set #x2C (logior (em-reg #x2C) 4))                     ; SD clock on
  (em-cmd #x00000000 0 "CMD0")
  (unless (= (logand (em-cmd #x081A0000 #x1AA "CMD8") #xFFF) #x1AA)
    (error "emmc: CMD8 echo wrong -- not an SD v2 card"))
  (let ((tries 0))
    (loop
      (em-cmd #x371A0000 0 "CMD55")
      (let ((ocr (em-cmd #x29020000 #x40FF8000 "ACMD41")))
        (when (logbitp 31 ocr)
          (unless (logbitp 30 ocr) (error "emmc: an SDSC card (OCR ~X) is not handled" ocr))
          (return t)))
      (when (> (incf tries) 400) (error "emmc: ACMD41 never ready"))))
  (em-cmd #x02090000 0 "CMD2")
  (setq *em-rca* (ash (em-cmd #x031A0000 0 "CMD3") -16))
  (em-cmd #x071B0000 (ash *em-rca* 16) "CMD7")
  (em-cmd #x371A0000 (ash *em-rca* 16) "CMD55")
  (em-cmd #x061A0000 2 "ACMD6")
  (em-set #x28 (logior (em-reg #x28) 2))                     ; 4-bit data bus
  (emmc-speed)
  t)

(defun emmc-read-block (lba buf start)
  "Sector LBA into BUF[START, START+512)."
  (em-set #x04 (logior (ash 1 16) 512))
  (em-cmd #x113A0010 lba "CMD17")
  (em-wait-irpt (ash 1 5) "read ready")
  (dotimes (n 128)
    (let ((w (em-reg #x20)) (p (+ start (* 4 n))))
      (setf (aref buf p) (logand w 255)
            (aref buf (+ p 1)) (logand (ash w -8) 255)
            (aref buf (+ p 2)) (logand (ash w -16) 255)
            (aref buf (+ p 3)) (logand (ash w -24) 255))))
  (em-wait-irpt 2 "read done")
  buf)

(defun emmc-write-block (lba buf start)
  "BUF[START, START+512) to sector LBA; returns once the card has the data."
  (em-set #x04 (logior (ash 1 16) 512))
  (em-cmd #x183A0000 lba "CMD24")
  (em-wait-irpt (ash 1 4) "write ready")
  (dotimes (n 128)
    (let ((p (+ start (* 4 n))))
      (em-set #x20 (logior (aref buf p) (ash (aref buf (+ p 1)) 8)
                           (ash (aref buf (+ p 2)) 16) (ash (aref buf (+ p 3)) 24)))))
  (em-wait-irpt 2 "write done")
  t)

(defun emmc-read-sectors (lba count buf start)
  (dotimes (i count buf) (emmc-read-block (+ lba i) buf (+ start (* 512 i)))))

(defun emmc-write-sectors (lba count buf start)
  (dotimes (i count t) (emmc-write-block (+ lba i) buf (+ start (* 512 i)))))
