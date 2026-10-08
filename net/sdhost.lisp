;;;; sdhost.lisp - READ sectors from the boot SD card (BCM2835 SDHOST), and
;;;; its MBR partition table.
;;;;
;;;; The Zero 2 W's card slot is the SDHOST controller at ARM 0x3F202000
;;;; (U-Boot: "mmc@7e202000: 0"); the Arasan EMMC at 0x7e300000 drives the
;;;; WiFi chip.  U-Boot has already powered the card, identified it, selected
;;;; it and set the clock, so this ADOPTS that state, the way the USB drivers
;;;; adopt U-Boot's enumeration: no card initialisation here.  Measured on
;;;; the board after U-Boot: SDVDD=1, SDCDIV=3, SDHCFG=#x40E, last command 12.
;;;;
;;;; READ ONLY, deliberately.  This is the card the board boots from; nothing
;;;; here can issue a write command.
;;;;
;;;; One block per command (CMD17).  The card is SDHC, so the argument is the
;;;; sector number, not a byte offset (verified: sector 16384 is a FAT boot
;;;; sector).  An SDSC card would need (* lba 512) and is not handled.

(defun sdh-reg (off) (mem-ref (+ #x3F202000 off) :u32))
(defun sdh-reg-set (off v) (setf (mem-ref (+ #x3F202000 off) :u32) v))

;; SDCMD #x00   SDARG #x04   SDRSP0 #x10   SDHSTS #x20   SDDATA #x40
;; SDHBCT #x3C  SDHBLC #x50
;; SDCMD: NEW_FLAG #x8000, FAIL_FLAG #x4000, READ_CMD #x40.
;; SDHSTS: DATA_FLAG bit 0; error bits #xF8; write #x7F8 to clear them.

(defun sdh-wait-command ()
  "SDCMD once the controller has taken the command (NEW_FLAG clear), or -1."
  (let ((i 0))
    (loop
      (let ((c (sdh-reg #x00)))
        (when (zerop (logand c #x8000)) (return c))
        (when (> i 2000000) (return -1))
        (setq i (+ i 1))))))

(defun sdh-read-block (lba buf start)
  "Read sector LBA into BUF[START, START+512).  Signals on any failure."
  (sdh-reg-set #x20 #x7F8)
  (sdh-reg-set #x3C 512)
  (sdh-reg-set #x50 1)
  (sdh-reg-set #x04 lba)
  (sdh-reg-set #x00 (logior 17 #x40 #x8000))
  (let ((c (sdh-wait-command)))
    (when (or (< c 0) (not (zerop (logand c #x4000))))
      (error "sd: CMD17 for sector ~D failed: SDCMD ~X SDHSTS ~X"
             lba c (sdh-reg #x20))))
  (dotimes (n 128)
    (let ((i 0))
      (loop
        (when (not (zerop (logand (sdh-reg #x20) 1))) (return nil))
        (when (> i 1000000)
          (error "sd: sector ~D stalled after ~D of 128 words: SDHSTS ~X"
                 lba n (sdh-reg #x20)))
        (setq i (+ i 1))))
    (let ((w (sdh-reg #x40))
          (p (+ start (* 4 n))))
      (setf (aref buf p) (logand w 255))
      (setf (aref buf (+ p 1)) (logand (ash w -8) 255))
      (setf (aref buf (+ p 2)) (logand (ash w -16) 255))
      (setf (aref buf (+ p 3)) (logand (ash w -24) 255))))
  (let ((s (sdh-reg #x20)))
    (unless (zerop (logand s #xF8))
      (error "sd: sector ~D read with error status ~X" lba s)))
  buf)

(defun sd-read-sectors (lba count buf start)
  "Read COUNT sectors from LBA into BUF at START (pagetree's READ-SECTORS)."
  (dotimes (i count)
    (sdh-read-block (+ lba i) buf (+ start (* 512 i))))
  buf)

(defun %sd-le32 (b o)
  (+ (aref b o) (* 256 (aref b (+ o 1))) (* 65536 (aref b (+ o 2)))
     (* 16777216 (aref b (+ o 3)))))

(defun sd-partitions ()
  "The MBR's primary partitions: a list of (index type start-sector sectors)."
  (let ((b (make-array 512 :element-type '(unsigned-byte 8))))
    (sdh-read-block 0 b 0)
    (unless (and (= (aref b 510) #x55) (= (aref b 511) #xAA))
      (error "sd: sector 0 is not an MBR (signature ~X ~X)" (aref b 510) (aref b 511)))
    (let ((out nil))
      (dotimes (i 4)
        (let ((o (+ 446 (* 16 i))))
          (unless (zerop (aref b (+ o 4)))
            (push (list (+ i 1) (aref b (+ o 4)) (%sd-le32 b (+ o 8)) (%sd-le32 b (+ o 12)))
                  out))))
      (nreverse out))))
