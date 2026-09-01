;;; ================================================================
;;; Multi-architecture dispatch — loaded after all driver/adapter source.
;;; Overrides NIC API and address functions via last-defun-wins.
;;; Checks my-architecture from metadata at 0x300008:
;;;   0=x64, 1=aarch64 → E1000 PCI (e1000-hw-*), AArch64 addresses
;;;   2=i386           → NE2000 ISA (ne2k-*), i386 addresses
;;; ================================================================

;;; Architecture config block at metadata+0x40 (VA 0x480040)
;;; Set once at boot by init-arch-addrs, read by accessor functions.
;;; Slots (4 bytes each):
;;;   0x480040: e1000-state-base
;;;   0x480044: ssh-conn-base
;;;   0x480048: ssh-ipc-base
;;;   0x48004C: e1000-rx-desc-base
;;;   0x480050: e1000-rx-buf-base
;;;   0x480054: e1000-tx-desc-base
;;;   0x480058: e1000-tx-buf-base
;;;   0x48005C: pci-mode (0=x64-pio, 1=ecam, 2=none)
;;;   0x480060: usb-ring-base
;;;   0x480064: usb-dma-base
;;;   0x480068: dwc2-base
(defun init-arch-addrs ()
  (let ((arch (td-read-u32 #x500008)))
    (cond
      ((= arch 0) ;; x64
       (td-write-u32 #x500040 #x05060000)
       (td-write-u32 #x500044 #x05080000)
       (td-write-u32 #x500048 #x300000)
       (td-write-u32 #x50004C #x05000000)
       (td-write-u32 #x500050 #x05001000)
       (td-write-u32 #x500054 #x05041000)
       (td-write-u32 #x500058 #x05041400)
       (td-write-u32 #x50005C 0)        ;; pci-mode=0 (port I/O)
       (td-write-u32 #x500060 #x05090000)
       (td-write-u32 #x500064 #x05000000)
       (td-write-u32 #x500068 0))
      ((= arch 1) ;; aarch64
       (td-write-u32 #x500040 #x41060000)
       (td-write-u32 #x500044 #x41080000)
       (td-write-u32 #x500048 #x41100000)
       (td-write-u32 #x50004C #x41000000)
       (td-write-u32 #x500050 #x41001000)
       (td-write-u32 #x500054 #x41041000)
       (td-write-u32 #x500058 #x41041400)
       (td-write-u32 #x50005C 1)        ;; pci-mode=1 (ECAM)
       (td-write-u32 #x500060 #x41090000)
       (td-write-u32 #x500064 #x41000000)
       (td-write-u32 #x500068 0))
      ((= arch 2) ;; i386
       ;; Fixpoint image extends to 0x480040 — all state must be AFTER image
       ;; (standalone i386 uses 0x200000 but fixpoint image is 3.5MB)
       (td-write-u32 #x500040 #x500000)   ;; state-base (was 0x200000)
       (td-write-u32 #x500044 #x580000)   ;; ssh-conn (was 0x280000)
       (td-write-u32 #x500048 #x600000)   ;; ssh-ipc (was 0x300000)
       (td-write-u32 #x50004C #x490000)   ;; rx-desc (was 0x180000)
       (td-write-u32 #x500050 #x491000)   ;; rx-buf (was 0x181000)
       (td-write-u32 #x500054 #x4D1000)   ;; tx-desc (was 0x1C1000)
       (td-write-u32 #x500058 #x4D1400)   ;; tx-buf (was 0x1C1400)
       (td-write-u32 #x50005C 2)           ;; pci-mode=2 (none)
       (td-write-u32 #x500060 #x690000)   ;; usb-ring (was 0x290000)
       (td-write-u32 #x500064 #x490000)   ;; usb-dma (was 0x180000)
       (td-write-u32 #x500068 0))
      ((= arch 3) ;; arm32
       (td-write-u32 #x500040 #x01060000)
       (td-write-u32 #x500044 #x01080000)
       (td-write-u32 #x500048 #x01100000)
       (td-write-u32 #x50004C #x01000000)
       (td-write-u32 #x500050 #x01001000)
       (td-write-u32 #x500054 #x01041000)
       (td-write-u32 #x500058 #x01041400)
       (td-write-u32 #x50005C 2)        ;; pci-mode=2 (none)
       (td-write-u32 #x500060 #x01090000)
       (td-write-u32 #x500064 #x01000000)
       (td-write-u32 #x500068 #x3F980000)))
    ;; Set buf-read-u32 high-byte mask: 0x3F (63) on 32-bit, 0xFF (255) on 64-bit
    ;; Stored at 0x48006C as single byte, read via mem-ref :u8 (no function call)
    (setf (mem-ref #x50006C :u8) (if (>= arch 2) 63 255))
    ;; Set is-32-bit flag: 1 on 32-bit (i386/arm32), 0 on 64-bit (x64/aarch64)
    ;; Used by dispatch wrappers via mem-ref :u8 (no function call, no register clobber)
    (setf (mem-ref #x50006D :u8) (if (>= arch 2) 1 0))))

(defun e1000-state-base () (td-read-u32 #x500040))
(defun e1000-rx-desc-base () (td-read-u32 #x50004C))
(defun e1000-rx-buf-base () (td-read-u32 #x500050))
(defun e1000-tx-desc-base () (td-read-u32 #x500054))
(defun e1000-tx-buf-base () (td-read-u32 #x500058))
(defun ssh-conn-base () (td-read-u32 #x500044))
(defun ssh-ipc-base () (td-read-u32 #x500048))
(defun usb-ring-base () (td-read-u32 #x500060))
(defun usb-dma-base () (td-read-u32 #x500064))
(defun dwc2-base () (td-read-u32 #x500068))

;;; PCI dispatch — uses pci-mode set at boot
;;; Mode 0: x64 port I/O (0xCF8/0xCFC)
;;; Mode 1: aarch64 ECAM MMIO (0x4010000000)
;;; Mode 2: none (i386 NE2000 ISA, arm32 DWC2 USB)
;; PCI config address: break deeply nested logior to avoid register clobber
(defun pci-config-addr-x64 (bus dev fn reg)
  (let ((a (logand reg #xFC)))
    (let ((b (ash fn 8)))
      (let ((c (logior b a)))
        (let ((d (ash dev 11)))
          (let ((e (logior d c)))
            (let ((f (ash bus 16)))
              (logior #x80000000 (logior f e)))))))))

(defun pci-config-addr-ecam (bus dev fn reg)
  (let ((a (logand reg #xFFC)))
    (let ((b (ash fn 12)))
      (let ((c (logior b a)))
        (let ((d (ash dev 15)))
          (let ((e (logior d c)))
            (let ((f (ash bus 20)))
              (+ #x4010000000 (logior f e)))))))))

(defun pci-config-read (bus dev fn reg)
  (let ((mode (td-read-u32 #x50005C)))
    (if (= mode 0)
        ;; x64: port I/O
        (let ((addr (pci-config-addr-x64 bus dev fn reg)))
          (io-out-dword #xCF8 addr)
          (io-in-dword #xCFC))
        ;; aarch64: ECAM MMIO
        (let ((addr (pci-config-addr-ecam bus dev fn reg)))
          (mem-ref addr :u32)))))

(defun pci-config-write (bus dev fn reg val)
  (let ((mode (td-read-u32 #x50005C)))
    (if (= mode 0)
        ;; x64: port I/O
        (let ((addr (pci-config-addr-x64 bus dev fn reg)))
          (io-out-dword #xCF8 addr)
          (io-out-dword #xCFC val))
        ;; aarch64: ECAM MMIO
        (let ((addr (pci-config-addr-ecam bus dev fn reg)))
          (setf (mem-ref addr :u32) val)))))

(defun pci-assign-bars ()
  (write-char-serial 66) ;; B
  ;; x64: BARs must be above RAM (512MB=0x20000000) but within 1GB identity map.
  ;; AArch64: RAM at 0x40000000, so 0x10000000 is fine.
  ;; Use 0x30000000 (768MB) — above RAM, within page tables.
  (let ((next-addr #x30000000))
    (let ((dev 0))
      (loop
        (when (>= dev 32) (return nil))
        (let ((id (pci-config-read 0 dev 0 0)))
          (when (not (eq id #xFFFFFFFF))
            (pci-config-write 0 dev 0 #x10 #xFFFFFFFF)
            (let ((bar-size-mask (pci-config-read 0 dev 0 #x10)))
              (when (not (= bar-size-mask 0))
                (let ((negated (logxor (logand bar-size-mask #xFFFFFFF0) #xFFFFFFFF)))
                  (let ((size (logand (+ negated 1) #xFFFFFFFF)))
                    (let ((align-mask (logxor (- size 1) #xFFFFFFFF)))
                      (let ((aligned (logand (+ next-addr (- size 1)) align-mask)))
                        (pci-config-write 0 dev 0 #x10 aligned)
                        (let ((cmd (pci-config-read 0 dev 0 4)))
                          (pci-config-write 0 dev 0 4 (logior cmd 7)))
                        (setq next-addr (+ aligned size))))))))))
        (setq dev (+ dev 1))))))

;;; NIC driver dispatch - all architectures use e1000-send/receive interface
;;; but we dispatch to the right implementation based on architecture
(defun e1000-send (buf len)
  (let ((arch (td-read-u32 #x500008)))
    (cond
      ((= arch 2)
       (write-char-serial 83) ;; S (send)
       (print-dec len)
       (write-char-serial 10)
       (ne2k-send buf len))      ;; i386: NE2000 ISA
      ((= arch 3) (usb-send buf len))       ;; arm32: DWC2 USB
      (t (e1000-hw-send buf len)))))        ;; x64/aarch64: E1000 PCI

(defvar *ne2k-dbg-cnt* 0)
(defun ne2k-debug-state ()
  ;; Print BNRY and CURR for debugging
  (let ((bnry (io-in-byte #x303)))
    (io-out-byte #x300 #x62) ;; page1
    (let ((curr (io-in-byte #x307)))
      (io-out-byte #x300 #x22) ;; page0
      (write-char-serial 91)  ;; [
      (print-dec bnry)
      (write-char-serial 47)  ;; /
      (print-dec curr)
      (write-char-serial 93)  ;; ]
      )))
(defun e1000-receive ()
  (let ((arch (td-read-u32 #x500008)))
    (cond
      ((= arch 2)
       (setq *ne2k-dbg-cnt* (+ *ne2k-dbg-cnt* 1))
       (when (eq (logand *ne2k-dbg-cnt* 255) 0)
         (ne2k-debug-state))
       (let ((r (ne2k-receive)))
         (when (not (zerop r))
           (write-char-serial 78) ;; N (NE2K got packet)
           (print-dec r)
           (write-char-serial 10))
         r))                                 ;; i386
      ((= arch 3) (usb-receive))            ;; arm32
      (t (e1000-hw-receive)))))             ;; x64/aarch64

(defun e1000-rx-buf ()
  (let ((arch (td-read-u32 #x500008)))
    (cond
      ((= arch 2) (ne2k-rx-host))           ;; i386
      ((= arch 3) (cdc-rx-buf-addr))        ;; arm32
      (t (e1000-hw-rx-buf)))))              ;; x64/aarch64

(defun pci-find-e1000 ()
  (write-char-serial 70) ;; F
  (let ((found 0))
    (let ((dev 0))
      (loop
        (when (>= dev 32) (return found))
        (write-char-serial 46) ;; .
        (let ((id (pci-config-read 0 dev 0 0)))
          (when (eq id #x100E8086)
            (write-char-serial 33) ;; !
            (let ((cmd (pci-config-read 0 dev 0 4)))
              (pci-config-write 0 dev 0 4 (logior cmd 7)))
            (let ((bar0 (pci-config-read 0 dev 0 #x10)))
              (setq found (logand bar0 #xFFFFFFF0))
              (setf (mem-ref (e1000-state-base) :u64) found))))
        (setq dev (+ dev 1))))))

;;; %serial-byte override — uses boot-time config for ssh-ipc-base, no zerop
(defun %serial-byte (b)
  (let ((ipc (ssh-ipc-base)))
    (let ((flags (mem-ref (+ ipc #x14) :u32)))
      (when (= (logand flags 2) 0)
        (write-char-serial b))
      (when (not (= (logand flags 1) 0))
        (let ((pos (mem-ref (+ ipc #x18) :u32)))
          (when (< pos 4096)
            (setf (mem-ref (+ (+ ipc #x100) pos) :u8) b)
            (setf (mem-ref (+ ipc #x18) :u32) (+ pos 1))))))))

;;; Line editor state — dynamic ssh-ipc-base, overrides dispatch wrappers
(defun edit-line-len () (mem-ref (+ (ssh-ipc-base) #x12800) :u64))
(defun edit-set-line-len (v) (setf (mem-ref (+ (ssh-ipc-base) #x12800) :u64) v))
(defun edit-cursor-pos () (mem-ref (+ (ssh-ipc-base) #x12808) :u64))
(defun edit-set-cursor-pos (v) (setf (mem-ref (+ (ssh-ipc-base) #x12808) :u64) v))

;;; print-hex-digit/byte override — uses write-char-serial for debug output
(defun print-hex-digit (n)
  (if (< n 10)
      (write-char-serial (+ n 48))
      (write-char-serial (+ n 55))))

(defun print-hex-byte (b)
  (let ((hi (logand (ash b -4) 15)))
    (let ((lo (logand b 15)))
      (print-hex-digit hi)
      (print-hex-digit lo))))

(defun print-hex32 (n)
  (print-hex-byte (logand (ash n -24) 255))
  (print-hex-byte (logand (ash n -16) 255))
  (print-hex-byte (logand (ash n -8) 255))
  (print-hex-byte (logand n 255)))

(defun e1000-hw-probe ()
  (let ((mmio (pci-find-e1000)))
    (if (= mmio 0)
        (progn
          (write-char-serial 78) (write-char-serial 70) (write-char-serial 10) ;; NF\n
          0)
        (progn
          (write-char-serial 69) (write-char-serial 61) ;; E=
          (dbg-hex32 (logand mmio #xFFFFFFFF))
          (write-char-serial 10)
          (e1000-init)))))

(defun e1000-probe ()
  (let ((arch (td-read-u32 #x500008)))
    (write-char-serial 80) ;; P
    (cond
      ((= arch 2) (progn (ne2k-reset) (ne2k-init)))  ;; i386
      ((= arch 3) nil)                                ;; arm32: handled by dwc2-init + cdc-ether-init
      (t (e1000-hw-probe)))))                        ;; x64/aarch64

;;; USB dispatch for ARM32 - these are the USB-based send/receive
;;; Copied from cdc-ether.lisp logic to avoid name conflicts
(defun usb-send (buf len)
  (let ((tx-buf (cdc-tx-buf-addr)))
    (let ((i 0))
      (loop
        (when (>= i len) (return nil))
        (setf (mem-ref (+ tx-buf i) :u8) (aref buf i))
        (setq i (+ i 1))))
    (let ((result (usb-bulk-send tx-buf len)))
      (if (eq result 1) 1 0))))

(defun usb-receive ()
  (let ((hctsiz-before (dwc2-read (dwc2-hctsiz 1))))
    (let ((result (usb-bulk-receive (cdc-rx-buf-addr) 2048)))
      (if (eq result 1)
          (let ((remaining (logand hctsiz-before #x7FFFF)))
            (let ((actual (- 2048 remaining)))
              actual))
          0))))

;;; Split fe-invert override: original c64-fe-invert has ~28 sequential forms
;;; in one let body, exceeding the ~25 form limit. Split into two halves.
;;; State array: s[0]=z s[1]=z2 s[2]=z9 s[3]=z11 s[4]=t0 s[5]=t1
(defun fe-invert-lo (s)
  (let ((z (aref s 0)) (z2 (aref s 1)) (z9 (aref s 2))
        (z11 (aref s 3)) (t0 (aref s 4)) (t1 (aref s 5)))
    (fe-sq z2 z)
    (fe-sq t0 z2)
    (fe-sq t1 t0)
    (fe-mul z9 z t1)
    (fe-mul z11 z2 z9)
    (fe-sq t0 z11)
    (fe-mul t0 z9 t0)
    (fe-sq t1 t0) (fe-sq-iter t1 4)
    (fe-mul t1 t1 t0)
    (fe-sq z2 t1) (fe-sq-iter z2 9)
    (fe-mul z2 z2 t1)
    0))

(defun fe-invert-hi (s)
  (let ((z11 (aref s 3)) (z2 (aref s 1)) (z9 (aref s 2))
        (t0 (aref s 4)) (t1 (aref s 5)))
    (fe-sq z9 z2) (fe-sq-iter z9 19)
    (fe-mul z9 z9 z2)
    (fe-sq t0 z9) (fe-sq-iter t0 9)
    (fe-mul t0 t0 t1)
    (fe-sq t1 t0) (fe-sq-iter t1 49)
    (fe-mul t1 t1 t0)
    (fe-sq z2 t1) (fe-sq-iter z2 99)
    (fe-mul z2 z2 t1)
    (fe-sq z9 z2) (fe-sq-iter z9 49)
    (fe-mul z9 z9 t0)
    (fe-sq z9 z9) (fe-sq-iter z9 4)
    (fe-mul t0 z9 z11)
    0))

(defun fe-invert (z)
  (let ((s (make-array 6)))
    (let ((z2 (make-array 40)) (z9 (make-array 40))
          (z11 (make-array 40)) (t0 (make-array 40)) (t1 (make-array 40)))
      (aset s 0 z) (aset s 1 z2) (aset s 2 z9)
      (aset s 3 z11) (aset s 4 t0) (aset s 5 t1)
      (fe-invert-lo s)
      (fe-invert-hi s)
      t0)))

;;; SSH server entry point — called when metadata mode=1
;;; Architecture-aware: uses E1000 PCI on x64/aarch64, NE2000 ISA on i386.
;;; NOTE: This is a monolithic function with ~46 sequential forms.
;;; Despite exceeding the ~25 form limit, it works correctly on all architectures.
;;; Splitting into helper functions caused ARM32 to lose all test output.
(defun ssh-kernel-main ()
  ;; Initialize architecture config block
  (init-arch-addrs)
  ;; NIC initialization (dispatch selects driver based on my-architecture)
  (write-char-serial 91) (write-char-serial 49) (write-char-serial 93) ;; [1]
  (let ((arch (td-read-u32 #x500008)))
    (if (or (= arch 2) (= arch 3))
        nil  ;; i386: no PCI (NE2000 ISA); arm32: no PCI (DWC2 USB)
        (pci-assign-bars)))
  (write-char-serial 91) (write-char-serial 50) (write-char-serial 93) ;; [2]
  (let ((arch (td-read-u32 #x500008)))
    (cond
      ((= arch 3)
       ;; arm32: DWC2 + CDC-Ether (full init: DWC2, USB enum, CDC state, bulk IN)
       (cdc-ether-init))
      (t (e1000-probe))))  ;; x64/aarch64: E1000, i386: NE2000 ISA
  ;; Crypto initialization
  (write-char-serial 91) (write-char-serial 51) (write-char-serial 93) ;; [3]
  (sha256-init)
  (write-char-serial 91) (write-char-serial 52) (write-char-serial 93) ;; [4]
  (sha512-init)
  (write-char-serial 91) (write-char-serial 53) (write-char-serial 93) ;; [5]
  (ed25519-init)
  (write-char-serial 91) (write-char-serial 54) (write-char-serial 93) ;; [6]
  (write-char-serial 10)
  (let ((arch (td-read-u32 #x500008)))
    (if (= arch 3)
        nil  ;; arm32: skip DHCP (USB network is always up)
        (dhcp-client)))
  (write-char-serial 91) (write-char-serial 55) (write-char-serial 93) ;; [7]
  (write-char-serial 10)
  (ssh-seed-random)
  (ssh-init-strings)
  ;; Pre-computed Ed25519 host key (private=zeros, public=ed25519(zeros))
  ;; NOTE: Must use :u8 stores, not :u32/:u64!
  (let ((state (e1000-state-base)))
    ;; Zero private key (32 bytes at state+0x710)
    (let ((pk-i 0))
      (loop
        (when (>= pk-i 32) (return 0))
        (setf (mem-ref (+ (+ state #x710) pk-i) :u8) 0)
        (setq pk-i (+ pk-i 1))))
    ;; Public key for all-zeros private key: ed25519(zeros)
    (setf (mem-ref (+ state #x730) :u8) #x3B)
    (setf (mem-ref (+ state #x731) :u8) #x6A)
    (setf (mem-ref (+ state #x732) :u8) #x27)
    (setf (mem-ref (+ state #x733) :u8) #xBC)
    (setf (mem-ref (+ state #x734) :u8) #xCE)
    (setf (mem-ref (+ state #x735) :u8) #xB6)
    (setf (mem-ref (+ state #x736) :u8) #xA4)
    (setf (mem-ref (+ state #x737) :u8) #x2D)
    (setf (mem-ref (+ state #x738) :u8) #x62)
    (setf (mem-ref (+ state #x739) :u8) #xA3)
    (setf (mem-ref (+ state #x73A) :u8) #xA8)
    (setf (mem-ref (+ state #x73B) :u8) #xD0)
    (setf (mem-ref (+ state #x73C) :u8) #x2A)
    (setf (mem-ref (+ state #x73D) :u8) #x6F)
    (setf (mem-ref (+ state #x73E) :u8) #x0D)
    (setf (mem-ref (+ state #x73F) :u8) #x73)
    (setf (mem-ref (+ state #x740) :u8) #x65)
    (setf (mem-ref (+ state #x741) :u8) #x32)
    (setf (mem-ref (+ state #x742) :u8) #x15)
    (setf (mem-ref (+ state #x743) :u8) #x77)
    (setf (mem-ref (+ state #x744) :u8) #x1D)
    (setf (mem-ref (+ state #x745) :u8) #xE2)
    (setf (mem-ref (+ state #x746) :u8) #x43)
    (setf (mem-ref (+ state #x747) :u8) #xA6)
    (setf (mem-ref (+ state #x748) :u8) #x3A)
    (setf (mem-ref (+ state #x749) :u8) #xC0)
    (setf (mem-ref (+ state #x74A) :u8) #x48)
    (setf (mem-ref (+ state #x74B) :u8) #xA1)
    (setf (mem-ref (+ state #x74C) :u8) #x8B)
    (setf (mem-ref (+ state #x74D) :u8) #x59)
    (setf (mem-ref (+ state #x74E) :u8) #xDA)
    (setf (mem-ref (+ state #x74F) :u8) #x29)
    (setf (mem-ref (+ state #x624) :u32) 1))
  (pre-compute-host-sign)
  (write-char-serial 91) (write-char-serial 56) (write-char-serial 93) ;; [8]
  (write-char-serial 10)
  ;; SSH port (22) — diagnostic block removed (ed25519/poly/chacha tests hung on i386/arm32)
  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)
  ;; Initialize connections
  (let ((i 0))
    (loop
      (when (>= i 4) (return 0))
      (setf (mem-ref (conn-base i) :u32) 0)
      (setq i (+ i 1))))
  ;; Pre-compute server ephemeral X25519 key pair
  (pre-compute-server-eph (conn-ssh 0))
  (write-char-serial 83) (write-char-serial 83) (write-char-serial 72) ;; SSH
  (write-char-serial 10)
  (net-actor-main))

;;; Override ssh-make-mpint: original uses 3-arg + in (+ 4 need-zero i)
;;; which is broken on MVM, AND (aref bytes (+ start i)) inside aset value
;;; which causes register clobber on arm32.
(defun ssh-make-mpint (bytes)
  (let ((start 0))
    (let ((done 0))
      (loop
        (if (< start 32)
            (if (zerop (aref bytes start))
                (if (zerop done) (setq start (+ start 1)) (return ()))
                (progn (setq done 1) (return ())))
            (return ()))))
    (let ((sig-len (- 32 start))
          (need-zero 0))
      (when (> sig-len 0)
        (when (not (zerop (logand (aref bytes start) #x80)))
          (setq need-zero 1)))
      (let ((total (+ sig-len need-zero)))
        (let ((r (make-array (+ 4 total))))
          (ssh-put-u32 r 0 total)
          (when need-zero (aset r 4 0))
          (let ((base (+ 4 need-zero)))
            (dotimes (i sig-len)
              (let ((src-idx (+ start i)))
                (let ((val (aref bytes src-idx)))
                  (let ((dst-idx (+ base i)))
                    (aset r dst-idx val))))))
          r)))))

;;; Flat helper: combine 4 values with logior (avoids nested expression clobber)
(defun flat-logior4 (a b c d)
  (let ((lo (logior a b)))
    (let ((mid (logior lo c)))
      (logior mid d))))

;;; Flat buf-read-u32 family — BARE overrides (replace dispatch wrappers).
;;; The original versions use nested logior which miscompiles on bare-metal x64.
;;; These flat versions use flat-logior4 and read the high-byte mask from
;;; config at 0x48006C (set at boot: 63 on 32-bit, 255 on 64-bit).
;;; No conditional branches — just a mem-ref for the mask.
;;;
;;; NOTE: The rename system only changes (defun NAME ...) — calls within
;;; renamed source still call the bare buf-read-u32, not c64-buf-read-u32.
;;; So these bare overrides are what c64-fe-mul etc. actually use.
(defun buf-read-u32 (buf off)
  (let ((b0 (logand (aref buf off) (mem-ref #x50006C :u8))))
    (let ((b1 (aref buf (+ off 1))))
      (let ((b2 (aref buf (+ off 2))))
        (let ((b3 (aref buf (+ off 3))))
          (flat-logior4 (ash b0 24) (ash b1 16) (ash b2 8) b3))))))
(defun buf-read-u16-mem (addr off)
  (let ((a0 (+ addr off)))
    (let ((hi (mem-ref a0 :u8)))
      (let ((lo (mem-ref (+ a0 1) :u8)))
        (logior (ash hi 8) lo)))))
(defun buf-read-u32-mem (addr off)
  (let ((a0 (+ addr off)))
    (let ((b0 (logand (mem-ref a0 :u8) (mem-ref #x50006C :u8))))
      (let ((b1 (mem-ref (+ a0 1) :u8)))
        (let ((b2 (mem-ref (+ a0 2) :u8)))
          (let ((b3 (mem-ref (+ a0 3) :u8)))
            (flat-logior4 (ash b0 24) (ash b1 16) (ash b2 8) b3)))))))
(defun buf-read-u32-le (buf offset)
  (let ((b0 (aref buf offset)))
    (let ((b1 (aref buf (+ offset 1))))
      (let ((b2 (aref buf (+ offset 2))))
        (let ((b3 (logand (aref buf (+ offset 3)) (mem-ref #x50006C :u8))))
          (flat-logior4 b0 (ash b1 8) (ash b2 16) (ash b3 24)))))))
(defun ssh-get-u32 (arr off)
  (let ((b0 (logand (aref arr off) (mem-ref #x50006C :u8))))
    (let ((b1 (aref arr (+ off 1))))
      (let ((b2 (aref arr (+ off 2))))
        (let ((b3 (aref arr (+ off 3))))
          (flat-logior4 (ash b0 24) (ash b1 16) (ash b2 8) b3))))))

;;; Safe buf-write-u32: pre-compute all values into let bindings to avoid
;;; variable-index ASET bug (value expression clobbers index register).
(defun buf-write-u32-helper (buf off v0 v1 v2 v3)
  (aset buf off v0)
  (let ((o1 (+ off 1)))
    (aset buf o1 v1))
  (let ((o2 (+ off 2)))
    (aset buf o2 v2))
  (let ((o3 (+ off 3)))
    (aset buf o3 v3)))
(defun buf-write-u32 (buf off val)
  (let ((v0 (logand (ash val -24) #xFF)))
    (let ((v1 (logand (ash val -16) #xFF)))
      (let ((v2 (logand (ash val -8) #xFF)))
        (let ((v3 (logand val #xFF)))
          (buf-write-u32-helper buf off v0 v1 v2 v3))))))

;;; ip-checksum / icmp-checksum: original uses (+ start i 1) which is 3-arg +
(defun ip-checksum (buf start len)
  (let ((sum 0))
    (let ((i 0))
      (dotimes (j (truncate len 2))
        (let ((pos (+ start i)))
          (let ((hi (aref buf pos)))
            (let ((lo (aref buf (+ pos 1))))
              (let ((word (logior (ash hi 8) lo)))
                (setq sum (+ sum word))
                (setq i (+ i 2))))))
        )
      (let ((folded (+ (logand sum #xFFFF) (ash sum -16))))
        (logand (logxor folded #xFFFF) #xFFFF)))))
(defun icmp-checksum (buf start len)
  (let ((sum 0))
    (let ((i 0))
      (dotimes (j (truncate len 2))
        (let ((pos (+ start i)))
          (let ((hi (aref buf pos)))
            (let ((lo (aref buf (+ pos 1))))
              (let ((word (logior (ash hi 8) lo)))
                (setq sum (+ sum word))
                (setq i (+ i 2)))))))
      (when (not (zerop (mod len 2)))
        (let ((last-byte (aref buf (+ start i))))
          (setq sum (+ sum (ash last-byte 8)))))
      (let ((folded (+ (logand sum #xFFFF) (ash sum -16))))
        (let ((folded2 (+ (logand folded #xFFFF) (ash folded -16))))
          (logand (logxor folded2 #xFFFF) #xFFFF))))))

;;; io-delay — interrupt-driven on all architectures (SSH mode), busy-wait otherwise
;;; x64/i386: PIC+PIT at ~1000Hz, STI-HLT sleeps until PIT tick (~1ms).
;;; AArch64/ARM32: ARM virtual timer (~1ms at 62.5MHz), timer-rearm + WFI.
;;; Cross-compile mode: busy-wait (speed matters more than CPU efficiency).
(defun io-delay ()
  (let ((mode (td-read-u32 #x500038)))
    (if (= mode 1)
        ;; SSH mode: sleep until timer/interrupt (~1ms)
        (let ((arch (td-read-u32 #x500008)))
          (if (= arch 3)
              ;; ARM32: timer-rearm + WFI
              (progn (timer-rearm) (wfi))
          (if (= arch 1)
              ;; AArch64: timer-rearm + WFI
              (progn (timer-rearm) (wfi))
              ;; x64/i386: STI+HLT (atomic), then CLI
              (progn (sti-hlt) (cli)))))
        ;; Cross-compile mode: quick busy-wait
        (dotimes (d 100) (mem-ref 0 :u8)))))

;;; Flat htonl — BARE override (replaces dispatch wrapper).
;;; Uses config mask at 0x48006C for byte 0 (63 on 32-bit, 255 on 64-bit).
(defun htonl (v)
  (let ((b0 (logand v (mem-ref #x50006C :u8))))
    (let ((b1 (logand (ash v -8) 255)))
      (let ((b2 (logand (ash v -16) 255)))
        (let ((b3 (logand (ash v -24) 255)))
          (flat-logior4 (ash b0 24) (ash b1 16) (ash b2 8) b3))))))
;;; Flat fe-from-bytes — each limb broken into lets to avoid nested logior
;;; Only for 64-bit: c32-fe-from-bytes uses byte-level extraction for 30-bit safety
(defun c64-fe-from-bytes (bytes)
  (let ((fe (make-array 40)))
    ;; Limb 0: bits 0-25
    (let ((v (flat-logior4 (aref bytes 0) (ash (aref bytes 1) 8)
                           (ash (aref bytes 2) 16)
                           (ash (logand (aref bytes 3) 3) 24))))
      (buf-write-u32 fe 0 v))
    ;; Limb 1: bits 26-50
    (let ((v (flat-logior4 (ash (aref bytes 3) -2) (ash (aref bytes 4) 6)
                           (ash (aref bytes 5) 14)
                           (ash (logand (aref bytes 6) 7) 22))))
      (buf-write-u32 fe 4 v))
    ;; Limb 2: bits 51-76
    (let ((v (flat-logior4 (ash (aref bytes 6) -3) (ash (aref bytes 7) 5)
                           (ash (aref bytes 8) 13)
                           (ash (logand (aref bytes 9) 31) 21))))
      (buf-write-u32 fe 8 v))
    ;; Limb 3: bits 77-101
    (let ((v (flat-logior4 (ash (aref bytes 9) -5) (ash (aref bytes 10) 3)
                           (ash (aref bytes 11) 11)
                           (ash (logand (aref bytes 12) 63) 19))))
      (buf-write-u32 fe 12 v))
    ;; Limb 4: bits 102-127
    (let ((v (flat-logior4 (ash (aref bytes 12) -6) (ash (aref bytes 13) 2)
                           (ash (aref bytes 14) 10) (ash (aref bytes 15) 18))))
      (buf-write-u32 fe 16 v))
    ;; Limb 5: bits 128-152 (25 bits)
    (let ((v (flat-logior4 (aref bytes 16) (ash (aref bytes 17) 8)
                           (ash (aref bytes 18) 16)
                           (ash (logand (aref bytes 19) 1) 24))))
      (buf-write-u32 fe 20 v))
    ;; Limb 6: bits 153-178 (26 bits)
    (let ((v (flat-logior4 (ash (aref bytes 19) -1) (ash (aref bytes 20) 7)
                           (ash (aref bytes 21) 15)
                           (ash (logand (aref bytes 22) 7) 23))))
      (buf-write-u32 fe 24 v))
    ;; Limb 7: bits 179-203 (25 bits)
    (let ((v (flat-logior4 (ash (aref bytes 22) -3) (ash (aref bytes 23) 5)
                           (ash (aref bytes 24) 13)
                           (ash (logand (aref bytes 25) 15) 21))))
      (buf-write-u32 fe 28 v))
    ;; Limb 8: bits 204-229 (26 bits)
    (let ((v (flat-logior4 (ash (aref bytes 25) -4) (ash (aref bytes 26) 4)
                           (ash (aref bytes 27) 12)
                           (ash (logand (aref bytes 28) 63) 20))))
      (buf-write-u32 fe 32 v))
    ;; Limb 9: bits 230-255
    (let ((v (flat-logior4 (ash (aref bytes 28) -6) (ash (aref bytes 29) 2)
                           (ash (aref bytes 30) 10) (ash (aref bytes 31) 18))))
      (buf-write-u32 fe 36 v))
    fe))

;;; Flat ssh-copy-array helper (shallow nesting avoids register spill issues)
(defun ssh-copy-array (dst src src-off len)
  (let ((i 0))
    (loop
      (when (>= i len) (return 0))
      (let ((v (aref src (+ src-off i))))
        (aset dst i v))
      (setq i (+ i 1)))))

;;; Flat ssh-parse-packet (override dispatch - reduces nesting depth)
(defun ssh-parse-packet (ssh data data-len)
  (when (< data-len 5) (return ()))
  (let ((packet-len (ssh-get-u32 data 0)))
    (when (< data-len (+ 4 packet-len)) (return ()))
    (let ((pad-len (aref data 4)))
      (let ((payload-len (- (- packet-len pad-len) 1)))
        (let ((payload (make-array payload-len)))
          (ssh-copy-array payload data 5 payload-len)
          (let ((cb (- ssh #x20)))
            (setf (mem-ref (+ cb #x16F8) :u32) (- data-len (+ 4 packet-len)))
            (setf (mem-ref (+ cb #x16FC) :u32) (+ 4 packet-len)))
          (cons payload payload-len))))))

;;; Direct poly overrides — bypass dispatch wrappers
;;; Flattened poly-from-17: avoids deeply nested logior/ash that triggers
;;; ARM32 register clobber bug.
(defun poly-from-17-limb0 (block limbs)
  (let ((b0 (aref block 0))
        (b1 (ash (aref block 1) 8))
        (b2 (ash (aref block 2) 16)))
    (let ((b3m (logand (aref block 3) #x03)))
      (let ((b3 (ash b3m 24)))
        (let ((v01 (logior b0 b1)))
          (let ((v23 (logior b2 b3)))
            (buf-write-u32 limbs 0 (logior v01 v23))))))))

(defun poly-from-17-limb1 (block limbs)
  (let ((b3 (ash (aref block 3) -2))
        (b4 (ash (aref block 4) 6))
        (b5 (ash (aref block 5) 14)))
    (let ((b6m (logand (aref block 6) #x0F)))
      (let ((b6 (ash b6m 22)))
        (let ((v01 (logior b3 b4)))
          (let ((v23 (logior b5 b6)))
            (buf-write-u32 limbs 4 (logand (logior v01 v23) #x3FFFFFF))))))))

(defun poly-from-17-limb2 (block limbs)
  (let ((b6 (ash (aref block 6) -4))
        (b7 (ash (aref block 7) 4))
        (b8 (ash (aref block 8) 12)))
    (let ((b9m (logand (aref block 9) #x3F)))
      (let ((b9 (ash b9m 20)))
        (let ((v01 (logior b6 b7)))
          (let ((v23 (logior b8 b9)))
            (buf-write-u32 limbs 8 (logand (logior v01 v23) #x3FFFFFF))))))))

(defun poly-from-17-limb3 (block limbs)
  (let ((b9 (ash (aref block 9) -6))
        (b10 (ash (aref block 10) 2))
        (b11 (ash (aref block 11) 10))
        (b12 (ash (aref block 12) 18)))
    (let ((v01 (logior b9 b10)))
      (let ((v23 (logior b11 b12)))
        (buf-write-u32 limbs 12 (logand (logior v01 v23) #x3FFFFFF))))))

(defun poly-from-17-limb4 (block limbs)
  (let ((b13 (aref block 13))
        (b14 (ash (aref block 14) 8))
        (b15 (ash (aref block 15) 16)))
    (let ((b16m (logand (aref block 16) #x03)))
      (let ((b16 (ash b16m 24)))
        (let ((v01 (logior b13 b14)))
          (let ((v23 (logior b15 b16)))
            (buf-write-u32 limbs 16 (logior v01 v23))))))))

(defun poly-from-17 (block limbs)
  (poly-from-17-limb0 block limbs)
  (poly-from-17-limb1 block limbs)
  (poly-from-17-limb2 block limbs)
  (poly-from-17-limb3 block limbs)
  (poly-from-17-limb4 block limbs))

;;; Flattened poly-to-16: avoids nested logior/ash expressions
(defun poly-to-16-lo (limbs result)
  (let ((l0 (buf-read-u32 limbs 0))
        (l1 (buf-read-u32 limbs 4)))
    (aset result 0 (logand l0 #xFF))
    (aset result 1 (logand (ash l0 -8) #xFF))
    (aset result 2 (logand (ash l0 -16) #xFF))
    (let ((l0h (ash l0 -24))
          (l1l (ash l1 2)))
      (aset result 3 (logand (logior l0h l1l) #xFF)))
    (aset result 4 (logand (ash l1 -6) #xFF))
    (aset result 5 (logand (ash l1 -14) #xFF))))

(defun poly-to-16-mid (limbs result)
  (let ((l1 (buf-read-u32 limbs 4))
        (l2 (buf-read-u32 limbs 8))
        (l3 (buf-read-u32 limbs 12)))
    (let ((l1h (ash l1 -22))
          (l2l (ash l2 4)))
      (aset result 6 (logand (logior l1h l2l) #xFF)))
    (aset result 7 (logand (ash l2 -4) #xFF))
    (aset result 8 (logand (ash l2 -12) #xFF))
    (let ((l2h (ash l2 -20))
          (l3m (logand l3 #x03)))
      (let ((l3l (ash l3m 6)))
        (aset result 9 (logand (logior l2h l3l) #xFF))))
    (aset result 10 (logand (ash l3 -2) #xFF))
    (aset result 11 (logand (ash l3 -10) #xFF))
    (aset result 12 (logand (ash l3 -18) #xFF))))

(defun poly-to-16-hi (limbs result)
  (let ((l4 (buf-read-u32 limbs 16)))
    (aset result 13 (logand l4 #xFF))
    (aset result 14 (logand (ash l4 -8) #xFF))
    (aset result 15 (logand (ash l4 -16) #xFF))))

(defun poly-to-16 (limbs result)
  (poly-to-16-lo limbs result)
  (poly-to-16-mid limbs result)
  (poly-to-16-hi limbs result))

;;; Split poly-mul: avoids >18 nested lets that miscompile on ARM32.
;;; Uses poly-mul-acc (5 products as triple) and poly-carry-step (write+carry)
;;; from 32bit-overrides.lisp.
(defun poly-mul (a r)
  (let ((a0 (buf-read-u32 a 0))
        (a1 (buf-read-u32 a 4))
        (a2 (buf-read-u32 a 8))
        (a3 (buf-read-u32 a 12))
        (a4 (buf-read-u32 a 16)))
    (poly-mul-1b a a0 a1 a2 a3 a4 r)))

(defun poly-mul-1b (a a0 a1 a2 a3 a4 r)
  (let ((r0 (buf-read-u32 r 0))
        (r1 (buf-read-u32 r 4))
        (r2 (buf-read-u32 r 8))
        (r3 (buf-read-u32 r 12))
        (r4 (buf-read-u32 r 16)))
    (poly-mul-1c a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4)))

(defun poly-mul-1c (a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4)
  (let ((s1 (* r1 5))
        (s2 (* r2 5))
        (s3 (* r3 5))
        (s4 (* r4 5)))
    (let ((d0 (poly-mul-acc a0 a1 a2 a3 a4 r0 s1 s2 s3 s4)))
      (let ((c0 (poly-carry-step a 0 d0 0 0)))
        (poly-mul-d1 a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 s2 s3 s4 c0)))))

(defun poly-mul-d1 (a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 s2 s3 s4 c0)
  (let ((d1 (poly-mul-acc a0 a1 a2 a3 a4 r1 s2 s3 s4 r0)))
    (let ((c1 (poly-carry-step a 4 d1 (car c0) (cdr c0))))
      (poly-mul-d2 a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 s3 s4 c1))))

(defun poly-mul-d2 (a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 s3 s4 c1)
  (let ((d2 (poly-mul-acc a0 a1 a2 a3 a4 r2 s3 s4 r0 r1)))
    (let ((c2 (poly-carry-step a 8 d2 (car c1) (cdr c1))))
      (poly-mul-d3 a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 s4 c2))))

(defun poly-mul-d3 (a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 s4 c2)
  (let ((d3 (poly-mul-acc a0 a1 a2 a3 a4 r3 s4 r0 r1 r2)))
    (let ((c3 (poly-carry-step a 12 d3 (car c2) (cdr c2))))
      (poly-mul-d4 a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 c3))))

(defun poly-mul-d4 (a a0 a1 a2 a3 a4 r0 r1 r2 r3 r4 c3)
  (let ((d4 (poly-mul-acc a0 a1 a2 a3 a4 r4 r0 r1 r2 r3)))
    (let ((dc4 (tadd d4 (cons 0 (cons (car c3) (cdr c3))))))
      (buf-write-u32 a 16 (cddr dc4))
      (poly-mul-wrap a (car dc4) (cadr dc4)))))

(defun poly-mul-wrap (a wh wm)
  (when (not (zerop (+ wm wh)))
    (let ((c5m (* wm 5))
          (c5h (* wh 5)))
      (let ((l0 (+ (buf-read-u32 a 0) c5m)))
        (buf-write-u32 a 0 (logand l0 67108863))
        (let ((c (+ (ash l0 -26) c5h)))
          (when (> c 0)
            (buf-write-u32 a 4 (+ (buf-read-u32 a 4) c))))))))

;;; Debug helper: print 32-bit value as 8 hex chars
(defun dbg-hex32 (v)
  (print-hex-byte (logand (ash v -24) #xFF))
  (print-hex-byte (logand (ash v -16) #xFF))
  (print-hex-byte (logand (ash v -8) #xFF))
  (print-hex-byte (logand v #xFF)))

;;; Debug helper: print array bytes as hex
(defun dbg-hex-bytes (arr start count)
  (let ((i 0))
    (loop
      (when (>= i count) (return 0))
      (print-hex-byte (aref arr (+ start i)))
      (setq i (+ i 1)))))

;;; Poly1305 diagnostic test: inline steps with intermediate output
(defun poly1305-diag-setup-msg (pmsg)
  (aset pmsg 0 67) (aset pmsg 1 114) (aset pmsg 2 121) (aset pmsg 3 112)
  (aset pmsg 4 116) (aset pmsg 5 111) (aset pmsg 6 103) (aset pmsg 7 114)
  (aset pmsg 8 97) (aset pmsg 9 112) (aset pmsg 10 104) (aset pmsg 11 105)
  (aset pmsg 12 99) (aset pmsg 13 32) (aset pmsg 14 70) (aset pmsg 15 111)
  (aset pmsg 16 114) (aset pmsg 17 117) (aset pmsg 18 109) (aset pmsg 19 32)
  (aset pmsg 20 82) (aset pmsg 21 101) (aset pmsg 22 115) (aset pmsg 23 101)
  (aset pmsg 24 97) (aset pmsg 25 114) (aset pmsg 26 99) (aset pmsg 27 104)
  (aset pmsg 28 32) (aset pmsg 29 71) (aset pmsg 30 114) (aset pmsg 31 111)
  (aset pmsg 32 117) (aset pmsg 33 112))

(defun poly1305-diag-print-limbs (tag h)
  (write-char-serial tag)
  (print-dec (buf-read-u32 h 0))
  (write-char-serial 44)
  (print-dec (buf-read-u32 h 4))
  (write-char-serial 44)
  (print-dec (buf-read-u32 h 8))
  (write-char-serial 44)
  (print-dec (buf-read-u32 h 12))
  (write-char-serial 44)
  (print-dec (buf-read-u32 h 16))
  (write-char-serial 10))

(defun poly1305-diag-block1 (h rlimbs blk nlimbs msg)
  ;; First block: bytes 0-15, pad byte 16 = 1
  (dotimes (i 17) (aset blk i 0))
  (dotimes (i 16) (aset blk i (aref msg i)))
  (aset blk 16 1)
  (poly-from-17 blk nlimbs)
  (poly1305-diag-print-limbs 110 nlimbs) ;; n: block 1 limbs
  (poly-add-limbs h nlimbs)
  (poly1305-diag-print-limbs 104 h) ;; h: after add
  (poly-mul h rlimbs)
  (poly1305-diag-print-limbs 109 h)) ;; m: after mul

(defun poly1305-diag-block2 (h rlimbs blk nlimbs msg)
  ;; Second block: bytes 16-31, pad byte 16 = 1
  (dotimes (i 17) (aset blk i 0))
  (dotimes (i 16) (aset blk i (aref msg (+ 16 i))))
  (aset blk 16 1)
  ;; Dump first 4 bytes of blk to verify copy
  (write-char-serial 88) ;; X
  (print-dec (aref blk 0)) ;; expect 114 (r)
  (write-char-serial 44)
  (print-dec (aref blk 1)) ;; expect 117 (u)
  (write-char-serial 44)
  (print-dec (aref blk 2)) ;; expect 109 (m)
  (write-char-serial 44)
  (print-dec (aref blk 3)) ;; expect 32 (space)
  (write-char-serial 10)
  (poly-from-17 blk nlimbs)
  (poly1305-diag-print-limbs 50 nlimbs) ;; 2: block2 limbs
  (poly-add-limbs h nlimbs)
  (poly1305-diag-print-limbs 65 h) ;; A: after add block2
  (poly-mul h rlimbs)
  (poly1305-diag-print-limbs 66 h)) ;; B: after mul block2

(defun poly1305-diag-block3 (h rlimbs blk nlimbs msg)
  ;; Third block: bytes 32-33, pad byte 2 = 1
  (dotimes (i 17) (aset blk i 0))
  (aset blk 0 (aref msg 32))
  (aset blk 1 (aref msg 33))
  (aset blk 2 1)
  (poly-from-17 blk nlimbs)
  (poly1305-diag-print-limbs 51 nlimbs) ;; 3: block3 limbs
  (poly-add-limbs h nlimbs)
  (poly1305-diag-print-limbs 67 h) ;; C: after add block3
  (poly-mul h rlimbs)
  (poly1305-diag-print-limbs 68 h)) ;; D: after mul block3

(defun poly1305-diag-finish (h key result)
  (poly-reduce h)
  (poly-to-16 h result)
  (poly1305-diag-print-limbs 114 h) ;; r: after reduce
  ;; Add s (key bytes 16..31) mod 2^128
  (let ((carry 0))
    (dotimes (i 16)
      (let ((ri (aref result i))
            (ki (aref key (+ 16 i))))
        (let ((sum (+ ri (+ ki carry))))
          (aset result i (logand sum #xFF))
          (setq carry (ash sum -8)))))))

(defun poly1305-diag-test (pkey pmsg)
  (let ((rbuf (make-array 17))
        (rlimbs (make-array 20))
        (h (make-array 20))
        (blk (make-array 17))
        (nlimbs (make-array 20))
        (result (make-array 16)))
    ;; Copy and clamp r
    (dotimes (i 16) (aset rbuf i (aref pkey i)))
    (aset rbuf 16 0)
    (poly-clamp rbuf)
    (poly-from-17 rbuf rlimbs)
    (poly1305-diag-print-limbs 82 rlimbs) ;; R: clamped r limbs
    ;; Zero h
    (dotimes (i 20) (aset h i 0))
    ;; Process blocks
    (poly1305-diag-block1 h rlimbs blk nlimbs pmsg)
    (poly1305-diag-block2 h rlimbs blk nlimbs pmsg)
    (poly1305-diag-block3 h rlimbs blk nlimbs pmsg)
    (poly1305-diag-finish h pkey result)
    ;; Print final MAC
    (write-char-serial 80) (write-char-serial 58) ;; P:
    (dotimes (i 16) (print-hex-byte (aref result i)))
    (write-char-serial 10)))

;;; Split fe-mul: the original has let* with ~43 bindings which exceeds
;;; the x64 translator's 32 frame slot limit (352 byte frame). Split into
;;; helpers that each use < 20 frame slots.
;;;
;;; Strategy: store pre-computed values (f*2, g*19) into temp arrays,
;;; then each product function reads from those arrays. Max 3 args per call.

;; Pre-compute doubled f limbs and g*19 limbs into temp arrays
;; ff[0..9] = f[0..9], ff[10..14] = f1*2, f3*2, f5*2, f7*2, f9*2
;; gg[0..9] = g[0..9], gg[10..18] = g1*19, g2*19, ..., g9*19
(defun fe-mul-precomp-f (ff f)
  ;; Copy f limbs to ff[0..9]
  (buf-write-u32 ff 0 (buf-read-u32 f 0))
  (buf-write-u32 ff 4 (buf-read-u32 f 4))
  (buf-write-u32 ff 8 (buf-read-u32 f 8))
  (buf-write-u32 ff 12 (buf-read-u32 f 12))
  (buf-write-u32 ff 16 (buf-read-u32 f 16))
  (buf-write-u32 ff 20 (buf-read-u32 f 20))
  (buf-write-u32 ff 24 (buf-read-u32 f 24))
  (buf-write-u32 ff 28 (buf-read-u32 f 28))
  (buf-write-u32 ff 32 (buf-read-u32 f 32))
  (buf-write-u32 ff 36 (buf-read-u32 f 36))
  ;; ff[10..14] = f1*2, f3*2, f5*2, f7*2, f9*2
  (buf-write-u32 ff 40 (* 2 (buf-read-u32 f 4)))
  (buf-write-u32 ff 44 (* 2 (buf-read-u32 f 12)))
  (buf-write-u32 ff 48 (* 2 (buf-read-u32 f 20)))
  (buf-write-u32 ff 52 (* 2 (buf-read-u32 f 28)))
  (buf-write-u32 ff 56 (* 2 (buf-read-u32 f 36)))
  0)

(defun fe-mul-precomp-g (gg g)
  ;; Copy g limbs to gg[0..9]
  (buf-write-u32 gg 0 (buf-read-u32 g 0))
  (buf-write-u32 gg 4 (buf-read-u32 g 4))
  (buf-write-u32 gg 8 (buf-read-u32 g 8))
  (buf-write-u32 gg 12 (buf-read-u32 g 12))
  (buf-write-u32 gg 16 (buf-read-u32 g 16))
  (buf-write-u32 gg 20 (buf-read-u32 g 20))
  (buf-write-u32 gg 24 (buf-read-u32 g 24))
  (buf-write-u32 gg 28 (buf-read-u32 g 28))
  (buf-write-u32 gg 32 (buf-read-u32 g 32))
  (buf-write-u32 gg 36 (buf-read-u32 g 36))
  ;; gg[10..18] = g1*19, g2*19, ..., g9*19
  (buf-write-u32 gg 40 (* 19 (buf-read-u32 g 4)))
  (buf-write-u32 gg 44 (* 19 (buf-read-u32 g 8)))
  (buf-write-u32 gg 48 (* 19 (buf-read-u32 g 12)))
  (buf-write-u32 gg 52 (* 19 (buf-read-u32 g 16)))
  (buf-write-u32 gg 56 (* 19 (buf-read-u32 g 20)))
  (buf-write-u32 gg 60 (* 19 (buf-read-u32 g 24)))
  (buf-write-u32 gg 64 (* 19 (buf-read-u32 g 28)))
  (buf-write-u32 gg 68 (* 19 (buf-read-u32 g 32)))
  (buf-write-u32 gg 72 (* 19 (buf-read-u32 g 36)))
  0)

;; Compute one h product: h[n] = sum of 10 f*g terms
;; ff layout: [f0..f9, f1*2, f3*2, f5*2, f7*2, f9*2]
;; gg layout: [g0..g9, g1*19, g2*19, ..., g9*19]
;; Even h (h0,h2,h4,h6,h8): use f1-2,f3-2,f5-2,f7-2,f9-2
;; Odd h (h1,h3,h5,h7,h9): use plain f values

;; Helper: read ff[i] — plain f for index 0-9, doubled f for 10-14
(defun ff-read (ff i)
  (buf-read-u32 ff (* i 4)))

;; h0 = f0*g0 + f1_2*g9_19 + f2*g8_19 + f3_2*g7_19 + f4*g6_19
;;     + f5_2*g5_19 + f6*g4_19 + f7_2*g3_19 + f8*g2_19 + f9_2*g1_19
;; NOTE: h values are 57+ bit intermediates — must use aset (tagged 63-bit),
;; NOT buf-write-u32 (truncates to 32 bits, destroying upper bits needed for carry).
(defun fe-mul-h0 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 0))))
    (let ((p1 (* (ff-read ff 10) (ff-read gg 18))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 17))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 11) (ff-read gg 16))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 15))))
              (let ((p5 (* (ff-read ff 12) (ff-read gg 14))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 13))))
                    (let ((p7 (* (ff-read ff 13) (ff-read gg 12))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 11))))
                        (let ((p9 (* (ff-read ff 14) (ff-read gg 10))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 0 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h1 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 1))))
    (let ((p1 (* (ff-read ff 1) (ff-read gg 0))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 18))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 3) (ff-read gg 17))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 16))))
              (let ((p5 (* (ff-read ff 5) (ff-read gg 15))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 14))))
                    (let ((p7 (* (ff-read ff 7) (ff-read gg 13))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 12))))
                        (let ((p9 (* (ff-read ff 9) (ff-read gg 11))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 1 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h2 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 2))))
    (let ((p1 (* (ff-read ff 10) (ff-read gg 1))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 0))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 11) (ff-read gg 18))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 17))))
              (let ((p5 (* (ff-read ff 12) (ff-read gg 16))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 15))))
                    (let ((p7 (* (ff-read ff 13) (ff-read gg 14))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 13))))
                        (let ((p9 (* (ff-read ff 14) (ff-read gg 12))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 2 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h3 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 3))))
    (let ((p1 (* (ff-read ff 1) (ff-read gg 2))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 1))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 3) (ff-read gg 0))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 18))))
              (let ((p5 (* (ff-read ff 5) (ff-read gg 17))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 16))))
                    (let ((p7 (* (ff-read ff 7) (ff-read gg 15))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 14))))
                        (let ((p9 (* (ff-read ff 9) (ff-read gg 13))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 3 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h4 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 4))))
    (let ((p1 (* (ff-read ff 10) (ff-read gg 3))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 2))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 11) (ff-read gg 1))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 0))))
              (let ((p5 (* (ff-read ff 12) (ff-read gg 18))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 17))))
                    (let ((p7 (* (ff-read ff 13) (ff-read gg 16))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 15))))
                        (let ((p9 (* (ff-read ff 14) (ff-read gg 14))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 4 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h5 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 5))))
    (let ((p1 (* (ff-read ff 1) (ff-read gg 4))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 3))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 3) (ff-read gg 2))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 1))))
              (let ((p5 (* (ff-read ff 5) (ff-read gg 0))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 18))))
                    (let ((p7 (* (ff-read ff 7) (ff-read gg 17))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 16))))
                        (let ((p9 (* (ff-read ff 9) (ff-read gg 15))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 5 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h6 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 6))))
    (let ((p1 (* (ff-read ff 10) (ff-read gg 5))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 4))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 11) (ff-read gg 3))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 2))))
              (let ((p5 (* (ff-read ff 12) (ff-read gg 1))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 0))))
                    (let ((p7 (* (ff-read ff 13) (ff-read gg 18))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 17))))
                        (let ((p9 (* (ff-read ff 14) (ff-read gg 16))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 6 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h7 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 7))))
    (let ((p1 (* (ff-read ff 1) (ff-read gg 6))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 5))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 3) (ff-read gg 4))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 3))))
              (let ((p5 (* (ff-read ff 5) (ff-read gg 2))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 1))))
                    (let ((p7 (* (ff-read ff 7) (ff-read gg 0))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 18))))
                        (let ((p9 (* (ff-read ff 9) (ff-read gg 17))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 7 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h8 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 8))))
    (let ((p1 (* (ff-read ff 10) (ff-read gg 7))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 6))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 11) (ff-read gg 5))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 4))))
              (let ((p5 (* (ff-read ff 12) (ff-read gg 3))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 2))))
                    (let ((p7 (* (ff-read ff 13) (ff-read gg 1))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 0))))
                        (let ((p9 (* (ff-read ff 14) (ff-read gg 18))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 8 (+ a (+ b c)))))))))))))))))

(defun fe-mul-h9 (h ff gg)
  (let ((p0 (* (ff-read ff 0) (ff-read gg 9))))
    (let ((p1 (* (ff-read ff 1) (ff-read gg 8))))
      (let ((p2 (* (ff-read ff 2) (ff-read gg 7))))
        (let ((a (+ p0 (+ p1 p2))))
          (let ((p3 (* (ff-read ff 3) (ff-read gg 6))))
            (let ((p4 (* (ff-read ff 4) (ff-read gg 5))))
              (let ((p5 (* (ff-read ff 5) (ff-read gg 4))))
                (let ((b (+ p3 (+ p4 p5))))
                  (let ((p6 (* (ff-read ff 6) (ff-read gg 3))))
                    (let ((p7 (* (ff-read ff 7) (ff-read gg 2))))
                      (let ((p8 (* (ff-read ff 8) (ff-read gg 1))))
                        (let ((p9 (* (ff-read ff 9) (ff-read gg 0))))
                          (let ((c (+ p6 (+ p7 (+ p8 p9)))))
                            (aset h 9 (+ a (+ b c)))))))))))))))))

;; Compute all 10 products (split into two halves to stay under 12 forms each)
(defun fe-mul-lo (h ff gg)
  (fe-mul-h0 h ff gg)
  (fe-mul-h1 h ff gg)
  (fe-mul-h2 h ff gg)
  (fe-mul-h3 h ff gg)
  (fe-mul-h4 h ff gg)
  0)

(defun fe-mul-hi (h ff gg)
  (fe-mul-h5 h ff gg)
  (fe-mul-h6 h ff gg)
  (fe-mul-h7 h ff gg)
  (fe-mul-h8 h ff gg)
  (fe-mul-h9 h ff gg)
  0)

;; Stage 2a: carry propagation c0-c4 in-place on h array (tagged 63-bit values)
(defun fe-mul-carry-lo (h)
  ;; c0
  (let ((h0 (aref h 0)))
    (let ((c0 (ash h0 -26)))
      (aset h 0 (logand h0 #x3FFFFFF))
      (aset h 1 (+ (aref h 1) c0))))
  ;; c1
  (let ((h1 (aref h 1)))
    (let ((c1 (ash h1 -25)))
      (aset h 1 (logand h1 #x1FFFFFF))
      (aset h 2 (+ (aref h 2) c1))))
  ;; c2
  (let ((h2 (aref h 2)))
    (let ((c2 (ash h2 -26)))
      (aset h 2 (logand h2 #x3FFFFFF))
      (aset h 3 (+ (aref h 3) c2))))
  ;; c3
  (let ((h3 (aref h 3)))
    (let ((c3 (ash h3 -25)))
      (aset h 3 (logand h3 #x1FFFFFF))
      (aset h 4 (+ (aref h 4) c3))))
  ;; c4
  (let ((h4 (aref h 4)))
    (let ((c4 (ash h4 -26)))
      (aset h 4 (logand h4 #x3FFFFFF))
      (aset h 5 (+ (aref h 5) c4))))
  0)

;; Stage 2b: carry c5-c9, wrap c9*19 back to h0, copy to dst
(defun fe-mul-carry-hi (dst h)
  ;; c5
  (let ((h5 (aref h 5)))
    (let ((c5 (ash h5 -25)))
      (aset h 5 (logand h5 #x1FFFFFF))
      (aset h 6 (+ (aref h 6) c5))))
  ;; c6
  (let ((h6 (aref h 6)))
    (let ((c6 (ash h6 -26)))
      (aset h 6 (logand h6 #x3FFFFFF))
      (aset h 7 (+ (aref h 7) c6))))
  ;; c7
  (let ((h7 (aref h 7)))
    (let ((c7 (ash h7 -25)))
      (aset h 7 (logand h7 #x1FFFFFF))
      (aset h 8 (+ (aref h 8) c7))))
  ;; c8
  (let ((h8 (aref h 8)))
    (let ((c8 (ash h8 -26)))
      (aset h 8 (logand h8 #x3FFFFFF))
      (aset h 9 (+ (aref h 9) c8))))
  ;; c9 → wraps back to h0 with *19
  (let ((h9 (aref h 9)))
    (let ((c9 (ash h9 -25)))
      (aset h 9 (logand h9 #x1FFFFFF))
      (let ((r0w (+ (aref h 0) (* c9 19))))
        (let ((c0b (ash r0w -26)))
          (aset h 0 (logand r0w #x3FFFFFF))
          (aset h 1 (+ (aref h 1) c0b))))))
  ;; Copy h to dst (h is tagged array, dst is byte array)
  (fe-mul-copy dst h)
  dst)

;; Copy 10 limbs from h (tagged array) to dst (byte array via buf-write-u32)
(defun fe-mul-copy (dst h)
  (buf-write-u32 dst 0 (aref h 0))
  (buf-write-u32 dst 4 (aref h 1))
  (buf-write-u32 dst 8 (aref h 2))
  (buf-write-u32 dst 12 (aref h 3))
  (buf-write-u32 dst 16 (aref h 4))
  (buf-write-u32 dst 20 (aref h 5))
  (buf-write-u32 dst 24 (aref h 6))
  (buf-write-u32 dst 28 (aref h 7))
  (buf-write-u32 dst 32 (aref h 8))
  (buf-write-u32 dst 36 (aref h 9))
  0)

;; Split fe-mul for 64-bit (avoids frame slot overflow from 43+ let* bindings)
;; h is a 10-element tagged array (not byte array!) because intermediate h values
;; are 57+ bit sums that don't fit in buf-write-u32's 32 bits.
(defun fe-mul-split (dst f g)
  (let ((ff (make-array 60)))
    (let ((gg (make-array 76)))
      (let ((h (make-array 10)))
        (dotimes (i 10) (aset h i 0))
        (fe-mul-precomp-f ff f)
        (fe-mul-precomp-g gg g)
        (fe-mul-lo h ff gg)
        (fe-mul-hi h ff gg)
        (fe-mul-carry-lo h)
        (fe-mul-carry-hi dst h))))
  dst)

;; Top-level fe-mul: dispatch to 32-bit pair arithmetic or 64-bit split
;; Must save args to let bindings before mem-ref (ARM32 VR=V0 aliasing)
(defun fe-mul (dst f g)
  (let ((d dst))
    (let ((ff f))
      (let ((gg g))
        (if (>= (mem-ref #x50006D :u8) 1)
            (c32-fe-mul d ff gg)
            (fe-mul-split d ff gg))))))

;;; Split ed-add: original has 20 let bindings (over 18 limit) + 15 forms.
;;; Split into init (extract point components) and compute (arithmetic).
;;; ed-add: restructured so helpers take ≤4 args (avoid >4 arg overflow bug).
(defun ed-add (p q)
  (ed-add-compute p q))

(defun ed-add-compute (p q)
  (let ((x1 (car (car p))) (y1 (cdr (car p)))
        (z1 (car (cdr p))) (t1 (cdr (cdr p))))
    (let ((x2 (car (car q))) (y2 (cdr (car q)))
          (z2 (car (cdr q))) (t2 (cdr (cdr q))))
      (let ((w0 (make-array 40)) (w1 (make-array 40))
            (w2 (make-array 40)) (w3 (make-array 40)))
        (let ((w4 (make-array 40)) (w5 (make-array 40)))
          ;; A = (Y1-X1)*(Y2-X2)
          (fe-sub w0 y1 x1) (fe-sub w1 y2 x2)
          (fe-mul w2 w0 w1)
          ;; B = (Y1+X1)*(Y2+X2)
          (fe-add w0 y1 x1) (fe-add w1 y2 x2)
          (fe-mul w3 w0 w1)
          ;; C = T1 * 2*d * T2
          (fe-load-fixed w0 (+ (e1000-state-base) #x500))
          (fe-add w0 w0 w0)
          (fe-mul w0 t1 w0) (fe-mul w4 w0 t2)
          ;; D = 2 * Z1 * Z2
          (fe-mul w5 z1 z2) (fe-add w5 w5 w5)
          (ed-add-finish w2 w3 w4 w5))))))

(defun ed-add-finish (w2 w3 w4 w5)
  (let ((w6 (make-array 40)) (w7 (make-array 40)))
    (let ((rx (make-array 40)) (ry (make-array 40))
          (rz (make-array 40)) (rt (make-array 40)))
      ;; E = B - A, F = D - C, G = D + C, H = B + A
      (fe-sub w6 w3 w2) (fe-sub w7 w5 w4)
      (let ((w0g (make-array 40)) (w1h (make-array 40)))
        (fe-add w0g w5 w4) (fe-add w1h w3 w2)
        ;; X3=E*F, Y3=G*H, T3=E*H, Z3=F*G
        (fe-mul rx w6 w7) (fe-mul ry w0g w1h)
        (fe-mul rt w6 w1h) (fe-mul rz w7 w0g)
        (cons (cons rx ry) (cons rz rt))))))

;;; Override ed-double: original has 13 let bindings + 14 body forms = 27 forms.
;;; Exceeds ~25 sequential forms limit. Split into two helpers.
(defun ed-double-ab (x1 y1 z1 out)
  ;; out = [a, b, c, dd, e] as 5x40-byte regions in a 200-byte array
  ;; A = X1^2
  (let ((a (make-array 40)) (b (make-array 40)))
    (fe-sq a x1)
    (fe-sq b y1)
    (let ((c (make-array 40)))
      (fe-sq c z1)
      (fe-add c c c)
      (let ((dd (make-array 40)) (e (make-array 40)))
        (fe-sub dd (fe-from-int 0) a)
        (fe-add e x1 y1)
        (fe-sq e e)
        (fe-sub e e a)
        (fe-sub e e b)
        ;; Return (a . (b . (c . (dd . e))))
        (cons a (cons b (cons c (cons dd e))))))))

(defun ed-double-finish (pack)
  (let ((a (car pack)) (b (car (cdr pack)))
        (c (car (cdr (cdr pack)))))
    (let ((dd (car (cdr (cdr (cdr pack)))))
          (e (cdr (cdr (cdr (cdr pack))))))
      (let ((g (make-array 40)) (f (make-array 40))
            (h (make-array 40)))
        (fe-add g dd b)
        (fe-sub f g c)
        (fe-sub h dd b)
        (let ((rx (make-array 40)) (ry (make-array 40))
              (rz (make-array 40)) (rt (make-array 40)))
          (fe-mul rx e f)
          (fe-mul ry g h)
          (fe-mul rt e h)
          (fe-mul rz f g)
          (cons (cons rx ry) (cons rz rt)))))))

(defun ed-double (p)
  (let ((x1 (car (car p)))
        (y1 (cdr (car p)))
        (z1 (car (cdr p))))
    (let ((pack (ed-double-ab x1 y1 z1 0)))
      (ed-double-finish pack))))

;;; Override ed-scalar-mult: pass scalar via cons to avoid register clobber
(defun ed-scalar-mult-byte (result temp scalar byte-idx)
  ;; Process 8 bits from scalar[byte-idx]
  (let ((byte-val (aref scalar byte-idx)))
    (let ((b0 (logand byte-val 1)))
      (let ((r0 (if (not (zerop b0)) (ed-add result temp) result)))
        (let ((t0 (ed-double temp)))
          (let ((b1 (logand byte-val 2)))
            (let ((r1 (if (not (zerop b1)) (ed-add r0 t0) r0)))
              (let ((t1 (ed-double t0)))
                (let ((b2 (logand byte-val 4)))
                  (let ((r2 (if (not (zerop b2)) (ed-add r1 t1) r1)))
                    (let ((t2 (ed-double t1)))
                      (let ((b3 (logand byte-val 8)))
                        (let ((r3 (if (not (zerop b3)) (ed-add r2 t2) r2)))
                          (let ((t3 (ed-double t2)))
                            (ed-scalar-mult-byte2 r3 t3 byte-val)))))))))))))))

(defun ed-scalar-mult-byte2 (result temp byte-val)
  (let ((b4 (logand byte-val 16)))
    (let ((r4 (if (not (zerop b4)) (ed-add result temp) result)))
      (let ((t4 (ed-double temp)))
        (let ((b5 (logand byte-val 32)))
          (let ((r5 (if (not (zerop b5)) (ed-add r4 t4) r4)))
            (let ((t5 (ed-double t4)))
              (let ((b6 (logand byte-val 64)))
                (let ((r6 (if (not (zerop b6)) (ed-add r5 t5) r5)))
                  (let ((t6 (ed-double t5)))
                    (let ((b7 (logand byte-val 128)))
                      (let ((r7 (if (not (zerop b7)) (ed-add r6 t6) r6)))
                        (let ((t7 (ed-double t6)))
                          (cons r7 t7))))))))))))))

(defun ed-scalar-mult-loop (result temp scalar i)
  ;; Process bytes 0..31
  (if (>= i 32)
      result
      (let ((pair (ed-scalar-mult-byte result temp scalar i)))
        (ed-scalar-mult-loop (car pair) (cdr pair) scalar (+ i 1)))))

(defun ed-scalar-mult (scalar point)
  ;; On 32-bit: use arena-based c32-ed-scalar-mult (resets alloc per byte).
  ;; Without arena, pair arithmetic in fe-mul creates ~50KB garbage per byte
  ;; iteration that never gets freed, causing heap exhaustion / massive slowdown.
  ;; On 64-bit: use recursive loop (plenty of heap, no arena needed).
  ;; Must save args to let bindings before mem-ref (ARM32 VR=V0 aliasing)
  (let ((s scalar))
    (let ((p point))
      (if (>= (mem-ref #x50006D :u8) 1)
          (c32-ed-scalar-mult s p)
          (let ((result (cons (cons (fe-from-int 0) (fe-from-int 1))
                              (cons (fe-from-int 1) (fe-from-int 0)))))
            (ed-scalar-mult-loop result p s 0))))))

;;; Split fe-carry: original has let* with 32 bindings + 10 buf-write-u32 = 42 forms.
;;; Reuse the in-place carry pattern from fe-mul-carry.
;; fe-carry operates on byte arrays (post-carry 26/25-bit limbs fit in 32 bits)
;; Uses buf-read-u32/buf-write-u32 directly (NOT fe-mul-carry-lo which uses aref/aset)
(defun fe-carry (h)
  (fe-carry-lo h)
  (fe-carry-hi h)
  h)

(defun fe-carry-lo (h)
  (let ((h0 (buf-read-u32 h 0)))
    (let ((c0 (ash h0 -26)))
      (buf-write-u32 h 0 (logand h0 #x3FFFFFF))
      (buf-write-u32 h 4 (+ (buf-read-u32 h 4) c0))))
  (let ((h1 (buf-read-u32 h 4)))
    (let ((c1 (ash h1 -25)))
      (buf-write-u32 h 4 (logand h1 #x1FFFFFF))
      (buf-write-u32 h 8 (+ (buf-read-u32 h 8) c1))))
  (let ((h2 (buf-read-u32 h 8)))
    (let ((c2 (ash h2 -26)))
      (buf-write-u32 h 8 (logand h2 #x3FFFFFF))
      (buf-write-u32 h 12 (+ (buf-read-u32 h 12) c2))))
  (let ((h3 (buf-read-u32 h 12)))
    (let ((c3 (ash h3 -25)))
      (buf-write-u32 h 12 (logand h3 #x1FFFFFF))
      (buf-write-u32 h 16 (+ (buf-read-u32 h 16) c3))))
  (let ((h4 (buf-read-u32 h 16)))
    (let ((c4 (ash h4 -26)))
      (buf-write-u32 h 16 (logand h4 #x3FFFFFF))
      (buf-write-u32 h 20 (+ (buf-read-u32 h 20) c4))))
  0)

(defun fe-carry-hi (h)
  (let ((h5 (buf-read-u32 h 20)))
    (let ((c5 (ash h5 -25)))
      (buf-write-u32 h 20 (logand h5 #x1FFFFFF))
      (buf-write-u32 h 24 (+ (buf-read-u32 h 24) c5))))
  (let ((h6 (buf-read-u32 h 24)))
    (let ((c6 (ash h6 -26)))
      (buf-write-u32 h 24 (logand h6 #x3FFFFFF))
      (buf-write-u32 h 28 (+ (buf-read-u32 h 28) c6))))
  (let ((h7 (buf-read-u32 h 28)))
    (let ((c7 (ash h7 -25)))
      (buf-write-u32 h 28 (logand h7 #x1FFFFFF))
      (buf-write-u32 h 32 (+ (buf-read-u32 h 32) c7))))
  (let ((h8 (buf-read-u32 h 32)))
    (let ((c8 (ash h8 -26)))
      (buf-write-u32 h 32 (logand h8 #x3FFFFFF))
      (buf-write-u32 h 36 (+ (buf-read-u32 h 36) c8))))
  ;; c9 wraps to h0 with *19
  (let ((h9 (buf-read-u32 h 36)))
    (let ((c9 (ash h9 -25)))
      (buf-write-u32 h 36 (logand h9 #x1FFFFFF))
      (let ((r0w (+ (buf-read-u32 h 0) (* c9 19))))
        (let ((c0b (ash r0w -26)))
          (buf-write-u32 h 0 (logand r0w #x3FFFFFF))
          (buf-write-u32 h 4 (+ (buf-read-u32 h 4) c0b))))))
  0)

;;; Split fe-reduce: original has let* with 30 bindings + when with 10 buf-write-u32.
;;; Do two fe-carry calls, then check+reduce in two halves.
(defun fe-reduce-check (h)
  ;; Check if h >= p by trying h + 19. Return carry out of limb 9.
  ;; Do this in-place on a copy to avoid modifying h prematurely.
  (let ((t0 (+ (buf-read-u32 h 0) 19)))
    (let ((c0 (ash t0 -26)))
      (let ((t1 (+ (buf-read-u32 h 4) c0)))
        (let ((c1 (ash t1 -25)))
          (let ((t2 (+ (buf-read-u32 h 8) c1)))
            (let ((c2 (ash t2 -26)))
              (fe-reduce-check2 h c2))))))))

(defun fe-reduce-check2 (h c2)
  (let ((t3 (+ (buf-read-u32 h 12) c2)))
    (let ((c3 (ash t3 -25)))
      (let ((t4 (+ (buf-read-u32 h 16) c3)))
        (let ((c4 (ash t4 -26)))
          (let ((t5 (+ (buf-read-u32 h 20) c4)))
            (let ((c5 (ash t5 -25)))
              (fe-reduce-check3 h c5))))))))

(defun fe-reduce-check3 (h c5)
  (let ((t6 (+ (buf-read-u32 h 24) c5)))
    (let ((c6 (ash t6 -26)))
      (let ((t7 (+ (buf-read-u32 h 28) c6)))
        (let ((c7 (ash t7 -25)))
          (let ((t8 (+ (buf-read-u32 h 32) c7)))
            (let ((c8 (ash t8 -26)))
              (let ((t9 (+ (buf-read-u32 h 36) c8)))
                (ash t9 -25)))))))))

(defun fe-reduce-apply (h)
  ;; If h >= p, subtract p by adding 19 in-place
  ;; This is safe because fe-carry already normalized the limbs
  (let ((t0 (+ (buf-read-u32 h 0) 19)))
    (let ((c0 (ash t0 -26)))
      (buf-write-u32 h 0 (logand t0 #x3FFFFFF))
      (let ((t1 (+ (buf-read-u32 h 4) c0)))
        (let ((c1 (ash t1 -25)))
          (buf-write-u32 h 4 (logand t1 #x1FFFFFF))
          (fe-reduce-apply2 h c1))))))

(defun fe-reduce-apply2 (h c1)
  (let ((t2 (+ (buf-read-u32 h 8) c1)))
    (let ((c2 (ash t2 -26)))
      (buf-write-u32 h 8 (logand t2 #x3FFFFFF))
      (let ((t3 (+ (buf-read-u32 h 12) c2)))
        (let ((c3 (ash t3 -25)))
          (buf-write-u32 h 12 (logand t3 #x1FFFFFF))
          (fe-reduce-apply3 h c3))))))

(defun fe-reduce-apply3 (h c3)
  (let ((t4 (+ (buf-read-u32 h 16) c3)))
    (let ((c4 (ash t4 -26)))
      (buf-write-u32 h 16 (logand t4 #x3FFFFFF))
      (let ((t5 (+ (buf-read-u32 h 20) c4)))
        (let ((c5 (ash t5 -25)))
          (buf-write-u32 h 20 (logand t5 #x1FFFFFF))
          (fe-reduce-apply4 h c5))))))

(defun fe-reduce-apply4 (h c5)
  (let ((t6 (+ (buf-read-u32 h 24) c5)))
    (let ((c6 (ash t6 -26)))
      (buf-write-u32 h 24 (logand t6 #x3FFFFFF))
      (let ((t7 (+ (buf-read-u32 h 28) c6)))
        (let ((c7 (ash t7 -25)))
          (buf-write-u32 h 28 (logand t7 #x1FFFFFF))
          (let ((t8 (+ (buf-read-u32 h 32) c7)))
            (let ((c8 (ash t8 -26)))
              (buf-write-u32 h 32 (logand t8 #x3FFFFFF))
              (let ((t9 (+ (buf-read-u32 h 36) c8)))
                (buf-write-u32 h 36 (logand t9 #x1FFFFFF)))))))))
  0)

(defun fe-reduce (h)
  (fe-carry h)
  (fe-carry h)
  (let ((c9 (fe-reduce-check h)))
    (when (not (zerop c9))
      (fe-reduce-apply h)))
  h)

;;; Split fe-to-bytes: original has 11 let bindings + 32 aset calls = 43 forms,
;;; well past the ~25 sequential forms limit.
(defun fe-to-bytes-lo (r fe)
  ;; Bytes 0-15 from limbs 0-4
  (let ((l0 (buf-read-u32 fe 0)) (l1 (buf-read-u32 fe 4))
        (l2 (buf-read-u32 fe 8)) (l3 (buf-read-u32 fe 12))
        (l4 (buf-read-u32 fe 16)))
    (aset r 0 (logand l0 255))
    (aset r 1 (logand (ash l0 -8) 255))
    (aset r 2 (logand (ash l0 -16) 255))
    (aset r 3 (logand (logior (ash l0 -24) (ash l1 2)) 255))
    (aset r 4 (logand (ash l1 -6) 255))
    (aset r 5 (logand (ash l1 -14) 255))
    (aset r 6 (logand (logior (ash l1 -22) (ash l2 3)) 255))
    (aset r 7 (logand (ash l2 -5) 255))
    (aset r 8 (logand (ash l2 -13) 255))
    (aset r 9 (logand (logior (ash l2 -21) (ash l3 5)) 255))
    (aset r 10 (logand (ash l3 -3) 255))
    (aset r 11 (logand (ash l3 -11) 255))
    (fe-to-bytes-lo2 r l3 l4))
  0)

(defun fe-to-bytes-lo2 (r l3 l4)
  ;; Pre-mask l4 to 2 bits: (ash l4 6) overflows 30-bit fixnums when l4 > 2^24
  (aset r 12 (logand (logior (ash l3 -19) (ash (logand l4 3) 6)) 255))
  (aset r 13 (logand (ash l4 -2) 255))
  (aset r 14 (logand (ash l4 -10) 255))
  (aset r 15 (logand (ash l4 -18) 255))
  0)

(defun fe-to-bytes-hi (r fe)
  ;; Bytes 16-31 from limbs 5-9
  (let ((l5 (buf-read-u32 fe 20)) (l6 (buf-read-u32 fe 24))
        (l7 (buf-read-u32 fe 28)) (l8 (buf-read-u32 fe 32))
        (l9 (buf-read-u32 fe 36)))
    (aset r 16 (logand l5 255))
    (aset r 17 (logand (ash l5 -8) 255))
    (aset r 18 (logand (ash l5 -16) 255))
    (aset r 19 (logand (logior (ash l5 -24) (ash l6 1)) 255))
    (aset r 20 (logand (ash l6 -7) 255))
    (aset r 21 (logand (ash l6 -15) 255))
    (aset r 22 (logand (logior (ash l6 -23) (ash l7 3)) 255))
    (aset r 23 (logand (ash l7 -5) 255))
    (aset r 24 (logand (ash l7 -13) 255))
    (aset r 25 (logand (logior (ash l7 -21) (ash l8 4)) 255))
    (aset r 26 (logand (ash l8 -4) 255))
    (aset r 27 (logand (ash l8 -12) 255))
    (fe-to-bytes-hi2 r l8 l9))
  0)

(defun fe-to-bytes-hi2 (r l8 l9)
  ;; Pre-mask l9 to 2 bits: (ash l9 6) overflows 30-bit fixnums when l9 > 2^24
  (aset r 28 (logand (logior (ash l8 -20) (ash (logand l9 3) 6)) 255))
  (aset r 29 (logand (ash l9 -2) 255))
  (aset r 30 (logand (ash l9 -10) 255))
  (aset r 31 (logand (ash l9 -18) 255))
  0)

(defun fe-to-bytes (fe)
  (fe-reduce fe)
  (let ((r (make-array 32)))
    (fe-to-bytes-lo r fe)
    (fe-to-bytes-hi r fe)
    r))

;;; Split fe-invert: original has 32 sequential forms (over ~25 limit).
;;; Split into two halves. Each helper takes a state array (1 arg) to avoid
;;; the >4 arg overflow calling convention which is broken in cross-compilation.
(defun fe-invert-lo (s)
  (let ((z (aref s 0)) (z2 (aref s 1)) (z9 (aref s 2))
        (z11 (aref s 3)) (t0 (aref s 4)) (t1 (aref s 5)))
    (fe-sq z2 z)
    (fe-sq t0 z2)
    (fe-sq t1 t0)
    (fe-mul z9 z t1)
    (fe-mul z11 z2 z9)
    (fe-sq t0 z11)
    (fe-mul t0 z9 t0)
    ;; z^(2^10-1)
    (fe-sq t1 t0) (fe-sq-iter t1 4)
    (fe-mul t1 t1 t0)
    ;; z^(2^20-1)
    (fe-sq z2 t1) (fe-sq-iter z2 9)
    (fe-mul z2 z2 t1)
    0))

(defun fe-invert-hi (s)
  (let ((z11 (aref s 3)) (z2 (aref s 1)) (z9 (aref s 2))
        (t0 (aref s 4)) (t1 (aref s 5)))
    ;; z^(2^40-1)
    (fe-sq z9 z2) (fe-sq-iter z9 19)
    (fe-mul z9 z9 z2)
    ;; z^(2^50-1)
    (fe-sq t0 z9) (fe-sq-iter t0 9)
    (fe-mul t0 t0 t1)
    ;; z^(2^100-1)
    (fe-sq t1 t0) (fe-sq-iter t1 49)
    (fe-mul t1 t1 t0)
    ;; z^(2^200-1)
    (fe-sq z2 t1) (fe-sq-iter z2 99)
    (fe-mul z2 z2 t1)
    ;; z^(2^250-1)
    (fe-sq z9 z2) (fe-sq-iter z9 49)
    (fe-mul z9 z9 t0)
    ;; z^(2^255-21)
    (fe-sq z9 z9) (fe-sq-iter z9 4)
    (fe-mul t0 z9 z11)
    0))

(defun fe-invert (z)
  (let ((s (make-array 6)))
    (let ((z2 (make-array 40)) (z9 (make-array 40))
          (z11 (make-array 40)) (t0 (make-array 40)) (t1 (make-array 40)))
      (aset s 0 z) (aset s 1 z2) (aset s 2 z9)
      (aset s 3 z11) (aset s 4 t0) (aset s 5 t1)
      (fe-invert-lo s)
      (fe-invert-hi s)
      t0)))

;;; Split x25519: uses state array to pass all work arrays (avoid >4 arg overflow bug).
;;; State layout: s[0]=kc s[1]=x1 s[2]=x2 s[3]=z2 s[4]=x3 s[5]=z3
;;;   s[6]=a s[7]=aa s[8]=b s[9]=bb s[10]=fe-e s[11]=c
;;;   s[12]=d s[13]=da s[14]=cb s[15]=t1 s[16]=t2 s[17]=a24
(defun x25519-alloc-state ()
  (let ((s (make-array 18)))
    (aset s 0 (make-array 32))
    (aset s 1 (make-array 40)) (aset s 2 (make-array 40))
    (aset s 3 (make-array 40)) (aset s 4 (make-array 40))
    (aset s 5 (make-array 40)) (aset s 6 (make-array 40))
    (aset s 7 (make-array 40)) (aset s 8 (make-array 40))
    (aset s 9 (make-array 40))
    s))

(defun x25519-alloc-state2 (s)
  (aset s 10 (make-array 40)) (aset s 11 (make-array 40))
  (aset s 12 (make-array 40)) (aset s 13 (make-array 40))
  (aset s 14 (make-array 40)) (aset s 15 (make-array 40))
  (aset s 16 (make-array 40)) (aset s 17 (make-array 40))
  0)

(defun x25519-init (s k u)
  (let ((kc (aref s 0)) (x1 (aref s 1)) (x2 (aref s 2))
        (z2 (aref s 3)) (x3 (aref s 4)) (z3 (aref s 5)))
    (let ((a24 (aref s 17)))
      ;; Clamp scalar
      (dotimes (i 32) (aset kc i (aref k i)))
      (aset kc 0 (logand (aref kc 0) 248))
      (aset kc 31 (logand (aref kc 31) 127))
      (aset kc 31 (logior (aref kc 31) 64))
      ;; Initialize field elements
      (fe-copy x1 (fe-from-bytes u))
      (dotimes (i 40) (aset x2 i 0))
      (buf-write-u32 x2 0 1)
      (dotimes (i 40) (aset z2 i 0))
      (fe-copy x3 x1)
      (dotimes (i 40) (aset z3 i 0))
      (buf-write-u32 z3 0 1)
      (dotimes (i 40) (aset a24 i 0))
      (buf-write-u32 a24 0 121665)
      0)))

(defun x25519-step-diag (pos byte-idx bit-idx byte-val mask result)
  (write-char-serial 68) ;; D
  (print-dec pos)
  (write-char-serial 44)
  (print-dec byte-idx)
  (write-char-serial 44)
  (print-dec bit-idx)
  (write-char-serial 44)
  (print-dec byte-val)
  (write-char-serial 44)
  (print-dec mask)
  (write-char-serial 44)
  (print-dec result)
  (write-char-serial 10))

(defun x25519-step (s swap pos)
  ;; One ladder step
  (let ((kc (aref s 0)) (x2 (aref s 2)) (z2 (aref s 3))
        (x3 (aref s 4)) (z3 (aref s 5)) (t1 (aref s 15)))
    (let ((byte-idx (ash pos -3)))
      (let ((bit-idx (logand pos 7)))
        (let ((byte-val (aref kc byte-idx)))
          (let ((mask (ash 1 bit-idx)))
            (let ((and-result (logand byte-val mask)))
              (let ((kt (if (zerop and-result) 0 1)))
                (when (not (= kt swap))
                  (fe-copy t1 x2) (fe-copy x2 x3) (fe-copy x3 t1)
                  (fe-copy t1 z2) (fe-copy z2 z3) (fe-copy z3 t1))
                (x25519-step2 s)
                kt))))))))

(defun x25519-step2-pre-diag (s cnt)
  ;; Print x2[0],z2[0] BEFORE step2 runs (after swap in step)
  (when (< cnt 1)
    (let ((x2 (aref s 2)) (z2 (aref s 3)) (x3 (aref s 4)) (z3 (aref s 5)))
      (write-char-serial 80) ;; P
      (print-dec (buf-read-u32 x2 0))
      (write-char-serial 44)
      (print-dec (buf-read-u32 z2 0))
      (write-char-serial 44)
      (print-dec (buf-read-u32 x3 0))
      (write-char-serial 44)
      (print-dec (buf-read-u32 z3 0))
      (write-char-serial 10))))

(defun x25519-step2 (s)
  (let ((x1 (aref s 1)) (x2 (aref s 2)) (z2 (aref s 3))
        (x3 (aref s 4)) (z3 (aref s 5)) (a (aref s 6))
        (aa (aref s 7)) (b (aref s 8)))
    (let ((bb (aref s 9)) (fe-e (aref s 10)) (c (aref s 11))
          (d (aref s 12)) (da (aref s 13)) (cb (aref s 14))
          (t1 (aref s 15)) (a24 (aref s 17)))
      (fe-add a x2 z2)
      (fe-sq aa a)
      (fe-sub b x2 z2)
      (fe-sq bb b)
      (fe-sub fe-e aa bb)
      (fe-add c x3 z3)
      (fe-sub d x3 z3)
      (fe-mul da d a)
      (fe-mul cb c b)
      (fe-add t1 da cb)
      (fe-sq x3 t1)
      (x25519-step3 s)
      0)))

(defun x25519-step3-diag (s cnt)
  ;; Print aa[0], bb[0], fe-e[0] for first 3 iterations
  (when (< cnt 3)
    (let ((aa (aref s 7)) (bb (aref s 9)) (fe-e (aref s 10)))
      (write-char-serial 83) ;; S
      (print-dec cnt)
      (write-char-serial 58)
      (print-dec (buf-read-u32 aa 0))
      (write-char-serial 44)
      (print-dec (buf-read-u32 bb 0))
      (write-char-serial 44)
      (print-dec (buf-read-u32 fe-e 0))
      (write-char-serial 10))))

(defun x25519-step3 (s)
  (let ((x1 (aref s 1)) (x2 (aref s 2)) (z2 (aref s 3))
        (z3 (aref s 5)) (t1 (aref s 15)) (t2 (aref s 16)))
    (let ((da (aref s 13)) (cb (aref s 14)) (fe-e (aref s 10))
          (aa (aref s 7)) (a24 (aref s 17)) (bb (aref s 9)))
      (fe-sub t1 da cb)
      (fe-sq t2 t1)
      (fe-mul z3 x1 t2)
      (fe-mul x2 aa bb)
      (fe-mul t1 a24 fe-e)
      (fe-add t2 aa t1)
      (fe-mul z2 fe-e t2)
      0)))

(defun x25519 (k u)
  ;; On 32-bit: use arena-based c32-x25519 (resets alloc per iteration).
  ;; On 64-bit: use split state-array version (avoids >4 arg overflow).
  ;; Must save args to let bindings before mem-ref (ARM32 VR=V0 aliasing)
  (let ((kk k))
    (let ((uu u))
      (if (>= (mem-ref #x50006D :u8) 1)
          (c32-x25519 kk uu)
          (let ((s (x25519-alloc-state)))
            (x25519-alloc-state2 s)
            (x25519-init s kk uu)
            (x25519-ladder s)
            (let ((t1 (aref s 15)) (x2 (aref s 2)) (z2 (aref s 3)))
              (fe-mul t1 x2 (fe-invert z2))
              (fe-to-bytes t1)))))))

(defun x25519-ladder (s)
  (let ((x2 (aref s 2)) (z2 (aref s 3)) (x3 (aref s 4))
        (z3 (aref s 5)) (t1 (aref s 15)))
    (let ((swap 0) (pos 254))
      (dotimes (iter 255)
        (let ((kt (x25519-step s swap pos)))
          (setq swap kt))
        (setq pos (- pos 1))
        (when (zerop (logand iter 31)) (usb-keepalive)))
      ;; Final swap
      (when (not (zerop swap))
        (fe-copy t1 x2) (fe-copy x2 x3) (fe-copy x3 t1)
        (fe-copy t1 z2) (fe-copy z2 z3) (fe-copy z3 t1))))
  0)


;;; ARM32 register clobber fix: (aset arr i (aref/mem-ref (+ base i) ...))
;;; clobbers index register on ARM32. All copies must use intermediate let bindings.

(defun le-u32 (b0 b1 b2 b3)
  (let ((lo (logior b0 (ash b1 8))))
    (let ((hi (logior (ash b2 16) (ash b3 24))))
      (logior lo hi))))

;;; Safe chacha-setup: override dispatch wrapper (last-defun-wins).
;;; Original uses nested (logior b0 (logior (ash b1 8) (logior (ash b2 16) (ash b3 24))))
;;; which clobbers registers on bare metal. Use le-u32 helper instead.
(defun chacha-setup-load-key (s key)
  (dotimes (i 8)
    (let ((j (* i 4)))
      (let ((b0 (aref key j)))
        (let ((b1 (aref key (+ j 1))))
          (let ((b2 (aref key (+ j 2))))
            (let ((b3 (aref key (+ j 3))))
              (let ((val (le-u32 b0 b1 b2 b3)))
                (let ((soff (+ 16 j)))
                  (buf-write-u32 s soff val))))))))))
(defun chacha-setup-load-nonce (s nonce)
  (dotimes (i 3)
    (let ((j (* i 4)))
      (let ((b0 (aref nonce j)))
        (let ((b1 (aref nonce (+ j 1))))
          (let ((b2 (aref nonce (+ j 2))))
            (let ((b3 (aref nonce (+ j 3))))
              (let ((val (le-u32 b0 b1 b2 b3)))
                (let ((soff (+ 52 j)))
                  (buf-write-u32 s soff val))))))))))
(defun c64-chacha-setup (key nonce counter)
  (let ((s (make-array 64)))
    (buf-write-u32 s 0 #x61707865)
    (buf-write-u32 s 4 #x3320646e)
    (buf-write-u32 s 8 #x79622d32)
    (buf-write-u32 s 12 #x6b206574)
    (chacha-setup-load-key s key)
    (buf-write-u32 s 48 counter)
    (chacha-setup-load-nonce s nonce)
    s))

;;; Safe chacha-block: override to use safe output writing
;;; (avoids variable-index aset with computed value bug and nested logior clobber)
(defun chacha-block-write-word (out off sum)
  ;; Write LE bytes of sum at out[off..off+3]
  (let ((v0 (logand sum #xFF)))
    (let ((v1 (logand (ash sum -8) #xFF)))
      (let ((v2 (logand (ash sum -16) #xFF)))
        (let ((v3 (logand (ash sum -24) #xFF)))
          (buf-write-u32-helper out off v0 v1 v2 v3))))))

(defun chacha-block-output (out work state)
  (dotimes (i 16)
    (let ((off (* i 4)))
      (let ((wv (buf-read-u32 work off)))
        (let ((sv (buf-read-u32 state off)))
          (let ((sum (logand (+ wv sv) #xFFFFFFFF)))
            (chacha-block-write-word out off sum)))))))

(defun c64-chacha-block (key nonce counter)
  (let ((state (chacha-setup key nonce counter))
        (work (make-array 64)))
    (dotimes (i 64)
      (aset work i (aref state i)))
    (chacha-inner work)
    (let ((out (make-array 64)))
      (chacha-block-output out work state)
      out)))

(defun test-chacha-block-rfc ()
  ;; RFC 8439 Section 2.3.2: key=00..1f, nonce=000000090000004a00000000, counter=1
  ;; Expected first 4 output bytes: 10 F1 E7 E4
  (let ((ck (make-array 32))
        (cn (make-array 12)))
    (dotimes (ci 32) (aset ck ci ci))
    (aset cn 3 #x09)
    (aset cn 7 #x4a)
    (let ((cks (chacha-block ck cn 1)))
      (write-char-serial 67) (write-char-serial 84) (write-char-serial 61) ;; CT=
      (print-hex-byte (aref cks 0))
      (print-hex-byte (aref cks 1))
      (print-hex-byte (aref cks 2))
      (print-hex-byte (aref cks 3))
      (write-char-serial 10))))

(defun safe-copy-mem-to-arr (arr base len)
  ;; Copy mem[base+0..base+len-1] to arr[0..len-1]
  (let ((j 0))
    (loop
      (when (>= j len) (return 0))
      (let ((addr (+ base j)))
        (let ((val (mem-ref addr :u8)))
          (aset arr j val)))
      (setq j (+ j 1)))))

;;; Safe chacha20-crypt override: avoids (aset result (+ base i) (logxor (aref data (+ base i)) (aref ks i)))
;;; register clobber on ARM32.
(defun chacha20-crypt-block (result data ks base data-len)
  (let ((i 0))
    (loop
      (when (>= i 64) (return 0))
      (let ((idx (+ base i)))
        (when (< idx data-len)
          (let ((dv (aref data idx)))
            (let ((kv (aref ks i)))
              (let ((xv (logxor dv kv)))
                (aset result idx xv))))))
      (setq i (+ i 1)))))

(defun chacha20-crypt (key nonce data data-len counter)
  (let ((result (make-array data-len))
        (block-num 0))
    (loop
      (when (not (< (* block-num 64) data-len)) (return ()))
      (let ((ks (chacha-block key nonce (+ counter block-num))))
        (let ((base (* block-num 64)))
          (chacha20-crypt-block result data ks base data-len)))
      (setq block-num (+ block-num 1)))
    result))

;;; Safe poly1305 override: avoids (aset x i (aref y (+ off i))) register clobber
;;; on ARM32. The bug: aref's value expression clobbers the aset index register.
(defun poly-safe-copy (dst src offset len)
  ;; Copy src[offset..offset+len-1] to dst[0..len-1]
  ;; Uses let binding to avoid register clobber
  (let ((j 0))
    (loop
      (when (>= j len) (return 0))
      (let ((idx (+ offset j)))
        (let ((val (aref src idx)))
          (aset dst j val)))
      (setq j (+ j 1)))))

(defun poly1305-process-block (h rlimbs blk nlimbs msg offset blen)
  ;; Zero blk, copy blen bytes from msg[offset..], set pad, convert, add, mul
  (dotimes (k 17) (aset blk k 0))
  (poly-safe-copy blk msg offset blen)
  (aset blk blen 1)
  (poly-from-17 blk nlimbs)
  (poly-add-limbs h nlimbs)
  (poly-mul h rlimbs))

(defun poly1305-add-s (result key)
  ;; Add s (key bytes 16..31) mod 2^128
  (let ((carry 0))
    (dotimes (i 16)
      (let ((ri (aref result i)))
        (let ((idx (+ 16 i)))
          (let ((ki (aref key idx)))
            (let ((sum (+ ri (+ ki carry))))
              (aset result i (logand sum #xFF))
              (setq carry (ash sum -8)))))))))

(defun poly1305 (key msg msg-len)
  (let ((rbuf (make-array 17))
        (rlimbs (make-array 20))
        (h (make-array 20))
        (blk (make-array 17))
        (nlimbs (make-array 20))
        (result (make-array 16)))
    (poly-safe-copy rbuf key 0 16)
    (aset rbuf 16 0)
    (poly-clamp rbuf)
    (poly-from-17 rbuf rlimbs)
    (dotimes (i 20) (aset h i 0))
    (poly1305-loop h rlimbs blk nlimbs msg msg-len)
    (poly-reduce h)
    (poly-to-16 h result)
    (poly1305-add-s result key)
    result))

(defun poly1305-loop (h rlimbs blk nlimbs msg msg-len)
  (let ((offset 0))
    (loop
      (when (>= offset msg-len) (return 0))
      (let ((remaining (- msg-len offset)))
        (let ((blen remaining))
          (when (> blen 16) (setq blen 16))
          (poly1305-process-block h rlimbs blk nlimbs msg offset blen)
          (setq offset (+ offset 16)))))))

;;; Fixed ssh-buf-consume: safe memory copy
(defun ssh-buf-consume (ssh n)
  (let ((buf-len (mem-ref (+ ssh #x6D4) :u32)))
    (let ((remaining (- buf-len n)))
      (when (> remaining 0)
        (let ((dst (+ ssh #x6D8))
              (src (+ (+ ssh #x6D8) n)))
          (let ((j 0))
            (loop
              (when (>= j remaining) (return 0))
              (let ((src-addr (+ src j)))
                (let ((val (mem-ref src-addr :u8)))
                  (let ((dst-addr (+ dst j)))
                    (setf (mem-ref dst-addr :u8) val))))
              (setq j (+ j 1))))))
      (setf (mem-ref (+ ssh #x6D4) :u32) remaining)
      remaining)))

;;; Fixed ssh-make-packet: safe copy with offset
(defun ssh-make-packet (ssh payload payload-len)
  (let ((base-len (+ 5 payload-len))
        (pad-len 0))
    (setq pad-len (- 8 (mod base-len 8)))
    (when (eq pad-len 8) (setq pad-len 0))
    (when (< pad-len 4) (setq pad-len (+ pad-len 8)))
    (let ((packet-len (+ (+ 1 payload-len) pad-len))
          (total-len (+ (+ (+ 4 1) payload-len) pad-len)))
      (let ((pkt (make-array total-len)))
        (ssh-put-u32 pkt 0 packet-len)
        (aset pkt 4 pad-len)
        ;; Safe copy payload to pkt[5..]
        (let ((j 0))
          (loop
            (when (>= j payload-len) (return 0))
            (let ((val (aref payload j)))
              (let ((dst (+ 5 j)))
                (aset pkt dst val)))
            (setq j (+ j 1))))
        ;; Safe random padding
        (let ((pad-off (+ 5 payload-len)))
          (let ((j 0))
            (loop
              (when (>= j pad-len) (return 0))
              (let ((r (ssh-random ssh)))
                (let ((dst (+ pad-off j)))
                  (aset pkt dst r)))
              (setq j (+ j 1)))))
        (cons pkt total-len)))))

;;; Fixed ssh-parse-packet: safe array copy with offset
(defun ssh-parse-packet (ssh data data-len)
  (when (< data-len 5) (return ()))
  (let ((packet-len (ssh-get-u32 data 0)))
    (when (< data-len (+ 4 packet-len)) (return ()))
    (let ((pad-len (aref data 4)))
      (let ((payload-len (- (- packet-len pad-len) 1)))
        (let ((payload (make-array payload-len))
              (cb (- ssh #x20)))
          (poly-safe-copy payload data 5 payload-len)
          (setf (mem-ref (+ cb #x16F8) :u32) (- data-len (+ 4 packet-len)))
          (setf (mem-ref (+ cb #x16FC) :u32) (+ 4 packet-len))
          (cons payload payload-len))))))

;;; Fixed ssh-decrypt-packet: all (aset arr i (x (+ base i))) patterns
;;; replaced with safe copies using intermediate let bindings.
(defun ssh-decrypt-setup-keys (ssh k1 k2)
  (let ((k1-base (+ ssh #x090))
        (k2-base (+ ssh #x0B0)))
    (safe-copy-mem-to-arr k1 k1-base 32)
    (safe-copy-mem-to-arr k2 k2-base 32)))

(defun ssh-decrypt-len (data len-ks)
  ;; Decrypt 4-byte length field
  (let ((plen 0))
    (dotimes (i 4)
      (let ((di (aref data i))
            (ki (aref len-ks i)))
        (let ((b (logxor di ki)))
          (setq plen (logior (ash plen 8) b)))))
    plen))

(defun ssh-decrypt-check-tag (data expected tag-off)
  ;; Compare 16-byte MAC tag, return 1 if match
  (let ((ok 1))
    (dotimes (i 16)
      (let ((doff (+ tag-off i)))
        (let ((dv (aref data doff)))
          (unless (eq dv (aref expected i))
            (setq ok 0)))))
    ok))

(defun ssh-decrypt-extract (data plain packet-len ssh data-len)
  ;; Extract payload from decrypted data
  (let ((pad-len (aref plain 0)))
    (let ((payload-len (- (- packet-len pad-len) 1)))
      (when (< payload-len 1) (return ()))
      (let ((payload (make-array payload-len)))
        (poly-safe-copy payload plain 1 payload-len)
        (let ((seq (mem-ref (+ ssh #x04) :u32)))
          (setf (mem-ref (+ ssh #x04) :u32) (+ seq 1)))
        ;; Set remaining bytes field (cb+0x16F8 = ssh+0x16D8)
        (let ((consumed (+ (+ 4 packet-len) 16)))
          (let ((remaining (- data-len consumed)))
            (let ((cb (- ssh #x20)))
              (setf (mem-ref (+ cb #x16F8) :u32) remaining))))
        (cons payload payload-len)))))

(defun ssh-decrypt-verify-mac (k1 nonce data packet-len)
  ;; Compute poly1305 MAC and compare with tag
  (let ((poly-ks (chacha-block k1 nonce 0))
        (poly-key (make-array 32)))
    (poly-safe-copy poly-key poly-ks 0 32)
    (let ((mac-len (+ 4 packet-len)))
      (let ((mac-input (make-array mac-len)))
        (poly-safe-copy mac-input data 0 mac-len)
        (let ((expected (poly1305 poly-key mac-input mac-len)))
          (let ((tag-off (+ 4 packet-len)))
            (let ((ok (ssh-decrypt-check-tag data expected tag-off)))
              (cons ok expected))))))))

(defun ssh-decrypt-packet (ssh data data-len)
  (when (< data-len 20) (return ()))
  (let ((k1 (make-array 32))
        (k2 (make-array 32)))
    (let ((seq (mem-ref (+ ssh #x04) :u32)))
      (let ((nonce (ssh-make-nonce seq)))
        (ssh-decrypt-setup-keys ssh k1 k2)
        (let ((len-ks (chacha-block k2 nonce 0)))
          (let ((packet-len (ssh-decrypt-len data len-ks)))
            (when (< data-len (+ (+ 4 packet-len) 16))
              (return ()))
            (ssh-decrypt-do ssh k1 nonce data data-len packet-len)))))))

(defun ssh-decrypt-do (ssh k1 nonce data data-len packet-len)
  (let ((mac-result (ssh-decrypt-verify-mac k1 nonce data packet-len)))
    (let ((tag-ok (car mac-result)))
      (when (zerop tag-ok) (return ()))
      (let ((enc-data (make-array packet-len)))
        (poly-safe-copy enc-data data 4 packet-len)
        (let ((plain (chacha20-crypt k1 nonce enc-data packet-len 1)))
          (ssh-decrypt-extract data plain packet-len ssh data-len))))))

;;; Fixed ssh-encrypt-packet: all computed aset indices use let bindings
(defun ssh-encrypt-build-plain (plain payload payload-len pad-len ssh)
  ;; Build plaintext: [pad-len][payload][random-padding]
  (aset plain 0 pad-len)
  (let ((j 0))
    (loop
      (when (>= j payload-len) (return 0))
      (let ((val (aref payload j)))
        (let ((dst (+ 1 j)))
          (aset plain dst val)))
      (setq j (+ j 1))))
  (let ((pad-off (+ 1 payload-len)))
    (let ((j 0))
      (loop
        (when (>= j pad-len) (return 0))
        (let ((r (ssh-random ssh)))
          (let ((dst (+ pad-off j)))
            (aset plain dst r)))
        (setq j (+ j 1))))))

(defun ssh-encrypt-build-result (result enc-len enc-data packet-len tag)
  ;; Assemble: [enc-len:4][enc-data:packet-len][tag:16]
  (poly-safe-copy result enc-len 0 4)
  (poly-safe-copy result enc-data 0 packet-len)
  ;; Actually result[4..] = enc-data[0..], need offset write
  0)

(defun ssh-encrypt-assemble (result enc-len enc-data packet-len tag)
  ;; Write enc-len to result[0..3]
  (poly-safe-copy result enc-len 0 4)
  ;; Write enc-data to result[4..4+packet-len-1]
  (let ((j 0))
    (loop
      (when (>= j packet-len) (return 0))
      (let ((val (aref enc-data j)))
        (let ((dst (+ 4 j)))
          (aset result dst val)))
      (setq j (+ j 1))))
  ;; Write tag to result[4+packet-len..4+packet-len+15]
  (let ((tag-off (+ 4 packet-len)))
    (let ((j 0))
      (loop
        (when (>= j 16) (return 0))
        (let ((val (aref tag j)))
          (let ((dst (+ tag-off j)))
            (aset result dst val)))
        (setq j (+ j 1))))))

(defun ssh-encrypt-mac-input (enc-len enc-data packet-len)
  ;; Build mac-input: [enc-len:4][enc-data:packet-len]
  (let ((mac-len (+ 4 packet-len)))
    (let ((mac-input (make-array mac-len)))
      (poly-safe-copy mac-input enc-len 0 4)
      (let ((j 0))
        (loop
          (when (>= j packet-len) (return 0))
          (let ((val (aref enc-data j)))
            (let ((dst (+ 4 j)))
              (aset mac-input dst val)))
          (setq j (+ j 1))))
      mac-input)))

(defun ssh-encrypt-packet (ssh payload payload-len)
  (let ((seq (mem-ref (+ ssh #x08) :u32))
        (k1 (make-array 32))
        (k2 (make-array 32)))
    (safe-copy-mem-to-arr k1 (+ ssh #x0D0) 32)
    (safe-copy-mem-to-arr k2 (+ ssh #x0F0) 32)
    (ssh-encrypt-packet-2 ssh k1 k2 seq payload payload-len)))

(defun ssh-encrypt-packet-2 (ssh k1 k2 seq payload payload-len)
  (let ((pad-len (- 8 (mod (+ 1 payload-len) 8))))
    (when (eq pad-len 8) (setq pad-len 0))
    (when (< pad-len 4) (setq pad-len (+ pad-len 8)))
    (let ((packet-len (+ (+ 1 payload-len) pad-len))
          (nonce (ssh-make-nonce seq)))
      (let ((plain (make-array packet-len)))
        (ssh-encrypt-build-plain plain payload payload-len pad-len ssh)
        (ssh-encrypt-packet-3 ssh k1 k2 seq nonce plain packet-len)))))

(defun ssh-encrypt-packet-3 (ssh k1 k2 seq nonce plain packet-len)
  (let ((len-ks (chacha-block k2 nonce 0))
        (enc-len (make-array 4))
        (len-bytes (make-array 4)))
    (ssh-put-u32 len-bytes 0 packet-len)
    (let ((j 0))
      (loop
        (when (>= j 4) (return 0))
        (let ((lb (aref len-bytes j)))
          (let ((lk (aref len-ks j)))
            (let ((val (logxor lb lk)))
              (aset enc-len j val))))
        (setq j (+ j 1))))
    (let ((poly-ks (chacha-block k1 nonce 0))
          (poly-key (make-array 32)))
      (poly-safe-copy poly-key poly-ks 0 32)
      (ssh-encrypt-packet-4 ssh k1 nonce enc-len poly-key plain packet-len seq))))

(defun ssh-encrypt-packet-4 (ssh k1 nonce enc-len poly-key plain packet-len seq)
  (let ((enc-data (chacha20-crypt k1 nonce plain packet-len 1)))
    (let ((mac-input (ssh-encrypt-mac-input enc-len enc-data packet-len)))
      (let ((tag (poly1305 poly-key mac-input (+ 4 packet-len))))
        (let ((total (+ (+ 4 packet-len) 16)))
          (let ((result (make-array total)))
            (ssh-encrypt-assemble result enc-len enc-data packet-len tag)
            (setf (mem-ref (+ ssh #x08) :u32) (+ seq 1))
            (cons result total)))))))

;;; Fixed ssh-eh-write-u32: safe version (avoids aset index clobber)
(defun ssh-eh-write-u32 (buf pos val)
  (let ((b0 (logand (ash val -24) #xFF)))
    (aset buf pos b0))
  (let ((p1 (+ pos 1)))
    (let ((b1 (logand (ash val -16) #xFF)))
      (aset buf p1 b1)))
  (let ((p2 (+ pos 2)))
    (let ((b2 (logand (ash val -8) #xFF)))
      (aset buf p2 b2)))
  (let ((p3 (+ pos 3)))
    (let ((b3 (logand val #xFF)))
      (aset buf p3 b3)))
  (+ pos 4))

;;; Fixed ssh-eh-write-arr: safe array-to-array copy with let bindings
(defun ssh-eh-write-arr (buf pos src len)
  (let ((p (ssh-eh-write-u32 buf pos len)))
    (let ((j 0))
      (loop
        (when (>= j len) (return 0))
        (let ((val (aref src j)))
          (let ((dst (+ p j)))
            (aset buf dst val)))
        (setq j (+ j 1))))
    (+ p len)))

;;; Fixed ssh-eh-write-mem: copies mem[src..src+len-1] to buf[pos+4..pos+4+len-1]
(defun ssh-eh-write-mem (buf pos src len)
  (let ((p (ssh-eh-write-u32 buf pos len)))
    (let ((j 0))
      (loop
        (when (>= j len) (return 0))
        (let ((addr (+ src j)))
          (let ((val (mem-ref addr :u8)))
            (let ((dst (+ p j)))
              (aset buf dst val))))
        (setq j (+ j 1))))
    (+ p len)))

;;; Fixed ssh-put-u32: safe version (original uses aset with computed index)
(defun ssh-put-u32 (arr off val)
  (let ((b0 (logand (ash val -24) #xFF)))
    (aset arr off b0))
  (let ((o1 (+ off 1)))
    (let ((b1 (logand (ash val -16) #xFF)))
      (aset arr o1 b1)))
  (let ((o2 (+ off 2)))
    (let ((b2 (logand (ash val -8) #xFF)))
      (aset arr o2 b2)))
  (let ((o3 (+ off 3)))
    (let ((b3 (logand val #xFF)))
      (aset arr o3 b3))))

;;; Fixed ssh-concat2: safe array copy with let bindings
;;; Original has (aset r i (aref a i)) which hits variable-index ASET bug on ARM32
(defun ssh-concat2 (a a-len b b-len)
  (let ((r (make-array (+ a-len b-len))))
    (let ((j 0))
      (loop
        (when (>= j a-len) (return 0))
        (let ((val (aref a j)))
          (let ((dummy (aset r j val)))
            dummy))
        (setq j (+ j 1))))
    (let ((j 0))
      (loop
        (when (>= j b-len) (return 0))
        (let ((val (aref b j)))
          (let ((dst (+ a-len j)))
            (let ((dummy (aset r dst val)))
              dummy)))
        (setq j (+ j 1))))
    r))

;;; Fixed ssh-make-str: safe version
(defun ssh-make-str (data data-len)
  (let ((r (make-array (+ 4 data-len))))
    (ssh-put-u32 r 0 data-len)
    (let ((j 0))
      (loop
        (when (>= j data-len) (return 0))
        (let ((val (aref data j)))
          (let ((dst (+ 4 j)))
            (let ((dummy (aset r dst val)))
              dummy)))
        (setq j (+ j 1))))
    r))

;;; Fixed ssh-mem-store: safe version
(defun ssh-mem-store (addr data len)
  (let ((j 0))
    (loop
      (when (>= j len) (return 0))
      (let ((val (aref data j)))
        (let ((dst (+ addr j)))
          (setf (mem-ref dst :u8) val)))
      (setq j (+ j 1)))))

;;; Fixed ssh-mem-load: safe version (used in ssh-encode-host-key)
(defun ssh-mem-load (arr addr len)
  (let ((j 0))
    (loop
      (when (>= j len) (return 0))
      (let ((src (+ addr j)))
        (let ((val (mem-ref src :u8)))
          (let ((dummy (aset arr j val)))
            dummy)))
      (setq j (+ j 1)))))

;;; Fixed ssh-buf-to-array: safe mem-to-array copy
(defun ssh-buf-to-array (ssh len)
  (let ((arr (make-array len))
        (base (+ ssh #x6D8)))
    (safe-copy-mem-to-arr arr base len)
    arr))

;;; Fixed ssh-dispatch-msg: safe array copies with offset
(defun ssh-dispatch-svc (ssh payload svc-len)
  (let ((svc (make-array 32)))
    (poly-safe-copy svc payload 5 svc-len)
    (ssh-send-service-accept ssh svc svc-len)))

(defun ssh-dispatch-exec (ssh payload cmd-off flag-addr)
  (let ((cmd-len (ssh-get-u32 payload cmd-off)))
    (let ((cmd (make-array cmd-len)))
      (let ((cmd-data-off (+ cmd-off 4)))
        (poly-safe-copy cmd payload cmd-data-off cmd-len))
      (ssh-eval-line ssh cmd cmd-len)
      (ssh-dispatch-exec-close ssh flag-addr))))

(defun ssh-dispatch-exec-close (ssh flag-addr)
  (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
    (let ((eof-msg (make-array 5)))
      (aset eof-msg 0 96)
      (ssh-put-u32 eof-msg 1 cli-chan)
      (ssh-send-payload ssh eof-msg 5))
    (let ((close-msg (make-array 5)))
      (aset close-msg 0 97)
      (ssh-put-u32 close-msg 1 cli-chan)
      (ssh-send-payload ssh close-msg 5)))
  (setf (mem-ref flag-addr :u32) 0))

(defun ssh-dispatch-msg (ssh payload plen flag-addr)
  (let ((msg-type (aref payload 0)))
    (when (eq msg-type 5)
      (let ((svc-len (ssh-get-u32 payload 1)))
        (ssh-dispatch-svc ssh payload svc-len)))
    (when (eq msg-type 50)
      (ssh-handle-userauth ssh payload plen))
    (when (eq msg-type 90)
      (let ((ctype-len (ssh-get-u32 payload 1)))
        (let ((cli-chan (ssh-get-u32 payload (+ 5 ctype-len))))
          (setf (mem-ref (+ ssh #x18) :u32) cli-chan)
          (setf (mem-ref (+ ssh #x14) :u32) 0)
          (ssh-send-channel-confirm ssh cli-chan 0))))
    (when (eq msg-type 98)
      (ssh-dispatch-chanreq ssh payload flag-addr))
    (when (eq msg-type 94)
      (ssh-handle-channel-data ssh payload plen))
    (when (eq msg-type 96)
      (ssh-dispatch-eof-close ssh flag-addr))
    (when (eq msg-type 97)
      (setf (mem-ref flag-addr :u32) 0))
    (when (eq msg-type 1)
      (setf (mem-ref flag-addr :u32) 0))))

(defun ssh-dispatch-chanreq (ssh payload flag-addr)
  (let ((rtype-len (ssh-get-u32 payload 5)))
    (let ((want-reply-off (+ 9 rtype-len)))
      (let ((want-reply (aref payload want-reply-off)))
        (when (not (zerop want-reply))
          (ssh-send-channel-success ssh
           (mem-ref (+ ssh #x18) :u32)))))
    (when (eq rtype-len 5)
      (when (eq (aref payload 9) 115)
        (ssh-send-prompt ssh)))
    (when (eq rtype-len 4)
      (when (eq (aref payload 9) 101)
        (let ((cmd-off (+ 10 rtype-len)))
          (ssh-dispatch-exec ssh payload cmd-off flag-addr))))))

(defun ssh-dispatch-eof-close (ssh flag-addr)
  (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
    (let ((eof-msg (make-array 5)))
      (aset eof-msg 0 96)
      (ssh-put-u32 eof-msg 1 cli-chan)
      (ssh-send-payload ssh eof-msg 5))
    (let ((close-msg (make-array 5)))
      (aset close-msg 0 97)
      (ssh-put-u32 close-msg 1 cli-chan)
      (ssh-send-payload ssh close-msg 5)))
  (setf (mem-ref flag-addr :u32) 0))

;;; Debug: dump full SHA-512 output to identify which state variables are wrong
(defun dbg-test-sha512-full ()
  (sha512-init)
  (let ((privkey (make-array 32)))
    (dotimes (i 32) (aset privkey i 0))
    (let ((hash (sha512 privkey)))
      ;; Dump all 64 bytes in 8-byte chunks (H0..H7)
      ;; Expected (SHA-512 of 32 zero bytes):
      ;; H0: 5046adc1dba83886
      ;; H1: 7b2bbbfdd0c3423e
      ;; H2: 58b57970b5267a90
      ;; H3: f57960924a87f196
      ;; H4: 0a6a85eaa642dac8
      ;; H5: 35424b5d7c8d637c
      ;; H6: 00408c7a73da672b
      ;; H7: 7f498521420b6dd3
      (let ((i 0))
        (loop
          (when (>= i 8) (return 0))
          (write-char-serial (+ 48 i)) ;; 0-7
          (write-char-serial 61)
          (dbg-hex-bytes hash (* i 8) 8)
          (write-char-serial 10)
          (setq i (+ i 1)))))))

;;; Debug: test concat-bytes using prefix from state memory (real code path)
(defun dbg-dump-concat-result (r-input)
  ;; Dump first 8 bytes of concat result (should be prefix bytes)
  (write-char-serial 114) ;; r
  (write-char-serial 48) ;; 0
  (write-char-serial 61) ;; =
  (dbg-hex-bytes r-input 0 8)
  (write-char-serial 10)
  ;; Dump bytes 32-39 (should be all zeros = message)
  (write-char-serial 114) ;; r
  (write-char-serial 51) ;; 3
  (write-char-serial 61) ;; =
  (dbg-hex-bytes r-input 32 8)
  (write-char-serial 10)
  ;; Now SHA-512 of concat result
  ;; Expected for prefix||zeros: E28B816F44B3B6E1
  (let ((h (sha512 r-input)))
    (write-char-serial 114) ;; r
    (write-char-serial 104) ;; h
    (write-char-serial 61)  ;; =
    (dbg-hex-bytes h 0 8)
    (write-char-serial 10)))

;;; Fix ed-reduce-if-needed: use nil instead of 0 for false flag.
;;; The MVM compiler's (when ge-l ...) treats 0 as truthy (only NIL is falsy),
;;; so (setq ge-l 0) doesn't prevent the subtraction from running.
(defun ed-reduce-if-needed (x)
  (let ((result (make-array 32)))
    (poly-safe-copy result x 0 32)
    (let ((ge-l 1) (ci 31))
      (loop
        (if (< ci 0) (return nil)
            (let ((rb (aref result ci))
                  (lb (ed-l-byte ci)))
              (if (< lb rb) (return nil)
                  (if (< rb lb)
                      (progn (setq ge-l nil) (return nil))
                      (setq ci (- ci 1)))))))
      (when ge-l
        (let ((borrow 0))
          (dotimes (i 32)
            (let ((ri (aref result i))
                  (li (ed-l-byte i)))
              (let ((diff (- (- ri li) borrow)))
                (if (< diff 0)
                    (progn (aset result i (+ diff 256)) (setq borrow 1))
                    (progn (aset result i diff) (setq borrow 0)))))))))
    result))

;;; Fix fe-equal: return nil/1 instead of 0/1 so (unless (fe-equal ...) ...)
;;; works correctly with MVM's NIL-only falsiness.
(defun fe-equal (a b)
  (let ((ab (fe-to-bytes a))
        (bb (fe-to-bytes b))
        (result 1))
    (dotimes (i 32)
      (unless (eq (aref ab i) (aref bb i))
        (setq result nil)))
    result))

;;; Arena-based fe-pow-sqrt: prevents ~2MB triple garbage per call
;;; from exhausting the heap. Same algorithm as crypto.lisp but with
;;; periodic alloc pointer resets (same pattern as fe-invert override).
(defun fe-pow-sqrt (u)
  (let ((z2 (make-array 40))
        (z9 (make-array 40))
        (z11 (make-array 40))
        (t0 (make-array 40))
        (t1 (make-array 40)))
    (let ((arena-save (get-alloc-ptr)))
      (fe-sq z2 u)
      (fe-sq t0 z2)
      (fe-sq t1 t0)
      (fe-mul z9 u t1)
      (fe-mul z11 z2 z9)
      (fe-sq t0 z11)
      (fe-mul t0 z9 t0)
      (set-alloc-ptr arena-save)
      (fe-sq t1 t0) (fe-sq-iter t1 4)
      (fe-mul t1 t1 t0)
      (set-alloc-ptr arena-save)
      (fe-sq z2 t1) (fe-sq-iter z2 9)
      (fe-mul z2 z2 t1)
      (set-alloc-ptr arena-save)
      (fe-sq z9 z2) (fe-sq-iter z9 19)
      (fe-mul z9 z9 z2)
      (set-alloc-ptr arena-save)
      (fe-sq t0 z9) (fe-sq-iter t0 9)
      (fe-mul t0 t0 t1)
      (set-alloc-ptr arena-save)
      (fe-sq t1 t0) (fe-sq-iter t1 49)
      (fe-mul t1 t1 t0)
      (set-alloc-ptr arena-save)
      (fe-sq z2 t1) (fe-sq-iter z2 99)
      (fe-mul z2 z2 t1)
      (set-alloc-ptr arena-save)
      (fe-sq z9 z2) (fe-sq-iter z9 49)
      (fe-mul z9 z9 t0)
      (set-alloc-ptr arena-save)
      (fe-sq z9 z9)
      (fe-mul z9 z9 u)
      (fe-sq t0 z9)
      t0)))

;;; Arena-based ed-recover-x: wraps the whole point recovery in an arena
;;; to prevent triple garbage from fe-pow-sqrt and fe-mul from accumulating.
;;; Must match crypto.lisp ed-recover-x exactly (same algorithm, same offsets).
(defun ed-recover-x (y sign)
  (let ((d-fe (make-array 40))
        (y2 (make-array 40))
        (y2m1 (make-array 40))
        (dy2 (make-array 40))
        (dy2p1 (make-array 40))
        (u (make-array 40))
        (x (make-array 40))
        (x2 (make-array 40))
        (sm (make-array 40)))
    (let ((arena-save (get-alloc-ptr)))
      (fe-load-fixed d-fe (+ (e1000-state-base) #x500))
      (fe-sq y2 y)
      (fe-sub y2m1 y2 (fe-from-int 1))
      (fe-mul dy2 d-fe y2)
      (fe-add dy2p1 dy2 (fe-from-int 1))
      (set-alloc-ptr arena-save)
      (fe-mul u y2m1 (fe-invert dy2p1))
      (set-alloc-ptr arena-save)
      (fe-copy x (fe-pow-sqrt u))
      (set-alloc-ptr arena-save)
      (fe-sq x2 x)
      (when (zerop (fe-equal x2 u))
        (fe-load-fixed sm (+ (e1000-state-base) #x578))
        (fe-mul x x sm))
      (set-alloc-ptr arena-save)
      (let ((xb (fe-to-bytes x)))
        (let ((x-odd (logand (aref xb 0) 1)))
          (if sign
              (when (zerop x-odd)
                (fe-sub x (fe-from-int 0) x))
              (unless (zerop x-odd)
                (fe-sub x (fe-from-int 0) x)))))
      (set-alloc-ptr arena-save)
      x)))

;;; Override ed25519-verify to avoid ASET+AREF register clobber bug
;;; on 32-bit targets. Must be in build-fixpoint.lisp for last-defun-wins.
(defun dbg-verify-inner (s-bytes k a-point r-point)
  ;; Dump alloc pointer before [S]B
  (write-char-serial 80) ;; P
  (write-char-serial 49) ;; 1
  (write-char-serial 61)
  (print-hex (get-alloc-ptr))
  (write-char-serial 10)
  ;; [S]B
  (let ((sb-point (ed-base-mult s-bytes)))
    (let ((sb-e (ed-encode-point sb-point)))
      (write-char-serial 76) ;; L
      (write-char-serial 61)
      (dbg-hex-bytes sb-e 0 8)
      (write-char-serial 10)
      ;; Dump alloc pointer after [S]B
      (write-char-serial 80) ;; P
      (write-char-serial 50) ;; 2
      (write-char-serial 61)
      (print-hex (get-alloc-ptr))
      (write-char-serial 10)
      ;; [k]A
      (let ((ka-point (ed-scalar-mult k a-point)))
        (let ((ka-e (ed-encode-point ka-point)))
          (write-char-serial 107) ;; k
          (write-char-serial 97)  ;; a
          (write-char-serial 61)
          (dbg-hex-bytes ka-e 0 8)
          (write-char-serial 10)
          ;; Dump alloc pointer after [k]A
          (write-char-serial 80) ;; P
          (write-char-serial 51) ;; 3
          (write-char-serial 61)
          (print-hex (get-alloc-ptr))
          (write-char-serial 10)
          ;; R + [k]A
          (let ((rka-point (ed-add r-point ka-point)))
            (let ((rka-e (ed-encode-point rka-point)))
              (write-char-serial 77) ;; M
              (write-char-serial 61)
              (dbg-hex-bytes rka-e 0 8)
              (write-char-serial 10)
              ;; Compare
              (let ((equal 1))
                (dotimes (i 32)
                  (unless (eq (aref sb-e i) (aref rka-e i))
                    (setq equal nil)))
                equal))))))))

(defun ed25519-verify (pubkey signature message msg-len)
  (ed25519-init)
  (sha512-init)
  (write-char-serial 73) ;; I
  (write-char-serial 33) ;; !
  (let ((r-bytes (make-array 32))
        (s-bytes (make-array 32)))
    ;; Extract R and S with let bindings
    (dotimes (i 32)
      (let ((r-val (aref signature i)))
        (aset r-bytes i r-val))
      (let ((s-idx (+ i 32)))
        (let ((s-val (aref signature s-idx)))
          (aset s-bytes i s-val))))
    ;; Dump extracted R and S
    (write-char-serial 100) ;; d (R part)
    (write-char-serial 114) ;; r
    (write-char-serial 61)  ;; =
    (dbg-hex-bytes r-bytes 0 8)
    (write-char-serial 10)
    (write-char-serial 100) ;; d (S part)
    (write-char-serial 115) ;; s
    (write-char-serial 61)  ;; =
    (dbg-hex-bytes s-bytes 0 8)
    (write-char-serial 10)
    ;; Decode points
    (let ((r-point (ed-decode-point r-bytes)))
      (let ((a-point (ed-decode-point pubkey)))
        ;; k = SHA-512(R || A || message) mod L
        (let ((k-input (concat3-bytes r-bytes 32 pubkey 32 message msg-len)))
          ;; Dump k-input to verify
          (write-char-serial 99)  ;; c (k-input first 8)
          (write-char-serial 48)  ;; 0
          (write-char-serial 61)
          (dbg-hex-bytes k-input 0 8)
          (write-char-serial 10)
          (write-char-serial 99)  ;; c (k-input bytes 32-39)
          (write-char-serial 51)  ;; 3
          (write-char-serial 61)
          (dbg-hex-bytes k-input 32 8)
          (write-char-serial 10)
          (let ((k (ed-reduce-scalar (sha512 k-input))))
            (write-char-serial 118) ;; v (verify k)
            (write-char-serial 107) ;; k
            (write-char-serial 61)
            (dbg-hex-bytes k 0 8)
            (write-char-serial 10)
            (dbg-verify-inner s-bytes k a-point r-point)))))))

(defun dbg-sign-part2 (r r-enc a-enc msg s)
  ;; k = SHA-512(R || A || msg) mod L
  (let ((k-input (concat3-bytes r-enc 32 a-enc 32 msg 32)))
    ;; Dump k-input bytes: first 8 should be R, bytes 32-39 should be A
    (write-char-serial 107) ;; k
    (write-char-serial 48)  ;; 0
    (write-char-serial 61)  ;; =
    (dbg-hex-bytes k-input 0 8)
    (write-char-serial 10)
    (write-char-serial 107) ;; k
    (write-char-serial 51)  ;; 3
    (write-char-serial 61)  ;; =
    (dbg-hex-bytes k-input 32 8)
    (write-char-serial 10)
    (let ((k-hash (sha512 k-input)))
      (let ((k (ed-reduce-scalar k-hash)))
        (write-char-serial 107) ;; k
        (write-char-serial 61)  ;; =
        (dbg-hex-bytes k 0 8)
        (write-char-serial 10)
        ;; S = (r + k*s) mod L
        (let ((ks (ed-scalar-mult-mod-l k s)))
          (write-char-serial 75)  ;; K (ks product)
          (write-char-serial 61)  ;; =
          (dbg-hex-bytes ks 0 8)
          (write-char-serial 10)
          (let ((sig-s (ed-scalar-add r ks)))
            (write-char-serial 83)  ;; S
            (write-char-serial 61)  ;; =
            (dbg-hex-bytes sig-s 0 8)
            (write-char-serial 10)
            ;; Build signature using concat-bytes (avoids aset index bug)
            (let ((sig (concat-bytes r-enc 32 sig-s 32)))
              ;; Dump sig[0:8] and sig[32:40] to verify assembly
              (write-char-serial 115) ;; s
              (write-char-serial 114) ;; r
              (write-char-serial 61)  ;; =
              (dbg-hex-bytes sig 0 8)
              (write-char-serial 10)
              (write-char-serial 115) ;; s
              (write-char-serial 115) ;; s
              (write-char-serial 61)  ;; =
              (dbg-hex-bytes sig 32 8)
              (write-char-serial 10)
              (let ((v (ed25519-verify a-enc sig msg 32)))
                (write-char-serial 86)  ;; V
                (write-char-serial 61)  ;; =
                (if v (write-char-serial 49) (write-char-serial 48))
                (write-char-serial 10)))))))))

(defun dbg-test-concat-sha512 ()
  ;; Full Ed25519 sign test using real code path
  (let ((state (e1000-state-base)))
    ;; Load s, prefix, a-enc from state (same as ed25519-sign-fast)
    (let ((s (make-array 32)))
      (dotimes (i 32)
        (aset s i (mem-ref (+ state (+ #x680 i)) :u8)))
      (let ((prefix (make-array 32)))
        (dotimes (i 32)
          (aset prefix i (mem-ref (+ state (+ #x6A0 i)) :u8)))
        (let ((a-enc (make-array 32)))
          (dotimes (i 32)
            (aset a-enc i (mem-ref (+ state (+ #x730 i)) :u8)))
          ;; r = SHA-512(prefix || msg) mod L
          (let ((msg (make-array 32)))
            (dotimes (i 32) (aset msg i 0))
            (let ((r-input (concat-bytes prefix 32 msg 32)))
              (let ((r (ed-reduce-scalar (sha512 r-input))))
                (write-char-serial 110) ;; n
                (write-char-serial 61)
                (dbg-hex-bytes r 0 8)
                (write-char-serial 10)
                (let ((r-enc (ed-encode-point (ed-base-mult r))))
                  (write-char-serial 82) ;; R
                  (write-char-serial 61)
                  (dbg-hex-bytes r-enc 0 8)
                  (write-char-serial 10)
                  (dbg-sign-part2 r r-enc a-enc msg s))))))))))

;;; Debug: test scalar arithmetic and full Ed25519 sign+verify
(defun dbg-test-sign-verify ()
  ;; Test ed-scalar-mult-mod-l: 255 * 255 = 65025 = 0xFE01
  (let ((a (make-array 32)) (b (make-array 32)))
    (dotimes (i 32) (aset a i 0) (aset b i 0))
    (aset a 0 255)
    (aset b 0 255)
    (let ((p (ed-scalar-mult-mod-l a b)))
      (write-char-serial 80) ;; P
      (write-char-serial 61) ;; =
      (print-hex-byte (aref p 0)) ;; should be 01
      (print-hex-byte (aref p 1)) ;; should be FE
      (print-hex-byte (aref p 2)) ;; should be 00
      (write-char-serial 10)))
  ;; Test ed-scalar-add: 200 + 100 = 300 = 0x012C
  (let ((a (make-array 32)) (b (make-array 32)))
    (dotimes (i 32) (aset a i 0) (aset b i 0))
    (aset a 0 200)
    (aset b 0 100)
    (let ((s (ed-scalar-add a b)))
      (write-char-serial 43) ;; +
      (write-char-serial 61) ;; =
      (print-hex-byte (aref s 0)) ;; should be 2C
      (print-hex-byte (aref s 1)) ;; should be 01
      (print-hex-byte (aref s 2)) ;; should be 00
      (write-char-serial 10)))
  ;; Test concat-bytes + SHA-512 (the signing flow)
  (dbg-test-concat-sha512))

;;; Debug helper: verify signature (separate function to avoid >18 nested lets)
(defun dbg-verify-sig (state sig h)
  (let ((a-enc (make-array 32)))
    (dotimes (i 32)
      (aset a-enc i (mem-ref (+ state (+ #x730 i)) :u8)))
    (ed25519-verify a-enc sig h 32)))

;;; ed25519-sign-fast: pre-computed s, prefix, host public key.
;;; One ed-base-mult per connection (unavoidable).
(defun ed25519-sign-fast (message msg-len)
  (let ((state (e1000-state-base)))
    (let ((s (make-array 32)))
      (safe-copy-mem-to-arr s (+ state #x680) 32)
      (let ((prefix (make-array 32)))
        (safe-copy-mem-to-arr prefix (+ state #x6A0) 32)
        (let ((a-enc (make-array 32)))
          (safe-copy-mem-to-arr a-enc (+ state #x730) 32)
          (let ((r-input (concat-bytes prefix 32 message msg-len)))
            (let ((r (ed-reduce-scalar (sha512 r-input))))
              (let ((r-enc (ed-encode-point (ed-base-mult r))))
                (let ((k-input (concat3-bytes r-enc 32 a-enc 32 message msg-len)))
                  (let ((k (ed-reduce-scalar (sha512 k-input))))
                    (let ((ks (ed-scalar-mult-mod-l k s)))
                      (let ((sig-s (ed-scalar-add r ks)))
                        (concat-bytes r-enc 32 sig-s 32)))))))))))))


;;; Override ssh-receive-version: original uses (when got-version ...)
;;; but fixnum 0 != nil on bare metal, so it returns immediately.
;;; Also fix 3-arg + patterns: (+ ssh #x6D8 i) → (+ (+ ssh #x6D8) i)
(defun ssh-receive-version (ssh)
  (let ((got-version 0))
    (let ((tries 0))
      (loop
        (when (not (zerop got-version)) (return 1))
        (when (> tries 50)
          (write-char-serial 84) ;; T (timeout)
          (return 0))
        (let ((msg (receive)))
          (when (zerop msg)
            (write-char-serial 82) ;; R (receive=0)
            (return 0)))
        (let ((blen (mem-ref (+ ssh #x6D4) :u32)))
          (when (zerop (logand tries 15))
            ;; Print blen every 16 iterations
            (write-char-serial 98) ;; b
            (print-dec blen)
            (write-char-serial 10))
          (when (> blen 8)
            (write-char-serial 86) ;; V (have data)
            (let ((buf-base (+ ssh #x6D8)))
              ;; Print first 3 bytes
              (print-dec (mem-ref buf-base :u8))
              (write-char-serial 44) ;; ,
              (print-dec (mem-ref (+ buf-base 1) :u8))
              (write-char-serial 44)
              (print-dec (mem-ref (+ buf-base 2) :u8))
              (write-char-serial 10)
              (when (eq (mem-ref buf-base :u8) 83)
                (when (eq (mem-ref (+ buf-base 1) :u8) 83)
                  (when (eq (mem-ref (+ buf-base 2) :u8) 72)
                    (let ((end 0))
                      (let ((i 3))
                        (loop
                          (when (not (zerop end)) (return 0))
                          (when (> i blen) (return 0))
                          (when (eq (mem-ref (+ buf-base i) :u8) 10)
                            (setq end i))
                          (setq i (+ i 1))))
                      (when (not (zerop end))
                        (let ((vlen end))
                          (when (> end 0)
                            (when (eq (mem-ref (+ buf-base (- end 1)) :u8) 13)
                              (setq vlen (- end 1))))
                          (let ((ver-base (+ ssh #x650)))
                            (dotimes (j vlen)
                              (setf (mem-ref (+ ver-base j) :u8)
                                    (mem-ref (+ buf-base j) :u8))))
                          (setf (mem-ref (+ ssh #x6D0) :u32) vlen)
                          (ssh-buf-consume ssh (+ end 1))
                          (setq got-version 1))))))))))
        (setq tries (+ tries 1))))))

(defun cd-do-enter (ssh)
  (let ((s ssh))
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
    (ssh-flush-output s)
    (cd-send-newline s)
    (ssh-do-eval s)
    (cd-send-prompt s)
    (cd-reinit-edit)))

(defun cd-send-newline (ssh)
  (let ((s ssh))
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
    (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
    (%serial-byte 10)
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
    (ssh-flush-output s)))

(defun cd-send-prompt (ssh)
  (let ((s ssh))
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
    (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
    (emit-prompt)
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
    (ssh-flush-output s)))

(defun cd-reinit-edit ()
  (edit-set-line-len 0)
  (edit-set-cursor-pos 0)
  (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) 0)
  (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
  (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0))

(defun cd-init-edit-state ()
  (edit-set-line-len 0)
  (edit-set-cursor-pos 0))

(defun cd-process-one (s p i)
  (let ((byte (aref p (+ 9 i))))
    (let ((rc (handle-edit-byte byte)))
      (if (eq rc 1) (cd-do-enter s) ()))))

(defun cd-process-bytes (ssh payload data-len)
  (let ((s ssh))
    (let ((p payload))
      (let ((dlen data-len))
        (let ((i 0))
          (loop
            (if (< i dlen)
                (progn
                  (cd-process-one s p i)
                  (setq i (+ i 1)))
                (return 0))))))))

(defun cd-channel-finish (ssh)
  (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
  (ssh-flush-output ssh)
  (setf (mem-ref (+ (ssh-ipc-base) #x12A00) :u64) 0))

(defun cd-setup-capture ()
  (let ((base (ssh-ipc-base)))
    (setf (mem-ref (+ base #x12A00) :u64) 1)
    (setf (mem-ref (+ base #x14) :u32) 3)
    (setf (mem-ref (+ base #x18) :u32) 0)))

(defun ssh-handle-channel-data (ssh payload plen)
  (let ((s ssh))
    (let ((p payload))
      (let ((data-len (ssh-get-u32 p 5)))
        (cd-init-edit-state)
        (cd-setup-capture)
        (cd-process-bytes s p data-len)
        (cd-channel-finish s)))))

(defun cd-eval-and-print (ssh)
  (let ((s ssh))
    (let ((lst (buf-read-list)))
      (let ((globals (ssh-get-globals)))
        (let ((result (eval-sexp lst nil globals)))
          (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
          (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
          (%serial-byte 10)
          (%serial-byte 61) (%serial-byte 32)
          (ssh-print-sexp result)
          (%serial-byte 10)
          (cd-flush-eval-output s))))))

(defun ssh-do-eval-expr (ssh)
  (let ((s ssh))
    (let ((len (edit-line-len)))
      (setf (mem-ref (+ (ssh-ipc-base) #x20) :u32) 1)
      (setf (mem-ref (+ (ssh-ipc-base) #x24) :u32) len)
      (cd-eval-and-print s))))

(defun cd-copy-capture-buf (out out-len)
  (let ((base (+ (ssh-ipc-base) #x100)))
    (dotimes (i out-len)
      (aset out i (mem-ref (+ base i) :u8)))))

(defun cd-flush-eval-output (ssh)
  (let ((s ssh))
    (let ((out-len (mem-ref (+ (ssh-ipc-base) #x18) :u32)))
      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
      (when (> out-len 0)
        (let ((out (make-array out-len)))
          (cd-copy-capture-buf out out-len)
          (ssh-send-string s out out-len))))))

;;; Safe ssh-derive-key: uses ssh-concat2 (avoids deeply nested lets)
(defun ssh-derive-key (ssh key-id needed-len)
  (let ((k-arr (make-array 32)))
    (ssh-mem-load k-arr (+ ssh #x070) 32)
    (let ((k-mpint (ssh-make-mpint k-arr)))
      (let ((h (make-array 32)))
        (ssh-mem-load h (+ ssh #x050) 32)
        (let ((id (make-array 1)))
          (aset id 0 key-id)
          (let ((sid (make-array 32)))
            (ssh-mem-load sid (+ ssh #x030) 32)
            (let ((p1 (ssh-concat2 k-mpint (array-length k-mpint)
                                    h 32)))
              (let ((p2 (ssh-concat2 p1 (array-length p1) id 1)))
                (let ((p3 (ssh-concat2 p2 (array-length p2) sid 32)))
                  (let ((k1 (sha256 p3 (array-length p3))))
                    (if (> needed-len 32)
                        (let ((p4 (ssh-concat2 k-mpint (array-length k-mpint)
                                                h 32)))
                          (let ((p5 (ssh-concat2 p4 (array-length p4) k1 32)))
                            (let ((k2 (sha256 p5 (array-length p5))))
                              (ssh-concat2 k1 32 k2 32))))
                        k1)))))))))))

;;; Override receive() to skip SYN packets on DWC2 USB targets (arm32/i386)
;;; Without this, retransmitted SYNs cause re-entrant ssh-handle-connection
(defun receive ()
  (io-delay)
  (let ((pkt-len (e1000-receive)))
    (if (zerop pkt-len)
        1
        (let ((buf (e1000-rx-buf)))
          (let ((et-hi (mem-ref (+ buf 12) :u8)))
            (when (eq et-hi #x08)
              (let ((et-lo (mem-ref (+ buf 13) :u8)))
                (if (eq et-lo #x06)
                    (let ((arp-op (buf-read-u16-mem buf 20)))
                      (when (eq arp-op 1) (arp-reply buf)))
                    (when (eq et-lo 0)
                      (let ((proto (mem-ref (+ buf 23) :u8)))
                        (if (eq proto 17)
                            (udp-handle buf 14)
                            (when (eq proto 6)
                              (let ((tcp-flags (mem-ref (+ buf 47) :u8)))
                                (when (not (eq (logand tcp-flags #x02) #x02))
                                  (net-handle-tcp buf pkt-len)))))))))))
          1))))

;;; Override ssh-handle-kex: use ed25519-sign-fast (pre-computed scalar mult)
;;; Same logic as standalone arm32 build — no usb-keepalive needed in fixpoint
(defun ssh-handle-kex (ssh kex-init-payload kex-init-len)
  (let ((cli-eph (make-array 32)))
    (let ((ci 0))
      (loop
        (when (>= ci 32) (return 0))
        (let ((src-idx (+ 5 ci)))
          (let ((val (aref kex-init-payload src-idx)))
            (let ((dummy (aset cli-eph ci val))) dummy)))
        (setq ci (+ ci 1))))
    (write-char-serial 75) ;; K (kex start)
    (let ((state (e1000-state-base)))
      (let ((srv-priv (make-array 32)))
        (safe-copy-mem-to-arr srv-priv (+ state #x6C4) 32)
        (let ((srv-eph (make-array 32)))
          (safe-copy-mem-to-arr srv-eph (+ state #x6E4) 32)
          (write-char-serial 88) ;; X (x25519 start)
          (let ((shared (x25519 srv-priv cli-eph)))
            ;; Print shared secret first 8 bytes
            (write-char-serial 120) ;; x
            (dotimes (di 8) (print-hex-byte (aref shared di)))
            (write-char-serial 10)
            (write-char-serial 72) ;; H (hash start)
            (ssh-mem-store (+ ssh #x070) shared 32)
            (let ((h (ssh-compute-exchange-hash ssh cli-eph srv-eph shared)))
              ;; Print exchange hash first 8 bytes
              (write-char-serial 104) ;; h
              (dotimes (di 8) (print-hex-byte (aref h di)))
              (write-char-serial 10)
              (ssh-mem-store (+ ssh #x050) h 32)
              (when (zerop (mem-ref ssh :u32))
                (ssh-mem-store (+ ssh #x030) h 32)
                (setf (mem-ref ssh :u32) 1))
              (write-char-serial 83) ;; S (sign start)
              (let ((sig (ed25519-sign-fast h 32)))
                (write-char-serial 82) ;; R (reply)
                (ssh-send-kex-reply ssh sig srv-eph)))))))))

;;; Override ssh-handle-connection: use (not x) instead of (zerop x)
;;; for nil checks (nil != 0 on bare metal).
;;; Timeouts bumped to 500000 for arm32 (DWC2 USB is slow).
(defun ssh-handle-connection (ssh)
  (let ((cb (- ssh #x20)))
    (ssh-send-version ssh)
    (when (zerop (ssh-receive-version ssh)) (return ()))
    (let ((kexinit (ssh-build-kexinit ssh)))
      (ssh-send-payload ssh kexinit (array-length kexinit)))
    (let ((cli-kex (ssh-receive-packet ssh 500000)))
      (when (not cli-kex) (return ()))
      (let ((cli-kex-payload (car cli-kex)))
        (let ((cli-kex-len (cdr cli-kex)))
          (when (not (eq (aref cli-kex-payload 0) 20)) (return ()))
          (ssh-mem-store (+ cb #x1F00) cli-kex-payload cli-kex-len)
          (setf (mem-ref (+ ssh #x20) :u32) cli-kex-len)
          (let ((kex-init (ssh-receive-packet ssh 500000)))
            (when (not kex-init) (return ()))
            (let ((kex-payload (car kex-init)))
              (when (not (eq (aref kex-payload 0) 30)) (return ()))
              (ssh-handle-kex ssh kex-payload (cdr kex-init))
              (ssh-send-newkeys ssh)
              (let ((nk (ssh-receive-packet ssh 800000)))
                (when (not nk) (return ()))
                (when (not (eq (aref (car nk) 0) 21)) (return ()))
                (ssh-derive-keys ssh)
                (ssh-message-loop ssh)))))))))
