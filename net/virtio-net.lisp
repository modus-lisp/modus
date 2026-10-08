;;;; virtio-net.lisp — virtio 1.x network driver, MODERN PCI transport, polled.
;;;;
;;;; Drives vendor 1AF4 device 1041 (modern-only, what a q35/PCIe KVM guest
;;;; sees — e.g. the VPSBG SEV-SNP VPS) and device 1000 (transitional) THROUGH
;;;; ITS MODERN CAPABILITIES.  The legacy I/O-port transport is NOT implemented:
;;;; this image's port primops (io-out-dword ...) take a COMPILE-TIME CONSTANT
;;;; port, and a legacy virtio BAR0 port is only known at run time.  A device
;;;; with no modern capabilities (QEMU disable-modern=on) is reported and
;;;; refused, and the E1000 is probed instead.
;;;;
;;;; Same contract as net/e1000.lisp, because net/ip.lisp, http-client.lisp and
;;;; the SSH server only ever call these four names:
;;;;   e1000-probe     find + initialise a NIC
;;;;   e1000-send      (buf len)  BUF a Lisp array of byte values, LEN bytes
;;;;   e1000-receive   -> frame length, or 0 when nothing is pending
;;;;   e1000-rx-buf    -> raw address of the frame just returned by
;;;;                      e1000-receive, Ethernet header at offset 0
;;;; and the MAC goes where e1000-init puts it, (e1000-state-base)+0x08, which
;;;; is where ip.lisp reads it.  The wrappers at the END of this file replace
;;;; e1000.lisp's by last-defun-wins and dispatch on the probe's verdict, so
;;;; the E1000 path is untouched: virtio when the PCI scan finds one, else E1000.
;;;;
;;;; Requires from the arch adapter (net/arch-x86-cl.lisp):
;;;;   pci-config-read / pci-config-write, print-hex32 / print-hex-byte,
;;;;   virtio-net-ring-base   DMA rings + buffers (device-visible memory)
;;;;   virtio-net-state-base  driver state (CPU-only memory)
;;;;   virtio-net-pt-page     two spare 4 KB pages for mapping a BAR above 4 GB
;;;;
;;;; LAYOUT (offsets from virtio-net-ring-base):
;;;;   +0x00000  RX descriptor table  (<=128 x 16 B)
;;;;   +0x01000  RX available ring
;;;;   +0x02000  RX used ring
;;;;   +0x03000  TX descriptor table
;;;;   +0x04000  TX available ring
;;;;   +0x05000  TX used ring
;;;;   +0x06000  TX buffers, 8 x 2048   (12-byte virtio_net_hdr + frame)
;;;;   +0x0A000  RX buffers, 128 x 2048 (12-byte virtio_net_hdr + frame)
;;;;   +0x4A000  end
;;;; One descriptor per buffer, header and frame in the same buffer: with
;;;; VIRTIO_F_VERSION_1 the device must accept any descriptor layout (spec
;;;; 2.7.4), and the header is always 12 bytes (5.1.6, num_buffers included).
;;;;
;;;; STATE (offsets from virtio-net-state-base; :u64 slots hold Lisp integers):
;;;;   +0x00 u32 active (1 = virtio selected)   +0x08..+0x20 u64 cap addresses,
;;;;   indexed by cfg_type: +0x08 common, +0x10 notify, +0x18 ISR, +0x20 device
;;;;   +0x28 u32 notify_off_multiplier   +0x30 u64 RX notify   +0x38 u64 TX notify
;;;;   +0x40 u32 RX queue size  +0x44 u32 TX queue size
;;;;   +0x48 u32 RX avail idx   +0x4C u32 RX last used  +0x50 u32 RX pending id
;;;;   +0x58 u64 current RX frame address
;;;;   +0x60 u32 TX avail idx   +0x64 u32 TX last used
;;;;   +0x68 u32 page-table pages consumed   +0x70 u64 RX frames  +0x78 u64 TX frames
;;;;
;;;; Ring indices are free-running 16-bit counters (spec 2.7.6/2.7.8); the slot
;;;; is idx AND (qsize-1), qsize a power of two.  No MOD (see e1000-hw-send).

;; ------------------------------------------------------------------
;; Sizes and layout
;; ------------------------------------------------------------------
(defun vnet-qmax () 128)                ; descriptors per queue we allocate for
(defun vnet-ntx () 8)                   ; TX buffers (sends are synchronous)
(defun vnet-hdr-len () 12)
(defun vnet-rx-desc () (virtio-net-ring-base))
(defun vnet-rx-avail () (+ (virtio-net-ring-base) #x1000))
(defun vnet-rx-used () (+ (virtio-net-ring-base) #x2000))
(defun vnet-tx-desc () (+ (virtio-net-ring-base) #x3000))
(defun vnet-tx-avail () (+ (virtio-net-ring-base) #x4000))
(defun vnet-tx-used () (+ (virtio-net-ring-base) #x5000))
(defun vnet-tx-buf (i) (+ (virtio-net-ring-base) #x6000 (* i 2048)))
(defun vnet-rx-buf (i) (+ (virtio-net-ring-base) #xA000 (* i 2048)))

(defun vnet-st (off) (+ (virtio-net-state-base) off))
(defun vnet-common () (mem-ref (vnet-st #x08) :u64))
(defun vnet-devcfg () (mem-ref (vnet-st #x20) :u64))

(defun virtio-net-active-p ()
  (= (mem-ref (vnet-st 0) :u32) 1))

;; ------------------------------------------------------------------
;; Small helpers.  Every shift count is a CONSTANT <= 30 (compile-ash inlines
;; those exactly; a variable count goes through bignum-ash).
;; ------------------------------------------------------------------
(defun vnet-hi32 (hi) (ash (ash hi 16) 16))          ; hi * 2^32
(defun vnet-hi-of (v) (logand (ash (ash v -16) -16) #xFFFFFFFF))

(defun vnet-byte-of (v k)
  (cond ((= k 0) (logand v #xFF))
        ((= k 1) (logand (ash v -8) #xFF))
        ((= k 2) (logand (ash v -16) #xFF))
        (t (logand (ash v -24) #xFF))))

(defun vnet-cfg-byte (bus dev reg)
  (vnet-byte-of (pci-config-read bus dev 0 reg) (logand reg 3)))

;; 64-bit device / page-table field as two 32-bit halves, HIGH half first, so a
;; half-written entry never has its low (present / address-low) word live with
;; a stale high word.
(defun vnet-write64 (addr v)
  (setf (mem-ref (+ addr 4) :u32) (vnet-hi-of v))
  (setf (mem-ref addr :u32) (logand v #xFFFFFFFF)))

(defun vnet-read64 (addr)
  (+ (mem-ref addr :u32) (vnet-hi32 (mem-ref (+ addr 4) :u32))))

;; ------------------------------------------------------------------
;; MMIO above 4 GB.  The boot stub identity-maps exactly the first 4 GB with
;; 2 MB pages (PML4 0x10000, PDPT 0x11000, PDs 0x12000-0x15FFF), and OVMF puts
;; a 64-bit BAR — which a modern virtio device's is — in its 64-bit window,
;; above 4 GB.  Map the 2 MB pages covering [ADDR, ADDR+LEN) uncached, using
;; at most two spare page-table pages.  Under SEV-SNP the table pointers carry
;; the C-bit (the tables are private memory) and the MMIO leaf does not; the
;; C-bit is read back out of PML4[0]'s high word (the stub wrote 0x11003|C).
;; Returns 1, or 0 if it could not map.
;; ------------------------------------------------------------------
(defun vnet-cbit () (vnet-hi32 (mem-ref (+ #x10000 4) :u32)))

(defun vnet-pt-alloc ()
  (let ((n (mem-ref (vnet-st #x68) :u32)))
    (if (>= n 2)
        0
        (let ((pg (virtio-net-pt-page n)))
          (setf (mem-ref (vnet-st #x68) :u32) (+ n 1))
          (dotimes (i 1024) (setf (mem-ref (+ pg (* i 4)) :u32) 0))
          pg))))

;; Table pointed to by entry E (an address), allocating it if not present.
(defun vnet-pt-next (e)
  (let ((v (vnet-read64 e)))
    (if (= (logand v 1) 1)
        (logand (- v (vnet-cbit)) #xFFFFF000)   ; tables live below 4 GB
        (let ((pg (vnet-pt-alloc)))
          (when (not (zerop pg))
            (vnet-write64 e (+ (logior pg 3) (vnet-cbit))))
          pg))))

(defun vnet-map-2m (pa)
  (let ((pdpt (if (zerop (ash (ash pa -30) -9))
                  #x11000
                  (vnet-pt-next (+ #x10000 (* 8 (logand (ash (ash pa -30) -9) 511)))))))
    (if (zerop pdpt)
        0
        (let ((pd (vnet-pt-next (+ pdpt (* 8 (logand (ash pa -30) 511))))))
          (if (zerop pd)
              0
              (progn
                ;; P | RW | PWT | PCD | PS = 0x9B: an uncached 2 MB device page.
                (vnet-write64 (+ pd (* 8 (logand (ash pa -21) 511)))
                              (logior (logand pa (- 0 #x200000)) #x9B))
                1))))))

;; A BAR BELOW 4 GB sits in the boot stub's identity map, where every page except the
;; shared one carries the C-bit.  An access through an encrypted mapping is an MMIO #VC
;; (NPF), which the handler does not implement, so the page must be remapped: each 2 MB
;; leaf covering the BAR becomes uncached (PWT|PCD) and C-less.  The identity map's PDs
;; are the four pages at 0x12000..0x15FFF (PD = pa>>30, entry = (pa>>21)&511).  A
;; page that is not a 2 MB leaf is refused rather than split.
(defun vnet-map-low-mmio (addr len)
  (let ((pa (logand addr (- 0 #x200000))) (ok 1))
    (dotimes (i 512 ok)
      (when (< pa (+ addr len))
        (let ((e (+ #x12000 (* 4096 (ash pa -30)) (* 8 (logand (ash pa -21) 511)))))
          (if (zerop (logand (vnet-read64 e) #x80))
              (setq ok 0)
              (vnet-write64 e (logior pa #x9B))))
        (setq pa (+ pa #x200000))))))

(defun vnet-map-mmio (addr len)
  (if (<= (+ addr len) #x100000000)
      (vnet-map-low-mmio addr len)
      ;; PML4[0] must point at the PDPT at 0x11000 (present).  Mask the low 12
      ;; bits: the CPU has set ACCESSED (0x20) in it by now, so it reads 0x11023.
      (if (not (= (logand (mem-ref #x10000 :u32) #xFFFFF001) #x11001))
          0                                    ; not the page-table layout we know
          (let ((pa (logand addr (- 0 #x200000)))
                (ok 1))
            (dotimes (i 512)
              (when (< pa (+ addr len))
                (when (zerop (vnet-map-2m pa)) (setq ok 0))
                (setq pa (+ pa #x200000))))
            ok))))

;; ------------------------------------------------------------------
;; PCI discovery
;; ------------------------------------------------------------------
;; A memory BAR's address (64-bit BARs combined), or 0 for an I/O BAR / none.
(defun vnet-bar-addr (bus dev bar)
  (if (> bar 5)
      0
      (let ((lo (pci-config-read bus dev 0 (+ #x10 (* bar 4)))))
        (if (= (logand lo 1) 1)
            0
            (if (= (logand lo 6) 4)
                (+ (logand lo #xFFFFFFF0)
                   (vnet-hi32 (pci-config-read bus dev 0 (+ #x14 (* bar 4)))))
                (logand lo #xFFFFFFF0))))))

;; Walk the capability list; record the FIRST virtio_pci_cap of each cfg_type
;; 1..4 (spec 4.1.4: the driver should use the first one it can use).
(defun vnet-read-caps (bus dev)
  (let ((ptr (if (zerop (logand (ash (pci-config-read bus dev 0 4) -16) #x10))
                 0
                 (logand (vnet-cfg-byte bus dev #x34) #xFC))))
    (dotimes (guard 48)
      (when (not (zerop ptr))
        (let ((id (vnet-cfg-byte bus dev ptr))
              (next (logand (vnet-cfg-byte bus dev (+ ptr 1)) #xFC)))
          (when (= id 9)
            (let ((type (vnet-cfg-byte bus dev (+ ptr 3)))
                  (bar (vnet-cfg-byte bus dev (+ ptr 4))))
              (when (and (>= type 1) (<= type 4)
                         (zerop (mem-ref (vnet-st (* 8 type)) :u64)))
                (let ((base (vnet-bar-addr bus dev bar))
                      (off (pci-config-read bus dev 0 (+ ptr 8)))
                      (len (pci-config-read bus dev 0 (+ ptr 12)))
                      (mapped 0))
                  (setq mapped (if (zerop base) 0 (vnet-map-mmio (+ base off) len)))
                  ;; VNET:CAP <type> BAR<n> @<address> +<len> map=<1|0>
                  (write-string-serial "VNET:CAP ") (print-dec type)
                  (write-string-serial " BAR") (print-dec bar)
                  (write-string-serial " @") (print-hex32 (vnet-hi-of (+ base off)))
                  (print-hex32 (logand (+ base off) #xFFFFFFFF))
                  (write-string-serial " +") (print-hex32 len)
                  (write-string-serial " map=") (print-dec mapped) (write-char-serial 10)
                  (when (and (not (zerop base)) (= mapped 1))
                    (setf (mem-ref (vnet-st (* 8 type)) :u64) (+ base off))
                    (when (= type 2)
                      (setf (mem-ref (vnet-st #x28) :u32)
                            (pci-config-read bus dev 0 (+ ptr 16)))))))))
          (setq ptr next))))))

;; Scan every bus for 1AF4:1041 / 1AF4:1000.  All buses, not just 0: on q35 a
;; PCIe device sits behind a root port (bus 1+), which is exactly where a
;; modern-only 1041 shows up.  Function 0 only.  Returns (bus*32 + dev) + 1,
;; or 0 if none.
(defun vnet-pci-find ()
  (let ((found 0))
    (dotimes (bus 256)
      (when (zerop found)
        (dotimes (dev 32)
          (when (zerop found)
            (let ((id (pci-config-read bus dev 0 0)))
              (when (or (eq id #x10411AF4) (eq id #x10001AF4))
                (setq found (+ (* bus 32) dev 1))))))))
    found))

;; ------------------------------------------------------------------
;; Device bring-up (virtio 1.x, 3.1.1)
;; ------------------------------------------------------------------
(defun vnet-status () (mem-ref (+ (vnet-common) #x14) :u8))
(defun vnet-set-status (s) (setf (mem-ref (+ (vnet-common) #x14) :u8) s))

;; Program queue Q at DESC/AVAIL/USED; record size and notify address at
;; state +SZOFF / +NOFF.  Returns the size used, or 0.
(defun vnet-setup-queue (q desc avail used szoff noff)
  (let ((c (vnet-common)))
    (setf (mem-ref (+ c #x16) :u16) q)                ; queue_select
    (let ((max (mem-ref (+ c #x18) :u16)))            ; queue_size (= device max)
      (if (zerop max)
          0
          (let ((qs (if (< max (vnet-qmax)) max (vnet-qmax))))
            (setf (mem-ref (+ c #x18) :u16) qs)
            (setf (mem-ref (+ c #x1A) :u16) #xFFFF)   ; queue_msix_vector = NO_VECTOR
            (vnet-write64 (+ c #x20) desc)            ; queue_desc
            (vnet-write64 (+ c #x28) avail)           ; queue_driver
            (vnet-write64 (+ c #x30) used)            ; queue_device
            (setf (mem-ref (vnet-st szoff) :u32) qs)
            (setf (mem-ref (vnet-st noff) :u64)
                  (+ (mem-ref (vnet-st #x10) :u64)
                     (* (mem-ref (+ c #x1E) :u16)      ; queue_notify_off
                        (mem-ref (vnet-st #x28) :u32))))
            (setf (mem-ref (+ c #x1C) :u16) 1)        ; queue_enable
            qs)))))

(defun vnet-zero (addr n)
  (dotimes (i (ash n -2)) (setf (mem-ref (+ addr (* i 4)) :u32) 0)))

(defun vnet-fail (why)
  (write-string-serial "VNET:FAIL ") (write-string-serial why) (write-char-serial 10)
  (when (not (zerop (vnet-common)))
    (vnet-set-status (logior (vnet-status) 128)))   ; FAILED
  0)

(defun vnet-init ()
  (let ((c (vnet-common)))
    ;; 1. reset, and wait for it to read back 0
    (vnet-set-status 0)
    (dotimes (i 100000)
      (if (zerop (vnet-status)) (setq i 100001) (io-delay)))
    ;; 2. ACKNOWLEDGE | DRIVER
    (vnet-set-status 1)
    (vnet-set-status 3)
    ;; 3. features.  Accept MAC (5), VERSION_1 (32), ACCESS_PLATFORM (33).
    ;;    ACCESS_PLATFORM is what an SEV host offers (iommu_platform=on): it
    ;;    means "DMA addresses are platform addresses", which with no IOMMU and
    ;;    rings in the shared region is exactly what this driver hands it.
    (setf (mem-ref c :u32) 0)
    (let ((f0 (mem-ref (+ c 4) :u32)))
      (setf (mem-ref c :u32) 1)
      (let ((f1 (mem-ref (+ c 4) :u32)))
        (write-string-serial "VNET:FEAT=") (print-hex32 f1) (print-hex32 f0)
        (write-char-serial 10)
        (if (zerop (logand f1 1))
            (vnet-fail "no VERSION_1")
            (let ((want0 (logand f0 #x20))
                  (want1 (logand f1 3)))
              (setf (mem-ref (+ c 8) :u32) 0)
              (setf (mem-ref (+ c #x0C) :u32) want0)
              (setf (mem-ref (+ c 8) :u32) 1)
              (setf (mem-ref (+ c #x0C) :u32) want1)
              ;; 4. FEATURES_OK, and check it stuck
              (vnet-set-status 11)
              (if (zerop (logand (vnet-status) 8))
                  (vnet-fail "FEATURES_OK refused")
                  (vnet-init-queues want0))))))))

(defun vnet-init-queues (want0)
  (let ((nq (mem-ref (+ (vnet-common) #x12) :u16)))
    (vnet-zero (virtio-net-ring-base) #x6000)
    (if (< nq 2)
        (vnet-fail "fewer than 2 queues")
        (let ((rq (vnet-setup-queue 0 (vnet-rx-desc) (vnet-rx-avail) (vnet-rx-used) #x40 #x30))
              (tq (vnet-setup-queue 1 (vnet-tx-desc) (vnet-tx-avail) (vnet-tx-used) #x44 #x38)))
          (if (or (zerop rq) (zerop tq))
              (vnet-fail "queue size 0")
              (progn
                (vnet-init-mac want0)
                ;; RX: every descriptor a device-writable 2048-byte buffer, all
                ;; offered.  avail.flags = 1 (NO_INTERRUPT): nothing takes IRQs.
                (dotimes (i rq)
                  (let ((d (+ (vnet-rx-desc) (* i 16))))
                    (vnet-write64 d (vnet-rx-buf i))
                    (setf (mem-ref (+ d 8) :u32) 2048)
                    (setf (mem-ref (+ d 12) :u16) 2)     ; VIRTQ_DESC_F_WRITE
                    (setf (mem-ref (+ d 14) :u16) 0)
                    (setf (mem-ref (+ (vnet-rx-avail) 4 (* i 2)) :u16) i)))
                (setf (mem-ref (vnet-rx-avail) :u16) 1)
                (setf (mem-ref (+ (vnet-rx-avail) 2) :u16) rq)
                (setf (mem-ref (vnet-st #x48) :u32) rq)
                (setf (mem-ref (vnet-st #x4C) :u32) 0)
                (setf (mem-ref (vnet-st #x50) :u32) #xFFFF)
                ;; TX: fixed buffer per descriptor, header zeroed once.
                (dotimes (i (vnet-ntx))
                  (let ((d (+ (vnet-tx-desc) (* i 16))))
                    (vnet-zero (vnet-tx-buf i) 16)
                    (vnet-write64 d (vnet-tx-buf i))
                    (setf (mem-ref (+ d 8) :u32) 0)
                    (setf (mem-ref (+ d 12) :u16) 0)
                    (setf (mem-ref (+ d 14) :u16) 0)))
                (setf (mem-ref (vnet-tx-avail) :u16) 1)
                (setf (mem-ref (vnet-st #x60) :u32) 0)
                (setf (mem-ref (vnet-st #x64) :u32) 0)
                ;; 5. DRIVER_OK, then tell the device the RX buffers are there.
                (vnet-set-status 15)
                (setf (mem-ref (mem-ref (vnet-st #x30) :u64) :u16) 0)
                (write-string-serial "VNET:Q=") (print-dec rq) (write-char-serial 47)
                (print-dec tq) (write-string-serial " STATUS=") (print-hex-byte (vnet-status))
                (write-char-serial 10)
                1))))))

;; MAC from the device config (offset 0) when VIRTIO_NET_F_MAC was accepted;
;; otherwise a fixed locally-administered one.  Stored where e1000-init stores
;; it and ip.lisp reads it.
(defun vnet-init-mac (want0)
  (let ((state (e1000-state-base)) (dc (vnet-devcfg)))
    (dotimes (i 6)
      (setf (mem-ref (+ state 8 i) :u8)
            (if (or (zerop want0) (zerop dc))
                (if (= i 0) 2 (if (= i 5) 1 0))
                (mem-ref (+ dc i) :u8))))
    (write-string-serial "MAC:")
    (dotimes (i 6)
      (when (> i 0) (write-char-serial 58))
      (print-hex-byte (mem-ref (+ state 8 i) :u8)))
    (write-char-serial 10)
    ;; Unconfigured until DHCP answers (see e1000-init for why not 10.0.2.15).
    (setf (mem-ref (+ state #x18) :u32) 0)
    (setf (mem-ref (+ state #x1C) :u32) 0)))

;; Find + initialise.  Returns 1 if a virtio NIC is up, else 0 (and the
;; caller falls back to the E1000).
(defun virtio-net-probe ()
  (vnet-zero (virtio-net-state-base) #x100)
  (let ((loc (vnet-pci-find)))
    (if (zerop loc)
        (progn (write-string-serial "VNET:NF") (write-char-serial 10) 0)
        (let ((bus (ash (- loc 1) -5)) (dev (logand (- loc 1) 31)))
          (write-string-serial "VNET:PCI ") (print-dec bus) (write-char-serial 58)
          (print-dec dev) (write-string-serial " ID=")
          (print-hex32 (pci-config-read bus dev 0 0)) (write-char-serial 10)
          ;; memory space + bus master (+ I/O, as pci-find-e1000 does)
          (pci-config-write bus dev 0 4 (logior (pci-config-read bus dev 0 4) 7))
          (vnet-read-caps bus dev)
          (write-string-serial "VNET:COMMON=") (print-hex32 (vnet-hi-of (vnet-common)))
          (print-hex32 (logand (vnet-common) #xFFFFFFFF))
          (write-string-serial " NOTIFY=") (print-hex32 (logand (mem-ref (vnet-st #x10) :u64) #xFFFFFFFF))
          (write-string-serial " DEVCFG=") (print-hex32 (logand (vnet-devcfg) #xFFFFFFFF))
          (write-string-serial " PT=") (print-dec (mem-ref (vnet-st #x68) :u32))
          (write-char-serial 10)
          (if (or (zerop (vnet-common)) (zerop (mem-ref (vnet-st #x10) :u64)))
              (vnet-fail "no modern common/notify capability (legacy-only device?)")
              (if (= (vnet-init) 1)
                  (progn
                    (setf (mem-ref (vnet-st 0) :u32) 1)
                    (write-string-serial "VNET:OK") (write-char-serial 10)
                    1)
                  0))))))

;; ------------------------------------------------------------------
;; Data path
;; ------------------------------------------------------------------
(defun vnet-notify (noff) (setf (mem-ref (mem-ref (vnet-st noff) :u64) :u16)
                                (if (= noff #x30) 0 1)))

;; Send LEN bytes of BUF.  Waits for the device to consume it, like
;; e1000-hw-send.  Returns 1 when the device reported it used, else 0.
(defun virtio-net-send (buf len)
  (let* ((n (if (> len 1514) 1514 len))
         (ai (mem-ref (vnet-st #x60) :u32))
         (slot (logand ai (- (vnet-ntx) 1)))
         (b (vnet-tx-buf slot))
         (d (+ (vnet-tx-desc) (* slot 16)))
         (qs (mem-ref (vnet-st #x44) :u32)))
    (dotimes (i 3) (setf (mem-ref (+ b (* i 4)) :u32) 0))   ; virtio_net_hdr
    (dotimes (i n) (setf (mem-ref (+ b 12 i) :u8) (aref buf i)))
    ;; Pad runts to the 60-byte Ethernet minimum (the E1000 does it in hw).
    (let ((wire (if (< n 60) 60 n)))
      (dotimes (i (- wire n)) (setf (mem-ref (+ b 12 n i) :u8) 0))
      (setf (mem-ref (+ d 8) :u32) (+ 12 wire)))
    (setf (mem-ref (+ (vnet-tx-avail) 4 (* 2 (logand ai (- qs 1)))) :u16) slot)
    (let ((next (logand (+ ai 1) #xFFFF)))
      (setf (mem-ref (vnet-st #x60) :u32) next)
      (setf (mem-ref (+ (vnet-tx-avail) 2) :u16) next)
      (vnet-notify #x38)
      (let ((done 0))
        (dotimes (try 100000)
          (when (= (mem-ref (+ (vnet-tx-used) 2) :u16) next)
            (setq done 1)
            (setq try 100001)))
        (setf (mem-ref (vnet-st #x64) :u32) (mem-ref (+ (vnet-tx-used) 2) :u16))
        (setf (mem-ref (vnet-st #x78) :u64) (+ (mem-ref (vnet-st #x78) :u64) 1))
        done))))

;; Re-offer the buffer handed out by the PREVIOUS receive.  Deferred for the
;; same reason as e1000-hw-receive's RDT write: the caller reads the frame
;; through e1000-rx-buf AFTER receive returns, so the buffer must stay ours
;; until the next receive call.
(defun vnet-rx-rearm ()
  (let ((id (mem-ref (vnet-st #x50) :u32)))
    (when (not (= id #xFFFF))
      (let ((ai (mem-ref (vnet-st #x48) :u32))
            (qs (mem-ref (vnet-st #x40) :u32)))
        (setf (mem-ref (+ (vnet-rx-avail) 4 (* 2 (logand ai (- qs 1)))) :u16) id)
        (let ((next (logand (+ ai 1) #xFFFF)))
          (setf (mem-ref (vnet-st #x48) :u32) next)
          (setf (mem-ref (+ (vnet-rx-avail) 2) :u16) next))
        (setf (mem-ref (vnet-st #x50) :u32) #xFFFF)
        ;; used.flags bit 0 = NO_NOTIFY: the device says it is polling anyway
        (when (zerop (logand (mem-ref (vnet-rx-used) :u16) 1))
          (vnet-notify #x30))))))

;; Length of the next received frame (header stripped), or 0.
(defun virtio-net-receive ()
  (vnet-rx-rearm)
  (let ((lu (mem-ref (vnet-st #x4C) :u32)))
    (if (= (mem-ref (+ (vnet-rx-used) 2) :u16) lu)
        0
        (let* ((qs (mem-ref (vnet-st #x40) :u32))
               (e (+ (vnet-rx-used) 4 (* 8 (logand lu (- qs 1)))))
               (id (mem-ref e :u32))
               (len (mem-ref (+ e 4) :u32)))
          (setf (mem-ref (vnet-st #x4C) :u32) (logand (+ lu 1) #xFFFF))
          (setf (mem-ref (vnet-st #x50) :u32) id)
          (setf (mem-ref (vnet-st #x58) :u64) (+ (vnet-rx-buf id) 12))
          (setf (mem-ref (vnet-st #x70) :u64) (+ (mem-ref (vnet-st #x70) :u64) 1))
          (if (> len 12) (- len 12) 0)))))

(defun virtio-net-rx-buf () (mem-ref (vnet-st #x58) :u64))

;; Debug: one line of driver counters, for the REPL.
(defun virtio-net-stats ()
  (write-string-serial "VNET rx=") (print-dec (mem-ref (vnet-st #x70) :u64))
  (write-string-serial " tx=") (print-dec (mem-ref (vnet-st #x78) :u64))
  (write-string-serial " rx-used=") (print-dec (mem-ref (+ (vnet-rx-used) 2) :u16))
  (write-string-serial " tx-used=") (print-dec (mem-ref (+ (vnet-tx-used) 2) :u16))
  (write-string-serial " status=") (print-hex-byte (vnet-status))
  (write-char-serial 10))

;; ------------------------------------------------------------------
;; The API wrappers — replace e1000.lisp's by last-defun-wins.
;; ------------------------------------------------------------------
(defun e1000-probe ()
  (if (= (virtio-net-probe) 1) 1 (e1000-hw-probe)))
(defun e1000-send (buf len)
  (if (virtio-net-active-p) (virtio-net-send buf len) (e1000-hw-send buf len)))
(defun e1000-receive ()
  (if (virtio-net-active-p) (virtio-net-receive) (e1000-hw-receive)))
(defun e1000-rx-buf ()
  (if (virtio-net-active-p) (virtio-net-rx-buf) (e1000-hw-rx-buf)))
