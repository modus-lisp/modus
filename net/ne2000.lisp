;;;; ne2000.lisp — NE2000 ISA NIC driver for i386
;;;;
;;;; Uses I/O port access only (all values < 0xFFFF, fixnum safe on i386).
;;;; QEMU: -device ne2k_isa,netdev=net0,iobase=0x300,irq=9
;;;;       -netdev user,id=net0,hostfwd=tcp::2222-:22
;;;;
;;;; Package MODUS.NET.NE2000 (builder packages step 3).  It implements
;;;; MODUS.NET's NIC interface -- NE2K-SEND / -RECEIVE / -RX-BUF / -PROBE --
;;;; and the i386 image binds the interface to them at boot (net/nic.lisp).
;;;; Its host buffers and state block are the BOARD's (net/arch-i386-cl.lisp):
;;;; NE2K-RX-HOST and E1000-STATE-BASE, which MODUS-INT exports to drivers.
;;;;
;;;; NOTE: MVM requires io-in-byte/io-out-byte ports to be compile-time constants.
;;;; All register accesses use literal port numbers (base 0x300 + register offset).

(in-package :modus.net.ne2000)

;; ================================================================
;; NE2000 Constants
;; ================================================================

;; NE2000 NIC RAM pages (each 256 bytes)
;; NE2000 (16-bit): pages 0x40-0xBF = 32KB
(defun ne2k-tx-start () #x40)
(defun ne2k-rx-start () #x46)
(defun ne2k-rx-stop ()  #xC0)

;; ================================================================
;; Register read/write with constant port numbers
;; ================================================================
;; Base = 0x300, registers 0x00-0x0F, data port 0x10, reset 0x1F
;;
;; ne2k-read/ne2k-write use when-chains to dispatch on constant reg values.
;; The MVM compiler inlines each branch's io-in-byte/io-out-byte with its
;; constant port. Only one branch executes at runtime.

(defun ne2k-read (reg)
  (if (eq reg 0) (io-in-byte #x300)
  (if (eq reg 1) (io-in-byte #x301)
  (if (eq reg 2) (io-in-byte #x302)
  (if (eq reg 3) (io-in-byte #x303)
  (if (eq reg 4) (io-in-byte #x304)
  (if (eq reg 5) (io-in-byte #x305)
  (if (eq reg 6) (io-in-byte #x306)
  (if (eq reg 7) (io-in-byte #x307)
  (if (eq reg 8) (io-in-byte #x308)
  (if (eq reg 9) (io-in-byte #x309)
  (if (eq reg 10) (io-in-byte #x30A)
  (if (eq reg 11) (io-in-byte #x30B)
  (if (eq reg 12) (io-in-byte #x30C)
  (if (eq reg 13) (io-in-byte #x30D)
  (if (eq reg 14) (io-in-byte #x30E)
  (io-in-byte #x30F)))))))))))))))))

(defun ne2k-write (reg val)
  (if (eq reg 0) (io-out-byte #x300 val)
  (if (eq reg 1) (io-out-byte #x301 val)
  (if (eq reg 2) (io-out-byte #x302 val)
  (if (eq reg 3) (io-out-byte #x303 val)
  (if (eq reg 4) (io-out-byte #x304 val)
  (if (eq reg 5) (io-out-byte #x305 val)
  (if (eq reg 6) (io-out-byte #x306 val)
  (if (eq reg 7) (io-out-byte #x307 val)
  (if (eq reg 8) (io-out-byte #x308 val)
  (if (eq reg 9) (io-out-byte #x309 val)
  (if (eq reg 10) (io-out-byte #x30A val)
  (if (eq reg 11) (io-out-byte #x30B val)
  (if (eq reg 12) (io-out-byte #x30C val)
  (if (eq reg 13) (io-out-byte #x30D val)
  (if (eq reg 14) (io-out-byte #x30E val)
  (io-out-byte #x30F val)))))))))))))))))

;; ================================================================
;; Page select helpers
;; ================================================================

;; CR bits: [7:6]=page [5:3]=DMA [2]=TXP [1]=STA [0]=STP
;; DMA abort = 110 = 0x30 in bits 5:3
;; STA = bit 1

(defun ne2k-page0-stop ()
  (io-out-byte #x300 #x21))

(defun ne2k-page0-start ()
  (io-out-byte #x300 #x22))

(defun ne2k-page1-start ()
  (io-out-byte #x300 #x62))

;; ================================================================
;; Initialization
;; ================================================================

(defun ne2k-reset ()
  ;; Read reset port to trigger reset, then write it
  (io-in-byte #x31F)
  (io-out-byte #x31F 0)
  ;; Wait for reset (ISR bit 7)
  (let ((i 0))
    (loop
      (when (>= i 10000) (return 0))
      (when (not (zerop (logand (io-in-byte #x307) #x80)))
        (return 1))
      (setq i (+ i 1))))
  ;; Clear ISR
  (io-out-byte #x307 #xFF))

(defun ne2k-init ()
  ;; Stop NIC, abort DMA
  (ne2k-page0-stop)
  ;; DCR: byte-wide DMA (WTS=0), normal mode (LS=1), 8-byte FIFO
  (io-out-byte #x30E #x48)
  ;; Clear remote byte count
  (io-out-byte #x30A 0)
  (io-out-byte #x30B 0)
  ;; RCR: accept broadcast + unicast (AB=1, AM=0)
  (io-out-byte #x30C #x04)
  ;; TCR: internal loopback during setup
  (io-out-byte #x30D #x02)
  ;; Ring buffer page boundaries
  (io-out-byte #x301 (ne2k-rx-start))   ;; PSTART
  (io-out-byte #x302 (ne2k-rx-stop))    ;; PSTOP
  (io-out-byte #x303 (ne2k-rx-start))   ;; BNRY
  ;; Clear ISR
  (io-out-byte #x307 #xFF)
  ;; Enable PRX interrupt (bit 0) — packet received fires IRQ.
  ;; IRQs stay masked at CPU (EFLAGS.IF=0) so no ISR runs for receive,
  ;; but the pending interrupt signal wakes HLT in io-delay.
  (io-out-byte #x30F #x01)
  ;; Read MAC from NE2000 PROM via remote DMA
  ;; PROM is at NIC address 0x0000, 32 bytes. In byte mode, each MAC byte
  ;; is duplicated: [MAC0,MAC0,MAC1,MAC1,...]. Read 12 bytes, use even offsets.
  (ne2k-dma-read 0 12 (ne2k-rx-host))
  ;; Switch to page 1 for MAC and CURR
  (ne2k-page1-start)
  ;; Set MAC from PROM (even offsets: 0,2,4,6,8,10)
  (io-out-byte #x301 (mem-ref (ne2k-rx-host) :u8))         ;; PAR0
  (io-out-byte #x302 (mem-ref (+ (ne2k-rx-host) 2) :u8))   ;; PAR1
  (io-out-byte #x303 (mem-ref (+ (ne2k-rx-host) 4) :u8))   ;; PAR2
  (io-out-byte #x304 (mem-ref (+ (ne2k-rx-host) 6) :u8))   ;; PAR3
  (io-out-byte #x305 (mem-ref (+ (ne2k-rx-host) 8) :u8))   ;; PAR4
  (io-out-byte #x306 (mem-ref (+ (ne2k-rx-host) 10) :u8))  ;; PAR5
  ;; CURR = rx_start + 1 (first free page for NIC to write)
  (io-out-byte #x307 (+ (ne2k-rx-start) 1))
  ;; Accept all multicast
  (io-out-byte #x308 #xFF)
  (io-out-byte #x309 #xFF)
  (io-out-byte #x30A #xFF)
  (io-out-byte #x30B #xFF)
  (io-out-byte #x30C #xFF)
  (io-out-byte #x30D #xFF)
  (io-out-byte #x30E #xFF)
  (io-out-byte #x30F #xFF)
  ;; Switch back to page 0
  (ne2k-page0-start)
  ;; TCR: normal transmit (no loopback)
  (io-out-byte #x30D #x00)
  ;; Store MAC in state area for networking code (from PROM)
  (let ((sb (e1000-state-base))
        (rx (ne2k-rx-host)))
    (setf (mem-ref (+ sb 8) :u8) (mem-ref rx :u8))
    (setf (mem-ref (+ sb 9) :u8) (mem-ref (+ rx 2) :u8))
    (setf (mem-ref (+ sb 10) :u8) (mem-ref (+ rx 4) :u8))
    (setf (mem-ref (+ sb 11) :u8) (mem-ref (+ rx 6) :u8))
    (setf (mem-ref (+ sb 12) :u8) (mem-ref (+ rx 8) :u8))
    (setf (mem-ref (+ sb 13) :u8) (mem-ref (+ rx 10) :u8))))

;; ================================================================
;; Remote DMA: read bytes from NIC RAM to host
;; ================================================================

(defun ne2k-dma-read (nic-addr len dst-addr)
  ;; Set up remote DMA read
  (io-out-byte #x300 #x22)       ;; CR: start, abort DMA
  ;; Remote start address
  (io-out-byte #x308 (logand nic-addr #xFF))
  (io-out-byte #x309 (logand (ash nic-addr -8) #xFF))
  ;; Remote byte count
  (io-out-byte #x30A (logand len #xFF))
  (io-out-byte #x30B (logand (ash len -8) #xFF))
  ;; CR: start, remote read
  (io-out-byte #x300 #x0A)
  ;; Read bytes from data port (0x310)
  (dotimes (i len)
    (setf (mem-ref (+ dst-addr i) :u8) (io-in-byte #x310)))
  ;; Wait for remote DMA complete (ISR bit 6)
  (let ((tries 0))
    (loop
      (when (>= tries 10000) (return 0))
      (when (not (zerop (logand (io-in-byte #x307) #x40)))
        (return 1))
      (setq tries (+ tries 1))))
  ;; Clear RDC flag
  (io-out-byte #x307 #x40))

;; ================================================================
;; Remote DMA: write bytes from host to NIC RAM
;; ================================================================

(defun ne2k-dma-write-arr (nic-addr len arr)
  ;; Set up remote DMA write
  (io-out-byte #x300 #x22)       ;; CR: start, abort DMA
  ;; Remote start address
  (io-out-byte #x308 (logand nic-addr #xFF))
  (io-out-byte #x309 (logand (ash nic-addr -8) #xFF))
  ;; Remote byte count (round up to even for 16-bit NIC compatibility)
  (let ((rlen (if (zerop (logand len 1)) len (+ len 1))))
    (io-out-byte #x30A (logand rlen #xFF))
    (io-out-byte #x30B (logand (ash rlen -8) #xFF)))
  ;; CR: start, remote write
  (io-out-byte #x300 #x12)
  ;; Write bytes to data port (0x310)
  (dotimes (i len)
    (io-out-byte #x310 (aref arr i)))
  ;; Pad to even length
  (when (not (zerop (logand len 1)))
    (io-out-byte #x310 0))
  ;; Wait for remote DMA complete
  (let ((tries 0))
    (loop
      (when (>= tries 10000) (return 0))
      (when (not (zerop (logand (io-in-byte #x307) #x40)))
        (return 1))
      (setq tries (+ tries 1))))
  (io-out-byte #x307 #x40))

;; ================================================================
;; Send packet
;; ================================================================

(defun ne2k-send (arr len)
  ;; Write packet to TX buffer pages via remote DMA
  (let ((nic-addr (* (ne2k-tx-start) 256)))
    (ne2k-dma-write-arr nic-addr len arr))
  ;; Set transmit page and byte count
  (io-out-byte #x304 (ne2k-tx-start))   ;; TPSR
  ;; Minimum Ethernet frame = 60 bytes (64 with FCS)
  (let ((tx-len (if (< len 60) 60 len)))
    (io-out-byte #x305 (logand tx-len #xFF))
    (io-out-byte #x306 (logand (ash tx-len -8) #xFF)))
  ;; CR: start + transmit
  (io-out-byte #x300 #x26)
  ;; Wait for transmit complete (ISR bit 0 or bit 3 for error)
  (let ((tries 0))
    (loop
      (when (>= tries 50000) (return 0))
      (let ((isr (io-in-byte #x307)))
        (when (not (zerop (logand isr #x0A)))
          ;; TX complete or TX error — clear flags
          (io-out-byte #x307 (logand isr #x0A))
          (return 1)))
      (setq tries (+ tries 1)))))

;; ================================================================
;; Receive packet
;; ================================================================

(defun ne2k-get-curr ()
  ;; Read CURR from page 1
  (ne2k-page1-start)
  (let ((curr (io-in-byte #x307)))
    (ne2k-page0-start)
    curr))

(defun ne2k-receive ()
  ;; Clear PRX interrupt flag (bit 0 of ISR at 0x307) so HLT can sleep
  ;; until next packet arrives. Write 0x01 to clear PRX bit (write-1-to-clear).
  (io-out-byte #x307 #x01)
  ;; Check if packet available: BNRY+1 != CURR
  (let ((bnry (io-in-byte #x303))
        (curr (ne2k-get-curr)))
    ;; Next page to read = BNRY + 1 (wrap at rx-stop)
    (let ((next (+ bnry 1)))
      (when (>= next (ne2k-rx-stop))
        (setq next (ne2k-rx-start)))
      (if (eq next curr)
          0  ;; No packet
          ;; Read 4-byte NE2000 header from ring buffer
          (let ((hdr-addr (* next 256))
                (hdr-buf (ne2k-rx-host)))
            ;; Read 4-byte NIC header: status, next_page, len_lo, len_hi
            (ne2k-dma-read hdr-addr 4 hdr-buf)
            (let ((status (mem-ref hdr-buf :u8))
                  (next-page (mem-ref (+ hdr-buf 1) :u8))
                  (pkt-len-lo (mem-ref (+ hdr-buf 2) :u8))
                  (pkt-len-hi (mem-ref (+ hdr-buf 3) :u8)))
              (let ((pkt-len (logior pkt-len-lo (ash pkt-len-hi 8))))
                ;; Sanity check
                (if (if (< pkt-len 4) 1 (> pkt-len 1536))
                    ;; Bad packet — reset BNRY and skip
                    (progn
                      (io-out-byte #x303 (if (eq next-page (ne2k-rx-start))
                                        (- (ne2k-rx-stop) 1)
                                        (- next-page 1)))
                      0)
                    ;; Read packet data (skip 4-byte NIC header)
                    (let ((data-len (- pkt-len 4))
                          (data-addr (+ hdr-addr 4))
                          (rx-buf (ne2k-rx-host)))
                      ;; Check if packet wraps around ring buffer
                      (let ((end-addr (+ data-addr data-len))
                            (ring-end (* (ne2k-rx-stop) 256))
                            (ring-start (* (ne2k-rx-start) 256)))
                        (if (<= end-addr ring-end)
                            ;; No wrap — read contiguously
                            (ne2k-dma-read data-addr data-len rx-buf)
                            ;; Wrap — read in two parts
                            (let ((first-len (- ring-end data-addr)))
                              (ne2k-dma-read data-addr first-len rx-buf)
                              (ne2k-dma-read ring-start (- data-len first-len)
                                            (+ rx-buf first-len)))))
                      ;; Update BNRY to next_page - 1
                      (io-out-byte #x303 (if (eq next-page (ne2k-rx-start))
                                        (- (ne2k-rx-stop) 1)
                                        (- next-page 1)))
                      data-len)))))))))

;; ================================================================
;; The NIC interface (net/nic.lisp): NE2K-SEND and NE2K-RECEIVE above, and
;; these two.
;; ================================================================

(defun ne2k-rx-buf () (ne2k-rx-host))
(defun ne2k-probe () (ne2k-reset) (ne2k-init))
