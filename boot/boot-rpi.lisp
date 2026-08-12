;;;; boot-rpi.lisp - Raspberry Pi (AArch64) Boot Sequence for Modus
;;;;
;;;; Raspberry Pi boot protocol (kernel8.img loaded by GPU firmware):
;;;;   1. GPU loads kernel8.img to 0x80000 (or 0x200000 with config.txt)
;;;;   2. CPU starts in EL2 (or EL1 depending on firmware)
;;;;   3. x0 = DTB pointer
;;;;   4. Only core 0 runs; cores 1-3 spin on mailbox
;;;;
;;;; Image layout (with interrupt-driven DWC2 USB gadget):
;;;;   0x00000  Boot code (SP, UART, alloc, TPIDR, VBAR, branch)
;;;;   0x00800  Exception vectors (2KB, 16 entries x 32 instructions)
;;;;   0x01000  ISR code (hand-assembled AArch64, ~500 bytes)
;;;;   0x01400  Native code (kernel-main)

(in-package :modus.mvm)

;;; ============================================================
;;; Raspberry Pi Boot Constants
;;; ============================================================

(defconstant +rpi-uart-base+      #x3F201000)
(defconstant +rpi-kernel-base+    #x00080000)
(defconstant +rpi-stack-top+      #x00200000)
(defconstant +rpi-cons-base+      #x04000000)
(defconstant +rpi-general-base+   #x05000000)
(defconstant +rpi-percpu-base+    #x02000000)

;;; ============================================================
;;; RPi Boot Code Generation
;;; ============================================================

(defun emit-rpi-entry (buf)
  "Emit RPi AArch64 boot code. Layout: boot(0x0) vectors(0x800) ISR(0x1000) native(0x1400)."
  (let ((sp 31) (x0 0) (x1 1) (x16 16) (x17 17)
        (x24 24) (x25 25) (x26 26))
    (declare (ignorable x1))
    ;; 1. Stack pointer
    (emit-aarch64-movz buf x16 #x0020 16)
    (emit-aarch64-mov-sp buf sp x16)
    ;; 2. PL011 UART init
    (emit-aarch64-movz buf x17 #x1000 0)
    (emit-aarch64-movk buf x17 #x3F20 16)
    (emit-aarch64-movz buf x0 0 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 12 10) (ash x17 5) x0))
    (emit-aarch64-movz buf x0 26 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 9 10) (ash x17 5) x0))
    (emit-aarch64-movz buf x0 3 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 10 10) (ash x17 5) x0))
    (emit-aarch64-movz buf x0 #x70 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 11 10) (ash x17 5) x0))
    (emit-aarch64-movz buf x0 #x0301 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 12 10) (ash x17 5) x0))
    ;; 3. Allocation registers
    (emit-aarch64-movz buf x24 #x0400 16)
    (emit-aarch64-movz buf x25 #x0500 16)
    (emit-aarch64-movz buf x26 0 0)
    ;; 4. TPIDR_EL1
    (emit-aarch64-movz buf x16 #x0200 16)
    (emit-aarch64-u32 buf #xD518D090)
    ;; 5. VBAR_EL1 = 0x00080800
    (emit-aarch64-movz buf x16 #x0008 16)
    (emit-aarch64-movk buf x16 #x0800 0)
    (emit-aarch64-u32 buf #xD518C010)
    (emit-aarch64-u32 buf #xD5033FDF)
    ;; 6. Branch to native code at 0x1400
    ;; BUF is an a64-buffer (see emit-aarch64-u32 in boot-aarch64.lisp) since
    ;; 8048454 unified boot + translator on one buffer, so POSITION already
    ;; counts 32-bit INSTRUCTIONS -- no /4.
    (let* ((cur (a64-buffer-position buf))
           (skip (- (/ #x1400 4) cur)))
      (emit-aarch64-u32 buf (logior (ash #b000101 26) (logand skip #x3FFFFFF)))
      (let ((pad (- 512 (a64-buffer-position buf))))
        (dotimes (i pad) (emit-aarch64-u32 buf #xD503201F))))
    ;; 7. Exception vectors at 0x800
    (emit-rpi-exception-vectors buf)
    ;; 8. ISR at 0x1000
    (emit-rpi-isr buf)))

;;; ============================================================
;;; RPi Exception Vectors (2KB at offset 0x800)
;;; ============================================================

(defun emit-rpi-exception-vectors (buf)
  "16 entries x 32 insns. Entry 5 (SP_ELx IRQ) branches to ISR at 0x1000."
  (dotimes (entry 16)
    (if (= entry 5)
        ;; Entry 5 at offset 0xA80, ISR at 0x1000 → delta = 0x580/4 = 352 insns
        (progn
          (emit-aarch64-u32 buf (logior (ash #b000101 26) (logand 352 #x3FFFFFF)))
          (dotimes (i 31) (emit-aarch64-u32 buf #xD503201F)))
        (progn
          (emit-aarch64-u32 buf #x14000000)  ; B . (infinite loop)
          (dotimes (i 31) (emit-aarch64-u32 buf #xD503201F))))))

;;; ============================================================
;;; DWC2 USB Gadget ISR (hand-assembled AArch64 at offset 0x1000)
;;; ============================================================
;;;
;;; Drains RX FIFO into a 4-slot ring buffer at 0x01090000.
;;; Sets deferred flags for USB reset, enum done, SETUP packets.
;;; Re-arms EP2 OUT on transfer completion.
;;; Does NOT touch x24/x25/x26 (alloc/limit/NIL).
;;; Does NOT allocate — pure hand-assembled, no MVM code.
;;;
;;; Ring buffer at 0x01090000:
;;;   +0x000  write_idx (u32)      +0x004  read_idx (u32)
;;;   +0x008  frame_len[4] (16B)   +0x018  frame_accum (u32)
;;;   +0x01C  deferred (u32)       +0x800  slot 0..3 data (2KB each)
;;;
;;; ISR register usage: x0-x7 saved/restored on stack.
;;;   x2 = DWC2 base (0x3F980000)
;;;   x3 = GINTSTS
;;;   x4 = ring buffer base (0x01090000)
;;;   x0,x1,x5,x6,x7 = temporaries

(defun build-rpi-isr-vector ()
  "Return vector of u32 AArch64 instructions for the DWC2 ISR.
   All branch offsets are pre-computed from the instruction indices below."
  ;;
  ;; Label indices (instruction numbers):
  ;;   rx_loop=12  check_more=34  check_pkt3=36  check_setup=52
  ;;   check_pkt4=64  discard=76  check_oep=82  clear_oep0=97
  ;;   check_iep=99  check_rst=104  check_enum=111  restore=118
  ;;
  (vector
   ;; === Save x0-x7 ===
   ;; [0]
   #xA9BC07E0   ; STP x0, x1, [SP, #-64]!
   #xA9010FE2   ; STP x2, x3, [SP, #16]
   #xA90217E4   ; STP x4, x5, [SP, #32]
   #xA9031FE6   ; STP x6, x7, [SP, #48]

   ;; === Check BCM IC: IRQ_pending_1 bit 9 (USB) ===
   ;; [4]
   #xD2964080   ; MOVZ x0, #0xB204
   #xF2A7E000   ; MOVK x0, #0x3F00, LSL #16
   #xB9400001   ; LDR w1, [x0]
   #x36480DE1   ; TBZ w1, #9, restore(118)  offset=111

   ;; === Load DWC2 base + GINTSTS ===
   ;; [8]
   #xD2A7F302   ; MOVZ x2, #0x3F98, LSL #16   (DWC2 base)
   #xB9401443   ; LDR w3, [x2, #0x14]          (GINTSTS)

   ;; === RxFLvl (bit 4) ===
   ;; [10]
   #x36200903   ; TBZ w3, #4, check_oep(82)  offset=72
   #xD2A02124   ; MOVZ x4, #0x0109, LSL #16   (ring base)

   ;; === rx_loop: [12] ===
   #xB9402045   ; LDR w5, [x2, #0x20]     (GRXSTSP pop)
   #x12000CA6   ; AND w6, w5, #0xF        (ep)
   #x530438A7   ; UBFX w7, w5, #4, #11    (bcnt)
   #x535050A0   ; UBFX w0, w5, #17, #4    (pktsts)

   ;; --- pktsts==2? ---
   ;; [16]
   #x7100081F   ; CMP w0, #2
   #x54000261   ; BNE check_pkt3(36)  offset=19
   #x7100085F   ; CMP w6, #2          (ep==2?)
   #x54000721   ; BNE discard(76)     offset=57

   ;; Bulk OUT: read FIFO → ring slot[write_idx] + frame_accum
   ;; [20]
   #xB9400081   ; LDR w1, [x4]        (write_idx)
   #x91200085   ; ADD x5, x4, #0x800
   #x8B012CA5   ; ADD x5, x5, x1, LSL #11  (slot = base+0x800+idx*0x800)
   #xB9401881   ; LDR w1, [x4, #0x18]      (frame_accum)
   #x8B0100A5   ; ADD x5, x5, x1           (dest = slot + accum)
   #x0B070021   ; ADD w1, w1, w7            (accum += bcnt)
   #xB9001881   ; STR w1, [x4, #0x18]
   #x11000CE0   ; ADD w0, w7, #3
   #x53027C00   ; LSR w0, w0, #2            (words = (bcnt+3)/4)

   ;; fifo_rd: [29]
   #x340000A0   ; CBZ w0, check_more(34)  offset=5
   #xB9500041   ; LDR w1, [x2, #0x1000]   (FIFO read)
   #xB80044A1   ; STR w1, [x5], #4        (post-index store)
   #x51000400   ; SUB w0, w0, #1
   #x17FFFFFC   ; B fifo_rd(29)            offset=-4

   ;; === check_more: [34] ===
   #xB9401443   ; LDR w3, [x2, #0x14]      (re-read GINTSTS)
   #x3727FD23   ; TBNZ w3, #4, rx_loop(12) offset=-23

   ;; === check_pkt3: [36] pktsts==3 (OUT transfer complete) ===
   #x71000C1F   ; CMP w0, #3
   #x540001E1   ; BNE check_setup(52)  offset=15
   #x7100085F   ; CMP w6, #2           (ep==2?)
   #x54FFFF61   ; BNE check_more(34)   offset=-5

   ;; Finalize frame: frame_len[write_idx] = frame_accum
   ;; [40]
   #xB9400080   ; LDR w0, [x4]         (write_idx)
   #xB9401881   ; LDR w1, [x4, #0x18]  (frame_accum)
   #x91002085   ; ADD x5, x4, #8       (frame_len array)
   #xB82078A1   ; STR w1, [x5, x0, LSL #2]  (frame_len[idx] = accum)
   #xB900189F   ; STR wzr, [x4, #0x18]       (reset accum)
   ;; Advance write_idx mod 4, check full
   ;; [45]
   #x11000400   ; ADD w0, w0, #1
   #x12000C00   ; AND w0, w0, #3
   #xB9400481   ; LDR w1, [x4, #4]     (read_idx)
   #x6B01001F   ; CMP w0, w1
   #x54FFFE20   ; BEQ check_more(34)   offset=-15  (full → discard frame)
   #xB9000080   ; STR w0, [x4]         (write_idx++)
   #x17FFFFEF   ; B check_more(34)     offset=-17

   ;; === check_setup: [52] pktsts==6 (SETUP data) ===
   #x7100181F   ; CMP w0, #6
   #x54000161   ; BNE check_pkt4(64)   offset=11
   ;; Read SETUP 8 bytes from FIFO to 0x01000080
   ;; [54]
   #xD2A02005   ; MOVZ x5, #0x0100, LSL #16
   #xF2801005   ; MOVK x5, #0x0080
   #xB9500041   ; LDR w1, [x2, #0x1000]  (FIFO word 0)
   #xB90000A1   ; STR w1, [x5]
   #xB9500041   ; LDR w1, [x2, #0x1000]  (FIFO word 1)
   #xB90004A1   ; STR w1, [x5, #4]
   ;; Set deferred bit 2 (setup)
   ;; [60]
   #xB9401C81   ; LDR w1, [x4, #0x1C]   (deferred)
   #x321E0021   ; ORR w1, w1, #4
   #xB9001C81   ; STR w1, [x4, #0x1C]
   #x17FFFFE3   ; B check_more(34)       offset=-29

   ;; === check_pkt4: [64] pktsts==4 (SETUP complete) ===
   #x7100101F   ; CMP w0, #4
   #x54000161   ; BNE discard(76)   offset=11
   ;; DOEPTSIZ[0] |= (3<<29) for STUPCNT
   ;; [66]
   #xB94B1041   ; LDR w1, [x2, #0xB10]
   #x32030421   ; ORR w1, w1, #0x60000000
   #xB90B1041   ; STR w1, [x2, #0xB10]
   ;; Clear GOUTNakEff: DCTL |= bit 10
   ;; [69]
   #xB9480441   ; LDR w1, [x2, #0x804]
   #x32160021   ; ORR w1, w1, #0x400
   #xB9080441   ; STR w1, [x2, #0x804]
   ;; Set deferred bit 2 (setup)
   ;; [72]
   #xB9401C81   ; LDR w1, [x4, #0x1C]
   #x321E0021   ; ORR w1, w1, #4
   #xB9001C81   ; STR w1, [x4, #0x1C]
   #x17FFFFD7   ; B check_more(34)   offset=-41

   ;; === discard: [76] read and discard FIFO words ===
   #x11000CE0   ; ADD w0, w7, #3
   #x53027C00   ; LSR w0, w0, #2     (words)
   ;; dis_loop: [78]
   #x34FFFA80   ; CBZ w0, check_more(34)  offset=-44
   #xB9500041   ; LDR w1, [x2, #0x1000]   (discard FIFO word)
   #x51000400   ; SUB w0, w0, #1
   #x17FFFFFD   ; B dis_loop(78)           offset=-3

   ;; === check_oep: [82] OEPInt (bit 19) ===
   #x36980223   ; TBZ w3, #19, check_iep(99)  offset=17
   ;; DOEPINT[2]
   ;; [83]
   #xB94B4840   ; LDR w0, [x2, #0xB48]
   #x360001A0   ; TBZ w0, #0, clear_oep0(97)  offset=13
   #xB90B4840   ; STR w0, [x2, #0xB48]   (clear DOEPINT[2])
   ;; Re-arm EP2: CGOUTNAK
   ;; [86]
   #xB9480440   ; LDR w0, [x2, #0x804]   (DCTL)
   #x32160000   ; ORR w0, w0, #0x400     (CGOUTNAK)
   #xB9080440   ; STR w0, [x2, #0x804]
   ;; DOEPTSIZ[2] = 2048 | (32<<19) = 0x01000800
   ;; [89]
   #x52810000   ; MOV w0, #0x0800
   #x72A02000   ; MOVK w0, #0x0100, LSL #16
   #xB90B5040   ; STR w0, [x2, #0xB50]
   ;; DOEPCTL[2] |= CNAK(26) | EPENA(31)
   ;; [92]
   #xB94B4040   ; LDR w0, [x2, #0xB40]
   #x32060000   ; ORR w0, w0, #0x04000000  (CNAK)
   #x32010000   ; ORR w0, w0, #0x80000000  (EPENA)
   #xB90B4040   ; STR w0, [x2, #0xB40]
   #x14000003   ; B check_iep(99)  offset=3

   ;; === clear_oep0: [97] clear DOEPINT[0] ===
   #xB94B0840   ; LDR w0, [x2, #0xB08]
   #xB90B0840   ; STR w0, [x2, #0xB08]

   ;; === check_iep: [99] IEPInt (bit 18) ===
   #x369000A3   ; TBZ w3, #18, check_rst(104)  offset=5
   ;; [100] Clear DIEPINT[0] and DIEPINT[1]
   #xB9490840   ; LDR w0, [x2, #0x908]
   #xB9090840   ; STR w0, [x2, #0x908]
   #xB9492840   ; LDR w0, [x2, #0x928]
   #xB9092840   ; STR w0, [x2, #0x928]

   ;; === check_rst: [104] USBRst (bit 12) ===
   #x366000E3   ; TBZ w3, #12, check_enum(111)  offset=7
   #xD2A02124   ; MOVZ x4, #0x0109, LSL #16
   #xB9401C80   ; LDR w0, [x4, #0x1C]
   #x32000000   ; ORR w0, w0, #1  (deferred bit 0 = reset)
   #xB9001C80   ; STR w0, [x4, #0x1C]
   #x52820000   ; MOV w0, #0x1000
   #xB9001440   ; STR w0, [x2, #0x14]  (clear GINTSTS bit 12)

   ;; === check_enum: [111] EnumDone (bit 13) ===
   #x366800E3   ; TBZ w3, #13, restore(118)  offset=7
   #xD2A02124   ; MOVZ x4, #0x0109, LSL #16
   #xB9401C80   ; LDR w0, [x4, #0x1C]
   #x321F0000   ; ORR w0, w0, #2  (deferred bit 1 = enum)
   #xB9001C80   ; STR w0, [x4, #0x1C]
   #x52840000   ; MOV w0, #0x2000
   #xB9001440   ; STR w0, [x2, #0x14]  (clear GINTSTS bit 13)

   ;; === restore: [118] ===
   #xA9431FE6   ; LDP x6, x7, [SP, #48]
   #xA94217E4   ; LDP x4, x5, [SP, #32]
   #xA9410FE2   ; LDP x2, x3, [SP, #16]
   #xA8C407E0   ; LDP x0, x1, [SP], #64
   #xD69F03E0   ; ERET
   ))

(defun emit-rpi-isr (buf)
  "Emit the DWC2 ISR at offset 0x1000 and pad to 0x1400."
  ;; a64-buffer POSITION counts instructions, so byte offset 0x1000 is index
  ;; #x400.  (See the note in emit-rpi-entry.)
  (assert (= (a64-buffer-position buf) (/ #x1000 4)))
  (let ((insns (build-rpi-isr-vector)))
    (dotimes (i (length insns))
      (emit-aarch64-u32 buf (aref insns i))))
  ;; Pad to offset 0x1400
  (let ((pad (- (/ #x1400 4) (a64-buffer-position buf))))
    (dotimes (i pad)
      (emit-aarch64-u32 buf #xD503201F))))

;;; ============================================================
;;; RPi Boot Integration
;;; ============================================================

(defun rpi-boot-descriptor ()
  "Return the Raspberry Pi boot descriptor for image building."
  (list :arch :aarch64
        :entry-fn #'emit-rpi-entry
        :load-addr +rpi-kernel-base+
        :stack-top +rpi-stack-top+
        :cons-base +rpi-cons-base+
        :general-base +rpi-general-base+
        :serial-base +rpi-uart-base+))
