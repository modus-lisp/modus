;;;; arch-rpi-cl.lisp — Raspberry Pi 3B (BCM2837) adapter for the BARE-METAL
;;;; CL/mvm image (task #209 rung 2: networking on the Pi CL REPL).
;;;;
;;;; This is `net/arch-raspi3b.lisp' RETARGETED at the CL image.  It is a
;;;; separate file rather than an edit because the two adapters differ in two
;;;; ways that cannot be reconciled in one text:
;;;;
;;;;   1. MEMORY MAP.  arch-raspi3b.lisp puts every DMA / state / IPC region in
;;;;      0x01000000-0x01113000 and the actor regions in 0x02000000-0x06400000.
;;;;      The bare CL image is ~20 MB and loads at 0x80000, so it ENDS around
;;;;      0x1450000 — i.e. the legacy USB DMA base at 0x01000000 sits INSIDE
;;;;      the kernel image and the NIC would DMA over native code.  (Exactly the
;;;;      class build-aarch64.lisp's MODUS_NET_BUILD relocation exists to avoid:
;;;;      there the E1000 rings aliased the GC heap and wedged the reader.)
;;;;      Every region here is shifted by +0x1F000000 into the large free
;;;;      identity-mapped hole between the runtime metadata (ends 0x10200000)
;;;;      and the BCM2837 peripheral window (0x3F000000).  Relative offsets are
;;;;      preserved exactly, so the ip.lisp / cdc-ether.lisp state layout
;;;;      (MAC at +0x08, IP at +0x18, gateway at +0x1C, RX cursor at +0x10 …)
;;;;      is untouched.
;;;;
;;;;   2. RUNTIME OVERLAP.  arch-raspi3b.lisp carries a whole miniature runtime
;;;;      — make-array / aref / aset / array-length / numberp / try-alloc-obj /
;;;;      tag-as-object — because in the legacy `repl-source' images those ARE
;;;;      the runtime.  Here the full CL runtime is already in the image and
;;;;      the net stack is concatenated AFTER it, so under last-defun-wins
;;;;      those legacy definitions would REPLACE the real ones.  The legacy
;;;;      make-array builds an incompatible object layout (element-count << 15,
;;;;      byte-packed payload) while the compiler's %prim-aref/%prim-aset read
;;;;      count << 8 and 8-byte slots at raw+16 — so mvm-eval's own constant and
;;;;      bytecode buffers would be corrupted and even (+ 1 2) would fail.  None
;;;;      of them are defined here.  Likewise the repl-source-era line-editor,
;;;;      native-eval and emit-prompt stubs are dropped: they exist only to
;;;;      satisfy mvm/repl-source.lisp, which this image does not load.
;;;;
;;;; WRITE-BYTE.  The net stack's `write-byte' is the legacy 1-arg "put a byte
;;;; on the console" primitive, and it is called ~200 times across dwc2.lisp /
;;;; usb.lisp / cdc-ether.lisp / ip.lisp / http-client.lisp for their status
;;;; output.  CL's WRITE-BYTE (mvm/cl-fileio.lisp) is the 2-arg (byte stream)
;;;; function.  In the flat MVM namespace the later definition wins, so this
;;;; file's 1-arg version SHADOWS the CL one for the whole image.  That is
;;;; deliberate and is what build-aarch64.lisp's net build does too; the only
;;;; casualty is binary-stream output, which a bare-metal image has no
;;;; file descriptors for anyway.

;; No PCI bus on a BCM2837 — stubs for shared code that calls these.
(defun pci-config-read (bus dev fn reg) 0)
(defun pci-config-write (bus dev fn reg val) nil)
(defun pci-assign-bars () nil)

;; DWC2 USB host controller (BCM2837 peripheral window).
(defun dwc2-base () #x3F980000)

;; I/O delay.  The legacy version conditionally does (timer-rearm)+(wfi) when
;; enable-rpi-timer has armed the BCM2836 local timer; this image programs no
;; interrupt controller at all (see build-rpi-cl-repl.lisp: no GICv2 on a
;; BCM2837 and the vectors all spin), so WFI would never be woken.  Spin on a
;; PL011 register read, which is what the legacy path does with the timer off.
(defun io-delay ()
  (dotimes (d 5000) (mem-ref #x3F201000 :u8)))

;; Console byte out.  No capture buffer / suppress flag in this image — the
;; serial port is the only console (kernel-main also points *error-output* at
;; it), so write straight to the UART.
(defun write-byte (b)
  (write-char-serial b))

;; Entropy from UART read timing.  Not used by the plain-HTTP path (no crypto
;; in this image), kept so ip.lisp's callers resolve.
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (mem-ref #x3F201000 :u8)
      (setq s (logxor (ash s 8) (logand i #xFF))))
    (when (zerop s) (setq s 42))
    s))

;; Hex printing (cdc-ether prints the MAC with these).
(defun print-hex-digit (n)
  (if (< n 10)
      (write-byte (+ n 48))
      (write-byte (+ n 55))))

(defun print-hex-byte (b)
  (let ((hi (logand (ash b -4) 15))
        (lo (logand b 15)))
    (print-hex-digit hi)
    (print-hex-digit lo)))

(defun print-hex32 (n)
  (print-hex-byte (logand (ash n -24) 255))
  (print-hex-byte (logand (ash n -16) 255))
  (print-hex-byte (logand (ash n -8) 255))
  (print-hex-byte (logand n 255)))

;; ============================================================
;; DMA / state regions — legacy layout shifted by +0x1F000000
;; ============================================================
;;
;;   0x11000000  USB control-transfer DMA + E1000-shaped RX descriptor base
;;               (+0x000 setup buf, +0x040 data buf, +0x800 USB state)
;;   0x11001000  RX frame buffer (2 KB, the CDC bulk-IN landing zone)
;;   0x11041000  TX descriptor base (unused by CDC)
;;   0x11041400  TX frame buffer (1536 B, the CDC bulk-OUT staging area)
;;   0x11060000  network state (MAC +0x08, IP +0x18, gateway +0x1C, …)
;;   0x11080000  TCP connection table (ip.lisp conn-base)
;;   0x11090000  USB IRQ ring (unused — no interrupts in this image)
;;   0x11100000  IPC / scratch (ip.lisp locks and counters)
;;
;; Everything is plain identity-mapped DRAM: above the runtime metadata window
;; (0x10000000-0x10200000) and below the peripherals (0x3F000000).
;;
;; THIS BLOCK USED TO SIT AT 0x20000000 AND WAS UNUSABLE ON THE REAL TARGET.
;; The old comment justified it with "raspi3b has a fixed 1 GiB so the
;; addresses are real on QEMU and on hardware" — but the hardware the push gate
;; targets is a Pi Zero 2 W with **512 MB**, and gpu_mem=16 leaves the ARM only
;; 0x1F000000 (496 MB).  0x20000000 is therefore 16 MB PAST the end of physical
;; RAM: every USB descriptor and every DMA landing zone addressed a hole.  It
;; works flawlessly under QEMU raspi3b, which models a 1 GiB Pi 3B, so no
;; amount of emulated testing could ever have caught it.  Same shape as the
;; PL011-vs-mini-UART trap: correct on the dev platform, silently fatal on the
;; deployment platform.
;;
;; Shifted down by a constant 0x0F000000, so every relative offset in this
;; block — and every consumer that computes from these bases — is unchanged.
;; Keep the whole span below +RPI-CL-MIN-BOARD-RAM+; build-rpi-cl-repl.lisp
;; asserts it at build time so this cannot regress silently.
(defun usb-dma-base ()       #x11000000)
(defun e1000-rx-desc-base () #x11000000)
(defun e1000-rx-buf-base ()  #x11001000)
(defun e1000-tx-desc-base () #x11041000)
(defun e1000-tx-buf-base ()  #x11041400)
(defun e1000-state-base ()   #x11060000)
(defun ssh-conn-base ()      #x11080000)
(defun usb-ring-base ()      #x11090000)
(defun ssh-ipc-base ()       #x11100000)  ; shifted with the block (was 0x20100000)

;; ============================================================
;; Single-threaded stubs
;; ============================================================
;; No interrupt controller and no actor scheduler in this image: the fetch
;; pipeline runs synchronously in kernel-main and polls.  These exist so the
;; shared net sources' references resolve rather than becoming NIL fn sentinels.
(defun enable-rpi-timer () nil)
(defun dwc2-enable-host-irq () nil)
(defun spin-lock (addr) nil)
(defun spin-unlock (addr) nil)
;; `yield' is NOT a compiler primop (the MVM YIELD opcode is emitted implicitly
;; at the end of every LOOP iteration, there is no source-level form for it), so
;; ip.lisp's net-actor-main reference would otherwise resolve to a NIL function
;; sentinel.  Nothing here calls net-actor-main, but leaving a live NIL fn in
;; the table is the :li-func offset-0 garbage-execution shape — define it.
(defun yield () (io-delay))
(defun receive () (e1000-receive) nil)
(defun actor-spawn (fn) 0)
(defun actor-exit () nil)
(defun nfn-lookup (hash) 0)
(defun hash-of (name) 0)
(defun init-gc-helper () nil)
