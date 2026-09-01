;;;; arch-aarch64-cl.lisp — QEMU virt (AArch64) PCI/E1000 adapter for the
;;;; BARE-METAL CL/mvm image built by mvm/build-aarch64.lisp.
;;;;
;;;; This is `net/arch-aarch64.lisp' RETARGETED at the CL image, and it is a
;;;; separate file rather than an edit for exactly the two reasons
;;;; net/arch-rpi-cl.lisp is a separate file from net/arch-raspi3b.lisp:
;;;;
;;;;   1. RUNTIME OVERLAP.  arch-aarch64.lisp carries a whole miniature runtime
;;;;      — make-array / aref / aset / array-length / numberp / try-alloc-obj /
;;;;      tag-as-object, plus native-eval / eval-line-expr / rt-compile-defun /
;;;;      emit-prompt / the edit-* line-editor slots — because in the legacy
;;;;      `repl-source' images those ARE the runtime.  Here the full CL runtime
;;;;      is already in the image and the net stack is concatenated AFTER it, so
;;;;      under last-defun-wins those legacy definitions would REPLACE the real
;;;;      ones.  The legacy make-array builds an incompatible object layout
;;;;      (element-count << 15, byte-packed payload) while the compiler's
;;;;      %prim-aref / %prim-aset read count << 8 and 8-byte slots at raw+16 —
;;;;      so mvm-eval's own constant and bytecode buffers would be corrupted and
;;;;      even (+ 1 2) would fail.  None of them are defined here.  PRINT-DEC is
;;;;      dropped for the same reason: mvm/prelude.lisp already has a correct
;;;;      one, and the legacy version divides with `/'.
;;;;
;;;;   2. MEMORY MAP.  arch-aarch64.lisp puts every DMA / state / IPC region at
;;;;      0x41000000-0x4111xxxx.  This image loads at PA 0x40200000 and is
;;;;      ~57 MB, ending around PA 0x43900000 — but worse, the fixpoint MMU maps
;;;;      the Cheney heap's VA 0x09000000-0x10000000 onto PA 0x49000000-
;;;;      0x50000000, so a region anywhere in 0x41xxxxxx-0x4Fxxxxxx aliases
;;;;      either the kernel image or the GC heap.  That is not a hang, it is
;;;;      silent corruption: the 2026-07-11 relocation to 0x49000000 had the NIC
;;;;      DMA-ing over the globals hash-table, whose bucket spine went circular
;;;;      and wedged the reader the moment e1000-init ran its RX-descriptor loop.
;;;;      Every region below is therefore PA-IDENTITY at 0x502xxxxx: above the
;;;;      runtime metadata's backing store (VA 0x10000000-0x10200000 -> PA
;;;;      0x50000000-0x50200000, boot-aarch64.lisp's L2[128] override) and well
;;;;      inside QEMU virt's -m 512 DRAM, which ends at 0x60000000.
;;;;      mvm/build-cl-repl-common.lisp asserts both bounds at build time.
;;;;
;;;; WRITE-BYTE.  The net stack's `%serial-byte' is the legacy 1-arg "put a byte
;;;; on the console" primitive, called from e1000.lisp / ip.lisp /
;;;; http-client.lisp for their status output.  CL's WRITE-BYTE
;;;; (mvm/cl-fileio.lisp) is the 2-arg (byte stream) function.  In the flat MVM
;;;; namespace the later definition wins, so this file's 1-arg version SHADOWS
;;;; the CL one for the whole image.  That is deliberate, it is what
;;;; arch-rpi-cl.lisp and build-aarch64-ansi.lisp's net build both do, and the
;;;; only casualty is binary-stream output, which a bare-metal image has no file
;;;; descriptors for anyway.

;; PCI configuration space via ECAM MMIO.  QEMU virt (highmem) places ECAM at
;; PA 0x40_10000000, which boot-aarch64.lisp's fixpoint L1[256] maps as a 1 GB
;; device block.
(defun pci-config-read (bus dev fn reg)
  (let ((addr (+ #x4010000000
                 (logior (ash bus 20)
                         (logior (ash dev 15)
                                 (logior (ash fn 12)
                                         (logand reg #xFFC)))))))
    (mem-ref addr :u32)))

(defun pci-config-write (bus dev fn reg val)
  (let ((addr (+ #x4010000000
                 (logior (ash bus 20)
                         (logior (ash dev 15)
                                 (logior (ash fn 12)
                                         (logand reg #xFFC)))))))
    (setf (mem-ref addr :u32) val)))

;; I/O delay.  The legacy version conditionally does (timer-rearm)+(wfi) when a
;; GICv2 timer has been armed; this image programs no interrupt controller at
;; all (mvm/build-cl-repl-common.lisp leaves *aarch64-setup-irq-enable* NIL — a
;; REPL polls the UART), so WFI would never be woken.  Spin on a UART register
;; read, which is what the legacy path does with the timer off.
;;
;; VA 0x20000000, NOT 0x09000000: under the fixpoint page tables the PL011 is
;; reached at +TDK-UART-VA+ (L2[256] -> PA 0x09000000), while VA 0x09000000 is
;; the base of the Cheney heap.  The legacy file reads 0x09000000 here, which on
;; this image is a read of live heap DRAM — harmless but meaningless.
(defun io-delay ()
  (dotimes (d 5000) (mem-ref #x20000000 :u8)))

;; Console byte out.  No capture buffer / suppress flag in this image — the
;; serial port is the only console (kernel-main also points *error-output* at
;; it), so write straight to the UART.  The legacy version routes through a
;; suppress flag at a hardcoded 0x41100014, an address that is inside this
;; kernel image, so whatever garbage sat there could silence all NIC/IP output.
(defun %serial-byte (b)
  (write-char-serial b))

;; Entropy from UART read timing.  Not used by the plain-HTTP path (no crypto in
;; this image), kept so ip.lisp's callers resolve.
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (mem-ref #x20000000 :u8)
      (setq s (logxor (ash s 8) (logand i #xFF))))
    (when (zerop s) (setq s 42))
    s))

;; Hex printing (e1000.lisp prints the MAC and the BAR with these).
(defun print-hex-digit (n)
  (if (< n 10)
      (%serial-byte (+ n 48))
      (%serial-byte (+ n 55))))

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

;; PCI BAR assignment — there is no BIOS or firmware to do it on bare metal.
;;
;; BARs are handed out from VA 0x11000000, NOT the legacy 0x10000000.  QEMU
;; virt's 32-bit PCI MMIO window is 0x10000000-0x3EFF0000 and the fixpoint page
;; tables map VA 0x10200000-0x3FFFFFFF identity-device, so most of that window
;; works — but L2[128] carves VA 0x10000000-0x101FFFFF out and remaps it to DRAM
;; at PA 0x50000000 for the runtime's BSS-equivalent metadata.  A BAR placed
;; there would route every register access into DRAM and the NIC would never
;; respond.  Same override build-aarch64-ansi.lisp's net build applies.
(defun pci-assign-bars ()
  (let ((next-addr #x11000000))
    (dotimes (dev 32)
      (let ((id (pci-config-read 0 dev 0 0)))
        (when (not (eq id #xFFFFFFFF))
          (pci-config-write 0 dev 0 #x10 #xFFFFFFFF)
          (let ((bar-size-mask (pci-config-read 0 dev 0 #x10)))
            (when (not (zerop bar-size-mask))
              (let ((size (logand (+ (logxor (logand bar-size-mask #xFFFFFFF0) #xFFFFFFFF) 1) #xFFFFFFFF)))
                (let ((aligned (logand (+ next-addr (- size 1)) (logxor (- size 1) #xFFFFFFFF))))
                  (pci-config-write 0 dev 0 #x10 aligned)
                  (let ((cmd (pci-config-read 0 dev 0 4)))
                    (pci-config-write 0 dev 0 4 (logior cmd 7)))
                  (setq next-addr (+ aligned size)))))))))))

;; ============================================================
;; DMA / state regions — PA-identity, above the metadata backing store
;; ============================================================
;;
;;   0x50200000  RX descriptor ring
;;   0x50201000  RX frame buffers
;;   0x50241000  TX descriptor ring
;;   0x50241400  TX frame buffer
;;   0x50260000  network state (MAC +0x08, IP +0x18, gateway +0x1C, …)
;;   0x50260900  crypto scratch (unused here — no crypto in this image)
;;   0x50280000  TCP connection table (ip.lisp conn-base)
;;   0x50300000  IPC / scratch (ip.lisp locks and counters)
;;
;; Relative offsets are IDENTICAL to the legacy 0x41xxxxxx block, so the
;; ip.lisp / e1000.lisp state layout is untouched.  The whole span is
;; 0x50200000-0x50313000, i.e. 1.2 MB inside 0x50200000-0x60000000.
;; RX RING = 128 (e1000.lisp's portable default; nothing overrides it here).
;;
;; WITHDRAWN 2026-08-31 — a 256-descriptor ring does not fit this map, and the
;; attempt to use one silently corrupted the network state.  rx-buf is
;; 50201000, so 256*2048 = 0x80000 spans 50201000..50281000, which swallows
;; THREE live regions further down this very list:
;;
;;   50260000  e1000-state-base   -> descriptor 190
;;   50260900  fe-scratch-base    -> descriptor 190
;;   50280000  ssh-conn-base      -> descriptor 255
;;
;; So once a transfer used more than 190 descriptors the NIC DMA'd received
;; frames straight over the TCP/IP state block — rx cursor, our IP, connection
;; state, the expected sequence number — and then over the connection table.
;; The first attempt moved only the TX rings clear (50241000 -> 50281000) and
;; checked only those, which is why this looked safe: the arithmetic was done
;; for the two regions that came to mind, not for the whole map.  A 348 KB
;; fetch died about a second in, and the change sat in the tree long enough to
;; be blamed on an unrelated TCP fix that shipped alongside it.
;;
;; Growing the ring is still worth doing — the ring is the only buffering
;; between the wire and a guest that can pause, and a 119 ms GC is exactly such
;; a pause — but it requires RELOCATING state/scratch/conn-table first, above
;; the enlarged rx-buf and below ssh-ipc-base at 50300000.  Do the whole map,
;; not the two regions that come to mind.
;;
;; LAYOUT AT 128 (DMA window 50200000..50313000):
;;   rx-desc 50200000 + 128*16   = 0x800   -> ends 50200800
;;   rx-buf  50201000 + 128*2048 = 0x40000 -> ends 50241000
;;   tx-desc 50241000 + 64*16    = 0x400   -> ends 50241400
;;   tx-buf  50241400 + 64*1536  = 0x18000 -> ends 50259400  (< 50260000 state)
(defun e1000-rx-desc-base () #x50200000)
(defun e1000-rx-buf-base ()  #x50201000)
(defun e1000-tx-desc-base () #x50241000)
(defun e1000-tx-buf-base ()  #x50241400)
(defun e1000-state-base ()   #x50260000)
(defun fe-scratch-base ()    #x50260900)
(defun ssh-conn-base ()      #x50280000)
(defun ssh-ipc-base ()       #x50300000)

;; ============================================================
;; Single-threaded stubs
;; ============================================================
;; No interrupt controller and no actor scheduler in this image: the fetch
;; pipeline runs synchronously in kernel-main and polls.  These exist so the
;; shared net sources' references resolve rather than becoming NIL fn sentinels
;; — a live NIL fn in the table is the :li-func offset-0 garbage-execution
;; shape.
(defun spin-lock (addr) nil)
(defun spin-unlock (addr) nil)
;; `yield' is NOT a compiler primop (the MVM YIELD opcode is emitted implicitly
;; at the end of every LOOP iteration; there is no source-level form for it), so
;; ip.lisp's net-actor-main reference would otherwise resolve to a NIL function
;; sentinel.
(defun yield () (io-delay))
(defun receive () (e1000-receive) nil)
(defun actor-spawn (fn) 0)
(defun actor-exit () nil)
(defun nfn-lookup (hash) 0)
(defun hash-of (name) 0)
(defun init-gc-helper () nil)
