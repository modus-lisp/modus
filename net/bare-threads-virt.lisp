;;;; bare-threads-virt.lisp - net/bare-threads.lisp's platform seams for QEMU virt.
;;;;
;;;; Linked after net/bare-threads.lisp (MODUS_RPI_THREADS=1 on the :VIRT image), whose
;;;; Pi answers these replace by last-defun-wins.  Two facts differ from the Pi:
;;;;
;;;;   * The image runs at EL1, not EL2, so the core entry stub, the register snapshot
;;;;     and the TLB flush name the EL1 registers (test/rpi-smp/virt-stubs.s,
;;;;     virt-tlbi.s).
;;;;   * A core is started by PSCI CPU_ON over HVC (QEMU's own PSCI; no firmware spin
;;;;     table), entering at a PHYSICAL address with its MMU off.  The release routine
;;;;     cleans the stub to the point of coherency and makes the call; the target core
;;;;     and the entry address ride in the parameter block at +0x70 / +0x78.
;;;;
;;;; MEMORY.  boot/boot-aarch64.lisp's threaded map (*AARCH64-FIXPOINT-THREADS*) makes
;;;; every VA bare-threads uses ordinary DRAM: VA 0..0x101FFFFF at PA +0x40000000 as
;;;; always, and VA 0x10200000..0x1FFFFFFF at PA +0x50000000 (so it does not alias the
;;;; E1000's identity-mapped DMA at PA 0x50200000).  It needs QEMU -m 1024.

(defun %bt-stub-entry-words ()
  (list #x10004000 #xf9400001 #xd518a201 #xf9400401 #xd5182041 #xf9400801
        #xd5182001 #xd508871f #xd5033f9f #xd5033fdf #xf9401001 #xd518c001
        #xd2a00601 #xd5181041 #xd5033fdf #xf9400c01 #xd5181001 #xd5033fdf
        #xf9401801 #x9100003f #xf9401c01 #xd518d081 #xf9402001 #xd51bd041
        #xf940141c #xf9402418 #xf9402819 #xd2a20012 #xd280003a #xf2bbd5ba
        #xd2800021 #xf9003001 #xf9402c09 #xd280001d #xd280001e #xd63f0120
        #x10003b80 #xd2800041 #xf9003001 #xd503205f #x17ffffff))

(defun %bt-stub-release-words (s)
  (append (%bt-movz-movk-x0 s)
          (list #x91240002 #xd50b7e20 #x91010000 #xeb02001f #x54ffffa3 #xd5033f9f
                #xd1240000 #xf9443801 #xf9443c02 #xd2800060 #xf2b88000 #xd2800003
                #xd4000002 #xd37ff800 #xd65f03c0)))

(defun %bt-stub-snapshot-words (p)
  (append (%bt-movz-movk-x0 p)
          (list #xd538a201 #xf9000001 #xd5382041 #xf9000401 #xd5382001 #xf9000801
                #xd5381001 #xf9000c01 #xd538c001 #xf9001001 #xf900141c #xd5384241
                #xf9003401 #xd65f03c0)))

(defun %bt-stub-tlbi-words ()
  (list #xd5033a9f #xd508831f #xd5033b9f #xd5033fdf #xd65f03c0))   ; TLBI VMALLE1IS

(defun %bt-va->pa (a)
  (cond ((< a #x10200000) (+ a #x40000000))
        ((< a #x20000000) (+ a #x50000000))
        (t a)))

;; The L2 table at PA 0x40011000, written through the identity map: its low VA
;; (0x11000) is in the range the boot marks read-only over the image.
(defun %bt-l2-table () #x40011000)

(defun %bt-release (c s)
  "PSCI CPU_ON core C (its MPIDR on virt is C) at the stub S's physical address."
  (let ((p (+ s #x800)))
    (%gc-write64 (+ p #x70) c)
    (%gc-write64 (+ p #x78) (%bt-va->pa s))
    (%jit-call (+ s #x1000))))
