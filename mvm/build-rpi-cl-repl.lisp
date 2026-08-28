;;;; build-rpi-cl-repl.lisp — the BARE-METAL RASPBERRY PI image that runs the
;;;; REAL CL (task #209), as a THIN TAIL over mvm/build-cli-common.lisp.
;;;;
;;;; A Raspberry Pi 3B / Zero 2 W (AArch64) kernel8.img that drops into Modus's
;;;; own self-hosted Common Lisp REPL over the serial port: the CL reader,
;;;; `eval' = mvm-eval (compile -> MVM bytecode -> mvm-interpret), and the CL
;;;; printer.  There is NO second Lisp here — `mvm/repl-source.lisp' (the
;;;; 708-line toy reader/printer that every legacy build-rpi-* and
;;;; build-pizero2w-* script bakes) is not part of this image.
;;;;
;;;; STRUCTURE — CONVERGED WITH THE HOSTED CLIs (task #266).  This file used to
;;;; be a 1825-line PRIVATE FORK of the shared assembly: 28 of its 38 defvars
;;;; were verbatim copies of ones build-cli-common.lisp already defines, and
;;;; among them was a complete copy of the **x64** JIT block (%init-x64-translator
;;;; + mvm/x64-asm.lisp + mvm/translate-x64.lisp) baked into an AARCH64 image,
;;;; dead only because its *jit-on* was NIL.  That is the exact shape of the
;;;; 2026-08-15 console (PL011 vs mini UART) and USB-DMA bugs: a fix lands in one
;;;; assembly and not the other.  Now the JIT block comes from build-cli-common's
;;;; ARCH DISPATCH, so enabling it (task #267) yields the aarch64 translator BY
;;;; CONSTRUCTION rather than by remembering to hand-copy one.
;;;;
;;;; What this file may legitimately contain is BARE-METAL / Pi HARDWARE FACT and
;;;; nothing else.  A capability belongs in build-cli-common.lisp, where every
;;;; image gets it at once.  The divergences that remain are named slots:
;;;;
;;;;   *CLI-BARE-METAL*             T — no Linux syscalls, fds, argv, cli-toplevel
;;;;   *CLI-BARE-METAL-TARBALL*     T — but DO bake lib/tar + lib/install-tarball,
;;;;                                    so the image can install a library it
;;;;                                    fetched itself (and so %IT-EVAL-SOURCE
;;;;                                    exists for the :GENERA / ASDF installers)
;;;;   *CLI-BARE-METAL-NET-SOURCE*  the Pi's own DWC2/USB/IP/HTTP stack
;;;;   *CLI-ARCH-SYSCALL-SOURCE*    `halt' (WFI), never `sys-exit' — see below
;;;;   *CLI-ARCH-PROBE-SOURCE*      lib/serial-repl.lisp + %rpi-gc-bitmap-init
;;;;   *CLI-ARCH-OVERRIDE-SOURCE*   lib/fdt.lisp + %cli-getenv over the
;;;;                                firmware device tree's /chosen/bootargs
;;;;                                (cmdline.txt on hardware, -append in QEMU)
;;;;   *CLI-ARCH-KERNEL-PROLOGUE*   banner, BSS-equivalent zeroing, %gc-init,
;;;;                                GC bitmaps — all before the FIRST allocation
;;;;   *CLI-ARCH-IO-SCRATCH-SOURCE* the globals whose defvar thunks must be
;;;;                                overridden AFTER (init-all-globals)
;;;;   *CLI-ARCH-KERNEL-EPILOGUE*   E2SMOKE, the net pipeline, the serial REPL
;;;;
;;;; and the build tail below: boot/boot-rpi-cl.lisp, the console selection, the
;;;; GC knobs and (build-image :target :rpi ...).
;;;;
;;;; MEMORY MAP.  Byte-for-byte the same VAs as the QEMU-virt bare image, but on
;;;; the Pi they are plain identity-mapped DRAM rather than an MMU remap, because
;;;; Pi DRAM starts at 0 (QEMU virt's starts at 0x40000000).  Image @0x80000,
;;;; stack top 0x08000000 growing down, Cheney heap [0x09000000, 0x10000000) with
;;;; the semispace midpoint at 0x0C800000, runtime metadata ("BSS") at 0x1000xxxx,
;;;; peripherals at 0x3F000000+.  Everything must stay below 0x3F000000.
;;;;
;;;; Usage: sbcl --dynamic-space-size 12288 --script mvm/build-rpi-cl-repl.lisp
;;;; Run:   qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \
;;;;          -serial stdio -serial null -display none
;;;; Output path override: MODUS_CL_REPL_OUT.

;;; MVM infrastructure, loaded HERE rather than only by build-cli-common: the
;;; Pi's USB/net stack is READ into the *CLI-BARE-METAL-NET-SOURCE* slot below,
;;; and a slot must be bound BEFORE build-cli-common is loaded.  That file's own
;;; load is guarded on (find-package "MODUS.MVM"), so the system loads once.
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

(format t "~%=== Building bare-metal RPi 3B CL REPL image ===~%")

;;; Source readers.  Deliberately %RPI--prefixed rather than reusing
;;; build-cli-common's READ-FILE-TEXT / MVM-TEXT: those do not exist yet (this
;;; file runs first) and defining them under the same names would have them
;;; redefined out from under us when that file loads.
(defun %rpi-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

;; #211: wrap the file so its own (in-package …) cannot leak into the next file
;; of the concatenated build blob.  Same contract as build-cli-common's MVM-TEXT.
(defun %rpi-mvm-text (relative-path)
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (modus.mvm::check-parses path)
    (modus.mvm::%build-package-scoped-source (%rpi-file-text path))))

;; net/ sources are read RAW (no package wrapper) — they are flat-namespace
;; bare-metal drivers and none of them declares a package.
(defun %rpi-net-text (rel)
  (let ((p (merge-pathnames rel (merge-pathnames "net/" *modus-base*))))
    (modus.mvm::check-parses p)
    (%rpi-file-text p)))

;;; ============================================================
;;; ARCH SLOTS — bare-metal AArch64 (BCM2837 / BCM2710A1)
;;; ============================================================

(defvar *cli-arch* :aarch64)

;;; BARE-METAL SEAM.  No OS: no Linux syscalls, no fds, no argv, no
;;; cli-toplevel.  The tarball pipeline is kept (untar -> parse .asd ->
;;; topo-sort -> eval), because this image fetches a library over its OWN
;;; USB/CDC stack and installs it in RAM — and because %IT-EVAL-SOURCE, which
;;; that file defines, is what the shared kernel-main's :GENERA and ASDF
;;; installers evaluate their surfaces with.
(defvar *cli-bare-metal* t)
(defvar *cli-bare-metal-tarball* t)

;;; Runtime JIT: ON (flipped 2026-08-24).  The mmap obstacle is gone:
;;; translate-aarch64's trap #x0531 now has a BARE-METAL branch that
;;; bump-allocates exec pages from the reserved region [0x14000000,
;;; 0x18000000) (Normal-WB, no PXN — already executable; bump word at
;;; 0x13FFFFF0 with a range-check init because real DRAM is not zeroed),
;;; and #x0534 (munmap) is a bare-metal no-op.  Traps #x0532 (BLR) and
;;; #x0533 (DC CVAU / IC IVAU / ISB) were always pure-CPU and work as-is.
;;; This image inherits build-cli-common's ARCH-DISPATCHED aarch64
;;; translator block; the Linux co-init is overridden below
;;; (*rpi-jit-coinit-override*) so runtime-emitted traps use the
;;; bare-metal paths (mini-UART serial, bump-allocator pages).
(defvar *jit-on* t)

;;; ------------------------------------------------------------
;;; BRING-UP CULLS (#209 rung 4).  Chain-loading a 20MB image over a 9600-baud
;;; mini-UART takes ~5.6 hours, so the bring-up image can drop payload the bare
;;; Pi does not need YET.  Both default OFF.
;;;
;;;   MODUS_RPI_NO_BLOB=1    drop the embedded self-source blob (~3.7MB).
;;;                          `read-embedded-source' is the only consumer and it
;;;                          is commented out at its single call site; the "MVMS"
;;;                          header is still emitted so the image footer's offset
;;;                          arithmetic stays valid.
;;;   MODUS_RPI_NO_BRIDGE=1  drop mvm/ansi-bridge.lisp (~1.7MB of source ->
;;;                          ~3.5MB of image).  NOT proven boot-safe — that file
;;;                          is load-bearing (list / make-array / rplaca / the
;;;                          CLOS defaults / %init-clos-protocol).  Measure.
(defun %rpi-cull-p (var)
  (let ((v (sb-ext:posix-getenv var)))
    (and v (plusp (length v)) (not (string= v "0")))))
(defvar *rpi-no-blob* (%rpi-cull-p "MODUS_RPI_NO_BLOB"))
(defvar *cli-omit-ansi-bridge* (%rpi-cull-p "MODUS_RPI_NO_BRIDGE"))
(when *rpi-no-blob*
  (setf modus.mvm::*embed-source-blob* nil)
  (format t "~&;; CULL: embedded source blob DROPPED~%"))
(when *cli-omit-ansi-bridge*
  (format t "~&;; CULL: mvm/ansi-bridge.lisp DROPPED~%"))

;;; ARCH SLOT: `halt', and deliberately NOT `sys-exit'.
;;;
;;; SYS-EXIT is a COMPILER SPECIAL FORM, not a function call: compiler.lisp's
;;; dispatch sends it to compile-sys-exit, filed under "--- Linux Syscalls ---",
;;; which unconditionally emits TRAP #x0500 -> a literal SVC.  Bare metal has no
;;; OS to service that, so it raises a synchronous exception.  Measured:
;;;   !!FAULT E0000000056000500 L000000000325cd4c      (ESR EC=0x15 = SVC)
;;; and the emitted sequence  mov x0,x26 / mov x0,#0 / svc #0x500  was read back
;;; out of RAM through the QEMU gdbstub and found byte-identical in the image
;;; FILE (file offset = vaddr - #x80000) — i.e. BAKED IN, not corruption.
;;;
;;; The (defun sys-exit (code) ... (halt)) below does NOT protect us: the
;;; compiler intrinsifies the NAME, so that defun is dead code.  A build CANNOT
;;; override a compiler special form by defining a function of the same name.
;;; See task #269 — the other bare-metal builds have the same live bug, and
;;; build-aarch64/build-x64 are ANSI gate runners where sys-exit is how the run
;;; TERMINATES, so do NOT blanket-replace them.
;;;
;;; KEEP THIS COMMENT OUTSIDE THE SOURCE STRING.  Text inside the strings in this
;;; file is COMPILED INTO THE IMAGE; adding a comment there changed the emitted
;;; image from offset #x89 onward (GENERIC-MULTIPLY's code included), which
;;; destroys any ability to attribute a behaviour change to a one-form edit.
(defvar *cli-arch-syscall-source* "
;; BARE METAL: no process model.  WFI in a loop (TRAP #x0304 = WFI on
;; AArch64) wakes on any IRQ and immediately WFIs again — effectively idle.
;; The x64 sibling uses (loop (hlt)); this is the same shape on ARM.
(defun halt ()
  (loop (trap #x0304)))
(defun sys-exit (code)
  (let ((c code)) c)
  (halt))
")

;;; ARCH SLOT: baked ahead of kernel-main.  The hosted CLIs put their JIT/argv
;;; probe apparatus here; a bare board puts its console and its collector's
;;; bitmap reservation, both of which kernel-main calls.
(defvar *cli-arch-probe-source*
  (concatenate 'string
    (string #\Newline)
    ;; The serial REPL itself: CL reader + EVAL(=mvm-eval) + CL printer over the
    ;; UART.  This is the bare-metal counterpart of lib/cli-toplevel.lisp, which
    ;; the hosted payload supplies and *CLI-BARE-METAL* omits.
    (%rpi-mvm-text "lib/serial-repl.lisp")
    "
;; #160 bitmaps, bare-metal flavour — see the call in kernel-main for why.
;; gc.lisp's %gc-bitmap-init uses %mmap-exec-page, which does not exist here.
;;
;; Both bases are published with the SAME convention gc.lisp's readers expect:
;; (setf (mem-ref .. :u64)) stores value<<1 and %gc-bitmap-base et al read it
;; back through the matching halving, exactly as the hosted path does — do NOT
;; \"helpfully\" pre-shift these.  page_base is from_start (the lowest object
;; address), matching %gc-bitmap-init.
;;
;; NON-ALLOCATING by construction: raw mem-ref stores and fixnum arithmetic
;; only.  It runs before init-symbol-table, so there is no heap to allocate
;; from yet, and a bignum here would fault rather than collect.
(defun %rpi-gc-bitmap-init (obj-base cons-base nbytes)
  (let ((i 0))
    (loop
      (when (>= i nbytes) (return nil))
      (setf (mem-ref (+ obj-base i) :u64) 0)
      (setf (mem-ref (+ cons-base i) :u64) 0)
      (setq i (+ i 8))))
  (setf (mem-ref #x10000E00 :u64) (%gc-from-start))
  (setf (mem-ref #x10000E18 :u64) obj-base)
  (setf (mem-ref #x10000E40 :u64) cons-base)
  nil)

"))

;;; ARCH SLOT: late last-defun-wins overrides, spliced right after the bridge.
;;;
;;; %CLI-GETENV — REAL, not a stub (task #271).
;;;
;;; There is no process environment on bare metal, and the hosted definition
;;; lives in lib/cli-toplevel.lisp — part of the payload *CLI-BARE-METAL* omits.
;;; But the SHARED kernel-main's :GENERA and ASDF installers consult
;;; MODUS_NO_GENERA / MODUS_NO_ASDF through it, so until now this slot held
;;; `(defun %cli-getenv (name) nil)': every knob was permanently "unset" and
;;; ~55 KB of Genera + ASDF source went through the in-image compiler on EVERY
;;; boot with no way to decline it.
;;;
;;; A board is not an environment-less machine; it is a machine whose
;;; environment comes from FIRMWARE.  boot/boot-rpi-cl.lisp now saves the
;;; device-tree pointer the AArch64 boot protocol passes in X0, and lib/fdt.lisp
;;; reads /chosen/bootargs out of that tree and splits it into KEY=VALUE pairs
;;; — which on a real Pi is the contents of cmdline.txt and under QEMU is
;;; `-append'.  So MODUS_NO_ASDF=1 in cmdline.txt now does on hardware exactly
;;; what MODUS_NO_ASDF=1 in the environment does on a hosted image, with no
;;; rebuild.
;;;
;;; The parse is LAZY — first call, then cached (see lib/fdt.lisp's header for
;;; why boot time would be the wrong place, and for the full guard list).  This
;;; slot therefore contributes exactly one line of its own.
(defvar *cli-arch-override-source*
  (concatenate 'string
    (string #\Newline)
    (%rpi-mvm-text "lib/fdt.lisp")
    "
(defun %cli-getenv (name) (%bootargs-lookup (%fdt-bootargs) name))
"))

;;; ARCH SLOT: hardware setup that must precede the FIRST allocation.
(defvar *cli-arch-kernel-prologue* "
  ;; Banner first: proves native code is executing and the UART is alive
  ;; before any runtime init runs.
  (write-string-serial \"MODUS-CL\")
  (write-char-serial 10)

  ;; NO BSS-EQUIVALENT INIT HERE.  It used to be a ~22-entry list of individual
  ;; (setf (mem-ref #x1000xxxx :u64) 0) slots, and it was wrong twice over: it
  ;; ran AFTER the banner above — so write-string-serial's own global reads
  ;; already walked the garbage alist head at 0x10000080 — and an enumeration
  ;; can only cover the slots someone remembered, leaving any newly added
  ;; metadata word uninitialised on hardware and fine under emulation.
  ;; boot/boot-rpi-cl.lisp step 0 now bulk-zeroes 0x10000000..0x10001000 plus
  ;; 0x10010000 before a single Lisp instruction runs, which is also the only
  ;; place the bulk form is legal: the two words that must survive
  ;; (0x10000F00 DTB pointer, 0x10000160/168 code bounds) are written by that
  ;; same preamble afterwards.

  ;; GC METADATA — must precede the first allocation.  The x64 bare image gets
  ;; this from boot-x64.lisp's kernel64 entry; the AArch64 boot publishes only
  ;; x24/x25, so kernel-main has to publish the semispace metadata itself
  ;; (same call the QEMU-virt bare image makes).  Heap is 112 MB split into two
  ;; 56-MB semispaces: from-start 0x09000000, to-start 0x0C800000.
  ;; boot-rpi-cl.lisp sets x25 = 0x0C800000 so the first overflow trips the GC
  ;; trampoline rather than running off the end of the from-space.  The third
  ;; argument is the conservative stack scan base — keep it equal to the boot
  ;; SP (+rpi-cl-stack-top+).
  (%gc-init #x09000000 #x07000000 #x08000000)

  ;; #160 OBJECT-START + CONS-KIND BITMAPS, bare-metal flavour.
  ;;
  ;; %gc-bitmap-init (gc.lisp) reserves these with %mmap-exec-page, which does
  ;; not exist here, so bare metal had NO bitmap: %gc-is-start degraded to T and
  ;; every conservative candidate was copied.  That was survivable only while
  ;; the collector forwarded almost nothing (the %gc-read64 word/2 bug, fixed in
  ;; b65730c).  With forwarding actually working, false roots get copied using
  ;; DATA as a header — measured twice on this image: a copy loop walking off
  ;; DRAM (ESR #x97000010), and a false root near the top of the upper semispace
  ;; copying past 0x10000000 onto the GC metadata at 0x10000040, i.e. the
  ;; collector overwriting its own saved_rsp (tell: phase trace 12345 -> 1S2345).
  ;;
  ;; Fixed RAM instead of mmap.  1 bit / 16-byte granule over the 112 MB heap =
  ;; 0x07000000/128 = 0xE0000 bytes (896 KB) per bitmap.  Placed at 80 MB
  ;; (0x05000000 / 0x05100000), which clears the image and sits well below the
  ;; stack top; both bounds are asserted at build time.  MUST run before the
  ;; first allocation (init-symbol-table, just below) so every mutator alloc
  ;; records its start bit, and after %gc-init because page_base is read from
  ;; from_start.
  ;;
  ;; DRAM is not guaranteed zero at reset, so the regions are cleared explicitly
  ;; — a stale bit would validate a false root, which is the whole failure this
  ;; exists to prevent.  Non-allocating: raw mem-ref stores only.
  (%rpi-gc-bitmap-init #x05000000 #x05100000 #xE0000)

  ;; NOTE: no (setup-irq) / (nic-irq-unmask) here.  Those program a GICv2,
  ;; which a BCM2837 does not have (it uses the BCM interrupt controller), and
  ;; nothing in this image needs interrupts — the REPL polls the UART.

")

;;; ARCH SLOT: spliced immediately AFTER (init-all-globals) and before the shared
;;; ANSI-constant block.  Its documented purpose is the *cstr-scratch* /
;;; *io-buf-addr* pair, and those are the reason the slot exists at all — on this
;;; board cl-fileio.lisp's defvar thunks restore #x1DF00000 / #x1DE00000, which
;;; are outside the declared heap.  The rest of the block is the same class:
;;; globals whose init thunks either do not run (limitation #7) or run with a
;;; hosted value, plus %init-clos-protocol, which every other build's kernel-main
;;; calls and which must land after the thunks that would otherwise reset it.
;;;
;;; It also carries the #271 DTB REPORT — one line naming the device-tree
;;; pointer the firmware passed, the /chosen/bootargs string read out of it, and
;;; the value of MODUS_PROBE.  It lives HERE, and not in the epilogue, for one
;;; reason: this is the earliest point in kernel-main at which allocation is
;;; legal (heap up, globals initialised) and it is still BEFORE the :GENERA and
;;; ASDF installers, which are the first real consumers of %CLI-GETENV.  So the
;;; boot log states what the machine was told before anything acts on it — and
;;; it is the call that populates lib/fdt.lisp's cache, which means the walk
;;; happens at a point where a fault would be attributable, rather than inside
;;; an installer's handler-case.  Wrapped, because a diagnostic must never be
;;; the thing that stops a boot.
(defvar *cli-arch-io-scratch-source* "  (setq *cstr-scratch* #x0FE00000)
  (setq *io-buf-addr*  #x0FF00000)
  (setq *scratch-mmapped* nil)
  (setq *filesystem* nil)
  (setq *default-pathname-defaults* \"/\")
  (setq *gensym-counter* 0)
  (setq *gentemp-counter* 0)
  (setq internal-time-units-per-second 1000000)
  (setq most-positive-fixnum  4611686018427387903)
  (setq most-negative-fixnum -4611686018427387904)
  (%init-standard-chars)
  (%init-boole-constants)
  (%init-clos-protocol)
  (handler-case
      (progn
        (write-string-serial \"DTB ptr=\")
        (print-dec (%fdt-base))
        (write-string-serial \" bootargs=[\")
        (let ((a (%fdt-bootargs)))
          (if (null a) (write-string-serial \"<none>\") (write-string-serial a)))
        (write-string-serial \"] MODUS_PROBE=[\")
        (let ((v (%cli-getenv \"MODUS_PROBE\")))
          (if (null v) (write-string-serial \"<nil>\") (write-string-serial v)))
        (write-string-serial \"]\")
        (write-char-serial 10))
    (t (c) nil))
  (setq *serial-repl-buf* nil)
  (setq *serial-repl-len* 0)
  (setq *serial-repl-cap* 0)
")

;;; ============================================================
;;; NET BUILD (MODUS_NET_BUILD=1) — #209 rung 2: HTTP over DWC2/USB
;;; ============================================================
;;;
;;; When enabled, append the Pi's USB net stack (arch adapter + DWC2 host
;;; controller + USB core + CDC Ethernet + IP/TCP/ARP/DHCP + HTTP client) to
;;; the image and drive a DHCP -> TCP -> HTTP GET pipeline from kernel-main.
;;; This is the DWC2 counterpart of build-aarch64.lisp's MODUS_NET_BUILD, which
;;; does the same thing over E1000 on QEMU virt; the driver text below is
;;; deliberately shaped like that one so the two stay diffable.
;;;
;;; EVERY var here is "" when the flag is off, and the kernel-main call site is
;;; a spliced "" too, so the default (rung 1) image is unaffected.
;;;
;;; No SSH and no crypto: a plain-HTTP fetch needs only the NIC, IP and the
;;; HTTP client.  No actors either — the fetch runs synchronously in
;;; kernel-main, so there is no yield/context-switch to corrupt cons cells
;;; (MVM active limitation #5).
;;;
;;; WHY THIS WORKS WHERE build-rpi-ssh DOES NOT.  The legacy repl-source RPi
;;; SSH image wedges under QEMU 7.2 at the FIRST USB control transfer
;;; (`D1E' = GET_DEVICE_DESCRIPTOR failed), and a `-trace usb_dwc2*' capture
;;; shows why: HCCHAR is written twice as 0x00000040, i.e. WITHOUT the CHENA
;;; enable bit, so QEMU is never asked to run a packet (zero usb_dwc2_packet_*
;;; events in 265k trace lines).  `(hcchar-chena)' is `(ash 1 31)', and
;;; compile-ash only INLINES a constant shift count <= 30 — 31 routes to the
;;; runtime function `bignum-ash', which does not exist in a repl-source image,
;;; so the constant evaluates to 0 and the OR is a no-op.  (`(ash 1 29)' for
;;; GUSBCFG force-host is <= 30 and does reach the register, which is why the
;;; controller inits and the port reports FS before it stalls.)  In THIS image
;;; bignum-ash is present — it comes in with mvm/cl-eval.lisp as part of
;;; *bridge-source*, which is concatenated BEFORE the net stack — so
;;; (ash 1 31) yields 2147483648, CHENA is set, and enumeration proceeds.
;;; The DWC2 driver was never broken; it was the runtime under it.
(defvar *net-build-p*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_NET_BUILD")))
    (and v (string= v "1"))))

;; MODUS_SSH_BUILD=1 — fold the crypto layer (SHA-256/512, ChaCha20, Poly1305,
;; X25519, Ed25519) into the CL host-net image, staging toward a bare-metal SSH
;; server.  Additive: "" when off, so the HTTP image is byte-identical.  Crypto
;; is separable from the SSH transport/actor stack — it depends only on pure
;; arithmetic + a scratch region at (e1000-state-base)+0x100, both of which the
;; CL host adapter (arch-rpi-cl.lisp) already provides, and which does NOT
;; overlap r8152.lisp's NIC state (+0x08..+0x44).  Building it VERIFIES the
;; crypto sources MVM-compile for AArch64 under the CL/mvm-eval image (the heavy
;; 32-bit rotations are exactly where a compile-ash/bignum gap would surface).
;; The SSH transport (actors + ssh.lisp + a fresh actor/SSH address map for the
;; 0x11000000 layout) is the next layer and needs on-hardware iteration.
(defvar *ssh-build-p*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_SSH_BUILD")))
    (and v (string= v "1"))))

;; Crypto source, spliced into *net-source* after ip.lisp.  "" unless SSH build.
(defvar *crypto-source*
  (if *ssh-build-p*
      (concatenate 'string
        (%rpi-net-text "crypto.lisp")        (string #\Newline)
        (%rpi-net-text "crypto-fast.lisp")   (string #\Newline))
      ""))

;; Actor/SSH address map for the CL host image's memory layout.  The legacy
;; block (arch-raspi3b) sits at 0x0200_0000/0x0600_0000, which lands INSIDE this
;; 58 MB image and would corrupt code; shift +0x10000000 (the same shift
;; arch-rpi-cl applied to the net block → 0x1100_0000).  All in Normal-WB RAM
;; per boot-rpi-cl's page table (0x11200000-0x3EFFFFFF is Normal-WB; the USB DMA
;; window 0x11000000-0x111FFFFF is Device — actor code must NOT run there).
;; arch-rpi-cl does NOT define these, so there is no last-defun-wins conflict.
(defvar *ssh-addr-map-source*
  (if *ssh-build-p*
      "
(defun percpu-data-base ()   #x12000000)
(defun sched-lock-addr ()    #x12000200)
(defun actor-table-base ()   #x12010000)
(defun sched-state-base ()   #x12012000)
(defun scratch-addr ()       #x12012050)
(defun decode-ptr-addr ()    #x12012058)
(defun actor-stack-base ()   #x12020000)
(defun mailbox-pool-base ()  #x12420000)
(defun mailbox-pool-limit () #x12440000)
(defun pool-state-base ()    #x12440000)
(defun staging-base-addr ()  #x12500000)
(defun actor-heap-base ()    #x16000000)
;; SSH CPU-side scratch MUST be Normal-WB, not the Device USB-DMA window: the
;; SSH stack does UNALIGNED u32 stores into e1000-state (ssh-init-strings @
;; +0x1000, crypto @ +0x680) and ssh-ipc, which Device-nGnRnE memory faults
;; (ESR alignment). arch-rpi-cl puts these at 0x1106/8/11_0000 (inside the 2 MB
;; Device block). Relocate to Normal-WB 0x1300_0000; the DMA buffers
;; (usb-dma/e1000-rx/tx-buf/desc, cdc-rx = e1000-rx-buf-base) STAY in the Device
;; window — they are separate addresses and not referenced off e1000-state.
(defun e1000-state-base ()   #x13000000)
(defun ssh-conn-base ()      #x13010000)
(defun ssh-ipc-base ()       #x13100000)
"
      ""))

;; SSH transport: address map + actor scheduler + net-actor + SSH server.
;; actors-net-overrides.lisp comes AFTER ip.lisp (which also defines
;; net-actor-main) so its actor-aware version wins under last-defun-wins.
;; NOTE (staged, not yet functional): actor-spawn/nfn-lookup are native-fn-addr
;; stubs in the arch adapters; running net-actor-main as a CL/mvm-eval function
;; needs them rewired to the CL fn-table — live-REPL work once the NIC is up.
;; aarch64-overrides.lisp is deliberately OMITTED (its reader conflicts with the
;; CL reader); the SSH channel→eval wiring is part of that same live work.
;; SINGLE-THREADED SSH (no actor scheduler): aarch64-overrides.lisp provides the
;; inline net-accept-connection -> ssh-connection-handler -> ssh-handle-connection
;; path (sidesteps the actor context-switch), the capture-aware write-byte SSH
;; output routing needs, and the crypto helpers (pre-compute-server-eph /
;; -host-sign, ed25519-sign-fast, ssh-random, usb-keepalive).  It loads AFTER
;; ssh.lisp so its single-threaded defuns win under last-defun-wins.  actors.lisp
;; is kept only so ssh.lisp's actor-spawn reference resolves; net-actor-main (the
;; poll loop) comes from ip.lisp and yields as a no-op when the actor system is
;; uninitialised, so calling it directly IS the single-threaded server.
;; Its native-eval = (eval-sexp ...) is the DELETED tree-walker; override it with
;; the CL image's production eval so the SSH shell evaluates via mvm-eval.
(defvar *ssh-transport-source*
  (if *ssh-build-p*
      (concatenate 'string
        *ssh-addr-map-source*                     (string #\Newline)
        (%rpi-net-text "actors.lisp")             (string #\Newline)
        (%rpi-net-text "ssh.lisp")                (string #\Newline)
        (%rpi-net-text "aarch64-overrides.lisp")  (string #\Newline)
        "(defun native-eval (form) (eval form))"  (string #\Newline)
        ;; ssh-handle-connection fix + trace live in net/aarch64-overrides.lisp.
        ;; FIX: single-threaded server handles ONE connection at a time.  Guard
        ;; against RE-ENTRANT net-accept-connection: while inside a connection
        ;; (flag ssh-ipc+0x60450 = 1), a reconnect SYN must NOT spawn a nested
        ;; accept — that crosses the per-conn receive buffers (the client's KEXINIT
        ;; ends up unread while a nested handler reads a fresh version).  Data
        ;; segments (non-SYN) still deliver to the active connection.
        "(defun net-handle-tcp (buf pkt-len)
  (let ((src-ip (buf-read-u32-mem buf 26))
        (src-port (buf-read-u16-mem buf 34))
        (dst-port (buf-read-u16-mem buf 36))
        (tcp-flags (mem-ref (+ buf 47) :u8)))
    (if (eq (logand tcp-flags #x12) #x02)
        (when (zerop (mem-ref (+ (ssh-ipc-base) #x60450) :u32))
          (when (eq dst-port (mem-ref (+ (ssh-ipc-base) #x60438) :u32))
            (setf (mem-ref (+ (ssh-ipc-base) #x60450) :u32) 1)
            (net-accept-connection src-ip src-port dst-port buf)
            (setf (mem-ref (+ (ssh-ipc-base) #x60450) :u32) 0)))
        (let ((conn (net-find-connection src-ip src-port dst-port)))
          (when (not (= conn (- 0 1)))
            (net-deliver-data conn buf pkt-len tcp-flags))))))"
        (string #\Newline)
        ;; CL-native exec path: the shared ssh-do-eval-expr (aarch64-overrides)
        ;; calls eval-sexp (the DELETED tree-walker) + buf-read-list (the legacy
        ;; repl-source reader) — neither exists in this image, so exec produced
        ;; no output.  Route the command through the REAL CL stack instead:
        ;; read-from-string -> eval (eval2) -> prin1-to-string -> channel data.
        "(defun ssh-eval-line (ssh cmd cmd-len)
  (let ((s (make-string cmd-len)))
    (dotimes (i cmd-len) (aset s i (code-char (aref cmd i))))
    (let ((result (handler-case (eval (read-from-string s))
                    (t (c) (list (quote error) c)))))
      (let ((rs (handler-case (prin1-to-string result)
                  (t (c) (prin1-to-string (quote unprintable))))))
        (let ((rl (length rs)))
          (let ((arr (make-array (+ rl 3))))
            (aset arr 0 61) (aset arr 1 32)
            (dotimes (i rl) (aset arr (+ 2 i) (char-code (aref rs i))))
            (aset arr (+ 2 rl) 10)
            (ssh-send-string ssh arr (+ rl 3))))))))"
        (string #\Newline)
        ;; One-shot single-threaded SSH bring-up: zero the Normal-WB scratch
        ;; (uninitialised DRAM on real HW), adopt the NIC, static IP 10.0.0.2,
        ;; register listen port 22, crypto pre-compute, actor/mailbox init, then
        ;; the synchronous poll loop.  Called from the REPL; net-actor-main
        ;; blocks, serving inline (net-accept-connection -> ssh-handle-connection).
        "(defun ssh-boot ()
  (let ((s (e1000-state-base))) (dotimes (i 1024) (setf (mem-ref (+ s (* i 8)) :u64) 0)))
  (let ((s (ssh-ipc-base))) (dotimes (i 76800) (setf (mem-ref (+ s (* i 8)) :u64) 0)))
  (let ((s (ssh-conn-base))) (dotimes (i 8192) (setf (mem-ref (+ s (* i 8)) :u64) 0)))
  (e1000-probe)
  (setf (mem-ref (+ (e1000-state-base) 24) :u32) 33554442)
  (setf (mem-ref (+ (e1000-state-base) 28) :u32) 16777226)
  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)
  (ssh-seed-random)
  (ssh-init-strings)
  (ssh-use-default-key)
  (pre-compute-host-sign)
  (pre-compute-server-eph (conn-ssh 0))
  (smp-init)
  (actor-init)
  (write-string-serial \"NETUP\") (write-char-serial 10)
  (net-actor-main))"
        (string #\Newline))
      ""))

(defvar *net-source*
  (if *net-build-p*
      (concatenate 'string
        ;; arch-rpi-cl.lisp, NOT arch-raspi3b.lisp: the legacy adapter's DMA
        ;; regions at 0x01000000 land INSIDE this ~20 MB image, and it carries a
        ;; miniature make-array/aref/aset runtime that would replace the real CL
        ;; one under last-defun-wins.  See that file's header.
        (%rpi-net-text "arch-rpi-cl.lisp")   (string #\Newline)
        (%rpi-net-text "dwc2.lisp")          (string #\Newline)
        (%rpi-net-text "usb.lisp")           (string #\Newline)
        (%rpi-net-text "cdc-ether.lisp")     (string #\Newline)
        ;; r8152.lisp AFTER cdc-ether: its e1000-probe/send/receive/rx-buf
        ;; defuns override the CDC-ECM ones (last-defun-wins) — on real
        ;; RTL8153 silicon the ECM config NAKs all bulk-IN, so the NIC is
        ;; driven in vendor config 1 with an explicit RX enable.
        (%rpi-net-text "r8152.lisp")         (string #\Newline)
        (%rpi-net-text "ip.lisp")            (string #\Newline)
        ;; MODUS_SSH_BUILD=1 folds crypto + the SSH transport here (after ip.lisp
        ;; so ssh-seed-random sees the NIC state and net-actor-main overrides
        ;; ip.lisp's); both "" otherwise.
        *crypto-source*
        *ssh-transport-source*
        (%rpi-net-text "http-client.lisp")   (string #\Newline)
        ;; Bigger HTTP response buffer.  The stock http-fetch-impl caps a
        ;; response at 4096 bytes and tcp-rx-copy bounds its copy to 4096 — too
        ;; small for a tarball.  Default is the same 32 KB override
        ;; build-aarch64.lisp's net build uses, so the two images fetch
        ;; identically sized bodies; MODUS_NET_BUFSZ raises it for a bigger
        ;; library (alexandria's .tar is 276480 bytes).  Baked as a defun, not a
        ;; defvar: MVM active limitation #7 means a defvar initform never runs.
        ;;
        ;; NOT OPTIONAL for a real library: %net-resp-cap is baked at BUILD time,
        ;; and past the cap tcp-rx-copy silently DROPS bytes while the log still
        ;; prints the full "FETCHED bytes=" length.  An 88%-zero-filled tarball
        ;; then presents as a wild-pointer data abort deep inside arithmetic.
        (format nil "~%(defun %net-resp-cap () ~D)~%"
                (or #+sbcl (let ((v (sb-ext:posix-getenv "MODUS_NET_BUFSZ")))
                             (and v (plusp (length v)) (parse-integer v)))
                    32768))
        "
(defun tcp-rx-copy (dest dest-off)
  (let ((buf (e1000-rx-buf)))
    (let ((ip-total (buf-read-u16-mem buf 16))
          (tcp-hdr-len (ash (logand (mem-ref (+ buf 46) :u8) #xF0) -2)))
      (let ((data-len (- ip-total (+ 20 tcp-hdr-len))))
        (let ((data-base (+ (+ buf 34) tcp-hdr-len)))
          (let ((i 0))
            (loop
              (when (>= i data-len) (return data-len))
              (let ((dst-idx (+ dest-off i)))
                (when (< dst-idx (%net-resp-cap))
                  (aset dest dst-idx (mem-ref (+ data-base i) :u8))))
              (setq i (+ i 1))))
          data-len)))))
(defun http-fetch-impl (url url-len)
  (let ((scheme-end (url-skip-http url url-len)))
    (let ((host-end (url-host-end url scheme-end url-len)))
      (let ((port (url-parse-port url host-end url-len))
            (path-start (url-path-off url scheme-end url-len)))
        (let ((host-len (- host-end scheme-end))
              (path-len (- url-len path-start)))
          (let ((ip (resolve-host url scheme-end host-end)))
            (when (zerop ip)
              (write-byte 68) (write-byte 78) (write-byte 83)
              (write-byte 58) (write-byte 48) (write-byte 10)
              (return 0))
            (when (zerop (tcp-connect ip port))
              (write-byte 84) (write-byte 67) (write-byte 80)
              (write-byte 58) (write-byte 70) (write-byte 10)
              (return 0))
            (let ((req-buf (make-array 512)))
              (let ((req-len (http-build-get url scheme-end host-len
                                              path-start path-len req-buf)))
                (tcp-send req-buf req-len)))
            (let ((resp (make-array (%net-resp-cap)))
                  (resp-len 0)
                  (done 0)
                  (idle 0))
              (loop
                (when (not (zerop done)) (return 0))
                (let ((n (tcp-receive 300)))
                  (if (> n 0)
                      (progn
                        (let ((copied (tcp-rx-copy resp resp-len)))
                          (setq resp-len (+ resp-len copied)))
                        (setq idle 0))
                      (setq idle (+ idle 1))))
                (when (zerop (tcp-state)) (setq done 1))
                (when (> idle 20) (setq done 1)))
              (tcp-close)
              (cons resp resp-len))))))))
"
        (string #\Newline))
      ""))

;; The URL is baked as a real defun (not a defvar — MVM active limitation #7
;; means a defvar initform never runs at boot).  MODUS_NET_URL overrides it.
(defvar *net-url-source*
  (if *net-build-p*
      (format nil "~%(defun %net-fetch-url () ~S)~%(defun %lib-call-expr () ~S)~%"
              (or #+sbcl (sb-ext:posix-getenv "MODUS_NET_URL")
                  "http://10.0.2.2:8080/sha1.tar")
              ;; #263 rung 3: the expression evaluated AFTER the fetched system
              ;; is installed + loaded.  A STRING, read at runtime — the build
              ;; reader cannot read `sha1:sha1-hex' (no SHA1 package until the
              ;; library loads).  Override with MODUS_LIB_EXPR.
              (or #+sbcl (sb-ext:posix-getenv "MODUS_LIB_EXPR")
                  "(sha1:sha1-hex \"abc\")"))
      ""))

;; The rung-2 pipeline: bring the USB NIC up, DHCP for an address, fetch a
;; .tar over real HTTP and report its length + an FNV-1a-32 checksum so the
;; body can be verified byte-for-byte against the file the host served; then
;; (rung 3) INSTALL it, LOAD its sources and CALL a function from it.  The
;; installer itself — lib/tar.lisp + lib/install-tarball.lisp — is NOT here: it
;; comes from build-cli-common's *CLI-BARE-METAL-TARBALL* branch, baked with the
;; CL runtime, which is earlier in the blob and therefore a backward reference.
(defvar *net-driver-source*
  (if *net-build-p* "
;; Build a byte-array URL from a Lisp string (chars are CHARACTERS in the real
;; CL reader — char-code them, unlike the legacy repl-source images where a
;; string slot already held the code).
(defun %net-url (s)
  (let* ((n (length s)) (arr (make-array n)))
    (dotimes (i n) (aset arr i (char-code (char s i))))
    (cons arr n)))

;; FNV-1a 32-bit over the first N bytes of ARR.  Every intermediate fits in a
;; 62-bit fixnum (2^32 * 2^24), so no bignum path is involved.
(defun %net-fnv1a (arr n)
  (let ((h 2166136261) (i 0))
    (loop
      (when (>= i n) (return h))
      (when (zerop (logand i 1023))
        (write-string-serial \"[f\") (print-dec i) (write-string-serial \"]\"))
      (setq h (logand (* (logxor h (aref arr i)) 16777619) #xFFFFFFFF))
      (setq i (+ i 1)))))

;; Fetch URL-STRING; return (body-array . body-length) or NIL.
(defun net-fetch-bytes (url-string)
  (let* ((u (%net-url url-string))
         (result (http-fetch-impl (car u) (cdr u))))
    (if (or (null result) (eq result 0))
        nil
        (let* ((resp (car result))
               (resp-len (cdr result))
               (body-off (http-find-body resp resp-len)))
          (let* ((blen (- resp-len body-off))
                 (out (make-array blen)))
            (let ((i 0))
              (loop
                (when (>= i blen) (return nil))
                (aset out i (aref resp (+ body-off i)))
                (setq i (+ i 1))))
            (cons out blen))))))

;; Fetch URL-STRING and REPORT it: body length, the first four bytes (a .tar.gz
;; must start 1F 8B 08) and an FNV-1a-32 of the whole body, which is what makes
;; the transfer verifiable byte-for-byte against the file the server holds.
;; A real defun (not driver-only text) so it lands in the symbol-function table
;; and can be called from the REPL as well as from the boot pipeline.
(defun net-fetch-report (url-string)
  (handler-case
      (let ((tb (net-fetch-bytes url-string)))
        (if (null tb)
            (progn (write-string-serial \"FETCH-FAIL\") (write-char-serial 10) 0)
            (progn
              (write-string-serial \"FETCHED bytes=\")
              (print-dec (cdr tb)) (write-char-serial 10)
              (write-string-serial \"HEAD4=\")
              (let ((k 0))
                (loop (when (>= k 4) (return nil))
                      (print-hex-byte (aref (car tb) k)) (write-char-serial 32)
                      (setq k (+ k 1))))
              (write-char-serial 10)
              (write-string-serial \"OUTLEN=\") (print-dec (length (car tb)))
              (write-char-serial 10)
              (write-string-serial \"FNV1A=\")
              (print-dec (%net-fnv1a (car tb) (cdr tb)))
              (write-char-serial 10)
              (cdr tb))))
    (t (c) (write-string-serial \"FETCH-ERR\") (write-char-serial 10) -1)))

;; Print the dotted quad held as a big-endian NUMBER in the u32 at ADDR.  The
;; DHCP router option (state+0x1C) is stored this way by dhcp-parse-offer, so
;; reading its four bytes in memory order prints them reversed.
(defun %net-print-ip-u32 (addr)
  (let ((v (mem-ref addr :u32)))
    (print-dec (logand (ash v -24) 255)) (write-char-serial 46)
    (print-dec (logand (ash v -16) 255)) (write-char-serial 46)
    (print-dec (logand (ash v -8) 255))  (write-char-serial 46)
    (print-dec (logand v 255))))

;; --- #263 rung 3 -------------------------------------------------------------
;; Fetch the archive ONCE, report it (rung-2 evidence: length / magic / FNV-1a),
;; then INSTALL it (untar -> .asd -> component order -> read+eval each file) and
;; CALL a function from the freshly loaded system.
;;
;; The call expression is a STRING read at runtime, never a literal in this
;; source: `(sha1:sha1-hex \"abc\")' cannot be READ at build time because the
;; SHA1 package does not exist until the library has loaded.  %lib-call-expr is
;; baked as a defun (MVM active limitation #7: a defvar initform never runs at
;; boot) and is overridable with MODUS_LIB_EXPR.
(defun net-install-and-call (url-string)
  ;; lib/tar.lisp's (defvar *tar-block-size* 512) init-thunk does NOT run at
  ;; boot, so the variable is NIL and (+ off *tar-block-size*) wedges
  ;; tar-do-entries.  Set it explicitly.  Same fix build-aarch64.lisp makes.
  (setq *tar-block-size* 512)
  (handler-case
      (let ((tb (net-fetch-bytes url-string)))
        (if (null tb)
            (progn (write-string-serial \"LIB-FETCH-FAIL\") (write-char-serial 10) nil)
            (progn
              (write-string-serial \"FETCHED bytes=\")
              (print-dec (cdr tb)) (write-char-serial 10)
              (write-string-serial \"HEAD4=\")
              (let ((k 0))
                (loop (when (>= k 4) (return nil))
                      (print-hex-byte (aref (car tb) k)) (write-char-serial 32)
                      (setq k (+ k 1))))
              (write-char-serial 10)
              (write-string-serial \"FNV1A=\")
              (print-dec (%net-fnv1a (car tb) (cdr tb))) (write-char-serial 10)
              ;; --- INSTALL: untar + parse .asd + read/eval every component ---
              (let ((sys (handler-case (install-tarball-from-bytes (car tb))
                           (t (c) nil))))
                (write-string-serial \"LIB-SYSTEM=\")
                (if (stringp sys) (write-string-serial sys)
                    (write-string-serial \"<install-failed>\"))
                (write-char-serial 10))
              ;; --- CALL: evaluate an expression from the installed system ---
              (write-string-serial \"LIB-EXPR=\")
              (write-string-serial (%lib-call-expr)) (write-char-serial 10)
              (write-string-serial \"LIB-VALUE=\")
              (handler-case
                  (let ((v (eval (read-from-string (%lib-call-expr)))))
                    (cond ((stringp v) (write-string-serial v))
                          ((integerp v) (print-dec v))
                          ((null v) (write-string-serial \"NIL\"))
                          (t (handler-case (write-object v)
                               (t (c) (write-string-serial \"<unprintable>\"))))))
                (t (c)
                  (write-string-serial \"<call-error> \")
                  (handler-case (write-object c)
                    (t (c2) (write-string-serial \"<err>\")))))
              (write-char-serial 10)
              t)))
    (t (c) (write-string-serial \"LIB-ERR\") (write-char-serial 10) nil)))

(defun run-net-pipeline ()
  (write-string-serial \"NET-PIPELINE-START\") (write-char-serial 10)
  ;; 1. DWC2 host controller + USB enumeration + CDC Ethernet.
  ;;    Prints DWC2:OK / PORT:xx / USB:vvvv:pppp / MAC:.. / CDC:OK itself.
  (let ((r (handler-case (cdc-ether-init) (t (c) 0))))
    (write-string-serial \"CDC-INIT=\") (print-dec r) (write-char-serial 10)
    (if (zerop r)
        (progn (write-string-serial \"NET-PIPELINE-ABORT\") (write-char-serial 10))
        (progn
          ;; 2. DHCP.  Prints DHCP:D / DHCP:O / DHCP:R / DHCP:A itself.
          (handler-case (dhcp-client) (t (c) nil))
          (let ((state (e1000-state-base)))
            (write-string-serial \"IP=\")
            (print-dec (mem-ref (+ state #x18) :u8)) (write-char-serial 46)
            (print-dec (mem-ref (+ state #x19) :u8)) (write-char-serial 46)
            (print-dec (mem-ref (+ state #x1A) :u8)) (write-char-serial 46)
            (print-dec (mem-ref (+ state #x1B) :u8)) (write-char-serial 10)
            (write-string-serial \"GW=\")
            (%net-print-ip-u32 (+ state #x1C)) (write-char-serial 10))
          ;; 3. HTTP GET of the library .tar from the QEMU slirp gateway, then
          ;;    (rung 3) install it, load it, and call a function from it.
          (net-install-and-call (%net-fetch-url)))))
  (write-string-serial \"NET-PIPELINE-DONE\") (write-char-serial 10))
"
      ""))

;; MODUS_NET_NOAUTO=1 — build the net stack IN but do not start it.
;;
;; This is the on-hardware development knob.  With the pipeline spliced,
;; kernel-main runs DHCP -> TCP -> HTTP before the REPL, which is exactly wrong
;; when the NIC is one Modus cannot drive yet: DHCP does not error, it WAITS,
;; so the board looks wedged and never reaches a prompt.  With NOAUTO the whole
;; stack — dwc2 host, usb enumeration, cdc-ether, ip, http-client — is compiled
;; in and reachable by name, and the image boots straight to the serial REPL,
;; where `(dwc2-init)`, `(usb-enumerate)`, `(usb-control-transfer ...)` and
;; `(usb-bulk-receive ...)` can be driven BY HAND against real silicon.  The
;; pipeline is then just `(run-net-pipeline)` typed at the prompt.
;;
;; That turns bring-up for a new NIC from a ~20-minute rebuild per hypothesis
;; into a line typed at a live board.
(defvar *net-noauto-p*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_NET_NOAUTO")))
    (and v (string= v "1"))))

;; Spliced into kernel-main.  "" when the flag is off => zero bytes added.
(defvar *net-pipeline-call*
  (if (and *net-build-p* (not *net-noauto-p*))
      "  (handler-case (run-net-pipeline) (t (c) nil))
"
      ""))

;;; BARE-METAL NET SEAM.  build-cli-common splices this right after
;;; *STAGE2-TEST-SOURCE* — after the CL runtime, the compiler and mvm-eval so the
;;; arch adapter's own definitions win under last-defun-wins (arch-rpi-cl.lisp
;;; deliberately overrides WRITE-BYTE), and BEFORE the driver, whose kernel-main
;;; calls run-net-pipeline.  It is ALSO spliced into *ALL-RUNTIME-SOURCE*, so
;;; every defun here reaches the symbol-function table and every token here
;;; reaches *SYM-NAME-TABLE*.
;;; BARE-METAL JIT co-init override.  build-cli-common's aarch64 co-init sets
;;; *aarch64-linux-mode* T (runtime-emitted traps = Linux syscalls).  On bare
;;; metal the runtime translator must instead emit the BARE-METAL trap paths:
;;; serial via the UART this build selected (mirrored from the build-time
;;; console selection above), exec pages via the #x0531 bump allocator.  Wins
;;; over the common co-init by last-defun-wins (net-source is appended after
;;; the JIT translator block in *all-runtime-source*).  Also zero the bump
;;; word explicitly at init — cheap belt to the trap's own range-check braces.
;;; NOTE: the serial globals (*aarch64-serial-base* etc.) are not set until
;;; the console-selection block AFTER build-cli-common loads, so this defvar
;;; re-derives the SAME choice from the env vars directly (identical logic to
;;; boot-rpi-cl.lisp's *rpi-cl-chainload* + the console block below).
(defvar *rpi-jit-coinit-override*
  (if *jit-on*
      (let* ((chain (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_CHAINLOAD")))
                      (and v (string= v "1"))))
             (mini (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_MINIUART")))
                     (if (and v (plusp (length v)))
                         (not (string= v "0"))
                         chain))))
        (format nil "
(defun %init-aarch64-translator ()
  (let ((map (make-array 23)))
    (aset map 0 0) (aset map 1 1) (aset map 2 2) (aset map 3 3)
    (aset map 4 19) (aset map 5 20) (aset map 6 21) (aset map 7 22) (aset map 8 23)
    (aset map 9 nil) (aset map 10 nil) (aset map 11 nil) (aset map 12 nil)
    (aset map 13 nil) (aset map 14 nil) (aset map 15 nil)
    (aset map 16 0) (aset map 17 24) (aset map 18 25) (aset map 19 26)
    (aset map 20 31) (aset map 21 29) (aset map 22 nil)
    (setq *a64-vreg-to-phys* map))
  (when (null *mvm-label-counter*) (setq *mvm-label-counter* 0))
  (setq *aarch64-stack-align-16* nil)
  (setq *aarch64-linux-mode* nil)
  (setq *aarch64-gc-native-mcgc* t)
  (setq *aarch64-gc-trampoline-call-via-bl* nil)
  (setq *aarch64-gc-trampoline-label* 1)
  ;; *aarch64-gc-bitmap-enabled* at runtime: MUST eventually be T — while
  ;; NIL, objects allocated by JIT'd code carry no object-start bit, the
  ;; native GC's conservative-root validation REJECTS stack roots pointing
  ;; at them, and they get dropped while live.  PROVEN consequence (QEMU,
  ;; 2026-08-25): loading alexandria, a dangling string wrote the symbol
  ;; name MAP-PRODUCT's char codes over the GC config page at #x10000000,
  ;; wrecking from_start/to_start/space_size -> wild-pointer data abort in
  ;; %PARSE-START-END.  But enabling it wedged the image in a recursive
  ;; exception storm at the first JIT'd alloc (PC pinned at VBAR+0x200,
  ;; gdbstub unable to translate guest addresses => page tables corrupted).
  ;; MODUS_RPI_JIT_BITMAP=1 builds the enable in for debugging that wedge.
  ~A
  ~A
  (setf (mem-ref #x13FFFFF0 :u64) #x14000000)
  t)
"
                (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_JIT_BITMAP")))
                  (if (and v (string= v "1"))
                      "(setq *aarch64-gc-bitmap-enabled* t)"
                      ";; bitmap-enable off (MODUS_RPI_JIT_BITMAP unset)"))
                (if mini
                    "(setq *aarch64-serial-base* #x3F215040)
  (setq *aarch64-serial-width* 2)
  (setq *aarch64-serial-tx-poll* (list #x14 5 :tbz))
  (setq *aarch64-serial-rx-poll* (list #x14 0 :tbz))"
                    "(setq *aarch64-serial-base* #x3F201000)
  (setq *aarch64-serial-width* 0)
  (setq *aarch64-serial-tx-poll* nil)
  (setq *aarch64-serial-rx-poll* (list #x18 4 :tbnz))")))
      ""))

(defvar *cli-bare-metal-net-source*
  (concatenate 'string *net-source* *net-url-source* *net-driver-source*
               *rpi-jit-coinit-override*))

;;; ARCH SLOT: the toplevel entry / probe program.  The hosted CLIs hand off to
;;; cli-toplevel here; this image runs the same E2SMOKE self-check the bare ANSI
;;; gate runs, then the net pipeline, then the serial REPL.
(defvar *cli-arch-kernel-epilogue*
  (concatenate 'string "
  ;; --- in-image self-check ------------------------------------------------
  ;; Same E2SMOKE probes the bare ANSI gate runs: prove compile->bytecode->
  ;; interpret works (including a defun in one form called from a later one)
  ;; BEFORE handing the machine to the user.  add=3 sqr=25 defcall=49
  ;; persist-call=36 persist-fn=45.
  (write-string-serial \"E2SMOKE-START\") (write-char-serial 10)
  (write-string-serial \"add=\")
  (print-dec (handler-case (mvm-eval (quote (+ 1 2))) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"sqr=\")
  (print-dec (handler-case (mvm-eval (quote (let ((x 5)) (* x x)))) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"defcall=\")
  (print-dec (handler-case
                 (mvm-eval-forms (list (quote (defun sq (x) (* x x))) (quote (sq 7))))
               (t (c) -1)))
  (write-char-serial 10)
  (handler-case (mvm-eval (quote (defun pf (x) (* x 9)))) (t (c) nil))
  (write-string-serial \"persist-call=\")
  (print-dec (handler-case (mvm-eval (quote (pf 4))) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"persist-fn=\")
  (print-dec (handler-case (funcall (quote pf) 5) (t (c) -1)))
  (write-char-serial 10)
  (write-string-serial \"E2SMOKE-END\") (write-char-serial 10)
"
    ;; --- #209 rung 2: DWC2/USB -> DHCP -> TCP -> HTTP GET -------------------
    ;; "" unless MODUS_NET_BUILD=1.
    *net-pipeline-call*
    "
  ;; --- the REPL (lib/serial-repl.lisp) ------------------------------------
  (handler-case (cl-serial-repl) (t (c) nil))
  (halt))
"))

;;; ============================================================
;;; THE SHARED ASSEMBLY
;;; ============================================================

(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

;;; ============================================================
;;; Build the bare-metal Pi image
;;; ============================================================

;; Bare-metal Raspberry Pi 3B boot descriptors.  boot-rpi.lisp arrives via
;; lib/load-mvm.lisp already; boot-rpi-cl.lisp is loaded AFTER it and redefines
;; `rpi-boot-descriptor' to the CL-lineage one (kernel8.img @0x80000, MMU off,
;; identity addressing, PL011 console, Cheney heap registers).  The
;; redefinition is process-local, so `build-rpi-ssh' / `-hid' / `-periph' are
;; untouched.
(mvm-load "boot/boot-rpi-cl.lisp")

(in-package :modus.mvm)

;; Install the AArch64 translator in BARE-METAL mode (*aarch64-linux-mode* is
;; NIL by default), so TRAP #x0300/#x0301 emit UART MMIO rather than Linux
;; syscalls — which is exactly what write-char-serial / read-char-serial need.
(install-aarch64-translator)

;; CONSOLE SELECTION.  Two different boards, two different UARTs:
;;
;;   QEMU raspi3b (SD-card / -kernel path)  -> PL011 UART0 at 0x3F201000,
;;     which is what QEMU wires to serial0.
;;   REAL Pi Zero 2 W (UART chain-load)     -> BCM2835 mini UART at 0x3F215040.
;;     On a Pi Zero 2 W the PL011 is routed to Bluetooth and the mini UART owns
;;     GPIO14/15, so an image that writes PL011 transmits into the Bluetooth
;;     modem and the wires stay SILENT.
;;
;; That difference is invisible under emulation and cost a full 88-minute
;; chain-load cycle to notice: the loader's own BOOT/RDY/AD:/SZ: arrive over
;; the mini UART (proving the pins), yet the image we hand control to was
;; built for PL011, so a PERFECTLY SUCCESSFUL chain-load would look exactly
;; like a dead transfer.
;;
;; The old comment here said the mini UART "could print but never read"
;; because `read-char-serial' hardcoded PL011's UARTFR+0x18/RXFE-bit-4.  That
;; is FIXED (1e84418): the RX ready-poll is now parameterized via
;; *aarch64-serial-rx-poll*, exactly mirroring the TX side.
(defvar *rpi-cl-miniuart*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_MINIUART")))
    (if (and v (plusp (length v)))
        (not (string= v "0"))
        ;; Default: follow the chain-load flag, because chain-loading is the
        ;; real-hardware path and SD/QEMU is the PL011 path.
        *rpi-cl-chainload*))
  "T => target the BCM2835 mini UART (real Pi Zero 2 W).  NIL => PL011 (QEMU).")

(if *rpi-cl-miniuart*
    (progn
      ;; AUX mini UART: AUX_MU_IO at base+0, AUX_MU_LSR at base+0x14.
      ;; TX: wait while LSR bit 5 (transmitter empty) is CLEAR.
      ;; RX: wait while LSR bit 0 (data ready)        is CLEAR.
      ;; AUX registers require 32-bit access, hence width 2.
      (setf *aarch64-serial-base* #x3F215040)
      (setf *aarch64-serial-width* 2)
      (setf *aarch64-serial-tx-poll* '(#x14 5 :tbz))
      (setf *aarch64-serial-rx-poll* '(#x14 0 :tbz))
      (format t "~&;; CONSOLE: BCM2835 mini UART 0x3F215040 (real Pi Zero 2 W)~%"))
    (progn
      (setf *aarch64-serial-base* #x3F201000)
      (setf *aarch64-serial-width* 0)
      (setf *aarch64-serial-tx-poll* nil)
      (setf *aarch64-serial-rx-poll* '(#x18 4 :tbnz))
      (format t "~&;; CONSOLE: PL011 UART0 0x3F201000 (QEMU raspi3b)~%")))

;; No GICv2 on a BCM2837, and nothing here needs interrupts (the REPL polls
;; the UART), so leave *aarch64-setup-irq-enable* NIL — the QEMU-virt bare
;; image only turns it on for its per-test vtimer deadline IRQ.
;; No actor scheduler either, so no sched lock: the translator then emits no
;; load/store-exclusive, which matters because this image runs MMU-off and
;; exclusives are UNPREDICTABLE on Device memory.
;; #160: emit the object-start + cons-kind bit-set at every alloc site, so
;; %gc-is-start can reject a conservative candidate that lands mid-object
;; instead of degrading to T and copying it.  Matches build-aarch64-cli.lisp
;; and build-aarch64-linux.lisp; the RPi image was the last aarch64 target
;; still running the collector with NO bitmap.  The backing RAM is reserved and
;; zeroed by %rpi-gc-bitmap-init in kernel-main.
(setf *aarch64-gc-bitmap-enabled* t)

;; ALLOC-OVERSHOOT GUARD BAND (#277 root cause): the gc-check compares x24
;; BEFORE the alloc, so an object allocated just under the limit writes
;; header + zero-init up to its full size past it — and this image's upper
;; semispace ends at #x10000000 with the GC config page immediately after.
;; Watchpoint-proven (2026-08-25): a 64K-element a64-buffer alloc zeroed
;; space_size at #x10000050 mid-JIT-translate, and the next collection ran on
;; garbage geometry.  8 MB clears every known large alloc (512 KB JIT code
;; array, 400 KB MODUS_NET_BUFSZ) with ~48 MB/semispace left.  Applied by the
;; baked trampoline at every exit AND by boot-rpi-cl's initial x25.
(setf *aarch64-gc-limit-guard* #x800000)

;; #267 step 1: use the NATIVE aarch64 Cheney collector — the same one
;; build-aarch64-cli.lisp runs — instead of falling through to gc.lisp's
;; INTERPRETED %gc-collect.  This image was the LAST target in the tree still on
;; the Lisp collector (x64 has its native trampoline, hosted aarch64 sets this
;; flag, i386 has its own arm), and it is also the only target that fails to
;; load alexandria.
;;
;; The BL flag is REQUIRED here, not cosmetic.  Native MCGC normally calls the
;; trampoline as `BLR x28`, and x28 is materialised by emit-linux-aarch64-entry
;; — the LINUX entry.  boot-rpi-cl.lisp never touches x28, so the mcgc flag
;; ALONE made every gc-check an indirect call through a garbage register:
;; measured 2026-08-20, the image branched into space on its first collection and
;; re-entered at the image start (boot banner printed twice) with ZERO logged
;; exceptions, since control never reached the EL2 vectors.  BL's ±128MB reach
;; covers this ~57MB image with room to spare.
(setf *aarch64-gc-native-mcgc* t)
(setf *aarch64-gc-trampoline-call-via-bl* t)

;; DEV/TRIAGE KNOB — MODUS_RPI_GC_SHIM=1 turns the native collector OFF, so
;; every gc-check lands in translate-aarch64's register-saving SHIM, which
;; CALLS mvm/gc.lisp's Lisp %GC-COLLECT.  That is the ONLY collector arm
;; bootable here that actually executes mvm/gc.lisp's Cheney code — x86-64
;; (hosted and bare) and i386 all run native trampolines — so it is how a
;; change to that file gets executed rather than merely compiled.  CLAUDE.md's
;; "shim behaviour" table (stage 3) was measured this way.  Default OFF, so the
;; shipping image is byte-identical.
#+sbcl
(let ((v (sb-ext:posix-getenv "MODUS_RPI_GC_SHIM")))
  (when (and v (> (length v) 0) (not (string= v "0")))
    (setf *aarch64-gc-native-mcgc* nil)
    (setf *aarch64-gc-trampoline-call-via-bl* nil)
    (format t "~%;; RPi CL REPL: NATIVE MCGC OFF — the LISP collector ~
               (mvm/gc.lisp %GC-COLLECT) runs via the aarch64 shim~%")))

(setf *aarch64-sched-lock-addr* nil)

;; SP alignment stays 8-byte (bare-metal EL1 with SCTLR.SA off), unlike Linux
;; EL0 which demands 16.  *aarch64-fn-align-offset* stays 0: the unified
;; buffer's alignment loop measures absolute position INCLUDING the boot
;; preamble, and the image base 0x80000 is 16-byte aligned, so fn entries land
;; on 16-byte VAs and the OR-3 fn tagging yields clean nibble-3 tags.

;; Bare-metal handler-stack helpers: the label vars stay NIL at toplevel —
;; cross.lisp's unified aarch64 emit binds fresh labels around the boot-entry
;; and translate calls (assemble-kernel-image).
(setf *aarch64-handler-pop-label* nil)
(setf *aarch64-handler-push-label* nil)
(setf *aarch64-gc-trampoline-label* nil)

#+sbcl
(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf modus.mvm::*write-symmap-path* sm)))

(format t "~%Compiling bare-metal RPi CL REPL image (~D chars)...~%"
        (length cl-user::*full-source*))

(let ((image (build-image :target :rpi
                          :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_CL_REPL_OUT")
                  "/tmp/piboot/kernel8.img")))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (let ((n (length modus.mvm::*redefinition-log*)))
      (when (> n 0)
        (format t "~%REDEFINITIONS: ~D total (grep the build log for \"NOTE: redefining\")~%" n)))

    ;; ------------------------------------------------------------------
    ;; BUILD-TIME MEMORY-MAP ASSERTS (Pi 3B geometry).
    ;;
    ;; The Pi map is TIGHTER than QEMU virt's in one direction and roomier in
    ;; another: DRAM starts at 0 (so the runtime VAs are plain RAM, no remap),
    ;; but the BCM2837 peripheral window at 0x3F000000 caps everything, and the
    ;; image loads at 0x80000 rather than being handed a whole 2 MB-aligned
    ;; region.  Assert the three ways a too-large image silently corrupts:
    ;;   1. image end reaching the stack region  -> pushes shred native code
    ;;      (this is precisely what boot-rpi.lisp's 0x00200000 stack top does
    ;;      to any CL-sized image, task #47's failure class)
    ;;   2. image end reaching the heap base     -> allocation over code
    ;;   3. heap end reaching the peripherals    -> DMA/MMIO aliasing
    ;; ------------------------------------------------------------------
    (let* ((image-bytes (length (kernel-image-image-bytes image)))
           (image-va-start #x80000)
           (image-va-end   (+ image-va-start image-bytes))
           (stack-top      +rpi-cl-stack-top+)
           (stack-headroom (* 8 1024 1024))
           (stack-va-lo    (- stack-top stack-headroom))
           (heap-base      +rpi-cl-heap-base+)
           (heap-end       +rpi-cl-heap-end+)
           (periph-base    #x3F000000))
      (when (>= image-va-end stack-va-lo)
        (error "BUILD-TIME ASSERT: image [~X..~X] (~,1F MB) reaches the stack ~
                region [~X..~X].  Stack pushes would overwrite native code."
               image-va-start image-va-end (/ image-bytes 1024.0 1024.0)
               stack-va-lo stack-top))
      (when (>= image-va-end heap-base)
        (error "BUILD-TIME ASSERT: image end ~X reached the heap base ~X."
               image-va-end heap-base))
      ;; #160 bitmaps live at 0x05000000 / 0x05100000, 0xE0000 bytes each
      ;; (1 bit / 16-byte granule over the 112 MB heap).  They must clear the
      ;; image below and the stack above; both are checked here rather than
      ;; discovered as heap corruption at runtime.  Keep in step with the
      ;; %rpi-gc-bitmap-init call in kernel-main.
      (let* ((objmap-lo  #x05000000)
             (consmap-lo #x05100000)
             (map-bytes  #xE0000)
             (maps-hi    (+ consmap-lo map-bytes)))
        (when (< objmap-lo image-va-end)
          (error "BUILD-TIME ASSERT: #160 object-start bitmap at ~X is inside ~
                  the image (ends ~X)." objmap-lo image-va-end))
        (when (> (+ objmap-lo map-bytes) consmap-lo)
          (error "BUILD-TIME ASSERT: #160 object-start bitmap ~X+~X overlaps ~
                  the cons-kind bitmap at ~X." objmap-lo map-bytes consmap-lo))
        (when (>= maps-hi stack-va-lo)
          (error "BUILD-TIME ASSERT: #160 bitmaps end ~X reach the stack low ~
                  water ~X." maps-hi stack-va-lo))
        (format t "  gc bitmaps ~8,'0X .. ~8,'0X  (#160 object-start + cons-kind)~%"
                objmap-lo maps-hi))
      (when (> heap-end periph-base)
        (error "BUILD-TIME ASSERT: heap end ~X is inside the BCM2837 ~
                peripheral window at ~X." heap-end periph-base))
      (format t "~%Pi 3B memory map (all identity-mapped DRAM, MMU off):~%")
      (format t "  image      ~8,'0X .. ~8,'0X  (~,2F MB)~%"
              image-va-start image-va-end (/ image-bytes 1024.0 1024.0))
      (format t "  stack top  ~8,'0X            (grows down, ~,1F MB clear of image)~%"
              stack-top (/ (- stack-va-lo image-va-end) 1024.0 1024.0))
      (format t "  heap       ~8,'0X .. ~8,'0X  (112 MB, midpoint ~8,'0X)~%"
              heap-base heap-end +rpi-cl-heap-mid+)
      (format t "  periph     ~8,'0X ..            (PL011 UART0 at 3F201000)~%"
              periph-base)
      ;; #209 rung 2: the USB/net DMA + state block (net/arch-rpi-cl.lisp).
      ;; It must clear BOTH the runtime metadata window (which ends at
      ;; 0x10200000) and the peripherals, or the NIC DMAs over live data —
      ;; the exact failure build-aarch64.lisp's net relocation exists to stop.
      (when cl-user::*net-build-p*
        (let ((net-lo #x11000000)
              (net-hi #x11113000)
              (meta-end #x10200000)
              ;; SMALLEST board this image must run on: Pi Zero 2 W, 512 MB,
              ;; minus gpu_mem=16 => the ARM sees 0x1F000000 (496 MB).  QEMU
              ;; raspi3b models a 1 GiB Pi 3B, so anything between 496 MB and
              ;; 1 GiB passes every emulated test and addresses a HOLE on the
              ;; real target.  The net block used to sit at 0x20000000 =
              ;; exactly 512 MB and was unusable on hardware for that reason.
              (min-board-ram #x1F000000))
          (when (< net-lo meta-end)
            (error "BUILD-TIME ASSERT: net DMA base ~X is inside the runtime ~
                    metadata window (ends ~X)." net-lo meta-end))
          (when (>= net-hi periph-base)
            (error "BUILD-TIME ASSERT: net region end ~X is inside the ~
                    BCM2837 peripheral window at ~X." net-hi periph-base))
          (when (>= net-hi min-board-ram)
            (error "BUILD-TIME ASSERT: net region end ~X is at/above ~X, the ~
                    RAM the ARM sees on a Pi Zero 2 W (512 MB - gpu_mem=16). ~
                    It would DMA into a hole on the real target while passing ~
                    every QEMU raspi3b test, because raspi3b models 1 GiB."
                   net-hi min-board-ram))
          (format t "  net/DMA    ~8,'0X .. ~8,'0X  (USB + E1000-shaped state)~%"
                  net-lo net-hi))))

    (format t "~%Run: qemu-system-aarch64 -M raspi3b -kernel ~A -serial stdio -serial null -display none -no-reboot~A~%"
            path
            (if cl-user::*net-build-p*
                " -device usb-net,netdev=net0 -netdev user,id=net0"
                ""))))
