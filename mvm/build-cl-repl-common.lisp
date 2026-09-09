;;;; build-cl-repl-common.lisp — the shared body of the BARE-METAL AArch64 CL
;;;; REPL images, as a THIN TAIL over mvm/build-cli-common.lisp.
;;;;
;;;; Loaded by BOTH bare-metal AArch64 clean images AFTER each sets the platform
;;;; selector *CL-REPL-PLATFORM*:
;;;;   mvm/build-rpi-cl-repl.lisp   (:rpi)   — Raspberry Pi 3B / Zero 2 W
;;;;                                           kernel8.img, DWC2/USB net stack
;;;;   mvm/build-aarch64.lisp       (:virt)  — QEMU virt fixpoint-MMU kernel,
;;;;                                           PCI/E1000 net stack
;;;;
;;;; Same contract as mvm/build-ansi-common.lisp (the four ANSI gate runners'
;;;; shared harness): the wrapper binds ONE selector, loads this file, and this
;;;; file does everything else — including the build tail.  These are CLEAN
;;;; images: no test corpus is baked in.  See CLAUDE.md "Build taxonomy".
;;;;
;;;; An AArch64 kernel that drops into Modus's own self-hosted Common Lisp REPL
;;;; over the serial port: the CL reader, `eval' = mvm-eval (compile -> MVM
;;;; bytecode -> mvm-interpret), and the CL printer.  There is NO second Lisp
;;;; here — `mvm/repl-source.lisp' (the 708-line toy reader/printer that every
;;;; legacy build-rpi-* and build-pizero2w-* script bakes) is not part of it.
;;;;
;;;; ============================================================
;;;; PLATFORM-DIVERGENT SITES — the complete list.  Everything else in this
;;;; file is byte-for-byte common to both images.
;;;; ============================================================
;;;;
;;;;   1. *JIT-ON*                   :rpi T / :virt NIL.  See the defvar.
;;;;   2. *CLI-ARCH-KERNEL-PROLOGUE* :virt zeroes its own BSS-equivalent slots
;;;;                                 (the RPi does it in the boot preamble).
;;;;   3. *NET-SOURCE*               DWC2/USB/CDC/r8152 vs PCI/E1000, and the
;;;;                                 DMA/state region bases that go with them.
;;;;   4. run-net-pipeline           cdc-ether-init vs pci-assign-bars +
;;;;                                 e1000-probe.  (Everything else in
;;;;                                 *NET-DRIVER-SOURCE* is shared.)
;;;;   5. MODUS_SSH_BUILD            :rpi only (its address map is Pi DRAM).
;;;;   6. boot descriptor            boot/boot-rpi-cl.lisp + (build-image
;;;;                                 :target :rpi)  vs  boot/boot-aarch64.lisp
;;;;                                 + :target :fixpoint + the fixpoint NIL
;;;;                                 value and re-entry guard.
;;;;   7. console                    :rpi selects PL011-vs-mini-UART by env;
;;;;                                 :virt leaves *AARCH64-SERIAL-BASE* NIL so
;;;;                                 cross.lisp takes the boot descriptor's
;;;;                                 +TDK-UART-VA+ (0x20000000 -> PA 0x9000000).
;;;;   8. output path                /tmp/piboot/kernel8.img vs
;;;;                                 /tmp/modus-aarch64-cl-repl.bin.
;;;;   9. build-time memory asserts  BCM2837 peripheral window + Pi Zero 2 W
;;;;                                 496 MB board RAM  vs  QEMU virt stack/heap.
;;;;
;;;; ============================================================
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
;;;; and the build tail below: the boot descriptor, the console selection, the
;;;; GC knobs and BUILD-IMAGE.
;;;;
;;;; MEMORY MAP.  IDENTICAL VAs on both platforms — that is what makes this file
;;;; shareable at all.  On the Pi they are plain identity-mapped DRAM (Pi DRAM
;;;; starts at 0); on QEMU virt boot/boot-aarch64.lisp's fixpoint page tables
;;;; remap them onto DRAM at PA 0x40000000+ (VA 0x00000000-0x0FFFFFFF -> PA
;;;; +0x40000000 via L2[0..127], VA 0x10000000-0x101FFFFF -> PA 0x50000000 via
;;;; L2[128]).  Image @0x80000, stack top 0x08000000 growing down, Cheney heap
;;;; [0x09000000, 0x10000000) with the semispace midpoint at 0x0C800000, runtime
;;;; metadata ("BSS") at 0x1000xxxx.  On the Pi everything must additionally
;;;; stay below the BCM2837 peripheral window at 0x3F000000.
;;;;
;;;; Usage: sbcl --dynamic-space-size 12288 --script mvm/build-rpi-cl-repl.lisp
;;;;        sbcl --dynamic-space-size 12288 --script mvm/build-aarch64.lisp
;;;; Run:   qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \
;;;;          -serial stdio -serial null -display none
;;;;        qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 \
;;;;          -kernel /tmp/modus-aarch64-cl-repl.bin -nographic -no-reboot
;;;; Output path override: MODUS_CL_REPL_OUT.

;;; The platform selector.  The including script binds it BEFORE loading this
;;; file; there is deliberately no default, so a new wrapper that forgets it
;;; fails loudly here instead of silently building a Pi image.
(declaim (special *cl-repl-platform*))
(unless (member *cl-repl-platform* '(:rpi :virt :x64))
  (error "build-cl-repl-common: *CL-REPL-PLATFORM* is ~S; want :RPI, :VIRT or :X64."
         *cl-repl-platform*))
(defvar *cl-repl-rpi-p*  (eq *cl-repl-platform* :rpi))
(defvar *cl-repl-virt-p* (eq *cl-repl-platform* :virt))
;; :X64 — the bare-metal x86-64 QEMU-pc image (mvm/build-x64-cl-repl.lisp).
;; Same thin-head contract as :VIRT; the x86 arms are marked at each
;; DIVERGENCE site below.  *CL-REPL-VIRT-P* keeps its aarch64-only meaning.
(defvar *cl-repl-x64-p*  (eq *cl-repl-platform* :x64))
(defvar *cl-repl-qemu-p* (or *cl-repl-virt-p* *cl-repl-x64-p*)
  "T for the two QEMU machines (virt, pc): E1000 over PCI, user-mode net.")

;;; MVM infrastructure, loaded HERE rather than only by build-cli-common: the
;;; board's USB/PCI net stack is READ into the *CLI-BARE-METAL-NET-SOURCE* slot
;;; below, and a slot must be bound BEFORE build-cli-common is loaded.  That
;;; file's own load is guarded on (find-package "MODUS.MVM"), so it loads once.
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

(format t "~%=== Building bare-metal ~A CL REPL image ===~%"
        (cond (*cl-repl-virt-p* "AArch64 QEMU-virt")
              (*cl-repl-x64-p*  "x86-64 QEMU-pc")
              (t "RPi 3B")))

;;; Source readers.  Deliberately %RPI--prefixed rather than reusing
;;; build-cli-common's READ-FILE-TEXT / MVM-TEXT: those do not exist yet (this
;;; file runs first) and defining them under the same names would have them
;;; redefined out from under us when that file loads.  (The names are host-side
;;; only — nothing they are called from reaches the image — so they kept their
;;; original spelling through the :rpi/:virt split rather than churn every call
;;; site for cosmetics.)
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

(defvar *cli-arch* (if *cl-repl-x64-p* :x64 :aarch64))

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
;;;
;;; DIVERGENCE 1 — :VIRT BUILDS WITH THE JIT OFF, and the reason is an ADDRESS,
;;; not a policy.  translate-aarch64.lisp's bare-metal trap #x0531 HARDCODES the
;;; exec-page region [0x14000000, 0x18000000) and its bump word at 0x13FFFFF0.
;;; On the Pi those are plain identity-mapped DRAM.  Under boot-aarch64.lisp's
;;; fixpoint page tables they are NOT: L2[128..511] map VA 0x10000000-0x3FFFFFFF
;;; as IDENTITY DEVICE memory (the QEMU virt 32-bit PCI MMIO window), so every
;;; JIT page would be written into unbacked MMIO — stores dropped, reads 0, and
;;; the first %jit-call branches into a page of 0x00000000 (UDF).
;;;
;;; RESOLVED 2026-08-30.  translate-aarch64.lisp now takes the region from
;;; (%jit-exec-bump) / (%jit-exec-lo) / (%jit-exec-hi) — DEFUNS, because that
;;; file is baked into the image and a defvar initform would read NIL in-image
;;; (Limitation #7, the #226/#282 shape).  :virt overrides them below with a
;;; real DRAM window, so :virt now builds JIT-ON like the Pi.
;;;
;;; FLIPPED ON, THEN REVERTED 2026-08-30 — the parameterisation below is right
;;; and stays, but turning :virt JIT-ON REGRESSED the image and the region is
;;; not the whole story.  Measured, same tree, only *jit-on* differing:
;;;   JIT OFF: 0 read errors, install completes, cabinet-mount succeeds.
;;;   JIT ON : 6 read errors across SIX files (genera-compat, pagetree/node,
;;;            pagetree/btree, cabinet/packages, cabinet/codec, cabinet/fs)
;;;            with READER-ERROR *and* TYPE-ERROR, a flood of PROGRAM-ERRORs,
;;;            and the run never reaches E1000:OK.
;;; Note the conditions DIFFER from the FP bug's uniform PROGRAM-ERROR, and the
;;; damage is spread across unrelated files — this is not the same defect.
;;; THE REGION IS NOT THE SUSPECT — checked, don't re-chase it.  boot-aarch64
;;; fills L2[0..255] with `(0x40000000 + i*0x200000) | 0x701`: bit0 valid,
;;; SH=inner-shareable, AF set, and NEITHER PXN (53) NOR UXN (54) — so the
;;; window is executable.  The read-only pass ORs AP[2] into L2[0..20] only,
;;; i.e. VA 0..0x02A00000; 0x04000000 is L2[32], well clear of it.  So the
;;; chosen window is writable AND executable, as intended.
;;; That points at the aa64 JIT itself miscompiling on bare metal rather than
;;; at the memory map — consistent with the spread of READER/TYPE errors across
;;; unrelated files.  Next step is a differential: JIT-translate a handful of
;;; forms in-image and compare against mvm-interpret, the way the x64 jitdiff
;;; harness did, rather than another whole-image boot.
;;; Keeping :virt interpret-only for now: correct, just slower.  The Pi/JIT
;;; parity gap this leaves is REAL and still owed before a hardware run.
;;;
;;; ROOT-CAUSED 2026-08-31 — it was NOT "the aa64 JIT miscompiling".  Not ONE
;;; form ever reached native code; the census on a full JIT-ON virt boot read
;;; *jit-native-count* = 0, *jit-fallback-count* = 263, and every R-* reason
;;; counter 0.  The 263 were all *jit-cv-reject* = 261 (+2 non-const forms):
;;; %JIT-CONSTVEC-COVERS-P rejecting every page, because mvm-eval.lisp published
;;; the JIT constant vector at x64's root #x10000F00 while the aarch64 back-end
;;; reads and roots #x10000F10.  Unrooted, the word went stale at the first
;;; collection — and %JIT-SYNC-CONSTVEC kept ASETting into that recycled
;;; address, which is exactly the "READER-ERROR and TYPE-ERROR spread across six
;;; unrelated files" above: wild heap writes from a JIT that never ran a single
;;; instruction.  Two fixes: the per-target root (mvm-eval.lisp
;;; %JIT-CONSTVEC-ROOT) and the HOST-side exec window (further down this file,
;;; next to the fixpoint-nil-value block) — the image-side %jit-exec-* override
;;; alone left the BAKED %mmap-exec-page pointing at the Pi window, which on
;;; virt is unbacked PCI MMIO.
;;;
;;; MODUS_VIRT_JIT=0 forces :virt back to interpret-only for triage/bisect;
;;; MODUS_VIRT_JIT=1 is the default and matches :rpi.
(defvar *jit-on*
  (cond
    ;; :X64 bare metal: JIT OFF by default (translate-x64's exec pages come
    ;; from mmap(2) in linux mode; the bare-metal bump allocator the aarch64
    ;; arm has is not ported yet).  MODUS_X64_JIT=1 opts in for the port work.
    (*cl-repl-x64-p*
     (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_X64_JIT")))
       (and v (string= v "1"))))
    ;; :RPI — JIT on by default; MODUS_RPI_JIT=0 builds it OFF.  The knob
    ;; exists because the aarch64 JIT early-binds `#'NAME' (compile-function-ref
    ;; -> :li-func, resolved once when the page is built), so a name CLOS later
    ;; redefines -- every generic function -- leaves the page holding a stale
    ;; object.  That is what blocks ql:quickload: QL-DIST::ENABLED-DISTS calls
    ;; (funcall #'enabledp d) and gets the pre-CLOS gf stub.  Measured on a
    ;; native Pi 5 2026-09-04: JIT ON dies in FIND-SYSTEM; JIT OFF completes
    ;; `ql:quickload :alexandria' in 613 s with flatten/curry correct.
    ((not *cl-repl-virt-p*)
     (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_JIT")))
       (not (and v (string= v "0")))))
    (t (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_VIRT_JIT")))
         (not (and v (string= v "0")))))))

;;; MODUS_YIELD_NOP=1 — translate the YIELD opcode as NOP instead of SEV+WFE.
;;; YIELD sits at EVERY compiled loop back-edge, including mvm-interpret's own
;;; dispatch loop, so on aa64 every interpreted bytecode op pays one SEV+WFE.
;;; x64 bare metal uses a cheap flag-check and x64 Linux a NOP for the same
;;; opcode; QEMU raspi3b already runs NOP (*aarch64-yield-nop* docstring).
;;; A/B measurement knob first; if the WFE is the measured wall, the non-actor
;;; CL images should default to NOP.
(let ((v #+sbcl (sb-ext:posix-getenv "MODUS_YIELD_NOP")))
  (when (and v (string= v "1"))
    (setq modus.mvm::*aarch64-yield-nop* t)))

;;; :VIRT JIT exec window.  From the build's own memory map: image ends
;;; 0x03933480 and the GC bitmaps start 0x05000000, so [0x04000000,0x05000000)
;;; is 16 MB of unused Normal-WB DRAM inside the VA 0-0FFFFFFF window, clear of
;;; the stack (grows down from 0x08000000) and the heap (0x09000000+).  The bump
;;; word sits just below the region.  Bare-metal DRAM is not zeroed, so the
;;; allocator's own range check re-initialises a garbage bump pointer to base —
;;; see the #x0531 arm.
(defvar *cl-repl-jit-region-source*
  (if *cl-repl-virt-p*
      "
(defun %jit-exec-bump () 67108848)
(defun %jit-exec-lo   () 67108864)
(defun %jit-exec-hi   () 83886080)
"
      ""))

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
(defvar *cli-arch-syscall-source*
  (if *cl-repl-x64-p*
      "
;; BARE METAL x86-64: HLT in a loop — wakes on any IRQ and halts again.
(defun halt ()
  (loop (hlt)))
(defun sys-exit (code)
  (let ((c code)) c)
  (halt))
"
      "
;; BARE METAL: no process model.  WFI in a loop (TRAP #x0304 = WFI on
;; AArch64) wakes on any IRQ and immediately WFIs again — effectively idle.
;; The x64 sibling uses (loop (hlt)); this is the same shape on ARM.
(defun halt ()
  (loop (trap #x0304)))
(defun sys-exit (code)
  (let ((c code)) c)
  (halt))
"))

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
;;; SAVE-AND-DIE on the Pi (lib/save-image.lisp, docs/save-and-die.md).  There
;;; is no file: the core is the RAM range at +core-addr+ = 0x18000000 (free
;;; DRAM above the JIT region, below the 448 MiB the board has), placed there
;;; by the boot loader (U-Boot `tftpboot 0x18000000 ql.core', QEMU
;;; `-device loader,file=…,addr=0x18000000') and detected by its magic word.
;;; The shared code's `fd' is the address of a cursor word here; read/write
;;; are 8-byte word copies (no allocation, exact bits — a u64 load/store pair
;;; round-trips a tagged pointer), padded to 8 so every section starts
;;; aligned.  The Pi's arena is the JIT region [%jit-exec-lo, bump) with the
;;; bump word at %jit-exec-bump; the bitmaps are at their own fixed addresses
;;; and travel as slices.  Saving ends with CORE-END=<addr> on serial so a
;;; QEMU harness can dump exactly [0x18000000, addr) through the gdbstub.
(defvar *cl-repl-rpi-core-source* "
(defun %core-addr () #x18000000)
(defun %core-cursor-slot () #x10000F60)
(defun %core-requested-p ()
  ;; The magic is a HEADER FIELD written with (setf (mem-ref .. :u64) magic),
  ;; i.e. deposited as magic<<1, so read it the same halving way -- NOT the
  ;; bit-exact %gc-read64 the raw cursor uses.
  (= (mem-ref (%core-addr) :u64) (%core-magic)))
(defun %core-open-in ()
  (%gc-write64 (%core-cursor-slot) (%core-addr))
  (%core-cursor-slot))
(defun %core-open-out (path)
  (%gc-write64 (%core-cursor-slot) (%core-addr))
  (%core-cursor-slot))
(defun %core-close (fd)
  (write-string-serial \"CORE-END=\")
  (print-dec (%gc-read64 fd))
  (write-char-serial 10)
  0)
(defun %core-copy-words (dst src nbytes)
  ;; Bit-EXACT 64-bit copy, NBYTES rounded up to 8.  Two :u32 halves, NOT one
  ;; :u64: a :u64 load returns raw>>1 and a :u64 store writes value<<1, so a
  ;; tagged pointer (odd low bits) loses its low bit and the whole heap
  ;; corrupts.  A :u32 load's <<1 and a :u32 store's >>1 cancel, so the half
  ;; round-trips exactly, and neither half ever materialises a >=2^62 word (no
  ;; bignum, no allocation mid-restore).  The fd cursor is a RAW address word,
  ;; so read it with %gc-read64, not mem-ref.
  (let ((i 0) (padded (logand (+ nbytes 7) -8)))
    (loop
      (when (>= i padded) (return padded))
      (setf (mem-ref (+ dst i) :u32)     (mem-ref (+ src i) :u32))
      (setf (mem-ref (+ dst i 4) :u32)   (mem-ref (+ src i 4) :u32))
      (setq i (+ i 8)))))
(defun %core-read-all (fd addr len)
  (let ((src (%gc-read64 fd)) (padded (logand (+ len 7) -8)))
    (%core-copy-words addr src len)
    (%gc-write64 fd (+ src padded))
    len))
(defun %core-write-all (fd addr len)
  (let ((dst (%gc-read64 fd)) (padded (logand (+ len 7) -8)))
    (%core-copy-words dst addr len)
    (%gc-write64 fd (+ dst padded))
    len))
(defun %core-open-path-at (addr) -1)
;; CARRY THE JIT ARENA.  The old comment here claimed the bare-aa64 JIT
;; translates nothing so no exec pages exist -- that is FALSE on this image:
;; a client load drove the exec bump to ~0x140D0000, and a heap function whose
;; code pointer landed in that region became a DANGLING raw fn-pointer once the
;; core (which did not carry the arena) was restored -- `blr' into a zeroed
;; page, the silent !!FAULT that stopped the alexandria install at a
;; funcall-of-a-loader-closure.  So the Pi carries [%jit-exec-lo, bump) exactly
;; as hosted carries its arena; the shared save/restore does the rest (writes
;; the arena, restores it, sets the bump word, IC-invalidates).  The
;; conservative-root bitmaps stay separate (fixed 0x05000000/0x05100000) and
;; still travel as slices.  bump is RANGE-VALIDATED: an uninitialised or
;; garbage bump word reports 0 (no arena) rather than a runaway length.
;; When the image is built MODUS_RPI_JIT=0 the exec-region accessors
;; (%jit-exec-bump / -lo / -hi) are UNRESOLVED and return the NIL sentinel, so
;; every one is guarded with integerp: a non-integer means no JIT and therefore
;; no arena (report 0), NOT `%gc-read64 NIL' -> mem-ref of NIL's machine word
;; (0xDEAD0001>>1 = 0x6F568000) -> data abort inside %save-image.  A
;; JIT-enabled image returns real integers and carries [lo,bump) as before.
(defun %core-jit-arena-lo () (let ((l (%jit-exec-lo))) (if (integerp l) l 0)))
(defun %core-jit-bump-slot () (%jit-exec-bump))
(defun %core-jit-arena-bump ()
  ;; JIT off: no region, no arena -> 0.  JIT on: the exec REGION always exists
  ;; (plain executable DRAM) but the bump WORD is only set by the trap's first
  ;; %mmap-exec-page, so report the region base for an uninitialised / out-of-
  ;; range bump (a fresh restore then passes the shared guard, which dies on 0,
  ;; and a pre-JIT save writes an EMPTY arena, never a runaway length); a valid
  ;; in-range bump means real JIT pages -> carry [lo,bump).
  (let ((slot (%jit-exec-bump)) (lo (%jit-exec-lo)) (hi (%jit-exec-hi)))
    (if (and (integerp slot) (integerp lo) (integerp hi))
        (let ((b (%gc-read64 slot)))
          (if (and (integerp b) (>= b lo) (< b hi)) b lo))
        0)))
(defun %core-jit-lossy-p () nil)
(defun %core-die (msg)
  (write-string-serial msg) (write-char-serial 10) (halt))
(defun %core-post-restore () nil)
")

(defvar *cli-arch-override-source*
  (if *cl-repl-x64-p*
      ;; x86 QEMU-pc: no device tree; nothing to read MODUS_* knobs from yet.
      "
(defun %cli-getenv (name) nil)
"
      (concatenate 'string
        (string #\Newline)
        (%rpi-mvm-text "lib/fdt.lisp")
        "
(defun %cli-getenv (name) (%bootargs-lookup (%fdt-bootargs) name))
;;; THE CLOCK ON BARE METAL.  ansi-bridge.lisp's GET-INTERNAL-REAL-TIME and
;;; GET-UNIVERSAL-TIME wrap a Linux syscall in a handler-case, expecting the
;;; bare-metal case to fall through to a counter.  It cannot: an SVC with no
;;; OS is a synchronous exception, and on a Pi that is the silent `!!FAULT'
;;; spin (ESR 0x56000000 = EC 0x15, SVC from AArch64) -- the fault was
;;; measured at GET-INTERNAL-REAL-TIME+0xbc inside a restored quicklisp core.
;;; Nothing on the board may reach syscall3; route both to the CNTVCT
;;; counter, exactly as %timer-universal-time's docstring says a bare-metal
;;; build must select explicitly.  MICROSECONDS: the boot init below sets
;;; internal-time-units-per-second to 1000000 (the hosted definition), and
;;; this used to return milliseconds against it — every bare-metal timing
;;; read 1000× too small (a 20 s reel decode on the Zero 2 W reported as
;;; 20 ms, 2026-09-10).  Split the division so the product never leaves
;;; the fixnum range: CNTVCT at 19.2/54 MHz × 10^6 overflows 62 bits in days.
(defun get-internal-real-time ()
  (let ((hz (cntfrq)))
    (if (and (integerp hz) (> hz 0))
        (let ((ticks (rdtsc)))
          (+ (* (floor ticks hz) 1000000)
             (floor (* (mod ticks hz) 1000000) hz)))
        0)))
(defun get-universal-time () (%timer-universal-time))
"
        (if *cl-repl-rpi-p* *cl-repl-rpi-core-source* ""))))





;;; DIVERGENCE 2 — WHO ZEROES THE BSS-EQUIVALENT SLOTS.
;;;
;;; The Pi's boot/boot-rpi-cl.lisp step 0 bulk-zeroes 0x10000000..0x10001000
;;; plus 0x10010000 before a single Lisp instruction runs (see the :rpi branch
;;; below for why that is the only legal place for the BULK form).
;;; boot/boot-aarch64.lisp's fixpoint entry does not: it builds page tables,
;;; brings up the UART and jumps.  So on :virt kernel-main has to do it, and it
;;; has to do it BEFORE the banner, not after — write-string-serial resolves a
;;; global, which walks the alist head at 0x10000080, so a banner printed over
;;; garbage is a fault before any diagnostic exists to report it.  (QEMU zero-
;;; fills DRAM, which is exactly why this must be written down rather than
;;; relied upon: it is invisible under emulation and fatal on silicon.)
;;;
;;; The list is an ENUMERATION and therefore has the weakness the :rpi comment
;;; names — a newly added metadata word will not be here.  It is the same list
;;; mvm/build-aarch64-ansi.lisp's kernel-main uses, which is the authority for
;;; what QEMU virt needs, plus 0x10000F00 (the DTB pointer slot lib/fdt.lisp
;;; reads; nothing on virt writes it, and %FDT-PLAUSIBLE-P must see a
;;; deterministic 0 rather than whatever the firmware left).
;;;
;;; NOT ZEROED, deliberately: 0x10000160/168, the code_base/code_end pair
;;; emit-aarch64-code-bounds-init wrote during boot.  FUNCTIONP's range arm
;;; depends on them.
(defvar *cl-repl-virt-kernel-prologue* "
  ;; BSS-EQUIVALENT INIT, first thing: see the build script's comment.
  (setf (mem-ref #x10000080 :u64) 0)
  (setf (mem-ref #x10000088 :u64) 0)
  (setf (mem-ref #x10000090 :u64) 0)
  (setf (mem-ref #x10000098 :u64) 0)
  (setf (mem-ref #x10000148 :u64) 0)
  (setf (mem-ref #x10000150 :u64) 0)
  (setf (mem-ref #x10000158 :u64) 0)
  (setf (mem-ref #x10000170 :u64) 0)
  (setf (mem-ref #x10000180 :u64) 0)
  (setf (mem-ref #x10000188 :u64) 0)
  (setf (mem-ref #x10000190 :u64) 0)
  (setf (mem-ref #x10000198 :u64) 0)
  (setf (mem-ref #x100001A0 :u64) 0)
  (setf (mem-ref #x100001A8 :u64) 0)
  (setf (mem-ref #x100001B0 :u64) 0)
  (setf (mem-ref #x100001B8 :u64) 0)
  (setf (mem-ref #x100001C0 :u64) 0)
  (setf (mem-ref #x100001C8 :u64) 0)
  (setf (mem-ref #x100001D0 :u64) 0)
  (setf (mem-ref #x10000C10 :u64) 0)
  (setf (mem-ref #x10000C18 :u64) 0)
  (setf (mem-ref #x10000C20 :u64) 0)
  (setf (mem-ref #x10000C30 :u64) 0)
  (setf (mem-ref #x10000C38 :u64) 0)
  (setf (mem-ref #x10000C40 :u64) 0)
  (setf (mem-ref #x10000C48 :u64) 0)
  (setf (mem-ref #x10000C50 :u64) 0)
  (setf (mem-ref #x10000C58 :u64) 0)
  (setf (mem-ref #x10000C70 :u64) 0)
  (setf (mem-ref #x10000C80 :u64) 0)
  (setf (mem-ref #x10000CD0 :u64) 0)
  (setf (mem-ref #x10000CD8 :u64) 0)
  (setf (mem-ref #x10000D40 :u64) 0)
  (setf (mem-ref #x10000D48 :u64) 0)
  (setf (mem-ref #x10000D50 :u64) 0)
  (setf (mem-ref #x10000D58 :u64) 0)
  (setf (mem-ref #x10000DA0 :u64) 0)
  (setf (mem-ref #x10000F00 :u64) 0)
  (setf (mem-ref #x10000F10 :u64) 0)
  ;; #286 GC pause-statistics block (start/total/max/last/bytes/lastb).  The
  ;; native collector ACCUMULATES into F28/F30/F40, so garbage here is not a
  ;; wrong first reading, it is a permanently wrong reading.  The Pi gets these
  ;; from boot-rpi-cl.lisp's bulk zero of 0x10000000..0x10001000; QEMU virt has
  ;; no bulk form, hence these six lines.  See mvm/gc.lisp's slot map.
  (setf (mem-ref #x10000F20 :u64) 0)
  (setf (mem-ref #x10000F28 :u64) 0)
  (setf (mem-ref #x10000F30 :u64) 0)
  (setf (mem-ref #x10000F38 :u64) 0)
  (setf (mem-ref #x10000F40 :u64) 0)
  (setf (mem-ref #x10000F48 :u64) 0)
  (setf (mem-ref #x10000F50 :u64) 0)
  ;; Handler-stack depth (AArch64 helpers) -- 0x10010000, not x64's 0x10000400.
  (setf (mem-ref #x10010000 :u64) 0)

  ;; Banner: proves native code is executing and the UART is alive before any
  ;; runtime init runs.  cross.lisp bound the serial base from the boot
  ;; descriptor (+tdk-uart-va+ = VA 0x20000000 -> PA 0x09000000, the QEMU virt
  ;; PL011), so this is the first thing that exercises the page tables.
  (write-string-serial \"MODUS-CL\")
  (write-char-serial 10)

  ;; GC METADATA -- must precede the first allocation.  Identical geometry to
  ;; the Pi image: 112 MB split into two 56-MB semispaces, from-start
  ;; 0x09000000, to-start 0x0C800000, stack scan base = the boot SP
  ;; (+tdk-stack-va+ = 0x08000000).  boot-aarch64.lisp sets x24/x25 from
  ;; +tdk-cons-base-va+ / +tdk-cons-limit-va+, which are those same values.
  (%gc-init #x09000000 #x07000000 #x08000000)

  ;; #160 OBJECT-START + CONS-KIND BITMAPS -- see the :rpi branch for the full
  ;; rationale.  Same addresses: VA 0x05000000 / 0x05100000 are L2[40]/L2[40]
  ;; here, i.e. normal cacheable DRAM at PA 0x45000000, clear of both the image
  ;; and the stack.  Asserted at build time below.
  (%rpi-gc-bitmap-init #x05000000 #x05100000 #xE0000)

  ;; NO (setup-irq) / (nic-irq-unmask) here.  The ANSI gate runner needs the
  ;; GICv2 + vtimer for its per-test deadline; a REPL polls the UART and wants
  ;; no interrupts at all, so *AARCH64-SETUP-IRQ-ENABLE* stays NIL below.

")

;;; ARCH SLOT: hardware setup that must precede the FIRST allocation.
(defvar *cl-repl-x64-kernel-prologue* "
  (setf (mem-ref #x10000080 :u64) 0)   ; global variable table head
  (setf (mem-ref #x10000088 :u64) 0)   ; symbol intern table
  (setf (mem-ref #x10000090 :u64) 0)   ; MV count
  (setf (mem-ref #x10000098 :u64) 0)   ; MV values
  (setf (mem-ref #x10000C30 :u64) 0)   ; fault diag slots
  (setf (mem-ref #x10000C38 :u64) 0)
  (setf (mem-ref #x10000C40 :u64) 0)
  (setf (mem-ref #x10000C48 :u64) 0)
  (setf (mem-ref #x10000C50 :u64) 0)
  (setf (mem-ref #x10000C58 :u64) 0)
  (setf (mem-ref #x10000DA0 :u64) 0)   ; safepoint boundary
  (write-string-serial \"MODUS-CL\")
  (write-char-serial 10)
")

;;; :X64 arm of DIVERGENCE 2 — boot-x64.lisp's stub writes the MCGC config page
;;; itself (mcgc-store #x10000E00 ...) before kernel-main; only the runtime's
;;; own BSS-equivalent slots need zeroing (the list the retired standalone
;;; build-x64-cl-repl.lisp driver zeroed).
(defvar *cli-arch-kernel-prologue*
 (if *cl-repl-x64-p*
  *cl-repl-x64-kernel-prologue*
  (if *cl-repl-virt-p*
      *cl-repl-virt-kernel-prologue*
      "
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

")))

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
(defvar *cli-arch-io-scratch-source*
  (if *cl-repl-x64-p*
      "  (setq *cstr-scratch* #x0FE00000)
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
  (setq *serial-repl-buf* nil)
  (setq *serial-repl-len* 0)
  (setq *serial-repl-cap* 0)
"
      "  (setq *cstr-scratch* #x0FE00000)
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
"))

;;; ============================================================
;;; NET BUILD (MODUS_NET_BUILD=1) — #209 rung 2: HTTP over the board's NIC
;;; ============================================================
;;;
;;; When enabled, append the board's net stack (arch adapter + NIC driver +
;;; IP/TCP/ARP/DHCP + HTTP client) to the image and drive a DHCP -> TCP ->
;;; HTTP GET pipeline from kernel-main.
;;;
;;; DIVERGENCE 3/4 — the NIC and its bring-up, and NOTHING else:
;;;   :rpi   arch-rpi-cl + dwc2 + usb + cdc-ether + r8152,   (cdc-ether-init)
;;;   :virt  arch-aarch64-cl + e1000,                        (pci-assign-bars
;;;                                                           + e1000-probe)
;;; net/ip.lisp, net/http-client.lisp, the response-cap / tcp-rx-copy /
;;; http-fetch-impl overrides and the whole fetch-install-call driver are
;;; SHARED.  This is the DWC2/E1000 pair build-aarch64-ansi.lisp's own
;;; MODUS_NET_BUILD used to fork; the driver text below is deliberately shaped
;;; like that one so the three stay diffable.
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
;;
;; DIVERGENCE 5 — :RPI ONLY.  Every address in *SSH-ADDR-MAP-SOURCE* below is a
;; Pi DRAM fact (0x12000000-0x16000000, chosen against boot-rpi-cl's page table
;; and the Pi Zero 2 W's 496 MB ceiling).  Under the fixpoint MMU those VAs are
;; identity-mapped PCI DEVICE memory, so the same map would silently put the
;; actor stacks and the crypto scratch in MMIO.  A :virt SSH image needs its own
;; map derived from the QEMU virt layout; until someone writes one, refuse the
;; flag loudly rather than build something that cannot work.
(defvar *ssh-build-p*
  (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_SSH_BUILD")))
    (and v (string= v "1"))))
(when (and *ssh-build-p* *cl-repl-qemu-p*)
  (error "MODUS_SSH_BUILD=1 is :RPI-only — the actor/SSH address map is Pi ~
          DRAM.  See the DIVERGENCE 5 comment in build-cl-repl-common.lisp."))

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
;; path (sidesteps the actor context-switch), the capture-aware %serial-byte SSH
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
        ;; SHELL-path fix (completes the interactive SSH loop): the active
        ;; ssh-do-eval-expr (net/aarch64-overrides.lisp) used eval-sexp (the
        ;; DELETED tree-walker) + buf-read-list (the legacy repl-source reader),
        ;; so interactive SSH input evaluated to NOTHING (only the prompt came
        ;; back).  Loaded LAST => wins last-defun-wins.  The edited line is the
        ;; raw bytes at ssh-ipc-base+0x28, length edit-line-len; route through the
        ;; SAME CL stack as exec (ssh-eval-line: read-from-string -> eval ->
        ;; prin1-to-string -> channel data).
        "(defun ssh-do-eval-expr (ssh)
    (let ((len (edit-line-len)))
      (when (> len 0)
        (let ((cmd (make-array len)))
          (dotimes (i len)
            (aset cmd i (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8)))
          (ssh-eval-line ssh cmd len)))))"
        (string #\Newline)
        ;; INTERACTIVE-shell fix: the stock ssh-handle-channel-data drives bytes
        ;; through the aarch64-overrides line editor (handle-edit-byte + edit
        ;; state) which does not integrate with this image, so typed/piped input
        ;; never reached eval (only the prompt came back; exec worked because it
        ;; bypasses the editor).  Override it (loaded after ssh.lisp) to
        ;; accumulate raw channel bytes in ssh-ipc scratch (0x60500 count /
        ;; 0x60510 buf, both zeroed by ssh-boot, above ssh.lisp's <=0x60450) and,
        ;; on CR/LF, eval the line through the SAME proven CL path as exec
        ;; (ssh-eval-line) and reprompt.  EXEC is untouched (it never calls this).
        "(defun ssh-handle-channel-data (ssh payload plen)
    (let ((data-len (ssh-get-u32 payload 5))
          (naddr (+ (ssh-ipc-base) #x60500))
          (baddr (+ (ssh-ipc-base) #x60510)))
      (let ((i 0))
        (loop
          (when (>= i data-len) (return nil))
          (let ((b (aref payload (+ 9 i))))
            (if (or (eq b 10) (eq b 13))
                (let ((n (mem-ref naddr :u32)))
                  (when (> n 0)
                    (let ((cmd (make-array n)))
                      (dotimes (k n) (aset cmd k (mem-ref (+ baddr k) :u8)))
                      (ssh-eval-line ssh cmd n)))
                  (setf (mem-ref naddr :u32) 0)
                  (ssh-send-prompt ssh))
                (let ((n (mem-ref naddr :u32)))
                  (when (< n 4000)
                    (setf (mem-ref (+ baddr n) :u8) b)
                    (setf (mem-ref naddr :u32) (+ n 1))))))
          (setq i (+ i 1))))))"
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
  (net-usb-probe)
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
        (string #\Newline)
        ;; usb-netdev.lisp LAST: runtime USB NIC binding + hot-plug.  Its
        ;; e1000-send/receive/rx-buf DISPATCHERS win over r8152's forwarders
        ;; (last-defun-wins), net-usb-probe (called by ssh-boot above) picks the
        ;; driver at runtime (RTL8153 -> r8152, else CDC-ECM) and latches it, and
        ;; the real usb-netdev-hotplug-poll overrides ip.lisp's no-op stub so the
        ;; net-actor-main loop re-binds on a USB connect/disconnect edge.
        (%rpi-net-text "usb-netdev.lisp")         (string #\Newline))
      ""))

;;; DIVERGENCE 3 — the arch adapter + the NIC driver.  Everything downstream of
;;; this block (ip.lisp, http-client.lisp, the overrides, the driver) is shared.
;;;
;;; BOTH adapters are the "-cl" flavour, and that suffix is load-bearing.  The
;;; legacy adapters — net/arch-raspi3b.lisp and net/arch-aarch64.lisp — each
;;; carry a MINIATURE RUNTIME (make-array / aref / aset / array-length /
;;; numberp / try-alloc-obj / tag-as-object) because in the `repl-source' images
;;; those ARE the runtime.  Here the full CL runtime is already in the image and
;;; the net stack is concatenated AFTER it, so under last-defun-wins those
;;; legacy definitions would REPLACE the real ones with an incompatible object
;;; layout (element-count << 15, byte-packed payload) and even (+ 1 2) would
;;; fail.  Their DMA/state bases are wrong here too: arch-raspi3b's 0x01000000
;;; block and arch-aarch64's 0x41000000 block both land INSIDE this ~57 MB
;;; kernel image, so the NIC would DMA over native code.  See each -cl file's
;;; header.
(defvar *net-nic-source*
  (if (not *net-build-p*)
      ""
      (if *cl-repl-x64-p*
          (concatenate 'string
            (%rpi-net-text "arch-x86-cl.lisp")     (string #\Newline)
            (%rpi-net-text "e1000.lisp")           (string #\Newline))
      (if *cl-repl-virt-p*
          (concatenate 'string
            ;; QEMU virt: PCI ECAM + E1000.  arch-aarch64-cl.lisp also carries
            ;; the fixpoint-MMU BAR assignment (VA 0x11000000, NOT 0x10000000 —
            ;; that VA is remapped to the runtime metadata DRAM) and relocates
            ;; every DMA/state region to PA-identity 0x502xxxxx.
            (%rpi-net-text "arch-aarch64-cl.lisp") (string #\Newline)
            (%rpi-net-text "e1000.lisp")           (string #\Newline)
            ;; NO RX-RING-DEPTH OVERRIDE.  e1000.lisp's portable default of 128
            ;; is what this platform's DMA map can hold: at 256 the rx buffers
            ;; span 50201000..50281000 and bury e1000-state-base (50260000),
            ;; fe-scratch-base (50260900) and ssh-conn-base (50280000), so the
            ;; NIC DMA's frames over the TCP state block partway through any
            ;; transfer long enough to reach descriptor 190.  See the LAYOUT
            ;; block in net/arch-aarch64-cl.lisp for what has to move first.
            ;;
            ;; If a depth override is ever reinstated it MUST be spliced here,
            ;; after e1000.lisp: arch files are spliced BEFORE the driver, so a
            ;; defun there loses to the driver's default under last-defun-wins
            ;; (Limitation 1).
            )
          (concatenate 'string
            (%rpi-net-text "arch-rpi-cl.lisp")   (string #\Newline)
            (%rpi-net-text "dwc2.lisp")          (string #\Newline)
            (%rpi-net-text "usb.lisp")           (string #\Newline)
            (%rpi-net-text "cdc-ether.lisp")     (string #\Newline)
            ;; r8152.lisp AFTER cdc-ether: its e1000-probe/send/receive/rx-buf
            ;; defuns override the CDC-ECM ones (last-defun-wins) — on real
            ;; RTL8153 silicon the ECM config NAKs all bulk-IN, so the NIC is
            ;; driven in vendor config 1 with an explicit RX enable.
            (%rpi-net-text "r8152.lisp")         (string #\Newline)
            ;; hdmi-fb.lisp: modus's DISPLAY PATH — VideoCore HDMI framebuffer +
            ;; the glass blit seam.  Independent of the NIC (self-contained
            ;; hdmi-* defuns over mem-ref + the property mailbox at 0x3F00B880);
            ;; spliced here because board builds are always net builds.
            (%rpi-net-text "hdmi-fb.lisp")       (string #\Newline))))))

(defvar *net-source*
  (if *net-build-p*
      (concatenate 'string
        ;; The arch adapter + NIC driver (DIVERGENCE 3, above).
        *net-nic-source*
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
        ;; build-aarch64-ansi.lisp's net build uses, so the two images fetch
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
              (%serial-byte 68) (%serial-byte 78) (%serial-byte 83)
              (%serial-byte 58) (%serial-byte 48) (%serial-byte 10)
              (return 0))
            (when (zerop (tcp-connect ip port))
              (%serial-byte 84) (%serial-byte 67) (%serial-byte 80)
              (%serial-byte 58) (%serial-byte 70) (%serial-byte 10)
              (return 0))
            (let ((req-buf (make-array 512)))
              (let ((req-len (http-build-get url scheme-end host-len
                                              path-start path-len req-buf)))
                (tcp-send req-buf req-len)))
            ;; IDLE BOUND IS A DURATION, NOT AN ITERATION COUNT.  This loop used
            ;; to stop after 20 consecutive empty polls.  That is not a timeout:
            ;; how much real time 20 polls span depends entirely on how fast the
            ;; loop runs.  Interpreted, they spanned long enough that the
            ;; server's next packet always arrived inside the window.  Once the
            ;; aarch64 JIT began compiling this loop (runtime-DEFUN native fix),
            ;; the same 20 polls elapsed almost instantly and the fetch returned
            ;; a TRUNCATED body mid-transfer -- 97723 then 31483 bytes of a
            ;; 348160-byte tar, the varying length tracking wherever the
            ;; server's pacing happened to leave a gap.  Downstream that read as
            ;; `setup.lisp bytes=0' with no error, because a short read is not
            ;; an error anywhere in this path.
            ;;
            ;; The speedup did not break this; it revealed it.  Any wait
            ;; expressed in iterations is a latent bug on a machine that might
            ;; get faster.  %timer-universal-time is the CNTVCT-based clock
            ;; (seconds), so the bound now costs at most +NET-IDLE-SECS+ of real
            ;; time and is speed-independent.  tcp-state going to 0 remains the
            ;; PRIMARY termination; this is only the safety net for a peer that
            ;; never closes.
            (let ((resp (make-array (%net-resp-cap)))
                  (resp-len 0)
                  (done 0)
                  (last-rx (%timer-universal-time)))
              (loop
                (when (not (zerop done)) (return 0))
                (let ((n (tcp-receive 300)))
                  (if (> n 0)
                      (progn
                        (let ((copied (tcp-rx-copy resp resp-len)))
                          (setq resp-len (+ resp-len copied)))
                        (setq last-rx (%timer-universal-time)))
                      nil))
                (when (zerop (tcp-state)) (setq done 1))
                (when (> (- (%timer-universal-time) last-rx) 30)
                  (setq done 2)))
              ;; SAY WHICH CONDITION ENDED THE TRANSFER.  A short read is not an
              ;; error anywhere in this path, so a truncated body is otherwise
              ;; indistinguishable from a complete one -- that is how 97723
              ;; bytes of a 348160-byte tar reached the untar loop and presented
              ;; as `setup.lisp bytes=0' five steps later.  done=1 means the peer
              ;; closed (a genuine end); done=2 means WE gave up while the peer
              ;; still had the connection open, i.e. the body is TRUNCATED.
              (write-string-serial \"RXEND why=\") (print-dec done)
              (write-string-serial \" len=\") (print-dec resp-len)
              (write-char-serial 10)
              (tcp-close)
              (cons resp resp-len))))))))
"
        (string #\Newline))
      ""))

;; The URL is baked as a real defun (not a defvar — MVM active limitation #7
;; means a defvar initform never runs at boot).  MODUS_NET_URL overrides it.
(defvar *net-url-source*
  (if *net-build-p*
      (format nil "~%(defun %net-fetch-url () ~S)~%(defun %lib-call-expr () ~S)~%(defun %ql-http-base () ~S)~%"
              (or #+sbcl (sb-ext:posix-getenv "MODUS_NET_URL")
                  "http://10.0.2.2:8080/sha1.tar")
              ;; #263 rung 3: the expression evaluated AFTER the fetched system
              ;; is installed + loaded.  A STRING, read at runtime — the build
              ;; reader cannot read `sha1:sha1-hex' (no SHA1 package until the
              ;; library loads).  Override with MODUS_LIB_EXPR.
              (or #+sbcl (sb-ext:posix-getenv "MODUS_LIB_EXPR")
                  "(sha1:sha1-hex \"abc\")")
              ;; Base URL for bare-metal ql:quickload: <base><name>.tar is
              ;; fetched over HTTP and installed from the in-memory bytes.  Also
              ;; a defun (limitation #7).  Default is the :8086 proxy that mirrors
              ;; the Quicklisp dist (sha1.tar, alexandria.tar, ...).
              (or #+sbcl (sb-ext:posix-getenv "MODUS_QL_BASE")
                  "http://10.0.2.2:8086/"))
      ""))

;; The rung-2 pipeline: bring the USB NIC up, DHCP for an address, fetch a
;; .tar over real HTTP and report its length + an FNV-1a-32 checksum so the
;; body can be verified byte-for-byte against the file the host served; then
;; (rung 3) INSTALL it, LOAD its sources and CALL a function from it.  The
;; installer itself — lib/tar.lisp + lib/install-tarball.lisp — is NOT here: it
;; comes from build-cli-common's *CLI-BARE-METAL-TARBALL* branch, baked with the
;; CL runtime, which is earlier in the blob and therefore a backward reference.
;;; PLATFORM-INDEPENDENT half of the driver: URL building, the FNV-1a checksum,
;;; the fetch, the report, and the fetch->install->call sequence.  Only
;;; run-net-pipeline (the NIC bring-up that precedes all of this) diverges, and
;;; it is spliced on below.
(defvar *net-driver-common-source* "
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
  ;; tar-do-entries.  Set it explicitly.  Same fix build-aarch64-ansi.lisp makes.
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

;; --- bare-metal ql:quickload over the network -------------------------------
;; The true `ql:quickload' front-end for the bare image.  Mirrors
;; modus-quicklisp/setup.lisp's %ql-quickload (keyword/string/symbol designator
;; -> <base><name>.tar -> install-tarball) but with an HTTP FETCH backend
;; (net-fetch-bytes -> install-tarball-from-bytes) in place of the filesystem
;; read, because bare metal has no filesystem.  ql-net-setup makes `ql:quickload'
;; nameable at the REPL; run-net-pipeline calls it once the NIC/DHCP are up.
(defun %ql-name-string (name)
  (cond ((stringp name) name)
        ((symbolp name) (string-downcase (symbol-name name)))
        (t (princ-to-string name))))

(defun %ql-quickload-net (name)
  (let* ((nm (%ql-name-string name))
         (url (concatenate 'string (%ql-http-base) nm \".tar\")))
    (write-string-serial \"; ql:quickload \") (write-string-serial nm)
    (write-string-serial \" <- \") (write-string-serial url) (write-char-serial 10)
    (let ((tb (net-fetch-bytes url)))
      (if (null tb)
          (progn (write-string-serial \"; ql:quickload fetch failed\")
                 (write-char-serial 10)
                 (error \"ql:quickload: fetch failed\"))
          (progn
            (write-string-serial \"FETCHED bytes=\") (print-dec (cdr tb))
            (write-char-serial 10)
            (install-tarball-from-bytes (car tb) nm)
            nm)))))

(defun ql-net-setup ()
  (handler-case
      (progn
        (unless (find-package \"QL\") (make-package \"QL\" :use (list \"CL\")))
        (let ((sym (intern \"QUICKLOAD\" \"QL\")))
          (export sym \"QL\")
          (setf (symbol-function sym) (function %ql-quickload-net)))
        (write-string-serial \"; ql:quickload ready (base \")
        (write-string-serial (%ql-http-base)) (write-string-serial \")\")
        (write-char-serial 10)
        t)
    (t (c) (write-string-serial \"; ql-net-setup failed\") (write-char-serial 10) nil)))

")

;;; DIVERGENCE 4 — the NIC bring-up, and only that.  Both arms print the same
;;; NET-PIPELINE-START / IP= / GW= / NET-PIPELINE-DONE trace and both end in the
;;; SHARED net-install-and-call, so a boot log from either board reads the same.
;;;
;;; :VIRT.  There is no firmware to program the PCI BARs on bare metal, so
;;; pci-assign-bars runs first (arch-aarch64-cl.lisp's fixpoint-aware version:
;;; BARs at VA 0x11000000, because VA 0x10000000-0x101FFFFF is remapped to the
;;; runtime metadata DRAM and a BAR there would route register accesses into
;;; RAM and the NIC would never answer).  e1000-probe then finds the device and
;;; sets up the rings.  Unlike cdc-ether-init it has no success return value to
;;; test, so the abort arm keys on the DHCP result instead: an all-zero IP after
;;; dhcp-client means nothing answered and there is no point fetching.
(defvar *net-pipeline-defun-source*
  (if *cl-repl-qemu-p*
      "(defun run-net-pipeline ()
  (write-string-serial \"NET-PIPELINE-START\") (write-char-serial 10)
  ;; 1. PCI bring-up: assign BARs (no firmware did it), then probe the E1000.
  ;;    e1000-probe prints its own MAC / link diagnostics.
  (handler-case (pci-assign-bars) (t (c) nil))
  (handler-case (e1000-probe) (t (c) nil))
  ;; 2. DHCP.  Prints DHCP:D / DHCP:O / DHCP:R / DHCP:A itself.
  ;;    Retry: on QEMU's e1000 model the first DISCOVER after a NIC reset draws
  ;;    a reply only after a fixed post-reset settle (STATUS.LU is already up),
  ;;    which the poll window can miss under TCG; a second DISCOVER once the NIC
  ;;    has cycled is answered immediately.  Re-run the client until it has an
  ;;    address (state+0x18) or the attempts run out.  The failing attempts also
  ;;    burn the settle time, so a later one hits the fast path.
  (dotimes (attempt 8)
    (when (zerop (mem-ref (+ (e1000-state-base) #x18) :u8))
      (handler-case (dhcp-client) (t (c) nil))))
  (let ((state (e1000-state-base)))
    (write-string-serial \"IP=\")
    (print-dec (mem-ref (+ state #x18) :u8)) (write-char-serial 46)
    (print-dec (mem-ref (+ state #x19) :u8)) (write-char-serial 46)
    (print-dec (mem-ref (+ state #x1A) :u8)) (write-char-serial 46)
    (print-dec (mem-ref (+ state #x1B) :u8)) (write-char-serial 10)
    (write-string-serial \"GW=\")
    (%net-print-ip-u32 (+ state #x1C)) (write-char-serial 10)
    ;; 3. HTTP GET of the library .tar from the QEMU slirp gateway, then
    ;;    (rung 3) install it, load it, and call a function from it.
    (if (zerop (mem-ref (+ state #x18) :u8))
        (progn (write-string-serial \"NET-PIPELINE-ABORT\") (write-char-serial 10))
        (progn
          ;; NIC + DHCP are up: make `ql:quickload' nameable at the REPL, then
          ;; run the baked sha1 self-test (evidence the install path works).
          (handler-case (ql-net-setup) (t (c) nil))
          (net-install-and-call (%net-fetch-url)))))
  (write-string-serial \"NET-PIPELINE-DONE\") (write-char-serial 10))
"
      "(defun run-net-pipeline ()
  (write-string-serial \"NET-PIPELINE-START\") (write-char-serial 10)
  ;; 1. DWC2 host controller + USB enumeration + CDC Ethernet.
  ;;    Prints DWC2:OK / PORT:xx / USB:vvvv:pppp / MAC:.. / CDC:OK itself.
  (let ((r (handler-case (cdc-ether-init) (t (c) 0))))
    (write-string-serial \"CDC-INIT=\") (print-dec r) (write-char-serial 10)
    (if (zerop r)
        (progn (write-string-serial \"NET-PIPELINE-ABORT\") (write-char-serial 10))
        (progn
          ;; 2. DHCP.  Prints DHCP:D / DHCP:O / DHCP:R / DHCP:A itself.
          ;;    Same retry as the QEMU arm: the host side of a CDC-ECM link
          ;;    (dnsmasq on the gadget interface) can miss the first
          ;;    DISCOVER while its interface is still coming up.
          (dotimes (attempt 8)
            (when (zerop (mem-ref (+ (e1000-state-base) #x18) :u8))
              (handler-case (dhcp-client) (t (c) nil))))
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
"))

(defvar *net-driver-source*
  (if *net-build-p*
      (concatenate 'string *net-driver-common-source* *net-pipeline-defun-source*)
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
;;;
;;; ...AND IT MUST HONOUR DIVERGENCE 7.  (Found 2026-08-31, once the constvec
;;; fix let :virt actually reach native code.)  The re-derivation above is the
;;; :RPI console choice, unconditionally — but :VIRT's console is neither Pi
;;; UART: it is the PL011 reached through the fixpoint page tables at
;;; +TDK-UART-VA+ = VA 0x20000000 (PA 0x09000000), which is exactly why the
;;; console block below sets NOTHING on :virt and lets cross.lisp bind the
;;; descriptor's :serial-base for the image build.
;;;
;;; That binding is BUILD-TIME ONLY.  WRITE-CHAR-SERIAL is a compiler INTRINSIC
;;; (trap #x0300), so the runtime JIT emits it INLINE against the RUNTIME value
;;; of *aarch64-serial-base* — and this co-init was handing :virt the Pi's
;;; 0x3F201000, an identity-mapped device VA with nothing behind it.  A JIT'd
;;; (write-char-serial c) therefore wrote into the void.  Measured: with the
;;; constvec fix in and 247 forms running native, the probe printed everything
;;; that went through the BAKED write-string-serial and then WEDGED on the
;;; first JIT-inlined write-char-serial.  An ordinary DEFUN hides this — a JIT'd
;;; caller CALLs the baked one, which carries the right UART — so only the
;;; intrinsics diverge, which is why it survived this long.
(defvar *rpi-jit-coinit-override*
  (if (and *jit-on* (not *cl-repl-x64-p*))
      (let* ((chain (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_CHAINLOAD")))
                      (and v (string= v "1"))))
             (mini (and (not *cl-repl-virt-p*)
                        (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_MINIUART")))
                          (if (and v (plusp (length v)))
                              (not (string= v "0"))
                              chain)))))
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
  ;; Mirror the BAKED alloc-overshoot guard band (build-cl-repl-common sets
  ;; *aarch64-gc-limit-guard* #x800000 for the image build).  The in-image
  ;; value is the defvar's 0, so anything the runtime translator emits that
  ;; reads it -- a trampoline exit -- would hand the mutator an UNGUARDED
  ;; limit.  Bare x64 lost alexandria to exactly this class (283cccf).
  (setq *aarch64-gc-limit-guard* #x800000)
  ;; *aarch64-gc-bitmap-enabled* at runtime: MUST be T — while NIL, objects
  ;; allocated by JIT'd code carry no object-start/cons-kind bit and the
  ;; native GC's bitmap gate (applied by scan_word to EVERY scanned word,
  ;; not just stack roots) refuses to FORWARD any reference to them: a
  ;; runtime DEFMETHOD's JIT-materialized specializer list goes stale at the
  ;; first collection and every runtime-defined GF loses dispatch (#281 —
  ;; the exact bug found and fixed on the hosted CLI, de6b02c).  The
  ;; recursive-exception-storm-at-first-JIT-alloc that once made this
  ;; enable look unsafe was symptom 3 of the alloc-overshoot guard-band
  ;; bug, root-caused and fixed in bfca1db (*aarch64-gc-limit-guard* 8MB),
  ;; and the enable was verified clean there (QEMU raspi3b, alexandria
  ;; through the JIT, bitmap-enabled image).  Default is therefore ON;
  ;; MODUS_RPI_JIT_BITMAP=0 builds it out for triage.
  ~A
  ;; #282 layer 2: JIT-mode li-const reads the pool object THROUGH the
  ;; GC-updated constant vector at #x10000F10 instead of baking its heap
  ;; address into the instruction stream.  A baked address is only re-baked
  ;; when the seam re-enters the thunk, so a collection that fired WHILE a
  ;; thunk was running left the rest of that run reading stale from-space --
  ;; on the hosted CLI that was a crash after about 5500 loop iterations.
  ;; (NO TILDE IN THIS BLOCK: it is a host FORMAT template, so a tilde is a
  ;; directive -- same class of trap as the no-double-quotes rule.)  The
  ;; native trampoline scans #x10000F10 as a fixed root (translate-aarch64),
  ;; and this image always uses that trampoline.
  ;;
  ;; ZERO THE ROOT FIRST, and note this is NOT ceremony on bare metal: DRAM
  ;; comes up with whatever was in it (QEMU zero-fills, silicon does not --
  ;; see the zeroed-DRAM dependency found on the real Pi Zero 2 W).  A garbage
  ;; word here would be read back as a tagged vector and its length taken.
  ;; %jit-constvec treats 0 as no-vector-installed, which is the safe state.
  (setf (mem-ref #x10000F10 :u64) 0)
  (setq *aarch64-jit-constvec-p* t)
  ~A
  ;; Seed the exec-page bump pointer THROUGH the same accessors the trap's
  ;; bare-metal arm reads, not through the Pi literals.  On :virt the literals
  ;; 0x13FFFFF0 / 0x14000000 are the QEMU PCI MMIO window (L2[159]/L2[160],
  ;; device nGnRnE, nothing behind it), so this store went nowhere while the
  ;; allocator it was meant to seed lives at 0x03FFFFF0.  Harmless either way --
  ;; the #x0531 arm range-checks the word and resets an out-of-range one to the
  ;; region base -- but an init that writes a different address than the thing
  ;; it initialises is exactly the divergence class this file keeps tripping on.
  (setf (mem-ref (%jit-exec-bump) :u64) (%jit-exec-lo))
  t)
"
                (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_JIT_BITMAP")))
                  (if (and v (string= v "0"))
                      ";; bitmap-enable OFF (MODUS_RPI_JIT_BITMAP=0 triage build)"
                      "(setq *aarch64-gc-bitmap-enabled* t)"))
                (cond
                  ;; :VIRT — the fixpoint UART VA, NOT a Pi peripheral address.
                  ;; Same PL011 register layout as the QEMU raspi3b arm below;
                  ;; only the base differs, because boot-aarch64.lisp's L2[256]
                  ;; maps VA 0x20000000 to PA 0x09000000.  Must match
                  ;; boot-aarch64.lisp's +TDK-UART-VA+ / :SERIAL-BASE.
                  (*cl-repl-virt-p*
                   "(setq *aarch64-serial-base* #x20000000)
  (setq *aarch64-serial-width* 0)
  (setq *aarch64-serial-tx-poll* nil)
  (setq *aarch64-serial-rx-poll* (list #x18 4 :tbnz))")
                  (mini
                   "(setq *aarch64-serial-base* #x3F215040)
  (setq *aarch64-serial-width* 2)
  (setq *aarch64-serial-tx-poll* (list #x14 5 :tbz))
  (setq *aarch64-serial-rx-poll* (list #x14 0 :tbz))")
                  (t
                   "(setq *aarch64-serial-base* #x3F201000)
  (setq *aarch64-serial-width* 0)
  (setq *aarch64-serial-tx-poll* nil)
  (setq *aarch64-serial-rx-poll* (list #x18 4 :tbnz))"))))
      ""))

(defvar *cli-bare-metal-net-source*
  ;; *cl-repl-jit-region-source* LAST: its %jit-exec-* defuns must come after
  ;; translate-aarch64.lisp's defaults so last-defun-wins picks the :virt DRAM
  ;; window.  Empty string on :rpi, which keeps the translator's own values.
  (concatenate 'string *net-source* *net-url-source* *net-driver-source*
               *rpi-jit-coinit-override* *cl-repl-jit-region-source*))

;;; ARCH SLOT: the toplevel entry / probe program.  The hosted CLIs hand off to
;;; cli-toplevel here; this image runs the same E2SMOKE self-check the bare ANSI
;;; gate runs, then the net pipeline, then the serial REPL.
;;; What a restored Pi process runs instead of boot init: straight to the
;;; toplevel the epilogue would have reached (no E2SMOKE; the net pipeline
;;; only when the build auto-starts it).
(defvar *cli-arch-core-resume*
  (if *cl-repl-rpi-p*
      (concatenate 'string
        "    (write-string-serial \"CORE-RESTORED\") (write-char-serial 10)
"       *net-pipeline-call*
        ;; SHIP JIT=1: a restored core adopts the SAME JIT default as a fresh
        ;; boot (*jit-on*) instead of being forced OFF.  The historical forced-
        ;; OFF is retired now that all three of its reasons are resolved:
        ;;   (a) retry-on-hot keeps one-shot LOAD forms interpreted, so JIT=1 no
        ;;       longer taxes ql:quickload (was ~10x slower; now interpret-speed);
        ;;   (b) cores carry the JIT exec arena (43efbe0) — no dangling fn-pointer
        ;;       on restore;
        ;;   (c) a restored ql core under *use-jit*=t was board-validated
        ;;       (quickload alexandria; flatten/iota correct, native-count 0).
        ;; MODUS_RPI_JIT=0 builds still restore JIT-off (*jit-on* NIL).
        (if *jit-on*
            "    (setq *use-jit* t)
"
            "    (setq *use-jit* nil)
")
        "    (handler-case (cl-serial-repl) (t (c) nil))
    (halt)
")
      ""))

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
  ;; #306: boot is over — enable the JIT's late-bound call bridge for what the
  ;; REPL evaluates (OFF through boot; see *jit-bridge-on* in mvm-eval.lisp).
  (setq *jit-bridge-on* t)
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
;;; Build the bare-metal image
;;; ============================================================

;;; DIVERGENCE 6 — the boot descriptor.
;;;
;;; :RPI.  boot-rpi.lisp arrives via lib/load-mvm.lisp already; boot-rpi-cl.lisp
;;; is loaded AFTER it and redefines `rpi-boot-descriptor' to the CL-lineage one
;;; (kernel8.img @0x80000, MMU off, identity addressing, PL011 console, Cheney
;;; heap registers).  The redefinition is process-local, so `build-rpi-ssh' /
;;; `-hid' / `-periph' are untouched.
;;;
;;; :VIRT.  boot-aarch64.lisp's FIXPOINT descriptor — MMU page tables that remap
;;; the x64-shaped runtime metadata VAs onto real DRAM (VA 0x10000000 ->
;;; PA 0x50000000 via L2[128]); without it, #x10000080 on QEMU virt targets
;;; device memory below DRAM base 0x40000000 and the kernel hangs the moment
;;; symbol-value reads the global-alist head.  QEMU -kernel loads the raw image
;;; at PA 0x40200000 (VA 0x80000).  Two descriptor knobs go with it and are
;;; NOT defaults:
;;;   *AARCH64-FIXPOINT-NIL-VALUE* = #xDEAD0001.  The compiler bakes
;;;     +nil-value+ = #xDEAD0001 into literals and interp.lisp keys truthiness
;;;     on that exact bit pattern; the legacy fixpoint default x26 = 0 splits
;;;     the NIL representation and breaks mvm-eval.  (Default 0 keeps
;;;     build-fixpoint byte-identical, which is why it is set here.)
;;;   *AARCH64-FIXPOINT-REENTRY-GUARD* = T.  A wild indirect branch to the image
;;;     base re-runs the boot preamble, which rebuilds the page tables under the
;;;     live MMU and kills the machine in a recursive fetch abort.  With the
;;;     guard it longjmps to the armed handler-case instead.
(cond
  (*cl-repl-virt-p* (mvm-load "boot/boot-aarch64.lisp"))
  (*cl-repl-x64-p*  (mvm-load "boot/boot-x64.lisp"))
  (t                (mvm-load "boot/boot-rpi-cl.lisp")))

(in-package :modus.mvm)

;;; DIVERGENCE 6/7 (:X64 arm) — the x86-64 boot descriptor and console.
;;; Verbatim from the retired standalone build-x64-cl-repl.lisp: the stack top
;;; moves to 512 MB (the 8 MB default sits inside a 60 MB image), data pages
;;; above the heap base are NX, the x64 translator is installed in BARE-METAL
;;; mode (no Linux syscalls: serial goes to COM1 port I/O), the collector is
;;; on, and the native-code offset is measured from the boot preamble the
;;; descriptor emits (multiboot header + 32-bit boot + 64-bit entry).
(when cl-user::*cl-repl-x64-p*
  (setf modus.mvm::*x64-stack-top-override* #x20000000)
  (setf modus.mvm::*x64-nx-data-enable* t)
  (funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
  (setf modus.mvm.x64::*x64-linux-mode* nil)
  (setf modus.mvm.x64::*x64-gc-enabled* t)
  ;; Disable the CONS-KIND bitmap scan_word reject on bare x64.  The kind bitmap
  ;; base is [bitmap_base] + +mcgc-kindbitmap-delta+, and that delta (#xFE4000)
  ;; is a LINUX-x64 layout constant — boot-x64.lisp lays the GC metadata out
  ;; differently, so on bare the check reads a wrong, uninitialised region and
  ;; falsely rejects valid conservative roots, dropping live objects across a
  ;; collection (symbols came back with empty name strings; the alexandria
  ;; install then died with read/eval errors, sooner the more GCs ran).  The
  ;; object-start bitmap (correctly based on the config word) still validates
  ;; roots, which is sufficient.  Enable the kind check here only once the delta
  ;; is made layout-agnostic for bare (a config word filled by boot-x64).  The
  ;; SET side is left as a dead no-op (its bits are never read).
  (setf modus.mvm.x64::*ws5-force-no-kindcheck* t)
  (setf modus.mvm.x64::*x64-native-code-offset*
        (let ((buf (make-mvm-buffer))
              (desc (x64-boot-descriptor)))
          (funcall (getf desc :multiboot-header-fn) buf)
          (funcall (getf desc :boot32-fn) buf)
          (funcall (getf desc :kernel64-entry-fn) buf)
          (let ((n (+ 5 (length (mvm-buffer-used-bytes buf)))))
            (format t "~%Bare-metal boot preamble: ~D bytes (native code offset)~%" n)
            n)))
  (format t "~&;; CONSOLE: COM1 via port I/O (x86-64 QEMU-pc)~%"))

(defun %x64-memory-map-asserts (image)
  "Build-time memory-map asserts for the :X64 image (DIVERGENCE 9 arm)."
  (let* ((image-bytes (length (kernel-image-image-bytes image)))
         (image-lo    #x100000)
         (image-hi    (+ image-lo image-bytes))
         (net-lo      #x0C000000)
         (net-hi      #x0C113000)
         (scratch-lo  #x0FE00000)
         (heap-lo     #x10000000))
    (when (>= image-hi net-lo)
      (error "BUILD-TIME ASSERT: image [~X..~X] (~,1F MB) reaches the E1000 DMA ~
              region at ~X." image-lo image-hi (/ image-bytes 1024.0 1024.0) net-lo))
    (when (>= net-hi scratch-lo)
      (error "BUILD-TIME ASSERT: E1000 region end ~X reaches the scratch buffers ~
              at ~X." net-hi scratch-lo))
    (format t "~%QEMU pc memory map (identity, 4 GB):~%")
    (format t "  image      ~8,'0X .. ~8,'0X  (~,2F MB)~%"
            image-lo image-hi (/ image-bytes 1024.0 1024.0))
    (format t "  net/DMA    ~8,'0X .. ~8,'0X  (E1000 rings + state)~%" net-lo net-hi)
    (format t "  scratch    ~8,'0X / ~8,'0X  (cstr / io-buf)~%" scratch-lo #x0FF00000)
    (format t "  heap       ~8,'0X .. ~8,'0X  (MCGC data), meta ~8,'0X~%"
            heap-lo #x1DFFF000 #x1E000000)
    (format t "  stack top  ~8,'0X            (grows down)~%" #x20000000)))

(when cl-user::*cl-repl-virt-p*
  (setf modus.mvm::*aarch64-fixpoint-nil-value* #xDEAD0001)
  (setf modus.mvm::*aarch64-fixpoint-reentry-guard* t))

;;; :VIRT JIT EXEC WINDOW — THE HOST SIDE OF IT.  (#282 residual, 2026-08-31.)
;;;
;;; *CL-REPL-JIT-REGION-SOURCE* (top of this file) redefines %JIT-EXEC-BUMP /
;;; -LO / -HI in the IMAGE, which fixes the exec pages the RUNTIME translator
;;; emits.  It does NOT fix the ones already BAKED: TRAP #x0531's bare-metal arm
;;; (translate-aarch64.lisp) calls (%jit-exec-bump) & co. AT EMIT TIME, so
;;; mvm-eval.lisp's own (%mmap-exec-page psize) — the single call the runtime
;;; JIT actually makes, inside %JIT-TRANSLATE-PAGE-1-AARCH64 — was assembled
;;; against the HOST defaults, which are the Pi's [0x14000000, 0x18000000) with
;;; its bump word at 0x13FFFFF0.
;;;
;;; On the Pi that is plain identity-mapped DRAM.  Under boot-aarch64.lisp's
;;; fixpoint tables it is NOT: step 7 fills L2[128..511] with device nGnRnE
;;; identity mappings for the QEMU virt PCI MMIO window (VA 0x10000000-
;;; 0x3FFFFFFF), and only L2[128] is restored to DRAM (step 7d).  VA 0x14000000
;;; is L2[160] — unbacked MMIO.  Every JIT page would be written into dropped
;;; stores and read back as zero, and the first %JIT-CALL would branch into a
;;; page of 0x00000000 (UDF).
;;;
;;; So override the HOST functions too, to the same window the image uses:
;;; [0x04000000, 0x05000000), VA 0x04000000 = L2[32] → PA 0x44000000, Normal-WB,
;;; no PXN/UXN, and above the AP[2] read-only band (L2[0..20], VA < 0x02A00000).
;;; FDEFINITION rather than DEFUN so this reads as the deliberate host-side
;;; override it is, and so the build log stays free of redefinition warnings.
(when cl-user::*cl-repl-virt-p*
  (setf (fdefinition 'modus.mvm::%jit-exec-bump) (lambda () #x03FFFFF0))
  (setf (fdefinition 'modus.mvm::%jit-exec-lo)   (lambda () #x04000000))
  (setf (fdefinition 'modus.mvm::%jit-exec-hi)   (lambda () #x05000000))
  (format t "~&;; JIT EXEC WINDOW (host): [0x04000000,0x05000000) bump 0x03FFFFF0~%"))

;; Install the AArch64 translator in BARE-METAL mode (*aarch64-linux-mode* is
;; NIL by default), so TRAP #x0300/#x0301 emit UART MMIO rather than Linux
;; syscalls — which is exactly what write-char-serial / read-char-serial need.
(unless cl-user::*cl-repl-x64-p*
  (install-aarch64-translator))

;;; DIVERGENCE 7 — the console.
;;;
;;; :VIRT does NOT set *AARCH64-SERIAL-BASE* at all.  cross.lisp binds it from
;;; the boot descriptor's :serial-base (= +TDK-UART-VA+ = VA 0x20000000, which
;;; the fixpoint L2[256] entry maps to PA 0x09000000, the QEMU virt PL011), and
;;; the width / tx-poll / rx-poll defaults are that same PL011.  Setting the
;;; variable here would OVERRIDE the descriptor with a raw PA the MMU does not
;;; map.  This is what build-aarch64-ansi.lisp does — i.e. it does nothing.
;;;
;;; :RPI has to choose, and the choice below is a real hardware fact:
;;
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
(if cl-user::*cl-repl-x64-p*
    nil
 (if cl-user::*cl-repl-virt-p*
    (format t "~&;; CONSOLE: PL011 via the fixpoint boot descriptor, VA 20000000 ~
               -> PA 09000000 (QEMU virt)~%")
    (progn
      (defvar *rpi-cl-miniuart*
        (let ((v #+sbcl (sb-ext:posix-getenv "MODUS_RPI_MINIUART")))
          (if (and v (plusp (length v)))
              (not (string= v "0"))
              ;; Default: follow the chain-load flag, because chain-loading is
              ;; the real-hardware path and SD/QEMU is the PL011 path.
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
            (format t "~&;; CONSOLE: PL011 UART0 0x3F201000 (QEMU raspi3b)~%"))))))

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
;;
;; SHARED, and it applies on :virt for the same reason (same semispaces, same
;; 0x10000000 upper bound, and above that the fixpoint tables hand out identity
;; DEVICE memory rather than DRAM).  One asymmetry worth knowing: boot-rpi-cl
;; folds the guard into its INITIAL x25, boot-aarch64's fixpoint entry does not
;; (x25 = +tdk-cons-limit-va+ exactly), so on :virt the very first semispace
;; runs unguarded.  That is benign — an overshoot there lands in the mapped
;; second semispace — and every limit the trampoline computes afterwards is
;; guarded.
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
;;
;; SHARED with :virt, and the BL flag is required there for the SAME reason:
;; boot-aarch64.lisp's fixpoint entry never materialises x28 either (it sets
;; x16/x17/x24/x25/x26 and nothing else), so BLR x28 would be an indirect call
;; through a garbage register.  NOTE that this is where :virt differs from
;; build-aarch64-ansi.lisp, which leaves both flags at their defaults and runs
;; gc.lisp's INTERPRETED %gc-collect: this is a REPL image, not a gate runner,
;; and it shares the Pi image's heap geometry exactly, so it gets the Pi
;; image's collector rather than the gate runner's.
(setf *aarch64-gc-native-mcgc* t)
(setf *aarch64-gc-trampoline-call-via-bl* t)

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

;;; PI MEMORY-MAP ASSERTS.  Verbatim from the pre-split script, moved into a
;;; DEFUN so the :virt/:rpi choice in the build tail is one IF instead of a
;;; duplicated 80-line block.  The +RPI-CL-*+ constants only exist once
;;; boot/boot-rpi-cl.lisp has been loaded, which the :virt build does not do —
;;; hence SYMBOL-VALUE rather than a direct reference, so reading this file on
;;; the :virt path raises no undefined-variable warning.
;;;
;;; The Pi map is TIGHTER than QEMU virt's in one direction and roomier in
;;; another: DRAM starts at 0 (so the runtime VAs are plain RAM, no remap),
;;; but the BCM2837 peripheral window at 0x3F000000 caps everything, and the
;;; image loads at 0x80000 rather than being handed a whole 2 MB-aligned
;;; region.  Assert the three ways a too-large image silently corrupts:
;;;   1. image end reaching the stack region  -> pushes shred native code
;;;      (this is precisely what boot-rpi.lisp's 0x00200000 stack top does
;;;      to any CL-sized image, task #47's failure class)
;;;   2. image end reaching the heap base     -> allocation over code
;;;   3. heap end reaching the peripherals    -> DMA/MMIO aliasing
(defun %rpi-memory-map-asserts (image)
    (let* ((image-bytes (length (kernel-image-image-bytes image)))
           (image-va-start #x80000)
           (image-va-end   (+ image-va-start image-bytes))
           (stack-top      (symbol-value '+rpi-cl-stack-top+))
           (stack-headroom (* 8 1024 1024))
           (stack-va-lo    (- stack-top stack-headroom))
           (heap-base      (symbol-value '+rpi-cl-heap-base+))
           (heap-end       (symbol-value '+rpi-cl-heap-end+))
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
              heap-base heap-end (symbol-value '+rpi-cl-heap-mid+))
      (format t "  periph     ~8,'0X ..            (PL011 UART0 at 3F201000)~%"
              periph-base)
      ;; #209 rung 2: the USB/net DMA + state block (net/arch-rpi-cl.lisp).
      ;; It must clear BOTH the runtime metadata window (which ends at
      ;; 0x10200000) and the peripherals, or the NIC DMAs over live data —
      ;; the exact failure build-aarch64-ansi.lisp's net relocation exists to stop.
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
)

(format t "~%Compiling bare-metal ~A CL REPL image (~D chars)...~%"
        (cond (cl-user::*cl-repl-virt-p* "AArch64 QEMU-virt")
              (cl-user::*cl-repl-x64-p* "x86-64 QEMU-pc")
              (t "RPi"))
        (length cl-user::*full-source*))

;;; DIVERGENCE 8 — BUILD-IMAGE :TARGET and the default output path.
;;; MODUS_CL_REPL_OUT overrides either one.
(let ((image (build-image :target (cond (cl-user::*cl-repl-virt-p* :fixpoint)
                                        (cl-user::*cl-repl-x64-p* :x86-64)
                                        (t :rpi))
                          :source-text cl-user::*full-source*)))
  (let ((path (or #+sbcl (sb-ext:posix-getenv "MODUS_CL_REPL_OUT")
                  (cond (cl-user::*cl-repl-virt-p* "/tmp/modus-aarch64-cl-repl.bin")
                        (cl-user::*cl-repl-x64-p* "/tmp/modus-x64-cl-repl.bin")
                        (t "/tmp/piboot/kernel8.img")))))
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
    ;; DIVERGENCE 9 — BUILD-TIME MEMORY-MAP ASSERTS.
    ;;
    ;; The VAs are the same on both platforms, but what BOUNDS them is not: the
    ;; Pi is capped by the BCM2837 peripheral window at 0x3F000000 and by the
    ;; 496 MB a Pi Zero 2 W actually gives the ARM, while QEMU virt is capped by
    ;; the fixpoint page tables and by -m 512.  Two assert blocks, therefore.
    ;; ------------------------------------------------------------------
    (cond
      (cl-user::*cl-repl-x64-p* (%x64-memory-map-asserts image))
      (cl-user::*cl-repl-virt-p*
        ;; ---- QEMU virt (fixpoint MMU) ----------------------------------
        ;; Image loads at VA 0x80000 and grows UP; the stack grows DOWN from
        ;; +TDK-STACK-VA+ (0x08000000) and the Cheney heap starts at 0x09000000.
        ;; If the image reaches the stack, pushes shred native code through the
        ;; shared mapping (commit 8bcacc8's layout-fragility class).
        (let* ((image-bytes (length (kernel-image-image-bytes image)))
               (image-va-start #x80000)
               (image-va-end   (+ image-va-start image-bytes))
               (stack-top      (or (and (boundp 'modus.mvm::+tdk-stack-va+)
                                        (symbol-value 'modus.mvm::+tdk-stack-va+))
                                   #x08000000))
               (stack-headroom (* 8 1024 1024))
               (stack-va-lo    (- stack-top stack-headroom))
               (heap-base      #x09000000)
               (heap-end       #x10000000)
               (objmap-lo      #x05000000)
               (consmap-lo     #x05100000)
               (map-bytes      #xE0000)
               (maps-hi        (+ consmap-lo map-bytes)))
          (when (>= image-va-end stack-va-lo)
            (error "BUILD-TIME ASSERT: image [~X..~X] (~,1F MB) reaches the stack ~
                    region [~X..~X].  Stack pushes would overwrite native code."
                   image-va-start image-va-end (/ image-bytes 1024.0 1024.0)
                   stack-va-lo stack-top))
          (when (>= image-va-end heap-base)
            (error "BUILD-TIME ASSERT: image end ~X reached the heap base ~X."
                   image-va-end heap-base))
          ;; #160 bitmaps — same addresses as the Pi, and here they must ALSO
          ;; stay under VA 0x10000000, because L2[128..511] map everything above
          ;; that as identity DEVICE memory (PCI MMIO), not DRAM.
          (when (< objmap-lo image-va-end)
            (error "BUILD-TIME ASSERT: #160 object-start bitmap at ~X is inside ~
                    the image (ends ~X)." objmap-lo image-va-end))
          (when (>= maps-hi stack-va-lo)
            (error "BUILD-TIME ASSERT: #160 bitmaps end ~X reach the stack low ~
                    water ~X." maps-hi stack-va-lo))
          (when (>= maps-hi #x10000000)
            (error "BUILD-TIME ASSERT: #160 bitmaps end ~X is at/above ~X, where ~
                    the fixpoint page tables switch from DRAM to identity ~
                    DEVICE memory (PCI MMIO)." maps-hi #x10000000))
          (format t "~%QEMU virt memory map (fixpoint MMU, VA 0-0FFFFFFF -> PA +40000000):~%")
          (format t "  image      ~8,'0X .. ~8,'0X  (~,2F MB)~%"
                  image-va-start image-va-end (/ image-bytes 1024.0 1024.0))
          (format t "  gc bitmaps ~8,'0X .. ~8,'0X  (#160 object-start + cons-kind)~%"
                  objmap-lo maps-hi)
          (format t "  stack top  ~8,'0X            (grows down, ~,1F MB clear of image)~%"
                  stack-top (/ (- stack-va-lo image-va-end) 1024.0 1024.0))
          (format t "  heap       ~8,'0X .. ~8,'0X  (112 MB, midpoint ~8,'0X)~%"
                  heap-base heap-end #x0C800000)
          (format t "  metadata   10000000 .. 10200000  (-> PA 50000000, L2[128])~%")
          ;; The E1000 DMA/state block (net/arch-aarch64-cl.lisp) is PA-IDENTITY
          ;; at 0x502xxxxx: above the metadata's PA 0x50000000-0x50200000 and
          ;; below the 0x60000000 end of QEMU virt's -m 512 DRAM.  Getting this
          ;; wrong is not a hang, it is silent heap corruption — the 2026-07-11
          ;; relocation to 0x49000000 aliased the Cheney heap's own PA and sent
          ;; the reader into an infinite bucket walk.
          (when cl-user::*net-build-p*
            (let ((net-lo   #x50200000)
                  (net-hi   #x50313000)
                  (meta-end #x50200000)
                  (dram-end #x60000000))
              (when (< net-lo meta-end)
                (error "BUILD-TIME ASSERT: E1000 DMA base ~X is inside the runtime ~
                        metadata's PA window (ends ~X)." net-lo meta-end))
              (when (>= net-hi dram-end)
                (error "BUILD-TIME ASSERT: E1000 region end ~X is at/above ~X, the ~
                        end of QEMU virt -m 512 DRAM." net-hi dram-end))
              (format t "  net/DMA    ~8,'0X .. ~8,'0X  (PA-identity, E1000 rings + state)~%"
                      net-lo net-hi)))))
        ;; ---- Raspberry Pi 3B / Zero 2 W --------------------------------
      (t (%rpi-memory-map-asserts image)))
    (format t "~%Run: ~A~%"
            (cond
              (cl-user::*cl-repl-x64-p*
                (format nil "qemu-system-x86_64 -m 512 -kernel ~A -display none -serial stdio -no-reboot~A"
                        path
                        (if cl-user::*net-build-p*
                            " -device e1000,netdev=net0 -netdev user,id=net0"
                            "")))
              (cl-user::*cl-repl-virt-p*
                (format nil "qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 -kernel ~A -nographic -no-reboot~A"
                        path
                        (if cl-user::*net-build-p*
                            " -device e1000,netdev=net0,romfile=,rombar=0 -netdev user,id=net0"
                            "")))
              (t (format nil "qemu-system-aarch64 -M raspi3b -kernel ~A -serial stdio -serial null -display none -no-reboot~A"
                        path
                        (if cl-user::*net-build-p*
                            " -device usb-net,netdev=net0 -netdev user,id=net0"
                            "")))))))
