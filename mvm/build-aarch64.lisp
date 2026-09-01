;;;; build-aarch64.lisp — the BARE-METAL AArch64 QEMU-virt image that runs the
;;;; REAL CL: a CLEAN kernel (no test corpus), E1000 networking.
;;;;
;;;; THIN HEAD.  Everything is in mvm/build-cl-repl-common.lisp, which this file
;;;; shares with mvm/build-rpi-cl-repl.lisp (the Raspberry Pi sibling).  Same
;;;; contract as the four ANSI gate runners over mvm/build-ansi-common.lisp:
;;;; bind the one selector, load the common file, and that file does the rest —
;;;; including the build tail.  The nine platform-divergent sites are enumerated
;;;; at the top of the common file and marked DIVERGENCE 1..9 where they occur.
;;;;
;;;; A QEMU virt kernel that drops into Modus's own self-hosted Common Lisp REPL
;;;; over the PL011: the CL reader, `eval' = mvm-eval (compile -> MVM bytecode
;;;; -> mvm-interpret), and the CL printer.  There is NO second Lisp here —
;;;; `mvm/repl-source.lisp' is not part of this image.
;;;;
;;;; NOT THE ANSI GATE RUNNER.  The file that used to have this name was the
;;;; bare-metal AArch64 ANSI runner, which BAKES the transformed ANSI test
;;;; corpus into the kernel.  It is now mvm/build-aarch64-ansi.lisp and is
;;;; unaffected by this one.  See CLAUDE.md "Build taxonomy — clean images vs
;;;; ANSI gate runners"; this is a clean image and bakes no corpus.
;;;;
;;;; Boot/memory: boot/boot-aarch64.lisp's FIXPOINT descriptor (MMU page tables
;;;; remap the runtime metadata VAs onto DRAM), image at VA 0x80000 = PA
;;;; 0x40200000, stack top 0x08000000, Cheney heap [0x09000000, 0x10000000).
;;;; Identical VAs to the Pi image — that is what makes the two shareable.
;;;;
;;;; Usage: sbcl --dynamic-space-size 12288 --script mvm/build-aarch64.lisp
;;;; Run:   qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 512 \
;;;;          -kernel /tmp/modus-aarch64-cl-repl.bin -nographic -no-reboot
;;;; Net:   MODUS_NET_BUILD=1 MODUS_NET_BUFSZ=400000 \
;;;;          sbcl --dynamic-space-size 12288 --script mvm/build-aarch64.lisp
;;;;        then add to the QEMU line:
;;;;          -device e1000,netdev=net0,romfile=,rombar=0 \
;;;;          -netdev user,id=net0
;;;; Output path override: MODUS_CL_REPL_OUT.
;;;;
;;;; Knobs (all read by the common file): MODUS_NET_BUILD, MODUS_NET_URL,
;;;; MODUS_NET_BUFSZ, MODUS_LIB_EXPR, MODUS_NET_NOAUTO, MODUS_RPI_NO_BLOB,
;;;; MODUS_RPI_NO_BRIDGE, MODUS_SYMMAP.  MODUS_SSH_BUILD is :RPI-only and
;;;; errors here (its actor/SSH address map is Pi DRAM) — see DIVERGENCE 5.

(defvar *cl-repl-platform* :virt)

(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
