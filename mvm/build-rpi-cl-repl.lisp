;;;; build-rpi-cl-repl.lisp — the BARE-METAL RASPBERRY PI image that runs the
;;;; REAL CL (task #209).
;;;;
;;;; THIN HEAD.  Everything is in mvm/build-cl-repl-common.lisp, which this file
;;;; shares with mvm/build-aarch64.lisp (the QEMU-virt sibling).  Same contract
;;;; as the four ANSI gate runners over mvm/build-ansi-common.lisp: bind the one
;;;; selector, load the common file, and that file does the rest — including the
;;;; build tail.  The extraction was gated on this image coming out
;;;; BYTE-IDENTICAL to the pre-split script, MODUS_NET_BUILD off and on.
;;;;
;;;; A Raspberry Pi 3B / Zero 2 W (AArch64) kernel8.img that drops into Modus's
;;;; own self-hosted Common Lisp REPL over the serial port: the CL reader,
;;;; `eval' = mvm-eval (compile -> MVM bytecode -> mvm-interpret), and the CL
;;;; printer.  There is NO second Lisp here — `mvm/repl-source.lisp' (the
;;;; 708-line toy reader/printer that every legacy build-rpi-* and
;;;; build-pizero2w-* script bakes) is not part of this image.
;;;;
;;;; Usage: sbcl --dynamic-space-size 12288 --script mvm/build-rpi-cl-repl.lisp
;;;; Run:   qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \
;;;;          -serial stdio -serial null -display none
;;;; Output path override: MODUS_CL_REPL_OUT.
;;;;
;;;; Knobs (all read by the common file): MODUS_NET_BUILD, MODUS_NET_URL,
;;;; MODUS_NET_BUFSZ, MODUS_LIB_EXPR, MODUS_NET_NOAUTO, MODUS_SSH_BUILD,
;;;; MODUS_RPI_MINIUART, MODUS_RPI_CHAINLOAD, MODUS_RPI_JIT_BITMAP,
;;;; MODUS_RPI_NO_BLOB, MODUS_RPI_NO_BRIDGE, MODUS_SYMMAP.

(defvar *cl-repl-platform* :rpi)

(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
