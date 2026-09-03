;;;; build-x64-cl-repl.lisp — the BARE-METAL x86-64 QEMU-pc image that runs the
;;;; REAL CL: a CLEAN kernel (no test corpus), E1000 networking when
;;;; MODUS_NET_BUILD=1.
;;;;
;;;; THIN HEAD.  Everything is in mvm/build-cl-repl-common.lisp, shared with
;;;; mvm/build-aarch64.lisp (QEMU virt) and mvm/build-rpi-cl-repl.lisp (the
;;;; Raspberry Pi): bind the one selector, load the common file, and that file
;;;; does the rest — including the build tail.  The x86 arms are marked
;;;; ":X64" at each DIVERGENCE site of the common file; the NIC adapter is
;;;; net/arch-x86-cl.lisp.
;;;;
;;;; Until 2026-09-03 this was a 1158-line standalone build (git history has
;;;; it).  It could not carry the fetch->install pipeline the bare-metal
;;;; quickload rig needs; the shared assembly gives it that for free.
;;;;
;;;; Usage: sbcl --dynamic-space-size 12288 --script mvm/build-x64-cl-repl.lisp
;;;;   MODUS_NET_BUILD=1 MODUS_NET_NOAUTO=1 MODUS_NET_BUFSZ=400000 for the rig;
;;;;   MODUS_X64_JIT=1 opts into the runtime JIT (off by default here).
;;;;   Output: /tmp/modus-x64-cl-repl.bin (MODUS_CL_REPL_OUT overrides).
;;;;   Run:    qemu-system-x86_64 -m 512 -kernel /tmp/modus-x64-cl-repl.bin \
;;;;             -display none -serial stdio -no-reboot \
;;;;             [-device e1000,netdev=net0 -netdev user,id=net0]

(defvar *cl-repl-platform* :x64)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
