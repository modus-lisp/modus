;;;; build-i386-cl-repl.lisp — the BARE-METAL 32-bit x86 QEMU-pc image that runs
;;;; the REAL CL.
;;;;
;;;; THIN HEAD, exactly like mvm/build-x64-cl-repl.lisp: bind the one selector,
;;;; load mvm/build-cl-repl-common.lisp, and that file does the rest.  The i386
;;;; arms are marked ":I386" at each DIVERGENCE site of the common file.
;;;;
;;;; NO NETWORKING.  DIVERGENCE 3/4/5 have no :I386 arm, so this image is the
;;;; REPL and nothing else.  That is the point of the generic-image strategy:
;;;; SSH and tooling arrive as source the REPL loads, not as baked payloads.
;;;;
;;;; Usage: sbcl --dynamic-space-size 12288 --script mvm/build-i386-cl-repl.lisp
;;;;   Output: /tmp/modus-i386-cl-repl.bin (MODUS_CL_REPL_OUT overrides).
;;;;   Run:    qemu-system-i386 -m 512 -kernel /tmp/modus-i386-cl-repl.bin \
;;;;             -display none -serial stdio -no-reboot

(defvar *cl-repl-platform* :i386)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
