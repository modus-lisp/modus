;;;; build-ppcmac-cl-repl.lisp — the CL REPL on a bare PowerPC Macintosh.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-ppcmac-cl-repl.lisp
;;;;   sbcl --script scripts/ppcmac-disk.lisp /tmp/modus-ppcmac.elf /tmp/modus-ppcmac.dsk
;;;;   qemu-system-ppc -M mac99 -m 1024 -nographic \
;;;;       -drive file=/tmp/modus-ppcmac.dsk,format=raw,media=disk
;;;;
;;;; Open Firmware boots the disk's blessed System Folder (its bootinfo.txt),
;;;; which loads :modus.elf; serial port A is the console.  Everything else is
;;;; build-cl-repl-common.lisp's :PPCMAC arms, boot/boot-ppcmac.lisp and
;;;; translate-ppc.lisp's *PPC-MAC-MODE*.  MODUS_CL_REPL_OUT overrides the
;;;; output path.
(defvar *cl-repl-platform* :ppcmac)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
