;;;; build-uefi-cl-repl.lisp — the bare-metal x86-64 CL image (build-x64-cl-repl)
;;;; as a PE32+ EFI application, bootable by OVMF and — with the AmdSev OVMF
;;;; build and QEMU -kernel — MEASURED into an SEV-SNP launch digest.
;;;;
;;;; THIN HEAD over mvm/build-cl-repl-common.lisp, exactly like
;;;; build-x64-cl-repl.lisp, plus two knobs:
;;;;   MODUS_UEFI_SNP   unset/0 = plain UEFI image; test = fake-#VC self-test
;;;;                    on a plain machine; 1 = real SEV-SNP guest image.
;;;;   MODUS_CL_REPL_OUT  output path (default /tmp/modus-uefi-cl.efi)
;;;; plus everything build-x64-cl-repl accepts (MODUS_NET_BUILD=1 for E1000).
;;;;
;;;; Boot: scripts/run-uefi-cl.sh, or by hand with OVMF (see docs/snp-guest.md).
(defvar *cl-repl-platform* :x64)
(defvar *cl-repl-uefi-p* t)
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
;; SEV-SNP mode + the shared page for THIS image's NIC (net/arch-x86-cl.lisp
;; puts the E1000 rings at 0x0C000000..0x0C113000).
(setq modus.mvm::*x64-snp-mode*
      (let ((v (sb-ext:posix-getenv "MODUS_UEFI_SNP")))
        (cond ((or (null v) (string= v "") (string= v "0")) nil)
              ((string-equal v "test") :test)
              (t :snp))))
(setq modus.mvm::*snp-shared-base* #x0C000000)
;; DDC triage: MODUS_STATIC_BUILD=1 builds under SBCL with the SAME static-emit
;; configuration modus-sh --compile-uefi forces, to separate "in-image compiler
;; bug" from "static configuration breaks this image".
(when (let ((v (sb-ext:posix-getenv "MODUS_STATIC_BUILD"))) (and v (string= v "1")))
  (setq modus.mvm::*static-build-p* t)
  (setq modus.mvm::*mvm-emit-halves* nil)
  (setq modus.mvm::*mvm-eval-runtime-p* nil)
  (format t "~&;; DDC: *static-build-p* forced T for this SBCL build~%"))
(format t "~&;; UEFI-CL: SNP mode ~A, shared page ~X~%"
        modus.mvm::*x64-snp-mode* modus.mvm::*snp-shared-base*)
(load (merge-pathnames "build-cl-repl-common.lisp"
                       (directory-namestring (truename *load-truename*))))
