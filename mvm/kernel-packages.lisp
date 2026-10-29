;;;; kernel-packages.lisp -- the kernel's packages beyond MODUS-INT
;;;;
;;;; Builder packages step 3: net, crypto, ssh and the boards move out of
;;;; MODUS-INT into packages of their own, one gate cycle at a time.  This file
;;;; is the ONE list of those packages.  The build host loads it (so the reader
;;;; can read source into them) and every image bakes it (so %INIT-PACKAGES
;;;; makes the same packages at boot, for the REPL and for the self-host build,
;;;; which re-reads this source in the image).  Plain CL on both sides.
;;;;
;;;; A seam between packages is an INTERFACE function bound per image with
;;;; (setf fdefinition) at boot (user, 2026-10-29): MODUS.NET:NIC-SEND is an
;;;; ordinary function whose definition the image's composition sets from its
;;;; driver's -- see net/nic.lisp.

(in-package :modus.mvm)

(defun %kernel-package-specs ()
  "Each entry is (NAME USE EXPORTS), all strings.  Order matters: a package
   comes after the ones it uses."
  (quote
   (("MODUS.NET" ("COMMON-LISP")
     ;; the NIC interface: every driver implements it, ip.lisp calls it
     ("NIC-SEND" "NIC-RECEIVE" "NIC-RX-BUF" "NIC-PROBE" "NIC-READ-REG"))
    ;; the drivers.  They use MODUS-INT for the board and console functions
    ;; it exports to them (%MODUS-INT-DRIVER-EXPORTS).
    ("MODUS.NET.NE2000" ("COMMON-LISP" "MODUS-INT")
     ("NE2K-SEND" "NE2K-RECEIVE" "NE2K-RX-BUF" "NE2K-PROBE"))
    ("MODUS.NET.E1000" ("COMMON-LISP" "MODUS-INT")
     ("E1000-HW-SEND" "E1000-HW-RECEIVE" "E1000-HW-RX-BUF" "E1000-HW-PROBE"
      "E1000-READ-REG" "E1000-WRITE-REG"))
    ("MODUS.NET.VIRTIO" ("COMMON-LISP" "MODUS-INT")
     ("VIRTIO-NET-PROBE" "VIRTIO-NET-SEND" "VIRTIO-NET-RECEIVE"
      "VIRTIO-NET-RX-BUF" "VIRTIO-NET-ACTIVE-P" "VIRTIO-NET-STATS"))
    ;; SHA-256/512, ChaCha20, Poly1305, X25519, Ed25519 (net/crypto*.lisp):
    ;; what SSH, the SNP attestation and the isolated net domain call
    ("MODUS.CRYPTO" ("COMMON-LISP" "MODUS-INT")
     ("SHA256-INIT" "SHA256" "SHA512-INIT" "SHA512" "SHA512-BLOCK"
      "CHACHA-BLOCK" "CHACHA20-CRYPT" "POLY1305" "X25519" "X25519-PUBLIC-KEY"
      "ED25519-PUBLIC-KEY" "ED25519-VERIFY" "ED-BASE-MULT" "ED-ENCODE-POINT"
      "ED-REDUCE-SCALAR" "ED-SCALAR-ADD" "ED-SCALAR-MULT-MOD-L"
      "CONCAT-BYTES" "CONCAT3-BYTES")))))

(defun %modus-int-driver-exports ()
  "What MODUS-INT exports to the step-3 packages: the console
   printers, PCI configuration access and the BOARD's address hooks (each
   arch-*-cl.lisp defines these for its machine).  The board hooks move to a
   MODUS.BOARD package when the boards get one.  Exported on the build host
   and in the image alike (%ENSURE-KERNEL-PACKAGES), so the image reads
   driver source as the host did."
  (quote ("PRINT-DEC" "PRINT-HEX-BYTE" "PRINT-HEX32" "WRITE-STRING-SERIAL"
          "%SERIAL-BYTE" "IO-DELAY" "PCI-CONFIG-READ" "PCI-CONFIG-WRITE"
          "E1000-STATE-BASE" "E1000-RX-DESC-BASE" "E1000-RX-BUF-BASE"
          "E1000-TX-DESC-BASE" "E1000-TX-BUF-BASE" "NE2K-RX-HOST"
          "VIRTIO-NET-RING-BASE" "VIRTIO-NET-STATE-BASE" "VIRTIO-NET-PT-PAGE"
          "SNP-MMIO-P" "SNP-MMIO-REAL-P" "SNP-MMIO-READ" "SNP-MMIO-WRITE"
          ;; for MODUS.CRYPTO: byte-buffer access, its scratch, and the
          ;; keep-alive a long computation calls (a seam the images rebind)
          "BUF-READ-U32" "BUF-WRITE-U32" "FE-SCRATCH-BASE" "USB-KEEPALIVE")))

(defun %kernel-package-name-p (name)
  "True when NAME is one of %KERNEL-PACKAGE-SPECS' packages."
  (let ((specs (%kernel-package-specs)) (hit nil))
    (loop
      (when (or hit (null specs)) (return hit))
      (when (string= name (car (car specs))) (setq hit t))
      (setq specs (cdr specs)))))

(defun %ensure-kernel-packages ()
  "Make every package in %KERNEL-PACKAGE-SPECS that does not exist yet, and
   export its names.  Answers the list of packages."
  (let ((out nil)
        (int (find-package "MODUS-INT")))
    (dolist (n (%modus-int-driver-exports))
      (export (intern n int) int))
    (dolist (s (%kernel-package-specs))
      (let ((p (or (find-package (car s))
                   (make-package (car s) :use (cadr s)))))
        (dolist (e (caddr s))
          (export (intern e p) p))
        (push p out)))
    (nreverse out)))
