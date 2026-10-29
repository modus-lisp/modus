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
     ("NIC-SEND" "NIC-RECEIVE" "NIC-RX-BUF" "NIC-PROBE"))
    ("MODUS.NET.NE2000" ("COMMON-LISP")
     ("NE2K-SEND" "NE2K-RECEIVE" "NE2K-RX-BUF" "NE2K-PROBE")))))

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
  (let ((out nil))
    (dolist (s (%kernel-package-specs))
      (let ((p (or (find-package (car s))
                   (make-package (car s) :use (cadr s)))))
        (dolist (e (caddr s))
          (export (intern e p) p))
        (push p out)))
    (nreverse out)))
