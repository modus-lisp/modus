;;;; nic.lisp -- MODUS.NET's NIC interface (builder packages step 3)
;;;;
;;;; The net stack (ip.lisp, ssh.lisp, tcp-conns.lisp) calls these four
;;;; functions; a driver implements them.  Which driver is an IMAGE decision,
;;;; made at boot by the image's composition, e.g. for the NE2000 image
;;;;
;;;;   (setf (fdefinition 'modus.net:nic-send) #'modus.net.ne2000:ne2k-send)
;;;;
;;;; This replaces "the driver redefines E1000-SEND and the last DEFUN wins".
;;;; Each interface function calls through a cell that %DEFINE-SEAM names, so
;;;; (setf fdefinition) rebinds every caller on every target -- including code
;;;; compiled ahead of time, which on most targets has no entry pad to patch.
;;;; The cost is one indirect call per use.

(in-package :modus.net)

(defvar *nic-send* nil "MODUS.NET:NIC-SEND's binding.")
(defvar *nic-receive* nil "MODUS.NET:NIC-RECEIVE's binding.")
(defvar *nic-rx-buf* nil "MODUS.NET:NIC-RX-BUF's binding.")
(defvar *nic-probe* nil "MODUS.NET:NIC-PROBE's binding.")

(defun nic-send (buf len)
  "Transmit LEN bytes of the frame at BUF."
  (funcall *nic-send* buf len))

(defun nic-receive ()
  "Poll for a received frame; its length, 0 when none."
  (funcall *nic-receive*))

(defun nic-rx-buf ()
  "The address of the frame NIC-RECEIVE last reported."
  (funcall *nic-rx-buf*))

(defun nic-probe ()
  "Find and initialise the NIC."
  (funcall *nic-probe*))

(defun %define-nic-seams ()
  "Register the four interface functions as seams.  The image's binding
   function calls this before its (setf fdefinition)s."
  (modus-int::%define-seam 'nic-send '*nic-send*)
  (modus-int::%define-seam 'nic-receive '*nic-receive*)
  (modus-int::%define-seam 'nic-rx-buf '*nic-rx-buf*)
  (modus-int::%define-seam 'nic-probe '*nic-probe*))
