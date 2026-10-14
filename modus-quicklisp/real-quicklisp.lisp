;;;; modus-quicklisp/real-quicklisp.lisp — let the REAL Quicklisp client fetch.
;;;;
;;;; The stock Quicklisp client (~/quicklisp/setup.lisp) loads on modus as is,
;;;; but its network layer dispatches on QL-IMPL:*IMPLEMENTATION*, an object
;;;; chosen by *FEATURES*.  Quicklisp has classes for SBCL, CCL, ECL, ... and
;;;; none for :MODUS or :GENERA (modus declares :GENERA for library
;;;; compatibility, but Quicklisp never had a Genera implementation either).
;;;; Nothing matches, and the first download dies with
;;;;
;;;;   Sorry, quicklisp in implementation "Modus" is not supported yet.
;;;;
;;;; This file adds a MODUS implementation and gives it HOST-ADDRESS and
;;;; OPEN-CONNECTION over modus's SB-BSD-SOCKETS.  Every other Quicklisp
;;;; operation already has a portable default.  Load it right after setup:
;;;;
;;;;   (load "~/quicklisp/setup.lisp")
;;;;   (load "/abs/path/to/modus/modus-quicklisp/real-quicklisp.lisp")
;;;;   (ql:quickload :zpng)
;;;;
;;;; Verified: downloads and installs lispqr, zpng and salza2 from the
;;;; Quicklisp dist (plain HTTP, as Quicklisp always fetches).

(in-package #:ql-impl)

(defclass modus (lisp) ())

(setf *implementation* (make-instance 'modus))

(in-package #:ql-network)

(ql-impl:defimplementation (host-address :for ql-impl::modus) (host)
  (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))

(ql-impl:defimplementation (open-connection :for ql-impl::modus) (host port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :protocol :tcp :type :stream)))
    (sb-bsd-sockets:socket-connect socket (host-address host) port)
    (sb-bsd-sockets:socket-make-stream socket
                                       :element-type '(unsigned-byte 8)
                                       :input t :output t :buffering :full)))
