;;;; glass-desk.lisp -- the glass desk from a modus-lisp source tree, with nothing else.
;;;;
;;;;   modus --dynamic-space-size 8192 --script modus/scripts/glass-desk.lisp
;;;;
;;;; Run from (or with this file inside) a directory holding the modus-lisp repositories side
;;;; by side -- modus/, glass/, scribe/, ... as the source tarball unpacks them.  ASDF finds
;;;; every system there and nowhere else: no Quicklisp, no network, no home directory.  It
;;;; loads glass/desk-serve, the desk's faces, and then glass's own demo/desk-run.lisp, which
;;;; serves the desk over RFB (VNC):
;;;;   DESK_PORT (5910), DESK_ADDRESS (127.0.0.1; 0.0.0.0 in a container), DESK_SIZE (1024x700).
;;;; With DESK_CORE=FILE it saves the loaded desk as a core instead, for
;;;; `modus --core FILE --script glass/demo/desk-run.lisp` to start in a second.

(require :asdf)

(defvar cl-user::*desk-tree*
  ;; .../modus/scripts/glass-desk.lisp -> ... (the directory holding the repositories)
  (let ((here (or *load-truename* *load-pathname*)))
    (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults here))))

(setf asdf:*central-registry* nil)
(dolist (d (directory (merge-pathnames "*/" cl-user::*desk-tree*)))
  (push d asdf:*central-registry*))
(format t "~&desk: ~d repositories under ~a~%" (length asdf:*central-registry*)
        (namestring cl-user::*desk-tree*))
(finish-output)

(let ((t0 (get-internal-real-time)))
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system "glass/desk-serve"))
  (format t "~&desk: loaded in ~,1fs~%"
          (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
(glass:default-font)
(glass:default-font t)

(let ((core (uiop:getenv "DESK_CORE")))
  (when (and core (plusp (length core)))
    (format t "~&desk: saving ~a~%" core)
    (finish-output)
    (save-and-die core)))

(load (merge-pathnames "glass/demo/desk-run.lisp" cl-user::*desk-tree*))
