;;;; glass-desk.lisp -- the glass desk from a modus-lisp source tree, with nothing else.
;;;;
;;;;   modus --dynamic-space-size 8192 --script modus/scripts/glass-desk.lisp
;;;;
;;;; Run from (or with this file inside) a directory holding the modus-lisp repositories side
;;;; by side -- modus/, glass/, scribe/, ... as the source tarball unpacks them.  ASDF finds
;;;; every system there and nowhere else: no Quicklisp, no network, no home directory.  It
;;;; loads glass/wm -- the window manager: OPEN LOOK chrome, the desk's apps, a Lisp listener and
;;;; a Terminal (left out of its menu here), plus Files (warren) and Browser (loom) where they
;;;; load (DESK_APPS) -- and serves it over RFB (VNC); with DESK=lean, the lean desk (glass/desk-serve,
;;;; served by glass's demo/desk-run.lisp) instead:
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

(defun cl-user::desk-env (name default)
  (let ((v (uiop:getenv name))) (if (and v (plusp (length v))) v default)))

(defvar cl-user::*desk-lean* (string-equal (cl-user::desk-env "DESK" "wm") "lean"))

(let ((t0 (get-internal-real-time)))
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system (if cl-user::*desk-lean* "glass/desk-serve" "glass/wm")))
  (format t "~&desk: loaded in ~,1fs~%"
          (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
;; THE DESK'S OPTIONAL APPS (glass.wm's *WM-OPTIONAL-APPS*: the menu offers what is loaded):
;; DESK_APPS, the systems to try, default warren (Files) and loom/glass (Browser).  Each one
;; that does not load costs its app, not the desk.
(unless cl-user::*desk-lean*
  (dolist (sys (let ((s (cl-user::desk-env "DESK_APPS" "warren loom/glass")) (acc nil) (start 0))
                 (loop (let ((sp (position #\Space s :start start)))
                         (when (> (or sp (length s)) start) (push (subseq s start sp) acc))
                         (if sp (setq start (1+ sp)) (return))))
                 (nreverse acc)))
    (let ((t0 (get-internal-real-time)))
      (handler-case
          (progn (handler-bind ((warning #'muffle-warning)) (asdf:load-system sys))
                 (format t "~&desk: ~a loaded in ~,1fs~%" sys
                         (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
        (error (e) (format t "~&desk: ~a did not load (~a); the desk runs without it~%" sys e))))
    (finish-output)))
(glass:default-font)
(glass:default-font t)

(let ((core (uiop:getenv "DESK_CORE")))
  (when (and core (plusp (length core)))
    (format t "~&desk: saving ~a~%" core)
    (finish-output)
    (save-and-die core)))

(if cl-user::*desk-lean*
    (load (merge-pathnames "glass/demo/desk-run.lisp" cl-user::*desk-tree*))
    (let* ((port (parse-integer (cl-user::desk-env "DESK_PORT" "5910")))
           (address (cl-user::desk-env "DESK_ADDRESS" "127.0.0.1"))
           (size (cl-user::desk-env "DESK_SIZE" "1024x700"))
           (x (position #\x size)))
      (format t "~&desk: the window manager, ~a on ~a:~d -- point a VNC viewer there~%"
              size address port)
      (finish-output)
      ;; every app the loaded systems offer, but Terminal: a shell on the host is not this
      ;; desk's to hand out
      (funcall (find-symbol "SERVE-WM" "GLASS.WM") port
               :width (parse-integer size :end x) :height (parse-integer size :start (1+ x))
               :address address
               :apps (remove "Terminal" (funcall (find-symbol "WM-AVAILABLE-APPS" "GLASS.WM"))
                             :key #'first :test #'string=))))
