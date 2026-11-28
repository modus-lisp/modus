;;;; glass-desk.lisp -- the glass desk from a modus-lisp source tree, with nothing else.
;;;;
;;;;   desk.sh [FLAGS]       (a release zip: desk.cmd on Windows, desk.command on macOS)
;;;;   modus.com desk [FLAGS]
;;;;   modus --dynamic-space-size 8192 --script modus/scripts/glass-desk.lisp [FLAGS]
;;;;   sbcl --dynamic-space-size 8192 --script modus/scripts/glass-desk.lisp [FLAGS]
;;;;
;;;; Run from (or with this file inside) a directory holding the modus-lisp repositories side
;;;; by side -- modus/, glass/, scribe/, ... as the source tarball unpacks them.  ASDF finds
;;;; every system there and nowhere else: no Quicklisp, no network, no home directory.  It
;;;; loads glass/wm -- the window manager: OPEN LOOK chrome, the desk's apps, a Lisp listener and
;;;; a Terminal (left out of its menu here), plus Files (warren) and Browser (loom) where they
;;;; load (--apps) -- and serves it over RFB (VNC), and to web browsers (noVNC over a WebSocket,
;;;; glass/src/web.lisp) on a second port, opening a browser on it; with --lean, the lean desk
;;;; (glass/desk-serve, served by glass's demo/desk-run.lisp) instead.  --help lists the flags
;;;; (*DESK-FLAGS* below).  The DESK_PORT, DESK_ADDRESS, DESK_SIZE, DESK=lean, DESK_APPS and
;;;; DESK_CORE environment variables still work where no flag is given.  With --core FILE it
;;;; saves the loaded desk as a core instead, for `modus --core FILE --script
;;;; glass/demo/desk-run.lisp` to start in a second.

(require :asdf)

;;; ---- the command line ------------------------------------------------------------------------
;;; One parser for every way in: the words after `desk' when modus.com's main.lisp loads this
;;; (MODUS-INT::*CLI-COMMAND-LINE*), else the arguments after the script (`modus --script' and
;;; `sbcl --script' both leave just those in *POSIX-ARGV*, after the program).

(defparameter cl-user::*desk-flags*
  ;; (FLAG KEY ENV DEFAULT ARGUMENT-NAME DOC); ARGUMENT-NAME NIL is a switch
  '(("--port"     :port     "DESK_PORT"    "5910"      "N"    "the RFB (VNC) port")
    ("--web-port" :web-port nil            "6080"      "N"    "the web port: the desk in a browser at http://ADDRESS:N/")
    ("--no-web"   :no-web   nil            nil         nil    "serve no web port")
    ("--no-open"  :no-open  nil            nil         nil    "do not open a web browser on the desk")
    ("--address"  :address  "DESK_ADDRESS" "127.0.0.1" "ADDR" "the interface both ports bind (0.0.0.0: every one)")
    ("--size"     :size     "DESK_SIZE"    "1024x700"  "WxH"  "the screen")
    ("--apps"     :apps     "DESK_APPS"    "warren loom/glass" "\"SYS ...\"" "the optional apps' systems to try")
    ("--lean"     :lean     nil            nil         nil    "the lean desk (glass/desk-serve), not the window manager")
    ("--core"     :core     "DESK_CORE"    nil         "FILE" "save the loaded desk as a core in FILE, and exit")
    ("--help"     :help     nil            nil         nil    "this")))

(defun cl-user::desk-usage (&optional (stream *standard-output*))
  (format stream "~&usage: desk [FLAGS]                    (a release: desk.sh, desk.cmd, desk.command)~%~
                    ~7@Tmodus.com desk [FLAGS]~%~
                    ~7@Tmodus --script modus/scripts/glass-desk.lisp [FLAGS]~%~%")
  (loop for (flag nil nil default arg doc) in cl-user::*desk-flags*
        ;; ~22A, not ~T: modus's FORMAT does not know stdout's column, and ~24T there is 24 spaces
        do (format stream "  ~22a~a~@[ (default ~a)~]~%" (format nil "~a~@[ ~a~]" flag arg) doc default))
  (format stream "~%A flag's value may also follow =, as --port=5910.~%")
  (finish-output stream))

(defun cl-user::desk-exit (code)
  (finish-output) (finish-output *error-output*)
  (uiop:quit code))

(defun cl-user::desk-arguments ()
  (let* ((pkg (find-package "MODUS-INT"))
         (cli (and pkg (find-package "MODUS-APP") (find-symbol "*CLI-COMMAND-LINE*" pkg))))
    (if (and cli (boundp cli))
        ;; modus.com: the words after `desk' (none when it came from the menu)
        (rest (member "desk" (rest (symbol-value cli)) :test #'string-equal))
        (rest (symbol-value (find-symbol "*POSIX-ARGV*" "SB-EXT"))))))

(defun cl-user::parse-desk-flags (words)
  "WORDS (strings) -> a plist of the flags given.  An unknown flag or a missing value prints the
   usage and exits 2; --help prints it and exits 0."
  (let ((options '()))
    (flet ((bad (fmt &rest args)
             (apply #'format *error-output* (concatenate 'string "~&desk: " fmt "~%") args)
             (cl-user::desk-usage *error-output*)
             (cl-user::desk-exit 2)))
      (loop while words
            do (let* ((w (pop words))
                      (eq (and (> (length w) 2) (string= "--" w :end2 2) (position #\= w)))
                      (name (if eq (subseq w 0 eq) w))
                      (spec (assoc name cl-user::*desk-flags* :test #'string=)))
                 (unless spec (bad "unknown argument ~s" w))
                 (destructuring-bind (flag key env default arg doc) spec
                   (declare (ignore env default doc))
                   (cond ((eq key :help) (cl-user::desk-usage) (cl-user::desk-exit 0))
                         ((null arg)
                          (when eq (bad "~a takes no value" flag))
                          (setf (getf options key) t))
                         (t (setf (getf options key)
                                  (cond (eq (subseq w (1+ eq)))
                                        (words (pop words))
                                        (t (bad "~a needs ~a" flag arg))))))))))
    options))

(defvar cl-user::*desk-options* (cl-user::parse-desk-flags (cl-user::desk-arguments)))

(defun cl-user::desk-option (key)
  "KEY's value: its flag, else its environment variable, else its default."
  (let ((spec (find key cl-user::*desk-flags* :key #'second)))
    (or (getf cl-user::*desk-options* key)
        (let ((env (third spec)))
          (and env (let ((v (uiop:getenv env))) (and v (plusp (length v)) v))))
        (fourth spec))))

(defun cl-user::desk-env (name default)
  "What glass's demo/desk-run.lisp asks for its settings (it defines its own when this is absent):
   the flag that stands for environment variable NAME, else NAME, else DEFAULT."
  (let* ((spec (find name cl-user::*desk-flags* :key #'third :test #'equal))
         (flag (and spec (getf cl-user::*desk-options* (second spec))))
         (v (uiop:getenv name)))
    (or flag (and v (plusp (length v)) v) default)))

(defun cl-user::desk-port (string what)
  (or (ignore-errors (let ((n (parse-integer string))) (and (<= 0 n 65535) n)))
      (progn (format *error-output* "~&desk: ~a ~s is not a port~%" what string)
             (cl-user::desk-exit 2))))

(defun cl-user::desk-size ()
  (let* ((size (cl-user::desk-option :size))
         (x (position #\x size :test #'char-equal))
         (w (and x (ignore-errors (parse-integer size :end x))))
         (h (and x (ignore-errors (parse-integer size :start (1+ x))))))
    (unless (and w h (plusp w) (plusp h))
      (format *error-output* "~&desk: --size ~s is not WxH~%" size)
      (cl-user::desk-exit 2))
    (values w h size)))

;; check the values now, not after minutes of loading
(cl-user::desk-port (cl-user::desk-option :port) "--port")
(cl-user::desk-port (cl-user::desk-option :web-port) "--web-port")
(cl-user::desk-size)

;;; ---- the tree, and loading -------------------------------------------------------------------

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

(defvar cl-user::*desk-lean* (or (getf cl-user::*desk-options* :lean)
                                 (string-equal (or (uiop:getenv "DESK") "wm") "lean")))

(let ((t0 (get-internal-real-time)))
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system (if cl-user::*desk-lean* "glass/desk-serve" "glass/wm")))
  (format t "~&desk: loaded in ~,1fs~%"
          (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
;; THE DESK'S OPTIONAL APPS (glass.wm's *WM-OPTIONAL-APPS*: the menu offers what is loaded):
;; --apps, the systems to try, default warren (Files) and loom/glass (Browser).  Each one
;; that does not load costs its app, not the desk.
(unless cl-user::*desk-lean*
  (dolist (sys (let ((s (cl-user::desk-option :apps)) (acc nil) (start 0))
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

(let ((core (cl-user::desk-option :core)))
  (when (and core (plusp (length core)))
    (format t "~&desk: saving ~a~%" core)
    (finish-output)
    (save-and-die core)))

;;; ---- opening a browser on it -------------------------------------------------------------------

(defun cl-user::desk-loopback-p (address)
  (or (string-equal address "localhost") (string= address "::1")
      (and (> (length address) 4) (string= address "127." :end1 4))))

(defun cl-user::desk-open-url (url)
  "Ask the platform to open URL in the default browser, without waiting.  T if it was asked; NIL
   and why if there is no way here.  Never an error: the desk runs either way."
  (flet ((launch (program &rest args)
           (uiop:launch-program (cons program args) :input nil :output nil :error-output nil)
           t))
    (handler-case
        ;; *FEATURES*, not UIOP:OS-WINDOWS-P: modus's UIOP has that only since 80a67663, after
        ;; the 20261126 release this must run on
        (cond ((or (member :win32 *features*) (member :windows *features*))
               (let ((w32 (and (find-package "MODUS-INT") (find-symbol "W32-CALL*" "MODUS-INT"))))
                 (if (and w32 (fboundp w32))
                     ;; ShellExecuteW (NULL, "open", URL, NULL, NULL, SW_SHOWNORMAL): > 32 is success
                     (let ((r (funcall w32 :ptr "shell32.dll!ShellExecuteW" 0 "open" url 0 0 1)))
                       (if (and (integerp r) (> r 32))
                           t
                           (values nil (format nil "ShellExecuteW answered ~a" r))))
                     (launch "cmd" "/c" "start" "" url))))
              ((member :darwin *features*) (launch "open" url))
              ((or (uiop:getenv "DISPLAY") (uiop:getenv "WAYLAND_DISPLAY")) (launch "xdg-open" url))
              (t (values nil "no display: neither DISPLAY nor WAYLAND_DISPLAY is set")))
      (error (e) (values nil (princ-to-string e))))))

(defun cl-user::desk-ready (address rfb-port web-port)
  "The network thread has bound its ports: say where they are, and open a browser on the web one."
  (let ((host (if (string= address "0.0.0.0") "127.0.0.1" address)))
    (format t "~&desk: VNC viewers: ~a:~d~%" host rfb-port)
    (when web-port
      (let ((url (format nil "http://~a:~d/" (if (find #\: host) (format nil "[~a]" host) host) web-port)))
        (format t "~&desk: in a web browser: ~a~%" url)
        (cond ((getf cl-user::*desk-options* :no-open))
              ((not (cl-user::desk-loopback-p address))
               (format t "~&desk: (not opening a browser: ~a is not this machine's loopback)~%" address))
              (t (multiple-value-bind (ok why) (cl-user::desk-open-url url)
                   (unless ok (format t "~&desk: (not opening a browser: ~a)~%" why)))))))
    (finish-output)))

;;; ---- serving -------------------------------------------------------------------------------------

(if cl-user::*desk-lean*
    (progn
      (when (getf cl-user::*desk-options* :web-port)
        (format t "~&desk: --web-port is the window manager's; the lean desk serves VNC only~%"))
      (load (merge-pathnames "glass/demo/desk-run.lisp" cl-user::*desk-tree*)))
    (let ((port (cl-user::desk-port (cl-user::desk-option :port) "--port"))
          (web-port (and (not (getf cl-user::*desk-options* :no-web))
                         (cl-user::desk-port (cl-user::desk-option :web-port) "--web-port")))
          (address (cl-user::desk-option :address)))
      (multiple-value-bind (w h size) (cl-user::desk-size)
        (format t "~&desk: the window manager, ~a on ~a~%" size address)
        (finish-output)
        ;; every app the loaded systems offer, but Terminal: a shell on the host is not this
        ;; desk's to hand out
        (funcall (find-symbol "SERVE-WM" "GLASS.WM") port
                 :width w :height h :address address :web-port web-port
                 :on-ready (lambda (rfb web) (cl-user::desk-ready address rfb web))
                 :apps (remove "Terminal" (funcall (find-symbol "WM-AVAILABLE-APPS" "GLASS.WM"))
                               :key #'first :test #'string=)))))
