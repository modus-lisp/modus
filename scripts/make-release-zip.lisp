;;;; make-release-zip.lisp -- a release as one ordinary zip: the three CLIs, a desk and a REPL
;;;; launcher for each system, and every modus-lisp repository's source.
;;;;
;;;;   sbcl --script scripts/make-release-zip.lisp --release 20261127 --tree DIR/ \
;;;;        --linux modus-linux-x64 --windows modus-windows-x64.exe --macos modus-macos-arm64 \
;;;;        [--out modus-20261127.zip] [--systems "SYS ..." | --exclude "REPO ..."]
;;;;   (or modus --script; the output is the same bytes)
;;;;
;;;; --bin DIR names the three CLIs at once (DIR/modus-linux-x64, ...).  The environment
;;;; variables make-source-tar.lisp reads stand in for flags not given: MODUS_SRC_TREE for
;;;; --tree, MODUS_SRC_SYSTEMS, MODUS_SRC_EXCLUDE.  The zip holds, under modus-RELEASE/:
;;;;
;;;;   README.txt                       scripts/release/README.txt, @RELEASE@ filled in
;;;;   desk.cmd desk.command desk.sh    scripts/release/: the desk (src/modus/scripts/
;;;;   repl.cmd repl.command repl.sh      glass-desk.lisp) and the REPL, per system
;;;;   modus-linux-x64 modus-macos-arm64 modus-windows-x64.exe
;;;;   src/REPO/...                     the source set (scripts/source-set.lisp, shared with
;;;;                                    make-source-tar.lisp): every repository in TREE less
;;;;                                    dist, or with --systems what those systems need
;;;;
;;;; TREE holds the modus-lisp repositories side by side as `git archive` exports them: every
;;;; file under it is a candidate, and a file keeps its execute bits (0755, else 0644).  The
;;;; zip is written by cram's ZIP writer (cram is loaded from TREE, or --cram DIR), so its
;;;; bytes depend only on the inputs: entries in this order, each stamped 1980-01-01.  It is
;;;; written to OUT.partial and renamed over OUT when complete.

(defvar *here* (make-pathname :name nil :type nil :version nil
                              :defaults (or *load-truename* *load-pathname*)))
(load (merge-pathnames "source-set.lisp" *here*))
#+sbcl (require :sb-posix)

(defun die (fmt &rest args)
  (format *error-output* "~&make-release-zip: ~?~%" fmt args)
  (finish-output *error-output*)
  (funcall (or (find-symbol "EXIT" "SB-EXT") (find-symbol "QUIT" "UIOP")) :code 2))

;;; --- the command line -----------------------------------------------------------------

(defparameter *flags* '("--release" "--tree" "--linux" "--windows" "--macos" "--bin" "--out"
                        "--systems" "--exclude" "--cram"))

(defun parse-args (words)
  (let ((opts nil))
    (loop while words
          do (let ((w (pop words)))
               (unless (member w *flags* :test #'string=) (die "unknown argument ~s" w))
               (unless words (die "~a needs a value" w))
               (push (cons w (pop words)) opts)))
    opts))

(defvar *opts* (parse-args (rest (symbol-value (find-symbol "*POSIX-ARGV*" "SB-EXT")))))

(defun opt (flag &optional env default)
  (or (cdr (assoc flag *opts* :test #'string=)) (and env (env env)) default))

(defvar *release* (or (opt "--release") (die "--release NAME is required")))
(defvar *tree* (tree-directory (or (opt "--tree" "MODUS_SRC_TREE") (die "--tree DIR is required"))))
(defvar *top* (format nil "modus-~a/" *release*))
(defvar *out* (opt "--out" nil (format nil "modus-~a.zip" *release*)))

(defun cli (flag name)
  (let ((f (or (opt flag)
               (and (opt "--bin") (concatenate 'string (tree-directory (opt "--bin")) name))
               (die "~a FILE (or --bin DIR) is required" flag))))
    (unless (probe-file f) (die "~a: no file ~a" flag f))
    f))

(defvar *clis* (list (list "modus-linux-x64" (cli "--linux" "modus-linux-x64"))
                     (list "modus-macos-arm64" (cli "--macos" "modus-macos-arm64"))
                     (list "modus-windows-x64.exe" (cli "--windows" "modus-windows-x64.exe"))))

;;; --- what goes in ---------------------------------------------------------------------

(defun file-mode (path)
  "0755 for a file with any execute bit, else 0644."
  (let ((mode (funcall (find-symbol "STAT-MODE" "SB-POSIX")
                       (funcall (find-symbol "STAT" "SB-POSIX") (namestring path)))))
    (if (logtest mode #o111) #o755 #o644)))

(defun release-file (name) (merge-pathnames (concatenate 'string "release/" name) *here*))

(defun read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s)
      b)))

(defun readme ()
  "scripts/release/README.txt with @RELEASE@ replaced: ASCII in, octets out."
  (let* ((text (map 'string #'code-char (read-octets (release-file "README.txt"))))
         (at (search "@RELEASE@" text))
         (s (if at (concatenate 'string (subseq text 0 at) *release* (subseq text (+ at 9))) text)))
    (map '(simple-array (unsigned-byte 8) (*)) #'char-code s)))

(defparameter *launchers*
  ;; (NAME MODE)
  '(("desk.cmd" #o644) ("desk.command" #o755) ("desk.sh" #o755)
    ("repl.cmd" #o644) ("repl.command" #o755) ("repl.sh" #o755)))

;;; --- run ----------------------------------------------------------------------------

(defvar *t0* (get-internal-real-time))
(defun elapsed () (/ (- (get-internal-real-time) *t0*) (float internal-time-units-per-second)))

(defvar *repos* (source-repos *tree* :all (not (opt "--systems" "MODUS_SRC_SYSTEMS"))
                                     :systems (opt "--systems" "MODUS_SRC_SYSTEMS" *default-systems*)
                                     :exclude (opt "--exclude" "MODUS_SRC_EXCLUDE" "dist")))
(defvar *names* (source-files *tree* *repos*))
(format t "~&make-release-zip: ~d repositories, ~d files (~,1fs)~%"
        (length *repos*) (length *names*) (elapsed))
(finish-output)

(if (opt "--cram")
    (push (truename (tree-directory (opt "--cram"))) asdf:*central-registry*))
(asdf:load-system "cram")
;; modus: native before the long calls (a DEFUN goes native on its next call)
#+modus (funcall (find-symbol "JIT-EAGER" "MODUS"))

(defun cram (name &rest args) (apply (find-symbol name "CRAM") args))

(let ((partial (concatenate 'string *out* ".partial"))
      (dirs (make-hash-table :test 'equal))
      (files 0) (bytes 0))
  (with-open-file (s partial :direction :output :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
    (let ((zw (cram "MAKE-ZIP-WRITER" s)))
      (labels ((dir (name)                     ; NAME ends in /; its parents first
                 (unless (gethash name dirs)
                   (let ((up (position #\/ name :end (1- (length name)) :from-end t)))
                     (when up (dir (subseq name 0 (1+ up)))))
                   (setf (gethash name dirs) t)
                   (cram "ZIP-ADD-DIRECTORY" zw name)))
               (add (name source mode)
                 (let ((slash (position #\/ name :from-end t)))
                   (when slash (dir (subseq name 0 (1+ slash)))))
                 (cram "ZIP-ADD-FILE" zw name source :mode mode)
                 (incf files)
                 (incf bytes (if (typep source '(or string pathname))
                                 (with-open-file (i source :element-type '(unsigned-byte 8))
                                   (file-length i))
                                 (length source)))))
        (dir *top*)
        (add (concatenate 'string *top* "README.txt") (readme) #o644)
        (dolist (l *launchers*)
          (add (concatenate 'string *top* (first l)) (release-file (first l)) (second l)))
        (dolist (c *clis*)
          (add (concatenate 'string *top* (first c)) (second c) #o755)
          (format t "~&make-release-zip: ~a (~,1fs)~%" (first c) (elapsed)) (finish-output))
        (dolist (n *names*)
          (let ((path (concatenate 'string *tree* n)))
            (add (concatenate 'string *top* "src/" n) path (file-mode path))))
        (format t "~&make-release-zip: ~d files, ~d directories, ~:d bytes in (~,1fs)~%"
                files (hash-table-count dirs) bytes (elapsed))
        (let ((size (cram "ZIP-FINISH" zw)))
          (format t "~&make-release-zip: ~a, ~:d bytes~%" *out* size)))))
  ;; RENAME-FILE merges a relative new name with the old file's directory: name it absolutely
  (let ((done (make-pathname :name (pathname-name *out*) :type (pathname-type *out*)
                             :version nil :defaults (truename partial))))
    (when (probe-file done) (delete-file done))
    (rename-file partial done))
  (format t "~&make-release-zip: done (~,1fs)~%" (elapsed)))
