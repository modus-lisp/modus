;;;; ape-main.lisp -- what modus.com does when it is run with no options.
;;;;
;;;; scripts/make-ape.lisp puts this file in modus.com as the segment main.lisp, beside
;;;;   cram.lisp   cram's source, uncompressed: the inflater and the tar reader
;;;;   src.tar.z   every modus-lisp repository, a tar, zlib-compressed
;;;; and lib/cli-toplevel.lisp loads it when modus.com's first argument is not an option.
;;;; The sources are unpacked once per release into the cache (~/.cache/modus/ on Unix,
;;;; %LOCALAPPDATA%\modus\ on Windows); everything else is loaded from there.
;;;;
;;;;   modus.com              a menu
;;;;   modus.com desk         the glass desk over VNC (DESK_PORT, DESK_ADDRESS, DESK_SIZE)
;;;;   modus.com repl         the REPL (or run modus.com with any option: it is modus)
;;;;   modus.com sources      unpack the sources and print where they are
;;;;   modus.com help

(defpackage :modus-app (:use :cl))
(in-package :modus-app)

(defun getenv (name)
  (let ((v (funcall (find-symbol "POSIX-GETENV" "SB-EXT") name)))
    (and v (plusp (length v)) v)))

(defun say (fmt &rest args)
  (apply #'format t fmt args)
  (finish-output))

(defun words ()
  "The command words after the program's name."
  (cdr (symbol-value (find-symbol "*CLI-COMMAND-LINE*" "MODUS-INT"))))

(defun windows-p () (search "Windows" (software-version)))

(defun cache-root ()
  (let ((base (or (getenv "XDG_CACHE_HOME")
                  (and (getenv "LOCALAPPDATA") (substitute #\/ #\\ (getenv "LOCALAPPDATA")))
                  (and (getenv "HOME") (concatenate 'string (getenv "HOME") "/.cache"))
                  "/tmp")))
    (concatenate 'string base "/modus/")))

(defun segment-entry (name)
  (assoc name (modus:ape-segments) :test #'string=))

(defun octets-string (b) (map 'string #'code-char b))

;;; --- cram, from its own segment ------------------------------------------------

(defun ensure-cram ()
  (unless (find-package "CRAM")
    (let ((src (modus:ape-segment "cram.lisp")))
      (unless src (error "this modus.com has no cram.lisp segment"))
      (load (make-string-input-stream (octets-string src))))))

;;; --- the sources ---------------------------------------------------------------

(defun sources-dir ()
  "Where this release's sources are unpacked: named after the segment's place in the file,
   which changes with every release."
  (let ((e (segment-entry "src.tar.z")))
    (unless e (error "this modus.com carries no sources (no src.tar.z segment)"))
    (format nil "~Asrc-~(~X-~X~)/" (cache-root) (second e) (third e))))

(defun write-octets (path octets)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence octets s)))

(defun ensure-sources ()
  "The directory holding the repositories (modus/, glass/, ...), unpacked if need be."
  (let* ((dir (sources-dir))
         (done (concatenate 'string dir ".unpacked")))
    (unless (probe-file done)
      (ensure-cram)
      (say "~&modus.com: unpacking the sources into ~A (once)...~%" dir)
      (let* ((z (modus:ape-segment "src.tar.z"))
             (tar (funcall (find-symbol "ZLIB-DECOMPRESS" "CRAM") z))
             (n 0))
        (setq z nil)
        (funcall (find-symbol "TAR-DO-ENTRIES" "CRAM") tar
                 (lambda (name type content)
                   (when (and (= type 48) content)
                     (write-octets (concatenate 'string dir name) content)
                     (incf n))))
        (write-octets done (map '(vector (unsigned-byte 8)) #'char-code "ok"))
        (say "~&modus.com: ~D files~%" n)))
    (concatenate 'string dir "modus-lisp/")))

;;; --- the commands ----------------------------------------------------------------

(defun cmd-desk ()
  (let ((tree (ensure-sources)))
    (load (concatenate 'string tree "modus/scripts/glass-desk.lisp"))))

(defun cmd-repl ()
  (funcall (find-symbol "%CLI-REPL" "MODUS-INT") t))

(defun cmd-sources ()
  (say "~A~%" (ensure-sources)))

(defun cmd-help ()
  (say "~&modus.com -- modus for ~A~%~%~
  modus.com desk      the glass desk, over VNC on DESK_PORT (5910)~%~
  modus.com repl      the Lisp REPL~%~
  modus.com sources   unpack the sources (every modus-lisp repository) and say where~%~
  modus.com OPTIONS   with --eval, --load, --script ...: modus itself~%"
       (software-version)))

(defparameter *commands*
  '(("desk" cmd-desk "the glass desk, over VNC")
    ("repl" cmd-repl "the Lisp REPL")
    ("sources" cmd-sources "unpack the sources and say where")
    ("help" cmd-help "what modus.com can do")))

(defun run-command (word)
  (let ((c (assoc word *commands* :test #'string-equal)))
    (if c
        (funcall (second c))
        (progn (say "~&modus.com: no command ~S~%" word) (cmd-help)))))

(defun menu ()
  (say "~&modus.com~%~%")
  (loop for (name nil doc) in *commands* for i from 1
        do (say "  ~D  ~A~20T~A~%" i name doc))
  (say "~%choose: ")
  (let* ((line (read-line *standard-input* nil ""))
         (k (ignore-errors (parse-integer line))))
    (cond ((and k (<= 1 k (length *commands*))) (run-command (first (nth (1- k) *commands*))))
          ((plusp (length (string-trim " " line))) (run-command (string-trim " " line))))))

(let ((w (words)))
  (if w (run-command (first w)) (menu)))
