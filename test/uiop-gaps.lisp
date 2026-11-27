;;;; test/uiop-gaps.lisp -- the UIOP names org code calls that modus lacked (2026-11-27).
;;;; Prints one line per check; test/run-uiop-gaps.sh requires the output to be
;;;; IDENTICAL to SBCL's (SBCL's real UIOP is the oracle).  Run with a scratch
;;;; directory as the first argument after the script.
(require :asdf)
(defvar *dir* (uiop:ensure-directory-pathname
               (car (last (uiop:command-line-arguments)))))
(defun line (name v) (format t "~a: ~s~%" name v))
(defun rel (p) (enough-namestring p *dir*))

;; files
(let ((a (merge-pathnames "a.txt" *dir*)) (b (merge-pathnames "b.txt" *dir*)))
  (uiop:with-output-file (s a :if-exists :supersede) (write-string "alpha" s))
  (uiop:with-output-file (s b :if-exists :supersede) (write-string "beta" s))
  (line "with-output-file :if-exists :error"
        (handler-case (uiop:with-output-file (s a) (write-string "x" s)) (error () :refused)))
  (uiop:copy-file a (merge-pathnames "c.txt" *dir*))
  (line "copy-file" (uiop:read-file-string (merge-pathnames "c.txt" *dir*)))
  (uiop:rename-file-overwriting-target a b)
  (line "rename-file-overwriting-target" (list (probe-file a) (uiop:read-file-string b)))
  (with-open-file (in b) (line "slurp-stream-string" (uiop:slurp-stream-string in))))

(let ((kept nil))
  (uiop:with-temporary-file (:pathname p :stream s :keep t :directory *dir*)
    (write-string "temp" s)
    (setq kept p))
  (line "with-temporary-file :keep t" (uiop:read-file-string kept))
  (delete-file kept)
  (let ((seen nil))
    (uiop:with-temporary-file (:pathname p :directory *dir*) (setq seen p))
    (line "with-temporary-file deletes" (probe-file seen))))

;; pathnames
(line "directory-pathname-p dir" (and (uiop:directory-pathname-p #p"/a/b/") t))
(line "directory-pathname-p file" (uiop:directory-pathname-p #p"/a/b"))
(line "subpathp in" (namestring (uiop:subpathp #p"/a/b/c.txt" #p"/a/")))
(line "subpathp out" (uiop:subpathp #p"/x/c.txt" #p"/a/"))
(line "parse-unix-namestring" (namestring (uiop:parse-unix-namestring "x/y/z.lisp")))
(line "parse-unix-namestring dir" (namestring (uiop:parse-unix-namestring "x/y" :ensure-directory t)))
(line "default-temporary-directory" (and (pathnamep (uiop:default-temporary-directory)) t))
(line "os-macosx-p" (uiop:os-macosx-p))
(line "xdg-cache-home" (namestring (uiop:xdg-cache-home "app" "f.txt")))
(line "xdg-config-pathnames" (mapcar #'namestring (uiop:xdg-config-pathnames "app")))
(let ((here (uiop:getcwd)))
  (uiop:with-current-directory (*dir*)
    (line "with-current-directory" (equal (namestring (uiop:getcwd)) (namestring *dir*))))
  (line "with-current-directory restores" (equal (uiop:getcwd) here)))

;; variables and hooks
(line "*compile-file-warnings-behaviour*" uiop:*compile-file-warnings-behaviour*)
(line "*compile-file-failure-behaviour*" uiop:*compile-file-failure-behaviour*)
(line "call-image-restore-hook" (progn (uiop:call-image-restore-hook) :ran))
(line "command-line-arguments" (mapcar #'rel (uiop:command-line-arguments)))

;; define-package
(uiop:define-package :uiop-gaps-a (:use :cl) (:export #:f))
(defun uiop-gaps-a:f () :fa)
(uiop:define-package :uiop-gaps-b (:use :cl) (:mix :uiop-gaps-a) (:reexport :uiop-gaps-a))
(line "define-package :reexport" (funcall (find-symbol "F" :uiop-gaps-b)))
(line "define-package exports" (nth-value 1 (find-symbol "F" :uiop-gaps-b)))

;; launch-program with streams
(let ((p (uiop:launch-program (list "cat") :input :stream :output :stream)))
  (write-line "one line" (uiop:process-info-input p))
  (close (uiop:process-info-input p))
  (line "launch-program :stream" (read-line (uiop:process-info-output p) nil :eof))
  (line "launch-program :stream eof" (read-line (uiop:process-info-output p) nil :eof))
  (line "wait-process" (uiop:wait-process p)))
(let ((p (uiop:launch-program "echo out; echo err >&2; exit 3" :output :stream :error-output :stream)))
  (line "process-info-error-output" (read-line (uiop:process-info-error-output p) nil :eof))
  (line "process-info-output" (read-line (uiop:process-info-output p) nil :eof))
  (line "exit code" (uiop:wait-process p)))
