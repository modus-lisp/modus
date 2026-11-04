;;;; test/uiop-delete-directory-tree.lisp -- UIOP:DELETE-DIRECTORY-TREE.
;;;;
;;;;   ./modus --script test/uiop-delete-directory-tree.lisp
;;;;
;;;; It signalled "not implemented" on modus; cl-payments' daemon gate cleans up
;;;; its node directories with it.  Checks the tree goes (files, nested
;;;; directories, the root), :IF-DOES-NOT-EXIST :IGNORE, and both refusals UIOP
;;;; makes (no :VALIDATE, the root directory).

(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok    ~a~%" name)
      (progn (incf *fails*) (format t "FAIL  ~a: got ~s want ~s~%" name got want))))

(defvar *root* (format nil "/tmp/uiop-ddt-~d/" (random 1000000000 (make-random-state t))))
(ensure-directories-exist (concatenate 'string *root* "a/b/c/"))
(dolist (f '("top.txt" "a/one.txt" "a/b/two.txt" "a/b/c/three.txt"))
  (with-open-file (s (concatenate 'string *root* f) :direction :output :if-exists :supersede)
    (write-line f s)))
(check "the tree is there" (and (probe-file (concatenate 'string *root* "a/b/c/three.txt")) t) t)
(check "refused without :VALIDATE"
       (handler-case (progn (uiop:delete-directory-tree (pathname *root*)) :deleted) (error () :refused))
       :refused)
(check "refused for the root directory"
       (handler-case (progn (uiop:delete-directory-tree #p"/" :validate t) :deleted) (error () :refused))
       :refused)
(check "deleted (returns the directory)"
       (and (uiop:delete-directory-tree (pathname *root*) :validate t) t) t)
(check "nothing left" (probe-file *root*) nil)
(check ":if-does-not-exist :ignore"
       (uiop:delete-directory-tree (pathname *root*) :validate t :if-does-not-exist :ignore) nil)
(check "a missing directory is an error by default"
       (handler-case (progn (uiop:delete-directory-tree (pathname *root*) :validate t) :no-error)
         (error () :error))
       :error)

(format t "~&uiop-delete-directory-tree: ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
