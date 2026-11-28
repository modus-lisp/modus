;;;; source-set.lisp -- which modus-lisp sources a release carries.  Loaded by
;;;; scripts/make-source-tar.lisp (modus.com's src.tar.z) and scripts/make-release-zip.lisp
;;;; (the release zip's src/), so both carry the same files.
;;;;
;;;; TREE is a directory holding the modus-lisp repositories side by side, as exported (`git
;;;; archive`, or the release's sources unpacked): every file under it is a candidate, so it
;;;; must be a clean export, not a working checkout.  Two selections:
;;;;   - (SOURCE-REPOS TREE :ALL T): every repository in TREE, less EXCLUDE (default dist:
;;;;     the release binaries), so TREE must hold the org's repositories and nothing else;
;;;;   - (SOURCE-REPOS TREE :SYSTEMS ...): the repositories the systems need (default: the
;;;;     window manager, cram and the disk writers), by ASDF's dependency closure, plus
;;;;     modus, natrium and vessel themselves.
;;;; SOURCE-FILES then drops what nothing loads: images, video and PDFs, inspect/ (screenshots
;;;; and corpora) and modus's test/ladder/tars/, and sorts what is left by name, so the result
;;;; depends only on the files.  The selection functions run on SBCL and on modus alike.

(require :asdf)

(defun env (name &optional default)
  (let ((v (funcall (find-symbol "POSIX-GETENV" "SB-EXT") name)))
    (if (and v (plusp (length v))) v default)))

(defun words (s)
  "S split at spaces, empty words dropped."
  (let ((acc nil) (start 0))
    (loop (let ((sp (position #\Space s :start start)))
            (when (> (or sp (length s)) start) (push (subseq s start sp) acc))
            (if sp (setq start (1+ sp)) (return))))
    (nreverse acc)))

(defun tree-directory (d)
  "D as a directory namestring, ending in /."
  (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/")))

(defparameter *default-systems* "glass/wm cram fs-fat fs-apple")

;; natrium too: building modus itself needs it (mvm/build-macos-cli.lisp's code signature)
(defparameter *always* '("modus" "natrium" "vessel"))

(defparameter *drop-types* '("pdf" "png" "jpg" "jpeg" "gif" "mp4" "webm" "mkv" "vob" "mov"))
(defparameter *drop-dirs* '("inspect" ".git" ".github"))
(defparameter *drop-paths* '("modus/test/ladder/tars/"))

;;; --- which repositories ------------------------------------------------------------

(defun repo-of (system tree)
  "The repository directory name under TREE that holds SYSTEM, or NIL for a system from
   elsewhere (the host's own ASDF and UIOP)."
  (let ((dir (namestring (asdf:system-source-directory system)))
        (root (namestring (truename tree))))
    (when (eql 0 (search root dir))
      (let ((rel (subseq dir (length root))))
        (subseq rel 0 (position #\/ rel))))))

(defun dep-name (d)
  (cond ((stringp d) d)
        ((symbolp d) (string-downcase d))
        ((and (consp d) (member (car d) '(:version :require))) (and (stringp (second d)) (second d)))
        ((and (consp d) (eq (car d) :feature)) (dep-name (third d)))))

(defun closure-repos (tree roots)
  (let ((seen (make-hash-table :test 'equal)) (repos (make-hash-table :test 'equal)))
    (labels ((walk (name)
               (unless (or (null name) (gethash name seen))
                 (setf (gethash name seen) t)
                 (let ((s (ignore-errors (asdf:find-system name nil))))
                   (when (and s (repo-of s tree))
                     (setf (gethash (repo-of s tree) repos) t)
                     (dolist (d (append (asdf:system-depends-on s) (asdf:system-defsystem-depends-on s)))
                       (walk (dep-name d))))))))
      (dolist (r roots) (walk r)))
    (dolist (r *always*) (setf (gethash r repos) t))
    (sort (loop for k being the hash-keys of repos collect k) #'string<)))

(defun all-repos (tree exclude)
  (let ((root (namestring (truename tree))))
    (sort (loop for d in (directory (concatenate 'string root "*/"))
                for n = (let ((s (namestring d))) (subseq s (length root) (1- (length s))))
                unless (member n exclude :test #'string=) collect n)
          #'string<)))

(defun source-repos (tree &key all (systems *default-systems*) (exclude "dist"))
  "The repository names under TREE (a directory namestring) a release carries: ALL of them
   less the words of EXCLUDE, else those the words of SYSTEMS need.  Points ASDF at TREE's
   repositories and nowhere else, so the caller can load systems (cram) from there."
  (setf asdf:*central-registry* (directory (concatenate 'string tree "*/")))
  (if all (all-repos tree (words exclude)) (closure-repos tree (words systems))))

;;; --- which files ---------------------------------------------------------------------

(defun keep-p (rel)
  "REL is a path under the tree (REPO/...)."
  (let ((type (let ((dot (position #\. rel :from-end t)) (slash (position #\/ rel :from-end t)))
                (and dot (or (null slash) (> dot slash)) (string-downcase (subseq rel (1+ dot)))))))
    (not (or (member type *drop-types* :test #'string=)
             (some (lambda (d) (search (concatenate 'string "/" d "/") rel)) *drop-dirs*)
             (some (lambda (p) (eql 0 (search p rel))) *drop-paths*)))))

(defun files-under (tree repo)
  "Every regular file of REPO, as paths relative to TREE."
  (let ((root (namestring (truename tree))) (acc nil))
    (labels ((walk (dir)
               (dolist (f (directory (concatenate 'string dir "*.*")))
                 (let ((n (namestring f)))
                   (unless (char= (char n (1- (length n))) #\/)
                     (push (subseq n (length root)) acc))))
               (dolist (d (directory (concatenate 'string dir "*/")))
                 (walk (namestring d)))))
      (walk (concatenate 'string root repo "/")))
    acc))

(defun source-files (tree repos)
  "The files of REPOS a release carries, as paths relative to TREE (REPO/...), sorted."
  (sort (remove-if-not #'keep-p (loop for r in repos append (files-under tree r))) #'string<))
