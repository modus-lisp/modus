;;;; make-source-tar.lisp -- the sources modus.com carries: what it uses, and nothing else.
;;;;
;;;;   MODUS_SRC_TREE=DIR/ MODUS_SRC_OUT=src.tar.z  modus --script scripts/make-source-tar.lisp
;;;;   (or sbcl --script; the output is the same bytes)
;;;;
;;;; DIR holds the modus-lisp repositories side by side, as exported (`git archive`, or the
;;;; release's source tarball unpacked): every file under it is a candidate, so it must be a
;;;; clean export, not a working checkout.  The output is a tar of DIR's repositories that
;;;;   - the systems in MODUS_SRC_SYSTEMS need (default: the window manager, cram and the disk
;;;;     writers), by ASDF's dependency closure, plus modus, natrium and vessel themselves;
;;;;   - less what nothing loads: images, video and PDFs, inspect/ (screenshots and corpora)
;;;;     and modus's test/ladder/tars/;
;;;; with entries sorted by name, owner 0, mode 644 and mtime 0, so the bytes depend only on
;;;; the files -- then zlib-compressed with cram (which loads from DIR too).  MODUS_SRC_TAR=FILE
;;;; also writes the uncompressed tar.

(require :asdf)

(defun env (name &optional default)
  (let ((v (funcall (find-symbol "POSIX-GETENV" "SB-EXT") name)))
    (if (and v (plusp (length v))) v default)))

(defparameter *tree* (let ((d (env "MODUS_SRC_TREE")))
                       (unless d (error "make-source-tar: set MODUS_SRC_TREE"))
                       (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/"))))

(defparameter *roots*
  (let ((s (env "MODUS_SRC_SYSTEMS" "glass/wm cram fs-fat fs-apple")) (acc nil) (start 0))
    (loop (let ((sp (position #\Space s :start start)))
            (when (> (or sp (length s)) start) (push (subseq s start sp) acc))
            (if sp (setq start (1+ sp)) (return))))
    (nreverse acc)))

;; natrium too: building modus itself needs it (mvm/build-macos-cli.lisp's code signature)
(defparameter *always* '("modus" "natrium" "vessel"))

(defparameter *drop-types* '("pdf" "png" "jpg" "jpeg" "gif" "mp4" "webm" "mkv" "vob" "mov"))
(defparameter *drop-dirs* '("inspect" ".git" ".github"))
(defparameter *drop-paths* '("modus/test/ladder/tars/"))

;;; --- which repositories ------------------------------------------------------------

(setf asdf:*central-registry* (directory (concatenate 'string *tree* "*/")))

(defun repo-of (system)
  "The repository directory name under *TREE* that holds SYSTEM, or NIL for a system from
   elsewhere (the host's own ASDF and UIOP)."
  (let ((dir (namestring (asdf:system-source-directory system)))
        (root (namestring (truename *tree*))))
    (when (eql 0 (search root dir))
      (let ((rel (subseq dir (length root))))
        (subseq rel 0 (position #\/ rel))))))

(defun dep-name (d)
  (cond ((stringp d) d)
        ((symbolp d) (string-downcase d))
        ((and (consp d) (member (car d) '(:version :require))) (and (stringp (second d)) (second d)))
        ((and (consp d) (eq (car d) :feature)) (dep-name (third d)))))

(defun closure-repos ()
  (let ((seen (make-hash-table :test 'equal)) (repos (make-hash-table :test 'equal)))
    (labels ((walk (name)
               (unless (or (null name) (gethash name seen))
                 (setf (gethash name seen) t)
                 (let ((s (ignore-errors (asdf:find-system name nil))))
                   (when (and s (repo-of s))
                     (setf (gethash (repo-of s) repos) t)
                     (dolist (d (append (asdf:system-depends-on s) (asdf:system-defsystem-depends-on s)))
                       (walk (dep-name d))))))))
      (dolist (r *roots*) (walk r)))
    (dolist (r *always*) (setf (gethash r repos) t))
    (sort (loop for k being the hash-keys of repos collect k) #'string<)))

;;; --- which files ---------------------------------------------------------------------

(defun keep-p (rel)
  "REL is a path under *TREE* (REPO/...)."
  (let ((type (let ((dot (position #\. rel :from-end t)) (slash (position #\/ rel :from-end t)))
                (and dot (or (null slash) (> dot slash)) (string-downcase (subseq rel (1+ dot)))))))
    (not (or (member type *drop-types* :test #'string=)
             (some (lambda (d) (search (concatenate 'string "/" d "/") rel)) *drop-dirs*)
             (some (lambda (p) (eql 0 (search p rel))) *drop-paths*)))))

(defun files-under (repo)
  "Every regular file of REPO, as paths relative to *TREE*."
  (let ((root (namestring (truename *tree*))) (acc nil))
    (labels ((walk (dir)
               (dolist (f (directory (concatenate 'string dir "*.*")))
                 (let ((n (namestring f)))
                   (unless (char= (char n (1- (length n))) #\/)
                     (push (subseq n (length root)) acc))))
               (dolist (d (directory (concatenate 'string dir "*/")))
                 (walk (namestring d)))))
      (walk (concatenate 'string root repo "/")))
    acc))

;;; --- a ustar writer --------------------------------------------------------------------

(defun read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s)
      b)))

(defun put-string (h off str len)
  (dotimes (i (min len (length str))) (setf (aref h (+ off i)) (char-code (char str i)))))

(defun put-octal (h off len n)
  "N in octal, zero-padded to LEN-1 digits and a NUL."
  (put-string h off (format nil "~v,'0O" (1- len) n) (1- len)))

(defun tar-header (name size)
  (let ((h (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
        (prefix "") (base name))
    (when (> (length name) 100)
      (let ((cut (position #\/ name :end (min (length name) 156) :from-end t)))
        (unless (and cut (<= (- (length name) cut 1) 100))
          (error "make-source-tar: path too long for ustar: ~A" name))
        (setq prefix (subseq name 0 cut) base (subseq name (1+ cut)))))
    (put-string h 0 base 100)
    (put-octal h 100 8 #o644) (put-octal h 108 8 0) (put-octal h 116 8 0)
    (put-octal h 124 12 size) (put-octal h 136 12 0)
    (fill h 32 :start 148 :end 156)                     ; checksum: spaces while summing
    (setf (aref h 156) (char-code #\0))
    (put-string h 257 "ustar" 6) (put-string h 263 "00" 2)
    (put-string h 345 prefix 155)
    (put-string h 148 (format nil "~6,'0O" (reduce #'+ h)) 6)
    (setf (aref h 154) 0 (aref h 155) 32)
    h))

(defun write-tar (names out)
  "A tar of NAMES (paths under *TREE*) as one octet vector, written to OUT if given."
  (let ((parts nil) (total 1024))
    (dolist (n names)
      (let ((data (read-octets (concatenate 'string *tree* n))))
        (push (tar-header (concatenate 'string "modus-lisp/" n) (length data)) parts)
        (push data parts)
        (incf total (+ 512 (* 512 (ceiling (length data) 512))))))
    (let ((tar (make-array total :element-type '(unsigned-byte 8) :initial-element 0)) (at 0))
      (dolist (p (nreverse parts))
        (replace tar p :start1 at)
        (incf at (if (= (length p) 512) 512 (* 512 (ceiling (length p) 512)))))
      (when out
        (with-open-file (s out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence tar s)))
      tar)))

;;; --- run ----------------------------------------------------------------------------

(let* ((repos (closure-repos))
       (names (sort (remove-if-not #'keep-p (loop for r in repos append (files-under r))) #'string<))
       (tar (write-tar names (env "MODUS_SRC_TAR"))))
  (format t "~&make-source-tar: ~D repositories (~{~A~^ ~}), ~D files, ~D bytes of tar~%"
          (length repos) repos (length names) (length tar))
  (asdf:load-system "cram")
  ;; modus: native before the one long ZLIB-COMPRESS call (a DEFUN goes native on its NEXT
  ;; call; there is no on-stack replacement) -- 116 minutes interpreted
  #+modus (funcall (find-symbol "JIT-EAGER" "MODUS"))
  (let ((z (funcall (find-symbol "ZLIB-COMPRESS" "CRAM") tar))
        (out (env "MODUS_SRC_OUT" "src.tar.z")))
    (with-open-file (s out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence z s))
    (format t "~&make-source-tar: ~A, ~D bytes~%" out (length z))))
