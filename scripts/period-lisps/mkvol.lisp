;;;; mkvol.lisp APPS OUT.hfv [SIZE_MB] [START] -- a System volume holding one or more
;;;; period Lisps and/or Modus, set to launch START (default: the first of APPS) at
;;;; startup.
;;;;
;;;;   APPS   comma-separated: pearl, mcl, acl, modus     e.g.  mcl,modus
;;;;   START  which of APPS launches at boot               e.g.  modus
;;;;          ("none": none, the Finder alone)
;;;;
;;;; Environment:
;;;;   LISPS        base dir (default /mnt/data-fast/claude/rss/lisps): LISPS/sys6/... holds
;;;;                System 6.0.8, LISPS/x/... what build-disks.sh extracted
;;;;   SYSTEM       608 (default): System 6.0.8; startup app via the boot blocks and the
;;;;                volume's Finder info ("Set Startup").
;;;;                753: the System Folder of the user's System 7.5.3 image
;;;;                ($UPLOADS/857557ab-System7_5_3.img) -- System 6.0.8 refuses to boot on
;;;;                68040 Quadras.  System 7's Finder ignores "Set Startup", so a copy of
;;;;                START's application also goes into System Folder:Startup Items.
;;;;   MODUS_FORKS  directory with data and rsrc of the Modus application, as written by
;;;;                scripts/mac68k-chain.lisp DISK OUT.bin --forks DIR (required for modus)
;;;;   VOLNAME      the volume's name (default Macintosh HD)
;;;;   MODUS_DESKTOP  V,H: Modus on the desktop (System 7), its icon there, not in a folder
;;;;
;;;; HFS, AppleDouble: fs-apple, found in the workspace (WORKSPACE, default the
;;;; directory above this checkout).  It replaced mkvol.py (machfs), byte for byte.
(require :asdf)
(defvar *workspace*
  (let ((w (sb-ext:posix-getenv "WORKSPACE")))
    (if (and w (plusp (length w)))
        (pathname (if (char= (char w (1- (length w))) #\/) w (concatenate 'string w "/")))
        (merge-pathnames "../../../" (make-pathname :name nil :type nil :defaults *load-truename*)))))
(push (lambda (name)
        (let ((base (subseq name 0 (or (position #\/ name) (length name)))))
          (probe-file (merge-pathnames (format nil "~A/~A.asd" base base) *workspace*))))
      asdf:*system-definition-search-functions*)
(let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
  (asdf:load-system "fs-apple"))

(defpackage #:mkvol (:use #:cl #:fs-apple))
(in-package #:mkvol)

(defun env (name &optional default)
  (let ((v (sb-ext:posix-getenv name))) (if (and v (plusp (length v))) v default)))
(defun die (fmt &rest args) (format *error-output* "~&mkvol: ~?~%" fmt args) (sb-ext:exit :code 1 :abort t))
(defun octets (path)
  (with-open-file (i (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence v i) v)))
(defun split (s c) (loop with st = 0 for i = (position c s :start st) collect (subseq s st i) while i do (setq st (1+ i))))

(defun listing (dir)
  "DIR's entries, (NAME . directory-p), by name (Python's sorted(os.listdir()))."
  (let ((out nil))
    (sb-ext:map-directory (lambda (p)
                            (let* ((n (sb-ext:native-namestring p)) (dirp (char= (char n (1- (length n))) #\/))
                                   (n (string-right-trim "/" n)))
                              (push (cons (subseq n (1+ (position #\/ n :from-end t))) dirp) out)))
                          (sb-ext:parse-native-namestring (concatenate 'string dir "/"))
                          :directories t :files t :classify-symlinks nil)
    (sort out #'string< :key #'car)))

(defun add-tree (dst path)
  "A directory unar wrote with -k hidden (AppleDouble ._ files) into the folder DST."
  (loop for (n . dirp) in (listing path)
        unless (and (>= (length n) 2) (string= (subseq n 0 2) "._"))
          do (let ((p (concatenate 'string path "/" n)))
               (if dirp
                   (let ((f (make-hfs-folder))) (add-tree f p) (hfs-set dst n f))
                   (let ((f (make-hfs-file :data (octets p))) (ad (concatenate 'string path "/._" n)))
                     (when (probe-file (sb-ext:parse-native-namestring ad))
                       (let ((m (appledouble-read (octets ad))))
                         (setf (hfs-file-rsrc f) (getf m :rsrc) (hfs-file-type f) (getf m :type)
                               (hfs-file-creator f) (getf m :creator)
                               ;; not "inited": the Finder places it
                               (hfs-file-flags f) (logand (getf m :flags) (lognot #x0100)))))
                     (hfs-set dst n f))))))

(destructuring-bind (apps-arg out &optional (size "24") start-arg) (rest sb-ext:*posix-argv*)
  (let* ((b (env "LISPS" "/mnt/data-fast/claude/rss/lisps"))
         (up (env "UPLOADS" "/mnt/data-fast/claude/rss/romsrc/uploads"))
         (apps (split apps-arg #\,)) (size (parse-integer size))
         (startname (or start-arg (first apps)))
         (system (env "SYSTEM" "608"))
         (sysimg (cond ((string= system "608") (concatenate 'string b "/sys6/MacintoshSystem6.0.8HD.img"))
                       ((string= system "753") (concatenate 'string up "/857557ab-System7_5_3.img"))
                       (t (die "SYSTEM is 608 or 753"))))
         (src (hfs-read (octets sysimg)))
         (v (make-hfs-folder :name (env "VOLNAME" "Macintosh HD")))
         (starts nil))
    (hfs-set v "System Folder" (hfs-ref src "System Folder"))
    (dolist (which apps)
      (cond ((string= which "pearl")
             (let ((folder (make-hfs-folder)))
               (add-tree folder (concatenate 'string b "/x/pearl/pearl/Pearl Lisp/Pearl Lisp Disk 1"))
               (hfs-set v "Pearl" folder) (push (cons which '("Pearl" "Pearl")) starts)))
            ((string= which "acl")
             (let ((folder (make-hfs-folder)))
               (dolist (d '("acl1.dsk" "acl2.dsk"))
                 (loop for (k . x) in (hfs-folder-entries (hfs-read (octets (concatenate 'string b "/x/" d))))
                       do (hfs-set folder k x)))
               (hfs-set v "Allegro" folder) (push (cons which '("Allegro" "Allegro Common LISP")) starts)))
            ((string= which "mcl")
             (let ((folder (make-hfs-folder)))
               (add-tree folder (concatenate 'string b "/x/mcl/MCL 2.0"))
               (hfs-set v "MCL 2.0" folder) (push (cons which '("MCL 2.0" "MCL 2.0")) starts)))
            ((string= which "modus")
             (let* ((d (or (env "MODUS_FORKS") (die "MODUS_FORKS is required for modus")))
                    (f (make-hfs-file :type "APPL" :creator "MDUS" :flags #x2400   ; hasBundle, hasCustomIcon
                                      :data (octets (concatenate 'string d "/data"))
                                      :rsrc (octets (concatenate 'string d "/rsrc")))))
               (if (env "MODUS_DESKTOP")
                   ;; on the desktop (System 7's Desktop Folder), its icon at V,H
                   (destructuring-bind (x y) (mapcar #'parse-integer (split (env "MODUS_DESKTOP") #\,))
                     (setf (hfs-file-x f) x (hfs-file-y f) y)
                     (let ((folder (make-hfs-folder :flags #x4000)))
                       (hfs-set folder "Modus" f) (hfs-set v "Desktop Folder" folder)
                       (push (cons which '("Desktop Folder" "Modus")) starts)))
                   (let ((folder (make-hfs-folder)))
                     (hfs-set folder "Modus" f) (hfs-set v "Modus" folder)
                     (push (cons which '("Modus" "Modus")) starts)))))
            (t (die "unknown app ~S: pearl, mcl, acl, modus" which))))
    (let* ((start (cdr (assoc startname starts :test #'string=)))   ; START "none": just the Finder
           (app (and start (apply #'hfs-ref v start))))
      (when (and start (string/= system "608"))
        (let ((si (make-hfs-folder)))
          (hfs-set (hfs-ref v "System Folder") "Startup Items" si)
          (hfs-set si (car (last start))
                   (make-hfs-file :type (hfs-file-type app) :creator (hfs-file-creator app) :flags (hfs-file-flags app)
                                  :data (hfs-file-data app) :rsrc (hfs-file-rsrc app)))))
      ;; System 7: no Desktop DB.  An empty one is not a B-tree the Finder can use:
      ;; "The disk Macintosh cannot be used, because an error of type -127 occurred"
      ;; (fsDSIntErr) the moment the Finder came forward on a Quadra 950 under 7.5.3.
      ;; Without it the Finder builds its own.
      (let ((disk (hfs-write v :size (* size 1024 1024) :startapp start :desktopdb (string= system "608"))))
        (with-open-file (o (sb-ext:parse-native-namestring out) :direction :output
                                                                :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence disk o)))
      (if start
          (format t "~A System ~A apps ~{~A~^,~} start ~{~A~^/~} ~A ~A ~D ~D~%" out system apps start
                  (hfs-file-type app) (hfs-file-creator app) (length (hfs-file-data app)) (length (hfs-file-rsrc app)))
          (format t "~A System ~A apps ~{~A~^,~} no startup application~%" out system apps)))))
