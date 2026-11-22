;;;; image-config.lisp - HOST ONLY.  What an image was built with, written into it.
;;;;
;;;; Modus is built by dozens of scripts for nine architectures and several boards,
;;;; steered by ~100 MODUS_* environment variables.  Without this, "is this image
;;;; threaded? which NIC driver? static keywords?" is answered by reading build
;;;; scripts and guessing which environment the build ran in.  With it, the image
;;;; says:  (modus:configuration)  -- see lib/configuration.lisp, the image half.
;;;;
;;;; HOW.  Loading this file wraps SB-EXT:POSIX-GETENV so every MODUS_* variable a
;;;; build READS is recorded with the value it got (NIL: unset, i.e. the default).
;;;; That is the knob list by construction: a knob nobody reads is not recorded, and a
;;;; knob added tomorrow is recorded without anyone listing it.  IMAGE-CONFIGURATION-
;;;; TEXT turns the record into one DEFUN of quoted data, which the build appends to
;;;; the image's source -- as SOURCE, so the self-hosted compiler (the DDC) compiles
;;;; the same text SBCL did.
;;;;
;;;; REPRODUCIBLE.  The record holds only inputs: knob values, the script, the source
;;;; commit.  No time, host name, host Lisp version or absolute path.  Output-path
;;;; knobs (where the image is written, logs, dumps) are recorded as set but not by
;;;; value: they do not change the image, and two checkouts differ in them.
;;;; MODUS_SOURCE_COMMIT pins the commit field (scripts/build-matrix.sh sets it, so a
;;;; pair of trees can still be proved byte-identical); unset, it is `git rev-parse
;;;; HEAD' and whether the tree has uncommitted changes to tracked files.
;;;;
;;;; KNOBS READ AFTER THE SOURCE IS ASSEMBLED (an output path, a translator option
;;;; read while compiling) are not in the record: it is written before compiling.

(defpackage :modus.image-config
  (:use :common-lisp)
  (:export #:image-configuration-text #:finalize-image-configuration #:*knob-reads*
           #:getenv-unrecorded))
(in-package :modus.image-config)

(defvar *tree* (namestring (truename (merge-pathnames "../" (directory-namestring *load-truename*))))
  "The checkout this build reads its source from.")

(defvar *knob-reads* (make-hash-table :test 'equal)
  "MODUS_* variable name -> the value this build read (NIL: unset).")

(defun %record-knob-read (getenv name)
  (let ((v (funcall getenv name)))
    (when (and (stringp name) (> (length name) 6) (string= "MODUS_" name :end2 6))
      (setf (gethash name *knob-reads*) v))
    v))

#+sbcl
(unless (sb-int:encapsulated-p 'sb-ext:posix-getenv 'modus-knob-recorder)
  (sb-ext:without-package-locks
    (sb-int:encapsulate 'sb-ext:posix-getenv 'modus-knob-recorder #'%record-knob-read)))
;; ON MODUS (a build that runs on modus itself): SB-EXT:POSIX-GETENV is the shim's
;; ordinary function, so it is wrapped by replacing its definition -- once, the
;; wrapper remembering the original.
#-sbcl
(defvar *getenv-unwrapped* nil)
#-sbcl
(unless *getenv-unwrapped*
  (let ((orig (fdefinition 'sb-ext:posix-getenv)))
    (setq *getenv-unwrapped* orig)
    (setf (fdefinition 'sb-ext:posix-getenv)
          (lambda (name) (%record-knob-read orig name)))))

;; A knob that cannot change a byte of an image -- a build check's mode, a diagnostic
;; dump's path -- is read with this, and stays out of the record, which holds the
;; inputs of the image only.  (mvm/build-checks.lisp, mvm/fixed-addresses.lisp)
(defun getenv-unrecorded (name)
  #+sbcl
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "getenv" (function sb-alien:c-string sb-alien:c-string))
   name)
  #-sbcl
  (funcall (or *getenv-unwrapped* (function sb-ext:posix-getenv)) name))

(defparameter *image-configuration-flags*
  '((:aarch64 "MODUS-INT" "*AARCH64-GC-NATIVE-MCGC*")
    (:aarch64 "MODUS-INT" "*A64-TLS-WINDOW*")
    (:aarch64 "MODUS-INT" "*AARCH64-YIELD-NOP*")
    (:aarch64 "MODUS-INT" "*AARCH64-SETUP-IRQ-ENABLE*")
    (:aarch64 "MODUS-INT" "*AARCH64-FIXPOINT-THREADS*")
    (:aarch64 "MODUS-INT" "*AARCH64-LINUX-MODE*")
    (:aarch64 "MODUS-INT" "*AARCH64-SERIAL-BASE*")
    (:all "MODUS-INT" "*STATIC-SYMBOLS-P*")
    (:all "MODUS-INT" "*STATIC-KEYWORDS-P*")
    (:all "MODUS-INT" "*TLS-WINDOW*")
    (:x64 "MODUS-INT" "*PARK-SAFEPOINTS*")
    (:aarch64 "MODUS-INT" "*PARK-SAFEPOINTS*")
    (:x64 "MODUS.MVM.X64" "*X64-TLS-WINDOW*")
    (:x64 "MODUS.MVM.X64" "*MCGC-COLLECTOR-ENABLED*")
    (:x64 "MODUS.MVM.X64" "*MCGC-KIND-CHECK-ENABLED*")
    (:i386 "MODUS.MVM.I386" "*I386-LINUX-MODE*")
    (:i386 "MODUS.MVM.I386" "*I386-X87*")
    (:arm32 "MODUS-INT" "*ARM32-LINUX-MODE*")
    (:riscv "MODUS-INT" "*RISCV-64-BIT*"))
  "The emission flags worth reading in an image's configuration: (arch package name),
   ARCH :all or the architecture whose translator reads it (an image shows only its
   own).  A flag that does not exist in this build, or is unbound, is left out.  Add
   one when its value decides what code an image contains.")

(defun %target-arch (target)
  "The translator family of a BUILD-IMAGE target keyword."
  (let ((n (string-upcase (string target))))
    (cond ((or (search "AARCH64" n) (search "RPI" n) (search "FIXPOINT" n) (search "A64" n)) :aarch64)
          ((or (search "X64" n) (search "X86" n)) :x64)
          ((search "I386" n) :i386)
          ((search "ARM" n) :arm32)
          ((search "RISCV" n) :riscv)
          ((search "PPC" n) :ppc)
          ((search "68K" n) :68k)
          (t :unknown))))

(defun %knob-output-path-p (name)
  (flet ((has (s) (search s name)))
    (or (let ((n (length name))) (and (> n 4) (string= "_OUT" name :start2 (- n 4))))
        (has "DUMP") (has "SYMMAP") (has "INVENTORY") (has "_LOG") (has "WORK"))))

(defun %file-first-line (path)
  (with-open-file (f path :if-does-not-exist nil)
    (and f (let ((l (read-line f nil nil))) (and l (string-trim '(#\Space #\Tab #\Return) l))))))

(defun %git-dir ()
  "The tree's git directory: .git, or the directory a worktree's .git FILE names."
  (let* ((dot (concatenate 'string *tree* ".git"))
         (l (ignore-errors (%file-first-line dot))))
    (cond ((and l (> (length l) 8) (string= "gitdir: " l :end2 8))
           (let ((d (subseq l 8))) (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/"))))
          (t (concatenate 'string dot "/")))))

(defun %git-head-from-files ()
  "HEAD's commit read from the git directory's own files -- HEAD, the loose ref, then
   packed-refs (a worktree's refs live in its common directory) -- for a build with no
   git to run: a modus image, or a browser.  \"\" when it cannot be found."
  (or (ignore-errors
       (let* ((gd (%git-dir))
              (head (%file-first-line (concatenate 'string gd "HEAD"))))
         (if (and head (> (length head) 5) (string= "ref: " head :end2 5))
             (let* ((ref (subseq head 5))
                    (common (let ((c (%file-first-line (concatenate 'string gd "commondir"))))
                              (if c (if (char= (char c 0) #\/) (concatenate 'string c "/")
                                        (concatenate 'string gd c "/"))
                                  gd))))
               (or (%file-first-line (concatenate 'string gd ref))
                   (%file-first-line (concatenate 'string common ref))
                   (with-open-file (f (concatenate 'string common "packed-refs") :if-does-not-exist nil)
                     (and f (loop for l = (read-line f nil nil) while l
                                  when (and (> (length l) 41) (string= ref l :start2 41))
                                    return (subseq l 0 40))))))
             head)))
      ""))

(defun %git-line (&rest args)
  #-sbcl (if (equal args '("rev-parse" "HEAD")) (%git-head-from-files) "")
  #+sbcl
  (handler-case
      (let* ((s (with-output-to-string (o)
                  (sb-ext:run-program "git" (list* "-C" *tree* args)
                                      :search t :output o :error nil)))
             (nl (position #\Newline s)))
        (if nl (subseq s 0 nl) s))
    (error () "")))

(defun %image-source-commit ()
  (let ((pin (sb-ext:posix-getenv "MODUS_SOURCE_COMMIT")))
    (if (and pin (plusp (length pin)))
        (list pin nil)
        (list (%git-line "rev-parse" "HEAD")
              (plusp (length (%git-line "status" "--porcelain" "--untracked-files=no")))))))

(defun %image-build-script ()
  "The build script, relative to the tree: the first argument naming a .lisp file
   under mvm/, from /proc/self/cmdline (SBCL's *POSIX-ARGV* drops --script's file).
   A host driver that LOADs the script itself (scripts/build-ccl.sh, build-abcl-driver)
   names it in CL-USER::*IMAGE-BUILD-SCRIPT*, so every host records the same script."
  (let ((v (find-symbol "*IMAGE-BUILD-SCRIPT*" "COMMON-LISP-USER")))
    (when (and v (boundp v) (stringp (symbol-value v)))
      (return-from %image-build-script (symbol-value v))))
  (let ((args (handler-case
                  (with-open-file (f "/proc/self/cmdline")
                    (let ((acc nil) (cur (make-string-output-stream)))
                      (loop for c = (read-char f nil nil)
                            while c
                            do (if (char= c (code-char 0))
                                   (push (get-output-stream-string cur) acc)
                                   (write-char c cur)))
                      (nreverse acc)))
                (error () sb-ext:*posix-argv*))))
    (or (dolist (a args)
          (let ((p (search "mvm/" a)))
            (when (and p (> (length a) 5) (string= ".lisp" a :start2 (- (length a) 5)))
              (return (subseq a p)))))
        "unknown")))

(defun %source-hash (text start end)
  "Two 32-bit FNV-1a lanes over TEXT outside [START, END), as one hex string:
   the image's whole source, so two builds with one configuration but different
   code differ here.  The FASL cache keys on it (mvm/fasl.lisp %FASL-BUILD-KEY):
   a module's function-table indices are this image's, not the next build's."
  (let ((a 2166136261) (b 3735928559))
    (declare (type (unsigned-byte 32) a b))
    (flet ((run (from to)
             (loop for i from from below to
                   for k = (char-code (char text i))
                   do (setf a (logand (* (logxor a k) 16777619) #xFFFFFFFF))
                      (setf b (logand (* (logxor b (logand (+ k 41) #xFFFF)) 16777619) #xFFFFFFFF)))))
      ;; and outside the build id: %CORE-BUILD-ID is drawn fresh per build
      ;; (build-cli-common; MODUS_BUILD_ID pins it), and the cache should be
      ;; shared by two builds of one source.  It names the binary for a saved
      ;; core, which must restore under the binary that wrote it; a FASL is
      ;; portable across them.
      (let* ((tag "(defun %core-build-id () ")
             (q (search tag text))
             (q-end (and q (position #\) text :start (+ q (length tag))))))
        (if (and q q-end (or (< q-end start) (>= q end)))
            (let ((cuts (sort (list (cons start end) (cons q (+ q-end 1))) #'< :key #'car)))
              (run 0 (car (first cuts)))
              (run (cdr (first cuts)) (car (second cuts)))
              (run (cdr (second cuts)) (length text)))
            (progn (run 0 start)
                   (run end (length text))))))
    (format nil "~8,'0x~8,'0x" a b)))

(defun %image-configuration-record (target &optional source-hash)
  (let* ((arch (%target-arch target))
         (commit (%image-source-commit))
         (knobs (let ((acc nil))
                  (maphash (lambda (k v)
                             (push (list k (cond ((null v) nil)
                                                 ((%knob-output-path-p k) :output-path)
                                                 (t v)))
                                   acc))
                           *knob-reads*)
                  (sort acc #'string< :key #'first)))
         (flags (let ((acc nil))
                  (dolist (f *image-configuration-flags*)
                    (when (or (eq (first f) :all) (eq (first f) arch))
                      (let* ((p (find-package (second f)))
                             (s (and p (find-symbol (third f) p))))
                        (when (and s (boundp s))
                          (let ((v (symbol-value s)))
                            (push (list (third f) (if (typep v '(or null (eql t) integer keyword string))
                                                      v
                                                      (princ-to-string v)))
                                  acc))))))
                  (nreverse acc))))
    (list :script (%image-build-script) :target (string-upcase (string target))
          :commit (first commit) :uncommitted-changes (second commit)
          :knobs knobs :flags flags :source-hash source-hash)))

;;; THE PLACEHOLDER.  The assembly appends IMAGE-CONFIGURATION-TEXT before the build
;;; script has finished setting knobs and flags; FINALIZE-IMAGE-CONFIGURATION, run by
;;; the host's BUILD-IMAGE wrapper (mvm/build-checks.lisp) and by the DDC's source
;;; dump, replaces it with the record as it stands when compiling starts.  Text with
;;; no placeholder (an already finalized dump) is returned unchanged.
(defparameter *placeholder* "'(:RECORD :PENDING)")

(defun image-configuration-text ()
  "Source text of MODUS-INT::%IMAGE-BUILD-CONFIGURATION, to be finalized."
  (format nil "~%(defun modus-int::%image-build-configuration ()~%  ~A)~%" *placeholder*))

(defun finalize-image-configuration (text target)
  (let ((p (and (stringp text) (search *placeholder* text))))
    (if (null p)
        text
        (concatenate 'string (subseq text 0 p) "'"
                     (with-standard-io-syntax
                       (let ((*package* (find-package "COMMON-LISP-USER")) (*print-readably* nil))
                         (prin1-to-string
                          (%image-configuration-record
                           target (%source-hash text p (+ p (length *placeholder*)))))))
                     (subseq text (+ p (length *placeholder*)))))))
