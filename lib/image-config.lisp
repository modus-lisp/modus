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
  (:export #:image-configuration-text #:finalize-image-configuration #:*knob-reads*))
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

(unless (sb-int:encapsulated-p 'sb-ext:posix-getenv 'modus-knob-recorder)
  (sb-ext:without-package-locks
    (sb-int:encapsulate 'sb-ext:posix-getenv 'modus-knob-recorder #'%record-knob-read)))

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

(defun %git-line (&rest args)
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
   under mvm/, from /proc/self/cmdline (SBCL's *POSIX-ARGV* drops --script's file)."
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

(defun %image-configuration-record (target)
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
          :knobs knobs :flags flags)))

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
                         (prin1-to-string (%image-configuration-record target))))
                     (subseq text (+ p (length *placeholder*)))))))
