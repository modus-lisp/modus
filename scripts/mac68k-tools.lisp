;;;; mac68k-tools.lisp -- what the Mac disk scripts share: fs-apple, files as
;;;; byte vectors, running a program, and assembling a .s file for its bytes and
;;;; symbols.  Loaded by scripts/mac68k-hd.lisp, mac68k-disk.lisp and the others
;;;; that replaced the Python ones; SBCL (the build host), for SB-EXT:RUN-PROGRAM.

(defpackage #:mac68k-tools
  (:use #:cl)
  (:export #:*scripts* #:kernel-source #:run #:read-bytes #:write-bytes #:pad-to #:assemble
           #:env #:env-hex #:argv #:load-cram #:load-fs-fat #:split-words #:basilisk-serial))
(in-package #:mac68k-tools)

(defparameter *scripts* (make-pathname :name nil :type nil :defaults *load-truename*)
  "This directory: scripts/.")

(defun env (name &optional default)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) v default)))

(defun env-hex (name &optional (default 0))
  (let ((v (env name))) (if v (parse-integer v :radix 16) default)))

(defun argv () (rest sb-ext:*posix-argv*))

(defun sibling-dir (repo var)
  "An org library, as vessel is found: $VAR, else .REPO/ in the tree, else a
   sibling checkout ../REPO."
  (let ((e (env var)))
    (if e
        (pathname (if (char= (char e (1- (length e))) #\/) e (concatenate 'string e "/")))
        (or (probe-file (merge-pathnames (format nil "../.~A/" repo) *scripts*))
            (probe-file (merge-pathnames (format nil "../../~A/" repo) *scripts*))
            (error "no ~A (set ~A, or check it out beside modus)" repo var)))))

;; fs-apple, with cram's CRC-16 first (its MacBinary header check)
(unless (find-package "CRAM")
  (let ((dir (sibling-dir "cram" "CRAM")))
    (load (merge-pathnames "src/packages.lisp" dir))
    (load (merge-pathnames "src/crc16.lisp" dir))))
(unless (find-package "FS-APPLE")
  (dolist (f '("package" "apm" "packbits" "resource-fork" "macbinary"))
    (load (merge-pathnames (format nil "lib/~A.lisp" f) (sibling-dir "fs-apple" "FS_APPLE")))))

(defun load-cram ()
  "cram's CRC-32 and LZMA (src/deflate.lisp, src/lzma.lisp), compiled."
  (unless (and (find-package "CRAM") (fboundp (find-symbol "LZMA-COMPRESS" "CRAM")))
    (let ((dir (sibling-dir "cram" "CRAM")))
      (unless (find-package "CRAM") (load (merge-pathnames "src/packages.lisp" dir)))
      (dolist (f '("deflate" "lzma"))
        (let ((fasl (compile-file (merge-pathnames (format nil "src/~A.lisp" f) dir)
                                  :output-file (format nil "/tmp/mac68k-cram-~A-~D.fasl" f
                                                       (random 1000000000 (make-random-state t))))))
          (unwind-protect (load fasl) (delete-file fasl)))))))

(defun load-fs-fat ()
  "fs-fat (lib/fat32.lisp: FAT12 to FAT32): the floppy that carries a Mac's cold
   code and its files."
  (unless (find-package "FS-FAT")
    (load (merge-pathnames "lib/fat32.lisp" (sibling-dir "fs-fat" "FS_FAT")))))

(defun run (program &rest args)
  "PROGRAM's standard output, or an error with its standard error."
  (let* ((err (make-string-output-stream))
         (out (with-output-to-string (s)
                (let ((p (sb-ext:run-program program args :search t :output s :error err)))
                  (unless (zerop (sb-ext:process-exit-code p))
                    (error "~A: ~A" program (get-output-stream-string err)))))))
    out))

(defun split-words (s)
  "S's space-separated words (no quoting: MAC68K_BASILISK and the like never needed any)."
  (loop with words = nil and start = nil
        for i from 0 to (length s)
        for c = (if (< i (length s)) (char s i) #\Space)
        do (cond ((and (char= c #\Space) start) (push (subseq s start i) words) (setq start nil))
                 ((and (char/= c #\Space) (null start)) (setq start i)))
        finally (return (nreverse words))))

(defun %open-pty ()
  "A pseudo-terminal: (values the master's fd, the slave's name, the slave's fd,
   kept open so the master never reads EOF while the emulator reopens it)."
  (let ((m (sb-alien:alien-funcall
            (sb-alien:extern-alien "posix_openpt" (function sb-alien:int sb-alien:int))
            (logior 2 #o400))))                ; O_RDWR | O_NOCTTY
    (when (< m 0) (error "posix_openpt failed"))
    (sb-alien:alien-funcall (sb-alien:extern-alien "grantpt" (function sb-alien:int sb-alien:int)) m)
    (sb-alien:alien-funcall (sb-alien:extern-alien "unlockpt" (function sb-alien:int sb-alien:int)) m)
    (let* ((name (sb-alien:alien-funcall
                  (sb-alien:extern-alien "ptsname" (function sb-alien:c-string sb-alien:int)) m))
           (s (sb-unix:unix-open name (logior 2 #o400) 0)))
      (run "stty" "-F" name "raw" "-echo")
      (values m name s))))

(defun basilisk-serial (disk timeout done-p &key (ram-mb 32) (fpu "false"))
  "Boot DISK under Basilisk II (MAC68K_BASILISK the command, MAC68K_ROM, MAC68K_JIT)
   with serial port A a pseudo-terminal; read it until (DONE-P bytes-so-far) is
   true, TIMEOUT seconds pass or Basilisk exits.  The bytes read.  Basilisk
   ignores SIGTERM, so its process group gets TERM and then KILL."
  (multiple-value-bind (master slave-name slave) (%open-pty)
    (let* ((cmd (append (split-words (env "MAC68K_BASILISK" "BasiliskII"))
                        (list "--nogui" "true" "--rom" (env "MAC68K_ROM") "--cpu" "4" "--fpu" fpu
                              "--ramsize" (princ-to-string (ash ram-mb 20))
                              "--jit" (env "MAC68K_JIT" "false") "--jitcachesize" "16384"
                              "--disk" disk "--seriala" slave-name)))
           (p (sb-ext:run-program (first cmd) (rest cmd) :search t :wait nil
                                                         :output nil :error nil :input nil))
           (got (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
           (buf (make-array 65536 :element-type '(unsigned-byte 8)))
           (end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
      (unwind-protect
           (loop until (or (funcall done-p got) (>= (get-internal-real-time) end)
                           (not (sb-ext:process-alive-p p)))
                 do (when (sb-sys:wait-until-fd-usable master :input 0.5)
                      (let ((n (sb-sys:with-pinned-objects (buf)
                                 (sb-unix:unix-read master (sb-sys:vector-sap buf) 65536))))
                        (when (and n (> n 0))
                          (loop for i below n do (vector-push-extend (aref buf i) got))))))
        (dolist (sig '(15 9))
          (when (sb-ext:process-alive-p p)
            (ignore-errors (sb-ext:process-kill p sig :process-group))
            (sleep 1)))
        (ignore-errors (sb-ext:process-kill p 9 :process-group))
        (sb-unix:unix-close master)
        (sb-unix:unix-close slave))
      (coerce got '(simple-array (unsigned-byte 8) (*))))))

(defun read-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

(defun write-bytes (path v)
  (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence v o))
  path)

(defun pad-to (v n &optional (at-least 0))
  "V, zero-padded to a multiple of N bytes and to at least AT-LEAST."
  (let ((len (max at-least (* n (ceiling (length v) n)))))
    (if (= len (length v))
        v
        (let ((w (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace w v)
          w))))

(defun kernel-source (name)
  "scripts/NAME as a build means it: modus's own if it has one, else vessel's
   kernel/scripts/NAME (lib/load-mvm.lisp MODUS-SOURCE's rule).  The Mac-side
   assembly -- boot blocks, loaders, the disk drivers -- lives in vessel."
  (or (probe-file (merge-pathnames name *scripts*))
      (probe-file (merge-pathnames (concatenate 'string "kernel/scripts/" name)
                                   (sibling-dir "vessel" "MODUS_VESSEL")))
      (error "~A is in neither modus's scripts/ nor vessel's kernel/scripts/" name)))

(defun assemble (source &key (cpu "68000") defsyms)
  "SOURCE (scripts/SOURCE, KERNEL-SOURCE's: modus's or vessel's) assembled for
   CPU (default the 68000; .include looks beside it), DEFSYMS ((NAME . VALUE)
   ...) defined first: (values its bytes, a hash of its symbols' offsets)."
  (let* ((dir (format nil "/tmp/mac68k-as-~D/" (random 1000000000 (make-random-state t))))
         (src (kernel-source source))
         (o (concatenate 'string dir "s.o")) (b (concatenate 'string dir "s.bin")))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (apply #'run "m68k-linux-gnu-as" (concatenate 'string "-m" cpu)
                  "-I" (directory-namestring src)
                  (append (loop for (n . v) in defsyms
                                collect "--defsym" collect (format nil "~A=~D" n v))
                          (list "-o" o (namestring src))))
           (run "m68k-linux-gnu-objcopy" "-O" "binary" o b)
           (let ((syms (make-hash-table :test 'equal)))
             (with-input-from-string (s (run "m68k-linux-gnu-nm" o))
               (loop for line = (read-line s nil) while line
                     do (let* ((sp1 (position #\Space line))
                               (sp2 (position #\Space line :start (1+ sp1))))
                          (setf (gethash (subseq line (1+ sp2)) syms)
                                (parse-integer line :end sp1 :radix 16)))))
             (values (read-bytes b) syms)))
      (dolist (f (list o b)) (when (probe-file f) (delete-file f)))
      (ignore-errors (sb-ext:delete-directory dir)))))
