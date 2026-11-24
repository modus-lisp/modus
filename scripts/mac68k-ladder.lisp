;;;; mac68k-ladder.lisp -- the architecture ladder on a bare 68k Mac.
;;;;
;;;;   sbcl --script scripts/mac68k-ladder.lisp [RUNG...] [--keep]
;;;;
;;;; Each rung (test/arch-rungs/) is built for the :mac68k target
;;;; (`mvm/build-hosted.lisp mac68k`, boot/boot-mac68k.lisp), written to a boot
;;;; disk (scripts/mac68k-disk.lisp) and booted under Basilisk II: the Mac ROM runs
;;;; our boot blocks, which load the image, which answers over serial port A --
;;;; four bytes, as on the hosted ladder.  Basilisk's serial port A is a
;;;; pseudo-terminal this script reads (mac68k-tools BASILISK-SERIAL).
;;;;
;;;; Environment:
;;;;   MAC68K_ROM        the Mac ROM file (required; Apple's, so not in the tree).
;;;;                     Measured with the LC 475 / Quadra 605 ROM (FF7439EE).
;;;;   MAC68K_BASILISK   the command that runs Basilisk II (default BasiliskII); it
;;;;                     gets Basilisk's own arguments appended.  It must have a
;;;;                     display (Basilisk's GTK insists) -- a wrapper that starts an
;;;;                     Xvfb is the usual answer on a headless box.
;;;;   MAC68K_FPU        "false" (default: a 68LC040) or "true" (a 68040).
;;;;   MAC68K_EMULATOR   "mame": a Mac Classic (68000, 4 MB) in MAME instead --
;;;;                     MAC68K_MAME (the command), MAC68K_ROMPATH (its ROM folder:
;;;;                     macclasc's ROM and the ADB modem's 342s0440-b.bin); build
;;;;                     with MODUS_MAC68K_MACHINE=classic MODUS_68K_CPU=68000.
;;;;   MAC68K_JIT        "true": Basilisk's JIT, ~8x faster.  Debian's
;;;;                     BasiliskII-jit is a PIE and its JIT truncates addresses;
;;;;                     relink it low first (scripts/basilisk-unpie.lisp) and run it
;;;;                     with MALLOC_MMAP_MAX_=0.
;;;;   MAC68K_LADDER_JOBS  cells at once (default 4).
;;;;
;;;; The rules are hosted-ladder.py's: a deliberately wrong expectation runs first
;;;; and must come back WRONG (built, ran, rejected), or nothing is run.  It
;;;; replaced mac68k-ladder.py (2026-11-21).

(load (merge-pathnames "mac68k-tools.lisp" *load-truename*))
(defpackage #:mac68k-ladder (:use #:cl #:mac68k-tools))
(in-package #:mac68k-ladder)

(defparameter *repo* (merge-pathnames "../" *scripts*))
(defparameter *rungs* (merge-pathnames "test/arch-rungs/" *repo*))

(defun payload (rung-source)
  "The rung, then KERNEL-MAIN: the answer as 4 bytes, then halt."
  (format nil "~A~%(defun kernel-main ()
  (let ((v (probe)))
    (write-char-serial (logand v 255))
    (write-char-serial (logand (ash v -8) 255))
    (write-char-serial (logand (ash v -16) 255))
    (write-char-serial (logand (ash v -24) 255))
    (sys-exit 0)))~%" rung-source))

(defun file-text (path)
  (with-open-file (in path) (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in)))))

(defun expected-of (path)
  "The integer after the first `expect' and whitespace in the rung's text, or NIL."
  (let ((s (file-text path)))
    (loop for k = (search "expect" s) then (search "expect" s :start2 (1+ k))
          while k
          do (let ((i (+ k 6)))
               (when (and (< i (length s)) (member (char s i) '(#\Space #\Tab #\Newline)))
                 (loop while (and (< i (length s)) (member (char s i) '(#\Space #\Tab #\Newline))) do (incf i))
                 (multiple-value-bind (n end) (parse-integer s :start i :junk-allowed t)
                   (when (and n (> end i)) (return n))))))))

(defun boot-mame (disk nbytes &optional (timeout 600))
  "Boot DISK on MAME's Mac Classic (macclasc, a 68000, 4 MB); the first NBYTES
   the Mac sends on serial port A (scripts/mac68k-mame-serial.lua taps the SCC)."
  (declare (ignorable nbytes))
  (let* ((out (concatenate 'string disk ".serial"))
         (cmd (append (split-words (env "MAC68K_MAME" "mame"))
                      (list "macclasc" "-rompath" (env "MAC68K_ROMPATH") "-ramsize" "4M"
                            "-flop1" disk "-video" "none" "-sound" "none" "-nothrottle"
                            "-skip_gameinfo" "-autoboot_script"
                            (namestring (merge-pathnames "scripts/mac68k-mame-serial.lua" *repo*)))))
         (p (sb-ext:run-program (first cmd) (rest cmd) :search t :wait nil :output nil :error nil
                                :directory (directory-namestring disk)   ; MAME writes cfg/, nvram/ there
                                :environment (append (list (format nil "MAC68K_SERIAL_OUT=~A" out)
                                                           (format nil "MAC68K_SERIAL_N=~D" nbytes)
                                                           (format nil "MAC68K_SECONDS=~A" (env "MAC68K_MAME_SECONDS" "150")))
                                                     (sb-ext:posix-environ))))
         (end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop while (and (sb-ext:process-alive-p p) (< (get-internal-real-time) end)) do (sleep 0.5))
    (when (sb-ext:process-alive-p p) (ignore-errors (sb-ext:process-kill p 9 :process-group)))
    (if (probe-file out) (read-bytes out) (make-array 0 :element-type '(unsigned-byte 8)))))

(defun boot (disk nbytes &optional (timeout 90))
  "Boot DISK under Basilisk II (or MAME); the first NBYTES from serial port A."
  (if (equal (env "MAC68K_EMULATOR") "mame")
      (boot-mame disk nbytes)
      (basilisk-serial disk timeout (lambda (got) (>= (length got) nbytes))
                       :fpu (env "MAC68K_FPU" "false"))))

(defun run-cell (rung-path expect &optional keep)
  "(values PASS | WRONG | BUILD | RUN, and why)."
  (let* ((tmpdir (format nil "/tmp/mac68k-ladder-~D/" (random (expt 10 12) (make-random-state t))))
         (srcfile (concatenate 'string tmpdir "payload.lisp"))
         (binfile (concatenate 'string tmpdir "image.bin"))
         (disk (concatenate 'string tmpdir "boot.dsk")))
    (ensure-directories-exist tmpdir)
    (unwind-protect
         (progn
           (with-open-file (o srcfile :direction :output) (write-string (payload (file-text rung-path)) o))
           (let* ((log (make-string-output-stream))
                  (b (sb-ext:run-program "sbcl" (list "--dynamic-space-size" "4096" "--script"
                                                      (namestring (merge-pathnames "mvm/build-hosted.lisp" *repo*))
                                                      "mac68k" srcfile)
                                         :search t :directory (namestring *repo*) :output log :error log
                                         :environment (cons (format nil "MODUS_MAC68K_OUT=~A" binfile)
                                                            (sb-ext:posix-environ)))))
             (declare (ignore b))
             (unless (probe-file binfile)
               (let ((s (get-output-stream-string log)))
                 (return-from run-cell
                   (values "BUILD" (format nil "BUILD FAILED: ~A" (subseq s (max 0 (- (length s) 300)))))))))
           (run "sbcl" "--script" (namestring (merge-pathnames "scripts/mac68k-disk.lisp" *repo*)) binfile disk)
           (let ((out (boot disk 4)))
             (if (< (length out) 4)
                 (values "RUN" (format nil "SHORT OUTPUT (~D bytes)" (length out)))
                 (let ((got (logior (aref out 0) (ash (aref out 1) 8) (ash (aref out 2) 16) (ash (aref out 3) 24))))
                   (values (if (= got expect) "PASS" "WRONG")
                           (format nil "~A ~D (want ~D)" (if (= got expect) "=" "!=") got expect))))))
      (unless keep (sb-ext:delete-directory tmpdir :recursive t)))))

(defun main (args)
  (let* ((keep (member "--keep" args :test #'string=))
         (names (remove-if (lambda (a) (and (> (length a) 1) (string= (subseq a 0 2) "--"))) args)))
    (when (and (null (env "MAC68K_ROM")) (not (equal (env "MAC68K_EMULATOR") "mame")))
      (format t "mac68k-ladder: set MAC68K_ROM to a Mac ROM file~%")
      (return-from main 2))
    (let ((rungs (if names
                     (mapcar (lambda (a) (namestring (or (probe-file a) (merge-pathnames a *rungs*)))) names)
                     (sort (mapcar #'namestring (directory (merge-pathnames "*.lisp" *rungs*))) #'string<))))
      (multiple-value-bind (outcome why) (run-cell (first rungs) (1+ (expected-of (first rungs))))
        (unless (string= outcome "WRONG")
          (format t "mac68k: POSITIVE CONTROL ~A (~A) -- refusing to run~%" outcome why)
          (return-from main 2))
        (format t "mac68k: positive control answered WRONG as required (~A)~%" why)
        (finish-output))
      (let* ((n (max 1 (min (parse-integer (env "MAC68K_LADDER_JOBS" "4")) (length rungs))))
             (results (make-array (length rungs)))
             (next 0) (lock (sb-thread:make-mutex))
             (threads (loop repeat n
                            collect (sb-thread:make-thread
                                     (lambda ()
                                       (loop (let ((i (sb-thread:with-mutex (lock)
                                                        (when (< next (length rungs)) (prog1 next (incf next))))))
                                               (unless i (return))
                                               (let ((r (nth i rungs)))
                                                 (setf (aref results i)
                                                       (multiple-value-list (run-cell r (expected-of r) keep)))))))))))
        (mapc #'sb-thread:join-thread threads)
        (let ((failures 0))
          (loop for r in rungs for (outcome why) across results
                do (unless (string= outcome "PASS") (incf failures))
                   (format t "  ~26A mac68k   ~5A ~A~%" (file-namestring r)
                           (if (string= outcome "WRONG") "FAIL" outcome) why))
          (format t "~%mac68k ladder: ~D failed~%" failures)
          (if (zerop failures) 0 1))))))

(sb-ext:exit :code (main (argv)))
