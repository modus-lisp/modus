;;;; shard.lisp — run one slice of the upstream ansi-test suite on Modus.
;;;;
;;;; Loaded with `modus --script` from the ROOT of a private copy of the suite
;;;; (scripts/ansi-sweep.sh makes one per shard: init.lsp rewrites compiled
;;;; files and walks directories, so shards must not share a tree).
;;;;
;;;;   SHARD_LO / SHARD_HI   run tests with index in [LO, HI) of the suite's
;;;;                         pending list, in suite order
;;;;   SHARD_NAMES=FILE      instead, run only the tests named in FILE (one
;;;;                         per line, e.g. CL-TEST::ACONS.1 or ACONS.1)
;;;;
;;;; Prints ">>NAME" before each test and "PASS NAME" / "FAILX NAME" after it,
;;;; a "FAULT after NAME" line whenever the SIGSEGV handler recovered a fault,
;;;; and "==SHARD-END" last: a shard without it did not finish.

;; ONE toplevel form on purpose: it is READ before init.lsp loads, so a
;; readtable left broken by the suite's load cannot stop the driver from
;; running the tests.
(progn
  (setq *load-abort-on-error* nil)
  (load "init.lsp")
  ;; doit.lsp: these two misbehave when the tests are invoked from a file
  ;; that is being loaded -- which this driver is -- so upstream removes them.
  (mapc #'regression-test:rem-test (list (intern "LOAD-PATHNAME.1" "CL-TEST")
                                         (intern "LOAD-TRUENAME.1" "CL-TEST")))
  (let* ((names-file (sb-ext:posix-getenv "SHARD_NAMES"))
         (wanted (when names-file
                   (with-open-file (s names-file)
                     (loop for line = (read-line s nil nil)
                           while line
                           when (plusp (length line))
                             collect (let ((p (search "::" line)))
                                       (string-upcase
                                        (if p (subseq line (+ p 2)) line)))))))
         (lo (if wanted 0 (parse-integer (sb-ext:posix-getenv "SHARD_LO"))))
         (hi (if wanted 0 (parse-integer (sb-ext:posix-getenv "SHARD_HI"))))
         (lastf 0) (i 0))
    (format t "~&==SHARD ~d ~d~%" lo hi)
    ;; sandbox/scratch/ is made by ENSURE-DIRECTORIES-EXIST.8 and then used by
    ;; the stream tests (OPEN-STREAM-P.3, ...); git archive drops the empty
    ;; directory.  A shard starts in the state a sequential run would have:
    ;; the directory exists iff that test comes before this shard's tests.
    (let* ((ede (intern "ENSURE-DIRECTORIES-EXIST.8" "CL-TEST"))
           (pos (position ede (regression-test::pending-tests))))
      (when (and pos (if wanted
                         (not (member (symbol-name ede) wanted :test #'string=))
                         (< pos lo)))
        (ensure-directories-exist "sandbox/scratch/")))
    ;; Same dynamic context upstream's DO-TESTS gives every test (rt.lsp):
    ;; files resolve against sandbox/, and the package is CL-TEST.
    (let ((*default-pathname-defaults* (truename #p"sandbox/"))
          (*package* (find-package "CL-TEST")))
      (dolist (n (regression-test::pending-tests))
        (when (if wanted
                  (member (symbol-name n) wanted :test #'string=)
                  (and (>= i lo) (< i hi)))
          (format t "~&>>~a~%" n) (finish-output)
          (format t "~&~a ~a~%" (if (regression-test::do-test n) "PASS" "FAILX") n)
          (finish-output)
          ;; the recovered-fault record is x64's (modus-int::%HC-FAULT-W); other
          ;; targets have no such reader, and calling it ended the shard
          (when (fboundp 'modus-int::%hc-fault-w)
            (let ((f (+ (modus-int::%hc-fault-w 0 0) (* 4294967296 (modus-int::%hc-fault-w 0 1)))))
              (unless (eql f lastf)
                (setq lastf f)
                (format t "~&FAULT after ~a rip=~x~%" n f) (finish-output)))))
        (setq i (+ i 1))))
    (format t "~&==SHARD-END~%")))
